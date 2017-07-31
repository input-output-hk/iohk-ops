{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Types where

import           Prelude                     hiding (FilePath)
import           Control.Exception                  (throwIO)
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.UTF8            as BUTF8
import           Data.ByteString.Lazy.Char8         (ByteString, pack)
import           Data.Char                          (ord)
import           Data.Data
import           Data.List                          (sort)
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Monoid                        ((<>))
import           Data.Optional (Optional)
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Data.Text.Lazy                     (fromStrict)
import           Data.Text.Lazy.Encoding            (encodeUtf8)
import qualified Data.Vector                     as V
import qualified Filesystem.Path.CurrentOS       as Path
import           GHC.Generics
import           Network.AWS                 hiding () -- send
import           Network.AWS.Auth
import           Network.AWS.EC2             hiding (All, DeleteTag, Snapshot, Stop)
import           Network.AWS.IAM             hiding (All, Any, AWS)
import           Safe                               (headMay)
import qualified System.IO                       as Sys
import qualified System.Logger                   as SL
import           System.Logger.Class                (MonadLogger (..))
import qualified System.Logger.Class             as Log
import           System.Logger.Message       hiding ((.=))
import           Text.Read                          (readMaybe)


-- * Leaves
import qualified Data.Aeson                      as AE
import           Data.Aeson                         ((.:), (.=))
import           Data.Csv                           (decodeWith, FromRecord(..), FromField(..), HasHeader(..), defaultDecodeOptions, decDelimiter)
import qualified Data.Yaml                       as YAML
import           Data.Yaml                          (FromJSON(..), ToJSON(..))
import           Turtle                      hiding (procs, inproc)
import qualified Turtle                          as Turtle


-- * Constants
--
awsPublicIPURL :: URL
awsPublicIPURL = "http://169.254.169.254/latest/meta-data/public-ipv4"

defaultEnvironment   = Development
defaultTarget        = AWS
defaultNodeLimit     = 14


-- * Utils
showT :: Show a => a -> Text
showT = T.pack . show

lowerShowT :: Show a => a -> Text
lowerShowT = T.toLower . T.pack . show

errorT :: Text -> a
errorT = error . T.unpack


-- * Projects
--
data Project
  = CardanoSL
  | CardanoExplorer
  | IOHK
  | Nixpkgs
  | Stack2nix
  deriving (Bounded, Enum, Eq, Read, Show)

every :: (Bounded a, Enum a) => [a]
every = enumFromTo minBound maxBound

projectURL     :: Project -> URL
projectURL     CardanoSL       = "https://github.com/input-output-hk/cardano-sl.git"
projectURL     CardanoExplorer = "https://github.com/input-output-hk/cardano-sl-explorer.git"
projectURL     IOHK            = "https://github.com/input-output-hk/iohk-nixops.git"
projectURL     Nixpkgs         = "https://github.com/nixos/nixpkgs.git"
projectURL     Stack2nix       = "https://github.com/input-output-hk/stack2nix.git"

projectSrcFile :: Project -> FilePath
projectSrcFile CardanoSL       = "cardano-sl-src.json"
projectSrcFile CardanoExplorer = "cardano-sl-explorer-src.json"
projectSrcFile Nixpkgs         = "nixpkgs-src.json"
projectSrcFile Stack2nix       = "stack2nix-src.json"
projectSrcFile IOHK            = error "Feeling self-referential?"


-- * Primitive types
--
newtype Branch    = Branch    { fromBranch  :: Text } deriving (FromJSON, Generic, Show, IsString)
newtype Commit    = Commit    { fromCommit  :: Text } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype NixParam  = NixParam  { fromNixParam :: Text } deriving (FromJSON, Generic, Show, IsString, Eq, Ord, AE.ToJSONKey, AE.FromJSONKey)
newtype IP        = IP        { getIP       :: Text } deriving (Show, Generic, FromField)
newtype NodeName  = NodeName  { fromNodeName :: Text } deriving (FromJSON, Generic, Show, FromField, IsString)
newtype NixHash   = NixHash   { fromNixHash :: Text } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype NixAttr   = NixAttr   { fromAttr    :: Text } deriving (FromJSON, Generic, Show, IsString)
newtype NixopsCmd = NixopsCmd { fromCmd     :: Text } deriving (FromJSON, Generic, Show, IsString)
newtype URL       = URL       { fromURL     :: Text } deriving (FromJSON, Generic, Show, IsString, ToJSON)


-- * A bit of Nix types
--
data SourceKind = Git | Github

data NixSource (a :: SourceKind) where
  -- | The output of 'nix-prefetch-git'
  GitSource ::
    { gUrl             :: URL
    , gRev             :: Commit
    , gSha256          :: NixHash
    , gFetchSubmodules :: Bool
    } -> NixSource Git
  GithubSource ::
    { ghOwner           :: Text
    , ghRepo            :: Text
    , ghRev             :: Commit
    , ghSha256          :: NixHash
    } -> NixSource Github
deriving instance Show (NixSource a)
instance FromJSON (NixSource Git) where
  parseJSON = AE.withObject "GitSource" $ \v -> GitSource
      <$> v .: "url"
      <*> v .: "rev"
      <*> v .: "sha256"
      <*> v .: "fetchSubmodules"
instance FromJSON (NixSource Github) where
  parseJSON = AE.withObject "GithubSource" $ \v -> GithubSource
      <$> v .: "owner"
      <*> v .: "repo"
      <*> v .: "rev"
      <*> v .: "sha256"

githubSource :: ByteString -> Maybe (NixSource Github)
githubSource = AE.decode
gitSource    :: ByteString -> Maybe (NixSource Git)
gitSource    = AE.decode

readSource :: (ByteString -> Maybe (NixSource a)) -> Project -> IO (NixSource a)
readSource parser (projectSrcFile -> path) =
  (fromMaybe (errorT $ format ("File doesn't parse as NixSource: "%fp) path) . parser)
  <$> BL.readFile (T.unpack $ format fp path)

nixpkgsNixosURL :: Commit -> URL
nixpkgsNixosURL (Commit rev) = URL $
  "https://github.com/NixOS/nixpkgs/archive/" <> rev <> ".tar.gz"

-- | The set of first-class types present in Nix
data NixValue
  = NixBool Bool
  | NixInt  Integer
  | NixStr  Text
  deriving (Generic, Show)
instance FromJSON NixValue
instance ToJSON NixValue

nixArgCmdline :: NixParam -> NixValue -> [Text]
nixArgCmdline (NixParam name) (NixBool bool) = ["--arg",    name, T.toLower $ showT bool]
nixArgCmdline (NixParam name) (NixInt int)   = ["--arg",    name, showT int]
nixArgCmdline (NixParam name) (NixStr str)   = ["--argstr", name, str]


-- * Domain
--
data Deployment
  = Explorer
  | Nodes
  | Infra
  | ReportServer
  | Timewarp
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Deployment
instance ToJSON Deployment

data Environment
  = Any               -- ^ Wildcard or unspecified, depending on context.
  | Production
  | Staging
  | Development
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Environment
instance ToJSON Environment

data Target
  = All               -- ^ Wildcard or unspecified, depending on context.
  | AWS
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Target
instance ToJSON Target

envConfigFilename :: IsString s => Environment -> s
envConfigFilename Any           = "config.yaml"
envConfigFilename Development   = "config.yaml"
envConfigFilename Staging       = "staging.yaml"
envConfigFilename Production    = "production.yaml"

selectDeployer :: Environment -> [Deployment] -> NodeName
selectDeployer Staging delts | elem Nodes delts = "iohk"
                             | otherwise        = "cardano-deployer"
selectDeployer _ _                              = "cardano-deployer"

type DeplArgs = Map.Map NixParam NixValue

selectDeploymentArgs :: Environment -> [Deployment] -> Integer -> DeplArgs
selectDeploymentArgs env delts limit = Map.fromList
  [ ("accessKeyId"
    , NixStr . fromNodeName $ selectDeployer env delts)
  , ("nodeLimit"
    , NixInt limit ) ]


-- * Deployment structure
--
type FileSpec = (Environment, Target, Text)

deployments :: [(Deployment, [FileSpec])]
deployments =
  [ (Explorer
    , [ (Any,         All, "deployments/cardano-explorer.nix")
      , (Development, All, "deployments/cardano-explorer-env-development.nix")
      , (Production,  All, "deployments/cardano-explorer-env-production.nix")
      , (Staging,     All, "deployments/cardano-explorer-env-staging.nix")
      , (Any,         AWS, "deployments/cardano-explorer-target-aws.nix") ])
  , (Nodes
    , [ (Any,         All, "deployments/cardano-nodes.nix")
      , (Production,  All, "deployments/cardano-nodes-env-production.nix")
      , (Staging,     All, "deployments/cardano-nodes-env-staging.nix")
      , (Any,         AWS, "deployments/cardano-nodes-target-aws.nix") ])
  , (Infra
    , [ (Any,         All, "deployments/infrastructure.nix")
      , (Production,  All, "deployments/infrastructure-env-production.nix")
      , (Any,         AWS, "deployments/infrastructure-target-aws.nix") ])
  , (ReportServer
    , [ (Any,         All, "deployments/report-server.nix")
      , (Production,  All, "deployments/report-server-env-production.nix")
      , (Staging,     All, "deployments/report-server-env-staging.nix")
      , (Any,         AWS, "deployments/report-server-target-aws.nix") ])
  , (Timewarp
    , [ (Any,         All, "deployments/timewarp.nix")
      , (Any,         AWS, "deployments/timewarp-target-aws.nix") ])
  ]

deploymentSpecs :: Deployment -> [FileSpec]
deploymentSpecs = fromJust . flip lookup deployments

filespecEnvSpecific :: Environment -> FileSpec -> Bool
filespecEnvSpecific x (x', _, _) = x == x'
filespecTgtSpecific :: Target      -> FileSpec -> Bool
filespecTgtSpecific x (_, x', _) = x == x'

filespecNeededEnv :: Environment -> FileSpec -> Bool
filespecNeededTgt :: Target      -> FileSpec -> Bool
filespecNeededEnv x fs = filespecEnvSpecific Any fs || filespecEnvSpecific x fs
filespecNeededTgt x fs = filespecTgtSpecific All fs || filespecTgtSpecific x fs

filespecFile :: FileSpec -> Text
filespecFile (_, _, x) = x

elementDeploymentFiles :: Environment -> Target -> Deployment -> [Text]
elementDeploymentFiles env tgt depl = filespecFile <$> (filter (\x -> filespecNeededEnv env x && filespecNeededTgt tgt x) $ deploymentSpecs depl)

deploymentFiles :: Environment -> Target -> [Deployment] -> [Text]
deploymentFiles cEnvironment cTarget cElements =
  "deployments/keypairs.nix":
  concat (elementDeploymentFiles cEnvironment cTarget <$> cElements)


data Options = Options
  { oConfigFile       :: Maybe FilePath
  , oConfirm          :: Bool
  , oDebug            :: Bool
  , oSerial           :: Bool
  , oVerbose          :: Bool
  } deriving Show

parserOptions :: Parser Options
parserOptions = Options
                <$> optional (optPath "config"  'c' "Configuration file")
                <*> switch  "confirm" 'y' "Pass --confirm to nixops"
                <*> switch  "debug"   'd' "Pass --debug to nixops"
                <*> switch  "serial"  's' "Disable parallelisation"
                <*> switch  "verbose" 'v' "Print all commands that are being run"

nixpkgsCommitPath :: Commit -> Text
nixpkgsCommitPath = ("nixpkgs=" <>) . fromURL . nixpkgsNixosURL

nixopsCmdOptions :: Options -> NixopsConfig -> [Text]
nixopsCmdOptions Options{..} NixopsConfig{..} =
  ["--debug"   | oDebug]   <>
  ["--confirm" | oConfirm] <>
  ["--show-trace"
  ,"--deployment", cName
  ,"-I", nixpkgsCommitPath cNixpkgsCommit
  ]


data NixopsConfig = NixopsConfig
  { cName             :: Text
  , cNixpkgsCommit    :: Commit
  , cEnvironment      :: Environment
  , cTarget           :: Target
  , cElements         :: [Deployment]
  , cFiles            :: [Text]
  , cDeplArgs         :: DeplArgs
  } deriving (Generic, Show)
instance FromJSON NixopsConfig where
    parseJSON = AE.withObject "NixopsConfig" $ \v -> NixopsConfig
        <$> v .: "name"
        <*> v .: "nixpkgs"
        <*> v .: "environment"
        <*> v .: "target"
        <*> v .: "elements"
        <*> v .: "files"
        <*> v .: "args"
instance ToJSON NixopsConfig where
  toJSON NixopsConfig{..} = AE.object
   [ "name"        .= cName
   , "nixpkgs"     .= fromCommit cNixpkgsCommit
   , "environment" .= showT cEnvironment
   , "target"      .= showT cTarget
   , "elements"    .= cElements
   , "files"       .= cFiles
   , "args"        .= cDeplArgs ]


-- | Interpret inputs into a NixopsConfig
mkConfig :: Branch -> Commit -> Environment -> Target -> [Deployment] -> Integer -> NixopsConfig
mkConfig (Branch cName) cNixpkgsCommit cEnvironment cTarget cElements nodeLimit =
  let cFiles    = deploymentFiles cEnvironment cTarget cElements
      cDeplArgs = selectDeploymentArgs cEnvironment cElements nodeLimit
  in NixopsConfig{..}

-- | Write the config file
writeConfig :: MonadIO m => Maybe FilePath -> NixopsConfig -> m FilePath
writeConfig mFp c@NixopsConfig{..} = do
  let configFilename = flip fromMaybe mFp $ envConfigFilename cEnvironment
  liftIO $ writeTextFile configFilename $ T.pack $ BUTF8.toString $ YAML.encode c
  pure configFilename

-- | Read back config, doing validation
readConfig :: MonadIO m => FilePath -> m NixopsConfig
readConfig cf = do
  cfParse <- liftIO $ YAML.decodeFileEither $ Path.encodeString $ cf
  let c@NixopsConfig{..}
        = case cfParse of
            Right c -> c
            -- TODO: catch and suggest versioning
            Left  e -> error $ T.unpack $ format ("Failed to parse config file "%fp%": "%s)
                       cf (T.pack $ YAML.prettyPrintParseException e)
      storedFileSet  = Set.fromList cFiles
      deducedFiles   = deploymentFiles cEnvironment cTarget cElements
      deducedFileSet = Set.fromList $ deducedFiles
  unless (storedFileSet == deducedFileSet) $
    die $ format ("Config file '"%fp%"' is incoherent with respect to elements "%w%":\n  - stored files:  "%w%"\n  - implied files: "%w%"\n")
          cf cElements (sort cFiles) (sort deducedFiles)
  pure c


-- * Snapshot schedule & args

data Schedule = Schedule
    { minGenerations  :: Int
    , minAge          :: NominalDiffTime
    } deriving (Data, Typeable, Show)

data Args = Args
    { _argsCredsFP         :: FilePath
    , _argsRegion          :: Region
    , _argsDefaultSchedule :: Schedule
    , _argsOnlyInstances   :: [Text]
    , _argsLogLevel        :: Log.Level
    } deriving (Data, Typeable, Show)

deriving instance Data Log.Level


-- * Bind them all: Context for Reader
--
data Context where
  Context ::
    { ctxOptions      :: Options
    , ctxConfig       :: NixopsConfig
    , ctxSnapshotArgs :: Args
    } -> Context

type ContextM a = ReaderT Context a
