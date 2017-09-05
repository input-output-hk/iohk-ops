{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-missing-signatures -Wno-unticked-promoted-constructors -Wno-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module NixOps where

import           Control.Arrow                    ((***))
import           Control.Exception                (throwIO)
import           Control.Lens                     ((<&>))
import           Control.Monad                    (forM_)
import qualified Data.Aeson                    as AE
import           Data.Aeson                       ((.:), (.:?), (.=), (.!=))
import qualified Data.Aeson.Types              as AE
import           Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy          as BL
import           Data.ByteString.Lazy.Char8       (ByteString)
import qualified Data.ByteString.UTF8          as BU
import qualified Data.ByteString.Lazy.UTF8     as LBU
import           Data.Char                        (ord, toLower)
import           Data.Csv                         (decodeWith, FromRecord(..), FromField(..), HasHeader(..), defaultDecodeOptions, decDelimiter)
import           Data.Either
import           Data.Foldable                    (asum)
import           Data.Hourglass                   (timeAdd, timeFromElapsed, timePrint, Duration(..), ISO8601_DateAndTime(..))
import           Data.List                        (nub, sort)
import           Data.Maybe
import qualified Data.Map.Strict               as Map
import           Data.Monoid                      ((<>))
import           Data.Optional                    (Optional)
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Text.Lazy                   (fromStrict)
import           Data.Text.Lazy.Encoding          (encodeUtf8)
import qualified Data.Vector                   as V
import qualified Data.Yaml                     as YAML
import           Data.Yaml                        (FromJSON(..), ToJSON(..))
import           Debug.Trace                      (trace)
import qualified Filesystem.Path.CurrentOS     as Path
import           GHC.Generics              hiding (from, to)
import           Prelude                   hiding (FilePath)
import           Safe                             (headMay)
import qualified System.IO                     as Sys
import           Time.System
import           Time.Types
import           Turtle                    hiding (env, err, fold, inproc, prefix, procs, e, f, o, x)
import qualified Turtle                        as Turtle


import           Topology


-- * Constants
--
awsPublicIPURL :: URL
awsPublicIPURL = "http://169.254.169.254/latest/meta-data/public-ipv4"

defaultEnvironment   = Development
defaultTarget        = AWS
defaultNode          = NodeName "c-a-1"
defaultNodePort      = PortNo 3000
defaultNixpkgs       = Commit "9b948ea439ddbaa26740ce35543e7e35d2aa6d18"

defaultHold          = 1200 :: Seconds -- 20 minutes

explorerNode         = NodeName "explorer"

orgs :: [NodeOrg]
orgs                 = enumFromTo minBound maxBound
defaultOrg           = IOHK
accessKeyChain       = [ AccessKeyId $ showT org <> "accessKeyId"
                       | org <- orgs ]

simpleTopoFile       :: FilePath
simpleTopoFile       = "topology.nix"


-- * Projects
--
data Project
  = CardanoSL
  | IOHKOps
  | Nixpkgs
  | Stack2nix
  | Nixops
  deriving (Bounded, Enum, Eq, Read, Show)

every :: (Bounded a, Enum a) => [a]
every = enumFromTo minBound maxBound

projectURL     :: Project -> URL
projectURL     CardanoSL       = "https://github.com/input-output-hk/cardano-sl"
projectURL     IOHKOps         = "https://github.com/input-output-hk/iohk-nixops"
projectURL     Nixpkgs         = "https://github.com/nixos/nixpkgs"
projectURL     Stack2nix       = "https://github.com/input-output-hk/stack2nix"
projectURL     Nixops          = "https://github.com/input-output-hk/nixops"

projectSrcFile :: Project -> FilePath
projectSrcFile CardanoSL       = "cardano-sl-src.json"
projectSrcFile Nixpkgs         = "nixpkgs-src.json"
projectSrcFile Stack2nix       = "stack2nix-src.json"
projectSrcFile IOHKOps         = error "Feeling self-referential?"
projectSrcFile Nixops          = error "No corresponding -src.json spec for 'nixops' yet."


-- * Primitive types
--
newtype AccessKeyId  = AccessKeyId  { fromAccessKeyId  :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype Branch       = Branch       { fromBranch       :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype Commit       = Commit       { fromCommit       :: Text   } deriving (Eq, FromJSON, Generic, Show, IsString, ToJSON)
newtype NixParam     = NixParam     { fromNixParam     :: Text   } deriving (FromJSON, Generic, Show, IsString, Eq, Ord, AE.ToJSONKey, AE.FromJSONKey)
newtype NixHash      = NixHash      { fromNixHash      :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype NixAttr      = NixAttr      { fromAttr         :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype NixopsCmd    = NixopsCmd    { fromCmd          :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype NixopsDepl   = NixopsDepl   { fromNixopsDepl   :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype Org          = Org          { fromOrg          :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype Region       = Region       { fromRegion       :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype URL          = URL          { fromURL          :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype FQDN         = FQDN         { fromFQDN         :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype IP           = IP           { getIP            :: Text   } deriving (Show, Generic, FromField)
newtype PortNo       = PortNo       { fromPortNo       :: Int    } deriving (FromJSON, Generic, Show, ToJSON)
newtype Exec         = Exec         { fromExec         :: Text   } deriving (IsString, Show)
newtype Arg          = Arg          { fromArg          :: Text   } deriving (IsString, Show)

deriving instance Eq NodeType
deriving instance Read NodeName
deriving instance AE.ToJSONKey NodeName

fromNodeName :: NodeName -> Text
fromNodeName (NodeName x) = x

-- | Sum to track assurance
data Confirmation = Confirm | Ask Text
  deriving (Eq, Read, Show)

confirmOrTerminate :: Confirmation -> IO ()
confirmOrTerminate  Confirm       = pure ()
confirmOrTerminate (Ask question) = do
  echo $ unsafeTextToLine question <> "  Enter 'yes' to proceed:"
  reply <- readline
  unless (reply == Just "yes") $ do
    echo "User declined to proceed, exiting."
    exit $ ExitFailure 1


-- * Some orphan instances..
--
instance FromJSON FilePath where parseJSON = AE.withText "filepath" $ \v -> pure $ fromText v
instance ToJSON   FilePath where toJSON    = AE.String . format fp
deriving instance Generic Seconds; instance FromJSON Seconds; instance ToJSON Seconds
deriving instance Generic Elapsed; instance FromJSON Elapsed; instance ToJSON Elapsed


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
instance ToJSON (NixSource Git) where
  toJSON GitSource{..} = AE.object
   [ "url"             .= fromURL gUrl
   , "rev"             .= fromCommit gRev
   , "sha256"          .= fromNixHash gSha256
   , "fetchSubmodules" .= lowerShowT gFetchSubmodules ]
instance FromJSON (NixSource Git) where
  parseJSON = AE.withObject "GitSource" $ \v -> GitSource
      <$> v .: "url"
      <*> v .: "rev"
      <*> v .: "sha256"
      <*> asum [ (v .: "fetchSubmodules")
               , readT . T.toTitle <$> (v .: "fetchSubmodules")]
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
  | NixAttrSet (Map.Map Text NixValue)
  | NixImport NixValue NixValue
  | NixFile FilePath
  deriving (Generic, Show)
instance FromJSON NixValue
instance ToJSON NixValue

nixValueStr :: NixValue -> Text
nixValueStr (NixBool bool)      = T.toLower $ showT bool
nixValueStr (NixAttrSet attrs)  = ("{ " <>) . (<> " }") . T.concat
                                  $ [ k <> " = " <> nixValueStr v <> "; "
                                    | (k, v) <- Map.toList attrs ]
nixValueStr (NixImport f as)    = "import " <> nixValueStr f <> " " <>  nixValueStr as
nixValueStr (NixInt  int)       = showT int
nixValueStr (NixStr  str)       = "\"" <> str <>"\""          -- XXX: this is naive, as it doesn't do escaping
nixValueStr (NixFile f)         = let txt = format fp f
                                  in if T.isPrefixOf "/" txt
                                     then txt else ("./" <> txt)

nixArgCmdline :: NixParam -> NixValue -> [Text]
nixArgCmdline (NixParam name) x@(NixStr _) = ["--argstr", name, T.drop 1 $ nixValueStr x & T.dropEnd 1]
nixArgCmdline (NixParam name) x            = ["--arg",    name, nixValueStr x]


-- * Domain
--
data Deployment
  = Explorer
  | Nodes
  | Infra
  | ReportServer
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Deployment

data Environment
  = Any               -- ^ Wildcard or unspecified, depending on context.
  | Production
  | Staging
  | Development
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Environment

data Target
  = All               -- ^ Wildcard or unspecified, depending on context.
  | AWS
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Target

envConfigFilename :: IsString s => Environment -> s
envConfigFilename Any           = "config.yaml"
envConfigFilename Development   = "config.yaml"
envConfigFilename Staging       = "staging.yaml"
envConfigFilename Production    = "production.yaml"

selectDeployer :: Environment -> [Deployment] -> NodeName
selectDeployer Staging   delts | elem Nodes delts = "iohk"
                               | otherwise        = "cardano-deployer"
selectDeployer _ _                                = "cardano-deployer"

selectTopologyConfig :: Environment -> [Deployment] -> FilePath
selectTopologyConfig Development _ = "topology-development.yaml"
selectTopologyConfig Staging     _ = "topology-staging.yaml"
selectTopologyConfig _           _ = "topology.yaml"

detectDeployerIP :: Options -> IO IP
detectDeployerIP o = IP <$> incmd o "curl" ["--silent", fromURL awsPublicIPURL]


-- * Topology
--
readTopology :: FilePath -> IO Topology
readTopology file = do
  eTopo <- liftIO $ YAML.decodeFileEither $ Path.encodeString file
  case eTopo of
    Right (topology :: Topology) -> pure topology
    Left err -> errorT $ format ("Failed to parse topology file: "%fp%": "%w) file err

data SimpleTopo
  =  SimpleTopo (Map.Map NodeName SimpleNode)
  deriving (Generic, Show)
instance ToJSON SimpleTopo
data SimpleNode
  =  SimpleNode
     { snType     :: NodeType
     , snRegion   :: NodeRegion
     , snOrg      :: NodeOrg
     , snFQDN     :: FQDN
     , snPort     :: PortNo
     , snInPeers  :: [NodeName]                  -- ^ Incoming connection edges
     , snKademlia :: RunKademlia
     } deriving (Generic, Show)
instance ToJSON SimpleNode where-- toJSON = jsonLowerStrip 2
  toJSON SimpleNode{..} = AE.object
   [ "type"        .= (lowerShowT snType & T.stripPrefix "node"
                        & fromMaybe (error "A NodeType constructor gone mad: doesn't start with 'Node'."))
   , "region"      .= snRegion
   , "org"         .= snOrg
   , "address"     .= fromFQDN snFQDN
   , "port"        .= fromPortNo snPort
   , "peers"       .= snInPeers
   , "kademlia"    .= snKademlia ]
instance ToJSON NodeRegion
instance ToJSON NodeName
deriving instance Generic NodeName
deriving instance Generic NodeRegion
deriving instance Generic NodeType
instance ToJSON NodeType

topoNodes :: SimpleTopo -> [NodeName]
topoNodes (SimpleTopo cmap) = Map.keys cmap

topoNCores :: SimpleTopo -> Int
topoNCores (SimpleTopo cmap) = Map.size $ flip Map.filter cmap ((== NodeCore) . snType)

summariseTopology :: Topology -> SimpleTopo
summariseTopology (TopologyStatic (AllStaticallyKnownPeers nodeMap)) =
  SimpleTopo $ Map.mapWithKey simplifier nodeMap
  where simplifier node (NodeMetadata snType snRegion (NodeRoutes outRoutes) nmAddr snKademlia mbOrg) =
          SimpleNode{..}
          where (mPort,  fqdn)   = case nmAddr of
                                     (NodeAddrExact fqdn'  mPort') -> (mPort', fqdn') -- (Ok, bizarrely, this contains FQDNs, even if, well.. : -)
                                     (NodeAddrDNS   mFqdn  mPort') -> (mPort', flip fromMaybe mFqdn
                                                                      $ error "Cannot deploy a topology with nodes lacking a FQDN address.")
                (snPort, snFQDN) = (,) (fromMaybe defaultNodePort $ PortNo . fromIntegral <$> mPort)
                                   $ (FQDN . T.pack . BU.toString) $ fqdn
                snInPeers = Set.toList . Set.fromList
                            $ [ other
                              | (other, (NodeMetadata _ _ (NodeRoutes routes) _ _ _)) <- Map.toList nodeMap
                              , elem node (concat routes) ]
                            <> concat outRoutes
                snOrg = fromMaybe (trace (T.unpack $ format ("WARNING: node '"%s%"' has no 'org' field specified, defaulting to "%w%".")
                                          (fromNodeName node) defaultOrg)
                                   defaultOrg)
                        mbOrg
summariseTopology x = errorT $ format ("Unsupported topology type: "%w) x

-- | Dump intermediate core/relay info, as parametrised by the simplified topology file.
dumpTopologyNix :: NixopsConfig -> IO ()
dumpTopologyNix NixopsConfig{..} = sh $ do
  let nodeSpecExpr prefix =
        format ("with (import <nixpkgs> {}); "%s%" (import ./globals.nix { deployerIP = \"\"; environment = \""%s%"\"; topologyYaml = ./"%fp%"; systemStart = 0; "%s%" = \"-stub-\"; })")
               prefix (lowerShowT cEnvironment) cTopology (T.intercalate " = \"-stub-\"; " $ fromAccessKeyId <$> accessKeyChain)
      getNodeArgsAttr prefix attr = inproc "nix-instantiate" ["--strict", "--show-trace", "--eval" ,"-E", nodeSpecExpr prefix <> "." <> attr] empty
      liftNixList = inproc "sed" ["s/\" \"/\", \"/g"]
  (cores  :: [NodeName]) <- getNodeArgsAttr "map (x: x.name)" "cores"  & liftNixList <&> ((NodeName <$>) . readT . lineToText)
  (relays :: [NodeName]) <- getNodeArgsAttr "map (x: x.name)" "relays" & liftNixList <&> ((NodeName <$>) . readT . lineToText)
  echo "Cores:"
  forM_ cores  $ \(NodeName x) -> do
    printf ("  "%s%"\n    ") x
    Turtle.proc "nix-instantiate" ["--strict", "--show-trace", "--eval" ,"-E", nodeSpecExpr "" <> ".nodeMap." <> x] empty
  echo "Relays:"
  forM_ relays $ \(NodeName x) -> do
    printf ("  "%s%"\n    ") x
    Turtle.proc "nix-instantiate" ["--strict", "--show-trace", "--eval" ,"-E", nodeSpecExpr "" <> ".nodeMap." <> x] empty

nodeNames :: Options -> NixopsConfig -> [NodeName]
nodeNames (oOnlyOn -> nodeLimit)  NixopsConfig{..}
  | Nothing   <- nodeLimit = topoNodes topology <> [explorerNode | elem Explorer cElements]
  | Just node <- nodeLimit
  , SimpleTopo nodeMap <- topology
  = if Map.member node nodeMap || node == explorerNode then [node]
    else errorT $ format ("Node '"%s%"' doesn't exist in cluster '"%fp%"'.") (showT $ fromNodeName node) cTopology


-- * deployment structure
--
type FileSpec = (Environment, Target, Text)

deployments :: [(Deployment, [FileSpec])]
deployments =
  [ (Explorer
    , [ (Any,         All, "global-resources.nix")
      , (Any,         All, "deployments/cardano-explorer.nix")
      , (Production,  All, "deployments/cardano-explorer-env-production.nix")
      , (Staging,     All, "deployments/cardano-explorer-env-staging.nix") ])
  , (Nodes
    , [ (Any,         All, "global-resources.nix")
      , (Any,         All, "deployments/cardano-nodes.nix")
      , (Development, All, "deployments/cardano-nodes-env-development.nix")
      , (Production,  All, "deployments/cardano-nodes-env-production.nix")
      , (Staging,     All, "deployments/cardano-nodes-env-staging.nix") ])
  , (ReportServer
    , [ (Any,         All, "global-resources.nix")
      , (Any,         All, "deployments/report-server.nix")
      , (Production,  All, "deployments/report-server-env-production.nix")
      , (Staging,     All, "deployments/report-server-env-staging.nix") ])
  , (Infra
    , [ (Any,         All, "deployments/infrastructure.nix")
      , (Production,  All, "deployments/infrastructure-env-production.nix")
      , (Any,         AWS, "deployments/infrastructure-target-aws.nix") ])
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


data Options = Options
  { oChdir            :: Maybe FilePath
  , oConfigFile       :: Maybe FilePath
  , oOnlyOn           :: Maybe NodeName
  , oConfirm          :: Bool
  , oDebug            :: Bool
  , oSerial           :: Bool
  , oVerbose          :: Bool
  , oNoComponentCheck :: Bool
  , oMosh             :: Bool
  , oNixpkgs          :: Maybe FilePath
  } deriving Show

parserBranch :: Optional HelpMessage -> Parser Branch
parserBranch desc = Branch <$> argText "branch" desc

parserCommit :: Optional HelpMessage -> Parser Commit
parserCommit desc = Commit <$> argText "commit" desc

parserNodeLimit :: Parser (Maybe NodeName)
parserNodeLimit = optional $ NodeName <$> (optText "just-node" 'n' "Limit operation to the specified node")

parserOptions :: Parser Options
parserOptions = Options
                <$> optional (optPath "chdir"     'C' "Run as if 'iohk-ops' was started in <path> instead of the current working directory.")
                <*> optional (optPath "config"    'c' "Configuration file")
                <*> (optional $ NodeName
                     <$>     (optText "on"        'o' "Limit operation to the specified node"))
                <*>           switch  "confirm"   'y' "Pass --confirm to nixops"
                <*>           switch  "debug"     'd' "Pass --debug to nixops"
                <*>           switch  "serial"    's' "Disable parallelisation"
                <*>           switch  "verbose"   'v' "Print all commands that are being run"
                <*>           switch  "no-component-check" 'p' "Disable deployment/*.nix component check"
                <*>           switch  "mosh"      'm' "Use 'mosh' instead of 'ssh' for time-consuming operations"
                <*> (optional $ optPath "nixpkgs" 'i' "Set 'nixpkgs' revision")

nixpkgsCommitPath :: Commit -> Text
nixpkgsCommitPath = ("nixpkgs=" <>) . fromURL . nixpkgsNixosURL

nixopsCmdOptions :: Options -> NixopsConfig -> [Text]
nixopsCmdOptions Options{..} NixopsConfig{..} =
  ["--debug"   | oDebug]   <>
  ["--confirm" | oConfirm] <>
  ["--show-trace"
  ,"--deployment", fromNixopsDepl cName
  ] <> fromMaybe [] ((["-I"] <>) . (:[]) . ("nixpkgs=" <>) . format fp <$> oNixpkgs)


-- | Before adding a field here, consider, whether the value in question
--   ought to be passed to Nix.
--   If so, the way to do it is to add a deployment argument (see DeplArgs),
--   which are smuggled across Nix border via --arg/--argstr.
data NixopsConfig = NixopsConfig
  { cName             :: NixopsDepl
  , cGenCmdline       :: Text
  , cNixpkgs          :: Maybe Commit
  , cNixops           :: FilePath
  , cTopology         :: FilePath
  , cEnvironment      :: Environment
  , cTarget           :: Target
  , cElements         :: [Deployment]
  , cFiles            :: [Text]
  , cDeplArgs         :: DeplArgs
  -- this isn't stored in the config file, but is, instead filled in during initialisation
  , topology          :: SimpleTopo
  } deriving (Generic, Show)
instance FromJSON NixopsConfig where
    parseJSON = AE.withObject "NixopsConfig" $ \v -> NixopsConfig
        <$> v .: "name"
        <*> v .:? "gen-cmdline" .!= "--unknown--"
        <*> v .:? "nixpkgs"
        <*> v .:? "nixops"      .!= "nixops"
        <*> v .:? "topology"    .!= "topology-development.yaml"
        <*> v .: "environment"
        <*> v .: "target"
        <*> v .: "elements"
        <*> v .: "files"
        <*> v .: "args"
        <*> pure undefined -- this is filled in in readConfig
instance ToJSON Environment
instance ToJSON Target
instance ToJSON Deployment
instance ToJSON NixopsConfig where
  toJSON NixopsConfig{..} = AE.object
   [ "name"         .= fromNixopsDepl cName
   , "gen-cmdline"  .= cGenCmdline
   , "nixops"       .= cNixops
   , "topology"     .= cTopology
   , "environment"  .= showT cEnvironment
   , "target"       .= showT cTarget
   , "elements"     .= cElements
   , "files"        .= cFiles
   , "args"         .= cDeplArgs ]

deploymentFiles :: Environment -> Target -> [Deployment] -> [Text]
deploymentFiles cEnvironment cTarget cElements =
  nub $ concat (elementDeploymentFiles cEnvironment cTarget <$> cElements)

type DeplArgs = Map.Map NixParam NixValue

selectDeploymentArgs :: Options -> FilePath -> Environment -> [Deployment] -> Elapsed -> Maybe IP -> IO DeplArgs
selectDeploymentArgs o _ env delts (Elapsed systemStart) mDeployerIP = do
    let staticArgs = [ ( NixParam $ fromAccessKeyId akid
                       , NixStr . fromNodeName $ selectDeployer env delts)
                     | akid <- accessKeyChain ]
    IP deployerIp <- case mDeployerIP of
                       Nothing -> detectDeployerIP o
                       Just ip -> pure ip
    pure $ Map.fromList $
      staticArgs
      <> [ ("deployerIP",   NixStr deployerIp)
         , ("systemStart",  NixInt $ fromIntegral systemStart)]

deplArg    :: NixopsConfig -> NixParam -> NixValue -> NixValue
deplArg      NixopsConfig{..} k def = Map.lookup k cDeplArgs & fromMaybe def
  --(errorT $ format ("Deployment arguments don't hold a value for key '"%s%"'.") (showT k))

setDeplArg :: NixopsConfig -> NixParam -> NixValue -> NixopsConfig
setDeplArg c@NixopsConfig{..} k v = c { cDeplArgs = Map.insert k v cDeplArgs }

-- | Interpret inputs into a NixopsConfig
mkConfig :: Options -> Text -> NixopsDepl -> Maybe FilePath -> Maybe FilePath -> Environment -> Target -> [Deployment] -> Elapsed -> Maybe IP -> IO NixopsConfig
mkConfig o cGenCmdline cName mNixops mTopology cEnvironment cTarget cElements systemStart mDeployerIP = do
  let cNixops   = fromMaybe "nixops" mNixops
      cFiles    = deploymentFiles                          cEnvironment cTarget cElements
      cTopology = flip fromMaybe mTopology $
                  selectTopologyConfig                     cEnvironment         cElements
      cNixpkgs  = Nothing
  cDeplArgs    <- selectDeploymentArgs o cTopology         cEnvironment         cElements systemStart mDeployerIP
  topology <- liftIO $ summariseTopology <$> readTopology cTopology
  pure NixopsConfig{..}

normaliseConfigFilename :: Maybe FilePath -> NixopsConfig -> FilePath
normaliseConfigFilename mFp NixopsConfig{..} = flip fromMaybe mFp $ envConfigFilename cEnvironment

-- | Write the config file
writeConfig :: MonadIO m => Maybe FilePath -> NixopsConfig -> m FilePath
writeConfig mFp c@NixopsConfig{..} = do
  let configFilename = normaliseConfigFilename mFp c
  liftIO $ writeTextFile configFilename $ T.pack $ BU.toString $ YAML.encode c
  pure configFilename

-- | Read back config, doing validation
readConfig :: MonadIO m => Options -> FilePath -> m NixopsConfig
readConfig Options{..} cf = do
  cfParse <- liftIO $ YAML.decodeFileEither $ Path.encodeString $ cf
  let c@NixopsConfig{..}
        = case cfParse of
            Right cfg -> cfg
            -- TODO: catch and suggest versioning
            Left  e -> errorT $ format ("Failed to parse config file "%fp%": "%s)
                       cf (T.pack $ YAML.prettyPrintParseException e)
      storedFileSet  = Set.fromList cFiles
      deducedFiles   = deploymentFiles cEnvironment cTarget cElements
      deducedFileSet = Set.fromList $ deducedFiles
  unless (storedFileSet == deducedFileSet || oNoComponentCheck) $
    die $ format ("Config file '"%fp%"' is incoherent with respect to elements "%w%":\n  - stored files:  "%w%"\n  - implied files: "%w%"\n")
          cf cElements (sort cFiles) (sort deducedFiles)
  -- Can't read topology file without knowing its name, hence this phasing.
  topo <- liftIO $ summariseTopology <$> readTopology cTopology
  pure c { topology = topo }


parallelIO' :: Options -> NixopsConfig -> ([NodeName] -> [a]) -> (a -> IO ()) -> IO ()
parallelIO' o@Options{..} c@NixopsConfig{..} xform action =
  ((if oSerial
    then sequence_
    else sh . parallel) $
   action <$> (xform $ nodeNames o c))
  >> echo ""

parallelIO :: Options -> NixopsConfig -> (NodeName -> IO ()) -> IO ()
parallelIO o c = parallelIO' o c id

logCmd  bin args = do
  printf ("-- "%s%"\n") $ T.intercalate " " $ bin:args
  Sys.hFlush Sys.stdout

inproc :: Text -> [Text] -> Shell Line -> Shell Line
inproc bin args inp = do
  liftIO $ logCmd bin args
  Turtle.inproc bin args inp

minprocs :: MonadIO m => Text -> [Text] -> Shell Line -> m (Either ProcFailed Text)
minprocs bin args inp = do
  (exitCode, out) <- liftIO $ procStrict bin args inp
  pure $ case exitCode of
           ExitSuccess -> Right out
           _           -> Left $ ProcFailed bin args exitCode

inprocs :: MonadIO m => Text -> [Text] -> Shell Line -> m Text
inprocs bin args inp = do
  ret <- minprocs bin args inp
  case ret of
    Right out -> pure out
    Left  err -> liftIO $ throwIO err

cmd   :: Options -> Text -> [Text] -> IO ()
cmd'  :: Options -> Text -> [Text] -> IO (ExitCode, Text)
incmd :: Options -> Text -> [Text] -> IO Text

cmd   Options{..} bin args = do
  when oVerbose $ logCmd bin args
  Turtle.procs      bin args empty
cmd'  Options{..} bin args = do
  when oVerbose $ logCmd bin args
  Turtle.procStrict bin args empty
incmd Options{..} bin args = do
  when oVerbose $ logCmd bin args
  inprocs bin args empty

gitHEADCommit :: Options -> IO Commit
gitHEADCommit o = Commit <$> incmd o "git" ["log", "-n1", "--pretty=format:%H"]


-- * Invoking nixops
--
nixops'' :: (Options -> Text -> [Text] -> IO b) -> Options -> NixopsConfig -> NixopsCmd -> [Arg] -> IO b
nixops'' executor o c@NixopsConfig{..} com args =
  executor o (format fp cNixops)
  (fromCmd com : nixopsCmdOptions o c <> fmap fromArg args)

nixops' :: Options -> NixopsConfig -> NixopsCmd -> [Arg] -> IO (ExitCode, Text)
nixops  :: Options -> NixopsConfig -> NixopsCmd -> [Arg] -> IO ()
nixops' = nixops'' cmd'
nixops  = nixops'' cmd

nixopsMaybeLimitNodes :: Options -> [Arg]
nixopsMaybeLimitNodes (oOnlyOn -> maybeNode) = ((("--include":) . (:[]) . Arg . fromNodeName) <$> maybeNode & fromMaybe [])


-- * Deployment lifecycle
--
exists :: Options -> NixopsConfig -> IO Bool
exists o c@NixopsConfig{..} = do
  (code, _) <- nixops' o c "info" []
  pure $ code == ExitSuccess

create :: Options -> NixopsConfig -> IO ()
create o c@NixopsConfig{..} = do
  deplExists <- exists o c
  if deplExists
  then do
    printf ("Deployment already exists?: '"%s%"'") $ fromNixopsDepl cName
  else do
    printf ("Creating deployment "%s%"\n") $ fromNixopsDepl cName
    nixops o c "create" $ Arg <$> deploymentFiles cEnvironment cTarget cElements

buildGlobalsImportNixExpr :: [(NixParam, NixValue)] -> NixValue
buildGlobalsImportNixExpr deplArgs =
  NixImport (NixFile "globals.nix")
  $ NixAttrSet $ Map.fromList $ (fromNixParam *** id) <$> deplArgs

computeFinalDeplArgs :: NixopsConfig -> [(NixParam, NixValue)]
computeFinalDeplArgs NixopsConfig{..} =
  let deplArgs = Map.toList cDeplArgs
                 <> [("topologyYaml", NixFile cTopology)
                    ,("environment",  NixStr  $ lowerShowT cEnvironment)]
  in ("globals", buildGlobalsImportNixExpr deplArgs): deplArgs

configDeployerIP :: NixopsConfig -> IP
configDeployerIP c = (\(NixStr x) -> IP x) <$> deplArg c "deployerIP" $ error "Asked to deploy a cluster config without 'deployerIP' set."

modify :: Options -> NixopsConfig -> IO ()
modify o@Options{..} c@NixopsConfig{..} = do
  printf ("Syncing Nix->state for deployment "%s%"\n") $ fromNixopsDepl cName
  nixops o c "modify" $ Arg <$> cFiles

  let deplArgs = computeFinalDeplArgs c
  printf ("Setting deployment arguments:\n")
  forM_ deplArgs $ \(name, val)
    -> printf ("  "%s%": "%s%"\n") (fromNixParam name) (nixValueStr val)
  nixops o c "set-args" $ Arg <$> (concat $ uncurry nixArgCmdline <$> deplArgs)

  printf ("Generating '"%fp%"' from '"%fp%"'..\n") simpleTopoFile cTopology
  preExisting <- testpath cTopology
  unless preExisting $
    die $ format ("Topology config '"%fp%"' doesn't exist.") cTopology
  simpleTopo <- summariseTopology <$> readTopology cTopology
  liftIO . writeTextFile simpleTopoFile . T.pack . LBU.toString $ encodePretty simpleTopo
  when oDebug $ dumpTopologyNix c

deploy :: Options -> NixopsConfig -> Bool -> Bool -> Bool -> Bool -> Maybe Seconds -> IO ()
deploy o@Options{..} c@NixopsConfig{..} evonly buonly check rebuildExplorerFrontend bumpSystemStartHeldBy = do
  when (elem Nodes cElements) $ do
     keyExists <- testfile "keys/key1.sk"
     unless keyExists $
       die "Deploying nodes, but 'keys/key1.sk' is absent."

  when (not evonly && elem Explorer cElements && rebuildExplorerFrontend) $ do
    cmd o "scripts/generate-explorer-frontend.sh" []
  when (not (evonly || buonly)) $ do
    export "SMART_GEN_IP" $ getIP $ configDeployerIP c
    when (elem Nodes cElements) $ do
      export "GC_INITIAL_HEAP_SIZE" (showT $ 8 * 1024*1024*1024) -- for 100 nodes it eats 12GB of ram *and* needs a bigger heap

  now <- timeCurrent
  let startParam             = NixParam "systemStart"
      secNixVal (Elapsed x)  = NixInt $ fromIntegral x
      holdSecs               = fromMaybe defaultHold bumpSystemStartHeldBy
      nowHeld                = now `timeAdd` mempty { durationSeconds = holdSecs }
      startE                 = case bumpSystemStartHeldBy of
        Just _  -> nowHeld
        Nothing -> Elapsed $ fromIntegral $ (\(NixInt x)-> x) $ deplArg c startParam (secNixVal nowHeld)
      c' = setDeplArg c startParam $ secNixVal startE
  when (isJust bumpSystemStartHeldBy) $ do
    let timePretty = (T.pack $ timePrint ISO8601_DateAndTime (timeFromElapsed startE :: DateTime))
    printf ("Setting --system-start to "%s%" ("%d%" minutes into future)\n")
           timePretty (div holdSecs 60)
    cFp <- writeConfig oConfigFile c'
    cmd o "git" (["add", format fp cFp])
    cmd o "git" ["commit", "-m", format ("Bump systemStart to "%s) timePretty]

  modify o c'

  printf ("Deploying cluster "%s%"\n") $ fromNixopsDepl cName
  nixops o c' "deploy"
    $  [ "--max-concurrent-copy", "50", "-j", "4" ]
    ++ [ "--evaluate-only" | evonly ]
    ++ [ "--build-only"    | buonly ]
    ++ [ "--check"         | check  ]
    ++ nixopsMaybeLimitNodes o
  echo "Done."

deployValidate :: Options -> NixopsConfig -> Bool -> IO ()
deployValidate o c _rebuildExplorerFrontend = do
  deploy o c True  False False False                   Nothing
  -- deploy o c False True  False rebuildExplorerFrontend Nothing

destroy :: Options -> NixopsConfig -> IO ()
destroy o c@NixopsConfig{..} = do
  printf ("Destroying cluster "%s%"\n") $ fromNixopsDepl cName
  nixops (o { oConfirm = True }) c "destroy"
    $ nixopsMaybeLimitNodes o
  echo "Done."

delete :: Options -> NixopsConfig -> IO ()
delete o c@NixopsConfig{..} = do
  printf ("Un-defining cluster "%s%"\n") $ fromNixopsDepl cName
  nixops (o { oConfirm = True }) c "delete"
    $ nixopsMaybeLimitNodes o
  echo "Done."

fromscratch :: Options -> NixopsConfig -> IO ()
fromscratch o c = do
  destroy o c
  delete o c
  create o c
  deploy o c False False False True (Just defaultHold)


-- * Building
--
prefetchURL :: Options -> Project -> Commit -> IO (NixHash, FilePath)
prefetchURL o proj rev = do
  let url = projectURL proj
  hashPath <- incmd o "nix-prefetch-url" ["--unpack", "--print-path", (fromURL $ url) <> "/archive/" <> fromCommit rev <> ".tar.gz"]
  let hashPath' = T.lines hashPath
  pure (NixHash (hashPath' !! 0), Path.fromText $ hashPath' !! 1)

runSetRev :: Options -> Project -> Commit -> Maybe Text -> IO ()
runSetRev o proj rev mCommitChanges = do
  printf ("Setting '"%s%"' commit to "%s%"\n") (lowerShowT proj) (fromCommit rev)
  let url = projectURL proj
  (hash, _) <- prefetchURL o proj rev
  printf ("Hash is"%s%"\n") (showT hash)
  let revspecFile = format fp $ projectSrcFile proj
      revSpec = GitSource{ gRev             = rev
                         , gUrl             = url
                         , gSha256          = hash
                         , gFetchSubmodules = True }
  writeFile (T.unpack $ revspecFile) $ LBU.toString $ encodePretty revSpec
  case mCommitChanges of
    Nothing  -> pure ()
    Just msg -> do
      cmd o "git" (["add", revspecFile])
      cmd o "git" ["commit", "-m", msg]

runFakeKeys :: IO ()
runFakeKeys = do
  echo "Faking keys/key*.sk"
  testdir "keys"
    >>= flip unless (mkdir "keys")
  forM_ ([0..41]) $
    (\x-> do touch $ Turtle.fromText $ format ("keys/key"%d%".sk") x)
  echo "Minimum viable keyset complete."

ensureCardanoBranchCheckout :: Options -> Branch -> FilePath -> IO Commit
ensureCardanoBranchCheckout o branch dir = do
  preExisting <- testdir dir
  if preExisting
  then do
    cd dir
    printf ("Updating local checkout of 'cardano-sl' to the tip of 'origin/"%s%"'..\n") (fromBranch branch)
    cmd o "git" ["fetch", "origin"]
    cmd o "git" ["checkout", "-B", fromBranch branch, "origin/" <> fromBranch branch]
  else do
    printf ("Cloning 'cardano-sl' branch "%s%"\n") (fromBranch branch)
    cmd o "git" ["clone", "--branch", fromBranch branch, fromURL $ projectURL CardanoSL, "cardano-sl"]
    cd dir
  commit <- gitHEADCommit o
  cd ".."
  pure commit

-- | Generate genesis driving the tip of specified 'cardano-sl' branch.
generateGenesis :: Options -> NixopsConfig -> Branch -> IO Text
generateGenesis o NixopsConfig{..} cardanoBranch = do
  let cardanoSLDir     = "cardano-sl"
      genesisName      = "genesis" :: Text
      genesisTarball   = genesisName <> ".tgz"
      genSuffix        = "tn"
      (,) genM genN    = (,) (topoNCores topology) 1200
      genFiles         = [ "core/genesis-core-"%s%".bin"
                         , "genesis-info/"%s%".log"
                         , "godtossing/genesis-godtossing-"%s%".bin" ] <&> flip format genSuffix
  cardanoCommit <- ensureCardanoBranchCheckout o cardanoBranch cardanoSLDir
  printf ("Generating genesis using cardano-sl branch '"%s%"' (commit "%s%")\n  M:"%d%"\n  N:"%d%"\n")
    (fromBranch cardanoBranch) (fromCommit cardanoCommit) genM genN
  cardanoGenesisCommit <- do
    cd cardanoSLDir
    cmd o "rm" ["-rf", genesisName, genesisTarball]
    cmd o "scripts/generate/genesis.sh"
      [ "--build-mode",        "nix"
      , "--install-as-suffix",  genSuffix
      , "--rich-keys",          showT genM
      , "--poor-keys",          showT genN
      , "--output-dir",         genesisName]
    cmd o "git" (["add"] <> genFiles)
    cmd o "git" ["commit", "-m", format ("Regenerate genesis, M="%d%", N="%d) genM genN]
    echo "Genesis generated and committed, bumping 'iohk-ops'"
    cardanoGenesisCommit <- gitHEADCommit o
    printf ("Committed new genesis as 'cardano-sl' commit "%s%", pushing to 'origin'\n") (fromCommit cardanoGenesisCommit)
    cmd o "git" ["push", "--force", "origin"]
    cd ".."
    pure cardanoGenesisCommit
  runSetRev o CardanoSL cardanoGenesisCommit (Just $ format ("Bump cardano: Regenerated genesis, M="%d%", N="%d%", cardano="%s) genM genN (fromCommit cardanoCommit))
  cmd o "git" ["push", "--force", "origin"]
  echo "Don't forget to archive and install the keys from our new genesis:"
  cmd o "ls" ["-l", format (fp%"/"%s) cardanoSLDir genesisTarball]
  pure genesisName

-- | Deploy the specified 'cardano-sl' branch, possibly with new genesis.
deployStaging :: Options -> NixopsConfig -> Branch -> Seconds -> Bool -> Bool -> Bool -> IO ()
deployStaging o@Options{..} c@NixopsConfig{..} cardanoBranchToDrive bumpHeldBy doGenesis rebuildExplorerFrontend skipValidation = do
  let cardanoSLDir      = "cardano-sl"
      deployerUser      = "staging"
      deploymentAccount = deployerUser <> "@" <> (getIP $ configDeployerIP c)
      deploymentDir     = fromNixopsDepl cName
      deploymentSource  = format (s%":"%s%"/") deploymentAccount deploymentDir
      rsh               = if oMosh then "mosh" else "ssh"
  -- 0. Validate
  unless skipValidation $ do
    echo "Validating 'iohk-ops' checkout.."
    create o c
    deployValidate o c rebuildExplorerFrontend
    -- XXX: re-enable this.. after figuring out how to use local 'iohk-ops'..
    -- cmd o "bash" ["scripts/travis.sh", "iohk-ops", format fp cNixops]
    -- cmd o "nix-build" ["--keep-failed", "jobsets/cardano.nix", "-A", "tests.simpleNode.x86_64-linux", "--show-trace"]
  -- 1. Bump Cardano
  branchHEAD <- ensureCardanoBranchCheckout o cardanoBranchToDrive cardanoSLDir
  GitSource{..} <- readSource gitSource CardanoSL
  unless (branchHEAD == gRev) $ do
    runSetRev o CardanoSL branchHEAD (Just $ format ("Bump cardano-sl to:  "%s) (fromCommit branchHEAD))
  -- 2. Record first bump and trigger CI
  printf ("Pushing local 'iohk-ops' commit into 'origin'..\n")
  cmd o "git" ["push", "--force", "origin"]
  -- 3. Genesis
  when doGenesis $ do
    echo "Genesis regeneration requested.."
    genesisName <- generateGenesis o c cardanoBranchToDrive
    let genesisTarball = genesisName <> ".tgz"
        richKeys       = topoNCores topology
    printf ("Updating deployer genesis: "%s%"\n") deploymentSource
    echo "  -- removing old genesis"
    cmd o "ssh" [ deploymentAccount, "bash", "-c", format ("'rm -rf ./"%s%" ./"%s%"'") genesisTarball genesisName]
    echo "  -- uploading generated genesis archive"
    cmd o "scp" [ format (fp%"/"%s) cardanoSLDir genesisTarball
                , deploymentSource ]
    echo "  -- unpacking genesis archive"
    cmd o "ssh" [ deploymentAccount, "tar", "xaf", deploymentDir <> "/" <> genesisTarball, "-C", deploymentDir ]
    echo "  -- removing old rich keys"
    cmd o "ssh" [ deploymentAccount, "bash", "-c"
                , format ("'cd "%s%" && rm -rf ./keys/*'") deploymentDir]
    echo "  -- installing genesis rich keys"
    cmd o "ssh" [ deploymentAccount, "bash", "-c"
                , format ("'cd "%s%" && for x in {1.."%d%"}; do cp "%s%"/keys-testnet/rich/testnet$x.key keys/key$((x-1)).sk; done'")
                  deploymentDir richKeys genesisName]
  -- 4. Wait for CI (either after first or second bump)
  echo "CI status:  https://hydra.iohk.io/jobset/serokell/iohk-nixops-staging#tabs-evaluations"
  opsCommit <- gitHEADCommit o
  printf ("\nPress 'Enter' when CI has built the evaluation for 'iohk-ops' commit "%s%"\n") (fromCommit opsCommit)
  void readline
  -- 5. Deploy
  printf ("Updating deployment source: "%s%"\n") deploymentSource
  cmd o "ssh"   [ deploymentAccount, "git", "-C", deploymentDir, "fetch", "origin" ]
  cmd o "ssh"   [ deploymentAccount, "git", "-C", deploymentDir, "reset", "--hard", fromCommit opsCommit ]
  cmd o  rsh  $ [ "--", deploymentAccount, "bash",  "-c",
                  format ("'echo && cd ~/"%s%" && pwd && $(nix-build -A iohk-ops default.nix)/bin/iohk-ops --config "%fp%" deploy-staging-phase1 --bump-system-start-held-by "%d%" "%s%" "%s%"'")
                  deploymentDir (fromJust oConfigFile) (bumpHeldBy `div` 60)
                  (if doGenesis               then "--wipe-node-dbs" else "")
                  (if rebuildExplorerFrontend then "" else "--no-explorer-rebuild")
                ]

deployStagingPhase1 :: Options -> NixopsConfig -> Seconds -> Bool -> Bool -> IO ()
deployStagingPhase1 o c@NixopsConfig{..} bumpHeldBy doWipeNodeDBs _rebuildExplorerFrontend = do
  -- 0. If we have nixpkgs commit set in the config, propagate
  nixpkgsPath <- case cNixpkgs of
    Nothing -> pure Nothing
    Just commit -> do
      (_, storePath) <- prefetchURL o Nixpkgs commit
      pure $ Just storePath
  let options' = o { oNixpkgs = nixpkgsPath }

  -- 1. --evaluate-only
  deploy options' c True False False rebuildExplorerFrontend Nothing

  -- 2. cleanup
  stop o c
  wipeJournals o c
  when doWipeNodeDBs $
    wipeNodeDBs o c Confirm

  -- 3. for real
  deploy options' c False False False False (Just bumpHeldBy)

deploymentBuildTarget :: Deployment -> NixAttr
deploymentBuildTarget Nodes = "cardano-sl-static"
deploymentBuildTarget x     = error $ "'deploymentBuildTarget' has no idea what to build for " <> show x

build :: Options -> NixopsConfig -> Deployment -> IO ()
build o _c depl = do
  echo "Building derivation..."
  cmd o "nix-build" ["--max-jobs", "4", "--cores", "2", "-A", fromAttr $ deploymentBuildTarget depl]


-- * State management
--
-- Check if nodes are online and reboots them if they timeout
checkstatus :: Options -> NixopsConfig -> IO ()
checkstatus o c = do
  parallelIO o c $ rebootIfDown o c

rebootIfDown :: Options -> NixopsConfig -> NodeName -> IO ()
rebootIfDown o c (Arg . fromNodeName -> node) = do
  (x, _) <- nixops' o c "ssh" $ (node : ["-o", "ConnectTimeout=5", "echo", "-n"])
  case x of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      TIO.putStrLn $ "Rebooting " <> fromArg node
      nixops o c "reboot" ["--include", node]

ssh  :: Options -> NixopsConfig -> Exec -> [Arg] -> NodeName -> IO ()
ssh o c e a n = ssh' o c e a n (TIO.putStr . ((fromNodeName n <> "> ") <>))

ssh' :: Options -> NixopsConfig -> Exec -> [Arg] -> NodeName -> (Text -> IO ()) -> IO ()
ssh' o c exec args (fromNodeName -> node) postFn = do
  let cmdline = Arg node: "--": Arg (fromExec exec): args
  (exitcode, out) <- nixops' o c "ssh" cmdline
  postFn out
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> TIO.putStrLn $ "ssh cmd '" <> (T.intercalate " " $ fromArg <$> cmdline) <> "' to '" <> node <> "' failed with " <> showT code

parallelSSH :: Options -> NixopsConfig -> Exec -> [Arg] -> IO ()
parallelSSH o c@NixopsConfig{..} ex as = do
  parallelIO o c $
    ssh o c ex as

scpFromNode :: Options -> NixopsConfig -> NodeName -> Text -> Text -> IO ()
scpFromNode o c (fromNodeName -> node) from to = do
  (exitcode, _) <- nixops' o c "scp" $ Arg <$> ["--from", node, from, to]
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> TIO.putStrLn $ "scp from " <> node <> " failed with " <> showT code

sshForEach :: Options -> NixopsConfig -> [Text] -> IO ()
sshForEach o c command =
  nixops o c "ssh-for-each" $ Arg <$> "--": command

deployedCommit :: Options -> NixopsConfig -> NodeName -> IO ()
deployedCommit o c m = do
  ssh' o c "pgrep" ["-fa", "cardano-node"] m $
    \r-> do
      case cut space r of
        (_:path:_) -> do
          drv <- incmd o "nix-store" ["--query", "--deriver", T.strip path]
          pathExists <- testpath $ fromText $ T.strip drv
          unless pathExists $
            errorT $ "The derivation used to build the package is not present on the system: " <> T.strip drv
          sh $ do
            str <- inproc "nix-store" ["--query", "--references", T.strip drv] empty &
                   inproc "egrep"       ["/nix/store/[a-z0-9]*-cardano-sl-[0-9a-f]{7}\\.drv"] &
                   inproc "sed" ["-E", "s|/nix/store/[a-z0-9]*-cardano-sl-([0-9a-f]{7})\\.drv|\\1|"]
            when (str == "") $
              errorT $ "Cannot determine commit id for derivation: " <> T.strip drv
            echo $ "The 'cardano-sl' process running on '" <> unsafeTextToLine (fromNodeName m) <> "' has commit id " <> str
        [""] -> errorT $ "Looks like 'cardano-node' is down on node '" <> fromNodeName m <> "'"
        _    -> errorT $ "Unexpected output from 'pgrep -fa cardano-node': '" <> r <> "' / " <> showT (cut space r)


startForeground :: Options -> NixopsConfig -> NodeName -> IO ()
startForeground o c node =
  ssh' o c "bash" [ "-c", "'systemctl show cardano-node --property=ExecStart | sed -e \"s/.*path=\\([^ ]*\\) .*/\\1/\" | xargs grep \"^exec \" | cut -d\" \" -f2-'"]
  node $ \unitStartCmd ->
    printf ("Starting Cardano in foreground;  Command line:\n  "%s%"\n") unitStartCmd >>
    ssh o c "bash" ["-c", Arg $ "'sudo -u cardano-node " <> unitStartCmd <> "'"] node

stop :: Options -> NixopsConfig -> IO ()
stop o c = echo "Stopping nodes..."
  >> parallelSSH o c "systemctl" ["stop", "cardano-node"]

defLogs, profLogs :: [(Text, Text -> Text)]
defLogs =
    [ ("/var/lib/cardano-node/node.log", (<> ".log"))
    , ("/var/lib/cardano-node/jsonLog.json", (<> ".json"))
    , ("/var/lib/cardano-node/time-slave.log", (<> "-ts.log"))
    , ("/var/log/saALL", (<> ".sar"))
    ]
profLogs =
    [ ("/var/lib/cardano-node/cardano-node.prof", (<> ".prof"))
    , ("/var/lib/cardano-node/cardano-node.hp", (<> ".hp"))
    -- in fact, if there's a heap profile then there's no eventlog and vice versa
    -- but scp will just say "not found" and it's all good
    , ("/var/lib/cardano-node/cardano-node.eventlog", (<> ".eventlog"))
    ]

start :: Options -> NixopsConfig -> IO ()
start o c =
  parallelSSH o c "bash" ["-c", Arg $ "'" <> rmCmd <> "; " <> startCmd <> "'"]
  where
    rmCmd = foldl (\str (f, _) -> str <> " " <> f) "rm -f" logs
    startCmd = "systemctl start cardano-node"
    logs = mconcat [ defLogs, profLogs ]

date :: Options -> NixopsConfig -> IO ()
date o c = parallelIO o c $
  \n -> ssh' o c "date" [] n
  (\out -> TIO.putStrLn $ fromNodeName n <> ": " <> out)

wipeJournals :: Options -> NixopsConfig -> IO ()
wipeJournals o c@NixopsConfig{..} = do
  echo "Wiping journals on cluster.."
  parallelSSH o c "bash"
    ["-c", "'systemctl --quiet stop systemd-journald && rm -f /var/log/journal/*/* && systemctl start systemd-journald && sleep 1 && systemctl restart nix-daemon'"]
  echo "Done."

getJournals :: Options -> NixopsConfig -> IO ()
getJournals o c@NixopsConfig{..} = do
  let nodes = nodeNames o c

  echo "Dumping journald logs on cluster.."
  parallelSSH o c "bash"
    ["-c", "'rm -f log && journalctl -u cardano-node > log'"]

  echo "Obtaining dumped journals.."
  let outfiles  = format ("log-cardano-node-"%s%".journal") . fromNodeName <$> nodes
  parallelIO' o c (flip zip outfiles) $
    \(node, outfile) -> scpFromNode o c node "log" outfile
  timeStr <- T.pack . timePrint ISO8601_DateAndTime <$> dateCurrent

  let archive   = format ("journals-"%s%"-"%s%"-"%s%".tgz") (lowerShowT cEnvironment) (fromNixopsDepl cName) timeStr
  printf ("Packing journals into "%s%"\n") archive
  cmd o "tar" (["czf", archive, "--force-local"] <> outfiles)
  cmd o "rm" $ "-f" : outfiles
  echo "Done."

wipeNodeDBs :: Options -> NixopsConfig -> Confirmation -> IO ()
wipeNodeDBs o c@NixopsConfig{..} confirmation = do
  confirmOrTerminate confirmation
  echo "Wiping node databases.."
  parallelSSH o c "rm" ["-rf", "/var/lib/cardano-node"]
  echo "Done."

updateNixops :: Options -> NixopsConfig -> IO ()
updateNixops o@Options{..} c@NixopsConfig{..} = do
  let (,) nixopsDir outLink = (,) "nixops" ("nixops-link" :: FilePath)
      configFile = flip fromMaybe oConfigFile $
        error "The 'update-nixops' subcommand requires the -c/--config option to 'iohk-ops'."
  preExists <- testpath nixopsDir
  unless preExists $ do
    errorT $ format ("The 'update-nixops' subcommand requires a '"%fp%"' subdirectory as input.") nixopsDir
  cd nixopsDir
  cmd o "nix-build" ["-A", "build.x86_64-linux", "--out-link", "../" <> format fp outLink, "release.nix"]
  sh $ do
    gitHeadRev <- inproc "git" ["rev-parse", "HEAD"] empty
    cd ".."
    nixopsStorePath <- inproc "readlink" [format fp outLink] empty
    liftIO $ printf ("Built nixops commit '"%s%"' is at '"%s%"', updating config '"%fp%"'\n")
      (lineToText gitHeadRev) (lineToText nixopsStorePath) configFile
    writeConfig (Just configFile) $ c { cNixops = Path.fromText $ lineToText nixopsStorePath <> "/bin/nixops" }
    -- Unfortunately, Turtle doesn't seem to provide anything of the form Shell a -> IO a,
    -- that would allow us to smuggle non-Text values out of a Shell monad.
  echo "Done."


-- * Functions for extracting information out of nixops info command
--
-- | Get all nodes in EC2 cluster
data DeploymentStatus = UpToDate | Obsolete | Outdated
  deriving (Show, Eq)

instance FromField DeploymentStatus where
  parseField "up-to-date" = pure UpToDate
  parseField "obsolete" = pure Obsolete
  parseField "outdated" = pure Outdated
  parseField _ = mzero

data DeploymentInfo = DeploymentInfo
    { diName :: !NodeName
    , diStatus :: !DeploymentStatus
    , diType :: !Text
    , diResourceID :: !Text
    , diPublicIP :: !IP
    , diPrivateIP :: !IP
    } deriving (Show, Generic)

instance FromRecord DeploymentInfo
deriving instance FromField NodeName

nixopsDecodeOptions = defaultDecodeOptions {
    decDelimiter = fromIntegral (ord '\t')
  }

info :: Options -> NixopsConfig -> IO (Either String (V.Vector DeploymentInfo))
info o c = do
  (exitcode, nodes) <- nixops' o c "info" ["--no-eval", "--plain"]
  case exitcode of
    ExitFailure code -> return $ Left ("Parsing info failed with exit code " <> show code)
    ExitSuccess -> return $ decodeWith nixopsDecodeOptions NoHeader (encodeUtf8 $ fromStrict nodes)

toNodesInfo :: V.Vector DeploymentInfo -> [DeploymentInfo]
toNodesInfo vector =
  V.toList $ V.filter filterEC2 vector
    where
      filterEC2 di = T.take 4 (diType di) == "ec2 " && diStatus di /= Obsolete

getNodePublicIP :: Text -> V.Vector DeploymentInfo -> Maybe Text
getNodePublicIP name vector =
    headMay $ V.toList $ fmap (getIP . diPublicIP) $ V.filter (\di -> fromNodeName (diName di) == name) vector


-- * Utils
showT :: Show a => a -> Text
showT = T.pack . show

readT :: Read a => Text -> a
readT = read . T.unpack

lowerShowT :: Show a => a -> Text
lowerShowT = T.toLower . T.pack . show

errorT :: Text -> a
errorT = error . T.unpack

jsonLowerStrip :: (Generic a, AE.GToJSON AE.Zero (Rep a)) => Int -> a -> AE.Value
jsonLowerStrip n = AE.genericToJSON $ AE.defaultOptions { AE.fieldLabelModifier = map toLower . drop n }
