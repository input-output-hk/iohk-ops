{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}

module UpdateProposal
  ( UpdateProposalCommand(..)
  , parseUpdateProposalCommand
  , updateProposal
  ) where

import Prelude hiding (FilePath)
import Options.Applicative
import Turtle hiding (Parser, switch, option)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, iso8601DateFormat, defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import Filesystem.Path.CurrentOS (encodeString)
import Data.Yaml (decodeFileEither, encodeFile)
import Data.Aeson
import qualified Control.Foldl as Fold
import Data.Char (isHexDigit)
import Data.Bifunctor (first)

import NixOps (NixopsConfig(..), nixopsConfigurationKey)
import Types (NixopsDepl(..), Environment(..))


----------------------------------------------------------------------------
-- Command-line arguments

data UpdateProposalCommand
  = UpdateProposalInit
    { updateProposalDate              :: Maybe Text
    , updateProposalInitialConfig     :: Either Text UpdateProposalConfig1
    }
  | UpdateProposalFindInstallers
    { updateProposalDate              :: Maybe Text
    , updateProposalCardanoSourcePath :: FilePath
    }
  | UpdateProposalUploadS3
    { updateProposalDate              :: Maybe Text
    }
  | UpdateProposalGenerate
    { updateProposalDate              :: Maybe Text
    , updateProposalCardanoSourcePath :: FilePath
    }
  | UpdateProposalSubmit
    { updateProposalDate              :: Maybe Text
    , updateProposalCardanoSourcePath :: FilePath
    , updateProposalRelayIP           :: Text
    , updateProposalDryRun            :: Bool
    }
  deriving Show

parseUpdateProposalCommand :: Parser UpdateProposalCommand
parseUpdateProposalCommand = subparser $
     ( command "init"
       (info ((UpdateProposalInit <$> date <*> initialInfo) <**> helper)
         (progDesc "Create template config file and working directory.") ) )
  <> ( command "find-installers"
       (info ((UpdateProposalFindInstallers <$> date <*> cslPath) <**> helper)
         (progDesc "Download installer files from the Daedalus build.") ) )
  <> ( command "upload-s3"
       (info ((UpdateProposalUploadS3 <$> date) <**> helper)
         (progDesc "Upload installer files to the S3 bucket.") ) )
  <> ( command "generate"
       (info ((UpdateProposalGenerate <$> date <*> cslPath) <**> helper)
         (progDesc "Create a voting transaction") ) )
  <> ( command "submit"
       (info (UpdateProposalSubmit <$> date <*> cslPath <*> relayIP <*> dryRun)
         (progDesc "Send update proposal transaction to the network") ) )
  where
    date :: Parser (Maybe Text)
    date = fmap (fmap fromString) . optional . argument str $
              metavar "DATE"
              <> help "Date string to identify the update proposal (default: today)"

    initialInfo :: Parser (Either Text UpdateProposalConfig1)
    initialInfo = (Left <$> fromDate) <|> (Right <$> updateProposalConfig)

    updateProposalConfig :: Parser UpdateProposalConfig1
    updateProposalConfig = UpdateProposalConfig1
      <$> ( option (eitherReader (gitRevision . T.pack))
            (long "revision" <> short 'r' <> metavar "SHA1"
              <> help "Daedalus revision to fetch") )
      <*> ( option auto (long "block-version" <> short 'b' <> metavar "VERSION"
           <> help "Last known block version. Check the wiki for more info.") )
      <*> ( option auto (long "voter-index" <> short 'v' <> metavar "INTEGER"
           <> help "A number representing you, the vote proposer. Check the wiki for more info.") )

    fromDate :: Parser Text
    fromDate = option auto $
               long "from" <> short 'f' <> metavar "DATE"
               <> help "Copy the previous config from date"

    cslPath :: Parser FilePath
    cslPath = fmap fromString . strOption $
              long "cardano-sl" <> metavar "PATH" <> value "./cardano-sl"
              <> help "Path to clone of cardano-sl (default: ./cardano-sl)"

    relayIP :: Parser Text
    relayIP = option auto $
              long "relay-ip" <> short 'r' <> metavar "ADDR"
              <> help "IP address of privileged relay"

    dryRun :: Parser Bool
    dryRun = switch ( long "dry-run" <> short 'n' <> help "Don't actually do anything" )

updateProposal :: NixopsConfig -> UpdateProposalCommand -> IO ()
updateProposal cfg up = do
  configKey <- maybe (fail "configurationKey not found") pure (nixopsConfigurationKey cfg)
  top <- pwd
  uid <- makeUpdateId (cName cfg) (updateProposalDate up)
  sh $ case up of
    UpdateProposalInit _ initial -> updateProposalInit top uid (first (UpdateID (cName cfg)) initial)
    UpdateProposalFindInstallers _ cslPath -> updateProposalFindInstallers
      (commandOptions (workPath top uid) cslPath configKey Nothing False (cUpdateBucket cfg))
      (cEnvironment cfg)
    UpdateProposalUploadS3 _ cslPath -> updateProposalUploadS3
      (commandOptions (workPath top uid) "none" configKey Nothing False (cUpdateBucket cfg))
    UpdateProposalGenerate _ cslPath -> updateProposalGenerate
      (commandOptions (workPath top uid) cslPath configKey Nothing False (cUpdateBucket cfg))
    UpdateProposalSubmit _ cslPath relay dryRun -> updateProposalSubmit
      (commandOptions (workPath top uid) cslPath configKey (Just relay) dryRun (cUpdateBucket cfg))

----------------------------------------------------------------------------
-- Parameters files. These are loaded/saved to yaml in the work dir.
-- There are three versions, for each step in the update proposal.

data UpdateProposalConfig1 = UpdateProposalConfig1
  { cfgDaedalusRevision      :: GitRevision
  , cfgLastKnownBlockVersion :: Text
  , cfgVoterIndex            :: Int
  } deriving (Show)

data UpdateProposalConfig2 = UpdateProposalConfig2
  { cfgUpdateProposal1    :: UpdateProposalConfig1
  , cfgApplicationVersion :: Int
  , cfgInstallers         :: InstallersConfig
  } deriving (Show)

data InstallersConfig = InstallersConfig
  { cfgInstallerDarwin  :: Text
  , cfgInstallerWindows :: Text
  } deriving (Show)

data UpdateProposalConfig3 = UpdateProposalConfig3
  { cfgUpdateProposal2 :: UpdateProposalConfig2
  , cfgUpdateProposalAddrs :: Text
  } deriving (Show)

data UpdateProposalConfig4 = UpdateProposalConfig4
  { cfgUpdateProposal3 :: UpdateProposalConfig3
  , cfgUpdateProposalId :: Text
  } deriving (Show)

instance FromJSON UpdateProposalConfig1 where
  parseJSON = withObject "UpdateProposalConfig1" $ \o ->
    UpdateProposalConfig1 <$> o .: "daedalusRevision"
                          <*> o .: "lastKnownBlockVersion"
                          <*> o .: "voterIndex"

instance FromJSON GitRevision where
  parseJSON = withText "SHA1" parseGitRevision

instance FromJSON UpdateProposalConfig2 where
  parseJSON = withObject "UpdateProposalConfig2" $ \o ->
    UpdateProposalConfig2 <$> parseJSON (Object o) <*> o .: "applicationVersion" <*> o .: "installerHash"

instance FromJSON UpdateProposalConfig3 where
  parseJSON = withObject "UpdateProposalConfig3" $ \o ->
    UpdateProposalConfig3 <$> parseJSON (Object o) <*> o .:? "addrs" .!= ""

instance FromJSON UpdateProposalConfig4 where
  parseJSON = withObject "UpdateProposalConfig4" $ \o ->
    UpdateProposalConfig4 <$> parseJSON (Object o) <*> o .: "proposalId"

instance FromJSON InstallersConfig where
  parseJSON = withObject "InstallersConfig" $ \o ->
    InstallersConfig <$> o .: "darwin" <*> o .: "windows"

instance ToJSON UpdateProposalConfig1 where
  toJSON (UpdateProposalConfig1 r v p) = object [ "daedalusRevision" .= r
                                                , "lastKnownBlockVersion" .= v
                                                , "voterIndex" .= p ]

instance ToJSON GitRevision where
  toJSON (GitRevision r) = String r

instance ToJSON UpdateProposalConfig2 where
  toJSON (UpdateProposalConfig2 p a i)
    = mergeObjects (toJSON p) (object [ "applicationVersion" .= a, "installerHash" .= i ])

instance ToJSON UpdateProposalConfig3 where
  toJSON (UpdateProposalConfig3 p a)
    = mergeObjects (toJSON p) (object [ "addrs" .= a ])

instance ToJSON UpdateProposalConfig4 where
  toJSON (UpdateProposalConfig4 p a)
    = mergeObjects (toJSON p) (object [ "updateProposal" .= a ])

instance ToJSON InstallersConfig where
  toJSON (InstallersConfig d w) = object [ "darwin" .= d, "windows" .= w ]

-- | Adds two json objects together.
mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object (a <> b)
mergeObjects _ b = b

----------------------------------------------------------------------------
-- Loading and saving the params file

loadParams :: (Checkable cfg, FromJSON cfg) => CommandOptions -> Shell cfg
loadParams opts = do
  let yaml = encodeString (paramsFile (cmdWorkPath opts))
  printf ("Loading update proposal parameters from "%w%"\n") yaml
  liftIO (decodeFileEither yaml) >>= \case
    Right cfg -> doCheckConfig cfg >> pure cfg
    Left e -> die $ format ("Bad config: "%w%"\n") e

storeParams :: ToJSON cfg => CommandOptions -> cfg -> Shell ()
storeParams opts params = do
  let yaml = encodeString (paramsFile (cmdWorkPath opts))
  printf ("Saving update proposal parameters to "%w%"\n") yaml
  liftIO (encodeFile yaml params)

----------------------------------------------------------------------------
-- Validation of config

-- | Polymorphic function to validate parameters.
class Checkable cfg where
  checkConfig :: cfg -> Maybe Text

instance Checkable UpdateProposalConfig1 where
  checkConfig UpdateProposalConfig1{..}
    | T.null cfgLastKnownBlockVersion = Just "Last known block version must be set"
    | cfgVoterIndex <= 0 = Just "Voter index must be set"
    | otherwise = Nothing

instance Checkable UpdateProposalConfig2 where
  checkConfig UpdateProposalConfig2{..}
    | cfgApplicationVersion <= 0 = Just "Application version must be set"
    | otherwise = Nothing

instance Checkable UpdateProposalConfig3 where
  checkConfig (UpdateProposalConfig3 p a) = checkConfig p <|> checkAddr a
    where
      checkAddr "" = Just "No addresses stored in config. Has the generate step been run?"
      checkAddr _ = Nothing

instance Checkable InstallersConfig where
  checkConfig (InstallersConfig d w)
    | T.null d || T.null w = Just "Need to set installer hashes"
    | not (isInstallerHash d) = Just "Bad hash for darwin installer"
    | not (isInstallerHash w) = Just "Bad hash for windows installer"
    | otherwise = Nothing

-- | Installer hashes are 64 hex digits.
isInstallerHash :: Text -> Bool
isInstallerHash t = T.length t == 64 && T.all isHexDigit t

-- | Wrapper for sha1 hash.
newtype GitRevision = GitRevision { unGitRevision :: Text } deriving (Show, Eq)

-- | Validates sha1 hash text.
gitRevision :: Text -> Either String GitRevision
gitRevision t | T.length t /= 40 = Left "SHA1 revision is not 40 characters"
              | T.any (not . isHexDigit) t = Left "Revision must be all hex digits"
              | otherwise = Right (GitRevision t)

parseGitRevision :: Monad m => Text -> m GitRevision
parseGitRevision t = case gitRevision t of
                       Right rev -> pure rev
                       Left e    -> fail e

-- | Die if not valid.
doCheckConfig :: (MonadIO io, Checkable cfg) => cfg -> io cfg
doCheckConfig cfg = case checkConfig cfg of
                       Nothing -> pure cfg
                       Just err -> die err

----------------------------------------------------------------------------
-- Update proposal

-- | Update ID is pair of deployment and date tag.
data UpdateID = UpdateID NixopsDepl Text
  deriving Show

-- | All the context for running an update proposal
data CommandOptions = CommandOptions
  { cmdWorkPath          :: FilePath
  , cmdCardanoConfigFile :: FilePath
  , cmdCardanoConfigKey  :: Text
  , cmdCardanoLogConfig  :: FilePath
  , cmdRelayIP           :: Maybe Text
  , cmdRelayPort         :: Int
  , cmdDryRun            :: Bool
  , cmdInstallersURL     :: Text
  } deriving Show

-- | Constructs an UpdateID, using current date as tag if not provided.
makeUpdateId :: NixopsDepl -> Maybe Text -> IO UpdateID
makeUpdateId name mdate = UpdateID name <$> maybe today pure mdate
  where
    today = fromString . fmt <$> getCurrentTime
    fmt = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

-- | Constructs a CommandOptions record.
commandOptions :: FilePath -> FilePath -> Text -> Maybe Text -> Bool -> Text -> CommandOptions
commandOptions cmdWorkPath cardanoSource cmdCardanoConfigKey cmdRelayIP cmdDryRun updateBucket
  = CommandOptions{..}
  where
    cmdCardanoConfigFile = cardanoSource </> "lib/configuration.yaml"
    cmdCardanoLogConfig = cardanoSource </> "log-configs/cluster.yaml"
    cmdInstallersURL = "https://" <> updateBucket <> "/"
    cmdRelayPort = 3000 -- hard-coded for now

----------------------------------------------------------------------------
-- Paths in the work directory

fmtUpdatePath :: UpdateID -> FilePath
fmtUpdatePath (UpdateID (NixopsDepl name) date) = fromText (name <> "-" <> date)

workPath :: FilePath -> UpdateID -> FilePath
workPath topDir uid = topDir </> "update-proposals" </> fmtUpdatePath uid

paramsFile :: FilePath -> FilePath
paramsFile workPath = workPath </> "params.yaml"

wikiFile :: FilePath -> FilePath
wikiFile workPath = workPath </> "wiki.md"

findKeys :: FilePath -> Shell FilePath
findKeys = Turtle.find (ends ".sk")

-- | List all the keys in the update proposal working directory.
findWorkDirKeys :: CommandOptions -> Shell FilePath
findWorkDirKeys opts = findKeys (cmdWorkPath opts </> "keys")

-- | Path to an installer (hash) in the work directory.
installersPath :: CommandOptions -> Text -> FilePath
installersPath opts hash = cmdWorkPath opts </> "installers" </> fromText hash

----------------------------------------------------------------------------

-- | Step 1. Init a new work directory
updateProposalInit :: FilePath -> UpdateID -> Either UpdateID UpdateProposalConfig1 -> Shell ()
updateProposalInit top uid initial = do
  let dir = workPath top uid
      yaml = paramsFile dir
      keysDir = top </> "keys"
      -- TODO: find installers rather than asking the user
      installers = InstallersConfig "" ""
  printf ("*** Setting up working area for "%fp%"\n") dir
  mktree dir
  testfile yaml >>= \case
    True -> die "Config file already exists, stopping."
    False -> pure ()
  case initial of
    Left fromUid -> do
      let src = paramsFile (workPath top fromUid)
      printf ("Copying from "%fp%"\n") src
      cp src yaml
    Right cfg -> do
      printf "Creating blank template\n"
      liftIO $ encodeFile (encodeString yaml) cfg
  liftIO . sh $ copyKeys keysDir (dir </> "keys")
  mktree (dir </> "installers")
  printf ("*** Now edit "%fp%" and update all fields.\n") yaml

-- | Copy secret keys out of top-level keys directory into the work directory.
copyKeys :: FilePath -> FilePath -> Shell ()
copyKeys src dst = mkdir dst >> findKeys src >>= copy
  where copy key = do
          let dst' = dst </> filename key
          printf ("Copying "%fp%" -> "%fp%"\n") key dst'
          cp key dst'

----------------------------------------------------------------------------

-- | Step 2. Find installers and download them.
updateProposalFindInstallers :: CommandOptions -> Environment -> Shell ()
updateProposalFindInstallers opts env = do
  params <- loadParams opts
  doCheckConfig (cfgInstallers params)
  echo "*** Finding installers"
  res <- realFindInstallers (unGitRevision . cfgDaedalusRevision params) (configurationKeys env)
  writeWikiRecord opts res
  storeParams opts (makeUpdateProposalConfig2 params res)

writeWikiRecord :: CommandOptions -> InstallersResults -> Shell ()
writeWikiRecord opts = do
  let md = wikiPath (cmdWorkPath opts)
  printf ("*** Writing wiki table entry to "%fp%"\n") md
  TIO.putStrLn md $ githubWikiRecord res

----------------------------------------------------------------------------

-- | Step 4. Generate database with keys, download installers.
updateProposalGenerate :: CommandOptions -> Shell ()
updateProposalGenerate opts@CommandOptions{..} = do
  params <- loadParams opts
  doCheckConfig (cfgInstallers params)
  echo "*** Downloading installers."
  downloadInstallers opts (cfgInstallers params)
  echo "*** Generating keys and database."
  addrs <- doGenerate opts params
  storeParams opts (UpdateProposalConfig3 params addrs)
  echo "*** Finished generate step. Next step is to submit."

-- | Download installers by hash and check that they are the correct type
downloadInstallers :: CommandOptions -> InstallersConfig -> Shell ()
downloadInstallers opts InstallersConfig{..} = do
  download "xar" cfgInstallerDarwin
  download "MS Windows" cfgInstallerWindows
  where
    download :: Text -> Text -> Shell ()
    download magic hash = do
      let dst = installersPath opts hash
      curl hash dst
      -- TODO: check hash after downloading
      info <- file dst
      unless (magic `T.isInfixOf` info) $
        die $ format ("Downloaded installer file "%fp%" is not of type "%s%". Instead is:\n"%s%"\n") dst magic info

    curl :: Text -> FilePath -> Shell ()
    curl hash dst = do
      let url = cmdInstallersURL opts <> hash
      printf ("Downloading from "%s%"\n") url
      procs "curl" ["--output", tt dst, url] empty

    file :: FilePath -> Shell Text
    file dst = snd <$> procStrict "file" [tt dst] empty

hashInstaller :: CommandOptions -> FilePath -> Shell Text
hashInstaller opts i = strict . limit 1 . grep isHash $ runCommands opts [cmd]
  where
    cmd = format ("hash-installer "%fp) i
    isHash = text " is " *> count 64 hexDigit

doGenerate :: CommandOptions -> UpdateProposalConfig2 -> Shell Text
doGenerate opts UpdateProposalConfig2{..} = do
  keys <- fold (findWorkDirKeys opts) Fold.list
  mapM_ (rearrangeKey opts) keys
  let cmds = map (format ("add-key "%fp%" primary:true")) keys ++ [ "listaddr" ]
  grabAddresses $ runCommands opts cmds

-- | Runs the "cardano-keygen rearrange" command on a single key file.
rearrangeKey :: MonadIO io => CommandOptions -> FilePath -> io ()
rearrangeKey opts sk = liftIO . sh $ runTool opts "cardano-keygen" ["rearrange", "--mask", tt sk]

-- | Filters just the output where it shows available addresses.
grabAddresses :: Shell Line -> Shell Text
grabAddresses = fmap grab . flip fold Fold.list
  where grab = T.unlines . dropWhile (not . T.isPrefixOf "Available addresses") . map (format l)

----------------------------------------------------------------------------

-- | Step 5. Submit update proposal.
updateProposalSubmit :: CommandOptions -> Shell ()
updateProposalSubmit opts@CommandOptions{..} = do
  echo "*** Submitting update proposal"
  params <- loadParams opts
  fmap (format l) <$> doPropose opts params >>= \case
    Just updateId -> do
      storeParams opts (UpdateProposalConfig4 params updateId)
      echo "*** Update proposal submitted!"
    Nothing -> echo "*** Submission of update proposal failed."

doPropose :: CommandOptions -> UpdateProposalConfig3 -> Shell (Maybe Line)
doPropose opts cfg = fold (runCommands opts [cmd] & grep isUpdateId) Fold.last
  where
    cmd = format upd cfgVoterIndex cfgLastKnownBlockVersion cfgApplicationVersion
          (inst cfgInstallerDarwin) (inst cfgInstallerWindows)
    upd = "propose-update "%d%" vote-all:true "%s%" ~software~csl-daedalus:"%d%" (upd-bin \"win64\" "%fp%") (upd-bin \"macos64\" "%fp%")"
    isUpdateId = count 64 hexDigit
    UpdateProposalConfig1{..} = cfgUpdateProposal1
    UpdateProposalConfig2{..} = cfgUpdateProposal2 cfg
    inst f = installersPath opts (f cfgInstallers)

----------------------------------------------------------------------------
-- Running cardano tools

runCommands :: CommandOptions -> [Text] -> Shell Line
runCommands opts@CommandOptions{..} cmds = runTool opts "cardano-auxx" args
  where
    args = auxxOpts ++ peerOpts ++ commandOpts
    auxxOpts   = [ "--log-config", tt cmdCardanoLogConfig
                 , "--logs-prefix", tt (cmdWorkPath </> "logs")
                 , "--db-path", tt (cmdWorkPath </> "db")
                 , "--mode", "with-config"
                 ]
    peerOpts   = case cmdRelayIP of
                   Just addr -> [ "--peer", format (s%":"%d) addr cmdRelayPort ]
                   Nothing -> []
    commandOpts = [ "cmd", "--commands", T.intercalate "; " cmds ]

-- | Run a cardano-sl tool with the common options passed.
-- Will capture the output, as well as printing it.
runTool :: CommandOptions -> Text -> [Text] -> Shell Line
runTool opts cmd args = do
  let args' = commonOpts opts ++ args
  printf (s%" "%s%"\n") cmd (T.intercalate " " args')
  if cmdDryRun opts
    then echo "(dry-run, doing nothing)" >> empty
    else do
      line <- inproc cmd args' empty
      echo line
      pure line

commonOpts :: CommandOptions -> [Text]
commonOpts CommandOptions{..} = [ "--system-start", "0"
                                , "--configuration-file", tt cmdCardanoConfigFile
                                , "--configuration-key", cmdCardanoConfigKey
                                ]

tt :: FilePath -> Text
tt = format fp

----------------------------------------------------------------------------
-- Testing utils, just for ghci.

-- | Build tools with nix, then adjust prepend them to PATH.
loadTools :: MonadIO io => FilePath -> io ()
loadTools dir = forM_ ["cardano-sl-tools", "cardano-sl-auxx"] $ \t -> do
  Just p <- single $ inproc "nix-build" [tt (dir </> "default.nix"), "-A", t] empty
  let bin = fromText (format l p) </> "bin"
  prependPath bin

prependPath :: MonadIO io => FilePath -> io ()
prependPath p = need "PATH" >>= \case
  Just path -> export "PATH" (tt p <> ":" <> path)
  Nothing -> pure ()
