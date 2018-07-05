{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}

module RunCardano where

import Prelude hiding (FilePath)
import Turtle hiding (f)
import qualified Control.Foldl as Fold
import Control.Monad (forM_)
import qualified Data.Text as T

import Utils (tt)

----------------------------------------------------------------------------
-- Types

-- | The context for running cardano-sl commands relating to an update
-- proposal.
data CommandOptions = CommandOptions
  { cmdWorkPath          :: FilePath
  , cmdCardanoConfigFile :: FilePath
  , cmdCardanoConfigKey  :: Text
  , cmdCardanoLogConfig  :: FilePath
  , cmdDryRun            :: Bool
  , cmdUpdateBucket      :: Text
  , cmdRelayIP           :: Maybe Text
  , cmdRelayPort         :: Int
  } deriving Show

-- | Constructs a CommandOptions record.
commandOptions :: FilePath -> FilePath -> Text -> Text -> CommandOptions
commandOptions cmdWorkPath cardanoSource cmdCardanoConfigKey cmdUpdateBucket
  = CommandOptions{..}
  where
    cmdCardanoConfigFile = cardanoSource </> "lib/configuration.yaml"
    cmdCardanoLogConfig = cardanoSource </> "log-configs/cluster.yaml"
    cmdDryRun = False
    cmdRelayIP = Nothing -- not required on all commands
    cmdRelayPort = 3000 -- hard-coded for now

----------------------------------------------------------------------------
-- Hashing

-- | Run cardano-auxx "hash-installer" command on a file and capture
-- its output.
cardanoHashInstaller :: CommandOptions -> FilePath -> Shell Text
cardanoHashInstaller opts i = runCommands opts [cmd] & grep hash & sed hash & chomp
  where
    cmd = format ("hash-installer "%fp) i
    hash = prefix (text "Hash" *> star anyChar *> text " is " *> hash256Hex)

-- | Matches 64 hex digits, which is a 256 bit value
hash256Hex :: Pattern Text
hash256Hex = fixed 64 (star hexDigit)

-- | Capture output of sha256sum from gnu coreutils.
sha256sum :: FilePath -> Shell Text
sha256sum f = inproc "sha256sum" ["--binary", tt f] empty & hash & chomp
  where hash = sed (prefix hash256Hex)

chomp :: Shell Line -> Shell Text
chomp = fmap T.stripEnd . strict . limit 1


----------------------------------------------------------------------------
-- keygen

rearrangeKeys :: CommandOptions -> [FilePath] -> Shell Text
rearrangeKeys opts keys = do
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

----------------------------------------------------------------------------
-- Testing utils, just for ghci.

-- | Build tools with nix, then adjust prepend them to PATH.
loadTools :: MonadIO io => FilePath -> io ()
loadTools dir = forM_ ["cardano-sl-tools", "cardano-sl-auxx"] $ \t -> do
  p <- single $ inproc "nix-build" [tt (dir </> "default.nix"), "-A", t] empty
  let bin = fromText (format l p) </> "bin"
  prependPath bin

prependPath :: MonadIO io => FilePath -> io ()
prependPath p = need "PATH" >>= \case
  Just path -> export "PATH" (tt p <> ":" <> path)
  Nothing -> pure ()
