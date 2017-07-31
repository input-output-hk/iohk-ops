{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

import           Prelude                     hiding (FilePath)
import           Control.Exception                  (throwIO)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.UTF8            as BUTF8
import           Data.ByteString.Lazy.Char8         (ByteString, pack)
import           Data.Char                          (ord)
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
import           Safe                               (headMay)
import qualified System.IO                       as Sys
import           Text.Read                          (readMaybe)


-- * Leaves
import qualified Data.Aeson                      as AE
import           Data.Aeson                         ((.:), (.=))
import           Data.Csv                           (decodeWith, FromRecord(..), FromField(..), HasHeader(..), defaultDecodeOptions, decDelimiter)
import qualified Data.Yaml                       as YAML
import           Data.Yaml                          (FromJSON(..), ToJSON(..))
import           Turtle                      hiding (procs, inproc)
import qualified Turtle                          as Turtle


-- * Local imports
import           Types


-- * Shell tooling
--
parallelIO :: Options -> [IO a] -> IO ()
parallelIO Options{..} =
  if oSerial
  then sequence_
  else sh . parallel

logCmd :: Text -> [Text] -> ContextM IO ()
logCmd  cmd args = liftIO $ do
  printf ("-- "%s%"\n") $ T.intercalate " " $ cmd:args
  Sys.hFlush Sys.stdout

inproc :: Text -> [Text] -> Shell Line -> Shell Line
inproc cmd args input = do
  logCmd cmd args
  liftIO $ Turtle.inproc cmd args input

inprocs :: MonadIO m => Text -> [Text] -> Shell Line -> m Text
inprocs cmd args input = do
  (exitCode, stdout) <- liftIO $ procStrict cmd args input
  unless (exitCode == ExitSuccess) $
    liftIO (throwIO (ProcFailed cmd args exitCode))
  pure stdout

cmd   :: Text -> [Text] -> ContextM IO ()
cmd'  :: Text -> [Text] -> ContextM IO (ExitCode, Text)
incmd :: Text -> [Text] -> ContextM IO Text

cmd   cmd args = do
  Context Options{..} _ _ <- ask
  when oVerbose $ logCmd cmd args
  Turtle.procs      cmd args empty
cmd'  cmd args = do
  when oVerbose $ logCmd cmd args
  Turtle.procStrict cmd args empty
incmd cmd args = do
  when oVerbose $ logCmd cmd args
  inprocs cmd args empty

nixops  :: NixopsCmd -> [Text] -> ContextM IO ()
nixops' :: NixopsCmd -> [Text] -> ContextM IO (ExitCode, Text)

nixops  (NixopsCmd com) args = do
  Context{..} <- ask
  liftIO $ cmd  ctxOptions "nixops" (com : nixopsCmdOptions ctxOptions ctxConfig <> args)
nixops' (NixopsCmd com) args = do
  Context{..} <- ask
  liftIO $ cmd' ctxOptions "nixops" (com : nixopsCmdOptions ctxOptions ctxConfig <> args)


-- * Deployment lifecycle
--
exists :: ContextM IO Bool
exists = do
  (code, _) <- nixops' "info" []
  pure $ code == ExitSuccess

create :: ContextM IO ()
create = do
  Context _ NixopsConfig{..} _ <- ask
  clusterExists <- exists
  when clusterExists $
    die $ format ("Cluster already exists?: '"%s%"'") cName
  printf ("Creating cluster "%s%"\n") cName
  export "NIX_PATH_LOCKED" "1"
  export "NIX_PATH" (nixpkgsCommitPath cNixpkgsCommit)
  nixops "create" $ deploymentFiles cEnvironment cTarget cElements

modify :: ContextM IO ()
modify = do
  Context _ NixopsConfig{..} _ <- ask
  printf ("Syncing Nix->state for cluster "%s%"\n") cName
  nixops "modify" $ deploymentFiles cEnvironment cTarget cElements

deploy :: Bool -> Bool -> ContextM IO ()
deploy evonly buonly = do
  Context _ NixopsConfig{..} _ <- ask
  when (elem Nodes cElements) $ do
     keyExists <- testfile "keys/key1.sk"
     unless keyExists $
       die "Deploying nodes, but 'keys/key1.sk' is absent."

  printf ("Deploying cluster "%s%"\n") cName
  export "NIX_PATH_LOCKED" "1"
  export "NIX_PATH" (nixpkgsCommitPath cNixpkgsCommit)
  when (not evonly) $ do
    when (elem Nodes cElements) $ do
      liftIO $ export "GC_INITIAL_HEAP_SIZE" (showT $ 8 * 1024*1024*1024) -- for 100 nodes it eats 12GB of ram *and* needs a bigger heap
    export "SMART_GEN_IP"     =<< incmd o "curl" ["--silent", fromURL awsPublicIPURL]
    when (elem Explorer cElements) $ do
      cmd "scripts/generate-explorer-frontend.sh" []

  nixops o c "set-args" $ concat $ uncurry nixArgCmdline <$> Map.toList cDeplArgs

  nixops o c "modify" $ deploymentFiles cEnvironment cTarget cElements

  nixops o c "deploy" $
    [ "--max-concurrent-copy", "50", "-j", "4" ]
    ++ [ "--evaluate-only" | evonly ]
    ++ [ "--build-only"    | buonly ]
  echo "Done."

destroy :: Options -> NixopsConfig -> IO ()
destroy o c@NixopsConfig{..} = do
  printf ("Destroying cluster "%s%"\n") cName
  nixops (o { oConfirm = True }) c "destroy" []
  echo "Done."

delete :: Options -> NixopsConfig -> IO ()
delete o c@NixopsConfig{..} = do
  printf ("Un-defining cluster "%s%"\n") cName
  nixops (o { oConfirm = True }) c "delete" []
  echo "Done."

fromscratch :: Options -> NixopsConfig -> IO ()
fromscratch o c = do
  destroy o c
  delete o c
  create o c
  deploy o c False False


-- * Building
--
generateGenesis :: Options -> NixopsConfig -> IO ()
generateGenesis o c = do
  let cardanoSLDir         = "cardano-sl"
  GitSource{..} <- readSource gitSource CardanoSL
  printf ("Generating genesis using cardano-sl commit "%s%"\n") $ fromCommit gRev
  exists <- testpath cardanoSLDir
  unless exists $
    cmd o "git" ["clone", fromURL $ projectURL CardanoSL, "cardano-sl"]
  cd "cardano-sl"
  cmd o "git" ["fetch"]
  cmd o "git" ["reset", "--hard", fromCommit gRev]
  cd ".."
  export "M" "14"
  cmd o "cardano-sl/scripts/generate/genesis.sh" ["genesis"]
-- M=14 NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/7648f528de9917933bc104359c9a507c6622925c.tar.gz ./util-scripts/generate-genesis.sh
-- cp genesis-qanet-2017-06-13/core/genesis-core.bin core/genesis-core.bin
-- cp genesis-qanet-2017-06-13/godtossing/genesis-godtossing.bin godtossing/genesis-godtossing.bin
-- cp genesis-qanet-2017-06-13/genesis.info .
-- scp genesis-qanet-2017-06-13/nodes/* staging@cardano:~/staging/keys/

deploymentBuildTarget :: Deployment -> NixAttr
deploymentBuildTarget Nodes = "cardano-sl-static"
deploymentBuildTarget x     = error $ "'deploymentBuildTarget' has no idea what to build for " <> show x

build :: Options -> NixopsConfig -> Deployment -> IO ()
build o c d = do
  echo "Building derivation..."
  cmd o "nix-build" ["--max-jobs", "4", "--cores", "2", "-A", fromAttr $ deploymentBuildTarget d]


-- * State management
--
-- Check if nodes are online and reboots them if they timeout
checkstatus :: Options -> NixopsConfig -> IO ()
checkstatus o c = do
  nodes <- getNodeNames o c
  parallelIO o $ fmap (rebootIfDown o c) nodes

rebootIfDown :: Options -> NixopsConfig -> NodeName -> IO ()
rebootIfDown o c (fromNodeName -> node) = do
  (x, _) <- nixops' o c "ssh" (node : ["-o", "ConnectTimeout=5", "echo", "-n"])
  case x of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      TIO.putStrLn $ "Rebooting " <> node
      nixops o c "reboot" ["--include", node]

ssh  :: Options -> NixopsConfig -> [Text] -> NodeName -> IO ()
ssh o c = ssh' o c $ const $ return ()

ssh' :: Options -> NixopsConfig -> (Text -> IO ()) -> [Text] -> NodeName -> IO ()
ssh' o c f cmd (fromNodeName -> node) = do
  let cmd' = node: "--": cmd
  (exitcode, output) <- nixops' o c "ssh" cmd'
  f output
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> TIO.putStrLn $ "ssh cmd '" <> (T.intercalate " " cmd') <> "' to '" <> node <> "' failed with " <> showT code

scpFromNode :: Options -> NixopsConfig -> NodeName -> Text -> Text -> IO ()
scpFromNode o c (fromNodeName -> node) from to = do
  (exitcode, output) <- nixops' o c "scp" ["--from", node, from, to]
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> TIO.putStrLn $ "scp from " <> node <> " failed with " <> showT code

sshForEach :: Options -> NixopsConfig -> [Text] -> IO ()
sshForEach o c cmd =
  nixops o c "ssh-for-each" ("--": cmd)


-- * Functions for extracting information out of nixops info command
--
-- | Get all nodes in EC2 cluster
getNodes :: Options -> NixopsConfig -> IO [DeploymentInfo]
getNodes o c = do
  result <- (fmap . fmap) toNodesInfo $ info o c
  case result of
    Left s -> do
        TIO.putStrLn $ T.pack s
        return []
    Right vector -> return vector

getNodeNames :: Options -> NixopsConfig -> IO [NodeName]
getNodeNames o c = do
  nodes <- getNodes o c
  return $ fmap diName nodes

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
