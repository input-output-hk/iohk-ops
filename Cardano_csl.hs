#!/usr/bin/env nix-shell
#! nix-shell -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])'

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Control.Monad (forM_, void)
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Conc (threadDelay)

{-

Notice that most of this could be submitted upstream to nixops:

have a configuration file (NIX_PATH, deployment name, --confirm)
add deploy --from-scratch

TODO:
 
- turn on the disable flag for asserts
- AMI
- Generate S3 backed AMI with 8GB of space

-}

data Command =
    Deploy
  | Destroy
  | FromScratch
  | CheckStatus
  | RunExperiment
  | Build
  | DumpLogs
  | Start
  | Stop
  deriving (Show)

-- TODO https://github.com/Gabriel439/Haskell-Turtle-Library/issues/193
parser :: Parser Command
parser =
      subcommand "deploy" "Deploy the whole cluster" (pure Deploy)
  <|> subcommand "destroy" "Destroy the whole cluster" (pure Destroy)
  <|> subcommand "build" "Build the application" (pure Build)
  <|> subcommand "fromscratch" "Destroy, Delete, Create, Deploy" (pure FromScratch)
  <|> subcommand "checkstatus" "Check if nodes are accessible via ssh and reboot if they timeout" (pure CheckStatus)
  <|> subcommand "runexperiment" "Deploy cluster and perform measurements" (pure RunExperiment)
  <|> subcommand "dumplogs" "Dump logs" (pure DumpLogs)
  <|> subcommand "stop" "Stop cardano-node service" (pure Stop)
  <|> subcommand "start" "Start cardano-node service" (pure Start)


main :: IO ()
main = do
  command <- options "Helper CLI around NixOps to run experiments" parser
  case command of
    Deploy -> deploy
    Destroy -> destroy
    FromScratch -> fromscratch
    CheckStatus -> checkstatus
    RunExperiment -> runexperiment
    Build -> build
    DumpLogs -> getNodes >>= void . dumpLogs
    Start -> getNodes >>= startCardanoNodes
    Stop -> getNodes >>= stopCardanoNodes
    -- TODO: invoke nixops with passed parameters


-- Custom build with https://github.com/NixOS/nixops/pull/550
nixops = "~/nixops/result/bin/nixops "
args = " -d cardano" <> nixpath
nixpath = " -I ~/ "

deploy :: IO ()
deploy = do
  echo "Deploying cluster..."
  -- for 100 nodes it eats 12GB of ram *and* needs a bigger heap
  shells ("GC_INITIAL_HEAP_SIZE=$((8*1024*1024*1024)) " <> nixops <> "deploy" <> args <> "--max-concurrent-copy 20") empty
  echo "Done."

destroy :: IO ()
destroy = do
  echo "Destroying cluster..."
  shells (nixops <> "destroy" <> args <> " --confirm") empty
  echo "Done."

build :: IO ()
build = do
  shells ("nix-build default.nix" <> nixpath) empty

fromscratch :: IO ()
fromscratch = do
  destroy
  shells (nixops <> "delete" <> args <> " --confirm") empty
  shells (nixops <> "create nixops.nix" <> args) empty
  deploy


-- Check if nodes are online and reboots them if they timeout
checkstatus :: IO ()
checkstatus = do
  nodes <- getNodes
  sh $ using $ parallel nodes rebootIfDown

runexperiment :: IO ()
runexperiment = do
  nodes <- getNodes
  -- build
  --checkstatus
  --deploy
  echo "Stopping nodes..."
  stopCardanoNodes nodes
  echo "Starting nodes..."
  startCardanoNodes nodes
  echo "Delaying... (30s)"
  threadDelay (30*1000000)
  echo "Launching txgen"
  -- shells ("./result/bin/cardano-tx-generator -d 240 -t 65 -k 600000 -i 0 --peer 52.59.93.58:3000/MHdtsP-oPf7UWly7QuXnLK5RDB8=") empty
  shells("./result/bin/cardano-smart-generator --json-log=txgen.json -i 0 -d 240 -t 65 -P 4 --init-money 600000  --peer 52.59.93.58:3000/MHdtsP-oPf7UWly7QuXnLK5RDB8= --log-config static/txgen-logging.yaml") empty
  echo "Delaying... (150s)"
  threadDelay (150*1000000)
  echo "Retreive logs..."
  dt <- dumpLogs nodes
  shells ("mv timestampsTxSender.json txgen.log txgen.json experiments/" <> dt) empty
  --shells (foldl (\s n -> s <> " --file " <> n <> ".json") ("sh -c 'cd " <> workDir <> "; ./result/bin/cardano-analyzer --tx-file timestampsTxSender.json") nodes <> "'") empty
  shells ("tar -czf experiments/" <> dt <> ".tgz experiments/" <> dt) empty
  
logs =
    [ ("/var/lib/cardano-node/node.log", (<> ".log"))
    , ("/var/lib/cardano-node/jsonLog.json", (<> ".json"))
    , ("/var/lib/cardano-node/time-slave.log", (<> "-ts.log"))
    , ("/var/log/saALL", (<> ".sar"))
    ]

dumpLogs nodes = do
    (_, dt) <- fmap T.strip <$> shellStrict "date +%F_%H%M%S" empty
    let workDir = "experiments/" <> dt
    echo workDir
    shell ("mkdir -p " <> workDir) empty
    sh . using $ parallel nodes (dump workDir)
    return dt
  where
    dump workDir node = do
        forM_ logs $ \(rpath, fname) -> do
            (exitcode, _) <- shellStrict (scpPerNode node rpath (workDir <> "/" <> fname node)) empty
            case exitcode of
                ExitSuccess -> return ()
                ExitFailure code -> echo $ "Scp from " <> node <> " failed with " <> (T.pack $ show code)


stopCardanoNodes = sh . using . flip parallel (ssh cmd)
  where
    cmd = "systemctl stop cardano-node"
startCardanoNodes nodes = do
    sh . using $ parallel nodes scpToAllNodes
    sh . using $ parallel nodes (ssh $ "'" <> rmCmd <> ";" <> startCmd <> "'")
    --sh . using $ parallel nodes (ssh startCmd)
  where
    rmCmd = foldl (\s (f, _) -> s <> " " <> f) "rm -f" logs
    startCmd = "systemctl start cardano-node"

scpToAllNodes :: Text -> IO ()
scpToAllNodes node = do
  (exitcode, output) <- shellStrict (scpToNode node "static/csl-logging.yaml" "/var/lib/cardano-node/logging.yaml") empty
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> echo $ "Scp to " <> node <> " failed with " <> (T.pack $ show code)

-- Helpers

-- Tricky helper to run commands in parallel and wait for all results
parallel :: [a] -> (a -> IO ()) -> Managed ()
parallel (first:rest) action = do
  async <- using $ fork $ action first
  parallel rest action
  liftIO (wait async)
parallel [] _ = return ()

rebootIfDown :: Text -> IO ()
rebootIfDown node = do
  x <- shell (nixops <> "ssh " <> args <> node <> " -o ConnectTimeout=5 echo -n") empty
  case x of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      echo $ "Rebooting " <> node
      shells (nixops <> "reboot " <> args <> "--include " <> node) empty

scpToNode :: Text -> Text -> Text -> Text
scpToNode node from to = nixops <> "scp" <> args <> "--to " <> node <> " " <> from <> " " <> to

ssh cmd' node = do
    (exitcode, _) <- shellStrict cmd empty
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure code -> echo $ "Ssh cmd '" <> cmd <> "' to " <> node <> " failed with " <> (T.pack $ show code)
  where
    cmd = nixops <> "ssh " <> args <> node <> " -- " <> cmd'

scpPerNode :: Text -> Text -> Text -> Text
scpPerNode node from to = nixops <> "scp" <> args <> "--from " <> node <> " " <> from <> " " <> to

getNodes = do
  (exitcode, nodes) <- shellStrict (nixops <> "info --no-eval --plain " <> args <> " | grep -vE 'keypair|obsolete' | awk '{ print $1 }'") empty
  return $ fmap T.pack $ words $ T.unpack nodes
