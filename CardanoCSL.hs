#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])'

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import CardanoLib
import Control.Monad (forM_, void)
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Conc (threadDelay)

{-

TODO:
 
- turn on the disable flag for asserts

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
  | PrintDate
  deriving (Show)

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
  <|> subcommand "date" "Print date/time" (pure PrintDate)


args = " -d cardano" <> nixpath
nixpath = " -I ~/ "
deployment = " deployments/cardano.nix "

main :: IO ()
main = do
  command <- options "Helper CLI around NixOps to run experiments" parser
  case command of
    Deploy -> deploy args
    Destroy -> destroy args
    FromScratch -> fromscratch args deployment
    CheckStatus -> checkstatus args
    RunExperiment -> runexperiment
    Build -> build
    DumpLogs -> getNodes args >>= void . dumpLogs
    Start -> getNodes args >>= startCardanoNodes
    Stop -> getNodes args >>= stopCardanoNodes
    PrintDate -> getNodes args >>= printDate
    -- TODO: invoke nixops with passed parameters


build :: IO ()
build = do
  echo "Building derivation..."
  shells ("nix-build default.nix" <> nixpath) empty

runexperiment :: IO ()
runexperiment = do
  nodes <- getNodes args
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
  shells ("rm -f timestampsTxSender.json txgen.log txgen.json smart-gen-verifications.csv smart-gen-tps.csv") empty
  -- shells ("./result/bin/cardano-tx-generator -d 240 -t 65 -k 600000 -i 0 --peer 52.59.93.58:3000/MHdtsP-oPf7UWly7QuXnLK5RDB8=") empty
  shells("./result/bin/cardano-smart-generator --json-log=txgen.json -i 0 -d 1000 -t 2000 -t 3000 -t 4000 -P 4 --init-money 60000000 --flat-distr '(40,60000000)' --peer 52.59.93.58:3000/MHdtsP-oPf7UWly7QuXnLK5RDB8= --log-config static/txgen-logging.yaml") empty
  echo "Delaying... (150s)"
  threadDelay (150*1000000)
  echo "Retreive logs..."
  dt <- dumpLogs nodes
  shells ("mv timestampsTxSender.json txgen.log txgen.json smart-gen-verifications.csv smart-gen-tps.csv experiments/" <> dt) empty
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

printDate nodes = do
    sh . using $ parallel nodes (\n -> ssh' (\s -> echo $ "Date on " <> n <> ": " <> s) "date" n)

stopCardanoNodes = sh . using . flip parallel (ssh cmd)
  where
    cmd = "systemctl stop cardano-node"
startCardanoNodes nodes = do
    sh . using $ parallel nodes (ssh $ "'" <> rmCmd <> ";" <> startCmd <> "'")
    --sh . using $ parallel nodes (ssh startCmd)
  where
    rmCmd = foldl (\s (f, _) -> s <> " " <> f) "rm -f" logs
    startCmd = "systemctl start cardano-node"

ssh = ssh' $ const $ return ()

ssh' f cmd' node = do
    (exitcode, output) <- shellStrict cmd empty
    f output
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure code -> echo $ "Ssh cmd '" <> cmd <> "' to " <> node <> " failed with " <> (T.pack $ show code)
  where
    cmd = nixops <> "ssh " <> args <> node <> " -- " <> cmd'

scpPerNode :: Text -> Text -> Text -> Text
scpPerNode node from to = nixops <> "scp" <> args <> "--from " <> node <> " " <> from <> " " <> to
