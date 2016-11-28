#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle cassava vector safe aeson ])'

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Turtle
import CardanoLib
import Control.Monad (forM_, void)
import Data.Monoid ((<>))
import Data.Aeson
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text as T
import GHC.Conc (threadDelay)
import GHC.Generics

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
  -- TODO: use info to avoid shell call
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
  result <- (fmap . fmap) (getNodePublicIP "node0") $ info args
  case result of
    Left err -> echo $ T.pack err
    Right ma ->
      case ma of
        Nothing -> echo "No node0 found"
        Just node0ip -> do
          config <- getConfig
          case config of
            Left err -> echo $ T.pack err
            Right c ->
              let
                tmc = T.pack $ show (totalMoneyAmount c)
                bot = (if bitcoinOverFlat c then "bitcoin" else "flat")
                gn = T.pack $ show (genesisN c)
                cp = T.pack $ show (coordinatorPort c)
              in shells ("./result/bin/cardano-smart-generator --json-log=txgen.json -i 0 -d 100 -N 8 -t 500 -S 20 -P 4 --init-money " <> tmc <> " --" <> bot <> "-distr '(" <> gn <> "," <> tmc <> ")' --peer " <> node0ip <> ":" <> cp <> "/" <> coordinatorDhtKey c <> " --log-config static/txgen-logging.yaml") empty
  echo "Delaying... (150s)"
  sleep 150
  echo "Retreive logs..."
  dt <- dumpLogs nodes
  shells ("mv txgen.log txgen.json smart-gen-verifications.csv smart-gen-tps.csv experiments/" <> dt) empty
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
          scpFromNode args node rpath (workDir <> "/" <> fname node)

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

data Config = Config
  { bitcoinOverFlat :: Bool
  , totalMoneyAmount :: Int
  , coordinatorDhtKey :: Text
  , coordinatorPort :: Int
  , genesisN :: Int
  , mpcRelayInteval :: Int
  , networkDiameter :: Int
  , slotDuration :: Int
  } deriving (Generic, Show)

instance FromJSON Config

getConfig :: IO (Either String Config)
getConfig = do
  (exitcode, output) <- shellStrict "nix-instantiate --eval --strict --json config.nix" empty
  return $ eitherDecode (encodeUtf8 $ fromStrict output)
