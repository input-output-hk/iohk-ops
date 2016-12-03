#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle cassava vector safe aeson ])'

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns   #-}

import Turtle
import CardanoLib
import Control.Monad (forM_, void, when)
import Data.Monoid ((<>))
import Data.Aeson
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text as T
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
  | PostExperiment
  | Build
  | DumpLogs { withProf :: Bool }
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
  <|> subcommand "postexperiment" "Post-experiments logs dumping (if failed)" (pure PostExperiment)
  <|> subcommand "dumplogs" "Dump logs" (DumpLogs <$> switch "prof" 'p' "Dump profiling data as well (requires service stop)")
  <|> subcommand "stop" "Stop cardano-node service" (pure Stop)
  <|> subcommand "start" "Start cardano-node service" (pure Start)
  <|> subcommand "date" "Print date/time" (pure PrintDate)


args = " -d cardano" <> nixpath
nixpath = " -I ~/ "
deployment = " deployments/cardano.nix "

main :: IO ()
main = do
  --dat <- fmap toNodesInfo <$> info args
  --echo $ T.pack $ show dat
  command <- options "Helper CLI around NixOps to run experiments" parser
  case command of
    Deploy -> deploy args
    Destroy -> destroy args
    FromScratch -> fromscratch args deployment
    CheckStatus -> checkstatus args
    RunExperiment -> runexperiment
    PostExperiment -> postexperiment
    Build -> build
    DumpLogs {..} -> getNodes args >>= void . dumpLogs withProf
    Start -> getNodes args >>= startCardanoNodes
    Stop -> getNodes args >>= stopCardanoNodes
    PrintDate -> getNodes args >>= printDate
    -- TODO: invoke nixops with passed parameters


build :: IO ()
build = do
  echo "Building derivation..."
  shells ("nix-build -j 4 --cores 2 default.nix" <> nixpath) empty

getSmartGenCmd :: IO Text
getSmartGenCmd = do
  result <- (fmap . fmap) (getNodePublicIP "node0") $ info args
  case result of
    Left (T.pack -> err) -> echo err >> return err
    Right ma ->
      case ma of
        Nothing -> echo "No node0 found" >> return "No node0 found"
        Just node0ip -> do
          config <- getConfig
          case config of
            Left (T.pack -> err) -> echo err >> return err
            Right c ->
              let
                tmc = T.pack $ show (totalMoneyAmount c)
                bot = (if bitcoinOverFlat c then "bitcoin" else "flat")
                gn = T.pack $ show (genesisN c)
                cp = T.pack $ show (coordinatorPort c)
                cliCmd = mconcat [ "./result/bin/cardano-smart-generator"
                                 , " +RTS -N -pa -A4G -qg -RTS"
                                 , " -i 0"
                                 , " --peer ", node0ip, ":", cp, "/", coordinatorDhtKey c
                                 , " -R 1 -N 3 -p 5"
                                 , " --init-money ", tmc
                                 , " -t 20 -S 5 -P 2"
                                 , " --recipients-share 0.3"
                                 , " --log-config static/txgen-logging.yaml"
                                 , " --json-log=txgen.json"
                                 , " --", bot, "-distr '(", gn, ",", tmc, ")'"
                                 ]
              in pure cliCmd

runexperiment :: IO ()
runexperiment = do
  -- TODO: use info to avoid shell call
  nodes <- getNodes args
  -- build
  echo "Checking nodes' status, rebooting failed"
  checkstatus args
  --deploy
  echo "Starting nodes..."
  startCardanoNodes nodes
  echo "Delaying... (40s)"
  sleep 40
  echo "Launching txgen"
  shells ("rm -f timestampsTxSender.json txgen.log txgen.json smart-gen-verifications*.csv smart-gen-tps.csv") empty
  -- shells ("./result/bin/cardano-tx-generator -d 240 -t 65 -k 600000 -i 0 --peer 52.59.93.58:3000/MHdtsP-oPf7UWly7QuXnLK5RDB8=") empty
  cliCmd <- getSmartGenCmd
  shells cliCmd empty 
  echo "Delaying... (150s)"
  sleep 150
  postexperiment

postexperiment = do
  nodes <- getNodes args
  echo "Checking nodes' status, rebooting failed"
  checkstatus args
  echo "Stopping nodes..."
  stopCardanoNodes nodes
  echo "Retreive logs..."
  dt <- dumpLogs True nodes
  cliCmd <- getSmartGenCmd
  let dirname = "./experiments/" <> dt
  shells ("echo \"" <> cliCmd <> "\" > " <> dirname <> "/txCommandLine") empty
  shells ("echo -n \"cardano-sl revision: \" > " <> dirname <> "/revisions.info") empty
  shells ("cat srk-nixpkgs/cardano-sl.nix | grep rev >> " <> dirname <> "/revisions.info") empty
  shells ("echo -n \"time-warp revision: \" >> " <> dirname <> "/revisions.info") empty
  shells ("cat srk-nixpkgs/time-warp.nix | grep rev >> " <> dirname <> "/revisions.info") empty
  shells ("cp config.nix " <> dirname) empty
  shells ("mv txgen* smart* " <> dirname) empty
  --shells (foldl (\s n -> s <> " --file " <> n <> ".json") ("sh -c 'cd " <> workDir <> "; ./result/bin/cardano-analyzer --tx-file timestampsTxSender.json") nodes <> "'") empty
  shells ("tar -czf experiments/" <> dt <> ".tgz experiments/" <> dt) empty
  
dumpLogs withProf nodes = do
    echo $ "WithProf: " <> T.pack (show withProf)
    when withProf $ do
        echo "Stopping nodes..."
        stopCardanoNodes nodes
        sleep 2
        echo "Dumping logs..."
    (_, dt) <- fmap T.strip <$> shellStrict "date +%F_%H%M%S" empty
    let workDir = "experiments/" <> dt
    echo workDir
    shell ("mkdir -p " <> workDir) empty
    sh . using $ parallel nodes (dump workDir)
    return dt
  where
    dump workDir node = do
        forM_ logs $ \(rpath, fname) -> do
          scpFromNode args node rpath (workDir <> "/" <> fname (getNodeName node))
    logs = mconcat
             [ if withProf
                  then profLogs
                  else []
             , defLogs
             ]
defLogs =
    [ ("/var/lib/cardano-node/node.log", (<> ".log"))
    , ("/var/lib/cardano-node/jsonLog.json", (<> ".json"))
    , ("/var/lib/cardano-node/time-slave.log", (<> "-ts.log"))
    , ("/var/log/saALL", (<> ".sar"))
    ]
profLogs =
    [ ("/var/lib/cardano-node/cardano-node.prof", (<> ".prof"))
    ]
   

printDate nodes = do
    sh . using $ parallel nodes (\n -> ssh' args (\s -> echo $ "Date on " <> getNodeName n <> ": " <> s) "date" n)

stopCardanoNodes = sh . using . flip parallel (ssh args cmd)
  where
    cmd = "systemctl stop cardano-node"

startCardanoNodes nodes = do
    sh . using $ parallel nodes (ssh args $ "'" <> rmCmd <> ";" <> startCmd <> "'")
  where
    rmCmd = foldl (\s (f, _) -> s <> " " <> f) "rm -f" logs
    startCmd = "systemctl start cardano-node"
    logs = mconcat [ defLogs, profLogs ]

data Config = Config
  { bitcoinOverFlat :: Bool
  , totalMoneyAmount :: Int
  , coordinatorDhtKey :: Text
  , coordinatorPort :: Int
  , genesisN :: Int
  , mpcRelayInterval :: Int
  , networkDiameter :: Int
  , slotDuration :: Int
  } deriving (Generic, Show)

instance FromJSON Config

getConfig :: IO (Either String Config)
getConfig = do
  (exitcode, output) <- shellStrict "nix-instantiate --eval --strict --json config.nix" empty
  return $ eitherDecode (encodeUtf8 $ fromStrict output)
