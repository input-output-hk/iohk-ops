#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle cassava vector safe aeson ])'

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections   #-}
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
import Data.Maybe (catMaybes)
import qualified Data.Map as M


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
  | AMI
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
  <|> subcommand "ami" "Build ami" (pure AMI)


args = " -d cardano" <> nixpath
nixpath = " -I ~/ "
deployment = " deployments/cardano.nix "

main :: IO ()
main = do
  --dat <- fmap toNodesInfo <$> info args
  dat <- getSmartGenCmd
  echo $ T.pack $ show dat
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
    AMI -> buildAMI
    PrintDate -> getNodes args >>= printDate
    -- TODO: invoke nixops with passed parameters

buildAMI :: IO ()
buildAMI = do
  shells ("GENERATING_AMI=1 nix-build jobsets/cardano.nix -A image -o image " <> nixpath) empty
  shells "./scripts/create-amis.sh" empty

build :: IO ()
build = do
  echo "Building derivation..."
  shells ("nix-build -j 4 --cores 2 default.nix" <> nixpath) empty

getNodesMap :: IO (Either String (M.Map Int DeploymentInfo))
getNodesMap = fmap (toMap . toNodesInfo) <$> info args
  where
    toMap :: [DeploymentInfo] -> M.Map Int DeploymentInfo
    toMap = M.fromList . catMaybes . map (\d -> (,d) <$> extractName (T.unpack . getNodeName $ diName d))
    extractName ('n':'o':'d':'e':rest) = Just $ read rest
    extractName _ = Nothing

getSmartGenCmd :: IO Text
getSmartGenCmd = do
  result <- getNodesMap
  case result of
    Left (T.pack -> err) -> echo err >> return err
    Right nodes ->
      case 0 `M.lookup` nodes of
        Nothing -> echo "Node0 retrieval failed" >> return "Node0 retrieval failed"
        Just node0 -> do
          config <- getConfig
          case config of
            Left (T.pack -> err) -> echo err >> return err
            Right c ->
              let
                tmc = T.pack $ show (totalMoneyAmount c)
                bot = (if bitcoinOverFlat c then "bitcoin" else "flat")
                gn = T.pack $ show (genesisN c)
                recipShare = "0.3"
                peers = if enableP2P c
                           then genPeers (nodePort c) [(0, node0)]
                           else genPeers (nodePort c) $ M.toList nodes
                cliCmd = mconcat [ "./result/bin/cardano-smart-generator"
                                 , " +RTS -N -pa -hc -T -A4G -qg -RTS"
                                 --, " +RTS -N -A4G -qg -RTS"
                                 , " -i 0"
                                 , if enableP2P c
                                      then " --explicit-initial --disable-propagation "
                                      else ""
                                 , peers
                                 , " -R 5 -N 3 -p 0"
                                 , " --init-money ", tmc
                                 , " -t 10 -S 5 -P 2"
                                 , " --recipients-share " <> (if enableP2P c then "0.3" else "1")
                                 , " --log-config static/txgen-logging.yaml"
                                 , " --json-log=txgen.json"
                                 , " --", bot, "-distr '(", gn, ",", tmc, ")'"
                                 ]
              in pure cliCmd

dhtKeyPrefix :: String
dhtKeyPrefix = "MHdrsP-oPf7UWl"

dhtKeySuffix :: String
dhtKeySuffix = "7QuXnLK5RD="

genDhtKey :: Int -> Text
genDhtKey i = T.pack $ dhtKeyPrefix ++ padding ++ show i ++ dhtKeySuffix
  where
    padding | i < 10    = "00"
            | i < 100   = "0"
            | otherwise = ""

genPeers :: Int -> [(Int, DeploymentInfo)] -> Text
genPeers port = mconcat . map impl
  where
    impl (i, getIP . diPublicIP -> ip) = " --peer " <> ip <> ":" <> port' <> "/" <> genDhtKey i
    port' = T.pack $ show port

runexperiment :: IO ()
runexperiment = do
  -- TODO: use info to avoid shell call
  nodes <- getNodes args
  -- build
  echo "Checking nodes' status, rebooting failed"
  checkstatus args
  --deploy
  echo "Stopping nodes..."
  stopCardanoNodes nodes
  echo "Starting nodes..."
  startCardanoNodes nodes
  echo "Delaying... (40s)"
  sleep 40
  echo "Launching txgen"
  shells ("rm -f timestampsTxSender.json txgen.log txgen.json smart-gen-verifications*.csv smart-gen-tps.csv") empty
  cliCmd <- getSmartGenCmd
  shells cliCmd empty 
  echo "Delaying... (150s)"
  sleep 150
  postexperiment

postexperiment = do
  nodes <- getNodes args
  echo "Checking nodes' status, rebooting failed"
  checkstatus args
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
    , ("/var/lib/cardano-node/cardano-node.hp", (<> ".hp"))
    -- in fact, if there's a heap profile then there's no eventlog and vice versa
    -- but scp will just say "not found" and it's all good
    , ("/var/lib/cardano-node/cardano-node.eventlog", (<> ".eventlog"))
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
  , nodePort :: Int
  , genesisN :: Int
  , mpcRelayInterval :: Int
  , networkDiameter :: Int
  , slotDuration :: Int
  , enableP2P :: Bool
  } deriving (Generic, Show)

instance FromJSON Config

getConfig :: IO (Either String Config)
getConfig = do
  (exitcode, output) <- shellStrict "nix-instantiate --eval --strict --json config.nix" empty
  return $ eitherDecode (encodeUtf8 $ fromStrict output)
