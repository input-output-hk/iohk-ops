#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle cassava vector safe aeson yaml ])'

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns   #-}

import Turtle
import Prelude hiding (FilePath)
import CardanoLib
import Control.Monad (forM_, void, when)
import Data.Monoid ((<>))
import Filesystem.Path.CurrentOS (encodeString)
import Data.Aeson
import Data.Yaml (decodeFile)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text as T
import GHC.Generics
import Data.Maybe (catMaybes)
import qualified Data.Map as M


data Command =
    Deploy
  | Create
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

subparser :: Parser Command
subparser =
      subcommand "deploy" "Deploy the whole cluster" (pure Deploy)
  <|> subcommand "create" "Create the whole cluster" (pure Create)
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

parser :: Parser (FilePath, Command)
parser = (,) <$> optPath "config" 'c' "Configuration file"
             <*> subparser

main :: IO ()
main = do
  (configFile, command) <- options "Helper CLI around NixOps to run experiments" parser
  Just c <- decodeFile $ encodeString configFile
  --dat <- fmap toNodesInfo <$> info c
  dat <- getSmartGenCmd c
  echo $ T.pack $ show dat
  case command of
    Deploy -> deploy c
    Create -> create c
    Destroy -> destroy c
    FromScratch -> fromscratch c
    CheckStatus -> checkstatus c
    RunExperiment -> runexperiment c
    PostExperiment -> postexperiment c
    Build -> build c
    DumpLogs {..} -> getNodes c >>= void . dumpLogs c withProf
    Start -> getNodes c >>= startCardanoNodes c
    Stop -> getNodes c >>= stopCardanoNodes c
    AMI -> buildAMI c
    PrintDate -> getNodes c >>= printDate c
    -- TODO: invoke nixops with passed parameters

buildAMI :: NixOpsConfig -> IO ()
buildAMI c = do
  shells ("GENERATING_AMI=1 nix-build jobsets/cardano.nix -A image -o image -I " <> nixPath c) empty
  shells "./scripts/create-amis.sh" empty

build :: NixOpsConfig -> IO ()
build c = do
  echo "Building derivation..."
  shells ("nix-build -j 4 --cores 2 default.nix -I " <> nixPath c) empty

getNodesMap :: NixOpsConfig -> IO (Either String (M.Map Int DeploymentInfo))
getNodesMap c = fmap (toMap . toNodesInfo) <$> info c
  where
    toMap :: [DeploymentInfo] -> M.Map Int DeploymentInfo
    toMap = M.fromList . catMaybes . map (\d -> (,d) <$> extractName (T.unpack . getNodeName $ diName d))
    extractName ('n':'o':'d':'e':rest) = Just $ read rest
    extractName _ = Nothing

getSmartGenCmd :: NixOpsConfig -> IO Text
getSmartGenCmd c = do
  result <- getNodesMap c
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

genDhtKey :: Int -> Text
genDhtKey i = T.pack $ "MHdrsP-oPf7UWl" ++ padding ++ show i ++ "7QuXnLK5RD="
  where
    padding | i < 10    = "00"
            | i < 100   = "0"
            | otherwise = ""

genPeers :: Int -> [(Int, DeploymentInfo)] -> Text
genPeers port = mconcat . map impl
  where
    impl (i, getIP . diPublicIP -> ip) = " --peer " <> ip <> ":" <> port' <> "/" <> genDhtKey i
    port' = T.pack $ show port

runexperiment :: NixOpsConfig -> IO ()
runexperiment c = do
  -- TODO: use info to avoid shell call
  nodes <- getNodes c
  -- build
  echo "Checking nodes' status, rebooting failed"
  checkstatus c
  --deploy
  echo "Stopping nodes..."
  stopCardanoNodes c nodes
  echo "Starting nodes..."
  startCardanoNodes c nodes
  echo "Delaying... (40s)"
  sleep 40
  echo "Launching txgen"
  shells ("rm -f timestampsTxSender.json txgen.log txgen.json smart-gen-verifications*.csv smart-gen-tps.csv") empty
  cliCmd <- getSmartGenCmd c
  shells cliCmd empty 
  echo "Delaying... (150s)"
  sleep 150
  postexperiment c

postexperiment :: NixOpsConfig -> IO ()
postexperiment c = do
  nodes <- getNodes c
  echo "Checking nodes' status, rebooting failed"
  checkstatus c
  echo "Retreive logs..."
  dt <- dumpLogs c True nodes
  cliCmd <- getSmartGenCmd c
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
  
dumpLogs :: NixOpsConfig -> Bool -> [NodeName] -> IO Text
dumpLogs c withProf nodes = do
    echo $ "WithProf: " <> T.pack (show withProf)
    when withProf $ do
        echo "Stopping nodes..."
        stopCardanoNodes c nodes
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
          scpFromNode c node rpath (workDir <> "/" <> fname (getNodeName node))
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
   

printDate c nodes = do
    sh . using $ parallel nodes (\n -> ssh' c (\s -> echo $ "Date on " <> getNodeName n <> ": " <> s) "date" n)

stopCardanoNodes c = sh . using . flip parallel (ssh c cmd)
  where
    cmd = "systemctl stop cardano-node"

startCardanoNodes c nodes = do
    sh . using $ parallel nodes (ssh c $ "'" <> rmCmd <> ";" <> startCmd <> "'")
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
