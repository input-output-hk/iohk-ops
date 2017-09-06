{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns   #-}

module CardanoCSL where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Prelude hiding (FilePath)
import Control.Monad (forM_, when)
import Control.Monad.Trans(lift)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Text.Lazy (fromStrict)
import Data.Text.IO as TIO
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text as T
import GHC.Generics
import Text.Printf
import           Turtle                    hiding (dirname, env, err, fold, inproc, prefix, printf, procs, e, f, o, x)
import qualified Data.Map as M

import           NixOps                           (DeploymentInfo(..)
                                                  ,Options(..), NixopsConfig(..), URL(..)
                                                  ,cmd, cmd', parallelIO
                                                  ,fromNodeName)
import qualified NixOps                        as Ops


buildAMI :: Options -> NixopsConfig -> IO ()
buildAMI o _ = do
  cmd o "nix-build" ["jobsets/cardano.nix", "-A", "cardano-node-image", "-o", "image"]
  cmd o "./scripts/create-amis.sh" []

runexperiment :: Options -> NixopsConfig -> IO ()
runexperiment o c = do
  -- build
  echo "Checking nodes' status, rebooting failed"
  Ops.checkstatus o c
  --deploy
  Ops.stop o c
  echo "Starting nodes..."
  Ops.start o c
  echo "Delaying... (40s)"
  sleep 40
  config <- either error id <$> getConfig
  when (enableDelegation config) $ do
    echo "Launching wallet to send delegation certs"
    cmd o "rm" ["-f", "wallet.log", "wallet.json"]
    cliCmd <- getWalletDelegationCmd o c
    shells (cliCmd <> " | awk '{print}; /Command execution finished/ {exit}'") empty
    echo "Delaying... (40s)"
    sleep 40
  echo "Launching txgen"
  shells "rm -f timestampsTxSender.json txgen.log txgen.json smart-gen-verifications*.csv smart-gen-tps.csv" empty
  cliCmd <- getSmartGenCmd o c
  shells cliCmd empty
  echo "Delaying... (150s)"
  sleep 150
  postexperiment o c

postexperiment :: Options -> NixopsConfig -> IO ()
postexperiment o c = do
  echo "Checking nodes' status, rebooting failed"
  Ops.checkstatus o c
  echo "Retreive logs..."
  dt <- dumpLogs o c True
  cliCmd <- getSmartGenCmd o c
  let dirname = "./experiments/" <> dt
  shells ("echo \"" <> cliCmd <> "\" > " <> dirname <> "/txCommandLine") empty
  shells ("echo -n \"cardano-sl revision: \" > " <> dirname <> "/revisions.info") empty
  shells ("cat pkgs/cardano-sl.nix | grep rev >> " <> dirname <> "/revisions.info") empty
  shells ("echo -n \"time-warp revision: \" >> " <> dirname <> "/revisions.info") empty
  shells ("cat kgs/time-warp.nix | grep rev >> " <> dirname <> "/revisions.info") empty
  shells ("cp config.nix " <> dirname) empty
  shells ("mv txgen* smart* " <> dirname) empty
  --shells (foldl (\s n -> s <> " --file " <> n <> ".json") ("sh -c 'cd " <> workDir <> "; ./result/bin/cardano-analyzer --tx-file timestampsTxSender.json") nodes <> "'") empty
  shells ("tar -czf experiments/" <> dt <> ".tgz experiments/" <> dt) empty

dumpLogs :: Options -> NixopsConfig -> Bool -> IO Text
dumpLogs o c withProf = do
    TIO.putStrLn $ "WithProf: " <> T.pack (show withProf)
    when withProf $ do
        Ops.stop o c
        sleep 2
        echo "Dumping logs..."
    (_, dt) <- fmap T.strip <$> cmd' o "date" ["+%F_%H%M%S"]
    let workDir = "experiments/" <> dt
    TIO.putStrLn workDir
    cmd o "mkdir" ["-p", workDir]
    parallelIO o c $ dump workDir
    return dt
  where
    dump workDir node =
        forM_ logs $ \(rpath, fname) -> do
          Ops.scpFromNode o c node rpath (workDir <> "/" <> fname (fromNodeName node))
    logs = mconcat
             [ if withProf
                  then Ops.profLogs
                  else []
             , Ops.defLogs
             ]

generateIPDHTMappings :: Options -> NixopsConfig -> IO Text
generateIPDHTMappings o c = runError $ do
  nodes <- ExceptT $ getNodesMap o c
  CardanoConfig{..} <- ExceptT $ getConfig
  let peers = genPeers nodePort (M.toList nodes)
  lift $ TIO.putStrLn $ T.unlines peers
  return $ T.unlines peers
-- Rest


getNodesMap :: Options -> NixopsConfig -> IO (Either String (M.Map Int DeploymentInfo))
getNodesMap o c = fmap (toMap . Ops.toNodesInfo) <$> Ops.info o c
  where
    toMap :: [DeploymentInfo] -> M.Map Int DeploymentInfo
    toMap = M.fromList . catMaybes . map (\di -> (,di) <$> extractName (T.unpack . fromNodeName $ diName di))
    extractName ('n':'o':'d':'e':rest) = Just $ read rest
    extractName _ = Nothing

runError :: ExceptT String IO Text -> IO Text
runError action = runExceptT action >>=
  either (\(T.pack -> err) -> TIO.putStrLn err >> return err) pure

show' :: Show a => a -> Text
show' = T.pack . show

getSmartGenCmd :: Options -> NixopsConfig -> IO Text
getSmartGenCmd o c = runError $ do
  nodes <- ExceptT $ getNodesMap o c
  config@CardanoConfig{..} <- ExceptT $ getConfig
  peers <- ExceptT $ return $ getPeers c config nodes
  (_, sgIp) <- fmap T.strip <$> shellStrict ("curl " <> fromURL Ops.awsPublicIPURL) empty

  let bot = if bitcoinOverFlat then "bitcoin" else "flat"
      recipShare = "0.5"
      cliCmd = mconcat [ "./result/bin/cardano-smart-generator"
                       --, " +RTS -N -pa -hc -T -A4G -qg -RTS"
                       , " +RTS -N -A4G -qg -RTS"
                       , (T.pack . mconcat $ map (\i -> " -i " <> show i) txgenAddresses)
                       , if enableP2P
                         then " --explicit-initial --disable-propagation "
                         else ""
                       , peers
                       , " -R ", show' txgenR," -N ", show' txgenN," -p ", show' txgenPause
                       , " --init-money ", show' totalMoneyAmount
                       , " -t ", show' txgenInitTps, " -S ", show' txgenTpsStep, " -P ", show' txgenP
                       , " --recipients-share " <> if enableP2P then recipShare else "1"
                       , " --log-config static/txgen-logging-warnonly.yaml"
                       , " --json-log=txgen.json"
                       , " --listen " <> sgIp <> ":24962"
                       -- , " --dht-key " <> sgDhtKey
                       , " --", bot, "-distr '(", show' genesisN, ",", show' totalMoneyAmount, ")'"
                       ]
  return cliCmd


genPeers :: Int -> [(Int, DeploymentInfo)] -> [Text]
genPeers port = map impl
  where
    impl (i, Ops.getIP . diPublicIP -> ip) = ip <> ":" <> show' port

getPeers :: NixopsConfig -> CardanoConfig -> M.Map Int DeploymentInfo -> Either String Text
getPeers _ config nodes = do
  case M.lookup 0 nodes of
    Nothing -> Left "Node0 retrieval failed"
    Just node0 -> let port = nodePort config
                      infos = if enableP2P config then [(0, node0)] else M.toList nodes
                  in Right $ mconcat $ map (\p -> " --kademlia-peer " <> p) $ genPeers port infos

getWalletDelegationCmd :: Options -> NixopsConfig -> IO Text
getWalletDelegationCmd o c = runError $ do
  nodes <- ExceptT $ getNodesMap o c
  config@CardanoConfig{..} <- ExceptT $ getConfig
  peers <- ExceptT $ return $ getPeers c config nodes

  let mkCmd i = "delegate-light " <> show' i <> " " <> show' delegationNode
      cmds = T.intercalate "," $ map mkCmd $ filter (/= delegationNode) $ M.keys nodes
      bot = if bitcoinOverFlat then "bitcoin" else "flat"
      cliCmd = mconcat [ "./result/bin/cardano-wallet"
                       --, " +RTS -N -pa -hc -T -A4G -qg -RTS"
                       , " +RTS -N -A4G -qg -RTS"
                       , if enableP2P
                         then " --explicit-initial --disable-propagation "
                         else ""
                       , peers
                       , " --log-config static/wallet-logging.yaml"
                       , " --json-log=wallet.json"
                       , " --", bot, "-distr '(", show' genesisN, ",", show' totalMoneyAmount, ")'"
                       , " cmd --commands \"", cmds, "\""
                       ]

  return cliCmd


-- * Cardano Config
--
data CardanoConfig = CardanoConfig
  { bitcoinOverFlat :: Bool
  , totalMoneyAmount :: Int
  , nodePort :: Int
  , genesisN :: Int
  , mpcRelayInterval :: Int
  , networkDiameter :: Int
  , slotDuration :: Int
  , enableP2P :: Bool
  , txgenR :: Int
  , txgenN :: Int
  , txgenPause :: Int
  , txgenP :: Int
  , txgenInitTps :: Int
  , txgenTpsStep :: Int
  , txgenAddresses :: [Int]
  , enableDelegation :: Bool
  , delegationNode :: Int
  } deriving (Generic, Show)

instance FromJSON CardanoConfig

getConfig :: IO (Either String CardanoConfig)
getConfig = do
  (_exitcode, output') <- shellStrict "nix-instantiate --eval --strict --json config.nix" empty
  return $ eitherDecode (encodeUtf8 $ fromStrict output')
