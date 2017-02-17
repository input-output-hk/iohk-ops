{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NixOps where

import Data.Char (ord)
import Data.Yaml (FromJSON(..))
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Csv (decodeWith, FromRecord(..), FromField(..), HasHeader(..), defaultDecodeOptions, decDelimiter)
import qualified Data.Vector as V
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import GHC.Generics
import Safe (headMay)
import Turtle hiding (FilePath)


data NixOpsConfig = NixOpsConfig
  { deploymentName :: Text
  , deploymentFiles :: [Text]
  , nixopsExecutable :: Text
  , nixPath :: Text
  } deriving (Generic, Show)

instance FromJSON NixOpsConfig


-- deprecated
getArgs :: NixOpsConfig -> Text
getArgs c =
  " --show-trace -d " <> deploymentName c <> " -I " <> nixPath c <> " "

getArgsList :: NixOpsConfig -> [Text]
getArgsList c =
  ["-d", deploymentName c, "-I", nixPath c]

deploy :: NixOpsConfig -> IO ()
deploy c = do
  echo "Deploying cluster..."
  -- for 100 nodes it eats 12GB of ram *and* needs a bigger heap
  shells ("GC_INITIAL_HEAP_SIZE=$((8*1024*1024*1024)) SMART_GEN_IP=$(curl http://169.254.169.254/latest/meta-data/public-ipv4) " <> nixopsExecutable c <> " deploy" <> getArgs c <> "--max-concurrent-copy 50 -j 4") empty
  echo "Done."

destroy :: NixOpsConfig -> IO ()
destroy c = do
  echo "Destroying cluster..."
  procs (nixopsExecutable c) (["destroy"] ++ getArgsList c ++ ["--confirm"]) empty
  echo "Done."

fromscratch :: NixOpsConfig -> IO ()
fromscratch c = do
  destroy c
  procs (nixopsExecutable c) (["delete"] ++ getArgsList c ++ ["--confirm"]) empty
  create c
  deploy c

create :: NixOpsConfig -> IO ()
create c =
  procs (nixopsExecutable c) (["create"] ++ deploymentFiles c ++ (getArgsList c)) empty

-- Check if nodes are online and reboots them if they timeout
checkstatus :: NixOpsConfig -> IO ()
checkstatus c = do
  nodes <- getNodeNames c
  parallelIO $ fmap (rebootIfDown c) nodes 

parallelIO :: [IO a] -> IO ()
parallelIO = sh . parallel

rebootIfDown :: NixOpsConfig -> NodeName -> IO ()
rebootIfDown c (getNodeName -> node) = do
  x <- shell (nixopsExecutable c <> " ssh " <> getArgs c <> node <> " -o ConnectTimeout=5 echo -n") empty
  case x of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      TIO.putStrLn $ "Rebooting " <> node
      procs (nixopsExecutable c) (["reboot"] ++ getArgsList c ++ ["--include", node]) empty

ssh :: NixOpsConfig -> Text -> NodeName -> IO ()
ssh c = ssh' c $ const $ return ()

ssh' :: NixOpsConfig -> (Text -> IO ()) -> Text -> NodeName -> IO ()
ssh' c f cmd' (getNodeName -> node) = do
    (exitcode, output) <- shellStrict cmd empty
    f output
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure code -> TIO.putStrLn $ "Ssh cmd '" <> cmd <> "' to " <> node <> " failed with " <> (T.pack $ show code)
  where
    cmd = nixopsExecutable c <> " ssh " <> getArgs c <> node <> " -- " <> cmd'

scpFromNode :: NixOpsConfig -> NodeName -> Text -> Text -> IO ()
scpFromNode c (getNodeName -> node) from to = do
  (exitcode, output) <- shellStrict (nixopsExecutable c <> " scp" <> getArgs c <> "--from " <> node <> " " <> from <> " " <> to) empty
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> TIO.putStrLn $ "Scp from " <> node <> " failed with " <> (T.pack $ show code)

sshForEach :: NixOpsConfig -> Text -> IO ()
sshForEach c cmd = 
  shells (nixopsExecutable c <> " ssh-for-each " <> getArgs c <> " -- " <> cmd) empty

-- Functions for extracting information out of nixops info command

newtype IP = IP { getIP :: Text }
   deriving (Show, Generic, FromField)
newtype NodeName = NodeName { getNodeName :: Text }
   deriving (Show, Generic, FromField)

-- | Get all nodes in EC2 cluster
getNodes :: NixOpsConfig -> IO [DeploymentInfo]
getNodes c = do
  result <- (fmap . fmap) toNodesInfo $ info c
  case result of
    Left s -> do
        TIO.putStrLn $ T.pack s
        return []
    Right vector -> return vector

getNodeNames :: NixOpsConfig -> IO [NodeName]
getNodeNames c = do
  nodes <- getNodes c
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

info :: NixOpsConfig -> IO (Either String (V.Vector DeploymentInfo))
info c = do
  (exitcode, nodes) <- shellStrict (nixopsExecutable c <> " info --no-eval --plain" <> getArgs c) empty
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
    headMay $ V.toList $ fmap (getIP . diPublicIP) $ V.filter (\di -> getNodeName (diName di) == name) vector
