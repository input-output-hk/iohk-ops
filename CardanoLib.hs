{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CardanoLib where

import Turtle
import Data.Char (ord)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Csv (decodeWith, FromRecord(..), FromField(..), HasHeader(..), defaultDecodeOptions, decDelimiter)
import qualified Data.Vector as V
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import GHC.Generics
import Safe (headMay)

-- Custom build with https://github.com/NixOS/nixops/pull/550
nixops = "~/nixops/result/bin/nixops "

type NixOpsArgs = Text
type DeploymentName = Text
type NodeIP = Text

deploy :: NixOpsArgs -> IO ()
deploy args = do
  echo "Deploying cluster..."
  -- for 100 nodes it eats 12GB of ram *and* needs a bigger heap
  shells ("GC_INITIAL_HEAP_SIZE=$((8*1024*1024*1024)) " <> nixops <> "deploy" <> args <> "--max-concurrent-copy 50") empty
  echo "Done."

destroy :: NixOpsArgs -> IO ()
destroy args = do
  echo "Destroying cluster..."
  shells (nixops <> "destroy" <> args <> " --confirm") empty
  echo "Done."

fromscratch :: NixOpsArgs -> DeploymentName -> IO ()
fromscratch args deployment = do
  destroy args
  shells (nixops <> "delete" <> args <> " --confirm") empty
  shells (nixops <> "create" <> deployment <> args) empty
  deploy args


-- Check if nodes are online and reboots them if they timeout
checkstatus :: NixOpsArgs -> IO ()
checkstatus args = do
  nodes <- getNodes args
  sh . using $ parallel nodes $ rebootIfDown args


-- Helper to run commands in parallel and wait for all results
parallel :: [a] -> (a -> IO ()) -> Managed ()
parallel (first:rest) action = do
  async <- using $ fork $ action first
  parallel rest action
  liftIO (wait async)
parallel [] _ = return ()

rebootIfDown :: NixOpsArgs -> NodeIP -> IO ()
rebootIfDown args node = do
  x <- shell (nixops <> "ssh " <> args <> node <> " -o ConnectTimeout=5 echo -n") empty
  case x of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      echo $ "Rebooting " <> node
      shells (nixops <> "reboot " <> args <> "--include " <> node) empty

scpFromAllNodes :: NixOpsArgs -> Text -> (NodeIP -> Text) -> NodeIP -> IO ()
scpFromAllNodes args from to node = scpFromNode args node from $ to node

scpFromNode :: NixOpsArgs -> NodeIP -> Text -> Text -> IO ()
scpFromNode args node from to = do
  (exitcode, output) <- shellStrict (nixops <> "scp" <> args <> "--from " <> node <> " " <> from <> " " <> to) empty
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> echo $ "Scp from " <> node <> " failed with " <> (T.pack $ show code)

sshForEach args cmd = nixops <> "ssh-for-each" <> args <> " -- " <> cmd

-- Functions for extracting information out of nixops info command

-- | Get all node IPs in EC2 cluster
getNodes :: NixOpsArgs -> IO [Text]
getNodes args = do
  result <- (fmap . fmap) getPublicIPs $ info args
  case result of
    Left s -> do
        echo $ T.pack s
        return []
    Right vector -> return vector

data DeploymentStatus = UpToDate | Obsolete | Outdated
  deriving (Show, Eq)

instance FromField DeploymentStatus where
  parseField "up-to-date" = pure UpToDate
  parseField "obsolete" = pure Obsolete
  parseField "outdated" = pure Outdated
  parseField _ = mzero

data DeploymentInfo = DeploymentInfo
    { diName :: !Text
    , diStatus :: !DeploymentStatus
    , diLocation :: !Text
    , diResourceID :: !Text
    , diPublicIP :: !Text
    , diPrivateIP :: !Text
    } deriving (Show, Generic)

instance FromRecord DeploymentInfo


nixopsDecodeOptions = defaultDecodeOptions {
    decDelimiter = fromIntegral (ord '\t')
  }

info :: NixOpsArgs -> IO (Either String (V.Vector DeploymentInfo))
info args = do
  (exitcode, nodes) <- shellStrict (nixops <> "info --no-eval --plain" <> args) empty
  case exitcode of
    ExitFailure code -> return $ Left ("Parsing info failed with exit code " <> show code)
    ExitSuccess -> return $ decodeWith nixopsDecodeOptions NoHeader (encodeUtf8 $ fromStrict nodes)


getPublicIPs :: V.Vector DeploymentInfo -> [Text]
getPublicIPs vector = 
  V.toList $ fmap diPublicIP $ V.filter filterEC2 vector
    where
      filterEC2 di = T.take 4 (diLocation di) == "ec2 " && diStatus di /= Obsolete


getNodePublicIP :: Text -> V.Vector DeploymentInfo -> Maybe Text
getNodePublicIP name vector =
    headMay $ V.toList $ fmap diPublicIP $ V.filter (\di -> diName di == name) vector


