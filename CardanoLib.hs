{-# LANGUAGE OverloadedStrings #-}
module CardanoLib where

import Turtle
import Data.Monoid ((<>))
import qualified Data.Text as T

-- Custom build with https://github.com/NixOS/nixops/pull/550
nixops = "~/nixops/result/bin/nixops "

type NixOpsArgs = Text
type DeploymentName = Text
type NodeIP = Text

deploy :: NixOpsArgs -> IO ()
deploy args = do
  echo "Deploying cluster..."
  -- for 100 nodes it eats 12GB of ram *and* needs a bigger heap
  shells ("GC_INITIAL_HEAP_SIZE=$((8*1024*1024*1024)) " <> nixops <> "deploy" <> args <> "--max-concurrent-copy 20") empty
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

-- | Get all node IPs in EC2 cluster
getNodes args = do
  (exitcode, nodes) <- shellStrict (nixops <> "info --no-eval" <> args <> " | grep ' ec2 ' | awk '{ print $2 }'") empty
  return $ fmap T.pack $ words $ T.unpack nodes
