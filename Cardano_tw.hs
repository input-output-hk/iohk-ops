#!/usr/bin/env nix-shell
#! nix-shell -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle ])'

{-# LANGUAGE OverloadedStrings #-}

import Turtle
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
    -- TODO: invoke nixops with passed parameters


-- Custom build with https://github.com/NixOS/nixops/pull/550
nixops = "~/nixops/result/bin/nixops "
args = " -d cardano2" <> nixpath
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
  -- build
  --checkstatus
  --deploy
  --shells ("./result/bin/cardano-tx-generator -d 120 -t 50 -k 6000 -i 0 --explicit-initial") empty
  shells (sshForEach "systemctl stop timewarp") empty
  shells (sshForEach "rm -f /home/timewarp/node.log") empty
  nodes <- getNodes
  sh $ using $ parallel nodes scpToAllNodes
  shells (sshForEach "systemctl start timewarp") empty
  threadDelay (150*1000000)
  sh $ using $ parallel nodes scpFromAllNodes
  

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

scpToAllNodes :: Text -> IO ()
scpToAllNodes node = do
  (exitcode, output) <- shellStrict (scpToNode node "static/receiver-logging.yaml" "/home/timewarp/receiver-logging.yaml") empty
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> echo $ "Scp to " <> node <> " failed with " <> (T.pack $ show code)
  (exitcode, output) <- shellStrict (scpToNode node "static/sender-logging.yaml" "/home/timewarp/sender-logging.yaml") empty
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> echo $ "Scp to " <> node <> " failed with " <> (T.pack $ show code)
  (exitcode, output) <- shellStrict (scpToNode node "/static/peers_tw.txt" "/home/timewarp/peers.txt") empty
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> echo $ "Scp to " <> node <> " failed with " <> (T.pack $ show code)

scpToNode :: Text -> Text -> Text -> Text
scpToNode node from to = nixops <> "scp" <> args <> "--to " <> node <> " " <> from <> " " <> to

scpFromAllNodes :: Text -> IO ()
scpFromAllNodes node = do
  let workDir = "experiment"
  -- TODO: mkdir `date`
  shell ("mkdir -p " <> workDir) empty
  (exitcode, output) <- shellStrict (scpPerNode node "/home/timewarp/node.log" (workDir <> "/" <> node <> ".log")) empty
  -- case exitcode of
  --   ExitSuccess -> return ()
  --   ExitFailure code -> echo $ "Scp from " <> node <> " failed with " <> (T.pack $ show code)
  -- (exitcode, output) <- shellStrict (scpPerNode node "/var/lib/cardano-node/jsonLog.json" (workDir <> "/jsonLog-" <> node <> ".json")) empty
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> echo $ "Scp from " <> node <> " failed with " <> (T.pack $ show code)

sshForEach cmd = nixops <> "ssh-for-each" <> args <> " -- " <> cmd

scpPerNode :: Text -> Text -> Text -> Text
scpPerNode node from to = nixops <> "scp" <> args <> "--from " <> node <> " " <> from <> " " <> to

getNodes = do
  (exitcode, nodes) <- shellStrict (nixops <> "info --no-eval" <> args <> " | grep ' ec2 ' | awk '{ print $2 }'") empty
  return $ fmap T.pack $ words $ T.unpack nodes
