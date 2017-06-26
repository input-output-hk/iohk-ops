#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings, RecordWildCards, StandaloneDeriving, TupleSections, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults #-}

import           Control.Monad                    (forM_)
import           Data.Monoid                      ((<>))
import           Data.Maybe
import           Data.Optional (Optional)
import qualified Data.Text                     as T
import qualified Filesystem.Path.CurrentOS     as Path
import           Text.Read                        (readMaybe)
import           Turtle                    hiding (procs, shells)

import           NixOps                           (Branch(..), Commit(..), Environment(..), Deployment(..), Target(..)
                                                  ,Options(..), NixopsCmd(..), Region(..), URL(..)
                                                  ,showT, cmd, incmd)
import qualified NixOps                        as Ops
import qualified CardanoCSL                    as Cardano
import qualified Timewarp                      as Timewarp


-- * Elementary parsers
--
optReadLower :: Read a => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optReadLower = opt (readMaybe . T.unpack . T.toTitle)

parserBranch :: Optional HelpMessage -> Parser Branch
parserBranch desc = Branch <$> argText "branch" desc

parserCommit :: Optional HelpMessage -> Parser Commit
parserCommit desc = Commit <$> argText "commit" desc

parserEnvironment :: Parser Environment
parserEnvironment = fromMaybe Ops.defaultEnvironment <$> optional (optReadLower "environment" 'e' "Environment: Development, Staging or Production;  defaults to Development")

parserTarget      :: Parser Target
parserTarget      = fromMaybe Ops.defaultTarget      <$> optional (optReadLower "target"      't' "Target: AWS, All;  defaults to AWS")

parserDeployment  :: Parser Deployment
parserDeployment  = argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'ReportServer' or 'Timewarp'"
parserDeployments :: Parser [Deployment]
parserDeployments = (\(a, b, c, d) -> concat $ maybeToList <$> [a, b, c, d])
                    <$> ((,,,)
                         <$> (optional parserDeployment) <*> (optional parserDeployment) <*> (optional parserDeployment) <*> (optional parserDeployment))

parserDo :: Parser [Command]
parserDo = (\(a, b, c, d) -> concat $ maybeToList <$> [a, b, c, d])
           <$> ((,,,)
                 <$> (optional centralCommandParser) <*> (optional centralCommandParser) <*> (optional centralCommandParser) <*> (optional centralCommandParser))


-- * Central command
--
data Command where

  -- * setup 
  Template              :: { tNodeLimit   :: Integer
                           , tEnvironment :: Environment
                           , tTarget      :: Target
                           , tBranch      :: Branch
                           , tDeployments :: [Deployment]
                           } -> Command
  SetCardanoRev         :: Commit -> Command
  SetExplorerRev        :: Commit -> Command
  MiniKeys              :: Command

  -- * building
  Genesis               :: Command
  GenerateIPDHTMappings :: Command
  Build                 :: Deployment -> Command
  AMI                   :: Command

  -- * cluster lifecycle
  Nixops                :: NixopsCmd -> [Text] -> Command
  Do                    :: [Command] -> Command
  Create                :: Command
  Modify                :: Command
  Deploy                :: Command
  Destroy               :: Command
  Delete                :: Command
  FromScratch           :: Command
  Info                  :: Command

  -- * live cluster ops
  CheckStatus           :: Command
  Start                 :: Command
  Stop                  :: Command
  FirewallBlock         :: { from :: Region, to :: Region } -> Command
  FirewallClear         :: Command
  RunExperiment         :: Deployment -> Command
  PostExperiment        :: Command
  DumpLogs              :: { depl :: Deployment, withProf :: Bool } -> Command
  PrintDate             :: Command
deriving instance Show Command

centralCommandParser :: Parser Command
centralCommandParser =
  (    subcommandGroup "General:"
    [ ("template",              "Clone iohk-nixops from git BRANCH, for a specified set of deployments",
                                Template
                                <$> (fromMaybe Ops.defaultNodeLimit
                                     <$> optional (optInteger "node-limit" 'l' "Limit cardano-node count to N"))
                                <*> parserEnvironment
                                <*> parserTarget
                                <*> parserBranch "iohk-nixops branch to check out"
                                <*> parserDeployments)
    , ("set-cardano-rev",       "Set cardano-sl commit to COMMIT",
                                SetCardanoRev  <$> parserCommit "Commit to set 'cardano-sl' version to")
    , ("set-explorer-rev",      "Set cardano-sl-explorer commit to COMMIT",
                                SetExplorerRev <$> parserCommit "Commit to set 'cardano-sl-explorer' version to")
    , ("mini-keys",             "Fake/enter minimum set of keys necessary for a basic deployment",  pure MiniKeys)
    , ("do",                    "Chain commands",                                                   Do <$> parserDo) ]

   <|> subcommandGroup "Build-related:"
    [ ("genesis",               "Generate genesis",                                                 pure Genesis)
    , ("generate-ipdht",        "Generate IP/DHT mappings for wallet use",                          pure GenerateIPDHTMappings)
    , ("build",                 "Build the application specified by DEPLOYMENT",                    Build <$> parserDeployment)
    , ("ami",                   "Build ami",                                                        pure AMI) ]
  
   -- * cluster lifecycle

   <|> subcommandGroup "Cluster lifecycle:"
   [
     -- ("nixops",                "Call 'nixops' with current configuration",
     --                           (Nixops
     --                            <$> (NixopsCmd <$> argText "CMD" "Nixops command to invoke")
     --                            <*> ???)) -- should we switch to optparse-applicative?
     ("create",                 "Create the whole cluster",                                         pure Create)
   , ("modify",                 "Update cluster state with the nix expression changes",             pure Modify)
   , ("deploy",                 "Deploy the whole cluster",                                         pure Deploy)
   , ("destroy",                "Destroy the whole cluster",                                        pure Destroy)
   , ("delete",                 "Unregistr the cluster from NixOps",                                pure Delete)
   , ("fromscratch",            "Destroy, Delete, Create, Deploy",                                  pure FromScratch)
   , ("info",                   "Invoke 'nixops info'",                                             pure Info)]

   <|> subcommandGroup "Live cluster ops:"
   [ ("checkstatus",            "Check if nodes are accessible via ssh and reboot if they timeout", pure CheckStatus)
   , ("start",                  "Start cardano-node service",                                       pure Start)
   , ("stop",                   "Stop cardano-node service",                                        pure Stop)
   , ("firewall-block-region",  "Block whole region in firewall",
                                FirewallBlock
                                <$> (Region <$> optText "from-region" 'f' "AWS Region that won't reach --to")
                                <*> (Region <$> optText "to-region"   't' "AWS Region that all nodes will be blocked"))
   , ("firewall-clear",         "Clear firewall",                                                   pure FirewallClear)
   , ("runexperiment",          "Deploy cluster and perform measurements",                          RunExperiment <$> parserDeployment)
   , ("postexperiment",         "Post-experiments logs dumping (if failed)",                        pure PostExperiment)
   , ("dumplogs",               "Dump logs",
                                DumpLogs
                                <$> parserDeployment
                                <*> switch "prof"         'p' "Dump profiling data as well (requires service stop)")
   , ("date",                   "Print date/time",                                                  pure PrintDate)]

   <|> subcommandGroup "Other:"
    [ ])
      

main :: IO ()
main = do
  (o@Options{..}, topcmd) <- options "Helper CLI around IOHK NixOps" $
                             (,) <$> Ops.parserOptions <*> centralCommandParser

  case topcmd of
    Template{..}             -> runTemplate    o topcmd

    _ -> do
      -- XXX: Config filename depends on environment, which defaults to 'Development'
      let cf = flip fromMaybe oConfigFile $
               error $ "Sub-command " <> show topcmd <> " requires -c <config-file> to be specified."
      c <- Ops.readConfig cf
      
      when oVerbose $
        printf ("-- config '"%fp%"'\n"%w%"\n") cf c

      -- * CardanoCSL
      -- dat <- getSmartGenCmd c
      -- TIO.putStrLn $ T.pack $ show dat

      doCommand o c topcmd
    where
        doCommand :: Options -> Ops.NixopsConfig -> Command -> IO ()
        doCommand o c cmd = do
          let isNode (T.unpack . Ops.fromNodeName -> ('n':'o':'d':'e':_)) = True
              isNode _ = False
              getNodeNames' = filter isNode <$> Ops.getNodeNames o c
          case cmd of
            -- * setup
            SetCardanoRev  commit    -> runSetCardanoRev  o commit
            SetExplorerRev commit    -> runSetExplorerRev o commit
            MiniKeys                 -> runMiniKeys
            -- * building
            Genesis                  -> Ops.generateGenesis           o c
            GenerateIPDHTMappings    -> void $
                                        Cardano.generateIPDHTMappings o c
            Build depl               -> Ops.build                     o c depl
            AMI                      -> Cardano.buildAMI              o c
            -- * deployment lifecycle
            Nixops cmd args          -> Ops.nixops                    o c cmd args
            Do cmds                  -> sequence_ $ doCommand o c <$> cmds
            Create                   -> Ops.create                    o c
            Modify                   -> Ops.modify                    o c
            Deploy                   -> Ops.deploy                    o c
            Destroy                  -> Ops.destroy                   o c
            Delete                   -> Ops.delete                    o c
            FromScratch              -> Ops.fromscratch               o c
            Info                     -> Ops.nixops                    o c "info" []
            -- * live deployment ops
            CheckStatus              -> Ops.checkstatus               o c
            Start                    -> getNodeNames'
                                        >>= Cardano.startNodes        o c
            Stop                     -> getNodeNames'
                                        >>= Cardano.stopNodes         o c
            FirewallBlock{..}        -> Cardano.firewallBlock         o c from to
            FirewallClear            -> Cardano.firewallClear         o c
            RunExperiment Nodes      -> getNodeNames'
                                        >>= Cardano.runexperiment     o c
            RunExperiment Timewarp   -> Timewarp.runexperiment        o c
            RunExperiment x          -> die $ "RunExperiment undefined for deployment " <> showT x
            PostExperiment           -> Cardano.postexperiment        o c
            DumpLogs{..}
              | Nodes        <- depl -> getNodeNames'
                                        >>= void . Cardano.dumpLogs  o c withProf
              | Timewarp     <- depl -> getNodeNames'
                                        >>= void . Timewarp.dumpLogs o c withProf
              | x            <- depl -> die $ "DumpLogs undefined for deployment " <> showT x
            PrintDate                -> getNodeNames'
                                        >>= Cardano.printDate        o c
            Template{..}             -> error "impossible"


runTemplate :: Options -> Command -> IO ()
runTemplate o@Options{..} Template{..} = do
  homeDir <- home
  let bname     = fromBranch tBranch
      branchDir = homeDir <> (fromText bname)
  exists <- testpath branchDir
  if exists
    then echo $ "Using existing git clone ..."
    else cmd o "git" ["clone", fromURL Ops.iohkNixopsURL, "-b", bname, bname]
  cd branchDir
  cmd o "git" (["config", "--replace-all", "receive.denyCurrentBranch", "updateInstead"])

  let config = Ops.mkConfig tBranch tEnvironment tTarget tDeployments tNodeLimit
  configFilename <- T.pack . Path.encodeString <$> Ops.writeConfig config

  echo ""
  echo $ "-- " <> (unsafeTextToLine $ configFilename) <> " is:"
  cmd o "cat" [configFilename]
runTemplate Options{..} _ = error "impossible"

runSetCardanoRev :: Options -> Commit -> IO ()
runSetCardanoRev o rev = do
  printf ("Setting cardano-sl commit to "%s%"\n") $ fromCommit rev
  spec <- incmd o "nix-prefetch-git" [fromURL Ops.cardanoSlURL, fromCommit rev]
  writeFile "cardano-sl-src.json" $ T.unpack spec

runSetExplorerRev :: Options -> Commit -> IO ()
runSetExplorerRev o rev = do
  printf ("Setting cardano-sl-explorer commit to "%s%"\n") $ fromCommit rev
  spec <- incmd o "nix-prefetch-git" [fromURL Ops.cardanoSlExplorerURL, fromCommit rev]
  writeFile "cardano-sl-explorer-src.json" $ T.unpack spec

runMiniKeys :: IO ()
runMiniKeys = do
  echo "Faking keys/key*.sk"
  testdir "keys"
    >>= flip unless (mkdir "keys")
  forM_ (41:[1..14]) $
    (\x-> do touch $ Turtle.fromText $ format ("keys/key"%d%".sk") x)
  echo "DataDog secrets cannot be faked, and must be provided:"
  printf "Enter static/datadog-api.secret: "
  Just apiKey <- readline
  writeTextFile "static/datadog-api.secret" $ lineToText apiKey
  printf "Enter static/datadog-application.secret: "
  Just appKey <- readline
  writeTextFile "static/datadog-application.secret" $ lineToText appKey
  echo "Minimum viable keyset complete."
