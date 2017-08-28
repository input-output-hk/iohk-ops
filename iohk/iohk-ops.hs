#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings, RecordWildCards, StandaloneDeriving, TupleSections, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults #-}

import           Control.Monad                    (forM_)
import           Data.Char                        (toLower)
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Monoid                      ((<>))
import           Data.Optional (Optional)
import qualified Data.Text                     as T
import qualified Filesystem.Path.CurrentOS     as Path
import qualified System.Environment            as Sys
import           Turtle                    hiding (env, err, fold, inproc, prefix, procs, shells, e, f, o, x)
import           Time.Types
import           Time.System


import           NixOps                           (Branch(..), Commit(..), Environment(..), Deployment(..), Target(..)
                                                  ,Options(..), NixopsCmd(..), NixopsDepl(..), Project(..), Exec(..), Arg(..)
                                                  ,showT, lowerShowT, errorT, cmd, every, fromNodeName)
import qualified NixOps                        as Ops
import qualified CardanoCSL                    as Cardano
import           Topology


-- * Elementary parsers
--
-- | Given a string, either return a constructor that being 'show'n case-insensitively matches the string,
--   or raise an error, explaining what went wrong.
diagReadCaseInsensitive :: (Bounded a, Enum a, Read a, Show a) => String -> Maybe a
diagReadCaseInsensitive str = diagRead $ toLower <$> str
  where mapping    = Map.fromList [ (toLower <$> show x, x) | x <- every ]
        diagRead x = Just $ flip fromMaybe (Map.lookup x mapping)
                     (errorT $ format ("Couldn't parse '"%s%"' as one of: "%s%"\n")
                                        (T.pack str) (T.pack $ intercalate ", " $ Map.keys mapping))

optReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optReadLower = opt (diagReadCaseInsensitive . T.unpack)
argReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> Optional HelpMessage -> Parser a
argReadLower = arg (diagReadCaseInsensitive . T.unpack)

parserBranch :: Optional HelpMessage -> Parser Branch
parserBranch desc = Branch <$> argText "branch" desc

parserCommit :: Optional HelpMessage -> Parser Commit
parserCommit desc = Commit <$> argText "commit" desc

parserEnvironment :: Parser Environment
parserEnvironment = fromMaybe Ops.defaultEnvironment <$> optional (optReadLower "environment" 'e' $ pure $
                                                                   Turtle.HelpMessage $ "Environment: "
                                                                   <> T.intercalate ", " (lowerShowT <$> (every :: [Environment])) <> ".  Default: development")

parserTarget      :: Parser Target
parserTarget      = fromMaybe Ops.defaultTarget      <$> optional (optReadLower "target"      't' "Target: aws, all;  defaults to AWS")

parserProject     :: Parser Project
parserProject     = argReadLower "project" $ pure $ Turtle.HelpMessage ("Project to set version of: " <> T.intercalate ", " (lowerShowT <$> (every :: [Project])))

parserNodeName    :: Parser NodeName
parserNodeName    = NodeName <$> (argText "NODE" $ pure $
                                   Turtle.HelpMessage $ "Node to operate on. Defaults to '" <> (fromNodeName $ Ops.defaultNode) <> "'")

parserDeployment  :: Parser Deployment
parserDeployment  = argReadLower "DEPL" (pure $
                                         Turtle.HelpMessage $ "Deployment, one of: "
                                         <> T.intercalate ", " (lowerShowT <$> (every :: [Deployment])))
parserDeployments :: Parser [Deployment]
parserDeployments = (\(a, b, c, d) -> concat $ maybeToList <$> [a, b, c, d])
                    <$> ((,,,)
                         <$> (optional parserDeployment) <*> (optional parserDeployment) <*> (optional parserDeployment) <*> (optional parserDeployment))


-- * Central command
--
data Command where

  -- * setup
  Clone                 :: { cBranch      :: Branch } -> Command
  Template              :: { tFile        :: Maybe Turtle.FilePath
                           , tNixops      :: Maybe Turtle.FilePath
                           , tTopology    :: Maybe Turtle.FilePath
                           , tEnvironment :: Environment
                           , tTarget      :: Target
                           , tName        :: NixopsDepl
                           , tDeployments :: [Deployment]
                           } -> Command
  SetRev                :: Project -> Commit -> Bool -> Command
  FakeKeys              :: Command
  UpdateNixops          :: Command

  -- * building
  Genesis               :: Branch -> Command
  GenerateIPDHTMappings :: Command
  Build                 :: Deployment -> Command
  AMI                   :: Command

  -- * cluster lifecycle
  Nixops'               :: NixopsCmd -> [Arg] -> Command
  Create                :: Command
  Modify                :: Command
  Deploy                :: Bool -> Bool -> Bool -> Bool -> Maybe Seconds -> Command
  Destroy               :: Command
  Delete                :: Command
  FromScratch           :: Command
  Info                  :: Command

  -- * Deployment
  DeployStaging0        :: Branch -> Bool -> Command

  -- * live cluster ops
  Ssh                   :: Exec -> [Arg] -> Command
  DeployedCommit        :: NodeName -> Command
  CheckStatus           :: Command
  StartForeground       :: Command
  Stop                  :: Command
  RunExperiment         :: Deployment -> Command
  PostExperiment        :: Command
  DumpLogs              :: { depl :: Deployment, withProf :: Bool } -> Command
  WipeJournals          :: Command
  GetJournals           :: Command
  WipeNodeDBs           :: Command
  PrintDate             :: Command
deriving instance Show Command

centralCommandParser :: Parser Command
centralCommandParser =
  (    subcommandGroup "General:"
    [ ("clone",                 "Clone an 'iohk-ops' repository branch",
                                Clone
                                <$> parserBranch "'iohk-ops' branch to checkout")
    , ("template",              "Produce (or update) a checkout of BRANCH with a configuration YAML file (whose default name depends on the ENVIRONMENT), primed for future operations.",
                                Template
                                <$> optional (optPath "config"    'c' "Override the default, environment-dependent config filename")
                                <*> optional (optPath "nixops"    'n' "Use a specific Nixops binary for this cluster")
                                <*> optional (optPath "topology"  't' "Cluster configuration.  Defaults to 'topology.yaml'")
                                <*> parserEnvironment
                                <*> parserTarget
                                <*> (NixopsDepl <$> argText "NAME"  "Nixops deployment name")
                                <*> parserDeployments)
    , ("set-rev",               "Set commit of PROJECT dependency to COMMIT, and commit the resulting changes",
                                SetRev
                                <$> parserProject
                                <*> parserCommit "Commit to set PROJECT's version to"
                                <*> (fromMaybe True
                                      <$> optional (switch "commit" 'n' "pkgs/generate.sh, then commit the *-src.json and pkgs/.")))
    , ("fake-keys",             "Fake minimum set of keys necessary for a minimum complete deployment (explorer + report-server + nodes)",  pure FakeKeys)
    , ("update-nixops",         "Rebuild and bump 'nixops' to the version checked out in the 'nixops' subdirectory.  WARNING: non-chainable, since it updates the config file.",
                                pure UpdateNixops)]

   <|> subcommandGroup "Build-related:"
    [ ("genesis",               "initiate production of Genesis in cardano-sl/genesis subdir",
                                Genesis
                                <$> parserBranch "'cardano-sl' branch to update with the new genesis")
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
   , ("deploy",                 "Deploy the whole cluster",
                                Deploy
                                <$> switch "evaluate-only"       'e' "Pass --evaluate-only to 'nixops build'"
                                <*> switch "build-only"          'b' "Pass --build-only to 'nixops build'"
                                <*> switch "check"               'c' "Pass --check to 'nixops build'"
                                <*> switch "no-explorer-rebuild" 'n' "Don't rebuild explorer frontend.  WARNING: use this only if you know what you are doing!"
                                <*> ((Seconds . (* 60) . fromIntegral <$>)
                                      <$> optional (optInteger "bump-system-start-held-by" 't' "Bump cluster --system-start time, and add this many minutes to delay")))
   , ("destroy",                "Destroy the whole cluster",                                        pure Destroy)
   , ("delete",                 "Unregistr the cluster from NixOps",                                pure Delete)
   , ("fromscratch",            "Destroy, Delete, Create, Deploy",                                  pure FromScratch)
   , ("info",                   "Invoke 'nixops info'",                                             pure Info)
   , ("deploy-staging-phase0",  "Deploy 'staging', optionally with genesis regeneration",
                                DeployStaging0
                                <$> parserBranch "'cardano-sl' branch to update & deploy"
                                <*> switch "with-genesis"        'g' "Regenerate genesis")]

   <|> subcommandGroup "Live cluster ops:"
   [ ("deployed-commit",        "Print commit id of 'cardano-node' running on MACHINE of current cluster.",
                                DeployedCommit
                                <$> parserNodeName)
   , ("ssh",                    "Execute a command on cluster nodes.  Use --on to limit",
                                Ssh <$> (Exec <$> (argText "CMD" "")) <*> many (Arg <$> (argText "ARG" "")))
   , ("checkstatus",            "Check if nodes are accessible via ssh and reboot if they timeout", pure CheckStatus)
   , ("start-foreground",       "Start cardano (or explorer) on the specified node (--on), in foreground",
                                 pure StartForeground)
   , ("stop",                   "Stop cardano-node service",                                        pure Stop)
   , ("runexperiment",          "Deploy cluster and perform measurements",                          RunExperiment <$> parserDeployment)
   , ("postexperiment",         "Post-experiments logs dumping (if failed)",                        pure PostExperiment)
   , ("dumplogs",               "Dump logs",
                                DumpLogs
                                <$> parserDeployment
                                <*> switch "prof"         'p' "Dump profiling data as well (requires service stop)")
   , ("wipe-journals",          "Wipe *all* journald logs on cluster",                              pure WipeJournals)
   , ("get-journals",           "Obtain cardano-node journald logs from cluster",                   pure GetJournals)
   , ("wipe-node-dbs",          "Wipe *all* node databases on cluster",                             pure WipeNodeDBs)
   , ("date",                   "Print date/time",                                                  pure PrintDate)]

   <|> subcommandGroup "Other:"
    [ ])


main :: IO ()
main = do
  args <- (Arg . T.pack <$>) <$> Sys.getArgs
  (opts@Options{..}, topcmds) <- options "Helper CLI around IOHK NixOps. For example usage see:\n\n  https://github.com/input-output-hk/internal-documentation/wiki/iohk-ops-reference#example-deployment" $
                     (,) <$> Ops.parserOptions <*> many centralCommandParser
  forM_ topcmds $ runTop opts args

runTop :: Options -> [Arg] -> Command -> IO ()
runTop o@Options{..} args topcmd = do
  case topcmd of
    Clone{..}                   -> runClone           o cBranch
    Template{..}                -> runTemplate        o topcmd  args
    SetRev proj comId mCom      -> Ops.runSetRev      o proj comId $
                                   if not mCom then Nothing
                                   else Just $ format ("Bump "%s%" revision to "%s%"; pkgs/generate.sh") (lowerShowT proj) (fromCommit comId)

    _ -> do
      -- XXX: Config filename depends on environment, which defaults to 'Development'
      let cf = flip fromMaybe oConfigFile $
               Ops.envConfigFilename Any
      c <- Ops.readConfig o cf

      when oVerbose $
        printf ("-- command "%s%"\n-- config '"%fp%"'\n") (showT topcmd) cf

      doCommand o c topcmd
    where
        doCommand :: Options -> Ops.NixopsConfig -> Command -> IO ()
        doCommand o@Options{..} c@Ops.NixopsConfig{..} cmd = do
          case cmd of
            -- * setup
            FakeKeys                 -> Ops.runFakeKeys
            -- * building
            Genesis branch           -> Ops.generateGenesis           o c branch
            GenerateIPDHTMappings    -> void $
                                        Cardano.generateIPDHTMappings o c
            Build depl               -> Ops.build                     o c depl
            AMI                      -> Cardano.buildAMI              o c
            -- * deployment lifecycle
            Nixops' cmd args         -> Ops.nixops                    o c cmd args
            UpdateNixops             -> Ops.updateNixops              o c
            Create                   -> Ops.create                    o c
            Modify                   -> Ops.modify                    o c
            Deploy ev bu ch ner buh  -> Ops.deploy                    o c ev bu ch (not ner) buh
            Destroy                  -> Ops.destroy                   o c
            Delete                   -> Ops.delete                    o c
            FromScratch              -> Ops.fromscratch               o c
            Info                     -> Ops.nixops                    o c "info" []
            DeployStaging0 br genp   -> Ops.deployStagingPhase0       o c br genp
            -- * live deployment ops
            DeployedCommit m         -> Ops.deployed'commit           o c m
            CheckStatus              -> Ops.checkstatus               o c
            StartForeground          -> Ops.startForeground           o c $
                                        flip fromMaybe oOnlyOn $ error "'start-foreground' requires a global value for --on/-o"
            Ssh exec args            -> Ops.parallelSSH               o c exec args
            Stop                     -> Ops.stop                      o c
            RunExperiment Nodes      -> Cardano.runexperiment     o c
            RunExperiment x          -> die $ "RunExperiment undefined for deployment " <> showT x
            PostExperiment           -> Cardano.postexperiment        o c
            DumpLogs{..}
              | Nodes        <- depl -> Cardano.dumpLogs              o c withProf >> pure ()
              | x            <- depl -> die $ "DumpLogs undefined for deployment " <> showT x
            WipeJournals             -> Ops.wipeJournals              o c
            GetJournals              -> Ops.getJournals               o c
            WipeNodeDBs              -> Ops.wipeNodeDBs               o c
            PrintDate                -> Ops.date                      o c
            Clone{..}                -> error "impossible"
            Template{..}             -> error "impossible"
            SetRev   _ _ _           -> error "impossible"


runClone :: Options -> Branch -> IO ()
runClone o@Options{..} branch = do
  let bname     = fromBranch branch
      branchDir = fromText bname
  exists <- testpath branchDir
  if exists
  then  echo $ "Using existing git clone ..."
  else cmd o "git" ["clone", Ops.fromURL $ Ops.projectURL IOHKOps, "-b", bname, bname]

  cd branchDir
  cmd o "git" (["config", "--replace-all", "receive.denyCurrentBranch", "updateInstead"])

runTemplate :: Options -> Command -> [Arg] -> IO ()
runTemplate o@Options{..} Template{..} args = do
  when (elem (fromNixopsDepl tName) $ showT <$> (every :: [Deployment])) $
    die $ format ("the deployment name "%w%" ambiguously refers to a deployment _type_.  Cannot have that!") (fromNixopsDepl tName)

  systemStart <- timeCurrent
  let cmdline = T.concat $ intersperse " " $ fromArg <$> args
  config <- Ops.mkConfig o cmdline tName tNixops tTopology tEnvironment tTarget tDeployments systemStart
  configFilename <- T.pack . Path.encodeString <$> Ops.writeConfig tFile config

  echo ""
  echo $ "-- " <> (unsafeTextToLine $ configFilename) <> " is:"
  cmd o "cat" [configFilename]
runTemplate _ _ _ = error "impossible"
