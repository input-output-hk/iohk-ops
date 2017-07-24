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
                                                  ,Options(..), NixopsCmd(..), Project(..), Region(..), URL(..)
                                                  ,showT, lowerShowT, cmd, incmd, projectURL)
import qualified NixOps                        as Ops
import qualified CardanoCSL                    as Cardano
import qualified Timewarp                      as Timewarp


-- * Elementary parsers
--
optReadLower :: Read a => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optReadLower = opt (readMaybe . T.unpack . T.toTitle)
argReadLower :: Read a => ArgName -> Optional HelpMessage -> Parser a
argReadLower = arg (readMaybe . T.unpack . T.toTitle)

parserBranch :: Optional HelpMessage -> Parser Branch
parserBranch desc = Branch <$> argText "branch" desc

parserCommit :: Optional HelpMessage -> Parser Commit
parserCommit desc = Commit <$> argText "commit" desc

parserEnvironment :: Parser Environment
parserEnvironment = fromMaybe Ops.defaultEnvironment <$> optional (optReadLower "environment" 'e' $ pure $
                                                                   Turtle.HelpMessage $ "Environment: "
                                                                   <> T.intercalate ", " (lowerShowT <$> Ops.allEnvironments) <> ".  Default: development")

parserTarget      :: Parser Target
parserTarget      = fromMaybe Ops.defaultTarget      <$> optional (optReadLower "target"      't' "Target: aws, all;  defaults to AWS")

parserProject     :: Parser Project
parserProject     = argReadLower "project" $ pure $ Turtle.HelpMessage ("Project to set version of: " <> T.intercalate ", " (lowerShowT <$> Ops.allProjects))

parserDeployment  :: Parser Deployment
parserDeployment  = argReadLower "DEPL" (pure $
                                         Turtle.HelpMessage $ "Deployment, one of: "
                                         <> T.intercalate ", " (lowerShowT <$> Ops.allDeployments))
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
                           , tHere        :: Bool
                           , tFile        :: Maybe Turtle.FilePath
                           , tEnvironment :: Environment
                           , tTarget      :: Target
                           , tBranch      :: Branch
                           , tDeployments :: [Deployment]
                           } -> Command
  SetRev                :: Project -> Commit -> Command
  FakeKeys              :: Command

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
  Deploy                :: Bool -> Bool -> Command
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
    [ ("template",              "Produce (or update) a checkout of BRANCH with a configuration YAML file (whose default name depends on the ENVIRONMENT), primed for future operations.",
                                Template
                                <$> (fromMaybe Ops.defaultNodeLimit
                                     <$> optional (optInteger "node-limit" 'l' "Limit cardano-node count to N"))
                                <*> (fromMaybe False
                                      <$> optional (switch "here" 'h' "Instead of cloning a subdir, operate on a config in the current directory"))
                                <*> (optional (optPath "config" 'c' "Override the default, environment-dependent config filename"))
                                <*> parserEnvironment
                                <*> parserTarget
                                <*> parserBranch "iohk-nixops branch to check out"
                                <*> parserDeployments)
    , ("set-rev",               "Set commit of PROJECT dependency to COMMIT",
                                SetRev
                                <$> parserProject
                                <*> parserCommit "Commit to set PROJECT's version to")
    , ("fake-keys",             "Fake minimum set of keys necessary for a minimum complete deployment (explorer + report-server + nodes)",  pure FakeKeys)
    , ("do",                    "Chain commands",                                                   Do <$> parserDo) ]

   <|> subcommandGroup "Build-related:"
    [ ("genesis",               "initiate production of Genesis in cardano-sl/genesis subdir",      pure Genesis)
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
                                <$> switch "evaluate-only" 'e' "Pass --evaluate-only to 'nixops'."
                                <*> switch "build-only"    'b' "Pass --build-only to 'nixops'.")
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
  (o@Options{..}, topcmd) <- options "Helper CLI around IOHK NixOps. For example usage see:\n\n  https://github.com/input-output-hk/internal-documentation/wiki/iohk-ops-reference#example-deployment" $
                             (,) <$> Ops.parserOptions <*> centralCommandParser

  case topcmd of
    Template{..}                -> runTemplate        o topcmd
    SetRev       project commit -> runSetRev          o project commit

    _ -> do
      -- XXX: Config filename depends on environment, which defaults to 'Development'
      let cf = flip fromMaybe oConfigFile $
               Ops.envConfigFilename Any
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
            FakeKeys                 -> runFakeKeys
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
            Deploy evonly buonly     -> Ops.deploy                    o c evonly buonly
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
            SetRev   _ _             -> error "impossible"


runTemplate :: Options -> Command -> IO ()
runTemplate o@Options{..} Template{..} = do
  when (elem (fromBranch tBranch) $ showT <$> Ops.allDeployments) $
    die $ format ("the branch name "%w%" ambiguously refers to a deployment.  Cannot have that!") (fromBranch tBranch)
  homeDir <- home
  let bname     = fromBranch tBranch
      branchDir = homeDir <> (fromText bname)
  exists <- testpath branchDir
  case (exists, tHere) of
    (_, True) -> pure ()
    (True, _) -> echo $ "Using existing git clone ..."
    _         -> cmd o "git" ["clone", fromURL $ projectURL Nixpkgs, "-b", bname, bname]

  unless tHere $ do
    cd branchDir
    cmd o "git" (["config", "--replace-all", "receive.denyCurrentBranch", "updateInstead"])

  Ops.GithubSource{..} <- Ops.readSource Ops.githubSource Nixpkgs

  let config = Ops.mkConfig tBranch ghRev tEnvironment tTarget tDeployments tNodeLimit
  configFilename <- T.pack . Path.encodeString <$> Ops.writeConfig tFile config

  echo ""
  echo $ "-- " <> (unsafeTextToLine $ configFilename) <> " is:"
  cmd o "cat" [configFilename]
runTemplate Options{..} _ = error "impossible"

runSetRev :: Options -> Project -> Commit -> IO ()
runSetRev o proj rev = do
  printf ("Setting '"%s%"' commit to "%s%"\n") (lowerShowT proj) (fromCommit rev)
  spec <- incmd o "nix-prefetch-git" ["--no-deepClone", fromURL $ projectURL proj, fromCommit rev]
  writeFile (T.unpack $ format fp $ Ops.projectSrcFile proj) $ T.unpack spec

runFakeKeys :: IO ()
runFakeKeys = do
  echo "Faking keys/key*.sk"
  testdir "keys"
    >>= flip unless (mkdir "keys")
  forM_ (41:[1..14]) $
    (\x-> do touch $ Turtle.fromText $ format ("keys/key"%d%".sk") x)
  echo "Minimum viable keyset complete."
