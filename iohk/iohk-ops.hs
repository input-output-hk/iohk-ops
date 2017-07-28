#!/usr/bin/env runhaskell
{-# LANGUAGE DataKinds, DeriveGeneric, GADTs, GeneralizedNewtypeDeriving, OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-orphans -Wno-type-defaults -Wno-unused-imports -Wno-unticked-promoted-constructors #-}

import           Control.Monad                    (forM, forM_)
import           Control.Monad.Trans.AWS   hiding (IAM, send)
import           Control.Lens              hiding ()
import           Data.Char                        (toLower)
import           Data.List
import           Data.Maybe
import           Data.Monoid                      ((<>))
import qualified Data.HashMap.Lazy             as Map
import           Data.Optional                    (Optional)
import qualified Data.Text                     as T
import qualified Filesystem.Path.CurrentOS     as Path
import           Network.AWS               hiding () -- send
import           Network.AWS.Auth
import           Network.AWS.EC2           hiding (DeleteTag, Snapshot, Stop)
import           Network.AWS.IAM           hiding (Any)
import           System.IO                     as Sys
import qualified System.Logger.Class           as Log
import qualified Text.Printf                   as T
import           Text.Read                        (readMaybe)
import           Turtle                    hiding (find, procs, shells)


-- * Local imports
import           NixOps                           (Branch(..), Commit(..), Environment(..), Deployment(..), Target(..)
                                                  ,Options(..), NixopsCmd(..), Project(..), URL(..)
                                                  ,showT, lowerShowT, errorT, cmd, incmd, projectURL, every)
import qualified NixOps                        as Ops

import qualified CardanoCSL                    as Cardano
import qualified Snapshot                      as Snapshot
import           Snapshot                         (Schedule(..))
import qualified Timewarp                      as Timewarp


-- Yes, -Wno-orphans.
deriving instance Enum Region
deriving instance Bounded Region


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

parserDeployment  :: Parser Deployment
parserDeployment  = argReadLower "DEPL" (pure $
                                         Turtle.HelpMessage $ "Deployment, one of: "
                                         <> T.intercalate ", " (lowerShowT <$> (every :: [Deployment])))
parserDeployments :: Parser [Deployment]
parserDeployments = (\(a, b, c, d) -> concat $ maybeToList <$> [a, b, c, d])
                    <$> ((,,,)
                         <$> (optional parserDeployment) <*> (optional parserDeployment) <*> (optional parserDeployment) <*> (optional parserDeployment))

parserDo :: Parser [Command Top]
parserDo = (\(a, b, c, d) -> concat $ maybeToList <$> [a, b, c, d])
           <$> ((,,,)
                 <$> (optional centralCommandParser) <*> (optional centralCommandParser) <*> (optional centralCommandParser) <*> (optional centralCommandParser))

newtype InstId  = InstId              Text   deriving (Eq, IsString, Show)
newtype InstTag = InstTag             Text   deriving (Eq, IsString, Show)
newtype AZ      = AZ      { fromAZ :: Text } deriving (Eq, IsString, Show)

parserAZ :: Optional HelpMessage -> Parser AZ
parserAZ  desc = AZ <$> optText "az" 'z'  desc

parserInstId :: Optional HelpMessage -> Parser InstId
parserInstId  desc =  InstId <$> argText "INSTANCE-ID"  desc

parserInstTag :: Optional HelpMessage -> Parser InstTag
parserInstTag desc = InstTag <$> argText "INSTANCE-TAG" desc

-- | Sum to track assurance
data Go = Go | Ask | Dry
  deriving (Eq, Read, Show)

parserGo  :: Parser Go
parserGo  = argRead "GO" "How to proceed before critical action: Go (fully automated), Ask (for confirmation) or Dry (no go)"


-- * Central command
--
data Kind = Top | EC2' | IAM'
data Command a where

  -- * setup
  Template              :: { tNodeLimit   :: Integer
                           , tHere        :: Bool
                           , tFile        :: Maybe Turtle.FilePath
                           , tEnvironment :: Environment
                           , tTarget      :: Target
                           , tBranch      :: Branch
                           , tDeployments :: [Deployment]
                           } -> Command Top
  SetRev                :: Project -> Commit -> Command Top
  FakeKeys              :: Command Top

  -- * building
  Genesis               :: Command Top
  GenerateIPDHTMappings :: Command Top
  Build                 :: Deployment -> Command Top
  AMI                   :: Command Top

  -- * cluster lifecycle
  Nixops                :: NixopsCmd -> [Text] -> Command Top
  Do                    :: [Command Top] -> Command Top
  Create                :: Command Top
  Modify                :: Command Top
  Deploy                :: Bool -> Bool -> Command Top
  Destroy               :: Command Top
  Delete                :: Command Top
  FromScratch           :: Command Top
  Status                :: Command Top

  -- * AWS
  EC2Sub                :: Maybe Region -> Maybe AZ -> Command EC2' -> Command Top
  Instances             :: Bool -> Command EC2'
  InstInfo              :: InstId -> Command EC2'
  SetTag                :: InstId -> InstTag -> Text -> Command EC2'
  DeleteTag             :: InstId -> InstTag -> Command EC2'
  ListSnapshottable     :: Command EC2'
  Snapshot              :: Go -> Command EC2'

  IAMSub                :: Command IAM' -> Command Top
  Whoami                :: Command IAM'

  -- * live cluster ops
  CheckStatus           :: Command Top
  Start                 :: Command Top
  Stop                  :: Command Top
  -- FirewallBlock         :: { from :: Region, to :: Region } -> Command Top
  FirewallClear         :: Command Top
  RunExperiment         :: Deployment -> Command Top
  PostExperiment        :: Command Top
  DumpLogs              :: { depl :: Deployment, withProf :: Bool } -> Command Top
  PrintDate             :: Command Top
deriving instance Show (Command a)

centralCommandParser :: Parser (Command Top)
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
    , ("fake-keys",             "Fake minimum set of keys necessary for a minimum complete deployment (explorer + report-server + nodes)",
                                                                                                    pure FakeKeys)
    , ("do",                    "Chain commands",                                                   Do <$> parserDo) ]

   <|> subcommandGroup "Build-related:"
    [ ("genesis",               "initiate production of Genesis in cardano-sl/genesis subdir",      pure Genesis)
    , ("generate-ipdht",        "Generate IP/DHT mappings for wallet use",                          pure GenerateIPDHTMappings)
    , ("build",                 "Build the application specified by DEPLOYMENT",                    Build <$> parserDeployment)
    , ("ami",                   "Build ami",                                                        pure AMI) ]

   <|> subcommandGroup "AWS:"
    [ ("ec2",                   "EC2 subcommand",
                                 EC2Sub
                                 <$> optional (optReadLower "region" 'r' "AWS region")
                                 <*> optional (parserAZ "AWS availability zone")
                                 <*> ec2CommandParser)
    , ("iam",                   "IAM subcommand",                                                   IAMSub <$> iamCommandParser) ]

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
   , ("info",                   "Invoke 'nixops info'",                                             pure Status)]

   <|> subcommandGroup "Live cluster ops:"
   [ ("checkstatus",            "Check if nodes are accessible via ssh and reboot if they timeout", pure CheckStatus)
   , ("start",                  "Start cardano-node service",                                       pure Start)
   , ("stop",                   "Stop cardano-node service",                                        pure Stop)
   -- , ("firewall-block-region",  "Block whole region in firewall",
   --                              FirewallBlock
   --                              <$> (Region <$> optReadLower "from-region" 'f' "AWS Region that won't reach --to")
   --                              <*> (Region <$> optReadLower "to-region"   't' "AWS Region that all nodes will be blocked"))
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

ec2CommandParser =
  subcommandGroup "General:"
    [ ("instances",             "Print instances on the chosen region",
                                Instances
                                <$> switch "tags" 't' "Print instance tags as well.")
    , ("info",                  "Print information about instance specified by INSTANCE-ID, in the chosen region",
                                InstInfo
                                <$> parserInstId "The ID of the instance to examine")
    , ("set-tag",               "Set INSTANCE-TAG of INSTANCE-ID, in the chosen region",
                                SetTag
                                <$> parserInstId  "The ID of the instance to examine"
                                <*> parserInstTag "The tag of the instance to set"
                                <*> argText "VALUE" "The new tag value")
    , ("unset-tag",             "Set INSTANCE-TAG of INSTANCE-ID, in the chosen region",
                                DeleteTag
                                <$> parserInstId  "The ID of the instance to examine"
                                <*> parserInstTag "The tag of the instance to set")
    , ("list-snapshottable",    "List instances that have the snapshot schedule set",               pure ListSnapshottable)
    , ("snapshot",              "WIP",
                                Snapshot
                                <$> (fromMaybe Ask
                                      <$> optional parserGo)) ]

iamCommandParser =
  subcommandGroup "General:"
    [ ("whoami",                "Print current access credentials",                                 pure Whoami) ]


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
        doCommand :: Options -> Ops.NixopsConfig -> Command Top -> IO ()
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
            Status                   -> Ops.nixops                    o c "info" []
            -- * AWS
            EC2Sub mayReg mayAZ cmd  -> runEC2                        o c (maybeRegOrAz mayReg mayAZ) cmd
            IAMSub cmd               -> runIAM                        o c cmd

            -- * live deployment ops
            CheckStatus              -> Ops.checkstatus               o c
            Start                    -> getNodeNames'
                                        >>= Cardano.startNodes        o c
            Stop                     -> getNodeNames'
                                        >>= Cardano.stopNodes         o c
            -- FirewallBlock{..}        -> Cardano.firewallBlock         o c from to
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


runTemplate :: Options -> Command Top -> IO ()
runTemplate o@Options{..} Template{..} = do
  when (elem (fromBranch tBranch) $ showT <$> (every :: [Deployment])) $
    die $ format ("the branch name "%w%" ambiguously refers to a deployment.  Cannot have that!") (fromBranch tBranch)
  homeDir <- home
  let bname     = fromBranch tBranch
      branchDir = homeDir <> (fromText bname)
  exists <- testpath branchDir
  case (exists, tHere) of
    (_, True) -> pure ()
    (True, _) -> echo $ "Using existing git clone ..."
    _         -> cmd o "git" ["clone", fromURL $ projectURL IOHK, "-b", bname, bname]

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


-- * AWS
--
-- type AWSConstraint r m = MonadBaseControl IO m

defaultRegion = Frankfurt

azRegionMap :: AZ -> Region
azRegionMap "ap-northeast-1a" = Tokyo
azRegionMap "ap-northeast-2c" = Seoul
azRegionMap "ap-southeast-1a" = Singapore
azRegionMap "ap-southeast-2b" = Sydney
azRegionMap "eu-central-1b"   = Frankfurt
azRegionMap "eu-west-1a"      = Ireland
azRegionMap "eu-west-1b"      = Ireland
azRegionMap "eu-west-1c"      = Ireland
azRegionMap "us-west-2b"      = Oregon
azRegionMap (AZ x)            = errorT $ "Unknown AZ '" <> x <> "'"

maybeRegOrAz :: Maybe Region -> Maybe AZ -> Maybe Region
maybeRegOrAz Nothing mayAZ = azRegionMap <$> mayAZ
maybeRegOrAz mayReg  _     = mayReg

snapshotSchedule :: Schedule
snapshotSchedule =
  Schedule
  { minGenerations = 3
  , minAge         = 48 * 3600
  }

snapshotArgs :: Options -> Ops.NixopsConfig -> Snapshot.Schedule -> Region -> Snapshot.Args
snapshotArgs Options{..} Ops.NixopsConfig{..} schedule region =
  Snapshot.Args
  { _argsCredsFP         = ""
  , _argsRegion          = region
  , _argsDefaultSchedule = schedule
  , _argsOnlyInstances   = []
  , _argsLogLevel        = Log.Info
  }

withAWS :: Options -> (forall m . (MonadAWS m) => m a) -> IO a
withAWS Options{..} awsAction = do
  lgr <- newLogger (if oDebug then Debug else Info) Sys.stdout
  env <- newEnv Discover
        <&> set envLogger lgr
        <&> set envRegion defaultRegion
  (runResourceT . runAWST env) awsAction

--   lgr'  <- newLogger AWS.Info stdout

--   -- create the AWS execution enviroment for the specified region
--   env  <- newEnv (FromFile "default" $ _argsCredsFP args')
--                   <&> envLogger .~ lgr' <&> envRegion .~ (_argsRegion args')

--   -- with logger for non-aws - run the system
--   lgr <- SL.new (userLogSettings (_argsLogLevel args'))
--   runResourceT . runAWS env
--                . evalStateT (runReaderT main' args')
--                $ lgr
--   where
--     userLogSettings l = Log.setDelimiter "|" $ Log.setLogLevel l Log.defSettings

allRegions :: [Region]
allRegions = [NorthVirginia, Ohio, NorthCalifornia, Oregon, Tokyo, Seoul, Mumbai, Singapore, Sydney, SaoPaulo, Ireland, Frankfurt]
forAWSRegions :: Options -> [Region] -> (forall m . MonadAWS m => Region -> m a) -> IO [a]
forAWSRegions Options{..} rs awsAction = do
  lgr <- newLogger (if oDebug then Debug else Info) Sys.stderr
  forM rs $
    \r-> do
      env <- newEnv Discover
        <&> set envLogger lgr
        <&> set envRegion r
      (runResourceT . runAWST env) (awsAction r)

forAWSRegions_ :: Options -> [Region] -> (forall m . MonadAWS m => Region -> m a) -> IO ()
forAWSRegions_ o rs a = forAWSRegions o rs a >> pure ()

doEC2 :: Options -> Ops.NixopsConfig -> Maybe Region
      -> (forall m . MonadAWS m => Region -> m ()) -> IO ()
doEC2 o _c maybeRegion action =
  forAWSRegions_ o (fromMaybe allRegions $ ((:[]) <$> maybeRegion))
  $ \r -> action r


-- * IAM
--
whoami :: (MonadAWS m) => m User
whoami = do
  gurs <- send $ getUser
  pure $ gurs^.gursUser

runIAM :: Options -> Ops.NixopsConfig -> Command IAM' -> IO ()
runIAM o _c Whoami = withAWS o $ do
  user <- whoami
  printf (w%"\n") user
  pure ()


-- * EC2
--
ppTag :: Tag -> Text
ppTag x = format (s%":"%s) (x^.tagKey) (x^.tagValue)

instTag :: Instance -> Text -> Maybe Tag
instTag ins tagName =
  find ((== tagName) . (^.tagKey)) $ ins^.insTags

instTagVal' :: Instance -> Text -> Text -> Text
instTagVal' ins tagName def = instTag ins tagName
                              <&> (^.tagValue)
                              & fromMaybe def

getInstsCurrentRegion :: MonadAWS m => m [Instance]
getInstsCurrentRegion = send describeInstances
                           <&> concat . ((^.rInstances) <$>) . (^.dirsReservations)

getInstsGlobal :: Options -> IO [Instance]
getInstsGlobal o = forAWSRegions o allRegions (\_ -> getInstsCurrentRegion)
                     <&> concat

instTagVal :: Instance -> Text -> Text
instTagVal ins tagName = instTagVal' ins tagName (errorT $ "Instance has no tag '" <> tagName <> "'")

instName :: Instance -> Text
instName ins = instTagVal' ins "Name" ""

instAZ :: Instance -> Maybe AZ
instAZ inst = AZ <$> inst^.insPlacement.pAvailabilityZone

instAZPretty :: Instance -> AZ
instAZPretty = fromMaybe (AZ "--unknown-AZ--") . instAZ

instInfo :: MonadAWS m => Options -> InstId -> m ()
instInfo _o (InstId instId) = do
  insts <- getInstsCurrentRegion
  let inst = flip find insts (\i-> i^.insInstanceId == instId)
             & fromMaybe (error $ T.printf "No instance with id '%s' in current region." instId)
  liftIO $ T.printf ("%s %s \"%s\"  tags:  %s\n")
    (inst^.insInstanceId) (fromAZ $ instAZPretty inst) (instName inst) (T.intercalate " " $ ppTag <$> inst^.insTags)

printInstsTags :: MonadIO m => Bool -> [Instance] -> m ()
printInstsTags printTags ists = do
  forM_ ists $
    \ins -> do
      liftIO $ T.printf ("  %45s  %20s  %s") (instName ins) (ins^.insInstanceId) (fromAZ $ instAZPretty ins) -- (ins^.insImageId) (T.intercalate " " $ ppTag <$> ins^.insTags)
      when printTags $
        liftIO $ do
        putStr "  TAGS:  "
        T.printf "%s" $ (T.intercalate ", " $ ppTag <$> ins^.insTags)
      liftIO $ putStrLn ""

runEC2 :: Options -> Ops.NixopsConfig -> Maybe Region -> Command EC2' -> IO ()
runEC2 o c rs (Instances printTags) = doEC2 o c rs $ \r -> do
  liftIO $ putStrLn $ "region " <> show r
  getInstsCurrentRegion
    >>= printInstsTags printTags

runEC2 o c rs (InstInfo instId) = doEC2 o c rs $  \_r -> do
  instInfo o instId

runEC2 o c rs (SetTag (InstId instId) (InstTag tagName) tagValue) = doEC2 o c rs $ \_r -> do
  _ctrs <- send $
    (createTags
     & cResources .~ [instId]
     & cTags      .~ [tag tagName tagValue])
  pure ()

-- | WARNING: this is inefficient (two reqs instead of one), because deleteTags is
--   slightly broken in amazonka-ec2 -- see semantics of
--   http://hackage.haskell.org/package/amazonka-ec2-1.4.5/docs/Network-AWS-EC2-DeleteTags.html#v:dtsTags
--   ..and observe how Tag' has no way to encode absence of value.
runEC2 o c rs (DeleteTag (InstId instId) (InstTag tagName)) = doEC2 o c rs $ \_r -> do
  _ctrs <- send $
    (createTags
     & cResources .~ [instId]
     & cTags      .~ [tag tagName ""])
  _dtrs <- send $
    (deleteTags
     & dtsResources .~ [instId]
     & dtsTags      .~ [tag tagName ""])
  pure ()

runEC2 o _c _r ListSnapshottable = do
  allInsts <- getInstsGlobal o
  let snapshottable = flip filter allInsts $
                      isJust . flip instTag Snapshot.schedule'tag
  printInstsTags False snapshottable

runEC2 o _c _r (Snapshot go) = do
  echo "Querying global instance list."
  allInsts <- getInstsGlobal o

  let snapshottable = flip filter allInsts $
                      isJust . flip instTag Snapshot.schedule'tag
      regionInsts  = Map.fromList [ (azRegionMap $ instAZPretty inst, inst)
                                   | inst <- snapshottable ]
      regions       = Map.keys regionInsts
  printf ("Snapshottable instances ("%d%" of total "%d%") in "%d%" regions:\n")
         (length snapshottable) (length allInsts) (Map.size regionInsts)
  printInstsTags False snapshottable

  case go of
    Dry -> do
      echo "Dry run mode, exiting."
      exit $ ExitFailure 1
    Ask -> do
      echo "Confirmation mode, enter 'yes' to proceed:"
      x <- readline
      unless (x == Just "yes") $ do
        echo "User declined to proceed, exiting."
        exit $ ExitFailure 1
    Go  -> pure ()

  forAWSRegions_ o regions $
    \_region-> do
      liftIO $ echo "Initiating snapshotting"
      Snapshot.processAllInstances (length snapshottable) $ zip [1..] snapshottable
