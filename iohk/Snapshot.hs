{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snapshot where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Data
import           Data.List ((\\), groupBy, intersperse, nub, sort, sortBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Format as T
import           Data.Text.Format hiding (print)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as TL
import           Data.Time
import qualified Network.AWS as AWS
import           Network.AWS hiding (Info)
import           Network.AWS.EC2
import           System.Exit
import           System.IO
import qualified System.Logger as SL
import           System.Logger.Class (MonadLogger (..))
import qualified System.Logger.Class as Log
import           System.Logger.Message
import           System.Random


-- for defaults see end of file

-- 1) go to the instance and add a tag of

schedule'tag :: Text
schedule'tag = "AutoSnapshotSchedule"

-- format for this not yet decided - so the tag value is currently
-- ignored, however the default schedule is

default'schedule :: Schedule
default'schedule = Schedule 3 (48 * 3600)

-- NOTE all volumes must have an associated tag "Name" if you want to
-- supress the automatic management of a particular volume on an
-- instace (e.g. it is completely ephmemeral) then add the following
-- tag to the volume

disablevol'tag :: Text
disablevol'tag = "AutoSnapshot" -- "disabled|Disabled"

-- the case of the value of the tag is ignored
-- The snapshots will be created with the tag

autoclean'tag :: Text
autoclean'tag = "AutoSnapshotCleanEnabled"

-- with the value set to True (case sensistive) , if you want to take
-- a particular snapshot out of the auto cleaning schedule, change it.

-- When a snapshot is intitated, the following tag is added to the
-- instance, this is used to limit the number of snapshots taken when
-- an instance is not running.

lastsnapshot'tag :: Text
lastsnapshot'tag = "AutoSnapshotLastTaken"

-- this (when present) will have one of two formats, the first is when
-- the snapshot was taken on a running instance - this has the time of
-- that snapshot - the second when the snapshot was taken on a
-- non-running instance - this is the time of the snapshot/time the
-- instance was last launched.
--
-- This permit the system to detect that between to snapshot times the
-- system was run then stopped.

type EnvM = ReaderT Args (StateT Log.Logger AWS)

instance MonadLogger EnvM where
  log lvl m = get >>= \l -> SL.log l lvl m

data Schedule = Schedule
    { minGenerations  :: Int
    , minAge          :: NominalDiffTime
    } deriving (Data, Typeable, Show)


data Args = Args
    { _argsCredsFP         :: FilePath
    , _argsRegion          :: Region
    , _argsDefaultSchedule :: Schedule
    , _argsOnlyInstances   :: [Text]
    , _argsLogLevel        :: Log.Level
    } deriving (Data, Typeable, Show)

-- | Get the instances from the region - if instances are supplied on the
--   command line, restrict the action to those instances mentioned
-- main :: IO ()
-- main = do
--   -- process the command line arguments
--   args' <- cmdArgs default'args
--   when (null $ _argsCredsFP args') $ do
--     hPutStrLn stderr "you must supply a credentials file, try -? for help"
--     exitFailure

--   -- set up a default logger for the Aws
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

main' :: EnvM ()
main' = do
  is <- selectInstances >>= liftIO . randomise
  when (null is) $ do
    Log.err $ msg ("no actionable instances" :: Text)
    liftIO exitFailure
  processAllInstances (length is) $ zip [1..] is
  logFlush


-- select the instances
selectInstances :: EnvM [Instance]
selectInstances = do
  a'is <- getManagedInstances
  Log.debug . field "available instances" . T.concat . intersperse ","
            $ map (view insInstanceId) a'is
  selected <- asks _argsOnlyInstances
  case selected of
    [] -> return a'is
    os -> let r'is  = filter (\x -> view insInstanceId x `elem` os) a'is
              r'is' = map (view insInstanceId) r'is
              miss  = r'is' \\ (nub os)
          in do unless (null miss) $ do
                  Log.warn . field "specified instances not found" . T.concat
                           $ intersperse "," miss
                return r'is


-- process all the instances, trapping any failures
processAllInstances :: Int -> [(Int, Instance)] -> EnvM ()
processAllInstances total iss =
 mapM_ runOne iss
 where
   runOne (n,inst) = let
     iname  = fromMaybe "<no-name>"
              $ extractTagValueI "Name" inst
     s'msg = T.format "Start processing {} ({}/{}) ({})"
                          (view insInstanceId inst,  n,  total,  iname)
     f'msg = T.format "Finished processing {} ({}/{}) ({})"
                          (view insInstanceId inst,  n,  total,  iname)
     in do Log.info $ msg s'msg
           (r :: Either SomeException ()) <- try $ processSingleInstance inst
           case r of
             Right () -> Log.info $ msg f'msg
             Left  e  -> Log.warn
                         $ msg $ T.format "Error processing {}: '{}'"
                                 (view insInstanceId inst, show e)


-- process a single instance
processSingleInstance :: Instance -> EnvM ()
processSingleInstance ins = do
  -- get schedule (note only default schedule currently returned)
  sched <- extractSchedule ins

  -- clean old snapshots
  (most'recent, snapshot'set) <- cleanSnapshots sched ins
  info'a most'recent

  -- decide on, and make, new snapshot
  snapshot'needed <- instanceNeedsSnapshot ins most'recent
  when (isJust snapshot'needed)
    $ snapshotInstance ins (fromJust snapshot'needed) snapshot'set
  where
    info'a x
      = Log.info $ field "instance" (view insInstanceId ins)
                 . field "most recent snapshot" (maybe "<none>" show x)

-- == Snapshot processing ==
-- clean the snapshots
cleanSnapshots :: Schedule -> Instance -> EnvM (Maybe UTCTime, [Volume])
cleanSnapshots sched i = do
  vols' <- getAttachedVolumeIds (view insInstanceId i)
  vols  <- filterVolumes vols'
  trace'a vols' vols
  sns   <- getSnapshots vols
  trace'b sns
  sns't <- trimSnapshots sched sns
  trace'c sns't
  forM_ sns't $ \sn -> do
    debug'd sn
    y <- deleteVolSnapshot sn
    when (isJust y) $ warn'a sn (fromJust y)
  return (most'recent sns, vols)
  where
   most'recent x
     = listToMaybe . reverse . sort
       $ Map.foldr (\a b -> (view sStartTime $ head a):b) [] x
   trace'a v1 v2
     = logTrace $ field "instance" (view insInstanceId i)
                 . field "attached volumes"
                   (T.concat . intersperse "," $ v1)
                 . field "snapshot volumes"
                   (T.concat . intersperse "," $ map (view vVolumeId) v2)
   trace'b sns = do
     lvl <- gets (Log.logLevel . Log.settings)
     when (lvl >= Log.Trace ) $
       forM_ (Map.keys sns) $ \key ->
         forM_ (Map.findWithDefault [] key sns) $ \x ->
          logTrace $ field "instance" (view insInstanceId i)
                    . field "volume" key
                    . field "snapshot"
                      (T.format "{}/{}"
                       (view sSnapshotId x, view sStartTime x))
   trace'c ss = do
     lvl <- gets (Log.logLevel . Log.settings)
     when (lvl >= Log.Trace ) $ do
       when (null ss) $
         logTrace $ field "instance" (view insInstanceId i)
                  . msg ("no selected snapshots for deletion" :: Text)
       forM_ ss $ \s ->
         logTrace $ field "instance" (view insInstanceId i)
                  . field "selected snapshot" (view sSnapshotId s)
   debug'd s
     = Log.debug $ field "instance" (view insInstanceId i)
         . field "deleting snapshot" (view sSnapshotId s)
   warn'a sn err
     = Log.warn $ field "instance" (view insInstanceId i)
                . field "deleting snapshot" (view sSnapshotId sn)
                . field "report error" err

-- get attached volumes to an instance
getAttachedVolumeIds :: (MonadAWS m) => Text -> m [Text]
getAttachedVolumeIds instId = do
  dbdm <- fmap (view drsBlockDeviceMappings)
             . send $ describeInstanceAttribute instId IANBlockDeviceMapping
  let f =  catMaybes . map (view eibdVolumeId)
                     . catMaybes . map (view ibdmEBS)
  return $ f dbdm

-- extract volume data, removing any marked as "disabled"
filterVolumes :: (MonadAWS m) => [Text] ->  m [Volume]
filterVolumes volNames = do
  vs <- fmap (view dvvrsVolumes)
        . send . set desVolumeIds volNames $ describeVolumes
  return [v | v <- vs, notDisabled v]
  where
    notDisabled v
      = case extractTagValue disablevol'tag (view vTags v)  of
          Nothing -> True
          Just  y -> T.toLower y /= "disabled"

-- get the snapshots for volume
getSnapshots :: (MonadAWS m) => [Volume] -> m (Map Text [Snapshot])
getSnapshots vols = do
  sg <- fmap (view dssrsSnapshots)
        . send  . set dssFilters fs $ describeSnapshots
  return $ Map.fromList [ (view sVolumeId $ head v,v)
                        | v <- (by'vol . by'age) sg  ]
  where
   fs = [ set fValues (map (view vVolumeId) vols)  $ filter' "volume-id"
        , set fValues ["True"] $ filter' ("tag:" `mappend` autoclean'tag)
        , set fValues ["completed"] $ filter' "status"
        ]
   by'vol = groupBy (fmap (== EQ) . comparing (view sVolumeId))
            . sortBy (comparing (view sVolumeId))
   by'age = reverse . sortBy (comparing (view sStartTime))

-- trim to those snapshots that are eligible for deletion
trimSnapshots :: (MonadAWS m) => Schedule -> Map Text [Snapshot]
              -> m [Snapshot]
trimSnapshots sched s'map = do
  now <- liftIO getCurrentTime
  return . concat .  Map.elems $
     Map.map (trim'list now (minGenerations sched) (minAge sched)) s'map
  where
    trim'list now n diff ss
      = [ x | x <- drop n ss
        , now `diffUTCTime` view sStartTime x > diff ]

-- delete a given snapshot - return any error string
deleteVolSnapshot :: (MonadAWS m) => Snapshot -> m (Maybe String)
deleteVolSnapshot s = do
    (r :: Either SomeException DeleteSnapshotResponse)
       <- try . send $ deleteSnapshot (view sSnapshotId s)
    return $! either (Just . show) (const Nothing) r

instanceNeedsSnapshot :: Instance -> Maybe UTCTime
                      -> EnvM (Maybe (Maybe UTCTime))
instanceNeedsSnapshot i last'snap
  |  view isName (view (insState) i) == ISNRunning
    = do Log.info $ field "instance" (view insInstanceId i)
                        . msg ("instance is running - proceeding" :: Text)
         return $ Just Nothing
  | isNothing last'snap
    = do Log.info $ field "instance" (view insInstanceId i)
                   . msg ("instance not running but no last snapshot - proceeding" :: Text)
         return $ Just $ Just launch'time
  | isNothing lss'tag
    = do Log.info $ field "instance" (view insInstanceId i)
                   . msg ("instance is not running and no last snapshot tag - proceeding" :: Text)
         return $ Just $ Just launch'time
  | has'been'run'recently
    = do Log.info $ field "instance" (view insInstanceId i)
                   . msg ("instance is not running but needs new snapshot - proceeding" :: Text)
         return $ Just $ Just launch'time
  | otherwise
    = do Log.info $ field "instance" (view insInstanceId i)
                  . msg ("instance is not running but snapshot valid - skipping" :: Text)
         return Nothing
  where
   lss'tag = extractTagValueI lastsnapshot'tag i
   launch'time = view insLaunchTime i
   has'been'run'recently
    | length toks /= 2 = True
    | read (T.unpack $ toks!!1) == launch'time = False
    | otherwise = True
   toks = T.splitOn "/" $ fromJust lss'tag


-- take a snapshot of an instance
snapshotInstance :: Instance -> Maybe UTCTime -> [Volume] -> EnvM ()
snapshotInstance i launch't snapshot'set = do
   (snap'time, results) <- snapshotVolumes snapshot'set
   forM_ results $ \(s,r) ->
     case (r,view sState s) of
       (Just e, _) -> do
         Log.warn $ field "instance" (view insInstanceId i)
                  . field "volume"   (view sVolumeId s)
                  . field "snapshot" (view sSnapshotId s)
                  . field "error"    e
       (_, SSError' ) -> do
         Log.warn $ field "instance" (view insInstanceId i)
                  . field "volume"   (view sVolumeId s)
                  . field "snapshot" (view sSnapshotId s)
                  . msg ("unspecified error" :: Text)
       (Nothing, _) -> do
         Log.info $ field "instance" (view insInstanceId i)
                  . field "volume"   (view sVolumeId s)
                  . field "snapshot" (view sSnapshotId s)
                  . msg ("initiated" :: Text)
   let t'value = toStrict $ maybe (format "{}" (Only snap'time))
                           (\x -> format "{}/{}" (snap'time, x)) launch't
   (r :: Either SomeException CreateTagsResponse)
      <- try . send . set cResources [view insInstanceId i]
                    . set cTags      [tag lastsnapshot'tag t'value]
                    $ createTags
   case r of
     Left e -> Log.warn $ field "instance" (view insInstanceId i)
                        . field "error setting tag"   (show e)
     Right _ -> return ()

-- snapshot all the volumes for this instance with same timestamp
snapshotVolumes :: [Volume] -> EnvM (UTCTime, [(Snapshot, Maybe String)])
snapshotVolumes vs = do
  now <- liftIO getCurrentTime
  let now' = formatTime defaultTimeLocale "%Y%m%dT%H%M%S%Q" now
  !rs <- mapM (snapshotSingleVolume now') vs
  return (now, rs)

-- launch a snapshot for a single volume
snapshotSingleVolume :: String -> Volume -> EnvM (Snapshot, Maybe String)
snapshotSingleVolume now vol = do
  let vol'name' = extractTagValue "Name" (view vTags vol)
      vol'name  = fromJust vol'name'
      vol'as    = view vAttachments vol
      vol'dev'  = view volDevice $ head vol'as
      vol'dev   = fromJust vol'dev'
      vol'desc  = T.format "@({}):{} attached to {}" (now, vol'name, vol'dev)
  when (isNothing vol'name') $
    fail . TL.unpack
         $ T.format "snapshotSingleVolume: volume {} lacks required name"
                    (Only $ view vVolumeId vol)
  when (length vol'as /= 1 || isNothing vol'dev') $
    fail . TL.unpack
         $ T.format "snapshotSingleVolume: volume {} ({}) missing/incorrect attachment info"
                    (view vVolumeId vol, vol'name)
  s <- send . set ccDescription (Just $ toStrict vol'desc)
            $ createSnapshot (view vVolumeId vol)
  (r :: Either SomeException CreateTagsResponse)
     <- try . send . set cResources [ view sSnapshotId s]
                   . set cTags      [ tag "Name" vol'name
                                    , tag autoclean'tag "True"]
                   $ createTags
  return (s, either (Just . show) (const Nothing) r)

-- Extract all the managed instances from the region
getManagedInstances :: (MonadAWS m) => m [Instance]
getManagedInstances = do
  paginate describeInstances
   =$= CL.concatMap (view dirsReservations)
   =$= CL.concatMap (view rInstances)
   =$= CL.filter (\x -> schedule'tag `elem` map (view tagKey) (view insTags x))
   $$  CL.consume

{-
getAllInstances :: (MonadAWS m) => m [Text]
getAllInstances = do
  paginate describeInstances
   =$= CL.concatMap (view dirsReservations)
   =$= CL.concatMap (view rInstances)
   =$= CL.map (view insInstanceId)
   $$  CL.consume
-}

randomise :: [a] -> IO [a]
randomise as
  | null as    = return []
  | otherwise  = do ri <- randomRIO (0,length as - 1)
                    let (x,y) = splitAt ri as
                    y' <- randomise $ x ++ tail y
                    return $ head y : y'

-- extract the schedule
extractSchedule :: Instance -> EnvM Schedule
extractSchedule i = do
  let s = extractTagValueI schedule'tag i
  dft <- asks _argsDefaultSchedule
  case s of
    Nothing -> do Log.debug $ msg ("empty schedule - using default" :: Text)
                  return dft
    Just  x -> do Log.warn . msg
                    $ T.format "schedule parsing TBW: ignoring '{}'" (Only x)
                  return dft

-- extracts the first (if any) matching value - which must be non-null
extractTagValue :: Text -> [Tag] -> Maybe Text
extractTagValue k tgs
 = listToMaybe . filter (not . T.null)
      $ [view tagValue t | t <- tgs, k == view tagKey t]

-- common case - tag from an instance
extractTagValueI :: Text -> Instance -> Maybe Text
extractTagValueI k = extractTagValue k . view insTags

logTrace :: (Msg -> Msg) -> EnvM ()
logTrace x = Log.trace x >> logFlush

logFlush :: EnvM ()
logFlush = get >>= Log.flush

deriving instance Data Log.Level

-- default'args :: Args
-- default'args = Args
--   { _argsCredsFP = ""
--                   &= typFile &= name "c" &= name "creds-file" &= explicit
--                   &= help "path to aws credentials file"
--   , _argsRegion  = Ireland &= explicit
--                   &= name "r" &= name "region"
--                   &= typ "AWS-REGION" &= help "AWS region (default Ireland)"
--   , _argsDefaultSchedule
--                 = default'schedule &= ignore
--   , _argsOnlyInstances
--                 = def &= args &= typ "INSTANCE-IDs"
--   , _argsLogLevel
--                 = Log.Info &= name "v" &= explicit &= help "Log level (default Info)"
--   } &= summary "Interrogate an AWS region and snapshot all the marked (in AWS) instances,\nsupply instance-ids to restrict."
--     &= program "awsSnapshotRegion"
