{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-implicit-prelude -Wno-missing-local-signatures #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpdateLogic
  ( realFindInstallers
  , CIResult(..)
  , uploadHashedInstaller
  , uploadSignature
  , updateVersionJson
  , githubWikiRecord
  , printInstallersResults
  , InstallersResults(..)
  , GlobalResults(..)
  , parseStatusContext
  , StatusContext(..)
  , bucketRegion
  , runAWS'
  ) where

import           Prelude                      hiding (FilePath)
import           Appveyor                     (AppveyorArtifact (AppveyorArtifact),
                                               build, fetchAppveyorArtifacts,
                                               fetchAppveyorBuild,
                                               buildNumber, unBuildNumber,
                                               getArtifactUrl, jobId, unJobId,
                                               parseCiUrl)
import qualified Appveyor
import           Buildkite.API                (APIToken(APIToken)
                                              , Artifact
                                              , artifactFilename, artifactSha1sum
                                              , listArtifactsForBuild)
import qualified Buildkite.API                as BK
import           Control.Applicative          ((<|>))
import           Control.Exception            (try)
import           Control.Lens                 (to, (^.))
import qualified Control.Lens                 as Lens
import           Control.Monad                (guard, when, (<=<))
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson                   (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.HashMap.Strict          as HashMap
import           Data.List                    (nub, find)
import           Data.Maybe                   (fromMaybe, isJust)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import           Data.Text                    (Text)
import qualified Data.Text.IO                 as T
import qualified Filesystem.Path.CurrentOS    as FP
import           GHC.Generics                 (Generic)
import           Github                       (Status, context,
                                               fetchGithubStatus, statuses,
                                               targetUrl, Rev)
import           Network.AWS                  (Credentials (Discover), newEnv,
                                               send, toBody, within, Region,
                                               AWS, runAWS,
                                               chunkedFile, defaultChunkSize)
import           Network.AWS.S3.PutObject     (poACL, putObject)
import           Network.AWS.S3.GetBucketLocation (getBucketLocation, gblbrsLocationConstraint)

import           Network.AWS.S3.Types         (BucketName (BucketName), constraintRegion,
                                               ObjectCannedACL (OPublicRead),
                                               ObjectKey (ObjectKey))
import           Network.AWS.S3.PutObject
import           Network.AWS.S3.CopyObject
import           Network.URI                  (uriPath, parseURI)
import           Safe                         (headMay, lastMay, readMay)
import           System.Console.ANSI          (Color (Green),
                                               ColorIntensity (Dull),
                                               ConsoleLayer (Foreground),
                                               SGR (Reset, SetColor), setSGR)
import           System.IO.Error              (ioeGetErrorString)
import           Turtle                       (void, MonadIO, printf, format, fp, d, s, w, (%), die, makeFormat)
import           Turtle.Prelude               (mktempdir, proc)
import           Filesystem.Path              (FilePath, (</>), filename)

import           InstallerVersions
import           Types                        (ApplicationVersion, ApplicationVersionKey,
                                               Arch (Linux64, Mac64, Win64), formatArch)
import           Utils                        (fetchCachedUrl, fetchCachedUrlWithSHA1, s3Link)

data CIResult = CIResult
  { ciResultSystem      :: CISystem
  , ciResultLocalPath   :: FilePath
  , ciResultUrl         :: Text
  , ciResultDownloadUrl :: Text
  , ciResultBuildNumber :: Int
  , ciResultArch        :: Arch
  , ciResultSHA1Sum     :: Maybe Text
  } deriving (Show, Generic)

data CISystem = Buildkite | AppVeyor deriving (Show, Generic)

data InstallersResults = InstallersResults
  { ciResults    :: [CIResult]
  , globalResult :: GlobalResults
  } deriving (Show, Generic)

instance FromJSON InstallersResults
instance FromJSON CIResult
instance FromJSON CISystem
instance ToJSON InstallersResults
instance ToJSON CIResult
instance ToJSON CISystem

-- | Allow selection of which CI artifacts to download.
type InstallerPredicate = CIResult -> Bool

-- | Read the Buildkite token from a config file. This file is not
-- checked into git, so the user needs to create it themself.
-- If the file isn't present, the program exits.
loadBuildkiteToken :: IO APIToken
loadBuildkiteToken = try (T.readFile buildkiteTokenFile) >>= \case
  Right contents -> case process contents of
    Just token -> pure $ APIToken token
    Nothing -> die $ format (st%" was empty.\n"%s) buildkiteTokenFile advice
  Left (e :: IOError) -> die $ format ("Could not read "%st%": "%st%s)
    buildkiteTokenFile (ioeGetErrorString e) advice
  where
    process = headMay . filter (not . T.null) . T.lines
    advice = "Obtain an API access token with read_builds scope from\n" <>
             "https://buildkite.com/user/api-access-tokens\n" <>
             "Exiting!" :: Text
    st = makeFormat T.pack

buildkiteTokenFile = "static/buildkite_token" :: String

realFindInstallers :: ApplicationVersionKey -> InstallerPredicate -> Rev -> Maybe FilePath -> IO InstallersResults
realFindInstallers keys instP daedalusRev destDir = do
  buildkiteToken <- liftIO $ loadBuildkiteToken
  globalResult <- liftIO $ findVersionInfo keys daedalusRev
  st <- liftIO $ statuses <$> fetchGithubStatus "input-output-hk" "daedalus" daedalusRev
  results <- mapM (handleCIResults instP destDir <=< findInstallersFromStatus buildkiteToken destDir) st
  pure $ InstallersResults (concat results) globalResult

handleCIResults :: InstallerPredicate -> Maybe FilePath -> Either Text [CIResult] -> IO [CIResult]
handleCIResults instP destDir (Right rs) = do
  let rs' = filter instP rs
  when (isJust destDir) $ fetchCIResults rs'
  pure rs'
handleCIResults _ _ (Left msg) = T.putStrLn msg >> pure []

buildkiteOrg     = "input-output-hk" :: Text
pipelineDaedalus = "daedalus"        :: Text

bkArtifactInstallerArch :: Artifact -> Maybe Arch
bkArtifactInstallerArch art | T.isSuffixOf ".pkg" fn = Just Mac64
                            | T.isSuffixOf ".bin" fn = Just Linux64
                            | otherwise = Nothing
  where fn = artifactFilename art

findInstallersBuildKite :: APIToken -> Maybe FilePath -> Int -> T.Text
                        -> IO (Either Text [CIResult])
findInstallersBuildKite apiToken destDir buildNum buildUrl = do
  let buildDesc = format ("Buildkite build #"%d) buildNum
  arts <- listArtifactsForBuild apiToken buildkiteOrg pipelineDaedalus buildNum
  let arts' = [ (art, arch) | (art, Just arch) <- [ (art, bkArtifactInstallerArch art) | art <- arts ] ]
  forInstallers buildDesc (const True) arts' $ \(art, arch) -> do
    -- ask Buildkite what the download URL is
    url <- BK.getArtifactURL apiToken buildkiteOrg pipelineDaedalus buildNum art
    let out = fromMaybe "." destDir </> FP.fromText (artifactFilename art)
    pure $ CIResult Buildkite out buildUrl url buildNum arch (Just $ artifactSha1sum art)

avArtifactIsInstaller :: AppveyorArtifact -> Bool
avArtifactIsInstaller (AppveyorArtifact _ name) = name == "Daedalus Win64 Installer"

findInstallersAppVeyor :: Maybe FilePath -> Text
                       -> Appveyor.Username -> Appveyor.Project -> ApplicationVersion
                       -> IO (Either Text [CIResult])
findInstallersAppVeyor destDir url user project version = do
  appveyorBuild <- fetchAppveyorBuild user project version
  let jobid = appveyorBuild ^. build . Appveyor.jobs . to head . jobId
      jobDesc = format ("AppVeyor job "%s) (unJobId jobid)
  artifacts <- fetchAppveyorArtifacts jobid
  forInstallers jobDesc avArtifactIsInstaller artifacts $ \(AppveyorArtifact art _) ->
    pure CIResult
      { ciResultSystem      = AppVeyor
      , ciResultLocalPath   = fromMaybe "." destDir </> filename (FP.fromText art)
      , ciResultUrl         = url
      , ciResultDownloadUrl = getArtifactUrl jobid art
      , ciResultBuildNumber = appveyorBuild ^. build . buildNumber . to unBuildNumber
      , ciResultArch        = Win64
      , ciResultSHA1Sum     = Nothing
      }

-- | Download artifacts into the nix store.
fetchCIResults :: [CIResult] -> IO ()
fetchCIResults = mapM_ fetchResult
  where
    fetchResult CIResult{..} = fetchCached ciResultDownloadUrl (filename ciResultLocalPath) ciResultLocalPath
      where fetchCached = case ciResultSHA1Sum of
                            Just sha1 -> fetchCachedUrlWithSHA1 sha1
                            Nothing -> fetchCachedUrl

forInstallers :: Text -> (a -> Bool) -> [a] -> (a -> IO CIResult) -> IO (Either Text [CIResult])
forInstallers job p arts action = case filter p arts of
  [] -> pure $ Left $ if null arts
    then "No artifacts for " <> job
    else "Installer package file not found in artifacts of " <> job
  files -> Right <$> mapM showResult files
    where showResult a = do
            b <- action a
            printCIResult b
            pure b

printCIResult :: CIResult -> IO ()
printCIResult CIResult{..} = do
  printf (w%" "%w%" URL: ") ciResultSystem ciResultArch
  setSGR [ SetColor Foreground Dull Green ]
  T.putStrLn ciResultUrl
  setSGR [ Reset ]

  printf (w%" "%w%" Installer: ") ciResultSystem ciResultArch
  setSGR [ SetColor Foreground Dull Green ]
  T.putStrLn ciResultDownloadUrl
  setSGR [ Reset ]

formatCIResults :: [CIResult] -> Text
formatCIResults rs = T.unlines $ ["CI links:"] ++ ciLinks ++ [""] ++ instLinks InstallerMainnet ++ [""] ++ instLinks InstallerStaging
  where
    ciLinks = nub $ map (("* " <>) . ciResultUrl) rs
    instLinks net = (format (w%" installers:") net:[ fmt res | res <- rs, isNet net res ])
    isNet net = (== Just net) . installerNetwork . ciResultLocalPath
    fmt res = format (s%" - "%s) (formatArch $ ciResultArch res) (ciResultDownloadUrl res)

formatVersionInfo :: GlobalResults -> Text
formatVersionInfo GlobalResults{..} = T.unlines
  [ format ("Daedalus version:   "%s) grDaedalusVersion
  , format ("Daedalus rev:       "%s) grDaedalusCommit
  , ""
  , format ("Cardano SL version: "%s) grCardanoVersion
  , format ("Cardano SL rev:     "%s) grCardanoCommit
  , ""
  , format ("applicationVersion: "%d) grApplicationVersion
  ]

printInstallersResults :: InstallersResults -> IO ()
printInstallersResults InstallersResults{..} = T.putStr $ T.unlines
  [rule, formatVersionInfo globalResult, "", formatCIResults ciResults, rule]
  where rule = "============================================================" :: Text

data StatusContext = StatusContextAppveyor Appveyor.Username Appveyor.Project ApplicationVersion
                   | StatusContextBuildkite Text Int
                   deriving (Show, Eq)

parseStatusContext :: Status -> Maybe StatusContext
parseStatusContext status = parseAppveyor <|> parseBuildKite
  where
    parseAppveyor = guard isAppveyor >> pure (StatusContextAppveyor user project version)
      where (user, project, version) = parseCiUrl $ targetUrl status
    parseBuildKite, parseAppveyor :: Maybe StatusContext
    parseBuildKite = guard isBuildkite >> do
      let parts = T.splitOn "/" (context status)
      repo <- headMay . tail $ parts
      uri <- parseURI . T.unpack . targetUrl $ status
      lastPart <- lastMay . T.splitOn "/" . T.pack . uriPath $ uri
      buildNum <- readMay . T.unpack $ lastPart
      pure $ StatusContextBuildkite repo buildNum

    isAppveyor = context status == "continuous-integration/appveyor/branch"
    isBuildkite = "buildkite/" `T.isPrefixOf` context status

findInstallersFromStatus :: BK.APIToken -> Maybe FilePath -> Status -> IO (Either Text [CIResult])
findInstallersFromStatus buildkiteToken destDir status =
  case parseStatusContext status of
    Just (StatusContextBuildkite _repo buildNum) ->
      findInstallersBuildKite buildkiteToken destDir buildNum (targetUrl status)
    Just (StatusContextAppveyor user project version) ->
      findInstallersAppVeyor destDir (targetUrl status) user project version
    Nothing -> pure $ Left $ "unrecognized CI status: " <> context status

githubWikiRecord :: InstallersResults -> Text
githubWikiRecord InstallersResults{..} = T.intercalate " | " cols <> "\n"
  where
    cols = [ format w $ grApplicationVersion globalResult
           , ""
           , githubLink grDaedalusCommit "daedalus"
           , githubLink grCardanoCommit "cardano-sl"
           , ciLink Mac64
           , ciLink Win64
           , "DATE" ]

    githubLink rev project = githubLink' (rev globalResult) project
    githubLink' rev project = mdLink (T.take 6 rev) (format ("https://github.com/input-output-hk/"%s%"/commit/"%s) project rev)

    ciLink arch = maybe "*missing*" ciLink' $ find ((== arch) . ciResultArch) ciResults
    ciLink' CIResult{..} = mdLink (format d ciResultBuildNumber) ciResultUrl

    mdLink = format ("["%s%"]("%s%")")

updateVersionJson :: Text -> LBS.ByteString -> IO Text
updateVersionJson bucket json = runAWS' . withinBucketRegion bucketName $ \region -> do
  uploadFile json key
  pure $ s3Link region bucketName key
  where
    key = ObjectKey "daedalus-latest-version.json"
    bucketName = BucketName bucket
    uploadFile :: LBS.ByteString -> ObjectKey -> AWS ()
    uploadFile body remoteKey = void . send $
      makePublic $ putObject bucketName remoteKey (toBody body)
    makePublic = Lens.set poACL (Just OPublicRead)

bucketRegion :: BucketName -> AWS Region
bucketRegion = fmap getRegion . send . getBucketLocation
  where getRegion lc = constraintRegion (lc ^. gblbrsLocationConstraint)

runAWS' :: MonadIO io => AWS a -> io a
runAWS' action = liftIO $ do
  env <- newEnv Discover
  runResourceT . runAWS env $ action

withinBucketRegion :: BucketName -> (Region -> AWS a) -> AWS a
withinBucketRegion bucketName action = do
  region <- bucketRegion bucketName
  within region $ action region

type AWSMeta = HashMap.HashMap Text Text

uploadHashedInstaller :: Text -> FilePath -> GlobalResults -> Text -> AWS Text
uploadHashedInstaller bucketName localPath GlobalResults{..} hash =
  withinBucketRegion bucketName' $ \region -> do
    uploadOneFile bucketName' localPath (ObjectKey hash) meta
    copyObject' hashedPath key
    pure $ s3Link region bucketName' key

  where
    key = simpleKey localPath
    bucketName' = BucketName bucketName
    hashedPath = bucketName <> "/" <> hash

    meta = HashMap.fromList
      [ ("daedalus-revision", grDaedalusCommit)
      , ("cardano-revision", grCardanoCommit)
      , ("application-version", (T.pack. show) grApplicationVersion)
      ] :: AWSMeta

    copyObject' :: Text -> ObjectKey -> AWS ()
    copyObject' source dest = void . send $ Lens.set coACL (Just OPublicRead) $ copyObject bucketName' source dest

uploadSignature :: Text -> FilePath -> AWS ()
uploadSignature bucket localPath = withinBucketRegion bucketName . const $
  uploadOneFile bucketName localPath (simpleKey localPath) mempty
  where bucketName = BucketName bucket

-- | S3 object key is just the base name of the filepath.
simpleKey :: FilePath -> ObjectKey
simpleKey = ObjectKey . format fp . FP.filename

uploadOneFile :: BucketName -> FilePath -> ObjectKey -> AWSMeta -> AWS ()
uploadOneFile bucket localPath remoteKey meta = do
  bdy <- chunkedFile defaultChunkSize (FP.encodeString localPath)
  void . send $ makePublic $ Lens.set poMetadata meta $ putObject bucket remoteKey bdy
  where
    makePublic = Lens.set poACL (Just OPublicRead)
