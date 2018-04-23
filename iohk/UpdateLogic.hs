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
                                               getArtifactUrl, jobId,
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
import           Control.Monad                (guard, forM, when)
import           Control.Monad.Managed        (Managed, liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson                   (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.HashMap.Strict          as HashMap
import           Data.List                    (nub)
import           Data.Maybe                   (fromMaybe, isJust)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
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
import           System.Exit                  (die)
import           Turtle                       (void, MonadIO, printf, format, fp, d, s, w, (%))
import           Turtle.Prelude               (mktempdir, proc)
import           Filesystem.Path              (FilePath, (</>), filename)

import           InstallerVersions
import           Types                        (ApplicationVersion, ApplicationVersionKey,
                                               Arch (Mac64, Win64), formatArch)
import           Utils                        (fetchCachedUrl, fetchCachedUrlWithSHA1, s3Link)

data CIResult = CIResult
  { ciResultLocalPath   :: FilePath
  , ciResultUrl         :: T.Text
  , ciResultDownloadUrl :: T.Text
  , ciResultBuildNumber :: Int
  } deriving (Show, Generic)

data InstallersResults = InstallersResults
  { ciResults    :: [(Arch, CIResult)]
  , globalResult :: GlobalResults
  } deriving (Show, Generic)

instance FromJSON InstallersResults
instance FromJSON CIResult
instance ToJSON InstallersResults
instance ToJSON CIResult

-- | Allow selection of which CI artifacts to download.
type InstallerPredicate = CIResult -> Bool

-- | Read the Buildkite token from a config file. This file is not
-- checked into git, so the user needs to create it themself.
-- If the file isn't present, the program exits.
loadBuildkiteToken :: IO APIToken
loadBuildkiteToken = try (T.readFile buildkiteTokenFile) >>= \case
  Right contents -> case process contents of
    Just token -> pure $ APIToken token
    Nothing -> die $ buildkiteTokenFile <> " was empty." <> advice
  Left (e :: IOError) -> die $ "Could not read " <> buildkiteTokenFile <> ": " <> ioeGetErrorString e <> advice
  where
    process = headMay . filter (not . T.null) . T.lines
    advice = concat [ "\nObtain an API access token with read_builds scope from\n"
                    , "https://buildkite.com/user/api-access-tokens"
                    , "\nExiting!" ]

buildkiteTokenFile = "static/buildkite_token" :: String

realFindInstallers :: ApplicationVersionKey -> InstallerPredicate -> Rev -> Maybe FilePath -> IO InstallersResults
realFindInstallers keys instP daedalusRev destDir = do
  buildkiteToken <- liftIO $ loadBuildkiteToken
  globalResult <- liftIO $ findVersionInfo keys daedalusRev
  st <- liftIO $ statuses <$> fetchGithubStatus "input-output-hk" "daedalus" daedalusRev
  results <- liftIO $ concat <$> mapM (findInstallersFromStatus buildkiteToken destDir instP) st
  pure $ InstallersResults results globalResult

buildkiteOrg     = "input-output-hk" :: T.Text
pipelineDaedalus = "daedalus"        :: T.Text

bkArtifactIsInstaller :: Artifact -> Bool
bkArtifactIsInstaller = T.isSuffixOf ".pkg" . artifactFilename

findInstallersBuildKite :: APIToken -> Maybe FilePath -> InstallerPredicate -> Int -> T.Text -> IO [(Arch, CIResult)]
findInstallersBuildKite apiToken destDir instP buildNum buildUrl = do
  arts <- listArtifactsForBuild apiToken buildkiteOrg pipelineDaedalus buildNum

  rs <- forInstallers bkArtifactIsInstaller arts $ \art -> do
    -- ask Buildkite what the download URL is
    url <- BK.getArtifactURL apiToken buildkiteOrg pipelineDaedalus buildNum art

    let out = fromMaybe "." destDir </> FP.fromText (artifactFilename art)
        res = CIResult out buildUrl url buildNum
    printCIResult "Buildkite" Mac64 res
    pure (res, artifactSha1sum art)

  forM (filter (instP . fst) rs) $ \(res, sha1) -> do
    when (isJust destDir) $
      -- download artifact into nix store
      let out = ciResultLocalPath res
      in fetchCachedUrlWithSHA1 (ciResultDownloadUrl res) (filename out) out sha1
    pure (Mac64, res)

avArtifactIsInstaller :: AppveyorArtifact -> Bool
avArtifactIsInstaller (AppveyorArtifact _ name) = name == "Daedalus Win64 Installer"

findInstallersAppVeyor :: Maybe FilePath -> InstallerPredicate -> T.Text
                       -> Appveyor.Username -> Appveyor.Project -> ApplicationVersion
                       -> IO [(Arch, CIResult)]
findInstallersAppVeyor destDir instP url user project version = do
  appveyorBuild <- fetchAppveyorBuild user project version
  let jobid = appveyorBuild ^. build . Appveyor.jobs . to head . jobId
  artifacts <- fetchAppveyorArtifacts jobid
  rs <- forInstallers avArtifactIsInstaller artifacts $ \(AppveyorArtifact art _) ->
    pure CIResult
      { ciResultLocalPath   = fromMaybe "." destDir </> filename (FP.fromText art)
      , ciResultUrl         = url
      , ciResultDownloadUrl = getArtifactUrl jobid art
      , ciResultBuildNumber = appveyorBuild ^. build . buildNumber . to unBuildNumber
      }

  forM (filter instP rs) $ \res -> do
    printCIResult "AppVeyor" Win64 res
    when (isJust destDir) $
      let out = ciResultLocalPath res
      in fetchCachedUrl (ciResultDownloadUrl res) (filename out) out
    pure (Win64, res)

forInstallers :: (a -> Bool) -> [a] -> (a -> IO b) -> IO [b]
forInstallers p arts action = case filter p arts of
  [] -> die $ if null arts
    then "No artifacts for job"
    else "Installer package file not found in artifacts"
  files -> mapM action files

printCIResult :: T.Text -> Arch -> CIResult -> IO ()
printCIResult ci arch CIResult{..} = do
  printf (s%" "%w%" URL: ") ci arch
  setSGR [ SetColor Foreground Dull Green ]
  T.putStrLn ciResultUrl
  setSGR [ Reset ]

  printf (s%" "%w%" Installer: ") ci arch
  setSGR [ SetColor Foreground Dull Green ]
  T.putStrLn ciResultDownloadUrl
  setSGR [ Reset ]

formatCIResults :: [(Arch, CIResult)] -> T.Text
formatCIResults rs = T.unlines $ ["CI links:"] ++ ciLinks ++ [""] ++ instLinks InstallerMainnet ++ [""] ++ instLinks InstallerStaging
  where
    ciLinks = nub $ map (("* " <>) . ciResultUrl . snd) rs
    instLinks net = (format (w%" installers:") net:[ fmt arch (ciResultDownloadUrl res)
                                                   | (arch, res) <- rs, isNet net res ])
    isNet net = (== Just net) . installerNetwork . ciResultLocalPath
    fmt arch = format (s%" - "%s) (formatArch arch)

formatVersionInfo :: GlobalResults -> T.Text
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
  where rule = "============================================================" :: T.Text

data StatusContext = StatusContextAppveyor Appveyor.Username Appveyor.Project ApplicationVersion
                   | StatusContextBuildkite T.Text Int
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

findInstallersFromStatus :: BK.APIToken -> Maybe FilePath -> InstallerPredicate -> Status -> IO [(Arch, CIResult)]
findInstallersFromStatus buildkiteToken destDir instP status =
  case parseStatusContext status of
    Just (StatusContextBuildkite _repo buildNum) ->
      findInstallersBuildKite buildkiteToken destDir instP buildNum (targetUrl status)
    Just (StatusContextAppveyor user project version) ->
      findInstallersAppVeyor destDir instP (targetUrl status) user project version
    Nothing -> do
      putStrLn $ "unrecognized CI status: " <> T.unpack (context status)
      pure []

githubWikiRecord :: InstallersResults -> T.Text
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

    ciLink arch = maybe "*missing*" ciLink' $ lookup arch ciResults
    ciLink' CIResult{..} = mdLink (format d ciResultBuildNumber) ciResultUrl

    mdLink = format ("["%s%"]("%s%")")

updateVersionJson :: T.Text -> LBS.ByteString -> IO T.Text
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

type AWSMeta = HashMap.HashMap T.Text T.Text

uploadHashedInstaller :: T.Text -> FilePath -> GlobalResults -> T.Text -> AWS T.Text
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

    copyObject' :: T.Text -> ObjectKey -> AWS ()
    copyObject' source dest = void . send $ Lens.set coACL (Just OPublicRead) $ copyObject bucketName' source dest

uploadSignature :: T.Text -> FilePath -> AWS ()
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
