{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-implicit-prelude -Wno-missing-local-signatures #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UpdateLogic
  ( realFindInstallers
  , CiResult(..)
  , ciResultLocalPath
  , ciResultVersion
  , ciResultUrl
  , hashInstaller
  , uploadHashedInstaller
  , uploadSignature
  , updateVersionJson
  , githubWikiRecord
  , InstallersResults(..)
  , GlobalResults(..)
  , parseStatusContext
  , StatusContext(..)
  , resultLocalPath
  , resultDesc
  , bucketRegion
  , runAWS'
  ) where

import           Prelude                      hiding (FilePath)
import           Appveyor                     (AppveyorArtifact (AppveyorArtifact),
                                               build, fetchAppveyorArtifacts,
                                               fetchAppveyorBuild,
                                               getArtifactUrl, jobId,
                                               parseCiUrl)
import qualified Appveyor
import           Buildkite.API                (APIToken(APIToken)
                                              , Artifact
                                              , artifactFilename, artifactSha1sum
                                              , listArtifactsForBuild)
import qualified Buildkite.API                as BK
import qualified Buildkite.Pipeline           as BK
import           Cardano                      (ConfigurationYaml,
                                               applicationVersion, update)
import           Control.Applicative          ((<|>), liftA2)
import           Control.Exception            (Exception, catch, throwIO, try)
import           Control.Lens                 (to, (^.))
import qualified Control.Lens                 as Lens
import           Control.Monad                (guard)
import           Control.Monad.Managed        (Managed, liftIO, with)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson                   (ToJSON, encode, decode)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BSC
import qualified Data.ByteString.Lazy         as LBS
import           Data.Coerce                  (coerce)
import           Data.Git                     (Blob (Blob),
                                               Commit (commitTreeish), EntName,
                                               ModePerm, Ref,
                                               Tree (treeGetEnts),
                                               blobGetContent, entName,
                                               getCommit, getTree)
import           Data.Git.Ref                 (SHA1, fromHexString)
import           Data.Git.Repository          ()
import           Data.Git.Storage             (Git, getObject, isRepo, openRepo)
import           Data.Git.Storage.Object      (Object (ObjBlob))
import qualified Data.HashMap.Strict          as HashMap
import           Data.List                    (find)
import           Data.Maybe                   (fromJust, fromMaybe, listToMaybe, catMaybes)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Yaml                    as Y
import qualified Filesystem.Path.CurrentOS    as FP
import           GHC.Generics                 (Generic)
import           GHC.Stack                    (HasCallStack)
import           Github                       (Status, context,
                                               fetchGithubStatus, statuses,
                                               targetUrl, GitHubSource(srcRev), Rev)
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
import qualified Network.AWS.Data             as AWS
import           Network.URI                  (uriPath, parseURI)
import           Safe                         (headMay, lastMay, readMay)
import           System.Console.ANSI          (Color (Green, Red),
                                               ColorIntensity (Dull),
                                               ConsoleLayer (Background, Foreground),
                                               SGR (Reset, SetColor), setSGR)
import           System.IO.Error              (ioeGetErrorString)
import           System.Exit                  (ExitCode (ExitFailure, ExitSuccess), die)
import           Text.Regex.PCRE              ((=~))
import           Turtle                       (FilePath, empty, void, MonadIO, format, fp)
import           Turtle.Prelude               (mktempdir, proc, procStrict,
                                               pushd)
import           Types                        (ApplicationVersion (ApplicationVersion),
                                               ApplicationVersionKey (ApplicationVersionKey),
                                               Arch (Linux64, Mac64, Win64),
                                               getApplicationVersion)
import           Utils                        (fetchCachedUrl, fetchCachedUrlWithSHA1, s3Link)

data CiResult = AppveyorResult
                { avLocalPath :: T.Text
                , avVersion   :: ApplicationVersion 'Win64
                , avUrl       :: T.Text
                }
              | BuildkiteResult
                { bkLocalPath     :: T.Text
                , bkVersion       :: ApplicationVersion 'Mac64
                , bkCardanoCommit :: T.Text
                , bkBuildNumber   :: Int
                , bkUrl           :: T.Text
                }
              deriving (Show, Generic)

ciResultLocalPath :: CiResult -> T.Text
ciResultLocalPath (AppveyorResult p _ _) = p
ciResultLocalPath (BuildkiteResult p _ _ _ _) = p

ciResultVersion :: CiResult -> T.Text
ciResultVersion (AppveyorResult _ v _) = getApplicationVersion v
ciResultVersion (BuildkiteResult _ v _ _ _) = getApplicationVersion v

ciResultUrl :: CiResult -> T.Text
ciResultUrl (AppveyorResult _ _ u) = u
ciResultUrl (BuildkiteResult _ _ _ _ u) = u

data GlobalResults = GlobalResults {
      grCardanoCommit      :: T.Text
    , grDaedalusCommit     :: T.Text
    , grApplicationVersion :: Int
  } deriving (Show, Generic)
data InstallersResults = InstallersResults
  { appveyorResult  :: Maybe CiResult
  , buildkiteResult :: Maybe CiResult
  , globalResult    :: GlobalResults
  } deriving (Show, Generic)
type TextPath = T.Text
type RepoUrl = T.Text

-- | Get VERSION environment variable from pipeline definition.
-- First looks in global env, otherwise finds first build step with a version.
extractVersionFromBuildkite :: BK.PipelineDefinition -> Maybe T.Text
extractVersionFromBuildkite BK.PipelineDefinition{..} = getVer plEnv <|> headMay stepVers
  where
    stepVers = catMaybes [getVer (BK.stepEnv step) | step <- plSteps]
    getVer = HashMap.lookup "VERSION" :: HashMap.HashMap T.Text T.Text -> Maybe T.Text

data GitNotFound = GitFileNotFound T.Text | GitDirNotFound deriving Show

instance Exception GitNotFound

readFileFromGit :: HasCallStack => T.Text -> T.Text -> T.Text -> RepoUrl -> IO (Maybe LBS.ByteString)
readFileFromGit rev path name url = do
  let clonePath = "/tmp/gitcache-" <> name
  putStrLn $ "Cloning or fetching to " <> T.unpack clonePath
  repo <- fetchOrClone clonePath url
  putStrLn $ "Getting " <> T.unpack rev <> ":" <> T.unpack path
  let
    pathParts = T.splitOn "/" path
    lookupTree :: T.Text -> Ref SHA1 -> IO (Maybe (Ref SHA1))
    lookupTree name' dirRef = do
      dir <- getTree repo dirRef
      let
        resultList = filter (travisFiler $ BSC.pack $ T.unpack name') (treeGetEnts dir)
      return $ fmap (\(_, _, x) -> x) $ listToMaybe resultList
    go :: HasCallStack => [T.Text] -> Ref SHA1 -> IO (Ref SHA1)
    go [] _ = error "empty path??"
    go [ file ] ref = lookupTree file ref >>= \refOut -> maybe (throwIO (GitFileNotFound path)) pure refOut
    go ( dir : rest ) ref = lookupTree dir ref >>= maybe (throwIO GitDirNotFound) (go rest)
    travisFiler :: BS.ByteString -> (ModePerm, EntName, Ref SHA1) -> Bool
    travisFiler needle (_, currentFile, _) = currentFile == entName needle
  commit <- getCommit repo (fromHexString $ T.unpack rev)
  travisRef <- go pathParts (commitTreeish commit)
  obj <- getObject repo travisRef True
  case obj of
    Just (ObjBlob (Blob { blobGetContent = content })) -> do
      return $ Just content
    _ -> return Nothing

readDaedalusFile :: HasCallStack => T.Text -> T.Text -> IO (Maybe LBS.ByteString)
readDaedalusFile rev path = catch (readFileFromGit rev path "daedalus" repo) notFound
  where
    repo = "https://github.com/input-output-hk/daedalus" :: T.Text
    notFound :: GitNotFound -> IO (Maybe LBS.ByteString)
    notFound _ = pure Nothing

fetchDaedalusRepoYAML :: Y.FromJSON a => T.Text -> T.Text -> IO (Maybe a)
fetchDaedalusRepoYAML rev path = do
  res <- readDaedalusFile rev path
  case res of
    Just yaml -> case Y.decodeEither (LBS.toStrict yaml) of
      Right a -> return (Just a)
      Left err -> fail $ "unable to parse " <> T.unpack path <> ": " <> err
    Nothing -> return Nothing

fetchDaedalusVersion :: T.Text -> IO T.Text
fetchDaedalusVersion rev = do
  pipeline <- fetchDaedalusRepoYAML rev ".buildkite/pipeline.yml"
  case extractVersionFromBuildkite =<< pipeline of
    Just ver -> pure ver
    Nothing -> fail "unable to find daedalus version in .buildkite/pipeline.yml"

fetchDaedalusCardanoSource :: T.Text -> IO GitHubSource
fetchDaedalusCardanoSource daedalusRev = do
  js <- readDaedalusFile daedalusRev "cardano-sl-src.json"
  case decode =<< js of
    Just ver -> return ver
    Nothing -> fail "unable to parse version info in cardano-sl-src.json"

findCardanoRevisionFromDaedalus :: T.Text -> IO Rev
findCardanoRevisionFromDaedalus = fmap srcRev . fetchDaedalusCardanoSource

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

realFindInstallers :: HasCallStack => T.Text -> (ApplicationVersionKey 'Win64, ApplicationVersionKey 'Mac64) -> Managed InstallersResults
realFindInstallers daedalus_rev keys = do
  buildkiteToken <- liftIO loadBuildkiteToken
  obj <- liftIO $ fetchGithubStatus "input-output-hk" "daedalus" daedalus_rev
  daedalusVersion <- liftIO $ fetchDaedalusVersion daedalus_rev
  tempdir <- mktempdir "/tmp" "iohk-ops"
  results <- liftIO $ mapM (findInstaller buildkiteToken daedalus_rev daedalusVersion (T.pack $ FP.encodeString tempdir) keys) (statuses obj)
  let
    findResult :: (CiResult -> Bool) -> [(Maybe GlobalResults, Maybe CiResult)] -> Maybe CiResult
    findResult p = headMay . filter p . catMaybes . map snd
    findGlobal :: [ (Maybe GlobalResults, Maybe CiResult) ] -> GlobalResults
    findGlobal = head . catMaybes . map fst
  _ <- proc "ls" [ "-ltrha", T.pack $ FP.encodeString tempdir ] mempty
  return $ InstallersResults
    (findResult isAppveyorResult results)
    (findResult isBuildkiteResult results)
    (findGlobal results)

isAppveyorResult, isBuildkiteResult :: CiResult -> Bool
isAppveyorResult  (AppveyorResult _ _ _)      = True
isAppveyorResult  _                           = False
isBuildkiteResult (BuildkiteResult _ _ _ _ _) = True
isBuildkiteResult _                           = False

-- | Path to where build result is downloaded.
resultLocalPath :: CiResult -> T.Text
resultLocalPath (AppveyorResult p _ _) = p
resultLocalPath (BuildkiteResult p _ _ _ _) = p

-- | Text describing what the build result is
resultDesc :: CiResult -> T.Text
resultDesc (AppveyorResult _ _ _) = "windows installer"
resultDesc (BuildkiteResult _ _ _ _ _) = "darwin installer (from Buildkite)"

fetchOrClone :: TextPath -> RepoUrl -> IO (Git SHA1)
fetchOrClone localpath url = do
  res <- isRepo (FP.decodeString $ T.unpack localpath)
  case res of
    True -> do
      fetchRepo localpath url
    False -> do
      exitCode <- proc "git" [ "clone", "--mirror", url, localpath ] mempty
      case exitCode of
        ExitSuccess   -> fetchRepo localpath url
        ExitFailure _ -> error "cant clone repo"

fetchRepo :: T.Text -> T.Text -> IO (Git SHA1)
fetchRepo localpath url = do
  let
    fetcher :: Managed (Git SHA1)
    fetcher = do
      pushd  $ FP.decodeString $ T.unpack localpath
      exitCode <- proc "git" [ "fetch", url ] mempty
      case exitCode of
        ExitSuccess -> liftIO $ openRepo $ FP.decodeString $ T.unpack localpath
        ExitFailure _ -> error "cant fetch repo"
  with fetcher $ \res -> pure res

buildkiteOrg     = "input-output-hk" :: T.Text
pipelineDaedalus = "daedalus"        :: T.Text

artifactIsInstaller :: Artifact -> Bool
artifactIsInstaller = T.isSuffixOf ".pkg" . artifactFilename

processDarwinBuildKite :: APIToken -> T.Text -> T.Text -> T.Text -> Int -> (ApplicationVersionKey 'Win64, ApplicationVersionKey 'Mac64) -> T.Text -> IO (GlobalResults, CiResult)
processDarwinBuildKite apiToken daedalus_rev daedalus_version tempdir buildNum versionKey ciUrl = do
  cardanoRev <- findCardanoRevisionFromDaedalus daedalus_rev
  arts <- listArtifactsForBuild apiToken buildkiteOrg pipelineDaedalus buildNum

  case find artifactIsInstaller arts of
    Just art -> do
      let outFile = tempdir <> "/" <> artifactFilename art

      -- ask Buildkite what the download URL is
      url <- BK.getArtifactURL apiToken buildkiteOrg pipelineDaedalus buildNum art

      printDarwinBuildInfo "buildkite" cardanoRev url

      -- download artifact into nix store
      fetchCachedUrlWithSHA1 url (artifactFilename art) outFile (artifactSha1sum art)

      appVersion <- liftIO $ grabAppVersion cardanoRev versionKey

      let res = BuildkiteResult
            { bkLocalPath = outFile
            , bkVersion = makeDaedalusVersion daedalus_version buildNum
            , bkCardanoCommit = cardanoRev
            , bkBuildNumber = buildNum
            , bkUrl = ciUrl
            }
      pure (GlobalResults cardanoRev daedalus_rev appVersion, res)

    Nothing -> die $ if null arts
      then "No artifacts for job"
      else "Installer package file not found in artifacts"

makeDaedalusVersion :: T.Text -> Int -> ApplicationVersion a
makeDaedalusVersion ver = makeDaedalusVersion' ver . T.pack . show

makeDaedalusVersion' :: T.Text -> T.Text -> ApplicationVersion a
makeDaedalusVersion' ver num = ApplicationVersion $ ver <> "." <> num

printDarwinBuildInfo :: String -> T.Text -> T.Text -> IO ()
printDarwinBuildInfo ci cardanoRev url = do
  putStr "cardano commit: "
  putStr (T.unpack $ cardanoRev)
  setSGR [ Reset ]
  putStr "\n"

  putStr $ ci <> " URL: "
  setSGR [ SetColor Foreground Dull Green ]
  putStrLn $ T.unpack url
  setSGR [ Reset ]

-- | Gets version information from the config files in the cardano-sl git repo.
grabAppVersion :: T.Text     -- ^ git commit id to check out
               -> (ApplicationVersionKey 'Win64, ApplicationVersionKey 'Mac64) -- ^ yaml keys to find
               -> IO Int     -- ^ an integer version, not sure really
grabAppVersion rev (winKey, macosKey) = do
    let
      readPath name = readFileFromGit rev name "cardano" "https://github.com/input-output-hk/cardano-sl"
      readPath1 = readPath "lib/configuration.yaml"
      readPath2 = readPath "node/configuration.yaml"
      fallback :: GitNotFound -> IO (Maybe LBS.ByteString)
      fallback _ = readPath2
    Just content <- catch readPath1 fallback
    let
      fullConfiguration :: ConfigurationYaml
      fullConfiguration = fromJust . Y.decode . LBS.toStrict $ content
      winVersion = HashMap.lookup (coerce winKey) fullConfiguration
      macosVersion = HashMap.lookup (coerce macosKey) fullConfiguration
    -- relies on only parsing out the subset that should match
    if winVersion == macosVersion then
      case winVersion of
        Just val -> do
          T.putStrLn ("applicationVersion is " <> (T.pack $ show $ applicationVersion $ update val))
          return $ applicationVersion $ update val
        Nothing -> error $ "configuration-key missing"
    else
      error $ "applicationVersions dont match"

data StatusContext = StatusContextAppveyor Appveyor.Username Appveyor.Project (ApplicationVersion 'Win64)
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

findInstaller :: HasCallStack => BK.APIToken -> T.Text -> T.Text -> T.Text -> (ApplicationVersionKey 'Win64, ApplicationVersionKey 'Mac64) -> Status -> IO (Maybe GlobalResults, Maybe CiResult)
findInstaller buildkiteToken daedalus_rev daedalus_version tempdir keys status = do
  case parseStatusContext status of
    Just (StatusContextBuildkite _repo buildNum) -> do
      (globalResults, travisResult') <- processDarwinBuildKite buildkiteToken daedalus_rev daedalus_version tempdir buildNum keys (targetUrl status)
      return (Just globalResults, Just travisResult')
    Just (StatusContextAppveyor user project version) -> do
      appveyorBuild <- fetchAppveyorBuild user project version
      let jobid = appveyorBuild ^. build . Appveyor.jobs . to head . jobId
      artifacts <- fetchAppveyorArtifacts jobid
      case headMay artifacts of
        Just (AppveyorArtifact filename "Daedalus Win64 Installer") -> do
          let
            artifactUrl = getArtifactUrl jobid filename
            basename = head $ drop 1 $ T.splitOn "/" filename
            outFile = tempdir <> "/" <> basename
          putStr "appveyor URL: "
          setSGR [ SetColor Foreground Dull Green ]
          putStrLn $ T.unpack artifactUrl
          setSGR [ Reset ]
          fetchCachedUrl artifactUrl basename outFile
          pure (Nothing, Just $ AppveyorResult outFile version (targetUrl status))
        _ -> pure (Nothing, Nothing)
    Nothing -> do
      putStrLn $ "unrecognized CI status: " <> T.unpack (context status)
      pure (Nothing, Nothing)

hashInstaller :: T.Text -> IO T.Text
hashInstaller path = do
  let
    -- TODO DEVOPS-502
    -- once cardano in `cardano-sl-src.json` has been bumped enough, switch this to building on the fly
    -- with `nix-build -A cardano-sl-auxx`
    exepath :: T.Text
    exepath = "/nix/store/hjvv6dxy197c9mjc2gh635am0c0shx5l-cardano-sl-auxx-1.0.3/bin/cardano-hash-installer"
  (exitStatus, res) <- procStrict exepath [ path ] empty
  case exitStatus of
    ExitSuccess -> do
      let cleanHash = fromMaybe res (T.stripSuffix "\n" res)
      return cleanHash
    ExitFailure _ -> error "error running cardano-hash-installer"

githubWikiRecord :: InstallersResults -> T.Text
githubWikiRecord results = join [ T.pack $ show appVersion
                                , ""
                                , githubLink daedalus_rev "daedalus"
                                , githubLink cardano_rev "cardano-sl"
                                , ciLink buildkite
                                , ciLink appvey
                                , "DATE\n" ]
  where
    buildkiteDetails = buildkiteResult results
    appveyorDetails = appveyorResult results
    globalDetails = globalResult results

    appVersion = grApplicationVersion globalDetails
    cardano_rev = grCardanoCommit globalDetails
    daedalus_rev = grDaedalusCommit globalDetails

    appvey = liftA2 (,) (getApplicationVersion . avVersion <$> appveyorDetails) (avUrl <$> appveyorDetails)
    buildkite = liftA2 (,) (T.pack . show . bkBuildNumber <$> buildkiteDetails) (bkUrl <$> buildkiteDetails)

    githubLink rev project = "[" <> (T.take 6 rev) <> "](https://github.com/input-output-hk/" <> project <> "/commit/" <> rev <> ")"

    ciLink :: Maybe (T.Text, T.Text) -> T.Text
    ciLink (Just (num, url)) = "[" <> num <> "](" <> url <> ")"
    ciLink Nothing = "*missing*"

    join = T.intercalate " | "

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
