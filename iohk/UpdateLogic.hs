{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-implicit-prelude #-}
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
  , hashInstaller
  , githubWikiRecord
  , InstallersResults(..)
  , GlobalResults(..)
  , updateVersionJson
  , extractBuildId
  , parseStatusContext
  , StatusContext(..)
  , resultLocalPath
  , resultDesc
  ) where

import           Appveyor                     (AppveyorArtifact (AppveyorArtifact),
                                               build, fetchAppveyorArtifacts,
                                               fetchAppveyorBuild,
                                               getArtifactUrl, jobId, jobs,
                                               parseCiUrl)
import qualified Appveyor
import           Buildkite.API                (APIToken(APIToken), Artifact(..)
                                              , listArtifactsForBuild,
                                               getBuild, getLatestBuildForPipeline)
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
import           Control.Monad.Trans.AWS      (AWST, runAWST)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   (ToJSON, encode)
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
                                               targetUrl)
import           Network.AWS                  (Credentials (Discover), newEnv,
                                               send, toBody, within, Region(Tokyo))
import           Network.AWS.S3.PutObject     (poACL, putObject)
import           Network.AWS.S3.Types         (BucketName (BucketName),
                                               ObjectCannedACL (OPublicRead),
                                               ObjectKey)
import           Network.URI                  (uriPath, parseURI)
import           Safe                         (headMay, lastMay, readMay)
import           System.Console.ANSI          (Color (Green, Red),
                                               ColorIntensity (Dull),
                                               ConsoleLayer (Background, Foreground),
                                               SGR (Reset, SetColor), setSGR)
import           System.IO.Error              (ioeGetErrorString)
import           System.Exit                  (ExitCode (ExitFailure, ExitSuccess), die)
import           Travis                       (BuildId, BuildNumber,
                                               TravisBuild (matrix, number),
                                               TravisInfo2 (ti2commit),
                                               TravisJobInfo (tjiId),
                                               TravisYaml, env, fetchTravis,
                                               fetchTravis2, global, lookup')
import           Turtle                       (empty, void)
import           Turtle.Prelude               (mktempdir, proc, procStrict,
                                               pushd)
import           Types                        (ApplicationVersion (ApplicationVersion),
                                               ApplicationVersionKey (ApplicationVersionKey),
                                               Arch (Linux64, Mac64, Win64))
import           Utils                        (fetchCachedUrl, fetchCachedUrlWithSHA1, fetchUrl)

data CiResult = TravisResult {
      travislocalPath :: T.Text
    , travisVersion   :: ApplicationVersion 'Mac64
    , cardanoCommit   :: T.Text
    , travisJobNumber :: T.Text
    , travisUrl       :: T.Text
  }
  | AppveyorResult {
      avLocalPath :: T.Text
    , avVersion   :: ApplicationVersion 'Win64
    , avUrl       :: T.Text
  }
  | BuildkiteResult {
      bkLocalPath     :: T.Text
    , bkVersion       :: ApplicationVersion 'Mac64
    , bkCardanoCommit :: T.Text
    , bkBuildNumber   :: Int
    , bkUrl           :: T.Text
  }
  deriving (Show)
data GlobalResults = GlobalResults {
      grCardanoCommit      :: T.Text
    , grDaedalusCommit     :: T.Text
    , grApplicationVersion :: Integer
  } deriving Show
data InstallersResults = InstallersResults {
      travisResult   :: Maybe CiResult
    , buildkiteResult :: Maybe CiResult
    , appveyorResult :: Maybe CiResult
    , globalResult   :: GlobalResults
  } deriving Show
type TextPath = T.Text
type RepoUrl = T.Text

data VersionJson = VersionJson {
      _linux :: ApplicationVersion 'Linux64
    , _macos :: ApplicationVersion 'Mac64
    , _win64 :: ApplicationVersion 'Win64
  } deriving (Show, Generic)

instance ToJSON VersionJson

extractVersionFromTravis :: TravisYaml -> Maybe T.Text
extractVersionFromTravis yaml = lookup' "VERSION" (global (env yaml))

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

fetchDaedalusRepoYAML :: Y.FromJSON a => T.Text -> T.Text -> IO (Maybe a)
fetchDaedalusRepoYAML path rev = do
  res <- try $ readFileFromGit rev path "daedalus" "https://github.com/input-output-hk/daedalus"
  case res of
    Right (Just yaml) -> case Y.decodeEither (LBS.toStrict yaml) of
      Right a -> return (Just a)
      Left err -> fail $ "unable to parse " <> T.unpack path <> ": " <> err
    Right Nothing -> return Nothing
    Left (_ :: GitNotFound) -> return Nothing

fetchDaedalusVersion :: T.Text -> IO T.Text
fetchDaedalusVersion rev = do
  let buildkiteVer = (>>= extractVersionFromBuildkite) <$> fetchDaedalusRepoYAML ".buildkite/pipeline.yml" rev
  let travisVer = (>>= extractVersionFromTravis) <$> fetchDaedalusRepoYAML ".travis.yml" rev
  buildkiteVer `orElse` travisVer >>= \case
    Just ver -> return ver
    Nothing -> fail "unable to find daedalus version in either .buildkite/pipeline.yml or .travis.yml"

-- | Try option A and if that doesn't work then try option B.
orElse :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
orElse ma mb = ma >>= maybe mb (pure . Just)

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
    (findResult isTravisResult results) (findResult isBuildkiteResult results)
    (findResult isAppveyorResult results) (findGlobal results)

isTravisResult, isAppveyorResult, isBuildkiteResult :: CiResult -> Bool
isTravisResult    (TravisResult _ _ _ _ _)    = True
isTravisResult    _                           = False
isAppveyorResult  (AppveyorResult _ _ _)      = True
isAppveyorResult  _                           = False
isBuildkiteResult (BuildkiteResult _ _ _ _ _) = True
isBuildkiteResult _                           = False

-- | Path to where build result is downloaded.
resultLocalPath :: CiResult -> T.Text
resultLocalPath (TravisResult p _ _ _ _) = p
resultLocalPath (AppveyorResult p _ _) = p
resultLocalPath (BuildkiteResult p _ _ _ _) = p

-- | Text describing what the build result is
resultDesc :: CiResult -> T.Text
resultDesc (TravisResult _ _ _ _ _) = "darwin installer (from Travis)"
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

processDarwinBuildKite :: APIToken -> T.Text -> T.Text -> T.Text -> Int -> (ApplicationVersionKey 'Win64, ApplicationVersionKey 'Mac64) -> T.Text -> IO (GlobalResults, CiResult)
processDarwinBuildKite apiToken daedalus_rev daedalus_version tempdir buildNum versionKey ciUrl = do
  let org = "input-output-hk" :: T.Text
      targetPipeline = "daedalus" :: T.Text
      cardanoPipeline = "cardano-sl" :: T.Text
      filenameP = T.isSuffixOf ".pkg"

  _targetBuild <- getBuild apiToken org targetPipeline buildNum
  cardanoBuild <- getLatestBuildForPipeline apiToken org cardanoPipeline
  arts <- listArtifactsForBuild apiToken org targetPipeline buildNum

  case (find (filenameP . artifactFilename) arts, cardanoBuild) of
    (Just art, Just cardanoInfo) -> do
      let cardanoRev = BK.buildCommit cardanoInfo
          outFile = tempdir <> "/" <> artifactFilename art

      -- ask Buildkite what the download URL is
      url <- BK.getArtifactURL apiToken org targetPipeline buildNum art

      printDarwinBuildInfo "buildkite" (fromIntegral $ BK.buildNumber cardanoInfo) cardanoRev url

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

    (Nothing, _) -> if null arts
      then error "No artifacts for job"
      else error "Installer package file not found in artifacts"

    (_, Nothing) -> error "Could not get latest build for pipeline"

makeDaedalusVersion :: T.Text -> Int -> ApplicationVersion a
makeDaedalusVersion ver = makeDaedalusVersion' ver . T.pack . show

makeDaedalusVersion' :: T.Text -> T.Text -> ApplicationVersion a
makeDaedalusVersion' ver num = ApplicationVersion $ ver <> "." <> num

processDarwinBuildTravis :: T.Text -> T.Text -> T.Text -> BuildId -> (ApplicationVersionKey 'Win64, ApplicationVersionKey 'Mac64) -> T.Text -> IO (GlobalResults, CiResult)
processDarwinBuildTravis daedalus_rev daedalus_version tempdir buildId versionKey ciUrl = do
  obj <- fetchTravis buildId
  let
    filename = "Daedalus-installer-" <> daedalus_version <> "." <> (number obj) <> ".pkg"
    version :: ApplicationVersion 'Mac64
    version = makeDaedalusVersion' daedalus_version (number obj)
    url = "http://s3.eu-central-1.amazonaws.com/daedalus-travis/" <> filename
    outFile = tempdir <> "/" <> filename
  buildLog <- fetchUrl mempty $ "https://api.travis-ci.org/jobs/" <> (T.pack $ show $ tjiId $ head $ drop 1 $ matrix obj) <> "/log"
  let
    cardanoBuildNumber = extractBuildId buildLog
  cardanoInfo <- fetchTravis2 "input-output-hk/cardano-sl" cardanoBuildNumber

  printDarwinBuildInfo "travis" cardanoBuildNumber (ti2commit cardanoInfo) url

  appVersion <- liftIO $ grabAppVersion (ti2commit cardanoInfo) versionKey
  fetchCachedUrl url filename outFile

  pure (GlobalResults (ti2commit cardanoInfo) daedalus_rev appVersion, TravisResult outFile version (ti2commit cardanoInfo) (number obj) ciUrl)

printDarwinBuildInfo :: String -> Integer -> T.Text -> T.Text -> IO ()
printDarwinBuildInfo ci cardanoBuildNumber cardanoRev url = do
  setSGR [ SetColor Background Dull Red ]
  putStr "cardano build number: "
  putStrLn $ show cardanoBuildNumber

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
               -> IO Integer -- ^ an integer version, not sure really
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

data StatusContext = StatusContextBuildkite T.Text Int
                   | StatusContextTravis BuildId
                   | StatusContextAppveyor Appveyor.Username Appveyor.Project (ApplicationVersion 'Win64)
                   deriving (Show, Eq)

parseStatusContext :: Status -> Maybe StatusContext
parseStatusContext status = parseBuildKite <|> parseTravis <|> parseAppveyor
  where
    parseBuildKite, parseTravis, parseAppveyor :: Maybe StatusContext
    parseBuildKite = guard isBuildkite >> do
      let parts = T.splitOn "/" (context status)
      repo <- headMay . tail $ parts
      uri <- parseURI . T.unpack . targetUrl $ status
      lastPart <- lastMay . T.splitOn "/" . T.pack . uriPath $ uri
      buildNum <- readMay . T.unpack $ lastPart
      pure $ StatusContextBuildkite repo buildNum
    parseTravis = guard isTravis >> do
      part1 <- headMay . drop 6 . T.splitOn "/" . targetUrl $ status
      buildId <- headMay . T.splitOn "?" $ part1
      pure $ StatusContextTravis buildId
    parseAppveyor = guard isAppveyor >> pure (StatusContextAppveyor user project version)
      where (user, project, version) = parseCiUrl $ targetUrl status

    isBuildkite = "buildkite/" `T.isPrefixOf` context status
    isTravis = context status == "continuous-integration/travis-ci/push"
    isAppveyor = context status == "continuous-integration/appveyor/branch"

findInstaller :: HasCallStack => APIToken -> T.Text -> T.Text -> T.Text -> (ApplicationVersionKey 'Win64, ApplicationVersionKey 'Mac64) -> Status -> IO (Maybe GlobalResults, Maybe CiResult)
findInstaller buildkiteToken daedalus_rev daedalus_version tempdir keys status = do
  -- TODO check for 404's
  -- TODO check file contents with libmagic
  case parseStatusContext status of
    Just (StatusContextBuildkite _repo buildNum) -> do
      (globalResults, travisResult') <- processDarwinBuildKite buildkiteToken daedalus_rev daedalus_version tempdir buildNum keys (targetUrl status)
      return (Just globalResults, Just travisResult')
    Just (StatusContextTravis buildId) -> do
      (globalResults, travisResult') <- processDarwinBuildTravis daedalus_rev daedalus_version tempdir buildId keys (targetUrl status)
      return (Just globalResults, Just travisResult')
    Just (StatusContextAppveyor user project version) -> do
      appveyorBuild <- fetchAppveyorBuild user project version
      let jobid = appveyorBuild ^. build . jobs . to head . jobId
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

extractBuildId :: LBS.ByteString -> BuildNumber
extractBuildId fullLog = read $ BSC.unpack f5
  where
    f1 = LBS.toStrict fullLog
    (_, f2) = BS.breakSubstring "build id is" f1
    f3 = BSC.takeWhile (\c -> c /= '\n') f2
    isNumber c = (c >= '0') && (c <= '9')
    f4 = BSC.dropWhile (\c -> (isNumber c) == False) f3
    f5 = BSC.takeWhile isNumber f4

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
                                , githubLink daedalus_rev "daedalus"
                                , githubLink cardano_rev "cardano-sl"
                                , ciLink buildkite
                                , ciLink travis
                                , ciLink appvey
                                , "DATE" ]
  where

    travisDetails = travisResult results
    buildkiteDetails = buildkiteResult results
    appveyorDetails = appveyorResult results
    globalDetails = globalResult results

    appVersion = grApplicationVersion globalDetails
    cardano_rev = grCardanoCommit globalDetails
    daedalus_rev = grDaedalusCommit globalDetails

    travis = liftA2 (,) (travisJobNumber <$> travisDetails) (travisUrl <$> travisDetails)
    appvey = liftA2 (,) (T.pack . show . avVersion <$> appveyorDetails) (avUrl <$> appveyorDetails)
    buildkite = liftA2 (,) (T.pack . show . bkBuildNumber <$> buildkiteDetails) (bkUrl <$> buildkiteDetails)

    githubLink rev project = "[" <> (T.take 6 rev) <> "](https://github.com/input-output-hk/" <> project <> "/commit/" <> rev <> ")"

    ciLink :: Maybe (T.Text, T.Text) -> T.Text
    ciLink (Just (num, url)) = "[" <> num <> "](" <> url <> ")"
    ciLink Nothing = "*missing*"

    join cols = T.concat ["| ", T.intercalate " | " cols, " |"]

updateVersionJson :: InstallersResults -> T.Text -> IO ()
updateVersionJson info bucket = do
  let
    macVersion = (bkVersion <$> buildkiteResult info) <|> (travisVersion <$> travisResult info)
    winVersion = avVersion <$> appveyorResult info
    json = encode $ VersionJson "" (fromMaybe "" macVersion) (fromMaybe "" winVersion)
    uploadOneFile :: BucketName -> LBS.ByteString -> ObjectKey -> AWST (ResourceT IO) ()
    uploadOneFile bucketName body remoteKey = do
      --bdy <- chunkedFile defaultChunkSize body
      let
        bdy = toBody body
      void . send $ Lens.set poACL (Just OPublicRead) $ putObject bucketName remoteKey bdy
  env' <- newEnv Discover
  -- XXX: change the hard-coded 'Tokyo' region to the AWS query of the bucket's region.
  liftIO $ runResourceT . runAWST env' $ within Tokyo $ do
    uploadOneFile (BucketName bucket) json "daedalus-latest-version.json"
