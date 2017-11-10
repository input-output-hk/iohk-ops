{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-implicit-prelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module UpdateLogic (realFindInstallers, CiResult(..), hashInstaller, githubWikiRecord) where

import           Appveyor                  (AppveyorArtifact (AppveyorArtifact),
                                            build, fetchAppveyorArtifacts,
                                            fetchAppveyorBuild, getArtifactUrl,
                                            jobId, jobs, parseCiUrl, Version, versionToText)
import           Cardano                   (ConfigurationYaml,
                                            applicationVersion, update)
import           Control.Exception         (Exception, catch, throwIO)
import           Control.Lens              (to, (^.))
import           Control.Monad.Managed     (Managed, liftIO, with)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import           Data.Git                  (Blob (Blob), Commit (commitTreeish),
                                            EntName, ModePerm, Ref,
                                            Tree (treeGetEnts), blobGetContent,
                                            entName, getCommit, getTree)
import           Data.Git.Ref              (SHA1, fromHexString)
import           Data.Git.Repository       ()
import           Data.Git.Storage          (Git, getObject, isRepo, openRepo)
import           Data.Git.Storage.Object   (Object (ObjBlob))
import qualified Data.HashMap.Strict       as HashMap
import           Data.Maybe                (fromMaybe, listToMaybe, catMaybes)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Yaml                 as Y
import qualified Filesystem.Path.CurrentOS as FP
import           GHC.Stack                 (HasCallStack)
import           Github                    (Status, context, fetchGithubStatus,
                                            statuses, target_url)
import           System.Console.ANSI       (Color (Green, Red),
                                            ColorIntensity (Dull),
                                            ConsoleLayer (Background, Foreground),
                                            SGR (Reset, SetColor), setSGR)
import           System.Exit               (ExitCode (ExitFailure, ExitSuccess))
import           Travis                    (BuildId, BuildNumber,
                                            TravisBuild (matrix, number),
                                            TravisInfo2 (ti2commit),
                                            TravisJobInfo (tjiId), TravisYaml,
                                            env, fetchTravis, fetchTravis2,
                                            global, lookup', parseTravisYaml)
import           Turtle.Prelude            (mktempdir, proc, pushd, procStrict)
import           Turtle                    (empty)
import           Utils                     (fetchCachedUrl, fetchUrl)
import qualified System.IO                 as Sys

data CiResult = TravisResult T.Text Integer T.Text T.Text T.Text | AppveyorResult T.Text Version T.Text deriving (Show)
type TextPath = T.Text
type RepoUrl = T.Text

extractVersionFromTravis :: LBS.ByteString -> Maybe T.Text
extractVersionFromTravis yaml = do
  let
    f1 = parseTravisYaml yaml
    f2 :: Either String TravisYaml -> TravisYaml
    f2 (Right v)  = v
    f2 (Left err) = error $ "unable to parse .travis.yml: " <> err
    f3 = global $ env (f2 f1)
  lookup' "VERSION" f3

data GitNotFound = GitFileNotFound T.Text | GitDirNotFound deriving Show

instance Exception GitNotFound

readFileFromGit :: HasCallStack => T.Text -> T.Text -> RepoUrl -> IO (Maybe LBS.ByteString)
readFileFromGit rev path url = do
  repo <- fetchOrClone "/tmp/gitcache" url
  let
    pathParts = T.splitOn "/" path
    lookupTree :: T.Text -> Ref SHA1 -> IO (Maybe (Ref SHA1))
    lookupTree name dirRef = do
      dir <- getTree repo dirRef
      let
        resultList = filter (travisFiler $ BSC.pack $ T.unpack name) (treeGetEnts dir)
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

realFindInstallers :: HasCallStack => T.Text -> Managed [CiResult]
realFindInstallers daedalus_rev = do
  daedalus_version <- liftIO $ do
    contentMaybe <- readFileFromGit daedalus_rev ".travis.yml" "https://github.com/input-output-hk/daedalus"
    case contentMaybe of
      Just content -> do
        case extractVersionFromTravis content of
          Just version -> return version
          Nothing -> fail "unable to find daedalus version in .travis.yml"
      _ -> fail ".travis.yml not found"
  obj <- liftIO $ fetchGithubStatus "input-output-hk" "daedalus" daedalus_rev
  -- https://api.travis-ci.org/repos/input-output-hk/daedalus/builds/285552368
  -- this url returns json, containing the short build#
  tempdir <- mktempdir "/tmp" "iohk-ops"
  results <- liftIO $ mapM (findInstaller daedalus_version $ T.pack $ FP.encodeString tempdir) (statuses obj)
  _ <- proc "ls" [ "-ltrha", T.pack $ FP.encodeString tempdir ] mempty
  return $ catMaybes results

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
        ExitSuccess -> do
          liftIO $ putStrLn "fetched"
          repo <- liftIO $ openRepo $ FP.decodeString $ T.unpack localpath
          pure repo
        ExitFailure _ -> error "cant fetch repo"
  with fetcher $ \res -> pure res

processDarwinBuild :: T.Text -> T.Text -> BuildId -> T.Text -> IO CiResult
processDarwinBuild daedalus_version tempdir buildId ciUrl = do
  obj <- fetchTravis buildId
  let
    filename = "Daedalus-installer-" <> daedalus_version <> "." <> (number obj) <> ".pkg"
    url = "http://s3.eu-central-1.amazonaws.com/daedalus-travis/" <> filename
    outFile = tempdir <> "/" <> filename
  buildLog <- fetchUrl mempty $ "https://api.travis-ci.org/jobs/" <> (T.pack $ show $ tjiId $ head $ drop 1 $ matrix obj) <> "/log"
  let
    cardanoBuildNumber = extractBuildId buildLog
  cardanoInfo <- fetchTravis2 "input-output-hk/cardano-sl" cardanoBuildNumber

  setSGR [ SetColor Background Dull Red ]
  putStr "cardano build number: "
  putStrLn $ show cardanoBuildNumber

  putStr "cardano commit: "
  putStr (T.unpack $ ti2commit cardanoInfo)
  setSGR [ Reset ]
  putStr "\n"

  putStr "travis URL: "
  setSGR [ SetColor Foreground Dull Green ]
  putStrLn $ T.unpack url
  setSGR [ Reset ]

  appVersion <- liftIO $ do
    let
      readPath name = readFileFromGit (ti2commit cardanoInfo) name "https://github.com/input-output-hk/cardano-sl"
      readPath1 = readPath "lib/configuration.yaml"
      readPath2 = readPath "node/configuration.yaml"
      fallback :: GitNotFound -> IO (Maybe LBS.ByteString)
      fallback _ = readPath2
    contentMaybe <- catch readPath1 fallback
    let
      -- TODO
      winKey :: T.Text
      winKey = "mainnet_wallet_win64"
      macosKey :: T.Text
      macosKey = "mainnet_wallet_macos64"
    let
      content = fromMaybe undefined contentMaybe
      fullConfiguration :: ConfigurationYaml
      fullConfiguration = fromMaybe undefined (Y.decode $ LBS.toStrict content)
      winVersion = HashMap.lookup winKey fullConfiguration
      macosVersion = HashMap.lookup macosKey fullConfiguration
    -- relies on only parsing out the subset that should match
    if winVersion == macosVersion then
      case winVersion of
        Just val -> do
          T.putStrLn ("applicationVersion is " <> (T.pack $ show $ applicationVersion $ update val))
          return $ applicationVersion $ update val
        Nothing -> error $ "configuration-key missing"
    else
      error $ "applicationVersions dont match"

  fetchCachedUrl url filename outFile

  pure $ TravisResult outFile appVersion (ti2commit cardanoInfo) (number obj) ciUrl

findInstaller :: HasCallStack => T.Text -> T.Text -> Status -> IO (Maybe CiResult)
findInstaller daedalus_version tempdir status = do
  let
  -- TODO check for 404's
  -- TODO check file contents with libmagic
  case (context status) of
    "continuous-integration/travis-ci/push" -> do
      let
        -- TODO, break this out into a parse function like Appveyor.parseCiUrl
        [part1] = drop 6 (T.splitOn "/" (target_url status))
        buildId = head  $ T.splitOn "?" part1
      --print $ "its travis buildId: " <> buildId
      result <- processDarwinBuild daedalus_version tempdir buildId (target_url status)
      return $ Just result
    "continuous-integration/appveyor/branch" -> do
      let
        (user, project, version) = parseCiUrl $ target_url status
      appveyorBuild <- fetchAppveyorBuild user project version
      let jobid = appveyorBuild ^. build . jobs . to head . jobId
      --print $ "job id is " <> jobId
      artifacts <- fetchAppveyorArtifacts jobid
      case head artifacts of
        AppveyorArtifact filename "Daedalus Win64 Installer" -> do
          let
            artifactUrl = getArtifactUrl jobid filename
            basename = head $ drop 1 $ T.splitOn "/" filename
            outFile = tempdir <> "/" <> basename
          putStr "appveyor URL: "
          setSGR [ SetColor Foreground Dull Green ]
          putStrLn $ T.unpack artifactUrl
          setSGR [ Reset ]
          fetchCachedUrl artifactUrl basename outFile
          pure $ Just $ AppveyorResult outFile version (target_url status)
    other -> do
      print $ "other CI status found: " <> other
      pure Nothing

extractBuildId :: LBS.ByteString -> BuildNumber
extractBuildId fullLog = do
  let
    f1 = LBS.toStrict fullLog
    (_, f2) = BS.breakSubstring "build id is" f1
    f3 = BSC.takeWhile (\c -> c /= '\n') f2
    isNumber c = (c >= '0') && (c <= '9')
    f4 = BSC.dropWhile (\c -> (isNumber c) == False) f3
    f5 = BSC.takeWhile isNumber f4
  read $ BSC.unpack f5

sampleInput :: LBS.ByteString
sampleInput = "junk\nbuild id is 13711'\r\r\njunk"

hashInstaller :: Sys.FilePath -> IO T.Text
hashInstaller path = do
  (exitStatus, res) <- procStrict "/nix/store/myla1pqh1hzif9b1k5pv8kj25fqyhjff-cardano-sl-auxx-1.0.2/bin/cardano-hash-installer" [ (T.pack path) ] empty
  case exitStatus of
    ExitSuccess -> do
      let cleanHash = fromMaybe res (T.stripSuffix "\n" res)
      return cleanHash
    ExitFailure _ -> error "error running cardano-hash-installer"

githubWikiRecord :: T.Text -> [CiResult] -> T.Text
githubWikiRecord daedalus_rev results = do
  let
    findTravis :: [CiResult] -> (Integer, T.Text, T.Text, T.Text)
    findTravis (TravisResult _ ver cardano_rev' buildNumber ciUrl : _) = (ver, cardano_rev', buildNumber, ciUrl)
    findTravis (_ : rest) = findTravis rest
    findTravis [] = error "TravisResult missing"
    findAppveyor :: [CiResult] -> (Version, T.Text)
    findAppveyor (AppveyorResult _ number' url' : _) = (number', url')
    findAppveyor (_ : rest) = findAppveyor rest
    findAppveyor [] = error "AppveyorResult missing"
    (appVersion, cardano_rev, travisNumber, travisUrl) = findTravis results
    (appveyVersion, appveyUrl) = findAppveyor results
    githubLink rev project = "[" <> (T.take 6 rev) <> "](https://github.com/input-output-hk/" <> project <> "/commit/" <> rev <> ")"
  (T.pack $ show appVersion) <> " | " <> (githubLink daedalus_rev "daedalus") <> " | " <> (githubLink cardano_rev "cardano-sl") <> " | [" <> travisNumber <> "](" <> travisUrl <> ") | [" <> (versionToText appveyVersion) <> "](" <> appveyUrl <> ") | DATE"
