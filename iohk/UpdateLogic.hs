{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UpdateLogic (realFindInstallers, CiResult(..)) where

import           Data.Monoid                      ((<>))
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.Text                     as T
import           GHC.Generics              hiding (from, to)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Data.Aeson
import           Control.Monad.Managed
import           Turtle.Prelude
import           Filesystem.Path.CurrentOS hiding (decode)
import           System.FilePath.Posix
import qualified Data.Aeson                    as AE
import Data.Maybe
import GHC.IO.Exception

type BuildId = T.Text
type JobId = Integer
type BuildNumber = Integer
type Repoid = String

data Status = Status {
    description :: T.Text
    , target_url :: T.Text
    , context :: T.Text
    } deriving (Show, Generic)

data CommitStatus = CommitStatus {
    state :: T.Text
    , statuses :: [ Status ]
    , sha :: T.Text
    } deriving (Show, Generic)

data TravisBuild = TravisBuild {
    number :: T.Text
    , matrix :: [ TravisJobInfo ]
    } deriving (Show, Generic)

data TravisJobInfo = TravisJobInfo {
    tjiId :: JobId
    } deriving (Show, Generic)

data TravisInfo2 = TravisInfo2 {
    ti2State :: String
    , ti2Commit :: String
    } deriving (Show, Generic)

data AppveyorBuild = AppveyorBuild {
    build :: AppveyorBuild2
    } deriving (Show, Generic)

data AppveyorBuild2 = AppveyorBuild2 {
    jobs :: [AppveyorBuild3]
    } deriving (Show, Generic)

data AppveyorBuild3 = AppveyorBuild3 {
    jobId :: T.Text
    } deriving (Show, Generic)

data AppveyorArtifact = AppveyorArtifact {
    fileName :: T.Text
    , name :: T.Text
    } deriving (Show, Generic)

data CiResult = TravisResult T.Text | AppveyorResult T.Text | Other deriving (Show)

instance FromJSON CommitStatus
instance FromJSON Status
instance FromJSON TravisBuild
instance FromJSON TravisJobInfo where
    parseJSON = AE.withObject "TravisJobInfo" $ \v -> TravisJobInfo
        <$> v .: "id"
instance FromJSON TravisInfo2 where
    parseJSON = AE.withObject "TravisJobInfo" $ \v -> TravisInfo2
        <$> v .: "state"
        <*> v .: "commit"
instance FromJSON AppveyorBuild
instance FromJSON AppveyorBuild2
instance FromJSON AppveyorBuild3
instance FromJSON AppveyorArtifact

fetchCachedUrl :: T.Text -> T.Text -> T.Text-> IO ()
fetchCachedUrl url name outPath = do
  exitStatus <- proc "nix-build" [ "-E", "with import <nixpkgs> {}; let file = builtins.fetchurl \"" <> url <> "\"; in runCommand \"" <> name <> "\" {} \"ln -sv ${file} $out\"", "-o", outPath ] mempty
  case exitStatus of
    ExitSuccess -> return ()
    ExitFailure _ -> error "error downloading file"

fetchUrl :: RequestHeaders -> String -> IO LBS.ByteString
fetchUrl extraHeaders url = do
  man <- newTlsManager
  reqUrl <- parseRequest url
  let req' = reqUrl { requestHeaders = [ ("User-Agent", "https://github.com/input-output-hk/iohk-ops") ] <> extraHeaders }
  resp <- httpLbs req' man
  return $ responseBody resp

fetchJson' :: FromJSON a => RequestHeaders -> String -> IO a
fetchJson' extraHeaders url = do
  json <- fetchUrl extraHeaders url
  let
    parseReply :: FromJSON a => LBS.ByteString -> Maybe a
    parseReply json = decode json
    maybeObj = parseReply json
  case maybeObj of
    Just v -> return v
    Nothing -> do
      error "unable to parse json"

fetchGithubJson :: FromJSON a => String -> IO a
fetchGithubJson url = do
  githubToken <- LBS.readFile "github_token"
  let authHeader = ("Authorization", LBS.toStrict githubToken)
  fetchJson' [] url

fetchJson :: FromJSON a => String -> IO a
fetchJson url = fetchJson' mempty url

realFindInstallers :: String -> Managed [CiResult]
realFindInstallers daedalus_rev = do
  let
    parseReply :: LBS.ByteString -> Maybe CommitStatus
    parseReply json = decode json
  -- https://developer.github.com/v3/repos/statuses/#get-the-combined-status-for-a-specific-ref
  maybeObj <- liftIO $ fetchGithubJson $ "https://api.github.com/repos/input-output-hk/daedalus/commits/" <> daedalus_rev <> "/status"
  -- https://api.travis-ci.org/repos/input-output-hk/daedalus/builds/285552368
  -- this url returns json, containing the short build#
  case maybeObj of
    Just obj -> do
      tempdir <- mktempdir "/tmp" "iohk-ops"
      results <- liftIO $ mapM (findInstaller $ T.pack $ encodeString tempdir) (statuses obj)
      proc "ls" [ "-ltrha", T.pack $ encodeString tempdir ] mempty
      return $ results

findInstaller :: T.Text -> Status -> IO CiResult
findInstaller tempdir status = do
  let
    fetchAppveyorBuild :: T.Text -> IO (Maybe AppveyorBuild)
    fetchAppveyorBuild url = fetchJson $ T.unpack url
    fetchAppveyorArtifacts :: T.Text -> IO (Maybe [AppveyorArtifact])
    fetchAppveyorArtifacts url = fetchJson $ T.unpack url
  -- TODO check for 404's
  -- TODO check file contents with libmagic
  case (context status) of
    "continuous-integration/travis-ci/push" -> do
      let 
        [part1] = drop 6 (T.splitOn "/" (target_url status))
        buildId = head  $ T.splitOn "?" part1
      --print $ "its travis buildId: " <> buildId
      obj <- fetchTravis buildId
      let
        -- TODO use .env.global.VERSION from daedalus .travis.yml
        filename = "Daedalus-installer-0.6." <> (number obj) <> ".pkg"
        url = "http://s3.eu-central-1.amazonaws.com/daedalus-travis/" <> filename
        outFile = tempdir <> "/" <> filename
      buildLog <- fetchUrl mempty $ "https://api.travis-ci.org/jobs/" <> (show $ tjiId $ head $ drop 1 $ matrix obj) <> "/log"
      let
        cardanoBuildNumber = extractBuildId buildLog
      print $ "cardano build number: " <> (show cardanoBuildNumber)
      cardanoInfo <- fetchTravis2 "input-output-hk/cardano-sl" cardanoBuildNumber
      print $ "cardano commit: " <> (ti2Commit cardanoInfo)
      print $ "travis URL: " <> url

      fetchCachedUrl url filename outFile

      pure $ TravisResult outFile
    "continuous-integration/appveyor/branch" -> do
      let
        url = "https://ci.appveyor.com/api/projects/" <> (T.intercalate "/" $ drop 4 $ T.splitOn "/" (target_url status))
      maybeObj <- fetchAppveyorBuild url
      case maybeObj of
        Just (AppveyorBuild {build = AppveyorBuild2 {jobs = [AppveyorBuild3 {jobId = jobId}]}}) -> do
          --print $ "job id is " <> jobId
          maybeArtifacts <- fetchAppveyorArtifacts $ "https://ci.appveyor.com/api/buildjobs/" <> jobId <> "/artifacts"
          case maybeArtifacts of
            Just artifacts -> do
              case head artifacts of
                AppveyorArtifact filename "Daedalus Win64 Installer" -> do
                  let
                    url = "https://ci.appveyor.com/api/buildjobs/" <> jobId <> "/artifacts/" <> filename
                    basename = head $ drop 1 $ T.splitOn "/" filename
                    outFile = tempdir <> "/" <> basename
                  print $ "appveyor URL: " <>  url
                  fetchCachedUrl url basename outFile
                  pure $ AppveyorResult outFile
    other -> do
      print $ "other CI status found: " <> other
      pure Other

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

fetchTravis2 :: Repoid -> BuildNumber -> IO TravisInfo2
fetchTravis2 repo number = do
  results <- fetchJson $ "https://api.travis-ci.org/builds?number=" <> (show number) <> "&slug=" <> repo
  return $ head results

fetchTravis :: BuildId -> IO TravisBuild
fetchTravis buildId = fetchJson $ T.unpack $ "https://api.travis-ci.org/repos/input-output-hk/daedalus/builds/" <> buildId
