{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UpdateLogic (realFindInstallers, CiResult, CiResult(..)) where

import           Data.Monoid                      ((<>))
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import           GHC.Generics              hiding (from, to)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Data.Aeson
import           Control.Monad.Managed
import           Turtle.Prelude
import           Filesystem.Path.CurrentOS hiding (decode)

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

data CiResult = TravisResult T.Text | AppveyorResult T.Text deriving (Show)

instance FromJSON CommitStatus
instance FromJSON Status
instance FromJSON TravisBuild
instance FromJSON AppveyorBuild
instance FromJSON AppveyorBuild2
instance FromJSON AppveyorBuild3
instance FromJSON AppveyorArtifact

fetchUrl :: RequestHeaders -> String -> IO LBS.ByteString
fetchUrl extraHeaders url = do
  man <- newTlsManager
  reqUrl <- parseRequest url
  let req' = reqUrl { requestHeaders = [ ("User-Agent", "https://github.com/input-output-hk/iohk-ops") ] <> extraHeaders }
  resp <- httpLbs req' man
  return $ responseBody resp

fetchJson' :: FromJSON a => RequestHeaders -> String -> IO (Maybe a)
fetchJson' extraHeaders url = do
  json <- fetchUrl extraHeaders url
  let
    parseReply :: FromJSON a => LBS.ByteString -> Maybe a
    parseReply json = decode json
    maybeObj = parseReply json
  return maybeObj

fetchGithubJson :: FromJSON a => String -> IO (Maybe a)
fetchGithubJson url = do
  githubToken <- LBS.readFile "github_token"
  let authHeader = ("Authorization", LBS.toStrict githubToken)
  fetchJson' [authHeader] url

fetchJson :: FromJSON a => String -> IO (Maybe a)
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
      liftIO $ print tempdir
      results <- liftIO $ mapM (findInstaller $ T.pack $ encodeString tempdir) (statuses obj)
      liftIO $ print results
      return $ results

findInstaller :: T.Text -> Status -> IO CiResult
findInstaller tempdir status = do
  print ""
  let
    fetchTravis :: T.Text -> IO (Maybe TravisBuild)
    fetchTravis buildId = fetchJson $ T.unpack $ "https://api.travis-ci.org/repos/input-output-hk/daedalus/builds/" <> buildId
    fetchAppveyorBuild :: T.Text -> IO (Maybe AppveyorBuild)
    fetchAppveyorBuild url = fetchJson $ T.unpack url
    fetchAppveyorArtifacts :: T.Text -> IO (Maybe [AppveyorArtifact])
    fetchAppveyorArtifacts url = fetchJson $ T.unpack url
  case (context status) of
    "continuous-integration/travis-ci/push" -> do
      let 
        [part1] = drop 6 (T.splitOn "/" (target_url status))
        buildId = head  $ T.splitOn "?" part1
      print $ "its travis buildId: " <> buildId
      maybeObj <- fetchTravis buildId
      case maybeObj of
        Just obj -> do
          let
            filename = "Daedalus-installer-0.6." <> (number obj) <> ".pkg"
            url = "http://s3.eu-central-1.amazonaws.com/daedalus-travis/" <> filename
          print $ "travis URL: " <> url
          stream <- fetchUrl mempty (T.unpack url)
          LBS.writeFile (T.unpack $ tempdir <> "/" <> filename) stream
          print "done"
          pure $ TravisResult $ tempdir <> "/" <> filename
    "continuous-integration/appveyor/branch" -> do
      let
        url = "https://ci.appveyor.com/api/projects/" <> (T.intercalate "/" $ drop 4 $ T.splitOn "/" (target_url status))
      maybeObj <- fetchAppveyorBuild url
      case maybeObj of
        Just (AppveyorBuild {build = AppveyorBuild2 {jobs = [AppveyorBuild3 {jobId = jobId}]}}) -> do
          print $ "job id is " <> jobId
          maybeArtifacts <- fetchAppveyorArtifacts $ "https://ci.appveyor.com/api/buildjobs/" <> jobId <> "/artifacts"
          case maybeArtifacts of
            Just artifacts -> do
              print $ head artifacts
              case head artifacts of
                AppveyorArtifact filename "Daedalus Win64 Installer" -> do
                  print filename
                  stream <- fetchUrl mempty $ T.unpack $ "https://ci.appveyor.com/api/buildjobs/" <> jobId <> "/artifacts/" <> filename
                  print $ "appveyor URL: " <>  "https://ci.appveyor.com/api/buildjobs/" <> jobId <> "/artifacts/" <> filename
                  let
                    basename = head $ drop 1 $ T.splitOn "/" filename
                  LBS.writeFile (T.unpack $ tempdir <> "/" <> basename) stream
                  pure $ AppveyorResult $ tempdir <> "/" <> basename
