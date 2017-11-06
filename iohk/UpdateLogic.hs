{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UpdateLogic (realFindInstallers, CiResult(..)) where

import           Control.Monad.Managed (Managed, liftIO, with)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import           Data.Git.Ref (SHA1)
import           Data.Git.Storage (Git, isRepo, openRepo)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FP
import           System.Exit               (ExitCode (ExitFailure, ExitSuccess))
import           Turtle.Prelude (mktempdir, proc, pushd)
import Utils (fetchUrl, fetchCachedUrl)
import Travis (BuildNumber, fetchTravis, TravisBuild(..), TravisJobInfo(..), fetchTravis2, TravisInfo2(..))
import Appveyor
import Github (Status, fetchGithubStatus, CommitStatus(..), Status(..))

data CiResult = TravisResult T.Text | AppveyorResult T.Text | Other deriving (Show)

realFindInstallers :: T.Text -> Managed [CiResult]
realFindInstallers daedalus_rev = do
  obj <- liftIO $ fetchGithubStatus "input-output-hk" "daedalus" daedalus_rev
  -- https://api.travis-ci.org/repos/input-output-hk/daedalus/builds/285552368
  -- this url returns json, containing the short build#
  tempdir <- mktempdir "/tmp" "iohk-ops"
  results <- liftIO $ mapM (findInstaller $ T.pack $ FP.encodeString tempdir) (statuses obj)
  _ <- proc "ls" [ "-ltrha", T.pack $ FP.encodeString tempdir ] mempty
  return $ results

fetchOrClone :: T.Text -> T.Text -> IO (Git SHA1)
fetchOrClone localpath url = do
  res <- isRepo (FP.decodeString $ T.unpack localpath)
  case res of
    True -> do
      fetchRepo localpath url
    False -> do
      exitCode <- proc "git" [ "clone", "--bare", url, localpath ] mempty
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

findInstaller :: T.Text -> Status -> IO CiResult
findInstaller tempdir status = do
  let
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
        version :: T.Text
        version = "1.0"
        filename = "Daedalus-installer-" <> version <> "." <> (number obj) <> ".pkg"
        url = "http://s3.eu-central-1.amazonaws.com/daedalus-travis/" <> filename
        outFile = tempdir <> "/" <> filename
      buildLog <- fetchUrl mempty $ "https://api.travis-ci.org/jobs/" <> (T.pack $ show $ tjiId $ head $ drop 1 $ matrix obj) <> "/log"
      let
        cardanoBuildNumber = extractBuildId buildLog
      print $ "cardano build number: " <> (show cardanoBuildNumber)
      cardanoInfo <- fetchTravis2 "input-output-hk/cardano-sl" cardanoBuildNumber
      print $ "cardano commit: " <> (ti2commit cardanoInfo)
      print $ "travis URL: " <> url

      fetchCachedUrl url filename outFile

      pure $ TravisResult outFile
    "continuous-integration/appveyor/branch" -> do
      let
        url = "https://ci.appveyor.com/api/projects/" <> (T.intercalate "/" $ drop 4 $ T.splitOn "/" (target_url status))
      appveyorBuild <- fetchAppveyorBuild url
      let jobid = (jobId . head . jobs . build) appveyorBuild
      --print $ "job id is " <> jobId
      artifacts <- fetchAppveyorArtifacts jobid
      case head artifacts of
        AppveyorArtifact filename "Daedalus Win64 Installer" -> do
          let
            artifactUrl = getArtifactUrl jobid filename
            basename = head $ drop 1 $ T.splitOn "/" filename
            outFile = tempdir <> "/" <> basename
          print $ "appveyor URL: " <>  artifactUrl
          fetchCachedUrl artifactUrl basename outFile
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

