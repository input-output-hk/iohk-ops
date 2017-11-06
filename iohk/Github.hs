{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Github where

import           Data.Aeson
import qualified Data.Text                 as T
import qualified Data.ByteString.Lazy      as LBS
import System.Directory
import Utils
import           GHC.Generics              hiding (from, to)

type Rev = T.Text
type Org = T.Text
type Repo = T.Text

data CommitStatus = CommitStatus {
    state      :: T.Text
    , statuses :: [ Status ]
    , sha      :: T.Text
    } deriving (Show, Generic)
instance FromJSON CommitStatus

data Status = Status {
    description  :: T.Text
    , target_url :: T.Text
    , context    :: T.Text
    } deriving (Show, Generic)
instance FromJSON Status

fetchGithubJson :: FromJSON a => T.Text -> IO a
fetchGithubJson url = do
  let fn = "github_token"
  exists <- doesFileExist fn
  headers <- if exists then do
    githubToken <- LBS.readFile "github_token"
    return $ [ ("Authorization", LBS.toStrict githubToken) ]
  else pure []
  fetchJson' headers url

  -- https://developer.github.com/v3/repos/statuses/#get-the-combined-status-for-a-specific-ref
fetchGithubStatus :: Rev -> Org -> Repo -> IO CommitStatus
fetchGithubStatus rev org repo = fetchGithubJson $ T.intercalate "/" [ "https://api.github.com/repos", org, repo, "commits", rev, "status" ]
