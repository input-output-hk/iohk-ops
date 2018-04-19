{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-implicit-prelude -Wno-semigroup #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Github where

import           Data.Aeson           (FromJSON, parseJSON, withObject, (.:))
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           GHC.Stack            (HasCallStack)
import           System.Directory     (doesFileExist)
import           Utils                (fetchJson')
import           Network.URI          (URI, parseAbsoluteURI, uriPath)

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
    , targetUrl  :: T.Text
    , context    :: T.Text
    } deriving (Show, Generic)

instance FromJSON Status where
  parseJSON = withObject "Status" $ \v -> Status
    <$> v .: "description"
    <*> v .: "target_url"
    <*> v .: "context"

data GitHubSource = GitHubSource
  { srcOwner :: Org
  , srcRepo  :: Repo
  , srcRev   :: Rev
  } deriving (Show, Eq)

instance FromJSON GitHubSource where
  parseJSON = withObject "fetchGit source" $ \o -> do
    (owner, repo) <- gitHubURLParser =<< uriParser =<< o .: "url"
    GitHubSource owner repo <$> o .: "rev"

uriParser :: T.Text -> Parser URI
uriParser u = case parseAbsoluteURI (T.unpack u) of
                Just uri -> pure uri
                Nothing -> fail "Could not parse absolute URI"

-- | Gets owner/repo from github.com URL
gitHubURLParser :: URI -> Parser (Org, Repo)
gitHubURLParser uri | null repo = fail "Missing repo in GitHub URL path"
                    | otherwise = pure (T.pack owner, T.pack (drop 1 repo))
  where (owner, repo) = break (== '/') (drop 1 $ uriPath uri)

fetchGithubJson :: (HasCallStack, FromJSON a) => T.Text -> IO a
fetchGithubJson url = do
  let
    filename :: String
    filename = "github_token"
  exists <- doesFileExist filename
  headers <- if exists then do
    githubToken <- LBS.readFile filename
    return [ ("Authorization", LBS.toStrict githubToken) ]
  else pure []
  fetchJson' headers url

  -- https://developer.github.com/v3/repos/statuses/#get-the-combined-status-for-a-specific-ref
fetchGithubStatus :: HasCallStack => Org -> Repo -> Rev -> IO CommitStatus
fetchGithubStatus org repo rev = fetchGithubJson $ T.intercalate "/" [ "https://api.github.com/repos", org, repo, "commits", rev, "status" ]
