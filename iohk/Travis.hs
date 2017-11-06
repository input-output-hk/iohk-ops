{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Travis where

import           Data.Aeson       (FromJSON, defaultOptions, genericParseJSON,
                                   (.:))
import qualified Data.Aeson       as AE
import           Data.Aeson.Types (fieldLabelModifier)
import           Data.Monoid      ((<>))
import qualified Data.Text        as T
import           GHC.Generics     (Generic)
import           Utils            (fetchJson)

type BuildId = T.Text
type JobId = Integer
type BuildNumber = Integer
type RepoId = T.Text

data TravisBuild = TravisBuild {
    number   :: T.Text
    , matrix :: [ TravisJobInfo ]
    } deriving (Show, Generic)

data TravisJobInfo = TravisJobInfo {
    tjiId :: JobId
    } deriving (Show, Generic)

data TravisInfo2 = TravisInfo2 {
    ti2state    :: String
    , ti2commit :: String
    } deriving (Show, Generic)

instance FromJSON TravisBuild
instance FromJSON TravisJobInfo where
    parseJSON = AE.withObject "TravisJobInfo" $ \v -> TravisJobInfo
        <$> v .: "id"
instance FromJSON TravisInfo2 where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

fetchTravis2 :: RepoId -> BuildNumber -> IO TravisInfo2
fetchTravis2 repo buildNumber = do
  results <- fetchJson $ "https://api.travis-ci.org/builds?number=" <> (T.pack . show $ buildNumber) <> "&slug=" <> repo
  return $ head results

fetchTravis :: BuildId -> IO TravisBuild
fetchTravis buildId = fetchJson $ "https://api.travis-ci.org/repos/input-output-hk/daedalus/builds/" <> buildId
