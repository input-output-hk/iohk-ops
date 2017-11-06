{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Appveyor where

import qualified Data.Text                 as T
import           GHC.Generics              hiding (from, to)
import           Data.Aeson
import Utils
import           Data.Monoid               ((<>))

type JobId = T.Text

data AppveyorBuild = AppveyorBuild {
    build :: AppveyorBuild2
    } deriving (Show, Generic)

data AppveyorBuild2 = AppveyorBuild2 {
    jobs :: [AppveyorBuild3]
    } deriving (Show, Generic)

data AppveyorBuild3 = AppveyorBuild3 {
    jobId :: JobId
    } deriving (Show, Generic)

data AppveyorArtifact = AppveyorArtifact {
    fileName :: T.Text
    , name   :: T.Text
    } deriving (Show, Generic)

instance FromJSON AppveyorBuild
instance FromJSON AppveyorBuild2
instance FromJSON AppveyorBuild3
instance FromJSON AppveyorArtifact

getArtifactUrl :: JobId -> T.Text -> T.Text
getArtifactUrl jobid filename = "https://ci.appveyor.com/api/buildjobs/" <> jobid <> "/artifacts/" <> filename

fetchAppveyorBuild :: T.Text -> IO AppveyorBuild
fetchAppveyorBuild = fetchJson

fetchAppveyorArtifacts :: JobId -> IO [AppveyorArtifact]
fetchAppveyorArtifacts jobid = do
  fetchJson $ "https://ci.appveyor.com/api/buildjobs/" <> jobid <> "/artifacts"
