{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-implicit-prelude -Wno-semigroup #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Appveyor where

import           Control.Lens.TH (makeFields)
import           Data.Aeson      (FromJSON, parseJSON, withObject, (.:))
import           Data.Monoid     ((<>))
import           Data.String     (IsString)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Utils           (fetchJson)

newtype JobId = JobId T.Text deriving (Show, Monoid)
newtype Version = Version { versionToText :: T.Text } deriving (Show, Monoid, IsString)
newtype BuildNumber = BuildNumber Integer deriving (Show)
newtype Username = Username { usernameToText :: T.Text } deriving (Show, Monoid, IsString)
newtype Project = Project { projectToText :: T.Text } deriving (Show, Monoid, IsString)

data AppveyorBuild = AppveyorBuild {
    _appveyorBuildBuild :: AppveyorBuild2
    } deriving (Show, Generic)

data AppveyorBuild2 = AppveyorBuild2 {
    _appveyorBuild2Jobs         :: [AppveyorBuild3]
    ,_appveyorBuild2BuildNumber :: BuildNumber
    ,_appveyorBuild2Version     :: Version
    } deriving (Show, Generic)

data AppveyorBuild3 = AppveyorBuild3 {
    _appveyorBuild3JobId :: JobId
    } deriving (Show, Generic)

data AppveyorArtifact = AppveyorArtifact {
    fileName :: T.Text
    , name   :: T.Text
    } deriving (Show, Generic)

makeFields ''AppveyorBuild
makeFields ''AppveyorBuild2
makeFields ''AppveyorBuild3
makeFields ''AppveyorArtifact

instance FromJSON AppveyorBuild where
  parseJSON = withObject "AppveyorBuild" $ \v -> AppveyorBuild
    <$> v .: "build"
instance FromJSON AppveyorBuild2 where
  parseJSON = withObject "AppveyorBuild2" $ \v -> AppveyorBuild2
    <$> v .: "jobs"
    <*> (BuildNumber <$> v .: "buildNumber")
    <*> (Version <$> v .: "version")
instance FromJSON AppveyorBuild3 where
  parseJSON = withObject "AppveyorBuild3" $ \v -> AppveyorBuild3
    . JobId <$> v .: "jobId"
instance FromJSON AppveyorArtifact

getArtifactUrl :: JobId -> T.Text -> T.Text
getArtifactUrl (JobId jobid) filename = "https://ci.appveyor.com/api/buildjobs/" <> jobid <> "/artifacts/" <> filename

-- input: "https://ci.appveyor.com/api/projects/jagajaga/daedalus/build/0.6.3356"
fetchAppveyorBuild :: Username -> Project -> Version -> IO AppveyorBuild
fetchAppveyorBuild user project version' = fetchJson $ "https://ci.appveyor.com/api/projects/" <> (T.intercalate "/" [ usernameToText user, projectToText project, "build", versionToText version' ])

fetchAppveyorArtifacts :: JobId -> IO [AppveyorArtifact]
fetchAppveyorArtifacts (JobId jobid) = fetchJson $ "https://ci.appveyor.com/api/buildjobs/" <> jobid <> "/artifacts"

-- input: https://ci.appveyor.com/project/jagajaga/daedalus/build/0.6.3356
-- output:
parseCiUrl :: T.Text -> (Username, Project, Version)
parseCiUrl input = case T.splitOn "/" input of
  ["https:", "", "ci.appveyor.com", "project", username, project, "build", version'] -> (Username username, Project project, Version version')
  other -> error $ show other
