{-# LANGUAGE DeriveGeneric              #-}

module Cardano where

import           GHC.Generics    hiding (from, to)
import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text       as T

type ConfigurationYaml = HashMap.HashMap T.Text ConfigurationRoot

data ConfigurationRoot = ConfigurationRoot {
    update :: ConfigurationUpdate
  } deriving (Show, Generic, Eq)

data ConfigurationUpdate = ConfigurationUpdate {
    applicationVersion :: Integer
  } deriving (Show, Generic, Eq)

instance FromJSON ConfigurationRoot
instance FromJSON ConfigurationUpdate
