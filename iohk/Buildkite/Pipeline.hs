{-# LANGUAGE OverloadedStrings #-}

-- | Module for parsing Buildkite pipeline definitions.
-- Definitions are usually in the file `.buildkite/pipeline.yml` of a git repo.
-- Not all attributes are parsed, because we don't really need them at present.

module Buildkite.Pipeline
  ( PipelineDefinition(..)
  , PipelineStep(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Control.Monad (guard)
import Data.Text (Text)

data PipelineDefinition = PipelineDefinition
  { plEnv   :: HashMap Text Text  -- ^ A map of environment variables to apply to all steps
  , plSteps :: [PipelineStep] -- ^ A list of build pipeline steps
  } deriving (Show, Eq)

data PipelineStep = CommandStep
                    { stepLabel :: Maybe Text
                    , stepCommand :: [Text]
                    , stepEnv    :: HashMap Text Text
                    , stepAgents :: HashMap Text Value
                    }
                  | WaitStep (Maybe Text)
                  | BlockStep Text
                  | TriggerStep Text
  deriving (Show, Eq)

instance FromJSON PipelineDefinition where
  parseJSON = withObject "Pipeline" $ \o ->
    PipelineDefinition <$> o .:? "env" .!= mempty <*> o .: "steps"

instance FromJSON PipelineStep where
  parseJSON v = parseWait v <|> parseBlock v <|> parseTrigger v <|> parseCommand v
    where
      parseCommand = withObject "Command Step" $ \o ->
        CommandStep <$> o .:? "label"
                    <*> (o .: "command" <|> (pure <$> o .: "command"))
                    <*> o .:? "env" .!= mempty
                    <*> o .:? "agents" .!= mempty
      parseWait w = withText "wait" (\t -> (guard $ t == "wait") *> pure (WaitStep Nothing)) w <|>
                    withObject "Wait Step" (\o -> WaitStep . Just <$> o .: "wait") w
      parseBlock = withObject "Block Step" $ \o -> BlockStep <$> o .: "block"
      parseTrigger = withObject "Trigger Step" $ \o -> TriggerStep <$> o .: "trigger"
