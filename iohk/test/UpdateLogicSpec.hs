{-# LANGUAGE OverloadedStrings #-}

module UpdateLogicSpec (spec, main) where

import Test.Hspec hiding (context)
import qualified Data.ByteString.Lazy as LBS

import UpdateLogic
import Github (CommitStatus(..), Status(..))
import qualified Buildkite.API as BK

main :: IO ()
main = hspec spec

sampleInput :: LBS.ByteString
sampleInput = "junk\nbuild id is 13711'\r\r\njunk"

spec :: Spec
spec = do
  describe "travis log output" $ do
    it "tests travis log output parsing" $ do
      extractBuildId sampleInput `shouldBe` Just 13711

  describe "GitHub commit status parsing" $ do
    it "recognizes a buildkite status entry" $
      let ctx = StatusContextBuildkite "daedalus" 100
      in parseStatusContext sampleBuildkiteStatus `shouldBe` Just ctx

sampleStatus = CommitStatus
  { state = "failure"
  , statuses = [ Status { description = "AppVeyor build failed"
                        , targetUrl = "https://ci.appveyor.com/project/jagajaga/daedalus/build/1.0.4001"
                        , context = "continuous-integration/appveyor/pr"
                        }
               , Status { description = "AppVeyor build failed"
                        , targetUrl = "https://ci.appveyor.com/project/jagajaga/daedalus/build/1.0.4000"
                        , context = "continuous-integration/appveyor/branch"
                        }
               , sampleBuildkiteStatus
               ]
  , sha = "66c7ef5723ef363d6701133654f4c65848821151"
  }

sampleBuildkiteStatus = Status
  { description = "Build #100 passed (6 minutes, 55 seconds)"
  , targetUrl = "https://buildkite.com/input-output-hk/daedalus/builds/100#job-id"
  , context = "buildkite/daedalus"
  }
