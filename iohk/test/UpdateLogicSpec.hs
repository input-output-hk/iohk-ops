{-# LANGUAGE OverloadedStrings #-}

module UpdateLogicSpec (spec, main) where

import Test.Hspec hiding (context)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Aeson (decode)

import UpdateLogic
import Github (CommitStatus(..), Status(..), GitHubSource(..))
import qualified Buildkite.API as BK

main :: IO ()
main = hspec spec

sampleCardanoSLSrc = "{ \"url\": \"https://github.com/input-output-hk/cardano-sl\", \"fetchSubmodules\": \"true\", \"rev\": \"1bba2fd0183f575752f4529182205e04074db336\", \"sha256\": \"1qfi307x7nfp42ldb6sjx30mk7b0zfvd4b48pmyd9g465f7sp420\" }" :: LBS.ByteString

spec :: Spec
spec = do
  describe "Cardano SL version spec" $ do
    it "Correctly parses cardano-sl version spec" $ do
      let src = decode sampleCardanoSLSrc
      fmap srcOwner src `shouldBe` Just "input-output-hk"
      fmap srcRepo src `shouldBe` Just "cardano-sl"
      fmap (T.take 7 . srcRev) src `shouldBe` Just "1bba2fd"

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
