{-# LANGUAGE OverloadedStrings #-}

module BuildkiteSpec (spec, main) where

import Test.Hspec
import Buildkite.Pipeline
import qualified Data.Yaml as Y
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = hspec spec

samplePipelineYml :: ByteString
samplePipelineYml = "steps:\n  - label: 'daedalus-x86_64-darwin'\n    command: 'scripts/nix-shell.sh --run \"scripts/build-installer-unix.sh $VERSION $CARDANO_SL_BRANCH  --build-id $BUILDKITE_BUILD_NUMBER --pr-id $BUILDKITE_PULL_REQUEST\"'\n    env:\n      VERSION: 0.8.2\n      CARDANO_SL_BRANCH: release/1.0.4\n      NIX_SSL_CERT_FILE: /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt\n    agents:\n      system: x86_64-darwin\n"

samplePipeline :: PipelineDefinition
samplePipeline = PipelineDefinition
  { plEnv = HM.fromList []
  , plSteps = [ CommandStep
                { stepLabel = Just "daedalus-x86_64-darwin"
                , stepCommand = ["scripts/nix-shell.sh --run \"scripts/build-installer-unix.sh $VERSION $CARDANO_SL_BRANCH  --build-id $BUILDKITE_BUILD_NUMBER --pr-id $BUILDKITE_PULL_REQUEST\""]
                , stepEnv = HM.fromList
                  [ ("CARDANO_SL_BRANCH","release/1.0.4")
                  , ("NIX_SSL_CERT_FILE","/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt")
                  , ("VERSION","0.8.2") ]
                , stepAgents = HM.fromList [("system", "x86_64-darwin")]
                }
              ]
  }

spec :: Spec
spec = do
  describe "Pipeline YAML Parsing" $ do
    it "works with a typical pipeline" $ do
      Y.decode samplePipelineYml `shouldBe` Just samplePipeline
