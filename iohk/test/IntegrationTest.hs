{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main (main, auxxSpec) where

import Prelude hiding (FilePath)
import Turtle
import Test.Hspec
import Control.Monad.Managed

import RunCardano
import NixOps ( Options(..), getCardanoSLConfig )
import Types

main :: IO ()
main = hspec auxxSpec

testOptions :: Options
testOptions = Options { oChdir = Nothing
                      , oConfigFile = Nothing
                      , oOnlyOn = Nothing
                      , oDeployerIP = Nothing
                      , oConfirm = Unconfirmed
                      , oDebug = NoDebug
                      , oSerial = DontSerialize
                      , oVerbose = Verbose
                      , oNoComponentCheck = NoComponentCheck
                      , oInitialHeapSize = Nothing
                      }

auxxSpec :: Spec
auxxSpec = do
  describe "Cardano Auxx hashing" $ do
    it "Correctly hashes a file" $ with testCommandOptions $ \opts -> do
      hash <- single (cardanoHashInstaller opts "test/Spec.hs")
      hash `shouldBe` "e00b5628ea65d18b34d6d999161d0524224514c27e7f36d5348052cda55ef78c"

-- | Set up command options for testing.
testCommandOptions :: (MonadManaged io, MonadIO io) => io CommandOptions
testCommandOptions = do
  cslPath <- testConfig
  tmpDir <- mktempdir "/tmp" "update-proposal-spec-"
  pure $ commandOptions tmpDir cslPath "mainnet_full" "test_update_bucket"

testConfig :: MonadIO io => io FilePath
testConfig = configFromEnv >>= \case
    Just cp -> pure cp
    Nothing -> do
      echo "CARDANO_SL_CONFIG not set -- trying nix-build"
      buildConfig

-- | Test harness nix derivation should set CARDANO_SL_CONFIG variable.
configFromEnv :: MonadIO io => io (Maybe FilePath)
configFromEnv = fmap fromText <$> need "CARDANO_SL_CONFIG"

-- | Build config with nix-build. Won't work within another nix-build.
buildConfig :: MonadIO io => io FilePath
buildConfig = do
  cd ".."
  loadTools "."
  cslPath <- liftIO $ getCardanoSLConfig testOptions
  cd "iohk"
  pure cslPath
