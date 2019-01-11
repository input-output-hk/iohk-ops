{-# LANGUAGE OverloadedStrings #-}

module UpdateProposalSpec (spec) where

import Test.Hspec hiding (context)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Control.Monad.Writer hiding ((<>))
import Options.Applicative (Parser, ParserResult(..), execParserPure, prefs, info)

import UpdateProposal
import UpdateLogic
import RunCardano (CommandOptions, commandOptions)
import Types
import Arch

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Update proposal command" $ do
    it "Generates the correct command" $ do
      proposeUpdateCmd testCommandOptions testConfig allSystems `shouldBe`
        ("propose-update 99 vote-all:true 1.2.3 ~software~csl-daedalus:42 " <>
         "(upd-bin \"linux64\" /workdir/installers/hash-linux) " <>
         "(upd-bin \"macos64\" /workdir/installers/hash-macos) " <>
         "(upd-bin \"win64\" /workdir/installers/hash-win)")
    it "Skips linux update if --with-linux option is disabled" $ do
      proposeUpdateCmd testCommandOptions testConfig notLinux `shouldBe`
        ("propose-update 99 vote-all:true 1.2.3 ~software~csl-daedalus:42 " <>
         "(upd-bin \"macos64\" /workdir/installers/hash-macos) " <>
         "(upd-bin \"win64\" /workdir/installers/hash-win)")

  describe "ArchMap abstraction" $ do
    it "correctly sorts through installers results" $ do
      let action :: WriterT [(Arch, T.Text)] IO (ArchMap ())
          action = forResults testInstallersResults $ \arch r -> tell [(arch, ciResultUrl r)]
          expected = [ (Linux64, "spec-url-Linux64")
                     , (Mac64, "spec-url-Mac64")
                     , (Win64, "spec-url-Win64")] :: [(Arch, T.Text)]
      execWriterT action `shouldReturn` expected

  describe "Command-line option parser for update-proposal submit" $ do
    let parseSubmit args = resultSystems $
          testParser parseUpdateProposalCommand (["submit", "1", "-r", "A"] ++ args)

    it "defaults to all platforms" $ do
      parseSubmit [] `shouldBe` Just (mkArchMap True True True)

    it "allows skipping of linux" $ do
      parseSubmit ["--without-linux"] `shouldBe` Just (mkArchMap False True True)

    it "has backwards-compatible options" $ do
      parseSubmit ["--with-linux"] `shouldBe` Just (mkArchMap True True True)

    it "allows skipping macos and windows" $ do
      parseSubmit ["--without-windows", "--without-macos"] `shouldBe`
        Just (mkArchMap True False False)

    it "does not permit contradictory arguments" $ do
      parseSubmit ["--without-windows", "--with-windows"] `shouldBe`
        Nothing

testCommandOptions :: CommandOptions
testCommandOptions = commandOptions "/workdir" "/cardano-source" "key_spec" "spec-s3"

testConfig :: UpdateProposalConfig4
testConfig = UpdateProposalConfig4
  { cfgUpdateProposal3 = UpdateProposalConfig3
    { cfgUpdateProposal2 = UpdateProposalConfig2
      { cfgUpdateProposal1 = UpdateProposalConfig1
        { cfgDaedalusRevision = undefined
        , cfgLastKnownBlockVersion = "1.2.3"
        , cfgVoterIndex = 99
        }
      , cfgInstallersResults = testInstallersResults
      , cfgInstallerHashes = mkArchMap "hash-linux" "hash-macos" "hash-win"
      , cfgInstallerSHA256 = mkArchMap "sha-linux" "sha-macos" "sha-win"
      }
    }
  , cfgUpdateProposalAddrs = "addrs"
  }

testInstallersResults :: InstallersResults
testInstallersResults = InstallersResults ci gr
  where
    ci = [ciResult Linux64, ciResult Mac64, ciResult Win64]
    ciResult arch = CIResult { ciResultSystem = Buildkite
                             , ciResultLocalPath = "spec-path"
                             , ciResultUrl = "spec-url-" <> T.pack (show arch)
                             , ciResultDownloadUrl = "spec-download-url"
                             , ciResultBuildNumber = 666
                             , ciResultArch = arch
                             , ciResultSHA1Sum = Nothing
                             }
    gr = GlobalResults { grCardanoCommit = "cardano-rev"
                       , grDaedalusCommit = "daedalus-rev"
                       , grApplicationVersion = 42
                       , grCardanoVersion = "cardano-version"
                       , grDaedalusVersion = "daedalus-version"
                       }

allSystems :: ArchMap Bool
allSystems = pure True

notLinux :: ArchMap Bool
notLinux = mkArchMap False True True


-- | Run an options parser against a list of arguments.
testParser :: Parser a -> [String] -> ParserResult a
testParser p = execParserPure (prefs mempty) (info p mempty)

-- | Pluck the systems argument out of the result of option parsing.
resultSystems :: ParserResult UpdateProposalCommand -> Maybe (ArchMap Bool)
resultSystems (Success (UpdateProposalCommand _ (UpdateProposalSubmit _ _ s))) = Just s
resultSystems _ = Nothing
