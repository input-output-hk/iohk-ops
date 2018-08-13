module UpdateProposalSpec (spec) where

import Test.Hspec hiding (context)

import Data.Semigroup ((<>))
import UpdateProposal
import UpdateLogic
import RunCardano (CommandOptions, commandOptions)
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Update proposal command" $ do
    it "Generates the correct command" $ do
      proposeUpdateCmd testCommandOptions testConfig allSystems `shouldBe`
        ("propose-update 99 vote-all:true 1.2.3 ~software~csl-daedalus:42 " <>
         "(upd-bin \"linux\" /workdir/installers/hash-linux) " <>
         "(upd-bin \"macos64\" /workdir/installers/hash-macos) " <>
         "(upd-bin \"win64\" /workdir/installers/hash-win)")
    it "Skips linux update if --with-linux option is disabled" $ do
      proposeUpdateCmd testCommandOptions testConfig notLinux `shouldBe`
        ("propose-update 99 vote-all:true 1.2.3 ~software~csl-daedalus:42 " <>
         "(upd-bin \"macos64\" /workdir/installers/hash-macos) " <>
         "(upd-bin \"win64\" /workdir/installers/hash-win)")

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
      , cfgInstallerHashes = ArchMap "hash-linux" "hash-macos" "hash-win"
      , cfgInstallerSHA256 = ArchMap "sha-linux" "sha-macos" "sha-win"
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
                             , ciResultUrl = "spec-url"
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
allSystems = straightArchMap True

notLinux :: ArchMap Bool
notLinux = mkArchMap False True True
