{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module InstallerVersions
  ( GlobalResults(..)
  , findVersionInfo
  , InstallerNetwork(..)
  , installerNetwork
  ) where


import Prelude hiding (FilePath)

import           Control.Lens hiding (strict)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8   as S8
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict          as HM
import qualified Data.Yaml                    as Y
import qualified Filesystem.Path.CurrentOS    as FP
import           GHC.Generics                 (Generic)
import           Turtle

import           Cardano                      (ConfigurationYaml, applicationVersion, update)
import           Github                       (Rev)
import           Nix                          (nixEvalExpr, nixBuildExpr)
import           Types
import           Utils                        (tt)

data GlobalResults = GlobalResults {
      grCardanoCommit      :: Text
    , grDaedalusCommit     :: Text
    , grApplicationVersion :: Int
    , grCardanoVersion     :: Text
    , grDaedalusVersion    :: Text
  } deriving (Show, Generic)

instance FromJSON GlobalResults
instance ToJSON GlobalResults

findVersionInfo :: ApplicationVersionKey -> Rev -> IO GlobalResults
findVersionInfo keys grDaedalusCommit = do
  grApplicationVersion <- grabAppVersion grDaedalusCommit keys
  printf ("applicationVersion: "%d%"\n") grApplicationVersion
  grCardanoVersion <- fetchCardanoVersionFromDaedalus grDaedalusCommit
  printf ("Cardano version: "%s%"\n") grCardanoVersion
  grDaedalusVersion <- fetchDaedalusVersion grDaedalusCommit
  printf ("Daedalus version: "%s%"\n") grDaedalusVersion
  grCardanoCommit <- fetchCardanoCommitFromDaedalus grDaedalusCommit
  printf ("Cardano commit: "%s%"\n") grCardanoCommit
  pure GlobalResults{..}

-- | Gets package.json version from Daedalus sources.
fetchDaedalusVersion :: Text -> IO Text
fetchDaedalusVersion rev = getVersion <$> fetchDaedalusJSON "package.json" rev
  where
    getVersion v = v ^?! key "version" . _String

-- | Gets the git rev from cardano-sl-src.json in Daedalus
fetchCardanoCommitFromDaedalus :: Rev -> IO Rev
fetchCardanoCommitFromDaedalus rev = getRev <$> fetchDaedalusJSON "cardano-sl-src.json" rev
  where
    getRev v = v ^?! key "rev" . _String

fetchDaedalusJSON :: FilePath -> Text -> IO S8.ByteString
fetchDaedalusJSON json rev = do
  res <- nixEvalExpr (fetchDaedalusNixExpr rev)
  loadFile json (FP.fromText $ (res ^?! _String))
  where
    loadFile fpath storePath = S8.readFile (FP.encodeString $ storePath </> fpath)

-- | Gets version string from daedalus-bridge attribute of Daedalus default.nix
fetchCardanoVersionFromDaedalus :: Text -> IO Text
fetchCardanoVersionFromDaedalus rev = getString <$> nixEvalExpr expr
  where
    getString val = val ^?! _String
    expr = format ("(import "%s%" {}).daedalus-bridge.version") (fetchDaedalusNixExpr rev)

-- | Returns the store path of daedalus-bridge.
fetchDaedalusBridge :: Rev -> IO FilePath
fetchDaedalusBridge rev = nixBuildExpr expr
  where expr = format ("(import "%s%" {}).daedalus-bridge") (fetchDaedalusNixExpr rev)

-- | A nix expression to import a specific revision of Deadalus from git.
fetchDaedalusNixExpr :: Rev -> Text
fetchDaedalusNixExpr = format ("(builtins.fetchTarball "%s%s%".tar.gz)") url
  where url = "https://github.com/input-output-hk/daedalus/archive/" :: Text

-- | Gets version information from the config files in the
-- daedalus-bridge derivation.
grabAppVersion :: Rev     -- ^ git commit id to check out
               -> ApplicationVersionKey -- ^ yaml keys to find
               -> IO Int     -- ^ an integer version
grabAppVersion rev key = do
  bridge <- fetchDaedalusBridge rev
  Just fullConfiguration <- Y.decodeFile (FP.encodeString $ bridge </> "config/configuration.yaml")
  appVersionFromConfig key fullConfiguration

appVersionFromConfig :: ApplicationVersionKey -> ConfigurationYaml -> IO Int
appVersionFromConfig key cfg = case (ver Win64, ver Mac64) of
  (Nothing, _)            -> fail "configuration-key missing"
  (_, Nothing)            -> fail "configuration-key missing"
  (win, mac) | win /= mac -> fail "applicationVersions dont match"
  (Just val, Just _)      -> pure (applicationVersion $ update val)
  where
    ver a = HM.lookup (key a) cfg

----------------------------------------------------------------------------

-- | Cardano cluster which the installer will connect to.
data InstallerNetwork = InstallerMainnet | InstallerStaging | InstallerTestnet deriving (Eq)

instance Show InstallerNetwork where
  show InstallerMainnet = "Mainnet"
  show InstallerStaging = "Staging"
  show InstallerTestnet = "Testnet"

-- | Determine which cardano network an installer is for based on its
-- filename. The inverse of this function is in
-- daedalus/installers/Types.hs.
installerNetwork :: FilePath -> Maybe InstallerNetwork
installerNetwork fpath | "mainnet" `T.isInfixOf` name = Just InstallerMainnet
                       | "staging" `T.isInfixOf` name = Just InstallerStaging
                       | "testnet" `T.isInfixOf` name = Just InstallerTestnet
                       | otherwise = Nothing
  where name = tt (filename fpath)
