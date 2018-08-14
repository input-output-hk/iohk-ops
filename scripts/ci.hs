#!/usr/bin/env nix-shell
#!nix-shell ../shell.nix -A ci --run "runhaskell -iiohk scripts/ci.hs"

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import           Turtle
import           Prelude                       hiding (FilePath)
import           Control.Monad                    (forM_)
import           Control.Monad.Managed

import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP

data Settings = Settings
  { sIohkOps          :: FilePath
   , sCleanup         :: Bool
   , sStaging         :: Bool
   , sProduction      :: Bool
   , sInfra           :: Bool
   , sExplorer        :: Bool
   , sReportServer    :: Bool
  } deriving Show
settingsParser :: Parser Settings
settingsParser =
    Settings <$> (optPath "iohk-ops" 'o' "Path to `iohk-ops` binary" <|> pure FP.empty)
             <*> switch  "cleanup-configs" 'c' "Clean up configs"
             <*> switch  "with-staging" 's' "Build Staging"
             <*> switch  "with-prod" 'p' "Build Production"
             <*> switch  "with-infra" 'i' "Build Infra"
             <*> switch  "with-explorer" 'e' "Enable Explorer"
             <*> switch  "with-report-server" 'r' "Enable Report Server"

main = do
  settings@Settings{..} <- addDefault =<< options "iohk-ops CI tool" settingsParser
  with setTempHome $ \home -> do
      Just home <- need "HOME"
      generateFakeSecrets
      generateEmptyKeys
      print settings
      when sStaging $ view $ iohkOpsNewCreateDeploy settings "test-staging" "staging" "cardano"
      when sInfra $ view $ iohkOpsNewCreateDeploy settings "test-infra" "production" "infra"

addDefault :: Settings -> IO Settings
addDefault s | sIohkOps s == FP.empty = do
                 defaultIohkOps <- getIohkOpsPath
                 pure s { sIohkOps = defaultIohkOps </> "bin/iohk-ops" }
             | otherwise = pure s

getIohkOpsPath :: IO FilePath
getIohkOpsPath = singleFilePath (inproc "nix-build" [ "-A", "iohk-ops" ] empty)

iohkOpsNewCreateDeploy :: Settings -> Text -> Text -> Text -> Shell ()
iohkOpsNewCreateDeploy settings@Settings{..} name env deployType = do
  let newArgs = case deployType of
                    "cardano" -> iohkOpsNewArgsCardano name env settings
                    "infra"   -> iohkOpsNewArgsInfra name env
      createDeployArgs = iohkOpsCreateDeployArgs name env

  echo $ fromString $ show newArgs
  echo $ fromString $ show createDeployArgs
  inproc (format fp sIohkOps) newArgs empty
  inproc (format fp sIohkOps) createDeployArgs empty
  when sCleanup $ cleanupBuild name

-- | this is helpful when dealing with nix-build output
singleFilePath :: MonadIO io => Shell Line -> io FilePath
singleFilePath = single . fmap lineToFilePath

iohkOpsNewArgsCardano :: Text -> Text -> Settings -> [ Text ]
iohkOpsNewArgsCardano name env settings = [ "new", "--config", name <> ".yaml", "--environment", env ] <> iohkOpsNewOptions <> [ name ] <> cardanoComponents settings

iohkOpsNewArgsInfra :: Text -> Text -> [ Text ]
iohkOpsNewArgsInfra name env = [ "new", "--config", name <> ".yaml", "--environment", env ] <> iohkOpsNewOptions <> [ name, "Infra" ] 

iohkOpsCreateDeployArgs :: Text -> Text -> [ Text ]
iohkOpsCreateDeployArgs name env = iohkOpsCreateDeployOptions <> [ "--config", name <> ".yaml", "create", "deploy", "--dry-run" ]

lineToFilePath :: Line -> FilePath
lineToFilePath = FP.fromText . lineToText

iohkOpsCreateDeployOptions :: [ Text ]
iohkOpsCreateDeployOptions = [ "--verbose", "--deployer", "0.0.0.0" ]

iohkOpsNewOptions :: [ Text ]
iohkOpsNewOptions = [ "--topology", "topology-min.yaml" ]

cardanoComponents :: Settings -> [ Text ] 
cardanoComponents s = [ "Nodes" ] <> explorer <> reportServer
    where
        explorer = [ "Explorer" | sExplorer s ]
        reportServer = [ "ReportServer" | sReportServer s ]

generateEmptyKeys :: IO ()
generateEmptyKeys = do 
    mktree "keys"
    mapM_ touch filenames
        where 
            range :: [ Int ]
            range = [ 0..9 ]
            filenames = map f range
            f = FP.fromText . format ("keys/key"%d%".sk")

generateFakeSecrets :: IO ()
generateFakeSecrets = do
    let secrets = [ "static/github_token"
                  , "static/id_buildfarm"
                  , "static/datadog-api.secret"
                  , "static/datadog-application.secret"
                  , "static/zendesk-token.secret" ]
    mktree "static"
    forM_ secrets touch
    echo "Ensured secrets exist"

setTempHome :: (MonadManaged io, MonadIO io) => io ()
setTempHome = do
    home <- mktempdir "/tmp" "iohk-ops."
    export "HOME" (format fp home)

cleanupBuild :: Text -> Shell ()
cleanupBuild name = rm $ FP.fromText $ name <> ".yaml"
