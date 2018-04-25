#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Prelude hiding (FilePath)
import Turtle
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hFlush)
import System.Environment (getExecutablePath)
import Filesystem.Path.CurrentOS (decodeString)
import Control.Monad (forM_)

-- nix-darwin master branch on 22/04/2018
darwinSource :: Text
darwinSource = "https://github.com/rvl/nix-darwin/archive/80baf761852f4f8ad4b04972aeea50cc4a4eb424.tar.gz"

nixpkgsSource :: Text
nixpkgsSource = nixChannel <> "/nixexprs.tar.xz"

nixChannel :: Text
nixChannel = "https://nixos.org/channels/nixpkgs-18.03-darwin"

iohkOpsSource :: Text
iohkOpsSource = "https://github.com/input-output-hk/iohk-ops/archive/master.tar.gz"

data Deployment = DeployBuildkite | DeployHydra deriving (Show, Eq)

parser :: Parser Deployment
parser = subcommand "buildkite" "Deploy as Buildkite agent" (pure DeployBuildkite)
         <|> subcommand "hydra" "Deploy as Hydra build slave" (pure DeployHydra)

main :: IO ()
main = do
  deployment <- options "Set up nix-darwin with a configuration." parser
  sh $ doSetup deployment

doSetup :: Deployment -> Shell ()
doSetup d = do
  -- install nix-darwin
  checkNix
  setupSSLCert
  setupNixChannel nixChannel
  nixPath <- setupNixPath
  installNixDarwin darwinSource

  -- update nix-darwin configuration
  dataPath <- liftIO getDataPath
  setupConfiguration (Just d) dataPath nixPath

  -- set up private buildkite keys directory and nix-darwin module
  bkDir <- (</> "buildkite") <$> home
  when (d == DeployBuildkite) $ do
    setupBuildkiteDir bkDir
    setupBuildkiteModule dataPath bkDir

  -- switch to new configuration
  darwinRebuild

  -- set permissions on keys directory now that users have been created
  when (d == DeployBuildkite) $
    secureBuildkiteDir bkDir

  -- tell user to fix the buildkite token
  printf "\nSetup is complete.\n"
  printf "Service log file is in /var/lib/buildkite-agent/buildkite-agent.log\n"
  printf "Remember to import an Apple developer signing certificate.\n"
  printf ("Put the token into " % fp % "\n") (bkDir </> "buildkite_token")

-- | If built with nix, datapath is above bin.
-- If run as script, then look in the current working directory.
getDataPath :: IO FilePath
getDataPath = choose . decodeString <$> getExecutablePath
  where choose exe | filename exe == "ghc" = "." -- runhaskell
                   | otherwise = parent (directory exe) -- nix-build

checkNix :: Shell ()
checkNix = sh $ which "nix-build" >>= \case
  Just nb -> procs (tt nb) ["--version"] empty
  Nothing -> do
    -- this is never going to happen duh
    echo "nix-build was not found. Installing nix"
    empty & inproc "curl" ["https://nixos.org/nix/install"] & inproc "sh" [] & void

-- | This is a workaround for nix curl on Darwin.
setupSSLCert :: Shell ()
setupSSLCert = unlessM (testfile cert) $ mapM_ sudo setup
  where
    cert = "/etc/ssl/cert.pem"
    bundle = "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
    setup = [ [ "mkdir", "-p", tt $ directory cert ]
            , [ "ln", "-sf", tt bundle, tt cert ] ]

type NixPath = [(Text, Text)]

setupNixPath :: Shell NixPath
setupNixPath = do
  nixPath <- makeNixPath darwinSource nixpkgsSource <$> home
  let var = strNixPath nixPath
  printf ("export NIX_PATH=" % s % "\n") var
  export "NIX_PATH" var
  pure nixPath

setupNixChannel :: Text -> Shell ()
setupNixChannel nixpkgs = do
  procs "nix-channel" ["--add", nixpkgs, "nixpkgs"] empty
  procs "nix-channel" ["--update"] empty

strNixPath :: NixPath -> Text
strNixPath ps = T.intercalate ":" [k <> "=" <> v | (k, v) <- ps]

makeNixPath :: Text -> Text -> FilePath -> NixPath
makeNixPath darwin nixpkgs homeDir =
  [ ("darwin", darwin)
  , ("nixpkgs", nixpkgs)
  , ("darwin-config", tt $ homeDir </> ".nixpkgs/darwin-configuration.nix")
  , ("iohk-ops", iohkOpsSource) ]

installNixDarwin :: Text -> Shell ()
installNixDarwin src = do
  -- build the installer
  installer <- inproc "nix-build" ["--no-out-link", "-A", "installer", src] empty
  -- prepare configs for nix darwin
  mapM_ moveAway ["/etc/bashrc", "/etc/nix/nix.conf"]
  chopProfile "/etc/profile"
  procs (lineToText installer <> "/bin/darwin-installer") [] (inproc "yes" [] empty)

moveAway :: FilePath -> Shell ()
moveAway cfg = do
  let backup = cfg <.> "backup-before-nix-darwin"
  exists <- testpath cfg
  backupExists <- testpath backup
  when (exists && not backupExists) $ do
    st <- stat cfg
    when (isRegularFile st || isDirectory st) $
      sudo ["mv", tt cfg, tt backup]

-- | Delete everything after the # Nix line
chopProfile :: FilePath -> Shell ()
chopProfile p = do
  contents <- input p & limitWhile (/= "# Nix") & strict
  (temp, h) <- using (mktemp "/tmp" "profile")
  liftIO $ T.hPutStr h contents
  liftIO $ hFlush h
  sudo ["cp", tt temp, tt p]

darwinRebuild :: Shell ()
darwinRebuild = procs "/run/current-system/sw/bin/darwin-rebuild" ["switch"] empty

setupDotNixpkgs :: Shell FilePath
setupDotNixpkgs = do
  dotNixpkgs <- (</> ".nixpkgs") <$> home
  unlessM (testdir dotNixpkgs) $ mkdir dotNixpkgs
  pure dotNixpkgs

setupConfiguration :: Maybe Deployment -> FilePath -> NixPath -> Shell ()
setupConfiguration deploy dataPath nixPath = do
  dotNixpkgs <- setupDotNixpkgs
  let cfg = dotNixpkgs </> "darwin-configuration.nix"
  void $ proc "chmod" ["644", tt cfg] empty -- nix-darwin installer sets this read-only
  input (dataPath </> "darwin-configuration.nix") &
    sedNixPath nixPath &
    sedImports deploy &
    output cfg

moduleName :: Deployment -> FilePath
moduleName DeployBuildkite = "buildkite-agent-module.nix"
moduleName DeployHydra = "hydra-slave-module.nix"

setupBuildkiteModule :: FilePath -> FilePath -> Shell ()
setupBuildkiteModule dataPath keysDir = do
  let nix = moduleName DeployBuildkite
  dotNixpkgs <- setupDotNixpkgs
  input (dataPath </> nix) &
    sedKeys keysDir &
    output (dotNixpkgs </> nix)

-- | Edit the module imports
sedImports :: Maybe Deployment -> Shell Line -> Shell Line
sedImports deploy = sed (text "imports = [" *> star (notChar ']') *> pure ("imports = [ " <> i))
  where i = case deploy of
              Nothing -> ""
              Just d -> "./" <> tt (moduleName d) <> " "

-- | replace "nixpkgs=qwerty" strings
sedNixPath :: NixPath -> Shell Line -> Shell Line
sedNixPath [] = fmap id
sedNixPath ((k, v):ps) = sed (rep k v) . sedNixPath ps
  where
    rep k v = text k *> char '=' *> (star (notChar '"')) *> pure (k <> "=" <> v)

-- | Update the keys variable assignment.
sedKeys :: FilePath -> Shell Line -> Shell Line
sedKeys keys = sed (text "keys = \"" *> star dot *> pure ("keys = \"" <> tt keys <> "\";"))

-- | Create a directory for secret files which shouldn't go in nix store.
setupBuildkiteDir :: FilePath -> Shell ()
setupBuildkiteDir bk = do
  unlessM (testpath bk) (mkdir bk)

  -- generate ssh key
  let keyname = bk </> "id_buildkite"
  unlessM (testfile keyname) $ do
    h <- hostname
    procs "ssh-keygen" ["-f", tt keyname, "-C", "buildkite@" <> h, "-N", ""] empty
    forM_ [keyname, keyname <.> "pub"] $ \f ->
      procs "chmod" ["0644", tt f] empty

  -- placeholder token
  let token = bk </> "buildkite_token"
  unlessM (testfile token) $ do
    printf ("Placeholder token file: " % fp % "\n") token
    liftIO $ writeTextFile token ""

-- | After the buildkite-agent group exists, we can tighten permissions.
secureBuildkiteDir :: FilePath -> Shell ()
secureBuildkiteDir bk = do
  -- this is apple speak for adding admin to the buildkite-agent group
  sudo ["dseditgroup", "-o", "edit", "-a", "admin", "-t", "user", "buildkite-agent"]
  -- procs "chgrp" ["-R", "buildkite-agent", tt bk] empty
  shells ("chgrp -R buildkite-agent " <> tt bk) empty
  procs "chmod" ["-R", "o-rx", tt bk] empty

sudo :: [Text] -> Shell ()
sudo cmd = do
  liftIO . T.putStrLn . T.unwords $ ("sudo":cmd)
  procs "sudo" cmd empty

tt :: FilePath -> Text
tt = format fp

unlessM :: Monad f => f Bool -> f () -> f ()
unlessM f a = f >>= \t -> if t then pure () else a

whenM :: Monad f => f Bool -> f () -> f ()
whenM f a = f >>= \t -> if t then a else pure ()
