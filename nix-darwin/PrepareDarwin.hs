{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module PrepareDarwin where

import           Control.Monad             (forM_)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Filesystem.Path.CurrentOS (decodeString)
import           Prelude                   hiding (FilePath)
import           System.Environment        (getEnv, getExecutablePath)
import           System.IO                 (hFlush)
import           Turtle

data Deployment = DeployBuildkite | DeployHydra deriving (Show, Eq)

parser :: Parser Deployment
parser = subcommand "buildkite" "Deploy as Buildkite agent" (pure DeployBuildkite)
         <|> subcommand "hydra" "Deploy as Hydra build slave" (pure DeployHydra)


prepare :: IO ()
prepare = do
  deployment <- options "Prepare for nix-darwin." parser
  sh $ do
    installNixDarwin
    doSetup deployment

-- | Install nix-darwin
installNixDarwin :: Shell ()
installNixDarwin = do
  checkNix
  setupSSLCert
  prepareConfigs
  liftIO setupUserBashrc
  createRunDir
  restartDaemon
  sleep 2.0
  cleanupEtc

-- | Update configuration and rebuild nix-darwin
doSetup :: Deployment -> Shell ()
doSetup depl = do
  bkDir <- (</> "buildkite") <$> home

  -- set up private buildkite keys directory and nix-darwin module
  when (depl == DeployBuildkite) $
    setupBuildkiteDir bkDir

  -- tell user to fix the buildkite token
  printf "\nSetup is complete.\n"
  printf "Remember to import an Apple developer signing certificate.\n"

  when (depl == DeployBuildkite) $
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
    echo "nix-build was not found. Installing nix"
    -- Nix fails to install if these backup files exist
    mapM_ restoreFile ["/etc/bashrc", "/etc/zshrc"]
    empty & inproc "curl" ["https://nixos.org/nix/install", "-o", "install-nix"] & stdout
    chmod executable  "install-nix"
    empty & inproc "./install-nix" ["--daemon"] & stdout

-- | This is a workaround for nix curl on Darwin.
setupSSLCert :: Shell ()
setupSSLCert = unlessM (testfile cert) $ mapM_ sudo setup
  where
    cert = "/etc/ssl/cert.pem"
    bundle = "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"
    setup = [ [ "mkdir", "-p", tt $ directory cert ]
            , [ "ln", "-sf", tt bundle, tt cert ] ]

-- | Prepare /etc for use with nix-darwin instead of nix.
prepareConfigs :: Shell ()
prepareConfigs =
  -- prepare configs for nix darwin
  need "USER" >>= \case
    Just user -> do
      mapM_ moveAway ["/etc/nix/nix.conf"]
      let contents = "trusted-users = " <> user
      liftIO $ writeTextFile "./nix.conf" contents
      sudo [ "cp", "./nix.conf", "/etc/nix/nix.conf" ]
      chopProfile "/etc/profile"
    Nothing -> echo "USER not set"

moveAway :: FilePath -> Shell ()
moveAway cfg = do
  let backup = cfg <.> "backup-before-nix-darwin"
  exists <- testpath cfg
  backupExists <- testpath backup
  when (exists && not backupExists) $ do
    st <- stat cfg
    when (isRegularFile st || isDirectory st) $
      sudo ["mv", tt cfg, tt backup]

restoreFile :: FilePath -> Shell ()
restoreFile cfg = do
  let backup = cfg <.> "backup-before-nix"
  exists <- testpath cfg
  backupExists <- testpath backup
  when backupExists $ do
    when exists $
      sudo ["rm", tt cfg]
    sudo ["mv", tt backup, tt cfg]

-- | Delete everything after the # Nix line
chopProfile :: FilePath -> Shell ()
chopProfile p = do
  contents <- input p & limitWhile (/= "# Nix") & strict
  (temp, h) <- using (mktemp "/tmp" "profile")
  liftIO $ T.hPutStr h contents
  liftIO $ hFlush h
  sudo ["cp", tt temp, tt p]

-- | This is needed so that nix-copy-closure to this host will work
setupUserBashrc :: IO ()
setupUserBashrc = do
  homeDir <- getEnv "HOME"
  writeTextFile (fromString homeDir </> ".bashrc") "source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"

-- | nixpkgs things need /run and normally the nix-darwin installer creates it
createRunDir :: Shell ()
createRunDir = unlessM (testpath "/run") $
  sudo ["ln", "-s", "private/var/run", "/run"]

setupDotNixpkgs :: Shell FilePath
setupDotNixpkgs = do
  dotNixpkgs <- (</> ".nixpkgs") <$> home
  unlessM (testdir dotNixpkgs) $ mkdir dotNixpkgs
  pure dotNixpkgs

restartDaemon :: Shell ()
restartDaemon = do
  sudo [ "launchctl", "stop", "org.nixos.nix-daemon" ]
  sudo [ "launchctl", "start", "org.nixos.nix-daemon" ]

cleanupEtc :: Shell ()
cleanupEtc = do
  sudo [ "rm", "-f", "/etc/nix/nix.conf" ]
  sudo [ "rm", "-f", "/etc/bashrc" ]
  sudo [ "rm", "-f", "/etc/zshrc" ]

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
