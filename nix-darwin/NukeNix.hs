{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Use this script to reset all/most of the nix and nix-darwin
-- changes. This is just for debugging the deployment script.
-- Run with sudo.

module NukeNix where

import           Control.Monad      (forM_)
import           Data.Text          (Text)
import           Prelude            hiding (FilePath)
import           System.Environment
import           Turtle

nuke :: IO ()
nuke = sh $ do
  sh $ do
    let pat = star dot *> tab *> star dot *> tab *> begins "org.nixos"
    daemon <- inproc "launchctl" ["list"] empty & grep pat & sed pat
    void $ proc "launchctl" ["unload", format ("/Library/LaunchDaemons/"%l%".plist") daemon] empty

  forM_ (under "/etc" ["bashrc", "profile", "zshrc"]) $ \cfg ->
    ifExists (cfg <> ".backup-before-nix") $ \backup ->
      procs "mv" (files [backup, cfg]) empty

  let userConfigs = [ ".nix-channels"
                    , ".nix-defexpr"
                    , ".nix-profile"
                    , ".nixpkgs"
                    , ".config/nixpkgs"
                    ]
  homeDir <- Turtle.home
  let dead = [ "/etc/nix"
             , "/etc/bashrc.backup-before-nix-darwin"
             , "/nix"
             , "/var/lib/buildkite-agent"
             , "/run"
             , "/usr/bin/nix-store"
             ] ++
             under "/Library/LaunchDaemons"
             [ "org.nixos.nix-daemon.plist"
             , "org.nixos.activate-system.plist"
             , "org.nixos.buildkite-agent.plist"
             , "org.nixos.nix-gc.plist"
             ] ++
             under homeDir userConfigs ++
             under "/var/root" userConfigs ++
             under "/root" userConfigs

  void $ proc "rm" ("-rf":files dead) empty

  let deadUsers = [ "buildkite-agent", "datadog" ] ++ map (format ("nixbld" % d)) [1 .. 32]
      deadGroups = [ "buildkite-agent", "datadog", "nixbld"]

  forM_ deadUsers $ \u -> proc "dscl" [".", "-delete", "/Users/" <> u] empty
  forM_ deadGroups $ \g -> proc "dscl" [".", "-delete", "/Groups/" <> g] empty

  echo "Log out and log in again to fix your environment"

ifExists :: MonadIO m => FilePath -> (FilePath -> m a) -> m ()
ifExists f a = testpath f >>= \case
  True -> void $ a f
  False -> pure ()

files :: [FilePath] -> [Text]
files = map (format fp)

under :: FilePath -> [FilePath] -> [FilePath]
under base fs = [base </> f | f <- fs]
