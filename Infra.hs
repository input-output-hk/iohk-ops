#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle cassava vector safe yaml ])'

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Conc (threadDelay)

import NixOps


c :: NixOpsConfig
c = NixOpsConfig
  { deploymentName = "serokell-infra"
  , deploymentFiles = ["deployments/infrastructure.nix"]
  , nixopsExecutable = "nixops"
  , nixPath = "nixpkgs=$HOME/nixpkgs-hydra"
  }

data Command =
    Deploy
  | Destroy
  | FromScratch
  | CheckStatus
  deriving (Show)

parser :: Parser Command
parser =
      subcommand "deploy" "Deploy the whole cluster" (pure Deploy)
  <|> subcommand "destroy" "Destroy the whole cluster" (pure Destroy)
  <|> subcommand "fromscratch" "Destroy, Delete, Create, Deploy" (pure FromScratch)
  <|> subcommand "checkstatus" "Check if nodes are accessible via ssh and reboot if they timeout" (pure CheckStatus)

main :: IO ()
main = do
  command <- options "Helper CLI around NixOps to run experiments" parser
  case command of
    Deploy -> deploy c
    Destroy -> destroy c
    FromScratch -> fromscratch c
    CheckStatus -> checkstatus c
