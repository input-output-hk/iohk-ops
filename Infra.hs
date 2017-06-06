#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle cassava vector safe yaml ])'
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/f2c4af4e3bd9ecd4b1cf494270eae5fd3ca9e68c.tar.gz

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Monoid ((<>))
import qualified Data.Text as T

import NixOps


c :: NixOpsConfig
c = NixOpsConfig
  { deploymentName = "iohk-infra"
  , deploymentFiles =
    [ "deployments/keypairs.nix"
    , "deployments/infrastructure.nix"
    , "deployments/infrastructure-target-aws.nix"
    , "deployments/infrastructure-env-production.nix"]
  , nixopsExecutable = "nixops"
  , nixPath = "nixpkgs=https://github.com/NixOS/nixpkgs/archive/05126bc8503a37bfd2fe80867eb5b0bea287c633.tar.gz"
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
