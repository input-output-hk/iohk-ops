#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle cassava vector safe yaml ])'
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/f2c4af4e3bd9ecd4b1cf494270eae5fd3ca9e68c.tar.gz

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Yaml (decodeFile)
import Data.Monoid ((<>))
import Filesystem.Path.CurrentOS (encodeString)
import Turtle
import qualified Data.Text as T

import NixOps


data Command
  = Create
  | Deploy
  | Destroy
  | FromScratch
  | CheckStatus
  deriving (Show)

parser :: Parser (Options, Command)
parser =
  (,)
  <$> optionsParser
  <*> (subcommand "create" "Create a new cluster" (pure Create)
   <|> subcommand "deploy" "Deploy the whole cluster" (pure Deploy)
   <|> subcommand "destroy" "Destroy the whole cluster" (pure Destroy)
   <|> subcommand "fromscratch" "Destroy, Delete, Create, Deploy" (pure FromScratch)
   <|> subcommand "checkstatus" "Check if nodes are accessible via ssh and reboot if they timeout" (pure CheckStatus))

main :: IO ()
main = do
  (opts@Options{..}, command) <- options "Helper CLI around NixOps to run experiments" parser
  Just c <- decodeFile $ encodeString oConfigFile
  case command of
    Create      -> create      opts c
    Deploy      -> deploy      opts c
    Destroy     -> destroy     opts c
    FromScratch -> fromscratch opts c
    CheckStatus -> checkstatus c
