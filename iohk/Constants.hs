{-# LANGUAGE DeriveGeneric, GADTs, LambdaCase, OverloadedStrings, RecordWildCards, StandaloneDeriving, TupleSections, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults #-}

module Constants where

import           Data.Monoid                      ((<>))
import           Prelude                   hiding (FilePath)
import           Time.Types
import           Turtle                    hiding (env, err, fold, inproc, prefix, procs, e, f, o, x)

import Types
import Utils


awsPublicIPURL       :: URL
awsPublicIPURL       = "http://169.254.169.254/latest/meta-data/public-ipv4"

defaultEnvironment   = Development
defaultTarget        = AWS
defaultNode          = NodeName "c-a-1"
defaultNodePort      = PortNo 3000
defaultNixpkgs       = Nothing

defaultHold          = 1200 :: Seconds -- 20 minutes

explorerNode         = NodeName "explorer"

orgs                 :: [NodeOrg]
orgs                 = enumFromTo minBound maxBound
defaultOrg           = IOHK
accessKeyChain       = [ AccessKeyId $ showT org <> "accessKeyId"
                       | org <- orgs ]

simpleTopoFile       :: FilePath
simpleTopoFile       = "topology.nix"


-- * Project-related constants
--
data Project
  = CardanoSL
  | IOHKOps
  | Nixpkgs
  | Stack2nix
  | Nixops
  deriving (Bounded, Enum, Eq, Read, Show)

projectURL     :: Project -> URL
projectURL     CardanoSL       = "https://github.com/input-output-hk/cardano-sl"
projectURL     IOHKOps         = "https://github.com/input-output-hk/iohk-nixops"
projectURL     Nixpkgs         = "https://github.com/nixos/nixpkgs"
projectURL     Stack2nix       = "https://github.com/input-output-hk/stack2nix"
projectURL     Nixops          = "https://github.com/input-output-hk/nixops"

projectSrcFile :: Project -> FilePath
projectSrcFile CardanoSL       = "cardano-sl-src.json"
projectSrcFile Nixpkgs         = "nixpkgs-src.json"
projectSrcFile Stack2nix       = "stack2nix-src.json"
projectSrcFile IOHKOps         = error "Feeling self-referential?"
projectSrcFile Nixops          = error "No corresponding -src.json spec for 'nixops' yet."
