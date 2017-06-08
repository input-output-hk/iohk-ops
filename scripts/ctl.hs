#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle safe base-unicode-symbols ])'
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/05126bc8503a37bfd2fe80867eb5b0bea287c633.tar.gz

{-# LANGUAGE GADTs, OverloadedStrings, RecordWildCards, UnicodeSyntax #-}

import           Data.Monoid               ((<>))
import           Data.Maybe
import qualified Data.Text                         as T
import           Data.Text                 (Text)
import           Filesystem.Path.CurrentOS         hiding (concat, empty)
-- import           Prelude.Unicode
import           Turtle


data Branch = Branch { fromBranch ∷ Text } deriving (Show)
data Commit = Commit { fromCommit ∷ Text } deriving (Show)


defaultNixpkgsCommit ∷ Commit
defaultNixpkgsCommit = Commit "05126bc8503a37bfd2fe80867eb5b0bea287c633"

iohkNixopsURL ∷ Text
iohkNixopsURL = "https://github.com/input-output-hk/iohk-nixops.git"


data Options where
  Options ∷
    { oNixpkgsCommit ∷ Commit
    } → Options

data Command
  = Area AreaCommand

data AreaCommand
  = New
  { anBranch       ∷ Branch
  , anDeployments  ∷ [Deployment]
  }

data Deployment
  = Explorer
  | Nodes
  | Infra
  | Report
  | Timewarp
  deriving (Eq, Read)

deployments ∷ [(Deployment, [Text])]
deployments =
  [ (Explorer
    , [ "deployments/cardano-explorer.nix"
      , "deployments/cardano-explorer-target-aws.nix"
      , "deployments/cardano-explorer-env-production.nix" ])
  , (Nodes
    , [ "deployments/cardano-nodes.nix"
      , "deployments/cardano-nodes-target-aws.nix"
      , "deployments/cardano-nodes-env-production.nix" ])
  , (Infra
    , [ "deployments/infrastructure.nix"
      , "deployments/infrastructure-target-aws.nix"
      , "deployments/infrastructure-env-production.nix" ])
  , (Report
    , [ "deployments/report-server.nix"
      , "deployments/report-server-target-aws.nix"
      , "deployments/report-server-env-production.nix" ])
  , (Timewarp
    , [ "deployments/timewarp.nix"
      , "deployments/timewarp-target-aws.nix" ])
  ]

deploymentFiles ∷ Deployment → [Text]
deploymentFiles = fromJust . flip lookup deployments


optionsParser ∷ Parser Options
optionsParser =
  Options
  <$> (fromMaybe defaultNixpkgsCommit
        <$> optional (Commit <$> optText "nixpkgs" 'n' "Nixpkgs commit to use"))

parser ∷ Parser (Options, Command)
parser = (,) <$> optionsParser <*>
  subcommand "area" "Operate on an iohk-nixops checkouts"
  (Area
   <$> (subcommand "new" "Checkout a new iohk-nixops experiment from BRANCH, for a specified set of deployments"
         (New
          <$> (Branch <$> argText "branch" "iohk-nixops branch to check out.")
          <*> (parseDeployments
               <$> ((,,,) 
                    <$> (optional (argRead "DEPL" "Deployment: 'Nodes', 'Infra', 'Report' or 'Timewarp'"))
                    <*> (optional (argRead "DEPL" "Deployment: 'Nodes', 'Infra', 'Report' or 'Timewarp'"))
                    <*> (optional (argRead "DEPL" "Deployment: 'Nodes', 'Infra', 'Report' or 'Timewarp'"))
                    <*> (optional (argRead "DEPL" "Deployment: 'Nodes', 'Infra', 'Report' or 'Timewarp'")))))))
  where parseDeployments (a, b, c, d) = concat $ maybeToList <$> [a, b, c, d]


main ∷ IO ()
main = do
  (opts, topcmd) ← options "Helper CLI around IOHK NixOps" parser
  case topcmd of
    Area cmd → runArea opts cmd


areaConfig ∷ Commit → Branch → [Deployment] → Text
areaConfig (Commit commit) (Branch branch) depls =
  T.unlines $
  [ "deploymentName: " <> branch
  , "nixPath: nixpkgs=https://github.com/NixOS/nixpkgs/archive/" <> commit <> ".tar.gz"
  , "nixopsExecutable: nixops"
  , "deploymentFiles:"
  ,    "  - deployments/keypairs.nix" ]
  ++ (("  - " <>) <$> concat (deploymentFiles <$> depls))

runArea ∷ Options → AreaCommand → IO ()
runArea opts@(Options nixpkgs) (New branch@(Branch bname) deployments) = do
  let branchDir = fromText bname
  exists ← testpath branchDir
  when exists $
    die $ "Directory already ∃: " <> bname
  procs "git" (["clone", iohkNixopsURL, bname]) empty
  cd branchDir
  procs "git" (["checkout", bname]) empty
  writeTextFile "config.yaml" $
    areaConfig nixpkgs branch deployments
