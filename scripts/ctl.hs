#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle safe base-unicode-symbols ])'
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/05126bc8503a37bfd2fe80867eb5b0bea287c633.tar.gz

{-# LANGUAGE GADTs, OverloadedStrings, RecordWildCards, UnicodeSyntax #-}

import           Data.Monoid               ((<>))
import           Data.Maybe
import qualified Data.Text                         as T
import           Data.Text                 (Text)
import           Filesystem.Path.CurrentOS         hiding (concat, empty, null)
-- import           Prelude.Unicode
import           Turtle


defaultNixpkgsCommit = Commit "05126bc8503a37bfd2fe80867eb5b0bea287c633"

iohkNixopsURL ∷ Text
iohkNixopsURL = "https://github.com/input-output-hk/iohk-nixops.git"

defaultEnvironment = Development

defaultTarget      = AWS


data Options where
  Options ∷
    { oNixpkgsCommit ∷ Commit
    } → Options

data Command
  = Area AreaCommand

data AreaCommand
  = New
  { anBranch       ∷ Branch
  , anEnvironment  ∷ Environment
  , anTarget       ∷ Target
  , anDeployments  ∷ [Deployment]
  }
  | Change
  { anBranch       ∷ Branch
  , anEnvironment  ∷ Environment
  , anTarget       ∷ Target
  , anDeployments  ∷ [Deployment]
  }


data Branch = Branch { fromBranch ∷ Text } deriving (Show)
data Commit = Commit { fromCommit ∷ Text } deriving (Show)


data Deployment
  = Explorer
  | Nodes
  | Infra
  | Report
  | Timewarp
  deriving (Eq, Read, Show)

data Environment
  = Any
  | Production
  | Staging
  | Development
  deriving (Eq, Read)

data Target
  = All
  | AWS
  deriving (Eq, Read)

type FileSpec = (Environment, Target, Text)

deployments ∷ [(Deployment, [(Environment, Target, Text)])]
deployments =
  [ (Explorer
    , [ (Any,         All, "deployments/cardano-explorer.nix")
      , (Development, All, "deployments/cardano-explorer-env-development.nix")
      , (Production,  All, "deployments/cardano-explorer-env-production.nix")
      , (Staging,     All, "deployments/cardano-explorer-env-staging.nix")
      , (Any,         AWS, "deployments/cardano-explorer-target-aws.nix") ])
  , (Nodes
    , [ (Any,         All, "deployments/cardano-nodes.nix")
      , (Production,  All, "deployments/cardano-nodes-env-production.nix")
      , (Staging,     All, "deployments/cardano-nodes-env-staging.nix")
      , (Any,         AWS, "deployments/cardano-nodes-target-aws.nix") ])
  , (Infra
    , [ (Any,         All, "deployments/infrastructure.nix")
      , (Production,  All, "deployments/infrastructure-env-production.nix")
      , (Any,         AWS, "deployments/infrastructure-target-aws.nix") ])
  , (Report
    , [ (Any,         All, "deployments/report-server.nix")
      , (Production,  All, "deployments/report-server-env-production.nix")
      , (Staging,     All, "deployments/report-server-env-staging.nix")
      , (Any,         AWS, "deployments/report-server-target-aws.nix") ])
  , (Timewarp
    , [ (Any,         All, "deployments/timewarp.nix")
      , (Any,         AWS, "deployments/timewarp-target-aws.nix") ])
  ]

deploymentSpecs ∷ Deployment → [FileSpec]
deploymentSpecs = fromJust . flip lookup deployments

filespecEnvSpecific ∷ Environment → FileSpec → Bool
filespecEnvSpecific x (x', _, _) = x == x'
filespecTgtSpecific ∷ Target      → FileSpec → Bool
filespecTgtSpecific x (_, x', _) = x == x'

filespecNeededEnv ∷ Environment → FileSpec → Bool
filespecNeededTgt ∷ Target      → FileSpec → Bool
filespecNeededEnv x fs = filespecEnvSpecific Any fs || filespecEnvSpecific x fs
filespecNeededTgt x fs = filespecTgtSpecific All fs || filespecTgtSpecific x fs

filespecFile ∷ FileSpec → Text
filespecFile (_, _, x) = x

deploymentFiles ∷ Environment → Target → Deployment → [Text]
deploymentFiles env tgt depl = filespecFile <$> (filter (\x → filespecNeededEnv env x && filespecNeededTgt tgt x) $ deploymentSpecs depl)


optionsParser ∷ Parser Options
optionsParser =
  Options
  <$> (fromMaybe defaultNixpkgsCommit
        <$> optional (Commit <$> optText "nixpkgs" 'n' "Nixpkgs commit to use"))

deploymentsParser ∷ Parser [Deployment]
deploymentsParser =
  (\(a, b, c, d) → concat $ maybeToList <$> [a, b, c, d])
  <$> ((,,,)
        <$> (optional (argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'Report' or 'Timewarp'"))
        <*> (optional (argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'Report' or 'Timewarp'"))
        <*> (optional (argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'Report' or 'Timewarp'"))
        <*> (optional (argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'Report' or 'Timewarp'")))

branchParser desc = Branch <$> argText "branch" desc
envParser         = fromMaybe defaultEnvironment <$> optional (optRead "env" 'e' "Environment: Development, Staging or Production;  defaults to Any")
tgtParser         = fromMaybe defaultTarget      <$> optional (optRead "tgt" 't' "Target: AWS;  defaults to All")

parser ∷ Parser (Options, AreaCommand)
parser = (,) <$> optionsParser <*>
  -- subcommand "area" "Operate on an iohk-nixops checkouts"
  (   subcommand "new"    "Checkout a new iohk-nixops experiment from BRANCH, for a specified set of deployments"
    (New    <$> branchParser "iohk-nixops branch to check out"                   <*> envParser <*> tgtParser <*> deploymentsParser)
  <|> subcommand "change" "Change configuration of the specified iohk-nixops experiment"
    (Change <$> branchParser "iohk-nixops experiment to update configuration of" <*> envParser <*> tgtParser <*> deploymentsParser)
  )


ttl = unsafeTextToLine

tShow ∷ Show a ⇒ a → Text
tShow = T.pack . show


main ∷ IO ()
main = do
  (opts, topcmd) ← options "Helper CLI around IOHK NixOps" parser
  runArea opts topcmd
  -- case topcmd of
    -- Area cmd → runArea opts cmd


areaConfig ∷ Commit → Branch → Environment → Target → [Deployment] → Text
areaConfig (Commit commit) (Branch branch) env tgt depls =
  T.unlines $
  [ "deploymentName: " <> branch
  , "nixPath: nixpkgs=https://github.com/NixOS/nixpkgs/archive/" <> commit <> ".tar.gz"
  , "nixopsExecutable: nixops"
  , "deploymentFiles:"
  ,    "  - deployments/keypairs.nix" ]
  ++ (("  - " <>) <$> concat (deploymentFiles env tgt <$> depls))

runArea ∷ Options → AreaCommand → IO ()
runArea opts@(Options nixpkgs) (New branch@(Branch bname) env tgt deployments) = do
  let branchDir = fromText bname
  exists ← testpath branchDir
  when exists $
    die $ "Directory already ∃: " <> bname
  procs "git" (["clone", iohkNixopsURL, bname]) empty
  cd branchDir
  procs "git" (["checkout", bname]) empty
  writeTextFile "config.yaml" $
    areaConfig nixpkgs branch env tgt deployments
  procs "git" (["config", "--replace-all", "receive.denyCurrentBranch", "warn"]) empty
  echo ""
  echo "-- config.yaml is:"
  procs "cat" ["config.yaml"] empty -- XXX TODO: figure out Turtle.cat
runArea opts@(Options nixpkgs) (Change branch@(Branch bname) env tgt deployments) = do
  let branchDir = fromText bname
  exists ← testpath branchDir
  unless exists $
    die $ "Directory does not ∃: " <> bname
  cd branchDir
  writeTextFile "config.yaml" $
    areaConfig nixpkgs branch env tgt deployments
  echo ""
  echo "-- config.yaml updated to:"
  procs "cat" ["config.yaml"] empty -- XXX TODO: figure out Turtle.cat
