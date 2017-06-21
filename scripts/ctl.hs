#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle safe base-unicode-symbols ])'
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/ed070354a9e307fdf20a94cb2af749738562385d.tar.gz

{-# LANGUAGE GADTs, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures #-}

import           Data.Monoid                      ((<>))
import           Data.Maybe
import           Data.Optional (Optional)
import qualified Data.Text                     as T
import           Data.Text                        (Text)
import           Filesystem.Path.CurrentOS hiding (concat, empty, null)
import           Text.Read                        (readMaybe)
import           Turtle


iohkNixopsURL :: Text
iohkNixopsURL = "https://github.com/input-output-hk/iohk-nixops.git"

defaultEnvironment = Development
defaultTarget = AWS
defaultNixpkgsCommit = Commit "05126bc8503a37bfd2fe80867eb5b0bea287c633"

data Options where
  Options ::
    { oNixpkgsCommit :: Commit
    } -> Options

data Command
  = Template
  { anEnvironment  :: Environment
  , anTarget       :: Target
  , anBranch       :: Branch
  , anDeployments  :: [Deployment]
  } deriving (Show)


data Branch = Branch { fromBranch :: Text } deriving (Show)
data Commit = Commit { fromCommit :: Text } deriving (Show)


data Deployment
  = Explorer
  | Nodes
  | Infra
  | ReportServer
  | Timewarp
  deriving (Eq, Read, Show)

data Environment
  = Any
  | Production
  | Staging
  | Development
  deriving (Eq, Read, Show)

data Target
  = All
  | AWS
  deriving (Eq, Read, Show)

type FileSpec = (Environment, Target, Text)

deployments :: [(Deployment, [(Environment, Target, Text)])]
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
  , (ReportServer
    , [ (Any,         All, "deployments/report-server.nix")
      , (Production,  All, "deployments/report-server-env-production.nix")
      , (Staging,     All, "deployments/report-server-env-staging.nix")
      , (Any,         AWS, "deployments/report-server-target-aws.nix") ])
  , (Timewarp
    , [ (Any,         All, "deployments/timewarp.nix")
      , (Any,         AWS, "deployments/timewarp-target-aws.nix") ])
  ]

deploymentSpecs :: Deployment -> [FileSpec]
deploymentSpecs = fromJust . flip lookup deployments

filespecEnvSpecific :: Environment -> FileSpec -> Bool
filespecEnvSpecific x (x', _, _) = x == x'
filespecTgtSpecific :: Target      -> FileSpec -> Bool
filespecTgtSpecific x (_, x', _) = x == x'

filespecNeededEnv :: Environment -> FileSpec -> Bool
filespecNeededTgt :: Target      -> FileSpec -> Bool
filespecNeededEnv x fs = filespecEnvSpecific Any fs || filespecEnvSpecific x fs
filespecNeededTgt x fs = filespecTgtSpecific All fs || filespecTgtSpecific x fs

filespecFile :: FileSpec -> Text
filespecFile (_, _, x) = x

deploymentFiles :: Environment -> Target -> Deployment -> [Text]
deploymentFiles env tgt depl = filespecFile <$> (filter (\x -> filespecNeededEnv env x && filespecNeededTgt tgt x) $ deploymentSpecs depl)

optReadLower :: Read a => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optReadLower = opt (readMaybe . T.unpack . T.toTitle)

optionsParser :: Parser Options
optionsParser =
  Options
  <$> (fromMaybe defaultNixpkgsCommit
        <$> optional (Commit <$> optText "nixpkgs" 'n' "Nixpkgs commit to use"))

deploymentsParser :: Parser [Deployment]
deploymentsParser =
  (\(a, b, c, d) -> concat $ maybeToList <$> [a, b, c, d])
  <$> ((,,,)
        <$> (optional (argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'ReportServer' or 'Timewarp'"))
        <*> (optional (argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'ReportServer' or 'Timewarp'"))
        <*> (optional (argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'ReportServer' or 'Timewarp'"))
        <*> (optional (argRead "DEPL" "Deployment: 'Explorer', 'Nodes', 'Infra', 'ReportServer' or 'Timewarp'")))


parser :: Parser (Options, Command)
parser = (,) <$> optionsParser <*>
  (subcommand "template" "Clone iohk-nixops from git BRANCH, for a specified set of deployments"
    (Template <$> envParser
              <*> tgtParser
              <*> branchParser "iohk-nixops branch to check out"
              <*> deploymentsParser)
  )
  where
    branchParser desc = Branch <$> argText "branch" desc
    envParser = fromMaybe defaultEnvironment <$> optional (optReadLower "environment" 'e' "Environment: Development, Staging or Production;  defaults to Development")
    tgtParser = fromMaybe defaultTarget      <$> optional (optReadLower "target" 't' "Target: AWS, All;  defaults to AWS")

main :: IO ()
main = do
  (opts, topcmd) <- options "Helper CLI around IOHK NixOps" parser
  run opts topcmd

-- TODO: use NixOpsConfig and ToJSON
areaConfig :: Commit -> Branch -> Environment -> Target -> [Deployment] -> Text
areaConfig (Commit commit) (Branch branch) env tgt depls =
  T.unlines $
  [ "deploymentName: " <> branch
  , "nixPath: nixpkgs=https://github.com/NixOS/nixpkgs/archive/" <> commit <> ".tar.gz"
  , "nixopsExecutable: nixops"
  , "deploymentFiles:"
  ,    "  - deployments/keypairs.nix" ]
  ++ (("  - " <>) <$> concat (deploymentFiles env tgt <$> depls))

run :: Options -> Command -> IO ()
run (Options nixpkgs) (Template env tgt branch@(Branch bname) deployments) = do
  homeDir <- home
  let branchDir = homeDir <> (fromText bname)
  exists <- testpath branchDir
  if exists
    then echo $ "Using existing git clone ..."
    else procs "git" (["clone", iohkNixopsURL, "-b", bname, bname]) empty
  cd branchDir
  let configFile = envToConfig env
  writeTextFile (fromText configFile) $ areaConfig nixpkgs branch env tgt deployments
  procs "git" (["config", "--replace-all", "receive.denyCurrentBranch", "warn"]) empty
  echo ""
  echo $ "-- " <> (unsafeTextToLine configFile) <> " is:"
  procs "cat" [configFile] mempty

envToConfig :: IsString s => Environment -> s
envToConfig Any = "config.yaml"
envToConfig Development = "config.yaml"
envToConfig Staging = "staging.yaml"
envToConfig Production = "production.yaml"
