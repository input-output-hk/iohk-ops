############################################################################
# This is the jobset declaration evaluated by Hydra to dynamically
# generate jobsets.
#
# The arguments for this file come from spec.json.
#
# See also the Hydra manual:
#   https://github.com/NixOS/hydra/blob/master/doc/manual/declarative-projects.xml
#
############################################################################

{ nixpkgs ? <nixpkgs>
, declInput ? {}

# Paths to JSON files containing PR info fetched from github.
# An example file is ./simple-pr-dummy.json.
, mantisPrsJSON ? ./simple-pr-dummy.json
}:

let pkgs = import nixpkgs {}; in

with pkgs.lib;

let

  nixpkgs-src = builtins.fromJSON (builtins.readFile ./../nixpkgs-src.json);
  iohkOpsJobsetsBranch = "goguen-ala-cardano"; # Should become "master" at some point.

  ##########################################################################
  # GitHub repos to make jobsets for.
  # These are processed by the mkRepoJobsets function below.

  repos = {
    mantis = {
      description = "Mantis Cardano";
      url = "git@github.com:input-output-hk/mantis-cardano.git";
      input = "mantis";  # corresponds to argument in goguen/release.nix
      branch = "master";
      branches = {
        master = "master";
      };
      prs = mantisPrsJSON;
      # prJobsetModifier = withFasterBuild;
      bors = false;
    };
  };

  ##########################################################################
  # Jobset generation functions

  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };

  defaultSettings = {
    enabled = 1;
    hidden = false;
    nixexprinput = "jobsets";
    keepnr = 5;
    schedulingshares = 42;
    checkinterval = 60;
    inputs = {
      nixpkgs = mkFetchGithub "https://github.com/NixOS/nixpkgs.git ${nixpkgs-src.rev}";
      jobsets = mkFetchGithub "${iohkOpsURI} ${iohkOpsJobsetsBranch}";
    };
    enableemail = false;
    emailoverride = "";
  };

  # Adds an arg which disables optimization for cardano-sl builds
  withFasterBuild = jobset: jobset // {
    inputs = (jobset.inputs or { }) // {
      fasterBuild = { type = "boolean"; emailresponsible = false; value = "true"; };
    };
  };

  # Use to put Bors jobs at the front of the build queue.
  highPrio = jobset: jobset // {
    schedulingshares = 420;
  };

  # Removes PRs which have any of the labels in ./pr-excluded-labels.nix
  exclusionFilter = let
    excludedLabels = import ./pr-excluded-labels.nix;
    justExcluded = filter (label: (elem label.name excludedLabels));
    isEmpty = ls: length ls == 0;
  in
    filterAttrs (_: prInfo: isEmpty (justExcluded (prInfo.labels or [])));

  loadPrsJSON = path: exclusionFilter (builtins.fromJSON (builtins.readFile path));

  # Make jobset for a project default build
  mkJobset = { name, description, url, input, branch }: let
    jobset = defaultSettings // {
      nixexprpath = "release.nix";
      nixexprinput = input;
      inherit description;
      inputs = {
        "${input}" = mkFetchGithub "${url} ${branch}";
      };
    };
  in
    nameValuePair name jobset;

  # Make jobsets for extra project branches (e.g. release branches)
  mkJobsetBranches = { name, description, url, input }:
    mapAttrsToList (suffix: branch:
      mkJobset { name = "${name}-${suffix}"; inherit description url input branch; });

  # Make a jobset for a GitHub PRs
  mkJobsetPR = { name, input, modifier }: num: info: {
    name = "${name}-pr-${num}";
    value = defaultSettings // modifier {
      description = "PR ${num}: ${info.title}";
      nixexprinput = input;
      nixexprpath = "release.nix";
      inputs = {
        "${input}" = mkFetchGithub "${info.base.repo.clone_url} pull/${num}/head";
      };
    };
  };

  # Load the PRs json and make a jobset for each
  mkJobsetPRs = { name, input, modifier, prs }:
    mapAttrsToList
      (mkJobsetPR { inherit name input modifier; })
      (loadPrsJSON prs);

  # Add two extra jobsets for the bors staging and trying branches
  mkJobsetBors = { name, ... }@args: let
    jobset = branch: (mkJobset (args // { inherit branch; })).value;
  in [
    (nameValuePair "${name}-bors-staging" (highPrio (jobset "bors/staging")))
    (nameValuePair "${name}-bors-trying" (jobset "bors/trying"))
  ];

  # Make all the jobsets for a project repo, according to the "repos" spec above.
  mkRepoJobsets = let
    mkRepo = name: info: let
      input = info.input or name;
      branch = info.branch or "master";
      params = { inherit name input; inherit (info) description url; };
      prJobsetModifier = info.prJobsetModifier or (s: s);
    in
      [ (mkJobset (params // { inherit branch; })) ] ++
      (mkJobsetBranches params (info.branches or {})) ++
      (mkJobsetPRs { inherit name input; inherit (info) prs; modifier = prJobsetModifier; }) ++
      (optionals (info.bors or false) (mkJobsetBors params));
  in
    rs: listToAttrs (concatLists (mapAttrsToList mkRepo rs));

  ##########################################################################
  # Jobsets which don't fit into the regular structure

  extraJobsets = mapAttrs (name: settings: defaultSettings // settings) ({
  });

  ##########################################################################
  # The final jobsets spec as JSON

  mainJobsets = mkRepoJobsets repos;
  jobsetsAttrs = mainJobsets // extraJobsets;
in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toJSON declInput}
    EOF
    cp ${pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs)} $out
  '';
}
