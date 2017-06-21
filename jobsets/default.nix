{ nixopsPrsJSON, cardanoPrsJSON, nixpkgs ? <nixpkgs>, declInput ? {} }:

# Followed by https://github.com/NixOS/hydra/pull/418/files

let
  nixopsPrs = builtins.fromJSON (builtins.readFile nixopsPrsJSON);
  cardanoPrs = builtins.fromJSON (builtins.readFile cardanoPrsJSON);
  iohkNixopsUri = "https://github.com/input-output-hk/iohk-nixops.git";
  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };
  nixpkgs-src = builtins.fromJSON (builtins.readFile ./../nixpkgs-src.json);
  pkgs = import nixpkgs {};
  defaultSettings = {
    enabled = 1;
    hidden = false;
    nixexprinput = "jobsets";
    keepnr = 5;
    schedulingshares = 42;
    checkinterval = 60;
    inputs = {
      nixpkgs = mkFetchGithub "https://github.com/NixOS/nixpkgs.git ${nixpkgs-src.rev}";
      jobsets = mkFetchGithub "${iohkNixopsUri} master";
    };
    enableemail = false;
    emailoverride = "";
  };
  mkCardano = nixopsBranch: nixpkgsRev: {
    nixexprpath = "jobsets/cardano.nix";
    description = "Cardano SL";
    inputs = {
      nixpkgs = mkFetchGithub "https://github.com/NixOS/nixpkgs.git ${nixpkgsRev}";
      jobsets = mkFetchGithub "${iohkNixopsUri} ${nixopsBranch}";
      nixops = mkFetchGithub "https://github.com/NixOS/NixOps.git tags/v1.5";
    };
  };
  makeNixopsPR = num: info: {
    name = "iohk-nixops-${num}";
    value = defaultSettings // {
      description = "PR ${num}: ${info.title}";
      nixexprpath = "jobsets/cardano.nix";
      inputs = {
        nixpkgs = mkFetchGithub "https://github.com/NixOS/nixpkgs.git ${nixpkgs-src.rev}";
        jobsets = mkFetchGithub "https://github.com/${info.head.repo.owner.login}/${info.head.repo.name}.git ${info.head.ref}";
        nixops = mkFetchGithub "https://github.com/NixOS/NixOps.git tags/v1.5";
      };
    };
  };
  nixopsPrJobsets = pkgs.lib.listToAttrs (pkgs.lib.mapAttrsToList makeNixopsPR nixopsPrs);
  mainJobsets = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    cardano-sl = mkCardano "master" nixpkgs-src.rev;
    cardano-sl-staging = mkCardano "staging" nixpkgs-src.rev;
    deployments = {
      nixexprpath = "jobsets/deployments.nix";
      description = "Builds for deployments";
    };
  });
  jobsetsAttrs =  nixopsPrJobsets // mainJobsets;
  jobsetJson = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);
in {
  jobsets = with pkgs.lib; pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toJSON declInput}
    EOF
    cp ${jobsetJson} $out
  '';
}
