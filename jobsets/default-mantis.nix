{ nixpkgs ? <nixpkgs>
, declInput ? {}
}:

# Followed by https://github.com/NixOS/hydra/pull/418/files

let
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
    };
    enableemail = false;
    emailoverride = "";
  };
  mkMantis = mantisBranch: {
    nixexprpath = "jobsets/release.nix";
    nixexprinput = "mantis";
    description = "Mantis";
    inputs = {
      mantis = mkFetchGithub "https://github.com/input-output-hk/mantis.git ${mantisBranch}";
    };
  };
  mainJobsets = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    mantis-testnet = mkMantis "phase/iele_testnet";
  });
  jobsetsAttrs = mainJobsets;
  jobsetJson = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);
in {
  jobsets = with pkgs.lib; pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toJSON declInput}
    EOF
    cp ${jobsetJson} $out
  '';
}
