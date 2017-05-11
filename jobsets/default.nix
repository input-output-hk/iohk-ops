{ nixpkgs ? <nixpkgs>, declInput ? {} }:

# Followed by https://github.com/NixOS/hydra/pull/418/files

let
  pkgs = import nixpkgs {};
  defaultSettings = {
    enabled = true;
    hidden = false;
    input = "jobsets";
    keep = 5;
    shares = 42;
    interval = 60;
    inputs = {
      nixpkgs = {
        type = "git";
        value = "https://github.com/NixOS/nixpkgs.git ebbababd8f9cb49d039f11d58e4e49d8e02d7533";
      };
      jobsets = {
        type = "git";
        value = "https://github.com/input-output-hk/iohk-nixops.git master";
      };
    };
    mail = false;
    mailOverride = "";
  };
  jobsetsAttrs = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    cardano-sl = {
      path = "jobsets/cardano.nix";
      description = "Cardano SL";
    };
    cardano-sl-staging = {
      path = "jobsets/cardano.nix";
      description = "Cardano SL";
      inputs = {
        nixpkgs = {
          type = "git";
          value = "https://github.com/NixOS/nixpkgs.git b9628313300b7c9e4cc88b91b7c98dfe3cfd9fc4";
        };
        jobsets = {
          type = "git";
          value = "https://github.com/input-output-hk/iohk-nixops.git staging";
        };
      };
    };
    deployments = {
      path = "jobsets/deployments.nix";
      description = "Builds for deployments";
    };
    #pos-prototype-deployment = {
    #  path = "jobsets/infra.nix";
    #  description = "Deployments infrastructure";
    #};
    #rscoin = {
    #  path = "jobsets/rscoin.nix";
    #  description = "RSCoin";
    #};
  });
in {
  jobsets = with pkgs.lib; pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
      ${concatStringsSep "," (mapAttrsToList (name: settings: ''
        "${name}": {
            "enabled": ${if settings.enabled then "1" else "0"},
            "hidden": ${if settings.hidden then "true" else "false"},
            "description": "${settings.description}",
            "nixexprinput": "${settings.input}",
            "nixexprpath": "${settings.path}",
            "checkinterval": ${toString settings.interval},
            "schedulingshares": ${toString settings.shares},
            "enableemail": ${if settings.mail then "true" else "false"},
            "emailoverride": "${settings.mailOverride}",
            "keepnr": ${toString settings.keep},
            "inputs": {
              ${concatStringsSep "," (mapAttrsToList (inputName: inputSettings: ''
                "${inputName}": { "type": "${inputSettings.type}", "value": "${inputSettings.value}", "emailresponsible": false }
              '') settings.inputs)}
            }
        }
      '') jobsetsAttrs)}
    }
    EOF
  '';
}
