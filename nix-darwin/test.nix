{ role ? "ci", host, port, hostname }:

let
  opsLib = import ../lib.nix;
  pkgs = opsLib.pkgs;
  nix-darwin = opsLib.sources.nix-darwin;
  system = (import nix-darwin {
    nixpkgs = pkgs.path;
    configuration = "${guestConfDir}/darwin-configuration.nix";
    system = "x86_64-darwin";
  }).system;
  lib = pkgs.lib;

  # this ensures that nix-darwin can find everything it needs, but wont see things it doesnt need
  # that prevents the guest from being rebooted when things it doesnt read get modified
  guestConfDir = pkgs.runCommand "guest-config-dir" {
    inherit host port hostname;
  } ''
    mkdir -pv $out
    cd $out
    mkdir -pv iohk-ops/nix-darwin sources
    cp -r --no-preserve=mode ${nix-darwin} sources/nix-darwin
    cd iohk-ops
    cp -r --no-preserve=mode ${./roles} nix-darwin/roles
    cp -r --no-preserve=mode ${./modules} nix-darwin/modules
    cp -r --no-preserve=mode ${./services} nix-darwin/services
    cp -r --no-preserve=mode ${../nix} nix
    cp ${./test.nix} nix-darwin/test.nix
    cp ${../lib.nix} lib.nix
    cp ${../iohk-nix.json} iohk-nix.json
    cp ${../nixpkgs-src.json} nixpkgs-src.json
    mkdir lib
    cp ${../lib/ssh-keys.nix} lib/ssh-keys.nix
    cp ${../fetch-nixpkgs.nix} fetch-nixpkgs.nix
    cd ..
    cp -r ${../modules/macs/guest}/* .
    substituteAll apply.sh apply.sh
    cd iohk-ops/nix-darwin/roles
    ln -sv ${role}.nix active-role.nix
  '';
in {
  inherit nix-darwin system guestConfDir;
}
