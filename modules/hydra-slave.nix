{ config, pkgs, lib, ... }:

with lib;

let
  iohk-pkgs = import ../default.nix {};
in
{
  services = {
    dd-agent.tags              = [" group:hydra-and-slaves"];
  };
  nixpkgs.overlays = [
    (self: super: {
      nix' = super.nix.overrideAttrs ( drv: {
        # This patch creates a nsswitch.conf in the build environment so cardano-sl
        # integration tests can pass inside a pure build derivation.
        patches = [
          (pkgs.fetchpatch {
             url = "https://github.com/NixOS/nix/commit/a6c5955c8d59ca26745843f9779253a173876665.patch";
             name = "nsswitch.patch";
             sha256 = "1kk2j9bnv3c4ck6c79mwmq04q1qzzq8jvxsi2csyswh9pvxdf1hv";
           })
        ];
      });
    })
  ];

  nix = {
    binaryCaches = mkForce [ "https://cache.nixos.org" ];
    gc = {
      automatic = true;
      dates = "*:15:00";
      options = ''--max-freed "$((64 * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print $4 }')))"'';
    };
    package = pkgs.nix';
  };

  environment.systemPackages = [ iohk-pkgs.iohk-ops ];

  users.extraUsers.root.openssh.authorizedKeys.keys = pkgs.lib.singleton ''
    command="nice -n20 ${pkgs.utillinux}/bin/flock -s /var/lock/lab nix-store --serve --write" ${pkgs.lib.readFile ./../static/id_buildfarm.pub}
  '';
}
