{ pkgs, lib, ... }:

with lib;
{
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
    package = pkgs.nix';
  };
}
