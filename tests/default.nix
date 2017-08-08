{ pkgs ? import <nixpkgs> {}, supportedSystems ? [ "x86_64-linux" ] }:

with pkgs;
with pkgs.lib;

let
  forAllSystems = genAttrs supportedSystems;
  importTest = fn: args: system: import fn ({
    inherit system;
    config.packageOverrides = old: {
      qemu = old.qemu.overrideAttrs (oldattrs: {
        patches = [ ./no-etc-install.patch ];
        src = pkgs.fetchFromGitHub {
          owner = "qemu";
          repo = "qemu";
          rev = "aaaec6acad7cf97372d48c1b09126a09697519c8";
          fetchSubmodules = true;
          sha256 = "07hjgrmk4dv3fqwl38bvg0511lkwvil4xfcli465cjpm74y8k3kz";
        };
      });
    };
  } // args);
  callTest = fn: args: forAllSystems (system: hydraJob (importTest fn args system));
in rec {
  # TODO: tests of images
  simpleNode = callTest ./simple-node.nix {};
  simpleNodeNixOps = callTest ./simple-node-nixops.nix {};
}
