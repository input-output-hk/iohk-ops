let
  # temporary hack until scripts/nix-shell.sh ceases to use -p
  pkgs_path = import ./fetchNixpkgs.nix;
  pkgs = import pkgs_path { config = {}; overlays = []; };
  wrapped = pkgs.runCommand "nixpkgs" {} ''
    ln -sv ${pkgs_path} $out
  '';
in wrapped
