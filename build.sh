nix-build -E 'with import ~/nixpkgs {}; callPackage ./default.nix { }'
notify-send "Cardano-sl build.sh has exited"
