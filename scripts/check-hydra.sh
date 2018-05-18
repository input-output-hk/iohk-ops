#!/bin/sh

echo '~~~ Prebuilding hydra'
/run/current-system/sw/bin/nix-build https://github.com/nixos/nixpkgs/archive/4fb198892d298452023ab176e7067da58d30772e.tar.gz -A hydra -o hydra
/run/current-system/sw/bin/nix-build https://github.com/nixos/nixpkgs/archive/6c064e6b1f3.tar.gz -A hydra -o hydra2

echo '~~~ Evaluating release.nix with nix1'
./hydra/bin/hydra-eval-jobs -I . jobsets/cardano.nix --show-trace --option allowed-uris https://github.com

echo
echo '~~~ Evaluating release.nix with nix2'
./hydra2/bin/hydra-eval-jobs -I . jobsets/cardano.nix --show-trace --option allowed-uris https://github.com
