with import <nixpkgs> {};
writeScriptBin "destroy-cluster.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  IO=$(nix-build -A iohk-ops)/bin/iohk-ops

  $IO destroy --confirm delete
''
