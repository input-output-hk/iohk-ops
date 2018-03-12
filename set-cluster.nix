{
  clusterName ? "benchmarks110policies"
  # commit of cardano-sl
, commit      ? "4a1d1080535f0c693a83ca9941505a51ec8aa7e2" 
}:

with import <nixpkgs> {};
writeScriptBin "set-cluster.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  CLUSTERNAME=${clusterName}

  COMMIT=${commit}  # commit rev of cardano-sl

  IO=$(nix-build -A iohk-ops)/bin/iohk-ops

  $IO set-rev cardanosl $COMMIT
  $IO -v new -t topology-staging.yaml -k bench $CLUSTERNAME nodes
''
