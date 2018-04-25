{
  clusterName ? "benchmarkspb51"
  # commit of cardano-sl
, commit      ? "517e7c7aa7e46bc584f309d423d2f18fd8d5365f"
}:

with import <nixpkgs> {};
writeScriptBin "set-cluster.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  CLUSTERNAME=${clusterName}

  COMMIT=${commit}  # commit rev of cardano-sl

  IO=$(nix-build -A iohk-ops)/bin/iohk-ops

  $IO -C .. set-rev cardanosl $COMMIT
  $IO -C .. -v new -t topology-staging.yaml -k bench $CLUSTERNAME nodes
''
