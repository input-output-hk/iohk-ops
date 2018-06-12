{
  clusterName ? "benchmarksCluster" # commit of cardano-sl
}:

with import <nixpkgs> {};
writeScriptBin "set-cluster.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  CLUSTERNAME=${clusterName}

  nix-build -A cardano-sl-tools -o tools
  export PATH=$PWD/tools/bin:$PATH

  IO=$(nix-build -A iohk-ops)/bin/iohk-ops

  $IO -C .. -v new -e benchmark $CLUSTERNAME nodes
''

#$IO -C .. -v new -t topology-staging.yaml -k bench $CLUSTERNAME nodes
