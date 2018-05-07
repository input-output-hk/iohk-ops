{
  clusterName ? "edgenodes-cluster"
}:

with import <nixpkgs> {};
writeScriptBin "set-wallets-cluster.sh" ''
  #!/usr/bin/env bash

  set -e        # exit on error
  set -o xtrace # print commands

  CLUSTERNAME=${clusterName}

  nixops create -d $CLUSTERNAME ../deployments/edgenodesv2.nix
  nixops set-args -d $CLUSTERNAME --argstr accessKeyId cardano-deployer --arg topologyFile ../topology-edgenode.yaml --arg nodes 10

  nixops deploy -d $CLUSTERNAME

''
