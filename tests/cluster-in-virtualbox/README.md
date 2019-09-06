# What

Runs a triplet of nodes inside a single VirtualBox machine, on a NixOS host.

# Usage
## Setup

    ./op.sh setup

## Deploy & tinker

    ./op.sh deploy

Then ssh the VirtualBox machine with the running nodes:

    ./op.sh ssh

..and issue:

    systemctl start cardano-node-cluster.target

This will start three `systemd` services: `cardano-node@0`,  `cardano-node@1` and `cardano-node@2` -- so all journald logging routines apply.
