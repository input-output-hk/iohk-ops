# What

Runs a triplet of nodes inside a single VirtualBox machine, on a NixOS host.

# Usage
## Setup

    ./tests/triplet-in-virtualbox setup

## Deploy & tinker

    ./tests/triplet-in-virtualbox deploy

Then ssh the VirtualBox machine with the running nodes:

    ./tests/triplet-in-virtualbox ssh

..and issue:

    systemctl start cardano-node-services.target

This will start three `systemd` services: `cardano-node@3000`,  `cardano-node@3001` and `cardano-node@3002` -- so all journald logging routines apply.
