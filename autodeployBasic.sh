#!/bin/sh

echo "Cleaning up previous deployment"
./destroy.sh

echo "Creating a deployment..."
nixops create nixops.nix -d cardano
nixops deploy -d cardano -I nixpkgs=~/nixpkgs/
