#!/bin/sh

nix-build -A "$1" $(dirname $0)/default.nix --cores 0 -j4
