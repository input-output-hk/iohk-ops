#!/usr/bin/env bash

set -xe

# Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

source ${scriptDir}/../pkgs/generate.sh

git diff --text --exit-code
