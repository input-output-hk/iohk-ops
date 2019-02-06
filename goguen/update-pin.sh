#!/bin/sh
## Usage: USER [REPO] [REV]  Pin a Github REPO to USER/REV,\n\t\t\t\t\t  Defaults: REPO=deduced from $CWD, REV=HEAD
set -x

nhroot="$(realpath $0 | xargs dirname)"
cwdRepo="$(basename $(pwd))"

repo=${1:-${cwdRepo}}
rev=${2:-$(if test "${cwdRepo}" = "${repo}"; then git rev-parse HEAD; fi)}

user=$(jq .url < "${nhroot}"/pins/${repo}.src-json | cut -d/ -f4)

base=https://github.com/
nix-prefetch-git --fetch-submodules ${base}${user}/${repo} ${rev} | tee "${nhroot}"/pins/${repo}.src-json

