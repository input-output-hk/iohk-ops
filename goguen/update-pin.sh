#!/bin/sh
## Usage: USER [REPO] [REV]  Pin a Github REPO to USER/REV,\n\t\t\t\t\t  Defaults: REPO=deduced from $CWD, REV=HEAD
set -xe

nhroot="$(realpath $0 | xargs dirname)"
cwdRepo="$(basename $(pwd))"

repo=${1:-${cwdRepo}}
rev=${2:-$(if test "${cwdRepo}" = "${repo}"; then git rev-parse HEAD; fi)}

url=$(jq .url < "${nhroot}"/pins/${repo}.src-json | xargs echo)
submodules="$(jq .submodules "${nhroot}"/pins/${repo}.src-json)"

fixup_private_repo_url_for_nix_prefetch_git() {
        sed 's,ssh://git@github.com/,git@github.com:,'
}
if echo ${url} | grep --quiet "git@github.com"
then fetchgit_fetch_submodules=""
     fetchgit_url=$(echo ${url} | fixup_private_repo_url_for_nix_prefetch_git)
     if test -n "${submodules}"
     then submodules=".submodules=${submodules}"
     else submodules="."
     fi
     sha256="del(.sha256)"
     fetchSubmodules="del(.fetchSubmodules)"
else fetchgit_fetch_submodules="--fetch-submodules"
     fetchgit_url=${url}
     submodules="."
     sha256="."
     fetchSubmodules="."
fi

tmp=$(mktemp iohk-update-pin.XXXXXXXX)
cleanup() {
        rm -f ${tmp}
}
trap cleanup EXIT
nix-prefetch-git ${fetchgit_fetch_submodules} ${fetchgit_url} ${rev} | jq "${submodules} | ${fetchSubmodules} | del(.date) | ${sha256} | .url=\"${url}\"" | tee ${tmp}
if test -n "$(cat ${tmp})"
then mv ${tmp} "${nhroot}"/pins/${repo}.src-json
else echo "ERROR:  pin update failed" >&2; rm -f ${tmp}; exit 1
fi
