#!/usr/bin/env bash
## Usage: [REPO] [REV] [REF]  Pin a Github REPO to USER/REV, with optional REF specified\n\t\t\t\t\t  Defaults: REPO=deduced from $CWD, REV=HEAD, REF=none-or-previous

set -e

nhroot="$(realpath "$0" | xargs dirname)"
cwdRepo="$(basename "$(pwd)")"

do_commit=
verbose=
while test -n "$1"
do case "$1" in
           --commit  | -c ) do_commit=t;;
           --verbose | -v ) set -x; export verbose="--verbose";;
           * ) break;;
   esac; shift; done

repo=${1:-${cwdRepo}}
rev=${2:-$(if test "${cwdRepo}" = "${repo}"; then git rev-parse 'HEAD'; fi)}
new_ref=$3

jq=$(nix-build -E '(import <nixpkgs> {}).jq' | xargs echo)/bin/jq
pinfile="${nhroot}"/pins/${repo}-src.json

if test -f "${pinfile}"
then url=$(        ${jq}  .url               "${pinfile}" | xargs echo)
     submodules="$(${jq} '.submodules // {}' "${pinfile}")"
     ref=$(        ${jq}  .ref               "${pinfile}" | xargs echo)
else echo "Seeing that ${repo}'s a pin file (${pinfile}) is absent, questions arise:"
     read -re -p "Git repository URL:  " url
     test -n "${new_ref}" ||
             read -re -i 'master' -p "Git 'ref' (not rev!) name to fetch:  " ref
fi
if   test -n "${new_ref}"
then ref=${new_ref}; refop='.ref="'${ref}'"'
elif test -n "${ref}"
then refop='.ref="'${ref}'"'
else refop='.'
fi

fixup_private_repo_url_for_nix_prefetch_git() {
        sed 's,ssh://git@github.com/,git@github.com:,'
}
if echo "${url}" | grep --quiet "git@github.com"
then fetchgit_fetch_submodules=""
     fetchgit_url=$(echo "${url}" | fixup_private_repo_url_for_nix_prefetch_git)
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

tmp=$(mktemp update-pin.XXXXXXXX)
cleanup() {
        rm -f "${tmp}"
}
trap cleanup EXIT
nix-prefetch-git --no-deepClone ${fetchgit_fetch_submodules} "${fetchgit_url}" "${rev}" | ${jq} "${submodules} | ${refop} | ${fetchSubmodules} | del(.date) | ${sha256} | .url=\"${url}\"" | tee "${tmp}"
if test -n "$(cat "${tmp}")"
then mv "${tmp}" "${pinfile}"
else echo "ERROR:  pin update failed" >&2; rm -f "${tmp}"; exit 1
fi

if test -n "${do_commit}"
then echo "-- commit requested, proceeding.."
     git add "${pinfile}"
     git commit -m "nix | pin:  ${repo} -> $(test -n "${ref}" && echo -n "$(echo "${ref}" | xargs)"/)${rev}"
fi
