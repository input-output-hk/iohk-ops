#!/bin/sh
## Usage: USER [REPO] [REV]  Pin a Github REPO to USER/REV,\n\t\t\t\t\t  Defaults: REPO=deduced from $CWD, REV=HEAD

nhroot="$(realpath $0 | xargs dirname)"
cwdRepo="$(basename $(pwd))"

user=$1
test -n "${user}" || { echo "USAGE: $(basename $0) GITHHUB-USER GITHUB-REPO COMMIT-ID" >&2; exit 1; }

repo=${2:-${cwdRepo}}
rev=${3:-$(if test "${cwdRepo}" = "${repo}"; then git rev-parse HEAD; fi)}


case ${user} in
    local | home ) base=file://$HOME; user='';;
    * )            base=https://github.com/ ;;
esac
nix-prefetch-git ${base}${user}/${repo} ${rev} | tee "${nhroot}"/pins/${repo}.src-json
