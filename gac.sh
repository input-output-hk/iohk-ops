#!/bin/sh
set -e

usage() {
        echo "$(basename $0) [--verbose] COMMAND ARGS.." >&2
        exit 1
}

cd `dirname $0`

while test -n "$1"
do case "$1" in
           --verbose ) set -x;;
           --help | "--"* ) usage;;
           * ) break;;
   esac; shift; done
cmd=${1:-doit}; test -n "$1"; shift
set -u

CLUSTER=create-.config.sh
if test -f .config.sh
then . ./.config.sh
else if test ${cmd} != "repl" -a ${cmd} != "eval"; then echo "ERROR:  echo CLUSTER=your-cluster-name > .config.sh" >&2; exit 1; fi
fi
ALL_NODES="mantis-a-0 mantis-a-1 mantis-b-0 mantis-b-1 mantis-c-0 "

nixpkgs_out=$(nix-instantiate --eval -E '(import ./lib.nix).goguenNixpkgs' | xargs echo)
nixops_out="$(nix-instantiate --eval -E '(import ((import ./lib.nix).goguenNixpkgs) {}).nixops.outPath' | xargs echo)"
nixops=${nixops_out}/bin/nixops
nix_opts="-I nixpkgs=${nixpkgs_out} -I nixops=${nixops_out}/share/nix/nixops"
ag=$(nix-build -E '(import ((import ./lib.nix).goguenNixpkgs) {}).ag' | xargs echo)/bin/ag

if test ! -f ${nixops}
then nix-store --realise ${nixops}
fi

export NIX_PATH="nixpkgs=${nixpkgs_out}"

nixops_subopts="--deployment ${CLUSTER} --max-jobs 4 --cores 0 --show-trace ${nix_opts}"
nixops_bincaches="https://cache.nixos.org https://hydra.iohk.io https://mantis-hydra.aws.iohkdev.io"

nixops_constituents="./deployments/goguen-ala-cardano.nix ./deployments/goguen-ala-cardano-target-aws.nix"
nixops_constituents_quoted="$(echo ${nixops_constituents} | sed 's,\\b,\",g')"
nixops_deplArgs="{ accessKeyId = \"AKID\"; deployerIP = \"127.0.0.1\"; }"
nixops_network_expr="import <nixops/eval-machine-info.nix> { \
    networkExprs = [ ${nixops_constituents_quoted} ]; \
    args = ${nixops_deplArgs}; \
    uuid = \"81976a61-80f6-11e7-9369-06258a1e40fd\"; \
    deploymentName = \"${CLUSTER}\"; \
    checkConfigurationOptions = false; \
  }"

generate_keys () {
  export JAVA_HOME=`nix-build -E  "(import (import ./lib.nix).goguenNixpkgs {}).pkgs.openjdk8"  --no-out-link`
  MANTIS=`nix-build -E  "(import ./goguen/. {}).mantis"`
  NODES="a-0 a-1 b-0 b-1 c-0"
  NODE_IDS="static/mantis-node-ids.nix"

  echo "{" > "$NODE_IDS"

  for n in $NODES; do 
    KEY_FILE="static/mantis-$n.key"
    
    if [ ! -f "$KEY_FILE" ]; then
      $MANTIS/bin/eckeygen > "$KEY_FILE"
    fi
    
    NODE_ID="`sed -n 2p \"$KEY_FILE\"`"
    echo "  mantis-$n = { id = \"$NODE_ID\"; };" >> "$NODE_IDS"
  done

  echo "}" >> "$NODE_IDS"

}

case ${cmd} in
###
###
###
        eval )
                nix-instantiate ${nix_opts} --eval -E  "let depl = ${nixops_network_expr}; in depl.machines { names = [\"mantis-a-0\"]; }";;
        repl )
                nix repl        ${nix_opts} --arg depl "${nixops_network_expr}" \
                                                            ./network.nix \
                                         --argstr nixpkgsSrc ${nixpkgs_out};;
###
###
###
genkey | g )
        generate_keys;;
create | c )
        ${nixops} create   ${nixops_subopts} ${nixops_constituents}
        deployerIP="$(curl --connect-timeout 2 --silent http://169.254.169.254/latest/meta-data/public-ipv4)"
        guessedAKID="$(grep aws_access_key_id ~/.aws/credentials | cut -d= -f2 | xargs echo)"
        read -ei "${guessedAKID}" -p "Use AWS access key ID: " AKID
        ${nixops} set-args ${nixops_subopts} --argstr accessKeyId "${AKID}" --argstr deployerIP "${deployerIP}"
        generate_keys
        ${nixops} deploy   ${nixops_subopts} "$@"
        ;;
delete | destroy | terminate | abolish | eliminate | demolish )
        ${nixops} destroy  ${nixops_subopts} --confirm
        ${nixops} delete   ${nixops_subopts};;
re )
        $0 delete && $0 create && $0 deploy;;
info   | i )
        ${nixops} info     ${nixops_subopts};;
###
###
###
staged-deploy )
        ${nixops} modify   ${nixops_subopts} ${nixops_constituents}
        ${nixops} deploy   ${nixops_subopts} "$@" --copy-only
        ${nixops} deploy   ${nixops_subopts} "$@";;
deploy | d )
        ${nixops} modify   ${nixops_subopts} ${nixops_constituents}
        ${nixops} deploy   ${nixops_subopts} "$@";;
deploy-one | one )
        $0 deploy --include mantis-a-0;;
###
###
###
ssh )
        machine="${1:-mantis-a-0}";
        set +u; test -n "$1" && shift; set -u
        ${nixops} ssh          ${nixops_subopts} ${machine} "$@";;
ssh-all )
        ${nixops} ssh-for-each ${nixops_subopts} --parallel --include ${ALL_NODES} -- "$@";;   
###
###
###
stop )
        $0        ssh-all      -- systemctl    stop mantis;;
start )
        $0        ssh-all      -- systemctl   start mantis; echo "### new start date:  $($0 since mantis-a-0)";;
restart )
        $0        ssh-all      -- systemctl restart mantis; echo "### new start date:  $($0 since mantis-a-0)";;
statuses | ss )
        $0        ssh-all      -- systemctl  status mantis;;
###
###
###
since )
        node=$1
        ${nixops} ssh          ${nixops_subopts} ${node} -- systemctl show mantis --value --property WatchdogTimestamp | cut -d' ' -f3;;
journal-on | jon )
        node=$1
        ${nixops} ssh          ${nixops_subopts} ${node} -- journalctl  -u mantis --since $($0 since ${node});;
journal    | jall | j )
        start_sample_node='mantis-a-0'
        since=${1:-$($0 since ${start_sample_node})}
        echo "### journal since:  ${since}" >&2
        $0        ssh-all      -- journalctl -u mantis --since ${since};;
system-journal | sysj )
        node=${1:-'mantis-a-0'}
        $0        ssh ${node}  -- journalctl -xe;;
follow | f )
        node=${1:-'mantis-a-0'}
        $0        ssh ${node}  -- journalctl -fu mantis;;
follow-all | fa )
        $0        ssh-all      -- journalctl -fu mantis "$@";;
grep-since )
        since=$1; shift
        echo "### journal since:  ${since}" >&2
        echo "### filter:         ag $*" >&2
        $0        journal "${since}" 2>&1 | ${ag} "$@";;
grep )
        start_sample_node='mantis-a-0'
        since=$($0 since ${start_sample_node})
        $0        grep-since ${since} "$@";;
watch-for )
        $0        follow-all 2>&1 | ${ag} "$@";;
             
###
###
###
blocks )
        $0        grep-since "'30 seconds ago'" 'Best Block: ' | cut -d' ' -f5,14-;;
roles )
        $0        grep 'role changed|LEADER';;
exceptions | ex )
        $0        grep -i exception | ag -v 'RiemannBatchClient|exception.counter';;
watch-blocks )
        $0 watch-for 'Best Block: ' | cut -d' ' -f5,14-;;
watch-exceptions )
        $0 watch-for -i exception | ag -v 'RiemannBatchClient|exception.counter';;
###
###
###
doit )
        echo "magic";;
* ) usage;;
esac
