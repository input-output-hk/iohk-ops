#!/bin/sh
set -xeu

cmd=$1; shift

CLUSTER=create-.config.sh
if test -f .config.sh
then . ./.config.sh
else if test ${cmd} != "repl" -a ${cmd} != "eval"; then echo "ERROR:  echo CLUSTER=your-cluster-name > .config.sh" >&2; exit 1; fi
fi

nixpkgs_out=$(nix-instantiate --eval -E '(import ./lib.nix).nixpkgs' | xargs echo)
nixops_out="$(nix-instantiate --eval -E '(import ((import ./lib.nix).nixpkgs) {}).nixops.outPath' | xargs echo)"
nixops=${nixops_out}/bin/nixops
nix_opts="-I nixpkgs=${nixpkgs_out} -I nixops=${nixops_out}/share/nix/nixops"

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

case ${cmd} in
        create | c ) ${nixops} create   ${nixops_subopts} ${nixops_constituents}
                     deployerIP="$(curl --connect-timeout 2 --silent http://169.254.169.254/latest/meta-data/public-ipv4)"
                     echo -n "Enter access key ID (aws_access_key_id at ~/.aws/credentials): "
                     read AKID
                     ${nixops} set-args ${nixops_subopts} --argstr accessKeyId "${AKID}" --argstr deployerIP "${deployerIP}"
                     ${nixops} deploy   ${nixops_subopts} "$@"
                     ;;
        deploy | d ) ${nixops} modify   ${nixops_subopts} ${nixops_constituents}
                     ${nixops} deploy   ${nixops_subopts} "$@" --copy-only
                     ${nixops} deploy   ${nixops_subopts} "$@";;
        delete )     ${nixops} destroy  ${nixops_subopts} --confirm
                     ${nixops} delete   ${nixops_subopts};;
        ssh )        ${nixops} ssh      ${nixops_subopts} "$@";;
        eval )       nix-instantiate ${nix_opts} --eval -E  "let depl = ${nixops_network_expr}; in depl.machines { names = [\"mantis-a-0\"]; }";;
        repl )       nix repl        ${nix_opts} --arg depl "${nixops_network_expr}" \
                                                                    ./network.nix \
                                                 --argstr nixpkgsSrc ${nixpkgs_out};;
        info   | i ) ${nixops} info     ${nixops_subopts};;
        re )         $0 delete && $0 create && $0 deploy;;
        * ) echo "ERROR: unknown command '${cmd}'" >&2; exit 1;;
esac
