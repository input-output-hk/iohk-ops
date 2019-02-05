#!/bin/sh
set -xeu

cmd=$1; shift

constituents="./deployments/goguen-ala-cardano.nix ./deployments/goguen-ala-cardano-target-aws.nix"
common_nixops_opts="--deployment goguen-ala-cardano --max-jobs 4 --cores 0 --show-trace"
case ${cmd} in
        create | c ) nixops create   ${common_nixops_opts} ${constituents}
                     deployerIP="$(curl --connect-timeout 2 --silent http://169.254.169.254/latest/meta-data/public-ipv4)"
                     echo -n "Enter access key ID: "
                     read AKID
                     nixops set-args ${common_nixops_opts} --argstr accessKeyId "${AKID}" --argstr deployerIP "${deployerIP}"
                     ;;
        deploy | d ) nixops modify   ${common_nixops_opts} ${constituents}
                     nixops deploy   ${common_nixops_opts} "$@" --build-only
                     nixops deploy   ${common_nixops_opts} "$@" --copy-only
                     nixops deploy   ${common_nixops_opts} "$@";;
        delete )     nixops destroy  ${common_nixops_opts} --confirm
                     nixops delete   ${common_nixops_opts};;
        re )         $0 delete && $0 create && $0 deploy;;
        info   | i ) nixops info     ${common_nixops_opts};;
        * ) echo "ERROR: unknown command '${cmd}'" >&2; exit 1;;
esac
