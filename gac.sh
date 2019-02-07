#!/bin/sh
set -xeu

cmd=$1; shift

constituents="./deployments/goguen-ala-cardano.nix ./deployments/goguen-ala-cardano-target-aws.nix"
nixops_subopts="--deployment goguen-ala-cardano --max-jobs 4 --cores 0 --show-trace"
nixops_bincaches="https://cache.nixos.org https://hydra.iohk.io https://mantis-hydra.aws.iohkdev.io"
case ${cmd} in
        create | c ) nixops create   ${nixops_subopts} ${constituents}
                     deployerIP="$(curl --connect-timeout 2 --silent http://169.254.169.254/latest/meta-data/public-ipv4)"
                     echo -n "Enter access key ID: "
                     read AKID
                     nixops set-args ${nixops_subopts} --argstr accessKeyId "${AKID}" --argstr deployerIP "${deployerIP}"
                     ;;
        full-deploy ) nixops modify   ${nixops_subopts} ${constituents}
                      nixops deploy   ${nixops_subopts} "$@"              --option trusted-substituters "${nixops_bincaches}"
                     ;;
        deploy | d ) nixops modify   ${nixops_subopts} ${constituents}
                     nixops deploy   ${nixops_subopts} "$@" --build-only --option trusted-substituters "${nixops_bincaches}"
                     nixops deploy   ${nixops_subopts} "$@" --copy-only
                     nixops deploy   ${nixops_subopts} "$@";;
        delete )     nixops destroy  ${nixops_subopts} --confirm
                     nixops delete   ${nixops_subopts};;
        re )         $0 delete && $0 create && $0 deploy;;
        info   | i ) nixops info     ${nixops_subopts};;
        * ) echo "ERROR: unknown command '${cmd}'" >&2; exit 1;;
esac
