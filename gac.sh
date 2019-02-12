#!/bin/sh
set -xeu

CLUSTER=create-.config.sh
if test -f .config.sh
then . ./.config.sh
else echo "ERROR:  echo CLUSTER=your-cluster-name > .config.sh" >&2; exit 1
fi

cmd=$1; shift

constituents="./deployments/goguen-ala-cardano.nix ./deployments/goguen-ala-cardano-target-aws.nix"
nixops_subopts="--deployment ${CLUSTER} --max-jobs 4 --cores 0 --show-trace"
nixops_bincaches="https://cache.nixos.org https://hydra.iohk.io https://mantis-hydra.aws.iohkdev.io"
case ${cmd} in
        create | c ) nixops create   ${nixops_subopts} ${constituents}
                     deployerIP="$(curl --connect-timeout 2 --silent http://169.254.169.254/latest/meta-data/public-ipv4)"
                     echo -n "Enter access key ID (aws_access_key_id at ~/.aws/credentials): "
                     read AKID
                     nixops set-args ${nixops_subopts} --argstr accessKeyId "${AKID}" --argstr deployerIP "${deployerIP}"
                     nixops deploy   ${nixops_subopts} "$@"              --option trusted-substituters "${nixops_bincaches}"
                     ;;
        deploy | d ) nixops modify   ${nixops_subopts} ${constituents}
                     nixops deploy   ${nixops_subopts} "$@" --copy-only --option trusted-substituters "${nixops_bincaches}"
                     nixops deploy   ${nixops_subopts} "$@";;
        delete )     nixops destroy  ${nixops_subopts} --confirm
                     nixops delete   ${nixops_subopts};;
        re )         $0 delete && $0 create && $0 deploy;;
        ssh )        nixops ssh      ${nixops_subopts} "$@";;
        info   | i ) nixops info     ${nixops_subopts};;
        * ) echo "ERROR: unknown command '${cmd}'" >&2; exit 1;;
esac
