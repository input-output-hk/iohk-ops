#!/bin/sh
set -e

usage() {
        echo "$(basename $0) [--verbose] COMMAND ARGS.." >&2
        echo
        grep ' ) # Doc:' $0 | sed 's,^\([^ ]*\) .* \([^ ]*\) ) # Doc:\(.*\)$,"\1" "\2",' | tail -n +2 | {
                read a b
                while test -n "$a"
                do
                        printf "%20s  %s\n" "$(echo $a | xargs echo)" "$(echo $b | sed 's,-, ,g; s/\b\(.\)/\u\1/' | xargs echo)"
                        read a b
                done
        }
}
if test -n "RUNNING_GAC"
then RECURSIVE_GAC=yes
fi
export RUNNING_GAC=yes
log() {
        echo "==(  $*" >&2
}
logtop() {
        test -z "${RECURSING_GAC}" && echo "==(  $*" >&2
}
logn() {
        echo -n "==(  $*" >&2
}

cd `dirname $0`

verbose=""
while test -n "$1"
do case "$1" in
           --verbose | -v ) set -x; verbose="--verbose";;
           --help | "--"* ) usage; exit 1;;
           * ) break;;
   esac; shift; done
self="$0 ${verbose}"
cmd=${1:-doit}; test -n "$1"; shift
set -u

CONFIG=default
CLUSTER=create-.config.sh
if test ! -f .config.sh
then
        log "WARNING:  creating a default .config.sh with an empty cluster"
        cat > .config.sh <<EOF
CLUSTER=mempty
CONFIG=default
EOF
fi
. ./.config.sh
ALL_NODES="mantis-a-0 mantis-a-1 mantis-b-0 mantis-b-1 mantis-c-0 "
if test -z "${RECURSIVE_GAC}"
then log ".config.sh settings:"
     cat <<EOF
CLUSTER=${CLUSTER}
CONFIG=${CONFIG}
EOF
fi

nix_out="$(   nix-instantiate --eval -E '(import ((import ./lib.nix).goguenNixpkgs) {}).nix.outPath'    | xargs echo)"
nixpkgs_out=$(nix-instantiate --eval -E '(import ./lib.nix).goguenNixpkgs' | xargs echo)
nixops_out="$(nix-instantiate --eval -E '(import ((import ./lib.nix).goguenNixpkgs) {}).nixops.outPath' | xargs echo)"
nix=${nix_out}/bin/nix
nix_build=${nix_out}/bin/nix-build
nix_inst=${nix_out}/bin/nix-instantiate
nixops=${nixops_out}/bin/nixops
nix_opts="\
--max-jobs 4 --cores 0 --show-trace \
-I nixpkgs=${nixpkgs_out} \
"
nixops_nix_opts="${nix_opts} \
-I nixops=${nixops_out}/share/nix/nixops \
-I config=./configs/${CONFIG}.nix \
-I module=./modules \
-I static=./static \
-I goguen=./goguen \
"

ag=$( nix-build -E '(import ((import ./lib.nix).goguenNixpkgs) {}).ag'     | xargs echo)/bin/ag
aws=$(nix-build -E '(import ((import ./lib.nix).goguenNixpkgs) {}).awscli' | xargs echo)/bin/aws
jq=$( nix-build -E '(import ((import ./lib.nix).goguenNixpkgs) {}).jq'     | xargs echo)/bin/jq

if test ! -f ${nixops}
then nix-store --realise ${nixops}
fi

export NIX_PATH="nixpkgs=${nixpkgs_out}"

nixops_subopts="--deployment ${CLUSTER} ${nixops_nix_opts}"
nixops_subopts_deploy="${nixops_subopts} --max-concurrent-copy=50"
nixops_bincaches="https://cache.nixos.org https://hydra.iohk.io https://mantis-hydra.aws.iohkdev.io"

nixops_constituents="$(ls ./clusters/${CLUSTER}/*.nix)"
nixops_constituents_quoted="$(echo ${nixops_constituents} | sed 's,\\b,\",g')"
nixops_deplArgs="{ accessKeyId = \"AKID\"; deployerIP = \"127.0.0.1\"; }"
nixops_network_expr="import <nixops/eval-machine-info.nix> { \
    networkExprs = [ ${nixops_constituents_quoted} ]; \
    args = ${nixops_deplArgs}; \
    uuid = \"81976a61-80f6-11e7-9369-06258a1e40fd\"; \
    deploymentName = \"${CLUSTER}\"; \
    checkConfigurationOptions = false; \
  }"

generate_faucet_and_node_keys () {
        export JAVA_HOME=`nix-build -E  "(import (import ./lib.nix).goguenNixpkgs {}).pkgs.openjdk8"  --no-out-link`
        mantis=`nix-build -E  "(import ./goguen/. {}).mantis"`
        NODES="a-0 a-1 b-0 b-1 c-0"
        NODE_IDS="static/mantis-node-ids.nix"

        FAUCETS="faucet-a"
        FAUCET_ADDRS="static/faucet-addresses.nix"
        FAUCET_KEYGEN_CONF="static/mantis-faucet-keygen.conf"

        echo 'include "application.conf"' > ${FAUCET_KEYGEN_CONF}

        echo '{'                          > ${FAUCET_ADDRS}
        for f in $FAUCETS; do
                rm -f ~/.mallet/*
                ${mantis}/bin/mantis -Dconfig.file=${FAUCET_KEYGEN_CONF} mallet "http://127.0.0.1" --command newAccount --password ""
                cat   ~/.mallet/*         >> "static/mallet-${f}.json"
                addr="$(jq ".address" ~/.mallet/*)"
                echo "${f} = ${addr};"    >> ${FAUCET_ADDRS}
        done
        echo '}'                          >> ${FAUCET_ADDRS}

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

region_tagged_instances() {
        local region="$1"
        local tags="$2"
        ${aws} ec2 describe-instances --filters "Name=${tags}" --region  ${region} --output json
}
region_tagged_instances_property() {
        local region="$1"
        local tags="$2"
        local prop="$3"
        region_tagged_instances ${region} ${tags} | ${jq} ".Reservations${prop}"
}
count_region_tagged_instances() {
        local region="$1"
        local tags="$2"
        region_tagged_instances_property ${region} ${tags} "| length"
}
has_region_tagged_instances() {
        test "$(count_region_tagged_instances $*)" != 0
}
wait_region_tagged_instances() {
        local region="$1"
        local tags="$2"
        logn "Waiting for $region/$tags to appear: "
        while ! has_region_tagged_instances "$region" "$tags"
        do echo -n "."
        done
        echo " ok."
}
wait_host_ssh() {
        local host="$1"

        logn "Waiting for ssh pong from ${host}: "
        while ! ssh -tt -o ConnectionAttempts=20 ${host} -o ConnectTimeout=10 -o KbdInteractiveAuthentication=no -o PasswordAuthentication=no -o ChallengeResponseAuthentication=no -o StrictHostKeyChecking=no -o HashKnownHosts=no -o CheckHostIP=no "echo pong." 2>/dev/null
        do true; done
}

case ${cmd} in
"" | "" ) # Doc:
        ;;
"" | debug-support ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
eval | evaluate-nixops-machine-definition ) # Doc:
        nix-instantiate ${nixops_nix_opts} --eval -E  "let depl = ${nixops_network_expr}; in depl.machines { names = [\"mantis-a-0\"]; }";;
repl | repl-with-machine-definition ) # Doc:
        nixver="$(nix --version | cut -d ' ' -f3)"
        if test ${nixver} != 2.2 -a ${nixver} != 2.3 -a ${nixver} != 2.4 -a ${nixver} != 2.5
        then log "ERROR:  nix version 2.2 required for 'gac repl'"
        fi
        nix repl     ${nixops_nix_opts} --arg    depl       "${nixops_network_expr}" ./network.nix \
                                        --argstr nixpkgsSrc "${nixpkgs_out}";;
dry | full-deploy-dry-run ) # Doc:
        export AWS_PROFILE=default AWS_ACCESS_KEY_ID=AKIASDADFLKJDFJDJFDJ AWS_SECRET_ACCESS_KEY=Hlkjdflsjfjlnrmnsiuhfskjhkshfiuurrfsd/Rp
        if ${nixops} info  ${nixops_subopts} >/dev/null 2>&1
        then op=modify
        else op=create; generate_faucet_and_node_keys; fi
        ${nixops} ${op}    ${nixops_subopts} ${nixops_constituents}
        deployerIP="127.0.0.1"
        AKID=someBoringAKID # "(pow 2.71828 . (3.1415 *) . sqrt) -1 = -1"
        ${nixops} set-args ${nixops_subopts} --argstr accessKeyId "${AKID}" --argstr deployerIP "${deployerIP}"
        ${nixops} deploy   ${nixops_subopts_deploy} --dry-run "$@"
        ;;
"" | "" ) # Doc:
        ;;
"" | initial-cluster-setup ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
components | ls | list-cluster-components ) # Doc:
        log "components of cluster '${CLUSTER}':"
        echo ${nixops_constituents} | tr " " "\n" | sort | sed 's,^,   ,';;
configure | conf | configure-nixops-deployment-arguments ) # Doc:
        log "querying own IP.."
        deployerIP="$(curl --connect-timeout 2 --silent http://169.254.169.254/latest/meta-data/public-ipv4)"
        log "setting up the AWS access key.."
        set +u; AKID="$1"; set -u
        test -z "$AKID" && {
                guessedAKID="$(grep aws_access_key_id ~/.aws/credentials | cut -d= -f2 | xargs echo)"
                read -ei "${guessedAKID}" -p "Use AWS access key ID: " AKID
        }
        ${nixops} set-args ${nixops_subopts} \
                  --argstr accessKeyId "${AKID}" \
                  --argstr deployerIP "${deployerIP}"
                ;;
genkey | g | generate-mantis-faucet-and-node-keys ) # Doc:
        generate_faucet_and_node_keys;;
create | create-deployment-from-cluster-components ) # Doc:
        set +u; AKID="$1"; test -n "$1" && shift; set -u
        $self     list-cluster-components
        ${nixops} create   ${nixops_subopts} clusters/${CLUSTER}/*.nix
        $self     configure-nixops-deployment-arguments $AKID
        $self     generate-mantis-faucet-and-node-keys
        ${nixops} deploy   ${nixops_subopts_deploy} "$@";;
delete | destroy | terminate | abolish | eliminate | demolish | delete-nixops-deployment ) # Doc:
        ${nixops} destroy  ${nixops_subopts} --confirm
        ${nixops} delete   ${nixops_subopts};;
fromscratch | re | redeploy-cluster-from-scrach ) # Doc:
        $self delete && $self create && $self deploy;;
info   | i | nixops-info ) # Doc:
        ${nixops} info     ${nixops_subopts};;
"" | "" ) # Doc:
        ;;
"" | cluster-deployment ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
deploy-one | one | deploy-one-machine ) # Doc:
        $self     deploy --include mantis-a-0;;
deploy | d | update-and-deploy ) # Doc:
        $self     components
        ${nixops} modify   ${nixops_subopts} clusters/${CLUSTER}/*.nix
        ${nixops} deploy   ${nixops_subopts_deploy} "$@";;
"" | "" ) # Doc:
        ;;
"" | building-derivations ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
build | build-goguen-package ) # Doc:
        pkg=$1
        ${nix_build} ${nix_opts} -A ${pkg} goguen/release.nix;;
build-ala-hydra | build-goguen-package-like-hydra-does ) # Doc:
        #pkg=$1
        log "building Hydra"
        hydra_drv="$(   nix-instantiate -E '(import ((import ./lib.nix).goguenNixpkgs) {}).hydra')"
        hydra=$(${nix_build} ${nix_opts} ${hydra_drv})
        log "using Hydra to evaluate goguen/release.nix"
        ${hydra}/bin/hydra-eval-jobs -I ${nixpkgs_out} -I . -I https://github.com/input-output-hk/iohk-nix/archive/25225e9e23d8fe73663c1e958d41d481b0a4e0f0.tar.gz goguen/release.nix "$@";;
drv | goguen-derivation ) # Doc:
        pkg=$1
        ${nix_inst}  ${nix_opts} -A ${pkg} goguen/release.nix;;
drv-show | prettyprint-goguen-derivation ) # Doc:
        pkg=$1
        ${nix_inst}  ${nix_opts} -A ${pkg} goguen/release.nix | xargs ${nix} show-derivation;;
"" | "" ) # Doc:
        ;;
"" | basic-node-ssh ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
ssh | ssh-to-mantis-node ) # Doc:
        machine="${1:-mantis-a-0}";
        set +u; test -n "$1" && shift; set -u
        ${nixops} ssh          ${nixops_subopts} ${machine} "$@";;
ssh-all | parallel-ssh-on-all-mantis-nodes ) # Doc:
        ${nixops} ssh-for-each ${nixops_subopts} --parallel --include ${ALL_NODES} -- "$@";;
"" | "" ) # Doc:
        ;;
"" | systemd-service-control ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
stop | stop-all-mantis-services ) # Doc:
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl    stop mantis;;
start | start-all-mantis-services ) # Doc:
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl   start mantis; log "new start date:  $($0 since mantis-a-0)";;
restart | restart-all-mantis-services ) # Doc:
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl restart mantis; log "new start date:  $($0 since mantis-a-0)";;
statuses | ss | all-mantis-service-statuses ) # Doc:
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl  status mantis;;
"" | "" ) # Doc:
        ;;
"" | journald-logs-and-grepping ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
since | mantis-service-start-date-on-node ) # Doc:
        node=$1
        ${nixops} ssh          ${nixops_subopts} ${node} -- systemctl show mantis --value --property WatchdogTimestamp | cut -d' ' -f3;;
journal-on | jon | mantis-service-journal-on-node ) # Doc:
        node=$1
        ${nixops} ssh          ${nixops_subopts} ${node} -- journalctl  -u mantis --since $($0 since ${node});;
journal    | jall | j | all-mantis-service-journals ) # Doc:
        start_sample_node='mantis-a-0'
        since=${1:-$($0 since ${start_sample_node})}
        log "journal since:  ${since}"
        $0        ssh-all      -- journalctl -u mantis --since ${since};;
system-journal | sysj | system-journal-on-node ) # Doc:
        node=${1:-'mantis-a-0'}
        $0        ssh ${node}  -- journalctl -xe;;
follow | f | follow-mantis-service-journal-on-node ) # Doc:
        node=${1:-'mantis-a-0'}
        $0        ssh ${node}  -- journalctl -fu mantis;;
follow-all | fa | follow-all-mantis-service-journals ) # Doc:
        $0        ssh-all      -- journalctl -fu mantis "$@";;
grep-since | grep-all-mantis-service-journals-since-timestamp ) # Doc:
        since=$1; shift
        log "journal since:  ${since}"
        log "filter:         ag $*"
        $0        journal "${since}" 2>&1 | ${ag} "$@";;
grep | grep-all-mantis-service-journals-since-last-restart ) # Doc:
        start_sample_node='mantis-a-0'
        since=$($0 since ${start_sample_node})
        $0        grep-since ${since} "$@";;
watch-for | watch-all-mantis-service-journals-for-regex-occurence ) # Doc:
        $0        follow-all 2>&1 | ${ag} "$@";;

"" | "" ) # Doc:
        ;;
"" | service-logs-interpretation ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
blocks | mantis-node-recent-block-numbers ) # Doc:
        $0        grep-since "'30 seconds ago'" 'Best Block: ' | cut -d' ' -f5,14-;;
roles | mantis-node-roles-since-start ) # Doc:
        $0        grep 'role changed|LEADER';;
exceptions | ex | mantis-node-exceptions-since-start ) # Doc:
        $0        grep -i exception | ag -v 'RiemannBatchClient|exception.counter';;
watch-blocks | watch-mantis-node-current-block-number-stream ) # Doc:
        $0 watch-for 'Best Block: ' | cut -d' ' -f5,14-;;
watch-exceptions | watch-mantis-node-current-exception-stream ) # Doc:
        $0 watch-for -i exception | ag -v 'RiemannBatchClient|exception.counter';;
"" | "" ) # Doc:
        ;;
"" | deployer-bootstrap ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
describe-image | describe-deployer-nixops-image ) # Doc:
        nixos_ver="18.09"
        region="eu-central-1"
        export AWS_PROFILE=${CLUSTER} AWS_REGION=${region}
        ami="$(nix-instantiate --eval -E "(import ${nixpkgs_out}/nixos/modules/virtualisation/ec2-amis.nix).\"${nixos_ver}\".${region}.hvm-ebs" | xargs echo)"
        ${aws} ec2 describe-images --image-id ${ami} --region ${region}
        ;;
create-deployer | create-and-maybe-deploy-new-deployer ) # Doc:
        nixos_ver="18.09"
        region="eu-central-1"
        az=${region}b
        org=IOHK
        ec2type=t2.large
        depl_ssh_sg="allow-public-ssh-${region}-${org}"
        deployer_tags="tag:deployment,Values=${CLUSTER},Name=tag:role,Values=deployer,Name=instance-state-name,Values=running"
        export AWS_PROFILE=${CLUSTER} AWS_REGION=${region}

        if ! ${aws} ec2  describe-security-groups        --group-names ${depl_ssh_sg}
        then ${aws} ec2    create-security-group         --group-name  ${depl_ssh_sg} --description "${depl_ssh_sg}"
             ${aws} ec2 authorize-security-group-ingress --group-name  ${depl_ssh_sg} --protocol tcp --port 22 --cidr 0.0.0.0/0
        fi

        if has_region_tagged_instances ${region} ${deployer_tags}
        then log "ERROR:  deployer already exist."; exit 1
        fi
        log "creating a deployer.."
        ami="$(nix-instantiate --eval -E "(import ${nixpkgs_out}/nixos/modules/virtualisation/ec2-amis.nix).\"${nixos_ver}\".${region}.hvm-ebs" | xargs echo)"
        ${aws}      ec2 run-instances                                                \
                    --image-id ${ami}                                                \
                    --security-groups ${depl_ssh_sg}                                 \
                    --instance-type ${ec2type}                                       \
                    --placement AvailabilityZone=${az}                               \
                    --block-device-mappings DeviceName=/dev/sda1,Ebs={VolumeSize=200} \
                    --tag-specifications "ResourceType=instance,Tags=[{Key=deployment,Value=${CLUSTER}},{Key=role,Value=deployer}]" \
                    --user-data file://terraform/deployer/configuration-bootstrap.nix

        wait_region_tagged_instances ${region} ${deployer_tags}
        deployer_ip=$(region_tagged_instances_property ${region} ${deployer_tags} '[0].Instances[0].PublicIpAddress' | xargs echo)
        log "new deployer IP: ${deployer_ip}"

        wait_host_ssh "deployer@${deployer_ip}"

        cat <<EOF
Access the new deployer as:
-------------------- 8< ----------------------
Host ${CLUSTER}-deployer
  User     deployer
  Hostname ${deployer_ip}
EOF
        if test "$1" = "--full"
        then shift
             log "full setup requested, proceeding to set up deployer."
             $0 setup-deployer "$@"
        fi;;
setup-deployer | finalise-deployer-nixops-bootstrap ) # Doc:
        test -d ./clusters/${CLUSTER} || {
                log "ERROR: unknown cluster ${CLUSTER} -- to see available clusters:  ls clusters"
                exit 1
        }
        cluster_config="${CONFIG}"
        while test ! -f "${cluster_config}"
        do read -ei "./configs/default.nix" -p "Enter path to configuration file (one of ./config/*.nix):  " cluster_config
           test -f "${cluster_config}" || log "ERROR: ${cluster_config} is not a readable file"
        done

        region="eu-central-1"
        export AWS_PROFILE=${CLUSTER} AWS_REGION=${region}
        deployer_tags="tag:deployment,Values=${CLUSTER},Name=tag:role,Values=deployer,Name=instance-state-name,Values=running"

        log "setting up AWS.."
        deployer_ip=$(region_tagged_instances_property ${region} ${deployer_tags} '[0].Instances[0].PublicIpAddress' | xargs echo)

        set +u
        ops_url="$1"
        ops_branch="$2"
        test -z "$1" && read -ei "http://github.com/input-output-hk/iohk-ops"       -p "Ops git repository URL: "    ops_url
        test -z "$2" && read -ei "$(git symbolic-ref HEAD | sed 's|refs/heads/||')" -p "Ops git repository branch: " ops_branch
        set -u

        log "setting up AWS credentials.."
        AKID="$(sed -n "/\\[${CLUSTER}\\]/,/^\\[.*/ p" < ~/.aws/credentials | grep aws_access_key_id     | cut -d= -f2 | xargs echo)"
        ssh "deployer@${deployer_ip}" sh -c "\"mkdir -p ~/.aws && cat > ~/.aws/credentials && chmod -R go-rwx ~/.aws\"" <<EOF
[default]
aws_access_key_id = ${AKID}
aws_secret_access_key = $(sed -n "/\\[${CLUSTER}\\]/,/^\\[.*/ p" < ~/.aws/credentials | grep aws_secret_access_key | cut -d= -f2 | xargs echo)
region = ${region}
EOF
        log "generating SSH key and setting up ops checkout.."
        setup_cmd="\
ssh-keygen  -t ed25519 -b 2048 -f ~/.ssh/id_ed25519 -N ''; \
ssh-keyscan -t rsa     github.com >> ~/.ssh/known_hosts && \
ssh-keyscan -t ed25519 localhost  >> ~/.ssh/known_hosts && \
cat ~/.ssh/id_ed25519.pub && \
echo 'Please authorise this key and press Enter: ' && \
read foo && \
git clone ${ops_url} infra && \
cd infra && \
echo CLUSTER=${CLUSTER} > .config.sh && \
echo CONFIG=${cluster_config} >> .config.sh && \
git checkout ${ops_branch} && \
git config --replace-all receive.denyCurrentBranch updateInstead"
        ssh "deployer@${deployer_ip}" sh -c "\"${setup_cmd}\""

        echo "Deploying infra cluster.." >&2
        ssh -A "deployer@${deployer_ip}" sh -c "\"cd infra && ./gac.sh ${verbose} create-deployment-from-cluster-components ${AKID} \""
        ;;
"" | "" ) # Doc:
        ;;
help | show-this-help-message ) # Doc: Show this help message
        usage; exit 1;;
* ) usage; exit 1;;
esac
