#!/usr/bin/env bash
set -e

usage() {
        echo "$(basename $0) [--verbose] COMMAND ARGS.." >&2
        echo
        echo "                new  CLUSTER-NAME [OPS-BRANCH-NAME] [CLUSTER-TYPE] [CLUSTER-KIND]"
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

OPS_REPO='git@github.com:input-output-hk/iohk-ops'
DEFAULT_OPS_BRANCH='master'

if test "$1" = "new" -o "$1" = "seed"
then
        shift
        case "$1" in
                -h | --help | help )cat <<EOF
$(basename $0) new CLUSTER-NAME [OPS-BRANCH-NAME] [CLUSTER-TYPE] [CLUSTER-KIND]

Sets up a new, named deployment checkout for a cluster of type CLUSTER_TYPE
for subsequent deployment.

The CLUSTER-NAME will be used both for the ops checkout directory, and
the Nixops deployment.

Available cluster types:  $(if test ! -d clusters; then cd .seed; fi && ls clusters | xargs echo)

Generals documentation: https://github.com/input-output-hk/iohk-ops/blob/goguen-ala-cardano/docs/Goguen-clusters-HOWTO.org

EOF
                                    exit 1;; esac;
        CLUSTER_NAME="$1";                            test -n "$1" && shift
        BRANCH_NAME="${1:-$DEFAULT_OPS_BRANCH}";      test -n "$1" && shift
        CLUSTER_TYPE="${1:-mantis}";                  test -n "$1" && shift
        CLUSTER_KIND="${1:-mantis}";                  test -n "$1" && shift
        set -u
        while test -d "${CLUSTER_NAME}" -o -z "${CLUSTER_NAME}" || nixops info -d "${CLUSTER_NAME}" >/dev/null 2>/dev/null
        do read -ei "${CLUSTER_NAME}" -p "Cluster '${CLUSTER_NAME}' already exists, please choose another name: " CLUSTER_NAME
        done
        git clone "${OPS_REPO}" "${CLUSTER_NAME}"
        cd "${CLUSTER_NAME}"
        git checkout "${BRANCH_NAME}"
        cat > .config.sh <<EOF
CLUSTER_KIND=${CLUSTER_KIND}
CLUSTER_TYPE=${CLUSTER_TYPE}
CLUSTER_NAME=${CLUSTER_NAME}
CONFIG=default
EOF
        $0           list-cluster-components
        if test "${CLUSTER_TYPE}" = "mantis"
        then log "generating node keys.."
             $0.sh   generate-node-keys
        fi
        log "creating the Nixops deployment.."
        nixops       create   -d "${CLUSTER_NAME}" "clusters/${CLUSTER_TYPE}"/*.nix
        $0           configure-nixops-deployment-arguments
        log "cluster has been set up, but not deployed;  Next steps:"
        log "  1. cd ${CLUSTER_NAME}"
        log "  2. gac deploy"
        exit 0
fi

verbose=""
if ! grep -q CLUSTER_KIND ./.config.sh || \
   ! grep -q CLUSTER_TYPE ./.config.sh || \
   ! grep -q CLUSTER_NAME ./.config.sh || \
   ! grep -q CONFIG       ./.config.sh
then
        log "ERROR:  malformed .config.sh -- missing one of CLUSTER_{KIND,TYPE,NAME} or CONFIG"
        exit 1
fi
. ./.config.sh
if test -z "${RECURSIVE_GAC}"
then log ".config.sh settings:"
     cat <<EOF
CLUSTER_KIND=${CLUSTER_KIND}
CLUSTER_TYPE=${CLUSTER_TYPE}
CLUSTER_NAME=${CLUSTER_NAME}
CONFIG=${CONFIG}
EOF
fi
OPS_REPO='git@github.com:input-output-hk/iohk-ops'
NODE_DERIVATION=${CLUSTER_KIND}
NODE_EXECUTABLE=${CLUSTER_KIND}
DEFAULT_NODE=${CLUSTER_KIND}-a-0
NODE_SERVICE=${CLUSTER_KIND}
NODE_DB_PATH=/data/${CLUSTER_KIND}
ALL_NODES="${DEFAULT_NODE} ${CLUSTER_KIND}-a-1 ${CLUSTER_KIND}-b-0 ${CLUSTER_KIND}-b-1 ${CLUSTER_KIND}-c-0 "

while test -n "$1"
do case "$1" in
           --verbose | -v ) set -x; verbose="--verbose";;
           --help | "--"* ) usage; exit 1;;
           * ) break;;
   esac; shift; done
self="$0 ${verbose}"
cmd=${1:-doit}; test -n "$1"; shift
set -u

###
### Note: Nixpkgs specification convention
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### gac.sh relies on ./default.nix providing the 'pkgs' and 'nixpkgs' attributes coming from iohk-nix.
###
nixpkgs_out=$(nix-instantiate  --eval -E '(import ./. {}).nixpkgs'     | xargs echo)
nix_out="$(   nix-build --no-out-link -E '(import ./. {}).pkgs.nix'    | xargs echo)"
nixops_out="$(nix-build --no-out-link -E '(import ./. {}).pkgs.nixops' | xargs echo)"
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
-I configs=./configs \
-I module=./modules \
-I static=./static \
-I goguen=./goguen \
"

if test ! -f ${nixops}
then nix-store --realise ${nixops}
fi

export NIX_PATH="nixpkgs=${nixpkgs_out}"

nixops_subopts="--deployment ${CLUSTER_NAME} ${nixops_nix_opts}"
nixops_subopts_deploy="${nixops_subopts} --max-concurrent-copy=50"
nixops_bincaches="https://cache.nixos.org https://hydra.iohk.io https://mantis-hydra.aws.iohkdev.io"

nixops_constituents="$(ls ./clusters/${CLUSTER_TYPE}/*.nix)"
nixops_constituents_quoted="$(echo ${nixops_constituents} | sed 's,\\b,\",g')"
nixops_deplArgs="{ accessKeyId = \"AKID\"; deployerIP = \"127.0.0.1\"; }"
nixops_network_expr="import <nixops/eval-machine-info.nix> { \
    networkExprs = [ ${nixops_constituents_quoted} ]; \
    args = ${nixops_deplArgs}; \
    uuid = \"81976a61-80f6-11e7-9369-06258a1e40fd\"; \
    deploymentName = \"${CLUSTER_NAME}\"; \
    checkConfigurationOptions = false; \
  }"

generate_faucet_keys () {
        export JAVA_HOME=`nix-build -E    "(import ./. {}).pkgs.openjdk8"  --no-out-link`
        node=`nix-build --no-out-link -E  "(import ./. {}).${NODE_DERIVATION}"`
        FAUCETS="faucet-a"
        FAUCET_ADDRS="static/faucet-addresses.nix"
        FAUCET_KEYGEN_CONF="static/faucet-keygen.conf"

        echo 'include "application.conf"' > ${FAUCET_KEYGEN_CONF}

        echo '{'                          > ${FAUCET_ADDRS}
        for f in $FAUCETS; do
                rm -f ~/.mallet/*
                ${node}/bin/${NODE_EXECUTABLE} -Dconfig.file=${FAUCET_KEYGEN_CONF} mallet "http://127.0.0.1" --command newAccount --password ""
                cat   ~/.mallet/*         >> "static/mallet-${f}.json"
                addr="$(jq ".address" ~/.mallet/*)"
                echo "${f} = ${addr};"    >> ${FAUCET_ADDRS}
        done
        echo '}'                          >> ${FAUCET_ADDRS}
}

generate_node_keys () {
        export JAVA_HOME=`nix-build   -E  "(import ./. {}).pkgs.openjdk8"  --no-out-link`
        node=`nix-build --no-out-link -E  "(import ./. {}).${NODE_DERIVATION}"`
        NODE_IDS="static/node-ids.nix"

        echo "{" > "$NODE_IDS"
        for n in $ALL_NODES; do
                KEY_FILE="static/$n.key"

                if [ ! -f "$KEY_FILE" ]; then
                        ${node}/bin/eckeygen > "$KEY_FILE"
                fi

                NODE_ID="`sed -n 2p \"$KEY_FILE\"`"
                echo "  $n = { id = \"$NODE_ID\"; };" >> "$NODE_IDS"
        done
        echo "}" >> "$NODE_IDS"
}

region_tagged_instances() {
        local region="$1"
        local tags="$2"
        aws=$(nix-build --no-out-link -E '(import ./. {}).pkgs.awscli' | xargs echo)/bin/aws
        ${aws} ec2 describe-instances --filters "Name=${tags}" --region  ${region} --output json
}
region_tagged_instances_property() {
        local region="$1"
        local tags="$2"
        local prop="$3"
        jq=$( nix-build --no-out-link -E '(import ./. {}).pkgs.jq'     | xargs echo)/bin/jq
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
        nix-instantiate ${nixops_nix_opts} --eval -E  "let depl = ${nixops_network_expr}; in depl.machines { names = [\"${DEFAULT_NODE}\"]; }";;
repl | repl-with-machine-definition ) # Doc:
        nixver="$(nix --version | cut -d ' ' -f3)"
        if test ${nixver} != 2.2 -a ${nixver} != 2.3 -a ${nixver} != 2.4 -a ${nixver} != 2.5
        then log "ERROR:  nix version 2.2 required for 'gac repl'"
        fi
        nix repl     ${nixops_nix_opts} --arg    depl       "${nixops_network_expr}" ./network.nix \
                                        --argstr nixpkgsSrc "${nixpkgs_out}";;
dry | full-new-cluster-create-and-deploy-dry-run ) # Doc:
        export AWS_PROFILE=default AWS_ACCESS_KEY_ID=AKIASDADFLKJDFJDJFDJ AWS_SECRET_ACCESS_KEY=Hlkjdflsjfjlnrmnsiuhfskjhkshfiuurrfsd/Rp
        if ${nixops} info  ${nixops_subopts} >/dev/null 2>&1
        then op=modify
        else op=create; generate_node_keys; fi
        ${nixops} ${op}    ${nixops_subopts} ${nixops_constituents}
        deployerIP="127.0.0.1"
        AKID=someBoringAKID # "(pow 2.71828 . (3.1415 *) . sqrt) -1 = -1"
        ${nixops} set-args ${nixops_subopts} \
		  --argstr accessKeyId "${AKID}" \
		  --argstr deployerIP "${deployerIP}" \
	          --argstr clusterName "${CLUSTER_NAME}" \
		  --arg config "import ./config.nix { clusterName = \"${CLUSTER_NAME}\"; deployerIP = \"${deployerIP}\"; accessKeyId = \"${AKID}\"; }"
        ${nixops} deploy   ${nixops_subopts_deploy} --dry-run "$@"
        ;;
"" | "" ) # Doc:
        ;;
"" | initial-cluster-setup ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
create | create-cluster-nixops-deployment ) # Doc:
        set +u; AKID="$1"; test -n "$1" && shift; set -u
        ${nixops}    create   -d "${CLUSTER_NAME}" "clusters/${CLUSTER_TYPE}"/*.nix
        ${self}      configure-nixops-deployment-arguments
        ;;

components | ls | list-cluster-components ) # Doc:
        log "components of cluster ${CLUSTER_NAME}:"
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
                  --argstr deployerIP "${deployerIP}" \
	          --argstr clusterName "${CLUSTER_NAME}" \
		  --arg config "import ./config.nix { clusterName = \"${CLUSTER_NAME}\"; deployerIP = \"${deployerIP}\"; accessKeyId = \"${AKID}\"; }"
                ;;
genkey | g | generate-node-keys ) # Doc:
        generate_node_keys;;

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
        $self     deploy --include ${DEFAULT_NODE};;
deploy | d | update-and-deploy ) # Doc:
	if ! ${nixops} info ${nixops_subopts} &>/dev/null; then
		log "recreating the Nixops deployment.."
		${nixops} create ${nixops_subopts} "clusters/${CLUSTER_TYPE}"/*.nix
	fi
        $self     configure-nixops-deployment-arguments
        $self     components
        ${nixops} modify   ${nixops_subopts} clusters/${CLUSTER_TYPE}/*.nix
        ${nixops} deploy   ${nixops_subopts_deploy} "$@";;
"" | "" ) # Doc:
        ;;
"" | building-derivations ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
update-pin | update | pin | update-a-goguen-package-pin ) # Doc:
        $(dirname $0)/goguen/update-pin.sh "$@";;
build | b | build-goguen-package ) # Doc:
        pkg=$1; shift
        ${nix_build} ${nix_opts} -A ${pkg} './default.nix' "$@";;
build-ala-hydra | build-goguen-package-like-hydra-does ) # Doc:
        #pkg=$1
        log "building Hydra"
        hydra_drv="$(   nix-instantiate -E '(import ./. {}).pkgs.hydra')"
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
ssh | ssh-to-node ) # Doc:
        machine="${1:-${DEFAULT_NODE}}";
        set +u; test -n "$1" && shift; set -u
        ${nixops} ssh          ${nixops_subopts} ${machine} "$@";;
ssh-all | parallel-ssh-on-all-nodes ) # Doc:
        ${nixops} ssh-for-each ${nixops_subopts} --parallel --include ${ALL_NODES} -- "$@";;
"" | "" ) # Doc:
        ;;
"" | systemd-service-control ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
stop | stop-all-node-services ) # Doc:
        on=${1:+--include $*}
        log "stopping node services on:  ${on}"
        $0        ssh-all ${on} -- systemctl    stop ${NODE_SERVICE};;
start | start-all-node-services ) # Doc:
        on=${1:+--include $*}
        log "starting node services on:  ${on}"
        $0        ssh-all ${on} -- systemctl   start ${NODE_SERVICE}; log "new start date:  $($0 since ${DEFAULT_NODE})";;
restart | restart-all-node-services ) # Doc:
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl restart ${NODE_SERVICE}; log "new start date:  $($0 since ${DEFAULT_NODE})";;
statuses | ss | all-node-service-statuses ) # Doc:
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl  status ${NODE_SERVICE};;
"" | "" ) # Doc:
        ;;
"" | journald-logs-and-grepping ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
since | node-service-start-date-on ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        ${nixops} ssh          ${nixops_subopts} ${node} -- systemctl show ${NODE_SERVICE} --value --property WatchdogTimestamp 2>&1 | cut -d' ' -f3;;
journal-on | jo | node-service-journal-on-node-since-start ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        set +u; since=${2:-$($0 since ${node})}; set -u
        if test -z "${since}"; then since='3 hours ago'; fi
        ${nixops} ssh          ${nixops_subopts} ${node} -- journalctl  -u ${NODE_SERVICE} --since "'${since}'" 2>&1;;
pretty-journal-on | pj | node-service-journal-on-node-since-its-start-prettified-version ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        $0 node-service-journal-on-node-since-start ${node} | fgrep -v ',"state": "ok","service": "' | cut -c92- | less;;
journal    | jall | j | all-node-service-journals ) # Doc:
        start_sample_node=${DEFAULT_NODE}
        since=${1:-$($0 since ${start_sample_node})}
        if test -z "${since}"; then since='3 hours ago'; fi
        log "journal since:  ${since}"
        $0        ssh-all      -- journalctl -u ${NODE_SERVICE} --since "'${since}'" 2>&1;;
system-journal | sysj | system-journal-on-node ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        $0        ssh ${node}  -- journalctl -xe 2>&1;;
follow | f | follow-node-service-journal-on ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        $0        ssh ${node}  -- journalctl -fu ${NODE_SERVICE} 2>&1;;
follow-all | fa | follow-all-node-service-journals ) # Doc:
        $0        ssh-all      -- journalctl -fu ${NODE_SERVICE} "$@" 2>&1;;
grep-on | gro | grep-node-service-journals-since-start ) # Doc:
        node=$1; shift
        log "node:           ${node}"
        log "filter:         ag $*"
        ag=$( nix-build --no-out-link -E '(import ./. {}).pkgs.ag'     | xargs echo)/bin/ag
        $0        journal-on ${node} 2>&1 | ${ag} "$@";;
grep-since | gras | grep-all-node-service-journals-since-timestamp ) # Doc:
        since=$1; shift
        log "journal since:  ${since}"
        log "filter:         ag $*"
        ag=$( nix-build --no-out-link -E '(import ./. {}).pkgs.ag'     | xargs echo)/bin/ag
        $0        journal "${since}" 2>&1 | ${ag} "$@";;
grep | g | grep-all-node-service-journals-since-last-restart ) # Doc:
        start_sample_node=${DEFAULT_NODE}
        since=$($0 since ${start_sample_node})
        if test -z "${since}"; then since='3 hours ago'; fi
        $0        grep-since ${since} "$@";;
watch-for | w | watch-all-node-service-journals-for-regex-occurence ) # Doc:
        ag=$( nix-build --no-out-link -E '(import ./. {}).pkgs.ag'     | xargs echo)/bin/ag
        $0        follow-all 2>&1 | ${ag} "$@";;

"" | "" ) # Doc:
        ;;
"" | service-logs-interpretation ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
blocks | bs |  node-recent-block-numbers ) # Doc:
        $0        grep-since "'30 seconds ago'" 'Best Block: ' | cut -d' ' -f5,14-;;
roles | rs | node-roles-since-start ) # Doc:
        $0        grep 'role changed|LEADER';;
exceptions | ex | node-exceptions-since-start ) # Doc:
        $0        grep -i exception | ag -v 'RiemannBatchClient|exception.counter';;
watch-blocks | wb | watch-node-current-block-number-stream ) # Doc:
        $0        watch-for 'Best Block: ' | cut -d' ' -f5,14-;;
watch-roles | wr | watch-node-current-role-stream ) # Doc:
        $0        watch-for 'role changed|LEADER';;
watch-roles-pretty | wrp | watch-node-current-role-stream-prettified ) # Doc:
        $0        watch-roles | sed 's/^\([^\.]*\)\..*oldRole","value": "\([A-Z]*\)".*newRole","value": "\([A-Z]*\).*$/\1: \2 -> \3/';;
watch-exceptions | we | watch-node-current-exception-stream ) # Doc:
        $0        watch-for -i exception | ag -v 'RiemannBatchClient|exception.counter';;
troubleshoot | tro | troubleshoot-node-logs-for-known-problems ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        $0        grep-node-service-journals-since-start ${node} 'Genesis data present in the database does not match genesis block from file.' && echo 'PROBLEM FOUND!' || true
        $0        grep-node-service-journals-since-start ${node} 'candidate is not known to the local member' && echo 'PROBLEM FOUND!' || true
        ;;
"" | "" ) # Doc:
        ;;
"" | DANGER:-destructive-operations ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
drop-dbs | remove-all-blockchain-data-on-all-nodes ) # Doc:
        log "about to nuke blockchain DB and leadership consensus DB, globally.."
        read -ei "No!" -p "Really? Type 'yes' to confirm:  " disaster
        test "${disaster}" == "yes" || { log "disaster averted"; exit 1; }
        log "disaster considered authorized"
        $0        stop
        log "nuking.."
        $0        ssh-all      -- rm -rf ${NODE_DB_PATH}
        $0        start;;
"" | "" ) # Doc:
        ;;
"" | blockchain-level-interaction ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
get-funds | faucet | money-please | withdraw-funds-from-faucet ) # Doc:
        to_addr=${1:-805932097ab26cc1061424ddcae9f4cd54a09c7b}
        curl -X POST "http://faucet-a.${CLUSTER_NAME}.dev-mantis.iohkdev.io:8099/faucet?address=0x${to_addr}";;
"" | "" ) # Doc:
        ;;
"" | deployer-bootstrap ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
describe-image | describe-deployer-nixops-image ) # Doc:
        nixos_ver="18.09"
        region="eu-central-1"
        export AWS_PROFILE=${CLUSTER_KIND} AWS_REGION=${region}
        ami="$(nix-instantiate --eval -E "(import ${nixpkgs_out}/nixos/modules/virtualisation/ec2-amis.nix).\"${nixos_ver}\".${region}.hvm-ebs" | xargs echo)"
        aws=$(nix-build --no-out-link -E '(import ./. {}).pkgs.awscli' | xargs echo)/bin/aws
        ${aws} ec2 describe-images --image-id ${ami} --region ${region}
        ;;
create-deployer | create-and-maybe-deploy-new-deployer ) # Doc:
        nixos_ver="18.09"
        region="eu-central-1"
        az=${region}b
        org=IOHK
        ec2type=t2.large
        depl_ssh_sg="allow-public-ssh-${region}-${org}"
        deployer_tags="tag:deployment,Values=${CLUSTER_KIND},Name=tag:role,Values=deployer,Name=instance-state-name,Values=running"
        export AWS_PROFILE=${CLUSTER_KIND} AWS_REGION=${region}
        aws=$(nix-build --no-out-link -E '(import ./. {}).pkgs.awscli' | xargs echo)/bin/aws

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
                    --tag-specifications "ResourceType=instance,Tags=[{Key=deployment,Value=${CLUSTER_KIND}},{Key=role,Value=deployer}]" \
                    --user-data file://terraform/deployer/configuration-bootstrap.nix

        wait_region_tagged_instances ${region} ${deployer_tags}
        deployer_ip=$(region_tagged_instances_property ${region} ${deployer_tags} '[0].Instances[0].PublicIpAddress' | xargs echo)
        log "new deployer IP: ${deployer_ip}"

        wait_host_ssh "deployer@${deployer_ip}"

        cat <<EOF
Access the new deployer as:
-------------------- 8< ----------------------
Host ${CLUSTER_KIND}-deployer
  User     deployer
  Hostname ${deployer_ip}
EOF
        if test "$1" = "--full"
        then shift
             log "full setup requested, proceeding to set up deployer."
             $0 setup-deployer "$@"
        fi;;
setup-deployer | finalise-deployer-nixops-bootstrap ) # Doc:
        test -d ./clusters/${CLUSTER_KIND} || {
                log "ERROR: unknown cluster ${CLUSTER_KIND} -- to see available clusters:  ls clusters"
                exit 1
        }
        cluster_config="${CONFIG}"
        while test ! -f "${cluster_config}"
        do read -ei "./configs/default.nix" -p "Enter path to configuration file (one of ./config/*.nix):  " cluster_config
           test -f "${cluster_config}" || log "ERROR: ${cluster_config} is not a readable file"
        done

        region="eu-central-1"
        export AWS_PROFILE=${CLUSTER_KIND} AWS_REGION=${region}
        deployer_tags="tag:deployment,Values=${CLUSTER_KIND},Name=tag:role,Values=deployer,Name=instance-state-name,Values=running"

        log "setting up AWS.."
        deployer_ip=$(region_tagged_instances_property ${region} ${deployer_tags} '[0].Instances[0].PublicIpAddress' | xargs echo)

        set +u
        ops_url="$1"
        ops_branch="$2"
        test -z "$1" && read -ei "http://github.com/input-output-hk/iohk-ops"       -p "Ops git repository URL: "    ops_url
        test -z "$2" && read -ei "$(git symbolic-ref HEAD | sed 's|refs/heads/||')" -p "Ops git repository branch: " ops_branch
        set -u

        log "setting up AWS credentials.."
        AKID="$(sed -n "/\\[${CLUSTER_KIND}\\]/,/^\\[.*/ p" < ~/.aws/credentials | grep aws_access_key_id     | cut -d= -f2 | xargs echo)"
        ssh "deployer@${deployer_ip}" sh -c "\"mkdir -p ~/.aws && cat > ~/.aws/credentials && chmod -R go-rwx ~/.aws\"" <<EOF
[default]
aws_access_key_id = ${AKID}
aws_secret_access_key = $(sed -n "/\\[${CLUSTER_KIND}\\]/,/^\\[.*/ p" < ~/.aws/credentials | grep aws_secret_access_key | cut -d= -f2 | xargs echo)
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
echo CLUSTER_KIND=${CLUSTER_KIND} > .config.sh && \
echo CLUSTER_TYPE=${CLUSTER_KIND}-infra > .config.sh && \
echo CLUSTER_NAME=infra > .config.sh && \
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
