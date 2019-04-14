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

load_config() {
    if [ ! "$CONFIGURED" = "true" ]
       then
            log "Loading .config.sh"
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
            if test -z "${GAC_RECURSIVE}"
            then log ".config.sh settings:"
                 cat <<EOF
CLUSTER_KIND=${CLUSTER_KIND}
CLUSTER_TYPE=${CLUSTER_TYPE}
CLUSTER_NAME=${CLUSTER_NAME}
CONFIG=${CONFIG}
EOF
            fi
            NODE_DERIVATION=${CLUSTER_KIND}
            NODE_EXECUTABLE=${CLUSTER_KIND}
            DEFAULT_NODE=${CLUSTER_KIND}-a-0
            NODE_SERVICE=${CLUSTER_KIND}
            NODE_DB_PATH=/data/${CLUSTER_KIND}
            ALL_NODES="${DEFAULT_NODE} ${CLUSTER_KIND}-a-1 ${CLUSTER_KIND}-b-0 ${CLUSTER_KIND}-b-1 ${CLUSTER_KIND}-c-0 "
            TLS_CERT_DIR="$(pwd)/tls-cert"
            TLS_CERT="${TLS_CERT_DIR}/cert.pem"
            TLS_CERT_KEY="${TLS_CERT_DIR}/key.pem"
            set -u
            CONFIGURED="true"
    fi
}

if test -n "GAC_RUNNING"
then GAC_RECURSIVE=yes
fi
export GAC_RUNNING=yes
log() {
        echo "==(  $*" >&2
}

while test -n "$1"
do case "$1" in
           --verbose | -v ) set -x; verbose="--verbose";;
           --help | "--"* ) usage; exit 1;;
           * ) break;;
   esac; shift; done
self="$(realpath $0) ${verbose}"
cmd=${1:-doit}; test -n "$1"; shift

###
### Overlay root detection: ${PWD}, $(dirname $self)
###
gacroot="$(realpath $0 | xargs dirname | xargs echo)"
if test -d "$(dirname $0)/clusters"
then overlayroot="$(dirname $0)"
else overlayroot="${gacroot}"
fi

###
### Overlay/defaults: ${PWD}/.defaults
###
if test -f "${overlayroot}/.gac.defaults"
then .     "${overlayroot}/.gac.defaults"
else .         "${gacroot}/.gac.defaults"
fi

###
### Overlay/command overrides: ${overlayroot}/gac-${cmd}.sh defines 'gac ${cmd}'
###
local_override="${overlayroot}/gac-${cmd}.sh"
central_override="${gacroot}/gac-${cmd}.sh"
if    test -x "${local_override}"
then override="${local_override}"
elif  test -x "${central_override}"
then override="${central_override}"
else override=
fi
if test -x "${override}"
then log "running an override for 'gac ${cmd}':  ${override}"
     GAC_CENTRAL=$self
     . ${override} "$@"
     exit $? ## set -e does this already, so adding just for clarity.
fi

load_config

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

## Are we in overlay mode?
if test "$(pwd)" != "${gacroot}"
then log "overlay mode ON"
     OVERLAY_MODE=yes
else OVERLAY_MODE=
fi

nix_opts="\
--max-jobs 4 --cores 0 --show-trace \
-I nixpkgs=${nixpkgs_out} \
"
nixops_nix_opts="${nix_opts} \
-I nixops=${nixops_out}/share/nix/nixops \
-I config=./configs/${CONFIG}.nix \
-I configs=./configs \
-I module=${gacroot}/modules \
-I static=./static \
-I goguen=${gacroot}/goguen \
${OVERLAY_MODE:+-I local-module=./modules} \
${OVERLAY_MODE:+-I iops=./iops} \
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

        mkdir -p "static"
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

generate_self_signed_tls_cert () {
    rm -rf ${TLS_CERT_DIR}
    mkdir -p ${TLS_CERT_DIR}
    openssl=$(nix-build --no-out-link -E '(import ./. {}).pkgs.openssl' | xargs echo)/bin/openssl
    ${openssl} req -x509 -newkey rsa:4096 -nodes -keyout "${TLS_CERT_KEY}" -out \
               "${TLS_CERT}" -days 365 -subj '/CN=localhost'
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

	if [[ -z $AKID ]]; then
		AKID="$(grep aws_access_key_id ~/.aws/credentials | cut -d= -f2 | xargs)"
	fi

	if [[ -z $AKID ]]; then
                read -ep "Use AWS access key ID: " AKID
	fi

        if ! ( [ -f "${TLS_CERT}" ] || [ -f "${TLS_CERT_KEY}" ] )
           then log "generating self-signed TLS certificate"
                generate_self_signed_tls_cert
        fi
        ${nixops} set-args ${nixops_subopts} \
                  --argstr accessKeyId "${AKID}" \
                  --argstr deployerIP "${deployerIP}" \
	          --argstr clusterName "${CLUSTER_NAME}" \
		  --arg config "import ${gacroot}/config.nix { \
                                  clusterName = \"${CLUSTER_NAME}\"; \
                                  deployerIP = \"${deployerIP}\"; \
                                  accessKeyId = \"${AKID}\"; \
                                  tlsCert = /. + \"${TLS_CERT}\"; \
                                  tlsCertKey = /. + \"${TLS_CERT_KEY}\";
                                }"
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
		$self create
	fi
        $self     components
        ${nixops} check    ${nixops_subopts} || true # <- nixops check returns non-zero status when resources are missing but it still updates the state. so we don't want this to stop us.
        ${nixops} modify   ${nixops_subopts} clusters/${CLUSTER_TYPE}/*.nix
        ${nixops} deploy   ${nixops_subopts_deploy} "$@";;
"" | "" ) # Doc:
        ;;
"" | building-derivations ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
update-pin | update | pin | update-a-goguen-package-pin ) # Doc:
        ${gacroot}/goguen/update-pin.sh "$@";;
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
        $self        ssh-all ${on} -- systemctl    stop ${NODE_SERVICE};;
start | start-all-node-services ) # Doc:
        on=${1:+--include $*}
        log "starting node services on:  ${on}"
        $self        ssh-all ${on} -- systemctl   start ${NODE_SERVICE}; log "new start date:  $($self since ${DEFAULT_NODE})";;
restart | restart-all-node-services ) # Doc:
        on=${1:+--include $*}
        $self        ssh-all ${on} -- systemctl restart ${NODE_SERVICE}; log "new start date:  $($self since ${DEFAULT_NODE})";;
statuses | ss | all-node-service-statuses ) # Doc:
        on=${1:+--include $*}
        $self        ssh-all ${on} -- systemctl  status ${NODE_SERVICE};;
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
        set +u; since=${2:-$($self since ${node})}; set -u
        if test -z "${since}"; then since='3 hours ago'; fi
        ${nixops} ssh          ${nixops_subopts} ${node} -- journalctl  -u ${NODE_SERVICE} --since "'${since}'" 2>&1;;
pretty-journal-on | pj | node-service-journal-on-node-since-its-start-prettified-version ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        $self node-service-journal-on-node-since-start ${node} | fgrep -v ',"state": "ok","service": "' | cut -c92- | less;;
journal    | jall | j | all-node-service-journals ) # Doc:
        start_sample_node=${DEFAULT_NODE}
        since=${1:-$($self since ${start_sample_node})}
        if test -z "${since}"; then since='3 hours ago'; fi
        log "journal since:  ${since}"
        $self        ssh-all      -- journalctl -u ${NODE_SERVICE} --since "'${since}'" 2>&1;;
system-journal | sysj | system-journal-on-node ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        $self        ssh ${node}  -- journalctl -xe 2>&1;;
follow | f | follow-node-service-journal-on ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        $self        ssh ${node}  -- journalctl -fu ${NODE_SERVICE} 2>&1;;
follow-all | fa | follow-all-node-service-journals ) # Doc:
        $self        ssh-all      -- journalctl -fu ${NODE_SERVICE} "$@" 2>&1;;
grep-on | gro | grep-node-service-journals-since-start ) # Doc:
        node=$1; shift
        log "node:           ${node}"
        log "filter:         ag $*"
        ag=$( nix-build --no-out-link -E '(import ./. {}).pkgs.ag'     | xargs echo)/bin/ag
        $self        journal-on ${node} 2>&1 | ${ag} "$@";;
grep-since | gras | grep-all-node-service-journals-since-timestamp ) # Doc:
        since=$1; shift
        log "journal since:  ${since}"
        log "filter:         ag $*"
        ag=$( nix-build --no-out-link -E '(import ./. {}).pkgs.ag'     | xargs echo)/bin/ag
        $self        journal "${since}" 2>&1 | ${ag} "$@";;
grep | g | grep-all-node-service-journals-since-last-restart ) # Doc:
        start_sample_node=${DEFAULT_NODE}
        since=$($self since ${start_sample_node})
        if test -z "${since}"; then since='3 hours ago'; fi
        $self        grep-since ${since} "$@";;
watch-for | w | watch-all-node-service-journals-for-regex-occurence ) # Doc:
        ag=$( nix-build --no-out-link -E '(import ./. {}).pkgs.ag'     | xargs echo)/bin/ag
        $self        follow-all 2>&1 | ${ag} "$@";;

"" | "" ) # Doc:
        ;;
"" | service-logs-interpretation ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
blocks | bs |  node-recent-block-numbers ) # Doc:
        $self        grep-since "'30 seconds ago'" 'Best Block: ' | cut -d' ' -f5,14-;;
roles | rs | node-roles-since-start ) # Doc:
        $self        grep 'role changed|LEADER';;
exceptions | ex | node-exceptions-since-start ) # Doc:
        $self        grep -i exception | ag -v 'RiemannBatchClient|exception.counter';;
watch-blocks | wb | watch-node-current-block-number-stream ) # Doc:
        $self        watch-for 'Best Block: ' | cut -d' ' -f5,14-;;
watch-roles | wr | watch-node-current-role-stream ) # Doc:
        $self        watch-for 'role changed|LEADER';;
watch-roles-pretty | wrp | watch-node-current-role-stream-prettified ) # Doc:
        $self        watch-roles | sed 's/^\([^\.]*\)\..*oldRole","value": "\([A-Z]*\)".*newRole","value": "\([A-Z]*\).*$/\1: \2 -> \3/';;
watch-exceptions | we | watch-node-current-exception-stream ) # Doc:
        $self        watch-for -i exception | ag -v 'RiemannBatchClient|exception.counter';;
troubleshoot | tro | troubleshoot-node-logs-for-known-problems ) # Doc:
        node=${1:-${DEFAULT_NODE}}
        $self        grep-node-service-journals-since-start ${node} 'Genesis data present in the database does not match genesis block from file.' && echo 'PROBLEM FOUND!' || true
        $self        grep-node-service-journals-since-start ${node} 'candidate is not known to the local member' && echo 'PROBLEM FOUND!' || true
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
        $self        stop
        log "nuking.."
        $self        ssh-all      -- rm -rf ${NODE_DB_PATH}
        $self        start;;
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
help | show-this-help-message ) # Doc: Show this help message
        usage; exit 1;;
* ) usage; exit 1;;
esac
