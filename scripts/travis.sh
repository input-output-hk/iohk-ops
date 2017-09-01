#!/bin/sh

set -xe

IOHK_OPS=${1:-iohk-ops}
NIXOPS=${2:-nixops}
WITH_CLEANUP=${3:-true}
WITH_STAGING=${4:-true}
WITH_PRODUCTION=${5:-true}
WITH_DEVELOPMENT=${6:-true}
WITH_EXPLORER=${7:-true}
WITH_REPORT_SERVER=${8:-true}
WITH_INFRA=${9:-true}

# 0. Check all scripts compile
# nixops --version
# nix-shell --run "./scripts/aws.hs --help"
# ${IOHK_OPS} --help

# # 1. check all packages build
# nix-instantiate jobsets/cardano.nix --show-trace

# 2. check all environments evaluate
CLEANUP_DEPLS=""
cleanup() {
        set +xe
        for depl in ${CLEANUP_DEPLS}
        do
                ${IOHK_OPS} --config ${depl}'.yaml' destroy delete >/dev/null 2>&1
                rm -f                ${depl}'.yaml'
        done
}
if test -n "${WITH_CLEANUP}"
then trap cleanup EXIT
fi

banner() {
        echo -e "--\n--\n--  $*\n--\n--\n"
}

COMMON_OPTIONS="--nixops ${NIXOPS} --deployer-ip 0.0.0.0 --topology topology-min.yaml"
CARDANO_COMPONENTS="Nodes ${WITH_EXPLORER:+Explorer} ${WITH_REPORT_SERVER:+ReportServer}"

if test -n "${WITH_STAGING}"; then
CLEANUP_DEPLS+=' test-stag'
${IOHK_OPS} template  --config 'test-stag.yaml'   --environment staging    ${COMMON_OPTIONS} 'test-stag'    ${CARDANO_COMPONENTS}
${IOHK_OPS} --verbose --config 'test-stag.yaml'   create deploy --evaluate-only
banner 'Staging env evaluated'
fi

if test -n "${WITH_PRODUCTION}"; then
CLEANUP_DEPLS+=' test-prod'
${IOHK_OPS} template  --config 'test-prod.yaml'   --environment production ${COMMON_OPTIONS} 'test-prod'    ${CARDANO_COMPONENTS}
${IOHK_OPS} --verbose --config 'test-prod.yaml'   create deploy --evaluate-only
banner 'Production env evaluated'
fi

if test -n "${WITH_DEVELOPMENT}"; then
CLEANUP_DEPLS+=' test-devo'
${IOHK_OPS} template  --config 'test-devo.yaml'                            ${COMMON_OPTIONS} 'test-devo'    ${CARDANO_COMPONENTS}
${IOHK_OPS} --verbose --config 'test-devo.yaml'   create deploy --evaluate-only
banner 'Development env evaluated'
fi

if test -n "${WITH_INFRA}"; then
CLEANUP_DEPLS+=' test-infra'
${IOHK_OPS} template  --config 'test-infra.yaml'  --environment production ${COMMON_OPTIONS} 'test-infra'   Infra
${IOHK_OPS} --verbose --config 'test-infra.yaml'  create deploy --evaluate-only
banner 'Infra evaluated'
fi
