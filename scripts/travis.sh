#!/bin/sh

set -eu

set -x; NIXOPS=${1:-$(nix-build -A nixops)/bin/nixops};             set +x; shift || true
set -x; IOHK_OPS=${1:-$(nix-build -A iohk-ops)/bin/iohk-ops};         set +x; shift || true
set -x; CLEANUP_DEPLOYS=${1:-true};      set +x; shift || true
set -x; CLEANUP_CONFIGS=${1:-true};      set +x; shift || true
set -x; WITH_STAGING=${1:-true};         set +x; shift || true
set -x; WITH_PRODUCTION=${1:-true};      set +x; shift || true
set -x; WITH_DEVELOPMENT=${1:-};     set +x; shift || true     ## XXX: stop-gap fix for CI
set -x; WITH_EXPLORER=${1:-true};        set +x; shift || true
set -x; WITH_REPORT_SERVER=${1:-true};   set +x; shift || true
set -x; WITH_INFRA=${1:-true};           set +x; shift || true

# 0. Check all scripts compile
nixops --version
nix-shell --run "./scripts/aws.hs --help"
${IOHK_OPS} --help

# 1. check all packages build
nix-instantiate jobsets/cardano.nix --show-trace

# 2. check all environments evaluate
CLEANUP_DEPLS=""
cleanup() {
        set +xe
        for depl in ${CLEANUP_DEPLS}
        do
                test -z "${CLEANUP_DEPLOYS}" ||
                        ${IOHK_OPS} --config ${depl}'.yaml' destroy delete >/dev/null 2>&1
                test -z "${CLEANUP_CONFIGS}" ||
                        rm -f                ${depl}'.yaml'
        done
}
trap cleanup EXIT

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
