#!/bin/sh

set -xe

IOHK_OPS=${1:-iohk-ops}

nix-shell --run "./scripts/aws.hs --help"
nixops --version

# check all scripts compile
${IOHK_OPS} --help

# check all packages build
nix-instantiate jobsets/cardano.nix --show-trace

# check deploy evaluations
${IOHK_OPS} --testing template --environment production --config 'csl-production.yaml'       'csl-production'       Nodes Explorer ReportServer
${IOHK_OPS} --testing template --environment staging    --config 'csl-staging.yaml'          'csl-staging'          Nodes Explorer ReportServer
${IOHK_OPS} --testing template                          --config 'csl.yaml'                  'csl'                  Nodes Explorer ReportServer
${IOHK_OPS} --testing template --environment staging    --config 'csl-explorer-staging.yaml' 'csl-explorer-staging'       Explorer
${IOHK_OPS} --testing template --environment production --config 'inf.yaml'                  'inf'         Infra
${IOHK_OPS}                                   --verbose --config 'csl-production.yaml'       create deploy --evaluate-only
${IOHK_OPS}                                   --verbose --config 'csl-staging.yaml'          create deploy --evaluate-only
${IOHK_OPS}                                   --verbose --config 'csl.yaml'                  create deploy --evaluate-only
${IOHK_OPS}                                   --verbose --config 'csl-explorer-staging.yaml' create deploy --evaluate-only
${IOHK_OPS}                                   --verbose --config 'tw.yaml'                   create deploy --evaluate-only
${IOHK_OPS}                                   --verbose --config 'inf.yaml'                  create deploy --evaluate-only
