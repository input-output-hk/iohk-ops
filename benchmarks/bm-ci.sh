#!/bin/sh

set -e        # exit on error
set -o xtrace # print commands

export TZ=UTC
export TEMPDIR=/tmp/
export TMPDIR=/tmp/
export TEMP=/tmp/
export TMP=/tmp/

"$(nix-build ./scripts/run-bench.nix\
    --argstr coreNodes     7       \
    --argstr startWaitTime 10      \
    --argstr txsPerThread  4000    \
    --argstr conc          1       \
    --argstr delay         500     \
    --argstr cooldown      10      \
    --argstr addGenerators 1       \
    --argstr edgeNodes     0       \
    --arg walletsDeployment \"\"   \
    )"/bin/run-bench.sh

#    --arg    walletsDeployment \"edgenodes-cluster\" \
