## Steps

1. `SSH` to the staging jumpserver (`ssh staging@18.196.206.34`)

1. `git clone https://github.com/input-output-hk/iohk-ops -b IOHK-OPS-BRANCH ISSUE-ID` (~`iohk-ops clone ISSUE-ID [IOHK-OPS-BRANCH]` ~) -- _the branch defaults to `develop`_

1. `cd ISSUE-ID`

1. `nix-shell`

1. `$(nix-build set-cluster.nix                                   \
    --argstr clusterName benchmarks110policies                    \
    --argstr commit      4a1d1080535f0c693a83ca9941505a51ec8aa7e2 \
    )/bin/set-cluster.sh`

1. `$(nix-build run-bench.nix     \
    --argstr coreNodes     7      \
    --argstr startWaitTime 10     \
    --argstr time          500    \
    --argstr conc          1      \
    --argstr delay         250    \
    --argstr sendMode send-random \
    --argstr cooldown      10     \
    --argstr addGenerators 6      \
    )/bin/run-bench.sh`


To destroy the current cluster:

`$(nix-build destroy-cluster.nix)/bin/destroy-cluster.sh`
