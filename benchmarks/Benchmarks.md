## Scripts

The scripts that are used are the following:

  * set-cluster.nix    , which sets up the cluster.
  * run-bench.nix      , which creates and executes a run.
  * collect-data.nix   , which collects data from the nodes
                         and create the plots. It is called
                         by run-bench.nix.
  * create-plots.nix   , which creates the plots. It is
                         called by collect-data.nix.
  * destroy-cluster.nix, which destroys the current cluster.


## Steps

1. `SSH` to the staging jumpserver (`ssh staging@18.196.206.34`)

1. `git clone https://github.com/input-output-hk/iohk-ops -b IOHK-OPS-BRANCH ISSUE-ID` (~`iohk-ops clone ISSUE-ID [IOHK-OPS-BRANCH]` ~) -- _the branch defaults to `develop`_

1. `cd ISSUE-ID`

1. `nix-shell`

1. Set the revision of cardano-sl passing the commit number:
    ```
    $(nix-build set-rev.nix                                       \
    --argstr commit      517e7c7aa7e46bc584f309d423d2f18fd8d5365f \
    )/bin/set-rev.sh
    ```
1. ctrl-d and `nix-shell`

#change it and io set rev in ./pb-13 NOT in pb-13/benchmarks
1. To set up the cluster choose a name and a cardano-sl commit:
    ```
    $(nix-build set-cluster.nix                                   \
    --argstr clusterName benchmarks110policies                    \
    --argstr commit      517e7c7aa7e46bc584f309d423d2f18fd8d5365f \
    )/bin/set-cluster.sh
    ```

1. `export TMPDIR=/tmp`

1. To run a benchmark set the settings as in the following example:
    ```
    $(nix-build run-bench.nix     \
    --argstr coreNodes     7      \
    --argstr startWaitTime 10     \
    --argstr txsPerThread  500    \
    --argstr conc          1      \
    --argstr delay         250    \
    --argstr sendMode send-random \
    --argstr cooldown      10     \
    --argstr addGenerators 6      \
    --argstr edgeNodes     0      \
    )/bin/run-bench.sh
    ```

Results after run is finished are placed in
experiments/{date of run}/ folder. Plots are located in the
experiments/{date of run}/plots/.

Every time another run must take place one can just
repeat the last step changing or not the arguments.

To destroy the current cluster:

  `$(nix-build destroy-cluster.nix)/bin/destroy-cluster.sh`

If an error occurred during the run and stopped it results
can be collected with the following command:

  ```
  $(nix-build collect-data.nix    \
  --argstr coreNodes     7        \
  --argstr startWaitTime 10       \
  --argstr txsPerThread  500      \
  --argstr conc          1        \
  --argstr delay         250      \
  --argstr sendMode send-random   \
  --argstr cooldown      10       \
  --argstr addGenerators 6        \
  --argstr edgeNodes     0        \
  )/bin/collect-data.sh
  ```

Collecting the data with collect-data.nix requires the
edgeNodes argument (edge nodes) to be set. If edge nodes
are not used the argument must be set to zero.

If edgenodes-cluster is not set up at all then argument
walletsDeployment must set as an empty string as in the
following example:

  ```
  $(nix-build run-bench.nix     \
  --argstr coreNodes     7      \
  --argstr startWaitTime 10     \
  --argstr txsPerThread  500    \
  --argstr conc          1      \
  --argstr delay         250    \
  --argstr sendMode send-random \
  --argstr cooldown      10     \
  --argstr addGenerators 6      \
  --argstr edgeNodes     0      \
  --arg walletsDeployment  \"\" \
  )/bin/run-bench.sh
  ```
