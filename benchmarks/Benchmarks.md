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
  * set-wallets-cluster.nix    , which sets up a cluster for wallets (edge nodes).


## Steps to set up a cluster

1. `SSH` to the staging jumpserver (`ssh staging@18.196.206.34`)

1. `git clone https://github.com/input-output-hk/iohk-ops -b IOHK-OPS-BRANCH ISSUE-ID` (~`iohk-ops clone ISSUE-ID [IOHK-OPS-BRANCH]` ~) -- _the branch defaults to `develop`_

1. `cd ISSUE-ID`

1. `cd benchmarks`

1. `nix-shell -A withAuxx`

1. Change the revision (commit) of `cardano-sl` repo that is used:
    ```
    $(nix-build scripts/set-rev.nix                          \
    --argstr commit 517e7c7aa7e46bc584f309d423d2f18fd8d5365f \
    )/bin/set-rev.sh
    ```

1. ctrl-d

1. `nix-shell`

1. Choose a name for the cluster:
    ```
    $(nix-build scripts/set-cluster.nix    \
    --argstr clusterName benchmarksCluster \
    )/bin/set-cluster.sh
    ```


## Run a benchmark

1. Set the parameters for a benchmarking round
   ```
   nano ./bm-ci.sh
   ```

1. Run bm-ci.sh:
    ```
    ./bm-ci.sh
    ```


## Results

Results after run is finished are placed in
experiments/{date of run}/ folder. Plots are located in the
experiments/{date of run}/plots/.

Every time another run must take place one can just
repeat the last step changing or not the arguments.


## Changing revision (commit) of cardano-sl

To change the revision (commit) of cardano-sl repo that is used:

  ```
  $(nix-build scripts/set-rev.nix                          \
  --argstr commit 517e7c7aa7e46bc584f309d423d2f18fd8d5365f \
  )/bin/set-rev.sh
  ```


## Set up a wallets (edge nodes) cluster

To set up a wallets cluster:

  ```
  $(nix-build scripts/set-wallets-cluster.nix  \
  --argstr clusterName edgenodes-cluster       \
  )/bin/set-wallets-cluster.sh
  ```


## Destroy the cluster

To destroy the current cluster:

  `$(nix-build destroy-cluster.nix)/bin/destroy-cluster.sh`


## Face some errors

If an error occurred during the run and stopped it results
can be collected with the following command:

  ```
  $(nix-build scripts/collect-data.nix \
  --argstr coreNodes     7             \
  --argstr startWaitTime 10            \
  --argstr txsPerThread  500           \
  --argstr conc          1             \
  --argstr delay         250           \
  --argstr cooldown      10            \
  --argstr addGenerators 6             \
  --argstr edgeNodes     0             \
  )/bin/collect-data.sh
  ```

Collecting the data with collect-data.nix requires the
edgeNodes argument (edge nodes) to be set. If edge nodes
are not used the argument must be set to zero.

If edgenodes-cluster is not set up at all then argument
walletsDeployment must set as an empty string as in the
following example:

  ```
  $(nix-build scripts/run-bench.nix \
  --argstr coreNodes     7          \
  --argstr startWaitTime 10         \
  --argstr txsPerThread  500        \
  --argstr conc          1          \
  --argstr delay         250        \
  --argstr cooldown      10         \
  --argstr addGenerators 6          \
  --argstr edgeNodes     0          \
  --arg walletsDeployment  \"\"     \
  )/bin/run-bench.sh
  ```
