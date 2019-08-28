# Benchmarking Guide

The purpose of this guide is to explain how to set up staging cluster,
run benchmark on it and collects the results.

## Login to Server

Please make sure that your SSH key is added to `ssh-keys.nix`
in [iohk-ops](https://github.com/input-output-hk/iohk-ops/).

Login to the staging server:

```bash
ssh staging@18.196.206.34
```

## Nix Scripts

The Nix scripts that are used are the following:

  * `set-rev.nix` - changes the revision (commit) of [cardano-sl](https://github.com/input-output-hk/cardano-sl/).
  * `set-cluster.nix` - sets up the cluster.
  * `run-bench.nix` - creates and executes a run.
  * `collect-data.nix` - collects data from the nodes and creates the plots. It's called by `run-bench.nix`.
  * `create-plots.nix` - creates the plots. It's called by `collect-data.nix`.
  * `destroy-cluster.nix` - destroys the cluster.
  * `set-wallets-cluster.nix` - sets up a cluster for wallets (edge nodes).

## Set up a New Cluster

Clone `iohk-ops` repository:

```bash
git clone https://github.com/input-output-hk/iohk-ops -b IOHK-OPS-BRANCH ISSUE-ID
```

where `IOHK-OPS-BRANCH` is the name of branch you need (default is `master`) and `ISSUE-ID`
is the name of directory `iohk-ops` will be cloned into (for example, `myissue`).

Now enter a `nix-shell`:

```bash
cd ISSUE-ID
nix-shell -A withAuxx
```

in the shell change to directory `benchmarks`:

```bash
cd benchmarks
```

Change the revision (commit hash) of [cardano-sl](https://github.com/input-output-hk/cardano-sl/) repo that is used,
for example:

```bash
$(nix-build scripts/set-rev.nix --argstr commit 51ad7c0503b1c52a75a6eb36096c407934136468)/bin/set-rev.sh
```

Exit from `nix-shell` and enter it again to have `nix` build the project:

```bash
exit
nix-shell -A withAuxx
cd benchmarks
```

Set a cluster and choose a name for it
(please note that its name should be in **lowercase** as referred into [iohk-ops Developer Clusters Guide](https://github.com/input-output-hk/iohk-ops/blob/master/docs/Developer-clusters-HOWTO.md#steps),
for example `benchmarkcluster`:

```bash
$(nix-build scripts/set-cluster.nix --argstr clusterName benchmarkcluster)/bin/set-cluster.sh
```

Please note that the step `Dumping generated genesis secrets into keys/generated-keys`
will take approx. 30 minutes.

In case of success the last outout lines will be:

```bash
...
benchmarkcluster> deployment finished successfully
Triggering commit monitor on c-a-1
Done.
```

After that you can see a new cluster in the list of working clusters:

```bash
nixops list
```

```
+--------------------------------------+-------------------+---------------------+------------+-----------+
| UUID                                 | Name              | Description         | # Machines |    Type   |
+--------------------------------------+-------------------+---------------------+------------+-----------+
| ...                                  | ...               | ...                 |        ... |    ...    |
| 3181ada3-c531-11e9-9268-06********60 | benchmarkcluster  | Cardano Development |         17 |    ec2    |
| ...                                  | ...               | ...                 |        ... |    ...    |
```

## Run a Benchmark

There's a top level benchmark script `bm-ci.sh` which calls `run-bench.nix` (see above).
Example of its content is following:

```bash
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
    --argstr relayPolicy policy_relay_relaxed.yaml   \
    --argstr edgeNodes     0       \
    --arg walletsDeployment \"\"   \
    )"/bin/run-bench.sh

#    --arg    walletsDeployment \"edgenodes-cluster\" \
```

All arguments (i.e. values after `--argstr`) are configuration parameters:

  * `coreNodes` - the number of core nodes.
  * `startWaitTime` - how many minutes to wait for the cluster to start.
  * `txsPerThread` - number of transactions that each thread should send.
  * `conc` - number of each generator's threads.
  * `delay` - number of milliseconds to wait between each transaction send.
  * `cooldown` - number of minutes to wait for cooldown.
  * `addGenerators` - using more than one generator increases the load for stress-tests; total number of generators = 1 + _addGenerators_
  * `edgeNodes` - the number of edge nodes (wallets), must be a multiple of 10.
  * `walletsDeployment` - edge nodes' deployment.

There're additional parameters as well:

  * `corePolicy` - policy file for core nodes.
  * `relayPolicy` - policy file for relay nodes.

The _relayPolicy_ parameter is important to relax the DDOS protection and increase the rate limit of transactions reaching the core nodes: `rateLimit: 99`.

Change parameters if required and run the script:

```bash
./bm-ci.sh
```

## Benchmark Results

### Collecting Results

After run is finished results are placed in `experiments/{timestamp of run}/` directory.

Plots are placed in `experiments/{timestamp of run}/plots/` directory.

The run's timestamp is taken when the benchmarking results are copied from the nodes.

### Storing Full Results

There's a directory on Google Drive - [Cardano Benchmarks](https://drive.google.com/drive/u/1/folders/0B6EeS_B4b4paUXU4ck1HdjBYMWM).
It stores benchmark results, each result is placed in a subdirectory with the name `run-DATE_OF_RUN`, for example `run-2019-03-05_005103`.
Usually it contains:

  * `*.tar.xz` archive
  * `plots/` directory with plots

Copy the results from `experiments/*` directories using `scp -r` and place them there.

### Storing Summary Data

There's a file `Benchmark Summary Data` (in the same directory on Google Drive) which contains a list of extracted results from benchmarking.
A new entry should be added for each new benchmark. There are following values:

  * Date of benchmark (for example, `2019-03-05_005103`).
  * `TPS` - Cardano transactions per second rate.
  * `TXS/THREAD` - number of transactions that each thread should send.
  * `CONC` - number of each generator's threads.
  * `GENS` - number of generators that will be used (how many executables will be launched).
  * `DELAY (ms)` - number of milliseconds to wait between each transaction send.
  * `(relay to core)` - threshold for number of messages sent from a relay node to a core node per second.
  * `(relay to relay)` - threshold for number of messages sent from a relay node to another relay node per second.
  * `IN BC` - number of transactions that are successfully included into the blockchain (successfully submitted and accepted).
  * `DISCARDED` - number of transactions that were submitted but rejected.
  * `FORKED` - number of transactions that are **NOT** successfully included into the blockchain (they're in a fork of the valid blockchain).
  * `COMMIT` - revision (commit) of [cardano-sl](https://github.com/input-output-hk/cardano-sl/).
  * `VERSION` - version of [cardano-sl](https://github.com/input-output-hk/cardano-sl/).

## Set up a wallets (edge nodes) cluster

To set up a wallets cluster:

```bash
$(nix-build scripts/set-wallets-cluster.nix --argstr clusterName edgenodes-cluster)/bin/set-wallets-cluster.sh
```

## Destroy the Cluster

Since a working cluster is spending computer resources, please do not leave such a cluster idle if it is not needed anymore.
To destroy the cluster use `destroy-cluster.nix`.
First, go to the root of your working directory (leaving `benchmarks/`):

```bash
cd ..
```

Then, call the script:

```
$(nix-build benchmarks/scripts/destroy-cluster.nix --argstr clusterName benchmarkcluster)/bin/destroy-cluster.sh
```

where _benchmarkcluster_ is the name of cluster.

Verify with `nixops list` that the cluster has been removed.

## Face Some Errors

If an error occurred during the benchmark run and stopped, its results can be collected with the following command:

```bash
$(nix-build scripts/collect-data.nix \
--argstr coreNodes     7             \
--argstr startWaitTime 10            \
--argstr txsPerThread  500           \
--argstr conc          1             \
--argstr delay         250           \
--argstr cooldown      10            \
--argstr addGenerators 6             \
--argstr edgeNodes     0             \
--arg walletsDeployment  \"\"     \
)/bin/collect-data.sh
```

Collecting the data with `collect-data.nix` requires the _edgeNodes_ argument to be set.
If edge nodes are not used, the argument must be set to _0_. Likewise for _walletsDeployment_

