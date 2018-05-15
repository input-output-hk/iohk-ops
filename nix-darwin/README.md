# Configuring a MacOS X Buildkite Agent

This directory contains an install script and configuration for
running build slave on a macOS system. There are currently two roles:

 * [Buildkite Agent][agents]
 * [Hydra build slave][machines]

There are *a few manual steps* required on the target mac. After that,
deployments and redeployments are done through SSH from a *deployment
host* (your laptop probably).

[agents]: https://buildkite.com/organizations/input-output-hk/agents
[machines]: https://hydra.iohk.io/machines

## Requirements

## Target Mac

* The Mac needs SSH enabled.

## Deployment host

* Needs a clone of `iohk-ops` somewhere.

* Set up entries for the macs in `~/.ssh/config` and make sure you
  have confirmed the host keys.

## Setting up `nix-darwin`

### On the target Mac

1. Install [nix](https://nixos.org/nix/)

       curl https://nixos.org/nix/install | sh
       source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

2. Run the prepare script. Specify on the command line whether it will
   be buildkite or hydra.

       nix-build -I iohk-ops=https://github.com/input-output-hk/iohk-ops/archive/develop.tar.gz '<iohk-ops/nix-darwin>'
       ./result/bin/deploy [ buildkite | hydra ]

   This will prepare the system so that `nix-darwin` can be installed.

   It will ask for the admin password so that it can `sudo`.

   It will take a while to start because it needs to download GHC to
   run, but should complete without errors.

3. Put the agent token for DataDog in
   `/Users/admin/.datadog_api_key`. Get this token from another Mac.

4. (Buildkite only) Set up the `/Users/admin/buildkite` directory with
   the necessary secrets as follows:

   a. The following variables are required in
      `/Users/admin/buildkite/buildkite_aws_creds` for artifact uploads
      to work:

      ```
      export BUILDKITE_S3_ACCESS_KEY_ID=AK...
      export BUILDKITE_S3_SECRET_ACCESS_KEY=...
      export BUILDKITE_S3_DEFAULT_REGION=...
      ```

   b. Put the agent token into `/Users/admin/buildkite/buildkite_token`.


### From the deployment host

1. `cd iohk-ops/nix-darwin`
2. `./deploy.hs --role ./roles/ROLE.nix HOSTS...`

Replace *HOSTS* with the ssh host name of the target mac(s).

Replace *ROLE* with `buildkite-agent` or `hydra-slave` as necessary.

Re-run this command as necessary to update the configuration of the Mac.

## What just happened?

It built a `nix-darwin` system from the given configuration and
activated it on the target mac.

The Mac should be registered in DataDog and sending stats.
The datadog logs are in `/var/log/datadog`.

Check that all necessary services are running with
`sudo launchctl list | grep org.nixos`.

### Buildkite

The agent should appear on the [Buildkite agents][agents] page.

The service log file is`/var/lib/buildkite-agent/buildkite-agent.log`.

### Hydra

You should be able to register this mac in your local `nix.buildMachines` and check that it builds things. For example:

    nix-build -E '(import <nixpkgs> { system = "x86_64-darwin"; }).pkgs.hello.overrideAttrs (oldAttrs: { doCheck = false; })'

## Details

The top-level configurations are in the [`roles`](./roles/)
subdirectory.

This configuration imports various configuration fragments from the
[`modules`](./modules/) subdirectory.

The expression to build a `nix-darwin` configuration with pinned
versions is in [`lib/build.nix`](./lib/build.nix).

The `nix-darwin` version is specified in
[`lib/nix-darwin.json`](./lib/nix-darwin.json) and the `nixpkgs`
revision in pinned in [`lib/nixpkgs.json`](./lib/nixpkgs.json).

The `buildkite-agent` package is actually plucked from a revision on
the `nixos-unstable` branch because the version in 18.03 is too old.

The `dd-agent` module is copied from NixOS and modified to work on a
Mac. It will need a bit more cleaning up before it could be accepted
into `nix-darwin` upstream.

## After deploying

### Buildkite agent: Installer Package Signing Key

So that packaging signing works, follow the instructions in the
[Installer Signing Certificate Setup section on the Wiki][1].

[1]: https://github.com/input-output-hk/internal-documentation/wiki/Configuring-a-macOS-%28darwin%29-build-slave-for-hydra#installer-signing-certificate-setup

### Hydra slave: Build Machines Setup

1. Register the host with the Hydra master by adding it to
   `nix.buildMachines` in
   [`../modules/hydra-master.nix`](../modules/hydra-master.nix).

2. After change is merged, redeploy Hydra (see [Operational Manual](https://github.com/input-output-hk/internal-documentation/wiki/Operational-Manual#hydraiohkio-and-cardano-deployer)).

3. After the poll interval (something like 5 mintes), the build slave will appear on the [Hydra machines][machines] page.
