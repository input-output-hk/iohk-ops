# Configuring a MacOS X Buildkite Agent

This directory contains an install script and configuration for
running [buildkite-agent](https://github.com/buildkite/agent) on a
MacOS X system.

There are *a few manual steps* required on the target mac. After that,
deployments and redeployments are done through SSH from a *deployment
host* (your laptop probably).

## Requirements

## Target Mac

* The Mac needs SSH enabled.

* The Mac needs to be running **OS X El Capitan 10.11.6**. Our GHC
  builds do not work with Sierra or higher.

## Deployment host

* Needs a clone of `iohk-ops` somewhere.

* Set up entries for the macs in `~/.ssh/config`.

## Setting up `nix-darwin`

### On the target Mac

1. Install [nix](https://nixos.org/nix/)

       curl https://nixos.org/nix/install | sh
       source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

2. Run the prepare script.

       $(nix-build --no-out-link -I iohk-ops=https://github.com/input-output-hk/iohk-ops/archive/master.tar.gz '<iohk-ops/nix-darwin>')/bin/prepare buildkite

   This will prepare the system so that `nix-darwin` can be installed.

   It will ask for the admin password so that it can `sudo`.

   It will take a while to start because it needs to download GHC to
   run, but should complete without errors.

3. Set up the `/Users/admin/buildkite` directory with the necessary
   secrets as follows:

   a. The following variables are required in
      `/Users/admin/buildkite/buildkite_aws_creds` for artifact uploads
      to work:

      ```
      export BUILDKITE_S3_ACCESS_KEY_ID=AK...
      export BUILDKITE_S3_SECRET_ACCESS_KEY=...
      export BUILDKITE_S3_DEFAULT_REGION=...
      ```

   b. Put the agent token into `/Users/admin/buildkite/buildkite_token`.

4. Put the agent token for DataDog in `/Users/admin/.datadog_api_key`.

### From the deployment host

1. `cd iohk-ops/nix-darwin`
2. `./deploy.hs --role ./roles/buildkite-agent.nix HOSTS...`

Replace *HOSTS* with the ssh host name of the target mac(s).

Re-run this command as necessary to update the configuration of the Mac.

## What just happened?

It built a `nix-darwin` system from the given configuration and
activated it on the target mac.

The agent should appear on the Buildkite agents page.

The service log file is`/var/lib/buildkite-agent/buildkite-agent.log`.

The Mac should be registered in DataDog and sending stats.

Check that the services are running with `sudo launchctl list | grep org.nixos`.


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

## Installer Package Signing

So that packaging signing works, follow the instructions in the
[Installer Signing Certificate Setup section on the Wiki][1].

[1]: https://github.com/input-output-hk/internal-documentation/wiki/Configuring-a-macOS-%28darwin%29-build-slave-for-hydra#installer-signing-certificate-setup
