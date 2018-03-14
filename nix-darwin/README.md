# Configuring a MacOS X Buildkite Agent

This directory contains an install script and configuration for
running [buildkite-agent](https://github.com/buildkite/agent) on a
MacOS X system.

## Requirements

* The Mac needs SSH enabled.

* The Mac needs to be running **OS X El Capitan 10.11.6**. Our GHC
  builds do not work with Sierra or higher.

## Setting up `nix-darwin`

1. Install [nix](https://nixos.org/nix/)

       curl https://nixos.org/nix/install | sh
       source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

2. Run the install script.

       $(nix-build --no-out-link -I iohk-ops=https://github.com/input-output-hk/iohk-ops/archive/master.tar.gz '<iohk-ops/nix-darwin>')/bin/deploy buildkite

   This will download the necessary dependencies then install and configure `nix-darwin`.

   It will ask for the admin password so that it can `sudo`.

   It should complete without errors.

3. Log out and log in again. This will ensure all the necessary
   environment settings work.

## What just happened?

The configuration is stored in
`/Users/admin/.nixpkgs/darwin-configuration.nix`.

The [nix-darwin](https://github.com/LnL7/nix-darwin) version tracks
its master branch. The [nixpkgs](https://github.com/NixOS/nixpkgs)
version tracks the `nixpkgs-17.09-darwin` (which is more or less the
same as the `nixos-17.09` [channel](https://nixos.org/channels/)).

The `buildkite-agent` package is actually plucked from a revision on
the `nixos-unstable` branch because there is no build for Mac in
17.09.

## After that

Set up the `/Users/admin/buildkite` directory with the necessary
secrets as follows.

1. The following variables are required in
   `/Users/admin/buildkite/buildkite_aws_creds` for artifact uploads
   to work:

       ```
       export BUILDKITE_S3_ACCESS_KEY_ID=AK...
       export BUILDKITE_S3_SECRET_ACCESS_KEY=...
       export BUILDKITE_S3_DEFAULT_REGION=...
       ```

2. Put the agent token into `buildkite_token`.

Since `launchd` tries to restart dead services continuously,
`buildkite-agent` should start working once the
`/Users/admin/buildkite/buildkite_token` file is set up.

The agent should appear on the Buildkite agents page.

The service log file is`/var/lib/buildkite-agent/buildkite-agent.log`.

## Installer Package Signing

So that packaging signing works, follow the instructions in the
[Installer Signing Certificate Setup section on the Wiki][1].

[1]: https://github.com/input-output-hk/internal-documentation/wiki/Configuring-a-macOS-%28darwin%29-build-slave-for-hydra#installer-signing-certificate-setup

## Administration

The `deploy.hs` script is meant to be idempotent. It will overwrite
`~/.nixpkgs/darwin-configuration.nix` and rebuild the system. Re-run
the deploy command to refresh the configuration.

To speed up re-deploys, pass the `--update` option to the script. This
will skip the nix-darwin install step.

### Debugging

To make changes to the system (e.g. changing the build queue), you can
edit `~/.nixpkgs/darwin-configuration.nix` and run:

    darwin-rebuild switch
