[![Build status](https://badge.buildkite.com/5645abfe1411086f06a4d8cee1e3bbbbba9fb9318738f1fdb1.svg)](https://buildkite.com/input-output-hk/iohk-ops?theme=solarized)

Collection of tooling and automation to deploy IOHK infrastructure.

### Structure

- `deployments` - includes all NixOps deployments controlled via `.hs` scripts
- `modules` - NixOS modules
- `lib.nix` - wraps upstream `<nixpkgs/lib.nix>` with our common functions
- `scripts` - has bash scripts not converted to Haskell/Turtle into Cardano.hs yet
- `default.nix` - is a collection of Haskell packages
- `static` includes files using in deployments
- `jobsets` is used by Hydra CI
- `terraform` - other AWS infrastructure
- `nix-darwin` - deployment script and configurations for MacOS X machines

### Getting SSH access

1. Append https://github.com/input-output-hk/iohk-ops/blob/master/lib.nix#L83 and submit a PR.
2. Wait until the DevOps team deploys the infrastructure cluster.

## The `io` command

Sources for the `iohk-ops` tool are in the [`iohk`](./iohk) directory.

### Usage

After cloning this repo, start a `nix-shell`.

    % nix-shell
    [nix-shell:~/iohk/iohk-ops]$ io --help

### Development

To hack on the `iohk-ops` tool, use

    % nix-shell --arg io-dev-mode true
    [nix-shell:~/iohk/iohk-ops]$ type io
    io is a function
    io ()
    {
        runhaskell -iiohk iohk/iohk-ops.hs "$@"
    }
    [nix-shell:~/iohk/iohk-ops]$ io --help

This will provide a Haskell environment where you can use `io` to run
the script or `ghci` for development.

### Run from anywhere

    $(nix-build --no-out-link https://github.com/input-output-hk/iohk-ops/archive/develop.tar.gz -A iohk-ops)/bin/iohk-ops --help
