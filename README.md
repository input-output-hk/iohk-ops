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


### Usage

   $(nix-build -A iohk-ops)/bin/iohk-ops --help


### Getting SSH access

1. Fork https://github.com/input-output-hk/iohk-ops
2. Check out the `master` branch
3. Add your key:
    - contents in the developer section at https://github.com/input-output-hk/iohk-ops/blob/master/ssh-keys.nix, under an appropriate name, at an alphabetically-appropriate position,
    - key name in the `devKeys` list https://github.com/input-output-hk/iohk-ops/blob/master/lib.nix#L63, again at the alphabetically-appropriate position.
4. Submit a PR against `master` and let DevOps know.
5. Wait until the DevOps team deploys the infrastructure cluster.
