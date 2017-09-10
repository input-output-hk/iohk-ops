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

Append https://github.com/input-output-hk/pos-prototype-deployment/blob/master/lib.nix#L46 and submit a PR.
