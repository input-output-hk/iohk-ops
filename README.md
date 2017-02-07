Collection of tools to deploy Serokell infrastructure.

File structure

- `deployments` includes all NixOps deployments controlled via `Cardano*.hs` scripts
- `modules` has NixOS modules
- `lib.nix` warps upstream `<nixpkgs/lib.nix>` with our common functions
- `scripts` has bash scripts not converted to Haskell/Turtle into Cardano.hs yet
- `srk-nixpkgs.nix` is a collection of upstream Serokell packages
- `static` includes files being static, not generated
- `jobsets` is used by Hydra CI


### Getting started

Check out nixpkgs repository to the root (~/nixpkgs) using branch `release-16.09`.

### Compiling localy 

./Cardano.hs build

### Deploying changes

./Cardano.hs deploy

Note this will not remove machines if they no longer exist. 

### Removing machines

If you want to remove a machine simply remove them in the bottom part of the `nixops.nix` file.
Then run `nixops deploy` with `-k`:

./Cardano.hs deploy

### List all deployments

`nixops list`

### Infos about all machines (including public IPs)

`nixops info [-d deployment]`

### Connect to a machine

`nixops ssh -d deployment machine-name`

Replace `machine-name` with the machine you want to connect to (from `info` call).

### Destroy everything

./Cardano.hs destroy

### Building AMIs

./Cardano.hs ami

This will update `modules/amis.nix` so make sure to commit it.

### Getting SSH access

Append https://github.com/input-output-hk/pos-prototype-deployment/blob/master/lib.nix#L46 and submit a PR.
