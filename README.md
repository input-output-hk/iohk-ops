Collection of tools to deploy IOHK infrastructure.

### Structure

- `deployments` - includes all NixOps deployments controlled via `.hs` scripts
- `modules` - NixOS modules
- `lib.nix` - warps upstream `<nixpkgs/lib.nix>` with our common functions
- `scripts` - has bash scripts not converted to Haskell/Turtle into Cardano.hs yet
- `srk-nixpkgs.nix` - is a collection of Haskell packages
- `static` includes files using in deployments
- `jobsets` is used by Hydra CI


### Compiling localy

     $ ./CardanoCSL.hs build

### Deploying changes

    $ ./CardanoCSL.hs deploy

**Note this will not remove machines if they no longer exist.**

### Removing machines

If you want to remove a machine simply remove them in the bottom part of the `nixops.nix` file.
Then run `nixops deploy` with `-k`:

./Cardano.hs deploy

### List all deployments

    $ nixops list

### Information about all machines (including public IPs)

    $ nixops info -d <deployment-name>

### SSH to a single machine

    $ nixops ssh -d <deployment-name> <machine-name>

Replace `<machine-name>` with the machine you want to connect to (from `info` call).

### Destroy everything

    $ ./CardanoCSL.hs destroy

### Building AMIs

    $ ./CardanoCSL.hs ami

This will update `modules/amis.nix` so make sure to commit it.

### Getting SSH access

Append https://github.com/input-output-hk/pos-prototype-deployment/blob/master/lib.nix#L46 and submit a PR.
