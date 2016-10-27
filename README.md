Check out nixpkgs repository to the root (~/nixpkgs).

### Compiling localy 
`nix-build -E 'with import <nixpkgs> {}; callPackage ./default.nix { }' -I nixpkgs=~/nixpkgs`

### Deploying changes
`./redeploy`
Note this will not remove machines if they no longer exist. 

### Removing machines
If you want to remove a machine simply remove them in the bottom part of the `nixops.nix` file.
Then run `nixops deploy` with `-k`:
`nixops deploy -k -d rscoin -I nixpkgs=~/nixpkgs`

### List all deployments
`nixops list`

### Infos about all machines (including public IPs)
`nixops info [-d deployment]`

### Connect to a machine
`nixops ssh -d rscoin machine-name`
Replace `machine-name` with the machine you want to connect to (from `info` call).

### Destroy everything
`./destroy.sh`
