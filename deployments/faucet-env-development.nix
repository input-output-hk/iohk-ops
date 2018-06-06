{ globals, ... }: with (import ./../lib.nix);

let params = globals.fullMap.faucet;
in
{
  faucet = { config, resources, ...}: {
    imports = [
      ./../modules/development.nix
    ];

  };
}
