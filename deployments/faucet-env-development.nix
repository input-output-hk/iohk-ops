{ globals, ... }: with (import ./../lib.nix);

let params = globals.fullMap.faucet;
in
{
  faucet = { config, resources, ...}: {
    imports = [
      ./../modules/development.nix
    ];
    services.faucet.faucet-config = {
      # TODO: use services.faucet.home here
      source-wallet-config = builtins.toString /var/lib/faucet/wallet-source.json;
    };

  };
}
