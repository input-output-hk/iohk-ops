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
      source-wallet = { generate-to = builtins.toString /var/lib/faucet/generated-wallet.json; };
    };

  };
}
