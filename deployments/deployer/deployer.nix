let
  users = import ../../lib/users/deployer-users.nix;
in {
  deployer = {
    imports = [
        <module/deployer-base.nix>
        <module/deployer-aws.nix>
    ];
    nix.trustedUsers = [ "root" users.deployer.name users.dev.name ];
  };
}
