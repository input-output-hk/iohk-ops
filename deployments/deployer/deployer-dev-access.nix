with import ../../lib.nix;
let users = import ../../lib/users/deployer-users.nix;
in {
  deployer.users.users.dev = users.dev;
}
