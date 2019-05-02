with (import ../../lib.nix);
{
  deployer = {
    # Heh, did not found a smarter way to
    # query the user name from somewhere else.
    name         = "deployer";
    isNormalUser = true;
    description  = "Deploy the deployer";
    group        = "deployers";
    extraGroups  = [ "wheel" ];
    openssh.authorizedKeys.keys = devOpsKeys;
  };

  dev = {
      name         = "dev";
      isNormalUser = true;
      description  = "Shared user for develelopment deploys";
      group        = "deployers";
      extraGroups  = [ ];
      openssh.authorizedKeys.keys = devKeys;
  };
}
