{ ... }:

{
  nix = {
    sandboxPaths = [ "/etc/nsswitch.conf" "/etc/protocols" ];
  };
}
