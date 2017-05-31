with (import ./../lib.nix);

let
  timeWarpReceiver = { pkgs, ... }: {
    imports = [ ./../modules/timewarp-node.nix ];
    services.timewarp-node.enable = true;
  };
  timeWarpSender = { pkgs, ... }: {
    imports = [ timeWarpReceiver ];
    services.timewarp-node.sender = true;
  };
in {
  network.description = "TimeWarp experiments";

  timewarp0 = timeWarpSender;
  timewarp1 = timeWarpReceiver;
  timewarp2 = timeWarpReceiver;
  timewarp3 = timeWarpReceiver;
  timewarp4 = timeWarpReceiver;
}
