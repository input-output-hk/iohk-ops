
let
  iohkNix = (import ../../lib.nix).iohkNix;
  pkgs = iohkNix.pkgs;
  dependencies = with pkgs.python3Packages; [
    dateutil
    netifaces
    requests
    prometheus_client
    ipython
  ];
  shell = pkgs.mkShell {
    name = "jormungandr-monitor";
    buildInputs = dependencies;
  };

in shell
