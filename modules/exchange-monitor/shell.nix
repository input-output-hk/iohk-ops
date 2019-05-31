
let
  iohkNix = (import ../../lib.nix).iohkNix;
  pkgs = iohkNix.pkgs;
  dependencies = with pkgs.python3Packages; [
    requests
    prometheus_client
    ipython
  ];
  shell = pkgs.mkShell {
    name = "exchange-monitor";
    buildInputs = dependencies;
  };

in shell
