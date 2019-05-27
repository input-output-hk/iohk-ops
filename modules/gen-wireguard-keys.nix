{ hosts ? [ "monitoring" ] }:

let
  pkgs = import (import ../fetch-nixpkgs.nix) {};
in pkgs.stdenv.mkDerivation {
  name = "gen-wireguard-keys";
  buildInputs = [ pkgs.wireguard ];
  shellHook = ''
    cd ${toString ../static}
    umask 077
    for host in ${toString hosts}; do
      if [[ -e ''${host}.wgprivate ]]; then
        echo "File \"''${host}.wgprivate\" already exists -- skipping public and private wg key creation for the host \"''${host}"\"
      else
        wg genkey > ''${host}.wgprivate
        wg pubkey < ''${host}.wgprivate > ''${host}.wgpublic
      fi
    done
    exit 0
  '';
}
