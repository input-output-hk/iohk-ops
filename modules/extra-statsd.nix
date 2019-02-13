{ pkgs, lib, config, ... }:

with lib;
let
  extra-statsd = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "cleverca22";
    repo = "extra-statsd";
    rev = "343e8793594c91126dfd18fb31b07fba605711ab";
    sha256 = "1qf1grqcp232s3vafvrq58xa29yqbnhfnpq2v03dhsfcqr63pdvz";
  }) {};
in {
  options = {
    services.extra-statsd = mkEnableOption "extra-statsd";
  };
  config = mkIf config.services.extra-statsd {
    systemd.services.extra-statsd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "dd-agent.service" ];
      script = ''
        sleep 30
        exec ${extra-statsd.static}/bin/extra-statsd
      '';
    };
  };
}
