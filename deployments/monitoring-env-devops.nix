{ IOHKaccessKeyId, ... }:
let
  iohklib = import ../lib.nix;
  mkUplink = iohklib.mkMkUplink {
    central = "192.168.20.1";
    subnet = "192.168.20";
    # TODO, `monitoring-ip` will be wrong if monitoring isnt using an elastic ip by that name
    endpoint = "monitoring-ip:51820";
  };
in {
  require = [
    ./monitoring.nix
    ./security-groups/allow-deployer-ssh.nix
  ];
  monitoring = { lib, resources, ... }:
  {
    imports = [
      ../modules/devops.nix
    ];
    networking.wireguard.interfaces.wg0 = {
      peers = let
        genPeer = n: path: {
          allowedIPs = [ "192.168.20.${toString n}/32" ];
          publicKey = lib.strings.removeSuffix "\n" (builtins.readFile path);
        };
      in [
        { allowedIPs = [ "192.168.20.20/32" ]; publicKey = "Iv+pHGJ6uGYfrSeF3PMSlN4v6YPZF52Xr5f8teH8OEE="; } # sams mac
        { allowedIPs = [ "192.168.21.1/32" ]; publicKey = "oycbQ1DhtRh0hhD5gpyiKTUh0USkAwbjMer6/h/aHg8="; } # michaels desktop
      ];
    };

    services.monitoring-services.enableWireguard = true;
    deployment.keys."monitoring.wgprivate" = {
      destDir = "/etc/wireguard";
      keyFile = ../static/monitoring.wgprivate;
    };
    services.monitoring-services.applicationDashboards = ../modules/grafana/cardano;
    services.monitoring-services.applicationRules = [
      {
        alert = "chain_quality_degraded";
        expr = "cardano_chain_quality_last_k__2160__blocks__ < 99";
        for = "5m";
        labels = {
          severity = "page";
        };
        annotations = {
          summary = "{{$labels.alias}}: Degraded Chain Quality over last 2160 blocks.";
          description = "{{$labels.alias}}: Degraded Chain Quality over last 2160 blocks (<99%).";
        };
      }
      {
        alert = "mempoolsize_tx_count_too_large";
        expr = "max_over_time(cardano_MemPoolSize[5m]) > 190";
        for = "1m";
        labels = {
          severity = "page";
        };
        annotations = {
          summary = "{{$labels.alias}}: MemPoolSize tx count is larger than expected.";
          description = "{{$labels.alias}}: When a node's MemPoolSize grows larger than the system can handle, transactions will be dropped. The actual thresholds for that in mainnet are unknown, but [based on benchmarks done beforehand](https://input-output-rnd.slack.com/archives/C2VJ41WDP/p1506563332000201) transactions started getting dropped when the MemPoolSize was ~200 txs.";
        };
      }
    ];
  };
}
