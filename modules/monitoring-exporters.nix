{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.monitoring-exporters;

in {

  options = {
    services.monitoring-exporters = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Enable all available exporters.
        '';
      };

      graylogHost = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "graylog:5044";
        description = ''
          The host port under which Graylog is externally reachable.
        '';
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [

    (mkIf (cfg.graylogHost != null ) {
      services.journalbeat = {
        enable = true;
        extraConfig = ''
        journalbeat:
          seek_position: cursor
          cursor_seek_fallback: tail
          write_cursor_state: true
          cursor_flush_period: 5s
          clean_field_names: true
          convert_to_numbers: false
          move_metadata_to_field: journal
          default_type: journal
        output.logstash:
          hosts: ["${cfg.graylogHost}"]
        '';
      };
    })

    (mkIf config.services.nginx.enable {
      services.nginx = {
        appendHttpConfig = ''
          vhost_traffic_status_zone;
          server {
            listen 9113;
            location /status {
              vhost_traffic_status_display;
              vhost_traffic_status_display_format html;
            }
          }
        '';
      };
      networking.firewall.allowedTCPPorts = [ 9113 ];
    })

    {

      systemd.services."statd-exporter" = {
        wantedBy = [ "multi-user.target" ];
        requires = [ "network.target" ];
        after = [ "network.target" ];
        script = ''
          ${pkgs.prometheus-statsd-exporter}/bin/statsd_bridge -statsd.listen-address ":8125" -web.listen-address ":9102" -statsd.add-suffix=false
        '';
      };

      services = {
        prometheus.exporters.node = {
          enable = true;
          enabledCollectors = [
            "systemd"
            "tcpstat"
            "conntrack"
            "diskstats"
            "entropy"
            "filefd"
            "filesystem"
            "loadavg"
            "meminfo"
            "netdev"
            "netstat"
            "stat"
            "time"
            "ntp"
            "timex"
            "vmstat"
            "logind"
            "interrupts"
            "ksmd"
            "processes"
          ];
        };
      };
      networking.firewall.allowedTCPPorts = [ 9100 9102 ];
  }
  ]);
}
