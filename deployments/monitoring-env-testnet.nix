{
  require = [
    ./monitoring.nix
  ];
  monitoring = { pkgs, lib, ... }:
  {
    imports = [
      ../modules/testnet.nix
      ../modules/monitoring-rules-cardano.nix
    ];

    deployment.ec2.instanceType = "t3.xlarge";
    boot.loader.grub.device = lib.mkForce "/dev/nvme0n1"; # t3.xlarge has an nvme disk, and amazon-image.nix isnt handling it right yet
    deployment.ec2.ebsInitialRootDiskSize = 1000;

    systemd.services.graylog.environment = { JAVA_OPTS = ''
      -Djava.library.path=${pkgs.graylog}/lib/sigar -Xms3g -Xmx3g -XX:NewRatio=1 -server -XX:+ResizeTLAB -XX:+UseConcMarkSweepGC -XX:+CMSConcurrentMTEnabled -XX:+CMSClassUnloadingEnabled -XX:+UseParNewGC -XX:-OmitStackTraceInFastThrow
    ''; };
    services.elasticsearch.extraJavaOptions = [ "-Xms6g" "-Xmx6g" ];

    services.monitoring-services.grafanaAutoLogin = true;
    services.monitoring-services.applicationDashboards = ../modules/grafana/cardano;
  };
}
