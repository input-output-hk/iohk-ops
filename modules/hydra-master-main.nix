{ resources, config, pkgs, lib, ... }:

with lib;

let
  commonBuildMachineOpt = {
    speedFactor = 1;
    sshKey = "/etc/nix/id_buildfarm";
    sshUser = "root";
    systems = [ "i686-linux" "x86_64-linux" ];
    supportedFeatures = [ "kvm" "nixos-test" "big-parallel" ];
  };
  mkLinux = hostName: commonBuildMachineOpt // {
    inherit hostName;
    maxJobs = 8;
  };
  mkLinuxLarge = hostName: commonBuildMachineOpt // {
    inherit hostName;
    maxJobs = 10;
    speedFactor = 5;
  };
  mkMac = hostName: commonBuildMachineOpt // {
    inherit hostName;
    maxJobs = 8;
    systems = [ "x86_64-darwin" ];
    sshUser = "builder";
    supportedFeatures = [];
  };
  localMachine = {
    hostName = "localhost";
    mandatoryFeatures = [ "local" ];
    systems = [ "x86_64-linux" "i686-linux" ];
    maxJobs = 16;
  };
  mkGithubStatus = { jobset, inputs }: ''
    <githubstatus>
      jobs = Cardano:${jobset}.*:required
      inputs = ${inputs}
      excludeBuildFromContext = 1
      useShortContext = 1
    </githubstatus>
  '';
  mkStatusBlocks = concatMapStringsSep "" mkGithubStatus;
in {
  environment.etc = lib.singleton {
    target = "nix/id_buildfarm";
    source = ../static/id_buildfarm;
    uid = config.ids.uids.hydra-queue-runner;
    gid = config.ids.gids.hydra;
    mode = "0400";
  };
  programs.ssh.extraConfig = lib.mkAfter ''
    Host sarov
    Hostname 192.168.20.20
    Port 2200
    Host mac-mini-1
    Hostname 192.168.20.21
    Port 2200
    Host mac-mini-2
    Hostname 192.168.20.22
    Port 2200
  '';

  nix = {
    distributedBuilds = true;
    buildMachines = [
      (mkLinux "packet-hydra-slave-1.aws.iohkdev.io")
      (mkLinux "packet-hydra-slave-2.aws.iohkdev.io")
      (mkLinux "packet-hydra-slave-3.aws.iohkdev.io")
      (mkLinux "packet-hydra-slave-4.aws.iohkdev.io")
      (mkLinux "packet-hydra-slave-5.aws.iohkdev.io")

      # Temporary extra build slave for addnl load
      #(mkLinux "packet-hydra-slave-6.aws.iohkdev.io")
      #(mkLinux "packet-hydra-slave-7.aws.iohkdev.io")
      #(mkLinux "packet-hydra-slave-8.aws.iohkdev.io")
      #(mkLinux "packet-hydra-slave-9.aws.iohkdev.io")
      #(mkLinux "packet-hydra-slave-10.aws.iohkdev.io")

      # Temporary ipxe build slaves for migration
      (mkLinuxLarge "packet-ipxe-1.ci.iohkdev.io")
      (mkLinuxLarge "packet-ipxe-2.ci.iohkdev.io")
      (mkLinuxLarge "packet-ipxe-3.ci.iohkdev.io")

      # ((mkMac "sarov") // { speedFactor = 1; })
      ((mkMac "mac-mini-1") // { speedFactor = 2; })
      ((mkMac "mac-mini-2") // { speedFactor = 2; })
      #(mkMac "osx-1.aws.iohkdev.io")
      #(mkMac "osx-2.aws.iohkdev.io")
      #(mkMac "osx-3.aws.iohkdev.io")
      #(mkMac "osx-4.aws.iohkdev.io")
      #(mkMac "osx-5.aws.iohkdev.io")
      localMachine
    ];
    binaryCaches = mkForce [ "https://cache.nixos.org" ];
  };

  services.auto-gc = {
    nixAutoMaxFreedGB  = 66;
    nixAutoMinFreeGB   = 60;
    nixAbsoluteTimedGB = 64;
  };

  services.hydra = {
    hydraURL = "https://hydra.iohk.io";
    package = pkgs.callPackage ../pkgs/hydra.nix {};
    # max output is 4GB because of amis
    # auth token needs `repo:status`
    extraConfig = ''
      max_output_size = 4294967296
      evaluator_max_heap_size = ${toString (5 * 1024 * 1024 * 1024)}
      max_db_connections = 50

      max_concurrent_evals = 14

      store_uri = s3://iohk-nix-cache?secret-key=/etc/nix/hydra.iohk.io-1/secret&log-compression=br&region=eu-central-1
      server_store_uri = https://iohk-nix-cache.s3-eu-central-1.amazonaws.com/
      binary_cache_public_uri = https://iohk-nix-cache.s3-eu-central-1.amazonaws.com/
      log_prefix = https://iohk-nix-cache.s3-eu-central-1.amazonaws.com/
      upload_logs_to_binary_cache = true

      <github_authorization>
        input-output-hk = ${builtins.readFile ../static/github_token}
      </github_authorization>
      ${mkStatusBlocks [
        { jobset = "iohk-ops"; inputs = "jobsets"; }
        { jobset = "cardano-base"; inputs = "cardano-base"; }
        { jobset = "cardano-byron-proxy"; inputs = "cardano-byron-proxy"; }
        { jobset = "cardano-prelude"; inputs = "cardano-prelude"; }
        { jobset = "decentralized-software-updates"; inputs = "decentralized-software-updates"; }
        { jobset = "cardano-ledger-specs"; inputs = "cardano-ledger-specs"; }
        { jobset = "cardano-ledger"; inputs = "cardano-ledger"; }
        { jobset = "cardano-wallet"; inputs = "cardano-wallet"; }
        { jobset = "cardano-shell"; inputs = "cardano-shell"; }
        { jobset = "cardano-node"; inputs = "cardano-node"; }
        { jobset = "cardano-explorer"; inputs = "cardano-explorer"; }
        { jobset = "jormungandr"; inputs = "jormungandr"; }
        { jobset = "cardano"; inputs = "cardano"; }
        { jobset = "plutus"; inputs = "plutus"; }
        { jobset = "log-classifier"; inputs = "log-classifier"; }
        { jobset = "ouroboros-network"; inputs = "ouroboros-network"; }
        { jobset = "iohk-monitoring"; inputs = "iohk-monitoring"; }
        { jobset = "haskell-nix"; inputs = "haskell-nix"; }
        { jobset = "tools"; inputs = "tools"; }
        { jobset = "iohk-nix"; inputs = "iohk-nix"; }
      ]}

      # DEVOPS-1208 This CI status for cardano-sl is needed while the
      # Daedalus Windows installer is built on AppVeyor or Buildkite
      <githubstatus>
        jobs = Cardano:cardano-sl.*:daedalus-mingw32-pkg
        inputs = cardano
        excludeBuildFromContext = 1
        useShortContext = 1
      </githubstatus>
      <githubstatus>
        jobs = Cardano:daedalus.*:tests\..*
        inputs = daedalus
        excludeBuildFromContext = 1
        useShortContext = 1
      </githubstatus>
    '';
  };
  services.grafana = {
    enable = true;
    users.allowSignUp = true;
    domain = "hydra.iohk.io";
    rootUrl = "%(protocol)ss://%(domain)s/grafana/";
    extraOptions = {
      AUTH_GOOGLE_ENABLED = "true";
      AUTH_GOOGLE_CLIENT_ID = "778964826061-5v0m922g1qcbc1mdtpaf8ffevlso2v7p.apps.googleusercontent.com";
      AUTH_GOOGLE_CLIENT_SECRET = builtins.readFile ../static/google_oauth_hydra_grafana.secret;
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  environment.systemPackages = with pkgs; [ goaccess ];
  services.nginx = {
    virtualHosts = {
      "hydra.iohk.io" = {
        forceSSL = true;
        enableACME = true;
        locations."/".extraConfig = ''
          proxy_pass http://127.0.0.1:8080;
          proxy_set_header Host $host;
          proxy_set_header REMOTE_ADDR $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
        locations."~ /(nix-cache-info|.*\\.narinfo|nar/*)".extraConfig = ''
          return 301 https://iohk-nix-cache.s3-eu-central-1.amazonaws.com$request_uri;
        '';
        locations."/graph/".extraConfig = ''
          proxy_pass http://127.0.0.1:8081;
        '';
        locations."/grafana/".extraConfig = ''
          proxy_pass http://localhost:3000/;
        '';
      };
    };
    commonHttpConfig = ''
      server_names_hash_bucket_size 64;
      keepalive_timeout   70;
      gzip            on;
      gzip_min_length 1000;
      gzip_proxied    expired no-cache no-store private auth;
      gzip_types      text/plain application/xml application/javascript application/x-javascript text/javascript text/xml text/css;
      log_format x-fwd '$remote_addr - $remote_user [$time_local] '
                       '"$request" $status $body_bytes_sent '
                       '"$http_referer" "$http_user_agent" "$http_x_forwarded_for"';
      access_log syslog:server=unix:/dev/log x-fwd;
    '';
  };
}
