{ resources, config, pkgs, lib, ... }:

with lib;

let
  commonBuildMachineOpt = {
    speedFactor = 1;
    sshKey = "/etc/nix/id_buildfarm";
    sshUser = "root";
    system = "x86_64-linux,i686-linux";
    supportedFeatures = [ "kvm" "nixos-test" "big-parallel" ];
  };
  mkLinux = hostName: commonBuildMachineOpt // {
    inherit hostName;
    maxJobs = 12;
  };
  mkMac = hostName: commonBuildMachineOpt // {
    inherit hostName;
    maxJobs = 8;
    system = "x86_64-darwin";
    sshUser = "builder";
    supportedFeatures = [];
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
      (mkLinux "packet-hydra-buildkite-1.ci.iohkdev.io")
      (mkLinux "packet-hydra-buildkite-2.ci.iohkdev.io")
    ];
    binaryCaches = mkForce [ "https://cache.nixos.org" ];
    binaryCachePublicKeys = mkForce [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
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

      store_uri = s3://iohk-nix-cache-temp?secret-key=/etc/nix/hydra.iohk.io-1/secret&log-compression=br&region=eu-central-1
      server_store_uri = https://iohk-nix-cache-temp.s3-eu-central-1.amazonaws.com/
      binary_cache_public_uri = https://iohk-nix-cache-temp.s3-eu-central-1.amazonaws.com/
      log_prefix = https://iohk-nix-cache-temp.s3-eu-central-1.amazonaws.com/
      upload_logs_to_binary_cache = true

      <github_authorization>
        input-output-hk = ${builtins.readFile ../static/github_token}
      </github_authorization>
    '';

      #${mkStatusBlocks [
      #  { jobset = "iohk-ops"; inputs = "jobsets"; }
      #  { jobset = "cardano-base"; inputs = "cardano-base"; }
      #  { jobset = "cardano-byron-proxy"; inputs = "cardano-byron-proxy"; }
      #  { jobset = "cardano-prelude"; inputs = "cardano-prelude"; }
      #  { jobset = "decentralized-software-updates"; inputs = "decentralized-software-updates"; }
      #  { jobset = "cardano-ledger-specs"; inputs = "cardano-ledger-specs"; }
      #  { jobset = "cardano-ledger"; inputs = "cardano-ledger"; }
      #  { jobset = "cardano-wallet"; inputs = "cardano-wallet"; }
      #  { jobset = "cardano-shell"; inputs = "cardano-shell"; }
      #  { jobset = "cardano-node"; inputs = "cardano-node"; }
      #  { jobset = "cardano"; inputs = "cardano"; }
      #  { jobset = "plutus"; inputs = "plutus"; }
      #  { jobset = "log-classifier"; inputs = "log-classifier"; }
      #  { jobset = "ouroboros-network"; inputs = "ouroboros-network"; }
      #  { jobset = "iohk-monitoring"; inputs = "iohk-monitoring"; }
      #  { jobset = "haskell-nix"; inputs = "haskell-nix"; }
      #  { jobset = "tools"; inputs = "tools"; }
      #  { jobset = "iohk-nix"; inputs = "iohk-nix"; }
      #  { jobset = "cardano-explorer"; inputs = "cardano-explorer"; }
      #]}

      ## DEVOPS-1208 This CI status for cardano-sl is needed while the
      ## Daedalus Windows installer is built on AppVeyor or Buildkite
      #<githubstatus>
      #  jobs = Cardano:cardano-sl.*:daedalus-mingw32-pkg
      #  inputs = cardano
      #  excludeBuildFromContext = 1
      #  useShortContext = 1
      #</githubstatus>
      #<githubstatus>
      #  jobs = Cardano:daedalus.*:tests\..*
      #  inputs = daedalus
      #  excludeBuildFromContext = 1
      #  useShortContext = 1
      #</githubstatus>
  };
  services.grafana = {
    enable = false;
    users.allowSignUp = true;
    domain = "hydra.iohk.io";
    rootUrl = "%(protocol)ss://%(domain)s/grafana/";
    extraOptions = {
      AUTH_GOOGLE_ENABLED = "true";
      AUTH_GOOGLE_CLIENT_ID = "778964826061-5v0m922g1qcbc1mdtpaf8ffevlso2v7p.apps.googleusercontent.com";
      AUTH_GOOGLE_CLIENT_SECRET = builtins.readFile ../static/google_oauth_hydra_grafana.secret;
    };
  };

  users.users.debug = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    hashedPassword = "$6$QATPTe6nhL$iYsq8ZWGpROFHTfPhANEFCXQxg7JE5emifO.KhJXkq13rYzMW5GnPRaLq2NDd7Fk6VUgrI4.l7jerCedXOtz3.";
  };
  security.sudo.wheelNeedsPassword = true;

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  environment.systemPackages = with pkgs; [ goaccess ];
  services.nginx = {
    virtualHosts = {
      "hydra.iohk.io" = {
        forceSSL = false;
        enableACME = false;
        locations."/".extraConfig = ''
          proxy_pass http://127.0.0.1:8080;
          proxy_set_header Host $host;
          proxy_set_header REMOTE_ADDR $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
        locations."~ /(nix-cache-info|.*\\.narinfo|nar/*)".extraConfig = ''
          return 301 https://iohk-nix-cache-temp.s3-eu-central-1.amazonaws.com$request_uri;
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
