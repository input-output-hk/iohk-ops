{ config, lib, ... }:

with lib;

let
  cfg = config.services.nginx-bazel-cache;

in {
  options.services.nginx-bazel-cache = {

    enable = mkEnableOption "nginx-bazel-cache";

    systemUser  = mkOption {
      description = "System user that should run the Ngnix instance";
      type = types.str;
      default = "bazel-cache";
    };
    publicHost  = mkOption {
      description = "Http virtual host to use.";
      type = types.str;
    };
    enableHttps  = mkOption {
      description = "Enable HTTPS (active by default)";
      default = true;
      type = types.bool;
    };
    writeUser  = mkOption {
      description = "User for writing to the cache (using basic auth)";
      type = types.str;
      default = "bazel-cache";
    };
    writePassword  = mkOption {
      description = "Password for writing to the cache (using basic auth)";
      type = types.str;
    };
    maxWriteSize  = mkOption {
      description = "Max accepted size for cached blobs";
      type = types.str;
      default = "100m";
    };
    cacheDirectory  = mkOption {
      description = "Root directory of the bazel http cache";
      type = types.str;
      default = "/var/lib/bazel-cache";
    };
    garbageCollectAfter  = mkOption {
      description = "Time to wait before garbage collecting unused cache entries";
      type = types.str;
      default = "30d";
    };
  };

  config = mkIf cfg.enable {

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    deployment.keys.bazel-cache-auth = {
      text = "${cfg.writeUser}:{PLAIN}${cfg.writePassword}";
      user = cfg.systemUser;
      destDir = "/var/lib/keys";
    };
    
    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      clientMaxBodySize = cfg.maxWriteSize;
      user = cfg.systemUser;
      virtualHosts."${cfg.publicHost}" = {
        addSSL = cfg.enableHttps;
        enableACME = cfg.enableHttps;
        locations."/" = {
          root = cfg.cacheDirectory;
          extraConfig = ''
              # Allow PUT
              dav_methods PUT;
              # Allow nginx to create the <project>/ac and <project>/cas subdirectories.
              create_full_put_path on;
              allow all;

              limit_except GET HEAD {
                # PUT is protected by password
                auth_basic 'Restricted';
                auth_basic_user_file /var/lib/keys/bazel-cache-auth;
              }
          '';
        };
      };
    };

    systemd = {
      services.nginx = {
        after = [ "bazel-cache-auth-key.service" ];
        wants = [ "bazel-cache-auth-key.service" ];
      };

      # Garbage collect unused entries in  bazel cache:
      tmpfiles.rules = [ "e ${cfg.cacheDirectory} - - - ${cfg.garbageCollectAfter}" ];
    };

    users.users."${cfg.systemUser}" = {
      isSystemUser = true;
      extraGroups = [ "keys" ];
      home = cfg.cacheDirectory;
      createHome = true;
      description = "Bazel cache server user";
    };
  };
}
