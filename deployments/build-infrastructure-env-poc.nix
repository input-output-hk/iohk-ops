{ ... }:

with (import ./../lib.nix);
{
  network.description = "IOHK build infrastructure POC";

  defaults = {
    services.dd-agent.tags = ["env:poc" ];
  };

  bazel-cache = { config, pkgs, resources, ... }: let
    hostName = "bazel-cache.aws.iohkdev.io";
  in {
    imports = [
      ../modules/common.nix
      ../modules/nginx-bazel-cache-service.nix
    ];
    services.nginx-bazel-cache = {
      enable = true;
      publicHost =  hostName;
      writePassword = readFile ../static/bazel-cache-write-password.secret;
    };
  };
}
