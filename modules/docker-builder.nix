{ name, pkgs, ... }:

{
  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
    autoPrune.dates = "daily";
    autoPrune.flags = [ "--all" "--force" ];
  };
}
