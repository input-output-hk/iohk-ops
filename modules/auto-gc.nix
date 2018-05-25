{ pkgs, ... }:

{
  nix = {
    gc = {
      automatic = true;
      dates = "*:15:00";
      options = ''--max-freed "$((64 * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | ${pkgs.gawk}/bin/awk '{ print $4 }')))"'';
    };
  };
}
