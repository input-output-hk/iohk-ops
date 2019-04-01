{ ... }:
{
  monitoring = { ... }: 
  {
    imports = [
      ../modules/staging.nix
    ];
  };
}

