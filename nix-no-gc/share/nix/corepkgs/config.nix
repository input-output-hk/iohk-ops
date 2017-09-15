let
  fromEnv = var: def:
    let val = builtins.getEnv var; in
    if val != "" then val else def;
in rec {
  perl = "/nix/store/nsa311yg8h93wfaacjk16c96a98bs09f-perl-5.22.3/bin/perl";
  shell = "/nix/store/fi3mbd2ml4pbgzyasrlnp0wyy6qi48fh-bash-4.4-p5/bin/bash";
  coreutils = "/nix/store/k8dyhz9hrjydl23ny3131am6cws1rc07-coreutils-8.26/bin";
  bzip2 = "/nix/store/6fc4ak60p1mnilrjfcyz0sywwmpmf45w-bzip2-1.0.6.0.1-bin/bin/bzip2";
  gzip = "/nix/store/m1z4gaqwal3gkhjz8309n2c24slr3dma-gzip-1.8/bin/gzip";
  xz = "/nix/store/s2hmf51bprsxb20v3bfigs8a7zsw3hjl-xz-5.2.2-bin/bin/xz";
  tar = "/nix/store/255w01kim6i6h604av7fxh63lplbldgb-gnutar-1.29/bin/tar";
  tarFlags = "--warning=no-timestamp";
  tr = "/nix/store/k8dyhz9hrjydl23ny3131am6cws1rc07-coreutils-8.26/bin/tr";
  nixBinDir = fromEnv "NIX_BIN_DIR" "/nix/store/367jrnx2rhk9g74zvg4m43h17jnf7371-nix-1.11.11/bin";
  nixPrefix = "/nix/store/367jrnx2rhk9g74zvg4m43h17jnf7371-nix-1.11.11";

  # If Nix is installed in the Nix store, then automatically add it as
  # a dependency to the core packages. This ensures that they work
  # properly in a chroot.
  chrootDeps =
    if dirOf nixPrefix == builtins.storeDir then
      [ (builtins.storePath nixPrefix) ]
    else
      [ ];
}
