with import <nixpkgs> { config = {}; };

let
  ghc = haskellPackages.ghcWithPackages (p: with p; [ cryptonite base64-bytestring cryptohash-sha256 text libsystemd-journal safe ]);
  keys = writeText "keys" (lib.concatStringsSep "\n" (lib.mapAttrsToList (key: value: "${key} ${value}") (import ../ssh_keys.nix)));
in runCommand "ssh-audit" {
  buildInputs = [ ghc ];
  inherit keys;
} ''
  mkdir -pv $out/bin/
  cp -vi ${./.}/* .
  ghc ssh-audit.hs -o $out/bin/ssh-audit
''
