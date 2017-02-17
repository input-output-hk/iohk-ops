{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc802
, genesisN ? 3 
, slotDuration ? 20 
, networkDiameter ? 6
, mpcRelayInterval ? 16 } :

with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs;});

let
  overrideAttrs = package: newAttrs:
    package.override (args: args // {
      mkDerivation = expr: args.mkDerivation (expr // newAttrs);
    });

  # Autogenerate default.nix from cabal file in src
  haskellPackageGen = { doHaddock ? false, doFilter ? false, doCheck ? true, profiling ? false }: src:
     let filteredSrc = builtins.filterSource (n: t: t != "unknown") src;
         package = pkgs.runCommand "default.nix" {} ''
           ${compiler.cabal2nix}/bin/cabal2nix \
             ${if doFilter then filteredSrc else src} \
             ${if doHaddock
                 then ""
                 else "--no-haddock"} \
             ${if doCheck
                 then ""
                 else "--no-check"} \
             ${if profiling
                 then "--enable-profiling"
                 else ""} \
             > $out
         '';
     in import package;
in rec {
  universum = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "universum";
        rev = "d4199cb7e67265a8b2ea1e5c0d9fa3142e473ab5";
        sha256 = "0l87zc75zd94qd5hwfb9ykq7834isqg7nij6zhgjjjs3k91is9jj";
      })
  ) { };
  acid-state = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "acid-state";
        rev = "95fce1dbada62020a0b2d6aa2dd7e88eadd7214b";
        sha256 = "109liqzk66cxkarw8r8jxh27n6qzdcha2xlhsj56xzyqc2aqjz15";
      })
  ) { };
  log-warper = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "log-warper";
        rev = "5c298604db2cba695dc24e19381810ba8095c668";
        sha256 = "0w2pjgimmfdgs4pwanp60y5z1cvh7h51vn0qaj1njq3lkx375rc4";
      })
  ) { };
  network-transport-tcp = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "avieth";
        repo = "network-transport-tcp";
        rev = "d2705abd5b54707ca97b5bf9c9c24005e800ee49";
        sha256 = "0qivhcvz25jvnpnf218qm7nkq84nbyzbqcx4ajys0rvv907pjybc";
      })
  ) { };
  network-transport = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "avieth";
        repo = "network-transport";
        rev = "e7a5f44d0d98370d16df103c9dc61ef7bf15aee8";
        sha256 = "032hl9jx5sq178kc1v0sjjr2347n5rvbfhkvjq0fmbgg710ghzfa";
      })
  ) { };
  plutus-prototype = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "plutus-prototype";
        rev = "4e3f6a268c3b13af78516f78ac71cd6215f5b0bf";
        sha256 = "180izcppph4npivm74swhka90wnnb1s6mlp6lm2f4h2i0c3696v2";
      })
  ) { };
  ed25519 = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "domenkozar";
        repo = "hs-ed25519";
        rev = "96e5db162d88482bc0e120dc61cadd45c168c275";
        sha256 = "1d3rry1lycdk4dq9ivhzlb5ygpr009ahfj07dsy3cigshy8r0llz";
      })
  ) { };
  rocksdb-haskell = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "rocksdb-haskell";
        rev = "cbe733d9bbdc61b07d9a6fd0bc7964ac1e78f6a5";
        sha256 = "0hi5fda4h8q1jrdr6k0knxhzs08lhwc5a7achj0ys75snh7w5151";
      })
  ) { };
  kademlia = hspkgs.callPackage (
    haskellPackageGen { doCheck = false; } (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "kademlia";
        rev = "bf65ac0cd50d2ccd7ef6507f0d71786c4bd10ae1";
        sha256 = "0sdb6w33vyx86pbp9s5s8c3lwsahcds2vzhpfpn7p9srp82mb82i";
      })
  ) { };
  node-sketch = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "time-warp-nt";
        rev = "9865cea6fe7a2b376d67c0ff77efaaa26ea73d45";
        sha256 = "13akc3yj8vm11xqzil5vg8l674r9d4lcnwdvv5g6qxsxrp0j2a3x";
      })
  ) { };
  cardano-report-server = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-report-server";
        rev = "344f0e6e1b69b512cbfb04caf42bbe285cc7727a";
        sha256 = "1qm430h8k9h7bvpr4nsgg25q65vn5z5hkk9gkm4zqia6ms4kqc0v";
      })
  ) { };
  
  cardano-sl = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "8540e55dd55094bbc9fb7da3266104dee1fac512";
        sha256 = "1z7jvfyc7n1czypljlbm9331lgp6pa2ib4069dwhvv6rgh16napf";
      })
  ) { rocksdb = rocksdb-haskell; };
  
  hspkgs = compiler.override {
    overrides = self: super: {
      inherit universum acid-state kademlia plutus-prototype node-sketch network-transport-tcp ed25519 log-warper cardano-report-server network-transport;

      QuickCheck = super.QuickCheck_2_9_2;
      optparse-applicative = super.optparse-applicative_0_13_0_0;

      turtle = doJailbreak super.turtle_1_3_0;

      cardano-sl = overrideCabal cardano-sl (drv: {
        # Build statically to reduce closure size
        enableSharedLibraries = false;
        enableSharedExecutables = false;
        isLibrary = false;
        patchPhase = ''
         export CSL_SYSTEM_TAG=linux64
        '';
        # production full nodes shouldn't use wallet as it means different constants
        configureFlags = [ "-f-asserts" "-f-with-wallet" "-f-with-web" "-f-dev-mode"];
        # speed up production build
        doHaddock = false;
        doCheck = false;
        postFixup = ''
          echo "Removing $out/lib to spare HDD space for deployments"
          rm -rf $out/lib
        '';
      });

      mkDerivation = args: super.mkDerivation (args // {
        #enableLibraryProfiling = false;

        # too many test dep mismatches with new quickcheck
        doCheck = false;
      });
    };
  };

}
