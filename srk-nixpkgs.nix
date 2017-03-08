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
  serokell-util = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "serokell-util";
        rev = "3675d2e8484c4e1bab98c92a4c9b01ca721ff808";
        sha256 = "1ak41860jdpmrgfjxnf8s5iapn2fh5ss0y1db1hw3g0hd70s80fd";
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
        rev = "b913bfc698fd2927ee5031826688eb7245906e6d";
        sha256 = "0vwj0vlby951v5180m7qh2rl3kxwncmcpfmvx3f2mnhb1nm05va4";
      })
  ) { };
  cardano-report-server = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-report-server";
        rev = "ede7a1a9eafd88e3449b38c7425f5529d6584014";
        sha256 = "1j6dcjs80mxqq5r1p47c8076629hbsqw4kf63023s2rvw548qh4i";
      })
  ) { };

  cardano-sl-core = hspkgs.callPackage (
    haskellPackageGen { } ("${pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "aa46ea4973b2afd3e1ec4d578cc10c1b63f5fcdb";
        sha256 = "03cqs2ddlw1sgk5lbr19f2br7vn1nn22f3xibvarcscbmryjrp33";
      }}/core")
  ) { };
  
  cardano-sl = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "aa46ea4973b2afd3e1ec4d578cc10c1b63f5fcdb";
        sha256 = "03cqs2ddlw1sgk5lbr19f2br7vn1nn22f3xibvarcscbmryjrp33";
      })
  ) { rocksdb = rocksdb-haskell; };
  
  hspkgs = compiler.override {
    overrides = self: super: {
      inherit universum acid-state kademlia plutus-prototype node-sketch network-transport-tcp ed25519 log-warper cardano-report-server network-transport serokell-util cardano-sl-core;

      QuickCheck = super.QuickCheck_2_9_2;
      optparse-applicative = super.optparse-applicative_0_13_0_0;
      criterion = super.criterion_1_1_4_0;
      #code-page = super.code-page_0_1_1;

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
