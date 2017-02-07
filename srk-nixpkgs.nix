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
        rev = "0c95eda5e900b1e769bf5648cc62a3ad3372c3bd";
        sha256 = "0scfv782apgdm5h8ndw1xhyh7i7d3pad5a5fxli6ykbjbdhj22qw";
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
        rev = "fa5f0ab17b4248d072e5a5bb01ce1a72fae4eebc";
        sha256 = "03anjzm3fcak4gqrmn6qfw06ki9c6d35p0zz449nfxmlhpczl5kw";
      })
  ) { };
  network-transport-tcp = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "network-transport-tcp";
        rev = "586c9bf830252476522cab6274ef8ddc32615686";
        sha256 = "04q73wpi5kzqr4fk1zm5bgayljjc62qi34yls59zwv73k745dwzp";
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
        rev = "7b44e90d9831de861ca6f563929630407c6294b4";
        sha256 = "143bkllc8lra8j2q93i36gwsz6dwpj3sjlha2mlfqfmnkzq0lr1k";
      })
  ) { };
  cardano-sl = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "32ca3c44d617932052dc9e28369ec8daa0d549a9";
        sha256 = "0xxr1yr5x6xfmnzcybw8rgkkd9xs8gkn44h17c1ffrp3qvvw02xr";
      })
  ) { rocksdb = rocksdb-haskell; };
  
  hspkgs = compiler.override {
    overrides = self: super: {
      inherit universum acid-state kademlia plutus-prototype node-sketch network-transport-tcp ed25519 log-warper;

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
