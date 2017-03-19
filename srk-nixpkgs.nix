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
        rev = "9edb5d374ec1fb6bb873dacc22a013f1eacd9c67";
        sha256 = "1cy1mkl2n9kjmya0225nc2bichb60q4y8n3wym7m79qcvsh6rxa2";
      })
  ) { };
  serokell-util = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "serokell-util";
        rev = "af79ca0c42b8fd23c499096f91f649edfb0862f1";
        sha256 = "1jlrx2cnlxkglgqkkggsfpjpppvnqn4rcj73jj534dfsd1hh9vv8";
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
        rev = "b3b2d4d5afa2c22f3aff10b4cceab3914c6bc0e5";
        sha256 = "0zwrjipjbjrqlgwdyn8ad0m28bndgz1dr2z01k4464c1whcy46m1";
      })
  ) { };
  network-transport-tcp = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "avieth";
        repo = "network-transport-tcp";
        rev = "1739cc6d5c73257201e5551088f4ba56d5ede15c";
        sha256 = "1lznyp62w4h5387p70i4hm29v52bi2cackxl4ig2adfi8dg83i88";
      })
  ) { };
  network-transport = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "avieth";
        repo = "network-transport";
        rev = "f2321a103f53f51d36c99383132e3ffa3ef1c401";
        sha256 = "0gd1c36khb0mdp0w9b8xrmcr1pdnki2l9m2rck8y5lmn5xjf2bhr";
      })
  ) { };
  plutus-prototype = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "plutus-prototype";
        rev = "e2e2711e6978002279b4d7c49cab1aff47a2fd43";
        sha256 = "0h2zq1kcss3f43yqhbz8bjpyxfqlf1wkkdwr91vdkcbjmbgkm8hb";
      })
  ) { };
  ed25519 = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "domenkozar";
        repo = "hs-ed25519";
        rev = "57adb970e198a9b377769ab46e776ec29f19cff6";
        sha256 = "1xkd683d3p84a4as38i4s99mz7pnhs7r4s7jwj91shri850n112j";
      })
  ) { };
  rocksdb-haskell = hspkgs.callPackage (
    haskellPackageGen {} (pkgs.fetchFromGitHub {
        owner = "serokell";
        repo = "rocksdb-haskell";
        rev = "4dfd8d61263d78a91168e86e8005eb9b7069389e";
        sha256 = "05dwx4wgiqin3ryyjf9z49vxnqs7qzyg296gbbhx3ikg3vxqb3bg";
        # rev = "cbe733d9bbdc61b07d9a6fd0bc7964ac1e78f6a5";
        # sha256 = "0hi5fda4h8q1jrdr6k0knxhzs08lhwc5a7achj0ys75snh7w5151";
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
        rev = "2943e7814b9f78b9ef68c9bdc96820ed32a0fdf3";
        sha256 = "0xl4ydbnl187ghpyvgbyi7lf9hw7xhy638ygpzr4d90nav8xmf1q";
      })
  ) { };
  cardano-report-server = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-report-server";
        rev = "424e4ecacdf038a01542025dd1296bd272ce770d";
        sha256 = "11wq4wjp45mn45x3109xi64p824mlimjkd4dk6r7gnmd0l9v77mh";
      })
  ) { };

  cardano-sl-core = hspkgs.callPackage (
    haskellPackageGen { } ("${pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "c10bcd8f19d538c58251e33a994e3246aeda891c";
        sha256 = "1r4fam0r55ivb9wrsfyz8ja5kfaz5m4pzg31ncjspq9rrl2la6id";
      }}/core")
  ) { };

    cardano-sl-db = hspkgs.callPackage (
    haskellPackageGen { } ("${pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "c10bcd8f19d538c58251e33a994e3246aeda891c";
        sha256 = "1r4fam0r55ivb9wrsfyz8ja5kfaz5m4pzg31ncjspq9rrl2la6id";
      }}/db")
  ) { rocksdb = rocksdb-haskell; };

    cardano-sl-infra = hspkgs.callPackage (
    haskellPackageGen { } ("${pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "c10bcd8f19d538c58251e33a994e3246aeda891c";
        sha256 = "1r4fam0r55ivb9wrsfyz8ja5kfaz5m4pzg31ncjspq9rrl2la6id";
      }}/infra")
  ) { };

    cardano-sl-update = hspkgs.callPackage (
    haskellPackageGen { } ("${pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "c10bcd8f19d538c58251e33a994e3246aeda891c";
        sha256 = "1r4fam0r55ivb9wrsfyz8ja5kfaz5m4pzg31ncjspq9rrl2la6id";
      }}/update")
  ) { };

    cardano-sl-lrc = hspkgs.callPackage (
    haskellPackageGen { } ("${pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "c10bcd8f19d538c58251e33a994e3246aeda891c";
        sha256 = "1r4fam0r55ivb9wrsfyz8ja5kfaz5m4pzg31ncjspq9rrl2la6id";
      }}/lrc")
  ) { };

    cardano-sl = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-sl";
        rev = "c10bcd8f19d538c58251e33a994e3246aeda891c";
        sha256 = "1r4fam0r55ivb9wrsfyz8ja5kfaz5m4pzg31ncjspq9rrl2la6id";
      })
  ) { rocksdb = rocksdb-haskell; };

      cardano-crypto = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-crypto";
        rev = "838b064d8a59286142aa2fe14434fe7601896ddb";
        sha256 = "02gjaj7889y30g2qfdh96ywrsdpmgfgyqyajw49zaklwjvkh87sv";
      })
  ) { };

    cryptonite = hspkgs.callPackage (
    haskellPackageGen { } (pkgs.fetchFromGitHub {
        owner = "haskell-crypto";
        repo = "cryptonite";
        rev = "6440a7ebab7d85612e47099017bee0da6339af05";
        sha256 = "14b64f45fgxq6kdq44ibsw23bzhkfy3arpd0ws468wd7vihdah4v";
      })
  ) { };


  hspkgs = compiler.override {
    overrides = self: super: {
      inherit universum acid-state kademlia plutus-prototype node-sketch network-transport-tcp ed25519 log-warper cardano-report-server network-transport serokell-util cardano-sl-db cardano-crypto cardano-sl-infra cardano-sl-update cardano-sl-lrc cryptonite;

      QuickCheck = super.QuickCheck_2_9_2;
      optparse-applicative = super.optparse-applicative_0_13_0_0;
      criterion = super.criterion_1_1_4_0;
      #code-page = super.code-page_0_1_1;

      turtle = doJailbreak super.turtle_1_3_0;

      cardano-sl-core = overrideCabal cardano-sl-core (drv: {
        configureFlags = ["-f-dev-mode"];
      });


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
