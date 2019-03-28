{ stdenv, getSrc, nodejs, yarn, fetchurl, linkFarm }:

let
  yarnDeps = import ./explorer/yarn.nix { inherit fetchurl linkFarm; };
  offlineCache = yarnDeps.offline_cache;
in stdenv.mkDerivation rec {
  name = "ethereum-explorer";
  src  = getSrc name;

  buildInputs = [ nodejs yarn ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"

    yarn config --offline set yarn-offline-mirror ${offlineCache}

    yarn install --offline --frozen-lockfile --ignore-engines --ignore-scripts
  '';

  buildPhase = "yarn run build";

  doCheck = true;

  checkPhase = "yarn test --coverage --ci --no-color";

  installPhase = "mv build $out";
}
