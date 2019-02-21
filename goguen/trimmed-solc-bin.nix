{ stdenv
, pkgs
, getSrc
}:

with pkgs;

stdenv.mkDerivation rec {
    name = "solc-bin";
    src  = getSrc name;
    buildInputs = [ jq ];
    buildPhase = ''
      echo "source size:"
      du --max-depth=1 $src
    '';
    installPhase = ''
      mkdir -p $out/bin

      SOLC_VERSION=$(jq .latestRelease $src/bin/list.json)
      SOLC_PATH=$(jq -r .releases.$SOLC_VERSION $src/bin/list.json)
      jq ".releases |= with_entries(select(.key == $SOLC_VERSION)) | .builds |= del(.[] | select(.path != \"$SOLC_PATH\"))"  $src/bin/list.json > $out/bin/list.json
      cp $src/bin/$SOLC_PATH $out/bin/
      du --max-depth=1 $out/bin/
    '';
}
