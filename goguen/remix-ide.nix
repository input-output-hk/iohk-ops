{ stdenv
, getSrc
, trimmed-solc-bin
, pkgs
}:

with pkgs;

# solcBin is huge, and we actually don't need all of it We just need
# to be careful that if we leave versions of solidity.js out, we
# update `list.json` accordingly.
stdenv.mkDerivation rec {
  name = "remix-ide";
  src  = getSrc name;

  buildInputs = [ nodejs-8_x git cacert python wget ];

  patches = [
    ./remix-ide.patch
  ];

  configurePhase = ''
    export HOME="$NIX_BUILD_TOP"
    npm install
  '';

  buildPhase = ''
    npm run linkremixcore
    npm run linkremixlib
    npm run linkremixsolidity
    npm run build
  '';

  installPhase = ''
    mv build $out

    ln -s ${trimmed-solc-bin} $out/solc-bin

    cp -r index.html soljson.js assets $out/
    substituteInPlace $out/index.html \
      --replace "build/app.js" "app.js" \
      --replace "UA-XXXXXXXXX-X" "UA-119953429-5"
  '';
}
