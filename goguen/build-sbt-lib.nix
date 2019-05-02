{ stdenv, scala, sbt, sbt-extras }:
# This file contains some utils we want to upstream to iohk-nix at some point
# but are currently not stable enough to bother extracting it from this repo.
with builtins;
let
  getManagedJars = drv: ''
    cp -r ${drv}/.ivy ${drv}/.sbt ${drv}/target .
    chmod u+w -R .ivy .sbt target
  '';
  copyManagedToOut = drv: ''
    cp -r ${drv}/.ivy ${drv}/.sbt ${drv}/target .
    chmod u+w -R .ivy .sbt target
  '';
  foldDepJars = op: unmanagedDeps:
    (foldl' (acc: dep: acc + "\n" + (op dep)) "" unmanagedDeps);
  # We are filtering out the irrelevant files for a sbt update.
  # Basically we just wanna keep the .sbt files and the project folder.
  getSbtFiles = path: type: baseNameOf path != "src" &&
                            baseNameOf path != ".git" &&
                            baseNameOf path != "jobsets" &&
                            baseNameOf path != "result";
in rec {
  # name          :: String  | Derivation name.
	# src           :: Path    | Sources directory.
	# unmanagedDeps :: [ Drv ] | Dependencies manually managed (ie. not managed by Maven)
	fetchSbtDeps = { name, src, unmanagedDeps ? [] } : stdenv.mkDerivation ({
	    name = name + "-pre-fetched-deps";
	    src = filterSource getSbtFiles src;
	    buildInputs = [ scala sbt sbt-extras ];
	    configurePhase = ''
	     export HOME="$NIX_BUILD_TOP"
	     export "_JAVA_OPTIONS=-Dsbt.global.base=.sbt/1.0 -Dsbt.ivy.home=.ivy"
	    '' + foldDepJars getManagedJars unmanagedDeps;

	    buildPhase = ''
	      sbt update
	    '';

	    installPhase = ''
	      mkdir $out
	      cp -r .ivy .sbt target $out
	    '' + foldDepJars copyManagedToOut unmanagedDeps;
	});

  # Helper function we use to build a sbt dependency
  # such as sbt-verify.
  buildSbtLib = {name, src, buildCmd ? "sbt publishLocal", unmanagedDeps ? []}: stdenv.mkDerivation ({
      inherit name src;
      buildInputs = [ scala sbt sbt-extras ];
      configurePhase = ''
       export HOME="$NIX_BUILD_TOP"
       export "_JAVA_OPTIONS=-Dsbt.global.base=.sbt/1.0 -Dsbt.ivy.home=.ivy"
      '';

      buildPhase = "
        ${buildCmd}
      ";

      installPhase = ''
      mkdir $out
      cp -r .ivy .sbt target $out
      '';
  });

}
