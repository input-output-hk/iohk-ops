{ user ? null, password ? null }:

let
  pkgs = import (import ../fetch-nixpkgs.nix) {};
in pkgs.stdenv.mkDerivation {
  name = "gen-grafana-creds";
  buildInputs = with pkgs; [ pwgen ];
  shellHook = ''
    credsFilename="grafana-creds.nix"    # Default grafana static filename
    defaultUser="root"                   # Default administrative user
    password="${toString password}"      # password supplied by cli arg
    passwordChar="32"                    # Default grafana password length
    staticPath=${toString ../static}     # Absolute path to the static dir
    user="${toString user}"              # user supplied by cli arg

    if [[ -e "$staticPath/$credsFilename" ]]; then
      echo "File already exists: $staticPath/$credsFilename, aborting!"
      exit 1
    elif [[ -z $user ]]; then
      echo "User is empty -- setting to a default administrative user of $defaultUser"
      user=$defaultUser
    fi
    echo "Writing grafana creds for user $user..."
    if [[ -z $password ]]; then
      echo "Password is empty -- setting to a random alphanumeric password of length $passwordChar"
      password=$(pwgen $passwordChar 1)
    fi

    umask 077
    cd $path
    cat << EOF > $staticPath/$credsFilename
    {
      user = "$user";
      password = "$password";
    }
    EOF
    exit 0
  '';
}
