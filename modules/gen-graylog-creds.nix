{ user ? null, password ? null }:

let
  pkgs = import (import ../fetch-nixpkgs.nix) {};
in pkgs.stdenv.mkDerivation {
  name = "gen-graylog-creds";
  buildInputs = with pkgs; [ pwgen gnused ];
  shellHook = ''
    clusterChar="96"                     # Default graylog cluster secret length
    clusterSecret=""                     # Var for the clusterSecret
    credsFilename="graylog-creds.nix"    # Default graylog static filename
    defaultUser="root"                   # Default administrative user
    password="${toString password}"      # password supplied by cli arg
    passwordChar="32"                    # Default graylog password length
    passwordHash=""                      # Sha256 hash of the plaintext password
    staticPath=${toString ../static}     # Absolute path to the static dir
    user="${toString user}"              # user supplied by cli arg

    if [[ -e "$staticPath/$credsFilename" ]]; then
      echo "File already exists: $staticPath/$credsFilename, aborting!"
      exit 1
    elif [[ -z $user ]]; then
      echo "User is empty -- setting to a default administrative user of $defaultUser"
      user=$defaultUser
    fi
    echo "Writing graylog creds for user $user..."
    if [[ -z $password ]]; then
      echo "Password is empty -- setting to a random alphanumeric password of length $passwordChar"
      password=$(pwgen $passwordChar 1)
    fi

    passwordHash=$(echo -n $password | sha256sum | sed -z 's/  -\n//g')
    clusterSecret=$(pwgen $clusterChar 1)

    umask 077
    cd $path
    cat << EOF > $staticPath/$credsFilename
    {
      user = "$user";
      password = "$password";
      passwordHash = "$passwordHash";
      clusterSecret = "$clusterSecret";
    }
    EOF
    exit 0
  '';
}
