package Nix::Config;

use MIME::Base64;

$version = "1.11.11";

$binDir = $ENV{"NIX_BIN_DIR"} || "/nix/store/367jrnx2rhk9g74zvg4m43h17jnf7371-nix-1.11.11/bin";
$libexecDir = $ENV{"NIX_LIBEXEC_DIR"} || "/nix/store/367jrnx2rhk9g74zvg4m43h17jnf7371-nix-1.11.11/libexec";
$stateDir = $ENV{"NIX_STATE_DIR"} || "/nix/var/nix";
$manifestDir = $ENV{"NIX_MANIFESTS_DIR"} || "/nix/var/nix/manifests";
$logDir = $ENV{"NIX_LOG_DIR"} || "/nix/var/log/nix";
$confDir = $ENV{"NIX_CONF_DIR"} || "/etc/nix";
$storeDir = $ENV{"NIX_STORE_DIR"} || "/nix/store";

$caBundle = $ENV{"NIX_SSL_CERT_FILE"} // $ENV{"SSL_CERT_FILE"} // $ENV{"CURL_CA_BUNDLE"} // $ENV{"OPENSSL_X509_CERT_FILE"};
$caBundle = "/etc/ssl/certs/ca-bundle.crt" if !$caBundle && -f "/etc/ssl/certs/ca-bundle.crt";
$caBundle = "/etc/ssl/certs/ca-certificates.crt" if !$caBundle && -f "/etc/ssl/certs/ca-certificates.crt";

$curlCaFlag = defined $caBundle ? "--cacert $caBundle" : "";

$bzip2 = "/nix/store/6fc4ak60p1mnilrjfcyz0sywwmpmf45w-bzip2-1.0.6.0.1-bin/bin/bzip2";
$xz = "/nix/store/s2hmf51bprsxb20v3bfigs8a7zsw3hjl-xz-5.2.2-bin/bin/xz";
$curl = "/nix/store/qiyaaa2cbkaxiri8zsx73xwx1lc6ljs0-curl-7.53.1-bin/bin/curl";
$openssl = "/nix/store/bizyb3ppl7r6xw23cigb48npwszrkzrz-openssl-1.0.2l-bin/bin/openssl";

$useBindings = "yes" eq "yes";

%config = ();

%binaryCachePublicKeys = ();

$defaultPublicKeys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";

sub readConfig {
    if (defined $ENV{'_NIX_OPTIONS'}) {
        foreach my $s (split '\n', $ENV{'_NIX_OPTIONS'}) {
            my ($n, $v) = split '=', $s, 2;
            $config{$n} = $v;
        }
    } else {
        my $config = "$confDir/nix.conf";
        return unless -f $config;

        open CONFIG, "<$config" or die "cannot open ‘$config’";
        while (<CONFIG>) {
            /^\s*([\w\-\.]+)\s*=\s*(.*)$/ or next;
            $config{$1} = $2;
        }
        close CONFIG;
    }

    foreach my $s (split(/ /, $config{"binary-cache-public-keys"} // $defaultPublicKeys)) {
        my ($keyName, $publicKey) = split ":", $s;
        next unless defined $keyName && defined $publicKey;
        $binaryCachePublicKeys{$keyName} = decode_base64($publicKey);
    }
}

return 1;
