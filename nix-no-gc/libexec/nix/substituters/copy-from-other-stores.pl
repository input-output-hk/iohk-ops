#! /nix/store/nsa311yg8h93wfaacjk16c96a98bs09f-perl-5.22.3/bin/perl -w -I/nix/store/367jrnx2rhk9g74zvg4m43h17jnf7371-nix-1.11.11/lib/perl5/site_perl/5.22.3/x86_64-linux-thread-multi -I/nix/store/1kx1ydjpbjvg5fq76y00fa36fb4fc7r8-perl-DBI-1.636/lib/perl5/site_perl -I/nix/store/g5dkn63v63zp15hkp7s3wj39lh2crrwk-perl-DBD-SQLite-1.50/lib/perl5/site_perl -I/nix/store/fgfismdazvinpkxja2xjd489kygxj94v-perl-WWW-Curl-4.17/lib/perl5/site_perl

use utf8;
use strict;
use File::Basename;
use IO::Handle;

my $binDir = $ENV{"NIX_BIN_DIR"} || "/nix/store/367jrnx2rhk9g74zvg4m43h17jnf7371-nix-1.11.11/bin";


STDOUT->autoflush(1);
binmode STDERR, ":encoding(utf8)";

my @remoteStoresAll = split ':', ($ENV{"NIX_OTHER_STORES"} or "");

my @remoteStores;
foreach my $dir (@remoteStoresAll) {
    push @remoteStores, glob($dir);
}

exit if scalar @remoteStores == 0;
print "\n";


$ENV{"NIX_REMOTE"} = "";


sub findStorePath {
    my $storePath = shift;
    foreach my $store (@remoteStores) {
        my $sourcePath = "$store/store/" . basename $storePath;
        next unless -e $sourcePath || -l $sourcePath;
        $ENV{"NIX_DB_DIR"} = "$store/var/nix/db";
        return ($store, $sourcePath) if
            system("$binDir/nix-store --check-validity $storePath") == 0;
    }
    return undef;
}


if ($ARGV[0] eq "--query") {

    while (<STDIN>) {
        chomp;
        my ($cmd, @args) = split " ", $_;

        if ($cmd eq "have") {
            foreach my $storePath (@args) {
                print "$storePath\n" if defined findStorePath($storePath);
            }
            print "\n";
        }

        elsif ($cmd eq "info") {
            foreach my $storePath (@args) {
                my ($store, $sourcePath) = findStorePath($storePath);
                next unless defined $store;

                $ENV{"NIX_DB_DIR"} = "$store/var/nix/db";

                my $deriver = `$binDir/nix-store --query --deriver $storePath`;
                die "cannot query deriver of ‘$storePath’" if $? != 0;
                chomp $deriver;
                $deriver = "" if $deriver eq "unknown-deriver";

                my @references = split "\n",
                    `$binDir/nix-store --query --references $storePath`;
                die "cannot query references of ‘$storePath’" if $? != 0;

                my $narSize = `$binDir/nix-store --query --size $storePath`;
                die "cannot query size of ‘$storePath’" if $? != 0;
                chomp $narSize;

                print "$storePath\n";
                print "$deriver\n";
                print scalar @references, "\n";
                print "$_\n" foreach @references;
                print "0\n";
                print "$narSize\n";
            }

            print "\n";
        }

        else { die "unknown command ‘$cmd’"; }
    }
}


elsif ($ARGV[0] eq "--substitute") {
    die unless scalar @ARGV == 3;
    my $storePath = $ARGV[1];
    my $destPath = $ARGV[2];
    my ($store, $sourcePath) = findStorePath $storePath;
    die unless $store;
    print STDERR "\n*** Copying ‘$storePath’ from ‘$sourcePath’\n\n";
    system("/nix/store/k8dyhz9hrjydl23ny3131am6cws1rc07-coreutils-8.26/bin/cp", "-rpd", $sourcePath, $destPath) == 0
        or die "cannot copy ‘$sourcePath’ to ‘$storePath’";
    print "\n"; # no hash to verify
}


else { die; }
