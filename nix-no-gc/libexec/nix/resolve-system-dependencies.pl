#! /nix/store/nsa311yg8h93wfaacjk16c96a98bs09f-perl-5.22.3/bin/perl -w -I/nix/store/367jrnx2rhk9g74zvg4m43h17jnf7371-nix-1.11.11/lib/perl5/site_perl/5.22.3/x86_64-linux-thread-multi -I/nix/store/1kx1ydjpbjvg5fq76y00fa36fb4fc7r8-perl-DBI-1.636/lib/perl5/site_perl -I/nix/store/g5dkn63v63zp15hkp7s3wj39lh2crrwk-perl-DBD-SQLite-1.50/lib/perl5/site_perl -I/nix/store/fgfismdazvinpkxja2xjd489kygxj94v-perl-WWW-Curl-4.17/lib/perl5/site_perl

use utf8;
use strict;
use warnings;
use Cwd qw(realpath);
use Errno;
use File::Basename qw(dirname);
use File::Path qw(make_path);
use File::Spec::Functions qw(catfile);
use List::Util qw(reduce);
use IPC::Open3;
use Nix::Config;
use Nix::Store qw(derivationFromPath);
use POSIX qw(uname);
use Storable qw(lock_retrieve lock_store);

my ($sysname, undef, $version, undef, $machine) = uname;
$sysname =~ /Darwin/ or die "This tool is only meant to be used on Darwin systems.";

my $cache = "$Nix::Config::stateDir/dependency-maps/$machine-$sysname-$version.map";

make_path dirname($cache);

our $DEPS;
eval {
  $DEPS = lock_retrieve($cache);
};

if($!{ENOENT}) {
  lock_store {}, $cache;
  $DEPS = {};
} elsif($@) {
  die "Unable to obtain a lock on dependency-map file $cache: $@";
}

sub mkset(@) {
  my %set;
  @set{@_} = ();
  \%set
}

sub union($$) {
  my ($set1, $set2) = @_;
  my %new = (%$set1, %$set2);
  \%new
}

sub cache_filepath($) {
  my $fp = shift;
  $fp =~ s/-/--/g;
  $fp =~ s/\//-/g;
  $fp =~ s/^-//g;
  catfile $cache, $fp
}

sub resolve_tree {
  sub resolve_tree_inner {
    my ($lib, $TREE) = @_;
    return if (defined $TREE->{$lib});
    $TREE->{$lib} = mkset(@{cache_get($lib)});
    foreach my $dep (keys %{$TREE->{$lib}}) {
      resolve_tree_inner($dep, $TREE);
    }
    values %$TREE
  }

  reduce { union($a, $b) } {}, resolve_tree_inner(@_)
}

sub cache_get {
  my $key = shift;
  if (defined $DEPS->{$key}) {
    $DEPS->{$key}
  } else {
    cache_insert($key);
    cache_get($key)
  }
}

sub cache_insert($) {
  my $key = shift;
  print STDERR "Finding dependencies for $key...\n";
  my @deps = find_deps($key);
  $DEPS->{$key} = \@deps;
}

sub find_deps($) {
  my $lib = shift;
  my($chld_in, $chld_out, $chld_err);
  my $pid = open3($chld_in, $chld_out, $chld_err, "", "-L", "-arch", "x86_64", $lib);
  waitpid($pid, 0);
  my $line = readline $chld_out;
  if($? == 0 and $line !~ /not an object file/) {
    my @libs;
    while(<$chld_out>) {
      my $dep = (split /\s+/)[1];
      push @libs, $dep unless $dep eq $lib or $dep =~ /\@rpath/;
    }
    @libs
  } elsif (-l $lib) {
    (realpath($lib))
  } else {
    ()
  }
}

if (defined $ARGV[0]) {
  my $deps = derivationFromPath($ARGV[0])->{"env"}->{"__impureHostDeps"};
  if (defined $deps) {
    my @files = split(/\s+/, $deps);
    my $depcache = {};
    my $depset = reduce { union($a, $b) } (map { resolve_tree($_, $depcache) } @files);
    print "extra-chroot-dirs\n";
    print join("\n", keys %$depset);
    print "\n";
  }
  lock_store($DEPS, $cache);
} else {
  print STDERR "Usage: $0 path/to/derivation.drv\n";
  exit 1
}
