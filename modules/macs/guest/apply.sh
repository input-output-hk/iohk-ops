#!/usr/bin/env bash

echo "apply started at $(date)" /dev/udp/@host@/@port@

printf '\n*.*\t@@host@:@port@\n' >> /etc/syslog.conf

scutil --set HostName @hostname@
scutil --set LocalHostName @hostname@
scutil --set ComputerName @hostname@
dscacheutil -flushcache

exec 3>&1
exec 2> >(nc -u @host@ @port@)
exec 1>&2

pkill syslog
pkill asl
sudo systemsetup -setcomputersleep Never
echo "preventing sleep with caffeinate"
sudo caffeinate -s &

PS4='${BASH_SOURCE}::${FUNCNAME[0]}::$LINENO '
set -o pipefail
set -ex
date

function finish {
    set +e
    cd /
    sleep 1
    umount -f /Volumes/CONFIG
}
trap finish EXIT

cat <<EOF >> /etc/ssh/sshd_config
PermitRootLogin prohibit-password
PasswordAuthentication no
PermitEmptyPasswords no
ChallengeResponseAuthentication no
EOF

launchctl stop com.openssh.sshd
launchctl unload /System/Library/LaunchDaemons/com.apple.platform.ptmd.plist

cd /Volumes/CONFIG

cp -rf ./etc/ssh/ssh_host_* /etc/ssh
chown root:wheel /etc/ssh/ssh_host_*
chmod 600 /etc/ssh/ssh_host_*
launchctl start com.openssh.sshd
cd /

echo "%admin ALL = NOPASSWD: ALL" > /etc/sudoers.d/passwordless

(
    # Make this thing work as root
    # shellcheck disable=SC2030,SC2031
    export USER=root
    # shellcheck disable=SC2030,SC2031
    export HOME=~root
    export ALLOW_PREEXISTING_INSTALLATION=1
    env
    curl https://nixos.org/releases/nix/nix-2.1.3/install > ~nixos/install-nix
    sudo -i -H -u nixos -- sh ~nixos/install-nix --daemon < /dev/null
)

(
    # Make this thing work as root
    # shellcheck disable=SC2030,SC2031
    export USER=root
    # shellcheck disable=SC2030,SC2031
    export HOME=~root

    mkdir -pv /etc/nix
cat <<EOF > /etc/nix/nix.conf
substituters = http://@host@:8081
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
EOF

    # shellcheck disable=SC1091
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
    env
    ls -la /private || true
    ls -la /private/var || true
    ls -la /private/var/run || true
    ln -s /private/var/run /run || true
    nix-channel --add https://nixos.org/channels/nixos-19.03 nixpkgs
    nix-channel --add https://github.com/LnL7/nix-darwin/archive/master.tar.gz darwin
    nix-channel --update

    sudo -i -H -u nixos -- nix-channel --add https://nixos.org/channels/nixos-19.03 nixpkgs
    sudo -i -H -u nixos -- nix-channel --add https://github.com/LnL7/nix-darwin/archive/master.tar.gz darwin
    sudo -i -H -u nixos -- nix-channel --update

    export NIX_PATH=$NIX_PATH:darwin=https://github.com/LnL7/nix-darwin/archive/master.tar.gz

    installer=$(nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer --no-out-link)
    set +e
    yes | sudo -i -H -u nixos -- "$installer/bin/darwin-installer"
    echo $?
    sudo launchctl kickstart system/org.nixos.nix-daemon
    set -e
    sleep 30
)
(
    if [ -d /Volumes/CONFIG/buildkite ]
    then
      cp -a /Volumes/CONFIG/buildkite /Users/nixos/buildkite
    fi
)
(
    # shellcheck disable=SC2031
    export USER=root
    # shellcheck disable=SC2031
    export HOME=~root

    rm -f /etc/nix/nix.conf
    rm -f /etc/bashrc
    ln -s /etc/static/bashrc /etc/bashrc
    # shellcheck disable=SC1091
    . /etc/static/bashrc
    cp -vf /Volumes/CONFIG/darwin-configuration.nix ~nixos/.nixpkgs/darwin-configuration.nix
    cp -vrf /Volumes/CONFIG/iohk-ops ~nixos/.nixpkgs/iohk-ops
    chown -R nixos ~nixos/.nixpkgs
    sudo -i -H -u nixos -- darwin-rebuild switch
)
