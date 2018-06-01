#!/usr/bin/env bash

set -e

top=$(cd `dirname $0`; pwd)
keys="$top/global/modules/user/private"

key_id() {
    terraform state show module.global.module.$1.aws_iam_access_key.self | awk -F= '/^id/ { print $2; }'
}

access_key() {
    base64 --decode $keys/$1.secret | gpg --quiet --decrypt
}

install_key() {
    user=$1
    id=$2
    secret=$3
    aws=/home/$user/.aws
    sudo mkdir -p $aws
    sudo dd status=none of=$aws/credentials <<EOF
[default]
aws_access_key_id=$id
aws_secret_access_key=$secret
EOF
    sudo chown -R $user: $aws
}

for user in staging testnet infra; do
    echo "Setting up deployers user $user"
    id=$(key_id user_deployer_$user)
    secret=$(access_key deployer.$user)
    install_key $user $id $secret
done

# fixme: install credentials for all developers defined in
# ../../ssh-keys.nix

# dev_id=$(key_id user_deployer_development)
# dev_secret=$(access_key deployer.development)

# for user in rodney; do
#     echo "Setting up developers user $user"
#     install_key $user $dev_id $dev_secret
# done

echo "Setting up developers user rodney"
install_key rodney $(key_id user_rodney_lorrimar) $(access_key rodney.lorrimar)
