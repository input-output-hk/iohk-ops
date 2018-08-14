#!/usr/bin/env bash

set -euo pipefail

top=$(cd "$(dirname "$0")"; pwd)
keys="$top/global/modules/user/private"

key_id() {
    terraform state show "module.global.module.$1.aws_iam_access_key.self" | awk -F= '/^id/ { print $2; }'
}

access_key() {
    base64 --decode "$keys/$1.secret" | gpg --quiet --decrypt
}

install_password() {
    user="$1"
    username="$2"
    out="/home/$user/$username-console-password.gpg"
    sudo cp "$keys/$username.password" "$out"
    sudo chown "$user:" "$out"
    sudo chmod 600 "$out"
}

iam_username() {
    terraform state show "module.global.module.user_$1.aws_iam_user_login_profile.self" | awk -F'= *' '/^id/ { print $2; }'
}

install_key() {
    user=$1
    id=$2
    secret=$3
    aws="/home/$user/.aws"
    sudo mkdir -p "$aws"
    sudo dd status=none "of=$aws/credentials" <<EOF
[default]
aws_access_key_id=$id
aws_secret_access_key=$secret
EOF
    sudo chown -R "$user": "$aws"
}

for user in staging testnet infra; do
    echo "Setting up deployers user $user"
    id=$(key_id user_deployer_$user)
    secret=$(access_key deployer.$user)
    install_key "$user" "$id" "$secret"
    install_password $user deployer.$user
done

developers_key_id="$(key_id user_deployer_development)"
developers_access_key="$(access_key deployer.development)"

for user in $(list-developers); do
    username=$(iam_username "$user")

    if [ -z "$username" ]; then
        echo "Setting up developers user $user with shared credentials..."
        install_key "$user" "$developers_key_id" "$developers_access_key"
    else
        echo "Setting up user $user with credentials for $username..."
        install_key "$user" "$(key_id "user_$user")" "$(access_key "$username")"
        install_password "$user" "$username"
    fi
done
