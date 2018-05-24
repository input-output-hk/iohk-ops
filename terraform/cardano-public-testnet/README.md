# Global Terraform Deployment

Deploy this from `deployer@testnet-deployer`. The correct version of
Terraform and necessary plugins should already have been installed by
nixops.

## 1. Credentials setup

### PGP Key

Import or generate the deployer GPG key.

    gpg --import
    > -----BEGIN PGP PRIVATE KEY BLOCK-----
    > ...
    gpg --edit-key A3738828EF38D2B9CE57538C67C525F74161862F
    > trust
    > 5
    > y
    > quit

This key is used to encrypt the secret keys and passwords of IAM users
deployed by terraform.

### AWS Root Account Credentials

Place credentials for AWS root account in `~/.aws/credentials`:

    mkdir ~/.aws
    cat > ~/.aws/credentials <<EOF
    [default]
    aws_access_key_id = AKIA...
    aws_secret_access_key = ...
    EOF

## 2. Terraform wants to init

    cd iohk-ops/terraform/cardano-public-testnet
    terraform init

The Terraform state file is stored in S3 so that it can easily be
restored (through `terraform init`) if the deployer is lost.

## 3. Deploy

    terraform apply

## 4. Install keys

This copies the AWS credentials out of the Terraform state file (and
locally written output files) into the home directories of deployer
users and developers.

    ./install_aws_credentials.sh

## 5. Finished global terraform

At this stage, all user accounts are created and local deployer users
have their credentials.

Log in as a user such as `testnet@testnet-deployer`.

Enable MFA on the given user using AWS console (fixme: password setup
and instructions) and set up the token app (andOTP from F-Droid is
good).

Run:

    eval `aws-mfa [TOKEN]`
    aws ec2 describe-instances

This should work.

## 6. Please sir, can I have some more?

The network needs elastic IPs, lots of them.

For starters, make a request under the testnet account to increase VPC
EIP limits.

 * eu-central-1: 8


## 7. Cardano Genesis Data Preparation

Log in to `testnet@testnet-deployer` and clone iohk-ops. Branch should
be master probably.

    io clone iohk-ops BRANCH
    cd iohk-ops

    nix-shell -A withAuxx


    # need a configuration file from cardano-sl
    git clone https://github.com/input-output-hk/cardano-sl -b devops-398-testnet

    # generate some parameters
    python -c "import secrets; print(secrets.randbelow(2**256))" > testnet-seed.txt
    python -c "import secrets; print(secrets.randbelow(2**256))" > testnet-seed2.txt
    python -c "import datetime; print(round(datetime.datetime.utcnow().timestamp()))" > start-time.txt

    cardano-keygen --system-start `cat start-time.txt` --configuration-file cardano-sl/lib/configuration.yaml --configuration-key testnet_gen --configuration-seed `cat testnet-seed.txt` dump-genesis-data --path cardano-sl/lib/testnet-genesis.json

    ./scripts/js/genesis-hash.js lib/testnet-genesis.json
    # put this hash into cardano-sl/lib/configuration.yaml

    cardano-keygen --system-start 0 --configuration-file cardano-sl/lib/configuration.yaml --configuration-key testnet_gen --configuration-seed `cat testnet-seed2.txt` generate-keys-by-spec --genesis-out-dir genesis-keys

    cp -Rv genesis-keys/generated-keys/rich keys

    # commit this and push, update cardano revision


## 8. Deploy testnet

From the directory `testnet@testnet-deployer:iohk-ops`.

    ln -s testnet.yaml config.yaml
    export NIXOPS_DEPLOYMENT=csl-testnet

    # # create 7 keys
    # mkdir keys
    # for i in 0 1 2 3 4 5 6; do
    #   cardano-keygen --configuration-file cardano-sl/lib/configuration.yaml --configuration-key testnet_full generate-key --path keys/key$i.sk
    # done


    # datadog api
    nano static/datadog-api.secret
    nano static/datadog-application.secret
    echo foo > static/zendesk-token.secret

    io deploy
