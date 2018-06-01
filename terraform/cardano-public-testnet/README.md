# Cardano SL Public Testnet Deployment

## Global Terraform

The global terraform files configure AWS resources such as IAM users,
policies, S3 buckets, DNS entries. The idea is to manage this
configuration in code so that it's easier to audit and maintain.

Each developer will have their own AWS credentials installed under
their own UNIX user account. MFA is required for everyone including
devops. Private information is protected with file permissions. All
developers share the same AWS subaccount however.

There are separate AWS credentials to deploy the testnet, under a
separate `testnet` user, using a separate `cardano-public-testnet` AWS
subaccount.

Deploy the global terraform code from `deployer@testnet-deployer`. The
correct version of Terraform and necessary plugins should already have
been installed by NixOps on the deployer host.

For deployment of the deployer, see
[../cardano-deployer/README.md](../cardano-deployer/README.md).

### 1. Credentials setup

#### PGP Key

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

#### AWS Root Account Credentials

Place credentials for AWS root account in `~/.aws/credentials`:

    mkdir ~/.aws
    cat > ~/.aws/credentials <<EOF
    [default]
    aws_access_key_id = AKIA...
    aws_secret_access_key = ...
    EOF

### 2. Terraform wants to init

    cd iohk-ops/terraform/cardano-public-testnet
    terraform init

The Terraform state file is stored in S3 so that it can easily be
restored (through `terraform init`) if the deployer is lost.

### 3. Deploy

    terraform apply

### 4. Install keys

This copies the AWS credentials out of the Terraform state file (and
locally written output files) into the home directories of deployer
users and developers.

    ./install_aws_credentials.sh

### 5. Finished global terraform

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

### 6. Please sir, can I have some more?

The network needs elastic IPs, lots of them.

For starters, make a request under the testnet account to increase VPC
EIP limits.

 * eu-central-1: 8



## Cardano SL network deployment with iohk-ops

Log in to `testnet@testnet-deployer` and clone iohk-ops. Branch should
be master probably.

    io clone iohk-ops BRANCH

Get yourself some temporary credentials with your preconfigured MFA
for the testnet IAM user.

    [testnet@testnet-deployer:~]$ eval `aws-mfa`
    Enter MFA Token and press [ENTER]: 447784


### 1. Cardano Genesis Data Preparation

    cd iohk-ops
    nix-shell -A withAuxx

    # need a configuration file from cardano-sl
    git clone https://github.com/input-output-hk/cardano-sl -b develop

    # generate some parameters
    python -c "import secrets; print(secrets.randbelow(2**256))" > testnet-seed.txt
    python -c "import datetime; print(round(datetime.datetime.utcnow().timestamp()))" > start-time.txt

    cardano-keygen --system-start `cat start-time.txt` --configuration-file cardano-sl/lib/configuration.yaml --configuration-key testnet_launch --configuration-seed `cat testnet-seed.txt` dump-genesis-data --path cardano-sl/lib/testnet-genesis.json

    genesis-hash cardano-sl/lib/testnet-genesis.json


Put the resulting hash into `cardano-sl/lib/configuration.yaml` under
the `testnet_full` section. Commit both `configuration.yaml` and
`testnet-genesis.json`, and then update `iohk-ops/cardano-sl-src.json`.


### 2. Key generation

Use the seed to get the genesis keys. The rich keys are deployed to
core nodes and the poor keys can be imported into a wallet and their
ADA spent.

    cardano-keygen --system-start 0 --configuration-file cardano-sl/lib/configuration.yaml --configuration-key testnet_launch --configuration-seed `cat testnet-seed.txt` generate-keys-by-spec --genesis-out-dir genesis-keys

Place the generated rich keys where they can be deployed to core
nodes:

    cp -Rv genesis-keys/generated-keys/rich keys


### 3. Deploy testnet

From the directory `testnet@testnet-deployer:iohk-ops`.

    ln -s testnet.yaml config.yaml
    export NIXOPS_DEPLOYMENT=csl-testnet

    # datadog api
    nano static/datadog-api.secret
    nano static/datadog-application.secret
    echo foo > static/zendesk-token.secret

    io deploy

### 4. Transfer Genesis ADA

Use a genesis key to create a wallet in Daedalus. Daedalus Testnet
needs to be running and the key should be copied to a temporary
location.

    curl -i https://localhost:8090/api/wallets/keys \
      --cacert ~/.local/share/Daedalus/testnet/tls/server/ca.crt \
      --cert ~/.local/share/Daedalus/testnet/tls/server/server.crt \
      --key ~/.local/share/Daedalus/testnet/tls/server/server.key \
      -H 'cache-control: no-cache' \
      -H 'content-type: application/json' \
      -d "\"$HOME/secret/key0.sk\""

    HTTP/2 200
    date: Sun, 17 Jun 2018 11:34:36 GMT
    server: Warp/3.2.22
    content-type: application/json;charset=utf-8

    {"Right":{"cwId":"Ae2tdPwUPEZMdfwG6TGDEU24TcBURhubkqy7ExrAefGFyMTCtee5cnrvNSB","cwMeta":{"cwName":"Genesis wallet","cwAssurance":"CWANormal","cwUnit":0},"cwAccountsNumber":1,"cwAmount":{"getCCoin":"9651253048499"},"cwHasPassphrase":false,"cwPassphraseLU":1.529235276571176605e9}}
