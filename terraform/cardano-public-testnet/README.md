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

### 5. MFA Token setup

At this stage, all user accounts are created and local deployer users
have their credentials.

The IAM users cannot be used without MFA, so this needs to be set up
for each user. The IAM users have permission to update their own MFA
details.

#### AWS Console Password

For each user with AWS access, decrypt the file
`~/username-console-password.gpg` using something like:

    cat username-console-password.gpg | base64 --decode | gpg --decrypt

For deployer users, the console password is encrypted with the
deployer's GPG key.

For developer users, the console password is encrypted with the user's
GPG key specified in [`global/users.tf`](./global/users.tf).

#### MFA Setup

Log in to [the console](https://117127962627.signin.aws.amazon.com/console) then
follow the [AWS MFA instructions](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_enable.html).

[andOTP](https://f-droid.org/en/packages/org.shadowice.flocke.andotp/)
from F-Droid is a good token app.

### 6. Finished global terraform

Log in as a user such as `testnet@testnet-deployer`.

Run:

    eval `aws-mfa [TOKEN]`
    aws ec2 describe-instances

This should work.

### 7. Please sir, can I have some more?

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

Place the generated "rich" keys where they can be deployed to core
nodes:

    cp -Rv genesis-keys/generated-keys/rich keys


### 3. Deploy testnet

From the directory `testnet@testnet-deployer:iohk-ops`.

    ln -s testnet.yaml config.yaml
    export NIXOPS_DEPLOYMENT=csl-testnet

    # datadog api
    nano static/datadog-api.secret
    nano static/datadog-application.secret

    # report server
    echo foo > static/zendesk-token.secret

    # recaptcha keypair for faucet
    nano static/recaptcha_site_key
    nano static/recaptcha_secret_key

    io deploy

### 4. Open Genesis Wallet in Daedalus

Copy a genesis "poor" key to a temporary location and use it to create
a wallet in Daedalus. Daedalus (Testnet build) needs to be running.

    curl -i https://localhost:8092/api/wallets/keys \
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

### 5. Transfer Testnet ADA to faucet

Use nixops to log in to the faucet node. After starting, it creates
the file `/var/lib/faucet/generated-wallet.json`. Send an amount of
ADA from the Genesis Wallet to the address in that file.

### 6. Withdraw Testnet ADA from faucet

In a new Daedalus Testnet wallet, create a new address to receive
payments. Use that address in the following query:

    curl -i -X POST http://cardano-faucet.cardano-testnet.iohkdev.io/withdraw -H 'Content-type: application/json;charset=utf-8' -d '{ "address": "DdzFFzCqrhso4y6jJo5zx8JGGVufuMNk2U1xXdrjaYDmZZT625mFQDtHHkVkicAt6dLP9wVoehGhxZnJHCYt2NWcD4sHn4PUvcWfcYkt" }'

    HTTP/1.1 200 OK
    Server: nginx
    Date: Tue, 19 Jun 2018 16:02:47 GMT
    Content-Type: application/json;charset=utf-8
    Transfer-Encoding: chunked
    Connection: keep-alive

    {"success":{"creationTime":"2018-06-19T16:02:41.478137","status":{"tag":"applying","data":{}},"amount":171794,"inputs":[{"amount":999999827807,"address":"DdzFFzCqrhsvHB5f1mciD9JLsFVeHy7TEsWVLSSLPe33hFLcoJfu9a11rCmy12GCqttCJ3WWnhkREKG1sFKxsLaU2VPwV5uxQPWknmCE"}],"direction":"outgoing","outputs":[{"amount":999999656013,"address":"DdzFFzCqrht3NGCA8BgSCfymLPP2FduQeo8V6u9atYxcbH2FKbqJCzYd2WGjwtFi9h3JSE2DJyiE1zBtJ3Dsb2Be1wwkTxZwt9PLUfFv"},{"amount":812,"address":"DdzFFzCqrhso4y6jJo5zx8JGGVufuMNk2U1xXdrjaYDmZZT625mFQDtHHkVkicAt6dLP9wVoehGhxZnJHCYt2NWcD4sHn4PUvcWfcYkt"}],"confirmations":0,"id":"8e356033bfb5de3d3866cb68d9a93c3bb6fef97788bd4228ebae86f97d6f9d83","type":"foreign"}}

Check http://cardano-explorer.cardano-testnet.iohkdev.io for the transaction ID.
