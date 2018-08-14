# Deploying the deployer

The idea is to first run terraform from your laptop to create a NixOS
instance. This is then fully deployed with NixOps by using
`deployment.targetHost = "localhost"`.

You can see the [`configuration.nix`](./configuration.nix) for this
stage is minimal, and basically just enough to get the deployer
running.

The Terraform state is stored in S3 under a *workspace*. There are two
deployers. Use either `testnet` or `mainnet` as the workspace.


## 1. Set up terraform locally

1. AWS credentials.

       export AWS_PROFILE=iohk

2. Clone `iohk-ops`.

       git clone iohk-ops
       cd iohk-ops/terraform/cardano-deployer

3. Get a `nix-shell` with terraform.

       nix-shell ../shell.nix


## 2. Init Terraform and Workspace

1. Terraform wants to init. Let Terraform init.

       terraform init

2. For the very first deployment, you need to create a
   workspace. There are two workspaces matching the two deployers:
   `testnet` and `mainnet`.

       terraform workspace new testnet

   The workspace is now stored in the S3 state bucket.

3. Switch to the workspace.

       terraform workspace select testnet


## 3. Provision deployer instance

This will create a deployer instance:

    terraform apply

It will show the IP of the deployer instance and the username.

There will be no DNS record for the deployer. Create a SSH snippet to
make it easier to use:

    Host testnet-deployer
        User deployer
        Hostname 1.2.3.4

You will need to wait a little while before logging in because it
needs time to apply the NixOS configuration. If it won't accept your
devops ssh key then try waiting a little longer.

## 4. Deploy deployer instance

On the deployer host:

    ssh deployer@testnet-deployer

Generate a keypair so that nixops can ssh to `root@localhost`. NixOps
will shortly replace this with its own keypair.

    ssh-keygen -t ecdsa -N ""
    sudo cp /home/deployer/.ssh/id_ecdsa.pub /etc/ssh/authorized_keys.d/root

Clone iohk-ops and create nixops network. _BRANCH_ should probably be `master`.

    io clone iohk-ops BRANCH
    cd iohk-ops
    nixops create -d testnet-deployer ./deployments/deployer-testnet.nix ./deployments/deployer-target-aws.nix

Install API keys for DataDog agent and tarsnap:

    nano static/datadog-api.secret
    nano static/tarsnap-testnet-deployer.secret

Deploy the deployer:

    nixops deploy -d testnet-deployer

Repeat the above command whenever the you want to update the deployer
host.

## 5. Apply terraform on the deployer

Normally you want to do Terraform applications on the deployer host
rather than on your laptop. Both are possible, but the former is more
traceable.

Using the `deployer@testnet-deployer` user:

    cd iohk-ops/terraform/cardano-deployer
    terraform init
    terraform workspace select testnet
    terraform plan

It should say "No changes. Infrastructure is up-to-date."


## 6. Deploy global terraform

Follow the instructions in
[`../cardano-public-testnet/README.md`](../cardano-public-testnet/README.md).
