# Cardano Testnet Deployment

## 1. Set up terraform locally

1. AWS credentials.

       export AWS_PROFILE=iohk

2. Clone `iohk-ops`.

       git clone iohk-ops
       cd iohk-ops/terraform/cardano-public-testnet

3. Get a `nix-shell` with terraform.

       nix-shell ../shell.nix

4. Change variables (fixme)

       vim terraform.tfvars

5. Terraform wants to init.

       terraform init

## 2. Provision deployer instance

This will create a deployer instance:

    terraform apply

It will show the IP of the deployer instance.

You will need to wait a little while before logging in because it
needs time to apply the NixOS configuration. If it won't accept your
devops ssh key then try waiting a little longer.

## 3. Set up self-deployment of deployer

    ln -s /nix/store/something-iohk-ops/testnet-infra.yaml config.yaml
    io clone ...
