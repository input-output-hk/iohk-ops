### Setup

1. AWS credentials.

       export AWS_PROFILE=iohk

2. Clone `iohk-ops`.

       git clone iohk-ops devops-839-hermes
       cd terraform/devops-839-hermes

3. Get a `nix-shell` with terraform.

       nix-shell ../shell.nix

4. Terraform wants to init.

       terraform init

5. Create new SSH key.

       ssh-keygen -f ssh-key -N "" -C devops-839-hermes
       eval `ssh-agent`
       ssh-add ssh-key

6. Provision and deploy.

       terraform apply


### Details

This terraform deployment uses the S3 state backend so can be run from
anywhere.

The resources are provisioned under the development AWS account.


### Using

The storage volume is mounted at `/data`.

To login, do `ssh -i ssh-key vincent@hermes.dev.iohkdev.io`.

It's a NixOS system, and the user has `sudo` access.
