This folder contains terraform snippets to administer AWS.

There are some common set up steps for each snippet. After completing
the prerequisites, look into each folder for further instruction.

# Prerequisites

## Shared State Bucket

To start with, create a bucket to store terraform state, if it hasn't
already been created.

- name: iohk-ops-tfstate
- region: US West (N. California)
- turn on versioning

## Shell

Get the correct version of terraform in your `PATH` with:

    nix-shell

## AWS Credentials

Side note: terraform will pick credentials from `~/.aws/credentials`
and you can switch profiles by doing:

     export AWS_PROFILE=iohk

## Initialize terraform

To create the state file and see what terraform will do:

    cd terraform/$SNIPPET
    terraform init
    terraform plan
