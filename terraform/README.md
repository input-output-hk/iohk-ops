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

# Global Terraform Config

This terraform project should be used to manage all infrastructure
that is not project specific.

## What should go in here

* IAM Users
* Roles that will be shared accross projects e.g. EC2 access to ECR
* Resources that will be shared accross projects, e.g. s3 buckets

## What should not go in here

Projects that can have multiple versions of the same infrastructure set up, for example iele-testnet can have a staging VPC and a prod VPC running at the same time. These should be managed using terraform workspaces. Note that the permissions of these workspaces can be restricted as they are s3 folders and these restrictions *should* go in this global terraform project.

## Multi region resources

This project uses multiple modules in order to allow the management of resources in multiple regions. The 'default' region is `eu-west-1` which means that any global resources such as IAM resources are managed through the `eu-west-1` region. This really is of no conscequence it's just where we were doing most of our work at the time.

The `regional` folder is for resources that should be created in all regions that we want to do stuff in. If you need to create a resource in only 1 region you should create a new module for that region.

When you are creating resources in a region, you must add the `region` attribute to all aws resources, e.g.

```
resource "aws_s3_bucket" "nix_cache" {
  provider = "aws.${var.aws_region}"
  bucket   = "iele-nix-cache-${var.aws_region}"
  acl      = "private"
  region   = "${var.aws_region}"
}
```

## Setting up IAM users

1. Ask the user to send you their gpg public key, this should be in binary format, base64 encoded: `gpg --export myuser | base64 > myuser.pub`
2. Copy their key to `global/keys/theuser.base64` and create a new IAM user and related resources in `global/users.tf`
3. Run `terraform plan` and if everything looks good `terraform apply`
4. There should be 2 new files in the `global/private` directory, these are the AWS console login password and the secret for cli access, send these to the user
5. The user needs to decrypt and decode them with `base64 -d < password.txt | gpg -d` and `base64 -d < secret.txt | gpg -d`
6. Ask the user to login to AWS console using the username you used in users.tf and the password you sent them
7. The user should now [set up MFA](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_enable_virtual.html)
8. The user can view their AWS_ACCESS_KEY in the Summary section of their user's IAM page, see [here](https://console.aws.amazon.com/iam/home?region=eu-west-1#/users)
9. The user should add a new entry in their `~/.aws/credentials` file (figure 1)
10. The user should save the script in figure 2 as getcreds.sh
11. The user can now run `eval $(~/getcreds.sh 123456)` where `123456` is the MFA code provided by their MFA device
12. The user should now be able to access aws resources

### Figure 1 - aws credentials entry

Fixme -- these are automatically installed on the deployer.

```
[myaccount]
aws_access_key_id = THEACCESSKEY
aws_secret_access_key = THESECRET
```

### Figure 2 - getcreds.sh

Fixme -- this is replaced by the script in `mfa.nix`

Replace `<AWSUSERNAMEHERE>` with your aws user name

```
#!/bin/bash

export AWS_PROFILE=myaccount
unset AWS_SESSION_TOKEN
unset AWS_SECRET_ACCESS_KEY
unset AWS_ACCESS_KEY_ID

aws sts get-session-token --serial-number arn:aws:iam::920648890259:mfa/<AWSUSERNAMEHERE> --output text --duration-seconds 86400 --token-code $1 \
        | awk '{printf("export AWS_ACCESS_KEY_ID=\"%s\"\nexport AWS_SECRET_ACCESS_KEY=\"%s\"\nexport AWS_SESSION_TOKEN=\"%s\"\n",$2,$4,$5)}'
```
