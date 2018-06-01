terraform {
  required_version = "~> 0.11"

  backend "s3" {
    bucket  = "iohk-ops-tfstate"
    key     = "remote-tfstate/global-state/terraform.tfstate"
    region  = "us-west-1"
    encrypt = true  # AWS KMS encryption of S3 files
  }
}

provider "aws" {
  version = "~> 1.19"
  region  = "eu-west-1"
  assume_role {
    role_arn = "${var.profile_arn}"
  }
}

provider "local" {
  version = "~> 1.1"
}

# module "eu-west-1" {
#   source     = "./regional"
#   aws_region = "eu-west-1"
# }

# module "eu-west-2" {
#   source     = "./regional"
#   aws_region = "eu-west-2"
# }

module "global" {
  source = "./global"
  deployer_pgp_key = "${file("${path.module}/keys/deployer.base64")}"
}

# module "testnet_installer_bucket" {
#   source      = "./modules/installer_bucket"
#   bucket_name = "updates.cardano-testnet.iohkdev.io"
#   prefix      = "updates-cardano-testnet"
#   aws_region  = "ap-southeast-1"
#   group       = "${aws_iam_group.deployers.name}"
# }
# fixme: add dns entry for s3 bucket
