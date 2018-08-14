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

provider "aws" {
  alias = "singapore"
  region = "ap-southeast-1"
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
  deployer_pgp_key = "${file("${path.module}/../../lib/gpg-keys/deployer.base64")}"
  providers = {
    aws = "aws"
    aws.singapore = "aws.singapore"
  }
}
