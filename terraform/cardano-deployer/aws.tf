terraform {
  required_version = "~> 0.11"

  backend "s3" {
    bucket  = "iohk-ops-tfstate"
    key     = "remote-tfstate/cardano-deployer/terraform.tfstate"
    region  = "us-west-1"
    encrypt = true  # AWS KMS encryption of S3 files
  }
}

provider "aws" {
  # No credentials explicitly set here because they come from either the
  # environment or the global credentials file.

  version = "~> 1.19"
  region  = "eu-west-1"

  assume_role {
    role_arn = "${var.workspace_iam_roles[terraform.workspace]}"
  }
}

provider "template" {
  version = "~> 1.0"
}
