terraform {
  required_version = "~> 0.11"

  backend "s3" {
    bucket  = "iohk-ops-tfstate"
    key     = "remote-tfstate/devops-839-hermes/terraform.tfstate"
    region  = "us-west-1"
    encrypt = true  # fixme: what key does this use?
  }
}

provider "aws" {
  version = "~> 1.10"
  region  = "eu-west-1"
  assume_role {
    role_arn = "${var.development_arn}"
  }
}

provider "template" {
  version = "~> 1.0"
}
