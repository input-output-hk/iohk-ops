terraform {
  required_version = "~> 0.11"

  backend "s3" {
    bucket  = "iohk-ops-tfstate"
    key     = "remote-tfstate/global-state/terraform.tfstate"
    region  = "us-west-1"
    encrypt = true  # fixme: what key does this use?
  }
}

provider "aws" {
  version = "~> 1.10"
  region  = "eu-west-1"
  assume_role {
    role_arn = "${var.profile_arn}"
  }
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
}

module "deployer" {
  source = "./deployer"
  aws_region = "eu-west-1"
  env = "testnet"
}

output "deployer_ssh" {
  value = "${module.deployer.deployer_ssh}"
}
