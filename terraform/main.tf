terraform {
  required_version = ">= 0.10.3"

  backend "s3" {
    bucket = "iele-tf-state"
    key    = "iele_testnet/global-state"
    region = "eu-west-1"
  }
}

provider "aws" {
  region = "eu-west-1"
}

module "eu-west-1" {
  source     = "./regional"
  aws_region = "eu-west-1"
}

module "eu-west-2" {
  source     = "./regional"
  aws_region = "eu-west-2"
}

module "global" {
  source = "./global"
}
