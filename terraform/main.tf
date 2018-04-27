terraform {
  required_version = ">= 0.10.3"

  backend "s3" {
    bucket = "iele-tf-state"
    key    = "iele_testnet/global-state"
    region = "eu-west-1"
  }
}

provider "aws" {
  region = "${var.aws_region}"
}
