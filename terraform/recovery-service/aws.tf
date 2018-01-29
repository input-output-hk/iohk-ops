provider "aws" {
  access_key = "${var.access_key}"
  secret_key = "${var.secret_key}"
  region     = "${var.aws_region}"
}

terraform {
  backend "s3" {
    bucket  = "iohk-ops-tfstate"
    key     = "remote-tfstate/recovery-service/terraform.tfstate"
    region  = "us-west-1"
    encrypt = true
  }
}
