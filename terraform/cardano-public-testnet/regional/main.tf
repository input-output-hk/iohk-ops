terraform {
  required_version = "~> 0.11"
}

variable "aws_region" {}

provider "aws" {
  version = "~> 1.10"
  alias  = "${var.aws_region}"
  region = "${var.aws_region}"
}
