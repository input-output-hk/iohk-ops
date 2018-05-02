terraform {
  required_version = ">= 0.10.3"
}

variable "aws_region" {
}

provider "aws" {
  alias = "${var.aws_region}"
  region = "${var.aws_region}"
}
