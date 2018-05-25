variable "aws_region" {
  default = "eu-west-1"
  description = "AWS region to deploy resources"
}

variable "instance_type" {
  default = "t2.small"
}

variable "storage_size" {
  default = 30
  description = "Size in GB of attached EBS volume"
}

variable "public_key_path" {
  default = "ssh-key.pub"
  description = "Filename of the SSH public key to use. This file must exist."
}

variable "key_name" {
  default = "devops-839-hermes"
  description = "Name for keypair entry in EC2"
}

variable "development_arn" {
  default = "arn:aws:iam::341718127296:role/development"
  description = "IAM role to switch to for deploying resources"
}

variable "hostname" {
  default = "hermes"
  description = "Name of instance."
}

variable "zone" {
  default = "dev.iohkdev.io"
  description = "Domain of instance."
}
