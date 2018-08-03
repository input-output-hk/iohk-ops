variable "workspace_iam_roles" {
  description = "AWS subaccount role for deploying resources"

  default = {
    testnet = "arn:aws:iam::117127962627:role/cardano-public-testnet"
    mainnet = "arn:aws:iam::PRODUCTION-ACCOUNT-ID:role/Terraform"
  }
}

variable "aws_region" {
  description = "AWS region to put deployer in"
  default = "eu-west-1"
}

variable "instance_type" {
  description = "Size of EC2 instance"
  # 8 vCPU / 61GB RAM - $0.593 per hour in Ireland
  default = "r4.2xlarge"
}

variable "volume_size" {
  description = "Size in GB of deployer root filesystem volume"
  default = 100
}

variable "username" {
  description = "Username for deployer login"
  default = "deployer"
}

variable "termination_protection" {
  description = "Prevent the instance being destroyed, except through web console."
  default = true
}
