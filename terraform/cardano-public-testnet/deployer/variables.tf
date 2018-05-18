variable "aws_region" {
  description = "AWS region to put deployer in"
}

variable "instance_type" {
  description = "Size of EC2 instance"
  # default = "r3.2xlarge"
  default = "t2.large"
}

variable "volume_size" {
  description = "Size in GB of deployer root filesystem volume"
  default = 100
}

variable "env" {
  description = "Name of environment"
}

variable "username" {
  description = "Username for deployer login"
  default = "infra"
}

# fixme: this might be helpful
variable "termination_protection" {
  description = "Prevent the instance being destroyed, except through web console."
  default = false
}
