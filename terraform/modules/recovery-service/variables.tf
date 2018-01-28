variable "public_key_path" {
  description = <<DESCRIPTION
Path to the SSH public key to be used for authentication.
Ensure this keypair is added to your local SSH agent so provisioners can
connect.

Example: ~/.ssh/terraform.pub
DESCRIPTION
}

variable "key_name" {
  description = "Desired name of AWS key pair"
}

variable "access_key" {
  description = "AWS access key"
}

variable "secret_key" {
  description = "AWS secret key"
}

variable "aws_region" {
  description = "AWS region to launch servers."
  default     = "ap-northeast-1"
}

# ap-northeast-1	xenial	16.04 LTS	amd64	hvm:ebs-ssd	20180122	ami-4b7e1c2d	hvm
# ap-southeast-1	xenial	16.04 LTS	amd64	hvm:ebs-ssd	20180122	ami-da3d45a6	hvm
# eu-central-1		xenial	16.04 LTS	amd64	hvm:ebs-ssd	20180122	ami-37940d58	hvm
variable "aws_amis" {
  default = {
    ap-northeast-1 = "ami-4b7e1c2d"
    ap-southeast-1 = "ami-da3d45a6"
    eu-central-1   = "ami-37940d58"
  }
}
