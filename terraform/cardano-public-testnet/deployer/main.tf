variable "aws_region" {}
variable "env" {}

data "aws_ami" "nixos" {
  most_recent = true

  filter {
    name   = "name"
    values = ["nixos-18.03.*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["080433136561"] # NixOS
}

resource "aws_key_pair" "deployer" {
  key_name   = "deployer-key"
  public_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC9soUucXq/jZvm4u5a49m+aB2+l1w8VRyrjcjMGHZslHZYtMuI6vNJ9AsK+cFEirq888ITa/ucriMInukvzP3WCRthvWgPINsbupOpaHxX0k6N2RRYZQbSeKMzjhnoIkM1GkrjHuRAEjUN4RbcbEzhgVGranb8+Mb6UIkFfCwgJJdzX8X9QWStVoUsO7C+x8+m1cYkxdWYrpGqyXZ+g9P7K2rKlfoz4kEAyo4Mivh8+xmO7bPSLpGuBgM7bt4Yyaq1YSuLOp5f5P4Nsa5MmXKANumEZqVNzgLlommB/3xr7N6q+K1nLt/OxvrxrNVMpwL/TYmTRGQ/UVQziglCQz1p rodney@aurora"
}

resource "aws_instance" "deployer" {
  ami                  = "${data.aws_ami.nixos.id}"
  # instance_type        = "r3.2xlarge"
  instance_type        = "t2.small"
  user_data            = "${file("${path.module}/configuration.nix")}"

  # fixme: this might be helpful
  # disable_api_termination = "${var.termination_protection}"

  # this allows attaching a role to the instance. however if anyone
  # but devops are allowed to login to the deployer machine then this
  # is useless because they will get all the privileges of the
  # deployer role.
  # fixme: how to do key rotation
  # iam_instance_profile = "deployer"

  vpc_security_group_ids = [
    "${aws_security_group.deployer.id}",
  ]

  key_name = "${aws_key_pair.deployer.id}"

  tags {
    Name        = "${var.env}_deployer"
    Environment = "${var.env}"
  }

  root_block_device = {
    volume_size = 20
  }
}

resource "aws_security_group" "deployer" {
  # vpc_id = "${aws_vpc.iele_testnet.id}"

  ## inbound (world): ssh
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## outgoing: all
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  tags = {
    Name        = "${var.env}_deployer"
    Environment = "${var.env}"
  }
}
