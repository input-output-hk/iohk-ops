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

data "template_file" "nixos_user_data" {
  template = "${file("${path.module}/configuration.nix")}"
  vars = {
    env = "${terraform.workspace}"
    username = "${var.username}"
  }
}

resource "aws_instance" "deployer" {
  ami = "${data.aws_ami.nixos.id}"
  instance_type = "${var.instance_type}"
  user_data = "${data.template_file.nixos_user_data.rendered}"

  disable_api_termination = "${var.termination_protection}"

  # this allows attaching a role to the instance. however if anyone
  # but devops are allowed to login to the deployer machine then this
  # is useless because they will get all the privileges of the
  # deployer role.
  # fixme: how to do key rotation
  # iam_instance_profile = "deployer"

  vpc_security_group_ids = [
    "${aws_security_group.deployer.id}",
  ]

  tags {
    Name        = "${terraform.workspace}_deployer"
    Environment = "${terraform.workspace}"
  }

  root_block_device = {
    volume_size = "${var.volume_size}"
  }

  lifecycle {
    ignore_changes = [ "user_data" ]
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

  ## inbound (world): mosh
  ingress {
    from_port   = 60000
    to_port     = 61000
    protocol    = "UDP"
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
    Name        = "${terraform.workspace}_deployer"
    Environment = "${terraform.workspace}"
  }
}

resource "aws_eip" "deployer_eip" {
  instance = "${aws_instance.deployer.id}"
  tags = {
    Name = "${terraform.workspace}_deployer_ip"
    Environment = "${terraform.workspace}"
  }
}
