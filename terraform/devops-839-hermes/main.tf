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

resource "aws_key_pair" "auth" {
  key_name   = "${var.key_name}"
  public_key = "${file(var.public_key_path)}"
}

data "template_file" "nixos_user_data" {
  template = "${file("${path.module}/configuration.nix")}"
  vars = {
    ssh_key = "${file(var.public_key_path)}"
    hostname = "${var.hostname}.${var.zone}"
  }
}

resource "aws_instance" "web" {
  ami                  = "${data.aws_ami.nixos.id}"
  instance_type        = "${var.instance_type}"
  user_data            = "${data.template_file.nixos_user_data.rendered}"

  vpc_security_group_ids = [
    "${aws_security_group.ssh_http.id}",
  ]

  key_name = "${aws_key_pair.auth.id}"

  tags {
    Name        = "DEVOPS-839 Hermes"
    Ticket      = "DEVOPS-839"
  }

  root_block_device = {
    volume_size = 20
  }
}

resource "aws_ebs_volume" "web_storage" {
  availability_zone = "${aws_instance.web.availability_zone}"
  size = 30
  tags {
    Name = "DEVOPS-839 Hermes Storage"
    Ticket = "DEVOPS-839"
  }
}

resource "aws_volume_attachment" "web_storage" {
  device_name = "/dev/sdf"
  volume_id   = "${aws_ebs_volume.web_storage.id}"
  instance_id = "${aws_instance.web.id}"
}

resource "aws_security_group" "ssh_http" {
  ## inbound (world): ssh
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): http
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): https
  ingress {
    from_port   = 443
    to_port     = 443
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
    Name        = "DEVOPS-839 Hermes SSH HTTP"
    Ticket      = "DEVOPS-839"
  }
}

# resource "aws_eip" "eip" {
#   instance = "${aws_instance.web.id}"
#   tags = {
#     Name = "devops-839-web-ip"
#     Ticket      = "DEVOPS-839"
#   }
# }

# output "ip" {
#   value = "${aws_eip.recovery_service_web_eip.public_ip}"
# }
