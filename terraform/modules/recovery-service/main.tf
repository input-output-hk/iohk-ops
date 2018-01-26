provider "aws" {
  region = "${var.aws_region}"
}

resource "aws_instance" "recovery_service_web" {
  tags = {
    Name = "Recovery Service Web"
  }

  connection {
    user = "ubuntu"
  }

  instance_type = "t2.micro"

  # Lookup the correct AMI based on the region
  # we specified
  ami = "${lookup(var.aws_amis, var.aws_region)}"

  security_groups = ["${aws_security_group.recovery_firewall.name}"]

  provisioner "remote-exec" {
    inline = [
      "sudo apt-get -y update"
    ]
  }
}

resource "aws_security_group" "recovery_firewall" {
  name = "recovery-firewall"
  description = "recovery service firewall"
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = [ "0.0.0.0/0" ]
  }

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = [ "0.0.0.0/0" ]
  }

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = [ "0.0.0.0/0" ]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_key_pair" "auth" {
  key_name   = "${var.key_name}"
  public_key = "${file(var.public_key_path)}"
}

### fixme: we have run out of elastic IPs for the time being
# resource "aws_eip" "recovery_service_web_eip" {
#   instance = "${aws_instance.recovery_service_web.id}"
#   vpc      = false
# }



# data "aws_route53_zone" "selected" {
#   name         = "test.com."
#   private_zone = false
# }

# resource "aws_route53_record" "recovery_service_web_dns" {
#   zone_id = "${data.aws_route53_zone.selected.zone_id}"
#   name    = "www.${data.aws_route53_zone.selected.name}"
#   type    = "A"
#   ttl     = "300"
#   records = ["${aws_eip.recovery_service_web_eip.public_ip"]
# }
