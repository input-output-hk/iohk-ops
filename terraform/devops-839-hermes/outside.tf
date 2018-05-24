
# provider "aws" {
#   alias  = "dns"
#   region = "eu-west-1"
# }

resource "aws_route53_zone" "dev" {
  # provider = "aws.dns"
  name = "${var.zone}"

  tags {
    Environment = "Development"
    Ticket = "DEVOPS-839"
  }
}

resource "aws_route53_record" "dev-ns" {
  # provider = "aws.dns"
  zone_id = "${aws_route53_zone.dev.zone_id}"
  name    = "${var.zone}"
  type    = "NS"
  ttl     = "30"

  records = [
    "${aws_route53_zone.dev.name_servers.0}",
    "${aws_route53_zone.dev.name_servers.1}",
    "${aws_route53_zone.dev.name_servers.2}",
    "${aws_route53_zone.dev.name_servers.3}",
  ]
}

resource "aws_route53_record" "web_dns" {
  # provider = "aws.dns"
  zone_id = "${aws_route53_zone.dev.zone_id}"
  name    = "${var.hostname}.${var.zone}"
  type    = "A"
  ttl     = "30"
  records = [ "${aws_instance.web.public_ip}" ]
}
