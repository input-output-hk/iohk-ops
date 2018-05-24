
resource "aws_route53_zone" "cardano_testnet" {
  name = "cardano-testnet.iohkdev.io"
  tags {
    Environment = "testnet"
  }
}

resource "aws_route53_record" "cardano_testnet_ns" {
  zone_id = "${aws_route53_zone.cardano_testnet.zone_id}"
  name    = "cardano-testnet.iohkdev.io"
  type    = "NS"
  ttl     = "30"

  records = [
    "${aws_route53_zone.cardano_testnet.name_servers.0}",
    "${aws_route53_zone.cardano_testnet.name_servers.1}",
    "${aws_route53_zone.cardano_testnet.name_servers.2}",
    "${aws_route53_zone.cardano_testnet.name_servers.3}",
  ]
}

# Put this temporary zone into cardano public testnet to keep nixops happy.
# It's only for development clusters so should be moved to development account.
resource "aws_route53_zone" "aws_iohkdev_temporary" {
  name = "aws.iohkdev.io"
  tags {
    Environment = "development"
  }
}
