module "testnet_installer_bucket" {
  source      = "./modules/installer_bucket"
  bucket_name = "updates-cardano-testnet"
  prefix      = "updates-cardano-testnet"
  aws_region  = "ap-southeast-1"
  providers = {
    aws = "aws.singapore"
  }
  group       = "${aws_iam_group.deployers.name}"
}

# Friendly DNS entry for S3 bucket.
# fixme: use testnet_installer_bucket.bucket_name variable
resource "aws_route53_record" "testnet_installer_bucket" {
  zone_id = "${aws_route53_zone.cardano_testnet.zone_id}"
  name    = "updates"
  type    = "CNAME"
  ttl     = "300"
  records = ["updates-cardano-testnet.s3.amazonaws.com"]
}
