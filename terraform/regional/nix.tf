resource "aws_s3_bucket" "nix_cache" {
  provider = "aws.${var.aws_region}"
  bucket   = "iele-nix-cache-${var.aws_region}"
  acl      = "private"
  region   = "${var.aws_region}"
}
