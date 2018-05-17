/**
 * ## ci-deploy-s3
 *
 * Adapted from: https://raw.githubusercontent.com/fpco/fpco-terraform-aws/master/tf-modules/ci-cache-s3/main.tf
 *
 * This module will create an S3 bucket and IAM user with write access
 * to it, so it can be used by the S3 deployer on Appveyor.
 *
 */

resource "aws_iam_user" "deploy-s3-user" {
  name = "${var.prefix}${var.user_name}"
}

resource "aws_iam_access_key" "deploy-s3-user-access-key" {
  user    = "${aws_iam_user.deploy-s3-user.name}"
  pgp_key = "${var.pgp_key}"
}

resource "aws_s3_bucket" "bucket" {
  bucket = "${var.prefix}${var.bucket_name}"
  acl    = "public-read"
}

module "s3-full-access" {
  source       = "../s3-full-access-policy"
  name         = "${var.prefix}ci-deploy-s3-access"
  bucket_names = ["${aws_s3_bucket.bucket.id}"]
}

resource "aws_iam_user_policy_attachment" "s3-full-access-attachment" {
  user       = "${aws_iam_user.deploy-s3-user.name}"
  policy_arn = "${module.s3-full-access.arn}"
}

resource "aws_iam_user_policy_attachment" "s3-grant-public-read" {
  user       = "${aws_iam_user.deploy-s3-user.name}"
  policy_arn = "${element(aws_iam_policy.s3-grant-public-read.*.arn, 0)}"
}

resource "aws_iam_policy" "s3-grant-public-read" {
  name   = "${var.prefix}ci-deploy-s3-grant-public-read"
  policy = "${data.aws_iam_policy_document.s3-grant-public-read.json}"
}

data "aws_iam_policy_document" "s3-grant-public-read" {
  statement {
    effect = "Allow"

    actions = [
      "s3:PutObjectAcl",
    ]

    condition {
      test     = "StringEquals"
      variable = "s3:x-amz-acl"
      values   = ["public-read"]
    }

    resources = ["arn:aws:s3:::${aws_s3_bucket.bucket.id}/*"]
  }
}
