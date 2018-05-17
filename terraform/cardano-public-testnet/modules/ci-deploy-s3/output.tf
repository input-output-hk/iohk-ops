output "bucket_name" {
  value = "${aws_s3_bucket.bucket.id}"
}

output "bucket_region" {
  value = "${aws_s3_bucket.bucket.region}"
}

output "access_key" {
  value = "${aws_iam_access_key.deploy-s3-user-access-key.id}"
}

output "secret_key" {
  value = "${aws_iam_access_key.deploy-s3-user-access-key.encrypted_secret}"
}
