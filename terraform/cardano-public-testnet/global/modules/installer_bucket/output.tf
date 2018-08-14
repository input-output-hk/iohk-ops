output "bucket_name" {
  value = "${aws_s3_bucket.bucket.id}"
}

output "bucket_region" {
  value = "${aws_s3_bucket.bucket.region}"
}
