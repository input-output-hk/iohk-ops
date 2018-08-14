variable "aws_region" {
  description = "Region for bucket"
}

variable "bucket_name" {
  description = "Name for the S3 bucket. Full DNS name is best to allow for HTTPS access"
}

variable "prefix" {
  description = "Text to make unique resource names"
}

variable "group" {
  description = "The group that can write to the bucket."
}
