variable "prefix" {
  description = "Name prefix for all of the resources"
}

variable "user_name" {
  description = "Username for the IAM user that will be created with full access to the newly created S3 bucket (will be prefixed by `prefix` variable)"
  default     = "ci-deploy-user"
}

variable "bucket_name" {
  description = "Name for the S3 bucket (will be prefixed by `prefix` variable)"
  default     = "ci-deploy"
}

variable "pgp_key" {
  description = "Name of PGP key to use for secret_key encryption, eg. keybase:username"
}
