provider "aws" {
  region = "ap-northeast-1"
}

terraform {
  backend "s3" {
    bucket  = "iohk-ops-tfstate"
    key     = "remote-tfstate/appveyor-deploy-s3/terraform.tfstate"
    region  = "us-west-1"
    encrypt = true
  }
}

module "appveyor-deploy-s3" {
  source      = "../modules/ci-deploy-s3"
  prefix      = "appveyor-"              # <-- make sure to set this to a custom value.
  pgp_key     = "keybase:rvliohk"        # <-- or a base64 encoded PGP public key
}

output "bucket_name" {
  value = "${module.appveyor-deploy-s3.bucket_name}"
}

output "bucket_region" {
  value = "${module.appveyor-deploy-s3.bucket_region}"
}

output "access_key" {
  value = "${module.appveyor-deploy-s3.access_key}"
}

output "secret_key" {
  value = "${module.appveyor-deploy-s3.secret_key}"
}
