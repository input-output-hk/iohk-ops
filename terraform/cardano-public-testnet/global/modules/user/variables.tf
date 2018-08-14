variable "name" {
  description = "User name"
}

variable "policy" {
  description = "Policy name"
  default = "AmazonEC2FullAccess"
}

variable "pgp_key" {
  description = "Name of PGP key to use for secret_key encryption, eg. keybase:username"
}

variable "pgp_user" {
  description = "Name of PGP key to use for console password, eg. keybase:username"
}
