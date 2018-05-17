# module "user_ben_ford" {
#   source  = "./modules/user"
#   name    = "ben.ford"
#   # policy  = "AmazonEC2FullAccess"
#   pgp_key = "${file("${path.module}/keys/ben.ford.base64")}"
# }

module "user_rodney_lorrimar" {
  source  = "./modules/user"
  name    = "rodney.lorrimar"
  # policy  = "AdministratorAccess"
  pgp_key = "keybase:rvliohk"
}

# resource "aws_iam_user_policy_attachment" "ben_ford_ec2" {
#   user       = "ben.ford"
#   policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
# }

resource "aws_iam_user_policy_attachment" "rodney_lorrimar_admin" {
  user       = "${module.user_rodney_lorrimar.name}"
  policy_arn = "arn:aws:iam::aws:policy/AdministratorAccess"
}

module "user_deployer_staging" {
  source  = "./modules/user"
  name    = "deployer.staging"
  pgp_key = "keybase:rvliohk"
}

module "user_deployer_testnet" {
  source  = "./modules/user"
  name    = "deployer.testnet"
  pgp_key = "keybase:rvliohk"
}

module "user_deployer_development" {
  source  = "./modules/user"
  name    = "deployer.development"
  pgp_key = "keybase:rvliohk"
}

resource "aws_iam_user_policy_attachment" "deployer_staging_ec2" {
  user       = "${module.user_deployer_staging.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
}
resource "aws_iam_user_policy_attachment" "deployer_staging_route53" {
  user       = "${module.user_deployer_staging.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonRoute53FullAccess"
}

resource "aws_iam_user_policy_attachment" "deployer_testnet_ec2" {
  user       = "${module.user_deployer_testnet.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
}
resource "aws_iam_user_policy_attachment" "deployer_testnet_route53" {
  user       = "${module.user_deployer_testnet.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonRoute53FullAccess"
}
