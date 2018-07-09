# module "user_ben_ford" {
#   source  = "./modules/user"
#   name    = "ben.ford"
#   # policy  = "AmazonEC2FullAccess"
#   pgp_key  = "${pgp_key}"
#   pgp_user = "${file("${path.module}/../../../lib/gpg-keys/ben.ford.base64")}"
# }

module "user_rodney_lorrimar" {
  source  = "./modules/user"
  name    = "rodney.lorrimar"
  pgp_key  = "${var.deployer_pgp_key}"
  pgp_user = "${file("${path.module}/../../../lib/gpg-keys/rodney.lorrimar.base64")}"
}

# resource "aws_iam_user_policy_attachment" "ben_ford_ec2" {
#   user       = "ben.ford"
#   policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
# }

resource "aws_iam_user_policy_attachment" "rodney_lorrimar_admin" {
  user       = "${module.user_rodney_lorrimar.name}"
  policy_arn = "arn:aws:iam::aws:policy/AdministratorAccess"
}

resource "aws_iam_user_policy_attachment" "rodney_lorrimar_enforce_mfa" {
  user       = "${module.user_rodney_lorrimar.name}"
  policy_arn = "${aws_iam_policy.enforce_mfa.arn}"
}

module "user_deployer_staging" {
  source   = "./modules/user"
  name     = "deployer.staging"
  pgp_key  = "${var.deployer_pgp_key}"
  pgp_user = "${var.deployer_pgp_key}"
}

module "user_deployer_testnet" {
  source  = "./modules/user"
  name    = "deployer.testnet"
  pgp_key = "${var.deployer_pgp_key}"
  pgp_user = "${var.deployer_pgp_key}"
}

module "user_deployer_infra" {
  source  = "./modules/user"
  name    = "deployer.infra"
  pgp_key = "${var.deployer_pgp_key}"
  pgp_user = "${var.deployer_pgp_key}"
}

module "user_deployer_development" {
  source  = "./modules/user"
  name    = "deployer.development"
  pgp_key = "${var.deployer_pgp_key}"
  pgp_user = "${var.deployer_pgp_key}"
}

############################################################################
# group memberships

resource "aws_iam_user_group_membership" "rodney_lorrimar_developers" {
  user = "${module.user_rodney_lorrimar.name}"
  groups = [ "${aws_iam_group.developers.name}" ]
}

resource "aws_iam_user_group_membership" "deployer_staging_deployers" {
  user = "${module.user_deployer_staging.name}"
  groups = [ "${aws_iam_group.deployers.name}" ]
}

resource "aws_iam_user_group_membership" "deployer_testnet_deployers" {
  user = "${module.user_deployer_testnet.name}"
  groups = [ "${aws_iam_group.deployers.name}" ]
}
resource "aws_iam_user_group_membership" "deployer_infra_deployers" {
  user = "${module.user_deployer_infra.name}"
  groups = [ "${aws_iam_group.deployers.name}" ]
}

resource "aws_iam_user_group_membership" "deployer_development_developers" {
  user = "${module.user_deployer_development.name}"
  groups = [ "${aws_iam_group.developers.name}" ]
}

############################################################################
# user policy attachments

resource "aws_iam_user_policy_attachment" "deployer_testnet_ec2" {
  user       = "${module.user_deployer_testnet.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
}
resource "aws_iam_user_policy_attachment" "deployer_testnet_route53" {
  user       = "${module.user_deployer_testnet.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonRoute53FullAccess"
}

resource "aws_iam_user_policy_attachment" "deployer_infra_ec2" {
  user       = "${module.user_deployer_infra.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
}
resource "aws_iam_user_policy_attachment" "deployer_infra_route53" {
  user       = "${module.user_deployer_infra.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonRoute53FullAccess"
}
