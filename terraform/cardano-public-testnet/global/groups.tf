# resource "aws_iam_group" "tweag" {
#   name = "tweag"
# }

############################################################################

resource "aws_iam_group" "developers" {
  name = "developers"
}

resource "aws_iam_group_policy_attachment" "developers_ec2" {
  group      = "${aws_iam_group.developers.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
}

resource "aws_iam_group_policy_attachment" "developers_enforce_mfa" {
  group      = "${aws_iam_group.developers.name}"
  policy_arn = "${aws_iam_policy.enforce_mfa.arn}"
}

############################################################################

resource "aws_iam_group" "deployers" {
  name = "deployers"
}

resource "aws_iam_group_policy_attachment" "deployers_staging_ec2" {
  group      = "${aws_iam_group.deployers.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
}
resource "aws_iam_group_policy_attachment" "deployers_staging_route53" {
  group      = "${aws_iam_group.deployers.name}"
  policy_arn = "arn:aws:iam::aws:policy/AmazonRoute53FullAccess"
}

# Alas! NixOps does not support session credentials necessary for MFA.
# resource "aws_iam_group_policy_attachment" "deployers_enforce_mfa" {
#   group      = "${aws_iam_group.deployers.name}"
#   policy_arn = "${aws_iam_policy.enforce_mfa.arn}"
# }

############################################################################
