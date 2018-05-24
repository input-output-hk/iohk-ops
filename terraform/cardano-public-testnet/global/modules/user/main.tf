data "aws_caller_identity" "current" {}

resource "aws_iam_user" "self" {
  name = "${var.name}"
}

resource "aws_iam_user_login_profile" "self" {
  user    = "${aws_iam_user.self.name}"
  pgp_key = "${var.pgp_key}"
}

resource "aws_iam_access_key" "self" {
  user    = "${aws_iam_user.self.name}"
  pgp_key = "${var.pgp_key}"
}

resource "aws_iam_user_policy_attachment" "self_mfa" {
  user       = "${aws_iam_user.self.name}"
  policy_arn = "arn:aws:iam::${data.aws_caller_identity.current.account_id}:policy/SelfManageMFADevice"
}

resource "aws_iam_user_policy_attachment" "self_create_access_key" {
  user       = "${aws_iam_user.self.name}"
  policy_arn = "arn:aws:iam::${data.aws_caller_identity.current.account_id}:policy/CreateAWSAccessKey"
}

resource "local_file" "self_password" {
  filename = "${path.module}/private/${var.name}.password"
  content  = "${aws_iam_user_login_profile.self.encrypted_password}"
}

resource "local_file" "self_secret" {
  filename = "${path.module}/private/${var.name}.secret"
  content  = "${aws_iam_access_key.self.encrypted_secret}"
}
