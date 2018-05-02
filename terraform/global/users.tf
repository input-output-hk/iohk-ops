resource "aws_iam_user" "ben_ford" {
  name = "ben.ford"
}

resource "aws_iam_user_login_profile" "ben_ford" {
  user    = "${aws_iam_user.ben_ford.name}"
  pgp_key = "${file("${path.module}/keys/ben.ford.base64")}"
}

resource "aws_iam_access_key" "ben_ford" {
  user    = "${aws_iam_user.ben_ford.name}"
  pgp_key = "${file("${path.module}/keys/ben.ford.base64")}"
}

resource "aws_iam_user_policy_attachment" "ben_ford_admin" {
  user       = "${aws_iam_user.ben_ford.name}"
  policy_arn = "arn:aws:iam::aws:policy/AdministratorAccess"
}

resource "local_file" "ben_ford_password" {
  filename = "${path.module}/private/ben.ford.password"
  content  = "${aws_iam_user_login_profile.ben_ford.encrypted_password}"
}

resource "local_file" "ben_ford_secret" {
  filename = "${path.module}/private/ben.ford.secret"
  content  = "${aws_iam_access_key.ben_ford.encrypted_secret}"
}
