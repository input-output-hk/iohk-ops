data "aws_iam_policy_document" "ecr_login_role_policy" {
  statement {
    actions   = ["ecr:GetAuthorizationToken"]
    resources = ["*"]
    effect    = "Allow"
  }
}

resource "aws_iam_policy" "ecr_login_policy" {
  name        = "ECRLogin"
  path        = "/"
  description = "Login to ECR"

  policy = <<EOF
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "ecr:GetAuthorizationToken"
            ],
            "Resource": "*"
        }
    ]
}
EOF
}

resource "aws_iam_role" "ecr_login_role" {
  name = "ECRLogin"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "",
      "Effect": "Allow",
      "Principal": {
        "Service": [
          "ec2.amazonaws.com"
        ]
      },
      "Action": "sts:AssumeRole"
    }
  ]
}
EOF
}

resource "aws_iam_role_policy_attachment" "ecr_login_policy_attachment" {
  role       = "${aws_iam_role.ecr_login_role.name}"
  policy_arn = "${aws_iam_policy.ecr_login_policy.arn}"
}
