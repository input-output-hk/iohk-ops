output "name" {
  value = "${aws_iam_user.self.name}"
}

# output "aws_credentials" {
#   value = <<EOF
# [${aws_iam_user.self.name}]
# aws_access_key_id = ${aws_iam_access_key.self.id}
# aws_secret_access_key = ${aws_iam_access_key.self.encrypted_secret}
# EOF
# }
