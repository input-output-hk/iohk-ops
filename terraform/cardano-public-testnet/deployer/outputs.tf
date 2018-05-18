output "deployer_ip" {
  value = "${aws_eip.deployer_eip.public_ip}"
}

output "deployer_ssh" {
  value = "${var.username}@${aws_eip.deployer_eip.public_ip}"
}
