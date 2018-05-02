resource "aws_iam_role" "ec2_assume_role" {
  name               = "EC2AssumeRole"
  assume_role_policy = "${file("${path.module}/templates/assume_role_policy.json")}"
}

data "template_file" "s3_bucket_policy" {
  template = "${file("${path.module}/templates/s3_bucket_policy.json")}"

  vars {
    bucket = "iele-nix-cache-*"
  }
}

resource "aws_iam_policy" "s3_bucket_policy" {
  name        = "NixCacheS3Bucket"
  description = "Allow access to the nix cache from EC2 instances"
  policy      = "${data.template_file.s3_bucket_policy.rendered}"
}

resource "aws_iam_policy_attachment" "s3_bucket_policy" {
  name       = "EC2NixCacheS3Bucket"
  roles      = ["${aws_iam_role.ec2_assume_role.name}"]
  policy_arn = "${aws_iam_policy.s3_bucket_policy.arn}"
}

resource "aws_iam_instance_profile" "nix_cache" {
  name = "nix_cache"
  role = "${aws_iam_role.ec2_assume_role.name}"
}
