# Prerequisites

See the [iohk-ops terraform main readme](../README.md).

# Provisioning appveyor bucket

Go ahead by making changes and output variables we need:

    terraform apply

You'll have to decrypt secret_key:

    terraform output secret_key | base64 --decode | gpg --decrypt

For each of the outputs use following form to encrypt them:

https://ci.appveyor.com/tools/encrypt

And then modify appveyor.yml with the output values:

environment:
  global:
    AWS_ACCESS_KEY_ID:
      secure: BSNfEghh/xxx=
    AWS_SECRET_ACCESS_KEY
      secure: BSNfEghh/xxx=
    S3_BUCKET: XXX
