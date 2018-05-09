# Prerequisites

See the [iohk-ops terraform main readme](../README.md).

# Provisioning appveyor deployment bucket

Go ahead by making changes and output variables we need:

    terraform apply

You'll have to decrypt secret_key:

    terraform output secret_key | base64 --decode | gpg --decrypt

For each of the outputs use following form to encrypt them:

https://ci.appveyor.com/tools/encrypt

And then modify `appveyor.yml` with the output values:

    deploy:
      provider: S3
      access_key_id:
        secure: **encrypted value**
      secret_access_key:
        secure: **encrypted value**
      bucket: **value of bucket_name output**
      region: **value of bucket_region output**
      folder: 
      artifact: **name of your artifact**
      set_public: true

# Bucket index file

Pop an `index.html` file in the bucket to enable readable directory
listings for users.

    aws s3 cp --acl public-read bucket-index.html s3://appveyor-ci-deploy/index.html
