# temp docs

Run with a command like this:

terraform apply -var 'key_name={your_aws_key_name}' \
   -var 'public_key_path={location_of_your_key_in_your_local_machine}'
For example:

terraform apply -var 'key_name=terraform' -var 'public_key_path=/Users/jsmith/.ssh/terraform.pub'



nix-shell -p '(terraform.withPlugins (ps: [ ps.aws ]))' 

terraform plan -var 'key_name=tftest' -var 'public_key_path=tftest.pub'


## questions

 - what region?
 - get iojp to delete our ssh key and add their own?
 - ubuntu lts 16.04?
 - what ports open for ssh, etc?

