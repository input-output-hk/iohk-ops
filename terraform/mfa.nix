{ stdenv, writeScriptBin, awscli, jq, gnused }:

writeScriptBin "aws-mfa" ''
  #!${stdenv.shell}
  set -euo pipefail
  export PATH=${awscli}/bin:${jq}/bin:${gnused}/bin

  unset AWS_SECRET_ACCESS_KEY
  unset AWS_SESSION_TOKEN
  unset AWS_ACCESS_KEY_ID

  token="''${1:-}"
  username=$(aws sts get-caller-identity | jq -re .Arn | sed 's=.*user/==')
  serial_number=$(aws iam list-mfa-devices --user-name $username | jq -re '.MFADevices[0].SerialNumber')

  if [ -z "$token" ]; then
    echo -n "Enter MFA Token and press [ENTER]: " >&2
    read token
  fi

  creds=$(aws sts get-session-token --serial-number "$serial_number" --token-code "$token" | jq -re .Credentials)

  jq -re '@text "\(.AccessKeyId) \(.SecretAccessKey) \(.SessionToken) default"' > ~/.ec2-keys <<< "$creds"
  jq -re '@text "export AWS_ACCESS_KEY_ID=\"\(.AccessKeyId)\"\nexport AWS_SECRET_ACCESS_KEY=\"\(.SecretAccessKey)\"\nexport AWS_SESSION_TOKEN=\"\(.SessionToken)\""' <<< "$creds"
''
