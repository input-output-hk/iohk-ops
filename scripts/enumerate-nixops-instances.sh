#!/bin/sh

set -e

ACCESS_KEY_ID=${1:-'iohk'}
REGIONS="$(aws ec2 describe-regions --query 'Regions[].{Name:RegionName}' --output text)"

for region in ${REGIONS}
do
        echo "-- region: ${region}"
        aws ec2 describe-instances \
            --region ${region} \
            --output text \
            --query 'Reservations[].Instances[].[Tags[?Key==`CharonMachineName`].Value,Tags[?Key==`CharonNetworkUUID`].Value[]]' \
            --filters "Name=tag:CharonNetworkUUID,Values=*"
done
