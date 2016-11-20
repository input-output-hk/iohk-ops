#!/usr/bin/env nix-shell
#!nix-shell -i bash -p qemu awscli ec2_ami_tools jq
# To start with do: nix-shell -p awscli --run aws configure

if [[ ! $AWS_ACCOUNT ]]; then
   echo "Please set AWS_ACCOUNT"
   exit 1
fi

if [[ ! $EC2_CERT ]]; then
   echo "Please set EC2_CERT"
   exit 1
fi

if [[ ! $EC2_PRIVATE_KEY ]]; then
   echo "Please set EC2_PRIVATE_KEY"
   exit 1
fi

if [[ ! $AWS_ACCESS_KEY_ID ]]; then
   echo "Please set AWS_ACCESS_KEY_ID"
   exit 1
fi

if [[ ! $AWS_SECRET_ACCESS_KEY ]]; then
   echo "Please set AWS_SECRET_ACCESS_KEY"
   exit 1
fi

set -o pipefail

stateDir=`pwd`/ec2-image
echo "keeping state in $stateDir"
mkdir -p $stateDir

version=$(nix-instantiate --eval --strict '<nixpkgs>' -A lib.nixpkgsVersion | sed s/'"'//g)
major=${version:0:5}
echo "NixOS version is $version ($major)"

type="hvm"
store="s3"
region="eu-central-1"

link=$1
imageFile=$link/nixos.qcow2
system=x86_64-linux
arch=x86_64

bucket=nixos-amis
bucketDir="$version-$type-$store"

name=nixos-$version-$arch-$type-$store
description="NixOS $system $version ($type-$store)"

amiFile=$stateDir/$region.$type.$store.ami-id

if ! [ -e $amiFile ]; then

    echo "Creating $amiFile..."

    # Bundle the image.
    imageDir=$stateDir/$type-bundled

    # Convert the image to raw format.
    rawFile=$stateDir/$type.raw
    if ! [ -e $rawFile ]; then
        qemu-img convert -f qcow2 -O raw $imageFile $rawFile.tmp
        mv $rawFile.tmp $rawFile
    fi

    if ! [ -d $imageDir ]; then
        rm -rf $imageDir.tmp
        mkdir -p $imageDir.tmp
        ec2-bundle-image \
            -d $imageDir.tmp \
            -i $rawFile --arch $arch \
            -u "$AWS_ACCOUNT" -c "$EC2_CERT" -k "$EC2_PRIVATE_KEY"
        mv $imageDir.tmp $imageDir
    fi

    if ! [ -e $imageDir/$type.raw.manifest.xml ]; then
      echo "ec2-bundle-failed" 
      exit 1
    fi

    # Upload the bundle to S3.
    if ! [ -e $imageDir/uploaded ]; then
        echo "uploading bundle to S3..."
        ec2-upload-bundle \
            -m $imageDir/$type.raw.manifest.xml \
            -b "$bucket/$bucketDir" \
            -a "$AWS_ACCESS_KEY_ID" -s "$AWS_SECRET_ACCESS_KEY" \
            --location EU
        touch $imageDir/uploaded
    fi

    echo -n "$ami" > $amiFile
    echo "created AMI $ami of type '$type' in $region."
else
    ami=$(cat $amiFile)
fi

echo "region = $region, type = $type, store = $store, ami = $ami"

echo -n "waiting for AMI to be available in EC2..."
while true; do
    status=$(aws ec2 describe-images --image-ids "$ami" --region "$region" | jq -r .Images[0].State)
    if [ "$status" = available ]; then break; fi
    sleep 10
    echo -n '.'
done
echo

# Make the image public.
aws ec2 modify-image-attribute \
    --image-id "$ami" --region "$region" --launch-permission 'Add={Group=all}'

echo "AMI: $ami"
