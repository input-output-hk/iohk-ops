#!/usr/bin/env nix-shell
#!nix-shell -i bash -p qemu jq ec2_api_tools awscli

# To start with do: nix-shell -p awscli --run aws configure

source ~/.ami-creds/export

set -o pipefail

stateDir=`pwd`/ec2-image
echo "keeping state in $stateDir"
mkdir -p $stateDir

version=$(nix-instantiate --eval --strict '<nixpkgs>' -A lib.nixpkgsVersion | sed s/'"'//g)
major=${version:0:5}
echo "NixOS version is $version ($major)"

type="hvm"
store="ebs"
region="eu-central-1"
newregions="us-west-1 ap-southeast-1 ap-southeast-2 sa-east-1"

link=`pwd`/image
imageFile=$link/nixos.qcow2
system=x86_64-linux
arch=x86_64

bucket=cardano-amis
bucketDir="$version-$type-$store"

name=nixos-cardano-$version-$(echo $link | cut -d '\' -f 5)-$arch-$type-$store
description="NixOS $system $version ($type-$store)"

amiFile=$stateDir/$region.$type.$store.ami-id

if ! [ -e $amiFile ]; then

    echo "Creating $amiFile..."

    # Convert the image to vhd format so we don't have
    # to upload a huge raw image.
    vhdFile=$stateDir/$type.vhd
    if ! [ -e $vhdFile ]; then
        qemu-img convert -f qcow2 -O vpc $imageFile $vhdFile.tmp
        mv $vhdFile.tmp $vhdFile
    fi

    vhdFileLogicalBytes="$(qemu-img info "$vhdFile" | grep ^virtual\ size: | cut -f 2 -d \(  | cut -f 1 -d \ )"
    vhdFileLogicalGigaBytes=$(((vhdFileLogicalBytes-1)/1024/1024/1024+1)) # Round to the next GB

    echo "Disk size is $vhdFileLogicalBytes bytes. Will be registered as $vhdFileLogicalGigaBytes GB."

    taskId=$(cat $stateDir/$region.$type.task-id 2> /dev/null || true)
    volId=$(cat $stateDir/$region.$type.vol-id 2> /dev/null || true)
    snapId=$(cat $stateDir/$region.$type.snap-id 2> /dev/null || true)

    # Import the VHD file.
    if [ -z "$snapId" -a -z "$volId" -a -z "$taskId" ]; then
        echo "importing $vhdFile..."
        taskId=$(ec2-import-volume $vhdFile --no-upload -f vhd \
            -O "$AWS_ACCESS_KEY_ID" -W "$AWS_SECRET_ACCESS_KEY" \
            -o "$AWS_ACCESS_KEY_ID" -w "$AWS_SECRET_ACCESS_KEY" \
            --region "$region" -z "${region}a" \
            --bucket "$bucket" --prefix "$bucketDir/" \
            | tee /dev/stderr \
            | sed 's/.*\(import-vol-[0-9a-z]\+\).*/\1/ ; t ; d')
        echo -n "$taskId" > $stateDir/$region.$type.task-id
    fi

    if [ -z "$snapId" -a -z "$volId" ]; then
        ec2-resume-import  $vhdFile -t "$taskId" --region "$region" \
            -O "$AWS_ACCESS_KEY_ID" -W "$AWS_SECRET_ACCESS_KEY" \
            -o "$AWS_ACCESS_KEY_ID" -w "$AWS_SECRET_ACCESS_KEY"
    fi

    # Wait for the volume creation to finish.
    if [ -z "$snapId" -a -z "$volId" ]; then
        echo "waiting for import to finish..."
        while true; do
            volId=$(aws ec2 describe-conversion-tasks --conversion-task-ids "$taskId" --region "$region" | jq -r .ConversionTasks[0].ImportVolume.Volume.Id)
            if [ "$volId" != null ]; then break; fi
            sleep 10
        done

        echo -n "$volId" > $stateDir/$region.$type.vol-id
    fi

    # Delete the import task.
    if [ -n "$volId" -a -n "$taskId" ]; then
        echo "removing import task..."
        ec2-delete-disk-image -t "$taskId" --region "$region" \
            -O "$AWS_ACCESS_KEY_ID" -W "$AWS_SECRET_ACCESS_KEY" \
            -o "$AWS_ACCESS_KEY_ID" -w "$AWS_SECRET_ACCESS_KEY" || true
        rm -f $stateDir/$region.$type.task-id
    fi

    # Create a snapshot.
    if [ -z "$snapId" ]; then
        echo "creating snapshot..."
        snapId=$(aws ec2 create-snapshot --volume-id "$volId" --region "$region" --description "$description" | jq -r .SnapshotId)
        if [ "$snapId" = null ]; then exit 1; fi
        echo -n "$snapId" > $stateDir/$region.$type.snap-id
    fi

    # Wait for the snapshot to finish.
    echo "waiting for snapshot to finish..."
    while true; do
        status=$(aws ec2 describe-snapshots --snapshot-ids "$snapId" --region "$region" | jq -r .Snapshots[0].State)
        if [ "$status" = completed ]; then break; fi
        sleep 10
    done

    # Delete the volume.
    if [ -n "$volId" ]; then
        echo "deleting volume..."
        aws ec2 delete-volume --volume-id "$volId" --region "$region" || true
        rm -f $stateDir/$region.$type.vol-id
    fi

    extraFlags=""
    extraFlags+=" --root-device-name /dev/sda1"
    extraFlags+=" --sriov-net-support simple"
    extraFlags+=" --ena-support"
    extraFlags+=" --virtualization-type hvm"

    blockDeviceMappings="DeviceName=/dev/sda1,Ebs={SnapshotId=$snapId,VolumeSize=$vhdFileLogicalGigaBytes,DeleteOnTermination=true,VolumeType=gp2}"
    blockDeviceMappings+=" DeviceName=/dev/sdb,VirtualName=ephemeral0"
    blockDeviceMappings+=" DeviceName=/dev/sdc,VirtualName=ephemeral1"
    blockDeviceMappings+=" DeviceName=/dev/sdd,VirtualName=ephemeral2"
    blockDeviceMappings+=" DeviceName=/dev/sde,VirtualName=ephemeral3"

    ami=$(aws ec2 register-image \
        --name "$name" \
        --description "$description" \
        --region "$region" \
        --architecture "$arch" \
        --block-device-mappings $blockDeviceMappings \
        $extraFlags | jq -r .ImageId)
    if [ "$ami" = null ]; then break; fi
else                    
    ami=$(cat $amiFile)
fi

echo $ami > $stateDir/$region.ami


amisFile = ../modules/amis.nix
echo "{" > $amisFile
echo "  $region = \"$ami\";" >> $amisFile

for newregion in $newregions; do
    echo	
    echo "Copying $ami to $newregion"
    echo	
    newami=$(aws ec2 copy-image --region "$newregion" \
	    --source-region "$region" --source-image-id $ami \
	    --name "$name" --description "$description" | jq --raw-output '.ImageId')
    echo $newami > $stateDir/$newregion.ami
    echo "  $newregion = \"$newami\";" >> $amisFile
done

echo "}" >> $amisFile

echo -n "waiting for AMI to be available in EC2..."

for newregion in $newregions; do
  while true; do
      ami=$(cat "$stateDir/$newregion.ami")
      status=$(aws ec2 describe-images --image-ids "$ami" --region "$newregion" | jq -r .Images[0].State)
      if [ "$status" = available ]; then break; fi
      sleep 10
      echo -n '.'
  done
  echo
done

