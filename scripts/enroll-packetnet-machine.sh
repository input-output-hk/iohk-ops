#!/bin/sh

machine_name=$1

test -n "$machine_name" || {
        echo "Usage: $(basename "$0") MACHINE-NAME" >&2
        exit 1
}
test ! -d deployments/"${machine_name}" || {
        echo "FATAL: deployments/${machine_name} already exists" >&2
        exit 1
}

scp -r -o StrictHostKeyChecking=accept-new root@"${machine_name}".aws.iohkdev.io:/etc/nixos/packet/ deployments/"${machine_name}"
