#!/usr/bin/env bash
set -e

usage() {
        echo "$(basename "$0") [--verbose] COMMAND ARGS.." >&2
        echo
        grep ' ) # Doc:' "$0" | sed 's,^\([^ ]*\) .* \([^ ]*\) ) # Doc:\(.*\)$,"\1" "\2",' | tail -n +2 | {
                read -r a b
                while test -n "$a"
                do
                        printf '%20s  %s\n' "$(echo "$a" | xargs echo)" "$(echo "$b" | sed 's,-, ,g; s/\b\(.\)/\u\1/' | xargs)"
                        read -r a b
                done
        }
}
log() {
        echo "==(  $*" >&2
}
logn() {
        echo -n "==(  $*" >&2
}
fail() {
        log "$@"
        exit 1
}
region_tagged_instances() {
        local region="$1"
        local tags="$2"
        aws=$(nix-build --no-out-link -E '(import ./. {}).pkgs.awscli' | xargs echo)/bin/aws
        ${aws} ec2 describe-instances --filters "Name=${tags}" --region  "${region}" --output json
}
region_tagged_instances_property() {
        local region="$1"
        local tags="$2"
        local prop="$3"
        jq=$( nix-build --no-out-link -E '(import ./. {}).pkgs.jq'     | xargs echo)/bin/jq
        region_tagged_instances "${region}" "${tags}" | "${jq}" ".Reservations${prop}"
}
count_region_tagged_instances() {
        local region="$1"
        local tags="$2"
        region_tagged_instances_property "${region}" "${tags}" "| length"
}
has_region_tagged_instances() {
        test "$(count_region_tagged_instances "$*")" != 0
}
wait_region_tagged_instances() {
        local region="$1"
        local tags="$2"
        logn "Waiting for $region/$tags to appear: "
        while ! has_region_tagged_instances "$region" "$tags"
        do echo -n "."
        done
        echo " ok."
}
wait_host_ssh() {
        local host="$1"

        logn "Waiting for ssh pong from ${host}: "
        while ! ssh -tt -o ConnectionAttempts=20 "${host}" -o ConnectTimeout=10 -o KbdInteractiveAuthentication=no -o PasswordAuthentication=no -o ChallengeResponseAuthentication=no -o StrictHostKeyChecking=no -o HashKnownHosts=no -o CheckHostIP=no "echo pong." 2>/dev/null
        do true; done
}

ops_url="http://github.com/input-output-hk/iohk-ops"
ops_branch="master"
full=""
verbose=""
while test -n "$1"
do case "$1" in
           --full )    full=t;;
           --ops-url ) ops_url="$2"; shift;;
           --ops-branch ) ops_branch="$2"; shift;;
           --verbose | -v ) set -x; verbose="--verbose";;
           --help | "--"* ) usage; exit 1;;
           * ) break;;
   esac; shift; done

cmd=${1:-create}; test -n "$1"; shift
set -u

# Shellcheck doesn't like `"" | "" )` but I believe Serge uses
# this construct for self-documenting so ignoring.
# shellcheck disable=SC2221,SC2222
case ${cmd} in
"" | "" ) # Doc:
        ;;
"" | deployer-bootstrap ) # Doc:
        ;;
"" | "" ) # Doc:
        ;;
describe-image | describe-deployer-nixops-image ) # Doc:
        nixos_ver="18.09"
        region="eu-central-1"
        export AWS_PROFILE=default AWS_REGION=${region}
        nixpkgs_out=$(nix-instantiate  --eval -E '(import ./. {}).nixpkgs'     | xargs echo)
        aws=$(        nix-build --no-out-link -E '(import ./. {}).pkgs.awscli' | xargs echo)/bin/aws
        ami="$(nix-instantiate --eval -E "(import ${nixpkgs_out}/nixos/modules/virtualisation/ec2-amis.nix).\"${nixos_ver}\".${region}.hvm-ebs" | xargs echo)"
        ${aws} ec2 describe-images --image-id "${ami}" --region "${region}"
        ;;
create | create-and-maybe-deploy-new-deployer ) # Doc:
        nixos_ver="18.09"
        region="eu-central-1"
        az=${region}b
        org=IOHK
        ec2type=t3.xlarge
        depl_ssh_sg="allow-public-ssh-${region}-${org}"
        set +u
        CLUSTER_KIND="$1"
        test -n "${CLUSTER_KIND}" || fail "First argument must be a non-empty cluster kind (e.g. 'mantis')."
        CLUSTER_TYPE="${2:-${CLUSTER_KIND}-infra}"
        while test ! -d "clusters/${CLUSTER_TYPE}"
        do read -rei "${CLUSTER_TYPE}" -p "Unknown cluster type '${CLUSTER_TYPE}', please choose another name: " CLUSTER_TYPE
        done
        CLUSTER_NAME="${3:-${CLUSTER_KIND}-infra}"
        test -n "${CLUSTER_NAME}" || fail "Third argument must be a non-empty cluster name."
        while nixops info -d "${CLUSTER_NAME}" >/dev/null 2>/dev/null
        do read -rei "${CLUSTER_NAME}" -p "Cluster '${CLUSTER_NAME}' already exists, please choose another name: " CLUSTER_NAME
        done
        set -u

        deployer_tags="tag:deployment,Values=${CLUSTER_NAME},Name=tag:role,Values=deployer,Name=instance-state-name,Values=running"
        
        export AWS_PROFILE=${CLUSTER_KIND} AWS_REGION=${region}
        nixpkgs_out=$(nix-instantiate  --eval -E '(import ./. {}).nixpkgs'     | xargs echo)
        aws=$(nix-build --no-out-link -E '(import ./. {}).pkgs.awscli' | xargs echo)/bin/aws

        if ! ${aws} ec2  describe-security-groups        --group-names ${depl_ssh_sg}
        then ${aws} ec2    create-security-group         --group-name  ${depl_ssh_sg} --description "${depl_ssh_sg}"
             ${aws} ec2 authorize-security-group-ingress --group-name  ${depl_ssh_sg} --protocol tcp --port 22 --cidr 0.0.0.0/0
        fi

        if has_region_tagged_instances ${region} "${deployer_tags}"
        then log "deployer already exists."
        else log "creating the deployer.."
             ami="$(nix-instantiate --eval -E "(import ${nixpkgs_out}/nixos/modules/virtualisation/ec2-amis.nix).\"${nixos_ver}\".${region}.hvm-ebs" | xargs echo)"
             ${aws} ec2 run-instances                                                   \
                    --image-id "${ami}"                                                 \
                    --security-groups ${depl_ssh_sg}                                    \
                    --instance-type ${ec2type}                                          \
                    --placement AvailabilityZone=${az}                                  \
                    --block-device-mappings "DeviceName=/dev/sda1,Ebs={VolumeSize=200}" \
                    --tag-specifications "ResourceType=instance,Tags=[{Key=deployment,Value=${CLUSTER_KIND}},{Key=role,Value=deployer}]" \
                    --user-data "file://$(dirname "$0" | xargs realpath)/modules/deployer-bootstrap.nix"
        fi

        wait_region_tagged_instances ${region} "${deployer_tags}"
        deployer_ip=$(region_tagged_instances_property ${region} "${deployer_tags}" '[0].Instances[0].PublicIpAddress' | xargs echo)
        log "new deployer IP: ${deployer_ip}"

        wait_host_ssh "deployer@${deployer_ip}"

        cat <<EOF
Access the new deployer as:
-------------------- 8< ----------------------
Host ${CLUSTER_KIND}-deployer
  User     deployer
  Hostname ${deployer_ip}
EOF
        if test -n "${full}"
        then shift
             log "full setup requested, proceeding to set up deployer."
             $0 --ops-url "${ops_url}" --ops-branch "${ops_branch}" setup "${CLUSTER_KIND}" "${CLUSTER_TYPE}" "${CLUSTER_NAME}"
        fi;;
setup | finalise-deployer-nixops-bootstrap ) # Doc:
        CLUSTER_KIND="$1"
        CLUSTER_TYPE="$2"
        CLUSTER_NAME="$3"
        test -d "./clusters/${CLUSTER_KIND}" || {
                log "ERROR: unknown cluster ${CLUSTER_KIND} -- to see available clusters:  ls clusters"
                exit 1
        }
        cluster_config="./configs/${CLUSTER_TYPE}.nix"
        while test ! -f "${cluster_config}"
        do read -rei "./configs/default.nix" -p "Enter path to configuration file (one of ./config/*.nix):  " cluster_config
           test -f "${cluster_config}" || log "ERROR: ${cluster_config} is not a readable file"
        done

        region="eu-central-1"
        export AWS_PROFILE=${CLUSTER_KIND} AWS_REGION=${region}
        deployer_tags="tag:deployment,Values=${CLUSTER_KIND},Name=tag:role,Values=deployer,Name=instance-state-name,Values=running"

        log "setting up AWS.."
        deployer_ip=$(region_tagged_instances_property ${region} "${deployer_tags}" '[0].Instances[0].PublicIpAddress' | xargs echo)

        log "setting up AWS credentials.."
        AKID="$(sed -n "/\\[${CLUSTER_KIND}\\]/,/^\\[.*/ p" < ~/.aws/credentials | grep aws_access_key_id     | cut -d= -f2 | xargs echo)"
        # Shellcheck wants us to use `<<'EOF'` to make sure that the heredoc gets evaluated server-side; however,
        # (in this case) we actually know what we're doing so no 🙃
        # shellcheck disable=SC2087
        ssh "deployer@${deployer_ip}" sh -c "\"mkdir -p ~/.aws && cat > ~/.aws/credentials && chmod -R go-rwx ~/.aws\"" <<EOF
[default]
aws_access_key_id = ${AKID}
aws_secret_access_key = $(sed -n "/\\[${CLUSTER_KIND}\\]/,/^\\[.*/ p" < ~/.aws/credentials | grep aws_secret_access_key | cut -d= -f2 | xargs echo)
region = ${region}
EOF
        log "generating SSH key and setting up ops checkout.."
        setup_cmd="\
ssh-keygen  -t ed25519 -b 2048 -f ~/.ssh/id_ed25519 -N ''; \
ssh-keyscan -t rsa     github.com >> ~/.ssh/known_hosts && \
ssh-keyscan -t ed25519 localhost  >> ~/.ssh/known_hosts && \
cat ~/.ssh/id_ed25519.pub && \
echo 'Please authorise this key and press Enter: ' && \
read foo && \
if test ! -d infra; then git clone ${ops_url} infra; fi && \
cd infra && \
echo CLUSTER_KIND=${CLUSTER_KIND} > .config.sh && \
echo CLUSTER_TYPE=${CLUSTER_KIND}-infra >> .config.sh && \
echo CLUSTER_NAME=infra >> .config.sh && \
echo CONFIG=${CLUSTER_KIND}-infra >> .config.sh && \
git checkout ${ops_branch} && \
git config --replace-all receive.denyCurrentBranch updateInstead"
        ssh "deployer@${deployer_ip}" sh -c "\"${setup_cmd}\""

        echo "Deploying infra cluster.." >&2
        ssh -A "deployer@${deployer_ip}" sh -c "\"cd infra && ./enter.sh gac ${verbose} create-deployment-from-cluster-components ${AKID} \"";;
* )
        fail "unknown command: ${cmd}";;
esac
