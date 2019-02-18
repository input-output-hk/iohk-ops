#!/bin/sh
set -e

usage() {
        echo "$(basename $0) [--verbose] COMMAND ARGS.." >&2
        exit 1
}

cd `dirname $0`

verbose=""
while test -n "$1"
do case "$1" in
           --verbose | -v ) set -x; verbose="--verbose";;
           --help | "--"* ) usage;;
           * ) break;;
   esac; shift; done
self="$0 ${verbose}"
cmd=${1:-doit}; test -n "$1"; shift
set -u

CONFIG=
CLUSTER=create-.config.sh
if test -f .config.sh
then . ./.config.sh
else if test ${cmd} != "repl" -a ${cmd} != "eval"
     then echo "ERROR:  echo CLUSTER=your-cluster-name > .config.sh" >&2; exit 1; fi
fi
ALL_NODES="mantis-a-0 mantis-a-1 mantis-b-0 mantis-b-1 mantis-c-0 "

nixpkgs_out=$(nix-instantiate --eval -E '(import ./lib.nix).goguenNixpkgs' | xargs echo)
nixops_out="$(nix-instantiate --eval -E '(import ((import ./lib.nix).goguenNixpkgs) {}).nixops.outPath' | xargs echo)"
nixops=${nixops_out}/bin/nixops
nix_opts="\
--max-jobs 4 --cores 0 --show-trace \
-I nixpkgs=${nixpkgs_out} \
-I nixops=${nixops_out}/share/nix/nixops \
-I config=./configs \
-I module=./modules \
-I static=./static \
"

ag=$( nix-build -E '(import ((import ./lib.nix).goguenNixpkgs) {}).ag'     | xargs echo)/bin/ag
aws=$(nix-build -E '(import ((import ./lib.nix).goguenNixpkgs) {}).awscli' | xargs echo)/bin/aws
jq=$( nix-build -E '(import ((import ./lib.nix).goguenNixpkgs) {}).jq'     | xargs echo)/bin/jq

if test ! -f ${nixops}
then nix-store --realise ${nixops}
fi

export NIX_PATH="nixpkgs=${nixpkgs_out}"

nixops_subopts="--deployment ${CLUSTER} ${nix_opts}"
nixops_subopts_deploy="${nixops_subopts} --max-concurrent-copy=50"
nixops_bincaches="https://cache.nixos.org https://hydra.iohk.io https://mantis-hydra.aws.iohkdev.io"

nixops_constituents="$(ls ./clusters/${CLUSTER}/*.nix)"
nixops_constituents_quoted="$(echo ${nixops_constituents} | sed 's,\\b,\",g')"
nixops_deplArgs="{ accessKeyId = \"AKID\"; deployerIP = \"127.0.0.1\"; }"
nixops_network_expr="import <nixops/eval-machine-info.nix> { \
    networkExprs = [ ${nixops_constituents_quoted} ]; \
    args = ${nixops_deplArgs}; \
    uuid = \"81976a61-80f6-11e7-9369-06258a1e40fd\"; \
    deploymentName = \"${CLUSTER}\"; \
    checkConfigurationOptions = false; \
  }"

generate_keys () {
  export JAVA_HOME=`nix-build -E  "(import (import ./lib.nix).goguenNixpkgs {}).pkgs.openjdk8"  --no-out-link`
  MANTIS=`nix-build -E  "(import ./goguen/. {}).mantis"`
  NODES="a-0 a-1 b-0 b-1 c-0"
  NODE_IDS="static/mantis-node-ids.nix"

  echo "{" > "$NODE_IDS"

  for n in $NODES; do 
    KEY_FILE="static/mantis-$n.key"
    
    if [ ! -f "$KEY_FILE" ]; then
      $MANTIS/bin/eckeygen > "$KEY_FILE"
    fi
    
    NODE_ID="`sed -n 2p \"$KEY_FILE\"`"
    echo "  mantis-$n = { id = \"$NODE_ID\"; };" >> "$NODE_IDS"
  done

  echo "}" >> "$NODE_IDS"

}

region_tagged_instances() {
        local region="$1"
        local tags="$2"
        ${aws} ec2 describe-instances --filters "Name=${tags}" --region  ${region} --output json
}
region_tagged_instances_property() {
        local region="$1"
        local tags="$2"
        local prop="$3"
        region_tagged_instances ${region} ${tags} | ${jq} ".Reservations${prop}"
}
count_region_tagged_instances() {
        local region="$1"
        local tags="$2"
        region_tagged_instances_property ${region} ${tags} "| length"
}
has_region_tagged_instances() {
        test "$(count_region_tagged_instances $*)" != 0
}
wait_region_tagged_instances() {
        local region="$1"
        local tags="$2"
        echo -n "Waiting for $region/$tags to appear: "
        while ! has_region_tagged_instances "$region" "$tags"
        do echo -n "."
        done
        echo " ok."
}
wait_host_ssh() {
        local host="$1"

        echo -n "Waiting for ssh on ${host}: "
        while ! ssh -tt -o ConnectionAttempts=20 ${host} -o ConnectTimeout=10 -o KbdInteractiveAuthentication=no -o PasswordAuthentication=no -o ChallengeResponseAuthentication=no -o StrictHostKeyChecking=no -o HashKnownHosts=no -o CheckHostIP=no "echo pong." 2>/dev/null
        do true; done
}

case ${cmd} in
###
###
###
        eval )
                nix-instantiate ${nix_opts} --eval -E  "let depl = ${nixops_network_expr}; in depl.machines { names = [\"mantis-a-0\"]; }";;
        repl )
                nix repl        ${nix_opts} --arg depl "${nixops_network_expr}" \
                                                            ./network.nix \
                                         --argstr nixpkgsSrc ${nixpkgs_out};;
###
###
###
genkey | g )
        generate_keys;;
create | c )
        ${nixops} create   ${nixops_subopts} ${nixops_constituents}
        deployerIP="$(curl --connect-timeout 2 --silent http://169.254.169.254/latest/meta-data/public-ipv4)"
        guessedAKID="$(grep aws_access_key_id ~/.aws/credentials | cut -d= -f2 | xargs echo)"
        read -ei "${guessedAKID}" -p "Use AWS access key ID: " AKID
        ${nixops} set-args ${nixops_subopts} --argstr accessKeyId "${AKID}" --argstr deployerIP "${deployerIP}"
        generate_keys
        ${nixops} deploy   ${nixops_subopts_deploy} "$@"
        ;;
delete | destroy | terminate | abolish | eliminate | demolish )
        ${nixops} destroy  ${nixops_subopts} --confirm
        ${nixops} delete   ${nixops_subopts};;
re )
        $self delete && $self create && $self deploy;;
info   | i )
        ${nixops} info     ${nixops_subopts};;
###
###
###
staged-deploy )
        ${nixops} modify   ${nixops_subopts} ${nixops_constituents}
        ${nixops} deploy   ${nixops_subopts_deploy} "$@" --copy-only
        ${nixops} deploy   ${nixops_subopts_deploy} "$@";;
deploy | d )
        ${nixops} modify   ${nixops_subopts} ${nixops_constituents}
        ${nixops} deploy   ${nixops_subopts_deploy} "$@";;
deploy-one | one )
        $self     deploy --include mantis-a-0;;
cluster-config | csconf )
        echo "Querying own IP.." >&2
        deployerIP="$(curl --connect-timeout 2 --silent http://169.254.169.254/latest/meta-data/public-ipv4)"
        echo "Setting up the AWS access key.." >&2
        AKID="$1"
        test -z "$1" && {
                guessedAKID="$(grep aws_access_key_id ~/.aws/credentials | cut -d= -f2 | xargs echo)"
                read -ei "${guessedAKID}" -p "Use AWS access key ID: " AKID
        }
        ${nixops} set-args ${nixops_subopts} \
                  --argstr accessKeyId "${AKID}" \
                  --argstr deployerIP "${deployerIP}" \
                  --arg    configFile "${CONFIG}" \
                ;;
cluster-create | csc )
        set +u; AKID="$1"; test -n "$1" && shift; set -u
        $self     cluster-components
        ${nixops} create   ${nixops_subopts} clusters/${CLUSTER}/*.nix
        $self     cluster-config $AKID
        ${nixops} deploy   ${nixops_subopts_deploy} "$@";;
cluster-components | ls )
        echo "Components of cluster '${CLUSTER}':"
        echo ${nixops_constituents} | tr " " "\n" | sort | sed 's,^,   ,';;
cluster-deploy | csd )
        $self     cluster-components
        ${nixops} modify   ${nixops_subopts} clusters/${CLUSTER}/*.nix
        ${nixops} set-args ${nixops_subopts} \
                  --arg    configFile "${CONFIG}"
        ${nixops} deploy   ${nixops_subopts_deploy} "$@";;
cluster-destroy )
        ${nixops} destroy  ${nixops_subopts} "$@"
        ${nixops} delete   ${nixops_subopts} "$@";;
###
###
###
ssh )
        machine="${1:-mantis-a-0}";
        set +u; test -n "$1" && shift; set -u
        ${nixops} ssh          ${nixops_subopts} ${machine} "$@";;
ssh-all )
        ${nixops} ssh-for-each ${nixops_subopts} --parallel --include ${ALL_NODES} -- "$@";;   
###
###
###
stop )
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl    stop mantis;;
start )
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl   start mantis; echo "### new start date:  $($0 since mantis-a-0)";;
restart )
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl restart mantis; echo "### new start date:  $($0 since mantis-a-0)";;
statuses | ss )
        on=${1:+--include $*}
        $0        ssh-all ${on} -- systemctl  status mantis;;
###
###
###
since )
        node=$1
        ${nixops} ssh          ${nixops_subopts} ${node} -- systemctl show mantis --value --property WatchdogTimestamp | cut -d' ' -f3;;
journal-on | jon )
        node=$1
        ${nixops} ssh          ${nixops_subopts} ${node} -- journalctl  -u mantis --since $($0 since ${node});;
journal    | jall | j )
        start_sample_node='mantis-a-0'
        since=${1:-$($0 since ${start_sample_node})}
        echo "### journal since:  ${since}" >&2
        $0        ssh-all      -- journalctl -u mantis --since ${since};;
system-journal | sysj )
        node=${1:-'mantis-a-0'}
        $0        ssh ${node}  -- journalctl -xe;;
follow | f )
        node=${1:-'mantis-a-0'}
        $0        ssh ${node}  -- journalctl -fu mantis;;
follow-all | fa )
        $0        ssh-all      -- journalctl -fu mantis "$@";;
grep-since )
        since=$1; shift
        echo "### journal since:  ${since}" >&2
        echo "### filter:         ag $*" >&2
        $0        journal "${since}" 2>&1 | ${ag} "$@";;
grep )
        start_sample_node='mantis-a-0'
        since=$($0 since ${start_sample_node})
        $0        grep-since ${since} "$@";;
watch-for )
        $0        follow-all 2>&1 | ${ag} "$@";;
             
###
###
###
blocks )
        $0        grep-since "'30 seconds ago'" 'Best Block: ' | cut -d' ' -f5,14-;;
roles )
        $0        grep 'role changed|LEADER';;
exceptions | ex )
        $0        grep -i exception | ag -v 'RiemannBatchClient|exception.counter';;
watch-blocks )
        $0 watch-for 'Best Block: ' | cut -d' ' -f5,14-;;
watch-exceptions )
        $0 watch-for -i exception | ag -v 'RiemannBatchClient|exception.counter';;
###
###
###
describe-image )
        nixos_ver="18.09"
        region="eu-central-1"
        export AWS_PROFILE=${CLUSTER} AWS_REGION=${region}
        ami="$(nix-instantiate --eval -E "(import ${nixpkgs_out}/nixos/modules/virtualisation/ec2-amis.nix).\"${nixos_ver}\".${region}.hvm-ebs" | xargs echo)"
        ${aws} ec2 describe-images --image-id ${ami} --region ${region}
        ;;
create-deployer )
        nixos_ver="18.09"
        region="eu-central-1"
        az=${region}b
        org=IOHK
        ec2type=t2.large
        depl_ssh_sg="allow-public-ssh-${region}-${org}"
        deployer_tags="tag:deployment,Values=${CLUSTER},Name=tag:role,Values=deployer,Name=instance-state-name,Values=running"
        export AWS_PROFILE=${CLUSTER} AWS_REGION=${region}

        if ! ${aws} ec2  describe-security-groups        --group-names ${depl_ssh_sg}
        then ${aws} ec2    create-security-group         --group-name  ${depl_ssh_sg} --description "${depl_ssh_sg}"
             ${aws} ec2 authorize-security-group-ingress --group-name  ${depl_ssh_sg} --protocol tcp --port 22 --cidr 0.0.0.0/0
        fi

        if has_region_tagged_instances ${region} ${deployer_tags}
        then echo "ERROR:  deployer already exist." >&2; exit 1
        fi
        echo "Creating a deployer..";
        ami="$(nix-instantiate --eval -E "(import ${nixpkgs_out}/nixos/modules/virtualisation/ec2-amis.nix).\"${nixos_ver}\".${region}.hvm-ebs" | xargs echo)"
        ${aws}      ec2 run-instances                                                \
                    --image-id ${ami}                                                \
                    --security-groups ${depl_ssh_sg}                                 \
                    --instance-type ${ec2type}                                       \
                    --placement AvailabilityZone=${az}                               \
                    --block-device-mappings DeviceName=/dev/sda1,Ebs={VolumeSize=200} \
                    --tag-specifications "ResourceType=instance,Tags=[{Key=deployment,Value=${CLUSTER}},{Key=role,Value=deployer}]" \
                    --user-data file://terraform/deployer/configuration-bootstrap.nix

        wait_region_tagged_instances ${region} ${deployer_tags}
        deployer_ip=$(region_tagged_instances_property ${region} ${deployer_tags} '[0].Instances[0].PublicIpAddress' | xargs echo)
        echo "New deployer IP: ${deployer_ip}"

        wait_host_ssh "deployer@${deployer_ip}"

        cat <<EOF
Access the new deployer as:
-------------------- 8< ----------------------
Host ${CLUSTER}-deployer
  User     deployer
  Hostname ${deployer_ip}
EOF
        if test "$1" = "--full"
        then shift
             echo "Full setup requested, proceeding to set up deployer."
             $0 setup-deployer "$@"
        fi;;
setup-deployer )
        test -d ./clusters/${CLUSTER} || {
                echo "ERROR: unknown cluster ${CLUSTER} -- to see available clusters:  ls clusters"
                exit 1
        }
        cluster_config="${CONFIG}"
        while test ! -f "${cluster_config}"
        do read -ei "./configs/default.nix" -p "Enter path to configuration file (one of ./config/*.nix):  " cluster_config
           test -f "${cluster_config}" || echo "ERROR: ${cluster_config} is not a readable file" >&2
        done

        region="eu-central-1"
        export AWS_PROFILE=${CLUSTER} AWS_REGION=${region}
        deployer_tags="tag:deployment,Values=${CLUSTER},Name=tag:role,Values=deployer,Name=instance-state-name,Values=running"

        echo "Setting up AWS.." >&2
        deployer_ip=$(region_tagged_instances_property ${region} ${deployer_tags} '[0].Instances[0].PublicIpAddress' | xargs echo)

        set +u
        ops_url="$1"
        ops_branch="$2"
        test -z "$1" && read -ei "http://github.com/input-output-hk/iohk-ops"       -p "Ops git repository URL: "    ops_url
        test -z "$2" && read -ei "$(git symbolic-ref HEAD | sed 's|refs/heads/||')" -p "Ops git repository branch: " ops_branch
        set -u

        echo "Setting up AWS credentials.." >&2
        AKID="$(sed -n "/\\[${CLUSTER}\\]/,/^\\[.*/ p" < ~/.aws/credentials | grep aws_access_key_id     | cut -d= -f2 | xargs echo)"
        ssh "deployer@${deployer_ip}" sh -c "\"mkdir -p ~/.aws && cat > ~/.aws/credentials && chmod -R go-rwx ~/.aws\"" <<EOF
[default]
aws_access_key_id = ${AKID}
aws_secret_access_key = $(sed -n "/\\[${CLUSTER}\\]/,/^\\[.*/ p" < ~/.aws/credentials | grep aws_secret_access_key | cut -d= -f2 | xargs echo)
region = ${region}
EOF
        echo "Generating SSH key and setting up ops checkout.." >&2
        setup_cmd="\
ssh-keygen  -t ed25519 -b 2048 -f ~/.ssh/id_ed25519 -N ''; \
ssh-keyscan -t rsa     github.com >> ~/.ssh/known_hosts && \
ssh-keyscan -t ed25519 localhost  >> ~/.ssh/known_hosts && \
cat ~/.ssh/id_ed25519.pub && \
echo 'Please authorise this key and press Enter: ' && \
read foo && \
git clone ${ops_url} infra && \
cd infra && \
echo CLUSTER=${CLUSTER} > .config.sh && \
echo CONFIG=${cluster_config} >> .config.sh && \
git checkout ${ops_branch} && \
git config --replace-all receive.denyCurrentBranch updateInstead"
        ssh "deployer@${deployer_ip}" sh -c "\"${setup_cmd}\""

        echo "Deploying infra cluster.." >&2
        ssh -A "deployer@${deployer_ip}" sh -c "\"cd infra && ./gac.sh ${verbose} cluster-create ${AKID} \""
        ;;
* ) usage;;
esac
