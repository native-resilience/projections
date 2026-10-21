#!/usr/bin/env bash
# launch.sh — start a self-terminating EC2 run of the pipeline.
#
#   ec2/launch.sh extract       [branch]   # projections-extract.R
#   ec2/launch.sh projections   [branch]   # projections.R
#
# The instance clones <branch> (default: the current git branch), runs the
# script in rocker/geospatial, streams its log to
# s3://native-resilience/projections/_logs/, and terminates on success or
# stops on failure. Runs are resumable: relaunching picks up from the ledger.
# Check on a run with:
#
#   aws s3 cp s3://native-resilience/projections/_logs/<stage>-status.txt - --profile mco
#
# Defaults are the MCO landing-zone account (private subnets with egress via
# the transit gateway, the S3 gateway endpoint, and EC2-Default-SSM-AD-Role,
# which carries the inline policy in ec2/iam-policy.json). Override with the
# environment variables below. Instance types are tried in order across both
# subnets until one has capacity. NR_EC2_DRY_RUN=1 validates without launching.
set -euo pipefail

STAGE=${1:?usage: ec2/launch.sh extract|projections [branch]}
BRANCH=${2:-$(git rev-parse --abbrev-ref HEAD)}
case "$STAGE" in
  extract)     TYPES=${NR_EC2_TYPES:-"m7i.8xlarge m6i.8xlarge r7i.8xlarge"}; DISK=150 ;;
  projections) TYPES=${NR_EC2_TYPES:-"r7i.16xlarge r6i.16xlarge r7i.8xlarge r6i.8xlarge"}; DISK=300 ;;
  *) echo "unknown stage: $STAGE" >&2; exit 2 ;;
esac

export AWS_PROFILE=${AWS_PROFILE:-mco}
export AWS_REGION=${AWS_REGION:-us-west-2}
SUBNETS=${NR_EC2_SUBNETS:-"subnet-0b687da7eda8bef8f subnet-09a22817da9991c3c"}
SECURITY_GROUP=${NR_EC2_SG:-sg-0939029739ce88636}
PROFILE=${NR_EC2_PROFILE:-EC2-Default-SSM-AD-Role}
AMI=$(aws ssm get-parameter \
  --name /aws/service/canonical/ubuntu/server/24.04/stable/current/amd64/hvm/ebs-gp3/ami-id \
  --query Parameter.Value --output text)

if ! git ls-remote --exit-code --heads origin "$BRANCH" > /dev/null; then
  echo "branch '$BRANCH' is not on origin; push it first (the instance clones it)" >&2
  exit 1
fi

USER_DATA=$(mktemp)
trap 'rm -f "$USER_DATA"' EXIT
sed "s|^BRANCH=.*|BRANCH=$BRANCH|" "$(dirname "$0")/user-data-$STAGE.sh" > "$USER_DATA"

# NR_EC2_DRY_RUN=1: have AWS validate the request without launching.
DRY=()
[ -n "${NR_EC2_DRY_RUN:-}" ] && DRY=(--dry-run)

for type in $TYPES; do
  for subnet in $SUBNETS; do
    if out=$(aws ec2 run-instances ${DRY[@]+"${DRY[@]}"} \
        --image-id "$AMI" --instance-type "$type" --subnet-id "$subnet" \
        --security-group-ids "$SECURITY_GROUP" \
        --iam-instance-profile Name="$PROFILE" \
        --block-device-mappings "DeviceName=/dev/sda1,Ebs={VolumeSize=$DISK,VolumeType=gp3,Encrypted=true,DeleteOnTermination=true}" \
        --instance-initiated-shutdown-behavior terminate \
        --metadata-options HttpTokens=required,HttpPutResponseHopLimit=2 \
        --user-data "file://$USER_DATA" \
        --tag-specifications \
          "ResourceType=instance,Tags=[{Key=Name,Value=nr-projections-$STAGE},{Key=Project,Value=nr-projections}]" \
          "ResourceType=volume,Tags=[{Key=Project,Value=nr-projections}]" \
        --query 'Instances[0].[InstanceId,InstanceType,Placement.AvailabilityZone]' \
        --output text 2>&1); then
      echo "launched $out ($STAGE, branch $BRANCH)"
      exit 0
    fi
    if grep -q DryRunOperation <<< "$out"; then
      echo "dry run OK: $type in $subnet ($STAGE, branch $BRANCH)"
      exit 0
    fi
    echo "$type in $subnet: $(grep -o 'InsufficientInstanceCapacity\|[A-Za-z]*Exception\|[A-Za-z]*Error' <<< "$out" | head -1)" >&2
  done
done
echo "no capacity for any of: $TYPES" >&2
exit 1
