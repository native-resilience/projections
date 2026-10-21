#!/bin/bash
# EC2 user data: run projections.R (annual metrics, gamlss2 smooths,
# workbooks/graphs/zips, dashboard parquet) in rocker/geospatial.
# Zips and PDFs go to s3://native-resilience/projections/_staging/ for review;
# data/annual, data/smoothed and the by-variable tables go to their final keys.
# The log streams to s3://native-resilience/projections/_logs/. Terminates on
# success; stops on failure so the disk can be inspected. Resumable: a rerun
# picks up from the ledger.
set -uo pipefail

BRANCH=main   # launch.sh substitutes the branch to run
WORKERS=$(( $(nproc) - 4 ))   # ~4 GB+ per worker; leave headroom for the main process
LOG=/work/projections.log
TS=$(date -u +%Y%m%dT%H%M%SZ)
S3_LOG=s3://native-resilience/projections/_logs/projections-$TS.log
S3_STATUS=s3://native-resilience/projections/_logs/projections-status.txt

mkdir -p /work
exec > >(tee -a /work/bootstrap.log) 2>&1

apt-get update -y
apt-get install -y docker.io unzip curl
curl -sSL "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o /tmp/awscli.zip
unzip -q /tmp/awscli.zip -d /tmp && /tmp/aws/install

echo "running since $TS on $(hostname); log $S3_LOG" | aws s3 cp - "$S3_STATUS" --quiet

# Push the log to S3 every 5 minutes while the run is going.
( while true; do sleep 300; cat /work/bootstrap.log $LOG 2>/dev/null | aws s3 cp - "$S3_LOG" --quiet; done ) &
LOGGER=$!

docker run --rm --network host \
  -e S3_AMBIENT_CREDENTIALS=1 -e AWS_REGION=us-west-2 -e AWS_DEFAULT_REGION=us-west-2 \
  -e NR_WORKERS=$WORKERS -e NR_KEEP_OUTPUTS=0 \
  -e NR_UPLOAD_PREFIX=projections/_staging \
  -v /work:/work -w /work \
  -v /usr/local/aws-cli:/usr/local/aws-cli:ro \
  rocker/geospatial:4.6.1 bash -c "
set -euo pipefail
ln -sf /usr/local/aws-cli/v2/current/bin/aws /usr/local/bin/aws
apt-get update -qq && apt-get install -y -qq ghostscript qpdf > /dev/null
Rscript -e '
options(repos = c(gamlss = \"https://gamlss-dev.r-universe.dev\", getOption(\"repos\")), Ncpus = 16)
pkgs <- c(\"arrow\", \"duckdb\", \"exactextractr\", \"mirai\", \"carrier\", \"purrr\",
          \"gamlss2\", \"gamlss\", \"gamlss.dist\", \"RcppRoll\", \"weathermetrics\", \"units\",
          \"openxlsx\", \"png\", \"ragg\", \"processx\", \"digest\", \"glue\", \"zip\")
install.packages(setdiff(pkgs, rownames(installed.packages())))
stopifnot(packageVersion(\"purrr\") >= \"1.1.0\")
for (p in pkgs) library(p, character.only = TRUE)
'
rm -rf projections
git clone --depth 1 --branch $BRANCH https://github.com/native-resilience/projections.git
cd projections
Rscript projections.R
" > $LOG 2>&1
STATUS=$?

echo "projections.R exited with status $STATUS" >> $LOG
kill $LOGGER
cat /work/bootstrap.log $LOG | aws s3 cp - "$S3_LOG" --quiet
echo "finished $(date -u +%Y%m%dT%H%M%SZ) with status $STATUS; log $S3_LOG" | aws s3 cp - "$S3_STATUS" --quiet

if [ $STATUS -eq 0 ]; then
  shutdown -h now          # terminates (shutdown behaviour = terminate)
else
  TOKEN=$(curl -sX PUT http://169.254.169.254/latest/api/token -H "X-aws-ec2-metadata-token-ttl-seconds: 60")
  ID=$(curl -s -H "X-aws-ec2-metadata-token: $TOKEN" http://169.254.169.254/latest/meta-data/instance-id)
  aws ec2 stop-instances --instance-ids "$ID" --region us-west-2 || shutdown -h now
fi
