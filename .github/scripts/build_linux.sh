#!/bin/bash
set -euox pipefail

os=$1
os_version=$2
tmp_dir=$3

image=otp:${os}
container=otp_${os}
dockerfile=.github/dockerfiles/Dockerfile.${os}-binary

.github/scripts/init-pre-release.sh

docker build \
  -t ${image} \
  -f $dockerfile \
  --build-arg OS_VERSION=${os_version} \
  --build-arg ARCHIVE=otp_src.tar.gz \
  .

docker run --name ${container} ${image}
mkdir -p ${tmp_dir}
docker cp ${container}:/tmp/release.tar.gz ${tmp_dir}/
docker rm -f ${container}
docker rmi -f ${image}
rm -rf otp_src.tar.gz
