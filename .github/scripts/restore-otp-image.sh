#!/bin/bash

BASE_BRANCH="$1"
BASE_TAG=$(grep "ARG BASE=" ".github/dockerfiles/Dockerfile.64-bit" | head -1 | tr '=' ' ' | awk '{print $3}')

BASE_TAG="$BASE_TAG" .github/scripts/build-base-image.sh "${BASE_BRANCH}"

cat > Dockerfile <<EOF
FROM ${BASE_TAG}
ADD otp-ubuntu-20.04.tar.gz /
WORKDIR /buildroot/otp/
EOF
docker build -t otp .
