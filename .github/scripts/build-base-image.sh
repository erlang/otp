#!/bin/bash

## %CopyrightBegin%
##
## Copyright Ericsson AB 2024-2025. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## %CopyrightEnd%

set -eo pipefail

BASE_BRANCH="$1"
LATEST_ERLANG_VERSION="unknown"

case "${BASE_BRANCH}" in
	OTP-*)
        ## Calculate the base branch if this is a tag push
        BASE_BRANCH="maint-$(echo $BASE_BRANCH | sed 's:OTP-\([^.]\+\).*:\1:g')"
        ;;
    *)
        ;;
esac

case "${BASE_BRANCH}" in
    maint-*)
        LATEST_ERLANG_VERSION=${BASE_BRANCH#"maint-"}
        ;;
    master|maint)
        ;;
    *)
        BASE_BRANCH="master"
        ;;
esac

if [ -z "${BASE_TAG}" ]; then
    BASE_TAG=$(grep "ARG BASE=" ".github/dockerfiles/Dockerfile.${2}" | head -1 | tr '=' ' ' | awk '{print $3}')
    ## If this script is used on pre 25 releases
    if [ -z "${BASE_TAG}" ]; then
        BASE_TAG=$(grep "FROM " ".github/dockerfiles/Dockerfile.${2}" | head -1 | awk '{print $2}')
    fi
fi

case "${BASE_TAG}" in
    *i386-debian-base)
        BASE="i386/debian:bookworm"
        BASE_TYPE=debian-base
        ;;
    *debian-base)
        BASE="debian:bookworm"
        BASE_TYPE=debian-base
        ;;
    *ubuntu-base)
        BASE="ubuntu:22.04"
        BASE_TYPE=ubuntu-base
        ;;
esac

echo "BASE=${BASE}" >> $GITHUB_OUTPUT
echo "BASE_TAG=${BASE_TAG}" >> $GITHUB_OUTPUT
echo "BASE_TYPE=${BASE_TYPE}" >> $GITHUB_OUTPUT

if [ -f "otp_docker_base.tar" ]; then
    docker load -i "otp_docker_base.tar"
    echo "BASE_BUILD=loaded" >> $GITHUB_OUTPUT
elif [ -f "otp_docker_base/otp_docker_base.tar" ]; then
    docker load -i "otp_docker_base/otp_docker_base.tar"
    echo "BASE_BUILD=loaded" >> $GITHUB_OUTPUT
else
    if [ "${BASE_USE_CACHE}" != "false" ]; then
        if docker pull "${BASE_TAG}:${BASE_BRANCH}"; then
            docker tag "${BASE_TAG}:${BASE_BRANCH}" "${BASE_TAG}:latest"
        fi
        BASE_CACHE="--cache-from type=registry,ref=${BASE_TAG}:${BASE_BRANCH}"
    fi

    BASE_IMAGE_ID=$(docker images -q "${BASE_TAG}:latest")

    DOCKER_BUILDKIT=1 docker build --pull --tag "${BASE_TAG}:latest" \
       ${BASE_CACHE} \
       --file ".github/dockerfiles/Dockerfile.${BASE_TYPE}" \
       --build-arg MAKEFLAGS=-j6 \
       --build-arg USER=otptest --build-arg GROUP=uucp \
       --build-arg uid="$(id -u)" \
       --build-arg LATEST_ERLANG_VERSION="${LATEST_ERLANG_VERSION}" \
       --build-arg BASE="${BASE}" \
       --build-arg BUILDKIT_INLINE_CACHE=1 \
       .github/

    NEW_BASE_IMAGE_ID=$(docker images -q "${BASE_TAG}:latest")
    if [ "${BASE_IMAGE_ID}" = "${NEW_BASE_IMAGE_ID}" ]; then
        echo "BASE_BUILD=cached" >> $GITHUB_OUTPUT
    else
        echo "BASE_BUILD=re-built" >> $GITHUB_OUTPUT
        docker save "${BASE_TAG}:latest" > "otp_docker_base.tar"
    fi
fi
