#!/bin/bash

if [ -z "${BASE_TAG}" ]; then
    BASE_TAG=$(grep "ARG BASE=" ".github/dockerfiles/Dockerfile.${1}" | head -1 | tr '=' ' ' | awk '{print $3}')
fi

case "${BASE_TAG}" in
    *i386-debian-base)
        BASE="i386/debian:bullseye"
        BASE_TYPE=debian-base
        ;;
    *debian-base)
        BASE="debian:bullseye"
        BASE_TYPE=debian-base
        ;;
    *ubuntu-base)
        BASE="ubuntu:20.04"
        BASE_TYPE=ubuntu-base
        ;;
esac

echo "::set-output name=BASE::${BASE}"
echo "::set-output name=BASE_TAG::${BASE_TAG}"
echo "::set-output name=BASE_TYPE::${BASE_TYPE}"

if [ -f "otp_docker_base.tar" ]; then
    docker load -i "otp_docker_base.tar"
    echo "::set-output name=BASE_BUILD::loaded"
elif [ -f "otp_docker_base/otp_docker_base.tar" ]; then
    docker load -i "otp_docker_base/otp_docker_base.tar"
    echo "::set-output name=BASE_BUILD::loaded"
else
    if [ "${BASE_USE_CACHE}" != "false" ]; then
        docker pull "${BASE_TAG}"
        BASE_CACHE="--cache-from ${BASE_TAG}"
    fi

    BASE_IMAGE_ID=$(docker images -q "${BASE_TAG}")

    docker build --pull --tag "${BASE_TAG}" \
       ${BASE_CACHE} \
       --file ".github/dockerfiles/Dockerfile.${BASE_TYPE}" \
       --build-arg MAKEFLAGS=-j$(($(nproc) + 2)) \
       --build-arg USER=otptest --build-arg GROUP=uucp \
       --build-arg uid="$(id -u)" \
       --build-arg BASE="${BASE}" .github/

    NEW_BASE_IMAGE_ID=$(docker images -q "${BASE_TAG}")
    if [ "${BASE_IMAGE_ID}" = "${NEW_BASE_IMAGE_ID}" ]; then
        echo "::set-output name=BASE_BUILD::cached"
    else
        echo "::set-output name=BASE_BUILD::re-built"
        docker save "${BASE_TAG}" > "otp_docker_base.tar"
    fi
fi
