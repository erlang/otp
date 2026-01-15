#!/bin/bash

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
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

## We create a tar ball that is used later by build-otp-tar
## to create the pre-built tar ball

AUTOCONF=0
TARGET=otp_src.tar.gz

set -e

if [ -n "$1" ]; then
    TARGET="$1"
fi

## This script is used to create archives for older releases
## so if configure does not exist in the git repo we need to
## create it.
if [ ! -f configure ]; then
    ./otp_build autoconf
    find . -name aclocal.m4 | xargs git add -f
    find . -name configure | xargs git add -f
    find . -name config.h.in | xargs git add -f
    find . -name config.guess | xargs git add -f
    find . -name config.sub | xargs git add -f
    find . -name install-sh | xargs git add -f
    if ! git config user.name; then
        git config user.email "you@example.com"
        git config user.name "Your Name"
    fi
    git commit --no-verify -m 'Add generated configure files'
    AUTOCONF=1
fi
git archive --prefix otp/ -o "$TARGET" HEAD

if [ "$AUTOCONF" = 1 ]; then
    git reset --hard HEAD~1
fi

## If we have a cache, we copy the timestamps from the cache to the archive
## for files that are equal
if [ -n "$2" ] && [ -f "$2" ]; then
    CACHE="$2"
    TMP_DIR=$(mktemp -d)
    TMP_TARGET_DIR="${TMP_DIR}/target"
    TMP_CACHE_DIR="${TMP_DIR}/cache"
    mkdir "${TMP_TARGET_DIR}/" "${TMP_CACHE_DIR}/"
    tar -C "${TMP_TARGET_DIR}/" -xzf "${TARGET}"
    tar -C "${TMP_CACHE_DIR}/" -xzf "${CACHE}"
    shopt -s globstar
    for target_file in "${TMP_TARGET_DIR}"/**/*; do
        cache_file=$(echo "$target_file" | sed "s:${TMP_TARGET_DIR}:${TMP_CACHE_DIR}:")
        if [ -f "${target_file}" ] && [ -f "${cache_file}" ] &&
               cmp --silent "${cache_file}" "${target_file}"; then
            cp -p "${cache_file}" "${target_file}"
        fi
    done
    shopt -u globstar
    tar -czf "${TARGET}" -C "${TMP_TARGET_DIR}" otp
    rm -rf "${TMP_DIR}"
fi
