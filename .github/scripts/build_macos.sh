#!/bin/bash
set -euox pipefail

tmp_dir=$1
# https://github.com/actions/virtual-environments/blob/main/images/macos/macos-10.15-Readme.md#utilities
ssl_dir=/usr/local/opt/openssl

./otp_build autoconf
./configure --with-ssl=$ssl_dir --disable-dynamic-ssl-lib
make release -j$(getconf _NPROCESSORS_ONLN) RELEASE_ROOT=$PWD/release
make release_docs RELEASE_ROOT=$PWD/release DOC_TARGETS=chunks
tar czf ${tmp_dir}/release.tar.gz release/
