#!/bin/bash
set -euox pipefail

tmp_dir=$1

./otp_build autoconf
./configure --with-ssl --disable-dynamic-ssl-lib
make release -j$(getconf _NPROCESSORS_ONLN) RELEASE_ROOT=$PWD/release
make release_docs RELEASE_ROOT=$PWD/release DOC_TARGETS=chunks
tar czf ${tmp_dir}/release.tar.gz release/
