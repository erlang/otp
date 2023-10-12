#!/bin/sh

export MAKEFLAGS="-j$(getconf _NPROCESSORS_ONLN)"
export ERL_TOP="$(pwd)"
export ERLC_USE_SERVER=true
export RELEASE_ROOT="$ERL_TOP/release"
BUILD_DOCS=false

if [ "$1" = "build_docs" ]; then
    BUILD_DOCS=true
    shift
fi

./otp_build configure $*
./otp_build boot -a
./otp_build release -a "$RELEASE_ROOT"
if $BUILD_DOCS; then
    make release_docs DOC_TARGETS=chunks
fi
