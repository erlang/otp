#!/bin/sh

export MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)
export ERL_TOP=`pwd`
export RELEASE_ROOT=$ERL_TOP/release
export ERLC_USE_SERVER=true

./otp_build configure \
  --disable-dynamic-ssl-lib
./otp_build boot -a
./otp_build release -a $RELEASE_ROOT
make release_docs DOC_TARGETS=chunks
