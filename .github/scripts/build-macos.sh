#!/bin/sh

export ERL_TOP=`pwd`
./otp_build setup -a --disable-dynamic-ssl-lib --with-ssl=`brew --prefix openssl`
make release
make release_docs DOC_TARGETS=chunks
