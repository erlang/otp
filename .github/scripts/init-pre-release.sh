#!/bin/sh

## We create a tar ball that has generated configure
## (old versions might not have these included)
## This is used later by build-otp-tar to create
## the pre-built tar ball

if [ -f ./configure ]; then
    git archive --prefix otp/ -o otp_src.tar.gz HEAD
else
    ERL_TOP=`pwd`
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
    git archive --prefix otp/ -o otp_src.tar.gz HEAD
    git reset --hard HEAD~1
fi
