#!/bin/sh

## We create a tar ball that is used later by build-otp-tar
## to create the pre-built tar ball

AUTOCONF=0
TARGET=otp_src.tar.gz

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
