#!/bin/sh

## We create a tar ball that is used later by build-otp-tar
## to create the pre-built tar ball

git archive --prefix otp/ -o otp_src.tar.gz HEAD
