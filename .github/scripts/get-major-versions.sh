#!/bin/sh

curl -sL https://raw.githubusercontent.com/erlang/otp/refs/heads/master/otp_versions.table | grep -E 'OTP-[^.]+[.]0 :'  | awk '{ print $1 }' | sed 's/[-.]/ /g' | awk '{print $2}'