#! /bin/sh

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2019-2025. All Rights Reserved.
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

mkdir -p $ERL_TOP/tmp
BAT_FILE=$ERL_TOP/tmp/w$$.bat
if [ -z "$1" -o -z "$2" ]; then
    echo "Usage:" "$0" '<key> <valuename>'
    exit 1
fi
BACKED=`echo "$1" | sed 's,/,\\\\,g'`

if [ $CONFIG_SUBTYPE = "win64" ]; then
    REG_OPT=" /reg:64"
elif [ X"$CONFIG_SUBTYPE" = X"arm64" -o X"$CONFIG_SUBTYPE" = X"x64_arm64" ]; then
    REG_OPT=" /reg:64"
else
    REG_OPT=" /reg:32"
fi

WIN_BAT_FILE=`w32_path.sh -w $BAT_FILE`
RESULT=`reg.exe query "$BACKED" /v "$2" $REG_OPT | sed 's@\\\@/@g' | tr -d '\r\n'`
echo "$RESULT" | sed "s,.*REG_[^ ]* *,,g"
