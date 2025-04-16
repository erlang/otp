#!/bin/bash

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

cat <<EOF
c-files:
  - 'erts/**'
  - 'make/**'
  - 'lib/Makefile'
  - 'Makefile.in'
  - 'otp_build'
  - 'configure'
  - '.github/**'
  - 'xcomp/**'
EOF

APPS=$(ls -d lib/*/doc | awk -F '/' '{print $2}')
for app in $APPS; do
    if find lib/$app/ -name '*.c' -o -name '*.h' \
            -o -name '*.cpp' -o -name '*.hpp' \
            -o -name '*.in' -o -name '*.ac' \
            -o -name 'configure' \
            | grep -v "lib/$app/test" \
            | grep -v "lib/$app/examples"\
            | grep . > /dev/null; then
        echo "  - 'lib/$app/**'"
    fi
done
