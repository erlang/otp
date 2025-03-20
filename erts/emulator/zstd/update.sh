#!/bin/bash
#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2025. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%

cd $ERL_TOP/erts/emulator/zstd

## Fetch latest version of zstd from github
VSN=$(curl -sL -H "Authorization: Bearer $(cat ~/.githubtoken)" -H "Accept: application/vnd.github+json"   -H "X-GitHub-Api-Version: 2022-11-28"   https://api.github.com/repos/facebook/zstd/releases/latest | jq ".tag_name" | sed 's/"//g')

## Clone it
git clone git@github.com:facebook/zstd -b $VSN zstd-copy

## Save sha version for book keeping
SHA=$(cd zstd-copy && git rev-parse --verify HEAD)
echo "// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Ericsson and the Erlang/OTP contributors
${SHA}" > zstd.version

## Remove old files
rm -rf common compress decompress ezstd.h LICENSE

## Copy new files
cp -r zstd-copy/lib/{common,compress,decompress} ./
cp zstd-copy/LICENSE zstd-copy/COPYING ./

## Copy API header file and rename in order to not
## conflict with system headers
API_HEADERS="zstd.h zdict.h zstd_errors.h"
for API_HEADER in ${API_HEADERS}; do
    cp "zstd-copy/lib/${API_HEADER}" "erl_${API_HEADER}"
    sed -i "s@../${API_HEADER}@../erl_${API_HEADER}@g" ./*/*.{c,S,h}
    sed -i "s@${API_HEADER}@erl_${API_HEADER}@g" ./*.h
done

rm -rf zstd-copy

git add common compress decompress ./erl_*.h COPYING LICENSE zstd.version

LICENSE=$(cat LICENSE)
COPYING=$(cat COPYING)
read -r -d '' SYSTEM_COPYRIGHT << EOM
[zstd]

* Info:
  * SPDX-License-Identifier: BSD-3-Clause OR GPL-2.0-only
  * Tool: zstd
  * Git Repository: https://github.com/facebook/zstd
  * Version: ${VSN}
  * Commit: ${SHA}
  * OTP Location: ./erts/emulator/zstd

${LICENSE}

${COPYING}

EOM
SYSTEM_COPYRIGHT=$(echo "${SYSTEM_COPYRIGHT}" | sed 's@/@\\/@g')

perl -0777 -i -pe 's/\[zstd\](.|\n)*(\n------*)/'"${SYSTEM_COPYRIGHT}"'\n$2/is' \
    "$ERL_TOP/system/COPYRIGHT"
