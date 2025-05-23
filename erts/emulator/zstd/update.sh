#!/bin/bash
#
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
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

GITHUB_TOKEN=${GITHUB_TOKEN:-$(cat ~/.githubtoken)}

if [ -z "${GITHUB_TOKEN}"]; then
    echo "You need to set ${GITHUB_TOKEN} to a valid github token"
    exit 1
fi

cd $ERL_TOP/erts/emulator/zstd

set -eo pipefail

## Remove old files
shopt -s extglob
git rm -rf $(ls -d !(update.sh|vendor.info|zstd.mk|obj))
shopt -u extglob

## Fetch latest version of zstd from github
VSN=$(curl -sL -H "Authorization: Bearer ${GITHUB_TOKEN}" -H "Accept: application/vnd.github+json"   -H "X-GitHub-Api-Version: 2022-11-28"   https://api.github.com/repos/facebook/zstd/releases/latest | jq ".tag_name" | sed 's/"//g')

## Clone it
git clone https://github.com/facebook/zstd -b $VSN zstd-copy

## Save sha version for book keeping
SHA=$(cd zstd-copy && git rev-parse --verify HEAD)

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

## Update vendor info
COMMENTS=$(cat vendor.info | grep "^//")
NEW_VENDOR_INFO=$(cat vendor.info | grep -v "^//" | jq "map(if .ID == \"erts-zstd\" then .versionInfo = \"${VSN}\" | .sha = \"${SHA}\" else . end)")

cat <<EOF > vendor.info
${COMMENTS}
${NEW_VENDOR_INFO}
EOF

## Add and commit everything
git add common compress decompress ./erl_*.h COPYING LICENSE vendor.info

git commit -m "erts: Update zstd version to ${VSN}"