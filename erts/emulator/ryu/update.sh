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

ORIGIN_REPO="https://github.com/erlang/ryu"
UPSTREAM_REPO="https://github.com/ulfjack/ryu"

cd $ERL_TOP/erts/emulator/ryu

set -exo pipefail

## Clone reference
git clone ${ORIGIN_REPO} ryu-copy

## into ryu-copy
cd ryu-copy
git remote add upstream ${UPSTREAM_REPO}
git fetch upstream

SHA=$(git rev-parse --verify upstream/master)
SHORT_SHA=$(git rev-parse --verify --short upstream/master)

if ! git merge upstream/master; then
    git status --short | grep "^D" | awk '{print $2}' | xargs git rm -rf
    git status --short | grep "^A" | awk '{print $2}' | xargs git rm -rf
    if ! git commit; then
        cat <<EOF
#######################################################################

Merge of ${SHORT_SHA} to $(git rev-parse --short master) failed, resolve and commit the conflict in
$(pwd) and push to ${ORIGIN_REPO}.
EOF
        exit 1
    fi
fi

cd .. ## back to ryu

## Fetch latest version of STL from github
STL_VSN=$(curl -sL -H "Authorization: Bearer ${GITHUB_TOKEN}" -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28"   https://api.github.com/repos/microsoft/STL/releases/latest | jq ".tag_name" | sed 's/"//g')

git clone --filter=blob:none --no-checkout -b ${STL_VSN} https://github.com/microsoft/STL.git
cd STL ## into STL
    git sparse-checkout init --cone
    git sparse-checkout set stl/inc/xcharconv_ryu.h
    git checkout HEAD -- stl/inc/xcharconv_ryu.h
    STL_CHARCONV_SHA=$(git log --format="%H" HEAD -1  -- stl/inc/xcharconv_ryu.h)
    STL_SHA=$(git rev-parse HEAD)
cd .. ## back to ryu

if [ "$(cat xcharconv_ryu.h.sha)" != "${STL_CHARCONV_SHA}" ]; then

    cat <<EOF
#######################################################################

https://github.com/microsoft/STL/blob/master/stl/inc/xcharconv_ryu.h has been
updated. Check that no changes affect the __to_chars function and if they do
incorporate them into to_chars.h.

Once done, update xcharconv_ryu.h.sha with the new sha. i.e.

echo "${STL_CHARCONV_SHA}" > ${ERL_TOP}/erts/emulator/ryu/xcharconv_ryu.h.sha

EOF

    exit 1

fi

## Remove old files
shopt -s extglob
git rm -rf $(ls -d !(update.sh|vendor.info|ryu.mk|obj|README.ryu_update.md|ryu-copy|STL|xcharconv_ryu.h.sha*))
shopt -u extglob

cp -r ryu-copy/* .
rm -rf ryu-copy STL

set -x

## Update vendor info
COMMENTS=$(cat vendor.info | grep "^//")
NEW_VENDOR_INFO=$(cat vendor.info | grep -v "^//" | jq "map(if .ID == \"erts-ryu\" then .versionInfo = \"${SHA}\" | .sha = \"${SHA}\" else . end)")
NEW_VENDOR_INFO=$(echo "${NEW_VENDOR_INFO}" | jq "map(if .ID == \"ryu-to_chars\" then .versionInfo = \"${STL_VSN}\" | .sha = \"${STL_SHA}\" else . end)")

cat <<EOF > vendor.info
${COMMENTS}
${NEW_VENDOR_INFO}
EOF

git add .

git commit -m "erts: Update ryu version to ${SHA}"