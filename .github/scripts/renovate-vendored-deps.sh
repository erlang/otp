#!/bin/bash

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2025. All Rights Reserved.
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

set -x

if [ $# -ne 3 ]; then
    echo "$(basename "$0") REMOTE BASE_REF HEAD_REF" >&2
    exit 1
fi

export ERL_TOP=$(pwd)

REMOTE="$1"
BASE="$2"
HEAD="$3"

# Find all vendor.info files modified in this PR
MODIFIED_FILES=$(git diff --name-only ${BASE} ${HEAD} | grep 'vendor\.info$' || true)

if [ -z "$MODIFIED_FILES" ]; then
    echo "No vendor.info files were modified in this PR"
    exit 0
fi

SUCCESS=true
FAILED_UPDATES=""

# Process each modified vendor.info file
for VENDOR_INFO in $MODIFIED_FILES; do
    echo "Processing $VENDOR_INFO"

    # Extract directory from vendor.info path
    VENDOR_DIR=$(dirname "$VENDOR_INFO")

    # Parse the vendor.info file
    sed 's@^/.*@@' "$VENDOR_INFO" | jq -c '.[]' |
    while read -r ENTRY; do
        ID=$(echo $ENTRY | jq -r '.ID')
        UPDATE_SCRIPT=$(echo $ENTRY | jq -r '.update')
        VERSION=$(echo $ENTRY | jq -r '.versionInfo')
    
        echo "Updating $ID to $VERSION using $UPDATE_SCRIPT"
    
        # Check if update script exists
        if [ -f "$UPDATE_SCRIPT" ]; then
            if bash "$UPDATE_SCRIPT"; then
                echo "✅ Successfully updated $ID to $VERSION"
                # Add changes to git
                git add .
            else
                echo "❌ Failed to update $ID to $VERSION"
                SUCCESS=false
                FAILED_UPDATES="$FAILED_UPDATES\n- $ID ($VERSION): $UPDATE_SCRIPT failed"
            fi
        else
            echo "❌ Update script not found: $UPDATE_SCRIPT"
            SUCCESS=false
            FAILED_UPDATES="$FAILED_UPDATES\n- $ID ($VERSION): Update script not found: $UPDATE_SCRIPT"
        fi
    done
done

if $SUCCESS && ! git diff --quiet "${HEAD}"; then
    if git diff --quiet; then
        echo "No changes to commit"
    else
        git commit -m "Update vendored dependencies per vendor.info"
    fi
    if [ "${REMOTE}" = "skip" ]; then
        echo "✅ Pushing $(git rev-parse HEAD) to ${REMOTE}/$(git branch --show-current)"
        git push "${REMOTE}" $(git branch --show-current)
    fi
fi

if ! $SUCCESS; then
    exit 1
fi