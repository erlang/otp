#!/usr/bin/env bash

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2026. All Rights Reserved.
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
##

set -euo pipefail

CHANGED_FILES="${1:-/tmp/changed-vex.txt}"
YEAR=$(date +%Y)

while IFS= read -r FILE; do
    [ -z "$FILE" ] && continue
    LICENSE_FILE="${FILE}.sigstore.license"
    if [ ! -f "$LICENSE_FILE" ]; then
        printf '%%CopyrightBegin%%\n\nSPDX-License-Identifier: Apache-2.0\n\nCopyright Ericsson AB %s. All Rights Reserved.\n\n%%CopyrightEnd%%\n' "$YEAR" > "$LICENSE_FILE"
        echo "Created $LICENSE_FILE"
    else
        echo "License file already exists: $LICENSE_FILE, skipping"
    fi
done < "$CHANGED_FILES"
