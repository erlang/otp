#!/bin/bash -x

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

EX_DOC_FORMATS="-f html -f markdown"

if [ ! -f "${EX_DOC}" ]; then
   EX_DOC=$(command -v ex_doc)
fi

if [ ! -f "${EX_DOC}" ]; then
   echo "${EX_DOC_FORMATS}"
   exit 0
fi

EX_DOC_VERSION=$(${EX_DOC} --version)

EX_DOC_VERSION_REGEX="ExDoc v([0-9]+)\.([0-9]+)\.([0-9]+)"
if [[ ${EX_DOC_VERSION} =~ ${EX_DOC_VERSION_REGEX} ]]; then
    EX_DOC_MAJOR_VERSION=${BASH_REMATCH[1]}
    EX_DOC_MINOR_VERSION=${BASH_REMATCH[2]}
    EX_DOC_PATCH_VERSION=${BASH_REMATCH[3]}

    if [[ "${EX_DOC_MAJOR_VERSION}" -eq 0 && "${EX_DOC_MINOR_VERSION}" -lt 40 ]]; then
        EX_DOC_FORMATS="-f html"
    fi
fi

echo "${EX_DOC_FORMATS}"
