#!/bin/sh

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2022-2025. All Rights Reserved.
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

set -e

export MAKEFLAGS="-j$(getconf _NPROCESSORS_ONLN)"
export ERL_TOP="$(pwd)"
export ERLC_USE_SERVER=true
export RELEASE_ROOT="$ERL_TOP/release"
BUILD_DOCS=false

if [ "$1" = "build_docs" ]; then
    BUILD_DOCS=true
    shift
fi

./otp_build configure $*
./otp_build boot -a
./otp_build release -a "$RELEASE_ROOT"
if $BUILD_DOCS; then
    make release_docs DOC_TARGETS=chunks
fi
