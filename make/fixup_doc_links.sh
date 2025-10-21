#!/bin/bash

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

set -eo pipefail

APPS=$(head -1 ${ERL_TOP}/otp_versions.table | awk -F: '{print $2}' | sed 's:#::g' | sed 's: \+: :g' | tr ' ' '\n' | awk -F- '{print $1}')

APP_VSNS=()

for app in ${APPS}; do
    dir="lib/${app}"
    VSN_VAR="$(echo $app | tr [:lower:] [:upper:])_VSN"
    if [ ${app} = "erts" ]; then
        dir="erts/"
        VSN_VAR="VSN"
    elif [ ${app} = "erl_interface" ]; then
        VSN_VAR="EI_VSN"
    fi
    VSN=$(grep "^${VSN_VAR}" "${ERL_TOP}/${dir}/vsn.mk" | grep "=" | awk -F= '{print $2}' | xargs)
    APP_VSNS=("${app}-${VSN}" ${APP_VSNS[@]})
done

FIXUP="-e s:lib/\\.\\./::g"

if [ $(basename $(dirname "$1")) = "system" ]; then
    ADJUST_PATH="../"
    FIXUP="${FIXUP} -e s:\\.\\./\\.\\./\\.\\./doc:\\.\\.:g"
fi

FIXUP="${FIXUP} -e s:${ADJUST_PATH}system/doc/html:doc/system:g"

for APP_VSN in ${APP_VSNS[@]}; do
    APP=$(echo ${APP_VSN} | awk -F- '{print $1}')
    if [ $APP = "erts" ]; then
        FIXUP="${FIXUP} -e s:${ADJUST_PATH}${APP}/doc/html/:${APP_VSN}/doc/html/:g"
    else
        FIXUP="${FIXUP} -e s:${ADJUST_PATH}lib/${APP}/doc/html/:lib/${APP_VSN}/doc/html/:g"
    fi
done

sed $FIXUP -i -- "$@"
