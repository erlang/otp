#!/bin/sh

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

DIR=${1}
TAG=${2}
VSN=${TAG#OTP-}

mkdir ${DIR}
tar -xzf otp_src.tar.gz
mv otp otp_src_${VSN}
tar -czf ${DIR}/otp_src_${VSN}.tar.gz otp_src_${VSN}
if [ -f otp_doc_man.tar.gz ]; then
    mv otp_doc_man.tar.gz ${DIR}/otp_doc_man_${VSN}.tar.gz
fi
mv otp_doc_html.tar.gz ${DIR}/otp_doc_html_${VSN}.tar.gz
