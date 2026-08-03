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

release=${1}
vsn=${2}
if [[ ${release} < 27 ]]; then
    docker run -v $PWD/:/github otp "make release docs release_docs && make release_docs DOC_TARGETS='man html pdf' RELEASE_ROOT=/github/docs"
else
    case "${vsn}" in
      "27.0**")
        DOC_TARGETS=html
        ;;
      *)
        DOC_TARGETS="html man"
        ;;
    esac
    docker run -v $PWD/:/github otp "./otp_build download_ex_doc && make release docs release_docs && make release_docs DOC_TARGETS='${DOC_TARGETS}' RELEASE_ROOT=/github/docs"
fi
sudo chown -R "$(whoami)" docs
cd docs
if test -x man; then
    tar czf ../otp_doc_man.tar.gz man
    rm -rf man
fi
tar czf ../otp_doc_html.tar.gz *
