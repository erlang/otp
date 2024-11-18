#!/bin/sh

## %CopyrightBegin%
##
## Copyright Ericsson AB 2024. All Rights Reserved.
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

docker run -v $PWD/:/github otp "make release docs release_docs && make release_docs DOC_TARGETS='man html' RELEASE_ROOT=/github/docs"
sudo chown -R `whoami` docs
cd docs
tar czf ../otp_doc_man.tar.gz man
rm -rf man
tar czf ../otp_doc_html.tar.gz *
