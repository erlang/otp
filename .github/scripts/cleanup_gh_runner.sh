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

set -euo pipefail {0}

## Delete large files from runner to get more disk space
## See https://github.com/actions/runner-images/issues/2840
sudo rm -rf /usr/share/dotnet
sudo rm -rf /opt/ghc
sudo rm -rf "/usr/local/share/boost"
sudo rm -rf "$AGENT_TOOLSDIRECTORY"
docker system prune -a -f
df -h
