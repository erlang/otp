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

sudo mkdir -p -m0755 /var/run/sshd

sudo /usr/sbin/sshd

sudo service postgresql start

sudo -E bash -c "apt-get update && apt-get install -y linux-tools-common linux-tools-generic"
sudo -E bash -c "apt-get install -y linux-tools-$(uname -r)" || true

sudo bash -c "Xvfb :99 -ac -screen 0 1920x1080x24 -nolisten tcp" &
export DISPLAY=:99

PATH="$PATH:$(ls -1d /usr/local/lib/erlang-*/bin | tr '\n' ':')"
