#!/bin/bash

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2026. All Rights Reserved.
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

set -e

## Remove systemd-coredump
! sudo apt remove systemd-coredump
## Removing systemd-coredump, caused apport to be installed instead, so we disable it
! sudo service apport stop

sudo bash -c "echo 'core.%p' > /proc/sys/kernel/core_pattern"

## Needed for some I/O tests to work, see interactive_shell_SUITE.erl
sudo sysctl -w dev.tty.legacy_tiocsti=1
