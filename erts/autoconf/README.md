<!--
# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2021-2025. All Rights Reserved.
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
-->

All files in this directory except for the README.md files are copies
of primary files located in the `$ERL_TOP/make/autoconf` directory.
Files in this directory are updated automatically when executing
`$ERL_TOP/otp_build update_configure [--no-commit]`.

The files in this directory are only kept here in order not to break
external scripts that might depend on them being here. You typically
want to use the files in the `$ERL_TOP/make/autoconf` directory and
*not* the ones in this directory. The files in this directory will
eventually be removed.
