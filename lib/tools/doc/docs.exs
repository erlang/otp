# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
[
  ## The order of these items determine
  ## how they are listed in the docs
  extras: [
    "guides/cover_chapter.md",
    "guides/cprof_chapter.md",
    "guides/erlang_mode_chapter.md",
    "guides/fprof_chapter.md",
    "guides/lcnt_chapter.md",
    "guides/xref_chapter.md",
    "references/erlang.el.md"
  ],
  skip_code_autolink_to: [
    "fprof:apply_start_stop/4",
    "prim_file:drv_command/4",
    "prim_file:drv_command/2",
    "prim_file:open_int/4",
    "prim_file:open_int_setopts/3",
    "prim_file:write/2"
  ]
]
