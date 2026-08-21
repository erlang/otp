# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2024-2026. All Rights Reserved.
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
  groups_for_modules: [
    "BASIC DATA TYPES": [
      :binary,
      :lists,
      :maps,
      :records
    ],
    "HIGH-LEVEL DATA STRUCTURES": [
      :gb_sets,
      :gb_trees,
      :array,
      :sofs,
      :dict,
      :digraph,
      :digraph_utils,
      :graph,
      :orddict,
      :ordsets,
      :proplists,
      :queue,
      :sets
    ],
    STRINGS: [
      :string,
      :unicode,
      :re,
      :base64,
      :uri_string
    ],
    PROCESSES: [
      :gen_event,
      :gen_fsm,
      :gen_server,
      :gen_statem,
      :log_mf_h,
      :pool,
      :proc_lib,
      :supervisor,
      :supervisor_bridge,
      :sys
    ],
    IO: [
      :io,
      :io_ansi,
      :io_lib,
      :filelib,
      :filename,
      :file_sorter
    ],
    "TERM STORAGE": [
      :ets,
      :dets,
      :qlc
    ],
    CODE: [
      :erl_scan,
      :erl_pp,
      :erl_parse,
      :erl_lint,
      :erl_internal,
      :erl_id_trans,
      :ms_transform,
      :erl_features,
      :erl_expand_records,
      :erl_eval,
      :erl_anno,
      :epp,
      :beam_lib
    ],
    COMPRESSION: [:zip, :erl_tar, :zstd],
    SHELL: [:shell, :shell_default, :c, :shell_docs, :edlin, :edlin_expand]
  ],
  ## The order of these items determine
  ## how they are listed in the docs
  extras: [
    "guides/introduction.md",
    "guides/io_protocol.md",
    "guides/custom_shell.md",
    "guides/terminal_interface.md",
    "guides/unicode_usage.md",
    "guides/uri_string_usage.md",
    "guides/re_incompat.md",
    "references/assert_hrl.md",
    "stdlib_app.md"
  ]
]
