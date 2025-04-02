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
  annotations_for_docs: fn
    md ->
      cond do
        md[:module] != :erlang ->
          []

        md.kind == :function && :erl_internal.guard_bif(md.name, md.arity) ->
          true = :erl_internal.bif(md.name, md.arity)
          ["auto-imported", "allowed in guard tests"]

        md.kind == :function && :erl_internal.bif(md.name, md.arity) ->
          ["auto-imported"]

        md.kind == :type && :erl_internal.is_type(md.name, md.arity) ->
          ["predefined"]

        true ->
          []
      end
  end,
  groups_for_docs: [
    {"Predefined datatypes",
     fn a ->
       a.kind == :type && a.module == :erlang && :erl_internal.is_type(a.name, a.arity)
     end},
    {"Types",
     fn a ->
       a.kind == :type
     end},
    {"Checksum",
     fn a ->
       a[:category] == :checksum
     end},
    {"Code",
     fn a ->
       a[:category] == :code
     end},
    {"Distributed Erlang",
     fn a ->
       a[:category] == :distribution
     end},
    {"Erlang Terms",
     fn a ->
       a[:category] == :terms
     end},
    {"Processes and Ports",
     fn a ->
       a[:category] == :processes || a[:category] == :ports
     end},
    {"System",
     fn a ->
       a[:category] == :system
     end},
    {"Time and timers",
     fn a ->
       a[:category] == :time || a[:category] == :timer
     end},
    {"Tracing",
     fn a ->
       a[:category] == :trace
     end},
    {"Deprecated functions",
     fn a ->
       a[:category] == :deprecated
     end},
    {"Functions without group",
     fn a ->
       if a.module == :erlang do
         IO.puts(:stderr, "Undefined group #{a[:category]} for #{a.module}:#{a.name}/#{a.arity}")
         true
       else
         false
       end
     end}
  ],
  ## The order of these items determine
  ## how they are listed in the docs
  extras:
    [
      "guides/introduction.md",
      "guides/communication.md",
      "guides/time_correction.md",
      "guides/match_spec.md",
      "guides/crash_dump.md",
      "guides/alt_dist.md",
      "guides/alt_disco.md",
      "guides/absform.md",
      "guides/tty.md",
      "guides/driver.md",
      "guides/inet_cfg.md",
      "guides/erl_ext_dist.md",
      "guides/erl_dist_protocol.md",
      "references/driver_entry.md",
      "references/epmd_cmd.md",
      "references/erl_cmd.md",
      "references/erlc_cmd.md",
      "references/erl_driver.md",
      "references/erl_nif.md",
      "references/erlsrv_cmd.md",
      "references/erts_alloc.md",
      "references/escript_cmd.md",
      "references/run_erl_cmd.md",
      "references/start_cmd.md",
      "references/start_erl_cmd.md",
      "references/werl_cmd.md"
    ] ++ Path.wildcard("../emulator/internal_doc/*.md"),
  skip_code_autolink_to: [
    "dist_util:net_ticker_spawn_options/0",
    "dist_util:handshake_we_started/1",
    "dist_util:handshake_other_started/1",
    "dist_util:start_timer/1",
    "dist_util:strict_order_flags/0",
    "erl_types:t_is_equal/2",
    "erl_types:t_has_var/1"
  ]
]
