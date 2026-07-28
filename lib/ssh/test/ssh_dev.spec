%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

%% SSH test spec for local developer iteration
{alias,dir,"../ssh_test"}.

%% ssh_basic_SUITE first: its p_basic parallel group is sensitive to
%% logger event leakage from other suites' daemon startups (shell tests
%% receive {TestRef, LogEvent} tuples instead of shell output).
%% Proper fix: 49f021164f (ssh: Fix logger event leakage between
%% parallel test cases) — adds filter_by_test_scope/2 to add_log_handler.
{suites,dir,[ssh_basic_SUITE,
             ssh_to_openssh_SUITE,
             ssh_protocol_SUITE,
             ssh_connection_SUITE,
             ssh_sup_SUITE,
             ssh_agent_SUITE,
             ssh_chan_behaviours_SUITE]}.

{skip_cases,dir,ssh_basic_SUITE,
 [ssh_file_is_host_key,
  ssh_file_is_host_key_misc,
  shell_exit_status,
  idle_time_client,
  idle_time_server,
  max_initial_idle_time,
  daemon_opt_fd,
  multi_daemon_opt_fd,
  packet_size,
  known_hosts
 ],"Dev: skip sequential/timer cases"}.

{skip_cases,dir,ssh_protocol_SUITE,
 [client_close_after_hello,
  alive_reneg_eserver_tclient,
  alive_reneg_tserver_eclient,
  alive_eserver_tclient,
  alive_tserver_eclient
 ],"Dev: skip timer-based (30s+ each)"}.

{event_handler, {cte_track, []}}.
{enable_builtin_hooks, false}.
{ct_hooks, [{cth_log_redirect, [{mode, replace}]}]}.
