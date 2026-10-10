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

%% SSH test spec for GitHub Actions CI (PR gate).
%% Automatically selected via SPEC_POSTFIX=gh.
%% Full validation: ssh.spec (lab).
{alias,dir,"../ssh_test"}.

{suites,dir,all}.

{skip_suites,dir,
 [ssh_bench_SUITE,
  ssh_upgrade_SUITE,
  ssh_renegotiate_SUITE,
  ssh_collect_labmachine_info_SUITE
 ],"Slow: timer-based/bench/upgrade/lab-only"}.

{skip_cases,dir,ssh_basic_SUITE,
 [ssh_file_is_host_key,
  ssh_file_is_host_key_misc,
  shell_exit_status,
  idle_time_client,
  idle_time_server,
  max_initial_idle_time
 ],"Slow: timer/exhaustive"}.

{skip_cases,dir,ssh_sftp_SUITE,
 [read_6GB
 ],"Slow: large transfer"}.

{skip_cases,dir,ssh_property_test_SUITE,
 [client_sends_info_timing,
  client_server_parallel
 ],"Slow: PBT timing"}.

{skip_cases,dir,ssh_protocol_SUITE,
 [client_close_after_hello,
  alive_reneg_eserver_tclient,
  alive_reneg_tserver_eclient,
  alive_eserver_tclient,
  alive_tserver_eclient
 ],"Slow: timer-based"}.

{skip_cases,dir,ssh_options_SUITE,
 [max_sessions_drops_tcp_connects,
  ssh_connect_nonegtimeout_connected_sequential,
  ssh_connect_nonegtimeout_connected_parallel
 ],"Slow: timeout/storm"}.

{event_handler, {cte_track, []}}.
{enable_builtin_hooks, false}.
{ct_hooks, [{cth_log_redirect, [{mode, replace}]}]}.
