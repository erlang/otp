%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%%

%% There are some environment variables that can be used to "manipulate"
%% the test suite: 
%%
%% Variable that controls "verbosity" of the test case(s):
%%
%%         ESOCK_TEST_QUIET: true (default) | false
%%
%% Defines the runtime of the ttest cases
%% (This is the time during which "measurement" is performed. 
%%  the actual time it takes for the test case to complete
%%  will be longer; setup, completion, ...)
%%
%%          ESOCK_TEST_TTEST_RUNTIME: 1 second
%%              Format of values: <integer>[<unit>]
%%              Where unit is: ms | s | m
%%                 ms - milli seconds
%%                 s  - seconds (default)
%%                 m  - minutes
%%
%% The ttest takes a long time to run, even when the runtime is small,
%% because there are such a large number of test cases.
%% So, by default only the 'small' test cases are included in a test run.
%% The following environment variables control which are included and 
%% excluded.
%%
%%          ESOCK_TEST_TTEST_SMALL:  included
%%          ESOCK_TEST_TTEST_MEDIUM: excluded
%%          ESOCK_TEST_TTEST_LARGE:  excluded
%%

%% Run the entire test suite: 
%% ts:run(kernel, socket_SUITE, [batch]).
%%
%% Run a specific group:
%% ts:run(kernel, socket_SUITE, {group, foo}, [batch]).
%%
%% Run a specific test case:
%% ts:run(kernel, socket_SUITE, foo, [batch]).
%%
%% (cd /mnt/c/$LOCAL_TESTS/26/kernel_test/ && $ERL_TOP/bin/win32/erl.exe -sname kernel-26-tester -pa c:$LOCAL_TESTS/26/test_server)
%% application:set_env(kernel, test_inet_backends, true).
%% S = fun() -> ts:run(kernel, socket_SUITE, [batch]) end.
%% S = fun(SUITE) -> ts:run(kernel, SUITE, [batch]) end.
%% S = fun() -> ct:run_test([{suite, socket_SUITE}]) end.
%% S = fun(SUITE) -> ct:run_test([{suite, SUITE}]) end.
%% G = fun(GROUP) -> ts:run(kernel, socket_SUITE, {group, GROUP}, [batch]) end.
%% G = fun(SUITE, GROUP) -> ts:run(kernel, SUITE, {group, GROUP}, [batch]) end.
%% G = fun(GROUP) -> ct:run_test([{suite, socket_SUITE}, {group, GROUP}]) end.
%% G = fun(SUITE, GROUP) -> ct:run_test([{suite, SUITE}, {group, GROUP}]) end.
%% T = fun(TC) -> ts:run(kernel, socket_SUITE, TC, [batch]) end.
%% T = fun(TC) -> ct:run_test([{suite, socket_SUITE}, {testcase, TC}]) end.
%% T = fun(S, TC) -> ct:run_test([{suite, S}, {testcase, TC}]) end.
%% T = fun(S, G, TC) -> ct:run_test([{suite, S}, {group, G}, {testcase, TC}]) end.
%%
%% Some official info about AF_UNIX
%% https://devblogs.microsoft.com/commandline/windowswsl-interop-with-af_unix/



-module(socket_ttest_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include("socket_test_evaluator.hrl").
-include("kernel_test_lib.hrl").

%% Suite exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
         %% *** Time Test ***
         %% Server: transport = gen_tcp, active = false
         %% Client: transport = gen_tcp
         ttest_sgenf_cgenf_small_tcp4/1,
         ttest_sgenf_cgenf_small_tcp6/1,
         ttest_sgenf_cgenf_medium_tcp4/1,
         ttest_sgenf_cgenf_medium_tcp6/1,
         ttest_sgenf_cgenf_large_tcp4/1,
         ttest_sgenf_cgenf_large_tcp6/1,

         ttest_sgenf_cgeno_small_tcp4/1,
         ttest_sgenf_cgeno_small_tcp6/1,
         ttest_sgenf_cgeno_medium_tcp4/1,
         ttest_sgenf_cgeno_medium_tcp6/1,
         ttest_sgenf_cgeno_large_tcp4/1,
         ttest_sgenf_cgeno_large_tcp6/1,

         ttest_sgenf_cgent_small_tcp4/1,
         ttest_sgenf_cgent_small_tcp6/1,
         ttest_sgenf_cgent_medium_tcp4/1,
         ttest_sgenf_cgent_medium_tcp6/1,
         ttest_sgenf_cgent_large_tcp4/1,
         ttest_sgenf_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp, active = false
         %% Client: transport = gen_tcp(socket)
         ttest_sgenf_cgsf_small_tcp4/1,
         ttest_sgenf_cgsf_small_tcp6/1,
         ttest_sgenf_cgsf_medium_tcp4/1,
         ttest_sgenf_cgsf_medium_tcp6/1,
         ttest_sgenf_cgsf_large_tcp4/1,
         ttest_sgenf_cgsf_large_tcp6/1,

         ttest_sgenf_cgso_small_tcp4/1,
         ttest_sgenf_cgso_small_tcp6/1,
         ttest_sgenf_cgso_medium_tcp4/1,
         ttest_sgenf_cgso_medium_tcp6/1,
         ttest_sgenf_cgso_large_tcp4/1,
         ttest_sgenf_cgso_large_tcp6/1,

         ttest_sgenf_cgst_small_tcp4/1,
         ttest_sgenf_cgst_small_tcp6/1,
         ttest_sgenf_cgst_medium_tcp4/1,
         ttest_sgenf_cgst_medium_tcp6/1,
         ttest_sgenf_cgst_large_tcp4/1,
         ttest_sgenf_cgst_large_tcp6/1,

         %% Server: transport = gen_tcp, active = false
         %% Client: transport = socket(tcp)
         ttest_sgenf_csockf_small_tcp4/1,
         ttest_sgenf_csockf_small_tcp6/1,
         ttest_sgenf_csockf_medium_tcp4/1,
         ttest_sgenf_csockf_medium_tcp6/1,
         ttest_sgenf_csockf_large_tcp4/1,
         ttest_sgenf_csockf_large_tcp6/1,

         ttest_sgenf_csocko_small_tcp4/1,
         ttest_sgenf_csocko_small_tcp6/1,
         ttest_sgenf_csocko_medium_tcp4/1,
         ttest_sgenf_csocko_medium_tcp6/1,
         ttest_sgenf_csocko_large_tcp4/1,
         ttest_sgenf_csocko_large_tcp6/1,

         ttest_sgenf_csockt_small_tcp4/1,
         ttest_sgenf_csockt_small_tcp6/1,
         ttest_sgenf_csockt_medium_tcp4/1,
         ttest_sgenf_csockt_medium_tcp6/1,
         ttest_sgenf_csockt_large_tcp4/1,
         ttest_sgenf_csockt_large_tcp6/1,

         %% Server: transport = gen_tcp, active = once
         %% Client: transport = gen_tcp
         ttest_sgeno_cgenf_small_tcp4/1,
         ttest_sgeno_cgenf_small_tcp6/1,
         ttest_sgeno_cgenf_medium_tcp4/1,
         ttest_sgeno_cgenf_medium_tcp6/1,
         ttest_sgeno_cgenf_large_tcp4/1,
         ttest_sgeno_cgenf_large_tcp6/1,

         ttest_sgeno_cgeno_small_tcp4/1,
         ttest_sgeno_cgeno_small_tcp6/1,
         ttest_sgeno_cgeno_medium_tcp4/1,
         ttest_sgeno_cgeno_medium_tcp6/1,
         ttest_sgeno_cgeno_large_tcp4/1,
         ttest_sgeno_cgeno_large_tcp6/1,

         ttest_sgeno_cgent_small_tcp4/1,
         ttest_sgeno_cgent_small_tcp6/1,
         ttest_sgeno_cgent_medium_tcp4/1,
         ttest_sgeno_cgent_medium_tcp6/1,
         ttest_sgeno_cgent_large_tcp4/1,
         ttest_sgeno_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp, active = once
         %% Client: transport = gen_tcp(socket)
         ttest_sgeno_cgsf_small_tcp4/1,
         ttest_sgeno_cgsf_small_tcp6/1,
         ttest_sgeno_cgsf_medium_tcp4/1,
         ttest_sgeno_cgsf_medium_tcp6/1,
         ttest_sgeno_cgsf_large_tcp4/1,
         ttest_sgeno_cgsf_large_tcp6/1,

         ttest_sgeno_cgso_small_tcp4/1,
         ttest_sgeno_cgso_small_tcp6/1,
         ttest_sgeno_cgso_medium_tcp4/1,
         ttest_sgeno_cgso_medium_tcp6/1,
         ttest_sgeno_cgso_large_tcp4/1,
         ttest_sgeno_cgso_large_tcp6/1,

         ttest_sgeno_cgst_small_tcp4/1,
         ttest_sgeno_cgst_small_tcp6/1,
         ttest_sgeno_cgst_medium_tcp4/1,
         ttest_sgeno_cgst_medium_tcp6/1,
         ttest_sgeno_cgst_large_tcp4/1,
         ttest_sgeno_cgst_large_tcp6/1,

         %% Server: transport = gen_tcp, active = once
         %% Client: transport = socket(tcp)
         ttest_sgeno_csockf_small_tcp4/1,
         ttest_sgeno_csockf_small_tcp6/1,
         ttest_sgeno_csockf_medium_tcp4/1,
         ttest_sgeno_csockf_medium_tcp6/1,
         ttest_sgeno_csockf_large_tcp4/1,
         ttest_sgeno_csockf_large_tcp6/1,

         ttest_sgeno_csocko_small_tcp4/1,
         ttest_sgeno_csocko_small_tcp6/1,
         ttest_sgeno_csocko_medium_tcp4/1,
         ttest_sgeno_csocko_medium_tcp6/1,
         ttest_sgeno_csocko_large_tcp4/1,
         ttest_sgeno_csocko_large_tcp6/1,

         ttest_sgeno_csockt_small_tcp4/1,
         ttest_sgeno_csockt_small_tcp6/1,
         ttest_sgeno_csockt_medium_tcp4/1,
         ttest_sgeno_csockt_medium_tcp6/1,
         ttest_sgeno_csockt_large_tcp4/1,
         ttest_sgeno_csockt_large_tcp6/1,

         %% Server: transport = gen_tcp, active = true
         %% Client: transport = gen_tcp
         ttest_sgent_cgenf_small_tcp4/1,
         ttest_sgent_cgenf_small_tcp6/1,
         ttest_sgent_cgenf_medium_tcp4/1,
         ttest_sgent_cgenf_medium_tcp6/1,
         ttest_sgent_cgenf_large_tcp4/1,
         ttest_sgent_cgenf_large_tcp6/1,

         ttest_sgent_cgeno_small_tcp4/1,
         ttest_sgent_cgeno_small_tcp6/1,
         ttest_sgent_cgeno_medium_tcp4/1,
         ttest_sgent_cgeno_medium_tcp6/1,
         ttest_sgent_cgeno_large_tcp4/1,
         ttest_sgent_cgeno_large_tcp6/1,

         ttest_sgent_cgent_small_tcp4/1,
         ttest_sgent_cgent_small_tcp6/1,
         ttest_sgent_cgent_medium_tcp4/0, ttest_sgent_cgent_medium_tcp4/1,
         ttest_sgent_cgent_medium_tcp6/0, ttest_sgent_cgent_medium_tcp6/1,
         ttest_sgent_cgent_large_tcp4/0, ttest_sgent_cgent_large_tcp4/1,
         ttest_sgent_cgent_large_tcp6/0, ttest_sgent_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp, active = true
         %% Client: transport = gen_tcp(socket)
         ttest_sgent_cgsf_small_tcp4/1,
         ttest_sgent_cgsf_small_tcp6/1,
         ttest_sgent_cgsf_medium_tcp4/1,
         ttest_sgent_cgsf_medium_tcp6/1,
         ttest_sgent_cgsf_large_tcp4/1,
         ttest_sgent_cgsf_large_tcp6/1,

         ttest_sgent_cgso_small_tcp4/1,
         ttest_sgent_cgso_small_tcp6/1,
         ttest_sgent_cgso_medium_tcp4/1,
         ttest_sgent_cgso_medium_tcp6/1,
         ttest_sgent_cgso_large_tcp4/1,
         ttest_sgent_cgso_large_tcp6/1,

         ttest_sgent_cgst_small_tcp4/1,
         ttest_sgent_cgst_small_tcp6/1,
         ttest_sgent_cgst_medium_tcp4/1,
         ttest_sgent_cgst_medium_tcp6/1,
         ttest_sgent_cgst_large_tcp4/1,
         ttest_sgent_cgst_large_tcp6/1,

         %% Server: transport = gen_tcp, active = true
         %% Client: transport = socket(tcp)
         ttest_sgent_csockf_small_tcp4/1,
         ttest_sgent_csockf_small_tcp6/1,
         ttest_sgent_csockf_medium_tcp4/1,
         ttest_sgent_csockf_medium_tcp6/1,
         ttest_sgent_csockf_large_tcp4/1,
         ttest_sgent_csockf_large_tcp6/1,

         ttest_sgent_csocko_small_tcp4/1,
         ttest_sgent_csocko_small_tcp6/1,
         ttest_sgent_csocko_medium_tcp4/1,
         ttest_sgent_csocko_medium_tcp6/1,
         ttest_sgent_csocko_large_tcp4/1,
         ttest_sgent_csocko_large_tcp6/1,

         ttest_sgent_csockt_small_tcp4/1,
         ttest_sgent_csockt_small_tcp6/1,
         ttest_sgent_csockt_medium_tcp4/1,
         ttest_sgent_csockt_medium_tcp6/1,
         ttest_sgent_csockt_large_tcp4/1,
         ttest_sgent_csockt_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = false
         %% Client: transport = gen_tcp
         ttest_sgsf_cgenf_small_tcp4/1,
         ttest_sgsf_cgenf_small_tcp6/1,
         ttest_sgsf_cgenf_medium_tcp4/1,
         ttest_sgsf_cgenf_medium_tcp6/1,
         ttest_sgsf_cgenf_large_tcp4/1,
         ttest_sgsf_cgenf_large_tcp6/1,

         ttest_sgsf_cgeno_small_tcp4/1,
         ttest_sgsf_cgeno_small_tcp6/1,
         ttest_sgsf_cgeno_medium_tcp4/1,
         ttest_sgsf_cgeno_medium_tcp6/1,
         ttest_sgsf_cgeno_large_tcp4/1,
         ttest_sgsf_cgeno_large_tcp6/1,

         ttest_sgsf_cgent_small_tcp4/1,
         ttest_sgsf_cgent_small_tcp6/1,
         ttest_sgsf_cgent_medium_tcp4/1,
         ttest_sgsf_cgent_medium_tcp6/1,
         ttest_sgsf_cgent_large_tcp4/1,
         ttest_sgsf_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = false
         %% Client: transport = gen_tcp(socket), 
         ttest_sgsf_cgsf_small_tcp4/1,
         ttest_sgsf_cgsf_small_tcp6/1,
         ttest_sgsf_cgsf_medium_tcp4/1,
         ttest_sgsf_cgsf_medium_tcp6/1,
         ttest_sgsf_cgsf_large_tcp4/1,
         ttest_sgsf_cgsf_large_tcp6/1,

         ttest_sgsf_cgso_small_tcp4/1,
         ttest_sgsf_cgso_small_tcp6/1,
         ttest_sgsf_cgso_medium_tcp4/1,
         ttest_sgsf_cgso_medium_tcp6/1,
         ttest_sgsf_cgso_large_tcp4/1,
         ttest_sgsf_cgso_large_tcp6/1,

         ttest_sgsf_cgst_small_tcp4/1,
         ttest_sgsf_cgst_small_tcp6/1,
         ttest_sgsf_cgst_medium_tcp4/1,
         ttest_sgsf_cgst_medium_tcp6/1,
         ttest_sgsf_cgst_large_tcp4/1,
         ttest_sgsf_cgst_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = false
         %% Client: transport = socket(tcp)
         ttest_sgsf_csockf_small_tcp4/1,
         ttest_sgsf_csockf_small_tcp6/1,
         ttest_sgsf_csockf_medium_tcp4/1,
         ttest_sgsf_csockf_medium_tcp6/1,
         ttest_sgsf_csockf_large_tcp4/1,
         ttest_sgsf_csockf_large_tcp6/1,

         ttest_sgsf_csocko_small_tcp4/1,
         ttest_sgsf_csocko_small_tcp6/1,
         ttest_sgsf_csocko_medium_tcp4/1,
         ttest_sgsf_csocko_medium_tcp6/1,
         ttest_sgsf_csocko_large_tcp4/1,
         ttest_sgsf_csocko_large_tcp6/1,

         ttest_sgsf_csockt_small_tcp4/1,
         ttest_sgsf_csockt_small_tcp6/1,
         ttest_sgsf_csockt_medium_tcp4/1,
         ttest_sgsf_csockt_medium_tcp6/1,
         ttest_sgsf_csockt_large_tcp4/1,
         ttest_sgsf_csockt_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = once
         %% Client: transport = gen_tcp
         ttest_sgso_cgenf_small_tcp4/1,
         ttest_sgso_cgenf_small_tcp6/1,
         ttest_sgso_cgenf_medium_tcp4/1,
         ttest_sgso_cgenf_medium_tcp6/1,
         ttest_sgso_cgenf_large_tcp4/1,
         ttest_sgso_cgenf_large_tcp6/1,

         ttest_sgso_cgeno_small_tcp4/1,
         ttest_sgso_cgeno_small_tcp6/1,
         ttest_sgso_cgeno_medium_tcp4/1,
         ttest_sgso_cgeno_medium_tcp6/1,
         ttest_sgso_cgeno_large_tcp4/1,
         ttest_sgso_cgeno_large_tcp6/1,

         ttest_sgso_cgent_small_tcp4/1,
         ttest_sgso_cgent_small_tcp6/1,
         ttest_sgso_cgent_medium_tcp4/1,
         ttest_sgso_cgent_medium_tcp6/1,
         ttest_sgso_cgent_large_tcp4/1,
         ttest_sgso_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = once
         %% Client: transport = gen_tcp(socket)
         ttest_sgso_cgsf_small_tcp4/1,
         ttest_sgso_cgsf_small_tcp6/1,
         ttest_sgso_cgsf_medium_tcp4/1,
         ttest_sgso_cgsf_medium_tcp6/1,
         ttest_sgso_cgsf_large_tcp4/1,
         ttest_sgso_cgsf_large_tcp6/1,

         ttest_sgso_cgso_small_tcp4/1,
         ttest_sgso_cgso_small_tcp6/1,
         ttest_sgso_cgso_medium_tcp4/1,
         ttest_sgso_cgso_medium_tcp6/1,
         ttest_sgso_cgso_large_tcp4/1,
         ttest_sgso_cgso_large_tcp6/1,

         ttest_sgso_cgst_small_tcp4/1,
         ttest_sgso_cgst_small_tcp6/1,
         ttest_sgso_cgst_medium_tcp4/1,
         ttest_sgso_cgst_medium_tcp6/1,
         ttest_sgso_cgst_large_tcp4/1,
         ttest_sgso_cgst_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = once
         %% Client: transport = socket(tcp)
         ttest_sgso_csockf_small_tcp4/1,
         ttest_sgso_csockf_small_tcp6/1,
         ttest_sgso_csockf_medium_tcp4/1,
         ttest_sgso_csockf_medium_tcp6/1,
         ttest_sgso_csockf_large_tcp4/1,
         ttest_sgso_csockf_large_tcp6/1,

         ttest_sgso_csocko_small_tcp4/1,
         ttest_sgso_csocko_small_tcp6/1,
         ttest_sgso_csocko_medium_tcp4/1,
         ttest_sgso_csocko_medium_tcp6/1,
         ttest_sgso_csocko_large_tcp4/1,
         ttest_sgso_csocko_large_tcp6/1,

         ttest_sgso_csockt_small_tcp4/1,
         ttest_sgso_csockt_small_tcp6/1,
         ttest_sgso_csockt_medium_tcp4/1,
         ttest_sgso_csockt_medium_tcp6/1,
         ttest_sgso_csockt_large_tcp4/1,
         ttest_sgso_csockt_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = true
         %% Client: transport = gen_tcp
         ttest_sgst_cgenf_small_tcp4/1,
         ttest_sgst_cgenf_small_tcp6/1,
         ttest_sgst_cgenf_medium_tcp4/1,
         ttest_sgst_cgenf_medium_tcp6/1,
         ttest_sgst_cgenf_large_tcp4/1,
         ttest_sgst_cgenf_large_tcp6/1,

         ttest_sgst_cgeno_small_tcp4/1,
         ttest_sgst_cgeno_small_tcp6/1,
         ttest_sgst_cgeno_medium_tcp4/1,
         ttest_sgst_cgeno_medium_tcp6/1,
         ttest_sgst_cgeno_large_tcp4/1,
         ttest_sgst_cgeno_large_tcp6/1,

         ttest_sgst_cgent_small_tcp4/1,
         ttest_sgst_cgent_small_tcp6/1,
         ttest_sgst_cgent_medium_tcp4/1,
         ttest_sgst_cgent_medium_tcp6/1,
         ttest_sgst_cgent_large_tcp4/1,
         ttest_sgst_cgent_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = true
         %% Client: transport = gen_tcp(socket)
         ttest_sgst_cgsf_small_tcp4/1,
         ttest_sgst_cgsf_small_tcp6/1,
         ttest_sgst_cgsf_medium_tcp4/1,
         ttest_sgst_cgsf_medium_tcp6/1,
         ttest_sgst_cgsf_large_tcp4/1,
         ttest_sgst_cgsf_large_tcp6/1,

         ttest_sgst_cgso_small_tcp4/1,
         ttest_sgst_cgso_small_tcp6/1,
         ttest_sgst_cgso_medium_tcp4/1,
         ttest_sgst_cgso_medium_tcp6/1,
         ttest_sgst_cgso_large_tcp4/1,
         ttest_sgst_cgso_large_tcp6/1,

         ttest_sgst_cgst_small_tcp4/1,
         ttest_sgst_cgst_small_tcp6/1,
         ttest_sgst_cgst_medium_tcp4/1,
         ttest_sgst_cgst_medium_tcp6/1,
         ttest_sgst_cgst_large_tcp4/1,
         ttest_sgst_cgst_large_tcp6/1,

         %% Server: transport = gen_tcp(socket), active = true
         %% Client: transport = socket(tcp)
         ttest_sgst_csockf_small_tcp4/1,
         ttest_sgst_csockf_small_tcp6/1,
         ttest_sgst_csockf_medium_tcp4/1,
         ttest_sgst_csockf_medium_tcp6/1,
         ttest_sgst_csockf_large_tcp4/1,
         ttest_sgst_csockf_large_tcp6/1,

         ttest_sgst_csocko_small_tcp4/1,
         ttest_sgst_csocko_small_tcp6/1,
         ttest_sgst_csocko_medium_tcp4/1,
         ttest_sgst_csocko_medium_tcp6/1,
         ttest_sgst_csocko_large_tcp4/1,
         ttest_sgst_csocko_large_tcp6/1,

         ttest_sgst_csockt_small_tcp4/1,
         ttest_sgst_csockt_small_tcp6/1,
         ttest_sgst_csockt_medium_tcp4/1,
         ttest_sgst_csockt_medium_tcp6/1,
         ttest_sgst_csockt_large_tcp4/1,
         ttest_sgst_csockt_large_tcp6/1,

         %% Server: transport = socket(tcp), active = false
         %% Client: transport = gen_tcp
         ttest_ssockf_cgenf_small_tcp4/1,
         ttest_ssockf_cgenf_small_tcp6/1,
         ttest_ssockf_cgenf_medium_tcp4/1,
         ttest_ssockf_cgenf_medium_tcp6/1,
         ttest_ssockf_cgenf_large_tcp4/1,
         ttest_ssockf_cgenf_large_tcp6/1,

         ttest_ssockf_cgeno_small_tcp4/1,
         ttest_ssockf_cgeno_small_tcp6/1,
         ttest_ssockf_cgeno_medium_tcp4/1,
         ttest_ssockf_cgeno_medium_tcp6/1,
         ttest_ssockf_cgeno_large_tcp4/1,
         ttest_ssockf_cgeno_large_tcp6/1,

         ttest_ssockf_cgent_small_tcp4/1,
         ttest_ssockf_cgent_small_tcp6/1,
         ttest_ssockf_cgent_medium_tcp4/1,
         ttest_ssockf_cgent_medium_tcp6/1,
         ttest_ssockf_cgent_large_tcp4/1,
         ttest_ssockf_cgent_large_tcp6/1,

         %% Server: transport = socket(tcp), active = false
         %% Client: transport = gen_tcp(socket)
         ttest_ssockf_cgsf_small_tcp4/1,
         ttest_ssockf_cgsf_small_tcp6/1,
         ttest_ssockf_cgsf_medium_tcp4/1,
         ttest_ssockf_cgsf_medium_tcp6/1,
         ttest_ssockf_cgsf_large_tcp4/1,
         ttest_ssockf_cgsf_large_tcp6/1,
         ttest_ssockf_cgso_small_tcp4/1,
         ttest_ssockf_cgso_small_tcp6/1,
         ttest_ssockf_cgso_medium_tcp4/1,
         ttest_ssockf_cgso_medium_tcp6/1,
         ttest_ssockf_cgso_large_tcp4/1,
         ttest_ssockf_cgso_large_tcp6/1,
         ttest_ssockf_cgst_small_tcp4/1,
         ttest_ssockf_cgst_small_tcp6/1,
         ttest_ssockf_cgst_medium_tcp4/1,
         ttest_ssockf_cgst_medium_tcp6/1,
         ttest_ssockf_cgst_large_tcp4/1,
         ttest_ssockf_cgst_large_tcp6/1,

         %% Server: transport = socket(tcp), active = false
         %% Client: transport = socket(tcp)
         ttest_ssockf_csockf_small_tcp4/1,
         ttest_ssockf_csockf_small_tcp6/1,
         ttest_ssockf_csockf_small_tcpL/1,
         ttest_ssockf_csockf_medium_tcp4/1,
         ttest_ssockf_csockf_medium_tcp6/1,
         ttest_ssockf_csockf_medium_tcpL/1,
         ttest_ssockf_csockf_large_tcp4/1,
         ttest_ssockf_csockf_large_tcp6/1,
         ttest_ssockf_csockf_large_tcpL/1,

         ttest_ssockf_csocko_small_tcp4/1,
         ttest_ssockf_csocko_small_tcp6/1,
         ttest_ssockf_csocko_small_tcpL/1,
         ttest_ssockf_csocko_medium_tcp4/1,
         ttest_ssockf_csocko_medium_tcp6/1,
         ttest_ssockf_csocko_medium_tcpL/1,
         ttest_ssockf_csocko_large_tcp4/1,
         ttest_ssockf_csocko_large_tcp6/1,
         ttest_ssockf_csocko_large_tcpL/1,

         ttest_ssockf_csockt_small_tcp4/1,
         ttest_ssockf_csockt_small_tcp6/1,
         ttest_ssockf_csockt_small_tcpL/1,
         ttest_ssockf_csockt_medium_tcp4/1,
         ttest_ssockf_csockt_medium_tcp6/1,
         ttest_ssockf_csockt_medium_tcpL/1,
         ttest_ssockf_csockt_large_tcp4/1,
         ttest_ssockf_csockt_large_tcp6/1,
         ttest_ssockf_csockt_large_tcpL/1,

         %% Server: transport = socket(tcp), active = once
         %% Client: transport = gen_tcp
         ttest_ssocko_cgenf_small_tcp4/1,
         ttest_ssocko_cgenf_small_tcp6/1,
         ttest_ssocko_cgenf_medium_tcp4/1,
         ttest_ssocko_cgenf_medium_tcp6/1,
         ttest_ssocko_cgenf_large_tcp4/1,
         ttest_ssocko_cgenf_large_tcp6/1,

         ttest_ssocko_cgeno_small_tcp4/1,
         ttest_ssocko_cgeno_small_tcp6/1,
         ttest_ssocko_cgeno_medium_tcp4/1,
         ttest_ssocko_cgeno_medium_tcp6/1,
         ttest_ssocko_cgeno_large_tcp4/1,
         ttest_ssocko_cgeno_large_tcp6/1,

         ttest_ssocko_cgent_small_tcp4/1,
         ttest_ssocko_cgent_small_tcp6/1,
         ttest_ssocko_cgent_medium_tcp4/1,
         ttest_ssocko_cgent_medium_tcp6/1,
         ttest_ssocko_cgent_large_tcp4/1,
         ttest_ssocko_cgent_large_tcp6/1,

         %% Server: transport = socket(tcp), active = once
         %% Client: transport = gen_tcp(socket)
         ttest_ssocko_cgsf_small_tcp4/1,
         ttest_ssocko_cgsf_small_tcp6/1,
         ttest_ssocko_cgsf_medium_tcp4/1,
         ttest_ssocko_cgsf_medium_tcp6/1,
         ttest_ssocko_cgsf_large_tcp4/1,
         ttest_ssocko_cgsf_large_tcp6/1,

         ttest_ssocko_cgso_small_tcp4/1,
         ttest_ssocko_cgso_small_tcp6/1,
         ttest_ssocko_cgso_medium_tcp4/1,
         ttest_ssocko_cgso_medium_tcp6/1,
         ttest_ssocko_cgso_large_tcp4/1,
         ttest_ssocko_cgso_large_tcp6/1,

         ttest_ssocko_cgst_small_tcp4/1,
         ttest_ssocko_cgst_small_tcp6/1,
         ttest_ssocko_cgst_medium_tcp4/1,
         ttest_ssocko_cgst_medium_tcp6/1,
         ttest_ssocko_cgst_large_tcp4/1,
         ttest_ssocko_cgst_large_tcp6/1,

         %% Server: transport = socket(tcp), active = once
         %% Client: transport = socket(tcp)
         ttest_ssocko_csockf_small_tcp4/1,
         ttest_ssocko_csockf_small_tcp6/1,
         ttest_ssocko_csockf_small_tcpL/1,
         ttest_ssocko_csockf_medium_tcp4/1,
         ttest_ssocko_csockf_medium_tcpL/1,
         ttest_ssocko_csockf_medium_tcp6/1,
         ttest_ssocko_csockf_large_tcp4/1,
         ttest_ssocko_csockf_large_tcp6/1,
         ttest_ssocko_csockf_large_tcpL/1,

         ttest_ssocko_csocko_small_tcp4/1,
         ttest_ssocko_csocko_small_tcp6/1,
         ttest_ssocko_csocko_small_tcpL/1,
         ttest_ssocko_csocko_medium_tcp4/1,
         ttest_ssocko_csocko_medium_tcp6/1,
         ttest_ssocko_csocko_medium_tcpL/1,
         ttest_ssocko_csocko_large_tcp4/1,
         ttest_ssocko_csocko_large_tcp6/1,
         ttest_ssocko_csocko_large_tcpL/1,

         ttest_ssocko_csockt_small_tcp4/1,
         ttest_ssocko_csockt_small_tcp6/1,
         ttest_ssocko_csockt_small_tcpL/1,
         ttest_ssocko_csockt_medium_tcp4/1,
         ttest_ssocko_csockt_medium_tcp6/1,
         ttest_ssocko_csockt_medium_tcpL/1,
         ttest_ssocko_csockt_large_tcp4/1,
         ttest_ssocko_csockt_large_tcp6/1,
         ttest_ssocko_csockt_large_tcpL/1,

         %% Server: transport = socket(tcp), active = true
         %% Client: transport = gen_tcp
         ttest_ssockt_cgenf_small_tcp4/1,
         ttest_ssockt_cgenf_small_tcp6/1,
         ttest_ssockt_cgenf_medium_tcp4/1,
         ttest_ssockt_cgenf_medium_tcp6/1,
         ttest_ssockt_cgenf_large_tcp4/1,
         ttest_ssockt_cgenf_large_tcp6/1,

         ttest_ssockt_cgeno_small_tcp4/1,
         ttest_ssockt_cgeno_small_tcp6/1,
         ttest_ssockt_cgeno_medium_tcp4/1,
         ttest_ssockt_cgeno_medium_tcp6/1,
         ttest_ssockt_cgeno_large_tcp4/1,
         ttest_ssockt_cgeno_large_tcp6/1,

         ttest_ssockt_cgent_small_tcp4/1,
         ttest_ssockt_cgent_small_tcp6/1,
         ttest_ssockt_cgent_medium_tcp4/1,
         ttest_ssockt_cgent_medium_tcp6/1,
         ttest_ssockt_cgent_large_tcp4/1,
         ttest_ssockt_cgent_large_tcp6/1,

         %% Server: transport = socket(tcp), active = true
         %% Client: transport = gen_tcp(socket)
         ttest_ssockt_cgsf_small_tcp4/1,
         ttest_ssockt_cgsf_small_tcp6/1,
         ttest_ssockt_cgsf_medium_tcp4/1,
         ttest_ssockt_cgsf_medium_tcp6/1,
         ttest_ssockt_cgsf_large_tcp4/1,
         ttest_ssockt_cgsf_large_tcp6/1,

         ttest_ssockt_cgso_small_tcp4/1,
         ttest_ssockt_cgso_small_tcp6/1,
         ttest_ssockt_cgso_medium_tcp4/1,
         ttest_ssockt_cgso_medium_tcp6/1,
         ttest_ssockt_cgso_large_tcp4/1,
         ttest_ssockt_cgso_large_tcp6/1,

         ttest_ssockt_cgst_small_tcp4/1,
         ttest_ssockt_cgst_small_tcp6/1,
         ttest_ssockt_cgst_medium_tcp4/1,
         ttest_ssockt_cgst_medium_tcp6/1,
         ttest_ssockt_cgst_large_tcp4/1,
         ttest_ssockt_cgst_large_tcp6/1,

         %% Server: transport = socket(tcp), active = true
         %% Client: transport = socket(tcp)
         ttest_ssockt_csockf_small_tcp4/1,
         ttest_ssockt_csockf_small_tcp6/1,
         ttest_ssockt_csockf_small_tcpL/1,
         ttest_ssockt_csockf_medium_tcp4/1,
         ttest_ssockt_csockf_medium_tcp6/1,
         ttest_ssockt_csockf_medium_tcpL/1,
         ttest_ssockt_csockf_large_tcp4/1,
         ttest_ssockt_csockf_large_tcp6/1,
         ttest_ssockt_csockf_large_tcpL/1,

         ttest_ssockt_csocko_small_tcp4/1,
         ttest_ssockt_csocko_small_tcp6/1,
         ttest_ssockt_csocko_small_tcpL/1,
         ttest_ssockt_csocko_medium_tcp4/1,
         ttest_ssockt_csocko_medium_tcp6/1,
         ttest_ssockt_csocko_medium_tcpL/1,
         ttest_ssockt_csocko_large_tcp4/1,
         ttest_ssockt_csocko_large_tcp6/1,
         ttest_ssockt_csocko_large_tcpL/1,

         ttest_ssockt_csockt_small_tcp4/1,
         ttest_ssockt_csockt_small_tcp6/1,
         ttest_ssockt_csockt_small_tcpL/1,
         ttest_ssockt_csockt_medium_tcp4/1,
         ttest_ssockt_csockt_medium_tcp6/1,
         ttest_ssockt_csockt_medium_tcpL/1,
         ttest_ssockt_csockt_large_tcp4/1,
         ttest_ssockt_csockt_large_tcp6/1,
         ttest_ssockt_csockt_large_tcpL/1,

         ttest_simple_ssockt_csocko_small_tcp4/1,
         ttest_simple_ssockt_csocko_small_tcp6/1,
         ttest_simple_ssockt_csocko_small_tcpL/1

        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SLIB,       socket_test_lib).
-define(KLIB,       kernel_test_lib).
-define(TTEST_LIB,  socket_test_ttest_lib).
-define(LOGGER,     socket_test_logger).

-define(TPP_SMALL,  lists:seq(1, 8)).
-define(TPP_MEDIUM, lists:flatten(lists:duplicate(100, ?TPP_SMALL))).
-define(TPP_LARGE,  lists:flatten(lists:duplicate(100, ?TPP_MEDIUM))).

-define(TPP_SMALL_NUM,  5000).
-define(TPP_MEDIUM_NUM, 500).
-define(TPP_LARGE_NUM,  50).
-define(TPP_NUM(Config, Base), (Base) div lookup(kernel_factor, 1, Config)).

-define(WINDOWS, {win32,nt}).

-define(TTEST_STANDARD_RUNTIME,              ?SECS(1)).
-define(TTEST_BENCH_RUNTIME,                 ?SECS(10)).
-define(TTEST_MIN_FACTOR,                    3).
-define(TTEST_MIN_FACTOR_WIN,                ?TTEST_MIN_FACTOR-1).
-define(TTEST_DEFAULT_SMALL_MAX_OUTSTANDING, 50).
-define(TTEST_DEFAULT_MEDIUM_MAX_OUTSTANDING,
        ?TTEST_MK_DEFAULT_MAX_OUTSTANDING(
           ?TTEST_DEFAULT_SMALL_MAX_OUTSTANDING)).
-define(TTEST_DEFAULT_LARGE_MAX_OUTSTANDING,
        ?TTEST_MK_DEFAULT_MAX_OUTSTANDING(
           ?TTEST_DEFAULT_MEDIUM_MAX_OUTSTANDING)).

-define(TTEST_MK_DEFAULT_MAX_OUTSTANDING(__X__),
        if ((__X__) >= 5) ->
                (__X__) div 5;
           true ->
                1
        end).

-define(TTEST_TCP(C, D, ST, SA, CT, CA, MSZ, MO),
        ttest_tcp(?FUNCTION_NAME, which_ttest_runtime((C)),
                  (D), (ST), (SA), (CT), (CA), (MSZ), (MO))).
-define(TTEST_TCP_SMALL(C, D, ST, SA, CT, CA),
        ?TTEST_TCP((C), (D), (ST), (SA), (CT), (CA),
                   1, ttest_small_max_outstanding((C)))).
-define(TTEST_TCP_MEDIUM(C, D, ST, SA, CT, CA),
        ?TTEST_TCP((C), (D), (ST), (SA), (CT), (CA),
                   2, ttest_medium_max_outstanding((C)))).
-define(TTEST_TCP_LARGE(C, D, ST, SA, CT, CA),
        ?TTEST_TCP((C), (D), (ST), (SA), (CT), (CA),
                   3, ttest_large_max_outstanding((C)))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes,1}}].

all() -> 
    [{group, standard}].
    

groups() -> 
    [{standard,                    [], standard_cases()},
     {bench,                       [], bench_cases()},
     {ttest,                       [], ttest_cases()},
     {ttest_sgenf,                 [], ttest_sgenf_cases()},
     {ttest_sgenf_cgen,            [], ttest_sgenf_cgen_cases()},
     {ttest_sgenf_cgenf,           [], ttest_sgenf_cgenf_cases()},
     {ttest_sgenf_cgeno,           [], ttest_sgenf_cgeno_cases()},
     {ttest_sgenf_cgent,           [], ttest_sgenf_cgent_cases()},
     {ttest_sgenf_cgs,             [], ttest_sgenf_cgs_cases()},
     {ttest_sgenf_cgsf,            [], ttest_sgenf_cgsf_cases()},
     {ttest_sgenf_cgso,            [], ttest_sgenf_cgso_cases()},
     {ttest_sgenf_cgst,            [], ttest_sgenf_cgst_cases()},
     {ttest_sgenf_csock,           [], ttest_sgenf_csock_cases()},
     {ttest_sgenf_csockf,          [], ttest_sgenf_csockf_cases()},
     {ttest_sgenf_csocko,          [], ttest_sgenf_csocko_cases()},
     {ttest_sgenf_csockt,          [], ttest_sgenf_csockt_cases()},
     {ttest_sgeno,                 [], ttest_sgeno_cases()},
     {ttest_sgeno_cgen,            [], ttest_sgeno_cgen_cases()},
     {ttest_sgeno_cgenf,           [], ttest_sgeno_cgenf_cases()},
     {ttest_sgeno_cgeno,           [], ttest_sgeno_cgeno_cases()},
     {ttest_sgeno_cgent,           [], ttest_sgeno_cgent_cases()},
     {ttest_sgeno_cgs,             [], ttest_sgeno_cgs_cases()},
     {ttest_sgeno_cgsf,            [], ttest_sgeno_cgsf_cases()},
     {ttest_sgeno_cgso,            [], ttest_sgeno_cgso_cases()},
     {ttest_sgeno_cgst,            [], ttest_sgeno_cgst_cases()},
     {ttest_sgeno_csock,           [], ttest_sgeno_csock_cases()},
     {ttest_sgeno_csockf,          [], ttest_sgeno_csockf_cases()},
     {ttest_sgeno_csocko,          [], ttest_sgeno_csocko_cases()},
     {ttest_sgeno_csockt,          [], ttest_sgeno_csockt_cases()},
     {ttest_sgent,                 [], ttest_sgent_cases()},
     {ttest_sgent_cgen,            [], ttest_sgent_cgen_cases()},
     {ttest_sgent_cgenf,           [], ttest_sgent_cgenf_cases()},
     {ttest_sgent_cgeno,           [], ttest_sgent_cgeno_cases()},
     {ttest_sgent_cgent,           [], ttest_sgent_cgent_cases()},
     {ttest_sgent_cgs,             [], ttest_sgent_cgs_cases()},
     {ttest_sgent_cgsf,            [], ttest_sgent_cgsf_cases()},
     {ttest_sgent_cgso,            [], ttest_sgent_cgso_cases()},
     {ttest_sgent_cgst,            [], ttest_sgent_cgst_cases()},
     {ttest_sgent_csock,           [], ttest_sgent_csock_cases()},
     {ttest_sgent_csockf,          [], ttest_sgent_csockf_cases()},
     {ttest_sgent_csocko,          [], ttest_sgent_csocko_cases()},
     {ttest_sgent_csockt,          [], ttest_sgent_csockt_cases()},

     {ttest_sgsf,                  [], ttest_sgsf_cases()},
     {ttest_sgsf_cgen,             [], ttest_sgsf_cgen_cases()},
     {ttest_sgsf_cgenf,            [], ttest_sgsf_cgenf_cases()},
     {ttest_sgsf_cgeno,            [], ttest_sgsf_cgeno_cases()},
     {ttest_sgsf_cgent,            [], ttest_sgsf_cgent_cases()},
     {ttest_sgsf_cgs,              [], ttest_sgsf_cgs_cases()},
     {ttest_sgsf_cgsf,             [], ttest_sgsf_cgsf_cases()},
     {ttest_sgsf_cgso,             [], ttest_sgsf_cgso_cases()},
     {ttest_sgsf_cgst,             [], ttest_sgsf_cgst_cases()},
     {ttest_sgsf_csock,            [], ttest_sgsf_csock_cases()},
     {ttest_sgsf_csockf,           [], ttest_sgsf_csockf_cases()},
     {ttest_sgsf_csocko,           [], ttest_sgsf_csocko_cases()},
     {ttest_sgsf_csockt,           [], ttest_sgsf_csockt_cases()},
     {ttest_sgso,                  [], ttest_sgso_cases()},
     {ttest_sgso_cgen,             [], ttest_sgso_cgen_cases()},
     {ttest_sgso_cgenf,            [], ttest_sgso_cgenf_cases()},
     {ttest_sgso_cgeno,            [], ttest_sgso_cgeno_cases()},
     {ttest_sgso_cgent,            [], ttest_sgso_cgent_cases()},
     {ttest_sgso_cgs,              [], ttest_sgso_cgs_cases()},
     {ttest_sgso_cgsf,             [], ttest_sgso_cgsf_cases()},
     {ttest_sgso_cgso,             [], ttest_sgso_cgso_cases()},
     {ttest_sgso_cgst,             [], ttest_sgso_cgst_cases()},
     {ttest_sgso_csock,            [], ttest_sgso_csock_cases()},
     {ttest_sgso_csockf,           [], ttest_sgso_csockf_cases()},
     {ttest_sgso_csocko,           [], ttest_sgso_csocko_cases()},
     {ttest_sgso_csockt,           [], ttest_sgso_csockt_cases()},
     {ttest_sgst,                  [], ttest_sgst_cases()},
     {ttest_sgst_cgen,             [], ttest_sgst_cgen_cases()},
     {ttest_sgst_cgenf,            [], ttest_sgst_cgenf_cases()},
     {ttest_sgst_cgeno,            [], ttest_sgst_cgeno_cases()},
     {ttest_sgst_cgent,            [], ttest_sgst_cgent_cases()},
     {ttest_sgst_cgs,              [], ttest_sgst_cgs_cases()},
     {ttest_sgst_cgsf,             [], ttest_sgst_cgsf_cases()},
     {ttest_sgst_cgso,             [], ttest_sgst_cgso_cases()},
     {ttest_sgst_cgst,             [], ttest_sgst_cgst_cases()},
     {ttest_sgst_csock,            [], ttest_sgst_csock_cases()},
     {ttest_sgst_csockf,           [], ttest_sgst_csockf_cases()},
     {ttest_sgst_csocko,           [], ttest_sgst_csocko_cases()},
     {ttest_sgst_csockt,           [], ttest_sgst_csockt_cases()},

     {ttest_ssockf,                [], ttest_ssockf_cases()},
     {ttest_ssockf_cgen,           [], ttest_ssockf_cgen_cases()},
     {ttest_ssockf_cgenf,          [], ttest_ssockf_cgenf_cases()},
     {ttest_ssockf_cgeno,          [], ttest_ssockf_cgeno_cases()},
     {ttest_ssockf_cgent,          [], ttest_ssockf_cgent_cases()},
     {ttest_ssockf_cgs,            [], ttest_ssockf_cgs_cases()},
     {ttest_ssockf_cgsf,           [], ttest_ssockf_cgsf_cases()},
     {ttest_ssockf_cgso,           [], ttest_ssockf_cgso_cases()},
     {ttest_ssockf_cgst,           [], ttest_ssockf_cgst_cases()},
     {ttest_ssockf_csock,          [], ttest_ssockf_csock_cases()},
     {ttest_ssockf_csockf,         [], ttest_ssockf_csockf_cases()},
     {ttest_ssockf_csocko,         [], ttest_ssockf_csocko_cases()},
     {ttest_ssockf_csockt,         [], ttest_ssockf_csockt_cases()},
     {ttest_ssocko,                [], ttest_ssocko_cases()},
     {ttest_ssocko_cgen,           [], ttest_ssocko_cgen_cases()},
     {ttest_ssocko_cgenf,          [], ttest_ssocko_cgenf_cases()},
     {ttest_ssocko_cgeno,          [], ttest_ssocko_cgeno_cases()},
     {ttest_ssocko_cgent,          [], ttest_ssocko_cgent_cases()},
     {ttest_ssocko_cgs,            [], ttest_ssocko_cgs_cases()},
     {ttest_ssocko_cgsf,           [], ttest_ssocko_cgsf_cases()},
     {ttest_ssocko_cgso,           [], ttest_ssocko_cgso_cases()},
     {ttest_ssocko_cgst,           [], ttest_ssocko_cgst_cases()},
     {ttest_ssocko_csock,          [], ttest_ssocko_csock_cases()},
     {ttest_ssocko_csockf,         [], ttest_ssocko_csockf_cases()},
     {ttest_ssocko_csocko,         [], ttest_ssocko_csocko_cases()},
     {ttest_ssocko_csockt,         [], ttest_ssocko_csockt_cases()},
     {ttest_ssockt,                [], ttest_ssockt_cases()},
     {ttest_ssockt_cgen,           [], ttest_ssockt_cgen_cases()},
     {ttest_ssockt_cgenf,          [], ttest_ssockt_cgenf_cases()},
     {ttest_ssockt_cgeno,          [], ttest_ssockt_cgeno_cases()},
     {ttest_ssockt_cgent,          [], ttest_ssockt_cgent_cases()},
     {ttest_ssockt_cgs,            [], ttest_ssockt_cgs_cases()},
     {ttest_ssockt_cgsf,           [], ttest_ssockt_cgsf_cases()},
     {ttest_ssockt_cgso,           [], ttest_ssockt_cgso_cases()},
     {ttest_ssockt_cgst,           [], ttest_ssockt_cgst_cases()},
     {ttest_ssockt_csock,          [], ttest_ssockt_csock_cases()},
     {ttest_ssockt_csockf,         [], ttest_ssockt_csockf_cases()},
     {ttest_ssockt_csocko,         [], ttest_ssockt_csocko_cases()},
     {ttest_ssockt_csockt,         [], ttest_ssockt_csockt_cases()},
     {ttest_simple_ssockt,         [], ttest_simple_ssockt_cases()},
     {ttest_simple_ssockt_csock,   [], ttest_simple_ssockt_csock_cases()},
     {ttest_simple_ssockt_csocko,  [], ttest_simple_ssockt_csocko_cases()}
    ].


%% Condition for running the ttest cases.
%% No point in running these cases unless the machine is
%% reasonably fast.
ttest_condition(Config) ->
    OsType = os:type(),
    case ?config(kernel_factor, Config) of
        Factor when (OsType =:= ?WINDOWS) andalso
                    is_integer(Factor) andalso
                    (Factor =< ?TTEST_MIN_FACTOR_WIN) ->
            ?P("~w -> (win) passed", [?FUNCTION_NAME]),
            ok;
        Factor when is_integer(Factor) andalso (Factor =< ?TTEST_MIN_FACTOR) ->
            ?P("~w -> passed", [?FUNCTION_NAME]),
            ok;
        Factor when is_integer(Factor) ->
            ?P("~w -> ~w => check special condition", [?FUNCTION_NAME, Factor]),
            case ?TTEST_CONDITION() of
                infinity ->
                    ?P("~w -> unlimited", [?FUNCTION_NAME]),
                    ok;
                F when is_integer(F) andalso (F > Factor) ->
                    ?P("~w -> ~w > ~w", [?FUNCTION_NAME, F, Factor]),
                    ok;
                _ ->
                    {skip, ?F("Too slow for TTest (~w)", [Factor])}
            end;
        _ ->
            ?P("~w -> undefined", [?FUNCTION_NAME]),
            {skip, "Too slow for TTest (undef)"}
    end.

ttest_small_max_outstanding(Config) ->
    EnvKey                = "ESOCK_TEST_TTEST_SMALL_MAX_OUTSTANDING",
    Default               = ?TTEST_DEFAULT_SMALL_MAX_OUTSTANDING,
    DefaultMaxOutstanding = ttest_max_outstanding(Config, EnvKey, Default),
    ttest_max_outstanding(Config, DefaultMaxOutstanding).

ttest_medium_max_outstanding(Config) ->
    SmallMaxOutstanding   = ttest_small_max_outstanding(Config),
    EnvKey                = "ESOCK_TEST_TTEST_MEDIUM_MAX_OUTSTANDING",
    Default               = ?TTEST_MK_DEFAULT_MAX_OUTSTANDING(
                               SmallMaxOutstanding),
    DefaultMaxOutstanding = ttest_max_outstanding(Config, EnvKey, Default),
    ttest_max_outstanding(Config, DefaultMaxOutstanding).

ttest_large_max_outstanding(Config) ->
    MediumMaxOutstanding  = ttest_medium_max_outstanding(Config),
    EnvKey                = "ESOCK_TEST_TTEST_LARGE_MAX_OUTSTANDING",
    Default               = ?TTEST_MK_DEFAULT_MAX_OUTSTANDING(
                               MediumMaxOutstanding),
    DefaultMaxOutstanding = ttest_max_outstanding(Config, EnvKey, Default),
    ttest_max_outstanding(Config, DefaultMaxOutstanding).

ttest_max_outstanding(Config, Default)
  when is_integer(Default) andalso (Default > 1) ->
    %% Note that we should not even get here if factor > 4
    case ?config(kernel_factor, Config) of
        1                     -> Default;
        2 when (Default >= 2) -> Default div 2;
        3 when (Default >= 4) -> Default div 4;
        _ when (Default >= 8) -> Default div 8;
        _                     -> 1
    end;
ttest_max_outstanding(_, _) ->
    1.

ttest_max_outstanding(Config, EnvKey, Default) ->
    Key = list_to_atom(string:to_lower(EnvKey)),
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, MO}} when is_integer(MO) andalso (MO > 0) ->
            MO;
        _ ->
            case os:getenv(EnvKey) of
                false ->
                    Default;
                Val ->
                    try list_to_integer(Val) of
                        MO when (MO > 0) ->
                            MO;
                        _ ->
                            1
                    catch
                        _:_:_ ->
                            Default
                    end
            end
    end.

standard_cases() ->
    [
     {group, ttest}
    ].

bench_cases() ->
    [
     {group, ttest}
    ].

ttest_cases() ->
    [
     %% Server: transport = gen_tcp, active = false
     {group, ttest_sgenf},

     %% Server: transport = gen_tcp, active = once
     {group, ttest_sgeno},

     %% Server: transport = gen_tcp, active = true
     {group, ttest_sgent},

     %% Server: transport = gen_tcp(socket), active = false
     {group, ttest_sgsf},

     %% Server: transport = gen_tcp(socket), active = once
     {group, ttest_sgso},

     %% Server: transport = gen_tcp(socket), active = true
     {group, ttest_sgst},

     %% Server: transport = socket(tcp), active = false
     {group, ttest_ssockf},

     %% Server: transport = socket(tcp), active = once
     {group, ttest_ssocko},

     %% Server: transport = socket(tcp), active = true
     {group, ttest_ssockt},

     %% simple: Server: transport = socket(tcp), active = true
     {group, ttest_simple_ssockt}

    ].


ttest_conditional_cases(Env, Default, Cases) ->
    case os:getenv(Env) of
        false ->
            Default;
        Val ->
            case list_to_atom(string:to_lower(Val)) of
                Use when (Use =:= include) orelse 
                         (Use =:= enable) orelse 
                         (Use =:= true) ->
                    Cases;
                _ -> % Assumed to be explicitly *disabled*
                    []
            end
    end.

ttest_small_conditional_cases(Cases) ->
    ttest_conditional_cases("ESOCK_TEST_TTEST_SMALL", Cases, Cases).

ttest_medium_conditional_cases(Cases) ->
    ttest_conditional_cases("ESOCK_TEST_TTEST_MEDIUM", [], Cases).

ttest_large_conditional_cases(Cases) ->
    ttest_conditional_cases("ESOCK_TEST_TTEST_LARGE", [], Cases).

ttest_select_conditional_cases(Small, Medium, Large) ->
    ttest_small_conditional_cases(Small) ++
        ttest_medium_conditional_cases(Medium) ++
        ttest_large_conditional_cases(Large).


%% Server: transport = gen_tcp, active = false
ttest_sgenf_cases() ->
    [
     {group, ttest_sgenf_cgen},
     {group, ttest_sgenf_cgs},
     {group, ttest_sgenf_csock}
    ].

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp
ttest_sgenf_cgen_cases() ->
    [
     {group, ttest_sgenf_cgenf},
     {group, ttest_sgenf_cgeno},
     {group, ttest_sgenf_cgent}
    ].

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp, active = false
ttest_sgenf_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_cgenf_small_tcp4,
       ttest_sgenf_cgenf_small_tcp6],
      %% Medium
      [ttest_sgenf_cgenf_medium_tcp4,
       ttest_sgenf_cgenf_medium_tcp6],
      %% Large
      [ttest_sgenf_cgenf_large_tcp4,
       ttest_sgenf_cgenf_large_tcp6]).

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp, active = once
ttest_sgenf_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_cgeno_small_tcp4,
       ttest_sgenf_cgeno_small_tcp6],
      %% Medium
      [ttest_sgenf_cgeno_medium_tcp4,
       ttest_sgenf_cgeno_medium_tcp6],
      %% Large
      [ttest_sgenf_cgeno_large_tcp4,
       ttest_sgenf_cgeno_large_tcp6]).

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp, active = true
ttest_sgenf_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_cgent_small_tcp4,
       ttest_sgenf_cgent_small_tcp6],
      %% Medium
      [ttest_sgenf_cgent_medium_tcp4,
       ttest_sgenf_cgent_medium_tcp6],
      %% Large
      [ttest_sgenf_cgent_large_tcp4,
       ttest_sgenf_cgent_large_tcp6]).


%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp(socket)
ttest_sgenf_cgs_cases() ->
    [
     {group, ttest_sgenf_cgsf},
     {group, ttest_sgenf_cgso},
     {group, ttest_sgenf_cgst}
    ].

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp(socket), active = once
ttest_sgenf_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_cgsf_small_tcp4,
       ttest_sgenf_cgsf_small_tcp6],
      %% Medium
      [ttest_sgenf_cgsf_medium_tcp4,
       ttest_sgenf_cgsf_medium_tcp6],
      %% Large
      [ttest_sgenf_cgsf_large_tcp4,
       ttest_sgenf_cgsf_large_tcp6]).

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp(socket), active = once
ttest_sgenf_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_cgso_small_tcp4,
       ttest_sgenf_cgso_small_tcp6],
      %% Medium
      [ttest_sgenf_cgso_medium_tcp4,
       ttest_sgenf_cgso_medium_tcp6],
      %% Large
      [ttest_sgenf_cgso_large_tcp4,
       ttest_sgenf_cgso_large_tcp6]).

%% Server: transport = gen_tcp, active = false
%% Client: transport = gen_tcp(socket), active = true
ttest_sgenf_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_cgst_small_tcp4,
       ttest_sgenf_cgst_small_tcp6],
      %% Medium
      [ttest_sgenf_cgst_medium_tcp4,
       ttest_sgenf_cgst_medium_tcp6],
      %% Large
      [ttest_sgenf_cgst_large_tcp4,
       ttest_sgenf_cgst_large_tcp6]).

%% Server: transport = gen_tcp, active = false
%% Client: transport = socket(tcp)
ttest_sgenf_csock_cases() ->
    [
     {group, ttest_sgenf_csockf},
     {group, ttest_sgenf_csocko},
     {group, ttest_sgenf_csockt}
    ].

ttest_sgenf_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_csockf_small_tcp4,
       ttest_sgenf_csockf_small_tcp6],
      %% Medium
      [ttest_sgenf_csockf_medium_tcp4,
       ttest_sgenf_csockf_medium_tcp6],
      %% Large
      [ttest_sgenf_csockf_large_tcp4,
       ttest_sgenf_csockf_large_tcp6]).

ttest_sgenf_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_csocko_small_tcp4,
       ttest_sgenf_csocko_small_tcp6],
      %% Medium
      [ttest_sgenf_csocko_medium_tcp4,
       ttest_sgenf_csocko_medium_tcp6],
      %% Large
      [ttest_sgenf_csocko_large_tcp4,
       ttest_sgenf_csocko_large_tcp6]).

ttest_sgenf_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgenf_csockt_small_tcp4,
       ttest_sgenf_csockt_small_tcp6],
      %% Medium
      [ttest_sgenf_csockt_medium_tcp4,
       ttest_sgenf_csockt_medium_tcp6],
      %% Large
     [ttest_sgenf_csockt_large_tcp4,
      ttest_sgenf_csockt_large_tcp6]).


%% Server: transport = gen_tcp(socket), active = false
ttest_sgsf_cases() ->
    [
     {group, ttest_sgsf_cgen},
     {group, ttest_sgsf_cgs},
     {group, ttest_sgsf_csock}
    ].

%% Server: transport = gen_tcp(socket), active = false
%% Client: transport = gen_tcp
ttest_sgsf_cgen_cases() ->
    [
     {group, ttest_sgsf_cgenf},
     {group, ttest_sgsf_cgeno},
     {group, ttest_sgsf_cgent}
    ].

ttest_sgsf_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_cgenf_small_tcp4,
       ttest_sgsf_cgenf_small_tcp6],
      %% Medium
      [ttest_sgsf_cgenf_medium_tcp4,
       ttest_sgsf_cgenf_medium_tcp6],
      %% Large
      [ttest_sgsf_cgenf_large_tcp4,
       ttest_sgsf_cgenf_large_tcp6]).

ttest_sgsf_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_cgeno_small_tcp4,
       ttest_sgsf_cgeno_small_tcp6],
      %% Medium
      [ttest_sgsf_cgeno_medium_tcp4,
       ttest_sgsf_cgeno_medium_tcp6],
      %% Large
      [ttest_sgsf_cgeno_large_tcp4,
       ttest_sgsf_cgeno_large_tcp6]).

ttest_sgsf_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_cgent_small_tcp4,
       ttest_sgsf_cgent_small_tcp6],
      %% Medium
      [ttest_sgsf_cgent_medium_tcp4,
       ttest_sgsf_cgent_medium_tcp6],
      %% Large
      [ttest_sgsf_cgent_large_tcp4,
       ttest_sgsf_cgent_large_tcp6]).

%% Server: transport = gen_tcp(socket), active = false
%% Client: transport = gen_tcp(socket)
ttest_sgsf_cgs_cases() ->
    [
     {group, ttest_sgsf_cgsf},
     {group, ttest_sgsf_cgso},
     {group, ttest_sgsf_cgst}
    ].

ttest_sgsf_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_cgsf_small_tcp4,
       ttest_sgsf_cgsf_small_tcp6],
      %% Medium
      [ttest_sgsf_cgsf_medium_tcp4,
       ttest_sgsf_cgsf_medium_tcp6],
      %% Large
      [ttest_sgsf_cgsf_large_tcp4,
       ttest_sgsf_cgsf_large_tcp6]).

ttest_sgsf_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_cgso_small_tcp4,
       ttest_sgsf_cgso_small_tcp6],
      %% Medium
      [ttest_sgsf_cgso_medium_tcp4,
       ttest_sgsf_cgso_medium_tcp6],
      %% Large
      [ttest_sgsf_cgso_large_tcp4,
       ttest_sgsf_cgso_large_tcp6]).

ttest_sgsf_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_cgst_small_tcp4,
       ttest_sgsf_cgst_small_tcp6],
      %% Medium
      [ttest_sgsf_cgst_medium_tcp4,
       ttest_sgsf_cgst_medium_tcp6],
      %% Large
      [ttest_sgsf_cgst_large_tcp4,
       ttest_sgsf_cgst_large_tcp6]).

%% Server: transport = gen_tcp(socket), active = false
%% Client: transport = socket(tcp)
ttest_sgsf_csock_cases() ->
    [
     {group, ttest_sgsf_csockf},
     {group, ttest_sgsf_csocko},
     {group, ttest_sgsf_csockt}
    ].

ttest_sgsf_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_csockf_small_tcp4,
       ttest_sgsf_csockf_small_tcp6],
      %% Medium
      [ttest_sgsf_csockf_medium_tcp4,
       ttest_sgsf_csockf_medium_tcp6],
      %% Large
      [ttest_sgsf_csockf_large_tcp4,
       ttest_sgsf_csockf_large_tcp6]).

ttest_sgsf_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_csocko_small_tcp4,
       ttest_sgsf_csocko_small_tcp6],
      %% Medium
      [ttest_sgsf_csocko_medium_tcp4,
       ttest_sgsf_csocko_medium_tcp6],
      %% Large
      [ttest_sgsf_csocko_large_tcp4,
       ttest_sgsf_csocko_large_tcp6]).

ttest_sgsf_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgsf_csockt_small_tcp4,
       ttest_sgsf_csockt_small_tcp6],
      %% Medium
      [ttest_sgsf_csockt_medium_tcp4,
       ttest_sgsf_csockt_medium_tcp6],
      %% Large
      [ttest_sgsf_csockt_large_tcp4,
       ttest_sgsf_csockt_large_tcp6]).

%% Server: transport = gen_tcp(socket), active = once
ttest_sgso_cases() ->
    [
     {group, ttest_sgso_cgen},
     {group, ttest_sgso_cgs},
     {group, ttest_sgso_csock}
    ].

%% Server: transport = gen_tcp(socket), active = once
%% Client: transport = gen_tcp
ttest_sgso_cgen_cases() ->
    [
     {group, ttest_sgso_cgenf},
     {group, ttest_sgso_cgeno},
     {group, ttest_sgso_cgent}
    ].

ttest_sgso_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_cgenf_small_tcp4,
       ttest_sgso_cgenf_small_tcp6],
      %% Medium
      [ttest_sgso_cgenf_medium_tcp4,
       ttest_sgso_cgenf_medium_tcp6],
      %% Large
      [ttest_sgso_cgenf_large_tcp4,
       ttest_sgso_cgenf_large_tcp6]).

ttest_sgso_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_cgeno_small_tcp4,
       ttest_sgso_cgeno_small_tcp6],
      %% Medium
      [ttest_sgso_cgeno_medium_tcp4,
       ttest_sgso_cgeno_medium_tcp6],
      %% Large
      [ttest_sgso_cgeno_large_tcp4,
       ttest_sgso_cgeno_large_tcp6]).

ttest_sgso_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_cgent_small_tcp4,
       ttest_sgso_cgent_small_tcp6],
      %% Medium
      [ttest_sgso_cgent_medium_tcp4,
       ttest_sgso_cgent_medium_tcp6],
      %% Large
      [ttest_sgso_cgent_large_tcp4,
       ttest_sgso_cgent_large_tcp6]).

%% Server: transport = gen_tcp(socket), active = once
%% Client: transport = gen_tcp(socket)
ttest_sgso_cgs_cases() ->
    [
     {group, ttest_sgso_cgsf},
     {group, ttest_sgso_cgso},
     {group, ttest_sgso_cgst}
    ].

ttest_sgso_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_cgsf_small_tcp4,
       ttest_sgso_cgsf_small_tcp6],
      %% Medium
      [ttest_sgso_cgsf_medium_tcp4,
       ttest_sgso_cgsf_medium_tcp6],
      %% Large
      [ttest_sgso_cgsf_large_tcp4,
       ttest_sgso_cgsf_large_tcp6]).

ttest_sgso_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_cgso_small_tcp4,
       ttest_sgso_cgso_small_tcp6],
      %% Medium
      [ttest_sgso_cgso_medium_tcp4,
       ttest_sgso_cgso_medium_tcp6],
      %% Large
      [ttest_sgso_cgso_large_tcp4,
       ttest_sgso_cgso_large_tcp6]).

ttest_sgso_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_cgst_small_tcp4,
       ttest_sgso_cgst_small_tcp6],
      %% Medium
      [ttest_sgso_cgst_medium_tcp4,
       ttest_sgso_cgst_medium_tcp6],
      %% Large
      [ttest_sgso_cgst_large_tcp4,
       ttest_sgso_cgst_large_tcp6]).

%% Server: transport = gen_tcp(socket), active = once
%% Client: transport = socket(tcp)
ttest_sgso_csock_cases() ->
    [
     {group, ttest_sgso_csockf},
     {group, ttest_sgso_csocko},
     {group, ttest_sgso_csockt}
    ].

ttest_sgso_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_csockf_small_tcp4,
       ttest_sgso_csockf_small_tcp6],
      %% Medium
      [ttest_sgso_csockf_medium_tcp4,
       ttest_sgso_csockf_medium_tcp6],
      %% Large
      [ttest_sgso_csockf_large_tcp4,
       ttest_sgso_csockf_large_tcp6]).

ttest_sgso_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_csocko_small_tcp4,
       ttest_sgso_csocko_small_tcp6],
      %% Medium
      [ttest_sgso_csocko_medium_tcp4,
       ttest_sgso_csocko_medium_tcp6],
      %% Large
      [ttest_sgso_csocko_large_tcp4,
       ttest_sgso_csocko_large_tcp6]).

ttest_sgso_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgso_csockt_small_tcp4,
       ttest_sgso_csockt_small_tcp6],
      %% Medium
      [ttest_sgso_csockt_medium_tcp4,
       ttest_sgso_csockt_medium_tcp6],
      %% Large
      [ttest_sgso_csockt_large_tcp4,
       ttest_sgso_csockt_large_tcp6]).

%% Server: transport = gen_tcp(socket), active = true
ttest_sgst_cases() ->
    [
     {group, ttest_sgst_cgen},
     {group, ttest_sgst_cgs},
     {group, ttest_sgst_csock}
    ].

%% Server: transport = gen_tcp(socket), active = true
%% Client: transport = gen_tcp
ttest_sgst_cgen_cases() ->
    [
     {group, ttest_sgst_cgenf},
     {group, ttest_sgst_cgeno},
     {group, ttest_sgst_cgent}
    ].

ttest_sgst_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_cgenf_small_tcp4,
       ttest_sgst_cgenf_small_tcp6],
      %% Medium
      [ttest_sgst_cgenf_medium_tcp4,
       ttest_sgst_cgenf_medium_tcp6],
      %% Large
      [ttest_sgst_cgenf_large_tcp4,
       ttest_sgst_cgenf_large_tcp6]).

ttest_sgst_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_cgeno_small_tcp4,
       ttest_sgst_cgeno_small_tcp6],
      %% Medium
      [ttest_sgst_cgeno_medium_tcp4,
       ttest_sgst_cgeno_medium_tcp6],
      %% Large
      [ttest_sgst_cgeno_large_tcp4,
       ttest_sgst_cgeno_large_tcp6]).

ttest_sgst_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_cgent_small_tcp4,
       ttest_sgst_cgent_small_tcp6],
      %% Medium
      [ttest_sgst_cgent_medium_tcp4,
       ttest_sgst_cgent_medium_tcp6],
      %% Large
      [ttest_sgst_cgent_large_tcp4,
       ttest_sgst_cgent_large_tcp6]).


%% Server: transport = gen_tcp(socket), active = true
%% Client: transport = gen_tcp(socket)
ttest_sgst_cgs_cases() ->
    [
     {group, ttest_sgst_cgsf},
     {group, ttest_sgst_cgso},
     {group, ttest_sgst_cgst}
    ].

ttest_sgst_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_cgsf_small_tcp4,
       ttest_sgst_cgsf_small_tcp6],
      %% Medium
      [ttest_sgst_cgsf_medium_tcp4,
       ttest_sgst_cgsf_medium_tcp6],
      %% Large
      [ttest_sgst_cgsf_large_tcp4,
       ttest_sgst_cgsf_large_tcp6]).

ttest_sgst_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_cgso_small_tcp4,
       ttest_sgst_cgso_small_tcp6],
      %% Medium
      [ttest_sgst_cgso_medium_tcp4,
       ttest_sgst_cgso_medium_tcp6],
      %% Large
      [ttest_sgst_cgso_large_tcp4,
       ttest_sgst_cgso_large_tcp6]).

ttest_sgst_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_cgst_small_tcp4,
       ttest_sgst_cgst_small_tcp6],
      %% Medium
      [ttest_sgst_cgst_medium_tcp4,
       ttest_sgst_cgst_medium_tcp6],
      %% Large
      [ttest_sgst_cgst_large_tcp4,
       ttest_sgst_cgst_large_tcp6]).


%% Server: transport = gen_tcp(socket), active = true
%% Client: transport = socket(tcp)
ttest_sgst_csock_cases() ->
    [
     {group, ttest_sgst_csockf},
     {group, ttest_sgst_csocko},
     {group, ttest_sgst_csockt}
    ].

ttest_sgst_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_csockf_small_tcp4,
       ttest_sgst_csockf_small_tcp6],
      %% Medium
      [ttest_sgst_csockf_medium_tcp4,
       ttest_sgst_csockf_medium_tcp6],
      %% Large
      [ttest_sgst_csockf_large_tcp4,
       ttest_sgst_csockf_large_tcp6]).

ttest_sgst_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_csocko_small_tcp4,
       ttest_sgst_csocko_small_tcp6],
      %% Medium
      [ttest_sgst_csocko_medium_tcp4,
       ttest_sgst_csocko_medium_tcp6],
      %% Large
      [ttest_sgst_csocko_large_tcp4,
       ttest_sgst_csocko_large_tcp6]).

ttest_sgst_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgst_csockt_small_tcp4,
       ttest_sgst_csockt_small_tcp6],
      %% Medium
      [ttest_sgst_csockt_medium_tcp4,
       ttest_sgst_csockt_medium_tcp6],
      %% Large
      [ttest_sgst_csockt_large_tcp4,
       ttest_sgst_csockt_large_tcp6]).


%% Server: transport = gen_tcp, active = once
ttest_sgeno_cases() ->
    [
     {group, ttest_sgeno_cgen},
     {group, ttest_sgeno_cgs},
     {group, ttest_sgeno_csock}
    ].

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp
ttest_sgeno_cgen_cases() ->
    [
     {group, ttest_sgeno_cgenf},
     {group, ttest_sgeno_cgeno},
     {group, ttest_sgeno_cgent}
    ].

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp, active = false
ttest_sgeno_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_cgenf_small_tcp4,
       ttest_sgeno_cgenf_small_tcp6],
      %% Medium
      [ttest_sgeno_cgenf_medium_tcp4,
       ttest_sgeno_cgenf_medium_tcp6],
      %% Large
      [ttest_sgeno_cgenf_large_tcp4,
       ttest_sgeno_cgenf_large_tcp6]).

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp, active = once
ttest_sgeno_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_cgeno_small_tcp4,
       ttest_sgeno_cgeno_small_tcp6],
      %% Medium
      [ttest_sgeno_cgeno_medium_tcp4,
       ttest_sgeno_cgeno_medium_tcp6],
      %% Large
      [ttest_sgeno_cgeno_large_tcp4,
       ttest_sgeno_cgeno_large_tcp6]).

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp, active = true
ttest_sgeno_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_cgent_small_tcp4,
       ttest_sgeno_cgent_small_tcp6],
      %% Medium
      [ttest_sgeno_cgent_medium_tcp4,
       ttest_sgeno_cgent_medium_tcp6],
      %% Large
      [ttest_sgeno_cgent_large_tcp4,
       ttest_sgeno_cgent_large_tcp6]).

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp(socket)(
ttest_sgeno_cgs_cases() ->
    [
     {group, ttest_sgeno_cgsf},
     {group, ttest_sgeno_cgso},
     {group, ttest_sgeno_cgst}
    ].

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp(socket), active = false
ttest_sgeno_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_cgsf_small_tcp4,
       ttest_sgeno_cgsf_small_tcp6],
      %% Medium
      [ttest_sgeno_cgsf_medium_tcp4,
       ttest_sgeno_cgsf_medium_tcp6],
      %% Large
      [ttest_sgeno_cgsf_large_tcp4,
       ttest_sgeno_cgsf_large_tcp6]).

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp(socket), active = once
ttest_sgeno_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_cgso_small_tcp4,
       ttest_sgeno_cgso_small_tcp6],
      %% Medium
      [ttest_sgeno_cgso_medium_tcp4,
       ttest_sgeno_cgso_medium_tcp6],
      %% Large
      [ttest_sgeno_cgso_large_tcp4,
       ttest_sgeno_cgso_large_tcp6]).

%% Server: transport = gen_tcp, active = once
%% Client: transport = gen_tcp(socket), active = true
ttest_sgeno_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_cgst_small_tcp4,
       ttest_sgeno_cgst_small_tcp6],
      %% Medium
      [ttest_sgeno_cgst_medium_tcp4,
       ttest_sgeno_cgst_medium_tcp6],
      %% Large
      [ttest_sgeno_cgst_large_tcp4,
       ttest_sgeno_cgst_large_tcp6]).

%% Server: transport = gen_tcp, active = once
%% Client: transport = socket(tcp)
ttest_sgeno_csock_cases() ->
    [
     {group, ttest_sgeno_csockf},
     {group, ttest_sgeno_csocko},
     {group, ttest_sgeno_csockt}
    ].

ttest_sgeno_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_csockf_small_tcp4,
       ttest_sgeno_csockf_small_tcp6],
      %% Medium
      [ttest_sgeno_csockf_medium_tcp4,
       ttest_sgeno_csockf_medium_tcp6],
      %% Large
      [ttest_sgeno_csockf_large_tcp4,
       ttest_sgeno_csockf_large_tcp6]).

ttest_sgeno_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_csocko_small_tcp4,
       ttest_sgeno_csocko_small_tcp6],
      %% Medium
      [ttest_sgeno_csocko_medium_tcp4,
       ttest_sgeno_csocko_medium_tcp6],
      %% Large
      [ttest_sgeno_csocko_large_tcp4,
       ttest_sgeno_csocko_large_tcp6]).

ttest_sgeno_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgeno_csockt_small_tcp4,
       ttest_sgeno_csockt_small_tcp6],
      %% Medium
      [ttest_sgeno_csockt_medium_tcp4,
       ttest_sgeno_csockt_medium_tcp6],
      %% Large
      [ttest_sgeno_csockt_large_tcp4,
       ttest_sgeno_csockt_large_tcp6]).

%% Server: transport = gen_tcp, active = true
ttest_sgent_cases() ->
    [
     {group, ttest_sgent_cgen},
     {group, ttest_sgent_cgs},
     {group, ttest_sgent_csock}
    ].

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp
ttest_sgent_cgen_cases() ->
    [
     {group, ttest_sgent_cgenf},
     {group, ttest_sgent_cgeno},
     {group, ttest_sgent_cgent}
    ].

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp, active = false
ttest_sgent_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_cgenf_small_tcp4,
       ttest_sgent_cgenf_small_tcp6],
      %% Medium
      [ttest_sgent_cgenf_medium_tcp4,
       ttest_sgent_cgenf_medium_tcp6],
      %% Large
      [ttest_sgent_cgenf_large_tcp4,
       ttest_sgent_cgenf_large_tcp6]).

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp, active = once
ttest_sgent_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_cgeno_small_tcp4,
       ttest_sgent_cgeno_small_tcp6],
      %% Medium
      [ttest_sgent_cgeno_medium_tcp4,
       ttest_sgent_cgeno_medium_tcp6],
      %% Large
      [ttest_sgent_cgeno_large_tcp4,
       ttest_sgent_cgeno_large_tcp6]).

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp, active = true
ttest_sgent_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_cgent_small_tcp4,
       ttest_sgent_cgent_small_tcp6],
      %% Medium
      [ttest_sgent_cgent_medium_tcp4,
       ttest_sgent_cgent_medium_tcp6],
      %% Large
      [ttest_sgent_cgent_large_tcp4,
       ttest_sgent_cgent_large_tcp6]).

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp(socket)
ttest_sgent_cgs_cases() ->
    [
     {group, ttest_sgent_cgsf},
     {group, ttest_sgent_cgso},
     {group, ttest_sgent_cgst}
    ].

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp(socket), active = false
ttest_sgent_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_cgsf_small_tcp4,
       ttest_sgent_cgsf_small_tcp6],
      %% Medium
      [ttest_sgent_cgsf_medium_tcp4,
       ttest_sgent_cgsf_medium_tcp6],
      %% Large
      [ttest_sgent_cgsf_large_tcp4,
       ttest_sgent_cgsf_large_tcp6]).

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp(socket), active = once
ttest_sgent_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_cgso_small_tcp4,
       ttest_sgent_cgso_small_tcp6],
      %% Medium
      [ttest_sgent_cgso_medium_tcp4,
       ttest_sgent_cgso_medium_tcp6],
      %% Large
      [ttest_sgent_cgso_large_tcp4,
       ttest_sgent_cgso_large_tcp6]).

%% Server: transport = gen_tcp, active = true
%% Client: transport = gen_tcp(socket), active = true
ttest_sgent_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_cgst_small_tcp4,
       ttest_sgent_cgst_small_tcp6],
      %% Medium
      [ttest_sgent_cgst_medium_tcp4,
       ttest_sgent_cgst_medium_tcp6],
      %% Large
      [ttest_sgent_cgst_large_tcp4,
       ttest_sgent_cgst_large_tcp6]).

%% Server: transport = gen_tcp, active = true
%% Client: transport = socket(tcp)
ttest_sgent_csock_cases() ->
    [
     {group, ttest_sgent_csockf},
     {group, ttest_sgent_csocko},
     {group, ttest_sgent_csockt}
    ].

ttest_sgent_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_csockf_small_tcp4,
       ttest_sgent_csockf_small_tcp6],
      %% Medium
      [ttest_sgent_csockf_medium_tcp4,
       ttest_sgent_csockf_medium_tcp6],
      %% Large
      [ttest_sgent_csockf_large_tcp4,
       ttest_sgent_csockf_large_tcp6]).

ttest_sgent_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_csocko_small_tcp4,
       ttest_sgent_csocko_small_tcp6],
      %% Medium
      [ttest_sgent_csocko_medium_tcp4,
       ttest_sgent_csocko_medium_tcp6],
      %% Large
      [ttest_sgent_csocko_large_tcp4,
       ttest_sgent_csocko_large_tcp6]).

ttest_sgent_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_sgent_csockt_small_tcp4,
       ttest_sgent_csockt_small_tcp6],
      %% Medium
      [ttest_sgent_csockt_medium_tcp4,
       ttest_sgent_csockt_medium_tcp6],
      %% Large
      [ttest_sgent_csockt_large_tcp4,
       ttest_sgent_csockt_large_tcp6]).

%% Server: transport = socket(tcp), active = false
ttest_ssockf_cases() ->
    [
     {group, ttest_ssockf_cgen},
     {group, ttest_ssockf_cgs},
     {group, ttest_ssockf_csock}
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp
ttest_ssockf_cgen_cases() ->
    [
     {group, ttest_ssockf_cgenf},
     {group, ttest_ssockf_cgeno},
     {group, ttest_ssockf_cgent}
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp, active = false
ttest_ssockf_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_cgenf_small_tcp4,
       ttest_ssockf_cgenf_small_tcp6],
      %% Medium
      [ttest_ssockf_cgenf_medium_tcp4,
       ttest_ssockf_cgenf_medium_tcp6],
      %% Large
      [ttest_ssockf_cgenf_large_tcp4,
       ttest_ssockf_cgenf_large_tcp6]).

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp, active = once
ttest_ssockf_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_cgeno_small_tcp4,
       ttest_ssockf_cgeno_small_tcp6],
      %% Medium
      [ttest_ssockf_cgeno_medium_tcp4,
       ttest_ssockf_cgeno_medium_tcp6],
      %% Large
      [ttest_ssockf_cgeno_large_tcp4,
       ttest_ssockf_cgeno_large_tcp6]).

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp, active = true
ttest_ssockf_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_cgent_small_tcp4,
       ttest_ssockf_cgent_small_tcp6],
      %% Medium
      [ttest_ssockf_cgent_medium_tcp4,
       ttest_ssockf_cgent_medium_tcp6],
      %% Large
      [ttest_ssockf_cgent_large_tcp4,
       ttest_ssockf_cgent_large_tcp6]).

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp(socket)
ttest_ssockf_cgs_cases() ->
    [
     {group, ttest_ssockf_cgsf},
     {group, ttest_ssockf_cgso},
     {group, ttest_ssockf_cgst}
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp(socket), active = false
ttest_ssockf_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_cgsf_small_tcp4,
       ttest_ssockf_cgsf_small_tcp6],
      %% Medium
      [ttest_ssockf_cgsf_medium_tcp4,
       ttest_ssockf_cgsf_medium_tcp6],
      %% Large
      [ttest_ssockf_cgsf_large_tcp4,
       ttest_ssockf_cgsf_large_tcp6]).

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp(socket), active = once
ttest_ssockf_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_cgso_small_tcp4,
       ttest_ssockf_cgso_small_tcp6],
      %% Medium
      [ttest_ssockf_cgso_medium_tcp4,
       ttest_ssockf_cgso_medium_tcp6],
      %% Large
      [ttest_ssockf_cgso_large_tcp4,
       ttest_ssockf_cgso_large_tcp6]).

%% Server: transport = socket(tcp), active = false
%% Client: transport = gen_tcp(socket), active = true
ttest_ssockf_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_cgst_small_tcp4,
       ttest_ssockf_cgst_small_tcp6],
      %% Medium
      [ttest_ssockf_cgst_medium_tcp4,
       ttest_ssockf_cgst_medium_tcp6],
      %% Large
      [ttest_ssockf_cgst_large_tcp4,
       ttest_ssockf_cgst_large_tcp6]).

%% Server: transport = socket(tcp), active = false
%% Client: transport = socket(tcp)
ttest_ssockf_csock_cases() ->
    [
     {group, ttest_ssockf_csockf},
     {group, ttest_ssockf_csocko},
     {group, ttest_ssockf_csockt}
    ].

%% Server: transport = socket(tcp), active = false
%% Client: transport = socket(tcp), active = false
ttest_ssockf_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_csockf_small_tcp4,
       ttest_ssockf_csockf_small_tcp6,
       ttest_ssockf_csockf_small_tcpL],
      %% Medium
      [ttest_ssockf_csockf_medium_tcp4,
       ttest_ssockf_csockf_medium_tcp6,
       ttest_ssockf_csockf_medium_tcpL],
      %% Large
      [ttest_ssockf_csockf_large_tcp4,
       ttest_ssockf_csockf_large_tcp6,
       ttest_ssockf_csockf_large_tcpL]).

%% Server: transport = socket(tcp), active = false
%% Client: transport = socket(tcp), active = once
ttest_ssockf_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_csocko_small_tcp4,
       ttest_ssockf_csocko_small_tcp6,
       ttest_ssockf_csocko_small_tcpL],
      %% Medium
      [ttest_ssockf_csocko_medium_tcp4,
       ttest_ssockf_csocko_medium_tcp6,
       ttest_ssockf_csocko_medium_tcpL],
      %% Large
      [ttest_ssockf_csocko_large_tcp4,
       ttest_ssockf_csocko_large_tcp6,
       ttest_ssockf_csocko_large_tcpL]).

%% Server: transport = socket(tcp), active = false
%% Client: transport = socket(tcp), active = true
ttest_ssockf_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockf_csockt_small_tcp4,
       ttest_ssockf_csockt_small_tcp6,
       ttest_ssockf_csockt_small_tcpL],
      %% Medium
      [ttest_ssockf_csockt_medium_tcp4,
       ttest_ssockf_csockt_medium_tcp6,
       ttest_ssockf_csockt_medium_tcpL],
      %% Large
      [ttest_ssockf_csockt_large_tcp4,
       ttest_ssockf_csockt_large_tcp6,
       ttest_ssockf_csockt_large_tcpL]).

%% Server: transport = socket(tcp), active = once
ttest_ssocko_cases() ->
    [
     {group, ttest_ssocko_cgen},
     {group, ttest_ssocko_cgs},
     {group, ttest_ssocko_csock}
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp
ttest_ssocko_cgen_cases() ->
    [
     {group, ttest_ssocko_cgenf},
     {group, ttest_ssocko_cgeno},
     {group, ttest_ssocko_cgent}
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp, active = false
ttest_ssocko_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_cgenf_small_tcp4,
       ttest_ssocko_cgenf_small_tcp6],
      %% Medium
      [ttest_ssocko_cgenf_medium_tcp4,
       ttest_ssocko_cgenf_medium_tcp6],
      %% Large
      [ttest_ssocko_cgenf_large_tcp4,
       ttest_ssocko_cgenf_large_tcp6]).

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp, active = once
ttest_ssocko_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_cgeno_small_tcp4,
       ttest_ssocko_cgeno_small_tcp6],
      %% Medium
      [ttest_ssocko_cgeno_medium_tcp4,
       ttest_ssocko_cgeno_medium_tcp6],
      %% Large
      [ttest_ssocko_cgeno_large_tcp4,
       ttest_ssocko_cgeno_large_tcp6]).

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp, active = true
ttest_ssocko_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_cgent_small_tcp4,
       ttest_ssocko_cgent_small_tcp6],
      %% Medium
      [ttest_ssocko_cgent_medium_tcp4,
       ttest_ssocko_cgent_medium_tcp6],
      %% Large
      [ttest_ssocko_cgent_large_tcp4,
       ttest_ssocko_cgent_large_tcp6]).

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp(socket)
ttest_ssocko_cgs_cases() ->
    [
     {group, ttest_ssocko_cgsf},
     {group, ttest_ssocko_cgso},
     {group, ttest_ssocko_cgst}
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp(socket), active = false
ttest_ssocko_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_cgsf_small_tcp4,
       ttest_ssocko_cgsf_small_tcp6],
      %% Medium
      [ttest_ssocko_cgsf_medium_tcp4,
       ttest_ssocko_cgsf_medium_tcp6],
      %% Large
      [ttest_ssocko_cgsf_large_tcp4,
       ttest_ssocko_cgsf_large_tcp6]).

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp(socket), active = once
ttest_ssocko_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_cgso_small_tcp4,
       ttest_ssocko_cgso_small_tcp6],
      %% Medium
      [ttest_ssocko_cgso_medium_tcp4,
       ttest_ssocko_cgso_medium_tcp6],
      %% Large
      [ttest_ssocko_cgso_large_tcp4,
       ttest_ssocko_cgso_large_tcp6]).

%% Server: transport = socket(tcp), active = once
%% Client: transport = gen_tcp(socket), active = true
ttest_ssocko_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_cgst_small_tcp4,
       ttest_ssocko_cgst_small_tcp6],
      %% Medium
      [ttest_ssocko_cgst_medium_tcp4,
       ttest_ssocko_cgst_medium_tcp6],
      %% Large
      [ttest_ssocko_cgst_large_tcp4,
       ttest_ssocko_cgst_large_tcp6]).

%% Server: transport = socket(tcp), active = once
%% Client: transport = socket(tcp)
ttest_ssocko_csock_cases() ->
    [
     {group, ttest_ssocko_csockf},
     {group, ttest_ssocko_csocko},
     {group, ttest_ssocko_csockt}
    ].

%% Server: transport = socket(tcp), active = once
%% Client: transport = socket(tcp), active = false
ttest_ssocko_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_csockf_small_tcp4,
       ttest_ssocko_csockf_small_tcp6,
       ttest_ssocko_csockf_small_tcpL],
     %% Medium
      [ttest_ssocko_csockf_medium_tcp4,
       ttest_ssocko_csockf_medium_tcp6,
       ttest_ssocko_csockf_medium_tcpL],
      %% Large
      [ttest_ssocko_csockf_large_tcp4,
       ttest_ssocko_csockf_large_tcp6,
       ttest_ssocko_csockf_large_tcpL]).

%% Server: transport = socket(tcp), active = once
%% Client: transport = socket(tcp), active = once
ttest_ssocko_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_csocko_small_tcp4,
       ttest_ssocko_csocko_small_tcp6,
       ttest_ssocko_csocko_small_tcpL],
      %% Medium
      [ttest_ssocko_csocko_medium_tcp4,
       ttest_ssocko_csocko_medium_tcp6,
       ttest_ssocko_csocko_medium_tcpL],
      %% Large
      [ttest_ssocko_csocko_large_tcp4,
       ttest_ssocko_csocko_large_tcp6,
       ttest_ssocko_csocko_large_tcpL]).

%% Server: transport = socket(tcp), active = once
%% Client: transport = socket(tcp), active = true
ttest_ssocko_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssocko_csockt_small_tcp4,
       ttest_ssocko_csockt_small_tcp6,
       ttest_ssocko_csockt_small_tcpL],
      %% Medium
      [ttest_ssocko_csockt_medium_tcp4,
       ttest_ssocko_csockt_medium_tcp6,
       ttest_ssocko_csockt_medium_tcpL],
      %% Large
      [ttest_ssocko_csockt_large_tcp4,
       ttest_ssocko_csockt_large_tcp6,
       ttest_ssocko_csockt_large_tcpL]).

%% Server: transport = socket(tcp), active = true
ttest_ssockt_cases() ->
    [
     {group, ttest_ssockt_cgen},
     {group, ttest_ssockt_cgs},
     {group, ttest_ssockt_csock}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp
ttest_ssockt_cgen_cases() ->
    [
     {group, ttest_ssockt_cgenf},
     {group, ttest_ssockt_cgeno},
     {group, ttest_ssockt_cgent}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp, active = false
ttest_ssockt_cgenf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_cgenf_small_tcp4,
       ttest_ssockt_cgenf_small_tcp6],
      %% Medium
      [ttest_ssockt_cgenf_medium_tcp4,
       ttest_ssockt_cgenf_medium_tcp6],
      %% Large
      [ttest_ssockt_cgenf_large_tcp4,
       ttest_ssockt_cgenf_large_tcp6]).

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp, active = once
ttest_ssockt_cgeno_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_cgeno_small_tcp4,
       ttest_ssockt_cgeno_small_tcp6],
      %% Medium
      [ttest_ssockt_cgeno_medium_tcp4,
       ttest_ssockt_cgeno_medium_tcp6],
      %% Large
      [ttest_ssockt_cgeno_large_tcp4,
       ttest_ssockt_cgeno_large_tcp6]).

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp, active = true
ttest_ssockt_cgent_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_cgent_small_tcp4,
       ttest_ssockt_cgent_small_tcp6],
      %% Medium
      [ttest_ssockt_cgent_medium_tcp4,
       ttest_ssockt_cgent_medium_tcp6],
      %% Large
      [ttest_ssockt_cgent_large_tcp4,
       ttest_ssockt_cgent_large_tcp6]).

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp(socket)
ttest_ssockt_cgs_cases() ->
    [
     {group, ttest_ssockt_cgsf},
     {group, ttest_ssockt_cgso},
     {group, ttest_ssockt_cgst}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp(socket), active = false
ttest_ssockt_cgsf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_cgsf_small_tcp4,
       ttest_ssockt_cgsf_small_tcp6],
      %% Medium
      [ttest_ssockt_cgsf_medium_tcp4,
       ttest_ssockt_cgsf_medium_tcp6],
      %% Large
      [ttest_ssockt_cgsf_large_tcp4,
       ttest_ssockt_cgsf_large_tcp6]).

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp(socket), active = once
ttest_ssockt_cgso_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_cgso_small_tcp4,
       ttest_ssockt_cgso_small_tcp6],
      %% Medium
      [ttest_ssockt_cgso_medium_tcp4,
       ttest_ssockt_cgso_medium_tcp6],
      %% Large
      [ttest_ssockt_cgso_large_tcp4,
       ttest_ssockt_cgso_large_tcp6]).

%% Server: transport = socket(tcp), active = true
%% Client: transport = gen_tcp(socket), active = true
ttest_ssockt_cgst_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_cgst_small_tcp4,
       ttest_ssockt_cgst_small_tcp6],
      %% Medium
      [ttest_ssockt_cgst_medium_tcp4,
       ttest_ssockt_cgst_medium_tcp6],
      %% Large
      [ttest_ssockt_cgst_large_tcp4,
       ttest_ssockt_cgst_large_tcp6]).

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp)
ttest_ssockt_csock_cases() ->
    [
     {group, ttest_ssockt_csockf},
     {group, ttest_ssockt_csocko},
     {group, ttest_ssockt_csockt}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp), active = false
ttest_ssockt_csockf_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_csockf_small_tcp4,
       ttest_ssockt_csockf_small_tcp6,
       ttest_ssockt_csockf_small_tcpL],
      %% Medium
      [ttest_ssockt_csockf_medium_tcp4,
       ttest_ssockt_csockf_medium_tcp6,
       ttest_ssockt_csockf_medium_tcpL],
      %% Large
      [ttest_ssockt_csockf_large_tcp4,
       ttest_ssockt_csockf_large_tcp6,
       ttest_ssockt_csockf_large_tcpL]).

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp), active = once
ttest_ssockt_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_csocko_small_tcp4,
       ttest_ssockt_csocko_small_tcp6,
       ttest_ssockt_csocko_small_tcpL],
      %% Medium
      [ttest_ssockt_csocko_medium_tcp4,
       ttest_ssockt_csocko_medium_tcp6,
       ttest_ssockt_csocko_medium_tcpL],
      %% Large
      [ttest_ssockt_csocko_large_tcp4,
       ttest_ssockt_csocko_large_tcp6,
       ttest_ssockt_csocko_large_tcpL]).

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp), active = true
ttest_ssockt_csockt_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_ssockt_csockt_small_tcp4,
       ttest_ssockt_csockt_small_tcp6,
       ttest_ssockt_csockt_small_tcpL],
      %% Medium
      [ttest_ssockt_csockt_medium_tcp4,
       ttest_ssockt_csockt_medium_tcp6,
       ttest_ssockt_csockt_medium_tcpL],
      %% Large
      [ttest_ssockt_csockt_large_tcp4,
       ttest_ssockt_csockt_large_tcp6,
       ttest_ssockt_csockt_large_tcpL]).

%% Server: transport = socket(tcp), active = true
ttest_simple_ssockt_cases() ->
    [
     {group, ttest_simple_ssockt_csock}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp)
ttest_simple_ssockt_csock_cases() ->
    [
     %% {group, ttest_simple_ssockt_csockf},
     {group, ttest_simple_ssockt_csocko}%% ,
     %% {group, ttest_simple_ssockt_csockt}
    ].

%% Server: transport = socket(tcp), active = true
%% Client: transport = socket(tcp), active = once
ttest_simple_ssockt_csocko_cases() ->
    ttest_select_conditional_cases(
      %% Small
      [ttest_simple_ssockt_csocko_small_tcp4,
       ttest_simple_ssockt_csocko_small_tcp6,
       ttest_simple_ssockt_csocko_small_tcpL],
      %% Medium
      [],
      %% Large
      []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config0) ->
    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),
    
    try socket:info() of
        #{} ->
            case ?KLIB:init_per_suite(Config0) of
                {skip, _} = SKIP ->
                    SKIP;

                Config1 when is_list(Config1) ->

                    ?P("init_per_suite -> end when "
                       "~n      Config: ~p", [Config1]),

                    %% We need a monitor on this node also
                    kernel_test_sys_monitor:start(),

                    socket:use_registry(false),
                    case quiet_mode(Config1) of
                        default ->
                            case ?LOGGER:start() of
                                ok ->
                                    Config1;
                                {error, Reason} ->
                                    ?P("init_per_suite -> "
                                       "Failed starting logger"
                                       "~n   Reason: ~p"
                                       "~n", [Reason]),
                                    {skip, "Failed starting logger"}
                            end;
                        Quiet ->
                            case ?LOGGER:start(Quiet) of
                                ok ->
                                    [{esock_test_quiet, Quiet} | Config1];
                                {error, Reason} ->
                                    ?P("init_per_suite -> "
                                       "Failed starting logger"
                                       "~n   Reason: ~p"
                                       "~n", [Reason]),
                                    {skip, "Failed starting logger"}
                            end
                    end
            end
    catch
        error : notsup ->
            {skip, "esock not supported"};
        error : undef ->
            {skip, "esock not configured"}
    end.

end_per_suite(Config0) ->

    ?P("end_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    %% Stop the local monitor
    kernel_test_sys_monitor:stop(),

    (catch ?LOGGER:stop()),

    Config1 = ?KLIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
       "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.


init_per_group(standard = GroupName, Config) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [GroupName, Config]),
    [{category, GroupName} | Config];
init_per_group(bench = GroupName, Config) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [GroupName, Config]),
    [{category, GroupName} | Config];
init_per_group(ttest = _GroupName, Config) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_GroupName, Config]),
    case ttest_condition(Config) of
        ok ->
            Category = ?config(category, Config),
            ttest_manager_start(Category),
            case lists:keysearch(esock_test_ttest_runtime, 1, Config) of
                {value, _} ->
                    Config;
                false ->
                    [{esock_test_ttest_runtime, which_ttest_runtime_env()} |
                     Config]
            end;
        {skip, _} = SKIP ->
            SKIP
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(ttest = _GroupName, Config) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_GroupName, Config]),
    ttest_manager_stop(),
    lists:keydelete(esock_test_ttest_runtime, 1, Config);
end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_TC, Config) ->
    io:format("init_per_testcase(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_TC, Config]),
    Config.

end_per_testcase(_TC, Config) ->
    Config.


quiet_mode(Config) ->
    case lists:keysearch(esock_test_quiet, 1, Config) of
        {value, {esock_test_quiet, Quiet}} ->
            Quiet;
        false ->
            case os:getenv("ESOCK_TEST_QUIET") of
                "true"  -> true;
                "false" -> false;
                _       -> default
            end
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                           TIME TEST                                 %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgenf_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgenf_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgenf_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, false,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgenf_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, false,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgenf_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgenf_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgeno_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgeno_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgeno_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, once,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgeno_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, once,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgeno_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgeno_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, once,
                     sock, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, true,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, true,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, true,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, true,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgent_medium_tcp4() ->
    [{doc, "Server(gen,true), Client(gen,true), Domain=inet, msg=medium"}].

ttest_sgent_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, true,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 
ttest_sgent_cgent_medium_tcp6() ->
    [{doc, "Server(gen,true), Client(gen,true), Domain=inet6, msg=medium"}].

ttest_sgent_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, true,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgent_large_tcp4() ->
    [{doc, "Server(gen,true), Client(gen,true), Domain=inet, msg=large"}].

ttest_sgent_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgent_large_tcp6() ->
    [{doc, "Server(gen,true), Client(gen,true), Domain=inet6, msg=large"}].

ttest_sgent_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     gen, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, true,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, true,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, true,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, true,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, true,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, true,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     gs, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%


ttest_sgent_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, true,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, true,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gen, true,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gen, true,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgent_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gen, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgent_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gen, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgent_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                     inet,
                     gen, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgent_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                     inet6,
                     gen, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgent_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gen, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp, Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgent_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gen, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgsf_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgsf_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgsf_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, false,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgsf_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, false,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgsf_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgsf_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, false,
                     sock, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     gen, true).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     gs, true).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgso_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgso_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgso_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, once,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgso_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, once,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgso_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgso_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, once,
                     sock, true).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     gen, true).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     gs, true).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_sgst_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     gs, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_sgst_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     gs, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_sgst_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      gs, true,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_sgst_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      gs, true,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_sgst_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     gs, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = gen_tcp(socket), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_sgst_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     gs, true,
                     sock, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, false,
                     sock, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssockf_csockf_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     local,
                     sock, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssockf_csockf_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, false,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssockf_csockf_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, false,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssockf_csocko_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     local,
                     sock, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssockf_csocko_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, false,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssockf_csocko_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, false,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockf_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockf_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssockf_csockt_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockf_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, false,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockf_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, false,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssockf_csockt_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, false,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockf_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockf_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, false,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssockf_csockt_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, false,
                     sock, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     gen, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = false
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     gs, true).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssocko_csockf_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     local,
                     sock, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssocko_csockf_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, once,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssocko_csockf_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, once,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssocko_csocko_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     local,
                     sock, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssocko_csocko_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, once,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssocko_csocko_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, once,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssocko_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssocko_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssocko_csockt_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     local,
                     sock, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssocko_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, once,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssocko_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, once,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssocko_csockt_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, once,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssocko_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssocko_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, once,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = once
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssocko_csockt_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, once,
                     sock, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgenf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgenf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgenf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgenf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgenf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgenf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     gen, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgeno_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgeno_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgeno_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgeno_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgeno_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgeno_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     gen, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgent_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgent_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgent_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgent_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgent_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     gen, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp, Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgent_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     gen, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgsf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgsf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgsf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgsf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgsf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgsf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     gs, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgso_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgso_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgso_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgso_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgso_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgso_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     gs, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_cgst_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_cgst_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_cgst_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_cgst_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_cgst_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     gs, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = gen_tcp(socket), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_cgst_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     gs, true).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_csockf_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_csockf_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssockt_csockf_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     local,
                     sock, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_csockf_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_csockf_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssockt_csockf_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, true,
                      sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_csockf_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_csockf_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = false
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssockt_csockf_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, true,
                     sock, false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_csocko_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_csocko_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssockt_csocko_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     local,
                     sock, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_csocko_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_csocko_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssockt_csocko_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, true,
                      sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_csocko_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_csocko_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssockt_csocko_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, true,
                     sock, once).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet
%%

ttest_ssockt_csockt_small_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet,
                     sock, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_ssockt_csockt_small_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     inet6,
                     sock, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport =  socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_ssockt_csockt_small_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_SMALL(Config,
                     local,
                     sock, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet
%%

ttest_ssockt_csockt_medium_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet,
                      sock, true,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       inet6
%% 

ttest_ssockt_csockt_medium_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      inet6,
                      sock, true,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: medium (=2)
%% Domain:       local
%% 

ttest_ssockt_csockt_medium_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_MEDIUM(Config,
                      local,
                      sock, true,
                      sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet
%%

ttest_ssockt_csockt_large_tcp4(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet,
                     sock, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       inet6
%% 

ttest_ssockt_csockt_large_tcp6(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     inet6,
                     sock, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = true
%% Message Size: large (=3)
%% Domain:       local
%% 

ttest_ssockt_csockt_large_tcpL(Config) when is_list(Config) ->
    ?TTEST_TCP_LARGE(Config,
                     local,
                     sock, true,
                     sock, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet
%% Remote:       false (run everything on the local node)
%%

ttest_simple_ssockt_csocko_small_tcp4(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(?FUNCTION_NAME,
              Runtime,
              inet,
              sock, true,
              sock, once,
              1, ttest_small_max_outstanding(Config),
	      false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       inet6
%% 

ttest_simple_ssockt_csocko_small_tcp6(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(?FUNCTION_NAME,
              Runtime,
              inet6,
              sock, true,
              sock, once,
              1, ttest_small_max_outstanding(Config),
	      false).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case uses the time test (ttest) utility to implement a 
%% ping-pong like test case.
%% Server:       Transport = socket(tcp), Active = true
%% Client:       Transport = socket(tcp), Active = once
%% Message Size: small (=1)
%% Domain:       local
%% 

ttest_simple_ssockt_csocko_small_tcpL(Config) when is_list(Config) ->
    Runtime = which_ttest_runtime(Config),
    ttest_tcp(?FUNCTION_NAME,
              Runtime,
              local,
              sock, true,
              sock, once,
              1, ttest_small_max_outstanding(Config),
	      false).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

which_ttest_runtime(Config) when is_list(Config) ->
    case proplists:get_value(category, Config, standard) of
        standard ->
            proplists:get_value(esock_test_ttest_runtime,
                                Config, which_ttest_runtime_env());
        bench ->
            %% We always run a certain time for benchmark runs
            ?TTEST_BENCH_RUNTIME
    end.

which_ttest_runtime_env() ->
    which_ttest_runtime_env(os:getenv("ESOCK_TEST_TTEST_RUNTIME")).

which_ttest_runtime_env(TStr) when is_list(TStr) ->
    which_ttest_runtime_env2(lists:reverse(TStr));
which_ttest_runtime_env(false) ->
    ?TTEST_STANDARD_RUNTIME.


%% The format is: <int>[unit]
%% where the optional unit can be:
%% ms: milliseconds
%% s:  seconds (default)
%% m:  minutes
which_ttest_runtime_env2([$s, $m | MS]) when (length(MS) > 0) ->
    convert_time(MS, fun(X) -> X end);
which_ttest_runtime_env2([$m | M]) when (length(M) > 0) ->
    convert_time(M, fun(X) -> ?MINS(X) end);
which_ttest_runtime_env2([$s | S]) when (length(S) > 0) ->
    convert_time(S, fun(X) -> ?SECS(X) end);
which_ttest_runtime_env2(S) ->
    convert_time(S, fun(X) -> ?SECS(X) end).

convert_time(TStrRev, Convert) ->
    try list_to_integer(lists:reverse(TStrRev)) of
        I -> Convert(I)
    catch
        _:_ ->
            ?TTEST_STANDARD_RUNTIME
    end.

%% ttest_tcp(TC,
%%           Domain,
%%           ServerMod, ServerActive,
%%           ClientMod, ClientActive,
%%           MsgID, MaxOutstanding) ->
%%     ttest_tcp(TC,
%%               ?TTEST_STANDARD_RUNTIME,
%%               Domain,
%%               ServerMod, ServerActive,
%%               ClientMod, ClientActive,
%%               MsgID, MaxOutstanding).
ttest_tcp(TC,
          Runtime,
          Domain,
          ServerMod, ServerActive,
          ClientMod, ClientActive,
          MsgID, MaxOutstanding) ->
    ttest_tcp(TC,
	      Runtime,
	      Domain,
	      ServerMod, ServerActive,
	      ClientMod, ClientActive,
	      MsgID, MaxOutstanding, true).

ttest_tcp(TC,
          Runtime,
          Domain,
          ServerMod, ServerActive,
          ClientMod, ClientActive,
          MsgID, MaxOutstanding,
	  Remote) ->
    tc_try(TC,
           fun() ->
                   if
 
                       (Domain =:= local) ->
                           %% On darwin we seem to hit the system limit(s)
                           %% much earlier.
                           %% The tests "mostly" work, but random cases fail
                           %% (even on reasonably powerful machines),
                           %% so its much simpler to just skip on darwin...
                           has_support_unix_domain_socket(),
                           is_not_darwin();
                       (Domain =:= inet) ->
                           has_support_ipv4();
                       (Domain =:= inet6) ->
                           has_support_ipv6();
                       true -> ok 
                   end
           end,
           fun() ->
                   %% This may be overkill, depending on the runtime,
                   %% but better safe then sorry...
                   ?TT(Runtime + ?SECS(60)),
                   ?P("Parameters: "
                      "~n   Domain:          ~p"
                      "~n   Message ID:      ~p"
                      "~n   Max Outstanding: ~p"
                      "~n   Running Time:    ~p"
                      "~n   Server Module:   ~p"
                      "~n   Server Active:   ~p"
                      "~n   Client Module:   ~p"
                      "~n   Client Active:   ~p"
                      "~n   Remote:          ~p",
                      [Domain, MsgID, MaxOutstanding, Runtime,
                       ServerMod, ServerActive, ClientMod, ClientActive,
		       Remote]),
                   InitState = #{domain          => Domain,
                                 msg_id          => MsgID,
                                 max_outstanding => MaxOutstanding,
                                 runtime         => Runtime,
                                 server_mod      => ServerMod,
                                 server_active   => ServerActive,
                                 client_mod      => ClientMod,
                                 client_active   => ClientActive,
				 remote          => Remote},
                   ttest_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ttest_tcp(InitState) ->
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},


         %% *** Init part ***
         #{desc => "(maybe) create node",
           cmd  => fun(#{remote := true} = State) ->
                           {Peer, Node} = start_node("server"),
			   ?SEV_IPRINT("server node created:"
				       "~n   Node: ~p", [Node]),
                           {ok, State#{peer => Peer, node => Node}};
		      (State) ->
			   ?SEV_IPRINT("use local node for server"),
			   {ok, State#{peer => undefined, node => node()}}
                   end},
         #{desc => "(maybe) monitor server node",
           cmd  => fun(#{node := Node} = _State) when (Node =/= node) ->
                           true = erlang:monitor_node(Node, true),
			   ?SEV_IPRINT("~p monitored", [Node]),
                           ok;
		      (_State) ->
			   ?SEV_IPRINT("nothing"),
			   ok
                   end},
         #{desc => "start ttest (remote) server",
           cmd  => fun(#{domain := local = Domain,
                         mod    := Mod,
                         active := Active,
                         node   := Node} = State) ->
                           case ttest_tcp_server_start(Node,
                                                       Domain, Mod, Active) of
                               {ok, {{Pid, _}, Path}} ->
                                   ?SEV_IPRINT("server started: "
                                               "~n   Pid:  ~p"
                                               "~n   Path: ~p", [Pid, Path]),
                                   {ok, State#{rserver => Pid,
                                               path    => Path}};
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{domain := Domain,
                         mod    := Mod,
                         active := Active,
                         node   := Node} = State) ->
                           case ttest_tcp_server_start(Node,
                                                       Domain, Mod, Active) of
                               {ok, {{Pid, _}, {Addr, Port}}} ->
                                   ?SEV_IPRINT("server started: "
                                               "~n   Pid:  ~p"
                                               "~n   Addr: ~p"
                                               "~n   Port: ~p",
                                               [Pid, Addr, Port]),
                                   {ok, State#{rserver => Pid,
                                               addr    => Addr,
                                               port    => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain := local,
                         tester := Tester,
                         path   := Path}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Path),
                           ok;
                      (#{tester := Tester,
                         addr   := Addr,
                         port   := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, {Addr, Port}),
                           ok
                   end},


         %% *** Termination ***
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester  := Tester, 
                         rserver := RServer} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester,
                                                     [{rserver, RServer}]) of
                               ok ->
                                   ?SEV_IPRINT("received termination request"),
                                   {ok, maps:remove(tester, State)};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("received unexpected error: "
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         %% The remote server is in a accept, with a timeout of 5 seconds,
         %% so may have to wait a bit...
         #{desc => "order (remote) ttest server terminate",
           cmd  => fun(#{node    := _Node,
                         rserver := RServer}) ->
                           ttest_tcp_server_stop(RServer),
                           ok
                   end},
         #{desc => "await ttest (remote) server termination",
           cmd  => fun(#{rserver := RServer} = State) ->
                           ?SEV_AWAIT_TERMINATION(RServer),
                           State1 = maps:remove(rserver, State),
                           {ok, State1}
                   end},
         #{desc => "(maybe) stop (server) node",
           cmd  => fun(#{peer := Peer,
                         node := _Node} = State) when (Peer =/= undefined) ->
                           {ok,
                            try peer:stop(Peer) of
                                ok ->
                                    State#{node_stop => ok};
                                {error, Reason} ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   ~p", [Reason]),
                                    State#{node_stop => error}
                            catch
                                C:E:S ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   Class: ~p"
                                                "~n   Error: ~p"
                                                "~n   Stack: ~p",[C, E, S]),
                                    State#{node_stop => error}
                            end};
		      (_) ->
                           ?SEV_IPRINT("nothing"),
			   ok
                   end},
         #{desc => "(maybe) await (server) node termination",
           cmd  => fun(#{node := Node, node_stop := ok} = State)
			 when (Node =/= node()) ->
                           ?SEV_IPRINT("Success node stop - await nodedown"),
                           receive
                               {nodedown, Node} ->
                                   ?SEV_IPRINT("nodedown received - cleanup"),
                                   State1 = maps:remove(node, State),
                                   {ok, State1}
                           end;
                      (#{node_stop := error} = State) ->
                           ?SEV_IPRINT("Failed node stop - cleanup"),
                           State1 = maps:remove(node, State),
                           {ok, State1};
		      (State) ->
                           ?SEV_IPRINT("nothing"),
                           State1 = maps:remove(node, State),
                           {ok, State1}
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(#{domain := local} = State) ->
                           {Tester, ServerPath} = ?SEV_AWAIT_START(),
                           ?SEV_IPRINT("started with server info: "
                                       "~n   Path: ~p", [ServerPath]),
                           {ok, State#{tester      => Tester,
                                       server_path => ServerPath}};
                      (State) ->
                           {Tester, {ServerAddr, ServerPort}} = 
                               ?SEV_AWAIT_START(),
                           ?SEV_IPRINT("started with server info: "
                                       "~n   Addr: ~p"
                                       "~n   Port: ~p",
                                       [ServerAddr, ServerPort]),
                           {ok, State#{tester      => Tester,
                                       server_addr => ServerAddr,
                                       server_port => ServerPort}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},


         %% *** Init part ***
         #{desc => "(maybe) create node",
           cmd  => fun(#{remote := true} = State) ->
                           ?SEV_IPRINT("start 'client' node"),
                           {Peer, Node} = start_node("client"),
			   ?SEV_IPRINT("client node created:"
                                       "~n   Peer: ~p"
				       "~n   Node: ~p", [Peer, Node]),
                           {ok, State#{peer => Peer, node => Node}};
		      (State) ->
                           ?SEV_IPRINT("use local node for client"),
			   {ok, State#{peer => undefined, node => node()}}
                   end},
         #{desc => "(maybe) monitor client node",
           cmd  => fun(#{node := Node} = _State) when (Node =/= node()) ->
                           true = erlang:monitor_node(Node, true),
			   ?SEV_IPRINT("~p monitored", [Node]),
                           ok;
		      (_) ->
			   ?SEV_IPRINT("nothing"),
			   ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},


         %% The actual test
         #{desc => "await continue (ttest)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, ttest),
                           ok
                   end},
         #{desc => "start ttest (remote) client",
           cmd  => fun(#{domain          := local = Domain,
                         node            := Node,
                         mod             := Mod,
                         active          := Active,
                         msg_id          := MsgID,
                         max_outstanding := MaxOutstanding,
                         runtime         := RunTime,
                         server_path     := Path} = State) ->
                           Self   = self(),
                           Notify =
                               fun(Result) ->
                                       ?SEV_ANNOUNCE_READY(Self, ttest, Result)
                               end,                           
                           case ttest_tcp_client_start(Node, Notify,
                                                       Domain, Mod,
                                                       Path,
                                                       Active,
                                                       MsgID, MaxOutstanding,
                                                       RunTime) of
                               {ok, {Pid, _MRef}} ->
                                   {ok, State#{rclient => Pid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{domain          := Domain,
                         node            := Node,
                         mod             := Mod,
                         active          := Active,
                         msg_id          := MsgID,
                         max_outstanding := MaxOutstanding,
                         runtime         := RunTime,
                         server_addr     := Addr,
                         server_port     := Port} = State) ->
                           Self   = self(),
                           Notify =
                               fun(Result) ->
                                       ?SEV_ANNOUNCE_READY(Self, ttest, Result)
                               end,                           
                           case ttest_tcp_client_start(Node, Notify,
                                                       Domain, Mod,
                                                       {Addr, Port},
                                                       Active,
                                                       MsgID, MaxOutstanding,
                                                       RunTime) of
                               {ok, {Pid, _MRef}} ->
                                   {ok, State#{rclient => Pid}};
                               {error, {connect, Reason, ServerInfo} = EI} ->
                                   ?SEV_EPRINT("Failed connecting to server: "
                                               "~n   Reason: ~p"
                                               "~n   Server: ~p",
                                               [Reason, ServerInfo]),
                                   case Reason of
                                       eaddrnotavail ->
                                           {skip, Reason};
                                       network_unreachable ->
                                           {skip, Reason};
                                       _ ->
                                           {error, EI}
                                   end;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await ttest ready",
           cmd  => fun(#{tester  := Tester,
                         rclient := RClient} = State) ->
                           case ?SEV_AWAIT_READY(RClient, rclient, ttest, 
                                                 [{tester, Tester}]) of
                             {ok, Result} ->
                                 {ok, State#{result => Result}};
                             {error, _} = ERROR ->
                                 ERROR
                         end
                   end},
         #{desc => "await ttest (remote) client termination",
           cmd  => fun(#{rclient := RClient} = State) ->
                           ?SEV_AWAIT_TERMINATION(RClient),
                           State1 = maps:remove(rclient, State),
                           {ok, State1}
                   end},
         #{desc => "announce ready (ttest)",
           cmd  => fun(#{tester := Tester,
                         result := Result} = State) ->
                           ?SEV_ANNOUNCE_READY(Tester, ttest, Result),
                           {ok, maps:remove(result, State)}
                   end},


         %% *** Termination ***
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "(maybe) stop (client) node",
           cmd  => fun(#{peer := Peer,
                         node := _Node} = State) when (Peer =/= undefined) ->
                           {ok,
                            try peer:stop(Peer) of
                                ok ->
                                    State#{node_stop => ok};
                                {error, Reason} ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   ~p", [Reason]),
                                    State#{node_stop => error}
                            catch
                                C:E:S ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   Class: ~p"
                                                "~n   Error: ~p"
                                                "~n   Stack: ~p",[C, E, S]),
                                    State#{node_stop => error}
                            end};
		      (_) ->
			   ?SEV_IPRINT("nothing"),
			   ok
                   end},
         #{desc => "(maybe) await (client) node termination",
           cmd  => fun(#{node := Node, node_stop := ok} = State)
			 when (Node =/= node()) ->
                           ?SEV_IPRINT("Success node stop - await nodedown"),
                           receive
                               {nodedown, Node} ->
                                   ?SEV_IPRINT("nodedown received - cleanup"),
                                   State1 = maps:remove(node, State),
                                   {ok, State1}
                           end;
                      (#{node_stop := error} = State) ->
                           ?SEV_IPRINT("Failed node stop - cleanup"),
                           State1 = maps:remove(node, State),
                           {ok, State1};
		      (_) ->
			   ?SEV_IPRINT("nothing"),
			   ok
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{domain := local,
                         server := Pid} = State) ->
                           {ok, Path} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_path => Path}};
                      (#{server := Pid} = State) ->
                           {ok, {Addr, Port}} =
                               ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_addr => Addr,
                                       server_port => Port}}
                   end},


         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{domain      := local,
                         client      := Pid,
                         server_path := Path} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Path),
                           ok;
                      (#{client      := Pid,
                         server_addr := Addr,
                         server_port := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {Addr, Port}),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Client} = _State) ->
                           ok = ?SEV_AWAIT_READY(Client, client, init)
                   end},
 
         %% The actual test
         #{desc => "order client continue (ttest)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, ttest),
                           ok
                   end},
         #{desc => "await client ready (ttest)",
           cmd  => fun(#{server := Server,
                         client := Client} = State) ->
                           case ?SEV_AWAIT_READY(Client, client, ttest,
                                                 [{server, Server}]) of
                               {ok, Result} ->
                                   {ok, State#{result => Result}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** Terminate server ***
         #{desc => "order client terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client down",
           cmd  => fun(#{client := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(client,    State),
                           {ok, State1}
                   end},
         #{desc => "order server terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server down",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_TERMINATION(Server),
                           ok
                   end},

         
         %% Present the results
         #{desc => "present the results",
           cmd  => fun(#{ctrl          := CTRL,
                         result        := Result,
                         domain        := Domain,
                         server_mod    := ServerTrans,
                         server_active := ServerActive,
                         client_mod    := ClientTrans,
                         client_active := ClientActive,
                         msg_id        := MsgID} = State) ->
                           case Result of
                               #{status  := ok,
                                 runtime := RunTime,
                                 cnt     := Cnt,
                                 bcnt    := BCnt} ->
                                   ttest_report(Domain,
                                                ServerTrans, ServerActive,
                                                ClientTrans, ClientActive,
                                                MsgID,
                                                RunTime, BCnt, Cnt),
                                   ?SEV_IPRINT(
                                      "TTest results: "
                                      "~n   Run Time:                    ~s"
                                      "~n   Byte Count:                  ~s"
                                      "~n   Number of message exchanges: ~s"
                                      "~n~n",
                                      [
                                       ?TTEST_LIB:format_time(RunTime),
                                       if ((BCnt =:= 0) orelse (RunTime =:= 0)) ->
                                               ?TTEST_LIB:format("~w, ~w",
                                                                 [BCnt, RunTime]);
                                          true ->
                                               ?TTEST_LIB:format("~p => ~p byte / ms",
                                                                 [BCnt, BCnt div RunTime])
                                       end,
                                       if (RunTime =:= 0) ->
                                               "-";
                                          true ->
                                               ?TTEST_LIB:format("~p => ~p iterations / ms",
                                                                 [Cnt, Cnt div RunTime])
                                       end
                                      ]),
                                   if ((BCnt > 0) andalso (RunTime > 0)) ->
                                           ?SEV_IPRINT("send short form results to CTRL (~p)", [CTRL]),
                                           CTRL ! {self(), {BCnt div RunTime,
                                                            Cnt div RunTime}};
                                      true ->
                                           ?SEV_IPRINT("no proper result: "
                                                       "~n   RunTime: ~p"
                                                       "~n   BCnt:    ~p"
                                                       "~n   Cnt:     ~p",
                                                       [RunTime, BCnt, Cnt])
                                   end,
                                   {ok, maps:remove(result, State)};

                               #{status  := Failure,
                                 runtime := RunTime,
                                 sid     := SID,
                                 rid     := RID,
                                 scnt    := SCnt,
                                 rcnt    := RCnt,
                                 bcnt    := BCnt,
                                 num     := Num} ->
                                   ?SEV_EPRINT("Time Test failed: "
                                               "~n   ~p"
                                               "~n"
                                               "~nwhen"
                                               "~n"
                                               "~n   Run Time:       ~s"
                                               "~n   Send ID:        ~p"
                                               "~n   Recv ID:        ~p"
                                               "~n   Send Count:     ~p"
                                               "~n   Recv Count:     ~p"
                                               "~n   Byte Count:     ~p"
                                               "~n   Num Iterations: ~p",
                                               [Failure,
                                                ?TTEST_LIB:format_time(RunTime),
                                                SID, RID, SCnt, RCnt, BCnt, Num]),
                                   {error, Failure}
                           end
                   end},

         %% This is just so that the printout above shall have time to come
         %% out before then end of the test case.
         ?SEV_SLEEP(?SECS(1)),

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    Domain = maps:get(domain, InitState),
    LHost  = local_host(),
    LAddr  = which_local_addr(Domain),

    i("start server evaluator"),
    ServerInitState = #{host   => LHost,
			addr   => LAddr,
                        domain => Domain,
                        mod    => maps:get(server_mod,    InitState),
                        active => maps:get(server_active, InitState),
                        remote => maps:get(remote,        InitState)},
    Server          = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator"),
    ClientInitState = #{host            => LHost,
			addr            => LAddr,
                        domain          => Domain,
                        mod             => maps:get(client_mod,      InitState),
                        active          => maps:get(client_active,   InitState),
                        msg_id          => maps:get(msg_id,          InitState),
                        max_outstanding => maps:get(max_outstanding, InitState),
                        runtime         => maps:get(runtime,         InitState),
                        remote          => maps:get(remote,          InitState)},
    Client          = ?SEV_START("client", ClientSeq, ClientInitState),
    
    i("start 'tester' evaluator"),
    TesterInitState = #{ctrl          => self(),
                        domain        => Domain,
			msg_id        => maps:get(msg_id,        InitState),
                        client        => Client#ev.pid,
			client_mod    => maps:get(client_mod,    InitState),
			client_active => maps:get(client_active, InitState),
                        server        => Server#ev.pid,
                        server_mod    => maps:get(server_mod,    InitState),
                        server_active => maps:get(server_active, InitState),
                        remote        => maps:get(remote,        InitState)},
    #ev{pid = TesterPid} = Tester =
        ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]),
    receive
        {TesterPid, {BCnt, MCnt}} ->
            {comment, ?F("~w b/ms, ~w iter/ms", [BCnt, MCnt])}
    after 0 ->
            {comment, "-"}
    end.



ttest_tcp_server_start(Node, Domain, gen, Active) ->
    TransportMod = socket_test_ttest_tcp_gen,
    Transport    = {TransportMod, #{domain => Domain}},
    socket_test_ttest_tcp_server:start_monitor(Node, Transport, Active);
ttest_tcp_server_start(Node, Domain, gs, Active) ->
    TransportMod = socket_test_ttest_tcp_gs,
    Transport    = {TransportMod, #{domain => Domain}},
    socket_test_ttest_tcp_server:start_monitor(Node, Transport, Active);
ttest_tcp_server_start(Node, Domain, sock, Active) ->
    TransportMod = socket_test_ttest_tcp_socket,
    Transport    = {TransportMod, #{domain => Domain,
                                    async  => true,
                                    method => plain}},
    socket_test_ttest_tcp_server:start_monitor(Node, Transport, Active).

ttest_tcp_server_stop(Pid) ->
    socket_test_ttest_tcp_server:stop(Pid).

ttest_tcp_client_start(Node,
                       Notify,
                       Domain, gen,
                       ServerInfo, Active, MsgID, MaxOutstanding, RunTime) ->
    TransportMod = socket_test_ttest_tcp_gen,
    Transport    = {TransportMod, #{domain => Domain}},    
    socket_test_ttest_tcp_client:start_monitor(Node,
                                               Notify,
                                               Transport,
                                               ServerInfo,
                                               Active,
                                               MsgID, MaxOutstanding, RunTime);
ttest_tcp_client_start(Node,
                       Notify,
                       Domain, gs,
                       ServerInfo, Active, MsgID, MaxOutstanding, RunTime) ->
    TransportMod = socket_test_ttest_tcp_gs,
    Transport    = {TransportMod, #{domain => Domain}},    
    socket_test_ttest_tcp_client:start_monitor(Node,
                                               Notify,
                                               Transport,
                                               ServerInfo,
                                               Active,
                                               MsgID, MaxOutstanding, RunTime);
ttest_tcp_client_start(Node,
                       Notify,
                       Domain, sock,
                       ServerInfo, Active, MsgID, MaxOutstanding, RunTime) ->
    TransportMod = socket_test_ttest_tcp_socket,
    Transport    = {TransportMod, #{domain => Domain,
                                    async  => true,
                                    method => plain}},
    socket_test_ttest_tcp_client:start_monitor(Node,
                                               Notify,
                                               Transport,
                                               ServerInfo,
                                               Active,
                                               MsgID, MaxOutstanding, RunTime).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TTEST_MANAGER, esock_ttest_manager).

-record(ttest_report_id,
        {domain        :: socket:domain(),
         serv_trans    :: gen | sock,
         serv_active   :: once | boolean(),
         client_trans  :: gen | sock,
         client_active :: once | boolean(),
         msg_id        :: small | medium | large}).

-record(ttest_report, {id    :: #ttest_report_id{},
                       time  :: non_neg_integer(),
                       bytes :: non_neg_integer(),
                       msgs  :: non_neg_integer()}).

-spec ttest_report(Domain       :: socket:domain(),
                   ServTrans    :: gen | gs | sock,
                   ServActive   :: once | boolean(),
                   ClientTrans  :: gen | gs | sock,
                   ClientActive :: once | boolean(),
                   MsgID        :: 1 | 2 | 3,
                   RunTime      :: non_neg_integer(),
                   NumBytes     :: non_neg_integer(),
                   NumMsgs      :: non_neg_integer()) -> ok.

ttest_report(Domain,
             ServTrans,   ServActive,
             ClientTrans, ClientActive,
             MsgID,
             RunTime,
             NumBytes,
             NumMsgs) ->
    ID = #ttest_report_id{domain        = Domain,
                          serv_trans    = ServTrans,
                          serv_active   = ServActive,
                          client_trans  = ClientTrans,
                          client_active = ClientActive,
                          msg_id        = ttest_msg_id_num_to_name(MsgID)},
    Report = #ttest_report{id    = ID,
                           time  = RunTime,
                           bytes = NumBytes,
                           msgs  = NumMsgs},
    %% If we run just one test case, the group init has never been run
    %% and therefore the ttest manager is not running (we also don't actually
    %% care about collecting reports in that case).
    (catch global:send(?TTEST_MANAGER, Report)),
    ok.

ttest_msg_id_num_to_name(1) ->
    small;
ttest_msg_id_num_to_name(2) ->
    medium;
ttest_msg_id_num_to_name(3) ->
    large.
    
ttest_manager_start(Category) ->
    Self = self(),
    {Pid, MRef} = spawn_monitor(fun() ->
                                        ttest_manager_init(Self, Category)
                                end),
    receive
        {ttest_manager_started, Pid} ->
            erlang:demonitor(MRef, [flush]),
            ok;
        {'DOWN', MRef, process, Pid, Reason} ->
            exit({failed_starting, ttest_manager, Reason})
    after 5000 ->
            exit(Pid, kill),
            exit({failed_starting, ttest_manager, timeout})
    end.

ttest_manager_stop() ->
    case global:whereis_name(?TTEST_MANAGER) of
        Pid when is_pid(Pid) ->
            erlang:monitor(process, Pid),
            global:send(?TTEST_MANAGER, stop),
            receive
                {'DOWN', _MRef, process, Pid, _} ->
                    ok
            after 10000 ->
                    exit(Pid, kill),
                    ok
            end;
        _ ->
            ok
    end.

ttest_manager_init(Parent, Category) ->
    yes = global:register_name(?TTEST_MANAGER, self()),
    ets:new(?TTEST_MANAGER, 
            [{keypos, #ttest_report.id}, named_table, protected, ordered_set]),
    Parent ! {ttest_manager_started, self()},
    ttest_manager_loop(#{category => Category}).


-define(BENCH_SUITE, socket_ttest).
-define(BENCH_EVENT(__N__, __V__),
        #event{name = benchmark_data,
               data = [{suite, ?BENCH_SUITE},
                       {value, (__V__)},
                       {name,  (__N__)}]}).

ttest_manager_loop(State) ->
    receive
        stop ->
            ?LOGGER:format("manager stopping~n", []),
            ttest_manager_done();

        #ttest_report{id = ID} = Report ->
            maybe_report(State, Report),
            case ets:insert_new(?TTEST_MANAGER, Report) of
		true ->
		    ttest_manager_loop(State);
		false ->
		    [Current] = ets:lookup(?TTEST_MANAGER, ID),
		    ?LOGGER:format("manager received duplicate report:"
				   "~n   ID:      ~p"
				   "~n   Current: ~p"
				   "~n   New:     ~p"
				   "~n", [ID, Current, Report]),
		    ttest_manager_loop(State)
	    end
    end.

maybe_report(#{category := bench},
             #ttest_report{id    = ID,
                           time  = RunTime,
                           bytes = NumBytes}) ->
    Event = ?BENCH_EVENT(format_ttest_report_id(ID),
                         format_ttest_report_value(RunTime, NumBytes)),
    ct_event:notify(Event),
    ok;
maybe_report(_State, _Report) ->
    ok.


format_ttest_report_id(#ttest_report_id{domain        = Domain,
                                        serv_trans    = STrans,
                                        serv_active   = SActive,
                                        client_trans  = CTrans,
                                        client_active = CActive,
                                        msg_id        = MsgID}) ->
    EventNameStr = ?F("server:~w:~w_client:~w:~w_~w_~w",
                      [format_ttest_report_id_trans(STrans),
                       format_ttest_report_id_active(SActive),
                       format_ttest_report_id_trans(CTrans),
                       format_ttest_report_id_active(CActive),
                       format_ttest_report_id_domain(Domain),
                       format_ttest_report_id_msg_id(MsgID)]),
    list_to_atom(EventNameStr).
    
format_ttest_report_id_trans(gen)  -> g;
format_ttest_report_id_trans(gs)   -> gs;
format_ttest_report_id_trans(sock) -> s.

format_ttest_report_id_active(once)  -> o;
format_ttest_report_id_active(true)  -> t;
format_ttest_report_id_active(false) -> f.

format_ttest_report_id_domain(D) -> D.

format_ttest_report_id_msg_id(ID) -> ID.

format_ttest_report_value(RunTime, NumBytes)
  when (RunTime > 0) andalso (NumBytes > 0) ->
    NumBytes div RunTime;
format_ttest_report_value(RunTime, _) when (RunTime > 0) ->
    -1;
format_ttest_report_value(_, _) ->
    -2.



%% We are supposed to pretty print the result here...
ttest_manager_done() ->
    format_reports(inet),
    %% format_reports(inet6),
    ets:delete(?TTEST_MANAGER),
    exit(normal).

format_reports(Domain) ->
    ?LOGGER:format("Domain ~w reports:~n~n", [Domain]),
    format_reports(Domain, small),
    format_reports(Domain, medium),
    format_reports(Domain, large).
    
format_reports(Domain, MsgID) when is_atom(MsgID) ->
    case which_ttest_reports(Domain, MsgID) of
        [] ->
            ?LOGGER:format("   No ~w reports~n~n", [MsgID]);
        Reports ->
            ?LOGGER:format("   ~w reports: ~n", [MsgID]),
            lists:foreach(fun(R) -> format_report(R) end, Reports)
    end.

%% This should really be a table like this:
%%
%%             client
%% server      gen(false)  gen(once)  gen(true)  sock(false)  sock(once)  sock(true)
%% gen(false)  nnn
%% gen(once)   nnn
%% gen(true)   nnn
%% sock(false) nnn
%% sock(once)  nnn
%% sock(true)  nnn
%%
format_report(#ttest_report{id    = #ttest_report_id{serv_trans    = STrans,
                                                     serv_active   = SActive,
                                                     client_trans  = CTrans,
                                                     client_active = CActive},
                            time  = RunTime,
                            bytes = BCnt,
                            msgs  = MCnt}) ->
    ?LOGGER:format("      server ~w[~w] - client ~w[~w] => "
                   "~n         Run Time: ~s"
                   "~n         Bytes:    ~s"
                   "~n         Messages: ~s"
                   "~n", [STrans, SActive, CTrans, CActive,
                          ?TTEST_LIB:format_time(RunTime),
                          if ((BCnt =:= 0) orelse (RunTime =:= 0)) ->
                                  ?TTEST_LIB:format("~w, ~w",
                                                    [BCnt, RunTime]);
                             true ->
                                  ?TTEST_LIB:format("~p => ~p byte / ms",
                                                    [BCnt, BCnt div RunTime])
                          end,
                          if (RunTime =:= 0) ->
                                  "-";
                             true ->
                                  ?TTEST_LIB:format("~p => ~p iterations / ms",
                                                    [MCnt, MCnt div RunTime])
                          end]),
    ok.


which_ttest_reports(Domain, all) ->
    [R || R = #ttest_report{id = #ttest_report_id{domain = D}} <- 
              ets:tab2list(?TTEST_MANAGER), Domain =:= D];
which_ttest_reports(Domain, MsgID) ->
    [R || R = #ttest_report{id = #ttest_report_id{domain = D, msg_id = MID}} <- 
              ets:tab2list(?TTEST_MANAGER), (Domain =:= D) andalso
                                                             (MsgID =:= MID)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local_host() ->
    try net_adm:localhost() of
        Host when is_list(Host) ->
	    %% Convert to shortname if long
	    case string:tokens(Host, [$.]) of
		[H|_] ->
		    list_to_atom(H)
	    end
    catch
        C:E:S ->
            erlang:raise(C, E, S)
    end.


%% The point of this is to "ensure" that paths from different test runs
%% don't clash.

mk_unique_path() ->
    ?SLIB:mk_unique_path().


which_local_addr(local = _Domain) ->
    mk_unique_path();

%% This gets the local address (not 127.0...)
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_addr(Domain) ->
    ?KLIB:which_local_addr(Domain).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here are all the *general* test case condition functions.

is_not_darwin() ->
    is_not_platform(darwin, "Darwin").

is_not_platform(Platform, PlatformStr)
  when is_atom(Platform) andalso is_list(PlatformStr) ->
      case os:type() of
          {unix, Platform} ->
              skip("This does not work on " ++ PlatformStr);
        _ ->
            ok
    end.
  

has_support_unix_domain_socket() ->
    case socket:is_supported(local) of
	true ->
	    ok;
	false ->
	    skip("Not supported")
    end.


%% The idea is that this function shall test if the test host has 
%% support for IPv4 or IPv6. If not, there is no point in running 
%% corresponding tests.
%% Currently we just skip.
has_support_ipv4() ->
    ?KLIB:has_support_ipv4().

has_support_ipv6() ->
    ?KLIB:has_support_ipv6().



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tc_try(Case, TCCondFun, TCFun) ->
    %% ?KLIB:tc_try(Case, TCCondFun, TCFun).
    ?TC_TRY(Case, TCCondFun, TCFun).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_node(Name) ->                
    start_node(Name, 5000).

start_node(Name, Timeout) when is_integer(Timeout) andalso (Timeout > 0) ->
    Pa   = filename:dirname(code:which(?MODULE)),
    Args = ["-pa", Pa,
            "-s", atom_to_list(?PROXY), "start", atom_to_list(node()),
            "-s", atom_to_list(kernel_test_sys_monitor), "start",
            "-s", "global", "sync"],
    ?SEV_IPRINT("try start node ~p", [Name]),
    try ?CT_PEER(#{name      => Name,
                   wait_boot => Timeout,
                   %% connection => standard_io,
                   args      => Args}) of
        {ok, Peer, Node} ->
            ?SEV_IPRINT("Started node: "
                        "~n   Peer:       ~p"
                        "~n   Node:       ~p"
                        "~n   => await global synced", [Peer, Node]),
            await_sys_monitor_synced(Node),
            ?SEV_IPRINT("ping proxy"),
            pong = ?PPING(Node),
            {Peer, Node};
        {error, Reason} ->
            ?SEV_EPRINT("failed starting node ~p (=> SKIP):"
                        "~n   ~p", [Name, Reason]),
            skip(Reason)
    catch
        Class:Reason:Stack ->
            ?SEV_EPRINT("Failed starting node: "
                        "~n   Class:  ~p"
                        "~n   Reason: ~p"
                        "~n   Stack:  ~p",
                        [Class, Reason, Stack]),
            skip({node_start, Class, Reason})
    end.

            
await_sys_monitor_synced(Node) ->
    i("check if system monitor on node ~p is running", [Node]),
    case kernel_test_sys_monitor:ping(Node) of
        pong ->
            ok;
        pang ->
            i("system monitor on node ~p not yet synced", [Node]),
            receive after 1000 -> ok end,
            await_sys_monitor_synced(Node)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    FStr = ?F("[~s] " ++ F, [?FTS()|A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).


