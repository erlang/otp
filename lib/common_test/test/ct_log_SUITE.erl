%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_log_SUITE
%%%
%%% Description: Test that ct:log, ct:pal and io:format print to
%%% the test case log file as expected, with or without special HTML
%%% characters being escaped. 
%%%
%%%-------------------------------------------------------------------
-module(ct_log_SUITE).

-compile(export_all).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct_test_support:init_per_suite(Config).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [logging].

groups() -> 
    [].

% In theory this could run on the host node, but since these tests -
% intentionally, to verify `ct:pal` and friends - print to stdout of wherever
% CT is being executed, run them in a separate node to keep them quiet.
logging(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Suites = [filename:join(DataDir, "log_SUITE")],
    Opts = [{suite, Suites} | ct_test_support:get_opts(Config)],
    ok = ct_test_support:run(Opts, Config).
