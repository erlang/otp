%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
%%% File: ct_keep_logs_SUITE
%%%
%%% Description:
%%% Test the 'keep_logs' option
%%%
%%%-------------------------------------------------------------------
-module(ct_keep_logs_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    ct_test_support:init_per_suite(Config0).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     keep_logs,
     refresh_logs
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%% Test the keep_logs option with normal common_test runs
keep_logs(Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "keep_logs_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),
    Opts = [{suite,Suite},{label,keep_logs} | Opts0],

    LogDir=?config(logdir,Opts),
    KeepLogsDir = filename:join(LogDir,unique_name("keep_logs-")),
    ok = file:make_dir(KeepLogsDir),
    Opts1 = lists:keyreplace(logdir,1,Opts,{logdir,KeepLogsDir}),
    ct:log("New LogDir = ~s", [KeepLogsDir]),

    %% Create 6 ct_run.* log directories
    [ok = ct_test_support:run(Opts1, Config) || _ <- lists:seq(1,3)],

    %% Verify the number of directories
    WC = filename:join(KeepLogsDir,"ct_run.ct@*"),
    L1 = filelib:wildcard(WC),
    6 = length(L1),

    %% Keep all logs
    {1,0,{0,0}}=ct_test_support:run_ct_run_test([{keep_logs,all}|Opts1], Config),
    L2 = filelib:wildcard(WC),
    7 = length(L2),
    0 = ct_test_support:run_ct_script_start([{keep_logs,all}|Opts1], Config),
    L3 = filelib:wildcard(WC),
    8 = length(L3),

    %% N<length of list
    {1,0,{0,0}}=ct_test_support:run_ct_run_test([{keep_logs,7}|Opts1], Config),
    L4 = filelib:wildcard(WC),
    7 = length(L4),
    0 = ct_test_support:run_ct_script_start([{keep_logs,6}|Opts1], Config),
    L5 = filelib:wildcard(WC),
    6 = length(L5),

    %% N>length of list
    {1,0,{0,0}}=ct_test_support:run_ct_run_test([{keep_logs,10}|Opts1], Config),
    L6 = filelib:wildcard(WC),
    7 = length(L6),
    0 = ct_test_support:run_ct_script_start([{keep_logs,10}|Opts1], Config),
    L7 = filelib:wildcard(WC),
    8 = length(L7),

    %% N==length of list
    {1,0,{0,0}}=ct_test_support:run_ct_run_test([{keep_logs,8}|Opts1], Config),
    L8 = filelib:wildcard(WC),
    8 = length(L8),
    0 = ct_test_support:run_ct_script_start([{keep_logs,8}|Opts1], Config),
    L9 = filelib:wildcard(WC),
    8 = length(L9),

    %% N==length of list + current run
    {1,0,{0,0}}=ct_test_support:run_ct_run_test([{keep_logs,9}|Opts1], Config),
    L10 = filelib:wildcard(WC),
    9 = length(L10),
    0 = ct_test_support:run_ct_script_start([{keep_logs,10}|Opts1], Config),
    L11 = filelib:wildcard(WC),
    10 = length(L11).

%% Test the keep_logs option togwther with the refresh_logs option
refresh_logs(Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "keep_logs_SUITE"),
    Opts0 = ct_test_support:get_opts(Config),
    LogDir=?config(logdir,Opts0),
    KeepLogsDir = filename:join(LogDir,unique_name("refresh_logs-")),
    ok = file:make_dir(KeepLogsDir),
    Opts1 = lists:keyreplace(logdir,1,Opts0,{logdir,KeepLogsDir}),
    ct:log("New LogDir = ~s", [KeepLogsDir]),

    %% Create 6 ct_run.* log directories
    SuiteOpts = [{suite,Suite},{label,refresh_logs} | Opts1],
    [ok = ct_test_support:run(SuiteOpts, Config) || _ <- lists:seq(1,3)],

    %% Verify the number of directories
    WC = filename:join(KeepLogsDir,"ct_run.ct@*"),
    L1 = filelib:wildcard(WC),
    6 = length(L1),

    RefreshOpts =  [{refresh_logs,KeepLogsDir},{label,refresh_logs} | Opts1],

    %% Keep all logs (note that refresh_logs option prevents the
    %% creation of a new log directory for the current run)
    done = ct_test_support:run_ct_run_test([{keep_logs,all}|RefreshOpts], Config),
    L2 = filelib:wildcard(WC),
    6 = length(L2),
    0 = ct_test_support:run_ct_script_start([{keep_logs,all}|RefreshOpts],Config),
    L3 = filelib:wildcard(WC),
    6 = length(L3),

    %% N<length of list
    done = ct_test_support:run_ct_run_test([{keep_logs,5}|RefreshOpts], Config),
    L5 = filelib:wildcard(WC),
    5 = length(L5),
    0 = ct_test_support:run_ct_script_start([{keep_logs,4}|RefreshOpts], Config),
    L6 = filelib:wildcard(WC),
    4 = length(L6),

    %% N>length of list
    done = ct_test_support:run_ct_run_test([{keep_logs,5}|RefreshOpts], Config),
    L7 = filelib:wildcard(WC),
    4 = length(L7),
    0 = ct_test_support:run_ct_script_start([{keep_logs,5}|RefreshOpts], Config),
    L8 = filelib:wildcard(WC),
    4 = length(L8),

    %% N==length of list
    done = ct_test_support:run_ct_run_test([{keep_logs,4}|RefreshOpts], Config),
    L9 = filelib:wildcard(WC),
    4 = length(L9),
    0 = ct_test_support:run_ct_script_start([{keep_logs,4}|RefreshOpts], Config),
    L10 = filelib:wildcard(WC),
    4 = length(L10),

    ok.
%%%-----------------------------------------------------------------
%%% Internal
unique_name(Prefix) ->
    I = erlang:unique_integer([positive]),
    Prefix ++ integer_to_list(I).
