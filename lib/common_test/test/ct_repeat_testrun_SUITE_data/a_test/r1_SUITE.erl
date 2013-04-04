%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%----------------------------------------------------------------------
%% File: r1_SUITE.erl
%%
%% Description:
%%
%%
%% @author Support
%% @doc
%% @end
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
-module(r1_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:seconds(30)).

all() ->
    testcases() ++ [{group,g},  tc2].

groups() ->
    [{g,testcases()}].

testcases() ->
    [tc1,tc2].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%%-----------------------------------------------------------------
%%% Test cases
tc1(_Config) ->
    timer:sleep(10000),
    ok.

tc2(_Config) ->
    ok.
