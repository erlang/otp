%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
-module(subgroups_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, test}].

groups() ->
    [{failing_group, [], [failing_tc]},
     {ok_group, [], [ok_tc]},
     {test, [sequence], [{group, failing_group}, {group, ok_group}]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(test, Config) ->
    Status = ?config(tc_group_result, Config),
    Failed = proplists:get_value(failed, Status),
    true = lists:member({group_result,failing_group}, Failed),
    {return_group_result,failed};

end_per_group(failing_group, Config) ->
    Status = ?config(tc_group_result, Config),
    [{subgroups_1_SUITE,failing_tc}] = proplists:get_value(failed, Status),
    {return_group_result,failed};

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(failing_tc, Config) ->
    {failed,_} = proplists:get_value(tc_status, Config),
    ok;

end_per_testcase(_TestCase, _Config) ->
    ok.

failing_tc(_Config) ->
    2=3.

ok_tc(_Config) ->
    ok.
