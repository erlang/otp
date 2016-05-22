%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(timetrap_2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,3}},
     {require,multiply},
     {require,scale}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(tc1, Config) ->
    ct:timetrap({seconds,1}),
    Config;

init_per_testcase(tc2, Config) ->
    ct:timetrap(250),
    Config;

init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_testcase(_, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [tc0,tc1,tc2,tc3].

tc0(_) ->
    N = list_to_integer(ct:get_config(multiply)),
    ct:comment(io_lib:format("TO after ~w sec", [3*N])),
    ct:sleep({seconds,5}),
    ok.

tc1(_) ->
    N =list_to_integer( ct:get_config(multiply)),
    ct:comment(io_lib:format("TO after ~w sec", [1*N])),
    ct:sleep({seconds,5}),
    ok.

tc2(_) ->
    ct:timetrap(500),
    N = list_to_integer(ct:get_config(multiply)),
    ct:comment(io_lib:format("TO after ~w sec", [0.5*N])),
    ct:sleep(2000),
    ok.

tc3() ->
    [{timetrap,{seconds,2}}].

tc3(_) ->
    T0 = erlang:monotonic_time(),
    ct:timetrap(infinity),
    N = list_to_integer(ct:get_config(multiply)),
    ct:comment(io_lib:format("Sleeping for ~w sec...", [4*N])),
    ct:sleep(4000),
    T1 = erlang:monotonic_time(),
    Diff = erlang:convert_time_unit(T1-T0, native, micro_seconds),
    if ((Diff < (N*4000000)) or (Diff > (N*4500000))) ->
	    exit(not_expected);
       true ->
	    ok
    end.
