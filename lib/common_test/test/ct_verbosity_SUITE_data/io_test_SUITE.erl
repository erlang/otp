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
-module(io_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,10}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.
    
%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{g1, [parallel], [tc1,tc2,tc3,{group,g2}]},
     {g2, [parallel], [tc1,tc2,tc3]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [tc1,tc2,tc3,{group,g1}].

tc1(_C) ->
    io:format("This is an io:format(~p)~n", [[]]),
    ct:log("ct:log(default)", []),
    ct:log(?STD_IMPORTANCE, "ct:log(default,~p)", [?STD_IMPORTANCE]),
    ct:log(error, "ct:log(error)", []),
    ct:log(error, ?STD_IMPORTANCE, "ct:log(error,~p)", [?STD_IMPORTANCE]),
    ct:log(1, "ct:log(default,~p)", [1]),
    ct:log(error, 1, "ct:log(error,~p)", [1]),
    ct:log(99, "ct:log(default,~p)", [99]),
    ct:log(error, 99, "ct:log(error,~p)", [99]),
    ok.

tc2(_C) ->
    io:format("This is an io:format(~p)~n", [[]]),
    ct:pal("ct:pal(default)", []),
    ct:pal(?STD_IMPORTANCE, "ct:pal(default,~p)", [?STD_IMPORTANCE]),
    ct:pal(error, "ct:pal(error)", []),
    ct:pal(error, ?STD_IMPORTANCE, "ct:pal(error,~p)", [?STD_IMPORTANCE]),
    ct:pal(1, "ct:pal(default,~p)", [1]),
    ct:pal(error, 1, "ct:pal(error,~p)", [1]),
    ct:pal(99, "ct:pal(default,~p)", [99]),
    ct:pal(error, 99, "ct:pal(error,~p)", [99]),
    ok.

tc3(_C) ->
    io:format("This is an io:format(~p)~n", [[]]),
    ct:print("ct:print(default)", []),
    ct:print(?STD_IMPORTANCE, "ct:print(default,~p)", [?STD_IMPORTANCE]),
    ct:print(error, "ct:print(error)", []),
    ct:print(error, ?STD_IMPORTANCE, "ct:print(error,~p)", [?STD_IMPORTANCE]),
    ct:print(1, "ct:print(default,~p)", [1]),
    ct:print(error, 1, "ct:print(error,~p)", [1]),
    ct:print(99, "ct:print(default,~p)", [99]),
    ct:print(error, 99, "ct:print(error,~p)", [99]),
    ok.
