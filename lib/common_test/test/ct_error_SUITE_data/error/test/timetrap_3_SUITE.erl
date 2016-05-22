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
-module(timetrap_3_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TO, 3).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,?TO}}].

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
    [{g1,[parallel],[tc0,tc1,tc2,tc3,tc4,tc5,tc6,tc7]}].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [{group,g1}].

tc0() ->
    [{timetrap,2000}].
tc0(_) ->
    ct:comment("TO after 2 sec"),
    ct:sleep({seconds,5}),
    ok.

tc1() ->
    [{timetrap,500}].
tc1(_) ->
    ct:comment("TO after 1/2 sec"),
    ct:sleep({seconds,5}),
    ok.

tc2() ->
    [{timetrap,1000}].
tc2(_) ->
    ct:comment("TO after 1 sec"),
    ct:sleep({seconds,5}),
    ok.

tc3(_) ->    
    ct:comment(io_lib:format("TO after ~w sec", [?TO])),
    ct:sleep({seconds,5}),
    ok.

tc4() ->
    [{timetrap,2000}].
tc4(_) ->
    ct:comment(io_lib:format("TO after 2 sec", [])),
    ct:sleep({seconds,5}),
    ok.

tc5() ->
    [{timetrap,2000}].
tc5(_) ->
    ct:comment("No timeout"),
    ct:sleep({seconds,1}),
    ok.

tc6() ->
    [{timetrap,1000}].
tc6(_) ->
    ct:comment("TO after 1 sec"),
    ct:sleep({seconds,5}),
    ok.

tc7() ->
    [{timetrap,1500}].
tc7(_) ->
    ct:comment("TO after 1 1/2 sec"),
    ct:sleep({seconds,5}),
    ok.
