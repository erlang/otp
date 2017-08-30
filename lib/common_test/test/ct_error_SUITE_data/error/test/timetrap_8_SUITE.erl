%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(timetrap_8_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TO, 4).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{timetrap_utils,timetrap_val,[{seconds,?TO}]}}].

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
init_per_group(G6, Config) when G6==g6; G6==pg6 ->
    ct:sleep({seconds,1}),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(G7or8, _Config) when G7or8==g7; G7or8==pg7; G7or8==g8; G7or8==pg8 ->
    ct:sleep({seconds,5}),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_, Config) ->
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
    [
     {g0,[],[tc0,tc2]}, % group override suite and tc overrides group
     {g1,[],[tc0,tc2]}, % group override suite and tc overrides group
     {g2,[],[tc1,tc2]}, % tc override group
     {g3,[],[tc4,{group,g1},{group,g2}]},  % subgroup override group
     {g4,[],[tc0,tc2]}, % exit during init_per_group
     {g5,[],[tc0,tc2]}, % exit during init_per_group
     {g6,[],[tc0,tc2]}, % timeout during init_per_group
     {g7,[],[tc5]},     % exit during end_per_group
     {g8,[],[tc5]},     % timeout during end_per_group
     {g9,[],[tc5,tc0]}, % exit during testcase
     {g10,[],[tc0,tc5]}, % exit during testcase
     {g11,[],[tc3,tc2]}, % suite is valid if nothing else is specified
     {pg0,[parallel],[tc0,tc2]}, % group override suite and tc overrides group
     {pg1,[parallel],[tc0,tc2]}, % group override suite and tc overrides group
     {pg2,[parallel],[tc1,tc2]}, % tc override group
     {pg3,[parallel],[tc4,{group,pg1},{group,pg2}]},  % subgroup override group
     {pg4,[parallel],[tc0,tc2]}, % exit during init_per_group
     {pg5,[parallel],[tc0,tc2]}, % exit during init_per_group
     {pg6,[parallel],[tc0,tc2]}, % timeout during init_per_group
     {pg7,[parallel],[tc5]},     % exit during end_per_group
     {pg8,[parallel],[tc5]},     % timeout during end_per_group
     {pg9,[parallel],[tc5,tc0]}, % exit during testcase
     {pg10,[parallel],[tc0,tc5]},% exit during testcase
     {pg11,[parallel],[tc3,tc2]},% suite is valid if nothing else is specified
     {sg1,[sequence],[tc5,tc0,tc1,tc2]}, % exit during sequencial testcase
     {sg2,[sequence],[tc5,tc0,tc1,tc2]}].% timeout during sequencial testcase

group(g0) ->
    [{timetrap,{timetrap_utils,timetrap_val,[{seconds,1}]}}];
group(g1) ->
    [{timetrap,fun() -> timetrap_utils:timetrap_val(1000) end}];
group(g2) ->
    [{timetrap,fun() -> timetrap_utils:timetrap_val(3000) end}];
group(g3) ->
    [{timetrap,fun() -> timetrap_utils:timetrap_val(2000) end}];
group(g4) ->
    [{timetrap,{timetrap_utils,timetrap_exit,[kaboom]}}];
group(g5) ->
    [{timetrap,fun() -> exit(kaboom) end}];
group(g6) ->
    [{timetrap,{timetrap_utils,timetrap_val,[500]}}];
group(g7) ->
    [{timetrap,fun() -> ct:sleep(1000),exit(kaboom) end}];
group(g8) ->
    [{timetrap,{timetrap_utils,timetrap_val,[500]}}];
group(g9) ->
    [{timetrap,fun() -> ct:sleep(1000),exit(kaboom) end}];
group(g10) ->
    [{timetrap,fun() -> ct:sleep(1000),exit(kaboom) end}];
group(g11) ->
    [];
group(pg0) ->
    [{timetrap,{timetrap_utils,timetrap_val,[{seconds,1}]}}];
group(pg1) ->
    [{timetrap,fun() -> timetrap_utils:timetrap_val(1000) end}];
group(pg2) ->
    [{timetrap,fun() -> timetrap_utils:timetrap_val(3000) end}];
group(pg3) ->
    [{timetrap,fun() -> timetrap_utils:timetrap_val(2000) end}];
group(pg4) ->
    [{timetrap,{timetrap_utils,timetrap_exit,[kaboom]}}];
group(pg5) ->
    [{timetrap,fun() -> exit(kaboom) end}];
group(pg6) ->
    [{timetrap,{timetrap_utils,timetrap_val,[500]}}];
group(pg7) ->
    [{timetrap,fun() -> ct:sleep(1000),exit(kaboom) end}];
group(pg8) ->
    [{timetrap,{timetrap_utils,timetrap_val,[500]}}];
group(pg9) ->
    [{timetrap,fun() -> ct:sleep(1000),exit(kaboom) end}];
group(pg10) ->
    [{timetrap,fun() -> ct:sleep(1000),exit(kaboom) end}];
group(pg11) ->
    [];
group(sg1) ->
    [{timetrap,fun() -> ct:sleep(1000),exit(kaboom) end}];
group(sg2) ->
    [{timetrap,{timetrap_utils,timetrap_val,[{seconds,1}]}}].


%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
     {group,g0},
     {group,g1},
     {group,g2},
     {group,g3},
     {group,g4},
     {group,g5},
     {group,g6},
     {group,g7},
     {group,g8},
     {group,g9},
     {group,g10},
     {group,g11},
     {group,pg0},
     {group,pg1},
     {group,pg2},
     {group,pg3},
     {group,pg4},
     {group,pg5},
     {group,pg6},
     {group,pg7},
     {group,pg8},
     {group,pg9},
     {group,pg10},
     {group,pg11},
     {group,sg1},
     {group,sg2}].



tc0(_) ->
    ct:comment("TO set by group"),
    ct:sleep({seconds,5}),
    ok.

tc1() ->
    [{timetrap,{timetrap_utils,timetrap_val,[1000]}}].
tc1(_) ->
    ct:comment("TO after 1 sec"),
    ct:sleep({seconds,2}),
    ok.

tc2() ->
    [{timetrap,fun() -> timetrap_utils:timetrap_val(500) end}].
tc2(_) ->
    ct:comment("TO after 0.5 sec"),
    ct:sleep({seconds,2}),
    ok.

tc3(_) ->
    ct:comment(io_lib:format("TO after ~w sec", [?TO])),
    ct:sleep({seconds,5}),
    ok.

tc4(_) ->
    ct:comment("TO set by group"),
    ct:sleep({seconds,5}),
    ok.

tc5(_) ->
    ct:comment("No TO in this testcase, maybe later"),
    ok.
