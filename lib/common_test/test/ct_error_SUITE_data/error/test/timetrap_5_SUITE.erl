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
-module(timetrap_5_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TO, 1).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, fun() -> timetrap_utils:timetrap_val({seconds,?TO}) end}].

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
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [tc0,tc1,tc2,tc3,tc4,tc5,tc6,tc7,tc8,tc9,
     tc10,tc11,tc12,tc13,tc14].

tc0(_) ->
    ct:comment(io_lib:format("TO after ~w sec", [?TO])),
    ct:sleep({seconds,5}),
    ok.

tc1() ->
    [{timetrap,{timetrap_utils,timetrap_exit,[kaboom]}}].
tc1(_) ->
    exit(this_should_not_execute).

tc2() ->
    [{timetrap,fun() -> exit(kaboom) end}].
tc2(_) ->
    exit(this_should_not_execute).

tc3() ->
    [{timetrap,{timetrap_utils,timetrap_val,[{seconds,2}]}}].
tc3(_) ->  
    ct:comment("TO after ~2 sec"),
    ct:sleep({seconds,10}),
    ok.

tc4() ->
    [{timetrap,fun() -> 500 end}].
tc4(_) ->  
    ct:comment("TO after 500 ms"),
    ct:sleep({seconds,10}),
    ok.

tc5() ->
   [{timetrap,{timetrap_utils,timetrap_timeout,[1000,ok]}}].
tc5(_) ->    
    ct:comment("TO after ~1 sec"),
    ct:sleep({seconds,10}),
    ok.

tc6() ->
    [{timetrap,{timetrap_utils,timetrap_timeout,[{seconds,40},
						 {seconds,1}]}}].
tc6(_) ->    
    ct:comment("TO after 40+1 sec"),
    ct:sleep({seconds,42}),
    ok.

tc7() ->
    [{timetrap,{timetrap_utils,timetrap_timeout,[1000,2000]}}].
tc7(_) ->
    ct:comment("TO after ~3 sec"),
    ct:sleep({seconds,10}),
    ok.

tc8() ->
    [{timetrap,fun() -> ct:sleep(6000), 1000 end}].
tc8(_) ->
    ct:comment("TO after 6+1 sec"),
    ct:sleep({seconds,10}),
    ok.

tc9() ->
    [{timetrap,{timetrap_utils,timetrap_timeout,
		[500,fun() -> {seconds,2} end]}}].
tc9(_) ->
    ct:comment("TO after ~2 sec (2.5 sec in reality)"),
    ct:sleep({seconds,10}),
    ok.

tc10() ->
    [{timetrap,500}].
tc10(_) ->
    ct:timetrap({timetrap_utils,timetrap_val,[1500]}),   
    ct:comment("TO after ~1.5 sec"),
    ct:sleep({seconds,10}),
    ok.

tc11() ->
    [{timetrap,2000}].
tc11(_) ->
    ct:timetrap(fun() -> 1500 end),
    ct:comment("TO after ~1.5 sec"),
    ct:sleep({seconds,10}),
    ok.
    
tc12() ->
    [{timetrap,500}].
tc12(_) ->
    ct:timetrap({timetrap_utils,timetrap_timeout,[1000,ok]}),   
    ct:comment("TO after ~1 sec"),
    ct:sleep({seconds,10}),
    ok.

tc13() ->
    [{timetrap,2000}].
tc13(_) ->
    ct:timetrap(fun() -> ct:sleep(500), ok end),
    ct:comment("TO after ~500 ms"),
    ct:sleep({seconds,10}),
    ok.

tc14(_) ->
    ct:comment(io_lib:format("TO after ~w sec", [?TO])),
    ct:sleep({seconds,5}),
    ok.
