%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
    [tc0,tc1,tc2,tc3,tc4,tc5,tc6,tc7].

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
    [{timetrap,{timetrap_utils,timetrap_err_mfa,[]}}].
tc3(_) ->
    exit(this_should_not_execute).

tc4() ->
    [{timetrap,fun() -> timetrap_utils:timetrap_err_fun() end}].
tc4(_) ->
    exit(this_should_not_execute).

tc5() ->
    [{timetrap,{timetrap_utils,timetrap_timeout,[{seconds,40},
						 {seconds,1}]}}].
tc5(_) ->    
    ct:comment("TO after 40+1 sec"),
    ct:sleep({seconds,42}),
    ok.

tc6() ->
    [{timetrap,fun() -> ct:sleep(6000), 1000 end}].
tc6(_) ->
    ct:comment("TO after 6+1 sec"),
    ct:sleep({seconds,10}).

tc7(_) ->
    ct:comment(io_lib:format("TO after ~w sec", [?TO])),
    ct:sleep({seconds,5}),
    ok.
