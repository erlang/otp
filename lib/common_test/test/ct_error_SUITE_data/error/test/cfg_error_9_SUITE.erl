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
-module(cfg_error_9_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,2}}].

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
end_per_suite(Config) ->
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
    exit(tc1_should_be_skipped),
    Config;
init_per_testcase(tc2, Config) ->
    ct:comment("init_per_testcase(tc2) timeout"),
    timer:sleep(5000),
    Config;
init_per_testcase(tc3, Config) ->
    badmatch = ?config(void, Config),
    Config;
init_per_testcase(tc4, _) ->
    ok;
init_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_testcase(tc11, _Config) ->
    ct:comment("A warning should be printed"),
    exit(warning_should_be_printed),
    done;
end_per_testcase(tc12, _Config) ->
    ct:comment("A warning should be printed"),
    timer:sleep(5000),
    done;
end_per_testcase(tc13, Config) ->
    ct:comment("A warning should be printed"),
    badmatch = ?config(void, Config),
    done;
end_per_testcase(tc14, Config) ->
    ok = ?config(tc_status, Config),
    {fail,tc14_should_be_failed};
end_per_testcase(tc15, Config) ->
    {failed,byebye} = ?config(tc_status, Config),
    ok;
end_per_testcase(_TestCase, _Config) ->
    done.

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
    [tc1,tc2,tc3,tc4,tc5,tc6,
     tc11,tc12,tc13,tc14].

tc1(_) ->
    fini.
tc2(_) ->
    fini.
tc3(_) ->
    fini.
tc4(_) ->
    fini.


tc5() ->
    put('$test_server_framework_test',
	fun(init_tc, _Default) -> {fail,fail_this_testcase};
	   (_, Default) -> Default
	end),
    [].
		
tc5(_) ->
    ct:comment("This one should get failed by init_tc!"),
    fini.

tc6() ->
    put('$test_server_framework_test',
	fun(end_tc, _Default) -> {fail,fail_this_testcase};
	   (_, Default) -> Default
	end),
    [].

tc6(_) ->
    ct:comment("This one should succeed but then get failed by end_tc!"),
    fini.

tc11(_) ->
    fini.
tc12(_) ->
    ct:comment("It's ok if this doesn't get printed"),
    fini.
tc13(_) ->
    fini.
tc14(_) ->
    ct:comment("This one should be failed by eptc"),
    yes.
tc15(_) ->
    exit(byebye).
