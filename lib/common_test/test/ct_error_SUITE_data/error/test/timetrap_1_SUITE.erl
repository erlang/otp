%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
-module(timetrap_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,1}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    TabPid = spawn(fun() ->
			   ets:new(?MODULE, [named_table, set, public]),
			   ets:insert(?MODULE, {last_case,ok}),
			   receive _ -> ok end
		   end),
    [{tab,TabPid} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    exit(?config(tab, Config), kill),
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
init_per_testcase(TC, Config) ->
    {_,_} = process_info(?config(tab, Config), priority),
    [{_,ok}] = ets:lookup(?MODULE, last_case),
    ets:insert(?MODULE, {last_case,fail}),
    init_per_testcase1(TC, Config).

init_per_testcase1(tc1, Config) ->
    [{tc,tc1}|Config];

init_per_testcase1(tc2, Config) ->
    [{tc,tc2}|Config];

init_per_testcase1(tc3, Config) ->
    [{tc,tc3}|Config];

init_per_testcase1(tc4, Config) ->
    [{tc,tc4},{default_timeout,5000}|Config];

init_per_testcase1(tc5, Config) ->
    [{tc,tc5}|Config];

init_per_testcase1(tc6, Config) ->
    [{tc,tc6}|Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_testcase(TC, Config) ->
    {_,_} = process_info(?config(tab, Config), priority),
    [{_,fail}] = ets:lookup(?MODULE, last_case),
    ets:insert(?MODULE, {last_case,ok}),
    end_per_testcase1(TC, Config).

end_per_testcase1(tc1, Config) ->
    ct:pal("end_per_testcase(tc1): ~p", [Config]),
    tc1 = ?config(tc, Config),
    {failed,timetrap_timeout} = ?config(tc_status, Config),
    ok;

end_per_testcase1(tc2, Config) ->
    ct:pal("end_per_testcase(tc2): ~p", [Config]),
    tc2 = ?config(tc, Config),
    {failed,timetrap_timeout} = ?config(tc_status, Config),
    timer:sleep(2000);

end_per_testcase1(tc3, Config) ->
    ct:pal("end_per_testcase(tc3): ~p", [Config]),
    tc3 = ?config(tc, Config),
    {failed,{testcase_aborted,testing_end_conf}} = ?config(tc_status, Config),
    ok;

end_per_testcase1(tc4, Config) ->
    ct:pal("end_per_testcase(tc4): ~p", [Config]),
    tc4 = ?config(tc, Config),
    {failed,{testcase_aborted,testing_end_conf}} = ?config(tc_status, Config),
    timer:sleep(2000);

end_per_testcase1(tc5, Config) ->
    ct:pal("end_per_testcase(tc5): ~p", [Config]),
    tc5 = ?config(tc, Config),
    exit(end_per_tc_fail_after_timeout);

end_per_testcase1(tc6, Config) ->
    ct:pal("end_per_testcase(tc6): ~p", [Config]),
    tc6 = ?config(tc, Config),
    exit(end_per_tc_fail_after_abort).

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
    [tc1, tc2, tc3, tc4, tc5, tc6].

tc1(_) ->
    timer:sleep(2000).

tc2(_) ->
    timer:sleep(2000).

tc3(_) ->
    spawn(ct, abort_current_testcase, [testing_end_conf]),
    timer:sleep(2000).

tc4(_) ->
    spawn(ct, abort_current_testcase, [testing_end_conf]),
    timer:sleep(2000).

tc5(_) ->
    timer:sleep(2000).

tc6(_) ->
    spawn(ct, abort_current_testcase, [testing_end_conf]),
    timer:sleep(2000).
