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

init_per_testcase1(tc4, Config) ->
    [{tc,tc4},{default_timeout,5000}|Config];

init_per_testcase1(TC, Config) ->
    [{tc,TC}|Config].

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
    ct:sleep(2000);

end_per_testcase1(tc3, Config) ->
    ct:pal("end_per_testcase(tc3): ~p", [Config]),
    tc3 = ?config(tc, Config),
    {failed,{testcase_aborted,testing_end_conf}} = ?config(tc_status, Config),
    ok;

end_per_testcase1(tc4, Config) ->
    ct:pal("end_per_testcase(tc4): ~p", [Config]),
    tc4 = ?config(tc, Config),
    {failed,{testcase_aborted,testing_end_conf}} = ?config(tc_status, Config),
    ct:sleep(2000);

end_per_testcase1(tc5, Config) ->
    ct:pal("end_per_testcase(tc5): ~p", [Config]),
    tc5 = ?config(tc, Config),
    exit(end_per_tc_fail_after_timeout);

end_per_testcase1(tc6, Config) ->
    ct:pal("end_per_testcase(tc6): ~p", [Config]),
    tc6 = ?config(tc, Config),
    exit(end_per_tc_fail_after_abort);

end_per_testcase1(tc7, Config) ->
    ct:pal("end_per_testcase(tc7): ~p", [Config]),
    tc7 = ?config(tc, Config),
    {failed,timetrap_timeout} = ?config(tc_status, Config),
    ok;

end_per_testcase1(tc8, Config) ->
    ct:pal("end_per_testcase(tc8): ~p", [Config]),
    tc8 = ?config(tc, Config),
    {failed,timetrap_timeout} = ?config(tc_status, Config),
    ok;

end_per_testcase1(tc9, Config) ->
    ct:pal("end_per_testcase(tc9): ~p", [Config]),
    tc9 = ?config(tc, Config),
    %% check that it's possible to send and receive synchronously
    %% with the group leader process for end_per_testcase
    test_server:stop_node(dummy@somehost),
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
    [tc1, tc2, tc3, tc4, tc5, tc6, tc7, tc8, tc9].

tc1(_) ->
    ct:sleep(2000),
    ok.

tc2(_) ->
    ct:sleep(2000).

tc3(_) ->
    spawn(ct, abort_current_testcase, [testing_end_conf]),
    ct:sleep(2000),
    ok.

tc4(_) ->
    spawn(ct, abort_current_testcase, [testing_end_conf]),
    ct:sleep(2000),
    ok.

tc5(_) ->
    ct:sleep(2000),
    ok.

tc6(_) ->
    spawn(ct, abort_current_testcase, [testing_end_conf]),
    ct:sleep(2000).

tc7(_) ->
    sleep(2000),
    ok.

tc8(_) ->
    timetrap_helper:sleep(2000),
    ok.

tc9(_) ->
    sleep(2000),
    ok.

%%%-----------------------------------------------------------------
sleep(T) ->
    ct:sleep(T),
    ok.
