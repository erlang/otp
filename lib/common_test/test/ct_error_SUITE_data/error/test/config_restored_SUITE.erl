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
-module(config_restored_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,1000}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config1 = [{init_per_suite,?MODULE} | Config],
    TabPid = spawn(fun() ->
			   ets:new(?MODULE, [named_table, set, public]),
			   receive _ -> ok end
		   end),
    [{tab,TabPid} | Config1].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    ets:delete(?MODULE),
    exit(?config(tab, Config), kill),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
    [{init_per_group,GroupName} | Config].

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
    Config1 = [{init_per_testcase,TC} | Config],
    ets:insert(?MODULE, {config,Config}),
    %% ct:pal("Config after init_per_testcase(~w) = ~p", [TC,Config1]),
    Config1.
   
%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_testcase(TC, Config) ->
    ct:pal("Config in end_per_testcase(~w) = ~p", [TC,Config]),
    [{_,MemConfig}] = ets:lookup(?MODULE, config), 
    diff_config(Config, MemConfig, [tc_status]),
    ?MODULE = proplists:get_value(init_per_suite, Config),
    TC = proplists:get_value(init_per_testcase, Config),
    case ?config(tc_group_properties, Config) of
	undefined ->
	    ok;
	Props ->
	    GName = proplists:get_value(name, Props),
	    GName = proplists:get_value(init_per_group, Config)
    end,
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
    [{g1,[],[to_tc, exit_tc]}].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [to_tc, exit_tc,
     {group,g1}].


to_tc(Config) ->
    %% ct:pal("Config for to_tc = ~p", [Config]),
    [{_,MemConfig}] = ets:lookup(?MODULE, config), 
    diff_config(Config, MemConfig, []),
    ct:sleep(2000).

exit_tc(Config) ->
    %% ct:pal("Config for exit_tc = ~p", [Config]),
    [{_,MemConfig}] = ets:lookup(?MODULE, config), 
    diff_config(Config, MemConfig, []),
    ct:fail("Goodbye!").
    
    
%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS

diff_config(Cfg1, Cfg2, DiffKeys) ->
    diff_config(Cfg1, Cfg2, DiffKeys, []).

diff_config([{K,V} | Cfg1], Cfg2, DiffKeys, RemKeys) ->
    case proplists:get_value(K, Cfg2) of
	undefined ->
	    diff_config(Cfg1, Cfg2, proplists:delete(K, DiffKeys), RemKeys);
	V ->
	    diff_config(Cfg1, proplists:delete(K, Cfg2), DiffKeys, RemKeys);
	_Other ->
	    case proplists:is_defined(K, DiffKeys) of
		true ->
		    diff_config(Cfg1, Cfg2, proplists:delete(K, DiffKeys), RemKeys);
		false ->
		    diff_config(Cfg1, Cfg2, DiffKeys, [K | RemKeys])
	    end
    end;
diff_config([], [], [], []) ->
    ct:pal("Diff ok!", []),
    ok;
diff_config([], Cfg2, DiffKeys, RemKeys) ->
    Result = {diff_failed, {cfg2,Cfg2}, {diffkeys,DiffKeys}, {remkeys,RemKeys}},
    ct:pal("Diff failed! Result = ~p", [Result]),
    exit(Result).

