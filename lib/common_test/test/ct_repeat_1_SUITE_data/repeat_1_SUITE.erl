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
-module(repeat_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    spawn(fun() -> db() end),
    Config.
%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    db(stop, ok),
    ok.

db() ->
    register(?MODULE, self()),
    db_loop([]).

db_loop(Dict) ->
    receive
	{insert,From,Key,Val} ->
	    From ! {?MODULE,ok},
	    db_loop([{Key,Val} | proplists:delete(Key, Dict)]);
	{lookup,From,Key}  ->
	    From ! {?MODULE,proplists:get_value(Key, Dict)},
	    db_loop(Dict);
	{delete,From,Key} ->
	    From ! {?MODULE,ok},
	    db_loop(proplists:delete(Key, Dict));
	{stop,From,_} ->
	    From ! {?MODULE,ok}
    end.

 db(Op, Key, Val) ->
    ?MODULE ! {Op,self(),Key,Val},
    receive {?MODULE,Result} -> Result end.

 db(Op, Key) ->
    ?MODULE ! {Op,self(),Key},
    receive {?MODULE,Result} -> Result end.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(G, Config) when G == gr_ok_1 ; G == gr_ok_2 ;
			       G == gr_fail_result;
			       G == gr_ok_then_fail_result ->
    ct:comment(G),
    Config;

init_per_group(G, _Config) when G == gr_fail_init ->
    ct:comment(G),
    exit(fails_on_purpose);

init_per_group(G, Config) when G == gr_ok_then_fail_init ->
    ct:comment(G),
    do_2nd_time(G,
		fun() -> exit(failing_this_time) end,
		fun() -> Config end);

init_per_group(G, Config) when G == gr_fail_init_then_ok ->
    ct:comment(G),
    do_2nd_time(G,
		fun() -> Config end,
		fun() -> exit(failing_this_time) end);

init_per_group(G, Config) ->
    ct:comment(G),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(G, _Config) when G == gr_fail_result ->
    ct:comment(G),
    {return_group_result,failed};

end_per_group(G, _Config) when G == gr_ok_then_fail_result ->
    ct:comment(G),
    do_2nd_time(G,
		fun() -> {return_group_result,failed} end,
		fun() -> ok end);

end_per_group(G, _Config) when G == gr_fail_result_then_ok ->
    ct:comment(G),
    do_2nd_time(G,
		fun() -> ok end,
		fun() -> {return_group_result,failed} end);

end_per_group(G, _Config) ->
    ct:comment(G),
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
    [
     %%---------------------------------------------------------------
     {repeat_cs, [], [{group,repeat_cs_0},
		      {group,repeat_cs_1},
		      {group,repeat_cs_2}]},
     {repeat_cs_0, [{repeat,0}], [tc_ok_1,tc_ok_2]},
     {repeat_cs_1, [{repeat,1}], [tc_ok_1,tc_ok_2]},
     {repeat_cs_2, [{repeat,2}], [tc_ok_1,tc_ok_2]},

     {repeat_cs_and_grs, [{repeat,2}], [{group,gr_ok_1},tc_fail_1,
					{group,gr_fail_result},tc_ok_1,
					{group,gr_fail_init},tc_ok_2]},

     %%---------------------------------------------------------------
     {repeat_seq, [], [{group,repeat_seq_1},
		       {group,repeat_seq_2},
		       {group,repeat_seq_3},
		       {group,repeat_seq_4}]},
     {repeat_seq_1, [sequence,{repeat,2}], [tc_ok_1,tc_fail_1,tc_ok_2]},
     {repeat_seq_2, [sequence,{repeat,2}], [tc_ok_1,{group,gr_fail_result},tc_ok_2]},
     {repeat_seq_3, [sequence,{repeat,2}], [tc_ok_1,{group,gr_fail_init},tc_ok_2]},
     {repeat_seq_4, [sequence,{repeat,2}], [tc_fail_1,{group,gr_ok_1},tc_ok_1]},

     %%---------------------------------------------------------------
     {repeat_cs_until_any_ok, [], [{group,repeat_cs_until_any_ok_1},
				   {group,repeat_cs_until_any_ok_2}]},
     {repeat_cs_until_any_ok_1, [{repeat_until_any_ok,3}], [tc_fail_1,
							    tc_fail_2,
							    tc_fail_then_ok_1]},
     {repeat_cs_until_any_ok_2, [{repeat_until_any_ok,3}], [tc_ok_1,tc_fail_1]},

     %%---------------------------------------------------------------
     {repeat_gr_until_any_ok, [], [{group,repeat_gr_until_any_ok_1},
				   {group,repeat_gr_until_any_ok_2}]},
     {repeat_gr_until_any_ok_1, [{repeat_until_any_ok,3}],
      [{group,gr_fail_result}, tc_fail_1, {group,gr_fail_init}, tc_fail_2,
       {group,gr_fail_result_then_ok}]},
     {repeat_gr_until_any_ok_2, [{repeat_until_any_ok,3}],
      [{group,gr_fail_result}, tc_fail_1, tc_fail_then_ok_1,
       {group,gr_fail_init}]},

     %%---------------------------------------------------------------
     {repeat_cs_until_any_fail, [], [{group,repeat_cs_until_any_fail_1},
				     {group,repeat_cs_until_any_fail_2}]},
     {repeat_cs_until_any_fail_1, [{repeat_until_any_fail,3}], [tc_ok_1,
								tc_ok_2,
								tc_ok_then_fail_1]},
     {repeat_cs_until_any_fail_2, [{repeat_until_any_fail,3}], [tc_fail_1,tc_fail_2]},

     %%---------------------------------------------------------------
     {repeat_gr_until_any_fail, [], [{group,repeat_gr_until_any_fail_1},
				     {group,repeat_gr_until_any_fail_2},
				     {group,repeat_gr_until_any_fail_3}]},
     {repeat_gr_until_any_fail_1, [{repeat_until_any_fail,3}],
      [{group,gr_ok_1}, tc_ok_1, {group,gr_ok_then_fail_result}, tc_ok_2]},
     {repeat_gr_until_any_fail_2, [{repeat_until_any_fail,3}],
      [{group,gr_ok_1}, tc_ok_1, {group,gr_ok_then_fail_init}, tc_ok_2]},
     {repeat_gr_until_any_fail_3, [{repeat_until_any_fail,3}], [tc_ok_then_fail_1,
								{group,gr_ok_1},
								tc_ok_1]},

     %%---------------------------------------------------------------
     {repeat_cs_until_all_ok, [], [{group,repeat_cs_until_all_ok_1},
				   {group,repeat_cs_until_all_ok_2}]},
     {repeat_cs_until_all_ok_1, [{repeat_until_all_ok,3}], [tc_fail_then_ok_1,
							    tc_ok_1,
							    tc_fail_then_ok_2]},
     {repeat_cs_until_all_ok_2, [{repeat_until_all_ok,3}], [tc_ok_1,tc_ok_2]},

     %%---------------------------------------------------------------
     {repeat_gr_until_all_ok, [], [{group,repeat_gr_until_all_ok_1},
				   {group,repeat_gr_until_all_ok_2},
				   {group,repeat_gr_until_all_ok_3}]},
     {repeat_gr_until_all_ok_1, [{repeat_until_all_ok,3}],
      [tc_ok_1, {group,gr_ok_1}, tc_fail_then_ok_1, {group,gr_fail_result_then_ok}]},
     {repeat_gr_until_all_ok_2, [{repeat_until_all_ok,3}],
      [{group,gr_fail_init_then_ok}, tc_ok_1]},
     {repeat_gr_until_all_ok_3, [{repeat_until_all_ok,3}],
      [{group,gr_ok_1}, tc_fail_then_ok_1]},

     %%---------------------------------------------------------------
     {repeat_cs_until_all_fail, [], [{group,repeat_cs_until_all_fail_1},
				     {group,repeat_cs_until_all_fail_2}]},
     {repeat_cs_until_all_fail_1, [{repeat_until_all_fail,3}], [tc_ok_then_fail_1,
								tc_fail_1,
								tc_ok_then_fail_2]},
     {repeat_cs_until_all_fail_2, [{repeat_until_all_fail,3}], [tc_fail_1]},

     %%---------------------------------------------------------------
     {repeat_gr_until_all_fail, [], [{group,repeat_gr_until_all_fail_1},
				     {group,repeat_gr_until_all_fail_2},
				     {group,repeat_gr_until_all_fail_3}]},
     {repeat_gr_until_all_fail_1, [{repeat_until_all_fail,3}],
      [tc_fail_1, {group,gr_fail_init}, tc_ok_then_fail_1, {group,gr_ok_then_fail_result}]},
     {repeat_gr_until_all_fail_2, [{repeat_until_all_fail,3}],
      [{group,gr_ok_then_fail_init}, tc_fail_1]},
     {repeat_gr_until_all_fail_3, [{repeat_until_all_fail,3}],
      [{group,gr_fail_result}, tc_ok_then_fail_1]},

     %%---------------------------------------------------------------
     {repeat_seq_until_any_fail, [], [{group,repeat_seq_until_any_fail_1},
				      {group,repeat_seq_until_any_fail_2},
				      {group,repeat_seq_until_any_fail_3},
				      {group,repeat_seq_until_any_fail_4},
				      {group,repeat_seq_until_any_fail_5}]},
     {repeat_seq_until_any_fail_1, [sequence,{repeat_until_any_fail,2}],
      [tc_ok_1, tc_ok_2]},
     {repeat_seq_until_any_fail_2, [{repeat_until_any_fail,2},sequence],
      [tc_ok_1, {group,gr_ok_1}, tc_ok_2]},
     {repeat_seq_until_any_fail_3, [sequence,{repeat_until_any_fail,3}],
      [tc_ok_1, tc_ok_then_fail_1, tc_ok_2, {group,gr_ok_1}]},
     {repeat_seq_until_any_fail_4, [{repeat_until_any_fail,3},sequence],
      [{group,gr_ok_then_fail_result}, {group,gr_ok_1}, tc_ok_1]},
     {repeat_seq_until_any_fail_5, [{repeat_until_any_fail,3},sequence],
      [{group,gr_ok_1}, {group,gr_ok_then_fail_init}, {group,gr_ok_2}, tc_ok_1]},

     %%---------------------------------------------------------------
     {repeat_shuffled_seq_until_any_fail, [], [{group,repeat_shuffled_seq_until_any_fail_1},
					       {group,repeat_shuffled_seq_until_any_fail_2},
					       {group,repeat_shuffled_seq_until_any_fail_3},
					       {group,repeat_shuffled_seq_until_any_fail_4},
					       {group,repeat_shuffled_seq_until_any_fail_5}]},
     {repeat_shuffled_seq_until_any_fail_1, [sequence,shuffle,{repeat_until_any_fail,2}],
      [tc_ok_1, tc_ok_2]},
     {repeat_shuffled_seq_until_any_fail_2, [{repeat_until_any_fail,2},{shuffle,{1,2,3}},sequence],
      [tc_ok_1, {group,gr_ok_1}, tc_ok_2]},
     {repeat_shuffled_seq_until_any_fail_3, [shuffle,sequence,{repeat_until_any_fail,3}],
      [tc_ok_1, tc_ok_then_fail_1, tc_ok_2, {group,gr_ok_1}]},
     {repeat_shuffled_seq_until_any_fail_4, [{repeat_until_any_fail,3},sequence,{shuffle,{1,2,3}}],
      [{group,gr_ok_then_fail_result}, {group,gr_ok_1}, tc_ok_1]},
     {repeat_shuffled_seq_until_any_fail_5, [{repeat_until_any_fail,3},sequence,{shuffle,{1,2,3}}],
      [{group,gr_ok_1}, {group,gr_ok_then_fail_init}, {group,gr_ok_2}, tc_ok_1]},

     %%---------------------------------------------------------------
     {gr_ok_1, [], [tc_ok_1]},

     {gr_ok_2, [], [tc_ok_1]},

     {gr_fail_init, [], [tc_ok_1]},

     {gr_fail_result, [], [tc_ok_1]},

     {gr_ok_then_fail_init, [], [tc_ok_1]},

     {gr_ok_then_fail_result, [], [tc_ok_1]},

     {gr_fail_result_then_ok, [], [tc_ok_1]},

     {gr_fail_init_then_ok, [], [tc_ok_1]}
    ].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [].

tc_ok_1(_) ->
    ok.

tc_ok_2(_) ->
    ok.

tc_fail_1(_) ->
    x=2.

tc_fail_2(_) ->
    exit(exit_on_purpose).

tc_ok_then_fail_1(_) ->
    do_2nd_time(tc_ok_then_fail_1,
		fun() -> exit(failing_this_time) end,
		fun() -> ok end),
    ok.

tc_ok_then_fail_2(_) ->
    do_2nd_time(tc_ok_then_fail_2,
		fun() -> exit(failing_this_time) end,
		fun() -> ok end),
    ok.

tc_fail_then_ok_1(_) ->
    do_2nd_time(tc_fail_then_ok_1,
		fun() -> ok end,
		fun() -> exit(failing_this_time) end),
    ok.

tc_fail_then_ok_2(_) ->
    do_2nd_time(tc_fail_then_ok_2,
		fun() -> ok end,
		fun() -> exit(failing_this_time) end),
    ok.

do_2nd_time(Case, True, False) ->
    case db(lookup, Case) of
	undefined ->
	    db(insert, Case, 1),
	    False();
	1 ->
	    ct:log("This is the second call...", []),
	    db(delete, Case),
	    True()
    end.
