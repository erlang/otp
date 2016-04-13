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

%%%-------------------------------------------------------------------
%%% File: ct_misc_1_SUITE
%%%
%%% Description:
%%% Test misc things in Common Test suites.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_misc_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config1 = ct_test_support:init_per_suite(Config),
    Config1.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [beam_me_up, {group,parse_table}].

groups() -> 
    [{parse_table,[parallel], 
      [parse_table_empty, parse_table_single,
       parse_table_multiline_row,
       parse_table_one_column_multiline,
       parse_table_one_column_simple]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
beam_me_up(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    CTNode = ?config(ct_node, Config),

    %% Path = rpc:call(CTNode, code, get_path, []),
    %% [_ | Parts] = lists:reverse(filename:split(DataDir)),
    %% TSDir = filename:join(lists:reverse(Parts)),
    %% true = rpc:call(CTNode, code, del_path, [TSDir]),

    Mods = [beam_1_SUITE, beam_2_SUITE],
    Suites = [atom_to_list(M) || M <- Mods],
    [{error,_} = rpc:call(CTNode, code, load_file, [M]) || M <- Mods],

    code:add_path(DataDir),
    CRes =
	[compile:file(filename:join(DataDir,F),
		      [verbose,report_errors,
		       report_warnings,binary]) || F <- Suites],

    [{module,_} = rpc:call(CTNode, code, load_binary,
			   [Mod, atom_to_list(Mod), Bin]) ||
	{ok,Mod,Bin} <- CRes],

    {Opts,ERPid} = setup([{suite,Suites},{auto_compile,false}], Config),

    {_Ok,_Fail,_Skip} = ct_test_support:run(ct, run_test, [Opts], Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(beam_me_up,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(beam_me_up, 1),
    ok = ct_test_support:verify_events(TestEvents, Events, Config).

parse_table_empty(Config) when is_list(Config) ->

    String = ["+----+-------+---------+---------+----------+------+--------+",
	      "| id | col11 | col2222 | col3333 | col4     | col5 | col6666 |",
	      "+----+-------+---------+---------+----------+------+--------+",
	      "+----+-------+---------+---------+----------+------+--------+",
	      "Query Done: 0 records selected"],

    {{"id","col11","col2222","col3333","col4","col5","col6666"},[]} =
	ct:parse_table(String).


parse_table_single(Config) when is_list(Config) ->

    String = ["+------+--------+--------------+------------+------------------+---------+--------+---------+-----------+",
	      "| id | col1 | col2 | col3 | col4 | col5 | col6 | col7 | col8 |",
"+------+--------+--------------+------------+------------------+---------+--------+---------+-----------+",
	      "| 0 | 0 | -1407231560 | -256 | -1407231489 | 1500 | 1 | 1 | 1 |",
	      "+------+--------+--------------+------------+------------------+---------+--------+---------+-----------+"
	      "Query Done: 1 record selected"],

    {{"id","col1","col2","col3","col4","col5","col6","col7","col8"},
     [{"0","0","-1407231560","-256","-1407231489", "1500","1","1","1"}]} =
	ct:parse_table(String).

parse_table_multiline_row(Config) when is_list(Config) ->
    
    String = ["+------+--------+--------------+------------+------------------+---------+--------+---------+-----------+",
	      "| id | col1 | col2 | col3 | col4 | col5 | col6 | col7 | col8 |",
"+------+--------+--------------+------------+------------------+---------+--------+---------+-----------+",
	      "| 0 | 0 | Free test string",
	      " on more lines",
	      "than one",
	      "| -256 | -1407231489 | 1500 | 1 | 1 | 1 |",
	      "+------+--------+--------------+------------+------------------+---------+--------+---------+-----------+"
	      "Query Done: 1 record selected"],

    {{"id","col1","col2","col3","col4","col5","col6","col7","col8"},
     [{"0","0","Free test string\n on more lines\nthan one\n",
       "-256","-1407231489", "1500","1","1","1"}]} =
	ct:parse_table(String).

parse_table_one_column_simple(Config) when is_list(Config) ->

    String = ["|test|","|test value|"],

    {{"test"},[{"test value"}]} = ct:parse_table(String).

parse_table_one_column_multiline(Config) when is_list(Config) ->
    String = ["|test|","|test","value|"],

    {{"test"},[{"test\nvalue"}]} = ct:parse_table(String).
	
%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}}|Test],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).
%reformat(Events, _EH) ->
%    Events.

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
events_to_check(Test) ->
    %% 2 tests (ct:run_test + script_start) is default
    events_to_check(Test, 2).

events_to_check(_, 0) ->
    [];
events_to_check(Test, N) ->
    test_events(Test) ++ events_to_check(Test, N-1).

test_events(beam_me_up) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{2,2,4}},
     {?eh,tc_start,{beam_1_SUITE,init_per_suite}},
     {?eh,tc_done,{beam_1_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{beam_1_SUITE,tc1}},
     {?eh,tc_done,{beam_1_SUITE,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{beam_1_SUITE,tc2}},
     {?eh,tc_done,{beam_1_SUITE,tc2,{failed,{error,'tc2 failed'}}}},
     {?eh,test_stats,{1,1,{0,0}}},
     {?eh,tc_start,{beam_1_SUITE,end_per_suite}},
     {?eh,tc_done,{beam_1_SUITE,end_per_suite,ok}},
     {?eh,tc_start,{beam_2_SUITE,init_per_suite}},
     {?eh,tc_done,{beam_2_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{beam_2_SUITE,tc1}},
     {?eh,tc_done,{beam_2_SUITE,tc1,ok}},
     {?eh,test_stats,{2,1,{0,0}}},
     {?eh,tc_start,{beam_2_SUITE,tc2}},
     {?eh,tc_done,{beam_2_SUITE,tc2,{failed,{error,'tc2 failed'}}}},
     {?eh,test_stats,{2,2,{0,0}}},
     {?eh,tc_start,{beam_2_SUITE,end_per_suite}},
     {?eh,tc_done,{beam_2_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
