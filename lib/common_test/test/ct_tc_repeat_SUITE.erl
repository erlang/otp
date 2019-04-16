%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2017. All Rights Reserved.
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
-module(ct_tc_repeat_SUITE).

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
    DataDir = ?config(data_dir, Config),
    ct_test_support:init_per_suite([{path_dirs,[DataDir]} | Config]).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).


suite() ->
    [{timetrap,{minutes,1}}].

all() ->
    all(suite).

all(suite) ->
      [
       repeat,
       repeat_parallel_until_ok,
       repeat_parallel_until_fail,
       repeat_sequence_until_ok,
       repeat_sequence_until_fail,
       pick_one_test_from_group,
       pick_one_test_from_subgroup
      ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
%% Test post_groups and post_all hook callbacks, introduced by OTP-14746
repeat(Config) ->
    ok = do_test(?FUNCTION_NAME, "tc_repeat_SUITE", [], [], Config).

repeat_parallel_until_ok(Config) ->
    {error,{{illegal_combination,{parallel,repeat_until_ok}},_}} =
        do_test(?FUNCTION_NAME, "tc_repeat_SUITE", [{group,g_parallel_until_ok}],
                [], Config, 1, []).

repeat_parallel_until_fail(Config) ->
    {error,{{illegal_combination,{parallel,repeat_until_fail}},_}} =
        do_test(?FUNCTION_NAME, "tc_repeat_SUITE", [{group,g_parallel_until_fail}],
                [], Config, 1, []).

repeat_sequence_until_ok(Config) ->
    {error,{{illegal_combination,{sequence,repeat_until_ok}},_}} =
        do_test(?FUNCTION_NAME, "tc_repeat_SUITE", [{group,g_sequence_until_ok}],
                [], Config, 1, []).

repeat_sequence_until_fail(Config) ->
    {error,{{illegal_combination,{sequence,repeat_until_fail}},_}} =
        do_test(?FUNCTION_NAME, "tc_repeat_SUITE", [{group,g_sequence_until_fail}],
                [], Config, 1, []).

pick_one_test_from_group(Config) ->
    do_test(?FUNCTION_NAME, "tc_repeat_SUITE", [{group,g_mixed},{testcase,tc2}],
            [], Config, 1, []).
    
pick_one_test_from_subgroup(Config) ->
    do_test(?FUNCTION_NAME, "tc_repeat_SUITE",
            [{group,[[g_mixed,subgroup]]},{testcase,tc2}],
            [], Config, 1, []).
    

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

do_test(Tag, Suite, WTT, CTHs, Config) ->
    do_test(Tag, Suite, WTT, CTHs, Config, 2, []).

do_test(Tag, Suite0, WTT, CTHs, Config, EC, ExtraOpts) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join([DataDir,Suite0]),
    {Opts,ERPid} =
        setup([{suite,Suite}|WTT]++[{ct_hooks,CTHs},{label,Tag}|ExtraOpts],
              Config),
    Res = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),
    %% io:format("~p~n",[Events]),

    ct_test_support:log_events(Tag,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(Tag, EC),
    ok = ct_test_support:verify_events(TestEvents, Events, Config),
    Res.

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}}|Test],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

gen_config(Name,KeyVals,Config) ->
    PrivDir = ?config(priv_dir,Config),
    File = filename:join(PrivDir,atom_to_list(Name)++".cfg"),
    ok = file:write_file(File,[io_lib:format("~p.~n",[{Key,Value}])
                               || {Key,Value} <- KeyVals]),
    File.

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

test_events(repeat) ->
    S = tc_repeat_SUITE,
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,unknown}},

     %% tc1, {repeat,2}
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     %% tc2, {repeat_until_ok,3}
     {?eh,tc_start,{S,tc2}},
     {?eh,tc_done,{S,tc2,ok}},
     {?eh,test_stats,{3,0,{0,0}}},
     %% tc3, {repeat_until_ok,3}
     {?eh,tc_start,{S,tc3}},
     {?eh,tc_done,{tc_repeat_SUITE,tc3,
                   {failed,{error,{test_case_failed,always_fail}}}}},
     {?eh,test_stats,{3,1,{0,0}}},
     {?eh,tc_start,{S,tc3}},
     {?eh,tc_done,{S,tc3,{failed,{error,{test_case_failed,always_fail}}}}},
     {?eh,test_stats,{3,2,{0,0}}},
     {?eh,tc_start,{S,tc3}},
     {?eh,tc_done,{S,tc3,{failed,{error,{test_case_failed,always_fail}}}}},
     {?eh,test_stats,{3,3,{0,0}}},
     %% tc4, {repeat_until_fail,3}
     {?eh,tc_start,{S,tc4}},
     {?eh,tc_done,{S,tc4,ok}},
     {?eh,test_stats,{4,3,{0,0}}},
     {?eh,tc_start,{S,tc4}},
     {?eh,tc_done,{S,tc4,{failed,{error,{test_case_failed,second_time_fail}}}}},
     {?eh,test_stats,{4,4,{0,0}}},
     %% g, tc1, {repeat,2}
     {?eh,tc_start,{S,{init_per_group,g,[]}}},
     {?eh,tc_done,{S,{init_per_group,g,[]},ok}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{5,4,{0,0}}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{6,4,{0,0}}},
     {?eh,tc_start,{S,{end_per_group,g,[]}}},
     {?eh,tc_done,{S,{end_per_group,g,[]},ok}},
     %% g_until_ok, tc2, {repeat_until_ok,3}
     {?eh,tc_start,{S,{init_per_group,g_until_ok,[]}}},
     {?eh,tc_done,{S,{init_per_group,g_until_ok,[]},ok}},
     {?eh,tc_start,{S,tc2}},
     {?eh,tc_done,{S,tc2,ok}},
     {?eh,test_stats,{7,4,{0,0}}},
     {?eh,tc_start,{S,{end_per_group,g_until_ok,[]}}},
     {?eh,tc_done,{S,{end_per_group,g_until_ok,[]},ok}},
     %% g_until_fail, tc4, {repeat_until_fail,3}
     {?eh,tc_start,{S,{init_per_group,g_until_fail,[]}}},
     {?eh,tc_done,{S,{init_per_group,g_until_fail,[]},ok}},
     {?eh,tc_start,{S,tc4}},
     {?eh,tc_done,{S,tc4,ok}},
     {?eh,test_stats,{8,4,{0,0}}},
     {?eh,tc_start,{S,tc4}},
     {?eh,tc_done,{S,tc4,{failed,{error,{test_case_failed,second_time_fail}}}}},
     {?eh,test_stats,{8,5,{0,0}}},
     {?eh,tc_start,{S,{end_per_group,g_until_fail,[]}}},
     {?eh,tc_done,{S,{end_per_group,g_until_fail,[]},ok}},
     %% g, parallel, tc1, {repeat,2}
     {parallel,
      [{?eh,tc_start,{S,{init_per_group,g,[parallel]}}},
       {?eh,tc_done,{S,{init_per_group,g,[parallel]},ok}},
       {?eh,tc_start,{S,tc1}},
       {?eh,tc_done,{S,tc1,ok}},
       {?eh,test_stats,{9,5,{0,0}}},
       {?eh,tc_start,{S,tc1}},
       {?eh,tc_done,{S,tc1,ok}},
       {?eh,test_stats,{10,5,{0,0}}},
       {?eh,tc_start,{S,{end_per_group,g,[parallel]}}},
       {?eh,tc_done,{S,{end_per_group,g,[parallel]},ok}}]},
     %% g, sequence, tc1, {repeat,2}
     {?eh,tc_start,{S,{init_per_group,g,[sequence]}}},
     {?eh,tc_done,{S,{init_per_group,g,[sequence]},ok}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{11,5,{0,0}}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{12,5,{0,0}}},
     {?eh,tc_start,{S,{end_per_group,g,[sequence]}}},
     {?eh,tc_done,{S,{end_per_group,g,[sequence]},ok}},
     %% g_sequence_skip_rest, 
     {?eh,tc_start,{S,{init_per_group,g_mixed,[sequence]}}},
     {?eh,tc_done,{S,{init_per_group,g_mixed,[sequence]},ok}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{13,5,{0,0}}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{14,5,{0,0}}},
     {?eh,tc_start,{S,tc4}},
     {?eh,tc_done,{S,tc4,ok}},
     {?eh,test_stats,{15,5,{0,0}}},
     {?eh,tc_start,{S,tc4}},
     {?eh,tc_done,{S,tc4,{failed,{error,{test_case_failed,second_time_fail}}}}},
     {?eh,test_stats,{15,6,{0,0}}},
     %% ----> fail in sequence, so skip rest
     {?eh,tc_auto_skip,{S,{tc4,g_mixed}, % last of current repeat tc4
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,1}}},
     {?eh,tc_auto_skip,{S,{tc1,g_mixed}, % single tc1
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,2}}},
     {?eh,tc_auto_skip,{S,{tc1,g}, % group g, tc1, {repeat,2}
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,3}}},
     {?eh,tc_auto_skip,{S,{tc1,subgroup}, % subgroup, single tc1
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,4}}},
     {?eh,tc_auto_skip,{S,{tc2,subgroup}, % subgroup, tc2, {repeat,2}
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,5}}},
     {?eh,tc_auto_skip,{S,{tc2,g_mixed}, % tc2, {repeat,2}
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,6}}},
     {?eh,tc_auto_skip,{S,{tc2,g_mixed}, % single tc2
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,7}}},
     {?eh,tc_auto_skip,{S,{tc1,g_mixed}, % tc1, {repeat,2}
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,8}}},
     {?eh,tc_auto_skip,{S,{tc1,g_mixed}, % single tc1
                        {failed,{tc_repeat_SUITE,tc4}}}},
     {?eh,test_stats,{15,6,{0,9}}},
     {?eh,tc_start,{S,{end_per_group,g_mixed,'_'}}},
     {?eh,tc_done,{S,{end_per_group,g_mixed,'_'},ok}},
     %% done
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(repeat_parallel_until_ok) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{'_',{init_per_group,g_parallel_until_ok,[parallel]}}},
     {?eh,tc_done,{'_',{init_per_group,g_parallel_until_ok,[parallel]},ok}},
     {?eh,severe_error,{{illegal_combination,{parallel,repeat_until_ok}},'_'}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(repeat_parallel_until_fail) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{'_',{init_per_group,g_parallel_until_fail,[parallel]}}},
     {?eh,tc_done,{'_',{init_per_group,g_parallel_until_fail,[parallel]},ok}},
     {?eh,severe_error,{{illegal_combination,{parallel,repeat_until_fail}},'_'}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(repeat_sequence_until_ok) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{'_',{init_per_group,g_sequence_until_ok,[sequence]}}},
     {?eh,tc_done,{'_',{init_per_group,g_sequence_until_ok,[sequence]},ok}},
     {?eh,severe_error,{{illegal_combination,{sequence,repeat_until_ok}},'_'}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(repeat_sequence_until_fail) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{'_',{init_per_group,g_sequence_until_fail,[sequence]}}},
     {?eh,tc_done,{'_',{init_per_group,g_sequence_until_fail,[sequence]},ok}},
     {?eh,severe_error,{{illegal_combination,{sequence,repeat_until_fail}},'_'}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(pick_one_test_from_group) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{'_',{init_per_group,g_mixed,[]}}},
     {?eh,tc_done,{'_',{init_per_group,g_mixed,[]},ok}},
     {negative,
      {?eh,tc_start,{'_',tc1}},
      {?eh,tc_start,{'_',tc2}}}, % single tc2
     {?eh,tc_done,{'_',tc2,ok}},
     {?eh,tc_start,{'_',tc2}}, % tc2, {repeat,2}
     {?eh,tc_done,{'_',tc2,ok}},
     {?eh,tc_start,{'_',tc2}},
     {?eh,tc_done,{'_',tc2,ok}},
     {negative,
      {?eh,tc_start,{'_',tc1}},
      {?eh,tc_start,{'_',{end_per_group,g_mixed,[]}}}},
     {?eh,tc_done,{'_',{end_per_group,g_mixed,[]},ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(pick_one_test_from_subgroup) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,tc_start,{'_',{init_per_group,g_mixed,[]}}},
     {?eh,tc_done,{'_',{init_per_group,g_mixed,[]},ok}},
     {negative,
      {?eh,tc_start,{'_',tc2}},
      {?eh,tc_start,{'_',{init_per_group,subgroup,[]}}}},
     {?eh,tc_done,{'_',{init_per_group,subgroup,[]},ok}},
     {negative,
      {?eh,tc_start,{'_',tc1}},
      {?eh,tc_start,{'_',tc2}}}, % tc2, {repeat,2}
     {?eh,tc_done,{'_',tc2,ok}},
     {?eh,tc_start,{'_',tc2}},
     {?eh,tc_done,{'_',tc2,ok}},
     {negative,
      {?eh,tc_start,{'_',tc1}},
      {?eh,tc_start,{'_',{end_per_group,subgroup,[]}}}},
     {?eh,tc_done,{'_',{end_per_group,subgroup,[]},ok}},
     {negative,
      {?eh,tc_start,{'_',tc2}},
      {?eh,tc_start,{'_',{end_per_group,g_mixed,[]}}}},
     {?eh,tc_done,{'_',{end_per_group,g_mixed,[]},ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(ok) ->
    ok.

%% test events help functions
contains(List) ->
    fun(Proplist) when is_list(Proplist) ->
	    contains(List,Proplist)
    end.

contains([{not_in_order,List}|T],Rest) ->
    contains_parallel(List,Rest),
    contains(T,Rest);
contains([{Ele,Pos}|T] = L,[H|T2]) ->
    case element(Pos,H) of
	Ele ->
	    contains(T,T2);
	_ ->
	    contains(L,T2)
    end;
contains([Ele|T],[{Ele,_}|T2])->
    contains(T,T2);
contains([Ele|T],[Ele|T2])->
    contains(T,T2);
contains(List,[_|T]) ->
    contains(List,T);
contains([],_) ->
    match.

contains_parallel([Key | T], Elems) ->
    contains([Key],Elems),
    contains_parallel(T,Elems);
contains_parallel([],_Elems) ->
    match.

not_contains(List) ->
    fun(Proplist) when is_list(Proplist) ->
	    [] = [Ele || {Ele,_} <- Proplist,
			 Test <- List,
			 Test =:= Ele]
    end.
