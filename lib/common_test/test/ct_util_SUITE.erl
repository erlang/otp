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
-module(ct_util_SUITE).

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
    CTHs = filelib:wildcard(filename:join(DataDir,"*_cth.erl")),
    io:format("CTHs: ~p",[CTHs]),
    [io:format("Compiling ~p: ~p",
	    [FileName,compile:file(FileName,[{outdir,DataDir},debug_info])]) ||
	FileName <- CTHs],
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
       pre_init_per_suite,
       post_init_per_suite,
       pre_end_per_suite,
       post_end_per_suite,
       pre_init_per_group,
       post_init_per_group,
       pre_end_per_group,
       post_end_per_group,
       pre_init_per_testcase,
       post_init_per_testcase,
       pre_end_per_testcase,
       post_end_per_testcase
      ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
pre_init_per_suite(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{pre_init_per_suite,
                           {curr_tc_SUITE,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).
    
post_init_per_suite(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{post_init_per_suite,
                           {curr_tc_SUITE,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).
    
pre_end_per_suite(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{pre_end_per_suite,
                           {curr_tc_SUITE,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).
    
post_end_per_suite(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{post_end_per_suite,
                           {curr_tc_SUITE,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).
    

pre_init_per_group(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{pre_init_per_group,
                           {curr_tc_SUITE,g,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).

post_init_per_group(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{post_init_per_group,
                           {curr_tc_SUITE,g,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).

pre_end_per_group(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{pre_end_per_group,
                           {curr_tc_SUITE,g,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).

post_end_per_group(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{post_end_per_group,
                           {curr_tc_SUITE,g,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).

pre_init_per_testcase(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{pre_init_per_testcase,
                           {curr_tc_SUITE,tc1,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).

post_init_per_testcase(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{post_init_per_testcase,
                           {curr_tc_SUITE,tc1,{timeout,5000}}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).

pre_end_per_testcase(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{pre_end_per_testcase,
                           {curr_tc_SUITE,tc1,{timeout,5000}}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).

post_end_per_testcase(Config) ->
    CfgFile = gen_config(?FUNCTION_NAME,
                         [{post_end_per_testcase,
                           {curr_tc_SUITE,tc1,kill}}],
                         Config),
    ok = do_test(?FUNCTION_NAME,
                 "curr_tc_SUITE.erl",
                 [{ct_hooks,[ct_util_cth]},{config,CfgFile}],
                 Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

do_test(Tag, Suite, RunTestArgs, Config) ->
    do_test(Tag, Suite, RunTestArgs, Config, 2).

do_test(Tag, Suite0, RunTestArgs, Config, EC) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join([DataDir,Suite0]),
    {Opts,ERPid} = setup([{suite,Suite}]++[{label,Tag}|RunTestArgs],Config),
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

test_events(IPS) when IPS=:=pre_init_per_suite; IPS=:=post_init_per_suite ->
    S = curr_tc_SUITE,
    FwFunc =
        case IPS of
            pre_init_per_suite -> init_tc;
            post_init_per_suite -> end_tc
        end,
    E = {failed,{ct_framework,FwFunc,{test_case_failed,hahahahahah}}},
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{S,init_per_suite}},
     {?eh,tc_done,{S,init_per_suite,E}},
     {?eh,tc_auto_skip,{S,tc1,{failed,{S,init_per_suite,E}}}},
     {?eh,tc_auto_skip,{S,tc2,{failed,{S,init_per_suite,E}}}},
     {?eh,tc_auto_skip,{S,{tc1,g},{failed,{S,init_per_suite,E}}}},
     {?eh,tc_auto_skip,{S,{tc2,g},{failed,{S,init_per_suite,E}}}},
     {?eh,test_stats,{0,0,{0,4}}},
     {?eh,tc_auto_skip,{S,end_per_suite,{failed,{S,init_per_suite,E}}}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(EPS) when EPS=:=pre_end_per_suite; EPS=:=post_end_per_suite ->
    S = curr_tc_SUITE,
    FwFunc =
        case EPS of
            pre_end_per_suite -> init_tc;
            post_end_per_suite -> end_tc
        end,
    E = {failed,{ct_framework,FwFunc,{test_case_failed,hahahahahah}}},
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{S,init_per_suite}},
     {?eh,tc_done,{S,init_per_suite,ok}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{S,tc2}},
     {?eh,tc_done,{S,tc2,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     [{?eh,tc_start,{S,{init_per_group,g,[]}}},
      {?eh,tc_done,{S,{init_per_group,g,[]},ok}},
      {?eh,tc_start,{S,tc1}},
      {?eh,tc_done,{S,tc1,ok}},
      {?eh,test_stats,{3,0,{0,0}}},
      {?eh,tc_start,{S,tc2}},
      {?eh,tc_done,{S,tc2,ok}},
      {?eh,test_stats,{4,0,{0,0}}},
      {?eh,tc_start,{S,{end_per_group,g,[]}}},
      {?eh,tc_done,{S,{end_per_group,g,[]},ok}}],
     {?eh,tc_start,{S,end_per_suite}},
     {?eh,tc_done,{S,end_per_suite,E}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(IPG) when IPG=:=pre_init_per_group; IPG=:=post_init_per_group ->
    S = curr_tc_SUITE,
    FwFunc =
        case IPG of
            pre_init_per_group -> init_tc;
            post_init_per_group -> end_tc
        end,
    E = {failed,{ct_framework,FwFunc,{test_case_failed,hahahahahah}}},
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{S,init_per_suite}},
     {?eh,tc_done,{S,init_per_suite,ok}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{S,tc2}},
     {?eh,tc_done,{S,tc2,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     [{?eh,tc_start,{S,{init_per_group,g,[]}}},
      {?eh,tc_done,{S,{init_per_group,g,[]},E}},
      {?eh,tc_auto_skip,{S,{tc1,g},{failed,{S,init_per_group,E}}}},
      {?eh,tc_auto_skip,{S,{tc2,g},{failed,{S,init_per_group,E}}}},
      {?eh,test_stats,{2,0,{0,2}}},
      {?eh,tc_auto_skip,{S,{end_per_group,g},{failed,{S,init_per_group,E}}}}],
     {?eh,tc_start,{S,end_per_suite}},
     {?eh,tc_done,{S,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(EPG) when EPG=:=pre_end_per_group; EPG=:=post_end_per_group ->
    S = curr_tc_SUITE,
    FwFunc =
        case EPG of
            pre_end_per_group -> init_tc;
            post_end_per_group -> end_tc
        end,
    E = {failed,{ct_framework,FwFunc,{test_case_failed,hahahahahah}}},
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{S,init_per_suite}},
     {?eh,tc_done,{S,init_per_suite,ok}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{S,tc2}},
     {?eh,tc_done,{S,tc2,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     [{?eh,tc_start,{S,{init_per_group,g,[]}}},
      {?eh,tc_done,{S,{init_per_group,g,[]},ok}},
      {?eh,tc_start,{S,tc1}},
      {?eh,tc_done,{S,tc1,ok}},
      {?eh,test_stats,{3,0,{0,0}}},
      {?eh,tc_start,{S,tc2}},
      {?eh,tc_done,{S,tc2,ok}},
      {?eh,test_stats,{4,0,{0,0}}},
      {?eh,tc_start,{S,{end_per_group,g,[]}}},
      {?eh,tc_done,{S,{end_per_group,g,[]},E}}],
     {?eh,tc_start,{S,end_per_suite}},
     {?eh,tc_done,{S,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(IPTC) when IPTC=:=pre_init_per_testcase;
                       IPTC=:=post_init_per_testcase ->
    S = curr_tc_SUITE,
    E = case IPTC of
            pre_init_per_testcase ->
                {failed,{ct_framework,init_tc,{test_case_failed,hahahahahah}}};
            post_init_per_testcase ->
                {failed,{ct_framework,end_tc,{timetrap,3000}}}
        end,
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{S,init_per_suite}},
     {?eh,tc_done,{S,init_per_suite,ok}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,{auto_skipped,E}}},
     {?eh,test_stats,{0,0,{0,1}}},
     {?eh,tc_start,{S,tc2}},
     {?eh,tc_done,{S,tc2,ok}},
     {?eh,test_stats,{1,0,{0,1}}},
     [{?eh,tc_start,{S,{init_per_group,g,[]}}},
      {?eh,tc_done,{S,{init_per_group,g,[]},ok}},
      {?eh,tc_start,{S,tc1}},
      {?eh,tc_done,{S,tc1,{auto_skipped,E}}},
      {?eh,test_stats,{1,0,{0,2}}},
      {?eh,tc_start,{S,tc2}},
      {?eh,tc_done,{S,tc2,ok}},
      {?eh,test_stats,{2,0,{0,2}}},
      {?eh,tc_start,{S,{end_per_group,g,[]}}},
      {?eh,tc_done,{S,{end_per_group,g,[]},ok}}],
     {?eh,tc_start,{S,end_per_suite}},
     {?eh,tc_done,{S,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];

test_events(EPTC) when EPTC=:=pre_end_per_testcase; EPTC=:=post_end_per_testcase->
    S = curr_tc_SUITE,
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,4}},
     {?eh,tc_start,{S,tc1}},
     {?eh,tc_done,{S,tc1,ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{S,tc2}},
     {?eh,tc_done,{S,tc2,ok}},
     {?eh,test_stats,{2,0,{0,0}}},
     [{?eh,tc_start,{S,{init_per_group,g,[]}}},
      {?eh,tc_done,{S,{init_per_group,g,[]},ok}},
      {?eh,tc_start,{S,tc1}},
      {?eh,tc_done,{S,tc1,ok}},
      {?eh,test_stats,{3,0,{0,0}}},
      {?eh,tc_start,{S,tc2}},
      {?eh,tc_done,{S,tc2,ok}},
      {?eh,test_stats,{4,0,{0,0}}},
      {?eh,tc_start,{S,{end_per_group,g,[]}}},
      {?eh,tc_done,{S,{end_per_group,g,[]},ok}}],
     {?eh,tc_start,{S,end_per_suite}},
     {?eh,tc_done,{S,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].

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
