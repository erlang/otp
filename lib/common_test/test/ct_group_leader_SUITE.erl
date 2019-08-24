%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
%%% File: ct_system_error_SUITE
%%%
%%% Description:
%%%
%%% Test the group leader functionality in the test_server application.
%%%-------------------------------------------------------------------
-module(ct_group_leader_SUITE).

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
    [
     basic
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
basic(Config) ->
    TC = basic,
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "group_leader_SUITE"),
    {Opts,ERPid} = setup([{suite,Suite},{label,TC}], Config),
    SuiteLog = execute(TC, Opts, ERPid, Config),
    {ok,Data} = file:read_file(SuiteLog),
    Lines = binary:split(Data, <<"\n">>, [global]),
    {ok,RE} = re:compile("(\\S+):(\\S+)$"),
    Cases0 = [begin
		  {match,[M,F]} = re:run(Case, RE, [{capture,all_but_first,list}]),
		  {list_to_atom(M),list_to_atom(F)}
	      end || <<"=case ",Case/binary>> <- Lines],
    Cases = [MF || {_,F}=MF <- Cases0,
		   F =/= init_per_suite,
		   F =/= end_per_suite,
		   F =/= init_per_group,
		   F =/= end_per_group],
    io:format("~p\n", [Cases]),
    [] = verify_cases(events_to_check(TC), Cases, false),
    ok.

verify_cases([{parallel,P}|Ts], Cases0, Par) ->
    Cases = verify_cases(P, Cases0, true),
    verify_cases(Ts, Cases, Par);
verify_cases([{?eh,tc_done,{M,F,_}}|Ts], Cases0, false) ->
    [{M,F}|Cases] = Cases0,
    verify_cases(Ts, Cases, false);
verify_cases([{?eh,tc_done,{M,F,_}}|Ts], Cases0, true) ->
    case lists:member({M,F}, Cases0) of
	true ->
	    Cases = Cases0 -- [{M,F}],
	    verify_cases(Ts, Cases, true);
	false ->
	    io:format("~p not found\n", [{M,F}]),
	    ?t:fail()
    end;
verify_cases([{?eh,_,_}|Ts], Cases, Par) ->
    verify_cases(Ts, Cases, Par);
verify_cases([], Cases, _) ->
    Cases;
verify_cases([List|Ts], Cases0, Par) when is_list(List) ->
    Cases = verify_cases(List, Cases0, false),
    verify_cases(Ts, Cases, Par).

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

execute(Name, Opts, ERPid, Config) ->
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Name,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),

    TestEvents = events_to_check(Name),
    ok = ct_test_support:verify_events(TestEvents, Events, Config),
    {event,tc_logfile,_,{_,File}} =
	lists:keyfind(tc_logfile, 2, [Ev || {?eh,Ev} <- Events]),
    LogDir = filename:dirname(File),
    filename:join(LogDir, "suite.log").

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------

events_to_check(_Test) ->
    [{?eh,tc_done,{group_leader_SUITE,tc1,ok}},
     {parallel,[{?eh,tc_start,{group_leader_SUITE,p1}},
		{?eh,tc_done,{group_leader_SUITE,p1,ok}},
		{?eh,tc_start,{group_leader_SUITE,p2}},
		{?eh,tc_done,{group_leader_SUITE,p2,ok}}]},
     {?eh,tc_done,{group_leader_SUITE,p_restart_my_io_server,ok}},
     {?eh,tc_done,{group_leader_SUITE,p3,ok}},
     {parallel,[
		{?eh,tc_start,{group_leader_SUITE,p10}},
		{?eh,tc_start,{group_leader_SUITE,p11}},
		{?eh,tc_done,{group_leader_SUITE,p10,ok}},
		{?eh,tc_done,{group_leader_SUITE,p11,ok}},
		[{?eh,tc_done,{group_leader_SUITE,s1,ok}},
		 {?eh,tc_done,{group_leader_SUITE,s2,ok}},
		 {?eh,tc_done,{group_leader_SUITE,s3,ok}}],
		{?eh,tc_start,{group_leader_SUITE,p12}},
		{?eh,tc_done,{group_leader_SUITE,p12,ok}},
		[{?eh,tc_done,{group_leader_SUITE,s4,ok}},
		 {?eh,tc_done,{group_leader_SUITE,s5,ok}}],
		{?eh,tc_start,{group_leader_SUITE,p13}},
		{?eh,tc_done,{group_leader_SUITE,p13,ok}} ]},
     {?eh,tc_done,{group_leader_SUITE,cap1,ok}},
     {?eh,tc_done,{group_leader_SUITE,cap2,ok}},
     {parallel,[{?eh,tc_start,{group_leader_SUITE,cap1}},
		{?eh,tc_done,{group_leader_SUITE,cap1,ok}},
		{?eh,tc_start,{group_leader_SUITE,cap2}},
		{?eh,tc_done,{group_leader_SUITE,cap2,ok}}]},
     {parallel,[{?eh,tc_start,{group_leader_SUITE,unexp1}},
		{?eh,tc_done,{group_leader_SUITE,unexp1,ok}},
		{?eh,tc_start,{group_leader_SUITE,unexp2}},
		{?eh,tc_done,{group_leader_SUITE,unexp2,ok}}]},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].
