%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2013. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_telnet_SUITE
%%%
%%% Description:
%%% Edit your ts.unix.config or ts.win32.config before runnings these tests
%%% Test ct_telnet_SUITE module
%%%
%%%-------------------------------------------------------------------
-module(ct_telnet_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

-define(erl_telnet_server_port,1234).
-define(erl_telnet_server_user,"telnuser").
-define(erl_telnet_server_pwd,"telnpwd").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

groups() ->
    [{legacy, [], [unix_telnet,own_server,timetrap]},
     {raw,    [], [unix_telnet,own_server,timetrap]},
     {html,   [], [unix_telnet,own_server]},
     {silent, [], [unix_telnet,own_server]}].

all() ->
    [
     {group,legacy},
     {group,raw},
     {group,html},
     {group,silent}
    ].

%%--------------------------------------------------------------------
%% CONFIG FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ct_test_support:init_per_suite(Config).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) when TestCase=/=unix_telnet->
    TS = telnet_server:start([{port,?erl_telnet_server_port},
			      {users,[{?erl_telnet_server_user,
				       ?erl_telnet_server_pwd}]}]),
    ct_test_support:init_per_testcase(TestCase, [{telnet_server,TS}|Config]);
init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    case ?config(telnet_server,Config) of
	undefined -> ok;
	TS -> telnet_server:stop(TS)
    end,
    ct_test_support:end_per_testcase(TestCase, Config).


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%%
unix_telnet(Config) ->
    CfgFile = "telnet.unix_telnet." ++
	atom_to_list(groupname(Config)) ++ ".cfg",
    all_tests_in_suite(unix_telnet,"ct_telnet_basic_SUITE",CfgFile,Config).

own_server(Config) ->
    CfgFile = "telnet.own_server." ++
	atom_to_list(groupname(Config)) ++ ".cfg",
    all_tests_in_suite(own_server,"ct_telnet_own_server_SUITE",
		       CfgFile,Config).

timetrap(Config) ->
    CfgFile = "telnet.timetrap." ++
	atom_to_list(groupname(Config)) ++ ".cfg",
    all_tests_in_suite(timetrap,"ct_telnet_timetrap_SUITE",
		       CfgFile,Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

groupname(Config) ->
    case proplists:get_value(tc_group_properties, Config) of
	undefined ->
	    undefined;
	TGP ->
	    proplists:get_value(name, TGP)
    end.

all_tests_in_suite(TestCase, SuiteName, CfgFileName, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, SuiteName),
    CfgFile = filename:join(PrivDir, CfgFileName),
    Cfg = telnet_config(TestCase, groupname(Config)),
    Txt = lists:flatten([lists:flatten(io_lib:write(C))++".\n" || C <- Cfg]),
    ok = file:write_file(CfgFile, Txt),
    {Opts,ERPid} = setup([{suite,Suite},
			  {label,TestCase},
			  {config,CfgFile}],
			 Config),
    ok = execute(TestCase, Opts, ERPid, Config).

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

    TestEvents = events_to_check(Name,Config),
    ct_test_support:verify_events(TestEvents, Events, Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

telnet_config(_, undefined) ->
    [];
telnet_config(unix_telnet, legacy) ->
    [{unix, ct:get_config(unix)},
     {ct_conn_log,[]}];
%% LogType same as GroupName
telnet_config(unix_telnet, LogType) ->
    [{unix, ct:get_config(unix)},
     {ct_conn_log,
      [{ct_telnet,[{log_type,LogType},
		   {hosts,[telnet_server_conn1,
			   telnet_server_conn2,
			   telnet_server_conn3,
			   telnet_server_conn4]}]}]}];
telnet_config(_, LogType) ->
    [{unix,[{telnet,"localhost"},
	    {port, ?erl_telnet_server_port},
	    {username,?erl_telnet_server_user},
	    {password,?erl_telnet_server_pwd},
	    {keep_alive,true}]} |
     if LogType == legacy -> 
	     [{ct_conn_log,[]}];
	true ->
	     [{ct_conn_log,
	       [{ct_telnet,[{log_type,LogType},
			    {hosts,[telnet_server_conn1,
				    telnet_server_conn2,
				    telnet_server_conn3,
				    telnet_server_conn4]}]}]}]
     end].

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
events_to_check(unix_telnet,Config) ->
    all_cases(ct_telnet_basic_SUITE,Config);
events_to_check(own_server,Config) ->
    all_cases(ct_telnet_own_server_SUITE,Config);
events_to_check(timetrap,_Config) ->
    [{?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,tc_done,{ct_telnet_timetrap_SUITE,expect_timetrap,
		   {failed,{timetrap_timeout,7000}}}},
     {?eh,tc_done,{ct_telnet_timetrap_SUITE,expect_success,ok}},
     {?eh,stop_logging,[]}].

all_cases(Suite,Config) ->
    {module,_} = code:load_abs(filename:join(?config(data_dir,Config),
					     Suite)),
    GroupsAndTCs = Suite:all(),

    Terms =
	lists:flatmap(
	  fun({group,G}) ->
		  {value,{G,Props,GTCs}} =
		      lists:keysearch(G,1,Suite:groups()),
		  GTCs1 = [[{?eh,tc_start,{Suite,GTC}},
			    {?eh,tc_done,{Suite,GTC,ok}}] ||
			      GTC <- GTCs],
		  GEvs = [{?eh,tc_start,{Suite,{init_per_group,G,Props}}},
			  {?eh,tc_done,{Suite,{init_per_group,G,Props},ok}} |
			  GTCs1] ++
		         [{?eh,tc_start,{Suite,{end_per_group,G,Props}}},
			  {?eh,tc_done,{Suite,{end_per_group,G,Props},ok}}],
		  case lists:member(parallel, Props) of
		      true -> [{parallel,GEvs}];
		      false -> GEvs
		  end;
	     (TC) ->
		  [{?eh,tc_done,{Suite,TC,ok}}]
	  end, GroupsAndTCs),
    
    code:purge(Suite),
    code:delete(Suite),

    FlatTerms = lists:flatten(Terms),

    ct:log("Verifying with terms:~n~p", [FlatTerms]),

    OneTest =
	[{?eh,start_logging,{'DEF','RUNDIR'}} |
	 FlatTerms] ++ [{?eh,stop_logging,[]}],

    %% 2 tests (ct:run_test + script_start) is default
    OneTest ++ OneTest.
