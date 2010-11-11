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

%%%-------------------------------------------------------------------
%%% File: ct_master_SUITE
%%%
%%% Description:
%%% Test ct_master.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_master_SUITE).
-compile(export_all).

-include_lib("test_server/include/test_server.hrl").
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
    ct_test_support:init_per_testcase(TestCase, [{master, true}|Config]).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

all() ->
    all(suite).
all(doc) ->
    [""];

all(suite) ->
    [
	ct_master_test
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
ct_master_test(Config) when is_list(Config)->
    NodeCount = 5,
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    NodeNames = [list_to_atom("testnode_"++integer_to_list(N)) ||
		 N <- lists:seq(1, NodeCount)],
    FileName = filename:join(PrivDir, "ct_master_spec.spec"),
    Suites = [master_SUITE],
    TSFile = make_spec(DataDir, FileName, NodeNames, Suites, Config),
    ERPid = ct_test_support:start_event_receiver(Config),
    spawn(ct@ancalagon,
	  fun() ->
		  dbg:tracer(),dbg:p(all,c),
		  dbg:tpl(erlang, spawn_link, 4,x),
		  receive ok -> ok end
	  end),

    [{TSFile, ok}] = run_test(ct_master_test, FileName, Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(groups_suite_1, 
			       reformat(Events, ?eh), 
			       ?config(priv_dir, Config)),
    find_events(NodeNames, [{tc_start,{master_SUITE,init_per_suite}},
			    {tc_start,{master_SUITE,first_testcase}},
			    {tc_start,{master_SUITE,second_testcase}},
			    {tc_start,{master_SUITE,third_testcase}},
			    {tc_start,{master_SUITE,end_per_suite}}],
	       Events),
    
    ok.

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
make_spec(DataDir, FileName, NodeNames, Suites, Config)->
    {ok, HostName} = inet:gethostname(),

    N = lists:map(fun(NodeName)->
	    {node, NodeName, list_to_atom(atom_to_list(NodeName)++"@"++HostName)}
	end,
	NodeNames),

    C = lists:map(fun(NodeName)->
	Rnd = random:uniform(2),
	if Rnd == 1->
	    {config, NodeName, filename:join(DataDir, "master/config.txt")};
	true->
	    {userconfig, NodeName, {ct_config_xml, filename:join(DataDir, "master/config.xml")}}
        end
	end,
	NodeNames),

    NS = lists:map(fun(NodeName)->
	     {init, NodeName, [
				{node_start, [{startup_functions, []}, {monitor_master, true}]},
				{eval, {erlang, nodes, []}}
			      ]
	     }
	 end,
	 NodeNames),

    S = [{suites, NodeNames, filename:join(DataDir, "master"), Suites}],

    PrivDir = ?config(priv_dir, Config),
    LD = lists:map(fun(NodeName)->
	     {logdir, NodeName, get_log_dir(PrivDir, NodeName)}
         end,
	 NodeNames) ++ [{logdir, master, PrivDir}],
    EvHArgs = [{cbm,ct_test_support},{trace_level,?config(trace_level,Config)}],
    EH = [{event_handler,master,[?eh],EvHArgs}],

    Include = [{include,filename:join([DataDir,"master/include"])}],

    ct_test_support:write_testspec(N++Include++EH++C++S++LD++NS, FileName).

get_log_dir(PrivDir, NodeName)->
    LogDir = filename:join(PrivDir, io_lib:format("slave.~p", [NodeName])),
    file:make_dir(LogDir),
    LogDir.

run_test(_Name, FileName, Config)->
    [{FileName, ok}] = ct_test_support:run(ct_master, run, [FileName], Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
find_events([], _CheckEvents, _) ->
    ok;
find_events([NodeName|NodeNames],CheckEvents,AllEvents) ->
    find_events(NodeNames, CheckEvents,
		remove_events(add_host(NodeName),CheckEvents, AllEvents, [])).

remove_events(Node,[{Name,Data} | RestChecks],
	      [{?eh,#event{ name = Name, node = Node, data = Data }}|RestEvs],
	       Acc) ->
    remove_events(Node, RestChecks, RestEvs, Acc);
remove_events(Node, Checks, [Event|RestEvs], Acc) ->
    remove_events(Node, Checks, RestEvs, [Event | Acc]);
remove_events(_Node, [], [], Acc) ->
    lists:reverse(Acc);
remove_events(Node, Events, [], Acc) ->
    test_server:format("Could not find events: ~p in ~p for node ~p",
	   [Events, lists:reverse(Acc), Node]),
    exit(event_not_found).

add_host(NodeName) ->
    {ok, HostName} = inet:gethostname(),
    list_to_atom(atom_to_list(NodeName)++"@"++HostName).
    
expected_events(_)->
    [].
