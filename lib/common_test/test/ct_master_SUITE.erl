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
%%% File: ct_master_SUITE
%%%
%%% Description:
%%% Test ct_master.
%%%
%%% The suites used for the test are located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_master_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

-define(TEMP_DIR, case os:type() of
		      {win32,_} ->
			  "c:/Temp";
		      _ ->
			  "/tmp"
		  end).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct_test_support:init_per_suite(Config).

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    NodeCount = 5,
    NodeNames = [list_to_atom("t_"++integer_to_list(N)) ||
		 N <- lists:seq(1, NodeCount)],
    ct_test_support:init_per_testcase(
      TestCase,[{node_names,NodeNames},
		{master, true}|Config]).

end_per_testcase(TestCase, Config) ->
    case os:type() of
	{win32,_} ->
	    %% If this is a windows run the logs are saved to /tmp and
	    %% then moved to private_dir as a tar because otherwise
	    %% the file names become too long! :(
	    Files = filelib:wildcard(filename:join(?TEMP_DIR,"slave.*")),
	    erl_tar:create(
	      filename:join(
		proplists:get_value(priv_dir,Config),"slaves.tar.gz"),
	      Files,[compressed]),
	    os:cmd("rm -rf "++filename:join(?TEMP_DIR,"slave.*"));
	_ ->
	    ok
    end,
    
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{timetrap,{seconds,60}},
	    {ct_hooks,[ts_install_cth]}].

all() -> 
    [ct_master_test].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
ct_master_test(Config) when is_list(Config) ->
    NodeNames = proplists:get_value(node_names, Config),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),

    FileName = filename:join(PrivDir, "ct_master_spec.spec"),
    Suites = [master_SUITE],
    TSFile = make_spec(DataDir, FileName, NodeNames, Suites, Config),

    ERPid = ct_test_support:start_event_receiver(Config),

    [{[TSFile],ok}] = run_test(ct_master_test, FileName, Config),

    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(ct_master_test, 
			       reformat(Events, ?eh),
			       PrivDir, []),

    TestEvents = events_to_check(ct_master_test),
    ok = find_events(NodeNames, TestEvents, Events, Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
make_spec(DataDir, FileName, NodeNames, Suites, Config) ->
    {ok,HostName} = inet:gethostname(),

    N = lists:map(fun(NodeName) ->
	    {node, NodeName, list_to_atom(atom_to_list(NodeName)++"@"++HostName)}
	end,
	NodeNames),

    C = lists:map(
	  fun(NodeName) ->
		  Rnd = rand:uniform(2),
		  if Rnd == 1->
			  {config,NodeName,filename:join(DataDir,
							 "master/config.txt")};
		     true ->
			  {userconfig,NodeName,
			   {ct_config_xml,filename:join(DataDir,
							"master/config.xml")}}
		  end
	  end,
	  NodeNames),
    
    CM = [{config,master,filename:join(DataDir,"master/config.txt")}],

    Env = [{"THIS_MUST_BE_SET","yes"},
	   {"SO_MUST_THIS","value"}],
    NS = lists:map(
	   fun(NodeName) ->
		   {init,NodeName,[
				   {node_start,[{startup_functions,[]},
						{monitor_master,true},
						{boot_timeout,10},
						{init_timeout,10},
						{startup_timeout,10},
						{env,Env}]},
				   {eval,{erlang,nodes,[]}}]
		   }
	   end,
	   NodeNames),
    
    S = [{suites, NodeNames, filename:join(DataDir, "master"), Suites}],
    
    PrivDir = ?config(priv_dir, Config),

    LD = lists:map(
	   fun(NodeName) ->
		   {logdir,NodeName,get_log_dir(os:type(),PrivDir, NodeName)}
	   end,
	   NodeNames) ++ [{logdir,master,PrivDir}],

    EvHArgs = [{cbm,ct_test_support},{trace_level,?config(trace_level,Config)}],
    EH = [{event_handler,master,[?eh],EvHArgs}],
    
    Include = [{include,filename:join([DataDir,"master/include"])}],
    
    ct_test_support:write_testspec(N++Include++EH++C++CM++S++LD++NS, FileName).

get_log_dir({win32,_}, _PrivDir, NodeName) ->
    case filelib:is_dir(?TEMP_DIR) of
	false ->
	    file:make_dir(?TEMP_DIR);
	_ ->
	    ok
    end,
    get_log_dir(tmp, ?TEMP_DIR,NodeName);
get_log_dir(_,PrivDir,NodeName) ->
    LogDir = filename:join(PrivDir, io_lib:format("slave.~p", [NodeName])),
    file:make_dir(LogDir),
    LogDir.

run_test(_Name, FileName, Config) ->
    %% run the test twice, using different html versions
    [{[FileName],ok}] = ct_test_support:run({ct_master,run,[FileName]},
					    [{ct_master,basic_html,[true]}],
					    Config),
    [{[FileName],ok}] = ct_test_support:run({ct_master,run,[FileName]},
					    [{ct_master,basic_html,[false]}],
					    Config).

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------

find_events(NodeNames, TestEvents, Events, Config) ->
    [begin
	 Node = add_host(Node0),
	 io:format("Searching for events for node: ~s", [Node]),
	 ok = ct_test_support:verify_events(TestEvents, Events, Node, Config),
	 io:nl()
     end || Node0 <- NodeNames],
    ok.

add_host(NodeName) ->
    {ok, HostName} = inet:gethostname(),
    list_to_atom(atom_to_list(NodeName)++"@"++HostName).
    
events_to_check(_) ->
    [{?eh,tc_start,{master_SUITE,first_testcase}},
     {?eh,tc_done,{master_SUITE,first_testcase,ok}},
     {?eh,tc_start,{master_SUITE,second_testcase}},
     {?eh,tc_done,{master_SUITE,second_testcase,ok}},
     {?eh,tc_start,{master_SUITE,third_testcase}},
     {?eh,tc_done,{master_SUITE,third_testcase,ok}},
     {?eh,tc_start,{master_SUITE,env_vars}},
     {?eh,tc_done,{master_SUITE,env_vars,ok}}].
