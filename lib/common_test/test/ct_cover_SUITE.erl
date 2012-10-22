%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
%%% File: ct_cover_SUITE
%%%
%%% Description:
%%% Test code cover analysis support
%%%
%%%-------------------------------------------------------------------
-module(ct_cover_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).
-define(mod, cover_test_mod).
-define(cover_spec_file,"test.cover").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case test_server:is_cover() of
	true ->
	    {skip,"Test server is running cover already - skipping"};
	false ->
	    ct_test_support:init_per_suite(Config)
    end.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    Node = fullname(existing_node),
    case lists:member(Node,nodes()) of
	true -> rpc:call(Node,erlang,halt,[]);
	false -> ok
    end,
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     default,
     cover_stop_true,
     cover_stop_false,
     slave,
     slave_start_slave,
     cover_node_option,
     ct_cover_add_remove_nodes
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%% Check that cover is collected from test node
%% Also check that cover is by default stopped after test is completed
default(Config) ->
    {ok,Events} = run_test(default,Config),
    false = check_cover(Config),
    check_calls(1,Events),
    ok.

%% Check that cover is stopped when cover_stop option is set to true
cover_stop_true(Config) ->
    {ok,_Events} = run_test(cover_stop_true,[{cover_stop,true}],Config),
    false = check_cover(Config).

%% Check that cover is not stopped when cover_stop option is set to false
cover_stop_false(Config) ->
    {ok,_Events} = run_test(cover_stop_false,[{cover_stop,false}],Config),
    {true,[],[?mod]} = check_cover(Config),
    CTNode = proplists:get_value(ct_node, Config),
    ok = rpc:call(CTNode,cover,stop,[]),
    false = check_cover(Config),
    ok.

%% Let test node start a slave node - check that cover is collected
%% from both nodes
slave(Config) ->
    {ok,Events} = run_test(slave,slave,[],Config),
    check_calls(2,Events),
    ok.

%% Let test node start a slave node which in turn starts another slave
%% node - check that cover is collected from all three nodes
slave_start_slave(Config) ->
    {ok,Events} = run_test(slave_start_slave,slave_start_slave,[],Config),
    check_calls(3,Events),
    ok.

%% Start a slave node before test starts - the node is listed in cover
%% spec file.
%% Check that cover is collected from test node and slave node.
cover_node_option(Config) ->
    {ok, HostStr}=inet:gethostname(),
    Host = list_to_atom(HostStr),
    DataDir = ?config(data_dir,Config),
    {ok,Node} = ct_slave:start(Host,existing_node,
			       [{erl_flags,"-pa " ++ DataDir}]),
    false = check_cover(Node),
    CoverFile = add_to_cover_file("node_opt.cover",[{nodes,[Node]}],Config),
    {ok,Events} = run_test(cover_node_option,cover_node_option,
			   [{cover,CoverFile}],Config),
    check_calls(2,Events),
    {ok,Node} = ct_slave:stop(existing_node),
    ok.

%% Test ct_cover:add_nodes/1 and ct_cover:remove_nodes/1
%% Check that cover is collected from added node
ct_cover_add_remove_nodes(Config) ->
    {ok, HostStr}=inet:gethostname(),
    Host = list_to_atom(HostStr),
    DataDir = ?config(data_dir,Config),
    {ok,Node} = ct_slave:start(Host,existing_node,
			       [{erl_flags,"-pa " ++ DataDir}]),
    false = check_cover(Node),
    {ok,Events} = run_test(ct_cover_add_remove_nodes,ct_cover_add_remove_nodes,
			   [],Config),
    check_calls(2,Events),
    {ok,Node} = ct_slave:stop(existing_node),
    ok.


%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
run_test(Label,Config) ->
    run_test(Label,[],Config).
run_test(Label,ExtraOpts,Config) ->
    run_test(Label,default,ExtraOpts,Config).
run_test(Label,Testcase,ExtraOpts,Config) ->
    DataDir = ?config(data_dir, Config),
    Suite = filename:join(DataDir, "cover_SUITE"),
    DefaultCover = filename:join(DataDir, ?cover_spec_file),
    Cover = proplists:get_value(cover,ExtraOpts,DefaultCover),
    RestOpts = lists:keydelete(cover,1,ExtraOpts),
    {Opts,ERPid} = setup([{suite,Suite},{testcase,Testcase},
			  {cover,Cover},{label,Label}] ++ RestOpts, Config),
    execute(Label, Testcase, Opts, ERPid, Config).

setup(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}}|Test],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

execute(Name, Testcase, Opts, ERPid, Config) ->
    ok = ct_test_support:run(Opts, Config),
    Events = ct_test_support:get_events(ERPid, Config),

    ct_test_support:log_events(Name,
			       reformat(Events, ?eh),
			       ?config(priv_dir, Config),
			       Opts),
    TestEvents = events_to_check(Testcase),
    R = ct_test_support:verify_events(TestEvents, Events, Config),
   {R,Events}.

reformat(Events, EH) ->
    ct_test_support:reformat(Events, EH).

events_to_check(Testcase) ->
    OneTest =
	[{?eh,start_logging,{'DEF','RUNDIR'}}] ++
	[{?eh,tc_done,{cover_SUITE,Testcase,ok}}] ++
	[{?eh,stop_logging,[]}],

    %% 2 tests (ct:run_test + script_start) is default
    OneTest ++ OneTest.

check_cover(Config) when is_list(Config) ->
    CTNode = proplists:get_value(ct_node, Config),
    check_cover(CTNode);
check_cover(Node) when is_atom(Node) ->
    case rpc:call(Node,test_server,is_cover,[]) of
	true ->
	    {true,
	     rpc:call(Node,cover,which_nodes,[]),
	     rpc:call(Node,cover,modules,[])};
	false ->
	    false
    end.

%% Check that each coverlog includes N calls to ?mod:foo/0
check_calls(N,Events) ->
    CoverLogs =
	[filename:join(filename:dirname(TCLog),"all.coverdata") ||
	    {ct_test_support_eh,
	     {event,tc_logfile,ct@falco,
	      {{cover_SUITE,init_per_suite},TCLog}}} <- Events],
    do_check_logs(N,CoverLogs).

do_check_logs(N,[CoverLog|CoverLogs]) ->
    {ok,_} = cover:start(),
    ok = cover:import(CoverLog),
    {ok,[{{?mod,foo,0},N}]} = cover:analyse(?mod,calls,function),
    ok = cover:stop(),
    do_check_logs(N,CoverLogs);
do_check_logs(_,[]) ->
    ok.

fullname(Name) ->
    {ok,Host} = inet:gethostname(),
    list_to_atom(atom_to_list(Name) ++ "@" ++ Host).

add_to_cover_file(CoverFileName,Terms,Config) ->
    DataDir = ?config(data_dir,Config),
    DefaultFile = filename:join(DataDir,?cover_spec_file),
    NewFile = filename:join(DataDir,CoverFileName),
    {ok,_} = file:copy(DefaultFile,NewFile),
    {ok,Fd} = file:open(NewFile,[append]),
    lists:foreach(fun(Term) ->
			  file:write(Fd,io_lib:format("~p.~n",[Term]))
		  end,Terms),
    ok = file:close(Fd),
    NewFile.
