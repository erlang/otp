%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
%%% File: ct_cover_nomerge_SUITE
%%%
%%% Description:
%%% Test code cover analysis support when merge_tests=false
%%%
%%%-------------------------------------------------------------------
-module(ct_cover_nomerge_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).
-define(mod, cover_test_mod).

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
    try apply(?MODULE,TestCase,[cleanup,Config])
    catch error:undef -> ok
    end,
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     local,
     remote,
     remote_nostop
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

local(Config) ->
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "local.spec"),
    CoverSpec = [{incl_mods,[?mod]}],
    CoverFile = create_cover_file(local,CoverSpec,Config),
    {Opts,ERPid} = setup([{spec,Spec},{label,local},{cover,CoverFile}], Config),
    {ok,Events} = execute(local, local, Opts, ERPid, Config),
    false = check_cover(Config),
    check_calls(Events,2),
    ok.

remote(Config) ->
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "remote.spec"),
    %% extending some timers for slow test hosts
    {ok,Node} = ct_slave:start(ct_nomerge,[{boot_timeout,15},
					   {init_timeout,15},
					   {startup_timeout,15}]),
    
    CoverSpec = [{nodes,[Node]},
		 {incl_mods,[?mod]}],
    CoverFile = create_cover_file(remote,CoverSpec,Config),
    {Opts,ERPid} = setup([{spec,Spec},{label,remote},{cover,CoverFile}], Config),
    {ok,Events} = execute(remote, remote, Opts, ERPid, Config),
    false = check_cover(Config),
    check_calls(Events,2),
    ok.
remote(cleanup,_Config) ->
    {ok,_} = ct_slave:stop(ct_nomerge),
    ok.

remote_nostop(Config) ->
    DataDir = ?config(data_dir, Config),
    Spec = filename:join(DataDir, "remote_nostop.spec"),
    %% extending some timers for slow test hosts
    {ok,Node} = ct_slave:start(ct_nomerge,[{boot_timeout,15},
					   {init_timeout,15},
					   {startup_timeout,15}]),
    
    CoverSpec = [{nodes,[Node]},
		 {incl_mods,[?mod]}],
    CoverFile = create_cover_file(remote_nostop,CoverSpec,Config),
    {Opts,ERPid} = setup([{spec,Spec},{label,remote_nostop},
			  {cover,CoverFile},{cover_stop,false}],
			 Config),
    {ok,Events} = execute(remote_nostop, remote_nostop, Opts, ERPid, Config),
    {true,[Node],[cover_test_mod]} = check_cover(Config),
    check_calls(Events,2),
    ok.
remote_nostop(cleanup,Config) ->
    CtNode = ?config(ct_node,Config),
    ok = rpc:call(CtNode,cover,stop,[]),
    {ok,_} = ct_slave:stop(ct_nomerge),
    ok.
    

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

events_to_check(local) ->
    events_to_check1(cover_nomerge_local_SUITE);
events_to_check(remote) ->
    events_to_check1(cover_nomerge_remote_SUITE);
events_to_check(remote_nostop) ->
    events_to_check1(cover_nomerge_remote_nostop_SUITE).
events_to_check1(Suite) ->
    OneTest =
	[{?eh,start_logging,{'DEF','RUNDIR'}}] ++
	[{?eh,tc_done,{Suite,t1,ok}}] ++
	[{?eh,tc_done,{Suite,t2,ok}}] ++
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

%% Get the log dir "ct_run.<timestamp>" for all (both!) tests
get_log_dirs(Events) ->
    [LogDir ||
	{ct_test_support_eh,
	 {event,start_logging,_Node,LogDir}} <- Events].

%% Check that each coverlog includes N calls to ?mod:foo/0
check_calls(Events,N) ->
    check_calls(Events,{?mod,foo,0},N).
check_calls(Events,MFA,N) ->
    CoverLogs = [filename:join(D,"all.coverdata") || D <- get_log_dirs(Events)],
    do_check_logs(CoverLogs,MFA,N).

do_check_logs([CoverLog|CoverLogs],{Mod,_,_} = MFA,N) ->
    {ok,_} = cover:start(),
    ok = cover:import(CoverLog),
    {ok,Calls} = cover:analyse(Mod,calls,function),
    ok = cover:stop(),
    {MFA,N} = lists:keyfind(MFA,1,Calls),
    do_check_logs(CoverLogs,MFA,N);
do_check_logs([],_,_) ->
    ok.

create_cover_file(Filename,Terms,Config) ->
    PrivDir = ?config(priv_dir,Config),
    File = filename:join(PrivDir,Filename) ++ ".cover",
    {ok,Fd} = file:open(File,[write]),
    lists:foreach(fun(Term) ->
			  file:write(Fd,io_lib:format("~p.~n",[Term]))
		  end,Terms),
    ok = file:close(Fd),
    File.
