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
-define(suite, cover_SUITE).
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
     default,
     cover_stop_true,
     cover_stop_false,
     slave,
     slave_start_slave,
     cover_node_option,
     ct_cover_add_remove_nodes,
     otp_9956,
     cross,
     export_import,
     relative_incl_dirs,
     absolute_incl_dirs,
     relative_excl_dirs,
     absolute_excl_dirs
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%% Check that cover is collected from test node
%% Also check that cover is by default stopped after test is completed
default(Config) ->
    {ok,Events} = run_test(default,Config),
    false = check_cover(Config),
    check_calls(Events,1),
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
    check_calls(Events,2),
    ok.

%% Let test node start a slave node which in turn starts another slave
%% node - check that cover is collected from all three nodes
slave_start_slave(Config) ->
    {ok,Events} = run_test(slave_start_slave,slave_start_slave,[],Config),
    check_calls(Events,3),
    ok.

%% Start a slave node before test starts - the node is listed in cover
%% spec file.
%% Check that cover is collected from test node and slave node.
cover_node_option(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Node} = start_slave(existing_node_1, "-pa " ++ DataDir),
    false = check_cover(Node),
    CoverSpec = default_cover_file_content() ++ [{nodes,[Node]}],
    CoverFile = create_cover_file(cover_node_option,CoverSpec,Config),
    {ok,Events} = run_test(cover_node_option,cover_node_option,
			   [{cover,CoverFile}],Config),
    check_calls(Events,2),
    {ok,Node} = ct_slave:stop(existing_node_1),
    ok.

cover_node_option(cleanup,_Config) ->
    _ = ct_slave:stop(existing_node_1),
    ok.

%% Test ct_cover:add_nodes/1 and ct_cover:remove_nodes/1
%% Check that cover is collected from added node
ct_cover_add_remove_nodes(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Node} = start_slave(existing_node_2, "-pa " ++ DataDir),
    false = check_cover(Node),
    {ok,Events} = run_test(ct_cover_add_remove_nodes,ct_cover_add_remove_nodes,
			   [],Config),
    check_calls(Events,2),
    {ok,Node} = ct_slave:stop(existing_node_2),
    ok.

ct_cover_add_remove_nodes(cleanup,_Config) ->
    _ = ct_slave:stop(existing_node_2),
    ok.

%% Test that the test suite itself can be cover compiled and that
%% data_dir is set correctly (OTP-9956)
otp_9956(Config) ->
    CoverFile = create_cover_file(otp_9956,[{incl_mods,[?suite]}],Config),
    {ok,Events} = run_test(otp_9956,otp_9956,[{cover,CoverFile}],Config),
    check_calls(Events,{?suite,otp_9956,1},1),
    ok.

%% Test cross cover mechanism
cross(Config) ->
    {ok,Events1} = run_test(cross1,Config),
    check_calls(Events1,1),

    CoverFile2 = create_cover_file(cross1,[{cross,[{cross1,[?mod]}]}],Config),
    {ok,Events2} = run_test(cross2,[{cover,CoverFile2}],Config),
    check_calls(Events2,1),

    %% Get the log dirs for each test and run cross cover analyse
    [D11,D12] = lists:sort(get_log_dirs(Events1)),
    [D21,D22] = lists:sort(get_log_dirs(Events2)),

    ct_cover:cross_cover_analyse(details,[{cross1,D11},{cross2,D21}]),
    ct_cover:cross_cover_analyse(details,[{cross1,D12},{cross2,D22}]),

    %% Get the cross cover logs and read for each test
    [C11,C12,C21,C22] =
	[filename:join(D,"cross_cover.html") || D <- [D11,D12,D21,D22]],

    {ok,CrossData} = file:read_file(C11),
    {ok,CrossData} = file:read_file(C12),

    {ok,Def} = file:read_file(C21),
    {ok,Def} = file:read_file(C22),

    %% A simple test: just check that the test module exists in the
    %% log from cross1 test, and that it does not exist in the log
    %% from cross2 test.
    TestMod = list_to_binary(atom_to_list(?mod)),
    {_,_} = binary:match(CrossData,TestMod),
    nomatch = binary:match(Def,TestMod),
    {_,_} = binary:match(Def,
			 <<"No cross cover modules exist for this application">>),

    ok.

export_import(Config) ->
    DataDir = ?config(data_dir,Config),
    false = check_cover(Config),
    CoverSpec1 =
	default_cover_file_content() ++ [{export,"export_import.coverdata"}],
    CoverFile1 = create_cover_file(export_import1,CoverSpec1,Config),
    {ok,Events1} = run_test(export_import1,default,[{cover,CoverFile1}],Config),
    check_calls(Events1,1),
    CoverSpec2 =
	default_cover_file_content() ++ [{import,"export_import.coverdata"}],
    CoverFile2 = create_cover_file(export_import2,CoverSpec2,Config),
    {ok,Events2} = run_test(export_import2,default,[{cover,CoverFile2}],Config),
    check_calls(Events2,2),
    ok.

relative_incl_dirs(Config) ->
    false = check_cover(Config),
    RelDir = rel_path(?config(priv_dir, Config), ?config(data_dir, Config)),
    CoverSpec = [{incl_dirs, [RelDir]}],
    CoverFile = create_cover_file(rel_incl_dirs, CoverSpec, Config),
    Opts = [{cover, CoverFile}],
    {ok, Events} = run_test(rel_incl_dirs, default, Opts, Config),
    check_calls(Events, 1),
    ok.

absolute_incl_dirs(Config) ->
    false = check_cover(Config),
    CoverSpec = [{incl_dirs, [?config(data_dir, Config)]}],
    CoverFile = create_cover_file(abs_incl_dirs, CoverSpec, Config),
    Opts = [{cover, CoverFile}],
    {ok, Events} = run_test(abs_incl_dirs, default, Opts, Config),
    check_calls(Events, 1),
    ok.

relative_excl_dirs(Config) ->
    false = check_cover(Config),
    RelDir = rel_path(?config(priv_dir, Config), ?config(data_dir, Config)),
    CoverSpec = default_cover_file_content() ++ [{excl_dirs, [RelDir]}],
    CoverFile = create_cover_file(rel_excl_dirs, CoverSpec, Config),
    Opts = [{cover, CoverFile}],
    {ok, Events} = run_test(rel_excl_dirs, default_no_cover, Opts, Config),
    check_no_cover_compiled(Events),
    ok.

absolute_excl_dirs(Config) ->
    false = check_cover(Config),
    AbsDir = ?config(data_dir, Config),
    CoverSpec = default_cover_file_content() ++ [{excl_dirs, [AbsDir]}],
    CoverFile = create_cover_file(abs_excl_dirs, CoverSpec, Config),
    Opts = [{cover, CoverFile}],
    {ok, Events} = run_test(abs_excl_dirs, default_no_cover, Opts, Config),
    check_no_cover_compiled(Events),
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
    Suite = filename:join(DataDir, ?suite),
    CoverFile =
	case proplists:get_value(cover,ExtraOpts) of
	    undefined ->
		create_default_cover_file(Label,Config);
	    CF ->
		CF
	end,
    RestOpts = lists:keydelete(cover,1,ExtraOpts),
    {Opts,ERPid} = setup([{suite,Suite},{testcase,Testcase},
			  {cover,CoverFile},{label,Label}] ++ RestOpts, Config),
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
	[{?eh,tc_done,{?suite,Testcase,ok}}] ++
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

%% Check if a module was compiled without cover
check_no_cover_compiled(Events) ->
    check_no_cover_compiled(Events, ?mod).
check_no_cover_compiled(Events, Mod) ->
    [ {error, {not_cover_compiled, Mod}} = analyse_log(CoverLog, Mod)
      || CoverLog <- cover_logs(Events) ].

%% Check that each coverlog includes N calls to ?mod:foo/0
check_calls(Events,N) ->
    check_calls(Events,{?mod,foo,0},N).
check_calls(Events,MFA,N) ->
    do_check_logs(cover_logs(Events),MFA,N).

do_check_logs([CoverLog|CoverLogs],{Mod,_,_} = MFA,N) ->
    {ok, Calls} = analyse_log(CoverLog, Mod),
    {MFA,N} = lists:keyfind(MFA,1,Calls),
    do_check_logs(CoverLogs,MFA,N);
do_check_logs([],_,_) ->
    ok.

cover_logs(Events) ->
    [filename:join(D,"all.coverdata") || D <- get_log_dirs(Events)].

analyse_log(CoverLog, Mod) ->
    {ok, _} = cover:start(),
    ok = cover:import(CoverLog),
    Result = cover:analyse(Mod, calls, function),
    ok = cover:stop(),
    Result.

fullname(Name) ->
    {ok,Host} = inet:gethostname(),
    list_to_atom(atom_to_list(Name) ++ "@" ++ Host).

default_cover_file_content() ->
    [{incl_mods,[?mod]}].

create_default_cover_file(Filename,Config) ->
    create_cover_file(Filename,default_cover_file_content(),Config).

create_cover_file(Filename,Terms,Config) ->
    PrivDir = ?config(priv_dir,Config),
    File = filename:join(PrivDir,Filename) ++ ".cover",
    {ok,Fd} = file:open(File,[write]),
    lists:foreach(fun(Term) ->
			  file:write(Fd,io_lib:format("~p.~n",[Term]))
		  end,Terms),
    ok = file:close(Fd),
    File.

start_slave(Name,Args) ->
    {ok, HostStr}=inet:gethostname(),
    Host = list_to_atom(HostStr),
    ct_slave:start(Host,Name,
		   [{erl_flags,Args},
		    {boot_timeout,10}, % extending some timers for slow test hosts
		    {init_timeout,10},
		    {startup_timeout,10}]).

rel_path(From, To) ->
    Segments = do_rel_path(filename:split(From), filename:split(To)),
    filename:join(Segments).

do_rel_path([Seg|RestA], [Seg|RestB]) ->
    do_rel_path(RestA, RestB);
do_rel_path(PathA, PathB) ->
    lists:duplicate(length(PathA), "..") ++ PathB.
