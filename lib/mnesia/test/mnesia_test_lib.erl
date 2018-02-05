%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

%%
%%% Author: Hakan Mattsson  hakan@erix.ericsson.se
%%% Purpose: Test case support library
%%%
%%% This test suite may be run as a part of the Grand Test Suite
%%% of Erlang.  The Mnesia test suite is structured in a hierarchy.
%%% Each test case is implemented as an exported function with arity 1.
%%% Test case identifiers must have the following syntax: {Module, Function}.
%%%
%%% The driver of the test suite runs in two passes as follows:
%%% first the test case function is invoked with the atom 'suite' as
%%% single argument. The returned value is treated as a list of sub
%%% test cases. If the list of sub test cases is [] the test case
%%% function is invoked again, this time with a list of nodes as
%%% argument. If the list of sub test cases is not empty, the test
%%% case driver applies the algorithm recursively on each element
%%% in the list.
%%%
%%% All test cases are written in such a manner
%%% that they start to invoke ?acquire_nodes(X, Config)
%%% in order to prepare the test case execution. When that is
%%% done, the test machinery ensures that at least X number
%%% of nodes are connected to each other. If too few nodes was
%%% specified in the Config, the test case is skipped. If there
%%% was enough node names in the Config, X of them are selected
%%% and if some of them happens to be down they are restarted
%%% via the slave module. When all nodes are up and running a
%%% disk resident schema is created on all nodes and Mnesia is
%%% started a on all nodes. This means that all test cases may
%%% assume that Mnesia is up and running on all acquired nodes.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% doc(TestCases)
%%%
%%%    Generates a test spec from parts of the test case structure
%%%
%%% struct(TestCases)
%%%
%%%    Prints out the test case structure
%%%
%%% test(TestCases)
%%%
%%%    Run parts of the test suite. Uses test/2.
%%%    Reads Config from mnesia_test.config and starts them if neccessary.
%%%    Kills Mnesia and wipes out the Mnesia directories as a starter.
%%%
%%% test(TestCases, Config)
%%%
%%%    Run parts of the test suite on the given Nodes,
%%%    assuming that the nodes are up and running.
%%%    Kills Mnesia and wipes out the Mnesia directories as a starter.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mnesia_test_lib).
-author('hakan@erix.ericsson.se').
-export([
	 log/2,
	 log/4,
	 verbose/4,
	 default_config/0,
	 diskless/1,
	 eval_test_case/3,
	 test_driver/2,
	 test_case_evaluator/3,
	 activity_evaluator/1,
	 flush/0,
	 pick_msg/0,
	 start_activities/1,
	 start_transactions/1,
	 start_transactions/2,
	 start_sync_transactions/1,
	 start_sync_transactions/2,
	 sync_trans_tid_serial/1,
	 prepare_test_case/5,
	 select_nodes/4,
	 init_nodes/3,
	 error/4,
	 slave_start_link/0,
	 slave_start_link/1,
	 slave_sup/0,

	 start_mnesia/1,
	 start_mnesia/2,
	 start_appls/2,
	 start_appls/3,
	 start_wait/2,
	 storage_type/2,
	 stop_mnesia/1,
	 stop_appls/2,
	 sort/1,
	 kill_mnesia/1,
	 kill_appls/2,
	 verify_mnesia/4,
	 shutdown/0,
	 verify_replica_location/5,
	 lookup_config/2,
	 sync_tables/2,
	 remote_start/3,
	 remote_stop/1,
	 remote_kill/1,

	 reload_appls/2,

	 remote_activate_debug_fun/6,
	 do_remote_activate_debug_fun/6,

	 test/1,
	 test/2,
	 doc/1,
	 struct/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 kill_tc/2
	]).

-include("mnesia_test_lib.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% included for test server compatibility
%% assume that all test cases only takes Config as sole argument
init_per_testcase(_Func, Config) ->
    Env = application:get_all_env(mnesia),
    [application:unset_env(mnesia, Key, [{timeout, infinity}]) ||
	{Key, _} <- Env, Key /= included_applications],
    global:register_name(mnesia_global_logger, group_leader()),
    Config.

end_per_testcase(_Func, Config) ->
    global:unregister_name(mnesia_global_logger),
    %% Nodes = select_nodes(all, Config, ?FILE, ?LINE),
    %% rpc:multicall(Nodes, mnesia, lkill, []),
    Config.

%% Use ?log(Format, Args) as wrapper
log(Format, Args, LongFile, Line) ->
    File = filename:basename(LongFile),
    Format2 = lists:concat([File, "(", Line, ")", ": ", Format]),
    log(Format2, Args).

log(Format, Args) ->
    case global:whereis_name(mnesia_global_logger) of
	undefined ->
	    io:format(user, Format, Args);
	Pid ->
	    io:format(Pid, Format, Args)
    end.

verbose(Format, Args, File, Line) ->
    Arg = mnesia_test_verbose,
    case get(Arg) of
	false ->
	    ok;
	true ->
	    log(Format, Args, File, Line);
	undefined ->
	    case init:get_argument(Arg) of
		{ok, List} when is_list(List) ->
		    case lists:last(List) of
			["true"] ->
			    put(Arg, true),
			    log(Format, Args, File, Line);
			_ ->
			    put(Arg, false),
			    ok
		    end;
		_ ->
		    put(Arg, false),
		    ok
	    end
    end.

-record('REASON', {file, line, desc}).

error(Format, Args, File, Line) ->
    global:send(mnesia_global_logger, {failed, File, Line}),
    Fail = #'REASON'{file = filename:basename(File),
		     line = Line,
		     desc = Args},
    case global:whereis_name(mnesia_test_case_sup) of
	undefined ->
	    ignore;
	Pid ->
	    Pid ! Fail
%% 	    global:send(mnesia_test_case_sup, Fail),
    end,
    log("<>ERROR<>~n" ++ Format, Args, File, Line).

storage_type(Default, Config) ->
    case diskless(Config) of
	true ->
	    ram_copies;
	false ->
	    Default
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_config() ->
    [{nodes, default_nodes()}].

default_nodes() ->
    mk_nodes(3, []).

mk_nodes(0, Nodes) ->
    Nodes;
mk_nodes(N, []) ->
    mk_nodes(N - 1, [node()]);
mk_nodes(N, Nodes) when N > 0 ->
    Head = hd(Nodes),
    [Name, Host] = node_to_name_and_host(Head),
    Nodes ++ [mk_node(I, Name, Host) || I <- lists:seq(1, N)].

mk_node(N, Name, Host) ->
    list_to_atom(lists:concat([Name ++ integer_to_list(N) ++ "@" ++ Host])).

slave_start_link() ->
    slave_start_link(node()).

slave_start_link(Node) ->
    [Local, Host] = node_to_name_and_host(Node),
    Count = erlang:unique_integer([positive]),
    List = [Local, "_", Count],
    Name = list_to_atom(lists:concat(List)),
    slave_start_link(list_to_atom(Host), Name).

slave_start_link(Host, Name) ->
    slave_start_link(Host, Name, 10).

slave_start_link(Host, Name, Retries) ->
    Debug = atom_to_list(mnesia:system_info(debug)),
    Args = "-mnesia debug " ++ Debug ++
	" -pa " ++
	filename:dirname(code:which(?MODULE)) ++
	" -pa " ++
	filename:dirname(code:which(mnesia)),
    case starter(Host, Name, Args) of
	{ok, NewNode} ->
	    ?match(pong, net_adm:ping(NewNode)),
	    {ok, Cwd} = file:get_cwd(),
	    Path = code:get_path(),
	    ok = rpc:call(NewNode, file, set_cwd, [Cwd]),
	    true = rpc:call(NewNode, code, set_path, [Path]),
	    ok = rpc:call(NewNode, error_logger, tty, [false]),
	    spawn_link(NewNode, ?MODULE, slave_sup, []),
	    rpc:multicall([node() | nodes()], global, sync, []),
	    {ok, NewNode};
	{error, Reason} when Retries == 0->
	    {error, Reason};
	{error, Reason} ->
	    io:format("Could not start slavenode ~p ~p retrying~n",
		      [{Host, Name, Args}, Reason]),
	    timer:sleep(500),
	    slave_start_link(Host, Name, Retries - 1)
    end.

starter(Host, Name, Args) ->
    slave:start(Host, Name, Args).

slave_sup() ->
    process_flag(trap_exit, true),
    receive
	{'EXIT', _, _} ->
	    ignore
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Index the test case structure

doc(TestCases) when is_list(TestCases) ->
    test(TestCases, suite),
    SuiteFname = "index.html",
    io:format("Generating HTML test specification to file: ~s~n",
	      [SuiteFname]),
    {ok, Fd} = file:open(SuiteFname, [write]),
    io:format(Fd, "<TITLE>Test specification for ~p</TITLE>.~n", [TestCases]),
    io:format(Fd, "<H1>Test specification for ~p</H1>~n", [TestCases]),
    io:format(Fd, "Test cases which not are implemented yet are written in <B>bold face</B>.~n~n", []),

    io:format(Fd, "<BR><BR>~n", []),
    io:format(Fd, "~n<DL>~n", []),
    do_doc(Fd, TestCases, []),
    io:format(Fd, "</DL>~n", []),
    file:close(Fd);
doc(TestCases) ->
    doc([TestCases]).

do_doc(Fd, [H | T], List) ->
    case H of
	{Module, TestCase} when is_atom(Module), is_atom(TestCase) ->
	    do_doc(Fd, Module, TestCase, List);
	TestCase when is_atom(TestCase), List == [] ->
	    do_doc(Fd, mnesia_SUITE, TestCase, List);
	TestCase when is_atom(TestCase) ->
	    do_doc(Fd, hd(List), TestCase, List)
    end,
    do_doc(Fd, T, List);
do_doc(_, [], _) ->
    ok.

do_doc(Fd, Module, TestCase, List) ->
    case get_suite(Module, TestCase) of
	[] ->
	    %% Implemented leaf test case
	    Head = ?flat_format("<A HREF=~p.html#~p_1>{~p, ~p}</A>}",
				[Module, TestCase, Module, TestCase]),
	    print_doc(Fd, Module, TestCase, Head);
	Suite when is_list(Suite) ->
	    %% Test suite
	    Head = ?flat_format("{~p, ~p}", [Module, TestCase]),
	    print_doc(Fd, Module, TestCase, Head),
	    io:format(Fd, "~n<DL>~n", []),
	    do_doc(Fd, Suite, [Module | List]),
	    io:format(Fd, "</DL>~n", []);
	'NYI' ->
	    %% Not yet implemented
	    Head = ?flat_format("<B>{~p, ~p}</B>", [Module, TestCase]),
	    print_doc(Fd, Module, TestCase, Head)
    end.

print_doc(Fd, Mod, Fun, Head) ->
    case catch (apply(Mod, Fun, [doc])) of
	{'EXIT', _} ->
	    io:format(Fd, "<DT>~s</DT>~n", [Head]);
	Doc when is_list(Doc) ->
	    io:format(Fd, "<DT><U>~s</U><BR><DD>~n", [Head]),
	    print_rows(Fd, Doc),
	    io:format(Fd, "</DD><BR><BR>~n", [])
    end.

print_rows(_Fd, []) ->
    ok;
print_rows(Fd, [H | T]) when is_list(H) ->
    io:format(Fd, "~s~n", [H]),
    print_rows(Fd, T);
print_rows(Fd, [H | T]) when is_integer(H) ->
    io:format(Fd, "~s~n", [[H | T]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Show the test case structure

struct(TestCases) ->
    T = test(TestCases, suite),
    struct(T, "").

struct({Module, TestCase}, Indentation)
        when is_atom(Module), is_atom(TestCase) ->
    log("~s{~p, ~p} ...~n", [Indentation, Module, TestCase]);
struct({Module, TestCase, Other}, Indentation)
        when is_atom(Module), is_atom(TestCase) ->
    log("~s{~p, ~p} ~p~n", [Indentation, Module, TestCase, Other]);
struct([], _) ->
    ok;
struct([TestCase | TestCases], Indentation) ->
    struct(TestCase, Indentation),
    struct(TestCases, Indentation);
struct({TestCase, []}, Indentation) ->
    struct(TestCase, Indentation);
struct({TestCase, SubTestCases}, Indentation) when is_list(SubTestCases) ->
    struct(TestCase, Indentation),
    struct(SubTestCases, Indentation ++ "  ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Execute the test cases

test(TestCases) ->
    test(TestCases, []).

test(TestCases, suite) when is_list(TestCases) ->
    test_driver(TestCases, suite);
test(TestCases, Config) when is_list(TestCases) ->
    D1 = lists:duplicate(10, $=),
    D2 =  lists:duplicate(10, $ ),
    log("~n~s TEST CASES: ~p~n ~sCONFIG: ~p~n~n", [D1, TestCases, D2, Config]),
    test_driver(TestCases, Config);
test(TestCase, Config) ->
    test([TestCase], Config).

test_driver([], _Config) ->
    [];
test_driver([T|TestCases], Config) ->
    L1 = test_driver(T, Config),
    L2 = test_driver(TestCases, Config),
    [L1|L2];
test_driver({Module, TestCases}, Config) when is_list(TestCases)->
    test_driver(default_module(Module, TestCases), Config);
test_driver({Module, all}, Config) ->
    get_suite(Module, all, Config);
test_driver({Module, G={group, _}}, Config) ->
    get_suite(Module, G, Config);
test_driver({_, {group, Module, Group}}, Config) ->
    get_suite(Module, {group, Group}, Config);

test_driver({Module, TestCase}, Config) ->
    Sec = timer:seconds(1) * 1000,
    case Config of
	suite ->
	    {Module, TestCase, 'IMPL'};
	_ ->
	    log("Eval test case: ~w~n", [{Module, TestCase}]),
	    try timer:tc(?MODULE, eval_test_case, [Module, TestCase, Config]) of
		{T, Res} ->
		    log("Tested ~w in ~w sec~n", [TestCase, T div Sec]),
		    {T div Sec, Res}
	    catch error:function_clause ->
		    log("<WARNING> Test case ~w NYI~n", [{Module, TestCase}]),
		    {0, {skip, {Module, TestCase}, "NYI"}}
	    end
    end;
test_driver(TestCase, Config) ->
    DefaultModule = mnesia_SUITE,
    log("<>WARNING<> Missing module in test case identifier. "
	"{~w, ~w} assumed~n", [DefaultModule, TestCase]),
    test_driver({DefaultModule, TestCase}, Config).

default_module(DefaultModule, TestCases) when is_list(TestCases) ->
    Fun = fun(T) ->
		  case T of
		      {group, _} -> {true, {DefaultModule, T}};
		      {_, _} -> true;
		      T -> {true, {DefaultModule, T}}
		  end
	  end,
    lists:zf(Fun, TestCases).

get_suite(Module, TestCase, Config) ->
    case get_suite(Module, TestCase) of
	Suite when is_list(Suite), Config == suite ->
	    Res = test_driver(default_module(Module, Suite), Config),
	    {{Module, TestCase}, Res};
	Suite when is_list(Suite) ->
	    log("Expand test case ~w~n", [{Module, TestCase}]),
	    Def = default_module(Module, Suite),
	    {T, Res} = timer:tc(?MODULE, test_driver, [Def, Config]),
	    Sec = timer:seconds(1) * 1000,
	    {T div Sec, {{Module, TestCase}, Res}};
	'NYI' when Config == suite ->
	    {Module, TestCase, 'NYI'};
	'NYI' ->
      	    log("<WARNING> Test case ~w NYI~n", [{Module, TestCase}]),
	    {0, {skip, {Module, TestCase}, "NYI"}}
    end.

%% Returns a list (possibly empty) or the atom 'NYI'
get_suite(Mod, {group, Suite}) ->
    try
	Groups = Mod:groups(),
	{_, _, TCList} = lists:keyfind(Suite, 1, Groups),
	TCList
    catch
	_:Reason:Stacktrace ->
	    io:format("Not implemented ~p ~p (~p ~p)~n",
		      [Mod,Suite,Reason,Stacktrace]),
	    'NYI'
    end;
get_suite(Mod, all) ->
    case catch (apply(Mod, all, [])) of
	{'EXIT', _} -> 'NYI';
	List when is_list(List) -> List
    end;
get_suite(_Mod, _Fun) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_test_case(Mod, Fun, Config) ->
    flush(),
    global:register_name(mnesia_test_case_sup, self()),
    Flag = process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, test_case_evaluator, [Mod, Fun, [Config]]),
    R = wait_for_evaluator(Pid, Mod, Fun, Config),
    global:unregister_name(mnesia_test_case_sup),
    process_flag(trap_exit, Flag),
    R.

flush() ->
    receive Msg -> [Msg | flush()]
    after 0 -> []
    end.

wait_for_evaluator(Pid, Mod, Fun, Config) ->
    receive
	{'EXIT', Pid, {test_case_ok, _PidRes}} ->
	    Errors = flush(),
	    Res =
		case Errors of
		    [] -> ok;
		    Errors -> failed
		end,
	    {Res, {Mod, Fun}, Errors};
	{'EXIT', Pid, {skipped, Reason}} ->
	    log("<WARNING> Test case ~w skipped, because ~p~n",
		[{Mod, Fun}, Reason]),
	    Mod:end_per_testcase(Fun, Config),
	    {skip, {Mod, Fun}, Reason};
	{'EXIT', Pid, Reason} ->
	    log("<>ERROR<> Eval process ~w exited, because ~p~n",
		[{Mod, Fun}, Reason]),
	    Mod:end_per_testcase(Fun, Config),
	    {crash, {Mod, Fun}, Reason}
    end.

test_case_evaluator(Mod, Fun, [Config]) ->
    NewConfig = Mod:init_per_testcase(Fun, Config),
    try
	R = apply(Mod, Fun, [NewConfig]),
	Mod:end_per_testcase(Fun, NewConfig),
	exit({test_case_ok, R})
    catch error:function_clause ->
	    exit({skipped, 'NYI'})
    end.

activity_evaluator(Coordinator) ->
    activity_evaluator_loop(Coordinator),
    exit(normal).

activity_evaluator_loop(Coordinator) ->
    receive
	begin_trans ->
	    transaction(Coordinator, 0);
	{begin_trans, MaxRetries} ->
	    transaction(Coordinator, MaxRetries);
	end_trans ->
	    end_trans;
	Fun when is_function(Fun) ->
	    Coordinator ! {self(), Fun()},
	    activity_evaluator_loop(Coordinator);
%	{'EXIT', Coordinator, Reason} ->
%	    Reason;
	ExitExpr ->
%	    ?error("activity_evaluator_loop ~p ~p: exit(~p)~n}", [Coordinator, self(), ExitExpr]),
	    exit(ExitExpr)
    end.

transaction(Coordinator, MaxRetries) ->
    Fun = fun() ->
		  Coordinator ! {self(), begin_trans},
		  activity_evaluator_loop(Coordinator)
	  end,
    Coordinator ! {self(), mnesia:transaction(Fun, MaxRetries)},
    activity_evaluator_loop(Coordinator).

pick_msg() ->
    receive
	Message -> Message
    after 4000 -> timeout
    end.

start_activities(Nodes) ->
    Fun = fun(N) -> spawn_link(N, ?MODULE, activity_evaluator, [self()]) end,
    Pids = mapl(Fun, Nodes),
    {success, Pids}.

mapl(Fun, [H|T]) ->
    Res = Fun(H),
    [Res|mapl(Fun, T)];
mapl(_Fun, []) ->
    [].

diskless(Config) ->
    case lists:keysearch(diskless, 1, Config) of
	{value, {diskless, true}} ->
	    true;
	_Else ->
	    false
    end.


start_transactions(Pids) ->
    Fun = fun(Pid) ->
		  Pid ! begin_trans,
		  ?match_receive({Pid, begin_trans})
	  end,
    mapl(Fun, Pids).

start_sync_transactions(Pids) ->
    Nodes = [node(Pid) || Pid <- Pids],
    Fun = fun(Pid) ->
		  sync_trans_tid_serial(Nodes),
		  Pid ! begin_trans,
		  ?match_receive({Pid, begin_trans})
	  end,
    mapl(Fun, Pids).


start_transactions(Pids, MaxRetries) ->
    Fun = fun(Pid) ->
		  Pid ! {begin_trans, MaxRetries},
		  ?match_receive({Pid, begin_trans})
	  end,
    mapl(Fun, Pids).

start_sync_transactions(Pids, MaxRetries) ->
    Nodes = [node(Pid) || Pid <- Pids],
    Fun = fun(Pid) ->
		  sync_trans_tid_serial(Nodes),
		  Pid ! {begin_trans, MaxRetries},
		  ?match_receive({Pid, begin_trans})
	  end,
    mapl(Fun, Pids).

sync_trans_tid_serial(Nodes) ->
    Fun = fun() -> mnesia:write_lock_table(schema) end,
    rpc:multicall(Nodes, mnesia, transaction, [Fun]).

select_nodes(N, Config, File, Line) ->
    prepare_test_case([], N, Config, File, Line).

prepare_test_case(Actions, N, Config, File, Line) ->
    NodeList1 = lookup_config(nodes, Config),
    NodeList2 = lookup_config(nodenames, Config), %% For testserver
    NodeList3 = append_unique(NodeList1, NodeList2),
    This = node(),
    All = [This | lists:delete(This, NodeList3)],
    Selected = pick_nodes(N, All, File, Line),
    case diskless(Config) of
	true ->
	    ok;
	false ->
	    rpc:multicall(Selected, application, set_env,[mnesia, schema_location, opt_disc])
    end,
    do_prepare(Actions, Selected, All, Config, File, Line).

do_prepare([], Selected, _All, _Config, _File, _Line) ->
    Selected;
do_prepare([{init_test_case, Appls} | Actions], Selected, All, Config, File, Line) ->
    set_kill_timer(Config),
    Started = init_nodes(Selected, File, Line),
    All2 = append_unique(Started, All),
    Alive = mnesia_lib:intersect(nodes() ++ [node()], All2),
    kill_appls(Appls, Alive),
    process_flag(trap_exit, true),
    do_prepare(Actions, Started, All2, Config, File, Line);
do_prepare([delete_schema | Actions], Selected, All, Config, File, Line) ->
    Alive = mnesia_lib:intersect(nodes() ++ [node()], All),
    case diskless(Config) of
	true ->
	    skip;
	false ->
	    Del = fun(Node) ->
			  case mnesia:delete_schema([Node]) of
			      ok -> ok;
			      {error, {"All nodes not running",_}} ->
				  ok;
			      Else ->
				  ?log("Delete schema error ~p ~n", [Else])
			  end
		  end,
	    lists:foreach(Del, Alive)
    end,
    do_prepare(Actions, Selected, All, Config, File, Line);
do_prepare([create_schema | Actions], Selected, All, Config, File, Line) ->
    Ext = ?BACKEND,
    case diskless(Config) of
	true ->
	    rpc:multicall(Selected, application, set_env, [mnesia, schema, Ext]),
	    skip;
	_Else ->
	    case mnesia:create_schema(Selected, Ext) of
		ok ->
		    ignore;
		BadNodes ->
		    ?fatal("Cannot create Mnesia schema on ~p~n", [BadNodes])
	    end
    end,
    do_prepare(Actions, Selected, All, Config, File, Line);
do_prepare([{start_appls, Appls} | Actions], Selected, All, Config, File, Line) ->
    case start_appls(Appls, Selected, Config) of
	[] -> ok;
	Bad -> ?fatal("Cannot start appls ~p: ~p~n", [Appls, Bad])
    end,
    do_prepare(Actions, Selected, All, Config, File, Line);
do_prepare([{reload_appls, Appls} | Actions], Selected, All, Config, File, Line) ->
    reload_appls(Appls, Selected),
    do_prepare(Actions, Selected, All, Config, File, Line).

set_kill_timer(Config) ->
    case init:get_argument(mnesia_test_timeout) of
	{ok, _ } -> ok;
	_ ->
	    Time0 =
		case lookup_config(tc_timeout, Config) of
		    [] -> timer:minutes(5);
		    ConfigTime when is_integer(ConfigTime) -> ConfigTime
		end,
	    Mul = try
		      test_server:timetrap_scale_factor()
		  catch _:_ -> 1 end,
	    (catch test_server:timetrap(Mul*Time0 + 1000)),
	    spawn_link(?MODULE, kill_tc, [self(),Time0*Mul])
    end.

kill_tc(Pid, Time) ->
    receive
    after Time ->
	    case process_info(Pid) of
		undefined ->  ok;
		_ ->
		    ?error("Watchdog in test case timed out "
			   "in ~p min~n", [Time div (1000*60)]),
		    Files = mnesia_lib:dist_coredump(),
		    ?log("Cores dumped to:~n ~p~n", [Files]),
		    %% Genarate erlang crashdumps.
		    %% GenDump = fun(Node) ->
		    %% 		      File = "CRASH_" ++ atom_to_list(Node) ++ ".dump",
		    %% 		      rpc:call(Node, os, putenv, ["ERL_CRASH_DUMP", File]),
		    %% 		      rpc:cast(Node, erlang, halt, ["RemoteTimeTrap"])
		    %% 	      end,
		    %% [GenDump(Node) || Node <- nodes()],

		    %% erlang:halt("DebugTimeTrap"),
		    exit(Pid, kill)
	    end
    end.


append_unique([], List) -> List;
append_unique([H|R], List) ->
    case lists:member(H, List) of
	true -> append_unique(R, List);
	false -> [H | append_unique(R, List)]
    end.

pick_nodes(all, Nodes, File, Line) ->
    pick_nodes(length(Nodes), Nodes, File, Line);
pick_nodes(N, [H | T], File, Line) when N > 0 ->
    [H | pick_nodes(N - 1, T, File, Line)];
pick_nodes(0, _Nodes, _File, _Line) ->
    [];
pick_nodes(N, [], File, Line) ->
    ?skip("Test case (~p(~p)) ignored: ~p nodes missing~n",
	  [File, Line, N]).

init_nodes([Node | Nodes], File, Line) ->
    case net_adm:ping(Node) of
	pong ->
	    [Node | init_nodes(Nodes, File, Line)];
	pang ->
	    [Name, Host] = node_to_name_and_host(Node),
	    case slave_start_link(Host, Name) of
		{ok, Node1} ->
		    Path = code:get_path(),
		    true = rpc:call(Node1, code, set_path, [Path]),
		    [Node1 | init_nodes(Nodes, File, Line)];
		Other ->
		    ?skip("Test case (~p(~p)) ignored: cannot start node ~p: ~p~n",
			  [File, Line, Node, Other])
	    end
    end;
init_nodes([], _File, _Line) ->
    [].

%% Returns [Name, Host]
node_to_name_and_host(Node) ->
    string:lexemes(atom_to_list(Node), [$@]).

lookup_config(Key,Config) ->
    case lists:keysearch(Key,1,Config) of
	{value,{Key,Val}} ->
	    Val;
	_ ->
	    []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_appls(Appls, Nodes) ->
    start_appls(Appls, Nodes, [],  [schema]).

start_appls(Appls, Nodes, Config) ->
    start_appls(Appls, Nodes, Config, [schema]).

start_appls([Appl | Appls], Nodes, Config, Tabs) ->
    {Started, BadStarters} =
	rpc:multicall(Nodes, ?MODULE, remote_start, [Appl, Config, Nodes]),
    BadS = [{Node, Appl, Res} || {Node, Res} <- Started, Res /= ok],
    BadN = [{BadNode, Appl, bad_start} || BadNode <- BadStarters],
    Bad = BadS ++ BadN,
    case Appl of
	mnesia when Bad == [] ->
	    sync_tables(Nodes, Tabs);
	_ ->
	    ignore
    end,
    Bad ++ start_appls(Appls, Nodes, Config, Tabs);
start_appls([], _Nodes, _Config, _Tabs) ->
    [].

remote_start(mnesia, Config, Nodes) ->
    case diskless(Config) of
	true ->
	    application_controller:set_env(mnesia,
					   extra_db_nodes,
					   Nodes -- [node()]),
	    application_controller:set_env(mnesia,
					   schema_location,
					   ram);
	false ->
	    application_controller:set_env(mnesia,
					   schema_location,
					   opt_disc),
	    ignore
    end,
    {node(), mnesia:start()};
remote_start(Appl, _Config, _Nodes) ->
    Res =
	case application:start(Appl) of
	    {error, {already_started, Appl}} ->
		ok;
	    Other ->
		Other
	end,
    {node(), Res}.

%% Start Mnesia on all given nodes and wait for specified
%% tables to be accessible on each node. The atom all means
%% that we should wait for all tables to be loaded
%%
%% Returns a list of error tuples {BadNode, mnesia, Reason}
start_mnesia(Nodes) ->
    start_appls([mnesia], Nodes).
start_mnesia(Nodes, Tabs) when is_list(Nodes) ->
    start_appls([mnesia], Nodes, [], Tabs).

%% Wait for the tables to be accessible from all nodes in the list
%% and that all nodes are aware of that the other nodes also ...
sync_tables(Nodes, Tabs) ->
    Res = send_wait(Nodes, Tabs, []),
    if
	Res == 	[] ->
	    mnesia:transaction(fun() -> mnesia:write_lock_table(schema) end),
	    Res;
	true ->
	    Res
    end.

send_wait([Node | Nodes], Tabs, Pids) ->
    Pid = spawn_link(Node, ?MODULE, start_wait, [self(), Tabs]),
    send_wait(Nodes, Tabs, [Pid | Pids]);
send_wait([], _Tabs, Pids) ->
    rec_wait(Pids, []).

rec_wait([Pid | Pids], BadRes) ->
    receive
	{'EXIT', Pid, R} ->
	    rec_wait(Pids, [{node(Pid), bad_wait, R} | BadRes]);
	{Pid, ok} ->
	    rec_wait(Pids, BadRes);
	{Pid, {error, R}} ->
	    rec_wait(Pids, [{node(Pid), bad_wait, R} | BadRes])
    end;
rec_wait([], BadRes) ->
    BadRes.

start_wait(Coord, Tabs) ->
    process_flag(trap_exit, true),
    Mon = whereis(mnesia_monitor),
    case catch link(Mon) of
	{'EXIT', _} ->
	    unlink(Coord),
	    Coord ! {self(), {error, {node_not_running, node()}}};
	_ ->
	    Res = start_wait_loop(Tabs),
	    unlink(Mon),
	    unlink(Coord),
	    Coord ! {self(), Res}
    end.

start_wait_loop(Tabs) ->
    receive
	{'EXIT', Pid, Reason} ->
	    {error, {start_wait, Pid, Reason}}
    after 0 ->
	    case mnesia:wait_for_tables(Tabs, timer:seconds(30)) of
		ok ->
		    verify_nodes(Tabs);
		{timeout, BadTabs} ->
		    log("<>WARNING<> Wait for tables ~p: ~p~n", [node(), Tabs]),
		    start_wait_loop(BadTabs);
		{error, Reason} ->
		    {error, {start_wait, Reason}}
	    end
    end.

verify_nodes(Tabs) ->
    verify_nodes(Tabs, 0).

verify_nodes([], _) ->
    ok;

verify_nodes([Tab| Tabs], N) ->
    ?match(X when is_atom(X), mnesia_lib:val({Tab, where_to_read})),
    Nodes = mnesia:table_info(Tab, where_to_write),
    Copies =
	mnesia:table_info(Tab, disc_copies) ++
        mnesia:table_info(Tab, disc_only_copies) ++
	mnesia:table_info(Tab, ram_copies),
    Local = mnesia:table_info(Tab, local_content),
    case Copies -- Nodes of
	[] ->
	    verify_nodes(Tabs, 0);
	_Else when Local == true, Nodes /= [] ->
	    verify_nodes(Tabs, 0);
        Else ->
	    N2 =
		if
		    N > 20 ->
			log("<>WARNING<> ~w Waiting for table: ~p on ~p ~n",
				 [node(), Tab, Else]),
			0;
		    true -> N+1
		end,
	    timer:sleep(500),
	    verify_nodes([Tab| Tabs], N2)
    end.


%% Nicely stop Mnesia on all given nodes
%%
%% Returns a list of error tuples {BadNode, Reason}
stop_mnesia(Nodes) when is_list(Nodes) ->
    stop_appls([mnesia], Nodes).

stop_appls([Appl | Appls], Nodes) when is_list(Nodes) ->
    {Stopped, BadNodes} = rpc:multicall(Nodes, ?MODULE, remote_stop, [Appl]),
    BadS =[{Node, Appl, Res} || {Node, Res} <- Stopped, Res /= stopped],
    BadN =[{BadNode, Appl, bad_node} || BadNode <- BadNodes],
    BadS ++ BadN ++ stop_appls(Appls, Nodes);
stop_appls([], _Nodes) ->
    [].

remote_stop(mnesia) ->
    {node(), mnesia:stop()};
remote_stop(Appl) ->
    {node(), application:stop(Appl)}.

remote_kill([Appl | Appls]) ->
    catch Appl:lkill(),
    application:stop(Appl),
    remote_kill(Appls);
remote_kill([]) ->
    ok.

%% Abruptly kill Mnesia on all given nodes
%% Returns []
kill_appls(Appls, Nodes) when is_list(Nodes) ->
    verbose("<>WARNING<> Intentionally killing ~p: ~w...~n",
	    [Appls, Nodes], ?FILE, ?LINE),
    rpc:multicall(Nodes, ?MODULE, remote_kill, [Appls]),
    [].

kill_mnesia(Nodes) when is_list(Nodes) ->
    kill_appls([mnesia], Nodes).

reload_appls([Appl | Appls], Selected) ->
    kill_appls([Appl], Selected),
    timer:sleep(1000),
    Ok = {[ok || _N <- Selected], []},
    {Ok2temp, Empty} = rpc:multicall(Selected, application, unload, [Appl]),
    Conv = fun({error,{not_loaded,mnesia}}) -> ok; (Else) -> Else end,
    Ok2 = {lists:map(Conv, Ok2temp), Empty},
    Ok3 = rpc:multicall(Selected, application, load, [Appl]),
    if
	Ok /= Ok2 ->
	    ?fatal("Cannot unload appl ~p: ~p~n", [Appl, Ok2]);
	Ok /= Ok3 ->
	    ?fatal("Cannot load appl ~p: ~p~n", [Appl, Ok3]);
	true ->
	    ok
    end,
    reload_appls(Appls, Selected);
reload_appls([], _Selected) ->
    ok.

shutdown() ->
    log("<>WARNING<> Intentionally shutting down all nodes... ~p~n",
	 [nodes() ++ [node()]]),
    rpc:multicall(nodes(), erlang, halt, []),
    erlang:halt().

verify_mnesia(Ups, Downs, File, Line) when is_list(Ups), is_list(Downs) ->
    BadUps =
	[N || N <- Ups, rpc:call(N, mnesia, system_info, [is_running]) /= yes],
    BadDowns =
	[N || N <- Downs, rpc:call(N, mnesia, system_info, [is_running]) == yes],
    if
	BadUps == [] ->
	    ignore;
	true ->
	    error("Mnesia is not running as expected: ~p~n",
		  [BadUps], File, Line)
    end,
    if
	BadDowns == [] ->
	    ignore;
	true ->
	    error("Mnesia is not stopped as expected: ~p~n",
		  [BadDowns], File, Line)
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_replica_location(Tab, [], [], [], _) ->
    ?match({'EXIT', _}, mnesia:table_info(Tab, ram_copies)),
    ?match({'EXIT', _}, mnesia:table_info(Tab, disc_copies)),
    ?match({'EXIT', _}, mnesia:table_info(Tab, disc_only_copies)),
    ?match({'EXIT', _}, mnesia:table_info(Tab, where_to_write)),
    ?match({'EXIT', _}, mnesia:table_info(Tab, where_to_read)),
    [];

verify_replica_location(Tab, DiscOnly0, Ram0, Disc0, AliveNodes0) ->
%%    sync_tables(AliveNodes0, [Tab]),
    AliveNodes = lists:sort(AliveNodes0),
    DiscOnly = lists:sort(DiscOnly0),
    Ram = lists:sort(Ram0),
    Disc = lists:sort(Disc0),
    Write = ignore_dead(DiscOnly ++ Ram ++ Disc, AliveNodes),
    Read = ignore_dead(DiscOnly ++ Ram ++ Disc, AliveNodes),
    This = node(),

    timer:sleep(100),

    S1 = ?match(AliveNodes, lists:sort(mnesia:system_info(running_db_nodes))),
    S2 = ?match(DiscOnly, lists:sort(mnesia:table_info(Tab, disc_only_copies))),
    S3 = ?match(Ram, lists:sort(mnesia:table_info(Tab, ram_copies) ++
				    mnesia:table_info(Tab, ext_ets))),
    S4 = ?match(Disc, lists:sort(mnesia:table_info(Tab, disc_copies))),
    S5 = ?match(Write, lists:sort(mnesia:table_info(Tab, where_to_write))),
    S6 = case lists:member(This, Read) of
	     true ->
		 ?match(This, mnesia:table_info(Tab, where_to_read));
	     false ->
		 ?match(true, lists:member(mnesia:table_info(Tab, where_to_read), Read))
	 end,
    lists:filter(fun({success,_}) -> false; (_) -> true end, [S1,S2,S3,S4,S5,S6]).

ignore_dead(Nodes, AliveNodes) ->
    Filter = fun(Node) -> lists:member(Node, AliveNodes) end,
    lists:sort(lists:zf(Filter, Nodes)).


remote_activate_debug_fun(N, I, F, C, File, Line) ->
    Pid = spawn_link(N, ?MODULE, do_remote_activate_debug_fun, [self(), I, F, C, File, Line]),
    receive
	{activated, Pid} -> ok;
	{'EXIT', Pid, Reason} -> {error, Reason}
    end.

do_remote_activate_debug_fun(From, I, F, C, File, Line) ->
    mnesia_lib:activate_debug_fun(I, F, C, File, Line),
    From ! {activated, self()},
    timer:sleep(infinity).  % Dies whenever the test process dies !!


sort(L) when is_list(L) ->
    lists:sort(L);
sort({atomic, L}) when is_list(L) ->
    {atomic, lists:sort(L)};
sort({ok, L}) when is_list(L) ->
    {ok, lists:sort(L)};
sort(W) ->
    W.
