%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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

%% 
%%----------------------------------------------------------------------
%% Purpose: Lightweight test server
%%----------------------------------------------------------------------
%% 

-module(diameter_test_lib).

-export([
	 sleep/1,
	 
	 hours/1,
	 minutes/1,
	 seconds/1,

	 key1search/2, 

	 non_pc_tc_maybe_skip/4,
	 os_based_skip/1,

	 fail/3, 
	 skip/3, 
	 fatal_skip/3,
	 
	 log/4, 
	 error/3,

	 flush/0,

	 proxy_start/1, proxy_start/2, 
	 proxy_init/2,

	 still_alive/1, 

	 prepare_test_case/5,
	 lookup_config/2, 

	 mk_nodes/2, start_nodes/3,

	 display_system_info/1, display_system_info/2, display_system_info/3, 
	 display_alloc_info/0,
	 alloc_info/0,

	 report_event/3
	 
	]).

-include("diameter_test_lib.hrl").

-record('REASON', {mod, line, desc}).


%% ----------------------------------------------------------------
%% Time related function
%%

sleep(infinity) ->
    receive
    after infinity ->
            ok
    end;
sleep(MSecs) ->
    receive
    after trunc(MSecs) ->
            ok
    end,
    ok.


hours(N)   -> trunc(N * 1000 * 60 * 60).
minutes(N) -> trunc(N * 1000 * 60).
seconds(N) -> trunc(N * 1000).


%% ----------------------------------------------------------------

key1search(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	undefined ->
	    fail({not_found, Key, L}, ?MODULE, ?LINE);
	{value, {Key, Value}} ->
	    Value
    end.


%% ----------------------------------------------------------------
%% Conditional skip of testcases
%%

non_pc_tc_maybe_skip(Config, Condition, File, Line) 
  when is_list(Config) andalso is_function(Condition) ->
    %% Check if we shall skip the skip
    case os:getenv("TS_OS_BASED_SKIP") of
	"false" ->
	    ok;
	_ ->
	    case lists:keysearch(ts, 1, Config) of
		{value, {ts, megaco}} ->
		    %% Always run the testcase if we are using our own 
		    %% test-server...
		    ok;
		_ ->
		    case (catch Condition()) of
			true ->
			    skip(non_pc_testcase, File, Line);
			_ ->
			    ok
		    end
	    end
    end.


os_based_skip(any) ->
    true;
os_based_skip(Skippable) when is_list(Skippable) ->
    {OsFam, OsName} = 
	case os:type() of
	    {_Fam, _Name} = FamAndName ->
		FamAndName;
	    Fam ->
		{Fam, undefined}
	end,
    case lists:member(OsFam, Skippable) of
	true ->
	    true;
	false ->
	    case lists:keysearch(OsFam, 1, Skippable) of
		{value, {OsFam, OsName}} ->
		    true;
		{value, {OsFam, OsNames}} when is_list(OsNames) ->
		    lists:member(OsName, OsNames);
		_ ->
		    false
	    end
    end;
os_based_skip(_) ->
    false.
    
	    
%%----------------------------------------------------------------------

error(Actual, Mod, Line) ->
    global:send(megaco_global_logger, {failed, Mod, Line}),
    log("<ERROR> Bad result: ~p~n", [Actual], Mod, Line),
    Label = lists:concat([Mod, "(", Line, ") unexpected result"]),
    report_event(60, Label, [{line, Mod, Line}, {error, Actual}]),
    case global:whereis_name(megaco_test_case_sup) of
	undefined -> 
	    ignore;
	Pid -> 
	    Fail = #'REASON'{mod = Mod, line = Line, desc = Actual},
	    Pid ! {fail, self(), Fail}
    end,
    Actual.

log(Format, Args, Mod, Line) ->
    case global:whereis_name(megaco_global_logger) of
	undefined ->
	    io:format(user, "~p~p(~p): " ++ Format, 
		      [self(), Mod, Line] ++ Args);
	Pid ->
	    io:format(Pid, "~p~p(~p): " ++ Format, 
		      [self(), Mod, Line] ++ Args)
    end.

skip(Actual, File, Line) ->
    log("Skipping test case~n", [], File, Line),
    String = lists:flatten(io_lib:format("Skipping test case ~p(~p): ~p~n",
					 [File, Line, Actual])),
    exit({skipped, String}).

fatal_skip(Actual, File, Line) ->
    error(Actual, File, Line),
    exit(shutdown).


fail(Actual, File, Line) ->
    log("Test case failing~n", [], File, Line),
    String = lists:flatten(io_lib:format("Test case failing ~p (~p): ~p~n",
					 [File, Line, Actual])),
    exit({suite_failed, String}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Flush the message queue and return its messages

flush() ->
    receive
	Msg ->
	    [Msg | flush()]
    after 1000 ->
	    []
    end.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The proxy process

proxy_start(ProxyId) ->
    spawn_link(?MODULE, proxy_init, [ProxyId, self()]).

proxy_start(Node, ProxyId) ->
    spawn_link(Node, ?MODULE, proxy_init, [ProxyId, self()]).

proxy_init(ProxyId, Controller) ->
    process_flag(trap_exit, true),
    ?LOG("[~p] proxy started by ~p~n",[ProxyId, Controller]),
    proxy_loop(ProxyId, Controller).

proxy_loop(OwnId, Controller) ->
    receive
	{'EXIT', Controller, Reason} ->
	    p("proxy_loop -> received exit from controller"
	      "~n   Reason: ~p"
	      "~n", [Reason]),
	    exit(Reason);
	{apply, Fun} ->
	    p("proxy_loop -> received apply request~n", []),
	    Res = Fun(),
	    p("proxy_loop -> apply result: "
	      "~n   ~p"
	      "~n", [Res]),
	    Controller ! {res, OwnId, Res},
	    proxy_loop(OwnId, Controller);
	OtherMsg ->
	    p("proxy_loop -> received unknown message: "
	      "~n  OtherMsg: ~p"
	      "~n", [OtherMsg]),
	    Controller ! {msg, OwnId, OtherMsg},
	    proxy_loop(OwnId, Controller)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check if process is alive and kicking
still_alive(Pid) ->   
    case catch erlang:is_process_alive(Pid) of % New BIF in Erlang/OTP R5
	true -> 
	    true;
	false -> 
	    false;
	{'EXIT', _} -> % Pre R5 backward compatibility 
	    case process_info(Pid, message_queue_len) of
		undefined -> false;
		_ -> true
	    end 
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    
%% Returns [Name, Host]    
node_to_name_and_host(Node) ->
    string:tokens(atom_to_list(Node), [$@]).

start_nodes([Node | Nodes], File, Line) ->
    case net_adm:ping(Node) of
	pong ->
	    start_nodes(Nodes, File, Line);
	pang ->
	    [Name, Host] = node_to_name_and_host(Node),
	    case slave:start_link(Host, Name) of
		{ok, NewNode} when NewNode =:= Node ->
		    Path = code:get_path(),
		    {ok, Cwd} = file:get_cwd(),
		    true = rpc:call(Node, code, set_path, [Path]),
		    ok = rpc:call(Node, file, set_cwd, [Cwd]),
		    true = rpc:call(Node, code, set_path, [Path]),
		    {_, []} = rpc:multicall(global, sync, []),
		    start_nodes(Nodes, File, Line);
		Other ->
		    fatal_skip({cannot_start_node, Node, Other}, File, Line)
	    end
    end;
start_nodes([], _File, _Line) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_alloc_info() ->
    io:format("Allocator memory information:~n", []),
    AllocInfo = alloc_info(),
    display_alloc_info(AllocInfo).

display_alloc_info([]) ->
    ok;
display_alloc_info([{Alloc, Mem}|AllocInfo]) ->
    io:format("  ~15w: ~10w~n", [Alloc, Mem]),
    display_alloc_info(AllocInfo).
    
alloc_info() ->
    case erlang:system_info(allocator) of
	{_Allocator, _Version, Features, _Settings} ->
	    alloc_info(Features);
	_ ->
	    []
    end.

alloc_info(Allocators) ->
    Allocs = [temp_alloc, sl_alloc, std_alloc, ll_alloc, eheap_alloc, 
	      ets_alloc, binary_alloc, driver_alloc], 
    alloc_info(Allocators, Allocs, []).

alloc_info([], _, Acc) ->
    lists:reverse(Acc);
alloc_info([Allocator | Allocators], Allocs, Acc) ->
    case lists:member(Allocator, Allocs) of
	true ->
	    Instances0 = erlang:system_info({allocator, Allocator}),
	    Instances = 
		if 
		    is_list(Instances0) ->
			[Instance || Instance <- Instances0, 
				     element(1, Instance) =:= instance];
		    true ->
			[]
		end,
	    AllocatorMem = alloc_mem_info(Instances),
	    alloc_info(Allocators, Allocs, [{Allocator, AllocatorMem} | Acc]);

	false ->
	    alloc_info(Allocators, Allocs, Acc)
    end.
    
alloc_mem_info(Instances) ->
    alloc_mem_info(Instances, []).

alloc_mem_info([], Acc) ->
    lists:sum([Mem || {instance, _, Mem} <- Acc]);
alloc_mem_info([{instance, N, Info}|Instances], Acc) ->
    InstanceMemInfo = alloc_instance_mem_info(Info),
    alloc_mem_info(Instances, [{instance, N, InstanceMemInfo} | Acc]).

alloc_instance_mem_info(InstanceInfo) ->
    MBCS = alloc_instance_mem_info(mbcs, InstanceInfo),
    SBCS = alloc_instance_mem_info(sbcs, InstanceInfo),
    MBCS + SBCS.

alloc_instance_mem_info(Key, InstanceInfo) ->
    case lists:keysearch(Key, 1, InstanceInfo) of
	{value, {Key, Info}} ->
	    case lists:keysearch(blocks_size, 1, Info) of
		{value, {blocks_size, Mem, _, _}} ->
		    Mem;
		_ ->
		    0
	    end;
	_ ->
	    0
    end.


display_system_info(WhenStr) ->
    display_system_info(WhenStr, undefined, undefined).

display_system_info(WhenStr, undefined, undefined) ->
    display_system_info(WhenStr, "");
display_system_info(WhenStr, Mod, Func) ->
    ModFuncStr = lists:flatten(io_lib:format(" ~w:~w", [Mod, Func])),
    display_system_info(WhenStr, ModFuncStr).

display_system_info(WhenStr, ModFuncStr) ->
    Fun = fun(F) -> case (catch F()) of
			{'EXIT', _} ->
			    undefined;
			Res ->
			    Res
		    end
	  end,
    ProcCount    = Fun(fun() -> erlang:system_info(process_count) end),
    ProcLimit    = Fun(fun() -> erlang:system_info(process_limit) end),
    ProcMemAlloc = Fun(fun() -> erlang:memory(processes) end),
    ProcMemUsed  = Fun(fun() -> erlang:memory(processes_used) end),
    ProcMemBin   = Fun(fun() -> erlang:memory(binary) end),
    ProcMemTot   = Fun(fun() -> erlang:memory(total) end),
    %% error_logger:info_msg(
    io:format("~n"
	      "~n*********************************************"
	      "~n"
	      "System info ~s~s => "
	      "~n   Process count:        ~w"
              "~n   Process limit:        ~w"
              "~n   Process memory alloc: ~w"
              "~n   Process memory used:  ~w"
              "~n   Memory for binaries:  ~w"
              "~n   Memory total:         ~w"
	      "~n"
	      "~n*********************************************"
	      "~n"
	      "~n", [WhenStr, ModFuncStr, 
		     ProcCount, ProcLimit, ProcMemAlloc, ProcMemUsed, 
		     ProcMemBin, ProcMemTot]),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_test_case(Actions, N, Config, File, Line) ->
    OrigNodes = lookup_config(nodes, Config),
    TestNodes = lookup_config(nodenames, Config), %% For testserver
    This = node(),
    SomeNodes = OrigNodes ++ (TestNodes -- OrigNodes),
    AllNodes = [This | (SomeNodes -- [This])],
    Nodes = pick_n_nodes(N, AllNodes, File, Line),
    start_nodes(Nodes, File, Line),
    do_prepare_test_case(Actions, Nodes, Config, File, Line).

do_prepare_test_case([init | Actions], Nodes, Config, File, Line) ->
    process_flag(trap_exit, true),
    megaco_test_lib:flush(),
    do_prepare_test_case(Actions, Nodes, Config, File, Line);
do_prepare_test_case([{stop_app, App} | Actions], Nodes, Config, File, Line) ->
    _Res = rpc:multicall(Nodes, application, stop, [App]),
    do_prepare_test_case(Actions, Nodes, Config, File, Line);
do_prepare_test_case([], Nodes, _Config, _File, _Line) ->
    Nodes.

pick_n_nodes(all, AllNodes, _File, _Line) ->
    AllNodes;
pick_n_nodes(N, AllNodes, _File, _Line)
  when is_integer(N) andalso (length(AllNodes) >= N) ->
    AllNodes -- lists:nthtail(N, AllNodes);
pick_n_nodes(N, AllNodes, File, Line) ->
    fatal_skip({too_few_nodes, N, AllNodes}, File, Line).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_config(Key, Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value, {Key, Val}} ->
	    Val;
	_ ->
	    []
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report_event(_Severity, _Label, _Content) ->
    %% diameter:report_event(Severity, Label, Content).
    hopefully_traced.


p(F,A) ->
    io:format("~p" ++ F ++ "~n", [self()|A]).
