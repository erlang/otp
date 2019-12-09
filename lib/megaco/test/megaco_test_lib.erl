%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2019. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Lightweight test server
%%----------------------------------------------------------------------
%%

-module(megaco_test_lib).

%% -compile(export_all).

-export([
         log/4,
         error/3,

         sleep/1,
         hours/1, minutes/1, seconds/1,
         formated_timestamp/0, format_timestamp/1,

         skip/3,
         non_pc_tc_maybe_skip/4,
         os_based_skip/1,

         flush/0,
         still_alive/1,

         display_alloc_info/0,
         display_system_info/1, display_system_info/2, display_system_info/3,

         prepare_test_case/5,

         proxy_start/1, proxy_start/2,

         mk_nodes/1,
         start_nodes/3,
         start_node/3

        ]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([proxy_init/2]).

-include("megaco_test_lib.hrl").

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


formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp(TS) ->
    megaco:format_timestamp(TS).


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


%% The type and spec'ing is just to increase readability
-type os_family()  :: win32 | unix.
-type os_name()    :: atom().
-type os_version() :: string() | {non_neg_integer(),
                                  non_neg_integer(),
                                  non_neg_integer()}.
-type os_skip_check() :: fun(() -> boolean()) | 
                            fun((os_version()) -> boolean()).
-type skippable() :: any | [os_family() | 
                            {os_family(), os_name() |
                                          [os_name() | {os_name(), 
                                                        os_skip_check()}]}].

-spec os_based_skip(skippable()) -> boolean().

os_based_skip(any) ->
    true;
os_based_skip(Skippable) when is_list(Skippable) ->
    os_base_skip(Skippable, os:type());
os_based_skip(_Crap) ->
    false.

os_base_skip(Skippable, {OsFam, OsName}) ->
    os_base_skip(Skippable, OsFam, OsName);
os_base_skip(Skippable, OsFam) ->
    os_base_skip(Skippable, OsFam, undefined).

os_base_skip(Skippable, OsFam, OsName) -> 
    %% Check if the entire family is to be skipped
    %% Example: [win32, unix]
    case lists:member(OsFam, Skippable) of
        true ->
            true;
        false ->
            %% Example: [{unix, freebsd}] | [{unix, [freebsd, darwin]}]
            case lists:keysearch(OsFam, 1, Skippable) of
                {value, {OsFam, OsName}} ->
                    true;
                {value, {OsFam, OsNames}} when is_list(OsNames) ->
                    %% OsNames is a list of: 
                    %%    [atom()|{atom(), function/0 | function/1}]
                    case lists:member(OsName, OsNames) of
                        true ->
                            true;
                        false ->
                            os_based_skip_check(OsName, OsNames)
                    end;
                _ ->
                    false
            end
    end.



%% Performs a check via a provided fun with arity 0 or 1.
%% The argument is the result of os:version().
os_based_skip_check(OsName, OsNames) ->
    case lists:keysearch(OsName, 1, OsNames) of
        {value, {OsName, Check}} when is_function(Check, 0) ->
            Check();
        {value, {OsName, Check}} when is_function(Check, 1) ->
            Check(os:version());
        _ ->
            false
    end.

    
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluates a test case or test suite
%% Returns a list of failing test cases:
%% 
%%     {Mod, Fun, ExpectedRes, ActualRes}
%%----------------------------------------------------------------------

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


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verify that the actual result of a test case matches the exected one
%% Returns the actual result
%% Stores the result in the process dictionary if mismatch

error(Actual, Mod, Line) ->
    global:send(megaco_global_logger, {failed, Mod, Line}),
    log("<ERROR> Bad result: ~p~n", [Actual], Mod, Line),
    Label = lists:concat([Mod, "(", Line, ") unexpected result"]),
    megaco:report_event(60, Mod, Mod, Label,
			[{line, Mod, Line}, {error, Actual}]),
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

skip(Reason) ->
    exit({skip, Reason}).

skip(Actual, File, Line) ->
    log("Skipping test case: ~p~n", [Actual], File, Line),
    String = f("~p(~p): ~p~n", [File, Line, Actual]),
    skip(String).

fatal_skip(Actual, File, Line) ->
    error(Actual, File, Line),
    skip({fatal, Actual, File, Line}).



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
%% Check if process is alive and kicking

still_alive(Pid) ->   
    erlang:is_process_alive(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The proxy process

proxy_start(ProxyId) ->
    spawn_link(?MODULE, proxy_init, [ProxyId, self()]).

proxy_start(Node, ProxyId) ->
    spawn_link(Node, ?MODULE, proxy_init, [ProxyId, self()]).

proxy_init(ProxyId, Controller) ->
    process_flag(trap_exit, true),
    IdStr = proxyid2string(ProxyId),
    put(id, IdStr),
    ?LOG("[~s] proxy started by ~p~n", [IdStr, Controller]),
    proxy_loop(ProxyId, Controller).

proxy_loop(OwnId, Controller) ->
    receive
	{'EXIT', Controller, Reason} ->
	    pprint("proxy_loop -> received exit from controller"
                   "~n   Reason: ~p", [Reason]),
	    exit(Reason);
	{stop, Controller, Reason} ->
	    p("proxy_loop -> received stop from controller"
	      "~n   Reason: ~p"
	      "~n", [Reason]),
	    exit(Reason);
	
	{apply, Fun} ->
            pprint("proxy_loop -> received apply request"),
	    Res = Fun(),
            pprint("proxy_loop -> apply result: "
                   "~n   ~p", [Res]),
	    Controller ! {res, OwnId, Res},
	    proxy_loop(OwnId, Controller);
	OtherMsg ->
	    pprint("proxy_loop -> received unknown message: "
                   "~n  ~p", [OtherMsg]),
	    Controller ! {msg, OwnId, OtherMsg},
	    proxy_loop(OwnId, Controller)
    end.

proxyid2string(Id) when is_list(Id) ->
    Id;
proxyid2string(Id) when is_atom(Id) ->
    atom_to_list(Id);
proxyid2string(Id) ->
    f("~p", [Id]).

pprint(F) ->
    pprint(F, []).

pprint(F, A) ->
    io:format("[~s] ~p ~s " ++ F ++ "~n",
              [get(id), self(), formated_timestamp() | A]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server callbacks

init_per_suite(Config) ->

    %% We have some crap machines that causes random test case failures
    %% for no obvious reason. So, attempt to identify those without actually
    %% checking for the host name...
    %% We have two "machines" we are checking for. Both are old installations
    %% running on really slow VMs (the host machines are old and tired).
    LinuxVersionVerify =
        fun(V) when (V > {3,6,11}) ->
                false; % OK - No skip
           (V) when (V =:= {3,6,11}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "Fedora release 16 " ++ _ -> % Stone age Fedora => Skip
                        true;
                    _ ->
                        false
                end;
           (V) when (V > {2,6,24}) ->
                false; % OK - No skip
           (_) ->
                %% We are specifically checking for
                %% a *really* old gento...
                case string:find(string:strip(os:cmd("uname -a")), "gentoo") of
                    nomatch ->
                        false;
                    _ -> % Stone age gentoo => Skip
                        true
                end
        end,
    DarwinVersionVerify =
        fun(V) when (V > {9, 8, 0}) ->
                %% This version is OK: No Skip
                false;
           (_V) ->
                %% This version is *not* ok: Skip
                true
        end,
    COND = [{unix, [{linux, LinuxVersionVerify}, {darwin, DarwinVersionVerify}]}],
    case os_based_skip(COND) of
        true ->
            {skip, "Unstable host and/or os (or combo thererof)"};
        false ->
            maybe_start_global_sys_monitor(Config),
            Config
    end.

%% We start the global system monitor unless explicitly disabled
maybe_start_global_sys_monitor(Config) ->
    case lists:keysearch(sysmon, 1, Config) of
        {value, {sysmon, false}} ->
            ok;
        _ ->
            megaco_test_global_sys_monitor:start()
    end.

end_per_suite(Config) when is_list(Config) ->

    case lists:keysearch(sysmon, 1, Config) of
        {value, {sysmon, false}} ->
            ok;
        _ ->
            megaco_test_global_sys_monitor:stop()
    end,

    Config.


init_per_testcase(_Case, Config) ->
    Pid = group_leader(),
    Name = megaco_global_logger,
    case global:whereis_name(Name) of
	undefined ->
	    global:register_name(megaco_global_logger, Pid);
	Pid ->
	    io:format("~w:init_per_testcase -> "
		      "already registered to ~p~n", [?MODULE, Pid]),
	    ok;
	OtherPid when is_pid(OtherPid) ->
	    io:format("~w:init_per_testcase -> "
		      "already registered to other ~p (~p)~n", 
		      [?MODULE, OtherPid, Pid]),
	    exit({already_registered, {megaco_global_logger, OtherPid, Pid}})
    end,
    set_kill_timer(Config).

end_per_testcase(_Case, Config) ->
    Name = megaco_global_logger,
    case global:whereis_name(Name) of
	undefined ->
	    io:format("~w:end_per_testcase -> already un-registered~n", 
		      [?MODULE]),
	    ok;
	Pid when is_pid(Pid) ->
	    global:unregister_name(megaco_global_logger),
	    ok
    end,
    reset_kill_timer(Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set kill timer

set_kill_timer(Config) ->
    case init:get_argument(megaco_test_timeout) of
	{ok, _} -> 
	    Config;
	_ ->
	    Time = 
		case lookup_config(tc_timeout, Config) of
		    [] ->
			timer:minutes(5);
		    ConfigTime when is_integer(ConfigTime) ->
			ConfigTime
		end,
	    Dog = test_server:timetrap(Time),
	    [{kill_timer, Dog}|Config]
		    
	    
    end.

reset_kill_timer(Config) ->
    case lists:keysearch(kill_timer, 1, Config) of
	{value, {kill_timer, Dog}} ->
	    test_server:timetrap_cancel(Dog), 
	    lists:keydelete(kill_timer, 1, Config);
	_ ->
	    Config
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_test_case(Actions, N, Config, File, Line) ->
    OrigNodes = lookup_config(nodes, Config),
    TestNodes = lookup_config(nodenames, Config), %% For testserver
    This      = node(),
    SomeNodes = OrigNodes ++ (TestNodes -- OrigNodes),
    AllNodes  = [This | (SomeNodes -- [This])],
    Nodes     = pick_n_nodes(N, AllNodes, File, Line),
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
   
lookup_config(Key,Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value,{Key,Val}} ->
	    Val;
	_ ->
	    []
    end.

mk_nodes(N) when (N > 0) ->
    mk_nodes(N, []).

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


start_nodes(Nodes, File, Line) when is_list(Nodes) ->
    lists:foreach(fun(N) -> start_node(N, File, Line) end, Nodes).

start_node(Node, File, Line) ->
    case net_adm:ping(Node) of
	pong ->
            p("node ~p already running", [Node]),
	    ok;
	pang ->
	    [Name, Host] = node_to_name_and_host(Node),
            Pa = filename:dirname(code:which(?MODULE)),
            Args = " -pa " ++ Pa ++
                " -s " ++ atom_to_list(megaco_test_sys_monitor) ++ " start" ++ 
                " -s global sync",
            p("try start node ~p", [Node]),
	    case slave:start_link(Host, Name, Args) of
		{ok, NewNode} when NewNode =:= Node ->
                    p("node ~p started - now set path, cwd and sync", [Node]),
		    Path = code:get_path(),
		    {ok, Cwd} = file:get_cwd(),
		    true = rpc:call(Node, code, set_path, [Path]),
		    ok = rpc:call(Node, file, set_cwd, [Cwd]),
		    true = rpc:call(Node, code, set_path, [Path]),
		    {_, []} = rpc:multicall(global, sync, []),
		    ok;
		Other ->
                    p("failed starting node ~p: ~p", [Node, Other]),
		    fatal_skip({cannot_start_node, Node, Other}, File, Line)
	    end
    end.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

p(F, A) ->
    io:format("~s ~p " ++ F ++ "~n", [?FTS(), self() | A]).
