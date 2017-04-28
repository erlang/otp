%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(erl_distribution_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([tick/1, tick_change/1,
         nodenames/1, hostnames/1,
         illegal_nodenames/1, hidden_node/1,
	 setopts/1,
	 table_waste/1, net_setuptime/1,
	 inet_dist_options_options/1,

	 monitor_nodes_nodedown_reason/1,
	 monitor_nodes_complex_nodedown_reason/1,
	 monitor_nodes_node_type/1,
	 monitor_nodes_misc/1,
	 monitor_nodes_otp_6481/1,
	 monitor_nodes_errors/1,
	 monitor_nodes_combinations/1,
	 monitor_nodes_cleanup/1,
	 monitor_nodes_many/1]).

%% Performs the test at another node.
-export([get_socket_priorities/0,
	 tick_cli_test/1, tick_cli_test1/1,
	 tick_serv_test/2, tick_serv_test1/1,
	 run_remote_test/1,
	 setopts_do/2,
	 keep_conn/1, time_ping/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([start_node/2]).

-export([pinger/1]).

-define(DUMMY_NODE,dummy@test01).

%%-----------------------------------------------------------------
%% The distribution is mainly tested in the big old test_suite.
%% This test only tests the net_ticktime configuration flag.
%% Should be started in a CC view with:
%% erl -sname master -rsh ctrsh
%%-----------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,4}}].

all() -> 
    [tick, tick_change, nodenames, hostnames, illegal_nodenames,
     hidden_node, setopts,
     table_waste, net_setuptime, inet_dist_options_options,
     {group, monitor_nodes}].

groups() -> 
    [{monitor_nodes, [],
      [monitor_nodes_nodedown_reason,
       monitor_nodes_complex_nodedown_reason,
       monitor_nodes_node_type, monitor_nodes_misc,
       monitor_nodes_otp_6481, monitor_nodes_errors,
       monitor_nodes_combinations, monitor_nodes_cleanup,
       monitor_nodes_many]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

tick(Config) when is_list(Config) ->
    PaDir = filename:dirname(code:which(erl_distribution_SUITE)),

    %% First check that the normal case is OK!
    {ok, Node} = start_node(dist_test, "-pa " ++ PaDir),
    rpc:call(Node, erl_distribution_SUITE, tick_cli_test, [node()]),

    erlang:monitor_node(Node, true),
    receive
	{nodedown, Node} ->
	    ct:fail("nodedown from other node")
    after 30000 ->
	    erlang:monitor_node(Node, false),
	    stop_node(Node)
    end,

    %% Now, set the net_ticktime for the other node to 12 secs.
    %% After the sleep(2sec) and cast the other node shall destroy
    %% the connection as it has not received anything on the connection.
    %% The nodedown message should arrive within 8 < T < 16 secs.

    %% We must have two slave nodes as the slave mechanism otherwise
    %% halts the client node after tick timeout (the connection is down
    %% and the slave node decides to halt !!

    %% Set the ticktime on the server node to 100 secs so the server
    %% node doesn't tick the client node within the interval ...

    {ok, ServNode} = start_node(dist_test_server,
				"-kernel net_ticktime 100 "
				"-pa " ++ PaDir),
    rpc:call(ServNode, erl_distribution_SUITE, tick_serv_test, [Node, node()]),

    {ok, _} = start_node(dist_test,
			 "-kernel net_ticktime 12 "
			 "-pa " ++ PaDir),
    rpc:call(Node, erl_distribution_SUITE, tick_cli_test, [ServNode]),

    spawn_link(erl_distribution_SUITE, keep_conn, [Node]),

    {tick_serv, ServNode} ! {i_want_the_result, self()},

    monitor_node(ServNode, true),
    monitor_node(Node, true),

    receive
	{tick_test, T} when is_integer(T) ->
	    stop_node(ServNode),
	    stop_node(Node),
	    T;
	{tick_test, Error} ->
	    stop_node(ServNode),
	    stop_node(Node),
	    ct:fail(Error);
	{nodedown, Node} ->
	    stop_node(ServNode),
	    ct:fail("client node died");
	{nodedown, ServNode} ->
	    stop_node(Node),
	    ct:fail("server node died")
    end,
    ok.

%% Checks that pinging nonexistyent nodes does not waste space in distribution table.
table_waste(Config) when is_list(Config) ->
    {ok, HName} = inet:gethostname(),
    F = fun(0,_F) -> [];
	   (N,F) -> 
		Name = list_to_atom("erl_distribution_"++integer_to_list(N)++
					"@"++HName),
		pang = net_adm:ping(Name),
		F(N-1,F)
	end,
    F(256,F),
    {ok, N} = start_node(erl_distribution_300,""),
    stop_node(N),
    ok.

%% Test that starting nodes with different legal name part works, and that illegal
%% ones are filtered
nodenames(Config) when is_list(Config) ->
    legal("a1@b"),
    legal("a-1@b"),
    legal("a_1@b"),

    illegal("cdé@a"),
    illegal("te欢st@a").

%% Test that starting nodes with different legal host part works, and that illegal
%% ones are filtered
hostnames(Config) when is_list(Config) ->
    Host = gethostname(),
    legal([$a,$@|atom_to_list(Host)]),
    legal("1@b1"),
    legal("b@b1-c"),
    legal("c@b1_c"),
    legal("d@b1#c"),
    legal("f@::1"),
    legal("g@1:bc3:4e3f:f20:0:1"),

    case file:native_name_encoding() of
        latin1 -> ignore;
        _ -> legal("e@b1é")
    end,
    long_hostnames(net_kernel:longnames()),

    illegal("h@testالع"),
    illegal("i@языtest"),
    illegal("j@te欢st").

long_hostnames(true) ->
    legal("k@b.b.c"),
    legal("l@b.b-c.d"),
    legal("m@b.b_c.d"),
    legal("n@127.0.0.1"),
    legal("o@207.123.456.789");
long_hostnames(false) ->
    illegal("k@b.b.c").

legal(Name) ->
    case test_node(Name) of
        started ->
            ok;
        not_started ->
            ct:fail("no ~p node started", [Name])
    end.

illegal(Name) ->
    case test_node(Name) of
        not_started ->
            ok;
        started ->
            ct:fail("~p node started with illegal name", [Name])
    end.

test_node(Name) ->
    ProgName = atom_to_list(lib:progname()),
    Command = ProgName ++ " -noinput " ++ long_or_short() ++ Name ++
               " -eval \"net_adm:ping('" ++ atom_to_list(node()) ++ "')\"",
    net_kernel:monitor_nodes(true),
    BinCommand = unicode:characters_to_binary(Command, utf8),
    open_port({spawn, BinCommand}, [stream]),
    Node = list_to_atom(Name),
    receive
        {nodeup, Node} ->
            net_kernel:monitor_nodes(false),
            slave:stop(Node),
            started
    after 5000 ->
        net_kernel:monitor_nodes(false),
        not_started
    end.

long_or_short() ->
    case net_kernel:longnames() of
        true -> " -name ";
        false -> " -sname "
    end.

% get the localhost's name, depending on the using name policy
gethostname() ->
    Hostname = case net_kernel:longnames() of
       true->
           net_adm:localhost();
       _->
           {ok, Name}=inet:gethostname(),
           Name
    end,
    list_to_atom(Hostname).

%% Test that pinging an illegal nodename does not kill the node.
illegal_nodenames(Config) when is_list(Config) ->
    PaDir = filename:dirname(code:which(erl_distribution_SUITE)),
    {ok, Node}=start_node(illegal_nodenames, "-pa " ++ PaDir),
    monitor_node(Node, true),
    RPid=rpc:call(Node, erlang, spawn,
		  [?MODULE, pinger, [self()]]),
    receive
	{RPid, pinged} ->
	    ok;
	{nodedown, Node} ->
	    ct:fail("Remote node died.")
    end,
    stop_node(Node),
    ok.

pinger(Starter) ->
    io:format("Starter:~p~n",[Starter]),
    net_adm:ping(a@b@c),
    Starter ! {self(), pinged},
    ok.


%% Test that you can set the net_setuptime properly.
net_setuptime(Config) when is_list(Config) ->
    %% In this test case, we reluctantly accept shorter times than the given
    %% setup time, because the connection attempt can end in a
    %% "Host unreachable" error before the timeout fires.

    Res0 = do_test_setuptime("2"),
    io:format("Res0 = ~p", [Res0]),
    true = (Res0 =< 4000),
    Res1 = do_test_setuptime("0.3"),
    io:format("Res1 = ~p", [Res1]),
    true = (Res1 =< 500),
    ok.

do_test_setuptime(Setuptime) when is_list(Setuptime) ->
    PaDir = filename:dirname(code:which(?MODULE)),
    {ok, Node} = start_node(dist_setuptime_test, "-pa " ++ PaDir ++
				" -kernel net_setuptime " ++ Setuptime),
    Res = rpc:call(Node,?MODULE,time_ping,[?DUMMY_NODE]),
    stop_node(Node),
    Res.

time_ping(Node) ->
    T0 = erlang:monotonic_time(),
    pang = net_adm:ping(Node),
    T1 = erlang:monotonic_time(),
    erlang:convert_time_unit(T1 - T0, native, millisecond).

%% Keep the connection with the client node up.
%% This is necessary as the client node runs with much shorter
%% tick time !!
keep_conn(Node) ->
    sleep(1),
    rpc:cast(Node, erlang, time, []),
    keep_conn(Node).

tick_serv_test(Node, MasterNode) ->
    spawn(erl_distribution_SUITE, keep_conn, [MasterNode]),
    spawn(erl_distribution_SUITE, tick_serv_test1, [Node]).

tick_serv_test1(Node) ->
    register(tick_serv, self()),
    TestServer = receive {i_want_the_result, TS} -> TS end,
    monitor_node(Node, true),
    receive
	{nodedown, Node} ->
	    net_adm:ping(Node), %% Set up the connection again !!

	    {tick_test, Node} ! {whats_the_result, self()},
	    receive
		{tick_test, Res} ->
		    TestServer ! {tick_test, Res}
	    end
    end.

tick_cli_test(Node) ->
    spawn(erl_distribution_SUITE, tick_cli_test1, [Node]).

tick_cli_test1(Node) ->
    register(tick_test, self()),
    erlang:monitor_node(Node, true),
    sleep(2),
    rpc:call(Node, erlang, time, []), %% simulate action on the connection
    T1 = erlang:monotonic_time(),
    receive
	{nodedown, Node} ->
	    T2 = erlang:monotonic_time(),
	    receive
		{whats_the_result, From} ->
		    Diff = erlang:convert_time_unit(T2-T1, native,
						    millisecond),
		    case Diff of
			T when T > 8000, T < 16000 ->
			    From ! {tick_test, T};
			T ->
			    From ! {tick_test,
				    {"T not in interval 8000 < T < 16000",
				     T}}
		    end
	    end
    end.

setopts(Config) when is_list(Config) ->
    register(setopts_regname, self()),
    [N1,N2,N3,N4] = get_nodenames(4, setopts),

    {_N1F,Port1} = start_node_unconnected(N1, ?MODULE, run_remote_test,
					["setopts_do", atom_to_list(node()), "1", "ping"]),
    0 = wait_for_port_exit(Port1),

    {_N2F,Port2} = start_node_unconnected(N2, ?MODULE, run_remote_test,
				 ["setopts_do", atom_to_list(node()), "2", "ping"]),
    0 = wait_for_port_exit(Port2),

    {ok, LSock} = gen_tcp:listen(0, [{packet,2}, {active,false}]),
    {ok, LTcpPort} = inet:port(LSock),

    {N3F,Port3} = start_node_unconnected(N3, ?MODULE, run_remote_test,
					["setopts_do", atom_to_list(node()),
					 "1", integer_to_list(LTcpPort)]),
    wait_and_connect(LSock, N3F, Port3),
    0 = wait_for_port_exit(Port3),

    {N4F,Port4} = start_node_unconnected(N4, ?MODULE, run_remote_test,
					["setopts_do", atom_to_list(node()),
					 "2", integer_to_list(LTcpPort)]),
    wait_and_connect(LSock, N4F, Port4),
    0 = wait_for_port_exit(Port4),

    ok.

wait_and_connect(LSock, NodeName, NodePort) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, "Connect please"} = gen_tcp:recv(Sock, 0),
    flush_from_port(NodePort),
    pong = net_adm:ping(NodeName),
    gen_tcp:send(Sock, "Connect done"),
    gen_tcp:close(Sock).


flush_from_port(Port) ->
    flush_from_port(Port, 10).

flush_from_port(Port, Timeout) ->
    receive
	{Port,{data,String}} ->
	    io:format("~p: ~s\n", [Port, String]),
	    flush_from_port(Port, Timeout)
    after Timeout ->
	    timeout
    end.

wait_for_port_exit(Port) ->
    case (receive M -> M end) of
	{Port,{exit_status,Status}} ->
	    Status;
	{Port,{data,String}} ->
	    io:format("~p: ~s\n", [Port, String]),
	    wait_for_port_exit(Port)
    end.

run_remote_test([FuncStr, TestNodeStr | Args]) ->
    Status = try
	io:format("Node ~p started~n", [node()]),
	TestNode = list_to_atom(TestNodeStr),
	io:format("Node ~p spawning function ~p~n", [node(), FuncStr]),
	{Pid,Ref} = spawn_monitor(?MODULE, list_to_atom(FuncStr), [TestNode, Args]),
	io:format("Node ~p waiting for function ~p~n", [node(), FuncStr]),
	receive
	    {'DOWN', Ref, process, Pid, normal} ->
		0;
	    Other ->
		io:format("Node ~p got unexpected msg: ~p\n",[node(), Other]),
		1
	end
    catch
	C:E ->
	    io:format("Node ~p got EXCEPTION ~p:~p\nat ~p\n",
		      [node(), C, E, erlang:get_stacktrace()]),
	    2
    end,
    io:format("Node ~p doing halt(~p).\n",[node(), Status]),
    erlang:halt(Status).

% Do the actual test on the remote node
setopts_do(TestNode, [OptNr, ConnectData]) ->
    [] = nodes(),
    {Opt, Val} = opt_from_nr(OptNr),
    ok = net_kernel:setopts(new, [{Opt, Val}]),

    [] = nodes(),
    {error, noconnection} = net_kernel:getopts(TestNode, [Opt]),

    case ConnectData of
	"ping" ->  % We connect
	    net_adm:ping(TestNode);
	TcpPort -> % Other connect
	    {ok, Sock} = gen_tcp:connect("localhost", list_to_integer(TcpPort),
					 [{active,false},{packet,2}]),
	    ok = gen_tcp:send(Sock, "Connect please"),
	    {ok, "Connect done"} = gen_tcp:recv(Sock, 0),
	    gen_tcp:close(Sock)
    end,
    [TestNode] = nodes(),
    {ok, [{Opt,Val}]} = net_kernel:getopts(TestNode, [Opt]),
    {error, noconnection} = net_kernel:getopts('pixie@fairyland', [Opt]),

    NewVal = change_val(Val),
    ok = net_kernel:setopts(TestNode, [{Opt, NewVal}]),
    {ok, [{Opt,NewVal}]} = net_kernel:getopts(TestNode, [Opt]),

    ok = net_kernel:setopts(TestNode, [{Opt, Val}]),
    {ok, [{Opt,Val}]} = net_kernel:getopts(TestNode, [Opt]),

    ok.

opt_from_nr("1") -> {nodelay, true};
opt_from_nr("2") -> {nodelay, false}.

change_val(true)  -> false;
change_val(false) -> true.

start_node_unconnected(Name, Mod, Func, Args) ->
    FullName = full_node_name(Name),
    CmdLine = mk_node_cmdline(Name,Mod,Func,Args),
    io:format("Starting node ~p: ~s~n", [FullName, CmdLine]),
    case open_port({spawn, CmdLine}, [exit_status]) of
	Port when is_port(Port) ->
	    {FullName, Port};
	Error ->
	    exit({failed_to_start_node, FullName, Error})
    end.

full_node_name(PreName) ->
    HostSuffix = lists:dropwhile(fun ($@) -> false; (_) -> true end,
				 atom_to_list(node())),
    list_to_atom(atom_to_list(PreName) ++ HostSuffix).

mk_node_cmdline(Name,Mod,Func,Args) ->
    Static = "-noinput",
    Pa = filename:dirname(code:which(?MODULE)),
    Prog = case catch init:get_argument(progname) of
	       {ok,[[P]]} -> P;
	       _ -> exit(no_progname_argument_found)
	   end,
    NameSw = case net_kernel:longnames() of
		 false -> "-sname ";
		 true -> "-name ";
		 _ -> exit(not_distributed_node)
	     end,
    {ok, Pwd} = file:get_cwd(),
    NameStr = atom_to_list(Name),
    Prog ++ " "
	++ Static ++ " "
	++ NameSw ++ " " ++ NameStr
	++ " -pa " ++ Pa
	++ " -env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ NameStr
	++ " -setcookie " ++ atom_to_list(erlang:get_cookie())
	++ " -run " ++ atom_to_list(Mod) ++ " " ++ atom_to_list(Func)
	++ " " ++ string:join(Args, " ").


%% OTP-4255.
tick_change(Config) when is_list(Config) ->
    PaDir = filename:dirname(code:which(?MODULE)),
    [BN, CN] = get_nodenames(2, tick_change),
    DefaultTT = net_kernel:get_net_ticktime(),
    unchanged = net_kernel:set_net_ticktime(DefaultTT, 60),
    case DefaultTT of
	I when is_integer(I) -> ok;
	_                 -> ct:fail(DefaultTT)
    end,

    %% In case other nodes are connected
    case nodes(connected) of
	[] -> net_kernel:set_net_ticktime(10, 0);
	_ ->  rpc:multicall(nodes([this, connected]), net_kernel,
			    set_net_ticktime, [10, 5])
    end,

    wait_until(fun () -> 10 == net_kernel:get_net_ticktime() end),
    {ok, B} = start_node(BN, "-kernel net_ticktime 10 -pa " ++ PaDir),
    {ok, C} = start_node(CN, "-kernel net_ticktime 10 -hidden -pa "
			 ++ PaDir),

    OTE = process_flag(trap_exit, true),
    case catch begin
		   run_tick_change_test(B, C, 10, 1, PaDir),
		   run_tick_change_test(B, C, 1, 10, PaDir)
	       end of
	{'EXIT', Reason} ->
	    stop_node(B),
	    stop_node(C),
	    %% In case other nodes are connected
	    case nodes(connected) of
		[] -> net_kernel:set_net_ticktime(DefaultTT, 0);
		_ ->  rpc:multicall(nodes([this, connected]), net_kernel,
				    set_net_ticktime, [DefaultTT, 10])
	    end,
	    wait_until(fun () ->
			       DefaultTT == net_kernel:get_net_ticktime()
		       end),
	    process_flag(trap_exit, OTE),
	    ct:fail(Reason);
	_ ->
	    ok
    end,
    process_flag(trap_exit, OTE),
    stop_node(B),
    stop_node(C),

    %% In case other nodes are connected
    case nodes(connected) of
	[] -> net_kernel:set_net_ticktime(DefaultTT, 0);
	_ ->  rpc:multicall(nodes([this, connected]), net_kernel,
			    set_net_ticktime, [DefaultTT, 5])
    end,

    wait_until(fun () -> DefaultTT == net_kernel:get_net_ticktime() end),
    ok.


wait_for_nodedowns(Tester, Ref) ->
    receive
	{nodedown, Node} ->
	    io:format("~p~n", [{node(), {nodedown, Node}}]),
	    Tester ! {Ref, {node(), {nodedown, Node}}}
    end,
    wait_for_nodedowns(Tester, Ref).

run_tick_change_test(B, C, PrevTT, TT, PaDir) ->
    [DN, EN] = get_nodenames(2, tick_change),

    Tester = self(),
    Ref = make_ref(),
    MonitorNodes = fun (Nodes) ->
			   lists:foreach(
			     fun (N) ->
				     monitor_node(N,true)
			     end,
			     Nodes),
			   wait_for_nodedowns(Tester, Ref)
		   end,

    {ok, D} = start_node(DN, "-kernel net_ticktime "
			 ++ integer_to_list(PrevTT) ++ " -pa " ++ PaDir),

    NMA = spawn_link(fun () -> MonitorNodes([B, C, D]) end),
    NMB = spawn_link(B, fun () -> MonitorNodes([node(), C, D]) end),
    NMC = spawn_link(C, fun () -> MonitorNodes([node(), B, D]) end),

    MaxTT = case PrevTT > TT of
		true  -> PrevTT;
		false -> TT
	    end,

    CheckResult = make_ref(),
    spawn_link(fun () ->
		       receive
		       after (25 + MaxTT)*1000 ->
			       Tester ! CheckResult
		       end
	       end),

    %% In case other nodes than these are connected
    case nodes(connected) -- [B, C, D] of
	[] -> ok;
	OtherNodes -> rpc:multicall(OtherNodes, net_kernel,
				    set_net_ticktime, [TT, 20])
    end,

    change_initiated = net_kernel:set_net_ticktime(TT,20),
    {ongoing_change_to,_} = net_kernel:set_net_ticktime(TT,20),
    sleep(3),
    change_initiated = rpc:call(B,net_kernel,set_net_ticktime,[TT,15]),
    sleep(7),
    change_initiated = rpc:call(C,net_kernel,set_net_ticktime,[TT,10]),

    {ok, E} = start_node(EN, "-kernel net_ticktime "
			 ++ integer_to_list(TT) ++ " -pa " ++ PaDir),
    NME  = spawn_link(E, fun () -> MonitorNodes([node(), B, C, D]) end),
    NMA2 = spawn_link(fun () -> MonitorNodes([E]) end),
    NMB2 = spawn_link(B, fun () -> MonitorNodes([E]) end),
    NMC2 = spawn_link(C, fun () -> MonitorNodes([E]) end),

    receive CheckResult -> ok end,

    unlink(NMA),  exit(NMA, kill),
    unlink(NMB),  exit(NMB, kill),
    unlink(NMC),  exit(NMC, kill),
    unlink(NME),  exit(NME, kill),
    unlink(NMA2), exit(NMA2, kill),
    unlink(NMB2), exit(NMB2, kill),
    unlink(NMC2), exit(NMC2, kill),

    %% The node not changing ticktime should have been disconnected from the
    %% other nodes
    receive {Ref, {Node, {nodedown, D}}} when Node == node() -> ok
    after 0 -> exit({?LINE, no_nodedown})
    end,
    receive {Ref, {B, {nodedown, D}}} -> ok
    after 0 -> exit({?LINE, no_nodedown})
    end,
    receive {Ref, {C, {nodedown, D}}} -> ok
    after 0 -> exit({?LINE, no_nodedown})
    end,
    receive {Ref, {E, {nodedown, D}}} -> ok
    after 0 -> exit({?LINE, no_nodedown})
    end,

    %% No other connections should have been broken
    receive
	{Ref, Reason} ->
	    stop_node(E),
	    exit({?LINE, Reason});
	{'EXIT', Pid, Reason} when Pid == NMA;
				   Pid == NMB;
				   Pid == NMC;
				   Pid == NME;
				   Pid == NMA2;
				   Pid == NMB2;
				   Pid == NMC2 ->
	    stop_node(E),

	    exit({?LINE, {node(Pid), Reason}})
    after 0 ->
	    TT = net_kernel:get_net_ticktime(),
	    TT = rpc:call(B, net_kernel, get_net_ticktime, []),
	    TT = rpc:call(C, net_kernel, get_net_ticktime, []),
	    TT = rpc:call(E, net_kernel, get_net_ticktime, []),
	    stop_node(E),
	    ok
    end.

%%
%% Basic tests of hidden node.
%%
%% Basic test of hidden node.
hidden_node(Config) when is_list(Config) ->
    PaDir = filename:dirname(code:which(?MODULE)),
    VArgs = "-pa " ++ PaDir,
    HArgs = "-hidden -pa " ++ PaDir,
    {ok, V} = start_node(visible_node, VArgs),
    VMN = start_monitor_nodes_proc(V),
    {ok, H} = start_node(hidden_node, HArgs),
    %% Connect visible_node -> hidden_node
    connect_nodes(V, H),
    test_nodes(V, H),
    stop_node(H),
    sleep(5),
    check_monitor_nodes_res(VMN, H),
    stop_node(V),
    {ok, H} = start_node(hidden_node, HArgs),
    HMN = start_monitor_nodes_proc(H),
    {ok, V} = start_node(visible_node, VArgs),
    %% Connect hidden_node -> visible_node
    connect_nodes(H, V),
    test_nodes(V, H),
    stop_node(V),
    sleep(5),
    check_monitor_nodes_res(HMN, V),
    stop_node(H),
    ok.

connect_nodes(A, B) ->
    %% Check that they haven't already connected.
    false = lists:member(A, rpc:call(B, erlang, nodes, [connected])),
    false = lists:member(B, rpc:call(A, erlang, nodes, [connected])),
    %% Connect them.
    pong = rpc:call(A, net_adm, ping, [B]).


test_nodes(V, H) ->
    %% No nodes should be visible on hidden_node
    [] = rpc:call(H, erlang, nodes, []),
    %% visible_node should be hidden on hidden_node
    true = lists:member(V, rpc:call(H, erlang, nodes, [hidden])),
    %% hidden_node node shouldn't be visible on visible_node
    false = lists:member(H, rpc:call(V, erlang, nodes, [])),
    %% hidden_node should be hidden on visible_node
    true = lists:member(H, rpc:call(V, erlang, nodes, [hidden])).

mn_loop(MNs) ->
    receive
	{nodeup, N} ->
	    mn_loop([{nodeup, N}|MNs]);
	{nodedown, N} ->
	    mn_loop([{nodedown, N}|MNs]);
	{monitor_nodes_result, Ref, From} ->
	    From ! {Ref, MNs};
	_ ->
	    mn_loop(MNs)
    end.

start_monitor_nodes_proc(Node) ->
    Ref = make_ref(),
    Starter = self(),
    Pid = spawn(Node,
		fun() ->
			net_kernel:monitor_nodes(true),
			Starter ! Ref,
			mn_loop([])
		end),
    receive
	Ref ->
	    ok
    end,
    Pid.


check_monitor_nodes_res(Pid, Node) ->
    Ref = make_ref(),
    Pid ! {monitor_nodes_result, Ref, self()},
    receive
	{Ref, MNs} ->
	    false = lists:keysearch(Node, 2, MNs)
    end.



%% Check the kernel inet_dist_{listen,connect}_options options.
inet_dist_options_options(Config) when is_list(Config) ->
    Prio = 1,
    case gen_udp:open(0, [{priority,Prio}]) of
	{ok,Socket} ->
	    case inet:getopts(Socket, [priority]) of
		{ok,[{priority,Prio}]} ->
		    ok = gen_udp:close(Socket),
		    do_inet_dist_options_options(Prio);
		_ ->
		    ok = gen_udp:close(Socket),
		    {skip,
		     "Can not set priority "++integer_to_list(Prio)++
			 " on socket"}
	    end;
	{error,_} ->
	    {skip, "Can not set priority on socket"}
    end.

do_inet_dist_options_options(Prio) ->
    PriorityString0 = "[{priority,"++integer_to_list(Prio)++"}]",
    PriorityString =
	case os:cmd("echo [{a,1}]") of
	    "[{a,1}]"++_ ->
		PriorityString0;
	    _ ->
		%% Some shells need quoting of [{}]
		"'"++PriorityString0++"'"
	end,
    InetDistOptions =
	"-hidden "
	"-kernel inet_dist_connect_options "++PriorityString++" "
	"-kernel inet_dist_listen_options "++PriorityString,
    {ok,Node1} =
	start_node(inet_dist_options_1, InetDistOptions),
    {ok,Node2} =
	start_node(inet_dist_options_2, InetDistOptions),
    %%
    pong =
	rpc:call(Node1, net_adm, ping, [Node2]),
    PrioritiesNode1 =
	rpc:call(Node1, ?MODULE, get_socket_priorities, []),
    PrioritiesNode2 =
	rpc:call(Node2, ?MODULE, get_socket_priorities, []),
    io:format("PrioritiesNode1 = ~p", [PrioritiesNode1]),
    io:format("PrioritiesNode2 = ~p", [PrioritiesNode2]),
    Elevated = [P || P <- PrioritiesNode1, P =:= Prio],
    Elevated = [P || P <- PrioritiesNode2, P =:= Prio],
    [_|_] = Elevated,
    %%
    stop_node(Node2),
    stop_node(Node1),
    ok.

get_socket_priorities() ->
    [Priority ||
	{ok,[{priority,Priority}]} <-
	    [inet:getopts(Port, [priority]) ||
		Port <- erlang:ports(),
		element(2, erlang:port_info(Port, name)) =:= "tcp_inet"]].



%%
%% Testcase:
%%   monitor_nodes_nodedown_reason
%%

monitor_nodes_nodedown_reason(Config) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    ok = net_kernel:monitor_nodes(true),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),

    Names = get_numbered_nodenames(5, node),
    [NN1, NN2, NN3, NN4, NN5] = Names,

    {ok, N1} = start_node(NN1),
    {ok, N2} = start_node(NN2),
    {ok, N3} = start_node(NN3),
    {ok, N4} = start_node(NN4, "-hidden"),

    receive {nodeup, N1} -> ok end,
    receive {nodeup, N2} -> ok end,
    receive {nodeup, N3} -> ok end,

    receive {nodeup, N1, []} -> ok end,
    receive {nodeup, N2, []} -> ok end,
    receive {nodeup, N3, []} -> ok end,

    stop_node(N1),
    stop_node(N4),
    true = net_kernel:disconnect(N2),
    TickTime = net_kernel:get_net_ticktime(),
    SleepTime = TickTime + (TickTime div 2),
    spawn(N3, fun () ->
		      block_emu(SleepTime*1000),
		      halt()
	      end),

    receive {nodedown, N1} -> ok end,
    receive {nodedown, N2} -> ok end,
    receive {nodedown, N3} -> ok end,

    receive {nodedown, N1, [{nodedown_reason, R1}]} -> connection_closed = R1 end,
    receive {nodedown, N2, [{nodedown_reason, R2}]} -> disconnect = R2 end,
    receive {nodedown, N3, [{nodedown_reason, R3}]} -> net_tick_timeout = R3 end,

    ok = net_kernel:monitor_nodes(false, [nodedown_reason]),

    {ok, N5} = start_node(NN5),
    stop_node(N5),

    receive {nodeup, N5} -> ok end,
    receive {nodedown, N5} -> ok end,
    print_my_messages(),
    ok = check_no_nodedown_nodeup(1000),
    ok = net_kernel:monitor_nodes(false),
    MonNodeState = monitor_node_state(),
    ok.


monitor_nodes_complex_nodedown_reason(Config) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    Me = self(),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    [Name] = get_nodenames(1, monitor_nodes_complex_nodedown_reason),
    {ok, Node} = start_node(Name, ""),
    Pid = spawn(Node,
		fun() ->
			Me ! {stuff,
			      self(),
			      [make_ref(),
			       {processes(), erlang:ports()}]}
		end),
    receive {nodeup, Node, []} -> ok end,
    {ok, NodeInfo} = net_kernel:node_info(Node),
    {value,{owner, Owner}} = lists:keysearch(owner, 1, NodeInfo),
    ComplexTerm = receive {stuff, Pid, _} = Msg ->
			  {Msg, term_to_binary(Msg)}
		  end,
    exit(Owner, ComplexTerm),
    receive
	{nodedown, Node, [{nodedown_reason, NodeDownReason}]} ->
	    ok
    end,
    %% If the complex nodedown_reason messed something up garbage collections
    %% are likely to dump core
    garbage_collect(),
    garbage_collect(),
    garbage_collect(),
    ComplexTerm = NodeDownReason,
    ok = net_kernel:monitor_nodes(false, [nodedown_reason]),
    no_msgs(),
    MonNodeState = monitor_node_state(),
    ok.




%%
%% Testcase:
%%   monitor_nodes_node_type
%%

monitor_nodes_node_type(Config) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    ok = net_kernel:monitor_nodes(true),
    ok = net_kernel:monitor_nodes(true, [{node_type, all}]),
    Names = get_numbered_nodenames(9, node),
    [NN1, NN2, NN3, NN4, NN5, NN6, NN7, NN8, NN9] = Names,

    {ok, N1} = start_node(NN1),
    {ok, N2} = start_node(NN2),
    {ok, N3} = start_node(NN3, "-hidden"),
    {ok, N4} = start_node(NN4, "-hidden"),

    receive {nodeup, N1} -> ok end,
    receive {nodeup, N2} -> ok end,

    receive {nodeup, N1, [{node_type, visible}]} -> ok end,
    receive {nodeup, N2, [{node_type, visible}]} -> ok end,
    receive {nodeup, N3, [{node_type, hidden}]} -> ok end,
    receive {nodeup, N4, [{node_type, hidden}]} -> ok end,

    stop_node(N1),
    stop_node(N2),
    stop_node(N3),
    stop_node(N4),

    receive {nodedown, N1} -> ok end,
    receive {nodedown, N2} -> ok end,

    receive {nodedown, N1, [{node_type, visible}]} -> ok end,
    receive {nodedown, N2, [{node_type, visible}]} -> ok end,
    receive {nodedown, N3, [{node_type, hidden}]} -> ok end,
    receive {nodedown, N4, [{node_type, hidden}]} -> ok end,

    ok = net_kernel:monitor_nodes(false, [{node_type, all}]),
    {ok, N5} = start_node(NN5),

    receive {nodeup, N5} -> ok end,
    stop_node(N5),
    receive {nodedown, N5} -> ok end,

    ok = net_kernel:monitor_nodes(true, [{node_type, hidden}]),
    {ok, N6} = start_node(NN6),
    {ok, N7} = start_node(NN7, "-hidden"),


    receive {nodeup, N6} -> ok end,
    receive {nodeup, N7, [{node_type, hidden}]} -> ok end,
    stop_node(N6),
    stop_node(N7),

    receive {nodedown, N6} -> ok end,
    receive {nodedown, N7, [{node_type, hidden}]} -> ok end,

    ok = net_kernel:monitor_nodes(true, [{node_type, visible}]),
    ok = net_kernel:monitor_nodes(false, [{node_type, hidden}]),
    ok = net_kernel:monitor_nodes(false),

    {ok, N8} = start_node(NN8),
    {ok, N9} = start_node(NN9, "-hidden"),

    receive {nodeup, N8, [{node_type, visible}]} -> ok end,
    stop_node(N8),
    stop_node(N9),

    receive {nodedown, N8, [{node_type, visible}]} -> ok end,
    print_my_messages(),
    ok = check_no_nodedown_nodeup(1000),
    ok = net_kernel:monitor_nodes(false, [{node_type, visible}]),
    MonNodeState = monitor_node_state(),
    ok.


%%
%% Testcase:
%%   monitor_nodes
%%

monitor_nodes_misc(Config) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    ok = net_kernel:monitor_nodes(true),
    ok = net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason, {node_type, all}]),
    Names = get_numbered_nodenames(3, node),
    [NN1, NN2, NN3] = Names,

    {ok, N1} = start_node(NN1),
    {ok, N2} = start_node(NN2, "-hidden"),

    receive {nodeup, N1} -> ok end,

    receive {nodeup, N1, [{node_type, visible}]} -> ok end,
    receive {nodeup, N1, [{node_type, visible}]} -> ok end,
    receive {nodeup, N2, [{node_type, hidden}]} -> ok end,
    receive {nodeup, N2, [{node_type, hidden}]} -> ok end,

    stop_node(N1),
    stop_node(N2),

    VisbleDownInfo = lists:sort([{node_type, visible},
				 {nodedown_reason, connection_closed}]),
    HiddenDownInfo = lists:sort([{node_type, hidden},
				 {nodedown_reason, connection_closed}]),

    receive {nodedown, N1} -> ok end,

    receive {nodedown, N1, Info1A} -> VisbleDownInfo = lists:sort(Info1A) end,
    receive {nodedown, N1, Info1B} -> VisbleDownInfo = lists:sort(Info1B) end,
    receive {nodedown, N2, Info2A} -> HiddenDownInfo = lists:sort(Info2A) end,
    receive {nodedown, N2, Info2B} -> HiddenDownInfo = lists:sort(Info2B) end,

    ok = net_kernel:monitor_nodes(false, [{node_type, all}, nodedown_reason]),

    {ok, N3} = start_node(NN3),
    receive {nodeup, N3} -> ok end,
    stop_node(N3),
    receive {nodedown, N3} -> ok end,
    print_my_messages(),
    ok = check_no_nodedown_nodeup(1000),
    ok = net_kernel:monitor_nodes(false),
    MonNodeState = monitor_node_state(),
    ok.


%% Tests that {nodeup, Node} messages are received before
%% messages from Node and that {nodedown, Node} messages are
%% received after messages from Node.
monitor_nodes_otp_6481(Config) when is_list(Config) ->
    io:format("Testing nodedown...~n"),
    monitor_nodes_otp_6481_test(Config, nodedown),
    io:format("ok~n"),
    io:format("Testing nodeup...~n"),
    monitor_nodes_otp_6481_test(Config, nodeup),
    io:format("ok~n"),
    ok.

monitor_nodes_otp_6481_test(Config, TestType) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    NodeMsg = make_ref(),
    Me = self(),
    [Name] = get_nodenames(1, monitor_nodes_otp_6481),
    case TestType of
	nodedown -> ok = net_kernel:monitor_nodes(true);
	nodeup -> ok
    end,
    Seq = lists:seq(1,10000),
    MN = spawn_link(
	   fun () ->
		   lists:foreach(
		     fun (_) ->
			     ok = net_kernel:monitor_nodes(true)
		     end,
		     Seq),
		   Me ! {mon_set, self()},
		   receive after infinity -> ok end
	   end),
    receive {mon_set, MN} -> ok end,
    case TestType of
	nodedown -> ok;
	nodeup -> ok = net_kernel:monitor_nodes(true)
    end,

    %% Whitebox:
    %% nodedown test: Since this process was the first one monitoring
    %%                nodes this process will be the first one notified
    %%                on nodedown.
    %% nodeup test:   Since this process was the last one monitoring
    %%                nodes this process will be the last one notified
    %%                on nodeup

    %% Verify the monitor_nodes order expected
    TestMonNodeState = monitor_node_state(),
    %% io:format("~p~n", [TestMonNodeState]),
    TestMonNodeState =
	MonNodeState
	++ case TestType of
	       nodedown -> [{self(), []}];
	       nodeup -> []
	   end
	++ lists:map(fun (_) -> {MN, []} end, Seq)
	++ case TestType of
	       nodedown -> [];
	       nodeup -> [{self(), []}]
	   end,


    {ok, Node} = start_node(Name, "", this),
    receive {nodeup, Node} -> ok end,

    RemotePid = spawn(Node,
		      fun () ->
			      receive after 1500 -> ok end,
			      %% infinite loop of msgs
			      %% we want an endless stream of messages and the kill
			      %% the node mercilessly.
			      %% We then want to ensure that the nodedown message arrives
			      %% last ... without garbage after it.
			      _ = spawn(fun() -> node_loop_send(Me, NodeMsg, 1) end),
			      receive {Me, kill_it} -> ok end,
			      halt()
		      end),

    net_kernel:disconnect(Node),
    receive {nodedown, Node} -> ok end,

    %% Verify that '{nodeup, Node}' comes before '{NodeMsg, 1}' (the message
    %% bringing up the connection).
    {nodeup, Node} = receive Msg1 -> Msg1 end,
    {NodeMsg, N}   = receive Msg2 -> Msg2 end,
    %% msg stream has begun, kill the node
    RemotePid ! {self(), kill_it},

    %% Verify that '{nodedown, Node}' comes after the last '{NodeMsg, N}'
    %% message.
    {nodedown, Node} = flush_node_msgs(NodeMsg, N+1),
    no_msgs(500),

    Mon = erlang:monitor(process, MN),
    unlink(MN),
    exit(MN, bang),
    receive {'DOWN', Mon, process, MN, bang} -> ok end,
    ok = net_kernel:monitor_nodes(false),
    MonNodeState = monitor_node_state(),
    ok.

flush_node_msgs(NodeMsg, No) ->
    case receive Msg -> Msg end of
	{NodeMsg, N} when N >= No ->
	    flush_node_msgs(NodeMsg, N+1);
	OtherMsg ->
	    OtherMsg
    end.

node_loop_send(Pid, Msg, No) ->
    Pid ! {Msg, No},
    node_loop_send(Pid, Msg, No + 1).

monitor_nodes_errors(Config) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    error = net_kernel:monitor_nodes(asdf),
    {error,
     {unknown_options,
      [gurka]}} = net_kernel:monitor_nodes(true,
					   [gurka]),
    {error,
     {options_not_a_list,
      gurka}} = net_kernel:monitor_nodes(true,
					 gurka),
    {error,
     {option_value_mismatch,
      [{node_type,visible},
       {node_type,hidden}]}}
	= net_kernel:monitor_nodes(true,
				   [{node_type,hidden},
				    {node_type,visible}]),
    {error,
     {option_value_mismatch,
      [{node_type,visible},
       {node_type,all}]}}
	= net_kernel:monitor_nodes(true,
				   [{node_type,all},
				    {node_type,visible}]),
    {error,
     {bad_option_value,
      {node_type,
       blaha}}}
	= net_kernel:monitor_nodes(true, [{node_type, blaha}]),
    MonNodeState = monitor_node_state(),
    ok.

monitor_nodes_combinations(Config) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    monitor_nodes_all_comb(true),
    [VisibleName, HiddenName] = get_nodenames(2,
					      monitor_nodes_combinations),
    {ok, Visible} = start_node(VisibleName, ""),
    receive_all_comb_nodeup_msgs(visible, Visible),
    no_msgs(),
    stop_node(Visible),
    receive_all_comb_nodedown_msgs(visible, Visible, connection_closed),
    no_msgs(),
    {ok, Hidden} = start_node(HiddenName, "-hidden"),
    receive_all_comb_nodeup_msgs(hidden, Hidden),
    no_msgs(),
    stop_node(Hidden),
    receive_all_comb_nodedown_msgs(hidden, Hidden, connection_closed),
    no_msgs(),
    monitor_nodes_all_comb(false),
    MonNodeState = monitor_node_state(),
    no_msgs(),
    ok.

monitor_nodes_all_comb(Flag) ->
    ok = net_kernel:monitor_nodes(Flag),
    ok = net_kernel:monitor_nodes(Flag,
				  [nodedown_reason]),
    ok = net_kernel:monitor_nodes(Flag,
				  [{node_type, hidden}]),
    ok = net_kernel:monitor_nodes(Flag,
				  [{node_type, visible}]),
    ok = net_kernel:monitor_nodes(Flag,
				  [{node_type, all}]),
    ok = net_kernel:monitor_nodes(Flag,
				  [nodedown_reason,
				   {node_type, hidden}]),
    ok = net_kernel:monitor_nodes(Flag,
				  [nodedown_reason,
				   {node_type, visible}]),
    ok = net_kernel:monitor_nodes(Flag,
				  [nodedown_reason,
				   {node_type, all}]),
    %% There currently are 8 different combinations
    8.


receive_all_comb_nodeup_msgs(visible, Node) ->
    io:format("Receive nodeup visible...~n"),
    Exp = [{nodeup, Node},
	   {nodeup, Node, []}]
	++ mk_exp_mn_all_comb_nodeup_msgs_common(visible, Node),
    receive_mn_msgs(Exp),
    io:format("ok~n"),
    ok;
receive_all_comb_nodeup_msgs(hidden, Node) ->
    io:format("Receive nodeup hidden...~n"),
    Exp = mk_exp_mn_all_comb_nodeup_msgs_common(hidden, Node),
    receive_mn_msgs(Exp),
    io:format("ok~n"),
    ok.

mk_exp_mn_all_comb_nodeup_msgs_common(Type, Node) ->
    InfoNt = [{node_type, Type}],
    [{nodeup, Node, InfoNt},
     {nodeup, Node, InfoNt},
     {nodeup, Node, InfoNt},
     {nodeup, Node, InfoNt}].

receive_all_comb_nodedown_msgs(visible, Node, Reason) ->
    io:format("Receive nodedown visible...~n"),
    Exp = [{nodedown, Node},
	   {nodedown, Node, [{nodedown_reason, Reason}]}]
	++ mk_exp_mn_all_comb_nodedown_msgs_common(visible,
						   Node,
						   Reason),
    receive_mn_msgs(Exp),
    io:format("ok~n"),
    ok;
receive_all_comb_nodedown_msgs(hidden, Node, Reason) ->
    io:format("Receive nodedown hidden...~n"),
    Exp = mk_exp_mn_all_comb_nodedown_msgs_common(hidden, Node, Reason),
    receive_mn_msgs(Exp),
    io:format("ok~n"),
    ok.

mk_exp_mn_all_comb_nodedown_msgs_common(Type, Node, Reason) ->
    InfoNt = [{node_type, Type}],
    InfoNdrNt = lists:sort([{nodedown_reason, Reason}]++InfoNt),
    [{nodedown, Node, InfoNt},
     {nodedown, Node, InfoNt},
     {nodedown, Node, InfoNdrNt},
     {nodedown, Node, InfoNdrNt}].

receive_mn_msgs([]) ->
    ok;
receive_mn_msgs(Msgs) ->
    io:format("Expecting msgs: ~p~n", [Msgs]),
    receive
	{_Dir, _Node} = Msg ->
	    io:format("received ~p~n", [Msg]),
	    case lists:member(Msg, Msgs) of
		true -> receive_mn_msgs(lists:delete(Msg, Msgs));
		false -> ct:fail({unexpected_message, Msg,
				  expected_messages, Msgs})
	    end;
	{Dir, Node, Info} ->
	    Msg = {Dir, Node, lists:sort(Info)},
	    io:format("received ~p~n", [Msg]),
	    case lists:member(Msg, Msgs) of
		true -> receive_mn_msgs(lists:delete(Msg, Msgs));
		false -> ct:fail({unexpected_message, Msg,
				  expected_messages, Msgs})
	    end;
	Msg ->
	    io:format("received ~p~n", [Msg]),
	    ct:fail({unexpected_message, Msg,
		     expected_messages, Msgs})
    end.

monitor_nodes_cleanup(Config) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    Me = self(),
    No = monitor_nodes_all_comb(true),
    Inf = spawn(fun () ->
			monitor_nodes_all_comb(true),
			Me ! {mons_set, self()},
			receive after infinity -> ok end
		end),
    TO = spawn(fun () ->
		       monitor_nodes_all_comb(true),
		       Me ! {mons_set, self()},
		       receive after 500 -> ok end
	       end),
    receive {mons_set, Inf} -> ok end,
    receive {mons_set, TO} -> ok end,
    MNLen = length(MonNodeState) + No*3,
    MNLen = length(monitor_node_state()),
    MonInf = erlang:monitor(process, Inf),
    MonTO = erlang:monitor(process, TO),
    exit(Inf, bang),
    No = monitor_nodes_all_comb(false),
    receive {'DOWN', MonInf, process, Inf, bang} -> ok end,
    receive {'DOWN', MonTO, process, TO, normal} -> ok end,
    MonNodeState = monitor_node_state(),
    no_msgs(),
    ok.

monitor_nodes_many(Config) when is_list(Config) ->
    MonNodeState = monitor_node_state(),
    [Name] = get_nodenames(1, monitor_nodes_many),
    %% We want to perform more than 2^16 net_kernel:monitor_nodes
    %% since this will wrap an internal counter
    No = (1 bsl 16) + 17,
    repeat(fun () -> ok = net_kernel:monitor_nodes(true) end, No),
    No = length(monitor_node_state()) - length(MonNodeState),
    {ok, Node} = start_node(Name),
    repeat(fun () -> receive {nodeup, Node} -> ok end end, No),
    stop_node(Node),
    repeat(fun () -> receive {nodedown, Node} -> ok end end, No),
    ok = net_kernel:monitor_nodes(false),
    no_msgs(10),
    MonNodeState = monitor_node_state(),
    ok.

%% Misc. functions

monitor_node_state() ->
    erts_debug:set_internal_state(available_internal_state, true),
    MonitoringNodes = erts_debug:get_internal_state(monitoring_nodes),
    erts_debug:set_internal_state(available_internal_state, false),
    MonitoringNodes.


check_no_nodedown_nodeup(TimeOut) ->
    receive
	{nodeup, _, _} = Msg -> ct:fail({unexpected_nodeup, Msg});
	{nodeup, _} = Msg -> ct:fail({unexpected_nodeup, Msg});
	{nodedown, _, _} = Msg -> ct:fail({unexpected_nodedown, Msg});
	{nodedown, _} = Msg -> ct:fail({unexpected_nodedown, Msg})
    after TimeOut ->
	    ok
    end.

print_my_messages() ->
    {messages, Messages} = process_info(self(), messages),
    io:format("Messages: ~p~n", [Messages]),
    ok.


sleep(T) -> receive after T * 1000 -> ok end.	

start_node(Name, Param, this) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, peer, [{args, NewParam}, {erl, [this]}]);
start_node(Name, Param, "this") ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, peer, [{args, NewParam}, {erl, [this]}]);
start_node(Name, Param, Rel) when is_atom(Rel) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, peer, [{args, NewParam}, {erl, [{release, atom_to_list(Rel)}]}]);
start_node(Name, Param, Rel) when is_list(Rel) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, peer, [{args, NewParam}, {erl, [{release, Rel}]}]).

start_node(Name, Param) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, NewParam}]).

start_node(Name) ->
    start_node(Name, "").

stop_node(Node) ->
    test_server:stop_node(Node).

get_nodenames(N, T) ->
    get_nodenames(N, T, []).

get_nodenames(0, _, Acc) ->
    Acc;
get_nodenames(N, T, Acc) ->
    U = erlang:unique_integer([positive]),
    get_nodenames(N-1, T, [list_to_atom(atom_to_list(T)
					++ "-"
					++ ?MODULE_STRING
					++ "-"
					++ integer_to_list(U)) | Acc]).

get_numbered_nodenames(N, T) ->
    get_numbered_nodenames(N, T, []).

get_numbered_nodenames(0, _, Acc) ->
    Acc;
get_numbered_nodenames(N, T, Acc) ->
    U = erlang:unique_integer([positive]),
    NL = [list_to_atom(atom_to_list(T) ++ integer_to_list(N)
		       ++ "-"
		       ++ ?MODULE_STRING
		       ++ "-"
		       ++ integer_to_list(U)) | Acc],
    get_numbered_nodenames(N-1, T, NL).

wait_until(Fun) ->
    case Fun() of
	true ->
	    ok;
	_ ->
	    receive
	    after 100 ->
		    wait_until(Fun)
	    end
    end.

repeat(Fun, 0) when is_function(Fun) ->
    ok;
repeat(Fun, N) when is_function(Fun), is_integer(N), N > 0 ->
    Fun(),
    repeat(Fun, N-1).

no_msgs(Wait) ->
    receive after Wait -> no_msgs() end.

no_msgs() ->
    {messages, []} = process_info(self(), messages).

block_emu(Ms) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Res = erts_debug:set_internal_state(block, Ms),
    erts_debug:set_internal_state(available_internal_state, false),
    Res.
