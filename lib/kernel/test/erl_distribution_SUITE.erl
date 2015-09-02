%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2015. All Rights Reserved.
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

%-define(line_trace, 1).
-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([tick/1, tick_change/1, illegal_nodenames/1, hidden_node/1,
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

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [tick, tick_change, illegal_nodenames, hidden_node,
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
    Dog=?t:timetrap(?t:minutes(4)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

tick(suite) -> [];
tick(doc) -> [];
tick(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(120)),
    PaDir = filename:dirname(code:which(erl_distribution_SUITE)),
    
    %% First check that the normal case is OK!
    ?line {ok, Node} = start_node(dist_test, "-pa " ++ PaDir),
    rpc:call(Node, erl_distribution_SUITE, tick_cli_test, [node()]),
    
    erlang:monitor_node(Node, true),
    receive
	{nodedown, Node} ->
	    test_server:fail("nodedown from other node")
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

    ?line {ok, ServNode} = start_node(dist_test_server,
				      "-kernel net_ticktime 100 "
				      "-pa " ++ PaDir),
    rpc:call(ServNode, erl_distribution_SUITE, tick_serv_test, [Node, node()]),

    ?line {ok, _} = start_node(dist_test,
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
	    test_server:fail(Error);
	{nodedown, Node} ->
	    stop_node(ServNode),
	    test_server:fail("client node died");
	{nodedown, ServNode} ->
	    stop_node(Node),
	    test_server:fail("server node died")
    end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

table_waste(doc) ->
    ["Checks that pinging nonexistyent nodes does not waste space in distribution table"];
table_waste(suite) ->
    [];
table_waste(Config) when is_list(Config) ->
    ?line {ok, HName} = inet:gethostname(), 
    F = fun(0,_F) -> [];
	   (N,F) -> 
		?line Name = list_to_atom("erl_distribution_"++integer_to_list(N)++
				    "@"++HName), 
		?line pang = net_adm:ping(Name), 
		?line F(N-1,F) 
	end,
    ?line F(256,F),
    ?line {ok, N} = start_node(erl_distribution_300,""),
    ?line stop_node(N),
    ok.
    
    

illegal_nodenames(doc) ->
    ["Test that pinging an illegal nodename does not kill the node"];
illegal_nodenames(suite) ->
    [];
illegal_nodenames(Config) when is_list(Config) ->
    ?line Dog=?t:timetrap(?t:minutes(2)),
    PaDir = filename:dirname(code:which(erl_distribution_SUITE)),
    ?line {ok, Node}=start_node(illegal_nodenames, "-pa " ++ PaDir),
    monitor_node(Node, true),
    ?line RPid=rpc:call(Node, erlang, spawn,
	     [?MODULE, pinger, [self()]]),
    receive
	{RPid, pinged} ->
	    ok;
	{nodedown, Node} ->
	    ?t:fail("Remote node died.")
    end,
    stop_node(Node),
    ?t:timetrap_cancel(Dog),
    ok.

pinger(Starter) ->
    io:format("Starter:~p~n",[Starter]),
    net_adm:ping(a@b@c),
    Starter ! {self(), pinged},
    ok.


net_setuptime(doc) -> ["Test that you can set the net_setuptime properly"];
net_setuptime(Config) when is_list(Config) ->
    %% In this test case, we reluctantly accept shorter times than the given
    %% setup time, because the connection attempt can end in a
    %% "Host unreachable" error before the timeout fires.

    Res0 = do_test_setuptime("2"),
    io:format("Res0 = ~p", [Res0]),
    ?line true = (Res0 =< 4000),
    Res1 = do_test_setuptime("0.3"),
    io:format("Res1 = ~p", [Res1]),
    ?line true = (Res1 =< 500),
    ok.

do_test_setuptime(Setuptime) when is_list(Setuptime) ->
    ?line PaDir = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node} = start_node(dist_setuptime_test, "-pa " ++ PaDir ++ 
				  " -kernel net_setuptime " ++ Setuptime),
    ?line Res = rpc:call(Node,?MODULE,time_ping,[?DUMMY_NODE]),
    ?line stop_node(Node),
    Res.

time_ping(Node) ->
    T0 = erlang:monotonic_time(),
    pang = net_adm:ping(Node),
    T1 = erlang:monotonic_time(),
    erlang:convert_time_unit(T1 - T0, native, milli_seconds).

%% Keep the connection with the client node up.
%% This is neccessary as the client node runs with much shorter
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
						    milli_seconds),
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


tick_change(doc) -> ["OTP-4255"];
tick_change(suite) -> [];
tick_change(Config) when is_list(Config) ->
    ?line PaDir = filename:dirname(code:which(?MODULE)),
    ?line [BN, CN] = get_nodenames(2, tick_change),
    ?line DefaultTT = net_kernel:get_net_ticktime(),
    ?line unchanged = net_kernel:set_net_ticktime(DefaultTT, 60),
    ?line case DefaultTT of
	      I when is_integer(I) -> ?line ok;
	      _                 -> ?line ?t:fail(DefaultTT)
	  end,

    % In case other nodes are connected
    case nodes(connected) of
	[] -> ?line net_kernel:set_net_ticktime(10, 0);
	_ ->  ?line rpc:multicall(nodes([this, connected]), net_kernel,
				  set_net_ticktime, [10, 5])
    end,

    ?line wait_until(fun () -> 10 == net_kernel:get_net_ticktime() end),
    ?line {ok, B} = start_node(BN, "-kernel net_ticktime 10 -pa " ++ PaDir),
    ?line {ok, C} = start_node(CN, "-kernel net_ticktime 10 -hidden -pa "
			       ++ PaDir),

    ?line OTE = process_flag(trap_exit, true),
    case catch begin
		   ?line run_tick_change_test(B, C, 10, 1, PaDir),
		   ?line run_tick_change_test(B, C, 1, 10, PaDir)
	       end of
	{'EXIT', Reason} ->
	    ?line stop_node(B),
	    ?line stop_node(C),
	    %% In case other nodes are connected
	    case nodes(connected) of
		[] -> ?line net_kernel:set_net_ticktime(DefaultTT, 0);
		_ ->  ?line rpc:multicall(nodes([this, connected]), net_kernel,
					  set_net_ticktime, [DefaultTT, 10])
	    end,
	    ?line wait_until(fun () -> 
				     DefaultTT == net_kernel:get_net_ticktime()
			     end),
	    ?line process_flag(trap_exit, OTE),
	    ?t:fail(Reason);
	_ ->
	    ok
    end,
    ?line process_flag(trap_exit, OTE),
    ?line stop_node(B),
    ?line stop_node(C),

    % In case other nodes are connected
    case nodes(connected) of
	[] -> ?line net_kernel:set_net_ticktime(DefaultTT, 0);
	_ ->  ?line rpc:multicall(nodes([this, connected]), net_kernel,
				  set_net_ticktime, [DefaultTT, 5])
    end,

    ?line wait_until(fun () -> DefaultTT == net_kernel:get_net_ticktime() end),
    ?line ok.


wait_for_nodedowns(Tester, Ref) ->
    receive
	{nodedown, Node} ->
	    ?t:format("~p~n", [{node(), {nodedown, Node}}]),
	    ?line Tester ! {Ref, {node(), {nodedown, Node}}}
    end,
    wait_for_nodedowns(Tester, Ref).

run_tick_change_test(B, C, PrevTT, TT, PaDir) ->
    ?line [DN, EN] = get_nodenames(2, tick_change),

    ?line Tester = self(),
    ?line Ref = make_ref(),
    ?line MonitorNodes = fun (Nodes) ->
				 ?line lists:foreach(
					 fun (N) ->
						 ?line monitor_node(N,true)
					 end,
					 Nodes),
				 wait_for_nodedowns(Tester, Ref)
			 end,

    ?line {ok, D} = start_node(DN, "-kernel net_ticktime "
			       ++ integer_to_list(PrevTT) ++ " -pa " ++ PaDir),

    ?line NMA = spawn_link(fun () -> MonitorNodes([B, C, D]) end),
    ?line NMB = spawn_link(B, fun () -> MonitorNodes([node(), C, D]) end),
    ?line NMC = spawn_link(C, fun () -> MonitorNodes([node(), B, D]) end),

    ?line MaxTT = case PrevTT > TT of
		      true  -> ?line PrevTT;
		      false -> ?line TT
		  end,

    ?line CheckResult = make_ref(),
    ?line spawn_link(fun () ->
			     receive
			     after (25 + MaxTT)*1000 ->
				     Tester ! CheckResult
			     end
		     end),

    % In case other nodes than these are connected
    case nodes(connected) -- [B, C, D] of
	[] -> ?line ok;
	OtherNodes -> ?line rpc:multicall(OtherNodes, net_kernel,
					  set_net_ticktime, [TT, 20])
    end,

    ?line change_initiated = net_kernel:set_net_ticktime(TT,20),
    ?line {ongoing_change_to,_} = net_kernel:set_net_ticktime(TT,20),
    ?line sleep(3),
    ?line change_initiated = rpc:call(B,net_kernel,set_net_ticktime,[TT,15]),
    ?line sleep(7),
    ?line change_initiated = rpc:call(C,net_kernel,set_net_ticktime,[TT,10]),

    ?line {ok, E} = start_node(EN, "-kernel net_ticktime "
			       ++ integer_to_list(TT) ++ " -pa " ++ PaDir),
    ?line NME  = spawn_link(E, fun () -> MonitorNodes([node(), B, C, D]) end),
    ?line NMA2 = spawn_link(fun () -> MonitorNodes([E]) end),
    ?line NMB2 = spawn_link(B, fun () -> MonitorNodes([E]) end),
    ?line NMC2 = spawn_link(C, fun () -> MonitorNodes([E]) end),

    receive CheckResult -> ?line ok end,

    ?line unlink(NMA),  exit(NMA, kill),
    ?line unlink(NMB),  exit(NMB, kill),
    ?line unlink(NMC),  exit(NMC, kill),
    ?line unlink(NME),  exit(NME, kill),
    ?line unlink(NMA2), exit(NMA2, kill),
    ?line unlink(NMB2), exit(NMB2, kill),
    ?line unlink(NMC2), exit(NMC2, kill),

    %% The node not changing ticktime should have been disconnected from the
    %% other nodes
    receive {Ref, {Node, {nodedown, D}}} when Node == node() -> ?line ok
    after 0 -> ?line exit({?LINE, no_nodedown})
    end,
    receive {Ref, {B, {nodedown, D}}} -> ?line ok
    after 0 -> ?line exit({?LINE, no_nodedown})
    end,
    receive {Ref, {C, {nodedown, D}}} -> ?line ok
    after 0 -> ?line exit({?LINE, no_nodedown})
    end,
    receive {Ref, {E, {nodedown, D}}} -> ?line ok
    after 0 -> ?line exit({?LINE, no_nodedown})
    end,

    %% No other connections should have been broken
    receive
	{Ref, Reason} ->
	    ?line stop_node(E),
	    ?line exit({?LINE, Reason});
	{'EXIT', Pid, Reason} when Pid == NMA;
				   Pid == NMB;
				   Pid == NMC;
				   Pid == NME;
				   Pid == NMA2;
				   Pid == NMB2;
				   Pid == NMC2 ->
	    ?line stop_node(E),

	    ?line exit({?LINE, {node(Pid), Reason}})
    after 0 ->
	    ?line TT = net_kernel:get_net_ticktime(),
	    ?line TT = rpc:call(B, net_kernel, get_net_ticktime, []),
	    ?line TT = rpc:call(C, net_kernel, get_net_ticktime, []),
	    ?line TT = rpc:call(E, net_kernel, get_net_ticktime, []),
	    ?line stop_node(E),
	    ?line ok
    end.

%%
%% Basic tests of hidden node.
%%
hidden_node(doc) ->
    ["Basic test of hidden node"];
hidden_node(suite) ->
    [];
hidden_node(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:seconds(40)),
    PaDir = filename:dirname(code:which(?MODULE)),
    VArgs = "-pa " ++ PaDir,
    HArgs = "-hidden -pa " ++ PaDir,
    ?line {ok, V} = start_node(visible_node, VArgs),
    VMN = start_monitor_nodes_proc(V),
    ?line {ok, H} = start_node(hidden_node, HArgs),
    % Connect visible_node -> hidden_node
    connect_nodes(V, H),
    test_nodes(V, H),
    stop_node(H),
    sleep(5),
    check_monitor_nodes_res(VMN, H),
    stop_node(V),
    ?line {ok, H} = start_node(hidden_node, HArgs),
    HMN = start_monitor_nodes_proc(H),
    ?line {ok, V} = start_node(visible_node, VArgs),
    % Connect hidden_node -> visible_node
    connect_nodes(H, V),
    test_nodes(V, H),
    stop_node(V),
    sleep(5),
    check_monitor_nodes_res(HMN, V),
    stop_node(H),
    ?line ?t:timetrap_cancel(Dog),
    ok.

connect_nodes(A, B) ->
    % Check that they haven't already connected.
    ?line false = lists:member(A, rpc:call(B, erlang, nodes, [connected])),
    ?line false = lists:member(B, rpc:call(A, erlang, nodes, [connected])),
    % Connect them.
    ?line pong = rpc:call(A, net_adm, ping, [B]).
    

test_nodes(V, H) ->
    % No nodes should be visible on hidden_node
    ?line [] = rpc:call(H, erlang, nodes, []),
    % visible_node should be hidden on hidden_node
    ?line true = lists:member(V, rpc:call(H, erlang, nodes, [hidden])),
    % hidden_node node shouldn't be visible on visible_node
    ?line false = lists:member(H, rpc:call(V, erlang, nodes, [])),
    % hidden_node should be hidden on visible_node
    ?line true = lists:member(H, rpc:call(V, erlang, nodes, [hidden])).

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
	    ?line false = lists:keysearch(Node, 2, MNs)
    end.



inet_dist_options_options(suite) -> [];
inet_dist_options_options(doc) ->
    ["Check the kernel inet_dist_{listen,connect}_options options"];
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
    ?line {ok,Node1} =
	start_node(inet_dist_options_1, InetDistOptions),
    ?line {ok,Node2} =
	start_node(inet_dist_options_2, InetDistOptions),
    %%
    ?line pong =
	rpc:call(Node1, net_adm, ping, [Node2]),
    ?line PrioritiesNode1 =
	rpc:call(Node1, ?MODULE, get_socket_priorities, []),
    ?line PrioritiesNode2 =
	rpc:call(Node2, ?MODULE, get_socket_priorities, []),
    ?line ?t:format("PrioritiesNode1 = ~p", [PrioritiesNode1]),
    ?line ?t:format("PrioritiesNode2 = ~p", [PrioritiesNode2]),
    ?line Elevated = [P || P <- PrioritiesNode1, P =:= Prio],
    ?line Elevated = [P || P <- PrioritiesNode2, P =:= Prio],
    ?line [_|_] = Elevated,
    %%
    ?line stop_node(Node2),
    ?line stop_node(Node1),
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

monitor_nodes_nodedown_reason(doc) -> [];
monitor_nodes_nodedown_reason(suite) -> [];
monitor_nodes_nodedown_reason(Config) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line ok = net_kernel:monitor_nodes(true),
    ?line ok = net_kernel:monitor_nodes(true, [nodedown_reason]),

    ?line Names = get_numbered_nodenames(5, node),
    ?line [NN1, NN2, NN3, NN4, NN5] = Names,

    ?line {ok, N1} = start_node(NN1),
    ?line {ok, N2} = start_node(NN2),
    ?line {ok, N3} = start_node(NN3),
    ?line {ok, N4} = start_node(NN4, "-hidden"),
    
    ?line receive {nodeup, N1} -> ok end,
    ?line receive {nodeup, N2} -> ok end,
    ?line receive {nodeup, N3} -> ok end,

    ?line receive {nodeup, N1, []} -> ok end,
    ?line receive {nodeup, N2, []} -> ok end,
    ?line receive {nodeup, N3, []} -> ok end,

    ?line stop_node(N1),
    ?line stop_node(N4),
    ?line true = net_kernel:disconnect(N2),
    ?line TickTime = net_kernel:get_net_ticktime(),
    ?line SleepTime = TickTime + (TickTime div 4),
    ?line spawn(N3, fun () ->
			    block_emu(SleepTime*1000),
			    halt()
		    end),

    ?line receive {nodedown, N1} -> ok end,
    ?line receive {nodedown, N2} -> ok end,
    ?line receive {nodedown, N3} -> ok end,

    ?line receive {nodedown, N1, [{nodedown_reason, R1}]} -> connection_closed = R1 end,
    ?line receive {nodedown, N2, [{nodedown_reason, R2}]} -> disconnect = R2 end,
    ?line receive {nodedown, N3, [{nodedown_reason, R3}]} -> net_tick_timeout = R3 end,

    ?line ok = net_kernel:monitor_nodes(false, [nodedown_reason]),

    ?line {ok, N5} = start_node(NN5),
    ?line stop_node(N5),

    ?line receive {nodeup, N5} -> ok end,
    ?line receive {nodedown, N5} -> ok end,
    ?line print_my_messages(),
    ?line ok = check_no_nodedown_nodeup(1000),
    ?line ok = net_kernel:monitor_nodes(false),
    ?line MonNodeState = monitor_node_state(),
    ?line ok.


monitor_nodes_complex_nodedown_reason(doc) -> [];
monitor_nodes_complex_nodedown_reason(suite) -> [];
monitor_nodes_complex_nodedown_reason(Config) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line Me = self(),
    ?line ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    ?line [Name] = get_nodenames(1, monitor_nodes_complex_nodedown_reason),
    ?line {ok, Node} = start_node(Name, ""),
    ?line Pid = spawn(Node,
		      fun() ->
			      Me ! {stuff,
				    self(),
				    [make_ref(),
				     {processes(), erlang:ports()}]}
		      end),
    ?line receive {nodeup, Node, []} -> ok end,
    ?line {ok, NodeInfo} = net_kernel:node_info(Node),
    ?line {value,{owner, Owner}} = lists:keysearch(owner, 1, NodeInfo),
    ?line ComplexTerm = receive {stuff, Pid, _} = Msg ->
				{Msg, term_to_binary(Msg)}
			end,
    ?line exit(Owner, ComplexTerm),
    ?line receive
	      {nodedown, Node, [{nodedown_reason, NodeDownReason}]} ->
		  ?line ok
	  end,
    %% If the complex nodedown_reason messed something up garbage collections
    %% are likely to dump core
    ?line garbage_collect(),
    ?line garbage_collect(),
    ?line garbage_collect(),
    ?line ComplexTerm = NodeDownReason,
    ?line ok = net_kernel:monitor_nodes(false, [nodedown_reason]),
    ?line no_msgs(),
    ?line MonNodeState = monitor_node_state(),
    ?line ok.
    

    

%%
%% Testcase:
%%   monitor_nodes_node_type
%%

monitor_nodes_node_type(doc) -> [];
monitor_nodes_node_type(suite) -> [];
monitor_nodes_node_type(Config) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line ok = net_kernel:monitor_nodes(true),
    ?line ok = net_kernel:monitor_nodes(true, [{node_type, all}]),
    ?line Names = get_numbered_nodenames(9, node),
%    ?line ?t:format("Names: ~p~n", [Names]),
    ?line [NN1, NN2, NN3, NN4, NN5, NN6, NN7, NN8, NN9] = Names,

    ?line {ok, N1} = start_node(NN1),
    ?line {ok, N2} = start_node(NN2),
    ?line {ok, N3} = start_node(NN3, "-hidden"),
    ?line {ok, N4} = start_node(NN4, "-hidden"),

    ?line receive {nodeup, N1} -> ok end,
    ?line receive {nodeup, N2} -> ok end,
    
    ?line receive {nodeup, N1, [{node_type, visible}]} -> ok end,
    ?line receive {nodeup, N2, [{node_type, visible}]} -> ok end,
    ?line receive {nodeup, N3, [{node_type, hidden}]} -> ok end,
    ?line receive {nodeup, N4, [{node_type, hidden}]} -> ok end,

    ?line stop_node(N1),
    ?line stop_node(N2),
    ?line stop_node(N3),
    ?line stop_node(N4),

    ?line receive {nodedown, N1} -> ok end,
    ?line receive {nodedown, N2} -> ok end,

    ?line receive {nodedown, N1, [{node_type, visible}]} -> ok end,
    ?line receive {nodedown, N2, [{node_type, visible}]} -> ok end,
    ?line receive {nodedown, N3, [{node_type, hidden}]} -> ok end,
    ?line receive {nodedown, N4, [{node_type, hidden}]} -> ok end,

    ?line ok = net_kernel:monitor_nodes(false, [{node_type, all}]),
    ?line {ok, N5} = start_node(NN5),

    ?line receive {nodeup, N5} -> ok end,
    ?line stop_node(N5),
    ?line receive {nodedown, N5} -> ok end,

    ?line ok = net_kernel:monitor_nodes(true, [{node_type, hidden}]),
    ?line {ok, N6} = start_node(NN6),
    ?line {ok, N7} = start_node(NN7, "-hidden"),


    ?line receive {nodeup, N6} -> ok end,
    ?line receive {nodeup, N7, [{node_type, hidden}]} -> ok end,
    ?line stop_node(N6),
    ?line stop_node(N7),

    ?line receive {nodedown, N6} -> ok end,
    ?line receive {nodedown, N7, [{node_type, hidden}]} -> ok end,

    ?line ok = net_kernel:monitor_nodes(true, [{node_type, visible}]),
    ?line ok = net_kernel:monitor_nodes(false, [{node_type, hidden}]),
    ?line ok = net_kernel:monitor_nodes(false),

    ?line {ok, N8} = start_node(NN8),
    ?line {ok, N9} = start_node(NN9, "-hidden"),

    ?line receive {nodeup, N8, [{node_type, visible}]} -> ok end,
    ?line stop_node(N8),
    ?line stop_node(N9),

    ?line receive {nodedown, N8, [{node_type, visible}]} -> ok end,
    ?line print_my_messages(),
    ?line ok = check_no_nodedown_nodeup(1000),
    ?line ok = net_kernel:monitor_nodes(false, [{node_type, visible}]),
    ?line MonNodeState = monitor_node_state(),
    ?line ok.


%%
%% Testcase:
%%   monitor_nodes
%%

monitor_nodes_misc(doc) -> [];
monitor_nodes_misc(suite) -> [];
monitor_nodes_misc(Config) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line ok = net_kernel:monitor_nodes(true),
    ?line ok = net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]),
    ?line ok = net_kernel:monitor_nodes(true, [nodedown_reason, {node_type, all}]),
    ?line Names = get_numbered_nodenames(3, node),
%    ?line ?t:format("Names: ~p~n", [Names]),
    ?line [NN1, NN2, NN3] = Names,

    ?line {ok, N1} = start_node(NN1),
    ?line {ok, N2} = start_node(NN2, "-hidden"),

    ?line receive {nodeup, N1} -> ok end,

    ?line receive {nodeup, N1, [{node_type, visible}]} -> ok end,
    ?line receive {nodeup, N1, [{node_type, visible}]} -> ok end,
    ?line receive {nodeup, N2, [{node_type, hidden}]} -> ok end,
    ?line receive {nodeup, N2, [{node_type, hidden}]} -> ok end,

    ?line stop_node(N1),
    ?line stop_node(N2),

    ?line VisbleDownInfo = lists:sort([{node_type, visible},
				       {nodedown_reason, connection_closed}]),
    ?line HiddenDownInfo = lists:sort([{node_type, hidden},
				       {nodedown_reason, connection_closed}]),

    ?line receive {nodedown, N1} -> ok end,

    ?line receive {nodedown, N1, Info1A} -> VisbleDownInfo = lists:sort(Info1A) end,
    ?line receive {nodedown, N1, Info1B} -> VisbleDownInfo = lists:sort(Info1B) end,
    ?line receive {nodedown, N2, Info2A} -> HiddenDownInfo = lists:sort(Info2A) end,
    ?line receive {nodedown, N2, Info2B} -> HiddenDownInfo = lists:sort(Info2B) end,

    ?line ok = net_kernel:monitor_nodes(false, [{node_type, all}, nodedown_reason]),

    ?line {ok, N3} = start_node(NN3),
    ?line receive {nodeup, N3} -> ok end,
    ?line stop_node(N3),
    ?line receive {nodedown, N3} -> ok end,
    ?line print_my_messages(),
    ?line ok = check_no_nodedown_nodeup(1000),
    ?line ok = net_kernel:monitor_nodes(false),
    ?line MonNodeState = monitor_node_state(),
    ?line ok.


monitor_nodes_otp_6481(doc) ->
    ["Tests that {nodeup, Node} messages are received before "
     "messages from Node and that {nodedown, Node} messages are"
     "received after messages from Node"];
monitor_nodes_otp_6481(suite) ->
    [];
monitor_nodes_otp_6481(Config) when is_list(Config) ->
    ?line ?t:format("Testing nodedown...~n"),
    ?line monitor_nodes_otp_6481_test(Config, nodedown),
    ?line ?t:format("ok~n"),
    ?line ?t:format("Testing nodeup...~n"),
    ?line monitor_nodes_otp_6481_test(Config, nodeup),
    ?line ?t:format("ok~n"),
    ?line ok.

monitor_nodes_otp_6481_test(Config, TestType) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line NodeMsg = make_ref(),
    ?line Me = self(),
    ?line [Name] = get_nodenames(1, monitor_nodes_otp_6481),
    ?line case TestType of
	      nodedown -> ?line ok = net_kernel:monitor_nodes(true);
	      nodeup -> ?line ok
	  end,
    ?line Seq = lists:seq(1,10000),
    ?line MN = spawn_link(
		 fun () ->
			 ?line lists:foreach(
				 fun (_) ->
					 ?line ok = net_kernel:monitor_nodes(true)
				 end,
				 Seq),
			 ?line Me ! {mon_set, self()},
			 ?line receive after infinity -> ok end
		 end),
    ?line receive {mon_set, MN} -> ok end,
    ?line case TestType of
	      nodedown -> ?line ok;
	      nodeup -> ?line ok = net_kernel:monitor_nodes(true)
	  end,

    %% Whitebox:
    %% nodedown test: Since this process was the first one monitoring
    %%                nodes this process will be the first one notified
    %%                on nodedown.
    %% nodeup test:   Since this process was the last one monitoring
    %%                nodes this process will be the last one notified
    %%                on nodeup

    %% Verify the monitor_nodes order expected
    ?line TestMonNodeState = monitor_node_state(),
    %?line ?t:format("~p~n", [TestMonNodeState]),
    ?line TestMonNodeState = 
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


    ?line {ok, Node} = start_node(Name, "", this),
    ?line receive {nodeup, Node} -> ok end,

    ?line RemotePid = spawn(Node,
		fun () ->
			receive after 1500 -> ok end,
			% infinit loop of msgs
			% we want an endless stream of messages and the kill
			% the node mercilessly.
			% We then want to ensure that the nodedown message arrives
			% last ... without garbage after it.
			_ = spawn(fun() -> node_loop_send(Me, NodeMsg, 1) end),
			receive {Me, kill_it} -> ok end, 
			halt()
		end),

    ?line net_kernel:disconnect(Node),
    ?line receive {nodedown, Node} -> ok end,

    %% Verify that '{nodeup, Node}' comes before '{NodeMsg, 1}' (the message
    %% bringing up the connection).
    ?line no_msgs(500),
    ?line {nodeup, Node} = receive Msg1 -> Msg1 end,
    ?line {NodeMsg, 1}   = receive Msg2 -> Msg2 end,
    % msg stream has begun, kill the node
    ?line RemotePid ! {self(), kill_it},

    %% Verify that '{nodedown, Node}' comes after the last '{NodeMsg, N}'
    %% message.
    ?line {nodedown, Node} = flush_node_msgs(NodeMsg, 2),
    ?line no_msgs(500),

    ?line Mon = erlang:monitor(process, MN),
    ?line unlink(MN),
    ?line exit(MN, bang),
    ?line receive {'DOWN', Mon, process, MN, bang} -> ok end,
    ?line ok = net_kernel:monitor_nodes(false),
    ?line MonNodeState = monitor_node_state(),
    ?line ok.

flush_node_msgs(NodeMsg, No) ->
    case receive Msg -> Msg end of
	{NodeMsg, No} -> flush_node_msgs(NodeMsg, No+1);
	OtherMsg -> OtherMsg
    end.

node_loop_send(Pid, Msg, No) ->
    Pid ! {Msg, No},
    node_loop_send(Pid, Msg, No + 1).

monitor_nodes_errors(doc) ->
    [];
monitor_nodes_errors(suite) ->
    [];
monitor_nodes_errors(Config) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line error = net_kernel:monitor_nodes(asdf),
    ?line {error,
	   {unknown_options,
	    [gurka]}} = net_kernel:monitor_nodes(true,
						 [gurka]),
    ?line {error,
	   {options_not_a_list,
	    gurka}} = net_kernel:monitor_nodes(true,
					       gurka),
    ?line {error,
	   {option_value_mismatch,
	    [{node_type,visible},
	     {node_type,hidden}]}}
	= net_kernel:monitor_nodes(true,
				   [{node_type,hidden},
				    {node_type,visible}]),
    ?line {error,
	   {option_value_mismatch,
	    [{node_type,visible},
	     {node_type,all}]}}
	= net_kernel:monitor_nodes(true,
				   [{node_type,all},
				    {node_type,visible}]),
    ?line {error,
	   {bad_option_value,
	    {node_type,
	     blaha}}}
	= net_kernel:monitor_nodes(true, [{node_type, blaha}]),
    ?line MonNodeState = monitor_node_state(),
    ?line ok.

monitor_nodes_combinations(doc) ->
    [];
monitor_nodes_combinations(suite) ->
    [];
monitor_nodes_combinations(Config) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line monitor_nodes_all_comb(true),
    ?line [VisibleName, HiddenName] = get_nodenames(2,
						    monitor_nodes_combinations),
    ?line {ok, Visible} = start_node(VisibleName, ""),
    ?line receive_all_comb_nodeup_msgs(visible, Visible),
    ?line no_msgs(),
    ?line stop_node(Visible),
    ?line receive_all_comb_nodedown_msgs(visible, Visible, connection_closed),
    ?line no_msgs(),
    ?line {ok, Hidden} = start_node(HiddenName, "-hidden"),
    ?line receive_all_comb_nodeup_msgs(hidden, Hidden),
    ?line no_msgs(),
    ?line stop_node(Hidden),
    ?line receive_all_comb_nodedown_msgs(hidden, Hidden, connection_closed),
    ?line no_msgs(),
    ?line monitor_nodes_all_comb(false),
    ?line MonNodeState = monitor_node_state(),
    ?line no_msgs(),
    ?line ok.

monitor_nodes_all_comb(Flag) ->
    ?line ok = net_kernel:monitor_nodes(Flag),
    ?line ok = net_kernel:monitor_nodes(Flag,
					[nodedown_reason]),
    ?line ok = net_kernel:monitor_nodes(Flag,
					[{node_type, hidden}]),
    ?line ok = net_kernel:monitor_nodes(Flag,
					[{node_type, visible}]),
    ?line ok = net_kernel:monitor_nodes(Flag,
					[{node_type, all}]),
    ?line ok = net_kernel:monitor_nodes(Flag,
					[nodedown_reason,
					 {node_type, hidden}]),
    ?line ok = net_kernel:monitor_nodes(Flag,
					[nodedown_reason,
					 {node_type, visible}]),
    ?line ok = net_kernel:monitor_nodes(Flag,
					[nodedown_reason,
					 {node_type, all}]),
    %% There currently are 8 different combinations
    ?line 8.


receive_all_comb_nodeup_msgs(visible, Node) ->
    ?t:format("Receive nodeup visible...~n"),
    Exp = [{nodeup, Node},
	   {nodeup, Node, []}]
	++ mk_exp_mn_all_comb_nodeup_msgs_common(visible, Node),
    receive_mn_msgs(Exp),
    ?t:format("ok~n"),
    ok;
receive_all_comb_nodeup_msgs(hidden, Node) ->
    ?t:format("Receive nodeup hidden...~n"),
    Exp = mk_exp_mn_all_comb_nodeup_msgs_common(hidden, Node),
    receive_mn_msgs(Exp),
    ?t:format("ok~n"),
    ok.

mk_exp_mn_all_comb_nodeup_msgs_common(Type, Node) ->
    InfoNt = [{node_type, Type}],
    [{nodeup, Node, InfoNt},
     {nodeup, Node, InfoNt},
     {nodeup, Node, InfoNt},
     {nodeup, Node, InfoNt}].

receive_all_comb_nodedown_msgs(visible, Node, Reason) ->
    ?t:format("Receive nodedown visible...~n"),
    Exp = [{nodedown, Node},
	   {nodedown, Node, [{nodedown_reason, Reason}]}]
	++ mk_exp_mn_all_comb_nodedown_msgs_common(visible,
						   Node,
						   Reason),
    receive_mn_msgs(Exp),
    ?t:format("ok~n"),
    ok;
receive_all_comb_nodedown_msgs(hidden, Node, Reason) ->
    ?t:format("Receive nodedown hidden...~n"),
    Exp = mk_exp_mn_all_comb_nodedown_msgs_common(hidden, Node, Reason),
    receive_mn_msgs(Exp),
    ?t:format("ok~n"),
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
    ?t:format("Expecting msgs: ~p~n", [Msgs]),
    receive
	{_Dir, _Node} = Msg ->
	    ?t:format("received ~p~n", [Msg]),
	    case lists:member(Msg, Msgs) of
		true -> receive_mn_msgs(lists:delete(Msg, Msgs));
		false -> ?t:fail({unexpected_message, Msg,
				  expected_messages, Msgs})
	    end;
	{Dir, Node, Info} ->
	    Msg = {Dir, Node, lists:sort(Info)},
	    ?t:format("received ~p~n", [Msg]),
	    case lists:member(Msg, Msgs) of
		true -> receive_mn_msgs(lists:delete(Msg, Msgs));
		false -> ?t:fail({unexpected_message, Msg,
				  expected_messages, Msgs})
	    end;
	Msg ->
	    ?t:format("received ~p~n", [Msg]),
	    ?t:fail({unexpected_message, Msg,
		     expected_messages, Msgs})
    end.

monitor_nodes_cleanup(doc) ->
    [];
monitor_nodes_cleanup(suite) ->
    [];
monitor_nodes_cleanup(Config) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line Me = self(),
    ?line No = monitor_nodes_all_comb(true),
    ?line Inf = spawn(fun () ->
			      monitor_nodes_all_comb(true),
			      Me ! {mons_set, self()},
			      receive after infinity -> ok end
		      end),
    ?line TO = spawn(fun () ->
			     monitor_nodes_all_comb(true),
			     Me ! {mons_set, self()},
			     receive after 500 -> ok end
		     end),
    ?line receive {mons_set, Inf} -> ok end,
    ?line receive {mons_set, TO} -> ok end,
    ?line MNLen = length(MonNodeState) + No*3,
    ?line MNLen = length(monitor_node_state()),
    ?line MonInf = erlang:monitor(process, Inf),
    ?line MonTO = erlang:monitor(process, TO),
    ?line exit(Inf, bang),
    ?line No = monitor_nodes_all_comb(false),
    ?line receive {'DOWN', MonInf, process, Inf, bang} -> ok end,
    ?line receive {'DOWN', MonTO, process, TO, normal} -> ok end,
    ?line MonNodeState = monitor_node_state(),
    ?line no_msgs(),
    ?line ok.

monitor_nodes_many(doc) ->
    [];
monitor_nodes_many(suite) ->
    [];
monitor_nodes_many(Config) when is_list(Config) ->
    ?line MonNodeState = monitor_node_state(),
    ?line [Name] = get_nodenames(1, monitor_nodes_many),
    %% We want to perform more than 2^16 net_kernel:monitor_nodes
    %% since this will wrap an internal counter
    ?line No = (1 bsl 16) + 17,
    ?line repeat(fun () -> ok = net_kernel:monitor_nodes(true) end, No),
    ?line No = length(monitor_node_state()) - length(MonNodeState),
    ?line {ok, Node} = start_node(Name),
    ?line repeat(fun () -> receive {nodeup, Node} -> ok end end, No),
    ?line stop_node(Node),
    ?line repeat(fun () -> receive {nodedown, Node} -> ok end end, No),
    ?line ok = net_kernel:monitor_nodes(false),
    ?line no_msgs(10),
    ?line MonNodeState = monitor_node_state(),
    ?line ok.

%% Misc. functions

monitor_node_state() ->
    erts_debug:set_internal_state(available_internal_state, true),
    MonitoringNodes = erts_debug:get_internal_state(monitoring_nodes),
    erts_debug:set_internal_state(available_internal_state, false),
    MonitoringNodes.


check_no_nodedown_nodeup(TimeOut) ->
    ?line receive
	      {nodeup, _, _} = Msg -> ?line ?t:fail({unexpected_nodeup, Msg});
	      {nodeup, _} = Msg -> ?line ?t:fail({unexpected_nodeup, Msg});
	      {nodedown, _, _} = Msg -> ?line ?t:fail({unexpected_nodedown, Msg});
	      {nodedown, _} = Msg -> ?line ?t:fail({unexpected_nodedown, Msg})
	  after TimeOut ->
		  ok
	  end.

print_my_messages() ->
    ?line {messages, Messages} = process_info(self(), messages),
    ?line ?t:format("Messages: ~p~n", [Messages]),
    ?line ok.


sleep(T) -> receive after T * 1000 -> ok end.	

start_node(Name, Param, this) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    ?t:start_node(Name, peer, [{args, NewParam}, {erl, [this]}]);
start_node(Name, Param, "this") ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    ?t:start_node(Name, peer, [{args, NewParam}, {erl, [this]}]);
start_node(Name, Param, Rel) when is_atom(Rel) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    ?t:start_node(Name, peer, [{args, NewParam}, {erl, [{release, atom_to_list(Rel)}]}]);
start_node(Name, Param, Rel) when is_list(Rel) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    ?t:start_node(Name, peer, [{args, NewParam}, {erl, [{release, Rel}]}]).

start_node(Name, Param) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    ?t:start_node(Name, slave, [{args, NewParam}]).
%    M = list_to_atom(from($@, atom_to_list(node()))),
%    slave:start_link(M, Name, Param).

start_node(Name) ->
    start_node(Name, "").

stop_node(Node) ->
    ?t:stop_node(Node).
%    erlang:monitor_node(Node, true),
%    rpc:cast(Node, init, stop, []),
%    receive
%	{nodedown, Node} ->
%	    ok
%    after 10000 ->
%	    test_server:fail({stop_node, Node})
%    end.

% from(H, [H | T]) -> T;
% from(H, [_ | T]) -> from(H, T);
% from(H, []) -> [].

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
