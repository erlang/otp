%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2022. All Rights Reserved.
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
-include_lib("kernel/include/dist.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([tick/1, tick_intensity/1, tick_change/1,
         connect_node/1,
         nodenames/1, hostnames/1,
         illegal_nodenames/1, hidden_node/1,
         dyn_node_name/1,
         epmd_reconnect/1,
	 setopts/1,
	 table_waste/1, net_setuptime/1,
	 inet_dist_options_options/1,
         net_ticker_spawn_options/1,

	 monitor_nodes_nodedown_reason/1,
	 monitor_nodes_complex_nodedown_reason/1,
	 monitor_nodes_node_type/1,
	 monitor_nodes_misc/1,
	 monitor_nodes_otp_6481/1,
	 monitor_nodes_errors/1,
	 monitor_nodes_combinations/1,
	 monitor_nodes_cleanup/1,
	 monitor_nodes_many/1,
         monitor_nodes_down_up/1,
         dist_ctrl_proc_smoke/1,
         dist_ctrl_proc_reject/1,
         erl_uds_dist_smoke_test/1,
         erl_1424/1, net_kernel_start/1, differing_cookies/1,
         cmdline_setcookie_2/1, connection_cookie/1,
         dyn_differing_cookies/1,
         xdg_cookie/1]).

%% Performs the test at another node.
-export([get_socket_priorities/0,
         get_net_ticker_fullsweep_option/1,
	 tick_cli_test/3, tick_cli_test1/3,
	 tick_serv_test/2, tick_serv_test1/1,
	 run_remote_test/1,
         dyn_node_name_do/2,
         epmd_reconnect_do/2,
	 setopts_do/2,
         setopts_deadlock_test/2,
	 keep_conn/1, time_ping/1,
         ddc_remote_run/2]).

-export([net_kernel_start_do_test/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([dist_cntrlr_output_test_size/2]).

-export([pinger/1]).

-export([start_uds_rpc_server/1]).

-define(DUMMY_NODE,dummy@test01).
-define(ALT_EPMD_PORT, "12321").
-define(ALT_EPMD_CMD, "epmd -port "++?ALT_EPMD_PORT).

%%-----------------------------------------------------------------
%% The distribution is mainly tested in the big old test_suite.
%% This test only tests the net_ticktime configuration flag.
%% Should be started in a CC view with:
%% erl -sname master -rsh ctrsh
%%-----------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,12}}].

all() -> 
    [dist_ctrl_proc_smoke,
     dist_ctrl_proc_reject,
     tick, tick_intensity, tick_change, nodenames, hostnames,
     illegal_nodenames, connect_node,
     dyn_node_name,
     epmd_reconnect,
     hidden_node, setopts,
     table_waste, net_setuptime, inet_dist_options_options,
     net_ticker_spawn_options,
     {group, monitor_nodes},
     erl_uds_dist_smoke_test,
     erl_1424, net_kernel_start,
     {group, differing_cookies}].

groups() -> 
    [{monitor_nodes, [],
      [monitor_nodes_nodedown_reason,
       monitor_nodes_complex_nodedown_reason,
       monitor_nodes_node_type, monitor_nodes_misc,
       monitor_nodes_otp_6481, monitor_nodes_errors,
       monitor_nodes_combinations, monitor_nodes_cleanup,
       monitor_nodes_many,
       monitor_nodes_down_up]},
     {differing_cookies, [],
      [differing_cookies,
       cmdline_setcookie_2,
       connection_cookie,
       dyn_differing_cookies,
       xdg_cookie]}].

init_per_suite(Config) ->
    start_gen_tcp_dist_test_type_server(),
    Config.

end_per_suite(_Config) ->
    [slave:stop(N) || N <- nodes()],
    kill_gen_tcp_dist_test_type_server(),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TC, Config) when TC == hostnames;
                                   TC == nodenames ->
    file:make_dir("hostnames_nodedir"),
    file:write_file("hostnames_nodedir/ignore_core_files",""),
    Config;
init_per_testcase(epmd_reconnect, Config) ->
    [] = os:cmd(?ALT_EPMD_CMD++" -relaxed_command_check -daemon"),
    Config;
init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Config.

end_per_testcase(epmd_reconnect, _Config) ->
    os:cmd(?ALT_EPMD_CMD++" -kill"),
    ok;
end_per_testcase(_Func, _Config) ->
    ok.

connect_node(Config) when is_list(Config) ->
    Connected = nodes(connected),
    true = net_kernel:connect_node(node()),
    Connected = nodes(connected),
    ok.

tick(Config) when is_list(Config) ->
    run_dist_configs(fun tick/2, Config).

tick(DCfg, Config) ->
    tick_test(DCfg, Config, false).

tick_intensity(Config) when is_list(Config) ->
    run_dist_configs(fun tick_intensity/2, Config).

tick_intensity(DCfg, Config) ->
    tick_test(DCfg, Config, true).

tick_test(DCfg, _Config, CheckIntensityArg) ->
    %%
    %% This test case use disabled "connect all" so that
    %% global wont interfere...
    %%

    [Name1, Name2] = get_nodenames(2, dist_test),

    {ok, Node} = start_node(DCfg, Name1),

    case CheckIntensityArg of
        true ->
            %% Not for intensity test...
            ok;
        false ->
            %% First check that the normal case is OK!
            rpc:call(Node, erl_distribution_SUITE, tick_cli_test, [node(), 8000, 16000]),

            erlang:monitor_node(Node, true),
            receive
                {nodedown, Node} ->
                    ct:fail("nodedown from other node")
            after 30000 ->
                    erlang:monitor_node(Node, false)
            end,
            ok
    end,

    stop_node(Node),

    %% Now, set the net_ticktime for the other node to 12 secs.
    %% After the sleep(2sec) and cast the other node shall destroy
    %% the connection as it has not received anything on the connection.
    %% The nodedown message should arrive within 8 < T < 16 secs.

    %% We must have two slave nodes as the slave mechanism otherwise
    %% halts the client node after tick timeout (the connection is down
    %% and the slave node decides to halt !!

    %% Set the ticktime on the server node to 100 secs so the server
    %% node doesn't tick the client node within the interval ...

    {ok, ServNode} = start_node(DCfg, Name2,
				"-kernel net_ticktime 100 -connect_all false"),
    rpc:call(ServNode, erl_distribution_SUITE, tick_serv_test, [Node, node()]),

    %% We set min/max a second lower/higher than expected since it takes
    %% time for termination of the dist controller, delivery of messages,
    %% scheduling of the process receiving nodedown, etc...
    {IArg, Min, Max} = case CheckIntensityArg of
                           false ->
                               {"", 7000, 17000};
                           true ->
                               {" -kernel net_tickintensity 24", 10500, 13500}
                       end,
    
    {ok, Node} = start_node(DCfg, Name1,
                            "-kernel net_ticktime 12 -connect_all false" ++ IArg),

    rpc:call(Node, erl_distribution_SUITE, tick_cli_test, [ServNode, Min, Max]),

    spawn_link(erl_distribution_SUITE, keep_conn, [Node]),

    {tick_serv, ServNode} ! {i_want_the_result, self()},

    monitor_node(ServNode, true),
    monitor_node(Node, true),

    receive
	{tick_test, T} when is_integer(T) ->
	    stop_node(ServNode),
	    stop_node(Node),
            io:format("Result: ~p~n", [T]),
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
    run_dist_configs(fun table_waste/2, Config).

table_waste(DCfg, _Config) ->
    {ok, HName} = inet:gethostname(),
    F = fun(0,_F) -> [];
	   (N,F) -> 
		Name = list_to_atom("erl_distribution_"++integer_to_list(N)++
					"@"++HName),
		pang = net_adm:ping(Name),
		F(N-1,F)
	end,
    F(256,F),
    {ok, N} = start_node(DCfg, erl_distribution_300),
    stop_node(N),
    ok.

%% Test that starting nodes with different legal name part works, and that illegal
%% ones are filtered
nodenames(Config) when is_list(Config) ->
    legal("a1@b"),
    legal("a-1@b"),
    legal("a_1@b"),

    %% Test that giving two -sname works as it should
    started = test_node("a_1@b", false, long_or_short() ++ "a_0@b"),

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
    case test_node(Name, true) of
        not_started ->
            ok;
        started ->
            ct:fail("~p node started with illegal name", [Name])
    end.

test_node(Name) ->
    test_node(Name, false).
test_node(Name, Illegal) ->
    test_node(Name, Illegal, "").
test_node(Name, Illegal, ExtraArgs) ->
    ProgName = ct:get_progname(),
    Command = ProgName ++ " -noinput " ++
        long_or_short() ++ Name ++ ExtraArgs ++
        " -eval \"net_adm:ping('" ++ atom_to_list(node()) ++ "')\"" ++
        case Illegal of
            true ->
                " -eval \"timer:sleep(10000),init:stop().\"";
            false ->
                ""
        end,
    net_kernel:monitor_nodes(true),
    BinCommand = unicode:characters_to_binary(Command, utf8),
    _Prt = open_port({spawn, BinCommand}, [stream,{cd,"hostnames_nodedir"}]),
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
    run_dist_configs(fun illegal_nodenames/2, Config).

illegal_nodenames(DCfg, _Config) ->
    {ok, Node}=start_node(DCfg, illegal_nodenames),
    monitor_node(Node, true),
    RPid=rpc:call(Node, erlang, spawn,
		  [?MODULE, pinger, [self()]]),
    receive
	{RPid, pinged} ->
            monitor_node(Node, false),
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
    run_dist_configs(fun net_setuptime/2, Config).

net_setuptime(DCfg, _Config) ->

    %% In this test case, we reluctantly accept shorter times than the given
    %% setup time, because the connection attempt can end in a
    %% "Host unreachable" error before the timeout fires.

    Res0 = do_test_setuptime(DCfg, "2"),
    io:format("Res0 = ~p", [Res0]),
    true = (Res0 =< 4000),
    Res1 = do_test_setuptime(DCfg, "0.3"),
    io:format("Res1 = ~p", [Res1]),
    true = (Res1 =< 500),
    ok.

do_test_setuptime(DCfg, Setuptime) when is_list(Setuptime) ->
    {ok, Node} = start_node(DCfg, dist_setuptime_test,
                            "-kernel net_setuptime " ++ Setuptime),
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

tick_cli_test(Node, Min, Max) ->
    spawn(erl_distribution_SUITE, tick_cli_test1, [Node, Min, Max]).

tick_cli_test1(Node, Min, Max) ->
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
			T when Min =< T, T =< Max ->
			    From ! {tick_test, T};
			T ->
			    From ! {tick_test,
				    {"T not in interval "
                                     ++ integer_to_list(Min)
                                     ++ " =< T =< "
                                     ++ integer_to_list(Max),
				     T}}
		    end
	    end
    end.

epmd_reconnect(Config) when is_list(Config) ->
    NodeNames = [N1,N2,N3] = get_nodenames(3, ?FUNCTION_NAME),
    Nodes = [atom_to_list(full_node_name(NN)) || NN <- NodeNames],

    DCfg = "-epmd_port "++?ALT_EPMD_PORT,

    {_N1F,Port1} = start_node_unconnected(DCfg, N1, ?MODULE, run_remote_test,
					["epmd_reconnect_do", atom_to_list(node()), "1" | Nodes]),
    {_N2F,Port2} = start_node_unconnected(DCfg, N2, ?MODULE, run_remote_test,
					["epmd_reconnect_do", atom_to_list(node()), "2" | Nodes]),
    {_N3F,Port3} = start_node_unconnected(DCfg, N3, ?MODULE, run_remote_test,
					["epmd_reconnect_do", atom_to_list(node()), "3" | Nodes]),
    Ports = [Port1, Port2, Port3],

    ok = reap_ports(Ports),
   
    ok.

reap_ports([]) ->
    ok;
reap_ports(Ports) ->
    case (receive M -> M end) of
	{Port, Message} ->
            case lists:member(Port, Ports) andalso Message of
                {data,String} ->
                    io:format("~p: ~s\n", [Port, String]),
                    reap_ports(Ports);
                {exit_status,0} ->
                    reap_ports(Ports -- [Port])
            end
    end.
    
epmd_reconnect_do(_Node, ["1", Node1, Node2, Node3]) ->
    Names = [Name || Name <- [hd(string:tokens(Node, "@")) || Node <- [Node1, Node2, Node3]]],
    %% wait until all nodes are registered
    ok = wait_for_names(Names),
    "Killed" ++_ = os:cmd(?ALT_EPMD_CMD++" -kill"),
    open_port({spawn, ?ALT_EPMD_CMD}, []),
    %% check that all nodes reregister with epmd
    ok = wait_for_names(Names),
    lists:foreach(fun(Node) ->
                          ANode = list_to_atom(Node),
                          pong = net_adm:ping(ANode),
                          {epmd_reconnect_do, ANode} ! {stop, Node1, Node}
                  end, [Node2, Node3]),
    ok;
epmd_reconnect_do(_Node, ["2", Node1, Node2, _Node3]) ->
    register(epmd_reconnect_do, self()),
    receive {stop, Node1, Node2} ->
            ok
    after 7000 ->
            exit(timeout)
    end;
epmd_reconnect_do(_Node, ["3", Node1, _Node2, Node3]) ->
    register(epmd_reconnect_do, self()),
    receive {stop, Node1, Node3} ->
            ok
    after 7000 ->
            exit(timeout)
    end.

wait_for_names(Names) ->
    %% wait for up to 3 seconds (the current retry timer in erl_epmd is 2s)
    wait_for_names(lists:sort(Names), 30, 100).

wait_for_names(Names, N, Wait) when N > 0 ->
    try
        {ok, Info} = erl_epmd:names(),
        Names = lists:sort([Name || {Name, _Port} <- Info]),
        ok
    catch
        error:{badmatch, _} ->
            timer:sleep(Wait),
            wait_for_names(Names, N-1, Wait)
    end.


dyn_node_name(Config) when is_list(Config) ->
    run_dist_configs(fun dyn_node_name/2, Config).

dyn_node_name(DCfg, _Config) ->
    NameDomain = case net_kernel:get_state() of
                     #{name_domain := shortnames} -> "shortnames";
                     #{name_domain := longnames} -> "longnames"
                 end,
    {_N1F,Port1} = start_node_unconnected(DCfg,
                                          undefined, ?MODULE, run_remote_test,
                                          ["dyn_node_name_do", atom_to_list(node()),
                                           NameDomain]),
    0 = wait_for_port_exit(Port1),
    ok.

dyn_node_name_do(TestNode, [NameDomainStr]) ->
    nonode@nohost = node(),
    [] = nodes(),
    [] = nodes(hidden),
    NameDomain = list_to_atom(NameDomainStr),
    #{started := static, name_type := dynamic, name := undefined,
     name_domain := NameDomain} = net_kernel:get_state(),
    net_kernel:monitor_nodes(true, [{node_type,all}]),
    net_kernel:connect_node(TestNode),
    [] = nodes(),
    [TestNode] = nodes(hidden),
    MyName = node(),
    false = (MyName =:= undefined),
    false = (MyName =:= nonode@nohost),
    #{started := static, name_type := dynamic, name := MyName,
      name_domain := NameDomain} = net_kernel:get_state(),
    check([MyName], rpc:call(TestNode, erlang, nodes, [hidden])),

    {nodeup, MyName, [{node_type, visible}]} = receive_any(),
    {nodeup, TestNode, [{node_type, hidden}]} = receive_any(),

    true = net_kernel:disconnect(TestNode),

    %% We don't know the order of these nodedown messages. Often
    %% nodedown from the connection comes first, but not always...

    NodedownMsgsA = lists:sort([{nodedown, TestNode, [{node_type, hidden}]},
                                {nodedown, MyName, [{node_type, visible}]}]),
    NodedownMsgA1 = receive_any(),
    NodedownMsgA2 = receive_any(),
    NodedownMsgsA = lists:sort([NodedownMsgA1, NodedownMsgA2]),

    [] = nodes(hidden),
    nonode@nohost = node(),
    #{started := static, name_type := dynamic, name := undefined,
      name_domain := NameDomain} = net_kernel:get_state(),

    net_kernel:connect_node(TestNode),
    [] = nodes(),
    [TestNode] = nodes(hidden),
    MyName = node(),
    #{started := static, name_type := dynamic, name := MyName,
      name_domain := NameDomain} = net_kernel:get_state(),

    check([MyName], rpc:call(TestNode, erlang, nodes, [hidden])),

    {nodeup, MyName, [{node_type, visible}]} = receive_any(),
    {nodeup, TestNode, [{node_type, hidden}]} = receive_any(),

    true = rpc:cast(TestNode, net_kernel, disconnect, [MyName]),

    %% We don't know the order of these nodedown messages. Often
    %% nodedown from the connection comes first, but not always...

    NodedownMsgsB = lists:sort([{nodedown, TestNode, [{node_type, hidden}]},
                                {nodedown, MyName, [{node_type, visible}]}]),
    NodedownMsgB1 = receive_any(),
    NodedownMsgB2 = receive_any(),
    NodedownMsgsB = lists:sort([NodedownMsgB1, NodedownMsgB2]),

    [] = nodes(hidden),
    nonode@nohost = node(),
    #{started := static, name_type := dynamic, name := undefined,
      name_domain := NameDomain} = net_kernel:get_state(),
    ok.

check(X, X) -> ok.

setopts(Config) when is_list(Config) ->
    run_dist_configs(fun setopts/2, Config).

setopts(DCfg, _Config) ->
    register(setopts_regname, self()),
    [N1,N2,N3,N4,N5] = get_nodenames(5, setopts),

    {_N1F,Port1} = start_node_unconnected(DCfg, N1, ?MODULE, run_remote_test,
					["setopts_do", atom_to_list(node()), "1", "ping"]),
    0 = wait_for_port_exit(Port1),

    {_N2F,Port2} = start_node_unconnected(DCfg, N2, ?MODULE, run_remote_test,
				 ["setopts_do", atom_to_list(node()), "2", "ping"]),
    0 = wait_for_port_exit(Port2),

    {ok, LSock} = gen_tcp:listen(0, [{packet,2}, {active,false}]),
    {ok, LTcpPort} = inet:port(LSock),

    {N3F,Port3} = start_node_unconnected(DCfg, N3, ?MODULE, run_remote_test,
					["setopts_do", atom_to_list(node()),
					 "1", integer_to_list(LTcpPort)]),
    wait_and_connect(LSock, N3F, Port3),
    0 = wait_for_port_exit(Port3),

    {N4F,Port4} = start_node_unconnected(DCfg, N4, ?MODULE, run_remote_test,
					["setopts_do", atom_to_list(node()),
					 "2", integer_to_list(LTcpPort)]),
    wait_and_connect(LSock, N4F, Port4),
    0 = wait_for_port_exit(Port4),

    %% net_kernel:setopts(new, _) used to be able to produce a deadlock
    %% in net_kernel. GH-6129/OTP-18198
    {N5F,Port5} = start_node_unconnected(DCfg, N5, ?MODULE, run_remote_test,
					["setopts_deadlock_test", atom_to_list(node()),
					 integer_to_list(LTcpPort)]),
    wait_and_connect(LSock, N5F, Port5),
    repeat(fun () ->
                   receive after 10 -> ok end,
                   erlang:disconnect_node(N5F),
                   WD = spawn_link(fun () ->
                                           receive after 2000 -> ok end,
                                           exit({net_kernel_probably_deadlocked, N5F})
                                   end),
                   pong = net_adm:ping(N5F),
                   unlink(WD),
                   exit(WD, kill),
                   false = is_process_alive(WD)
           end,
           200),
    try
        erpc:call(N5F, erlang, halt, [])
    catch
        error:{erpc,noconnection} -> ok
    end,
    0 = wait_for_port_exit(Port5),

    unregister(setopts_regname),
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
	C:E:S ->
	    io:format("Node ~p got EXCEPTION ~p:~p\nat ~p\n",
		      [node(), C, E, S]),
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

setopts_deadlock_test(_TestNode, [TcpPort]) ->
    {ok, Sock} = gen_tcp:connect("localhost", list_to_integer(TcpPort),
                                 [{active,false},{packet,2}]),
    ok = gen_tcp:send(Sock, "Connect please"),
    {ok, "Connect done"} = gen_tcp:recv(Sock, 0),
    gen_tcp:close(Sock),
    setopts_new_loop().
    
setopts_new_loop() ->
    ok = net_kernel:setopts(new, [{nodelay, true}]),
    receive after 10 -> ok end,
    setopts_new_loop().
    
opt_from_nr("1") -> {nodelay, true};
opt_from_nr("2") -> {nodelay, false}.

change_val(true)  -> false;
change_val(false) -> true.


start_node_unconnected(DCfg, Name, Mod, Func, Args) ->
    start_node_unconnected(DCfg, Name, erlang:get_cookie(), Mod, Func, Args).

start_node_unconnected(DCfg, Name, Cookie, Mod, Func, Args) ->
    FullName = full_node_name(Name),
    CmdLine =
        mk_node_cmdline(DCfg, Name, Cookie, Mod, Func, Args),
    io:format("Starting node ~p: ~s~n", [FullName, CmdLine]),
    case open_port({spawn, CmdLine}, [exit_status]) of
	Port when is_port(Port) ->
	    {FullName, Port};
	Error ->
	    exit({failed_to_start_node, FullName, Error})
    end.

full_node_name(PreName) when is_atom(PreName) ->
    full_node_name(atom_to_list(PreName));
full_node_name(PreNameL) when is_list(PreNameL) ->
    HostSuffix = lists:dropwhile(fun ($@) -> false; (_) -> true end,
				 atom_to_list(node())),
    list_to_atom(PreNameL ++ HostSuffix).

mk_node_cmdline(DCfg, Name, Cookie, Mod, Func, Args) ->
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
        ++ " " ++ DCfg
	++ " -pa " ++ Pa
	++ " -env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ NameStr
	++ " -setcookie " ++ atom_to_list(Cookie)
	++ " -run " ++ atom_to_list(Mod) ++ " " ++ atom_to_list(Func)
	++ " " ++ string:join(Args, " ").


%% OTP-4255.
tick_change(Config) when is_list(Config) ->
    run_dist_configs(fun tick_change/2, Config).

tick_change(DCfg, _Config) ->
    %%
    %% This test case use disabled "connect all" so that
    %% global wont interfere...
    %%
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
    {ok, B} = start_node(DCfg, BN, "-kernel net_ticktime 10 -connect_all false"),
    {ok, C} = start_node(DCfg, CN, "-kernel net_ticktime 10 -hidden"),

    OTE = process_flag(trap_exit, true),
    case catch begin
		   run_tick_change_test(DCfg, B, C, 10, 1),
		   run_tick_change_test(DCfg, B, C, 1, 10)
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

run_tick_change_test(DCfg, B, C, PrevTT, TT) ->
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

    {ok, D} = start_node(DCfg, DN, "-connect_all false -kernel net_ticktime "
			 ++ integer_to_list(PrevTT)),

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

    {ok, E} = start_node(DCfg, EN, "-connect_all false -kernel net_ticktime "
			 ++ integer_to_list(TT)),
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
    run_dist_configs(fun hidden_node/2, Config).

hidden_node(DCfg, Config) ->
    hidden_node(DCfg, "-hidden", Config),
    hidden_node(DCfg, "-hidden -hidden", Config),
    hidden_node(DCfg, "-hidden true -hidden true", Config),
    ok.

hidden_node(DCfg, HArgs, _Config) ->
    ct:pal("--- Hidden argument(s): ~s~n", [HArgs]),
    {ok, V} = start_node(DCfg, visible_node),
    VMN = start_monitor_nodes_proc(V),
    {ok, H} = start_node(DCfg, hidden_node, HArgs),
    %% Connect visible_node -> hidden_node
    connect_nodes(V, H),
    test_nodes(V, H),
    stop_node(H),
    sleep(5),
    check_monitor_nodes_res(VMN, H),
    stop_node(V),
    {ok, H} = start_node(DCfg, hidden_node, HArgs),
    HMN = start_monitor_nodes_proc(H),
    {ok, V} = start_node(DCfg, visible_node),
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
	start_node("", inet_dist_options_1, InetDistOptions),
    {ok,Node2} =
	start_node("", inet_dist_options_2, InetDistOptions),
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



%% check net_ticker_spawn_options
net_ticker_spawn_options(Config) when is_list(Config) ->
    run_dist_configs(fun net_ticker_spawn_options/2, Config).

net_ticker_spawn_options(DCfg, Config) when is_list(Config) ->
    FullsweepString0 = "[{fullsweep_after,0}]",
    FullsweepString =
        case os:cmd("echo [{a,1}]") of
            "[{a,1}]"++_ ->
                FullsweepString0;
            _ ->
                %% Some shells need quoting of [{}]
                "'"++FullsweepString0++"'"
        end,
    InetDistOptions =
        "-hidden "
        "-kernel net_ticker_spawn_options "++FullsweepString,
    {ok,Node1} =
        start_node(DCfg, net_ticker_spawn_options_1, InetDistOptions),
    {ok,Node2} =
        start_node(DCfg, net_ticker_spawn_options_2, InetDistOptions),
    %%
    pong =
        erpc:call(Node1, net_adm, ping, [Node2]),
    FullsweepOptionNode1 =
        erpc:call(Node1, ?MODULE, get_net_ticker_fullsweep_option, [Node2]),
    FullsweepOptionNode2 =
        erpc:call(Node2, ?MODULE, get_net_ticker_fullsweep_option, [Node1]),
    io:format("FullsweepOptionNode1 = ~p", [FullsweepOptionNode1]),
    io:format("FullsweepOptionNode2 = ~p", [FullsweepOptionNode2]),
    0 = FullsweepOptionNode1,
    0 = FullsweepOptionNode2,
    %%
    stop_node(Node2),
    stop_node(Node1),
    ok.

get_net_ticker_fullsweep_option(Node) ->
    Links = case proplists:get_value(Node, erlang:system_info(dist_ctrl)) of
                DistCtrl when is_port(DistCtrl) ->
                    {links, Ls} = erlang:port_info(DistCtrl, links),
                    Ls;
                DistCtrl when is_pid(DistCtrl) ->
                    {links, Ls} = process_info(DistCtrl, links),
                    Ls
            end,
    Ticker = try
                 lists:foreach(
                   fun (Pid) when is_pid(Pid) ->
                           {current_stacktrace, Stk}
                               = process_info(Pid, current_stacktrace),
                           lists:foreach(
                             fun ({dist_util, con_loop, _, _}) ->
                                     throw(Pid);
                                 (_) ->
                                     ok
                             end, Stk);
                       (_) ->
                           ok
                   end, Links),
                 error(no_ticker_found)
             catch
                 throw:Pid when is_pid(Pid) -> Pid
             end,
    {garbage_collection, GCOpts} = erlang:process_info(Ticker, garbage_collection),
    proplists:get_value(fullsweep_after, GCOpts).



%%
%% Testcase:
%%   monitor_nodes_nodedown_reason
%%

monitor_nodes_nodedown_reason(Config) when is_list(Config) ->
    run_dist_configs(fun monitor_nodes_nodedown_reason/2, Config).

monitor_nodes_nodedown_reason(DCfg, _Config) ->
    MonNodeState = monitor_node_state(),
    ok = net_kernel:monitor_nodes(true),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),

    Names = get_numbered_nodenames(5, node),
    [NN1, NN2, NN3, NN4, NN5] = Names,

    {ok, N1} = start_node(DCfg, NN1, "-connect_all false"),
    {ok, N2} = start_node(DCfg, NN2, "-connect_all false"),
    {ok, N3} = start_node(DCfg, NN3, "-connect_all false"),
    {ok, N4} = start_node(DCfg, NN4, "-hidden"),

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

    {ok, N5} = start_node(DCfg, NN5),
    stop_node(N5),

    receive {nodeup, N5} -> ok end,
    receive {nodedown, N5} -> ok end,
    print_my_messages(),
    ok = check_no_nodedown_nodeup(1000),
    ok = net_kernel:monitor_nodes(false),
    MonNodeState = monitor_node_state(),
    ok.


monitor_nodes_complex_nodedown_reason(Config) when is_list(Config) ->
    run_dist_configs(fun monitor_nodes_complex_nodedown_reason/2, Config).

monitor_nodes_complex_nodedown_reason(DCfg, _Config) ->
    MonNodeState = monitor_node_state(),
    Me = self(),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    [Name] = get_nodenames(1, monitor_nodes_complex_nodedown_reason),
    {ok, Node} = start_node(DCfg, Name, ""),
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
    run_dist_configs(fun monitor_nodes_node_type/2, Config).

monitor_nodes_node_type(DCfg, _Config) ->
    MonNodeState = monitor_node_state(),
    ok = net_kernel:monitor_nodes(true),
    ok = net_kernel:monitor_nodes(true, [{node_type, all}]),
    Names = get_numbered_nodenames(9, node),
    [NN1, NN2, NN3, NN4, NN5, NN6, NN7, NN8, NN9] = Names,

    {ok, N1} = start_node(DCfg, NN1),
    {ok, N2} = start_node(DCfg, NN2),
    {ok, N3} = start_node(DCfg, NN3, "-hidden"),
    {ok, N4} = start_node(DCfg, NN4, "-hidden"),

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
    {ok, N5} = start_node(DCfg, NN5),

    receive {nodeup, N5} -> ok end,
    stop_node(N5),
    receive {nodedown, N5} -> ok end,

    ok = net_kernel:monitor_nodes(true, [{node_type, hidden}]),
    {ok, N6} = start_node(DCfg, NN6),
    {ok, N7} = start_node(DCfg, NN7, "-hidden"),


    receive {nodeup, N6} -> ok end,
    receive {nodeup, N7, [{node_type, hidden}]} -> ok end,
    stop_node(N6),
    stop_node(N7),

    receive {nodedown, N6} -> ok end,
    receive {nodedown, N7, [{node_type, hidden}]} -> ok end,

    ok = net_kernel:monitor_nodes(true, [{node_type, visible}]),
    ok = net_kernel:monitor_nodes(false, [{node_type, hidden}]),
    ok = net_kernel:monitor_nodes(false),

    {ok, N8} = start_node(DCfg, NN8),
    {ok, N9} = start_node(DCfg, NN9, "-hidden"),

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
    run_dist_configs(fun monitor_nodes_misc/2, Config).

monitor_nodes_misc(DCfg, _Config) ->
    MonNodeState = monitor_node_state(),
    ok = net_kernel:monitor_nodes(true),
    ok = net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason, {node_type, all}, connection_id]),
    ok = net_kernel:monitor_nodes(true, #{node_type => all, nodedown_reason => true}),
    ok = net_kernel:monitor_nodes(true, #{node_type => all, nodedown_reason => true, connection_id => true}),
    Names = get_numbered_nodenames(3, node),
    [NN1, NN2, NN3] = Names,

    {ok, N1} = start_node(DCfg, NN1),
    {ok, N2} = start_node(DCfg, NN2, "-hidden"),

    receive {nodeup, N1} -> ok end,

    receive {nodeup, N1, #{node_type := visible}} -> ok end,
    receive {nodeup, N2, #{node_type := hidden}} -> ok end,
    receive {nodeup, N1, [{node_type, visible}]} -> ok end,
    receive {nodeup, N2, [{node_type, hidden}]} -> ok end,

    NodesInfo = erlang:nodes(connected, #{connection_id => true}),

    {N1, #{connection_id := N1CId}} = lists:keyfind(N1, 1, NodesInfo),
    {N2, #{connection_id := N2CId}} = lists:keyfind(N2, 1, NodesInfo),

    ct:pal("N1: ~p ~p~n", [N1, N1CId]),
    ct:pal("N2: ~p ~p~n", [N2, N2CId]),

    receive {nodeup, N1, #{node_type := visible, connection_id := N1CId}} -> ok end,
    receive {nodeup, N2, #{node_type := hidden, connection_id := N2CId}} -> ok end,

    N1UpInfoSorted = lists:sort([{node_type, visible},{connection_id, N1CId}]),
    N2UpInfoSorted = lists:sort([{node_type, hidden},{connection_id, N2CId}]),

    receive {nodeup, N1, UpN1Info} -> N1UpInfoSorted = lists:sort(UpN1Info) end,
    receive {nodeup, N2, UpN2Info} -> N2UpInfoSorted = lists:sort(UpN2Info) end,

    stop_node(N1),
    stop_node(N2),

    receive {nodedown, N1} -> ok end,

    receive {nodedown, N1, #{node_type := visible,
                             nodedown_reason := connection_closed}} -> ok end,
    receive {nodedown, N1, #{node_type := visible,
                             nodedown_reason := connection_closed,
                             connection_id := N1CId}} -> ok end,
    receive {nodedown, N2, #{node_type := hidden,
                             nodedown_reason := connection_closed}} -> ok end,
    receive {nodedown, N2, #{node_type := hidden,
                             nodedown_reason := connection_closed,
                             connection_id := N2CId}} -> ok end,

    N1ADownInfoSorted = lists:sort([{node_type, visible},
                                    {nodedown_reason, connection_closed}]),
    N1BDownInfoSorted = lists:sort([{node_type, visible},
                                    {nodedown_reason, connection_closed},
                                    {connection_id, N1CId}]),
    N2ADownInfoSorted = lists:sort([{node_type, hidden},
                                    {nodedown_reason, connection_closed}]),
    N2BDownInfoSorted = lists:sort([{node_type, hidden},
                                    {nodedown_reason, connection_closed},
                                    {connection_id, N2CId}]),

    receive
        {nodedown, N1, N1Info1} ->
            case lists:sort(N1Info1) of
                N1ADownInfoSorted ->
                    receive
                        {nodedown, N1, N1Info2} ->
                            N1BDownInfoSorted = lists:sort(N1Info2)
                    end;
                N1BDownInfoSorted ->
                    receive
                        {nodedown, N1, N1Info2} ->
                            N1ADownInfoSorted = lists:sort(N1Info2)
                    end
            end
    end,
    receive
        {nodedown, N2, N2Info1} ->
            case lists:sort(N2Info1) of
                N2ADownInfoSorted ->
                    receive
                        {nodedown, N2, N2Info2} ->
                            N2BDownInfoSorted = lists:sort(N2Info2)
                    end;
                N2BDownInfoSorted ->
                    receive
                        {nodedown, N2, N2Info2} ->
                            N2ADownInfoSorted = lists:sort(N2Info2)
                    end
            end
    end,

    ok = net_kernel:monitor_nodes(false, [{node_type, all}, nodedown_reason]),
    ok = net_kernel:monitor_nodes(false, [nodedown_reason, {node_type, all}, connection_id]),
    ok = net_kernel:monitor_nodes(false, #{node_type => all, nodedown_reason => true}),
    ok = net_kernel:monitor_nodes(false, #{node_type => all, nodedown_reason => true, connection_id => true}),

    {ok, N3} = start_node(DCfg, NN3),
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
    run_dist_configs(fun monitor_nodes_otp_6481/2, Config).

monitor_nodes_otp_6481(DCfg, Config) ->
    io:format("Testing nodedown...~n"),
    monitor_nodes_otp_6481_test(DCfg, Config, nodedown),
    io:format("ok~n"),
    io:format("Testing nodeup...~n"),
    monitor_nodes_otp_6481_test(DCfg, Config, nodeup),
    io:format("ok~n"),
    ok.

monitor_nodes_otp_6481_test(DCfg, Config, TestType) when is_list(Config) ->
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
	case TestType of
	    nodedown -> [];
	    nodeup -> [{self(), []}]
	end
	++ lists:map(fun (_) -> {MN, []} end, Seq)
	++ case TestType of
	       nodedown -> [{self(), []}];
	       nodeup -> []
	   end
	++ MonNodeState,

    {ok, Node} = start_node(DCfg, Name, "", this),
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
     {unknown_options,
      #{gurka := true}}} = net_kernel:monitor_nodes(true,
                                                    #{gurka => true}),
    {error,
     {invalid_options,
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
    {error,
     {bad_option_value,
      #{node_type := blaha}}}
	= net_kernel:monitor_nodes(true, #{node_type => blaha}),
    MonNodeState = monitor_node_state(),
    ok.

monitor_nodes_combinations(Config) when is_list(Config) ->
    run_dist_configs(fun monitor_nodes_combinations/2, Config).

monitor_nodes_combinations(DCfg, _Config) ->
    MonNodeState = monitor_node_state(),
    monitor_nodes_all_comb(true),
    [VisibleName, HiddenName] = get_nodenames(2,
					      monitor_nodes_combinations),
    {ok, Visible} = start_node(DCfg, VisibleName, ""),
    receive_all_comb_nodeup_msgs(visible, Visible),
    no_msgs(),
    stop_node(Visible),
    receive_all_comb_nodedown_msgs(visible, Visible, connection_closed),
    no_msgs(),
    {ok, Hidden} = start_node(DCfg, HiddenName, "-hidden"),
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
    run_dist_configs(fun monitor_nodes_many/2, Config).

monitor_nodes_many(DCfg, _Config) ->
    MonNodeState = monitor_node_state(),
    [Name] = get_nodenames(1, monitor_nodes_many),
    %% We want to perform more than 2^16 net_kernel:monitor_nodes
    %% since this will wrap an internal counter
    No = (1 bsl 16) + 17,
    repeat(fun () -> ok = net_kernel:monitor_nodes(true) end, No),
    No = length(monitor_node_state()) - length(MonNodeState),
    {ok, Node} = start_node(DCfg, Name),
    repeat(fun () -> receive {nodeup, Node} -> ok end end, No),
    stop_node(Node),
    repeat(fun () -> receive {nodedown, Node} -> ok end end, No),
    ok = net_kernel:monitor_nodes(false),
    no_msgs(10),
    MonNodeState = monitor_node_state(),
    ok.

%% Test order of messages nodedown and nodeup.
monitor_nodes_down_up(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(#{connection => 0}),
    true = net_kernel:connect_node(Node),
    monitor_nodes_yoyo(Node),
    peer:stop(Peer).

monitor_nodes_yoyo(A) ->
    net_kernel:monitor_nodes(true),
    Papa = self(),

    %% Spawn lots of processes doing one erlang:monitor_node(A,true) each
    %% just to get lots of other monitors to fire when connection goes down
    %% and thereby give time for {nodeup,A} to race before {nodedown,A}.
    NodeMonCnt = 10000,
    NodeMons = [my_spawn_opt(fun F() ->
                                     monitor_node = receive_any(),
                                     monitor_node(A, true),
                                     Papa ! ready,
                                     {nodedown, A} =  receive_any(),
                                     F()
                             end,
                             [link, monitor, {priority, low}])
                ||
                   _ <- lists:seq(1, NodeMonCnt)],

    %% Spawn message spamming process to trigger new connection setups
    %% as quick as possible.
    Spammer = my_spawn_opt(fun F() ->
                                   {dummy, A} ! trigger_auto_connect,
                                   F()
                           end,
                           [link, monitor]),

    %% Now bring connection down and verify we get {nodedown,A} before {nodeup,A}.
    Yoyos = 20,
    [begin
         [P ! monitor_node || P <- NodeMons],
         [receive ready -> ok end || _ <- NodeMons],

         Owner = get_conn_owner(A),
         exit(Owner, kill),

         {nodedown, A} = receive_any(),
         {nodeup, A} = receive_any()
     end
     || _ <- lists:seq(1,Yoyos)],

    unlink(Spammer),
    exit(Spammer, die),
    receive {'DOWN',_,process,Spammer,_} -> ok end,

    [begin unlink(P), exit(P, die) end || P <- NodeMons],
    [receive {'DOWN',_,process,P,_} -> ok end || P <- NodeMons],

    net_kernel:monitor_nodes(false),
    ok.

receive_any() ->
    receive_any(infinity).

receive_any(Timeout) ->
    receive
        M -> M
    after
        Timeout -> timeout
    end.

my_spawn_opt(Fun, Opts) ->
    case spawn_opt(Fun, Opts) of
        {Pid, _Mref} -> Pid;
        Pid -> Pid
    end.

get_conn_owner(Node) ->
    {ok, NodeInfo} = net_kernel:node_info(Node),
    {value,{owner, Owner}} = lists:keysearch(owner, 1, NodeInfo),
    Owner.

dist_ctrl_proc_smoke(Config) when is_list(Config) ->
    dist_ctrl_proc_test(get_nodenames(2, ?FUNCTION_NAME)).

dist_ctrl_proc_reject(Config) when is_list(Config) ->
    ToReject = combinations(dist_util:rejectable_flags()),
    lists:map(fun(Flags) ->
                      ct:log("Try to reject ~p",[Flags]),
                      dist_ctrl_proc_test(get_nodenames(2, ?FUNCTION_NAME),
                        "-gen_tcp_dist_reject_flags " ++ integer_to_list(Flags))
              end, ToReject).

combinations([H | T]) ->
    lists:flatten([[(1 bsl H) bor C || C <- combinations(T)] | combinations(T)]);
combinations([]) ->
    [0];
combinations(BitField) ->
    lists:sort(combinations(bits(BitField, 0))).

bits(0, _) ->
    [];
bits(BitField, Cnt) when BitField band 1 == 1 ->
    [Cnt | bits(BitField bsr 1, Cnt + 1)];
bits(BitField, Cnt) ->
    bits(BitField bsr 1, Cnt + 1).

dist_ctrl_proc_test(Nodes) ->
    dist_ctrl_proc_test(Nodes,"").

dist_ctrl_proc_test([Name1,Name2], Extra) ->
    ThisNode = node(),
    GenTcpOptProlog = "-proto_dist gen_tcp "
        "-gen_tcp_dist_output_loop " ++ atom_to_list(?MODULE) ++ " " ++
        "dist_cntrlr_output_test_size " ++ Extra,
    {ok, Node1} = start_node("", Name1, "-proto_dist gen_tcp"),
    {ok, Node2} = start_node("", Name2, GenTcpOptProlog),
    NL = lists:sort([ThisNode, Node1, Node2]),
    wait_until(fun () ->
                       NL == lists:sort([node()|nodes()])
               end),
    wait_until(fun () ->
                       NL == lists:sort([rpc:call(Node1,erlang, node, [])
                                         | rpc:call(Node1, erlang, nodes, [])])
               end),
    wait_until(fun () ->
                       NL == lists:sort([rpc:call(Node2,erlang, node, [])
                                         | rpc:call(Node2, erlang, nodes, [])])
               end),

    smoke_communicate(Node1, gen_tcp_dist, dist_cntrlr_output_loop),
    smoke_communicate(Node2, erl_distribution_SUITE, dist_cntrlr_output_loop_size),

    stop_node(Node1),
    stop_node(Node2),
    ok.

smoke_communicate(Node, OLoopMod, OLoopFun) ->
    %% Verify that we actually are executing the distribution
    %% module we expect and also massage message passing over
    %% the connection a bit...
    Ps = rpc:call(Node, erlang, processes, []),
    try
        lists:foreach(
          fun (P) ->
                  case rpc:call(Node, erlang, process_info, [P, current_stacktrace]) of
                      undefined ->
                          ok;
                      {current_stacktrace, StkTrace} ->
                          lists:foreach(fun ({Mod, Fun, 2, _}) when Mod == OLoopMod,
                                                                    Fun == OLoopFun ->
                                                io:format("~p ~p~n", [P, StkTrace]),
                                                throw(found_it);
                                            (_) ->
                                                ok
                                        end, StkTrace)
                  end
          end, Ps),
        exit({missing, {OLoopMod, OLoopFun}})
    catch
        throw:found_it -> ok
    end,
    Bin = list_to_binary(lists:duplicate(1000,100)),
    BitStr = <<0:7999>>,
    List = [[Bin], atom, [BitStr|Bin], make_ref(), [[[BitStr|"hopp"]]],
            4711, 111122222211111111111111,"hej", fun () -> ok end, BitStr,
            self(), fun erlang:node/1],
    Pid = spawn_link(Node, fun () -> receive {From1, Msg1} -> From1 ! Msg1 end,
                                     receive {From2, Msg2} -> From2 ! Msg2 end
                           end),
    R = make_ref(),
    Pid ! {self(), [R, List]},
    receive [R, L1] -> List = L1 end,

    %% Send a huge message in order to trigger message fragmentation if enabled
    FragBin = <<0:(2*(1024*64*8))>>,
    Pid ! {self(), [R, List, FragBin]},
    receive [R, L2, B] -> List = L2, FragBin = B end,

    unlink(Pid),
    exit(Pid, kill),
    ok.


erl_uds_dist_smoke_test(Config) when is_list(Config) ->
    case os:type() of
        {win32,_} ->
            {skipped, "Not on Windows"};
        _ ->
            do_erl_uds_dist_smoke_test()
    end.

do_erl_uds_dist_smoke_test() ->
    [Node1, Node2] = lists:map(fun (Name) ->
                                       list_to_atom(atom_to_list(Name) ++ "@localhost")
                               end,
                               get_nodenames(2, erl_uds_dist_smoke_test)),
    {LPort, Acceptor} = uds_listen(),
    start_uds_node(Node1, LPort),
    start_uds_node(Node2, LPort),
    receive
        {uds_nodeup, N1} ->
            io:format("~p is up~n", [N1])
    end,
    receive
        {uds_nodeup, N2} ->
            io:format("~p is up~n", [N2])
    end,
    
    io:format("Testing ping net_adm:ping(~p) on ~p~n", [Node2, Node1]),
    Node1 ! {self(), {net_adm, ping, [Node2]}},
    receive
        {Node1, PingRes} ->
            io:format("~p~n", [PingRes]),
            pong = PingRes
    end,

    io:format("Testing nodes() on ~p~n", [Node1]),
    Node1 ! {self(), {erlang, nodes, []}},
    receive
        {Node1, N1List} ->
            io:format("~p~n", [N1List]),
            [Node2] = N1List
    end,

    io:format("Testing nodes() on ~p~n", [Node2]),
    Node2 ! {self(), {erlang, nodes, []}},
    receive
        {Node2, N2List} ->
            io:format("~p~n", [N2List]),
            [Node1] = N2List
    end,

    io:format("Shutting down~n", []),

    Node1 ! {self(), close},
    Node2 ! {self(), close},

    receive {Node1, C1} -> ok = C1 end,
    receive {Node2, C2} -> ok = C2 end,

    unlink(Acceptor),
    exit(Acceptor, kill),

    io:format("ok~n", []),

    ok.

%% Helpers for testing the erl_uds_dist example

uds_listen() ->
    Me = self(),
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, 4}, {active, false}]),
    {ok, LPort} = inet:port(LSock),
    {LPort, spawn_link(fun () ->
                               uds_accept_loop(LSock, Me)
                       end)}.

uds_accept_loop(LSock, TestProc) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    _ = spawn_link(fun () ->
                           uds_rpc_client_init(Sock, TestProc)
                   end),
    uds_accept_loop(LSock, TestProc).

uds_rpc(Sock, MFA) ->
    ok = gen_tcp:send(Sock, term_to_binary(MFA)),
    case gen_tcp:recv(Sock, 0) of
        {error, Reason} ->
            error({recv_failed, Reason});
        {ok, Packet} ->
            binary_to_term(Packet)
    end.

uds_rpc_client_init(Sock, TestProc) ->
    case uds_rpc(Sock, {erlang, node, []}) of
        nonode@nohost ->
            %% Wait for distribution to come up...
            receive after 100 -> ok end,
            uds_rpc_client_init(Sock, TestProc);
        Node when is_atom(Node) ->
            register(Node, self()),
            TestProc ! {uds_nodeup, Node},
            uds_rpc_client_loop(Sock, Node)
    end.

uds_rpc_client_loop(Sock, Node) ->
    receive
        {From, close} ->
            ok = gen_tcp:send(Sock, term_to_binary(close)),
            From ! {Node, gen_tcp:close(Sock)},
            exit(normal);
        {From, ApplyData} ->
            From ! {Node, uds_rpc(Sock, ApplyData)},
            uds_rpc_client_loop(Sock, Node)
    end.

uds_rpc_server_loop(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {error, Reason} ->
            error({recv_failed, Reason});
        {ok, Packet} ->
            case binary_to_term(Packet) of
                {M, F, A} when is_atom(M), is_atom(F), is_list(A) ->
                    ok = gen_tcp:send(Sock, term_to_binary(apply(M, F, A)));
                {F, A} when is_function(F), is_list(A) ->
                    ok = gen_tcp:send(Sock, term_to_binary(apply(F, A)));
                close ->
                    ok = gen_tcp:close(Sock),
                    exit(normal);
                Other ->
                    error({unexpected_data, Other})
            end
    end,
    uds_rpc_server_loop(Sock).

start_uds_rpc_server([PortString]) ->
    Port = list_to_integer(PortString),
    {Pid, Mon} = spawn_monitor(fun () ->
                                       {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port,
                                                                    [binary, {packet, 4},
                                                                     {active, false}]),
                                       uds_rpc_server_loop(Sock)
                               end),
    receive
        {'DOWN', Mon, process, Pid, Reason} ->
            if Reason == normal ->
                    halt();
               true ->
                    EStr = lists:flatten(io_lib:format("uds rpc server crashed: ~p", [Reason])),
                    (catch file:write_file("uds_rpc_server_crash."++os:getpid(), EStr)),
                    halt(EStr)
            end
    end.

start_uds_node(NodeName, LPort) ->
    Static = "-detached -noinput -proto_dist erl_uds",
    Pa = filename:dirname(code:which(?MODULE)),
    Prog = case catch init:get_argument(progname) of
	       {ok,[[P]]} -> P;
	       _ -> error(no_progname_argument_found)
	   end,
    {ok, Pwd} = file:get_cwd(),
    NameStr = atom_to_list(NodeName),
    CmdLine = Prog ++ " "
	++ Static
	++ " -sname " ++ NameStr
	++ " -pa " ++ Pa
	++ " -env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ NameStr
	++ " -setcookie " ++ atom_to_list(erlang:get_cookie())
        ++ " -run " ++ atom_to_list(?MODULE) ++ " start_uds_rpc_server "
        ++ integer_to_list(LPort),
    io:format("Starting: ~p~n", [CmdLine]),
    case open_port({spawn, CmdLine}, []) of
	Port when is_port(Port) ->
	    unlink(Port),
	    erlang:port_close(Port);
	Error ->
	    error({open_port_failed, Error})
    end,
    ok.

erl_1424(Config) when is_list(Config) ->
    {error, Reason} = erl_epmd:names("."),
    {comment, lists:flatten(io_lib:format("Reason: ~p", [Reason]))}.

net_kernel_start(Config) when is_list(Config) ->
    MyName = net_kernel_start_tester,
    register(MyName, self()),
    net_kernel_start_test(MyName, 120, 8, true, false),
    net_kernel_start_test(MyName, 120, 8, false, false),
    net_kernel_start_test(MyName, 120, 8, true, true),
    net_kernel_start_test(MyName, undefined, undefined, undefined, undefined).

net_kernel_start_test(MyName, NetTickTime, NetTickIntesity, DistListen, Hidden) ->
    TestNameStr = "net_kernel_start_test_node-"
        ++ integer_to_list(erlang:system_time(seconds))
        ++ "-" ++ integer_to_list(erlang:unique_integer([monotonic,positive])),
    TestNode = list_to_atom(TestNameStr ++ "@" ++ atom_to_list(gethostname())),
    CmdLine = net_kernel_start_cmdline(MyName, list_to_atom(TestNameStr),
                                       NetTickTime, NetTickIntesity, DistListen, Hidden),
    io:format("Starting test node ~p: ~s~n", [TestNode, CmdLine]),
    case open_port({spawn, CmdLine}, []) of
	Port when is_port(Port) ->
            case DistListen == false of
                false ->
                    ok;
                true ->
                    receive after 1500 -> ok end,
                    pang = net_adm:ping(TestNode),
                    ok
            end,
            receive
                {i_am_alive, Pid, Node, NTT} = Msg ->
                    IsHidden = lists:member(TestNode, nodes(hidden)),
                    IsVisible = lists:member(TestNode, nodes(visible)),
                    io:format("IsVisible = ~p~nIsHidden = ~p~n", [IsVisible, IsHidden]),
                    io:format("Response from ~p: ~p~n", [Node, Msg]),
                    rpc:cast(Node, erlang, halt, []),
                    catch erlang:port_close(Port),
                    TestNode = node(Pid),
                    TestNode = Node,
                    case NetTickTime == undefined of
                        true ->
                            {ok, DefNTT} = application:get_env(kernel, net_ticktime),
                            DefNTT = NTT;
                        false ->
                            NetTickTime = NTT
                    end,
                    case DistListen == false orelse Hidden == true of
                        true ->
                            true = IsHidden,
                            false = IsVisible;
                        false ->
                            false = IsHidden,
                            true = IsVisible
                    end
            end,
            ok;
	Error ->
	    error({open_port_failed, TestNode, Error})
    end.

net_kernel_start_cmdline(TestName, Name, NetTickTime, NetTickIntensity, DistListen, Hidden) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Prog = case catch init:get_argument(progname) of
	       {ok, [[Prg]]} -> Prg;
	       _ -> error(missing_progname)
	   end,
    NameDomain = case net_kernel:longnames() of
                     false -> "shortnames";
                     true -> "longnames"
                 end,
    {ok, Pwd} = file:get_cwd(),
    NameStr = atom_to_list(Name),
    Prog ++ " -noinput -noshell -detached -pa " ++ Pa
	++ " -env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ NameStr
	++ " -setcookie " ++ atom_to_list(erlang:get_cookie())
	++ " -run " ++ atom_to_list(?MODULE) ++ " net_kernel_start_do_test "
	++ atom_to_list(TestName) ++ " " ++ atom_to_list(node()) ++ " "
        ++ NameStr ++ " " ++ NameDomain
        ++ case NetTickTime == undefined of
               true ->
                   "";
               false ->
                   " " ++ integer_to_list(NetTickTime) ++
                       " " ++ integer_to_list(NetTickIntensity)
           end
        ++ case DistListen == undefined of
               true -> "";
               false -> " " ++ atom_to_list(DistListen)
           end
        ++ case Hidden == undefined of
               true -> "";
               false -> " " ++ atom_to_list(Hidden)
           end.

net_kernel_start_do_test([TestName, TestNode, Name, NameDomain]) ->
    net_kernel_start_do_test(TestName, TestNode, list_to_atom(Name),
                             #{name_domain => list_to_atom(NameDomain)});

net_kernel_start_do_test([TestName, TestNode, Name, NameDomain, NetTickTime, NetTickIntensity,
                          DistListen, Hidden]) ->
    net_kernel_start_do_test(TestName, TestNode, list_to_atom(Name),
                             #{net_ticktime => list_to_integer(NetTickTime),
                               name_domain => list_to_atom(NameDomain),
                               net_tickintensity => list_to_integer(NetTickIntensity),
                               dist_listen => list_to_atom(DistListen),
                               hidden => list_to_atom(Hidden)}).

net_kernel_start_do_test(TestName, TestNode, Name, Options) ->
    case net_kernel:start(Name, Options) of
        {ok, _Pid} ->
            case maps:get(dist_listen, Options, true) of
                false -> receive after 3000 -> ok end;
                true -> ok
            end,
            Tester = {list_to_atom(TestName), list_to_atom(TestNode)},
            Tester ! {i_am_alive, self(), node(), net_kernel:get_net_ticktime()},
            receive after 60000 -> ok end,
            erlang:halt();
        Error ->
            erlang:halt(lists:flatten(io_lib:format("~p", [Error])))
    end.

differing_cookies(Config) when is_list(Config) ->
    test_server:timetrap({minutes, 1}),
    Node = node(),
    true = Node =/= nonode@nohost,
    [] = nodes(),
    BaseName = atom_to_list(?FUNCTION_NAME),

    %% Use -hidden nodes to avoid global connecting all nodes

    %% Start node A with different cookie
    NodeAName = BaseName++"_nodeA",
    NodeA = full_node_name(NodeAName),
    NodeACookieL = BaseName++"_cookieA",
    NodeACookie = list_to_atom(NodeACookieL),
    true = erlang:set_cookie( NodeA, NodeACookie ),
    { ok, NodeA } =
        start_node( "-hidden", NodeAName, "-setcookie "++NodeACookieL ),
    try

        %% Verify the cluster
        [ NodeA ] = nodes(hidden),
        [ Node ] = rpc:call( NodeA, erlang, nodes, [hidden] ),

        %% Start node B with another different cookie
        NodeBName = BaseName++"_nodeB",
        NodeB = full_node_name(NodeBName),
        NodeBCookieL = BaseName++"_cookieB",
        NodeBCookie = list_to_atom(NodeBCookieL),
        true = erlang:set_cookie( NodeB, NodeBCookie ),
        { ok, NodeB } =
            start_node( "-hidden", NodeBName, "-setcookie "++NodeBCookieL ),
        try

            %% Verify the cluster
            equal_sets( [NodeA, NodeB], nodes(hidden) ),
            [ Node ] = rpc:call( NodeA, erlang, nodes, [hidden] ),
            [ Node ] = rpc:call( NodeB, erlang, nodes, [hidden] ),

            %% Verify that the nodes can not connect
            %% before correcting the cookie configuration
            pang = rpc:call( NodeA, net_adm, ping, [NodeB] ),
            pang = rpc:call( NodeB, net_adm, ping, [NodeA] ),

            %% Configure cookie and connect node A -> B
            true = rpc:call( NodeA, erlang, set_cookie, [NodeB, NodeBCookie] ),
            pong = rpc:call( NodeA, net_adm, ping, [NodeB] ),

            %% Verify the cluster
            NodeACookie = rpc:call( NodeA, erlang, get_cookie, []),
            NodeBCookie = rpc:call( NodeB, erlang, get_cookie, []),
            equal_sets( [NodeA, NodeB], nodes(hidden) ),
            equal_sets( [Node, NodeB],
                        rpc:call( NodeA, erlang, nodes, [hidden] )),
            equal_sets( [Node, NodeA],
                        rpc:call( NodeB, erlang, nodes, [hidden] )),

            %% Disconnect node A from B
            true = rpc:call( NodeB, net_kernel, disconnect, [NodeA] ),

            %% Verify the cluster
            equal_sets( [NodeA, NodeB], nodes(hidden) ),
            [ Node ] = rpc:call( NodeA, erlang, nodes, [hidden] ),
            [ Node ] = rpc:call( NodeB, erlang, nodes, [hidden] ),

            %% Reconnect, now node B -> A
            pong = rpc:call( NodeB, net_adm, ping, [NodeA] ),

            %% Verify the cluster
            equal_sets( [NodeA, NodeB], nodes(hidden) ),
            equal_sets( [Node, NodeB],
                        rpc:call( NodeA, erlang, nodes, [hidden] )),
            equal_sets( [Node, NodeA],
                        rpc:call( NodeB, erlang, nodes, [hidden] ))

        after
            _ = stop_node(NodeB)
        end
    after
        _ = stop_node(NodeA)
    end,
    [] = nodes(hidden),
    ok.

cmdline_setcookie_2(Config) when is_list(Config) ->
    test_server:timetrap({minutes, 1}),
    Node = node(),
    true = Node =/= nonode@nohost,
    [] = nodes(),
    NodeL = atom_to_list(Node),
    BaseName = atom_to_list(?FUNCTION_NAME),
    NodeCookie = erlang:get_cookie(),
    NodeCookieL = atom_to_list(NodeCookie),

    %% Use -hidden nodes to avoid global connecting all nodes

    %% Start node A with different cookie
    %% and cookie configuration of mother node
    NodeAName = BaseName++"_nodeA",
    NodeA = full_node_name(NodeAName),
    NodeACookieL = BaseName++"_cookieA",
    NodeACookie = list_to_atom(NodeACookieL),
    { ok, NodeA } =
        start_node(
          "-hidden", NodeAName,
          "-setcookie "++NodeL++" "++NodeCookieL ),

    try

        %% Verify the cluster
        [ NodeA ] = nodes(hidden),
        [ Node ] = rpc:call( NodeA, erlang, nodes, [hidden] ),
        NodeCookie = rpc:call( NodeA, erlang, get_cookie, []),

        true = rpc:call( NodeA, erlang, set_cookie, [NodeACookie] ),

        %% Start node B with different cookie
        %% and cookie configuration of mother node and node A
        NodeBName = BaseName++"_nodeB",
        NodeB = full_node_name(NodeBName),
        NodeBCookieL = BaseName++"_cookieB",
        NodeBCookie = list_to_atom(NodeBCookieL),
        { ok, NodeB } =
            start_node(
              "-hidden", NodeBName,
              "-setcookie "++NodeBCookieL++" "
              "-setcookie "++NodeL++" "++NodeCookieL++" "
              "-setcookie "++atom_to_list(NodeA)++" "++NodeACookieL ),

        try

            %% Verify the cluster
            NodeACookie = rpc:call( NodeA, erlang, get_cookie, []),
            NodeBCookie = rpc:call( NodeB, erlang, get_cookie, []),
            equal_sets( [NodeA, NodeB], nodes(hidden) ),
            [ Node ] = rpc:call( NodeA, erlang, nodes, [hidden] ),
            [ Node ] = rpc:call( NodeB, erlang, nodes, [hidden] ),

            %% Connect the nodes
            pong = rpc:call( NodeA, net_adm, ping, [NodeB] ),

            %% Verify the cluster
            NodeACookie = rpc:call( NodeA, erlang, get_cookie, []),
            NodeBCookie = rpc:call( NodeB, erlang, get_cookie, []),
            equal_sets( [NodeA, NodeB], nodes(hidden) ),
            equal_sets( [Node, NodeB],
                        rpc:call( NodeA, erlang, nodes, [hidden] )),
            equal_sets( [Node, NodeA],
                        rpc:call( NodeB, erlang, nodes, [hidden] ))

        after
            _ = stop_node(NodeB)
        end
    after
        _ = stop_node(NodeA)
    end,
    [] = nodes(hidden),
    ok.

connection_cookie(Config) when is_list(Config) ->
    test_server:timetrap({minutes, 1}),
    Node = node(),
    true = Node =/= nonode@nohost,
    [] = nodes(),
    NodeL = atom_to_list(Node),
    BaseName = atom_to_list(?FUNCTION_NAME),

    %% Start node A with dedicated connection cookie
    NodeAName = BaseName++"_nodeA",
    NodeA = full_node_name(NodeAName),
    NodeACookieL = BaseName++"_cookieA",
    NodeACookie = list_to_atom(NodeACookieL),
    true = NodeACookie =/= erlang:get_cookie(),
    ConnectionCookieL = BaseName++"_connectionCookie",
    ConnectionCookie = list_to_atom(ConnectionCookieL),
    true = erlang:set_cookie( NodeA, ConnectionCookie ),
    { ok, NodeA } =
        start_node(
          "", NodeAName,
          "-setcookie "++NodeACookieL++" "
          "-setcookie "++NodeL++" "++ConnectionCookieL ),

    try

        %% Verify the cluster
        [ NodeA ] = nodes(),
        [ Node ] = rpc:call( NodeA, erlang, nodes, [] ),
        NodeACookie = rpc:call( NodeA, erlang, get_cookie, []),
        ConnectionCookie = rpc:call( NodeA, auth, get_cookie, [Node]),
        ConnectionCookie = erlang:get_cookie( NodeA )

    after
        _ = stop_node(NodeA)
    end,
    [] = nodes(),
    ok.

dyn_differing_cookies(Config) when is_list(Config) ->
    test_server:timetrap({minutes, 1}),
    MotherNode = node(),
    true = MotherNode =/= nonode@nohost,
    [] = nodes(hidden),
    MotherNodeL = atom_to_list(MotherNode),
    BaseName = atom_to_list(?FUNCTION_NAME),
    MotherNodeCookie = erlang:get_cookie(),
    MotherNodeCookieL = atom_to_list(MotherNodeCookie),
    register(?FUNCTION_NAME, self()),

    %% Start node A with different cookie
    %% and cookie configuration of mother node
    DynNodeCookieL = BaseName++"_cookieA",
    DynNodeCookie = list_to_atom(DynNodeCookieL),
    {_NF1, Port1} =
        start_node_unconnected(
          "-setcookie "++MotherNodeL++" "++MotherNodeCookieL,
          undefined, DynNodeCookie,
          ?MODULE, run_remote_test,
          ["ddc_remote_run", MotherNodeL, "cmdline", MotherNodeCookieL] ),

    dyn_differing_cookies(MotherNode, MotherNodeCookie, DynNodeCookie, Port1),

    %% Same again, but use erlang:set_cookie/2 to set MotherNodeCookie
    {_NF2, Port2} =
        start_node_unconnected(
          "",
          undefined, DynNodeCookie,
          ?MODULE, run_remote_test,
          ["ddc_remote_run", MotherNodeL, "set_cookie", MotherNodeCookieL] ),

    dyn_differing_cookies(MotherNode, MotherNodeCookie, DynNodeCookie, Port2).


dyn_differing_cookies(MotherNode, MotherNodeCookie, DynNodeCookie, Port) ->
    receive
        { MotherNode, MotherNodeCookie, DynNodeCookie, DynNode } ->
            [ DynNode ] = nodes(hidden),
            [ MotherNode ] = rpc:call( DynNode, erlang, nodes, [hidden] ),
            DynNodeCookie = rpc:call( DynNode, erlang, get_cookie, [] ),
            MotherNodeCookie =
                rpc:call( DynNode, erlang, get_cookie, [MotherNode] ),
            {ddc_remote_run, DynNode} !
                {MotherNode, MotherNodeCookie, DynNode},

            0 = wait_for_port_exit(Port),

            [] = nodes(hidden),
            ok;
        {Port, {data, Data}} ->
            io:format("~p: ~s", [Port, Data]),
            dyn_differing_cookies(
              MotherNode, MotherNodeCookie, DynNodeCookie, Port);
        Other ->
            error({unexpected, Other})
    end.

ddc_remote_run(MotherNode, [SetCookie, MotherNodeCookieL]) ->
    nonode@nohost = node(),
    [] = nodes(hidden),
    MotherNodeCookie = list_to_atom(MotherNodeCookieL),
    case SetCookie of
        "set_cookie" ->
            erlang:set_cookie(MotherNode, MotherNodeCookie);
        "cmdline" ->
            ok
    end,
    MotherNodeCookie = erlang:get_cookie(MotherNode),
    true = net_kernel:connect_node( MotherNode ),
    [ MotherNode ] = nodes(hidden),
    DynNode = node(),
    [ DynNode ] = rpc:call( MotherNode, erlang, nodes, [hidden] ),
    MotherNodeCookie = erlang:get_cookie( MotherNode ),
    MotherNodeCookie = rpc:call( MotherNode, erlang, get_cookie, [] ),
    %% Here we get the mother node's default cookie
    MotherNodeCookie = rpc:call( MotherNode, erlang, get_cookie, [DynNode] ),
    DynNodeCookie = erlang:get_cookie(),
    register(ddc_remote_run, self() ),
    {dyn_differing_cookies, MotherNode} !
        {MotherNode, MotherNodeCookie, DynNodeCookie, DynNode},
    receive
        { MotherNode, MotherNodeCookie, DynNode } ->
            true = disconnect_node( MotherNode ),
            [] = nodes(hidden),
            ok;
        Other ->
            error({unexpected, Other})
    end.

xdg_cookie(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    TestHome = filename:join(PrivDir, ?FUNCTION_NAME),
    ok = file:make_dir(TestHome),

    HomeEnv = case os:type() of
              {win32, _} ->
                  [Drive | Path] = filename:split(TestHome),
                  [{"APPDATA", filename:join(TestHome,"AppData")},
                   {"HOMEDRIVE", Drive}, {"HOMEPATH", filename:join(Path)}];
              _ ->
                  [{"HOME", TestHome}]
          end,

    NodeOpts = #{ env => HomeEnv ++
                      [{"ERL_CRASH_DUMP", filename:join([TestHome,"erl_crash.dump"])}],
                  connection => 0 },

    %% Test that a default .erlang.cookie file is created
    {ok, CreatorPeer, _} = peer:start_link(NodeOpts#{ name => peer:random_name(?FUNCTION_NAME) }),
    UserConfig = peer:call(CreatorPeer, filename, basedir, [user_config,"erlang"]),
    ?assert(peer:call(CreatorPeer, filelib, is_regular,
                      [filename:join(TestHome, ".erlang.cookie")])),
    OrigCookie = peer:call(CreatorPeer, erlang, get_cookie, []),
    peer:stop(CreatorPeer),

    %% Test that the $HOME/.erlang.cookie file takes precedence over XDG
    XDGCookie = filename:join([UserConfig, ".erlang.cookie"]),
    ok = filelib:ensure_dir(XDGCookie),
    ok = file:write_file(XDGCookie, "Me want cookie!"),
    {ok, XDGFI} = file:read_file_info(XDGCookie),
    ok = file:write_file_info(XDGCookie, XDGFI#file_info{ mode = 8#600 }),

    {ok, Peer, _} = peer:start_link(NodeOpts#{ name => peer:random_name(?FUNCTION_NAME) }),

    ?assertEqual(OrigCookie, peer:call(Peer, erlang, get_cookie, [])),

    peer:stop(Peer),

    %% Check that XDG cookie works after we delete the $HOME cookie
    HomeCookie = filename:join(TestHome, ".erlang.cookie"),
    {ok, HomeFI} = file:read_file_info(HomeCookie),
    ok = file:write_file_info(HomeCookie, HomeFI#file_info{ mode = 8#777 }),
    ok = file:delete(HomeCookie),
    {ok, Peer2, _} = peer:start_link(NodeOpts#{ name => peer:random_name(?FUNCTION_NAME) }),
    ?assertEqual('Me want cookie!', peer:call(Peer2, erlang, get_cookie, [])),

    peer:stop(Peer2),

    ok.



%% Misc. functions

equal_sets(A, B) ->
    S = lists:sort(A),
    case lists:sort(B) of
        S ->
            ok;
        _ ->
            erlang:error({not_equal_sets, A, B})
    end.

run_dist_configs(Func, Config) ->
    GetOptProlog = "-proto_dist gen_tcp -gen_tcp_dist_output_loop "
        ++ atom_to_list(?MODULE) ++ " ",
    GenTcpDistTest = case get_gen_tcp_dist_test_type() of
                         default ->
                             {"gen_tcp_dist", "-proto_dist gen_tcp"};
                         size ->
                             {"gen_tcp_dist (get_size)",
                              GetOptProlog ++ "dist_cntrlr_output_test_size"}
                     end,
    lists:map(fun ({DCfgName, DCfg}) ->
                      io:format("~n~n=== Running ~s configuration ===~n~n",
                                [DCfgName]),
                      Func(DCfg, Config)
              end,
              [{"default", ""}, GenTcpDistTest]).

start_gen_tcp_dist_test_type_server() ->
    Me = self(),
    Go = make_ref(),
    io:format("STARTING: gen_tcp_dist_test_type_server~n",[]),
    {P, M} = spawn_monitor(fun () ->
                                   register(gen_tcp_dist_test_type_server, self()),
                                   Me ! Go,
                                   gen_tcp_dist_test_type_server()
                           end),
    receive
        Go ->
            ok;
        {'DOWN', M, process, P, _} ->
            start_gen_tcp_dist_test_type_server()
    end.

kill_gen_tcp_dist_test_type_server() ->
    case whereis(gen_tcp_dist_test_type_server) of
        undefined ->
            ok;
        Pid ->
            exit(Pid,kill),
            %% Sync death, before continuing...
            false = erlang:is_process_alive(Pid)
    end.

gen_tcp_dist_test_type_server() ->
    Type = case abs(erlang:monotonic_time(second)) rem 2 of
               0 -> default;
               1 -> size
           end,
    gen_tcp_dist_test_type_server(Type).

gen_tcp_dist_test_type_server(Type) ->
    receive
        {From, Ref} ->
            From ! {Ref, Type},
            NewType = case Type of
                          default -> size;
                          size -> default
                      end,
            gen_tcp_dist_test_type_server(NewType)
    end.

get_gen_tcp_dist_test_type() ->
    Ref = make_ref(),
    try
        gen_tcp_dist_test_type_server ! {self(), Ref},
        receive
            {Ref, Type} ->
                Type
        end
    catch
        error:badarg ->
            start_gen_tcp_dist_test_type_server(),
            get_gen_tcp_dist_test_type()
    end.

dist_cntrlr_output_test_size(DHandle, Socket) ->
    false = erlang:dist_ctrl_get_opt(DHandle, get_size),
    false = erlang:dist_ctrl_set_opt(DHandle, get_size, true),
    true = erlang:dist_ctrl_get_opt(DHandle, get_size),
    true = erlang:dist_ctrl_set_opt(DHandle, get_size, false),
    false = erlang:dist_ctrl_get_opt(DHandle, get_size),
    false = erlang:dist_ctrl_set_opt(DHandle, get_size, true),
    true = erlang:dist_ctrl_get_opt(DHandle, get_size),
    dist_cntrlr_output_loop_size(DHandle, Socket).

dist_cntrlr_output_loop_size(DHandle, Socket) ->
    receive
        dist_data ->
            %% Outgoing data from this node...
            dist_cntrlr_send_data_size(DHandle, Socket);
        _ ->
            ok %% Drop garbage message...
    end,
    dist_cntrlr_output_loop_size(DHandle, Socket).

dist_cntrlr_send_data_size(DHandle, Socket) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            erlang:dist_ctrl_get_data_notification(DHandle);
        {Size, Data} ->
            ok = ensure_iovec(Data),
            Size = erlang:iolist_size(Data),
            ok = gen_tcp:send(Socket, Data),
            dist_cntrlr_send_data_size(DHandle, Socket)
    end.

ensure_iovec([]) ->
    ok;
ensure_iovec([X|Y]) when is_binary(X) ->
    ensure_iovec(Y).

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

start_node(_DCfg, Name, Param, this) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, peer, [{args, NewParam}, {erl, [this]}]);
start_node(DCfg, Name, Param, "this") ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)) ++ " " ++ DCfg,
    test_server:start_node(Name, peer, [{args, NewParam}, {erl, [this]}]);
start_node(DCfg, Name, Param, Rel) when is_atom(Rel) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)) ++ " " ++ DCfg,
    test_server:start_node(Name, peer, [{args, NewParam}, {erl, [{release, atom_to_list(Rel)}]}]);
start_node(DCfg, Name, Param, Rel) when is_list(Rel) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)) ++ " " ++ DCfg,
    test_server:start_node(Name, peer, [{args, NewParam}, {erl, [{release, Rel}]}]).

start_node(DCfg, Name, Param) ->
    NewParam = Param ++ " -pa " ++ filename:dirname(code:which(?MODULE)) ++ " " ++ DCfg,
    test_server:start_node(Name, slave, [{args, NewParam}]).

start_node(DCfg, Name) ->
    start_node(DCfg, Name, "").

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

    
