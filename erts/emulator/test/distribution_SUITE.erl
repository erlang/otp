%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

-module(distribution_SUITE).
-compile(r15).

-define(VERSION_MAGIC,       131).

-define(ATOM_EXT,            100).
-define(REFERENCE_EXT,       101).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).
-define(ATOM_UTF8_EXT,       118).
-define(SMALL_ATOM_UTF8_EXT, 119).

%% Tests distribution and the tcp driver.

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 ping/1, bulk_send_small/1,
	 bulk_send_big/1, bulk_send_bigbig/1,
	 local_send_small/1, local_send_big/1,
	 local_send_legal/1, link_to_busy/1, exit_to_busy/1,
	 lost_exit/1, link_to_dead/1, link_to_dead_new_node/1,
	 applied_monitor_node/1, ref_port_roundtrip/1, nil_roundtrip/1,
	 trap_bif_1/1, trap_bif_2/1, trap_bif_3/1,
	 stop_dist/1, 
	 dist_auto_connect_never/1, dist_auto_connect_once/1,
	 dist_parallel_send/1,
	 atom_roundtrip/1,
	 unicode_atom_roundtrip/1,
	 atom_roundtrip_r15b/1,
	 contended_atom_cache_entry/1,
	 contended_unicode_atom_cache_entry/1,
	 bad_dist_structure/1,
	 bad_dist_ext_receive/1,
	 bad_dist_ext_process_info/1,
	 bad_dist_ext_control/1,
	 bad_dist_ext_connection_id/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

%% Internal exports.
-export([sender/3, receiver2/2, dummy_waiter/0, dead_process/0,
	 roundtrip/1, bounce/1, do_dist_auto_connect/1, inet_rpc_server/1,
	 dist_parallel_sender/3, dist_parallel_receiver/0,
	 dist_evil_parallel_receiver/0,
         sendersender/4, sendersender2/4]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [ping, {group, bulk_send}, {group, local_send},
     link_to_busy, exit_to_busy, lost_exit, link_to_dead,
     link_to_dead_new_node, applied_monitor_node,
     ref_port_roundtrip, nil_roundtrip, stop_dist,
     {group, trap_bif}, {group, dist_auto_connect},
     dist_parallel_send, atom_roundtrip, unicode_atom_roundtrip, atom_roundtrip_r15b,
     contended_atom_cache_entry, contended_unicode_atom_cache_entry,
     bad_dist_structure, {group, bad_dist_ext}].

groups() -> 
    [{bulk_send, [], [bulk_send_small, bulk_send_big, bulk_send_bigbig]},
     {local_send, [],
      [local_send_small, local_send_big, local_send_legal]},
     {trap_bif, [], [trap_bif_1, trap_bif_2, trap_bif_3]},
     {dist_auto_connect, [],
      [dist_auto_connect_never, dist_auto_connect_once]},
     {bad_dist_ext, [],
      [bad_dist_ext_receive, bad_dist_ext_process_info,
       bad_dist_ext_control, bad_dist_ext_connection_id]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-define(DEFAULT_TIMETRAP, 4*60*1000).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?DEFAULT_TIMETRAP),
    [{watchdog, Dog},{testcase, Func}|Config].

end_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

ping(doc) ->
    ["Tests pinging a node in different ways."];
ping(Config) when is_list(Config) ->
    Times = 1024,

    %% Ping a non-existing node many times.  This used to crash the emulator
    %% on Windows.

    ?line Host = hostname(),
    ?line BadName = list_to_atom("__pucko__@" ++ Host),
    ?line io:format("Pinging ~s (assumed to not exist)", [BadName]),
    ?line test_server:do_times(Times, fun() -> pang = net_adm:ping(BadName)
			       end),

    %% Pings another node.

    ?line {ok, OtherNode} = start_node(distribution_SUITE_other),
    ?line io:format("Pinging ~s (assumed to exist)", [OtherNode]),
    ?line test_server:do_times(Times, fun() -> pong = net_adm:ping(OtherNode) end),
    ?line stop_node(OtherNode),

    %% Pings our own node many times.

    ?line Node = node(),
    ?line io:format("Pinging ~s (the same node)", [Node]),
    ?line test_server:do_times(Times, fun() -> pong = net_adm:ping(Node) end),

    ok.

bulk_send_small(Config) when is_list(Config) ->
    ?line bulk_send(64, 32).

bulk_send_big(Config) when is_list(Config) ->
    ?line bulk_send(32, 64).

bulk_send_bigbig(Config) when is_list(Config) ->
    ?line bulk_sendsend(32*5, 4).

bulk_send(Terms, BinSize) ->
    ?line Dog = test_server:timetrap(test_server:seconds(30)),

    ?line io:format("Sending ~w binaries, each of size ~w K",
		    [Terms, BinSize]),
    ?line {ok, Node} = start_node(bulk_receiver),
    ?line Recv = spawn(Node, erlang, apply, [fun receiver/2, [0, 0]]),
    ?line Bin = list_to_binary(lists:duplicate(BinSize*1024, 253)),
    ?line Size = Terms*size(Bin),
    ?line {Elapsed, {Terms, Size}} = test_server:timecall(?MODULE, sender,
							  [Recv, Bin, Terms]),
    ?line stop_node(Node),

    ?line test_server:timetrap_cancel(Dog),
    {comment, integer_to_list(trunc(Size/1024/Elapsed+0.5)) ++ " K/s"}.

bulk_sendsend(Terms, BinSize) ->
    {Rate1, MonitorCount1} = bulk_sendsend2(Terms, BinSize,   5),
    {Rate2, MonitorCount2} = bulk_sendsend2(Terms, BinSize, 995),
    Ratio = if MonitorCount2 == 0 -> MonitorCount1 / 1.0;
               true               -> MonitorCount1 / MonitorCount2
            end,
    Comment = integer_to_list(Rate1) ++ " K/s, " ++
	integer_to_list(Rate2) ++ " K/s, " ++
	integer_to_list(MonitorCount1) ++ " monitor msgs, " ++
	integer_to_list(MonitorCount2) ++ " monitor msgs, " ++
	float_to_list(Ratio) ++ " monitor ratio",
    if
	%% A somewhat arbitrary ratio, but hopefully one that will
	%% accommodate a wide range of CPU speeds.
	Ratio > 8.0 ->
	    {comment,Comment};
	true ->
	    io:put_chars(Comment),
	    ?line ?t:fail(ratio_too_low)
    end.

bulk_sendsend2(Terms, BinSize, BusyBufSize) ->
    ?line Dog = test_server:timetrap(test_server:seconds(30)),

    ?line io:format("Sending ~w binaries, each of size ~w K",
		    [Terms, BinSize]),
    ?line {ok, NodeRecv} = start_node(bulk_receiver),
    ?line Recv = spawn(NodeRecv, erlang, apply, [fun receiver/2, [0, 0]]),
    ?line Bin = list_to_binary(lists:duplicate(BinSize*1024, 253)),
    %%?line Size = Terms*size(Bin),

    %% SLF LEFT OFF HERE.
    %% When the caller uses small hunks, like 4k via
    %% bulk_sendsend(32*5, 4), then (on my laptop at least), we get
    %% zero monitor messages.  But if we use "+zdbbl 5", then we
    %% get a lot of monitor messages.  So, if we can count up the
    %% total number of monitor messages that we get when running both
    %% default busy size and "+zdbbl 5", and if the 5 case gets
    %% "many many more" monitor messages, then we know we're working.

    ?line {ok, NodeSend} = start_node(bulk_sender, "+zdbbl " ++ integer_to_list(BusyBufSize)),
    ?line _Send = spawn(NodeSend, erlang, apply, [fun sendersender/4, [self(), Recv, Bin, Terms]]),
    ?line {Elapsed, {_TermsN, SizeN}, MonitorCount} =
        receive {sendersender, BigRes} ->
                BigRes
        end,
    ?line stop_node(NodeRecv),
    ?line stop_node(NodeSend),

    ?line test_server:timetrap_cancel(Dog),
    {trunc(SizeN/1024/Elapsed+0.5), MonitorCount}.

sender(To, _Bin, 0) ->
    To ! {done, self()},
    receive
	Any ->
	    Any
    end;
sender(To, Bin, Left) ->
    To ! {term, Bin},
    sender(To, Bin, Left-1).

%% Sender process to be run on a slave node

sendersender(Parent, To, Bin, Left) ->
    erlang:system_monitor(self(), [busy_dist_port]),
    [spawn(fun() -> sendersender2(To, Bin, Left, false) end) ||
        _ <- lists:seq(1,1)],
    {USec, {Res, MonitorCount}} =
        timer:tc(?MODULE, sendersender2, [To, Bin, Left, true]),
    Parent ! {sendersender, {USec/1000000, Res, MonitorCount}}.

sendersender2(To, Bin, Left, SendDone) ->
    sendersender3(To, Bin, Left, SendDone, 0).

sendersender3(To, _Bin, 0, SendDone, MonitorCount) ->
    if SendDone ->
            To ! {done, self()};
       true ->
            ok
    end,
    receive
        {monitor, _Pid, _Type, _Info} ->
            sendersender3(To, _Bin, 0, SendDone, MonitorCount + 1)
    after 0 ->
            if SendDone ->
                    receive
                        Any when is_tuple(Any), size(Any) == 2 ->
                            {Any, MonitorCount}
                    end;
               true ->
                    exit(normal)
            end
    end;
sendersender3(To, Bin, Left, SendDone, MonitorCount) ->
    To ! {term, Bin},
    %%timer:sleep(50),
    sendersender3(To, Bin, Left-1, SendDone, MonitorCount).

%% Receiver process to be run on a slave node.

receiver(Terms, Size) ->
    receive
	{term, Bin} ->
	    receiver(Terms+1, Size+size(Bin));
	{done, ReplyTo} ->
	    ReplyTo ! {Terms, Size}
    end.



local_send_big(doc) ->
    ["Sends several big message to an non-registered process on ",
     "the local node."];
local_send_big(Config) when is_list(Config) ->
    Data0=local_send_big(doc)++
	["Tests sending small and big messages to a non-existing ",
        "local registered process."],
    Data1=[Data0,[Data0, Data0, [Data0], Data0],Data0],
    Data2=Data0++lists:flatten(Data1)++
	list_to_binary(lists:flatten(Data1)),
    Func=fun() -> Data2= {arbitrary_name, node()} ! Data2 end,
    ?line test_server:do_times(4096, Func),
    ok.

local_send_small(doc) ->
    ["Sends a small message to an non-registered process on the ",
     "local node."];
local_send_small(Config) when is_list(Config) ->
    Data={some_stupid, "arbitrary", 'Data'},
    Func=fun() -> Data= {unregistered_name, node()} ! Data end,
    ?line test_server:do_times(4096, Func),
    ok.

local_send_legal(doc) ->
    ["Sends data to a registered process on the local node, ",
     "as if it was on another node."];
local_send_legal(Config) when is_list(Config) ->
    Times=16384,
    Data={local_send_legal(doc), local_send_legal(doc)},
    Pid=spawn(?MODULE,receiver2, [0, 0]) ,
    ?line true=register(registered_process, Pid),

    Func=fun() -> Data={registered_process, node()} ! Data end,
    TotalSize=size(Data)*Times,
    ?line test_server:do_times(Times, Func),

    % Check that all msgs really came through.
    Me=self(),
    ?line {done, Me}=
	{registered_process, node()} ! {done, Me},
    receive
	{Times, TotalSize} ->
	    ok;
	_ ->
	    test_server:fail("Wrong number of msgs received.")
    end,
    ok.

receiver2(Num, TotSize) ->
    receive
	{done, ReplyTo} ->
	    ReplyTo ! {Num, TotSize};
	Stuff ->
	    receiver2(Num+1, TotSize+size(Stuff))
    end.

link_to_busy(doc) -> "Test that link/1 to a busy distribution port works.";
link_to_busy(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    ?line {ok, Node} = start_node(link_to_busy),
    ?line Recv = spawn(Node, erlang, apply, [fun sink/1, [link_to_busy_sink]]),

    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
		 "true" -> start_busy_dist_port_tracer();
		 _ -> false
	     end,

    %% We will spawn off a process which will try to link to the other
    %% node.  The linker process will not actually run until this
    %% process is suspended due to the busy distribution port (because
    %% of the big send).  When the link/1 is run, the linker
    %% process will block, too, because of the because busy port,
    %% and will later be restarted.

    ?line do_busy_test(Node, fun () -> linker(Recv) end),

    %% Same thing, but we apply link/1 instead of calling it directly.

    ?line do_busy_test(Node, fun () -> applied_linker(Recv) end),

    %% Same thing again, but we apply link/1 in the tail of a function.

    ?line do_busy_test(Node, fun () -> tail_applied_linker(Recv) end),

    %% Done.
    ?line stop_node(Node),
    ?line stop_busy_dist_port_tracer(Tracer),
    ?line test_server:timetrap_cancel(Dog),
    ok.

linker(Pid) ->
    true = link(Pid),
    {links, Links} = process_info(self(), links),
    true = lists:member(Pid, Links).

applied_linker(Pid) ->
    true = apply(erlang, link, [Pid]),
    {links, Links} = process_info(self(), links),
    true = lists:member(Pid, Links).

tail_applied_linker(Pid) ->
    apply(erlang, link, [Pid]).
    
exit_to_busy(doc) -> "Test that exit/2 to a busy distribution port works.";
exit_to_busy(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    ?line {ok, Node} = start_node(exit_to_busy),

    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
		 "true" -> start_busy_dist_port_tracer();
		 _ -> false
	     end,

    %% We will spawn off a process which will try to exit a process on
    %% the other node.  That process will not actually run until this
    %% process is suspended due to the busy distribution port
    %% The process executing exit/2 will block,
    %% too, because of the busy distribution port, and will be allowed
    %% to continue when the port becomes non-busy.

    ?line Recv1 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    ?line M1 = erlang:monitor(process, Recv1),
    ?line do_busy_test(Node, fun () -> joey_killer(Recv1) end),
    ?line receive
	      {'DOWN', M1, process, Recv1, R1} ->
		  ?line joey_said_die = R1
	  end,

    %% Same thing, but tail call to exit/2.
    ?line Recv2 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    ?line M2 = erlang:monitor(process, Recv2),
    ?line do_busy_test(Node, fun () -> tail_joey_killer(Recv2) end),
    ?line receive
	      {'DOWN', M2, process, Recv2, R2} ->
		  ?line joey_said_die = R2
	  end,

    %% Same thing, but we apply exit/2 instead of calling it directly.
    ?line Recv3 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    ?line M3 = erlang:monitor(process, Recv3),
    ?line do_busy_test(Node, fun () -> applied_joey_killer(Recv3) end),
    ?line receive
	      {'DOWN', M3, process, Recv3, R3} ->
		  ?line joey_said_die = R3
	  end,

    %% Same thing again, but we apply exit/2 in the tail of a function.
    ?line Recv4 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    ?line M4 = erlang:monitor(process, Recv4),
    ?line do_busy_test(Node, fun () -> tail_applied_joey_killer(Recv4) end),
    ?line receive
	      {'DOWN', M4, process, Recv4, R4} ->
		  ?line joey_said_die = R4
	  end,
    
    %% Done.
    ?line stop_node(Node),
    ?line stop_busy_dist_port_tracer(Tracer),
    ?line test_server:timetrap_cancel(Dog),
    ok.

make_busy_data() ->
    Size = 1024*1024,
    Key = '__busy__port__data__',
    case get(Key) of
	undefined ->
	    Data = list_to_binary(lists:duplicate(Size, 253)),
	    put(Key, Data),
	    Data;
	Data ->
	    true = is_binary(Data),
	    true = size(Data) == Size,
	    Data
    end.

make_busy(Node, Time) when is_integer(Time) ->
    Own = 500,
    freeze_node(Node, Time+Own),
    Data = make_busy_data(),
    %% first make port busy
    Pid = spawn_link(fun () ->
			     forever(fun () ->
					     dport_reg_send(Node,
							    '__noone__',
							    Data)
				     end)
		     end),
    receive after Own -> ok end,
    until(fun () ->
		  case process_info(Pid, status) of
		      {status, suspended} -> true;
		      _ -> false
		  end
	  end),
    %% then dist entry
    make_busy(Node, [nosuspend], Data),
    Pid.

make_busy(Node, Opts, Data) ->
    case erlang:send({'__noone__', Node}, Data, Opts) of
	nosuspend -> nosuspend;
	_ -> make_busy(Node, Opts, Data)
    end.

unmake_busy(Pid) ->
    unlink(Pid),
    exit(Pid, bang).

do_busy_test(Node, Fun) ->
    Busy = make_busy(Node, 1000),
    {P, M} = spawn_monitor(Fun),
    receive after 100 -> ok end,
    Pinfo = process_info(P, [status, current_function]),
    unmake_busy(Busy),
    ?t:format("~p : ~p~n", [P, Pinfo]),
    case Pinfo of
	undefined ->
	    receive
		{'DOWN', M, process, P, Reason} ->
		    ?t:format("~p died with exit reason ~p~n", [P, Reason])
	    end,
	    ?t:fail(premature_death);
	_ ->
	    %% Don't match arity; it is different in debug and
	    %% optimized emulator
	    [{status, suspended},
	     {current_function, {erlang, bif_return_trap, _}}] = Pinfo,
	    receive
		{'DOWN', M, process, P, Reason} ->
		    ?t:format("~p died with exit reason ~p~n", [P, Reason]),
		    normal = Reason
	    end
    end.

remote_is_process_alive(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive,
	     [Pid]).

joey_killer(Pid) ->
    exit(Pid, joey_said_die),
    until(fun () -> false == remote_is_process_alive(Pid) end).

tail_joey_killer(Pid) ->
    exit(Pid, joey_said_die).

applied_joey_killer(Pid) ->
    apply(erlang, exit, [Pid, joey_said_die]),
    until(fun () -> false == remote_is_process_alive(Pid) end).

tail_applied_joey_killer(Pid) ->
    apply(erlang, exit, [Pid, joey_said_die]).

sink(Name) ->
    register(Name, self()),
    sink1().

sink1() ->
    receive
	_Any -> sink1()
    end.

lost_exit(doc) ->
    "Test that EXIT and DOWN messages send to another node are not lost if "
	"the distribution port is busy.";
lost_exit(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(lost_exit),

    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
		 "true" -> start_busy_dist_port_tracer();
		 _ -> false
	     end,

    Self = self(),
    Die = make_ref(),
    ?line R1 = spawn(fun () -> receive after infinity -> ok end end),
    ?line MR1 = erlang:monitor(process, R1),
    
    ?line {L1, ML1} = spawn_monitor(fun() ->
					    link(R1),
					    Self ! {self(), linked},
					    receive
						Die ->
						    exit(controlled_suicide)
					    end
				    end),

    ?line R2 = spawn(fun () ->
			     M = erlang:monitor(process, L1),
			     receive
				 {'DOWN', M, process, L1, R} ->
				     Self ! {self(), got_down_message, L1, R}
			     end
		     end),

    ?line receive {L1, linked} -> ok end,
    
    Busy = make_busy(Node, 2000),
    receive after 100 -> ok end,
    L1 ! Die,
    ?line receive
	      {'DOWN', ML1, process, L1, RL1} ->
		  ?line controlled_suicide = RL1
	  end,
    receive after 500 -> ok end,
    unmake_busy(Busy),

    ?line receive
	      {'DOWN', MR1, process, R1, RR1} ->
		  ?line controlled_suicide = RR1
	  end,
    
    ?line receive
	      {R2, got_down_message, L1, RR2} ->
		  ?line controlled_suicide = RR2
	  end,

    %% Done.
    ?line stop_busy_dist_port_tracer(Tracer),
    ?line stop_node(Node),
    ok.

dummy_waiter() ->
    receive
    after infinity ->
	    ok
    end.

link_to_dead(doc) ->
    ["Test that linking to a dead remote process gives an EXIT message ",
     "AND that the link is teared down."];
link_to_dead(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line {ok, Node} = start_node(link_to_dead),
%    ?line monitor_node(Node, true),
    ?line net_adm:ping(Node), %% Ts_cross_server workaround.
    ?line Pid = spawn(Node, ?MODULE, dead_process, []),
    receive
    after 5000 -> ok
    end,
    ?line link(Pid),
    ?line receive
	      {'EXIT', Pid, noproc} ->
		  ok;
	      Other ->
		  ?line test_server:fail({unexpected_message, Other})
	  after 5000 ->
		  ?line test_server:fail(nothing_received)
	  end,
    ?line {links, Links} = process_info(self(), links),
    ?line io:format("Pid=~p, links=~p", [Pid, Links]),
    ?line false = lists:member(Pid, Links),
    ?line stop_node(Node),
    ?line receive
	      Message ->
		  ?line test_server:fail({unexpected_message, Message})
	  after 3000 ->
		  ok
	  end,
    ok.
    
dead_process() ->
    erlang:error(die).

link_to_dead_new_node(doc) ->
    ["Test that linking to a pid on node that has gone and restarted gives ",
     "the correct EXIT message (OTP-2304)."];
link_to_dead_new_node(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),

    %% Start the node, get a Pid and stop the node again.
    ?line {ok, Node} = start_node(link_to_dead_new_node),
    ?line Pid = spawn(Node, ?MODULE, dead_process, []),
    ?line stop_node(Node),

    %% Start a new node with the same name.
    ?line {ok, Node} = start_node(link_to_dead_new_node),
    ?line link(Pid),
    ?line receive
	      {'EXIT', Pid, noproc} ->
		  ok;
	      Other ->
		  ?line test_server:fail({unexpected_message, Other})
	  after 5000 ->
		  ?line test_server:fail(nothing_received)
	  end,

    %% Make sure that the link wasn't created.
    ?line {links, Links} = process_info(self(), links),
    ?line io:format("Pid=~p, links=~p", [Pid, Links]),
    ?line false = lists:member(Pid, Links),
    ?line stop_node(Node),
    ?line receive
	      Message ->
		  ?line test_server:fail({unexpected_message, Message})
	  after 3000 ->
		  ok
	  end,
    ok.

applied_monitor_node(doc) ->
    "Test that monitor_node/2 works when applied.";
applied_monitor_node(Config) when is_list(Config) ->
    ?line NonExisting = list_to_atom("__non_existing__@" ++ hostname()),

    %% Tail-recursive call to apply (since the node is non-existing,
    %% there will be a trap).

    ?line true = tail_apply(erlang, monitor_node, [NonExisting, true]),
    ?line [{nodedown, NonExisting}] = test_server:messages_get(),

    %% Ordinary call (with trap).

    ?line true = apply(erlang, monitor_node, [NonExisting, true]),
    ?line [{nodedown, NonExisting}] = test_server:messages_get(),
    
    ok.

tail_apply(M, F, A) ->
    apply(M, F, A).

ref_port_roundtrip(doc) ->
    "Test that sending a port or reference to another node and back again "
	"doesn't correct them in any way.";
ref_port_roundtrip(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Port = open_port({spawn, efile}, []),
    ?line Ref = make_ref(),
    ?line {ok, Node} = start_node(ref_port_roundtrip),
    ?line net_adm:ping(Node),
    ?line Term = {Port, Ref},
    ?line io:format("Term before: ~p", [show_term(Term)]),
    ?line Pid = spawn_link(Node, ?MODULE, roundtrip, [Term]),
    ?line receive after 5000 -> ok end,
    ?line stop_node(Node),
    ?line receive
	      {'EXIT', Pid, {Port, Ref}} ->
		  ?line io:format("Term after: ~p", [show_term(Term)]),
		  ok;
	      Other ->
		  ?line io:format("Term after: ~p", [show_term(Term)]),
		  ?line test_server:fail({unexpected, Other})
	  after 10000 ->
		  ?line test_server:fail(timeout)
	  end,
    ok.

roundtrip(Term) ->
    exit(Term).

nil_roundtrip(doc) ->
    "Test that the smallest external term [] aka NIL can be sent to "
	"another node node and back again.";
nil_roundtrip(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line {ok, Node} = start_node(nil_roundtrip),
    ?line net_adm:ping(Node),
    ?line Pid = spawn_link(Node, ?MODULE, bounce, [self()]),
    ?line Pid ! [],
    ?line receive
	      [] ->
		  ?line receive
			    {'EXIT', Pid, []} ->
				?line stop_node(Node),
				ok
			end
	  end.

bounce(Dest) ->
    receive Msg ->
	    Dest ! Msg,
	    exit(Msg)
    end.

show_term(Term) ->
    binary_to_list(term_to_binary(Term)).

stop_dist(doc) ->
    ["Tests behaviour after net_kernel:stop (OTP-2586)."];
stop_dist(Config) when is_list(Config) ->
    ?line Str = os:cmd(atom_to_list(lib:progname())
		       ++ " -noshell -pa "
		       ++ ?config(data_dir, Config)
		       ++ " -s run"),
    %% The "true" may be followed by an error report, so ignore anything that
    %% follows it.
    ?line "true\n"++_ = Str,

    %% "May fail on FreeBSD due to differently configured name lookup - ask Arndt",
    %% if you can find him.

    ok.


trap_bif_1(doc) ->
    [""];
trap_bif_1(Config) when is_list(Config) ->
    ?line {true} = tr1(),
    ok.

trap_bif_2(doc) ->
    [""];
trap_bif_2(Config) when is_list(Config) ->
    ?line {true} = tr2(),
    ok.

trap_bif_3(doc) ->
    [""];
trap_bif_3(Config) when is_list(Config) ->
    ?line {hoo} = tr3(),
    ok.

tr1() ->
    ?line NonExisting = 'abc@boromir',
    ?line X = erlang:monitor_node(NonExisting, true),
    {X}.

tr2() ->
    ?line NonExisting = 'abc@boromir',
    ?line X = apply(erlang, monitor_node, [NonExisting, true]),
    {X}.

tr3() ->
    ?line NonExisting = 'abc@boromir',
    ?line X = {NonExisting, glirp} ! hoo,
    {X}.




% This has to be done by nodes with differrent cookies, otherwise global
% will connect nodes, which is correct, but makes it hard to test.
% * Start two nodes, n1 and n2. n2 with the dist_auto_connect once parameter
% * n2 pings n1 -> connection
% * check that they now know each other
% * Kill n1
% * Make sure n2 gets pang when pinging n1
% * restart n1
% * Make sure n2 *still gets pang*!
% * Ping n2 from n1 -> pong
% * n2 now also gets pong when pinging n1
% * disconnect n2 from n1
% * n2 gets pang when pinging n1
% * n2 forces connection by using net_kernel:connect_node (ovverrides)
% * n2 gets pong when pinging n1.
dist_auto_connect_once(doc) -> "Test the dist_auto_connect once kernel parameter";
dist_auto_connect_once(Config) when is_list(Config) ->
    ?line Sock = start_relay_node(dist_auto_connect_relay_node,[]),
    ?line NN = inet_rpc_nodename(Sock),
    ?line Sock2 = start_relay_node(dist_auto_connect_once_node,
				   "-kernel dist_auto_connect once"),
    ?line NN2 = inet_rpc_nodename(Sock2),
    ?line {ok,[]} = do_inet_rpc(Sock,erlang,nodes,[]),
    ?line {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    ?line {ok,[NN2]} = do_inet_rpc(Sock,erlang,nodes,[]),
    ?line {ok,[NN]} = do_inet_rpc(Sock2,erlang,nodes,[]),
    ?line [_,HostPartPeer] = string:tokens(atom_to_list(NN),"@"),
    ?line [_,MyHostPart] = string:tokens(atom_to_list(node()),"@"),
    % Give net_kernel a chance to change the state of the node to up to.
    ?line receive after 1000 -> ok end,
    case HostPartPeer of
	MyHostPart ->
	    ?line ok = stop_relay_node(Sock),
	    ?line {ok,pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]);
	_ ->
	    ?line {ok, true} = do_inet_rpc(Sock,net_kernel,disconnect,[NN2]),
	    receive
	    after 500 -> ok
	    end
    end,
    ?line {ok, []} = do_inet_rpc(Sock2,erlang,nodes,[]),
    Sock3 = case HostPartPeer of
		MyHostPart ->
		    ?line start_relay_node(dist_auto_connect_relay_node,[]);
		_ ->
		    Sock
	    end,
    ?line TS1 = timestamp(),
    ?line {ok, pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    ?line TS2 = timestamp(),
    RefT = net_kernel:connecttime() - 1000,
    ?line true = ((TS2 - TS1) < RefT),
    ?line TS3 = timestamp(),
    ?line {ok, true} = do_inet_rpc(Sock2,erlang,monitor_node,
				   [NN,true,[allow_passive_connect]]),
    ?line TS4 = timestamp(),
    ?line true = ((TS4 - TS3) > RefT),
    ?line {ok, pong} = do_inet_rpc(Sock3,net_adm,ping,[NN2]),
    ?line {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    ?line {ok, true} = do_inet_rpc(Sock3,net_kernel,disconnect,[NN2]),
    receive
    after 500 -> ok
    end,
    ?line {ok, pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    ?line {ok, true} = do_inet_rpc(Sock2,net_kernel,connect_node,[NN]),
    ?line {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    ?line stop_relay_node(Sock3),
    ?line stop_relay_node(Sock2).
    


%% Start a relay node and a lonely (dist_auto_connect never) node.
%% Lonely node pings relay node. That should fail. 
%% Lonely node connects to relay node with net_kernel:connect_node/1.
%% Result is sent here through relay node.
dist_auto_connect_never(Config) when is_list(Config) ->
    Self = self(),
    ?line {ok, RelayNode} = 
	start_node(dist_auto_connect_relay),
    ?line spawn(RelayNode, 
	      fun() ->
		      register(dist_auto_connect_relay, self()),
		      dist_auto_connect_relay(Self) 
	      end),
    ?line {ok, Handle} = dist_auto_connect_start(dist_auto_connect, never),
    ?line Result = 
	receive
	    {do_dist_auto_connect, ok} ->
		ok;
	    {do_dist_auto_connect, Error} ->
		{error, Error};
	    Other ->
		{error, Other}
	after 32000 ->
		timeout
	end,
    ?line stop_node(RelayNode),
    ?line Stopped =  dist_auto_connect_stop(Handle),
    ?line Junk = 
	receive 
	    {do_dist_auto_connect, _} = J -> 
		J 
	after 0 -> 
		ok 
	end,
    ?line {ok, ok, ok} = {Result, Stopped, Junk},
    ok.


do_dist_auto_connect([never]) ->
    Node = list_to_atom("dist_auto_connect_relay@" ++ hostname()),
    io:format("~p:do_dist_auto_connect([false]) Node=~p~n", 
	      [?MODULE, Node]),
    Ping = net_adm:ping(Node),
    io:format("~p:do_dist_auto_connect([false]) Ping=~p~n", 
	      [?MODULE, Ping]),
    Result = case Ping of
		 pang -> ok;
		 _ -> {error, Ping}
	     end,
    io:format("~p:do_dist_auto_connect([false]) Result=~p~n", 
	      [?MODULE, Result]),
    net_kernel:connect_node(Node),
    catch {dist_auto_connect_relay, Node} ! {do_dist_auto_connect, Result};
%    receive after 1000 -> ok end,
%    halt();

do_dist_auto_connect(Arg) ->
    io:format("~p:do_dist_auto_connect(~p)~n", 
	      [?MODULE, Arg]),
    receive after 10000 -> ok end,
    halt().
	    

dist_auto_connect_start(Name, Value) when is_atom(Name) ->
    dist_auto_connect_start(atom_to_list(Name), Value);
dist_auto_connect_start(Name, Value) when is_list(Name), is_atom(Value) ->
    Node = list_to_atom(lists:append([Name, "@", hostname()])),
    ModuleDir = filename:dirname(code:which(?MODULE)),
    ValueStr = atom_to_list(Value),
    Cookie = atom_to_list(erlang:get_cookie()),
    Cmd = lists:concat(
	    [%"xterm -e ",
	     atom_to_list(lib:progname()),
%	     " -noinput ",
	     " -detached ", 
	     long_or_short(), " ", Name,
	     " -setcookie ", Cookie,
	     " -pa ", ModuleDir,
	     " -s ", atom_to_list(?MODULE), 
	     " do_dist_auto_connect ", ValueStr,
	     " -kernel dist_auto_connect ", ValueStr]),
    io:format("~p:dist_auto_connect_start() cmd: ~p~n", [?MODULE, Cmd]),
    Port = open_port({spawn, Cmd}, [stream]),
    {ok, {Port, Node}}.


dist_auto_connect_stop({Port, Node}) ->
    Pid = spawn_link(fun() -> rpc:call(Node, erlang, halt, []) end),
    dist_auto_connect_stop(Port, Node, Pid, 5000).

dist_auto_connect_stop(Port, _Node, Pid, N) when is_integer(N), N =< 0 ->
    exit(Pid, normal),
    catch erlang:port_close(Port),
    Result = {error, node_not_down},
    io:format("~p:dist_auto_connect_stop() ~p~n", [?MODULE, Result]),
    Result;
dist_auto_connect_stop(Port, Node, Pid, N) when is_integer(N) ->
    case net_adm:ping(Node) of
	pong ->
	    receive after 100 -> ok end,
	    dist_auto_connect_stop(Port, Node, Pid, N-100);
	pang ->
	    exit(Pid, normal),
	    catch erlang:port_close(Port),
	    io:format("~p:dist_auto_connect_stop() ok~n", [?MODULE]),
	    ok
    end.


dist_auto_connect_relay(Parent) ->	    
    receive X ->
	    catch Parent ! X
    end,
    dist_auto_connect_relay(Parent).


dist_parallel_send(doc) ->
    [];
dist_parallel_send(suite) ->
    [];
dist_parallel_send(Config) when is_list(Config) ->
    ?line {ok, RNode} = start_node(dist_parallel_receiver),
    ?line {ok, SNode} = start_node(dist_parallel_sender),
    ?line WatchDog = spawn_link(
		       fun () ->
			       TRef = erlang:start_timer((?DEFAULT_TIMETRAP
							  div 2),
							 self(),
							 oops),
			       receive
				   {timeout, TRef, _ } ->
				       spawn(SNode,
					     fun () ->
						     abort(timeout)
					     end),
				       spawn(RNode,
					     fun () ->
						     abort(timeout)
					     end)
%%				       rpc:cast(SNode, erlang, halt,
%%						["Timetrap (sender)"]),
%%				       rpc:cast(RNode, erlang, halt,
%%						["Timetrap (receiver)"])
			       end
		       end),
    ?line MkSndrs = fun (Receiver) ->
			    lists:map(fun (_) ->
					      spawn_link(SNode,
							 ?MODULE,
							 dist_parallel_sender,
							 [self(),
							  Receiver,
							  1000])
				      end,
				      lists:seq(1, 64))
		    end,
    ?line SndrsStart = fun (Sndrs) ->
			       Parent = self(),
			       spawn_link(
				 SNode,
				 fun () ->
					 lists:foreach(fun (P) ->
							       P ! {go, Parent}
						       end,
						       Sndrs)
				 end)
		       end,
    ?line SndrsWait = fun (Sndrs) ->
			      lists:foreach(fun (P) ->
						    receive {P, done} -> ok end
					    end,
					    Sndrs)
		      end,
    ?line DPR = spawn_link(RNode, ?MODULE, dist_parallel_receiver, []),
    ?line Sndrs1 = MkSndrs(DPR),
    ?line SndrsStart(Sndrs1),
    ?line SndrsWait(Sndrs1),
    ?line unlink(DPR),
    ?line exit(DPR, bang),

    ?line DEPR = spawn_link(RNode, ?MODULE, dist_evil_parallel_receiver, []),
    ?line Sndrs2 = MkSndrs(DEPR),
    ?line SndrsStart(Sndrs2),
    ?line SndrsWait(Sndrs2),
    ?line unlink(DEPR),
    ?line exit(DEPR, bang),

    ?line unlink(WatchDog),
    ?line exit(WatchDog, bang),

    ?line stop_node(RNode),
    ?line stop_node(SNode),

    ?line ok.

do_dist_parallel_sender(Parent, _Receiver, 0) ->
    Parent ! {self(), done};
do_dist_parallel_sender(Parent, Receiver, N) ->
    Receiver ! {self(), "Some data"},
    do_dist_parallel_sender(Parent, Receiver, N-1).

dist_parallel_sender(Parent, Receiver, N) ->
    receive {go, Parent} -> ok end,
    do_dist_parallel_sender(Parent, Receiver, N).

dist_parallel_receiver() ->
    receive {_Sender, _Data} -> ok end,
    dist_parallel_receiver().

dist_evil_parallel_receiver() ->
    receive {Sender, _Data} -> ok end,
    net_kernel:disconnect(node(Sender)),
    dist_evil_parallel_receiver().

atom_roundtrip(Config) when is_list(Config) ->
    ?line AtomData = atom_data(),
    ?line verify_atom_data(AtomData),
    ?line {ok, Node} = start_node(Config),
    ?line do_atom_roundtrip(Node, AtomData),
    ?line stop_node(Node),
    ?line ok.

atom_roundtrip_r15b(Config) when is_list(Config) ->
    case ?t:is_release_available("r15b") of
	true ->
	    ?line AtomData = atom_data(),
	    ?line verify_atom_data(AtomData),
	    ?line {ok, Node} = start_node(Config, [], "r15b"),
	    ?line do_atom_roundtrip(Node, AtomData),
	    ?line stop_node(Node),
	    ?line ok;
	false ->
	    ?line {skip,"No OTP R15B available"}
    end.

unicode_atom_roundtrip(Config) when is_list(Config) ->
    ?line AtomData = unicode_atom_data(),
    ?line verify_atom_data(AtomData),
    ?line {ok, Node} = start_node(Config),
    ?line do_atom_roundtrip(Node, AtomData),
    ?line stop_node(Node),
    ?line ok.

do_atom_roundtrip(Node, AtomData) ->
    ?line Parent = self(),
    ?line Proc = spawn_link(Node, fun () -> verify_atom_data_loop(Parent) end),
    ?line Proc ! {self(), AtomData},
    ?line receive {Proc, AD1} -> AtomData = AD1 end,
    ?line Proc ! {self(), AtomData},
    ?line receive {Proc, AD2} -> AtomData = AD2 end,
    ?line RevAtomData = lists:reverse(AtomData),
    ?line Proc ! {self(), RevAtomData},
    ?line receive {Proc, RAD1} -> RevAtomData = RAD1 end,
    ?line unlink(Proc),
    ?line exit(Proc, bang),
    ?line ok.

verify_atom_data_loop(From) ->
    receive
	{From, AtomData} ->
	    verify_atom_data(AtomData),
	    From ! {self(), AtomData},
	    verify_atom_data_loop(From)
    end.

atom_data() ->
    lists:map(fun (N) ->
		      ATxt = "a"++integer_to_list(N),
		      {list_to_atom(ATxt), ATxt}
	      end,
	      lists:seq(1, 2000)).

verify_atom_data(AtomData) ->
    lists:foreach(fun ({Atom, AtomTxt}) when is_atom(Atom) ->
			  AtomTxt = atom_to_list(Atom);
		      ({PPR, AtomTxt}) ->
			  % Pid, Port, or Ref
			  AtomTxt = atom_to_list(node(PPR))
		  end,
		  AtomData).

uc_atom_tup(ATxt) ->
    Atom = string_to_atom(ATxt),
    ATxt = atom_to_list(Atom),
    {Atom, ATxt}.

uc_pid_tup(ATxt) ->
    ATxtExt = string_to_atom_ext(ATxt),
    Pid = mk_pid({ATxtExt, 1}, 4711,17),
    true = is_pid(Pid),
    Atom = node(Pid),
    true = is_atom(Atom),
    ATxt = atom_to_list(Atom),
    {Pid, ATxt}.

uc_port_tup(ATxt) ->
    ATxtExt = string_to_atom_ext(ATxt),
    Port = mk_port({ATxtExt, 2}, 4711),
    true = is_port(Port),
    Atom = node(Port),
    true = is_atom(Atom),
    ATxt = atom_to_list(Atom),
    {Port, ATxt}.

uc_ref_tup(ATxt) ->
    ATxtExt = string_to_atom_ext(ATxt),
    Ref = mk_ref({ATxtExt, 3}, [4711,17, 4711]),
    true = is_reference(Ref),
    Atom = node(Ref),
    true = is_atom(Atom),
    ATxt = atom_to_list(Atom),
    {Ref, ATxt}.


unicode_atom_data() ->
    [uc_pid_tup(lists:seq(16#1f600, 16#1f600+249) ++ "@host"),
     uc_pid_tup(lists:seq(16#1f600, 16#1f600+30) ++ "@host"),
     uc_port_tup(lists:seq(16#1f600, 16#1f600+249) ++ "@host"),
     uc_port_tup(lists:seq(16#1f600, 16#1f600+30) ++ "@host"),
     uc_ref_tup(lists:seq(16#1f600, 16#1f600+249) ++ "@host"),
     uc_ref_tup(lists:seq(16#1f600, 16#1f600+30) ++ "@host"),
     uc_atom_tup(lists:seq(16#1f600, 16#1f600+254)),
     uc_atom_tup(lists:seq(16#1f600, 16#1f600+63)),
     uc_atom_tup(lists:seq(0, 254)),
     uc_atom_tup(lists:seq(100, 163)),
     uc_atom_tup(lists:seq(200, 354)),
     uc_atom_tup(lists:seq(200, 263)),
     uc_atom_tup(lists:seq(2000, 2254)),
     uc_atom_tup(lists:seq(2000, 2063)),
     uc_atom_tup(lists:seq(65500, 65754)),
     uc_atom_tup(lists:seq(65500, 65563))
     | lists:map(fun (N) ->
			 uc_atom_tup(lists:seq(64000+N, 64254+N))
		 end,
		 lists:seq(1, 2000))].

contended_atom_cache_entry(Config) when is_list(Config) ->
    contended_atom_cache_entry_test(Config, latin1).

contended_unicode_atom_cache_entry(Config) when is_list(Config) ->
    contended_atom_cache_entry_test(Config, unicode).

contended_atom_cache_entry_test(Config, Type) ->
    ?line TestServer = self(),
    ?line ProcessPairs = 10,
    ?line Msgs = 100000,
    ?line {ok, SNode} = start_node(Config),
    ?line {ok, RNode} = start_node(Config),
    ?line Success = make_ref(),
    ?line spawn_link(
	    SNode,
	    fun () ->
		    erts_debug:set_internal_state(available_internal_state,
						  true),
		    Master = self(),
		    CIX = get_cix(),
		    TestAtoms = case Type of
				    latin1 ->
					get_conflicting_atoms(CIX,
							      ProcessPairs);
				    unicode ->
					get_conflicting_unicode_atoms(CIX,
								      ProcessPairs)
				end,
		    io:format("Testing with the following atoms all using "
			      "cache index ~p:~n ~w~n",
			      [CIX, TestAtoms]),
		    Ps = lists:map(
			   fun (A) ->
				   Ref = make_ref(),
				   R = spawn_link(
					 RNode,
					 fun () ->
						 Atom = receive
							    {Ref, txt, ATxt} ->
								case Type of
								    latin1 ->
									list_to_atom(ATxt);
								    unicode ->
									string_to_atom(ATxt)
								end
							end,
						 receive_ref_atom(Ref,
								  Atom,
								  Msgs),
						 Master ! {self(), success}
					 end),
				   S = spawn_link(
					 SNode,
					 fun () ->
						 receive go -> ok end,
						 R ! {Ref,
						      txt,
						      atom_to_list(A)},
						 send_ref_atom(R, Ref, A, Msgs)
					 end),
				   {S, R}
			   end,
			   TestAtoms),
		    lists:foreach(fun ({S, _}) ->
					  S ! go
				  end,
				  Ps),
		    lists:foreach(fun ({_, R}) ->
					  receive {R, success} -> ok end
				  end,
				  Ps),
		    TestServer ! Success
	    end),
    ?line receive
	      Success ->
		  ok
	  end,
    ?line stop_node(SNode),
    ?line stop_node(RNode),
    ?line ok.

send_ref_atom(_To, _Ref, _Atom, 0) ->
    ok;
send_ref_atom(To, Ref, Atom, N) ->
    To ! {Ref, Atom},
    send_ref_atom(To, Ref, Atom, N-1).

receive_ref_atom(_Ref, _Atom, 0) ->
    ok;
receive_ref_atom(Ref, Atom, N) ->
    receive
	{Ref, Value} ->
	    Atom = Value
    end,
    receive_ref_atom(Ref, Atom, N-1).
      
get_cix() ->
    get_cix(1000).

get_cix(CIX) when is_integer(CIX), CIX < 0 ->
    get_cix(0);
get_cix(CIX) when is_integer(CIX) ->
    get_cix(CIX,
	    unwanted_cixs(),
	    erts_debug:get_internal_state(max_atom_out_cache_index)).

get_cix(CIX, Unwanted, MaxCIX) when CIX > MaxCIX ->
    get_cix(0, Unwanted, MaxCIX);
get_cix(CIX, Unwanted, MaxCIX) ->
    case lists:member(CIX, Unwanted) of
	true -> get_cix(CIX+1, Unwanted, MaxCIX);
	false -> CIX
    end.

unwanted_cixs() ->
    lists:map(fun (Node) ->
		      erts_debug:get_internal_state({atom_out_cache_index,
						     Node})
	      end,
	      nodes()).
    
    
get_conflicting_atoms(_CIX, 0) ->
    [];
get_conflicting_atoms(CIX, N) ->
    {A, B, C} = now(),
    Atom = list_to_atom("atom" ++ integer_to_list(A*1000000000000
						  + B*1000000
						  + C)),
    case erts_debug:get_internal_state({atom_out_cache_index, Atom}) of
	CIX ->
	    [Atom|get_conflicting_atoms(CIX, N-1)];
	_ ->
	    get_conflicting_atoms(CIX, N)
    end.

get_conflicting_unicode_atoms(_CIX, 0) ->
    [];
get_conflicting_unicode_atoms(CIX, N) ->
    {A, B, C} = now(),
    Atom = string_to_atom([16#1f608] ++ "atom" ++ integer_to_list(A*1000000000000
								  + B*1000000
								  + C)),
    case erts_debug:get_internal_state({atom_out_cache_index, Atom}) of
	CIX ->
	    [Atom|get_conflicting_unicode_atoms(CIX, N-1)];
	_ ->
	    get_conflicting_unicode_atoms(CIX, N)
    end.

-define(COOKIE, '').
-define(DOP_LINK,		1).
-define(DOP_SEND,		2).
-define(DOP_EXIT,		3).
-define(DOP_UNLINK,		4).
-define(DOP_REG_SEND,		6).
-define(DOP_GROUP_LEADER,	7).
-define(DOP_EXIT2,		8).

-define(DOP_SEND_TT,		12).
-define(DOP_EXIT_TT,		13).
-define(DOP_REG_SEND_TT,	16).
-define(DOP_EXIT2_TT,		18).

-define(DOP_MONITOR_P,		19).
-define(DOP_DEMONITOR_P,	20).
-define(DOP_MONITOR_P_EXIT,	21).

start_monitor(Offender,P) ->
    ?line Parent = self(),
    ?line Q = spawn(Offender,
		    fun () -> 
			    Ref = erlang:monitor(process,P),
			    Parent ! {self(),ref,Ref},
			    receive
				just_stay_alive -> ok
			    end
		    end),
    ?line Ref = receive
		    {Q,ref,R} ->
			R
		after  5000 ->
			error
		end,
    io:format("Ref is ~p~n",[Ref]),
    ok.
start_link(Offender,P) ->
    ?line Parent = self(),
    ?line Q = spawn(Offender,
		    fun () ->
			    process_flag(trap_exit,true),
			    link(P),
			    Parent ! {self(),ref,P},
			    receive
				just_stay_alive -> ok
			    end
		    end),
    ?line Ref = receive
		    {Q,ref,R} ->
			R
		after  5000 ->
			error
		end,
    io:format("Ref is ~p~n",[Ref]),
    ok.

bad_dist_structure(suite) ->
    [];
bad_dist_structure(doc) ->
    ["Test dist messages with valid structure (binary to term ok) but malformed"
     "control content"];
bad_dist_structure(Config) when is_list(Config) ->
    %process_flag(trap_exit,true),
    ODog = ?config(watchdog, Config),
    ?t:timetrap_cancel(ODog),
    Dog = ?t:timetrap(?t:seconds(15)),

    ?line {ok, Offender} = start_node(bad_dist_structure_offender),
    ?line {ok, Victim} = start_node(bad_dist_structure_victim),
    ?line start_node_monitors([Offender,Victim]),
    ?line Parent = self(),
    ?line P = spawn(Victim,
		    fun () ->
			    process_flag(trap_exit,true),
			    Parent ! {self(), started},
			    receive check_msgs -> ok end,
			    bad_dist_struct_check_msgs([one,
							two]),
			    Parent ! {self(), messages_checked},
			    receive done -> ok end
		    end),
    ?line receive {P, started} -> ok end,
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line verify_up(Offender, Victim),
    ?line true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    ?line start_monitor(Offender,P),
    ?line P ! one,
    ?line send_bad_structure(Offender, P,{?DOP_MONITOR_P_EXIT,'replace',P,normal},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_monitor(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_MONITOR_P_EXIT,'replace',P,normal,normal},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_link(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_LINK},0),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_link(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_UNLINK,'replace'},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_link(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_UNLINK,'replace',make_ref()},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_link(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_UNLINK,make_ref(),P},0),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_link(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_UNLINK,normal,normal},0),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_monitor(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_MONITOR_P,'replace',P},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_monitor(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_MONITOR_P,'replace',P,normal},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_monitor(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_DEMONITOR_P,'replace',P},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line start_monitor(Offender,P),
    ?line send_bad_structure(Offender, P,{?DOP_DEMONITOR_P,'replace',P,normal},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_EXIT,'replace',P},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_EXIT,make_ref(),normal,normal},0),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_EXIT_TT,'replace',token,P},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_EXIT_TT,make_ref(),token,normal,normal},0),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_EXIT2,'replace',P},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_EXIT2,make_ref(),normal,normal},0),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_EXIT2_TT,'replace',token,P},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_EXIT2_TT,make_ref(),token,normal,normal},0),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace'},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace','atomic'},2),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace',P},0),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_REG_SEND_TT,'replace','',name},2,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_REG_SEND_TT,'replace','',name,token},0,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace',''},2,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',P},0,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',name},0,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',name,{token}},2,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_SEND_TT,'',P},0,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_SEND_TT,'',name,token},0,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_SEND,''},0,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_SEND,'',name},0,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line send_bad_structure(Offender, P,{?DOP_SEND,'',P,{token}},0,{message}),
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line P ! two,
    ?line P ! check_msgs,
    ?line receive 
	      {P, messages_checked} -> ok 
	  after 5000 ->
		  exit(victim_is_dead)
	  end,

    ?line {message_queue_len, 0}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),
    
    ?line unlink(P),
    ?line P ! done,
    ?line stop_node(Offender),
    ?line stop_node(Victim),
    ?t:timetrap_cancel(Dog),
    ok.



bad_dist_ext_receive(Config) when is_list(Config) ->
    ?line {ok, Offender} = start_node(bad_dist_ext_receive_offender),
    ?line {ok, Victim} = start_node(bad_dist_ext_receive_victim),
    ?line start_node_monitors([Offender,Victim]),

    ?line Parent = self(),

    ?line P = spawn_link(Victim,
			 fun () ->
				 Parent ! {self(), started},
				 receive check_msgs -> ok end,
				 bad_dist_ext_check_msgs([one,
							  two,
							  three]),
				 Parent ! {self(), messages_checked},
				 receive done -> ok end
			 end),

    ?line receive {P, started} -> ok end,
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line verify_up(Offender, Victim),
    ?line true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    ?line P ! one,
    ?line send_bad_msg(Offender, P),
    ?line P ! two,
    ?line verify_down(Offender, connection_closed, Victim, killed),
    ?line {message_queue_len, 2}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    ?line Suspended = make_ref(),
    ?line S = spawn(Victim,
		     fun () ->
			     erlang:suspend_process(P),
			     Parent ! Suspended,
			     receive after infinity -> ok end
		     end),
    ?line MS = erlang:monitor(process, S),
    ?line receive Suspended -> ok end,
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line verify_up(Offender, Victim),
    ?line true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    ?line send_bad_msgs(Offender, P, 5),
    ?line true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    ?line P ! three,
    ?line send_bad_msgs(Offender, P, 5),

    %% Make sure bad msgs has reached Victim
    ?line rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    ?line verify_still_up(Offender, Victim),
    ?line {message_queue_len, 13}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),
    
    ?line exit(S, bang),
    ?line receive {'DOWN', MS, process, S, bang} -> ok end,
    ?line verify_down(Offender, connection_closed, Victim, killed),
    ?line {message_queue_len, 3}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    ?line P ! check_msgs,
    ?line receive {P, messages_checked} -> ok end,

    ?line {message_queue_len, 0}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),
    
    ?line P ! done,
    ?line unlink(P),
    ?line verify_no_down(Offender, Victim),
    ?line stop_node(Offender),
    ?line stop_node(Victim).


bad_dist_ext_process_info(Config) when is_list(Config) ->
    ?line {ok, Offender} = start_node(bad_dist_ext_process_info_offender),
    ?line {ok, Victim} = start_node(bad_dist_ext_process_info_victim),
    ?line start_node_monitors([Offender,Victim]),

    ?line Parent = self(),
    ?line P = spawn_link(Victim,
			 fun () ->
				 Parent ! {self(), started},
				 receive check_msgs -> ok end,
				 bad_dist_ext_check_msgs([one, two]),
				 Parent ! {self(), messages_checked},
				 receive done -> ok end
			 end),

    ?line receive {P, started} -> ok end,
    ?line P ! one,
    
    ?line Suspended = make_ref(),
    ?line S = spawn(Victim,
		     fun () ->
			     erlang:suspend_process(P),
			     Parent ! Suspended,
			     receive after infinity -> ok end
		     end),

    ?line receive Suspended -> ok end,
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line verify_up(Offender, Victim),
    ?line send_bad_msgs(Offender, P, 5),

    ?line P ! two,
    ?line send_bad_msgs(Offender, P, 5),

    %% Make sure bad msgs has reached Victim
    ?line rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    ?line verify_still_up(Offender, Victim),
    ?line {message_queue_len, 12}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),
    ?line verify_still_up(Offender, Victim),
    ?line [{message_queue_len, 2},
	   {messages, [one, two]}]
	= rpc:call(Victim, erlang, process_info, [P, [message_queue_len,
						      messages]]),
    ?line verify_down(Offender, connection_closed, Victim, killed),

    ?line P ! check_msgs,
    ?line exit(S, bang),
    ?line receive {P, messages_checked} -> ok end,

    ?line {message_queue_len, 0}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    ?line P ! done,
    ?line unlink(P),
    ?line verify_no_down(Offender, Victim),
    ?line stop_node(Offender),
    ?line stop_node(Victim).

bad_dist_ext_control(Config) when is_list(Config) ->
    ?line {ok, Offender} = start_node(bad_dist_ext_control_offender),
    ?line {ok, Victim} = start_node(bad_dist_ext_control_victim),
    ?line start_node_monitors([Offender,Victim]),

    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line verify_up(Offender, Victim),
    ?line send_bad_dhdr(Offender, Victim),
    ?line verify_down(Offender, connection_closed, Victim, killed),

    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line verify_up(Offender, Victim),
    ?line send_bad_ctl(Offender, Victim),
    ?line verify_down(Offender, connection_closed, Victim, killed),

    ?line verify_no_down(Offender, Victim),
    ?line stop_node(Offender),
    ?line stop_node(Victim).

bad_dist_ext_connection_id(Config) when is_list(Config) ->
    ?line {ok, Offender} = start_node(bad_dist_ext_connection_id_offender),
    ?line {ok, Victim} = start_node(bad_dist_ext_connection_id_victim),
    ?line start_node_monitors([Offender,Victim]),

    ?line Parent = self(),
    ?line P = spawn_link(Victim,
			 fun () ->
				 Parent ! {self(), started},
				 receive check_msgs -> ok end,
				 bad_dist_ext_check_msgs([]),
				 Parent ! {self(), messages_checked},
				 receive done -> ok end
			 end),

    ?line receive {P, started} -> ok end,
    ?line Suspended = make_ref(),
    ?line S = spawn(Victim,
		    fun () ->
			    erlang:suspend_process(P),
			    Parent ! Suspended,
			    receive after infinity -> ok end
		    end),
    ?line MS = erlang:monitor(process, S),
    ?line receive Suspended -> ok end,
    ?line pong = rpc:call(Victim, net_adm, ping, [Offender]),
    ?line verify_up(Offender, Victim),
    ?line send_bad_msg(Offender, P),

    %% Make sure bad msg has reached Victim
    ?line rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    ?line {message_queue_len, 1}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    ?line true = rpc:call(Offender, net_kernel, disconnect, [Victim]),
    ?line verify_down(Offender, disconnect, Victim, connection_closed),
    ?line pong = rpc:call(Offender, net_adm, ping, [Victim]),

    ?line verify_up(Offender, Victim),
    %% We have a new connection between Offender and Victim, bad message
    %% should not bring it down.

    ?line {message_queue_len, 1}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    ?line exit(S, bang),
    ?line receive {'DOWN', MS, process, S, bang} -> ok end,
    %% Wait for a while (if the connection is taken down it might take a
    %% while).
    ?line receive after 2000 -> ok end,
    ?line verify_still_up(Offender, Victim),

    ?line P ! check_msgs,
    ?line receive {P, messages_checked} -> ok end,

    ?line {message_queue_len, 0}
	= rpc:call(Victim, erlang, process_info, [P, message_queue_len]),
    
    ?line verify_still_up(Offender, Victim),
    ?line P ! done,
    ?line unlink(P),
    ?line verify_no_down(Offender, Victim),
    ?line stop_node(Offender),
    ?line stop_node(Victim).


bad_dist_struct_check_msgs([]) ->
    receive
	Msg ->
	    exit({unexpected_message, Msg})
    after 0 ->
	    ok
    end;
bad_dist_struct_check_msgs([M|Ms]) ->
    receive
	{'EXIT',_,_} = EM ->
	    io:format("Ignoring exit message: ~p~n",[EM]),
	    bad_dist_struct_check_msgs([M|Ms]);
	Msg ->
	    M = Msg,
	    bad_dist_struct_check_msgs(Ms)
    end.
bad_dist_ext_check_msgs([]) ->
    receive
	Msg ->
	    exit({unexpected_message, Msg})
    after 0 ->
	    ok
    end;
bad_dist_ext_check_msgs([M|Ms]) ->
    receive
	Msg ->
	    M = Msg,
	    bad_dist_ext_check_msgs(Ms)
    end.
    

dport_reg_send(Node, Name, Msg) ->
    DPrt = case dport(Node) of
	       undefined ->
		   pong = net_adm:ping(Node),
		   dport(Node);
	       Prt ->
		   Prt
	   end,
    port_command(DPrt, [dmsg_hdr(),
			dmsg_ext({?DOP_REG_SEND,
				  self(),
				  ?COOKIE,
				  Name}),
			dmsg_ext(Msg)]).


dport_send(To, Msg) ->
    Node = node(To),
    DPrt = case dport(Node) of
	       undefined ->
		   pong = net_adm:ping(Node),
		   dport(Node);
	       Prt ->
		   Prt
	   end,
    port_command(DPrt, [dmsg_hdr(),
			dmsg_ext({?DOP_SEND,
				  ?COOKIE,
				  To}),
			dmsg_ext(Msg)]).
send_bad_structure(Offender,Victim,Bad,WhereToPutSelf) ->
    send_bad_structure(Offender,Victim,Bad,WhereToPutSelf,[]).
send_bad_structure(Offender,Victim,Bad,WhereToPutSelf,PayLoad) ->
    Parent = self(),
    Done = make_ref(),
    spawn(Offender,
	  fun () ->
		  Node = node(Victim),
		  pong = net_adm:ping(Node),
		  DPrt = dport(Node),
		  Bad1 = case WhereToPutSelf of
			     0 -> 
				 Bad;
			     N when N > 0 ->
				 setelement(N,Bad,self())
			 end,
		  DData = [dmsg_hdr(),
			   dmsg_ext(Bad1)] ++
		      case PayLoad of
			  [] -> [];
			  _Other -> [dmsg_ext(PayLoad)]
		      end,
		  port_command(DPrt, DData),
		  Parent ! {DData,Done}
	  end),
    receive 
	{WhatSent,Done} -> 
	    io:format("Offender sent ~p~n",[WhatSent]),
	    ok
    after 5000 ->
	    exit(unable_to_send)
    end.
   

%% send_bad_msgs():
%% Send a valid distribution header and control message
%% but an invalid message. This invalid message will be
%% enqueued in the receivers message queue.
send_bad_msg(BadNode, To) ->
    send_bad_msgs(BadNode, To, 1).

send_bad_msgs(BadNode, To, Repeat) when is_atom(BadNode),
					is_pid(To),
					is_integer(Repeat) ->
    Parent = self(),
    Done = make_ref(),
    spawn_link(BadNode,
	       fun () ->
		       Node = node(To),
		       pong = net_adm:ping(Node),
		       DPrt = dport(Node),
		       DData = [dmsg_hdr(),
				dmsg_ext({?DOP_SEND, ?COOKIE, To}),
				dmsg_bad_atom_cache_ref()],
		       repeat(fun () -> port_command(DPrt, DData) end, Repeat),
		       Parent ! Done
	       end),
    receive Done -> ok end.

%% send_bad_ctl():
%% Send a valid distribution header but an invalid control message.
send_bad_ctl(BadNode, ToNode) when is_atom(BadNode), is_atom(ToNode) ->
    Parent = self(),
    Done = make_ref(),
    spawn_link(BadNode,
	       fun () ->
		       pong = net_adm:ping(ToNode),
		       %% We creat a valid ctl msg and replace an
		       %% atom with an invalid atom cache reference
		       <<131,Replace/binary>> = term_to_binary(replace),
		       Ctl = dmsg_ext({?DOP_REG_SEND,
				       self(),
				       ?COOKIE,
				       replace}),
		       CtlBeginSize = size(Ctl) - size(Replace),
		       <<CtlBegin:CtlBeginSize/binary, Replace/binary>> = Ctl,
		       port_command(dport(ToNode),
				    [dmsg_fake_hdr2(),
				     CtlBegin,
				     dmsg_bad_atom_cache_ref(),
				     dmsg_ext({a, message})]),
		       Parent ! Done
	       end),
    receive Done -> ok end.

%% send_bad_dhr():
%% Send an invalid distribution header
send_bad_dhdr(BadNode, ToNode) when is_atom(BadNode), is_atom(ToNode) ->
    Parent = self(),
    Done = make_ref(),
    spawn_link(BadNode,
	       fun () ->
		       pong = net_adm:ping(ToNode),
		       port_command(dport(ToNode), dmsg_bad_hdr()),
		       Parent ! Done
	       end),
    receive Done -> ok end.

dport(Node) when is_atom(Node) ->
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> true;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end,
    erts_debug:get_internal_state({dist_port, Node}).

dmsg_hdr() ->
    [131, % Version Magic
     $D,  % Dist header
     0].  % No atom cache referenses

dmsg_bad_hdr() ->
    [131, % Version Magic
     $D,  % Dist header
     255].  % 255 atom references
    

%% dmsg_fake_hdr1() ->
%%     A = <<"fake header atom 1">>,
%%     [131, % Version Magic
%%      $D, 1, 16#8, 0, size(A), A]. % Fake header

dmsg_fake_hdr2() ->
    A1 = <<"fake header atom 1">>,
    A2 = <<"atom 2">>,
    A3 = <<"atom 3">>,
    [131, % Version Magic
     $D,
     3,
     16#88, 16#08, % Flags
     0, size(A1), A1,
     1, size(A2), A2,
     2, size(A3), A3].

dmsg_ext(Term) ->	
    <<131, Res/binary>> = term_to_binary(Term),
    Res.

dmsg_bad_atom_cache_ref() ->
    [$R, 137].

%%% Utilities

timestamp() ->
    {A,B,C} = erlang:now(),
    (C div 1000) + (B * 1000) + (A * 1000000000).

start_node(X) ->
    start_node(X, [], []).

start_node(X, Y) ->
    start_node(X, Y, []).

start_node(Name, Args, Rel) when is_atom(Name), is_list(Rel) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Cookie = atom_to_list(erlang:get_cookie()),
    RelArg = case Rel of
		 [] -> [];
		 _ -> [{erl,[{release,Rel}]}]
	     end,
    test_server:start_node(Name, slave, 
			   [{args,
			     Args++" -setcookie "++Cookie++" -pa \""++Pa++"\""}
			    | RelArg]);
start_node(Config, Args, Rel) when is_list(Config), is_list(Rel) ->
    Name = list_to_atom((atom_to_list(?MODULE)
			 ++ "-"
			 ++ atom_to_list(?config(testcase, Config))
			 ++ "-"
			 ++ integer_to_list(timestamp()))),
    start_node(Name, Args, Rel).

stop_node(Node) ->
    test_server:stop_node(Node).

freeze_node(Node, MS) ->
    Own = 300,
    DoingIt = make_ref(),
    Freezer = self(),
    spawn_link(Node,
	       fun () ->
		       erts_debug:set_internal_state(available_internal_state,
						     true),
		       dport_send(Freezer, DoingIt),
		       receive after Own -> ok end,
		       erts_debug:set_internal_state(block, MS+Own)
	       end),
    receive DoingIt -> ok end,
    receive after Own -> ok end.

inet_rpc_nodename({N,H,_Sock}) ->
    list_to_atom(N++"@"++H).

do_inet_rpc({_,_,Sock},M,F,A) ->
    Bin = term_to_binary({M,F,A}),
    gen_tcp:send(Sock,Bin),
    case gen_tcp:recv(Sock,0) of
	{ok, Bin2} ->
	    T = binary_to_term(Bin2),
	    {ok,T};
	Else ->
	    {error, Else}
    end.

inet_rpc_server([Host, PortList]) ->
    Port = list_to_integer(PortList),
    {ok, Sock} = gen_tcp:connect(Host, Port,[binary, {packet, 4}, 
                                        {active, false}]),
    inet_rpc_server_loop(Sock).

inet_rpc_server_loop(Sock) ->
    case gen_tcp:recv(Sock,0) of
	{ok, Bin} ->
	    {M,F,A} = binary_to_term(Bin),
	    Res = (catch apply(M,F,A)),
	    RB = term_to_binary(Res),
	    gen_tcp:send(Sock,RB),
	    inet_rpc_server_loop(Sock);
	_ ->
	    erlang:halt()
    end.
				 

start_relay_node(Node, Args) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Cookie = "NOT"++atom_to_list(erlang:get_cookie()),
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, 4}, 
                                        {active, false}]),
    {ok, Port} = inet:port(LSock),
    {ok, Host} = inet:gethostname(),
    RunArg = "-run " ++ atom_to_list(?MODULE) ++ " inet_rpc_server " ++
	Host ++ " " ++ integer_to_list(Port),
    {ok, NN} = 
	test_server:start_node(Node, peer, 
			       [{args, Args ++
				 " -setcookie "++Cookie++" -pa "++Pa++" "++
				 RunArg}]),
    [N,H] = string:tokens(atom_to_list(NN),"@"),
    {ok, Sock} = gen_tcp:accept(LSock),
    pang = net_adm:ping(NN),
    {N,H,Sock}.

stop_relay_node({N,H,Sock}) ->
    catch do_inet_rpc(Sock,erlang,halt,[]),
    catch gen_tcp:close(Sock),
    wait_dead(N,H,10).

wait_dead(N,H,0) ->
    {error,{not_dead,N,H}};
wait_dead(N,H,X) ->
    case erl_epmd:port_please(N,H) of
	{port,_,_} ->
	    receive 
	    after 1000 ->
		    ok
	    end,
	    wait_dead(N,H,X-1);
	noport ->
	    ok;
	Else ->
	    {error, {unexpected, Else}}
    end.
    

start_node_monitors(Nodes) ->
    Master = self(),
    lists:foreach(fun (Node) ->
			  spawn(Node,
				fun () ->
					node_monitor(Master)
				end)
		  end,
		  Nodes),
    ok.

node_monitor(Master) ->
    Opts = [nodedown_reason,{node_type,all}],
    Nodes0 = nodes(connected),
    net_kernel:monitor_nodes(true, Opts),
    Nodes1 = nodes(connected),
    case lists:sort(Nodes0) == lists:sort(Nodes1) of
	true ->
	    lists:foreach(fun (Node) ->
				  Master ! {nodeup, node(), Node}
			  end,
			  Nodes0),
	    ?t:format("~p ~p: ~p~n", [node(), erlang:now(), Nodes0]),
	    node_monitor_loop(Master);
	false ->
	    net_kernel:monitor_nodes(false, Opts),
	    flush_node_changes(),
	    node_monitor(Master)
    end.

flush_node_changes() ->
    receive
	{NodeChange, _Node, _InfoList} when NodeChange == nodeup;
					    NodeChange == nodedown ->
	    flush_node_changes()
    after 0 ->
	    ok
    end.

node_monitor_loop(Master) ->
    receive
	{nodeup, Node, _InfoList} = Msg ->
	    Master ! {nodeup, node(), Node},
	    ?t:format("~p ~p: ~p~n", [node(), erlang:now(), Msg]),
	    node_monitor_loop(Master);
	{nodedown, Node, InfoList} = Msg ->
	    Reason = case lists:keysearch(nodedown_reason, 1, InfoList) of
			 {value, {nodedown_reason, R}} -> R;
			 _ -> undefined
		     end,
	    Master ! {nodedown, node(), Node, Reason},
	    ?t:format("~p ~p: ~p~n", [node(), erlang:now(), Msg]),
	    node_monitor_loop(Master)
    end.

verify_up(A, B) ->
    receive {nodeup, A, B} -> ok end,
    receive {nodeup, B, A} -> ok end.

verify_still_up(A, B) ->
    true = lists:member(B, rpc:call(A, erlang, nodes, [connected])),
    true = lists:member(A, rpc:call(B, erlang, nodes, [connected])),
    verify_no_down(A, B).

verify_no_down(A, B) ->
    receive
	{nodedown, A, B, _} = Msg0 ->
	    ?t:fail(Msg0)
    after 0 ->
	    ok
    end,
    receive
	{nodedown, B, A, _} = Msg1 ->
	    ?t:fail(Msg1)
    after 0 ->
	    ok
    end.

%% verify_down(A, B) ->
%%     receive {nodedown, A, B, _} -> ok end,
%%     receive {nodedown, B, A, _} -> ok end.

verify_down(A, ReasonA, B, ReasonB) ->
    receive
	{nodedown, A, B, _} = Msg0 ->
	    {nodedown, A, B, ReasonA} = Msg0
    end,
    receive
	{nodedown, B, A, _} = Msg1 ->
	    {nodedown, B, A, ReasonB} = Msg1
    end,
    ok.

hostname() ->
    from($@, atom_to_list(node())).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_, []) -> [].

%% fun_spawn(Fun) ->
%%     fun_spawn(Fun, []).

%% fun_spawn(Fun, Args) ->
%%     spawn_link(erlang, apply, [Fun, Args]).


long_or_short() -> 
    case net_kernel:longnames() of
	true -> " -name ";
	false -> " -sname "
    end.

until(Fun) ->
    case Fun() of
	true ->
	    ok;
	false ->
	    receive after 10 -> ok end,
	    until(Fun)
    end.

forever(Fun) ->
    Fun(),
    forever(Fun).

abort(Why) ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:set_internal_state(abort, Why).


start_busy_dist_port_tracer() ->
    Tracer = spawn_link(fun () -> busy_dist_port_tracer() end),
    erlang:system_monitor(Tracer, [busy_dist_port]),
    Tracer.

stop_busy_dist_port_tracer(Tracer) when is_pid(Tracer) ->
    unlink(Tracer),
    exit(Tracer, bye);
stop_busy_dist_port_tracer(_) ->
    true.

busy_dist_port_tracer() ->
    receive
	{monitor, _SuspendedProcess, busy_dist_port, _Port} = M ->
	    erlang:display(M),
	    busy_dist_port_tracer()
    end.

repeat(_Fun, 0) ->
    ok;
repeat(Fun, N) ->
    Fun(),
    repeat(Fun, N-1).

string_to_atom_ext(String) ->
    Utf8List = string_to_utf8_list(String),
    Len = length(Utf8List),
    case Len < 256 of
	true ->
	    [?SMALL_ATOM_UTF8_EXT, Len | Utf8List];
	false ->
	    [?ATOM_UTF8_EXT, Len bsr 8, Len band 16#ff | Utf8List]
    end.

string_to_atom(String) ->
    binary_to_term(list_to_binary([?VERSION_MAGIC
				   | string_to_atom_ext(String)])).

string_to_utf8_list([]) ->
    [];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
				   0 =< CP,
				   CP =< 16#7F ->
    [CP | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
				   16#80 =< CP,
				   CP =< 16#7FF ->
    [16#C0 bor (CP bsr 6),
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
				   16#800 =< CP,
				   CP =< 16#FFFF ->
    [16#E0 bor (CP bsr 12),
     16#80 bor (16#3F band (CP bsr 6)),
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
				   16#10000 =< CP,
				   CP =< 16#10FFFF ->
    [16#F0 bor (CP bsr 18),
     16#80 bor (16#3F band (CP bsr 12)),
     16#80 bor (16#3F band (CP bsr 6)),
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)].

utf8_list_to_string([]) ->
    [];
utf8_list_to_string([B|Bs]) when is_integer(B),
				 0 =< B,
				 B =< 16#7F ->
    [B | utf8_list_to_string(Bs)];
utf8_list_to_string([B0, B1 | Bs]) when is_integer(B0),
					16#C0 =< B0,
					B0 =< 16#DF,
					is_integer(B1),
					16#80 =< B1,
					B1 =< 16#BF ->
    [(((B0 band 16#1F) bsl 6)
	  bor (B1 band 16#3F))
     | utf8_list_to_string(Bs)];
utf8_list_to_string([B0, B1, B2 | Bs]) when is_integer(B0),
					    16#E0 =< B0,
					    B0 =< 16#EF,
					    is_integer(B1),
					    16#80 =< B1,
					    B1 =< 16#BF,
					    is_integer(B2),
					    16#80 =< B2,
					    B2 =< 16#BF ->
    [(((B0 band 16#F) bsl 12)
	  bor ((B1 band 16#3F) bsl 6)
	  bor (B2 band 16#3F))
     | utf8_list_to_string(Bs)];
utf8_list_to_string([B0, B1, B2, B3 | Bs]) when is_integer(B0),
						16#F0 =< B0,
						B0 =< 16#F7,
						is_integer(B1),
						16#80 =< B1,
						B1 =< 16#BF,
						is_integer(B2),
						16#80 =< B2,
						B2 =< 16#BF,
						is_integer(B3),
						16#80 =< B3,
						B3 =< 16#BF ->
    [(((B0 band 16#7) bsl 18)
	  bor ((B1 band 16#3F) bsl 12)
	  bor ((B2 band 16#3F) bsl 6)
	  bor (B3 band 16#3F))
     | utf8_list_to_string(Bs)].

mk_pid({NodeName, Creation}, Number, Serial) when is_atom(NodeName) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_pid({NodeNameExt, Creation}, Number, Serial);
mk_pid({NodeNameExt, Creation}, Number, Serial) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?PID_EXT,
					      NodeNameExt,
					      uint32_be(Number),
					      uint32_be(Serial),
					      uint8(Creation)])) of
	Pid when is_pid(Pid) ->
	    Pid;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_pid, [{NodeNameExt, Creation}, Number, Serial]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_port({NodeName, Creation}, Number) when is_atom(NodeName) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_port({NodeNameExt, Creation}, Number);
mk_port({NodeNameExt, Creation}, Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?PORT_EXT,
					      NodeNameExt,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Port when is_port(Port) ->
	    Port;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_port, [{NodeNameExt, Creation}, Number]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_ref({NodeName, Creation}, [Number] = NL) when is_atom(NodeName),
						 is_integer(Creation),
						 is_integer(Number) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_ref({NodeNameExt, Creation}, NL);
mk_ref({NodeNameExt, Creation}, [Number]) when is_integer(Creation),
					       is_integer(Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?REFERENCE_EXT,
					      NodeNameExt,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeNameExt, Creation}, [Number]]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end;
mk_ref({NodeName, Creation}, Numbers) when is_atom(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_ref({NodeNameExt, Creation}, Numbers);
mk_ref({NodeNameExt, Creation}, Numbers) when is_integer(Creation),
					      is_list(Numbers) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?NEW_REFERENCE_EXT,
					      uint16_be(length(Numbers)),
					      NodeNameExt,
					      uint8(Creation),
					      lists:map(fun (N) ->
								uint32_be(N)
							end,
							Numbers)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeNameExt, Creation}, Numbers]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.


uint32_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 32 ->
    [(Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint32_be(Uint) ->
    exit({badarg, uint32_be, [Uint]}).


uint16_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 16 ->
    [(Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint16_be(Uint) ->
    exit({badarg, uint16_be, [Uint]}).

uint8(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 8 ->
    Uint band 16#ff;
uint8(Uint) ->
    exit({badarg, uint8, [Uint]}).
