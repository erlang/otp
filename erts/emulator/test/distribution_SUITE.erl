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

-module(distribution_SUITE).
-compile(r16).

-define(VERSION_MAGIC,       131).

-define(ATOM_EXT,            100).
-define(REFERENCE_EXT,       101).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).
-define(ATOM_UTF8_EXT,       118).
-define(SMALL_ATOM_UTF8_EXT, 119).

%% Tests distribution and the tcp driver.

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, groups/0,
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
         atom_roundtrip_r16b/1,
         contended_atom_cache_entry/1,
         contended_unicode_atom_cache_entry/1,
         bad_dist_structure/1,
         bad_dist_ext_receive/1,
         bad_dist_ext_process_info/1,
         bad_dist_ext_control/1,
         bad_dist_ext_connection_id/1,
	 start_epmd_false/1, epmd_module/1]).

%% Internal exports.
-export([sender/3, receiver2/2, dummy_waiter/0, dead_process/0,
         roundtrip/1, bounce/1, do_dist_auto_connect/1, inet_rpc_server/1,
         dist_parallel_sender/3, dist_parallel_receiver/0,
         dist_evil_parallel_receiver/0,
         sendersender/4, sendersender2/4]).

%% epmd_module exports
-export([start_link/0, register_node/2, register_node/3, port_please/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() ->
    [ping, {group, bulk_send}, {group, local_send},
     link_to_busy, exit_to_busy, lost_exit, link_to_dead,
     link_to_dead_new_node, applied_monitor_node,
     ref_port_roundtrip, nil_roundtrip, stop_dist,
     {group, trap_bif}, {group, dist_auto_connect},
     dist_parallel_send, atom_roundtrip, unicode_atom_roundtrip,
     atom_roundtrip_r16b,
     contended_atom_cache_entry, contended_unicode_atom_cache_entry,
     bad_dist_structure, {group, bad_dist_ext},
     start_epmd_false, epmd_module].

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

%% Tests pinging a node in different ways.
ping(Config) when is_list(Config) ->
    Times = 1024,

    %% Ping a non-existing node many times.  This used to crash the emulator
    %% on Windows.

    Host = hostname(),
    BadName = list_to_atom("__pucko__@" ++ Host),
    io:format("Pinging ~s (assumed to not exist)", [BadName]),
    test_server:do_times(Times, fun() -> pang = net_adm:ping(BadName)
                                end),

    %% Pings another node.

    {ok, OtherNode} = start_node(distribution_SUITE_other),
    io:format("Pinging ~s (assumed to exist)", [OtherNode]),
    test_server:do_times(Times, fun() -> pong = net_adm:ping(OtherNode) end),
    stop_node(OtherNode),

    %% Pings our own node many times.

    Node = node(),
    io:format("Pinging ~s (the same node)", [Node]),
    test_server:do_times(Times, fun() -> pong = net_adm:ping(Node) end),

    ok.

bulk_send_small(Config) when is_list(Config) ->
    bulk_send(64, 32).

bulk_send_big(Config) when is_list(Config) ->
    bulk_send(32, 64).

bulk_send_bigbig(Config) when is_list(Config) ->
    bulk_sendsend(32*5, 4).

bulk_send(Terms, BinSize) ->
    ct:timetrap({seconds, 30}),

    io:format("Sending ~w binaries, each of size ~w K", [Terms, BinSize]),
    {ok, Node} = start_node(bulk_receiver),
    Recv = spawn(Node, erlang, apply, [fun receiver/2, [0, 0]]),
    Bin = list_to_binary(lists:duplicate(BinSize*1024, 253)),
    Size = Terms*size(Bin),
    {Elapsed, {Terms, Size}} = test_server:timecall(?MODULE, sender,
                                                    [Recv, Bin, Terms]),
    stop_node(Node),
    {comment, integer_to_list(trunc(Size/1024/max(1,Elapsed)+0.5)) ++ " K/s"}.

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
            ct:fail(ratio_too_low)
    end.

bulk_sendsend2(Terms, BinSize, BusyBufSize) ->
    ct:timetrap({seconds, 30}),

    io:format("Sending ~w binaries, each of size ~w K",
              [Terms, BinSize]),
    {ok, NodeRecv} = start_node(bulk_receiver),
    Recv = spawn(NodeRecv, erlang, apply, [fun receiver/2, [0, 0]]),
    Bin = list_to_binary(lists:duplicate(BinSize*1024, 253)),
    %%Size = Terms*size(Bin),

    %% SLF LEFT OFF HERE.
    %% When the caller uses small hunks, like 4k via
    %% bulk_sendsend(32*5, 4), then (on my laptop at least), we get
    %% zero monitor messages.  But if we use "+zdbbl 5", then we
    %% get a lot of monitor messages.  So, if we can count up the
    %% total number of monitor messages that we get when running both
    %% default busy size and "+zdbbl 5", and if the 5 case gets
    %% "many many more" monitor messages, then we know we're working.

    {ok, NodeSend} = start_node(bulk_sender, "+zdbbl " ++ integer_to_list(BusyBufSize)),
    _Send = spawn(NodeSend, erlang, apply, [fun sendersender/4, [self(), Recv, Bin, Terms]]),
    {Elapsed, {_TermsN, SizeN}, MonitorCount} =
    receive
        %% On some platforms (windows), the time taken is 0 so we
        %% simulate that some little time has passed.
        {sendersender, {0.0,T,MC}} ->
            {0.0015, T, MC};
        {sendersender, BigRes} ->
            BigRes
    end,
    stop_node(NodeRecv),
    stop_node(NodeSend),
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



%% Sends several big message to an non-registered process on the local node.
local_send_big(Config) when is_list(Config) ->
    Data0= ["Tests sending small and big messages to a non-existing ",
            "local registered process."],
    Data1=[Data0,[Data0, Data0, [Data0], Data0],Data0],
    Data2=Data0++lists:flatten(Data1)++
    list_to_binary(lists:flatten(Data1)),
    Func=fun() -> Data2= {arbitrary_name, node()} ! Data2 end,
    test_server:do_times(4096, Func),
    ok.

%% Sends a small message to an non-registered process on the local node.
local_send_small(Config) when is_list(Config) ->
    Data={some_stupid, "arbitrary", 'Data'},
    Func=fun() -> Data= {unregistered_name, node()} ! Data end,
    test_server:do_times(4096, Func),
    ok.

%% Sends data to a registered process on the local node, as if it was on another node.
local_send_legal(Config) when is_list(Config) ->
    Times=16384,
    Txt = "Some Not so random Data",
    Data={[Txt,Txt,Txt], [Txt,Txt,Txt]},
    Pid=spawn(?MODULE,receiver2, [0, 0]) ,
    true=register(registered_process, Pid),

    Func=fun() -> Data={registered_process, node()} ! Data end,
    TotalSize=size(Data)*Times,
    test_server:do_times(Times, Func),

    % Check that all msgs really came through.
    Me=self(),
    {done, Me}=
    {registered_process, node()} ! {done, Me},
    receive
        {Times, TotalSize} ->
            ok;
        _ ->
            ct:fail("Wrong number of msgs received.")
    end,
    ok.

receiver2(Num, TotSize) ->
    receive
        {done, ReplyTo} ->
            ReplyTo ! {Num, TotSize};
        Stuff ->
            receiver2(Num+1, TotSize+size(Stuff))
    end.

%% Test that link/1 to a busy distribution port works.
link_to_busy(Config) when is_list(Config) ->
    ct:timetrap({seconds, 60}),
    {ok, Node} = start_node(link_to_busy),
    Recv = spawn(Node, erlang, apply, [fun sink/1, [link_to_busy_sink]]),

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

    do_busy_test(Node, fun () -> linker(Recv) end),

    %% Same thing, but we apply link/1 instead of calling it directly.

    do_busy_test(Node, fun () -> applied_linker(Recv) end),

    %% Same thing again, but we apply link/1 in the tail of a function.

    do_busy_test(Node, fun () -> tail_applied_linker(Recv) end),

    %% Done.
    stop_node(Node),
    stop_busy_dist_port_tracer(Tracer),
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

%% Test that exit/2 to a busy distribution port works.
exit_to_busy(Config) when is_list(Config) ->
    ct:timetrap({seconds, 60}),
    {ok, Node} = start_node(exit_to_busy),

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

    Recv1 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    M1 = erlang:monitor(process, Recv1),
    do_busy_test(Node, fun () -> joey_killer(Recv1) end),
    receive
        {'DOWN', M1, process, Recv1, R1} ->
            joey_said_die = R1
    end,

    %% Same thing, but tail call to exit/2.
    Recv2 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    M2 = erlang:monitor(process, Recv2),
    do_busy_test(Node, fun () -> tail_joey_killer(Recv2) end),
    receive
        {'DOWN', M2, process, Recv2, R2} ->
            joey_said_die = R2
    end,

    %% Same thing, but we apply exit/2 instead of calling it directly.
    Recv3 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    M3 = erlang:monitor(process, Recv3),
    do_busy_test(Node, fun () -> applied_joey_killer(Recv3) end),
    receive
        {'DOWN', M3, process, Recv3, R3} ->
            joey_said_die = R3
    end,

    %% Same thing again, but we apply exit/2 in the tail of a function.
    Recv4 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    M4 = erlang:monitor(process, Recv4),
    do_busy_test(Node, fun () -> tail_applied_joey_killer(Recv4) end),
    receive
        {'DOWN', M4, process, Recv4, R4} ->
            joey_said_die = R4
    end,

    %% Done.
    stop_node(Node),
    stop_busy_dist_port_tracer(Tracer),
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
    io:format("~p : ~p~n", [P, Pinfo]),
    case Pinfo of
        undefined ->
            receive
                {'DOWN', M, process, P, Reason} ->
                    io:format("~p died with exit reason ~p~n", [P, Reason])
            end,
            ct:fail(premature_death);
        _ ->
            %% Don't match arity; it is different in debug and
            %% optimized emulator
            [{status, suspended},
             {current_function, {erlang, bif_return_trap, _}}] = Pinfo,
            receive
                {'DOWN', M, process, P, Reason} ->
                    io:format("~p died with exit reason ~p~n", [P, Reason]),
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

%% Test that EXIT and DOWN messages send to another node are not lost if
%% the distribution port is busy.
lost_exit(Config) when is_list(Config) ->
    {ok, Node} = start_node(lost_exit),

    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
                 "true" -> start_busy_dist_port_tracer();
                 _ -> false
             end,

    Self = self(),
    Die = make_ref(),
    R1 = spawn(fun () -> receive after infinity -> ok end end),
    MR1 = erlang:monitor(process, R1),

    {L1, ML1} = spawn_monitor(fun() ->
                                      link(R1),
                                      Self ! {self(), linked},
                                      receive
                                          Die ->
                                              exit(controlled_suicide)
                                      end
                              end),

    R2 = spawn(fun () ->
                       M = erlang:monitor(process, L1),
                       receive
                           {'DOWN', M, process, L1, R} ->
                               Self ! {self(), got_down_message, L1, R}
                       end
               end),

    receive {L1, linked} -> ok end,

    Busy = make_busy(Node, 2000),
    receive after 100 -> ok end,
    L1 ! Die,
    receive
        {'DOWN', ML1, process, L1, RL1} ->
            controlled_suicide = RL1
    end,
    receive after 500 -> ok end,
    unmake_busy(Busy),

    receive
        {'DOWN', MR1, process, R1, RR1} ->
            controlled_suicide = RR1
    end,

    receive
        {R2, got_down_message, L1, RR2} ->
            controlled_suicide = RR2
    end,

    %% Done.
    stop_busy_dist_port_tracer(Tracer),
    stop_node(Node),
    ok.

dummy_waiter() ->
    receive
    after infinity ->
              ok
    end.

%% Test that linking to a dead remote process gives an EXIT message
%% AND that the link is teared down.
link_to_dead(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Node} = start_node(link_to_dead),
    %    monitor_node(Node, true),
    net_adm:ping(Node), %% Ts_cross_server workaround.
    Pid = spawn(Node, ?MODULE, dead_process, []),
    receive
    after 5000 -> ok
    end,
    link(Pid),
    receive
        {'EXIT', Pid, noproc} ->
            ok;
        Other ->
            ct:fail({unexpected_message, Other})
    after 5000 ->
              ct:fail(nothing_received)
    end,
    {links, Links} = process_info(self(), links),
    io:format("Pid=~p, links=~p", [Pid, Links]),
    false = lists:member(Pid, Links),
    stop_node(Node),
    receive
        Message ->
            ct:fail({unexpected_message, Message})
    after 3000 ->
              ok
    end,
    ok.

dead_process() ->
    erlang:error(die).

%% Test that linking to a pid on node that has gone and restarted gives
%% the correct EXIT message (OTP-2304).
link_to_dead_new_node(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    %% Start the node, get a Pid and stop the node again.
    {ok, Node} = start_node(link_to_dead_new_node),
    Pid = spawn(Node, ?MODULE, dead_process, []),
    stop_node(Node),

    %% Start a new node with the same name.
    {ok, Node} = start_node(link_to_dead_new_node),
    link(Pid),
    receive
        {'EXIT', Pid, noproc} ->
            ok;
        Other ->
            ct:fail({unexpected_message, Other})
    after 5000 ->
              ct:fail(nothing_received)
    end,

    %% Make sure that the link wasn't created.
    {links, Links} = process_info(self(), links),
    io:format("Pid=~p, links=~p", [Pid, Links]),
    false = lists:member(Pid, Links),
    stop_node(Node),
    receive
        Message ->
            ct:fail({unexpected_message, Message})
    after 3000 ->
              ok
    end,
    ok.

%% Test that monitor_node/2 works when applied.
applied_monitor_node(Config) when is_list(Config) ->
    NonExisting = list_to_atom("__non_existing__@" ++ hostname()),

    %% Tail-recursive call to apply (since the node is non-existing,
    %% there will be a trap).

    true = tail_apply(erlang, monitor_node, [NonExisting, true]),
    [{nodedown, NonExisting}] = test_server:messages_get(),

    %% Ordinary call (with trap).

    true = apply(erlang, monitor_node, [NonExisting, true]),
    [{nodedown, NonExisting}] = test_server:messages_get(),

    ok.

tail_apply(M, F, A) ->
    apply(M, F, A).

%% Test that sending a port or reference to another node and back again
%% doesn't correct them in any way.
ref_port_roundtrip(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, efile}, []),
    Ref = make_ref(),
    {ok, Node} = start_node(ref_port_roundtrip),
    net_adm:ping(Node),
    Term = {Port, Ref},
    io:format("Term before: ~p", [show_term(Term)]),
    Pid = spawn_link(Node, ?MODULE, roundtrip, [Term]),
    receive after 5000 -> ok end,
    stop_node(Node),
    receive
        {'EXIT', Pid, {Port, Ref}} ->
            io:format("Term after: ~p", [show_term(Term)]),
            ok;
        Other ->
            io:format("Term after: ~p", [show_term(Term)]),
            ct:fail({unexpected, Other})
    after 10000 ->
              ct:fail(timeout)
    end,
    ok.

roundtrip(Term) ->
    exit(Term).

%% Test that the smallest external term [] aka NIL can be sent to
%% another node node and back again.
nil_roundtrip(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Node} = start_node(nil_roundtrip),
    net_adm:ping(Node),
    Pid = spawn_link(Node, ?MODULE, bounce, [self()]),
    Pid ! [],
    receive
        [] ->
            receive
                {'EXIT', Pid, []} ->
                    stop_node(Node),
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

%% Tests behaviour after net_kernel:stop (OTP-2586).
stop_dist(Config) when is_list(Config) ->
    Str = os:cmd(atom_to_list(lib:progname())
                 ++ " -noshell -pa "
                 ++ proplists:get_value(data_dir, Config)
                 ++ " -s run"),
    %% The "true" may be followed by an error report, so ignore anything that
    %% follows it.
    "true\n"++_ = Str,

    %% "May fail on FreeBSD due to differently configured name lookup - ask Arndt",
    %% if you can find him.

    ok.


trap_bif_1(Config) when is_list(Config) ->
    {true} = tr1(),
    ok.

trap_bif_2(Config) when is_list(Config) ->
    {true} = tr2(),
    ok.

trap_bif_3(Config) when is_list(Config) ->
    {hoo} = tr3(),
    ok.

tr1() ->
    NonExisting = 'abc@boromir',
    X = erlang:monitor_node(NonExisting, true),
    {X}.

tr2() ->
    NonExisting = 'abc@boromir',
    X = apply(erlang, monitor_node, [NonExisting, true]),
    {X}.

tr3() ->
    NonExisting = 'abc@boromir',
    X = {NonExisting, glirp} ! hoo,
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

%% Test the dist_auto_connect once kernel parameter
dist_auto_connect_once(Config) when is_list(Config) ->
    Sock = start_relay_node(dist_auto_connect_relay_node,[]),
    NN = inet_rpc_nodename(Sock),
    Sock2 = start_relay_node(dist_auto_connect_once_node,
                             "-kernel dist_auto_connect once"),
    NN2 = inet_rpc_nodename(Sock2),
    {ok,[]} = do_inet_rpc(Sock,erlang,nodes,[]),
    {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    {ok,[NN2]} = do_inet_rpc(Sock,erlang,nodes,[]),
    {ok,[NN]} = do_inet_rpc(Sock2,erlang,nodes,[]),
    [_,HostPartPeer] = string:tokens(atom_to_list(NN),"@"),
    [_,MyHostPart] = string:tokens(atom_to_list(node()),"@"),
    % Give net_kernel a chance to change the state of the node to up to.
    receive after 1000 -> ok end,
    case HostPartPeer of
        MyHostPart ->
            ok = stop_relay_node(Sock),
            {ok,pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]);
        _ ->
            {ok, true} = do_inet_rpc(Sock,net_kernel,disconnect,[NN2]),
            receive
            after 500 -> ok
            end
    end,
    {ok, []} = do_inet_rpc(Sock2,erlang,nodes,[]),
    Sock3 = case HostPartPeer of
                MyHostPart ->
                    start_relay_node(dist_auto_connect_relay_node,[]);
                _ ->
                    Sock
            end,
    TS1 = timestamp(),
    {ok, pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    TS2 = timestamp(),
    RefT = net_kernel:connecttime() - 1000,
    true = ((TS2 - TS1) < RefT),
    TS3 = timestamp(),
    {ok, true} = do_inet_rpc(Sock2,erlang,monitor_node,
                             [NN,true,[allow_passive_connect]]),
    TS4 = timestamp(),
    true = ((TS4 - TS3) > RefT),
    {ok, pong} = do_inet_rpc(Sock3,net_adm,ping,[NN2]),
    {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    {ok, true} = do_inet_rpc(Sock3,net_kernel,disconnect,[NN2]),
    receive
    after 500 -> ok
    end,
    {ok, pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    {ok, true} = do_inet_rpc(Sock2,net_kernel,connect_node,[NN]),
    {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    stop_relay_node(Sock3),
    stop_relay_node(Sock2).



%% Start a relay node and a lonely (dist_auto_connect never) node.
%% Lonely node pings relay node. That should fail. 
%% Lonely node connects to relay node with net_kernel:connect_node/1.
%% Result is sent here through relay node.
dist_auto_connect_never(Config) when is_list(Config) ->
    Self = self(),
    {ok, RelayNode} = start_node(dist_auto_connect_relay),
    spawn(RelayNode,
          fun() ->
                  register(dist_auto_connect_relay, self()),
                  dist_auto_connect_relay(Self)
          end),
    {ok, Handle} = dist_auto_connect_start(dist_auto_connect, never),
    Result = receive
                 {do_dist_auto_connect, ok} ->
                     ok;
                 {do_dist_auto_connect, Error} ->
                     {error, Error};
                 Other ->
                     {error, Other}
             after 32000 ->
                       timeout
             end,
    stop_node(RelayNode),
    Stopped = dist_auto_connect_stop(Handle),
    Junk = receive
               {do_dist_auto_connect, _} = J -> J
           after 0 -> ok
           end,
    {ok, ok, ok} = {Result, Stopped, Junk},
    ok.


do_dist_auto_connect([never]) ->
    Node = list_to_atom("dist_auto_connect_relay@" ++ hostname()),
    io:format("~p:do_dist_auto_connect([false]) Node=~p~n", [?MODULE, Node]),
    Ping = net_adm:ping(Node),
    io:format("~p:do_dist_auto_connect([false]) Ping=~p~n", [?MODULE, Ping]),
    Result = case Ping of
                 pang -> ok;
                 _ -> {error, Ping}
             end,
    io:format("~p:do_dist_auto_connect([false]) Result=~p~n", [?MODULE, Result]),
    net_kernel:connect_node(Node),
    catch {dist_auto_connect_relay, Node} ! {do_dist_auto_connect, Result};
%    receive after 1000 -> ok end,
%    halt();

do_dist_auto_connect(Arg) ->
    io:format("~p:do_dist_auto_connect(~p)~n", [?MODULE, Arg]),
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


dist_parallel_send(Config) when is_list(Config) ->
    {ok, RNode} = start_node(dist_parallel_receiver),
    {ok, SNode} = start_node(dist_parallel_sender),
    WatchDog = spawn_link(
                 fun () ->
                         TRef = erlang:start_timer((2*60*1000), self(), oops),
                         receive
                             {timeout, TRef, _ } ->
                                 spawn(SNode, fun () -> abort(timeout) end),
                                 spawn(RNode, fun () -> abort(timeout) end)
                                 %%       rpc:cast(SNode, erlang, halt,
                                 %%		["Timetrap (sender)"]),
                                 %%       rpc:cast(RNode, erlang, halt,
                                 %%		["Timetrap (receiver)"])
                         end
                 end),
    MkSndrs = fun (Receiver) ->
                      lists:map(fun (_) ->
                                        spawn_link(SNode,
                                                   ?MODULE,
                                                   dist_parallel_sender,
                                                   [self(), Receiver, 1000])
                                end, lists:seq(1, 64))
              end,
    SndrsStart = fun (Sndrs) ->
                         Parent = self(),
                         spawn_link(SNode,
                           fun () ->
                                   lists:foreach(fun (P) ->
                                                         P ! {go, Parent}
                                                 end, Sndrs)
                           end)
                 end,
    SndrsWait = fun (Sndrs) ->
                        lists:foreach(fun (P) ->
                                              receive {P, done} -> ok end
                                      end, Sndrs)
                end,
    DPR = spawn_link(RNode, ?MODULE, dist_parallel_receiver, []),
    Sndrs1 = MkSndrs(DPR),
    SndrsStart(Sndrs1),
    SndrsWait(Sndrs1),
    unlink(DPR),
    exit(DPR, bang),

    DEPR = spawn_link(RNode, ?MODULE, dist_evil_parallel_receiver, []),
    Sndrs2 = MkSndrs(DEPR),
    SndrsStart(Sndrs2),
    SndrsWait(Sndrs2),
    unlink(DEPR),
    exit(DEPR, bang),

    unlink(WatchDog),
    exit(WatchDog, bang),

    stop_node(RNode),
    stop_node(SNode),

    ok.

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
    AtomData = atom_data(),
    verify_atom_data(AtomData),
    {ok, Node} = start_node(Config),
    do_atom_roundtrip(Node, AtomData),
    stop_node(Node),
    ok.

atom_roundtrip_r16b(Config) when is_list(Config) ->
    case test_server:is_release_available("r16b") of
        true ->
            ct:timetrap({minutes, 6}),
            AtomData = unicode_atom_data(),
            verify_atom_data(AtomData),
            case start_node(Config, [], "r16b") of
                {ok, Node} ->
                    do_atom_roundtrip(Node, AtomData),
                    stop_node(Node);
                {error, timeout} ->
                    {skip,"Unable to start OTP R16B release"}
            end;
        false ->
            {skip,"No OTP R16B available"}
    end.

unicode_atom_roundtrip(Config) when is_list(Config) ->
    AtomData = unicode_atom_data(),
    verify_atom_data(AtomData),
    {ok, Node} = start_node(Config),
    do_atom_roundtrip(Node, AtomData),
    stop_node(Node),
    ok.

do_atom_roundtrip(Node, AtomData) ->
    Parent = self(),
    Proc = spawn_link(Node, fun () -> verify_atom_data_loop(Parent) end),
    Proc ! {self(), AtomData},
    receive {Proc, AD1} -> AtomData = AD1 end,
    Proc ! {self(), AtomData},
    receive {Proc, AD2} -> AtomData = AD2 end,
    RevAtomData = lists:reverse(AtomData),
    Proc ! {self(), RevAtomData},
    receive {Proc, RAD1} -> RevAtomData = RAD1 end,
    unlink(Proc),
    exit(Proc, bang),
    ok.

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
                 end, lists:seq(1, 2000))].

contended_atom_cache_entry(Config) when is_list(Config) ->
    contended_atom_cache_entry_test(Config, latin1).

contended_unicode_atom_cache_entry(Config) when is_list(Config) ->
    contended_atom_cache_entry_test(Config, unicode).

contended_atom_cache_entry_test(Config, Type) ->
    TestServer = self(),
    ProcessPairs = 10,
    Msgs = 100000,
    {ok, SNode} = start_node(Config),
    {ok, RNode} = start_node(Config),
    Success = make_ref(),
    spawn_link(
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
                             R = spawn_link(RNode,
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
                             S = spawn_link(SNode,
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
    receive
        Success ->
            ok
    end,
    stop_node(SNode),
    stop_node(RNode),
    ok.

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
    Atom = list_to_atom("atom" ++ integer_to_list(erlang:unique_integer([positive]))),
    case erts_debug:get_internal_state({atom_out_cache_index, Atom}) of
        CIX ->
            [Atom|get_conflicting_atoms(CIX, N-1)];
        _ ->
            get_conflicting_atoms(CIX, N)
    end.

get_conflicting_unicode_atoms(_CIX, 0) ->
    [];
get_conflicting_unicode_atoms(CIX, N) ->
    Atom = string_to_atom([16#1f608] ++ "atom" ++ integer_to_list(erlang:unique_integer([positive]))),
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
    Parent = self(),
    Q = spawn(Offender,
              fun () ->
                      Ref = erlang:monitor(process,P),
                      Parent ! {self(),ref,Ref},
                      receive
                          just_stay_alive -> ok
                      end
              end),
    Ref = receive
              {Q,ref,R} ->
                  R
          after  5000 ->
                     error
          end,
    io:format("Ref is ~p~n",[Ref]),
    ok.
start_link(Offender,P) ->
    Parent = self(),
    Q = spawn(Offender,
              fun () ->
                      process_flag(trap_exit,true),
                      link(P),
                      Parent ! {self(),ref,P},
                      receive
                          just_stay_alive -> ok
                      end
              end),
    Ref = receive
              {Q,ref,R} ->
                  R
          after  5000 ->
                     error
          end,
    io:format("Ref is ~p~n",[Ref]),
    ok.

%% Test dist messages with valid structure (binary to term ok) but malformed control content
bad_dist_structure(Config) when is_list(Config) ->
    ct:timetrap({seconds, 15}),

    {ok, Offender} = start_node(bad_dist_structure_offender),
    {ok, Victim} = start_node(bad_dist_structure_victim),
    start_node_monitors([Offender,Victim]),
    Parent = self(),
    P = spawn(Victim,
              fun () ->
                      process_flag(trap_exit,true),
                      Parent ! {self(), started},
                      receive check_msgs -> ok end,
                      bad_dist_struct_check_msgs([one,
                                                  two]),
                      Parent ! {self(), messages_checked},
                      receive done -> ok end
              end),
    receive {P, started} -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    start_monitor(Offender,P),
    P ! one,
    send_bad_structure(Offender, P,{?DOP_MONITOR_P_EXIT,'replace',P,normal},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_MONITOR_P_EXIT,'replace',P,normal,normal},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_LINK},0),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_UNLINK,'replace'},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_UNLINK,'replace',make_ref()},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_UNLINK,make_ref(),P},0),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_UNLINK,normal,normal},0),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_MONITOR_P,'replace',P},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_MONITOR_P,'replace',P,normal},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_DEMONITOR_P,'replace',P},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_DEMONITOR_P,'replace',P,normal},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_EXIT,'replace',P},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_EXIT,make_ref(),normal,normal},0),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_EXIT_TT,'replace',token,P},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_EXIT_TT,make_ref(),token,normal,normal},0),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_EXIT2,'replace',P},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_EXIT2,make_ref(),normal,normal},0),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_EXIT2_TT,'replace',token,P},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_EXIT2_TT,make_ref(),token,normal,normal},0),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace'},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace','atomic'},2),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace',P},0),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_REG_SEND_TT,'replace','',name},2,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_REG_SEND_TT,'replace','',name,token},0,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace',''},2,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',P},0,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',name},0,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',name,{token}},2,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_SEND_TT,'',P},0,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_SEND_TT,'',name,token},0,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_SEND,''},0,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_SEND,'',name},0,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    send_bad_structure(Offender, P,{?DOP_SEND,'',P,{token}},0,{message}),
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    P ! two,
    P ! check_msgs,
    receive
        {P, messages_checked} -> ok
    after 5000 ->
              exit(victim_is_dead)
    end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    unlink(P),
    P ! done,
    stop_node(Offender),
    stop_node(Victim),
    ok.



bad_dist_ext_receive(Config) when is_list(Config) ->
    {ok, Offender} = start_node(bad_dist_ext_receive_offender),
    {ok, Victim} = start_node(bad_dist_ext_receive_victim),
    start_node_monitors([Offender,Victim]),

    Parent = self(),

    P = spawn_link(Victim,
                   fun () ->
                           Parent ! {self(), started},
                           receive check_msgs -> ok end,
                           bad_dist_ext_check_msgs([one,
                                                    two,
                                                    three]),
                           Parent ! {self(), messages_checked},
                           receive done -> ok end
                   end),

    receive {P, started} -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    P ! one,
    send_bad_msg(Offender, P),
    P ! two,
    verify_down(Offender, connection_closed, Victim, killed),
    {message_queue_len, 2}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    Suspended = make_ref(),
    S = spawn(Victim,
              fun () ->
                      erlang:suspend_process(P),
                      Parent ! Suspended,
                      receive after infinity -> ok end
              end),
    MS = erlang:monitor(process, S),
    receive Suspended -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    send_bad_msgs(Offender, P, 5),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    P ! three,
    send_bad_msgs(Offender, P, 5),

    %% Make sure bad msgs has reached Victim
    rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    verify_still_up(Offender, Victim),
    {message_queue_len, 13}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    exit(S, bang),
    receive {'DOWN', MS, process, S, bang} -> ok end,
    verify_down(Offender, connection_closed, Victim, killed),
    {message_queue_len, 3}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    P ! check_msgs,
    receive {P, messages_checked} -> ok end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    P ! done,
    unlink(P),
    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).


bad_dist_ext_process_info(Config) when is_list(Config) ->
    {ok, Offender} = start_node(bad_dist_ext_process_info_offender),
    {ok, Victim} = start_node(bad_dist_ext_process_info_victim),
    start_node_monitors([Offender,Victim]),

    Parent = self(),
    P = spawn_link(Victim,
                   fun () ->
                           Parent ! {self(), started},
                           receive check_msgs -> ok end,
                           bad_dist_ext_check_msgs([one, two]),
                           Parent ! {self(), messages_checked},
                           receive done -> ok end
                   end),

    receive {P, started} -> ok end,
    P ! one,

    Suspended = make_ref(),
    S = spawn(Victim,
              fun () ->
                      erlang:suspend_process(P),
                      Parent ! Suspended,
                      receive after infinity -> ok end
              end),

    receive Suspended -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_msgs(Offender, P, 5),

    P ! two,
    send_bad_msgs(Offender, P, 5),

    %% Make sure bad msgs has reached Victim
    rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    verify_still_up(Offender, Victim),
    {message_queue_len, 12}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),
    verify_still_up(Offender, Victim),
    [{message_queue_len, 2},
     {messages, [one, two]}]
    = rpc:call(Victim, erlang, process_info, [P, [message_queue_len,
                                                  messages]]),
    verify_down(Offender, connection_closed, Victim, killed),

    P ! check_msgs,
    exit(S, bang),
    receive {P, messages_checked} -> ok end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    P ! done,
    unlink(P),
    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).

bad_dist_ext_control(Config) when is_list(Config) ->
    {ok, Offender} = start_node(bad_dist_ext_control_offender),
    {ok, Victim} = start_node(bad_dist_ext_control_victim),
    start_node_monitors([Offender,Victim]),

    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_dhdr(Offender, Victim),
    verify_down(Offender, connection_closed, Victim, killed),

    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_ctl(Offender, Victim),
    verify_down(Offender, connection_closed, Victim, killed),

    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).

bad_dist_ext_connection_id(Config) when is_list(Config) ->
    {ok, Offender} = start_node(bad_dist_ext_connection_id_offender),
    {ok, Victim} = start_node(bad_dist_ext_connection_id_victim),
    start_node_monitors([Offender,Victim]),

    Parent = self(),
    P = spawn_link(Victim,
                   fun () ->
                           Parent ! {self(), started},
                           receive check_msgs -> ok end,
                           bad_dist_ext_check_msgs([]),
                           Parent ! {self(), messages_checked},
                           receive done -> ok end
                   end),

    receive {P, started} -> ok end,
    Suspended = make_ref(),
    S = spawn(Victim,
              fun () ->
                      erlang:suspend_process(P),
                      Parent ! Suspended,
                      receive after infinity -> ok end
              end),
    MS = erlang:monitor(process, S),
    receive Suspended -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_msg(Offender, P),

    %% Make sure bad msg has reached Victim
    rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    {message_queue_len, 1}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    true = rpc:call(Offender, net_kernel, disconnect, [Victim]),
    verify_down(Offender, disconnect, Victim, connection_closed),
    pong = rpc:call(Offender, net_adm, ping, [Victim]),

    verify_up(Offender, Victim),
    %% We have a new connection between Offender and Victim, bad message
    %% should not bring it down.

    {message_queue_len, 1}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    exit(S, bang),
    receive {'DOWN', MS, process, S, bang} -> ok end,
    %% Wait for a while (if the connection is taken down it might take a
    %% while).
    receive after 2000 -> ok end,
    verify_still_up(Offender, Victim),

    P ! check_msgs,
    receive {P, messages_checked} -> ok end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    verify_still_up(Offender, Victim),
    P ! done,
    unlink(P),
    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).


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

start_epmd_false(Config) when is_list(Config) ->
    %% Start a node with the option -start_epmd false.
    {ok, OtherNode} = start_node(start_epmd_false, "-start_epmd false"),
    %% We should be able to ping it, as epmd was started by us:
    pong = net_adm:ping(OtherNode),
    stop_node(OtherNode),

    ok.

epmd_module(Config) when is_list(Config) ->
    %% We need a relay node to test this, since the test node uses the
    %% standard epmd module.
    Sock1 = start_relay_node(epmd_module_node1, "-epmd_module " ++ ?MODULE_STRING),
    Node1 = inet_rpc_nodename(Sock1),
    %% Ask what port it's listening on - it won't have registered with
    %% epmd.
    {ok, {ok, Port1}} = do_inet_rpc(Sock1, application, get_env, [kernel, dist_listen_port]),

    %% Start a second node, passing the port number as a secret
    %% argument.
    Sock2 = start_relay_node(epmd_module_node2, "-epmd_module " ++ ?MODULE_STRING
			     ++ " -other_node_port " ++ integer_to_list(Port1)),
    Node2 = inet_rpc_nodename(Sock2),
    %% Node 1 can't ping node 2
    {ok, pang} = do_inet_rpc(Sock1, net_adm, ping, [Node2]),
    {ok, []} = do_inet_rpc(Sock1, erlang, nodes, []),
    {ok, []} = do_inet_rpc(Sock2, erlang, nodes, []),
    %% But node 2 can ping node 1
    {ok, pong} = do_inet_rpc(Sock2, net_adm, ping, [Node1]),
    {ok, [Node2]} = do_inet_rpc(Sock1, erlang, nodes, []),
    {ok, [Node1]} = do_inet_rpc(Sock2, erlang, nodes, []),

    stop_relay_node(Sock2),
    stop_relay_node(Sock1).

%% epmd_module functions:

start_link() ->
    ignore.

register_node(Name, Port) ->
    register_node(Name, Port, inet_tcp).
register_node(_Name, Port, _Driver) ->
    %% Save the port number we're listening on.
    application:set_env(kernel, dist_listen_port, Port),
    Creation = rand:uniform(3),
    {ok, Creation}.

port_please(_Name, _Ip) ->
    case init:get_argument(other_node_port) of
	error ->
	    %% None specified.  Default to 42.
	    Port = 42,
	    Version = 5,
	    {port, Port, Version};
	{ok, [[PortS]]} ->
	    %% Port number given on command line.
	    Port = list_to_integer(PortS),
	    Version = 5,
	    {port, Port, Version}
    end.

%%% Utilities

timestamp() ->
    erlang:monotonic_time(millisecond).

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
                         ++ atom_to_list(proplists:get_value(testcase, Config))
                         ++ "-"
                         ++ integer_to_list(erlang:system_time(second))
                         ++ "-"
                         ++ integer_to_list(erlang:unique_integer([positive])))),
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
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, 4}, {active, false}]),
    {ok, Port} = inet:port(LSock),
    {ok, Host} = inet:gethostname(),
    RunArg = "-run " ++ atom_to_list(?MODULE) ++ " inet_rpc_server " ++
    Host ++ " " ++ integer_to_list(Port),
    {ok, NN} = test_server:start_node(Node, peer,
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
            io:format("~p ~p: ~p~n", [node(), erlang:system_time(microsecond), Nodes0]),
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
            io:format("~p ~p: ~p~n", [node(), erlang:system_time(microsecond), Msg]),
            node_monitor_loop(Master);
        {nodedown, Node, InfoList} = Msg ->
            Reason = case lists:keysearch(nodedown_reason, 1, InfoList) of
                         {value, {nodedown_reason, R}} -> R;
                         _ -> undefined
                     end,
            Master ! {nodedown, node(), Node, Reason},
            io:format("~p ~p: ~p~n", [node(), erlang:system_time(microsecond), Msg]),
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
            ct:fail(Msg0)
    after 0 ->
              ok
    end,
    receive
        {nodedown, B, A, _} = Msg1 ->
            ct:fail(Msg1)
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
