%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2024. All Rights Reserved.
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
%%% File    : signal_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Test signals
%%%
%%% Created : 10 Jul 2006 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(signal_SUITE).
-author('rickard.s.green@ericsson.com').

%-define(line_trace, 1).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0,init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, init_per_group/2, end_per_group/2]).

% Test cases
-export([xm_sig_order/1,
         kill2killed/1,
         contended_signal_handling/1,
         dirty_signal_handling_race/1,
         dirty_signal_handling_race_dirty_access/1,
         dirty_signal_handling/1,
         busy_dist_exit_signal/1,
         busy_dist_demonitor_signal/1,
         busy_dist_down_signal/1,
         busy_dist_spawn_reply_signal/1,
         busy_dist_unlink_ack_signal/1,
         unlink_exit/1,
         monitor_order/1,
         monitor_named_order_local/1,
         monitor_named_order_remote/1,
         monitor_nodes_order/1,
         move_msgs_off_heap_signal_basic/1,
         move_msgs_off_heap_signal_recv/1,
         move_msgs_off_heap_signal_exit/1,
         move_msgs_off_heap_signal_recv_exit/1,
         copy_literal_area_signal_basic/1,
         copy_literal_area_signal_recv/1,
         copy_literal_area_signal_exit/1,
         copy_literal_area_signal_recv_exit/1,
         simultaneous_signals_basic/1,
         simultaneous_signals_recv/1,
         simultaneous_signals_exit/1,
         simultaneous_signals_recv_exit/1,
         parallel_signal_enqueue_race_1/1,
         parallel_signal_enqueue_race_2/1,
         dirty_schedule/1]).

-export([spawn_spammers/3]).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(_Func, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    try erts_debug:set_internal_state(available_internal_state, false)
    catch  _:_ -> ok
    end,
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [xm_sig_order,
     kill2killed,
     contended_signal_handling,
     dirty_signal_handling_race,
     dirty_signal_handling_race_dirty_access,
     dirty_signal_handling,
     busy_dist_exit_signal,
     busy_dist_demonitor_signal,
     busy_dist_down_signal,
     busy_dist_spawn_reply_signal,
     busy_dist_unlink_ack_signal,
     unlink_exit,
     monitor_order,
     monitor_named_order_local,
     monitor_named_order_remote,
     monitor_nodes_order,
     parallel_signal_enqueue_race_1,
     parallel_signal_enqueue_race_2,
     dirty_schedule,
     {group, adjust_message_queue}].

groups() ->
    [{adjust_message_queue, [],
      [move_msgs_off_heap_signal_basic,
       move_msgs_off_heap_signal_recv,
       move_msgs_off_heap_signal_exit,
       move_msgs_off_heap_signal_recv_exit,
       copy_literal_area_signal_basic,
       copy_literal_area_signal_recv,
       copy_literal_area_signal_exit,
       copy_literal_area_signal_recv_exit,
       simultaneous_signals_basic,
       simultaneous_signals_recv,
       simultaneous_signals_exit,
       simultaneous_signals_recv_exit]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% Test that exit signals and messages are received in correct order
xm_sig_order(Config) when is_list(Config) ->
    LNode = node(),
    repeat(fun (_) -> xm_sig_order_test(LNode) end, 1000),
    {ok, Peer, RNode} = ?CT_PEER(),
    repeat(fun (_) -> xm_sig_order_test(RNode) end, 1000),
    peer:stop(Peer),
    ok.

xm_sig_order_test(Node) ->
    P = spawn(Node, fun () -> xm_sig_order_proc() end),
    M = erlang:monitor(process, P),
    P ! may_reach,
    P ! may_reach,
    P ! may_reach,
    exit(P, good_signal_order),
    P ! may_not_reach,
    P ! may_not_reach,
    P ! may_not_reach,
    receive
	      {'DOWN', M, process, P, R} ->
		  good_signal_order = R
	  end.

xm_sig_order_proc() ->
    receive
	may_not_reach -> exit(bad_signal_order);
	may_reach -> ok
    after 0 -> erlang:yield()
    end,
    xm_sig_order_proc().

kill2killed(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    kill2killed_test(node()),
    {ok, Peer, Node} = ?CT_PEER(),
    kill2killed_test(Node),
    peer:stop(Peer).

kill2killed_test(Node) ->
    if Node == node() ->
            io:format("Testing against local node", []);
       true ->
            io:format("Testing against remote node ~p", [Node])
    end,
    check_exit(Node, other_exit2, 1),
    check_exit(Node, other_exit2, 2),
    check_exit(Node, other_exit2, 9),
    check_exit(Node, other_exit2, 10),
    check_exit(Node, exit2, 1),
    check_exit(Node, exit2, 2),
    check_exit(Node, exit2, 9),
    check_exit(Node, exit2, 10),
    check_exit(Node, exit1, 1),
    check_exit(Node, exit1, 2),
    check_exit(Node, exit1, 9),
    check_exit(Node, exit1, 10),
    ok.

check_exit(Node, Type, N) ->
    io:format("Testing ~p length ~p~n", [Type, N]),
    P = spawn_link_line(Node, node(), Type, N, self()),
    if Type == other_exit2 ->
            receive
                {end_of_line, EOL} ->
                    exit(EOL, kill)
            end;
       true -> ok
    end,
    receive
        {'EXIT', P, Reason} ->
            if Type == exit1 ->
                    kill = Reason;
               true ->
                    killed = Reason
            end
    end.

spawn_link_line(_NodeA, _NodeB, other_exit2, 0, Tester) ->
    Tester ! {end_of_line, self()},
    receive after infinity -> ok end;
spawn_link_line(_NodeA, _NodeB, exit1, 0, _Tester) ->
    exit(kill);
spawn_link_line(_NodeA, _NodeB, exit2, 0, _Tester) ->
    exit(self(), kill);
spawn_link_line(NodeA, NodeB, Type, N, Tester) ->
    spawn_link(NodeA,
               fun () ->
                       spawn_link_line(NodeB, NodeA, Type, N-1, Tester),
                       receive after infinity -> ok end
               end).

contended_signal_handling(Config) when is_list(Config) ->
    %%
    %% Test for a race in signal handling of a process.
    %%
    %% When executing dirty, a "dirty signal handler"
    %% process will handle signals for the process. If
    %% the process stops executing dirty while the dirty
    %% signal handler process is handling signals on
    %% behalf of the process, both the dirty signal handler
    %% process and the process itself might try to handle
    %% signals for the process at the same time. There used
    %% to be a bug that caused both processes to enter the
    %% signal handling code simultaneously when the main
    %% lock of the process was temporarily released during
    %% signal handling (see GH-4885/OTP-17462/PR-4914).
    %% Currently the main lock is only released when the
    %% process receives an 'unlock' signal from a port,
    %% and then responds by sending an 'unlock-ack' signal
    %% to the port. This testcase tries to massage that
    %% scenario. It is quite hard to cause a crash even
    %% when the bug exists, but this testcase at least
    %% sometimes causes a crash when the bug is present.
    %%
    move_dirty_signal_handlers_to_first_scheduler(),
    process_flag(priority, high),
    case erlang:system_info(schedulers_online) of
        1 ->
            ok;
        SOnln -> 
            process_flag(scheduler, SOnln),
            ok
    end,
    Drv = unlink_signal_drv,
    ok = load_driver(Config, Drv),
    try
        contended_signal_handling_test(Drv, 250)
    after
        ok = erl_ddll:unload_driver(Drv)
    end,
    ok.

contended_signal_handling_test(_Drv, 0) ->
    ok;
contended_signal_handling_test(Drv, N) ->
    Ports = contended_signal_handling_make_ports(Drv, 100, []),
    erlang:yield(),
    contended_signal_handling_cmd_ports(Ports),
    erts_debug:dirty_cpu(wait, rand:uniform(5)),
    wait_until(fun () -> Ports == Ports -- erlang:ports() end),
    contended_signal_handling_test(Drv, N-1).

contended_signal_handling_cmd_ports([]) ->
    ok;
contended_signal_handling_cmd_ports([P|Ps]) ->
    P ! {self(), {command, "c"}},
    contended_signal_handling_cmd_ports(Ps).

contended_signal_handling_make_ports(_Drv, 0, Ports) ->
    Ports;
contended_signal_handling_make_ports(Drv, N, Ports) ->
    Port = open_port({spawn, Drv}, []),
    true = is_port(Port),
    contended_signal_handling_make_ports(Drv, N-1, [Port|Ports]).

dirty_signal_handling_race(Config) ->
    %% This test case trigger more or less the same
    %% problematic scenario as the contended_signal_handling
    %% test case is trying to trigger. This test case triggers
    %% it via another signal and is also much more likely
    %% (close to 100%) to trigger the problematic schenario.
    Tester = self(),
    move_dirty_signal_handlers_to_first_scheduler(),
    {S0, S1} = case erlang:system_info(schedulers_online) of
                   1 -> {1, 1};
                   2 -> {2, 1};
                   SOnln -> {SOnln, SOnln-1}
               end,
    process_flag(priority, high),
    process_flag(scheduler, S0),
    erts_debug:set_internal_state(available_internal_state, true),
    Drv = unlink_signal_drv,
    ok = load_driver(Config, Drv),
    try
        %% {parallelism, true} option will ensure that each
        %% signal to the port from a process is scheduled which
        %% forces the process to release its main lock when
        %% sending the signal...
        Port = open_port({spawn, Drv}, [{parallelism, true}]),
        true = is_port(Port),
        %% The {alias, reply_demonitor} option will trigger a
        %% 'demonitor' signal from Tester to the port when an
        %% alias message sent using the alias is received by
        %% Tester...
        MA1 = erlang:monitor(port, Port, [{alias, reply_demonitor}]),
        MA2 = erlang:monitor(port, Port, [{alias, reply_demonitor}]),
        Pid = spawn_opt(fun () ->
                                Tester ! go,
                                receive after 500 -> ok end,
                                %% The 'proc_sig_block' test signal will cause
                                %% dirty signal handling to start and be
                                %% blocked in the signal handling.
                                erts_debug:set_internal_state(proc_sig_block,
                                                              {Tester, 1000}),
                                %% Tester will be stuck waiting for main lock
                                %% when being scheduled out from its dirty
                                %% execution. When this alias message is
                                %% by the dirty signal handler Tester will be
                                %% able to aquire the main lock and complete
                                %% the schedule out operation.
                                MA1 ! {MA1, trigger_demonitor_port_please},
                                erts_debug:set_internal_state(proc_sig_block,
                                                              {Tester, 100}),
                                %% Tester will have been selected for
                                %% execution, but stuck waiting for main lock.
                                %% When this alias message is handled by the
                                %% dirty signal handler, Tester will be able
                                %% to aquire the main lock which will let it
                                %% enter the problematic scenario. That is,
                                %% ongoing dirty signal handling while it
                                %% begins executing.
                                MA2 ! {MA2, trigger_demonitor_port_please},
                                erts_debug:set_internal_state(proc_sig_block,
                                                              {Tester, 500}),
                                ok
                        end, [link, {scheduler, S1}]),
        receive go -> ok end,
        receive {'DOWN', MA1, port, Port, _} -> ct:fail(unexpected_port_down)
        after 0 -> ok
        end,
        receive {'DOWN', MA2, port, Port, _} -> ct:fail(unexpected_port_down)
        after 0 -> ok
        end,
        erts_debug:dirty_cpu(wait, 1000),
        receive
            {MA1, trigger_demonitor_port_please} -> ok
        end,
        receive
            {MA2, trigger_demonitor_port_please} -> ok
        end,
        unlink(Pid),
        unlink(Port),
        exit(Pid, kill),
        exit(Port, kill),
        false = erlang:is_process_alive(Pid)
    after
        ok = erl_ddll:unload_driver(Drv)
    end,
    ok.

dirty_signal_handling_race_dirty_access(Config) ->
    %% This test case trigger more or less the same
    %% problematic scenario as the contended_signal_handling
    %% and dirty_signal_handling_race, but when a dirty
    %% scheduler access the signal queue.
    dirty_signal_handling_race_dirty_access_test(Config).

dirty_signal_handling_race_dirty_access_test(Config) ->
    Tester = self(),
    move_dirty_signal_handlers_to_first_scheduler(),
    {S0, S1} = case erlang:system_info(schedulers_online) of
                   1 -> {1, 1};
                   2 -> {2, 1};
                   SOnln -> {SOnln, SOnln-1}
               end,
    process_flag(priority, high),
    process_flag(scheduler, S0),
    erts_debug:set_internal_state(available_internal_state, true),
    Drv = unlink_signal_drv,
    ok = load_driver(Config, Drv),
    DCpuOnln = erlang:system_flag(dirty_cpu_schedulers_online, 1),
    try
        _ = process_flag(fullsweep_after, 0),
        Data = lists:seq(1, 1000000),
        %% {parallelism, true} option will ensure that each
        %% signal to the port from a process is scheduled which
        %% forces the process to release its main lock when
        %% sending the signal...
        Port = open_port({spawn, Drv}, [{parallelism, true}]),
        true = is_port(Port),
        %% The {alias, reply_demonitor} option will trigger a
        %% 'demonitor' signal from Tester to the port when an
        %% alias message sent using the alias is received by
        %% Tester...
        MA1 = erlang:monitor(port, Port, [{alias, reply_demonitor}]),
        P0 = spawn_link(fun () ->
                                erlang:yield(),
                                Tester ! {self(), going_dirty},
                                erts_debug:dirty_cpu(wait, 1000)
                        end),
        receive {P0, going_dirty} -> ok end,
        P1 = spawn_opt(fun () ->
                               receive after 500 -> ok end,
                               %% The 'proc_sig_block' test signal will cause
                               %% dirty signal handling to start and be
                               %% blocked in the signal handling.
                               erts_debug:set_internal_state(proc_sig_block,
                                                             {Tester, 1000}),
                               %% Tester will be stuck waiting for main lock
                               %% when being scheduled in for dirty GC. When
                               %% the following alias message is handled by
                               %% the dirty signal handler Tester will be
                               %% able to aquire the main lock and begin
                               %% execution while the dirt signal handler
                               %% still is not finnished.
                               MA1 ! {MA1, trigger_demonitor_port_please},
                               ok
                       end, [link, {scheduler, S1}]),
        receive after 250 -> ok end,
        %% Do a GC. It will be performed on a dirty scheduler due to
        %% the amount of data on the heap. Since we only got one dirty
        %% cpu scheduler online, we will be stuck in dirty cpu runq until
        %% P0 is done
        garbage_collect(), %% Dirty GC due to Data
        receive
            {'DOWN', MA1, port, Port, _} -> ct:fail(unexpected_port_down);
            {MA1, trigger_demonitor_port_please} -> ok
        end,
        unlink(P1),
        unlink(Port),
        exit(P0, kill),
        exit(P1, kill),
        exit(Port, kill),
        false = erlang:is_process_alive(P0),
        false = erlang:is_process_alive(P1),
        _ = id(Data)
    after
        erlang:system_flag(dirty_cpu_schedulers_online, DCpuOnln),
        ok = erl_ddll:unload_driver(Drv)
    end,
    ok.

move_dirty_signal_handlers_to_first_scheduler() ->
    SOnln = erlang:system_flag(schedulers_online, 1),
    try
        true = lists:foldl(
                 fun (Pid, FoundOne) ->
                         case process_info(Pid, initial_call) of
                             {initial_call, {erts_dirty_process_signal_handler,start,0}} ->
                                 Pid ! please_execute_a_bit,
                                 true;
                             _ ->
                                 FoundOne
                         end
                 end,
                 false,
                 processes())
    after
        erlang:system_flag(schedulers_online, SOnln)
    end,
    ok.

dirty_signal_handling(Config) when is_list(Config) ->
    %%
    %% PR-7822 (third commit)
    %%
    %% Make sure signals are handled regardless of whether a process is
    %% executing dirty or is scheduled for dirty execution...

    %% Make sure all dirty I/O schedulers are occupied with work...
    Ps = lists:map(fun (_) ->
                           spawn(fun () ->
                                         erts_debug:dirty_io(wait, 1000)
                                 end)
                   end, lists:seq(1, erlang:system_info(dirty_io_schedulers))),
    %% P ends up in the run queue waiting for a free dirty I/O scheduler...
    P = spawn(fun () ->
                      erts_debug:dirty_io(wait, 1000)
              end),
    receive after 300 -> ok end,
    %% current_function is added to prevent read of status from being optimized
    %% to read status directly...
    [{status,runnable},{current_function, _}] = process_info(P, [status,current_function]),
    receive after 1000 -> ok end,
    [{status,running},{current_function, _}] = process_info(P, [status,current_function]),
    lists:foreach(fun (X) -> exit(X, kill) end, [P|Ps]),
    lists:foreach(fun (X) -> false = is_process_alive(X) end, [P|Ps]),
    ok.

busy_dist_exit_signal(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),

    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    {Exiter,MRef} = spawn_monitor(BusyChannelNode,
                                  fun () ->
                                          pong = net_adm:ping(OtherNode),
                                          Tester ! {self(), alive},
                                          receive after infinity -> ok end
                                  end),
    receive
        {Exiter, alive} ->
            erlang:demonitor(MRef, [flush]);
        {'DOWN', MRef, process, Why, normal} ->
            ct:fail({exiter_died, Why})
    end,
    Linker = spawn_link(OtherNode,
                        fun () ->
                                process_flag(trap_exit, true),
                                link(Exiter),
                                receive
                                    {'EXIT', Exiter, Reason} ->
                                        tester_killed_me = Reason,
                                        Tester ! {self(), got_exiter_exit_message};
                                    Unexpected ->
                                        exit({unexpected_message, Unexpected})
                                end
                         end),
    make_busy(BusyChannelNode, OtherNode, BusyTime),
    exit(Exiter, tester_killed_me),
    receive
        {Linker, got_exiter_exit_message} ->
            unlink(Linker),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_exit_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

busy_dist_demonitor_signal(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),

    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    Demonitorer = spawn(BusyChannelNode,
                        fun () ->
                                pong = net_adm:ping(OtherNode),
                                Tester ! {self(), alive},
                                receive
                                    {Tester, monitor, Pid} ->
                                        _Mon = erlang:monitor(process, Pid)
                                end,
                                receive after infinity -> ok end
                        end),
    receive {Demonitorer, alive} -> ok end,
    Demonitoree = spawn_link(OtherNode,
                             fun () ->
                                     wait_until(fun () ->
                                                        {monitored_by, MB1}
                                                            = process_info(self(),
                                                                           monitored_by),
                                                        lists:member(Demonitorer, MB1)
                                                end),
                                     Tester ! {self(), monitored},
                                     wait_until(fun () ->
                                                        {monitored_by, MB2}
                                                            = process_info(self(),
                                                                           monitored_by),
                                                        not lists:member(Demonitorer, MB2)
                                                end),
                                     Tester ! {self(), got_demonitorer_demonitor_signal}
                             end),
    Demonitorer ! {self(), monitor, Demonitoree},
    receive {Demonitoree, monitored} -> ok end,
    make_busy(BusyChannelNode, OtherNode, BusyTime),
    exit(Demonitorer, tester_killed_me),
    receive
        {Demonitoree, got_demonitorer_demonitor_signal} ->
            unlink(Demonitoree),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_demonitor_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

busy_dist_down_signal(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),

    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    {Exiter,MRef} = spawn_monitor(BusyChannelNode,
                                  fun () ->
                                          pong = net_adm:ping(OtherNode),
                                          Tester ! {self(), alive},
                                          receive after infinity -> ok end
                                  end),
    receive
        {Exiter, alive} ->
            erlang:demonitor(MRef, [flush]);
        {'DOWN', MRef, process, Why, normal} ->
            ct:fail({exiter_died, Why})
    end,
    Monitorer = spawn_link(OtherNode,
                        fun () ->
                                process_flag(trap_exit, true),
                                Mon = erlang:monitor(process, Exiter),
                                receive
                                    {'DOWN', Mon, process, Exiter, Reason} ->
                                        tester_killed_me = Reason,
                                        Tester ! {self(), got_exiter_down_message};
                                    Unexpected ->
                                        exit({unexpected_message, Unexpected})
                                end
                         end),
    make_busy(BusyChannelNode, OtherNode, BusyTime),
    exit(Exiter, tester_killed_me),
    receive
        {Monitorer, got_exiter_down_message} ->
            unlink(Monitorer),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_down_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

busy_dist_spawn_reply_signal(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),

    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    Spawner = spawn_link(OtherNode,
                         fun () ->
                                 pong = net_adm:ping(BusyChannelNode),
                                 Tester ! {self(), ready},
                                 receive {Tester, go} -> ok end,
                                 ReqID = spawn_request(BusyChannelNode,
                                                       fun () -> ok end,
                                                       []),
                                 receive
                                     {spawn_reply, ReqID, Result, _Pid} ->
                                         ok = Result,
                                         Tester ! {self(), got_spawn_reply_message};
                                     Unexpected ->
                                         exit({unexpected_message, Unexpected})
                                 end
                         end),
    receive {Spawner, ready} -> ok end,
    make_busy(BusyChannelNode, OtherNode, BusyTime),
    Spawner ! {self(), go},
    receive
        {Spawner, got_spawn_reply_message} ->
            unlink(Spawner),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_spawn_reply_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

-record(erl_link, {type,           % process | port | dist_process
                   pid = [],
                   state,          % linked | unlinking
                   id}).

busy_dist_unlink_ack_signal(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),

    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    {Unlinkee,MRef} = spawn_monitor(BusyChannelNode,
                                    fun () ->
                                            pong = net_adm:ping(OtherNode),
                                            Tester ! {self(), alive},
                                            receive after infinity -> ok end
                                    end),
    receive
        {Unlinkee, alive} ->
            erlang:demonitor(MRef, [flush]);
        {'DOWN', MRef, process, Why, normal} ->
            ct:fail({unlinkee_died, Why})
    end,
    Unlinker = spawn_link(OtherNode,
                          fun () ->
                                  erts_debug:set_internal_state(available_internal_state, true),
                                  link(Unlinkee),
                                  #erl_link{type = dist_process,
                                            pid = Unlinkee,
                                            state = linked} = find_proc_link(self(),
                                                                             Unlinkee),
                                  Tester ! {self(), ready},
                                  receive {Tester, go} -> ok end,
                                  unlink(Unlinkee),
                                  #erl_link{type = dist_process,
                                            pid = Unlinkee,
                                            state = unlinking} = find_proc_link(self(),
                                                                                Unlinkee),
                                  wait_until(fun () ->
                                                     false == find_proc_link(self(),
                                                                             Unlinkee)
                                             end),
                                  Tester ! {self(), got_unlink_ack_signal}
                          end),
    receive {Unlinker, ready} -> ok end,
    make_busy(BusyChannelNode, OtherNode, BusyTime),
    Unlinker ! {self(), go},
    receive
        {Unlinker, got_unlink_ack_signal} ->
            unlink(Unlinker),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_unlink_ack_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

unlink_exit(Config) when is_list(Config) ->
    %% OTP-18177
    %%
    %% This bug is theoretically possible, at least in the
    %% node local scenario, but more or less undetectable and
    %% quite harmless when it hits. A process A (the child in
    %% the testcase) could get actual exit reason of another
    %% process B (the parent in the testcase) when it should
    %% have gotten 'noproc' as exit reason. This can happen if
    %% 1. B unlinks A
    %% 2. B begin terminating before it has received an unlink
    %%    ack from A
    %% 3. A links to B after it has received the unlink signal
    %%
    %% This testcase hammers on the above scenario, but I have
    %% not seen it fail yet though when the bug is present...
    repeat(fun unlink_exit_test/0, 1000).

unlink_exit_test() ->
    Tester = self(),
    ChildFun =
        fun () ->
                process_flag(trap_exit, true),
                Tester ! {child, self()},
                Parent = receive {tester_parent, Tester, Pid} -> Pid end,
                Parent ! {go, self()},
                busy_wait_until(fun () ->
                                        receive {go, Parent} -> true
                                        after 0 -> false
                                        end
                                end),
                IsAlive = erlang:is_process_alive(Parent),
                try
                    link(Parent),
                    case IsAlive of
                        false ->
                            receive
                                {'EXIT', Parent, noproc} ->
                                    exit(ok);
                                {'EXIT', Parent, R1} ->
                                    exit({not_alive_unexpected_exit_reason, R1})
                            after 1000 ->
                                    exit(not_alive_missing_exit)
                            end;
                        true ->
                            receive
                                {'EXIT', Parent, R2} when R2 == noproc;
                                                          R2 == bye ->
                                    exit(ok);
                                {'EXIT', Parent, R2} ->
                                    exit({alive_unexpected_exit_reason, R2})
                            after 1000 ->
                                    exit(alive_missing_exit)
                            end
                    end
                catch error:noproc ->
                        receive
                            {'EXIT', Parent, _} = X0 ->
                                exit({unexpected_exit, X0})
                        after 1000 ->
                                exit(ok)
                        end
                end
        end,
    {Parent, PMon} = spawn_opt(fun () ->
                                       %% Work to do when terminating in order
                                       %% to increase the likelyhood of the
                                       %% bug triggering (if present)...
                                       T = ets:new(x,[]),
                                       ets:insert(T, lists:map(fun (I) ->
                                                                       {I,I}
                                                               end,
                                                               lists:seq(1,10000))),

                                       Child = spawn_opt(ChildFun,
                                                         [{priority, high},
                                                          link]),
                                       receive {go, Child} -> ok end,
                                       unlink(Child),
                                       Child ! {go, self()},
                                       exit(bye)
                               end, [{priority, high}, monitor]),
    Child = receive {child, Chld} -> Chld end,
    CMon = erlang:monitor(process, Child),
    Child ! {tester_parent, Tester, Parent},
    receive
        {'DOWN', PMon, process, Parent, bye} ->
            ok
    end,
    receive
        {'DOWN', CMon, process, Child, ok} ->
            ok;
        {'DOWN', CMon, process, Child, ChildReason} ->
            ct:fail(ChildReason)
    end.

%% Monitors could be reordered relative to message signals when the parallel
%% signal sending optimization was active.
monitor_order(_Config) ->
    process_flag(message_queue_data, off_heap),
    monitor_order_1(10).

monitor_order_1(0) ->
    ok;
monitor_order_1(N) ->
    Self = self(),
    {Pid, MRef} = spawn_monitor(fun() ->
                                        receive
                                            MRef ->
                                                %% The first message sets up
                                                %% the parallel signal buffer,
                                                %% the second uses it.
                                                Self ! {self(), MRef, first},
                                                Self ! {self(), MRef, second}
                                        end,
                                        exit(normal)
                                end),
    Pid ! MRef,
    receive
        {'DOWN', MRef, process, _, normal} ->
            ct:fail("Down signal arrived before second message!");
        {Pid, MRef, second} ->
            receive {Pid, MRef, first} -> ok end,
            erlang:demonitor(MRef, [flush]),
            monitor_order_1(N - 1)
    end.

%% Signal order: Message vs DOWN from local process monitored by name.
monitor_named_order_local(_Config) ->
    process_flag(message_queue_data, off_heap),
    erts_debug:set_internal_state(available_internal_state, true),
    true = erts_debug:set_internal_state(proc_sig_buffers, true),

    LNode = node(),
    repeat(fun (N) -> monitor_named_order(LNode, N) end, 100),
    ok.

%% Signal order: Message vs DOWN from remote process monitored by name.
monitor_named_order_remote(_Config) ->
    process_flag(message_queue_data, off_heap),
    erts_debug:set_internal_state(available_internal_state, true),
    true = erts_debug:set_internal_state(proc_sig_buffers, true),

    {ok, Peer, RNode} = ?CT_PEER(),
    repeat(fun (N) -> monitor_named_order(RNode, N) end, 10),
    peer:stop(Peer),
    ok.

monitor_named_order(Node, N) ->
    %% Send messages using pid, name and alias.
    Pid = self(),
    register(tester, Pid),
    Name = {tester, node()},
    AliasA = alias(),
    NumMsg = 1000 + N,
    Sender = spawn_link(Node,
                     fun() ->
                             register(monitor_named_order, self()),
                             Pid ! {self(), ready},
                             {go, AliasM} = receive_any(),
                             send_msg_seq(Pid, Name, AliasA, AliasM, NumMsg),
                             exit(normal)
                     end),
    {Sender, ready} = receive_any(),
    AliasM = monitor(process, {monitor_named_order,Node},
                     [{alias,explicit_unalias}]),
    Sender ! {go, AliasM},
    recv_msg_seq(NumMsg),
    {'DOWN', AliasM, process, {monitor_named_order,Node}, normal}
        = receive_any(),
    unregister(tester),
    unalias(AliasA),
    unalias(AliasM),
    ok.

send_msg_seq(_, _, _, _, 0) -> ok;
send_msg_seq(To1, To2, To3, To4, N) ->
    To1 ! N,
    send_msg_seq(To2, To3, To4, To1, N-1).

recv_msg_seq(0) -> ok;
recv_msg_seq(N) ->
    N = receive M -> M end,
    recv_msg_seq(N-1).

receive_any() ->
    receive M -> M end.

receive_any(Timeout) ->
    receive M -> M
    after Timeout -> timeout
    end.

monitor_nodes_order(_Config) ->
    process_flag(message_queue_data, off_heap),
    erts_debug:set_internal_state(available_internal_state, true),
    true = erts_debug:set_internal_state(proc_sig_buffers, true),

    {ok, Peer, RNode} = ?CT_PEER(#{peer_down => continue,
                                   connection => 0}),
    Self = self(),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    [] = nodes(connected),
    Pids = peer:call(Peer, ?MODULE, spawn_spammers, [64, Self, []]),
    {nodeup, RNode, []} = receive_any(),

    ok = peer:cast(Peer, erlang, halt, [0]),

    [put(P, 0) || P <- Pids],  % spam counters per sender
    {nodedown, RNode, [{nodedown_reason,connection_closed}]} =
        receive_filter_spam(),
    [io:format("From spammer ~p: ~p messages\n", [P, get(P)]) || P <- Pids],
    timeout = receive_any(100),   % Nothing after nodedown

    {down, tcp_closed} = peer:get_state(Peer),
    peer:stop(Peer),
    ok.

spawn_spammers(0, _To, Acc) ->
    Acc;
spawn_spammers(N, To, Acc) ->
    Pid = spawn(fun() -> spam_pid(To, 1) end),
    spawn_spammers(N-1, To, [Pid | Acc]).

spam_pid(To, N) ->
    To ! {spam, self(), N},
    erlang:yield(), % Let other spammers run to get lots of different senders
    spam_pid(To, N+1).

receive_filter_spam() ->
    receive
        {spam, From, N} ->
            match(N, get(From) + 1),
            put(From, N),
            receive_filter_spam();
        M -> M
    end.

move_msgs_off_heap_signal_basic(Config) when is_list(Config) ->
    move_msgs_off_heap_signal_test(false, false).

move_msgs_off_heap_signal_recv(Config) when is_list(Config) ->
    move_msgs_off_heap_signal_test(true, false).

move_msgs_off_heap_signal_exit(Config) when is_list(Config) ->
    move_msgs_off_heap_signal_test(false, true).

move_msgs_off_heap_signal_recv_exit(Config) when is_list(Config) ->
    move_msgs_off_heap_signal_test(true, true).

move_msgs_off_heap_signal_test(RecvPair, Exit) ->
    erlang:trace(new_processes, true, [running_procs]),
    SFact = test_server:timetrap_scale_factor(),
    GoTime = erlang:monotonic_time(millisecond) + 1000*SFact,
    ProcF = fun () ->
                    Now = erlang:monotonic_time(millisecond),
                    Tmo = case GoTime - Now of
                              Left when Left < 0 ->
                                  erlang:display({go_time_passed, Left}),
                                  0;
                              Left ->
                                  Left
                          end,
                    receive after Tmo -> ok end,
                    on_heap = process_flag(message_queue_data, off_heap),
                    if RecvPair -> receive_integer_pairs(infinity);
                       true -> receive after infinity -> ok end
                    end
            end,
    Ps = lists:map(fun (_) ->
                           spawn_opt(ProcF,
                                     [link,
                                      {message_queue_data, on_heap}])
                   end, lists:seq(1, 100)),
    lists:foreach(fun (P) ->
                          lists:foreach(fun (N) when N rem 100 == 0 ->
                                                P ! [N|N];
                                            (N) ->
                                                P ! N
                                        end, lists:seq(1, 10000))
                  end, Ps),
    Now = erlang:monotonic_time(millisecond),
    Tmo = case GoTime - Now + 10 of
              Left when Left < 0 ->
                  erlang:display({go_time_passed, Left}),
                  0;
              Left ->
                  Left
          end,
    receive after Tmo -> ok end,
    if Exit ->
            _ = lists:foldl(fun (P, N) when N rem 10 ->
                                    unlink(P),
                                    exit(P, terminated),
                                    N+1;
                                (_P, N) ->
                                    N+1
                            end,
                            0,
                            Ps),
            ok;
       true ->
            ok
    end,
    wait_traced_not_running(1000 + 200*SFact),
    erlang:trace(new_processes, false, [running_procs]),
    lists:foreach(fun (P) ->
                          unlink(P),
                          exit(P, kill)
                  end, Ps),
    lists:foreach(fun (P) ->
                          false = is_process_alive(P)
                  end, Ps),
    ok.

copy_literal_area_signal_basic(Config) when is_list(Config) ->
    copy_literal_area_signal_test(false, false).

copy_literal_area_signal_recv(Config) when is_list(Config) ->
    copy_literal_area_signal_test(true, false).

copy_literal_area_signal_exit(Config) when is_list(Config) ->
    copy_literal_area_signal_test(false, true).

copy_literal_area_signal_recv_exit(Config) when is_list(Config) ->
    copy_literal_area_signal_test(true, true).

copy_literal_area_signal_test(RecvPair, Exit) ->
    persistent_term:put({?MODULE, ?FUNCTION_NAME}, make_ref()),
    Literal = persistent_term:get({?MODULE, ?FUNCTION_NAME}),
    true = is_reference(Literal),
    0 = erts_debug:size_shared(Literal), %% Should be a literal...
    ProcF = fun () ->
                    0 = erts_debug:size_shared(Literal), %% Should be a literal...
                    if RecvPair ->
                            receive receive_pairs -> ok end,
                            receive_integer_pairs(0);
                       true ->
                            ok
                    end,
                    receive check_literal_conversion -> ok end,
                    receive
                        Literal ->
                            %% Should not be a literal anymore...
                            false = (0 == erts_debug:size_shared(Literal))
                    end
            end,
    PMs = lists:map(fun (_) ->
                            spawn_opt(ProcF, [link, monitor])
                    end, lists:seq(1, 100)),
    lists:foreach(fun ({P,_M}) ->
                          lists:foreach(fun (N) when N rem 100 == 0 ->
                                                P ! [N|N];
                                            (N) ->
                                                P ! N
                                        end, lists:seq(1, 10000)),
                          P ! Literal
                  end, PMs),
    persistent_term:erase({?MODULE, ?FUNCTION_NAME}),
    receive after 1 -> ok end,
    if RecvPair ->
            lists:foreach(fun ({P,_M}) ->
                                  P ! receive_pairs
                          end, PMs);
       true ->
            ok
    end,
    if Exit ->
            _ = lists:foldl(fun ({P, _M}, N) when N rem 10 ->
                                    unlink(P),
                                    exit(P, terminated),
                                    N+1;
                                (_PM, N) ->
                                    N+1
                            end,
                            0,
                            PMs),
            ok;
       true ->
            ok
    end,
    literal_area_collector_test:check_idle(),
    lists:foreach(fun ({P,_M}) ->
                          P ! check_literal_conversion
                  end, PMs),
    lists:foreach(fun ({P, M}) ->
                          receive
                              {'DOWN', M, process, P, R} ->
                                  case R of
                                      normal -> ok;
                                      terminated -> ok
                                  end
                          end
                  end, PMs),
    ok.

simultaneous_signals_basic(Config) when is_list(Config) ->
    simultaneous_signals_test(false, false).

simultaneous_signals_recv(Config) when is_list(Config) ->
    simultaneous_signals_test(true, false).

simultaneous_signals_exit(Config) when is_list(Config) ->
    simultaneous_signals_test(false, true).

simultaneous_signals_recv_exit(Config) when is_list(Config) ->
    simultaneous_signals_test(true, true).

simultaneous_signals_test(RecvPairs, Exit) ->
    erlang:trace(new_processes, true, [running_procs]),
    persistent_term:put({?MODULE, ?FUNCTION_NAME}, make_ref()),
    Literal = persistent_term:get({?MODULE, ?FUNCTION_NAME}),
    true = is_reference(Literal),
    0 = erts_debug:size_shared(Literal), %% Should be a literal...
    SFact = test_server:timetrap_scale_factor(),
    GoTime = erlang:monotonic_time(millisecond) + 1000*SFact,
    ProcF = fun () ->
                    0 = erts_debug:size_shared(Literal), %% Should be a literal...
                    Now = erlang:monotonic_time(millisecond),
                    Tmo = case GoTime - Now of
                              Left when Left < 0 ->
                                  erlang:display({go_time_passed, Left}),
                                  0;
                              Left ->
                                  Left
                          end,
                    receive after Tmo -> ok end,
                    on_heap = process_flag(message_queue_data, off_heap),
                    if RecvPairs -> receive_integer_pairs(0);
                       true -> ok
                    end,
                    receive check_literal_conversion -> ok end,
                    receive
                        Literal ->
                            %% Should not be a literal anymore...
                            false = (0 == erts_debug:size_shared(Literal))
                    end
            end,
    PMs = lists:map(fun (_) ->
                            spawn_opt(ProcF,
                                      [link,
                                       monitor,
                                       {message_queue_data, on_heap}])
                    end, lists:seq(1, 100)),
    lists:foreach(fun ({P,_M}) ->
                          lists:foreach(fun (N) when N rem 100 == 0 ->
                                                P ! [N|N];
                                            (N) ->
                                                P ! N
                                        end, lists:seq(1, 10000)),
                          P ! Literal
                  end, PMs),
    Now = erlang:monotonic_time(millisecond),
    Tmo = case GoTime - Now - 5 of % a bit earlier...
              Left when Left < 0 ->
                  erlang:display({go_time_passed, Left}),
                  0;
              Left ->
                  Left
          end,
    receive after Tmo -> ok end,
    persistent_term:erase({?MODULE, ?FUNCTION_NAME}),
    receive after 10 -> ok end,
    if Exit ->
            _ = lists:foldl(fun ({P, _M}, N) when N rem 10 ->
                                    unlink(P),
                                    exit(P, terminated),
                                    N+1;
                                (_PM, N) ->
                                    N+1
                            end,
                            0,
                            PMs),
            ok;
       true ->
            ok
    end,
    wait_traced_not_running(1000 + 200*SFact),
    erlang:trace(new_processes, false, [running_procs]),
    literal_area_collector_test:check_idle(),
    lists:foreach(fun ({P,_M}) ->
                          P ! check_literal_conversion
                  end, PMs),
    lists:foreach(fun ({P, M}) ->
                          receive
                              {'DOWN', M, process, P, R} ->
                                  case R of
                                      normal -> ok;
                                      terminated -> ok
                                  end
                          end
                  end, PMs),
    ok.
    

wait_traced_not_running(Tmo) ->
    receive
        {trace,_,What,_} when What == in;
                              What == out ->
            wait_traced_not_running(Tmo)
    after
        Tmo ->
            ok
    end.

receive_integer_pairs(Tmo) ->
    receive
        [N|N] ->
            receive_integer_pairs(Tmo)
    after
        Tmo ->
            ok
    end.

parallel_signal_enqueue_race_1(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    try
        lists:foreach(fun (_) -> parallel_signal_enqueue_race_1_test() end,
                      lists:seq(1, 5))
    after
        erts_debug:set_internal_state(available_internal_state, false)
    end.

parallel_signal_enqueue_race_1_test() ->
    %%
    %% PR-7822 (first commit)
    %%
    %% This bug could be triggered when
    %% * receiver had parallel signal enqueue optimization enabled
    %% * receiver fetched signals while it wasn't in a running state (only
    %%   happens when receive traced)
    %% * signals were enqueued simultaneously as the fetch of signals
    %%
    %% When the bug was triggered, the receiver could end up in an inconsistent
    %% state where it potentially would be stuck for ever.
    %%
    %% The above scenario is very hard to trigger, so the test typically do
    %% not fail even with the bug present, but we at least try to massage
    %% the scenario...
    R = spawn_opt(fun () ->
                          true = erts_debug:set_internal_state(proc_sig_buffers,
                                                               true),
                          receive after infinity -> ok end
                  end,
                  [{message_queue_data, off_heap}, {priority, high}, link]),
    T = spawn_link(fun FlushTrace () ->
                           receive {trace,R,'receive',_} -> ok end,
                           FlushTrace()
                   end),
    1 = erlang:trace(R, true, ['receive', {tracer, T}]),
    CountLoop = fun CountLoop (0) ->
                        ok;
                    CountLoop (N) ->
                        CountLoop(N-1)
                end,
    SigLoop = fun SigLoop (0)  ->
                      ok;
                  SigLoop (N) ->
                      CountLoop(rand:uniform(4000)),
                      erlang:demonitor(erlang:monitor(process, R), [flush]),
                      receive after 1 -> ok end,
                      SigLoop(N-1)
              end,
    SMs = lists:map(fun (X) ->
                            spawn_opt(fun () -> SigLoop(1000) end,
                                      [{scheduler, X}, link, monitor])
                    end, lists:seq(1,erlang:system_info(schedulers_online))),
    R ! hello,
    lists:foreach(fun ({P, M}) ->
                          receive {'DOWN', M, process, P, _} -> ok end
                  end, SMs),

    %% These signals would typically not be delivered if the bug was
    %% triggered and the test case would time out.
    true = is_process_alive(R),
    unlink(R),
    exit(R, kill),
    false = is_process_alive(R),

    true = is_process_alive(T),
    unlink(T),
    exit(T, kill),
    false = is_process_alive(T),

    ok.

parallel_signal_enqueue_race_2(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    try
        parallel_signal_enqueue_race_2_test()
    after
        erts_debug:set_internal_state(available_internal_state, false)
    end.

parallel_signal_enqueue_race_2_test() ->
    %%
    %% PR-7822 (first commit)
    %%
    %% This bug could be triggered when
    %% * A signal receiver process had the parallel signal enqueue optimization
    %%   enabled
    %% * Another process called process_info(Receiver, message_queue_len)
    %%   while the receiver was not executing and the process_info() call
    %%   internaly called erts_proc_sig_fetch() on receiver trying to
    %%   optimize the process_info() call
    %% * Yet another process simultaneously sent the receiver another
    %%   signal.
    %%
    %% When the bug was triggered, the receiver could end up in an inconsistent
    %% state where it potentially would be stuck for ever.
    %%
    %% The above scenario is very hard to trigger, so the test typically do
    %% not fail even with the bug present, but we at least try to massage
    %% the scenario...
    process_flag(scheduler, 1),
    {RSched, PISched, LUSched} = case erlang:system_info(schedulers_online) of
                                     1 ->
                                         {1, 1, 1};
                                     2 ->
                                         {2, 1, 2};
                                     3 ->
                                         {1, 2, 3};
                                     _ ->
                                         {2, 3, 4}
                                 end,
    Tester = self(),
    R = spawn_opt(fun () ->
                          true = erts_debug:set_internal_state(proc_sig_buffers,
                                                               true),
                          Tester ! recv_ready,
                          receive after infinity -> ok end
                  end,
                  [{message_queue_data, off_heap}, link, {scheduler, RSched}]),

    PI = spawn_opt(fun PILoop () ->
                           true = is_process_alive(R),
                           Tester ! pi_ready,
                           receive go -> ok end,
                           _ = process_info(R, message_queue_len),
                           PILoop()
                   end,
                   [link, {scheduler, PISched}]),
    LU = spawn_opt(fun LULoop () ->
                           true = is_process_alive(R),
                           Tester ! lu_ready,
                           receive go -> ok end,
                           link(R),
                           unlink(R),
                           LULoop()
                   end,
                   [link, {scheduler, LUSched}]),

    receive recv_ready -> ok end,
    TriggerLoop = fun TriggerLoop(0) ->
                          ok;
                      TriggerLoop (N) ->
                          receive lu_ready -> ok end,
                          receive pi_ready -> ok end,
                          %% Give them some time to schedule out...
                          erlang:yield(),
                          case N rem 2 of
                              0 ->
                                  PI ! go,
                                  LU ! go;
                              1 ->
                                  LU ! go,
                                  PI ! go
                          end,
                          TriggerLoop(N-1)
                  end,
    TriggerLoop(400000),

    unlink(PI),
    exit(PI, kill),
    false = is_process_alive(PI),
    unlink(LU),
    exit(LU, kill),
    false = is_process_alive(LU),
    unlink(R),
    exit(R, kill),
    false = is_process_alive(R),
    ok.

dirty_schedule(Config) when is_list(Config) ->
    lists:foreach(fun (_) ->
                          dirty_schedule_test()
                  end,
                  lists:seq(1, 5)),
    ok.

dirty_schedule_test() ->
    %%
    %% PR-7822 (second commit)
    %%
    %% This bug could occur when a process was to be scheduled due to an
    %% incomming signal just as the receiving process was selected for
    %% execution on a dirty scheduler. The process could then be inserted
    %% into a run-queue simultaneously as it began executing dirty. If
    %% the scheduled instance was selected for execution on one dirty
    %% scheduler simultaneously as it was scheduled out on another scheduler
    %% a race could cause the thread scheduling out the process to think it
    %% already was in the run-queue, so there is no need to insert it in the
    %% run-queue, while the other thread selecting it for execution dropped
    %% the process, since it was already running on another scheduler. By
    %% this the process ended up stuck in a runnable state, but not in the
    %% run-queue.
    %%
    %% When the bug was triggered, the receiver could end up in an inconsistent
    %% state where it potentially would be stuck for ever.
    %%
    %% The above scenario is very hard to trigger, so the test typically do
    %% not fail even with the bug present, but we at least try to massage
    %% the scenario...
    %%
    Proc = spawn_link(fun DirtyLoop () ->
                              erts_debug:dirty_io(scheduler,type),
                              DirtyLoop()
                      end),
    NoPs = lists:seq(1, erlang:system_info(schedulers_online)),
    SpawnSender =
        fun (Prio) ->
                spawn_opt(
                  fun () ->
                          Loop = fun Loop (0) ->
                                         ok;
                                     Loop (N) ->
                                         _ = process_info(Proc,
                                                          current_function),
                                         Loop(N-1)
                                 end,
                          receive go -> ok end,
                          Loop(100000)
                  end, [monitor,{priority,Prio}])
        end,
    Go = fun ({P, _M}) -> P ! go end,
    WaitProcs = fun ({P, M}) ->
                        receive {'DOWN', M, process, P, R} ->
                                normal = R
                        end
                end,
    PM1s = lists:map(fun (_) -> SpawnSender(normal) end, NoPs),
    lists:foreach(Go, PM1s),
    lists:foreach(WaitProcs, PM1s),
    PM2s = lists:map(fun (_) -> SpawnSender(high) end, NoPs),
    lists:foreach(Go, PM2s),
    lists:foreach(WaitProcs, PM2s),
    PM3s = lists:map(fun (N) ->
                             Prio = case N rem 2 of
                                        0 -> normal;
                                        1 -> high
                                    end,
                             SpawnSender(Prio) end, NoPs),
    lists:foreach(Go, PM3s),
    lists:foreach(WaitProcs, PM3s),
    unlink(Proc),
    exit(Proc, kill),
    false = is_process_alive(Proc),
    ok.

%%
%% -- Internal utils --------------------------------------------------------
%%

match(X,X) -> ok.

load_driver(Config, Driver) ->
    DataDir = proplists:get_value(data_dir, Config),
    case erl_ddll:load_driver(DataDir, Driver) of
        ok ->
            ok;
        {error, Error} = Res ->
            io:format("~s\n", [erl_ddll:format_error(Error)]),
            Res
    end.

wait_until(Fun) ->
    case (catch Fun()) of
        true ->
            ok;
        _ ->
            receive after 1 -> ok end,
            wait_until(Fun)
    end.

find_proc_link(Pid, To) when is_pid(Pid), is_pid(To) ->
    lists:keyfind(To,
                  #erl_link.pid,
                  erts_debug:get_internal_state({link_list, Pid})).

make_busy(OnNode, ToNode, Time) ->
    Parent = self(),
    Fun = fun () ->
                  Proxy = self(),
                  Sspndr = spawn_link(
                             ToNode,
                             fun () ->
                                     IC = find_gen_tcp_input_cntrlr(OnNode),
                                     erlang:suspend_process(IC),
                                     Proxy ! {self(), input_cntrlr_suspended},
                                     receive
                                         {Proxy, resume_input_cntrlr} ->
                                             erlang:resume_process(IC)
                                     end,
                                     Proxy ! {self(), input_cntrlr_resumed}
                             end),
                  receive
                      {Sspndr, input_cntrlr_suspended} ->
                          ok
                  end,
                  Spammer = spawn_link(
                              OnNode,
                              fun () ->
                                      spammed = spam(ToNode),
                                      Proxy ! {self(), channel_busy},
                                      receive
                                      after Time -> ok
                                      end,
                                      Proxy ! {self(), timeout}
                              end),
                  receive
                      {Spammer, channel_busy} ->
                          Parent ! {self(), channel_busy}
                  end,
                  receive
                      {Spammer, timeout} ->
                          Sspndr ! {self(), resume_input_cntrlr}
                  end,
                  receive
                      {Sspndr, input_cntrlr_resumed} ->
                          ok
                  end
          end,
    Proxy = spawn_link(Fun),
    receive
        {Proxy, channel_busy} ->
            ok
    end,
    Proxy.

find_gen_tcp_input_cntrlr(Node) when is_atom(Node) ->
    case lists:keyfind(Node, 1, erlang:system_info(dist_ctrl)) of
        {Node, DistCtrl} ->
            find_gen_tcp_input_cntrlr(DistCtrl);
        false ->
            undefined
    end;
find_gen_tcp_input_cntrlr(DistCtrl) when is_pid(DistCtrl) ->
    {links, LList} = process_info(DistCtrl, links),
    try
        lists:foreach(fun (Pid) ->
                              case process_info(Pid, initial_call) of
                                  {initial_call,
                                   {gen_tcp_dist,dist_cntrlr_input_setup,3}} ->
                                      throw({input_ctrlr, Pid});
                                  _ ->
                                      ok
                              end
                      end,
                      LList),
        undefined
    catch
        throw:{input_ctrlr, DistInputCtrlr} ->
            DistInputCtrlr
    end.

spam(Node) ->
    To = {'__a_name_hopefully_not_registered__', Node},
    Data = lists:seq(1, 100),
    spam(To, Data).

spam(To, Data) ->
    case erlang:send(To, Data, [nosuspend]) of
        nosuspend ->
            spammed;
        _ ->
            spam(To, Data)
    end.

repeat(_Fun, N) when is_integer(N), N =< 0 ->
    ok;
repeat(Fun, N) when is_function(Fun, 0), is_integer(N)  ->
    Fun(),
    repeat(Fun, N-1);
repeat(Fun, N) when is_function(Fun, 1), is_integer(N)  ->
    Fun(N),
    repeat(Fun, N-1).

busy_wait_until(Fun) ->
    case catch Fun() of
        true -> ok;
        _ -> busy_wait_until(Fun)
    end.

id(X) ->
    X.

