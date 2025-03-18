%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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
-include_lib("kernel/include/dist.hrl").
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
         dirty_schedule/1,
         priority_messages_link_enable_disable/1,
         priority_messages_monitor_enable_disable/1,
         priority_messages_alias_enable_disable/1,
         priority_messages_order/1,
         priority_messages_hopefull_encoding/1,
         priority_messages_old_nodes/1]).

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
     {group, adjust_message_queue},
     {group, priority_messages}].

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
       simultaneous_signals_recv_exit]},
     {priority_messages, [],
      [priority_messages_link_enable_disable,
       priority_messages_monitor_enable_disable,
       priority_messages_alias_enable_disable,
       priority_messages_order,
       priority_messages_hopefull_encoding,
       priority_messages_old_nodes]}].

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

priority_messages_link_enable_disable(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {priority_messages, false} = process_info(self(), priority_messages),
    ct:log("Testing against local process~n", []),
    priority_messages_link_enable_disable_test(node()),
    ct:log("Testing against local process succeeded~n", []),

    {ok, Peer, Node} = ?CT_PEER(),
    ct:log("Testing against remote process~n", []),
    priority_messages_link_enable_disable_test(Node),
    {priority_messages, false} = process_info(self(), priority_messages),
    P1 = spawn_opt(Node, fun () -> receive after infinity -> ok end end,
                   [{link, [priority]}]),
    {priority_messages, true} = process_info(self(), priority_messages),
    peer:stop(Peer),
    receive {'EXIT', P1, noconnection} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),
    P2 = spawn_opt(Node, fun () -> receive after infinity -> ok end end,
                   [{link, [priority]}]),
    receive {'EXIT', P2, noconnection} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),
    ct:log("Testing against remote process succeeded~n", []),

    ok.

priority_messages_link_enable_disable_test(Node) ->
    P1 = spawn(Node, fun () -> receive bye -> ok end end),
    {priority_messages, false} = process_info(self(), priority_messages),
    link(P1, [priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    link(P1, []),
    {priority_messages, false} = process_info(self(), priority_messages),
    link(P1, [priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    unlink(P1),
    {priority_messages, false} = process_info(self(), priority_messages),
    link(P1, [priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    P1 ! bye,
    receive {'EXIT', P1, normal} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),

    P2 = spawn_opt(Node, fun () -> receive after infinity -> ok end end,
                   [{link, [priority]}]),
    {priority_messages, true} = process_info(self(), priority_messages),
    exit(P2, bye),
    receive {'EXIT', P2, bye} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),
    erlang:yield(),
    M = spawn_request(Node, fun () -> ok end, [{link, [priority]}]),
    case spawn_request_abandon(M) of
        true ->
            ct:log("spawn request abandoned"),
            ok;
        false ->
            ct:log("spawn request not abandoned"),
            receive
                {spawn_reply, M, ok, P3} ->
                    receive
                        {'EXIT', P3, Reason} ->
                            normal = Reason
                    end;
                {spawn_reply, M, error, Error} ->
                    ct:fail({spawn_error, Error})
            end
    end,
    {priority_messages, false} = process_info(self(), priority_messages),
    ok.

priority_messages_monitor_enable_disable(Config) when is_list(Config) ->
    {priority_messages, false} = process_info(self(), priority_messages),
    ct:log("Testing against local process~n", []),
    priority_messages_monitor_enable_disable_test(node()),
    {priority_messages, false} = process_info(self(), priority_messages),
    M1 = monitor(time_offset, clock_service, [priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    demonitor(M1),
    {priority_messages, false} = process_info(self(), priority_messages),
    ct:log("Testing against local process succeeded~n", []),

    {ok, Peer, Node} = ?CT_PEER(),
    ct:log("Testing against remote process~n", []),
    priority_messages_monitor_enable_disable_test(Node),
    {priority_messages, false} = process_info(self(), priority_messages),
    {P1, M2} = spawn_opt(Node, fun () -> receive after infinity -> ok end end,
                         [{monitor, [priority]}]),
    {priority_messages, true} = process_info(self(), priority_messages),
    peer:stop(Peer),
    receive {'DOWN', M2, process, P1, noconnection} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),
    M3 = monitor(process, P1),
    receive {'DOWN', M3, process, P1, noconnection} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),
    {P2, M4} = spawn_opt(Node, fun () -> receive after infinity -> ok end end,
                         [{monitor, [priority]}]),
    receive {'DOWN', M4, process, P2, noconnection} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),
    ct:log("Testing against remote process succeeded~n", []),

    ok.

priority_messages_monitor_enable_disable_test(Node) ->
    P1 = spawn(Node, fun () -> receive bye -> ok end end),
    {priority_messages, false} = process_info(self(), priority_messages),
    M1 = monitor(process, P1, [priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    demonitor(M1),
    {priority_messages, false} = process_info(self(), priority_messages),
    M2 = monitor(process, P1, [priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    P1 ! bye,
    receive {'DOWN', M2, process, P1, normal} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),

    {P2, M3} = spawn_opt(Node, fun () -> receive after infinity -> ok end end,
                         [{monitor, [priority]}]),
    {priority_messages, true} = process_info(self(), priority_messages),
    exit(P2, bye),
    receive {'DOWN', M3, process, P2, bye} -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),
    erlang:yield(),
    M4 = spawn_request(Node, fun () -> ok end, [{monitor, [priority]}]),
    case spawn_request_abandon(M4) of
        true ->
            ct:log("spawn request abandoned"),
            ok;
        false ->
            ct:log("spawn request not abandoned"),
            receive
                {spawn_reply,M4,ok,P3} ->
                    receive
                        {'DOWN', M4, process, P3, Reason} ->
                            normal = Reason
                    end;
                {spawn_reply,M4,error,Error} ->
                    ct:fail({spawn_error, Error})
            end
    end,
    {priority_messages, false} = process_info(self(), priority_messages),
    ok.

priority_messages_alias_enable_disable(Config) when is_list(Config) ->
    {priority_messages, false} = process_info(self(), priority_messages),
    ct:log("Testing against local process~n", []),
    priority_messages_alias_enable_disable_test(node()),
    {priority_messages, false} = process_info(self(), priority_messages),
    ct:log("Testing against local process succeeded~n", []),

    {ok, Peer, Node} = ?CT_PEER(),
    ct:log("Testing against remote process~n", []),
    priority_messages_alias_enable_disable_test(Node),
    peer:stop(Peer),
    ct:log("Testing against remote process succeeded~n", []),

    ok.

priority_messages_alias_enable_disable_test(Node) ->
    process_flag(trap_exit, true),
    {PS, MS} = spawn_opt(Node, fun SendPrioAlias () ->
                                       receive
                                           A -> erlang:send(A, A, [priority])
                                       end,
                                       SendPrioAlias()
                               end, [monitor,link]),
    {PE, ME} = spawn_opt(Node, fun ExitPrioAlias () ->
                                       receive
                                           A -> erlang:exit(A, A, [priority])
                                       end,
                                       ExitPrioAlias()
                               end, [monitor,link]),

    {priority_messages, false} = process_info(self(), priority_messages),
    A1 = alias([explicit_unalias, priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    PE ! A1,
    receive {'EXIT', PE, A1} -> ok end,
    {priority_messages, true} = process_info(self(), priority_messages),
    PS ! A1,
    receive A1 -> ok end,
    {priority_messages, true} = process_info(self(), priority_messages),
    unalias(A1),
    {priority_messages, false} = process_info(self(), priority_messages),

    A2 = alias([priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    PE ! A2,
    receive {'EXIT', PE, A2} -> ok end,
    {priority_messages, true} = process_info(self(), priority_messages),
    PS ! A2,
    receive A2 -> ok end,
    {priority_messages, true} = process_info(self(), priority_messages),
    unalias(A2),
    {priority_messages, false} = process_info(self(), priority_messages),

    A3 = alias([reply, priority]),
    {priority_messages, true} = process_info(self(), priority_messages),
    PE ! A3,
    receive {'EXIT', PE, A3} -> ok end,
    {priority_messages, true} = process_info(self(), priority_messages),
    PS ! A3,
    receive A3 -> ok end,
    {priority_messages, false} = process_info(self(), priority_messages),

    unlink(PS),
    unlink(PE),
    exit(PS, kill),
    exit(PE, kill),
    receive {'DOWN', MS, process, PS, killed} -> ok end,
    receive {'DOWN', ME, process, PE, killed} -> ok end.

priority_messages_order(Config) when is_list(Config) ->
    ct:log("Testing against local process~n", []),
    priority_messages_order_test(node()),
    ct:log("Testing against local process succeeded~n", []),
    {ok, Peer, Node} = ?CT_PEER(),
    ct:log("Testing against remote process~n", []),
    priority_messages_order_test(Node),
    ct:log("Testing against remote process succeeded~n", []),
    peer:stop(Peer),
    ok.

priority_messages_order_test(Node) ->
    Parent = self(),
    LinkProc1 = spawn(fun () -> receive after infinity -> ok end end),
    LinkProc2 = spawn(fun () -> receive after infinity -> ok end end),
    LinkProc3 = spawn(fun () -> receive after infinity -> ok end end),
    LinkProc4 = spawn(fun () -> receive after infinity -> ok end end),
    MonProc1 = spawn(fun () -> receive after infinity -> ok end end),
    MonProc2 = spawn(fun () -> receive after infinity -> ok end end),
    MonProc3 = spawn(fun () -> receive after infinity -> ok end end),
    MonProc4 = spawn(fun () -> receive after infinity -> ok end end),
    Rcvr = spawn_link(Node,
                      fun () ->
                              link(LinkProc1, [priority]),
                              link(LinkProc2, []),
                              link(LinkProc3, [priority]),
                              link(LinkProc4),
                              Mon1 = monitor(process, MonProc1, [priority]),
                              Mon2 = monitor(process, MonProc2, []),
                              Mon3 = monitor(process, MonProc3, [priority]),
                              Mon4 = monitor(process, MonProc4),
                              Parent ! {monitors, Mon1, Mon2, Mon3, Mon4},
                              process_flag(trap_exit, true),
                              PAlias = alias([priority]),
                              Parent ! {priority_alias, PAlias},
                              receive {Parent, go} -> ok end,
                              Msgs = get_all_msgs(),
                              unalias(PAlias),
                              Parent ! {self(), messages, Msgs},
                              receive {Parent, go} -> ok end,
                              Msgs2 = get_all_msgs(),
                              Parent ! {self(), messages, Msgs2},
                              receive after infinity -> ok end
                      end),
    {monitors, Mon1, Mon2, Mon3, Mon4}
        = receive
              {monitors, _, _, _, _} = Monitors ->
                  Monitors
          end,

    {priority_messages, true} = proc_info(Rcvr, priority_messages),

    {priority_alias, PrioAlias} = receive {priority_alias, _} = PA -> PA end,

    {priority_messages, true} = proc_info(Rcvr, priority_messages),

    Rcvr ! {msg, 1},
    wait_until(fun () -> msg_received(Rcvr, {msg, 1}) end),
    exit(LinkProc2, link_exit_1),
    wait_until(fun () -> exit_received(Rcvr, LinkProc2) end),
    exit(LinkProc1, prio_link_exit_1),
    wait_until(fun () -> exit_received(Rcvr, LinkProc1) end),
    exit(MonProc2, down_1),
    wait_until(fun () -> down_received(Rcvr, MonProc2) end),
    exit(MonProc1, prio_down_1),
    wait_until(fun () -> down_received(Rcvr, MonProc1) end),
    Rcvr ! {msg, 2},
    Rcvr ! {msg, 3},
    exit(PrioAlias, func_exit_1),
    Rcvr ! {msg, 4},
    exit(PrioAlias, prio_func_exit_1, [priority]),
    exit(Rcvr, {func_exit, 2}, [priority]),
    exit(PrioAlias, func_exit_3),
    Rcvr ! {msg, 5},
    PrioAlias ! {msg, 6},
    erlang:send(PrioAlias, {prio_msg, 1}, [priority]),
    erlang:send(PrioAlias, {msg, 7}, []),
    erlang:send(PrioAlias, prio_msg_2, [priority]),
    erlang:send(Rcvr, {msg, 8}, [priority]),
    wait_until(fun () -> msg_received(Rcvr, {msg, 8}) end),
    exit(LinkProc4, {link_exit, 2}),
    wait_until(fun () -> exit_received(Rcvr, LinkProc4) end),
    exit(LinkProc3, {prio_link_exit, 2}),
    wait_until(fun () -> exit_received(Rcvr, LinkProc3) end),
    exit(MonProc4, {down, 2}),
    wait_until(fun () -> down_received(Rcvr, MonProc4) end),
    exit(MonProc3, {prio_down, 2}),
    wait_until(fun () -> down_received(Rcvr, MonProc3) end),

    Expect = [
              %% Priority messages
              {'EXIT', LinkProc1, prio_link_exit_1},
              {'DOWN', Mon1, process, MonProc1, prio_down_1},
              {'EXIT', Parent, prio_func_exit_1},
              {prio_msg, 1},
              prio_msg_2,
              {'EXIT', LinkProc3, {prio_link_exit, 2}},
              {'DOWN', Mon3, process, MonProc3, {prio_down, 2}},

              %% Ordinary messages
              {msg, 1},
              {'EXIT', LinkProc2, link_exit_1},
              {'DOWN', Mon2, process, MonProc2, down_1},
              {msg, 2},
              {msg, 3},
              {'EXIT', Parent, func_exit_1},
              {msg, 4},
              {'EXIT', Parent, {func_exit, 2}},
              {'EXIT', Parent, func_exit_3},
              {msg, 5},
              {msg, 6},
              {msg, 7},
              {msg, 8},
              {'EXIT', LinkProc4, {link_exit, 2}},
              {'DOWN', Mon4, process, MonProc4, {down, 2}}],

    {messages, PIMessages} = proc_info(Rcvr, messages),

    case Expect =:= PIMessages of
        true ->
            ct:log("Process-info messages as expected!~n", []);
        false ->
            ct:log("Expected:~n ~p~n~nGot process info messages:~n ~p~n", [Expect, PIMessages]),
            ct:fail(invalid_message_queue)
    end,

    Rcvr ! {self(), go},

    RecvMessages = receive
                       {Rcvr, messages, Msgs} ->
                           Msgs
                   end,

    case Expect =:= RecvMessages of
        true ->
            ct:log("Received messages as expected!~n", []);
        false ->
            ct:log("Expected:~n ~p~n~nGot received messages:~n ~p~n", [Expect, RecvMessages]),
            ct:fail(invalid_message_queue)
    end,

    {priority_messages, false} = proc_info(Rcvr, priority_messages),

    Rcvr ! a_message,

    exit(PrioAlias, {prio_func_exit, 2}, [priority]),
    exit(PrioAlias, {func_exit, 2}),
    erlang:send(PrioAlias, {prio_msg, 3}, [priority]),
    erlang:send(PrioAlias, {msg, 9}, []),

    Rcvr ! {self(), go},

    [a_message] = receive
                       {Rcvr, messages, Msgs2} ->
                           Msgs2
                   end,

    unlink(Rcvr),
    exit(Rcvr, kill),
    ok.

priority_messages_hopefull_encoding(Config) when is_list(Config) ->
    ct:log("Removing DFLAG_ALTACT_SIG test~n", []),
    priority_messages_hopefull_encoding_test(?DFLAG_ALTACT_SIG, false),
    ct:log("Removing DFLAG_ALTACT_SIG and DFLAG_SEND_SENDER test~n", []),
    priority_messages_hopefull_encoding_test(?DFLAG_ALTACT_SIG bor ?DFLAG_SEND_SENDER, false),
    ct:log("Removing DFLAG_ALTACT_SIG and DFLAG_EXIT_PAYLOAD test~n", []),
    priority_messages_hopefull_encoding_test(?DFLAG_ALTACT_SIG bor ?DFLAG_EXIT_PAYLOAD, false),

    ct:log("Removing DFLAG_ALTACT_SIG test run with seq-trace~n", []),
    priority_messages_hopefull_encoding_test(?DFLAG_ALTACT_SIG, true),
    ct:log("Removing DFLAG_ALTACT_SIG and DFLAG_SEND_SENDER test run with seq-trace~n", []),
    priority_messages_hopefull_encoding_test(?DFLAG_ALTACT_SIG bor ?DFLAG_SEND_SENDER, true),
    ct:log("Removing DFLAG_ALTACT_SIG and DFLAG_EXIT_PAYLOAD test run with seq-trace~n", []),
    priority_messages_hopefull_encoding_test(?DFLAG_ALTACT_SIG bor ?DFLAG_EXIT_PAYLOAD, true),
    ct:log("Done~n", []),
    ok.

priority_messages_hopefull_encoding_test(RmDFlags, SeqTrace) ->
    Self = self(),
    {ok, Peer, Node} = ?CT_PEER(#{connection => 0}),
    peer:call(Peer, erts_debug, set_internal_state, [available_internal_state, true]),
    peer:call(Peer, erts_debug, set_internal_state, [remove_dflags, RmDFlags]),
    verify_dflags_removed(Peer, RmDFlags),
    Fun = fun () ->
                  register(hopefull_enc_recv__,self()),
                  process_flag(trap_exit, true),
                  Alias = alias([priority]),
                  put(my_alias, Alias),
                  self() ! msg_1,
                  receive after infinity -> ok end
          end,
    Pid = peer:call(Peer, erlang, spawn, [erlang, apply, [Fun, []]]),
    wait_until(fun () ->
                       {{dictionary, my_alias}, undefined}
                           /= peer:call(Peer, erlang, process_info,
                                        [Pid, {dictionary, my_alias}])
               end),
    {{dictionary, my_alias}, Alias} = peer:call(Peer, erlang, process_info,
                                                 [Pid, {dictionary, my_alias}]),

    ST1 = setup_seq_trace(SeqTrace, 1),
    false = lists:member(Node, nodes()),
    erlang:send(Alias, prio_msg_1, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_1) end),
    erlang:send(Alias, prio_msg_2, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_2) end),
    {messages, [msg_1, prio_msg_1, prio_msg_2]} = proc_info(Pid, messages),
    finish_seq_trace(ST1),

    node_disconnect(Node),
    ST2 = setup_seq_trace(SeqTrace, 2),
    erlang:send(Pid, prio_msg_3, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_3) end),
    erlang:send(Pid, prio_msg_4, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_4) end),
    {messages, [msg_1, prio_msg_1, prio_msg_2, prio_msg_3, prio_msg_4]}
        = proc_info(Pid, messages),
    finish_seq_trace(ST2),

    node_disconnect(Node),
    ST3 = setup_seq_trace(SeqTrace, 3),
    erlang:send({hopefull_enc_recv__, Node}, prio_msg_5, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_5) end),
    erlang:send({hopefull_enc_recv__, Node}, prio_msg_6, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_6) end),
    {messages, [msg_1, prio_msg_1, prio_msg_2, prio_msg_3, prio_msg_4,
                prio_msg_5, prio_msg_6]}
        = proc_info(Pid, messages),
    finish_seq_trace(ST3),

    node_disconnect(Node),
    ST4 = setup_seq_trace(SeqTrace, 4),
    exit(Alias, prio_exit_1, [priority]),
    wait_until(fun () -> lists:member(Node, nodes()) end),
    exit(Alias, prio_exit_2, [priority]),
    finish_seq_trace(ST4),

    node_disconnect(Node),
    ST5 = setup_seq_trace(SeqTrace, 5),
    exit(Pid, prio_exit_3, [priority]),
    wait_until(fun () -> msg_received(Pid, {'EXIT', Self, prio_exit_3}) end),
    exit(Pid, prio_exit_4, [priority]),
    wait_until(fun () -> msg_received(Pid, {'EXIT', Self, prio_exit_4}) end),
    {messages, [msg_1, prio_msg_1, prio_msg_2, prio_msg_3, prio_msg_4,
                prio_msg_5, prio_msg_6, {'EXIT', Self, prio_exit_3},
                {'EXIT', Self, prio_exit_4}]}
        = proc_info(Pid, messages),
    finish_seq_trace(ST5),

    peer:stop(Peer),

    ok.

setup_seq_trace(false, _Label) ->
    undefined;
setup_seq_trace(true, Label) ->
    Tracer = spawn_link(fun () ->
                                receive
                                    {get_seq_trace, Label, From} ->
                                        From ! {seq_trace, Label, get_all_msgs()}
                                end
                        end),
    seq_trace:set_system_tracer(Tracer),
    seq_trace:reset_trace(),
    0 = seq_trace:set_token(label,Label),
    false = seq_trace:set_token(send,true),
    {label, Label} = seq_trace:get_token(label),
    {send, true} = seq_trace:get_token(send),
    {Tracer, Label}.

finish_seq_trace(undefined) ->
    ok;
finish_seq_trace({Tracer, Label}) when is_pid(Tracer) ->
    Tracer ! {get_seq_trace, Label, self()},
    SeqTrace = receive
                   {seq_trace, Label, Msgs} ->
                       Msgs
               end,
    ct:log("Seq-trace ~p:~n~n~p~n", [Label, SeqTrace]),
    ok.

verify_dflags_removed(Peer, DFlags) ->
    {erts_dflags, Default, Mandatory, Addable, Rejectable, StrictOrder}
        = peer:call(Peer, erts_internal, get_dflags, []),
    0 = Default band DFlags,
    0 = Mandatory band DFlags,
    0 = Addable band DFlags,
    0 = Rejectable band DFlags,
    0 = StrictOrder band DFlags,
    ok.

priority_messages_old_nodes(Config) when is_list(Config) ->
    ct:log("Running test against current release~n", []),
    {ok, PeerThis, NodeThis} = ?CT_PEER(#{connection => 0}),
    priority_messages_old_nodes_test(Config, NodeThis),
    peer:stop(PeerThis),
    ct:log("ok~n", []),
    {OldRelName1, OldRel1} = release_names(-1),
    ct:log("Running test against ~s release~n", [OldRel1]),
    C1 = case ?CT_PEER_REL(#{connection => 0},
                           OldRelName1,
                           proplists:get_value(priv_dir, Config)) of
             not_available ->
                 Cmt1 = " Not able to start an OTP "++OldRel1++" node.",
                 ct:log("~s~n", [Cmt1]),
                 Cmt1;
             {ok, PeerOld1, NodeOld1} ->
                 priority_messages_old_nodes_test(Config, NodeOld1),
                 peer:stop(PeerOld1),
                 ct:log("ok~n", []),
                 []
         end,
    {OldRelName2, OldRel2} = release_names(-2),
    ct:log("Running test against ~s release~n", [OldRel2]),
    C2 = case ?CT_PEER_REL(#{connection => 0},
                           OldRelName2,
                           proplists:get_value(priv_dir, Config)) of
             not_available ->
                 Cmt2 = " Not able to start an OTP "++OldRel2++" node.",
                 ct:log("~s~n", [Cmt2]),
                 Cmt2;
             {ok, PeerOld2, NodeOld2} ->
                 priority_messages_old_nodes_test(Config, NodeOld2),
                 peer:stop(PeerOld2),
                 ct:log("ok~n", []),
                 []
         end,
    case C1++C2 of
        [] ->
            ok;
        Comment ->
            {comment, Comment}
    end.

priority_messages_old_nodes_test(Config, Node) ->
    Self = self(),
    Self ! ordinary_message,
    MyPrioAlias = alias([priority]),
    PrioMsgSupport = try
                         erpc:call(Node, erlang, send,
                                   [MyPrioAlias, prio_message, [priority]]),
                         prio_message = get_one_msg(1000),
                         true
                     catch
                         _ : _ ->
                             false
                     end,
    try
        ordinary_message = get_one_msg()
    catch
        error:no_message ->
            halt(abort)
    end,
    unalias(MyPrioAlias),

    SrcFile = filename:join(proplists:get_value(priv_dir, Config),
                            "prio_messages_old_nodes_code.erl"),
    ok = file:write_file(SrcFile,
                         """
       -module(prio_messages_old_nodes_code).
       -export([proc/2]).
       proc(Tester, AliasArgs) ->
           register(hopefull_enc_recv__,self()),
           process_flag(trap_exit, true),
           Alias = alias(AliasArgs),
           put(my_alias, Alias),
           self() ! msg_1,
           Tester ! {self(), initialized},
           receive after infinity -> ok end.
       """),

    {ok, prio_messages_old_nodes_code, BeamCode} = erpc:call(Node, compile, file,
                                                             [SrcFile, [binary]]),
    {module, prio_messages_old_nodes_code} = erpc:call(Node, code, load_binary,
                                                       [prio_messages_old_nodes_code,
                                                        SrcFile, BeamCode]),

    Pid = spawn(Node,
                prio_messages_old_nodes_code,
                proc,
                [Self,
                 case PrioMsgSupport of
                     true -> [priority];
                     false -> []
                 end]),

    receive {Pid, initialized} -> ok end,

    Alias = try
                {{dictionary, my_alias}, Alias1}
                    = proc_info(Pid, {dictionary, my_alias}),
                Alias1
            catch
                _:_ ->
                    {dictionary, Dict} = proc_info(Pid, dictionary),
                    {my_alias, Alias2} = lists:keyfind(my_alias, 1, Dict),
                    Alias2
            end,

    node_disconnect(Node),
    erlang:send(Alias, prio_msg_1, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_1) end),
    erlang:send(Alias, prio_msg_2, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_2) end),
    case PrioMsgSupport of
        true ->
            {messages, [prio_msg_1, prio_msg_2, msg_1]} = proc_info(Pid, messages);
        false ->
            {messages, [msg_1, prio_msg_1, prio_msg_2]} = proc_info(Pid, messages)
    end,

    node_disconnect(Node),
    erlang:send(Pid, prio_msg_3, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_3) end),
    erlang:send(Pid, prio_msg_4, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_4) end),
    case PrioMsgSupport of
        true ->
            {messages, [prio_msg_1, prio_msg_2, msg_1, prio_msg_3, prio_msg_4]}
                = proc_info(Pid, messages);
        false ->
            {messages, [msg_1, prio_msg_1, prio_msg_2, prio_msg_3, prio_msg_4]}
                = proc_info(Pid, messages)
    end,

    node_disconnect(Node),
    erlang:send({hopefull_enc_recv__, Node}, prio_msg_5, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_5) end),
    erlang:send({hopefull_enc_recv__, Node}, prio_msg_6, [priority]),
    wait_until(fun () -> msg_received(Pid, prio_msg_6) end),
    case PrioMsgSupport of
        true ->
            {messages, [prio_msg_1, prio_msg_2, msg_1, prio_msg_3, prio_msg_4,
                        prio_msg_5, prio_msg_6]}
                = proc_info(Pid, messages);
        false ->
            {messages, [msg_1, prio_msg_1, prio_msg_2, prio_msg_3, prio_msg_4,
                        prio_msg_5, prio_msg_6]}
                = proc_info(Pid, messages)
    end,

    node_disconnect(Node),
    exit(Alias, prio_exit_1, [priority]),
    wait_until(fun () -> lists:member(Node, nodes()) end),
    exit(Alias, prio_exit_2, [priority]),
    Pid ! msg_2,
    wait_until(fun () -> msg_received(Pid, msg_2) end),

    node_disconnect(Node),
    exit(Pid, prio_exit_3, [priority]),
    wait_until(fun () -> msg_received(Pid, {'EXIT', Self, prio_exit_3}) end),
    exit(Pid, prio_exit_4, [priority]),
    wait_until(fun () -> msg_received(Pid, {'EXIT', Self, prio_exit_4}) end),
    case PrioMsgSupport of
        true ->
            {messages, [prio_msg_1, prio_msg_2, {'EXIT', Self, prio_exit_1},
                        {'EXIT', Self, prio_exit_2}, msg_1, prio_msg_3,
                        prio_msg_4, prio_msg_5, prio_msg_6, msg_2,
                        {'EXIT', Self, prio_exit_3}, {'EXIT', Self, prio_exit_4}]}
                = proc_info(Pid, messages);
        false ->
            {messages, [msg_1, prio_msg_1, prio_msg_2, prio_msg_3, prio_msg_4,
                        prio_msg_5, prio_msg_6, msg_2,
                        {'EXIT', Self, prio_exit_3}, {'EXIT', Self, prio_exit_4}]}
                = proc_info(Pid, messages)
    end,

    ok.

%%
%% -- Internal utils --------------------------------------------------------
%%

node_disconnect(Node) ->
    erlang:disconnect_node(Node),
    wait_until(fun () ->
                       false == lists:member(Node, nodes())
               end).

msg_received(Pid, Msg) ->
    {messages, Msgs} = proc_info(Pid, messages),
    try
        lists:foreach(fun (M) when M == Msg ->
                              throw(true);
                          (_) ->
                              ok
                      end, Msgs),
        false
    catch
        throw:true ->
            true
    end.

exit_received(Pid, From) ->
    {messages, Msgs} = proc_info(Pid, messages),
    try
        lists:foreach(fun ({'EXIT', P, _}) when P == From ->
                              throw(true);
                          (_) ->
                              ok
                      end, Msgs),
        false
    catch
        throw:true ->
            true
    end.

down_received(Pid, From) ->
    {messages, Msgs} = proc_info(Pid, messages),
    try
        lists:foreach(fun ({'DOWN', _, process, P, _}) when P == From ->
                              throw(true);
                          (_) ->
                              ok
                      end, Msgs),
        false
    catch
        throw:true ->
            true
    end.

proc_info(Pid, What) when node(Pid) == node() ->
    process_info(Pid, What);
proc_info(Pid, What) ->
    erpc:call(node(Pid), erlang, process_info, [Pid, What]).

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

get_one_msg() ->
    get_one_msg(0).

get_one_msg(Tmo) ->
    receive
        Msg ->
            Msg
    after Tmo ->
            error(no_message)
    end.

get_all_msgs() ->
    get_all_msgs([]).

get_all_msgs(Msgs) ->
    receive
        Msg ->
            get_all_msgs([Msg|Msgs])
    after
        0 ->
            lists:reverse(Msgs)
    end.

release_names(N) ->
    OldRel = integer_to_list(list_to_integer(erlang:system_info(otp_release))+N),
    {OldRel++"_latest", OldRel}.
