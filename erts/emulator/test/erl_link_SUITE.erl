%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

%%%----------------------------------------------------------------------
%%% File    : erl_link_SUITE.erl
%%% Author  : Rickard Green <rickard.green@uab.ericsson.se>
%%% Purpose : Test erlang links
%%% Created : 13 Dec 2001 by Rickard Green <rickard.green@uab.ericsson.se>
%%%----------------------------------------------------------------------

-module(erl_link_SUITE).
-author('rickard.green@uab.ericsson.se').

%-define(line_trace, 1).
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

% Test cases
-export([links/1,
         dist_links/1,
         monitor_nodes/1,
         process_monitors/1,
         dist_process_monitors/1,
         busy_dist_port_monitor/1,
         busy_dist_port_link/1,
         otp_5772_link/1,
         otp_5772_dist_link/1,
         otp_5772_monitor/1,
         otp_5772_dist_monitor/1,
         otp_7946/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

% Internal exports
-export([test_proc/0]).


-define(LINK_UNDEF, 0).
-define(LINK_PID,   1).
-define(LINK_NODE,  3).


% These are to be kept in sync with erl_monitors.h 
-define(MON_ORIGIN, 1).
-define(MON_TARGET, 3).


-record(erl_link, {type = ?LINK_UNDEF,
                   pid = [],
                   targets = []}).

% This is to be kept in sync with erl_bif_info.c (make_monitor_list)

-record(erl_monitor, {type,        % MON_ORIGIN or MON_TARGET (1 or 3)
                      ref,
                      pid,         % Process or nodename
                      name = []}). % registered name or []


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [links, dist_links, monitor_nodes, process_monitors,
     dist_process_monitors, busy_dist_port_monitor,
     busy_dist_port_link, otp_5772_link, otp_5772_dist_link,
     otp_5772_monitor, otp_5772_dist_monitor, otp_7946].

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    case catch erts_debug:get_internal_state(available_internal_state) of
        true -> ok;
        _ -> erts_debug:set_internal_state(available_internal_state, true)
    end,
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false).


%% Tests node local links
links(Config) when is_list(Config) ->
    common_link_test(node(), node()),
    true = link(self()),
    [] = find_erl_link(self(), ?LINK_PID, self()),
    true = unlink(self()),
    ok.

%% Tests distributed links
dist_links(Config) when is_list(Config) ->
    [NodeName] = get_names(1, dist_link),
    {ok, Node} = start_node(NodeName),
    common_link_test(node(), Node),
    TP4 = spawn(?MODULE, test_proc, []),
    TP5 = spawn(?MODULE, test_proc, []),
    TP6 = spawn(Node, ?MODULE, test_proc, []),
    true = tp_call(TP6, fun() -> link(TP4) end),
    check_link(TP4, TP6),
    true = tp_call(TP5,
                   fun() ->
                           process_flag(trap_exit,true),
                           link(TP6)
                   end),
    check_link(TP5, TP6),
    rpc:cast(Node, erlang, halt, []),
    wait_until(fun () -> is_proc_dead(TP4) end),
    check_unlink(TP4, TP6),
    true = tp_call(TP5,
                   fun() ->
                           receive
                               {'EXIT', TP6, noconnection} ->
                                   true
                           end
                   end),
    check_unlink(TP5, TP6),
    tp_cast(TP5, fun() -> exit(normal) end),
    ok.

common_link_test(NodeA, NodeB) ->
    TP1 = spawn(NodeA, ?MODULE, test_proc, []),
    check_unlink(TP1, self()),
    TP2 = tp_call(TP1,
                  fun () ->
                          spawn_link(NodeB, ?MODULE, test_proc, [])
                  end),
    check_link(TP1, TP2),
    true = tp_call(TP2, fun() -> unlink(TP1) end),
    check_unlink(TP1, TP2),
    true = tp_call(TP2, fun() -> link(TP1) end),
    check_link(TP1, TP2),
    false = tp_call(TP2, fun() -> process_flag(trap_exit, true) end),
    tp_cast(TP1, fun () -> exit(died) end),
    true = tp_call(TP2, fun() ->
                                receive 
                                    {'EXIT', TP1, died} ->
                                        true
                                end
                        end),
    check_unlink(TP1, TP2),
    TP3 = tp_call(TP2,
                  fun () ->
                          spawn_link(NodeA, ?MODULE, test_proc, [])
                  end),
    check_link(TP3, TP2),
    tp_cast(TP2, fun() -> exit(died) end),
    wait_until(fun () -> is_proc_dead(TP3) end),
    check_unlink(TP3, TP2),
    ok.

%% Tests monitor of nodes
monitor_nodes(Config) when is_list(Config) ->
    [An, Bn, Cn, Dn] = get_names(4, dist_link),
    {ok, A} = start_node(An),
    {ok, B} = start_node(Bn),
    C = list_to_atom(lists:concat([Cn, "@", hostname()])),
    D = list_to_atom(lists:concat([Dn, "@", hostname()])),
    0 = no_of_monitor_node(self(), A),
    0 = no_of_monitor_node(self(), B),
    monitor_node(A, true),
    monitor_node(B, true),
    monitor_node(D, true),
    monitor_node(D, true),

    %% Has been known to crash the emulator.
    {memory,_} = process_info(self(), memory),

    monitor_node(A, false),
    monitor_node(B, true),
    monitor_node(C, true),
    monitor_node(C, false),
    monitor_node(C, true),
    monitor_node(B, true),
    monitor_node(A, false),
    monitor_node(B, true),
    monitor_node(B, false),
    monitor_node(A, true),
    check_monitor_node(self(), A, 1),
    check_monitor_node(self(), B, 3),
    check_monitor_node(self(), C, 0),
    check_monitor_node(self(), D, 0),
    receive {nodedown, C} -> ok end,
    receive {nodedown, C} -> ok end,
    receive {nodedown, C} -> ok end,
    receive {nodedown, D} -> ok end,
    receive {nodedown, D} -> ok end,
    stop_node(A),
    receive {nodedown, A} -> ok end,
    check_monitor_node(self(), A, 0),
    check_monitor_node(self(), B, 3),
    stop_node(B),
    receive {nodedown, B} -> ok end,
    receive {nodedown, B} -> ok end,
    receive {nodedown, B} -> ok end,
    check_monitor_node(self(), B, 0),
    receive
        {nodedown, X} ->
            ct:fail({unexpected_nodedown, X})
    after 0 ->
              ok
    end,
    ok.


%% Tests node local process monitors
process_monitors(Config) when is_list(Config) ->
    common_process_monitors(node(), node()),
    Mon1 = erlang:monitor(process,self()),
    [] = find_erl_monitor(self(), Mon1),
    [Name] = get_names(1, process_monitors),
    true = register(Name, self()),
    Mon2 = erlang:monitor(process, Name),
    [] = find_erl_monitor(self(), Mon2),
    receive
        {'DOWN', Mon1, _, _, _} = Msg ->
            ct:fail({unexpected_down_msg, Msg});
        {'DOWN', Mon2, _, _, _} = Msg ->
            ct:fail({unexpected_down_msg, Msg})
    after 500 ->
              true = erlang:demonitor(Mon1),
              true = erlang:demonitor(Mon2),
              ok
    end.

%% Tests distributed process monitors
dist_process_monitors(Config) when is_list(Config) -> 
    [Name] = get_names(1,dist_process_monitors),
    {ok, Node} = start_node(Name),
    common_process_monitors(node(), Node),
    TP1 = spawn(Node, ?MODULE, test_proc, []),
    R1 = erlang:monitor(process, TP1),
    TP1O = get_down_object(TP1, self()),
    check_process_monitor(self(), TP1, R1),
    tp_cast(TP1, fun () -> halt() end),
    receive
        {'DOWN',R1,process,TP1O,noconnection} ->
            ok
    end,
    check_process_demonitor(self(), TP1, R1),
    R2 = erlang:monitor(process, TP1),
    receive
        {'DOWN',R2,process,TP1O,noconnection} ->
            ok
    end,
    check_process_demonitor(self(), TP1, R2),
    ok.


common_process_monitors(NodeA, NodeB) ->
    TP1 = spawn(NodeA, ?MODULE, test_proc, []),
    TP2 = spawn(NodeB, ?MODULE, test_proc, []),
    run_common_process_monitors(TP1, TP2),
    TP3 = spawn(NodeA, ?MODULE, test_proc, []),
    TP4 = spawn(NodeB, ?MODULE, test_proc, []),
    [TP4N] = get_names(1, common_process_monitors),
    true = tp_call(TP4, fun () -> register(TP4N,self()) end),
    run_common_process_monitors(TP3,
                                case node() == node(TP4) of
                                    true -> TP4N;
                                    false -> {TP4N, node(TP4)}
                                end),
    ok.

run_common_process_monitors(TP1, TP2) ->
    R1 = tp_call(TP1, fun () -> erlang:monitor(process, TP2) end),
    check_process_monitor(TP1, TP2, R1),

    tp_call(TP2, fun () -> catch erlang:demonitor(R1) end),
    check_process_monitor(TP1, TP2, R1),

    true = tp_call(TP1, fun () -> erlang:demonitor(R1) end),
    check_process_demonitor(TP1, TP2, R1),

    R2 = tp_call(TP1, fun () -> erlang:monitor(process, TP2) end),
    TP2O = get_down_object(TP2, TP1),
    check_process_monitor(TP1, TP2, R2),
    tp_cast(TP2, fun () -> exit(bye) end),
    wait_until(fun () -> is_proc_dead(TP2) end),
    ok = tp_call(TP1, fun () ->
                              receive
                                  {'DOWN',R2,process,TP2O,bye} ->
                                      ok
                              end
                      end),
    check_process_demonitor(TP1, TP2, R2),

    R3 = tp_call(TP1, fun () -> erlang:monitor(process, TP2) end),
    ok = tp_call(TP1, fun () ->
                              receive
                                  {'DOWN',R3,process,TP2O,noproc} ->
                                      ok
                              end
                      end),
    check_process_demonitor(TP1, TP2, R3),

    tp_cast(TP1, fun () -> exit(normal) end),
    wait_until(fun () -> is_proc_dead(TP1) end),
    ok.


%% Tests distributed monitor/2, demonitor/1, and 'DOWN' message
%% over busy distribution port
busy_dist_port_monitor(Config) when is_list(Config) ->

    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
                 "true" -> start_busy_dist_port_tracer();
                 _ -> false
             end,

    [An] = get_names(1, busy_dist_port_monitor),
    {ok, A} = start_node(An),
    TP1 = spawn(A, ?MODULE, test_proc, []),
    %% Check monitor over busy port
    M1 = suspend_on_busy_test(A,
                              "erlang:monitor(process, TP1)",
                              fun () -> erlang:monitor(process, TP1) end),
    check_process_monitor(self(), TP1, M1),
    %% Check demonitor over busy port
    suspend_on_busy_test(A,
                         "erlang:demonitor(M1)",
                         fun () -> erlang:demonitor(M1) end),
    check_process_demonitor(self(), TP1, M1),
    %% Check down message over busy port
    TP2 = spawn(?MODULE, test_proc, []),
    M2 = tp_call(TP1, fun () -> erlang:monitor(process, TP2) end),
    check_process_monitor(TP1, TP2, M2),
    Ref = make_ref(),
    Busy = make_busy(A, 1000),
    receive after 100 -> ok end,
    tp_cast(TP2, fun () -> exit(Ref) end),
    receive after 100 -> ok end,
    unmake_busy(Busy),
    Ref = tp_call(TP1, fun () ->
                               receive
                                   {'DOWN', M2, process, TP2, Ref} ->
                                       Ref
                               end
                       end),
    tp_cast(TP1, fun () -> exit(normal) end),
    stop_node(A),
    stop_busy_dist_port_tracer(Tracer),
    ok.

%% Tests distributed link/1, unlink/1, and 'EXIT'
%% message over busy distribution port
busy_dist_port_link(Config) when is_list(Config) ->
    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
                 "true" -> start_busy_dist_port_tracer();
                 _ -> false
             end,

    [An] = get_names(1, busy_dist_port_link),
    {ok, A} = start_node(An),
    TP1 = spawn(A, ?MODULE, test_proc, []),
    %% Check link over busy port
    suspend_on_busy_test(A,
                         "link(TP1)",
                         fun () -> link(TP1) end),
    check_link(self(), TP1),
    %% Check unlink over busy port
    suspend_on_busy_test(A,
                         "unlink(TP1)",
                         fun () -> unlink(TP1) end),
    check_unlink(self(), TP1),
    %% Check trap exit message over busy port
    TP2 = spawn(?MODULE, test_proc, []),
    ok = tp_call(TP1, fun () ->
                              process_flag(trap_exit, true),
                              link(TP2),
                              ok
                      end),
    check_link(TP1, TP2),
    Ref = make_ref(),
    Busy = make_busy(A, 1000),
    receive after 100 -> ok end,
    tp_cast(TP2, fun () -> exit(Ref) end),
    receive after 100 -> ok end,
    unmake_busy(Busy),
    Ref = tp_call(TP1, fun () ->
                               receive
                                   {'EXIT', TP2, Ref} ->
                                       Ref
                               end
                       end),
    tp_cast(TP1, fun () -> exit(normal) end),
    stop_node(A),
    stop_busy_dist_port_tracer(Tracer),
    ok.


otp_5772_link(Config) when is_list(Config) ->
    otp_5772_link_test(node()).

otp_5772_dist_link(Config) when is_list(Config) ->
    [An] = get_names(1, otp_5772_dist_link),
    {ok, A} = start_node(An),
    otp_5772_link_test(A),
    stop_node(A).

otp_5772_link_test(Node) ->
    Prio = process_flag(priority, high),
    TE = process_flag(trap_exit, true),
    TP1 = spawn_opt(Node, ?MODULE, test_proc, [],
                    [link, {priority, low}]),
    exit(TP1, bang),
    unlink(TP1),
    receive
        {'EXIT', TP1, _} ->
            ok
    after 0 ->
              ok
    end,
    receive
        {'EXIT', TP1, _} = Exit ->
            ct:fail({got_late_exit_message, Exit})
    after 1000 ->
              ok
    end,
    process_flag(trap_exit, TE),
    process_flag(priority, Prio),
    ok.

otp_5772_monitor(Config) when is_list(Config) ->
    otp_5772_monitor_test(node()).

otp_5772_dist_monitor(Config) when is_list(Config) ->
    [An] = get_names(1, otp_5772_dist_monitor),
    {ok, A} = start_node(An),
    otp_5772_monitor_test(A),
    stop_node(A),
    ok.

otp_5772_monitor_test(Node) ->
    Prio = process_flag(priority, high),
    TP1 = spawn_opt(Node, ?MODULE, test_proc, [], [{priority, low}]),
    M1 = erlang:monitor(process, TP1),
    exit(TP1, bang),
    erlang:demonitor(M1),
    receive
        {'DOWN', M1, _, _, _} ->
            ok
    after 0 ->
              ok
    end,
    receive
        {'DOWN', M1, _, _, _} = Down ->
            ct:fail({got_late_down_message, Down})
    after 1000 ->
              ok
    end,
    process_flag(priority, Prio),
    ok.

otp_7946(Config) when is_list(Config) ->
    [NodeName] = get_names(1, otp_7946),
    {ok, Node} = start_node(NodeName),
    Proc = rpc:call(Node, erlang, whereis, [net_kernel]),
    Mon = erlang:monitor(process, Proc),
    rpc:cast(Node, erlang, halt, []),
    receive {'DOWN', Mon, process, Proc , _} -> ok end,
    {Linker, LMon} = spawn_monitor(fun () ->
                                           link(Proc),
                                           receive
                                           after infinity -> ok
                                           end
                                   end),
    receive
        {'DOWN', LMon, process, Linker, Reason} ->
            io:format("Reason=~p~n", [Reason]),
            Reason = noconnection
    end.

%%
%% -- Internal utils --------------------------------------------------------
%%

-define(BUSY_DATA_KEY, '__busy__port__data__').
-define(BUSY_DATA_SIZE, 1024*1024).

busy_data() ->
    case get(?BUSY_DATA_KEY) of
        undefined ->
            set_busy_data([]);
        Data ->
            true = is_binary(Data),
            true = size(Data) == ?BUSY_DATA_SIZE,
            Data
    end.

set_busy_data(SetData) ->
    case get(?BUSY_DATA_KEY) of
        undefined ->
            Data = case SetData of
                       D when is_binary(D), size(D) == ?BUSY_DATA_SIZE ->
                           SetData;
                       _ ->
                           list_to_binary(lists:duplicate(?BUSY_DATA_SIZE, 253))
                   end,
            put(?BUSY_DATA_KEY, Data),
            Data;
        OldData ->
            OldData
    end.

freeze_node(Node, MS) ->
    Own = 500,
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

make_busy(Node, Time) when is_integer(Time) ->
    Own = 500,
    freeze_node(Node, Time+Own), 
    Data = busy_data(),
    %% first make port busy
    Pid = spawn_link(fun () ->
                             forever(fun () ->
                                             dport_reg_send(Node,
                                                            '__noone__',
                                                            Data)
                                     end)
                     end),
    receive after Own -> ok end,
    wait_until(fun () ->
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

suspend_on_busy_test(Node, Doing, Fun) ->
    Tester = self(),
    DoIt = make_ref(),
    Done = make_ref(),
    Data = busy_data(),
    spawn_link(fun () ->
                       set_busy_data(Data),
                       Busy = make_busy(Node, 1000),
                       Tester ! DoIt,
                       receive after 100 -> ok end,
                       Info = process_info(Tester, [status, current_function]),
                       unmake_busy(Busy),
                       io:format("~p doing ~s: ~p~n", [Tester, Doing, Info]),
                       Tester ! {Done, Info}
               end),
    receive DoIt -> ok end,
    Res = Fun(),
    receive
        {Done, MyInfo} ->
            %% Don't match arity; it is different in
            %% debug and optimized emulator
            [{status, suspended},
             {current_function, {erlang, bif_return_trap, _}}] = MyInfo,
            ok
    end,
    Res.

% get_node(Name) when is_atom(Name) ->
%     node();
% get_node({Name, Node}) when is_atom(Name) ->
%     Node;
% get_node(NC) when is_pid(NC); is_port(NC); is_reference(NC) ->
%     node(NC).

get_down_object(Item, _) when is_pid(Item) ->
    Item;
get_down_object({Name, Node} = Item, _) when is_atom(Name); is_atom(Node) ->
    Item;
get_down_object(Item, Watcher) when is_atom(Item), is_pid(Watcher) ->
    {Item, node(Watcher)};
get_down_object(Item, {_,Node}) when is_atom(Item), is_atom(Node) ->
    {Item, Node};
get_down_object(Item, Watcher) when is_atom(Item), is_atom(Watcher) ->
    {Item, node()}.

is_proc_dead(P) ->
    case is_proc_alive(P) of
        true -> false;
        false -> true
    end.

is_proc_alive(Pid) when is_pid(Pid), node(Pid) == node() ->
    is_process_alive(Pid);
is_proc_alive(Name) when is_atom(Name) ->
    case catch whereis(Name) of
        Pid when is_pid(Pid) ->
            is_proc_alive(Pid);
        _ ->
            false
    end;
is_proc_alive({Name, Node}) when is_atom(Name), Node == node() ->
    is_proc_alive(Name);
is_proc_alive(Proc) ->
    is_remote_proc_alive(Proc).

is_remote_proc_alive({Name, Node}) when is_atom(Name), is_atom(Node) ->
    is_remote_proc_alive(Name, Node);
is_remote_proc_alive(Pid) when is_pid(Pid) ->
    is_remote_proc_alive(Pid, node(Pid));
is_remote_proc_alive(_) ->
    false.

is_remote_proc_alive(PN, Node) ->		 
    S = self(),
    R = make_ref(),
    monitor_node(Node, true),
    _P = spawn(Node, fun () -> S ! {R, is_proc_alive(PN)} end),
    receive
        {R, Bool} ->
            monitor_node(Node, false),
            Bool;
        {nodedown, Node} ->
            false
    end.

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

forever(Fun) ->
    Fun(),
    forever(Fun).

tp_call(Tp, Fun) ->
    R = make_ref(),
    Tp ! {call, self(), R, Fun},
    receive
        {R, Res} ->
            Res
    end.

tp_cast(Tp, Fun) ->
    Tp ! {cast, Fun}.

test_proc() ->
    receive
        {call, From, Ref, Fun} ->
            From ! {Ref, Fun()};
        {cast, Fun} ->
            Fun()
    end,
    test_proc().

expand_link_list([#erl_link{type = ?LINK_NODE, targets = N} = Rec | T]) ->
    lists:duplicate(N,Rec#erl_link{targets = []}) ++ expand_link_list(T);
expand_link_list([#erl_link{targets = [#erl_link{pid = Pid}]} = Rec | T]) ->
    [Rec#erl_link{targets = [Pid]} | expand_link_list(T)];
expand_link_list([#erl_link{targets = [#erl_link{pid = Pid}|TT]} = Rec | T]) ->
    [ Rec#erl_link{targets = [Pid]} | expand_link_list( 
                                        [Rec#erl_link{targets = TT} | T])]; 
expand_link_list([#erl_link{targets = []} = Rec | T]) ->
    [Rec | expand_link_list(T)];
expand_link_list([]) ->
    [].

get_local_link_list(Obj) ->
    case catch erts_debug:get_internal_state({link_list, Obj}) of
        LL when is_list(LL) ->
            expand_link_list(LL);
        _ ->
            []
    end.

get_remote_link_list(Node, Obj) ->
    case catch rpc:call(Node, erts_debug, get_internal_state,
                        [{link_list, Obj}]) of
        LL when is_list(LL) ->
            expand_link_list(LL);
        _ ->
            []
    end.


get_link_list({Node, DistEntry}) when Node == node(), is_atom(DistEntry) ->
    get_local_link_list(DistEntry);
get_link_list({Node, DistEntry}) when is_atom(Node), is_atom(DistEntry) ->
    get_remote_link_list(Node, DistEntry);
get_link_list(P) when is_pid(P); is_port(P) ->
    case node(P) of
        Node when Node == node() ->
            get_local_link_list(P);
        Node ->
            get_remote_link_list(Node, P)
    end;
get_link_list(undefined) ->
    [].

get_local_monitor_list(Obj) ->
    case catch erts_debug:get_internal_state({monitor_list, Obj}) of
        LL when is_list(LL) ->
            LL;
        _ ->
            []
    end.

get_remote_monitor_list(Node, Obj) ->
    case catch rpc:call(Node, erts_debug, get_internal_state,
                        [{monitor_list, Obj}]) of
        LL when is_list(LL) ->
            LL;
        _ ->
            []
    end.


get_monitor_list({Node, DistEntry}) when Node == node(), is_atom(DistEntry) ->
    get_local_monitor_list(DistEntry);
get_monitor_list({Node, DistEntry}) when is_atom(Node), is_atom(DistEntry) ->
    get_remote_monitor_list(Node, DistEntry);
get_monitor_list(P) when is_pid(P) ->
    case node(P) of
        Node when Node == node() ->
            get_local_monitor_list(P);
        Node ->
            get_remote_monitor_list(Node, P)
    end;
get_monitor_list(undefined) ->
    [].


find_erl_monitor(Pid, Ref) when is_reference(Ref) ->
    lists:foldl(fun (#erl_monitor{ref = R} = EL, Acc) when R == Ref ->
                        [EL|Acc];
                    (_, Acc) ->
                        Acc
                end,
                [],
                get_monitor_list(Pid)).

% find_erl_link(Obj, Ref) when is_reference(Ref) -> 
%     lists:foldl(fun (#erl_link{ref = R} = EL, Acc) when R == Ref ->
% 			      [EL|Acc];
% 			  (_, Acc) ->
% 			      Acc
% 		      end,
% 		      [],
% 		      get_link_list(Obj)).

find_erl_link(Obj, Type, [Item, Data]) when is_pid(Item);
                                            is_port(Item);
                                            is_atom(Item) -> 
    lists:foldl(fun (#erl_link{type = T, pid = I, targets = D} = EL,
                     Acc) when T == Type, I == Item ->
                        case Data of
                            D ->
                                [EL|Acc];
                            [] ->
                                [EL|Acc];
                            _ ->
                                Acc
                        end;
                    (_, Acc) ->
                        Acc
                end,
                [],
                get_link_list(Obj));
find_erl_link(Obj, Type, Item) when is_pid(Item); is_port(Item); is_atom(Item) ->
    find_erl_link(Obj, Type, [Item, []]). 



check_link(A, B) ->
    [#erl_link{type = ?LINK_PID,
               pid = B,
               targets = []}] = find_erl_link(A, ?LINK_PID, B),
    [#erl_link{type = ?LINK_PID,
               pid = A,
               targets = []}] = find_erl_link(B, ?LINK_PID, A),
    case node(A) == node(B) of
        false ->
            [#erl_link{type = ?LINK_PID,
                       pid = A,
                       targets = [B]}] = find_erl_link({node(A),
                                                        node(B)},
                                                       ?LINK_PID,
                                                       [A, [B]]),
            [#erl_link{type = ?LINK_PID,
                       pid = B,
                       targets = [A]}] = find_erl_link({node(B),
                                                        node(A)},
                                                       ?LINK_PID,
                                                       [B, [A]]);
        true ->
            [] = find_erl_link({node(A), node(B)},
                               ?LINK_PID,
                               [A, [B]]),
            [] = find_erl_link({node(B), node(A)},
                               ?LINK_PID,
                               [B, [A]])
    end,
    ok.

check_unlink(A, B) ->
    [] = find_erl_link(A, ?LINK_PID, B),
    [] = find_erl_link(B, ?LINK_PID, A),
    [] = find_erl_link({node(A), node(B)}, ?LINK_PID, [A, [B]]),
    [] = find_erl_link({node(B), node(A)}, ?LINK_PID, [B, [A]]),
    ok.

check_process_monitor(From, {Name, Node}, Ref) when is_pid(From),
                                                    is_atom(Name),
                                                    Node == node(From),
                                                    is_reference(Ref) ->
    check_process_monitor(From, Name, Ref);
check_process_monitor(From, {Name, Node}, Ref) when is_pid(From),
                                                    is_atom(Name),
                                                    is_atom(Node),
                                                    is_reference(Ref) ->
    MonitoredPid = rpc:call(Node, erlang, whereis, [Name]),
    [#erl_monitor{type = ?MON_ORIGIN,
                  ref = Ref,
                  pid = Node,
                  name = Name}] = find_erl_monitor(From, Ref),
    [#erl_monitor{type = ?MON_TARGET,
                  ref = Ref,
                  pid = From,
                  name = Name}] = 	find_erl_monitor({node(From), Node}, Ref),
    [#erl_monitor{type = ?MON_ORIGIN,
                  ref = Ref,
                  pid = MonitoredPid,
                  name = Name}] = find_erl_monitor({Node, node(From)}, Ref),
    [#erl_monitor{type = ?MON_TARGET,
                  ref = Ref,
                  pid = From,
                  name = Name}] = find_erl_monitor(MonitoredPid, Ref),
    ok;
check_process_monitor(From, Name, Ref) when is_pid(From),
                                            is_atom(Name),
                                            undefined /= Name,
                                            is_reference(Ref) ->
    MonitoredPid = rpc:call(node(From), erlang, whereis, [Name]),

    [#erl_monitor{type = ?MON_ORIGIN,
                  ref = Ref,
                  pid = MonitoredPid,
                  name = Name}] = find_erl_monitor(From, Ref),


    [#erl_monitor{type = ?MON_TARGET,
                  ref = Ref,
                  pid = From,
                  name = Name}] = find_erl_monitor(MonitoredPid,Ref),
    ok;
check_process_monitor(From, To, Ref) when is_pid(From),
                                          is_pid(To),
                                          is_reference(Ref) ->
    OriMon = [#erl_monitor{type = ?MON_ORIGIN,
                           ref = Ref,
                           pid = To}],

    OriMon = find_erl_monitor(From, Ref), 

    TargMon = [#erl_monitor{type = ?MON_TARGET,
                            ref = Ref,
                            pid = From}],
    TargMon = find_erl_monitor(To, Ref),


    case node(From) == node(To) of
        false ->
            TargMon = find_erl_monitor({node(From), node(To)}, Ref),
            OriMon = find_erl_monitor({node(To), node(From)}, Ref);
        true ->
            [] = find_erl_monitor({node(From), node(From)}, Ref)
    end,
    ok.


check_process_demonitor(From, {undefined, Node}, Ref) when is_pid(From),
                                                           is_reference(Ref) ->
    [] = find_erl_monitor(From, Ref),
    case node(From) == Node of
        false ->
            [] = find_erl_monitor({node(From), Node}, Ref),
            [] = find_erl_monitor({Node, node(From)}, Ref);
        true ->
            [] = find_erl_monitor({Node, Node}, Ref)
    end,
    ok;
check_process_demonitor(From, {Name, Node}, Ref) when is_pid(From),
                                                      is_atom(Name),
                                                      Node == node(From),
                                                      is_reference(Ref) ->
    MonitoredPid = rpc:call(Node, erlang, whereis, [Name]),
    case rpc:call(Node, erlang, whereis, [Name]) of
        undefined ->
            check_process_demonitor(From, {undefined, Node}, Ref);
        MonitoredPid ->
            check_process_demonitor(From, MonitoredPid, Ref)
    end;
check_process_demonitor(From, {Name, Node}, Ref) when is_pid(From),
                                                      is_atom(Name),
                                                      is_atom(Node),
                                                      is_reference(Ref) ->
    MonitoredPid = rpc:call(Node, erlang, whereis, [Name]),
    [] = find_erl_monitor(From, Ref),
    [] = find_erl_monitor({node(From), Node}, Ref),
    [] = find_erl_monitor({Node, node(From)}, Ref),
    [] = find_erl_monitor(MonitoredPid, Ref),
    ok;
check_process_demonitor(From, undefined, Ref) when is_pid(From),
                                                   is_reference(Ref) ->
    [] = find_erl_monitor(From, Ref),
    case node(From) == node() of
        false ->
            [] = find_erl_monitor({node(From), node()}, Ref),
            [] = find_erl_monitor({node(), node(From)}, Ref);
        true ->
            [] = find_erl_monitor({node(), node()}, Ref)
    end,
    ok;
check_process_demonitor(From, Name, Ref) when is_pid(From),
                                              is_atom(Name),
                                              undefined /= Name,
                                              is_reference(Ref) ->
    check_process_demonitor(From, {Name, node()}, Ref);
check_process_demonitor(From, To, Ref) when is_pid(From),
                                            is_pid(To),
                                            is_reference(Ref) ->
    [] = find_erl_monitor(From, Ref),
    [] = find_erl_monitor(To, Ref),
    case node(From) == node(To) of
        false ->
            [] = find_erl_monitor({node(From), node(To)}, Ref),
            [] = find_erl_monitor({node(To), node(From)}, Ref);
        true ->
            [] = find_erl_monitor({node(From), node(From)}, Ref)
    end,
    ok.

no_of_monitor_node(From, Node) when is_pid(From), is_atom(Node) ->
    length(find_erl_link(From, ?LINK_NODE, Node)).

check_monitor_node(From, Node, No) when is_pid(From),
                                        is_atom(Node),
                                        is_integer(No),
                                        No >= 0 ->
    LL = lists:duplicate(No, #erl_link{type = ?LINK_NODE, pid = Node}),
    DLL = lists:duplicate(No, #erl_link{type = ?LINK_NODE, pid = From}),
    LL = find_erl_link(From, ?LINK_NODE, Node),
    DLL = find_erl_link({node(From), Node}, ?LINK_NODE, From),
    ok.



hostname() ->
    from($@, atom_to_list(node())).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].

get_names(N, T) when is_atom(T) ->
    get_names(N, T, []).
get_names(0, _, Acc) ->
    Acc;
get_names(N, T, Acc) ->
    get_names(N-1, T, [list_to_atom(atom_to_list(?MODULE)
                                    ++ "-"
                                    ++ atom_to_list(T)
                                    ++ "-"
                                    ++ integer_to_list(erlang:system_time(seconds))
                                    ++ "-"
                                    ++ integer_to_list(erlang:unique_integer([positive]))) | Acc]).

start_node(Name) ->
    start_node(Name, "").

start_node(Name, Args) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Res = test_server:start_node(Name, slave, [{args,  Args ++ " -pa " ++ Pa}]),
    {ok, Node} = Res,
    rpc:call(Node, erts_debug, set_internal_state,
             [available_internal_state, true]),
    Res.


stop_node(Node) ->
    test_server:stop_node(Node).

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

dmsg_ext(Term) ->	
    <<131, Res/binary>> = term_to_binary(Term),
    Res.

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
