%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2017. All Rights Reserved.
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

-module(timer_bif_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2,end_per_testcase/2]).
-export([start_timer_1/1, send_after_1/1, send_after_2/1, send_after_3/1,
	 cancel_timer_1/1,
	 start_timer_big/1, send_after_big/1,
	 start_timer_e/1, send_after_e/1, cancel_timer_e/1,
	 read_timer_trivial/1, read_timer/1, read_timer_async/1,
	 cleanup/1, evil_timers/1, registered_process/1, same_time_yielding/1,
	 same_time_yielding_with_cancel/1, same_time_yielding_with_cancel_other/1,
%	 same_time_yielding_with_cancel_other_accessor/1,
	 auto_cancel_yielding/1]).

-include_lib("common_test/include/ct.hrl").

-define(SHORT_TIMEOUT, 5000). %% Bif timers as short as this may be pre-allocated
-define(TIMEOUT_YIELD_LIMIT, 100).
-define(AUTO_CANCEL_YIELD_LIMIT, 100).

init_per_testcase(_Case, Config) ->
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> ok;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end,
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 5}}].

all() -> 
    [start_timer_1, send_after_1, send_after_2,
     cancel_timer_1, start_timer_e, send_after_e,
     cancel_timer_e, start_timer_big, send_after_big,
     read_timer_trivial, read_timer, read_timer_async,
     cleanup, evil_timers, registered_process,
     same_time_yielding, same_time_yielding_with_cancel,
     same_time_yielding_with_cancel_other,
%     same_time_yielding_with_cancel_other_accessor,
     auto_cancel_yielding].


%% Basic start_timer/3 functionality
start_timer_1(Config) when is_list(Config) ->
    Ref1 = erlang:start_timer(1000, self(), plopp),
    ok   = get(1400, {timeout, Ref1, plopp}),

    false = erlang:read_timer(Ref1),
    false = erlang:cancel_timer(Ref1),
    false = erlang:read_timer(Ref1),

    Ref2  = erlang:start_timer(1000, self(), plapp),
    Left2 = erlang:cancel_timer(Ref2),
    UpperLimit = 1000,
    true = (Left2 > 600) and (Left2 =< UpperLimit),
    empty = get_msg(),
    false = erlang:cancel_timer(Ref2),

    Ref3 = erlang:start_timer(1000, self(), plopp),
    no_message = get(600, {timeout, Ref3, plopp}),
    ok.

%% Basic send_after/3 functionality
send_after_1(Config) when is_list(Config) ->
    Ref3 = erlang:send_after(1000, self(), plipp),
    ok = get(1500, plipp),
    false = erlang:read_timer(Ref3),
    ok.

%% Big timeouts for start_timer/3
start_timer_big(Config) when is_list(Config) ->
    Big = 1 bsl 31,
    R = erlang:start_timer(Big, self(), hej),
    timer:sleep(200),
    Left = erlang:cancel_timer(R),
    case Big - Left of
        Diff when Diff >= 200, Diff < 10000 ->
            ok;
        _Diff ->
            ct:fail({big, Big, Left})
    end,
    ok.

%% Big timeouts for send_after/3
send_after_big(Config) when is_list(Config) ->
    Big = 1 bsl 31,
    R = erlang:send_after(Big, self(), hej),
    timer:sleep(200),
    Left = erlang:cancel_timer(R),
    case Big - Left of
        Diff when Diff >= 200, Diff < 10000 ->
            ok;
        _Diff ->
            ct:fail({big, Big, Left})
    end,
    ok.

%% send_after/3: messages in the right order, kind version
send_after_2(Config) when is_list(Config) ->
    _ = erlang:send_after(5000, self(), last),
    _ = erlang:send_after(0, self(), a0),
    _ = erlang:send_after(200, self(), a2),
    _ = erlang:send_after(100, self(), a1),
    _ = erlang:send_after(500, self(), a5),
    _ = erlang:send_after(300, self(), a3),
    _ = erlang:send_after(400, self(), a4),
    [a0,a1,a2,a3,a4,a5,last] = collect(last),
    ok.

%% send_after/3: messages in the right order, worse than send_after_2
send_after_3(Config) when is_list(Config) ->
    _ = erlang:send_after(100, self(), b1),
    _ = erlang:send_after(101, self(), b2),
    _ = erlang:send_after(102, self(), b3),
    _ = erlang:send_after(103, self(), last),
    [b1, b2, b3, last] = collect(last),

    % This behaviour is not guaranteed:
    %    _ = erlang:send_after(100, self(), c1),
    %    _ = erlang:send_after(100, self(), c2),
    %    _ = erlang:send_after(100, self(), c3),
    %    _ = erlang:send_after(100, self(), last),
    %    [c1, c2, c3, last] = collect(last),

    ok.

%% Check trivial cancel_timer/1 behaviour
cancel_timer_1(Config) when is_list(Config) ->
    false = erlang:cancel_timer(make_ref()),

    ok.

%% Error cases for start_timer/3
start_timer_e(Config) when is_list(Config) ->
    {'EXIT', _} = (catch erlang:start_timer(-4, self(), hej)),
    {'EXIT', _} = (catch erlang:start_timer(1 bsl 64,
                                            self(), hej)),

    {'EXIT', _} = (catch erlang:start_timer(4.5, self(), hej)),
    {'EXIT', _} = (catch erlang:start_timer(a, self(), hej)),

    Node = start_slave(),
    Pid = spawn(Node, timer, sleep, [10000]),
    {'EXIT', _} = (catch erlang:start_timer(1000, Pid, hej)),
    stop_slave(Node),


    ok.

%% Error cases for send_after/3
send_after_e(Config) when is_list(Config) ->
    {'EXIT', _} = (catch erlang:send_after(-4, self(), hej)),
    {'EXIT', _} = (catch erlang:send_after(1 bsl 64,
                                           self(), hej)),

    {'EXIT', _} = (catch erlang:send_after(4.5, self(), hej)),
    {'EXIT', _} = (catch erlang:send_after(a, self(), hej)),

    Node = start_slave(),
    Pid = spawn(Node, timer, sleep, [10000]),
    {'EXIT', _} = (catch erlang:send_after(1000, Pid, hej)),
    stop_slave(Node),
    ok.

%% Error cases for cancel_timer/1
cancel_timer_e(Config) when is_list(Config) ->
    {'EXIT', _} = (catch erlang:cancel_timer(1)),
    {'EXIT', _} = (catch erlang:cancel_timer(self())),
    {'EXIT', _} = (catch erlang:cancel_timer(a)),
    ok.

%% Trivial and error test cases for read_timer/1.
read_timer_trivial(Config) when is_list(Config) ->
    false = erlang:read_timer(make_ref()),
    {'EXIT', _} = (catch erlang:read_timer(42)),
    {'EXIT', _} = (catch erlang:read_timer(423497834744444444457667444444)),
    {'EXIT', _} = (catch erlang:read_timer(self())),
    {'EXIT', _} = (catch erlang:read_timer(ab)),
    ok.

%% Test that read_timer/1 seems to return the correct values.
read_timer(Config) when is_list(Config) ->
    process_flag(scheduler, 1),
    Big = 1 bsl 31,
    R = erlang:send_after(Big, self(), hej_hopp),

    receive after 200 -> ok end,		% Delay and clear reductions.
    Left = erlang:read_timer(R),
    Left2 = erlang:cancel_timer(R),
    case Left == Left2 of
        true -> ok;
        false -> Left = Left2 + 1
    end,
    false = erlang:read_timer(R),

    case Big - Left of
        Diff when Diff >= 200, Diff < 10000 ->
            ok;
        _Diff ->
            ct:fail({big, Big, Left})
    end,
    process_flag(scheduler, 0),
    ok.

%% Test that read_timer/1 seems to return the correct values.
read_timer_async(Config) when is_list(Config) ->
    process_flag(scheduler, 1),
    Big = 1 bsl 33,
    R = erlang:send_after(Big, self(), hej_hopp),

    %% Access from another scheduler
    process_flag(scheduler, erlang:system_info(schedulers_online)),

    receive after 200 -> ok end,		% Delay and clear reductions.
    ok = erlang:read_timer(R, [{async, true}]),
    ok = erlang:cancel_timer(R, [{async, true}, {info, true}]),
    ok = erlang:read_timer(R, [{async, true}]),

    {read_timer, R, Left} = receive_one(),
    {cancel_timer, R, Left2} = receive_one(),
    case Left == Left2 of
        true -> ok;
        false -> Left = Left2 + 1
    end,
    {read_timer, R, false} = receive_one(),

    case Big - Left of
        Diff when Diff >= 200, Diff < 10000 ->
            ok;
        _Diff ->
            ct:fail({big, Big, Left})
    end,
    process_flag(scheduler, 0),
    ok.

cleanup(Config) when is_list(Config) ->
    Mem = mem(),
    %% Timer on dead process
    P1 = spawn(fun () -> ok end),
    wait_until(fun () -> process_is_cleaned_up(P1) end),
    T1 = erlang:start_timer(?SHORT_TIMEOUT*2, P1, "hej"),
    T2 = erlang:send_after(?SHORT_TIMEOUT*2, P1, "hej"),
    receive after 1000 -> ok end,
    Mem = mem(),
    false = erlang:read_timer(T1),
    false = erlang:read_timer(T2),
    Mem = mem(),
    %% Process dies before timeout
    P2 = spawn(fun () -> receive after (?SHORT_TIMEOUT div 10) -> ok end end),
    T3 = erlang:start_timer(?SHORT_TIMEOUT*2, P2, "hej"),
    T4 = erlang:send_after(?SHORT_TIMEOUT*2, P2, "hej"),
    true = mem_larger_than(Mem),
    true = is_integer(erlang:read_timer(T3)),
    true = is_integer(erlang:read_timer(T4)),
    wait_until(fun () -> process_is_cleaned_up(P2) end),
    receive after 1000 -> ok end,
    false = erlang:read_timer(T3),
    false = erlang:read_timer(T4),
    Mem = mem(),
    %% Cancel timer
    P3 = spawn(fun () -> receive after ?SHORT_TIMEOUT*4 -> ok end end),
    T5 = erlang:start_timer(?SHORT_TIMEOUT*2, P3, "hej"),
    T6 = erlang:send_after(?SHORT_TIMEOUT*2, P3, "hej"),
    true = mem_larger_than(Mem),
    true = is_integer(erlang:cancel_timer(T5)),
    true = is_integer(erlang:cancel_timer(T6)),
    false = erlang:read_timer(T5),
    false = erlang:read_timer(T6),
    exit(P3, kill),
    wait_until(fun () -> process_is_cleaned_up(P3) end),
    Mem = mem(),
    %% Timeout
    Ref = make_ref(),
    T7 = erlang:start_timer(?SHORT_TIMEOUT+1, self(), Ref),
    T8 = erlang:send_after(?SHORT_TIMEOUT+1, self(), Ref),
    true = mem_larger_than(Mem),
    true = is_integer(erlang:read_timer(T7)),
    true = is_integer(erlang:read_timer(T8)),
    receive {timeout, T7, Ref} -> ok end,
    receive Ref -> ok end,
    Mem = mem(),
    ok.


evil_timers(Config) when is_list(Config) ->
    %% Create a composite term consisting of at least:
    %% * externals (remote pids, ports, and refs)
    %% * large (off heap) binaries
    %% * small (heap) binaries
    %% * funs
    %% * bignums
    %% * tuples
    %% * lists
    %% since data of these types have to be adjusted if moved
    %% in memory
    Self = self(),
    R1 = make_ref(),
    Node = start_slave(),
    spawn_link(Node,
               fun () ->
                       Self ! {R1,
                               [lists:sublist(erlang:ports(), 3),
                                [make_ref(), make_ref(), make_ref()],
                                lists:sublist(processes(), 3),
                                [fun () -> gurka end,
                                 fun (A) -> A + 1 end,
                                 fun (A, B) -> A + B end]]}
               end),
    ExtList = receive {R1, L} -> L end,
    stop_slave(Node),
    BinList = [<<"bla">>,
               <<"blipp">>,
               <<"blupp">>,
               list_to_binary(lists:duplicate(1000000,$a)),
               list_to_binary(lists:duplicate(1000000,$b)),
               list_to_binary(lists:duplicate(1000000,$c))],
    FunList = [fun () -> gurka end,
               fun (A) -> A + 1 end,
               fun (A, B) -> A + B end],
    PidList = lists:sublist(processes(), 3),
    PortList = lists:sublist(erlang:ports(), 3),
    RefList = [make_ref(), make_ref(), make_ref()],
    BigList = [111111111111, 22222222222222, 333333333333333333],
    Msg = {BinList,[FunList,{RefList,ExtList,PidList,PortList,BigList}]},
    %% io:format("Msg=~p~n",[Msg]),

    Prio = process_flag(priority, max),
    %%
    %% In the smp case there are four major cases we want to test:
    %%
    %% 1. A timer started with erlang:start_timer(Time, Receiver, Msg),
    %%    where Msg is a composite term, expires, and the receivers main
    %%    lock *can not* be acquired immediately (typically when the
    %%    receiver *is* running).
    %%
    %%    The wrap tuple ({timeout, TRef, Msg}) will in this case
    %%    be allocated in the previously allocated message buffer along
    %%    with Msg, i.e. the previously allocated message buffer will be
    %%    reallocated and potentially moved.
    TimeOutMsgs0 = evil_setup_timers(200, Self, Msg),
    RecvTimeOutMsgs0 = evil_recv_timeouts(200),
    %% 2. A timer started with erlang:start_timer(Time, Receiver, Msg),
    %%    where Msg is an immediate term, expires, and the receivers main
    %%    lock *can not* be acquired immediately (typically when the
    %%    receiver *is* running).
    %%
    %%    The wrap tuple will in this case be allocated in a new
    %%    message buffer.
    TimeOutMsgs1 = evil_setup_timers(200, Self, immediate),
    RecvTimeOutMsgs1 = evil_recv_timeouts(200),
    %% 3. A timer started with erlang:start_timer(Time, Receiver, Msg),
    %%    where Msg is a composite term, expires, and the receivers main
    %%    lock *can* be acquired immediately (typically when the receiver
    %%    *is not* running).
    %%
    %%    The wrap tuple will in this case be allocated on the receivers
    %%    heap, and Msg is passed in the previously allocated message
    %%    buffer.
    R2 = make_ref(),
    spawn_link(fun () ->
                       Self ! {R2, evil_setup_timers(200, Self, Msg)}
               end),
    receive after 1000 -> ok end,
    TimeOutMsgs2 = receive {R2, TOM2} -> TOM2 end,
    RecvTimeOutMsgs2 = evil_recv_timeouts(200),
    %% 4. A timer started with erlang:start_timer(Time, Receiver, Msg),
    %%    where Msg is an immediate term, expires, and the Receivers main
    %%    lock *can* be acquired immediately (typically when the receiver
    %%    *is not* running).
    %%
    %%    The wrap tuple will in this case be allocated on the receivers
    %%    heap.
    R3 = make_ref(),
    spawn_link(fun () ->
                       Self ! {R3, evil_setup_timers(200,Self,immediate)}
               end),
    receive after 1000 -> ok end,
    TimeOutMsgs3 = receive {R3, TOM3} -> TOM3 end,
    RecvTimeOutMsgs3 = evil_recv_timeouts(200),

    %% Garge collection will hopefully crash the emulator if something
    %% is wrong...
    garbage_collect(),
    garbage_collect(),
    garbage_collect(),

    %% Make sure we got the timeouts we expected
    %%
    %% Note timeouts are *not* guaranteed to be delivered in order
    ok = match(lists:sort(RecvTimeOutMsgs0), lists:sort(TimeOutMsgs0)),
    ok = match(lists:sort(RecvTimeOutMsgs1), lists:sort(TimeOutMsgs1)),
    ok = match(lists:sort(RecvTimeOutMsgs2), lists:sort(TimeOutMsgs2)),
    ok = match(lists:sort(RecvTimeOutMsgs3), lists:sort(TimeOutMsgs3)),

    process_flag(priority, Prio),
    ok.

evil_setup_timers(N, Receiver, Msg) ->
    evil_setup_timers(0, N, Receiver, Msg, []).

evil_setup_timers(N, N, _Receiver, _Msg, TOs) ->
    TOs;
evil_setup_timers(N, Max, Receiver, Msg, TOs) ->
    TRef = erlang:start_timer(N, Receiver, Msg),
    evil_setup_timers(N+1, Max, Receiver, Msg, [{timeout,TRef,Msg}|TOs]).


evil_recv_timeouts(M) ->
    evil_recv_timeouts([], 0, M).

evil_recv_timeouts(TOs, N, N) ->
    TOs;
evil_recv_timeouts(TOs, N, M) ->
    receive
        {timeout, _, _} = TO ->
            evil_recv_timeouts([TO|TOs], N+1, M)
    after 0 ->
              evil_recv_timeouts(TOs, N, M)
    end.

registered_process(Config) when is_list(Config) ->
    Mem = mem(),
    %% Cancel
    T1 = erlang:start_timer(?SHORT_TIMEOUT+1, ?MODULE, "hej"),
    T2 = erlang:send_after(?SHORT_TIMEOUT+1, ?MODULE, "hej"),
    undefined = whereis(?MODULE),
    true = mem_larger_than(Mem),
    true = is_integer(erlang:cancel_timer(T1)),
    true = is_integer(erlang:cancel_timer(T2)),
    false = erlang:read_timer(T1),
    false = erlang:read_timer(T2),
    Mem = mem(),
    %% Timeout register after start
    Ref1 = make_ref(),
    T3 = erlang:start_timer(?SHORT_TIMEOUT+1, ?MODULE, Ref1),
    T4 = erlang:send_after(?SHORT_TIMEOUT+1, ?MODULE, Ref1),
    undefined = whereis(?MODULE),
    true = mem_larger_than(Mem),
    true = is_integer(erlang:read_timer(T3)),
    true = is_integer(erlang:read_timer(T4)),
    true = register(?MODULE, self()),
    receive {timeout, T3, Ref1} -> ok end,
    receive Ref1 -> ok end,
    Mem = mem(),
    %% Timeout register before start
    Ref2 = make_ref(),
    T5 = erlang:start_timer(?SHORT_TIMEOUT+1, ?MODULE, Ref2),
    T6 = erlang:send_after(?SHORT_TIMEOUT+1, ?MODULE, Ref2),
    true = mem_larger_than(Mem),
    true = is_integer(erlang:read_timer(T5)),
    true = is_integer(erlang:read_timer(T6)),
    receive {timeout, T5, Ref2} -> ok end,
    receive Ref2 -> ok end,
    Mem = mem(),
    true = unregister(?MODULE),
    ok.

same_time_yielding(Config) when is_list(Config) ->
    Mem = mem(),
    Ref = make_ref(),
    SchdlrsOnln = erlang:system_info(schedulers_online),
    Tmo = erlang:monotonic_time(millisecond) + 3000,
    Tmrs = lists:map(fun (I) ->
                             process_flag(scheduler, (I rem SchdlrsOnln) + 1),
                             erlang:start_timer(Tmo, self(), Ref, [{abs, true}])
                     end,
                     lists:seq(1, (?TIMEOUT_YIELD_LIMIT*3+1)*SchdlrsOnln)),
    true = mem_larger_than(Mem),
    receive_all_timeouts(length(Tmrs), Ref),
    Done = erlang:monotonic_time(millisecond),
    true = Done >= Tmo,
    MsAfterTmo = Done - Tmo,
    io:format("Done ~p ms after Tmo\n", [MsAfterTmo]),
    case erlang:system_info(build_type) of
        opt ->
            true = MsAfterTmo < 200;
        _ ->
            true = MsAfterTmo < 1000
    end,
    Mem = mem(),
    ok.

%% Read out all timeouts in receive queue order. This is efficient
%% even if there are very many messages.

receive_all_timeouts(0, _Ref) ->
    ok;
receive_all_timeouts(N, Ref) ->
    receive
        {timeout, _Tmr, Ref} ->
            receive_all_timeouts(N-1, Ref)
    end.

same_time_yielding_with_cancel(Config) when is_list(Config) ->
    same_time_yielding_with_cancel_test(false, false).

same_time_yielding_with_cancel_other(Config) when is_list(Config) ->
    same_time_yielding_with_cancel_test(true, false).

%same_time_yielding_with_cancel_other_accessor(Config) when is_list(Config) ->
%    same_time_yielding_with_cancel_test(true, true).

do_cancel_tmrs(Tmo, Tmrs, Tester) ->
    BeginCancel = erlang:convert_time_unit(Tmo,
                                           millisecond,
                                           microsecond) - 100,
    busy_wait_until(fun () ->
                            erlang:monotonic_time(microsecond) >= BeginCancel
                    end),
    lists:foreach(fun (Tmr) ->
                          erlang:cancel_timer(Tmr,
                                              [{async, true},
                                               {info, true}])
                  end, Tmrs),
    case Tester == self() of
        true -> ok;
        false -> forward_msgs(Tester)
    end.

same_time_yielding_with_cancel_test(Other, Accessor) ->
    Mem = mem(),
    SchdlrsOnln = erlang:system_info(schedulers_online),
    Tmo = erlang:monotonic_time(millisecond) + 3000,
    Tester = self(),
    Cancelor = case Other of
                   false ->
                       Tester;
                   true ->
                       spawn(fun () ->
                                     receive
                                         {timers, Tmrs} ->
                                             do_cancel_tmrs(Tmo, Tmrs, Tester)
                                     end
                             end)
               end,
    Opts = case Accessor of
               false -> [{abs, true}];
               true -> [{accessor, Cancelor}, {abs, true}]
           end,
    Tmrs = lists:map(fun (I) ->
                             process_flag(scheduler, (I rem SchdlrsOnln) + 1),
                             erlang:start_timer(Tmo, self(), hej, Opts)
                     end,
                     lists:seq(1, (?TIMEOUT_YIELD_LIMIT*3+1)*SchdlrsOnln)),
    true = mem_larger_than(Mem),
    case Other of
        false ->
            do_cancel_tmrs(Tmo, Tmrs, Tester);
        true ->
            Cancelor ! {timers, Tmrs}
    end,
    {Tmos, Cncls} = lists:foldl(fun (Tmr, {T, C}) ->
                                        receive
                                            {timeout, Tmr, hej} ->
                                                receive
                                                    {cancel_timer, Tmr, Info} ->
                                                        false = Info,
                                                        {T+1, C}
                                                end;
                                            {cancel_timer, Tmr, false} ->
                                                receive
                                                    {timeout, Tmr, hej} ->
                                                        {T+1, C}
                                                end;
                                            {cancel_timer, Tmr, TimeLeft} ->
                                                true = is_integer(TimeLeft),
                                                {T, C+1}
                                        end
                                end,
                                {0, 0},
                                Tmrs),
    io:format("Timeouts: ~p Cancels: ~p~n", [Tmos, Cncls]),
    Mem = mem(),
    case Other of
        true -> exit(Cancelor, bang);
        false -> ok
    end,
    {comment,
     "Timeouts: " ++ integer_to_list(Tmos) ++ " Cancels: "
     ++ integer_to_list(Cncls)}.

auto_cancel_yielding(Config) when is_list(Config) ->
    Mem = mem(),
    SchdlrsOnln = erlang:system_info(schedulers_online),
    P = spawn(fun () ->
                      lists:foreach(
                        fun (I) ->
                                process_flag(scheduler, (I rem SchdlrsOnln)+1),
                                erlang:start_timer((1 bsl 28)+I*10, self(), hej)
                        end,
                        lists:seq(1,
                                  ((?AUTO_CANCEL_YIELD_LIMIT*3+1)
                                   *SchdlrsOnln))),
                      receive after infinity -> ok end
              end),
    true = mem_larger_than(Mem),
    exit(P, bang),
    wait_until(fun () -> process_is_cleaned_up(P) end),
    Mem = mem(),
    ok.

process_is_cleaned_up(P) when is_pid(P) ->
    undefined == erts_debug:get_internal_state({process_status, P}).

wait_until(Pred) when is_function(Pred) ->
    case catch Pred() of
        true -> ok;
        _ -> receive after 50 -> ok end, wait_until(Pred)
    end.

busy_wait_until(Pred) when is_function(Pred) ->
    case catch Pred() of
        true -> ok;
        _ -> busy_wait_until(Pred)
    end.

forward_msgs(To) ->
    receive
        Msg ->
            To ! Msg
    end,
    forward_msgs(To).

get(Time, Msg) ->
    receive 
        Msg ->
            ok
    after Time
          ->
              no_message
    end.

get_msg() ->
    receive
        Msg ->
            {ok, Msg}
    after 0 ->
              empty
    end.

start_slave() ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = atom_to_list(?MODULE)
    ++ "-" ++ integer_to_list(erlang:system_time(second))
    ++ "-" ++ integer_to_list(erlang:unique_integer([positive])),
    {ok, Node} = test_server:start_node(Name, slave, [{args, "-pa " ++ Pa}]),
    Node.

stop_slave(Node) ->
    test_server:stop_node(Node).

collect(Last) ->
    collect(Last, []).

receive_one() ->
    receive
        Msg ->
            Msg
    end.

collect(Last, Msgs0) ->
    Msg = receive_one(),
    Msgs = Msgs0 ++ [Msg],
    case Msg of
        Last ->
            Msgs;
        _ ->
            collect(Last, Msgs)
    end.

match(X, X) ->
    %erlang:display({match, X}),
    ok;
match(X, Y) ->
    %erlang:display({mismatch, X, Y}),
    match_aux(X, Y).

match_aux(X, X) ->
    unexpected_error;
match_aux(X, Y) when is_list(X), is_list(Y), length(X) =/= length(Y) ->
    %% erlang:display({mismatch, X, Y}),
    {list_length_mismatch, length(X), length(Y)};
match_aux([X|Xs], [X|Ys]) ->
    match_aux(Xs, Ys);
match_aux([X|_], [Y|_]) ->
    match_aux(X, Y);
match_aux(X, Y) when is_tuple(X), is_tuple(Y), size(X) =/= size(Y) ->
    %% erlang:display({mismatch, X, Y}),
    {tuple_size_mismatch, size(X), size(Y)};
match_aux(X, Y) when is_tuple(X), is_tuple(Y) ->
    match_aux(tuple_to_list(X), tuple_to_list(Y));
match_aux(X, Y) ->
    %% erlang:display({mismatch, X, Y}),
    {mismatch, type(X), type(Y)}.

type(X) when is_list(X) -> list;
type(X) when is_tuple(X) -> tuple;
type(X) when is_float(X) -> float;
type(X) when is_integer(X) -> integer;
type(X) when is_pid(X) -> {pid, node(X)};
type(X) when is_reference(X) -> {reference, node(X)};
type(X) when is_port(X) -> {port, node(X)};
type(X) when is_binary(X) -> binary;
type(X) when is_atom(X) -> atom;
type(_) -> unknown.


mem_larger_than(no_fix_alloc) ->
    true;
mem_larger_than(Mem) ->
    mem() > Mem.

mem() ->
    erts_debug:set_internal_state(wait, timer_cancellations),
    erts_debug:set_internal_state(wait, deallocations),
    case mem_get() of
        {-1, -1} -> no_fix_alloc;
        {A, U} -> io:format("mem = ~p ~p~n", [A, U]), U
    end.

mem_get() ->
    % Bif timer memory
    Ref = make_ref(),
    erlang:system_info({memory_internal, Ref, [fix_alloc]}),
    mem_recv(erlang:system_info(schedulers), Ref, {0, 0}).

mem_recv(0, _Ref, AU) ->
    AU;
mem_recv(N, Ref, AU) ->
    receive
        {Ref, _, IL} ->
            mem_recv(N-1, Ref, mem_parse_ilists(IL, AU))
    end.


mem_parse_ilists([], AU) ->
    AU;
mem_parse_ilists([I|Is], AU) ->
    mem_parse_ilists(Is, mem_parse_ilist(I, AU)).

mem_parse_ilist({fix_alloc, false}, _) ->
    {-1, -1};
mem_parse_ilist({fix_alloc, _, IDL}, {A, U}) ->
    case lists:keyfind(fix_types, 1, IDL) of
        {fix_types, TL} ->
            {ThisA, ThisU} = mem_get_btm_aus(TL, 0, 0),
            {ThisA + A, ThisU + U};
        {fix_types, Mask, TL} ->
            {ThisA, ThisU} = mem_get_btm_aus(TL, 0, 0),
            {(ThisA + A) band Mask , (ThisU + U) band Mask}
    end.

mem_get_btm_aus([], A, U) ->
    {A, U};
mem_get_btm_aus([{BtmType, BtmA, BtmU} | Types],
                A, U) when BtmType == bif_timer;
                           BtmType == accessor_bif_timer ->
    mem_get_btm_aus(Types, BtmA+A, BtmU+U);
mem_get_btm_aus([_|Types], A, U) ->
    mem_get_btm_aus(Types, A, U).
