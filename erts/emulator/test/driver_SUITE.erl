%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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

%%% Purpose : Test interaction Erlang/Drivers (new features as of R3A)

%%% Checks that new features (as of R3) of the Erlang/Driver
%%% implementation works as expected.
%%%
%%% Things that should be tested:
%%% - outputv
%%% - timeouts
%%% - queueing

-module(driver_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, 
         end_per_suite/1, init_per_group/2,end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         a_test/1,
         outputv_echo/1,
         timer_measure/1,
         timer_cancel/1,
         timer_change/1,
         timer_delay/1,
         queue_echo/1,
         outputv_errors/1,
         driver_unloaded/1,
         io_ready_exit/1,
         use_fallback_pollset/0,
         use_fallback_pollset/1,
         bad_fd_in_pollset/1,
         fd_change/1,
         steal_control/1,
         otp_6602/1,
         driver_system_info_base_ver/1,
         driver_system_info_prev_ver/1,
         driver_system_info_current_ver/1,
         driver_monitor/1,

         ioq_exit_ready_input/1,
         ioq_exit_ready_output/1,
         ioq_exit_timeout/1,
         ioq_exit_ready_async/1,
         ioq_exit_ready_input_async/1,
         ioq_exit_ready_output_async/1,
         ioq_exit_timeout_async/1,
         zero_extended_marker_garb_drv/1,
         invalid_extended_marker_drv/1,
         larger_major_vsn_drv/1,
         larger_minor_vsn_drv/1,
         smaller_major_vsn_drv/1,
         smaller_minor_vsn_drv/1,
         peek_non_existing_queue/1,
         otp_6879/1,
         caller/1,
         many_events/1,
         missing_callbacks/1,
         smp_select/1,
         driver_select_use/1,
         thread_mseg_alloc_cache_clean/1,
         otp_9302/1,
         thr_free_drv/1,
         async_blast/1,
         thr_msg_blast/1,
         consume_timeslice/1,
         env/1,
         z_test/1]).

-export([bin_prefix/2]).

-export([get_check_io_total/1]).   % for z_SUITE.erl

-include_lib("common_test/include/ct.hrl").


% First byte in communication with the timer driver
-define(START_TIMER, 0).
-define(CANCEL_TIMER, 1).
-define(DELAY_START_TIMER, 2).
-define(TIMER, 3).
-define(CANCELLED, 4).

% First byte in communication with queue driver
-define(PUSHQ, 0).
-define(ENQ, 1).
-define(PUSHQ_BIN, 2).
-define(ENQ_BIN, 3).
-define(PUSHQV, 4).
-define(ENQV, 5).

-define(DEQ, 6).
-define(BYTES_QUEUED, 7).
-define(READ_HEAD, 8).

-define(RANDOM, random).

% Max data size that is queued in one instance
-define(MAX_DATA_SIZE, 16384).

% This is the allowed delay when testing the driver timer functionality
-define(delay, 400).

-define(heap_binary_size, 64).

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    CIOD = rpc(Config,
               fun() ->
                       case catch erts_debug:get_internal_state(available_internal_state) of
                           true -> ok;
                           _ -> erts_debug:set_internal_state(available_internal_state, true)
                       end,
                       erts_debug:get_internal_state(check_io_debug)
               end),
    erlang:display({init_per_testcase, Case}),
    0 = element(1, CIOD),
    [{testcase, Case}|Config].

end_per_testcase(Case, Config) ->
    erlang:display({end_per_testcase, Case}),
    CIOD = rpc(Config,
               fun() ->
                       get_stable_check_io_info(),
                       erts_debug:get_internal_state(check_io_debug)
               end),
    0 = element(1, CIOD),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> %% Keep a_test first and z_test last...
    [a_test, outputv_errors, outputv_echo, queue_echo,
     {group, timer},
     driver_unloaded, io_ready_exit, otp_6602,
     {group, polling},
     {group, poll_thread},
     {group, poll_set},
     driver_system_info_base_ver,
     driver_system_info_prev_ver,
     driver_system_info_current_ver, driver_monitor,
     {group, ioq_exit}, zero_extended_marker_garb_drv,
     invalid_extended_marker_drv, larger_major_vsn_drv,
     larger_minor_vsn_drv, smaller_major_vsn_drv,
     smaller_minor_vsn_drv, peek_non_existing_queue,
     otp_6879, caller, many_events, missing_callbacks,
     thread_mseg_alloc_cache_clean,
     otp_9302,
     thr_free_drv,
     async_blast,
     thr_msg_blast,
     consume_timeslice,
     env,
     z_test].

groups() -> 
    [{timer, [],
      [timer_measure, timer_cancel, timer_delay,
       timer_change]},
     {poll_thread, [], [{group, polling}]},
     {poll_set, [], [{group, polling}]},
     {polling, [],
      [a_test, use_fallback_pollset,
       bad_fd_in_pollset, fd_change,
       steal_control, smp_select,
       driver_select_use, z_test]},
     {ioq_exit, [],
      [ioq_exit_ready_input, ioq_exit_ready_output,
       ioq_exit_timeout, ioq_exit_ready_async,
       ioq_exit_ready_input_async, ioq_exit_ready_output_async,
       ioq_exit_timeout_async]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false).

init_per_group(poll_thread, Config) ->
    [{node_args, "+IOt 2"} | Config];
init_per_group(poll_set, Config) ->
    [{node_args, "+IOt 2 +IOp 2"} | Config];
init_per_group(polling, Config) ->
    case proplists:get_value(node_args, Config) of
        undefined ->
            Config;
        Args ->
            {ok, Node} = start_node(polling, Args),
            [{node, Node} | Config]
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    case proplists:get_value(node, Config) of
        undefined ->
            ok;
        Node ->
            stop_node(Node)
    end,
    Config.

%% Test sending bad types to port with an outputv-capable driver.
outputv_errors(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, outputv_drv),

    outputv_bad_types(fun(T) ->
                              outputv_errors_1(T),
                              outputv_errors_1([1|T]),
                              L = [1,2,3],
                              outputv_errors_1([L,T]),
                              outputv_errors_1([L|T])
                      end),
    outputv_errors_1(42),

    %% Test iolists that do not fit in the address space.
    %% Unfortunately, it would be too slow to test in a 64-bit emulator.
    case erlang:system_info(wordsize) of
        4 -> outputv_huge_iolists();
        _ -> ok
    end.

outputv_bad_types(Test) ->
    Types = [-1,256,atom,42.0,{a,b,c},make_ref(),fun() -> 42 end,
             [1|2],<<1:1>>,<<1:9>>,<<1:15>>],
    _ = [Test(Type) || Type <- Types],
    ok.

outputv_huge_iolists() ->
    FourGigs = 1 bsl 32,
    Sizes = [FourGigs+N || N <- lists:seq(0, 64)] ++
    [1 bsl N || N <- lists:seq(33, 37)],
    Base = <<0:(1 bsl 20)/unit:8>>,
    [begin
         L = build_iolist(Sz, Base),
         outputv_errors_1(L)
     end || Sz <- Sizes],
    ok.

outputv_errors_1(Term) ->
    Port = open_port({spawn_driver,outputv_drv}, []),
    {'EXIT',{badarg,_}} = (catch port_command(Port, Term)),
    port_close(Port).

build_iolist(N, Base) when N < 16 ->
    case rand:uniform(3) of
        1 ->
            <<Bin:N/binary,_/binary>> = Base,
            Bin;
        _ ->
            lists:seq(1, N)
    end;
build_iolist(N, Base) when N =< byte_size(Base) ->
    case rand:uniform(3) of
        1 ->
            <<Bin:N/binary,_/binary>> = Base,
            Bin;
        2 ->
            <<Bin:N/binary,_/binary>> = Base,
            [Bin];
        3 ->
            case N rem 2 of
                0 ->
                    L = build_iolist(N div 2, Base),
                    [L,L];
                1 ->
                    L = build_iolist(N div 2, Base),
                    [L,L,45]
            end
    end;
build_iolist(N0, Base) ->
    Small = rand:uniform(15),
    Seq = lists:seq(1, Small),
    N = N0 - Small,
    case N rem 2 of
        0 ->
            L = build_iolist(N div 2, Base),
            [L,L|Seq];
        1 ->
            L = build_iolist(N div 2, Base),
            [47,L,L|Seq]
    end.

%% Test echoing data with a driver that supports outputv.
outputv_echo(Config) when is_list(Config) ->
    ct:timetrap({minutes, 10}),
    Name = 'outputv_drv',
    P = start_driver(Config, Name, true),

    ov_test(P, {bin,0}),
    ov_test(P, {bin,1}),
    ov_test(P, {bin,2}),
    ov_test(P, {bin,3}),
    ov_test(P, {bin,4}),
    ov_test(P, {bin,5}),
    ov_test(P, {bin,6}),
    ov_test(P, {bin,7}),
    ov_test(P, {bin,8}),
    ov_test(P, {bin,15}),
    ov_test(P, {bin,16}),
    ov_test(P, {bin,17}),

    ov_test(P, {list,0}),
    ov_test(P, {list,1}),
    ov_test(P, {list,2}),
    ov_test(P, [int,int,{list,0},int]),
    ov_test(P, [int,int,{list,1},int]),
    ov_test(P, [int,int,{list,2}]),
    ov_test(P, [{list,3},int,int,{list,2}]),
    ov_test(P, {list,33}),

    ov_test(P, [{bin,0}]),
    ov_test(P, [{bin,1}]),
    ov_test(P, [{bin,2}]),
    ov_test(P, [{bin,3}]),
    ov_test(P, [{bin,4}]),
    ov_test(P, [{bin,5}]),
    ov_test(P, [{bin,6},int]),
    ov_test(P, [int,{bin,3}]),
    ov_test(P, [int|{bin,4}]),
    ov_test(P, [{bin,17},int,{bin,13}|{bin,3}]),

    ov_test(P, [int,{bin,17},int,{bin,?heap_binary_size+1}|{bin,3}]),

    stop_driver(P, Name),
    ok.

ov_test(Port, Template) ->
    Self = self(),
    spawn_opt(erlang, apply, [fun () -> ov_test(Self, Port, Template) end,[]],
              [link,{fullsweep_after,0}]),
    receive
        done -> ok
    end.

ov_test(Parent, Port, Template) ->
    true = port_connect(Port, self()),

    HeapData = build_data(Template),
    io:format("Mostly heap binaries"),
    ov_send_and_test(Port, HeapData, HeapData),

    %% Try sub binaries.
    io:format("Mostly sub binaries of heap binaries"),
    SubHeapData = make_sub_binaries(HeapData),
    ov_send_and_test(Port, SubHeapData, HeapData),

    %% Try refc binaries.
    io:format("Refc binaries"),
    RefcData = make_refc_binaries(HeapData),
    ov_send_and_test(Port, RefcData, RefcData),

    %% Try sub binaries of heap binaries.
    io:format("Sub binaries of refc binaries"),
    SubRefcData = make_sub_binaries(RefcData),
    ov_send_and_test(Port, SubRefcData, RefcData),
    io:format("", []),

    %% Garbage collect and make sure that there are no binaries left.
    %% R7 note:
    %%  - dead variables on the stack are killed after last use,
    %%  - erlang:garbage_collect/0 collects garbage immediately.
    %%  (there used to be dummy functions here)
    erlang:garbage_collect(),
    {binary,[]} = process_info(self(), binary),

    %% Reassign Port back to parent and tell him we are done.
    true = port_connect(Port, Parent),
    Parent ! done.

ov_send_and_test(Port, Data, ExpectedResult) ->
    io:format("~p ! ~P", [Port,Data,12]),
    Port ! {self(),{command,Data}},
    receive 
        {Port,{data,ReturnData}} ->
            io:format("~p returned ~P", [Port,ReturnData,12]),
            compare(ReturnData, ExpectedResult);
        {Port,{data,OtherData}} ->
            ct:fail("~p returned WRONG data ~p", [Port,OtherData]);
        Wrong ->
            ct:fail({unexpected_port_or_data,Wrong})
    end.

compare(Got, Expected) ->
    case {list_to_binary([Got]),list_to_binary([Expected])} of
        {B,B} -> ok;
        {_Gb,_Eb} ->
            ct:fail(got_bad_data)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 		Driver timer test suites
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Check that timers time out in good time.
timer_measure(Config) when is_list(Config) ->
    Name = 'timer_drv',
    Port = start_driver(Config, Name, false),

    try_timeouts(Port, 8997),

    stop_driver(Port, Name),
    ok.

try_timeouts(_, 0) -> ok;
try_timeouts(Port, Timeout) ->
    TimeBefore = erlang:monotonic_time(),
    erlang:port_command(Port, <<?START_TIMER,Timeout:32>>),
    receive
        {Port,{data,[?TIMER]}} ->
            Elapsed = erl_millisecs() - erl_millisecs(TimeBefore),
            io:format("Elapsed: ~p Timeout: ~p\n", [Elapsed, Timeout]),
            if
                Elapsed < Timeout ->
                    ct:fail(too_short);
                Elapsed > Timeout + ?delay ->
                    ct:fail(too_long);
                true ->
                    try_timeouts(Port, Timeout div 2)
            end
    after Timeout + 100*?delay ->
              ct:fail("driver failed to timeout")
    end.

%% Try cancelling timers set in a driver.
timer_cancel(Config) when is_list(Config) ->
    Name = 'timer_drv',
    Port = start_driver(Config, Name, false),

    try_cancel(Port, 10000),

    stop_driver(Port, Name),
    ok.

try_cancel(Port, Timeout) ->
    T_before = erl_millisecs(),
    Port ! {self(),{command,<<?START_TIMER,(Timeout + ?delay):32>>}},
    receive
        {Port, {data, [?TIMER]}} ->
            ct:fail("driver timed out before cancelling it")
    after Timeout -> 
              Port ! {self(), {command, [?CANCEL_TIMER]}},
              receive 
                  {Port, {data, [?TIMER]}} ->
                      ct:fail("driver timed out after cancelling it");
                  {Port, {data, [?CANCELLED]}} ->
                      Time_milli_secs = erl_millisecs() - T_before,

                      io:format("Time_milli_secs: ~p Timeout: ~p\n",
                                [Time_milli_secs, Timeout]), 
                      if
                          Time_milli_secs > (Timeout + ?delay) ->
                              ct:fail("too long real time");
                          Timeout == 0 -> ok;
                          true -> try_cancel(Port, Timeout div 2)
                      end
              after 100*?delay ->
                        ct:fail("No message from driver")
              end
    end.

%% Test that timers don't time out too early if we do a sleep
%% before setting a timer.

timer_delay(Config) when is_list(Config) ->
    Name = 'timer_drv',
    Port = start_driver(Config, Name, false),

    TimeBefore = erlang:monotonic_time(),
    Timeout0 = 350,
    erlang:port_command(Port, <<?DELAY_START_TIMER,Timeout0:32>>),
    Timeout = Timeout0 + 1000,
    receive
        {Port,{data,[?TIMER]}} ->
            Elapsed = erl_millisecs() - erl_millisecs(TimeBefore),
            io:format("Elapsed time: ~p Timeout: ~p\n",
                      [Elapsed,Timeout]), 
            if
                Elapsed < Timeout ->
                    ct:fail(too_short);
                Elapsed > Timeout + ?delay ->
                    ct:fail(too_long);
                true ->
                    ok
            end
    end,

    stop_driver(Port, Name),
    ok.

%% Test that driver_set_timer with new timout really changes
%% the timer (ticket OTP-5942), it didn't work before

timer_change(Config) when is_list(Config) ->
    Name = 'timer_drv',
    Port = start_driver(Config, Name, false),

    try_change_timer(Port, 10000),

    stop_driver(Port, Name),
    ok.

try_change_timer(_Port, 0) -> ok;
try_change_timer(Port, Timeout) ->
    Timeout_3 = Timeout*3,
    TimeBefore = erlang:monotonic_time(),
    erlang:port_command(Port, <<?START_TIMER,Timeout_3:32>>),
    erlang:port_command(Port, <<?START_TIMER,Timeout:32>>),
    receive
        {Port,{data,[?TIMER]}} ->
            Elapsed = erl_millisecs() - erl_millisecs(TimeBefore),
            io:format("Elapsed: ~p Timeout: ~p\n", [Elapsed,Timeout]),
            if
                Elapsed < Timeout ->
                    ct:fail(too_short);
                Elapsed > Timeout + ?delay ->
                    ct:fail(too_long);
                true ->
                    try_timeouts(Port, Timeout div 2)
            end
    after Timeout + 100*?delay ->
              ct:fail("driver failed to timeout")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 		Queue test suites
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 1) Queue up data in a driver that uses the full driver_queue API to do this.
%% 2) Get the data back, a random amount at a time.
queue_echo(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) of
        true -> exit(crashes_native_code);
        false -> queue_echo_1(Config)
    end.

queue_echo_1(Config) ->
    ct:timetrap({minutes, 10}),
    Name = 'queue_drv',
    P = start_driver(Config, Name, true),

    q_echo(P, [{?ENQ, {list,1}},
               {?ENQ, {list,0}},
               {?ENQ, {bin,0}},
               {?ENQ, {bin,1}},
               {?ENQ, {bin,2}},
               {?ENQ, {bin,3}},
               {?ENQ, {bin,4}},
               {?ENQ, {bin,5}},
               {?ENQ, {bin,600}},
               {?PUSHQ, {list,0}},
               {?PUSHQ, {list,1}},
               {?PUSHQ, {bin,0}},
               {?PUSHQ, {bin,1}},
               {?PUSHQ, {bin,888}},
               {?ENQ_BIN, {bin,0}},
               {?ENQ_BIN, {bin,1}},
               {?ENQ_BIN, {bin,2}},
               {?ENQ_BIN, {bin,3}},
               {?ENQ_BIN, {bin,4}},
               {?ENQ_BIN, {bin,777}},
               {?PUSHQ_BIN, {bin,0}},
               {?PUSHQ_BIN, {bin,1}},
               {?PUSHQ_BIN, {bin,334}},
               {?ENQV, [{bin,0},{list,1},{bin,1},{bin,555}]},
               {?ENQV, [{bin,0},{list,1},{bin,1}]},
               {?PUSHQV, [{bin,0},{list,1},{bin,1},{bin,319}]}]),

    stop_driver(P, Name),
    ok.

q_echo(Port, SpecList) ->
    io:format("Heap binaries"),
    HeapData = [{M,build_data(T)} || {M,T} <- SpecList],
    {HeapDataReturn,HeapDataLen} = feed_driver(Port, HeapData),
    dequeue(Port, HeapDataReturn, HeapDataLen, 1),

    %% Try sub binaries.
    io:format("Sub binaries of heap binaries"),
    SubHeapData = make_sub_binaries(HeapData),
    %% The following line will generate a warning.
    {HeapDataReturn,HeapDataLen} = feed_driver(Port, SubHeapData),
    dequeue(Port, HeapDataReturn, HeapDataLen, 1),

    %% Try refc binaries.
    io:format("Refc binaries"),
    RefcData = make_refc_binaries(HeapData),
    {RefcDataReturn,RefcDataLen} = feed_driver(Port, RefcData),
    dequeue(Port, RefcDataReturn, RefcDataLen, 1),

    %% Try sub binaries of refc binaries.
    io:format("Sub binaries of refc binaries"),
    SubRefcData = make_sub_binaries(RefcData),
    {RefcDataReturn,RefcDataLen} = feed_driver(Port, SubRefcData),
    dequeue(Port, RefcDataReturn, RefcDataLen, 1),

    %% Try a writable binary.
    io:format("Writable binaries"),
    WritableBinData = make_writable_binaries(HeapData),
    {WritableDataReturn,WritableDatalen} = feed_driver(Port, WritableBinData),
    _ = append_to_writable_binaries(WritableBinData),
    dequeue(Port, WritableDataReturn, WritableDatalen, 1),

    %% Try dequeing more than one byte at the time.
    io:format("Heap binaries -- dequeueing more than one byte at the time"),
    feed_and_dequeue(Port, HeapData, 2),
    feed_and_dequeue(Port, HeapData, 3),
    feed_and_dequeue(Port, HeapData, 4),

    io:format("\n").

feed_and_dequeue(Port, Data, DeqSize) ->
    {DataReturn,DataLen} = feed_driver(Port, Data),
    dequeue(Port, DataReturn, DataLen, DeqSize),
    ok.

%% Send all data according to the specification to the driver side (where it
%% is queued up for later return to this process).    

feed_driver(Port, Description) ->
    feed_driver(Port, Description, <<>>, 0).

feed_driver(Port, [], ExpectedInPort, Qb) ->
    io:format("Expected in port: ~P", [ExpectedInPort,12]),
    io:format("In port: ~P", [read_head(Port, Qb),12]),
    {ExpectedInPort,Qb};
feed_driver(Port, [{Method0,Data}|T], Expected_return, Qb_before) ->
    Method = case Method0 of
                 ?RANDOM -> uniform(6)-1;
                 Other -> Other
             end,
    Size = size(list_to_binary([Data])),

    %% ***********************************************************************
    %% NOTE! Never never never change this to io:format/2, as that will imply
    %% message sending, and sending as message will spoil the test of
    %% writable binaries.

    %% erlang:display({sending,method_name(Method),Data}),
    %% ***********************************************************************

    queue_op(Port, Method, Data),

    Qb_in_driver = bytes_queued(Port),
    case Qb_before + Size of
        Qb_in_driver -> ok;
        Sum ->
            ct:fail("Qb_before: ~p\n"
                    "Qb_before+Size: ~p\n"
                    "Qb_in_driver: ~p",
                    [Qb_before,Sum,Qb_in_driver])
    end,
    X_return = case Method of
                   ?ENQ -> list_to_binary([Expected_return,Data]);
                   ?PUSHQ -> list_to_binary([Data,Expected_return]);
                   ?PUSHQ_BIN -> list_to_binary([Data,Expected_return]);
                   ?ENQ_BIN -> list_to_binary([Expected_return,Data]);
                   ?PUSHQV -> list_to_binary([Data,Expected_return]);
                   ?ENQV -> list_to_binary([Expected_return,Data])
               end,
    feed_driver(Port, T, X_return, Qb_before + Size).

%% method_name(0) -> pushq;
%% method_name(1) -> enq;
%% method_name(2) -> pushq_bin;
%% method_name(3) -> enq_bin;
%% method_name(4) -> pushqv;
%% method_name(5) -> enqv.

dequeue(Port, DataList, LenToGet, DeqSize) ->
    io:format("Dequeuing ~p bytes, ~p byte(s) at once...", [LenToGet,DeqSize]),
    compare_return(Port, DataList, LenToGet, DeqSize).

compare_return(Port, _Data_list, 0, _Back_len) ->
    0 = bytes_queued(Port);
compare_return(Port, QueuedInPort0, Len_to_get, DeqSize) ->
    case bytes_queued(Port) of
        Len_to_get -> ok;
        BytesInQueue ->
            ct:fail("Len_to_get: ~p\nBytes in queue: ~p", [Len_to_get,BytesInQueue])
    end,
    BytesToDequeue = if (DeqSize > Len_to_get) -> Len_to_get;
                        true -> DeqSize
                     end,
    Dequeued = read_head(Port, BytesToDequeue),
    case bin_prefix(Dequeued, QueuedInPort0) of
        true ->
            deq(Port, BytesToDequeue),
            <<_:BytesToDequeue/binary,QueuedInPort/binary>> = QueuedInPort0,
            compare_return(Port, QueuedInPort, Len_to_get - BytesToDequeue, DeqSize);
        false ->
            ct:fail("Bytes to dequeue: ~p\nDequeued: ~p\nQueued in port: ~P",
                    [BytesToDequeue, Dequeued, QueuedInPort0,12])
    end.

%% bin_prefix(PrefixBinary, Binary)
%%  Is PrefixBinary a prefix of Binary?

bin_prefix(<<C:8,PreTail/binary>>, <<C:8,Tail/binary>>) ->
    bin_prefix(PreTail, Tail);
bin_prefix(<<>>, _Bin) -> true;
bin_prefix(_, _) -> false.

queue_op(Port, Method, Data) ->
    [] = erlang:port_control(Port, Method, []),
    Port ! {self(),{command,Data}},
    ok.

bytes_queued(Port) ->
    case erlang:port_control(Port, ?BYTES_QUEUED, []) of
        <<I:32>> -> I;
        Bad -> ct:fail({bad_result,Bad})
    end.

deq(Port, Size) ->
    [] = erlang:port_control(Port, ?DEQ, <<Size:32>>).

read_head(Port, Size) ->
    erlang:port_control(Port, ?READ_HEAD, <<Size:32>>).


driver_unloaded(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Drv = timer_drv,
    User = self(),
    Loaded = make_ref(),
    Die = make_ref(),
    Loader = spawn(fun () ->
                           erl_ddll:start(),
                           ok = load_driver(proplists:get_value(data_dir,
                                                                Config),
                                            Drv),
                           User ! Loaded,
                           receive Die -> exit(bye) end
                   end),
    receive Loaded -> ok end,
    Port = open_port({spawn, Drv}, []),
    Loader ! Die,
    receive
        {'EXIT', Port, Reason} ->
            driver_unloaded = Reason
            %% Reason used to be -1
    end.


io_ready_exit(Config) when is_list(Config) ->
    OTE = process_flag(trap_exit, true),
    Test = self(),
    Dgawd = spawn(fun () ->
                          ok = dgawd_handler:install(),
                          Mon = erlang:monitor(process, Test),
                          Test ! dgawd_handler_started,
                          receive
                              {'DOWN', Mon, _, _, _} -> ok;
                              stop_dgawd_handler -> ok
                          end,
                          dgawd_handler:restore(),
                          Test ! dgawd_handler_stopped
                  end),
    receive dgawd_handler_started -> ok end,
    Drv = io_ready_exit_drv,
    erl_ddll:start(),
    ok = load_driver(proplists:get_value(data_dir, Config), Drv),
    Port = open_port({spawn, Drv}, []),
    case erlang:port_control(Port, 0, "") of
        "ok" ->
            receive
                {'EXIT', Port, Reason} ->
                    case Reason of
                        ready_output_driver_failure ->
                            io:format("Exited in output_ready()~n"),
                            ok;
                        ready_input_driver_failure ->
                            io:format("Exited in input_ready()~n"),
                            ok;
                        Error -> ct:fail(Error)
                    end
            end,
            receive after 2000 -> ok end,
            false = dgawd_handler:got_dgawd_report(),
            Dgawd ! stop_dgawd_handler,
            receive dgawd_handler_stopped -> ok end,
            process_flag(trap_exit, OTE),
            ok;
        "nyiftos" ->
            process_flag(trap_exit, OTE),
            {skipped, "Not yet implemented for this OS"};
        Error ->
            process_flag(trap_exit, OTE),
            ct:fail({unexpected_control_result, Error})
    end.


-define(CHKIO_STOP, 0).
-define(CHKIO_USE_FALLBACK_POLLSET, 1).
-define(CHKIO_BAD_FD_IN_POLLSET, 2).
-define(CHKIO_FD_CHANGE, 4).
-define(CHKIO_STEAL, 5).
-define(CHKIO_STEAL_AUX, 6).
-define(CHKIO_SMP_SELECT, 7).
-define(CHKIO_DRV_USE, 8).

use_fallback_pollset() ->
    [{timetrap, {minutes, 2}}].

use_fallback_pollset(Config) when is_list(Config) ->
    rpc(Config, fun() -> use_fallback_pollset_t(Config) end).

use_fallback_pollset_t(Config) when is_list(Config) ->
    FlbkFun = fun () ->
                      {Flbk, _} = get_fallback(erlang:system_info(check_io)),
                      case lists:keysearch(total_poll_set_size, 1, Flbk) of
                          {value, {total_poll_set_size, N}} when N > 0 ->
                              ok;
                          Error ->
                              ct:fail({failed_to_use_fallback, Error})
                      end
              end,
    {BckupTest, Handel, OkRes}
    = case chkio_test_init(Config) of
          {erts_poll_info, ChkIo} = Hndl ->
              case lists:keysearch(fallback, 1, ChkIo) of
                  {value, {fallback, B}} when B =/= false ->
                      {FlbkFun, Hndl, ok};
                  _ ->
                      {fun () -> ok end,
                       Hndl,
                       {comment,
                        "This implementation does not use "
                        "a fallback pollset"}}
              end;
          Skip ->
              {fun () -> ok end, Skip, ok}
      end,
    io:format("Node = ~p~n",[node()]),
    case chkio_test_fini(chkio_test(Handel,
                                    ?CHKIO_USE_FALLBACK_POLLSET,
                                    fun () ->
                                            sleep(1000),
                                            BckupTest()
                                    end)) of
        {skipped, _} = Res -> Res;
        _ -> OkRes
    end.

bad_fd_in_pollset(Config) when is_list(Config) ->
    rpc(Config,
        fun() ->
                chkio_test_fini(chkio_test(chkio_test_init(Config),
                                           ?CHKIO_BAD_FD_IN_POLLSET,
                                           fun () -> sleep(1000) end))
        end).

fd_change(Config) when is_list(Config) ->
    rpc(Config,
        fun() ->
                chkio_test_fini(chkio_test(chkio_test_init(Config),
                                           ?CHKIO_FD_CHANGE,
                                           fun () -> sleep(1000) end))
        end).

steal_control(Config) when is_list(Config) ->
    rpc(Config,
        fun() ->
                chkio_test_fini(case chkio_test_init(Config) of
                                    {erts_poll_info, _} = Hndl ->
                                        steal_control_test(Hndl);
                                    Skip ->
                                        Skip
                                end)
        end).

steal_control_test(Hndl = {erts_poll_info, Before}) ->
    Port = open_chkio_port(),
    case erlang:port_control(Port, ?CHKIO_STEAL_AUX, "") of
        [$f,$d,$s,$:| _] = FdList ->
            chk_chkio_port(Port),
            sleep(500),
            chk_chkio_port(Port),
            Res = chkio_test(Hndl,
                             ?CHKIO_STEAL,
                             FdList,
                             fun () ->
                                     chk_chkio_port(Port),
                                     sleep(500),
                                     chk_chkio_port(Port)
                             end),
            case erlang:port_control(Port, ?CHKIO_STOP, "") of
                "ok" ->
                    chk_chkio_port(Port),
                    ok;
                StopErr ->
                    chk_chkio_port(Port),
                    ct:fail({stop_error, StopErr})
            end,
            close_chkio_port(Port),
            Res;
        [$s,$k,$i,$p,$:,$\ |Skip] ->
            chk_chkio_port(Port),
            close_chkio_port(Port),
            {chkio_test_result,
             {skipped, Skip},
             Before};
        StartErr ->
            chk_chkio_port(Port),
            ct:fail({start_error, StartErr})
    end.

chkio_test_init(Config) when is_list(Config) ->
    ChkIo = get_stable_check_io_info(),
    case catch lists:keysearch(name, 1, ChkIo) of
        {value, {name, erts_poll}} ->
            ct:log("Before test: ~p~n", [ChkIo]),
            Path = proplists:get_value(data_dir, Config),
            erl_ddll:start(),
            ok = load_driver(Path, 'chkio_drv'),
            process_flag(trap_exit, true),
            {erts_poll_info, ChkIo};
        _ ->
            {skipped, "Test written to test erts_poll() which isn't used"}
    end.


chkio_test_fini({skipped, _} = Res) ->
    Res;
chkio_test_fini({chkio_test_result, Res, Before}) ->
    ok = erl_ddll:unload_driver('chkio_drv'),
    ok = erl_ddll:stop(),
    After = get_stable_check_io_info(),
    io:format("After test: ~p~n", [After]),
    verify_chkio_state(Before, After),
    Res.

open_chkio_port() ->
    open_port({spawn, 'chkio_drv'}, []).

close_chkio_port(Port) when is_port(Port) ->
    true = erlang:port_close(Port),
    receive
        {'EXIT', Port, normal} ->
            ok;
        {'EXIT', Port, Reason} ->
            ct:fail({abnormal_port_exit, Port, Reason});
        {Port, Message} ->
            ct:fail({strange_message_from_port, Message})
    end.

chk_chkio_port(Port) ->
    receive
        {'EXIT', Port, Reason} when Reason /= normal ->
            ct:fail({port_exited, Port, Reason})
    after 0 ->
              ok
    end.


chkio_test({skipped, _} = Res, _Test, _Fun) ->
    Res;
chkio_test({erts_poll_info, _Before} = EPI, Test, Fun) when is_integer(Test) ->
    chkio_test(EPI, Test, "", Fun).

chkio_test({skipped, _} = Res, _Test, _TestArgs, _Fun) ->
    Res;
chkio_test({erts_poll_info, Before},
           Test,
           TestArgs,
           Fun) when is_integer(Test),
                     is_list(TestArgs) ->
    Port = open_chkio_port(),
    case erlang:port_control(Port, Test, TestArgs) of
        "ok" ->
            chk_chkio_port(Port),
            Fun(),
            During = get_check_io_total(erlang:system_info(check_io)),
            erlang:display(During),

            0 = element(1, erts_debug:get_internal_state(check_io_debug)),
            io:format("During test: ~p~n", [During]),
            chk_chkio_port(Port),
            case erlang:port_control(Port, ?CHKIO_STOP, "") of
                Res when is_list(Res) ->
                    chk_chkio_port(Port),
                    io:format("~s", [Res]),
                    close_chkio_port(Port),
                    Res,
                    case Res of
                        [$c,$o,$m,$m,$e,$n,$t,$:,$\ |Cmnt] ->
                            {chkio_test_result,
                             {comment, Cmnt},
                             Before};
                        _ ->
                            {chkio_test_result,
                             Res,
                             Before}
                    end;
                StopErr ->
                    chk_chkio_port(Port),
                    ct:fail({stop_error, StopErr})
            end;
        [$s,$k,$i,$p,$:,$\ |Skip] ->
            chk_chkio_port(Port),
            close_chkio_port(Port),
            {chkio_test_result,
             {skipped, Skip},
             Before};
        StartErr ->
            chk_chkio_port(Port),
            ct:fail({start_error, StartErr})
    end.

verify_chkio_state(Before, After) ->
    TotSetSize = lists:keysearch(total_poll_set_size, 1, Before),
    TotSetSize = lists:keysearch(total_poll_set_size, 1, After),
    case lists:keysearch(fallback, 1, Before) of
        {value,{fallback,false}} ->
            ok;
        _ ->
            BckupSetSize = lists:keysearch(fallback_poll_set_size,
                                           1,
                                           Before),
            BckupSetSize = lists:keysearch(fallback_poll_set_size,
                                           1,
                                                       After)
    end,
    ok.

get_stable_check_io_info() ->
    get_stable_check_io_info(10).
get_stable_check_io_info(0) ->
    get_check_io_total(erlang:system_info(check_io));
get_stable_check_io_info(N) ->
    ChkIo = get_check_io_total(erlang:system_info(check_io)),
    PendUpdNo = proplists:get_value(pending_updates, ChkIo, 0),
    ActFds = proplists:get_value(active_fds, ChkIo),
    case {PendUpdNo, ActFds} of
        {0, 0} ->
            ChkIo;
        _ ->
            receive after 100 -> ok end,
            get_stable_check_io_info(N-1)
    end.

%% Merge return from erlang:system_info(check_io)
%% as if it was one big pollset.
get_check_io_total(ChkIo) ->
    ct:log("ChkIo = ~p~n",[ChkIo]),
    {Fallback, Rest} = get_fallback(ChkIo),
    add_fallback_infos(Fallback,
      lists:foldl(fun(Pollset, Acc) ->
                          lists:zipwith(fun(A, B) ->
                                                add_pollset_infos(A,B)
                                        end,
                                        Pollset, Acc)
                  end,
                  hd(Rest), tl(Rest))).

add_pollset_infos({Tag, A}=TA , {Tag, B}=TB) ->
    case tag_type(Tag) of
        sum ->
            {Tag, A + B};
        const ->
            case A of
                B -> TA;
                _ ->
                    ct:fail("Unexpected diff in pollsets ~p != ~p",
                            [TA,TB])
            end
    end.

get_fallback([MaybeFallback | ChkIo] = AllChkIo) ->
    case proplists:get_value(fallback, MaybeFallback) of
        true ->
            {MaybeFallback, ChkIo};
        false ->
            {undefined, AllChkIo}
    end.

add_fallback_infos(undefined, Acc) ->
    Acc;
add_fallback_infos(Flbk, Acc) ->
    lists:zipwith(fun({Tag, A}=TA, {Tag, B}=TB) ->
                          case tag_type(Tag) of
                              sum -> {Tag, A + B};
                              const when Tag =:= fallback -> TA;
                              const -> TB
                          end
                  end,
                  Flbk, Acc).

tag_type(name) -> const;
tag_type(primary) -> const;
tag_type(fallback) -> const;
tag_type(kernel_poll) -> const;
tag_type(memory_size) -> sum;
tag_type(total_poll_set_size) -> sum;
tag_type(lazy_updates) -> const;
tag_type(pending_updates) -> sum;
tag_type(batch_updates) -> const;
tag_type(concurrent_updates) -> const;
tag_type(max_fds) -> const;
tag_type(active_fds) -> sum;
tag_type(poll_threads) -> sum.


%% Missed port lock when stealing control of fd from a
%% driver that didn't use the same lock. The lock checker
%% used to trigger on this and dump core.
otp_6602(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config),
    Done = make_ref(),
    Parent = self(),
    Tester = spawn_link(Node,
                        fun () ->
                                %% Inet driver use port locking...
                                {ok, S} = gen_udp:open(0),
                                {ok, Fd} = inet:getfd(S),
                                %% Steal fd (lock checker used to
                                %% trigger here).
                                {ok, _S2} = gen_udp:open(0,[{fd,Fd}]),
                                Parent ! Done
                        end),
    receive Done -> ok end,
    unlink(Tester),
    stop_node(Node),
    ok.

-define(EXPECTED_SYSTEM_INFO_NAMES1,
        ["drv_drv_vsn",
         "emu_drv_vsn",
         "erts_vsn",
         "otp_vsn",
         "thread",
         "smp"]).
-define(EXPECTED_SYSTEM_INFO_NAMES2,
        (?EXPECTED_SYSTEM_INFO_NAMES1 ++
         ["async_thrs",
          "sched_thrs"])).

-define(EXPECTED_SYSTEM_INFO_NAMES3,
        (?EXPECTED_SYSTEM_INFO_NAMES2 ++
         ["emu_nif_vsn"])).

-define(EXPECTED_SYSTEM_INFO_NAMES4,
        (?EXPECTED_SYSTEM_INFO_NAMES3 ++
         ["dirty_sched"])).

-define(EXPECTED_SYSTEM_INFO_NAMES, ?EXPECTED_SYSTEM_INFO_NAMES4).

'driver_system_info_base_ver'(Config) when is_list(Config) ->
 driver_system_info_test(Config, sys_info_base_drv).

'driver_system_info_prev_ver'(Config) when is_list(Config) ->
 driver_system_info_test(Config, sys_info_prev_drv).

driver_system_info_current_ver(Config) when is_list(Config) ->
    driver_system_info_test(Config, sys_info_curr_drv).

driver_system_info_test(Config, Name) ->
    Port = start_driver(Config, Name, false),
    case erlang:port_control(Port, 0, []) of
        [$o,$k,$:,_ | Result] ->
            check_driver_system_info_result(Result);
        [$e,$r,$r,$o,$r,$:,_ | Error] ->
            ct:fail(Error);
        Unexpected ->
            ct:fail({unexpected_result, Unexpected})
    end,
    stop_driver(Port, Name),
    ok.

check_driver_system_info_result(Result) ->
    io:format("All names: ~p~n", [?EXPECTED_SYSTEM_INFO_NAMES]),
    io:format("Result: ~p~n", [Result]),
    {[], Ns, DDVSN} = chk_sis(lists:map(fun (Str) ->
                                                string:lexemes(Str, "=")
                                        end,
                                        string:lexemes(Result, " ")),
                              ?EXPECTED_SYSTEM_INFO_NAMES),
    case {DDVSN,
          drv_vsn_str2tup(erlang:system_info(driver_version))} of
        {DDVSN, DDVSN} ->
            [] = Ns;
        %% {{1, 0}, _} ->
        %% 	  ExpNs = lists:sort(?EXPECTED_SYSTEM_INFO_NAMES
        %% 				   -- ?EXPECTED_SYSTEM_INFO_NAMES1),
        %% 	  ExpNs = lists:sort(Ns);
        %% {{1, 1}, _} ->
        %% 	  ExpNs = lists:sort(?EXPECTED_SYSTEM_INFO_NAMES
        %% 				   -- ?EXPECTED_SYSTEM_INFO_NAMES2),
        %% 	  ExpNs = lists:sort(Ns);
        {{3, 0}, _} ->
            ExpNs = lists:sort(?EXPECTED_SYSTEM_INFO_NAMES
                               -- ?EXPECTED_SYSTEM_INFO_NAMES3),
            ExpNs = lists:sort(Ns)
    end.

chk_sis(SIs, Ns) ->
    chk_sis(SIs, Ns, unknown).

chk_sis(SIs, [], DDVSN) ->
    {SIs, [], DDVSN};
chk_sis([], Ns, DDVSN) ->
    {[], Ns, DDVSN};
chk_sis([[N, _] = SI| SIs], Ns, DDVSN) ->
    true = lists:member(N, Ns),
    case check_si_res(SI) of
        {driver_version, NewDDVSN} ->
            chk_sis(SIs, lists:delete(N, Ns), NewDDVSN);
        _ ->
            chk_sis(SIs, lists:delete(N, Ns), DDVSN)
    end.

%% Data in first version of driver_system_info() (driver version 1.0)
check_si_res(["drv_drv_vsn", Value]) ->
    DDVSN = drv_vsn_str2tup(Value),
    {Major, DMinor} = DDVSN,
    {Major, EMinor} = drv_vsn_str2tup(erlang:system_info(driver_version)),
    true = DMinor =< EMinor,
    {driver_version, DDVSN};
check_si_res(["emu_drv_vsn", Value]) ->
    Value = erlang:system_info(driver_version);
check_si_res(["erts_vsn", Value]) ->
    Value = erlang:system_info(version);
check_si_res(["otp_vsn", Value]) ->
    Value = erlang:system_info(otp_release);
check_si_res(["thread", "true"]) ->
    true = erlang:system_info(threads);
check_si_res(["thread", "false"]) ->
    false = erlang:system_info(threads);
check_si_res(["smp", "true"]) ->
    true = erlang:system_info(smp_support);

%% Data added in second version of driver_system_info() (driver version 1.1)
check_si_res(["async_thrs", Value]) ->
    Value = integer_to_list(erlang:system_info(thread_pool_size));
check_si_res(["sched_thrs", Value]) ->
    Value = integer_to_list(erlang:system_info(schedulers));

%% Data added in 3rd version of driver_system_info() (driver version 1.5)
check_si_res(["emu_nif_vsn", Value]) ->
    Value = erlang:system_info(nif_version);

%% Data added in 4th version of driver_system_info() (driver version 3.1)
check_si_res(["dirty_sched", _Value]) ->
    true;

check_si_res(Unexpected) ->
    ct:fail({unexpected_result, Unexpected}).

-define(MON_OP_I_AM_IPID,1).
-define(MON_OP_MONITOR_ME,2).
-define(MON_OP_DEMONITOR_ME,3).
-define(MON_OP_MONITOR_ME_LATER,4).
-define(MON_OP_DO_DELAYED_MONITOR,5).

%% Test monitoring of processes from drivers
driver_monitor(Config) when is_list(Config) ->
    Name = monitor_drv,
    Port = start_driver(Config, Name, false),
    "ok" = port_control(Port,?MON_OP_I_AM_IPID,[]),
    "ok" = port_control(Port,?MON_OP_MONITOR_ME,[]),
    "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    {monitors, []} = erlang:port_info(Port,monitors),

    "ok:"++Id1 = port_control(Port,?MON_OP_MONITOR_ME_LATER,[]),
    {monitored_by, []} = process_info(self(),monitored_by),
    "ok" = port_control(Port,?MON_OP_DO_DELAYED_MONITOR,Id1),
    {monitored_by, [Port]} = process_info(self(),monitored_by),
    "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    {monitored_by, []} = process_info(self(),monitored_by),

    "ok" = port_control(Port,?MON_OP_MONITOR_ME,[]),
    Me = self(),
    {Pid1,Ref1} =
    spawn_monitor(fun() ->
                          Me ! port_control(Port,?MON_OP_MONITOR_ME,[]), 
                          Me ! process_info(self(),monitored_by), 
                          Me ! erlang:port_info(Port,monitors) 
                  end),
    ok = receive
             "ok" ->
                 ok
         after 1000 ->
                   timeout
         end,
    ok = receive
             {monitored_by, L} ->
                 L2 = lists:sort(L),
                 L3 = lists:sort([Me,Port]),
                 case L2 of
                     L3 ->
                         ok;
                     _ ->
                         mismatch
                 end
         after 1000 ->
                   timeout
         end,
    ok = receive
             {monitors, LL} ->
                 LL2 = lists:sort(LL),
                 LL3 = lists:sort([{process,Me},{process,Pid1}]),
                 case LL2 of
                     LL3 ->
                         ok;
                     _ ->
                         mismatch
                 end
         after 1000 ->
                   timeout
         end,
    ok = receive
             {'DOWN', Ref1, process, Pid1, _} ->
                 ok
         after 1000 ->
                   timeout
         end,
    ok = receive
             {monitor_fired,Port,Pid1} ->
                 ok
         after 1000 ->
                   timeout
         end,
    "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    {monitors,[]} = erlang:port_info(Port,monitors),
    {monitored_by, []} = process_info(self(),monitored_by),

    "ok" = port_control(Port,?MON_OP_MONITOR_ME,[]),
    {Pid2,Ref2} =
    spawn_monitor(fun() -> 			       
                          receive go -> ok end,
                          Me ! port_control(Port,?MON_OP_MONITOR_ME_LATER,[]), 
                          Me ! process_info(self(),monitored_by), 
                          Me ! erlang:port_info(Port,monitors) 
                  end),
    Pid2 ! go,
    {ok,Id2} = receive
                   "ok:"++II ->
                       {ok,II}
               after 1000 ->
                         timeout
               end,
    ok = receive
             {monitored_by, [Me]} ->
                 ok
         after 1000 ->
                   timeout
         end,
    ok = receive
             {monitors, [{process,Me}]} ->
                 ok
         after 1000 ->
                   timeout
         end,
    ok = receive
             {'DOWN', Ref2, process, Pid2, _} ->
                 ok
         after 1000 ->
                   timeout
         end,
    "noproc" = port_control(Port,?MON_OP_DO_DELAYED_MONITOR,Id2),
    {monitors,[{process,Me}]} = erlang:port_info(Port,monitors),
    "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    "not_monitored" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    {monitors,[]} = erlang:port_info(Port,monitors),
    {monitored_by, []} = process_info(self(),monitored_by),


    "ok" = port_control(Port,?MON_OP_MONITOR_ME,[]),
    {Pid3,Ref3} =
    spawn_monitor(fun() ->
                          receive go -> ok end,
                          Me ! port_control(Port,?MON_OP_MONITOR_ME_LATER,[]), 
                          Me ! process_info(self(),monitored_by), 
                          Me ! erlang:port_info(Port,monitors) ,
                          receive die -> ok end
                  end),
    Pid3 ! go,
    {ok,Id3} = receive
                   "ok:"++III ->
                       {ok,III}
               after 1000 ->
                         timeout
               end,
    ok = receive
             {monitored_by, [Me]} ->
                 ok
         after 1000 ->
                   timeout
         end,
    ok = receive
             {monitors, [{process,Me}]} ->
                 ok
         after 1000 ->
                   timeout
         end,
    "ok" = port_control(Port,?MON_OP_DO_DELAYED_MONITOR,Id3),
    LLL1 = lists:sort([{process,Me},{process,Pid3}]),
    {monitors,LLL2} = erlang:port_info(Port,monitors),
    LLL1 = lists:sort(LLL2),
    "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    {monitors,[{process,Pid3}]} = erlang:port_info(Port,monitors),
    Pid3 ! die,
    ok = receive
             {'DOWN', Ref3, process, Pid3, _} ->
                 ok
         after 1000 ->
                   timeout
         end,
    "not_found" = port_control(Port,?MON_OP_DO_DELAYED_MONITOR,Id2),
    {monitors,[]} = erlang:port_info(Port,monitors),
    "not_monitored" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    {monitors,[]} = erlang:port_info(Port,monitors),
    {monitored_by, []} = process_info(self(),monitored_by),

    stop_driver(Port, Name),
    ok.


-define(IOQ_EXIT_READY_INPUT, 1).
-define(IOQ_EXIT_READY_OUTPUT, 2).
-define(IOQ_EXIT_TIMEOUT, 3).
-define(IOQ_EXIT_READY_ASYNC, 4).
-define(IOQ_EXIT_READY_INPUT_ASYNC, 6).
-define(IOQ_EXIT_READY_OUTPUT_ASYNC, 7).
-define(IOQ_EXIT_TIMEOUT_ASYNC, 8).

ioq_exit_test(Config, TestNo) ->
    Drv = ioq_exit_drv,
    try
        begin
            case load_driver(proplists:get_value(data_dir, Config),
                             Drv) of
                ok -> ok;
                {error, permanent} -> ok;
                LoadError -> ct:fail({load_error, LoadError})
            end,
            case open_port({spawn, Drv}, []) of
                Port when is_port(Port) ->
                    try port_control(Port, TestNo, "") of
                        "ok" ->
                            ok;
                        "nyiftos" ->
                            throw({skipped,
                                   "Not yet implemented for "
                                   "this OS"});
                        [$s,$k,$i,$p,$:,$ | Comment] ->
                            throw({skipped, Comment});
                        [$e,$r,$r,$o,$r,$:,$ | Error] ->
                            ct:fail(Error)
                    after
                        Port ! {self(), close},
                        receive {Port, closed} -> ok end,
                        false = lists:member(Port, erlang:ports()),
                        ok
                    end;
                Error ->
                    ct:fail({open_port_failed, Error})
            end
        end
    catch
        throw:Term -> Term
    after
        erl_ddll:unload_driver(Drv)
    end.

ioq_exit_ready_input(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_INPUT).

ioq_exit_ready_output(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_OUTPUT).

ioq_exit_timeout(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_TIMEOUT).

ioq_exit_ready_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_ASYNC).

ioq_exit_ready_input_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_INPUT_ASYNC).

ioq_exit_ready_output_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_OUTPUT_ASYNC).

ioq_exit_timeout_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_TIMEOUT_ASYNC).


vsn_mismatch_test(Config, LoadResult) ->
    Path = proplists:get_value(data_dir, Config),
    DrvName = proplists:get_value(testcase, Config),
    LoadResult = load_driver(Path, DrvName),
    case LoadResult of
        ok ->
            Port = open_port({spawn, DrvName}, []),
            true = is_port(Port),
            true = port_close(Port),
            ok = erl_ddll:unload_driver(DrvName);
        _ ->
            ok
    end.

zero_extended_marker_garb_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

invalid_extended_marker_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

larger_major_vsn_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

larger_minor_vsn_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

smaller_major_vsn_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

smaller_minor_vsn_drv(Config) when is_list(Config) ->
    DrvVsnStr = erlang:system_info(driver_version),
    case drv_vsn_str2tup(DrvVsnStr) of
        {_, 0} ->
            {skipped,
             "Cannot perform test when minor driver version is 0. "
             "Current driver version is " ++ DrvVsnStr ++ "."};
        _ ->
            vsn_mismatch_test(Config, ok)
    end.

-define(PEEK_NONXQ_TEST, 0).
-define(PEEK_NONXQ_WAIT, 1).

peek_non_existing_queue(Config) when is_list(Config) ->
    OTE = process_flag(trap_exit, true),
    Drv = peek_non_existing_queue_drv,
    try
        begin
            case load_driver(proplists:get_value(data_dir, Config),
                             Drv) of
                ok -> ok;
                {error, permanent} -> ok;
                LoadError -> ct:fail({load_error, LoadError})
            end,
            case open_port({spawn, Drv}, []) of
                Port1 when is_port(Port1) ->
                    try port_control(Port1, ?PEEK_NONXQ_TEST, "") of
                        "ok" ->
                            ok;
                        [$s,$k,$i,$p,$p,$e,$d,$:,$ | SkipReason] ->
                            throw({skipped, SkipReason});
                        [$e,$r,$r,$o,$r,$:,$ | Error1] ->
                            ct:fail(Error1)
                    after
                        exit(Port1, kill),
                        receive {'EXIT', Port1, _} -> ok end
                    end;
                Error1 ->
                    ct:fail({open_port1_failed, Error1})
            end,
            case open_port({spawn, Drv}, []) of
                Port2 when is_port(Port2) ->
                    try port_control(Port2, ?PEEK_NONXQ_WAIT, "") of
                        "ok" ->
                            ok;
                        [$e,$r,$r,$o,$r,$:,$ | Error2] ->
                            ct:fail(Error2)
                    after
                        receive {Port2, test_successful} -> ok end,
                        Port2 ! {self(), close},
                        receive {Port2, closed} -> ok end
                    end;
                Error2 ->
                    ct:fail({open_port2_failed, Error2})
            end
        end
    catch
        throw:Term -> Term
    after
        process_flag(trap_exit, OTE),
        erl_ddll:unload_driver(Drv)
    end.    

otp_6879(Config) when is_list(Config) ->
    Drv = 'otp_6879_drv',
    Parent = self(),
    ok = load_driver(proplists:get_value(data_dir, Config), Drv),
    Procs = lists:map(
              fun (No) ->
                      spawn_link(
                        fun () ->
                                case open_port({spawn, Drv}, []) of
                                    Port when is_port(Port) ->
                                        Res = otp_6879_call(Port, No, 10000),
                                        erlang:port_close(Port),
                                        Parent ! {self(), Res};
                                    _ ->
                                        Parent ! {self(),
                                                  open_port_failed}
                                end
                        end)
              end,
              lists:seq(1,10)),
    lists:foreach(fun (P) ->
                          receive
                              {P, ok} ->
                                  ok;
                              {P, Error} ->
                                  ct:fail({P, Error})
                          end
                  end,
                  Procs),
    %% Also try it when input exceeds default buffer (256 bytes)
    Data = lists:seq(1, 1000),
    case open_port({spawn, Drv}, []) of
        Port when is_port(Port) ->
            ok = otp_6879_call(Port, Data, 10),
            erlang:port_close(Port);
        _ ->
            ct:fail(open_port_failed)
    end,
    erl_ddll:unload_driver(Drv),
    ok.

otp_6879_call(_Port, _Data, 0) ->
    ok;
otp_6879_call(Port, Data, N) ->
    case catch erlang:port_call(Port, 0, Data) of
        Data -> otp_6879_call(Port, Data, N-1);
        BadData -> {mismatch, Data, BadData}
    end.

caller(Config) when is_list(Config) ->
    run_caller_test(Config, false),
    run_caller_test(Config, true).

run_caller_test(Config, Outputv) ->
    Drv = 'caller_drv',
    Cmd = case Outputv of
              true ->
                  os:putenv("CALLER_DRV_USE_OUTPUTV",
                            "true"),
                  outputv;
              false ->
                  os:putenv("CALLER_DRV_USE_OUTPUTV",
                            "false"),
                  output
          end,
    ok = load_driver(proplists:get_value(data_dir, Config), Drv),
    Port = open_port({spawn, Drv}, []),
    true = is_port(Port),
    chk_caller(Port, start, self()),
    chk_caller(Port,
               Cmd,
               spawn_link(
                 fun () ->
                         port_command(Port, "")
                 end)),
    Port ! {self(), {command, ""}},
    chk_caller(Port, Cmd, self()),
    chk_caller(Port,
               control,
               spawn_link(
                 fun () ->
                         port_control(Port, 0, "")
                 end)),
    chk_caller(Port,
               call,
               spawn_link(
                 fun () ->
                         erlang:port_call(Port, 0, "")
                 end)),
    true = port_close(Port),
    erl_ddll:unload_driver(Drv),
    ok.

chk_caller(Port, Callback, ExpectedCaller) ->
    receive
        {caller, Port, Callback, Caller} ->
            ExpectedCaller = Caller
    end.

%% Check that many simultaneously signalled events work (win32)
many_events(Config) when is_list(Config) ->
    Name = 'many_events_drv',
    Port = start_driver(Config, Name, false),
    Number = "1000",
    Port ! {self(), {command, Number}},
    receive 
        {Port, {data,Number}} ->
            receive %% Just to make sure the emulator does not crash 
                %% after this case is run (if faulty)
            after 2000 ->
                      ok
            end
    after 1000 ->
              exit(the_driver_does_not_respond)
    end,
    stop_driver(Port, Name),
    ok.


missing_callbacks(Config) when is_list(Config) ->
    Name = 'missing_callback_drv',
    Port = start_driver(Config, Name, false),

    Port ! {self(), {command, "tjenix"}},
    true = erlang:port_command(Port, "halloj"),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(Port, 4711, "mors")),
    {'EXIT', {badarg, _}} = (catch erlang:port_call(Port, 17, "hej")),

    %% Give the (non-existing) ready_output(), ready_input(), event(),
    %% and timeout() some time to be called.
    receive after 1000 -> ok end,

    stop_driver(Port, Name),
    ok.

%% Test concurrent calls to driver_select.
smp_select(Config) when is_list(Config) -> 
    case os:type() of
        {win32,_} -> {skipped, "Test not implemented for this OS"};
        _ -> rpc(Config, fun() -> smp_select0(Config) end)
    end.

smp_select0(Config) ->
    DrvName = 'chkio_drv',
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, DrvName),    
    Master = self(),
    ProcFun = fun()-> io:format("Worker ~p starting\n",[self()]),	
                      Port = open_port({spawn, DrvName}, []),
                      smp_select_loop(Port, 100000),
                      sleep(1000), % wait for driver to handle pending events
                      true = erlang:port_close(Port),
                      Master ! {ok,self()},
                      io:format("Worker ~p finished\n",[self()])
              end,
    Pids = lists:map(fun(_) -> spawn_link(ProcFun) end,
                     lists:seq(1,4)),
    TimeoutMsg = make_ref(),
    {ok,TRef} = timer:send_after(5*1000, TimeoutMsg), % Limit test duration on slow machines
    smp_select_wait(Pids, TimeoutMsg),
    timer:cancel(TRef),
    ok = erl_ddll:unload_driver(DrvName),
    ok = erl_ddll:stop(),
    ok.

smp_select_loop(_, 0) ->
    ok;
smp_select_loop(Port, N) ->
    case erlang:port_control(Port, ?CHKIO_SMP_SELECT, []) of
        "yield" -> erlang:yield();
        "ok" -> ok
    end,
    receive
        stop -> 
            io:format("Worker ~p stopped with ~p laps left\n",[self(), N]), 
            ok
    after 0 ->
              smp_select_loop(Port, N-1)
    end.

smp_select_wait([], _) ->
    ok;
smp_select_wait(Pids, TimeoutMsg) ->
    receive
        {ok,Pid} when is_pid(Pid) ->
            smp_select_wait(lists:delete(Pid,Pids), TimeoutMsg);
        TimeoutMsg ->
            lists:foreach(fun(Pid)-> Pid ! stop end,
                          Pids),
            smp_select_wait(Pids, TimeoutMsg)
    end.


%% Test driver_select() with new ERL_DRV_USE flag.
driver_select_use(Config) when is_list(Config) -> 
    case os:type() of
        {win32,_} -> {skipped, "Test not implemented for this OS"};
        _ -> rpc(Config, fun() -> driver_select_use0(Config) end)
    end.

driver_select_use0(Config) ->
    DrvName = 'chkio_drv',
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, DrvName),    
    Port = open_port({spawn, DrvName}, []),
    "ok" = erlang:port_control(Port, ?CHKIO_DRV_USE, []),
    {Port,{data,"TheEnd"}} = receive Msg -> Msg
                             after 10000 -> timeout end,
    true = erlang:port_close(Port),
    ok = erl_ddll:unload_driver(DrvName),
    ok = erl_ddll:stop(),
    ok.

thread_mseg_alloc_cache_clean(Config) when is_list(Config) ->
    case {erlang:system_info(threads),
          erlang:system_info({allocator,mseg_alloc}),
          driver_alloc_sbct()} of
        {_, false, _} ->
            {skipped, "No mseg_alloc"};
        {false, _, _} ->
            {skipped, "No threads"};
        {_, _, false} ->
            {skipped, "driver_alloc() not using the alloc_util framework"};
        {_, _, SBCT} when is_integer(SBCT), SBCT > 10*1024*1024 ->
            {skipped, "driver_alloc() using too large single block threshold"};
        {_, _, 0} ->
            {skipped, "driver_alloc() using too low single block threshold"};
        {true, _MsegAllocInfo, SBCT} ->
            DrvName = 'thr_alloc_drv',
            Path = proplists:get_value(data_dir, Config),
            erl_ddll:start(),
            ok = load_driver(Path, DrvName),   
            Port = open_port({spawn, DrvName}, []),
            CCI = 1000,
            io:format("CCI = ~p~n", [CCI]),
            CCC = mseg_alloc_ccc(),
            io:format("CCC = ~p~n", [CCC]),
            thread_mseg_alloc_cache_clean_test(Port,
                                               10,
                                               CCI,
                                               SBCT+100),
            true = erlang:port_close(Port),
            ok = erl_ddll:unload_driver(DrvName),
            ok = erl_ddll:stop(),
            ok
    end.

mseg_alloc_ccc() ->
    mseg_alloc_ccc(mseg_inst_info(0)).

mseg_alloc_ccc(MsegAllocInfo) ->
    {value,{memkind, MKL}} = lists:keysearch(memkind,1,MsegAllocInfo),
    {value,{calls, CL}} = lists:keysearch(calls, 1, MKL),
    {value,{mseg_check_cache, GigaCCC, CCC}}
    = lists:keysearch(mseg_check_cache, 1, CL),
    GigaCCC*1000000000 + CCC.

mseg_alloc_cached_segments() ->
    mseg_alloc_cached_segments(mseg_inst_info(0)).

mseg_alloc_cached_segments(MsegAllocInfo) ->
    MemName = "all memory",
    [{memkind,DrvMem}]
    = lists:filter(fun(E) -> case E of
                                 {memkind, [{name, MemName} | _]} -> true;
                                 _ -> false
                             end end, MsegAllocInfo),
    {value,{status, SL}}
    = lists:keysearch(status, 1, DrvMem),
    {value,{cached_segments, CS}}
    = lists:keysearch(cached_segments, 1, SL),
    CS.

mseg_inst_info(I) ->
    {value, {instance, I, Value}}
    = lists:keysearch(I,
                      2,
                      erlang:system_info({allocator,mseg_alloc})),
    Value.

driver_alloc_sbct() ->
    {_, _, _, As} = erlang:system_info(allocator),
    case lists:keysearch(driver_alloc, 1, As) of
        {value,{driver_alloc,DAOPTs}} ->
            case lists:keysearch(sbct, 1, DAOPTs) of
                {value,{sbct,SBCT}} ->
                    SBCT;
                _ ->
                    false
            end;
        _ ->
            false
    end.

thread_mseg_alloc_cache_clean_test(_Port, 0, _CCI, _Size) ->
    ok;
thread_mseg_alloc_cache_clean_test(Port, N, CCI, Size) ->
    wait_until(fun () -> 0 == mseg_alloc_cached_segments() end),
    receive after CCI+500 -> ok end,
    OCCC = mseg_alloc_ccc(),
    "ok" = erlang:port_control(Port, 0, integer_to_list(Size)),
    receive after CCI+500 -> ok end,
    CCC = mseg_alloc_ccc(),
    io:format("CCC = ~p~n", [CCC]),
    true = CCC > OCCC,
    thread_mseg_alloc_cache_clean_test(Port, N-1, CCI, Size).

otp_9302(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, otp_9302_drv),
    Port = open_port({spawn, otp_9302_drv}, []),
    true = is_port(Port),
    port_command(Port, ""),
    {msg, block} = get_port_msg(Port, infinity),
    {msg, job} = get_port_msg(Port, infinity),
    C = case erlang:system_info(thread_pool_size) of
            0 ->
                {msg, cancel} = get_port_msg(Port, infinity),
                {msg, job} = get_port_msg(Port, infinity),
                false;
            _ ->
                case get_port_msg(Port, infinity) of
                    {msg, cancel} -> %% Cancel always fail in Rel >= 15
                        {msg, job} = get_port_msg(Port, infinity),
                        false;
                    {msg, job} ->
                        ok,
                        true
                end
        end,
    {msg, end_of_jobs} = get_port_msg(Port, infinity),
    no_msg = get_port_msg(Port, 2000),
    port_close(Port),
    case C of
        true ->
            {comment, "Async job cancelled"};
        false ->
            {comment, "Async job not cancelled"}
    end.

thr_free_drv(Config) when is_list(Config) ->
    case erlang:system_info(threads) of
        false ->
            {skipped, "No thread support"};
        true ->
            thr_free_drv_do(Config)
    end.

thr_free_drv_do(Config) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, thr_free_drv),
    MemBefore = driver_alloc_size(),
    %    io:format("SID=~p", [erlang:system_info(scheduler_id)]),
    Port = open_port({spawn, thr_free_drv}, []),
    MemPeek = driver_alloc_size(),
    true = is_port(Port),
    ok = thr_free_drv_control(Port, 0),
    port_close(Port),
    MemAfter = driver_alloc_size(),
    io:format("MemPeek=~p~n", [MemPeek]),
    io:format("MemBefore=~p, MemAfter=~p~n", [MemBefore, MemAfter]),
    MemBefore = MemAfter,
    case MemPeek of
        undefined -> ok;
        _ ->
            true = MemPeek > MemBefore
    end,
    ok.

thr_free_drv_control(Port, N) ->
    case erlang:port_control(Port, 0, "") of
        "done" ->
            ok;
        "more" ->
            erlang:yield(),
            %	    io:format("N=~p, SID=~p", [N, erlang:system_info(scheduler_id)]),
            thr_free_drv_control(Port, N+1)
    end.

async_blast(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, async_blast_drv),
    SchedOnln = erlang:system_info(schedulers_online),
    MemBefore = driver_alloc_size(),
    Start = os:timestamp(),
    Blast = fun () ->
                    Port = open_port({spawn, async_blast_drv}, []),
                    true = is_port(Port),
                    port_command(Port, ""),
                    receive
                        {Port, done} ->
                            ok
                    end,
                    port_close(Port)
            end,
    Ps = lists:map(fun (N) ->
                           spawn_opt(Blast,
                                     [{scheduler,
                                       (N rem SchedOnln)+ 1},
                                      monitor])
                   end,
                   lists:seq(1, 100)),
    MemMid = driver_alloc_size(),
    lists:foreach(fun ({Pid, Mon}) ->
                          receive
                              {'DOWN',Mon,process,Pid,_} -> ok
                          end
                  end, Ps),
    End = os:timestamp(),
    MemAfter = driver_alloc_size(),
    io:format("MemBefore=~p, MemMid=~p, MemAfter=~p~n",
              [MemBefore, MemMid, MemAfter]),
    AsyncBlastTime = timer:now_diff(End,Start)/1000000,
    io:format("AsyncBlastTime=~p~n", [AsyncBlastTime]),
    MemBefore = MemAfter,
    erlang:display({async_blast_time, AsyncBlastTime}),
    ok.

thr_msg_blast_receiver(_Port, N, N) ->
    ok;
thr_msg_blast_receiver(Port, N, Max) ->
    receive
        {Port, hi} ->
            thr_msg_blast_receiver(Port, N+1, Max)
    end.

thr_msg_blast_receiver_proc(Port, Max, Parent, Done) ->
    case port_control(Port, 0, "") of
        "receiver" ->
            spawn(fun () ->
                          thr_msg_blast_receiver_proc(Port, Max+1, Parent, Done)
                  end),
            thr_msg_blast_receiver(Port, 0, Max);
        "done" ->
            Parent ! Done
    end.

thr_msg_blast(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, thr_msg_blast_drv),
    MemBefore = driver_alloc_size(),
    Start = os:timestamp(),
    Port = open_port({spawn, thr_msg_blast_drv}, []),
    true = is_port(Port),
    Done = make_ref(),
    Me = self(),
    spawn(fun () ->
                  thr_msg_blast_receiver_proc(Port, 1, Me, Done)
          end),
    receive
        Done -> ok
    end,
    ok = thr_msg_blast_receiver(Port, 0, 32*10000),
    port_close(Port),
    End = os:timestamp(),
    receive
        Garbage ->
            ct:fail({received_garbage, Port, Garbage})
    after 2000 ->
            ok
    end,
    MemAfter = driver_alloc_size(),
    io:format("MemBefore=~p, MemAfter=~p~n",
              [MemBefore, MemAfter]),
    ThrMsgBlastTime = timer:now_diff(End,Start)/1000000,
    io:format("ThrMsgBlastTime=~p~n", [ThrMsgBlastTime]),
    MemBefore = MemAfter,
    Res = {thr_msg_blast_time, ThrMsgBlastTime},
    erlang:display(Res),
    Res.

-define(IN_RANGE(LoW_, VaLuE_, HiGh_),
        case in_range(LoW_, VaLuE_, HiGh_) of
            true -> ok;
            false ->
                case erlang:system_info(lock_checking) of
                    true ->
                        io:format("~p:~p: Ignore bad sched count due to "
                                  "lock checking~n",
                                  [?MODULE,?LINE]);
                    false ->
                        ct:fail({unexpected_sched_counts, VaLuE_})
                end
        end).


consume_timeslice(Config) when is_list(Config) ->
    %%
    %% Verify that erl_drv_consume_timeslice() works.
    %%
    %% The first four cases expect that the command signal is
    %% delivered immediately, i.e., isn't scheduled. Since there
    %% are no conflicts these signals should normally be delivered
    %% immediately. However some builds and configurations may
    %% schedule these ops anyway, in these cases we do not verify
    %% scheduling counts.
    %%
    %% When signal is delivered immediately we must take into account
    %% that process and port are "virtualy" scheduled out and in
    %% in the trace generated.
    %%
    %% Port ! {_, {command, _}, and port_command() differs. The send
    %% instruction needs to check if the caller is out of reductions
    %% at the end of the instruction, since no erlang function call
    %% is involved. Otherwise, a sequence of send instructions would
    %% not be scheduled out even when out of reductions. port_commond()
    %% doesn't do that since it will always (since R16A) be called via
    %% the erlang wrappers in the erlang module.
    %%
    %% The last two cases tests scheduled operations. We create
    %% a conflict by executing at the same time on different
    %% schedulers. When only one scheduler we enable parallelism on
    %% the port instead.
    %%

    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(Path, consume_timeslice_drv),
    Port = open_port({spawn, consume_timeslice_drv}, [{parallelism, false}]),

    Parent = self(),
    Go = make_ref(),

    "enabled" = port_control(Port, $E, ""),
    Proc1 = spawn_link(fun () ->
                               receive Go -> ok end,
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}}
                       end),
    receive after 100 -> ok end,
    count_pp_sched_start(),
    Proc1 ! Go,
    wait_command_msgs(Port, 10),
    [{Port, Sprt1}, {Proc1, Sproc1}] = count_pp_sched_stop([Port, Proc1]),
    ?IN_RANGE(10, Sprt1, 10),
    ?IN_RANGE(5, Sproc1-10, 7),

    "disabled" = port_control(Port, $D, ""),
    Proc2 = spawn_link(fun () ->
                               receive Go -> ok end,
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}},
                               Port ! {Parent, {command, ""}}
                       end),
    receive after 100 -> ok end,
    count_pp_sched_start(),
    Proc2 ! Go,
    wait_command_msgs(Port, 10),
    [{Port, Sprt2}, {Proc2, Sproc2}] = count_pp_sched_stop([Port, Proc2]),
    ?IN_RANGE(10, Sprt2, 10),
    ?IN_RANGE(1, Sproc2-10, 2),

    "enabled" = port_control(Port, $E, ""),
    Proc3 = spawn_link(fun () ->
                               receive Go -> ok end,
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, "")
                       end),
    count_pp_sched_start(),
    Proc3 ! Go,
    wait_command_msgs(Port, 10),
    [{Port, Sprt3}, {Proc3, Sproc3}] = count_pp_sched_stop([Port, Proc3]),
    ?IN_RANGE(10, Sprt3, 10),
    ?IN_RANGE(5, Sproc3-10, 7),

    "disabled" = port_control(Port, $D, ""),
    Proc4 = spawn_link(fun () ->
                               receive Go -> ok end,
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, ""),
                               port_command(Port, "")
                       end),
    count_pp_sched_start(),
    Proc4 ! Go,
    wait_command_msgs(Port, 10),
    [{Port, Sprt4}, {Proc4, Sproc4}] = count_pp_sched_stop([Port, Proc4]),
    ?IN_RANGE(10, Sprt4, 10),
    ?IN_RANGE(1, Sproc4-10, 2),

    SOnl = erlang:system_info(schedulers_online),
    %% If only one scheduler use port with parallelism set to true,
    %% in order to trigger scheduling of command signals
    Port2 = case SOnl of
                1 ->
                    Port ! {self(), close},
                    receive {Port, closed} -> ok end,
                    open_port({spawn, consume_timeslice_drv},
                              [{parallelism, true}]);
                _ ->
                    process_flag(scheduler, 1),
                    1 = erlang:system_info(scheduler_id),
                    Port
            end,
    count_pp_sched_start(),
    "enabled" = port_control(Port2, $E, ""),
    W5 = case SOnl of
             1 ->
                 false;
             _ ->
                 W1= spawn_opt(fun () ->
                                       2 = erlang:system_info(scheduler_id),
                                       "sleeped" = port_control(Port2, $S, "")
                               end, [link,{scheduler,2}]),
                 receive after 100 -> ok end,
                 W1
         end,
    Proc5 = spawn_opt(fun () ->
                              receive Go -> ok end,
                              1 = erlang:system_info(scheduler_id),
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}}
                      end, [link,{scheduler,1}]),
    receive after 100 -> ok end,
    Proc5 ! Go,
    wait_procs_exit([W5, Proc5]),
    wait_command_msgs(Port2, 10),
    [{Port2, Sprt5}, {Proc5, Sproc5}] = count_pp_sched_stop([Port2, Proc5]),
    ?IN_RANGE(2, Sproc5, 3),
    ?IN_RANGE(6, Sprt5, 20),

    count_pp_sched_start(),
    "disabled" = port_control(Port2, $D, ""),
    W6 = case SOnl of
             1 ->
                 false;
             _ ->
                 W2= spawn_opt(fun () ->
                                       2 = erlang:system_info(scheduler_id),
                                       "sleeped" = port_control(Port2, $S, "")
                               end, [link,{scheduler,2}]),
                 receive after 100 -> ok end,
                 W2
         end,
    Proc6 = spawn_opt(fun () ->
                              receive Go -> ok end,
                              1 = erlang:system_info(scheduler_id),
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}},
                              Port2 ! {Parent, {command, ""}}
                      end, [link,{scheduler,1}]),
    receive after 100 -> ok end,
    Proc6 ! Go,
    wait_procs_exit([W6, Proc6]),
    wait_command_msgs(Port2, 10),
    [{Port2, Sprt6}, {Proc6, Sproc6}] = count_pp_sched_stop([Port2, Proc6]),
    ?IN_RANGE(2, Sproc6, 3),
    ?IN_RANGE(2, Sprt6, 6),

    process_flag(scheduler, 0),

    Port2 ! {self(), close},
    receive {Port2, closed} -> ok end,
    ok.


wait_command_msgs(_, 0) ->
    ok;
wait_command_msgs(Port, N) ->
    receive
        {Port, command} ->
            wait_command_msgs(Port, N-1)
    end.

in_range(Low, Val, High) when is_integer(Low),
                              is_integer(Val),
                              is_integer(High),
                              Low =< Val,
                              Val =< High ->
    true;
in_range(Low, Val, High) when is_integer(Low),
                              is_integer(Val),
                              is_integer(High) ->
    false.

count_pp_sched_start() ->
    erlang:trace(all, true, [running_procs, running_ports, {tracer, self()}]),
    ok.

count_pp_sched_stop(Ps) ->
    Td = erlang:trace_delivered(all),
    erlang:trace(all, false, [running_procs, running_ports, {tracer, self()}]),
    PNs = lists:map(fun (P) -> {P, 0} end, Ps),
    receive {trace_delivered, all, Td} -> ok end,
    Res = count_proc_sched(Ps, PNs),
    io:format("Scheduling counts: ~p~n", [Res]),
    erlang:display({scheduling_counts, Res}),
    Res.

do_inc_pn(_P, []) ->
    throw(undefined);
do_inc_pn(P, [{P,N}|PNs]) ->
    [{P,N+1}|PNs];
do_inc_pn(P, [PN|PNs]) ->
    [PN|do_inc_pn(P, PNs)].

inc_pn(P, PNs) ->
    try
        do_inc_pn(P, PNs)
    catch
        throw:undefined -> PNs
    end.

count_proc_sched(Ps, PNs) ->
    receive
        TT when element(1, TT) == trace, element(3, TT) == in ->
            %	    erlang:display(TT),
            count_proc_sched(Ps, inc_pn(element(2, TT), PNs));
        TT when element(1, TT) == trace, element(3, TT) == out ->
            count_proc_sched(Ps, PNs)
    after 0 ->
              PNs
    end.

%%
%% Tests whether erl_drv_putenv reflects in os:getenv and vice versa.
%%
env(Config) when is_list(Config) ->
    ok = load_driver(proplists:get_value(data_dir, Config), env_drv),
    Port = open_port({spawn_driver, env_drv}, []),
    true = is_port(Port),

    Keys = ["env_drv_a_key", "env_drv_b_key", "env_drv_c_key"],
    Values = ["a_value", "b_value", "c_value"],

    [env_put_test(Port, Key, Value) || Key <- Keys, Value <- Values],
    [env_get_test(Port, Key, Value) || Key <- Keys, Value <- Values],
    [env_oversize_test(Port, Key) || Key <- Keys],
    [env_notfound_test(Port, Key) || Key <- Keys],

    true = port_close(Port),
    erl_ddll:unload_driver(env_drv),
    ok.

env_control(Port, Command, Key, Value) ->
    KeyBin = list_to_binary(Key),
    ValueBin = list_to_binary(Value),
    Header = <<(byte_size(KeyBin)), (byte_size(ValueBin))>>,
    Payload = <<KeyBin/binary, ValueBin/binary>>,
    port_control(Port, Command, <<Header/binary, Payload/binary>>).

env_put_test(Port, Key, Value) ->
    os:unsetenv(Key),
    [0] = env_control(Port, 0, Key, Value),
    Value = os:getenv(Key).

env_get_test(Port, Key, ExpectedValue) ->
    true = os:putenv(Key, ExpectedValue),
    [0] = env_control(Port, 1, Key, ExpectedValue).

env_oversize_test(Port, Key) ->
    os:putenv(Key, [$A || _ <- lists:seq(1, 1024)]),
    [127] = env_control(Port, 1, Key, "").

env_notfound_test(Port, Key) ->
    true = os:unsetenv(Key),
    [255] = env_control(Port, 1, Key, "").


a_test(Config) when is_list(Config) ->
    rpc(Config, fun check_io_debug/0).

z_test(Config) when is_list(Config) ->
    rpc(Config, fun check_io_debug/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 		Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_io_debug() ->
    get_stable_check_io_info(),
    {NoErrorFds, NoUsedFds, NoDrvSelStructs, NoEnifSelStructs}
        = CheckIoDebug = erts_debug:get_internal_state(check_io_debug),
    HasGetHost = has_gethost(),
    ct:log("check_io_debug: ~p~n"
           "HasGetHost: ~p",[CheckIoDebug, HasGetHost]),
    0 = NoErrorFds,
    if
        NoUsedFds == NoDrvSelStructs ->
            ok;
        HasGetHost andalso (NoUsedFds == (NoDrvSelStructs - 1)) ->
            %% If the inet_gethost port is alive, we may have
            %% one extra used fd that is not selected on
            ok
    end,
    0 = NoEnifSelStructs,
    ok.

has_gethost() ->
    has_gethost(erlang:ports()).
has_gethost([P|T]) ->
    case erlang:port_info(P, name) of
        {name,"inet_gethost"++_} ->
            true;
        _ ->
            has_gethost(T)
    end;
has_gethost([]) ->
    false.

%flush_msgs() ->
%    receive
%	M ->
%	    erlang:display(M),
%	    flush_msgs()
%    after 0 ->
%	    ok
%    end.

wait_procs_exit([]) ->
    ok;
wait_procs_exit([P|Ps]) when is_pid(P) ->
    Mon = erlang:monitor(process, P),
    receive
        {'DOWN', Mon, process, P, _} ->
            wait_procs_exit(Ps)
    end;
wait_procs_exit([_|Ps]) ->
    wait_procs_exit(Ps).

get_port_msg(Port, Timeout) ->
    receive
        {Port, What} ->
            {msg, What}
    after Timeout ->
              no_msg
    end.

wait_until(Fun) ->
    case Fun() of
        true -> ok;
        false ->
            receive after 100 -> ok end,
            wait_until(Fun)
    end.

drv_vsn_str2tup(Str) ->
    [Major, Minor] = string:lexemes(Str, "."),
    {list_to_integer(Major), list_to_integer(Minor)}.

%% Build port data from a template.

build_data({bin,Size})  -> build_binary(Size);
build_data({list,Size}) -> build_list(Size);
build_data(int) -> random_char();
build_data([])  -> [];
build_data([H|T]) -> [build_data(H)|build_data(T)].

%% Transform all binaries in a term.

transform_bins(_Transform, []) -> [];
transform_bins(Transform, [H|T]) ->
    [transform_bins(Transform, H)|transform_bins(Transform, T)];
transform_bins(Transform, Tuple) when is_tuple(Tuple) ->
    list_to_tuple([transform_bins(Transform, E) || E <- tuple_to_list(Tuple)]);
transform_bins(Transform, Bin) when is_binary(Bin) ->
    Transform(Bin);
transform_bins(_Transform, Other) -> Other.


%% Convert all binaries in a term to sub binaries.

make_sub_binaries(Term) ->
    MakeSub = fun(Bin0) ->
                      Bin1 = <<243:8,0:3,Bin0/binary,31:5,19:8>>,
                      Sz = size(Bin0),
                      <<243:8,0:3,Bin:Sz/binary,31:5,19:8>> = id(Bin1),
                      Bin
              end,
    transform_bins(MakeSub, Term).

id(I) -> I.

%% Convert all binaries in a term to refc binaries.

make_refc_binaries(Term) ->
    F = fun(B0) -> list_to_binary([build_binary(?heap_binary_size+1),B0]) end,
    transform_bins(F, Term).

build_binary(Elements) ->
    list_to_binary(build_list(Elements)).

build_list(Elements) -> build_list(Elements, []).

build_list(0, Acc) -> Acc;
build_list(Elements, Acc) -> build_list(Elements-1, [random_char()|Acc]).


%% Convert all binaries in a list to writable binaries.

make_writable_binaries(Term) ->
    transform_bins(fun(Bin) -> <<Bin/binary,1,2,3>> end, Term).

append_to_writable_binaries(Term) ->
    transform_bins(fun(Bin) -> <<Bin/binary,0:(64*1024*8)>> end, Term).

random_char() ->
    uniform(256) - 1.

uniform(N) ->
    rand:uniform(N).

erl_millisecs() ->
    erl_millisecs(erlang:monotonic_time()).

erl_millisecs(MonotonicTime) ->
    (1000*MonotonicTime)/erlang:convert_time_unit(1,second,native).

%% Start/stop drivers.
start_driver(Config, Name, Binary) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:start(),

    %% Load the driver
    ok = load_driver(Path, Name),

    %% open port.
    case Binary of
        true ->
            open_port({spawn, Name}, [binary]);
        false ->
            open_port({spawn, Name}, [])
    end.

stop_driver(Port, Name) ->
    true = erlang:port_close(Port),
    receive
        {Port,Message} ->
            ct:fail({strange_message_from_port,Message})
    after 0 ->
              ok
    end,

    %% Unload the driver.
    ok = erl_ddll:unload_driver(Name),
    ok = erl_ddll:stop().

load_driver(Dir, Driver) ->
    Before = erlang:system_info(taints),
    case erl_ddll:load_driver(Dir, Driver) of
        ok ->
            After = erlang:system_info(taints),
            case lists:member(Driver, Before) of
                true ->
                    After = Before;
                false ->
                    true = lists:member(Driver, After),
                    Before = lists:delete(Driver, After)
            end,
            ok;
        {error, Error} = Res ->
            io:format("~s\n", [erl_ddll:format_error(Error)]),
            Res
    end.

sleep() ->
    receive after infinity -> ok end.

sleep(infinity) ->
    sleep();
sleep(Ms) when is_integer(Ms), Ms >= 0 ->
    receive after Ms -> ok end.


start_node(Config) when is_list(Config) ->
    start_node(proplists:get_value(testcase, Config));
start_node(Name) ->
    start_node(Name, "").
start_node(NodeName, Args) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
                        ++ "-"
                        ++ atom_to_list(NodeName)
                        ++ "-"
                        ++ integer_to_list(erlang:system_time(second))
                        ++ "-"
                        ++ integer_to_list(erlang:unique_integer([positive]))),
    test_server:start_node(Name, slave, [{args, Args ++ " -pa "++Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).

wait_deallocations() ->
    try
        erts_debug:set_internal_state(wait, deallocations)
    catch error:undef ->
              erts_debug:set_internal_state(available_internal_state, true),
              wait_deallocations()
    end.

driver_alloc_size() ->
    wait_deallocations(),
    case erlang:system_info({allocator_sizes, driver_alloc}) of
        false ->
            undefined;
        MemInfo ->
            CS = lists:foldl(
                   fun ({instance, _, L}, Acc) ->
                           {value,{_,MBCS}} = lists:keysearch(mbcs, 1, L),
                           {value,{_,SBCS}} = lists:keysearch(sbcs, 1, L),
                           [MBCS,SBCS | Acc]
                   end,
                   [],
                   MemInfo),
            lists:foldl(
              fun(L, Sz0) ->
                      {value,{_,Sz,_,_}} = lists:keysearch(blocks_size, 1, L),
                      Sz0+Sz
              end, 0, CS)
    end.

rpc(Config, Fun) ->
    case proplists:get_value(node, Config) of
        undefined ->
            Fun();
        Node ->
            Self = self(),
            Ref = make_ref(),
            Pid = spawn(Node,
                        fun() ->
                                Result
                                    = try Fun() of
                                          Res -> Res
                                      catch E:R:Stk ->
                                              {'EXIT',E,R,Stk}
                                      end,
                                Self ! {Ref, Result}
                        end),
            MRef = monitor(process, Pid),
            receive
                {'DOWN', MRef, _Type, _Object, Info} ->
                    erlang:error({died, Pid, Info});
                {Ref, {'EXIT',E,R,ST}} ->
                    erlang:demonitor(MRef, [flush]),
                    erlang:raise(E,R,ST);
                {Ref, Ret} ->
                    erlang:demonitor(MRef, [flush]),
                    Ret;
                Other ->
                    ct:fail(Other)
            end
    end.
