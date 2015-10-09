%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2014. All Rights Reserved.
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
	 use_fallback_pollset/1,
	 bad_fd_in_pollset/1,
	 driver_event/1,
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
	 ioq_exit_event/1,
	 ioq_exit_ready_input_async/1,
	 ioq_exit_ready_output_async/1,
	 ioq_exit_timeout_async/1,
	 ioq_exit_event_async/1,
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
	 z_test/1]).

-export([bin_prefix/2]).

-include_lib("test_server/include/test_server.hrl").


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
-define(delay, 100).

-define(heap_binary_size, 64).

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(2)),
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> ok;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end,
    erlang:display({init_per_testcase, Case}),
    ?line 0 = element(1, erts_debug:get_internal_state(check_io_debug)),
    [{watchdog, Dog},{testcase, Case}|Config].

end_per_testcase(Case, Config) ->
    Dog = ?config(watchdog, Config),
    erlang:display({end_per_testcase, Case}),
    ?line 0 = element(1, erts_debug:get_internal_state(check_io_debug)),
    ?t:timetrap_cancel(Dog).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> %% Keep a_test first and z_test last...
    [a_test, outputv_errors, outputv_echo, queue_echo, {group, timer},
     driver_unloaded, io_ready_exit, use_fallback_pollset,
     bad_fd_in_pollset, driver_event, fd_change,
     steal_control, otp_6602, driver_system_info_base_ver,
     driver_system_info_prev_ver,
     driver_system_info_current_ver, driver_monitor,
     {group, ioq_exit}, zero_extended_marker_garb_drv,
     invalid_extended_marker_drv, larger_major_vsn_drv,
     larger_minor_vsn_drv, smaller_major_vsn_drv,
     smaller_minor_vsn_drv, peek_non_existing_queue,
     otp_6879, caller, many_events, missing_callbacks,
     smp_select, driver_select_use,
     thread_mseg_alloc_cache_clean,
     otp_9302,
     thr_free_drv,
     async_blast,
     thr_msg_blast,
     consume_timeslice,
     z_test].

groups() -> 
    [{timer, [],
      [timer_measure, timer_cancel, timer_delay,
       timer_change]},
     {ioq_exit, [],
      [ioq_exit_ready_input, ioq_exit_ready_output,
       ioq_exit_timeout, ioq_exit_ready_async, ioq_exit_event,
       ioq_exit_ready_input_async, ioq_exit_ready_output_async,
       ioq_exit_timeout_async, ioq_exit_event_async]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

outputv_errors(doc) -> "Test sending bad types to port with an outputv-capable driver.";
outputv_errors(Config) when is_list(Config) ->
    ?line Path = ?config(data_dir, Config),
    ?line erl_ddll:start(),
    ?line ok = load_driver(Path, outputv_drv),

    outputv_bad_types(fun(T) ->
			      ?line outputv_errors_1(T),
			      ?line outputv_errors_1([1|T]),
			      ?line L = [1,2,3],
			      ?line outputv_errors_1([L,T]),
			      ?line outputv_errors_1([L|T])
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
    ?line Sizes = [FourGigs+N || N <- lists:seq(0, 64)] ++
	[1 bsl N || N <- lists:seq(33, 37)],
    ?line Base = <<0:(1 bsl 20)/unit:8>>,
    [begin
	 ?line L = build_iolist(Sz, Base),
	 ?line outputv_errors_1(L)
     end || Sz <- Sizes],
    ok.

outputv_errors_1(Term) ->
    Port = open_port({spawn_driver,outputv_drv}, []),
    {'EXIT',{badarg,_}} = (catch port_command(Port, Term)),
    port_close(Port).

build_iolist(N, Base) when N < 16 ->
    case random:uniform(3) of
	1 ->
	    <<Bin:N/binary,_/binary>> = Base,
	    Bin;
	_ ->
	    lists:seq(1, N)
    end;
build_iolist(N, Base) when N =< byte_size(Base) ->
    case random:uniform(3) of
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
    Small = random:uniform(15),
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

outputv_echo(doc) -> ["Test echoing data with a driver that supports outputv."];
outputv_echo(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(10)),
    Name = 'outputv_drv',
    P = start_driver(Config, Name, true),

    ?line ov_test(P, {bin,0}),
    ?line ov_test(P, {bin,1}),
    ?line ov_test(P, {bin,2}),
    ?line ov_test(P, {bin,3}),
    ?line ov_test(P, {bin,4}),
    ?line ov_test(P, {bin,5}),
    ?line ov_test(P, {bin,6}),
    ?line ov_test(P, {bin,7}),
    ?line ov_test(P, {bin,8}),
    ?line ov_test(P, {bin,15}),
    ?line ov_test(P, {bin,16}),
    ?line ov_test(P, {bin,17}),

    ?line ov_test(P, {list,0}),
    ?line ov_test(P, {list,1}),
    ?line ov_test(P, {list,2}),
    ?line ov_test(P, [int,int,{list,0},int]),
    ?line ov_test(P, [int,int,{list,1},int]),
    ?line ov_test(P, [int,int,{list,2}]),
    ?line ov_test(P, [{list,3},int,int,{list,2}]),
    ?line ov_test(P, {list,33}),

    ?line ov_test(P, [{bin,0}]),
    ?line ov_test(P, [{bin,1}]),
    ?line ov_test(P, [{bin,2}]),
    ?line ov_test(P, [{bin,3}]),
    ?line ov_test(P, [{bin,4}]),
    ?line ov_test(P, [{bin,5}]),
    ?line ov_test(P, [{bin,6},int]),
    ?line ov_test(P, [int,{bin,3}]),
    ?line ov_test(P, [int|{bin,4}]),
    ?line ov_test(P, [{bin,17},int,{bin,13}|{bin,3}]),

    ?line ov_test(P, [int,{bin,17},int,{bin,?heap_binary_size+1}|{bin,3}]),

    stop_driver(P, Name),
    ?line test_server:timetrap_cancel(Dog),
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
	    io:format("~p returned WRONG data ~p", [Port,OtherData]),
	    ?line test_server:fail();
	Wrong ->
	    ?line test_server:fail({unexpected_port_or_data,Wrong})
    end.

compare(Got, Expected) ->
    case {list_to_binary([Got]),list_to_binary([Expected])} of
	{B,B} -> ok;
	{_Gb,_Eb} ->
	    ?t:fail(got_bad_data)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 		Driver timer test suites
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


timer_measure(doc) -> ["Check that timers time out in good time."];
timer_measure(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(1)),
    Name = 'timer_drv',
    ?line Port = start_driver(Config, Name, false),

    ?line try_timeouts(Port, 8997),

    ?line stop_driver(Port, Name),
    ?line test_server:timetrap_cancel(Dog),
    ok.

try_timeouts(_, 0) -> ok;
try_timeouts(Port, Timeout) ->
    ?line TimeBefore = erlang:monotonic_time(),
    ?line erlang:port_command(Port, <<?START_TIMER,Timeout:32>>),
    receive
	{Port,{data,[?TIMER]}} ->
	    ?line Elapsed = erl_millisecs() - erl_millisecs(TimeBefore),
	    io:format("Elapsed: ~p Timeout: ~p\n", [Elapsed, Timeout]),
	    if
		Elapsed < Timeout ->
		    ?line ?t:fail(too_short);
		Elapsed > Timeout + ?delay ->
		    ?line ?t:fail(too_long);
		true ->
		    try_timeouts(Port, Timeout div 2)
	    end
    after Timeout + ?delay ->
	    ?line test_server:fail("driver failed to timeout")
    end.

timer_cancel(doc) -> ["Try cancelling timers set in a driver."];
timer_cancel(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(1)),
    Name = 'timer_drv',
    ?line Port = start_driver(Config, Name, false),

    ?line try_cancel(Port, 10000),

    ?line stop_driver(Port, Name),
    ?line test_server:timetrap_cancel(Dog),
    ok.
    
try_cancel(Port, Timeout) ->
    ?line T_before = erl_millisecs(),
    Port ! {self(),{command,<<?START_TIMER,(Timeout + ?delay):32>>}},
    receive
	{Port, {data, [?TIMER]}} ->
	    ?line test_server:fail("driver timed out before cancelling it")
    after Timeout -> 
	    Port ! {self(), {command, [?CANCEL_TIMER]}},
	    receive 
		{Port, {data, [?TIMER]}} ->
		    ?line test_server:fail("driver timed out after cancelling it");
		{Port, {data, [?CANCELLED]}} ->
		    ?line Time_milli_secs = erl_millisecs() - T_before,

		    io:format("Time_milli_secs: ~p Timeout: ~p\n",
			      [Time_milli_secs, Timeout]), 
		    if
			Time_milli_secs > (Timeout + ?delay) ->
			    ?line test_server:fail("too long real time");
			Timeout == 0 -> ok;
			true -> try_cancel(Port, Timeout div 2)
		    end
	    after ?delay ->
		    test_server:fail("No message from driver")
	    end
    end.

%% Test that timers don't time out too early if we do a sleep
%% before setting a timer.

timer_delay(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(1)),
    Name = 'timer_drv',
    ?line Port = start_driver(Config, Name, false),

    ?line TimeBefore = erlang:monotonic_time(),
    Timeout0 = 350,
    ?line erlang:port_command(Port, <<?DELAY_START_TIMER,Timeout0:32>>),
    Timeout = Timeout0 +
	case os:type() of
	    {win32,_} -> 0;			%Driver doesn't sleep on Windows.
	    _ -> 1000
	end,
    receive
	{Port,{data,[?TIMER]}} ->
	    ?line Elapsed = erl_millisecs() - erl_millisecs(TimeBefore),
	    io:format("Elapsed time: ~p Timeout: ~p\n",
		      [Elapsed,Timeout]), 
	    if
		Elapsed < Timeout ->
		    ?line ?t:fail(too_short);
		Elapsed > Timeout + ?delay ->
		    ?line ?t:fail(too_long);
		true ->
		    ok
	    end
    end,

    ?line stop_driver(Port, Name),
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Test that driver_set_timer with new timout really changes
%% the timer (ticket OTP-5942), it didn't work before

timer_change(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(1)),
    Name = 'timer_drv',
    ?line Port = start_driver(Config, Name, false),

    ?line try_change_timer(Port, 10000),

    ?line stop_driver(Port, Name),
    ?line test_server:timetrap_cancel(Dog),
    ok.
    
try_change_timer(_Port, 0) -> ok;
try_change_timer(Port, Timeout) ->
    ?line Timeout_3 = Timeout*3,
    ?line TimeBefore = erlang:monotonic_time(),
    ?line erlang:port_command(Port, <<?START_TIMER,Timeout_3:32>>),
    ?line erlang:port_command(Port, <<?START_TIMER,Timeout:32>>),
    receive
	{Port,{data,[?TIMER]}} ->
	    ?line Elapsed = erl_millisecs() - erl_millisecs(TimeBefore),
	    io:format("Elapsed: ~p Timeout: ~p\n", [Elapsed,Timeout]),
	    if
		Elapsed < Timeout ->
		    ?line ?t:fail(too_short);
		Elapsed > Timeout + ?delay ->
		    ?line ?t:fail(too_long);
		true ->
		    try_timeouts(Port, Timeout div 2)
	    end
    after Timeout + ?delay ->
	    ?line test_server:fail("driver failed to timeout")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 		Queue test suites
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

queue_echo(doc) ->
    ["1) Queue up data in a driver that uses the full driver_queue API to do this."
     "2) Get the data back, a random amount at a time."];
queue_echo(Config) when is_list(Config) ->
    case ?t:is_native(?MODULE) of
	true -> exit(crashes_native_code);
	false -> queue_echo_1(Config)
    end.

queue_echo_1(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(10)),
    Name = 'queue_drv',
    ?line P = start_driver(Config, Name, true),

    ?line q_echo(P, [{?ENQ, {list,1}},
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

    ?line stop_driver(P, Name),
    ?line test_server:timetrap_cancel(Dog),
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
	    io:format("Qb_before: ~p\n"
		      "Qb_before+Size: ~p\n"
		      "Qb_in_driver: ~p",
		      [Qb_before,Sum,Qb_in_driver]),
	    ?t:fail()
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
	    io:format("Len_to_get: ~p", [Len_to_get]),
	    io:format("Bytes in queue: ~p", [BytesInQueue]),
	    ?line test_server:fail()
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
	    io:format("Bytes to dequeue: ~p", [BytesToDequeue]),
	    io:format("Dequeued: ~p", [Dequeued]),
	    io:format("Queued in port: ~P", [QueuedInPort0,12]),
	    ?t:fail()
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
	Bad -> ?t:fail({bad_result,Bad})
    end.

deq(Port, Size) ->
    [] = erlang:port_control(Port, ?DEQ, <<Size:32>>).

read_head(Port, Size) ->
    erlang:port_control(Port, ?READ_HEAD, <<Size:32>>).


driver_unloaded(doc) ->
    [];
driver_unloaded(suite) ->
    [];
driver_unloaded(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Drv = timer_drv,
    ?line User = self(),
    ?line Loaded = make_ref(),
    ?line Die = make_ref(),
    ?line Loader = spawn(fun () ->
				 erl_ddll:start(),
				 ok = load_driver(?config(data_dir,
								   Config),
							   Drv),
				 User ! Loaded,
				 receive Die -> exit(bye) end
			 end),
    ?line receive Loaded -> ok end,
    ?line Port = open_port({spawn, Drv}, []),
    ?line Loader ! Die,
    ?line receive
	      {'EXIT', Port, Reason} ->
		  ?line driver_unloaded = Reason
		  %% Reason used to be -1
	  end.
					 

io_ready_exit(doc) -> [];
io_ready_exit(suite) -> [];
io_ready_exit(Config) when is_list(Config) ->
    ?line OTE = process_flag(trap_exit, true),
    ?line Test = self(),
    ?line Dgawd = spawn(fun () ->
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
    ?line receive dgawd_handler_started -> ok end,
    ?line Drv = io_ready_exit_drv,
    ?line erl_ddll:start(),
    ?line ok = load_driver(?config(data_dir, Config), Drv),
    ?line Port = open_port({spawn, Drv}, []),
    ?line case erlang:port_control(Port, 0, "") of
	      "ok" ->
		  receive
		      {'EXIT', Port, Reason} ->
			  ?line case Reason of
				    ready_output_driver_failure ->
					?t:format("Exited in output_ready()~n"),
					?line ok;
				    ready_input_driver_failure ->
					?t:format("Exited in input_ready()~n"),
					?line ok;
				    Error -> ?line ?t:fail(Error)
				end
		  end,
		  receive after 2000 -> ok end,
		  ?line false = dgawd_handler:got_dgawd_report(),
		  ?line Dgawd ! stop_dgawd_handler,
		  ?line receive dgawd_handler_stopped -> ok end,
		  ?line process_flag(trap_exit, OTE),
		  ?line ok;
	      "nyiftos" ->
		  ?line process_flag(trap_exit, OTE),
		  ?line {skipped, "Not yet implemented for this OS"};
	      Error ->
		  ?line process_flag(trap_exit, OTE),
		  ?line ?t:fail({unexpected_control_result, Error})
	  end.
    

-define(CHKIO_STOP, 0).
-define(CHKIO_USE_FALLBACK_POLLSET, 1).
-define(CHKIO_BAD_FD_IN_POLLSET, 2).
-define(CHKIO_DRIVER_EVENT, 3).
-define(CHKIO_FD_CHANGE, 4).
-define(CHKIO_STEAL, 5).
-define(CHKIO_STEAL_AUX, 6).
-define(CHKIO_SMP_SELECT, 7).
-define(CHKIO_DRV_USE, 8).

use_fallback_pollset(doc) -> [];
use_fallback_pollset(suite) -> [];
use_fallback_pollset(Config) when is_list(Config) ->
    FlbkFun = fun () ->
		      ChkIoDuring = erlang:system_info(check_io),
		      case lists:keysearch(fallback_poll_set_size,
					   1,
					   ChkIoDuring) of
			  {value,
			   {fallback_poll_set_size, N}} when N > 0 ->
			      ?line ok;
			  Error ->
			      ?line ?t:fail({failed_to_use_fallback, Error})
		      end
	      end,
    ?line {BckupTest, Handel, OkRes}
	= case chkio_test_init(Config) of
	      {erts_poll_info, ChkIo} = Hndl ->
		  case lists:keysearch(fallback, 1, ChkIo) of
		      {value, {fallback, B}} when B =/= false ->
			  ?line {FlbkFun, Hndl, ok};
		      _ ->
			  ?line {fun () -> ok end,
				 Hndl,
				 {comment,
				  "This implementation does not use "
				  "a fallback pollset"}}
		  end;
	      Skip ->
		  {fun () -> ok end, Skip, ok}
	  end,
    ?line case chkio_test_fini(chkio_test(Handel,
					  ?CHKIO_USE_FALLBACK_POLLSET,
					  fun () ->
						  ?line sleep(1000),
						  ?line BckupTest()
					  end)) of
	      {skipped, _} = Res -> ?line Res;
	      _ -> ?line OkRes
	  end.

bad_fd_in_pollset(doc) -> [];
bad_fd_in_pollset(suite) -> [];
bad_fd_in_pollset(Config) when is_list(Config) ->
    ?line chkio_test_fini(chkio_test(chkio_test_init(Config),
				     ?CHKIO_BAD_FD_IN_POLLSET,
				     fun () -> ?line sleep(1000) end)).

driver_event(doc) -> [];
driver_event(suite) -> [];
driver_event(Config) when is_list(Config) ->
    ?line chkio_test_fini(chkio_test(chkio_test_init(Config),
				     ?CHKIO_DRIVER_EVENT,
				     fun () -> ?line sleep(1000) end)).

fd_change(doc) -> [];
fd_change(suite) -> [];
fd_change(Config) when is_list(Config) ->
    ?line chkio_test_fini(chkio_test(chkio_test_init(Config),
				     ?CHKIO_FD_CHANGE,
				     fun () -> ?line sleep(1000) end)).

steal_control(doc) -> [];
steal_control(suite) -> [];
steal_control(Config) when is_list(Config) ->
    ?line chkio_test_fini(case chkio_test_init(Config) of
			      {erts_poll_info, _} = Hndl ->
				  ?line steal_control_test(Hndl);
			      Skip ->
				  ?line Skip
			  end).

steal_control_test(Hndl = {erts_poll_info, Before}) ->
    ?line Port = open_chkio_port(),
    ?line case erlang:port_control(Port, ?CHKIO_STEAL_AUX, "") of
	      [$f,$d,$s,$:| _] = FdList ->
		  ?line chk_chkio_port(Port),
		  sleep(500),
		  ?line chk_chkio_port(Port),
		  ?line Res = chkio_test(Hndl,
					 ?CHKIO_STEAL,
					 FdList,
					 fun () ->
						 ?line chk_chkio_port(Port),
						 ?line sleep(500),
						 ?line chk_chkio_port(Port)
					 end),
		  ?line case erlang:port_control(Port, ?CHKIO_STOP, "") of
			    "ok" ->
				?line chk_chkio_port(Port),
				?line ok;
			    StopErr ->
				?line chk_chkio_port(Port),
				?line ?t:fail({stop_error, StopErr})
			end,
		  ?line close_chkio_port(Port),
		  ?line Res;
	      [$s,$k,$i,$p,$:,$\ |Skip] ->
		  ?line chk_chkio_port(Port),
		  ?line close_chkio_port(Port),
		  {chkio_test_result,
		   {skipped, Skip},
		   Before};
	      StartErr ->
		  ?line chk_chkio_port(Port),
		  ?line ?t:fail({start_error, StartErr})
	  end.

chkio_test_init(Config) when is_list(Config) ->
    ?line ChkIo = get_stable_check_io_info(),
    ?line case catch lists:keysearch(name, 1, ChkIo) of
	      {value, {name, erts_poll}} ->
		  ?line ?t:format("Before test: ~p~n", [ChkIo]),
		  ?line Path = ?config(data_dir, Config),
		  ?line erl_ddll:start(),
		  ?line ok = load_driver(Path, 'chkio_drv'),
		  ?line process_flag(trap_exit, true),
		  ?line {erts_poll_info, ChkIo};
	      _ ->
		  ?line {skipped, "Test written to test erts_poll() which isn't used"}
	  end.
		  

chkio_test_fini({skipped, _} = Res) ->
    Res;
chkio_test_fini({chkio_test_result, Res, Before}) ->
    ?line ok = erl_ddll:unload_driver('chkio_drv'),
    ?line ok = erl_ddll:stop(),
    ?line After = get_stable_check_io_info(),
    ?line ?t:format("After test: ~p~n", [After]),
    ?line verify_chkio_state(Before, After),
    ?line Res.

open_chkio_port() ->
    open_port({spawn, 'chkio_drv'}, []).

close_chkio_port(Port) when is_port(Port) ->
    true = erlang:port_close(Port),
    receive
	{'EXIT', Port, normal} ->
	    ok;
	{'EXIT', Port, Reason} ->
	    ?t:fail({abnormal_port_exit, Port, Reason});
	{Port, Message} ->
	    ?t:fail({strange_message_from_port, Message})
    end.

chk_chkio_port(Port) ->
    receive
	{'EXIT', Port, Reason} when Reason /= normal ->
	    ?t:fail({port_exited, Port, Reason})
    after 0 ->
	    ok
    end.
	    

chkio_test({skipped, _} = Res, _Test, _Fun) ->
    ?line Res;
chkio_test({erts_poll_info, _Before} = EPI, Test, Fun) when is_integer(Test) ->
    chkio_test(EPI, Test, "", Fun).

chkio_test({skipped, _} = Res, _Test, _TestArgs, _Fun) ->
    ?line Res;
chkio_test({erts_poll_info, Before},
	   Test,
	   TestArgs,
	   Fun) when is_integer(Test),
		     is_list(TestArgs) ->
    ?line Port = open_chkio_port(),
    ?line case erlang:port_control(Port, Test, TestArgs) of
	      "ok" ->
		  ?line chk_chkio_port(Port),
		  ?line Fun(),
		  ?line During = erlang:system_info(check_io),
		  ?line erlang:display(During),
		  ?line 0 = element(1, erts_debug:get_internal_state(check_io_debug)),
		  ?line ?t:format("During test: ~p~n", [During]),
		  ?line chk_chkio_port(Port),
		  ?line case erlang:port_control(Port, ?CHKIO_STOP, "") of
			    Res when is_list(Res) ->
				?line chk_chkio_port(Port),
				?line ?t:format("~s", [Res]),
				?line close_chkio_port(Port),
				?line Res,
				?line case Res of
					  [$c,$o,$m,$m,$e,$n,$t,$:,$\ |Cmnt] ->
					      ?line {chkio_test_result,
						     {comment, Cmnt},
						     Before};
					  _ ->
					      ?line {chkio_test_result,
						     Res,
						     Before}
				      end;
			    StopErr ->
				?line chk_chkio_port(Port),
				?line ?t:fail({stop_error, StopErr})
			end;
	      [$s,$k,$i,$p,$:,$\ |Skip] ->
		  ?line chk_chkio_port(Port),
		  ?line close_chkio_port(Port),
		  {chkio_test_result,
		   {skipped, Skip},
		   Before};
	      StartErr ->
		  ?line chk_chkio_port(Port),
		  ?line ?t:fail({start_error, StartErr})
	  end.

verify_chkio_state(Before, After) ->
    ?line TotSetSize = lists:keysearch(total_poll_set_size, 1, Before),
    ?line TotSetSize = lists:keysearch(total_poll_set_size, 1, After),
    ?line case lists:keysearch(fallback, 1, Before) of
	      {value,{fallback,false}} ->
		  ?line ok;
	      _ ->
		  ?line BckupSetSize = lists:keysearch(fallback_poll_set_size,
						       1,
						       Before),
		  ?line BckupSetSize = lists:keysearch(fallback_poll_set_size,
						       1,
						       After)
	  end,
    ?line ok.

get_stable_check_io_info() ->
    ChkIo = erlang:system_info(check_io),
    PendUpdNo = case lists:keysearch(pending_updates, 1, ChkIo) of
		    {value, {pending_updates, PendNo}} ->
			PendNo;
		    false ->
			0
		end,
    {value, {active_fds, ActFds}} = lists:keysearch(active_fds, 1, ChkIo),
    case {PendUpdNo, ActFds} of
	{0, 0} ->
	    ChkIo;
	_ ->
	    receive after 10 -> ok end,
	    get_stable_check_io_info()
    end.

otp_6602(doc) -> ["Missed port lock when stealing control of fd from a "
		  "driver that didn't use the same lock. The lock checker "
		  "used to trigger on this and dump core."];
otp_6602(suite) ->
    [];
otp_6602(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(Config),
    ?line Done = make_ref(),
    ?line Parent = self(),
    ?line Tester = spawn_link(Node,
			      fun () ->
				      %% Inet driver use port locking...
				      {ok, S} = gen_udp:open(0),
				      {ok, Fd} = inet:getfd(S),
				      %% Steal fd (lock checker used to
				      %% trigger here).
				      {ok, _S2} = gen_udp:open(0,[{fd,Fd}]),
				      Parent ! Done
			      end),
    ?line receive Done -> ok end,
    ?line unlink(Tester),
    ?line stop_node(Node),
    ?line ok.

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

'driver_system_info_base_ver'(doc) ->
    [];
'driver_system_info_base_ver'(suite) ->
    [];
'driver_system_info_base_ver'(Config) when is_list(Config) ->
    ?line driver_system_info_test(Config, sys_info_base_drv).

'driver_system_info_prev_ver'(doc) ->
    [];
'driver_system_info_prev_ver'(suite) ->
    [];
'driver_system_info_prev_ver'(Config) when is_list(Config) ->
    ?line driver_system_info_test(Config, sys_info_prev_drv).

driver_system_info_current_ver(doc) ->    
    [];
driver_system_info_current_ver(suite) ->
    [];
driver_system_info_current_ver(Config) when is_list(Config) ->
    ?line driver_system_info_test(Config, sys_info_curr_drv).

driver_system_info_test(Config, Name) ->
    ?line Port = start_driver(Config, Name, false),
    ?line case erlang:port_control(Port, 0, []) of
	      [$o,$k,$:,_ | Result] ->
		  ?line check_driver_system_info_result(Result);
	      [$e,$r,$r,$o,$r,$:,_ | Error] ->
		  ?line ?t:fail(Error);
	      Unexpected ->
		  ?line ?t:fail({unexpected_result, Unexpected})
	  end,
    ?line stop_driver(Port, Name),
    ?line ok.

check_driver_system_info_result(Result) ->
    ?line ?t:format("All names: ~p~n", [?EXPECTED_SYSTEM_INFO_NAMES]),
    ?line ?t:format("Result: ~p~n", [Result]),
    ?line {[], Ns, DDVSN} = chk_sis(lists:map(fun (Str) ->
						      string:tokens(Str, "=")
					      end,
					      string:tokens(Result, " ")),
				    ?EXPECTED_SYSTEM_INFO_NAMES),
    ?line case {DDVSN,
		drv_vsn_str2tup(erlang:system_info(driver_version))} of
	      {DDVSN, DDVSN} ->
		  ?line [] = Ns;
	      %% {{1, 0}, _} ->
	      %% 	  ?line ExpNs = lists:sort(?EXPECTED_SYSTEM_INFO_NAMES
	      %% 				   -- ?EXPECTED_SYSTEM_INFO_NAMES1),
	      %% 	  ?line ExpNs = lists:sort(Ns);
	      %% {{1, 1}, _} ->
	      %% 	  ?line ExpNs = lists:sort(?EXPECTED_SYSTEM_INFO_NAMES
	      %% 				   -- ?EXPECTED_SYSTEM_INFO_NAMES2),
	      %% 	  ?line ExpNs = lists:sort(Ns);
	      {{3, 0}, _} ->
		  ?line ExpNs = lists:sort(?EXPECTED_SYSTEM_INFO_NAMES
					   -- ?EXPECTED_SYSTEM_INFO_NAMES3),
		  ?line ExpNs = lists:sort(Ns)
	  end.

chk_sis(SIs, Ns) ->
    chk_sis(SIs, Ns, unknown).

chk_sis(SIs, [], DDVSN) ->
    ?line {SIs, [], DDVSN};
chk_sis([], Ns, DDVSN) ->
    ?line {[], Ns, DDVSN};
chk_sis([[N, _] = SI| SIs], Ns, DDVSN) ->
    ?line true = lists:member(N, Ns),
    ?line case check_si_res(SI) of
	      {driver_version, NewDDVSN} ->
		  ?line chk_sis(SIs, lists:delete(N, Ns), NewDDVSN);
	      _ ->
		  ?line chk_sis(SIs, lists:delete(N, Ns), DDVSN)
	  end.

%% Data in first version of driver_system_info() (driver version 1.0)
check_si_res(["drv_drv_vsn", Value]) ->
    ?line DDVSN = drv_vsn_str2tup(Value),
    ?line {Major, DMinor} = DDVSN,
    ?line {Major, EMinor} = drv_vsn_str2tup(erlang:system_info(driver_version)),
    ?line true = DMinor =< EMinor,
    ?line {driver_version, DDVSN};
check_si_res(["emu_drv_vsn", Value]) ->
    ?line Value = erlang:system_info(driver_version);
check_si_res(["erts_vsn", Value]) ->
    ?line Value = erlang:system_info(version);
check_si_res(["otp_vsn", Value]) ->
    ?line Value = erlang:system_info(otp_release);
check_si_res(["thread", "true"]) ->
    ?line true = erlang:system_info(threads);
check_si_res(["thread", "false"]) ->
    ?line false = erlang:system_info(threads);
check_si_res(["smp", "true"]) ->
    ?line true = erlang:system_info(smp_support);
check_si_res(["smp", "false"]) ->
    ?line false = erlang:system_info(smp_support);

%% Data added in second version of driver_system_info() (driver version 1.1)
check_si_res(["async_thrs", Value]) ->
    ?line Value = integer_to_list(erlang:system_info(thread_pool_size));
check_si_res(["sched_thrs", Value]) ->
    ?line Value = integer_to_list(erlang:system_info(schedulers));

%% Data added in 3rd version of driver_system_info() (driver version 1.5)
check_si_res(["emu_nif_vsn", Value]) ->
    ?line Value = erlang:system_info(nif_version);

%% Data added in 4th version of driver_system_info() (driver version 3.1)
check_si_res(["dirty_sched", _Value]) ->
    true;

check_si_res(Unexpected) ->
    ?line ?t:fail({unexpected_result, Unexpected}).

-define(MON_OP_I_AM_IPID,1).
-define(MON_OP_MONITOR_ME,2).
-define(MON_OP_DEMONITOR_ME,3).
-define(MON_OP_MONITOR_ME_LATER,4).
-define(MON_OP_DO_DELAYED_MONITOR,5).

driver_monitor(suite) ->
    [];
driver_monitor(doc) ->    
    ["Test monitoring of processes from drivers"];
driver_monitor(Config) when is_list(Config) ->
    ?line Name = monitor_drv,
    ?line Port = start_driver(Config, Name, false),
    ?line "ok" = port_control(Port,?MON_OP_I_AM_IPID,[]),
    ?line "ok" = port_control(Port,?MON_OP_MONITOR_ME,[]),
    ?line "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    ?line {monitors, []} = erlang:port_info(Port,monitors),
    
    ?line "ok:"++Id1 = port_control(Port,?MON_OP_MONITOR_ME_LATER,[]),
    ?line {monitored_by, []} = process_info(self(),monitored_by),
    ?line "ok" = port_control(Port,?MON_OP_DO_DELAYED_MONITOR,Id1),
    ?line {monitored_by, [Port]} = process_info(self(),monitored_by),
    ?line "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    ?line {monitored_by, []} = process_info(self(),monitored_by),
    
    ?line "ok" = port_control(Port,?MON_OP_MONITOR_ME,[]),
    ?line Me = self(),
    ?line {Pid1,Ref1} =
	spawn_monitor(fun() ->
			      Me ! port_control(Port,?MON_OP_MONITOR_ME,[]), 
			      Me ! process_info(self(),monitored_by), 
			      Me ! erlang:port_info(Port,monitors) 
		      end),
    ?line ok = receive
		   "ok" ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line ok = receive
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
    ?line ok = receive
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
    ?line ok = receive
		   {'DOWN', Ref1, process, Pid1, _} ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line ok = receive
		   {monitor_fired,Port,Pid1} ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    ?line {monitors,[]} = erlang:port_info(Port,monitors),
    ?line {monitored_by, []} = process_info(self(),monitored_by),

    ?line "ok" = port_control(Port,?MON_OP_MONITOR_ME,[]),
    ?line {Pid2,Ref2} =
	spawn_monitor(fun() -> 			       
			      receive go -> ok end,
			      Me ! port_control(Port,?MON_OP_MONITOR_ME_LATER,[]), 
			      Me ! process_info(self(),monitored_by), 
			      Me ! erlang:port_info(Port,monitors) 
		      end),
    ?line Pid2 ! go,
    ?line {ok,Id2} = receive
		   "ok:"++II ->
		       {ok,II}
	       after 1000 ->
		       timeout
	       end,
    ?line ok = receive
		   {monitored_by, [Me]} ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line ok = receive
		   {monitors, [{process,Me}]} ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line ok = receive
		   {'DOWN', Ref2, process, Pid2, _} ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line "noproc" = port_control(Port,?MON_OP_DO_DELAYED_MONITOR,Id2),
    ?line {monitors,[{process,Me}]} = erlang:port_info(Port,monitors),
    ?line "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    ?line "not_monitored" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    ?line {monitors,[]} = erlang:port_info(Port,monitors),
    ?line {monitored_by, []} = process_info(self(),monitored_by),


    ?line "ok" = port_control(Port,?MON_OP_MONITOR_ME,[]),
    ?line {Pid3,Ref3} =
	spawn_monitor(fun() ->
			      receive go -> ok end,
			      Me ! port_control(Port,?MON_OP_MONITOR_ME_LATER,[]), 
			      Me ! process_info(self(),monitored_by), 
			      Me ! erlang:port_info(Port,monitors) ,
			      receive die -> ok end
		      end),
    ?line Pid3 ! go,
    ?line {ok,Id3} = receive
		   "ok:"++III ->
		       {ok,III}
	       after 1000 ->
		       timeout
	       end,
    ?line ok = receive
		   {monitored_by, [Me]} ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line ok = receive
		   {monitors, [{process,Me}]} ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line "ok" = port_control(Port,?MON_OP_DO_DELAYED_MONITOR,Id3),
    ?line LLL1 = lists:sort([{process,Me},{process,Pid3}]),
    ?line {monitors,LLL2} = erlang:port_info(Port,monitors),
    ?line LLL1 = lists:sort(LLL2),
    ?line "ok" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    ?line {monitors,[{process,Pid3}]} = erlang:port_info(Port,monitors),
    ?line Pid3 ! die,
    ?line ok = receive
		   {'DOWN', Ref3, process, Pid3, _} ->
		       ok
	       after 1000 ->
		       timeout
	       end,
    ?line "not_found" = port_control(Port,?MON_OP_DO_DELAYED_MONITOR,Id2),
    ?line {monitors,[]} = erlang:port_info(Port,monitors),
    ?line "not_monitored" = port_control(Port,?MON_OP_DEMONITOR_ME,[]),
    ?line {monitors,[]} = erlang:port_info(Port,monitors),
    ?line {monitored_by, []} = process_info(self(),monitored_by),
    
    ?line stop_driver(Port, Name),
    ?line ok.


-define(IOQ_EXIT_READY_INPUT, 1).
-define(IOQ_EXIT_READY_OUTPUT, 2).
-define(IOQ_EXIT_TIMEOUT, 3).
-define(IOQ_EXIT_READY_ASYNC, 4).
-define(IOQ_EXIT_EVENT, 5).
-define(IOQ_EXIT_READY_INPUT_ASYNC, 6).
-define(IOQ_EXIT_READY_OUTPUT_ASYNC, 7).
-define(IOQ_EXIT_TIMEOUT_ASYNC, 8).
-define(IOQ_EXIT_EVENT_ASYNC, 9).

ioq_exit_test(Config, TestNo) ->
    ?line Drv = ioq_exit_drv,
    ?line try
	      begin
		  ?line case load_driver(?config(data_dir, Config),
						  Drv) of
			    ok -> ?line ok;
			    {error, permanent} -> ?line ok;
			    LoadError -> ?line ?t:fail({load_error, LoadError})
			end,
		  case open_port({spawn, Drv}, []) of
		      Port when is_port(Port) ->
			      try port_control(Port, TestNo, "") of
				  "ok" ->
				      ?line ok;
				  "nyiftos" ->
				      ?line throw({skipped,
						   "Not yet implemented for "
						   "this OS"});
				  [$s,$k,$i,$p,$:,$ | Comment] ->
				      ?line throw({skipped, Comment});
				  [$e,$r,$r,$o,$r,$:,$ | Error] ->
				      ?line ?t:fail(Error)
			      after
				  Port ! {self(), close},
				receive {Port, closed} -> ok end,
				false = lists:member(Port, erlang:ports()),
				ok
			      end;
		      Error ->
			  ?line ?t:fail({open_port_failed, Error})
		  end
	      end
	  catch
	      throw:Term -> ?line Term
	  after
	      erl_ddll:unload_driver(Drv)
	  end.

ioq_exit_ready_input(doc) -> [];
ioq_exit_ready_input(suite) -> [];
ioq_exit_ready_input(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_INPUT).

ioq_exit_ready_output(doc) -> [];
ioq_exit_ready_output(suite) -> [];
ioq_exit_ready_output(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_OUTPUT).

ioq_exit_timeout(doc) -> [];
ioq_exit_timeout(suite) -> [];
ioq_exit_timeout(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_TIMEOUT).

ioq_exit_ready_async(doc) -> [];
ioq_exit_ready_async(suite) -> [];
ioq_exit_ready_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_ASYNC).

ioq_exit_event(doc) -> [];
ioq_exit_event(suite) -> [];
ioq_exit_event(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_EVENT).

ioq_exit_ready_input_async(doc) -> [];
ioq_exit_ready_input_async(suite) -> [];
ioq_exit_ready_input_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_INPUT_ASYNC).

ioq_exit_ready_output_async(doc) -> [];
ioq_exit_ready_output_async(suite) -> [];
ioq_exit_ready_output_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_READY_OUTPUT_ASYNC).

ioq_exit_timeout_async(doc) -> [];
ioq_exit_timeout_async(suite) -> [];
ioq_exit_timeout_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_TIMEOUT_ASYNC).

ioq_exit_event_async(doc) -> [];
ioq_exit_event_async(suite) -> [];
ioq_exit_event_async(Config) when is_list(Config) ->
    ioq_exit_test(Config, ?IOQ_EXIT_EVENT_ASYNC).


vsn_mismatch_test(Config, LoadResult) ->
    ?line Path = ?config(data_dir, Config),
    ?line DrvName = ?config(testcase, Config),
    ?line LoadResult = load_driver(Path, DrvName),
    ?line case LoadResult of
	      ok ->
		  ?line Port = open_port({spawn, DrvName}, []),
		  ?line true = is_port(Port),
		  ?line true = port_close(Port),
		  ?line ok = erl_ddll:unload_driver(DrvName);
	      _ ->
		  ?line ok
	  end.

zero_extended_marker_garb_drv(doc) -> [];
zero_extended_marker_garb_drv(suite) -> [];
zero_extended_marker_garb_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

invalid_extended_marker_drv(doc) -> [];
invalid_extended_marker_drv(suite) -> [];
invalid_extended_marker_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

larger_major_vsn_drv(doc) -> [];
larger_major_vsn_drv(suite) -> [];
larger_major_vsn_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

larger_minor_vsn_drv(doc) -> [];
larger_minor_vsn_drv(suite) -> [];
larger_minor_vsn_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

smaller_major_vsn_drv(doc) -> [];
smaller_major_vsn_drv(suite) -> [];
smaller_major_vsn_drv(Config) when is_list(Config) ->
    vsn_mismatch_test(Config, {error, driver_incorrect_version}).

smaller_minor_vsn_drv(doc) -> [];
smaller_minor_vsn_drv(suite) -> [];
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

peek_non_existing_queue(doc) -> [];
peek_non_existing_queue(suite) -> [];
peek_non_existing_queue(Config) when is_list(Config) ->
    ?line OTE = process_flag(trap_exit, true),
    ?line Drv = peek_non_existing_queue_drv,
    ?line try
	      begin
		  ?line case load_driver(?config(data_dir, Config),
						  Drv) of
			    ok -> ?line ok;
			    {error, permanent} -> ?line ok;
			    LoadError -> ?line ?t:fail({load_error, LoadError})
			end,
		  case open_port({spawn, Drv}, []) of
		      Port1 when is_port(Port1) ->
			  try port_control(Port1, ?PEEK_NONXQ_TEST, "") of
			      "ok" ->
				  ?line ok;
			      [$s,$k,$i,$p,$p,$e,$d,$:,$ | SkipReason] ->
				  ?line throw({skipped, SkipReason});
			      [$e,$r,$r,$o,$r,$:,$ | Error1] ->
				  ?line ?t:fail(Error1)
			  after
			      exit(Port1, kill),
			    receive {'EXIT', Port1, _} -> ok end
			  end;
		      Error1 ->
			  ?line ?t:fail({open_port1_failed, Error1})
		  end,
		  case open_port({spawn, Drv}, []) of
		      Port2 when is_port(Port2) ->
			  try port_control(Port2, ?PEEK_NONXQ_WAIT, "") of
			      "ok" ->
				  ?line ok;
			      [$e,$r,$r,$o,$r,$:,$ | Error2] ->
				  ?line ?t:fail(Error2)
			  after
			      receive {Port2, test_successful} -> ok end,
			    Port2 ! {self(), close},
			    receive {Port2, closed} -> ok end
			  end;
		      Error2 ->
			  ?line ?t:fail({open_port2_failed, Error2})
		  end
	      end
	  catch
	      throw:Term -> ?line Term
	  after
	      process_flag(trap_exit, OTE),
	      erl_ddll:unload_driver(Drv)
	  end.    

otp_6879(doc) ->
    [];
otp_6879(suite) ->
    [];
otp_6879(Config) when is_list(Config) ->
    ?line Drv = 'otp_6879_drv',
    ?line Parent = self(),
    ?line ok = load_driver(?config(data_dir, Config), Drv),
    ?line Procs = lists:map(
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
    ?line lists:foreach(fun (P) ->
				?line receive
					  {P, ok} ->
					      ?line ok;
					  {P, Error} ->
					      ?line ?t:fail({P, Error})
				      end
			end,
			Procs),
    %% Also try it when input exceeds default buffer (256 bytes)
    ?line Data = lists:seq(1, 1000),
    ?line case open_port({spawn, Drv}, []) of
	      Port when is_port(Port) ->
		  ?line ok = otp_6879_call(Port, Data, 10),
		  ?line erlang:port_close(Port);
	      _ ->
		  ?line ?t:fail(open_port_failed)
	  end,
    ?line erl_ddll:unload_driver(Drv),
    ?line ok.

otp_6879_call(_Port, _Data, 0) ->
    ok;
otp_6879_call(Port, Data, N) ->
    case catch erlang:port_call(Port, 0, Data) of
	Data -> otp_6879_call(Port, Data, N-1);
	BadData -> {mismatch, Data, BadData}
    end.

caller(doc) ->
    [];
caller(suite) ->
    [];
caller(Config) when is_list(Config) ->
    ?line run_caller_test(Config, false),
    ?line run_caller_test(Config, true).
    
run_caller_test(Config, Outputv) ->
    ?line Drv = 'caller_drv',
    ?line Cmd = case Outputv of
		    true ->
			?line os:putenv("CALLER_DRV_USE_OUTPUTV",
					"true"),
			outputv;
		    false ->
			?line os:putenv("CALLER_DRV_USE_OUTPUTV",
					"false"),
			output
		end,
    ?line ok = load_driver(?config(data_dir, Config), Drv),
    ?line Port = open_port({spawn, Drv}, []),
    ?line true = is_port(Port),
    ?line chk_caller(Port, start, self()),
    ?line chk_caller(Port,
		     Cmd,
		     spawn_link(
		       fun () ->
			       port_command(Port, "")
		       end)),
    ?line Port ! {self(), {command, ""}},
    ?line chk_caller(Port, Cmd, self()),
    ?line chk_caller(Port,
		     control,
		     spawn_link(
		       fun () ->
			       port_control(Port, 0, "")
		       end)),
    ?line chk_caller(Port,
		     call,
		     spawn_link(
		       fun () ->
			       erlang:port_call(Port, 0, "")
		       end)),
    ?line true = port_close(Port),
    ?line erl_ddll:unload_driver(Drv),
    ?line ok.

chk_caller(Port, Callback, ExpectedCaller) ->
    receive
	{caller, Port, Callback, Caller} ->
	    ExpectedCaller = Caller
    end.

many_events(suite) ->
    [];
many_events(doc) ->
    ["Check that many simultaneously signalled events work (win32)"];
many_events(Config) when is_list(Config) ->
    ?line Name = 'many_events_drv',
    ?line Port = start_driver(Config, Name, false),
    Number = "1000",
    Port ! {self(), {command, Number}},
    receive 
	{Port, {data,Number}} ->
	    ?line receive %% Just to make sure the emulator does not crash 
		    %% after this case is run (if faulty)
		      after 2000 ->
			      ok
		      end
    after 1000 ->
	    ?line exit(the_driver_does_not_respond)
    end,
    ?line stop_driver(Port, Name),
    ?line ok.
	     

missing_callbacks(doc) ->
    [];
missing_callbacks(suite) ->
    [];
missing_callbacks(Config) when is_list(Config) ->
    ?line Name = 'missing_callback_drv',
    ?line Port = start_driver(Config, Name, false),

    ?line Port ! {self(), {command, "tjenix"}},
    ?line true = erlang:port_command(Port, "halloj"),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(Port, 4711, "mors")),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_call(Port, 17, "hej")),

    ?line %% Give the (non-existing) ready_output(), ready_input(), event(),
    ?line %% and timeout() some time to be called.
    ?line receive after 1000 -> ok end,

    ?line stop_driver(Port, Name),
    ?line ok.

smp_select(doc) ->
    ["Test concurrent calls to driver_select."];
smp_select(suite) ->
    [];
smp_select(Config) when is_list(Config) -> 
    case os:type() of
	{win32,_} -> {skipped, "Test not implemented for this OS"};
	_ -> smp_select0(Config)
    end.
    
smp_select0(Config) ->
    ?line DrvName = 'chkio_drv',
    Path = ?config(data_dir, Config),
    erl_ddll:start(),
    ?line ok = load_driver(Path, DrvName),    
    Master = self(),
    ProcFun = fun()-> io:format("Worker ~p starting\n",[self()]),	
		      ?line Port = open_port({spawn, DrvName}, []),
		      smp_select_loop(Port, 100000),
		      sleep(1000), % wait for driver to handle pending events
		      ?line true = erlang:port_close(Port),
		      Master ! {ok,self()},
		      io:format("Worker ~p finished\n",[self()])
	     end,
    ?line Pids = lists:map(fun(_) -> spawn_link(ProcFun) end,
			   lists:seq(1,4)),
    TimeoutMsg = make_ref(),
    {ok,TRef} = timer:send_after(5*1000, TimeoutMsg), % Limit test duration on slow machines
    smp_select_wait(Pids, TimeoutMsg),
    timer:cancel(TRef),
    ?line ok = erl_ddll:unload_driver(DrvName),
    ?line ok = erl_ddll:stop(),
    ok.

smp_select_loop(_, 0) ->
    ok;
smp_select_loop(Port, N) ->
    ?line "ok" = erlang:port_control(Port, ?CHKIO_SMP_SELECT, []),
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


driver_select_use(doc) ->
    ["Test driver_select() with new ERL_DRV_USE flag."];
driver_select_use(suite) ->
    [];
driver_select_use(Config) when is_list(Config) -> 
    case os:type() of
	{win32,_} -> {skipped, "Test not implemented for this OS"};
	_ -> driver_select_use0(Config)
    end.
    
driver_select_use0(Config) ->
    ?line DrvName = 'chkio_drv',
    Path = ?config(data_dir, Config),
    erl_ddll:start(),
    ?line ok = load_driver(Path, DrvName),    
    ?line Port = open_port({spawn, DrvName}, []),
    ?line "ok" = erlang:port_control(Port, ?CHKIO_DRV_USE, []),
    ?line {Port,{data,"TheEnd"}} = receive Msg -> Msg
				   after 10000 -> timeout end,
    ?line true = erlang:port_close(Port),
    ?line ok = erl_ddll:unload_driver(DrvName),
    ?line ok = erl_ddll:stop(),
    ok.

thread_mseg_alloc_cache_clean(Config) when is_list(Config) ->
    case {erlang:system_info(threads),
	  erlang:system_info({allocator,mseg_alloc}),
	  driver_alloc_sbct()} of
	{_, false, _} ->
	    ?line {skipped, "No mseg_alloc"};
	{false, _, _} ->
	    ?line {skipped, "No threads"};
	{_, _, false} ->
	    ?line {skipped, "driver_alloc() not using the alloc_util framework"};
	{_, _, SBCT} when is_integer(SBCT), SBCT > 10*1024*1024 ->
	    ?line {skipped, "driver_alloc() using too large single block threshold"};
	{_, _, 0} ->
	    ?line {skipped, "driver_alloc() using too low single block threshold"};
	{true, _MsegAllocInfo, SBCT} ->
	    ?line DrvName = 'thr_alloc_drv',
	    ?line Path = ?config(data_dir, Config),
	    ?line erl_ddll:start(),
	    ?line ok = load_driver(Path, DrvName),   
	    ?line Port = open_port({spawn, DrvName}, []),
	    ?line CCI = 1000,
	    ?line ?t:format("CCI = ~p~n", [CCI]),
	    ?line CCC = mseg_alloc_ccc(),
	    ?line ?t:format("CCC = ~p~n", [CCC]),
	    ?line thread_mseg_alloc_cache_clean_test(Port,
						     10,
						     CCI,
						     SBCT+100),
	    ?line true = erlang:port_close(Port),
	    ?line ok = erl_ddll:unload_driver(DrvName),
	    ?line ok = erl_ddll:stop(),
	    ?line ok
    end.

mseg_alloc_cci(MsegAllocInfo) ->
    ?line {value,{options, OL}}
	= lists:keysearch(options, 1, MsegAllocInfo),
    ?line {value,{cci,CCI}} = lists:keysearch(cci,1,OL),
    ?line CCI.

mseg_alloc_ccc() ->
    mseg_alloc_ccc(mseg_inst_info(0)).

mseg_alloc_ccc(MsegAllocInfo) ->
    ?line {value,{memkind, MKL}} = lists:keysearch(memkind,1,MsegAllocInfo),
    ?line {value,{calls, CL}} = lists:keysearch(calls, 1, MKL),
    ?line {value,{mseg_check_cache, GigaCCC, CCC}}
	= lists:keysearch(mseg_check_cache, 1, CL),
    ?line GigaCCC*1000000000 + CCC.

mseg_alloc_cached_segments() ->
    mseg_alloc_cached_segments(mseg_inst_info(0)).

mseg_alloc_cached_segments(MsegAllocInfo) ->
    MemName = "all memory",
    ?line [{memkind,DrvMem}]
	= lists:filter(fun(E) -> case E of
				    {memkind, [{name, MemName} | _]} -> true;
				    _ -> false
		       end end, MsegAllocInfo),
    ?line {value,{status, SL}}
	= lists:keysearch(status, 1, DrvMem),
    ?line {value,{cached_segments, CS}}
	= lists:keysearch(cached_segments, 1, SL),
    ?line CS.

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
    ?line ok;
thread_mseg_alloc_cache_clean_test(Port, N, CCI, Size) ->
    ?line wait_until(fun () -> 0 == mseg_alloc_cached_segments() end),
    ?line receive after CCI+500 -> ok end,
    ?line OCCC = mseg_alloc_ccc(),
    ?line "ok" = erlang:port_control(Port, 0, integer_to_list(Size)),
    ?line receive after CCI+500 -> ok end,
    ?line CCC = mseg_alloc_ccc(),
    ?line ?t:format("CCC = ~p~n", [CCC]),
    ?line true = CCC > OCCC,
    ?line thread_mseg_alloc_cache_clean_test(Port, N-1, CCI, Size).

otp_9302(Config) when is_list(Config) ->
    ?line Path = ?config(data_dir, Config),
    ?line erl_ddll:start(),
    ?line ok = load_driver(Path, otp_9302_drv),
    ?line Port = open_port({spawn, otp_9302_drv}, []),
    ?line true = is_port(Port),
    ?line port_command(Port, ""),
    ?line {msg, block} = get_port_msg(Port, infinity),
    ?line {msg, job} = get_port_msg(Port, infinity),
    ?line C = case erlang:system_info(thread_pool_size) of
		  0 ->
		      ?line {msg, cancel} = get_port_msg(Port, infinity),
		      ?line {msg, job} = get_port_msg(Port, infinity),
		      ?line false;
		  _ ->
		      case get_port_msg(Port, infinity) of
			  {msg, cancel} -> %% Cancel always fail in Rel >= 15
			      ?line {msg, job} = get_port_msg(Port, infinity),
			      ?line false;
			  {msg, job} ->
			      ?line ok,
			      ?line true
		      end
	      end,
    ?line {msg, end_of_jobs} = get_port_msg(Port, infinity),
    ?line no_msg = get_port_msg(Port, 2000),
    ?line port_close(Port),
    ?line case C of
	      true ->
		  ?line {comment, "Async job cancelled"};
	      false ->
		  ?line {comment, "Async job not cancelled"}
	  end.

thr_free_drv(Config) when is_list(Config) ->
    case erlang:system_info(threads) of
	false ->
	    {skipped, "No thread support"};
	true ->
	    thr_free_drv_do(Config)
    end.

thr_free_drv_do(Config) ->
    ?line Path = ?config(data_dir, Config),
    ?line erl_ddll:start(),
    ?line ok = load_driver(Path, thr_free_drv),
    ?line MemBefore = driver_alloc_size(),
%    io:format("SID=~p", [erlang:system_info(scheduler_id)]),
    ?line Port = open_port({spawn, thr_free_drv}, []),
    ?line MemPeek = driver_alloc_size(),
    ?line true = is_port(Port),
    ?line ok = thr_free_drv_control(Port, 0),
    ?line port_close(Port),
    ?line MemAfter = driver_alloc_size(),
    ?line io:format("MemPeek=~p~n", [MemPeek]),
    ?line io:format("MemBefore=~p, MemAfter=~p~n", [MemBefore, MemAfter]),
    ?line MemBefore = MemAfter,
    ?line case MemPeek of
	      undefined -> ok;
	      _ ->
		  ?line true = MemPeek > MemBefore
	  end,
    ?line ok.

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
    ?line Path = ?config(data_dir, Config),
    ?line erl_ddll:start(),
    ?line ok = load_driver(Path, async_blast_drv),
    ?line SchedOnln = erlang:system_info(schedulers_online),
    ?line MemBefore = driver_alloc_size(),
    ?line Start = os:timestamp(),
    ?line Blast = fun () ->
			  Port = open_port({spawn, async_blast_drv}, []),
			  true = is_port(Port),
			  port_command(Port, ""),
			  receive
			      {Port, done} ->
				  ok
			  end,
			  port_close(Port)
		  end,
    ?line Ps = lists:map(fun (N) ->
				 spawn_opt(Blast,
					   [{scheduler,
					     (N rem SchedOnln)+ 1},
					    monitor])
			 end,
			 lists:seq(1, 100)),
    ?line MemMid = driver_alloc_size(),
    ?line lists:foreach(fun ({Pid, Mon}) ->
				receive
				    {'DOWN',Mon,process,Pid,_} -> ok
				end
			end, Ps),
    ?line End = os:timestamp(),
    ?line MemAfter = driver_alloc_size(),
    ?line io:format("MemBefore=~p, MemMid=~p, MemAfter=~p~n",
		    [MemBefore, MemMid, MemAfter]),
    ?line AsyncBlastTime = timer:now_diff(End,Start)/1000000,
    ?line io:format("AsyncBlastTime=~p~n", [AsyncBlastTime]),
    ?line MemBefore = MemAfter,
    ?line erlang:display({async_blast_time, AsyncBlastTime}),
    ?line ok.

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
    case erlang:system_info(smp_support) of
	false ->
	    {skipped, "Non-SMP emulator; nothing to test..."};
	true ->
	    Path = ?config(data_dir, Config),
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
		    ?t:fail({received_garbage, Port, Garbage})
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
	    Res
    end.

-define(IN_RANGE(LoW_, VaLuE_, HiGh_),
	case in_range(LoW_, VaLuE_, HiGh_) of
	    true -> ok;
	    false ->
		case erlang:system_info(lock_checking) of
		    true ->
			?t:format("~p:~p: Ignore bad sched count due to "
				  "lock checking~n",
				  [?MODULE,?LINE]);
		    false ->
			?t:fail({unexpected_sched_counts, VaLuE_})
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

    Path = ?config(data_dir, Config),
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
    ?t:format("Scheduling counts: ~p~n", [Res]),
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
    
a_test(Config) when is_list(Config) ->
    check_io_debug().

z_test(Config) when is_list(Config) ->
    check_io_debug().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 		Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_io_debug() ->
    get_stable_check_io_info(),
    {NoErrorFds, NoUsedFds, NoDrvSelStructs, NoDrvEvStructs}
	= erts_debug:get_internal_state(check_io_debug),
    0 = NoErrorFds,
    NoUsedFds = NoDrvSelStructs,
    0 = NoDrvEvStructs,
    ok.

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
    [Major, Minor] = string:tokens(Str, "."),
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
    case get(random_seed) of
	undefined ->
	    {X, Y, Z} = time(),
	    random:seed(X, Y, Z);
	_ ->
	    ok
    end,
    random:uniform(N).

erl_millisecs() ->
    erl_millisecs(erlang:monotonic_time()).

erl_millisecs(MonotonicTime) ->
    (1000*MonotonicTime)/erlang:convert_time_unit(1,seconds,native).

%% Start/stop drivers.
start_driver(Config, Name, Binary) ->
    Path = ?config(data_dir, Config),
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
    ?line true = erlang:port_close(Port),
    receive
	{Port,Message} ->
	    ?t:fail({strange_message_from_port,Message})
    after 0 ->
	    ok
    end,

    %% Unload the driver.
    ok = erl_ddll:unload_driver(Name),
    ?line ok = erl_ddll:stop().

load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
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
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(?config(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(seconds))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    ?t:start_node(Name, slave, [{args, "-pa "++Pa}]).

stop_node(Node) ->
    ?t:stop_node(Node).

wait_deallocations() ->
    try
	erts_debug:set_internal_state(wait, deallocations)
    catch error:undef ->
	    erts_debug:set_internal_state(available_internal_state, true),
	    wait_deallocations()
    end.

driver_alloc_size() ->
    case erlang:system_info(smp_support) of
	true ->
	    ok;
	false ->
	    %% driver_alloc also used by elements in lock-free queues,
	    %% give these some time to be deallocated...
	    receive after 100 -> ok end
    end,
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
