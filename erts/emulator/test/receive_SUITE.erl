%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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

-module(receive_SUITE).

%% Tests receive after.

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_testcase/2, end_per_testcase/2,
	 call_with_huge_message_queue/1,receive_in_between/1,
         receive_opt_exception/1,receive_opt_recursion/1,
         receive_opt_deferred_save/1,
         erl_1199/1, multi_recv_opt/1,
         multi_recv_opt_clear/1,
         gh_5235_missing_save_reset/1,
         gh_5235_recv_mark/1,
         recv_marker_reserve/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() ->
    [call_with_huge_message_queue,
     receive_in_between,
     receive_opt_exception,
     receive_opt_recursion,
     receive_opt_deferred_save,
     erl_1199, multi_recv_opt,
     multi_recv_opt_clear,
     gh_5235_missing_save_reset,
     gh_5235_recv_mark,
     recv_marker_reserve].

init_per_testcase(receive_opt_deferred_save, Config) ->
    case erlang:system_info(schedulers_online) of
        1 ->
            {skip, "Needs more schedulers to run"};
        _ ->
	    erts_debug:set_internal_state(available_internal_state, true),
            Config
    end;
init_per_testcase(erl_1199, Config) ->
    SO = erlang:system_info(schedulers_online),
    [{schedulers_online, SO}|Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(erl_1199, Config) ->
    {value, {schedulers_online, SO}} = lists:keysearch(schedulers_online,
                                                       1, Config),
    case erlang:system_info(schedulers_online) of
        SO ->
            ok;
        _ ->
            erlang:system_info(schedulers_online, SO),
            SO = erlang:system_info(schedulers_online)
    end,
    Config;
end_per_testcase(receive_opt_deferred_save, Config) ->
    erts_debug:set_internal_state(available_internal_state, false),
    Config;
end_per_testcase(_Name, Config) ->
    Config.

call_with_huge_message_queue(Config) when is_list(Config) ->
    Pid = spawn_link(fun echo_loop/0),
    process_flag(message_queue_data, off_heap),
    _WarmUpTime = time_calls(Pid),
    erlang:garbage_collect(),
    Time = time_calls(Pid),
    io:format("Time for empty message queue: ~p", [Time]),
    _ = [self() ! {msg,N} || N <- lists:seq(1, 500000)],
    call_with_huge_message_queue_1(Pid, Time, 5).

call_with_huge_message_queue_1(_Pid, _Time, 0) ->
    ct:fail(bad_ratio);
call_with_huge_message_queue_1(Pid, Time, NumTries) ->
    erlang:garbage_collect(),
    HugeTime = time_calls(Pid),
    io:format("Time for huge message queue: ~p", [HugeTime]),

    case (HugeTime+1) / (Time+1) of
        Q when Q < 10 ->
	    {comment, "Ratio: "++erlang:float_to_list(Q)};
        Q ->
            io:format("Too high ratio: ~p\n", [Q]),
            call_with_huge_message_queue_1(Pid, Time, NumTries-1)
    end.

%% Time a number calls. Try to avoid returning a zero time.
time_calls(Pid) ->
    time_calls(Pid, 10).

time_calls(_Pid, 0) ->
    0;
time_calls(Pid, NumTries) ->
    case timer:tc(fun() -> calls(Pid) end) of
        {0,ok} ->
            time_calls(Pid, NumTries-1);
        {Time,ok} ->
            Time
    end.

calls(Pid) ->
    calls(10000, Pid).

calls(0, _) -> ok;
calls(N, Pid) ->
    {ok,{ultimate_answer,42}} = call(Pid, {ultimate_answer,42}),
    calls(N-1, Pid).

call(Pid, Msg) ->
    Mref = erlang:monitor(process, Pid),
    Pid ! {Mref,{self(),Msg}},
    receive
	{Mref, Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    {ok, Reply};
	{'DOWN', Mref, _, _, Reason} ->
	    exit(Reason)
    end.

receive_in_between(Config) when is_list(Config) ->
    Pid = spawn_link(fun echo_loop/0),
    [{ok,{a,b}} = call2(Pid, {a,b}) || _ <- lists:seq(1, 100000)],
    ok.

call2(Pid, Msg) ->
    self() ! dummy,
    Mref = erlang:monitor(process, Pid),
    Pid ! {Mref,{self(),Msg}},
    receive_one(),
    receive
	{Mref,Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    {ok,Reply};
	{'DOWN',Mref,_,_,Reason} ->
	    exit(Reason)
    end.

receive_one() ->
    receive
	dummy -> ok
    end.

receive_opt_exception(_Config) ->
    Recurse = fun() ->
                      %% Overwrite with the same mark,
                      %% and never consume it.
                      ThrowFun = fun() -> throw(aborted) end,
                      aborted = (catch do_receive_opt_exception(ThrowFun)),
                      ok
              end,
    do_receive_opt_exception(Recurse),

    %% Eat the second message.
    receive
        Ref when is_reference(Ref) -> ok
    end.

do_receive_opt_exception(Disturber) ->
    %% Create a receive mark.
    Ref = make_ref(),
    self() ! Ref,
    Disturber(),
    receive
        Ref ->
            ok
    after 0 ->
            error(the_expected_message_was_not_there)
    end.

receive_opt_recursion(_Config) ->
    Recurse = fun() ->
                      %% Overwrite with the same mark,
                      %% and never consume it.
                      NoOp = fun() -> ok end,
                      BlackHole = spawn(NoOp),
                      expected = do_receive_opt_recursion(BlackHole, NoOp, true),
                      ok
              end,
    do_receive_opt_recursion(self(), Recurse, false),
    ok.

do_receive_opt_recursion(Recipient, Disturber, IsInner) ->
    Ref = make_ref(),
    Recipient ! Ref,
    Disturber(),
    receive
        Ref -> ok
    after 0 ->
            case IsInner of
                true ->
                    expected;
                false ->
                    error(the_expected_message_was_not_there)
            end
    end.


%% Test that the receive opt behaves correctly when
%% the messages are in the middle queue. It only triggers
%% a very special scenario that OTP-16241 solves.
receive_opt_deferred_save(_Config) ->

    %% This testcase is very very white-boxy, but I'm not
    %% sure what to do about that.

    Pid = spawn_opt(fun() ->
                        deferred()
                end,[{scheduler, 2}]),
    spawn_opt(fun() ->
                      link(Pid),
                      Lst = lists:seq(1,200),
                      Pid ! go,
                      %% Sleep in order to make sure that Pid gets
                      %% scheduled in.
                      erts_debug:set_internal_state(sleep, 250),
                      Ref = erlang:monitor(process, Pid),
                      [Pid ! I || I <- Lst],
                      Pid ! stop,
                      receive
                          {'DOWN', Ref, process, Pid, normal} ->
                              ok
                      after 2000 ->
                              exit(stop_timeout)
                      end
              end,[{scheduler,1},monitor]),
    receive
        {'DOWN',_, process, _, normal} ->
            ok;
        {'DOWN',_, process, _, Reason} ->
            ct:fail(Reason)
    end.

deferred() ->
    receive
        go ->
            %% Sleep for a while so that the middle queue
            %% is filled
            erts_debug:set_internal_state(sleep, 1000)
    end,

    %% Here the inner queue should be empty and the middle queue
    %% should have 200 messages and one monitor signal.
    %% The monitor signal is important as otherwise all messages
    %% will just be moved from the outer to the inner queue
    %% immediately.

    %% Setup the receive opt mark
    %% This receive opt is as of PR-2439 disabled, though future
    %% optimizations may enable it again....
    Ref = make_ref(),
    self() ! Ref,

    %% Call another function that does a non-receive opt receive.
    %% Before OTP-16241 this would hang as the save marker in the
    %% message queue would be incorrectly set.
    deferred(1,200),

    %% Get the ref using receive opt here.
    receive
        Ref ->
            ok
    end,
    receive
        stop ->
            erlang:display(ok)
    end.

deferred(N,M) when N > M ->
    ok;
deferred(N,M) ->
    receive
        N ->
            deferred(N+1,M)
    end.

erl_1199(Config) when is_list(Config) ->
    %% Whitebox testing for issue in ERL-1199/OTP-16572
    %%
    %% When the bug hits, the client save pointer will be pointing to
    %% a message later than the actual message we want to match on.
    %%
    %% In order to trigger the bug we want to have messages in the
    %% message queue (inner queue) and get scheduled out while we are
    %% working with signals in the middle queue and have handled signals
    %% past the save_last pointer while it is deferred (possibly pointing
    %% into the middle queue). When this happens the save pointer is set
    %% to the end of the message queue (to indicate that we need to
    %% handle more signals in the middle queue before we have any
    %% messages that can match) via deferred save. The save_last pointer
    %% now points into the message queue but we cannot determine that
    %% since we don't know if we have handled signals past it or not. The
    %% main issue here is that we did not keep the deferred_save flag so
    %% we later can adjust save_last when we know longer have a
    %% deferred_save_last.
    %%
    %% The testcase tries with a psequdo random amount of signals, over
    %% and over again, which makes it likely to hit the bug if reintroduced
    %% even if reduction costs etc are changed. The test-case, at the
    %% time of writing, consistently fail (on my machine) while the bug is
    %% present.
    %%
    SO = erlang:system_info(schedulers_online),
    try
        process_flag(priority, high),
        Srv = spawn_opt(fun erl_1199_server/0, [link, {priority, max}]),
        Clnt = spawn_opt(fun () -> erl_1199_client(Srv) end,
                         [link, {priority, normal},
                          {message_queue_data, on_heap}]),
        Srv ! {client, Clnt},
        receive after 10000 -> ok end,
        Responsive = make_ref(),
        Clnt ! {responsive_check, self(), Responsive},
        Result = receive
                     Responsive ->
                         ok
                 after 10000 ->
                         hanging_client
                 end,
        unlink(Clnt),
        exit(Clnt, kill),
        unlink(Srv),
        exit(Srv, kill),
        %% Wait for terminations...
        false = is_process_alive(Clnt),
        false = is_process_alive(Srv),
        ok = Result
    after
        erlang:system_flag(schedulers_online, SO)
    end.

erl_1199_server() ->
    receive
	{client, Clnt} ->
	    rand:seed(exrop, 4711),
	    BEN = fun () -> Clnt ! {blipp}, exit(Clnt, normal) end,
	    erl_1199_server(Clnt, BEN)
    end.

erl_1199_server(Clnt, BEN) ->
    receive
	prepare ->
	    Extra = rand:uniform(10000),
	    erl_1199_do(BEN, 1000),
	    erl_1199_do(BEN, Extra);
	{request, Ref} ->
	    Extra = rand:uniform(10000),
	    erl_1199_do(BEN, 1000),
	    erl_1199_do(BEN, Extra),
	    Clnt ! Ref, %% Response...
	    erl_1199_do(BEN, 1000),
	    erl_1199_do(BEN, Extra)
    end,
    erl_1199_server(Clnt, BEN).

erl_1199_do(_Fun, 0) ->
    ok;
erl_1199_do(Fun, N) ->
    Fun(),
    erl_1199_do(Fun, N-1).

erl_1199_client(Srv) ->
    Srv ! prepare,
    erlang:yield(),
    Ref = erlang:monitor(process, Srv),
    Srv ! {request, Ref},
    erlang:yield(),
    receive
	Ref -> ok;
	{'DOWN', Ref, _, _, _} -> ok
    end,
    erl_1199_flush_blipp(),
    receive
	{responsive_check, From, FromRef} ->
	    From ! FromRef
    after
	0 ->
	    ok
    end,
    erl_1199_client(Srv).

erl_1199_flush_blipp() ->
    receive
	{blipp} ->
	    erl_1199_flush_blipp()
    after 0 ->
	    ok
    end.

multi_recv_opt(Config) when is_list(Config) ->
    HugeMsgQ = 500000,
    Srv = spawn_link(fun multi_echoer/0),
    process_flag(message_queue_data, off_heap),
    _WarmUpTime = time_multi_call(Srv, fun multi_call/1),
    EmptyTime = time_multi_call(Srv, fun multi_call/1),
    io:format("Time with empty message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(EmptyTime, native, microsecond)]),
    _ = [self() ! {msg,N} || N <- lists:seq(1, HugeMsgQ)],
    HugeTime = time_multi_call(Srv, fun multi_call/1),
    io:format("Time with huge message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(HugeTime, native, microsecond)]),
    Q = HugeTime / EmptyTime,
    HugeMsgQ = flush_msgq(),
    case Q > 10 of
	true ->
	    ct:fail({ratio, Q});
	false ->
	    {comment, "Ratio: "++erlang:float_to_list(Q)}
    end.
    
multi_echoer() ->
    receive
	{Mref, From, Msg, Responses} ->
	    multi_response(Mref, From, Msg, Responses);
	_Garbage ->
	    ok
    end,
    multi_echoer().

multi_response(_Mref, _From, _Msg, 0) ->
    ok;
multi_response(Mref, From, Msg, N) ->
    From ! {Mref, Msg},
    multi_response(Mref, From, Msg, N-1).

time_multi_call(Srv, Fun) ->
    garbage_collect(),
    Start = erlang:monotonic_time(),
    ok = Fun(Srv),
    erlang:monotonic_time() - Start.
    
multi_call(Srv) ->
    Resp = 100,
    NoOp = fun () -> ok end,
    Fun3 = fun () ->
		   multi_call(Srv, 1, three, Resp, false, NoOp)
	   end,
    Fun2 = fun () ->
		   multi_call(Srv, 1, two, Resp, false, Fun3)
	   end,
    multi_call(Srv, 1000, one, Resp, false, Fun2).

multi_call(_Srv, 0, _Msg, _Responses, _Clear, _Fun) ->
    ok;
multi_call(Srv, N, Msg, Responses, Clear, Fun) ->

    Mref = erlang:monitor(process, Srv),

    Srv ! {Mref, self(), Msg, Responses},
    Fun(),
    multi_receive(Mref, Msg, Responses),
    erlang:demonitor(Mref, [flush]),
    multi_call(Srv, N-1, Msg, Responses, Clear, Fun).
    
multi_receive(_Mref, _Msg, 0) ->
    ok;
multi_receive(Mref, Msg, N) ->

    receive
	{Mref, RMsg} ->
	    Msg = RMsg;
	{'DOWN', Mref, process, _, Reason} ->
	    exit({server_error, Reason})
    end,
    multi_receive(Mref, Msg, N-1).

multi_recv_opt_clear(Config) when is_list(Config) ->
    %% Whitebox: we know that we have at most 8 active
    %% receive markers...
    %%
    %% We here test a scenario where we need clear instructions
    %% to be issued when the a receive marker is no longer
    %% needed. If no clear instructions are issued, we will
    %% reuse the marker for the outer call which will cause
    %% scans of the whole message queue when receiving outer
    %% results...
    %%
    HugeMsgQ = 500000,
    Srv = spawn_link(fun multi_echoer/0),
    process_flag(message_queue_data, off_heap),
    _WarmUpTime = time_multi_call(Srv, fun multi_call_clear/1),
    EmptyTime = time_multi_call(Srv, fun multi_call_clear/1),
    io:format("Time with empty message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(EmptyTime, native, microsecond)]),
    _ = [self() ! {msg,N} || N <- lists:seq(1, HugeMsgQ)],
    HugeTime = time_multi_call(Srv, fun multi_call_clear/1),
    io:format("Time with huge message queue: ~p microsecond~n",
	      [erlang:convert_time_unit(HugeTime, native, microsecond)]),
    Q = HugeTime / EmptyTime,
    HugeMsgQ = flush_msgq(),
    case Q > 10 of
	true ->
	    ct:fail({ratio, Q});
	false ->
	    {comment, "Ratio: "++erlang:float_to_list(Q)}
    end.

multi_call_clear(Srv) ->
    NoOp = fun () -> ok end,
    Fun2 = fun () ->
		   multi_call(Srv, 1, two0, 2, true, NoOp),
		   multi_call(Srv, 1, two1, 2, true, NoOp),
		   multi_call(Srv, 1, two2, 2, true, NoOp),
		   multi_call(Srv, 1, two3, 2, true, NoOp),
		   multi_call(Srv, 1, two4, 2, true, NoOp),
		   multi_call(Srv, 1, two5, 2, true, NoOp),
		   multi_call(Srv, 1, two6, 2, true, NoOp),
		   multi_call(Srv, 1, two7, 2, true, NoOp),
		   multi_call(Srv, 1, two8, 2, true, NoOp),
		   multi_call(Srv, 1, two9, 2, true, NoOp)
	   end,
    multi_call(Srv, 1000, one, 100, true, Fun2).
  
gh_5235_missing_save_reset(Config) when is_list(Config) ->
    %%
    %% Used to hang in the second receive due to save
    %% pointer not being reset on bad timeout value...
    %%
    ct:timetrap({seconds, 10}),
    id(self()) ! init,
    try
        receive blipp -> ok after blupp -> ok end
    catch _:_ ->
            ok
    end,
    receive init -> ok end,

    %% Try with a timeout value not known in compile
    %% time as well...
    id(self()) ! init2,
    try
        receive blapp -> ok after id(blepp) -> ok end
    catch _:_ ->
            ok
    end,
    receive init2 -> ok end.

gh_5235_recv_mark(Config) when is_list(Config) ->
    %%
    %% Used to leak a recv marker into the receive
    %% via erts_msgq_peek_msg() due to save pointer
    %% not being reset on bad timeout value...
    %%
    ct:timetrap({seconds, 10}),
    id(self()) ! init,
    try
        receive blipp -> ok after blupp -> ok end
    catch _:_ ->
            ok
    end,
    Ref = make_ref(),
    id(self()) ! Ref,
    receive init -> ok end,
    receive Ref -> ok end,
    
    %% Try with a timeout value not known in compile
    %% time as well...
    id(self()) ! init2,
    try
        receive blapp -> ok after id(blepp) -> ok end
    catch _:_ ->
            ok
    end,
    Ref2 = make_ref(),
    id(self()) ! Ref2,
    receive init2 -> ok end,
    receive Ref2 -> ok end.

%% ERIERL-905: The `recv_marker_reserve` instruction didn't update heap/stack
%% pointers despite allocating memory after a per-process counter goes beyond
%% MAX_SMALL.
recv_marker_reserve(Config) when is_list(Config) ->
    Retries = 500,
    {MinSmall, MaxSmall} = determine_small_limits(0),
    Counter = -MinSmall + MaxSmall - Retries,

    DummyRef = make_ref(),
    self() ! DummyRef,

    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:set_internal_state(process_uniq_counter, Counter),
    recv_marker_reserve_retry(Retries * 2),
    erts_debug:set_internal_state(available_internal_state, false),

    receive
        DummyRef -> ok
    end.

recv_marker_reserve_retry(0) ->
    ok;
recv_marker_reserve_retry(N) ->
    Ref = monitor(process, self(), [{alias,demonitor}]),
    self() ! Ref,
    receive
        Ref -> erlang:demonitor(Ref, [flush])
    end,
    recv_marker_reserve_retry(N - 1).

determine_small_limits(N) ->
    case is_small(-1 bsl N) of
        true -> determine_small_limits(N + 1);
        false -> {-1 bsl (N - 1), (1 bsl (N - 1)) - 1}
    end.

is_small(N) when is_integer(N) ->
    0 =:= erts_debug:flat_size(N).


%%%
%%% Common helpers.
%%%

id(X) -> X.

flush_msgq() ->
    flush_msgq(0).
flush_msgq(N) ->
    receive
	_ ->
	    flush_msgq(N+1)
    after 0 ->
	    N
    end.

echo_loop() ->
    receive
	{Ref,{Pid,Msg}} ->
	    Pid ! {Ref,Msg},
	    echo_loop()
    end.
