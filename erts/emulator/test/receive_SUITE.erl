%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-export([all/0, suite/0,
	 call_with_huge_message_queue/1,receive_in_between/1,
         receive_opt_exception/1,receive_opt_recursion/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() ->
    [call_with_huge_message_queue, receive_in_between,
     receive_opt_exception, receive_opt_recursion].

call_with_huge_message_queue(Config) when is_list(Config) ->
    Pid = spawn_link(fun echo_loop/0),
    _WarmUpTime = time_calls(Pid),
    Time = time_calls(Pid),
    _ = [self() ! {msg,N} || N <- lists:seq(1, 500000)],
    io:format("Time for empty message queue: ~p", [Time]),
    erlang:garbage_collect(),
    call_with_huge_message_queue_1(Pid, Time, 5).

call_with_huge_message_queue_1(_Pid, _Time, 0) ->
    ct:fail(bad_ratio);
call_with_huge_message_queue_1(Pid, Time, NumTries) ->
    HugeTime = time_calls(Pid),
    io:format("Time for huge message queue: ~p", [HugeTime]),

    case (HugeTime+1) / (Time+1) of
        Q when Q < 10 ->
            ok;
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
    calls(100, Pid).

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

%%%
%%% Common helpers.
%%%

echo_loop() ->
    receive
	{Ref,{Pid,Msg}} ->
	    Pid ! {Ref,Msg},
	    echo_loop()
    end.
