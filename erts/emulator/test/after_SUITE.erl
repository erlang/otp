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

-module(after_SUITE).

%% Tests receive after.

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
	 t_after/1, receive_after/1, receive_after_big/1,
	 receive_after_errors/1, receive_var_zero/1, receive_zero/1,
	 multi_timeout/1, receive_after_32bit/1,
	 receive_after_blast/1]).

%% Internal exports.

-export([timeout_g/0]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() -> 
    [t_after, receive_after, receive_after_big,
     receive_after_errors, receive_var_zero, receive_zero,
     multi_timeout, receive_after_32bit, receive_after_blast].

%% Tests for an old round-off error in 'receive after'."
t_after(Config) when is_list(Config) ->
    spawn(fun frequent_process/0),
    Period = test_server:minutes(1),
    Before = erlang:monotonic_time(),
    receive
    after Period ->
                After = erlang:monotonic_time(),
                report(Period, Before, After)
    end.

report(Period, Before, After) ->
    case erlang:convert_time_unit(After - Before, native, 100*1000) / Period of
        Percent when Percent > 100.10 ->
            ct:fail({too_inaccurate, Percent});
        Percent when Percent < 100.0 ->
            ct:fail({too_early, Percent});
        Percent ->
            Comment = io_lib:format("Elapsed/expected: ~.2f %", [Percent]),
            {comment, lists:flatten(Comment)}
    end.

frequent_process() ->
    receive
    after 100 ->
              frequent_process()
    end.

%%  Test that 'receive after' works (doesn't hang).
%%  The test takes 10 seconds to complete.
receive_after(Config) when is_list(Config) ->
    receive_after1(5000).

receive_after1(1) ->
    io:format("Testing: receive after ~p~n", [1]), 
    receive after 1 -> ok end;
receive_after1(N) -> 
    io:format("Testing: receive after ~p~n", [N]), 
    receive after N -> receive_after1(N div 2) end.

receive_after_big(Config) when is_list(Config) ->
    %% Test that 'receive after' with a 32 bit number works.
    receive_after_big1(16#f7654321),
    receive_after_big2().

receive_after_big1(Timeout) ->
    Self = self(),
    erlang:yield(),
    spawn(fun() -> Self ! here_is_a_message end),
    ok = receive
             here_is_a_message ->
                 ok
         after Timeout ->
                   %% We test that the timeout can be set,
                   %% not that an timeout occurs after the appropriate delay
                   %% (48 days, 56 minutes, 48 seconds)!
                   timeout
         end.

receive_after_big2() ->
    Self = self(),
    erlang:yield(),
    spawn(fun() -> Self ! here_is_a_message end),
    ok = receive
	     here_is_a_message ->
		 ok
	 after 16#f7999977 ->
		 %% We only test that the timeout can be set.
		 timeout
	 end.

-define(TryAfter(Timeout), 
	{'EXIT',{timeout_value,_}} = (catch receive mission -> exit(impossible) after Timeout -> ok end),
	{'EXIT',{timeout_value,_}} = (catch receive after Timeout -> ok end),
	try_after(Timeout)).

%% Test error cases for 'receive after'.
receive_after_errors(Config) when is_list(Config) ->
    ?TryAfter(-1),
    ?TryAfter(0.0),
    ?TryAfter(3.14),
    ?TryAfter(16#100000000),
    ?TryAfter(392347129847294724972398472984729847129874),
    ?TryAfter(16#3fffffffffffffff),
    ?TryAfter(16#ffffffffffffffff),
    ?TryAfter(-16#100000000),
    ?TryAfter(-3891278094774921784123987129848),
    ?TryAfter(xxx),
    ok.

try_after(Timeout) ->
    {'EXIT',{timeout_value,_}} = (catch receive after Timeout -> ok end).

%% Test 'after Z', when Z == 0.
receive_var_zero(Config) when is_list(Config) ->
    self() ! x,
    self() ! y,
    Z = zero(),
    timeout = receive
                  z -> ok
              after Z -> timeout
              end,
    timeout = receive
              after Z -> timeout
              end,
    self() ! w,
    receive
	x -> ok;
	Other ->
	    ct:fail({bad_message,Other})
    end.

zero() -> 0.

%% Test 'after 0'.
receive_zero(Config) when is_list(Config) ->
    self() ! x,
    self() ! y,
    timeout = receive
                  z -> ok
              after 0 ->
                        timeout
              end,
    self() ! w,
    timeout = receive
              after 0 -> timeout
              end,
    receive
        x -> ok;
        Other ->
            ct:fail({bad_message,Other})
    end.

%% Test for catching invalid assertion in erl_message.c (in queue_message)
%% This failed (dumped core) with debug-compiled emulator.
multi_timeout(Config) when is_list(Config) ->
    P = spawn(?MODULE, timeout_g, []),
    P ! a,
    P ! b,
    receive
    after 1000 -> ok
    end,
    P ! c,
    receive
    after 1000 -> ok
    end,
    P ! d,
    ok.

timeout_g() ->
    receive
        a -> ok
    end,
    receive
    after 100000 -> ok
    end,
    ok.

%% OTP-7493: Timeout for 32 bit numbers (such as 16#ffffFFFF) could
%% timeout at once.
receive_after_32bit(Config) when is_list(Config) ->
    T = 16#ffffFFFF,
    Pids = [spawn_link(fun() -> recv_after_32bit(I, T) end) ||
	       I <- lists:seq(1, 2048)],

    %% Wait two seconds for any of the processes to timeout too early.
    receive after 2000 -> ok end,

    %% Kill the processes.
    [begin unlink(Pid), exit(Pid, kill) end || Pid <- Pids],
    ok.

recv_after_32bit(I, T) when I rem 2 =:= 0 ->
    receive after T -> exit(timeout) end;
recv_after_32bit(_, _) ->
    receive after 16#ffffFFFF -> exit(timeout) end.

blaster() ->
    receive
	{go, TimeoutTime} ->
	    Tmo = TimeoutTime - erlang:monotonic_time(milli_seconds),
	    receive after Tmo -> ok end
    end.

spawn_blasters(0) ->
    [];
spawn_blasters(N) ->
    [spawn_monitor(fun () -> blaster() end)|spawn_blasters(N-1)].

receive_after_blast(Config) when is_list(Config) ->
    PMs = spawn_blasters(10000),
    TimeoutTime = erlang:monotonic_time(milli_seconds) + 5000,
    lists:foreach(fun ({P, _}) -> P ! {go, TimeoutTime} end, PMs),
    lists:foreach(fun ({P, M}) ->
                          receive
                              {'DOWN', M, process, P, normal} ->
                                  ok
                          end
                  end, PMs).
