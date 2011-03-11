%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(after_SUITE).

%% Tests receive after.

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 t_after/1, receive_after/1, receive_after_big/1,
	 receive_after_errors/1, receive_var_zero/1, receive_zero/1,
	 multi_timeout/1, receive_after_32bit/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

%% Internal exports.

-export([timeout_g/0]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [t_after, receive_after, receive_after_big,
     receive_after_errors, receive_var_zero, receive_zero,
     multi_timeout, receive_after_32bit].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(3)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

%% Tests for an old round-off error in 'receive after'."
t_after(Config) when is_list(Config) ->
    ?line spawn(fun frequent_process/0),
    ?line Period = test_server:minutes(1),
    ?line Before = erlang:now(),
    receive
	after Period ->
		?line After = erlang:now(),
		?line report(Period, Before, After)
	end.


report(Period, Before, After) ->
    ?line Elapsed = (element(1, After)*1000000000
		     +element(2, After)*1000
		     +element(3, After) div 1000) -
	(element(1,Before)*1000000000
	 + element(2,Before)*1000 + element(3,Before) div 1000),
    ?line case Elapsed*100 / Period of
	      Percent when Percent > 100.10 ->
		  ?line test_server:fail({too_inaccurate, Percent});
	      Percent when Percent < 100.0 ->
		  ?line test_server:fail({too_early, Percent});
	      Percent ->
		  ?line Comment = io_lib:format("Elapsed/expected: ~.2f %",
						[Percent]),
		  {comment, lists:flatten(Comment)}
	  end.

frequent_process() ->
    receive
	after 100 ->
		?line frequent_process()
	end.

receive_after(doc) ->
    "Test that 'receive after' works (doesn't hang). "
    "The test takes 10 seconds to complete.";
receive_after(Config) when is_list(Config) ->
    ?line receive_after1(5000).

receive_after1(1) ->
    ?line io:format("Testing: receive after ~p~n", [1]), 
    ?line receive after 1 -> ok end;
receive_after1(N) -> 
    ?line io:format("Testing: receive after ~p~n", [N]), 
    ?line receive after N -> receive_after1(N div 2) end.

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
    ?line ?TryAfter(-1),
    ?line ?TryAfter(0.0),
    ?line ?TryAfter(3.14),
    ?line ?TryAfter(16#100000000),
    ?line ?TryAfter(392347129847294724972398472984729847129874),
    ?line ?TryAfter(16#3fffffffffffffff),
    ?line ?TryAfter(16#ffffffffffffffff),
    ?line ?TryAfter(-16#100000000),
    ?line ?TryAfter(-3891278094774921784123987129848),
    ?line ?TryAfter(xxx),
    ok.

try_after(Timeout) ->
    {'EXIT',{timeout_value,_}} = (catch receive after Timeout -> ok end).

receive_var_zero(doc) -> "Test 'after Z', when Z == 0.";
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
	    ?line ?t:fail({bad_message,Other})
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
	    ?line ?t:fail({bad_message,Other})
    end.

multi_timeout(doc) ->
    "Test for catching invalid assertion in erl_message.c (in queue_message)."
    "This failed (dumped core) with debug-compiled emulator.";
multi_timeout(Config) when is_list(Config) ->
    ?line P = spawn(?MODULE, timeout_g, []),
    ?line P ! a,
    ?line P ! b,
    ?line receive
	  after 1000 -> ok
	  end,
    ?line P ! c,
    ?line receive
	  after 1000 -> ok
	  end,
    ?line P ! d,
    ok.

timeout_g() ->
    ?line receive
	a -> ok
    end,
    ?line receive
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
		    
