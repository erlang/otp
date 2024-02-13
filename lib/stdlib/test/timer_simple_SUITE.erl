%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
%%% Purpose : Test the timer module a simpler/faster test than timer_SUITE

-module(timer_simple_SUITE).

%% This test suite matches on opaque tref() return values from the *_after
%% and *_interval functions, namely {instant, _}, {send_local, _}, {once, _}
%% and {interval, _}.
%% If the implementation changes, the test suite has to be changed accordingly.

%% external
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
         init_per_group/2,end_per_group/2, 
         init_per_testcase/2,
         apply_after1/1,
         apply_after2/1,
         apply_after3/1,
         apply_after4/1,
         apply_after_invalid_args/1,
         send_after1/1,
         send_after2/1,
         send_after3/1,
         send_after4/1,
         send_after5/1,
         send_after6/1,
         send_after7/1,
         send_after_invalid_args/1,
         exit_after1/1,
         exit_after2/1,
         exit_after3/1,
         exit_after4/1,
         kill_after1/1,
         kill_after2/1,
         kill_after3/1,
         apply_interval1/1,
         apply_interval2/1,
         apply_interval_invalid_args/1,
         apply_repeatedly1/1,
         apply_repeatedly2/1,
         apply_repeatedly_invalid_args/1,
         send_interval1/1,
         send_interval2/1,
         send_interval3/1,
         send_interval4/1,
         send_interval5/1,
         send_interval_invalid_args/1,
         cancel1/1,
         cancel2/1,
         cancel3/1,
         cancel4/1,
         cancel5/1,
         cancel6/1,
         cancel_invalid_args/1,
         sleep1/1,
         sleep2/1,
         tc/1,
         unexpected1/1,
         unexpected2/1,
         unexpected3/1,
         nonexistent1/1,
         nonexistent2/1,
         timer_perf/1]).

%% internal
-export([forever/0,
         do_nrev/2,
         send/2,
         timer/4,
         timer/5]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,10}}].

all() ->
    [
        {group, apply_after},
        {group, send_after},
        {group, exit_after},
        {group, kill_after},
        {group, apply_interval},
        {group, apply_repeatedly},
        {group, send_interval},
        {group, cancel},
        {group, sleep},
        {group, misc}
    ].

groups() -> 
    [
        {
            apply_after,
            [],
            [
                apply_after1,
                apply_after2,
                apply_after3,
                apply_after4,
                apply_after_invalid_args
            ]
        },
        {
            send_after,
            [],
            [
                send_after1,
                send_after2,
                send_after3,
                send_after4,
                send_after5,
                send_after6,
                send_after7,
                send_after_invalid_args
            ]
        },
        {
            exit_after,
            [],
            [
                exit_after1,
                exit_after2,
                exit_after3,
                exit_after4
            ]
        },
        {
            kill_after,
            [],
            [
                kill_after1,
                kill_after2,
                kill_after3
            ]
        },
        {
            apply_interval,
            [],
            [
                apply_interval1,
                apply_interval2,
                apply_interval_invalid_args
            ]
        },
        {
            apply_repeatedly,
            [],
            [
                apply_repeatedly1,
                apply_repeatedly2,
                apply_repeatedly_invalid_args
            ]
        },
        {
            send_interval,
            [],
            [
                send_interval1,
                send_interval2,
                send_interval3,
                send_interval4,
                send_interval5,
                send_interval_invalid_args
            ]
        },
        {
            cancel,
            [],
            [
                cancel1,
                cancel2,
                cancel3,
                cancel4,
                cancel5,
                cancel6,
                cancel_invalid_args
            ]
        },
        {
            sleep,
            [],
            [
                sleep1,
                sleep2
            ]
        },
        {
            misc,
            [],
            [
                tc,
                unexpected1,
                unexpected2,
                unexpected3,
                nonexistent1,
                nonexistent2,
                timer_perf
            ]
        }
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_, Config) when is_list(Config) ->
    ok = timer:start(),
    Config.

%% Testing timer interface!!

%% Test of apply_after with time = 0, with sending of message.
apply_after1(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, {instant, _}} = timer:apply_after(0, ?MODULE, send, [self(), {Msg, 1}]),
    {ok, {instant, _}} = timer:apply_after(0, fun erlang:send/2, [self(), {Msg, 2}]),
    Self = self(),
    {ok, {instant, _}} = timer:apply_after(0, fun() -> Self ! {Msg, 3} end),
    ok = get_messes(1000, Msg, [1, 2, 3]).

%% Test of apply_after with time = 500, with sending of message.
apply_after2(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, {once, _}} = timer:apply_after(500, ?MODULE, send, [self(), {Msg, 1}]),
    {ok, {once, _}} = timer:apply_after(500, fun erlang:send/2, [self(), {Msg, 2}]),
    Self = self(),
    {ok, {once, _}} = timer:apply_after(500, fun() -> Self ! {Msg, 3} end),
    ok = get_messes(1000, Msg, [1, 2, 3]).

%% Test that a request starts the timer server if it is not running.
apply_after3(Config) when is_list(Config) ->
    ok = supervisor:terminate_child(kernel_sup, timer_server),
    Msg = make_ref(),
    timer:apply_after(100, erlang, send, [self(), Msg]),
    ok = get_mess(500, Msg),
    {timer_server, Pid, worker, [timer]} = lists:keyfind(timer_server, 1, supervisor:which_children(kernel_sup)),
    true = is_pid(Pid).

%% Test that a request starts the timer server if it is not running.
apply_after4(Config) when is_list(Config) ->
    ok = supervisor:terminate_child(kernel_sup, timer_server),
    ok = supervisor:delete_child(kernel_sup, timer_server),
    Msg = make_ref(),
    timer:apply_after(100, erlang, send, [self(), Msg]),
    ok = get_mess(500, Msg),
    {timer_server, Pid, worker, [timer]} = lists:keyfind(timer_server, 1, supervisor:which_children(kernel_sup)),
    true = is_pid(Pid).

%% Test that apply_after rejects invalid arguments.
apply_after_invalid_args(Config) when is_list(Config) ->
    {error, badarg} = timer:apply_after(-1, fun() -> ok end),
    {error, badarg} = timer:apply_after(0, foo),
    {error, badarg} = timer:apply_after(0, fun(_X) -> ok end),
    {error, badarg} = timer:apply_after(-1, fun(_X) -> ok end, [foo]),
    {error, badarg} = timer:apply_after(0, foo, []),
    {error, badarg} = timer:apply_after(0, fun(_X) -> ok end, []),
    {error, badarg} = timer:apply_after(0, fun(_X) -> ok end, [foo, bar]),
    {error, badarg} = timer:apply_after(0, fun(_X) -> ok end, foo),
    {error, badarg} = timer:apply_after(-1, foo, bar, []),
    {error, badarg} = timer:apply_after(0, "foo", bar, []),
    {error, badarg} = timer:apply_after(0, foo, "bar", []),
    {error, badarg} = timer:apply_after(0, foo, bar, baz),
    ok.

%% Test of send_after with time = 0.
send_after1(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, {instant, _}} = timer:send_after(0, Msg),
    ok = get_mess(1000, Msg).

%% Test of send_after with time = 0 using a registered name.
send_after2(Config) when is_list(Config) ->
    Msg = make_ref(),
    Name = register_name(self()),
    {ok, {instant, _}} = timer:send_after(0, Name, Msg),
    ok = get_mess(1000, Msg),
    unregister(Name).

%% Test of send_after with time = 0 using a registered name
%% and node.
send_after3(Config) when is_list(Config) ->
    Msg = make_ref(),
    Name = register_name(self()),
    {ok, {instant, _}} = timer:send_after(0, {Name, node()}, Msg),
    ok = get_mess(1000, Msg),
    unregister(Name).

%% Test of send_after with time = 500.
send_after4(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, {send_local, _}} = timer:send_after(500, self(), Msg),
    ok = get_mess(2000, Msg).

%% Test of send_after with time = 500, with receiver a registered
%% process. [OTP-2735]
send_after5(Config) when is_list(Config) ->
    Msg = make_ref(),
    Name = register_name(self()),
    {ok, {once, _}} = timer:send_after(500, Name, Msg),
    ok = get_mess(2000, Msg),
    unregister(Name).

%% Test of send_after with time = 500 using a registered process
%% and node.
send_after6(Config) when is_list(Config) ->
    Msg = make_ref(),
    Name = register_name(self()),
    {ok, {once, _}} = timer:send_after(500, {Name, node()}, Msg),
    ok = get_mess(2000, Msg),
    unregister(Name).

%% Test that send_after works if the destination is a registered
%% name which gets registered after the timer is started.
send_after7(Config) when is_list(Config) ->
    Msg = make_ref(),
    Name = make_name(),
    {ok, {once, _}} = timer:send_after(500, Name, Msg),
    register(Name, self()),
    ok = get_mess(2000, Msg),
    unregister(Name).

%% Test that send_after rejects invalid arguments.
send_after_invalid_args(Config) when is_list(Config) ->
    {error, badarg} = timer:send_after(-1, test),
    {error, badarg} = timer:send_after(-1, self(), test),
    {error, badarg} = timer:send_after(-1, ?MODULE, test),
    {error, badarg} = timer:send_after(0, "", test),
    ok.

%% Test of exit_after with time = 1000.
exit_after1(Config) when is_list(Config) ->
    Msg = make_ref(),
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, forever, []),
    {ok, {once, _}} = timer:exit_after(1000, Pid, Msg),
    ok = get_mess(5000, {'EXIT', Pid, Msg}).

%% Test of exit_after with time = 1000. The process to exit is the
%% name of a registered process.  [OTP-2735]
exit_after2(Config) when is_list(Config) ->
    Msg = make_ref(),
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, forever, []),
    Name = register_name(Pid),
    {ok, {once, _}} = timer:exit_after(1000, Name, Msg),
    ok = get_mess(2000, {'EXIT', Pid, Msg}).

%% Test of exit_after for sending an exit to self.
exit_after3(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Msg = make_ref(),
    Pid = spawn_link(
        fun () ->
            {ok, {once, _}} = timer:exit_after(1000, Msg),
            forever()
        end
    ),
    ok = get_mess(2000, {'EXIT', Pid, Msg}).

%% Test that using exit_after to a non-existent
%% process does not crash the timer server.
exit_after4(Config) when is_list(Config) ->
    Mon = monitor(process, timer_server),
    timer:exit_after(0, make_name(), make_ref()),
    receive
        {'DOWN', Mon, process, _, _} ->
            error(timer_server_crashed)
    after 1000 ->
        ok
    end.

%% Test of kill_after with time = 1000.
kill_after1(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, forever, []),
    {ok, {once, _}} = timer:kill_after(1000, Pid),
    ok = get_mess(2000, {'EXIT', Pid, killed}).

%% Test of kill_after with time = 1000. The process to exit is the
%% name of a registered process.  [OTP-2735]
kill_after2(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, forever, []),
    Name = register_name(Pid),
    {ok, {once, _}} = timer:kill_after(1000, Name),
    ok = get_mess(2000, {'EXIT', Pid, killed}).

%% Test of kill_after for self-killing.
kill_after3(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(
        fun () ->
            {ok, {once, _}} = timer:kill_after(1000),
            forever()
        end
    ),
    ok = get_mess(2000, {'EXIT', Pid, killed}).

%% Test of apply_interval by sending messages. Receive
%% 3 messages, cancel the timer, and check that we do
%% not get any more messages.
apply_interval1(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, Ref1} = timer:apply_interval(1000, ?MODULE, send,
                                      [self(), {Msg, 1}]),
    {ok, Ref2} = timer:apply_interval(1000, fun erlang:send/2,
				      [self(), {Msg, 2}]),
    Self = self(),
    {ok, Ref3} = timer:apply_interval(1000, fun() -> Self ! {Msg, 3} end),
    ok = get_messes(1500, Msg, [1, 2, 3], 3),
    {ok, cancel} = timer:cancel(Ref1),
    {ok, cancel} = timer:cancel(Ref2),
    {ok, cancel} = timer:cancel(Ref3),
    nor = get_messes(1000, Msg, [1, 2, 3]).

%% Test apply_interval with the execution time of the action
%% longer than the timer interval. The timer should not wait for
%% the action to complete, ie start another action while the
%% previously started action is still running.
apply_interval2(Config) when is_list(Config) ->
    Msg = make_ref(),
    Fn = fun(P, Idx) ->
             P ! {Msg, Idx},
	    receive after 1000 -> ok end
         end,
    {ok, Ref1} = timer:apply_interval(500, erlang, apply,
                                     [Fn, [self(), 1]]),
    {ok, Ref2} = timer:apply_interval(500, Fn, [self(), 2]),
    Self = self(),
    {ok, Ref3} = timer:apply_interval(500, fun() -> Fn(Self, 3) end),
    receive after 1800 -> ok end,
    {ok, cancel} = timer:cancel(Ref1),
    {ok, cancel} = timer:cancel(Ref2),
    {ok, cancel} = timer:cancel(Ref3),
    ok = get_messes(1000, Msg, [1, 2, 3], 3),
    nor = get_messes(1000, Msg, [1, 2, 3]).

%% Test that apply_interval rejects invalid arguments.
apply_interval_invalid_args(Config) when is_list(Config) ->
    {error, badarg} = timer:apply_interval(-1, fun() -> ok end),
    {error, badarg} = timer:apply_interval(0, foo),
    {error, badarg} = timer:apply_interval(0, fun(_X) -> ok end),
    {error, badarg} = timer:apply_interval(-1, fun(_X) -> ok end, [foo]),
    {error, badarg} = timer:apply_interval(0, foo, []),
    {error, badarg} = timer:apply_interval(0, fun(_X) -> ok end, []),
    {error, badarg} = timer:apply_interval(0, fun(_X) -> ok end, [foo, bar]),
    {error, badarg} = timer:apply_interval(0, fun(_X) -> ok end, foo),
    {error, badarg} = timer:apply_interval(-1, foo, bar, []),
    {error, badarg} = timer:apply_interval(0, "foo", bar, []),
    {error, badarg} = timer:apply_interval(0, foo, "bar", []),
    {error, badarg} = timer:apply_interval(0, foo, bar, baz),
    ok.

%% Test of apply_repeatedly by sending messages. Receive
%% 3 messages, cancel the timer, and check that we do
%% not get any more messages. In a case like this, ie where
%% the execution time of the action is shorter than the timer
%% interval, this should behave the same as apply_interval.
apply_repeatedly1(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, Ref1} = timer:apply_repeatedly(1000, ?MODULE, send,
                                        [self(), {Msg, 1}]),
    {ok, Ref2} = timer:apply_repeatedly(1000, fun erlang:send/2,
                                        [self(), {Msg, 2}]),
    Self = self(),
    {ok, Ref3} = timer:apply_repeatedly(1000, fun() -> Self ! {Msg, 3} end),
    ok = get_messes(1500, Msg, [1, 2, 3], 3),
    {ok, cancel} = timer:cancel(Ref1),
    {ok, cancel} = timer:cancel(Ref2),
    {ok, cancel} = timer:cancel(Ref3),
    nor = get_messes(1000, Msg, [1, 2, 3]).

%% Test apply_repeatedly with the execution time of the action
%% longer than the timer interval. The timer should wait for
%% the action to complete, ie not start another action until it
%% has completed.
apply_repeatedly2(Config) when is_list(Config) ->
    Msg = make_ref(),
    Fn = fun(P, I) ->
             P ! {Msg, I},
             receive after 1000 -> ok end
         end,
    Self = self(),
    {ok, Ref1} = timer:apply_repeatedly(1, erlang, apply,
                                        [Fn, [self(), 1]]),
    {ok, Ref2} = timer:apply_repeatedly(1, Fn, [self(), 2]),
    Self = self(),
    {ok, Ref3} = timer:apply_repeatedly(1, fun() -> Fn(Self, 3) end),
    receive after 2500 -> ok end,
    {ok, cancel} = timer:cancel(Ref1),
    {ok, cancel} = timer:cancel(Ref2),
    {ok, cancel} = timer:cancel(Ref3),
    ok = get_messes(1000, Msg, [1, 2, 3], 3),
    nor = get_messes(1000, Msg, [1, 2, 3]).

%% Test that apply_repeatedly rejects invalid arguments.
apply_repeatedly_invalid_args(Config) when is_list(Config) ->
    {error, badarg} = timer:apply_repeatedly(-1, fun() -> ok end),
    {error, badarg} = timer:apply_repeatedly(0, foo),
    {error, badarg} = timer:apply_repeatedly(0, fun(_X) -> ok end),
    {error, badarg} = timer:apply_repeatedly(-1, fun(_X) -> ok end, [foo]),
    {error, badarg} = timer:apply_repeatedly(0, foo, []),
    {error, badarg} = timer:apply_repeatedly(0, fun(_X) -> ok end, []),
    {error, badarg} = timer:apply_repeatedly(0, fun(_X) -> ok end, [foo, bar]),
    {error, badarg} = timer:apply_repeatedly(0, fun(_X) -> ok end, foo),
    {error, badarg} = timer:apply_repeatedly(-1, foo, bar, []),
    {error, badarg} = timer:apply_repeatedly(0, "foo", bar, []),
    {error, badarg} = timer:apply_repeatedly(0, foo, "bar", []),
    {error, badarg} = timer:apply_repeatedly(0, foo, bar, baz),
    ok.

%% Test of send_interval/2. Receive 5 messages, cancel the timer, and
%% check that we do not get any more messages.
send_interval1(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, Ref} = timer:send_interval(1000, Msg),
    ok = get_mess(1500, Msg, 5),
    {ok, cancel} = timer:cancel(Ref),
    nor = get_mess(1000, Msg). % We should receive only five

%% Test of send_interval/3. Receive 2 messages, cancel the timer, and
%% check that we do not get any more messages.
send_interval2(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, Ref} = timer:send_interval(1000, self(), Msg),
    ok = get_mess(1500, Msg, 2),
    {ok, cancel} = timer:cancel(Ref),
    nor = get_mess(1000, Msg).  % We should receive only two

%% Test of send_interval/3. Receive 2 messages, cancel the timer, and
%% check that we do not get any more messages. The receiver is the
%% name of a registered process. [OTP-2735]
send_interval3(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Msg = make_ref(),
    Name = register_name(self()),
    {ok, Ref} = timer:send_interval(1000, Name, Msg),
    ok = get_mess(1500, Msg, 2),
    {ok, cancel} = timer:cancel(Ref),
    nor = get_mess(1000, Msg),  % We should receive only two
    unregister(Name).

%% Test of send_interval/3 using a registered name and node.
send_interval4(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Msg = make_ref(),
    Name = register_name(self()),
    {ok, Ref} = timer:send_interval(1000, {Name, node()}, Msg),
    ok = get_mess(1500, Msg, 2),
    {ok, cancel} = timer:cancel(Ref),
    nor = get_mess(1000, Msg), % We should receive only two
    unregister(Name).

%% Test that send interval stops sending msg when the receiving
%% process terminates.
send_interval5(Config) when is_list(Config) ->
    Msg1 = make_ref(),
    {ok, {interval, Ref}} = timer:send_interval(500, Msg1),
    receive 
        Msg1 -> ok
    end,
    timer_server ! {'DOWN', Ref, process, self(), test},
    Msg2 = make_ref(),
    {ok, {send_local, _}} = timer:send_after(600, Msg2),
    Msg2 = receive
        TmpMsg -> TmpMsg
    end.

%% Test that send_interval rejects invalid arguments.
send_interval_invalid_args(Config) when is_list(Config) ->
    {error, badarg} = timer:send_interval(-1, test),
    {error, badarg} = timer:send_interval(-1, self(), test),
    {error, badarg} = timer:send_interval(-1, ?MODULE, test),
    {error, badarg} = timer:send_interval(0, "", test),
    ok.

%% Test that we can cancel an instant timer
cancel1(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, Ref} = timer:send_after(0, Msg),
    {ok, cancel} = timer:cancel(Ref),
    ok = get_mess(0, Msg). % We should rec 1 msg as it got sent immediately

%% Test that we can cancel a send-once timer.
cancel2(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, Ref} = timer:send_after(1000, Msg),
    {ok, cancel} = timer:cancel(Ref),
    nor = get_mess(2000, Msg). % We should rec 0 msgs

%% Test that we can cancel an apply-once timer.
cancel3(Config) when is_list(Config) ->
    Msg = make_ref(),
    {ok, Ref} = timer:apply_after(1000, erlang, send, [self(), Msg]),
    {ok, cancel} = timer:cancel(Ref),
    nor = get_mess(2000, Msg).

%% Test that we can cancel a send-interval timer.
cancel4(Config) when is_list(Config) ->
    Msg1 = make_ref(),
    {ok, Ref} = timer:send_interval(500, Msg1),
    receive
        Msg1 -> ok
    end,
    {ok, cancel} = timer:cancel(Ref),
    Msg2 = make_ref(),
    {ok, {send_local, _}} = timer:send_after(600, Msg2),
    Msg2 = receive
        TmpMsg -> TmpMsg
    end.

%% Test that we can cancel an apply-interval timer.
cancel5(Config) when is_list(Config) ->
    Msg1 = make_ref(),
    {ok, Ref} = timer:apply_interval(500, erlang, send, [self(), Msg1]),
    receive
        Msg1 -> ok
    end,
    {ok, cancel} = timer:cancel(Ref),
    Msg2 = make_ref(),
    {ok, {send_local, _}} = timer:send_after(600, Msg2),
    Msg2 = receive
        TmpMsg -> TmpMsg
    end.

%% Test that cancelling non-existent timers does not crash the
%% timer server. 
cancel6(Config) when is_list(Config) ->
    lists:foreach(
        fun (TimerType) ->
            Mon = monitor(process, timer_server),
	    {ok, cancel} = timer:cancel({TimerType, make_ref()}),
            receive
                {'DOWN', Mon, process, _, _} ->
                    error({timer_server_crashed, {cancel, TimerType}})
            after 500 ->
                ok
            end,
	    demonitor(Mon)
        end,
	[once, instant, interval, send_local]
    ).

%% Test that cancel rejects invalid arguments.
cancel_invalid_args(Config) when is_list(Config) ->
    {error, badarg} = timer:cancel(no_reference),
    {error, badarg} = timer:cancel({foo, make_ref()}),
    {error, badarg} = timer:cancel({once, foo}),
    {error, badarg} = timer:cancel({interval, foo}),
    {error, badarg} = timer:cancel({instant, foo}),
    ok.

%% Test that sleep pauses the calling process for
%% at least the given time.
sleep1(Config) when is_list(Config) ->
    T0 = erlang:monotonic_time(millisecond),
    ok = timer:sleep(1000),
    T1 = erlang:monotonic_time(millisecond),
    true = T1 - T0 >= 1000,
    ok.

%% Test that sleep accepts times >(2^32)-1, which is
%% the maximum time for the after clause of a receive
%% operation, at the time of this writing.
sleep2(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(
        fun () ->
            {ok, {once, _}} = timer:kill_after(1000),
            ok = timer:sleep(16#ffffffff+1)
        end
    ),
    ok = get_mess(2000, {'EXIT', Pid, killed}).

%% Test that unexpected calls do not crash the timer server.
unexpected1(Config) when is_list(Config) ->
    Mon = monitor(process, timer_server),
    try
        gen_server:call(timer_server, foo, 100)
    of
        _ ->
            error(timeout_expected)
    catch
        exit:{timeout, _} ->
            ok
    end,
    receive
        {'DOWN', Mon, process, _, _} ->
            error(timer_server_crashed)
    after 500 ->
        ok
    end,
    demonitor(Mon).

%% Test that unexpected casts do not crash the timer server.
unexpected2(Config) when is_list(Config) ->
    Mon = monitor(process, timer_server),
    gen_server:cast(timer_server, foo),
    receive
        {'DOWN', Mon, process, _, _} ->
            error(timer_server_crashed)
    after 500 ->
        ok
    end,
    demonitor(Mon).

%% Test that unexpected info messages do not crash the timer server.
unexpected3(Config) when is_list(Config) ->
    Mon = monitor(process, timer_server),
    timer_server ! foo,
    receive
        {'DOWN', Mon, process, _, _} ->
            error(timer_server_crashed)
    after 500 ->
        ok
    end,
    demonitor(Mon).

%% Test that timeouts of one-shot timers the timer server does not
%% know are not executed.
nonexistent1(Config) when is_list(Config) ->
    Msg = make_ref(),
    timer_server ! {timeout, make_ref(), {apply_once, {erlang, send, [self(), Msg]}}},
    nor = get_mess(1000, Msg).

%% Test that timeouts of interval timers the timer server does not
%% know are not executed.
nonexistent2(Config) when is_list(Config) ->
    Msg = make_ref(),
    timer_server ! {timeout, make_ref, {apply_interval, erlang:monotonic_time(millisecond), 1000, make_ref(), {erlang, send, [self(), Msg]}}},
    nor = get_mess(1000, Msg).

%% Test sleep/1 and tc/3.
tc(Config) when is_list(Config) ->
    %% This should test both sleep and tc/3
    {Res1, ok} = timer:tc(timer, sleep, [500]),
    ok = if
             Res1 < 500*1000 -> {too_early, Res1}; % Too early
             Res1 > 800*1000 -> {too_late, Res1};  % Too much time
             true -> ok
         end,

    %% tc/2
    {Res2, ok} = timer:tc(fun(T) -> ok = timer:sleep(T) end, [500]),
    ok = if
             Res2 < 500*1000 -> {too_early, Res2}; % Too early
             Res2 > 800*1000 -> {too_late, Res2};  % Too much time
             true -> ok
         end,

    %% tc/1
    {Res3, ok} = timer:tc(fun() -> ok = timer:sleep(500) end),
    ok = if
             Res3 < 500*1000 -> {too_early, Res3}; % Too early
             Res3 > 800*1000 -> {too_late, Res3};  % Too much time
             true -> ok
         end,

    %% tc/4
    {Res4, ok} = timer:tc(timer, sleep, [500], millisecond),
    ok = if
             Res4 < 500 -> {too_early, Res4};
             Res4 > 800 -> {too_late, Res4};
             true -> ok
         end,

    %% Check that timer:tc don't catch errors
    ok = try timer:tc(erlang, exit, [foo], second)
         catch exit:foo -> ok
         end,

    ok = try timer:tc(erlang, exit, [foo])
         catch exit:foo -> ok
         end,

    ok = try timer:tc(fun(Reason) -> 1 = Reason end, [foo])
         catch error:{badmatch,_} -> ok
         end,

    ok = try timer:tc(fun() -> throw(foo) end)
         catch foo -> ok
         end,

    %% Check that return values are propageted
    Self = self(),
    {_, Self} = timer:tc(erlang, self, [], second),
    {_, Self} = timer:tc(erlang, self, []),
    {_, Self} = timer:tc(fun(P) -> P end, [self()]),
    {_, Self} = timer:tc(fun() -> self() end),

    Sec = timer:seconds(4),
    Min = timer:minutes(4),
    Hour = timer:hours(4),
    MyRes = 4*1000 + 4*60*1000 + 4*60*60*1000,
    if  MyRes == Sec + Min + Hour -> ok end,
    TimerRes = timer:hms(4,4,4),
    if MyRes == TimerRes -> ok end,
    ok.

get_mess(Time, Mess) -> get_mess(Time, Mess, 1).
get_mess(_, _, 0) -> ok;  % Received
get_mess(Time, Mess, N) ->
    receive 
        Mess -> get_mess(Time, Mess, N-1)
    after Time ->
        nor   % Not Received
    end.

get_messes(Time, Mess, Indexes) -> get_messes(Time, Mess, Indexes, 1).
get_messes(Time, Mess, Indexes, N) -> get_messes1(Time, Mess, lists:append(lists:duplicate(N, Indexes))).
get_messes1(_, _, []) -> ok;
get_messes1(Time, Mess, Indexes) ->
    receive
        {Mess, Index} -> get_messes1(Time, Mess, lists:delete(Index, Indexes))
    after Time ->
        nor
    end.

forever() ->
    ok = timer:sleep(1000),
    forever().


%%
%% Testing for performance (on different implementations) of timers
%%


timer_perf(Config) when is_list(Config) ->
    performance(timer).

performance(Mod) ->
    process_flag(trap_exit, true),    
    {Y,Mo,D} = date(),
    {H,M,S} = time(),
    io:format("Testing module '~p' Date: ~w/~w/~w ~w:~w:~w~n", 
              [Mod,Y,Mo,D,H,M,S]),
    Result = big_test(Mod),
    report_result(Result).

big_test(M) ->
    Load_Pids = start_nrev(20, M),   % Increase if more load wanted :)

    LPids = spawn_timers(5, M, 10000, 5),

    apply(M, sleep, [4000]),
    MPids = spawn_timers(10, M, 1000, 6),

    apply(M, sleep, [3500]),
    SPids = spawn_timers(15, M, 100, 3),

    Res = wait(SPids ++ MPids ++ LPids, [], 0, M),

    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Load_Pids),
    Res.

wait([], Res, N, _) ->
    {Res, N};
wait(Pids, ResList, N, M) ->
    receive
        {Pid, ok, Res, T} ->
            wait(lists:delete(Pid, Pids), [{T, Res} | ResList], N, M);
        {Pid, Error}->
            ct:fail(Error),
            wait(lists:delete(Pid, Pids), ResList, N+1, M);
        {'EXIT', Pid, normal} ->
            wait(lists:delete(Pid, Pids), ResList, N, M);
        {'EXIT', Pid, Reason} ->
            ct:fail({Pid,Reason})
    end.

spawn_timers(0, _, _, _) ->
    [];
spawn_timers(N, M, T, NumIter) ->
    apply(M, sleep, [120*N]),
    Pid1 = spawn_link(?MODULE, timer, [apply, M, T, self()]),
    Pid2 = spawn_link(?MODULE, timer, [interval, M, T, self(), NumIter]),
    [Pid1, Pid2 | spawn_timers(N-1, M, T, NumIter)].

timer(apply, Mod, T, Pid) ->
    Before = system_time(),
    {ok, Ref} = apply(Mod, apply_after, [T, ?MODULE, send, [self(), done]]),
    receive 
        done ->
            After = system_time(),
            Pid ! {self(), ok, (After-Before) div 1000, T}
    after T*3 + 300 ->   % Watch dog
            io:format("WARNING TIMER WATCHDOG timed out: ~w ~n", [T]),
            {ok, cancel} = timer:cancel(Ref),
            Pid ! {self(), watch_dog_timed_out}
    end.

timer(interval, Mod, T, Pid, NumIter) ->
    Before = system_time(),
    {ok, Ref} = apply(Mod, apply_interval, [T, ?MODULE, send, [self(), done]]),
    timer_irec(Before, T, {0, NumIter}, [], {Pid, Mod, Ref}).

timer_irec(_Start, T, {N, N}, Res, {Pid, Mod, Ref}) ->
    apply(Mod, cancel, [Ref]),
    Min = lists:min(Res),
    Max = lists:max(Res),
    Tot = lists:sum(Res),
    Pid ! {self(), ok, {N, Tot, Tot div N, Min, Max}, T};
timer_irec(Start, T, {N, Max}, Res, {Pid, Mod, Ref}) ->
    receive
        done ->
            Now = system_time(),
            Elapsed = (Now - (Start + (N*T*1000))) div 1000,
            timer_irec(Start, T,
                       {N+1, Max},
                       [Elapsed | Res],
                       {Pid, Mod, Ref})
    after T*3 + 300 ->
            apply(Mod, cancel, [Ref]),
            io:format("WARNING: TIMER WATCHDOG timed out <Interval>~w~n",[T]),
            Pid ! {self(), timer_watchdog_timed_out_in_interlval_test}  
    end.

%% ------------------------------------------------------- %%
%%  Small last generator

start_nrev(0, _) ->
    [];

start_nrev(N, M) ->
    Pid = spawn_link(?MODULE, do_nrev, [N, M]),
    [Pid | start_nrev(N-1, M)].

do_nrev(Sleep, Mod) ->
    apply(Mod, sleep, [50 * Sleep]),
    test(1000,"abcdefghijklmnopqrstuvxyz1234"),
    ok.

test(0,_) ->
    true;
test(N,L) ->
    nrev(L),
    test(N - 1, L).

nrev([]) ->
    [];
nrev([H|T]) ->
    append(nrev(T), [H]).

append([H|T],Z) ->
    [H|append(T,Z)];
append([],X) ->
    X.

system_time() ->    
    erlang:monotonic_time(microsecond).

%% ------------------------------------------------------- %%

report_result({Res, 0}) ->
    {A_List, I_List} = split_list(Res, [], []),
    A_val = calc_a_val(A_List),
    I_val = calc_i_val(I_List),
    print_report(A_val, I_val),
    ok;

report_result({Head, N}) ->
    io:format("Test Failed: Number of internal tmo ~w~n", [N]),
    ct:fail({Head, N}).

split_list([], AL, IL) ->
    {AL, IL};
split_list([{T, {N, Tot, A, Min, Max}} | Rest], AL, IL) ->
    split_list(Rest, AL, [{T, {N, Tot, A, Min, Max}} | IL]);
split_list([Head | Rest], AL, IL) ->
    split_list(Rest, [Head | AL], IL).

split([{T, Res} | R]) ->
    split(R, {{T,[Res]}, {T*10,[]}, {T*100,[]}}).

split([{T, Res} | R], {{T,S}, M, L}) ->
    split(R, {{T,[Res|S]}, M, L});

split([{T, Res} | R], {S, {T,M}, L}) ->
    split(R, {S, {T, [Res|M]}, L});

split([{T, Res} | R], {S, M, {T,L}}) ->
    split(R, {S, M, {T, [Res|L]}});

split(_Done, Vals) ->
    Vals.

calc_a_val(List) ->
    New = lists:sort(List),
    {{T1, S}, {T2, M}, {T3, L}} = split(New),
    S2 = {length(S), lists:max(S), lists:min(S), 
          lists:sum(S) div length(S)},
    M2 = {length(M), lists:max(M), lists:min(M), 
          lists:sum(M) div length(M)},
    L2 = {length(L), lists:max(L), lists:min(L), 
          lists:sum(L) div length(L)},
    [{T1, S2}, {T2, M2}, {T3, L2}].

calc_i_val(List) ->
    New =  lists:sort(List),
    {{T1, S}, {T2, M}, {T3, L}} = split(New),
    S2 = get_ivals(S),
    M2 = get_ivals(M),
    L2 = get_ivals(L),
    [{T1, S2}, {T2, M2}, {T3, L2}].

get_ivals(List) ->
    Len = length(List),
    Num = element(1, hd(List)), % Number of iterations

    LTot = lists:map(fun(X) -> element(2, X) end, List),
    LMin = lists:map(fun(X) -> element(4, X) end, List),
    LMax = lists:map(fun(X) -> element(5, X) end, List),

    MaxTot  = lists:max(LTot),
    MinTot  = lists:min(LTot),
    AverTot = lists:sum(LTot) div Len,

    IterMax = lists:max(LMax),
    IterMin = lists:min(LMin),
    IterAver= AverTot div Num,

    {Len, Num,
     {MaxTot, MinTot, AverTot}, 
     {IterMax, IterMin, IterAver}}.


print_report(A_L, I_L) ->
    io:format("~nRESULTS from timer test~n~n",[]),
    io:format("Time out times for send_after~n~n", []),
    io:format("Time No of tests  Max    Min  Average~n",[]),
    print_aval(A_L),
    io:format("Time out times for send_interval~n~n", []),
    io:format("Time No.tests  No.intvals TotMax TotMin TotAver  MaxI   MinI  AverI~n", []),
    print_ival(I_L).

print_aval([]) ->
    io:format("~n~n", []);
print_aval([{T, {L, Max, Min, Aver}}|R]) ->
    io:format("~5w ~8w ~6w ~6w ~8w ~n", 
              [T,L,Max,Min,Aver]),
    print_aval(R).

print_ival([]) ->
    io:format("~n", []);
print_ival([{T, {Len, Num, 
                 {MaxT, MinT, AverT},
                 {MaxI, MinI, AverI}}}|R]) ->
    io:format("~5w ~6w ~10w ~8w ~6w ~6w ~6w ~6w ~6w~n", 
              [T,Len,Num,MaxT,MinT,AverT, MaxI, MinI, AverI]),
    print_ival(R).

send(Pid, Msg) ->
    Pid ! Msg.

%% Create a unique name and register it to the given process.
register_name(Pid) ->
    Name = make_name(),
    register(Name, Pid),
    Name.

%% Create a unique name.
make_name() ->
    list_to_atom(ref_to_list(make_ref())).
