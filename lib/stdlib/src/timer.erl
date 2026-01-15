%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-module(timer).
-moduledoc """
Timer functions.

This module provides useful functions related to time. Unless otherwise stated,
time is always measured in _milliseconds_. All timer functions return
immediately, regardless of work done by another process.

Successful evaluations of the timer functions give return values containing a
timer reference, denoted `TRef`. By using `cancel/1`, the returned reference can
be used to cancel any requested action. A `TRef` is an Erlang term, which
contents must not be changed.

The time-outs are not exact, but are _at least_ as long as requested.

Creating timers using `erlang:send_after/3` and `erlang:start_timer/3` is more
efficient than using the timers provided by this module. However, the timer
module has been improved in OTP 25, making it more efficient and less
susceptible to being overloaded. See
[the Timer Module section in the Efficiency Guide](`e:system:commoncaveats.md#timer-module`).

For more information on timers in Erlang in general, see the
[*Timers*](`e:erts:time_correction.md#timers`) section of the
[*Time and Time Correction in Erlang*](`e:erts:time_correction.md`)
ERTS User's guide.

## Examples

_Example 1_

The following example shows how to print "Hello World\!" in 5 seconds:

```erlang
1> timer:apply_after(5000, io, format, ["~nHello World!~n", []]).
{ok,TRef}
Hello World!
```

_Example 2_

The following example shows a process performing a certain action, and if this
action is not completed within a certain limit, the process is killed:

```erlang
Pid = spawn(mod, fun, [foo, bar]),
%% If pid is not finished in 10 seconds, kill him
{ok, R} = timer:kill_after(timer:seconds(10), Pid),
...
%% We change our mind...
timer:cancel(R),
...
```

## Notes

A timer can always be removed by calling `cancel/1`.

An interval timer, that is, a timer created by evaluating any of the functions
`apply_interval/2`, `apply_interval/3`, `apply_interval/4`,
`apply_repeatedly/2`, `apply_repeatedly/3`, `apply_repeatedly/4`,
`send_interval/2`, and `send_interval/3` is linked to the process to which the
timer performs its task.

A one-shot timer, that is, a timer created by evaluating any of the functions
`apply_after/2`, `apply_after/3`, `apply_after/4`, `send_after/2`,
`send_after/3`, `exit_after/2`, `exit_after/3`, `kill_after/1`, and
`kill_after/2` is not linked to any process. Hence, such a timer is removed only
when it reaches its time-out, or if it is explicitly removed by a call to
`cancel/1`.

The functions given to `apply_after/2`, `apply_after/3`, `apply_interval/2`,
`apply_interval/3`, `apply_repeatedly/2`, and `apply_repeatedly/3`, or denoted
by `Module`, `Function` and `Arguments` given to `apply_after/4`,
`apply_interval/4`, and `apply_repeatedly/4` are executed in a freshly-spawned
process, and therefore calls to `self/0` in those functions will return the Pid
of this process, which is different from the process that called
`timer:apply_*`.

_Example_

In the following example, the intention is to set a timer to execute a function
after 1 second, which performs a fictional task, and then wants to inform the
process which set the timer about its completion, by sending it a `done`
message.

Using `self/0` _inside_ the timed function, the code below does not work as
intended. The task gets done, but the `done` message gets sent to the wrong
process and is lost.

```erlang
1> timer:apply_after(1000, fun() -> do_something(), self() ! done end).
{ok,TRef}
2> receive done -> done after 5000 -> timeout end.
%% ... 5s pass...
timeout
```

The code below calls `self/0` in the process which sets the timer and assigns it
to a variable, which is then used in the function to send the `done` message to,
and so works as intended.

```erlang
1> Target = self()
<0.82.0>
2> timer:apply_after(1000, fun() -> do_something(), Target ! done end).
{ok,TRef}
3> receive done -> done after 5000 -> timeout end.
%% ... 1s passes...
done
```

Another option is to pass the message target as a parameter to the function.

```erlang
1> timer:apply_after(1000, fun(Target) -> do_something(), Target ! done end, [self()]).
{ok,TRef}
2> receive done -> done after 5000 -> timeout end.
%% ... 1s passes...
done
```
""".

-export([apply_after/2, apply_after/3, apply_after/4,
         send_after/3, send_after/2,
         exit_after/3, exit_after/2, kill_after/2, kill_after/1,
         apply_interval/2, apply_interval/3, apply_interval/4,
	 apply_repeatedly/2, apply_repeatedly/3, apply_repeatedly/4,
         send_interval/3, send_interval/2,
         cancel/1, sleep/1, tc/1, tc/2, tc/3, tc/4, now_diff/2,
         seconds/1, minutes/1, hours/1, hms/3]).

-export([start_link/0, start/0,
         handle_call/3,  handle_info/2,
         init/1,
         code_change/3, handle_cast/2, terminate/2]).

%% Types which can be used by other modules
-export_type([tref/0, time/0]).

%% Max value for a receive's after clause.
-define(MAX_RECEIVE_AFTER, 16#ffffffff).

%% Validations
-define(valid_time(T), is_integer(T), T >= 0).
-define(valid_apply(F), is_function(F, 0)).
-define(valid_apply(F, A), is_list(A), is_function(F, length(A))).
-define(valid_apply(M, F, A), is_atom(M), is_atom(F), is_list(A)).

%%
%% Time is in milliseconds.
%%
-doc "A timer reference.".
-opaque tref() :: {type(), reference()}.
-type type()   :: 'once' | 'interval' | 'instant' | 'send_local'.

-doc "Time in milliseconds.".
-doc #{since => ~"OTP 28.0"}.
-nominal time() :: non_neg_integer().

%%
%% Interface functions
%%
-doc """
Evaluates [`spawn(erlang, apply, [Function, []])`](`spawn/3`) after `Time`
milliseconds.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec apply_after(Time, Function) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Function :: fun(() -> _),
                   TRef :: tref(),
                   Reason :: term().
apply_after(Time, F)
  when ?valid_apply(F) ->
    apply_after(Time, erlang, apply, [F, []]);
apply_after(_Time, _F) ->
    {error, badarg}.

-doc """
Evaluates [`spawn(erlang, apply, [Function, Arguments])`](`spawn/3`) after
`Time` milliseconds.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec apply_after(Time, Function, Arguments) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Function :: fun((...) -> _),
                   Arguments :: [term()],
                   TRef :: tref(),
                   Reason :: term().
apply_after(Time, F, A)
  when ?valid_apply(F, A) ->
    apply_after(Time, erlang, apply, [F, A]);
apply_after(_Time, _F, _A) ->
    {error, badarg}.

-doc """
Evaluates [`spawn(Module, Function, Arguments)`](`spawn/3`) after `Time`
milliseconds.
""".
-spec apply_after(Time, Module, Function, Arguments) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Module :: module(),
                   Function :: atom(),
                   Arguments :: [term()],
                   TRef :: tref(),
                   Reason :: term().
apply_after(0, M, F, A)
  when ?valid_apply(M, F, A) ->
    _ = do_apply({M, F, A}, false),
    {ok, {instant, make_ref()}};
apply_after(Time, M, F, A)
  when ?valid_time(Time),
       ?valid_apply(M, F, A) ->
    req(apply_once, {system_time(), Time, {M, F, A}});
apply_after(_Time, _M, _F, _A) ->
    {error, badarg}.

-doc """
Evaluates `Destination ! Message` after `Time` milliseconds.

`Destination` can be a remote or local process identifier, an atom of a
registered name or a tuple `{RegName, Node}` for a registered name at another node.

See also [the Timer Module section in the Efficiency Guide](`e:system:commoncaveats.md#timer-module`).
""".
-spec send_after(Time, Destination, Message) -> {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Destination :: pid() | (RegName :: atom()) | {RegName :: atom(), Node :: node()},
                   Message :: term(),
                   TRef :: tref(),
                   Reason :: term().
send_after(0, PidOrRegName, Message)
  when is_pid(PidOrRegName);
       is_atom(PidOrRegName) ->
    PidOrRegName ! Message,
    {ok, {instant, make_ref()}};
send_after(0, {RegName, Node} = Dest, Message)
  when is_atom(RegName),
       is_atom(Node) ->
    Dest ! Message,
    {ok, {instant, make_ref()}};
send_after(Time, Pid, Message)
  when ?valid_time(Time),
       is_pid(Pid),
       node(Pid) =:= node() ->
    TRef = erlang:send_after(Time, Pid, Message),
    {ok, {send_local, TRef}};
send_after(Time, Pid, Message)
  when is_pid(Pid) ->
    apply_after(Time, ?MODULE, send, [Pid, Message]);
send_after(Time, RegName, Message)
  when is_atom(RegName) ->
    apply_after(Time, ?MODULE, send, [RegName, Message]);
send_after(Time, {RegName, Node} = Dest, Message)
  when is_atom(RegName),
       is_atom(Node) ->
    apply_after(Time, ?MODULE, send, [Dest, Message]);
send_after(_Time, _PidOrRegName, _Message) ->
    {error, badarg}.

-doc(#{equiv => send_after(Time, self(), Message)}).
-spec send_after(Time, Message) -> {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Message :: term(),
                   TRef :: tref(),
                   Reason :: term().
send_after(Time, Message) ->
    send_after(Time, self(), Message).

-doc """
Sends an exit signal with reason `Reason1` to `Target`, which can be a local
process identifier or an atom of a registered name.
""".
-spec exit_after(Time, Target, Reason1) -> {'ok', TRef} | {'error', Reason2}
              when Time :: time(),
                   Target :: pid() | (RegName :: atom()),
                   TRef :: tref(),
                   Reason1 :: term(),
                   Reason2 :: term().
exit_after(Time, Pid, Reason) ->
    apply_after(Time, erlang, exit, [Pid, Reason]).

-doc(#{equiv => exit_after(Time, self(), Reason)}).
-spec exit_after(Time, Reason1) -> {'ok', TRef} | {'error', Reason2}
              when Time :: time(),
                   TRef :: tref(),
                   Reason1 :: term(),
                   Reason2 :: term().
exit_after(Time, Reason) ->
    exit_after(Time, self(), Reason).

-doc #{ equiv => exit_after(Time, Target, kill) }.
-spec kill_after(Time, Target) -> {'ok', TRef} | {'error', Reason2}
              when Time :: time(),
                   Target :: pid() | (RegName :: atom()),
                   TRef :: tref(),
                   Reason2 :: term().
kill_after(Time, Pid) ->
    exit_after(Time, Pid, kill).

-doc #{ equiv => exit_after(Time, self(), kill) }.
-spec kill_after(Time) -> {'ok', TRef} | {'error', Reason2}
              when Time :: time(),
                   TRef :: tref(),
                   Reason2 :: term().
kill_after(Time) ->
    exit_after(Time, self(), kill).

-doc """
Evaluates [`spawn(erlang, apply, [Function, []])`](`spawn/3`) repeatedly at
intervals of `Time`, irrespective of whether a previously spawned process has
finished or not.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec apply_interval(Time, Function) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Function :: fun(() -> _),
                   TRef :: tref(),
                   Reason :: term().
apply_interval(Time, F)
  when ?valid_apply(F) ->
    apply_interval(Time, erlang, apply, [F, []]);
apply_interval(_Time, _F) ->
    {error, badarg}.

-doc """
Evaluates [`spawn(erlang, apply, [Function, Arguments])`](`spawn/3`) repeatedly
at intervals of `Time`, irrespective of whether a previously spawned process has
finished or not.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec apply_interval(Time, Function, Arguments) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Function :: fun((...) -> _),
                   Arguments :: [term()],
                   TRef :: tref(),
                   Reason :: term().
apply_interval(Time, F, A)
  when ?valid_apply(F, A) ->
    apply_interval(Time, erlang, apply, [F, A]);
apply_interval(_Time, _F, _A) ->
    {error, badarg}.

-doc """
Evaluates [`spawn(Module, Function, Arguments)`](`spawn/3`) repeatedly at
intervals of `Time`, irrespective of whether a previously spawned process has
finished or not.

> #### Warning {: .warning }
>
> If the execution time of the spawned process is, on average, greater than the
> given `Time`, multiple such processes will run at the same time. With long
> execution times, short intervals, and many interval timers running, this may
> even lead to exceeding the number of allowed processes. As an extreme example,
> consider
> `[timer:apply_interval(1, timer, sleep, [1000]) || _ <- lists:seq(1, 1000)]`,
> that is, 1,000 interval timers executing a process that takes 1s to complete,
> started in intervals of 1ms, which would result in 1,000,000 processes running
> at the same time, far more than a node started with default settings allows
> (see the
> [System Limits section in the Effiency Guide](`e:system:system_limits.md`)).
""".
-spec apply_interval(Time, Module, Function, Arguments) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Module :: module(),
                   Function :: atom(),
                   Arguments :: [term()],
                   TRef :: tref(),
                   Reason :: term().
apply_interval(Time, M, F, A)
  when ?valid_time(Time),
       ?valid_apply(M, F, A) ->
    req(apply_interval, {system_time(), Time, self(), {M, F, A}});
apply_interval(_Time, _M, _F, _A) ->
    {error, badarg}.

-doc """
Evaluates [`spawn(erlang, apply, [Function, []])`](`spawn/3`) repeatedly at
intervals of `Time`, waiting for the spawned process to finish before starting
the next.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec apply_repeatedly(Time, Function) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Function :: fun(() -> _),
                   TRef :: tref(),
                   Reason :: term().
apply_repeatedly(Time, F)
  when ?valid_apply(F) ->
    apply_repeatedly(Time, erlang, apply, [F, []]);
apply_repeatedly(_Time, _F) ->
    {error, badarg}.

-doc """
Evaluates [`spawn(erlang, apply, [Function, Arguments])`](`spawn/3`) repeatedly
at intervals of `Time`, waiting for the spawned process to finish before
starting the next.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec apply_repeatedly(Time, Function, Arguments) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Function :: fun((...) -> _),
                   Arguments :: [term()],
                   TRef :: tref(),
                   Reason :: term().
apply_repeatedly(Time, F, A)
  when ?valid_apply(F, A) ->
    apply_repeatedly(Time, erlang, apply, [F, A]);
apply_repeatedly(_Time, _F, _A) ->
    {error, badarg}.

-doc """
Evaluates [`spawn(Module, Function, Arguments)`](`spawn/3`) repeatedly at
intervals of `Time`, waiting for the spawned process to finish before starting
the next.

If the execution time of the spawned process is greater than the given `Time`,
the next process is spawned immediately after the one currently running has
finished. Assuming that execution times of the spawned processes performing the
applies on average are smaller than `Time`, the amount of applies made over a
large amount of time will be the same even if some individual execution times
are larger than `Time`. The system will try to catch up as soon as possible. For
example, if one apply takes `2.5*Time`, the following two applies will be made
immediately one after the other in sequence.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec apply_repeatedly(Time, Module, Function, Arguments) ->
          {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Module :: module(),
                   Function :: atom(),
                   Arguments :: [term()],
                   TRef :: tref(),
                   Reason :: term().
apply_repeatedly(Time, M, F, A)
  when ?valid_time(Time),
       ?valid_apply(M, F, A) ->
    req(apply_repeatedly, {system_time(), Time, self(), {M, F, A}});
apply_repeatedly(_Time, _M, _F, _A) ->
    {error, badarg}.

-doc """
Evaluates `Destination ! Message` repeatedly after `Time` milliseconds.

`Destination` can be a remote or local process identifier, an atom of a registered
name or a tuple `{RegName, Node}` for a registered name at another node.
""".
-spec send_interval(Time, Destination, Message) -> {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Destination :: pid() | (RegName :: atom()) | {RegName :: atom(), Node :: node()},
                   Message :: term(),
                   TRef :: tref(),
                   Reason :: term().
send_interval(Time, Pid, Message)
  when ?valid_time(Time),
       is_pid(Pid) ->
    req(apply_interval, {system_time(), Time, Pid, {?MODULE, send, [Pid, Message]}});
send_interval(Time, RegName, Message)
  when ?valid_time(Time),
       is_atom(RegName) ->
    req(apply_interval, {system_time(), Time, RegName, {?MODULE, send, [RegName, Message]}});
send_interval(Time, Dest = {RegName, Node}, Message)
  when ?valid_time(Time),
       is_atom(RegName),
       is_atom(Node) ->
    req(apply_interval, {system_time(), Time, Dest, {?MODULE, send, [Dest, Message]}});
send_interval(_Time, _Pid, _Message) ->
    {error, badarg}.

-doc(#{equiv => send_interval(Time, self(), Message)}).
-spec send_interval(Time, Message) -> {'ok', TRef} | {'error', Reason}
              when Time :: time(),
                   Message :: term(),
                   TRef :: tref(),
                   Reason :: term().
send_interval(Time, Message) ->
    send_interval(Time, self(), Message).

-doc """
Cancels a previously requested time-out. `TRef` is a unique timer reference
returned by the related timer function.

Returns `{ok, cancel}`, or `{error, Reason}` when `TRef` is not a timer
reference.
""".
-spec cancel(TRef) -> {'ok', 'cancel'} | {'error', Reason}
              when TRef :: tref(),
                   Reason :: term().
cancel({instant, Ref})
  when is_reference(Ref) ->
    {ok, cancel};
cancel({send_local, Ref})
  when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    {ok, cancel};
cancel({once, Ref} = TRef)
  when is_reference(Ref) ->
    req(cancel, TRef);
cancel({interval, Ref} = TRef)
  when is_reference(Ref) ->
    req(cancel, TRef);
cancel(_TRef) ->
    {error, badarg}.

-doc """
Suspends the process calling this function for `Time` milliseconds and then
returns `ok`, or suspends the process forever if `Time` is the atom `infinity`.
Naturally, this function does _not_ return immediately.

> #### Note {: .info }
>
> Before OTP 25, `timer:sleep/1` did not accept integer timeout values greater
> than `16#ffffffff`, that is, `2^32-1`. Since OTP 25, arbitrarily high integer
> values are accepted.
""".
-spec sleep(Time) -> 'ok'
              when Time :: time() | 'infinity'.
sleep(T)
  when is_integer(T),
       T > ?MAX_RECEIVE_AFTER ->
    receive
    after ?MAX_RECEIVE_AFTER ->
            sleep(T - ?MAX_RECEIVE_AFTER)
    end;
sleep(T) ->
    receive
    after T -> ok
    end.

%%
%% Measure the execution time (in microseconds) for Fun().
%%
-doc(#{equiv => tc(Fun, microsecond)}).
-doc(#{since => ~"OTP R14B03"}).
-spec tc(Fun) -> {Time, Value}
              when Fun :: function(),
                   Time :: integer(),
                   Value :: term().
tc(F) ->
    tc(F, microsecond).

%%
%% Measure the execution time (in microseconds) for Fun(Args)
%%      or the execution time (in TimeUnit) for Fun().
%%
-doc """
tc(Fun, ArgumentsOrTimeUnit)

Measures the execution time of `Fun`.

Equivalent to [`tc(Fun, Arguments, microsecond)`](`tc/3`) if called as `tc(Fun, Arguments)`.

Measures the execution time of `Fun` in `TimeUnit` if called as `tc(Fun, TimeUnit)`. Added in OTP 26.0.
""".
-doc #{ since => ~"OTP R14B" }.
-spec tc(Fun, Arguments) -> {Time, Value}
              when Fun :: function(),
                   Arguments :: [term()],
                   Time :: integer(),
                   Value :: term();
        (Fun, TimeUnit) -> {Time, Value}
              when Fun :: function(),
                   TimeUnit :: erlang:time_unit(),
                   Time :: integer(),
                   Value :: term().
tc(F, A) when is_list(A) ->
    tc(F, A, microsecond);
tc(F, TimeUnit) ->
    T1 = erlang:monotonic_time(),
    Val = F(),
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, TimeUnit),
    {Time, Val}.

%%
%% Measure the execution time (in microseconds) for an MFA
%%      or the execution time (in TimeUnit) for Fun(Args).
%%
-doc """
tc(ModuleOrFun, FunctionOrArguments, ArgumentsOrTimeUnit)

Measures the execution time of `Fun` or `apply(Module, Function, Arguments)`.

Equivalent to [`tc(Module, Function, Arguments, microsecond)`](`tc/4`) if called as `tc(Module, Function, Arguments)`.

Equivalent to [`tc(erlang, apply, [Fun, Arguments], TimeUnit)`](`tc/4`) if called as `tc(Fun, Arguments, TimeUnit)`. Added in OTP 26.0
""".
-spec tc(Module, Function, Arguments) -> {Time, Value}
              when Module :: module(),
                   Function :: atom(),
                   Arguments :: [term()],
                   Time :: integer(),
                   Value :: term();
        (Fun, Arguments, TimeUnit) -> {Time, Value}
              when Fun :: function(),
                   Arguments :: [term()],
                   TimeUnit :: erlang:time_unit(),
                   Time :: integer(),
                   Value :: term().
tc(M, F, A) when is_list(A) ->
    tc(M, F, A, microsecond);
tc(F, A, TimeUnit) ->
    T1 = erlang:monotonic_time(),
    Val = apply(F, A),
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, TimeUnit),
    {Time, Val}.

%%
%% Measure the execution time (in TimeUnit) for an MFA.
%%
-doc """
Evaluates [`apply(Module, Function, Arguments)`](`apply/3`) and measures the elapsed
real time as reported by `erlang:monotonic_time/0`.

Returns `{Time, Value}`, where `Time` is the elapsed real time in the
specified `TimeUnit`, and `Value` is what is returned from the apply.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec tc(Module, Function, Arguments, TimeUnit) -> {Time, Value}
              when Module :: module(),
                   Function :: atom(),
                   Arguments :: [term()],
                   TimeUnit :: erlang:time_unit(),
                   Time :: integer(),
                   Value :: term().
tc(M, F, A, TimeUnit) ->
    T1 = erlang:monotonic_time(),
    Val = apply(M, F, A),
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, TimeUnit),
    {Time, Val}.

%%
%% Calculate the time difference (in microseconds) of two
%% erlang:now() timestamps, T2-T1.
%%
-doc """
Calculates the time difference `Tdiff = T2 - T1` in _microseconds_, where `T1`
and `T2` are time-stamp tuples on the same format as returned from
`erlang:timestamp/0` or `os:timestamp/0`.
""".
-spec now_diff(T2, T1) -> Tdiff
              when T1 :: erlang:timestamp(),
                   T2 :: erlang:timestamp(),
                   Tdiff :: integer().
now_diff({A2, B2, C2}, {A1, B1, C1}) ->
    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.

%%
%% Convert seconds, minutes etc. to milliseconds.
%%
-doc "Returns the number of milliseconds in `Seconds`.".
-spec seconds(Seconds) -> MilliSeconds
              when Seconds :: non_neg_integer(),
                   MilliSeconds :: time().
seconds(Seconds) ->
    1000*Seconds.

-doc "Returns the number of milliseconds in `Minutes`.".
-spec minutes(Minutes) -> MilliSeconds
              when Minutes :: non_neg_integer(),
                   MilliSeconds :: time().
minutes(Minutes) ->
    1000*60*Minutes.

-doc "Returns the number of milliseconds in `Hours`.".
-spec hours(Hours) -> MilliSeconds
              when Hours :: non_neg_integer(),
                   MilliSeconds :: time().
hours(Hours) ->
    1000*60*60*Hours.

-doc "Returns the number of milliseconds in `Hours + Minutes + Seconds`.".
-spec hms(Hours, Minutes, Seconds) -> MilliSeconds
              when Hours :: non_neg_integer(),
                   Minutes :: non_neg_integer(),
                   Seconds :: non_neg_integer(),
                   MilliSeconds :: time().
hms(H, M, S) ->
    hours(H) + minutes(M) + seconds(S).

%%
%%   Start/init functions
%%

-doc """
Starts the timer server.

Normally, the server does not need to be started explicitly. It is started dynamically
if it is needed. This is useful during development, but in a target system the server
is to be started explicitly. Use configuration parameters for [Kernel](`e:kernel:index.html`)
for this.
""".
-spec start() -> 'ok'.
start() ->
    {ok, _Pid} = do_start(),
    ok.

do_start() ->
    case
        supervisor:start_child(
          kernel_sup,
          #{
            id => timer_server,
            start => {?MODULE, start_link, []},
            restart => permanent,
            shutdown => 1000,
            type => worker,
            modules => [?MODULE]
           }
         )
    of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, already_present} ->
            case supervisor:restart_child(kernel_sup, timer_server) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, {already_started, Pid}} ->
                    {ok, Pid}
            end;
        Error ->
            Error
    end.

-doc false.
-spec start_link() -> {'ok', pid()} | {'error', term()}.
start_link() ->
    gen_server:start_link({local, timer_server}, ?MODULE, [], []).

-doc false.
-spec init([]) -> {'ok', ets:tid()}.
init([]) ->
    process_flag(trap_exit, true),
    Tab = ets:new(?MODULE, [private]),
    {ok, Tab}.

%% server calls

%% Try sending a call. If it fails with reason noproc,
%% try starting the timer server and try once again.
req(Req, Arg) ->
    try
        maybe_req(Req, Arg)
    catch
        exit:{noproc, _} ->
            {ok, _Pid} = do_start(),
            maybe_req(Req, Arg)
    end.

maybe_req(Req, Arg) ->
    gen_server:call(timer_server, {Req, Arg}, infinity).

%% Call handling.
-doc false.
-spec handle_call(term(), term(), Tab) ->
          {'reply', term(), Tab} | {'noreply', Tab} when
      Tab :: ets:tid().
%% Start a one-shot timer.
handle_call({apply_once, {Started, Time, MFA}}, _From, Tab) ->
    Timeout = Started + Time,
    Reply = try
                erlang:start_timer(Timeout, self(), {apply_once, MFA},
				   [{abs, true}])
            of
                SRef ->
                    ets:insert(Tab, {SRef}),
                    {ok, {once, SRef}}
            catch
                error:badarg ->
                    {error, badarg}
            end,
    {reply, Reply, Tab};
%% Start an interval timer.
handle_call({apply_interval, {Started, Time, Pid, MFA}}, _From, Tab) ->
    {TRef, TPid, Tag} = start_interval_loop(Started, Time, Pid, MFA, false),
    ets:insert(Tab, {TRef, TPid, Tag}),
    {reply, {ok, {interval, TRef}}, Tab};
handle_call({apply_repeatedly, {Started, Time, Pid, MFA}}, _From, Tab) ->
    {TRef, TPid, Tag} = start_interval_loop(Started, Time, Pid, MFA, true),
    ets:insert(Tab, {TRef, TPid, Tag}),
    {reply, {ok, {interval, TRef}}, Tab};
%% Cancel a one-shot timer.
handle_call({cancel, {once, TRef}}, _From, Tab) ->
    _ = remove_timer(TRef, Tab),
    {reply, {ok, cancel}, Tab};
%% Cancel an interval timer.
handle_call({cancel, {interval, TRef}}, _From, Tab) ->
    _ = case remove_timer(TRef, Tab) of
            true ->
                demonitor(TRef, [flush]);
            false ->
                ok
        end,
    {reply, {ok, cancel}, Tab};
%% Unexpected.
handle_call(_Req, _From, Tab) ->
    {noreply, Tab}.

%% Info handling.
-doc false.
-spec handle_info(term(), Tab) -> {'noreply', Tab}
              when Tab :: ets:tid().
%% One-shot timer timeout.
handle_info({timeout, TRef, {apply_once, MFA}}, Tab) ->
    _ = case ets:take(Tab, TRef) of
            [{TRef}] ->
                do_apply(MFA, false);
            [] ->
                ok
        end,
    {noreply, Tab};
%% An interval timer loop process died.
handle_info({'DOWN', TRef, process, _Pid, _Reason}, Tab) ->
    _ = remove_timer(TRef, Tab),
    {noreply, Tab};
%% Unexpected.
handle_info(_Req, Tab) ->
    {noreply, Tab}.

%% Cast handling.
-doc false.
-spec handle_cast(term(), Tab) -> {'noreply', Tab}
              when Tab :: ets:tid().
%% Unexpected.
handle_cast(_Req, Tab) ->
    {noreply, Tab}.

-doc false.
-spec terminate(term(), _Tab) -> 'ok'.
terminate(_Reason, undefined) ->
    ok;
terminate(Reason, Tab) ->
    _ = ets:foldl(fun
                      ({TRef}, Acc) ->
                          _ = cancel_timer(TRef),
                          Acc;
                      ({_TRef, TPid, Tag}, Acc) ->
                          TPid ! {cancel, Tag},
                          Acc
                  end,
                  undefined,
                  Tab),
    true = ets:delete(Tab),
    terminate(Reason, undefined).

-doc false.
-spec code_change(term(), State, term()) -> {'ok', State}.
code_change(_OldVsn, Tab, _Extra) ->
    %% According to the man for gen server no timer can be set here.
    {ok, Tab}.

start_interval_loop(Started, Time, TargetPid, MFA, WaitComplete) ->
    Tag = make_ref(),
    TimeServerPid = self(),
    {TPid, TRef} = spawn_monitor(fun() ->
                                     TimeServerRef = monitor(process, TimeServerPid),
                                     TargetRef = monitor(process, TargetPid),
                                     TimerRef = schedule_interval_timer(Started, Time,
                                                                        MFA),
                                     _ = interval_loop(TimeServerRef, TargetRef, Tag,
                                                       WaitComplete, TimerRef)
                                 end),
    {TRef, TPid, Tag}.

%% Interval timer loop.
interval_loop(TimerServerMon, TargetMon, Tag, WaitComplete, TimerRef0) ->
    receive
        {cancel, Tag} ->
            ok = cancel_timer(TimerRef0);
        {'DOWN', TimerServerMon, process, _, _} ->
            ok = cancel_timer(TimerRef0);
        {'DOWN', TargetMon, process, _, _} ->
            ok = cancel_timer(TimerRef0);
        {timeout, TimerRef0, {apply_interval, CurTimeout, Time, MFA}} ->
            case do_apply(MFA, WaitComplete) of
                {ok, {spawn, ActionMon}} ->
                    receive
                        {cancel, Tag} ->
                            ok;
                        {'DOWN', TimerServerMon, process, _, _} ->
                            ok;
                        {'DOWN', TargetMon, process, _, _} ->
                            ok;
                        {'DOWN', ActionMon, process, _, _} ->
                            TimerRef1 = schedule_interval_timer(CurTimeout, Time, MFA),
                            interval_loop(TimerServerMon, TargetMon, Tag, WaitComplete, TimerRef1)
                    end;
                _ ->
                    TimerRef1 = schedule_interval_timer(CurTimeout, Time, MFA),
                    interval_loop(TimerServerMon, TargetMon, Tag, WaitComplete, TimerRef1)
            end
    end.

schedule_interval_timer(CurTimeout, Time, MFA) ->
    NextTimeout = CurTimeout + Time,
    case NextTimeout =< system_time() of
        true ->
            TimerRef = make_ref(),
            self() ! {timeout, TimerRef, {apply_interval, NextTimeout, Time, MFA}},
            TimerRef;
        false ->
            erlang:start_timer(NextTimeout, self(), {apply_interval, NextTimeout, Time, MFA}, [{abs, true}])
    end.

%% Remove a timer.
remove_timer(TRef, Tab) ->
    case ets:take(Tab, TRef) of
        [{TRef}] -> % One-shot timer.
            ok = cancel_timer(TRef),
            true;
        [{TRef, TPid, Tag}] -> % Interval timer.
            TPid ! {cancel, Tag},
            true;
        [] -> % TimerReference does not exist, do nothing
            false
    end.

%% Cancel a timer.
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef, [{async, true}, {info, false}]).

%% Help functions

%% If send op. send directly (faster than spawn)
do_apply({?MODULE, send, A}, _) ->
    try send(A)
    of _ -> {ok, send}
    catch _:_ -> error
    end;
%% If exit op. resolve registered name
do_apply({erlang, exit, [Name, Reason]}, _) ->
    try exit(get_pid(Name), Reason)
    of _ -> {ok, exit}
    catch _:_ -> error
    end;
do_apply({M,F,A}, false) ->
    try spawn(M, F, A)
    of _ -> {ok, spawn}
    catch error:badarg -> error
    end;
do_apply({M, F, A}, true) ->
    try spawn_monitor(M, F, A)
    of {_, Ref} -> {ok, {spawn, Ref}}
    catch error:badarg -> error
    end.

%% Get current time in milliseconds,
%% ceil'ed to the next millisecond.
system_time() ->
    (erlang:monotonic_time(microsecond) + 999) div 1000.

send([Pid, Msg]) ->
    Pid ! Msg.

%% Resolve a registered name.
get_pid(Name) when is_pid(Name) ->
    Name;
get_pid(undefined) ->
    undefined;
get_pid(Name) when is_atom(Name) ->
    get_pid(whereis(Name));
get_pid(_) ->
    undefined.
