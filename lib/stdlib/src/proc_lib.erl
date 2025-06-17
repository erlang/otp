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
-module(proc_lib).
-moduledoc """
Functions for asynchronous and synchronous start of processes adhering to the
OTP design principles.

This module is used to start processes adhering to the
[OTP Design Principles](`e:system:design_principles.md`). Specifically, the
functions in this module are used by the OTP standard behaviors (for example,
`m:gen_server` and `m:gen_statem`) when starting new processes. The functions can
also be used to start _special processes_, user-defined processes that comply to
the OTP design principles. For an example, see section
[sys and proc_lib](`e:system:spec_proc.md`) in OTP Design Principles.

Some useful information is initialized when a process starts. The registered
names, or the process identifiers, of the parent process, and the parent
ancestors, are stored together with information about the function initially
called in the process.

While in "plain Erlang", a process is said to terminate normally only for exit
reason `normal`, a process started using `m:proc_lib` is also said to terminate
normally if it exits with reason `shutdown` or `{shutdown,Term}`. `shutdown` is
the reason used when an application (supervision tree) is stopped.

When a process that is started using `m:proc_lib` terminates abnormally (that is,
with another exit reason than `normal`, `shutdown`, or `{shutdown,Term}`), a
_crash report_ is generated, which is written to terminal by the default logger
handler setup by Kernel. For more information about how crash reports were
logged prior to Erlang/OTP 21.0, see
[SASL Error Logging](`e:sasl:error_logging.md`) in the SASL User's Guide.

Unlike in "plain Erlang", `m:proc_lib` processes will not generate _error
reports_, which are written to the terminal by the emulator. All exceptions are
converted to _exits_ which are ignored by the default `logger` handler.

The crash report contains the previously stored information, such as ancestors
and initial function, the termination reason, and information about other
processes that terminate as a result of this process terminating.

## See Also

`m:logger`
""".

-compile(nowarn_deprecated_catch).

%% This module is used to set some initial information
%% in each created process. 
%% Then a process terminates the Reason is checked and
%% a crash report is generated if the Reason was not expected.

-export([spawn/1, spawn_link/1, spawn/2, spawn_link/2,
         spawn/3, spawn_link/3, spawn/4, spawn_link/4,
         spawn_opt/2, spawn_opt/3, spawn_opt/4, spawn_opt/5,
	 start/3, start/4, start/5, start_link/3, start_link/4, start_link/5,
         start_monitor/3, start_monitor/4, start_monitor/5,
	 hibernate/3,
	 init_ack/1, init_ack/2,
	 init_fail/2, init_fail/3,
	 init_p/3,init_p/5,format/1,format/2,format/3,report_cb/2,
	 initial_call/1,
         translate_initial_call/1,
         set_label/1, get_label/1,
	 stop/1, stop/3]).

%% Internal exports.
-export([wake_up/3]).

-export_type([spawn_option/0]).
-export_type([start_spawn_option/0]).

-include("logger.hrl").

%%-----------------------------------------------------------------------------

%% This shall be spawn_option() -- monitor options and must be kept in sync
%% (with erlang:spawn_opt_options())
%%
-doc """
A restricted set of [spawn options](`t:spawn_option/0`). Most notably `monitor`
is _not_ part of these options.
""".
-type start_spawn_option() :: 'link'
                            | {'priority', erlang:priority_level()}
                            | {'fullsweep_after', non_neg_integer()}
                            | {'min_heap_size', non_neg_integer()}
                            | {'min_bin_vheap_size', non_neg_integer()}
                            | {'max_heap_size', erlang:max_heap_size()}
                            | {'message_queue_data', erlang:message_queue_data() }.
%% and this macro is used to verify that there are no monitor options
%% which also needs to be kept in sync all kinds of monitor options
%% in erlang:spawn_opt_options().
%%
-define(VERIFY_NO_MONITOR_OPT(M, F, A, T, Opts),
        Monitor = monitor,
        case lists:member(Monitor, Opts) of
            true ->
                erlang:error(badarg, [M,F,A,T,Opts]);
            false ->
                case lists:keyfind(Monitor, 1, Opts) of
                    false ->
                        ok;
                    {Monitor, _} ->
                        erlang:error(badarg, [M,F,A,T,Opts])
                end
        end).

-doc """
An exception passed to `init_fail/3`. See `erlang:raise/3` for a description
of `Class`, `Reason` and `Stacktrace`.
""".
-type exception() :: {Class :: 'error' | 'exit' | 'throw', Reason :: term()} |
                     {Class :: 'error' | 'exit' | 'throw', Reason :: term(),
                      Stacktrace :: erlang:raise_stacktrace()}.

-doc "Equivalent to `t:erlang:spawn_opt_option/0`.".
-type spawn_option()   :: erlang:spawn_opt_option().

-type dict_or_pid()    :: pid()
                        | (ProcInfo :: [_])
                        | {X :: integer(), Y :: integer(), Z :: integer()}.

%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------

-doc(#{equiv => spawn(erlang, apply, [Fun])}).
-spec spawn(Fun) -> pid() when
      Fun :: function().

spawn(F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(?MODULE, init_p, [Parent,Ancestors,F]).

-doc(#{equiv => spawn(node(), Module, Function, Args)}).
-spec spawn(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

spawn(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-doc(#{equiv => spawn_link(erlang, apply, [Fun])}).
-spec spawn_link(Fun) -> pid() when
      Fun :: function().

spawn_link(F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(?MODULE, init_p, [Parent,Ancestors,F]).

-doc(#{equiv => spawn_link(node(), Module, Function, Args)}).
-spec spawn_link(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

spawn_link(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-doc(#{equiv => spawn(Node, apply, erlang, [Fun])}).
-spec spawn(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().

spawn(Node, F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(Node, ?MODULE, init_p, [Parent,Ancestors,F]).

-doc """
Spawns a new process and initializes it as described in the beginning of this
manual page. The process is spawned using the [`spawn`](`erlang:spawn/1`) BIFs.
""".
-spec spawn(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

spawn(Node, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-doc(#{equiv => spawn_link(Node, erlang, apply, [Fun])}).
-spec spawn_link(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().

spawn_link(Node, F) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(Node, ?MODULE, init_p, [Parent,Ancestors,F]).

-doc """
Spawns a new process and initializes it as described in the beginning of this
manual page. The process is spawned using the
[`spawn_link`](`erlang:spawn_link/1`) BIFs.
""".
-spec spawn_link(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

spawn_link(Node, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-doc(#{equiv => spawn_opt(erlang, apply, [Fun], SpawnOpts)}).
-spec spawn_opt(Fun, SpawnOpts) -> pid() | {pid(), reference()} when
      Fun :: function(),
      SpawnOpts :: [erlang:spawn_opt_option()].

spawn_opt(F, Opts) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,F],Opts).

-doc(#{equiv => spawn_opt(Node, erlang, apply, [Fun], SpawnOpts)}).
-spec spawn_opt(Node, Fun, SpawnOpts) -> pid() | {pid(), reference()} when
      Node :: node(),
      Fun :: function(),
      SpawnOpts :: [erlang:spawn_opt_option()].

spawn_opt(Node, F, Opts) when is_function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(Node, ?MODULE, init_p, [Parent,Ancestors,F], Opts).

-doc(#{equiv => spawn_opt(node(), Module, Function, Args, SpawnOpts)}).
-spec spawn_opt(Module, Function, Args, SpawnOpts) -> pid() | {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      SpawnOpts :: [erlang:spawn_opt_option()].

spawn_opt(M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,M,F,A], Opts).

-doc """
Spawns a new process and initializes it as described in the beginning of this
manual page. The process is spawned using the
[`erlang:spawn_opt`](`erlang:spawn_opt/2`) BIFs.
""".
-spec spawn_opt(Node, Module, Function, Args, SpawnOpts) -> pid() | {pid(), reference()} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      SpawnOpts :: [erlang:spawn_opt_option()].

spawn_opt(Node, M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(Node, ?MODULE, init_p, [Parent,Ancestors,M,F,A], Opts).

spawn_mon(M,F,A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_monitor(?MODULE, init_p, [Parent,Ancestors,M,F,A]).

-doc """
This function does the same as (and does call) the
[`hibernate/3`](`erlang:hibernate/3`) BIF, but ensures that exception handling
and logging continues to work as expected when the process wakes up.

Always use this function instead of the BIF for processes started using
`proc_lib` functions.
""".
-spec hibernate(Module, Function, Args) -> no_return() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].

hibernate(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    erlang:hibernate(?MODULE, wake_up, [M, F, A]).

-doc false.
-spec init_p(pid(), [pid()], function()) -> term().

init_p(Parent, Ancestors, Fun) when is_function(Fun) ->
    put('$ancestors', [Parent|Ancestors]),
    Mfa = erlang:fun_info_mfa(Fun),
    put('$initial_call', Mfa),
    try
	Fun()
    catch
	Class:Reason:Stacktrace ->
	    exit_p(Class, Reason, Stacktrace)
    end.

-doc false.
-spec init_p(pid(), [pid()], atom(), atom(), [term()]) -> term().

init_p(Parent, Ancestors, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    put('$ancestors', [Parent|Ancestors]),
    put('$initial_call', trans_init(M, F, A)),
    init_p_do_apply(M, F, A).

init_p_do_apply(M, F, A) ->
    try
	apply(M, F, A) 
    catch
	Class:Reason:Stacktrace ->
	    exit_p(Class, Reason, Stacktrace)
    end.

-doc false.
-spec wake_up(atom(), atom(), [term()]) -> term().

wake_up(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    try
	apply(M, F, A) 
    catch
	Class:Reason:Stacktrace ->
	    exit_p(Class, Reason, Stacktrace)
    end.

exit_p(Class, Reason, Stacktrace) ->
    case get('$initial_call') of
	{M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
	    MFA = {M,F,make_dummy_args(A, [])},
	    crash_report(Class, Reason, MFA, Stacktrace),
	    erlang:raise(exit, exit_reason(Class, Reason, Stacktrace), Stacktrace);
	_ ->
	    %% The process dictionary has been cleared or
	    %% possibly modified.
	    crash_report(Class, Reason, [], Stacktrace),
	    erlang:raise(exit, exit_reason(Class, Reason, Stacktrace), Stacktrace)
    end.

exit_reason(error, Reason, Stacktrace) ->
    {Reason, Stacktrace};
exit_reason(exit, Reason, _Stacktrace) ->
    Reason;
exit_reason(throw, Reason, Stacktrace) ->
    {{nocatch, Reason}, Stacktrace}.


-doc(#{equiv => start(Module, Function, Args, infinity)}).
-spec start(Module, Function, Args) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Ret :: term() | {error, Reason :: term()}.

start(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    start(M, F, A, infinity).

-doc(#{equiv => start(Module, Function, Args, Time, [])}).
-spec start(Module, Function, Args, Time) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      Ret :: term() | {error, Reason :: term()}.

start(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    sync_start(spawn_mon(M, F, A), Timeout).

-doc """
Starts a new process synchronously. Spawns the process and waits for it to
start.

To indicate a succesful start, the started process _must_ call
[`init_ack(Parent, Ret)`](`init_ack/2`) where `Parent` is the process that
evaluates this function, or [`init_ack(Ret)`](`init_ack/1`). `Ret` is then
returned by this function.

If the process fails to start, it _must_ fail; preferably by calling
[`init_fail(Parent, Ret, Exception)` ](`init_fail/3`) where `Parent` is the
process that evaluates this function, or
[`init_fail(Ret, Exception)`](`init_fail/2`). `Ret` is then returned by this
function, and the started process fails with `Exception`.

If the process instead fails before calling `init_ack/1,2` or `init_fail/2,3`,
this function returns `{error, Reason}` where `Reason` depends a bit on the
exception just like for a process link `{'EXIT',Pid,Reason}` message.

If `Time` is specified as an integer, this function waits for `Time`
milliseconds for the new process to call `init_ack/1,2` or `init_fail/2,3`,
otherwise the process gets killed and `Ret = {error, timeout}` is returned.

Argument `SpawnOpts`, if specified, is passed as the last argument to the
[`spawn_opt/4`](`erlang:spawn_opt/4`) BIF.

> #### Note {: .info }
>
> Using spawn option `monitor` is not allowed. It causes the function to fail
> with reason `badarg`.
>
> Using spawn option `link` will set a link to the spawned process, just like
> [start_link/3,4,5](`start_link/3`).
""".
-spec start(Module, Function, Args, Time, SpawnOpts) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [start_spawn_option()],
      Ret :: term() | {error, Reason :: term()}.

start(M, F, A, Timeout, SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    ?VERIFY_NO_MONITOR_OPT(M, F, A, Timeout, SpawnOpts),
    sync_start(?MODULE:spawn_opt(M, F, A, [monitor|SpawnOpts]), Timeout).

sync_start({Pid, Ref}, Timeout) ->
    receive
	{ack, Pid, Return} ->
	    erlang:demonitor(Ref, [flush]),
            Return;
	{nack, Pid, Return} ->
            flush_EXIT(Pid),
            _ = await_DOWN(Pid, Ref),
            Return;
	{'DOWN', Ref, process, Pid, Reason} ->
            flush_EXIT(Pid),
            {error, Reason}
    after Timeout ->
            kill_flush_EXIT(Pid),
            _ = await_DOWN(Pid, Ref),
            {error, timeout}
    end.


-doc(#{equiv => start_link(Module, Function, Args, infinity)}).
-spec start_link(Module, Function, Args) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Ret :: term() | {error, Reason :: term()}.

start_link(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    start_link(M, F, A, infinity).

-doc(#{equiv => start_link(Module, Function, Args, Time, [])}).
-spec start_link(Module, Function, Args, Time) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      Ret :: term() | {error, Reason :: term()}.

start_link(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    sync_start(?MODULE:spawn_opt(M, F, A, [link,monitor]), Timeout).

-doc """
Starts a new process synchronously. Spawns the process and waits for it to
start. A link is atomically set on the newly spawned process.

> #### Note {: .info }
>
> If the started process gets killed or crashes with a reason that is not
> `normal`, the process link will kill the calling process so this function does
> not return, unless the calling process traps exits. For example, if this
> function times out it will kill the spawned process, and then the link might
> kill the calling process.

Besides setting a link on the spawned process this function behaves like
[start/5](`start/5`).

When the calling process traps exits; if this function returns due to the
spawned process exiting (any error return), this function receives (consumes)
the `'EXIT'` message, also when this function times out and kills the spawned
process.

> #### Note {: .info }
>
> Using spawn option `monitor` is not allowed. It causes the function to fail
> with reason `badarg`.
""".
-spec start_link(Module, Function, Args, Time, SpawnOpts) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [start_spawn_option()],
      Ret :: term() | {error, Reason :: term()}.

start_link(M,F,A,Timeout,SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    ?VERIFY_NO_MONITOR_OPT(M, F, A, Timeout, SpawnOpts),
    sync_start(
      ?MODULE:spawn_opt(M, F, A, [link,monitor|SpawnOpts]), Timeout).


-doc(#{equiv => start_monitor(Module, Function, Args, infinity)}).
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(Module, Function, Args) -> {Ret, Mon} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Mon :: reference(),
      Ret :: term() | {error, Reason :: term()}.

start_monitor(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    start_monitor(M, F, A, infinity).

-doc(#{equiv => start_monitor(Module, Function, Args, Time, [])}).
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(Module, Function, Args, Time) -> {Ret, Mon} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      Mon :: reference(),
      Ret :: term() | {error, Reason :: term()}.

start_monitor(M, F, A, Timeout) when is_atom(M), is_atom(F), is_list(A) ->
    sync_start_monitor(spawn_mon(M, F, A), Timeout).

-doc """
Starts a new process synchronously. Spawns the process and waits for it to
start. A monitor is atomically set on the newly spawned process.

Besides setting a monitor on the spawned process this function behaves like
[start/5](`start/5`).

The return value is `{Ret, Mon}` where `Ret` corresponds to the `Ret` argument
in the call to `init_ack/1,2` or `init_fail/2,3`, and `Mon` is the monitor
reference of the monitor that has been set up.

If this function returns due to the spawned process exiting, that is returns any
error value, a `'DOWN'` message will be delivered to the calling process, also
when this function times out and kills the spawned process.

> #### Note {: .info }
>
> Using spawn option `monitor` is not allowed. It causes the function to fail
> with reason `badarg`.
>
> Using spawn option `link` will set a link to the spawned process, just like
> [start_link/3,4,5](`start_link/3`).
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(Module, Function, Args, Time, SpawnOpts) -> {Ret, Mon} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [start_spawn_option()],
      Mon :: reference(),
      Ret :: term() | {error, Reason :: term()}.

start_monitor(M,F,A,Timeout,SpawnOpts) when is_atom(M),
                                            is_atom(F),
                                            is_list(A) ->
    ?VERIFY_NO_MONITOR_OPT(M, F, A, Timeout, SpawnOpts),
    sync_start_monitor(
      ?MODULE:spawn_opt(M, F, A, [monitor|SpawnOpts]), Timeout).

sync_start_monitor({Pid, Ref}, Timeout) ->
    receive
	{ack, Pid, Return} ->
            {Return, Ref};
	{nack, Pid, Return} ->
            flush_EXIT(Pid),
            self() ! await_DOWN(Pid, Ref),
            {Return, Ref};
	{'DOWN', Ref, process, Pid, Reason} = Down ->
            flush_EXIT(Pid),
            self() ! Down,
            {{error, Reason}, Ref}
    after Timeout ->
            kill_flush_EXIT(Pid),
            self() ! await_DOWN(Pid, Ref),
            {{error, timeout}, Ref}
    end.


%% We regard the existence of an {'EXIT', Pid, _} message
%% as proof enough that there was a link that fired and
%% we had process_flag(trap_exit, true),
%% so the message should be flushed.
%% It is the best we can do.
%%
%% After an unlink(Pid) an {'EXIT', Pid, _} link message
%% cannot arrive so receive after 0 will work,

flush_EXIT(Pid) ->
    unlink(Pid),
    receive {'EXIT', Pid, _} -> ok after 0 -> ok end.

kill_flush_EXIT(Pid) ->
    %% unlink/1 has to be called before exit/2
    %% or we might be killed by the link
    unlink(Pid),
    exit(Pid, kill),
    receive {'EXIT', Pid, _} -> ok after 0 -> ok end.

await_DOWN(Pid, Ref) ->
    receive
	{'DOWN', Ref, process, Pid, _} = Down ->
            Down
    end.


-doc """
This function must only be used by a process that has been started by a
[`start[_link|_monitor]/3,4,5`](`start/5`) function. It tells `Parent` that the
process has initialized itself and started.

Function [`init_ack/1`](`init_ack/1`) uses the parent value previously stored by
the start function used.

If neither this function nor [`init_fail/2,3`](`init_fail/3`) is called by the
started process, the start function returns an error tuple when the started
process exits, or when the start function time-out (if used) has passed, see
[`start/3,4,5`](`start/5`).

> #### Warning {: .warning }
>
> Do not use this function to return an error indicating that the process start
> failed. When doing so the start function can return before the failing process
> has exited, which may block VM resources required for a new start attempt to
> succeed. Use [`init_fail/2,3`](`init_fail/3`) for that purpose.

The following example illustrates how this function and `proc_lib:start_link/3`
are used:

```erlang
-module(my_proc).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    proc_lib:start_link(my_proc, init, [self()]).

init(Parent) ->
    case do_initialization() of
        ok ->
            proc_lib:init_ack(Parent, {ok, self()});
        {error, Reason} ->
            exit(Reason)
    end,
    loop().

...
```
""".
-spec init_ack(Parent, Ret) -> 'ok' when
      Parent :: pid(),
      Ret :: term().

init_ack(Parent, Return) ->
    Parent ! {ack, self(), Return},
    ok.

-doc """
Equivalent to [`init_ack(Parent, Ret)`](`init_ack/2`) where `Parent` is
the process that called `start/5`.
""".
-spec init_ack(Ret) -> 'ok' when
      Ret :: term().

init_ack(Return) ->
    [Parent|_] = get('$ancestors'),
    init_ack(Parent, Return).

-doc """
This function must only be used by a process that has been started by a
[`start[_link|_monitor]/3,4,5`](`start/3`) function. It tells `Parent` that the
process has failed to initialize, and immediately raises an exception according
to `Exception`. The start function then returns `Ret`.

See `erlang:raise/3` for a description of `Class`, `Reason` and `Stacktrace`.

> #### Warning {: .warning }
>
> Do not consider catching the exception from this function. That would defeat
> its purpose. A process started by a [`start[_link|_monitor]/3,4,5`](`start/3`)
> function should end in a value (that will be ignored) or an exception that
> will be handled by this module. See [Description](`m:proc_lib`).

If neither this function nor [`init_ack/1,2`](`init_ack/1`) is called by the
started process, the start function returns an error tuple when the started
process exits, or when the start function time-out (if used) has passed, see
[`start/3,4,5`](`start/3`).

The following example illustrates how this function and `proc_lib:start_link/3`
can be used:

```erlang
-module(my_proc).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    proc_lib:start_link(my_proc, init, [self()]).

init(Parent) ->
    case do_initialization() of
        ok ->
            proc_lib:init_ack(Parent, {ok, self()});
        {error, Reason} = Error ->
            proc_lib:init_fail(Parent, Error, {exit, normal})
    end,
    loop().

...
```
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec init_fail(Parent :: pid(), Return :: term(), Exception :: exception()) -> no_return().
init_fail(Parent, Return, Exception) ->
    _ = Parent ! {nack, self(), Return},
    case Exception of
        {error, Reason} ->
            erlang:error(Reason);
        {exit, Reason} ->
            erlang:exit(Reason);
        {throw, Reason} ->
            erlang:throw(Reason);
        {Class, Reason, Stacktrace} ->
            erlang:error(
              erlang:raise(Class, Reason, Stacktrace),
              [Parent, Return, Exception])
    end.

-doc """
Equivalent to [`init_fail(Parent, Return, Exception)`](`init_fail/3`) where
`Parent` is the process that called `start/5`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec init_fail(Return :: term(), Exception :: exception()) -> no_return().
init_fail(Return, Exception) ->
    [Parent|_] = get('$ancestors'),
    init_fail(Parent, Return, Exception).

%% -----------------------------------------------------
%% Fetch the initial call of a proc_lib spawned process.
%% -----------------------------------------------------

-doc """
Extracts the initial call of a process that was started using one of the spawn
or start functions in this module. `Process` can either be a pid, an integer
tuple (from which a pid can be created), or the process information of a process
`Pid` fetched through an `erlang:process_info(Pid)` function call.

> #### Note {: .info }
>
> The list `Args` no longer contains the arguments, but the same number of atoms
> as the number of arguments; the first atom is `'Argument__1'`, the second
> `'Argument__2'`, and so on. The reason is that the argument list could waste a
> significant amount of memory, and if the argument list contained funs, it
> could be impossible to upgrade the code for the module.
>
> If the process was spawned using a fun, [`initial_call/1`](`initial_call/1`)
> no longer returns the fun, but the module, function for the local function
> implementing the fun, and the arity, for example,
> `{some_module,-work/3-fun-0-,0}` (meaning that the fun was created in function
> `some_module:work/3`). The reason is that keeping the fun would prevent code
> upgrade for the module, and that a significant amount of memory could be
> wasted.
""".
-spec initial_call(Process) -> {Module, Function, Args} | 'false' when
      Process :: dict_or_pid(),
      Module :: module(),
      Function :: atom(),
      Args :: [atom()].

initial_call(DictOrPid) ->
    case raw_initial_call(DictOrPid) of
	{M,F,A} ->
	    {M,F,make_dummy_args(A, [])};
	false ->
	    false
    end.

make_dummy_args(0, Acc) ->
    Acc;
make_dummy_args(N, Acc) ->
    Arg = list_to_atom("Argument__" ++ integer_to_list(N)),
    make_dummy_args(N-1, [Arg|Acc]).

%% -----------------------------------------------------
%% Translate the '$initial_call' to some useful information.
%% However, the arguments are not returned here; only the
%% arity of the initial function.
%% This function is typically called from c:i() and c:regs().
%% -----------------------------------------------------

-doc """
This function is used by functions `\c:i/0` and `\c:regs/0` to present process
information.

This function extracts the initial call of a process that was started using one
of the spawn or start functions in this module, and translates it to more useful
information. `Process` can either be a pid, an integer tuple (from which a pid
can be created), or the process information of a process `Pid` fetched through
an `erlang:process_info(Pid)` function call.

If the initial call is to one of the system-defined behaviors such as
`gen_server` or `gen_event`, it is translated to more useful information. If a
`gen_server` is spawned, the returned `Module` is the name of the callback
module and `Function` is `init` (the function that initiates the new server).

A `supervisor` and a `supervisor_bridge` are also `gen_server` processes. To
return information that this process is a supervisor and the name of the
callback module, `Module` is `supervisor` and `Function` is the name of the
supervisor callback module. `Arity` is `1`, as the `init/1` function is called
initially in the callback module.

By default, `{proc_lib,init_p,5}` is returned if no information about the
initial call can be found. It is assumed that the caller knows that the process
has been spawned with the `proc_lib` module.
""".
-spec translate_initial_call(Process) -> {Module, Function, Arity} when
      Process :: dict_or_pid(),
      Module :: module(),
      Function :: atom(),
      Arity :: byte().

translate_initial_call(DictOrPid) ->
    case raw_initial_call(DictOrPid) of
	{_,_,_}=MFA ->
	    MFA;
	false ->
	    {?MODULE,init_p,5}
    end.

%% -----------------------------------------------------
%% [get] set_label/1
%% Add and fetch process id's to aid in debugging
%% -----------------------------------------------------

-doc """
Set a label for the current process. The primary purpose is to aid in debugging
unregistered processes. The process label can be used in tools and crash reports
to identify processes but it doesn't have to be unique or an atom, as a
registered name needs to be. The process label can be any term, for example
`{worker_process, 1..N}`.

Use [`proc_lib:get_label/1`](`get_label/1`) to lookup the process description.
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec set_label(Label) -> ok when
      Label :: term().
set_label(Label) ->
    put('$process_label', Label),
    ok.

-doc """
Returns either `undefined` or the label for the process Pid set with
[`proc_lib:set_label/1`](`set_label/1`).
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec get_label(Pid) -> undefined | term() when
      Pid :: pid().
get_label(Pid) ->
    case Pid == self() of
        true ->
            get('$process_label');
        false ->
            try get_process_info(Pid, {dictionary, '$process_label'}) of
                {process_label, Id} -> Id;
                _ -> undefined
            catch _:_ -> %% Old Node
                    undefined
            end
    end.

%% -----------------------------------------------------
%% Fetch the initial call information exactly as stored
%% in the process dictionary.
%% -----------------------------------------------------

raw_initial_call({X,Y,Z}) when is_integer(X), is_integer(Y), is_integer(Z) ->
    raw_initial_call(c:pid(X,Y,Z));
raw_initial_call(Pid) when is_pid(Pid) ->
    case get_dictionary(Pid, '$initial_call') of
	{_,_,_}=MFA -> MFA;
	_ -> false
    end;
raw_initial_call(ProcInfo) when is_list(ProcInfo) ->
    case lists:keyfind({dictionary, '$initial_call'}, 1, ProcInfo) of
        {{dictionary,_}, {_,_,_}=MFA} ->
            MFA;
        {{dictionary,_}, _} ->
            false;
        false ->
            case lists:keyfind(dictionary, 1, ProcInfo) of
                {dictionary,Dict} ->
                    case lists:keyfind('$initial_call', 1, Dict) of
                        {_,{_,_,_}=MFA} ->
                            MFA;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end
    end.

%% -----------------------------------------------------
%% Translate the initial call to some useful information.
%% -----------------------------------------------------

trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_event|_]) ->
    {gen_event,init_it,6};
trans_init(gen,init_it,[_GenMod,_,_,Module,_,_]) when is_atom(Module) ->
    {Module,init,1};
trans_init(gen,init_it,[_GenMod,_,_,_,Module|_]) when is_atom(Module) ->
    {Module,init,1};
trans_init(M, F, A) when is_atom(M), is_atom(F) ->
    {M,F,length(A)}.

%% -----------------------------------------------------
%% Generate a crash report.
%% -----------------------------------------------------

crash_report(exit, normal, _, _)       -> ok;
crash_report(exit, shutdown, _, _)     -> ok;
crash_report(exit, {shutdown,_}, _, _) -> ok;
crash_report(Class, Reason, StartF, Stacktrace) ->
    ?LOG_ERROR(#{label=>{proc_lib,crash},
                 report=>[my_info(Class, Reason, StartF, Stacktrace),
                          linked_info(self())]},
               #{domain=>[otp,sasl],
                 report_cb=>fun proc_lib:report_cb/2,
                 logger_formatter=>#{title=>"CRASH REPORT"},
                 error_logger=>#{tag=>error_report,type=>crash_report}}).

my_info(Class, Reason, [], Stacktrace) ->
    my_info_1(Class, Reason, Stacktrace);
my_info(Class, Reason, StartF, Stacktrace) ->
    [{initial_call, StartF}|
     my_info_1(Class, Reason, Stacktrace)].

my_info_1(Class, Reason, Stacktrace) ->
    Keys = [registered_name, dictionary, message_queue_len,
            links, trap_exit, status, heap_size, stack_size, reductions],
    PInfo = get_process_info(self(), Keys),
    {dictionary, Dict} = lists:keyfind(dictionary,1,PInfo),
    [{pid, self()},
     lists:keyfind(registered_name,1,PInfo),
     {process_label, get_label(self())},
     {error_info, {Class,Reason,Stacktrace}},
     {ancestors, get_ancestors()},
     lists:keyfind(message_queue_len,1,PInfo),
     get_messages(self()),
     lists:keyfind(links, 1, PInfo),
     {dictionary, cleaned_dict(Dict)},
     lists:keyfind(trap_exit, 1, PInfo),
     lists:keyfind(status, 1, PInfo),
     lists:keyfind(heap_size, 1, PInfo),
     lists:keyfind(stack_size, 1, PInfo),
     lists:keyfind(reductions, 1, PInfo)
    ].

%% The messages and the dictionary are possibly limited too much if
%% some error handles output the messages or the dictionary using ~P
%% or ~W with depth greater than the depth used here (the depth of
%% control characters P and W takes precedence over the depth set by
%% application variable error_logger_format_depth). However, it is
%% assumed that all report handlers call proc_lib:format().
get_messages(Pid) ->
    Messages = get_process_messages(Pid),
    {messages, error_logger:limit_term(Messages)}.

get_process_messages(Pid) ->
    Depth = error_logger:get_format_depth(),
    case Pid =/= self() orelse Depth =:= unlimited of
        true ->
            {messages, Messages} = get_process_info(Pid, messages),
            Messages;
        false ->
            %% If there are more messages than Depth, garbage
            %% collection can sometimes be avoided by collecting just
            %% enough messages for the crash report. It is assumed the
            %% process is about to die anyway.
            receive_messages(Depth)
    end.

receive_messages(0) -> [];
receive_messages(N) ->
    receive
        M ->
            [M|receive_messages(N - 1)]
    after 0 ->
            []
    end.

cleaned_dict(Dict) ->
    CleanDict = clean_dict(Dict),
    error_logger:limit_term(CleanDict).

clean_dict([{'$ancestors',_}|Dict]) ->
    clean_dict(Dict);
clean_dict([{'$initial_call',_}|Dict]) ->
    clean_dict(Dict);
clean_dict([{'$process_label',_}|Dict]) ->
    clean_dict(Dict);
clean_dict([E|Dict]) ->
    [E|clean_dict(Dict)];
clean_dict([]) ->
    [].

get_dictionary(Pid,Tag) ->
    try get_process_info(Pid, {dictionary, Tag}) of
	{{dictionary,Tag},Value} ->
            Value;
	_ ->
	    undefined
    catch _:_ -> %% rpc to old node
            case get_process_info(Pid,dictionary) of
                {dictionary,Dict} ->
                    case lists:keysearch(Tag,1,Dict) of
                        {value,Value} -> Value;
                        _ -> undefined
                    end;
                _ ->
                    undefined
            end
    end.

linked_info(Pid) ->
  make_neighbour_reports1(neighbours(Pid)).
  
make_neighbour_reports1([P|Ps]) ->
    %%
    %%  Process P might have been deleted.
    %%
    case make_neighbour_report(P) of
        undefined ->
            make_neighbour_reports1(Ps);
        ReportBody ->
            [{neighbour, ReportBody}|make_neighbour_reports1(Ps)]
    end;
make_neighbour_reports1([]) ->
    [].
  
%% Do not include messages or process dictionary, even if
%% error_logger_format_depth is unlimited.
make_neighbour_report(Pid) ->
    Keys = [registered_name,
            initial_call, current_function,
            message_queue_len, links, trap_exit,
            status, heap_size, stack_size, reductions,
            current_stacktrace
           ],
    ProcInfo = get_process_info(Pid, Keys),
    
    DictKeys = [{dictionary, '$process_label'},
                {dictionary, '$initial_call'},
                {dictionary, '$ancestors'}],

    DictInfo = try get_process_info(Pid, DictKeys)
            catch _:_ -> %% old node
                    get_process_info(Pid, dictionary)
            end,
    case ProcInfo =:= undefined orelse DictInfo =:= undefined of
        true -> undefined;
        false ->
            [{pid, Pid},
             lists:keyfind(registered_name,1,ProcInfo),
             dict_find_info('$process_label', DictInfo, undefined),
             get_initial_call(DictInfo, ProcInfo),
             lists:keyfind(current_function, 1, ProcInfo),
             dict_find_info('$ancestors', DictInfo, []),
             lists:keyfind(message_queue_len, 1, ProcInfo),
             lists:keyfind(links, 1, ProcInfo),
             lists:keyfind(trap_exit, 1, ProcInfo),
             lists:keyfind(status, 1, ProcInfo),
             lists:keyfind(heap_size, 1, ProcInfo),
             lists:keyfind(stack_size, 1, ProcInfo),
             lists:keyfind(reductions, 1, ProcInfo),
             lists:keyfind(current_stacktrace, 1, ProcInfo)
            ]
    end.

get_initial_call(DictInfo, ProcInfo) ->
    case dict_find_info('$initial_call', DictInfo, undefined) of
	{initial_call, {M, F, A}} ->
	    {initial_call, {M, F, make_dummy_args(A, [])}};
	_R ->
	    lists:keyfind(initial_call, 1, ProcInfo)
    end.

dict_find_info(DictKey, Dict, Default) ->
    [$$|KeyList] = atom_to_list(DictKey),
    InfoKey = list_to_existing_atom(KeyList),
    case lists:keyfind({dictionary, DictKey}, 1, Dict) of
        false ->
            case lists:keyfind(DictKey, 1, Dict) of
                {DictKey, V} -> {InfoKey, V};
                false -> {InfoKey, Default}
            end;
        {{dictionary,DictKey}, undefined} ->
            {InfoKey,Default};
        {{dictionary,DictKey}, V} ->
            {InfoKey,V}
    end.

%%  neighbours(Pid) = list of Pids
%%
%%  Get the neighbours of Pid. A neighbour is a process which is 
%%  linked to Pid and does not trap exit; or a neigbour of a 
%%  neighbour etc.
%% 
%%  A breadth-first search is performed.

-spec neighbours(pid()) -> [pid()].

neighbours(Pid) ->
    {_, Visited} = visit(adjacents(Pid), {max_neighbours(), [Pid]}),
    lists:delete(Pid, Visited).

max_neighbours() -> 15.

%%
%% visit(Ps, {N, Vs}) = {N0, V0s}
%%
%% A breadth-first search of neighbours.
%%    Ps   processes,
%%    Vs   visited processes,
%%    N    max number to visit.
%%   
visit([P|Ps], {N, Vs} = NVs) when N > 0 ->
  case lists:member(P, Vs) of
    false -> visit(adjacents(P), visit(Ps, {N-1, [P|Vs]}));
    true  -> visit(Ps, NVs)
  end;
visit(_, {_N, _Vs} = NVs) ->
  NVs.

%%
%% adjacents(Pid) = AdjacencyList
%% 
-spec adjacents(pid()) -> [pid()].

adjacents(Pid) ->
  case catch proc_info(Pid, links) of
    {links, Links} -> no_trap(Links);
    _              -> []
  end.
  
no_trap([P|Ps]) ->
  case catch proc_info(P, trap_exit) of
    {trap_exit, false} -> [P|no_trap(Ps)];
    _                  -> no_trap(Ps)
  end;
no_trap([]) ->
  [].
 
get_process_info(Pid, Tag) ->
    translate_process_info(Tag, catch proc_info(Pid, Tag)).

translate_process_info({dictionary, '$process_label'} = Tag, {Tag, Value}) ->
    {process_label, Value};
translate_process_info(_ , {'EXIT', _}) ->
    undefined;
translate_process_info(_, Result) ->
    Result.

%%% -----------------------------------------------------------
%%% Misc. functions
%%% -----------------------------------------------------------

get_my_name() ->
    case proc_info(self(),registered_name) of
	{registered_name,Name} -> Name;
	_                      -> self()
    end.

-spec get_ancestors() -> [pid()].
get_ancestors() ->
    case get('$ancestors') of
	A when is_list(A) -> A;
	_                 -> []
    end.

proc_info(Pid,Item) when node(Pid) =:= node() ->
    process_info(Pid,Item);
proc_info(Pid,Item) ->
    case lists:member(node(Pid),nodes()) of
	true ->
	    check(rpc:call(node(Pid), erlang, process_info, [Pid, Item]));
	_ ->
	    hidden
    end.

check({badrpc,nodedown}) -> undefined;
check({badrpc,Error})    -> Error;
check(Res)               -> Res.

%%% -----------------------------------------------------------
%%% Format a generated crash info structure.
%%% -----------------------------------------------------------

-doc false.
-spec report_cb(CrashReport,FormatOpts) -> unicode:chardata() when
      CrashReport :: #{label => {proc_lib,crash},
                       report => [term()]},
      FormatOpts :: logger:report_cb_config().
report_cb(#{label:={proc_lib,crash}, report:=CrashReport}, Extra) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    do_format(CrashReport, maps:merge(Default,Extra)).

-doc "Equivalent to [`format(CrashReport, latin1)`](`format/2`).".
-spec format(CrashReport) -> string() when
      CrashReport :: [term()].
format(CrashReport) ->
    format(CrashReport, latin1).

-doc """
> #### Note {: .info }
>
> This function is deprecated in the sense that the `error_logger` is no longer
> the preferred interface for logging in Erlang/OTP. A new
> [logging API](`e:kernel:logger_chapter.md`) was added in Erlang/OTP 21.0, but
> legacy `error_logger` handlers can still be used. New Logger handlers do not
> need to use this function, since the formatting callback (`report_cb`) is
> included as metadata in the log event.

This function can be used by a user-defined legacy `error_logger` event handler
to format a crash report. The crash report is sent using `m:logger`, and the
event to be handled is of the format
`{error_report, GL, {Pid, crash_report, CrashReport}}`, where `GL` is the group
leader pid of process `Pid` that sent the crash report.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec format(CrashReport, Encoding) -> string() when
      CrashReport :: [term()],
      Encoding :: latin1 | unicode | utf8.

format(CrashReport, Encoding) ->
    format(CrashReport, Encoding, unlimited).

-doc """
> #### Note {: .info }
>
> This function is deprecated in the sense that the `error_logger` is no longer
> the preferred interface for logging in Erlang/OTP. A new
> [logging API](`e:kernel:logger_chapter.md`) was added in Erlang/OTP 21.0, but
> legacy `error_logger` handlers can still be used. New Logger handlers do not
> need to used this function, since the formatting callback (`report_cb`) is
> included as metadata in the log event.

This function can be used by a user-defined legacy `error_logger` event handler
to format a crash report. When Depth is specified as a positive integer, it is
used in the format string to limit the output as follows:
`io_lib:format("~P", [Term,Depth])`.
""".
-doc(#{since => <<"OTP 18.1">>}).
-spec format(CrashReport, Encoding, Depth) -> string() when
      CrashReport :: [term()],
      Encoding :: latin1 | unicode | utf8,
      Depth :: unlimited | pos_integer().

format(CrashReport, Encoding, Depth) ->
    do_format(CrashReport, #{chars_limit => unlimited,
                             depth => Depth,
                             encoding => Encoding,
                             single_line => false}).

do_format([OwnReport,LinkReport], Extra) ->
    #{encoding:=Enc, single_line:=Single, chars_limit:=Limit0} = Extra,
    Indent = if Single -> "";
                true -> "  "
             end,
    Nl = nl(Single," "),
    Sep = nl(Single, report_separator()),
    {PartLimit, Limit} =
        case Limit0 of
            unlimited ->
                {Limit0, Limit0};
            _ when is_integer(Limit0) ->
                %% HardcodedSize is the length of the hardcoded heading +
                %% separators in the final format string below,
                %% including neighbours. Just make sure the limit
                %% does not become negative.
                Num = length(OwnReport),
                HardcodedSize = (length(Indent) + length("crasher")
                                 + length(Nl) + length(Sep)
                                 + (length(Sep) * Num)),
                Limit1 = max(Limit0-HardcodedSize, 1),

                %% Divide the available characters over all report
                %% parts. Spend one third of the characters on the
                %% crash reason, and let the rest of the elements
                %% (including the neighbours) share the other two
                %% thirds. This is to make sure we see a good part of
                %% the crash reason. Most of the other elements in the
                %% crasher's report are quite small, so we don't loose
                %% a lot of info from these anyway.
                EL = Limit1 div 3,
                PL = (Limit1-EL) div (Num),
                {PL, Limit1}
        end,
    LinkFormat = format_link_reports(LinkReport, Indent, Extra, PartLimit),
    LinkFormatSize = size(Enc, LinkFormat),

    OwnFormat = format_own_report(OwnReport, Indent, Extra,
                                  LinkFormatSize, PartLimit, Limit),
    io_lib:format("~scrasher:"++Nl++"~ts"++Sep++"~ts",
                  [Indent,OwnFormat,LinkFormat]).

format_own_report(OwnReport, Indent, Extra, LinkFormatSize, PartLimit, Limit0) ->
    MyIndent = Indent ++ Indent,
    case separate_error_info(OwnReport) of
        {First,{Class,Reason,StackTrace},Rest} ->
            F = format_report(First, MyIndent, Extra, PartLimit),
            R = format_report(Rest, MyIndent, Extra, PartLimit),
            #{encoding:=Enc, single_line:=Single} = Extra,
            Sep = nl(Single, part_separator()),
            Limit = case Limit0 of
                        unlimited ->
                            Limit0;
                        _ when is_integer(Limit0) ->
                            %% Some of the report parts are quite small,
                            %% and we can use the leftover chars to show
                            %% more of the error_info part.
                            SizeOfOther = (size(Enc, F)
                                           +size(Enc, R)
                                           -length(Sep)*(length(F)+length(R))
                                           +LinkFormatSize),
                            max(Limit0-SizeOfOther, 1)
                end,
            EI = format_exception(Class, Reason, StackTrace, Extra, Limit),
            lists:join(Sep, [F, EI, R]);
    no ->
        Limit = case Limit0 of
                    unlimited ->
                        Limit0;
                    _ when is_integer(Limit0) ->
                        max(Limit0-LinkFormatSize, 1)
                end,
        format_report(OwnReport, MyIndent, Extra, Limit)
    end.

separate_error_info(Report) ->
    try
        lists:splitwith(fun(A) -> element(1, A) =/= error_info end, Report)
    of
        {First, [{error_info,ErrorInfo}|Rest]} ->
            {First,ErrorInfo,Rest};
        _ -> no
    catch _:_ -> no
    end.

%% If the size of the total report is limited by chars_limit, then
%% print only the pids.
format_link_reports(LinkReports, Indent, Extra, PartLimit)
         when is_integer(PartLimit) ->
    #{encoding:=Enc, depth:=Depth, single_line:=Single} = Extra,
    Pids = [P || {neighbour,[{pid,P}|_]} <- LinkReports],
    {P,Tl} = p(Enc,Depth),
    Width = if Single -> "0";
               true -> ""
            end,
    io_lib:format(Indent++"neighbours: ~"++Width++P,
                  [Pids|Tl],
                  [{chars_limit,PartLimit}]);
format_link_reports(LinkReports, Indent, Extra, PartLimit) ->
    #{single_line:=Single} = Extra,
    MyIndent = Indent ++ Indent,
    LinkFormat =
      lists:join(nl(Single, report_separator()),
                 format_link_report(LinkReports, MyIndent, Extra, PartLimit)),
    [Indent,"neighbours:",nl(Single," "),LinkFormat].

format_link_report([Link|Reps], Indent0, Extra, PartLimit) ->
    #{single_line:=Single} = Extra,
    Rep = case Link of
              {neighbour,Rep0} -> Rep0;
              _ -> Link
          end,
    Indent = if Single -> "";
                true -> Indent0
             end,
    LinkIndent = ["  ",Indent],
    [[Indent,"neighbour:",nl(Single," "),
      format_report(Rep, LinkIndent, Extra, PartLimit)]|
     format_link_report(Reps, Indent, Extra, PartLimit)];
format_link_report(Rep, Indent, Extra, PartLimit) ->
    format_report(Rep, Indent, Extra, PartLimit).

format_report(Rep, Indent, Extra, Limit) when is_list(Rep) ->
    #{single_line:=Single} = Extra,
    lists:join(nl(Single, part_separator()),
               format_rep(Rep, Indent, Extra, Limit));
format_report(Rep, Indent0, Extra, Limit) ->
    #{encoding:=Enc, depth:=Depth, single_line:=Single} = Extra,
    {P,Tl} = p(Enc,Depth),
    {Indent,Width} = if Single -> {"","0"};
                        true -> {Indent0,""}
                     end,
    Opts = chars_limit_opt(Limit),
    io_lib:format("~s~"++Width++P, [Indent, Rep | Tl], Opts).

format_rep([{initial_call,InitialCall}|Rep], Indent, Extra, Limit) ->
    [format_mfa(Indent, InitialCall, Extra, Limit)|
     format_rep(Rep, Indent, Extra, Limit)];
format_rep([{process_label,undefined}|Rep], Indent, Extra, Limit) ->
    format_rep(Rep, Indent, Extra, Limit);
format_rep([{Tag,Data}|Rep], Indent, Extra, Limit) ->
    [format_tag(Indent, Tag, Data, Extra, Limit)|
     format_rep(Rep, Indent, Extra, Limit)];
format_rep(_, _, _Extra, _Limit) ->
    [].

format_exception(Class, Reason, StackTrace, Extra, Limit) ->
    #{encoding:=Enc,depth:=Depth, single_line:=Single} = Extra,
    StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    if Single ->
            {P,Tl} = p(Enc,Depth),
            Opts = chars_limit_opt(Limit),
            [atom_to_list(Class), ": ",
             io_lib:format("~0"++P,[{Reason,StackTrace}|Tl],Opts)];
       true ->
            %% Notice that each call to PF uses chars_limit, which
            %% means that the total size of the formatted exception
            %% can exceed the limit a lot.
            PF = pp_fun(Extra, Enc),
            EI = "    ",
            Lim = case Limit of
                      unlimited -> -1;
                      _ -> Limit
                  end,
            FE = erl_error:format_exception(1+length(EI), Class, Reason,
                                            StackTrace, StackFun, PF, Enc,
                                            Lim),
            [EI, FE]
    end.

format_mfa(Indent0, {M,F,Args}=StartF, Extra, Limit) ->
    #{encoding:=Enc,single_line:=Single} = Extra,
    Indent = if Single -> "";
                true -> Indent0
             end,
    try
	A = length(Args),
	[Indent,"initial call: ",to_string(M, Enc),$:,to_string(F, Enc),$/,
	 integer_to_list(A)]
    catch
	error:_ ->
	    format_tag(Indent, initial_call, StartF, Extra, Limit)
    end.

to_string(A, latin1) ->
    io_lib:write_atom_as_latin1(A);
to_string(A, _) ->
    io_lib:write_atom(A).

pp_fun(Extra, Enc) ->
    #{encoding:=Enc,depth:=Depth, single_line:=Single} = Extra,
    {P,Tl} = p(Enc, Depth),
    Width = if Single -> "0";
               true -> ""
            end,
    fun(Term, I, Limit) ->
            S = io_lib:format("~" ++ Width ++ "." ++ integer_to_list(I) ++ P,
                              [Term|Tl], [{chars_limit, Limit}]),
            {S, sub(Limit, S, Enc)}
    end.

format_tag(Indent0, Tag, Data, Extra, Limit) ->
    #{encoding:=Enc,depth:=Depth,single_line:=Single} = Extra,
    {P,Tl} = p(Enc, Depth),
    {Indent,Width} = if Single -> {"","0"};
                        true -> {Indent0,""}
                     end,
    Opts = chars_limit_opt(Limit),
    io_lib:format("~s~" ++ Width ++ "p: ~" ++ Width ++ ".18" ++ P,
                  [Indent, Tag, Data|Tl], Opts).

p(Encoding, Depth) ->
    {Letter, Tl}  = case Depth of
                        unlimited -> {"p", []};
                        _         -> {"P", [Depth]}
                    end,
    P = modifier(Encoding) ++ Letter,
    {P, Tl}.

report_separator() -> "; ".

part_separator() -> ", ".

chars_limit_opt(CharsLimit) ->
    [{chars_limit, CharsLimit} || is_integer(CharsLimit)].

modifier(latin1) -> "";
modifier(_) -> "t".

nl(true,Else) -> Else;
nl(false,_) -> "\n".

%% Make sure T does change sign.
sub(T, _, _Enc) when T < 0 -> T;
sub(T, E, Enc) ->
    Sz = size(Enc, E),
    if
        T >= Sz ->
            T - Sz;
        true ->
            0
    end.

size(latin1, S) ->
    iolist_size(S);
size(_, S) ->
    string:length(S).

%%% -----------------------------------------------------------
%%% Stop a process and wait for it to terminate
%%% -----------------------------------------------------------
-doc "Equivalent to [`stop(Process, normal, infinity)`](`stop/3`).".
-doc(#{since => <<"OTP 18.0">>}).
-spec stop(Process) -> 'ok' when
      Process :: pid() | RegName | {RegName,node()},
      RegName :: atom().
stop(Process) ->
    stop(Process, normal, infinity).

-doc """
Orders the process to exit with the specified `Reason` and waits for it to
terminate.

Returns `ok` if the process exits with the specified `Reason` within `Timeout`
milliseconds.

If the call times out, a `timeout` exception is raised.

If the process does not exist, a `noproc` exception is raised.

The implementation of this function is based on the `terminate` system message,
and requires that the process handles system messages correctly. For information
about system messages, see `m:sys` and section
[sys and proc_lib](`e:system:spec_proc.md`) in OTP Design Principles.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec stop(Process, Reason, Timeout) -> 'ok' when
      Process :: pid() | RegName | {RegName,node()},
      RegName :: atom(),
      Reason :: term(),
      Timeout :: timeout().
stop(Process, Reason, Timeout) ->
    Mref = erlang:monitor(process, Process),
    T0 = erlang:monotonic_time(millisecond),

    StopTimeout = fun(infinity) -> infinity;
                     (T1) -> T1 - (((erlang:monotonic_time(microsecond) + 999) div 1000) - T0)
                end,

    RemainingTimeout = try
	sys:terminate(Process, Reason, Timeout)
    of
	ok -> StopTimeout(Timeout)
    catch
	exit:{noproc, {sys, terminate, _}} ->
	    demonitor(Mref, [flush]),
	    exit(noproc);
	exit:{timeout, {sys, terminate, _}} ->
	    demonitor(Mref, [flush]),
	    exit(timeout);
        exit:{Reason, {sys, terminate, _}} ->
            StopTimeout(Timeout);
	exit:Reason1 ->
	    demonitor(Mref, [flush]),
	    exit(Reason1)
    end,
    receive
	{'DOWN', Mref, _, _, Reason} ->
	    ok;
	{'DOWN', Mref, _, _, Reason2} ->
	    exit(Reason2)
    after RemainingTimeout ->
	demonitor(Mref, [flush]),
	exit(timeout)
    end.
