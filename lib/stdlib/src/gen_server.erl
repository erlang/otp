%%
%% %CopyrightBegin%
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
-module(gen_server).
-moduledoc """
Generic server behavior.

This behavior module provides the server in a client-server relation.
A generic server process (`gen_server`) implemented using this module
has a standard set of interface functions and includes functionality
for tracing and error reporting.  It also fits into
an OTP supervision tree. For more information, see section
[gen_server Behaviour](`e:system:gen_server_concepts.md`)
in OTP Design Principles.

A `gen_server` process assumes all specific parts to be located
in a callback module exporting a predefined set of functions.
The relationship between the behavior functions
and the callback functions is as follows:

```text
gen_server module            Callback module
-----------------            ---------------
gen_server:start
gen_server:start_monitor
gen_server:start_link -----> Module:init/1

gen_server:stop       -----> Module:terminate/2

gen_server:call
gen_server:send_request
gen_server:multi_call -----> Module:handle_call/3

gen_server:cast
gen_server:abcast     -----> Module:handle_cast/2

-                     -----> Module:handle_info/2

-                     -----> Module:handle_continue/2

-                     -----> Module:terminate/2

-                     -----> Module:code_change/3
```

If a callback function fails or returns a bad value,
the `gen_server` process terminates.  However, an exception of class
[`throw`](`erlang:throw/1`) is not regarded as an error
but as a valid return, from all callback functions.

A `gen_server` process handles system messages as described in `m:sys`.
The `m:sys` module can be used for debugging a `gen_server` process.

Notice that a `gen_server` process does not trap exit signals
automatically, this must be explicitly initiated in the callback module.

Unless otherwise stated, all functions in this module fail
if the specified `gen_server` process does not exist
or if bad arguments are specified.

The `gen_server` process can go into hibernation (see `erlang:hibernate/3`)
if a callback function specifies `'hibernate'` instead of a time-out value.
This can be useful if the server is expected to be idle for a long time.
However, use this feature with care, as hibernation implies at least
two garbage collections (when hibernating and shortly after waking up)
and is not something you want to do between each call to a busy server.

If the `gen_server` process needs to perform an action after
initialization or to break the execution of a callback into multiple steps,
it can return `{continue, Continue}` in place of
the time-out or hibernation value, which will invoke
the [`Module:handle_continue/2`](`c:handle_continue/2`) callback,
before receiving any external message / request.

If the `gen_server` process terminates, e.g. as a result of a function
in the callback module returning `{stop,Reason,NewState}`,
an exit signal with this `Reason` is sent to linked processes and ports.
See [Processes](`e:system:ref_man_processes.md#errors`)
in the Reference Manual for details regarding error handling
using exit signals.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_][1]
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
> Blocking signaling can, for example, cause call time-outs
> in `gen_server` to be significantly delayed.

[1]: `e:system:ref_man_processes.md#blocking-signaling-over-distribution`

## See Also

`m:gen_event`, `m:gen_statem`, `m:proc_lib`, `m:supervisor`, `m:sys`
""".

%%%
%%% NOTE: If init_ack() return values are modified, see comment
%%%       above monitor_return() in gen.erl!
%%%

%%% ---------------------------------------------------
%%%
%%% The idea behind THIS server is that the user module
%%% provides (different) functions to handle different
%%% kind of inputs.
%%% If the Parent process terminates the Module:terminate/2
%%% function is called.
%%%
%%% The user module should export:
%%%
%%%   init(Args)
%%%     ==> {ok, State}
%%%         {ok, State, Timeout}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   handle_call(Msg, {From, Tag}, State)
%%%
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_cast(Msg, State)
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_info(Info, State) Info is e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term, terminate(State) is called
%%%
%%%   terminate(Reason, State) Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> ok
%%%
%%%
%%% The work flow (of the server) can be described as follows:
%%%
%%%   User module                          Generic
%%%   -----------                          -------
%%%     start            ----->             start
%%%     init             <-----              .
%%%
%%%                                         loop
%%%     handle_call      <-----              .
%%%                      ----->             reply
%%%
%%%     handle_cast      <-----              .
%%%
%%%     handle_info      <-----              .
%%%
%%%     terminate        <-----              .
%%%
%%%                      ----->             reply
%%%
%%%
%%% ---------------------------------------------------

-compile(nowarn_deprecated_catch).

%% API
-export([start/3, start/4,
	 start_link/3, start_link/4,
         start_monitor/3, start_monitor/4,
	 stop/1, stop/3,
	 call/2, call/3,
         send_request/2, send_request/4,
         wait_response/2, receive_response/2, check_response/2,
         wait_response/3, receive_response/3, check_response/3,
         reqids_new/0, reqids_size/1,
         reqids_add/3, reqids_to_list/1,
	 cast/2, reply/2,
	 abcast/2, abcast/3,
	 multi_call/2, multi_call/3, multi_call/4,
	 enter_loop/3, enter_loop/4, enter_loop/5]).

%% System exports
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 system_get_state/1,
	 system_replace_state/2,
	 format_status/2]).

-behaviour(sys).

%% logger callback
-export([format_log/1, format_log/2]).

%% Internal exports
-export([init_it/6]).

-include("logger.hrl").

-export_type(
   [from/0,
    action/0,
    reply_tag/0,
    request_id/0,
    request_id_collection/0,
    format_status/0]).

-export_type(
   [server_name/0,
    server_ref/0,
    start_opt/0,
    enter_loop_opt/0,
    start_ret/0,
    start_mon_ret/0]).

-define(
   STACKTRACE(),
   element(2, erlang:process_info(self(), current_stacktrace))).

-define(
	is_timeout(Abs, X),
	( ( Abs =:= false andalso ?is_rel_timeout(X) ) orelse ( Abs =:= true andalso ?is_abs_timeout(X) ) )
).

-define(
	is_rel_timeout(X),
	( (X) =:= infinity orelse ( is_integer(X) andalso (X) >= 0 ) )
).

-define(
	is_abs_timeout(X),
	( (X) =:= infinity orelse is_integer(X) )
).

-record(server_data, {parent :: pid(),
		      tag = make_ref() :: reference(),
		      name :: term(),
		      module :: module(),
		      hibernate_after :: timeout(),
		      handle_call :: fun((Request :: term(), From :: from(), State :: term()) ->
                          {reply, Reply :: term(), NewState :: term()} |
                          {reply, Reply :: term(), NewState :: term(), Action :: action()} |
                          {noreply, NewState :: term()} |
                          {noreply, NewState :: term(), action()} |
                          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                          {stop, Reason :: term(), NewState :: term()}),
                      handle_cast :: fun((Request :: term(), State :: term()) ->
                          {noreply, NewState :: term()} |
                          {noreply, NewState :: term(), Action :: action()} |
                          {stop, Reason :: term(), NewState :: term()}),
                      handle_info :: fun((Info :: timeout | term(), State :: term()) ->
                          {noreply, NewState :: term()} |
                          {noreply, NewState :: term(), Action :: action()} |
                          {stop, Reason :: term(), NewState :: term()}),
                      handle_continue :: fun((Info :: term(), State :: term()) ->
                          {noreply, NewState :: term()} |
                          {noreply, NewState :: term(), Action :: action()} |
                          {stop, Reason :: term(), NewState :: term()})}).

%%%=========================================================================
%%%  API
%%%=========================================================================

-doc """
Initialize the server.

Whenever a `gen_server` process is started using [`start/3,4`](`start/3`),
[`start_monitor/3,4`](`start_monitor/3`),
or [`start_link/3,4`](`start_link/3`), this function is called
by the new process to initialize the server.

`Args` is the `Args` argument provided to the start function.

The return value `Result` is interpreted as follows:

- **`{ok,State}`\
  `{ok,State,_}`** - Initialization was succesful
   and `State` is the internal state of the `gen_server` process.

- **`{ok,_,Timeout}`\
  `{ok,_,hibernate}`\
  `{ok,_,{continue,Continue}}`** - See the corresponding return values from
  [`Module:handle_call/3`](`c:handle_call/3`) for a description
  of this tuple member.

- **`{stop,Reason}`** - Initialization failed.  The `gen_server`
  process exits with reason `Reason`.

- **`{error,Reason}` _since OTP 26.0_\
  `ignore`** - Initialization failed. The `gen_server` process exits
  with reason `normal`.

See function [`start_link/3,4`](`start_link/3`)'s return value
`t:start_ret/0` in these different cases.
""".
-callback init(Args :: term()) ->
    {ok, State :: term()} |
    {ok, State :: term(), action()} |
    {stop, Reason :: term()} |
    ignore |
    {error, Reason :: term()}.

-doc """
Handle a call.

Whenever a `gen_server` process receives a request sent using
[`call/2,3`](`call/3`), [`multi_call/2,3,4`](`multi_call/4`),
or [`send_request/2,4`](`send_request/4`), this function is called
to handle the request.

`State` is the internal state of the `gen_server` process,
and `NewState` a possibly updated one.

`Request` is passed from the same argument provided
to `call` or `multi_call`.

The return value `Result` is interpreted as follows:

- **`{reply,Reply,NewState}`\
  `{reply,Reply,NewState,_}`** - The `Reply` value is sent back
 to the client request and there becomes its return value.

  The `gen_server` process continues executing with the possibly updated
  internal state `NewState`.

- **`{noreply,NewState}`\
  `{noreply,NewState,_}`** - The `gen_server` process
  continues executing with the possibly updated internal state `NewState`.

  A reply to the client request has to be created by calling
  [`reply(From, Reply)`](`reply/2`), either in this
  or in a later callback.

- **`{reply,_,_,Timeout}`\
  `{noreply,_,Timeout}`** - If an integer `Timeout` is provided,
  a time-out occurs unless a request or a message is received
  within that many milliseconds. A time-out is represented
  by the atom `timeout` to be handled by the
  [`Module:handle_info/2`](`c:handle_info/2`) callback function.
  `Timeout =:= infinity` can be used to wait indefinitely,
  which is the same as returning a value without a `Timeout` member.

- **`{reply,_,_,hibernate}`\
  `{noreply,_,hibernate}`** - The process goes into hibernation,
  by calling `proc_lib:hibernate/3`, waiting for
  the next message to arrive

- **`{reply,_,_,{continue,Continue}}`\
  `{noreply,_,{continue,Continue}}`** - The process will execute the
  [`Module:handle_continue/2`](`c:handle_continue/2`) callback function,
  with `Continue` as the first argument.

- **`{stop,Reason,NewState}`\
  `{stop,Reason,Reply,NewState}`** - The `gen_server` process will call
  [`Module:terminate(Reason,NewState)`](`c:terminate/2`),
  and then terminate.

  `{stop,_,Reply,_}` will create a reply to the client request just as
  `{reply,Reply,...}` while `{stop,_,_}` will not, so just as for
  `{noreply,NewState,...}` a reply has to be created by calling
  [`reply(From, Reply)`](`reply/2`) before returning `{stop,_,_}`.
""".
-callback handle_call(Request :: term(), From :: from(),
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), action()} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), action()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-doc """
Handle a cast message.

Whenever a `gen_server` process receives a request sent using `cast/2`
or [`abcast/2,3`](`abcast/2`), this function is called
to handle the request.

For a description of the arguments and possible return values,
see [`Module:handle_call/3`](`c:handle_call/3`).
""".
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), action()} |
    {stop, Reason :: term(), NewState :: term()}.

-doc """
Handle an info message (regular process message).

This function is called by a `gen_server` process when a time-out occurs
or when it receives any other message than a synchronous
or asynchronous request (or a system message).

`Info` is either the atom `timeout`, if a time-out has occurred,
or the received message.

For a description of the other arguments and possible return values,
see [`Module:handle_call/3`](`c:handle_call/3`).

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_server` module provides a default implementation
> of this function that logs about the unexpected `Info` message,
> drops it and returns `{noreply, State}`.

""".
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), action()} |
    {stop, Reason :: term(), NewState :: term()}.

-doc """
Handle a callback continuation.

This function is called by a `gen_server` process whenever
a previous callback returns one of the tuples containing
`{continue, Continue}`.  The call is invoked immediately after
the previous callback, which makes it useful for performing work
after initialization or, for splitting the work in a callback
into multiple steps, updating the process state along the way.

For a description of the other arguments and possible return values,
see [`Module:handle_call/3`](`c:handle_call/3`).

> #### Note {: .info }
>
> This callback is optional, so callback modules need to export it
> only if they return one of the tuples containing `{continue,Continue}`
> from another callback.  If such a `{continue,_}` tuple is used
> and the callback is not implemented, the process will exit
> with `undef` error.
""".

-doc(#{since => <<"OTP 21.0">>}).
-callback handle_continue(Info :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), action()} |
    {stop, Reason :: term(), NewState :: term()}.

-doc """
Handle server termination.

This function is called by a `gen_server` process
when it is about to terminate.

It is to be the opposite of [`Module:init/1`](`c:init/1`)
and do any necessary cleaning up.  When it returns,
the `gen_server` process terminates with `Reason`.
The return value is ignored.

`Reason` is a term denoting the stop reason and `State`
is the internal state of the `gen_server` process.

`Reason` depends on why the `gen_server` process is terminating.
If it is because another callback function has returned a stop tuple
`{stop,..}`, `Reason` has the value specified in that tuple.
If it is because of a failure, `Reason` is the error reason.

If the `gen_server` process is part of a supervision tree
and is ordered by its supervisor to terminate, this function is called
with `Reason=shutdown` if the following conditions apply:

- The `gen_server` process has been set to trap exit signals.
- The shutdown strategy as defined in the child specification
  of the supervisor is an integer time-out value, not `brutal_kill`.

Even if the `gen_server` process is _not_ part of a supervision tree,
this function is called if it receives an `'EXIT'` message from its parent.
`Reason` is the same as in the `'EXIT'` message.

If the `gen_server` process does not trap exits,
the `gen_server` process terminates immediately.

Notice that for any other reason than `normal`, `shutdown`, or
`{shutdown,Term}`, see `stop/3`, the `gen_server` process is assumed
to terminate because of an error, and an error report is issued
using `m:logger`.

When the gen_server process exits, an exit signal with the same reason
is sent to linked processes and ports.

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_server` module provides a default implementation
> with no cleanup.
""".
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-doc """
Update the server state after code change.

This function is called by a `gen_server` process when it is to update
its internal state during a release upgrade/downgrade, that is,
when the instruction `{update, Module, Change, ...}`, is specified
in the [`appup`](`e:sasl:appup.md`) file.

For more information, see section
[Release Handling Instructions](`e:system:release_handling.md#instr`)
in OTP Design Principles.

For an upgrade, `OldVsn` is `Vsn`, and for a downgrade, `OldVsn` is
`{down,Vsn}`.  `Vsn` is defined by the `vsn` attribute(s)
of the old version of the callback module `Module`.  If no such attribute
is defined, the version is the checksum of the Beam file.

`State` is the internal state of the `gen_server` process.

`Extra` is passed "as is" from the `{advanced,Extra}` part
of the update instruction.

If successful, the function must return the updated internal state.

If the function returns `{error,Reason}`,
the ongoing upgrade fails and rolls back to the old release.

> #### Note {: .info }
>
> If a release upgrade/downgrade with `Change = {advanced, Extra}`
> specified in the [`.appup`](`e:sasl:appup.md`) file is made when
> [`Module:code_change/3`](`c:code_change/3`) is not implemented,
> the callback call will crash with an `undef` error reason.
""".
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

-doc """
Format/limit the status value.

This function is called by a `gen_server` process
in in order to format/limit the server state
for debugging and logging purposes.

It is called in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked to get the
  `gen_server` status. `Opt` is set to the atom `normal`.
- The `gen_server` process terminates abnormally and logs an error.
  `Opt` is set to the atom `terminate`.

This function is useful for changing the form and appearance
of the `gen_server` status for these cases. A callback module
wishing to change the `sys:get_status/1,2` return value,
as well as how its status appears in termination error logs,
exports an instance of [`Module:format_status/2`](`c:format_status/2`)
that returns a term describing the current status
of the `gen_server` process.

`PDict` is the current value of the process dictionary
of the `gen_server` process..

`State` is the internal state of the `gen_server` process.

The function is to return `Status`, a term that changes the details
of the current state and status of the `gen_server` process.
There are no restrictions of the form `Status` can take,
but for the `sys:get_status/1,2` case (when `Opt` is `normal`),
the recommended form for the `Status` value is
`[{data, [{"State", Term}]}]`, where `Term` provides relevant details
of the `gen_server` state.  Following this recommendation is not required,
but it makes the callback module status consistent with the rest of
the `sys:get_status/1,2` return value.

One use for this function is to return compact alternative
state representations to avoid that large state terms are printed
in log files.

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_server` module provides a default implementation
> of this function that returns the callback module state.
""".
-deprecated_callback({format_status, 2, "use format_status/1 instead"}).
-doc(#{since => <<"OTP R13B04">>}).
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-doc """
A map that describes the `gen_server` status.

The keys are:
- **`state`** - The internal state of the `gen_server` process.
- **`message`** - The message that caused the server to terminate.
- **`reason`** - The reason that caused the server to terminate.
- **`log`** - The [sys log](`sys:log/2`) of the server.

New associations may be added to the status map without prior notice.
""".
-type format_status() ::
        #{ state => term(),
           message => term(),
           reason => term(),
           log => [sys:system_event()] }.

-doc """
Format/limit the status value.

This function is called by a `gen_server` process in in order to
format/limit the server state for debugging and logging purposes.

It is called in the following situations:

- [`sys:get_status/1,2`](`sys:get_status/1`) is invoked
  to get the `gen_server` status.
- The `gen_server` process terminates abnormally and logs an error.

This callback is used to limit the status of the process returned by
[`sys:get_status/1,2`](`sys:get_status/1`) or sent to `m:logger`.

The callback gets a map `Status` describing the current status
and shall return a map `NewStatus` with the same keys,
but it may transform some values.

Two possible use cases for this callback is to remove
sensitive information from the state to prevent it from being printed
in log files, or to compact large irrelevant status items
that would only clutter the logs.

Example:

```erlang
format_status(Status) ->
  maps:map(
    fun(state,State) ->
            maps:remove(private_key, State);
       (message,{password, _Pass}) ->
            {password, removed};
       (_,Value) ->
            Value
    end, Status).
```

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it. The
> `gen_server` module provides a default implementation
> of this function that returns the callback module state.
>
> If this callback is exported but fails,
> to hide possibly sensitive data,
> the default function will instead return the fact that
> [`Module:format_status/1`](`c:format_status/1`) has crashed.
""".
-doc(#{since => <<"OTP 25.0">>}).
-callback format_status(Status) -> NewStatus when
      Status    :: format_status(),
      NewStatus :: format_status().

-optional_callbacks(
    [handle_info/2, handle_continue/2, terminate/2, code_change/3,
     format_status/1, format_status/2]).



-doc """
Time-out timer start option, to select absolute time of expiry.

If `Abs` is `true` an absolute timer is started,
and if it is `false` a relative, which is the default.
See [`erlang:start_timer/4`](`erlang:start_timer/4`) for details.
""".
-type timeout_option() :: {abs, Abs :: boolean()}.

-type action() :: Timeout :: timeout() |
                  'hibernate' |
                  {'timeout', Time :: timeout(), Message :: term()} |
                  {'timeout', Time :: timeout(), Message :: term(), Options :: timeout_option() | [timeout_option()]} |
                  {'hibernate', Time :: timeout(), Message :: term()} |
                  {'hibernate', Time :: timeout(), Message :: term(), Options :: timeout_option() | [timeout_option()]} |
		  {'continue', Continue :: term()}.

-doc """
A call's reply destination.

Destination, given to the `gen_server` as the first argument
to the callback function [`Module:handle_call/3`](`c:handle_call/3`),
to be used by the when replying through `reply/2` (instead of
through the callback function's return value), to the process `Client`
that has called the `gen_server` using [`call/2,3`](`call/2`).
`Tag` is a term that is unique for this call/request instance.
""".
-type from() ::	{Client :: pid(), Tag :: reply_tag()}.

-doc "A handle that associates a reply to the corresponding request.".
-opaque reply_tag() :: gen:reply_tag().

-doc "An opaque request identifier. See `send_request/2` for details.".
-opaque request_id() :: gen:request_id().

-doc """
An opaque collection of request identifiers (`t:request_id/0`).

Each request identifier can be associated with a label
chosen by the user.  For more information see `reqids_new/0`.
""".
-opaque request_id_collection() :: gen:request_id_collection().

-doc """
Response time-out for an asynchronous call.

Used to set a time limit on how long to wait for a response using either
`receive_response/2`, `receive_response/3`, `wait_response/2`, or
`wait_response/3`. The time unit used is `millisecond`.

Currently valid values:

- **`0..4294967295`** - Time-out relative to current time in milliseconds.

- **`infinity`** - Infinite time-out. That is,
  the operation will never time out.

- **`{abs, Timeout}`** - An absolute
  [Erlang monotonic time](`erlang:monotonic_time/1`)
  time-out in milliseconds. That is, the operation will time out when
  [`erlang:monotonic_time(millisecond)`](`erlang:monotonic_time/1`)
  returns a value larger than or equal to `Timeout`.
  `Timeout` is not allowed to identify a time further into the future
  than `4294967295` milliseconds.  Specifying the time-out
  using an absolute value is especially handy when you have
  a deadline for responses corresponding to a complete collection
  of requests (`t:request_id_collection/0`), since you do not have to
  recalculate the relative time until the deadline over and over again.
""".
-type response_timeout() ::
        timeout() | {abs, integer()}.

%%%  -----------------------------------------------------------------
%%% Starts a generic server.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, term()} | {via, atom(), term()}
%%%    Mod  ::= atom(), callback module implementing the 'real' server
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{timeout, Timeout} | {debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% -----------------------------------------------------------------

-doc """
Server name specification: `local`, `global`, or `via` registered.

To be used when starting a `gen_server`.  See functions
[`start/3,4`](`start/3`),
[`start_link/3,4`](`start_link/3`),
[`start_monitor/3,4`](`start_monitor/3`),
[`enter_loop/3,4,5`](`enter_loop/3`), and the type `t:server_ref/0`.

- **`{local, LocalName}`** - Register the `gen_server` locally
  as `LocalName` using [`register/2`](`erlang:register/2`).

- **`{global, GlobalName}`** - Register the `gen_server` process id
  globally as `GlobalName` using `global:register_name/2`.

- **`{via, RegMod, ViaName}`** - Register the `gen_server` process
  with the registry represented by `RegMod`. The `RegMod` callback
  is to export the functions `register_name/2`, `unregister_name/1`,
  `whereis_name/1`, and `send/2`, which are to behave like
  the corresponding functions in `m:global`.
  Thus, `{via, global, GlobalName}` is a valid reference
  equivalent to `{global, GlobalName}`.
""".
-type server_name() :: % Duplicate of gen:emgr_name()
        {'local', LocalName :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.

-doc """
Server specification: `t:pid/0` or registered `t:server_name/0`.

To be used when addressing a `gen_server`.  See [`call/2,3`](`call/2`),
`cast/2`, `send_request/2`, `check_response/2`, `wait_response/2`,
[`stop/2,3`](`stop/1`) and the type `t:server_name/0`.

It can be:

- **`t:pid/0`** - The `gen_server`'s process identifier.

- **`LocalName`** - The `gen_server` is locally registered
  as `LocalName` with [`register/2`](`erlang:register/2`).

- **`{Name,Node}`** - The `gen_server` is locally registered
  on another node.

- **`{global, GlobalName}`** - The `gen_server` is globally registered
  in `m:global`.

- **`{via, RegMod, ViaName}`** - The `gen_server` is registered
  in an alternative process registry.  See the same term
  described for `t:server_name/0`.
""".
-type server_ref() :: % What gen:call/3,4 and gen:stop/1,3 accepts
        pid()
      | (LocalName :: atom())
      | {Name :: atom(), Node :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.

-doc """
Server start options for the [`start` functions](`start_link/3`).

Options that can be used when starting a `gen_server` server through,
for example, [`start_link/3,4`](`start_link/4`).

- **`{timeout, Timeout}`** - How many milliseconds
  the `gen_server` process is allowed to spend initializing
  or it is terminated and the start function returns `{error, timeout}`.

- **`{spawn_opt, SpawnOptions}`** - The `SpawnOptions` option list
  is passed to the function used to spawn the `gen_server`;
  see `t:proc_lib:start_spawn_option/0`).

  > #### Note {: .info }
  >
  > Using spawn option `monitor` is not allowed -
  > it causes a `badarg` failure.

- **`t:enter_loop_opt/0`** - See the type `t:enter_loop_opt/0`
  below for more start options that are also allowed
  by [`enter_loop/3,4,5`](`enter_loop/3`).
""".
-type start_opt() :: % Duplicate of gen:option()
        {'timeout', Timeout :: timeout()}
      | {'spawn_opt', SpawnOptions :: [proc_lib:start_spawn_option()]}
      | enter_loop_opt().
%%
-doc """
Server start options for the [`start`](`start_link/4`) or
[`enter_loop`](`enter_loop/5`) functions.

Options that can be used when starting a `gen_server` server through
[`enter_loop/3-5`](`enter_loop/5`) or the start functions such as
[`start_link/3,4`](`start_link/4`).

- **`{hibernate_after, HibernateAfterTimeout}`** - Specifies that the
  `gen_server` process awaits any message for `HibernateAfterTimeout`
  milliseconds and if no message is received, the process goes into
  hibernation automatically (by calling `proc_lib:hibernate/3`).

- **`{debug, Dbgs}`** - For every entry in `Dbgs`,
  the corresponding function in `m:sys` is called.
""".
-type enter_loop_opt() :: % Some gen:option()s works for enter_loop/*
	{'hibernate_after', HibernateAfterTimeout :: timeout()}
      | {'debug', Dbgs :: [sys:debug_option()]}.

-doc """
Return value from the [`start/3,4`](`start/3`) and
[`start_link/3,4`](`start_link/3`) functions.

- **`{ok, Pid}`** - The `gen_server` process was succesfully created and
  initialized, with the process identifier `Pid`.

- **`{error, {already_started, OtherPid}}`** - A process with the specified
  `ServerName` exists already with the process identifier `OtherPid`.
  This function failed to start a `gen_server`.  It exited with reason
  `normal` before calling [`Module:init/1`](`c:init/1`).

- **`{error, timeout}`** - The `gen_server` process failed to initialize
  since [`Module:init/1`](`c:init/1`) did not return within the
  [start time-out](`t:start_opt/0`). The `gen_server` process was killed
  with [`exit(_, kill)`](`erlang:exit/2`).

- **`ignore`** - The `gen_server` process failed to initialize since
  [`Module:init/1`](`c:init/1`) returned `ignore`.

- **`{error,Reason}`** - The `gen_server` process failed to initialize since
  [`Module:init/1`](`c:init/1`) returned `{stop,Reason}`, `{error,Reason}`,
  or it failed with reason `Reason`.

See [`Module:init/1`](`c:init/1`) about the exit reason
for the `gen_server` process when it fails to initialize.
""".
-type start_ret() :: % gen:start_ret() without monitor return
        {'ok', Pid :: pid()}
      | 'ignore'
      | {'error', Reason :: term()}.

-doc """
Return value from the [`start_monitor/3,4`](`start_monitor/3`) functions.

The same as type `t:start_ret/0` except that for a succesful start
it returns both the process identifier `Pid`
and a [`monitor/2,3`](`erlang:monitor/2`) [`MonRef`](`t:reference/0`).
""".
-type start_mon_ret() :: % gen:start_ret() with only monitor return
        {'ok', {Pid :: pid(), MonRef :: reference()}}
      | 'ignore'
      | {'error', Reason :: term()}.

%%% ---------------------------------------------------

-doc """
Start a server, neither linked nor registered.

Equivalent to `start/4` except that the `gen_server` process is not
registered with any [name service](`t:server_name/0`).
""".
-spec start(
	Module  :: module(),
        Args    :: term(),
        Options :: [start_opt()]
       ) ->
		   start_ret().
%%
start(Module, Args, Options)
  when is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, nolink, Module, Args, Options);
start(Module, Args, Options) ->
    error(badarg, [Module, Args, Options]).

-doc """
Start a server, registered but not linked.

Creates a standalone `gen_server` process, that is,
a `gen_server` process that is not part of a supervision tree,
and thus has no supervisor.

Other than that see `start_link/4`.
""".
-spec start(
	ServerName :: server_name(),
	Module     :: module(),
        Args       :: term(),
        Options    :: [start_opt()]
       ) ->
		   start_ret().
%%
start(ServerName, Module, Args, Options)
  when is_tuple(ServerName), is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, nolink, ServerName, Module, Args, Options);
start(ServerName, Module, Args, Options) ->
    error(badarg, [ServerName, Module, Args, Options]).

-doc """
Start a server, linked but not registered.

Equivalent to `start_link/4` except that the `gen_server` process is
not registered with any [name service](`t:server_name/0`).
""".
-spec start_link(
	Module  :: module(),
        Args    :: term(),
        Options :: [start_opt()]
       ) ->
		   start_ret().
%%
start_link(Module, Args, Options)
  when is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, link, Module, Args, Options);
start_link(Module, Args, Options) ->
    error(badarg, [Module, Args, Options]).

-doc """
Start a server, linked and registered.

Creates a `gen_server` process as part of a supervision tree.
This function is to be called, directly or indirectly, by the supervisor.
For example, it ensures that the `gen_server` process is spawned
as linked to the caller (supervisor).

The `gen_server` process calls [`Module:init/1`](`c:init/1`)
to initialize.  To ensure a synchronized startup procedure,
`start_link/3,4` does not return until [`Module:init/1`](`c:init/1`)
has returned or failed.

[`ServerName`](`t:server_name/0`) specifies with what name
and now to register the server name.  See type `t:server_name/0`
for different name registrations.

`Module` is the name of the callback module.

`Args` is any term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

See type `t:start_opt/0` for `Options` for starting
the `gen_server` process.

See type `t:start_ret/0` for a description this function's return values.

If `start_link/3,4` returns `ignore` or `{error, _}`,
the started `gen_server` process has terminated.  If an `'EXIT'` message
was delivered to the calling process (due to the process link),
that message has been consumed.

> #### Warning {: .warning }
>
> Before OTP 26.0, if the started `gen_server` process returned e.g.
> `{stop, Reason}` from [`Module:init/1`](`c:init/1`), this function
> could return `{error, Reason}` _before_ the started `m:gen_server` process
> had terminated so starting again might fail because VM resources
> such as the registered name was not yet unregistered. An `'EXIT'` message
> could arrive later to the process calling this function.
>
> But if the started `gen_server` process instead failed during
> [`Module:init/1`](`c:init/1`), a process link `{'EXIT', Pid, Reason}`
> message caused this function to return `{error, Reason}`,
> so the `'EXIT'` message had been consumed and the started
> `m:gen_server` process had terminated.
>
> Since it was impossible to tell the difference between these two cases
> from `start_link/3,4`'s return value, this inconsistency was cleaned up
> in OTP 26.0.

The difference between returning `{stop, _}` and `{error, _}` from
[`Module:init/1`](`c:init/1`), is that `{error, _}` results in a graceful
("silent") termination since the `gen_server` process exits
with reason `normal`.
""".
-spec start_link(
	ServerName :: server_name(),
	Module     :: module(),
        Args       :: term(),
        Options    :: [start_opt()]
       ) ->
		   start_ret().
%%
start_link(ServerName, Module, Args, Options)
  when is_tuple(ServerName), is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, link, ServerName, Module, Args, Options);
start_link(ServerName, Module, Args, Options) ->
    error(badarg, [ServerName, Module, Args, Options]).

-doc """
Start a server, monitored but neither linked nor registered.

Equivalent to `start_monitor/4` except that the `gen_server` process
is not registered with any [name service](`t:server_name/0`).
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(
	Module  :: module(),
        Args    :: term(),
        Options :: [start_opt()]
       ) ->
		   start_mon_ret().
%%
start_monitor(Module, Args, Options)
  when is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, monitor, Module, Args, Options);
start_monitor(Module, Args, Options) ->
    error(badarg, [Module, Args, Options]).

-doc """
Start a server, monitored and registered, but not linked.

Creates a standalone `gen_server` process, that is,
a `gen_server` process that is not part of a supervision tree
(and thus has no supervisor) and atomically sets up a monitor
to the newly created server.

Other than that see [`start_link/3,4`](`start_link/3`).
Note that the return value for a successful start differs in that
it returns a monitor `reference`.  See type `t:start_mon_ret/0`.

If the start is not successful, the caller will be blocked
until the monitor's `'DOWN'` message has been received
and removed from the message queue.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(
	ServerName :: server_name(),
	Module     :: module(),
        Args       :: term(),
        Options    :: [start_opt()]
       ) ->
		   start_mon_ret().
%%
start_monitor(ServerName, Module, Args, Options)
  when is_tuple(ServerName), is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, monitor, ServerName, Module, Args, Options);
start_monitor(ServerName, Module, Args, Options) ->
    error(badarg, [ServerName, Module, Args, Options]).


%% -----------------------------------------------------------------
%% Stop a generic server and wait for it to terminate.
%% If the server is located at another node, that node will
%% be monitored.
%% -----------------------------------------------------------------

-doc(#{equiv => stop(ServerRef, normal, infinity)}).
-doc(#{since => <<"OTP 18.0">>}).
-spec stop(
        ServerRef :: server_ref()
       ) -> ok.
%%
stop(ServerRef) ->
    gen:stop(ServerRef).

-doc """
Stop a server.

Orders the generic server specified by `ServerRef` to exit
with the specified `Reason` and waits for it to terminate.
The `gen_server` process calls [`Module:terminate/2`](`c:terminate/2`)
before exiting.

The function returns `ok` if the server terminates
with the expected reason. Any other reason than `normal`, `shutdown`,
or `{shutdown,Term}` causes an error report to be issued using `m:logger`.
An exit signal with the same reason is sent to linked processes and ports.

`Timeout` is an integer that specifies how many milliseconds to wait
for the server to terminate, or the atom `infinity` to wait indefinitely.
If the server has not terminated within the specified time,
the call exits the calling process with reason `timeout`.

If the process does not exist, the call exits the calling process
with reason `noproc`, or with reason `{nodedown,Node}`
if the connection fails to the remote `Node` where the server runs.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec stop(
	ServerRef :: server_ref(),
	Reason    :: term(),
	Timeout   :: timeout()
       ) -> ok.
%%
stop(ServerRef, Reason, Timeout) ->
    gen:stop(ServerRef, Reason, Timeout).

%% -----------------------------------------------------------------
%% Make a call to a generic server.
%% If the server is located at another node, that node will
%% be monitored.
%% If the client is trapping exits and is linked server termination
%% is handled here (? Shall we do that here (or rely on time-outs) ?).
%% -----------------------------------------------------------------

-doc(#{equiv => call(ServerRef, Request, 5000)}).
-spec call(
        ServerRef :: server_ref(),
        Request   :: term()
       ) ->
                  Reply :: term().
%%
call(ServerRef, Request) ->
    case catch gen:call(ServerRef, '$gen_call', Request) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [ServerRef, Request]}})
    end.

-doc """
Call a server: send request and wait for response.

Makes a synchronous call to the `ServerRef` of the `gen_server` process
by sending a request and waiting until a reply arrives
or a time-out occurs.  The `gen_server` process calls
[`Module:handle_call/3`](`c:handle_call/3`) to handle the request.

See also `ServerRef`'s type `t:server_ref/0`.

`Request` is any term that is passed as the first argument to
[`Module:handle_call/3`](`c:handle_call/3`).

`Timeout` is an integer that specifies how many milliseconds to wait
for a reply, or the atom `infinity` to wait indefinitely.  If no reply
is received within the specified time, this function exits the calling
process with an exit term containing `Reason = timeout` as described below.

> #### Note {: .info }
>
> Before OTP 24, if the caller uses (`try`...)`catch`
> to avoid process exit, and the server happens to just be late
> with the reply, it may arrive to the process message queue
> any time later. The calling process must therefore after
> catching a time-out exit be prepared to receive garbage message(s)
> of the form `{reference(), _}` and deal with them appropriately
> (discard them) so they do not clog the process message queue,
> or gets mistaken for other messages.
>
> Starting with OTP 24, `gen_server:call` uses process aliases,
> so late replies will not be received.

The return value `Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

This call may exit the calling process with an exit term on the form
`{Reason, Location}` where `Location = {gen_server, call, ArgList}`
and `Reason` can be (at least) one of:

- **`timeout`** - The call was aborted after waiting `Timeout` milliseconds
  for a reply, as described above.

- **`noproc`** - The `ServerRef` refers to a server by name (it is not a
  `t:pid/0`) and looking up the server process failed, or the `t:pid/0`
  was already terminated.

- **`{nodedown,Node}`** - The `ServerRef` refers to a server
  on the remote node `Node` and the connection to that node failed.

- **`calling_self`** - A call to `self/0` would hang indefinitely.

- **`shutdown`** - The server was stopped during the call
  by its supervisor.  See also `stop/3`.

- **`normal`\
  `{shutdown,Term}`** - The server stopped during the call
  by returning `{stop,Reason,_}` from one of its callbacks
  without replying to this call. See also `stop/3`.

- **`_OtherTerm`** - The server process exited during the call,
  with reason `Reason`. Either by returning `{stop,Reason,_}`
  from one of its callbacks (without replying to this call),
  by raising an exception, or due to getting an exit signal
  it did not trap.
""".
-spec call(
        ServerRef :: server_ref(),
        Request   :: term(),
        Timeout   :: timeout()
       ) ->
                  Reply :: term().
%%
call(ServerRef, Request, Timeout) ->
    case catch gen:call(ServerRef, '$gen_call', Request, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [ServerRef, Request, Timeout]}})
    end.

%% -----------------------------------------------------------------
%% Send a request to a generic server and return a Key which should be
%% used with wait_response/2 or check_response/2 to fetch the
%% result of the request.

-doc """
Send an asynchronous `call` request.

Sends `Request` to the `gen_server` process identified by `ServerRef`
and returns a request identifier `ReqId`.

The return value `ReqId` shall later be used with `receive_response/2`,
`wait_response/2`, or `check_response/2` to fetch the actual result
of the request.  Besides passing the request identifier directly
to these functions, it can also be stored in
a request identifier collection using `reqids_add/3`.
Such a collection of request identifiers can later be used
in order to get one response corresponding to a
request in the collection by passing the collection
as argument to `receive_response/3`, `wait_response/3`,
or `check_response/3`.  If you are about to store the request identifier
in a collection, you may want to consider using `send_request/4` instead.

The call
`gen_server:receive_response(gen_server:send_request(ServerRef, Request), Timeout)`
can be seen as equivalent to
[`gen_server:call(ServerRef, Request, Timeout)`](`call/3`),
ignoring the error handling.

The `gen_server` process calls [`Module:handle_call/3`](`c:handle_call/3`) to
handle the request.

See the type `t:server_ref/0` for the possible values for `ServerRef`.

`Request` is any term that is passed as the first argument to
[`Module:handle_call/3`](`c:handle_call/3`).
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec send_request(ServerRef::server_ref(), Request::term()) ->
          ReqId::request_id().

send_request(ServerRef, Request) ->
    try
        gen:send_request(ServerRef, '$gen_call', Request)
    catch
        error:badarg ->
            error(badarg, [ServerRef, Request])
    end.

-doc """
Send an asynchronous `call` request and add it
to a request identifier collection.

Sends `Request` to the `gen_server` process identified by `ServerRef`.
The `Label` will be associated with the request identifier
of the operation and added to the returned request identifier collection
`NewReqIdCollection`.  The collection can later be used in order to
get one response corresponding to a request in the collection
by passing the collection as argument to `receive_response/3`,
`wait_response/3`, or `check_response/3`.

The same as calling
[`reqids_add`](`reqids_add/3`)`(`[`send_request`](`send_request/2`)`(ServerRef, Request), Label, ReqIdCollection)`,
but slightly more efficient.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec send_request(ServerRef::server_ref(),
                   Request::term(),
                   Label::term(),
                   ReqIdCollection::request_id_collection()) ->
          NewReqIdCollection::request_id_collection().

send_request(ServerRef, Request, Label, ReqIdCol) ->
    try
        gen:send_request(ServerRef, '$gen_call', Request, Label, ReqIdCol)
    catch
        error:badarg ->
            error(badarg, [ServerRef, Request, Label, ReqIdCol])
    end.

-doc """
Wait for a request response.

Wait for the response to the request identifier `ReqId`. The request
must have been made by `send_request/2`, and it must have been made
by the same process calling this function.

`WaitTime` specifies how long to wait for a reply.
If no reply is received within the specified time,
the function returns `timeout` and no cleanup is done.
Thus the function can be invoked repeatedly until a reply is returned.

The return value `Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

The function returns an error if the `gen_server`
died before a reply was sent.

The difference between `receive_response/2` and
`wait_response/2` is that `receive_response/2` abandons
the request at time-out so that a potential future response is ignored,
while [`wait_response/2`](`wait_response/2`) does not.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec wait_response(ReqId, WaitTime) -> Result when
      ReqId :: request_id(),
      WaitTime :: response_timeout(),
      Response :: {reply, Reply::term()}
                | {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

wait_response(ReqId, WaitTime) ->
    try
        gen:wait_response(ReqId, WaitTime)
    catch
        error:badarg ->
            error(badarg, [ReqId, WaitTime])
    end.

-doc """
Wait for any request response in a collection.

Wait for a response in a `ReqIdCollection`.  All request identifiers
of `ReqIdCollection` must correspond to requests that have been made
using `send_request/2` or `send_request/4`, and all requests
must have been made by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [adding the request id](`reqids_add/3`) to a collection,
or when sending the request using `send_request/4`.

Compared to `wait_response/2`, the returned result or exception
associated with a specific request identifier will be wrapped
in a 3-tuple `{Response, Label, NewReqIdCollection}`.
`Response` is the value that would have been produced
by `wait_response/2`, `Label` is the value associated with
the specific [request identifier](`t:request_id/0`)
and `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned.

If no response is received before `WaitTime` has expired,
`timeout` is returned.  It is valid to continue waiting
for a response as many times as needed up until a response
has been received and completed by `check_response()`,
`receive_response()`, or `wait_response()`.

The difference between `receive_response/3` and `wait_response/3`
is that `receive_response/3` abandons requests at time-out
so that potential future responses are ignored, while
`wait_response/3` does not.

If `Delete` is `true`, the association with `Label`
has been deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`.  If `Delete` is `false`, `NewReqIdCollection`
will equal `ReqIdCollection`.  Note that deleting an association
is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
`wait_response/3`, `check_response/3`, and `receive_response/3`.

However, without deleting handled associations, the above
calls will not be able to detect when there are
no more outstanding requests to handle, so you will have to keep track
of this some other way than relying on a `no_request` return.
Note that if you pass a collection only containing
associations of already handled or abandoned requests
to this function, it will always block until `WaitTime` expires
and then return `timeout`.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec wait_response(ReqIdCollection, WaitTime, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      WaitTime :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

wait_response(ReqIdCol, WaitTime, Delete) ->
    try
        gen:wait_response(ReqIdCol, WaitTime, Delete)
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, WaitTime, Delete])
    end.

-doc """
Receive a request response.

Receive a response corresponding to the request identifier `ReqId`.
The request must have been made by `send_request/2`,
and it must have been made by the same process calling this function.

`Timeout` specifies how long to wait for a response.
If no response is received within the specified time,
this function returns `timeout`.  Assuming that the
server executes on a node supporting aliases (introduced in OTP 24)
the request will also be abandoned.  That is,
no response will be received after a time-out.
Otherwise, a stray response might be received at a later time.

The return value `Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

The function returns an error if the `gen_server` died
before a reply was sent.

The difference between `receive_response/2` and `wait_response/2`
is that `receive_response/2` abandons the request at time-out
so that a potential future response is ignored,
while `wait_response/2` does not.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec receive_response(ReqId, Timeout) -> Result when
      ReqId :: request_id(),
      Timeout :: response_timeout(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

receive_response(ReqId, Timeout) ->
    try
        gen:receive_response(ReqId, Timeout)
    catch
        error:badarg ->
            error(badarg, [ReqId, Timeout])
    end.

-doc """
Receive a request response in a collection.

Receive a response in `ReqIdCollection`. All request identifiers
of `ReqIdCollection` must correspond to requests that have been made
using `send_request/2` or `send_request/4`, and all requests
must have been made by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [adding the request id](`reqids_add/3`) to a collection,
or when sending the request using `send_request/4`.

Compared to `receive_response/2`, the returned result or exception
associated with a specific request identifier will be wrapped
in a 3-tuple `{Response, Label, NewReqIdCollection}`.
`Response` is the value that would have been produced
by `receive_response/2`, `Label` is the value associated with
the specific [request identifier](`t:request_id/0`)
and `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned.

`Timeout` specifies how long to wait for a response.  If no response
is received within the specified time, the function returns `timeout`.
Assuming that the server executes on a node supporting aliases
(introduced in OTP 24) all requests identified by `ReqIdCollection`
will also be abandoned.  That is, no responses will be received
after a time-out.  Otherwise, stray responses might be received
at a later time.

The difference between `receive_response/3` and `wait_response/3`
is that `receive_response/3` abandons the requests at time-out
so that potential future responses are ignored,
while [`wait_response/3`](`wait_response/3`) does not.

If `Delete` is `true`, the association with `Label`
is deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`. If `Delete` is `false`, `NewReqIdCollection`
will equal `ReqIdCollection`.  Note that deleting an association
is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
`receive_response/3`, `check_response/3`, and `wait_response/3`.

However, without deleting handled associations,
the above calls will not be able to detect when there are
no more outstanding requests to handle, so you will have to keep track
of this some other way than relying on a `no_request` return.
Note that if you pass a collection only containing
associations of already handled or abandoned requests to
this function, it will always block until `Timeout` expires
and then return `timeout`.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec receive_response(ReqIdCollection, Timeout, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      Timeout :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

receive_response(ReqIdCol, Timeout, Delete) ->
    try
        gen:receive_response(ReqIdCol, Timeout, Delete)
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, Timeout, Delete])
    end.

-doc """
Check if a received message is a request response.

Checks if `Msg` is a response corresponding to
the request identifier `ReqId`.  The request must have been made
by `send_request/2`, and by the same process calling this function.

If `Msg` is a reply to the handle `ReqId` the result of the request
is returned in `Reply`.  Otherwise this function returns `no_reply`
and no cleanup is done, and thus the function shall be invoked repeatedly
until the response is returned.

The return value `Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

If the `gen_statem` server process has died when this function
is called, that is; `Msg` reports the server's death,
this function returns an `error` return with the exit `Reason`.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec check_response(Msg, ReqId) -> Result when
      Msg :: term(),
      ReqId :: request_id(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: Response | 'no_reply'.

check_response(Msg, ReqId) ->
    try
        gen:check_response(Msg, ReqId)
    catch
        error:badarg ->
            error(badarg, [Msg, ReqId])
    end.

-doc """
Check if a received message is a request response in a collection.

Check if `Msg` is a response corresponding to a request identifier
stored in `ReqIdCollection`.  All request identifiers of `ReqIdCollection`
must correspond to requests that have been made using `send_request/2`
or `send_request/4`, by the process calling this function.

The `Label` in the response equals the `Label` associated
with the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [storing the request id](`reqids_add/3`) in a collection,
or when sending the request using `send_request/4`.

Compared to `check_response/2`, the returned result or exception
associated with a specific request identifier will be wrapped
in a 3-tuple `{Response, Label, NewReqIdCollection}`.
`Response` is the value that would have been produced
by `check_response/2`, `Label` is the value associated with
the specific [request identifier](`t:request_id/0`)
and `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned.

If `Msg` does not correspond to any of the request identifiers
in `ReqIdCollection`, `no_reply` is returned.

If `Delete` is `true`, the association with `Label`
has been deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`.  If `Delete` is `false`, `NewReqIdCollection`
will equal `ReqIdCollection`.  Note that deleting an association
is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
`check_response/3`, `receive_response/3`, and `wait_response/3`.

However, without deleting handled associations,
the above calls will not be able to detect when there are
no more outstanding requests to handle, so you will have to keep track
of this some other way than relying on a `no_request` return.
Note that if you pass a collection only containing
associations of already handled or abandoned requests to
this function, it will always return `no_reply`.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec check_response(Msg, ReqIdCollection, Delete) -> Result when
      Msg :: term(),
      ReqIdCollection :: request_id_collection(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'no_reply'.

check_response(Msg, ReqIdCol, Delete) ->
    try
        gen:check_response(Msg, ReqIdCol, Delete)
    catch
        error:badarg ->
            error(badarg, [Msg, ReqIdCol, Delete])
    end.

-doc """
Create an empty request identifier collection.

Returns a new empty request identifier collection.
A request identifier collection can be utilized to handle
multiple outstanding requests.

Request identifiers of requests made by `send_request/2`
can be stored in a collection using `reqids_add/3`.
Such a collection of request identifiers can later be used
in order to get one response corresponding to a request
in the collection by passing the collection as argument to
`receive_response/3`, `wait_response/3`, or, `check_response/3`.

`reqids_size/1` can be used to determine the number of
request identifiers in a collection.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_new() ->
          NewReqIdCollection::request_id_collection().

reqids_new() ->
    gen:reqids_new().

-doc "Returns the number of request identifiers in `ReqIdCollection`.".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_size(ReqIdCollection::request_id_collection()) ->
          non_neg_integer().

reqids_size(ReqIdCollection) ->
    try
        gen:reqids_size(ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqIdCollection])
    end.

-doc """
Store a request identifier in a colletion.

Stores `ReqId` and associates a `Label` with the request identifier
by adding this information to `ReqIdCollection` and returning
the resulting request identifier collection.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_add(ReqId::request_id(), Label::term(),
                 ReqIdCollection::request_id_collection()) ->
          NewReqIdCollection::request_id_collection().

reqids_add(ReqId, Label, ReqIdCollection) ->
    try
        gen:reqids_add(ReqId, Label, ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqId, Label, ReqIdCollection])
    end.

-doc """
Convert a request identifier collection to a list.

Returns a list of `{ReqId, Label}` tuples which corresponds to
all request identifiers with their associated labels
in [`ReqIdCollection`](`t:request_id_collection/0`).
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_to_list(ReqIdCollection::request_id_collection()) ->
          [{ReqId::request_id(), Label::term()}].

reqids_to_list(ReqIdCollection) ->
    try
        gen:reqids_to_list(ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqIdCollection])
    end.

%% -----------------------------------------------------------------
%% Make a cast to a generic server.
%% -----------------------------------------------------------------

-doc """
Cast a request to a server.

Sends an asynchronous request to the `gen_server`
[`ServerRef`](`t:server_ref/0`) and returns `ok` immediately,
ignoring if the destination node or `gen_server`
process does not exist.

The `gen_server` process calls
[`Module:handle_cast(Request, _)`](`c:handle_cast/2`)
to handle the request.
""".
-spec cast(
        ServerRef :: server_ref(),
        Request   :: term()) ->
          ok.
%%
cast({global,Name}, Request) ->
    catch global:send(Name, cast_msg(Request)),
    ok;
cast({via, Mod, Name}, Request) ->
    catch Mod:send(Name, cast_msg(Request)),
    ok;
cast({Name,Node}=Dest, Request) when is_atom(Name), is_atom(Node) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_atom(Dest) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_pid(Dest) ->
    do_cast(Dest, Request).

do_cast(Dest, Request) ->
    do_send(Dest, cast_msg(Request)),
    ok.

cast_msg(Request) -> {'$gen_cast',Request}.

%% -----------------------------------------------------------------
%% Send a reply to the client.
%% -----------------------------------------------------------------

-doc """
Send a reply to a client.

This function can be used by a `gen_server` process to explicitly send
a reply to a client that called [`call/2,3`](`call/2`) or
[`multi_call/2,3,4`](`multi_call/2`), when the reply cannot be passed
in the return value of [`Module:handle_call/3`](`c:handle_call/3`).

`Client` must be the `From` argument provided to the `c:handle_call/3`
callback function. `Reply` is any term passed back to the client
as the return value of `call/2,3` or `multi_call/2,3,4`.
""".
-spec reply(
        Client :: from(),
        Reply  :: term()
       ) ->
                   ok.
%%
reply(Client, Reply) ->
    gen:reply(Client, Reply).

%% -----------------------------------------------------------------
%% Asynchronous broadcast, returns nothing, it's just send 'n' pray
%%-----------------------------------------------------------------

-doc """
Cast a request to multiple nodes.

Equivalent to [`abcast(Nodes, Name, Request)`](`abcast/3`)
where `Nodes` is all nodes connected to the calling node,
including the calling node itself.
""".
-spec abcast(
        Name    :: atom(),
        Request :: term()
       ) ->
                  abcast.
%%
abcast(Name, Request) when is_atom(Name) ->
    do_abcast([node() | nodes()], Name, cast_msg(Request)).

-doc """
Cast a request to multiple nodes.

Sends an asynchronous request to the `gen_server` processes
locally registered as `Name` at the specified nodes.
The function returns immediately and ignores nodes that do not exist,
or where the `gen_server` `Name` does not exist.  The  `gen_server`
processes call [`Module:handle_cast/2`](`c:handle_cast/2`)
to handle the request.

For a description of the arguments,
see [`multi_call/2,3,4`](`multi_call/2`).
""".
-spec abcast(
        Nodes   :: [node()],
        Name    :: atom(),
        Request :: term()
       ) ->
                  abcast.
%%
abcast(Nodes, Name, Request) when is_list(Nodes), is_atom(Name) ->
    do_abcast(Nodes, Name, cast_msg(Request)).

do_abcast([Node|Nodes], Name, Msg) when is_atom(Node) ->
    do_send({Name,Node},Msg),
    do_abcast(Nodes, Name, Msg);
do_abcast([], _,_) -> abcast.

%%% -----------------------------------------------------------------
%%% Make a call to servers at several nodes.
%%% Returns: {[Replies],[BadNodes]}
%%% A time-out can be given
%%%
%%% A middleman process is used in case late answers arrives after
%%% the time-out. If they would be allowed to glog the callers message
%%% queue, it would probably become confused. Late answers will
%%% now arrive to the terminated middleman and so be discarded.
%%% -----------------------------------------------------------------

-doc """
Call servers on multiple nodes in parallel.

Equivalent to [`multi_call(Nodes, Name, Request)`](`multi_call/3`)
where `Nodes` is all nodes connected to the calling node,
including the calling node itself.
""".
-spec multi_call(
        Name    :: atom(),
        Request :: term()
       ) ->
                        {Replies ::
                           [{Node :: node(), Reply :: term()}],
                         BadNodes :: [node()]
                        }.
%%
multi_call(Name, Request)
  when is_atom(Name) ->
    multi_call([node() | nodes()], Name, Request, infinity).

-doc(#{equiv => multi_call(Nodes, Name, Request, infinity)}).
-spec multi_call(
        Nodes   :: [node()],
        Name    :: atom(),
        Request :: term()
       ) ->
                        {Replies ::
                           [{Node :: node(), Reply :: term()}],
                         BadNodes :: [node()]
                        }.
%%
multi_call(Nodes, Name, Request)
  when is_list(Nodes), is_atom(Name) ->
    multi_call(Nodes, Name, Request, infinity).

-doc """
Call servers on multiple nodes in parallel.

Makes a synchronous call to all `gen_server` processes
locally registered as `Name` at the specified nodes,
by first sending the request to the nodes, and then waiting
for the replies. The `gen_server` processes on the nodes call
[`Module:handle_call/3`](`c:handle_call/3`) to handle the request.

The function returns a tuple `{Replies, BadNodes}`,
where `Replies` is a list of `{Node, Reply}` tuples,
and `BadNodes` is a list of nodes that either did not exist,
where `Name` was not a registered `gen_server`,
or where it did not reply.

`Nodes` is a list of node names to which the request is to be sent.

`Name` is the locally registered name for each `gen_server` process.

`Request` is any term that is passed as the first argument to
[`Module:handle_call/3`](`c:handle_call/3`).

`Timeout` is an integer that specifies how many milliseconds
to wait for all replies, or the atom `infinity` to wait indefinitely.
If no reply is received from a node within the specified time,
the node is added to `BadNodes`.

When a reply `Reply` is received from the `gen_server` process
at a node `Node`, `{Node,Reply}` is added to `Replies`.
`Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

> #### Warning {: .warning }
>
> If one of the nodes cannot process monitors, for example,
> C or Java nodes, and the `gen_server` process is not started
> when the requests are sent, but starts within 2 seconds,
> this function waits the whole `Timeout`, which may be infinity.
>
> This problem does not exist if all nodes are Erlang nodes.

To prevent late answers (after the time-out)
from polluting the message queue of the caller,
a middleman process is used to do the calls.
Late answers are then discarded when they arrive to
the terminated middleman process.
""".
-spec multi_call(
        Nodes   :: [node()],
        Name    :: atom(),
        Request :: term(),
        Timeout :: timeout()
       ) ->
                        {Replies ::
                           [{Node :: node(), Reply :: term()}],
                         BadNodes :: [node()]
                        }.
%%
multi_call(Nodes, Name, Request, Timeout)
  when is_list(Nodes), is_atom(Name), ?is_rel_timeout(Timeout) ->
    Alias = alias(),
    try
        Timer = if Timeout == infinity -> undefined;
                   true -> erlang:start_timer(Timeout, self(), Alias)
                end,
        Reqs = mc_send(Nodes, Name, Alias, Request, Timer, []),
        mc_recv(Reqs, Alias, Timer, [], [])
    after
        _ = unalias(Alias)
    end.

-dialyzer({no_improper_lists, mc_send/6}).

mc_send([], _Name, _Alias, _Request, _Timer, Reqs) ->
    Reqs;
mc_send([Node|Nodes], Name, Alias, Request, Timer, Reqs) when is_atom(Node) ->
    NN = {Name, Node},
    Mon = try
              erlang:monitor(process, NN, [{tag, Alias}])
          catch
              error:badarg ->
                  %% Node not alive...
                  M = make_ref(),
                  Alias ! {Alias, M, process, NN, noconnection},
                  M
          end,
    try
        %% We use 'noconnect' since it is no point in bringing up a new
        %% connection if it was not brought up by the monitor signal...
        _ = erlang:send(NN,
                        {'$gen_call', {self(), [[alias|Alias]|Mon]}, Request},
                        [noconnect]),
        ok
    catch
        _:_ ->
            ok
    end,
    mc_send(Nodes, Name, Alias, Request, Timer, [[Node|Mon]|Reqs]);
mc_send(_BadNodes, _Name, Alias, _Request, Timer, Reqs) ->
    %% Cleanup then fail...
    unalias(Alias),
    mc_cancel_timer(Timer, Alias),
    _ = mc_recv_tmo(Reqs, Alias, [], []),
    error(badarg).

mc_recv([], Alias, Timer, Replies, BadNodes) ->
    mc_cancel_timer(Timer, Alias),
    unalias(Alias),
    {Replies, BadNodes};
mc_recv([[Node|Mon] | RestReqs] = Reqs, Alias, Timer, Replies, BadNodes) ->
    receive
        {[[alias|Alias]|Mon], Reply} ->
            erlang:demonitor(Mon, [flush]),
            mc_recv(RestReqs, Alias, Timer, [{Node,Reply}|Replies], BadNodes);
        {Alias, Mon, process, _, _} ->
            mc_recv(RestReqs, Alias, Timer, Replies, [Node|BadNodes]);
        {timeout, Timer, Alias} ->
            unalias(Alias),
            mc_recv_tmo(Reqs, Alias, Replies, BadNodes)
    end.

mc_recv_tmo([], _Alias, Replies, BadNodes) ->
    {Replies, BadNodes};
mc_recv_tmo([[Node|Mon] | RestReqs], Alias, Replies, BadNodes) ->
    erlang:demonitor(Mon),
    receive
        {[[alias|Alias]|Mon], Reply} ->
            mc_recv_tmo(RestReqs, Alias, [{Node,Reply}|Replies], BadNodes);
        {Alias, Mon, process, _, _} ->
            mc_recv_tmo(RestReqs, Alias, Replies, [Node|BadNodes])
    after
        0 ->
            mc_recv_tmo(RestReqs, Alias, Replies, [Node|BadNodes])
    end.

mc_cancel_timer(undefined, _Alias) ->
    ok;
mc_cancel_timer(Timer, Alias) ->
    case erlang:cancel_timer(Timer) of
        false ->
            receive
                {timeout, Timer, Alias} ->
                    ok
            end;
        _ ->
            ok
    end.

%%-----------------------------------------------------------------
%% enter_loop(Mod, Options, State, <ServerName>, <TimeOut>) ->_
%%
%% Description: Makes an existing process into a gen_server.
%%              The calling process will enter the gen_server receive
%%              loop and become a gen_server process.
%%              The process *must* have been started using one of the
%%              start functions in proc_lib, see proc_lib(3).
%%              The user is responsible for any initialization of the
%%              process, including registering a name for it.
%%-----------------------------------------------------------------

-doc(#{equiv => enter_loop(Mod, Options, State, self())}).
-spec enter_loop(
        Module  :: module(),
        Options :: [enter_loop_opt()],
        State   :: term()
       ) ->
                        no_return().
%%
enter_loop(Mod, Options, State)
  when is_atom(Mod), is_list(Options) ->
    enter_loop(Mod, Options, State, self(), infinity).

-doc """
Make the calling process become a `gen_server` process.

With argument `ServerName` equivalent to
[`enter_loop(Module, Options,
  State, ServerName, infinity)`](`enter_loop/5`).

With argument `How` equivalent to
[`enter_loop(Module, Options, State, self(), How)`](`enter_loop/5`).
""".
-spec enter_loop(
        Module     :: module(),
        Options    :: [enter_loop_opt()],
        State      :: term(),
        ServerName :: server_name() | pid()
       ) ->
          no_return();
                (
        Module     :: module(),
        Options    :: [enter_loop_opt()],
        State      :: term(),
	Action     :: action()
       ) ->
          no_return().
%%
enter_loop(Mod, Options, State, ServerName = {Scope, _})
  when is_atom(Mod), is_list(Options), Scope == local;
       is_atom(Mod), is_list(Options), Scope == global ->
    enter_loop(Mod, Options, State, ServerName, infinity);
%%
enter_loop(Mod, Options, State, ServerName = {via, _, _})
  when is_atom(Mod), is_list(Options) ->
    enter_loop(Mod, Options, State, ServerName, infinity);
%%
enter_loop(Mod, Options, State, Action)
  when is_atom(Mod), is_list(Options) ->
    enter_loop(Mod, Options, State, self(), Action).

-doc """
Make the calling process become a `gen_server` process.

Does not return, instead the calling process enters the `gen_server`
process receive loop and becomes a `gen_server` process.
The process _must_ have been started using one of the start functions
in `m:proc_lib`.  The user is responsible for any initialization
of the process, including registering a name for it.

This function is useful when a more complex initialization procedure
is needed than the `gen_server` [`Module:init/1`](`c:init/1`);
callback provides.

`Module`, `Options`, and `ServerName` have the same meanings
as when calling [`start[_link|_monitor]/3,4`](`start_link/3`)
or `ServerName` can be `self/0` for an anonymous server,
which is the same as calling an `enter_loop/3,4` function
without a `ServerName` argument.  However, if `ServerName`
is specified (and not as `self/0`), the process must have been registered
accordingly _before_ this function is called.

`State`, `Timeout`, `Hibernate` and `Cont` have the same meanings
as in the return value of [`Module:init/1`](`c:init/1`),
which is _not_ called when `enter_loop/3,4,5` is used.  Note that
to adhere to the [gen_server Behaviour](`e:system:gen_server_concepts.md`)
such a callback function needs to be defined, and it might as well
be the one used when starting the `gen_server` process
through `proc_lib`, and then be the one that calls `enter_loop/3,4,5`.
But if such a [`Module:init/1`](`c:init/1`) function,
in for example error cases, cannot call `enter_loop/3,4,5`,
it should return a value that follows the type specification
for [`Module:init/1`](`c:init/1`) such as `ignore`,
although that value will be lost when returning to the spawning function.

This function fails if the calling process was not started
by a `proc_lib` start function, or if it is not registered
according to `ServerName`.
""".
-spec enter_loop(
	   Module     :: module(),
	   Options    :: [enter_loop_opt()],
	   State      :: term(),
	   ServerName :: server_name() | pid(),
	   Action     :: action()
       ) ->
                        no_return().
%%
enter_loop(Mod, Options, State, ServerName, Action)
  when is_atom(Mod), is_list(Options) ->
    Name = gen:get_proc_name(ServerName),
    ServerData = server_data(gen:get_parent(), Name, Mod, gen:hibernate_after(Options)),
    case handle_action(ServerData, Action) of
        error ->
            gen:unregister_name(Name),
            exit({bad_action, Action});
        LoopAction ->
            loop(ServerData, State, LoopAction, gen:debug_options(Name, Options))
    end.

%%%========================================================================
%%% Gen-callback functions
%%%========================================================================

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
-doc false.
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = gen:name(Name0),
    ServerData = server_data(Parent, Name, Mod, gen:hibernate_after(Options)),
    Debug = gen:debug_options(Name, Options),
    case init_it(Mod, Args) of
	{ok, {ok, State}} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(ServerData, State, infinity, Debug);
	{ok, {ok, State, Action} = Return} ->
            case handle_action(ServerData, Action) of
		error ->
		    gen:unregister_name(Name0),
		    exit({bad_return_value, Return});
                LoopAction ->
                    proc_lib:init_ack(Starter, {ok, self()}),
                    loop(ServerData, State, LoopAction, Debug)
            end;
	{ok, {stop, Reason}} ->
	    %% For consistency, we must make sure that the
	    %% registered name (if any) is unregistered before
	    %% the parent process is notified about the failure.
	    %% (Otherwise, the parent process could get
	    %% an 'already_started' error if it immediately
	    %% tried starting the process again.)
	    gen:unregister_name(Name0),
            exit(Reason);
	{ok, {error, _Reason} = ERROR} ->
            %% The point of this clause is that we shall have a silent/graceful
            %% termination. The error reason will be returned to the
            %% 'Starter' ({error, Reason}), but *no* crash report.
	    gen:unregister_name(Name0),
	    proc_lib:init_fail(Starter, ERROR, {exit, normal});
	{ok, ignore} ->
	    gen:unregister_name(Name0),
            proc_lib:init_fail(Starter, ignore, {exit, normal});
	{ok, Else} ->
	    gen:unregister_name(Name0),
            exit({bad_return_value, Else});
	{'EXIT', Class, Reason, Stacktrace} ->
	    gen:unregister_name(Name0),
            erlang:raise(Class, Reason, Stacktrace)
    end.
init_it(Mod, Args) ->
    try
        {ok, Mod:init(Args)}
    catch
        throw:R -> {ok, R};
        Class:R:S -> {'EXIT', Class, R, S}
    end.


%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------

loop(ServerData, State, {continue, Continue} = Msg, Debug) ->
    Reply = try_handle_continue(ServerData, State, Continue),
    From  = undefined,
    case Debug of
        [] ->
            handle_common_reply(ServerData, State, Msg, From, Reply);
        _ ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, ServerData#server_data.name, Msg),
            handle_common_reply(ServerData, State, Msg, Debug1, From, Reply)
    end;
%%
loop(ServerData, State, LoopAction, Debug) ->
    case LoopAction of
        {timeout_zero, TimeoutMsg} ->
            decode_msg(ServerData, State, infinity, Debug, [], TimeoutMsg);
        {timeout, Timer, HibInf} ->
            loop(ServerData, State, HibInf, Debug, Timer);
        HibT ->
            loop(ServerData, State, HibT, Debug, [])
    end.

loop(ServerData, State, hibernate, Debug, Timer) ->
    receive
	Msg ->
	    erlang:garbage_collect(),
	    decode_msg(ServerData, State, hibernate, Debug, Timer, Msg)
    after 0 ->
	loop_hibernate(ServerData, State, Timer, Debug)
    end;
%%
loop(ServerData, State, infinity, Debug, Timer) ->
    receive
	Msg ->
	    decode_msg(ServerData, State, infinity, Debug, Timer, Msg)
    after ServerData#server_data.hibernate_after ->
	loop_hibernate(ServerData, State, Timer, Debug)
    end;
%%
loop(ServerData, State, Time, Debug, Timer)
  when ?is_rel_timeout(Time) ->
    receive
        Msg ->
            decode_msg(ServerData, State, Time, Debug, Timer, Msg)
    after Time  ->
            decode_msg(ServerData, State, infinity, Debug, Timer, timeout)
    end.

loop_hibernate(ServerData, State, Timer, Debug) ->
    erlang:hibernate(),
    loop_wakeup(ServerData, State, Timer, Debug).

loop_wakeup(ServerData, State, Timer, Debug) ->
    receive
	Msg ->
	    decode_msg(
              update_callback_cache(ServerData), State, hibernate, Debug, Timer, Msg)
    end.

cancel_timer([]) ->
    ok;
cancel_timer([TRef | _]) ->
    ok = erlang:cancel_timer(TRef, [{async, true}, {info, false}]).

-compile({inline, [server_data/4, update_callback_cache/1]}).

server_data(Parent, Name, Mod, HibernateAfter) ->
    #server_data{
       parent          = Parent,
       name            = Name,
       module          = Mod,
       hibernate_after = HibernateAfter,
       handle_call     = fun Mod:handle_call/3,
       handle_cast     = fun Mod:handle_cast/2,
       handle_info     = fun Mod:handle_info/2,
       handle_continue = fun Mod:handle_continue/2}.

update_callback_cache(#server_data{module = Mod} = ServerData) ->
    ServerData#server_data{
      handle_call     = fun Mod:handle_call/3,
      handle_cast     = fun Mod:handle_cast/2,
      handle_info     = fun Mod:handle_info/2,
      handle_continue = fun Mod:handle_continue/2}.

decode_msg(#server_data{parent = Parent, tag = Tag} = ServerData, State, HibT, Debug, Timer, Msg) ->
    case Msg of
        {system, From, Req} ->
            sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                                  [ServerData, State, HibT, Timer], HibT =:= hibernate);
        {'EXIT', Parent, Reason} ->
            terminate(ServerData, State, Msg, undefined, Reason, ?STACKTRACE(), Debug);
	{timeout, TRef, Tag} when TRef =:= hd(Timer) ->
            decode_msg(ServerData, State, tl(Timer), Debug);
	{timeout, _Stale, Tag} ->
	    loop(ServerData, State, HibT, Debug, Timer);
        _ ->
	    cancel_timer(Timer),
            decode_msg(ServerData, State, Msg, Debug)

    end.
%%
decode_msg(ServerData, State, Msg, []) ->
    handle_msg(ServerData, State, Msg);
decode_msg(#server_data{name = Name} = ServerData, State, Msg, Debug) ->
    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, {in, Msg}),
    handle_msg(ServerData, State, Msg, Debug1).

%%% ---------------------------------------------------
%%% Send/receive functions
%%% ---------------------------------------------------
do_send(Dest, Msg) ->
    try erlang:send(Dest, Msg)
    catch
        error:_ -> ok
    end,
    ok.

%% ---------------------------------------------------
%% Helper functions for try-catch of callbacks.
%% Returns the return value of the callback, or
%% {'EXIT', Class, Reason, Stack} (if an exception occurs)
%%
%% The Class, Reason and Stack are given to erlang:raise/3
%% to make sure proc_lib receives the proper reasons and
%% stacktraces.
%% ---------------------------------------------------

-compile({inline, [try_dispatch/3]}).
try_dispatch(ServerData, State, {'$gen_cast', Msg}) ->
    try_handle_cast(ServerData, State, Msg);
try_dispatch(ServerData, State, Info) ->
    try_handle_info(ServerData, State, Info).

try_handle_continue(#server_data{handle_continue = HandleContinue}, State, Msg) ->
    try
        {ok, HandleContinue(Msg, State)}
    catch
        throw:R ->
            {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end.

try_handle_info(#server_data{module = Mod, handle_info = HandleInfo}, State, Msg) ->
    try
        {ok, HandleInfo(Msg, State)}
    catch
        throw:R ->
            {ok, R};
        error:undef = R:Stacktrace ->
            case erlang:function_exported(Mod, handle_info, 2) of
                false ->
                    ?LOG_WARNING(
                    #{label=>{gen_server,no_handle_info},
                        module=>Mod,
                        message=>Msg},
                    #{domain=>[otp],
                        report_cb=>fun gen_server:format_log/2,
                        error_logger=>
                            #{tag=>warning_msg,
                            report_cb=>fun gen_server:format_log/1}}),
                    {ok, {noreply, State}};
                true ->
                    {'EXIT', error, R, Stacktrace}
            end;
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end.

try_handle_cast(#server_data{handle_cast = HandleCast}, State, Msg) ->
    try
        {ok, HandleCast(Msg, State)}
    catch
        throw:R ->
            {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end.

try_handle_call(#server_data{handle_call = HandleCall}, State, Msg, From) ->
    try
        {ok, HandleCall(Msg, From, State)}
    catch
        throw:R ->
            {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end.

try_terminate(#server_data{module = Mod}, State, Reason) ->
    case erlang:function_exported(Mod, terminate, 2) of
        true ->
            try
                {ok, Mod:terminate(Reason, State)}
            catch
                throw:R ->
                    {ok, R};
                Class:R:Stacktrace ->
                    {'EXIT', Class, R, Stacktrace}
            end;
        false ->
            {ok, ok}
    end.


%%% ---------------------------------------------------
%%% Message handling functions
%%% ---------------------------------------------------

handle_msg(ServerData, State, {'$gen_call', From, Msg}) ->
    case try_handle_call(ServerData, State, Msg, From) of
	{ok, {reply, Reply, NState}} ->
	    reply(From, Reply),
	    loop(ServerData, NState, infinity, []);
	{ok, {reply, Reply, NState, Action} = Return} ->
            case handle_action(ServerData, Action) of
                error ->
		    terminate(ServerData, State, Msg, From, {bad_return_value, Return}, ?STACKTRACE(), []);
                LoopAction ->
                    reply(From, Reply),
                    loop(ServerData, NState, LoopAction, [])
            end;
	{ok, {stop, Reason, Reply, NState}} ->
	    try
		terminate(ServerData, NState, Msg, From, Reason, ?STACKTRACE(), [])
	    after
		reply(From, Reply)
	    end;
	Result ->
	    handle_common_reply(ServerData, State, Msg, From, Result)
    end;
handle_msg(ServerData, State, Msg) ->
    Reply = try_dispatch(ServerData, State, Msg),
    handle_common_reply(ServerData, State, Msg, undefined, Reply).

handle_msg(#server_data{name = Name} = ServerData, State, {'$gen_call', From, Msg}, Debug) ->
    case try_handle_call(ServerData, State, Msg, From) of
	{ok, {reply, Reply, NState}} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(ServerData, NState, infinity, Debug1);
	{ok, {reply, Reply, NState, Action} = Return} ->
            case handle_action(ServerData, Action) of
		error ->
		    terminate(ServerData, State, Msg, From, {bad_return_value, Return}, ?STACKTRACE(), []);
                LoopAction ->
                    Debug1 = reply(Name, From, Reply, NState, Debug),
                    loop(ServerData, NState, LoopAction, Debug1)
            end;
	{ok, {stop, Reason, Reply, NState}} ->
	    try
		terminate(ServerData, NState, Msg, From, Reason, ?STACKTRACE(), Debug)
	    after
		_ = reply(Name, From, Reply, NState, Debug)
	    end;
	Result ->
	    handle_common_reply(ServerData, State, Msg, Debug, From, Result)
    end;
handle_msg(ServerData, State, Msg, Debug) ->
    Reply = try_dispatch(ServerData, State, Msg),
    handle_common_reply(ServerData, State, Msg, Debug, undefined, Reply).

handle_common_reply(ServerData, State, Msg, From, Reply) ->
    case Reply of
	{ok, {noreply, NState}} ->
	    loop(ServerData, NState, infinity, []);
	{ok, {noreply, NState, Action} = Return} ->
            case handle_action(ServerData, Action) of
		error ->
		    terminate(ServerData, State, Msg, From, {bad_return_value, Return}, ?STACKTRACE(), []);
                LoopAction ->
                    loop(ServerData, NState, LoopAction, [])
	    end;
	{ok, {stop, Reason, NState}} ->
	    terminate(ServerData, NState, Msg, From, Reason, ?STACKTRACE(), []);
	{'EXIT', Class, Reason, Stacktrace} ->
	    terminate(ServerData, State, Msg, From, Class, Reason, Stacktrace, []);
	{ok, BadReturn} ->
	    terminate(ServerData, State, Msg, From, {bad_return_value, BadReturn}, ?STACKTRACE(), [])
    end.

handle_common_reply(#server_data{name = Name} = ServerData, State, Msg, Debug, From, Reply) ->
    case Reply of
	{ok, {noreply, NState}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(ServerData, NState, infinity, Debug1);
	{ok, {noreply, NState, Action} = Return} ->
            case handle_action(ServerData, Action) of
		error ->
		    terminate(ServerData, State, Msg, From, {bad_return_value, Return}, ?STACKTRACE(), Debug);
                LoopAction ->
                    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, {noreply, NState}),
                    loop(ServerData, NState, LoopAction, Debug1)
	    end;
	{ok, {stop, Reason, NState}} ->
	    terminate(ServerData, NState, Msg, From, Reason, ?STACKTRACE(), Debug);
	{'EXIT', Class, Reason, Stacktrace} ->
	    terminate(ServerData, State, Msg, From, Class, Reason, Stacktrace, Debug);
	{ok, BadReturn} ->
	    terminate(ServerData, State, Msg, From, {bad_return_value, BadReturn}, ?STACKTRACE(), Debug)
    end.

reply(Name, From, Reply, State, Debug) ->
    reply(From, Reply),
    sys:handle_debug(Debug, fun print_event/3, Name,
                     {out, Reply, From, State} ).

handle_action(ServerData, Action) ->
    case Action of
        {continue, _} = Cont                    -> Cont;
        hibernate                               -> hibernate;
        Timeout when ?is_rel_timeout(Timeout)   -> Timeout;
        {timeout, T, M} ->
            handle_timeout(ServerData, T, M, infinity);
        {hibernate, T, M} ->
            handle_timeout(ServerData, T, M, hibernate);
        {timeout, T, M, Opts} ->
            handle_timeout(ServerData, T, M, infinity, listify(Opts), false);
        {hibernate, T, M, Opts} ->
            handle_timeout(ServerData, T, M, hibernate, listify(Opts), false);
        _ ->
            error
    end.

handle_timeout(ServerData, T, M, HibInf) ->
    if
        ?is_rel_timeout(T) ->
            handle_timer(ServerData, T, M, HibInf, false);
        true ->
            error
    end.

handle_timeout(ServerData, T, M, HibInf, [], Abs) when ?is_timeout(Abs, T) ->
    handle_timer(ServerData, T, M, HibInf, Abs);
handle_timeout(ServerData, T, M, HibInf, [{abs, Abs} | Opts], _Abs) when is_boolean(Abs) ->
    handle_timeout(ServerData, T, M, HibInf, Opts, Abs);
handle_timeout(_ServerData, _T, _M, _HibInf, _Opts, _Abs) ->
    error.

handle_timer(_ServerData, 0, M, HibInf, false) ->
    case HibInf of
        hibernate -> garbage_collect();
        infinity  -> true
    end,
    {timeout_zero, M};
handle_timer(#server_data{tag = Tag}, T, M, HibInf, Abs) ->
    TRef =
        case Abs of
            true ->
                erlang:start_timer(T, self(), Tag, [{abs,Abs}]);
            false ->
                erlang:start_timer(T, self(), Tag)
        end,
    {timeout, [TRef | M], HibInf}.

-compile({inline, [listify/1]}).
listify(Opts) when is_list(Opts) ->
    Opts;
listify(Opt) ->
    [Opt].

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
-doc false.
system_continue(
  Parent, Debug, [#server_data{parent=Parent} = ServerData, State, HibT, Timer]) ->
    loop(update_callback_cache(ServerData), State, HibT, Debug, Timer).

-doc false.
-spec system_terminate(_, _, _, [_]) -> no_return().

system_terminate(Reason, _Parent, Debug, [ServerData, State, _Timer, _Hib]) ->
    terminate(ServerData, State, [], undefined, Reason, ?STACKTRACE(), Debug).

-doc false.
system_code_change([#server_data{module = Mod} = ServerData, State, Timer, Hib], _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, State, Extra) of
        {ok, NewState} -> {ok, [ServerData, NewState, Timer, Hib]};
        Else -> Else
    end.

-doc false.
system_get_state([_ServerData, State, _Timer, _Hib]) ->
    {ok, State}.

-doc false.
system_replace_state(StateFun, [ServerData, State, Timer, Hib]) ->
    NState = StateFun(State),
    {ok, NState, [ServerData, NState, Timer, Hib]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{'$gen_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~tp got call ~tp from ~tw~n",
		      [Name, Call, From]);
	{'$gen_cast', Cast} ->
	    io:format(Dev, "*DBG* ~tp got cast ~tp~n",
		      [Name, Cast]);
	_ ->
	    io:format(Dev, "*DBG* ~tp got ~tp~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, {To,_Tag}, State}, Name) ->
    io:format(Dev, "*DBG* ~tp sent ~tp to ~tw, new state ~tp~n",
	      [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~tp new state ~tp~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~tp dbg  ~tp~n", [Name, Event]).


%%% ---------------------------------------------------
%%% Terminate the server.
%%%
%%% terminate/8 is triggered by {stop, Reason} or bad
%%% return values. The stacktrace is generated via the
%%% ?STACKTRACE() macro and the ReportReason must not
%%% be wrapped in tuples.
%%%
%%% terminate/9 is triggered in case of error/exit in
%%% the user callback. In this case the report reason
%%% always includes the user stacktrace.
%%%
%%% The reason received in the terminate/2 callbacks
%%% always includes the stacktrace for errors and never
%%% for exits.
%%% ---------------------------------------------------

-spec terminate(_, _, _, _, _, _, _) -> no_return().
terminate(ServerData, State, Msg, From, Reason, Stacktrace, Debug) ->
  terminate(ServerData, State, Msg, From, exit, Reason, Stacktrace, Debug, false).

-spec terminate(_, _, _, _, _, _, _, _) -> no_return().
terminate(ServerData, State, Msg, From, Class, Reason, Stacktrace, Debug) ->
  terminate(ServerData, State, Msg, From, Class, Reason, Stacktrace, Debug, true).

-spec terminate(_, _, _, _, _, _, _, _, _) -> no_return().
terminate(ServerData, State, Msg, From, Class, Reason, Stacktrace, Debug, ReportStacktrace) ->
    Reply = try_terminate(ServerData, State, catch_result(Class, Reason, Stacktrace)),
    case Reply of
	{'EXIT', C, R, S} ->
	    error_info(ServerData, State, Msg, From, R, S, Debug),
	    erlang:raise(C, R, S);
	_ ->
	    case {Class, Reason} of
		{exit, normal} -> ok;
		{exit, shutdown} -> ok;
		{exit, {shutdown,_}} -> ok;
		_ when ReportStacktrace ->
		    error_info(ServerData, State, Msg, From, Reason, Stacktrace, Debug);
                _ ->
		    error_info(ServerData, State, Msg, From, Reason, undefined, Debug)
	    end
    end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	_ ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

%% What an old style `catch` would return
catch_result(error, Reason, Stacktrace) -> {Reason, Stacktrace};
catch_result(exit, Reason, _Stacktrace) -> Reason.

error_info(#server_data{name = application_controller}, _State, _Msg, _From, _Reason, _ST, _Debug) ->
    %% OTP-5811 Do not send an error report if it's the system process
    %% application_controller which is terminating - let init take care
    %% of it instead
    ok;
error_info(#server_data{name = Name, module = Mod}, State, Msg, From, Reason, ST, Debug) ->
    Log = sys:get_log(Debug),
    Status =
        gen:format_status(Mod, terminate,
                          #{ reason => Reason,
                             state => State,
                             message => Msg,
                             log => Log },
                          [get(),State]),
    ReportReason =
        if ST == undefined ->
                %% When ST is undefined, it should not be included in the
                %% reported reason for the crash as it is then caused
                %% by an invalid return from a callback and thus thus the
                %% stacktrace is irrelevant.
                maps:get(reason, Status);
           true ->
                {maps:get(reason, Status), ST}
        end,

    ?LOG_ERROR(#{label=>{gen_server,terminate},
                 name=>Name,
                 last_message=>maps:get(message,Status),
                 state=>maps:get('EXIT',Status,maps:get('$status',Status,maps:get(state,Status))),
                 log=>format_log_state(Mod,maps:get(log,Status)),
                 reason=>ReportReason,
                 client_info=>client_stacktrace(From),
                 process_label=>proc_lib:get_label(self())},
               #{domain=>[otp],
                 report_cb=>fun gen_server:format_log/2,
                 error_logger=>#{tag=>error,
                                 report_cb=>fun gen_server:format_log/1}}),
    ok.

client_stacktrace(undefined) ->
    undefined;
client_stacktrace({From,_Tag}) ->
    client_stacktrace(From);
client_stacktrace(From) when is_pid(From), node(From) =:= node() ->
    case process_info(From, [current_stacktrace, registered_name]) of
        undefined ->
            {From,dead};
        [{current_stacktrace, Stacktrace}, {registered_name, []}]  ->
            {From,{From,Stacktrace}};
        [{current_stacktrace, Stacktrace}, {registered_name, Name}]  ->
            {From,{Name,Stacktrace}}
    end;
client_stacktrace(From) when is_pid(From) ->
    {From,remote}.


%% format_log/1 is the report callback used by Logger handler
%% error_logger only. It is kept for backwards compatibility with
%% legacy error_logger event handlers. This function must always
%% return {Format,Args} compatible with the arguments in this module's
%% calls to error_logger prior to OTP-21.0.
-doc false.
format_log(Report) ->
    Depth = error_logger:get_format_depth(),
    FormatOpts = #{chars_limit => unlimited,
                   depth => Depth,
                   single_line => false,
                   encoding => utf8},
    format_log_multi(limit_report(Report,Depth),FormatOpts).

limit_report(Report,unlimited) ->
    Report;
limit_report(#{label:={gen_server,terminate},
               last_message:=Msg,
               state:=State,
               log:=Log,
               reason:=Reason,
               client_info:=Client,
               process_label:=ProcessLabel}=Report,
            Depth) ->
    Report#{last_message=>io_lib:limit_term(Msg,Depth),
            state=>io_lib:limit_term(State,Depth),
            log=>[io_lib:limit_term(L,Depth)||L<-Log],
            reason=>io_lib:limit_term(Reason,Depth),
            client_info=>limit_client_report(Client,Depth),
            process_label=>io_lib:limit_term(ProcessLabel,Depth)};
limit_report(#{label:={gen_server,no_handle_info},
               message:=Msg}=Report,Depth) ->
    Report#{message=>io_lib:limit_term(Msg,Depth)}.

limit_client_report({From,{Name,Stacktrace}},Depth) ->
    {From,{Name,io_lib:limit_term(Stacktrace,Depth)}};
limit_client_report(Client,_) ->
    Client.

%% format_log/2 is the report callback for any Logger handler, except
%% error_logger.
-doc false.
format_log(Report, FormatOpts0) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    FormatOpts = maps:merge(Default,FormatOpts0),
    IoOpts =
        case FormatOpts of
            #{chars_limit:=unlimited} ->
                [];
            #{chars_limit:=Limit} ->
                [{chars_limit,Limit}]
        end,
    {Format,Args} = format_log_single(Report, FormatOpts),
    io_lib:format(Format, Args, IoOpts).

format_log_single(#{label:={gen_server,terminate},
                    name:=Name,
                    last_message:=Msg,
                    state:=State,
                    log:=Log,
                    reason:=Reason,
                    client_info:=Client,
                    process_label:=ProcessLabel},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format1 = lists:append(["Generic server ",P," terminating",
                            case ProcessLabel of
                                undefined -> "";
                                _ -> ". Label: "++P
                            end,
                            ". Reason: ",P,
                            ". Last message: ", P, ". State: ",P,"."]),
    {ServerLogFormat,ServerLogArgs} = format_server_log_single(Log,FormatOpts),
    {ClientLogFormat,ClientLogArgs} = format_client_log_single(Client,FormatOpts),

    Args1 =
        case Depth of
            unlimited ->
                [Name] ++
                case ProcessLabel of
                    undefined -> [];
                    _ -> [ProcessLabel]
                end ++
                [fix_reason(Reason),Msg,State];
            _ ->
                [Name,Depth] ++
                case ProcessLabel of
                    undefined -> [];
                    _ -> [ProcessLabel,Depth]
                end ++
                [fix_reason(Reason),Depth,Msg,Depth,State,Depth]
        end,
    {Format1++ServerLogFormat++ClientLogFormat,
     Args1++ServerLogArgs++ClientLogArgs};
format_log_single(#{label:={gen_server,no_handle_info},
                    module:=Mod,
                    message:=Msg},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format = lists:append(["Undefined handle_info in ",P,
                           ". Unhandled message: ",P,"."]),
    Args =
        case Depth of
            unlimited ->
                [Mod,Msg];
            _ ->
                [Mod,Depth,Msg,Depth]
        end,
    {Format,Args};
format_log_single(Report,FormatOpts) ->
    format_log_multi(Report,FormatOpts).

format_log_multi(#{label:={gen_server,terminate},
                   name:=Name,
                   last_message:=Msg,
                   state:=State,
                   log:=Log,
                   reason:=Reason,
                   client_info:=Client,
                   process_label:=ProcessLabel},
                 #{depth:=Depth}=FormatOpts) ->
    Reason1 = fix_reason(Reason),
    {ClientFmt,ClientArgs} = format_client_log(Client,FormatOpts),
    P = p(FormatOpts),
    Format =
        lists:append(
          ["** Generic server ",P," terminating \n"] ++
          case ProcessLabel of
              undefined -> [];
              _ -> ["** Process label == ",P,"~n"]
          end ++
          ["** Last message in was ",P,"~n"
           "** When Server state == ",P,"~n"
           "** Reason for termination ==~n** ",P,"~n"] ++
               case Log of
                   [] -> [];
                   _ -> ["** Log ==~n** ["|
                         lists:join(",~n    ",lists:duplicate(length(Log),P))]++
                            ["]~n"]
               end) ++ ClientFmt,
    Args =
        case Depth of
            unlimited ->
                [Name] ++
                case ProcessLabel of
                    undefined -> [];
                    _ -> [ProcessLabel]
                end ++
                [Msg, State, Reason1] ++ Log ++ ClientArgs;
            _ ->
                [Name, Depth] ++
                case ProcessLabel of
                    undefined -> [];
                    _ -> [ProcessLabel, Depth]
                end ++
                [Msg, Depth, State, Depth, Reason1, Depth] ++
                    case Log of
                        [] -> [];
                        _ -> lists:flatmap(fun(L) -> [L, Depth] end, Log)
                    end ++ ClientArgs
        end,
    {Format,Args};
format_log_multi(#{label:={gen_server,no_handle_info},
                   module:=Mod,
                   message:=Msg},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format =
        "** Undefined handle_info in ~p~n"
        "** Unhandled message: "++P++"~n",
    Args =
        case Depth of
            unlimited ->
                [Mod,Msg];
            _ ->
                [Mod,Msg,Depth]
                    end,
    {Format,Args}.

fix_reason({undef,[{M,F,A,L}|MFAs]}=Reason) ->
    case code:is_loaded(M) of
        false ->
            {'module could not be loaded',[{M,F,A,L}|MFAs]};
        _ ->
            case erlang:function_exported(M, F, length(A)) of
                true ->
                    Reason;
                false ->
                    {'function not exported',[{M,F,A,L}|MFAs]}
            end
    end;
fix_reason(Reason) ->
    Reason.

format_server_log_single([],_) ->
    {"",[]};
format_server_log_single(Log,FormatOpts) ->
    Args =
        case maps:get(depth,FormatOpts) of
            unlimited ->
                [Log];
            Depth ->
                [Log, Depth]
        end,
     {" Log: "++p(FormatOpts),Args}.

format_client_log_single(undefined,_) ->
    {"",[]};
format_client_log_single({From,dead},_) ->
    {" Client ~0p is dead.",[From]};
format_client_log_single({From,remote},_) ->
    {" Client ~0p is remote on node ~0p.", [From, node(From)]};
format_client_log_single({_From,{Name,Stacktrace0}},FormatOpts) ->
    P = p(FormatOpts),
    %% Minimize the stacktrace a bit for single line reports. This is
    %% hopefully enough to point out the position.
    Stacktrace = lists:sublist(Stacktrace0,4),
    Args =
        case maps:get(depth,FormatOpts) of
            unlimited ->
                [Name, Stacktrace];
            Depth ->
                [Name, Depth, Stacktrace, Depth]
        end,
    {" Client "++P++" stacktrace: "++P++".", Args}.

format_client_log(undefined,_) ->
    {"", []};
format_client_log({From,dead},_) ->
    {"** Client ~p is dead~n", [From]};
format_client_log({From,remote},_) ->
    {"** Client ~p is remote on node ~p~n", [From, node(From)]};
format_client_log({_From,{Name,Stacktrace}},FormatOpts) ->
    P = p(FormatOpts),
    Format = lists:append(["** Client ",P," stacktrace~n",
                           "** ",P,"~n"]),
    Args =
        case maps:get(depth,FormatOpts) of
            unlimited ->
                [Name, Stacktrace];
            Depth ->
                [Name, Depth, Stacktrace, Depth]
        end,
    {Format,Args}.

p(#{single_line:=Single,depth:=Depth,encoding:=Enc}) ->
    "~"++single(Single)++mod(Enc)++p(Depth);
p(unlimited) ->
    "p";
p(_Depth) ->
    "P".

single(true) -> "0";
single(false) -> "".

mod(latin1) -> "";
mod(_) -> "t".

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
-doc false.
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [#server_data{parent=Parent, name=Name, module=Mod}, State, _Timer, _Hib]] = StatusData,
    Header = gen:format_status_header("Status for generic server", Name),
    Status =
        case gen:format_status(Mod, Opt, #{ state => State, log => sys:get_log(Debug) },
                               [PDict, State]) of
            #{ 'EXIT' := R } = M ->
                M#{ '$status' => [{data,[{"State",R}]}] };
            %% Status is set when the old format_status/2 is called,
            %% so we do a little backwards compatibility dance here
            #{ '$status' := S } = M when is_list(S) -> M;
            #{ '$status' := S } = M -> M#{ '$status' := [S] };
            #{ state := S } = M ->
                M#{ '$status' => [{data, [{"State",S}] }] }
        end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", format_log_state(Mod, maps:get(log,Status))}]} |
     maps:get('$status',Status)].

format_log_state(Mod, Log) ->
    %% If format_status/1 was exported, the log has already been handled by
    %% that call, so we should not pass all log events into the callback again.
    case erlang:function_exported(Mod, format_status, 1) of
        false ->
            [case Event of
                 {out,Msg,From,State} ->
                     Status = gen:format_status(
                                Mod, terminate, #{ state => State },
                                [get(), State]),
                     {out, Msg, From, maps:get(state, Status) };
                 {noreply,State} ->
                     Status = gen:format_status(
                                Mod, terminate, #{ state => State },
                                [get(), State]),
                     {noreply, maps:get(state, Status)};
                 _ -> Event
             end || Event <- Log];
        true ->
            Log
    end.
