%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(gen_event).
-moduledoc """
Generic event handling behavior.

This behavior module provides event handling functionality.
It consists of a generic event manager process with any number of
event handlers that are added and deleted dynamically.

An event manager implemented using this module has a standard set of
interface functions and includes functionality for tracing
and error reporting.  It also fits into an OTP supervision tree.
For more information, see [OTP Design Principles](`e:system:events.md`).

Each event handler is implemented as a callback module
exporting a predefined set of functions. The relationship between
the behavior functions and the callback functions is as follows:

```text
gen_event module                   Callback module
----------------                   ---------------
gen_event:start
gen_event:start_monitor
gen_event:start_link       ----->  -

gen_event:add_handler
gen_event:add_sup_handler  ----->  Module:init/1

gen_event:notify
gen_event:sync_notify      ----->  Module:handle_event/2

gen_event:send_request
gen_event:call             ----->  Module:handle_call/2

-                          ----->  Module:handle_info/2

gen_event:delete_handler   ----->  Module:terminate/2

gen_event:swap_handler
gen_event:swap_sup_handler ----->  Module1:terminate/2
                                   Module2:init/1

gen_event:which_handlers   ----->  -

gen_event:stop             ----->  Module:terminate/2

-                          ----->  Module:code_change/3
```

As each event handler is one callback module, an event manager
has many callback modules that are added and deleted dynamically.
`gen_event` is therefore more tolerant of callback module errors
than the other behaviors.  If a callback function for an installed
event handler fails with `Reason`, or returns a bad value `Term`,
the event manager does not fail.  It deletes the event handler
by calling callback function [`Module:terminate/2`](`c:terminate/2`),
giving as argument `{error, {'EXIT', Reason}}` or `{error, Term}`,
respectively.  No other event handler is affected.

A `gen_event` process handles system messages as described in `m:sys`.
The `sys` module can be used for debugging an event manager.

Notice that an event manager _does_ trap exit signals automatically.

The `gen_event` process can go into hibernation
(see `erlang:hibernate/3`) if a callback function in a handler module
specifies `hibernate` in its return value.  This can be useful
if the server is expected to be idle for a long time.
However, use this feature with care, as hibernation implies
at least two garbage collections (when hibernating
and shortly after waking up) and is not something you want to do
between each event handled by a busy event manager.

Notice that when multiple event handlers are invoked,
it is sufficient that one single event handler returns a `hibernate`
request for the whole event manager to go into hibernation.

Unless otherwise stated, all functions in this module fail
if the specified event manager does not exist
or if bad arguments are specified.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_
> ](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
> Blocking signaling can, for example, cause call timeouts in `gen_event`
> to be significantly delayed.

## See Also

`m:supervisor`, `m:sys`
""".

%%%
%%% A general event handler.
%%% Several handlers (functions) can be added.
%%% Each handler holds a state and will be called
%%% for every event received of the handler.
%%%

%%% Modified by Magnus.
%%%       Take care of fault situations and made notify asynchronous.
%%% Re-written by Joe with new functional interface !
%%% Modified by Martin - uses proc_lib, sys and gen!

%%%
%%% NOTE: If init_ack() return values are modified, see comment
%%%       above monitor_return() in gen.erl!
%%%

-export([start/0, start/1, start/2,
         start_link/0, start_link/1, start_link/2,
         start_monitor/0, start_monitor/1, start_monitor/2,
         stop/1, stop/3,
	 notify/2, sync_notify/2,
	 add_handler/3, add_sup_handler/3, delete_handler/3, swap_handler/3,
	 swap_sup_handler/3, which_handlers/1, call/3, call/4,
         send_request/3, send_request/5,
         wait_response/2, receive_response/2, check_response/2,
         wait_response/3, receive_response/3, check_response/3,
         reqids_new/0, reqids_size/1,
         reqids_add/3, reqids_to_list/1,
         wake_hib/5]).

-export([init_it/6,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 system_get_state/1,
	 system_replace_state/2,
	 format_status/2]).

-behaviour(sys).

%% logger callback
-export([format_log/1, format_log/2]).

-export_type([handler/0, handler_args/0, add_handler_ret/0,
              del_handler_ret/0, request_id/0, request_id_collection/0,
              format_status/0]).

-record(handler, {module             :: atom(),
		  id = false,
		  state,
		  supervised = false :: 'false' | pid()}).

-include("logger.hrl").

%%%=========================================================================
%%%  API
%%%=========================================================================

%% gen_event:start(Handler) -> {ok, Pid} | {error, What}
%%   gen_event:add_handler(Handler, Mod, Args) -> ok | Other
%%      gen_event:notify(Handler, Event) -> ok
%%      gen_event:call(Handler, Mod, Query) -> {ok, Val} | {error, Why}
%%      gen_event:call(Handler, Mod, Query, Timeout) -> {ok, Val} | {error, Why}
%%   gen_event:delete_handler(Handler, Mod, Args) -> Val
%%   gen_event:swap_handler(Handler, {OldMod, Args1}, {NewMod, Args2}) -> ok
%%   gen_event:which_handler(Handler) -> [Mod]
%% gen_event:stop(Handler) -> ok

-doc """
Initialize the event handler.

Whenever a new event handler is added to an event manager,
this function is called to initialize the event handler.

If the event handler is added because of a call to `add_handler/3` or
`add_sup_handler/3`, `InitArgs` is the `Args` argument of these functions.

If the event handler replaces another event handler because of
a call to `swap_handler/3` or `swap_sup_handler/3`, or because of
a `swap` return tuple from one of the other callback functions,
`InitArgs` is a tuple `{Args, Term}`, where `Args` is the argument
provided in the function call/return tuple and `Term` is the result
of terminating the old event handler, see `swap_handler/3`.

If successful, the function returns `{ok, State}` or
`{ok, State, hibernate}`, where `State` is the initial internal state
of the event handler.

If `{ok, State, hibernate}` is returned, the event manager
goes into hibernation (by calling `proc_lib:hibernate/3`),
waiting for the next event to occur.
""".
-callback init(InitArgs :: term()) ->
    {ok, State :: term()} |
    {ok, State :: term(), hibernate} |
    {error, Reason :: term()}.

-doc """
Handle an event.

Whenever an event manager receives an event sent using `notify/2` or
`sync_notify/2`, this function is called for each installed event handler
to handle the event.

`Event` is the `Event` argument of `notify/2` / `sync_notify/2`.

`State` is the internal state of the event handler.

- If `{ok, NewState}` or `{ok, NewState, hibernate}` is returned,
  the event handler remains in the event manager with the possibly
  updated internal state `NewState`.

- If `{ok, NewState, hibernate}` is returned, the event manager
  also goes into hibernation (by calling `proc_lib:hibernate/3`),
  waiting for the next event to occur.  It is sufficient
  that one of the event handlers return `{ok, NewState, hibernate}`
  for the whole event manager process to hibernate.

- If `{swap_handler, Args1, NewState, Handler2, Args2}` is returned,
  the event handler is replaced by `Handler2` by first calling
  [`Module:terminate(Args1, NewState)`](`c:terminate/2`) and then
  [`Module2:init({Args2, Term})`](`c:init/1`), where `Term`
  is the return value of [`Module:terminate/2`](`c:terminate/2`).
  For more information, see `swap_handler/3`.

- If `remove_handler` is returned, the event handler is deleted by calling
  [`Module:terminate(remove_handler, State)`](`c:terminate/2`).
""".
-callback handle_event(Event :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), hibernate} |
    {swap_handler, Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler.

-doc """
Handle a call.

Whenever an event manager receives a request sent using
[`call/3,4`](`call/3`), this function is called
for the specified event handler to handle the request.

`Request` is the `Request` argument of `call/3,4`.

`State` is the internal state of the event handler.

The return values are the same as for
[`Module:handle_event/2`](`c:handle_event/2`) except that
they also contain a term `Reply`, which is the reply to the client
as the return value of `call/3,4`.
""".
-callback handle_call(Request :: term(), State :: term()) ->
    {ok, Reply :: term(), NewState :: term()} |
    {ok, Reply :: term(), NewState :: term(), hibernate} |
    {swap_handler, Reply :: term(), Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    {remove_handler, Reply :: term()}.

-doc """
Handle an info message (regular process message).

This function is called for each installed event handler when
an event manager receives any other message than an event
or a synchronous request (or a system message).

`Info` is the received message.

In particular, this callback will be made when a process terminated
after calling `add_sup_handler/3`. Any event handler attached to
an event manager which in turn has a supervised handler
should expect callbacks of the shape
[`Module:handle_info({'EXIT', Pid, Reason}, State)`](`c:handle_info/2`).

For a description of `State` and possible return values,
see [`Module:handle_event/2`](`c:handle_event/2`).

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_event` module provides a default implementation
> of this function that logs about the unexpected `Info` message,
> drops it and returns `{ok, State}`.
""".
-callback handle_info(Info :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), hibernate} |
    {swap_handler, Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler.

-doc """
Handle event handler termination.

Whenever an event handler is deleted from an event manager,
this function is called. It is to be the opposite
of [`Module:init/1`](`c:init/1`) and do any necessary cleaning up.

If the event handler is deleted because of a call to `delete_handler/3`,
`swap_handler/3`, or `swap_sup_handler/3`, `Arg` is
the `Args` argument of this function call.

`Arg = {stop, Reason}` if the event handler has a supervised connection
to a process that has terminated with reason `Reason`.

`Arg = stop` if the event handler is deleted because
the event manager is terminating.

The event manager terminates if it is part of a supervision tree
and it is ordered by its supervisor to terminate.  Even if
it is _not_ part of a supervision tree, it terminates if it receives
an `'EXIT'` message from its parent.

`Arg = remove_handler` if the event handler is deleted
because another callback function has returned `remove_handler`
or `{remove_handler, Reply}`.

`Arg = {error, Term}` if the event handler is deleted because
a callback function returned an unexpected value `Term`,
or `Arg = {error, {'EXIT', Reason}}` if a callback function failed.

`State` is the internal state of the event handler.

The function can return any term.  If the event handler
is deleted because of a call to `gen_event:delete_handler/3`,
the return value of that function becomes the return value
of this function. If the event handler is to be replaced with
another event handler because of a swap, the return value
is passed to the `init` function of the new event handler.
Otherwise the return value is ignored.

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_event` module provides a default implementation
> without cleanup.
""".
-callback terminate(Args :: (term() | {stop, Reason :: term()} |
                             stop | remove_handler |
                             {error, {'EXIT', Reason :: term()}} |
                             {error, term()}),
                    State :: term()) ->
    term().

-doc """
Update the event handler state after code change.

This function is called for an installed event handler
that is to update its internal state during a release upgrade/downgrade,
that is, when the instruction `{update, Module, Change,...}`,
is specified in the [`appup`](`e:sasl:appup.md`) file.

For more information, see [OTP Design Principles](`e:system:index.html`).

For an upgrade, `OldVsn` is `Vsn`, and for a downgrade,
`OldVsn` is `{down, Vsn}`.  `Vsn` is defined by the `vsn` attribute(s)
of the old version of the callback module `Module`.  If no such attribute
is defined, the version is the checksum of the Beam file.

`State` is the internal state of the event handler.

`Extra` is passed "as is" from the `{advanced, Extra}` part
of the update instruction.

The function is to return the updated internal state.

> #### Note {: .info }
>
> If a release upgrade/downgrade with `Change={advanced, Extra}`
> specified in the [`.appup`](`e:sasl:appup.md`) file is made
> when `c:code_change/3` is not implemented the event handler will crash
> with an `undef` error reason.
""".
-callback code_change(OldVsn :: (term() | {down, term()}),
                      State :: term(), Extra :: term()) ->
    {ok, NewState :: term()}.

-doc """
Format/limit the status value.

This function is called by a `gen_event` process in in order to
format/limit the server state for debugging and logging purposes.

It is called in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked
  to get the `gen_event` status. `Opt` is set to the atom `normal`
  for this case.

- The event handler terminates abnormally and `gen_event` logs an error.
  `Opt` is set to the atom `terminate` for this case.

This function is useful for changing the form and appearance of the event
handler state for these cases. An event handler callback module
wishing to change the `sys:get_status/1,2` return value as well as
how its state appears in termination error logs, exports an instance of
[`format_status/2`](`c:format_status/2`) that returns a term
describing the current state of the event handler.

`PDict` is the current value of the process dictionary of `gen_event`.

`State` is the internal state of the event handler.

The function is to return `Status`, a term that change the details of
the current state of the event handler. Any term is allowed for `Status`.
The `gen_event` module uses `Status` as follows:

- When `sys:get_status/1,2` is called, `gen_event` ensures that
  its return value contains `Status` in place of the state term
  of the event handler.

- When an event handler terminates abnormally, `gen_event` logs `Status`
  in place of the state term of the event handler.

One use for this function is to return compact alternative
state representations to avoid that large state terms
are printed in log files.

> #### Note {: .info }
>
> This callback is optional, so event handler modules need not export it.
> If a handler does not export this function, the `gen_event` module
> uses the handler state directly for the purposes described below.
""".
-deprecated_callback({format_status, 2, "use format_status/1 instead"}).
-doc(#{since => <<"OTP R14B">>}).
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-doc """
A map that describes the `gen_event` process status.

The keys are:
- **`state`** - The internal state of the event handler.
- **`message`** - The message that caused the event handler to terminate.
- **`reason`** - The reason that caused the event handler to terminate.
- **`log`** - The [sys log](`sys:log/2`) of the server.

New associations may be added into the status map without prior notice.
""".
-type format_status() ::
        #{ state => term(),
           message => term(),
           reason => term(),
           log => [sys:system_event()] }.

-doc """
Format/limit the status value.

This function is called by a `gen_event` process in in order to
format/limit the server state for debugging and logging purposes.

It is called in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked
  to get the `gen_event` status.

- The event handler terminates abnormally and `gen_event` logs an error.

This callback is used to limit the status of the event handler returned by
[`sys:get_status/1,2`](`sys:get_status/1`) or sent to `m:logger`.

The callback gets a map `Status` describing the current status
and shall return a map `NewStatus` with the same keys,
but it may transform some values.

Two possible use cases for this callback is to remove
sensitive information from the state to prevent it from being printed
in log files, or to compact large irrelevant status items
that would only clutter the logs.

_Example_:

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
> This callback is optional, so event handler modules need not export it.
> If a handler does not export this function, the `gen_event` module
> uses the handler state directly for the purposes described below.
>
> If this callback is exported but fails, to hide possibly sensitive data,
> the default function will instead return the fact that
> [`format_status/1`](`c:format_status/1`) has crashed.
""".
-doc(#{since => <<"OTP 25.0">>}).
-callback format_status(Status) -> NewStatus when
      Status    :: format_status(),
      NewStatus :: format_status().

-optional_callbacks(
    [handle_info/2, terminate/2, code_change/3, format_status/1, format_status/2]).

%%---------------------------------------------------------------------------

-type handler()          :: atom() | {atom(), term()}.
-type handler_args()     :: term().
-type add_handler_ret()  :: ok | term() | {'EXIT',term()}.
-type del_handler_ret()  :: ok | term() | {'EXIT',term()}.

-doc """
Event manager name specification: `local`, `global`, or `via` registered.

- *`{local, Name}`* - the event manager is registered locally as
  `Name` using [`register/2`](`register/2`).
- *`{global, GlobalName}`* - The event manager is registered
  globally as `GlobalName` using `global:register_name/2`.
  If no name is provided, the event manager is not registered.
- *`{via, Module, ViaName}`*, the event manager registers with the
  registry represented by `Module`. The `Module` callback is to export
  the functions `register_name/2`, `unregister_name/1`, `whereis_name/1`,
  and `send/2`, which are to behave as the corresponding functions
  in `m:global`.  Thus, `{via, global, GlobalName}` is a valid reference.
""".
-type emgr_name() :: {'local', atom()} | {'global', term()}
                   | {'via', atom(), term()}.

-type debug_flag() :: 'trace' | 'log' | 'statistics' | 'debug'
                    | {'logfile', string()}.

-doc """
Options that can be used to configure an event handler
when it is started.
""".
-type options() :: [{'timeout', timeout()}
                   | {'debug', [debug_flag()]}
                   | {'spawn_opt', [proc_lib:start_spawn_option()]}
                   | {'hibernate_after', timeout()}].

-doc """
A reference used to locate an event manager.

The reference can be any of the following:

- The pid of the event manager
- `Name`, if the event manager is locally registered
- `{Name, Node}`, if the event manager is locally registered
  at another node
- `{global, GlobalName}`, if the event manager is globally registered
- `{via, Module, ViaName}`, if the event manager is registered through
  an alternative process registry
""".
-type emgr_ref()  :: atom() | {atom(), node()} |  {'global', term()}
                   | {'via', atom(), term()} | pid().
-type start_ret() :: {'ok', pid()} | {'error', term()}.
-type start_mon_ret() :: {'ok', {pid(),reference()}} | {'error', term()}.

-doc "An opaque request identifier. See `send_request/3` for details.".
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

- **`0..4294967295`** - Timeout relative to current time in milliseconds.

- **`infinity`** - Infinite timeout. That is, the operation
  will never time out.

- **`{abs, Timeout}`** - An absolute
  [Erlang monotonic time](`erlang:monotonic_time/1`) timeout
  in milliseconds.  That is, the operation will time out when
  [`erlang:monotonic_time(millisecond)`](`erlang:monotonic_time/1`)
  returns a value larger than or equal to `Timeout`.
 `Timeout` is not allowed to identify a time further into the future
  than `4294967295` milliseconds. Identifying the timeout using
  an absolute timeout value is especially handy when you have a
  deadline for responses corresponding to a complete collection
  of requests (`t:request_id_collection/0`) , since you do not have to
  recalculate the relative time until the deadline over and over again.
""".
-type response_timeout() ::
        timeout() | {abs, integer()}.

%%---------------------------------------------------------------------------

-define(NO_CALLBACK, 'no callback module').

%% -----------------------------------------------------------------
%% Starts a generic event handler.
%% start()
%% start(MgrName | Options)
%% start(MgrName, Options)
%% start_link()
%% start_link(MgrName | Options)
%% start_link(MgrName, Options)
%%    MgrName ::= {local, atom()} | {global, term()} | {via, atom(), term()}
%%    Options ::= [{timeout, Timeout} | {debug, [Flag]} | {spawn_opt,SOpts}]
%%       Flag ::= trace | log | {logfile, File} | statistics | debug
%%          (debug == log && statistics)
%% Returns: {ok, Pid} |
%%          {error, {already_started, Pid}} |
%%          {error, Reason}
%% -----------------------------------------------------------------

-doc(#{equiv => start([])}).
-spec start() -> start_ret().
start() ->
    gen:start(?MODULE, nolink, ?NO_CALLBACK, [], []).

-doc """
Create a stand-alone event manager process, possibly nameless.

Equivalent to [`start(EventMgrName, Options)`](`start/2`).

With argument `EventMgrName`, `Options` is `[]`.

With argument `Options` a nameless event manager is created.

For a description of the arguments and return values, see `start_link/2`.
""".
-spec start(EventMgrName :: emgr_name()) -> start_ret();
           (Options :: options()) -> start_ret().
start(Name) when is_tuple(Name) ->
    gen:start(?MODULE, nolink, Name, ?NO_CALLBACK, [], []);
start(Options) when is_list(Options) ->
    gen:start(?MODULE, nolink, ?NO_CALLBACK, [], Options);
start(Arg) ->
    error(badarg, [Arg]).

-doc """
Create a stand-alone event manager process.

The created event manager process is not part of a supervision tree
and thus has no supervisor.

For a description of the arguments and return values, see `start_link/2`.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec start(EventMgrName :: emgr_name(), Options :: options()) -> start_ret().
start(Name, Options) when is_tuple(Name), is_list(Options) ->
    gen:start(?MODULE, nolink, Name, ?NO_CALLBACK, [], Options);
start(Name, Options) ->
    error(badarg, [Name, Options]).

-doc(#{equiv => start_link([])}).
-spec start_link() -> start_ret().
start_link() ->
    gen:start(?MODULE, link, ?NO_CALLBACK, [], []).

-doc """
Create an event manager process as part of a supervision tree,
possibly nameless.

Equivalent to [`start_link(EventMgrName, Options)`](`start_link/2`).

With argument `EventMgrName`, `Options` is `[]`.

With argument `Options` a nameless event manager is created.

For a description of the arguments and return values, see `start_link/2`.
""".
-spec start_link(EventMgrName :: emgr_name()) -> start_ret();
                (Options :: options()) -> start_ret().
start_link(Name) when is_tuple(Name) ->
    gen:start(?MODULE, link, Name, ?NO_CALLBACK, [], []);
start_link(Options) when is_list(Options) ->
    gen:start(?MODULE, link, ?NO_CALLBACK, [], Options);
start_link(Arg) ->
    error(badarg, [Arg]).

-doc """
Create an event manager process as part of a supervision tree.

The function is to be called, directly or indirectly, by the supervisor.
For example, it ensures that the event manager is linked
to the caller (supervisor).

- If option `{hibernate_after, HibernateAfterTimeout}` is present, the
  `gen_event` process awaits any message for `HibernateAfterTimeout`
  milliseconds and if no message is received, the process
  goes into hibernation automatically (by calling `proc_lib:hibernate/3`).

If the event manager is successfully created,
the function returns `{ok, Pid}` where `Pid` is the `t:pid/0`
of the event manager.

If a process with the specified `EventMgrName` exists already,
the function returns `{error,{already_started,OtherPid}}`,
where `OtherPid` is the pid of that process, and the event manager process
exits with reason `normal`.

If the event manager fails to start within the specified start timeout
`{timeout, Time}`, which is very unlikely since the start
does not interact with other processes, the function returns
`{error, timeout}` and the failed event manager is killed with
[`exit(_, kill)`](`erlang:exit/2`).

If `start_link/1,2` returns `{error, _}`, the started event manager process
has terminated.  If an `'EXIT'` message was delivered
to the calling process (due to the process link), that message
has been consumed.

> #### Warning {: .warning }
>
> Before OTP 26.0, if the started event manager failed to register
> its name, this founction could return
> `{error, {already_started, OtherPid}}` _before_
> the started event manager process had terminated,
> so starting again might fail because the registered name
> was not yet unregistered, and an `'EXIT'` message could arrive later
> to the process calling this function.
>
> But if the start timed out, this function killed
> the started event manager process and returned `{error, timeout}`,
> and then the process link `{'EXIT', Pid, killed}` message _was_ consumed.
>
> The start was made synchronous in OTP 26.0 and a guarantee
> was implemented that no process link `'EXIT'` message
> from a failed start will linger in the caller's inbox.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec start_link(EventMgrName :: emgr_name(), Options :: options()) -> start_ret().
start_link(Name, Options) when is_tuple(Name), is_list(Options) ->
    gen:start(?MODULE, link, Name, ?NO_CALLBACK, [], Options);
start_link(Name, Options) ->
    error(badarg, [Name, Options]).

-doc(#{equiv => start_monitor([])}).
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor() -> start_mon_ret().
start_monitor() ->
    gen:start(?MODULE, monitor, ?NO_CALLBACK, [], []).

-doc """
Creates a stand-alone event manager process,
monitored, possibly nameless.

Equivalent to [`start_monitor(EventMgrName, Options)`](`start_monitor/2`).

With argument `EventMgrName`, `Options` is `[]`.

With argument `Options` a nameless event manager is created.

For a description of the arguments and return values,
see `start_monitor/2` and `start_link/1`.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(EventMgrNameOrOptions :: emgr_name() | options()) -> start_mon_ret().
start_monitor(Name) when is_tuple(Name) ->
    gen:start(?MODULE, monitor, Name, ?NO_CALLBACK, [], []);
start_monitor(Options) when is_list(Options) ->
    gen:start(?MODULE, monitor, ?NO_CALLBACK, [], Options);
start_monitor(Arg) ->
    error(badarg, [Arg]).

-doc """
Creates a stand-alone event manager process, monitored.

The created event manager process is not part of a supervision tree
and thus has no supervisor.  A monitor is atomically set up
to the newly created process.

For a description of the arguments and return values, see
[`start_link/2`](`start_link/2`). Note that the return value
for a successful start differs from `start_link/2`.
`start_monitor/0,1,2` will return `{ok, {Pid, Mon}}`
where `Pid` is the process identifier of the process,
and `Mon` is a reference to the monitor set up to monitor the process.
If the start is not successful, the caller will be blocked
until the `DOWN` message has been received and removed
from the message queue.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(EventMgtName :: emgr_name(), Options :: options()) -> start_mon_ret().
start_monitor(Name, Options) when is_tuple(Name), is_list(Options) ->
    gen:start(?MODULE, monitor, Name, ?NO_CALLBACK, [], Options);
start_monitor(Name, Options) ->
    error(badarg, [Name, Options]).

%% -spec init_it(pid(), 'self' | pid(), emgr_name(), module(), [term()], [_]) ->
-doc false.
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, _, _, Options) ->
    process_flag(trap_exit, true),
    Name = gen:name(Name0),
    Debug = gen:debug_options(Name, Options),
	HibernateAfterTimeout = gen:hibernate_after(Options),
    proc_lib:init_ack(Starter, {ok, self()}),
    loop(Parent, Name, [], HibernateAfterTimeout, Debug, false).

-doc """
Add a new event handler to an event manager.

The new event handler is added to event manager `EventMgrRef`.
The event manager calls [`Module:init/1`](`c:init/1`)
to initiate the event handler and its internal state.

`Handler` is the name of the callback module `Module`
or a tuple `{Module, Id}`, where `Id` is any term.
The `{Module, Id}` representation makes it possible to
identify a specific event handler, when many event handlers
use the same callback module.

`Args` is any term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

If [`Module:init/1`](`c:init/1`) returns a correct value
indicating successful completion, the event manager
adds the event handler and this function returns `ok`.
If [`Module:init/1`](`c:init/1`) fails with `Reason` or returns
`{error,Reason}`, the event handler is ignored and this function
returns `{'EXIT',Reason}` or `{error,Reason}`, respectively.
""".
-spec add_handler(EventMgrRef :: emgr_ref(), Handler :: handler(), Args :: term()) -> term().
add_handler(M, Handler, Args) -> rpc(M, {add_handler, Handler, Args}).

-doc """
Add a new event handler to an event manager, supervised.

The new event handler is added as for `add_handler/3`,
but the event manager also supervises the connection
by linking the event handler and the calling process.

- If the calling process later terminates with `Reason`,
  the event manager deletes any supervised event handlers by calling
  [`Module:terminate/2`](`c:terminate/2`), then calls
  [`Module:handle_info/2`](`c:handle_info/2`) for each remaining handler.

- If the event handler is deleted later, the event manager
  sends a message `{gen_event_EXIT,Handler,Reason}`
  to the calling process. `Reason` is one of the following:

  + `normal`, if the event handler has been removed because of
    a call to [`delete_handler/3`](`delete_handler/3`),
    or `remove_handler` has been returned by a callback function
    (see below).
  + `shutdown`, if the event handler has been removed
    because the event manager is terminating.
  + `{swapped, NewHandler, Pid}`, if the process `Pid` has replaced
    the event handler with another event handler `NewHandler`,
    through a call to `swap_handler/3` or `swap_sup_handler/3`.
  + Other `t:term/0`, if the event handler is removed
    because of an error.  Which term depends on the error.

For a description of the arguments and return values, see `add_handler/3`.
""".
-spec add_sup_handler(EventMgrRef :: emgr_ref(), Handler :: handler(), Args :: term()) -> term().
add_sup_handler(M, Handler, Args)  ->
    rpc(M, {add_sup_handler, Handler, Args, self()}).

-doc """
Send an asynchronous event notification to an event manager.

The event is sent to `EventMgrRef`, that calls
[`Module:handle_event/2`](`c:handle_event/2`) for each installed
event handler to handle the event.

`Event` is any term that is passed as one of the arguments to
[`Module:handle_event/2`](`c:handle_event/2`).

`notify/1` does not fail even if the specified event manager
does not exist, unless it is specified as `Name`.
""".

-spec notify(EventMgrRef :: emgr_ref(), Event :: term()) -> 'ok'.
notify(M, Event) -> send(M, {notify, Event}).

-doc """
Send a synchronous event notification to an event manager.

The event is sent to `EventMgrRef` that callsr calls
[`Module:handle_event/2`](`c:handle_event/2`) for each installed
event handler to handle the event. This function will return `ok`
after the event has been handled by all event handlers.

`Event` is any term that is passed as one of the arguments to
[`Module:handle_event/2`](`c:handle_event/2`).
""".
-spec sync_notify(EventMgrRef :: emgr_ref(), Event :: term()) -> 'ok'.
sync_notify(M, Event) -> rpc(M, {sync_notify, Event}).

-doc(#{equiv => call(EventMgrRef, Handler, Request, 5000)}).
-spec call(EventMgrRef :: emgr_ref(), Handler :: handler(), Request :: term()) -> term().
call(M, Handler, Query) -> call1(M, Handler, Query).

-doc """
Make a synchronous call to an event handler.

The call is sent to `Handler`, installed in event manager `EventMgrRef`,
by sending a request and waiting until a reply arrives,
or a time-out occurs.  The event manager calls
[`Module:handle_call/2`](`c:handle_call/2`) to handle the request.

`Request` is any term that is passed as one of the arguments to
[`Module:handle_call/2`](`c:handle_call/2`).

`Timeout` is an integer greater than zero that specifies
how many milliseconds to wait for a reply, or the atom `infinity`
to wait indefinitely.  Defaults to 5000.  If no reply is received
within the specified time, the function call fails.

The return value `Reply` is defined in the return value of
[`Module:handle_call/2`](`c:handle_call/2`).  If the specified
event handler is not installed, the function returns
`{error, bad_module}`.  If the callback function fails with `Reason`,
or returns an unexpected value `Term`, this function returns
`{error, {'EXIT', Reason}}` or `{error, Term}`, respectively.

When this call fails it [exits](`erlang:exit/1`) the calling process.
The exit term is on the form `{Reason, Location}` where
`Location = {gen_event, call, ArgList}`. See `gen_server:call/3`
that has a description of relevant values for the `Reason`
in the exit term.
""".
-spec call(EventMgrRef :: emgr_ref(), Handler :: handler(), Request :: term(), Timeout :: timeout()) -> term().
call(M, Handler, Query, Timeout) -> call1(M, Handler, Query, Timeout).

-doc """
Send an asynchronous `call` request to an event handler.

This function sends the call request `Request` to the event handler
`Handler` installed in the event manager identified by `EventMgrRef`,
and returns a request identifier `ReqId`.  The return value `ReqId`
shall later be used with `receive_response/2`, `wait_response/2`,
or `check_response/2` to fetch the actual result of the request.

Besides passing the request identifier directly to these functions,
it can also be stored in a request identifier collection
using `reqids_add/3`.  Such a collection of request identifiers
can later be used in order to get one response corresponding to
a request in the collection by passing the collection as argument to
`receive_response/3`, `wait_response/3`, or `check_response/3`.
If you are about to store the request identifier in a collection,
you may want to consider using `send_request/5` instead.

The calls
`gen_event:receive_response(gen_event:send_request(EventMgrRef,
Handler, Request), Timeout)`
can be seen as equivalent to
[`gen_event:call(EventMgrRef, Handler, Request, Timeout)`](`call/3`),
ignoring the error handling.

The event manager calls [`Module:handle_call/2`](`c:handle_call/2`)
to handle the request.

`Request` may be any term and is passed as one of the arguments to
[`Module:handle_call/2`](`c:handle_call/2`).
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec send_request(EventMgrRef::emgr_ref(), Handler::handler(), Request::term()) ->
          ReqId::request_id().
send_request(M, Handler, Request) ->
    try
        gen:send_request(M, self(), {call, Handler, Request})
    catch
        error:badarg ->
            error(badarg, [M, Handler, Request])
    end.

-doc """
Send an asynchronous `call` request to an event handler,
storing it in a request identifier collection.

This function sends the call request `Request` to the event handler
`Handler` installed in the event manager identified by `EventMgrRef`.
The `Label` will be associated with the request identifier
of the operation and added to the returned
request identifier collection `NewReqIdCollection`.

The collection can later be used in order to get one response
corresponding to a request in the collection by passing the collection
as argument to `receive_response/3`, `wait_response/3`,
or `check_response/3`.

The same as calling
[`gen_event:reqids_add`](`reqids_add/3`)`(`[`gen_event:send_request`](`send_request/3`)`(EventMgrRef, Handler, Request), Label, ReqIdCollection)`,
but slightly more efficient.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec send_request(EventMgrRef::emgr_ref(),
                   Handler::handler(),
                   Request::term(),
                   Label::term(),
                   ReqIdCollection::request_id_collection()) ->
          NewReqIdCollection::request_id_collection().
send_request(M, Handler, Request, Label, ReqIdCol) ->
    try
        gen:send_request(M, self(), {call, Handler, Request}, Label, ReqIdCol)
    catch
        error:badarg ->
            error(badarg, [M, Handler, Request, Label, ReqIdCol])
    end.

-doc """
Wait for a request resonse.

Wait for the response to the request identifier `ReqId`. The request
must have been made by `send_request/3`, from the same process
that called `send_request/3`.

`WaitTime` specifies how long to wait for a response.
If no response is received within the specified time,
the function returns `timeout` and no cleanup is done,
Thus the function can be invoked repeatedly until a reply is returned.

The return value `Reply` is defined in the return value of
[`Module:handle_call/2`](`c:handle_call/2`).

If the specified event handler is not installed, the function returns
`{error, bad_module}`.  If the callback function fails with `Reason`,
or returns an unexpected value `Term`, this function returns
`{error,{'EXIT',Reason}}` or `{error,Term}`, respectively.
If the event manager dies before or during the request
this function returns `{error, {Reason, EventMgrRef}}`.

The difference between `receive_response/2` and
`wait_response/2` is that `receive_response/2` abandons the request
at timeout so that a potential future response is ignored,
while [`wait_response/2`](`wait_response/2`) does not.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec wait_response(ReqId, WaitTime) -> Result when
      ReqId :: request_id(),
      WaitTime :: response_timeout(),
      Response :: {reply, Reply::term()}
                | {error, {Reason::term(), emgr_ref()}},
      Result :: Response | 'timeout'.

wait_response(ReqId, WaitTime) ->
    try gen:wait_response(ReqId, WaitTime) of
        {reply, {error, _} = Err} -> Err;
        Return -> Return
    catch
        error:badarg ->
            error(badarg, [ReqId, WaitTime])
    end.

-doc """
Wait for any request response in a collection.

Wait for a response in a `ReqIdCollection`.  All request identifiers
of `ReqIdCollection` must correspond to requests that have been made
using `send_request/3` or `send_request/5`, and all requests
must have been made by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [adding the request id](`reqids_add/3`) to a collection,
or when sending the request using `send_request/5`.

Compared to `wait_response/2`, the returned result or exception
associated with a specific request identifier will be wrapped
in a 3-tuple `{Response, Label, NewReqIdCollection}`.
`Response` is the value that would have been produced
by `wait_response/2`, `Label` is the value associated with
the specific [request identifier](`t:request_id/0`)
and `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned.

If no response is received before the `WaitTime` has expired,
`timeout` is returned.  It is valid to continue waiting
for a response as many times as needed up until a response
has been received and completed by `check_response()`,
`receive_response()`, or `wait_response()`.

The difference between `receive_response/3` and `wait_response/3`
is that `receive_response/3` abandons requests at time-out
so that potential future responses are ignored, while
[`wait_response/3`](`wait_response/3`) does not.

If `Delete` is `true`, the association with `Label`
has been deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`. If `Delete` is`false`, `NewReqIdCollection`
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
                  {error, {Reason::term(), emgr_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

wait_response(ReqIdCol, WaitTime, Delete) ->
    try gen:wait_response(ReqIdCol, WaitTime, Delete) of
        {{reply, {error, _} = Err}, Label, NewReqIdCol} ->
            {Err, Label, NewReqIdCol};
        Return ->
            Return
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, WaitTime, Delete])
    end.

-doc """
Receive a request response.

Receive a response corresponding to the request identifier `ReqId`.
The request must have been made by `send_request/3`,
and it must have been made from the same process calling this function.

`Timeout` specifies how long to wait for a response.
If no response is received within the specified time,
this function returns `timeout`. Assuming that the
server executes on a node supporting aliases (introduced in OTP 24)
the request will also be abandoned.  That is,
no response will be received after a timeout.
Otherwise, a stray response might be received at a later time.

The return value `Reply` is defined in the return value of
[`Module:handle_call/2`](`c:handle_call/2`).

If the specified event handler is not installed, this function returns
`{error, bad_module}`. If the callback function fails
with `Reason` or returns an unexpected value `Term`,
this function returns `{error, {'EXIT', Reason}}` or`{error,Term}`,
respectively.  If the event manager dies before or during the
request this function returns `{error, {Reason, EventMgrRef}}`.

The difference between `wait_response/2` and `receive_response/2`
is that `receive_response/2` abandons the request at time-out
so that a potential future response is ignored,
while [`wait_response/2`](`wait_response/2`) does not.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec receive_response(ReqId, Timeout) -> Result when
      ReqId :: request_id(),
      Timeout :: response_timeout(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: Response | 'timeout'.

receive_response(ReqId, Timeout) ->
    try gen:receive_response(ReqId, Timeout) of
        {reply, {error, _} = Err} -> Err;
        Return -> Return
    catch
        error:badarg ->
            error(badarg, [ReqId, Timeout])
    end.

-doc """
Receive a request response in a collection.

Receive a response in `ReqIdCollection`. All request identifiers
of `ReqIdCollection` must correspond to requests that have been
made using `send_request/3` or `send_request/5`, and all requests
must have been made by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [adding the request id](`reqids_add/3`) to a collection,
or when sending the request using `send_request/5`.

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
while `wait_response/3` does not.

If `Delete` is `true`, the association with `Label`
is deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`.  If `Delete` is `false`, `NewReqIdCollection`
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
`receive_response/3`, it will always block until `Timeout` expires
and then return `timeout`.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec receive_response(ReqIdCollection, Timeout, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      Timeout :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

receive_response(ReqIdCol, Timeout, Delete) ->
    try gen:receive_response(ReqIdCol, Timeout, Delete) of
        {{reply, {error, _} = Err}, Label, NewReqIdCol} ->
            {Err, Label, NewReqIdCol};
        Return ->
            Return
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, Timeout, Delete])
    end.

-doc """
Check if a received message is a request response.

Check if `Msg` is a response corresponding to
the request identifier `ReqId`.  The request must have been made
by `send_request/3`, and by the same process calling this function.

If `Msg` is a response corresponding to `ReqId` the response is returned
in `Reply`.  Otherwise this function returns `no_reply`
and no cleanup is done.  Thus this function must be invoked repeatedly
until a response is returned.

If the specified event handler is not installed, the function returns
`{error, bad_module}`.  If the callback function fails with `Reason`
or returns an unexpected value `Term`, this function returns
`{error, {'EXIT', Reason}}` or `{error, Term}`, respectively.
If the event manager has died before this function is called,
that is; `Msg` reports the server's death, this function returns
`{error,{Reason, EventMgrRef}}` where `Reason` is the exit reason.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec check_response(Msg, ReqId) -> Result when
      Msg :: term(),
      ReqId :: request_id(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: Response | 'no_reply'.

check_response(Msg, ReqId) ->
    try gen:check_response(Msg, ReqId) of
        {reply, {error, _} = Err} -> Err;
        Return -> Return
    catch
        error:badarg ->
            error(badarg, [Msg, ReqId])
    end.

-doc """
Check if a received message is a request response in a collection.

Check if `Msg` is a response corresponding to a request identifier
stored in `ReqIdCollection`.  All request identifiers of `ReqIdCollection`
must correspond to requests that have been made using `send_request/3`
or `send_request/5`, and all requests must have been made
by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [storing the request id](`reqids_add/3`) in a collection,
or when sending the request using `send_request/5`.

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

If `Delete` is `true`, the association with `Label` has been deleted
from `ReqIdCollection` in the resulting `NewReqIdCollection`.
If `Delete` is `false`, `NewReqIdCollection` will equal `ReqIdCollection`.
Note that deleting an association is not for free and that
a collection containing already handled requests
can still be used by subsequent calls to `check_response/3`),
`receive_response/3`, and `wait_response/3`.

However, without deleting handled associations, the above calls
will not be able to detect when there are no more outstanding requests
to handle, so you will have to keep track of this some other way
than relying on a `no_request` return.  Note that if you pass
a collection only containing associations of already handled
or abandoned requests to `check_response/3`,
it will always return `no_reply`.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec check_response(Msg, ReqIdCollection, Delete) -> Result when
      Msg :: term(),
      ReqIdCollection :: request_id_collection(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'no_reply'.

check_response(Msg, ReqIdCol, Delete) ->
    try gen:check_response(Msg, ReqIdCol, Delete) of
        {{reply, {error, _} = Err}, Label, NewReqIdCol} ->
            {Err, Label, NewReqIdCol};
        Return ->
            Return
    catch
        error:badarg ->
            error(badarg, [Msg, ReqIdCol, Delete])
    end.

-doc """
Create an empty request identifier collection.

Returns a new empty request identifier collection.
A request identifier collection can be utilized to handle
multiple outstanding requests.

Request identifiers of requests made by `send_request/3`
can be saved in a request identifier collection using `reqids_add/3`.
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

Returns a list of `{ReqId, Label}` tuples which corresponds to
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

-doc """
Deletes an event handler from an event manager.

This function deletes event handler `Handler` from event manager
`EventMgrRef`. The event manager calls
[`Module:terminate/2`](`c:terminate/2`) to terminate the event handler.

`Args` is any term that is passed as one of the arguments to
[`Module:terminate/2`](`c:terminate/2`).

The return value is the return value of
[`Module:terminate/2`](`c:terminate/2`).  If the specified
event handler is not installed, the function returns
`{error, module_not_found}`. If the callback function fails
with `Reason`, the function returns `{'EXIT', Reason}`.
""".
-spec delete_handler(EventMgrRef :: emgr_ref(), Handler :: handler(), Args :: term()) -> term().
delete_handler(M, Handler, Args) -> rpc(M, {delete_handler, Handler, Args}).

-doc """
Replace an event handler.

This function replaces an event handler in event manager `EventMgrRef`.

For a description of `OldHandler` and `NewHandler`, see `add_handler/3`.

First the old event handler `OldHandler` is deleted. The event manager
calls `OldModule:terminate(Args1, ...)`, where `OldModule`
is the callback module of `OldHandler`, and collects the return value.

Then the new event handler `NewHandler` is added and initiated
by calling [`NewModule:init({Args2,Term})`](`c:init/1`), where `NewModule`
is the callback module of `NewHandler`, and `Term` is the return value
of [`OldModule:terminate/2`](`c:terminate/2`).  This makes it possible
to transfer information from `OldHandler` to `NewHandler`.

The new handler is added even if the the specified old event handler
is not installed, in which case `Term = error`, or if
[`OldModule:terminate/2`](`c:terminate/2`) fails with `Reason`,
in which case `Term = {'EXIT', Reason}`.  The old handler
is deleted even if [`NewModule:init/1`](`c:init/1`) fails.

If there was a supervised connection
between `OldHandler` and a process `Pid`,
there is a supervised connection between `NewHandler` and `Pid` instead.

If [`NewModule:init/1`](`c:init/1`) returns a correct value,
this function returns `ok`. If [`NewModule:init/1`](`c:init/1`) fails
with `Reason` or returns an unexpected value `Term`,
this function returns `{error, {'EXIT', Reason}}` or
`{error, Term}`, respectively.
""".
-spec swap_handler(EventMgrRef :: emgr_ref(),
                   OldHandler :: {handler(), term()},
                   NewHandler :: {handler(), term()}) ->
	    'ok' | {'error', term()}.
swap_handler(M, {H1, A1}, {H2, A2}) -> rpc(M, {swap_handler, H1, A1, H2, A2}).

-doc """
Replace an event handler, and supervise it.

Replaces an event handler in event manager `EventMgrRef`
in the same way as [`swap_handler/3`](`swap_handler/3`),
but also supervises the connection between `NewHandler`
and the calling process.

For a description of the arguments and return values, see `swap_handler/3`.
""".
-spec swap_sup_handler(EventMgrRef :: emgr_ref(),
                       OldHandler :: {handler(), term()},
                       NewHandler :: {handler(), term()}) ->
	    'ok' | {'error', term()}.
swap_sup_handler(M, {H1, A1}, {H2, A2}) ->
    rpc(M, {swap_sup_handler, H1, A1, H2, A2, self()}).

-doc """
Return all event handlers in an event manager.

This function returns a list of all event handlers
installed in event manager `EventMgrRef`.

For a description of `Handler`, see `add_handler/3`.
""".
-spec which_handlers(EventMgrRef :: emgr_ref()) -> [handler()].
which_handlers(M) -> rpc(M, which_handlers).

-doc(#{equiv => stop(EventMgrRef, normal, infinity)}).
-spec stop(EventMgrRef :: emgr_ref()) -> 'ok'.
stop(M) ->
    gen:stop(M).

-doc """
Stop an event manager.

Orders event manager `EventMgrRef` to exit with the specifies `Reason`,
and waits for it to terminate.  Before terminating, `gen_event` calls
[`Module:terminate(stop,...)`](`c:terminate/2`)
for each installed event handler.

The function returns `ok` if the event manager terminates
with the expected reason.  Any other reason than `normal`,
`shutdown`, or `{shutdown, Term}` causes an error report
to be issued using `m:logger`.

`Timeout` is an integer greater than zero that specifies
how many milliseconds to wait for the event manager to terminate,
or the atom `infinity` to wait indefinitely.  If the event manager
has not terminated within the specified time, the call exits
the calling process with reason `timeout`.

If the process does not exist,
the call exits the calling process with reason `noproc`,
and with reason `{nodedown, Node}` if the connection fails
to the remote `Node` where the server runs.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec stop(EventMgrRef :: emgr_ref(), Reason :: term(), Timeout :: timeout()) -> 'ok'.
stop(M, Reason, Timeout) ->
    gen:stop(M, Reason, Timeout).

rpc(M, Cmd) ->
    {ok, Reply} = gen:call(M, self(), Cmd, infinity),
    Reply.

call1(M, Handler, Query) ->
    Cmd = {call, Handler, Query},
    try gen:call(M, self(), Cmd) of
	{ok, Res} ->
	    Res
    catch
	exit:Reason ->
	    exit({Reason, {?MODULE, call, [M, Handler, Query]}})
    end.

call1(M, Handler, Query, Timeout) ->
    Cmd = {call, Handler, Query},
    try gen:call(M, self(), Cmd, Timeout) of
	{ok, Res} ->
	    Res
    catch
	exit:Reason ->
	    exit({Reason, {?MODULE, call, [M, Handler, Query, Timeout]}})
    end.

send({global, Name}, Cmd) ->
    catch global:send(Name, Cmd),
    ok;
send({via, Mod, Name}, Cmd) ->
    catch Mod:send(Name, Cmd),
    ok;
send(M, Cmd) ->
    M ! Cmd,
    ok.

loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true) ->
     proc_lib:hibernate(?MODULE, wake_hib, [Parent, ServerName, MSL, HibernateAfterTimeout, Debug]);
loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, _) ->
    fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false).

-doc false.
wake_hib(Parent, ServerName, MSL, HibernateAfterTimeout, Debug) ->
    fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true).

fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib) ->
    receive
	Msg ->
	    decode_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib)
    after HibernateAfterTimeout ->
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true)
    end.

decode_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib) ->
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [ServerName, MSL, HibernateAfterTimeout, Hib],Hib);
	{'EXIT', Parent, Reason} ->
	    terminate_server(Reason, Parent, MSL, ServerName);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, []);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      ServerName, {in, Msg}),
	    handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug1)
    end.

handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug) ->
    case Msg of
	{notify, Event} ->
	    {Hib,MSL1} = server_notify(Event, handle_event, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {sync_notify, Event}} ->
	    {Hib, MSL1} = server_notify(Event, handle_event, MSL, ServerName),
	    reply(Tag, ok),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{'EXIT', From, Reason} ->
	    MSL1 = handle_exit(From, Reason, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, false);
	{_From, Tag, {call, Handler, Query}} ->
	    {Hib, Reply, MSL1} = server_call(Handler, Query, MSL, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {add_handler, Handler, Args}} ->
	    {Hib, Reply, MSL1} = server_add_handler(Handler, Args, MSL),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {add_sup_handler, Handler, Args, SupP}} ->
	    {Hib, Reply, MSL1} = server_add_sup_handler(Handler, Args, MSL, SupP),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {delete_handler, Handler, Args}} ->
	    {Reply, MSL1} = server_delete_handler(Handler, Args, MSL,
						  ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, false);
	{_From, Tag, {swap_handler, Handler1, Args1, Handler2, Args2}} ->
	    {Hib, Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
						     Args2, MSL, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {swap_sup_handler, Handler1, Args1, Handler2, Args2,
		     Sup}} ->
	    {Hib, Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
						Args2, MSL, Sup, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, stop} ->
	    catch terminate_server(normal, Parent, MSL, ServerName),
	    reply(Tag, ok);
	{_From, Tag, which_handlers} ->
	    reply(Tag, the_handlers(MSL)),
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false);
	{_From, Tag, get_modules} ->
	    reply(Tag, get_modules(MSL)),
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false);
	Other  ->
	    {Hib, MSL1} = server_notify(Other, handle_info, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib)
    end.

terminate_server(Reason, Parent, MSL, ServerName) ->
    stop_handlers(MSL, ServerName),
    do_unlink(Parent, MSL),
    exit(Reason).

reply(From, Reply) ->
    gen:reply(From, Reply).

%% unlink the supervisor process of all supervised handlers.
%% We do not want a handler supervisor to EXIT due to the
%% termination of the event manager (server).
%% Do not unlink Parent !
do_unlink(Parent, MSL) ->
    lists:foreach(fun(Handler) when Handler#handler.supervised =:= Parent ->
			  true;
		     (Handler) when is_pid(Handler#handler.supervised) ->
			  unlink(Handler#handler.supervised),
			  true;
		     (_) ->
			  true
		  end,
		  MSL).

%% First terminate the supervised (if exists) handlers and
%% then inform other handlers.
%% We do not know if any handler really is interested but it
%% may be so !
handle_exit(From, Reason, MSL, SName) ->
    MSL1 = terminate_supervised(From, Reason, MSL, SName),
    {_,MSL2}=server_notify({'EXIT', From, Reason}, handle_info, MSL1, SName),
    MSL2.

terminate_supervised(Pid, Reason, MSL, SName) ->
    F = fun(Ha) when Ha#handler.supervised =:= Pid ->
		do_terminate(Ha#handler.module,
			     Ha,
			     {stop,Reason},
			     Ha#handler.state,
			     {parent_terminated, {Pid,Reason}},
			     SName,
			     shutdown),
		false;
	   (_) ->
		true
	end,
    lists:filter(F, MSL).

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
-doc false.
system_continue(Parent, Debug, [ServerName, MSL, HibernateAfterTimeout, Hib]) ->
    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib).

-doc false.
-spec system_terminate(_, _, _, [_]) -> no_return().
system_terminate(Reason, Parent, _Debug, [ServerName, MSL, _HibernateAfterTimeout, _Hib]) ->
    terminate_server(Reason, Parent, MSL, ServerName).

%%-----------------------------------------------------------------
%% Module here is sent in the system msg change_code.  It specifies
%% which module should be changed.
%%-----------------------------------------------------------------
-doc false.
system_code_change([ServerName, MSL, HibernateAfterTimeout, Hib], Module, OldVsn, Extra) ->
    MSL1 = lists:zf(fun(H) when H#handler.module =:= Module ->
			    {ok, NewState} =
				Module:code_change(OldVsn,
						   H#handler.state, Extra),
			    {true, H#handler{state = NewState}};
		       (_) -> true
		    end,
		    MSL),
    {ok, [ServerName, MSL1, HibernateAfterTimeout, Hib]}.

-doc false.
system_get_state([_ServerName, MSL, _HibernateAfterTimeout, _Hib]) ->
    {ok, [{Mod,Id,State} || #handler{module=Mod, id=Id, state=State} <- MSL]}.

-doc false.
system_replace_state(StateFun, [ServerName, MSL, HibernateAfterTimeout, Hib]) ->
    {NMSL, NStates} =
		lists:unzip([begin
				 Cur = {Mod,Id,State},
				 try
				     NState = {Mod,Id,NS} = StateFun(Cur),
				     {HS#handler{state=NS}, NState}
				 catch
				     _:_ ->
					 {HS, Cur}
				 end
			     end || #handler{module=Mod, id=Id, state=State}=HS <- MSL]),
    {ok, NStates, [ServerName, NMSL, HibernateAfterTimeout, Hib]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{notify, Event} ->
	    io:format(Dev, "*DBG* ~tp got event ~tp~n", [Name, Event]);
	{_,_,{call, Handler, Query}} ->
	    io:format(Dev, "*DBG* ~tp(~tp) got call ~tp~n",
		      [Name, Handler, Query]);
	_ ->
	    io:format(Dev, "*DBG* ~tp got ~tp~n", [Name, Msg])
    end;
print_event(Dev, Dbg, Name) ->
    io:format(Dev, "*DBG* ~tp : ~tp~n", [Name, Dbg]).


%% server_add_handler(Handler, Args, MSL) -> {Ret, MSL'}.
%%   where MSL = [#handler{}]
%%   Ret goes to the top level MSL' is the new internal state of the
%%   event handler

server_add_handler({Mod,Id}, Args, MSL) ->
    Handler = #handler{module = Mod,
		       id = Id},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_handler(Mod, Args, MSL) ->
    Handler = #handler{module = Mod},
    server_add_handler(Mod, Handler, Args, MSL).

server_add_handler(Mod, Handler, Args, MSL) ->
    case catch Mod:init(Args) of
        {ok, State} ->
	    {false, ok, [Handler#handler{state = State}|MSL]};
        {ok, State, hibernate} ->
	    {true, ok, [Handler#handler{state = State}|MSL]};
        Other ->
            {false, Other, MSL}
    end.

%% Set up a link to the supervising process.
%% (Ought to be unidirected links here, Erl5.0 !!)
%% NOTE: This link will not be removed then the
%% handler is removed in case another handler has
%% own link to this process.
server_add_sup_handler({Mod,Id}, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
		       id = Id,
		       supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_sup_handler(Mod, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
		       supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL).

%% server_delete_handler(HandlerId, Args, MSL) -> {Ret, MSL'}

server_delete_handler(HandlerId, Args, MSL, SName) ->
    case split(HandlerId, MSL) of
	{Mod, Handler, MSL1} ->
	    {do_terminate(Mod, Handler, Args,
			  Handler#handler.state, delete, SName, normal),
	     MSL1};
	error ->
	    {{error, module_not_found}, MSL}
    end.

%% server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SN) -> MSL'
%% server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SN) -> MSL'

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SName) ->
    {State2, Sup, MSL1} = split_and_terminate(Handler1, Args1, MSL,
					      SName, Handler2, false),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
	{Hib, ok, MSL2} ->
	    {Hib, ok, MSL2};
	{Hib, What, MSL2} ->
	    {Hib, {error, What}, MSL2}
    end.

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SName) ->
    {State2, _, MSL1} = split_and_terminate(Handler1, Args1, MSL,
					    SName, Handler2, Sup),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
	{Hib, ok, MSL2} ->
	    {Hib, ok, MSL2};
	{Hib, What, MSL2} ->
	    {Hib, {error, What}, MSL2}
    end.

s_s_h(false, Handler, Args, MSL) ->
    server_add_handler(Handler, Args, MSL);
s_s_h(Pid, Handler, Args, MSL) ->
    server_add_sup_handler(Handler, Args, MSL, Pid).

split_and_terminate(HandlerId, Args, MSL, SName, Handler2, Sup) ->
    case split(HandlerId, MSL) of
	{Mod, Handler, MSL1} ->
	    OldSup = Handler#handler.supervised,
	    NewSup = if
			 not Sup -> OldSup;
			 true    -> Sup
		     end,
	    {do_terminate(Mod, Handler, Args,
			  Handler#handler.state, swapped, SName,
			  {swapped, Handler2, NewSup}),
	     OldSup,
	     MSL1};
	error ->
            {error, false, MSL}
    end.

%% server_notify(Event, Func, MSL, SName) -> MSL'

server_notify(Event, Func, [Handler|T], SName) ->
    case server_update(Handler, Func, Event, SName) of
	{ok, Handler1} ->
	    {Hib, NewHandlers} = server_notify(Event, Func, T, SName),
	    {Hib, [Handler1|NewHandlers]};
	{hibernate, Handler1} ->
	    {_Hib, NewHandlers} = server_notify(Event, Func, T, SName),
	    {true, [Handler1|NewHandlers]};
	no ->
	    server_notify(Event, Func, T, SName)
    end;
server_notify(_, _, [], _) ->
    {false, []}.

%% server_update(Handler, Func, Event, ServerName) -> Handler1 | no

server_update(Handler1, Func, Event, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:Func(Event, State) of
	{ok, State1} ->
	    {ok, Handler1#handler{state = State1}};
	{ok, State1, hibernate} ->
	    {hibernate, Handler1#handler{state = State1}};
	{swap_handler, Args1, State1, Handler2, Args2} ->
	    do_swap(Mod1, Handler1, Args1, State1, Handler2, Args2, SName);
	remove_handler ->
	    do_terminate(Mod1, Handler1, remove_handler, State,
			 remove, SName, normal),
	    no;
        {'EXIT', {undef, [{Mod1, handle_info, [_,_], _}|_]}} ->
            ?LOG_WARNING(#{label=>{gen_event,no_handle_info},
                           module=>Mod1,
                           message=>Event},
                         #{domain=>[otp],
                           report_cb=>fun gen_event:format_log/2,
                           error_logger=>
                               #{tag=>warning_msg, % warningmap??
                                 report_cb=>fun gen_event:format_log/1}}),
            {ok, Handler1};
	Other ->
	    do_terminate(Mod1, Handler1, {error, Other}, State,
			 Event, SName, crash),
	    no
    end.

do_swap(Mod1, Handler1, Args1, State1, Handler2, Args2, SName) ->
    %% finalise the existing handler
    State2 = do_terminate(Mod1, Handler1, Args1, State1,
			  swapped, SName,
			  {swapped, Handler2, Handler1#handler.supervised}),
    {Mod2, Handler} = new_handler(Handler2, Handler1),
    case catch Mod2:init({Args2, State2}) of
	{ok, State2a} ->
	    {ok, Handler#handler{state = State2a}};
	Other ->
	    report_terminate(Handler, crash, {error, Other}, SName, false),
	    no
    end.

new_handler({Mod,Id}, Handler1) ->
    {Mod, #handler{module = Mod,
		   id = Id,
		   supervised = Handler1#handler.supervised}};
new_handler(Mod, Handler1) ->
    {Mod, #handler{module = Mod,
		   supervised = Handler1#handler.supervised}}.


-spec split(handler(), [#handler{}]) ->
	{atom(), #handler{}, [#handler{}]} | 'error'.

split(Ha, MSL) -> split(Ha, MSL, []).

split({Mod,Id}, [Ha|T], L) when Ha#handler.module =:= Mod,
                                Ha#handler.id =:= Id ->
    {Mod, Ha, lists:reverse(L, T)};
split(Mod, [Ha|T], L) when Ha#handler.module =:= Mod,
                           not Ha#handler.id ->
    {Mod, Ha, lists:reverse(L, T)};
split(Ha, [H|T], L) ->
    split(Ha, T, [H|L]);
split(_, [], _) ->
    error.

%% server_call(Handler, Query, MSL, ServerName) ->
%%    {Reply, MSL1}

server_call(Handler, Query, MSL, SName) ->
    case search(Handler, MSL) of
	{ok, Ha} ->
	    case server_call_update(Ha, Query, SName) of
		{no, Reply} ->
		    {false, Reply, delete(Handler, MSL)};
		{{ok, Ha1}, Reply} ->
		    {false, Reply, replace(Handler, MSL, Ha1)};
		{{hibernate, Ha1}, Reply} ->
		    {true, Reply, replace(Handler, MSL, Ha1)}
	    end;
	false ->
	    {false, {error, bad_module}, MSL}
    end.

search({Mod, Id}, [Ha|_MSL]) when Ha#handler.module =:= Mod,
				  Ha#handler.id =:= Id ->
    {ok, Ha};
search(Mod, [Ha|_MSL]) when Ha#handler.module =:= Mod,
			    not Ha#handler.id ->
    {ok, Ha};
search(Handler, [_|MSL]) ->
    search(Handler, MSL);
search(_, []) ->
    false.

delete({Mod, Id}, [Ha|MSL]) when Ha#handler.module =:= Mod,
                                 Ha#handler.id =:= Id ->
    MSL;
delete(Mod, [Ha|MSL]) when Ha#handler.module =:= Mod,
                           not Ha#handler.id ->
    MSL;
delete(Handler, [Ha|MSL]) ->
    [Ha|delete(Handler, MSL)];
delete(_, []) ->
    [].

replace({Mod, Id}, [Ha|MSL], NewHa) when Ha#handler.module =:= Mod,
                                         Ha#handler.id =:= Id ->
    [NewHa|MSL];
replace(Mod, [Ha|MSL], NewHa) when Ha#handler.module =:= Mod,
                                   not Ha#handler.id ->
    [NewHa|MSL];
replace(Handler, [Ha|MSL], NewHa) ->
    [Ha|replace(Handler, MSL, NewHa)];
replace(_, [], NewHa) ->
    [NewHa].

%% server_call_update(Handler, Query, ServerName) ->
%%    {{Handler1, State1} | 'no', Reply}

server_call_update(Handler1, Query, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:handle_call(Query, State) of
	{ok, Reply, State1} ->
	    {{ok, Handler1#handler{state = State1}}, Reply};
	{ok, Reply, State1, hibernate} ->
	    {{hibernate, Handler1#handler{state = State1}},
	     Reply};
	{swap_handler, Reply, Args1, State1, Handler2, Args2} ->
	    {do_swap(Mod1,Handler1,Args1,State1,Handler2,Args2,SName), Reply};
	{remove_handler, Reply} ->
	    do_terminate(Mod1, Handler1, remove_handler, State,
			 remove, SName, normal),
	    {no, Reply};
	Other ->
	    do_terminate(Mod1, Handler1, {error, Other}, State,
			 Query, SName, crash),
	    {no, {error, Other}}
    end.

do_terminate(Mod, Handler, Args, State, LastIn, SName, Reason) ->
    case erlang:function_exported(Mod, terminate, 2) of
	true ->
	    Res = (catch Mod:terminate(Args, State)),
	    report_terminate(Handler, Reason, Args, State, LastIn, SName, Res),
	    Res;
	false ->
	    report_terminate(Handler, Reason, Args, State, LastIn, SName, ok),
	    ok
    end.

-spec report_terminate(_, How, _, _, _, _, _) -> ok when
      How :: crash | normal | shutdown | {swapped, handler(), false | pid()}.
report_terminate(Handler, crash, {error, Why}, State, LastIn, SName, _) ->
    report_terminate(Handler, Why, State, LastIn, SName);
report_terminate(Handler, How, _, State, LastIn, SName, _) ->
    %% How == normal | shutdown | {swapped, NewHandler, NewSupervisor}
    report_terminate(Handler, How, State, LastIn, SName).

report_terminate(Handler, Reason, State, LastIn, SName) ->
    report_error(Handler, Reason, State, LastIn, SName),
    case Handler#handler.supervised of
	false ->
	    ok;
	Pid ->
	    Pid ! {gen_event_EXIT,handler(Handler),Reason},
	    ok
    end.

report_error(_Handler, normal, _, _, _)             -> ok;
report_error(_Handler, shutdown, _, _, _)           -> ok;
report_error(_Handler, {swapped,_,_}, _, _, _)      -> ok;
report_error(Handler, Exit, State, LastIn, SName) ->

    %% The reason comes from a catch expression, so we remove
    %% the 'EXIT' and stacktrace from it so that the format_status
    %% callback does not have deal with that.
    {Reason, ReasonFun} =
        case Exit of
            {'EXIT',{R,ST}} ->
                {R, fun(Reason) -> {'EXIT',{Reason,ST}} end};
            {'EXIT',R} ->
                {R, fun(Reason) -> {'EXIT',Reason} end};
            R ->
                {R, fun(Reason) -> Reason end}
        end,
    Status = gen:format_status(
               Handler#handler.module,
               terminate,
               #{ state => State,
                  message => LastIn,
                  reason => Reason
                },
               [get(), State]),
    ?LOG_ERROR(#{label=>{gen_event,terminate},
                 handler=>handler(Handler),
                 name=>SName,
                 last_message=>maps:get(message,Status),
                 state=>maps:get('$status',Status,maps:get(state,Status)),
                 reason=>ReasonFun(maps:get(reason,Status)),
                 process_label=>proc_lib:get_label(self())},
               #{domain=>[otp],
                 report_cb=>fun gen_event:format_log/2,
                 error_logger=>#{tag=>error,
                                 report_cb=>fun gen_event:format_log/1}}).

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
    format_log_multi(limit_report(Report, Depth), FormatOpts).

limit_report(Report, unlimited) ->
    Report;
limit_report(#{label:={gen_event,terminate},
               last_message:=LastIn,
               state:=State,
               reason:=Reason,
               process_label:=ProcessLabel}=Report,
             Depth) ->
    Report#{last_message => io_lib:limit_term(LastIn, Depth),
            state => io_lib:limit_term(State, Depth),
            reason => io_lib:limit_term(Reason, Depth),
            process_label => io_lib:limit_term(ProcessLabel, Depth)};
limit_report(#{label:={gen_event,no_handle_info},
               message:=Msg}=Report,
             Depth) ->
    Report#{message => io_lib:limit_term(Msg, Depth)}.

%% format_log/2 is the report callback for any Logger handler, except
%% error_logger.
-doc false.
format_log(Report, FormatOpts0) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    FormatOpts = maps:merge(Default, FormatOpts0),
    IoOpts =
        case FormatOpts of
            #{chars_limit:=unlimited} ->
                [];
            #{chars_limit:=Limit} ->
                [{chars_limit,Limit}]
        end,
    {Format,Args} = format_log_single(Report, FormatOpts),
    io_lib:format(Format, Args, IoOpts).

format_log_single(#{label:={gen_event,terminate},
                    handler:=Handler,
                    name:=SName,
                    last_message:=LastIn,
                    state:=State,
                    reason:=Reason,
                    process_label:=ProcessLabel},
                  #{single_line:=true, depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Reason1 = fix_reason(Reason),
    Format1 = lists:append(["Generic event handler ",P," crashed. "
                            "Installed: ",P,
                            case ProcessLabel of
                                undefined -> "";
                                _ -> ". Label: "++P
                            end,
                            ". Last event: ",P,
                            ". State: ",P,". Reason: ",P,"."]),
    Args1 =
        case Depth of
            unlimited ->
                [Handler,SName] ++
                case ProcessLabel of
                    undefined -> [];
                    _ -> [ProcessLabel]
                end ++
                [LastIn,State,Reason1];
            _ ->
                [Handler,Depth,SName,Depth] ++
                case ProcessLabel of
                    undefined -> [];
                    _ -> [ProcessLabel,Depth]
                end ++
                [LastIn,Depth,State,Depth,Reason1,Depth]
        end,
    {Format1, Args1};
format_log_single(#{label:={gen_event,no_handle_info},
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

format_log_multi(#{label:={gen_event,terminate},
                   handler:=Handler,
                   name:=SName,
                   last_message:=LastIn,
                   state:=State,
                   reason:=Reason,
                   process_label:=ProcessLabel},
                 #{depth:=Depth}=FormatOpts) ->
    Reason1 = fix_reason(Reason),
    P = p(FormatOpts),
    Format =
        lists:append(["** gen_event handler ",P," crashed.\n",
                      "** Was installed in ",P,"\n",
                      case ProcessLabel of
                          undefined -> [];
                          _ -> "** Process label == "++P++"\n"
                      end,
                      "** Last event was: ",P,"\n",
                      "** When handler state == ",P,"\n",
                      "** Reason == ",P,"\n"]),
    Args =
        case Depth of
            unlimited ->
                [Handler, SName] ++
                case ProcessLabel of
                    undefined -> [];
                    _ -> [ProcessLabel]
                end ++
                [LastIn,State,Reason1];
            _ ->
                [Handler, Depth, SName, Depth] ++
                case ProcessLabel of
                    undefined -> [];
                    _ -> [ProcessLabel, Depth]
                end ++
                [LastIn, Depth, State, Depth, Reason1, Depth]
        end,
    {Format,Args};
format_log_multi(#{label:={gen_event,no_handle_info},
                   module:=Mod,
                   message:=Msg},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format =
        "** Undefined handle_info in ~p\n"
        "** Unhandled message: "++P++"\n",
    Args =
        case Depth of
            unlimited ->
                [Mod,Msg];
            _ ->
                [Mod,Msg,Depth]
        end,
    {Format,Args}.

fix_reason({'EXIT',{undef,[{M,F,A,_L}|_]=MFAs}=Reason}) ->
    case code:is_loaded(M) of
        false ->
            {'module could not be loaded',MFAs};
        _ ->
            case erlang:function_exported(M, F, length(A)) of
                true ->
                    Reason;
                false ->
                    {'function not exported',MFAs}
            end
    end;
fix_reason({'EXIT',Reason}) ->
    Reason;
fix_reason(Reason) ->
    Reason.

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

handler(Handler) when not Handler#handler.id ->
    Handler#handler.module;
handler(Handler) ->
    {Handler#handler.module, Handler#handler.id}.

the_handlers(MSL) ->
    [handler(Handler) || Handler <- MSL].

%% stop_handlers(MSL, ServerName) -> []

stop_handlers([Handler|T], SName) ->
    Mod = Handler#handler.module,
    do_terminate(Mod, Handler, stop, Handler#handler.state,
		 stop, SName, shutdown),
    stop_handlers(T, SName);
stop_handlers([], _) ->
    [].

%% Message from the release_handler.
%% The list of modules got to be a set, i.e. no duplicate elements!
get_modules(MSL) ->
    Mods = [Handler#handler.module || Handler <- MSL],
    ordsets:to_list(ordsets:from_list(Mods)).

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
-doc false.
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [ServerName, MSL, _HibernateAfterTimeout, _Hib]] = StatusData,
    Header = gen:format_status_header("Status for event handler", ServerName),
    {FmtMSL, Logs} =
        lists:mapfoldl(
          fun(#handler{module = Mod, state = State} = MS, Logs) ->
                  Status = gen:format_status(
                             Mod, Opt, #{ log => Logs, state => State },
                             [PDict, State]),
                  {MS#handler{state=maps:get('$status',Status,maps:get(state,Status))},
                   maps:get(log,Status)}
          end, sys:get_log(Debug), MSL),
    [{header, Header},
     {data, [{"Status", SysState},
             {"Logged Events", Logs},
	     {"Parent", Parent}]},
     {items, {"Installed handlers", FmtMSL}}].
