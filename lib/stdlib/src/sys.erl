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
-module(sys).
-moduledoc """
A functional interface to system messages.

This module contains functions for sending system messages used by programs, and
messages used for debugging purposes.

Functions used for implementation of processes are also expected to understand
system messages, such as debug messages and code change. These functions must be
used to implement the use of system messages for a process; either directly, or
through standard behaviors, such as `m:gen_server`.

The default time-out is 5000 ms, unless otherwise specified. `timeout` defines
the time to wait for the process to respond to a request. If the process does
not respond, the function evaluates [`exit({timeout, {M, F, A}})`](`exit/1`).

[](){: #dbg_opt }

The functions make references to a debug structure. The debug structure is a
list of `t:dbg_opt/0`, which is an internal data type used by function
`handle_system_msg/6`. No debugging is performed if it is an empty list.

## System Messages

Processes that are not implemented as one of the standard behaviors must still
understand system messages. The following three messages must be understood:

- Plain system messages. These are received as `{system, From, Msg}`. The
  content and meaning of this message are not interpreted by the receiving
  process module. When a system message is received, function
  `handle_system_msg/6` is called to handle the request.
- Shutdown messages. If the process traps exits, it must be able to handle a
  shutdown request from its parent, the supervisor. The message
  `{'EXIT', Parent, Reason}` from the parent is an order to terminate. The
  process must terminate when this message is received, normally with the same
  `Reason` as `Parent`.
- If the modules used to implement the process change dynamically during
  runtime, the process must understand one more message. An example is the
  `m:gen_event` processes. The message is `{_Label, {From, Ref}, get_modules}`.
  The reply to this message is `From ! {Ref, Modules}`, where `Modules` is a
  list of the currently active modules in the process.

  This message is used by the release handler to find which processes that
  execute a certain module. The process can later be suspended and ordered to
  perform a code change for one of its modules.

## System Events

When debugging a process with the functions of this module, the process
generates _system_events_, which are then treated in the debug function. For
example, `trace` formats the system events to the terminal.

Four predefined system events are used when a process receives or sends a
message. The process can also define its own system events. It is always up to
the process itself to format these events.
""".
-moduledoc(#{titles =>
                 [{function,<<"Process Implementation Functions">>},
                  {callback,<<"Process Implementation Functions">>}]}).

%% External exports
-export([suspend/1, suspend/2, resume/1, resume/2,
	 get_status/1, get_status/2,
	 get_state/1, get_state/2,
	 replace_state/2, replace_state/3,
	 change_code/4, change_code/5,
	 terminate/2, terminate/3,
	 log/2, log/3, trace/2, trace/3, statistics/2, statistics/3,
	 log_to_file/2, log_to_file/3, no_debug/1, no_debug/2,
	 install/2, install/3, remove/2, remove/3]).
-export([handle_system_msg/6, handle_system_msg/7, handle_debug/4,
	 print_log/1, get_log/1, get_debug/3, debug_options/1, suspend_loop_hib/6]).

-deprecated([{get_debug,3,
              "incorrectly documented and only for internal use. Can often "
              "be replaced with sys:get_log/1"}]).

%%-----------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------

-export_type([dbg_opt/0, dbg_fun/0, debug_option/0, system_event/0]).

-type name()         :: pid() | atom()
                      | {'global', term()}
                      | {'via', module(), term()}.
-doc """
- **`{in,Msg}`** - Is produced by `gen_server` and `gen_event` when the message
  `Msg` arrives.

- **`{in,Msg,State}`** - Is produced by `gen_statem` when the message `Msg`
  arrives in state `State`.

  For `gen_statem` the `Msg` term is an `{EventType,EventContent}` tuple.

- **`{out,Msg,To}`** - Is produced by `gen_statem` when the reply `Msg` is sent
  back to `To` by returning a `{reply,To,Msg}` action from the callback module.

  `To` is of the same type as the first argument to `gen_statem:reply/2`.

- **`{out,Msg,To,State}`** - Is produced by `gen_server` when the reply `Msg` is
  sent back to `To` by returning a `{reply,...}` tuple from the callback module.

  `To` is of the same type as the first argument to `gen_server:reply/2`.

  `State` is the new server state.

- **`{noreply,State}`** - Is produced by `gen_server` when a `{noreply,...}`
  tuple is returned from the callback module.

  `State` is the new server state.

- **`{continue,Continuation}`** - Is produced by `gen_server` when a
  `{continue,Continuation}` tuple is returned from the callback module.

- **`{postpone,Event,State,NextState}`** - Is produced by `gen_statem` when the
  message `Event` is postponed in state `State`. `NextState` is the new state.

  `Event` is an `{EventType,EventContent}` tuple.

- **`{consume,Event,State,NextState}`** - Is produced by `gen_statem` when the
  message `Event` is consumed in state `State`. `NextState` is the new state.

  `Event` is an `{EventType,EventContent}` tuple.

- **`{start_timer,Action,State}`** - Is produced by `gen_statem` when the action
  `Action` starts a timer in state `State`.

- **`{insert_timeout,Event,State}`** - Is produced by `gen_statem` when a
  timeout zero action inserts event `Event` in state `State`.

  `Event` is an `{EventType,EventContent}` tuple.

- **`{enter,Module,State}`** - Is produced by `gen_statem` when module `Module`
  enters the first state `State`.

- **`{module,Module,State}`** - Is produced by `gen_statem` when setting module
  `Module` in state `State`.

- **`{terminate,Reason,State}`** - Is produced by `gen_statem` when it
  terminates with reason `Reason` in state `State`.
""".
-type system_event() :: {'in', Msg :: _}
                      | {'in', Msg :: _, State :: _}
                      | {'out', Msg :: _, To :: _}
                      | {'out', Msg :: _, To :: _, State :: _}
                      | {'noreply', State :: _}
                      | {'continue', Continuation :: _}
                      | {'postpone', Event :: _, State :: _, NextState :: _}
                      | {'consume', Event :: _, State :: _, NextState :: _}
                      | {'start_timer', Action :: _, State :: _}
                      | {'insert_timeout', Event :: _, State :: _}
                      | {'enter', Module :: module(), State :: _}
                      | {'module', Module :: module(), State :: _}
                      | {'terminate', Reason :: _, State :: _}
                      | term().
-doc "See the introduction of this manual page.".
-opaque dbg_opt()    :: {'trace', 'true'}
                      | {'log',
                         {N :: non_neg_integer(),
                          [{Event :: system_event(),
                            FuncState :: _,
                            FormFunc :: format_fun()}]}}
                      | {'statistics', {file:date_time(),
                                        {'reductions', non_neg_integer()},
                                        MessagesIn :: non_neg_integer(),
                                        MessagesOut :: non_neg_integer()}}
                      | {'log_to_file', file:io_device()}
                      | {Func :: dbg_fun(), FuncState :: term()}
                      | {FuncId :: term(), Func :: dbg_fun(), FuncState :: term()}.
-type dbg_fun()      :: fun((FuncState :: _,
                             Event :: system_event(),
                             ProcState :: _) -> 'done' | (NewFuncState :: _)).

-type format_fun()   :: fun((Device :: io:device() | file:io_device(),
			     Event :: system_event(),
			     Extra :: term()) -> any()).

-type debug_option() ::
        'trace'
      | 'log'
      | {'log', N :: pos_integer()}
      | 'statistics'
      | {'log_to_file', FileName :: file:name()}
      | {'install',
         {Func :: dbg_fun(), FuncState :: term()}
         | {FuncId :: term(), Func :: dbg_fun(), FuncState :: term()}}.

%%-----------------------------------------------------------------
%% Callbacks
%%-----------------------------------------------------------------

-doc """
Called from `handle_system_msg/6` when the process is to perform a code change.
The code change is used when the internal data structure has changed. This
function converts argument `Misc` to the new data structure. `OldVsn` is
attribute _vsn_ of the old version of the `Module`. If no such attribute is
defined, the atom `undefined` is sent.
""".
-doc(#{title => <<"Process Implementation Functions">>}).
-callback system_code_change(Misc, Module, OldVsn, Extra) -> {ok, NMisc} when
      Misc :: term(),
      OldVsn :: undefined | term(),
      Module :: atom(),
      Extra :: term(),
      NMisc :: term().
-doc """
Called from `handle_system_msg/6` when the process is to continue its execution
(for example, after it has been suspended). This function never returns.
""".
-doc(#{title => <<"Process Implementation Functions">>}).
-callback system_continue(Parent, Debug, Misc) -> no_return() when
      Parent :: pid(),
      Debug :: [dbg_opt()],
      Misc :: term().
-doc """
Called from `handle_system_msg/6` when the process is to return a term that
reflects its current state. `State` is the value returned by `get_state/2`.
""".
-doc(#{title => <<"Process Implementation Functions">>,
       since => <<"OTP 17.0">>}).
-callback system_get_state(Misc) -> {ok, State} when
      Misc :: term(), State :: term().
-doc """
Called from `handle_system_msg/6` when the process is to replace its current
state. `NState` is the value returned by `replace_state/3`.
""".
-doc(#{title => <<"Process Implementation Functions">>,
       since => <<"OTP 17.0">>}).
-callback system_replace_state(StateFun, Misc) -> {ok, NState, NMisc} when
      Misc :: term(),
      NState :: term(),
      NMisc :: term(),
      StateFun :: fun((State :: term()) -> NState).
-doc """
Called from `handle_system_msg/6` when the process is to terminate. For example,
this function is called when the process is suspended and its parent orders
shutdown. It gives the process a chance to do a cleanup. This function never
returns.
""".
-doc(#{title => <<"Process Implementation Functions">>}).
-callback system_terminate(Reason, Parent, Debug, Misc) -> no_return() when
      Reason :: term(),
      Parent :: pid(),
      Debug :: [dbg_opt()],
      Misc :: term().

%%-----------------------------------------------------------------
%% System messages
%%-----------------------------------------------------------------
-doc(#{equiv => suspend/2}).
-spec suspend(Name) -> 'ok' when
      Name :: name().
suspend(Name) -> send_system_msg(Name, suspend).

-doc """
Suspends the process. When the process is suspended, it only responds to other
system messages, but not other messages.
""".
-spec suspend(Name, Timeout) -> 'ok' when
      Name :: name(),
      Timeout :: timeout().
suspend(Name, Timeout) -> send_system_msg(Name, suspend, Timeout).

-doc(#{equiv => resume/2}).
-spec resume(Name) -> 'ok' when
      Name :: name().
resume(Name) -> send_system_msg(Name, resume).

-doc "Resumes a suspended process.".
-spec resume(Name, Timeout) -> 'ok' when
      Name :: name(),
      Timeout :: timeout().
resume(Name, Timeout) -> send_system_msg(Name, resume, Timeout).

-doc(#{equiv => get_status/2}).
-spec get_status(Name) -> Status when
      Name :: name(),
      Status :: {status, Pid :: pid(), {module, Module :: module()}, [SItem]},
      SItem :: (PDict :: [{Key :: term(), Value :: term()}])
             | (SysState :: 'running' | 'suspended')
             | (Parent :: pid())
             | (Dbg :: [dbg_opt()])
             | (Misc :: term()).
get_status(Name) -> send_system_msg(Name, get_status).

-doc """
Gets the status of the process.

The value of `Misc` varies for different types of processes, for example:

- A `m:gen_server` process returns the state of the callback module.
- A `m:gen_statem` process returns information, such as its current state name
  and state data.
- A `m:gen_event` process returns information about each of its registered
  handlers.

Callback modules for `gen_server`, `gen_statem`, and `gen_event` can also change
the value of `Misc` by exporting a function `format_status/2`, which contributes
module-specific information. For details, see `c:gen_server:format_status/2`,
`c:gen_statem:format_status/2`, and `c:gen_event:format_status/2`.
""".
-spec get_status(Name, Timeout) -> Status when
      Name :: name(),
      Timeout :: timeout(),
      Status :: {status, Pid :: pid(), {module, Module :: module()}, [SItem]},
      SItem :: (PDict :: [{Key :: term(), Value :: term()}])
             | (SysState :: 'running' | 'suspended')
             | (Parent :: pid())
             | (Dbg :: [dbg_opt()])
             | (Misc :: term()).
get_status(Name, Timeout) -> send_system_msg(Name, get_status, Timeout).

-doc(#{equiv => get_state/2}).
-doc(#{since => <<"OTP R16B01">>}).
-spec get_state(Name) -> State when
      Name :: name(),
      State :: term().
get_state(Name) ->
    case send_system_msg(Name, get_state) of
	{error, Reason} -> error(Reason);
	{ok, State} -> State
    end.

-doc """
Gets the state of the process.

> #### Note {: .info }
>
> These functions are intended only to help with debugging. They are provided
> for convenience, allowing developers to avoid having to create their own state
> extraction functions and also avoid having to interactively extract the state
> from the return values of `get_status/1` or `get_status/2` while debugging.

The value of `State` varies for different types of processes, as follows:

- For a `m:gen_server` process, the returned `State` is the state of the
  callback module.
- For a `m:gen_statem` process, `State` is the tuple
  `{CurrentState,CurrentData}`.
- For a `m:gen_event` process, `State` is a list of tuples, where each tuple
  corresponds to an event handler registered in the process and contains
  `{Module, Id, HandlerState}`, as follows:

  - **`Module`** - The module name of the event handler.

  - **`Id`** - The ID of the handler (which is `false` if it was registered
    without an ID).

  - **`HandlerState`** - The state of the handler.

If the callback module exports a function
[`system_get_state/1`](`c:system_get_state/1`), it is called in the target
process to get its state. Its argument is the same as the `Misc` value returned
by [`get_status/1,2`](`get_status/1`), and function
[`Module:system_get_state/1`](`c:system_get_state/1`) is expected to extract the
state of the callback module from it. Function
[`system_get_state/1`](`c:system_get_state/1`) must return `{ok, State}`, where
`State` is the state of the callback module.

If the callback module does not export a
[`system_get_state/1`](`c:system_get_state/1`) function, `get_state/1,2` assumes
that the `Misc` value is the state of the callback module and returns it
directly instead.

If the callback module's [`system_get_state/1`](`c:system_get_state/1`) function
crashes or throws an exception, the caller exits with error
`{callback_failed, {Module, system_get_state}, {Class, Reason}}`, where `Module`
is the name of the callback module and `Class` and `Reason` indicate details of
the exception.

Function [`system_get_state/1`](`c:system_get_state/1`) is primarily useful for
user-defined behaviors and modules that implement OTP
[special processes](`m:sys#process-implementation-functions`). The `gen_server`,
`gen_statem`, and `gen_event` OTP behavior modules export this function, so
callback modules for those behaviors need not to supply their own.

For more information about a process, including its state, see `get_status/1`
and `get_status/2`.
""".
-doc(#{since => <<"OTP R16B01">>}).
-spec get_state(Name, Timeout) -> State when
      Name :: name(),
      Timeout :: timeout(),
      State :: term().
get_state(Name, Timeout) ->
    case send_system_msg(Name, get_state, Timeout) of
	{error, Reason} -> error(Reason);
	{ok, State} -> State
    end.

-doc(#{equiv => replace_state/3}).
-doc(#{since => <<"OTP R16B01">>}).
-spec replace_state(Name, StateFun) -> NewState when
      Name :: name(),
      StateFun :: fun((State :: term()) -> NewState :: term()),
      NewState :: term().
replace_state(Name, StateFun) ->
    case send_system_msg(Name, {replace_state, StateFun}) of
	{error, Reason} -> error(Reason);
	{ok, State} -> State
    end.

-doc """
Replaces the state of the process, and returns the new state.

> #### Note {: .info }
>
> These functions are intended only to help with debugging, and are not to be
> called from normal code. They are provided for convenience, allowing
> developers to avoid having to create their own custom state replacement
> functions.

Function `StateFun` provides a new state for the process. Argument `State` and
the `NewState` return value of `StateFun` vary for different types of processes
as follows:

- For a `m:gen_server` process, `State` is the state of the callback module and
  `NewState` is a new instance of that state.
- For a `m:gen_statem` process, `State` is the tuple
  `{CurrentState,CurrentData}`, and `NewState` is a similar tuple, which can
  contain a new current state, new state data, or both.
- For a `m:gen_event` process, `State` is the tuple `{Module, Id, HandlerState}`
  as follows:

  - **`Module`** - The module name of the event handler.

  - **`Id`** - The ID of the handler (which is `false` if it was registered
    without an ID).

  - **`HandlerState`** - The state of the handler.

  `NewState` is a similar tuple where `Module` and `Id` are to have the same
  values as in `State`, but the value of `HandlerState` can be different.
  Returning a `NewState`, whose `Module` or `Id` values differ from those of
  `State`, leaves the state of the event handler unchanged. For a `gen_event`
  process, `StateFun` is called once for each event handler registered in the
  `gen_event` process.

If a `StateFun` function decides not to effect any change in process state, then
regardless of process type, it can return its `State` argument.

If a `StateFun` function crashes or throws an exception, the original state of
the process is unchanged for `gen_server`, and `gen_statem` processes. For
`gen_event` processes, a crashing or failing `StateFun` function means that only
the state of the particular event handler it was working on when it failed or
crashed is unchanged; it can still succeed in changing the states of other event
handlers registered in the same `gen_event` process.

If the callback module exports a `c:system_replace_state/2` function, it is
called in the target process to replace its state using `StateFun`. Its two
arguments are `StateFun` and `Misc`, where `Misc` is the same as the `Misc`
value returned by [`get_status/1,2`](`get_status/1`). A
[`system_replace_state/2`](`c:system_replace_state/2`) function is expected to
return `{ok, NewState, NewMisc}`, where `NewState` is the new state of the
callback module, obtained by calling `StateFun`, and `NewMisc` is a possibly new
value used to replace the original `Misc` (required as `Misc` often contains the
state of the callback module within it).

If the callback module does not export a
[`system_replace_state/2`](`c:system_replace_state/2`) function,
[`replace_state/2,3`](`replace_state/2`) assumes that `Misc` is the state of the
callback module, passes it to `StateFun` and uses the return value as both the
new state and as the new value of `Misc`.

If the callback module's function
[`system_replace_state/2`](`c:system_replace_state/2`) crashes or throws an
exception, the caller exits with error
`{callback_failed, {Module, system_replace_state}, {Class, Reason}}`, where
`Module` is the name of the callback module and `Class` and `Reason` indicate
details of the exception. If the callback module does not provide a
[`system_replace_state/2`](`c:system_replace_state/2`) function and `StateFun`
crashes or throws an exception, the caller exits with error
`{callback_failed, StateFun, {Class, Reason}}`.

Function [`system_replace_state/2`](`c:system_replace_state/2`) is primarily
useful for user-defined behaviors and modules that implement OTP
[special processes](`m:sys#process-implementation-functions`). The OTP behavior
modules `gen_server`, `gen_statem`, and `gen_event` export this function, so
callback modules for those behaviors need not to supply their own.
""".
-doc(#{since => <<"OTP R16B01">>}).
-spec replace_state(Name, StateFun, Timeout) -> NewState when
      Name :: name(),
      StateFun :: fun((State :: term()) -> NewState :: term()),
      Timeout :: timeout(),
      NewState :: term().
replace_state(Name, StateFun, Timeout) ->
    case send_system_msg(Name, {replace_state, StateFun}, Timeout) of
	{error, Reason} -> error(Reason);
	{ok, State} -> State
    end.

-doc(#{equiv => change_code/5}).
-spec change_code(Name, Module, OldVsn, Extra) -> 'ok' | {error, Reason} when
      Name :: name(),
      Module :: module(),
      OldVsn :: 'undefined' | term(),
      Extra :: term(),
      Reason :: term().
change_code(Name, Mod, Vsn, Extra) ->
    send_system_msg(Name, {change_code, Mod, Vsn, Extra}).

-doc """
Tells the process to change code. The process must be suspended to handle this
message. Argument `Extra` is reserved for each process to use as its own.
Function [`Module:system_code_change/4`](`c:system_code_change/4`) is called.
`OldVsn` is the old version of the `Module`.
""".
-spec change_code(Name, Module, OldVsn, Extra, Timeout) ->
                         'ok' | {error, Reason} when
      Name :: name(),
      Module :: module(),
      OldVsn :: 'undefined' | term(),
      Extra :: term(),
      Timeout :: timeout(),
      Reason :: term().
change_code(Name, Mod, Vsn, Extra, Timeout) ->
    send_system_msg(Name, {change_code, Mod, Vsn, Extra}, Timeout).

-doc(#{equiv => terminate/3}).
-doc(#{since => <<"OTP 18.0">>}).
-spec terminate(Name, Reason) -> 'ok' when
      Name :: name(),
      Reason :: term().
terminate(Name, Reason) ->
    send_system_msg(Name, {terminate, Reason}).

-doc """
Orders the process to terminate with the specified `Reason`. The termination is
done asynchronously, so it is not guaranteed that the process is terminated when
the function returns.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec terminate(Name, Reason, Timeout) -> 'ok' when
      Name :: name(),
      Reason :: term(),
      Timeout :: timeout().
terminate(Name, Reason, Timeout) ->
    send_system_msg(Name, {terminate, Reason}, Timeout).

%%-----------------------------------------------------------------
%% Debug commands
%%-----------------------------------------------------------------

-doc(#{equiv => log/3}).
-spec log(Name, Flag) -> 'ok' | {'ok', [system_event()]} when
      Name :: name(),
      Flag :: 'true' |
              {'true', N :: pos_integer()}
            | 'false' | 'get' | 'print'.
log(Name, Flag) ->
    send_system_msg(Name, {debug, {log, Flag}}).

-doc """
Turns the logging of system events on or off. If on, a maximum of `N` events are
kept in the debug structure (default is 10).

If `Flag` is `get`, a list of all logged events is returned.

If `Flag` is `print`, the logged events are printed to
[`standard_io`](`t:io:standard_io/0`).

The events are formatted with a function that is defined by the process that
generated the event (with a call to [`handle_debug/4`)](`handle_debug/4`).
""".
-spec log(Name, Flag, Timeout) -> 'ok' | {'ok', [system_event()]} when
      Name :: name(),
      Flag :: 'true' |
              {'true', N :: pos_integer()}
            | 'false' | 'get' | 'print',
      Timeout :: timeout().
log(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {log, Flag}}, Timeout).

-doc(#{equiv => trace/3}).
-spec trace(Name, Flag) -> 'ok' when
      Name :: name(),
      Flag :: boolean().
trace(Name, Flag) ->
    send_system_msg(Name, {debug, {trace, Flag}}).

-doc """
Prints all system events on [`standard_io`](`t:io:standard_io/0`). The events
are formatted with a function that is defined by the process that generated the
event (with a call to `handle_debug/4`).
""".
-spec trace(Name, Flag, Timeout) -> 'ok' when
      Name :: name(),
      Flag :: boolean(),
      Timeout :: timeout().
trace(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {trace, Flag}}, Timeout).

-doc(#{equiv => log_to_file/3}).
-spec log_to_file(Name, Flag) -> 'ok' | {'error','open_file'} when
      Name :: name(),
      Flag :: (FileName :: string()) | 'false'.
log_to_file(Name, FileName) ->
    send_system_msg(Name, {debug, {log_to_file, FileName}}).

-doc """
Enables or disables the logging of all system events in text format to the file.
The events are formatted with a function that is defined by the process that
generated the event (with a call to `handle_debug/4`). The file is opened with
encoding UTF-8.
""".
-spec log_to_file(Name, Flag, Timeout) -> 'ok' | {'error','open_file'} when
      Name :: name(),
      Flag :: (FileName :: string()) | 'false',
      Timeout :: timeout().
log_to_file(Name, FileName, Timeout) ->
    send_system_msg(Name, {debug, {log_to_file, FileName}}, Timeout).

-doc(#{equiv => statistics/3}).
-spec statistics(Name, Flag) -> 'ok' | {'ok', Statistics} when
      Name :: name(),
      Flag :: 'true' | 'false' | 'get',
      Statistics :: [StatisticsTuple] | no_statistics,
      StatisticsTuple :: {'start_time', DateTime1}
                       | {'current_time', DateTime2}
                       | {'reductions', non_neg_integer()}
                       | {'messages_in', non_neg_integer()}
                       | {'messages_out', non_neg_integer()},
      DateTime1 :: file:date_time(),
      DateTime2 :: file:date_time().
statistics(Name, Flag) ->
    send_system_msg(Name, {debug, {statistics, Flag}}).

-doc """
Enables or disables the collection of statistics. If `Flag` is `get`, the
statistical collection is returned.
""".
-spec statistics(Name, Flag, Timeout) -> 'ok' | {'ok', Statistics} when
      Name :: name(),
      Flag :: 'true' | 'false' | 'get',
      Statistics :: [StatisticsTuple] | no_statistics,
      StatisticsTuple :: {'start_time', DateTime1}
                       | {'current_time', DateTime2}
                       | {'reductions', non_neg_integer()}
                       | {'messages_in', non_neg_integer()}
                       | {'messages_out', non_neg_integer()},
      DateTime1 :: file:date_time(),
      DateTime2 :: file:date_time(),
      Timeout :: timeout().
statistics(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {statistics, Flag}}, Timeout).

-doc(#{equiv => no_debug/2}).
-spec no_debug(Name) -> 'ok' when
      Name :: name().
no_debug(Name) -> send_system_msg(Name, {debug, no_debug}).

-doc """
Turns off all debugging for the process. This includes functions that are
installed explicitly with function [`install/2,3`](`install/2`), for example,
triggers.
""".
-spec no_debug(Name, Timeout) -> 'ok' when
      Name :: name(),
      Timeout :: timeout().
no_debug(Name, Timeout) -> send_system_msg(Name, {debug, no_debug}, Timeout).

-doc(#{equiv => install/3}).
-spec install(Name, FuncSpec) -> 'ok' when
      Name :: name(),
      FuncSpec :: {Func, FuncState} | {FuncId, Func, FuncState},
      FuncId :: term(),
      Func :: dbg_fun(),
      FuncState :: term().
install(Name, {Func, FuncState}) ->
    send_system_msg(Name, {debug, {install, {Func, FuncState}}});
install(Name, {FuncId, Func, FuncState}) ->
    send_system_msg(Name, {debug, {install, {FuncId, Func, FuncState}}}).

-doc """
Enables installation of alternative debug functions. An example of such a
function is a trigger, a function that waits for some special event and performs
some action when the event is generated. For example, turning on low-level
tracing.

`Func` is called whenever a system event is generated. This function is to
return `done`, or a new `Func` state. In the first case, the function is
removed. It is also removed if the function fails. If one debug function should
be installed more times, a unique `FuncId` must be specified for each
installation.
""".
-spec install(Name, FuncSpec, Timeout) -> 'ok' when
      Name :: name(),
      FuncSpec :: {Func, FuncState} | {FuncId, Func, FuncState},
      FuncId :: term(),
      Func :: dbg_fun(),
      FuncState :: term(),
      Timeout :: timeout().
install(Name, {Func, FuncState}, Timeout) ->
    send_system_msg(Name, {debug, {install, {Func, FuncState}}}, Timeout);
install(Name, {FuncId, Func, FuncState}, Timeout) ->
    send_system_msg(Name, {debug, {install, {FuncId, Func, FuncState}}}, Timeout).

-doc(#{equiv => remove/3}).
-spec remove(Name, Func | FuncId) -> 'ok' when
      Name :: name(),
      Func :: dbg_fun(),
      FuncId :: term().
remove(Name, FuncOrFuncId) ->
    send_system_msg(Name, {debug, {remove, FuncOrFuncId}}).

-doc """
Removes an installed debug function from the process. `Func` or `FuncId` must be
the same as previously installed.
""".
-spec remove(Name, Func | FuncId, Timeout) -> 'ok' when
      Name :: name(),
      Func :: dbg_fun(),
      FuncId :: term(),
      Timeout :: timeout().
remove(Name, FuncOrFuncId, Timeout) ->
    send_system_msg(Name, {debug, {remove, FuncOrFuncId}}, Timeout).

%%-----------------------------------------------------------------
%% All system messages sent are on the form {system, From, Msg}
%% The receiving side should send Msg to handle_system_msg/5.
%%-----------------------------------------------------------------
send_system_msg(Name, Request) ->
    try gen:call(Name, system, Request) of
        {ok, Res} ->
            Res
    catch exit : Reason ->
            exit({Reason, mfa(Name, Request)})
    end.

send_system_msg(Name, Request, Timeout) ->
    try gen:call(Name, system, Request, Timeout) of
        {ok, Res} ->
            Res
    catch exit : Reason ->
            exit({Reason, mfa(Name, Request, Timeout)})
    end.

mfa(Name, {debug, {Func, Arg2}}) ->
    {sys, Func, [Name, Arg2]};
mfa(Name, {change_code, Mod, Vsn, Extra}) ->
    {sys, change_code, [Name, Mod, Vsn, Extra]};
mfa(Name, {terminate, Reason}) ->
    {sys, terminate, [Name, Reason]};
mfa(Name, Atom) ->
    {sys, Atom, [Name]}.

mfa(Name, Req, Timeout) ->
    {M, F, A} = mfa(Name, Req),
    {M, F, A ++ [Timeout]}.

%%-----------------------------------------------------------------
%% Func: handle_system_msg/6
%% Purpose: Used by a process module that wishes to take care of
%%          system messages.  The process receives a {system, From,
%%          Msg} message, and passes the Msg to this function.
%% Returns: This function *never* returns! It calls the function
%%          Module:system_continue(Parent, NDebug, Misc)
%%          there the process continues the execution or
%%          Module:system_terminate(Reason, Parent, Debug, Misc) if
%%          the process should terminate.
%%          The Module must export system_continue/3, system_terminate/4
%%          and format_status/2 for status information.
%%-----------------------------------------------------------------
-doc """
This function is used by a process module to take care of system messages. The
process receives a `{system, From, Msg}` message and passes `Msg` and `From` to
this function.

This function _never_ returns. It calls either of the following functions:

- [`Module:system_continue(Parent, NDebug, Misc)`](`c:system_continue/3`), where
  the process continues the execution.
- [`Module:system_terminate(Reason, Parent, Debug, Misc)`](`c:system_terminate/4`),
  if the process is to terminate.

`Module` must export the following:

- [`system_continue/3`](`c:system_continue/3`)
- [`system_terminate/4`](`c:system_terminate/4`)
- [`system_code_change/4`](`c:system_code_change/4`)
- [`system_get_state/1`](`c:system_get_state/1`)
- [`system_replace_state/2`](`c:system_replace_state/2`)

Argument `Misc` can be used to save internal data in a process, for example, its
state. It is sent to [`Module:system_continue/3`](`c:system_continue/3`) or
[`Module:system_terminate/4`](`c:system_terminate/4`).
""".
-doc(#{title => <<"Process Implementation Functions">>}).
-spec handle_system_msg(Msg, From, Parent, Module, Debug, Misc) ->
                               no_return() when
      Msg :: term(),
      From :: {pid(), Tag :: _},
      Parent :: pid(),
      Module :: module(),
      Debug :: [dbg_opt()],
      Misc :: term().
handle_system_msg(Msg, From, Parent, Module, Debug, Misc) ->
    handle_system_msg(running, Msg, From, Parent, Module, Debug, Misc, false).

-doc false.
handle_system_msg(Msg, From, Parent, Mod, Debug, Misc, Hib) ->
   handle_system_msg(running, Msg, From, Parent, Mod, Debug, Misc, Hib).

handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib) ->
    case do_cmd(SysState, Msg, Parent, Mod, Debug, Misc) of
	{suspended, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
	    suspend_loop(suspended, Parent, Mod, NDebug, NMisc, Hib);
	{running, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
            Mod:system_continue(Parent, NDebug, NMisc);
	{{terminating, Reason}, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
	    Mod:system_terminate(Reason, Parent, NDebug, NMisc)
    end.

%%-----------------------------------------------------------------
%% Func: handle_debug/4
%% Purpose: Called by a process that wishes to debug an event.
%%          Func is a formatting function, called as Func(Device, Event).
%% Returns: [debug_opts()]
%%-----------------------------------------------------------------
-doc """
This function is called by a process when it generates a system event.
`FormFunc` is a formatting function, called as `FormFunc(Device, Event, Extra)`
to print the events, which is necessary if tracing is activated. `Extra` is any
extra information that the process needs in the format function, for example,
the process name.
""".
-doc(#{title => <<"Process Implementation Functions">>}).
-spec handle_debug(Debug, FormFunc, Extra, Event) -> [dbg_opt()] when
      Debug :: [dbg_opt()],
      FormFunc :: format_fun(),
      Extra :: term(),
      Event :: system_event().
handle_debug([{trace, true} = DbgOpt | T], FormFunc, State, Event) ->
    print_event({Event, State, FormFunc}),
    [DbgOpt | handle_debug(T, FormFunc, State, Event)];
handle_debug([{log, NLog} | T], FormFunc, State, Event) ->
    Item = {Event, State, FormFunc},
    [{log, nlog_put(Item, NLog)} | handle_debug(T, FormFunc, State, Event)];
handle_debug([{log_to_file, Fd} = DbgOpt | T], FormFunc, State, Event) ->
    print_event(Fd, {Event, State, FormFunc}),
    [DbgOpt | handle_debug(T, FormFunc, State, Event)];
handle_debug([{statistics, StatData} | T], FormFunc, State, Event) ->
    NStatData = stat(Event, StatData),
    [{statistics, NStatData} | handle_debug(T, FormFunc, State, Event)];
handle_debug([{FuncId, {Func, FuncState}} | T], FormFunc, State, Event) ->
    try Func(FuncState, Event, State) of
        done -> handle_debug(T, FormFunc, State, Event);
        NFuncState ->
            [{FuncId, {Func, NFuncState}} |
             handle_debug(T, FormFunc, State, Event)]
    catch
        done -> handle_debug(T, FormFunc, State, Event);
        NFuncState ->
            [{FuncId, {Func, NFuncState}} |
             handle_debug(T, FormFunc, State, Event)];
        _:_ -> handle_debug(T, FormFunc, State, Event)
    end;
handle_debug([{Func, FuncState} | T], FormFunc, State, Event) ->
    try Func(FuncState, Event, State) of
	done -> handle_debug(T, FormFunc, State, Event);
	NFuncState ->
	    [{Func, NFuncState} | handle_debug(T, FormFunc, State, Event)]
    catch
	done -> handle_debug(T, FormFunc, State, Event);
	NFuncState ->
	    [{Func, NFuncState} | handle_debug(T, FormFunc, State, Event)];
        _:_ -> handle_debug(T, FormFunc, State, Event)
    end;
handle_debug([], _FormFunc, _State, _Event) ->
    [].

%%-----------------------------------------------------------------
%% When a process is suspended, it can only respond to system
%% messages.
%%-----------------------------------------------------------------
suspend_loop(SysState, Parent, Mod, Debug, Misc, Hib) ->
    case Hib of
	true ->
	   suspend_loop_hib(SysState, Parent, Mod, Debug, Misc, Hib);
	_ ->
	    receive
		{system, From, Msg} ->
		    handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib);
		{'EXIT', Parent, Reason} ->
		    Mod:system_terminate(Reason, Parent, Debug, Misc)
	    end
    end.

-doc false.
suspend_loop_hib(SysState, Parent, Mod, Debug, Misc, Hib) ->
    receive
	{system, From, Msg} ->
	    handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib);
	{'EXIT', Parent, Reason} ->
            Mod:system_terminate(Reason, Parent, Debug, Misc)
    after 0 -> % Not a system message, go back into hibernation
	 proc_lib:hibernate(?MODULE, suspend_loop_hib, [SysState, Parent, Mod, 
							Debug, Misc, Hib])
    end.


do_cmd(_, suspend, _Parent, _Mod, Debug, Misc) ->
    {suspended, ok, Debug, Misc};
do_cmd(_, resume, _Parent, _Mod, Debug, Misc) ->
    {running, ok, Debug, Misc};
do_cmd(SysState, get_state, _Parent, Mod, Debug, Misc) ->
    {SysState, do_get_state(Mod, Misc), Debug, Misc};
do_cmd(SysState, {replace_state, StateFun},  _Parent, Mod, Debug, Misc) ->
    {Res, NMisc} = do_replace_state(StateFun, Mod, Misc),
    {SysState, Res, Debug, NMisc};
do_cmd(SysState, get_status, Parent, Mod, Debug, Misc) ->
    Res = get_status(SysState, Parent, Mod, Debug, Misc),
    {SysState, Res, Debug, Misc};
do_cmd(SysState, {debug, What}, _Parent, _Mod, Debug, Misc) ->
    {Res, NDebug} = debug_cmd(What, Debug),
    {SysState, Res, NDebug, Misc};
do_cmd(_, {terminate, Reason}, _Parent, _Mod, Debug, Misc) ->
    {{terminating, Reason}, ok, Debug, Misc};
do_cmd(suspended, {change_code, Module, Vsn, Extra}, _Parent,
       Mod, Debug, Misc) ->
    {Res, NMisc} = do_change_code(Mod, Module, Vsn, Extra, Misc),
    {suspended, Res, Debug, NMisc};
do_cmd(SysState, Other, _Parent, _Mod, Debug, Misc) ->
    {SysState, {error, {unknown_system_msg, Other}}, Debug, Misc}.

do_get_state(Mod, Misc) ->
    case erlang:function_exported(Mod, system_get_state, 1) of
	true ->
	    try Mod:system_get_state(Misc) of
                {ok, _} = Result ->
                    Result;
                Other ->
		    {error,
                     {callback_failed, {Mod,system_get_state},
                      {bad_return,Other}}}
            catch
                Cl : Exc ->
		    {error,
                     {callback_failed, {Mod,system_get_state},
                      {Cl,Exc}}}
            end;
	false ->
	    {ok, Misc}
    end.

do_replace_state(StateFun, Mod, Misc) ->
    case erlang:function_exported(Mod, system_replace_state, 2) of
	true ->
	    try Mod:system_replace_state(StateFun, Misc) of
		{ok, State, NMisc} ->
                    {{ok, State}, NMisc};
                Other ->
		    {{error,
                      {callback_failed, {Mod,system_replace_state},
                       {bad_return,Other}}},
                     Misc}
	    catch
		Cl : Exc ->
		    {{error,
                      {callback_failed, {Mod,system_replace_state},
                       {Cl,Exc}}},
                     Misc}
	    end;
	false ->
	    try StateFun(Misc) of
		NMisc ->
                    {{ok, NMisc}, NMisc}
	    catch
		Cl : Exc ->
		    {{error,
                      {callback_failed, StateFun, {Cl,Exc}}},
                     Misc}
	    end
    end.

get_status(SysState, Parent, Mod, Debug, Misc) ->
    PDict = get(),
    FmtMisc =
        case erlang:function_exported(Mod, format_status, 2) of
            true ->
                FmtArgs = [PDict, SysState, Parent, Debug, Misc],
                Mod:format_status(normal, FmtArgs);
            _ ->
                Misc
        end,
    {status, self(), {module, Mod},
     [PDict, SysState, Parent, Debug, FmtMisc]}.

%%-----------------------------------------------------------------
%% These are the system debug commands.
%% {trace,       true|false} -> io:format
%% {log,         true|false|get|print} -> keeps the 10 last debug messages
%% {log_to_file, FileName | false} -> io:format to file.
%% {statistics,  true|false|get}   -> keeps track of messages in/out + reds.
%%-----------------------------------------------------------------
debug_cmd({trace, true}, Debug) ->
    {ok, install_debug(trace, true, Debug)};
debug_cmd({trace, false}, Debug) ->
    {ok, remove_debug(trace, Debug)};
debug_cmd({log, true}, Debug) ->
    NLog = get_debug(log, Debug, nlog_new()),
    {ok, install_debug(log, nlog_new(NLog), Debug)};
debug_cmd({log, {true, N}}, Debug) when is_integer(N), 1 =< N ->
    NLog = get_debug(log, Debug, nlog_new(N)),
    {ok, install_debug(log, nlog_new(N, NLog), Debug)};
debug_cmd({log, false}, Debug) ->
    {ok, remove_debug(log, Debug)};
debug_cmd({log, print}, Debug) ->
    print_log(Debug),
    {ok, Debug};
debug_cmd({log, get}, Debug) ->
    NLog = get_debug(log, Debug, nlog_new()),
    {{ok, [Event || {Event, _State, _FormFunc} <- nlog_get(NLog)]}, Debug};
debug_cmd({log_to_file, false}, Debug) ->
    NDebug = close_log_file(Debug),
    {ok, NDebug};
debug_cmd({log_to_file, FileName}, Debug) ->
    NDebug = close_log_file(Debug),
    case file:open(FileName, [write,{encoding,utf8}]) of
	{ok, Fd} ->
	    {ok, install_debug(log_to_file, Fd, NDebug)};
	_Error ->
	    {{error, open_file}, NDebug}
    end;
debug_cmd({statistics, true}, Debug) ->
    {ok, install_debug(statistics, init_stat(), Debug)};
debug_cmd({statistics, false}, Debug) ->
    {ok, remove_debug(statistics, Debug)};
debug_cmd({statistics, get}, Debug) ->
    {{ok, get_stat(get_debug(statistics, Debug, []))}, Debug};
debug_cmd(no_debug, Debug) ->
    close_log_file(Debug),
    {ok, []};
debug_cmd({install, {Func, FuncState}}, Debug) ->
    {ok, install_debug(Func, FuncState, Debug)};
debug_cmd({install, {FuncId, Func, FuncState}}, Debug) ->
    {ok, install_debug(FuncId, {Func, FuncState}, Debug)};
debug_cmd({remove, FuncOrFuncId}, Debug) ->
    {ok, remove_debug(FuncOrFuncId, Debug)};
debug_cmd(_Unknown, Debug) ->
    {unknown_debug, Debug}.


do_change_code(Mod, Module, Vsn, Extra, Misc) ->
    case catch Mod:system_code_change(Misc, Module, Vsn, Extra) of
	{ok, NMisc} -> {ok, NMisc};
	Else -> {{error, Else}, Misc}
    end.

print_event(X) -> print_event(standard_io, X).

print_event(Dev, {Event, State, FormFunc}) ->
    FormFunc(Dev, Event, State).

init_stat() -> {erlang:localtime(), process_info(self(), reductions), 0, 0}.

get_stat({Time, {reductions, Reds}, In, Out}) ->
    {reductions, Reds2} = process_info(self(), reductions),
    [{start_time, Time}, {current_time, erlang:localtime()},
     {reductions, Reds2 - Reds}, {messages_in, In}, {messages_out, Out}];
get_stat(_) ->
    no_statistics.

stat({in, _Msg}, {Time, Reds, In, Out}) -> {Time, Reds, In+1, Out};
stat({in, _Msg, _From}, {Time, Reds, In, Out}) -> {Time, Reds, In+1, Out};
stat({out, _Msg, _To}, {Time, Reds, In, Out}) -> {Time, Reds, In, Out+1};
stat({out, _Msg, _To, _State}, {Time, Reds, In, Out}) -> {Time, Reds, In, Out+1};
stat(_, StatData) -> StatData.

%%-----------------------------------------------------------------
%% Debug structure manipulating functions
%%-----------------------------------------------------------------
install_debug(Item, Data, Debug) ->
    case lists:keysearch(Item, 1, Debug) of
        false -> [{Item, Data} | Debug];
        _ -> Debug
    end.
remove_debug(Item, Debug) -> lists:keydelete(Item, 1, Debug).

-doc """
> #### Warning {: .warning }
>
> [`get_debug/3`](`get_debug/3`) is deprecated since it returns data of an
> internal type only useful for debugging.

Gets the data associated with a debug option. `Default` is returned if `Item` is
not found. Can be used by the process to retrieve debug data for printing before
it terminates.
""".
-doc(#{title => <<"Process Implementation Functions">>}).
-spec get_debug(Item, Debug, Default) -> term() when
      Item :: 'log' | 'statistics',
      Debug :: [dbg_opt()],
      Default :: term().
get_debug(Item, Debug, Default) -> 
    get_debug2(Item, Debug, Default).

%% Workaround: accepts more Item types than get_debug/3.
get_debug2(Item, Debug, Default) ->
    case lists:keysearch(Item, 1, Debug) of
	{value, {Item, Data}} -> Data;
	_ -> Default
    end.

-doc """
Prints the logged system events in the debug structure, using `FormFunc` as
defined when the event was generated by a call to `handle_debug/4`.
""".
-doc(#{title => <<"Process Implementation Functions">>}).
-spec print_log(Debug) -> 'ok' when
      Debug :: [dbg_opt()].
print_log(Debug) ->
    NLog = get_debug(log, Debug, nlog_new()),
    lists:foreach(fun print_event/1, nlog_get(NLog)).

-doc """
Returns the logged system events in the debug structure, that is the last
argument to `handle_debug/4`.
""".
-doc(#{title => <<"Process Implementation Functions">>,
       since => <<"OTP-22.0">>}).
-spec get_log(Debug) -> [system_event()] when
      Debug :: [dbg_opt()].
get_log(Debug) ->
    NLog = get_debug(log, Debug, nlog_new()),
    [Event || {Event, _State, _FormFunc} <- nlog_get(NLog)].
    
close_log_file(Debug) ->
    case get_debug2(log_to_file, Debug, []) of
	[] ->
	    Debug;
	Fd -> 
	    ok = file:close(Fd),
	    remove_debug(log_to_file, Debug)
    end.

%%-----------------------------------------------------------------
%% Keep the last N Log functions
%%-----------------------------------------------------------------
%%
%% Streamlined Okasaki queue as base for "keep the last N" log.
%%
%% To the reverse list head we cons new items.
%% The forward list contains elements in insertion order,
%% so the head is the oldest and the one to drop off
%% when the log is full.
%%
%% Here is how we can get away with only using one cons cell
%% to wrap the forward and reverse list, and the log size:
%%
%% A full log does not need a counter; we just cons one
%% and drop one:
%%
%%     [ReverseList|ForwardList]
%%
%% A non-full log is filling up to N elements;
%% use a down counter instead of a list as first element:
%%
%%     [RemainingToFullCount|ReverseList]

nlog_new() ->
    nlog_new(10).
%%
nlog_new([_|_] = NLog) ->
    nlog_new(10, NLog);
nlog_new(N) ->
    [N]. % Empty log size N >= 1
%%
nlog_new(N, NLog) ->
    lists:foldl(
      fun (Item, NL) -> nlog_put(Item, NL) end,
      nlog_new(N),
      nlog_get(NLog)).

%%
nlog_put(Item, NLog) ->
    case NLog of
        [R|FF] when is_list(R) ->
            %% Full log
            case FF of
                [_|F] ->
                    %% Cons to reverse list, drop from forward list
                    [[Item|R]|F];
                [] ->
                    %% Create new forward list from reverse list,
                    %% create new empty reverse list
                    [_|F] = lists:reverse(R, [Item]),
                    [[]|F]
            end;
        [1|R] ->
            %% Log now gets full
            [[Item|R]];
        [J|R] ->
            %% Filling up to N elements
            [J - 1,Item|R]
    end.

nlog_get([[]|F]) ->
    F;
nlog_get([[_|_] = R|F]) ->
    F ++ lists:reverse(R);
nlog_get([_J|R]) ->
    lists:reverse(R).

%%-----------------------------------------------------------------
%% Func: debug_options/1
%% Purpose: Initiate a debug structure.  Called by a process that
%%          wishes to initiate the debug structure without the
%%          system messages.
%% Returns: [debug_opts()]
%%-----------------------------------------------------------------

-doc """
Can be used by a process that initiates a debug structure from a list of
options. The values of argument `Opt` are the same as for the corresponding
functions.
""".
-doc(#{title => <<"Process Implementation Functions">>}).
-spec debug_options([Opt :: debug_option()]) -> [dbg_opt()].
debug_options(Options) ->
    debug_options(Options, []).

debug_options([trace | T], Debug) ->
    debug_options(T, install_debug(trace, true, Debug));
debug_options([log | T], Debug) ->
    debug_options(T, install_debug(log, nlog_new(), Debug));
debug_options([{log, N} | T], Debug) when is_integer(N), N > 0 ->
    debug_options(T, install_debug(log, nlog_new(N), Debug));
debug_options([statistics | T], Debug) ->
    debug_options(T, install_debug(statistics, init_stat(), Debug));
debug_options([{log_to_file, FileName} | T], Debug) ->
    case file:open(FileName, [write,{encoding,utf8}]) of
	{ok, Fd} ->
	    debug_options(T, install_debug(log_to_file, Fd, Debug));
	_Error ->
	    debug_options(T, Debug)
    end;
debug_options([{install, {Func, FuncState}} | T], Debug) ->
    debug_options(T, install_debug(Func, FuncState, Debug));
debug_options([{install, {FuncId, Func, FuncState}} | T], Debug) ->
    debug_options(T, install_debug(FuncId, {Func, FuncState}, Debug));
debug_options([_ | T], Debug) ->
    debug_options(T, Debug);
debug_options([], Debug) -> 
    Debug.
