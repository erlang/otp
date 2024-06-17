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
-module(gen_fsm).

-moduledoc """
Deprecated and replaced by `m:gen_statem` in OTP 20.

Migration to gen_statem
-----------------------

Here follows a simple example of turning a gen_fsm into a `m:gen_statem`.
The example comes from the previous User's Guide for `gen_fsm`

```erlang
-module(code_lock).
-define(NAME, code_lock).
%-define(BEFORE_REWRITE, true).

-ifdef(BEFORE_REWRITE).
-behaviour(gen_fsm).
-else.
-behaviour(gen_statem).
-endif.

-export([start_link/1, button/1, stop/0]).

-ifdef(BEFORE_REWRITE).
-export([init/1, locked/2, open/2, handle_sync_event/4, handle_event/3,
     handle_info/3, terminate/3, code_change/4]).
-else.
-export([init/1, callback_mode/0, locked/3, open/3,
     terminate/3, code_change/4]).
%% Add callback__mode/0
%% Change arity of the state functions
%% Remove handle_info/3
-endif.

-ifdef(BEFORE_REWRITE).
start_link(Code) ->
    gen_fsm:start_link({local, ?NAME}, ?MODULE, Code, []).
-else.
start_link(Code) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
-endif.

-ifdef(BEFORE_REWRITE).
button(Digit) ->
    gen_fsm:send_event(?NAME, {button, Digit}).
-else.
button(Digit) ->
    gen_statem:cast(?NAME, {button,Digit}).
    %% send_event is asynchronous and becomes a cast
-endif.

-ifdef(BEFORE_REWRITE).
stop() ->
    gen_fsm:sync_send_all_state_event(?NAME, stop).
-else.
stop() ->
    gen_statem:call(?NAME, stop).
    %% sync_send is synchronous and becomes call
    %% all_state is handled by callback code in gen_statem
-endif.

init(Code) ->
    do_lock(),
    Data = #{code => Code, remaining => Code},
    {ok, locked, Data}.

-ifdef(BEFORE_REWRITE).
-else.
callback_mode() ->
    state_functions.
%% state_functions mode is the mode most similar to
%% gen_fsm. There is also handle_event mode which is
%% a fairly different concept.
-endif.

-ifdef(BEFORE_REWRITE).
locked({button, Digit}, Data0) ->
    case analyze_lock(Digit, Data0) of
    {open = StateName, Data} ->
        {next_state, StateName, Data, 10000};
    {StateName, Data} ->
        {next_state, StateName, Data}
    end.
-else.
locked(cast, {button,Digit}, Data0) ->
    case analyze_lock(Digit, Data0) of
    {open = StateName, Data} ->
        {next_state, StateName, Data, 10000};
    {StateName, Data} ->
        {next_state, StateName, Data}
    end;
locked({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
locked({info, Msg}, StateName, Data) ->
    handle_info(Msg, StateName, Data).
%% Arity differs
%% All state events are dispatched to handle_call and handle_info help
%% functions. If you want to handle a call or cast event specifically
%% for this state you would add a special clause for it above.
-endif.

-ifdef(BEFORE_REWRITE).
open(timeout, State) ->
     do_lock(),
    {next_state, locked, State};
open({button,_}, Data) ->
    {next_state, locked, Data}.
-else.
open(timeout, _, Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button,_}, Data) ->
    {next_state, locked, Data};
open({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
open(info, Msg, Data) ->
    handle_info(Msg, open, Data).
%% Arity differs
%% All state events are dispatched to handle_call and handle_info help
%% functions. If you want to handle a call or cast event specifically
%% for this state you would add a special clause for it above.
-endif.

-ifdef(BEFORE_REWRITE).
handle_sync_event(stop, _From, _StateName, Data) ->
    {stop, normal, ok, Data}.

handle_event(Event, StateName, Data) ->
    {stop, {shutdown, {unexpected, Event, StateName}}, Data}.

handle_info(Info, StateName, Data) ->
    {stop, {shutdown, {unexpected, Info, StateName}}, StateName, Data}.
-else.
-endif.

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Internal functions
-ifdef(BEFORE_REWRITE).
-else.
handle_call(From, stop, Data) ->
     {stop_and_reply, normal,  {reply, From, ok}, Data}.

handle_info(Info, StateName, Data) ->
    {stop, {shutdown, {unexpected, Info, StateName}}, StateName, Data}.
%% These are internal functions for handling all state events
%% and not behaviour callbacks as in gen_fsm
-endif.

analyze_lock(Digit, #{code := Code, remaining := Remaining} = Data) ->
     case Remaining of
         [Digit] ->
         do_unlock(),
         {open,  Data#{remaining := Code}};
         [Digit|Rest] -> % Incomplete
             {locked, Data#{remaining := Rest}};
         _Wrong ->
             {locked, Data#{remaining := Code}}
     end.

do_lock() ->
    io:format("Lock~n", []).
do_unlock() ->
    io:format("Unlock~n", []).
```

OTP 19 Documentation
--------------------

### Module

`gen_fsm`

### Module Summary

Generic finite state machine behavior.

### Description

This behavior module provides a finite state machine.
A generic finite state machine process (`gen_fsm`) implemented
using this module has a standard set of interface functions
and includes functionality for tracing and error reporting.
It also fits into an OTP supervision tree.  For more information,
see [OTP Design Principles](`e:system:design_principles`).

A `gen_fsm` process assumes all specific parts to be located
in a callback module exporting a predefined set of functions.
The relationship between the behavior functions
and the callback functions is as follows:

``` text
gen_fsm module                    Callback module
--------------                    ---------------
gen_fsm:start
gen_fsm:start_link                -----> Module:init/1

gen_fsm:stop                      -----> Module:terminate/3

gen_fsm:send_event                -----> Module:StateName/2

gen_fsm:send_all_state_event      -----> Module:handle_event/3

gen_fsm:sync_send_event           -----> Module:StateName/3

gen_fsm:sync_send_all_state_event -----> Module:handle_sync_event/4

-                                 -----> Module:handle_info/3

-                                 -----> Module:terminate/3

-                                 -----> Module:code_change/4
```

If a callback function fails or returns a bad value,
the `gen_fsm` process terminates.

A `gen_fsm` process handles system messages as described
in [sys(3)](`m:sys`).  The sys module can be used for
debugging a `gen_fsm` process.

Notice that a `gen_fsm` process does not trap exit signals automatically,
this must be explicitly initiated in the callback module.

Unless otherwise stated, all functions in this module fail
if the specified `gen_fsm` process does not exist
or if bad arguments are specified.

The gen_fsm process can go into hibernation (see `erlang:hibernate/3`)
if a callback function specifies `hibernate` instead of a time-out value.
This can be useful if the server is expected to be idle for a long time.
However, use this feature with care, as hibernation implies at least
two garbage collections (when hibernating and shortly after waking up)
and is not something you want to do between each call
to a busy state machine.

### Callback Functions

See the [Callback Functions](#callbacks-deprecated) section
for the functions to be exported from a `gen_fsm` callback module.

[]() {: #state-name }
**State name** denotes a state of the state machine.

[]() {: #state-data }
**State data** denotes the internal state of the Erlang process
that implements the state machine.
""".
-moduledoc #{titles =>
                 [{callback, ~"deprecated"}]}.

%%%-----------------------------------------------------------------
%%%
%%% This state machine is somewhat more pure than state_lib.  It is
%%% still based on State dispatching (one function per state), but
%%% allows a function handle_event to take care of events in all states.
%%% It's not that pure anymore :(  We also allow synchronized event sending.
%%%
%%% If the Parent process terminates the Module:terminate/2
%%% function is called.
%%%
%%% The user module should export:
%%%
%%%   init(Args)
%%%     ==> {ok, StateName, StateData}
%%%         {ok, StateName, StateData, Timeout}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   StateName(Msg, StateData)
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   StateName(Msg, From, StateData)
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {reply, Reply, NewStateName, NewStateData}
%%%        {reply, Reply, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_event(Msg, StateName, StateData)
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, Reply, NewStateData}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_sync_event(Msg, From, StateName, StateData)
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {reply, Reply, NewStateName, NewStateData}
%%%        {reply, Reply, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, Reply, NewStateData}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_info(Info, StateName) (e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%
%%%    ==> {next_state, NewStateName, NewStateData}
%%%        {next_state, NewStateName, NewStateData, Timeout}
%%%        {stop, Reason, NewStateData}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   terminate(Reason, StateName, StateData) Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> the return value is ignored
%%%
%%%
%%% The work flow (of the fsm) can be described as follows:
%%%
%%%   User module                           fsm
%%%   -----------                          -------
%%%     start              ----->             start
%%%     init               <-----              .
%%%
%%%                                           loop
%%%     StateName          <-----              .
%%%
%%%     handle_event       <-----              .
%%%
%%%     handle__sunc_event <-----              .
%%%
%%%     handle_info        <-----              .
%%%
%%%     terminate          <-----              .
%%%
%%%
%%% ---------------------------------------------------

-include("logger.hrl").

-export([start/3, start/4,
	 start_link/3, start_link/4,
	 stop/1, stop/3,
	 send_event/2, sync_send_event/2, sync_send_event/3,
	 send_all_state_event/2,
	 sync_send_all_state_event/2, sync_send_all_state_event/3,
	 reply/2,
	 start_timer/2,send_event_after/2,cancel_timer/1,
	 enter_loop/4, enter_loop/5, enter_loop/6, wake_hib/7]).

%% Internal exports
-export([init_it/6,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 system_get_state/1,
	 system_replace_state/2,
	 format_status/2]).

%% logger callback
-export([format_log/1, format_log/2]).

-deprecated({'_','_', "use the 'gen_statem' module instead"}).
-deprecated_callback({'_','_', "use the 'gen_statem' module instead"}).

%%% ---------------------------------------------------
%%% Interface functions.
%%% ---------------------------------------------------

-doc "Reply destination. See `reply/2`".
-type from() :: {To :: pid(), Tag :: term()}.

-doc #{ title => ~"deprecated" }.
-doc """
Initialize process and internal [*state name*](#state-name)
and [*state data*](#state-data).

Whenever a `gen_fsm` process is started using
[`start/3,4`](`start/4`) or [`start_link/3,4`](`start_link/4`),
this function is called by the new process to initialize.

`Args` is the `Args` argument provided to the start function.

If initialization is successful, the function is to return
{ok, StateName, StateData}, {ok, StateName, StateData, Timeout},
or {ok, StateName, StateData, hibernate}, where `StateName`
is the initial [*state name*](#state-name) and `StateData`
the initial [*state data*](#state-data) of the `gen_fsm` process.

If an `t:integer/0` time-out value is provided, a time-out occurs
unless an event or a message is received within `Timeout` milliseconds.
A time-out is represented by the atom `timeout` and is to be handled
by the [`Module:StateName/2`](`c:'StateName'/2`) callback functions.
The atom `infinity` can be used to wait indefinitely, this is
the default value.

If `hibernate` is specified instead of a time-out value,
the process goes into hibernation when waiting for the next message
to arrive (by calling `proc_lib:hibernate/3`).

If the initialization fails, the function returns `{stop, Reason}`,
where `Reason` is any term, or `ignore`.
""".
-callback init(Args) -> Result when
      Args :: term(),
      Result :: {ok, StateName, StateData}
              | {ok, StateName, StateData, Timeout}
              | {ok, StateName, StateData, hibernate}
              | {stop, Reason}
              | ignore,
      StateName :: atom(),
      StateData :: term(),
      Timeout   :: timeout(),
      Reason    :: term().

-doc #{ title => ~"deprecated" }.
-doc """
Handle an asynchronous event.

There is to be one instance of this function
for each possible [*state name*](#state-name).
Whenever a `gen_fsm` process receives an event sent using `send_event/2`,
the instance of this function with the same name as the current
[*state name*](#state-name) `StateName` is called to handle the event.
It is also called if a time-out occurs.

`Event` is either the atom `timeout`, if a time-out has occurred,
or the `Event` argument provided to `send_event/2`.

`StateData` is the [*state data*](#state-data) of the `gen_fsm` process.

If the function returns `{next_state, NextStateName, NewStateData},
{next_state, NextStateName, NewStateData, Timeout},
or {next_state, NextStateName, NewStateData, hibernate},
the `gen_fsm` process continues executing with
the current [*state name*](#state-name) set to `NextStateName`
and with the possibly updated [*state data*](#state-data)
`NewStateData`.  For a description of `Timeout` and `hibernate`,
see [`Module:init/1`](`c:init/1`).

If the function returns `{stop ,Reason, NewStateData},
the `gen_fsm` process calls
[`Module:terminate(Reason, StateName, NewStateData)`](`c:terminate/3`)
and terminates.
""".
-callback 'StateName'(Event, StateData) -> Result when
      Event :: 'timeout' | term(),
      StateData :: term(),
      Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, 'hibernate'}
              | {stop, Reason, NewStateData},
      NextStateName :: atom(),
      NewStateData  :: term(),
      Timeout       :: timeout(),
      Reason        :: term().

-doc #{ title => ~"deprecated" }.
-doc """
Handle a synchronous event.

There is to be one instance of this function
for each possible [*state name*](#state-name).
Whenever a `gen_fsm` process receives an event sent using
[`sync_send_event/2,3`](`sync_send_event/3`),
the instance of this function with the same name
as the current [*state name*](#state-name) `StateName` is called
to handle the event.

`Event` is the `Event` argument provided to
[`sync_send_event/2,3`](`sync_send_event/3`).

`From` is a tuple `{Pid, Tag}` where `Pid` is the `t:pid/0`
of the process that called [`sync_send_event/2,3`](`sync_send_event/3`),
`Tag` is a unique tag.

`StateData` is the [*state data*](#state-data) of the `gen_fsm` process.

- If `{reply, Reply, NextStateName, NewStateData}`,
  `{reply, Reply, NextStateName, NewStateData, Timeout}`,
  or `{reply, Reply, NextStateName, NewStateData, hibernate}` is returned,
  `Reply` is given back to `From` as the return value of
  [`sync_send_event/2,3`](`sync_send_event/3`).
  The `gen_fsm` process then continues executing
  with the current [*state name*](#state-name) set to `NextStateName`
  and with the possibly updated [*state data*](#state-data) `NewStateData`.
  For a description of `Timeout` and `hibernate`,
  see [`Module:init/1`](`c:init/1`).

- If `{next_state, NextStateName, NewStateData}`,
  `{next_state, NextStateName, NewStateData, Timeout}`,
  or `{next_state, NextStateName, NewStateData, hibernate}` is returned,
  the `gen_fsm` process continues executing in `NextStateName`
  with `NewStateData`.  Any reply to `From`
  must be specified explicitly using `reply/2`.

- If the function returns `{stop, Reason, Reply, NewStateData}`,
  `Reply` is given back to `From`.  If the function returns
  {stop, Reason, NewStateData}, any reply to `From` must be specified
  explicitly using `reply/2`.  The `gen_fsm` process then calls
  [`Module:terminate(Reason, StateName, NewStateData)`](`c:terminate/3`)
  and terminates.
""".
-callback 'StateName'(Event, From, StateData) -> Result when
      Event     :: term(),
      From      :: from(),
      StateData :: term(),
      Result :: {reply, Reply, NextStateName, NewStateData}
              | {reply, Reply, NextStateName, NewStateData, Timeout}
              | {reply, Reply, NextStateName, NewStateData, 'hibernate'}
              | {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, 'hibernate'}
              | {stop, Reason, Reply, NewStateData}
              | {stop, Reason, NewStateData},
      Reply         :: term(),
      NextStateName :: atom(),
      NewStateData  :: term(),
      Timeout       :: timeout(),
      Reason        :: 'normal' | term().

-doc #{ title => ~"deprecated" }.
-doc """
Handle an asynchronous event.

Whenever a `gen_fsm` process receives an event sent using
`send_all_state_event/2`, this function is called to handle the event.

`StateName` is the current [*state name*](#state-name)
of the `gen_fsm` process.

For a description of the other arguments and possible return values,
see [`Module:StateName/2`](`c:'StateName'/2`).
""".
-callback handle_event(Event, StateName, StateData) -> Result when
      Event     :: term(),
      StateName :: atom(),
      StateData :: term(),
      Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, 'hibernate'}
              | {stop, Reason, NewStateData},
      NextStateName :: atom(),
      NewStateData  :: term(),
      Timeout       :: timeout(),
      Reason        :: term().

-doc #{ title => ~"deprecated" }.
-doc """
Handle a synchronous event.

Whenever a `gen_fsm` process receives an event sent using
[`sync_send_all_state_event/2,3`](`sync_send_all_state_event/3`),
this function is called to handle the event.

`StateName` is the current [*state name*](#state-name)
of the `gen_fsm` process.

For a description of the other arguments and possible return values,
see [`Module:StateName/3`](`c:'StateName'/3`).
""".
-callback handle_sync_event(Event, From, StateName, StateData) -> Result when
      Event     :: term(),
      From      :: from(),
      StateName :: atom(),
      StateData :: term(),
      Result :: {reply, Reply, NextStateName, NewStateData}
              | {reply, Reply, NextStateName, NewStateData, Timeout}
              | {reply, Reply, NextStateName, NewStateData, 'hibernate'}
              | {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, 'hibernate'}
              | {stop, Reason, Reply, NewStateData}
              | {stop, Reason, NewStateData},
      Reply         :: term(),
      NextStateName :: atom(),
      NewStateData  :: term(),
      Timeout       :: timeout(),
      Reason        :: term().
-doc #{ title => ~"deprecated" }.
-doc """
Handle an incoming message

This function is called by a `gen_fsm` process when it receives
any other message than a synchronous or asynchronous event
(or a system message).

`Info` is the received message.

For a description of the other arguments and possible return values,
see [`Module:StateName/2`](`c:'StateName'/2`).
""".
-callback handle_info(Info, StateName, StateData) -> Result when
      Info      :: term(),
      StateName :: atom(),
      StateData :: term(),
      Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, 'hibernate'}
              | {stop, Reason, NewStateData},
      NextStateName :: atom(),
      NewStateData  :: term(),
      Timeout       :: timeout(),
      Reason        :: normal | term().
-doc #{ title => ~"deprecated" }.
-doc """
Clean up before termination.

This function is called by a `gen_fsm` process
when it is about to terminate.  It is to be the opposite of
[`Module:init/1`](`c:init/1`) and do any necessary cleaning up.
When it returns, the `gen_fsm` process terminates with `Reason`.
The return value is ignored.

`Reason` is a term denoting the stop reason, `StateName` is
the current [*state name*](#state-name),
and `StateData` is the [*state data*](#state-data)
of the `gen_fsm` process.

`Reason` depends on why the `gen_fsm` process is terminating.
If it is because another callback function has returned a stop tuple
`{stop, ...}`, `Reason` has the value specified in that tuple.
If it is because of a failure, `Reason` is the error reason.

If the `gen_fsm` process is part of a supervision tree
and is ordered by its supervisor to terminate, this function
is called with `Reason = shutdown` if the following conditions apply:

- The gen_fsm process has been set to trap exit signals.

- The shutdown strategy as defined in the child specification
  of the supervisor is an integer time-out value, not brutal_kill.

Even if the gen_fsm process is **not** part of a supervision tree,
this function is called if it receives an `'EXIT'` message
from its parent. `Reason` is the same as in the `'EXIT'` message.

Otherwise, the gen_fsm process terminates immediately.

Notice that for any other reason than `normal`, `shutdown`,
or `{shutdown, Term}` the `gen_fsm` process is assumed to terminate
because of an error and an error report is issued
using `error_logger:format/2`.
""".
-callback terminate(Reason, StateName, StateData) -> _ when
      Reason    :: normal | shutdown | {shutdown, term()} | term(),
      StateName :: atom(),
      StateData :: term().

-doc #{ title => ~"deprecated" }.
-doc """
Update the internal [*state data*](#state-data) during upgrade/downgrade.

This function is called by a `gen_fsm` process when it is to update
its internal [*state data*](#state-data)
during a release upgrade/downgrade, that is,
when instruction `{update, Module, Change, ...}`,
where `Change = {advanced, Extra}`, is given in the appup file;
see section Release Handling Instructions in OTP Design Principles.
[OTP Design Principles](`e:system:release_handling.md#instr`).

For an upgrade, `OldVsn` is `Vsn`, and for a downgrade,
`OldVsn` is `{down, Vsn}`. `Vsn` is defined by the vsn attribute(s)
of the old version of the callback module `Module`.  If no such
 attribute is defined, the version is the checksum of the Beam file.

`StateName` is the current [*state name*](#state-name)
 and `StateData` the internal [*state data*](#state-data)
 of the `gen_fsm` process.

`Extra` is passed "as is" from the `{advanced, Extra}` part
 of the update instruction.

The function is to return the new current [*state name*](#state-name)
and updated internal data.
""".
-callback code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, NextStateName, NewStateData} when
      OldVsn        :: Vsn | {'down', Vsn},
      Vsn           :: term(),
      StateName     :: atom(),
      NextStateName :: atom(),
      StateData     :: term(),
      NewStateData  :: term(),
      Extra         :: term().

-doc #{ title => ~"deprecated" }.
-doc """
Optional function for providing a term describing
the current `gen_fsm` process status.

The second argument is `[PDict, StateData]`, that is, a list
with the 2 elements, in that order.

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_fsm` module provides a default implementation
> of this function that returns the callback module
> [*state data*](#state-data).

This function is called by a `gen_fsm` process
in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked to get
  the `gen_fsm` status. `Opt` is set to the atom `normal` for this case.
- The `gen_fsm` process terminates abnormally and logs an error.
  `Opt` is set to the atom terminate for this case.

This function is useful for changing the form and appearance
of the `gen_fsm` status for these cases.  A callback module
wishing to change the [`sys:get_status/1,2`](`sys:get_status/1`)
return value as well as how its status appears in termination error logs,
exports an instance of `c:format_status/2` that returns a term
describing the current status of the `gen_fsm` process.

`PDict` is the current value of the process dictionary
of the `gen_fsm` process.

`StateData` is the internal [*state data*](#state-data)
of the `gen_fsm` process.

The function is to return `Status`, a term that change the details
of the current state and status of the `gen_fsm` process.
There are no restrictions on the form `Status` can take,
but for the [`sys:get_status/1,2`](`sys:get_status/1`) case
(when `Opt` is `normal`), the recommended form for the `Status` value
is `[{data, [{"StateData", Term}]}]`, where `Term` provides
relevant details of the `gen_fsm` [*state data*](#state-data).
Following this recommendation is not required, but it makes
the callback module status consistent with the rest of
the [`sys:get_status/1,2`](`sys:get_status/1`) return value.

One use for this function is to return compact alternative
[*state data*](#state-data) representations to avoid
that large state terms are printed in log files.
""".
-callback format_status(Opt, nonempty_improper_list(PDict, [StateData])) ->
    Status when
      Opt :: 'normal' | 'terminate',
      PDict :: [{Key :: term(), Value :: term()}],
      StateData :: term(),
      Status :: term().

-optional_callbacks(
    ['StateName'/2, 'StateName'/3,
     handle_info/3, terminate/3, code_change/4, format_status/2]).



-doc """
[FSM name](#fsm-name) specification:
`local`, `global`, or `via` registered.

To be used when starting a `gen_fsm`. See `start_link/4`.
""".
-type fsm_name() :: % Duplicate of gen:emgr_name()
        {'local', LocalName :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.

-doc """
[FSM reference](#fsm-ref) `t:pid/0` or registered `t:fsm_name/0`.

To be used in for example `send_event/2` to specify the server.
""".
-type fsm_ref() :: % What gen:call/3,4 and gen:stop/1,3 accepts
        pid()
      | (LocalName :: atom())
      | {Name :: atom(), Node :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.


-doc """
[Start options](#start-options) for the [`start/3,4`](`start/3`),
and [`start_link/3,4`](`start_link/3`) functions.

See `start_link/4`.
""".
-type start_opt() :: % Duplicate of gen:option()
        {'timeout', Time :: timeout()}
      | {'spawn_opt', [proc_lib:start_spawn_option()]}
      | enter_loop_opt().

%%----------------------
-doc """
[Start options](#start-options) for the
[`enter_loop/4,5,6`](`enter_loop/6`), [`start/3,4`](`start/3`),
and [`start_link/3,4`](`start_link/3`) functions.

See `start_link/4`.
""".
-type enter_loop_opt() :: % Some gen:option()s works for enter_loop/*
      {'debug', Dbgs :: [sys:debug_option()]}.



%%% ---------------------------------------------------
%%% Starts a generic state machine.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, term()} | {via, atom(), term()}
%%%    Mod  ::= atom(), callback module implementing the 'real' fsm
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% ---------------------------------------------------
-doc """
Create a standalone `gen_fsm` process, not registered.

Equivalent to [`start(Name, Mod, Args, Options)`](`start/4`)
without registering a `Name`.

For a description of arguments and return values,
see [`start_link/3,4`](`start_link/3`).
""".
-spec start(Module, Args, Options) -> Result when
      Module  :: module(),                    %
      Args    :: term(),
      Options :: [start_opt()],
      Result  :: {ok, Pid} | ignore | {error, Reason},
      Pid     :: pid(),
      Reason  :: term().
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

-doc """
Create a standalone `gen_fsm` process.

The created process is not part of a supervision tree
and thus has no supervisor.

For a description of arguments and return values,
see [`start_link/3,4`](`start_link/4`).
""".
-spec start(FsmName, Module, Args, Options) -> Result when
      FsmName :: fsm_name(),
      Module  :: module(),
      Args    :: term(),
      Options :: [start_opt()],
      Result  :: {ok, Pid} | ignore | {error, Reason},
      Pid     :: pid(),
      Reason  :: {'already_started', Pid} | term().
start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

-doc """
Create a `gen_fsm` process in a supervision tree, not registered.

Equivalent to [`start_link(Name, Mod, Args, Options)`](`start_link/4`)
without registering a `Name`.
""".
-spec start_link(Module, Args, Options) -> Result when
      Module  :: module(),
      Args    :: term(),
      Options :: [start_opt()],
      Result  :: {ok, Pid} | ignore | {error, Reason},
      Pid     :: pid(),
      Reason  :: term().
start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

-doc """
Create a `gen_fsm` process in a supervision tree.

The process is created as part of a supervision tree.  The function
is to be called, directly or indirectly, by the supervisor.
For example, it ensures that the `gen_fsm` process
is linked to the supervisor.

The `gen_fsm` process calls [`Module:init/1`](`c:init/1`) to initialize.
To ensure a synchronized startup procedure,
[`start_link/3,4`](`start_link/4`) does not return
until `Module:init/1` has returned.

[]() {: #fsm-name }

- If **`FsmName = {local, Name}`**, the `gen_fsm` process
  is registered locally as `Name` using `register/2`.

- If **`FsmName = {global, GlobalName}`**, the `gen_fsm` process
  is registered globally as `GlobalName` using `global:register_name/2`.

- If **`FsmName = {via, Module, ViaName}`**,
  the `gen_fsm` process registers with the registry
  represented by `Module`.  The `Module` callback is to export
  the functions `register_name/2`, `unregister_name/1`,
  `whereis_name/1`, and `send/2`, which are to behave like
  the corresponding functions in `m:global`.
  Thus, `{via, global, GlobalName}` is a valid reference.

`Module` is the name of the callback module.

`Args` is any term that is passed as the argument to `Module:init/1`.

[]() {: #start-options }

If option **`{timeout, Time}`** is present, the `gen_fsm` process
is allowed to spend `Time` milliseconds initializing or it terminates
and the start function returns `{error, timeout}`.

If option **`{debug, Dbgs}`** is present, the corresponding `sys` function
is called for each item in `Dbgs`; see [`sys(3)`](`m:sys`).

If option **`{spawn_opt, SOpts}`** is present, `SOpts` is passed
as option list to the `spawn_opt` BIF that is used
to spawn the `gen_fsm` process; see `spawn_opt/2`.

> #### Note {: .info }
> Using spawn option `monitor` is not allowed, it causes
> the function to fail with reason `badarg`.

If the `gen_fsm` process is successfully created and initialized,
the function returns `{ok, Pid}`, where `Pid` is the pid
of the `gen_fsm` process.  If a process with the specified `FsmName`
exists already, the function returns `{error, {already_started, Pid}}`,
where `Pid` is the pid of that process.

If `Module:init/1` fails with `Reason`, the function returns
`{error, Reason}`.  If `Module:init/1` returns `{stop, Reason}`
or `ignore`, the process is terminated and the function returns
`{error, Reason}` or `ignore`, respectively.
""".
-spec start_link(FsmName, Module, Args, Options) -> Result when
      FsmName :: fsm_name(),
      Module  :: module(),
      Args    :: term(),
      Options :: [start_opt()],
      Result  :: {ok, Pid} | ignore | {error, Reason},
      Pid     :: pid(),
      Reason  :: {'already_started', Pid} | term().
start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).

-doc #{ equiv => stop(FsmRef, normal, infinity) }.
-spec stop(FsmRef) -> ok when
      FsmRef :: fsm_ref().
stop(Name) ->
    gen:stop(Name).

-doc """
Synchronously stop a generic FSM.

Orders a generic finite state machine to exit with the specified `Reason`
and waits for it to terminate.  The `gen_fsm` process calls
[`Module:terminate/3`](`c:terminate/3`) before exiting.

The function returns `ok` if the generic finite state machine terminates
with the expected reason.  Any other reason than `normal`, `shutdown`,
or `{shutdown, Term}` causes an error report to be issued using
`error_logger:format/2`.

`Timeout` is an integer greater than zero that specifies
how many milliseconds to wait for the generic FSM to terminate,
or the atom `infinity` to wait indefinitely.
If the generic finite state machine has not terminated
within the specified time, a `timeout` exception is raised.

If the process does not exist, a `noproc` exception is raised.
""".
-spec stop(FsmRef, Reason, Timeout) -> ok when
      FsmRef :: fsm_ref(),
      Reason :: term(),
      Timeout :: timeout().
stop(Name, Reason, Timeout) ->
    gen:stop(Name, Reason, Timeout).

-doc """
Send an event asynchronously to a generic FSM.

Sends `Event` to the `FsmRef` of the `gen_fsm` process
and returns `ok` immediately.  The `gen_fsm` process calls
[`Module:StateName/2`](`c:'StateName'/2`) to handle the event,
where `StateName` is the name of the current state
of the `gen_fsm` process.

[](){: #fsm-ref }
`FsmRef` can be any of the following:

- The `t:pid/0`
- `Name`, if the `gen_fsm` process is locally registered
- `{Name, Node}`, if the `gen_fsm` process is locally registered
  at another node
- `{global, GlobalName}`, if the `gen_fsm` process is globally registered
- `{via, Module, ViaName}`, if the `gen_fsm` process is registered
  through an alternative process registry

`Event` is any term that is passed as one of the arguments
to `Module:StateName/2`.
""".
-spec send_event(FsmRef, Event) -> ok when
      FsmRef :: fsm_ref(),
      Event  :: term().
send_event({global, Name}, Event) ->
    catch global:send(Name, {'$gen_event', Event}),
    ok;
send_event({via, Mod, Name}, Event) ->
    catch Mod:send(Name, {'$gen_event', Event}),
    ok;
send_event(Name, Event) ->
    Name ! {'$gen_event', Event},
    ok.

-doc #{ equiv => sync_send_event(FsmRef, Event, 5000) }.
-spec sync_send_event(FsmRef, Event) -> Reply when
      FsmRef :: fsm_ref(),
      Event  :: term(),
      Reply  :: term().
sync_send_event(Name, Event) ->
    case catch gen:call(Name, '$gen_sync_event', Event) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, sync_send_event, [Name, Event]}})
    end.

-doc """
Send an event synchronously to a generic FSM.

Sends an event to the `FsmRef` of the `gen_fsm` process
and waits until a reply arrives or a time-out occurs.
The `gen_fsm` process calls [`Module:StateName/3`](`c:'StateName'/3`)
to handle the event, where `'StateName'` is the name
of the current state of the `gen_fsm` process.

For a description of `FsmRef` and `Event`, see `send_event/2`.

`Timeout` is an integer greater than zero that specifies
how many milliseconds to wait for a reply, or the atom `infinity`
to wait indefinitely.  If no reply is received within the specified time,
the function call fails.

Return value `Reply` is defined in the return value of
[`Module:StateName/3`](`c:'StateName'/3`)

> #### Note {: .info }
> The ancient behavior of sometimes consuming the server exit message
> if the server died during the call while linked to the client
> was removed in Erlang 5.6/OTP R12B.
""".
-spec sync_send_event(FsmRef, Event, Timeout) -> Reply when
      FsmRef  :: fsm_ref(),
      Event   :: term(),
      Timeout :: timeout(),
      Reply   :: term().
sync_send_event(Name, Event, Timeout) ->
    case catch gen:call(Name, '$gen_sync_event', Event, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, sync_send_event, [Name, Event, Timeout]}})
    end.

-doc """
Send an event asynchronously to a generic FSM.

Sends an event asynchronously to the `FsmRef` of the `gen_fsm` process
and returns `ok` immediately.  The `gen_fsm` process calls
[`Module:handle_event/3`](`c:handle_event/3`) to handle the event.

For a description of the arguments, see `send_event/2`.

The difference between `send_event/2` and `send_all_state_event/2`
is which callback function is used to handle the event.
This function is useful when sending events that are handled
the same way in every state, as only one `handle_event` clause
is needed to handle the event instead of one clause
in each state name function.
""".
-spec send_all_state_event(FsmRef, Event) -> ok when
      FsmRef :: fsm_ref(),
      Event :: term().
send_all_state_event({global, Name}, Event) ->
    catch global:send(Name, {'$gen_all_state_event', Event}),
    ok;
send_all_state_event({via, Mod, Name}, Event) ->
    catch Mod:send(Name, {'$gen_all_state_event', Event}),
    ok;
send_all_state_event(Name, Event) ->
    Name ! {'$gen_all_state_event', Event},
    ok.

-doc #{ equiv => sync_send_all_state_event(FsmRef, Event, 5000) }.
-spec sync_send_all_state_event(FsmRef, Event) -> Reply when
      FsmRef :: fsm_ref(),
      Event  :: term,
      Reply  :: term().
sync_send_all_state_event(Name, Event) ->
    case catch gen:call(Name, '$gen_sync_all_state_event', Event) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, sync_send_all_state_event, [Name, Event]}})
    end.

-doc """
Send an event synchronously to a generic FSM.

Sends an event to the `FsmRef` of the `gen_fsm` process and waits
until a reply arrives or a time-out occurs.  The `gen_fsm` process calls
[`Module:handle_sync_event/4`](`c:handle_sync_event/4`)
to handle the event.

For a description of `FsmRef` and `Event`, see `send_event/2`.
For a description of `Timeout` and `Reply`, see `sync_send_event/3`.

For a discussion about the difference between `sync_send_event`
and `sync_send_all_state_event`, see `send_all_state_event/2`.
""".
-spec sync_send_all_state_event(FsmRef, Event, Timeout) -> Reply when
      FsmRef  :: fsm_ref(),
      Event   :: term(),
      Timeout :: timeout(),
      Reply   :: term().
sync_send_all_state_event(Name, Event, Timeout) ->
    case catch gen:call(Name, '$gen_sync_all_state_event', Event, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, sync_send_all_state_event,
			   [Name, Event, Timeout]}})
    end.

%% Designed to be only callable within one of the callbacks
%% hence using the self() of this instance of the process.
%% This is to ensure that timers don't go astray in global
%% e.g. when straddling a failover, or turn up in a restarted
%% instance of the process.

%% Returns Ref, sends event {timeout,Ref,Msg} after Time
%% to the (then) current state.
-doc """
Send a time-out event internally in a generic FSM.

Sends a time-out event internally in the `gen_fsm process`
that calls this function after `Time` milliseconds.
Returns immediately a reference that can be used to cancel the timer
using `cancel_timer/1`.

The `gen_fsm` process calls [`Module:StateName/2`](`c:'StateName'/2`)
to handle the event, where `'StateName'` is the name
of the current state of the `gen_fsm` process at the time
the time-out message is delivered.

`Msg` is any term that is passed in the time-out message,
`{timeout, Ref, Msg}`, as one of the arguments
to [`Module:StateName/2`](`c:'StateName'/2`).
""".
-spec start_timer(Time, Msg) -> Ref when
      Time :: non_neg_integer(),
      Msg  :: term(),
      Ref  :: reference().
start_timer(Time, Msg) ->
    erlang:start_timer(Time, self(), {'$gen_timer', Msg}).

%% Returns Ref, sends Event after Time to the (then) current state.
-doc """
Send a delayed event internally in a generic FSM.

Sends a delayed event internally in the `gen_fsm` process
that calls this function after `Time` milliseconds.
Returns immediately a reference that can be used to cancel
the delayed send using `cancel_timer/1`.

The `gen_fsm` process calls [`Module:StateName/2`](`c:'StateName'/2`)
to handle the event, where `'StateName'` is the name of
the current state of the `gen_fsm` process at the time
the delayed event is delivered.

`Event` is any term that is passed as one of the arguments
to [`Module:StateName/2`](`c:'StateName'/2`).
""".
-spec send_event_after(Time, Event) -> Ref when
      Time  :: non_neg_integer(),
      Event :: term(),
      Ref   :: reference().
send_event_after(Time, Event) ->
    erlang:start_timer(Time, self(), {'$gen_event', Event}).

%% Returns the remaining time for the timer if Ref referred to
%% an active timer/send_event_after, false otherwise.
-doc """
Cancel an internal timer in a generic FSM.

Cancels an internal timer referred by `Ref` in the `gen_fsm` process
that calls this function.

`Ref` is a reference returned from `send_event_after/2`
or `start_timer/2`.

If the timer has already timed out, but the event not yet been delivered,
it is cancelled as if it had not timed out, so there is no false
timer event after returning from this function.

Returns the remaining time in milliseconds until the timer
would have expired if `Ref` referred to an active timer,
otherwise `false`.
""".
-spec cancel_timer(Ref) -> RemainingTime | 'false' when
      Ref           :: reference(),
      RemainingTime :: non_neg_integer().
cancel_timer(Ref) ->
    case erlang:cancel_timer(Ref) of
	false ->
	    receive {timeout, Ref, _} -> 0
	    after 0 -> false
	    end;
	RemainingTime ->
	    RemainingTime
    end.

%% enter_loop/4,5,6
%% Makes an existing process into a gen_fsm.
%% The calling process will enter the gen_fsm receive loop and become a
%% gen_fsm process.
%% The process *must* have been started using one of the start functions
%% in proc_lib, see proc_lib(3).
%% The user is responsible for any initialization of the process,
%% including registering a name for it.
-doc """
Enter the `gen_fsm` receive loop.

Equivalent to `enter_loop/6` with `Timeout = infinity`
but the started server is not registered as for `start_link/3`.
""".
-spec enter_loop(Module, Options, StateName, StateData) ->
          no_return() when
      Module    :: module(),
      Options   :: [enter_loop_opt()],
      StateName :: atom(),
      StateData :: term().
enter_loop(Mod, Options, StateName, StateData) ->
    enter_loop(Mod, Options, StateName, StateData, self(), infinity).

-doc """
Enter the `gen_fsm` receive loop.

With argument `FsmName` equivalent to `enter_loop/6`
with `Timeout = infinity`.

With argument `Timeout` equivalent to `enter_loop/6`
but the started server is not registered as for `start_link/3`.
""".
-spec enter_loop(Module, Options, StateName, StateData, FsmName) ->
          no_return() when
      Module    :: module(),
      Options   :: [enter_loop_opt()],
      StateName :: atom(),
      StateData :: term(),
      FsmName   :: fsm_name();
                (Module, Options, StateName, StateData, Timeout) ->
          no_return() when
      Module    :: module(),
      Options   :: enter_loop_opt(),
      StateName :: atom(),
      StateData :: term(),
      Timeout   :: timeout().
enter_loop(Mod, Options, StateName, StateData, {Scope,_} = ServerName)
  when Scope == local; Scope == global ->
    enter_loop(Mod, Options, StateName, StateData, ServerName, infinity);
enter_loop(Mod, Options, StateName, StateData, {via,_,_} = ServerName) ->
    enter_loop(Mod, Options, StateName, StateData, ServerName, infinity);
enter_loop(Mod, Options, StateName, StateData, Timeout) ->
    enter_loop(Mod, Options, StateName, StateData, self(), Timeout).

-doc """
Enter the `gen_fsm` receive loop.

Makes an existing process into a `gen_fsm` process.  Does not return,
instead the calling process enters the `gen_fsm` receive loop
and becomes a `gen_fsm` process.  The process must have been started
using one of the start functions in `m:proc_lib`.  The user is responsible
for any initialization of the process, including registering a name for it.

This function is useful when a more complex initialization procedure
is needed than the `gen_fsm` behavior provides.

`Module`, `Options`, and `FsmName` have the same meanings
as when calling [`start[_link]/3,4`](`start_link/4`).
However, the process must have been registered according to
`FsmName` before this function is called.

`StateName`, `StateData`, and `Timeout` have the same meanings
as in the return value of [`Module:init/1`](`c:init/1`).
The callback module `Module` does not need to export
an `c:init/1` function.

The function fails if the calling process was not started
by a `m:proc_lib` start function, or if it is not registered
according to `FsmName`.
""".
-spec enter_loop(Module, Options, StateName, StateData, FsmName, Timeout) ->
          no_return() when
      Module    :: module(),
      Options   :: [enter_loop_opt()],
      StateName :: atom(),
      StateData :: term(),
      FsmName   :: fsm_name() | pid(),
      Timeout   :: timeout().
enter_loop(Mod, Options, StateName, StateData, ServerName, Timeout) ->
    Name = gen:get_proc_name(ServerName),
    Parent = gen:get_parent(),
    Debug = gen:debug_options(Name, Options),
	HibernateAfterTimeout = gen:hibernate_after(Options),
    loop(Parent, Name, StateName, StateData, Mod, Timeout, HibernateAfterTimeout, Debug).

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
    Debug = gen:debug_options(Name, Options),
    HibernateAfterTimeout = gen:hibernate_after(Options),
    case catch Mod:init(Args) of
	{ok, StateName, StateData} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(Parent, Name, StateName, StateData, Mod, infinity, HibernateAfterTimeout, Debug);
	{ok, StateName, StateData, Timeout} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(Parent, Name, StateName, StateData, Mod, Timeout, HibernateAfterTimeout, Debug);
	{stop, Reason} ->
            gen:unregister_name(Name0),
            exit(Reason);
	ignore ->
	    gen:unregister_name(Name0),
	    proc_lib:init_fail(Starter, ignore, {exit, normal});
	{'EXIT', Reason} ->
	    gen:unregister_name(Name0),
            exit(Reason);
	Else ->
	    Reason = {bad_return_value, Else},
            exit(Reason)
    end.

%%-----------------------------------------------------------------
%% The MAIN loop
%%-----------------------------------------------------------------
loop(Parent, Name, StateName, StateData, Mod, hibernate, HibernateAfterTimeout, Debug) ->
    proc_lib:hibernate(?MODULE,wake_hib,
		       [Parent, Name, StateName, StateData, Mod, HibernateAfterTimeout,
			Debug]);

loop(Parent, Name, StateName, StateData, Mod, infinity, HibernateAfterTimeout, Debug) ->
	receive
		Msg ->
			decode_msg(Msg,Parent, Name, StateName, StateData, Mod, infinity, HibernateAfterTimeout, Debug, false)
	after HibernateAfterTimeout ->
		loop(Parent, Name, StateName, StateData, Mod, hibernate, HibernateAfterTimeout, Debug)
	end;

loop(Parent, Name, StateName, StateData, Mod, Time, HibernateAfterTimeout, Debug) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  {'$gen_event', timeout}
	  end,
    decode_msg(Msg,Parent, Name, StateName, StateData, Mod, Time, HibernateAfterTimeout, Debug, false).

-doc false.
wake_hib(Parent, Name, StateName, StateData, Mod, HibernateAfterTimeout, Debug) ->
    Msg = receive
	      Input ->
		  Input
	  end,
    decode_msg(Msg, Parent, Name, StateName, StateData, Mod, hibernate, HibernateAfterTimeout, Debug, true).

decode_msg(Msg,Parent, Name, StateName, StateData, Mod, Time, HibernateAfterTimeout, Debug, Hib) ->
    case Msg of
        {system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, StateName, StateData, Mod, Time, HibernateAfterTimeout], Hib);
	{'EXIT', Parent, Reason} ->
	    terminate(
              Reason, Name, undefined, Msg, Mod, StateName, StateData, Debug);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, Name, StateName, StateData, Mod, Time, HibernateAfterTimeout);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {in, Msg, StateName}),
	    handle_msg(Msg, Parent, Name, StateName, StateData,
		       Mod, Time, HibernateAfterTimeout, Debug1)
    end.

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
-doc false.
system_continue(Parent, Debug, [Name, StateName, StateData, Mod, Time, HibernateAfterTimeout]) ->
    loop(Parent, Name, StateName, StateData, Mod, Time, HibernateAfterTimeout, Debug).

-doc false.
-spec system_terminate(term(), _, _, [term(),...]) -> no_return().

system_terminate(Reason, _Parent, Debug,
		 [Name, StateName, StateData, Mod, _Time, _HibernateAfterTimeout]) ->
    terminate(Reason, Name, undefined, [], Mod, StateName, StateData, Debug).

-doc false.
system_code_change([Name, StateName, StateData, Mod, Time, HibernateAfterTimeout],
		   _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, StateName, StateData, Extra) of
	{ok, NewStateName, NewStateData} ->
	    {ok, [Name, NewStateName, NewStateData, Mod, Time, HibernateAfterTimeout]};
	Else -> Else
    end.

-doc false.
system_get_state([_Name, StateName, StateData, _Mod, _Time, _HibernateAfterTimeout]) ->
    {ok, {StateName, StateData}}.

-doc false.
system_replace_state(StateFun, [Name, StateName, StateData, Mod, Time, HibernateAfterTimeout]) ->
    Result = {NStateName, NStateData} = StateFun({StateName, StateData}),
    {ok, Result, [Name, NStateName, NStateData, Mod, Time, HibernateAfterTimeout]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg, StateName}, Name) ->
    case Msg of
	{'$gen_event', Event} ->
	    io:format(Dev, "*DBG* ~tp got event ~tp in state ~tw~n",
		      [Name, Event, StateName]);
	{'$gen_all_state_event', Event} ->
	    io:format(Dev,
		      "*DBG* ~tp got all_state_event ~tp in state ~tw~n",
		      [Name, Event, StateName]);
	{'$gen_sync_event', {From,_Tag}, Event} ->
	    io:format(Dev,
                      "*DBG* ~tp got sync_event ~tp "
                      "from ~tw in state ~tw~n",
		      [Name, Event, From, StateName]);
	{'$gen_sync_all_state_event', {From,_Tag}, Event} ->
	    io:format(Dev,
		      "*DBG* ~tp got sync_all_state_event ~tp "
                      "from ~tw in state ~tw~n",
		      [Name, Event, From, StateName]);
	{timeout, Ref, {'$gen_timer', Message}} ->
	    io:format(Dev,
		      "*DBG* ~tp got timer ~tp in state ~tw~n",
		      [Name, {timeout, Ref, Message}, StateName]);
	{timeout, _Ref, {'$gen_event', Event}} ->
	    io:format(Dev,
		      "*DBG* ~tp got timer ~tp in state ~tw~n",
		      [Name, Event, StateName]);
	_ ->
	    io:format(Dev, "*DBG* ~tp got ~tp in state ~tw~n",
		      [Name, Msg, StateName])
    end;
print_event(Dev, {out, Msg, {To,_Tag}, StateName}, Name) ->
    io:format(Dev, "*DBG* ~tp sent ~tp to ~tw~n"
	           "      and switched to state ~tw~n",
	      [Name, Msg, To, StateName]);
print_event(Dev, {noreply, StateName}, Name) ->
    io:format(Dev, "*DBG* ~tp switched to state ~tw~n",
	      [Name, StateName]).

handle_msg(Msg, Parent, Name, StateName, StateData, Mod, _Time, HibernateAfterTimeout) -> %No debug here
    From = from(Msg),
    case catch dispatch(Msg, Mod, StateName, StateData) of
	{next_state, NStateName, NStateData} ->
	    loop(Parent, Name, NStateName, NStateData, Mod, infinity, HibernateAfterTimeout, []);
	{next_state, NStateName, NStateData, Time1} ->
	    loop(Parent, Name, NStateName, NStateData, Mod, Time1, HibernateAfterTimeout, []);
        {reply, Reply, NStateName, NStateData} when From =/= undefined ->
	    reply(From, Reply),
	    loop(Parent, Name, NStateName, NStateData, Mod, infinity, HibernateAfterTimeout, []);
        {reply, Reply, NStateName, NStateData, Time1} when From =/= undefined ->
	    reply(From, Reply),
	    loop(Parent, Name, NStateName, NStateData, Mod, Time1, HibernateAfterTimeout, []);
	{stop, Reason, NStateData} ->
	    terminate(Reason, Name, From, Msg, Mod, StateName, NStateData, []);
	{stop, Reason, Reply, NStateData} when From =/= undefined ->
	    {'EXIT', R} = (catch terminate(Reason, Name, From, Msg, Mod,
					   StateName, NStateData, [])),
	    reply(From, Reply),
	    exit(R);
        {'EXIT', {undef, [{Mod, handle_info, [_,_,_], _}|_]}} ->
            ?LOG_WARNING(#{label=>{gen_fsm,no_handle_info},
                           module=>Mod,
                           message=>Msg},
                         #{domain=>[otp],
                           report_cb=>fun gen_fsm:format_log/2,
                         error_logger=>
                             #{tag=>warning_msg,
                               report_cb=>fun gen_fsm:format_log/1}}),
            loop(Parent, Name, StateName, StateData, Mod, infinity, HibernateAfterTimeout, []);
	{'EXIT', What} ->
	    terminate(What, Name, From, Msg, Mod, StateName, StateData, []);
	Reply ->
	    terminate({bad_return_value, Reply},
		      Name, From, Msg, Mod, StateName, StateData, [])
    end.

handle_msg(Msg, Parent, Name, StateName, StateData, Mod, _Time, HibernateAfterTimeout, Debug) ->
    From = from(Msg),
    case catch dispatch(Msg, Mod, StateName, StateData) of
	{next_state, NStateName, NStateData} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {noreply, NStateName}),
	    loop(Parent, Name, NStateName, NStateData, Mod, infinity, HibernateAfterTimeout, Debug1);
	{next_state, NStateName, NStateData, Time1} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {noreply, NStateName}),
	    loop(Parent, Name, NStateName, NStateData, Mod, Time1, HibernateAfterTimeout, Debug1);
        {reply, Reply, NStateName, NStateData} when From =/= undefined ->
	    Debug1 = reply(Name, From, Reply, Debug, NStateName),
	    loop(Parent, Name, NStateName, NStateData, Mod, infinity, HibernateAfterTimeout, Debug1);
        {reply, Reply, NStateName, NStateData, Time1} when From =/= undefined ->
	    Debug1 = reply(Name, From, Reply, Debug, NStateName),
	    loop(Parent, Name, NStateName, NStateData, Mod, Time1, HibernateAfterTimeout, Debug1);
	{stop, Reason, NStateData} ->
	    terminate(
              Reason, Name, From, Msg, Mod, StateName, NStateData, Debug);
	{stop, Reason, Reply, NStateData} when From =/= undefined ->
	    {'EXIT', R} = (catch terminate(Reason, Name, From, Msg, Mod,
					   StateName, NStateData, Debug)),
	    _ = reply(Name, From, Reply, Debug, StateName),
	    exit(R);
	{'EXIT', What} ->
	    terminate(What, Name, From, Msg, Mod, StateName, StateData, Debug);
	Reply ->
	    terminate({bad_return_value, Reply},
		      Name, From, Msg, Mod, StateName, StateData, Debug)
    end.

dispatch({'$gen_event', Event}, Mod, StateName, StateData) ->
    Mod:StateName(Event, StateData);
dispatch({'$gen_all_state_event', Event}, Mod, StateName, StateData) ->
    Mod:handle_event(Event, StateName, StateData);
dispatch({'$gen_sync_event', From, Event}, Mod, StateName, StateData) ->
    Mod:StateName(Event, From, StateData);
dispatch({'$gen_sync_all_state_event', From, Event},
	 Mod, StateName, StateData) ->
    Mod:handle_sync_event(Event, From, StateName, StateData);
dispatch({timeout, Ref, {'$gen_timer', Msg}}, Mod, StateName, StateData) ->
    Mod:StateName({timeout, Ref, Msg}, StateData);
dispatch({timeout, _Ref, {'$gen_event', Event}}, Mod, StateName, StateData) ->
    Mod:StateName(Event, StateData);
dispatch(Info, Mod, StateName, StateData) ->
    Mod:handle_info(Info, StateName, StateData).

from({'$gen_sync_event', From, _Event}) -> From;
from({'$gen_sync_all_state_event', From, _Event}) -> From;
from(_) -> undefined.

%% Send a reply to the client.
-doc """
Send a reply to a caller.

This function can be used by a `gen_fsm` process to explicitly send
a reply to a client process that called
[`sync_send_event/2,3`](`sync_send_event/3`) or
[`sync_send_all_state_event/2,3`](`sync_send_all_state_event/3`)
when the reply cannot be defined in the return value of
[`Module:StateName/3`](`c:'StateName'/3`) or
[`Module:handle_sync_event/4`](`c:handle_sync_event/4`).

`Caller` must be the `From` argument provided to the callback function.
`Reply` is any term given back to the client as the return value of
[`sync_send_event/2,3`](`sync_send_event/3`) or
[`sync_send_all_state_event/2,3`](`sync_send_all_state_event/3`).

Return value `Result` is not further defined, and is always to be ignored.
""".
-spec reply(Caller, Reply) -> Result when
      Caller :: from(),
      Reply  :: term(),
      Result :: term().
reply(From, Reply) ->
    gen:reply(From, Reply).

reply(Name, From, Reply, Debug, StateName) ->
    reply(From, Reply),
    sys:handle_debug(Debug, fun print_event/3, Name,
		     {out, Reply, From, StateName}).

%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

-spec terminate(term(), _, _, _, atom(), _, _, _) -> no_return().

terminate(Reason, Name, From, Msg, Mod, StateName, StateData, Debug) ->
    case erlang:function_exported(Mod, terminate, 3) of
	true ->
	    case catch Mod:terminate(Reason, StateName, StateData) of
		{'EXIT', R} ->
		    FmtStateData = format_status(terminate, Mod, get(), StateData),
		    error_info(
                      R, Name, From, Msg, StateName, FmtStateData, Debug),
		    exit(R);
		_ ->
		    ok
	    end;
	false ->
	    ok
    end,
    case Reason of
	normal ->
	    exit(normal);
	shutdown ->
	    exit(shutdown);
 	{shutdown,_}=Shutdown ->
 	    exit(Shutdown);
	_ ->
	    FmtStateData1 = format_status(terminate, Mod, get(), StateData),
	    error_info(
              Reason, Name, From, Msg, StateName, FmtStateData1, Debug),
	    exit(Reason)
    end.

error_info(Reason, Name, From, Msg, StateName, StateData, Debug) ->
    Log = sys:get_log(Debug),
    ?LOG_ERROR(#{label=>{gen_fsm,terminate},
                 name=>Name,
                 last_message=>Msg,
                 state_name=>StateName,
                 state_data=>StateData,
                 log=>Log,
                 reason=>Reason,
                 client_info=>client_stacktrace(From),
                 process_label=>proc_lib:get_label(self())},
               #{domain=>[otp],
                 report_cb=>fun gen_fsm:format_log/2,
                 error_logger=>#{tag=>error,
                                 report_cb=>fun gen_fsm:format_log/1}}),
    ok.

client_stacktrace(undefined) ->
    undefined;
client_stacktrace({Pid,_Tag}) ->
    client_stacktrace(Pid);
client_stacktrace(Pid) when is_pid(Pid), node(Pid) =:= node() ->
    case process_info(Pid, [current_stacktrace, registered_name]) of
        undefined ->
            {Pid,dead};
        [{current_stacktrace, Stacktrace}, {registered_name, []}]  ->
            {Pid,{Pid,Stacktrace}};
        [{current_stacktrace, Stacktrace}, {registered_name, Name}]  ->
            {Pid,{Name,Stacktrace}}
    end;
client_stacktrace(Pid) when is_pid(Pid) ->
    {Pid,remote}.


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
limit_report(#{label:={gen_fsm,terminate},
               last_message:=Msg,
               state_data:=StateData,
               log:=Log,
               reason:=Reason,
               client_info:=ClientInfo,
               process_label:=ProcessLabel}=Report,
            Depth) ->
    Report#{last_message=>io_lib:limit_term(Msg, Depth),
            state_data=>io_lib:limit_term(StateData, Depth),
            log=>[io_lib:limit_term(L, Depth) || L <- Log],
            reason=>io_lib:limit_term(Reason, Depth),
            client_info=>limit_client_report(ClientInfo, Depth),
            process_label=>io_lib:limit_term(ProcessLabel, Depth)};
limit_report(#{label:={gen_fsm,no_handle_info},
               message:=Msg}=Report, Depth) ->
    Report#{message=>io_lib:limit_term(Msg, Depth)}.

limit_client_report({From,{Name,Stacktrace}}, Depth) ->
    {From,{Name,io_lib:limit_term(Stacktrace, Depth)}};
limit_client_report(Client, _) ->
    Client.

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

format_log_single(#{label:={gen_fsm,terminate},
                    name:=Name,
                    last_message:=Msg,
                    state_name:=StateName,
                    state_data:=StateData,
                    log:=Log,
                    reason:=Reason,
                    client_info:=ClientInfo,
                    process_label:=ProcessLabel},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    FixedReason = fix_reason(Reason),
    {ClientFmt,ClientArgs} = format_client_log_single(ClientInfo, P, Depth),
    Format =
        lists:append(
          ["State machine ",P," terminating",
           case ProcessLabel of
               undefined -> "";
               _ -> ". Label: "++P
           end,
           ". Reason: ",P,
           ". Last event: ",P,
           ". State: ",P,
           ". Data: ",P,
           case Log of
               [] -> "";
               _ -> ". Log: "++P
           end,
          "."]),
    Args0 =
        [Name] ++
        case ProcessLabel of
            undefined -> [];
            _ -> [ProcessLabel]
        end ++
        [FixedReason,get_msg(Msg),StateName,StateData] ++
        case Log of
            [] -> [];
            _ -> [Log]
        end,
    Args = case Depth of
               unlimited ->
                   Args0;
               _ ->
                   lists:flatmap(fun(A) -> [A, Depth] end, Args0)
           end,
    {Format++ClientFmt, Args++ClientArgs};
format_log_single(#{label:={gen_fsm,no_handle_info},
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
format_log_single(Report, FormatOpts) ->
    format_log_multi(Report, FormatOpts).

format_log_multi(#{label:={gen_fsm,terminate},
                   name:=Name,
                   last_message:=Msg,
                   state_name:=StateName,
                   state_data:=StateData,
                   log:=Log,
                   reason:=Reason,
                   client_info:=ClientInfo,
                   process_label:=ProcessLabel},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    FixedReason = fix_reason(Reason),
    {ClientFmt,ClientArgs} = format_client_log(ClientInfo, P, Depth),
    Format =
        lists:append(
          ["** State machine ",P," terminating \n",
           case ProcessLabel of
               undefined -> [];
               _ -> "** Process label == "++P++"~n"
           end,
           get_msg_str(Msg, P)++
           "** When State == ",P,"~n",
           "**      Data  == ",P,"~n",
           "** Reason for termination ==~n** ",P,"~n",
           case Log of
               [] -> [];
               _ -> "** Log ==~n**"++P++"~n"
           end]),
    Args0 =
        [Name|
         case ProcessLabel of
             undefined -> [];
             _ -> [ProcessLabel]
         end] ++
        get_msg(Msg) ++
        [StateName,StateData,FixedReason |
         case Log of
             [] -> [];
             _ -> [Log]
         end],
    Args = case Depth of
               unlimited ->
                   Args0;
               _ ->
                   lists:flatmap(fun(A) -> [A, Depth] end, Args0)
           end,
    {Format++ClientFmt,Args++ClientArgs};
format_log_multi(#{label:={gen_fsm,no_handle_info},
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

get_msg_str({'$gen_event', _Event}, P) ->
    "** Last event in was "++P++"~n";
get_msg_str({'$gen_sync_event', _From, _Event}, P) ->
    "** Last sync event in was "++P++" from ~tw~n";
get_msg_str({'$gen_all_state_event', _Event}, P) ->
    "** Last event in was "++P++" (for all states)~n";
get_msg_str({'$gen_sync_all_state_event', _From, _Event}, P) ->
    "** Last sync event in was "++P++" (for all states) from "++P++"~n";
get_msg_str({timeout, _Ref, {'$gen_timer', _Msg}}, P) ->
    "** Last timer event in was "++P++"~n";
get_msg_str({timeout, _Ref, {'$gen_event', _Msg}}, P) ->
    "** Last timer event in was "++P++"~n";
get_msg_str(_Msg, P) ->
    "** Last message in was "++P++"~n".

get_msg({'$gen_event', Event}) -> [Event];
get_msg({'$gen_sync_event', {From,_Tag}, Event}) -> [Event,From];
get_msg({'$gen_all_state_event', Event}) -> [Event];
get_msg({'$gen_sync_all_state_event', {From,_Tag}, Event}) -> [Event,From];
get_msg({timeout, Ref, {'$gen_timer', Msg}}) -> [{timeout, Ref, Msg}];
get_msg({timeout, _Ref, {'$gen_event', Event}}) -> [Event];
get_msg(Msg) -> [Msg].

format_client_log_single(undefined, _, _) ->
    {"", []};
format_client_log_single({Pid,dead}, _, _) ->
    {" Client ~0p is dead.", [Pid]};
format_client_log_single({Pid,remote}, _, _) ->
    {" Client ~0p is remote on node ~0p.", [Pid,node(Pid)]};
format_client_log_single({_Pid,{Name,Stacktrace0}}, P, Depth) ->
    %% Minimize the stacktrace a bit for single line reports. This is
    %% hopefully enough to point out the position.
    Stacktrace = lists:sublist(Stacktrace0, 4),
    Format = lists:append([" Client ",P," stacktrace: ",P,"."]),
    Args = case Depth of
               unlimited ->
                   [Name, Stacktrace];
               _ ->
                   [Name, Depth, Stacktrace, Depth]
           end,
    {Format, Args}.

format_client_log(undefined, _, _) ->
    {"", []};
format_client_log({Pid,dead}, _, _) ->
    {"** Client ~p is dead~n", [Pid]};
format_client_log({Pid,remote}, _, _) ->
    {"** Client ~p is remote on node ~p~n", [Pid,node(Pid)]};
format_client_log({_Pid,{Name,Stacktrace}}, P, Depth) ->
    Format = lists:append(["** Client ",P," stacktrace~n** ",P,"~n"]),
    Args = case Depth of
               unlimited ->
                   [Name, Stacktrace];
               _ ->
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
    [PDict, SysState, Parent, Debug, [Name, StateName, StateData, Mod, _Time, _HibernateAfterTimeout]] =
	StatusData,
    Header = gen:format_status_header("Status for state machine",
                                      Name),
    Log = sys:get_log(Debug),
    Specific =
        case format_status(Opt, Mod, PDict, StateData) of
            S when is_list(S) -> S;
            S -> [S]
        end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", Log},
	     {"StateName", StateName}]} |
     Specific].

format_status(Opt, Mod, PDict, State) ->
    DefStatus = case Opt of
		    terminate -> State;
		    _ -> [{data, [{"StateData", State}]}]
		end,
    case erlang:function_exported(Mod, format_status, 2) of
	true ->
	    case catch Mod:format_status(Opt, [PDict, State]) of
		{'EXIT', _} -> DefStatus;
		Else -> Else
	    end;
	_ ->
	    DefStatus
    end.
