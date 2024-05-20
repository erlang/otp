%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2024. All Rights Reserved.
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
-module(gen_statem).
-moduledoc """
Generic state machine behavior.

`gen_statem` provides a generic state machine behaviour that for new code
replaces its predecessor `m:gen_fsm` since Erlang/OTP 20.0. The `gen_fsm`
behaviour remains in OTP "as is".

> #### Note {: .info }
>
> If you are new to `gen_statem` and want an overview of concepts and operation
> the section [`gen_statem` Behaviour ](`e:system:statem.md`)located in the
> User's Guide [OTP Design Principles ](`e:system:index.html`)is recommended to
> read before this reference manual, possibly after the Description section you
> are reading here.

This reference manual contains type descriptions generated from types in the
`gen_statem` source code, so they are correct. However, the generated
descriptions also reflect the type hierarchy, which sometimes makes it hard to
get a good overview. If so, see the section
[`gen_statem` Behaviour ](`e:system:statem.md`)in the
[OTP Design Principles ](`e:system:index.html`)User's Guide.

> #### Note {: .info }
>
> - This behavior appeared in Erlang/OTP 19.0.
> - In OTP 19.1 a backwards incompatible change of the return tuple from
>   [`Module:init/1`](`c:init/1`) was made and the mandatory callback function
>   [`Module:callback_mode/0` ](`c:callback_mode/0`)was introduced.
> - In OTP 20.0 [generic time-outs ](`t:generic_timeout/0`)were added.
> - In OTP 22.1 time-out content [`update` ](`t:timeout_update_action/0`)and
>   explicit time-out [`cancel` ](`t:timeout_cancel_action/0`)were added.
> - In OTP 22.3 the possibility to change the callback module with actions
>   [`change_callback_module`](`t:action/0`),
>   [`push_callback_module`](`t:action/0`) and
>   [`pop_callback_module`](`t:action/0`), was added.

`gen_statem` has got the same features that `m:gen_fsm` had and adds some really
useful:

- Co-located state code
- Arbitrary term state
- Event postponing
- Self-generated events
- State time-out
- Multiple generic named time-outs
- Absolute time-out time
- Automatic state enter calls
- Reply from other state than the request, `m:sys` traceable
- Multiple `m:sys` traceable replies
- Changing the callback module

Two [_callback modes_](`t:callback_mode/0`) are supported:

- One for finite-state machines (`m:gen_fsm` like), which requires the state to
  be an atom and uses that state as the name of the current callback function.
- One that allows the state to be any term and that uses one callback function
  for all states.

The callback model(s) for `gen_statem` differs from the one for `m:gen_fsm`, but
it is still fairly easy to
[rewrite from ](`m:gen_fsm#module-migration-to-gen_statem`)`gen_fsm` to `gen_statem`.

A generic state machine server process (`gen_statem`) implemented using this
module has a standard set of interface functions and includes functionality for
tracing and error reporting. It also fits into an OTP supervision tree. For more
information, see [OTP Design Principles](`e:system:statem.md`).

A `gen_statem` assumes all specific parts to be located in a callback module
exporting a predefined set of functions. The relationship between the behavior
functions and the callback functions is as follows:

```erlang
gen_statem module            Callback module
-----------------            ---------------
gen_statem:start
gen_statem:start_monitor
gen_statem:start_link -----> Module:init/1

Server start or code change
                      -----> Module:callback_mode/0

gen_statem:stop       -----> Module:terminate/3

gen_statem:call
gen_statem:cast
gen_statem:send_request
erlang:send
erlang:'!'            -----> Module:StateName/3
                             Module:handle_event/4

-                     -----> Module:terminate/3

-                     -----> Module:code_change/4
```

Events are of different [types](`t:event_type/0`), so the callback functions can
know the origin of an event and how to respond.

If a callback function fails or returns a bad value, the `gen_statem`
terminates, unless otherwise stated. However, an exception of class
[`throw`](`erlang:throw/1`) is not regarded as an error but as a valid return
from all callback functions.

[](){: #state-callback }

The _state callback_ for a specific [state](`t:state/0`) in a `gen_statem` is
the callback function that is called for all events in this state. It is
selected depending on which [_callback mode_](`t:callback_mode/0`) that the
callback module defines with the callback function
[`Module:callback_mode/0`](`c:callback_mode/0`).

When the [_callback mode_](`t:callback_mode/0`) is `state_functions`, the state
must be an atom and is used as the _state callback_ name; see
[`Module:StateName/3`](`c:'StateName'/3`). This co-locates all code for a specific
state in one function as the `gen_statem` engine branches depending on state
name. Note the fact that the callback function
[`Module:terminate/3`](`c:terminate/3`) makes the state name `terminate`
unusable in this mode.

When the [_callback mode_](`t:callback_mode/0`) is `handle_event_function`, the
state can be any term and the _state callback_ name is
[`Module:handle_event/4`](`c:handle_event/4`). This makes it easy to branch
depending on state or event as you desire. Be careful about which events you
handle in which states so that you do not accidentally postpone an event forever
creating an infinite busy loop.

When `gen_statem` receives a process message it is converted into an event and
the [_state callback_](`m:gen_statem#state-callback`) is called with the event
as two arguments: type and content. When the
[_state callback_](`m:gen_statem#state-callback`) has processed the event it
returns to `gen_statem` which does a _state transition_. If this _state
transition_ is to a different state, that is: `NextState =/= State`, it is a
_state change_.

The [_state callback_](`m:gen_statem#state-callback`) may return
[_transition actions_](`t:action/0`) for `gen_statem` to execute during the
_state transition_, for example to reply to a [`gen_statem:call/2,3`](`call/2`).

One of the possible _transition actions_ is to postpone the current event. Then
it is not retried in the current state. The `gen_statem` engine keeps a queue of
events divided into the postponed events and the events still to process. After
a _state change_ the queue restarts with the postponed events.

The `gen_statem` event queue model is sufficient to emulate the normal process
message queue with selective receive. Postponing an event corresponds to not
matching it in a receive statement, and changing states corresponds to entering
a new receive statement.

The [_state callback_](`m:gen_statem#state-callback`) can insert events using
the [_transition actions_](`t:action/0`) `next_event` and such an event is
inserted in the event queue as the next to call the
[_state callback_](`m:gen_statem#state-callback`) with. That is, as if it is the
oldest incoming event. A dedicated `t:event_type/0` `internal` can be used for
such events making them impossible to mistake for external events.

Inserting an event replaces the trick of calling your own state handling
functions that you often would have to resort to in, for example, `m:gen_fsm` to
force processing an inserted event before others.

The `gen_statem` engine can automatically make a specialized call to the
[_state callback_](`m:gen_statem#state-callback`) whenever a new state is
entered; see `t:state_enter/0`. This is for writing code common to all state
entries. Another way to do it is to explicitly insert an event at the _state
transition_, and/or to use a dedicated _state transition_ function, but that is
something you will have to remember at every _state transition_ to the state(s)
that need it.

> #### Note {: .info }
>
> If you in `gen_statem`, for example, postpone an event in one state and then
> call another _state callback_ of yours, you have not done a _state change_ and
> hence the postponed event is not retried, which is logical but can be
> confusing.

For the details of a _state transition_, see type `t:transition_option/0`.

A `gen_statem` handles system messages as described in `m:sys`. The `m:sys` module
can be used for debugging a `gen_statem`.

Notice that a `gen_statem` does not trap exit signals automatically, this must
be explicitly initiated in the callback module (by calling
[`process_flag(trap_exit, true)`](`erlang:process_flag/2`).

Unless otherwise stated, all functions in this module fail if the specified
`gen_statem` does not exist or if bad arguments are specified.

The `gen_statem` process can go into hibernation; see `proc_lib:hibernate/3`. It
is done when a [_state callback_](`m:gen_statem#state-callback`) or
[`Module:init/1`](`c:init/1`) specifies `hibernate` in the returned
[`Actions`](`t:action/0`) list. This feature can be useful to reclaim process
heap memory while the server is expected to be idle for a long time. However,
use this feature with care, as hibernation can be too costly to use after every
event; see `erlang:hibernate/3`.

There is also a server start option
[`{hibernate_after, Timeout}` ](`t:enter_loop_opt/0`)for
[`start/3,4`](`start/3`), [`start_monitor/3,4`](`start_monitor/3`),
[`start_link/3,4`](`start_link/3`) or [`enter_loop/4,5,6`](`enter_loop/4`), that
may be used to automatically hibernate the server.

If the `gen_statem` process terminates, e.g. as a result of a function in the
callback module returning `{stop,Reason}`, an exit signal with this `Reason` is
sent to linked processes and ports. See
[Processes](`e:system:ref_man_processes.md#errors`) in the Reference Manual for
details regarding error handling using exit signals.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_. Blocking
> signaling can, for example, cause call timeouts in `gen_statem` to be
> significantly delayed.

## Example

The following example shows a simple pushbutton model for a toggling pushbutton
implemented with [_callback mode_](`t:callback_mode/0`) `state_functions`. You
can push the button and it replies if it went on or off, and you can ask for a
count of how many times it has been pushed to switch on.

The following is the complete callback module file `pushbutton.erl`:

```erlang
-module(pushbutton).
-behaviour(gen_statem).

-export([start/0,push/0,get_count/0,stop/0]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([on/3,off/3]).

name() -> pushbutton_statem. % The registered server name

%% API.  This example uses a registered name name()
%% and does not link to the caller.
start() ->
    gen_statem:start({local,name()}, ?MODULE, [], []).
push() ->
    gen_statem:call(name(), push).
get_count() ->
    gen_statem:call(name(), get_count).
stop() ->
    gen_statem:stop(name()).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
init([]) ->
    %% Set the initial state + data.  Data is used only as a counter.
    State = off, Data = 0,
    {ok,State,Data}.
callback_mode() -> state_functions.

%%% state callback(s)

off({call,From}, push, Data) ->
    %% Go to 'on', increment count and reply
    %% that the resulting status is 'on'
    {next_state,on,Data+1,[{reply,From,on}]};
off(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

on({call,From}, push, Data) ->
    %% Go to 'off' and reply that the resulting status is 'off'
    {next_state,off,Data,[{reply,From,off}]};
on(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.
```

The following is a shell session when running it:

```erlang
1> pushbutton:start().
{ok,<0.36.0>}
2> pushbutton:get_count().
0
3> pushbutton:push().
on
4> pushbutton:get_count().
1
5> pushbutton:push().
off
6> pushbutton:get_count().
1
7> pushbutton:stop().
ok
8> pushbutton:push().
** exception exit: {noproc,{gen_statem,call,[pushbutton_statem,push,infinity]}}
     in function  gen:do_for_proc/2 (gen.erl, line 261)
     in call from gen_statem:call/3 (gen_statem.erl, line 386)
```

To compare styles, here follows the same example using
[_callback mode_](`t:callback_mode/0`) `handle_event_function`, or rather the
code to replace after function [`init/1`](`c:init/1`) of the `pushbutton.erl`
example file above:

```erlang
callback_mode() -> handle_event_function.

%%% state callback(s)

handle_event({call,From}, push, off, Data) ->
    %% Go to 'on', increment count and reply
    %% that the resulting status is 'on'
    {next_state,on,Data+1,[{reply,From,on}]};
handle_event({call,From}, push, on, Data) ->
    %% Go to 'off' and reply that the resulting status is 'off'
    {next_state,off,Data,[{reply,From,off}]};
%%
%% Event handling common to all states
handle_event({call,From}, get_count, State, Data) ->
    %% Reply with the current count
    {next_state,State,Data,[{reply,From,Data}]};
handle_event(_, _, State, Data) ->
    %% Ignore all other events
    {next_state,State,Data}.
```

## See Also

`m:gen_event`, `m:gen_fsm`, `m:gen_server`, `m:proc_lib`, `m:supervisor`,
`m:sys`.
""".
-moduledoc(#{since => "OTP 19.0"}).

-include("logger.hrl").

%%%
%%% NOTE: If init_ack() return values are modified, see comment
%%%       above monitor_return() in gen.erl!
%%%

%% API
-export(
   [start/3,start/4,start_link/3,start_link/4,
    start_monitor/3,start_monitor/4,
    stop/1,stop/3,
    cast/2,call/2,call/3,
    send_request/2, send_request/4,
    wait_response/1, wait_response/2, wait_response/3,
    receive_response/1, receive_response/2, receive_response/3,
    check_response/2, check_response/3,
    reqids_new/0, reqids_size/1,
    reqids_add/3, reqids_to_list/1,
    enter_loop/4,enter_loop/5,enter_loop/6,
    reply/1,reply/2]).

%% gen callbacks
-export(
   [init_it/6]).

%% sys callbacks
-export(
   [system_continue/3,
    system_terminate/4,
    system_code_change/4,
    system_get_state/1,
    system_replace_state/2,
    format_status/2]).

-behaviour(sys).

%% Internal callbacks
-export(
   [wakeup_from_hibernate/3]).

%% logger callback
-export([format_log/1, format_log/2]).

%% Type exports for templates and callback modules
-export_type(
   [event_type/0,
    from/0,
    reply_tag/0,
    callback_mode_result/0,
    init_result/1,
    init_result/2,
    state_enter_result/1,
    state_enter_result/2,
    event_handler_result/1,
    event_handler_result/2,
    reply_action/0,
    enter_action/0,
    action/0,
    request_id/0,
    request_id_collection/0,
    format_status/0
   ]).
%% Old types, not advertised
-export_type(
   [state_function_result/0,
    handle_event_result/0]).

%% Type that is exported just to be documented
-export_type([transition_option/0]).

%% Type exports for start_link & friends
-export_type(
   [server_name/0,
    server_ref/0,
    start_opt/0,
    enter_loop_opt/0,
    start_ret/0,
    start_mon_ret/0]).

%% -define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).


%%%==========================================================================
%%% Interface functions.
%%%==========================================================================

-doc """
Destination to use when replying through, for example, the `t:action/0`
`{reply,From,Reply}` to a process that has called the `gen_statem` server using
`call/2`.
""".
-type from() ::
	{To :: pid(), Tag :: reply_tag()}. % Reply-to specifier for call
-doc "A handle that associates a reply to the corresponding request.".
-opaque reply_tag() :: gen:reply_tag().

-doc """
If the [_callback mode_](`t:callback_mode/0`) is `handle_event_function`, the
state can be any term. After a _state change_ (`NextState =/= State`), all
postponed events are retried.
""".
-type state() ::
	state_name() | % For StateName/3 callback functions
	term(). % For handle_event/4 callback function

-doc """
If the [_callback mode_](`t:callback_mode/0`) is `state_functions`, the state
must be an atom. After a _state change_ (`NextState =/= State`), all postponed
events are retried. Note that the state `terminate` is not possible to use since
it would collide with the optional callback function
[`Module:terminate/3`](`c:terminate/3`).
""".
-type state_name() :: atom().

-doc """
A term in which the state machine implementation is to store any server data it
needs. The difference between this and the `t:state/0` itself is that a change
in this data does not cause postponed events to be retried. Hence, if a change
in this data would change the set of events that are handled, then that data
item is to be made a part of the state.
""".
-type data() :: term().

-doc """
There are 3 categories of events: [external](`t:external_event_type/0`),
[timeout](`t:timeout_event_type/0`), and `internal`.

`internal` events can only be generated by the state machine itself through the
_transition action_ [`next_event`](`t:action/0`).
""".
-type event_type() ::
        external_event_type() | timeout_event_type() | 'internal'.
-doc """
External events are of 3 types: `{call,From}`, `cast`, or `info`. Type `call`
originates from the API functions `call/2` and `send_request/2`. For calls, the
event contains whom to reply to. Type `cast` originates from the API function
`cast/2`. Type `info` originates from regular process messages sent to the
`gen_statem`.
""".
-type external_event_type() ::
        {'call',From :: from()} | 'cast' | 'info'.
-doc """
There are 3 types of time-out events that the state machine can generate for
itself with the corresponding `t:timeout_action/0`s.
""".
-type timeout_event_type() ::
        'timeout' | {'timeout', Name :: term()} | 'state_timeout'.

-doc """
Any event's content can be any term.

See [`event_type`](`t:event_type/0`) that describes the origins of the different
event types, which is also where the event content comes from.
""".
-type event_content() :: term().

-doc """
This is the return type from [`Module:callback_mode/0`](`c:callback_mode/0`) and
selects [_callback mode_](`t:callback_mode/0`) and whether to do
[_state enter calls_](`t:state_enter/0`), or not.
""".
-type callback_mode_result() ::
	callback_mode() | [callback_mode() | state_enter()].
-doc """
The _callback mode_ is selected with the return value from
[`Module:callback_mode/0`](`c:callback_mode/0`):

- **`state_functions`** - The state must be of type `t:state_name/0` and one
  callback function per state, that is, [`Module:StateName/3`](`c:'StateName'/3`),
  is used.

- **`handle_event_function`** - The state can be any term and the callback
  function [`Module:handle_event/4`](`c:handle_event/4`) is used for all states.

The function [`Module:callback_mode/0`](`c:callback_mode/0`) is called when
starting the `gen_statem`, after code change and after changing the callback
module with any of the actions [`change_callback_module`](`t:action/0`),
[`push_callback_module`](`t:action/0`) or [`pop_callback_module`](`t:action/0`).
The result is cached for subsequent calls to
[state callbacks](`m:gen_statem#state-callback`).
""".
-type callback_mode() :: 'state_functions' | 'handle_event_function'.
-doc """
Whether the state machine should use _state enter calls_ or not is selected when
starting the `gen_statem` and after code change using the return value from
[`Module:callback_mode/0`](`c:callback_mode/0`).

If [`Module:callback_mode/0`](`c:callback_mode/0`) returns a list containing
`state_enter`, the `gen_statem` engine will, at every _state change_, call the
[state callback](`m:gen_statem#state-callback`) with arguments
`(enter, OldState, Data)` or `(enter, OldState, State, Data)`, depending on the
[_callback mode_](`t:callback_mode/0`). This may look like an event but is
really a call performed after the previous
[_state callback_](`m:gen_statem#state-callback`) returned and before any event
is delivered to the new [_state callback_](`m:gen_statem#state-callback`). See
[`Module:StateName/3`](`c:'StateName'/3`) and
[`Module:handle_event/4`](`c:handle_event/4`). Such a call can be repeated by
returning a [`repeat_state` ](`t:state_callback_result/1`)or
[`repeat_state_and_data` ](`t:state_callback_result/1`)tuple from the _state
callback_.

If [`Module:callback_mode/0`](`c:callback_mode/0`) does not return such a list,
no _state enter calls_ are done.

If [`Module:code_change/4`](`c:code_change/4`) should transform the state, it is
regarded as a state rename and not a _state change_, which will not cause a
_state enter call_.

Note that a _state enter call_ _will_ be done right before entering the initial
state even though this actually is not a _state change_. In this case
`OldState =:= State`, which cannot happen for a subsequent state change, but
will happen when repeating the _state enter call_.
""".
-type state_enter() :: 'state_enter'.

-doc """
Transition options can be set by [actions](`t:action/0`) and modify the _state
transition_. The _state transition_ takes place when the
[_state callback_](`m:gen_statem#state-callback`) has processed an event and
returns. Here are the sequence of steps for a _state transition_:

1. All returned [actions](`t:action/0`) are processed in order of appearance. In
   this step all replies generated by any `t:reply_action/0` are sent. Other
   actions set `t:transition_option/0`s that come into play in subsequent steps.
1. If [_state enter calls_ ](`t:state_enter/0`)are used, and either it is the
   initial state or one of the callback results
   [`repeat_state_and_data` ](`t:state_callback_result/1`)or
   [`repeat_state_and_data` ](`t:state_callback_result/1`)is used the
   `gen_statem` engine calls the current state callback with arguments
   [`(enter, State, Data)`](`t:state_enter/0`) or
   [`(enter, State, State, Data)`](`t:state_enter/0`) (depending on
   [_callback mode_](`t:callback_mode/0`)) and when it returns starts again from
   the top of this sequence.

If [_state enter calls_ ](`t:state_enter/0`)are used, and the state changes the
`gen_statem` engine calls the new state callback with arguments
[`(enter, OldState, Data)`](`t:state_enter/0`) or
[`(enter, OldState, State, Data)`](`t:state_enter/0`) (depending on
[_callback mode_](`t:callback_mode/0`)) and when it returns starts again from
the top of this sequence.

1. If `t:postpone/0` is `true`, the current event is postponed.
1. If this is a _state change_, the queue of incoming events is reset to start
   with the oldest postponed.
1. All events stored with `t:action/0` `next_event` are inserted to be processed
   before previously queued events.
1. Time-out timers `t:event_timeout/0`, `t:generic_timeout/0` and
   `t:state_timeout/0` are handled. Time-outs with zero time are guaranteed to
   be delivered to the state machine before any external not yet received event
   so if there is such a time-out requested, the corresponding time-out zero
   event is enqueued as the newest received event; that is after already queued
   events such as inserted and postponed events.

Any event cancels an `t:event_timeout/0` so a zero time event time-out is only
generated if the event queue is empty.

A _state change_ cancels a `t:state_timeout/0` and any new transition option of
this type belongs to the new state, that is; a `t:state_timeout/0` applies to
the state the state machine enters.

1. If there are enqueued events the
   [_state callback_](`m:gen_statem#state-callback`) for the possibly new state
   is called with the oldest enqueued event, and we start again from the top of
   this sequence.
1. Otherwise the `gen_statem` goes into `receive` or hibernation (if
   `t:hibernate/0` is `true`) to wait for the next message. In hibernation the
   next non-system event awakens the `gen_statem`, or rather the next incoming
   message awakens the `gen_statem`, but if it is a system event it goes right
   back into hibernation. When a new message arrives the
   [_state callback_](`m:gen_statem#state-callback`) is called with the
   corresponding event, and we start again from the top of this sequence.
""".
-type transition_option() ::
	postpone() | hibernate() |
	event_timeout() | generic_timeout() | state_timeout().
-doc """
If `true`, postpones the current event and retries it after a _state change_
(`NextState =/= State`).
""".
-type postpone() ::
	%% If 'true' postpone the current event
	%% and retry it when the state changes (=/=)
	boolean().
-doc """
If `true`, hibernates the `gen_statem` by calling `proc_lib:hibernate/3` before
going into `receive` to wait for a new external event.

> #### Note {: .info }
>
> If there are enqueued events to process when hibernation is requested, this is
> optimized by not hibernating but instead calling
> [`erlang:garbage_collect/0` ](`erlang:garbage_collect/0`)to simulate that the
> `gen_statem` entered hibernation and immediately got awakened by an enqueued
> event.
""".
-type hibernate() ::
	%% If 'true' hibernate the server instead of going into receive
	boolean().
-doc """
Starts a timer set by `t:enter_action/0` `timeout`. When the timer expires an
event of `t:event_type/0` `timeout` will be generated. See
`erlang:start_timer/4` for how `Time` and [`Options`](`t:timeout_option/0`) are
interpreted. Future `erlang:start_timer/4` `Options` will not necessarily be
supported.

Any event that arrives cancels this time-out. Note that a retried or inserted
event counts as arrived. So does a state time-out zero event, if it was
generated before this time-out is requested.

If `Time` is `infinity`, no timer is started, as it never would expire anyway.

If `Time` is relative and `0` no timer is actually started, instead the the
time-out event is enqueued to ensure that it gets processed before any not yet
received external event, but after already queued events.

Note that it is not possible nor needed to cancel this time-out, as it is
cancelled automatically by any other event.
""".
-type event_timeout() ::
	%% Generate a ('timeout', EventContent, ...) event
	%% unless some other event is delivered
	Time :: timeout() | integer().
-doc """
Starts a timer set by `t:enter_action/0` `{timeout,Name}`. When the timer
expires an event of `t:event_type/0` `{timeout,Name}` will be generated. See
`erlang:start_timer/4` for how `Time` and [`Options`](`t:timeout_option/0`) are
interpreted. Future `erlang:start_timer/4` `Options` will not necessarily be
supported.

If `Time` is `infinity`, no timer is started, as it never would expire anyway.

If `Time` is relative and `0` no timer is actually started, instead the the
time-out event is enqueued to ensure that it gets processed before any not yet
received external event.

Setting a timer with the same `Name` while it is running will restart it with
the new time-out value. Therefore it is possible to cancel a specific time-out
by setting it to `infinity`.
""".
-type generic_timeout() ::
	%% Generate a ({'timeout',Name}, EventContent, ...) event
	Time :: timeout() | integer().
-doc """
Starts a timer set by `t:enter_action/0` `state_timeout`. When the timer expires
an event of `t:event_type/0` `state_timeout` will be generated. See
`erlang:start_timer/4` for how `Time` and [`Options`](`t:timeout_option/0`) are
interpreted. Future `erlang:start_timer/4` `Options` will not necessarily be
supported.

If `Time` is `infinity`, no timer is started, as it never would expire anyway.

If `Time` is relative and `0` no timer is actually started, instead the the
time-out event is enqueued to ensure that it gets processed before any not yet
received external event.

Setting this timer while it is running will restart it with the new time-out
value. Therefore it is possible to cancel this time-out by setting it to
`infinity`.
""".
-type state_timeout() ::
	%% Generate a ('state_timeout', EventContent, ...) event
	%% unless the state is changed
	Time :: timeout() | integer().
-doc """
If `Abs` is `true` an absolute timer is started, and if it is `false` a
relative, which is the default. See
[`erlang:start_timer/4` ](`erlang:start_timer/4`)for details.
""".
-type timeout_option() :: {abs,Abs :: boolean()}.

-doc """
These _transition actions_ can be invoked by returning them from the
[_state callback_](`m:gen_statem#state-callback`) when it is called with an
[event](`t:event_type/0`), from [`Module:init/1`](`c:init/1`) or by giving them
to [`enter_loop/5,6`](`enter_loop/5`).

Actions are executed in the containing list order.

Actions that set [transition options ](`t:transition_option/0`)override any
previous of the same type, so the last in the containing list wins. For example,
the last `t:postpone/0` overrides any previous `t:postpone/0` in the list.

- **`postpone`** - Sets the
  [`transition_option()` ](`t:transition_option/0`)`t:postpone/0` for this
  _state transition_. This action is ignored when returned from
  [`Module:init/1`](`c:init/1`) or given to [`enter_loop/5,6`](`enter_loop/5`),
  as there is no event to postpone in those cases.

- **`next_event`** - This action does not set any
  [`transition_option()` ](`t:transition_option/0`)but instead stores the
  specified `EventType` and `EventContent` for insertion after all actions have
  been executed.

  The stored events are inserted in the queue as the next to process before any
  already queued events. The order of these stored events is preserved, so the
  first `next_event` in the containing list becomes the first to process.

  An event of type [`internal`](`t:event_type/0`) is to be used when you want to
  reliably distinguish an event inserted this way from any external event.

- **`change_callback_module`** - Changes the callback module to `NewModule`
  which will be used when calling all subsequent
  [state callbacks](`m:gen_statem#state-callback`).

  The `gen_statem` engine will find out the
  [_callback mode_ ](`t:callback_mode/0`)of `NewModule` by calling
  [`NewModule:callback_mode/0` ](`c:callback_mode/0`)before the next
  [state callback](`m:gen_statem#state-callback`).

  Changing the callback module does not affect the _state transition_ in any
  way, it only changes which module that handles the events. Be aware that all
  relevant callback functions in `NewModule` such as the
  [state callback](`m:gen_statem#state-callback`),
  [`NewModule:code_change/4`](`c:code_change/4`),
  [`NewModule:format_status/1` ](`c:format_status/1`)and
  [`NewModule:terminate/3` ](`c:terminate/3`)must be able to handle the state
  and data from the old module.

- **`push_callback_module`** - Pushes the current callback module to the top of
  an internal stack of callback modules and changes the callback module to
  `NewModule`. Otherwise like `{change_callback_module, NewModule}` above.

- **`pop_callback_module`** - Pops the top module from the internal stack of
  callback modules and changes the callback module to be the popped module. If
  the stack is empty the server fails. Otherwise like
  `{change_callback_module, NewModule}` above.
""".
-type action() ::
	%% During a state change:
	%% * NextState and NewData are set.
	%% * All action()s are executed in order of apperance.
	%% * Postponing the current event is performed
	%%   iff 'postpone' is 'true'.
	%% * A state timeout is started iff 'timeout' is set.
	%% * Pending events are handled or if there are
	%%   no pending events the server goes into receive
	%%   or hibernate (iff 'hibernate' is 'true')
	%%
	%% These action()s are executed in order of appearence
	%% in the containing list. The ones that set options
	%% will override any previous so the last of each kind wins.
	%%
	'postpone' |  % Set the postpone option
	{'postpone', Postpone :: postpone()} |
	%%
	%% All 'next_event' events are kept in a list and then
	%% inserted at state changes so the first in the
	%% action() list is the first to be delivered.
	{'next_event', % Insert event as the next to handle
	 EventType :: event_type(),
	 EventContent :: event_content()} |
        {'change_callback_module', NewModule :: module()} |
        {'push_callback_module', NewModule :: module()} |
        'pop_callback_module' |
	enter_action().
-doc """
These _transition actions_ can be invoked by returning them from the
[_state callback_](`m:gen_statem#state-callback`), from
[`Module:init/1`](`c:init/1`) or by giving them to
[`enter_loop/5,6`](`enter_loop/5`).

Actions are executed in the containing list order.

Actions that set [transition options](`t:transition_option/0`) override any
previous of the same type, so the last in the containing list wins. For example,
the last `t:event_timeout/0` overrides any previous `t:event_timeout/0` in the
list.

- **`hibernate`** - Sets the `t:transition_option/0` `t:hibernate/0` for this
  _state transition_.
""".
-type enter_action() ::
	'hibernate' | % Set the hibernate option
	{'hibernate', Hibernate :: hibernate()} |
        timeout_action() |
	reply_action().
-doc """
These _transition actions_ can be invoked by returning them from the
[_state callback_](`m:gen_statem#state-callback`), from
[`Module:init/1`](`c:init/1`) or by giving them to
[`enter_loop/5,6`](`enter_loop/5`).

These time-out actions sets time-out
[transition options](`t:transition_option/0`).

- **`Time`** - Short for `{timeout,Time,Time}`, that is, the time-out message is
  the time-out time. This form exists to make the
  [_state callback_](`m:gen_statem#state-callback`) return value
  `{next_state,NextState,NewData,Time}` allowed like for `gen_fsm`.

- **`timeout`** - Sets the `t:transition_option/0` `t:event_timeout/0` to `Time`
  with `EventContent` and time-out options [`Options`](`t:timeout_option/0`).

- **`{timeout,Name}`** - Sets the `t:transition_option/0` `t:generic_timeout/0`
  to `Time` for `Name` with `EventContent` and time-out options
  [`Options`](`t:timeout_option/0`).

- **`state_timeout`** - Sets the `t:transition_option/0` `t:state_timeout/0` to
  `Time` with `EventContent` and time-out options
  [`Options`](`t:timeout_option/0`).
""".
-type timeout_action() ::
	(Time :: event_timeout()) | % {timeout,Time,Time}
	{'timeout', % Set the event_timeout option
	 Time :: event_timeout(), EventContent :: event_content()} |
	{'timeout', % Set the event_timeout option
	 Time :: event_timeout(),
	 EventContent :: event_content(),
	 Options :: (timeout_option() | [timeout_option()])} |
	%%
	{{'timeout', Name :: term()}, % Set the generic_timeout option
	 Time :: generic_timeout(), EventContent :: event_content()} |
	{{'timeout', Name :: term()}, % Set the generic_timeout option
	 Time :: generic_timeout(),
	 EventContent :: event_content(),
	 Options :: (timeout_option() | [timeout_option()])} |
	%%
	{'state_timeout', % Set the state_timeout option
	 Time :: state_timeout(), EventContent :: event_content()} |
	{'state_timeout', % Set the state_timeout option
	 Time :: state_timeout(),
	 EventContent :: event_content(),
	 Options :: (timeout_option() | [timeout_option()])} |
        timeout_cancel_action() |
        timeout_update_action().
-doc """
This is a shorter and clearer form of
[timeout_action() ](`t:timeout_action/0`)with `Time = infinity` which cancels a
time-out.
""".
-type timeout_cancel_action() ::
        {'timeout', 'cancel'} |
        {{'timeout', Name :: term()}, 'cancel'} |
        {'state_timeout', 'cancel'}.
-doc """
Updates a time-out with a new `EventContent`. See
[timeout_action() ](`t:timeout_action/0`)for how to start a time-out.

If no time-out of the same type is active instead insert the time-out event just
like when starting a time-out with relative `Time = 0`.
""".
-type timeout_update_action() ::
        {'timeout', 'update', EventContent :: event_content()} |
        {{'timeout', Name :: term()},
         'update', EventContent :: event_content()} |
        {'state_timeout', 'update', EventContent :: event_content()}.
-doc """
This _transition action_ can be invoked by returning it from the
[_state callback_](`m:gen_statem#state-callback`), from
[`Module:init/1`](`c:init/1`) or by giving it to
[`enter_loop/5,6`](`enter_loop/5`).

It does not set any [`transition_option()` ](`t:transition_option/0`)but instead
replies to a caller waiting for a reply in `call/2`. `From` must be the term
from argument [`{call,From}`](`t:event_type/0`) in a call to a
[_state callback_](`m:gen_statem#state-callback`).

Note that using this action from [`Module:init/1`](`c:init/1`) or
[`enter_loop/5,6`](`enter_loop/5`) would be weird on the border of witchcraft
since there has been no earlier call to a
[_state callback_](`m:gen_statem#state-callback`) in this server.
""".
-type reply_action() ::
	{'reply', % Reply to a caller
	 From :: from(), Reply :: term()}.

-type init_result(StateType) :: init_result(StateType, term()).
-doc """
For a succesful initialization, `State` is the initial `t:state/0` and `Data`
the initial server `t:data/0` of the `gen_statem`.

The [`Actions`](`t:action/0`) are executed when entering the first
[state](`t:state/0`) just as for a
[_state callback_](`m:gen_statem#state-callback`), except that the action
`postpone` is forced to `false` since there is no event to postpone.

For an unsuccesful initialization, `{stop,Reason}`, `{error,Reason}` or `ignore`
should be used; see [`start_link/3,4`](`start_link/3`).

`{error,Reason}` was introduced in OTP 26.0.
""".
-type init_result(StateType, DataType) ::
    {ok, State :: StateType, Data :: DataType} |
    {ok, State :: StateType, Data :: DataType,
     Actions :: [action()] | action()} |
        'ignore' |
        {'stop', Reason :: term()} |
        {'error', Reason :: term()}.

%% Old, not advertised
-type state_function_result() ::
	event_handler_result(state_name()).
-type handle_event_result() ::
	event_handler_result(state()).
%%
-type state_enter_result(State) :: state_enter_result(State, term()).
-doc """
`State` is the current state and it cannot be changed since the state callback
was called with a [_state enter call_](`t:state_enter/0`).

- **`next_state`** - The `gen_statem` does a state transition to `State`, which
  has to be the current state, sets `NewData`, and executes all `Actions`.
""".
-type state_enter_result(State, DataType) ::
	{'next_state', % {next_state,State,NewData,[]}
	 State,
	 NewData :: DataType} |
	{'next_state', % State entry for state State
	 State,
	 NewData :: DataType,
	 Actions :: [enter_action()] | enter_action()} |
	state_callback_result(enter_action()).
-type event_handler_result(StateType) ::
    event_handler_result(StateType, term()).
-doc """
`StateType` is `t:state_name/0` if [_callback mode_](`t:callback_mode/0`) is
`state_functions`, or `t:state/0` if [_callback mode_](`t:callback_mode/0`) is
`handle_event_function`.

- **`next_state`** - The `gen_statem` does a _state transition_ to `NextState`
  (which can be the same as the current state), sets `NewData`, and executes all
  `Actions`. If `NextState =/= CurrentState` the _state transition_ is a _state
  change_.
""".
-type event_handler_result(StateType, DataType) ::
	{'next_state', % {next_state,NextState,NewData,[]}
	 NextState :: StateType,
	 NewData :: DataType} |
	{'next_state', % State transition, maybe to the same state
	 NextState :: StateType,
	 NewData :: DataType,
	 Actions :: [action()] | action()} |
	state_callback_result(action()).
-type state_callback_result(ActionType) ::
    state_callback_result(ActionType, term()).
-doc """
`ActionType` is `t:enter_action/0` if the state callback was called with a
[_state enter call_](`t:state_enter/0`) and `t:action/0` if the state callback
was called with an event.

- **`keep_state`** - The same as `{next_state,CurrentState,NewData,Actions}`.

- **`keep_state_and_data`** - The same as `{keep_state,CurrentData,Actions}`.

- **`repeat_state`** - If the `gen_statem` runs with
  [_state enter calls_](`t:state_enter/0`), the _state enter call_ is repeated,
  see type `t:transition_option/0`, other than that `repeat_state` is the same
  as `keep_state`.

- **`repeat_state_and_data`** - The same as
  `{repeat_state,CurrentData,Actions}`.

- **`stop`** - Terminates the `gen_statem` by calling
  [`Module:terminate/3`](`c:terminate/3`) with `Reason` and `NewData`, if
  specified. An exit signal with this reason is sent to linked processes and
  ports. The default `Reason` is `normal`.

- **`stop_and_reply`** - Sends all `Replies`, then terminates the `gen_statem`
  by calling [`Module:terminate/3`](`c:terminate/3`) with `Reason` and
  `NewData`, if specified. An exit signal with this reason is sent to linked
  processes and ports.

All these terms are tuples or atoms and this property will hold in any future
version of `gen_statem`.
""".
-type state_callback_result(ActionType, DataType) ::
	{'keep_state', % {keep_state,NewData,[]}
	 NewData :: DataType} |
	{'keep_state', % Keep state, change data
	 NewData :: DataType,
	 Actions :: [ActionType] | ActionType} |
	'keep_state_and_data' | % {keep_state_and_data,[]}
	{'keep_state_and_data', % Keep state and data -> only actions
	 Actions :: [ActionType] | ActionType} |
	%%
	{'repeat_state', % {repeat_state,NewData,[]}
	 NewData :: DataType} |
	{'repeat_state', % Repeat state, change data
	 NewData :: DataType,
	 Actions :: [ActionType] | ActionType} |
	'repeat_state_and_data' | % {repeat_state_and_data,[]}
	{'repeat_state_and_data', % Repeat state and data -> only actions
	 Actions :: [ActionType] | ActionType} |
	%%
	'stop' | % {stop,normal}
	{'stop', % Stop the server
	 Reason :: term()} |
	{'stop', % Stop the server
	 Reason :: term(),
	 NewData :: DataType} |
	%%
	{'stop_and_reply', % Reply then stop the server
	 Reason :: term(),
	 Replies :: [reply_action()] | reply_action()} |
	{'stop_and_reply', % Reply then stop the server
	 Reason :: term(),
	 Replies :: [reply_action()] | reply_action(),
	 NewData :: DataType}.

-doc "An opaque request identifier. See `send_request/2` for details.".
-opaque request_id() :: gen:request_id().

-doc """
An opaque collection of request identifiers (`t:request_id/0`) where each
request identifier can be associated with a label chosen by the user. For more
information see `reqids_new/0`.
""".
-opaque request_id_collection() :: gen:request_id_collection().

-doc """
Used to set a time limit on how long to wait for a response using either
`receive_response/2`, `receive_response/3`, `wait_response/2`, or
`wait_response/3`. The time unit used is `millisecond`. Currently valid values:

- **`0..4294967295`** - Timeout relative to current time in milliseconds.

- **`infinity`** - Infinite timeout. That is, the operation will never time out.

- **`{abs, Timeout}`** - An absolute
  [Erlang monotonic time](`erlang:monotonic_time/1`) timeout in milliseconds.
  That is, the operation will time out when
  [`erlang:monotonic_time(millisecond)`](`erlang:monotonic_time/1`) returns a
  value larger than or equal to `Timeout`. `Timeout` is not allowed to identify
  a time further into the future than `4294967295` milliseconds. Identifying the
  timeout using an absolute timeout value is especially handy when you have a
  deadline for responses corresponding to a complete collection of requests
  (`t:request_id_collection/0`) , since you do not have to recalculate the
  relative time until the deadline over and over again.
""".
-type response_timeout() ::
        timeout() | {abs, integer()}.

%% The state machine init function.  It is called only once and
%% the server is not running until this function has returned
%% an {ok, ...} tuple.  Thereafter the state callbacks are called
%% for all events to this server.
-doc """
Whenever a `gen_statem` is started using [`start_link/3,4`](`start_link/3`),
[`start_monitor/3,4`](`start_monitor/3`), or [`start/3,4`](`start/3`), this
function is called by the new process to initialize the implementation state and
server data.

`Args` is the `Args` argument provided to that start function.

> #### Note {: .info }
>
> Note that if the `gen_statem` is started through `m:proc_lib` and
> [`enter_loop/4-6`](`enter_loop/4`), this callback will never be called. Since
> this callback is not optional it can in that case be implemented as:
>
> ```erlang
> -spec init(_) -> no_return().
> init(Args) -> erlang:error(not_implemented, [Args]).
> ```
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback init(Args :: term()) -> init_result(state()).

%% This callback shall return the callback mode of the callback module.
%%
%% It is called once after init/0 and code_change/4 but before
%% the first state callback StateName/3 or handle_event/4.
-doc """
This function is called by a `gen_statem` when it needs to find out the
[_callback mode_](`t:callback_mode/0`) of the callback module.

The value is cached by `gen_statem` for efficiency reasons, so this function is only called
once after server start, after code change, and after changing the callback
module, but before the first [_state callback_](`m:gen_statem#state-callback`)
in the current callback module's code version is called. More occasions may be
added in future versions of `gen_statem`.

Server start happens either when [`Module:init/1`](`c:init/1`) returns or when
[`enter_loop/4-6`](`enter_loop/4`) is called. Code change happens when
[`Module:code_change/4`](`c:code_change/4`) returns. A change of the callback
module happens when a [_state callback_](`m:gen_statem#state-callback`) returns
any of the actions [`change_callback_module`](`t:action/0`),
[`push_callback_module`](`t:action/0`) or [`pop_callback_module`](`t:action/0`).

The `CallbackMode` is either just `t:callback_mode/0` or a list containing
`t:callback_mode/0` and possibly the atom [`state_enter`](`t:state_enter/0`).

> #### Note {: .info }
>
> If this function's body does not return an inline constant value the callback
> module is doing something strange.
""".
-doc(#{since => <<"OTP 19.1">>}).
-callback callback_mode() -> callback_mode_result().

%% Example state callback for StateName = 'state_name'
%% when callback_mode() =:= state_functions.
%%
%% In this mode all states has to be of type state_name() i.e atom().
%%
%% Note that the only callbacks that have arity 3 are these
%% StateName/3 callbacks and terminate/3, so the state name
%% 'terminate' is unusable in this mode.
-doc(#{equiv => handle_event/4, since => <<"OTP 19.0">>}).
-callback 'StateName'(
	    'enter',
	    OldStateName :: state_name(),
	    data()) ->
    state_enter_result('state_name');
           (event_type(),
	    event_content(),
	    data()) ->
    event_handler_result(state_name()).
%%
%% State callback for all states
%% when callback_mode() =:= handle_event_function.
-doc """
Whenever a `gen_statem` receives an event from `call/2`, `cast/2`, or as a
normal process message, one of these functions is called. If
[_callback mode_](`t:callback_mode/0`) is `state_functions`,
`Module:StateName/3` is called, and if it is `handle_event_function`,
[`Module:handle_event/4`](`c:handle_event/4`) is called.

If `EventType` is [`{call,From}`](`t:event_type/0`), the caller waits for a
reply. The reply can be sent from this or from any other
[_state callback_](`m:gen_statem#state-callback`) by returning with
`{reply,From,Reply}` in [`Actions`](`t:action/0`), in
[`Replies`](`t:reply_action/0`), or by calling
[`reply(From, Reply)`](`reply/2`).

If this function returns with a next state that does not match equal (`=/=`) to
the current state, all postponed events are retried in the next state.

The only difference between `StateFunctionResult` and `HandleEventResult` is
that for `StateFunctionResult` the next state must be an atom, but for
`HandleEventResult` there is no restriction on the next state.

For options that can be set and actions that can be done by `gen_statem` after
returning from this function, see `t:action/0`.

When the `gen_statem` runs with [_state enter calls_](`t:state_enter/0`), these
functions are also called with arguments `(enter, OldState, ...)` during every
_state change_. In this case there are some restrictions on the
[actions](`t:enter_action/0`) that may be returned: `t:postpone/0` is not
allowed since a _state enter call_ is not an event so there is no event to
postpone, and [`{next_event,_,_}`](`t:action/0`) is not allowed since using
_state enter calls_ should not affect how events are consumed and produced. You
may also not change states from this call. Should you return
`{next_state,NextState, ...}` with `NextState =/= State` the `gen_statem`
crashes. Note that it is actually allowed to use `{repeat_state, NewData, ...}`
although it makes little sense since you immediately will be called again with a
new _state enter call_ making this just a weird way of looping, and there are
better ways to loop in Erlang. If you do not update `NewData` and have some loop
termination condition, or if you use `{repeat_state_and_data, _}` or
`repeat_state_and_data` you have an infinite loop\! You are advised to use
`{keep_state,...}`, `{keep_state_and_data,_}` or `keep_state_and_data` since
changing states from a _state enter call_ is not possible anyway.

Note the fact that you can use [`throw`](`erlang:throw/1`) to return the result,
which can be useful. For example to bail out with
[`throw(keep_state_and_data)`](`throw/1`) from deep within complex code that
cannot return `{next_state,State,Data}` because `State` or `Data` is no longer
in scope.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback handle_event(
	    'enter',
	    OldState :: state(),
	    CurrentState,
	    data()) ->
    state_enter_result(CurrentState);
           (event_type(),
	    event_content(),
	    CurrentState :: state(),
	    data()) ->
    event_handler_result(state()). % New state
%% The following fun() should have the same type as the previous callback,
%% but ...
%% the type language cannot express a fun() with multiple clauses
%% so we have to specify the union fun() here.  Furthermore this
%% type is only used for record field #params.callback_mode
%% so the type checker can verify that we use correct arguments
%% (could, but all end up in term() so that will be in vain),
%% but the return value comes from some Module:handle_event/4
%% function so we cannot assume it is correct, and a type checker
%% cannot make the connection between such an external function
%% and this type anyway...
-type handle_event_fun() ::
        fun (('enter'                  | event_type(),
              (OldState :: state())    | event_content(),
              CurrentState :: state(),
              data()) -> term()).

%% Clean up before the server terminates.
-doc """
This function is called by a `gen_statem` when it is about to terminate. It is
to be the opposite of [`Module:init/1`](`c:init/1`) and do any necessary
cleaning up. When it returns, the `gen_statem` terminates with `Reason`. The
return value is ignored.

`Reason` is a term denoting the stop reason and [`State`](`t:state/0`) is the
internal state of the `gen_statem`.

`Reason` depends on why the `gen_statem` is terminating. If it is because
another callback function has returned, a stop tuple `{stop,Reason}` in
[`Actions`](`t:action/0`), `Reason` has the value specified in that tuple. If it
is because of a failure, `Reason` is the error reason.

If the `gen_statem` is part of a supervision tree and is ordered by its
supervisor to terminate, this function is called with `Reason = shutdown` if
both the following conditions apply:

- The `gen_statem` has been set to trap exit signals.
- The shutdown strategy as defined in the supervisor's child specification is an
  integer time-out value, not `brutal_kill`.

Even if the `gen_statem` is _not_ part of a supervision tree, this function is
called if it receives an `'EXIT'` message from its parent. `Reason` is the same
as in the `'EXIT'` message.

Otherwise, the `gen_statem` is immediately terminated.

Notice that for any other reason than `normal`, `shutdown`, or
`{shutdown,Term}`, the `gen_statem` is assumed to terminate because of an error
and an error report is issued using `m:logger`.

When the `gen_statem` process exits, an exit signal with the same reason is sent
to linked processes and ports.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback terminate(
	    Reason :: 'normal' | 'shutdown' | {'shutdown', term()}
		    | term(),
	    CurrentState :: state(),
	    data()) ->
    any().

%% Note that the new code can expect to get an OldState from
%% the old code version not only in code_change/4 but in the first
%% state callback function called thereafter
-doc """
This function is called by a `gen_statem` when it is to update its internal
state during a release upgrade/downgrade, that is, when the instruction
`{update,Module,Change,...}`, where `Change = {advanced,Extra}`, is specified in
the [`appup`](`e:sasl:appup.md`) file. For more information, see
[OTP Design Principles](`e:system:release_handling.md#instr`).

For an upgrade, `OldVsn` is `Vsn`, and for a downgrade, `OldVsn` is
`{down,Vsn}`. `Vsn` is defined by the `vsn` attribute(s) of the old version of
the callback module `Module`. If no such attribute is defined, the version is
the checksum of the Beam file.

`OldState` and `OldData` is the internal state of the `gen_statem`.

`Extra` is passed "as is" from the `{advanced,Extra}` part of the update
instruction.

If successful, the function must return the updated internal state in an
`{ok,NewState,NewData}` tuple.

If the function returns a failure `Reason`, the ongoing upgrade fails and rolls
back to the old release. Note that `Reason` cannot be an `{ok,_,_}` tuple since
that will be regarded as a `{ok,NewState,NewData}` tuple, and that a tuple
matching `{ok,_}` is an also invalid failure `Reason`. It is recommended to use
an atom as `Reason` since it will be wrapped in an `{error,Reason}` tuple.

Also note when upgrading a `gen_statem`, this function and hence the
`Change = {advanced,Extra}` parameter in the [`appup`](`e:sasl:appup.md`) file
is not only needed to update the internal state or to act on the `Extra`
argument. It is also needed if an upgrade or downgrade should change
[_callback mode_](`t:callback_mode/0`), or else the _callback mode_ after the
code change will not be honoured, most probably causing a server crash.

If the server changes callback module using any of the actions
[`change_callback_module`](`t:action/0`), [`push_callback_module`](`t:action/0`)
or [`pop_callback_module`](`t:action/0`), be aware that it is always the current
callback module that will get this callback call. That the current callback
module handles the current state and data update should be no surprise, but it
must be able to handle even parts of the state and data that it is not familiar
with, somehow.

In the supervisor
[child specification](`e:system:sup_princ.md#child-specification`) there is a
list of modules which is recommended to contain only the callback module. For a
`gen_statem` with multiple callback modules there is no real need to list all of
them, it may not even be possible since the list could change after code
upgrade. If this list would contain only the start callback module, as
recommended, what is important is to upgrade _that_ module whenever a
_synchronized code replacement_ is done. Then the release handler concludes that
an upgrade that upgrades _that_ module needs to suspend, code change, and resume
any server whose child specification declares that it is using _that_ module.
And again; the _current_ callback module will get the
[`Module:code_change/4`](`c:code_change/4`) call.

> #### Note {: .info }
>
> If a release upgrade/downgrade with `Change = {advanced,Extra}` specified in the
> `.appup` file is made when [`code_change/4`](`c:code_change/4`) is not
> implemented the process will crash with exit reason `undef`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback code_change(
	    OldVsn :: term() | {'down', term()},
	    OldState :: state(),
	    OldData :: data(),
	    Extra :: term()) ->
    {ok, NewState :: state(), NewData :: data()} |
    (Reason :: term()).

%% Format the callback module state in some sensible that is
%% often condensed way.  For StatusOption =:= 'normal' the preferred
%% return term is [{data,[{"State",FormattedState}]}], and for
%% StatusOption =:= 'terminate' it is just FormattedState.
%%
%% Deprecated
-doc """
This function is called by a `gen_statem` process in in order to format/limit the
server state for debugging and logging purposes.

It is called in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked to get the
  `gen_statem` status. `Opt` is set to the atom `normal` for this case.
- The `gen_statem` terminates abnormally and logs an error. `Opt` is set to the
  atom `terminate` for this case.

This function is useful for changing the form and appearance of the `gen_statem`
status for these cases. A callback module wishing to change the
[`sys:get_status/1,2`](`sys:get_status/1`) return value and how its status
appears in termination error logs exports an instance of
[`format_status/2`](`c:format_status/2`), which returns a term describing the
current status of the `gen_statem`.

`PDict` is the current value of the process dictionary of the `gen_statem`.

[`State`](`t:state/0`) is the internal state of the `gen_statem`.

[`Data`](`t:data/0`) is the internal server data of the `gen_statem`.

The function is to return `Status`, a term that contains the appropriate details
of the current state and status of the `gen_statem`. There are no restrictions
on the form `Status` can take, but for the
[`sys:get_status/1,2`](`sys:get_status/1`) case (when `Opt` is `normal`), the
recommended form for the `Status` value is `[{data, [{"State", Term}]}]`, where
`Term` provides relevant details of the `gen_statem` state. Following this
recommendation is not required, but it makes the callback module status
consistent with the rest of the [`sys:get_status/1,2`](`sys:get_status/1`)
return value.

One use for this function is to return compact alternative state representations
to avoid having large state terms printed in log files. Another use is to hide
sensitive data from being written to the error log.

> #### Note {: .info }
>
> This callback is optional, so a callback module does not need to export it.
> The `gen_statem` module provides a default implementation of this function
> that returns `{State,Data}`.
>
> If this callback is exported but fails, to hide possibly sensitive data, the
> default function will instead return `{State,Info}`, where `Info` says nothing
> but the fact that [`format_status/2`](`c:format_status/2`) has crashed.
""".
-deprecated_callback({format_status, 2, "use format_status/1 instead"}).
-doc(#{since => <<"OTP 19.0">>}).
-callback format_status(
	    StatusOption,
	    [ [{Key :: term(), Value :: term()}] |
	      state() |
	      data()]) ->
    Status :: term() when
      StatusOption :: 'normal' | 'terminate'.

-doc """
A map that describes the `gen_statem` status.

The keys are:

- **`state`** - The current state of the `gen_statem` process.

- **`data`** - The state data of the the `gen_statem` process.

- **`reason`** - The reason that caused the state machine to terminate.

- **`queue`** - The event queue of the `gen_statem` process.

- **`postponed`** - The [postponed](`t:postpone/0`) events queue of the
  `gen_statem` process.

- **`timeouts`** - The active [time-outs](`t:timeout_action/0`) of the
  `gen_statem` process.

- **`log`** - The [sys log](`sys:log/2`) of the server.

New associations may be added to the status map without prior notice.
""".
-type format_status() ::
        #{ state => state(),
           data => data(),
           reason => term(),
           queue => [{event_type(), event_content()}],
           postponed => [{event_type(), event_content()}],
           timeouts => [{timeout_event_type(), event_content()}],
           log => [sys:system_event()] }.

%% Format the callback module status in some sensible that is
%% often condensed way.
-doc """
This function is called by a `gen_statem` process in in order to format/limit the
server state for debugging and logging purposes.

It is called in the following situations:

- [`sys:get_status/1,2`](`sys:get_status/1`) is invoked to get the `gen_statem`
  status.
- The `gen_statem` process terminates abnormally and logs an error.

This function is useful for changing the form and appearance of the `gen_statem`
status for these cases. A callback module wishing to change the
[`sys:get_status/1,2`](`sys:get_status/1`) return value and how its status
appears in termination error logs exports an instance of
[`format_status/1`](`c:format_status/1`), which will get a map `Status` that
describes the current states of the `gen_statem`, and shall return a map
`NewStatus` containing the same keys as the input map, but it may transform some
values.

One use case for this function is to return compact alternative state
representations to avoid having large state terms printed in log files. Another
is to hide sensitive data from being written to the error log.

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
> This callback is optional, so a callback module does not need to export it.
> The `gen_statem` module provides a default implementation of this function
> that returns `{State,Data}`.
>
> If this callback is exported but fails, to hide possibly sensitive data, the
> default function will instead return `{State,Info}`, where `Info` says nothing
> but the fact that [`format_status/2`](`c:format_status/2`) has crashed.
""".
-doc(#{since => <<"OTP 25.0">>}).
-callback format_status(Status) -> NewStatus when
      Status    :: format_status(),
      NewStatus :: format_status().

-optional_callbacks(
   [format_status/1, % Has got a default implementation
    format_status/2, % Has got a default implementation
    terminate/3, % Has got a default implementation
    code_change/4, % Only needed by advanced soft upgrade
    %%
    'StateName'/3, % Just an example callback;
    %% for callback_mode() =:= state_functions
    %% there has to be a StateName/3 callback function
    %% for every StateName in your state machine,
    %% but not one has to be named 'state_name'
    %%
    handle_event/4 % Only for callback_mode() =:= handle_event_function
   ]).


%% Helper function for #params.callback_mode, that caches callback_mode()
-compile({inline, [params_callback_mode/2]}).
params_callback_mode(CallbackMode, Modules) ->
    case CallbackMode of
        state_functions -> CallbackMode;
        handle_event_function ->
            Module = hd(Modules),
            fun Module:handle_event/4
end.

%% Type validation functions
%% - return true if the value is of the type, false otherwise
-compile(
   {inline,
    [callback_mode/1, state_enter/1,
     event_type/1, from/1, timeout_event_type/1]}).
%%
callback_mode(CallbackMode) ->
    case CallbackMode of
	state_functions -> true;
	handle_event_function -> true;
	_ -> false
    end.
%%
state_enter(StateEnter) ->
    case StateEnter of
        state_enter ->
            true;
        _ ->
            false
    end.
%%
-define(
   timeout_event_type(Type),
   case (Type) of
       timeout -> true;
       state_timeout -> true;
       {timeout,_} -> true;
       _ -> false
   end).
timeout_event_type(Type) ->
    ?timeout_event_type(Type).
%%
-define(
   from(From),
   case (From) of
       {_,_} when is_pid(element(1, (From))) -> true;
       _ -> false
   end).
from(From) ->
    ?from(From).
%%
event_type(Type) ->
    case Type of
	{call,From} -> ?from(From);
        %%
	cast -> true;
	info -> true;
	internal -> true;
        %%
        _ -> ?timeout_event_type(Type)
    end.


-define(
   relative_timeout(T),
   ((is_integer(T) andalso 0 =< (T)) orelse (T) =:= infinity)).

-define(
   absolute_timeout(T),
   (is_integer(T) orelse (T) =:= infinity)).

-define(
   STACKTRACE(),
   element(2, erlang:process_info(self(), current_stacktrace))).

-define(not_sys_debug, []).
%%
%% This is a macro to only evaluate arguments if Debug =/= [].
%% Debug is evaluated 2 times.
-define(
   sys_debug(Debug, Extra, SystemEvent),
   case begin Debug end of
       ?not_sys_debug ->
           begin Debug end;
       _ ->
           sys_debug(
             begin Debug end, begin Extra end, begin SystemEvent end)
    end).

-record(params,
        {callback_mode = state_functions ::
           'state_functions' | handle_event_fun(),
         state_enter = false :: boolean(),
         parent :: pid(),
         modules = [?MODULE] :: nonempty_list(module()),
         name :: atom() | pid(),
         hibernate_after = infinity :: timeout()
        }).

-record(state,
        {state_data = {undefined,undefined} ::
           {State :: term(),Data :: term()},
         postponed = [] :: [{event_type(),event_content()}],
         timers = #{t0q => []} ::
           #{
              %% Timeout 0 Queue.
              %% Marked in the table with TimerRef = 0.
              %% Stored here because they also are updated
              %% by e.g cancel_timer/3.
              't0q' := [timeout_event_type()],

              TimeoutType :: timeout_event_type() =>
                             {TimerRef :: reference() | 0,
                              TimeoutMsg :: event_content()}},
         hibernate = false :: boolean()
        }).

%%%==========================================================================
%%% API

-doc """
Name specification to use when starting a `gen_statem` server. See
`start_link/3` and `t:server_ref/0` below.
""".
-type server_name() :: % Duplicate of gen:emgr_name()
        {'local', atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), Name :: term()}.

-doc """
Server specification to use when addressing a `gen_statem` server.

See `call/2` and `t:server_name/0`.

It can be:

- **`pid() | LocalName`** - The `gen_statem` is locally registered.

- **`{Name,Node}`** - The `gen_statem` is locally registered on another node.

- **`{global,GlobalName}`** - The `gen_statem` is globally registered in
  `m:global`.

- **`{via,RegMod,ViaName}`** - The `gen_statem` is registered in an alternative
  process registry. The registry callback module `RegMod` is to export functions
  `register_name/2`, `unregister_name/1`, `whereis_name/1`, and `send/2`, which
  are to behave like the corresponding functions in `m:global`. Thus,
  `{via,global,GlobalName}` is the same as `{global,GlobalName}`.
""".
-type server_ref() :: % What gen:call/3,4 and gen:stop/1,3 accepts
        pid()
      | (LocalName :: atom())
      | {Name :: atom(), Node :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.

-doc """
Options that can be used when starting a `gen_statem` server through, for
example, `start_link/3`.
""".
-type start_opt() :: % Duplicate of gen:option()
        {'timeout', Time :: timeout()}
      | {'spawn_opt', [proc_lib:spawn_option()]}
      | enter_loop_opt().
%%
-doc """
Options that can be used when starting a `gen_statem` server through,
[`enter_loop/4-6`](`enter_loop/4`).

- **`hibernate_after`** - `HibernateAfterTimeout` specifies that the
  `gen_statem` process awaits any message for `HibernateAfterTimeout`
  milliseconds and if no message is received, the process goes into hibernation
  automatically (by calling `proc_lib:hibernate/3`).

- **`debug`** - For every entry in `Dbgs`, the corresponding function in `m:sys`
  is called.
""".
-type enter_loop_opt() :: % Some gen:option()s works for enter_loop/*
	{'hibernate_after', HibernateAfterTimeout :: timeout()}
      | {'debug', Dbgs :: [sys:debug_option()]}.

-doc """
Return value from the [`start/3,4`](`start/3`) and
[`start_link/3,4`](`start_link/3`) functions.
""".
-type start_ret() :: % gen:start_ret() without monitor return
        {'ok', pid()}
      | 'ignore'
      | {'error', term()}.

-doc "Return value from the [`start_monitor/3,4`](`start_monitor/3`) functions.".
-type start_mon_ret() :: % gen:start_ret() with only monitor return
        {'ok', {pid(),reference()}}
      | 'ignore'
      | {'error', term()}.




%% Start a state machine
-doc """
Equivalent to `start/4` except that the `gen_statem` process is not
registered with any [name service](`t:server_name/0`).
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec start(
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start(Module, Args, Opts)
  when is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, nolink, Module, Args, Opts);
start(Module, Args, Opts) ->
    error(badarg, [Module, Args, Opts]).
%%
-doc """
Creates a standalone `gen_statem` process according to OTP design principles
(using `m:proc_lib` primitives). As it does not get linked to the calling
process, this start function cannot be used by a supervisor to start a child.

For a description of arguments and return values, see
[`start_link/4`](`start_link/4`).
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec start(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start(ServerName, Module, Args, Opts)
  when is_tuple(ServerName), is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, nolink, ServerName, Module, Args, Opts);
start(ServerName, Module, Args, Opts) ->
    error(badarg, [ServerName, Module, Args, Opts]).

%% Start and link to a state machine
-doc """
Equivalent to `start_link/4` except that the `gen_statem` process is not
registered with any [name service](`t:server_name/0`).
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec start_link(
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start_link(Module, Args, Opts)
  when is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, link, Module, Args, Opts);
start_link(Module, Args, Opts) ->
    error(badarg, [Module, Args, Opts]).
%%
-doc """
Creates a `gen_statem` process according to OTP design principles (using
`m:proc_lib` primitives) that is spawned as linked to the calling process. This
is essential when the `gen_statem` must be part of a supervision tree so it gets
linked to its supervisor.

The `gen_statem` process calls [`Module:init/1`](`c:init/1`) to initialize the
server. To ensure a synchronized startup procedure, `start_link/3,4` does not
return until [`Module:init/1`](`c:init/1`) has returned or failed.

`ServerName` specifies the `t:server_name/0` to register for the `gen_statem`
process. If the `gen_statem` process is started with
[`start_link/3`](`start_link/3`), no `ServerName` is provided and the
`gen_statem` process is not registered.

`Module` is the name of the callback module.

`Args` is an arbitrary term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

- If option [`{timeout,Time}` ](`t:start_opt/0`)is present in `Opts`, the
  `gen_statem` process is allowed to spend `Time` milliseconds initializing or
  it is terminated and the start function returns
  [`{error,timeout}`](`t:start_ret/0`).
- If option [`{hibernate_after,HibernateAfterTimeout}` ](`t:enter_loop_opt/0`)is
  present, the `gen_statem` process awaits any message for
  `HibernateAfterTimeout` milliseconds and if no message is received, the
  process goes into hibernation automatically (by calling
  `proc_lib:hibernate/3`).
- If option [`{debug,Dbgs}` ](`t:enter_loop_opt/0`)is present in `Opts`,
  debugging through `m:sys` is activated.
- If option [`{spawn_opt,SpawnOpts}` ](`t:start_opt/0`)is present in `Opts`,
  `SpawnOpts` is passed as option list to `erlang:spawn_opt/2`, which is used to
  spawn the `gen_statem` process.

> #### Note {: .info }
>
> Using spawn option `monitor` is not allowed, it causes this function to fail
> with reason `badarg`.

If the `gen_statem` process is successfully created and initialized, this
function returns [`{ok,Pid}`](`t:start_ret/0`), where `Pid` is the `t:pid/0` of
the `gen_statem` process. If a process with the specified `ServerName` exists
already, this function returns
[`{error,{already_started,OtherPid}}`](`t:start_ret/0`), where `OtherPid` is the
`t:pid/0` of that process, and the `gen_statem` process exits with reason
`normal` before calling [`Module:init/1`](`c:init/1`).

If [`Module:init/1`](`c:init/1`) does not return within the
[start timeout](`t:start_opt/0`), the `gen_statem` process is killed with
[`exit(_, kill)`](`erlang:exit/2`), and this function returns
[`{error,timeout}`](`t:start_ret/0`).

This function returns [`{error,Reason}`](`t:start_ret/0`) if
[`Module:init/1`](`c:init/1`) returns [`{stop,Reason}`](`t:init_result/1`) or
[`{error,Reason}`](`t:init_result/1`), or fails with reason `Reason`. This
function returns [`ignore`](`t:start_ret/0`) if [`Module:init/1`](`c:init/1`)
returns [`ignore`](`t:init_result/1`). In these cases the `gen_statem` process
exits with reason `Reason`, except when [`Module:init/1`](`c:init/1`) returns
`ignore` or `{error,_}`; then the `gen_statem` process exits with reason
`normal`.

If `start_link/3,4` returns `ignore` or `{error,_}`, the started `gen_statem`
process has terminated. If an `'EXIT'` message was delivered to the calling
process (due to the process link), that message has been consumed.

> #### Warning {: .warning }
>
> Before OTP 26.0, if the started `gen_statem` process returned e.g.
> `{stop,Reason}` from [`Module:init/1`](`c:init/1`), this function could return
> `{error,Reason}` _before_ the started `gen_statem` process had terminated so
> starting again might fail because VM resources such as the registered name was
> not yet unregistered, and an `'EXIT'` message could arrive later to the
> process calling this function.
>
> But if the started `gen_statem` process instead failed during
> [`Module:init/1`](`c:init/1`), a process link `{'EXIT',Pid,Reason}` message
> caused this function to return `{error,Reason}` so the `'EXIT'` message had
> been consumed and the started `gen_statem` process had terminated.
>
> Since it was impossible to tell the difference between these two cases from
> `start_link/3,4`'s return value, this inconsistency was cleaned up in OTP
> 26.0.

The difference between returning `{stop,_}` and `{error,_}` from
[`Module:init/1`](`c:init/1`), is that `{error,_}` results in a graceful
("silent") termination since the `gen_statem` process exits with reason
`normal`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec start_link(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start_link(ServerName, Module, Args, Opts)
  when is_tuple(ServerName), is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, link, ServerName, Module, Args, Opts);
start_link(ServerName, Module, Args, Opts) ->
    error(badarg, [ServerName, Module, Args, Opts]).

%% Start and monitor a state machine
-doc """
Equivalent to `start_monitor/4` except that the `gen_statem` process is not
registered with any [name service](`t:server_name/0`).
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_mon_ret().
start_monitor(Module, Args, Opts)
  when is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, monitor, Module, Args, Opts);
start_monitor(Module, Args, Opts) ->
    error(badarg, [Module, Args, Opts]).
%%
-doc """
Creates a standalone `gen_statem` process according to OTP design principles
(using `m:proc_lib` primitives) and atomically sets up a monitor to the newly
created process. As it does not get linked to the calling process, this start
function cannot be used by a supervisor to start a child.

For a description of arguments and return values, see
[`start_link/3,4`](`start_link/3`). Note that the return value on successful
start differs from `start_link/3,4`. `start_monitor/3,4` will return
`{ok,{Pid,Mon}}` where `Pid` is the process identifier of the process, and `Mon`
is a reference to the monitor set up to monitor the process. If the start is not
successful, the caller will be blocked until the `DOWN` message has been
received and removed from the message queue.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec start_monitor(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_mon_ret().
start_monitor(ServerName, Module, Args, Opts)
  when is_tuple(ServerName), is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, monitor, ServerName, Module, Args, Opts);
start_monitor(ServerName, Module, Args, Opts) ->
    error(badarg, [ServerName, Module, Args, Opts]).

%% Stop a state machine
-doc #{ equiv => stop(ServerRef, normal, infinity) }.
-doc(#{since => <<"OTP 19.0">>}).
-spec stop(ServerRef :: server_ref()) -> ok.
stop(ServerRef) ->
    gen:stop(ServerRef).
%%
-doc """
Orders the `gen_statem` [`ServerRef`](`t:server_ref/0`) to exit with the
specified `Reason` and waits for it to terminate. The `gen_statem` calls
[`Module:terminate/3`](`c:terminate/3`) before exiting.

This function returns `ok` if the server terminates with the expected reason.
Any other reason than `normal`, `shutdown`, or `{shutdown,Term}` causes an error
report to be issued through `m:logger`. An exit signal with the same reason is
sent to linked processes and ports. The default `Reason` is `normal`.

`Timeout` is an integer > 0, which specifies how many milliseconds to wait for
the server to terminate, or the atom `infinity` to wait indefinitely. Defaults
to `infinity`. If the server does not terminate within the specified time, the
call exits the calling process with reason `timeout`.

If the process does not exist, the call exits the calling process with reason
`noproc`, and with reason `{nodedown,Node}` if the connection fails to the
remote `Node` where the server runs.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec stop(
	ServerRef :: server_ref(),
	Reason :: term(),
	Timeout :: timeout()) -> ok.
stop(ServerRef, Reason, Timeout) ->
    gen:stop(ServerRef, Reason, Timeout).

%% Send an event to a state machine that arrives with type 'event'
-doc """
Sends an asynchronous event to the `gen_statem` [`ServerRef`](`t:server_ref/0`)
and returns `ok` immediately, ignoring if the destination node or `gen_statem`
does not exist.

The `gen_statem` calls the
[_state callback_](`m:gen_statem#state-callback`) with `t:event_type/0` `cast`
and event content `Msg`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec cast(ServerRef :: server_ref(), Msg :: term()) -> ok.
cast(ServerRef, Msg) when is_pid(ServerRef) ->
    send(ServerRef, wrap_cast(Msg));
cast(ServerRef, Msg) when is_atom(ServerRef) ->
    send(ServerRef, wrap_cast(Msg));
cast({global,Name}, Msg) ->
    try	global:send(Name, wrap_cast(Msg)) of
	_ -> ok
    catch
	_:_ -> ok
    end;
cast({via,RegMod,Name}, Msg) ->
    try	RegMod:send(Name, wrap_cast(Msg)) of
	_ -> ok
    catch
	_:_ -> ok
    end;
cast({Name,Node} = ServerRef, Msg) when is_atom(Name), is_atom(Node) ->
    send(ServerRef, wrap_cast(Msg)).

%% Call a state machine (synchronous; a reply is expected) that
%% arrives with type {call,From}
-doc(#{equiv => call(ServerRef, Request, infinity)}).
-doc(#{since => <<"OTP 19.0">>}).
-spec call(ServerRef :: server_ref(), Request :: term()) -> Reply :: term().
call(ServerRef, Request) ->
    call(ServerRef, Request, infinity).
%%
-doc """
Makes a synchronous call to the `gen_statem` [`ServerRef`](`t:server_ref/0`) by
sending a request and waiting until its reply arrives.

The `gen_statem` calls the [_state callback_](`m:gen_statem#state-callback`)
with `t:event_type/0` `{call,From}` and event content `Request`.

A `Reply` is generated when a [_state callback_](`m:gen_statem#state-callback`)
returns with `{reply,From,Reply}` as one `t:action/0`, and that `Reply` becomes
the return value of this function.

`Timeout` is an integer > 0, which specifies how many milliseconds to wait for a
reply, or the atom `infinity` to wait indefinitely, which is the default. If no
reply is received within the specified time, the function call fails.

Previous issue with late replies that could occur when having network issues or
using `dirty_timeout` is now prevented by use of
[_process aliases_](`e:system:ref_man_processes.md#process-aliases`).
`{clean_timeout, T}` and `{dirty_timeout, T}` therefore no longer serves any
purpose and will work the same as `Timeout` while all of them also being equally
efficient.

The call can also fail, for example, if the `gen_statem` dies before or during
this function call.

When this call fails it [exits](`erlang:exit/1`) the calling process. The exit
term is on the form `{Reason, Location}` where
`Location = {gen_statem,call,ArgList}`. See
[`gen_server:call/3` ](`gen_server:call/3`)that has a description of relevant
values for the `Reason` in the exit term.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec call(
	ServerRef :: server_ref(),
	Request :: term(),
	Timeout ::
	  timeout() |
	  {'clean_timeout',T :: timeout()} |
	  {'dirty_timeout',T :: timeout()}) ->
		  Reply :: term().
call(ServerRef, Request, infinity = T = Timeout) ->
    call(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {dirty_timeout, T} = Timeout) ->
    call(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {clean_timeout, T} = Timeout) ->
    call(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {_, _} = Timeout) ->
    error(badarg, [ServerRef,Request,Timeout]);
call(ServerRef, Request, Timeout) ->
    call(ServerRef, Request, Timeout, Timeout).

-doc """
Sends an asynchronous `call` request `Request` to the `gen_statem` process
identified by `ServerRef` and returns a request identifier `ReqId`.

The return value `ReqId` shall later be used with `receive_response/2`, `wait_response/2`,
or `check_response/2` to fetch the actual result of the request. Besides passing
the request identifier directly to these functions, it can also be saved in a
request identifier collection using `reqids_add/3`. Such a collection of request
identifiers can later be used in order to get one response corresponding to a
request in the collection by passing the collection as argument to
`receive_response/3`, `wait_response/3`, or `check_response/3`. If you are about
to save the request identifier in a request identifier collection, you may want
to consider using `send_request/4` instead.

The call
`gen_statem:wait_response(gen_statem:send_request(ServerRef,Request), Timeout)`
can be seen as equivalent to
[`gen_statem:call(Server,Request,Timeout)`](`call/3`), ignoring the error
handling.

The `gen_statem` calls the [_state callback_](`m:gen_statem#state-callback`)
with `t:event_type/0` `{call,From}` and event content `Request`.

A `Reply` is generated when a [_state callback_](`m:gen_statem#state-callback`)
returns with `{reply,From,Reply}` as one `t:action/0`, and that `Reply` becomes
the return value of [`receive_response/1,2`](`receive_response/2`),
[`wait_response/1,2`](`wait_response/2`), or `check_response/2` function.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec send_request(ServerRef::server_ref(), Request::term()) ->
        ReqId::request_id().
send_request(Name, Request) ->
    try
        gen:send_request(Name, '$gen_call', Request)
    catch
        error:badarg ->
            error(badarg, [Name, Request])
    end.

-doc """
Sends an asynchronous `call` request `Request` to the `gen_statem` process
identified by `ServerRef`. The `Label` will be associated with the request
identifier of the operation and added to the returned request identifier
collection `NewReqIdCollection`. The collection can later be used in order to
get one response corresponding to a request in the collection by passing the
collection as argument to `receive_response/3`, `wait_response/3`, or,
`check_response/3`.

The same as calling
[`gen_statem:reqids_add`](`reqids_add/3`)([`statem:send_request`](`send_request/2`)`(ServerRef, Request), Label, ReqIdCollection)`,
but calling [`send_request/4`](`send_request/4`) is slightly more efficient.
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


-doc #{ equiv => receive_response(ReqId, infinity) }.
-doc(#{since => <<"OTP 23.0">>}).
-spec wait_response(ReqId) -> Result when
      ReqId :: request_id(),
      Response :: {reply, Reply::term()}
                | {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

wait_response(ReqId) ->
    wait_response(ReqId, infinity).

-doc """
Wait for a response corresponding to the request identifier `ReqId`. The request
must have been made by `send_request/2` to the `gen_statem` process. This
function must be called from the same process from which `send_request/2` was
made.

`WaitTime` specifies how long to wait for a reply. If no reply is received
within the specified time, the function returns `timeout` and no cleanup is
done, and thus the function can be invoked repeatedly until a reply is returned.

The return value `Reply` is generated when a
[_state callback_](`m:gen_statem#state-callback`) returns with
`{reply,From,Reply}` as one `t:action/0`, and that `Reply` becomes the return
value of this function.

The function returns an error if the `gen_statem` dies before or during this
function call.

The difference between `receive_response/2` and
[`wait_response/2`](`wait_response/2`) is that
[`receive_response/2`](`receive_response/2`) abandons the request at timeout so
that a potential future response is ignored, while
[`wait_response/2`](`wait_response/2`) does not.
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
Wait for a response corresponding to a request identifier saved in
`ReqIdCollection`. All request identifiers of `ReqIdCollection` must correspond
to requests that have been made using `send_request/2` or `send_request/4`, and
all requests must have been made by the process calling this function.

The `Label` in the response equals the `Label` associated with the request
identifier that the response corresponds to. The `Label` of a request identifier
is associated when [saving the request id](`reqids_add/3`) in a request
identifier collection, or when sending the request using `send_request/4`.

Compared to `wait_response/2`, the returned result associated with a specific
request identifier or an exception associated with a specific request identifier
will be wrapped in a 3-tuple. The first element of this tuple equals the value
that would have been produced by [`wait_response/2`](`wait_response/2`), the
second element equals the `Label` associated with the specific request
identifier, and the third element `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned. If no response is
received before the `WaitTime` timeout has triggered, the atom `timeout` is
returned. It is valid to continue waiting for a response as many times as needed
up until a response has been received and completed by `check_response()`,
`receive_response()`, or `wait_response()`.

The difference between `receive_response/3` and
[`wait_response/3`](`wait_response/3`) is that
[`receive_response/3`](`receive_response/3`) abandons requests at timeout so
that potential future responses are ignored, while
[`wait_response/3`](`wait_response/3`) does not.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `ReqIdCollection` in the resulting `NewReqIdCollection`. If `Delete` equals
`false`, `NewReqIdCollection` will equal `ReqIdCollection`. Note that deleting
an association is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
[`wait_response/3`](`wait_response/3`), `check_response/3`, and
`receive_response/3`. However, without deleting handled associations, the above
calls will not be able to detect when there are no more outstanding requests to
handle, so you will have to keep track of this some other way than relying on a
`no_request` return. Note that if you pass a collection only containing
associations of already handled or abandoned requests to
[`wait_response/3`](`wait_response/3`), it will always block until a timeout
determined by `WaitTime` is triggered and then return `no_reply`.
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

-doc #{ equiv => receive_response(ReqId, infinity) }.
-doc(#{since => <<"OTP 24.0">>}).
-spec receive_response(ReqId) -> Result when
      ReqId :: request_id(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

receive_response(ReqId) ->
    receive_response(ReqId, infinity).

-doc """
Receive a response corresponding to the request identifier `ReqId`\- The request
must have been made by `send_request/2` to the `gen_statem` process. This
function must be called from the same process from which `send_request/2` was
made.

`Timeout` specifies how long to wait for a response. If no response is received
within the specified time, the function returns `timeout`. Assuming that the
server executes on a node supporting aliases (introduced in OTP 24) the request
will also be abandoned. That is, no response will be received after a timeout.
Otherwise, a stray response might be received at a later time.

The return value `Reply` is generated when a
[_state callback_](`m:gen_statem#state-callback`) returns with
`{reply,From,Reply}` as one `t:action/0`, and that `Reply` becomes the return
value of this function.

The function returns an error if the `gen_statem` dies before or during this
function call.

The difference between `wait_response/2` and
[`receive_response/2`](`receive_response/2`) is that
[`receive_response/2`](`receive_response/2`) abandons the request at timeout so
that a potential future response is ignored, while
[`wait_response/2`](`wait_response/2`) does not.
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
Receive a response corresponding to a request identifier saved in
`ReqIdCollection`. All request identifiers of `ReqIdCollection` must correspond
to requests that have been made using `send_request/2` or `send_request/4`, and
all requests must have been made by the process calling this function.

The `Label` in the response equals the `Label` associated with the request
identifier that the response corresponds to. The `Label` of a request identifier
is associated when [adding the request id](`reqids_add/3`) in a request
identifier collection, or when sending the request using `send_request/4`.

Compared to `receive_response/2`, the returned result associated with a specific
request identifier will be wrapped in a 3-tuple. The first element of this tuple
equals the value that would have been produced by
[`receive_response/2`](`receive_response/2`), the second element equals the
`Label` associated with the specific request identifier, and the third element
`NewReqIdCollection` is a possibly modified request identifier collection.

If `ReqIdCollection` is empty, the atom `no_request` will be returned.

`Timeout` specifies how long to wait for a response. If no response is received
within the specified time, the function returns `timeout`. Assuming that the
server executes on a node supporting aliases (introduced in OTP 24) all requests
identified by `ReqIdCollection` will also be abandoned. That is, no responses
will be received after a timeout. Otherwise, stray responses might be received
at a later time.

The difference between [`receive_response/3`](`receive_response/3`) and
`wait_response/3` is that [`receive_response/3`](`receive_response/3`) abandons
the requests at timeout so that potential future responses are ignored, while
[`wait_response/3`](`wait_response/3`) does not.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `ReqIdCollection` in the resulting `NewReqIdCollection`. If `Delete` equals
`false`, `NewReqIdCollection` will equal `ReqIdCollection`. Note that deleting
an association is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
[`receive_response/3`](`receive_response/3`), `check_response/3`, and
`wait_response/3`. However, without deleting handled associations, the above
calls will not be able to detect when there are no more outstanding requests to
handle, so you will have to keep track of this some other way than relying on a
`no_request` return. Note that if you pass a collection only containing
associations of already handled or abandoned requests to
[`receive_response/3`](`receive_response/3`), it will always block until a
timeout determined by `Timeout` is triggered.
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
Check if `Msg` is a response corresponding to the request identifier `ReqId`.

The request must have been made by `send_request/2`. If `Msg` is a reply to the
handle `ReqId` the result of the request is returned in `Reply`. Otherwise
returns `no_reply` and no cleanup is done, and thus the function shall be
invoked repeatedly until a reply is returned.

The return value `Reply` is generated when a
[_state callback_](`m:gen_statem#state-callback`) returns with
`{reply,From,Reply}` as one `t:action/0`, and that `Reply` becomes the return
value of this function.

The function returns an error if the `gen_statem` dies before or during this
request.
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
Check if `Msg` is a response corresponding to a request identifier saved in
`ReqIdCollection`. All request identifiers of `ReqIdCollection` must correspond
to requests that have been made using `send_request/2` or `send_request/4`, and
all requests must have been made by the process calling this function.

The `Label` in the response equals the `Label` associated with the request
identifier that the response corresponds to. The `Label` of a request identifier
is associated when [saving the request id](`reqids_add/3`) in a request
identifier collection, or when sending the request using `send_request/4`.

Compared to `check_response/2`, the returned result associated with a specific
request identifier or an exception associated with a specific request identifier
will be wrapped in a 3-tuple. The first element of this tuple equals the value
that would have been produced by [`check_response/2`](`check_response/2`), the
second element equals the `Label` associated with the specific request
identifier, and the third element `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, the atom `no_request` will be returned. If `Msg`
does not correspond to any of the request identifiers in `ReqIdCollection`, the
atom `no_reply` is returned.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `ReqIdCollection` in the resulting `NewReqIdCollection`. If `Delete` equals
`false`, `NewReqIdCollection` will equal `ReqIdCollection`. Note that deleting
an association is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
[`check_response/3`](`check_response/3`), `receive_response/3`, and
`wait_response/3`. However, without deleting handled associations, the above
calls will not be able to detect when there are no more outstanding requests to
handle, so you will have to keep track of this some other way than relying on a
`no_request` return. Note that if you pass a collection only containing
associations of already handled or abandoned requests to
[`check_response/3`](`check_response/3`), it will always return `no_reply`.
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
Returns a new empty request identifier collection. A request identifier
collection can be utilized in order the handle multiple outstanding requests.

Request identifiers of requests made by `send_request/2` can be saved in a
request identifier collection using `reqids_add/3`. Such a collection of request
identifiers can later be used in order to get one response corresponding to a
request in the collection by passing the collection as argument to
`receive_response/3`, `wait_response/3`, or, `check_response/3`.

`reqids_size/1` can be used to determine the amount of request identifiers in a
request identifier collection.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec reqids_new() ->
          NewReqIdCollection::request_id_collection().

reqids_new() ->
    gen:reqids_new().

-doc "Returns the amount of request identifiers saved in `ReqIdCollection`.".
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
Saves `ReqId` and associates a `Label` with the request identifier by adding
this information to `ReqIdCollection` and returning the resulting request
identifier collection.
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
Returns a list of `{ReqId, Label}` tuples which corresponds to all request
identifiers with their associated labels present in the `ReqIdCollection`
collection.
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

%% Reply from a state machine callback to whom awaits in call/2
-doc """
Send a reply or multiple replies using one or several `t:reply_action/0`s from a
[_state callback_](`m:gen_statem#state-callback`).

This function can be used by a `gen_statem` to explicitly send a reply to a
process that waits in `call/2` when the reply cannot be defined in the return
value of a [_state callback_](`m:gen_statem#state-callback`).

> #### Note {: .info }
>
> A reply sent with this function is not visible in `m:sys` debug output.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec reply(Replies :: [reply_action()] | reply_action()) -> ok.
reply({reply,From,Reply}) ->
    reply(From, Reply);
reply(Replies) when is_list(Replies) ->
    replies(Replies).
%%
-compile({inline, [reply/2]}).
-doc """
Send a `Reply` to `From`.

This function can be used by a `gen_statem` to explicitly send a reply to a
process that waits in `call/2` when the reply cannot be defined in the return
value of a [_state callback_](`m:gen_statem#state-callback`).

`From` must be the term from argument [`{call,From}`](`t:event_type/0`) to the
[_state callback_](`m:gen_statem#state-callback`). 

> #### Note {: .info }
>
> A reply sent with this function is not visible in `m:sys` debug output.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec reply(From :: from(), Reply :: term()) -> ok.
reply(From, Reply) ->
    gen:reply(From, Reply).

%% Instead of starting the state machine through start/3,4
%% or start_link/3,4 turn the current process presumably
%% started by proc_lib into a state machine using
%% the same arguments as you would have returned from init/1
-doc """
The same as `enter_loop/6` with `Actions = []` except that no `t:server_name/0`
must have been registered. This creates an anonymous server.
""".
-doc(#{since => <<"OTP 19.1">>}).
-spec enter_loop(
	Module :: module(), Opts :: [enter_loop_opt()],
	State :: state(), Data :: data()) ->
			no_return().
enter_loop(Module, Opts, State, Data) ->
    enter_loop(Module, Opts, State, Data, self()).
%%
-doc """
If `Server_or_Actions` is a `t:list/0`, the same as `enter_loop/6` except that
no `t:server_name/0` must have been registered and
`Actions = Server_or_Actions`. This creates an anonymous server.

Otherwise the same as `enter_loop/6` with `Server = Server_or_Actions` and
`Actions = []`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec enter_loop(
	Module :: module(), Opts :: [enter_loop_opt()],
	State :: state(), Data :: data(),
	Server_or_Actions ::
	  server_name() | pid() | [action()]) ->
			no_return().
enter_loop(Module, Opts, State, Data, Server_or_Actions) ->
    if
	is_list(Server_or_Actions) ->
	    enter_loop(Module, Opts, State, Data, self(), Server_or_Actions);
	true ->
	    enter_loop(Module, Opts, State, Data, Server_or_Actions, [])
    end.
%%
-doc """
Makes the calling process become a `gen_statem`.

Does not return, instead the
calling process enters the `gen_statem` receive loop and becomes a `gen_statem`
server. The process _must_ have been started using one of the start functions in
`m:proc_lib`. The user is responsible for any initialization of the process,
including registering a name for it.

This function is useful when a more complex initialization procedure is needed
than the `gen_statem` behavior provides.

`Module`, `Opts` have the same meaning as when calling
[`start[_link|_monitor]/3,4`](`start_link/3`).

If `Server` is `self/0` an anonymous server is created just as when using
[`start[_link|_monitor]/3`](`start_link/3`). If `Server` is a `t:server_name/0`
a named server is created just as when using
[`start[_link|_monitor]/4`](`start_link/4`). However, the `t:server_name/0` name
must have been registered accordingly _before_ this function is called.

`State`, `Data`, and `Actions` have the same meanings as in the return value of
[`Module:init/1`](`c:init/1`). Also, the callback module does not need to export
a [`Module:init/1`](`c:init/1`) function.

The function fails if the calling process was not started by a `m:proc_lib`
start function, or if it is not registered according to `t:server_name/0`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec enter_loop(
	Module :: module(), Opts :: [enter_loop_opt()],
	State :: state(), Data :: data(),
	Server :: server_name() | pid(),
	Actions :: [action()] | action()) ->
			no_return().
enter_loop(Module, Opts, State, Data, Server, Actions) ->
    is_atom(Module) orelse error({atom,Module}),
    Parent = gen:get_parent(),
    Name = gen:get_proc_name(Server),
    Debug = gen:debug_options(Name, Opts),
    HibernateAfterTimeout = gen:hibernate_after(Opts),
    enter(
      Parent, Debug, Module, Name, HibernateAfterTimeout,
      State, Data, Actions).

%%---------------------------------------------------------------------------
%% API helpers

-compile({inline, [wrap_cast/1]}).
wrap_cast(Event) ->
    {'$gen_cast',Event}.

-compile({inline, [call/4]}).
call(ServerRef, Request, Timeout, T) ->
    try gen:call(ServerRef, '$gen_call', Request, T) of
        {ok,Reply} ->
            Reply
    catch
        %% 'gen' raises 'exit' for problems
        Class:Reason:Stacktrace when Class =:= exit ->
            erlang:raise(
              Class,
              %% Wrap the reason according to tradition
              {Reason,{?MODULE,call,[ServerRef,Request,Timeout]}},
              Stacktrace)
    end.

replies([{reply,From,Reply}|Replies]) ->
    reply(From, Reply),
    replies(Replies);
replies([]) ->
    ok.

%% Might actually not send the message in case of caught exception
send(Proc, Msg) ->
    try erlang:send(Proc, Msg)
    catch
        error:_ -> ok
    end,
    ok.

%% Here the init_it/6 and enter_loop/5,6,7 functions converge
enter(
  Parent, Debug, Module, Name, HibernateAfterTimeout,
  State, Data, Actions) ->
    %% The values should already have been type checked
    Q = [{internal,init_state}],
    %% We enforce {postpone,false} to ensure that
    %% our fake Event gets discarded, thought it might get logged
    Actions_1 = listify(Actions) ++ [{postpone,false}],
    Modules = [Module],
    P =
        #params{
           parent = Parent,
           name = Name,
           hibernate_after = HibernateAfterTimeout},
    S = #state{state_data = {State,Data}},
    case get_callback_mode(P, Modules) of
        #params{} = P_1 ->
            Debug_1 = ?sys_debug(Debug, Name, {enter,Module,State}),
            loop_enter(P_1, Debug_1, S, Q, {State,Data}, Actions_1);
        {Class, Reason, Stacktrace} ->
            P_1 = P#params{modules = Modules},
            terminate(Class, Reason, Stacktrace, P_1, Debug, S, Q)
    end.

%%%==========================================================================
%%%  gen callbacks

-doc false.
init_it(Starter, self, ServerRef, Module, Args, Opts) ->
    init_it(Starter, self(), ServerRef, Module, Args, Opts);
init_it(Starter, Parent, ServerRef, Module, Args, Opts) ->
    Name = gen:get_proc_name(ServerRef),
    Debug = gen:debug_options(Name, Opts),
    HibernateAfterTimeout = gen:hibernate_after(Opts),
    try Module:init(Args) of
	Result ->
	    init_result(
              Starter, Parent, ServerRef, Module, Result,
              Name, Debug, HibernateAfterTimeout)
    catch
	Result ->
	    init_result(
              Starter, Parent, ServerRef, Module, Result,
              Name, Debug, HibernateAfterTimeout);
	Class:Reason:Stacktrace ->
	    gen:unregister_name(ServerRef),
	    error_info(
	      Class, Reason, Stacktrace, Debug,
              #params{parent = Parent, name = Name, modules = [Module]},
              #state{}, []),
            proc_lib:init_fail(
              Starter, {error,Reason}, {Class,Reason,Stacktrace})
    end.

%%---------------------------------------------------------------------------
%% gen callbacks helpers

init_result(
  Starter, Parent, ServerRef, Module, Result,
  Name, Debug, HibernateAfterTimeout) ->
    case Result of
	{ok,State,Data} ->
	    proc_lib:init_ack(Starter, {ok,self()}),
            enter(
              Parent, Debug, Module, Name, HibernateAfterTimeout,
              State, Data, []);
	{ok,State,Data,Actions} ->
	    proc_lib:init_ack(Starter, {ok,self()}),
            enter(
              Parent, Debug, Module, Name, HibernateAfterTimeout,
              State, Data, Actions);
	{stop,Reason} ->
	    gen:unregister_name(ServerRef),
            exit(Reason);
	{error, _Reason} = ERROR ->
            %% The point of this clause is that we shall have a *silent*
            %% termination. The error reason will be returned to the
            %% 'Starter' ({error, Reason}), but *no* crash report.
	    gen:unregister_name(ServerRef),
	    proc_lib:init_fail(Starter, ERROR, {exit,normal});
	ignore ->
	    gen:unregister_name(ServerRef),
            proc_lib:init_fail(Starter, ignore, {exit,normal});
	_ ->
	    gen:unregister_name(ServerRef),
	    Reason = {bad_return_from_init,Result},
	    error_info(
	      error, Reason, ?STACKTRACE(), Debug,
              #params{parent = Parent, name = Name, modules = [Module]},
              #state{}, []),
            exit(Reason)
    end.

%%%==========================================================================
%%% sys callbacks
%%%
%%% We use {P,S} as state (Misc) for the sys module,
%%% wrap/unwrap it for the server loop* and update
%%% P#params{parent = Parent}.

-doc false.
system_continue(Parent, Debug, {P,S}) ->
    loop(update_parent(P, Parent), Debug, S).

-doc false.
system_terminate(Reason, Parent, Debug, {P,S}) ->
    terminate(
      exit, Reason, ?STACKTRACE(),
      update_parent(P, Parent), Debug, S, []).

-doc false.
system_code_change(
  {#params{modules = [Module | _] = Modules} = P,
   #state{state_data = {State,Data}} = S},
  _Mod, OldVsn, Extra) ->
    case
	try Module:code_change(OldVsn, State, Data, Extra)
	catch
	    Result -> Result
	end
    of
	{ok,NewState,NewData} ->
            case get_callback_mode(P, Modules) of
                #params{} = P_1 ->
                    {ok,
                     {P_1,
                      S#state{state_data = {NewState,NewData}}}};
                {Class, Reason, Stacktrace} ->
                    erlang:raise(Class, Reason, Stacktrace)
            end;
	{ok,_} = Error ->
	    error({case_clause,Error});
	Error ->
	    Error
    end.

-doc false.
system_get_state({_P,#state{state_data = State_Data}}) ->
    {ok,State_Data}.

-doc false.
system_replace_state(
  StateFun, {P,#state{state_data = State_Data} = S}) ->
    %%
    NewState_NewData = StateFun(State_Data),
    {ok,NewState_NewData,{P,S#state{state_data = NewState_NewData}}}.

-doc false.
format_status(
  Opt,
  [PDict,SysState,Parent,Debug,
   {#params{name = Name, modules = [Mod | _] = Modules},
    #state{postponed = Postponed, timers = Timers,
           state_data = {State,Data}}}]) ->
    Header = gen:format_status_header("Status for state machine", Name),

    {NumTimers, ListTimers} = list_timeouts(Timers),
    StatusMap = #{ state => State, data => Data,
                   postponed => Postponed, log => sys:get_log(Debug),
                   timeouts => ListTimers
                 },

    NewStatusMap =
        case gen:format_status(Mod, Opt, StatusMap, [PDict,State,Data]) of
            #{ 'EXIT' := R } ->
                Crashed = [{data,[{"State",{State,R}}]}],
                StatusMap#{ '$status' => Crashed };
            %% Status is set when the old format_status/2 is called,
            %% so we do a little backwards compatibility dance here
            #{ '$status' := L } = SM when is_list(L) -> SM;
            #{ '$status' := T } = SM -> SM#{ '$status' := [T] };
            #{ state := S, data := D } = SM ->
                SM#{ '$status' => [{data,[{"State",{S,D}}]}]}
        end,

    [{header,Header},
     {data,
      [{"Status",SysState},
       {"Parent",Parent},
       {"Modules",Modules},
       {"Time-outs",{NumTimers,maps:get(timeouts,NewStatusMap)}},
       {"Logged Events",maps:get(log,NewStatusMap)},
       {"Postponed",maps:get(postponed,NewStatusMap)}]} |
     maps:get('$status',NewStatusMap)].

%% Update #params.parent only if it differs.  This should not
%% be possible today (OTP-22.0), but could happen for example
%% if someone implements changing a server's parent
%% in a new sys call.
-compile({inline, update_parent/2}).
update_parent(P, Parent) ->
    case P of
        #params{parent = Parent} ->
            P;
        #params{} ->
            P#params{parent = Parent}
    end.

%%---------------------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%---------------------------------------------------------------------------

sys_debug(Debug, NameState, Entry) ->
  sys:handle_debug(Debug, fun print_event/3, NameState, Entry).

print_event(Dev, SystemEvent, Name) ->
    case SystemEvent of
        {in,Event,State} ->
            io:format(
              Dev, "*DBG* ~tp receive ~ts in state ~tp~n",
              [Name,event_string(Event),State]);
        {out,Reply,{To,_Tag}} ->
            io:format(
              Dev, "*DBG* ~tp send ~tp to ~tw~n",
              [Name,Reply,To]);
        {enter,Module,State} ->
            io:format(
              Dev, "*DBG* ~tp enter ~tp in state ~tp~n",
              [Name,Module,State]);
        {module,Module,State} ->
            io:format(
              Dev, "*DBG* ~tp module ~tp in state ~tp~n",
              [Name,Module,State]);
        {start_timer,Action,State} ->
            io:format(
              Dev, "*DBG* ~tp start_timer ~tp in state ~tp~n",
              [Name,Action,State]);
        {insert_timeout,Event,State} ->
            io:format(
              Dev, "*DBG* ~tp insert_timeout ~tp in state ~tp~n",
              [Name,Event,State]);
        {terminate,Reason,State} ->
            io:format(
              Dev, "*DBG* ~tp terminate ~tp in state ~tp~n",
              [Name,Reason,State]);
        {Tag,Event,State,NextState}
          when Tag =:= postpone; Tag =:= consume ->
            StateString =
                case NextState of
                    State ->
                        io_lib:format("~tp", [State]);
                    _ ->
                        io_lib:format("~tp => ~tp", [State,NextState])
                end,
            io:format(
              Dev, "*DBG* ~tp ~tw ~ts in state ~ts~n",
              [Name,Tag,event_string(Event),StateString])
    end.

event_string(Event) ->
    case Event of
	{{call,{Pid,_Tag}},Request} ->
	    io_lib:format("call ~tp from ~tw", [Request,Pid]);
	{EventType,EventContent} ->
	    io_lib:format("~tw ~tp", [EventType,EventContent])
    end.

%%%==========================================================================
%%% Internal callbacks

-doc false.
wakeup_from_hibernate(P, Debug, S) ->
    %% It is a new message that woke us up so we have to receive it now
    loop_receive(P, Debug, S).

%%%==========================================================================
%%% State Machine engine implementation on proc_lib/gen

%% Server loop, consists of all loop* functions
%% and detours through sys:handle_system_message/7 and proc_lib:hibernate/3
%%
%% The loop tries to keep all temporary values in arguments
%% and takes shortcuts for ?not_sys_debug, empty lists, etc.
%% The engine state #state{} is picked apart during the loop,
%% new values are kept in arguments, and a new #state{} is
%% composed at the end of the loop.  #params{} collect engine
%% state fields that rarely changes.
%%
%% The loop is optimized a bit for staying in the loop, assuming that
%% system events are rare.  So a detour to sys requires re-packing
%% of the engine state.

%% Entry point for system_continue/3
%%
loop(P, Debug, #state{hibernate = true} = S) ->
    loop_hibernate(P, Debug, S);
loop(P, Debug, S) ->
    loop_receive(P, Debug, S).

%% Go to hibernation
%%
loop_hibernate(P, Debug, S) ->
    %%
    %% Does not return but restarts process at
    %% wakeup_from_hibernate/3 that jumps to loop_receive/3
    %%
    proc_lib:hibernate(?MODULE, wakeup_from_hibernate, [P, Debug, S]),
    error(
      {should_not_have_arrived_here_but_instead_in,
       {?MODULE,wakeup_from_hibernate,3}}).


%% Entry point for wakeup_from_hibernate/3
%%
%% Receive a new process message
%%
loop_receive(
  #params{hibernate_after = HibernateAfterTimeout} = P, Debug, S) ->
    %%
    receive
	Msg ->
	    case Msg of
                {'$gen_call',From,Request} ->
                    loop_receive_result(P, Debug, S, {{call,From},Request});
                {'$gen_cast',Cast} ->
                    loop_receive_result(P, Debug, S, {cast,Cast});
                %%
		{timeout,TimerRef,TimeoutType} ->
                    case S#state.timers of
                        #{TimeoutType := {TimerRef,TimeoutMsg}} = Timers
                          when TimeoutType =/= t0q->
                            %% Our timer
                            Timers_1 = maps:remove(TimeoutType, Timers),
                            S_1 = S#state{timers = Timers_1},
                            loop_receive_result(
                              P, Debug, S_1, {TimeoutType,TimeoutMsg});
                        #{} ->
			    loop_receive_result(P, Debug, S, {info,Msg})
		    end;
                %%
		{system,Pid,Req} ->
		    %% Does not return but tail recursively calls
		    %% system_continue/3 that jumps to loop/3
		    sys:handle_system_msg(
		      Req, Pid, P#params.parent, ?MODULE, Debug,
                      {P,S},
		      S#state.hibernate);
		{'EXIT',Pid,Reason} ->
                    case P#params.parent of
                        Pid ->
                            terminate(
                              exit, Reason, ?STACKTRACE(), P, Debug, S, []);
                        _ ->
                            loop_receive_result(P, Debug, S, {info,Msg})
                    end;
                %%
                _ ->
                    loop_receive_result(P, Debug, S, {info,Msg})
	    end
    after
        HibernateAfterTimeout ->
            loop_hibernate(P, Debug, S)
    end.

%% We have received an event
%%
loop_receive_result(P, ?not_sys_debug = Debug, S, Event) ->
    %% Here is the queue of not yet handled events created
    Events = [],
    loop_event(P, Debug, S, Event, Events);
loop_receive_result(
  #params{name = Name} = P, Debug,
  #state{state_data = {State,_Data}} = S, Event) ->
    Debug_1 = sys_debug(Debug, Name, {in,Event,State}),
    %% Here is the queue of not yet handled events created
    Events = [],
    loop_event(P, Debug_1, S, Event, Events).

%% Handle one event; received or enqueued
%%
loop_event(
  P, Debug, #state{hibernate = true} = S, Event, Events) ->
    %%
    %% If (this old) Hibernate is true here it can only be
    %% because it was set from an event action
    %% and we did not go into hibernation since there were
    %% events in queue, so we do what the user
    %% might rely on i.e collect garbage which
    %% would have happened if we actually hibernated
    %% and immediately was awakened.
    %%
    _ = garbage_collect(),
    %%
    %% The field 'hibernate' in S is now invalid and will be
    %% restored when looping back to loop/3 or loop_event/5.
    %%
    Q = [Event|Events],
    loop_state_callback(P, Debug, S, Q, S#state.state_data, Event);
loop_event(P, Debug, S, Event, Events) ->
    %%
    %% The field 'hibernate' in S is now invalid and will be
    %% restored when looping back to loop/3 or loop_event/5.
    %%
    Q = [Event|Events],
    loop_state_callback(P, Debug, S, Q, S#state.state_data, Event).

%% Make a state enter call to the state function, we loop back here
%% from further down if state enter calls are enabled
%%
-compile({inline, [loop_state_enter/9]}).
loop_state_enter(
  P, Debug, #state{state_data = {PrevState,_PrevData}} = S,
  Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone) ->
    %%
    StateCall = false,
    CallbackEvent = {enter,PrevState},
    loop_state_callback(
      P, Debug, S, Q, NextState_NewData,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      StateCall, CallbackEvent).

%% Loop entry point from enter/8 skipping to loop_actions_list
%% since we should not call a state callback, but initialize
%% loop variables in the same way; compare to
%% loop_state_callback/6 just below
-compile({inline, [loop_enter/6]}).
loop_enter(P, Debug, S, Q, State_Data, Actions) ->
    NextEventsR = [],
    Hibernate = false,
    TimeoutsR = [],
    Postpone = false,
    CallEnter = true,
    StateCall = true,
    loop_actions_list(
      P, Debug, S, Q, State_Data,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      CallEnter, StateCall, Actions).

%% Make a state call (not state enter call) to the state function
%%
-compile({inline, [loop_state_callback/6]}).
loop_state_callback(P, Debug, S, Q, State_Data, CallbackEvent) ->
    NextEventsR = [],
    Hibernate = false,
    TimeoutsR = [],
    Postpone = false,
    StateCall = true,
    loop_state_callback(
      P, Debug, S, Q, State_Data,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      StateCall, CallbackEvent).
%%
loop_state_callback(
  #params{callback_mode = CallbackMode, modules = [Module | _]} = P,
  Debug, S, Q, {State,Data} = State_Data,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  StateCall, {Type,Content}) ->
    try
	case CallbackMode of
	    state_functions ->
		Module:State(Type, Content, Data);
            HandleEventFun when is_function(HandleEventFun, 4) ->
		HandleEventFun(Type, Content, State, Data)
	end
    of
	Result ->
            loop_state_callback_result(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              StateCall, Result)
    catch
	Result ->
            loop_state_callback_result(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              StateCall, Result);
	Class:Reason:Stacktrace ->
	    terminate(Class, Reason, Stacktrace, P, Debug, S, Q)
    end.

%% Process the result from the state function
%%
loop_state_callback_result(
  P, Debug, S, Q, {State,_Data} = State_Data,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  StateCall, Result) ->
    %%
    case Result of
	{next_state,State,NewData} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false);
	{next_state,NextState,NewData}
          when StateCall ->
            loop_actions(
              P, Debug, S, Q, {NextState,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true);
	{next_state,_NextState,_NewData} ->
            terminate(
              error,
              {bad_state_enter_return_from_state_function,Result},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q);
	{next_state,State,NewData,Actions} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false, StateCall, Actions);
	{next_state,NextState,NewData,Actions}
          when StateCall ->
            loop_actions(
              P, Debug, S, Q, {NextState,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true, StateCall, Actions);
	{next_state,_NextState,_NewData,_Actions} ->
            terminate(
              error,
              {bad_state_enter_return_from_state_function,Result},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q);
        %%
        {keep_state,NewData} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false);
        {keep_state,NewData,Actions} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false, StateCall, Actions);
        %%
        keep_state_and_data ->
            loop_actions(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false);
        {keep_state_and_data,Actions} ->
            loop_actions(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false, StateCall, Actions);
        %%
        {repeat_state,NewData} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true);
        {repeat_state,NewData,Actions} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true, StateCall, Actions);
        %%
        repeat_state_and_data ->
            loop_actions(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true);
        {repeat_state_and_data,Actions} ->
            loop_actions(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true, StateCall, Actions);
        %%
	stop ->
            terminate(
              exit, normal, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q);
	{stop,Reason} ->
            terminate(
              exit, Reason, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q);
	{stop,Reason,NewData} ->
            terminate(
              exit, Reason, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = {State,NewData},
                hibernate = Hibernate},
              Q);
	%%
	{stop_and_reply,Reason,Replies} ->
            reply_then_terminate(
              exit, Reason, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q, Replies);
	{stop_and_reply,Reason,Replies,NewData} ->
            reply_then_terminate(
              exit, Reason, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = {State,NewData},
                hibernate = Hibernate},
              Q, Replies);
	%%
	_ ->
            terminate(
              error,
              {bad_return_from_state_function,Result},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q)
    end.

%% Ensure that Actions are a list
%%
loop_actions(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, _StateCall, []) ->
    loop_actions(
      P, Debug, S, Q, NextState_NewData,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      CallEnter);
loop_actions(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions) ->
    %%
    loop_actions_list(
      P, Debug, S, Q, NextState_NewData,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      CallEnter, StateCall, listify(Actions)).
%%
%% Shortcut for no actions
loop_actions(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter) ->
    %%
    %% Shortcut for no actions
    case CallEnter andalso P#params.state_enter of
	true ->
            loop_state_enter(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone);
	false ->
            loop_state_transition(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone)
    end.

%% Process the returned actions
%%
loop_actions_list(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, _StateCall, []) ->
    %%
    case P#params.state_enter of
        true when CallEnter ->
            loop_state_enter(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone);
        _ ->
            loop_state_transition(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone)
    end;
loop_actions_list(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, [Action|Actions]) ->
    %%
    case Action of
	%% Actual actions
	{reply,From,Reply} ->
            loop_actions_reply(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions,
              From, Reply);
	%%
	%% Actions that set options
	{hibernate,Hibernate_1} when is_boolean(Hibernate_1) ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate_1, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions);
	hibernate ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, true, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions);
	%%
	{postpone,Postpone_1} when not Postpone_1 orelse StateCall ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone_1,
              CallEnter, StateCall, Actions);
	postpone when StateCall ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, true,
              CallEnter, StateCall, Actions);
	postpone ->
            terminate(
              error,
              {bad_state_enter_action_from_state_function,Action},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q);
	%%
	{next_event,Type,Content} ->
            loop_actions_next_event(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions, Type, Content);
        %%
        {Tag, NewModule}
          when Tag =:= change_callback_module, is_atom(NewModule);
               Tag =:= push_callback_module, is_atom(NewModule) ->
            if
                StateCall ->
                    NewModules =
                        case Tag of
                            change_callback_module ->
                                [NewModule | tl(P#params.modules)];
                            push_callback_module ->
                                [NewModule | P#params.modules]
                        end,
                    case get_callback_mode(P, NewModules) of
                        #params{} = P_1 ->
                            {NextState,_NewData} = NextState_NewData,
                            Debug_1 =
                                ?sys_debug(
                                   Debug, P#params.name,
                                   {module,NewModule,NextState}),
                            loop_actions_list(
                              P_1, Debug_1, S, Q, NextState_NewData,
                              NextEventsR, Hibernate, TimeoutsR, Postpone,
                              CallEnter, StateCall, Actions);
                        {Class, Reason, Stacktrace} ->
                            terminate(
                              Class, Reason, Stacktrace, P, Debug,
                              S#state{
                                state_data = NextState_NewData,
                                hibernate = Hibernate},
                              Q)
                    end;
                true ->
                    terminate(
                      error,
                      {bad_state_enter_action_from_state_function,Action},
                      ?STACKTRACE(), P, Debug,
                      S#state{
                        state_data = NextState_NewData,
                        hibernate = Hibernate},
                      Q)
            end;
        pop_callback_module when tl(P#params.modules) =/= [] ->
            if
                StateCall ->
                    NewModules = tl(P#params.modules),
                    case get_callback_mode(P, NewModules) of
                        #params{} = P_1 ->
                            {NextState,_NewData} = NextState_NewData,
                            Debug_1 =
                                ?sys_debug(
                                   Debug, P#params.name,
                                   {module,hd(NewModules),NextState}),
                            loop_actions_list(
                              P_1, Debug_1, S, Q, NextState_NewData,
                              NextEventsR, Hibernate, TimeoutsR, Postpone,
                              CallEnter, StateCall, Actions);
                        {Class, Reason, Stacktrace} ->
                            terminate(
                              Class, Reason, Stacktrace, P, Debug,
                              S#state{
                                state_data = NextState_NewData,
                                hibernate = Hibernate},
                              Q)
                    end;
                true ->
                    terminate(
                      error,
                      {bad_state_enter_action_from_state_function,Action},
                      ?STACKTRACE(), P, Debug,
                      S#state{
                        state_data = NextState_NewData,
                        hibernate = Hibernate},
                      Q)
            end;
	%%
        _ ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions, Action)
    end.

%% Process all other actions, i.e timeout actions,
%% all others are unrecognized
%%
loop_actions_list(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  {TimeoutType,Time,TimeoutMsg,TimeoutOpts} = Timeout) ->
    %%
    case timeout_event_type(TimeoutType) of
        true ->
            case listify(TimeoutOpts) of
                %% Optimization cases
                [{abs,true}] when ?absolute_timeout(Time) ->
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      NextEventsR, Hibernate,
                      [Timeout|TimeoutsR], Postpone,
                      CallEnter, StateCall, Actions);
                [{abs,false}] when ?relative_timeout(Time) ->
                    RelativeTimeout = {TimeoutType,Time,TimeoutMsg},
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      NextEventsR, Hibernate,
                      [RelativeTimeout|TimeoutsR], Postpone,
                      CallEnter, StateCall, Actions);
                [] when ?relative_timeout(Time) ->
                    RelativeTimeout = {TimeoutType,Time,TimeoutMsg},
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      NextEventsR, Hibernate,
                      [RelativeTimeout|TimeoutsR], Postpone,
                      CallEnter, StateCall, Actions);
                %% Generic case
                TimeoutOptsList ->
                    case parse_timeout_opts_abs(TimeoutOptsList) of
                        true when ?absolute_timeout(Time) ->
                            loop_actions_list(
                              P, Debug, S, Q, NextState_NewData,
                              NextEventsR, Hibernate,
                              [Timeout|TimeoutsR], Postpone,
                              CallEnter, StateCall, Actions);
                        false when ?relative_timeout(Time) ->
                            RelativeTimeout = {TimeoutType,Time,TimeoutMsg},
                            loop_actions_list(
                              P, Debug, S, Q, NextState_NewData,
                              NextEventsR, Hibernate,
                              [RelativeTimeout|TimeoutsR], Postpone,
                              CallEnter, StateCall, Actions);
                        _ ->
                            terminate(
                              error,
                              {bad_action_from_state_function,Timeout},
                              ?STACKTRACE(), P, Debug,
                              S#state{
                                state_data = NextState_NewData,
                                hibernate = Hibernate},
                              Q)
                    end
            end;
        false ->
            terminate(
              error,
              {bad_action_from_state_function,Timeout},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end;
loop_actions_list(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  {TimeoutType,Time,_TimeoutMsg} = Timeout) ->
    %%
    case timeout_event_type(TimeoutType) of
        true
          when ?relative_timeout(Time);
               Time =:= update ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate,
              [Timeout|TimeoutsR], Postpone,
              CallEnter, StateCall, Actions);
        _ ->
            terminate(
              error,
              {bad_action_from_state_function,Timeout},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end;
loop_actions_list(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  {TimeoutType,cancel} = Action) ->
    %%
    case timeout_event_type(TimeoutType) of
        true ->
            Timeout = {TimeoutType,infinity,undefined},
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate,
              [Timeout|TimeoutsR], Postpone,
              CallEnter, StateCall, Actions);
        false ->
            terminate(
              error,
              {bad_action_from_state_function,Action},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end;
loop_actions_list(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  Time) ->
    %%
    if
        ?relative_timeout(Time) ->
            Timeout = {timeout,Time,Time},
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate,
              [Timeout|TimeoutsR], Postpone,
              CallEnter, StateCall, Actions);
        true ->
            terminate(
              error,
              {bad_action_from_state_function,Time},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end.

%% Process a reply action
%%
loop_actions_reply(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  From, Reply) ->
    %%
    case from(From) of
        true ->
            %% No need for a separate ?not_sys_debug clause here
            %% since the external call to erlang:'!'/2 in reply/2
            %% will cause swap out of all live registers anyway
            reply(From, Reply),
            Debug_1 = ?sys_debug(Debug, P#params.name, {out,Reply,From}),
            loop_actions_list(
              P, Debug_1, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions);
        false ->
            terminate(
              error,
              {bad_action_from_state_function,{reply,From,Reply}},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end.

%% Process a next_event action
%%
loop_actions_next_event(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions, Type, Content) ->
    case event_type(Type) of
        true when StateCall ->
            NextEvent = {Type,Content},
            if
                Debug =:= ?not_sys_debug ->
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      [NextEvent|NextEventsR],
                      Hibernate, TimeoutsR, Postpone,
                      CallEnter, StateCall, Actions);
                true ->
                    Name = P#params.name,
                    {NextState,_NewData} = NextState_NewData,
                    Debug_1 = sys_debug(Debug, Name, {in,NextEvent,NextState}),
                    loop_actions_list(
                      P, Debug_1, S, Q, NextState_NewData,
                      [NextEvent|NextEventsR],
                      Hibernate, TimeoutsR, Postpone,
                      CallEnter, StateCall, Actions)
            end;
        _ ->
            loop_actions_next_event_bad(
              P, Debug, S, Q, NextState_NewData,
              StateCall, Hibernate, Type, Content)
    end.

loop_actions_next_event_bad(
  P, Debug, S, Q, NextState_NewData,
  StateCall, Hibernate, Type, Content) ->
    terminate(
      error,
      {case StateCall of
           true ->
               bad_action_from_state_function;
           false ->
               bad_state_enter_action_from_state_function
       end,
       {next_event,Type,Content}},
      ?STACKTRACE(), P, Debug,
      S#state{
        state_data = NextState_NewData,
        hibernate = Hibernate},
      Q).

%% Do the state transition
%%
loop_state_transition(
  P, Debug, #state{state_data = {State,_Data}, postponed = Postponed} = S,
  [Event|Events], {NextState,_NewData} = NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone) ->
    %%
    %% All options have been collected and next_events are buffered.
    %% Do the actual state transition.
    %%
    Postponed_1 = % Move current event to postponed if Postpone
	case Postpone of
	    true ->
                [Event|Postponed];
	    false ->
                Postponed
	end,
    case Debug of
        ?not_sys_debug ->
	    %% Optimization for no sys_debug
	    %% - avoid calling sys_debug/3
	    if
		NextState =:= State ->
		    loop_keep_state(
                      P, Debug, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed_1);
		true ->
		    loop_state_change(
                      P, Debug, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed_1)
	    end;
        _ ->
            %% With sys_debug
            Name = P#params.name,
	    Debug_1 =
		case Postpone of
		    true ->
			sys_debug(
			   Debug, Name,
			   {postpone,Event,State,NextState});
		    false ->
			sys_debug(
			   Debug, Name,
			   {consume,Event,State,NextState})
		end,
	    if
		NextState =:= State ->
		    loop_keep_state(
                      P, Debug_1, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed_1);
		true ->
		    loop_state_change(
                      P, Debug_1, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed_1)
	    end
    end.

%% State transition to the same state
%%
loop_keep_state(
  P, Debug, #state{timers = Timers} = S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed) ->
    %%
    %% Cancel event timeout
    %%
    case Timers of
	%% Optimization
	%% - only cancel timeout when it is active
	%%
        #{timeout := {TimerRef,_TimeoutMsg}} ->
	    %% Event timeout active
	    loop_next_events(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
	      cancel_timer(timeout, TimerRef, Timers));
	_ ->
	    %% No event timeout active
	    loop_next_events(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers)
    end.

%% State transition to a different state
%%
loop_state_change(
  P, Debug, S, Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed) ->
    %%
    %% Retry postponed events
    %%
    case Postponed of
        [] ->
            loop_state_change(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR);
        [E1] ->
            loop_state_change(
              P, Debug, S,
              [E1|Events], NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR);
        [E2,E1] ->
            loop_state_change(
              P, Debug, S,
              [E1,E2|Events], NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR);
        [_,_|_] ->
            loop_state_change(
              P, Debug, S,
              lists:reverse(Postponed, Events), NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR)
    end.
%%
loop_state_change(
  P, Debug, #state{timers = Timers} = S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR) ->
    %%
    %% Cancel state and event timeout
    %%
    case Timers of
	%% Optimization
	%% - only cancel timeout when it is active
	%%
	#{state_timeout := {TimerRef,_TimeoutMsg}} ->
	    %% State timeout active
            %% - cancel event timeout too since it is faster than inspecting
	    loop_next_events(
              P, Debug, S, Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, [],
              cancel_timer(
                timeout,
                cancel_timer(state_timeout, TimerRef, Timers)));
        #{timeout := {TimerRef,_TimeoutMsg}} ->
            %% Event timeout active but not state timeout
            %% - cancel event timeout only
            loop_next_events(
              P, Debug, S, Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, [],
              cancel_timer(timeout, TimerRef, Timers));
        _ ->
            %% No state nor event timeout active.
            loop_next_events(
              P, Debug, S, Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, [],
              Timers)
    end.

%% Continue state transition with processing of
%% timeouts and inserted events
%%
loop_next_events(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, [], Postponed,
  Timers) ->
    %%
    %% Optimization when there are no timeouts
    %% hence no zero timeout events to append to Events
    %% - avoid loop_timeouts
    loop_done(
      P, Debug,
      S#state{
        state_data = NextState_NewData,
	postponed = Postponed,
        timers = Timers,
	hibernate = Hibernate},
      Events, NextEventsR);
loop_next_events(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers) ->
    %%
    Seen = #{},
    TimeoutEvents = [],
    loop_timeouts(
      P, Debug, S,
      Events, NextState_NewData,
      NextEventsR, Hibernate, TimeoutsR, Postponed,
      Timers, Seen, TimeoutEvents).

%% Continue state transition with processing of timeouts
%% and finally inserted events
%%
loop_timeouts(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, [], Postponed,
  Timers, _Seen, TimeoutEvents) ->
    %%
    %% End of timeouts
    %%
    case TimeoutEvents of
        [] ->
            S_1 =
                S#state{
                  state_data = NextState_NewData,
                  postponed = Postponed,
                  timers = Timers,
                  hibernate = Hibernate},
            loop_done(P, Debug, S_1, Events, NextEventsR);
        [_|_] ->
            #{t0q := T0Q} = Timers,
            S_1 =
                S#state{
                  state_data = NextState_NewData,
                  postponed = Postponed,
                  timers = Timers#{t0q := T0Q ++ TimeoutEvents},
                  hibernate = Hibernate},
            loop_done(P, Debug, S_1, Events, NextEventsR)
    end;
loop_timeouts(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, [Timeout|TimeoutsR], Postponed,
  Timers, Seen, TimeoutEvents) ->
    %%
    TimeoutType = element(1, Timeout),
    case Seen of
        #{TimeoutType := _} ->
            %% Type seen before - ignore
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents);
        #{} ->
            case Timeout of
                {_,Time,TimeoutMsg} ->
                    %% Relative timeout or update
                    loop_timeouts_start(
                      P, Debug, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed,
                      Timers, Seen, TimeoutEvents,
                      TimeoutType, Time, TimeoutMsg, []);
                {_,Time,TimeoutMsg,TimeoutOpts} ->
                    %% Absolute timeout
                    loop_timeouts_start(
                      P, Debug, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed,
                      Timers, Seen, TimeoutEvents,
                      TimeoutType, Time, TimeoutMsg, listify(TimeoutOpts))
            end
    end.

%% Loop helper to start or restart a timeout
%%
loop_timeouts_start(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, Time, TimeoutMsg, TimeoutOpts) ->
    %%
    case Time of
        infinity ->
            %% Cancel any running timer
            loop_timeouts_cancel(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType);
        update ->
            loop_timeouts_update(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType, TimeoutMsg);
        0 ->
            %% (Re)start zero timeout
            TimerRef = 0,
            TimeoutEvents_1 = [TimeoutType | TimeoutEvents],
            loop_timeouts_register(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents_1,
              TimeoutType, Time, TimeoutMsg, TimeoutOpts, TimerRef);
        _ ->
            %% (Re)start the timer
            TimerRef =
                erlang:start_timer(Time, self(), TimeoutType, TimeoutOpts),
            loop_timeouts_register(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType, Time, TimeoutMsg, TimeoutOpts, TimerRef)
    end.

%% Loop helper to register a newly started timer
%% and to cancel any running timer
%%
loop_timeouts_register(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, Time, TimeoutMsg, TimeoutOpts, TimerRef) ->
    %%
    case Debug of
        ?not_sys_debug ->
            loop_timeouts_register(
              P, Debug, S, Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType, TimerRef, TimeoutMsg);
        _ ->
            {State,_Data} = NextState_NewData,
            Debug_1 =
                sys_debug(
                  Debug, P#params.name,
                  {start_timer,
                   {TimeoutType,Time,TimeoutMsg,TimeoutOpts},
                   State}),
            loop_timeouts_register(
              P, Debug_1, S, Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType, TimerRef, TimeoutMsg)
    end.
%%
loop_timeouts_register(
  P, Debug, S, Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, TimerRef, TimeoutMsg) ->
    %%
    case Timers of
        #{TimeoutType := {0,_OldTimeoutMsg},
          t0q := T0Q} ->
            %% Cancel the running timer,
            %% and update timer type and ref
            Timers_1 =
                Timers
                #{TimeoutType := {0,TimeoutMsg},
                  t0q := lists:delete(TimeoutType, T0Q)},
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true}, TimeoutEvents);
        #{TimeoutType := {OldTimerRef,_OldTimeoutMsg}} ->
            %% Cancel the running timer,
            %% and update timer type and ref
            cancel_timer(OldTimerRef),
            Timers_1 = Timers#{TimeoutType := {TimerRef,TimeoutMsg}},
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true}, TimeoutEvents);
        #{} ->
            %% Insert the new timer type and ref
            Timers_1 = Timers#{TimeoutType => {TimerRef,TimeoutMsg}},
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true}, TimeoutEvents)
    end.

%% Loop helper to cancel a timeout
%%
loop_timeouts_cancel(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents, TimeoutType) ->
    %% This function body should have been:
    %%    Timers_1 = cancel_timer(TimeoutType, Timers),
    %%    loop_timeouts(
    %%      P, Debug, S,
    %%      Events, NextState_NewData,
    %%      NextEventsR, Hibernate, TimeoutsR, Postponed,
    %%      Timers_1, Seen#{TimeoutType => true}, TimeoutEvents).
    %%
    %% Explicitly separate cases to get separate code paths for when
    %% the map key exists vs. not, since otherwise the external call
    %% to erlang:cancel_timer/1 and to maps:remove/2 within
    %% cancel_timer/2 would cause all live registers
    %% to be saved to and restored from the stack also for
    %% the case when the map key TimeoutType does not exist
    case Timers of
        #{TimeoutType := {TimerRef,_TimeoutMsg}} ->
            Timers_1 = cancel_timer(TimeoutType, TimerRef, Timers),
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true}, TimeoutEvents);
        #{} ->
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen#{TimeoutType => true}, TimeoutEvents)
    end.

%% Loop helper to update the timeout message,
%% or start a zero timeout if no timer is running
%%
loop_timeouts_update(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, TimeoutMsg) ->
    %%
    case Timers of
        #{TimeoutType := {TimerRef,_OldTimeoutMsg}} ->
            Timers_1 = Timers#{TimeoutType := {TimerRef,TimeoutMsg}},
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true},
              TimeoutEvents);
        #{} ->
            Timers_1 = Timers#{TimeoutType => {0, TimeoutMsg}},
            TimeoutEvents_1 = [TimeoutType|TimeoutEvents],
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true},
              TimeoutEvents_1)
    end.

%% Place inserted events first in the event queue
%%
loop_done(P, Debug, S, Events, NextEventsR) ->
    case NextEventsR of
        [] ->
            loop_done(P, Debug, S, Events);
        [E1] ->
            loop_done(P, Debug, S, [E1|Events]);
        [E2,E1] ->
            loop_done(P, Debug, S, [E1,E2|Events]);
        [_,_|_] ->
            loop_done(P, Debug, S, lists:reverse(NextEventsR, Events))
    end.
%%
%% State transition is done, keep looping if there are
%% enqueued events, or if there are zero timeouts,
%% otherwise get a new event
%%
loop_done(P, Debug, S, Q) ->
%%%    io:format(
%%%      "loop_done: state_data = ~p,~n"
%%%      "    postponed = ~p, Q = ~p,~n",
%%%      "    timers = ~p.~n"
%%%      [S#state.state_data,,S#state.postponed,Q,S#state.timers]),
    case Q of
        [] ->
            case S#state.timers of
                #{t0q := [TimeoutType|_]} = Timers ->
                    #{TimeoutType := {0 = TimerRef, TimeoutMsg}} = Timers,
                    Timers_1 = cancel_timer(TimeoutType, TimerRef, Timers),
                    S_1 = S#state{timers = Timers_1},
                    Event = {TimeoutType, TimeoutMsg},
                    loop_receive_result(P, Debug, S_1, Event);
                #{} ->
                    %% Get a new event
                    loop(P, Debug, S)
            end;
        [Event|Events] ->
	    %% Loop until out of enqueued events
	    loop_event(P, Debug, S, Event, Events)
    end.


%%---------------------------------------------------------------------------
%% Server loop helpers

%% Parse an option list for erlang:start_timer/4 to figure out
%% if the timeout will be absolute or relative
%%
parse_timeout_opts_abs(Opts) ->
    parse_timeout_opts_abs(Opts, false).
%%
parse_timeout_opts_abs(Opts, Abs) ->
    case Opts of
        [] ->
            Abs;
        [{abs,Abs_1}|Opts] when is_boolean(Abs_1) ->
            parse_timeout_opts_abs(Opts, Abs_1);
        _ ->
            badarg
    end.

%% Get the callback mode, update #params{}
%%
get_callback_mode(P, [Module | _] = Modules) ->
    try Module:callback_mode() of
        CallbackModeResult ->
            callback_mode_result(P, Modules, CallbackModeResult)
    catch
        CallbackModeResult ->
            callback_mode_result(P, Modules, CallbackModeResult);
        Class:Reason:Stacktrace ->
            {Class,Reason,Stacktrace}
    end.

callback_mode_result(P, Modules, CallbackModeResult) ->
    callback_mode_result(
      P, Modules, CallbackModeResult,
      listify(CallbackModeResult), undefined, false).
%%
callback_mode_result(
  P, Modules, CallbackModeResult,
  [H|T], CallbackMode, StateEnter) ->
    case callback_mode(H) of
        true ->
            callback_mode_result(
              P, Modules, CallbackModeResult,
              T, H, StateEnter);
        false ->
            case state_enter(H) of
                true ->
                    callback_mode_result(
                      P, Modules, CallbackModeResult,
                      T, CallbackMode, true);
                false ->
                    {error,
                     {bad_return_from_callback_mode, CallbackModeResult},
                     ?STACKTRACE()}
            end
    end;
callback_mode_result(
  P, Modules, CallbackModeResult,
  [], CallbackMode, StateEnter) ->
    if
        CallbackMode =:= undefined ->
            {error,
             {bad_return_from_callback_mode, CallbackModeResult},
             ?STACKTRACE()};
        true ->
            P#params{
              modules = Modules,
              callback_mode = params_callback_mode(CallbackMode, Modules),
              state_enter = StateEnter}
    end.

%%---------------------------------------------------------------------------
%% Server helpers

reply_then_terminate(Class, Reason, Stacktrace, P, Debug, S, Q, Replies) ->
    do_reply_then_terminate(
      Class, Reason, Stacktrace, P, Debug, S, Q, listify(Replies)).
%%
do_reply_then_terminate(
  Class, Reason, Stacktrace, P, Debug, S, Q, []) ->
    terminate(Class, Reason, Stacktrace, P, Debug, S, Q);
do_reply_then_terminate(
  Class, Reason, Stacktrace, P, Debug, S, Q, [R|Rs]) ->
    case R of
        {reply,From,Reply} ->
            case from(From) of
                true ->
                    reply(From, Reply),
                    Debug_1 =
                        ?sys_debug(
                           Debug,
                           P#params.name,
                           {out,Reply,From}),
                    do_reply_then_terminate(
                      Class, Reason, Stacktrace, P, Debug_1, S, Q, Rs);
                false ->
                    terminate(
                      error,
                      {bad_reply_action_from_state_function,R},
                      ?STACKTRACE(),
                      P, Debug, S, Q)
            end;
	_ ->
	    terminate(
	      error,
	      {bad_reply_action_from_state_function,R},
	      ?STACKTRACE(),
	      P, Debug, S, Q)
    end.

terminate(
  Class, Reason, Stacktrace,
  #params{modules = [Module | _]} = P, Debug,
  #state{state_data = {State,Data}} = S, Q) ->
    case erlang:function_exported(Module, terminate, 3) of
	true ->
	    try Module:terminate(Reason, State, Data) of
		_ -> ok
	    catch
		_ -> ok;
		C:R:ST ->
		    error_info(C, R, ST, Debug, P, S, Q),
		    erlang:raise(C, R, ST)
	    end;
	false ->
	    ok
    end,
    _ =
	case Reason of
	    normal ->
                terminate_sys_debug(Debug, P, State, Reason);
	    shutdown ->
                terminate_sys_debug(Debug, P, State, Reason);
	    {shutdown,_} ->
                terminate_sys_debug(Debug, P, State, Reason);
	    _ ->
		error_info(Class, Reason, Stacktrace, Debug, P, S, Q)
	end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	[_|_] ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

terminate_sys_debug(Debug, P, State, Reason) ->
    ?sys_debug(Debug, P#params.name, {terminate,Reason,State}).


error_info(
  Class, Reason, Stacktrace, Debug,
  #params{
     name = Name,
     modules = [Mod|_] = Modules,
     callback_mode = CallbackMode,
     state_enter = StateEnter},
  #state{
     postponed = Postponed,
     timers = Timers,
     state_data = {State,Data}},
  Q) ->

    {NumTimers,ListTimers} = list_timeouts(Timers),

    Status =
        gen:format_status(Mod, terminate,
                          #{ reason => Reason,
                             state => State,
                             data => Data,
                             queue => Q,
                             postponed => Postponed,
                             timeouts => ListTimers,
                             log => sys:get_log(Debug)},
                          [get(),State,Data]),
    NewState = case maps:find('$status', Status) of
                   error ->
                       {maps:get(state,Status),maps:get(data,Status)};
                   {ok, S} ->
                       S
               end,
    ?LOG_ERROR(#{label=>{gen_statem,terminate},
                 name=>Name,
                 queue=>maps:get(queue,Status),
                 postponed=>maps:get(postponed,Status),
                 modules=>Modules,
                 callback_mode=>CallbackMode,
                 state_enter=>StateEnter,
                 state=>NewState,
                 timeouts=>{NumTimers,maps:get(timeouts,Status)},
                 log=>maps:get(log,Status),
                 reason=>{Class,maps:get(reason,Status),Stacktrace},
                 client_info=>client_stacktrace(Q),
                 process_label=>proc_lib:get_label(self())},
               #{domain=>[otp],
                 report_cb=>fun gen_statem:format_log/2,
                 error_logger=>
                     #{tag=>error,
                       report_cb=>fun gen_statem:format_log/1}}).

client_stacktrace([]) ->
    undefined;
client_stacktrace([{{call,{Pid,_Tag}},_Req}|_]) when is_pid(Pid) ->
    if
        node(Pid) =:= node() ->
            case
                process_info(Pid, [current_stacktrace, registered_name])
            of
                undefined ->
                    {Pid,dead};
                [{current_stacktrace, Stacktrace},
                 {registered_name, []}] ->
                    {Pid,{Pid,Stacktrace}};
                [{current_stacktrace, Stacktrace},
                 {registered_name, Name}] ->
                    {Pid,{Name,Stacktrace}}
            end;
        true ->
            {Pid,remote}
    end;
client_stacktrace([_|_]) ->
    undefined.


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
limit_report(#{label:={gen_statem,terminate},
               queue:=Q,
               postponed:=Postponed,
               modules:=Modules,
               state:=FmtData,
	       timeouts:=Timeouts,
               log:=Log,
               reason:={Class,Reason,Stacktrace},
               client_info:=ClientInfo,
               process_label:=ProcessLabel}=Report,
             Depth) ->
    Report#{queue =>
                case Q of
                    [Event|Events] ->
                        [io_lib:limit_term(Event, Depth)
                         |io_lib:limit_term(Events, Depth)];
                    _ -> []
                end,
            postponed =>
                case Postponed of
                    [] -> [];
                    _ -> io_lib:limit_term(Postponed, Depth)
                end,
            modules => io_lib:limit_term(Modules, Depth),
            state => io_lib:limit_term(FmtData, Depth),
	    timeouts =>
	    	     case Timeouts of
                         {0,_} -> Timeouts;
                         _ -> io_lib:limit_term(Timeouts, Depth)
                     end,
            log =>
                case Log of
                    [] -> [];
                    _ -> [io_lib:limit_term(T, Depth) || T <- Log]
                end,
            reason =>
                {Class,
                 io_lib:limit_term(Reason, Depth),
                 io_lib:limit_term(Stacktrace, Depth)},
            client_info => limit_client_info(ClientInfo, Depth),
            process_label => io_lib:limit_term(ProcessLabel, Depth)}.


limit_client_info({Pid,{Name,Stacktrace}}, Depth) ->
    {Pid,{Name,io_lib:limit_term(Stacktrace, Depth)}};
limit_client_info(Client, _Depth) ->
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

format_log_single(#{label:={gen_statem,terminate},
                    name:=Name,
                    queue:=Q,
                    %% postponed
                    %% callback_mode
                    %% state_enter
                    state:=FmtData,
		    %% timeouts
                    log:=Log,
                    reason:={Class,Reason,Stacktrace},
                    client_info:=ClientInfo,
                    process_label:=ProcessLabel},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    {FixedReason,FixedStacktrace} = fix_reason(Class, Reason, Stacktrace),
    {ClientFmt,ClientArgs} = format_client_log_single(ClientInfo, P, Depth),
    Format =
        lists:append(
          ["State machine ",P," terminating",
           case ProcessLabel of
               undefined -> "";
               _ -> ". Label: "++P
           end,
           ". Reason: ",P,
           case FixedStacktrace of
               [] -> "";
               _ -> ". Stack: "++P
           end,
           case Q of
               [] -> "";
               _ -> ". Last event: "++P
           end,
           ". State: ",P,
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
        [FixedReason] ++
        case FixedStacktrace of
            [] -> [];
            _ -> [FixedStacktrace]
        end ++
        case Q of
            [] -> [];
            [Event|_] -> [Event]
        end ++
        [FmtData] ++
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
format_log_single(Report, FormatOpts) ->
    format_log_multi(Report, FormatOpts).

format_log_multi(#{label:={gen_statem,terminate},
                   name:=Name,
                   queue:=Q,
                   postponed:=Postponed,
                   modules:=Modules,
                   callback_mode:=CallbackMode,
                   state_enter:=StateEnter,
                   state:=FmtData,
		   timeouts:=Timeouts,
                   log:=Log,
                   reason:={Class,Reason,Stacktrace},
                   client_info:=ClientInfo,
                   process_label:=ProcessLabel},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    {FixedReason,FixedStacktrace} = fix_reason(Class, Reason, Stacktrace),
    {ClientFmt,ClientArgs} = format_client_log(ClientInfo, P, Depth),
    CBMode =
	 case StateEnter of
	     true ->
		 [CallbackMode,state_enter];
	     false ->
		 CallbackMode
	 end,
    Format =
        lists:append(
          ["** State machine ",P," terminating~n",
           case ProcessLabel of
               undefined -> "";
               _ -> "** Process label = "++P++"~n"
           end,
           case Q of
               [] -> "";
               _ -> "** Last event = "++P++"~n"
           end,
           "** When server state  = ",P,"~n",
           "** Reason for termination = ",P,":",P,"~n",
           "** Callback modules = ",P,"~n",
           "** Callback mode = ",P,"~n",
           case Q of
               [_,_|_] -> "** Queued = "++P++"~n";
               _ -> ""
           end,
           case Postponed of
               [] -> "";
               _ -> "** Postponed = "++P++"~n"
           end,
           case FixedStacktrace of
               [] -> "";
               _ -> "** Stacktrace =~n**  "++P++"~n"
           end,
           case Timeouts of
               {0,_} -> "";
               _ -> "** Time-outs: "++P++"~n"
           end,
           case Log of
               [] -> "";
               _ -> "** Log =~n**  "++P++"~n"
           end]),
    Args0 =
        [Name] ++
        case ProcessLabel of
            undefined -> [];
            _ -> [ProcessLabel]
        end ++
        case Q of
            [] -> [];
            [Event|_] -> [Event]
        end ++
        [FmtData,
         Class,FixedReason,
         Modules,
         CBMode] ++
        case Q of
            [_|[_|_] = Events] -> [Events];
            _ -> []
        end ++
        case Postponed of
            [] -> [];
            _ -> [Postponed]
        end ++
        case FixedStacktrace of
            [] -> [];
            _ -> [FixedStacktrace]
        end  ++
        case Timeouts of
            {0,_} -> [];
            _ -> [Timeouts]
        end ++
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
    {Format++ClientFmt,Args++ClientArgs}.

fix_reason(Class, Reason, Stacktrace) ->
    case Stacktrace of
        [{M,F,Args,_}|ST]
          when Class =:= error, Reason =:= undef ->
            case code:is_loaded(M) of
                false ->
                    {{'module could not be loaded',M},ST};
                _ ->
                    Arity =
                        if
                            is_list(Args) ->
                                length(Args);
                            is_integer(Args) ->
                                Args
                        end,
                    case erlang:function_exported(M, F, Arity) of
                        true ->
                            {Reason,Stacktrace};
                        false ->
                            {{'function not exported',{M,F,Arity}},ST}
                    end
            end;
        _ -> {Reason,Stacktrace}
    end.

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


-compile({inline, [listify/1]}).
listify(Item) when is_list(Item) ->
    Item;
listify(Item) ->
    [Item].


-define(
   cancel_timer(TimerRef),
   case erlang:cancel_timer(TimerRef) of
       false ->
           %% No timer found and we have not seen the timeout message
           receive
               {timeout,(TimerRef),_} ->
                   ok
           end;
       _ ->
           %% Timer was running
           ok
   end).
%%
%% Cancel erlang: timer and consume timeout message
%%
-compile({inline, [cancel_timer/1]}).
cancel_timer(TimerRef) ->
    ?cancel_timer(TimerRef).


-define(
   cancel_timer(TimeoutType, TimerRef, Timers),
   case (TimerRef) of
       0 ->
           maps:remove(
             begin TimeoutType end,
             maps:update(
               t0q,
               lists:delete(
                 begin TimeoutType end,
                 maps:get(t0q, begin Timers end)),
               begin Timers end));
       _ ->
           ?cancel_timer(TimerRef),
           maps:remove(begin TimeoutType end, begin Timers end)
   end).
%%
%% Cancel timer and remove from Timers
%%
-compile({inline, [cancel_timer/3]}).
cancel_timer(TimeoutType, TimerRef, Timers) ->
    ?cancel_timer(TimeoutType, TimerRef, Timers).

%% Cancel timer if running, otherwise no op
%%
%% Remove the timer from Timers
-compile({inline, [cancel_timer/2]}).
cancel_timer(TimeoutType, Timers) ->
    case Timers of
        #{TimeoutType := {TimerRef, _TimeoutMsg}} ->
            ?cancel_timer(TimeoutType, TimerRef, Timers);
        #{} ->
            Timers
    end.


%% Return a list of all pending timeouts
list_timeouts(Timers) ->
    {maps:size(Timers) - 1, % Subtract fixed key 't0q'
     [{TimeoutType, TimeoutMsg}
      || TimeoutType := {_TimerRef, TimeoutMsg} <- Timers,
         TimeoutType =/= t0q]}.
