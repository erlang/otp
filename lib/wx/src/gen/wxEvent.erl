%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

-module(wxEvent).
-moduledoc """
Functions for wxEvent class

An event is a structure holding information about an event passed to a callback
or member function.

`m:wxEvent` used to be a multipurpose event object, and is an abstract base
class for other event classes (see below).

For more information about events, see the overview_events overview.

See: `m:wxCommandEvent`, `m:wxMouseEvent`

wxWidgets docs: [wxEvent](https://docs.wxwidgets.org/3.1/classwx_event.html)
""".
-include("wxe.hrl").
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,resumePropagation/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% inherited exports
-export([parent_class/1]).

-type wxEvent() :: wx:wx_object().
-export_type([wxEvent/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventgetid">external documentation</a>.
-doc "Returns the identifier associated with this event, such as a button command id.".
-spec getId(This) -> integer() when
	This::wxEvent().
getId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_GetId),
  wxe_util:rec(?wxEvent_GetId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventgetskipped">external documentation</a>.
-doc "Returns true if the event handler should be skipped, false otherwise.".
-spec getSkipped(This) -> boolean() when
	This::wxEvent().
getSkipped(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_GetSkipped),
  wxe_util:rec(?wxEvent_GetSkipped).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventgettimestamp">external documentation</a>.
-doc """
Gets the timestamp for the event.

The timestamp is the time in milliseconds since some fixed moment (not
necessarily the standard Unix Epoch, so only differences between the timestamps
and not their absolute values usually make sense).

Warning: wxWidgets returns a non-NULL timestamp only for mouse and key events
(see `m:wxMouseEvent` and `m:wxKeyEvent`).
""".
-spec getTimestamp(This) -> integer() when
	This::wxEvent().
getTimestamp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_GetTimestamp),
  wxe_util:rec(?wxEvent_GetTimestamp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventiscommandevent">external documentation</a>.
-doc """
Returns true if the event is or is derived from `m:wxCommandEvent` else it
returns false.

Note: exists only for optimization purposes.
""".
-spec isCommandEvent(This) -> boolean() when
	This::wxEvent().
isCommandEvent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_IsCommandEvent),
  wxe_util:rec(?wxEvent_IsCommandEvent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventresumepropagation">external documentation</a>.
-doc """
Sets the propagation level to the given value (for example returned from an
earlier call to `stopPropagation/1`).
""".
-spec resumePropagation(This, PropagationLevel) -> 'ok' when
	This::wxEvent(), PropagationLevel::integer().
resumePropagation(#wx_ref{type=ThisT}=This,PropagationLevel)
 when is_integer(PropagationLevel) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,PropagationLevel,?get_env(),?wxEvent_ResumePropagation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventshouldpropagate">external documentation</a>.
-doc """
Test if this event should be propagated or not, i.e. if the propagation level is
currently greater than 0.
""".
-spec shouldPropagate(This) -> boolean() when
	This::wxEvent().
shouldPropagate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_ShouldPropagate),
  wxe_util:rec(?wxEvent_ShouldPropagate).

%% @equiv skip(This, [])
-spec skip(This) -> 'ok' when
	This::wxEvent().

skip(This)
 when is_record(This, wx_ref) ->
  skip(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventskip">external documentation</a>.
-doc """
This method can be used inside an event handler to control whether further event
handlers bound to this event will be called after the current one returns.

Without `skip/2` (or equivalently if Skip(false) is used), the event will not be
processed any more. If Skip(true) is called, the event processing system
continues searching for a further handler function for this event, even though
it has been processed already in the current handler.

In general, it is recommended to skip all non-command events to allow the
default handling to take place. The command events are, however, normally not
skipped as usually a single command such as a button click or menu item
selection must only be processed by one handler.
""".
-spec skip(This, [Option]) -> 'ok' when
	This::wxEvent(),
	Option :: {'skip', boolean()}.
skip(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxEvent),
  MOpts = fun({skip, _skip} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxEvent_Skip).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventstoppropagation">external documentation</a>.
-doc """
Stop the event from propagating to its parent window.

Returns the old propagation level value which may be later passed to
`resumePropagation/2` to allow propagating the event again.
""".
-spec stopPropagation(This) -> integer() when
	This::wxEvent().
stopPropagation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_StopPropagation),
  wxe_util:rec(?wxEvent_StopPropagation).

