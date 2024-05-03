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

-module(wxIdleEvent).
-moduledoc """
Functions for wxIdleEvent class

This class is used for idle events, which are generated when the system becomes
idle. Note that, unless you do something specifically, the idle events are not
sent if the system remains idle once it has become it, e.g. only a single idle
event will be generated until something else resulting in more normal events
happens and only then is the next idle event sent again.

If you need to ensure a continuous stream of idle events, you can either use
`requestMore/2` method in your handler or call ?wxWakeUpIdle() periodically (for
example from a timer event handler), but note that both of these approaches (and
especially the first one) increase the system load and so should be avoided if
possible.

By default, idle events are sent to all windows, including even the hidden ones
because they may be shown if some condition is met from their `wxEVT_IDLE` (or
related `wxEVT_UPDATE_UI`) handler. The children of hidden windows do not
receive idle events however as they can't change their state in any way
noticeable by the user. Finally, the global `wxApp` (not implemented in wx)
object also receives these events, as usual, so it can be used for any global
idle time processing.

If sending idle events to all windows is causing a significant overhead in your
application, you can call `setMode/1` with the value wxIDLE_PROCESS_SPECIFIED,
and set the wxWS_EX_PROCESS_IDLE extra window style for every window which
should receive idle events, all the other ones will not receive them in this
case.

Delayed Action Mechanism

`m:wxIdleEvent` can be used to perform some action "at slightly later time".
This can be necessary in several circumstances when, for whatever reason,
something can't be done in the current event handler. For example, if a mouse
event handler is called with the mouse button pressed, the mouse can be
currently captured and some operations with it - notably capturing it again -
might be impossible or lead to undesirable results. If you still want to capture
it, you can do it from `wxEVT_IDLE` handler when it is called the next time
instead of doing it immediately.

This can be achieved in two different ways: when using static event tables, you
will need a flag indicating to the (always connected) idle event handler whether
the desired action should be performed. The originally called handler would then
set it to indicate that it should indeed be done and the idle handler itself
would reset it to prevent it from doing the same action again.

Using dynamically connected event handlers things are even simpler as the
original event handler can simply `wxEvtHandler::Connect()` (not implemented in
wx) or `wxEvtHandler::Bind()` (not implemented in wx) the idle event handler
which would only be executed then and could `wxEvtHandler::Disconnect()` (not
implemented in wx) or `wxEvtHandler::Unbind()` (not implemented in wx) itself.

See:
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events),
`m:wxUpdateUIEvent`, `wxWindow::OnInternalIdle` (not implemented in wx)

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxIdleEvent](https://docs.wxwidgets.org/3.1/classwx_idle_event.html)

## Events

Use `wxEvtHandler:connect/3` with [`wxIdleEventType`](`t:wxIdleEventType/0`) to
subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getMode/0,moreRequested/1,requestMore/1,requestMore/2,setMode/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxIdleEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxIdleEventType() :: 'idle'.
-export_type([wxIdleEvent/0, wxIdle/0, wxIdleEventType/0]).
%% @hidden
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventgetmode">external documentation</a>.
%%<br /> Res = ?wxIDLE_PROCESS_ALL | ?wxIDLE_PROCESS_SPECIFIED
-doc """
Static function returning a value specifying how wxWidgets will send idle
events: to all windows, or only to those which specify that they will process
the events.

See: `setMode/1`
""".
-spec getMode() -> wx:wx_enum().
getMode() ->
  wxe_util:queue_cmd(?get_env(), ?wxIdleEvent_GetMode),
  wxe_util:rec(?wxIdleEvent_GetMode).

%% @equiv requestMore(This, [])
-spec requestMore(This) -> 'ok' when
	This::wxIdleEvent().

requestMore(This)
 when is_record(This, wx_ref) ->
  requestMore(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventrequestmore">external documentation</a>.
-doc """
Tells wxWidgets that more processing is required.

This function can be called by an OnIdle handler for a window or window event
handler to indicate that wxApp::OnIdle should forward the OnIdle event once more
to the application windows.

If no window calls this function during OnIdle, then the application will remain
in a passive event loop (not calling OnIdle) until a new event is posted to the
application by the windowing system.

See: `moreRequested/1`
""".
-spec requestMore(This, [Option]) -> 'ok' when
	This::wxIdleEvent(),
	Option :: {'needMore', boolean()}.
requestMore(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxIdleEvent),
  MOpts = fun({needMore, _needMore} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxIdleEvent_RequestMore).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventmorerequested">external documentation</a>.
-doc """
Returns true if the OnIdle function processing this event requested more
processing time.

See: `requestMore/2`
""".
-spec moreRequested(This) -> boolean() when
	This::wxIdleEvent().
moreRequested(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxIdleEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxIdleEvent_MoreRequested),
  wxe_util:rec(?wxIdleEvent_MoreRequested).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxidleevent.html#wxidleeventsetmode">external documentation</a>.
%%<br /> Mode = ?wxIDLE_PROCESS_ALL | ?wxIDLE_PROCESS_SPECIFIED
-doc """
Static function for specifying how wxWidgets will send idle events: to all
windows, or only to those which specify that they will process the events.
""".
-spec setMode(Mode) -> 'ok' when
	Mode::wx:wx_enum().
setMode(Mode)
 when is_integer(Mode) ->
  wxe_util:queue_cmd(Mode,?get_env(),?wxIdleEvent_SetMode).

 %% From wxEvent
%% @hidden
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
-doc false.
skip(This) -> wxEvent:skip(This).
%% @hidden
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
-doc false.
getId(This) -> wxEvent:getId(This).
