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

-module(wxFocusEvent).
-moduledoc """
Functions for wxFocusEvent class

A focus event is sent when a window's focus changes. The window losing focus
receives a "kill focus" event while the window gaining it gets a "set focus"
one.

Notice that the set focus event happens both when the user gives focus to the
window (whether using the mouse or keyboard) and when it is done from the
program itself using `wxWindow:setFocus/1`.

The focus event handlers should almost invariably call `wxEvent:skip/2` on their
event argument to allow the default handling to take place. Failure to do this
may result in incorrect behaviour of the native controls. Also note that
wxEVT_KILL_FOCUS handler must not call `wxWindow:setFocus/1` as this, again, is
not supported by all native controls. If you need to do this, consider using the
`Delayed Action Mechanism` (not implemented in wx) described in `m:wxIdleEvent`
documentation.

See:
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events)

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxFocusEvent](https://docs.wxwidgets.org/3.1/classwx_focus_event.html)

## Events

Use `wxEvtHandler:connect/3` with [`wxFocusEventType`](`t:wxFocusEventType/0`)
to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getWindow/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxFocusEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxFocusEventType() :: 'set_focus' | 'kill_focus'.
-export_type([wxFocusEvent/0, wxFocus/0, wxFocusEventType/0]).
%% @hidden
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfocusevent.html#wxfocuseventgetwindow">external documentation</a>.
-doc """
Returns the window associated with this event, that is the window which had the
focus before for the `wxEVT_SET_FOCUS` event and the window which is going to
receive focus for the `wxEVT_KILL_FOCUS` one.

Warning: the window pointer may be NULL\!
""".
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxFocusEvent().
getWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFocusEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxFocusEvent_GetWindow),
  wxe_util:rec(?wxFocusEvent_GetWindow).

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
