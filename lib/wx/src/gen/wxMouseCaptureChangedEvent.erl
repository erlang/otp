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

-module(wxMouseCaptureChangedEvent).
-moduledoc """
Functions for wxMouseCaptureChangedEvent class

An mouse capture changed event is sent to a window that loses its mouse capture.
This is called even if `wxWindow:releaseMouse/1` was called by the application
code. Handling this event allows an application to cater for unexpected capture
releases which might otherwise confuse mouse handling code.

Only for:wxmsw

See: `m:wxMouseCaptureLostEvent`,
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events),
`wxWindow:captureMouse/1`, `wxWindow:releaseMouse/1`, `wxWindow:getCapture/0`

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxMouseCaptureChangedEvent](https://docs.wxwidgets.org/3.1/classwx_mouse_capture_changed_event.html)

## Events

Use `wxEvtHandler:connect/3` with
[`wxMouseCaptureChangedEventType`](`t:wxMouseCaptureChangedEventType/0`) to
subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getCapturedWindow/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxMouseCaptureChangedEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxMouseCaptureChangedEventType() :: 'mouse_capture_changed'.
-export_type([wxMouseCaptureChangedEvent/0, wxMouseCaptureChanged/0, wxMouseCaptureChangedEventType/0]).
%% @hidden
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmousecapturechangedevent.html#wxmousecapturechangedeventgetcapturedwindow">external documentation</a>.
-doc """
Returns the window that gained the capture, or NULL if it was a non-wxWidgets
window.
""".
-spec getCapturedWindow(This) -> wxWindow:wxWindow() when
	This::wxMouseCaptureChangedEvent().
getCapturedWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseCaptureChangedEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseCaptureChangedEvent_GetCapturedWindow),
  wxe_util:rec(?wxMouseCaptureChangedEvent_GetCapturedWindow).

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
