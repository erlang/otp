%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(wxMouseCaptureLostEvent).
-moduledoc """
A mouse capture lost event is sent to a window that had obtained mouse capture, which was
subsequently lost due to an "external" event (for example, when a dialog box is shown or
if another application captures the mouse).

If this happens, this event is sent to all windows that are on the capture stack (i.e.
called CaptureMouse, but didn't call ReleaseMouse yet). The event is not sent if the
capture changes because of a call to CaptureMouse or ReleaseMouse.

This event is currently emitted under Windows only.

See:
* `m:wxMouseCaptureChangedEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxWindow:captureMouse/1`

* `wxWindow:releaseMouse/1`

* `wxWindow:getCapture/0`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxMouseCaptureLostEvent](https://docs.wxwidgets.org/3.2/classwx_mouse_capture_lost_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxMouseCaptureLostEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxMouseCaptureLostEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxMouseCaptureLostEventType() :: 'mouse_capture_lost'.
-export_type([wxMouseCaptureLostEvent/0, wxMouseCaptureLost/0, wxMouseCaptureLostEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

 %% From wxEvent
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
-doc false.
skip(This) -> wxEvent:skip(This).
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
-doc false.
getId(This) -> wxEvent:getId(This).
