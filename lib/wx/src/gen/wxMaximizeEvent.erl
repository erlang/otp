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

-module(wxMaximizeEvent).
-moduledoc """
An event being sent when a top level window is maximized.

Notice that it is not sent when the window is restored to its original size after it had
been maximized, only a normal `m:wxSizeEvent` is generated in this case.

Currently this event is only generated in wxMSW, wxGTK and wxOSX/Cocoa ports so portable
programs should only rely on receiving `wxEVT_SIZE` and not necessarily this event when
the window is maximized.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxTopLevelWindow:maximize/2`

* `wxTopLevelWindow:isMaximized/1`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxMaximizeEvent](https://docs.wxwidgets.org/3.2/classwx_maximize_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxMaximizeEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxMaximizeEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxMaximizeEventType() :: 'maximize'.
-export_type([wxMaximizeEvent/0, wxMaximize/0, wxMaximizeEventType/0]).
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
