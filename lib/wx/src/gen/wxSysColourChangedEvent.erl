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

-module(wxSysColourChangedEvent).
-moduledoc """
Functions for wxSysColourChangedEvent class

This class is used for system colour change events, which are generated when the
user changes the colour settings using the control panel. This is only
appropriate under Windows.

Remark: The default event handler for this event propagates the event to child
windows, since Windows only sends the events to top-level windows. If
intercepting this event for a top-level window, remember to call the base class
handler, or to pass the event on to the window's children explicitly.

See:
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events)

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxSysColourChangedEvent](https://docs.wxwidgets.org/3.1/classwx_sys_colour_changed_event.html)

## Events

Use `wxEvtHandler:connect/3` with
[`wxSysColourChangedEventType`](`t:wxSysColourChangedEventType/0`) to subscribe
to events of this type.
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxSysColourChangedEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxSysColourChangedEventType() :: 'sys_colour_changed'.
-export_type([wxSysColourChangedEvent/0, wxSysColourChanged/0, wxSysColourChangedEventType/0]).
%% @hidden
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

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
