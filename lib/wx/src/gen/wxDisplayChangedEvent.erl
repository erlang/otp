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

-module(wxDisplayChangedEvent).
-moduledoc """
Functions for wxDisplayChangedEvent class

A display changed event is sent to top-level windows when the display resolution
has changed.

This event is currently emitted under Windows only.

Only for:wxmsw

See: `m:wxDisplay`

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxDisplayChangedEvent](https://docs.wxwidgets.org/3.1/classwx_display_changed_event.html)

## Events

Use `wxEvtHandler:connect/3` with
[`wxDisplayChangedEventType`](`t:wxDisplayChangedEventType/0`) to subscribe to
events of this type.
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxDisplayChangedEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxDisplayChangedEventType() :: 'display_changed'.
-export_type([wxDisplayChangedEvent/0, wxDisplayChanged/0, wxDisplayChangedEventType/0]).
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
