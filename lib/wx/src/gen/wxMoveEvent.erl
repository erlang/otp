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

-module(wxMoveEvent).
-moduledoc """
A move event holds information about window position change.

These events are currently generated for top level (see `m:wxTopLevelWindow`) windows in
all ports, but are not generated for the child windows in wxGTK.

See:
* {X,Y}

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxMoveEvent](https://docs.wxwidgets.org/3.2/classwx_move_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxMoveEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getPosition/1,getRect/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxMoveEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxMoveEventType() :: 'move'.
-export_type([wxMoveEvent/0, wxMove/0, wxMoveEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Returns the position of the window generating the move change event.".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxMoveEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMoveEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMoveEvent_GetPosition),
  wxe_util:rec(?wxMoveEvent_GetPosition).

-doc "".
-spec getRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxMoveEvent().
getRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMoveEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMoveEvent_GetRect),
  wxe_util:rec(?wxMoveEvent_GetRect).

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
