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

-module(wxIconizeEvent).
-moduledoc """
An event being sent when the frame is iconized (minimized) or restored.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxTopLevelWindow:iconize/2`

* `wxTopLevelWindow:isIconized/1`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxIconizeEvent](https://docs.wxwidgets.org/3.2/classwx_iconize_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxIconizeEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([isIconized/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxIconizeEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxIconizeEventType() :: 'iconize'.
-export_type([wxIconizeEvent/0, wxIconize/0, wxIconizeEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Returns true if the frame has been iconized, false if it has been restored.".
-spec isIconized(This) -> boolean() when
	This::wxIconizeEvent().
isIconized(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxIconizeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxIconizeEvent_IsIconized),
  wxe_util:rec(?wxIconizeEvent_IsIconized).

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
