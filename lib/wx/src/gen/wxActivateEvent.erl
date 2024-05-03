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

-module(wxActivateEvent).
-moduledoc """
Functions for wxActivateEvent class

An activate event is sent when a window or application is being activated or
deactivated.

Note: Until wxWidgets 3.1.0 activation events could be sent by wxMSW when the
window was minimized. This reflected the native MSW behaviour but was often
surprising and unexpected, so starting from 3.1.0 such events are not sent any
more when the window is in the minimized state.

See:
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events),
`wxApp::IsActive` (not implemented in wx)

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxActivateEvent](https://docs.wxwidgets.org/3.1/classwx_activate_event.html)

## Events

Use `wxEvtHandler:connect/3` with
[`wxActivateEventType`](`t:wxActivateEventType/0`) to subscribe to events of
this type.
""".
-include("wxe.hrl").
-export([getActive/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxActivateEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxActivateEventType() :: 'activate' | 'activate_app' | 'hibernate'.
-export_type([wxActivateEvent/0, wxActivate/0, wxActivateEventType/0]).
%% @hidden
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxactivateevent.html#wxactivateeventgetactive">external documentation</a>.
-doc "Returns true if the application or window is being activated, false otherwise.".
-spec getActive(This) -> boolean() when
	This::wxActivateEvent().
getActive(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxActivateEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxActivateEvent_GetActive),
  wxe_util:rec(?wxActivateEvent_GetActive).

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
