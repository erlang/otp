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

-module(wxShowEvent).
-moduledoc """
Functions for wxShowEvent class

An event being sent when the window is shown or hidden. The event is triggered
by calls to `wxWindow:show/2`, and any user action showing a previously hidden
window or vice versa (if allowed by the current platform and/or window manager).
Notice that the event is not triggered when the application is iconized
(minimized) or restored under wxMSW.

See:
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events),
`wxWindow:show/2`, `wxWindow:isShown/1`

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxShowEvent](https://docs.wxwidgets.org/3.1/classwx_show_event.html)

## Events

Use `wxEvtHandler:connect/3` with [`wxShowEventType`](`t:wxShowEventType/0`) to
subscribe to events of this type.
""".
-include("wxe.hrl").
-export([isShown/1,setShow/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxShowEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxShowEventType() :: 'show'.
-export_type([wxShowEvent/0, wxShow/0, wxShowEventType/0]).
%% @hidden
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxshowevent.html#wxshoweventsetshow">external documentation</a>.
-doc "Set whether the windows was shown or hidden.".
-spec setShow(This, Show) -> 'ok' when
	This::wxShowEvent(), Show::boolean().
setShow(#wx_ref{type=ThisT}=This,Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxShowEvent),
  wxe_util:queue_cmd(This,Show,?get_env(),?wxShowEvent_SetShow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxshowevent.html#wxshoweventisshown">external documentation</a>.
-doc "Return true if the window has been shown, false if it has been hidden.".
-spec isShown(This) -> boolean() when
	This::wxShowEvent().
isShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxShowEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxShowEvent_IsShown),
  wxe_util:rec(?wxShowEvent_IsShown).

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
