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

-module(wxScrollWinEvent).
-moduledoc """
A scroll event holds information about events sent from scrolling windows.

Note that you can use the EVT_SCROLLWIN* macros for intercepting scroll window events
from the receiving window.

See:
* `m:wxScrollEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxScrollWinEvent](https://docs.wxwidgets.org/3.2/classwx_scroll_win_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxScrollWinEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getOrientation/1,getPosition/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxScrollWinEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxScrollWinEventType() :: 'scrollwin_top' | 'scrollwin_bottom' | 'scrollwin_lineup' | 'scrollwin_linedown' | 'scrollwin_pageup' | 'scrollwin_pagedown' | 'scrollwin_thumbtrack' | 'scrollwin_thumbrelease'.
-export_type([wxScrollWinEvent/0, wxScrollWin/0, wxScrollWinEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Returns wxHORIZONTAL or wxVERTICAL, depending on the orientation of the scrollbar.".
-spec getOrientation(This) -> integer() when
	This::wxScrollWinEvent().
getOrientation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxScrollWinEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxScrollWinEvent_GetOrientation),
  wxe_util:rec(?wxScrollWinEvent_GetOrientation).

-doc """
Returns the position of the scrollbar for the thumb track and release events.

Note that this field can't be used for the other events, you need to query the window
itself for the current position in that case.
""".
-spec getPosition(This) -> integer() when
	This::wxScrollWinEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxScrollWinEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxScrollWinEvent_GetPosition),
  wxe_util:rec(?wxScrollWinEvent_GetPosition).

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
