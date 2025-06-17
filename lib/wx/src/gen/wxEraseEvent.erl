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

-module(wxEraseEvent).
-moduledoc """
An erase event is sent when a window's background needs to be repainted.

On some platforms, such as GTK+, this event is simulated (simply generated just before
the paint event) and may cause flicker. It is therefore recommended that you set the text
background colour explicitly in order to prevent flicker. The default background colour
under GTK+ is grey.

To intercept this event, use the EVT_ERASE_BACKGROUND macro in an event table definition.

You must use the device context returned by `getDC/1` to draw on, don't create a `m:wxPaintDC` in
the event handler.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxEraseEvent](https://docs.wxwidgets.org/3.2/classwx_erase_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxEraseEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getDC/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxEraseEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxEraseEventType() :: 'erase_background'.
-export_type([wxEraseEvent/0, wxErase/0, wxEraseEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the device context associated with the erase event to draw on.

The returned pointer is never NULL.
""".
-spec getDC(This) -> wxDC:wxDC() when
	This::wxEraseEvent().
getDC(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEraseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEraseEvent_GetDC),
  wxe_util:rec(?wxEraseEvent_GetDC).

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
