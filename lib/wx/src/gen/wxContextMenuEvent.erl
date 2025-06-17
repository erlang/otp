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

-module(wxContextMenuEvent).
-moduledoc """
This class is used for context menu events, sent to give the application a chance to show
a context (popup) menu for a `m:wxWindow`.

Note that if `getPosition/1` returns wxDefaultPosition, this means that the event originated from a
keyboard context button event, and you should compute a suitable position yourself, for
example by calling `wx_misc:getMousePosition/0`.

Notice that the exact sequence of mouse events is different across the platforms. For
example, under MSW the context menu event is generated after `EVT_RIGHT_UP` event and only
if it was not handled but under GTK the context menu event is generated after `EVT_RIGHT_DOWN`
event. This is correct in the sense that it ensures that the context menu is shown
according to the current platform UI conventions and also means that you must not handle
(or call `wxEvent:skip/2` in your handler if you do have one) neither right mouse down nor right mouse up
event if you plan on handling `EVT_CONTEXT_MENU` event.

See:
* `m:wxCommandEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxContextMenuEvent](https://docs.wxwidgets.org/3.2/classwx_context_menu_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxContextMenuEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getPosition/1,setPosition/2]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxContextMenuEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxContextMenuEventType() :: 'context_menu'.
-export_type([wxContextMenuEvent/0, wxContextMenu/0, wxContextMenuEventType/0]).
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the position in screen coordinates at which the menu should be shown.

Use `wxWindow:screenToClient/2` to convert to client coordinates.

You can also omit a position from `wxWindow:popupMenu/4` in order to use the current mouse pointer position.

If the event originated from a keyboard event, the value returned from this function will
be wxDefaultPosition.
""".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxContextMenuEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxContextMenuEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxContextMenuEvent_GetPosition),
  wxe_util:rec(?wxContextMenuEvent_GetPosition).

-doc "Sets the position at which the menu should be shown.".
-spec setPosition(This, Point) -> 'ok' when
	This::wxContextMenuEvent(), Point::{X::integer(), Y::integer()}.
setPosition(#wx_ref{type=ThisT}=This,{PointX,PointY} = Point)
 when is_integer(PointX),is_integer(PointY) ->
  ?CLASS(ThisT,wxContextMenuEvent),
  wxe_util:queue_cmd(This,Point,?get_env(),?wxContextMenuEvent_SetPosition).

 %% From wxCommandEvent
-doc false.
setString(This,String) -> wxCommandEvent:setString(This,String).
-doc false.
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
-doc false.
isSelection(This) -> wxCommandEvent:isSelection(This).
-doc false.
isChecked(This) -> wxCommandEvent:isChecked(This).
-doc false.
getString(This) -> wxCommandEvent:getString(This).
-doc false.
getSelection(This) -> wxCommandEvent:getSelection(This).
-doc false.
getInt(This) -> wxCommandEvent:getInt(This).
-doc false.
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
-doc false.
getClientData(This) -> wxCommandEvent:getClientData(This).
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
