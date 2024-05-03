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

-module(wxChildFocusEvent).
-moduledoc """
Functions for wxChildFocusEvent class

A child focus event is sent to a (parent-)window when one of its child windows
gains focus, so that the window could restore the focus back to its
corresponding child if it loses it now and regains later.

Notice that child window is the direct child of the window receiving event. Use
`wxWindow:findFocus/0` to retrieve the window which is actually getting focus.

See:
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events)

This class is derived (and can use functions) from: `m:wxCommandEvent`
`m:wxEvent`

wxWidgets docs:
[wxChildFocusEvent](https://docs.wxwidgets.org/3.1/classwx_child_focus_event.html)

## Events

Use `wxEvtHandler:connect/3` with
[`wxChildFocusEventType`](`t:wxChildFocusEventType/0`) to subscribe to events of
this type.
""".
-include("wxe.hrl").
-export([getWindow/1]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxChildFocusEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxChildFocusEventType() :: 'child_focus'.
-export_type([wxChildFocusEvent/0, wxChildFocus/0, wxChildFocusEventType/0]).
%% @hidden
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxchildfocusevent.html#wxchildfocuseventgetwindow">external documentation</a>.
-doc """
Returns the direct child which receives the focus, or a (grand-)parent of the
control receiving the focus.

To get the actually focused control use `wxWindow:findFocus/0`.
""".
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxChildFocusEvent().
getWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxChildFocusEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxChildFocusEvent_GetWindow),
  wxe_util:rec(?wxChildFocusEvent_GetWindow).

 %% From wxCommandEvent
%% @hidden
-doc false.
setString(This,String) -> wxCommandEvent:setString(This,String).
%% @hidden
-doc false.
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
%% @hidden
-doc false.
isSelection(This) -> wxCommandEvent:isSelection(This).
%% @hidden
-doc false.
isChecked(This) -> wxCommandEvent:isChecked(This).
%% @hidden
-doc false.
getString(This) -> wxCommandEvent:getString(This).
%% @hidden
-doc false.
getSelection(This) -> wxCommandEvent:getSelection(This).
%% @hidden
-doc false.
getInt(This) -> wxCommandEvent:getInt(This).
%% @hidden
-doc false.
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
%% @hidden
-doc false.
getClientData(This) -> wxCommandEvent:getClientData(This).
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
