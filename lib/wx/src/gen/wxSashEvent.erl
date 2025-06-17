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

-module(wxSashEvent).
-moduledoc """
A sash event is sent when the sash of a `m:wxSashWindow` has been dragged by the user.

Remark: When a sash belonging to a sash window is dragged by the user, and then released,
this event is sent to the window, where it may be processed by an event table entry in a
derived class, a plug-in event handler or an ancestor class. Note that the `m:wxSashWindow`
doesn't change the window's size itself. It relies on the application's event handler to
do that. This is because the application may have to handle other consequences of the
resize, or it may wish to veto it altogether. The event handler should look at the drag
rectangle: see `getDragRect/1` to see what the new size of the window would be if the resize were to be
applied. It should also call `getDragStatus/1` to see whether the drag was OK or out of the current allowed range.

See:
* `m:wxSashWindow`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxSashEvent](https://docs.wxwidgets.org/3.2/classwx_sash_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxSashEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getDragRect/1,getDragStatus/1,getEdge/1]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxSashEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxSashEventType() :: 'sash_dragged'.
-export_type([wxSashEvent/0, wxSash/0, wxSashEventType/0]).
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the dragged edge.

The return value is one of wxSASH_TOP, wxSASH_RIGHT, wxSASH_BOTTOM, wxSASH_LEFT.
""".
%%  Res = ?wxSASH_TOP | ?wxSASH_RIGHT | ?wxSASH_BOTTOM | ?wxSASH_LEFT | ?wxSASH_NONE
-spec getEdge(This) -> wx:wx_enum() when
	This::wxSashEvent().
getEdge(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSashEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSashEvent_GetEdge),
  wxe_util:rec(?wxSashEvent_GetEdge).

-doc """
Returns the rectangle representing the new size the window would be if the resize was
applied.

It is up to the application to set the window size if required.
""".
-spec getDragRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxSashEvent().
getDragRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSashEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSashEvent_GetDragRect),
  wxe_util:rec(?wxSashEvent_GetDragRect).

-doc """
Returns the status of the sash: one of wxSASH\_STATUS\_OK,
wxSASH\_STATUS\_OUT\_OF\_RANGE.

If the drag caused the notional bounding box of the window to flip over, for example, the
drag will be out of rage.
""".
%%  Res = ?wxSASH_STATUS_OK | ?wxSASH_STATUS_OUT_OF_RANGE
-spec getDragStatus(This) -> wx:wx_enum() when
	This::wxSashEvent().
getDragStatus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSashEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSashEvent_GetDragStatus),
  wxe_util:rec(?wxSashEvent_GetDragStatus).

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
