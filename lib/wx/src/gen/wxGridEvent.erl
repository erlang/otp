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

-module(wxGridEvent).
-moduledoc """
Functions for wxGridEvent class

This event class contains information about various grid events.

Notice that all grid event table macros are available in two versions:
`EVT_GRID_XXX` and `EVT_GRID_CMD_XXX`. The only difference between the two is
that the former doesn't allow to specify the grid window identifier and so takes
a single parameter, the event handler, but is not suitable if there is more than
one grid control in the window where the event table is used (as it would catch
the events from all the grids). The version with `CMD` takes the id as first
argument and the event handler as the second one and so can be used with
multiple grids as well. Otherwise there are no difference between the two and
only the versions without the id are documented below for brevity.

This class is derived (and can use functions) from: `m:wxNotifyEvent`
`m:wxCommandEvent` `m:wxEvent`

wxWidgets docs:
[wxGridEvent](https://docs.wxwidgets.org/3.1/classwx_grid_event.html)

## Events

Use `wxEvtHandler:connect/3` with [`wxGridEventType`](`t:wxGridEventType/0`) to
subscribe to events of this type.
""".
-include("wxe.hrl").
-export([altDown/1,controlDown/1,getCol/1,getPosition/1,getRow/1,metaDown/1,
  selecting/1,shiftDown/1]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,
  getSkipped/1,getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-type wxGridEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxGridEventType() :: 'grid_cell_left_click' | 'grid_cell_right_click' | 'grid_cell_left_dclick' | 'grid_cell_right_dclick' | 'grid_label_left_click' | 'grid_label_right_click' | 'grid_label_left_dclick' | 'grid_label_right_dclick' | 'grid_cell_changed' | 'grid_select_cell' | 'grid_cell_begin_drag' | 'grid_editor_shown' | 'grid_editor_hidden' | 'grid_col_move' | 'grid_col_sort' | 'grid_tabbing'.
-export_type([wxGridEvent/0, wxGrid/0, wxGridEventType/0]).
%% @hidden
-doc false.
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventaltdown">external documentation</a>.
-doc "Returns true if the Alt key was down at the time of the event.".
-spec altDown(This) -> boolean() when
	This::wxGridEvent().
altDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_AltDown),
  wxe_util:rec(?wxGridEvent_AltDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventcontroldown">external documentation</a>.
-doc "Returns true if the Control key was down at the time of the event.".
-spec controlDown(This) -> boolean() when
	This::wxGridEvent().
controlDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_ControlDown),
  wxe_util:rec(?wxGridEvent_ControlDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetcol">external documentation</a>.
-doc """
Column at which the event occurred.

Notice that for a `wxEVT_GRID_SELECT_CELL` event this column is the column of
the newly selected cell while the previously selected cell can be retrieved
using `wxGrid:getGridCursorCol/1`.
""".
-spec getCol(This) -> integer() when
	This::wxGridEvent().
getCol(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetCol),
  wxe_util:rec(?wxGridEvent_GetCol).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetposition">external documentation</a>.
-doc "Position in pixels at which the event occurred.".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxGridEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetPosition),
  wxe_util:rec(?wxGridEvent_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetrow">external documentation</a>.
-doc """
Row at which the event occurred.

Notice that for a `wxEVT_GRID_SELECT_CELL` event this row is the row of the
newly selected cell while the previously selected cell can be retrieved using
`wxGrid:getGridCursorRow/1`.
""".
-spec getRow(This) -> integer() when
	This::wxGridEvent().
getRow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetRow),
  wxe_util:rec(?wxGridEvent_GetRow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventmetadown">external documentation</a>.
-doc "Returns true if the Meta key was down at the time of the event.".
-spec metaDown(This) -> boolean() when
	This::wxGridEvent().
metaDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_MetaDown),
  wxe_util:rec(?wxGridEvent_MetaDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventselecting">external documentation</a>.
-doc "Returns true if the user is selecting grid cells, or false if deselecting.".
-spec selecting(This) -> boolean() when
	This::wxGridEvent().
selecting(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_Selecting),
  wxe_util:rec(?wxGridEvent_Selecting).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventshiftdown">external documentation</a>.
-doc "Returns true if the Shift key was down at the time of the event.".
-spec shiftDown(This) -> boolean() when
	This::wxGridEvent().
shiftDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_ShiftDown),
  wxe_util:rec(?wxGridEvent_ShiftDown).

 %% From wxNotifyEvent
%% @hidden
-doc false.
veto(This) -> wxNotifyEvent:veto(This).
%% @hidden
-doc false.
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
%% @hidden
-doc false.
allow(This) -> wxNotifyEvent:allow(This).
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
