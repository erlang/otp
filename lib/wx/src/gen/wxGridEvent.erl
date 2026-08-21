%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxGridEvent).
-moduledoc """
This event class contains information about various grid events.

Notice that all grid event table macros are available in two versions: `EVT_GRID_XXX` and `EVT_GRID_CMD_XXX`.
The only difference between the two is that the former doesn't allow to specify the grid
window identifier and so takes a single parameter, the event handler, but is not suitable
if there is more than one grid control in the window where the event table is used (as it
would catch the events from all the grids). The version with `CMD` takes the id as first
argument and the event handler as the second one and so can be used with multiple grids as
well. Otherwise there are no difference between the two and only the versions without the
id are documented below for brevity.

This class is derived, and can use functions, from:

* `m:wxNotifyEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxGridEvent](https://docs.wxwidgets.org/3.2/classwx_grid_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxGridEventType` to subscribe to events of this type.
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
-doc false.
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Returns true if the Alt key was down at the time of the event.".
-spec altDown(This) -> boolean() when
	This::wxGridEvent().
altDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_AltDown),
  wxe_util:rec(?wxGridEvent_AltDown).

-doc "Returns true if the Control key was down at the time of the event.".
-spec controlDown(This) -> boolean() when
	This::wxGridEvent().
controlDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_ControlDown),
  wxe_util:rec(?wxGridEvent_ControlDown).

-doc """
Column at which the event occurred.

Notice that for a `wxEVT_GRID_SELECT_CELL` event this column is the column of the newly
selected cell while the previously selected cell can be retrieved using `wxGrid:getGridCursorCol/1`.
""".
-spec getCol(This) -> integer() when
	This::wxGridEvent().
getCol(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetCol),
  wxe_util:rec(?wxGridEvent_GetCol).

-doc "Position in pixels at which the event occurred.".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxGridEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetPosition),
  wxe_util:rec(?wxGridEvent_GetPosition).

-doc """
Row at which the event occurred.

Notice that for a `wxEVT_GRID_SELECT_CELL` event this row is the row of the newly
selected cell while the previously selected cell can be retrieved using `wxGrid:getGridCursorRow/1`.
""".
-spec getRow(This) -> integer() when
	This::wxGridEvent().
getRow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetRow),
  wxe_util:rec(?wxGridEvent_GetRow).

-doc "Returns true if the Meta key was down at the time of the event.".
-spec metaDown(This) -> boolean() when
	This::wxGridEvent().
metaDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_MetaDown),
  wxe_util:rec(?wxGridEvent_MetaDown).

-doc "Returns true if the user is selecting grid cells, or false if deselecting.".
-spec selecting(This) -> boolean() when
	This::wxGridEvent().
selecting(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_Selecting),
  wxe_util:rec(?wxGridEvent_Selecting).

-doc "Returns true if the Shift key was down at the time of the event.".
-spec shiftDown(This) -> boolean() when
	This::wxGridEvent().
shiftDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_ShiftDown),
  wxe_util:rec(?wxGridEvent_ShiftDown).

 %% From wxNotifyEvent
-doc false.
veto(This) -> wxNotifyEvent:veto(This).
-doc false.
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
-doc false.
allow(This) -> wxNotifyEvent:allow(This).
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
