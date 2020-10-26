%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
-type wxGridEventType() :: 'grid_cell_left_click' | 'grid_cell_right_click' | 'grid_cell_left_dclick' | 'grid_cell_right_dclick' | 'grid_label_left_click' | 'grid_label_right_click' | 'grid_label_left_dclick' | 'grid_label_right_dclick' | 'grid_row_size' | 'grid_col_size' | 'grid_range_select' | 'grid_cell_changed' | 'grid_select_cell' | 'grid_editor_shown' | 'grid_editor_hidden' | 'grid_editor_created' | 'grid_cell_begin_drag'.
-export_type([wxGridEvent/0, wxGrid/0, wxGridEventType/0]).
%% @hidden
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventaltdown">external documentation</a>.
-spec altDown(This) -> boolean() when
	This::wxGridEvent().
altDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_AltDown),
  wxe_util:rec(?wxGridEvent_AltDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventcontroldown">external documentation</a>.
-spec controlDown(This) -> boolean() when
	This::wxGridEvent().
controlDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_ControlDown),
  wxe_util:rec(?wxGridEvent_ControlDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetcol">external documentation</a>.
-spec getCol(This) -> integer() when
	This::wxGridEvent().
getCol(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetCol),
  wxe_util:rec(?wxGridEvent_GetCol).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxGridEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetPosition),
  wxe_util:rec(?wxGridEvent_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetrow">external documentation</a>.
-spec getRow(This) -> integer() when
	This::wxGridEvent().
getRow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_GetRow),
  wxe_util:rec(?wxGridEvent_GetRow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventmetadown">external documentation</a>.
-spec metaDown(This) -> boolean() when
	This::wxGridEvent().
metaDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_MetaDown),
  wxe_util:rec(?wxGridEvent_MetaDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventselecting">external documentation</a>.
-spec selecting(This) -> boolean() when
	This::wxGridEvent().
selecting(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_Selecting),
  wxe_util:rec(?wxGridEvent_Selecting).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventshiftdown">external documentation</a>.
-spec shiftDown(This) -> boolean() when
	This::wxGridEvent().
shiftDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxGridEvent_ShiftDown),
  wxe_util:rec(?wxGridEvent_ShiftDown).

 %% From wxNotifyEvent
%% @hidden
veto(This) -> wxNotifyEvent:veto(This).
%% @hidden
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
%% @hidden
allow(This) -> wxNotifyEvent:allow(This).
 %% From wxCommandEvent
%% @hidden
setString(This,String) -> wxCommandEvent:setString(This,String).
%% @hidden
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
%% @hidden
isSelection(This) -> wxCommandEvent:isSelection(This).
%% @hidden
isChecked(This) -> wxCommandEvent:isChecked(This).
%% @hidden
getString(This) -> wxCommandEvent:getString(This).
%% @hidden
getSelection(This) -> wxCommandEvent:getSelection(This).
%% @hidden
getInt(This) -> wxCommandEvent:getInt(This).
%% @hidden
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
%% @hidden
getClientData(This) -> wxCommandEvent:getClientData(This).
 %% From wxEvent
%% @hidden
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
skip(This) -> wxEvent:skip(This).
%% @hidden
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
getId(This) -> wxEvent:getId(This).
