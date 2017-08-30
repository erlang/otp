%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html">wxGridEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>grid_cell_left_click</em>, <em>grid_cell_right_click</em>, <em>grid_cell_left_dclick</em>, <em>grid_cell_right_dclick</em>, <em>grid_label_left_click</em>, <em>grid_label_right_click</em>, <em>grid_label_left_dclick</em>, <em>grid_label_right_dclick</em>, <em>grid_row_size</em>, <em>grid_col_size</em>, <em>grid_range_select</em>, <em>grid_cell_change</em>, <em>grid_select_cell</em>, <em>grid_editor_shown</em>, <em>grid_editor_hidden</em>, <em>grid_editor_created</em>, <em>grid_cell_begin_drag</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxGrid(). #wxGrid{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxNotifyEvent}
%% <br />{@link wxCommandEvent}
%% <br />{@link wxEvent}
%% </p>
%% @type wxGridEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridEvent).
-include("wxe.hrl").
-export([altDown/1,controlDown/1,getCol/1,getPosition/1,getRow/1,metaDown/1,
  selecting/1,shiftDown/1]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,
  getSkipped/1,getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-export_type([wxGridEvent/0]).
%% @hidden
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGridEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventaltdown">external documentation</a>.
-spec altDown(This) -> boolean() when
	This::wxGridEvent().
altDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:call(?wxGridEvent_AltDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventcontroldown">external documentation</a>.
-spec controlDown(This) -> boolean() when
	This::wxGridEvent().
controlDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:call(?wxGridEvent_ControlDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetcol">external documentation</a>.
-spec getCol(This) -> integer() when
	This::wxGridEvent().
getCol(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:call(?wxGridEvent_GetCol,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxGridEvent().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:call(?wxGridEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventgetrow">external documentation</a>.
-spec getRow(This) -> integer() when
	This::wxGridEvent().
getRow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:call(?wxGridEvent_GetRow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventmetadown">external documentation</a>.
-spec metaDown(This) -> boolean() when
	This::wxGridEvent().
metaDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:call(?wxGridEvent_MetaDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventselecting">external documentation</a>.
-spec selecting(This) -> boolean() when
	This::wxGridEvent().
selecting(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:call(?wxGridEvent_Selecting,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridevent.html#wxgrideventshiftdown">external documentation</a>.
-spec shiftDown(This) -> boolean() when
	This::wxGridEvent().
shiftDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridEvent),
  wxe_util:call(?wxGridEvent_ShiftDown,
  <<ThisRef:32/?UI>>).

 %% From wxNotifyEvent
%% @hidden
veto(This) -> wxNotifyEvent:veto(This).
%% @hidden
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
%% @hidden
allow(This) -> wxNotifyEvent:allow(This).
 %% From wxCommandEvent
%% @hidden
setString(This,S) -> wxCommandEvent:setString(This,S).
%% @hidden
setInt(This,I) -> wxCommandEvent:setInt(This,I).
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
