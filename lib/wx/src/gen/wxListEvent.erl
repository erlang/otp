%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html">wxListEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>command_list_begin_drag</em>, <em>command_list_begin_rdrag</em>, <em>command_list_begin_label_edit</em>, <em>command_list_end_label_edit</em>, <em>command_list_delete_item</em>, <em>command_list_delete_all_items</em>, <em>command_list_key_down</em>, <em>command_list_insert_item</em>, <em>command_list_col_click</em>, <em>command_list_col_right_click</em>, <em>command_list_col_begin_drag</em>, <em>command_list_col_dragging</em>, <em>command_list_col_end_drag</em>, <em>command_list_item_selected</em>, <em>command_list_item_deselected</em>, <em>command_list_item_right_click</em>, <em>command_list_item_middle_click</em>, <em>command_list_item_activated</em>, <em>command_list_item_focused</em>, <em>command_list_cache_hint</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxList(). #wxList{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxNotifyEvent}
%% <br />{@link wxCommandEvent}
%% <br />{@link wxEvent}
%% </p>
%% @type wxListEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxListEvent).
-include("wxe.hrl").
-export([getCacheFrom/1,getCacheTo/1,getColumn/1,getData/1,getImage/1,getIndex/1,
  getItem/1,getKeyCode/1,getLabel/1,getMask/1,getPoint/1,getText/1,isEditCancelled/1]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,
  getSkipped/1,getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

%% @hidden
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxListEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetcachefrom">external documentation</a>.
getCacheFrom(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetCacheFrom,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetcacheto">external documentation</a>.
getCacheTo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetCacheTo,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetkeycode">external documentation</a>.
getKeyCode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetKeyCode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetindex">external documentation</a>.
getIndex(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetIndex,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetcolumn">external documentation</a>.
getColumn(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetColumn,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetpoint">external documentation</a>.
getPoint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetPoint,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetlabel">external documentation</a>.
getLabel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetLabel,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgettext">external documentation</a>.
getText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetText,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetimage">external documentation</a>.
getImage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetImage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetdata">external documentation</a>.
getData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetmask">external documentation</a>.
getMask(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetMask,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> wxListItem:wxListItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventgetitem">external documentation</a>.
getItem(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_GetItem,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistevent.html#wxlisteventiseditcancelled">external documentation</a>.
isEditCancelled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:call(?wxListEvent_IsEditCancelled,
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
