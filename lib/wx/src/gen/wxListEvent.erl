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

-module(wxListEvent).
-include("wxe.hrl").
-export([getCacheFrom/1,getCacheTo/1,getColumn/1,getData/1,getImage/1,getIndex/1,
  getItem/1,getKeyCode/1,getLabel/1,getMask/1,getPoint/1,getText/1,isEditCancelled/1]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,
  getSkipped/1,getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-type wxListEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxListEventType() :: 'command_list_begin_drag' | 'command_list_begin_rdrag' | 'command_list_begin_label_edit' | 'command_list_end_label_edit' | 'command_list_delete_item' | 'command_list_delete_all_items' | 'command_list_key_down' | 'command_list_insert_item' | 'command_list_col_click' | 'command_list_col_right_click' | 'command_list_col_begin_drag' | 'command_list_col_dragging' | 'command_list_col_end_drag' | 'command_list_item_selected' | 'command_list_item_deselected' | 'command_list_item_right_click' | 'command_list_item_middle_click' | 'command_list_item_activated' | 'command_list_item_focused' | 'command_list_cache_hint'.
-export_type([wxListEvent/0, wxList/0, wxListEventType/0]).
%% @hidden
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetcachefrom">external documentation</a>.
-spec getCacheFrom(This) -> integer() when
	This::wxListEvent().
getCacheFrom(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetCacheFrom),
  wxe_util:rec(?wxListEvent_GetCacheFrom).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetcacheto">external documentation</a>.
-spec getCacheTo(This) -> integer() when
	This::wxListEvent().
getCacheTo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetCacheTo),
  wxe_util:rec(?wxListEvent_GetCacheTo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetkeycode">external documentation</a>.
-spec getKeyCode(This) -> integer() when
	This::wxListEvent().
getKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetKeyCode),
  wxe_util:rec(?wxListEvent_GetKeyCode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetindex">external documentation</a>.
-spec getIndex(This) -> integer() when
	This::wxListEvent().
getIndex(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetIndex),
  wxe_util:rec(?wxListEvent_GetIndex).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetcolumn">external documentation</a>.
-spec getColumn(This) -> integer() when
	This::wxListEvent().
getColumn(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetColumn),
  wxe_util:rec(?wxListEvent_GetColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetpoint">external documentation</a>.
-spec getPoint(This) -> {X::integer(), Y::integer()} when
	This::wxListEvent().
getPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetPoint),
  wxe_util:rec(?wxListEvent_GetPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetlabel">external documentation</a>.
-spec getLabel(This) -> unicode:charlist() when
	This::wxListEvent().
getLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetLabel),
  wxe_util:rec(?wxListEvent_GetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgettext">external documentation</a>.
-spec getText(This) -> unicode:charlist() when
	This::wxListEvent().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetText),
  wxe_util:rec(?wxListEvent_GetText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetimage">external documentation</a>.
-spec getImage(This) -> integer() when
	This::wxListEvent().
getImage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetImage),
  wxe_util:rec(?wxListEvent_GetImage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetdata">external documentation</a>.
-spec getData(This) -> integer() when
	This::wxListEvent().
getData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetData),
  wxe_util:rec(?wxListEvent_GetData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetmask">external documentation</a>.
-spec getMask(This) -> integer() when
	This::wxListEvent().
getMask(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetMask),
  wxe_util:rec(?wxListEvent_GetMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventgetitem">external documentation</a>.
-spec getItem(This) -> wxListItem:wxListItem() when
	This::wxListEvent().
getItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetItem),
  wxe_util:rec(?wxListEvent_GetItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistevent.html#wxlisteventiseditcancelled">external documentation</a>.
-spec isEditCancelled(This) -> boolean() when
	This::wxListEvent().
isEditCancelled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_IsEditCancelled),
  wxe_util:rec(?wxListEvent_IsEditCancelled).

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
