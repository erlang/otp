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

-module(wxTreeEvent).
-include("wxe.hrl").
-export([getItem/1,getKeyCode/1,getKeyEvent/1,getLabel/1,getOldItem/1,getPoint/1,
  isEditCancelled/1,setToolTip/2]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,
  getSkipped/1,getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-type wxTreeEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxTreeEventType() :: 'command_tree_begin_drag' | 'command_tree_begin_rdrag' | 'command_tree_begin_label_edit' | 'command_tree_end_label_edit' | 'command_tree_delete_item' | 'command_tree_get_info' | 'command_tree_set_info' | 'command_tree_item_expanded' | 'command_tree_item_expanding' | 'command_tree_item_collapsed' | 'command_tree_item_collapsing' | 'command_tree_sel_changed' | 'command_tree_sel_changing' | 'command_tree_key_down' | 'command_tree_item_activated' | 'command_tree_item_right_click' | 'command_tree_item_middle_click' | 'command_tree_end_drag' | 'command_tree_state_image_click' | 'command_tree_item_gettooltip' | 'command_tree_item_menu' | 'dirctrl_selectionchanged' | 'dirctrl_fileactivated'.
-export_type([wxTreeEvent/0, wxTree/0, wxTreeEventType/0]).
%% @hidden
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetkeycode">external documentation</a>.
-spec getKeyCode(This) -> integer() when
	This::wxTreeEvent().
getKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetKeyCode),
  wxe_util:rec(?wxTreeEvent_GetKeyCode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetitem">external documentation</a>.
-spec getItem(This) -> integer() when
	This::wxTreeEvent().
getItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetItem),
  wxe_util:rec(?wxTreeEvent_GetItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetkeyevent">external documentation</a>.
-spec getKeyEvent(This) -> wxKeyEvent:wxKeyEvent() when
	This::wxTreeEvent().
getKeyEvent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetKeyEvent),
  wxe_util:rec(?wxTreeEvent_GetKeyEvent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetlabel">external documentation</a>.
-spec getLabel(This) -> unicode:charlist() when
	This::wxTreeEvent().
getLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetLabel),
  wxe_util:rec(?wxTreeEvent_GetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetolditem">external documentation</a>.
-spec getOldItem(This) -> integer() when
	This::wxTreeEvent().
getOldItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetOldItem),
  wxe_util:rec(?wxTreeEvent_GetOldItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetpoint">external documentation</a>.
-spec getPoint(This) -> {X::integer(), Y::integer()} when
	This::wxTreeEvent().
getPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetPoint),
  wxe_util:rec(?wxTreeEvent_GetPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventiseditcancelled">external documentation</a>.
-spec isEditCancelled(This) -> boolean() when
	This::wxTreeEvent().
isEditCancelled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_IsEditCancelled),
  wxe_util:rec(?wxTreeEvent_IsEditCancelled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventsettooltip">external documentation</a>.
-spec setToolTip(This, Tooltip) -> 'ok' when
	This::wxTreeEvent(), Tooltip::unicode:chardata().
setToolTip(#wx_ref{type=ThisT}=This,Tooltip)
 when ?is_chardata(Tooltip) ->
  ?CLASS(ThisT,wxTreeEvent),
  Tooltip_UC = unicode:characters_to_binary(Tooltip),
  wxe_util:queue_cmd(This,Tooltip_UC,?get_env(),?wxTreeEvent_SetToolTip).

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
