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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html">wxTreeEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>command_tree_begin_drag</em>, <em>command_tree_begin_rdrag</em>, <em>command_tree_begin_label_edit</em>, <em>command_tree_end_label_edit</em>, <em>command_tree_delete_item</em>, <em>command_tree_get_info</em>, <em>command_tree_set_info</em>, <em>command_tree_item_expanded</em>, <em>command_tree_item_expanding</em>, <em>command_tree_item_collapsed</em>, <em>command_tree_item_collapsing</em>, <em>command_tree_sel_changed</em>, <em>command_tree_sel_changing</em>, <em>command_tree_key_down</em>, <em>command_tree_item_activated</em>, <em>command_tree_item_right_click</em>, <em>command_tree_item_middle_click</em>, <em>command_tree_end_drag</em>, <em>command_tree_state_image_click</em>, <em>command_tree_item_gettooltip</em>, <em>command_tree_item_menu</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxTree(). #wxTree{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxNotifyEvent}
%% <br />{@link wxCommandEvent}
%% <br />{@link wxEvent}
%% </p>
%% @type wxTreeEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxTreeEvent).
-include("wxe.hrl").
-export([getItem/1,getKeyCode/1,getKeyEvent/1,getLabel/1,getOldItem/1,getPoint/1,
  isEditCancelled/1,setToolTip/2]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,
  getSkipped/1,getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-export_type([wxTreeEvent/0]).
%% @hidden
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxTreeEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetkeycode">external documentation</a>.
-spec getKeyCode(This) -> integer() when
	This::wxTreeEvent().
getKeyCode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:call(?wxTreeEvent_GetKeyCode,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetitem">external documentation</a>.
-spec getItem(This) -> integer() when
	This::wxTreeEvent().
getItem(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:call(?wxTreeEvent_GetItem,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetkeyevent">external documentation</a>.
-spec getKeyEvent(This) -> wxKeyEvent:wxKeyEvent() when
	This::wxTreeEvent().
getKeyEvent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:call(?wxTreeEvent_GetKeyEvent,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetlabel">external documentation</a>.
-spec getLabel(This) -> unicode:charlist() when
	This::wxTreeEvent().
getLabel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:call(?wxTreeEvent_GetLabel,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetolditem">external documentation</a>.
-spec getOldItem(This) -> integer() when
	This::wxTreeEvent().
getOldItem(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:call(?wxTreeEvent_GetOldItem,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventgetpoint">external documentation</a>.
-spec getPoint(This) -> {X::integer(), Y::integer()} when
	This::wxTreeEvent().
getPoint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:call(?wxTreeEvent_GetPoint,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventiseditcancelled">external documentation</a>.
-spec isEditCancelled(This) -> boolean() when
	This::wxTreeEvent().
isEditCancelled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:call(?wxTreeEvent_IsEditCancelled,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreeevent.html#wxtreeeventsettooltip">external documentation</a>.
-spec setToolTip(This, ToolTip) -> 'ok' when
	This::wxTreeEvent(), ToolTip::unicode:chardata().
setToolTip(#wx_ref{type=ThisT,ref=ThisRef},ToolTip)
 when is_list(ToolTip) ->
  ?CLASS(ThisT,wxTreeEvent),
  ToolTip_UC = unicode:characters_to_binary([ToolTip,0]),
  wxe_util:cast(?wxTreeEvent_SetToolTip,
  <<ThisRef:32/?UI,(byte_size(ToolTip_UC)):32/?UI,(ToolTip_UC)/binary, 0:(((8- ((0+byte_size(ToolTip_UC)) band 16#7)) band 16#7))/unit:8>>).

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
