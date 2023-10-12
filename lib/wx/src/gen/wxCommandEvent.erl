%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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

-module(wxCommandEvent).
-include("wxe.hrl").
-export([getClientData/1,getExtraLong/1,getInt/1,getSelection/1,getString/1,
  isChecked/1,isSelection/1,setInt/2,setString/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxCommandEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxCommandEventType() :: 'command_button_clicked' | 'command_checkbox_clicked' | 'command_choice_selected' | 'command_listbox_selected' | 'command_listbox_doubleclicked' | 'command_text_updated' | 'command_text_enter' | 'text_maxlen' | 'command_menu_selected' | 'command_slider_updated' | 'command_radiobox_selected' | 'command_radiobutton_selected' | 'command_scrollbar_updated' | 'command_vlbox_selected' | 'command_combobox_selected' | 'combobox_dropdown' | 'combobox_closeup' | 'command_tool_rclicked' | 'command_tool_enter' | 'tool_dropdown' | 'command_checklistbox_toggled' | 'command_togglebutton_clicked' | 'command_left_click' | 'command_left_dclick' | 'command_right_click' | 'command_set_focus' | 'command_kill_focus' | 'command_enter' | 'notification_message_click' | 'notification_message_dismissed' | 'notification_message_action'.
-export_type([wxCommandEvent/0, wxCommand/0, wxCommandEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventgetclientobject">external documentation</a>.
-spec getClientData(This) -> term() when
	This::wxCommandEvent().
getClientData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_getClientData),
  wxe_util:rec(?wxCommandEvent_getClientData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventgetextralong">external documentation</a>.
-spec getExtraLong(This) -> integer() when
	This::wxCommandEvent().
getExtraLong(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_GetExtraLong),
  wxe_util:rec(?wxCommandEvent_GetExtraLong).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventgetint">external documentation</a>.
-spec getInt(This) -> integer() when
	This::wxCommandEvent().
getInt(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_GetInt),
  wxe_util:rec(?wxCommandEvent_GetInt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventgetselection">external documentation</a>.
-spec getSelection(This) -> integer() when
	This::wxCommandEvent().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_GetSelection),
  wxe_util:rec(?wxCommandEvent_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventgetstring">external documentation</a>.
-spec getString(This) -> unicode:charlist() when
	This::wxCommandEvent().
getString(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_GetString),
  wxe_util:rec(?wxCommandEvent_GetString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventischecked">external documentation</a>.
-spec isChecked(This) -> boolean() when
	This::wxCommandEvent().
isChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_IsChecked),
  wxe_util:rec(?wxCommandEvent_IsChecked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventisselection">external documentation</a>.
-spec isSelection(This) -> boolean() when
	This::wxCommandEvent().
isSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_IsSelection),
  wxe_util:rec(?wxCommandEvent_IsSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventsetint">external documentation</a>.
-spec setInt(This, IntCommand) -> 'ok' when
	This::wxCommandEvent(), IntCommand::integer().
setInt(#wx_ref{type=ThisT}=This,IntCommand)
 when is_integer(IntCommand) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,IntCommand,?get_env(),?wxCommandEvent_SetInt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcommandevent.html#wxcommandeventsetstring">external documentation</a>.
-spec setString(This, String) -> 'ok' when
	This::wxCommandEvent(), String::unicode:chardata().
setString(#wx_ref{type=ThisT}=This,String)
 when ?is_chardata(String) ->
  ?CLASS(ThisT,wxCommandEvent),
  String_UC = unicode:characters_to_binary(String),
  wxe_util:queue_cmd(This,String_UC,?get_env(),?wxCommandEvent_SetString).

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
