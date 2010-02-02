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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html">wxCommandEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>command_button_clicked</em>, <em>command_checkbox_clicked</em>, <em>command_choice_selected</em>, <em>command_listbox_selected</em>, <em>command_listbox_doubleclicked</em>, <em>command_text_updated</em>, <em>command_text_enter</em>, <em>command_menu_selected</em>, <em>command_slider_updated</em>, <em>command_radiobox_selected</em>, <em>command_radiobutton_selected</em>, <em>command_scrollbar_updated</em>, <em>command_vlbox_selected</em>, <em>command_combobox_selected</em>, <em>command_tool_rclicked</em>, <em>command_tool_enter</em>, <em>command_checklistbox_toggled</em>, <em>command_togglebutton_clicked</em>, <em>command_left_click</em>, <em>command_left_dclick</em>, <em>command_right_click</em>, <em>command_set_focus</em>, <em>command_kill_focus</em>, <em>command_enter</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxCommand(). #wxCommand{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxCommandEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxCommandEvent).
-include("wxe.hrl").
-export([getClientData/1,getExtraLong/1,getInt/1,getSelection/1,getString/1,
  isChecked/1,isSelection/1,setInt/2,setString/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxCommandEvent()) -> term()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventgetclientobject">external documentation</a>.
getClientData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:call(?wxCommandEvent_getClientData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCommandEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventgetextralong">external documentation</a>.
getExtraLong(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:call(?wxCommandEvent_GetExtraLong,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCommandEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventgetint">external documentation</a>.
getInt(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:call(?wxCommandEvent_GetInt,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCommandEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventgetselection">external documentation</a>.
getSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:call(?wxCommandEvent_GetSelection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCommandEvent()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventgetstring">external documentation</a>.
getString(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:call(?wxCommandEvent_GetString,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCommandEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventischecked">external documentation</a>.
isChecked(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:call(?wxCommandEvent_IsChecked,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCommandEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventisselection">external documentation</a>.
isSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:call(?wxCommandEvent_IsSelection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCommandEvent(), I::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventsetint">external documentation</a>.
setInt(#wx_ref{type=ThisT,ref=ThisRef},I)
 when is_integer(I) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:cast(?wxCommandEvent_SetInt,
  <<ThisRef:32/?UI,I:32/?UI>>).

%% @spec (This::wxCommandEvent(), S::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcommandevent.html#wxcommandeventsetstring">external documentation</a>.
setString(#wx_ref{type=ThisT,ref=ThisRef},S)
 when is_list(S) ->
  ?CLASS(ThisT,wxCommandEvent),
  S_UC = unicode:characters_to_binary([S,0]),
  wxe_util:cast(?wxCommandEvent_SetString,
  <<ThisRef:32/?UI,(byte_size(S_UC)):32/?UI,(S_UC)/binary, 0:(((8- ((0+byte_size(S_UC)) band 16#7)) band 16#7))/unit:8>>).

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
