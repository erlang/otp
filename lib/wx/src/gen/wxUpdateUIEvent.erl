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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html">wxUpdateUIEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>update_ui</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxUpdateUI(). #wxUpdateUI{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxCommandEvent}
%% <br />{@link wxEvent}
%% </p>
%% @type wxUpdateUIEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxUpdateUIEvent).
-include("wxe.hrl").
-export([canUpdate/1,check/2,enable/2,getChecked/1,getEnabled/1,getMode/0,getSetChecked/1,
  getSetEnabled/1,getSetShown/1,getSetText/1,getShown/1,getText/1,getUpdateInterval/0,
  resetUpdateTime/0,setMode/1,setText/2,setUpdateInterval/1,show/2]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Win::wxWindow:wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventcanupdate">external documentation</a>.
canUpdate(#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(WinT,wxWindow),
  wxe_util:call(?wxUpdateUIEvent_CanUpdate,
  <<WinRef:32/?UI>>).

%% @spec (This::wxUpdateUIEvent(), Check::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventcheck">external documentation</a>.
check(#wx_ref{type=ThisT,ref=ThisRef},Check)
 when is_boolean(Check) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:cast(?wxUpdateUIEvent_Check,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Check)):32/?UI>>).

%% @spec (This::wxUpdateUIEvent(), Enable::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventenable">external documentation</a>.
enable(#wx_ref{type=ThisT,ref=ThisRef},Enable)
 when is_boolean(Enable) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:cast(?wxUpdateUIEvent_Enable,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>).

%% @spec (This::wxUpdateUIEvent(), Show::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventshow">external documentation</a>.
show(#wx_ref{type=ThisT,ref=ThisRef},Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:cast(?wxUpdateUIEvent_Show,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>).

%% @spec (This::wxUpdateUIEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetchecked">external documentation</a>.
getChecked(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:call(?wxUpdateUIEvent_GetChecked,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxUpdateUIEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetenabled">external documentation</a>.
getEnabled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:call(?wxUpdateUIEvent_GetEnabled,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxUpdateUIEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetshown">external documentation</a>.
getShown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:call(?wxUpdateUIEvent_GetShown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxUpdateUIEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetsetchecked">external documentation</a>.
getSetChecked(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:call(?wxUpdateUIEvent_GetSetChecked,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxUpdateUIEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetsetenabled">external documentation</a>.
getSetEnabled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:call(?wxUpdateUIEvent_GetSetEnabled,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxUpdateUIEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetsetshown">external documentation</a>.
getSetShown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:call(?wxUpdateUIEvent_GetSetShown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxUpdateUIEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetsettext">external documentation</a>.
getSetText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:call(?wxUpdateUIEvent_GetSetText,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxUpdateUIEvent()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgettext">external documentation</a>.
getText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:call(?wxUpdateUIEvent_GetText,
  <<ThisRef:32/?UI>>).

%% @spec () -> WxUpdateUIMode
%% WxUpdateUIMode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetmode">external documentation</a>.
%%<br /> WxUpdateUIMode is one of ?wxUPDATE_UI_PROCESS_ALL | ?wxUPDATE_UI_PROCESS_SPECIFIED
getMode() ->
  wxe_util:call(?wxUpdateUIEvent_GetMode,
  <<>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventgetupdateinterval">external documentation</a>.
getUpdateInterval() ->
  wxe_util:call(?wxUpdateUIEvent_GetUpdateInterval,
  <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventresetupdatetime">external documentation</a>.
resetUpdateTime() ->
  wxe_util:cast(?wxUpdateUIEvent_ResetUpdateTime,
  <<>>).

%% @spec (Mode::WxUpdateUIMode) -> ok
%% WxUpdateUIMode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventsetmode">external documentation</a>.
%%<br /> WxUpdateUIMode is one of ?wxUPDATE_UI_PROCESS_ALL | ?wxUPDATE_UI_PROCESS_SPECIFIED
setMode(Mode)
 when is_integer(Mode) ->
  wxe_util:cast(?wxUpdateUIEvent_SetMode,
  <<Mode:32/?UI>>).

%% @spec (This::wxUpdateUIEvent(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventsettext">external documentation</a>.
setText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxUpdateUIEvent_SetText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (UpdateInterval::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxupdateuievent.html#wxupdateuieventsetupdateinterval">external documentation</a>.
setUpdateInterval(UpdateInterval)
 when is_integer(UpdateInterval) ->
  wxe_util:cast(?wxUpdateUIEvent_SetUpdateInterval,
  <<UpdateInterval:32/?UI>>).

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
