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

-type wxUpdateUIEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxUpdateUIEventType() :: 'update_ui'.
-export_type([wxUpdateUIEvent/0, wxUpdateUI/0, wxUpdateUIEventType/0]).
%% @hidden
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventcanupdate">external documentation</a>.
-spec canUpdate(Window) -> boolean() when
	Window::wxWindow:wxWindow().
canUpdate(#wx_ref{type=WindowT}=Window) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(Window,?get_env(),?wxUpdateUIEvent_CanUpdate),
  wxe_util:rec(?wxUpdateUIEvent_CanUpdate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventcheck">external documentation</a>.
-spec check(This, Check) -> 'ok' when
	This::wxUpdateUIEvent(), Check::boolean().
check(#wx_ref{type=ThisT}=This,Check)
 when is_boolean(Check) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,Check,?get_env(),?wxUpdateUIEvent_Check).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventenable">external documentation</a>.
-spec enable(This, Enable) -> 'ok' when
	This::wxUpdateUIEvent(), Enable::boolean().
enable(#wx_ref{type=ThisT}=This,Enable)
 when is_boolean(Enable) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,Enable,?get_env(),?wxUpdateUIEvent_Enable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventshow">external documentation</a>.
-spec show(This, Show) -> 'ok' when
	This::wxUpdateUIEvent(), Show::boolean().
show(#wx_ref{type=ThisT}=This,Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,Show,?get_env(),?wxUpdateUIEvent_Show).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetchecked">external documentation</a>.
-spec getChecked(This) -> boolean() when
	This::wxUpdateUIEvent().
getChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetChecked),
  wxe_util:rec(?wxUpdateUIEvent_GetChecked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetenabled">external documentation</a>.
-spec getEnabled(This) -> boolean() when
	This::wxUpdateUIEvent().
getEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetEnabled),
  wxe_util:rec(?wxUpdateUIEvent_GetEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetshown">external documentation</a>.
-spec getShown(This) -> boolean() when
	This::wxUpdateUIEvent().
getShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetShown),
  wxe_util:rec(?wxUpdateUIEvent_GetShown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetsetchecked">external documentation</a>.
-spec getSetChecked(This) -> boolean() when
	This::wxUpdateUIEvent().
getSetChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetSetChecked),
  wxe_util:rec(?wxUpdateUIEvent_GetSetChecked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetsetenabled">external documentation</a>.
-spec getSetEnabled(This) -> boolean() when
	This::wxUpdateUIEvent().
getSetEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetSetEnabled),
  wxe_util:rec(?wxUpdateUIEvent_GetSetEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetsetshown">external documentation</a>.
-spec getSetShown(This) -> boolean() when
	This::wxUpdateUIEvent().
getSetShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetSetShown),
  wxe_util:rec(?wxUpdateUIEvent_GetSetShown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetsettext">external documentation</a>.
-spec getSetText(This) -> boolean() when
	This::wxUpdateUIEvent().
getSetText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetSetText),
  wxe_util:rec(?wxUpdateUIEvent_GetSetText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgettext">external documentation</a>.
-spec getText(This) -> unicode:charlist() when
	This::wxUpdateUIEvent().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetText),
  wxe_util:rec(?wxUpdateUIEvent_GetText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetmode">external documentation</a>.
%%<br /> Res = ?wxUPDATE_UI_PROCESS_ALL | ?wxUPDATE_UI_PROCESS_SPECIFIED
-spec getMode() -> wx:wx_enum().
getMode() ->
  wxe_util:queue_cmd(?get_env(), ?wxUpdateUIEvent_GetMode),
  wxe_util:rec(?wxUpdateUIEvent_GetMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventgetupdateinterval">external documentation</a>.
-spec getUpdateInterval() -> integer().
getUpdateInterval() ->
  wxe_util:queue_cmd(?get_env(), ?wxUpdateUIEvent_GetUpdateInterval),
  wxe_util:rec(?wxUpdateUIEvent_GetUpdateInterval).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventresetupdatetime">external documentation</a>.
-spec resetUpdateTime() -> 'ok'.
resetUpdateTime() ->
  wxe_util:queue_cmd(?get_env(), ?wxUpdateUIEvent_ResetUpdateTime).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventsetmode">external documentation</a>.
%%<br /> Mode = ?wxUPDATE_UI_PROCESS_ALL | ?wxUPDATE_UI_PROCESS_SPECIFIED
-spec setMode(Mode) -> 'ok' when
	Mode::wx:wx_enum().
setMode(Mode)
 when is_integer(Mode) ->
  wxe_util:queue_cmd(Mode,?get_env(),?wxUpdateUIEvent_SetMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventsettext">external documentation</a>.
-spec setText(This, Text) -> 'ok' when
	This::wxUpdateUIEvent(), Text::unicode:chardata().
setText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxUpdateUIEvent_SetText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxupdateuievent.html#wxupdateuieventsetupdateinterval">external documentation</a>.
-spec setUpdateInterval(UpdateInterval) -> 'ok' when
	UpdateInterval::integer().
setUpdateInterval(UpdateInterval)
 when is_integer(UpdateInterval) ->
  wxe_util:queue_cmd(UpdateInterval,?get_env(),?wxUpdateUIEvent_SetUpdateInterval).

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
