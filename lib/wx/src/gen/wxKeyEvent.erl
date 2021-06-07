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

-module(wxKeyEvent).
-include("wxe.hrl").
-export([altDown/1,cmdDown/1,controlDown/1,getKeyCode/1,getModifiers/1,getPosition/1,
  getRawKeyCode/1,getRawKeyFlags/1,getUnicodeKey/1,getX/1,getY/1,hasModifiers/1,
  metaDown/1,shiftDown/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxKeyEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxKeyEventType() :: 'char' | 'char_hook' | 'key_down' | 'key_up'.
-export_type([wxKeyEvent/0, wxKey/0, wxKeyEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventaltdown">external documentation</a>.
-spec altDown(This) -> boolean() when
	This::wxKeyEvent().
altDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_AltDown),
  wxe_util:rec(?wxKeyEvent_AltDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventcmddown">external documentation</a>.
-spec cmdDown(This) -> boolean() when
	This::wxKeyEvent().
cmdDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_CmdDown),
  wxe_util:rec(?wxKeyEvent_CmdDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventcontroldown">external documentation</a>.
-spec controlDown(This) -> boolean() when
	This::wxKeyEvent().
controlDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_ControlDown),
  wxe_util:rec(?wxKeyEvent_ControlDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetkeycode">external documentation</a>.
-spec getKeyCode(This) -> integer() when
	This::wxKeyEvent().
getKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetKeyCode),
  wxe_util:rec(?wxKeyEvent_GetKeyCode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetmodifiers">external documentation</a>.
-spec getModifiers(This) -> integer() when
	This::wxKeyEvent().
getModifiers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetModifiers),
  wxe_util:rec(?wxKeyEvent_GetModifiers).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxKeyEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetPosition),
  wxe_util:rec(?wxKeyEvent_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetrawkeycode">external documentation</a>.
-spec getRawKeyCode(This) -> integer() when
	This::wxKeyEvent().
getRawKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetRawKeyCode),
  wxe_util:rec(?wxKeyEvent_GetRawKeyCode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetrawkeyflags">external documentation</a>.
-spec getRawKeyFlags(This) -> integer() when
	This::wxKeyEvent().
getRawKeyFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetRawKeyFlags),
  wxe_util:rec(?wxKeyEvent_GetRawKeyFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetunicodekey">external documentation</a>.
-spec getUnicodeKey(This) -> integer() when
	This::wxKeyEvent().
getUnicodeKey(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetUnicodeKey),
  wxe_util:rec(?wxKeyEvent_GetUnicodeKey).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetx">external documentation</a>.
-spec getX(This) -> integer() when
	This::wxKeyEvent().
getX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetX),
  wxe_util:rec(?wxKeyEvent_GetX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgety">external documentation</a>.
-spec getY(This) -> integer() when
	This::wxKeyEvent().
getY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetY),
  wxe_util:rec(?wxKeyEvent_GetY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventhasmodifiers">external documentation</a>.
-spec hasModifiers(This) -> boolean() when
	This::wxKeyEvent().
hasModifiers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_HasModifiers),
  wxe_util:rec(?wxKeyEvent_HasModifiers).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventmetadown">external documentation</a>.
-spec metaDown(This) -> boolean() when
	This::wxKeyEvent().
metaDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_MetaDown),
  wxe_util:rec(?wxKeyEvent_MetaDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventshiftdown">external documentation</a>.
-spec shiftDown(This) -> boolean() when
	This::wxKeyEvent().
shiftDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_ShiftDown),
  wxe_util:rec(?wxKeyEvent_ShiftDown).

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
