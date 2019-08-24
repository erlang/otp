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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html">wxKeyEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>char</em>, <em>char_hook</em>, <em>key_down</em>, <em>key_up</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxKey(). #wxKey{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxKeyEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxKeyEvent).
-include("wxe.hrl").
-export([altDown/1,cmdDown/1,controlDown/1,getKeyCode/1,getModifiers/1,getPosition/1,
  getRawKeyCode/1,getRawKeyFlags/1,getUnicodeKey/1,getX/1,getY/1,hasModifiers/1,
  metaDown/1,shiftDown/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-export_type([wxKeyEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxKeyEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventaltdown">external documentation</a>.
-spec altDown(This) -> boolean() when
	This::wxKeyEvent().
altDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_AltDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventcmddown">external documentation</a>.
-spec cmdDown(This) -> boolean() when
	This::wxKeyEvent().
cmdDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_CmdDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventcontroldown">external documentation</a>.
-spec controlDown(This) -> boolean() when
	This::wxKeyEvent().
controlDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_ControlDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetkeycode">external documentation</a>.
-spec getKeyCode(This) -> integer() when
	This::wxKeyEvent().
getKeyCode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetKeyCode,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetmodifiers">external documentation</a>.
-spec getModifiers(This) -> integer() when
	This::wxKeyEvent().
getModifiers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetModifiers,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxKeyEvent().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetrawkeycode">external documentation</a>.
-spec getRawKeyCode(This) -> integer() when
	This::wxKeyEvent().
getRawKeyCode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetRawKeyCode,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetrawkeyflags">external documentation</a>.
-spec getRawKeyFlags(This) -> integer() when
	This::wxKeyEvent().
getRawKeyFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetRawKeyFlags,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetunicodekey">external documentation</a>.
-spec getUnicodeKey(This) -> integer() when
	This::wxKeyEvent().
getUnicodeKey(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetUnicodeKey,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgetx">external documentation</a>.
-spec getX(This) -> integer() when
	This::wxKeyEvent().
getX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetX,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventgety">external documentation</a>.
-spec getY(This) -> integer() when
	This::wxKeyEvent().
getY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetY,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventhasmodifiers">external documentation</a>.
-spec hasModifiers(This) -> boolean() when
	This::wxKeyEvent().
hasModifiers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_HasModifiers,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventmetadown">external documentation</a>.
-spec metaDown(This) -> boolean() when
	This::wxKeyEvent().
metaDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_MetaDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxkeyevent.html#wxkeyeventshiftdown">external documentation</a>.
-spec shiftDown(This) -> boolean() when
	This::wxKeyEvent().
shiftDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_ShiftDown,
  <<ThisRef:32/?UI>>).

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
