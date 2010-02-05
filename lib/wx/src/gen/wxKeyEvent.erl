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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html">wxKeyEvent</a>.
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

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventaltdown">external documentation</a>.
altDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_AltDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventcmddown">external documentation</a>.
cmdDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_CmdDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventcontroldown">external documentation</a>.
controlDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_ControlDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventgetkeycode">external documentation</a>.
getKeyCode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetKeyCode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventgetmodifiers">external documentation</a>.
getModifiers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetModifiers,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventgetposition">external documentation</a>.
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventgetrawkeycode">external documentation</a>.
getRawKeyCode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetRawKeyCode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventgetrawkeyflags">external documentation</a>.
getRawKeyFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetRawKeyFlags,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventgetunicodekey">external documentation</a>.
getUnicodeKey(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetUnicodeKey,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventgetx">external documentation</a>.
getX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetX,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventgety">external documentation</a>.
getY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_GetY,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventhasmodifiers">external documentation</a>.
hasModifiers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_HasModifiers,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventmetadown">external documentation</a>.
metaDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:call(?wxKeyEvent_MetaDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxkeyevent.html#wxkeyeventshiftdown">external documentation</a>.
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
