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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsetcursorevent.html">wxSetCursorEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>set_cursor</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxSetCursor(). #wxSetCursor{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxSetCursorEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxSetCursorEvent).
-include("wxe.hrl").
-export([getCursor/1,getX/1,getY/1,hasCursor/1,setCursor/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxSetCursorEvent()) -> wxCursor:wxCursor()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsetcursorevent.html#wxsetcursoreventgetcursor">external documentation</a>.
getCursor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:call(?wxSetCursorEvent_GetCursor,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSetCursorEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsetcursorevent.html#wxsetcursoreventgetx">external documentation</a>.
getX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:call(?wxSetCursorEvent_GetX,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSetCursorEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsetcursorevent.html#wxsetcursoreventgety">external documentation</a>.
getY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:call(?wxSetCursorEvent_GetY,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSetCursorEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsetcursorevent.html#wxsetcursoreventhascursor">external documentation</a>.
hasCursor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:call(?wxSetCursorEvent_HasCursor,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSetCursorEvent(), Cursor::wxCursor:wxCursor()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsetcursorevent.html#wxsetcursoreventsetcursor">external documentation</a>.
setCursor(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CursorT,ref=CursorRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  ?CLASS(CursorT,wxCursor),
  wxe_util:cast(?wxSetCursorEvent_SetCursor,
  <<ThisRef:32/?UI,CursorRef:32/?UI>>).

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
