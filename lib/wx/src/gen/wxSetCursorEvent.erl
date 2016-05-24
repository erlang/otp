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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html">wxSetCursorEvent</a>.
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

-export_type([wxSetCursorEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxSetCursorEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventgetcursor">external documentation</a>.
-spec getCursor(This) -> wxCursor:wxCursor() when
	This::wxSetCursorEvent().
getCursor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:call(?wxSetCursorEvent_GetCursor,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventgetx">external documentation</a>.
-spec getX(This) -> integer() when
	This::wxSetCursorEvent().
getX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:call(?wxSetCursorEvent_GetX,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventgety">external documentation</a>.
-spec getY(This) -> integer() when
	This::wxSetCursorEvent().
getY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:call(?wxSetCursorEvent_GetY,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventhascursor">external documentation</a>.
-spec hasCursor(This) -> boolean() when
	This::wxSetCursorEvent().
hasCursor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:call(?wxSetCursorEvent_HasCursor,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventsetcursor">external documentation</a>.
-spec setCursor(This, Cursor) -> 'ok' when
	This::wxSetCursorEvent(), Cursor::wxCursor:wxCursor().
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
