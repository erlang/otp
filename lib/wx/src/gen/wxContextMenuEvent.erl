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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcontextmenuevent.html">wxContextMenuEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>context_menu</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxContextMenu(). #wxContextMenu{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxCommandEvent}
%% <br />{@link wxEvent}
%% </p>
%% @type wxContextMenuEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxContextMenuEvent).
-include("wxe.hrl").
-export([getPosition/1,setPosition/2]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxContextMenuEvent()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcontextmenuevent.html#wxcontextmenueventgetposition">external documentation</a>.
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxContextMenuEvent),
  wxe_util:call(?wxContextMenuEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxContextMenuEvent(), Pos::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcontextmenuevent.html#wxcontextmenueventsetposition">external documentation</a>.
setPosition(#wx_ref{type=ThisT,ref=ThisRef},{PosX,PosY})
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxContextMenuEvent),
  wxe_util:cast(?wxContextMenuEvent_SetPosition,
  <<ThisRef:32/?UI,PosX:32/?UI,PosY:32/?UI>>).

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
