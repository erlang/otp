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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuevent.html">wxMenuEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>menu_open</em>, <em>menu_close</em>, <em>menu_highlight</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxMenu(). #wxMenu{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxMenuEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxMenuEvent).
-include("wxe.hrl").
-export([getMenu/1,getMenuId/1,isPopup/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxMenuEvent()) -> wxMenu:wxMenu()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuevent.html#wxmenueventgetmenu">external documentation</a>.
getMenu(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuEvent),
  wxe_util:call(?wxMenuEvent_GetMenu,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuevent.html#wxmenueventgetmenuid">external documentation</a>.
getMenuId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuEvent),
  wxe_util:call(?wxMenuEvent_GetMenuId,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuevent.html#wxmenueventispopup">external documentation</a>.
isPopup(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuEvent),
  wxe_util:call(?wxMenuEvent_IsPopup,
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
