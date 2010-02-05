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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxquerynewpaletteevent.html">wxQueryNewPaletteEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>query_new_palette</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxQueryNewPalette(). #wxQueryNewPalette{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxQueryNewPaletteEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxQueryNewPaletteEvent).
-include("wxe.hrl").
-export([getPaletteRealized/1,setPaletteRealized/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxQueryNewPaletteEvent(), Realized::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxquerynewpaletteevent.html#wxquerynewpaletteeventsetpaletterealized">external documentation</a>.
setPaletteRealized(#wx_ref{type=ThisT,ref=ThisRef},Realized)
 when is_boolean(Realized) ->
  ?CLASS(ThisT,wxQueryNewPaletteEvent),
  wxe_util:cast(?wxQueryNewPaletteEvent_SetPaletteRealized,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Realized)):32/?UI>>).

%% @spec (This::wxQueryNewPaletteEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxquerynewpaletteevent.html#wxquerynewpaletteeventgetpaletterealized">external documentation</a>.
getPaletteRealized(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxQueryNewPaletteEvent),
  wxe_util:call(?wxQueryNewPaletteEvent_GetPaletteRealized,
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
