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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxquerynewpaletteevent.html">wxQueryNewPaletteEvent</a>.
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

-export_type([wxQueryNewPaletteEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxQueryNewPaletteEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxquerynewpaletteevent.html#wxquerynewpaletteeventsetpaletterealized">external documentation</a>.
-spec setPaletteRealized(This, Realized) -> 'ok' when
	This::wxQueryNewPaletteEvent(), Realized::boolean().
setPaletteRealized(#wx_ref{type=ThisT,ref=ThisRef},Realized)
 when is_boolean(Realized) ->
  ?CLASS(ThisT,wxQueryNewPaletteEvent),
  wxe_util:cast(?wxQueryNewPaletteEvent_SetPaletteRealized,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Realized)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxquerynewpaletteevent.html#wxquerynewpaletteeventgetpaletterealized">external documentation</a>.
-spec getPaletteRealized(This) -> boolean() when
	This::wxQueryNewPaletteEvent().
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
