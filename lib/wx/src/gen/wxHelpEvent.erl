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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhelpevent.html">wxHelpEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>help</em>, <em>detailed_help</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxHelp(). #wxHelp{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxHelpEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxHelpEvent).
-include("wxe.hrl").
-export([getOrigin/1,getPosition/1,setOrigin/2,setPosition/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxHelpEvent()) -> Origin
%% Origin = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhelpevent.html#wxhelpeventgetorigin">external documentation</a>.
%%<br /> Origin is one of ?wxHelpEvent_Origin_Unknown | ?wxHelpEvent_Origin_Keyboard | ?wxHelpEvent_Origin_HelpButton
getOrigin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:call(?wxHelpEvent_GetOrigin,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxHelpEvent()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhelpevent.html#wxhelpeventgetposition">external documentation</a>.
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:call(?wxHelpEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxHelpEvent(), Origin::Origin) -> ok
%% Origin = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhelpevent.html#wxhelpeventsetorigin">external documentation</a>.
%%<br /> Origin is one of ?wxHelpEvent_Origin_Unknown | ?wxHelpEvent_Origin_Keyboard | ?wxHelpEvent_Origin_HelpButton
setOrigin(#wx_ref{type=ThisT,ref=ThisRef},Origin)
 when is_integer(Origin) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:cast(?wxHelpEvent_SetOrigin,
  <<ThisRef:32/?UI,Origin:32/?UI>>).

%% @spec (This::wxHelpEvent(), Pos::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhelpevent.html#wxhelpeventsetposition">external documentation</a>.
setPosition(#wx_ref{type=ThisT,ref=ThisRef},{PosX,PosY})
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:cast(?wxHelpEvent_SetPosition,
  <<ThisRef:32/?UI,PosX:32/?UI,PosY:32/?UI>>).

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
