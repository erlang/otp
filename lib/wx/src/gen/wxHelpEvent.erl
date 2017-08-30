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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html">wxHelpEvent</a>.
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

-export_type([wxHelpEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxHelpEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html#wxhelpeventgetorigin">external documentation</a>.
%%<br /> Res = ?wxHelpEvent_Origin_Unknown | ?wxHelpEvent_Origin_Keyboard | ?wxHelpEvent_Origin_HelpButton
-spec getOrigin(This) -> wx:wx_enum() when
	This::wxHelpEvent().
getOrigin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:call(?wxHelpEvent_GetOrigin,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html#wxhelpeventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxHelpEvent().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:call(?wxHelpEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html#wxhelpeventsetorigin">external documentation</a>.
%%<br /> Origin = ?wxHelpEvent_Origin_Unknown | ?wxHelpEvent_Origin_Keyboard | ?wxHelpEvent_Origin_HelpButton
-spec setOrigin(This, Origin) -> 'ok' when
	This::wxHelpEvent(), Origin::wx:wx_enum().
setOrigin(#wx_ref{type=ThisT,ref=ThisRef},Origin)
 when is_integer(Origin) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:cast(?wxHelpEvent_SetOrigin,
  <<ThisRef:32/?UI,Origin:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html#wxhelpeventsetposition">external documentation</a>.
-spec setPosition(This, Pos) -> 'ok' when
	This::wxHelpEvent(), Pos::{X::integer(), Y::integer()}.
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
