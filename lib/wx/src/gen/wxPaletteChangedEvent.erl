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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalettechangedevent.html">wxPaletteChangedEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>palette_changed</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxPaletteChanged(). #wxPaletteChanged{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxPaletteChangedEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPaletteChangedEvent).
-include("wxe.hrl").
-export([getChangedWindow/1,setChangedWindow/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-export_type([wxPaletteChangedEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPaletteChangedEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalettechangedevent.html#wxpalettechangedeventsetchangedwindow">external documentation</a>.
-spec setChangedWindow(This, Win) -> 'ok' when
	This::wxPaletteChangedEvent(), Win::wxWindow:wxWindow().
setChangedWindow(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(ThisT,wxPaletteChangedEvent),
  ?CLASS(WinT,wxWindow),
  wxe_util:cast(?wxPaletteChangedEvent_SetChangedWindow,
  <<ThisRef:32/?UI,WinRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalettechangedevent.html#wxpalettechangedeventgetchangedwindow">external documentation</a>.
-spec getChangedWindow(This) -> wxWindow:wxWindow() when
	This::wxPaletteChangedEvent().
getChangedWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPaletteChangedEvent),
  wxe_util:call(?wxPaletteChangedEvent_GetChangedWindow,
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
