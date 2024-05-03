%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(wxPaletteChangedEvent).
-moduledoc """
Functions for wxPaletteChangedEvent class

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxPaletteChangedEvent](https://docs.wxwidgets.org/3.1/classwx_palette_changed_event.html)
""".
-include("wxe.hrl").
-export([getChangedWindow/1,setChangedWindow/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxPaletteChangedEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxPaletteChangedEventType() :: 'palette_changed'.
-export_type([wxPaletteChangedEvent/0, wxPaletteChanged/0, wxPaletteChangedEventType/0]).
%% @hidden
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalettechangedevent.html#wxpalettechangedeventsetchangedwindow">external documentation</a>.
-spec setChangedWindow(This, Win) -> 'ok' when
	This::wxPaletteChangedEvent(), Win::wxWindow:wxWindow().
setChangedWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=WinT}=Win) ->
  ?CLASS(ThisT,wxPaletteChangedEvent),
  ?CLASS(WinT,wxWindow),
  wxe_util:queue_cmd(This,Win,?get_env(),?wxPaletteChangedEvent_SetChangedWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalettechangedevent.html#wxpalettechangedeventgetchangedwindow">external documentation</a>.
-spec getChangedWindow(This) -> wxWindow:wxWindow() when
	This::wxPaletteChangedEvent().
getChangedWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPaletteChangedEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxPaletteChangedEvent_GetChangedWindow),
  wxe_util:rec(?wxPaletteChangedEvent_GetChangedWindow).

 %% From wxEvent
%% @hidden
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
-doc false.
skip(This) -> wxEvent:skip(This).
%% @hidden
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
-doc false.
getId(This) -> wxEvent:getId(This).
