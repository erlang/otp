%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(wxQueryNewPaletteEvent).
-moduledoc """
Functions for wxQueryNewPaletteEvent class

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxQueryNewPaletteEvent](https://docs.wxwidgets.org/3.2/classwx_query_new_palette_event.html)
""".
-include("wxe.hrl").
-export([getPaletteRealized/1,setPaletteRealized/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxQueryNewPaletteEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxQueryNewPaletteEventType() :: 'query_new_palette'.
-export_type([wxQueryNewPaletteEvent/0, wxQueryNewPalette/0, wxQueryNewPaletteEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "".
-spec setPaletteRealized(This, Realized) -> 'ok' when
	This::wxQueryNewPaletteEvent(), Realized::boolean().
setPaletteRealized(#wx_ref{type=ThisT}=This,Realized)
 when is_boolean(Realized) ->
  ?CLASS(ThisT,wxQueryNewPaletteEvent),
  wxe_util:queue_cmd(This,Realized,?get_env(),?wxQueryNewPaletteEvent_SetPaletteRealized).

-doc "".
-spec getPaletteRealized(This) -> boolean() when
	This::wxQueryNewPaletteEvent().
getPaletteRealized(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxQueryNewPaletteEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxQueryNewPaletteEvent_GetPaletteRealized),
  wxe_util:rec(?wxQueryNewPaletteEvent_GetPaletteRealized).

 %% From wxEvent
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
-doc false.
skip(This) -> wxEvent:skip(This).
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
-doc false.
getId(This) -> wxEvent:getId(This).
