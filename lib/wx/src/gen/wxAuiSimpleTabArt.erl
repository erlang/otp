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

-module(wxAuiSimpleTabArt).
-moduledoc """
Functions for wxAuiSimpleTabArt class

Another standard tab art provider for `m:wxAuiNotebook`.

`m:wxAuiSimpleTabArt` is derived from `m:wxAuiTabArt` demonstrating how to write
a completely new tab art class. It can also be used as alternative to
`wxAuiDefaultTabArt` (not implemented in wx).

This class is derived (and can use functions) from: `m:wxAuiTabArt`

wxWidgets docs:
[wxAuiSimpleTabArt](https://docs.wxwidgets.org/3.1/classwx_aui_simple_tab_art.html)
""".
-include("wxe.hrl").
-export([destroy/1,new/0]).

%% inherited exports
-export([parent_class/1,setActiveColour/2,setColour/2,setFlags/2,setMeasuringFont/2,
  setNormalFont/2,setSelectedFont/2]).

-type wxAuiSimpleTabArt() :: wx:wx_object().
-export_type([wxAuiSimpleTabArt/0]).
%% @hidden
-doc false.
parent_class(wxAuiTabArt) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauisimpletabart.html#wxauisimpletabartwxauisimpletabart">external documentation</a>.
-spec new() -> wxAuiSimpleTabArt().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxAuiSimpleTabArt_new),
  wxe_util:rec(?wxAuiSimpleTabArt_new).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxAuiSimpleTabArt()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiSimpleTabArt),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxAuiSimpleTabArt_destroy),
  ok.
 %% From wxAuiTabArt
%% @hidden
-doc false.
setActiveColour(This,Colour) -> wxAuiTabArt:setActiveColour(This,Colour).
%% @hidden
-doc false.
setColour(This,Colour) -> wxAuiTabArt:setColour(This,Colour).
%% @hidden
-doc false.
setSelectedFont(This,Font) -> wxAuiTabArt:setSelectedFont(This,Font).
%% @hidden
-doc false.
setNormalFont(This,Font) -> wxAuiTabArt:setNormalFont(This,Font).
%% @hidden
-doc false.
setMeasuringFont(This,Font) -> wxAuiTabArt:setMeasuringFont(This,Font).
%% @hidden
-doc false.
setFlags(This,Flags) -> wxAuiTabArt:setFlags(This,Flags).
