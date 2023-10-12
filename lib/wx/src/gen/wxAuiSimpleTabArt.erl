%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
-include("wxe.hrl").
-export([destroy/1,new/0]).

%% inherited exports
-export([parent_class/1,setActiveColour/2,setColour/2,setFlags/2,setMeasuringFont/2,
  setNormalFont/2,setSelectedFont/2]).

-type wxAuiSimpleTabArt() :: wx:wx_object().
-export_type([wxAuiSimpleTabArt/0]).
%% @hidden
parent_class(wxAuiTabArt) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauisimpletabart.html#wxauisimpletabartwxauisimpletabart">external documentation</a>.
-spec new() -> wxAuiSimpleTabArt().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxAuiSimpleTabArt_new),
  wxe_util:rec(?wxAuiSimpleTabArt_new).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxAuiSimpleTabArt()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiSimpleTabArt),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxAuiSimpleTabArt_destroy),
  ok.
 %% From wxAuiTabArt
%% @hidden
setActiveColour(This,Colour) -> wxAuiTabArt:setActiveColour(This,Colour).
%% @hidden
setColour(This,Colour) -> wxAuiTabArt:setColour(This,Colour).
%% @hidden
setSelectedFont(This,Font) -> wxAuiTabArt:setSelectedFont(This,Font).
%% @hidden
setNormalFont(This,Font) -> wxAuiTabArt:setNormalFont(This,Font).
%% @hidden
setMeasuringFont(This,Font) -> wxAuiTabArt:setMeasuringFont(This,Font).
%% @hidden
setFlags(This,Flags) -> wxAuiTabArt:setFlags(This,Flags).
