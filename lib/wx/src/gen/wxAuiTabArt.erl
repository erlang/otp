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

-module(wxAuiTabArt).
-include("wxe.hrl").
-export([setActiveColour/2,setColour/2,setFlags/2,setMeasuringFont/2,setNormalFont/2,
  setSelectedFont/2]).

%% inherited exports
-export([parent_class/1]).

-type wxAuiTabArt() :: wx:wx_object().
-export_type([wxAuiTabArt/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxAuiTabArt(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxAuiTabArt),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxAuiTabArt_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetmeasuringfont">external documentation</a>.
-spec setMeasuringFont(This, Font) -> 'ok' when
	This::wxAuiTabArt(), Font::wxFont:wxFont().
setMeasuringFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxAuiTabArt),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxAuiTabArt_SetMeasuringFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetnormalfont">external documentation</a>.
-spec setNormalFont(This, Font) -> 'ok' when
	This::wxAuiTabArt(), Font::wxFont:wxFont().
setNormalFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxAuiTabArt),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxAuiTabArt_SetNormalFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetselectedfont">external documentation</a>.
-spec setSelectedFont(This, Font) -> 'ok' when
	This::wxAuiTabArt(), Font::wxFont:wxFont().
setSelectedFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxAuiTabArt),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxAuiTabArt_SetSelectedFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetcolour">external documentation</a>.
-spec setColour(This, Colour) -> 'ok' when
	This::wxAuiTabArt(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxAuiTabArt),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxAuiTabArt_SetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetactivecolour">external documentation</a>.
-spec setActiveColour(This, Colour) -> 'ok' when
	This::wxAuiTabArt(), Colour::wx:wx_colour().
setActiveColour(#wx_ref{type=ThisT}=This,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxAuiTabArt),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxAuiTabArt_SetActiveColour).

