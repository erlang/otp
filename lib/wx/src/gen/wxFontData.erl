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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html">wxFontData</a>.
%% @type wxFontData().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxFontData).
-include("wxe.hrl").
-export([destroy/1,enableEffects/2,getAllowSymbols/1,getChosenFont/1,getColour/1,
  getEnableEffects/1,getInitialFont/1,getShowHelp/1,new/0,new/1,setAllowSymbols/2,
  setChosenFont/2,setColour/2,setInitialFont/2,setRange/3,setShowHelp/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxFontData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxFontData() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatawxfontdata">external documentation</a>.
-spec new() -> wxFontData().
new() ->
  wxe_util:construct(?wxFontData_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatawxfontdata">external documentation</a>.
-spec new(Data) -> wxFontData() when
	Data::wxFontData().
new(#wx_ref{type=DataT,ref=DataRef}) ->
  ?CLASS(DataT,wxFontData),
  wxe_util:construct(?wxFontData_new_1,
  <<DataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdataenableeffects">external documentation</a>.
-spec enableEffects(This, Flag) -> 'ok' when
	This::wxFontData(), Flag::boolean().
enableEffects(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_EnableEffects,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetallowsymbols">external documentation</a>.
-spec getAllowSymbols(This) -> boolean() when
	This::wxFontData().
getAllowSymbols(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetAllowSymbols,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetcolour">external documentation</a>.
-spec getColour(This) -> wx:wx_colour4() when
	This::wxFontData().
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetchosenfont">external documentation</a>.
-spec getChosenFont(This) -> wxFont:wxFont() when
	This::wxFontData().
getChosenFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetChosenFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetenableeffects">external documentation</a>.
-spec getEnableEffects(This) -> boolean() when
	This::wxFontData().
getEnableEffects(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetEnableEffects,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetinitialfont">external documentation</a>.
-spec getInitialFont(This) -> wxFont:wxFont() when
	This::wxFontData().
getInitialFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetInitialFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetshowhelp">external documentation</a>.
-spec getShowHelp(This) -> boolean() when
	This::wxFontData().
getShowHelp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetShowHelp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetallowsymbols">external documentation</a>.
-spec setAllowSymbols(This, Flag) -> 'ok' when
	This::wxFontData(), Flag::boolean().
setAllowSymbols(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_SetAllowSymbols,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetchosenfont">external documentation</a>.
-spec setChosenFont(This, Font) -> 'ok' when
	This::wxFontData(), Font::wxFont:wxFont().
setChosenFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxFontData),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxFontData_SetChosenFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetcolour">external documentation</a>.
-spec setColour(This, Colour) -> 'ok' when
	This::wxFontData(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_SetColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetinitialfont">external documentation</a>.
-spec setInitialFont(This, Font) -> 'ok' when
	This::wxFontData(), Font::wxFont:wxFont().
setInitialFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxFontData),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxFontData_SetInitialFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetrange">external documentation</a>.
-spec setRange(This, MinRange, MaxRange) -> 'ok' when
	This::wxFontData(), MinRange::integer(), MaxRange::integer().
setRange(#wx_ref{type=ThisT,ref=ThisRef},MinRange,MaxRange)
 when is_integer(MinRange),is_integer(MaxRange) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_SetRange,
  <<ThisRef:32/?UI,MinRange:32/?UI,MaxRange:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetshowhelp">external documentation</a>.
-spec setShowHelp(This, Flag) -> 'ok' when
	This::wxFontData(), Flag::boolean().
setShowHelp(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_SetShowHelp,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxFontData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFontData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
