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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html">wxFontData</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxFontData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatawxfontdata">external documentation</a>.
new() ->
  wxe_util:construct(?wxFontData_new_0,
  <<>>).

%% @spec (Data::wxFontData()) -> wxFontData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatawxfontdata">external documentation</a>.
new(#wx_ref{type=DataT,ref=DataRef}) ->
  ?CLASS(DataT,wxFontData),
  wxe_util:construct(?wxFontData_new_1,
  <<DataRef:32/?UI>>).

%% @spec (This::wxFontData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdataenableeffects">external documentation</a>.
enableEffects(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_EnableEffects,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxFontData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatagetallowsymbols">external documentation</a>.
getAllowSymbols(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetAllowSymbols,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFontData()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatagetcolour">external documentation</a>.
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFontData()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatagetchosenfont">external documentation</a>.
getChosenFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetChosenFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFontData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatagetenableeffects">external documentation</a>.
getEnableEffects(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetEnableEffects,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFontData()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatagetinitialfont">external documentation</a>.
getInitialFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetInitialFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFontData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatagetshowhelp">external documentation</a>.
getShowHelp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:call(?wxFontData_GetShowHelp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFontData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatasetallowsymbols">external documentation</a>.
setAllowSymbols(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_SetAllowSymbols,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxFontData(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatasetchosenfont">external documentation</a>.
setChosenFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxFontData),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxFontData_SetChosenFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxFontData(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatasetcolour">external documentation</a>.
setColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_SetColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxFontData(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatasetinitialfont">external documentation</a>.
setInitialFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxFontData),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxFontData_SetInitialFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxFontData(), MinRange::integer(), MaxRange::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatasetrange">external documentation</a>.
setRange(#wx_ref{type=ThisT,ref=ThisRef},MinRange,MaxRange)
 when is_integer(MinRange),is_integer(MaxRange) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_SetRange,
  <<ThisRef:32/?UI,MinRange:32/?UI,MaxRange:32/?UI>>).

%% @spec (This::wxFontData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfontdata.html#wxfontdatasetshowhelp">external documentation</a>.
setShowHelp(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:cast(?wxFontData_SetShowHelp,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxFontData()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFontData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
