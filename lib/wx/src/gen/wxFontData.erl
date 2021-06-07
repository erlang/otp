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

-module(wxFontData).
-include("wxe.hrl").
-export([destroy/1,enableEffects/2,getAllowSymbols/1,getChosenFont/1,getColour/1,
  getEnableEffects/1,getInitialFont/1,getShowHelp/1,new/0,new/1,setAllowSymbols/2,
  setChosenFont/2,setColour/2,setInitialFont/2,setRange/3,setShowHelp/2]).

%% inherited exports
-export([parent_class/1]).

-type wxFontData() :: wx:wx_object().
-export_type([wxFontData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatawxfontdata">external documentation</a>.
-spec new() -> wxFontData().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxFontData_new_0),
  wxe_util:rec(?wxFontData_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatawxfontdata">external documentation</a>.
-spec new(Data) -> wxFontData() when
	Data::wxFontData().
new(#wx_ref{type=DataT}=Data) ->
  ?CLASS(DataT,wxFontData),
  wxe_util:queue_cmd(Data,?get_env(),?wxFontData_new_1),
  wxe_util:rec(?wxFontData_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdataenableeffects">external documentation</a>.
-spec enableEffects(This, Enable) -> 'ok' when
	This::wxFontData(), Enable::boolean().
enableEffects(#wx_ref{type=ThisT}=This,Enable)
 when is_boolean(Enable) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,Enable,?get_env(),?wxFontData_EnableEffects).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetallowsymbols">external documentation</a>.
-spec getAllowSymbols(This) -> boolean() when
	This::wxFontData().
getAllowSymbols(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetAllowSymbols),
  wxe_util:rec(?wxFontData_GetAllowSymbols).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetcolour">external documentation</a>.
-spec getColour(This) -> wx:wx_colour4() when
	This::wxFontData().
getColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetColour),
  wxe_util:rec(?wxFontData_GetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetchosenfont">external documentation</a>.
-spec getChosenFont(This) -> wxFont:wxFont() when
	This::wxFontData().
getChosenFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetChosenFont),
  wxe_util:rec(?wxFontData_GetChosenFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetenableeffects">external documentation</a>.
-spec getEnableEffects(This) -> boolean() when
	This::wxFontData().
getEnableEffects(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetEnableEffects),
  wxe_util:rec(?wxFontData_GetEnableEffects).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetinitialfont">external documentation</a>.
-spec getInitialFont(This) -> wxFont:wxFont() when
	This::wxFontData().
getInitialFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetInitialFont),
  wxe_util:rec(?wxFontData_GetInitialFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatagetshowhelp">external documentation</a>.
-spec getShowHelp(This) -> boolean() when
	This::wxFontData().
getShowHelp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,?get_env(),?wxFontData_GetShowHelp),
  wxe_util:rec(?wxFontData_GetShowHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetallowsymbols">external documentation</a>.
-spec setAllowSymbols(This, AllowSymbols) -> 'ok' when
	This::wxFontData(), AllowSymbols::boolean().
setAllowSymbols(#wx_ref{type=ThisT}=This,AllowSymbols)
 when is_boolean(AllowSymbols) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,AllowSymbols,?get_env(),?wxFontData_SetAllowSymbols).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetchosenfont">external documentation</a>.
-spec setChosenFont(This, Font) -> 'ok' when
	This::wxFontData(), Font::wxFont:wxFont().
setChosenFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxFontData),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxFontData_SetChosenFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetcolour">external documentation</a>.
-spec setColour(This, Colour) -> 'ok' when
	This::wxFontData(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxFontData_SetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetinitialfont">external documentation</a>.
-spec setInitialFont(This, Font) -> 'ok' when
	This::wxFontData(), Font::wxFont:wxFont().
setInitialFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxFontData),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxFontData_SetInitialFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetrange">external documentation</a>.
-spec setRange(This, Min, Max) -> 'ok' when
	This::wxFontData(), Min::integer(), Max::integer().
setRange(#wx_ref{type=ThisT}=This,Min,Max)
 when is_integer(Min),is_integer(Max) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,Min,Max,?get_env(),?wxFontData_SetRange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfontdata.html#wxfontdatasetshowhelp">external documentation</a>.
-spec setShowHelp(This, ShowHelp) -> 'ok' when
	This::wxFontData(), ShowHelp::boolean().
setShowHelp(#wx_ref{type=ThisT}=This,ShowHelp)
 when is_boolean(ShowHelp) ->
  ?CLASS(ThisT,wxFontData),
  wxe_util:queue_cmd(This,ShowHelp,?get_env(),?wxFontData_SetShowHelp).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxFontData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFontData),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
