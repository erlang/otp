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

-module(wxPalette).
-include("wxe.hrl").
-export([create/4,destroy/1,getColoursCount/1,getPixel/4,getRGB/2,isOk/1,new/0,
  new/1,new/3,ok/1]).

%% inherited exports
-export([parent_class/1]).

-type wxPalette() :: wx:wx_object().
-export_type([wxPalette/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettewxpalette">external documentation</a>.
-spec new() -> wxPalette().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxPalette_new_0),
  wxe_util:rec(?wxPalette_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettewxpalette">external documentation</a>.
-spec new(Palette) -> wxPalette() when
	Palette::wxPalette().
new(#wx_ref{type=PaletteT}=Palette) ->
  ?CLASS(PaletteT,wxPalette),
  wxe_util:queue_cmd(Palette,?get_env(),?wxPalette_new_1),
  wxe_util:rec(?wxPalette_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettewxpalette">external documentation</a>.
-spec new(Red, Green, Blue) -> wxPalette() when
	Red::binary(), Green::binary(), Blue::binary().
new(Red,Green,Blue)
 when is_binary(Red),is_binary(Green),is_binary(Blue) ->
  wxe_util:queue_cmd(Red,Green,Blue,?get_env(),?wxPalette_new_4),
  wxe_util:rec(?wxPalette_new_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettecreate">external documentation</a>.
-spec create(This, Red, Green, Blue) -> boolean() when
	This::wxPalette(), Red::binary(), Green::binary(), Blue::binary().
create(#wx_ref{type=ThisT}=This,Red,Green,Blue)
 when is_binary(Red),is_binary(Green),is_binary(Blue) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,Red,Green,Blue,?get_env(),?wxPalette_Create),
  wxe_util:rec(?wxPalette_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettegetcolourscount">external documentation</a>.
-spec getColoursCount(This) -> integer() when
	This::wxPalette().
getColoursCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,?get_env(),?wxPalette_GetColoursCount),
  wxe_util:rec(?wxPalette_GetColoursCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettegetpixel">external documentation</a>.
-spec getPixel(This, Red, Green, Blue) -> integer() when
	This::wxPalette(), Red::integer(), Green::integer(), Blue::integer().
getPixel(#wx_ref{type=ThisT}=This,Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,Red,Green,Blue,?get_env(),?wxPalette_GetPixel),
  wxe_util:rec(?wxPalette_GetPixel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettegetrgb">external documentation</a>.
-spec getRGB(This, Pixel) -> Result when
	Result ::{Res ::boolean(), Red::integer(), Green::integer(), Blue::integer()},
	This::wxPalette(), Pixel::integer().
getRGB(#wx_ref{type=ThisT}=This,Pixel)
 when is_integer(Pixel) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,Pixel,?get_env(),?wxPalette_GetRGB),
  wxe_util:rec(?wxPalette_GetRGB).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpaletteisok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxPalette().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpaletteisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPalette().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,?get_env(),?wxPalette_IsOk),
  wxe_util:rec(?wxPalette_IsOk).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPalette()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPalette),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
