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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html">wxPalette</a>.
%% @type wxPalette().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPalette).
-include("wxe.hrl").
-export([create/4,destroy/1,getColoursCount/1,getPixel/4,getRGB/2,isOk/1,new/0,
  new/3]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxPalette/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPalette() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettewxpalette">external documentation</a>.
-spec new() -> wxPalette().
new() ->
  wxe_util:construct(?wxPalette_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettewxpalette">external documentation</a>.
-spec new(Red, Green, Blue) -> wxPalette() when
	Red::binary(), Green::binary(), Blue::binary().
new(Red,Green,Blue)
 when is_binary(Red),is_binary(Green),is_binary(Blue) ->
  wxe_util:send_bin(Red),
  wxe_util:send_bin(Green),
  wxe_util:send_bin(Blue),
  wxe_util:construct(?wxPalette_new_4,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettecreate">external documentation</a>.
-spec create(This, Red, Green, Blue) -> boolean() when
	This::wxPalette(), Red::binary(), Green::binary(), Blue::binary().
create(#wx_ref{type=ThisT,ref=ThisRef},Red,Green,Blue)
 when is_binary(Red),is_binary(Green),is_binary(Blue) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:send_bin(Red),
  wxe_util:send_bin(Green),
  wxe_util:send_bin(Blue),
  wxe_util:call(?wxPalette_Create,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettegetcolourscount">external documentation</a>.
-spec getColoursCount(This) -> integer() when
	This::wxPalette().
getColoursCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:call(?wxPalette_GetColoursCount,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettegetpixel">external documentation</a>.
-spec getPixel(This, Red, Green, Blue) -> integer() when
	This::wxPalette(), Red::integer(), Green::integer(), Blue::integer().
getPixel(#wx_ref{type=ThisT,ref=ThisRef},Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:call(?wxPalette_GetPixel,
  <<ThisRef:32/?UI,Red:32/?UI,Green:32/?UI,Blue:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpalettegetrgb">external documentation</a>.
-spec getRGB(This, Pixel) -> Result when
	Result ::{Res ::boolean(), Red::integer(), Green::integer(), Blue::integer()},
	This::wxPalette(), Pixel::integer().
getRGB(#wx_ref{type=ThisT,ref=ThisRef},Pixel)
 when is_integer(Pixel) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:call(?wxPalette_GetRGB,
  <<ThisRef:32/?UI,Pixel:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpalette.html#wxpaletteisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPalette().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:call(?wxPalette_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPalette()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPalette),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
