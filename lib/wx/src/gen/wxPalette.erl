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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpalette.html">wxPalette</a>.
%% @type wxPalette().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPalette).
-include("wxe.hrl").
-export([create/4,destroy/1,getColoursCount/1,getPixel/4,getRGB/2,isOk/1,new/0,
  new/3]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxPalette()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpalette.html#wxpalettewxpalette">external documentation</a>.
new() ->
  wxe_util:construct(?wxPalette_new_0,
  <<>>).

%% @spec (Red::binary(), Green::binary(), Blue::binary()) -> wxPalette()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpalette.html#wxpalettewxpalette">external documentation</a>.
new(Red,Green,Blue)
 when is_binary(Red),is_binary(Green),is_binary(Blue) ->
  wxe_util:send_bin(Red),
  wxe_util:send_bin(Green),
  wxe_util:send_bin(Blue),
  wxe_util:construct(?wxPalette_new_4,
  <<>>).

%% @spec (This::wxPalette(), Red::binary(), Green::binary(), Blue::binary()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpalette.html#wxpalettecreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},Red,Green,Blue)
 when is_binary(Red),is_binary(Green),is_binary(Blue) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:send_bin(Red),
  wxe_util:send_bin(Green),
  wxe_util:send_bin(Blue),
  wxe_util:call(?wxPalette_Create,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPalette()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpalette.html#wxpalettegetcolourscount">external documentation</a>.
getColoursCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:call(?wxPalette_GetColoursCount,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPalette(), Red::integer(), Green::integer(), Blue::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpalette.html#wxpalettegetpixel">external documentation</a>.
getPixel(#wx_ref{type=ThisT,ref=ThisRef},Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:call(?wxPalette_GetPixel,
  <<ThisRef:32/?UI,Red:32/?UI,Green:32/?UI,Blue:32/?UI>>).

%% @spec (This::wxPalette(), Pixel::integer()) -> {bool(),Red::integer(),Green::integer(),Blue::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpalette.html#wxpalettegetrgb">external documentation</a>.
getRGB(#wx_ref{type=ThisT,ref=ThisRef},Pixel)
 when is_integer(Pixel) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:call(?wxPalette_GetRGB,
  <<ThisRef:32/?UI,Pixel:32/?UI>>).

%% @spec (This::wxPalette()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpalette.html#wxpaletteisok">external documentation</a>.
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:call(?wxPalette_IsOk,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPalette()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPalette),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
