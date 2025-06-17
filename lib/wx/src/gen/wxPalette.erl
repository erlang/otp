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

-module(wxPalette).
-moduledoc """
A palette is a table that maps pixel values to RGB colours.

It allows the colours of a low-depth bitmap, for example, to be mapped to the available
colours in a display. The notion of palettes is becoming more and more obsolete nowadays
and only the MSW port is still using a native palette. All other ports use generic code
which is basically just an array of colours.

It is likely that in the future the only use for palettes within wxWidgets will be for
representing colour indices from images (such as GIF or PNG). The image handlers for these
formats have been modified to create a palette if there is such information in the
original image file (usually 256 or less colour images). See `m:wxImage` for more information.

Predefined objects (include wx.hrl): ?wxNullPalette

See:
* `wxDC:setPalette/2`

* `m:wxBitmap`

wxWidgets docs: [wxPalette](https://docs.wxwidgets.org/3.2/classwx_palette.html)
""".
-include("wxe.hrl").
-export([create/4,destroy/1,getColoursCount/1,getPixel/4,getRGB/2,isOk/1,new/0,
  new/1,new/3,ok/1]).

%% inherited exports
-export([parent_class/1]).

-type wxPalette() :: wx:wx_object().
-export_type([wxPalette/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxPalette().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxPalette_new_0),
  wxe_util:rec(?wxPalette_new_0).

-doc "Copy constructor, uses overview\_refcount.".
-spec new(Palette) -> wxPalette() when
	Palette::wxPalette().
new(#wx_ref{type=PaletteT}=Palette) ->
  ?CLASS(PaletteT,wxPalette),
  wxe_util:queue_cmd(Palette,?get_env(),?wxPalette_new_1),
  wxe_util:rec(?wxPalette_new_1).

-doc """
Creates a palette from arrays of size `n`, one for each red, blue or green component.

See: `create/4`
""".
-spec new(Red, Green, Blue) -> wxPalette() when
	Red::binary(), Green::binary(), Blue::binary().
new(Red,Green,Blue)
 when is_binary(Red),is_binary(Green),is_binary(Blue) ->
  wxe_util:queue_cmd(Red,Green,Blue,?get_env(),?wxPalette_new_4),
  wxe_util:rec(?wxPalette_new_4).

-doc """
Creates a palette from arrays of size `n`, one for each red, blue or green component.

Return: true if the creation was successful, false otherwise.

See: `new/3`
""".
-spec create(This, Red, Green, Blue) -> boolean() when
	This::wxPalette(), Red::binary(), Green::binary(), Blue::binary().
create(#wx_ref{type=ThisT}=This,Red,Green,Blue)
 when is_binary(Red),is_binary(Green),is_binary(Blue) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,Red,Green,Blue,?get_env(),?wxPalette_Create),
  wxe_util:rec(?wxPalette_Create).

-doc "Returns number of entries in palette.".
-spec getColoursCount(This) -> integer() when
	This::wxPalette().
getColoursCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,?get_env(),?wxPalette_GetColoursCount),
  wxe_util:rec(?wxPalette_GetColoursCount).

-doc """
Returns a pixel value (index into the palette) for the given RGB values.

Return: The nearest palette index or `wxNOT_FOUND` for unexpected errors.

See: `getRGB/2`
""".
-spec getPixel(This, Red, Green, Blue) -> integer() when
	This::wxPalette(), Red::integer(), Green::integer(), Blue::integer().
getPixel(#wx_ref{type=ThisT}=This,Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,Red,Green,Blue,?get_env(),?wxPalette_GetPixel),
  wxe_util:rec(?wxPalette_GetPixel).

-doc """
Returns RGB values for a given palette index.

Return: true if the operation was successful.

See: `getPixel/4`
""".
-spec getRGB(This, Pixel) -> Result when
	Result ::{Res ::boolean(), Red::integer(), Green::integer(), Blue::integer()},
	This::wxPalette(), Pixel::integer().
getRGB(#wx_ref{type=ThisT}=This,Pixel)
 when is_integer(Pixel) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,Pixel,?get_env(),?wxPalette_GetRGB),
  wxe_util:rec(?wxPalette_GetRGB).

-doc "Equivalent to: `isOk/1`".
-spec ok(This) -> boolean() when
	This::wxPalette().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

-doc "Returns true if palette data is present.".
-spec isOk(This) -> boolean() when
	This::wxPalette().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPalette),
  wxe_util:queue_cmd(This,?get_env(),?wxPalette_IsOk),
  wxe_util:rec(?wxPalette_IsOk).

-doc "Destroys the object".
-spec destroy(This::wxPalette()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPalette),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
