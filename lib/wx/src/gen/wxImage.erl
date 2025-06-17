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

%%
%% All (default) image handlers are initialized.

%%
-module(wxImage).
-moduledoc """
This class encapsulates a platform-independent image.

An image can be created from data, or using `wxBitmap:convertToImage/1`. An image can be loaded from a file in a
variety of formats, and is extensible to new formats via image format handlers. Functions
are available to set and get image bits, so it can be used for basic image manipulation.

A `m:wxImage` cannot (currently) be drawn directly to a `m:wxDC`. Instead, a
platform-specific `m:wxBitmap` object must be created from it using the
wxBitmap::wxBitmap(wxImage,int depth) constructor. This bitmap can then be drawn in a
device context, using `wxDC:drawBitmap/4`.

More on the difference between `m:wxImage` and `m:wxBitmap`: `m:wxImage` is just a buffer
of RGB bytes with an optional buffer for the alpha bytes. It is all generic, platform
independent and image file format independent code. It includes generic code for scaling,
resizing, clipping, and other manipulations of the image data. OTOH, `m:wxBitmap` is
intended to be a wrapper of whatever is the native image format that is quickest/easiest
to draw to a DC or to be the target of the drawing operations performed on a `m:wxMemoryDC`.
By splitting the responsibilities between wxImage/wxBitmap like this then it's easier to
use generic code shared by all platforms and image types for generic operations and
platform specific code where performance or compatibility is needed.

One colour value of the image may be used as a mask colour which will lead to the
automatic creation of a `m:wxMask` object associated to the bitmap object.

Alpha channel support

Starting from wxWidgets 2.5.0 `m:wxImage` supports alpha channel data, that is in
addition to a byte for the red, green and blue colour components for each pixel it also
stores a byte representing the pixel opacity.

An alpha value of 0 corresponds to a transparent pixel (null opacity) while a value of
255 means that the pixel is 100% opaque. The constants ?wxIMAGE\_ALPHA\_TRANSPARENT and
?wxIMAGE\_ALPHA\_OPAQUE can be used to indicate those values in a more readable form.

While all images have RGB data, not all images have an alpha channel. Before using `getAlpha/3` you
should check if this image contains an alpha channel with `hasAlpha/1`. Currently the BMP, PNG, TGA,
and TIFF format handlers have full alpha channel support for loading so if you want to use
alpha you have to use one of these formats. If you initialize the image alpha channel
yourself using `setAlpha/4`, you should save it in either PNG, TGA, or TIFF format to avoid losing it
as these are the only handlers that currently support saving with alpha.

Available image handlers

The following image handlers are available. wxBMPHandler is always installed by default.
To use other image formats, install the appropriate handler with `wxImage::AddHandler`
(not implemented in wx) or call ?wxInitAllImageHandlers().

* wxBMPHandler: For loading (including alpha support) and saving, always installed.

* `wxPNGHandler` (not implemented in wx): For loading and saving. Includes alpha support.

* `wxJPEGHandler` (not implemented in wx): For loading and saving.

* `wxGIFHandler` (not implemented in wx): For loading and saving (see below).

* `wxPCXHandler` (not implemented in wx): For loading and saving (see below).

* `wxPNMHandler` (not implemented in wx): For loading and saving (see below).

* `wxTIFFHandler` (not implemented in wx): For loading and saving. Includes alpha support.

* `wxTGAHandler` (not implemented in wx): For loading and saving. Includes alpha support.

* `wxIFFHandler` (not implemented in wx): For loading only.

* `wxXPMHandler` (not implemented in wx): For loading and saving.

* wxICOHandler: For loading and saving.

* wxCURHandler: For loading and saving.

* wxANIHandler: For loading only.

When saving in PCX format, `wxPCXHandler` (not implemented in wx) will count the number
of different colours in the image; if there are 256 or less colours, it will save as 8
bit, else it will save as 24 bit.

Loading PNMs only works for ASCII or raw RGB images. When saving in PNM format, `wxPNMHandler`
(not implemented in wx) will always save as raw RGB.

Saving GIFs requires images of maximum 8 bpp (see `wxQuantize` (not implemented in wx)),
and the alpha channel converted to a mask (see `convertAlphaToMask/5`). Saving an animated GIF requires images of
the same size (see `wxGIFHandler::SaveAnimation` (not implemented in wx))

Predefined objects (include wx.hrl): ?wxNullImage

See:
* `m:wxBitmap`

* ?wxInitAllImageHandlers()

wxWidgets docs: [wxImage](https://docs.wxwidgets.org/3.2/classwx_image.html)
""".
-include("wxe.hrl").
-export(['Destroy'/1,blur/2,blurHorizontal/2,blurVertical/2,convertAlphaToMask/1,
  convertAlphaToMask/2,convertAlphaToMask/4,convertAlphaToMask/5,convertToGreyscale/1,
  convertToGreyscale/4,convertToMono/4,copy/1,create/2,create/3,create/4,
  create/5,destroy/1,findFirstUnusedColour/1,findFirstUnusedColour/2,
  getAlpha/1,getAlpha/3,getBlue/3,getData/1,getGreen/3,getHeight/1,getImageCount/1,
  getImageCount/2,getImageExtWildcard/0,getMaskBlue/1,getMaskGreen/1,
  getMaskRed/1,getOption/2,getOptionInt/2,getOrFindMaskColour/1,getPalette/1,
  getRed/3,getSubImage/2,getWidth/1,hasAlpha/1,hasMask/1,hasOption/2,
  initAlpha/1,initStandardHandlers/0,isOk/1,isTransparent/3,isTransparent/4,
  loadFile/2,loadFile/3,loadFile/4,mirror/1,mirror/2,new/0,new/1,new/2,
  new/3,new/4,ok/1,removeHandler/1,replace/7,rescale/3,rescale/4,resize/3,
  resize/4,rotate/3,rotate/4,rotate90/1,rotate90/2,rotateHue/2,saveFile/2,
  saveFile/3,scale/3,scale/4,setAlpha/2,setAlpha/4,setData/2,setData/4,
  setMask/1,setMask/2,setMaskColour/4,setMaskFromImage/5,setOption/3,
  setPalette/2,setRGB/5,setRGB/6,size/3,size/4]).

%% inherited exports
-export([parent_class/1]).

-type wxImage() :: wx:wx_object().
-export_type([wxImage/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Creates an empty `m:wxImage` object without an alpha channel.".
-spec new() -> wxImage().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxImage_new_0),
  wxe_util:rec(?wxImage_new_0).

-doc "Equivalent to: `new/2`".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Name) -> wxImage() when
	Name::unicode:chardata();
      (Sz) -> wxImage() when
	Sz::{W::integer(), H::integer()}.

new(Name)
 when ?is_chardata(Name) ->
  new(Name, []);

new({SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  new(Sz, []).

-doc """
This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.
""".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Width, Height) -> wxImage() when
	Width::integer(), Height::integer();
      (Name, [Option]) -> wxImage() when
	Name::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}
		 | {'index', integer()};
      (Sz, Data) -> wxImage() when
	Sz::{W::integer(), H::integer()}, Data::binary();
      (Sz, [Option]) -> wxImage() when
	Sz::{W::integer(), H::integer()},
	Option :: {'clear', boolean()}.

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []);
new(Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({type, _type} = Arg) -> Arg;
          ({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC, Opts,?get_env(),?wxImage_new_2_0),
  wxe_util:rec(?wxImage_new_2_0);
new({SzW,SzH} = Sz,Data)
 when is_integer(SzW),is_integer(SzH),is_binary(Data) ->
  wxe_util:queue_cmd(Sz,Data,?get_env(),?wxImage_new_2_1),
  wxe_util:rec(?wxImage_new_2_1);
new({SzW,SzH} = Sz, Options)
 when is_integer(SzW),is_integer(SzH),is_list(Options) ->
  MOpts = fun({clear, _clear} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Sz, Opts,?get_env(),?wxImage_new_2_2),
  wxe_util:rec(?wxImage_new_2_2).

-doc """
This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.
""".
-spec new(Width, Height, Data) -> wxImage() when
	Width::integer(), Height::integer(), Data::binary();
      (Width, Height, [Option]) -> wxImage() when
	Width::integer(), Height::integer(),
	Option :: {'clear', boolean()};
      (Name, Mimetype, [Option]) -> wxImage() when
	Name::unicode:chardata(), Mimetype::unicode:chardata(),
	Option :: {'index', integer()};
      (Sz, Data, Alpha) -> wxImage() when
	Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary().
new(Width,Height,Data)
 when is_integer(Width),is_integer(Height),is_binary(Data) ->
  wxe_util:queue_cmd(Width,Height,Data,?get_env(),?wxImage_new_3_0),
  wxe_util:rec(?wxImage_new_3_0);
new(Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  MOpts = fun({clear, _clear} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Width,Height, Opts,?get_env(),?wxImage_new_3_1),
  wxe_util:rec(?wxImage_new_3_1);
new(Name,Mimetype, Options)
 when ?is_chardata(Name),?is_chardata(Mimetype),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  Mimetype_UC = unicode:characters_to_binary(Mimetype),
  MOpts = fun({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC,Mimetype_UC, Opts,?get_env(),?wxImage_new_3_2),
  wxe_util:rec(?wxImage_new_3_2);
new({SzW,SzH} = Sz,Data,Alpha)
 when is_integer(SzW),is_integer(SzH),is_binary(Data),is_binary(Alpha) ->
  wxe_util:queue_cmd(Sz,Data,Alpha,?get_env(),?wxImage_new_3_3),
  wxe_util:rec(?wxImage_new_3_3).

-doc """
Creates an image from data in memory.

If `static_data` is false then the `m:wxImage` will take ownership of the data and free
it afterwards. For this, it has to be allocated with `malloc`.
""".
-spec new(Width, Height, Data, Alpha) -> wxImage() when
	Width::integer(), Height::integer(), Data::binary(), Alpha::binary().
new(Width,Height,Data,Alpha)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha) ->
  wxe_util:queue_cmd(Width,Height,Data,Alpha,?get_env(),?wxImage_new_4),
  wxe_util:rec(?wxImage_new_4).

-doc """
Blurs the image in both horizontal and vertical directions by the specified pixel `blurRadius`.

This should not be used when using a single mask colour for transparency.

See:
* `blurHorizontal/2`

* `blurVertical/2`
""".
-spec blur(This, BlurRadius) -> wxImage() when
	This::wxImage(), BlurRadius::integer().
blur(#wx_ref{type=ThisT}=This,BlurRadius)
 when is_integer(BlurRadius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,BlurRadius,?get_env(),?wxImage_Blur),
  wxe_util:rec(?wxImage_Blur).

-doc """
Blurs the image in the horizontal direction only.

This should not be used when using a single mask colour for transparency.

See:
* `blur/2`

* `blurVertical/2`
""".
-spec blurHorizontal(This, BlurRadius) -> wxImage() when
	This::wxImage(), BlurRadius::integer().
blurHorizontal(#wx_ref{type=ThisT}=This,BlurRadius)
 when is_integer(BlurRadius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,BlurRadius,?get_env(),?wxImage_BlurHorizontal),
  wxe_util:rec(?wxImage_BlurHorizontal).

-doc """
Blurs the image in the vertical direction only.

This should not be used when using a single mask colour for transparency.

See:
* `blur/2`

* `blurHorizontal/2`
""".
-spec blurVertical(This, BlurRadius) -> wxImage() when
	This::wxImage(), BlurRadius::integer().
blurVertical(#wx_ref{type=ThisT}=This,BlurRadius)
 when is_integer(BlurRadius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,BlurRadius,?get_env(),?wxImage_BlurVertical),
  wxe_util:rec(?wxImage_BlurVertical).

-doc(#{equiv => convertAlphaToMask(This, [])}).
-spec convertAlphaToMask(This) -> boolean() when
	This::wxImage().

convertAlphaToMask(This)
 when is_record(This, wx_ref) ->
  convertAlphaToMask(This, []).

-doc """
If the image has alpha channel, this method converts it to mask.

If the image has an alpha channel, all pixels with alpha value less than `threshold` are
replaced with the mask colour and the alpha channel is removed. Otherwise nothing is done.

The mask colour is chosen automatically using `findFirstUnusedColour/2`, see the overload below if this is not appropriate.

Return: Returns true on success, false on error.
""".
-spec convertAlphaToMask(This, [Option]) -> boolean() when
	This::wxImage(),
	Option :: {'threshold', integer()}.
convertAlphaToMask(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({threshold, _threshold} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_ConvertAlphaToMask_1),
  wxe_util:rec(?wxImage_ConvertAlphaToMask_1).

-doc(#{equiv => convertAlphaToMask(This,Mr,Mg,Mb, [])}).
-spec convertAlphaToMask(This, Mr, Mg, Mb) -> boolean() when
	This::wxImage(), Mr::integer(), Mg::integer(), Mb::integer().

convertAlphaToMask(This,Mr,Mg,Mb)
 when is_record(This, wx_ref),is_integer(Mr),is_integer(Mg),is_integer(Mb) ->
  convertAlphaToMask(This,Mr,Mg,Mb, []).

-doc """
If the image has alpha channel, this method converts it to mask using the specified
colour as the mask colour.

If the image has an alpha channel, all pixels with alpha value less than `threshold` are
replaced with the mask colour and the alpha channel is removed. Otherwise nothing is done.

Since: 2.9.0

Return: Returns true on success, false on error.
""".
-spec convertAlphaToMask(This, Mr, Mg, Mb, [Option]) -> boolean() when
	This::wxImage(), Mr::integer(), Mg::integer(), Mb::integer(),
	Option :: {'threshold', integer()}.
convertAlphaToMask(#wx_ref{type=ThisT}=This,Mr,Mg,Mb, Options)
 when is_integer(Mr),is_integer(Mg),is_integer(Mb),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({threshold, _threshold} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Mr,Mg,Mb, Opts,?get_env(),?wxImage_ConvertAlphaToMask_4),
  wxe_util:rec(?wxImage_ConvertAlphaToMask_4).

-doc """
Returns a greyscale version of the image.

Since: 2.9.0
""".
-spec convertToGreyscale(This) -> wxImage() when
	This::wxImage().
convertToGreyscale(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_ConvertToGreyscale_0),
  wxe_util:rec(?wxImage_ConvertToGreyscale_0).

-doc """
Returns a greyscale version of the image.

The returned image uses the luminance component of the original to calculate the
greyscale. Defaults to using the standard ITU-T BT.601 when converting to YUV, where every
pixel equals (R * `weight_r`) + (G * `weight_g`) + (B * `weight_b`).
""".
-spec convertToGreyscale(This, Weight_r, Weight_g, Weight_b) -> wxImage() when
	This::wxImage(), Weight_r::number(), Weight_g::number(), Weight_b::number().
convertToGreyscale(#wx_ref{type=ThisT}=This,Weight_r,Weight_g,Weight_b)
 when is_number(Weight_r),is_number(Weight_g),is_number(Weight_b) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Weight_r,Weight_g,Weight_b,?get_env(),?wxImage_ConvertToGreyscale_3),
  wxe_util:rec(?wxImage_ConvertToGreyscale_3).

-doc """
Returns monochromatic version of the image.

The returned image has white colour where the original has (r,g,b) colour and black
colour everywhere else.
""".
-spec convertToMono(This, R, G, B) -> wxImage() when
	This::wxImage(), R::integer(), G::integer(), B::integer().
convertToMono(#wx_ref{type=ThisT}=This,R,G,B)
 when is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,R,G,B,?get_env(),?wxImage_ConvertToMono),
  wxe_util:rec(?wxImage_ConvertToMono).

-doc "Returns an identical copy of this image.".
-spec copy(This) -> wxImage() when
	This::wxImage().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_Copy),
  wxe_util:rec(?wxImage_Copy).

-doc(#{equiv => create(This,Sz, [])}).
-spec create(This, Sz) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()}.

create(This,{SzW,SzH} = Sz)
 when is_record(This, wx_ref),is_integer(SzW),is_integer(SzH) ->
  create(This,Sz, []).

-doc """
This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.
""".
-spec create(This, Width, Height) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer();
      (This, Sz, Data) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary();
      (This, Sz, [Option]) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()},
	Option :: {'clear', boolean()}.

create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []);
create(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz,Data)
 when is_integer(SzW),is_integer(SzH),is_binary(Data) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Sz,Data,?get_env(),?wxImage_Create_2_0),
  wxe_util:rec(?wxImage_Create_2_0);
create(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz, Options)
 when is_integer(SzW),is_integer(SzH),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({clear, _clear} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sz, Opts,?get_env(),?wxImage_Create_2_1),
  wxe_util:rec(?wxImage_Create_2_1).

-doc """
This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.
""".
-spec create(This, Width, Height, Data) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer(), Data::binary();
      (This, Width, Height, [Option]) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer(),
	Option :: {'clear', boolean()};
      (This, Sz, Data, Alpha) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary().
create(#wx_ref{type=ThisT}=This,Width,Height,Data)
 when is_integer(Width),is_integer(Height),is_binary(Data) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Width,Height,Data,?get_env(),?wxImage_Create_3_0),
  wxe_util:rec(?wxImage_Create_3_0);
create(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({clear, _clear} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxImage_Create_3_1),
  wxe_util:rec(?wxImage_Create_3_1);
create(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz,Data,Alpha)
 when is_integer(SzW),is_integer(SzH),is_binary(Data),is_binary(Alpha) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Sz,Data,Alpha,?get_env(),?wxImage_Create_3_2),
  wxe_util:rec(?wxImage_Create_3_2).

-doc """
Creates a fresh image.

See `new/4` for more info.

Return: true if the call succeeded, false otherwise.
""".
-spec create(This, Width, Height, Data, Alpha) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer(), Data::binary(), Alpha::binary().
create(#wx_ref{type=ThisT}=This,Width,Height,Data,Alpha)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Width,Height,Data,Alpha,?get_env(),?wxImage_Create_4),
  wxe_util:rec(?wxImage_Create_4).

-doc "Destroys the image data.".
-spec 'Destroy'(This) -> 'ok' when
	This::wxImage().
'Destroy'(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_Destroy).

-doc(#{equiv => findFirstUnusedColour(This, [])}).
-spec findFirstUnusedColour(This) -> Result when
	Result ::{Res ::boolean(), R::integer(), G::integer(), B::integer()},
	This::wxImage().

findFirstUnusedColour(This)
 when is_record(This, wx_ref) ->
  findFirstUnusedColour(This, []).

-doc """
Finds the first colour that is never used in the image.

The search begins at given initial colour and continues by increasing R, G and B
components (in this order) by 1 until an unused colour is found or the colour space exhausted.

The parameters `r`, `g`, `b` are pointers to variables to save the colour.

The parameters `startR`, `startG`, `startB` define the initial values of the colour. The
returned colour will have RGB values equal to or greater than these.

Return: Returns false if there is no unused colour left, true on success.

Note: This method involves computing the histogram, which is a computationally intensive
operation.
""".
-spec findFirstUnusedColour(This, [Option]) -> Result when
	Result :: {Res ::boolean(), R::integer(), G::integer(), B::integer()},
	This::wxImage(),
	Option :: {'startR', integer()}
		 | {'startG', integer()}
		 | {'startB', integer()}.
findFirstUnusedColour(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({startR, _startR} = Arg) -> Arg;
          ({startG, _startG} = Arg) -> Arg;
          ({startB, _startB} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_FindFirstUnusedColour),
  wxe_util:rec(?wxImage_FindFirstUnusedColour).

-doc """
Iterates all registered `wxImageHandler` (not implemented in wx) objects, and returns a
string containing file extension masks suitable for passing to file open/save dialog
boxes.

Return: The format of the returned string is `"(*.ext1;*.ext2)|*.ext1;*.ext2"`. It is
usually a good idea to prepend a description before passing the result to the dialog.
Example:
""".
-spec getImageExtWildcard() -> unicode:charlist().
getImageExtWildcard() ->
  wxe_util:queue_cmd(?get_env(), ?wxImage_GetImageExtWildcard),
  wxe_util:rec(?wxImage_GetImageExtWildcard).

-doc """
Returns pointer to the array storing the alpha values for this image.

This pointer is NULL for the images without the alpha channel. If the image does have it,
this pointer may be used to directly manipulate the alpha values which are stored as the
RGB ones.
""".
-spec getAlpha(This) -> binary() when
	This::wxImage().
getAlpha(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetAlpha_0),
  wxe_util:rec(?wxImage_GetAlpha_0).

-doc "Return alpha value at given pixel location.".
-spec getAlpha(This, X, Y) -> integer() when
	This::wxImage(), X::integer(), Y::integer().
getAlpha(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxImage_GetAlpha_2),
  wxe_util:rec(?wxImage_GetAlpha_2).

-doc "Returns the blue intensity at the given coordinate.".
-spec getBlue(This, X, Y) -> integer() when
	This::wxImage(), X::integer(), Y::integer().
getBlue(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxImage_GetBlue),
  wxe_util:rec(?wxImage_GetBlue).

-doc """
Returns the image data as an array.

This is most often used when doing direct image manipulation. The return value points to
an array of characters in RGBRGBRGB... format in the top-to-bottom, left-to-right order,
that is the first RGB triplet corresponds to the first pixel of the first row, the second
one - to the second pixel of the first row and so on until the end of the first row, with
second row following after it and so on.

You should not delete the returned pointer nor pass it to `setData/4`.
""".
-spec getData(This) -> binary() when
	This::wxImage().
getData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetData),
  wxe_util:rec(?wxImage_GetData).

-doc "Returns the green intensity at the given coordinate.".
-spec getGreen(This, X, Y) -> integer() when
	This::wxImage(), X::integer(), Y::integer().
getGreen(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxImage_GetGreen),
  wxe_util:rec(?wxImage_GetGreen).

-doc(#{equiv => getImageCount(Filename, [])}).
-spec getImageCount(Filename) -> integer() when
	Filename::unicode:chardata().

getImageCount(Filename)
 when ?is_chardata(Filename) ->
  getImageCount(Filename, []).

-doc """
If the image file contains more than one image and the image handler is capable of
retrieving these individually, this function will return the number of available images.

For the overload taking the parameter `filename`, that's the name of the file to query.
For the overload taking the parameter `stream`, that's the opened input stream with image data.

See `wxImageHandler::GetImageCount()` (not implemented in wx) for more info.

The parameter `type` may be one of the following values:

* wxBITMAP_TYPE_BMP: Load a Windows bitmap file.

* wxBITMAP_TYPE_GIF: Load a GIF bitmap file.

* wxBITMAP_TYPE_JPEG: Load a JPEG bitmap file.

* wxBITMAP_TYPE_PNG: Load a PNG bitmap file.

* wxBITMAP_TYPE_PCX: Load a PCX bitmap file.

* wxBITMAP_TYPE_PNM: Load a PNM bitmap file.

* wxBITMAP_TYPE_TIFF: Load a TIFF bitmap file.

* wxBITMAP_TYPE_TGA: Load a TGA bitmap file.

* wxBITMAP_TYPE_XPM: Load a XPM bitmap file.

* wxBITMAP_TYPE_ICO: Load a Windows icon file (ICO).

* wxBITMAP_TYPE_CUR: Load a Windows cursor file (CUR).

* wxBITMAP_TYPE_ANI: Load a Windows animated cursor file (ANI).

* wxBITMAP_TYPE_ANY: Will try to autodetect the format.

Return: Number of available images. For most image handlers, this is 1 (exceptions are
TIFF and ICO formats as well as animated GIFs for which this function returns the number
of frames in the animation).
""".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec getImageCount(Filename, [Option]) -> integer() when
	Filename::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}.
getImageCount(Filename, Options)
 when ?is_chardata(Filename),is_list(Options) ->
  Filename_UC = unicode:characters_to_binary(Filename),
  MOpts = fun({type, _type} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Filename_UC, Opts,?get_env(),?wxImage_GetImageCount),
  wxe_util:rec(?wxImage_GetImageCount).

-doc """
Gets the height of the image in pixels.

See: `getWidth/1`
""".
-spec getHeight(This) -> integer() when
	This::wxImage().
getHeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetHeight),
  wxe_util:rec(?wxImage_GetHeight).

-doc "Gets the blue value of the mask colour.".
-spec getMaskBlue(This) -> integer() when
	This::wxImage().
getMaskBlue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetMaskBlue),
  wxe_util:rec(?wxImage_GetMaskBlue).

-doc "Gets the green value of the mask colour.".
-spec getMaskGreen(This) -> integer() when
	This::wxImage().
getMaskGreen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetMaskGreen),
  wxe_util:rec(?wxImage_GetMaskGreen).

-doc "Gets the red value of the mask colour.".
-spec getMaskRed(This) -> integer() when
	This::wxImage().
getMaskRed(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetMaskRed),
  wxe_util:rec(?wxImage_GetMaskRed).

-doc """
Get the current mask colour or find a suitable unused colour that could be used as a mask
colour.

Returns true if the image currently has a mask.
""".
-spec getOrFindMaskColour(This) -> Result when
	Result ::{Res ::boolean(), R::integer(), G::integer(), B::integer()},
	This::wxImage().
getOrFindMaskColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetOrFindMaskColour),
  wxe_util:rec(?wxImage_GetOrFindMaskColour).

-doc """
Returns the palette associated with the image.

Currently the palette is only used when converting to `m:wxBitmap` under Windows.

Some of the `m:wxImage` handlers have been modified to set the palette if one exists in
the image file (usually 256 or less colour images in GIF or PNG format).
""".
-spec getPalette(This) -> wxPalette:wxPalette() when
	This::wxImage().
getPalette(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetPalette),
  wxe_util:rec(?wxImage_GetPalette).

-doc "Returns the red intensity at the given coordinate.".
-spec getRed(This, X, Y) -> integer() when
	This::wxImage(), X::integer(), Y::integer().
getRed(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxImage_GetRed),
  wxe_util:rec(?wxImage_GetRed).

-doc "Returns a sub image of the current one as long as the rect belongs entirely to the image.".
-spec getSubImage(This, Rect) -> wxImage() when
	This::wxImage(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
getSubImage(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxImage_GetSubImage),
  wxe_util:rec(?wxImage_GetSubImage).

-doc """
Gets the width of the image in pixels.

See: `getHeight/1`
""".
-spec getWidth(This) -> integer() when
	This::wxImage().
getWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetWidth),
  wxe_util:rec(?wxImage_GetWidth).

-doc """
Returns true if this image has alpha channel, false otherwise.

See:
* `getAlpha/3`

* `setAlpha/4`
""".
-spec hasAlpha(This) -> boolean() when
	This::wxImage().
hasAlpha(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_HasAlpha),
  wxe_util:rec(?wxImage_HasAlpha).

-doc "Returns true if there is a mask active, false otherwise.".
-spec hasMask(This) -> boolean() when
	This::wxImage().
hasMask(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_HasMask),
  wxe_util:rec(?wxImage_HasMask).

-doc """
Gets a user-defined string-valued option.

Generic options:

* `wxIMAGE_OPTION_FILENAME:` The name of the file from which the image was loaded.

Options specific to `wxGIFHandler` (not implemented in wx):

* `wxIMAGE_OPTION_GIF_COMMENT:` The comment text that is read from or written to the GIF
file. In an animated GIF each frame can have its own comment. If there is only a comment
in the first frame of a GIF it will not be repeated in other frames.

Return: The value of the option or an empty string if not found. Use `hasOption/2` if an empty string
can be a valid option value.

See:
* `setOption/3`

* `getOptionInt/2`

* `hasOption/2`
""".
-spec getOption(This, Name) -> unicode:charlist() when
	This::wxImage(), Name::unicode:chardata().
getOption(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxImage_GetOption),
  wxe_util:rec(?wxImage_GetOption).

-doc """
Gets a user-defined integer-valued option.

The function is case-insensitive to `name`. If the given option is not present, the
function returns 0. Use `hasOption/2` if 0 is a possibly valid value for the option.

Generic options:

* `wxIMAGE_OPTION_MAX_WIDTH` and `wxIMAGE_OPTION_MAX_HEIGHT:` If either of these options is
specified, the loaded image will be scaled down (preserving its aspect ratio) so that its
width is less than the max width given if it is not 0 `and` its height is less than the
max height given if it is not 0. This is typically used for loading thumbnails and the
advantage of using these options compared to calling `rescale/4` after loading is that some handlers
(only JPEG one right now) support rescaling the image during loading which is vastly more
efficient than loading the entire huge image and rescaling it later (if these options are
not supported by the handler, this is still what happens however). These options must be
set before calling `loadFile/4` to have any effect.

* `wxIMAGE_OPTION_ORIGINAL_WIDTH` and `wxIMAGE_OPTION_ORIGINAL_HEIGHT:` These options will
return the original size of the image if either `wxIMAGE_OPTION_MAX_WIDTH` or `wxIMAGE_OPTION_MAX_HEIGHT`
is specified.

Since: 2.9.3

* `wxIMAGE_OPTION_QUALITY:` JPEG quality used when saving. This is an integer in 0..100
range with 0 meaning very poor and 100 excellent (but very badly compressed). This option
is currently ignored for the other formats.

* `wxIMAGE_OPTION_RESOLUTIONUNIT:` The value of this option determines whether the
resolution of the image is specified in centimetres or inches, see wxImageResolution enum elements.

* `wxIMAGE_OPTION_RESOLUTION`, `wxIMAGE_OPTION_RESOLUTIONX` and `wxIMAGE_OPTION_RESOLUTIONY:`
These options define the resolution of the image in the units corresponding to `wxIMAGE_OPTION_RESOLUTIONUNIT`
options value. The first option can be set before saving the image to set both horizontal
and vertical resolution to the same value. The X and Y options are set by the image
handlers if they support the image resolution (currently BMP, JPEG and TIFF handlers do)
and the image provides the resolution information and can be queried after loading the image.

Options specific to `wxPNGHandler` (not implemented in wx):

* `wxIMAGE_OPTION_PNG_FORMAT:` Format for saving a PNG file, see wxImagePNGType for the
supported values.

* `wxIMAGE_OPTION_PNG_BITDEPTH:` Bit depth for every channel (R/G/B/A).

* `wxIMAGE_OPTION_PNG_FILTER:` Filter for saving a PNG file, see libpng ([http://www.libpng.org/pub/png/libpng-1.2.5-manual.html](http://www.libpng.org/pub/png/libpng-1.2.5-manual.html))
for possible values (e.g. PNG_FILTER_NONE, PNG_FILTER_SUB, PNG_FILTER_UP, etc).

* `wxIMAGE_OPTION_PNG_COMPRESSION_LEVEL:` Compression level (0..9) for saving a PNG file.
An high value creates smaller-but-slower PNG file. Note that unlike other formats (e.g.
JPEG) the PNG format is always lossless and thus this compression level doesn't tradeoff
the image quality.

* `wxIMAGE_OPTION_PNG_COMPRESSION_MEM_LEVEL:` Compression memory usage level (1..9) for
saving a PNG file. An high value means the saving process consumes more memory, but may
create smaller PNG file.

* `wxIMAGE_OPTION_PNG_COMPRESSION_STRATEGY:` Possible values are 0 for default strategy, 1
for filter, and 2 for Huffman-only. You can use OptiPNG ([http://optipng.sourceforge.net/](http://optipng.sourceforge.net/))
to get a suitable value for your application.

* `wxIMAGE_OPTION_PNG_COMPRESSION_BUFFER_SIZE:` Internal buffer size (in bytes) for saving
a PNG file. Ideally this should be as big as the resulting PNG file. Use this option if
your application produces images with small size variation.

Options specific to `wxTIFFHandler` (not implemented in wx):

* `wxIMAGE_OPTION_TIFF_BITSPERSAMPLE:` Number of bits per sample (channel). Currently
values of 1 and 8 are supported. A value of 1 results in a black and white image. A value
of 8 (the default) can mean greyscale or RGB, depending on the value of `wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL`.

* `wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL:` Number of samples (channels) per pixel. Currently
values of 1 and 3 are supported. A value of 1 results in either a greyscale (by default)
or black and white image, depending on the value of `wxIMAGE_OPTION_TIFF_BITSPERSAMPLE`. A
value of 3 (the default) will result in an RGB image.

* `wxIMAGE_OPTION_TIFF_COMPRESSION:` Compression type. By default it is set to 1
(COMPRESSION_NONE). Typical other values are 5 (COMPRESSION_LZW) and 7 (COMPRESSION_JPEG).
See tiff.h for more options.

* `wxIMAGE_OPTION_TIFF_PHOTOMETRIC:` Specifies the photometric interpretation. By default
it is set to 2 (PHOTOMETRIC_RGB) for RGB images and 0 (PHOTOMETRIC_MINISWHITE) for
greyscale or black and white images. It can also be set to 1 (PHOTOMETRIC_MINISBLACK) to
treat the lowest value as black and highest as white. If you want a greyscale image it is
also sufficient to only specify `wxIMAGE_OPTION_TIFF_PHOTOMETRIC` and set it to either
PHOTOMETRIC_MINISWHITE or PHOTOMETRIC_MINISBLACK. The other values are taken care of.

Options specific to `wxGIFHandler` (not implemented in wx):

* `wxIMAGE_OPTION_GIF_TRANSPARENCY:` How to deal with transparent pixels. By default, the
color of transparent pixels is changed to bright pink, so that if the image is
accidentally drawn without transparency, it will be obvious. Normally, this would not be
noticed, as these pixels will not be rendered. But in some cases it might be useful to
load a GIF without making any modifications to its colours. Use `wxIMAGE_OPTION_GIF_TRANSPARENCY_UNCHANGED`
to keep the colors correct. Use `wxIMAGE_OPTION_GIF_TRANSPARENCY_HIGHLIGHT` to convert
transparent pixels to pink (default). This option has been added in wxWidgets 3.1.1.

Note: Be careful when combining the options `wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL`, `wxIMAGE_OPTION_TIFF_BITSPERSAMPLE`,
and `wxIMAGE_OPTION_TIFF_PHOTOMETRIC`. While some measures are taken to prevent illegal
combinations and/or values, it is still easy to abuse them and come up with invalid
results in the form of either corrupted images or crashes.

Return: The value of the option or 0 if not found. Use `hasOption/2` if 0 can be a valid option value.

See:
* `setOption/3`

* `getOption/2`
""".
-spec getOptionInt(This, Name) -> integer() when
	This::wxImage(), Name::unicode:chardata().
getOptionInt(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxImage_GetOptionInt),
  wxe_util:rec(?wxImage_GetOptionInt).

-doc """
Returns true if the given option is present.

The function is case-insensitive to `name`.

The lists of the currently supported options are in `getOption/2` and `getOptionInt/2` function docs.

See:
* `setOption/3`

* `getOption/2`

* `getOptionInt/2`
""".
-spec hasOption(This, Name) -> boolean() when
	This::wxImage(), Name::unicode:chardata().
hasOption(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxImage_HasOption),
  wxe_util:rec(?wxImage_HasOption).

-doc """
Initializes the image alpha channel data.

It is an error to call it if the image already has alpha data. If it doesn't, alpha data
will be by default initialized to all pixels being fully opaque. But if the image has a
mask colour, all mask pixels will be completely transparent.
""".
-spec initAlpha(This) -> 'ok' when
	This::wxImage().
initAlpha(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_InitAlpha).

-doc """
Internal use only.

Adds standard image format handlers. It only install wxBMPHandler for the time being,
which is used by `m:wxBitmap`.

This function is called by wxWidgets on startup, and shouldn't be called by the user.

See: ?wxInitAllImageHandlers()
""".
-spec initStandardHandlers() -> 'ok'.
initStandardHandlers() ->
  wxe_util:queue_cmd(?get_env(), ?wxImage_InitStandardHandlers).

-doc(#{equiv => isTransparent(This,X,Y, [])}).
-spec isTransparent(This, X, Y) -> boolean() when
	This::wxImage(), X::integer(), Y::integer().

isTransparent(This,X,Y)
 when is_record(This, wx_ref),is_integer(X),is_integer(Y) ->
  isTransparent(This,X,Y, []).

-doc """
Returns true if the given pixel is transparent, i.e. either has the mask colour if this
image has a mask or if this image has alpha channel and alpha value of this pixel is
strictly less than `threshold`.
""".
-spec isTransparent(This, X, Y, [Option]) -> boolean() when
	This::wxImage(), X::integer(), Y::integer(),
	Option :: {'threshold', integer()}.
isTransparent(#wx_ref{type=ThisT}=This,X,Y, Options)
 when is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({threshold, _threshold} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,X,Y, Opts,?get_env(),?wxImage_IsTransparent),
  wxe_util:rec(?wxImage_IsTransparent).

-doc(#{equiv => loadFile(This,Name, [])}).
-spec loadFile(This, Name) -> boolean() when
	This::wxImage(), Name::unicode:chardata().

loadFile(This,Name)
 when is_record(This, wx_ref),?is_chardata(Name) ->
  loadFile(This,Name, []).

-doc """
Loads an image from a file.

If no handler type is provided, the library will try to autodetect the format.
""".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec loadFile(This, Name, [Option]) -> boolean() when
	This::wxImage(), Name::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}
		 | {'index', integer()}.
loadFile(#wx_ref{type=ThisT}=This,Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({type, _type} = Arg) -> Arg;
          ({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Name_UC, Opts,?get_env(),?wxImage_LoadFile_2),
  wxe_util:rec(?wxImage_LoadFile_2).

-doc """
Loads an image from a file.

If no handler type is provided, the library will try to autodetect the format.
""".
-spec loadFile(This, Name, Mimetype, [Option]) -> boolean() when
	This::wxImage(), Name::unicode:chardata(), Mimetype::unicode:chardata(),
	Option :: {'index', integer()}.
loadFile(#wx_ref{type=ThisT}=This,Name,Mimetype, Options)
 when ?is_chardata(Name),?is_chardata(Mimetype),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  Mimetype_UC = unicode:characters_to_binary(Mimetype),
  MOpts = fun({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Name_UC,Mimetype_UC, Opts,?get_env(),?wxImage_LoadFile_3),
  wxe_util:rec(?wxImage_LoadFile_3).

-doc "Equivalent to: `isOk/1`".
-spec ok(This) -> boolean() when
	This::wxImage().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

-doc "Returns true if image data is present.".
-spec isOk(This) -> boolean() when
	This::wxImage().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_IsOk),
  wxe_util:rec(?wxImage_IsOk).

-doc """
Finds the handler with the given name, and removes it.

The handler is also deleted.

Return: true if the handler was found and removed, false otherwise.
""".
-spec removeHandler(Name) -> boolean() when
	Name::unicode:chardata().
removeHandler(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxImage_RemoveHandler),
  wxe_util:rec(?wxImage_RemoveHandler).

-doc(#{equiv => mirror(This, [])}).
-spec mirror(This) -> wxImage() when
	This::wxImage().

mirror(This)
 when is_record(This, wx_ref) ->
  mirror(This, []).

-doc """
Returns a mirrored copy of the image.

The parameter `horizontally` indicates the orientation.
""".
-spec mirror(This, [Option]) -> wxImage() when
	This::wxImage(),
	Option :: {'horizontally', boolean()}.
mirror(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({horizontally, _horizontally} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_Mirror),
  wxe_util:rec(?wxImage_Mirror).

-doc "Replaces the colour specified by `r1`,g1,b1 by the colour `r2`,g2,b2.".
-spec replace(This, R1, G1, B1, R2, G2, B2) -> 'ok' when
	This::wxImage(), R1::integer(), G1::integer(), B1::integer(), R2::integer(), G2::integer(), B2::integer().
replace(#wx_ref{type=ThisT}=This,R1,G1,B1,R2,G2,B2)
 when is_integer(R1),is_integer(G1),is_integer(B1),is_integer(R2),is_integer(G2),is_integer(B2) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,R1,G1,B1,R2,G2,B2,?get_env(),?wxImage_Replace).

-doc(#{equiv => rescale(This,Width,Height, [])}).
-spec rescale(This, Width, Height) -> wxImage() when
	This::wxImage(), Width::integer(), Height::integer().

rescale(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  rescale(This,Width,Height, []).

-doc """
Changes the size of the image in-place by scaling it: after a call to this function,the
image will have the given width and height.

For a description of the `quality` parameter, see the `scale/4` function. Returns the (modified)
image itself.

See: `scale/4`
""".
%%  Quality = ?wxIMAGE_QUALITY_NEAREST | ?wxIMAGE_QUALITY_BILINEAR | ?wxIMAGE_QUALITY_BICUBIC | ?wxIMAGE_QUALITY_BOX_AVERAGE | ?wxIMAGE_QUALITY_NORMAL | ?wxIMAGE_QUALITY_HIGH
-spec rescale(This, Width, Height, [Option]) -> wxImage() when
	This::wxImage(), Width::integer(), Height::integer(),
	Option :: {'quality', wx:wx_enum()}.
rescale(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({quality, _quality} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxImage_Rescale),
  wxe_util:rec(?wxImage_Rescale).

-doc(#{equiv => resize(This,Size,Pos, [])}).
-spec resize(This, Size, Pos) -> wxImage() when
	This::wxImage(), Size::{W::integer(), H::integer()}, Pos::{X::integer(), Y::integer()}.

resize(This,{SizeW,SizeH} = Size,{PosX,PosY} = Pos)
 when is_record(This, wx_ref),is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY) ->
  resize(This,Size,Pos, []).

-doc """
Changes the size of the image in-place without scaling it by adding either a border with
the given colour or cropping as necessary.

The image is pasted into a new image with the given `size` and background colour at the
position `pos` relative to the upper left of the new image.

If `red` = green = blue = -1 then use either the current mask colour if set or find, use,
and set a suitable mask colour for any newly exposed areas.

Return: The (modified) image itself.

See: `size/4`
""".
-spec resize(This, Size, Pos, [Option]) -> wxImage() when
	This::wxImage(), Size::{W::integer(), H::integer()}, Pos::{X::integer(), Y::integer()},
	Option :: {'r', integer()}
		 | {'g', integer()}
		 | {'b', integer()}.
resize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size,{PosX,PosY} = Pos, Options)
 when is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({r, _r} = Arg) -> Arg;
          ({g, _g} = Arg) -> Arg;
          ({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Size,Pos, Opts,?get_env(),?wxImage_Resize),
  wxe_util:rec(?wxImage_Resize).

-doc(#{equiv => rotate(This,Angle,RotationCentre, [])}).
-spec rotate(This, Angle, RotationCentre) -> wxImage() when
	This::wxImage(), Angle::number(), RotationCentre::{X::integer(), Y::integer()}.

rotate(This,Angle,{RotationCentreX,RotationCentreY} = RotationCentre)
 when is_record(This, wx_ref),is_number(Angle),is_integer(RotationCentreX),is_integer(RotationCentreY) ->
  rotate(This,Angle,RotationCentre, []).

-doc """
Rotates the image about the given point, by `angle` radians.

Passing true to `interpolating` results in better image quality, but is slower.

If the image has a mask, then the mask colour is used for the uncovered pixels in the
rotated image background. Else, black (rgb 0, 0, 0) will be used.

Returns the rotated image, leaving this image intact.
""".
-spec rotate(This, Angle, RotationCentre, [Option]) -> wxImage() when
	This::wxImage(), Angle::number(), RotationCentre::{X::integer(), Y::integer()},
	Option :: {'interpolating', boolean()}
		 | {'offset_after_rotation', {X::integer(), Y::integer()}}.
rotate(#wx_ref{type=ThisT}=This,Angle,{RotationCentreX,RotationCentreY} = RotationCentre, Options)
 when is_number(Angle),is_integer(RotationCentreX),is_integer(RotationCentreY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({interpolating, _interpolating} = Arg) -> Arg;
          ({offset_after_rotation, {_offset_after_rotationX,_offset_after_rotationY}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Angle,RotationCentre, Opts,?get_env(),?wxImage_Rotate),
  wxe_util:rec(?wxImage_Rotate).

-doc """
Rotates the hue of each pixel in the image by `angle`, which is a double in the range of
-1.0 to +1.0, where -1.0 corresponds to -360 degrees and +1.0 corresponds to +360 degrees.
""".
-spec rotateHue(This, Angle) -> 'ok' when
	This::wxImage(), Angle::number().
rotateHue(#wx_ref{type=ThisT}=This,Angle)
 when is_number(Angle) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Angle,?get_env(),?wxImage_RotateHue).

-doc(#{equiv => rotate90(This, [])}).
-spec rotate90(This) -> wxImage() when
	This::wxImage().

rotate90(This)
 when is_record(This, wx_ref) ->
  rotate90(This, []).

-doc "Returns a copy of the image rotated 90 degrees in the direction indicated by `clockwise`.".
-spec rotate90(This, [Option]) -> wxImage() when
	This::wxImage(),
	Option :: {'clockwise', boolean()}.
rotate90(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({clockwise, _clockwise} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_Rotate90),
  wxe_util:rec(?wxImage_Rotate90).

-doc """
Saves an image in the named file.

File type is determined from the extension of the file name. Note that this function may
fail if the extension is not recognized! You can use one of the forms above to save images
to files with non-standard extensions.
""".
-spec saveFile(This, Name) -> boolean() when
	This::wxImage(), Name::unicode:chardata().
saveFile(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxImage_SaveFile_1),
  wxe_util:rec(?wxImage_SaveFile_1).

-doc "Saves an image in the named file.".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec saveFile(This, Name, Type) -> boolean() when
	This::wxImage(), Name::unicode:chardata(), Type::wx:wx_enum();
      (This, Name, Mimetype) -> boolean() when
	This::wxImage(), Name::unicode:chardata(), Mimetype::unicode:chardata().
saveFile(#wx_ref{type=ThisT}=This,Name,Type)
 when ?is_chardata(Name),is_integer(Type) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,Type,?get_env(),?wxImage_SaveFile_2_0),
  wxe_util:rec(?wxImage_SaveFile_2_0);
saveFile(#wx_ref{type=ThisT}=This,Name,Mimetype)
 when ?is_chardata(Name),?is_chardata(Mimetype) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  Mimetype_UC = unicode:characters_to_binary(Mimetype),
  wxe_util:queue_cmd(This,Name_UC,Mimetype_UC,?get_env(),?wxImage_SaveFile_2_1),
  wxe_util:rec(?wxImage_SaveFile_2_1).

-doc(#{equiv => scale(This,Width,Height, [])}).
-spec scale(This, Width, Height) -> wxImage() when
	This::wxImage(), Width::integer(), Height::integer().

scale(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  scale(This,Width,Height, []).

-doc """
Returns a scaled version of the image.

This is also useful for scaling bitmaps in general as the only other way to scale bitmaps
is to blit a `m:wxMemoryDC` into another `m:wxMemoryDC`.

The parameter `quality` determines what method to use for resampling the image, see
wxImageResizeQuality documentation.

It should be noted that although using `wxIMAGE_QUALITY_HIGH` produces much nicer looking
results it is a slower method. Downsampling will use the box averaging method which seems
to operate very fast. If you are upsampling larger images using this method you will most
likely notice that it is a bit slower and in extreme cases it will be quite substantially
slower as the bicubic algorithm has to process a lot of data.

It should also be noted that the high quality scaling may not work as expected when using
a single mask colour for transparency, as the scaling will blur the image and will
therefore remove the mask partially. Using the alpha channel will work.

Example:

See: `rescale/4`
""".
%%  Quality = ?wxIMAGE_QUALITY_NEAREST | ?wxIMAGE_QUALITY_BILINEAR | ?wxIMAGE_QUALITY_BICUBIC | ?wxIMAGE_QUALITY_BOX_AVERAGE | ?wxIMAGE_QUALITY_NORMAL | ?wxIMAGE_QUALITY_HIGH
-spec scale(This, Width, Height, [Option]) -> wxImage() when
	This::wxImage(), Width::integer(), Height::integer(),
	Option :: {'quality', wx:wx_enum()}.
scale(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({quality, _quality} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxImage_Scale),
  wxe_util:rec(?wxImage_Scale).

-doc(#{equiv => size(This,Size,Pos, [])}).
-spec size(This, Size, Pos) -> wxImage() when
	This::wxImage(), Size::{W::integer(), H::integer()}, Pos::{X::integer(), Y::integer()}.

size(This,{SizeW,SizeH} = Size,{PosX,PosY} = Pos)
 when is_record(This, wx_ref),is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY) ->
  size(This,Size,Pos, []).

-doc """
Returns a resized version of this image without scaling it by adding either a border with
the given colour or cropping as necessary.

The image is pasted into a new image with the given `size` and background colour at the
position `pos` relative to the upper left of the new image.

If `red` = green = blue = -1 then the areas of the larger image not covered by this image
are made transparent by filling them with the image mask colour (which will be allocated
automatically if it isn't currently set).

Otherwise, the areas will be filled with the colour with the specified RGB components.

See: `resize/4`
""".
-spec size(This, Size, Pos, [Option]) -> wxImage() when
	This::wxImage(), Size::{W::integer(), H::integer()}, Pos::{X::integer(), Y::integer()},
	Option :: {'r', integer()}
		 | {'g', integer()}
		 | {'b', integer()}.
size(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size,{PosX,PosY} = Pos, Options)
 when is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({r, _r} = Arg) -> Arg;
          ({g, _g} = Arg) -> Arg;
          ({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Size,Pos, Opts,?get_env(),?wxImage_Size),
  wxe_util:rec(?wxImage_Size).

-doc """
This function is similar to `setData/4` and has similar restrictions.

The pointer passed to it may however be NULL in which case the function will allocate the
alpha array internally - this is useful to add alpha channel data to an image which
doesn't have any.

If the pointer is not NULL, it must have one byte for each image pixel and be allocated
with malloc(). `m:wxImage` takes ownership of the pointer and will free it unless `static_data`
parameter is set to true - in this case the caller should do it.
""".
-spec setAlpha(This, Alpha) -> 'ok' when
	This::wxImage(), Alpha::binary().
setAlpha(#wx_ref{type=ThisT}=This,Alpha)
 when is_binary(Alpha) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Alpha,?get_env(),?wxImage_SetAlpha_1).

-doc """
Sets the alpha value for the given pixel.

This function should only be called if the image has alpha channel data, use `hasAlpha/1` to check
for this.
""".
-spec setAlpha(This, X, Y, Alpha) -> 'ok' when
	This::wxImage(), X::integer(), Y::integer(), Alpha::integer().
setAlpha(#wx_ref{type=ThisT}=This,X,Y,Alpha)
 when is_integer(X),is_integer(Y),is_integer(Alpha) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,Alpha,?get_env(),?wxImage_SetAlpha_3).

-doc """
Sets the image data without performing checks.

The data given must have the size (width*height*3) or results will be unexpected. Don't
use this method if you aren't sure you know what you are doing.

The data must have been allocated with `malloc()`, `NOT` with `operator` new.

If `static_data` is false, after this call the pointer to the data is owned by the `m:wxImage`
object, that will be responsible for deleting it. Do not pass to this function a pointer
obtained through `getData/1`.
""".
-spec setData(This, Data) -> 'ok' when
	This::wxImage(), Data::binary().
setData(#wx_ref{type=ThisT}=This,Data)
 when is_binary(Data) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Data,?get_env(),?wxImage_SetData_1).

-doc """
This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.
""".
-spec setData(This, Data, New_width, New_height) -> 'ok' when
	This::wxImage(), Data::binary(), New_width::integer(), New_height::integer().
setData(#wx_ref{type=ThisT}=This,Data,New_width,New_height)
 when is_binary(Data),is_integer(New_width),is_integer(New_height) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Data,New_width,New_height,?get_env(),?wxImage_SetData_3).

-doc(#{equiv => setMask(This, [])}).
-spec setMask(This) -> 'ok' when
	This::wxImage().

setMask(This)
 when is_record(This, wx_ref) ->
  setMask(This, []).

-doc """
Specifies whether there is a mask or not.

The area of the mask is determined by the current mask colour.
""".
-spec setMask(This, [Option]) -> 'ok' when
	This::wxImage(),
	Option :: {'mask', boolean()}.
setMask(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({mask, _mask} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_SetMask).

-doc "Sets the mask colour for this image (and tells the image to use the mask).".
-spec setMaskColour(This, Red, Green, Blue) -> 'ok' when
	This::wxImage(), Red::integer(), Green::integer(), Blue::integer().
setMaskColour(#wx_ref{type=ThisT}=This,Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Red,Green,Blue,?get_env(),?wxImage_SetMaskColour).

-doc """
Sets image's mask so that the pixels that have RGB value of mr,mg,mb in mask will be
masked in the image.

This is done by first finding an unused colour in the image, setting this colour as the
mask colour and then using this colour to draw all pixels in the image who corresponding
pixel in mask has given RGB value.

The parameter `mask` is the mask image to extract mask shape from. It must have the same
dimensions as the image.

The parameters `mr`, `mg`, `mb` are the RGB values of the pixels in mask that will be
used to create the mask.

Return: Returns false if mask does not have same dimensions as the image or if there is
no unused colour left. Returns true if the mask was successfully applied.

Note: Note that this method involves computing the histogram, which is a computationally
intensive operation.
""".
-spec setMaskFromImage(This, Mask, Mr, Mg, Mb) -> boolean() when
	This::wxImage(), Mask::wxImage(), Mr::integer(), Mg::integer(), Mb::integer().
setMaskFromImage(#wx_ref{type=ThisT}=This,#wx_ref{type=MaskT}=Mask,Mr,Mg,Mb)
 when is_integer(Mr),is_integer(Mg),is_integer(Mb) ->
  ?CLASS(ThisT,wxImage),
  ?CLASS(MaskT,wxImage),
  wxe_util:queue_cmd(This,Mask,Mr,Mg,Mb,?get_env(),?wxImage_SetMaskFromImage),
  wxe_util:rec(?wxImage_SetMaskFromImage).

-doc """
Sets a user-defined option.

The function is case-insensitive to `name`.

For example, when saving as a JPEG file, the option `quality` is used, which is a number
between 0 and 100 (0 is terrible, 100 is very good).

The lists of the currently supported options are in `getOption/2` and `getOptionInt/2` function docs.

See:
* `getOption/2`

* `getOptionInt/2`

* `hasOption/2`
""".
-spec setOption(This, Name, Value) -> 'ok' when
	This::wxImage(), Name::unicode:chardata(), Value::integer();
      (This, Name, Value) -> 'ok' when
	This::wxImage(), Name::unicode:chardata(), Value::unicode:chardata().
setOption(#wx_ref{type=ThisT}=This,Name,Value)
 when ?is_chardata(Name),is_integer(Value) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,Value,?get_env(),?wxImage_SetOption_2_0);
setOption(#wx_ref{type=ThisT}=This,Name,Value)
 when ?is_chardata(Name),?is_chardata(Value) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Name_UC,Value_UC,?get_env(),?wxImage_SetOption_2_1).

-doc """
Associates a palette with the image.

The palette may be used when converting `m:wxImage` to `m:wxBitmap` (MSW only at present)
or in file save operations (none as yet).
""".
-spec setPalette(This, Palette) -> 'ok' when
	This::wxImage(), Palette::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT}=This,#wx_ref{type=PaletteT}=Palette) ->
  ?CLASS(ThisT,wxImage),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:queue_cmd(This,Palette,?get_env(),?wxImage_SetPalette).

-doc """
Sets the colour of the pixels within the given rectangle.

This routine performs bounds-checks for the coordinate so it can be considered a safe way
to manipulate the data.
""".
-spec setRGB(This, Rect, Red, Green, Blue) -> 'ok' when
	This::wxImage(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, Red::integer(), Green::integer(), Blue::integer().
setRGB(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect,Red,Green,Blue)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Rect,Red,Green,Blue,?get_env(),?wxImage_SetRGB_4).

-doc "Set the color of the pixel at the given x and y coordinate.".
-spec setRGB(This, X, Y, R, G, B) -> 'ok' when
	This::wxImage(), X::integer(), Y::integer(), R::integer(), G::integer(), B::integer().
setRGB(#wx_ref{type=ThisT}=This,X,Y,R,G,B)
 when is_integer(X),is_integer(Y),is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,R,G,B,?get_env(),?wxImage_SetRGB_5).

-doc "Destroys the object".
-spec destroy(This::wxImage()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxImage),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
