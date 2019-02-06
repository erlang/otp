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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html">wxBitmap</a>.
%% @type wxBitmap().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxBitmap).
-include("wxe.hrl").
-export([convertToImage/1,copyFromIcon/2,create/3,create/4,destroy/1,getDepth/1,
  getHeight/1,getMask/1,getPalette/1,getSubBitmap/2,getWidth/1,loadFile/2,
  loadFile/3,new/0,new/1,new/2,new/3,new/4,ok/1,saveFile/3,saveFile/4,setDepth/2,
  setHeight/2,setMask/2,setPalette/2,setWidth/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxBitmap/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxBitmap() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
-spec new() -> wxBitmap().
new() ->
  wxe_util:construct(?wxBitmap_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
%% <br /> Also:<br />
%% new(Image) -> wxBitmap() when<br />
%% 	Image::wxImage:wxImage().<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Filename) -> wxBitmap() when
	Filename::unicode:chardata();
      (Image) -> wxBitmap() when
	Image::wxImage:wxImage().

new(Filename)
 when ?is_chardata(Filename) ->
  new(Filename, []);

new(Image)
 when is_record(Image, wx_ref) ->
  new(Image, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
%% <br /> Also:<br />
%% new(Filename, [Option]) -> wxBitmap() when<br />
%% 	Filename::unicode:chardata(),<br />
%% 	Option :: {'type', wx:wx_enum()};<br />
%%       (Image, [Option]) -> wxBitmap() when<br />
%% 	Image::wxImage:wxImage(),<br />
%% 	Option :: {'depth', integer()}.<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Width, Height) -> wxBitmap() when
	Width::integer(), Height::integer();
      (Filename, [Option]) -> wxBitmap() when
	Filename::unicode:chardata(),
	Option :: {'type', wx:wx_enum()};
      (Image, [Option]) -> wxBitmap() when
	Image::wxImage:wxImage(),
	Option :: {'depth', integer()}.

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []);
new(Filename, Options)
 when ?is_chardata(Filename),is_list(Options) ->
  Filename_UC = unicode:characters_to_binary([Filename,0]),
  MOpts = fun({type, Type}, Acc) -> [<<1:32/?UI,Type:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxBitmap_new_2_0,
  <<(byte_size(Filename_UC)):32/?UI,(Filename_UC)/binary, 0:(((8- ((4+byte_size(Filename_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>);
new(#wx_ref{type=ImageT,ref=ImageRef}, Options)
 when is_list(Options) ->
  ?CLASS(ImageT,wxImage),
  MOpts = fun({depth, Depth}, Acc) -> [<<1:32/?UI,Depth:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxBitmap_new_2_1,
  <<ImageRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
%% <br /> Also:<br />
%% new(Width, Height, [Option]) -> wxBitmap() when<br />
%% 	Width::integer(), Height::integer(),<br />
%% 	Option :: {'depth', integer()}.<br />
%% 
-spec new(Bits, Width, Height) -> wxBitmap() when
	Bits::binary(), Width::integer(), Height::integer();
      (Width, Height, [Option]) -> wxBitmap() when
	Width::integer(), Height::integer(),
	Option :: {'depth', integer()}.

new(Bits,Width,Height)
 when is_binary(Bits),is_integer(Width),is_integer(Height) ->
  new(Bits,Width,Height, []);
new(Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  MOpts = fun({depth, Depth}, Acc) -> [<<1:32/?UI,Depth:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxBitmap_new_3,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
-spec new(Bits, Width, Height, [Option]) -> wxBitmap() when
	Bits::binary(), Width::integer(), Height::integer(),
	Option :: {'depth', integer()}.
new(Bits,Width,Height, Options)
 when is_binary(Bits),is_integer(Width),is_integer(Height),is_list(Options) ->
  wxe_util:send_bin(Bits),
  MOpts = fun({depth, Depth}, Acc) -> [<<1:32/?UI,Depth:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxBitmap_new_4,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapconverttoimage">external documentation</a>.
-spec convertToImage(This) -> wxImage:wxImage() when
	This::wxBitmap().
convertToImage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_ConvertToImage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapcopyfromicon">external documentation</a>.
-spec copyFromIcon(This, Icon) -> boolean() when
	This::wxBitmap(), Icon::wxIcon:wxIcon().
copyFromIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef}) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(IconT,wxIcon),
  wxe_util:call(?wxBitmap_CopyFromIcon,
  <<ThisRef:32/?UI,IconRef:32/?UI>>).

%% @equiv create(This,Width,Height, [])
-spec create(This, Width, Height) -> boolean() when
	This::wxBitmap(), Width::integer(), Height::integer().

create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapcreate">external documentation</a>.
-spec create(This, Width, Height, [Option]) -> boolean() when
	This::wxBitmap(), Width::integer(), Height::integer(),
	Option :: {'depth', integer()}.
create(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  MOpts = fun({depth, Depth}, Acc) -> [<<1:32/?UI,Depth:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxBitmap_Create,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetdepth">external documentation</a>.
-spec getDepth(This) -> integer() when
	This::wxBitmap().
getDepth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetDepth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetheight">external documentation</a>.
-spec getHeight(This) -> integer() when
	This::wxBitmap().
getHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetHeight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetpalette">external documentation</a>.
-spec getPalette(This) -> wxPalette:wxPalette() when
	This::wxBitmap().
getPalette(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetPalette,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetmask">external documentation</a>.
-spec getMask(This) -> wxMask:wxMask() when
	This::wxBitmap().
getMask(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetMask,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetwidth">external documentation</a>.
-spec getWidth(This) -> integer() when
	This::wxBitmap().
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetsubbitmap">external documentation</a>.
-spec getSubBitmap(This, Rect) -> wxBitmap() when
	This::wxBitmap(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
getSubBitmap(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetSubBitmap,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @equiv loadFile(This,Name, [])
-spec loadFile(This, Name) -> boolean() when
	This::wxBitmap(), Name::unicode:chardata().

loadFile(This,Name)
 when is_record(This, wx_ref),?is_chardata(Name) ->
  loadFile(This,Name, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmaploadfile">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec loadFile(This, Name, [Option]) -> boolean() when
	This::wxBitmap(), Name::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}.
loadFile(#wx_ref{type=ThisT,ref=ThisRef},Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({type, Type}, Acc) -> [<<1:32/?UI,Type:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxBitmap_LoadFile,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxBitmap().
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_Ok,
  <<ThisRef:32/?UI>>).

%% @equiv saveFile(This,Name,Type, [])
-spec saveFile(This, Name, Type) -> boolean() when
	This::wxBitmap(), Name::unicode:chardata(), Type::wx:wx_enum().

saveFile(This,Name,Type)
 when is_record(This, wx_ref),?is_chardata(Name),is_integer(Type) ->
  saveFile(This,Name,Type, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsavefile">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec saveFile(This, Name, Type, [Option]) -> boolean() when
	This::wxBitmap(), Name::unicode:chardata(), Type::wx:wx_enum(),
	Option :: {'palette', wxPalette:wxPalette()}.
saveFile(#wx_ref{type=ThisT,ref=ThisRef},Name,Type, Options)
 when ?is_chardata(Name),is_integer(Type),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({palette, #wx_ref{type=PaletteT,ref=PaletteRef}}, Acc) ->   ?CLASS(PaletteT,wxPalette),[<<1:32/?UI,PaletteRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxBitmap_SaveFile,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,Type:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetdepth">external documentation</a>.
-spec setDepth(This, Depth) -> 'ok' when
	This::wxBitmap(), Depth::integer().
setDepth(#wx_ref{type=ThisT,ref=ThisRef},Depth)
 when is_integer(Depth) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:cast(?wxBitmap_SetDepth,
  <<ThisRef:32/?UI,Depth:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetheight">external documentation</a>.
-spec setHeight(This, Height) -> 'ok' when
	This::wxBitmap(), Height::integer().
setHeight(#wx_ref{type=ThisT,ref=ThisRef},Height)
 when is_integer(Height) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:cast(?wxBitmap_SetHeight,
  <<ThisRef:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetmask">external documentation</a>.
-spec setMask(This, Mask) -> 'ok' when
	This::wxBitmap(), Mask::wxMask:wxMask().
setMask(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MaskT,ref=MaskRef}) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(MaskT,wxMask),
  wxe_util:cast(?wxBitmap_SetMask,
  <<ThisRef:32/?UI,MaskRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetpalette">external documentation</a>.
-spec setPalette(This, Palette) -> 'ok' when
	This::wxBitmap(), Palette::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PaletteT,ref=PaletteRef}) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:cast(?wxBitmap_SetPalette,
  <<ThisRef:32/?UI,PaletteRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetwidth">external documentation</a>.
-spec setWidth(This, Width) -> 'ok' when
	This::wxBitmap(), Width::integer().
setWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:cast(?wxBitmap_SetWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxBitmap()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBitmap),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
