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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html">wxBitmap</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
new() ->
  wxe_util:construct(?wxBitmap_new_0,
  <<>>).

%% @spec (X::string()|term()) -> wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Filename::string()) -> new(Filename, []) </c></p>
%% <p><c>
%% new(Image::wxImage:wxImage()) -> new(Image, []) </c></p>

new(Filename)
 when is_list(Filename) ->
  new(Filename, []);

new(Image)
 when is_record(Image, wx_ref) ->
  new(Image, []).

%% @spec (X::integer()|string()|term(),X::integer()|term()) -> wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Width::integer(), Height::integer()) -> new(Width,Height, []) </c></p>
%% <p><c>
%% new(Filename::string(), [Option]) -> wxBitmap() </c>
%%<br /> Option = {type, WxBitmapType}
%%<br /> WxBitmapType = integer()
%%<br /> WxBitmapType is one of ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
%% </p>
%% <p><c>
%% new(Image::wxImage:wxImage(), [Option]) -> wxBitmap() </c>
%%<br /> Option = {depth, integer()}
%% </p>

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []);
new(Filename, Options)
 when is_list(Filename),is_list(Options) ->
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

%% @spec (X::binary()|integer(),X::integer(),X::integer()|term()) -> wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Bits::binary(), Width::integer(), Height::integer()) -> new(Bits,Width,Height, []) </c></p>
%% <p><c>
%% new(Width::integer(), Height::integer(), [Option]) -> wxBitmap() </c>
%%<br /> Option = {depth, integer()}
%% </p>

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

%% @spec (Bits::binary(), Width::integer(), Height::integer(), [Option]) -> wxBitmap()
%% Option = {depth, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
new(Bits,Width,Height, Options)
 when is_binary(Bits),is_integer(Width),is_integer(Height),is_list(Options) ->
  wxe_util:send_bin(Bits),
  MOpts = fun({depth, Depth}, Acc) -> [<<1:32/?UI,Depth:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxBitmap_new_4,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @spec (This::wxBitmap()) -> wxImage:wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapconverttoimage">external documentation</a>.
convertToImage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_ConvertToImage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxBitmap(), Icon::wxIcon:wxIcon()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapcopyfromicon">external documentation</a>.
copyFromIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef}) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(IconT,wxIcon),
  wxe_util:call(?wxBitmap_CopyFromIcon,
  <<ThisRef:32/?UI,IconRef:32/?UI>>).

%% @spec (This::wxBitmap(), Width::integer(), Height::integer()) -> bool()
%% @equiv create(This,Width,Height, [])
create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []).

%% @spec (This::wxBitmap(), Width::integer(), Height::integer(), [Option]) -> bool()
%% Option = {depth, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  MOpts = fun({depth, Depth}, Acc) -> [<<1:32/?UI,Depth:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxBitmap_Create,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxBitmap()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapgetdepth">external documentation</a>.
getDepth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetDepth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxBitmap()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapgetheight">external documentation</a>.
getHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetHeight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxBitmap()) -> wxPalette:wxPalette()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapgetpalette">external documentation</a>.
getPalette(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetPalette,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxBitmap()) -> wxMask:wxMask()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapgetmask">external documentation</a>.
getMask(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetMask,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxBitmap()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapgetwidth">external documentation</a>.
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxBitmap(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapgetsubbitmap">external documentation</a>.
getSubBitmap(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_GetSubBitmap,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxBitmap(), Name::string()) -> bool()
%% @equiv loadFile(This,Name, [])
loadFile(This,Name)
 when is_record(This, wx_ref),is_list(Name) ->
  loadFile(This,Name, []).

%% @spec (This::wxBitmap(), Name::string(), [Option]) -> bool()
%% Option = {type, WxBitmapType}
%% WxBitmapType = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmaploadfile">external documentation</a>.
%%<br /> WxBitmapType is one of ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
loadFile(#wx_ref{type=ThisT,ref=ThisRef},Name, Options)
 when is_list(Name),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({type, Type}, Acc) -> [<<1:32/?UI,Type:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxBitmap_LoadFile,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxBitmap()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapok">external documentation</a>.
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:call(?wxBitmap_Ok,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxBitmap(), Name::string(), Type::WxBitmapType) -> bool()
%% @equiv saveFile(This,Name,Type, [])
saveFile(This,Name,Type)
 when is_record(This, wx_ref),is_list(Name),is_integer(Type) ->
  saveFile(This,Name,Type, []).

%% @spec (This::wxBitmap(), Name::string(), Type::WxBitmapType, [Option]) -> bool()
%% Option = {palette, wxPalette:wxPalette()}
%% WxBitmapType = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapsavefile">external documentation</a>.
%%<br /> WxBitmapType is one of ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
saveFile(#wx_ref{type=ThisT,ref=ThisRef},Name,Type, Options)
 when is_list(Name),is_integer(Type),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({palette, #wx_ref{type=PaletteT,ref=PaletteRef}}, Acc) ->   ?CLASS(PaletteT,wxPalette),[<<1:32/?UI,PaletteRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxBitmap_SaveFile,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,Type:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxBitmap(), Depth::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapsetdepth">external documentation</a>.
setDepth(#wx_ref{type=ThisT,ref=ThisRef},Depth)
 when is_integer(Depth) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:cast(?wxBitmap_SetDepth,
  <<ThisRef:32/?UI,Depth:32/?UI>>).

%% @spec (This::wxBitmap(), Height::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapsetheight">external documentation</a>.
setHeight(#wx_ref{type=ThisT,ref=ThisRef},Height)
 when is_integer(Height) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:cast(?wxBitmap_SetHeight,
  <<ThisRef:32/?UI,Height:32/?UI>>).

%% @spec (This::wxBitmap(), Mask::wxMask:wxMask()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapsetmask">external documentation</a>.
setMask(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MaskT,ref=MaskRef}) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(MaskT,wxMask),
  wxe_util:cast(?wxBitmap_SetMask,
  <<ThisRef:32/?UI,MaskRef:32/?UI>>).

%% @spec (This::wxBitmap(), Palette::wxPalette:wxPalette()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapsetpalette">external documentation</a>.
setPalette(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PaletteT,ref=PaletteRef}) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:cast(?wxBitmap_SetPalette,
  <<ThisRef:32/?UI,PaletteRef:32/?UI>>).

%% @spec (This::wxBitmap(), Width::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmap.html#wxbitmapsetwidth">external documentation</a>.
setWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:cast(?wxBitmap_SetWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @spec (This::wxBitmap()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBitmap),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
