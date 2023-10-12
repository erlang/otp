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

-module(wxBitmap).
-include("wxe.hrl").
-export([convertToImage/1,copyFromIcon/2,create/2,create/3,create/4,destroy/1,
  getDepth/1,getHeight/1,getMask/1,getPalette/1,getSubBitmap/2,getWidth/1,
  isOk/1,loadFile/2,loadFile/3,new/0,new/1,new/2,new/3,new/4,ok/1,saveFile/3,
  saveFile/4,setDepth/2,setHeight/2,setMask/2,setPalette/2,setWidth/2]).

%% inherited exports
-export([parent_class/1]).

-type wxBitmap() :: wx:wx_object().
-export_type([wxBitmap/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
-spec new() -> wxBitmap().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxBitmap_new_0),
  wxe_util:rec(?wxBitmap_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
%% <br /> Also:<br />
%% new(Sz) -> wxBitmap() when<br />
%% 	Sz::{W::integer(), H::integer()};<br />
%%       (Img) -> wxBitmap() when<br />
%% 	Img::wxImage:wxImage() | wxBitmap:wxBitmap().<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Name) -> wxBitmap() when
	Name::unicode:chardata();
      (Sz) -> wxBitmap() when
	Sz::{W::integer(), H::integer()};
      (Img) -> wxBitmap() when
	Img::wxImage:wxImage() | wxBitmap:wxBitmap().

new(Name)
 when ?is_chardata(Name) ->
  new(Name, []);

new({SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  new(Sz, []);
new(#wx_ref{type=ImgT}=Img) ->
  IswxImage = ?CLASS_T(ImgT,wxImage),
  IswxBitmap = ?CLASS_T(ImgT,wxBitmap),
  ImgType = if
    IswxImage ->   wxImage;
    IswxBitmap ->   wxBitmap;
    true -> error({badarg, ImgT})
  end,
  wxe_util:queue_cmd(wx:typeCast(Img, ImgType),?get_env(),?wxBitmap_new_2_3),
  wxe_util:rec(?wxBitmap_new_2_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
%% <br /> Also:<br />
%% new(Name, [Option]) -> wxBitmap() when<br />
%% 	Name::unicode:chardata(),<br />
%% 	Option :: {'type', wx:wx_enum()};<br />
%%       (Sz, [Option]) -> wxBitmap() when<br />
%% 	Sz::{W::integer(), H::integer()},<br />
%% 	Option :: {'depth', integer()};<br />
%%       (Img, [Option]) -> wxBitmap() when<br />
%% 	Img::wxImage:wxImage(),<br />
%% 	Option :: {'depth', integer()}.<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Width, Height) -> wxBitmap() when
	Width::integer(), Height::integer();
      (Name, [Option]) -> wxBitmap() when
	Name::unicode:chardata(),
	Option :: {'type', wx:wx_enum()};
      (Sz, [Option]) -> wxBitmap() when
	Sz::{W::integer(), H::integer()},
	Option :: {'depth', integer()};
      (Img, [Option]) -> wxBitmap() when
	Img::wxImage:wxImage(),
	Option :: {'depth', integer()}.

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []);
new(Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({type, _type} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC, Opts,?get_env(),?wxBitmap_new_2_0),
  wxe_util:rec(?wxBitmap_new_2_0);
new({SzW,SzH} = Sz, Options)
 when is_integer(SzW),is_integer(SzH),is_list(Options) ->
  MOpts = fun({depth, _depth} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Sz, Opts,?get_env(),?wxBitmap_new_2_1),
  wxe_util:rec(?wxBitmap_new_2_1);
new(#wx_ref{type=ImgT}=Img, Options)
 when is_list(Options) ->
  ?CLASS(ImgT,wxImage),
  MOpts = fun({depth, _depth} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Img, Opts,?get_env(),?wxBitmap_new_2_2),
  wxe_util:rec(?wxBitmap_new_2_2).

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
  MOpts = fun({depth, _depth} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Width,Height, Opts,?get_env(),?wxBitmap_new_3),
  wxe_util:rec(?wxBitmap_new_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapwxbitmap">external documentation</a>.
-spec new(Bits, Width, Height, [Option]) -> wxBitmap() when
	Bits::binary(), Width::integer(), Height::integer(),
	Option :: {'depth', integer()}.
new(Bits,Width,Height, Options)
 when is_binary(Bits),is_integer(Width),is_integer(Height),is_list(Options) ->
  MOpts = fun({depth, _depth} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Bits,Width,Height, Opts,?get_env(),?wxBitmap_new_4),
  wxe_util:rec(?wxBitmap_new_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapconverttoimage">external documentation</a>.
-spec convertToImage(This) -> wxImage:wxImage() when
	This::wxBitmap().
convertToImage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmap_ConvertToImage),
  wxe_util:rec(?wxBitmap_ConvertToImage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapcopyfromicon">external documentation</a>.
-spec copyFromIcon(This, Icon) -> boolean() when
	This::wxBitmap(), Icon::wxIcon:wxIcon().
copyFromIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,?get_env(),?wxBitmap_CopyFromIcon),
  wxe_util:rec(?wxBitmap_CopyFromIcon).

%% @equiv create(This,Sz, [])
-spec create(This, Sz) -> boolean() when
	This::wxBitmap(), Sz::{W::integer(), H::integer()}.

create(This,{SzW,SzH} = Sz)
 when is_record(This, wx_ref),is_integer(SzW),is_integer(SzH) ->
  create(This,Sz, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapcreate">external documentation</a>.
%% <br /> Also:<br />
%% create(This, Sz, [Option]) -> boolean() when<br />
%% 	This::wxBitmap(), Sz::{W::integer(), H::integer()},<br />
%% 	Option :: {'depth', integer()}.<br />
%% 
-spec create(This, Width, Height) -> boolean() when
	This::wxBitmap(), Width::integer(), Height::integer();
      (This, Sz, [Option]) -> boolean() when
	This::wxBitmap(), Sz::{W::integer(), H::integer()},
	Option :: {'depth', integer()}.

create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []);
create(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz, Options)
 when is_integer(SzW),is_integer(SzH),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  MOpts = fun({depth, _depth} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sz, Opts,?get_env(),?wxBitmap_Create_2),
  wxe_util:rec(?wxBitmap_Create_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapcreate">external documentation</a>.
%% <br /> Also:<br />
%% create(This, Width, Height, Dc) -> boolean() when<br />
%% 	This::wxBitmap(), Width::integer(), Height::integer(), Dc::wxDC:wxDC().<br />
%% 
-spec create(This, Width, Height, [Option]) -> boolean() when
	This::wxBitmap(), Width::integer(), Height::integer(),
	Option :: {'depth', integer()};
      (This, Width, Height, Dc) -> boolean() when
	This::wxBitmap(), Width::integer(), Height::integer(), Dc::wxDC:wxDC().
create(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  MOpts = fun({depth, _depth} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxBitmap_Create_3_0),
  wxe_util:rec(?wxBitmap_Create_3_0);
create(#wx_ref{type=ThisT}=This,Width,Height,#wx_ref{type=DcT}=Dc)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(This,Width,Height,Dc,?get_env(),?wxBitmap_Create_3_1),
  wxe_util:rec(?wxBitmap_Create_3_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetdepth">external documentation</a>.
-spec getDepth(This) -> integer() when
	This::wxBitmap().
getDepth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmap_GetDepth),
  wxe_util:rec(?wxBitmap_GetDepth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetheight">external documentation</a>.
-spec getHeight(This) -> integer() when
	This::wxBitmap().
getHeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmap_GetHeight),
  wxe_util:rec(?wxBitmap_GetHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetpalette">external documentation</a>.
-spec getPalette(This) -> wxPalette:wxPalette() when
	This::wxBitmap().
getPalette(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmap_GetPalette),
  wxe_util:rec(?wxBitmap_GetPalette).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetmask">external documentation</a>.
-spec getMask(This) -> wxMask:wxMask() when
	This::wxBitmap().
getMask(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmap_GetMask),
  wxe_util:rec(?wxBitmap_GetMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetwidth">external documentation</a>.
-spec getWidth(This) -> integer() when
	This::wxBitmap().
getWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmap_GetWidth),
  wxe_util:rec(?wxBitmap_GetWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapgetsubbitmap">external documentation</a>.
-spec getSubBitmap(This, Rect) -> wxBitmap() when
	This::wxBitmap(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
getSubBitmap(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxBitmap_GetSubBitmap),
  wxe_util:rec(?wxBitmap_GetSubBitmap).

%% @equiv loadFile(This,Name, [])
-spec loadFile(This, Name) -> boolean() when
	This::wxBitmap(), Name::unicode:chardata().

loadFile(This,Name)
 when is_record(This, wx_ref),?is_chardata(Name) ->
  loadFile(This,Name, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmaploadfile">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec loadFile(This, Name, [Option]) -> boolean() when
	This::wxBitmap(), Name::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}.
loadFile(#wx_ref{type=ThisT}=This,Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({type, _type} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Name_UC, Opts,?get_env(),?wxBitmap_LoadFile),
  wxe_util:rec(?wxBitmap_LoadFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapisok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxBitmap().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxBitmap().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmap_IsOk),
  wxe_util:rec(?wxBitmap_IsOk).

%% @equiv saveFile(This,Name,Type, [])
-spec saveFile(This, Name, Type) -> boolean() when
	This::wxBitmap(), Name::unicode:chardata(), Type::wx:wx_enum().

saveFile(This,Name,Type)
 when is_record(This, wx_ref),?is_chardata(Name),is_integer(Type) ->
  saveFile(This,Name,Type, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsavefile">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec saveFile(This, Name, Type, [Option]) -> boolean() when
	This::wxBitmap(), Name::unicode:chardata(), Type::wx:wx_enum(),
	Option :: {'palette', wxPalette:wxPalette()}.
saveFile(#wx_ref{type=ThisT}=This,Name,Type, Options)
 when ?is_chardata(Name),is_integer(Type),is_list(Options) ->
  ?CLASS(ThisT,wxBitmap),
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({palette, #wx_ref{type=PaletteT}} = Arg) ->   ?CLASS(PaletteT,wxPalette),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Name_UC,Type, Opts,?get_env(),?wxBitmap_SaveFile),
  wxe_util:rec(?wxBitmap_SaveFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetdepth">external documentation</a>.
-spec setDepth(This, Depth) -> 'ok' when
	This::wxBitmap(), Depth::integer().
setDepth(#wx_ref{type=ThisT}=This,Depth)
 when is_integer(Depth) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,Depth,?get_env(),?wxBitmap_SetDepth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetheight">external documentation</a>.
-spec setHeight(This, Height) -> 'ok' when
	This::wxBitmap(), Height::integer().
setHeight(#wx_ref{type=ThisT}=This,Height)
 when is_integer(Height) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,Height,?get_env(),?wxBitmap_SetHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetmask">external documentation</a>.
-spec setMask(This, Mask) -> 'ok' when
	This::wxBitmap(), Mask::wxMask:wxMask().
setMask(#wx_ref{type=ThisT}=This,#wx_ref{type=MaskT}=Mask) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(MaskT,wxMask),
  wxe_util:queue_cmd(This,Mask,?get_env(),?wxBitmap_SetMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetpalette">external documentation</a>.
-spec setPalette(This, Palette) -> 'ok' when
	This::wxBitmap(), Palette::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT}=This,#wx_ref{type=PaletteT}=Palette) ->
  ?CLASS(ThisT,wxBitmap),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:queue_cmd(This,Palette,?get_env(),?wxBitmap_SetPalette).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmap.html#wxbitmapsetwidth">external documentation</a>.
-spec setWidth(This, Width) -> 'ok' when
	This::wxBitmap(), Width::integer().
setWidth(#wx_ref{type=ThisT}=This,Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxBitmap),
  wxe_util:queue_cmd(This,Width,?get_env(),?wxBitmap_SetWidth).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxBitmap()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBitmap),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
