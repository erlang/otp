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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxicon.html">wxIcon</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxBitmap}
%% </p>
%% @type wxIcon().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxIcon).
-include("wxe.hrl").
-export([copyFromBitmap/2,destroy/1,new/0,new/1,new/2]).

%% inherited exports
-export([convertToImage/1,copyFromIcon/2,getDepth/1,getHeight/1,getMask/1,getPalette/1,
  getSubBitmap/2,getWidth/1,loadFile/2,loadFile/3,ok/1,parent_class/1,
  saveFile/3,saveFile/4,setDepth/2,setHeight/2,setMask/2,setPalette/2,
  setWidth/2]).

%% @hidden
parent_class(wxBitmap) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxIcon()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxicon.html#wxiconwxicon">external documentation</a>.
new() ->
  wxe_util:construct(?wxIcon_new_0,
  <<>>).

%% @spec (X::string()|term()) -> wxIcon()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxicon.html#wxiconwxicon">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Filename::string()) -> new(Filename, []) </c></p>
%% <p><c>
%% new(Loc::wx:wx()) -> wxIcon() </c>
%% </p>

new(Filename)
 when is_list(Filename) ->
  new(Filename, []);
new(#wx_ref{type=LocT,ref=LocRef}) ->
  ?CLASS(LocT,wx),
  wxe_util:construct(?wxIcon_new_1,
  <<LocRef:32/?UI>>).

%% @spec (Filename::string(), [Option]) -> wxIcon()
%% Option = {type, WxBitmapType} | {desiredWidth, integer()} | {desiredHeight, integer()}
%% WxBitmapType = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxicon.html#wxiconwxicon">external documentation</a>.
%%<br /> WxBitmapType is one of ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
new(Filename, Options)
 when is_list(Filename),is_list(Options) ->
  Filename_UC = unicode:characters_to_binary([Filename,0]),
  MOpts = fun({type, Type}, Acc) -> [<<1:32/?UI,Type:32/?UI>>|Acc];
          ({desiredWidth, DesiredWidth}, Acc) -> [<<2:32/?UI,DesiredWidth:32/?UI>>|Acc];
          ({desiredHeight, DesiredHeight}, Acc) -> [<<3:32/?UI,DesiredHeight:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxIcon_new_2,
  <<(byte_size(Filename_UC)):32/?UI,(Filename_UC)/binary, 0:(((8- ((4+byte_size(Filename_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxIcon(), Bmp::wxBitmap:wxBitmap()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxicon.html#wxiconcopyfrombitmap">external documentation</a>.
copyFromBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BmpT,ref=BmpRef}) ->
  ?CLASS(ThisT,wxIcon),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:cast(?wxIcon_CopyFromBitmap,
  <<ThisRef:32/?UI,BmpRef:32/?UI>>).

%% @spec (This::wxIcon()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxIcon),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxBitmap
%% @hidden
setWidth(This,Width) -> wxBitmap:setWidth(This,Width).
%% @hidden
setPalette(This,Palette) -> wxBitmap:setPalette(This,Palette).
%% @hidden
setMask(This,Mask) -> wxBitmap:setMask(This,Mask).
%% @hidden
setHeight(This,Height) -> wxBitmap:setHeight(This,Height).
%% @hidden
setDepth(This,Depth) -> wxBitmap:setDepth(This,Depth).
%% @hidden
saveFile(This,Name,Type, Options) -> wxBitmap:saveFile(This,Name,Type, Options).
%% @hidden
saveFile(This,Name,Type) -> wxBitmap:saveFile(This,Name,Type).
%% @hidden
ok(This) -> wxBitmap:ok(This).
%% @hidden
loadFile(This,Name, Options) -> wxBitmap:loadFile(This,Name, Options).
%% @hidden
loadFile(This,Name) -> wxBitmap:loadFile(This,Name).
%% @hidden
getSubBitmap(This,Rect) -> wxBitmap:getSubBitmap(This,Rect).
%% @hidden
getWidth(This) -> wxBitmap:getWidth(This).
%% @hidden
getMask(This) -> wxBitmap:getMask(This).
%% @hidden
getPalette(This) -> wxBitmap:getPalette(This).
%% @hidden
getHeight(This) -> wxBitmap:getHeight(This).
%% @hidden
getDepth(This) -> wxBitmap:getDepth(This).
%% @hidden
copyFromIcon(This,Icon) -> wxBitmap:copyFromIcon(This,Icon).
%% @hidden
convertToImage(This) -> wxBitmap:convertToImage(This).
