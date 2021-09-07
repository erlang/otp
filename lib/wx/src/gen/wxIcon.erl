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

-module(wxIcon).
-include("wxe.hrl").
-export([copyFromBitmap/2,destroy/1,new/0,new/1,new/2]).

%% inherited exports
-export([convertToImage/1,copyFromIcon/2,getDepth/1,getHeight/1,getMask/1,getPalette/1,
  getSubBitmap/2,getWidth/1,isOk/1,loadFile/2,loadFile/3,ok/1,parent_class/1,
  saveFile/3,saveFile/4,setDepth/2,setHeight/2,setMask/2,setPalette/2,
  setWidth/2]).

-type wxIcon() :: wx:wx_object().
-export_type([wxIcon/0]).
%% @hidden
parent_class(wxBitmap) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxicon.html#wxiconwxicon">external documentation</a>.
-spec new() -> wxIcon().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxIcon_new_0),
  wxe_util:rec(?wxIcon_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxicon.html#wxiconwxicon">external documentation</a>.
%% <br /> Also:<br />
%% new(Icon) -> wxIcon() when<br />
%% 	Icon::wxIcon().<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Name) -> wxIcon() when
	Name::unicode:chardata();
      (Icon) -> wxIcon() when
	Icon::wxIcon().

new(Name)
 when ?is_chardata(Name) ->
  new(Name, []);
new(#wx_ref{type=IconT}=Icon) ->
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(Icon,?get_env(),?wxIcon_new_1),
  wxe_util:rec(?wxIcon_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxicon.html#wxiconwxicon">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Name, [Option]) -> wxIcon() when
	Name::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}
		 | {'desiredWidth', integer()}
		 | {'desiredHeight', integer()}.
new(Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({type, _type} = Arg) -> Arg;
          ({desiredWidth, _desiredWidth} = Arg) -> Arg;
          ({desiredHeight, _desiredHeight} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC, Opts,?get_env(),?wxIcon_new_2),
  wxe_util:rec(?wxIcon_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxicon.html#wxiconcopyfrombitmap">external documentation</a>.
-spec copyFromBitmap(This, Bmp) -> 'ok' when
	This::wxIcon(), Bmp::wxBitmap:wxBitmap().
copyFromBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp) ->
  ?CLASS(ThisT,wxIcon),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Bmp,?get_env(),?wxIcon_CopyFromBitmap).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxIcon()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxIcon),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
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
isOk(This) -> wxBitmap:isOk(This).
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
