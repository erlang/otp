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

-module(wxIcon).
-moduledoc """
An icon is a small rectangular bitmap usually used for denoting a minimized application.

It differs from a `m:wxBitmap` in always having a mask associated with it for transparent
drawing. On some platforms, icons and bitmaps are implemented identically, since there is
no real distinction between a `m:wxBitmap` with a mask and an icon; and there is no
specific icon format on some platforms (X-based applications usually standardize on XPMs
for small bitmaps and icons). However, some platforms (such as Windows) make the
distinction, so a separate class is provided.

Remark: It is usually desirable to associate a pertinent icon with a frame. Icons can
also be used for other purposes, for example with `m:wxTreeCtrl` and `m:wxListCtrl`. Icons
have different formats on different platforms therefore separate icons will usually be
created for the different environments. Platform-specific methods for creating a `m:wxIcon`
structure are catered for, and this is an occasion where conditional compilation will
probably be required. Note that a new icon must be created for every time the icon is to
be used for a new window. In Windows, the icon will not be reloaded if it has already been
used. An icon allocated to a frame will be deleted when the frame is deleted. For more
information please see overview_bitmap.

Predefined objects (include wx.hrl): ?wxNullIcon

See:
* [Overview bitmap](https://docs.wxwidgets.org/3.2/overview_bitmap.html#overview_bitmap)

* [Overview bitmap](https://docs.wxwidgets.org/3.2/overview_bitmap.html#overview_bitmap_supportedformats)

* `m:wxIconBundle`

* `wxDC:drawIcon/3`

* `m:wxCursor`

This class is derived, and can use functions, from:

* `m:wxBitmap`

wxWidgets docs: [wxIcon](https://docs.wxwidgets.org/3.2/classwx_icon.html)
""".
-include("wxe.hrl").
-export([copyFromBitmap/2,destroy/1,new/0,new/1,new/2]).

%% inherited exports
-export([convertToImage/1,copyFromIcon/2,getDepth/1,getHeight/1,getMask/1,getPalette/1,
  getSubBitmap/2,getWidth/1,isOk/1,loadFile/2,loadFile/3,ok/1,parent_class/1,
  saveFile/3,saveFile/4,setDepth/2,setHeight/2,setMask/2,setPalette/2,
  setWidth/2]).

-type wxIcon() :: wx:wx_object().
-export_type([wxIcon/0]).
-doc false.
parent_class(wxBitmap) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Default ctor.

Constructs an icon object with no data; an assignment or another member function such as `wxBitmap:loadFile/3`
must be called subsequently.
""".
-spec new() -> wxIcon().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxIcon_new_0),
  wxe_util:rec(?wxIcon_new_0).

-doc "Copy ctor.".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
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

-doc """
Loads an icon from a file or resource.

See: `wxBitmap:loadFile/3`
""".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
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

-doc """
Copies `bmp` bitmap to this icon.

Under MS Windows the bitmap must have mask colour set.

See: `wxBitmap:loadFile/3`
""".
-spec copyFromBitmap(This, Bmp) -> 'ok' when
	This::wxIcon(), Bmp::wxBitmap:wxBitmap().
copyFromBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp) ->
  ?CLASS(ThisT,wxIcon),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Bmp,?get_env(),?wxIcon_CopyFromBitmap).

-doc "Destroys the object".
-spec destroy(This::wxIcon()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxIcon),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxBitmap
-doc false.
setWidth(This,Width) -> wxBitmap:setWidth(This,Width).
-doc false.
setPalette(This,Palette) -> wxBitmap:setPalette(This,Palette).
-doc false.
setMask(This,Mask) -> wxBitmap:setMask(This,Mask).
-doc false.
setHeight(This,Height) -> wxBitmap:setHeight(This,Height).
-doc false.
setDepth(This,Depth) -> wxBitmap:setDepth(This,Depth).
-doc false.
saveFile(This,Name,Type, Options) -> wxBitmap:saveFile(This,Name,Type, Options).
-doc false.
saveFile(This,Name,Type) -> wxBitmap:saveFile(This,Name,Type).
-doc false.
isOk(This) -> wxBitmap:isOk(This).
-doc false.
ok(This) -> wxBitmap:ok(This).
-doc false.
loadFile(This,Name, Options) -> wxBitmap:loadFile(This,Name, Options).
-doc false.
loadFile(This,Name) -> wxBitmap:loadFile(This,Name).
-doc false.
getSubBitmap(This,Rect) -> wxBitmap:getSubBitmap(This,Rect).
-doc false.
getWidth(This) -> wxBitmap:getWidth(This).
-doc false.
getMask(This) -> wxBitmap:getMask(This).
-doc false.
getPalette(This) -> wxBitmap:getPalette(This).
-doc false.
getHeight(This) -> wxBitmap:getHeight(This).
-doc false.
getDepth(This) -> wxBitmap:getDepth(This).
-doc false.
copyFromIcon(This,Icon) -> wxBitmap:copyFromIcon(This,Icon).
-doc false.
convertToImage(This) -> wxBitmap:convertToImage(This).
