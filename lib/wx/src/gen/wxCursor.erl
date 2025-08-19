%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxCursor).
-moduledoc """
A cursor is a small bitmap usually used for denoting where the mouse pointer is, with a
picture that might indicate the interpretation of a mouse click.

As with icons, cursors in X and MS Windows are created in a different manner. Therefore,
separate cursors will be created for the different environments. Platform-specific methods
for creating a `m:wxCursor` object are catered for, and this is an occasion where
conditional compilation will probably be required (see `m:wxIcon` for an example).

A single cursor object may be used in many windows (any subwindow type). The wxWidgets
convention is to set the cursor for a window, as in X, rather than to set it globally as
in MS Windows, although a global `wx_misc:setCursor/1` function is also available for MS Windows use.

Creating a Custom Cursor

The following is an example of creating a cursor from 32x32 bitmap data (down_bits) and a
mask (down_mask) where 1 is black and 0 is white for the bits, and 1 is opaque and 0 is
transparent for the mask. It works on Windows and GTK+.

Predefined objects (include wx.hrl):

* ?wxNullCursor

* ?wxSTANDARD\_CURSOR

* ?wxHOURGLASS\_CURSOR

* ?wxCROSS\_CURSOR

See:
* `m:wxBitmap`

* `m:wxIcon`

* `wxWindow:setCursor/2`

* `wx_misc:setCursor/1`

* ?wxStockCursor

This class is derived, and can use functions, from:

* `m:wxBitmap`

wxWidgets docs: [wxCursor](https://docs.wxwidgets.org/3.2/classwx_cursor.html)
""".
-include("wxe.hrl").
-export([destroy/1,isOk/1,new/0,new/1,new/2,ok/1]).

%% inherited exports
-export([convertToImage/1,copyFromIcon/2,getDepth/1,getHeight/1,getMask/1,getPalette/1,
  getSubBitmap/2,getWidth/1,loadFile/2,loadFile/3,parent_class/1,saveFile/3,
  saveFile/4,setDepth/2,setHeight/2,setMask/2,setPalette/2,setWidth/2]).

-type wxCursor() :: wx:wx_object().
-export_type([wxCursor/0]).
-doc false.
parent_class(wxBitmap) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxCursor().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxCursor_new_0),
  wxe_util:rec(?wxCursor_new_0).

-doc "Constructs a cursor using a cursor identifier.".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
%%  CursorId = ?wxCURSOR_NONE | ?wxCURSOR_ARROW | ?wxCURSOR_RIGHT_ARROW | ?wxCURSOR_BULLSEYE | ?wxCURSOR_CHAR | ?wxCURSOR_CROSS | ?wxCURSOR_HAND | ?wxCURSOR_IBEAM | ?wxCURSOR_LEFT_BUTTON | ?wxCURSOR_MAGNIFIER | ?wxCURSOR_MIDDLE_BUTTON | ?wxCURSOR_NO_ENTRY | ?wxCURSOR_PAINT_BRUSH | ?wxCURSOR_PENCIL | ?wxCURSOR_POINT_LEFT | ?wxCURSOR_POINT_RIGHT | ?wxCURSOR_QUESTION_ARROW | ?wxCURSOR_RIGHT_BUTTON | ?wxCURSOR_SIZENESW | ?wxCURSOR_SIZENS | ?wxCURSOR_SIZENWSE | ?wxCURSOR_SIZEWE | ?wxCURSOR_SIZING | ?wxCURSOR_SPRAYCAN | ?wxCURSOR_WAIT | ?wxCURSOR_WATCH | ?wxCURSOR_BLANK | ?wxCURSOR_DEFAULT | ?wxCURSOR_ARROWWAIT | ?wxCURSOR_MAX
-spec new(CursorName) -> wxCursor() when
	CursorName::unicode:chardata();
      (Image) -> wxCursor() when
	Image::wxImage:wxImage() | wxCursor:wxCursor();
      (CursorId) -> wxCursor() when
	CursorId::wx:wx_enum().

new(CursorName)
 when ?is_chardata(CursorName) ->
  new(CursorName, []);
new(#wx_ref{type=ImageT}=Image) ->
  IswxImage = ?CLASS_T(ImageT,wxImage),
  IswxCursor = ?CLASS_T(ImageT,wxCursor),
  ImageType = if
    IswxImage ->   wxImage;
    IswxCursor ->   wxCursor;
    true -> error({badarg, ImageT})
  end,
  wxe_util:queue_cmd(wx:typeCast(Image, ImageType),?get_env(),?wxCursor_new_1_0),
  wxe_util:rec(?wxCursor_new_1_0);
new(CursorId)
 when is_integer(CursorId) ->
  wxe_util:queue_cmd(CursorId,?get_env(),?wxCursor_new_1_1),
  wxe_util:rec(?wxCursor_new_1_1).

-doc """
Constructs a cursor by passing a string resource name or filename.

The arguments `hotSpotX` and `hotSpotY` are only used when there's no hotspot info in the
resource/image-file to load (e.g. when using `wxBITMAP_TYPE_ICO` under wxMSW or `wxBITMAP_TYPE_XPM`
under wxGTK).
""".
%%  Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(CursorName, [Option]) -> wxCursor() when
	CursorName::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}
		 | {'hotSpotX', integer()}
		 | {'hotSpotY', integer()}.
new(CursorName, Options)
 when ?is_chardata(CursorName),is_list(Options) ->
  CursorName_UC = unicode:characters_to_binary(CursorName),
  MOpts = fun({type, _type} = Arg) -> Arg;
          ({hotSpotX, _hotSpotX} = Arg) -> Arg;
          ({hotSpotY, _hotSpotY} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(CursorName_UC, Opts,?get_env(),?wxCursor_new_2),
  wxe_util:rec(?wxCursor_new_2).

-doc "Equivalent to: `isOk/1`".
-spec ok(This) -> boolean() when
	This::wxCursor().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

-doc "Returns true if cursor data is present.".
-spec isOk(This) -> boolean() when
	This::wxCursor().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCursor),
  wxe_util:queue_cmd(This,?get_env(),?wxCursor_IsOk),
  wxe_util:rec(?wxCursor_IsOk).

-doc "Destroys the object".
-spec destroy(This::wxCursor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCursor),
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
