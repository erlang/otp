%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(wxCursor).
-moduledoc """
Functions for wxCursor class

A cursor is a small bitmap usually used for denoting where the mouse pointer is,
with a picture that might indicate the interpretation of a mouse click. As with
icons, cursors in X and MS Windows are created in a different manner. Therefore,
separate cursors will be created for the different environments.
Platform-specific methods for creating a `m:wxCursor` object are catered for,
and this is an occasion where conditional compilation will probably be required
(see `m:wxIcon` for an example).

A single cursor object may be used in many windows (any subwindow type). The
wxWidgets convention is to set the cursor for a window, as in X, rather than to
set it globally as in MS Windows, although a global `wx_misc:setCursor/1`
function is also available for MS Windows use.

Creating a Custom Cursor

The following is an example of creating a cursor from 32x32 bitmap data
(down_bits) and a mask (down_mask) where 1 is black and 0 is white for the bits,
and 1 is opaque and 0 is transparent for the mask. It works on Windows and GTK+.

Predefined objects (include wx.hrl):

See: `m:wxBitmap`, `m:wxIcon`, `wxWindow:setCursor/2`, `wx_misc:setCursor/1`,
?wxStockCursor

This class is derived (and can use functions) from: `m:wxBitmap`

wxWidgets docs: [wxCursor](https://docs.wxwidgets.org/3.1/classwx_cursor.html)
""".
-include("wxe.hrl").
-export([destroy/1,isOk/1,new/0,new/1,new/2,ok/1]).

%% inherited exports
-export([convertToImage/1,copyFromIcon/2,getDepth/1,getHeight/1,getMask/1,getPalette/1,
  getSubBitmap/2,getWidth/1,loadFile/2,loadFile/3,parent_class/1,saveFile/3,
  saveFile/4,setDepth/2,setHeight/2,setMask/2,setPalette/2,setWidth/2]).

-type wxCursor() :: wx:wx_object().
-export_type([wxCursor/0]).
%% @hidden
-doc false.
parent_class(wxBitmap) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxCursor().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxCursor_new_0),
  wxe_util:rec(?wxCursor_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
%% <br /> Also:<br />
%% new(Image) -> wxCursor() when<br />
%% 	Image::wxImage:wxImage() | wxCursor:wxCursor();<br />
%%       (CursorId) -> wxCursor() when<br />
%% 	CursorId::wx:wx_enum().<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
%%<br /> CursorId = ?wxCURSOR_NONE | ?wxCURSOR_ARROW | ?wxCURSOR_RIGHT_ARROW | ?wxCURSOR_BULLSEYE | ?wxCURSOR_CHAR | ?wxCURSOR_CROSS | ?wxCURSOR_HAND | ?wxCURSOR_IBEAM | ?wxCURSOR_LEFT_BUTTON | ?wxCURSOR_MAGNIFIER | ?wxCURSOR_MIDDLE_BUTTON | ?wxCURSOR_NO_ENTRY | ?wxCURSOR_PAINT_BRUSH | ?wxCURSOR_PENCIL | ?wxCURSOR_POINT_LEFT | ?wxCURSOR_POINT_RIGHT | ?wxCURSOR_QUESTION_ARROW | ?wxCURSOR_RIGHT_BUTTON | ?wxCURSOR_SIZENESW | ?wxCURSOR_SIZENS | ?wxCURSOR_SIZENWSE | ?wxCURSOR_SIZEWE | ?wxCURSOR_SIZING | ?wxCURSOR_SPRAYCAN | ?wxCURSOR_WAIT | ?wxCURSOR_WATCH | ?wxCURSOR_BLANK | ?wxCURSOR_DEFAULT | ?wxCURSOR_ARROWWAIT | ?wxCURSOR_MAX
-doc "Constructs a cursor using a cursor identifier.".
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-doc """
Constructs a cursor by passing a string resource name or filename.

The arguments `hotSpotX` and `hotSpotY` are only used when there's no hotspot
info in the resource/image-file to load (e.g. when using `wxBITMAP_TYPE_ICO`
under wxMSW or `wxBITMAP_TYPE_XPM` under wxGTK).
""".
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorisok">external documentation</a>.
-doc "See: `isOk/1`.".
-spec ok(This) -> boolean() when
	This::wxCursor().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorisok">external documentation</a>.
-doc "Returns true if cursor data is present.".
-spec isOk(This) -> boolean() when
	This::wxCursor().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCursor),
  wxe_util:queue_cmd(This,?get_env(),?wxCursor_IsOk),
  wxe_util:rec(?wxCursor_IsOk).

%% @doc Destroys this object, do not use object again
-doc """
Destroys the cursor.

See reference-counted object destruction for more info.

A cursor can be reused for more than one window, and does not get destroyed when
the window is destroyed. wxWidgets destroys all cursors on application exit,
although it is best to clean them up explicitly.
""".
-spec destroy(This::wxCursor()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCursor),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxBitmap
%% @hidden
-doc false.
setWidth(This,Width) -> wxBitmap:setWidth(This,Width).
%% @hidden
-doc false.
setPalette(This,Palette) -> wxBitmap:setPalette(This,Palette).
%% @hidden
-doc false.
setMask(This,Mask) -> wxBitmap:setMask(This,Mask).
%% @hidden
-doc false.
setHeight(This,Height) -> wxBitmap:setHeight(This,Height).
%% @hidden
-doc false.
setDepth(This,Depth) -> wxBitmap:setDepth(This,Depth).
%% @hidden
-doc false.
saveFile(This,Name,Type, Options) -> wxBitmap:saveFile(This,Name,Type, Options).
%% @hidden
-doc false.
saveFile(This,Name,Type) -> wxBitmap:saveFile(This,Name,Type).
%% @hidden
-doc false.
loadFile(This,Name, Options) -> wxBitmap:loadFile(This,Name, Options).
%% @hidden
-doc false.
loadFile(This,Name) -> wxBitmap:loadFile(This,Name).
%% @hidden
-doc false.
getSubBitmap(This,Rect) -> wxBitmap:getSubBitmap(This,Rect).
%% @hidden
-doc false.
getWidth(This) -> wxBitmap:getWidth(This).
%% @hidden
-doc false.
getMask(This) -> wxBitmap:getMask(This).
%% @hidden
-doc false.
getPalette(This) -> wxBitmap:getPalette(This).
%% @hidden
-doc false.
getHeight(This) -> wxBitmap:getHeight(This).
%% @hidden
-doc false.
getDepth(This) -> wxBitmap:getDepth(This).
%% @hidden
-doc false.
copyFromIcon(This,Icon) -> wxBitmap:copyFromIcon(This,Icon).
%% @hidden
-doc false.
convertToImage(This) -> wxBitmap:convertToImage(This).
