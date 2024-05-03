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

-module(wxDC).
-moduledoc """
Functions for wxDC class

A `m:wxDC` is a `"device context"` onto which graphics and text can be drawn. It
is intended to represent different output devices and offers a common abstract
API for drawing on any of them.

wxWidgets offers an alternative drawing API based on the modern drawing backends
GDI+, CoreGraphics, Cairo and Direct2D. See `m:wxGraphicsContext`,
`m:wxGraphicsRenderer` and related classes. There is also a `m:wxGCDC` linking
the APIs by offering the `m:wxDC` API on top of a `m:wxGraphicsContext`.

`m:wxDC` is an abstract base class and cannot be created directly. Use
`m:wxPaintDC`, `m:wxClientDC`, `m:wxWindowDC`, `m:wxScreenDC`, `m:wxMemoryDC` or
`wxPrinterDC` (not implemented in wx). Notice that device contexts which are
associated with windows (i.e. `m:wxClientDC`, `m:wxWindowDC` and `m:wxPaintDC`)
use the window font and colours by default (starting with wxWidgets 2.9.0) but
the other device context classes use system-default values so you always must
set the appropriate fonts and colours before using them.

In addition to the versions of the methods documented below, there are also
versions which accept single \{X,Y\} parameter instead of the two wxCoord ones
or \{X,Y\} and \{Width,Height\} instead of the four wxCoord parameters.

Beginning with wxWidgets 2.9.0 the entire `m:wxDC` code has been reorganized.
All platform dependent code (actually all drawing code) has been moved into
backend classes which derive from a common wxDCImpl class. The user-visible
classes such as `m:wxClientDC` and `m:wxPaintDC` merely forward all calls to the
backend implementation.

Device and logical units

In the `m:wxDC` context there is a distinction between `logical` units and
`device` units.

`Device` units are the units native to the particular device; e.g. for a screen,
a device unit is a `pixel`. For a printer, the device unit is defined by the
resolution of the printer (usually given in `DPI:` dot-per-inch).

All `m:wxDC` functions use instead `logical` units, unless where explicitly
stated. Logical units are arbitrary units mapped to device units using the
current mapping mode (see `setMapMode/2`).

This mechanism allows reusing the same code which prints on e.g. a window on the
screen to print on e.g. a paper.

Support for Transparency / Alpha Channel

In general `m:wxDC` methods don't support alpha transparency and the alpha
component of [`wx_color()`](`t:wx:wx_colour/0`) is simply ignored and you need
to use `m:wxGraphicsContext` for full transparency support. There are, however,
a few exceptions: first, under macOS and GTK+ 3 colours with alpha channel are
supported in all the normal wxDC-derived classes as they use
`m:wxGraphicsContext` internally. Second, under all platforms `wxSVGFileDC` (not
implemented in wx) also fully supports alpha channel. In both of these cases the
instances of `m:wxPen` or `m:wxBrush` that are built from
[`wx_color()`](`t:wx:wx_colour/0`) use the colour's alpha values when stroking
or filling.

Support for Transformation Matrix

On some platforms (currently under MSW, GTK+ 3, macOS) `m:wxDC` has support for
applying an arbitrary affine transformation matrix to its coordinate system
(since 3.1.1 this feature is also supported by `m:wxGCDC` in all ports). Call
`CanUseTransformMatrix()` (not implemented in wx) to check if this support is
available and then call `SetTransformMatrix()` (not implemented in wx) if it is.
If the transformation matrix is not supported, `SetTransformMatrix()` (not
implemented in wx) always simply returns `false` and doesn't do anything.

This feature is only available when `wxUSE_DC_TRANSFORM_MATRIX` build option is
enabled.

See: [Overview dc](https://docs.wxwidgets.org/3.1/overview_dc.html#overview_dc),
`m:wxGraphicsContext`, `wxDCFontChanger` (not implemented in wx),
`wxDCTextColourChanger` (not implemented in wx), `wxDCPenChanger` (not
implemented in wx), `wxDCBrushChanger` (not implemented in wx), `wxDCClipper`
(not implemented in wx)

wxWidgets docs: [wxDC](https://docs.wxwidgets.org/3.1/classwx_d_c.html)
""".
-include("wxe.hrl").
-export([blit/5,blit/6,calcBoundingBox/3,clear/1,crossHair/2,destroyClippingRegion/1,
  deviceToLogicalX/2,deviceToLogicalXRel/2,deviceToLogicalY/2,deviceToLogicalYRel/2,
  drawArc/4,drawBitmap/3,drawBitmap/4,drawCheckMark/2,drawCircle/3,drawEllipse/2,
  drawEllipse/3,drawEllipticArc/5,drawIcon/3,drawLabel/3,drawLabel/4,
  drawLine/3,drawLines/2,drawLines/3,drawPoint/2,drawPolygon/2,drawPolygon/3,
  drawRectangle/2,drawRectangle/3,drawRotatedText/4,drawRoundedRectangle/3,
  drawRoundedRectangle/4,drawText/3,endDoc/1,endPage/1,floodFill/3,floodFill/4,
  getBackground/1,getBackgroundMode/1,getBrush/1,getCharHeight/1,getCharWidth/1,
  getClippingBox/1,getFont/1,getLayoutDirection/1,getLogicalFunction/1,
  getMapMode/1,getMultiLineTextExtent/2,getMultiLineTextExtent/3,getPPI/1,
  getPartialTextExtents/2,getPen/1,getPixel/2,getSize/1,getSizeMM/1,
  getTextBackground/1,getTextExtent/2,getTextExtent/3,getTextForeground/1,
  getUserScale/1,gradientFillConcentric/4,gradientFillConcentric/5,
  gradientFillLinear/4,gradientFillLinear/5,isOk/1,logicalToDeviceX/2,
  logicalToDeviceXRel/2,logicalToDeviceY/2,logicalToDeviceYRel/2,maxX/1,
  maxY/1,minX/1,minY/1,resetBoundingBox/1,setAxisOrientation/3,setBackground/2,
  setBackgroundMode/2,setBrush/2,setClippingRegion/2,setClippingRegion/3,
  setDeviceOrigin/3,setFont/2,setLayoutDirection/2,setLogicalFunction/2,
  setMapMode/2,setPalette/2,setPen/2,setTextBackground/2,setTextForeground/2,
  setUserScale/3,startDoc/2,startPage/1]).

%% inherited exports
-export([parent_class/1]).

-type wxDC() :: wx:wx_object().
-export_type([wxDC/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv blit(This,Dest,Size,Source,Src, [])
-spec blit(This, Dest, Size, Source, Src) -> boolean() when
	This::wxDC(), Dest::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Source::wxDC(), Src::{X::integer(), Y::integer()}.

blit(This,{DestX,DestY} = Dest,{SizeW,SizeH} = Size,Source,{SrcX,SrcY} = Src)
 when is_record(This, wx_ref),is_integer(DestX),is_integer(DestY),is_integer(SizeW),is_integer(SizeH),is_record(Source, wx_ref),is_integer(SrcX),is_integer(SrcY) ->
  blit(This,Dest,Size,Source,Src, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcblit">external documentation</a>.
%%<br /> Rop = ?wxCLEAR | ?wxXOR | ?wxINVERT | ?wxOR_REVERSE | ?wxAND_REVERSE | ?wxCOPY | ?wxAND | ?wxAND_INVERT | ?wxNO_OP | ?wxNOR | ?wxEQUIV | ?wxSRC_INVERT | ?wxOR_INVERT | ?wxNAND | ?wxOR | ?wxSET
-doc """
Copy from a source DC to this DC.

With this method you can specify the destination coordinates and the size of
area to copy which will be the same for both the source and target DCs. If you
need to apply scaling while copying, use `StretchBlit()` (not implemented in
wx).

Notice that source DC coordinates `xsrc` and `ysrc` are interpreted using the
current source DC coordinate system, i.e. the scale, origin position and axis
directions are taken into account when transforming them to physical (pixel)
coordinates.

Remark: There is partial support for `blit/6` in `m:wxPostScriptDC`, under X.

See: `StretchBlit()` (not implemented in wx), `m:wxMemoryDC`, `m:wxBitmap`,
`m:wxMask`
""".
-spec blit(This, Dest, Size, Source, Src, [Option]) -> boolean() when
	This::wxDC(), Dest::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Source::wxDC(), Src::{X::integer(), Y::integer()},
	Option :: {'rop', wx:wx_enum()}
		 | {'useMask', boolean()}
		 | {'srcPtMask', {X::integer(), Y::integer()}}.
blit(#wx_ref{type=ThisT}=This,{DestX,DestY} = Dest,{SizeW,SizeH} = Size,#wx_ref{type=SourceT}=Source,{SrcX,SrcY} = Src, Options)
 when is_integer(DestX),is_integer(DestY),is_integer(SizeW),is_integer(SizeH),is_integer(SrcX),is_integer(SrcY),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(SourceT,wxDC),
  MOpts = fun({rop, _rop} = Arg) -> Arg;
          ({useMask, _useMask} = Arg) -> Arg;
          ({srcPtMask, {_srcPtMaskX,_srcPtMaskY}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Dest,Size,Source,Src, Opts,?get_env(),?wxDC_Blit),
  wxe_util:rec(?wxDC_Blit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdccalcboundingbox">external documentation</a>.
-doc """
Adds the specified point to the bounding box which can be retrieved with
`minX/1`, `maxX/1` and `minY/1`, `maxY/1` functions.

See: `resetBoundingBox/1`
""".
-spec calcBoundingBox(This, X, Y) -> 'ok' when
	This::wxDC(), X::integer(), Y::integer().
calcBoundingBox(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxDC_CalcBoundingBox).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcclear">external documentation</a>.
-doc """
Clears the device context using the current background brush.

Note that `setBackground/2` method must be used to set the brush used by
`clear/1`, the brush used for filling the shapes set by `setBrush/2` is ignored
by it.

If no background brush was set, solid white brush is used to clear the device
context.
""".
-spec clear(This) -> 'ok' when
	This::wxDC().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdccrosshair">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec crossHair(This, Pt) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}.
crossHair(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxDC_CrossHair).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdestroyclippingregion">external documentation</a>.
-doc """
Destroys the current clipping region so that none of the DC is clipped.

See: `setClippingRegion/3`
""".
-spec destroyClippingRegion(This) -> 'ok' when
	This::wxDC().
destroyClippingRegion(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_DestroyClippingRegion).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdevicetologicalx">external documentation</a>.
-doc """
Convert `device` X coordinate to logical coordinate, using the current mapping
mode, user scale factor, device origin and axis orientation.
""".
-spec deviceToLogicalX(This, X) -> integer() when
	This::wxDC(), X::integer().
deviceToLogicalX(#wx_ref{type=ThisT}=This,X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,X,?get_env(),?wxDC_DeviceToLogicalX),
  wxe_util:rec(?wxDC_DeviceToLogicalX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdevicetologicalxrel">external documentation</a>.
-doc """
Convert `device` X coordinate to relative logical coordinate, using the current
mapping mode and user scale factor but ignoring the axis orientation.

Use this for converting a width, for example.
""".
-spec deviceToLogicalXRel(This, X) -> integer() when
	This::wxDC(), X::integer().
deviceToLogicalXRel(#wx_ref{type=ThisT}=This,X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,X,?get_env(),?wxDC_DeviceToLogicalXRel),
  wxe_util:rec(?wxDC_DeviceToLogicalXRel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdevicetologicaly">external documentation</a>.
-doc """
Converts `device` Y coordinate to logical coordinate, using the current mapping
mode, user scale factor, device origin and axis orientation.
""".
-spec deviceToLogicalY(This, Y) -> integer() when
	This::wxDC(), Y::integer().
deviceToLogicalY(#wx_ref{type=ThisT}=This,Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Y,?get_env(),?wxDC_DeviceToLogicalY),
  wxe_util:rec(?wxDC_DeviceToLogicalY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdevicetologicalyrel">external documentation</a>.
-doc """
Convert `device` Y coordinate to relative logical coordinate, using the current
mapping mode and user scale factor but ignoring the axis orientation.

Use this for converting a height, for example.
""".
-spec deviceToLogicalYRel(This, Y) -> integer() when
	This::wxDC(), Y::integer().
deviceToLogicalYRel(#wx_ref{type=ThisT}=This,Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Y,?get_env(),?wxDC_DeviceToLogicalYRel),
  wxe_util:rec(?wxDC_DeviceToLogicalYRel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawarc">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawArc(This, PtStart, PtEnd, Centre) -> 'ok' when
	This::wxDC(), PtStart::{X::integer(), Y::integer()}, PtEnd::{X::integer(), Y::integer()}, Centre::{X::integer(), Y::integer()}.
drawArc(#wx_ref{type=ThisT}=This,{PtStartX,PtStartY} = PtStart,{PtEndX,PtEndY} = PtEnd,{CentreX,CentreY} = Centre)
 when is_integer(PtStartX),is_integer(PtStartY),is_integer(PtEndX),is_integer(PtEndY),is_integer(CentreX),is_integer(CentreY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,PtStart,PtEnd,Centre,?get_env(),?wxDC_DrawArc).

%% @equiv drawBitmap(This,Bmp,Pt, [])
-spec drawBitmap(This, Bmp, Pt) -> 'ok' when
	This::wxDC(), Bmp::wxBitmap:wxBitmap(), Pt::{X::integer(), Y::integer()}.

drawBitmap(This,Bmp,{PtX,PtY} = Pt)
 when is_record(This, wx_ref),is_record(Bmp, wx_ref),is_integer(PtX),is_integer(PtY) ->
  drawBitmap(This,Bmp,Pt, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawbitmap">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawBitmap(This, Bmp, Pt, [Option]) -> 'ok' when
	This::wxDC(), Bmp::wxBitmap:wxBitmap(), Pt::{X::integer(), Y::integer()},
	Option :: {'useMask', boolean()}.
drawBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp,{PtX,PtY} = Pt, Options)
 when is_integer(PtX),is_integer(PtY),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BmpT,wxBitmap),
  MOpts = fun({useMask, _useMask} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Bmp,Pt, Opts,?get_env(),?wxDC_DrawBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawcheckmark">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawCheckMark(This, Rect) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
drawCheckMark(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxDC_DrawCheckMark).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawcircle">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawCircle(This, Pt, Radius) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Radius::integer().
drawCircle(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt,Radius)
 when is_integer(PtX),is_integer(PtY),is_integer(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt,Radius,?get_env(),?wxDC_DrawCircle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawellipse">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawEllipse(This, Rect) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
drawEllipse(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxDC_DrawEllipse_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawellipse">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawEllipse(This, Pt, Size) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}.
drawEllipse(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt,{SizeW,SizeH} = Size)
 when is_integer(PtX),is_integer(PtY),is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt,Size,?get_env(),?wxDC_DrawEllipse_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawellipticarc">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawEllipticArc(This, Pt, Sz, Sa, Ea) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}, Sa::number(), Ea::number().
drawEllipticArc(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt,{SzW,SzH} = Sz,Sa,Ea)
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH),is_number(Sa),is_number(Ea) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt,Sz,Sa,Ea,?get_env(),?wxDC_DrawEllipticArc).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawicon">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawIcon(This, Icon, Pt) -> 'ok' when
	This::wxDC(), Icon::wxIcon:wxIcon(), Pt::{X::integer(), Y::integer()}.
drawIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,Pt,?get_env(),?wxDC_DrawIcon).

%% @equiv drawLabel(This,Text,Rect, [])
-spec drawLabel(This, Text, Rect) -> 'ok' when
	This::wxDC(), Text::unicode:chardata(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.

drawLabel(This,Text,{RectX,RectY,RectW,RectH} = Rect)
 when is_record(This, wx_ref),?is_chardata(Text),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  drawLabel(This,Text,Rect, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawlabel">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawLabel(This, Text, Rect, [Option]) -> 'ok' when
	This::wxDC(), Text::unicode:chardata(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},
	Option :: {'alignment', integer()}
		 | {'indexAccel', integer()}.
drawLabel(#wx_ref{type=ThisT}=This,Text,{RectX,RectY,RectW,RectH} = Rect, Options)
 when ?is_chardata(Text),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({alignment, _alignment} = Arg) -> Arg;
          ({indexAccel, _indexAccel} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Text_UC,Rect, Opts,?get_env(),?wxDC_DrawLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawline">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawLine(This, Pt1, Pt2) -> 'ok' when
	This::wxDC(), Pt1::{X::integer(), Y::integer()}, Pt2::{X::integer(), Y::integer()}.
drawLine(#wx_ref{type=ThisT}=This,{Pt1X,Pt1Y} = Pt1,{Pt2X,Pt2Y} = Pt2)
 when is_integer(Pt1X),is_integer(Pt1Y),is_integer(Pt2X),is_integer(Pt2Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt1,Pt2,?get_env(),?wxDC_DrawLine).

%% @equiv drawLines(This,Points, [])
-spec drawLines(This, Points) -> 'ok' when
	This::wxDC(), Points::[{X::integer(), Y::integer()}].

drawLines(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawLines(This,Points, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawlines">external documentation</a>.
-doc """
Draws lines using an array of points of size `n` adding the optional offset
coordinate.

The current pen is used for drawing the lines.
""".
-spec drawLines(This, Points, [Option]) -> 'ok' when
	This::wxDC(), Points::[{X::integer(), Y::integer()}],
	Option :: {'xoffset', integer()}
		 | {'yoffset', integer()}.
drawLines(#wx_ref{type=ThisT}=This,Points, Options)
 when is_list(Points),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({xoffset, _xoffset} = Arg) -> Arg;
          ({yoffset, _yoffset} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Points, Opts,?get_env(),?wxDC_DrawLines).

%% @equiv drawPolygon(This,Points, [])
-spec drawPolygon(This, Points) -> 'ok' when
	This::wxDC(), Points::[{X::integer(), Y::integer()}].

drawPolygon(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawPolygon(This,Points, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawpolygon">external documentation</a>.
%%<br /> FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
-doc """
Draws a filled polygon using an array of points of size `n`, adding the optional
offset coordinate.

The first and last points are automatically closed.

The last argument specifies the fill rule: `wxODDEVEN_RULE` (the default) or
`wxWINDING_RULE`.

The current pen is used for drawing the outline, and the current brush for
filling the shape. Using a transparent brush suppresses filling.
""".
-spec drawPolygon(This, Points, [Option]) -> 'ok' when
	This::wxDC(), Points::[{X::integer(), Y::integer()}],
	Option :: {'xoffset', integer()}
		 | {'yoffset', integer()}
		 | {'fillStyle', wx:wx_enum()}.
drawPolygon(#wx_ref{type=ThisT}=This,Points, Options)
 when is_list(Points),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({xoffset, _xoffset} = Arg) -> Arg;
          ({yoffset, _yoffset} = Arg) -> Arg;
          ({fillStyle, _fillStyle} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Points, Opts,?get_env(),?wxDC_DrawPolygon).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawpoint">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawPoint(This, Pt) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}.
drawPoint(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxDC_DrawPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawrectangle">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawRectangle(This, Rect) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
drawRectangle(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxDC_DrawRectangle_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawrectangle">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawRectangle(This, Pt, Sz) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}.
drawRectangle(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt,{SzW,SzH} = Sz)
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt,Sz,?get_env(),?wxDC_DrawRectangle_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawrotatedtext">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawRotatedText(This, Text, Point, Angle) -> 'ok' when
	This::wxDC(), Text::unicode:chardata(), Point::{X::integer(), Y::integer()}, Angle::number().
drawRotatedText(#wx_ref{type=ThisT}=This,Text,{PointX,PointY} = Point,Angle)
 when ?is_chardata(Text),is_integer(PointX),is_integer(PointY),is_number(Angle) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,Point,Angle,?get_env(),?wxDC_DrawRotatedText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawroundedrectangle">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawRoundedRectangle(This, Rect, Radius) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, Radius::number().
drawRoundedRectangle(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect,Radius)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_number(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Rect,Radius,?get_env(),?wxDC_DrawRoundedRectangle_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawroundedrectangle">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawRoundedRectangle(This, Pt, Sz, Radius) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}, Radius::number().
drawRoundedRectangle(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt,{SzW,SzH} = Sz,Radius)
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH),is_number(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt,Sz,Radius,?get_env(),?wxDC_DrawRoundedRectangle_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawtext">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec drawText(This, Text, Pt) -> 'ok' when
	This::wxDC(), Text::unicode:chardata(), Pt::{X::integer(), Y::integer()}.
drawText(#wx_ref{type=ThisT}=This,Text,{PtX,PtY} = Pt)
 when ?is_chardata(Text),is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,Pt,?get_env(),?wxDC_DrawText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcenddoc">external documentation</a>.
-doc "Ends a document (only relevant when outputting to a printer).".
-spec endDoc(This) -> 'ok' when
	This::wxDC().
endDoc(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_EndDoc).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcendpage">external documentation</a>.
-doc "Ends a document page (only relevant when outputting to a printer).".
-spec endPage(This) -> 'ok' when
	This::wxDC().
endPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_EndPage).

%% @equiv floodFill(This,Pt,Col, [])
-spec floodFill(This, Pt, Col) -> boolean() when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Col::wx:wx_colour().

floodFill(This,{PtX,PtY} = Pt,Col)
 when is_record(This, wx_ref),is_integer(PtX),is_integer(PtY),?is_colordata(Col) ->
  floodFill(This,Pt,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcfloodfill">external documentation</a>.
%%<br /> Style = ?wxFLOOD_SURFACE | ?wxFLOOD_BORDER
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec floodFill(This, Pt, Col, [Option]) -> boolean() when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Col::wx:wx_colour(),
	Option :: {'style', wx:wx_enum()}.
floodFill(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt,Col, Options)
 when is_integer(PtX),is_integer(PtY),?is_colordata(Col),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pt,wxe_util:color(Col), Opts,?get_env(),?wxDC_FloodFill),
  wxe_util:rec(?wxDC_FloodFill).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetbackground">external documentation</a>.
-doc """
Gets the brush used for painting the background.

See: `setBackground/2`
""".
-spec getBackground(This) -> wxBrush:wxBrush() when
	This::wxDC().
getBackground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetBackground),
  wxe_util:rec(?wxDC_GetBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetbackgroundmode">external documentation</a>.
-doc """
Returns the current background mode: `wxPENSTYLE_SOLID` or
`wxPENSTYLE_TRANSPARENT`.

See: `setBackgroundMode/2`
""".
-spec getBackgroundMode(This) -> integer() when
	This::wxDC().
getBackgroundMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetBackgroundMode),
  wxe_util:rec(?wxDC_GetBackgroundMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetbrush">external documentation</a>.
-doc """
Gets the current brush.

See: `setBrush/2`
""".
-spec getBrush(This) -> wxBrush:wxBrush() when
	This::wxDC().
getBrush(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetBrush),
  wxe_util:rec(?wxDC_GetBrush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetcharheight">external documentation</a>.
-doc "Gets the character height of the currently set font.".
-spec getCharHeight(This) -> integer() when
	This::wxDC().
getCharHeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetCharHeight),
  wxe_util:rec(?wxDC_GetCharHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetcharwidth">external documentation</a>.
-doc "Gets the average character width of the currently set font.".
-spec getCharWidth(This) -> integer() when
	This::wxDC().
getCharWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetCharWidth),
  wxe_util:rec(?wxDC_GetCharWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetclippingbox">external documentation</a>.
-doc """
`Gets the rectangle surrounding the current clipping region. If no clipping region is set this function returns the extent of the device context. @remarks Clipping region is given in logical coordinates. @param x If non-<span class='literal'>NULL</span>, filled in with the logical horizontal coordinate of the top left corner of the clipping region if the function returns true or 0 otherwise. @param y If non-<span class='literal'>NULL</span>, filled in with the logical vertical coordinate of the top left corner of the clipping region if the function returns true or 0 otherwise. @param width If non-<span class='literal'>NULL</span>, filled in with the width of the clipping region if the function returns true or the device context width otherwise. @param height If non-<span class='literal'>NULL</span>, filled in with the height of the clipping region if the function returns true or the device context height otherwise.`

Return: true if there is a clipping region or false if there is no active
clipping region (note that this return value is available only since wxWidgets
3.1.2, this function didn't return anything in the previous versions).
""".
-spec getClippingBox(This) -> Result when
	Result ::{X::integer(), Y::integer(), Width::integer(), Height::integer()},
	This::wxDC().
getClippingBox(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetClippingBox),
  wxe_util:rec(?wxDC_GetClippingBox).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetfont">external documentation</a>.
-doc """
Gets the current font.

Notice that even although each device context object has some default font after
creation, this method would return a ?wxNullFont initially and only after
calling `setFont/2` a valid font is returned.
""".
-spec getFont(This) -> wxFont:wxFont() when
	This::wxDC().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetFont),
  wxe_util:rec(?wxDC_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetlayoutdirection">external documentation</a>.
%%<br /> Res = ?wxLayout_Default | ?wxLayout_LeftToRight | ?wxLayout_RightToLeft
-doc """
Gets the current layout direction of the device context.

On platforms where RTL layout is supported, the return value will either be
`wxLayout_LeftToRight` or `wxLayout_RightToLeft`. If RTL layout is not
supported, the return value will be `wxLayout_Default`.

See: `setLayoutDirection/2`
""".
-spec getLayoutDirection(This) -> wx:wx_enum() when
	This::wxDC().
getLayoutDirection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetLayoutDirection),
  wxe_util:rec(?wxDC_GetLayoutDirection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetlogicalfunction">external documentation</a>.
%%<br /> Res = ?wxCLEAR | ?wxXOR | ?wxINVERT | ?wxOR_REVERSE | ?wxAND_REVERSE | ?wxCOPY | ?wxAND | ?wxAND_INVERT | ?wxNO_OP | ?wxNOR | ?wxEQUIV | ?wxSRC_INVERT | ?wxOR_INVERT | ?wxNAND | ?wxOR | ?wxSET
-doc """
Gets the current logical function.

See: `setLogicalFunction/2`
""".
-spec getLogicalFunction(This) -> wx:wx_enum() when
	This::wxDC().
getLogicalFunction(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetLogicalFunction),
  wxe_util:rec(?wxDC_GetLogicalFunction).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetmapmode">external documentation</a>.
%%<br /> Res = ?wxMM_TEXT | ?wxMM_METRIC | ?wxMM_LOMETRIC | ?wxMM_TWIPS | ?wxMM_POINTS
-doc """
Gets the current mapping mode for the device context.

See: `setMapMode/2`
""".
-spec getMapMode(This) -> wx:wx_enum() when
	This::wxDC().
getMapMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetMapMode),
  wxe_util:rec(?wxDC_GetMapMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetmultilinetextextent">external documentation</a>.
-doc """
Gets the dimensions of the string using the currently selected font.

`string` is the text string to measure.

Return: The text extent as a \{Width,Height\} object.

Note: This function works with both single-line and multi-line strings.

See: `m:wxFont`, `setFont/2`, `getPartialTextExtents/2`, `getTextExtent/3`
""".
-spec getMultiLineTextExtent(This, String) -> {W::integer(), H::integer()} when
	This::wxDC(), String::unicode:chardata().
getMultiLineTextExtent(#wx_ref{type=ThisT}=This,String)
 when ?is_chardata(String) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary(String),
  wxe_util:queue_cmd(This,String_UC,?get_env(),?wxDC_GetMultiLineTextExtent_1),
  wxe_util:rec(?wxDC_GetMultiLineTextExtent_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetmultilinetextextent">external documentation</a>.
-doc """
Gets the dimensions of the string using the currently selected font.

`string` is the text string to measure, `heightLine`, if non NULL, is where to
store the height of a single line.

The text extent is set in the given `w` and `h` pointers.

If the optional parameter `font` is specified and valid, then it is used for the
text extent calculation, otherwise the currently selected font is used.

If `string` is empty, its horizontal extent is 0 but, for convenience when using
this function for allocating enough space for a possibly multi-line string, its
vertical extent is the same as the height of an empty line of text. Please note
that this behaviour differs from that of `getTextExtent/3`.

Note: This function works with both single-line and multi-line strings.

See: `m:wxFont`, `setFont/2`, `getPartialTextExtents/2`, `getTextExtent/3`
""".
-spec getMultiLineTextExtent(This, String, [Option]) -> {W::integer(), H::integer(), HeightLine::integer()} when
	This::wxDC(), String::unicode:chardata(),
	Option :: {'font', wxFont:wxFont()}.
getMultiLineTextExtent(#wx_ref{type=ThisT}=This,String, Options)
 when ?is_chardata(String),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary(String),
  MOpts = fun({font, #wx_ref{type=FontT}} = Arg) ->   ?CLASS(FontT,wxFont),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,String_UC, Opts,?get_env(),?wxDC_GetMultiLineTextExtent_4),
  wxe_util:rec(?wxDC_GetMultiLineTextExtent_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetpartialtextextents">external documentation</a>.
-doc """
Fills the `widths` array with the widths from the beginning of `text` to the
corresponding character of `text`.

The generic version simply builds a running total of the widths of each
character using `getTextExtent/3`, however if the various platforms have a
native API function that is faster or more accurate than the generic
implementation then it should be used instead.

See: `getMultiLineTextExtent/3`, `getTextExtent/3`
""".
-spec getPartialTextExtents(This, Text) -> Result when
	Result ::{Res ::boolean(), Widths::[integer()]},
	This::wxDC(), Text::unicode:chardata().
getPartialTextExtents(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxDC_GetPartialTextExtents),
  wxe_util:rec(?wxDC_GetPartialTextExtents).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetpen">external documentation</a>.
-doc """
Gets the current pen.

See: `setPen/2`
""".
-spec getPen(This) -> wxPen:wxPen() when
	This::wxDC().
getPen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetPen),
  wxe_util:rec(?wxDC_GetPen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetpixel">external documentation</a>.
-doc """
Gets in `colour` the colour at the specified location.

This method isn't available for `m:wxPostScriptDC` or `wxMetafileDC` (not
implemented in wx) nor for any DC in wxOSX port and simply returns false there.

Note: Setting a pixel can be done using `drawPoint/2`.

Note: This method shouldn't be used with `m:wxPaintDC` as accessing the DC while
drawing can result in unexpected results, notably in wxGTK.
""".
-spec getPixel(This, Pos) -> Result when
	Result ::{Res ::boolean(), Colour::wx:wx_colour4()},
	This::wxDC(), Pos::{X::integer(), Y::integer()}.
getPixel(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos)
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxDC_GetPixel),
  wxe_util:rec(?wxDC_GetPixel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetppi">external documentation</a>.
-doc "Returns the resolution of the device in pixels per inch.".
-spec getPPI(This) -> {W::integer(), H::integer()} when
	This::wxDC().
getPPI(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetPPI),
  wxe_util:rec(?wxDC_GetPPI).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetsize">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxDC().
getSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetSize),
  wxe_util:rec(?wxDC_GetSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetsizemm">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec getSizeMM(This) -> {W::integer(), H::integer()} when
	This::wxDC().
getSizeMM(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetSizeMM),
  wxe_util:rec(?wxDC_GetSizeMM).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgettextbackground">external documentation</a>.
-doc """
Gets the current text background colour.

See: `setTextBackground/2`
""".
-spec getTextBackground(This) -> wx:wx_colour4() when
	This::wxDC().
getTextBackground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetTextBackground),
  wxe_util:rec(?wxDC_GetTextBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgettextextent">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec getTextExtent(This, String) -> {W::integer(), H::integer()} when
	This::wxDC(), String::unicode:chardata().
getTextExtent(#wx_ref{type=ThisT}=This,String)
 when ?is_chardata(String) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary(String),
  wxe_util:queue_cmd(This,String_UC,?get_env(),?wxDC_GetTextExtent_1),
  wxe_util:rec(?wxDC_GetTextExtent_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgettextextent">external documentation</a>.
-doc """
Gets the dimensions of the string using the currently selected font.

`string` is the text string to measure, `descent` is the dimension from the
baseline of the font to the bottom of the descender, and `externalLeading` is
any extra vertical space added to the font by the font designer (usually is
zero).

The text extent is returned in `w` and `h` pointers or as a \{Width,Height\}
object depending on which version of this function is used.

If the optional parameter `font` is specified and valid, then it is used for the
text extent calculation. Otherwise the currently selected font is.

If `string` is empty, its extent is 0 in both directions, as expected.

Note: This function only works with single-line strings.

See: `m:wxFont`, `setFont/2`, `getPartialTextExtents/2`,
`getMultiLineTextExtent/3`
""".
-spec getTextExtent(This, String, [Option]) -> Result when
	Result :: {W::integer(), H::integer(), Descent::integer(), ExternalLeading::integer()},
	This::wxDC(), String::unicode:chardata(),
	Option :: {'theFont', wxFont:wxFont()}.
getTextExtent(#wx_ref{type=ThisT}=This,String, Options)
 when ?is_chardata(String),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary(String),
  MOpts = fun({theFont, #wx_ref{type=TheFontT}} = Arg) ->   ?CLASS(TheFontT,wxFont),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,String_UC, Opts,?get_env(),?wxDC_GetTextExtent_4),
  wxe_util:rec(?wxDC_GetTextExtent_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgettextforeground">external documentation</a>.
-doc """
Gets the current text foreground colour.

See: `setTextForeground/2`
""".
-spec getTextForeground(This) -> wx:wx_colour4() when
	This::wxDC().
getTextForeground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetTextForeground),
  wxe_util:rec(?wxDC_GetTextForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetuserscale">external documentation</a>.
-doc """
Gets the current user scale factor.

See: `setUserScale/3`
""".
-spec getUserScale(This) -> {X::number(), Y::number()} when
	This::wxDC().
getUserScale(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_GetUserScale),
  wxe_util:rec(?wxDC_GetUserScale).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgradientfillconcentric">external documentation</a>.
-doc """
Fill the area specified by rect with a radial gradient, starting from
`initialColour` at the centre of the circle and fading to `destColour` on the
circle outside.

The circle is placed at the centre of `rect`.

Note: Currently this function is very slow, don't use it for real-time drawing.
""".
-spec gradientFillConcentric(This, Rect, InitialColour, DestColour) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, InitialColour::wx:wx_colour(), DestColour::wx:wx_colour().
gradientFillConcentric(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect,InitialColour,DestColour)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),?is_colordata(InitialColour),?is_colordata(DestColour) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Rect,wxe_util:color(InitialColour),wxe_util:color(DestColour),?get_env(),?wxDC_GradientFillConcentric_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgradientfillconcentric">external documentation</a>.
-doc """
Fill the area specified by rect with a radial gradient, starting from
`initialColour` at the centre of the circle and fading to `destColour` on the
circle outside.

`circleCenter` are the relative coordinates of centre of the circle in the
specified `rect`.

Note: Currently this function is very slow, don't use it for real-time drawing.
""".
-spec gradientFillConcentric(This, Rect, InitialColour, DestColour, CircleCenter) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, InitialColour::wx:wx_colour(), DestColour::wx:wx_colour(), CircleCenter::{X::integer(), Y::integer()}.
gradientFillConcentric(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect,InitialColour,DestColour,{CircleCenterX,CircleCenterY} = CircleCenter)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),?is_colordata(InitialColour),?is_colordata(DestColour),is_integer(CircleCenterX),is_integer(CircleCenterY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Rect,wxe_util:color(InitialColour),wxe_util:color(DestColour),CircleCenter,?get_env(),?wxDC_GradientFillConcentric_4).

%% @equiv gradientFillLinear(This,Rect,InitialColour,DestColour, [])
-spec gradientFillLinear(This, Rect, InitialColour, DestColour) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, InitialColour::wx:wx_colour(), DestColour::wx:wx_colour().

gradientFillLinear(This,{RectX,RectY,RectW,RectH} = Rect,InitialColour,DestColour)
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),?is_colordata(InitialColour),?is_colordata(DestColour) ->
  gradientFillLinear(This,Rect,InitialColour,DestColour, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgradientfilllinear">external documentation</a>.
%%<br /> NDirection = ?wxLEFT | ?wxRIGHT | ?wxUP | ?wxDOWN | ?wxTOP | ?wxBOTTOM | ?wxNORTH | ?wxSOUTH | ?wxWEST | ?wxEAST | ?wxALL | ?wxDIRECTION_MASK
-doc """
Fill the area specified by `rect` with a linear gradient, starting from
`initialColour` and eventually fading to `destColour`.

The `nDirection` specifies the direction of the colour change, default is to use
`initialColour` on the left part of the rectangle and `destColour` on the right
one.
""".
-spec gradientFillLinear(This, Rect, InitialColour, DestColour, [Option]) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, InitialColour::wx:wx_colour(), DestColour::wx:wx_colour(),
	Option :: {'nDirection', wx:wx_enum()}.
gradientFillLinear(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect,InitialColour,DestColour, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),?is_colordata(InitialColour),?is_colordata(DestColour),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({nDirection, _nDirection} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Rect,wxe_util:color(InitialColour),wxe_util:color(DestColour), Opts,?get_env(),?wxDC_GradientFillLinear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdclogicaltodevicex">external documentation</a>.
-doc """
Converts logical X coordinate to device coordinate, using the current mapping
mode, user scale factor, device origin and axis orientation.
""".
-spec logicalToDeviceX(This, X) -> integer() when
	This::wxDC(), X::integer().
logicalToDeviceX(#wx_ref{type=ThisT}=This,X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,X,?get_env(),?wxDC_LogicalToDeviceX),
  wxe_util:rec(?wxDC_LogicalToDeviceX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdclogicaltodevicexrel">external documentation</a>.
-doc """
Converts logical X coordinate to relative device coordinate, using the current
mapping mode and user scale factor but ignoring the axis orientation.

Use this for converting a width, for example.
""".
-spec logicalToDeviceXRel(This, X) -> integer() when
	This::wxDC(), X::integer().
logicalToDeviceXRel(#wx_ref{type=ThisT}=This,X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,X,?get_env(),?wxDC_LogicalToDeviceXRel),
  wxe_util:rec(?wxDC_LogicalToDeviceXRel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdclogicaltodevicey">external documentation</a>.
-doc """
Converts logical Y coordinate to device coordinate, using the current mapping
mode, user scale factor, device origin and axis orientation.
""".
-spec logicalToDeviceY(This, Y) -> integer() when
	This::wxDC(), Y::integer().
logicalToDeviceY(#wx_ref{type=ThisT}=This,Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Y,?get_env(),?wxDC_LogicalToDeviceY),
  wxe_util:rec(?wxDC_LogicalToDeviceY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdclogicaltodeviceyrel">external documentation</a>.
-doc """
Converts logical Y coordinate to relative device coordinate, using the current
mapping mode and user scale factor but ignoring the axis orientation.

Use this for converting a height, for example.
""".
-spec logicalToDeviceYRel(This, Y) -> integer() when
	This::wxDC(), Y::integer().
logicalToDeviceYRel(#wx_ref{type=ThisT}=This,Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Y,?get_env(),?wxDC_LogicalToDeviceYRel),
  wxe_util:rec(?wxDC_LogicalToDeviceYRel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcmaxx">external documentation</a>.
-doc "Gets the maximum horizontal extent used in drawing commands so far.".
-spec maxX(This) -> integer() when
	This::wxDC().
maxX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_MaxX),
  wxe_util:rec(?wxDC_MaxX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcmaxy">external documentation</a>.
-doc "Gets the maximum vertical extent used in drawing commands so far.".
-spec maxY(This) -> integer() when
	This::wxDC().
maxY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_MaxY),
  wxe_util:rec(?wxDC_MaxY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcminx">external documentation</a>.
-doc "Gets the minimum horizontal extent used in drawing commands so far.".
-spec minX(This) -> integer() when
	This::wxDC().
minX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_MinX),
  wxe_util:rec(?wxDC_MinX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcminy">external documentation</a>.
-doc "Gets the minimum vertical extent used in drawing commands so far.".
-spec minY(This) -> integer() when
	This::wxDC().
minY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_MinY),
  wxe_util:rec(?wxDC_MinY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcisok">external documentation</a>.
-doc "Returns true if the DC is ok to use.".
-spec isOk(This) -> boolean() when
	This::wxDC().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_IsOk),
  wxe_util:rec(?wxDC_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcresetboundingbox">external documentation</a>.
-doc """
Resets the bounding box: after a call to this function, the bounding box doesn't
contain anything.

See: `calcBoundingBox/3`
""".
-spec resetBoundingBox(This) -> 'ok' when
	This::wxDC().
resetBoundingBox(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_ResetBoundingBox).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetaxisorientation">external documentation</a>.
-doc """
Sets the x and y axis orientation (i.e. the direction from lowest to highest
values on the axis).

The default orientation is x axis from left to right and y axis from top down.
""".
-spec setAxisOrientation(This, XLeftRight, YBottomUp) -> 'ok' when
	This::wxDC(), XLeftRight::boolean(), YBottomUp::boolean().
setAxisOrientation(#wx_ref{type=ThisT}=This,XLeftRight,YBottomUp)
 when is_boolean(XLeftRight),is_boolean(YBottomUp) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,XLeftRight,YBottomUp,?get_env(),?wxDC_SetAxisOrientation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetbackground">external documentation</a>.
-doc "Sets the current background brush for the DC.".
-spec setBackground(This, Brush) -> 'ok' when
	This::wxDC(), Brush::wxBrush:wxBrush().
setBackground(#wx_ref{type=ThisT}=This,#wx_ref{type=BrushT}=Brush) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BrushT,wxBrush),
  wxe_util:queue_cmd(This,Brush,?get_env(),?wxDC_SetBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetbackgroundmode">external documentation</a>.
-doc """
`mode` may be one of `wxPENSTYLE_SOLID` and `wxPENSTYLE_TRANSPARENT`.

This setting determines whether text will be drawn with a background colour or
not.
""".
-spec setBackgroundMode(This, Mode) -> 'ok' when
	This::wxDC(), Mode::integer().
setBackgroundMode(#wx_ref{type=ThisT}=This,Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Mode,?get_env(),?wxDC_SetBackgroundMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetbrush">external documentation</a>.
-doc """
Sets the current brush for the DC.

If the argument is ?wxNullBrush (or another invalid brush; see
`wxBrush:isOk/1`), the current brush is selected out of the device context
(leaving `m:wxDC` without any valid brush), allowing the current brush to be
destroyed safely.

See: `m:wxBrush`, `m:wxMemoryDC`, (for the interpretation of colours when
drawing into a monochrome bitmap)
""".
-spec setBrush(This, Brush) -> 'ok' when
	This::wxDC(), Brush::wxBrush:wxBrush().
setBrush(#wx_ref{type=ThisT}=This,#wx_ref{type=BrushT}=Brush) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BrushT,wxBrush),
  wxe_util:queue_cmd(This,Brush,?get_env(),?wxDC_SetBrush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetclippingregion">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec setClippingRegion(This, Rect) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
setClippingRegion(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxDC_SetClippingRegion_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetclippingregion">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec setClippingRegion(This, Pt, Sz) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}.
setClippingRegion(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt,{SzW,SzH} = Sz)
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Pt,Sz,?get_env(),?wxDC_SetClippingRegion_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetdeviceorigin">external documentation</a>.
-doc """
Sets the device origin (i.e. the origin in pixels after scaling has been
applied).

This function may be useful in Windows printing operations for placing a graphic
on a page.
""".
-spec setDeviceOrigin(This, X, Y) -> 'ok' when
	This::wxDC(), X::integer(), Y::integer().
setDeviceOrigin(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxDC_SetDeviceOrigin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetfont">external documentation</a>.
-doc """
Sets the current font for the DC.

If the argument is ?wxNullFont (or another invalid font; see `wxFont:isOk/1`),
the current font is selected out of the device context (leaving `m:wxDC` without
any valid font), allowing the current font to be destroyed safely.

See: `m:wxFont`
""".
-spec setFont(This, Font) -> 'ok' when
	This::wxDC(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxDC_SetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetlayoutdirection">external documentation</a>.
%%<br /> Dir = ?wxLayout_Default | ?wxLayout_LeftToRight | ?wxLayout_RightToLeft
-doc """
Sets the current layout direction for the device context.

See: `getLayoutDirection/1`
""".
-spec setLayoutDirection(This, Dir) -> 'ok' when
	This::wxDC(), Dir::wx:wx_enum().
setLayoutDirection(#wx_ref{type=ThisT}=This,Dir)
 when is_integer(Dir) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Dir,?get_env(),?wxDC_SetLayoutDirection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetlogicalfunction">external documentation</a>.
%%<br /> Function = ?wxCLEAR | ?wxXOR | ?wxINVERT | ?wxOR_REVERSE | ?wxAND_REVERSE | ?wxCOPY | ?wxAND | ?wxAND_INVERT | ?wxNO_OP | ?wxNOR | ?wxEQUIV | ?wxSRC_INVERT | ?wxOR_INVERT | ?wxNAND | ?wxOR | ?wxSET
-doc """
Sets the current logical function for the device context.

Note: This function is not fully supported in all ports, due to the limitations
of the underlying drawing model. Notably, `wxINVERT` which was commonly used for
drawing rubber bands or other moving outlines in the past, is not, and will not,
be supported by wxGTK3 and wxMac. The suggested alternative is to draw
temporarily objects normally and refresh the (affected part of the) window to
remove them later.

It determines how a `source` pixel (from a pen or brush colour, or source device
context if using `blit/6`) combines with a `destination` pixel in the current
device context. Text drawing is not affected by this function.

See ?wxRasterOperationMode enumeration values for more info.

The default is `wxCOPY`, which simply draws with the current colour. The others
combine the current colour and the background using a logical operation.
""".
-spec setLogicalFunction(This, Function) -> 'ok' when
	This::wxDC(), Function::wx:wx_enum().
setLogicalFunction(#wx_ref{type=ThisT}=This,Function)
 when is_integer(Function) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Function,?get_env(),?wxDC_SetLogicalFunction).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetmapmode">external documentation</a>.
%%<br /> Mode = ?wxMM_TEXT | ?wxMM_METRIC | ?wxMM_LOMETRIC | ?wxMM_TWIPS | ?wxMM_POINTS
-doc """
The mapping mode of the device context defines the unit of measurement used to
convert `logical` units to `device` units.

Note that in X, text drawing isn't handled consistently with the mapping mode; a
font is always specified in point size. However, setting the user scale (see
`setUserScale/3`) scales the text appropriately. In Windows, scalable TrueType
fonts are always used; in X, results depend on availability of fonts, but
usually a reasonable match is found.

The coordinate origin is always at the top left of the screen/printer.

Drawing to a Windows printer device context uses the current mapping mode, but
mapping mode is currently ignored for PostScript output.
""".
-spec setMapMode(This, Mode) -> 'ok' when
	This::wxDC(), Mode::wx:wx_enum().
setMapMode(#wx_ref{type=ThisT}=This,Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,Mode,?get_env(),?wxDC_SetMapMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetpalette">external documentation</a>.
-doc """
If this is a window DC or memory DC, assigns the given palette to the window or
bitmap associated with the DC.

If the argument is ?wxNullPalette, the current palette is selected out of the
device context, and the original palette restored.

See: `m:wxPalette`
""".
-spec setPalette(This, Palette) -> 'ok' when
	This::wxDC(), Palette::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT}=This,#wx_ref{type=PaletteT}=Palette) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:queue_cmd(This,Palette,?get_env(),?wxDC_SetPalette).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetpen">external documentation</a>.
-doc """
Sets the current pen for the DC.

If the argument is ?wxNullPen (or another invalid pen; see `wxPen:isOk/1`), the
current pen is selected out of the device context (leaving `m:wxDC` without any
valid pen), allowing the current pen to be destroyed safely.

See: `m:wxMemoryDC`, for the interpretation of colours when drawing into a
monochrome bitmap
""".
-spec setPen(This, Pen) -> 'ok' when
	This::wxDC(), Pen::wxPen:wxPen().
setPen(#wx_ref{type=ThisT}=This,#wx_ref{type=PenT}=Pen) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(PenT,wxPen),
  wxe_util:queue_cmd(This,Pen,?get_env(),?wxDC_SetPen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsettextbackground">external documentation</a>.
-doc "Sets the current text background colour for the DC.".
-spec setTextBackground(This, Colour) -> 'ok' when
	This::wxDC(), Colour::wx:wx_colour().
setTextBackground(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxDC_SetTextBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsettextforeground">external documentation</a>.
-doc """
Sets the current text foreground colour for the DC.

See: `m:wxMemoryDC`, for the interpretation of colours when drawing into a
monochrome bitmap
""".
-spec setTextForeground(This, Colour) -> 'ok' when
	This::wxDC(), Colour::wx:wx_colour().
setTextForeground(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxDC_SetTextForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetuserscale">external documentation</a>.
-doc "Sets the user scaling factor, useful for applications which require 'zooming'.".
-spec setUserScale(This, XScale, YScale) -> 'ok' when
	This::wxDC(), XScale::number(), YScale::number().
setUserScale(#wx_ref{type=ThisT}=This,XScale,YScale)
 when is_number(XScale),is_number(YScale) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,XScale,YScale,?get_env(),?wxDC_SetUserScale).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcstartdoc">external documentation</a>.
-doc """
Starts a document (only relevant when outputting to a printer).

`message` is a message to show while printing.
""".
-spec startDoc(This, Message) -> boolean() when
	This::wxDC(), Message::unicode:chardata().
startDoc(#wx_ref{type=ThisT}=This,Message)
 when ?is_chardata(Message) ->
  ?CLASS(ThisT,wxDC),
  Message_UC = unicode:characters_to_binary(Message),
  wxe_util:queue_cmd(This,Message_UC,?get_env(),?wxDC_StartDoc),
  wxe_util:rec(?wxDC_StartDoc).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcstartpage">external documentation</a>.
-doc "Starts a document page (only relevant when outputting to a printer).".
-spec startPage(This) -> 'ok' when
	This::wxDC().
startPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:queue_cmd(This,?get_env(),?wxDC_StartPage).

