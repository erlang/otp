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

-module(wxGraphicsContext).
-moduledoc """
A `m:wxGraphicsContext` instance is the object that is drawn upon.

It is created by a renderer using `wxGraphicsRenderer:createContext/2`. This can be either directly using a renderer
instance, or indirectly using the static convenience `create/1` functions of `m:wxGraphicsContext`
that always delegate the task to the default renderer.

Remark: For some renderers (like Direct2D or Cairo) processing of drawing operations may
be deferred (Direct2D render target normally builds up a batch of rendering commands but
defers processing of these commands, Cairo operates on a separate surface) so to make
drawing results visible you need to update the content of the context by calling `wxGraphicsContext::Flush()`
(not implemented in wx) or by destroying the context.

See:
* `wxGraphicsRenderer:createContext/2`

* `m:wxGCDC`

* `m:wxDC`

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsContext](https://docs.wxwidgets.org/3.2/classwx_graphics_context.html)
""".
-include("wxe.hrl").
-export([clip/2,clip/5,concatTransform/2,create/0,create/1,createBrush/2,createFont/2,
  createFont/3,createFont/4,createLinearGradientBrush/6,createLinearGradientBrush/7,
  createMatrix/1,createMatrix/2,createPath/1,createPen/2,createRadialGradientBrush/7,
  createRadialGradientBrush/8,destroy/1,drawBitmap/6,drawEllipse/5,
  drawIcon/6,drawLines/2,drawLines/3,drawPath/2,drawPath/3,drawRectangle/5,
  drawRoundedRectangle/6,drawText/4,drawText/5,drawText/6,fillPath/2,
  fillPath/3,getPartialTextExtents/2,getTextExtent/2,getTransform/1,
  resetClip/1,rotate/2,scale/3,setBrush/2,setFont/2,setFont/3,setPen/2,
  setTransform/2,strokeLine/5,strokeLines/2,strokePath/2,translate/3]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

-type wxGraphicsContext() :: wx:wx_object().
-export_type([wxGraphicsContext/0]).
-doc false.
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Create a lightweight context that can be used only for measuring text.".
-spec create() -> wxGraphicsContext().
create() ->
  wxe_util:queue_cmd(?get_env(), ?wxGraphicsContext_Create_STAT_0),
  wxe_util:rec(?wxGraphicsContext_Create_STAT_0).

-doc """
Creates a `m:wxGraphicsContext` from a `m:wxWindowDC`.

See: `wxGraphicsRenderer:createContext/2`
""".
-spec create(WindowDC) -> wxGraphicsContext() when
	WindowDC::wxWindowDC:wxWindowDC() | wxWindow:wxWindow() | wxMemoryDC:wxMemoryDC() | wxImage:wxImage().
create(#wx_ref{type=WindowDCT}=WindowDC) ->
  IswxWindowDC = ?CLASS_T(WindowDCT,wxWindowDC),
  IswxWindow = ?CLASS_T(WindowDCT,wxWindow),
  IswxMemoryDC = ?CLASS_T(WindowDCT,wxMemoryDC),
  IswxImage = ?CLASS_T(WindowDCT,wxImage),
  WindowDCType = if
    IswxWindowDC ->   wxWindowDC;
    IswxWindow ->   wxWindow;
    IswxMemoryDC ->   wxMemoryDC;
    IswxImage ->   wxImage;
    true -> error({badarg, WindowDCT})
  end,
  wxe_util:queue_cmd(wx:typeCast(WindowDC, WindowDCType),?get_env(),?wxGraphicsContext_Create_STAT_1),
  wxe_util:rec(?wxGraphicsContext_Create_STAT_1).

-doc """
Creates a native pen from a `m:wxPen`.

Prefer to use the overload taking `wxGraphicsPenInfo` (not implemented in wx) unless you
already have a `m:wxPen` as constructing one only to pass it to this method is wasteful.
""".
-spec createPen(This, Pen) -> wxGraphicsPen:wxGraphicsPen() when
	This::wxGraphicsContext(), Pen::wxPen:wxPen().
createPen(#wx_ref{type=ThisT}=This,#wx_ref{type=PenT}=Pen) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PenT,wxPen),
  wxe_util:queue_cmd(This,Pen,?get_env(),?wxGraphicsContext_CreatePen),
  wxe_util:rec(?wxGraphicsContext_CreatePen).

-doc "Creates a native brush from a `m:wxBrush`.".
-spec createBrush(This, Brush) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), Brush::wxBrush:wxBrush().
createBrush(#wx_ref{type=ThisT}=This,#wx_ref{type=BrushT}=Brush) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(BrushT,wxBrush),
  wxe_util:queue_cmd(This,Brush,?get_env(),?wxGraphicsContext_CreateBrush),
  wxe_util:rec(?wxGraphicsContext_CreateBrush).

-doc """
This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.
""".
-spec createRadialGradientBrush(This, StartX, StartY, EndX, EndY, Radius, Stops) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), StartX::number(), StartY::number(), EndX::number(), EndY::number(), Radius::number(), Stops::wxGraphicsGradientStops:wxGraphicsGradientStops().
createRadialGradientBrush(#wx_ref{type=ThisT}=This,StartX,StartY,EndX,EndY,Radius,#wx_ref{type=StopsT}=Stops)
 when is_number(StartX),is_number(StartY),is_number(EndX),is_number(EndY),is_number(Radius) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(StopsT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,StartX,StartY,EndX,EndY,Radius,Stops,?get_env(),?wxGraphicsContext_CreateRadialGradientBrush_6),
  wxe_util:rec(?wxGraphicsContext_CreateRadialGradientBrush_6).

-doc """
` Creates a native brush with a radial gradient. The brush originates at (@a startX, @a
startY) and ends on a circle around (@a endX, @a endY) with the given @a radius. The
gradient may be specified either by its start and end colours @a oColor and @a cColor or
by a full set of gradient @a stops. The version taking wxGraphicsGradientStops is new in
wxWidgets 2.9.1. `
The ability to apply a transformation matrix to the gradient was added in 3.1.3
""".
-spec createRadialGradientBrush(This, StartX, StartY, EndX, EndY, Radius, OColor, CColor) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), StartX::number(), StartY::number(), EndX::number(), EndY::number(), Radius::number(), OColor::wx:wx_colour(), CColor::wx:wx_colour().
createRadialGradientBrush(#wx_ref{type=ThisT}=This,StartX,StartY,EndX,EndY,Radius,OColor,CColor)
 when is_number(StartX),is_number(StartY),is_number(EndX),is_number(EndY),is_number(Radius),?is_colordata(OColor),?is_colordata(CColor) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,StartX,StartY,EndX,EndY,Radius,wxe_util:color(OColor),wxe_util:color(CColor),?get_env(),?wxGraphicsContext_CreateRadialGradientBrush_7),
  wxe_util:rec(?wxGraphicsContext_CreateRadialGradientBrush_7).

-doc """
This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.
""".
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, Stops) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), X1::number(), Y1::number(), X2::number(), Y2::number(), Stops::wxGraphicsGradientStops:wxGraphicsGradientStops().
createLinearGradientBrush(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2,#wx_ref{type=StopsT}=Stops)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(StopsT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,Stops,?get_env(),?wxGraphicsContext_CreateLinearGradientBrush_5),
  wxe_util:rec(?wxGraphicsContext_CreateLinearGradientBrush_5).

-doc """
` Creates a native brush with a linear gradient. The brush starts at (@a x1, @a y1) and
ends at (@a x2, @a y2). Either just the start and end gradient colours (@a c1 and @a c2)
or full set of gradient @a stops can be specified. The version taking
wxGraphicsGradientStops is new in wxWidgets 2.9.1. `
The `matrix` parameter was added in wxWidgets 3.1.3
""".
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, C1, C2) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), X1::number(), Y1::number(), X2::number(), Y2::number(), C1::wx:wx_colour(), C2::wx:wx_colour().
createLinearGradientBrush(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2,C1,C2)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2),?is_colordata(C1),?is_colordata(C2) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,wxe_util:color(C1),wxe_util:color(C2),?get_env(),?wxGraphicsContext_CreateLinearGradientBrush_6),
  wxe_util:rec(?wxGraphicsContext_CreateLinearGradientBrush_6).

-doc(#{equiv => createFont(This,Font, [])}).
-spec createFont(This, Font) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsContext(), Font::wxFont:wxFont().

createFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  createFont(This,Font, []).

-doc """
Creates a native graphics font from a `m:wxFont` and a text colour.

Remark: For Direct2D graphics fonts can be created from TrueType fonts only.
""".
-spec createFont(This, SizeInPixels, Facename) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsContext(), SizeInPixels::number(), Facename::unicode:chardata();
      (This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsContext(), Font::wxFont:wxFont(),
	Option :: {'col', wx:wx_colour()}.

createFont(This,SizeInPixels,Facename)
 when is_record(This, wx_ref),is_number(SizeInPixels),?is_chardata(Facename) ->
  createFont(This,SizeInPixels,Facename, []);
createFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxFont),
  MOpts = fun({col, Col}) -> {col,wxe_util:color(Col)};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Font, Opts,?get_env(),?wxGraphicsContext_CreateFont_2),
  wxe_util:rec(?wxGraphicsContext_CreateFont_2).

-doc """
Creates a font object with the specified attributes.

The use of overload taking `m:wxFont` is preferred, see `wxGraphicsRenderer:createFont/4` for more details.

Remark: For Direct2D graphics fonts can be created from TrueType fonts only.

Since: 2.9.3
""".
-spec createFont(This, SizeInPixels, Facename, [Option]) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsContext(), SizeInPixels::number(), Facename::unicode:chardata(),
	Option :: {'flags', integer()}
		 | {'col', wx:wx_colour()}.
createFont(#wx_ref{type=ThisT}=This,SizeInPixels,Facename, Options)
 when is_number(SizeInPixels),?is_chardata(Facename),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Facename_UC = unicode:characters_to_binary(Facename),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          ({col, Col}) -> {col,wxe_util:color(Col)};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,SizeInPixels,Facename_UC, Opts,?get_env(),?wxGraphicsContext_CreateFont_3),
  wxe_util:rec(?wxGraphicsContext_CreateFont_3).

-doc(#{equiv => createMatrix(This, [])}).
-spec createMatrix(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsContext().

createMatrix(This)
 when is_record(This, wx_ref) ->
  createMatrix(This, []).

-doc """
Creates a native affine transformation matrix from the passed in values.

The default parameters result in an identity matrix.
""".
-spec createMatrix(This, [Option]) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsContext(),
	Option :: {'a', number()}
		 | {'b', number()}
		 | {'c', number()}
		 | {'d', number()}
		 | {'tx', number()}
		 | {'ty', number()}.
createMatrix(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  MOpts = fun({a, _a} = Arg) -> Arg;
          ({b, _b} = Arg) -> Arg;
          ({c, _c} = Arg) -> Arg;
          ({d, _d} = Arg) -> Arg;
          ({tx, _tx} = Arg) -> Arg;
          ({ty, _ty} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGraphicsContext_CreateMatrix),
  wxe_util:rec(?wxGraphicsContext_CreateMatrix).

-doc "Creates a native graphics path which is initially empty.".
-spec createPath(This) -> wxGraphicsPath:wxGraphicsPath() when
	This::wxGraphicsContext().
createPath(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsContext_CreatePath),
  wxe_util:rec(?wxGraphicsContext_CreatePath).

-doc """
Sets the clipping region to the intersection of the given region and the previously set
clipping region.

The clipping region is an area to which drawing is restricted.

Remark:

* Clipping region should be given in logical coordinates.

* Calling this function can only make the clipping region smaller, never larger.

* You need to call `resetClip/1` first if you want to set the clipping region exactly to the region specified.

* If resulting clipping region is empty, then all drawing upon the context is clipped out
(all changes made by drawing operations are masked out).
""".
-spec clip(This, Region) -> 'ok' when
	This::wxGraphicsContext(), Region::wxRegion:wxRegion().
clip(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(RegionT,wxRegion),
  wxe_util:queue_cmd(This,Region,?get_env(),?wxGraphicsContext_Clip_1).

-doc """
This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.
""".
-spec clip(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
clip(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxGraphicsContext_Clip_4).

-doc "Resets the clipping to original shape.".
-spec resetClip(This) -> 'ok' when
	This::wxGraphicsContext().
resetClip(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsContext_ResetClip).

-doc """
Draws the bitmap.

In case of a mono bitmap, this is treated as a mask and the current brushed is used for
filling.
""".
-spec drawBitmap(This, Bmp, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), Bmp::wxBitmap:wxBitmap(), X::number(), Y::number(), W::number(), H::number().
drawBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Bmp,X,Y,W,H,?get_env(),?wxGraphicsContext_DrawBitmap).

-doc "Draws an ellipse.".
-spec drawEllipse(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
drawEllipse(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxGraphicsContext_DrawEllipse).

-doc "Draws the icon.".
-spec drawIcon(This, Icon, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), Icon::wxIcon:wxIcon(), X::number(), Y::number(), W::number(), H::number().
drawIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,X,Y,W,H,?get_env(),?wxGraphicsContext_DrawIcon).

-doc(#{equiv => drawLines(This,Points, [])}).
-spec drawLines(This, Points) -> 'ok' when
	This::wxGraphicsContext(), Points::[{X::float(), Y::float()}].

drawLines(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawLines(This,Points, []).

-doc "Draws a polygon.".
%%  FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
-spec drawLines(This, Points, [Option]) -> 'ok' when
	This::wxGraphicsContext(), Points::[{X::float(), Y::float()}],
	Option :: {'fillStyle', wx:wx_enum()}.
drawLines(#wx_ref{type=ThisT}=This,Points, Options)
 when is_list(Points),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  MOpts = fun({fillStyle, _fillStyle} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Points, Opts,?get_env(),?wxGraphicsContext_DrawLines).

-doc(#{equiv => drawPath(This,Path, [])}).
-spec drawPath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().

drawPath(This,Path)
 when is_record(This, wx_ref),is_record(Path, wx_ref) ->
  drawPath(This,Path, []).

-doc "Draws the path by first filling and then stroking.".
%%  FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
-spec drawPath(This, Path, [Option]) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath(),
	Option :: {'fillStyle', wx:wx_enum()}.
drawPath(#wx_ref{type=ThisT}=This,#wx_ref{type=PathT}=Path, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  MOpts = fun({fillStyle, _fillStyle} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Path, Opts,?get_env(),?wxGraphicsContext_DrawPath).

-doc "Draws a rectangle.".
-spec drawRectangle(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
drawRectangle(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxGraphicsContext_DrawRectangle).

-doc "Draws a rounded rectangle.".
-spec drawRoundedRectangle(This, X, Y, W, H, Radius) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number(), Radius::number().
drawRoundedRectangle(#wx_ref{type=ThisT}=This,X,Y,W,H,Radius)
 when is_number(X),is_number(Y),is_number(W),is_number(H),is_number(Radius) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X,Y,W,H,Radius,?get_env(),?wxGraphicsContext_DrawRoundedRectangle).

-doc "Draws text at the defined position.".
-spec drawText(This, Str, X, Y) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number().
drawText(#wx_ref{type=ThisT}=This,Str,X,Y)
 when ?is_chardata(Str),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary(Str),
  wxe_util:queue_cmd(This,Str_UC,X,Y,?get_env(),?wxGraphicsContext_DrawText_3).

-doc "Draws text at the defined position.".
-spec drawText(This, Str, X, Y, Angle) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), Angle::number();
      (This, Str, X, Y, BackgroundBrush) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush().
drawText(#wx_ref{type=ThisT}=This,Str,X,Y,Angle)
 when ?is_chardata(Str),is_number(X),is_number(Y),is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary(Str),
  wxe_util:queue_cmd(This,Str_UC,X,Y,Angle,?get_env(),?wxGraphicsContext_DrawText_4_0);
drawText(#wx_ref{type=ThisT}=This,Str,X,Y,#wx_ref{type=BackgroundBrushT}=BackgroundBrush)
 when ?is_chardata(Str),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary(Str),
  ?CLASS(BackgroundBrushT,wxGraphicsBrush),
  wxe_util:queue_cmd(This,Str_UC,X,Y,BackgroundBrush,?get_env(),?wxGraphicsContext_DrawText_4_1).

-doc "Draws text at the defined position.".
-spec drawText(This, Str, X, Y, Angle, BackgroundBrush) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), Angle::number(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush().
drawText(#wx_ref{type=ThisT}=This,Str,X,Y,Angle,#wx_ref{type=BackgroundBrushT}=BackgroundBrush)
 when ?is_chardata(Str),is_number(X),is_number(Y),is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary(Str),
  ?CLASS(BackgroundBrushT,wxGraphicsBrush),
  wxe_util:queue_cmd(This,Str_UC,X,Y,Angle,BackgroundBrush,?get_env(),?wxGraphicsContext_DrawText_5).

-doc(#{equiv => fillPath(This,Path, [])}).
-spec fillPath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().

fillPath(This,Path)
 when is_record(This, wx_ref),is_record(Path, wx_ref) ->
  fillPath(This,Path, []).

-doc "Fills the path with the current brush.".
%%  FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
-spec fillPath(This, Path, [Option]) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath(),
	Option :: {'fillStyle', wx:wx_enum()}.
fillPath(#wx_ref{type=ThisT}=This,#wx_ref{type=PathT}=Path, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  MOpts = fun({fillStyle, _fillStyle} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Path, Opts,?get_env(),?wxGraphicsContext_FillPath).

-doc "Strokes along a path with the current pen.".
-spec strokePath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().
strokePath(#wx_ref{type=ThisT}=This,#wx_ref{type=PathT}=Path) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  wxe_util:queue_cmd(This,Path,?get_env(),?wxGraphicsContext_StrokePath).

-doc """
Fills the `widths` array with the widths from the beginning of `text` to the
corresponding character of `text`.
""".
-spec getPartialTextExtents(This, Text) -> [number()] when
	This::wxGraphicsContext(), Text::unicode:chardata().
getPartialTextExtents(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxGraphicsContext_GetPartialTextExtents),
  wxe_util:rec(?wxGraphicsContext_GetPartialTextExtents).

-doc "Gets the dimensions of the string using the currently selected font.".
-spec getTextExtent(This, Text) -> Result when
	Result ::{Width::number(), Height::number(), Descent::number(), ExternalLeading::number()},
	This::wxGraphicsContext(), Text::unicode:chardata().
getTextExtent(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxGraphicsContext_GetTextExtent),
  wxe_util:rec(?wxGraphicsContext_GetTextExtent).

-doc "Rotates the current transformation matrix (in radians).".
-spec rotate(This, Angle) -> 'ok' when
	This::wxGraphicsContext(), Angle::number().
rotate(#wx_ref{type=ThisT}=This,Angle)
 when is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,Angle,?get_env(),?wxGraphicsContext_Rotate).

-doc "Scales the current transformation matrix.".
-spec scale(This, XScale, YScale) -> 'ok' when
	This::wxGraphicsContext(), XScale::number(), YScale::number().
scale(#wx_ref{type=ThisT}=This,XScale,YScale)
 when is_number(XScale),is_number(YScale) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,XScale,YScale,?get_env(),?wxGraphicsContext_Scale).

-doc "Translates the current transformation matrix.".
-spec translate(This, Dx, Dy) -> 'ok' when
	This::wxGraphicsContext(), Dx::number(), Dy::number().
translate(#wx_ref{type=ThisT}=This,Dx,Dy)
 when is_number(Dx),is_number(Dy) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,Dx,Dy,?get_env(),?wxGraphicsContext_Translate).

-doc "Gets the current transformation matrix of this context.".
-spec getTransform(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsContext().
getTransform(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsContext_GetTransform),
  wxe_util:rec(?wxGraphicsContext_GetTransform).

-doc "Sets the current transformation matrix of this context.".
-spec setTransform(This, Matrix) -> 'ok' when
	This::wxGraphicsContext(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix().
setTransform(#wx_ref{type=ThisT}=This,#wx_ref{type=MatrixT}=Matrix) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Matrix,?get_env(),?wxGraphicsContext_SetTransform).

-doc "Concatenates the passed in transform with the current transform of this context.".
-spec concatTransform(This, Matrix) -> 'ok' when
	This::wxGraphicsContext(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix().
concatTransform(#wx_ref{type=ThisT}=This,#wx_ref{type=MatrixT}=Matrix) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Matrix,?get_env(),?wxGraphicsContext_ConcatTransform).

-doc "Sets the brush for filling paths.".
-spec setBrush(This, Brush) -> 'ok' when
	This::wxGraphicsContext(), Brush::wxGraphicsBrush:wxGraphicsBrush() | wxBrush:wxBrush().
setBrush(#wx_ref{type=ThisT}=This,#wx_ref{type=BrushT}=Brush) ->
  ?CLASS(ThisT,wxGraphicsContext),
  IswxGraphicsBrush = ?CLASS_T(BrushT,wxGraphicsBrush),
  IswxBrush = ?CLASS_T(BrushT,wxBrush),
  BrushType = if
    IswxGraphicsBrush ->   wxGraphicsBrush;
    IswxBrush ->   wxBrush;
    true -> error({badarg, BrushT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Brush, BrushType),?get_env(),?wxGraphicsContext_SetBrush).

-doc "Sets the font for drawing text.".
-spec setFont(This, Font) -> 'ok' when
	This::wxGraphicsContext(), Font::wxGraphicsFont:wxGraphicsFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxGraphicsFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxGraphicsContext_SetFont_1).

-doc """
Sets the font for drawing text.

Remark: For Direct2D only TrueType fonts can be used.
""".
-spec setFont(This, Font, Colour) -> 'ok' when
	This::wxGraphicsContext(), Font::wxFont:wxFont(), Colour::wx:wx_colour().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,wxe_util:color(Colour),?get_env(),?wxGraphicsContext_SetFont_2).

-doc "Sets the pen used for stroking.".
-spec setPen(This, Pen) -> 'ok' when
	This::wxGraphicsContext(), Pen::wxPen:wxPen() | wxGraphicsPen:wxGraphicsPen().
setPen(#wx_ref{type=ThisT}=This,#wx_ref{type=PenT}=Pen) ->
  ?CLASS(ThisT,wxGraphicsContext),
  IswxPen = ?CLASS_T(PenT,wxPen),
  IswxGraphicsPen = ?CLASS_T(PenT,wxGraphicsPen),
  PenType = if
    IswxPen ->   wxPen;
    IswxGraphicsPen ->   wxGraphicsPen;
    true -> error({badarg, PenT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Pen, PenType),?get_env(),?wxGraphicsContext_SetPen).

-doc "Strokes a single line.".
-spec strokeLine(This, X1, Y1, X2, Y2) -> 'ok' when
	This::wxGraphicsContext(), X1::number(), Y1::number(), X2::number(), Y2::number().
strokeLine(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,?get_env(),?wxGraphicsContext_StrokeLine).

-doc """
Stroke lines connecting all the points.

Unlike the other overload of this function, this method draws a single polyline and not a
number of disconnected lines.
""".
-spec strokeLines(This, Points) -> 'ok' when
	This::wxGraphicsContext(), Points::[{X::float(), Y::float()}].
strokeLines(#wx_ref{type=ThisT}=This,Points)
 when is_list(Points) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,Points,?get_env(),?wxGraphicsContext_StrokeLines).

-doc "Destroys the object".
-spec destroy(This::wxGraphicsContext()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGraphicsContext),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxGraphicsObject
-doc false.
isNull(This) -> wxGraphicsObject:isNull(This).
-doc false.
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
