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

-module(wxBufferedPaintDC).
-moduledoc """
This is a subclass of `m:wxBufferedDC` which can be used inside of an `EVT\_PAINT()`
event handler to achieve double-buffered drawing.

Just use this class instead of `m:wxPaintDC` and make sure `wxWindow:setBackgroundStyle/2` is called with
wxBG_STYLE_PAINT somewhere in the class initialization code, and that's all you have to do
to (mostly) avoid flicker. The only thing to watch out for is that if you are using this
class together with `wxScrolled` (not implemented in wx), you probably do `not` want to
call `wxScrolledWindow:prepareDC/2` on it as it already does this internally for the real underlying `m:wxPaintDC`.

See:
* `m:wxDC`

* `m:wxBufferedDC`

* `m:wxPaintDC`

This class is derived, and can use functions, from:

* `m:wxBufferedDC`

* `m:wxMemoryDC`

* `m:wxDC`

wxWidgets docs: [wxBufferedPaintDC](https://docs.wxwidgets.org/3.2/classwx_buffered_paint_d_c.html)
""".
-include("wxe.hrl").
-export([destroy/1,new/1,new/2,new/3]).

%% inherited exports
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
  gradientFillLinear/4,gradientFillLinear/5,init/2,init/3,init/4,isOk/1,
  logicalToDeviceX/2,logicalToDeviceXRel/2,logicalToDeviceY/2,logicalToDeviceYRel/2,
  maxX/1,maxY/1,minX/1,minY/1,parent_class/1,resetBoundingBox/1,selectObject/2,
  selectObjectAsSource/2,setAxisOrientation/3,setBackground/2,setBackgroundMode/2,
  setBrush/2,setClippingRegion/2,setClippingRegion/3,setDeviceOrigin/3,
  setFont/2,setLayoutDirection/2,setLogicalFunction/2,setMapMode/2,
  setPalette/2,setPen/2,setTextBackground/2,setTextForeground/2,setUserScale/3,
  startDoc/2,startPage/1]).

-type wxBufferedPaintDC() :: wx:wx_object().
-export_type([wxBufferedPaintDC/0]).
-doc false.
parent_class(wxBufferedDC) -> true;
parent_class(wxMemoryDC) -> true;
parent_class(wxDC) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new(Window, [])}).
-spec new(Window) -> wxBufferedPaintDC() when
	Window::wxWindow:wxWindow().

new(Window)
 when is_record(Window, wx_ref) ->
  new(Window, []).

-doc "".
-spec new(Window, Buffer) -> wxBufferedPaintDC() when
	Window::wxWindow:wxWindow(), Buffer::wxBitmap:wxBitmap();
      (Window, [Option]) -> wxBufferedPaintDC() when
	Window::wxWindow:wxWindow(),
	Option :: {'style', integer()}.

new(Window,Buffer)
 when is_record(Window, wx_ref),is_record(Buffer, wx_ref) ->
  new(Window,Buffer, []);
new(#wx_ref{type=WindowT}=Window, Options)
 when is_list(Options) ->
  ?CLASS(WindowT,wxWindow),
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Window, Opts,?get_env(),?wxBufferedPaintDC_new_2),
  wxe_util:rec(?wxBufferedPaintDC_new_2).

-doc """
As with `m:wxBufferedDC`, you may either provide the bitmap to be used for buffering or
let this object create one internally (in the latter case, the size of the client part of
the window is used).

Pass wxBUFFER_CLIENT_AREA for the `style` parameter to indicate that just the client area
of the window is buffered, or wxBUFFER_VIRTUAL_AREA to indicate that the buffer bitmap
covers the virtual area.
""".
-spec new(Window, Buffer, [Option]) -> wxBufferedPaintDC() when
	Window::wxWindow:wxWindow(), Buffer::wxBitmap:wxBitmap(),
	Option :: {'style', integer()}.
new(#wx_ref{type=WindowT}=Window,#wx_ref{type=BufferT}=Buffer, Options)
 when is_list(Options) ->
  ?CLASS(WindowT,wxWindow),
  ?CLASS(BufferT,wxBitmap),
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Window,Buffer, Opts,?get_env(),?wxBufferedPaintDC_new_3),
  wxe_util:rec(?wxBufferedPaintDC_new_3).

-doc "Destroys the object".
-spec destroy(This::wxBufferedPaintDC()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBufferedPaintDC),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxBufferedDC
-doc false.
init(This,Dc,Area, Options) -> wxBufferedDC:init(This,Dc,Area, Options).
-doc false.
init(This,Dc,Area) -> wxBufferedDC:init(This,Dc,Area).
-doc false.
init(This,Dc) -> wxBufferedDC:init(This,Dc).
 %% From wxMemoryDC
-doc false.
selectObjectAsSource(This,Bitmap) -> wxMemoryDC:selectObjectAsSource(This,Bitmap).
-doc false.
selectObject(This,Bitmap) -> wxMemoryDC:selectObject(This,Bitmap).
 %% From wxDC
-doc false.
startPage(This) -> wxDC:startPage(This).
-doc false.
startDoc(This,Message) -> wxDC:startDoc(This,Message).
-doc false.
setUserScale(This,XScale,YScale) -> wxDC:setUserScale(This,XScale,YScale).
-doc false.
setTextForeground(This,Colour) -> wxDC:setTextForeground(This,Colour).
-doc false.
setTextBackground(This,Colour) -> wxDC:setTextBackground(This,Colour).
-doc false.
setPen(This,Pen) -> wxDC:setPen(This,Pen).
-doc false.
setPalette(This,Palette) -> wxDC:setPalette(This,Palette).
-doc false.
setMapMode(This,Mode) -> wxDC:setMapMode(This,Mode).
-doc false.
setLogicalFunction(This,Function) -> wxDC:setLogicalFunction(This,Function).
-doc false.
setLayoutDirection(This,Dir) -> wxDC:setLayoutDirection(This,Dir).
-doc false.
setFont(This,Font) -> wxDC:setFont(This,Font).
-doc false.
setDeviceOrigin(This,X,Y) -> wxDC:setDeviceOrigin(This,X,Y).
-doc false.
setClippingRegion(This,Pt,Sz) -> wxDC:setClippingRegion(This,Pt,Sz).
-doc false.
setClippingRegion(This,Rect) -> wxDC:setClippingRegion(This,Rect).
-doc false.
setBrush(This,Brush) -> wxDC:setBrush(This,Brush).
-doc false.
setBackgroundMode(This,Mode) -> wxDC:setBackgroundMode(This,Mode).
-doc false.
setBackground(This,Brush) -> wxDC:setBackground(This,Brush).
-doc false.
setAxisOrientation(This,XLeftRight,YBottomUp) -> wxDC:setAxisOrientation(This,XLeftRight,YBottomUp).
-doc false.
resetBoundingBox(This) -> wxDC:resetBoundingBox(This).
-doc false.
isOk(This) -> wxDC:isOk(This).
-doc false.
minY(This) -> wxDC:minY(This).
-doc false.
minX(This) -> wxDC:minX(This).
-doc false.
maxY(This) -> wxDC:maxY(This).
-doc false.
maxX(This) -> wxDC:maxX(This).
-doc false.
logicalToDeviceYRel(This,Y) -> wxDC:logicalToDeviceYRel(This,Y).
-doc false.
logicalToDeviceY(This,Y) -> wxDC:logicalToDeviceY(This,Y).
-doc false.
logicalToDeviceXRel(This,X) -> wxDC:logicalToDeviceXRel(This,X).
-doc false.
logicalToDeviceX(This,X) -> wxDC:logicalToDeviceX(This,X).
-doc false.
gradientFillLinear(This,Rect,InitialColour,DestColour, Options) -> wxDC:gradientFillLinear(This,Rect,InitialColour,DestColour, Options).
-doc false.
gradientFillLinear(This,Rect,InitialColour,DestColour) -> wxDC:gradientFillLinear(This,Rect,InitialColour,DestColour).
-doc false.
gradientFillConcentric(This,Rect,InitialColour,DestColour,CircleCenter) -> wxDC:gradientFillConcentric(This,Rect,InitialColour,DestColour,CircleCenter).
-doc false.
gradientFillConcentric(This,Rect,InitialColour,DestColour) -> wxDC:gradientFillConcentric(This,Rect,InitialColour,DestColour).
-doc false.
getUserScale(This) -> wxDC:getUserScale(This).
-doc false.
getTextForeground(This) -> wxDC:getTextForeground(This).
-doc false.
getTextExtent(This,String, Options) -> wxDC:getTextExtent(This,String, Options).
-doc false.
getTextExtent(This,String) -> wxDC:getTextExtent(This,String).
-doc false.
getTextBackground(This) -> wxDC:getTextBackground(This).
-doc false.
getSizeMM(This) -> wxDC:getSizeMM(This).
-doc false.
getSize(This) -> wxDC:getSize(This).
-doc false.
getPPI(This) -> wxDC:getPPI(This).
-doc false.
getPixel(This,Pos) -> wxDC:getPixel(This,Pos).
-doc false.
getPen(This) -> wxDC:getPen(This).
-doc false.
getPartialTextExtents(This,Text) -> wxDC:getPartialTextExtents(This,Text).
-doc false.
getMultiLineTextExtent(This,String, Options) -> wxDC:getMultiLineTextExtent(This,String, Options).
-doc false.
getMultiLineTextExtent(This,String) -> wxDC:getMultiLineTextExtent(This,String).
-doc false.
getMapMode(This) -> wxDC:getMapMode(This).
-doc false.
getLogicalFunction(This) -> wxDC:getLogicalFunction(This).
-doc false.
getLayoutDirection(This) -> wxDC:getLayoutDirection(This).
-doc false.
getFont(This) -> wxDC:getFont(This).
-doc false.
getClippingBox(This) -> wxDC:getClippingBox(This).
-doc false.
getCharWidth(This) -> wxDC:getCharWidth(This).
-doc false.
getCharHeight(This) -> wxDC:getCharHeight(This).
-doc false.
getBrush(This) -> wxDC:getBrush(This).
-doc false.
getBackgroundMode(This) -> wxDC:getBackgroundMode(This).
-doc false.
getBackground(This) -> wxDC:getBackground(This).
-doc false.
floodFill(This,Pt,Col, Options) -> wxDC:floodFill(This,Pt,Col, Options).
-doc false.
floodFill(This,Pt,Col) -> wxDC:floodFill(This,Pt,Col).
-doc false.
endPage(This) -> wxDC:endPage(This).
-doc false.
endDoc(This) -> wxDC:endDoc(This).
-doc false.
drawText(This,Text,Pt) -> wxDC:drawText(This,Text,Pt).
-doc false.
drawRoundedRectangle(This,Pt,Sz,Radius) -> wxDC:drawRoundedRectangle(This,Pt,Sz,Radius).
-doc false.
drawRoundedRectangle(This,Rect,Radius) -> wxDC:drawRoundedRectangle(This,Rect,Radius).
-doc false.
drawRotatedText(This,Text,Point,Angle) -> wxDC:drawRotatedText(This,Text,Point,Angle).
-doc false.
drawRectangle(This,Pt,Sz) -> wxDC:drawRectangle(This,Pt,Sz).
-doc false.
drawRectangle(This,Rect) -> wxDC:drawRectangle(This,Rect).
-doc false.
drawPoint(This,Pt) -> wxDC:drawPoint(This,Pt).
-doc false.
drawPolygon(This,Points, Options) -> wxDC:drawPolygon(This,Points, Options).
-doc false.
drawPolygon(This,Points) -> wxDC:drawPolygon(This,Points).
-doc false.
drawLines(This,Points, Options) -> wxDC:drawLines(This,Points, Options).
-doc false.
drawLines(This,Points) -> wxDC:drawLines(This,Points).
-doc false.
drawLine(This,Pt1,Pt2) -> wxDC:drawLine(This,Pt1,Pt2).
-doc false.
drawLabel(This,Text,Rect, Options) -> wxDC:drawLabel(This,Text,Rect, Options).
-doc false.
drawLabel(This,Text,Rect) -> wxDC:drawLabel(This,Text,Rect).
-doc false.
drawIcon(This,Icon,Pt) -> wxDC:drawIcon(This,Icon,Pt).
-doc false.
drawEllipticArc(This,Pt,Sz,Sa,Ea) -> wxDC:drawEllipticArc(This,Pt,Sz,Sa,Ea).
-doc false.
drawEllipse(This,Pt,Size) -> wxDC:drawEllipse(This,Pt,Size).
-doc false.
drawEllipse(This,Rect) -> wxDC:drawEllipse(This,Rect).
-doc false.
drawCircle(This,Pt,Radius) -> wxDC:drawCircle(This,Pt,Radius).
-doc false.
drawCheckMark(This,Rect) -> wxDC:drawCheckMark(This,Rect).
-doc false.
drawBitmap(This,Bmp,Pt, Options) -> wxDC:drawBitmap(This,Bmp,Pt, Options).
-doc false.
drawBitmap(This,Bmp,Pt) -> wxDC:drawBitmap(This,Bmp,Pt).
-doc false.
drawArc(This,PtStart,PtEnd,Centre) -> wxDC:drawArc(This,PtStart,PtEnd,Centre).
-doc false.
deviceToLogicalYRel(This,Y) -> wxDC:deviceToLogicalYRel(This,Y).
-doc false.
deviceToLogicalY(This,Y) -> wxDC:deviceToLogicalY(This,Y).
-doc false.
deviceToLogicalXRel(This,X) -> wxDC:deviceToLogicalXRel(This,X).
-doc false.
deviceToLogicalX(This,X) -> wxDC:deviceToLogicalX(This,X).
-doc false.
destroyClippingRegion(This) -> wxDC:destroyClippingRegion(This).
-doc false.
crossHair(This,Pt) -> wxDC:crossHair(This,Pt).
-doc false.
clear(This) -> wxDC:clear(This).
-doc false.
calcBoundingBox(This,X,Y) -> wxDC:calcBoundingBox(This,X,Y).
-doc false.
blit(This,Dest,Size,Source,Src, Options) -> wxDC:blit(This,Dest,Size,Source,Src, Options).
-doc false.
blit(This,Dest,Size,Source,Src) -> wxDC:blit(This,Dest,Size,Source,Src).
