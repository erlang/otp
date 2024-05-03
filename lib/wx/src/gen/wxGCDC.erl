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

-module(wxGCDC).
-moduledoc """
Functions for wxGCDC class

`m:wxGCDC` is a device context that draws on a `m:wxGraphicsContext`.

`m:wxGCDC` does its best to implement `m:wxDC` API, but the following features
are not (fully) implemented because `m:wxGraphicsContext` doesn't support them:

See: `m:wxDC`, `m:wxGraphicsContext`

This class is derived (and can use functions) from: `m:wxDC`

wxWidgets docs: [wxGCDC](https://docs.wxwidgets.org/3.1/classwx_g_c_d_c.html)
""".
-include("wxe.hrl").
-export([destroy/1,getGraphicsContext/1,new/0,new/1,setGraphicsContext/2]).

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
  gradientFillLinear/4,gradientFillLinear/5,isOk/1,logicalToDeviceX/2,
  logicalToDeviceXRel/2,logicalToDeviceY/2,logicalToDeviceYRel/2,maxX/1,
  maxY/1,minX/1,minY/1,parent_class/1,resetBoundingBox/1,setAxisOrientation/3,
  setBackground/2,setBackgroundMode/2,setBrush/2,setClippingRegion/2,
  setClippingRegion/3,setDeviceOrigin/3,setFont/2,setLayoutDirection/2,
  setLogicalFunction/2,setMapMode/2,setPalette/2,setPen/2,setTextBackground/2,
  setTextForeground/2,setUserScale/3,startDoc/2,startPage/1]).

-type wxGCDC() :: wx:wx_object().
-export_type([wxGCDC/0]).
%% @hidden
-doc false.
parent_class(wxDC) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgcdc.html#wxgcdcwxgcdc">external documentation</a>.
-spec new() -> wxGCDC().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxGCDC_new_0),
  wxe_util:rec(?wxGCDC_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgcdc.html#wxgcdcwxgcdc">external documentation</a>.
-doc "Constructs a `m:wxGCDC` from a `m:wxWindowDC`.".
-spec new(WindowDC) -> wxGCDC() when
	WindowDC::wxWindowDC:wxWindowDC() | wxMemoryDC:wxMemoryDC() | wxGraphicsContext:wxGraphicsContext().
new(#wx_ref{type=WindowDCT}=WindowDC) ->
  IswxWindowDC = ?CLASS_T(WindowDCT,wxWindowDC),
  IswxMemoryDC = ?CLASS_T(WindowDCT,wxMemoryDC),
  IswxGraphicsContext = ?CLASS_T(WindowDCT,wxGraphicsContext),
  WindowDCType = if
    IswxWindowDC ->   wxWindowDC;
    IswxMemoryDC ->   wxMemoryDC;
    IswxGraphicsContext ->   wxGraphicsContext;
    true -> error({badarg, WindowDCT})
  end,
  wxe_util:queue_cmd(wx:typeCast(WindowDC, WindowDCType),?get_env(),?wxGCDC_new_1),
  wxe_util:rec(?wxGCDC_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgcdc.html#wxgcdcgetgraphicscontext">external documentation</a>.
-doc "Retrieves associated `m:wxGraphicsContext`.".
-spec getGraphicsContext(This) -> wxGraphicsContext:wxGraphicsContext() when
	This::wxGCDC().
getGraphicsContext(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGCDC),
  wxe_util:queue_cmd(This,?get_env(),?wxGCDC_GetGraphicsContext),
  wxe_util:rec(?wxGCDC_GetGraphicsContext).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgcdc.html#wxgcdcsetgraphicscontext">external documentation</a>.
-doc """
Set the graphics context to be used for this `m:wxGCDC`.

Note that this object takes ownership of `context` and will delete it when it is
destroyed or when `setGraphicsContext/2` is called again.

Also, unlike the constructor taking `m:wxGraphicsContext`, this method will
reapply the current font, pen and brush, so that this object continues to use
them, if they had been changed before (which is never the case when constructing
`m:wxGCDC` directly from `m:wxGraphicsContext`).
""".
-spec setGraphicsContext(This, Context) -> 'ok' when
	This::wxGCDC(), Context::wxGraphicsContext:wxGraphicsContext().
setGraphicsContext(#wx_ref{type=ThisT}=This,#wx_ref{type=ContextT}=Context) ->
  ?CLASS(ThisT,wxGCDC),
  ?CLASS(ContextT,wxGraphicsContext),
  wxe_util:queue_cmd(This,Context,?get_env(),?wxGCDC_SetGraphicsContext).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGCDC()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGCDC),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxDC
%% @hidden
-doc false.
startPage(This) -> wxDC:startPage(This).
%% @hidden
-doc false.
startDoc(This,Message) -> wxDC:startDoc(This,Message).
%% @hidden
-doc false.
setUserScale(This,XScale,YScale) -> wxDC:setUserScale(This,XScale,YScale).
%% @hidden
-doc false.
setTextForeground(This,Colour) -> wxDC:setTextForeground(This,Colour).
%% @hidden
-doc false.
setTextBackground(This,Colour) -> wxDC:setTextBackground(This,Colour).
%% @hidden
-doc false.
setPen(This,Pen) -> wxDC:setPen(This,Pen).
%% @hidden
-doc false.
setPalette(This,Palette) -> wxDC:setPalette(This,Palette).
%% @hidden
-doc false.
setMapMode(This,Mode) -> wxDC:setMapMode(This,Mode).
%% @hidden
-doc false.
setLogicalFunction(This,Function) -> wxDC:setLogicalFunction(This,Function).
%% @hidden
-doc false.
setLayoutDirection(This,Dir) -> wxDC:setLayoutDirection(This,Dir).
%% @hidden
-doc false.
setFont(This,Font) -> wxDC:setFont(This,Font).
%% @hidden
-doc false.
setDeviceOrigin(This,X,Y) -> wxDC:setDeviceOrigin(This,X,Y).
%% @hidden
-doc false.
setClippingRegion(This,Pt,Sz) -> wxDC:setClippingRegion(This,Pt,Sz).
%% @hidden
-doc false.
setClippingRegion(This,Rect) -> wxDC:setClippingRegion(This,Rect).
%% @hidden
-doc false.
setBrush(This,Brush) -> wxDC:setBrush(This,Brush).
%% @hidden
-doc false.
setBackgroundMode(This,Mode) -> wxDC:setBackgroundMode(This,Mode).
%% @hidden
-doc false.
setBackground(This,Brush) -> wxDC:setBackground(This,Brush).
%% @hidden
-doc false.
setAxisOrientation(This,XLeftRight,YBottomUp) -> wxDC:setAxisOrientation(This,XLeftRight,YBottomUp).
%% @hidden
-doc false.
resetBoundingBox(This) -> wxDC:resetBoundingBox(This).
%% @hidden
-doc false.
isOk(This) -> wxDC:isOk(This).
%% @hidden
-doc false.
minY(This) -> wxDC:minY(This).
%% @hidden
-doc false.
minX(This) -> wxDC:minX(This).
%% @hidden
-doc false.
maxY(This) -> wxDC:maxY(This).
%% @hidden
-doc false.
maxX(This) -> wxDC:maxX(This).
%% @hidden
-doc false.
logicalToDeviceYRel(This,Y) -> wxDC:logicalToDeviceYRel(This,Y).
%% @hidden
-doc false.
logicalToDeviceY(This,Y) -> wxDC:logicalToDeviceY(This,Y).
%% @hidden
-doc false.
logicalToDeviceXRel(This,X) -> wxDC:logicalToDeviceXRel(This,X).
%% @hidden
-doc false.
logicalToDeviceX(This,X) -> wxDC:logicalToDeviceX(This,X).
%% @hidden
-doc false.
gradientFillLinear(This,Rect,InitialColour,DestColour, Options) -> wxDC:gradientFillLinear(This,Rect,InitialColour,DestColour, Options).
%% @hidden
-doc false.
gradientFillLinear(This,Rect,InitialColour,DestColour) -> wxDC:gradientFillLinear(This,Rect,InitialColour,DestColour).
%% @hidden
-doc false.
gradientFillConcentric(This,Rect,InitialColour,DestColour,CircleCenter) -> wxDC:gradientFillConcentric(This,Rect,InitialColour,DestColour,CircleCenter).
%% @hidden
-doc false.
gradientFillConcentric(This,Rect,InitialColour,DestColour) -> wxDC:gradientFillConcentric(This,Rect,InitialColour,DestColour).
%% @hidden
-doc false.
getUserScale(This) -> wxDC:getUserScale(This).
%% @hidden
-doc false.
getTextForeground(This) -> wxDC:getTextForeground(This).
%% @hidden
-doc false.
getTextExtent(This,String, Options) -> wxDC:getTextExtent(This,String, Options).
%% @hidden
-doc false.
getTextExtent(This,String) -> wxDC:getTextExtent(This,String).
%% @hidden
-doc false.
getTextBackground(This) -> wxDC:getTextBackground(This).
%% @hidden
-doc false.
getSizeMM(This) -> wxDC:getSizeMM(This).
%% @hidden
-doc false.
getSize(This) -> wxDC:getSize(This).
%% @hidden
-doc false.
getPPI(This) -> wxDC:getPPI(This).
%% @hidden
-doc false.
getPixel(This,Pos) -> wxDC:getPixel(This,Pos).
%% @hidden
-doc false.
getPen(This) -> wxDC:getPen(This).
%% @hidden
-doc false.
getPartialTextExtents(This,Text) -> wxDC:getPartialTextExtents(This,Text).
%% @hidden
-doc false.
getMultiLineTextExtent(This,String, Options) -> wxDC:getMultiLineTextExtent(This,String, Options).
%% @hidden
-doc false.
getMultiLineTextExtent(This,String) -> wxDC:getMultiLineTextExtent(This,String).
%% @hidden
-doc false.
getMapMode(This) -> wxDC:getMapMode(This).
%% @hidden
-doc false.
getLogicalFunction(This) -> wxDC:getLogicalFunction(This).
%% @hidden
-doc false.
getLayoutDirection(This) -> wxDC:getLayoutDirection(This).
%% @hidden
-doc false.
getFont(This) -> wxDC:getFont(This).
%% @hidden
-doc false.
getClippingBox(This) -> wxDC:getClippingBox(This).
%% @hidden
-doc false.
getCharWidth(This) -> wxDC:getCharWidth(This).
%% @hidden
-doc false.
getCharHeight(This) -> wxDC:getCharHeight(This).
%% @hidden
-doc false.
getBrush(This) -> wxDC:getBrush(This).
%% @hidden
-doc false.
getBackgroundMode(This) -> wxDC:getBackgroundMode(This).
%% @hidden
-doc false.
getBackground(This) -> wxDC:getBackground(This).
%% @hidden
-doc false.
floodFill(This,Pt,Col, Options) -> wxDC:floodFill(This,Pt,Col, Options).
%% @hidden
-doc false.
floodFill(This,Pt,Col) -> wxDC:floodFill(This,Pt,Col).
%% @hidden
-doc false.
endPage(This) -> wxDC:endPage(This).
%% @hidden
-doc false.
endDoc(This) -> wxDC:endDoc(This).
%% @hidden
-doc false.
drawText(This,Text,Pt) -> wxDC:drawText(This,Text,Pt).
%% @hidden
-doc false.
drawRoundedRectangle(This,Pt,Sz,Radius) -> wxDC:drawRoundedRectangle(This,Pt,Sz,Radius).
%% @hidden
-doc false.
drawRoundedRectangle(This,Rect,Radius) -> wxDC:drawRoundedRectangle(This,Rect,Radius).
%% @hidden
-doc false.
drawRotatedText(This,Text,Point,Angle) -> wxDC:drawRotatedText(This,Text,Point,Angle).
%% @hidden
-doc false.
drawRectangle(This,Pt,Sz) -> wxDC:drawRectangle(This,Pt,Sz).
%% @hidden
-doc false.
drawRectangle(This,Rect) -> wxDC:drawRectangle(This,Rect).
%% @hidden
-doc false.
drawPoint(This,Pt) -> wxDC:drawPoint(This,Pt).
%% @hidden
-doc false.
drawPolygon(This,Points, Options) -> wxDC:drawPolygon(This,Points, Options).
%% @hidden
-doc false.
drawPolygon(This,Points) -> wxDC:drawPolygon(This,Points).
%% @hidden
-doc false.
drawLines(This,Points, Options) -> wxDC:drawLines(This,Points, Options).
%% @hidden
-doc false.
drawLines(This,Points) -> wxDC:drawLines(This,Points).
%% @hidden
-doc false.
drawLine(This,Pt1,Pt2) -> wxDC:drawLine(This,Pt1,Pt2).
%% @hidden
-doc false.
drawLabel(This,Text,Rect, Options) -> wxDC:drawLabel(This,Text,Rect, Options).
%% @hidden
-doc false.
drawLabel(This,Text,Rect) -> wxDC:drawLabel(This,Text,Rect).
%% @hidden
-doc false.
drawIcon(This,Icon,Pt) -> wxDC:drawIcon(This,Icon,Pt).
%% @hidden
-doc false.
drawEllipticArc(This,Pt,Sz,Sa,Ea) -> wxDC:drawEllipticArc(This,Pt,Sz,Sa,Ea).
%% @hidden
-doc false.
drawEllipse(This,Pt,Size) -> wxDC:drawEllipse(This,Pt,Size).
%% @hidden
-doc false.
drawEllipse(This,Rect) -> wxDC:drawEllipse(This,Rect).
%% @hidden
-doc false.
drawCircle(This,Pt,Radius) -> wxDC:drawCircle(This,Pt,Radius).
%% @hidden
-doc false.
drawCheckMark(This,Rect) -> wxDC:drawCheckMark(This,Rect).
%% @hidden
-doc false.
drawBitmap(This,Bmp,Pt, Options) -> wxDC:drawBitmap(This,Bmp,Pt, Options).
%% @hidden
-doc false.
drawBitmap(This,Bmp,Pt) -> wxDC:drawBitmap(This,Bmp,Pt).
%% @hidden
-doc false.
drawArc(This,PtStart,PtEnd,Centre) -> wxDC:drawArc(This,PtStart,PtEnd,Centre).
%% @hidden
-doc false.
deviceToLogicalYRel(This,Y) -> wxDC:deviceToLogicalYRel(This,Y).
%% @hidden
-doc false.
deviceToLogicalY(This,Y) -> wxDC:deviceToLogicalY(This,Y).
%% @hidden
-doc false.
deviceToLogicalXRel(This,X) -> wxDC:deviceToLogicalXRel(This,X).
%% @hidden
-doc false.
deviceToLogicalX(This,X) -> wxDC:deviceToLogicalX(This,X).
%% @hidden
-doc false.
destroyClippingRegion(This) -> wxDC:destroyClippingRegion(This).
%% @hidden
-doc false.
crossHair(This,Pt) -> wxDC:crossHair(This,Pt).
%% @hidden
-doc false.
clear(This) -> wxDC:clear(This).
%% @hidden
-doc false.
calcBoundingBox(This,X,Y) -> wxDC:calcBoundingBox(This,X,Y).
%% @hidden
-doc false.
blit(This,Dest,Size,Source,Src, Options) -> wxDC:blit(This,Dest,Size,Source,Src, Options).
%% @hidden
-doc false.
blit(This,Dest,Size,Source,Src) -> wxDC:blit(This,Dest,Size,Source,Src).
