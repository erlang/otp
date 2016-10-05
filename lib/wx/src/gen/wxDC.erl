%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html">wxDC</a>.
%% @type wxDC().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxDC).
-include("wxe.hrl").
-export([blit/5,blit/6,calcBoundingBox/3,clear/1,computeScaleAndOrigin/1,crossHair/2,
  destroyClippingRegion/1,deviceToLogicalX/2,deviceToLogicalXRel/2,
  deviceToLogicalY/2,deviceToLogicalYRel/2,drawArc/4,drawBitmap/3,drawBitmap/4,
  drawCheckMark/2,drawCircle/3,drawEllipse/2,drawEllipse/3,drawEllipticArc/5,
  drawIcon/3,drawLabel/3,drawLabel/4,drawLine/3,drawLines/2,drawLines/3,
  drawPoint/2,drawPolygon/2,drawPolygon/3,drawRectangle/2,drawRectangle/3,
  drawRotatedText/4,drawRoundedRectangle/3,drawRoundedRectangle/4,
  drawText/3,endDoc/1,endPage/1,floodFill/3,floodFill/4,getBackground/1,
  getBackgroundMode/1,getBrush/1,getCharHeight/1,getCharWidth/1,getClippingBox/1,
  getFont/1,getLayoutDirection/1,getLogicalFunction/1,getMapMode/1,
  getMultiLineTextExtent/2,getMultiLineTextExtent/3,getPPI/1,getPartialTextExtents/2,
  getPen/1,getPixel/2,getSize/1,getSizeMM/1,getTextBackground/1,getTextExtent/2,
  getTextExtent/3,getTextForeground/1,getUserScale/1,gradientFillConcentric/4,
  gradientFillConcentric/5,gradientFillLinear/4,gradientFillLinear/5,
  isOk/1,logicalToDeviceX/2,logicalToDeviceXRel/2,logicalToDeviceY/2,
  logicalToDeviceYRel/2,maxX/1,maxY/1,minX/1,minY/1,resetBoundingBox/1,
  setAxisOrientation/3,setBackground/2,setBackgroundMode/2,setBrush/2,
  setClippingRegion/2,setClippingRegion/3,setDeviceOrigin/3,setFont/2,
  setLayoutDirection/2,setLogicalFunction/2,setMapMode/2,setPalette/2,
  setPen/2,setTextBackground/2,setTextForeground/2,setUserScale/3,startDoc/2,
  startPage/1]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxDC/0]).
-deprecated([computeScaleAndOrigin/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxDC() :: wx:wx_object().
%% @equiv blit(This,DestPt,Sz,Source,SrcPt, [])
-spec blit(This, DestPt, Sz, Source, SrcPt) -> boolean() when
	This::wxDC(), DestPt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}, Source::wxDC(), SrcPt::{X::integer(), Y::integer()}.

blit(This,DestPt={DestPtX,DestPtY},Sz={SzW,SzH},Source,SrcPt={SrcPtX,SrcPtY})
 when is_record(This, wx_ref),is_integer(DestPtX),is_integer(DestPtY),is_integer(SzW),is_integer(SzH),is_record(Source, wx_ref),is_integer(SrcPtX),is_integer(SrcPtY) ->
  blit(This,DestPt,Sz,Source,SrcPt, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcblit">external documentation</a>.
%%<br /> Rop = integer
-spec blit(This, DestPt, Sz, Source, SrcPt, [Option]) -> boolean() when
	This::wxDC(), DestPt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}, Source::wxDC(), SrcPt::{X::integer(), Y::integer()},
	Option :: {'rop', wx:wx_enum()}
		 | {'useMask', boolean()}
		 | {'srcPtMask', {X::integer(), Y::integer()}}.
blit(#wx_ref{type=ThisT,ref=ThisRef},{DestPtX,DestPtY},{SzW,SzH},#wx_ref{type=SourceT,ref=SourceRef},{SrcPtX,SrcPtY}, Options)
 when is_integer(DestPtX),is_integer(DestPtY),is_integer(SzW),is_integer(SzH),is_integer(SrcPtX),is_integer(SrcPtY),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(SourceT,wxDC),
  MOpts = fun({rop, Rop}, Acc) -> [<<1:32/?UI,Rop:32/?UI>>|Acc];
          ({useMask, UseMask}, Acc) -> [<<2:32/?UI,(wxe_util:from_bool(UseMask)):32/?UI>>|Acc];
          ({srcPtMask, {SrcPtMaskX,SrcPtMaskY}}, Acc) -> [<<3:32/?UI,SrcPtMaskX:32/?UI,SrcPtMaskY:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxDC_Blit,
  <<ThisRef:32/?UI,DestPtX:32/?UI,DestPtY:32/?UI,SzW:32/?UI,SzH:32/?UI,SourceRef:32/?UI,SrcPtX:32/?UI,SrcPtY:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdccalcboundingbox">external documentation</a>.
-spec calcBoundingBox(This, X, Y) -> 'ok' when
	This::wxDC(), X::integer(), Y::integer().
calcBoundingBox(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_CalcBoundingBox,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxDC().
clear(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_Clear,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdccomputescaleandorigin">external documentation</a>.
-spec computeScaleAndOrigin(This) -> 'ok' when
	This::wxDC().
computeScaleAndOrigin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_ComputeScaleAndOrigin,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdccrosshair">external documentation</a>.
-spec crossHair(This, Pt) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}.
crossHair(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_CrossHair,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdestroyclippingregion">external documentation</a>.
-spec destroyClippingRegion(This) -> 'ok' when
	This::wxDC().
destroyClippingRegion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DestroyClippingRegion,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdevicetologicalx">external documentation</a>.
-spec deviceToLogicalX(This, X) -> integer() when
	This::wxDC(), X::integer().
deviceToLogicalX(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_DeviceToLogicalX,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdevicetologicalxrel">external documentation</a>.
-spec deviceToLogicalXRel(This, X) -> integer() when
	This::wxDC(), X::integer().
deviceToLogicalXRel(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_DeviceToLogicalXRel,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdevicetologicaly">external documentation</a>.
-spec deviceToLogicalY(This, Y) -> integer() when
	This::wxDC(), Y::integer().
deviceToLogicalY(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_DeviceToLogicalY,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdevicetologicalyrel">external documentation</a>.
-spec deviceToLogicalYRel(This, Y) -> integer() when
	This::wxDC(), Y::integer().
deviceToLogicalYRel(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_DeviceToLogicalYRel,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawarc">external documentation</a>.
-spec drawArc(This, Pt1, Pt2, Centre) -> 'ok' when
	This::wxDC(), Pt1::{X::integer(), Y::integer()}, Pt2::{X::integer(), Y::integer()}, Centre::{X::integer(), Y::integer()}.
drawArc(#wx_ref{type=ThisT,ref=ThisRef},{Pt1X,Pt1Y},{Pt2X,Pt2Y},{CentreX,CentreY})
 when is_integer(Pt1X),is_integer(Pt1Y),is_integer(Pt2X),is_integer(Pt2Y),is_integer(CentreX),is_integer(CentreY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawArc,
  <<ThisRef:32/?UI,Pt1X:32/?UI,Pt1Y:32/?UI,Pt2X:32/?UI,Pt2Y:32/?UI,CentreX:32/?UI,CentreY:32/?UI>>).

%% @equiv drawBitmap(This,Bmp,Pt, [])
-spec drawBitmap(This, Bmp, Pt) -> 'ok' when
	This::wxDC(), Bmp::wxBitmap:wxBitmap(), Pt::{X::integer(), Y::integer()}.

drawBitmap(This,Bmp,Pt={PtX,PtY})
 when is_record(This, wx_ref),is_record(Bmp, wx_ref),is_integer(PtX),is_integer(PtY) ->
  drawBitmap(This,Bmp,Pt, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawbitmap">external documentation</a>.
-spec drawBitmap(This, Bmp, Pt, [Option]) -> 'ok' when
	This::wxDC(), Bmp::wxBitmap:wxBitmap(), Pt::{X::integer(), Y::integer()},
	Option :: {'useMask', boolean()}.
drawBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BmpT,ref=BmpRef},{PtX,PtY}, Options)
 when is_integer(PtX),is_integer(PtY),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BmpT,wxBitmap),
  MOpts = fun({useMask, UseMask}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(UseMask)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxDC_DrawBitmap,
  <<ThisRef:32/?UI,BmpRef:32/?UI,PtX:32/?UI,PtY:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawcheckmark">external documentation</a>.
-spec drawCheckMark(This, Rect) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
drawCheckMark(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawCheckMark,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawcircle">external documentation</a>.
-spec drawCircle(This, Pt, Radius) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Radius::integer().
drawCircle(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},Radius)
 when is_integer(PtX),is_integer(PtY),is_integer(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawCircle,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,Radius:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawellipse">external documentation</a>.
-spec drawEllipse(This, Rect) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
drawEllipse(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawEllipse_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawellipse">external documentation</a>.
-spec drawEllipse(This, Pt, Sz) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}.
drawEllipse(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH})
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawEllipse_2,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawellipticarc">external documentation</a>.
-spec drawEllipticArc(This, Pt, Sz, Sa, Ea) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}, Sa::number(), Ea::number().
drawEllipticArc(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH},Sa,Ea)
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH),is_number(Sa),is_number(Ea) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawEllipticArc,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI,0:32,Sa:64/?F,Ea:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawicon">external documentation</a>.
-spec drawIcon(This, Icon, Pt) -> 'ok' when
	This::wxDC(), Icon::wxIcon:wxIcon(), Pt::{X::integer(), Y::integer()}.
drawIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(IconT,wxIcon),
  wxe_util:cast(?wxDC_DrawIcon,
  <<ThisRef:32/?UI,IconRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @equiv drawLabel(This,Text,Rect, [])
-spec drawLabel(This, Text, Rect) -> 'ok' when
	This::wxDC(), Text::unicode:chardata(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.

drawLabel(This,Text,Rect={RectX,RectY,RectW,RectH})
 when is_record(This, wx_ref),?is_chardata(Text),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  drawLabel(This,Text,Rect, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawlabel">external documentation</a>.
-spec drawLabel(This, Text, Rect, [Option]) -> 'ok' when
	This::wxDC(), Text::unicode:chardata(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},
	Option :: {'alignment', integer()}
		 | {'indexAccel', integer()}.
drawLabel(#wx_ref{type=ThisT,ref=ThisRef},Text,{RectX,RectY,RectW,RectH}, Options)
 when ?is_chardata(Text),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({alignment, Alignment}, Acc) -> [<<1:32/?UI,Alignment:32/?UI>>|Acc];
          ({indexAccel, IndexAccel}, Acc) -> [<<2:32/?UI,IndexAccel:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxDC_DrawLabel,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawline">external documentation</a>.
-spec drawLine(This, Pt1, Pt2) -> 'ok' when
	This::wxDC(), Pt1::{X::integer(), Y::integer()}, Pt2::{X::integer(), Y::integer()}.
drawLine(#wx_ref{type=ThisT,ref=ThisRef},{Pt1X,Pt1Y},{Pt2X,Pt2Y})
 when is_integer(Pt1X),is_integer(Pt1Y),is_integer(Pt2X),is_integer(Pt2Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawLine,
  <<ThisRef:32/?UI,Pt1X:32/?UI,Pt1Y:32/?UI,Pt2X:32/?UI,Pt2Y:32/?UI>>).

%% @equiv drawLines(This,Points, [])
-spec drawLines(This, Points) -> 'ok' when
	This::wxDC(), Points::[{X::integer(), Y::integer()}].

drawLines(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawLines(This,Points, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawlines">external documentation</a>.
-spec drawLines(This, Points, [Option]) -> 'ok' when
	This::wxDC(), Points::[{X::integer(), Y::integer()}],
	Option :: {'xoffset', integer()}
		 | {'yoffset', integer()}.
drawLines(#wx_ref{type=ThisT,ref=ThisRef},Points, Options)
 when is_list(Points),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({xoffset, Xoffset}, Acc) -> [<<1:32/?UI,Xoffset:32/?UI>>|Acc];
          ({yoffset, Yoffset}, Acc) -> [<<2:32/?UI,Yoffset:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxDC_DrawLines,
  <<ThisRef:32/?UI,(length(Points)):32/?UI,
        (<< <<X:32/?I,Y:32/?I>> || {X,Y} <- Points>>)/binary, BinOpt/binary>>).

%% @equiv drawPolygon(This,Points, [])
-spec drawPolygon(This, Points) -> 'ok' when
	This::wxDC(), Points::[{X::integer(), Y::integer()}].

drawPolygon(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawPolygon(This,Points, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawpolygon">external documentation</a>.
%%<br /> FillStyle = integer
-spec drawPolygon(This, Points, [Option]) -> 'ok' when
	This::wxDC(), Points::[{X::integer(), Y::integer()}],
	Option :: {'xoffset', integer()}
		 | {'yoffset', integer()}
		 | {'fillStyle', wx:wx_enum()}.
drawPolygon(#wx_ref{type=ThisT,ref=ThisRef},Points, Options)
 when is_list(Points),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({xoffset, Xoffset}, Acc) -> [<<1:32/?UI,Xoffset:32/?UI>>|Acc];
          ({yoffset, Yoffset}, Acc) -> [<<2:32/?UI,Yoffset:32/?UI>>|Acc];
          ({fillStyle, FillStyle}, Acc) -> [<<3:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxDC_DrawPolygon,
  <<ThisRef:32/?UI,(length(Points)):32/?UI,
        (<< <<X:32/?I,Y:32/?I>> || {X,Y} <- Points>>)/binary, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawpoint">external documentation</a>.
-spec drawPoint(This, Pt) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}.
drawPoint(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawPoint,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawrectangle">external documentation</a>.
-spec drawRectangle(This, Rect) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
drawRectangle(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawRectangle_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawrectangle">external documentation</a>.
-spec drawRectangle(This, Pt, Sz) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}.
drawRectangle(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH})
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawRectangle_2,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawrotatedtext">external documentation</a>.
-spec drawRotatedText(This, Text, Pt, Angle) -> 'ok' when
	This::wxDC(), Text::unicode:chardata(), Pt::{X::integer(), Y::integer()}, Angle::number().
drawRotatedText(#wx_ref{type=ThisT,ref=ThisRef},Text,{PtX,PtY},Angle)
 when ?is_chardata(Text),is_integer(PtX),is_integer(PtY),is_number(Angle) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxDC_DrawRotatedText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,PtX:32/?UI,PtY:32/?UI,Angle:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawroundedrectangle">external documentation</a>.
-spec drawRoundedRectangle(This, R, Radius) -> 'ok' when
	This::wxDC(), R::{X::integer(), Y::integer(), W::integer(), H::integer()}, Radius::number().
drawRoundedRectangle(#wx_ref{type=ThisT,ref=ThisRef},{RX,RY,RW,RH},Radius)
 when is_integer(RX),is_integer(RY),is_integer(RW),is_integer(RH),is_number(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawRoundedRectangle_2,
  <<ThisRef:32/?UI,RX:32/?UI,RY:32/?UI,RW:32/?UI,RH:32/?UI,0:32,Radius:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawroundedrectangle">external documentation</a>.
-spec drawRoundedRectangle(This, Pt, Sz, Radius) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}, Radius::number().
drawRoundedRectangle(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH},Radius)
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH),is_number(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawRoundedRectangle_3,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI,0:32,Radius:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcdrawtext">external documentation</a>.
-spec drawText(This, Text, Pt) -> 'ok' when
	This::wxDC(), Text::unicode:chardata(), Pt::{X::integer(), Y::integer()}.
drawText(#wx_ref{type=ThisT,ref=ThisRef},Text,{PtX,PtY})
 when ?is_chardata(Text),is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxDC_DrawText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcenddoc">external documentation</a>.
-spec endDoc(This) -> 'ok' when
	This::wxDC().
endDoc(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_EndDoc,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcendpage">external documentation</a>.
-spec endPage(This) -> 'ok' when
	This::wxDC().
endPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_EndPage,
  <<ThisRef:32/?UI>>).

%% @equiv floodFill(This,Pt,Col, [])
-spec floodFill(This, Pt, Col) -> boolean() when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Col::wx:wx_colour().

floodFill(This,Pt={PtX,PtY},Col)
 when is_record(This, wx_ref),is_integer(PtX),is_integer(PtY),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  floodFill(This,Pt,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcfloodfill">external documentation</a>.
%%<br /> Style = integer
-spec floodFill(This, Pt, Col, [Option]) -> boolean() when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Col::wx:wx_colour(),
	Option :: {'style', wx:wx_enum()}.
floodFill(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},Col, Options)
 when is_integer(PtX),is_integer(PtY),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4,is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({style, Style}, Acc) -> [<<1:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxDC_FloodFill,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,(wxe_util:colour_bin(Col)):16/binary, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetbackground">external documentation</a>.
-spec getBackground(This) -> wxBrush:wxBrush() when
	This::wxDC().
getBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetBackground,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetbackgroundmode">external documentation</a>.
-spec getBackgroundMode(This) -> integer() when
	This::wxDC().
getBackgroundMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetBackgroundMode,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetbrush">external documentation</a>.
-spec getBrush(This) -> wxBrush:wxBrush() when
	This::wxDC().
getBrush(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetBrush,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetcharheight">external documentation</a>.
-spec getCharHeight(This) -> integer() when
	This::wxDC().
getCharHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetCharHeight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetcharwidth">external documentation</a>.
-spec getCharWidth(This) -> integer() when
	This::wxDC().
getCharWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetCharWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetclippingbox">external documentation</a>.
-spec getClippingBox(This) -> Result when
	Result ::{X::integer(), Y::integer(), W::integer(), H::integer()},
	This::wxDC().
getClippingBox(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetClippingBox,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxDC().
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetlayoutdirection">external documentation</a>.
%%<br /> Res = ?wxLayout_Default | ?wxLayout_LeftToRight | ?wxLayout_RightToLeft
-spec getLayoutDirection(This) -> wx:wx_enum() when
	This::wxDC().
getLayoutDirection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetLayoutDirection,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetlogicalfunction">external documentation</a>.
-spec getLogicalFunction(This) -> integer() when
	This::wxDC().
getLogicalFunction(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetLogicalFunction,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetmapmode">external documentation</a>.
-spec getMapMode(This) -> integer() when
	This::wxDC().
getMapMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetMapMode,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetmultilinetextextent">external documentation</a>.
-spec getMultiLineTextExtent(This, String) -> {W::integer(), H::integer()} when
	This::wxDC(), String::unicode:chardata().
getMultiLineTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String)
 when ?is_chardata(String) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary([String,0]),
  wxe_util:call(?wxDC_GetMultiLineTextExtent_1,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetmultilinetextextent">external documentation</a>.
-spec getMultiLineTextExtent(This, String, [Option]) -> {Width::integer(), Height::integer(), HeightLine::integer()} when
	This::wxDC(), String::unicode:chardata(),
	Option :: {'font', wxFont:wxFont()}.
getMultiLineTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String, Options)
 when ?is_chardata(String),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary([String,0]),
  MOpts = fun({font, #wx_ref{type=FontT,ref=FontRef}}, Acc) ->   ?CLASS(FontT,wxFont),[<<1:32/?UI,FontRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxDC_GetMultiLineTextExtent_4,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetpartialtextextents">external documentation</a>.
-spec getPartialTextExtents(This, Text) -> Result when
	Result ::{Res ::boolean(), Widths::[integer()]},
	This::wxDC(), Text::unicode:chardata().
getPartialTextExtents(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxDC_GetPartialTextExtents,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetpen">external documentation</a>.
-spec getPen(This) -> wxPen:wxPen() when
	This::wxDC().
getPen(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetPen,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetpixel">external documentation</a>.
-spec getPixel(This, Pt) -> Result when
	Result ::{Res ::boolean(), Col::wx:wx_colour4()},
	This::wxDC(), Pt::{X::integer(), Y::integer()}.
getPixel(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetPixel,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetppi">external documentation</a>.
-spec getPPI(This) -> {W::integer(), H::integer()} when
	This::wxDC().
getPPI(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetPPI,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetsize">external documentation</a>.
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxDC().
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetsizemm">external documentation</a>.
-spec getSizeMM(This) -> {W::integer(), H::integer()} when
	This::wxDC().
getSizeMM(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetSizeMM,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgettextbackground">external documentation</a>.
-spec getTextBackground(This) -> wx:wx_colour4() when
	This::wxDC().
getTextBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetTextBackground,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgettextextent">external documentation</a>.
-spec getTextExtent(This, String) -> {W::integer(), H::integer()} when
	This::wxDC(), String::unicode:chardata().
getTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String)
 when ?is_chardata(String) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary([String,0]),
  wxe_util:call(?wxDC_GetTextExtent_1,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgettextextent">external documentation</a>.
-spec getTextExtent(This, String, [Option]) -> Result when
	Result :: {X::integer(), Y::integer(), Descent::integer(), ExternalLeading::integer()},
	This::wxDC(), String::unicode:chardata(),
	Option :: {'theFont', wxFont:wxFont()}.
getTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String, Options)
 when ?is_chardata(String),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary([String,0]),
  MOpts = fun({theFont, #wx_ref{type=TheFontT,ref=TheFontRef}}, Acc) ->   ?CLASS(TheFontT,wxFont),[<<1:32/?UI,TheFontRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxDC_GetTextExtent_4,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgettextforeground">external documentation</a>.
-spec getTextForeground(This) -> wx:wx_colour4() when
	This::wxDC().
getTextForeground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetTextForeground,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgetuserscale">external documentation</a>.
-spec getUserScale(This) -> {X::number(), Y::number()} when
	This::wxDC().
getUserScale(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetUserScale,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgradientfillconcentric">external documentation</a>.
-spec gradientFillConcentric(This, Rect, InitialColour, DestColour) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, InitialColour::wx:wx_colour(), DestColour::wx:wx_colour().
gradientFillConcentric(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH},InitialColour,DestColour)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),tuple_size(InitialColour) =:= 3; tuple_size(InitialColour) =:= 4,tuple_size(DestColour) =:= 3; tuple_size(DestColour) =:= 4 ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_GradientFillConcentric_3,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,(wxe_util:colour_bin(InitialColour)):16/binary,(wxe_util:colour_bin(DestColour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgradientfillconcentric">external documentation</a>.
-spec gradientFillConcentric(This, Rect, InitialColour, DestColour, CircleCenter) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, InitialColour::wx:wx_colour(), DestColour::wx:wx_colour(), CircleCenter::{X::integer(), Y::integer()}.
gradientFillConcentric(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH},InitialColour,DestColour,{CircleCenterX,CircleCenterY})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),tuple_size(InitialColour) =:= 3; tuple_size(InitialColour) =:= 4,tuple_size(DestColour) =:= 3; tuple_size(DestColour) =:= 4,is_integer(CircleCenterX),is_integer(CircleCenterY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_GradientFillConcentric_4,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,(wxe_util:colour_bin(InitialColour)):16/binary,(wxe_util:colour_bin(DestColour)):16/binary,CircleCenterX:32/?UI,CircleCenterY:32/?UI>>).

%% @equiv gradientFillLinear(This,Rect,InitialColour,DestColour, [])
-spec gradientFillLinear(This, Rect, InitialColour, DestColour) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, InitialColour::wx:wx_colour(), DestColour::wx:wx_colour().

gradientFillLinear(This,Rect={RectX,RectY,RectW,RectH},InitialColour,DestColour)
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),tuple_size(InitialColour) =:= 3; tuple_size(InitialColour) =:= 4,tuple_size(DestColour) =:= 3; tuple_size(DestColour) =:= 4 ->
  gradientFillLinear(This,Rect,InitialColour,DestColour, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcgradientfilllinear">external documentation</a>.
%%<br /> NDirection = ?wxLEFT | ?wxRIGHT | ?wxUP | ?wxDOWN | ?wxTOP | ?wxBOTTOM | ?wxNORTH | ?wxSOUTH | ?wxWEST | ?wxEAST | ?wxALL
-spec gradientFillLinear(This, Rect, InitialColour, DestColour, [Option]) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, InitialColour::wx:wx_colour(), DestColour::wx:wx_colour(),
	Option :: {'nDirection', wx:wx_enum()}.
gradientFillLinear(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH},InitialColour,DestColour, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),tuple_size(InitialColour) =:= 3; tuple_size(InitialColour) =:= 4,tuple_size(DestColour) =:= 3; tuple_size(DestColour) =:= 4,is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({nDirection, NDirection}, Acc) -> [<<1:32/?UI,NDirection:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxDC_GradientFillLinear,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,(wxe_util:colour_bin(InitialColour)):16/binary,(wxe_util:colour_bin(DestColour)):16/binary, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdclogicaltodevicex">external documentation</a>.
-spec logicalToDeviceX(This, X) -> integer() when
	This::wxDC(), X::integer().
logicalToDeviceX(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_LogicalToDeviceX,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdclogicaltodevicexrel">external documentation</a>.
-spec logicalToDeviceXRel(This, X) -> integer() when
	This::wxDC(), X::integer().
logicalToDeviceXRel(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_LogicalToDeviceXRel,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdclogicaltodevicey">external documentation</a>.
-spec logicalToDeviceY(This, Y) -> integer() when
	This::wxDC(), Y::integer().
logicalToDeviceY(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_LogicalToDeviceY,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdclogicaltodeviceyrel">external documentation</a>.
-spec logicalToDeviceYRel(This, Y) -> integer() when
	This::wxDC(), Y::integer().
logicalToDeviceYRel(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_LogicalToDeviceYRel,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcmaxx">external documentation</a>.
-spec maxX(This) -> integer() when
	This::wxDC().
maxX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_MaxX,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcmaxy">external documentation</a>.
-spec maxY(This) -> integer() when
	This::wxDC().
maxY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_MaxY,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcminx">external documentation</a>.
-spec minX(This) -> integer() when
	This::wxDC().
minX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_MinX,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcminy">external documentation</a>.
-spec minY(This) -> integer() when
	This::wxDC().
minY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_MinY,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxDC().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcresetboundingbox">external documentation</a>.
-spec resetBoundingBox(This) -> 'ok' when
	This::wxDC().
resetBoundingBox(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_ResetBoundingBox,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetaxisorientation">external documentation</a>.
-spec setAxisOrientation(This, XLeftRight, YBottomUp) -> 'ok' when
	This::wxDC(), XLeftRight::boolean(), YBottomUp::boolean().
setAxisOrientation(#wx_ref{type=ThisT,ref=ThisRef},XLeftRight,YBottomUp)
 when is_boolean(XLeftRight),is_boolean(YBottomUp) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetAxisOrientation,
  <<ThisRef:32/?UI,(wxe_util:from_bool(XLeftRight)):32/?UI,(wxe_util:from_bool(YBottomUp)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetbackground">external documentation</a>.
-spec setBackground(This, Brush) -> 'ok' when
	This::wxDC(), Brush::wxBrush:wxBrush().
setBackground(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BrushT,wxBrush),
  wxe_util:cast(?wxDC_SetBackground,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetbackgroundmode">external documentation</a>.
-spec setBackgroundMode(This, Mode) -> 'ok' when
	This::wxDC(), Mode::integer().
setBackgroundMode(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetBackgroundMode,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetbrush">external documentation</a>.
-spec setBrush(This, Brush) -> 'ok' when
	This::wxDC(), Brush::wxBrush:wxBrush().
setBrush(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BrushT,wxBrush),
  wxe_util:cast(?wxDC_SetBrush,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetclippingregion">external documentation</a>.
%% <br /> Also:<br />
%% setClippingRegion(This, Rect) -> 'ok' when<br />
%% 	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec setClippingRegion(This, Region) -> 'ok' when
	This::wxDC(), Region::wxRegion:wxRegion();
      (This, Rect) -> 'ok' when
	This::wxDC(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
setClippingRegion(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RegionT,ref=RegionRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(RegionT,wxRegion),
  wxe_util:cast(?wxDC_SetClippingRegion_1_0,
  <<ThisRef:32/?UI,RegionRef:32/?UI>>);
setClippingRegion(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetClippingRegion_1_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetclippingregion">external documentation</a>.
-spec setClippingRegion(This, Pt, Sz) -> 'ok' when
	This::wxDC(), Pt::{X::integer(), Y::integer()}, Sz::{W::integer(), H::integer()}.
setClippingRegion(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH})
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetClippingRegion_2,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetdeviceorigin">external documentation</a>.
-spec setDeviceOrigin(This, X, Y) -> 'ok' when
	This::wxDC(), X::integer(), Y::integer().
setDeviceOrigin(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetDeviceOrigin,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxDC(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxDC_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetlayoutdirection">external documentation</a>.
%%<br /> Dir = ?wxLayout_Default | ?wxLayout_LeftToRight | ?wxLayout_RightToLeft
-spec setLayoutDirection(This, Dir) -> 'ok' when
	This::wxDC(), Dir::wx:wx_enum().
setLayoutDirection(#wx_ref{type=ThisT,ref=ThisRef},Dir)
 when is_integer(Dir) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetLayoutDirection,
  <<ThisRef:32/?UI,Dir:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetlogicalfunction">external documentation</a>.
%%<br /> Function = integer
-spec setLogicalFunction(This, Function) -> 'ok' when
	This::wxDC(), Function::wx:wx_enum().
setLogicalFunction(#wx_ref{type=ThisT,ref=ThisRef},Function)
 when is_integer(Function) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetLogicalFunction,
  <<ThisRef:32/?UI,Function:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetmapmode">external documentation</a>.
%%<br /> Mode = integer
-spec setMapMode(This, Mode) -> 'ok' when
	This::wxDC(), Mode::wx:wx_enum().
setMapMode(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetMapMode,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetpalette">external documentation</a>.
-spec setPalette(This, Palette) -> 'ok' when
	This::wxDC(), Palette::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PaletteT,ref=PaletteRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:cast(?wxDC_SetPalette,
  <<ThisRef:32/?UI,PaletteRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetpen">external documentation</a>.
-spec setPen(This, Pen) -> 'ok' when
	This::wxDC(), Pen::wxPen:wxPen().
setPen(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PenT,ref=PenRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(PenT,wxPen),
  wxe_util:cast(?wxDC_SetPen,
  <<ThisRef:32/?UI,PenRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsettextbackground">external documentation</a>.
-spec setTextBackground(This, Colour) -> 'ok' when
	This::wxDC(), Colour::wx:wx_colour().
setTextBackground(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetTextBackground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsettextforeground">external documentation</a>.
-spec setTextForeground(This, Colour) -> 'ok' when
	This::wxDC(), Colour::wx:wx_colour().
setTextForeground(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetTextForeground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcsetuserscale">external documentation</a>.
-spec setUserScale(This, X, Y) -> 'ok' when
	This::wxDC(), X::number(), Y::number().
setUserScale(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetUserScale,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcstartdoc">external documentation</a>.
-spec startDoc(This, Message) -> boolean() when
	This::wxDC(), Message::unicode:chardata().
startDoc(#wx_ref{type=ThisT,ref=ThisRef},Message)
 when ?is_chardata(Message) ->
  ?CLASS(ThisT,wxDC),
  Message_UC = unicode:characters_to_binary([Message,0]),
  wxe_util:call(?wxDC_StartDoc,
  <<ThisRef:32/?UI,(byte_size(Message_UC)):32/?UI,(Message_UC)/binary, 0:(((8- ((0+byte_size(Message_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdc.html#wxdcstartpage">external documentation</a>.
-spec startPage(This) -> 'ok' when
	This::wxDC().
startPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_StartPage,
  <<ThisRef:32/?UI>>).

