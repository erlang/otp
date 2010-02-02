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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html">wxDC</a>.
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
  getBackgroundMode/1,getBrush/1,getCharHeight/1,getCharWidth/1,getClippingBox/2,
  getFont/1,getLayoutDirection/1,getLogicalFunction/1,getMapMode/1,
  getMultiLineTextExtent/2,getMultiLineTextExtent/3,getPPI/1,getPartialTextExtents/3,
  getPen/1,getPixel/3,getSize/1,getSizeMM/1,getTextBackground/1,getTextExtent/2,
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxDC(), DestPt::{X::integer(),Y::integer()}, Sz::{W::integer(),H::integer()}, Source::wxDC(), SrcPt::{X::integer(),Y::integer()}) -> bool()
%% @equiv blit(This,DestPt,Sz,Source,SrcPt, [])
blit(This,DestPt={DestPtX,DestPtY},Sz={SzW,SzH},Source,SrcPt={SrcPtX,SrcPtY})
 when is_record(This, wx_ref),is_integer(DestPtX),is_integer(DestPtY),is_integer(SzW),is_integer(SzH),is_record(Source, wx_ref),is_integer(SrcPtX),is_integer(SrcPtY) ->
  blit(This,DestPt,Sz,Source,SrcPt, []).

%% @spec (This::wxDC(), DestPt::{X::integer(),Y::integer()}, Sz::{W::integer(),H::integer()}, Source::wxDC(), SrcPt::{X::integer(),Y::integer()}, [Option]) -> bool()
%% Option = {rop, integer()} | {useMask, bool()} | {srcPtMask, {X::integer(),Y::integer()}}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcblit">external documentation</a>.
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

%% @spec (This::wxDC(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdccalcboundingbox">external documentation</a>.
calcBoundingBox(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_CalcBoundingBox,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxDC()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcclear">external documentation</a>.
clear(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_Clear,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdccomputescaleandorigin">external documentation</a>.
computeScaleAndOrigin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_ComputeScaleAndOrigin,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdccrosshair">external documentation</a>.
crossHair(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_CrossHair,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxDC()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdestroyclippingregion">external documentation</a>.
destroyClippingRegion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DestroyClippingRegion,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), X::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdevicetologicalx">external documentation</a>.
deviceToLogicalX(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_DeviceToLogicalX,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @spec (This::wxDC(), X::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdevicetologicalxrel">external documentation</a>.
deviceToLogicalXRel(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_DeviceToLogicalXRel,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @spec (This::wxDC(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdevicetologicaly">external documentation</a>.
deviceToLogicalY(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_DeviceToLogicalY,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @spec (This::wxDC(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdevicetologicalyrel">external documentation</a>.
deviceToLogicalYRel(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_DeviceToLogicalYRel,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @spec (This::wxDC(), Pt1::{X::integer(),Y::integer()}, Pt2::{X::integer(),Y::integer()}, Centre::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawarc">external documentation</a>.
drawArc(#wx_ref{type=ThisT,ref=ThisRef},{Pt1X,Pt1Y},{Pt2X,Pt2Y},{CentreX,CentreY})
 when is_integer(Pt1X),is_integer(Pt1Y),is_integer(Pt2X),is_integer(Pt2Y),is_integer(CentreX),is_integer(CentreY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawArc,
  <<ThisRef:32/?UI,Pt1X:32/?UI,Pt1Y:32/?UI,Pt2X:32/?UI,Pt2Y:32/?UI,CentreX:32/?UI,CentreY:32/?UI>>).

%% @spec (This::wxDC(), Bmp::wxBitmap:wxBitmap(), Pt::{X::integer(),Y::integer()}) -> ok
%% @equiv drawBitmap(This,Bmp,Pt, [])
drawBitmap(This,Bmp,Pt={PtX,PtY})
 when is_record(This, wx_ref),is_record(Bmp, wx_ref),is_integer(PtX),is_integer(PtY) ->
  drawBitmap(This,Bmp,Pt, []).

%% @spec (This::wxDC(), Bmp::wxBitmap:wxBitmap(), Pt::{X::integer(),Y::integer()}, [Option]) -> ok
%% Option = {useMask, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawbitmap">external documentation</a>.
drawBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BmpT,ref=BmpRef},{PtX,PtY}, Options)
 when is_integer(PtX),is_integer(PtY),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BmpT,wxBitmap),
  MOpts = fun({useMask, UseMask}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(UseMask)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxDC_DrawBitmap,
  <<ThisRef:32/?UI,BmpRef:32/?UI,PtX:32/?UI,PtY:32/?UI, BinOpt/binary>>).

%% @spec (This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawcheckmark">external documentation</a>.
drawCheckMark(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawCheckMark,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Radius::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawcircle">external documentation</a>.
drawCircle(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},Radius)
 when is_integer(PtX),is_integer(PtY),is_integer(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawCircle,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,Radius:32/?UI>>).

%% @spec (This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawellipse">external documentation</a>.
drawEllipse(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawEllipse_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Sz::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawellipse">external documentation</a>.
drawEllipse(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH})
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawEllipse_2,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Sz::{W::integer(),H::integer()}, Sa::float(), Ea::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawellipticarc">external documentation</a>.
drawEllipticArc(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH},Sa,Ea)
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH),is_float(Sa),is_float(Ea) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawEllipticArc,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI,0:32,Sa:64/?F,Ea:64/?F>>).

%% @spec (This::wxDC(), Icon::wxIcon:wxIcon(), Pt::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawicon">external documentation</a>.
drawIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(IconT,wxIcon),
  wxe_util:cast(?wxDC_DrawIcon,
  <<ThisRef:32/?UI,IconRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxDC(), Text::string(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok
%% @equiv drawLabel(This,Text,Rect, [])
drawLabel(This,Text,Rect={RectX,RectY,RectW,RectH})
 when is_record(This, wx_ref),is_list(Text),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  drawLabel(This,Text,Rect, []).

%% @spec (This::wxDC(), Text::string(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, [Option]) -> ok
%% Option = {alignment, integer()} | {indexAccel, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawlabel">external documentation</a>.
drawLabel(#wx_ref{type=ThisT,ref=ThisRef},Text,{RectX,RectY,RectW,RectH}, Options)
 when is_list(Text),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({alignment, Alignment}, Acc) -> [<<1:32/?UI,Alignment:32/?UI>>|Acc];
          ({indexAccel, IndexAccel}, Acc) -> [<<2:32/?UI,IndexAccel:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxDC_DrawLabel,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI, BinOpt/binary>>).

%% @spec (This::wxDC(), Pt1::{X::integer(),Y::integer()}, Pt2::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawline">external documentation</a>.
drawLine(#wx_ref{type=ThisT,ref=ThisRef},{Pt1X,Pt1Y},{Pt2X,Pt2Y})
 when is_integer(Pt1X),is_integer(Pt1Y),is_integer(Pt2X),is_integer(Pt2Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawLine,
  <<ThisRef:32/?UI,Pt1X:32/?UI,Pt1Y:32/?UI,Pt2X:32/?UI,Pt2Y:32/?UI>>).

%% @spec (This::wxDC(), Points::[{X::integer(),Y::integer()}]) -> ok
%% @equiv drawLines(This,Points, [])
drawLines(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawLines(This,Points, []).

%% @spec (This::wxDC(), Points::[{X::integer(),Y::integer()}], [Option]) -> ok
%% Option = {xoffset, integer()} | {yoffset, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawlines">external documentation</a>.
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

%% @spec (This::wxDC(), Points::[{X::integer(),Y::integer()}]) -> ok
%% @equiv drawPolygon(This,Points, [])
drawPolygon(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawPolygon(This,Points, []).

%% @spec (This::wxDC(), Points::[{X::integer(),Y::integer()}], [Option]) -> ok
%% Option = {xoffset, integer()} | {yoffset, integer()} | {fillStyle, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawpolygon">external documentation</a>.
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

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawpoint">external documentation</a>.
drawPoint(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawPoint,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawrectangle">external documentation</a>.
drawRectangle(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawRectangle_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Sz::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawrectangle">external documentation</a>.
drawRectangle(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH})
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawRectangle_2,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @spec (This::wxDC(), Text::string(), Pt::{X::integer(),Y::integer()}, Angle::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawrotatedtext">external documentation</a>.
drawRotatedText(#wx_ref{type=ThisT,ref=ThisRef},Text,{PtX,PtY},Angle)
 when is_list(Text),is_integer(PtX),is_integer(PtY),is_float(Angle) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxDC_DrawRotatedText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,PtX:32/?UI,PtY:32/?UI,Angle:64/?F>>).

%% @spec (This::wxDC(), R::{X::integer(),Y::integer(),W::integer(),H::integer()}, Radius::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawroundedrectangle">external documentation</a>.
drawRoundedRectangle(#wx_ref{type=ThisT,ref=ThisRef},{RX,RY,RW,RH},Radius)
 when is_integer(RX),is_integer(RY),is_integer(RW),is_integer(RH),is_float(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawRoundedRectangle_2,
  <<ThisRef:32/?UI,RX:32/?UI,RY:32/?UI,RW:32/?UI,RH:32/?UI,0:32,Radius:64/?F>>).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Sz::{W::integer(),H::integer()}, Radius::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawroundedrectangle">external documentation</a>.
drawRoundedRectangle(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH},Radius)
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH),is_float(Radius) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_DrawRoundedRectangle_3,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI,0:32,Radius:64/?F>>).

%% @spec (This::wxDC(), Text::string(), Pt::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcdrawtext">external documentation</a>.
drawText(#wx_ref{type=ThisT,ref=ThisRef},Text,{PtX,PtY})
 when is_list(Text),is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxDC_DrawText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxDC()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcenddoc">external documentation</a>.
endDoc(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_EndDoc,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcendpage">external documentation</a>.
endPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_EndPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Col::wx:colour()) -> bool()
%% @equiv floodFill(This,Pt,Col, [])
floodFill(This,Pt={PtX,PtY},Col)
 when is_record(This, wx_ref),is_integer(PtX),is_integer(PtY),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  floodFill(This,Pt,Col, []).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Col::wx:colour(), [Option]) -> bool()
%% Option = {style, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcfloodfill">external documentation</a>.
floodFill(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},Col, Options)
 when is_integer(PtX),is_integer(PtY),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4,is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({style, Style}, Acc) -> [<<1:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxDC_FloodFill,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,(wxe_util:colour_bin(Col)):16/binary, 0:32,BinOpt/binary>>).

%% @spec (This::wxDC()) -> wxBrush:wxBrush()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetbackground">external documentation</a>.
getBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetBackground,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetbackgroundmode">external documentation</a>.
getBackgroundMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetBackgroundMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> wxBrush:wxBrush()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetbrush">external documentation</a>.
getBrush(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetBrush,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetcharheight">external documentation</a>.
getCharHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetCharHeight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetcharwidth">external documentation</a>.
getCharWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetCharWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetclippingbox">external documentation</a>.
getClippingBox(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_GetClippingBox,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxDC()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetfont">external documentation</a>.
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> WxLayoutDirection
%% WxLayoutDirection = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetlayoutdirection">external documentation</a>.
%%<br /> WxLayoutDirection is one of ?wxLayout_Default | ?wxLayout_LeftToRight | ?wxLayout_RightToLeft
getLayoutDirection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetLayoutDirection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetlogicalfunction">external documentation</a>.
getLogicalFunction(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetLogicalFunction,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetmapmode">external documentation</a>.
getMapMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetMapMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), String::string()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetmultilinetextextent">external documentation</a>.
getMultiLineTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String)
 when is_list(String) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary([String,0]),
  wxe_util:call(?wxDC_GetMultiLineTextExtent_1,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxDC(), String::string(), [Option]) -> {Width::integer(),Height::integer(),HeightLine::integer()}
%% Option = {font, wxFont:wxFont()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetmultilinetextextent">external documentation</a>.
getMultiLineTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String, Options)
 when is_list(String),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary([String,0]),
  MOpts = fun({font, #wx_ref{type=FontT,ref=FontRef}}, Acc) ->   ?CLASS(FontT,wxFont),[<<1:32/?UI,FontRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxDC_GetMultiLineTextExtent_4,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxDC(), Text::string(), Widths::[integer()]) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetpartialtextextents">external documentation</a>.
getPartialTextExtents(#wx_ref{type=ThisT,ref=ThisRef},Text,Widths)
 when is_list(Text),is_list(Widths) ->
  ?CLASS(ThisT,wxDC),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxDC_GetPartialTextExtents,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,(length(Widths)):32/?UI,
        (<< <<C:32/?I>> || C <- Widths>>)/binary, 0:(((1+length(Widths)) rem 2)*32)>>).

%% @spec (This::wxDC()) -> wxPen:wxPen()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetpen">external documentation</a>.
getPen(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetPen,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Col::wx:colour()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetpixel">external documentation</a>.
getPixel(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},Col)
 when is_integer(PtX),is_integer(PtY),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetPixel,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @spec (This::wxDC()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetppi">external documentation</a>.
getPPI(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetPPI,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetsize">external documentation</a>.
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetsizemm">external documentation</a>.
getSizeMM(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetSizeMM,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgettextbackground">external documentation</a>.
getTextBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetTextBackground,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), String::string()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgettextextent">external documentation</a>.
getTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String)
 when is_list(String) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary([String,0]),
  wxe_util:call(?wxDC_GetTextExtent_1,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxDC(), String::string(), [Option]) -> {X::integer(),Y::integer(),Descent::integer(),ExternalLeading::integer()}
%% Option = {theFont, wxFont:wxFont()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgettextextent">external documentation</a>.
getTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String, Options)
 when is_list(String),is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  String_UC = unicode:characters_to_binary([String,0]),
  MOpts = fun({theFont, #wx_ref{type=TheFontT,ref=TheFontRef}}, Acc) ->   ?CLASS(TheFontT,wxFont),[<<1:32/?UI,TheFontRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxDC_GetTextExtent_4,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxDC()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgettextforeground">external documentation</a>.
getTextForeground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetTextForeground,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> {X::float(),Y::float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgetuserscale">external documentation</a>.
getUserScale(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_GetUserScale,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, InitialColour::wx:colour(), DestColour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgradientfillconcentric">external documentation</a>.
gradientFillConcentric(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH},InitialColour,DestColour)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),tuple_size(InitialColour) =:= 3; tuple_size(InitialColour) =:= 4,tuple_size(DestColour) =:= 3; tuple_size(DestColour) =:= 4 ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_GradientFillConcentric_3,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,(wxe_util:colour_bin(InitialColour)):16/binary,(wxe_util:colour_bin(DestColour)):16/binary>>).

%% @spec (This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, InitialColour::wx:colour(), DestColour::wx:colour(), CircleCenter::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgradientfillconcentric">external documentation</a>.
gradientFillConcentric(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH},InitialColour,DestColour,{CircleCenterX,CircleCenterY})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),tuple_size(InitialColour) =:= 3; tuple_size(InitialColour) =:= 4,tuple_size(DestColour) =:= 3; tuple_size(DestColour) =:= 4,is_integer(CircleCenterX),is_integer(CircleCenterY) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_GradientFillConcentric_4,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,(wxe_util:colour_bin(InitialColour)):16/binary,(wxe_util:colour_bin(DestColour)):16/binary,CircleCenterX:32/?UI,CircleCenterY:32/?UI>>).

%% @spec (This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, InitialColour::wx:colour(), DestColour::wx:colour()) -> ok
%% @equiv gradientFillLinear(This,Rect,InitialColour,DestColour, [])
gradientFillLinear(This,Rect={RectX,RectY,RectW,RectH},InitialColour,DestColour)
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),tuple_size(InitialColour) =:= 3; tuple_size(InitialColour) =:= 4,tuple_size(DestColour) =:= 3; tuple_size(DestColour) =:= 4 ->
  gradientFillLinear(This,Rect,InitialColour,DestColour, []).

%% @spec (This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, InitialColour::wx:colour(), DestColour::wx:colour(), [Option]) -> ok
%% Option = {nDirection, WxDirection}
%% WxDirection = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcgradientfilllinear">external documentation</a>.
%%<br /> WxDirection is one of ?wxLEFT | ?wxRIGHT | ?wxUP | ?wxDOWN | ?wxTOP | ?wxBOTTOM | ?wxNORTH | ?wxSOUTH | ?wxWEST | ?wxEAST | ?wxALL
gradientFillLinear(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH},InitialColour,DestColour, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),tuple_size(InitialColour) =:= 3; tuple_size(InitialColour) =:= 4,tuple_size(DestColour) =:= 3; tuple_size(DestColour) =:= 4,is_list(Options) ->
  ?CLASS(ThisT,wxDC),
  MOpts = fun({nDirection, NDirection}, Acc) -> [<<1:32/?UI,NDirection:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxDC_GradientFillLinear,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,(wxe_util:colour_bin(InitialColour)):16/binary,(wxe_util:colour_bin(DestColour)):16/binary, 0:32,BinOpt/binary>>).

%% @spec (This::wxDC(), X::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdclogicaltodevicex">external documentation</a>.
logicalToDeviceX(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_LogicalToDeviceX,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @spec (This::wxDC(), X::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdclogicaltodevicexrel">external documentation</a>.
logicalToDeviceXRel(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_LogicalToDeviceXRel,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @spec (This::wxDC(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdclogicaltodevicey">external documentation</a>.
logicalToDeviceY(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_LogicalToDeviceY,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @spec (This::wxDC(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdclogicaltodeviceyrel">external documentation</a>.
logicalToDeviceYRel(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_LogicalToDeviceYRel,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcmaxx">external documentation</a>.
maxX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_MaxX,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcmaxy">external documentation</a>.
maxY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_MaxY,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcminx">external documentation</a>.
minX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_MinX,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcminy">external documentation</a>.
minY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_MinY,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcisok">external documentation</a>.
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:call(?wxDC_IsOk,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcresetboundingbox">external documentation</a>.
resetBoundingBox(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_ResetBoundingBox,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxDC(), XLeftRight::bool(), YBottomUp::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetaxisorientation">external documentation</a>.
setAxisOrientation(#wx_ref{type=ThisT,ref=ThisRef},XLeftRight,YBottomUp)
 when is_boolean(XLeftRight),is_boolean(YBottomUp) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetAxisOrientation,
  <<ThisRef:32/?UI,(wxe_util:from_bool(XLeftRight)):32/?UI,(wxe_util:from_bool(YBottomUp)):32/?UI>>).

%% @spec (This::wxDC(), Brush::wxBrush:wxBrush()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetbackground">external documentation</a>.
setBackground(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BrushT,wxBrush),
  wxe_util:cast(?wxDC_SetBackground,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @spec (This::wxDC(), Mode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetbackgroundmode">external documentation</a>.
setBackgroundMode(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetBackgroundMode,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @spec (This::wxDC(), Brush::wxBrush:wxBrush()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetbrush">external documentation</a>.
setBrush(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(BrushT,wxBrush),
  wxe_util:cast(?wxDC_SetBrush,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @spec (This::wxDC(),X::term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetclippingregion">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setClippingRegion(This::wxDC(), Region::wxRegion:wxRegion()) -> ok </c>
%% </p>
%% <p><c>
%% setClippingRegion(This::wxDC(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok </c>
%% </p>
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

%% @spec (This::wxDC(), Pt::{X::integer(),Y::integer()}, Sz::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetclippingregion">external documentation</a>.
setClippingRegion(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY},{SzW,SzH})
 when is_integer(PtX),is_integer(PtY),is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetClippingRegion_2,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @spec (This::wxDC(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetdeviceorigin">external documentation</a>.
setDeviceOrigin(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetDeviceOrigin,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxDC(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetfont">external documentation</a>.
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxDC_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxDC(), Dir::WxLayoutDirection) -> ok
%% WxLayoutDirection = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetlayoutdirection">external documentation</a>.
%%<br /> WxLayoutDirection is one of ?wxLayout_Default | ?wxLayout_LeftToRight | ?wxLayout_RightToLeft
setLayoutDirection(#wx_ref{type=ThisT,ref=ThisRef},Dir)
 when is_integer(Dir) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetLayoutDirection,
  <<ThisRef:32/?UI,Dir:32/?UI>>).

%% @spec (This::wxDC(), Function::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetlogicalfunction">external documentation</a>.
setLogicalFunction(#wx_ref{type=ThisT,ref=ThisRef},Function)
 when is_integer(Function) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetLogicalFunction,
  <<ThisRef:32/?UI,Function:32/?UI>>).

%% @spec (This::wxDC(), Mode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetmapmode">external documentation</a>.
setMapMode(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetMapMode,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @spec (This::wxDC(), Palette::wxPalette:wxPalette()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetpalette">external documentation</a>.
setPalette(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PaletteT,ref=PaletteRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:cast(?wxDC_SetPalette,
  <<ThisRef:32/?UI,PaletteRef:32/?UI>>).

%% @spec (This::wxDC(), Pen::wxPen:wxPen()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetpen">external documentation</a>.
setPen(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PenT,ref=PenRef}) ->
  ?CLASS(ThisT,wxDC),
  ?CLASS(PenT,wxPen),
  wxe_util:cast(?wxDC_SetPen,
  <<ThisRef:32/?UI,PenRef:32/?UI>>).

%% @spec (This::wxDC(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsettextbackground">external documentation</a>.
setTextBackground(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetTextBackground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxDC(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsettextforeground">external documentation</a>.
setTextForeground(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetTextForeground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxDC(), X::float(), Y::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcsetuserscale">external documentation</a>.
setUserScale(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_float(X),is_float(Y) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_SetUserScale,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F>>).

%% @spec (This::wxDC(), Message::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcstartdoc">external documentation</a>.
startDoc(#wx_ref{type=ThisT,ref=ThisRef},Message)
 when is_list(Message) ->
  ?CLASS(ThisT,wxDC),
  Message_UC = unicode:characters_to_binary([Message,0]),
  wxe_util:call(?wxDC_StartDoc,
  <<ThisRef:32/?UI,(byte_size(Message_UC)):32/?UI,(Message_UC)/binary, 0:(((8- ((0+byte_size(Message_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxDC()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxdc.html#wxdcstartpage">external documentation</a>.
startPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDC),
  wxe_util:cast(?wxDC_StartPage,
  <<ThisRef:32/?UI>>).

