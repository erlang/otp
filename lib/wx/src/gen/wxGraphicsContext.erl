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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html">wxGraphicsContext</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGraphicsObject}
%% </p>
%% @type wxGraphicsContext().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGraphicsContext).
-include("wxe.hrl").
-export([clip/2,clip/5,concatTransform/2,create/0,create/1,createBrush/2,createFont/2,
  createFont/3,createLinearGradientBrush/7,createMatrix/1,createMatrix/2,
  createPath/1,createPen/2,createRadialGradientBrush/8,drawBitmap/6,
  drawEllipse/5,drawIcon/6,drawLines/3,drawLines/4,drawPath/2,drawPath/3,
  drawRectangle/5,drawRoundedRectangle/6,drawText/4,drawText/5,drawText/6,
  fillPath/2,fillPath/3,getNativeContext/1,getPartialTextExtents/3,
  getTextExtent/2,getTransform/1,resetClip/1,rotate/2,scale/3,setBrush/2,
  setFont/2,setFont/3,setPen/2,setTransform/2,strokeLine/5,strokeLines/3,
  strokeLines/4,strokePath/2,translate/3]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

%% @hidden
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxGraphicsContext()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreate">external documentation</a>.
create() ->
  wxe_util:call(?wxGraphicsContext_Create_0,
  <<>>).

%% @spec (Dc::wxWindowDC:wxWindowDC() | wxWindow:wxWindow()) -> wxGraphicsContext()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreate">external documentation</a>.
create(#wx_ref{type=DcT,ref=DcRef}) ->
  DcOP = case ?CLASS_T(DcT,wxWindowDC) of
     true ->
       ?wxGraphicsContext_Create_1_1;
     _ -> ?CLASS(DcT,wxWindow),
       ?wxGraphicsContext_Create_1_0
     end,
  wxe_util:call(DcOP,
  <<DcRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Pen::wxPen:wxPen()) -> wxGraphicsPen:wxGraphicsPen()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreatepen">external documentation</a>.
createPen(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PenT,ref=PenRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PenT,wxPen),
  wxe_util:call(?wxGraphicsContext_CreatePen,
  <<ThisRef:32/?UI,PenRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Brush::wxBrush:wxBrush()) -> wxGraphicsBrush:wxGraphicsBrush()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreatebrush">external documentation</a>.
createBrush(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(BrushT,wxBrush),
  wxe_util:call(?wxGraphicsContext_CreateBrush,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Xo::float(), Yo::float(), Xc::float(), Yc::float(), Radius::float(), OColor::wx:colour(), CColor::wx:colour()) -> wxGraphicsBrush:wxGraphicsBrush()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreateradialgradientbrush">external documentation</a>.
createRadialGradientBrush(#wx_ref{type=ThisT,ref=ThisRef},Xo,Yo,Xc,Yc,Radius,OColor,CColor)
 when is_float(Xo),is_float(Yo),is_float(Xc),is_float(Yc),is_float(Radius),tuple_size(OColor) =:= 3; tuple_size(OColor) =:= 4,tuple_size(CColor) =:= 3; tuple_size(CColor) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:call(?wxGraphicsContext_CreateRadialGradientBrush,
  <<ThisRef:32/?UI,0:32,Xo:64/?F,Yo:64/?F,Xc:64/?F,Yc:64/?F,Radius:64/?F,(wxe_util:colour_bin(OColor)):16/binary,(wxe_util:colour_bin(CColor)):16/binary>>).

%% @spec (This::wxGraphicsContext(), X1::float(), Y1::float(), X2::float(), Y2::float(), C1::wx:colour(), C2::wx:colour()) -> wxGraphicsBrush:wxGraphicsBrush()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreatelineargradientbrush">external documentation</a>.
createLinearGradientBrush(#wx_ref{type=ThisT,ref=ThisRef},X1,Y1,X2,Y2,C1,C2)
 when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2),tuple_size(C1) =:= 3; tuple_size(C1) =:= 4,tuple_size(C2) =:= 3; tuple_size(C2) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:call(?wxGraphicsContext_CreateLinearGradientBrush,
  <<ThisRef:32/?UI,0:32,X1:64/?F,Y1:64/?F,X2:64/?F,Y2:64/?F,(wxe_util:colour_bin(C1)):16/binary,(wxe_util:colour_bin(C2)):16/binary>>).

%% @spec (This::wxGraphicsContext(), Font::wxFont:wxFont()) -> wxGraphicsFont:wxGraphicsFont()
%% @equiv createFont(This,Font, [])
createFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  createFont(This,Font, []).

%% @spec (This::wxGraphicsContext(), Font::wxFont:wxFont(), [Option]) -> wxGraphicsFont:wxGraphicsFont()
%% Option = {col, wx:colour()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreatefont">external documentation</a>.
createFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxFont),
  MOpts = fun({col, Col}, Acc) -> [<<1:32/?UI,(wxe_util:colour_bin(Col)):16/binary,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsContext_CreateFont,
  <<ThisRef:32/?UI,FontRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxGraphicsContext()) -> wxGraphicsMatrix:wxGraphicsMatrix()
%% @equiv createMatrix(This, [])
createMatrix(This)
 when is_record(This, wx_ref) ->
  createMatrix(This, []).

%% @spec (This::wxGraphicsContext(), [Option]) -> wxGraphicsMatrix:wxGraphicsMatrix()
%% Option = {a, float()} | {b, float()} | {c, float()} | {d, float()} | {tx, float()} | {ty, float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreatematrix">external documentation</a>.
createMatrix(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  MOpts = fun({a, A}, Acc) -> [<<1:32/?UI,0:32,A:64/?F>>|Acc];
          ({b, B}, Acc) -> [<<2:32/?UI,0:32,B:64/?F>>|Acc];
          ({c, C}, Acc) -> [<<3:32/?UI,0:32,C:64/?F>>|Acc];
          ({d, D}, Acc) -> [<<4:32/?UI,0:32,D:64/?F>>|Acc];
          ({tx, Tx}, Acc) -> [<<5:32/?UI,0:32,Tx:64/?F>>|Acc];
          ({ty, Ty}, Acc) -> [<<6:32/?UI,0:32,Ty:64/?F>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsContext_CreateMatrix,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxGraphicsContext()) -> wxGraphicsPath:wxGraphicsPath()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextcreatepath">external documentation</a>.
createPath(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:call(?wxGraphicsContext_CreatePath,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Region::wxRegion:wxRegion()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextclip">external documentation</a>.
clip(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RegionT,ref=RegionRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(RegionT,wxRegion),
  wxe_util:cast(?wxGraphicsContext_Clip_1,
  <<ThisRef:32/?UI,RegionRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), X::float(), Y::float(), W::float(), H::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextclip">external documentation</a>.
clip(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_float(X),is_float(Y),is_float(W),is_float(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_Clip_4,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @spec (This::wxGraphicsContext()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextresetclip">external documentation</a>.
resetClip(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_ResetClip,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Bmp::wxBitmap:wxBitmap(), X::float(), Y::float(), W::float(), H::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawbitmap">external documentation</a>.
drawBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BmpT,ref=BmpRef},X,Y,W,H)
 when is_float(X),is_float(Y),is_float(W),is_float(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:cast(?wxGraphicsContext_DrawBitmap,
  <<ThisRef:32/?UI,BmpRef:32/?UI,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @spec (This::wxGraphicsContext(), X::float(), Y::float(), W::float(), H::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawellipse">external documentation</a>.
drawEllipse(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_float(X),is_float(Y),is_float(W),is_float(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_DrawEllipse,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @spec (This::wxGraphicsContext(), Icon::wxIcon:wxIcon(), X::float(), Y::float(), W::float(), H::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawicon">external documentation</a>.
drawIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef},X,Y,W,H)
 when is_float(X),is_float(Y),is_float(W),is_float(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(IconT,wxIcon),
  wxe_util:cast(?wxGraphicsContext_DrawIcon,
  <<ThisRef:32/?UI,IconRef:32/?UI,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @spec (This::wxGraphicsContext(), N::integer(), Points::{X::float(),Y::float()}) -> ok
%% @equiv drawLines(This,N,Points, [])
drawLines(This,N,Points={PointsX,PointsY})
 when is_record(This, wx_ref),is_integer(N),is_number(PointsX),is_number(PointsY) ->
  drawLines(This,N,Points, []).

%% @spec (This::wxGraphicsContext(), N::integer(), Points::{X::float(),Y::float()}, [Option]) -> ok
%% Option = {fillStyle, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawlines">external documentation</a>.
drawLines(#wx_ref{type=ThisT,ref=ThisRef},N,{PointsX,PointsY}, Options)
 when is_integer(N),is_number(PointsX),is_number(PointsY),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGraphicsContext_DrawLines,
  <<ThisRef:32/?UI,N:32/?UI,PointsX:64/float,PointsY:64/float, BinOpt/binary>>).

%% @spec (This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath()) -> ok
%% @equiv drawPath(This,Path, [])
drawPath(This,Path)
 when is_record(This, wx_ref),is_record(Path, wx_ref) ->
  drawPath(This,Path, []).

%% @spec (This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath(), [Option]) -> ok
%% Option = {fillStyle, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawpath">external documentation</a>.
drawPath(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PathT,ref=PathRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGraphicsContext_DrawPath,
  <<ThisRef:32/?UI,PathRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxGraphicsContext(), X::float(), Y::float(), W::float(), H::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawrectangle">external documentation</a>.
drawRectangle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_float(X),is_float(Y),is_float(W),is_float(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_DrawRectangle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @spec (This::wxGraphicsContext(), X::float(), Y::float(), W::float(), H::float(), Radius::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawroundedrectangle">external documentation</a>.
drawRoundedRectangle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H,Radius)
 when is_float(X),is_float(Y),is_float(W),is_float(H),is_float(Radius) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_DrawRoundedRectangle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F,Radius:64/?F>>).

%% @spec (This::wxGraphicsContext(), Str::string(), X::float(), Y::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
drawText(#wx_ref{type=ThisT,ref=ThisRef},Str,X,Y)
 when is_list(Str),is_float(X),is_float(Y) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxGraphicsContext_DrawText_3,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8,X:64/?F,Y:64/?F>>).

%% @spec (This::wxGraphicsContext(),Str::string(),X::float(),Y::float(),X::float()|term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% drawText(This::wxGraphicsContext(), Str::string(), X::float(), Y::float(), Angle::float()) -> ok </c>
%% </p>
%% <p><c>
%% drawText(This::wxGraphicsContext(), Str::string(), X::float(), Y::float(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush()) -> ok </c>
%% </p>
drawText(#wx_ref{type=ThisT,ref=ThisRef},Str,X,Y,Angle)
 when is_list(Str),is_float(X),is_float(Y),is_float(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxGraphicsContext_DrawText_4_0,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8,X:64/?F,Y:64/?F,Angle:64/?F>>);
drawText(#wx_ref{type=ThisT,ref=ThisRef},Str,X,Y,#wx_ref{type=BackgroundBrushT,ref=BackgroundBrushRef})
 when is_list(Str),is_float(X),is_float(Y) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary([Str,0]),
  ?CLASS(BackgroundBrushT,wxGraphicsBrush),
  wxe_util:cast(?wxGraphicsContext_DrawText_4_1,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8,X:64/?F,Y:64/?F,BackgroundBrushRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Str::string(), X::float(), Y::float(), Angle::float(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
drawText(#wx_ref{type=ThisT,ref=ThisRef},Str,X,Y,Angle,#wx_ref{type=BackgroundBrushT,ref=BackgroundBrushRef})
 when is_list(Str),is_float(X),is_float(Y),is_float(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary([Str,0]),
  ?CLASS(BackgroundBrushT,wxGraphicsBrush),
  wxe_util:cast(?wxGraphicsContext_DrawText_5,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8,X:64/?F,Y:64/?F,Angle:64/?F,BackgroundBrushRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath()) -> ok
%% @equiv fillPath(This,Path, [])
fillPath(This,Path)
 when is_record(This, wx_ref),is_record(Path, wx_ref) ->
  fillPath(This,Path, []).

%% @spec (This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath(), [Option]) -> ok
%% Option = {fillStyle, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextfillpath">external documentation</a>.
fillPath(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PathT,ref=PathRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGraphicsContext_FillPath,
  <<ThisRef:32/?UI,PathRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextstrokepath">external documentation</a>.
strokePath(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PathT,ref=PathRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsContext_StrokePath,
  <<ThisRef:32/?UI,PathRef:32/?UI>>).

%% @spec (This::wxGraphicsContext()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextgetnativecontext">external documentation</a>.
getNativeContext(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_GetNativeContext,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Text::string(), Widths::[float()]) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextgetpartialtextextents">external documentation</a>.
getPartialTextExtents(#wx_ref{type=ThisT,ref=ThisRef},Text,Widths)
 when is_list(Text),is_list(Widths) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxGraphicsContext_GetPartialTextExtents,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,(length(Widths)):32/?UI,
0:32,  (<< <<C:64/float>> || C <- Widths>>)/binary>>).

%% @spec (This::wxGraphicsContext(), Text::string()) -> {Width::float(),Height::float(),Descent::float(),ExternalLeading::float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextgettextextent">external documentation</a>.
getTextExtent(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxGraphicsContext_GetTextExtent,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxGraphicsContext(), Angle::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextrotate">external documentation</a>.
rotate(#wx_ref{type=ThisT,ref=ThisRef},Angle)
 when is_float(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_Rotate,
  <<ThisRef:32/?UI,0:32,Angle:64/?F>>).

%% @spec (This::wxGraphicsContext(), XScale::float(), YScale::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextscale">external documentation</a>.
scale(#wx_ref{type=ThisT,ref=ThisRef},XScale,YScale)
 when is_float(XScale),is_float(YScale) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_Scale,
  <<ThisRef:32/?UI,0:32,XScale:64/?F,YScale:64/?F>>).

%% @spec (This::wxGraphicsContext(), Dx::float(), Dy::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontexttranslate">external documentation</a>.
translate(#wx_ref{type=ThisT,ref=ThisRef},Dx,Dy)
 when is_float(Dx),is_float(Dy) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_Translate,
  <<ThisRef:32/?UI,0:32,Dx:64/?F,Dy:64/?F>>).

%% @spec (This::wxGraphicsContext()) -> wxGraphicsMatrix:wxGraphicsMatrix()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextgettransform">external documentation</a>.
getTransform(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:call(?wxGraphicsContext_GetTransform,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextsettransform">external documentation</a>.
setTransform(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MatrixT,ref=MatrixRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsContext_SetTransform,
  <<ThisRef:32/?UI,MatrixRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextconcattransform">external documentation</a>.
concatTransform(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MatrixT,ref=MatrixRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsContext_ConcatTransform,
  <<ThisRef:32/?UI,MatrixRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Brush::wxGraphicsBrush:wxGraphicsBrush() | wxBrush:wxBrush()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextsetbrush">external documentation</a>.
setBrush(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  BrushOP = case ?CLASS_T(BrushT,wxGraphicsBrush) of
     true ->
       ?wxGraphicsContext_SetBrush_1_1;
     _ -> ?CLASS(BrushT,wxBrush),
       ?wxGraphicsContext_SetBrush_1_0
     end,
  wxe_util:cast(BrushOP,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Font::wxGraphicsFont:wxGraphicsFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextsetfont">external documentation</a>.
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxGraphicsFont),
  wxe_util:cast(?wxGraphicsContext_SetFont_1,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), Font::wxFont:wxFont(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextsetfont">external documentation</a>.
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxGraphicsContext_SetFont_2,
  <<ThisRef:32/?UI,FontRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxGraphicsContext(), Pen::wxPen:wxPen() | wxGraphicsPen:wxGraphicsPen()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextsetpen">external documentation</a>.
setPen(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PenT,ref=PenRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  PenOP = case ?CLASS_T(PenT,wxPen) of
     true ->
       ?wxGraphicsContext_SetPen_1_1;
     _ -> ?CLASS(PenT,wxGraphicsPen),
       ?wxGraphicsContext_SetPen_1_0
     end,
  wxe_util:cast(PenOP,
  <<ThisRef:32/?UI,PenRef:32/?UI>>).

%% @spec (This::wxGraphicsContext(), X1::float(), Y1::float(), X2::float(), Y2::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextstrokeline">external documentation</a>.
strokeLine(#wx_ref{type=ThisT,ref=ThisRef},X1,Y1,X2,Y2)
 when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_StrokeLine,
  <<ThisRef:32/?UI,0:32,X1:64/?F,Y1:64/?F,X2:64/?F,Y2:64/?F>>).

%% @spec (This::wxGraphicsContext(), N::integer(), Points::{X::float(),Y::float()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextstrokelines">external documentation</a>.
strokeLines(#wx_ref{type=ThisT,ref=ThisRef},N,{PointsX,PointsY})
 when is_integer(N),is_number(PointsX),is_number(PointsY) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_StrokeLines_2,
  <<ThisRef:32/?UI,N:32/?UI,PointsX:64/float,PointsY:64/float>>).

%% @spec (This::wxGraphicsContext(), N::integer(), BeginPoints::{X::float(),Y::float()}, EndPoints::{X::float(),Y::float()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicscontext.html#wxgraphicscontextstrokelines">external documentation</a>.
strokeLines(#wx_ref{type=ThisT,ref=ThisRef},N,{BeginPointsX,BeginPointsY},{EndPointsX,EndPointsY})
 when is_integer(N),is_number(BeginPointsX),is_number(BeginPointsY),is_number(EndPointsX),is_number(EndPointsY) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_StrokeLines_3,
  <<ThisRef:32/?UI,N:32/?UI,BeginPointsX:64/float,BeginPointsY:64/float,EndPointsX:64/float,EndPointsY:64/float>>).

 %% From wxGraphicsObject
%% @hidden
isNull(This) -> wxGraphicsObject:isNull(This).
%% @hidden
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
