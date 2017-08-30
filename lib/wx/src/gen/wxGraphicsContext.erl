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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html">wxGraphicsContext</a>.
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
  createPath/1,createPen/2,createRadialGradientBrush/8,destroy/1,drawBitmap/6,
  drawEllipse/5,drawIcon/6,drawLines/2,drawLines/3,drawPath/2,drawPath/3,
  drawRectangle/5,drawRoundedRectangle/6,drawText/4,drawText/5,drawText/6,
  fillPath/2,fillPath/3,getPartialTextExtents/2,getTextExtent/2,getTransform/1,
  resetClip/1,rotate/2,scale/3,setBrush/2,setFont/2,setFont/3,setPen/2,
  setTransform/2,strokeLine/5,strokeLines/2,strokePath/2,translate/3]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

-export_type([wxGraphicsContext/0]).
-deprecated([createLinearGradientBrush/7,createRadialGradientBrush/8]).

%% @hidden
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGraphicsContext() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreate">external documentation</a>.
-spec create() -> wxGraphicsContext().
create() ->
  wxe_util:call(?wxGraphicsContext_Create_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreate">external documentation</a>.
-spec create(Dc) -> wxGraphicsContext() when
	Dc::wxWindowDC:wxWindowDC() | wxWindow:wxWindow().
create(#wx_ref{type=DcT,ref=DcRef}) ->
  DcOP = case ?CLASS_T(DcT,wxWindowDC) of
     true ->
       ?wxGraphicsContext_Create_1_1;
     _ -> ?CLASS(DcT,wxWindow),
       ?wxGraphicsContext_Create_1_0
     end,
  wxe_util:call(DcOP,
  <<DcRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatepen">external documentation</a>.
-spec createPen(This, Pen) -> wxGraphicsPen:wxGraphicsPen() when
	This::wxGraphicsContext(), Pen::wxPen:wxPen().
createPen(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PenT,ref=PenRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PenT,wxPen),
  wxe_util:call(?wxGraphicsContext_CreatePen,
  <<ThisRef:32/?UI,PenRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatebrush">external documentation</a>.
-spec createBrush(This, Brush) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), Brush::wxBrush:wxBrush().
createBrush(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(BrushT,wxBrush),
  wxe_util:call(?wxGraphicsContext_CreateBrush,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreateradialgradientbrush">external documentation</a>.
-spec createRadialGradientBrush(This, Xo, Yo, Xc, Yc, Radius, OColor, CColor) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), Xo::number(), Yo::number(), Xc::number(), Yc::number(), Radius::number(), OColor::wx:wx_colour(), CColor::wx:wx_colour().
createRadialGradientBrush(#wx_ref{type=ThisT,ref=ThisRef},Xo,Yo,Xc,Yc,Radius,OColor,CColor)
 when is_number(Xo),is_number(Yo),is_number(Xc),is_number(Yc),is_number(Radius),tuple_size(OColor) =:= 3; tuple_size(OColor) =:= 4,tuple_size(CColor) =:= 3; tuple_size(CColor) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:call(?wxGraphicsContext_CreateRadialGradientBrush,
  <<ThisRef:32/?UI,0:32,Xo:64/?F,Yo:64/?F,Xc:64/?F,Yc:64/?F,Radius:64/?F,(wxe_util:colour_bin(OColor)):16/binary,(wxe_util:colour_bin(CColor)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatelineargradientbrush">external documentation</a>.
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, C1, C2) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), X1::number(), Y1::number(), X2::number(), Y2::number(), C1::wx:wx_colour(), C2::wx:wx_colour().
createLinearGradientBrush(#wx_ref{type=ThisT,ref=ThisRef},X1,Y1,X2,Y2,C1,C2)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2),tuple_size(C1) =:= 3; tuple_size(C1) =:= 4,tuple_size(C2) =:= 3; tuple_size(C2) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:call(?wxGraphicsContext_CreateLinearGradientBrush,
  <<ThisRef:32/?UI,0:32,X1:64/?F,Y1:64/?F,X2:64/?F,Y2:64/?F,(wxe_util:colour_bin(C1)):16/binary,(wxe_util:colour_bin(C2)):16/binary>>).

%% @equiv createFont(This,Font, [])
-spec createFont(This, Font) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsContext(), Font::wxFont:wxFont().

createFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  createFont(This,Font, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatefont">external documentation</a>.
-spec createFont(This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsContext(), Font::wxFont:wxFont(),
	Option :: {'col', wx:wx_colour()}.
createFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxFont),
  MOpts = fun({col, Col}, Acc) -> [<<1:32/?UI,(wxe_util:colour_bin(Col)):16/binary,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsContext_CreateFont,
  <<ThisRef:32/?UI,FontRef:32/?UI, BinOpt/binary>>).

%% @equiv createMatrix(This, [])
-spec createMatrix(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsContext().

createMatrix(This)
 when is_record(This, wx_ref) ->
  createMatrix(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatematrix">external documentation</a>.
-spec createMatrix(This, [Option]) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsContext(),
	Option :: {'a', number()}
		 | {'b', number()}
		 | {'c', number()}
		 | {'d', number()}
		 | {'tx', number()}
		 | {'ty', number()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatepath">external documentation</a>.
-spec createPath(This) -> wxGraphicsPath:wxGraphicsPath() when
	This::wxGraphicsContext().
createPath(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:call(?wxGraphicsContext_CreatePath,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextclip">external documentation</a>.
-spec clip(This, Region) -> 'ok' when
	This::wxGraphicsContext(), Region::wxRegion:wxRegion().
clip(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RegionT,ref=RegionRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(RegionT,wxRegion),
  wxe_util:cast(?wxGraphicsContext_Clip_1,
  <<ThisRef:32/?UI,RegionRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextclip">external documentation</a>.
-spec clip(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
clip(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_Clip_4,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextresetclip">external documentation</a>.
-spec resetClip(This) -> 'ok' when
	This::wxGraphicsContext().
resetClip(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_ResetClip,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawbitmap">external documentation</a>.
-spec drawBitmap(This, Bmp, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), Bmp::wxBitmap:wxBitmap(), X::number(), Y::number(), W::number(), H::number().
drawBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BmpT,ref=BmpRef},X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:cast(?wxGraphicsContext_DrawBitmap,
  <<ThisRef:32/?UI,BmpRef:32/?UI,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawellipse">external documentation</a>.
-spec drawEllipse(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
drawEllipse(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_DrawEllipse,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawicon">external documentation</a>.
-spec drawIcon(This, Icon, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), Icon::wxIcon:wxIcon(), X::number(), Y::number(), W::number(), H::number().
drawIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef},X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(IconT,wxIcon),
  wxe_util:cast(?wxGraphicsContext_DrawIcon,
  <<ThisRef:32/?UI,IconRef:32/?UI,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @equiv drawLines(This,Points, [])
-spec drawLines(This, Points) -> 'ok' when
	This::wxGraphicsContext(), Points::[{X::float(), Y::float()}].

drawLines(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawLines(This,Points, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawlines">external documentation</a>.
%%<br /> FillStyle = integer
-spec drawLines(This, Points, [Option]) -> 'ok' when
	This::wxGraphicsContext(), Points::[{X::float(), Y::float()}],
	Option :: {'fillStyle', wx:wx_enum()}.
drawLines(#wx_ref{type=ThisT,ref=ThisRef},Points, Options)
 when is_list(Points),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGraphicsContext_DrawLines,
  <<ThisRef:32/?UI,(length(Points)):32/?UI,
    (<< <<X:64/?F,Y:64/?F>> || {X,Y} <- Points>>)/binary, BinOpt/binary>>).

%% @equiv drawPath(This,Path, [])
-spec drawPath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().

drawPath(This,Path)
 when is_record(This, wx_ref),is_record(Path, wx_ref) ->
  drawPath(This,Path, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawpath">external documentation</a>.
%%<br /> FillStyle = integer
-spec drawPath(This, Path, [Option]) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath(),
	Option :: {'fillStyle', wx:wx_enum()}.
drawPath(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PathT,ref=PathRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGraphicsContext_DrawPath,
  <<ThisRef:32/?UI,PathRef:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawrectangle">external documentation</a>.
-spec drawRectangle(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
drawRectangle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_DrawRectangle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawroundedrectangle">external documentation</a>.
-spec drawRoundedRectangle(This, X, Y, W, H, Radius) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number(), Radius::number().
drawRoundedRectangle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H,Radius)
 when is_number(X),is_number(Y),is_number(W),is_number(H),is_number(Radius) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_DrawRoundedRectangle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F,Radius:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
-spec drawText(This, Str, X, Y) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number().
drawText(#wx_ref{type=ThisT,ref=ThisRef},Str,X,Y)
 when is_list(Str),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxGraphicsContext_DrawText_3,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8,X:64/?F,Y:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
%% <br /> Also:<br />
%% drawText(This, Str, X, Y, BackgroundBrush) -> 'ok' when<br />
%% 	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush().<br />
%% 
-spec drawText(This, Str, X, Y, Angle) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), Angle::number();
      (This, Str, X, Y, BackgroundBrush) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush().
drawText(#wx_ref{type=ThisT,ref=ThisRef},Str,X,Y,Angle)
 when is_list(Str),is_number(X),is_number(Y),is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxGraphicsContext_DrawText_4_0,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8,X:64/?F,Y:64/?F,Angle:64/?F>>);
drawText(#wx_ref{type=ThisT,ref=ThisRef},Str,X,Y,#wx_ref{type=BackgroundBrushT,ref=BackgroundBrushRef})
 when is_list(Str),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary([Str,0]),
  ?CLASS(BackgroundBrushT,wxGraphicsBrush),
  wxe_util:cast(?wxGraphicsContext_DrawText_4_1,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8,X:64/?F,Y:64/?F,BackgroundBrushRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
-spec drawText(This, Str, X, Y, Angle, BackgroundBrush) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), Angle::number(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush().
drawText(#wx_ref{type=ThisT,ref=ThisRef},Str,X,Y,Angle,#wx_ref{type=BackgroundBrushT,ref=BackgroundBrushRef})
 when is_list(Str),is_number(X),is_number(Y),is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary([Str,0]),
  ?CLASS(BackgroundBrushT,wxGraphicsBrush),
  wxe_util:cast(?wxGraphicsContext_DrawText_5,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8,X:64/?F,Y:64/?F,Angle:64/?F,BackgroundBrushRef:32/?UI>>).

%% @equiv fillPath(This,Path, [])
-spec fillPath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().

fillPath(This,Path)
 when is_record(This, wx_ref),is_record(Path, wx_ref) ->
  fillPath(This,Path, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextfillpath">external documentation</a>.
%%<br /> FillStyle = integer
-spec fillPath(This, Path, [Option]) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath(),
	Option :: {'fillStyle', wx:wx_enum()}.
fillPath(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PathT,ref=PathRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGraphicsContext_FillPath,
  <<ThisRef:32/?UI,PathRef:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextstrokepath">external documentation</a>.
-spec strokePath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().
strokePath(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PathT,ref=PathRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsContext_StrokePath,
  <<ThisRef:32/?UI,PathRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextgetpartialtextextents">external documentation</a>.
-spec getPartialTextExtents(This, Text) -> [number()] when
	This::wxGraphicsContext(), Text::unicode:chardata().
getPartialTextExtents(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxGraphicsContext_GetPartialTextExtents,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextgettextextent">external documentation</a>.
-spec getTextExtent(This, Text) -> Result when
	Result ::{Width::number(), Height::number(), Descent::number(), ExternalLeading::number()},
	This::wxGraphicsContext(), Text::unicode:chardata().
getTextExtent(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxGraphicsContext_GetTextExtent,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextrotate">external documentation</a>.
-spec rotate(This, Angle) -> 'ok' when
	This::wxGraphicsContext(), Angle::number().
rotate(#wx_ref{type=ThisT,ref=ThisRef},Angle)
 when is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_Rotate,
  <<ThisRef:32/?UI,0:32,Angle:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextscale">external documentation</a>.
-spec scale(This, XScale, YScale) -> 'ok' when
	This::wxGraphicsContext(), XScale::number(), YScale::number().
scale(#wx_ref{type=ThisT,ref=ThisRef},XScale,YScale)
 when is_number(XScale),is_number(YScale) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_Scale,
  <<ThisRef:32/?UI,0:32,XScale:64/?F,YScale:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontexttranslate">external documentation</a>.
-spec translate(This, Dx, Dy) -> 'ok' when
	This::wxGraphicsContext(), Dx::number(), Dy::number().
translate(#wx_ref{type=ThisT,ref=ThisRef},Dx,Dy)
 when is_number(Dx),is_number(Dy) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_Translate,
  <<ThisRef:32/?UI,0:32,Dx:64/?F,Dy:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextgettransform">external documentation</a>.
-spec getTransform(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsContext().
getTransform(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:call(?wxGraphicsContext_GetTransform,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsettransform">external documentation</a>.
-spec setTransform(This, Matrix) -> 'ok' when
	This::wxGraphicsContext(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix().
setTransform(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MatrixT,ref=MatrixRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsContext_SetTransform,
  <<ThisRef:32/?UI,MatrixRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextconcattransform">external documentation</a>.
-spec concatTransform(This, Matrix) -> 'ok' when
	This::wxGraphicsContext(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix().
concatTransform(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MatrixT,ref=MatrixRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsContext_ConcatTransform,
  <<ThisRef:32/?UI,MatrixRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsetbrush">external documentation</a>.
-spec setBrush(This, Brush) -> 'ok' when
	This::wxGraphicsContext(), Brush::wxGraphicsBrush:wxGraphicsBrush() | wxBrush:wxBrush().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxGraphicsContext(), Font::wxGraphicsFont:wxGraphicsFont().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxGraphicsFont),
  wxe_util:cast(?wxGraphicsContext_SetFont_1,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsetfont">external documentation</a>.
-spec setFont(This, Font, Colour) -> 'ok' when
	This::wxGraphicsContext(), Font::wxFont:wxFont(), Colour::wx:wx_colour().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxGraphicsContext_SetFont_2,
  <<ThisRef:32/?UI,FontRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsetpen">external documentation</a>.
-spec setPen(This, Pen) -> 'ok' when
	This::wxGraphicsContext(), Pen::wxPen:wxPen() | wxGraphicsPen:wxGraphicsPen().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextstrokeline">external documentation</a>.
-spec strokeLine(This, X1, Y1, X2, Y2) -> 'ok' when
	This::wxGraphicsContext(), X1::number(), Y1::number(), X2::number(), Y2::number().
strokeLine(#wx_ref{type=ThisT,ref=ThisRef},X1,Y1,X2,Y2)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_StrokeLine,
  <<ThisRef:32/?UI,0:32,X1:64/?F,Y1:64/?F,X2:64/?F,Y2:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextstrokelines">external documentation</a>.
-spec strokeLines(This, Points) -> 'ok' when
	This::wxGraphicsContext(), Points::[{X::float(), Y::float()}].
strokeLines(#wx_ref{type=ThisT,ref=ThisRef},Points)
 when is_list(Points) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:cast(?wxGraphicsContext_StrokeLines,
  <<ThisRef:32/?UI,(length(Points)):32/?UI,
    (<< <<X:64/?F,Y:64/?F>> || {X,Y} <- Points>>)/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGraphicsContext()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGraphicsContext),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxGraphicsObject
%% @hidden
isNull(This) -> wxGraphicsObject:isNull(This).
%% @hidden
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
