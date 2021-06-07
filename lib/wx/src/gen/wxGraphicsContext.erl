%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
%% @hidden
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreate">external documentation</a>.
-spec create() -> wxGraphicsContext().
create() ->
  wxe_util:queue_cmd(?get_env(), ?wxGraphicsContext_Create_STAT_0),
  wxe_util:rec(?wxGraphicsContext_Create_STAT_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreate">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatepen">external documentation</a>.
-spec createPen(This, Pen) -> wxGraphicsPen:wxGraphicsPen() when
	This::wxGraphicsContext(), Pen::wxPen:wxPen().
createPen(#wx_ref{type=ThisT}=This,#wx_ref{type=PenT}=Pen) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PenT,wxPen),
  wxe_util:queue_cmd(This,Pen,?get_env(),?wxGraphicsContext_CreatePen),
  wxe_util:rec(?wxGraphicsContext_CreatePen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatebrush">external documentation</a>.
-spec createBrush(This, Brush) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), Brush::wxBrush:wxBrush().
createBrush(#wx_ref{type=ThisT}=This,#wx_ref{type=BrushT}=Brush) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(BrushT,wxBrush),
  wxe_util:queue_cmd(This,Brush,?get_env(),?wxGraphicsContext_CreateBrush),
  wxe_util:rec(?wxGraphicsContext_CreateBrush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreateradialgradientbrush">external documentation</a>.
-spec createRadialGradientBrush(This, StartX, StartY, EndX, EndY, Radius, Stops) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), StartX::number(), StartY::number(), EndX::number(), EndY::number(), Radius::number(), Stops::wxGraphicsGradientStops:wxGraphicsGradientStops().
createRadialGradientBrush(#wx_ref{type=ThisT}=This,StartX,StartY,EndX,EndY,Radius,#wx_ref{type=StopsT}=Stops)
 when is_number(StartX),is_number(StartY),is_number(EndX),is_number(EndY),is_number(Radius) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(StopsT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,StartX,StartY,EndX,EndY,Radius,Stops,?get_env(),?wxGraphicsContext_CreateRadialGradientBrush_6),
  wxe_util:rec(?wxGraphicsContext_CreateRadialGradientBrush_6).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreateradialgradientbrush">external documentation</a>.
-spec createRadialGradientBrush(This, StartX, StartY, EndX, EndY, Radius, OColor, CColor) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), StartX::number(), StartY::number(), EndX::number(), EndY::number(), Radius::number(), OColor::wx:wx_colour(), CColor::wx:wx_colour().
createRadialGradientBrush(#wx_ref{type=ThisT}=This,StartX,StartY,EndX,EndY,Radius,OColor,CColor)
 when is_number(StartX),is_number(StartY),is_number(EndX),is_number(EndY),is_number(Radius),tuple_size(OColor) =:= 3; tuple_size(OColor) =:= 4,tuple_size(CColor) =:= 3; tuple_size(CColor) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,StartX,StartY,EndX,EndY,Radius,wxe_util:color(OColor),wxe_util:color(CColor),?get_env(),?wxGraphicsContext_CreateRadialGradientBrush_7),
  wxe_util:rec(?wxGraphicsContext_CreateRadialGradientBrush_7).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatelineargradientbrush">external documentation</a>.
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, Stops) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), X1::number(), Y1::number(), X2::number(), Y2::number(), Stops::wxGraphicsGradientStops:wxGraphicsGradientStops().
createLinearGradientBrush(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2,#wx_ref{type=StopsT}=Stops)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(StopsT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,Stops,?get_env(),?wxGraphicsContext_CreateLinearGradientBrush_5),
  wxe_util:rec(?wxGraphicsContext_CreateLinearGradientBrush_5).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatelineargradientbrush">external documentation</a>.
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, C1, C2) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsContext(), X1::number(), Y1::number(), X2::number(), Y2::number(), C1::wx:wx_colour(), C2::wx:wx_colour().
createLinearGradientBrush(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2,C1,C2)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2),tuple_size(C1) =:= 3; tuple_size(C1) =:= 4,tuple_size(C2) =:= 3; tuple_size(C2) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,wxe_util:color(C1),wxe_util:color(C2),?get_env(),?wxGraphicsContext_CreateLinearGradientBrush_6),
  wxe_util:rec(?wxGraphicsContext_CreateLinearGradientBrush_6).

%% @equiv createFont(This,Font, [])
-spec createFont(This, Font) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsContext(), Font::wxFont:wxFont().

createFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  createFont(This,Font, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatefont">external documentation</a>.
%% <br /> Also:<br />
%% createFont(This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont() when<br />
%% 	This::wxGraphicsContext(), Font::wxFont:wxFont(),<br />
%% 	Option :: {'col', wx:wx_colour()}.<br />
%% 
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatefont">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextcreatepath">external documentation</a>.
-spec createPath(This) -> wxGraphicsPath:wxGraphicsPath() when
	This::wxGraphicsContext().
createPath(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsContext_CreatePath),
  wxe_util:rec(?wxGraphicsContext_CreatePath).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextclip">external documentation</a>.
-spec clip(This, Region) -> 'ok' when
	This::wxGraphicsContext(), Region::wxRegion:wxRegion().
clip(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(RegionT,wxRegion),
  wxe_util:queue_cmd(This,Region,?get_env(),?wxGraphicsContext_Clip_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextclip">external documentation</a>.
-spec clip(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
clip(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxGraphicsContext_Clip_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextresetclip">external documentation</a>.
-spec resetClip(This) -> 'ok' when
	This::wxGraphicsContext().
resetClip(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsContext_ResetClip).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawbitmap">external documentation</a>.
-spec drawBitmap(This, Bmp, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), Bmp::wxBitmap:wxBitmap(), X::number(), Y::number(), W::number(), H::number().
drawBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Bmp,X,Y,W,H,?get_env(),?wxGraphicsContext_DrawBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawellipse">external documentation</a>.
-spec drawEllipse(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
drawEllipse(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxGraphicsContext_DrawEllipse).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawicon">external documentation</a>.
-spec drawIcon(This, Icon, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), Icon::wxIcon:wxIcon(), X::number(), Y::number(), W::number(), H::number().
drawIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,X,Y,W,H,?get_env(),?wxGraphicsContext_DrawIcon).

%% @equiv drawLines(This,Points, [])
-spec drawLines(This, Points) -> 'ok' when
	This::wxGraphicsContext(), Points::[{X::float(), Y::float()}].

drawLines(This,Points)
 when is_record(This, wx_ref),is_list(Points) ->
  drawLines(This,Points, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawlines">external documentation</a>.
%%<br /> FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
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

%% @equiv drawPath(This,Path, [])
-spec drawPath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().

drawPath(This,Path)
 when is_record(This, wx_ref),is_record(Path, wx_ref) ->
  drawPath(This,Path, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawpath">external documentation</a>.
%%<br /> FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawrectangle">external documentation</a>.
-spec drawRectangle(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number().
drawRectangle(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxGraphicsContext_DrawRectangle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawroundedrectangle">external documentation</a>.
-spec drawRoundedRectangle(This, X, Y, W, H, Radius) -> 'ok' when
	This::wxGraphicsContext(), X::number(), Y::number(), W::number(), H::number(), Radius::number().
drawRoundedRectangle(#wx_ref{type=ThisT}=This,X,Y,W,H,Radius)
 when is_number(X),is_number(Y),is_number(W),is_number(H),is_number(Radius) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X,Y,W,H,Radius,?get_env(),?wxGraphicsContext_DrawRoundedRectangle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
-spec drawText(This, Str, X, Y) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number().
drawText(#wx_ref{type=ThisT}=This,Str,X,Y)
 when ?is_chardata(Str),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary(Str),
  wxe_util:queue_cmd(This,Str_UC,X,Y,?get_env(),?wxGraphicsContext_DrawText_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
%% <br /> Also:<br />
%% drawText(This, Str, X, Y, BackgroundBrush) -> 'ok' when<br />
%% 	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush().<br />
%% 
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextdrawtext">external documentation</a>.
-spec drawText(This, Str, X, Y, Angle, BackgroundBrush) -> 'ok' when
	This::wxGraphicsContext(), Str::unicode:chardata(), X::number(), Y::number(), Angle::number(), BackgroundBrush::wxGraphicsBrush:wxGraphicsBrush().
drawText(#wx_ref{type=ThisT}=This,Str,X,Y,Angle,#wx_ref{type=BackgroundBrushT}=BackgroundBrush)
 when ?is_chardata(Str),is_number(X),is_number(Y),is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Str_UC = unicode:characters_to_binary(Str),
  ?CLASS(BackgroundBrushT,wxGraphicsBrush),
  wxe_util:queue_cmd(This,Str_UC,X,Y,Angle,BackgroundBrush,?get_env(),?wxGraphicsContext_DrawText_5).

%% @equiv fillPath(This,Path, [])
-spec fillPath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().

fillPath(This,Path)
 when is_record(This, wx_ref),is_record(Path, wx_ref) ->
  fillPath(This,Path, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextfillpath">external documentation</a>.
%%<br /> FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextstrokepath">external documentation</a>.
-spec strokePath(This, Path) -> 'ok' when
	This::wxGraphicsContext(), Path::wxGraphicsPath:wxGraphicsPath().
strokePath(#wx_ref{type=ThisT}=This,#wx_ref{type=PathT}=Path) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(PathT,wxGraphicsPath),
  wxe_util:queue_cmd(This,Path,?get_env(),?wxGraphicsContext_StrokePath).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextgetpartialtextextents">external documentation</a>.
-spec getPartialTextExtents(This, Text) -> [number()] when
	This::wxGraphicsContext(), Text::unicode:chardata().
getPartialTextExtents(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxGraphicsContext_GetPartialTextExtents),
  wxe_util:rec(?wxGraphicsContext_GetPartialTextExtents).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextgettextextent">external documentation</a>.
-spec getTextExtent(This, Text) -> Result when
	Result ::{Width::number(), Height::number(), Descent::number(), ExternalLeading::number()},
	This::wxGraphicsContext(), Text::unicode:chardata().
getTextExtent(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxGraphicsContext),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxGraphicsContext_GetTextExtent),
  wxe_util:rec(?wxGraphicsContext_GetTextExtent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextrotate">external documentation</a>.
-spec rotate(This, Angle) -> 'ok' when
	This::wxGraphicsContext(), Angle::number().
rotate(#wx_ref{type=ThisT}=This,Angle)
 when is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,Angle,?get_env(),?wxGraphicsContext_Rotate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextscale">external documentation</a>.
-spec scale(This, XScale, YScale) -> 'ok' when
	This::wxGraphicsContext(), XScale::number(), YScale::number().
scale(#wx_ref{type=ThisT}=This,XScale,YScale)
 when is_number(XScale),is_number(YScale) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,XScale,YScale,?get_env(),?wxGraphicsContext_Scale).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontexttranslate">external documentation</a>.
-spec translate(This, Dx, Dy) -> 'ok' when
	This::wxGraphicsContext(), Dx::number(), Dy::number().
translate(#wx_ref{type=ThisT}=This,Dx,Dy)
 when is_number(Dx),is_number(Dy) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,Dx,Dy,?get_env(),?wxGraphicsContext_Translate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextgettransform">external documentation</a>.
-spec getTransform(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsContext().
getTransform(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsContext_GetTransform),
  wxe_util:rec(?wxGraphicsContext_GetTransform).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsettransform">external documentation</a>.
-spec setTransform(This, Matrix) -> 'ok' when
	This::wxGraphicsContext(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix().
setTransform(#wx_ref{type=ThisT}=This,#wx_ref{type=MatrixT}=Matrix) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Matrix,?get_env(),?wxGraphicsContext_SetTransform).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextconcattransform">external documentation</a>.
-spec concatTransform(This, Matrix) -> 'ok' when
	This::wxGraphicsContext(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix().
concatTransform(#wx_ref{type=ThisT}=This,#wx_ref{type=MatrixT}=Matrix) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Matrix,?get_env(),?wxGraphicsContext_ConcatTransform).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsetbrush">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxGraphicsContext(), Font::wxGraphicsFont:wxGraphicsFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxGraphicsFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxGraphicsContext_SetFont_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsetfont">external documentation</a>.
-spec setFont(This, Font, Colour) -> 'ok' when
	This::wxGraphicsContext(), Font::wxFont:wxFont(), Colour::wx:wx_colour().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsContext),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,wxe_util:color(Colour),?get_env(),?wxGraphicsContext_SetFont_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextsetpen">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextstrokeline">external documentation</a>.
-spec strokeLine(This, X1, Y1, X2, Y2) -> 'ok' when
	This::wxGraphicsContext(), X1::number(), Y1::number(), X2::number(), Y2::number().
strokeLine(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,?get_env(),?wxGraphicsContext_StrokeLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicscontext.html#wxgraphicscontextstrokelines">external documentation</a>.
-spec strokeLines(This, Points) -> 'ok' when
	This::wxGraphicsContext(), Points::[{X::float(), Y::float()}].
strokeLines(#wx_ref{type=ThisT}=This,Points)
 when is_list(Points) ->
  ?CLASS(ThisT,wxGraphicsContext),
  wxe_util:queue_cmd(This,Points,?get_env(),?wxGraphicsContext_StrokeLines).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGraphicsContext()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGraphicsContext),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxGraphicsObject
%% @hidden
isNull(This) -> wxGraphicsObject:isNull(This).
%% @hidden
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
