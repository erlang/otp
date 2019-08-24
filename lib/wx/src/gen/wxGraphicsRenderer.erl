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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html">wxGraphicsRenderer</a>.
%% @type wxGraphicsRenderer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGraphicsRenderer).
-include("wxe.hrl").
-export([createBrush/2,createContext/2,createFont/2,createFont/3,createLinearGradientBrush/7,
  createMatrix/1,createMatrix/2,createPath/1,createPen/2,createRadialGradientBrush/8,
  getDefaultRenderer/0]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxGraphicsRenderer/0]).
-deprecated([createLinearGradientBrush/7,createRadialGradientBrush/8]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGraphicsRenderer() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderergetdefaultrenderer">external documentation</a>.
-spec getDefaultRenderer() -> wxGraphicsRenderer().
getDefaultRenderer() ->
  wxe_util:call(?wxGraphicsRenderer_GetDefaultRenderer,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatecontext">external documentation</a>.
-spec createContext(This, Dc) -> wxGraphicsContext:wxGraphicsContext() when
	This::wxGraphicsRenderer(), Dc::wxWindowDC:wxWindowDC() | wxWindow:wxWindow().
createContext(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DcT,ref=DcRef}) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  DcOP = case ?CLASS_T(DcT,wxWindowDC) of
     true ->
       ?wxGraphicsRenderer_CreateContext_1_1;
     _ -> ?CLASS(DcT,wxWindow),
       ?wxGraphicsRenderer_CreateContext_1_0
     end,
  wxe_util:call(DcOP,
  <<ThisRef:32/?UI,DcRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatepen">external documentation</a>.
-spec createPen(This, Pen) -> wxGraphicsPen:wxGraphicsPen() when
	This::wxGraphicsRenderer(), Pen::wxPen:wxPen().
createPen(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PenT,ref=PenRef}) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(PenT,wxPen),
  wxe_util:call(?wxGraphicsRenderer_CreatePen,
  <<ThisRef:32/?UI,PenRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatebrush">external documentation</a>.
-spec createBrush(This, Brush) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), Brush::wxBrush:wxBrush().
createBrush(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(BrushT,wxBrush),
  wxe_util:call(?wxGraphicsRenderer_CreateBrush,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatelineargradientbrush">external documentation</a>.
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, C1, C2) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), X1::number(), Y1::number(), X2::number(), Y2::number(), C1::wx:wx_colour(), C2::wx:wx_colour().
createLinearGradientBrush(#wx_ref{type=ThisT,ref=ThisRef},X1,Y1,X2,Y2,C1,C2)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2),tuple_size(C1) =:= 3; tuple_size(C1) =:= 4,tuple_size(C2) =:= 3; tuple_size(C2) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:call(?wxGraphicsRenderer_CreateLinearGradientBrush,
  <<ThisRef:32/?UI,0:32,X1:64/?F,Y1:64/?F,X2:64/?F,Y2:64/?F,(wxe_util:colour_bin(C1)):16/binary,(wxe_util:colour_bin(C2)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreateradialgradientbrush">external documentation</a>.
-spec createRadialGradientBrush(This, Xo, Yo, Xc, Yc, Radius, OColor, CColor) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), Xo::number(), Yo::number(), Xc::number(), Yc::number(), Radius::number(), OColor::wx:wx_colour(), CColor::wx:wx_colour().
createRadialGradientBrush(#wx_ref{type=ThisT,ref=ThisRef},Xo,Yo,Xc,Yc,Radius,OColor,CColor)
 when is_number(Xo),is_number(Yo),is_number(Xc),is_number(Yc),is_number(Radius),tuple_size(OColor) =:= 3; tuple_size(OColor) =:= 4,tuple_size(CColor) =:= 3; tuple_size(CColor) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:call(?wxGraphicsRenderer_CreateRadialGradientBrush,
  <<ThisRef:32/?UI,0:32,Xo:64/?F,Yo:64/?F,Xc:64/?F,Yc:64/?F,Radius:64/?F,(wxe_util:colour_bin(OColor)):16/binary,(wxe_util:colour_bin(CColor)):16/binary>>).

%% @equiv createFont(This,Font, [])
-spec createFont(This, Font) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsRenderer(), Font::wxFont:wxFont().

createFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  createFont(This,Font, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatefont">external documentation</a>.
-spec createFont(This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsRenderer(), Font::wxFont:wxFont(),
	Option :: {'col', wx:wx_colour()}.
createFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(FontT,wxFont),
  MOpts = fun({col, Col}, Acc) -> [<<1:32/?UI,(wxe_util:colour_bin(Col)):16/binary,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsRenderer_CreateFont,
  <<ThisRef:32/?UI,FontRef:32/?UI, BinOpt/binary>>).

%% @equiv createMatrix(This, [])
-spec createMatrix(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsRenderer().

createMatrix(This)
 when is_record(This, wx_ref) ->
  createMatrix(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatematrix">external documentation</a>.
-spec createMatrix(This, [Option]) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsRenderer(),
	Option :: {'a', number()}
		 | {'b', number()}
		 | {'c', number()}
		 | {'d', number()}
		 | {'tx', number()}
		 | {'ty', number()}.
createMatrix(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  MOpts = fun({a, A}, Acc) -> [<<1:32/?UI,0:32,A:64/?F>>|Acc];
          ({b, B}, Acc) -> [<<2:32/?UI,0:32,B:64/?F>>|Acc];
          ({c, C}, Acc) -> [<<3:32/?UI,0:32,C:64/?F>>|Acc];
          ({d, D}, Acc) -> [<<4:32/?UI,0:32,D:64/?F>>|Acc];
          ({tx, Tx}, Acc) -> [<<5:32/?UI,0:32,Tx:64/?F>>|Acc];
          ({ty, Ty}, Acc) -> [<<6:32/?UI,0:32,Ty:64/?F>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsRenderer_CreateMatrix,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatepath">external documentation</a>.
-spec createPath(This) -> wxGraphicsPath:wxGraphicsPath() when
	This::wxGraphicsRenderer().
createPath(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:call(?wxGraphicsRenderer_CreatePath,
  <<ThisRef:32/?UI>>).

