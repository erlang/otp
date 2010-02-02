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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html">wxGraphicsRenderer</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxGraphicsRenderer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderergetdefaultrenderer">external documentation</a>.
getDefaultRenderer() ->
  wxe_util:call(?wxGraphicsRenderer_GetDefaultRenderer,
  <<>>).

%% @spec (This::wxGraphicsRenderer(), Dc::wxWindowDC:wxWindowDC() | wxWindow:wxWindow()) -> wxGraphicsContext:wxGraphicsContext()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatecontext">external documentation</a>.
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

%% @spec (This::wxGraphicsRenderer(), Pen::wxPen:wxPen()) -> wxGraphicsPen:wxGraphicsPen()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatepen">external documentation</a>.
createPen(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PenT,ref=PenRef}) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(PenT,wxPen),
  wxe_util:call(?wxGraphicsRenderer_CreatePen,
  <<ThisRef:32/?UI,PenRef:32/?UI>>).

%% @spec (This::wxGraphicsRenderer(), Brush::wxBrush:wxBrush()) -> wxGraphicsBrush:wxGraphicsBrush()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatebrush">external documentation</a>.
createBrush(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BrushT,ref=BrushRef}) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(BrushT,wxBrush),
  wxe_util:call(?wxGraphicsRenderer_CreateBrush,
  <<ThisRef:32/?UI,BrushRef:32/?UI>>).

%% @spec (This::wxGraphicsRenderer(), X1::float(), Y1::float(), X2::float(), Y2::float(), C1::wx:colour(), C2::wx:colour()) -> wxGraphicsBrush:wxGraphicsBrush()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatelineargradientbrush">external documentation</a>.
createLinearGradientBrush(#wx_ref{type=ThisT,ref=ThisRef},X1,Y1,X2,Y2,C1,C2)
 when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2),tuple_size(C1) =:= 3; tuple_size(C1) =:= 4,tuple_size(C2) =:= 3; tuple_size(C2) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:call(?wxGraphicsRenderer_CreateLinearGradientBrush,
  <<ThisRef:32/?UI,0:32,X1:64/?F,Y1:64/?F,X2:64/?F,Y2:64/?F,(wxe_util:colour_bin(C1)):16/binary,(wxe_util:colour_bin(C2)):16/binary>>).

%% @spec (This::wxGraphicsRenderer(), Xo::float(), Yo::float(), Xc::float(), Yc::float(), Radius::float(), OColor::wx:colour(), CColor::wx:colour()) -> wxGraphicsBrush:wxGraphicsBrush()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreateradialgradientbrush">external documentation</a>.
createRadialGradientBrush(#wx_ref{type=ThisT,ref=ThisRef},Xo,Yo,Xc,Yc,Radius,OColor,CColor)
 when is_float(Xo),is_float(Yo),is_float(Xc),is_float(Yc),is_float(Radius),tuple_size(OColor) =:= 3; tuple_size(OColor) =:= 4,tuple_size(CColor) =:= 3; tuple_size(CColor) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:call(?wxGraphicsRenderer_CreateRadialGradientBrush,
  <<ThisRef:32/?UI,0:32,Xo:64/?F,Yo:64/?F,Xc:64/?F,Yc:64/?F,Radius:64/?F,(wxe_util:colour_bin(OColor)):16/binary,(wxe_util:colour_bin(CColor)):16/binary>>).

%% @spec (This::wxGraphicsRenderer(), Font::wxFont:wxFont()) -> wxGraphicsFont:wxGraphicsFont()
%% @equiv createFont(This,Font, [])
createFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  createFont(This,Font, []).

%% @spec (This::wxGraphicsRenderer(), Font::wxFont:wxFont(), [Option]) -> wxGraphicsFont:wxGraphicsFont()
%% Option = {col, wx:colour()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatefont">external documentation</a>.
createFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(FontT,wxFont),
  MOpts = fun({col, Col}, Acc) -> [<<1:32/?UI,(wxe_util:colour_bin(Col)):16/binary,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsRenderer_CreateFont,
  <<ThisRef:32/?UI,FontRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxGraphicsRenderer()) -> wxGraphicsMatrix:wxGraphicsMatrix()
%% @equiv createMatrix(This, [])
createMatrix(This)
 when is_record(This, wx_ref) ->
  createMatrix(This, []).

%% @spec (This::wxGraphicsRenderer(), [Option]) -> wxGraphicsMatrix:wxGraphicsMatrix()
%% Option = {a, float()} | {b, float()} | {c, float()} | {d, float()} | {tx, float()} | {ty, float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatematrix">external documentation</a>.
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

%% @spec (This::wxGraphicsRenderer()) -> wxGraphicsPath:wxGraphicsPath()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatepath">external documentation</a>.
createPath(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:call(?wxGraphicsRenderer_CreatePath,
  <<ThisRef:32/?UI>>).

