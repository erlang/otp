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

-module(wxGraphicsRenderer).
-include("wxe.hrl").
-export([createBrush/2,createContext/2,createFont/2,createFont/3,createFont/4,
  createLinearGradientBrush/6,createMatrix/1,createMatrix/2,createPath/1,
  createRadialGradientBrush/7,getDefaultRenderer/0]).

%% inherited exports
-export([parent_class/1]).

-type wxGraphicsRenderer() :: wx:wx_object().
-export_type([wxGraphicsRenderer/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderergetdefaultrenderer">external documentation</a>.
-spec getDefaultRenderer() -> wxGraphicsRenderer().
getDefaultRenderer() ->
  wxe_util:queue_cmd(?get_env(), ?wxGraphicsRenderer_GetDefaultRenderer),
  wxe_util:rec(?wxGraphicsRenderer_GetDefaultRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatecontext">external documentation</a>.
-spec createContext(This, WindowDC) -> wxGraphicsContext:wxGraphicsContext() when
	This::wxGraphicsRenderer(), WindowDC::wxWindowDC:wxWindowDC() | wxWindow:wxWindow() | wxMemoryDC:wxMemoryDC().
createContext(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowDCT}=WindowDC) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  IswxWindowDC = ?CLASS_T(WindowDCT,wxWindowDC),
  IswxWindow = ?CLASS_T(WindowDCT,wxWindow),
  IswxMemoryDC = ?CLASS_T(WindowDCT,wxMemoryDC),
  WindowDCType = if
    IswxWindowDC ->   wxWindowDC;
    IswxWindow ->   wxWindow;
    IswxMemoryDC ->   wxMemoryDC;
    true -> error({badarg, WindowDCT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(WindowDC, WindowDCType),?get_env(),?wxGraphicsRenderer_CreateContext),
  wxe_util:rec(?wxGraphicsRenderer_CreateContext).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatebrush">external documentation</a>.
-spec createBrush(This, Brush) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), Brush::wxBrush:wxBrush().
createBrush(#wx_ref{type=ThisT}=This,#wx_ref{type=BrushT}=Brush) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(BrushT,wxBrush),
  wxe_util:queue_cmd(This,Brush,?get_env(),?wxGraphicsRenderer_CreateBrush),
  wxe_util:rec(?wxGraphicsRenderer_CreateBrush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatelineargradientbrush">external documentation</a>.
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, Stops) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), X1::number(), Y1::number(), X2::number(), Y2::number(), Stops::wxGraphicsGradientStops:wxGraphicsGradientStops().
createLinearGradientBrush(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2,#wx_ref{type=StopsT}=Stops)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(StopsT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,Stops,?get_env(),?wxGraphicsRenderer_CreateLinearGradientBrush),
  wxe_util:rec(?wxGraphicsRenderer_CreateLinearGradientBrush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreateradialgradientbrush">external documentation</a>.
-spec createRadialGradientBrush(This, StartX, StartY, EndX, EndY, Radius, Stops) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), StartX::number(), StartY::number(), EndX::number(), EndY::number(), Radius::number(), Stops::wxGraphicsGradientStops:wxGraphicsGradientStops().
createRadialGradientBrush(#wx_ref{type=ThisT}=This,StartX,StartY,EndX,EndY,Radius,#wx_ref{type=StopsT}=Stops)
 when is_number(StartX),is_number(StartY),is_number(EndX),is_number(EndY),is_number(Radius) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(StopsT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,StartX,StartY,EndX,EndY,Radius,Stops,?get_env(),?wxGraphicsRenderer_CreateRadialGradientBrush),
  wxe_util:rec(?wxGraphicsRenderer_CreateRadialGradientBrush).

%% @equiv createFont(This,Font, [])
-spec createFont(This, Font) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsRenderer(), Font::wxFont:wxFont().

createFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  createFont(This,Font, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatefont">external documentation</a>.
%% <br /> Also:<br />
%% createFont(This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont() when<br />
%% 	This::wxGraphicsRenderer(), Font::wxFont:wxFont(),<br />
%% 	Option :: {'col', wx:wx_colour()}.<br />
%% 
-spec createFont(This, SizeInPixels, Facename) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsRenderer(), SizeInPixels::number(), Facename::unicode:chardata();
      (This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsRenderer(), Font::wxFont:wxFont(),
	Option :: {'col', wx:wx_colour()}.

createFont(This,SizeInPixels,Facename)
 when is_record(This, wx_ref),is_number(SizeInPixels),?is_chardata(Facename) ->
  createFont(This,SizeInPixels,Facename, []);
createFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(FontT,wxFont),
  MOpts = fun({col, Col}) -> {col,wxe_util:color(Col)};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Font, Opts,?get_env(),?wxGraphicsRenderer_CreateFont_2),
  wxe_util:rec(?wxGraphicsRenderer_CreateFont_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatefont">external documentation</a>.
-spec createFont(This, SizeInPixels, Facename, [Option]) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsRenderer(), SizeInPixels::number(), Facename::unicode:chardata(),
	Option :: {'flags', integer()}
		 | {'col', wx:wx_colour()}.
createFont(#wx_ref{type=ThisT}=This,SizeInPixels,Facename, Options)
 when is_number(SizeInPixels),?is_chardata(Facename),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  Facename_UC = unicode:characters_to_binary(Facename),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          ({col, Col}) -> {col,wxe_util:color(Col)};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,SizeInPixels,Facename_UC, Opts,?get_env(),?wxGraphicsRenderer_CreateFont_3),
  wxe_util:rec(?wxGraphicsRenderer_CreateFont_3).

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
createMatrix(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  MOpts = fun({a, _a} = Arg) -> Arg;
          ({b, _b} = Arg) -> Arg;
          ({c, _c} = Arg) -> Arg;
          ({d, _d} = Arg) -> Arg;
          ({tx, _tx} = Arg) -> Arg;
          ({ty, _ty} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGraphicsRenderer_CreateMatrix),
  wxe_util:rec(?wxGraphicsRenderer_CreateMatrix).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatepath">external documentation</a>.
-spec createPath(This) -> wxGraphicsPath:wxGraphicsPath() when
	This::wxGraphicsRenderer().
createPath(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsRenderer_CreatePath),
  wxe_util:rec(?wxGraphicsRenderer_CreatePath).

