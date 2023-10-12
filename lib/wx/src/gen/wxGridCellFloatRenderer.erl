%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2020. All Rights Reserved.
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

-module(wxGridCellFloatRenderer).
-include("wxe.hrl").
-export([destroy/1,getPrecision/1,getWidth/1,new/0,new/1,setParameters/2,setPrecision/2,
  setWidth/2]).

%% inherited exports
-export([draw/8,getBestSize/6,parent_class/1]).

-type wxGridCellFloatRenderer() :: wx:wx_object().
-export_type([wxGridCellFloatRenderer/0]).
%% @hidden
parent_class(wxGridCellStringRenderer) -> true;
parent_class(wxGridCellRenderer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxGridCellFloatRenderer().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrendererwxgridcellfloatrenderer">external documentation</a>.
-spec new([Option]) -> wxGridCellFloatRenderer() when
	Option :: {'width', integer()}
		 | {'precision', integer()}
		 | {'format', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({width, _width} = Arg) -> Arg;
          ({precision, _precision} = Arg) -> Arg;
          ({format, _format} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxGridCellFloatRenderer_new),
  wxe_util:rec(?wxGridCellFloatRenderer_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderergetprecision">external documentation</a>.
-spec getPrecision(This) -> integer() when
	This::wxGridCellFloatRenderer().
getPrecision(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellFloatRenderer_GetPrecision),
  wxe_util:rec(?wxGridCellFloatRenderer_GetPrecision).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderergetwidth">external documentation</a>.
-spec getWidth(This) -> integer() when
	This::wxGridCellFloatRenderer().
getWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridCellFloatRenderer_GetWidth),
  wxe_util:rec(?wxGridCellFloatRenderer_GetWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetparameters">external documentation</a>.
-spec setParameters(This, Params) -> 'ok' when
	This::wxGridCellFloatRenderer(), Params::unicode:chardata().
setParameters(#wx_ref{type=ThisT}=This,Params)
 when ?is_chardata(Params) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  Params_UC = unicode:characters_to_binary(Params),
  wxe_util:queue_cmd(This,Params_UC,?get_env(),?wxGridCellFloatRenderer_SetParameters).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetprecision">external documentation</a>.
-spec setPrecision(This, Precision) -> 'ok' when
	This::wxGridCellFloatRenderer(), Precision::integer().
setPrecision(#wx_ref{type=ThisT}=This,Precision)
 when is_integer(Precision) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(This,Precision,?get_env(),?wxGridCellFloatRenderer_SetPrecision).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetwidth">external documentation</a>.
-spec setWidth(This, Width) -> 'ok' when
	This::wxGridCellFloatRenderer(), Width::integer().
setWidth(#wx_ref{type=ThisT}=This,Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(This,Width,?get_env(),?wxGridCellFloatRenderer_SetWidth).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGridCellFloatRenderer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellFloatRenderer),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxGridCellFloatRenderer_destroy),
  ok.
 %% From wxGridCellStringRenderer
 %% From wxGridCellRenderer
%% @hidden
getBestSize(This,Grid,Attr,Dc,Row,Col) -> wxGridCellRenderer:getBestSize(This,Grid,Attr,Dc,Row,Col).
%% @hidden
draw(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected) -> wxGridCellRenderer:draw(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected).
