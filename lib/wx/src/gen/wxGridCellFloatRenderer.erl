%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html">wxGridCellFloatRenderer</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGridCellStringRenderer}
%% <br />{@link wxGridCellRenderer}
%% </p>
%% @type wxGridCellFloatRenderer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridCellFloatRenderer).
-include("wxe.hrl").
-export([destroy/1,getPrecision/1,getWidth/1,new/0,new/1,setParameters/2,setPrecision/2,
  setWidth/2]).

%% inherited exports
-export([draw/8,getBestSize/6,parent_class/1]).

-export_type([wxGridCellFloatRenderer/0]).
%% @hidden
parent_class(wxGridCellStringRenderer) -> true;
parent_class(wxGridCellRenderer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGridCellFloatRenderer() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxGridCellFloatRenderer().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrendererwxgridcellfloatrenderer">external documentation</a>.
-spec new([Option]) -> wxGridCellFloatRenderer() when
	Option :: {'width', integer()}
		 | {'precision', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({width, Width}, Acc) -> [<<1:32/?UI,Width:32/?UI>>|Acc];
          ({precision, Precision}, Acc) -> [<<2:32/?UI,Precision:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxGridCellFloatRenderer_new,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderergetprecision">external documentation</a>.
-spec getPrecision(This) -> integer() when
	This::wxGridCellFloatRenderer().
getPrecision(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:call(?wxGridCellFloatRenderer_GetPrecision,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderergetwidth">external documentation</a>.
-spec getWidth(This) -> integer() when
	This::wxGridCellFloatRenderer().
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:call(?wxGridCellFloatRenderer_GetWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetparameters">external documentation</a>.
-spec setParameters(This, Params) -> 'ok' when
	This::wxGridCellFloatRenderer(), Params::unicode:chardata().
setParameters(#wx_ref{type=ThisT,ref=ThisRef},Params)
 when is_list(Params) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  Params_UC = unicode:characters_to_binary([Params,0]),
  wxe_util:cast(?wxGridCellFloatRenderer_SetParameters,
  <<ThisRef:32/?UI,(byte_size(Params_UC)):32/?UI,(Params_UC)/binary, 0:(((8- ((0+byte_size(Params_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetprecision">external documentation</a>.
-spec setPrecision(This, Precision) -> 'ok' when
	This::wxGridCellFloatRenderer(), Precision::integer().
setPrecision(#wx_ref{type=ThisT,ref=ThisRef},Precision)
 when is_integer(Precision) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:cast(?wxGridCellFloatRenderer_SetPrecision,
  <<ThisRef:32/?UI,Precision:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetwidth">external documentation</a>.
-spec setWidth(This, Width) -> 'ok' when
	This::wxGridCellFloatRenderer(), Width::integer().
setWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:cast(?wxGridCellFloatRenderer_SetWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGridCellFloatRenderer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridCellFloatRenderer),
  wxe_util:destroy(?wxGridCellFloatRenderer_destroy,Obj),
  ok.
 %% From wxGridCellStringRenderer
 %% From wxGridCellRenderer
%% @hidden
getBestSize(This,Grid,Attr,Dc,Row,Col) -> wxGridCellRenderer:getBestSize(This,Grid,Attr,Dc,Row,Col).
%% @hidden
draw(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected) -> wxGridCellRenderer:draw(This,Grid,Attr,Dc,Rect,Row,Col,IsSelected).
