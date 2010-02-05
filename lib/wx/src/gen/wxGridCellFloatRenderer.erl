%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellfloatrenderer.html">wxGridCellFloatRenderer</a>.
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

%% @hidden
parent_class(wxGridCellStringRenderer) -> true;
parent_class(wxGridCellRenderer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxGridCellFloatRenderer()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxGridCellFloatRenderer()
%% Option = {width, integer()} | {precision, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrendererwxgridcellfloatrenderer">external documentation</a>.
new(Options)
 when is_list(Options) ->
  MOpts = fun({width, Width}, Acc) -> [<<1:32/?UI,Width:32/?UI>>|Acc];
          ({precision, Precision}, Acc) -> [<<2:32/?UI,Precision:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxGridCellFloatRenderer_new,
  <<BinOpt/binary>>).

%% @spec (This::wxGridCellFloatRenderer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderergetprecision">external documentation</a>.
getPrecision(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:call(?wxGridCellFloatRenderer_GetPrecision,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGridCellFloatRenderer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderergetwidth">external documentation</a>.
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:call(?wxGridCellFloatRenderer_GetWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGridCellFloatRenderer(), Params::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetparameters">external documentation</a>.
setParameters(#wx_ref{type=ThisT,ref=ThisRef},Params)
 when is_list(Params) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  Params_UC = unicode:characters_to_binary([Params,0]),
  wxe_util:cast(?wxGridCellFloatRenderer_SetParameters,
  <<ThisRef:32/?UI,(byte_size(Params_UC)):32/?UI,(Params_UC)/binary, 0:(((8- ((0+byte_size(Params_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxGridCellFloatRenderer(), Precision::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetprecision">external documentation</a>.
setPrecision(#wx_ref{type=ThisT,ref=ThisRef},Precision)
 when is_integer(Precision) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:cast(?wxGridCellFloatRenderer_SetPrecision,
  <<ThisRef:32/?UI,Precision:32/?UI>>).

%% @spec (This::wxGridCellFloatRenderer(), Width::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridcellfloatrenderer.html#wxgridcellfloatrenderersetwidth">external documentation</a>.
setWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGridCellFloatRenderer),
  wxe_util:cast(?wxGridCellFloatRenderer_SetWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @spec (This::wxGridCellFloatRenderer()) -> ok
%% @doc Destroys this object, do not use object again
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
