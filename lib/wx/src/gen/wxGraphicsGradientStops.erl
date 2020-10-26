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

-module(wxGraphicsGradientStops).
-include("wxe.hrl").
-export([add/3,destroy/1,getCount/1,getEndColour/1,getStartColour/1,item/2,new/0,
  new/1,setEndColour/2,setStartColour/2]).

%% inherited exports
-export([parent_class/1]).

-type wxGraphicsGradientStops() :: wx:wx_object().
-export_type([wxGraphicsGradientStops/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxGraphicsGradientStops().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopswxgraphicsgradientstops">external documentation</a>.
-spec new([Option]) -> wxGraphicsGradientStops() when
	Option :: {'startCol', wx:wx_colour()}
		 | {'endCol', wx:wx_colour()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({startCol, StartCol}) -> {startCol,wxe_util:color(StartCol)};
          ({endCol, EndCol}) -> {endCol,wxe_util:color(EndCol)};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxGraphicsGradientStops_new),
  wxe_util:rec(?wxGraphicsGradientStops_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsitem">external documentation</a>.
-spec item(This, N) -> {wx:wx_colour4(), float()} when
	This::wxGraphicsGradientStops(), N::integer().
item(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,N,?get_env(),?wxGraphicsGradientStops_Item),
  wxe_util:rec(?wxGraphicsGradientStops_Item).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsgetcount">external documentation</a>.
-spec getCount(This) -> integer() when
	This::wxGraphicsGradientStops().
getCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsGradientStops_GetCount),
  wxe_util:rec(?wxGraphicsGradientStops_GetCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopssetstartcolour">external documentation</a>.
-spec setStartColour(This, Col) -> 'ok' when
	This::wxGraphicsGradientStops(), Col::wx:wx_colour().
setStartColour(#wx_ref{type=ThisT}=This,Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxGraphicsGradientStops_SetStartColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsgetstartcolour">external documentation</a>.
-spec getStartColour(This) -> wx:wx_colour4() when
	This::wxGraphicsGradientStops().
getStartColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsGradientStops_GetStartColour),
  wxe_util:rec(?wxGraphicsGradientStops_GetStartColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopssetendcolour">external documentation</a>.
-spec setEndColour(This, Col) -> 'ok' when
	This::wxGraphicsGradientStops(), Col::wx:wx_colour().
setEndColour(#wx_ref{type=ThisT}=This,Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxGraphicsGradientStops_SetEndColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsgetendcolour">external documentation</a>.
-spec getEndColour(This) -> wx:wx_colour4() when
	This::wxGraphicsGradientStops().
getEndColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsGradientStops_GetEndColour),
  wxe_util:rec(?wxGraphicsGradientStops_GetEndColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsadd">external documentation</a>.
-spec add(This, Col, Pos) -> 'ok' when
	This::wxGraphicsGradientStops(), Col::wx:wx_colour(), Pos::number().
add(#wx_ref{type=ThisT}=This,Col,Pos)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4,is_number(Pos) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,wxe_util:color(Col),Pos,?get_env(),?wxGraphicsGradientStops_Add).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGraphicsGradientStops()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGraphicsGradientStops),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
