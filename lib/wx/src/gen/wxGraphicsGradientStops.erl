%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-moduledoc """
Represents a collection of wxGraphicGradientStop values for use with
CreateLinearGradientBrush and CreateRadialGradientBrush.

The stops are maintained in order of position. If two or more stops are added
with the same position then the one(s) added later come later. This can be
useful for producing discontinuities in the colour gradient.

Notice that this class is write-once, you can't modify the stops once they had
been added.

Since: 2.9.1

wxWidgets docs:
[wxGraphicsGradientStops](https://docs.wxwidgets.org/3.1/classwx_graphics_gradient_stops.html)
""".
-include("wxe.hrl").
-export([add/3,destroy/1,getCount/1,getEndColour/1,getStartColour/1,item/2,new/0,
  new/1,setEndColour/2,setStartColour/2]).

%% inherited exports
-export([parent_class/1]).

-type wxGraphicsGradientStops() :: wx:wx_object().
-export_type([wxGraphicsGradientStops/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxGraphicsGradientStops().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopswxgraphicsgradientstops">external documentation</a>.
-doc """
Initializes the gradient stops with the given boundary colours.

Creates a `m:wxGraphicsGradientStops` instance with start colour given by
`startCol` and end colour given by `endCol`.
""".
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
-doc "Returns the stop at the given index.".
-spec item(This, N) -> {wx:wx_colour4(), float()} when
	This::wxGraphicsGradientStops(), N::integer().
item(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,N,?get_env(),?wxGraphicsGradientStops_Item),
  wxe_util:rec(?wxGraphicsGradientStops_Item).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsgetcount">external documentation</a>.
-doc "Returns the number of stops.".
-spec getCount(This) -> integer() when
	This::wxGraphicsGradientStops().
getCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsGradientStops_GetCount),
  wxe_util:rec(?wxGraphicsGradientStops_GetCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopssetstartcolour">external documentation</a>.
-doc "Set the start colour to `col`.".
-spec setStartColour(This, Col) -> 'ok' when
	This::wxGraphicsGradientStops(), Col::wx:wx_colour().
setStartColour(#wx_ref{type=ThisT}=This,Col)
 when ?is_colordata(Col) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxGraphicsGradientStops_SetStartColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsgetstartcolour">external documentation</a>.
-doc "Returns the start colour.".
-spec getStartColour(This) -> wx:wx_colour4() when
	This::wxGraphicsGradientStops().
getStartColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsGradientStops_GetStartColour),
  wxe_util:rec(?wxGraphicsGradientStops_GetStartColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopssetendcolour">external documentation</a>.
-doc "Set the end colour to `col`.".
-spec setEndColour(This, Col) -> 'ok' when
	This::wxGraphicsGradientStops(), Col::wx:wx_colour().
setEndColour(#wx_ref{type=ThisT}=This,Col)
 when ?is_colordata(Col) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxGraphicsGradientStops_SetEndColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsgetendcolour">external documentation</a>.
-doc "Returns the end colour.".
-spec getEndColour(This) -> wx:wx_colour4() when
	This::wxGraphicsGradientStops().
getEndColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsGradientStops_GetEndColour),
  wxe_util:rec(?wxGraphicsGradientStops_GetEndColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsgradientstops.html#wxgraphicsgradientstopsadd">external documentation</a>.
-doc "Add a new stop.".
-spec add(This, Col, Pos) -> 'ok' when
	This::wxGraphicsGradientStops(), Col::wx:wx_colour(), Pos::number().
add(#wx_ref{type=ThisT}=This,Col,Pos)
 when ?is_colordata(Col),is_number(Pos) ->
  ?CLASS(ThisT,wxGraphicsGradientStops),
  wxe_util:queue_cmd(This,wxe_util:color(Col),Pos,?get_env(),?wxGraphicsGradientStops_Add).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxGraphicsGradientStops()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGraphicsGradientStops),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
