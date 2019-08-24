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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html">wxColourData</a>.
%% @type wxColourData().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxColourData).
-include("wxe.hrl").
-export([destroy/1,getChooseFull/1,getColour/1,getCustomColour/2,new/0,new/1,
  setChooseFull/2,setColour/2,setCustomColour/3]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxColourData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxColourData() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html#wxcolourdatawxcolourdata">external documentation</a>.
-spec new() -> wxColourData().
new() ->
  wxe_util:construct(?wxColourData_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html#wxcolourdatawxcolourdata">external documentation</a>.
-spec new(Data) -> wxColourData() when
	Data::wxColourData().
new(#wx_ref{type=DataT,ref=DataRef}) ->
  ?CLASS(DataT,wxColourData),
  wxe_util:construct(?wxColourData_new_1,
  <<DataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html#wxcolourdatagetchoosefull">external documentation</a>.
-spec getChooseFull(This) -> boolean() when
	This::wxColourData().
getChooseFull(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:call(?wxColourData_GetChooseFull,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html#wxcolourdatagetcolour">external documentation</a>.
-spec getColour(This) -> wx:wx_colour4() when
	This::wxColourData().
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:call(?wxColourData_GetColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html#wxcolourdatagetcustomcolour">external documentation</a>.
-spec getCustomColour(This, I) -> wx:wx_colour4() when
	This::wxColourData(), I::integer().
getCustomColour(#wx_ref{type=ThisT,ref=ThisRef},I)
 when is_integer(I) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:call(?wxColourData_GetCustomColour,
  <<ThisRef:32/?UI,I:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html#wxcolourdatasetchoosefull">external documentation</a>.
-spec setChooseFull(This, Flag) -> 'ok' when
	This::wxColourData(), Flag::boolean().
setChooseFull(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:cast(?wxColourData_SetChooseFull,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html#wxcolourdatasetcolour">external documentation</a>.
-spec setColour(This, Colour) -> 'ok' when
	This::wxColourData(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:cast(?wxColourData_SetColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcolourdata.html#wxcolourdatasetcustomcolour">external documentation</a>.
-spec setCustomColour(This, I, Colour) -> 'ok' when
	This::wxColourData(), I::integer(), Colour::wx:wx_colour().
setCustomColour(#wx_ref{type=ThisT,ref=ThisRef},I,Colour)
 when is_integer(I),tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:cast(?wxColourData_SetCustomColour,
  <<ThisRef:32/?UI,I:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxColourData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxColourData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
