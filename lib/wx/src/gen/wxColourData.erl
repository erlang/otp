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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html">wxColourData</a>.
%% @type wxColourData().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxColourData).
-include("wxe.hrl").
-export([destroy/1,getChooseFull/1,getColour/1,getCustomColour/2,new/0,new/1,
  setChooseFull/2,setColour/2,setCustomColour/3]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxColourData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html#wxcolourdatawxcolourdata">external documentation</a>.
new() ->
  wxe_util:construct(?wxColourData_new_0,
  <<>>).

%% @spec (Data::wxColourData()) -> wxColourData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html#wxcolourdatawxcolourdata">external documentation</a>.
new(#wx_ref{type=DataT,ref=DataRef}) ->
  ?CLASS(DataT,wxColourData),
  wxe_util:construct(?wxColourData_new_1,
  <<DataRef:32/?UI>>).

%% @spec (This::wxColourData()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html#wxcolourdatagetchoosefull">external documentation</a>.
getChooseFull(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:call(?wxColourData_GetChooseFull,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxColourData()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html#wxcolourdatagetcolour">external documentation</a>.
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:call(?wxColourData_GetColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxColourData(), I::integer()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html#wxcolourdatagetcustomcolour">external documentation</a>.
getCustomColour(#wx_ref{type=ThisT,ref=ThisRef},I)
 when is_integer(I) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:call(?wxColourData_GetCustomColour,
  <<ThisRef:32/?UI,I:32/?UI>>).

%% @spec (This::wxColourData(), Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html#wxcolourdatasetchoosefull">external documentation</a>.
setChooseFull(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_boolean(Flag) ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:cast(?wxColourData_SetChooseFull,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (This::wxColourData(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html#wxcolourdatasetcolour">external documentation</a>.
setColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:cast(?wxColourData_SetColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxColourData(), I::integer(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcolourdata.html#wxcolourdatasetcustomcolour">external documentation</a>.
setCustomColour(#wx_ref{type=ThisT,ref=ThisRef},I,Colour)
 when is_integer(I),tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxColourData),
  wxe_util:cast(?wxColourData_SetCustomColour,
  <<ThisRef:32/?UI,I:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxColourData()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxColourData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
