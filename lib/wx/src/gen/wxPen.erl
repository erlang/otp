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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html">wxPen</a>.
%% @type wxPen().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPen).
-include("wxe.hrl").
-export([destroy/1,getCap/1,getColour/1,getJoin/1,getStyle/1,getWidth/1,isOk/1,
  new/0,new/1,new/2,setCap/2,setColour/2,setColour/4,setJoin/2,setStyle/2,
  setWidth/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxPen/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPen() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpenwxpen">external documentation</a>.
-spec new() -> wxPen().
new() ->
  wxe_util:construct(?wxPen_new_0,
  <<>>).

%% @equiv new(Colour, [])
-spec new(Colour) -> wxPen() when
	Colour::wx:wx_colour().

new(Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  new(Colour, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpenwxpen">external documentation</a>.
-spec new(Colour, [Option]) -> wxPen() when
	Colour::wx:wx_colour(),
	Option :: {'width', integer()}
		 | {'style', integer()}.
new(Colour, Options)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4,is_list(Options) ->
  MOpts = fun({width, Width}, Acc) -> [<<1:32/?UI,Width:32/?UI>>|Acc];
          ({style, Style}, Acc) -> [<<2:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxPen_new_2,
  <<(wxe_util:colour_bin(Colour)):16/binary, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetcap">external documentation</a>.
-spec getCap(This) -> integer() when
	This::wxPen().
getCap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetCap,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetcolour">external documentation</a>.
-spec getColour(This) -> wx:wx_colour4() when
	This::wxPen().
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetjoin">external documentation</a>.
-spec getJoin(This) -> integer() when
	This::wxPen().
getJoin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetJoin,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetstyle">external documentation</a>.
-spec getStyle(This) -> integer() when
	This::wxPen().
getStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetStyle,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetwidth">external documentation</a>.
-spec getWidth(This) -> integer() when
	This::wxPen().
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpenisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPen().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetcap">external documentation</a>.
%%<br /> CapStyle = integer
-spec setCap(This, CapStyle) -> 'ok' when
	This::wxPen(), CapStyle::wx:wx_enum().
setCap(#wx_ref{type=ThisT,ref=ThisRef},CapStyle)
 when is_integer(CapStyle) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetCap,
  <<ThisRef:32/?UI,CapStyle:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetcolour">external documentation</a>.
-spec setColour(This, Colour) -> 'ok' when
	This::wxPen(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetColour_1,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetcolour">external documentation</a>.
-spec setColour(This, Red, Green, Blue) -> 'ok' when
	This::wxPen(), Red::integer(), Green::integer(), Blue::integer().
setColour(#wx_ref{type=ThisT,ref=ThisRef},Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetColour_3,
  <<ThisRef:32/?UI,Red:32/?UI,Green:32/?UI,Blue:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetjoin">external documentation</a>.
%%<br /> JoinStyle = integer
-spec setJoin(This, JoinStyle) -> 'ok' when
	This::wxPen(), JoinStyle::wx:wx_enum().
setJoin(#wx_ref{type=ThisT,ref=ThisRef},JoinStyle)
 when is_integer(JoinStyle) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetJoin,
  <<ThisRef:32/?UI,JoinStyle:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetstyle">external documentation</a>.
-spec setStyle(This, Style) -> 'ok' when
	This::wxPen(), Style::integer().
setStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetStyle,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetwidth">external documentation</a>.
-spec setWidth(This, Width) -> 'ok' when
	This::wxPen(), Width::integer().
setWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPen()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPen),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
