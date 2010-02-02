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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html">wxPen</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxPen()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpenwxpen">external documentation</a>.
new() ->
  wxe_util:construct(?wxPen_new_0,
  <<>>).

%% @spec (Colour::wx:colour()) -> wxPen()
%% @equiv new(Colour, [])
new(Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  new(Colour, []).

%% @spec (Colour::wx:colour(), [Option]) -> wxPen()
%% Option = {width, integer()} | {style, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpenwxpen">external documentation</a>.
new(Colour, Options)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4,is_list(Options) ->
  MOpts = fun({width, Width}, Acc) -> [<<1:32/?UI,Width:32/?UI>>|Acc];
          ({style, Style}, Acc) -> [<<2:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxPen_new_2,
  <<(wxe_util:colour_bin(Colour)):16/binary, BinOpt/binary>>).

%% @spec (This::wxPen()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpengetcap">external documentation</a>.
getCap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetCap,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPen()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpengetcolour">external documentation</a>.
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPen()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpengetjoin">external documentation</a>.
getJoin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetJoin,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPen()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpengetstyle">external documentation</a>.
getStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetStyle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPen()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpengetwidth">external documentation</a>.
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_GetWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPen()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpenisok">external documentation</a>.
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:call(?wxPen_IsOk,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPen(), CapStyle::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpensetcap">external documentation</a>.
setCap(#wx_ref{type=ThisT,ref=ThisRef},CapStyle)
 when is_integer(CapStyle) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetCap,
  <<ThisRef:32/?UI,CapStyle:32/?UI>>).

%% @spec (This::wxPen(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpensetcolour">external documentation</a>.
setColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetColour_1,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxPen(), Red::integer(), Green::integer(), Blue::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpensetcolour">external documentation</a>.
setColour(#wx_ref{type=ThisT,ref=ThisRef},Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetColour_3,
  <<ThisRef:32/?UI,Red:32/?UI,Green:32/?UI,Blue:32/?UI>>).

%% @spec (This::wxPen(), JoinStyle::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpensetjoin">external documentation</a>.
setJoin(#wx_ref{type=ThisT,ref=ThisRef},JoinStyle)
 when is_integer(JoinStyle) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetJoin,
  <<ThisRef:32/?UI,JoinStyle:32/?UI>>).

%% @spec (This::wxPen(), Style::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpensetstyle">external documentation</a>.
setStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetStyle,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @spec (This::wxPen(), Width::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpen.html#wxpensetwidth">external documentation</a>.
setWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:cast(?wxPen_SetWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @spec (This::wxPen()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPen),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
