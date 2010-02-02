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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html">wxCalendarDateAttr</a>.
%% @type wxCalendarDateAttr().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxCalendarDateAttr).
-include("wxe.hrl").
-export([destroy/1,getBackgroundColour/1,getBorder/1,getBorderColour/1,getFont/1,
  getTextColour/1,hasBackgroundColour/1,hasBorder/1,hasBorderColour/1,
  hasFont/1,hasTextColour/1,isHoliday/1,new/0,new/1,new/2,setBackgroundColour/2,
  setBorder/2,setBorderColour/2,setFont/2,setHoliday/2,setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxCalendarDateAttr()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrwxcalendardateattr">external documentation</a>.
new() ->
  wxe_util:construct(?wxCalendarDateAttr_new_0,
  <<>>).

%% @spec (X::WxCalendarDateBorder|term()) -> wxCalendarDateAttr()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrwxcalendardateattr">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Border::WxCalendarDateBorder) -> new(Border, []) </c></p>
%% <p><c>
%% new(ColText::wx:colour()) -> new(ColText, []) </c></p>

new(Border)
 when is_integer(Border) ->
  new(Border, []);

new(ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  new(ColText, []).

%% @spec (X::WxCalendarDateBorder|term(),[Option]) -> wxCalendarDateAttr()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrwxcalendardateattr">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Border::WxCalendarDateBorder, [Option]) -> wxCalendarDateAttr() </c>
%%<br /> Option = {colBorder, wx:colour()}
%%<br /> WxCalendarDateBorder = integer()
%%<br /> WxCalendarDateBorder is one of ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
%% </p>
%% <p><c>
%% new(ColText::wx:colour(), [Option]) -> wxCalendarDateAttr() </c>
%%<br /> Option = {colBack, wx:colour()} | {colBorder, wx:colour()} | {font, wxFont:wxFont()} | {border, WxCalendarDateBorder}
%%<br /> WxCalendarDateBorder = integer()
%%<br /> WxCalendarDateBorder is one of ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
%% </p>
new(Border, Options)
 when is_integer(Border),is_list(Options) ->
  MOpts = fun({colBorder, ColBorder}, Acc) -> [<<1:32/?UI,(wxe_util:colour_bin(ColBorder)):16/binary,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxCalendarDateAttr_new_2_0,
  <<Border:32/?UI, 0:32,BinOpt/binary>>);
new(ColText, Options)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4,is_list(Options) ->
  MOpts = fun({colBack, ColBack}, Acc) -> [<<1:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary,0:32>>|Acc];
          ({colBorder, ColBorder}, Acc) -> [<<2:32/?UI,(wxe_util:colour_bin(ColBorder)):16/binary,0:32>>|Acc];
          ({font, #wx_ref{type=FontT,ref=FontRef}}, Acc) ->   ?CLASS(FontT,wxFont),[<<3:32/?UI,FontRef:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<4:32/?UI,Border:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxCalendarDateAttr_new_2_1,
  <<(wxe_util:colour_bin(ColText)):16/binary, BinOpt/binary>>).

%% @spec (This::wxCalendarDateAttr(), ColText::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrsettextcolour">external documentation</a>.
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColText)):16/binary>>).

%% @spec (This::wxCalendarDateAttr(), ColBack::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrsetbackgroundcolour">external documentation</a>.
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary>>).

%% @spec (This::wxCalendarDateAttr(), Col::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrsetbordercolour">external documentation</a>.
setBorderColour(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetBorderColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @spec (This::wxCalendarDateAttr(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrsetfont">external documentation</a>.
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxCalendarDateAttr_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr(), Border::WxCalendarDateBorder) -> ok
%% WxCalendarDateBorder = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrsetborder">external documentation</a>.
%%<br /> WxCalendarDateBorder is one of ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
setBorder(#wx_ref{type=ThisT,ref=ThisRef},Border)
 when is_integer(Border) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetBorder,
  <<ThisRef:32/?UI,Border:32/?UI>>).

%% @spec (This::wxCalendarDateAttr(), Holiday::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrsetholiday">external documentation</a>.
setHoliday(#wx_ref{type=ThisT,ref=ThisRef},Holiday)
 when is_boolean(Holiday) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetHoliday,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Holiday)):32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrhastextcolour">external documentation</a>.
hasTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasTextColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrhasbackgroundcolour">external documentation</a>.
hasBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrhasbordercolour">external documentation</a>.
hasBorderColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasBorderColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrhasfont">external documentation</a>.
hasFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrhasborder">external documentation</a>.
hasBorder(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasBorder,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrisholiday">external documentation</a>.
isHoliday(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_IsHoliday,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrgettextcolour">external documentation</a>.
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrgetbackgroundcolour">external documentation</a>.
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrgetbordercolour">external documentation</a>.
getBorderColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetBorderColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrgetfont">external documentation</a>.
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> WxCalendarDateBorder
%% WxCalendarDateBorder = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcalendardateattr.html#wxcalendardateattrgetborder">external documentation</a>.
%%<br /> WxCalendarDateBorder is one of ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
getBorder(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetBorder,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCalendarDateAttr()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCalendarDateAttr),
  wxe_util:destroy(?wxCalendarDateAttr_destroy,Obj),
  ok.
