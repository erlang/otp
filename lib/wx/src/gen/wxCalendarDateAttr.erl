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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html">wxCalendarDateAttr</a>.
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

-export_type([wxCalendarDateAttr/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxCalendarDateAttr() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrwxcalendardateattr">external documentation</a>.
-spec new() -> wxCalendarDateAttr().
new() ->
  wxe_util:construct(?wxCalendarDateAttr_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrwxcalendardateattr">external documentation</a>.
%% <br /> Also:<br />
%% new(ColText) -> wxCalendarDateAttr() when<br />
%% 	ColText::wx:wx_colour().<br />
%% 
%%<br /> Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-spec new(Border) -> wxCalendarDateAttr() when
	Border::wx:wx_enum();
      (ColText) -> wxCalendarDateAttr() when
	ColText::wx:wx_colour().

new(Border)
 when is_integer(Border) ->
  new(Border, []);

new(ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  new(ColText, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrwxcalendardateattr">external documentation</a>.
%% <br /> Also:<br />
%% new(ColText, [Option]) -> wxCalendarDateAttr() when<br />
%% 	ColText::wx:wx_colour(),<br />
%% 	Option :: {'colBack', wx:wx_colour()}<br />
%% 		 | {'colBorder', wx:wx_colour()}<br />
%% 		 | {'font', wxFont:wxFont()}<br />
%% 		 | {'border', wx:wx_enum()}.<br />
%% 
%%<br /> Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-spec new(Border, [Option]) -> wxCalendarDateAttr() when
	Border::wx:wx_enum(),
	Option :: {'colBorder', wx:wx_colour()};
      (ColText, [Option]) -> wxCalendarDateAttr() when
	ColText::wx:wx_colour(),
	Option :: {'colBack', wx:wx_colour()}
		 | {'colBorder', wx:wx_colour()}
		 | {'font', wxFont:wxFont()}
		 | {'border', wx:wx_enum()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsettextcolour">external documentation</a>.
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxCalendarDateAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColText)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxCalendarDateAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetbordercolour">external documentation</a>.
-spec setBorderColour(This, Col) -> 'ok' when
	This::wxCalendarDateAttr(), Col::wx:wx_colour().
setBorderColour(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetBorderColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxCalendarDateAttr(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxCalendarDateAttr_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetborder">external documentation</a>.
%%<br /> Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-spec setBorder(This, Border) -> 'ok' when
	This::wxCalendarDateAttr(), Border::wx:wx_enum().
setBorder(#wx_ref{type=ThisT,ref=ThisRef},Border)
 when is_integer(Border) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetBorder,
  <<ThisRef:32/?UI,Border:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetholiday">external documentation</a>.
-spec setHoliday(This, Holiday) -> 'ok' when
	This::wxCalendarDateAttr(), Holiday::boolean().
setHoliday(#wx_ref{type=ThisT,ref=ThisRef},Holiday)
 when is_boolean(Holiday) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:cast(?wxCalendarDateAttr_SetHoliday,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Holiday)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhastextcolour">external documentation</a>.
-spec hasTextColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhasbackgroundcolour">external documentation</a>.
-spec hasBackgroundColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhasbordercolour">external documentation</a>.
-spec hasBorderColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBorderColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasBorderColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhasfont">external documentation</a>.
-spec hasFont(This) -> boolean() when
	This::wxCalendarDateAttr().
hasFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhasborder">external documentation</a>.
-spec hasBorder(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBorder(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_HasBorder,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrisholiday">external documentation</a>.
-spec isHoliday(This) -> boolean() when
	This::wxCalendarDateAttr().
isHoliday(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_IsHoliday,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgetbordercolour">external documentation</a>.
-spec getBorderColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getBorderColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetBorderColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxCalendarDateAttr().
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgetborder">external documentation</a>.
%%<br /> Res = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-spec getBorder(This) -> wx:wx_enum() when
	This::wxCalendarDateAttr().
getBorder(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:call(?wxCalendarDateAttr_GetBorder,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxCalendarDateAttr()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCalendarDateAttr),
  wxe_util:destroy(?wxCalendarDateAttr_destroy,Obj),
  ok.
