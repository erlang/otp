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

-module(wxCalendarDateAttr).
-moduledoc """
Functions for wxCalendarDateAttr class

`m:wxCalendarDateAttr` is a custom attributes for a calendar date. The objects
of this class are used with `m:wxCalendarCtrl`.

See: `m:wxCalendarCtrl`

wxWidgets docs:
[wxCalendarDateAttr](https://docs.wxwidgets.org/3.1/classwx_calendar_date_attr.html)
""".
-include("wxe.hrl").
-export([destroy/1,getBackgroundColour/1,getBorder/1,getBorderColour/1,getFont/1,
  getTextColour/1,hasBackgroundColour/1,hasBorder/1,hasBorderColour/1,
  hasFont/1,hasTextColour/1,isHoliday/1,new/0,new/1,new/2,setBackgroundColour/2,
  setBorder/2,setBorderColour/2,setFont/2,setHoliday/2,setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

-type wxCalendarDateAttr() :: wx:wx_object().
-export_type([wxCalendarDateAttr/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxCalendarDateAttr().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrwxcalendardateattr">external documentation</a>.
%% <br /> Also:<br />
%% new([Option]) -> wxCalendarDateAttr() when<br />
%% 	Option :: {'colText', wx:wx_colour()}<br />
%% 		 | {'colBack', wx:wx_colour()}<br />
%% 		 | {'colBorder', wx:wx_colour()}<br />
%% 		 | {'font', wxFont:wxFont()}<br />
%% 		 | {'border', wx:wx_enum()}.<br />
%% 
%%<br /> Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-doc "Constructor for specifying all `m:wxCalendarDateAttr` properties.".
-spec new(Border) -> wxCalendarDateAttr() when
	Border::wx:wx_enum();
      ([Option]) -> wxCalendarDateAttr() when
	Option :: {'colText', wx:wx_colour()}
		 | {'colBack', wx:wx_colour()}
		 | {'colBorder', wx:wx_colour()}
		 | {'font', wxFont:wxFont()}
		 | {'border', wx:wx_enum()}.

new(Border)
 when is_integer(Border) ->
  new(Border, []);
new(Options)
 when is_list(Options) ->
  MOpts = fun({colText, ColText}) -> {colText,wxe_util:color(ColText)};
          ({colBack, ColBack}) -> {colBack,wxe_util:color(ColBack)};
          ({colBorder, ColBorder}) -> {colBorder,wxe_util:color(ColBorder)};
          ({font, #wx_ref{type=FontT}} = Arg) ->   ?CLASS(FontT,wxFont),Arg;
          ({border, _border} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxCalendarDateAttr_new_1),
  wxe_util:rec(?wxCalendarDateAttr_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrwxcalendardateattr">external documentation</a>.
%%<br /> Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-doc "Constructor using default properties except the given border.".
-spec new(Border, [Option]) -> wxCalendarDateAttr() when
	Border::wx:wx_enum(),
	Option :: {'colBorder', wx:wx_colour()}.
new(Border, Options)
 when is_integer(Border),is_list(Options) ->
  MOpts = fun({colBorder, ColBorder}) -> {colBorder,wxe_util:color(ColBorder)};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Border, Opts,?get_env(),?wxCalendarDateAttr_new_2),
  wxe_util:rec(?wxCalendarDateAttr_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsettextcolour">external documentation</a>.
-doc "Sets the text (foreground) colour to use.".
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxCalendarDateAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,ColText)
 when ?is_colordata(ColText) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColText),?get_env(),?wxCalendarDateAttr_SetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetbackgroundcolour">external documentation</a>.
-doc "Sets the text background colour to use.".
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxCalendarDateAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,ColBack)
 when ?is_colordata(ColBack) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColBack),?get_env(),?wxCalendarDateAttr_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetbordercolour">external documentation</a>.
-doc "Sets the border colour to use.".
-spec setBorderColour(This, Col) -> 'ok' when
	This::wxCalendarDateAttr(), Col::wx:wx_colour().
setBorderColour(#wx_ref{type=ThisT}=This,Col)
 when ?is_colordata(Col) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxCalendarDateAttr_SetBorderColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetfont">external documentation</a>.
-doc "Sets the font to use.".
-spec setFont(This, Font) -> 'ok' when
	This::wxCalendarDateAttr(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxCalendarDateAttr_SetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetborder">external documentation</a>.
%%<br /> Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-doc "Sets the border to use.".
-spec setBorder(This, Border) -> 'ok' when
	This::wxCalendarDateAttr(), Border::wx:wx_enum().
setBorder(#wx_ref{type=ThisT}=This,Border)
 when is_integer(Border) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,Border,?get_env(),?wxCalendarDateAttr_SetBorder).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrsetholiday">external documentation</a>.
-doc "If `holiday` is true, this calendar day will be displayed as a holiday.".
-spec setHoliday(This, Holiday) -> 'ok' when
	This::wxCalendarDateAttr(), Holiday::boolean().
setHoliday(#wx_ref{type=ThisT}=This,Holiday)
 when is_boolean(Holiday) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,Holiday,?get_env(),?wxCalendarDateAttr_SetHoliday).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhastextcolour">external documentation</a>.
-doc "Returns true if a non-default text foreground colour is set.".
-spec hasTextColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasTextColour),
  wxe_util:rec(?wxCalendarDateAttr_HasTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhasbackgroundcolour">external documentation</a>.
-doc "Returns true if a non-default text background colour is set.".
-spec hasBackgroundColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasBackgroundColour),
  wxe_util:rec(?wxCalendarDateAttr_HasBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhasbordercolour">external documentation</a>.
-doc "Returns true if a non-default border colour is set.".
-spec hasBorderColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBorderColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasBorderColour),
  wxe_util:rec(?wxCalendarDateAttr_HasBorderColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhasfont">external documentation</a>.
-doc "Returns true if a non-default font is set.".
-spec hasFont(This) -> boolean() when
	This::wxCalendarDateAttr().
hasFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasFont),
  wxe_util:rec(?wxCalendarDateAttr_HasFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrhasborder">external documentation</a>.
-doc "Returns true if a non-default (i.e. any) border is set.".
-spec hasBorder(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBorder(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasBorder),
  wxe_util:rec(?wxCalendarDateAttr_HasBorder).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrisholiday">external documentation</a>.
-doc "Returns true if this calendar day is displayed as a holiday.".
-spec isHoliday(This) -> boolean() when
	This::wxCalendarDateAttr().
isHoliday(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_IsHoliday),
  wxe_util:rec(?wxCalendarDateAttr_IsHoliday).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgettextcolour">external documentation</a>.
-doc "Returns the text colour set for the calendar date.".
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetTextColour),
  wxe_util:rec(?wxCalendarDateAttr_GetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgetbackgroundcolour">external documentation</a>.
-doc "Returns the background colour set for the calendar date.".
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetBackgroundColour),
  wxe_util:rec(?wxCalendarDateAttr_GetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgetbordercolour">external documentation</a>.
-doc "Returns the border colour set for the calendar date.".
-spec getBorderColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getBorderColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetBorderColour),
  wxe_util:rec(?wxCalendarDateAttr_GetBorderColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgetfont">external documentation</a>.
-doc "Returns the font set for the calendar date.".
-spec getFont(This) -> wxFont:wxFont() when
	This::wxCalendarDateAttr().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetFont),
  wxe_util:rec(?wxCalendarDateAttr_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendardateattr.html#wxcalendardateattrgetborder">external documentation</a>.
%%<br /> Res = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-doc "Returns the border set for the calendar date.".
-spec getBorder(This) -> wx:wx_enum() when
	This::wxCalendarDateAttr().
getBorder(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetBorder),
  wxe_util:rec(?wxCalendarDateAttr_GetBorder).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxCalendarDateAttr()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCalendarDateAttr),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxCalendarDateAttr_destroy),
  ok.
