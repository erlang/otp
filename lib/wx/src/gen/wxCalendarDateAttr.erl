%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxCalendarDateAttr).
-moduledoc """
`m:wxCalendarDateAttr` is a custom attributes for a calendar date.

The objects of this class are used with `m:wxCalendarCtrl`.

See: `m:wxCalendarCtrl`

wxWidgets docs: [wxCalendarDateAttr](https://docs.wxwidgets.org/3.2/classwx_calendar_date_attr.html)
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
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new([])}).
-spec new() -> wxCalendarDateAttr().

new() ->
  new([]).

-doc "Constructor for specifying all `m:wxCalendarDateAttr` properties.".
%%  Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
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

-doc "Constructor using default properties except the given border.".
%%  Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
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

-doc "Sets the text (foreground) colour to use.".
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxCalendarDateAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,ColText)
 when ?is_colordata(ColText) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColText),?get_env(),?wxCalendarDateAttr_SetTextColour).

-doc "Sets the text background colour to use.".
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxCalendarDateAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,ColBack)
 when ?is_colordata(ColBack) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColBack),?get_env(),?wxCalendarDateAttr_SetBackgroundColour).

-doc "Sets the border colour to use.".
-spec setBorderColour(This, Col) -> 'ok' when
	This::wxCalendarDateAttr(), Col::wx:wx_colour().
setBorderColour(#wx_ref{type=ThisT}=This,Col)
 when ?is_colordata(Col) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxCalendarDateAttr_SetBorderColour).

-doc "Sets the font to use.".
-spec setFont(This, Font) -> 'ok' when
	This::wxCalendarDateAttr(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxCalendarDateAttr_SetFont).

-doc "Sets the border to use.".
%%  Border = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-spec setBorder(This, Border) -> 'ok' when
	This::wxCalendarDateAttr(), Border::wx:wx_enum().
setBorder(#wx_ref{type=ThisT}=This,Border)
 when is_integer(Border) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,Border,?get_env(),?wxCalendarDateAttr_SetBorder).

-doc "If `holiday` is true, this calendar day will be displayed as a holiday.".
-spec setHoliday(This, Holiday) -> 'ok' when
	This::wxCalendarDateAttr(), Holiday::boolean().
setHoliday(#wx_ref{type=ThisT}=This,Holiday)
 when is_boolean(Holiday) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,Holiday,?get_env(),?wxCalendarDateAttr_SetHoliday).

-doc "Returns true if a non-default text foreground colour is set.".
-spec hasTextColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasTextColour),
  wxe_util:rec(?wxCalendarDateAttr_HasTextColour).

-doc "Returns true if a non-default text background colour is set.".
-spec hasBackgroundColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasBackgroundColour),
  wxe_util:rec(?wxCalendarDateAttr_HasBackgroundColour).

-doc "Returns true if a non-default border colour is set.".
-spec hasBorderColour(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBorderColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasBorderColour),
  wxe_util:rec(?wxCalendarDateAttr_HasBorderColour).

-doc "Returns true if a non-default font is set.".
-spec hasFont(This) -> boolean() when
	This::wxCalendarDateAttr().
hasFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasFont),
  wxe_util:rec(?wxCalendarDateAttr_HasFont).

-doc "Returns true if a non-default (i.e. any) border is set.".
-spec hasBorder(This) -> boolean() when
	This::wxCalendarDateAttr().
hasBorder(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_HasBorder),
  wxe_util:rec(?wxCalendarDateAttr_HasBorder).

-doc "Returns true if this calendar day is displayed as a holiday.".
-spec isHoliday(This) -> boolean() when
	This::wxCalendarDateAttr().
isHoliday(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_IsHoliday),
  wxe_util:rec(?wxCalendarDateAttr_IsHoliday).

-doc "Returns the text colour set for the calendar date.".
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetTextColour),
  wxe_util:rec(?wxCalendarDateAttr_GetTextColour).

-doc "Returns the background colour set for the calendar date.".
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetBackgroundColour),
  wxe_util:rec(?wxCalendarDateAttr_GetBackgroundColour).

-doc "Returns the border colour set for the calendar date.".
-spec getBorderColour(This) -> wx:wx_colour4() when
	This::wxCalendarDateAttr().
getBorderColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetBorderColour),
  wxe_util:rec(?wxCalendarDateAttr_GetBorderColour).

-doc "Returns the font set for the calendar date.".
-spec getFont(This) -> wxFont:wxFont() when
	This::wxCalendarDateAttr().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetFont),
  wxe_util:rec(?wxCalendarDateAttr_GetFont).

-doc "Returns the border set for the calendar date.".
%%  Res = ?wxCAL_BORDER_NONE | ?wxCAL_BORDER_SQUARE | ?wxCAL_BORDER_ROUND
-spec getBorder(This) -> wx:wx_enum() when
	This::wxCalendarDateAttr().
getBorder(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarDateAttr_GetBorder),
  wxe_util:rec(?wxCalendarDateAttr_GetBorder).

-doc "Destroys the object".
-spec destroy(This::wxCalendarDateAttr()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCalendarDateAttr),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxCalendarDateAttr_destroy),
  ok.
