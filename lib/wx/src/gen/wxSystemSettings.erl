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

-module(wxSystemSettings).
-moduledoc """
Functions for wxSystemSettings class

`m:wxSystemSettings` allows the application to ask for details about the system.

This can include settings such as standard colours, fonts, and user interface
element sizes.

See: `m:wxFont`, [`wx_color()`](`t:wx:wx_colour/0`), `m:wxSystemOptions`

wxWidgets docs:
[wxSystemSettings](https://docs.wxwidgets.org/3.1/classwx_system_settings.html)
""".
-include("wxe.hrl").
-export([getColour/1,getFont/1,getMetric/1,getMetric/2,getScreenType/0]).

%% inherited exports
-export([parent_class/1]).

-type wxSystemSettings() :: wx:wx_object().
-export_type([wxSystemSettings/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemsettings.html#wxsystemsettingsgetcolour">external documentation</a>.
%%<br /> Index = ?wxSYS_COLOUR_SCROLLBAR | ?wxSYS_COLOUR_DESKTOP | ?wxSYS_COLOUR_ACTIVECAPTION | ?wxSYS_COLOUR_INACTIVECAPTION | ?wxSYS_COLOUR_MENU | ?wxSYS_COLOUR_WINDOW | ?wxSYS_COLOUR_WINDOWFRAME | ?wxSYS_COLOUR_MENUTEXT | ?wxSYS_COLOUR_WINDOWTEXT | ?wxSYS_COLOUR_CAPTIONTEXT | ?wxSYS_COLOUR_ACTIVEBORDER | ?wxSYS_COLOUR_INACTIVEBORDER | ?wxSYS_COLOUR_APPWORKSPACE | ?wxSYS_COLOUR_HIGHLIGHT | ?wxSYS_COLOUR_HIGHLIGHTTEXT | ?wxSYS_COLOUR_BTNFACE | ?wxSYS_COLOUR_BTNSHADOW | ?wxSYS_COLOUR_GRAYTEXT | ?wxSYS_COLOUR_BTNTEXT | ?wxSYS_COLOUR_INACTIVECAPTIONTEXT | ?wxSYS_COLOUR_BTNHIGHLIGHT | ?wxSYS_COLOUR_3DDKSHADOW | ?wxSYS_COLOUR_3DLIGHT | ?wxSYS_COLOUR_INFOTEXT | ?wxSYS_COLOUR_INFOBK | ?wxSYS_COLOUR_LISTBOX | ?wxSYS_COLOUR_HOTLIGHT | ?wxSYS_COLOUR_GRADIENTACTIVECAPTION | ?wxSYS_COLOUR_GRADIENTINACTIVECAPTION | ?wxSYS_COLOUR_MENUHILIGHT | ?wxSYS_COLOUR_MENUBAR | ?wxSYS_COLOUR_LISTBOXTEXT | ?wxSYS_COLOUR_LISTBOXHIGHLIGHTTEXT | ?wxSYS_COLOUR_BACKGROUND | ?wxSYS_COLOUR_3DFACE | ?wxSYS_COLOUR_3DSHADOW | ?wxSYS_COLOUR_BTNHILIGHT | ?wxSYS_COLOUR_3DHIGHLIGHT | ?wxSYS_COLOUR_3DHILIGHT | ?wxSYS_COLOUR_FRAMEBK
-doc """
Returns a system colour.

Return: The returned colour is always valid.
""".
-spec getColour(Index) -> wx:wx_colour4() when
	Index::wx:wx_enum().
getColour(Index)
 when is_integer(Index) ->
  wxe_util:queue_cmd(Index,?get_env(),?wxSystemSettings_GetColour),
  wxe_util:rec(?wxSystemSettings_GetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemsettings.html#wxsystemsettingsgetfont">external documentation</a>.
%%<br /> Index = ?wxSYS_OEM_FIXED_FONT | ?wxSYS_ANSI_FIXED_FONT | ?wxSYS_ANSI_VAR_FONT | ?wxSYS_SYSTEM_FONT | ?wxSYS_DEVICE_DEFAULT_FONT | ?wxSYS_DEFAULT_GUI_FONT
-doc """
Returns a system font.

Return: The returned font is always valid.
""".
-spec getFont(Index) -> wxFont:wxFont() when
	Index::wx:wx_enum().
getFont(Index)
 when is_integer(Index) ->
  wxe_util:queue_cmd(Index,?get_env(),?wxSystemSettings_GetFont),
  wxe_util:rec(?wxSystemSettings_GetFont).

%% @equiv getMetric(Index, [])
-spec getMetric(Index) -> integer() when
	Index::wx:wx_enum().

getMetric(Index)
 when is_integer(Index) ->
  getMetric(Index, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemsettings.html#wxsystemsettingsgetmetric">external documentation</a>.
%%<br /> Index = ?wxSYS_MOUSE_BUTTONS | ?wxSYS_BORDER_X | ?wxSYS_BORDER_Y | ?wxSYS_CURSOR_X | ?wxSYS_CURSOR_Y | ?wxSYS_DCLICK_X | ?wxSYS_DCLICK_Y | ?wxSYS_DRAG_X | ?wxSYS_DRAG_Y | ?wxSYS_EDGE_X | ?wxSYS_EDGE_Y | ?wxSYS_HSCROLL_ARROW_X | ?wxSYS_HSCROLL_ARROW_Y | ?wxSYS_HTHUMB_X | ?wxSYS_ICON_X | ?wxSYS_ICON_Y | ?wxSYS_ICONSPACING_X | ?wxSYS_ICONSPACING_Y | ?wxSYS_WINDOWMIN_X | ?wxSYS_WINDOWMIN_Y | ?wxSYS_SCREEN_X | ?wxSYS_SCREEN_Y | ?wxSYS_FRAMESIZE_X | ?wxSYS_FRAMESIZE_Y | ?wxSYS_SMALLICON_X | ?wxSYS_SMALLICON_Y | ?wxSYS_HSCROLL_Y | ?wxSYS_VSCROLL_X | ?wxSYS_VSCROLL_ARROW_X | ?wxSYS_VSCROLL_ARROW_Y | ?wxSYS_VTHUMB_Y | ?wxSYS_CAPTION_Y | ?wxSYS_MENU_Y | ?wxSYS_NETWORK_PRESENT | ?wxSYS_PENWINDOWS_PRESENT | ?wxSYS_SHOW_SOUNDS | ?wxSYS_SWAP_BUTTONS | ?wxSYS_DCLICK_MSEC | ?wxSYS_CARET_ON_MSEC | ?wxSYS_CARET_OFF_MSEC | ?wxSYS_CARET_TIMEOUT_MSEC
-doc """
Returns the value of a system metric, or -1 if the metric is not supported on
the current system.

The value of `win` determines if the metric returned is a global value or a
`m:wxWindow` based value, in which case it might determine the widget, the
display the window is on, or something similar. The window given should be as
close to the metric as possible (e.g. a `m:wxTopLevelWindow` in case of the
wxSYS_CAPTION_Y metric).

`index` can be one of the ?wxSystemMetric enum values.

`win` is a pointer to the window for which the metric is requested. Specifying
the `win` parameter is encouraged, because some metrics on some ports are not
supported without one,or they might be capable of reporting better values if
given one. If a window does not make sense for a metric, one should still be
given, as for example it might determine which displays cursor width is
requested with wxSYS_CURSOR_X.
""".
-spec getMetric(Index, [Option]) -> integer() when
	Index::wx:wx_enum(),
	Option :: {'win', wxWindow:wxWindow()}.
getMetric(Index, Options)
 when is_integer(Index),is_list(Options) ->
  MOpts = fun({win, #wx_ref{type=WinT}} = Arg) ->   ?CLASS(WinT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Index, Opts,?get_env(),?wxSystemSettings_GetMetric),
  wxe_util:rec(?wxSystemSettings_GetMetric).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemsettings.html#wxsystemsettingsgetscreentype">external documentation</a>.
%%<br /> Res = ?wxSYS_SCREEN_NONE | ?wxSYS_SCREEN_TINY | ?wxSYS_SCREEN_PDA | ?wxSYS_SCREEN_SMALL | ?wxSYS_SCREEN_DESKTOP
-doc """
Returns the screen type.

The return value is one of the ?wxSystemScreenType enum values.
""".
-spec getScreenType() -> wx:wx_enum().
getScreenType() ->
  wxe_util:queue_cmd(?get_env(), ?wxSystemSettings_GetScreenType),
  wxe_util:rec(?wxSystemSettings_GetScreenType).

