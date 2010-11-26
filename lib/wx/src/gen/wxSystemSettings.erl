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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsystemsettings.html">wxSystemSettings</a>.
%% @type wxSystemSettings().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxSystemSettings).
-include("wxe.hrl").
-export([getColour/1,getFont/1,getMetric/1,getMetric/2,getScreenType/0]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Index::WxSystemColour) -> wx:colour()
%% WxSystemColour = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsystemsettings.html#wxsystemsettingsgetcolour">external documentation</a>.
%%<br /> WxSystemColour is one of ?wxSYS_COLOUR_SCROLLBAR | ?wxSYS_COLOUR_BACKGROUND | ?wxSYS_COLOUR_DESKTOP | ?wxSYS_COLOUR_ACTIVECAPTION | ?wxSYS_COLOUR_INACTIVECAPTION | ?wxSYS_COLOUR_MENU | ?wxSYS_COLOUR_WINDOW | ?wxSYS_COLOUR_WINDOWFRAME | ?wxSYS_COLOUR_MENUTEXT | ?wxSYS_COLOUR_WINDOWTEXT | ?wxSYS_COLOUR_CAPTIONTEXT | ?wxSYS_COLOUR_ACTIVEBORDER | ?wxSYS_COLOUR_INACTIVEBORDER | ?wxSYS_COLOUR_APPWORKSPACE | ?wxSYS_COLOUR_HIGHLIGHT | ?wxSYS_COLOUR_HIGHLIGHTTEXT | ?wxSYS_COLOUR_BTNFACE | ?wxSYS_COLOUR_3DFACE | ?wxSYS_COLOUR_BTNSHADOW | ?wxSYS_COLOUR_3DSHADOW | ?wxSYS_COLOUR_GRAYTEXT | ?wxSYS_COLOUR_BTNTEXT | ?wxSYS_COLOUR_INACTIVECAPTIONTEXT | ?wxSYS_COLOUR_BTNHIGHLIGHT | ?wxSYS_COLOUR_BTNHILIGHT | ?wxSYS_COLOUR_3DHIGHLIGHT | ?wxSYS_COLOUR_3DHILIGHT | ?wxSYS_COLOUR_3DDKSHADOW | ?wxSYS_COLOUR_3DLIGHT | ?wxSYS_COLOUR_INFOTEXT | ?wxSYS_COLOUR_INFOBK | ?wxSYS_COLOUR_LISTBOX | ?wxSYS_COLOUR_HOTLIGHT | ?wxSYS_COLOUR_GRADIENTACTIVECAPTION | ?wxSYS_COLOUR_GRADIENTINACTIVECAPTION | ?wxSYS_COLOUR_MENUHILIGHT | ?wxSYS_COLOUR_MENUBAR | ?wxSYS_COLOUR_LISTBOXTEXT | ?wxSYS_COLOUR_MAX
getColour(Index)
 when is_integer(Index) ->
  wxe_util:call(?wxSystemSettings_GetColour,
  <<Index:32/?UI>>).

%% @spec (Index::WxSystemFont) -> wxFont:wxFont()
%% WxSystemFont = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsystemsettings.html#wxsystemsettingsgetfont">external documentation</a>.
%%<br /> WxSystemFont is one of ?wxSYS_OEM_FIXED_FONT | ?wxSYS_ANSI_FIXED_FONT | ?wxSYS_ANSI_VAR_FONT | ?wxSYS_SYSTEM_FONT | ?wxSYS_DEVICE_DEFAULT_FONT | ?wxSYS_DEFAULT_PALETTE | ?wxSYS_SYSTEM_FIXED_FONT | ?wxSYS_DEFAULT_GUI_FONT | ?wxSYS_ICONTITLE_FONT
getFont(Index)
 when is_integer(Index) ->
  wxe_util:call(?wxSystemSettings_GetFont,
  <<Index:32/?UI>>).

%% @spec (Index::WxSystemMetric) -> integer()
%% @equiv getMetric(Index, [])
getMetric(Index)
 when is_integer(Index) ->
  getMetric(Index, []).

%% @spec (Index::WxSystemMetric, [Option]) -> integer()
%% Option = {win, wxWindow:wxWindow()}
%% WxSystemMetric = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsystemsettings.html#wxsystemsettingsgetmetric">external documentation</a>.
%%<br /> WxSystemMetric is one of ?wxSYS_MOUSE_BUTTONS | ?wxSYS_BORDER_X | ?wxSYS_BORDER_Y | ?wxSYS_CURSOR_X | ?wxSYS_CURSOR_Y | ?wxSYS_DCLICK_X | ?wxSYS_DCLICK_Y | ?wxSYS_DRAG_X | ?wxSYS_DRAG_Y | ?wxSYS_EDGE_X | ?wxSYS_EDGE_Y | ?wxSYS_HSCROLL_ARROW_X | ?wxSYS_HSCROLL_ARROW_Y | ?wxSYS_HTHUMB_X | ?wxSYS_ICON_X | ?wxSYS_ICON_Y | ?wxSYS_ICONSPACING_X | ?wxSYS_ICONSPACING_Y | ?wxSYS_WINDOWMIN_X | ?wxSYS_WINDOWMIN_Y | ?wxSYS_SCREEN_X | ?wxSYS_SCREEN_Y | ?wxSYS_FRAMESIZE_X | ?wxSYS_FRAMESIZE_Y | ?wxSYS_SMALLICON_X | ?wxSYS_SMALLICON_Y | ?wxSYS_HSCROLL_Y | ?wxSYS_VSCROLL_X | ?wxSYS_VSCROLL_ARROW_X | ?wxSYS_VSCROLL_ARROW_Y | ?wxSYS_VTHUMB_Y | ?wxSYS_CAPTION_Y | ?wxSYS_MENU_Y | ?wxSYS_NETWORK_PRESENT | ?wxSYS_PENWINDOWS_PRESENT | ?wxSYS_SHOW_SOUNDS | ?wxSYS_SWAP_BUTTONS
getMetric(Index, Options)
 when is_integer(Index),is_list(Options) ->
  MOpts = fun({win, #wx_ref{type=WinT,ref=WinRef}}, Acc) ->   ?CLASS(WinT,wxWindow),[<<1:32/?UI,WinRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSystemSettings_GetMetric,
  <<Index:32/?UI, 0:32,BinOpt/binary>>).

%% @spec () -> WxSystemScreenType
%% WxSystemScreenType = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsystemsettings.html#wxsystemsettingsgetscreentype">external documentation</a>.
%%<br /> WxSystemScreenType is one of ?wxSYS_SCREEN_NONE | ?wxSYS_SCREEN_TINY | ?wxSYS_SCREEN_PDA | ?wxSYS_SCREEN_SMALL | ?wxSYS_SCREEN_DESKTOP
getScreenType() ->
  wxe_util:call(?wxSystemSettings_GetScreenType,
  <<>>).

