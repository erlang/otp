%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html">Misc</a>.

%% This module contains wxWidgets utility functions.

-module(wx_misc).
-include("wxe.hrl").
-export([beginBusyCursor/0,beginBusyCursor/1,bell/0,endBusyCursor/0,findMenuItemId/3,
  findWindowAtPoint/1,getCurrentId/0,getEmailAddress/0,getHomeDir/0,
  getKeyState/1,getMousePosition/0,getMouseState/0,getOsDescription/0,
  getUserId/0,isBusy/0,isPlatform64Bit/0,isPlatformLittleEndian/0,launchDefaultBrowser/1,
  launchDefaultBrowser/2,newId/0,registerId/1,setDetectableAutoRepeat/1,
  shell/0,shell/1,shutdown/0,shutdown/1]).

-export([displaySize/0,setCursor/1]).

-export([mSWSetEmulationLevel/1, mSWSetEmulationLevel/2]).

%% @doc See <a href="https://docs.wxwidgets.org/3.1.4/classwx_web_view_i_e.html#a7a45d02cb7dd6dbfcc09566449a1f3bd">external documentation</a>.
%%<br /> Level = ?wxWEBVIEWIE_EMU_DEFAULT | ?wxWEBVIEWIE_EMU_IE7 | ?wxWEBVIEWIE_EMU_IE8 | ?wxWEBVIEWIE_EMU_IE8_FORCE | ?wxWEBVIEWIE_EMU_IE9 | ?wxWEBVIEWIE_EMU_IE9_FORCE | ?wxWEBVIEWIE_EMU_IE10 | ?wxWEBVIEWIE_EMU_IE10_FORCE | ?wxWEBVIEWIE_EMU_IE11 | ?wxWEBVIEWIE_EMU_IE11_FORCE
-spec mSWSetEmulationLevel(Level) -> boolean() when
	Level :: wx:wx_enum().
mSWSetEmulationLevel(Level) when is_integer(Level) ->
  mSWSetEmulationLevel(Level, "erl.exe"),
  mSWSetEmulationLevel(Level, "werl.exe"),
  true.

%% @doc See <a href="https://docs.wxwidgets.org/3.1.4/classwx_web_view_i_e.html#a7a45d02cb7dd6dbfcc09566449a1f3bd">external documentation</a>.
%%<br /> Level = ?wxWEBVIEWIE_EMU_DEFAULT | ?wxWEBVIEWIE_EMU_IE7 | ?wxWEBVIEWIE_EMU_IE8 | ?wxWEBVIEWIE_EMU_IE8_FORCE | ?wxWEBVIEWIE_EMU_IE9 | ?wxWEBVIEWIE_EMU_IE9_FORCE | ?wxWEBVIEWIE_EMU_IE10 | ?wxWEBVIEWIE_EMU_IE10_FORCE | ?wxWEBVIEWIE_EMU_IE11 | ?wxWEBVIEWIE_EMU_IE11_FORCE
-spec mSWSetEmulationLevel(Level, Executable) -> boolean() when
	Level :: wx:wx_enum(),
  Executable :: string().
mSWSetEmulationLevel(Level, Executable) ->
  {ok, Reg} = win32reg:open([write]),
  ok = win32reg:change_key(Reg, "\\hkey_current_user\\software\\microsoft\\internet explorer\\main\\featurecontrol\\"),
  ok = win32reg:change_key_create(Reg, "FEATURE_BROWSER_EMULATION"),
  ok = win32reg:set_value(Reg, Executable, Level),
  ok = win32reg:close(Reg),
  true.
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetkeystate">external documentation</a>.
%%<br /> Key = ?WXK_NONE | ?WXK_CONTROL_A | ?WXK_CONTROL_B | ?WXK_CONTROL_C | ?WXK_CONTROL_D | ?WXK_CONTROL_E | ?WXK_CONTROL_F | ?WXK_CONTROL_G | ?WXK_CONTROL_H | ?WXK_CONTROL_I | ?WXK_CONTROL_J | ?WXK_CONTROL_K | ?WXK_CONTROL_L | ?WXK_CONTROL_M | ?WXK_CONTROL_N | ?WXK_CONTROL_O | ?WXK_CONTROL_P | ?WXK_CONTROL_Q | ?WXK_CONTROL_R | ?WXK_CONTROL_S | ?WXK_CONTROL_T | ?WXK_CONTROL_U | ?WXK_CONTROL_V | ?WXK_CONTROL_W | ?WXK_CONTROL_X | ?WXK_CONTROL_Y | ?WXK_CONTROL_Z | ?WXK_BACK | ?WXK_TAB | ?WXK_RETURN | ?WXK_ESCAPE | ?WXK_SPACE | ?WXK_DELETE | ?WXK_START | ?WXK_LBUTTON | ?WXK_RBUTTON | ?WXK_CANCEL | ?WXK_MBUTTON | ?WXK_CLEAR | ?WXK_SHIFT | ?WXK_ALT | ?WXK_CONTROL | ?WXK_MENU | ?WXK_PAUSE | ?WXK_CAPITAL | ?WXK_END | ?WXK_HOME | ?WXK_LEFT | ?WXK_UP | ?WXK_RIGHT | ?WXK_DOWN | ?WXK_SELECT | ?WXK_PRINT | ?WXK_EXECUTE | ?WXK_SNAPSHOT | ?WXK_INSERT | ?WXK_HELP | ?WXK_NUMPAD0 | ?WXK_NUMPAD1 | ?WXK_NUMPAD2 | ?WXK_NUMPAD3 | ?WXK_NUMPAD4 | ?WXK_NUMPAD5 | ?WXK_NUMPAD6 | ?WXK_NUMPAD7 | ?WXK_NUMPAD8 | ?WXK_NUMPAD9 | ?WXK_MULTIPLY | ?WXK_ADD | ?WXK_SEPARATOR | ?WXK_SUBTRACT | ?WXK_DECIMAL | ?WXK_DIVIDE | ?WXK_F1 | ?WXK_F2 | ?WXK_F3 | ?WXK_F4 | ?WXK_F5 | ?WXK_F6 | ?WXK_F7 | ?WXK_F8 | ?WXK_F9 | ?WXK_F10 | ?WXK_F11 | ?WXK_F12 | ?WXK_F13 | ?WXK_F14 | ?WXK_F15 | ?WXK_F16 | ?WXK_F17 | ?WXK_F18 | ?WXK_F19 | ?WXK_F20 | ?WXK_F21 | ?WXK_F22 | ?WXK_F23 | ?WXK_F24 | ?WXK_NUMLOCK | ?WXK_SCROLL | ?WXK_PAGEUP | ?WXK_PAGEDOWN | ?WXK_NUMPAD_SPACE | ?WXK_NUMPAD_TAB | ?WXK_NUMPAD_ENTER | ?WXK_NUMPAD_F1 | ?WXK_NUMPAD_F2 | ?WXK_NUMPAD_F3 | ?WXK_NUMPAD_F4 | ?WXK_NUMPAD_HOME | ?WXK_NUMPAD_LEFT | ?WXK_NUMPAD_UP | ?WXK_NUMPAD_RIGHT | ?WXK_NUMPAD_DOWN | ?WXK_NUMPAD_PAGEUP | ?WXK_NUMPAD_PAGEDOWN | ?WXK_NUMPAD_END | ?WXK_NUMPAD_BEGIN | ?WXK_NUMPAD_INSERT | ?WXK_NUMPAD_DELETE | ?WXK_NUMPAD_EQUAL | ?WXK_NUMPAD_MULTIPLY | ?WXK_NUMPAD_ADD | ?WXK_NUMPAD_SEPARATOR | ?WXK_NUMPAD_SUBTRACT | ?WXK_NUMPAD_DECIMAL | ?WXK_NUMPAD_DIVIDE | ?WXK_WINDOWS_LEFT | ?WXK_WINDOWS_RIGHT | ?WXK_WINDOWS_MENU | ?WXK_RAW_CONTROL | ?WXK_COMMAND | ?WXK_SPECIAL1 | ?WXK_SPECIAL2 | ?WXK_SPECIAL3 | ?WXK_SPECIAL4 | ?WXK_SPECIAL5 | ?WXK_SPECIAL6 | ?WXK_SPECIAL7 | ?WXK_SPECIAL8 | ?WXK_SPECIAL9 | ?WXK_SPECIAL10 | ?WXK_SPECIAL11 | ?WXK_SPECIAL12 | ?WXK_SPECIAL13 | ?WXK_SPECIAL14 | ?WXK_SPECIAL15 | ?WXK_SPECIAL16 | ?WXK_SPECIAL17 | ?WXK_SPECIAL18 | ?WXK_SPECIAL19 | ?WXK_SPECIAL20 | ?WXK_BROWSER_BACK | ?WXK_BROWSER_FORWARD | ?WXK_BROWSER_REFRESH | ?WXK_BROWSER_STOP | ?WXK_BROWSER_SEARCH | ?WXK_BROWSER_FAVORITES | ?WXK_BROWSER_HOME | ?WXK_VOLUME_MUTE | ?WXK_VOLUME_DOWN | ?WXK_VOLUME_UP | ?WXK_MEDIA_NEXT_TRACK | ?WXK_MEDIA_PREV_TRACK | ?WXK_MEDIA_STOP | ?WXK_MEDIA_PLAY_PAUSE | ?WXK_LAUNCH_MAIL | ?WXK_LAUNCH_APP1 | ?WXK_LAUNCH_APP2
-spec getKeyState(Key) -> boolean() when
	Key::wx:wx_enum().
getKeyState(Key)
 when is_integer(Key) ->
  wxe_util:queue_cmd(Key,?get_env(),?utils_wxGetKeyState),
  wxe_util:rec(?utils_wxGetKeyState).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetmouseposition">external documentation</a>.
-spec getMousePosition() -> {X::integer(), Y::integer()}.
getMousePosition() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetMousePosition),
  wxe_util:rec(?utils_wxGetMousePosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetmousestate">external documentation</a>.
-spec getMouseState() -> wx:wx_wxMouseState().
getMouseState() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetMouseState),
  wxe_util:rec(?utils_wxGetMouseState).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxsetdetectableautorepeat">external documentation</a>.
-spec setDetectableAutoRepeat(Flag) -> boolean() when
	Flag::boolean().
setDetectableAutoRepeat(Flag)
 when is_boolean(Flag) ->
  wxe_util:queue_cmd(Flag,?get_env(),?utils_wxSetDetectableAutoRepeat),
  wxe_util:rec(?utils_wxSetDetectableAutoRepeat).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxbell">external documentation</a>.
-spec bell() -> 'ok'.
bell() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxBell).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxfindmenuitemid">external documentation</a>.
-spec findMenuItemId(Frame, MenuString, ItemString) -> integer() when
	Frame::wxFrame:wxFrame(), MenuString::unicode:chardata(), ItemString::unicode:chardata().
findMenuItemId(#wx_ref{type=FrameT}=Frame,MenuString,ItemString)
 when ?is_chardata(MenuString),?is_chardata(ItemString) ->
  ?CLASS(FrameT,wxFrame),
  MenuString_UC = unicode:characters_to_binary(MenuString),
  ItemString_UC = unicode:characters_to_binary(ItemString),
  wxe_util:queue_cmd(Frame,MenuString_UC,ItemString_UC,?get_env(),?utils_wxFindMenuItemId),
  wxe_util:rec(?utils_wxFindMenuItemId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxfindwindowatpoint">external documentation</a>.
-spec findWindowAtPoint(Pt) -> wxWindow:wxWindow() when
	Pt::{X::integer(), Y::integer()}.
findWindowAtPoint({PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:queue_cmd(Pt,?get_env(),?utils_wxFindWindowAtPoint),
  wxe_util:rec(?utils_wxFindWindowAtPoint).

%% @equiv beginBusyCursor([])
-spec beginBusyCursor() -> 'ok'.

beginBusyCursor() ->
  beginBusyCursor([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxbeginbusycursor">external documentation</a>.
-spec beginBusyCursor([Option]) -> 'ok' when
	Option :: {'cursor', wxCursor:wxCursor()}.
beginBusyCursor(Options)
 when is_list(Options) ->
  MOpts = fun({cursor, #wx_ref{type=CursorT}} = Arg) ->   ?CLASS(CursorT,wxCursor),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?utils_wxBeginBusyCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxendbusycursor">external documentation</a>.
-spec endBusyCursor() -> 'ok'.
endBusyCursor() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxEndBusyCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxisbusy">external documentation</a>.
-spec isBusy() -> boolean().
isBusy() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxIsBusy),
  wxe_util:rec(?utils_wxIsBusy).

%% @equiv shutdown([])
-spec shutdown() -> boolean().

shutdown() ->
  shutdown([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxshutdown">external documentation</a>.
-spec shutdown([Option]) -> boolean() when
	Option :: {'flags', integer()}.
shutdown(Options)
 when is_list(Options) ->
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?utils_wxShutdown),
  wxe_util:rec(?utils_wxShutdown).

%% @equiv shell([])
-spec shell() -> boolean().

shell() ->
  shell([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxshell">external documentation</a>.
-spec shell([Option]) -> boolean() when
	Option :: {'command', unicode:chardata()}.
shell(Options)
 when is_list(Options) ->
  MOpts = fun({command, Command}) ->   Command_UC = unicode:characters_to_binary(Command),{command,Command_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?utils_wxShell),
  wxe_util:rec(?utils_wxShell).

%% @equiv launchDefaultBrowser(Url, [])
-spec launchDefaultBrowser(Url) -> boolean() when
	Url::unicode:chardata().

launchDefaultBrowser(Url)
 when ?is_chardata(Url) ->
  launchDefaultBrowser(Url, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxlaunchdefaultbrowser">external documentation</a>.
-spec launchDefaultBrowser(Url, [Option]) -> boolean() when
	Url::unicode:chardata(),
	Option :: {'flags', integer()}.
launchDefaultBrowser(Url, Options)
 when ?is_chardata(Url),is_list(Options) ->
  Url_UC = unicode:characters_to_binary(Url),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Url_UC, Opts,?get_env(),?utils_wxLaunchDefaultBrowser),
  wxe_util:rec(?utils_wxLaunchDefaultBrowser).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetemailaddress">external documentation</a>.
-spec getEmailAddress() -> unicode:charlist().
getEmailAddress() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetEmailAddress),
  wxe_util:rec(?utils_wxGetEmailAddress).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetuserid">external documentation</a>.
-spec getUserId() -> unicode:charlist().
getUserId() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetUserId),
  wxe_util:rec(?utils_wxGetUserId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgethomedir">external documentation</a>.
-spec getHomeDir() -> unicode:charlist().
getHomeDir() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetHomeDir),
  wxe_util:rec(?utils_wxGetHomeDir).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxnewid">external documentation</a>.
-spec newId() -> integer().
newId() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxNewId),
  wxe_util:rec(?utils_wxNewId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxregisterid">external documentation</a>.
-spec registerId(Id) -> 'ok' when
	Id::integer().
registerId(Id)
 when is_integer(Id) ->
  wxe_util:queue_cmd(Id,?get_env(),?utils_wxRegisterId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetcurrentid">external documentation</a>.
-spec getCurrentId() -> integer().
getCurrentId() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetCurrentId),
  wxe_util:rec(?utils_wxGetCurrentId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetosdescription">external documentation</a>.
-spec getOsDescription() -> unicode:charlist().
getOsDescription() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetOsDescription),
  wxe_util:rec(?utils_wxGetOsDescription).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxisplatformlittleendian">external documentation</a>.
-spec isPlatformLittleEndian() -> boolean().
isPlatformLittleEndian() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxIsPlatformLittleEndian),
  wxe_util:rec(?utils_wxIsPlatformLittleEndian).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxisplatform64bit">external documentation</a>.
-spec isPlatform64Bit() -> boolean().
isPlatform64Bit() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxIsPlatform64Bit),
  wxe_util:rec(?utils_wxIsPlatform64Bit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_gdicmn.html#gdicmnwxdisplaysize">external documentation</a>.
-spec displaySize() -> {Width::integer(), Height::integer()}.
displaySize() ->
  wxe_util:queue_cmd(?get_env(), ?gdicmn_wxDisplaySize),
  wxe_util:rec(?gdicmn_wxDisplaySize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_gdicmn.html#gdicmnwxsetcursor">external documentation</a>.
-spec setCursor(Cursor) -> 'ok' when
	Cursor::wxCursor:wxCursor().
setCursor(#wx_ref{type=CursorT}=Cursor) ->
  ?CLASS(CursorT,wxCursor),
  wxe_util:queue_cmd(Cursor,?get_env(),?gdicmn_wxSetCursor).

