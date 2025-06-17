%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html">Misc</a>.

%% This module contains wxWidgets utility functions.

-module(wx_misc).
-include("wxe.hrl").

-moduledoc """
Miscellaneous functions.

""".
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
-doc """
For normal keys, returns true if the specified key is currently down.

For togglable keys (Caps Lock, Num Lock and Scroll Lock), returns true if the key is
toggled such that its LED indicator is lit. There is currently no way to test whether
togglable keys are up or down.

Even though there are virtual key codes defined for mouse buttons, they cannot be used
with this function currently.

In wxGTK, this function can be only used with modifier keys (`WXK_ALT`, `WXK_CONTROL` and `WXK_SHIFT`)
when not using X11 backend currently.
""".
%%  Key = ?WXK_NONE | ?WXK_CONTROL_A | ?WXK_CONTROL_B | ?WXK_CONTROL_C | ?WXK_CONTROL_D | ?WXK_CONTROL_E | ?WXK_CONTROL_F | ?WXK_CONTROL_G | ?WXK_CONTROL_H | ?WXK_CONTROL_I | ?WXK_CONTROL_J | ?WXK_CONTROL_K | ?WXK_CONTROL_L | ?WXK_CONTROL_M | ?WXK_CONTROL_N | ?WXK_CONTROL_O | ?WXK_CONTROL_P | ?WXK_CONTROL_Q | ?WXK_CONTROL_R | ?WXK_CONTROL_S | ?WXK_CONTROL_T | ?WXK_CONTROL_U | ?WXK_CONTROL_V | ?WXK_CONTROL_W | ?WXK_CONTROL_X | ?WXK_CONTROL_Y | ?WXK_CONTROL_Z | ?WXK_BACK | ?WXK_TAB | ?WXK_RETURN | ?WXK_ESCAPE | ?WXK_SPACE | ?WXK_DELETE | ?WXK_START | ?WXK_LBUTTON | ?WXK_RBUTTON | ?WXK_CANCEL | ?WXK_MBUTTON | ?WXK_CLEAR | ?WXK_SHIFT | ?WXK_ALT | ?WXK_CONTROL | ?WXK_MENU | ?WXK_PAUSE | ?WXK_CAPITAL | ?WXK_END | ?WXK_HOME | ?WXK_LEFT | ?WXK_UP | ?WXK_RIGHT | ?WXK_DOWN | ?WXK_SELECT | ?WXK_PRINT | ?WXK_EXECUTE | ?WXK_SNAPSHOT | ?WXK_INSERT | ?WXK_HELP | ?WXK_NUMPAD0 | ?WXK_NUMPAD1 | ?WXK_NUMPAD2 | ?WXK_NUMPAD3 | ?WXK_NUMPAD4 | ?WXK_NUMPAD5 | ?WXK_NUMPAD6 | ?WXK_NUMPAD7 | ?WXK_NUMPAD8 | ?WXK_NUMPAD9 | ?WXK_MULTIPLY | ?WXK_ADD | ?WXK_SEPARATOR | ?WXK_SUBTRACT | ?WXK_DECIMAL | ?WXK_DIVIDE | ?WXK_F1 | ?WXK_F2 | ?WXK_F3 | ?WXK_F4 | ?WXK_F5 | ?WXK_F6 | ?WXK_F7 | ?WXK_F8 | ?WXK_F9 | ?WXK_F10 | ?WXK_F11 | ?WXK_F12 | ?WXK_F13 | ?WXK_F14 | ?WXK_F15 | ?WXK_F16 | ?WXK_F17 | ?WXK_F18 | ?WXK_F19 | ?WXK_F20 | ?WXK_F21 | ?WXK_F22 | ?WXK_F23 | ?WXK_F24 | ?WXK_NUMLOCK | ?WXK_SCROLL | ?WXK_PAGEUP | ?WXK_PAGEDOWN | ?WXK_NUMPAD_SPACE | ?WXK_NUMPAD_TAB | ?WXK_NUMPAD_ENTER | ?WXK_NUMPAD_F1 | ?WXK_NUMPAD_F2 | ?WXK_NUMPAD_F3 | ?WXK_NUMPAD_F4 | ?WXK_NUMPAD_HOME | ?WXK_NUMPAD_LEFT | ?WXK_NUMPAD_UP | ?WXK_NUMPAD_RIGHT | ?WXK_NUMPAD_DOWN | ?WXK_NUMPAD_PAGEUP | ?WXK_NUMPAD_PAGEDOWN | ?WXK_NUMPAD_END | ?WXK_NUMPAD_BEGIN | ?WXK_NUMPAD_INSERT | ?WXK_NUMPAD_DELETE | ?WXK_NUMPAD_EQUAL | ?WXK_NUMPAD_MULTIPLY | ?WXK_NUMPAD_ADD | ?WXK_NUMPAD_SEPARATOR | ?WXK_NUMPAD_SUBTRACT | ?WXK_NUMPAD_DECIMAL | ?WXK_NUMPAD_DIVIDE | ?WXK_WINDOWS_LEFT | ?WXK_WINDOWS_RIGHT | ?WXK_WINDOWS_MENU | ?WXK_RAW_CONTROL | ?WXK_COMMAND | ?WXK_SPECIAL1 | ?WXK_SPECIAL2 | ?WXK_SPECIAL3 | ?WXK_SPECIAL4 | ?WXK_SPECIAL5 | ?WXK_SPECIAL6 | ?WXK_SPECIAL7 | ?WXK_SPECIAL8 | ?WXK_SPECIAL9 | ?WXK_SPECIAL10 | ?WXK_SPECIAL11 | ?WXK_SPECIAL12 | ?WXK_SPECIAL13 | ?WXK_SPECIAL14 | ?WXK_SPECIAL15 | ?WXK_SPECIAL16 | ?WXK_SPECIAL17 | ?WXK_SPECIAL18 | ?WXK_SPECIAL19 | ?WXK_SPECIAL20 | ?WXK_BROWSER_BACK | ?WXK_BROWSER_FORWARD | ?WXK_BROWSER_REFRESH | ?WXK_BROWSER_STOP | ?WXK_BROWSER_SEARCH | ?WXK_BROWSER_FAVORITES | ?WXK_BROWSER_HOME | ?WXK_VOLUME_MUTE | ?WXK_VOLUME_DOWN | ?WXK_VOLUME_UP | ?WXK_MEDIA_NEXT_TRACK | ?WXK_MEDIA_PREV_TRACK | ?WXK_MEDIA_STOP | ?WXK_MEDIA_PLAY_PAUSE | ?WXK_LAUNCH_MAIL | ?WXK_LAUNCH_APP1 | ?WXK_LAUNCH_APP2
-spec getKeyState(Key) -> boolean() when
	Key::wx:wx_enum().
getKeyState(Key)
 when is_integer(Key) ->
  wxe_util:queue_cmd(Key,?get_env(),?utils_wxGetKeyState),
  wxe_util:rec(?utils_wxGetKeyState).

-doc "Returns the mouse position in screen coordinates.".
-spec getMousePosition() -> {X::integer(), Y::integer()}.
getMousePosition() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetMousePosition),
  wxe_util:rec(?utils_wxGetMousePosition).

-doc """
Returns the current state of the mouse.

Returns a `wx_wxMouseState()` instance that contains the current position of the mouse pointer in screen
coordinates, as well as boolean values indicating the up/down status of the mouse buttons
and the modifier keys.
""".
-spec getMouseState() -> wx:wx_wxMouseState().
getMouseState() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetMouseState),
  wxe_util:rec(?utils_wxGetMouseState).

-doc """
Don't synthesize KeyUp events holding down a key and producing KeyDown events with
autorepeat.

On by default and always on in wxMSW.
""".
-spec setDetectableAutoRepeat(Flag) -> boolean() when
	Flag::boolean().
setDetectableAutoRepeat(Flag)
 when is_boolean(Flag) ->
  wxe_util:queue_cmd(Flag,?get_env(),?utils_wxSetDetectableAutoRepeat),
  wxe_util:rec(?utils_wxSetDetectableAutoRepeat).

-doc """
Ring the system bell.

Note: This function is categorized as a GUI one and so is not thread-safe.
""".
-spec bell() -> 'ok'.
bell() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxBell).

-doc "Find a menu item identifier associated with the given frame's menu bar.".
-spec findMenuItemId(Frame, MenuString, ItemString) -> integer() when
	Frame::wxFrame:wxFrame(), MenuString::unicode:chardata(), ItemString::unicode:chardata().
findMenuItemId(#wx_ref{type=FrameT}=Frame,MenuString,ItemString)
 when ?is_chardata(MenuString),?is_chardata(ItemString) ->
  ?CLASS(FrameT,wxFrame),
  MenuString_UC = unicode:characters_to_binary(MenuString),
  ItemString_UC = unicode:characters_to_binary(ItemString),
  wxe_util:queue_cmd(Frame,MenuString_UC,ItemString_UC,?get_env(),?utils_wxFindMenuItemId),
  wxe_util:rec(?utils_wxFindMenuItemId).

-doc """
Find the deepest window at the given mouse position in screen coordinates, returning the
window if found, or NULL if not.

This function takes child windows at the given position into account even if they are
disabled. The hidden children are however skipped by it.
""".
-spec findWindowAtPoint(Pt) -> wxWindow:wxWindow() when
	Pt::{X::integer(), Y::integer()}.
findWindowAtPoint({PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:queue_cmd(Pt,?get_env(),?utils_wxFindWindowAtPoint),
  wxe_util:rec(?utils_wxFindWindowAtPoint).

-doc(#{equiv => beginBusyCursor([])}).
-spec beginBusyCursor() -> 'ok'.

beginBusyCursor() ->
  beginBusyCursor([]).

-doc """
Changes the cursor to the given cursor for all windows in the application.

Use `wx_misc:endBusyCursor/0` to revert the cursor back to its previous state. These two calls can be nested, and
a counter ensures that only the outer calls take effect.

See: `wx_misc:isBusy/0`
""".
-spec beginBusyCursor([Option]) -> 'ok' when
	Option :: {'cursor', wxCursor:wxCursor()}.
beginBusyCursor(Options)
 when is_list(Options) ->
  MOpts = fun({cursor, #wx_ref{type=CursorT}} = Arg) ->   ?CLASS(CursorT,wxCursor),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?utils_wxBeginBusyCursor).

-doc """
Changes the cursor back to the original cursor, for all windows in the application.

Use with `wx_misc:beginBusyCursor/1`.

See: `wx_misc:isBusy/0`
""".
-spec endBusyCursor() -> 'ok'.
endBusyCursor() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxEndBusyCursor).

-doc """
Returns true if between two `wx_misc:beginBusyCursor/1` and `wx_misc:endBusyCursor/0`
calls.
""".
-spec isBusy() -> boolean().
isBusy() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxIsBusy),
  wxe_util:rec(?utils_wxIsBusy).

-doc(#{equiv => shutdown([])}).
-spec shutdown() -> boolean().

shutdown() ->
  shutdown([]).

-doc """
This function shuts down or reboots the computer depending on the value of the `flags`.

Note: Note that performing the shutdown requires the corresponding access rights
(superuser under Unix, SE_SHUTDOWN privilege under Windows) and that this function is only
implemented under Unix and MSW.

Return: true on success, false if an error occurred.
""".
-spec shutdown([Option]) -> boolean() when
	Option :: {'flags', integer()}.
shutdown(Options)
 when is_list(Options) ->
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?utils_wxShutdown),
  wxe_util:rec(?utils_wxShutdown).

-doc(#{equiv => shell([])}).
-spec shell() -> boolean().

shell() ->
  shell([]).

-doc """
Executes a command in an interactive shell window.

If no command is specified, then just the shell is spawned.

See: [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_exec)
""".
-spec shell([Option]) -> boolean() when
	Option :: {'command', unicode:chardata()}.
shell(Options)
 when is_list(Options) ->
  MOpts = fun({command, Command}) ->   Command_UC = unicode:characters_to_binary(Command),{command,Command_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?utils_wxShell),
  wxe_util:rec(?utils_wxShell).

-doc(#{equiv => launchDefaultBrowser(Url, [])}).
-spec launchDefaultBrowser(Url) -> boolean() when
	Url::unicode:chardata().

launchDefaultBrowser(Url)
 when ?is_chardata(Url) ->
  launchDefaultBrowser(Url, []).

-doc """
Opens the `url` in user's default browser.

If the `flags` parameter contains `wxBROWSER_NEW_WINDOW` flag, a new window is opened for
the URL (currently this is only supported under Windows).

And unless the `flags` parameter contains `wxBROWSER_NOBUSYCURSOR` flag, a busy cursor is
shown while the browser is being launched (using `wxBusyCursor` (not implemented in wx)).

The parameter `url` is interpreted as follows:

* if it has a valid scheme (e.g. `"file:"`, `"http:"` or `"mailto:"`) it is passed to the
appropriate browser configured in the user system.

* if it has no valid scheme (e.g. it's a local file path without the `"file:"` prefix),
then ?wxFileExists and ?wxDirExists are used to test if it's a local file/directory; if it
is, then the browser is called with the `url` parameter eventually prefixed by `"file:"`.

* if it has no valid scheme and it's not a local file/directory, then `"http:"` is
prepended and the browser is called.

Returns true if the application was successfully launched.

Note: For some configurations of the running user, the application which is launched to
open the given URL may be URL-dependent (e.g. a browser may be used for local URLs while
another one may be used for remote URLs).
""".
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

-doc """
Copies the user's email address into the supplied buffer, by concatenating the values
returned by `wxGetFullHostName()` (not implemented in wx) and `wx_misc:getUserId/0`.

Return: true if successful, false otherwise.
""".
-spec getEmailAddress() -> unicode:charlist().
getEmailAddress() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetEmailAddress),
  wxe_util:rec(?utils_wxGetEmailAddress).

-doc """
This function returns the "user id" also known as "login name" under Unix (i.e.

something like "jsmith"). It uniquely identifies the current user (on this system). Under
Windows or NT, this function first looks in the environment variables USER and LOGNAME; if
neither of these is found, the entry `UserId` in the `wxWidgets` section of the WIN.INI
file is tried.

Return: The login name if successful or an empty string otherwise.
""".
-spec getUserId() -> unicode:charlist().
getUserId() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetUserId),
  wxe_util:rec(?utils_wxGetUserId).

-doc "Return the (current) user's home directory.".
-spec getHomeDir() -> unicode:charlist().
getHomeDir() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetHomeDir),
  wxe_util:rec(?utils_wxGetHomeDir).

-doc """
Deprecated:

Ids generated by it can conflict with the Ids defined by the user code, use `wxID_ANY` to
assign ids which are guaranteed to not conflict with the user-defined ids for the controls
and menu items you create instead of using this function.

Generates an integer identifier unique to this run of the program.
""".
-spec newId() -> integer().
newId() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxNewId),
  wxe_util:rec(?utils_wxNewId).

-doc "Ensures that Ids subsequently generated by `wx_misc:newId/0` do not clash with the given `id`.".
-spec registerId(Id) -> 'ok' when
	Id::integer().
registerId(Id)
 when is_integer(Id) ->
  wxe_util:queue_cmd(Id,?get_env(),?utils_wxRegisterId).

-doc "Returns the current id.".
-spec getCurrentId() -> integer().
getCurrentId() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetCurrentId),
  wxe_util:rec(?utils_wxGetCurrentId).

-doc """
Returns the string containing the description of the current platform in a user-readable
form.

For example, this function may return strings like "Windows 10 (build 10240), 64-bit
edition" or "Linux 4.1.4 i386".
""".
-spec getOsDescription() -> unicode:charlist().
getOsDescription() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxGetOsDescription),
  wxe_util:rec(?utils_wxGetOsDescription).

-doc """
Returns true if the current platform is little endian (instead of big endian).

The check is performed at run-time.
""".
-spec isPlatformLittleEndian() -> boolean().
isPlatformLittleEndian() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxIsPlatformLittleEndian),
  wxe_util:rec(?utils_wxIsPlatformLittleEndian).

-doc """
Returns true if the operating system the program is running under is 64 bit.

The check is performed at run-time and may differ from the value available at
compile-time (at compile-time you can just check if `sizeof(void*) == 8`) since the
program could be running in emulation mode or in a mixed 32/64 bit system (bi-architecture
operating system).

Note: This function is not 100% reliable on some systems given the fact that there isn't
always a standard way to do a reliable check on the OS architecture.
""".
-spec isPlatform64Bit() -> boolean().
isPlatform64Bit() ->
  wxe_util:queue_cmd(?get_env(), ?utils_wxIsPlatform64Bit),
  wxe_util:rec(?utils_wxIsPlatform64Bit).

-doc """
Returns the display size in pixels.

Note: Use of this function is not recommended in the new code as it only works for the
primary display. Use `wxDisplay:getGeometry/1` to retrieve the size of the appropriate display instead.

Either of output pointers can be NULL if the caller is not interested in the
corresponding value.

See: `m:wxDisplay`
""".
-spec displaySize() -> {Width::integer(), Height::integer()}.
displaySize() ->
  wxe_util:queue_cmd(?get_env(), ?gdicmn_wxDisplaySize),
  wxe_util:rec(?gdicmn_wxDisplaySize).

-doc """
Globally sets the cursor; only has an effect on Windows, Mac and GTK+.

You should call this function with wxNullCursor to restore the system cursor.

See:
* `m:wxCursor`

* `wxWindow:setCursor/2`
""".
-spec setCursor(Cursor) -> 'ok' when
	Cursor::wxCursor:wxCursor().
setCursor(#wx_ref{type=CursorT}=Cursor) ->
  ?CLASS(CursorT,wxCursor),
  wxe_util:queue_cmd(Cursor,?get_env(),?gdicmn_wxSetCursor).

