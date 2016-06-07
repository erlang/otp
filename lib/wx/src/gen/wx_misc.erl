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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html">Misc</a>.

%% This module contains wxWidgets utility functions.

-module(wx_misc).
-include("wxe.hrl").
-export([beginBusyCursor/0,beginBusyCursor/1,bell/0,endBusyCursor/0,findMenuItemId/3,
  findWindowAtPoint/1,genericFindWindowAtPoint/1,getCurrentId/0,getEmailAddress/0,
  getHomeDir/0,getKeyState/1,getMousePosition/0,getMouseState/0,getOsDescription/0,
  getUserId/0,isBusy/0,isPlatform64Bit/0,isPlatformLittleEndian/0,launchDefaultBrowser/1,
  launchDefaultBrowser/2,newId/0,registerId/1,setDetectableAutoRepeat/1,
  shell/0,shell/1,shutdown/1]).

-export([displaySize/0,setCursor/1]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetkeystate">external documentation</a>.
%%<br /> Key = ?WXK_BACK | ?WXK_TAB | ?WXK_RETURN | ?WXK_ESCAPE | ?WXK_SPACE | ?WXK_DELETE | ?WXK_START | ?WXK_LBUTTON | ?WXK_RBUTTON | ?WXK_CANCEL | ?WXK_MBUTTON | ?WXK_CLEAR | ?WXK_SHIFT | ?WXK_ALT | ?WXK_CONTROL | ?WXK_MENU | ?WXK_PAUSE | ?WXK_CAPITAL | ?WXK_END | ?WXK_HOME | ?WXK_LEFT | ?WXK_UP | ?WXK_RIGHT | ?WXK_DOWN | ?WXK_SELECT | ?WXK_PRINT | ?WXK_EXECUTE | ?WXK_SNAPSHOT | ?WXK_INSERT | ?WXK_HELP | ?WXK_NUMPAD0 | ?WXK_NUMPAD1 | ?WXK_NUMPAD2 | ?WXK_NUMPAD3 | ?WXK_NUMPAD4 | ?WXK_NUMPAD5 | ?WXK_NUMPAD6 | ?WXK_NUMPAD7 | ?WXK_NUMPAD8 | ?WXK_NUMPAD9 | ?WXK_MULTIPLY | ?WXK_ADD | ?WXK_SEPARATOR | ?WXK_SUBTRACT | ?WXK_DECIMAL | ?WXK_DIVIDE | ?WXK_F1 | ?WXK_F2 | ?WXK_F3 | ?WXK_F4 | ?WXK_F5 | ?WXK_F6 | ?WXK_F7 | ?WXK_F8 | ?WXK_F9 | ?WXK_F10 | ?WXK_F11 | ?WXK_F12 | ?WXK_F13 | ?WXK_F14 | ?WXK_F15 | ?WXK_F16 | ?WXK_F17 | ?WXK_F18 | ?WXK_F19 | ?WXK_F20 | ?WXK_F21 | ?WXK_F22 | ?WXK_F23 | ?WXK_F24 | ?WXK_NUMLOCK | ?WXK_SCROLL | ?WXK_PAGEUP | ?WXK_PAGEDOWN | ?WXK_NUMPAD_SPACE | ?WXK_NUMPAD_TAB | ?WXK_NUMPAD_ENTER | ?WXK_NUMPAD_F1 | ?WXK_NUMPAD_F2 | ?WXK_NUMPAD_F3 | ?WXK_NUMPAD_F4 | ?WXK_NUMPAD_HOME | ?WXK_NUMPAD_LEFT | ?WXK_NUMPAD_UP | ?WXK_NUMPAD_RIGHT | ?WXK_NUMPAD_DOWN | ?WXK_NUMPAD_PAGEUP | ?WXK_NUMPAD_PAGEDOWN | ?WXK_NUMPAD_END | ?WXK_NUMPAD_BEGIN | ?WXK_NUMPAD_INSERT | ?WXK_NUMPAD_DELETE | ?WXK_NUMPAD_EQUAL | ?WXK_NUMPAD_MULTIPLY | ?WXK_NUMPAD_ADD | ?WXK_NUMPAD_SEPARATOR | ?WXK_NUMPAD_SUBTRACT | ?WXK_NUMPAD_DECIMAL | ?WXK_NUMPAD_DIVIDE | ?WXK_WINDOWS_LEFT | ?WXK_WINDOWS_RIGHT | ?WXK_WINDOWS_MENU | ?WXK_COMMAND | ?WXK_SPECIAL1 | ?WXK_SPECIAL2 | ?WXK_SPECIAL3 | ?WXK_SPECIAL4 | ?WXK_SPECIAL5 | ?WXK_SPECIAL6 | ?WXK_SPECIAL7 | ?WXK_SPECIAL8 | ?WXK_SPECIAL9 | ?WXK_SPECIAL10 | ?WXK_SPECIAL11 | ?WXK_SPECIAL12 | ?WXK_SPECIAL13 | ?WXK_SPECIAL14 | ?WXK_SPECIAL15 | ?WXK_SPECIAL16 | ?WXK_SPECIAL17 | ?WXK_SPECIAL18 | ?WXK_SPECIAL19 | ?WXK_SPECIAL20
-spec getKeyState(Key) -> boolean() when
	Key::wx:wx_enum().
getKeyState(Key)
 when is_integer(Key) ->
  wxe_util:call(?utils_wxGetKeyState,
  <<Key:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetmouseposition">external documentation</a>.
-spec getMousePosition() -> {X::integer(), Y::integer()}.
getMousePosition() ->
  wxe_util:call(?utils_wxGetMousePosition,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetmousestate">external documentation</a>.
-spec getMouseState() -> wx:wx_wxMouseState().
getMouseState() ->
  wxe_util:call(?utils_wxGetMouseState,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxsetdetectableautorepeat">external documentation</a>.
-spec setDetectableAutoRepeat(Flag) -> boolean() when
	Flag::boolean().
setDetectableAutoRepeat(Flag)
 when is_boolean(Flag) ->
  wxe_util:call(?utils_wxSetDetectableAutoRepeat,
  <<(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxbell">external documentation</a>.
-spec bell() -> 'ok'.
bell() ->
  wxe_util:cast(?utils_wxBell,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxfindmenuitemid">external documentation</a>.
-spec findMenuItemId(Frame, MenuString, ItemString) -> integer() when
	Frame::wxFrame:wxFrame(), MenuString::unicode:chardata(), ItemString::unicode:chardata().
findMenuItemId(#wx_ref{type=FrameT,ref=FrameRef},MenuString,ItemString)
 when is_list(MenuString),is_list(ItemString) ->
  ?CLASS(FrameT,wxFrame),
  MenuString_UC = unicode:characters_to_binary([MenuString,0]),
  ItemString_UC = unicode:characters_to_binary([ItemString,0]),
  wxe_util:call(?utils_wxFindMenuItemId,
  <<FrameRef:32/?UI,(byte_size(MenuString_UC)):32/?UI,(MenuString_UC)/binary, 0:(((8- ((0+byte_size(MenuString_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(ItemString_UC)):32/?UI,(ItemString_UC)/binary, 0:(((8- ((4+byte_size(ItemString_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgenericfindwindowatpoint">external documentation</a>.
-spec genericFindWindowAtPoint(Pt) -> wxWindow:wxWindow() when
	Pt::{X::integer(), Y::integer()}.
genericFindWindowAtPoint({PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:call(?utils_wxGenericFindWindowAtPoint,
  <<PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxfindwindowatpoint">external documentation</a>.
-spec findWindowAtPoint(Pt) -> wxWindow:wxWindow() when
	Pt::{X::integer(), Y::integer()}.
findWindowAtPoint({PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:call(?utils_wxFindWindowAtPoint,
  <<PtX:32/?UI,PtY:32/?UI>>).

%% @equiv beginBusyCursor([])
-spec beginBusyCursor() -> 'ok'.

beginBusyCursor() ->
  beginBusyCursor([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxbeginbusycursor">external documentation</a>.
-spec beginBusyCursor([Option]) -> 'ok' when
	Option :: {'cursor', wxCursor:wxCursor()}.
beginBusyCursor(Options)
 when is_list(Options) ->
  MOpts = fun({cursor, #wx_ref{type=CursorT,ref=CursorRef}}, Acc) ->   ?CLASS(CursorT,wxCursor),[<<1:32/?UI,CursorRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?utils_wxBeginBusyCursor,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxendbusycursor">external documentation</a>.
-spec endBusyCursor() -> 'ok'.
endBusyCursor() ->
  wxe_util:cast(?utils_wxEndBusyCursor,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxisbusy">external documentation</a>.
-spec isBusy() -> boolean().
isBusy() ->
  wxe_util:call(?utils_wxIsBusy,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxshutdown">external documentation</a>.
%%<br /> WFlags = ?wxSHUTDOWN_POWEROFF | ?wxSHUTDOWN_REBOOT
-spec shutdown(WFlags) -> boolean() when
	WFlags::wx:wx_enum().
shutdown(WFlags)
 when is_integer(WFlags) ->
  wxe_util:call(?utils_wxShutdown,
  <<WFlags:32/?UI>>).

%% @equiv shell([])
-spec shell() -> boolean().

shell() ->
  shell([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxshell">external documentation</a>.
-spec shell([Option]) -> boolean() when
	Option :: {'command', unicode:chardata()}.
shell(Options)
 when is_list(Options) ->
  MOpts = fun({command, Command}, Acc) ->   Command_UC = unicode:characters_to_binary([Command,0]),[<<1:32/?UI,(byte_size(Command_UC)):32/?UI,(Command_UC)/binary, 0:(((8- ((0+byte_size(Command_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?utils_wxShell,
  <<BinOpt/binary>>).

%% @equiv launchDefaultBrowser(Url, [])
-spec launchDefaultBrowser(Url) -> boolean() when
	Url::unicode:chardata().

launchDefaultBrowser(Url)
 when is_list(Url) ->
  launchDefaultBrowser(Url, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxlaunchdefaultbrowser">external documentation</a>.
-spec launchDefaultBrowser(Url, [Option]) -> boolean() when
	Url::unicode:chardata(),
	Option :: {'flags', integer()}.
launchDefaultBrowser(Url, Options)
 when is_list(Url),is_list(Options) ->
  Url_UC = unicode:characters_to_binary([Url,0]),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?utils_wxLaunchDefaultBrowser,
  <<(byte_size(Url_UC)):32/?UI,(Url_UC)/binary, 0:(((8- ((4+byte_size(Url_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetemailaddress">external documentation</a>.
-spec getEmailAddress() -> unicode:charlist().
getEmailAddress() ->
  wxe_util:call(?utils_wxGetEmailAddress,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetuserid">external documentation</a>.
-spec getUserId() -> unicode:charlist().
getUserId() ->
  wxe_util:call(?utils_wxGetUserId,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgethomedir">external documentation</a>.
-spec getHomeDir() -> unicode:charlist().
getHomeDir() ->
  wxe_util:call(?utils_wxGetHomeDir,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxnewid">external documentation</a>.
-spec newId() -> integer().
newId() ->
  wxe_util:call(?utils_wxNewId,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxregisterid">external documentation</a>.
-spec registerId(Id) -> 'ok' when
	Id::integer().
registerId(Id)
 when is_integer(Id) ->
  wxe_util:cast(?utils_wxRegisterId,
  <<Id:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetcurrentid">external documentation</a>.
-spec getCurrentId() -> integer().
getCurrentId() ->
  wxe_util:call(?utils_wxGetCurrentId,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxgetosdescription">external documentation</a>.
-spec getOsDescription() -> unicode:charlist().
getOsDescription() ->
  wxe_util:call(?utils_wxGetOsDescription,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxisplatformlittleendian">external documentation</a>.
-spec isPlatformLittleEndian() -> boolean().
isPlatformLittleEndian() ->
  wxe_util:call(?utils_wxIsPlatformLittleEndian,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#wxisplatform64bit">external documentation</a>.
-spec isPlatform64Bit() -> boolean().
isPlatform64Bit() ->
  wxe_util:call(?utils_wxIsPlatform64Bit,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_gdicmn.html#gdicmnwxdisplaysize">external documentation</a>.
-spec displaySize() -> {Width::integer(), Height::integer()}.
displaySize() ->
  wxe_util:call(?gdicmn_wxDisplaySize,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_gdicmn.html#gdicmnwxsetcursor">external documentation</a>.
-spec setCursor(Cursor) -> 'ok' when
	Cursor::wxCursor:wxCursor().
setCursor(#wx_ref{type=CursorT,ref=CursorRef}) ->
  ?CLASS(CursorT,wxCursor),
  wxe_util:cast(?gdicmn_wxSetCursor,
  <<CursorRef:32/?UI>>).

