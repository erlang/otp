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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html">Misc</a>.

%% This module contains wxWidgets utility functions.

-module(wx_misc).
-include("wxe.hrl").
-export([beginBusyCursor/0,beginBusyCursor/1,bell/0,endBusyCursor/0,findMenuItemId/3,
  findWindowAtPoint/1,genericFindWindowAtPoint/1,getCurrentId/0,getEmailAddress/0,
  getHomeDir/0,getKeyState/1,getMousePosition/0,getMouseState/0,getOsDescription/0,
  getUserId/0,isBusy/0,isPlatform64Bit/0,isPlatformLittleEndian/0,launchDefaultBrowser/1,
  launchDefaultBrowser/2,newId/0,registerId/1,setDetectableAutoRepeat/1,
  shell/0,shell/1,shutdown/1]).

%% @spec (Key::WxKeyCode) -> bool()
%% WxKeyCode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgetkeystate">external documentation</a>.
%%<br /> WxKeyCode is one of ?WXK_BACK | ?WXK_TAB | ?WXK_RETURN | ?WXK_ESCAPE | ?WXK_SPACE | ?WXK_DELETE | ?WXK_START | ?WXK_LBUTTON | ?WXK_RBUTTON | ?WXK_CANCEL | ?WXK_MBUTTON | ?WXK_CLEAR | ?WXK_SHIFT | ?WXK_ALT | ?WXK_CONTROL | ?WXK_MENU | ?WXK_PAUSE | ?WXK_CAPITAL | ?WXK_END | ?WXK_HOME | ?WXK_LEFT | ?WXK_UP | ?WXK_RIGHT | ?WXK_DOWN | ?WXK_SELECT | ?WXK_PRINT | ?WXK_EXECUTE | ?WXK_SNAPSHOT | ?WXK_INSERT | ?WXK_HELP | ?WXK_NUMPAD0 | ?WXK_NUMPAD1 | ?WXK_NUMPAD2 | ?WXK_NUMPAD3 | ?WXK_NUMPAD4 | ?WXK_NUMPAD5 | ?WXK_NUMPAD6 | ?WXK_NUMPAD7 | ?WXK_NUMPAD8 | ?WXK_NUMPAD9 | ?WXK_MULTIPLY | ?WXK_ADD | ?WXK_SEPARATOR | ?WXK_SUBTRACT | ?WXK_DECIMAL | ?WXK_DIVIDE | ?WXK_F1 | ?WXK_F2 | ?WXK_F3 | ?WXK_F4 | ?WXK_F5 | ?WXK_F6 | ?WXK_F7 | ?WXK_F8 | ?WXK_F9 | ?WXK_F10 | ?WXK_F11 | ?WXK_F12 | ?WXK_F13 | ?WXK_F14 | ?WXK_F15 | ?WXK_F16 | ?WXK_F17 | ?WXK_F18 | ?WXK_F19 | ?WXK_F20 | ?WXK_F21 | ?WXK_F22 | ?WXK_F23 | ?WXK_F24 | ?WXK_NUMLOCK | ?WXK_SCROLL | ?WXK_PAGEUP | ?WXK_PAGEDOWN | ?WXK_NUMPAD_SPACE | ?WXK_NUMPAD_TAB | ?WXK_NUMPAD_ENTER | ?WXK_NUMPAD_F1 | ?WXK_NUMPAD_F2 | ?WXK_NUMPAD_F3 | ?WXK_NUMPAD_F4 | ?WXK_NUMPAD_HOME | ?WXK_NUMPAD_LEFT | ?WXK_NUMPAD_UP | ?WXK_NUMPAD_RIGHT | ?WXK_NUMPAD_DOWN | ?WXK_NUMPAD_PAGEUP | ?WXK_NUMPAD_PAGEDOWN | ?WXK_NUMPAD_END | ?WXK_NUMPAD_BEGIN | ?WXK_NUMPAD_INSERT | ?WXK_NUMPAD_DELETE | ?WXK_NUMPAD_EQUAL | ?WXK_NUMPAD_MULTIPLY | ?WXK_NUMPAD_ADD | ?WXK_NUMPAD_SEPARATOR | ?WXK_NUMPAD_SUBTRACT | ?WXK_NUMPAD_DECIMAL | ?WXK_NUMPAD_DIVIDE | ?WXK_WINDOWS_LEFT | ?WXK_WINDOWS_RIGHT | ?WXK_WINDOWS_MENU | ?WXK_COMMAND | ?WXK_SPECIAL1 | ?WXK_SPECIAL2 | ?WXK_SPECIAL3 | ?WXK_SPECIAL4 | ?WXK_SPECIAL5 | ?WXK_SPECIAL6 | ?WXK_SPECIAL7 | ?WXK_SPECIAL8 | ?WXK_SPECIAL9 | ?WXK_SPECIAL10 | ?WXK_SPECIAL11 | ?WXK_SPECIAL12 | ?WXK_SPECIAL13 | ?WXK_SPECIAL14 | ?WXK_SPECIAL15 | ?WXK_SPECIAL16 | ?WXK_SPECIAL17 | ?WXK_SPECIAL18 | ?WXK_SPECIAL19 | ?WXK_SPECIAL20
getKeyState(Key)
 when is_integer(Key) ->
  wxe_util:call(?utils_wxGetKeyState,
  <<Key:32/?UI>>).

%% @spec () -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgetmouseposition">external documentation</a>.
getMousePosition() ->
  wxe_util:call(?utils_wxGetMousePosition,
  <<>>).

%% @spec () -> wx:wxMouseState()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgetmousestate">external documentation</a>.
getMouseState() ->
  wxe_util:call(?utils_wxGetMouseState,
  <<>>).

%% @spec (Flag::bool()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxsetdetectableautorepeat">external documentation</a>.
setDetectableAutoRepeat(Flag)
 when is_boolean(Flag) ->
  wxe_util:call(?utils_wxSetDetectableAutoRepeat,
  <<(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec () -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxbell">external documentation</a>.
bell() ->
  wxe_util:cast(?utils_wxBell,
  <<>>).

%% @spec (Frame::wxFrame:wxFrame(), MenuString::string(), ItemString::string()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxfindmenuitemid">external documentation</a>.
findMenuItemId(#wx_ref{type=FrameT,ref=FrameRef},MenuString,ItemString)
 when is_list(MenuString),is_list(ItemString) ->
  ?CLASS(FrameT,wxFrame),
  MenuString_UC = unicode:characters_to_binary([MenuString,0]),
  ItemString_UC = unicode:characters_to_binary([ItemString,0]),
  wxe_util:call(?utils_wxFindMenuItemId,
  <<FrameRef:32/?UI,(byte_size(MenuString_UC)):32/?UI,(MenuString_UC)/binary, 0:(((8- ((0+byte_size(MenuString_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(ItemString_UC)):32/?UI,(ItemString_UC)/binary, 0:(((8- ((4+byte_size(ItemString_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (Pt::{X::integer(),Y::integer()}) -> wxWindow:wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgenericfindwindowatpoint">external documentation</a>.
genericFindWindowAtPoint({PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:call(?utils_wxGenericFindWindowAtPoint,
  <<PtX:32/?UI,PtY:32/?UI>>).

%% @spec (Pt::{X::integer(),Y::integer()}) -> wxWindow:wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxfindwindowatpoint">external documentation</a>.
findWindowAtPoint({PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  wxe_util:call(?utils_wxFindWindowAtPoint,
  <<PtX:32/?UI,PtY:32/?UI>>).

%% @spec () -> ok
%% @equiv beginBusyCursor([])
beginBusyCursor() ->
  beginBusyCursor([]).

%% @spec ([Option]) -> ok
%% Option = {cursor, wxCursor:wxCursor()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxbeginbusycursor">external documentation</a>.
beginBusyCursor(Options)
 when is_list(Options) ->
  MOpts = fun({cursor, #wx_ref{type=CursorT,ref=CursorRef}}, Acc) ->   ?CLASS(CursorT,wxCursor),[<<1:32/?UI,CursorRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?utils_wxBeginBusyCursor,
  <<BinOpt/binary>>).

%% @spec () -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxendbusycursor">external documentation</a>.
endBusyCursor() ->
  wxe_util:cast(?utils_wxEndBusyCursor,
  <<>>).

%% @spec () -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxisbusy">external documentation</a>.
isBusy() ->
  wxe_util:call(?utils_wxIsBusy,
  <<>>).

%% @spec (WFlags::WxShutdownFlags) -> bool()
%% WxShutdownFlags = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxshutdown">external documentation</a>.
%%<br /> WxShutdownFlags is one of ?wxSHUTDOWN_POWEROFF | ?wxSHUTDOWN_REBOOT
shutdown(WFlags)
 when is_integer(WFlags) ->
  wxe_util:call(?utils_wxShutdown,
  <<WFlags:32/?UI>>).

%% @spec () -> bool()
%% @equiv shell([])
shell() ->
  shell([]).

%% @spec ([Option]) -> bool()
%% Option = {command, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxshell">external documentation</a>.
shell(Options)
 when is_list(Options) ->
  MOpts = fun({command, Command}, Acc) ->   Command_UC = unicode:characters_to_binary([Command,0]),[<<1:32/?UI,(byte_size(Command_UC)):32/?UI,(Command_UC)/binary, 0:(((8- ((0+byte_size(Command_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?utils_wxShell,
  <<BinOpt/binary>>).

%% @spec (Url::string()) -> bool()
%% @equiv launchDefaultBrowser(Url, [])
launchDefaultBrowser(Url)
 when is_list(Url) ->
  launchDefaultBrowser(Url, []).

%% @spec (Url::string(), [Option]) -> bool()
%% Option = {flags, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxlaunchdefaultbrowser">external documentation</a>.
launchDefaultBrowser(Url, Options)
 when is_list(Url),is_list(Options) ->
  Url_UC = unicode:characters_to_binary([Url,0]),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?utils_wxLaunchDefaultBrowser,
  <<(byte_size(Url_UC)):32/?UI,(Url_UC)/binary, 0:(((8- ((4+byte_size(Url_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec () -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgetemailaddress">external documentation</a>.
getEmailAddress() ->
  wxe_util:call(?utils_wxGetEmailAddress,
  <<>>).

%% @spec () -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgetuserid">external documentation</a>.
getUserId() ->
  wxe_util:call(?utils_wxGetUserId,
  <<>>).

%% @spec () -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgethomedir">external documentation</a>.
getHomeDir() ->
  wxe_util:call(?utils_wxGetHomeDir,
  <<>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxnewid">external documentation</a>.
newId() ->
  wxe_util:call(?utils_wxNewId,
  <<>>).

%% @spec (Id::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxregisterid">external documentation</a>.
registerId(Id)
 when is_integer(Id) ->
  wxe_util:cast(?utils_wxRegisterId,
  <<Id:32/?UI>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgetcurrentid">external documentation</a>.
getCurrentId() ->
  wxe_util:call(?utils_wxGetCurrentId,
  <<>>).

%% @spec () -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxgetosdescription">external documentation</a>.
getOsDescription() ->
  wxe_util:call(?utils_wxGetOsDescription,
  <<>>).

%% @spec () -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxisplatformlittleendian">external documentation</a>.
isPlatformLittleEndian() ->
  wxe_util:call(?utils_wxIsPlatformLittleEndian,
  <<>>).

%% @spec () -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_miscellany.html#wxisplatform64bit">external documentation</a>.
isPlatform64Bit() ->
  wxe_util:call(?utils_wxIsPlatform64Bit,
  <<>>).

