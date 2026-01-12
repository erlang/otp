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

%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% SPDX-FileCopyrightText: Copyright 2021-2025 Dominic Letz <dominic@diode.io>
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
