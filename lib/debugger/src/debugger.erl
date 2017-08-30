%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%
-module(debugger).

%% External exports
-export([start/0, start/1, start/2, stop/0, quick/3, auto_attach/1]).

%%==Erlang Debugger===================================================
%%
%% Graphical user interface to the Erlang Interpreter.
%% The code for each process is divided into two modules, Name.erl
%% and Name_win.erl, where Name.erl contains the logic and
%% Name_win.erl the GS specific functionality.
%%
%% debugger
%% --------
%% Interface module.
%%
%% dbg_wx_winman
%% -------------
%% Window manager, keeping track of open windows and Debugger
%% processes.
%%
%% dbg_wx_mon, dbg_wx_mon_win
%% --------------------------
%% Monitor window, main window of Debugger, displaying information
%% about interpreted modules and debugged processes.
%%
%% dbg_wx_trace, dbg_wx_trace_win
%% ------------------------------
%% Attach process window, showing the code executed by a debugged
%% process and providing a GUI for stepping, inspecting variables etc.
%%
%% dbg_wx_break, dbg_wx_break_win
%% ------------------------------
%% Help window for creating new breakpoints.
%%
%% dbg_wx_interpret, dbg_wx_filedialog_win
%% --------------------------------------
%% Help window for selecting modules to interpret.
%%
%% dbg_wx_settings, dbg_wx_filedialog_win
%% --------------------------------------
%% Help window for saving and loading Debugger settings.
%%
%% dbg_wx_view
%% -----------
%% Help window for viewing interpreted modules (uses dbg_wx_trace_win).
%%
%% dbg_wx_win
%% ----------
%% GUI specific functionality used by more than one window type.
%%
%%====================================================================
start() ->
    start(global, default, default).
start(Mode) when Mode==local; Mode==global ->
    start(Mode, default, default);
start(Gui) when Gui==wx ->
    start(global, default, Gui);
start(SFile) when is_list(SFile), is_integer(hd(SFile)) ->
    start(global, SFile, default).

start(Mode, SFile) ->
    start(Mode, SFile, default).

start(Mode, SFile, wx) ->
    dbg_wx_mon:start(Mode, SFile);
start(Mode, SFile, default) ->
    Gui = which_gui(),
    start(Mode, SFile, Gui).

stop() ->
    dbg_wx_mon:stop().

quick(M, F, A) ->
    int:i(M),
    auto_attach([init]),
    apply(M, F, A).

auto_attach(Flags) ->    
    case which_gui() of
	wx -> int:auto_attach(Flags, {dbg_wx_trace, start, []})
    end.

which_gui() -> wx.
