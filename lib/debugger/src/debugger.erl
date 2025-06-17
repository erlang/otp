%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
-moduledoc """
The Erlang Debugger for debugging and testing of Erlang programs.
""".

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

-doc """
Starts Debugger.

Started by this function, Debugger interprets code on all known nodes.
""".
-spec start() -> term().
start() ->
    start(global, default, default).


-doc """
start(ModeOrFile)

Starts Debugger.

If `ModeOrFile` is a string, it is assumed to be the name of a file,
and Debugger tries to load its settings from this file. For details
about settings, see the [User's Guide](debugger_chapter.md).

If `ModeOrFile` is atom `local`, Debugger interprets code only at the
current node. If `ModeOrFile` is `global`, Debugger interprets code on
all known nodes.
""".
-spec start(Mode) -> term() when Mode :: local | global | wx;
           (File) -> term() when File :: string().
start(Mode) when Mode==local; Mode==global ->
    start(Mode, default, default);
start(Gui) when Gui==wx ->
    start(global, default, Gui);
start(SFile) when is_list(SFile), is_integer(hd(SFile)) ->
    start(global, SFile, default).

-doc """
Starts Debugger.

Debugger tries to load its settings from the file named by `File`.
For details about settings, see the [User's Guide](debugger_chapter.md).

If `Mode` is `local`, Debugger interprets code only on the current
node. If `Mode` is `global`, Debugger interprets code on all known
nodes.
""".
-spec start(Mode, File) -> term() when Mode :: local | global,
   File :: string().
start(Mode, SFile) ->
    start(Mode, SFile, default).

start(Mode, SFile, wx) ->
    dbg_wx_mon:start(Mode, SFile);
start(Mode, SFile, default) ->
    Gui = which_gui(),
    start(Mode, SFile, Gui).

-doc false.
stop() ->
    dbg_wx_mon:stop().

-doc """
Debugs a single process.

The module `Module` is interpreted and
[`apply(Module, Name, Args)`](`apply/3`) is called. This opens an "Attach
Process" window. For details, see the
[User's Guide](debugger_chapter.md).
""".
-spec quick(Module, Name, Args) -> term() when Module :: atom(),
   Name :: atom(),
   Args :: [term()].
quick(M, F, A) ->
    _ = int:i(M),
    auto_attach([init]),
    apply(M, F, A).

-doc false.
auto_attach(Flags) ->    
    case which_gui() of
	wx -> int:auto_attach(Flags, {dbg_wx_trace, start, []})
    end.

which_gui() -> wx.

