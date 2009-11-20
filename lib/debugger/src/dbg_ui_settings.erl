%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
%%
-module(dbg_ui_settings).

-include_lib("kernel/include/file.hrl").

%% External exports
-export([start/4]).

%% Internal exports
-export([init/6]).

%% OTP-6011 What's denoted gs="Graphics system id" is now in fact
%% the object id of the monitor window.
-record(state, {gs,      % term() Graphics system id
		win,     % term() Settings dialog window data
		monitor, % pid()  Monitor pid
		action   % load | save
	       }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(GS, Pos, Action, SFile)
%%   GS  = Graphics system id
%%   Pos = {X,Y}
%%   Action = load | save
%%   SFile = default | string()
%%--------------------------------------------------------------------
start(GS, Pos, Action, SFile) ->
    Title = case Action of
		load -> "Load Settings Dialog";
		save -> "Save Settings Dialog"
	    end,
    case dbg_ui_winman:is_started(Title) of
	true -> ignore;
	false ->
	    spawn(?MODULE, init, [self(), GS, Pos, Title, Action, SFile])
    end.

%%====================================================================
%% Internal exports
%%====================================================================

init(Monitor, GS, Pos, Title, Action, SFile) ->
    {SDir, SFileName} =
	if
	    %% If settings are saved for the first time, and to
	    %% the default directory HOME/erlang.tools/debugger,
	    %% make sure the directory exists, or create it if
	    %% desired and possible
	    SFile==default -> {default_settings_dir(GS), "NoName.state"};
	    true -> {filename:dirname(SFile), filename:basename(SFile)}
	end,
		    
    Filter = filename:join(SDir, "*.state"),
    Extra = fun(_File) -> true end,
			
    %% Create window
    Win = case Action of
	      load ->
		  dbg_ui_filedialog_win:create_win(GS, Title, Pos, normal,
						   Filter, Extra);
	      save ->
		  dbg_ui_filedialog_win:create_win(GS, Title, Pos, normal,
						   Filter, Extra, SFileName)
	  end,
    Window = dbg_ui_filedialog_win:get_window(Win),
    dbg_ui_winman:insert(Title, Window),

    State = #state{gs=GS, win=Win, monitor=Monitor, action=Action},
    loop(State).


%%====================================================================
%% Main loop and message handling
%%====================================================================

loop(State) ->
    receive

	%% From the GUI
	GuiEvent when is_tuple(GuiEvent), element(1, GuiEvent)==gs ->
	    Cmd = dbg_ui_filedialog_win:handle_event(GuiEvent,
						     State#state.win),
	    State2 = gui_cmd(Cmd, State),
	    loop(State2);

	%% From the dbg_ui_winman process (Debugger window manager)
	{dbg_ui_winman, update_windows_menu, _Data} ->
	    loop(State);
	{dbg_ui_winman, destroy} ->
	    exit(normal)
    end.

gui_cmd(ignore, State) ->
    State;
gui_cmd({stopped, _Dir}, _State) ->	
    exit(normal);
gui_cmd({win, Win}, State) ->
    State#state{win=Win};
gui_cmd({select, File}, State) ->
    State#state.monitor ! {dbg_ui_settings, File, State#state.action},
    exit(normal).


%%====================================================================
%% Internal functions
%%====================================================================

default_settings_dir(GS) ->
    {ok, [[Home]]} = init:get_argument(home),
    DefDir = filename:join([Home, ".erlang_tools", "debugger"]),

    case filelib:is_dir(DefDir) of
	true -> DefDir;
	false ->
	    {ok, CWD} = file:get_cwd(),
	    
	    Msg = ["Default directory", DefDir, "does not exist.",
		   "Click Ok to create it or", 
		   "Cancel to use other directory!"],
	    case tool_utils:confirm(GS, Msg) of
		ok ->
		    ToolsDir = filename:dirname(DefDir),
		    case filelib:is_dir(ToolsDir) of
			true ->
			    case file:make_dir(DefDir) of
				ok -> DefDir;
				_Error -> CWD
			    end;
			false ->
			    case file:make_dir(ToolsDir) of
				ok ->
				    case file:make_dir(DefDir) of
					ok -> DefDir;
					_Error -> CWD
				    end;
				_Error -> CWD
			    end
		    end;
		cancel -> CWD
	    end
    end.
