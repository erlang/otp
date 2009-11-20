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
-module(dbg_ui_edit).

%% External exports
-export([start/5]).

%% Internal exports
-export([init/6]).

-record(state, {win,    % term() Edit dialog window data
		pid,    % pid() Parent
		prompt  % atom()
	       }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(GS, Pos, Title, Prompt, {Type, Value})
%%   GS = graphics system identifier
%%   Pos = {X, Y}
%%     X = Y = integer()
%%   Title = string()
%%   Prompt = atom()
%%   Type = term | atom | float | integer | string
%%   Value = term()
%%--------------------------------------------------------------------
start(GS, Pos, Title, Prompt, Edit) ->
    case dbg_ui_winman:is_started(Title) of
	true -> ignore;
	false ->
	    spawn(?MODULE, init, [self(), GS, Pos, Title, Prompt, Edit])
    end.


%%====================================================================
%% Internal exports
%%====================================================================

init(Pid, GS, Pos, Title, Prompt, Edit) ->

    %% Create edit dialog window
    Win = dbg_ui_edit_win:create_win(GS, Pos, Title, Prompt, Edit),
    Window = dbg_ui_edit_win:get_window(Win),
    dbg_ui_winman:insert(Title, Window),
    State = #state{win=Win, pid=Pid, prompt=Prompt},

    loop(State).

loop(State) ->
    receive

	%% From the GUI
	GuiEvent when is_tuple(GuiEvent), element(1, GuiEvent)==gs ->
	    Cmd = dbg_ui_edit_win:handle_event(GuiEvent,
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
gui_cmd(stopped, _State) ->
    exit(normal);
gui_cmd({edit, Value}, State) ->
    State#state.pid ! {dbg_ui_edit, State#state.prompt, Value},
    exit(normal).
