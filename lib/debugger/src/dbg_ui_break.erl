%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(dbg_ui_break).

%% External exports
-export([start/3, start/4, start/5]).

%% Internal exports
-export([init/5]).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(GS, Pos, Type)
%% start(GS, Pos, Type, Module, Line)
%%   GS = graphics system identifier
%%   Pos = {X, Y}
%%     X = Y = integer()
%%   Type =  line | conditional | function
%%   Module = atom()
%%   Line = integer()
%%--------------------------------------------------------------------
start(GS, Pos, Type) ->
    start(GS, Pos, Type, "", "").
start(GS, Pos, Type, Mod) ->
    start(GS, Pos, Type, Mod, "").
start(GS, Pos, Type, Mod, Line) ->
    spawn_link(?MODULE, init, [GS, Pos, Type, Mod, Line]).


%%====================================================================
%% Internal exports
%%====================================================================

init(GS, Pos, Type, Mod, Line) ->
    Win = dbg_ui_break_win:create_win(GS, Pos, Type, Mod, Line),
    if
	Type==function, is_atom(Mod) ->
	    Win2 = gui_cmd({module, Mod}, Win),
	    loop(Win2);
	true ->
	    loop(Win)
    end.

loop(Win) ->
    receive

	%% From the GUI
	GuiEvent when is_tuple(GuiEvent), element(1, GuiEvent)==gs ->
	    Cmd = dbg_ui_break_win:handle_event(GuiEvent, Win),
	    Win2 = gui_cmd(Cmd, Win),
	    loop(Win2)
    end.

gui_cmd(ignore, Win) ->
    Win;
gui_cmd(stopped, _Win) ->
    exit(normal);
gui_cmd({win, Win2}, _Win) ->
    Win2;
gui_cmd({module, Mod}, Win) ->
    Funcs = int:functions(Mod),
    dbg_ui_break_win:update_functions(Win, Funcs);
gui_cmd({break, DataL, Action}, _Win) ->
    Fun =
	fun(Data) ->
		case Data of
		    [Mod, Line] ->
			int:break(Mod, Line),
			int:action_at_break(Mod, Line, Action);
		    [Mod, Line, CMod, CFunc] ->
			int:break(Mod, Line),
			int:test_at_break(Mod, Line, {CMod, CFunc}),
			int:action_at_break(Mod, Line, Action);
		    [Mod, Func, Arity] ->
			int:break_in(Mod, Func, Arity)
		end
	end,
    lists:foreach(Fun, DataL),
    exit(normal).
