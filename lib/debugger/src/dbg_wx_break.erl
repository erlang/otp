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
%%

%%
-module(dbg_wx_break).

%% External exports
-export([start/3, start/4, start/5]).

%% Internal exports
-export([init/6]).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(Wx, Pos, Type)
%% start(Wx, Pos, Type, Module, Line)
%%   Wx = Parent Window
%%   Pos = {X, Y}
%%     X = Y = integer()
%%   Type =  line | conditional | function
%%   Module = atom()
%%   Line = integer()
%%--------------------------------------------------------------------
start(Wx, Pos, Type) ->
    start(Wx, Pos, Type, "", "").
start(Wx, Pos, Type, Mod) ->
    start(Wx, Pos, Type, Mod, "").
start(Wx, Pos, Type, Mod, Line) ->
    Env = wx:get_env(),
    spawn_link(?MODULE, init, [Wx, Env, Pos, Type, Mod, Line]).


%%====================================================================
%% Internal exports
%%====================================================================

init(Wx, Env, Pos, Type, Mod, Line) ->
    wx:set_env(Env),
    Win = wx:batch(fun() -> dbg_wx_break_win:create_win(Wx, Pos, Type, Mod, Line) end),
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
	GuiEvent when element(1, GuiEvent)==gs; element(1, GuiEvent)==wx ->
	    Cmd  = wx:batch(fun() -> dbg_wx_break_win:handle_event(GuiEvent, Win) end),
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
    dbg_wx_break_win:update_functions(Win, Funcs);
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
