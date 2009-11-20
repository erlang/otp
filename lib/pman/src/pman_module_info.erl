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
-module(pman_module_info).

%% Window with module information (View->Module Info...)

%% External exports
-export([start/1]).

%% Record for keeping the loop state for the 
%% module info process.
-record(state, {topwin,      % GS identifier for top window
		editor,      % GS identifier for editor
		module,      % Name of the viewed module
		parent}).    % Pid of the parent

start(Module) ->
    Self = self(),
    spawn_link(fun() -> init(Module, Self) end).

init(Module, Parent) ->
    process_flag(trap_exit, true),

    GS = gs:start([{kernel,true}]),
    Font = pman_win:font(GS),

    WinTitle = lists:flatten(io_lib:format("Pman - Module Info: ~p",
					   [Module])),
    WinOptions = [{title,WinTitle}, {width,550}, {height, 400},
		  {configure,true}, {keypress,true}, {destroy,true}],
    TopWindow = gse:window(GS, WinOptions),

    %% File menu
    MenuBar = gse:menubar(TopWindow, []),
    MBFile = gse:menubutton(MenuBar, [{label,{text," File "}},
				      {font,Font}, {underline, 1}]),
    MenuFile = gse:menu(MBFile, []),

    gse:named_menuitem('Save buffer', MenuFile,
		       [{label,{text,"Save buffer..."}},
			{font,Font}, {underline,0}]),
    gse:named_menuitem('Close', MenuFile,
		       [{label,{text,"Close"}},
			{font,Font}, {underline,0}]),

    %% Output part of window
    Editor = gse:editor(TopWindow,
			[{font,Font},
			 {x,3}, {y,40}, {width,546}, {height,348}]),
    gse:config(Editor, [{keypress,true},
			{insert,{'end',pman_win:module_data(Module)}}]),
    gse:config(Editor, [{enable,false},
			{vscroll,right}, {hscroll,bottom},
			{wrap,none}]),
    gse:map(TopWindow),

    State = #state{topwin=TopWindow, editor=Editor, module=Module,
		   parent=Parent},
    loop(State).

loop(State) ->

    receive
	%% Die if the parent dies
	{'EXIT', Pid, _Reason} when Pid==State#state.parent ->
	    gse:destroy(State#state.topwin);

	%% Ignore other exit signals (from file dialog window)
	{'EXIT', _Pid, _Reason} ->
	    loop(State);

	%% Window closed
	{gs, _TopWindow, destroy, [], []} ->
	    ok;

	%% Window resized or moved
        {gs, _TopWindow, configure ,_Data, [W,H,_X,_Y|_]} ->
	    gs:config(State#state.editor, [{width,W-3}, {height,H-40}]),
	    loop(State);

	%% Close - destroy window and exit process
	{gs, 'Close', click, _Data, _Args} ->
	    gse:destroy(State#state.topwin),
	    ok;

	%% Save Buffer - make filename and save buffer to file
	{gs, 'Save buffer', click, _Data, _Args} ->
	    save_buffer(State),
	    loop(State);

	%% Keyboard accelerator commands
	{gs, _, keypress, [], [c,_,0,1]} -> % 'Close'
	    gse:destroy(State#state.topwin),
	    ok;
	{gs, _, keypress, [], [s,_,0,1]} -> % 'Save buffer'
	    save_buffer(State),
	    loop(State);
	{gs, _, keypress, _Data, _Args} ->
	    loop(State)
    end.

save_buffer(State) ->
    DefaultFile = atom_to_list(State#state.module) ++ ".module_info",
    Result = tool_utils:file_dialog([{type,save}, {file,DefaultFile}]),
    case Result of
	%% User selected a file, now save the result
	{ok, File, _Dir} ->	    
	    gs:config(State#state.editor, {save,File}),
	    Msg = "Module information saved in file\n" ++ File,
	    tool_utils:notify(State#state.topwin, Msg);

	%% File dialog was cancelled in some way.
	{error, _Reason} ->
	    ignore
    end.
