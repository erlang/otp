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
-module(pman_main).

%% Main process and window

-export([init/2]).

-record(state, {win,		     % GS top window
		frame,	             % GS top frame
		grid,		     % GS process info grid

		size,                % int() No. of displayed procs
		w,                   % int() Window width
		h,                   % int() Window height

		hide_system=false,   % bool() Auto-hide system procs
		hide_new=false,      % bool() Auto-hide new processes

		hide_modules,        % ordset() Excluded modules

		hide_all=[],         % [{node(), bool()}] Hide all
		hide_pids=[],        % [{node(), Ordset}] Processes
				     %  explicitly to hide, per node
		show_pids=[],        % [{node(), Ordset}] Processes
				     %  explicitly to show, per node

		shown_pids=[],       % [{node(), Ordset}] Processes
		                     %  actually shown, per node

		node,		     % node() Current node
		nodes=[],            % [node()] All known nodes

		focus=1,	     % int() Grid line with focus
		focus_pid=undefined, % pid() | undefined Proc in focus

		noshell,	     % bool() Noshell mode on

		options}).           % term() Trace options settings


-include("pman_win.hrl").

-define(REFRESH_TIME,5000).

-define(REQUIRES_FOCUS,		     % List of menus that should
	['Trace Process',	     % be disabled if no process
	 'Kill',		     % is in focus
	 'Hide Selected Process',
	 'Module']).

%%--Process init and loop-----------------------------------------------

init(PidCaller, OSModuleExcluded) ->
    process_flag(trap_exit, true),

    %% Monitor all nodes in a distributed system
    case is_alive() of

	%% We have a distributed system
	true -> net_kernel:monitor_nodes(true);

	%% No distribution
	false -> ignore
    end,
    Nodes = [node()|nodes()],

    %% Create the main window
    %% For some extremely strange reason, the frame must be resized
    %% or the grid won't be visible...
    GridSize = length(processes()) + 61,
    {Window, Grid, Frame, Visible, W, H}  =
	pman_win:pman_window(GridSize, OSModuleExcluded, Nodes),
    gse:resize(Frame, ?WIN_WIDTH, ?WIN_HEIGHT-?MENU_HEIGHT),

    Noshell = case pman_shell:find_shell() of
		  noshell -> true;
		  _ -> false
	      end,

    State1 = #state{win=Window, frame=Frame, grid=Grid,
		    size=Visible,
		    w=W, h=H,
		    hide_modules=OSModuleExcluded,
		    node=node(),
		    noshell=Noshell},

    State2 = lists:foldl(fun(Node, State) -> add_node(Node, State) end,
			 State1,
			 Nodes),

    State3 = refresh(State2),

    %% Notify caller that the process appears
    %% to have been started.
    PidCaller ! {initialization_complete, self()},

    %% Initiate a 'catch all' trace pattern so call tracing works
    erlang:trace_pattern({'_', '_', '_'}, true, [local]),

    %% Read default options file
    Options = restore_options(State3),

    loop(State3#state{options=Options}).

add_node(Node, State) ->
    pman_win:add_menu(node, [Node], "Show"),
    State#state{hide_all=nl_update(Node, false, State#state.hide_all),
		hide_pids=nl_update(Node, [], State#state.hide_pids),
		show_pids=nl_update(Node, [], State#state.show_pids),
		shown_pids=nl_update(Node, [], State#state.shown_pids),
		nodes=[Node|State#state.nodes]}.

%% Restore saved options from default file
restore_options(State)->
    File = options_file(),
    case pman_options:read_from_file(File) of
	{ok, Options} ->
	    Options;
	{error, ReasonStr, DefOptions} ->
	    Parent = State#state.win,
	    Msg = io_lib:format(
		    "Problems reading default option file~n~s:~n~s",
		    [File, ReasonStr]),
	    tool_utils:notify(Parent, Msg),
	    DefOptions
    end.

options_file() ->
    {ok, [[Home]]} = init:get_argument(home),
    filename:join([Home, ".erlang_tools", "pman.opts"]).

loop(State) ->
    receive
	{nodeup, Node} ->
	    case nl_exists(Node, State#state.hide_all) of
		true ->
		    pman_win:add_menu(node, [Node], "Show"),
		    loop(State#state{nodes=[Node|State#state.nodes]});
		false ->
		    loop(add_node(Node, State))
	    end;

	{nodedown, Node} ->
	    pman_win:remove_menu([Node]),

	    Msg = io_lib:format("Node~n~p~ndown.", [Node]),
	    spawn_link(tool_utils, notify, [State#state.win, Msg]),

	    %% We remove Node from the list of nodes but not from
	    %% the other lists of State, in case Node reappears later
	    Nodes = lists:delete(Node, State#state.nodes),
	    State2 = State#state{nodes=Nodes},

	    %% If it was the shown node that went down,
	    %% change overview to this node
	    if
		Node==State#state.node ->
		    State3 = execute_cmd({node,node()}, State2, [], []),
		    loop(State3);
		true ->
		    loop(State2)
	    end;

	%% Ignore EXIT signals from help processes
	{'EXIT', _Pid, _Reason} ->
	    loop(State);

	%% GS events
	{gs, _Obj, _Event, _Data, _Args} = Cmd ->
	    case gs_cmd(Cmd, State) of
		stop ->
		    exit(topquit);
		State2 ->
		    loop(State2)
	    end

    after ?REFRESH_TIME ->
	    State2 = refresh(State),
	    loop(State2)
    end.

%% gs_cmd(Event, State) -> stop | State'
gs_cmd(Event, State) ->
    case Event of

	%% --- Window manager commands ---

	%% Window is moved or resized
        {gs, _, configure, _Data, Args} ->
	    configure(Args, State);

	%% Window closed, stop Pman
        {gs, _, destroy, _, _} ->
	    stop;

	%% --- Dynamic commands ---

	%% Click in any object where the GS Data field is a 2-tuple
	{gs, _, click, Data, Args} when is_tuple(Data), size(Data)==2 ->
	    execute_cmd(Data, State, [], Args);

	%% Single click in the grid sets focus to selected process
	{gs, _, click, {pidfunc,_,_}, [_,Row|_]} when is_integer(Row) ->
	    focus(Row, State);

	%% Double click in the grid starts tracing of selected process
	{gs, _, doubleclick, {pidfunc,_,_}, [_Col,Row| _]} when is_integer(Row) ->
	    execute_cmd('Trace Process', State, [], []);

	%% Click in named GS objects
        {gs, Cmd, click, Data, Args} when is_atom(Cmd);
					  is_atom(element(1, Cmd)) ->
	    execute_cmd(Cmd, State, Data, Args);

        %% --- Keyboard accelerator commands ---

	%% Move focus up and down
	{gs, _, keypress, [], ['Up',_,0,0]} ->
	    execute_cmd(focus_previous, State, [], []);
	{gs, _, keypress, [], ['Down',_,0,0]} ->
	    execute_cmd(focus_next, State, [], []);

	%% Other keyboard shortcuts
	{gs, _, keypress, [], ['Return',_,0,0]} ->
	    execute_cmd('Trace Process', State, [], []);
        {gs, _, keypress, [], [Key,_,0,1]} ->
	    execute_cmd(shortcut(Key), State, [], []);

	%% Ignore all other GS events
     	_Other ->
	    State
    end.

%% Keyboard shortcuts

%% File menu
shortcut(o) -> 'Default Options';
shortcut(e) -> 'Exit';
shortcut(z) -> 'Exit';

%% View menu
shortcut(i) -> 'Hide All';
shortcut(u) -> 'Hide Modules';
shortcut(d) -> 'Hide Selected Process';
shortcut(m) -> 'Module';
shortcut(r) -> 'Refresh';

%% Trace menu
shortcut(k) -> 'Kill';
shortcut(t) -> 'Trace Process';
shortcut(s) -> 'Trace Shell';

%% Help menu
shortcut(h) -> 'Help';

%% Keyboard command only
shortcut(l) -> 'All Links';

%% Process grid traversal
shortcut(p) -> focus_previous;
shortcut(n) -> focus_next;
shortcut(_) -> dummy.

%% configure([W,H,X,Y|_], State) -> State'
%% Window has been moved or resized
configure([W,H|_], State) ->
    if
	W==State#state.w, H==State#state.h ->
	    ignore;

	true ->
	    gse:resize(State#state.frame, W, H-?MENU_HEIGHT),

	    Grid = State#state.grid,
	    case abs(W - gs:read(Grid,width) - 6) of
		0 ->
		    ok;   %% Avoid refreshing width if possible
		_Anything ->
		    Cols = pman_win:calc_columnwidths(W-6),
		    gs:config(Grid, Cols)
	    end,
	    pman_win:configwin(Grid, W, H)
    end,
    State.

%% focus(Row, State) -> State'
%%   Row = int()  Grid row
%% User has selected a row in the grid.
%% Row==1 means header row.
focus(Row, State) ->

    Pid = case get_pid_in_focus(Row, State#state.grid) of
	      {true, {pidfunc,Pid0,_}} ->
		  pman_win:change_colour(State#state.grid,
					 State#state.focus, Row),
		  enable_pid_actions(),
		  Pid0;
	      false ->
		  disable_pid_actions(),
		  undefined
	  end,

    State#state{focus=Row, focus_pid=Pid}.

%% get_pid_in_focus(Row, Grid) -> {true, Data} | false
%%   Data = {pidfunc, Pid, Func}
%%     Func = {Mod,Name,Arity} | term()
%% Return the data associated with the process in focus if there is one,
get_pid_in_focus(1, _Grid) ->
    false;
get_pid_in_focus(Row, Grid) ->
    case gs:read(Grid, {obj_at_row,Row}) of
	undefined -> false;
	GridLine ->
	    Data = gs:read(GridLine, data),
	    {true, Data}
    end.

%% execute_cmd(Cmd, State, Data, Args) -> stop | State'

%% Checkbutton "Hide System Processes"
execute_cmd('Hide System', State, _Data, Args) ->
    [_Text, _Group, Bool|_Rest] = Args,
    State2 = State#state{hide_system=Bool},
    refresh(State2);

%% Checkbutton "Auto-Hide New"
execute_cmd('Auto Hide New', State, _Data, Args ) ->
    [_Text, _Group, Bool|_Rest] = Args,
    refresh(State#state{hide_new=Bool});

%% File->Options...
execute_cmd('Default Options', State, _Data, _Args) ->
    OldOptions = State#state.options,
    NewOptions = pman_options:dialog(State#state.win,
				     "Default Trace Options",
				     OldOptions),
    case NewOptions of
	{error, _Reason} ->
	    State;
	Options ->
	    State#state{options=Options}
    end;

%% File->Save Options
%% Save the set default options to the user's option file
execute_cmd('Save Options', State, _Data, _Args)->
    Options = State#state.options,
    File = options_file(),
    Parent = State#state.win,

    case pman_options:save_to_file(Options, File) of
	ok ->
	    tool_utils:notify(Parent, "Options saved to\n" ++ File);
	{error, ReasonStr} ->
	    Msg = io_lib:format("Could not save options to~n~s:~n~s",
				[File, ReasonStr]),
	    tool_utils:notify(Parent, Msg)
    end,
    State;

%% File->Exit
%% Exit the application
execute_cmd('Exit', _State, _Data, _Args) ->
    stop;

%% View->Hide All Processes
execute_cmd('Hide All', State, _Data, _Args) ->
    Node = State#state.node,
    HideAll = nl_update(Node, true, State#state.hide_all),
    ShowPids = nl_del_all(State#state.node, State#state.show_pids),
    State2 = State#state{hide_all=HideAll, show_pids=ShowPids},
    refresh(State2, true);

%% View->Hide modules...
%% Opens a dialog where the user can select from a list of 
%% the loaded modules.
%% The selected module is added to the list of hidden modules.
execute_cmd('Hide Modules', State, _Data, _Args) ->

    %% Get all loaded modules that are not already hidden
    AllModules = lists:map(fun({Module, _File}) -> Module end,
			   code:all_loaded()),
    ModulesSet = ordsets:subtract(ordsets:from_list(AllModules),
				  State#state.hide_modules),

    %% Let the user select which of the loaded modules to exclude from
    %% the process overview
    Title = "Module selection",
    case pman_tool:select(State#state.win, Title, ModulesSet) of
	Modules when is_list(Modules) ->
	    HideModules = ordsets:union(State#state.hide_modules,
					ordsets:from_list(Modules)),
	    refresh(State#state{hide_modules=HideModules});
	cancelled -> State
    end;

%% View->Hide Selected Process
%% The process in focus should explicitly be hidden
execute_cmd('Hide Selected Process', State, _Data, _Args) ->
    case State#state.focus_pid of
	undefined -> State;
	Pid ->
	    Node = State#state.node,
	    HidePids = nl_add(Node, Pid, State#state.hide_pids),
	    ShowPids = nl_del(Node, Pid, State#state.show_pids),
	    refresh(State#state{hide_pids=HidePids, show_pids=ShowPids})
    end;

%% View->Module Info...
%% Open window with module information.
execute_cmd('Module', State, _Data, _Args) ->
    case get_pid_in_focus(State#state.focus, State#state.grid) of
	{true, {pidfunc, _Pid, {Module,_Name,_Arity}}} ->
	    pman_module_info:start(Module);
	_ -> % false | {true, {pidfunc, Pid, Other}}
	    ignore
    end,
    State;

%% View->Refresh
%% Refresh the main window. 
%% (Called automatically every ?REFRESH_TIME millisecond)
execute_cmd('Refresh', State, _Data, _Args) ->
    refresh(State);

%% View->Show All Processes
%% Makes all processes visible except system processes and new
%% processes, if those buttons are checked.
%% Note: Also un-hides all hidden modules!
execute_cmd('Show All', State, _Data, _Args) ->
    Node = State#state.node,
    HideAll = nl_update(Node, false, State#state.hide_all),
    HidePids = nl_del_all(State#state.node, State#state.hide_pids),
    ShowPids = nl_del_all(State#state.node, State#state.show_pids),
    State2 = State#state{hide_modules=ordsets:new(), hide_all=HideAll,
			 hide_pids=HidePids, show_pids=ShowPids},
    refresh(State2, true);

%% View->Show Processes...
%% Open a list of all hidden processes, if the user selects one this
%% process should explicitly be shown
execute_cmd('Show Selected', State, _Data, _Args) ->
    Node = State#state.node,

    All = pman_process:r_processes(Node),
    Hidden = case nl_lookup(Node, State#state.hide_all) of
		 true ->
		     All;
		 false ->
		     Shown = nl_lookup(Node, State#state.shown_pids),
		     ordsets:subtract(All, Shown)
	     end,

    %% Selection window
    Title = "Select Processes to Show",
    Tuples =
	lists:map(fun(Pid) ->
			  {M,F,A} = pman_process:function_info(Pid),
			  Str = case pman_process:get_name(Pid) of
				    " " ->
					io_lib:format("~p:~p/~p",
						      [M, F, A]);
				    Name ->
					io_lib:format("[~p] ~p:~p/~p",
						      [Name, M, F, A])
				end,
			  {Pid, Str}
		  end,
		  Hidden),
    case pman_tool:select(State#state.win, Title, Tuples) of
	Pids when is_list(Pids) ->
	    HidePids = nl_del(Node, Pids, State#state.hide_pids),
	    ShowPids = nl_add(Node, Pids, State#state.show_pids),
	    refresh(State#state{hide_pids=HidePids,show_pids=ShowPids});
	cancelled -> State
    end;

%% Trace->Kill
execute_cmd('Kill', State, _Data, _Args) ->
    case State#state.focus_pid of
	Pid when is_pid(Pid) ->
	    exit(Pid, kill);
	undefined ->
	    ignore
    end,
    State;

%% Trace->Selected Process
execute_cmd('Trace Process', State, _Data, _Args) ->
    case State#state.focus_pid of
	Pid when is_pid(Pid) ->
	    pman_shell:start({Pid,self()}, State#state.options);
	undefined ->
	    ignore
    end,
    State;

%% Trace->Shell Process
execute_cmd('Trace Shell', State, _Data, _Args) ->
    case pman_shell:find_shell() of
	noshell ->
	    State;
	Shell -> 
	    pman_shell:start({{shell,Shell},self()},
			     State#state.options),
	    State#state{noshell=false}
    end;

%% Nodes->Show <Node>
%% Change shown node
execute_cmd({node,Node}, State, _Data, _Args) ->
    gse:config(State#state.win,
	       [{title,lists:concat(["Pman: Overview on ", Node])}]),
    gse:disable(Node),
    catch gse:enable(State#state.node), % Menu may not exist any more
    refresh(State#state{node=Node}, true);

%% Help->Help
execute_cmd('Help', State, _Data, _Args)  ->
    Win = State#state.win,
    HelpFile =
	filename:join([code:lib_dir(pman),"doc","html","index.html"]),
    tool_utils:open_help(Win, HelpFile),
    State;

%% Keyboard shortcut Ctrl-l
execute_cmd('All Links', State, _Data, _Args) ->
    case State#state.focus_pid of
	Pid when is_pid(Pid) ->
	    case process_info(Pid, links) of
		{links, Pids} ->
		    pman_shell:start_list(Pids, self(),
					  State#state.options);
		undefined ->
		    ignore
	    end;
	undefined -> ignore
    end,
    State;

%% Keyboard shortcuts for process grid traversal
execute_cmd(focus_previous, State, _Data, _Args) ->
    focus(previous_row(State), State);
execute_cmd(focus_next, State, _Data, _Args) ->
    focus(next_row(State), State);

%% Keyboard combinations that are not shortcuts
execute_cmd(dummy, State, _Data, _Args) ->
    State.

%% Convenience functions for disabling/enabling menu items that require
%% that a process is selected.
disable_pid_actions() ->
    lists:foreach(fun(X) -> gse:disable(X) end, ?REQUIRES_FOCUS).

enable_pid_actions()  ->
    lists:foreach(fun(X) -> gse:enable(X) end, ?REQUIRES_FOCUS).

%% refresh(State) -> State'
%% refresh(State, ForceP) -> State'
%% Refreshes the main window.
refresh(State) ->
    refresh(State, false).
refresh(#state{node=Node} = State, ForceP) ->

    %% Update shown processes

    %% First, get an ordset of all processes running at the current node
    All = pman_process:r_processes(Node),

    Shown = nl_lookup(Node, State#state.shown_pids),
    ExpShown = nl_lookup(Node, State#state.show_pids),

    {Show, State2} =
	case nl_lookup(Node, State#state.hide_all) of

	    %% If the user has selected "Hide All Processes", only
	    %% explicitly selected processes which still exist should
	    %% be shown
	    true ->
		{ordsets:intersection(ExpShown, All), State};

	    false ->
		%% Compute which processes should be hidden according
		%% to the flags/menu items selected
		Hidden = hidden_pids(All, State),

		NotHidden = ordsets:subtract(All, Hidden),

		Show0 = case State#state.hide_new of
			    %% If the user has selected "Auto-Hide New",
			    %% then only those processes in NotHidden
			    %% which are already shown, should be shown,
			    %% together with explicitly selected
			    %% processes which still exist
			    true ->
				ordsets:union(
				  ordsets:intersection(NotHidden,Shown),
				  ordsets:intersection(ExpShown, All));

			    %% Otherwise, show all processes in
			    %% NotHidden, together with explicitly
			    %% selected processes which still exist
			    false ->
				ordsets:union(
				  NotHidden,
				  ordsets:intersection(ExpShown, All))
			end,

		ShownPids = nl_update(Node, Show0,
				      State#state.shown_pids),
		{Show0, State#state{shown_pids=ShownPids}}
	end,

    NoOfHidden = length(All) - length(Show),

    if
	Show==Shown, not ForceP ->
	    pman_win:update(NoOfHidden),
	    State;

	true ->
	    ShowInfo = display_info(Show),
	    pman_win:update(State#state.grid, ShowInfo, NoOfHidden),
	    
	    %% Set the focus appropriately
	    State3 = case State2#state.focus_pid of
			 undefined ->
			     disable_pid_actions(),
			     State2;
			 Pid ->
			     Row = get_row(Pid, Show),
			     focus(Row, State2)
		     end,

	    trace_shell_possible(State3),

	    Size = length(Show),
	    case Size of
		1 -> gse:disable('Hide All');
		_ -> gse:enable('Hide All')
	    end,

	    State3#state{size=Size}
    end.

%% hidden_pids(All, State) -> Hidden
hidden_pids(All, State) ->

    %% Processes hidden because they are system processes
    HideSys = case State#state.hide_system of
		  true ->
		      lists:filter(
			fun(Pid) ->
				pman_process:is_system_process(Pid)
			end,
			All);
		  false ->
		      []
	      end,
	    
    %% Process hidden because they are executing code in a hidden module
    Mods = State#state.hide_modules,
    HideMod =
	lists:filter(fun(Pid) ->
			     pman_process:is_hidden_by_module(Pid, Mods)
		     end,
		     All),

    %% Explicitly hidden processes
    HideExp = nl_lookup(State#state.node, State#state.hide_pids),

    %% All hidden processes
    ordsets:union([HideSys, HideMod, HideExp]).

display_info(Pids) ->
    lists:map(fun(Pid) ->
		      Func = pman_process:function_info(Pid),
		      Name = pman_process:get_name(Pid),
		      Msgs = pman_process:msg(Pid),
		      Reds = pman_process:reds(Pid),
		      Size = pman_process:psize(Pid),
		      {Pid, Func, Name, Msgs, Reds, Size}
	      end,
	      Pids).

get_row(Pid, List) ->
    get_row(Pid, List, length(List)+1).

get_row(Pid, [Pid | _], Row) ->
    Row;
get_row(Pid, [_ | T], Row) ->
    get_row(Pid, T, Row-1);
get_row(_Pid, [], _Row) ->
    1.

next_row(#state{size=Size, focus=Row}) ->
    check_row(Row+1, Size).

previous_row(#state{size=Size, focus=Row}) ->
    check_row(Row-1, Size).

check_row(1, Size) ->
    Size+1;
check_row(Row, Size) when Row==Size+2 ->
    2;
check_row(Row, _Size) ->
    Row.

%% Check if node is running in noshell mode and if so disable the
%% 'Trace Shell' menu option.
trace_shell_possible(#state{noshell=true}) ->
    gse:disable('Trace Shell');
trace_shell_possible(_) ->
    ok.

%% -- Functions for manipulating {Node, Data} lists --

%% nl_add(Node, Elem|Elems, NList) -> NList'
nl_add(Node, Elems, [{Node, Ordset} | T]) when is_list(Elems) ->
    [{Node, ordsets:union(Elems, Ordset)} | T];
nl_add(Node, Elem, [{Node, Ordset} | T]) ->
    [{Node, ordsets:add_element(Elem, Ordset)} | T];
nl_add(Node, Elem, [H | T]) ->
    [H | nl_add(Node, Elem, T)];
nl_add(Node, Elems, []) when is_list(Elems) ->
    [{Node, Elems}];
nl_add(Node, Elem, []) ->
    [{Node, ordsets:add_element(Elem, ordsets:new())}].

%% nl_del(Node, Elem|Elems, NList) -> NList'
nl_del(Node, Elems, [{Node, Ordset} | T]) when is_list(Elems) ->
    [{Node, ordsets:subtract(Ordset, Elems)} | T];
nl_del(Node, Elem, [{Node, Ordset} | T]) ->
    [{Node, ordsets:del_element(Elem, Ordset)} | T];
nl_del(Node, Elem, [H | T]) ->
    [H | nl_del(Node, Elem, T)];
nl_del(_Node, _Elem, []) ->
    [].

%% nl_del_all(Node, NList) -> NList'
nl_del_all(Node, [{Node, _Ordset} | T]) ->
    [{Node, ordsets:new()} | T];
nl_del_all(Node, [H | T]) ->
    [H | nl_del_all(Node, T)];
nl_del_all(_Node, []) ->
    [].

%% nl_update(Node, Val, NList) -> NList'
nl_update(Node, Val, [{Node, _OldVal} | T]) ->
    [{Node, Val} | T];
nl_update(Node, Val, [H | T]) ->
    [H | nl_update(Node, Val, T)];
nl_update(Node, Val, []) ->
    [{Node, Val}].

%% nl_lookup(Node, NList) -> Val
nl_lookup(Node, NList) ->
    {value, {_Node,Val}} = lists:keysearch(Node, 1, NList),
    Val.

%% nl_exists(Node, NList) -> bool()
nl_exists(Node, NList) ->
    case lists:keysearch(Node, 1, NList) of
	{value, _Val} ->
	    true;
	false ->
	    false
    end.
