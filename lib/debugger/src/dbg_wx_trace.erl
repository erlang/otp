%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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

%%
-module(dbg_wx_trace).

%% External exports
-export([start/1, start/3]).
-export([title/1]).

-define(TRACEWIN, ['Button Area', 'Evaluator Area', 'Bindings Area']).
-define(BACKTRACE, 100).

-record(state, {win,           % term() Attach process window data
		coords,        % {X,Y} Mouse point position

		pid,           % pid() Debugged process
		meta,          % pid() Meta process
		status,        % {Status,Mod,Line} � {exit,Where,Reason}
		               %   Status = init � idle | break
		               %      | wait_break � wait_running
		               %      � running
                               % Where={Mod,Line} | null

		cm,            % atom() | undefined Current module
		cm_obsolete=false, % boolean() Curr mod needs reloading

		stack,         % {Cur,Max}

		trace,         % boolean()
		stack_trace,   % all | no_tail | false
		backtrace      % integer() #call frames to fetch
	       }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(Pid)
%% start(Parent, Pid, TraceWin, BackTrace)
%%   Parent = #wxObj{}
%%   Pid = pid()
%%   TraceWin = [WinArea]
%%     WinArea = 'Button|Evaluator|Bindings|Trace Area'
%%   Backtrace = integer()
%%--------------------------------------------------------------------
start(Pid) -> % Used by debugger:quick/3 (no monitor)    
    start(Pid, ?TRACEWIN, ?BACKTRACE).
start(Pid, TraceWin, BackTrace) ->    
    case {whereis(dbg_wx_mon), whereis(dbg_ui_mon)} of
	{undefined, undefined} ->
	    case which_gui() of
		gs ->
		    dbg_ui_trace:start(Pid, TraceWin, BackTrace);
		wx ->
		    Parent = wx:new(),
		    Env = wx:get_env(),
		    start(Pid, Env, Parent, TraceWin, BackTrace)
	    end;
	{undefined, Monitor} when is_pid(Monitor) ->
	    dbg_ui_trace:start(Pid, TraceWin, BackTrace);
	{Monitor, _} when is_pid(Monitor) ->
	    Monitor ! {?MODULE, self(), get_env},
	    receive 
		{env, Monitor, Env, Parent} ->
		    start(Pid, Env, Parent, TraceWin, BackTrace)
	    end
    end.
    
start(Pid, Env, Parent, TraceWin, BackTrace) ->
    %% Inform int about my existence and get the meta pid back
    case int:attached(Pid) of
	{ok, Meta} ->
	    try
		wx:set_env(Env),
		init(Pid, Parent, Meta, TraceWin, BackTrace)
	    catch 
		_:stop ->
		    exit(stop);
		E:R ->
		    io:format("TraceWin Crashed ~p~n",[E]),
		    io:format(" ~p in ~p~n",[R, erlang:get_stacktrace()]),
		    exit(R)
	    end;
	error ->
	    ignore
    end.

which_gui() ->
    try
	wx:new(),
	wx:destroy(),
	wx
    catch _:_ ->
	    gs
    end.

%%--------------------------------------------------------------------
%% title(Pid) -> string()
%% By exporting this function, dbg_wx_mon may check with dbg_wx_winman
%% if there already is an attach window for a given pid and thus avoid
%% spawning processes unnecessarily.
%%--------------------------------------------------------------------
title(Pid) ->
    "Attach Process " ++ pid_to_list(Pid).


%%====================================================================
%% Main loop and message handling
%%====================================================================

init(Pid, Parent, Meta, TraceWin, BackTrace) ->

    %% Start necessary stuff
    dbg_wx_trace_win:init(),               % Graphics system

    %% Create attach process window
    Title = title(Pid),
    Win = dbg_wx_trace_win:create_win(Parent, Title, TraceWin, menus()),
    Window = dbg_wx_trace_win:get_window(Win),
    dbg_wx_winman:insert(Title, Window),

    %% Initial process state
    State1 = #state{win=Win, coords={0,0}, pid=Pid, meta=Meta,
		    status={idle,null,null},
		    stack={1,1}},

    State2 = init_options(TraceWin,
			  int:stack_trace(),    % Stack Trace
			  BackTrace,            % Back trace size
			  State1),

    State3 = init_contents(int:all_breaks(),    % Breakpoints
			   State2),

    int:meta(Meta, trace, State3#state.trace),

    gui_enable_updown(stack_trace, {1,1}),
    gui_enable_btrace(false, false),
    dbg_wx_trace_win:display(Win,idle),

    loop(State3).

init_options(TraceWin, StackTrace, BackTrace, State) ->
    lists:foreach(fun(Area) -> dbg_wx_trace_win:select(Area, true) end,
		  TraceWin),

    Trace = lists:member('Trace Area', TraceWin),

    dbg_wx_trace_win:select(map(StackTrace), true),

    %% Backtrace size is (currently) not shown in window

    State#state{trace=Trace,stack_trace=StackTrace,backtrace=BackTrace}.

init_contents(Breaks, State) ->
    Win =
	lists:foldl(fun(Break, Win) ->
			    dbg_wx_trace_win:add_break(Win,'Break',Break)
		    end,
		    State#state.win,
		    Breaks),

    State#state{win=Win}.

loop(#state{meta=Meta, win=Win} = State) ->
    receive
	%% From the GUI main window
	GuiEvent when element(1, GuiEvent) =:= wx ->
	    Cmd = wx:batch(fun() -> 
				   dbg_wx_trace_win:handle_event(GuiEvent,Win)
			   end),
	    State2 = gui_cmd(Cmd, State),
	    loop(State2);

	%% From the GUI help windows
	{gui, Cmd} ->
	    State2 = gui_cmd(Cmd, State),
	    loop(State2);

	%% From the interpreter
	{int, Cmd} ->
	    State2 = int_cmd(Cmd, State),
	    loop(State2);

	%% From the meta process
	{Meta, Cmd} ->
	    State2 = meta_cmd(Cmd, State),
	    loop(State2);
	{NewMeta, {exit_at, Where, Reason, Cur}} ->
	    State2 = meta_cmd({exit_at, Where, Reason, Cur},
			      State#state{meta=NewMeta}),
	    loop(State2);

	%% From the dbg_wx_winman process (Debugger window manager)
	{dbg_ui_winman, update_windows_menu, Data} ->
	    Window = dbg_wx_trace_win:get_window(Win),
	    dbg_wx_winman:update_windows_menu(Window,Data),
	    loop(State);
	{dbg_ui_winman, destroy} ->
	    dbg_wx_trace_win:stop(Win),
	    exit(stop)
    end.

%%--Commands from the GUI---------------------------------------------

gui_cmd(ignore, State) ->
    State;
gui_cmd({win, Win}, State) ->
    State#state{win=Win};
gui_cmd(stopped, State) ->
    dbg_wx_trace_win:stop(State#state.win),
    exit(stop);
gui_cmd({coords, Coords}, State) ->
    State#state{coords=Coords};

gui_cmd({shortcut, Key}, State) ->
    case shortcut(Key) of
	{always, Cmd} -> gui_cmd(Cmd, State);
	{if_enabled, Cmd} ->
	    case dbg_wx_trace_win:is_enabled(Cmd) of
		true -> gui_cmd(Cmd, State);
		false -> State
	    end;
	false -> State
    end;

%% File menu
gui_cmd('Close', State) ->
    gui_cmd(stopped, State);

%% Edit menu
gui_cmd('Go To Line', State) ->
    %% Will result in message handled below: {gui, {gotoline, Line}}
    Win = dbg_wx_trace_win:helpwin(gotoline, State#state.win),
    State#state{win=Win};
gui_cmd('Search', State) ->
    Win = dbg_wx_trace_win:helpwin(search, State#state.win),
    State#state{win=Win};
gui_cmd({gotoline, Line}, State) ->
    Win = dbg_wx_trace_win:select_line(State#state.win, Line),
    State#state{win=Win};

%% Process menu
gui_cmd('Step', State) ->
    int:meta(State#state.meta, step),
    State;
gui_cmd('Next', State) ->
    int:meta(State#state.meta, next),
    State;
gui_cmd('Continue', State) ->
    int:meta(State#state.meta, continue),
    {Status, Mod, Line} = State#state.status,
    if
	Status =:= wait_break ->
	    Win = dbg_wx_trace_win:unmark_line(State#state.win),
	    gui_enable_functions(wait_running),
	    State#state{win=Win, status={wait_running,Mod,Line}};
	true ->
	    dbg_wx_trace_win:enable(['Stop'], true),
	    dbg_wx_trace_win:enable(['Continue'], false),
	    State
    end;
gui_cmd('Finish', State) ->
    int:meta(State#state.meta, finish),
    State;
gui_cmd('Skip', State) ->
    int:meta(State#state.meta, skip),
    State;
gui_cmd('Time Out', State) ->
    int:meta(State#state.meta, timeout),
    State;
gui_cmd('Stop', State) ->
    int:meta(State#state.meta, stop),
    {Status, Mod, Line} = State#state.status,
    if
	Status =:= wait_running ->
	    Win = dbg_wx_trace_win:mark_line(State#state.win, Line,
					     break),
	    gui_enable_functions(wait_break),
	    gui_enable_updown(State#state.stack_trace,
			      State#state.stack),
	    gui_enable_btrace(State#state.trace,
			      State#state.stack_trace),
	    dbg_wx_trace_win:display(State#state.win,{wait, Mod, Line}),
	    State#state{win=Win, status={wait_break,Mod,Line}};
	true ->
	    dbg_wx_trace_win:enable(['Stop'], false),
	    dbg_wx_trace_win:enable(['Continue'], true),
	    State
    end;
gui_cmd('Where', State) ->
    {_Cur, Max} = State#state.stack,
    Stack = {Max, Max},
    {_Status, Mod, Line} = State#state.status,
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid, break),
    gui_update_bindings(State#state.win, State#state.meta),
    gui_enable_updown(State#state.stack_trace, Stack),
    dbg_wx_trace_win:display(State#state.win,State#state.status),
    State#state{win=Win, cm=Mod, stack=Stack};

gui_cmd('Kill', State) ->
    exit(State#state.pid, kill),
    State;
gui_cmd('Messages', State) ->
    case int:meta(State#state.meta, messages) of
	[] ->
	    dbg_wx_trace_win:eval_output(State#state.win,"< No Messages!\n", bold);
	Messages ->
	    dbg_wx_trace_win:eval_output(State#state.win,"< --- Current Messages ---\n",
					 bold),
	    lists:foldl(
	      fun(Msg, N) ->
		      Str1 = io_lib:format(" ~w:", [N]),
		      dbg_wx_trace_win:eval_output(State#state.win,Str1, bold),
		      Str2 = io_lib:format(" ~s~n",[io_lib:print(Msg)]),
		      dbg_wx_trace_win:eval_output(State#state.win,Str2, normal),
		      N+1
	      end,
	      1,
	      Messages)
    end,
    State;
gui_cmd('Back Trace', State) ->
    dbg_wx_trace_win:trace_output(State#state.win,"\nBACK TRACE\n----------\n"),
    lists:foreach(
      fun({Le, {Mod,Func,Args}}) ->
	      Str = io_lib:format("~p > ~p:~p~p~n",
				  [Le, Mod, Func, Args]),
	      dbg_wx_trace_win:trace_output(State#state.win,Str);
	 ({Le, {Fun,Args}}) ->
	      Str = io_lib:format("~p > ~p~p~n", [Le, Fun, Args]),
	      dbg_wx_trace_win:trace_output(State#state.win,Str);
	 (_) -> ignore
      end,
      int:meta(State#state.meta, backtrace, State#state.backtrace)),
    dbg_wx_trace_win:trace_output(State#state.win,"\n"),
    State;
gui_cmd('Up', State) ->
    {Cur, Max} = State#state.stack,
    case int:meta(State#state.meta, stack_frame, {up, Cur}) of
	{New, {undefined,-1}, _Bs} -> % call from non-interpreted code
	    Stack = {New, Max},
	    Win = dbg_wx_trace_win:show_no_code(State#state.win),
	    dbg_wx_trace_win:update_bindings(State#state.win,[]),
	    gui_enable_updown(State#state.stack_trace, Stack),
	    dbg_wx_trace_win:display(State#state.win,{New,null,null}),
	    State#state{win=Win, cm=null, stack=Stack};

	{New, {Mod,Line}, Bs} ->
	    Stack = {New, Max},
	    Win = gui_show_module(State#state.win, Mod, Line,
				  State#state.cm, State#state.pid,
				  where),
	    dbg_wx_trace_win:update_bindings(State#state.win,Bs),
	    gui_enable_updown(State#state.stack_trace, Stack),
	    dbg_wx_trace_win:display(State#state.win,{New,Mod,Line}),
	    State#state{win=Win, cm=Mod, stack=Stack};
	top ->
	    dbg_wx_trace_win:enable(['Up'], false),
	    State
    end;
gui_cmd('Down', State) ->
    {Cur, Max} = State#state.stack,
    case int:meta(State#state.meta, stack_frame, {down, Cur}) of
	{New, {undefined,-1}, _Bs} -> % call from non-interpreted code
	    Stack = {New, Max},
	    Win = dbg_wx_trace_win:show_no_code(State#state.win),
	    dbg_wx_trace_win:update_bindings(State#state.win, []),
	    gui_enable_updown(State#state.stack_trace, Stack),
	    dbg_wx_trace_win:display(State#state.win, {New,null,null}),
	    State#state{win=Win, cm=null, stack=Stack};

	{New, {Mod,Line}, Bs} ->
	    Stack = {New, Max},
	    Win = gui_show_module(State#state.win, Mod, Line,
				  State#state.cm, State#state.pid,
				  where),
	    dbg_wx_trace_win:update_bindings(State#state.win, Bs),
	    gui_enable_updown(State#state.stack_trace, Stack),
	    dbg_wx_trace_win:display(State#state.win, {New,Mod,Line}),
	    State#state{win=Win, cm=Mod, stack=Stack};

	bottom ->
	    gui_cmd('Where', State)
    end;

%% Break menu
gui_cmd('Line Break...', State) ->
    add_break(State#state.win, State#state.coords, line,
	      State#state.cm,
	      dbg_wx_trace_win:selected_line(State#state.win)),
    State;
gui_cmd('Conditional Break...', State) ->
    add_break(State#state.win, State#state.coords, conditional,
	      State#state.cm,
	      dbg_wx_trace_win:selected_line(State#state.win)),
    State;
gui_cmd('Function Break...', State) ->
    add_break(State#state.win, State#state.coords, function,
	      State#state.cm, undefined),
    State;
gui_cmd('Enable All', State) ->
    Breaks = int:all_breaks(),
    ThisMod = State#state.cm,
    lists:foreach(fun ({{Mod, Line}, _Options}) when Mod =:= ThisMod ->
			  int:enable_break(Mod, Line);
		      (_Break) ->
			  ignore
		  end,
		  Breaks),
    State;
gui_cmd('Disable All', State) ->
    Breaks = int:all_breaks(),
    ThisMod = State#state.cm,
    lists:foreach(fun ({{Mod, Line}, _Options}) when Mod =:= ThisMod ->
			  int:disable_break(Mod, Line);
		      (_Break) ->
			  ignore
		  end,
		  Breaks),
    State;
gui_cmd('Delete All', State) ->
    int:no_break(State#state.cm),
    State;
gui_cmd({break, {Mod, Line}, What}, State) ->
    case What of
	add -> int:break(Mod, Line);
	delete -> int:delete_break(Mod, Line);
	{status, inactive} -> int:disable_break(Mod, Line);
	{status, active} -> int:enable_break(Mod, Line);
	{trigger, Action} -> int:action_at_break(Mod, Line, Action)
    end,
    State;

%% Options menu
gui_cmd({'Trace Window', TraceWin}, State) ->
    Trace = lists:member('Trace Area', TraceWin),
    int:meta(State#state.meta, trace, Trace),
    Win = dbg_wx_trace_win:configure(State#state.win, TraceWin),
    {Status,_,_} = State#state.status,
    if
	Status =:= break; Status =:= wait_break ->
	    gui_enable_btrace(Trace, State#state.stack_trace);
	true -> ignore
    end,
    State#state{win=Win, trace=Trace};
gui_cmd({'Stack Trace', [Name]}, State) ->
    int:meta(State#state.meta, stack_trace, map(Name)),
    {Status,_,_} = State#state.status,
    if
	Status =:= break; Status =:= wait_break ->
	    gui_enable_btrace(State#state.trace, map(Name));
	true -> ignore
    end,
    State;
gui_cmd('Back Trace Size...', State) ->
    Win = dbg_wx_trace_win:get_window(State#state.win),
    case dbg_wx_win:entry(Win, "Backtrace",'Backtrace:', {integer, State#state.backtrace}) of
	cancel -> State;
	{_, BackTrace} ->  State#state{backtrace=BackTrace}
    end;

%% Help menu
gui_cmd('Debugger', State) ->
    Window = dbg_wx_trace_win:get_window(State#state.win),
    HelpFile = filename:join([code:lib_dir(debugger),
			      "doc", "html", "part_frame.html"]),
    dbg_wx_win:open_help(Window, HelpFile),
    State;

gui_cmd({user_command, Cmd}, State) ->
    {Status, _Mod, _Line} = State#state.status,
    if
	Status =:= break;
	Status =:= wait_break;
	Status =:= wait_running ->
	    Cm = State#state.cm,
	    Arg = case State#state.stack of
		      {Cur, Max} when Cur<Max -> {Cm, Cmd, Cur};
		      _Stack -> {Cm, Cmd}
		  end,

	    %% Reply will be received as {Meta, {eval_rsp, Res}}
	    int:meta(State#state.meta, eval, Arg);
	true ->
	    Str = "Commands not allowed",
	    dbg_wx_trace_win:eval_output(State#state.win, [$<,Str,10], normal)
    end,
    State;

gui_cmd({edit, {Var, Val}}, State) ->
    Window = dbg_wx_trace_win:get_window(State#state.win),
    case dbg_wx_win:entry(Window, "Edit variable", Var, {term, Val}) of
	cancel ->
	    State;
	{Var, Term} ->
	    Cmd = atom_to_list(Var)++"="++io_lib:format("~p", [Term]),
	    gui_cmd({user_command, lists:flatten(Cmd)}, State)
    end.

add_break(WI, Coords, Type, undefined, _Line) ->
    Win = dbg_wx_trace_win:get_window(WI),
    dbg_wx_break:start(Win, Coords, Type);
add_break(WI, Coords, Type, Mod, undefined) ->
    Win = dbg_wx_trace_win:get_window(WI),
    dbg_wx_break:start(Win, Coords, Type, Mod);
add_break(WI, Coords, Type, Mod, Line) ->
    Win = dbg_wx_trace_win:get_window(WI),
    dbg_wx_break:start(Win, Coords, Type, Mod, Line).

%%--Commands from the interpreter-------------------------------------

int_cmd({interpret, Mod}, State) ->
    if
	Mod =:= State#state.cm ->
	    State#state{cm_obsolete=true};
	true ->
	    State
    end;
int_cmd({no_interpret, Mod}, State) ->
    if
	Mod =:= State#state.cm ->
	    State#state{cm_obsolete=true};
	true ->
	    Win = dbg_wx_trace_win:remove_code(State#state.win, Mod),
	    State#state{win=Win}
    end;

int_cmd({new_break, Break}, State) ->
    Win = dbg_wx_trace_win:add_break(State#state.win, 'Break', Break),
    State#state{win=Win};
int_cmd({delete_break, Point}, State) ->
    Win = dbg_wx_trace_win:delete_break(State#state.win, Point),
    State#state{win=Win};
int_cmd({break_options, Break}, State) ->
    Win = dbg_wx_trace_win:update_break(State#state.win, Break),
    State#state{win=Win};
int_cmd(no_break, State) ->
    Win = dbg_wx_trace_win:clear_breaks(State#state.win),
    State#state{win=Win};
int_cmd({no_break, Mod}, State) ->
    Win = dbg_wx_trace_win:clear_breaks(State#state.win, Mod),
    State#state{win=Win}.

%%--Commands from the meta process------------------------------------

%% Message received when first attached to a living process
%% '_Trace' is a boolean indicating if the process is traced or not --
%% ignore this as we already have ordered tracing or not depending on if
%% the Trace Area is shown or not.
meta_cmd({attached, Mod, Line, _Trace}, State) ->
    Win = if
	      Mod/=undefined ->
		  gui_enable_functions(init),
		  gui_show_module(State#state.win, Mod, Line,
				  State#state.cm, State#state.pid,
				  break);
	      true -> State#state.win
	  end,
    State#state{win=Win, status={init,Mod,Line}, cm=Mod};

%% Message received when returning to interpreted code
meta_cmd({re_entry, dbg_ieval, eval_fun}, State) ->
    State;
meta_cmd({re_entry, Mod, _Func}, State) ->
    Obs = State#state.cm_obsolete,
    case State#state.cm of
	Mod when Obs =:= true ->
	    Win = gui_load_module(State#state.win, Mod,State#state.pid),
	    State#state{win=Win, cm_obsolete=false};
	Mod -> State;
	Cm ->
	    Win = gui_show_module(State#state.win, Mod, 0,
				  Cm, State#state.pid, break),
	    State#state{win=Win, cm=Mod}
    end;

%% Message received when attached to a terminated process
meta_cmd({exit_at, null, Reason, Cur}, State) ->
    Stack = {Cur, Cur},
    gui_enable_functions(exit),
    gui_enable_updown(false, Stack),
    dbg_wx_trace_win:display(State#state.win, {exit, null, Reason}),
    State#state{status={exit,null,Reason}, stack=Stack};
meta_cmd({exit_at, {Mod,Line}, Reason, Cur}, State) ->
    Stack = {Cur+1, Cur+1},
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid, break),
    gui_enable_functions(exit),
    gui_enable_updown(State#state.stack_trace, Stack),
    gui_enable_btrace(State#state.trace, State#state.stack_trace),
    gui_update_bindings(State#state.win, State#state.meta),
    dbg_wx_trace_win:display(State#state.win, {exit, {Mod,Line}, Reason}),
    State#state{win=Win, cm=Mod,status={exit,{Mod,Line},Reason},
		stack=Stack};

meta_cmd({break_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid, break),
    gui_enable_functions(break),
    gui_enable_updown(State#state.stack_trace, Stack),
    gui_enable_btrace(State#state.trace, State#state.stack_trace),
    gui_update_bindings(State#state.win, State#state.meta),
    dbg_wx_trace_win:display(State#state.win, {break, Mod, Line}),
    State#state{win=Win, cm=Mod, status={break,Mod,Line}, stack=Stack};
meta_cmd({func_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid, where),
    gui_enable_functions(idle),
    dbg_wx_trace_win:display(State#state.win, idle),
    State#state{win=Win, cm=Mod, status={idle,Mod,Line}, stack=Stack};
meta_cmd({wait_at, Mod, Line, Cur}, #state{status={Status,_,_}, win=Win}=State)
  when Status =/= init, Status =/= break ->
    Stack = {Cur,Cur},
    gui_enable_functions(wait_running),
    dbg_wx_trace_win:display(Win, {wait,Mod,Line}),
    State#state{status={wait_running,Mod,Line}, stack=Stack};
meta_cmd({wait_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid, break),
    gui_enable_functions(wait_break),
    gui_enable_updown(State#state.stack_trace, Stack),
    gui_enable_btrace(State#state.trace, State#state.stack_trace),
    gui_update_bindings(State#state.win, State#state.meta),
    dbg_wx_trace_win:display(State#state.win, {wait, Mod, Line}),
    State#state{win=Win, cm=Mod, status={wait_break,Mod,Line},
		stack=Stack};
meta_cmd({wait_after_at, Mod, Line, Sp}, State) ->
    meta_cmd({wait_at, Mod, Line, Sp}, State);
meta_cmd(running, State) ->
    Win = dbg_wx_trace_win:unmark_line(State#state.win),
    gui_enable_functions(running),
    dbg_wx_trace_win:update_bindings(State#state.win, []),
    dbg_wx_trace_win:display(State#state.win, {running, State#state.cm}),
    State#state{win=Win, status={running,null,null}};

meta_cmd(idle, State) ->
    Win = dbg_wx_trace_win:show_no_code(State#state.win),
    gui_enable_functions(idle),
    dbg_wx_trace_win:update_bindings(State#state.win, []),
    dbg_wx_trace_win:display(State#state.win, idle),
    State#state{win=Win, status={idle,null,null}, cm=undefined};

%% Message about changed trace option can be ignored, the change must
%% have been ordered by this process. (In theory, the change could have
%% been ordered by another attached process. The Debugger, though,
%% allows max one attached process per debugged process).
meta_cmd({trace, _Bool}, State) ->
    State;

meta_cmd({stack_trace, Flag}, State) ->
    dbg_wx_trace_win:select(map(Flag), true),
    gui_enable_updown(Flag, State#state.stack),
    {Status,_,_} = State#state.status,
    if
	Status =:= break; Status =:= wait_break ->
	    gui_enable_btrace(State#state.trace, Flag);
	true -> ignore
    end,
    State#state{stack_trace=Flag};

meta_cmd({trace_output, Str}, State) ->
    dbg_wx_trace_win:trace_output(State#state.win, Str),
    State;

%% Reply on a user command
meta_cmd({eval_rsp, Res}, State) ->
    Str = io_lib:print(Res),
    dbg_wx_trace_win:eval_output(State#state.win, [$<,Str,10], normal),
    State.


%%====================================================================
%% GUI auxiliary functions
%%====================================================================

menus() ->
    [{'File', [{'Close', no}]},
     {'Edit', [{'Go To Line', 0},
	       {'Search', 1}]},
     {'Process', [{'Step', 0},
		  {'Next', 0},
		  {'Continue', 0},
		  {'Finish', 0},
		  {'Skip', no},
		  {'Time Out', no},
		  {'Stop', no},
		  separator,
		  {'Kill', no},
		  separator,
		  {'Messages', 0},
		  {'Back Trace', no},
		  separator,
		  {'Where', 0},
		  {'Up', no},
		  {'Down', no}]},
     {'Break', [{'Line Break...', 5},
		{'Conditional Break...', no},
		{'Function Break...', no},
		separator,
		{'Enable All', no},
		{'Disable All', no},
		{'Delete All', 0},
		separator]},
     {'Options', [{'Trace Window', no, cascade,
		   [{'Search Area', no, check},
		    {'Button Area', no, check},
		    {'Evaluator Area', no, check},
		    {'Bindings Area', no, check},
		    {'Trace Area', no, check}]},
		  {'Stack Trace', no, cascade,
		   [{'Stack On, Tail', no, radio},
		    {'Stack On, No Tail', no, radio},
		    {'Stack Off', no, radio}]},
		  {'Back Trace Size...', no}]},
     {'Windows', []},
     {'Help', [{'Debugger', no}]}].

%% enable(Status) -> [MenuItem]
%%   Status = init  % when first message from Meta has arrived
%%          | idle | break | exit | wait_break � wait_running | running
enable(init) -> [];
enable(idle) -> ['Stop','Kill'];
enable(break) -> ['Step','Next','Continue','Finish','Skip',
		  'Kill','Messages'];
enable(exit) -> [];
enable(wait_break) -> ['Continue','Time Out','Kill'];
enable(wait_running) -> ['Stop','Kill'];
enable(running) -> ['Stop','Kill'].

all_buttons() ->
    ['Step','Next','Continue','Finish','Skip','Time Out','Stop',
     'Kill','Messages','Back Trace','Where','Up','Down'].

shortcut(e) -> {if_enabled, 'Search'};
shortcut(g) -> {if_enabled, 'Go To Line'};
shortcut(s) -> {if_enabled, 'Step'};
shortcut(n) -> {if_enabled, 'Next'};
shortcut(c) -> {if_enabled, 'Continue'};
shortcut(f) -> {if_enabled, 'Finish'};
shortcut(m) -> {if_enabled, 'Messages'};
shortcut(w) -> {if_enabled, 'Where'};

shortcut(b) -> {always, 'Line Break...'};
shortcut(d) -> {always, 'Delete All'};

shortcut(_) -> false.

map('Stack On, Tail')    -> all;               % Stack trace
map('Stack On, No Tail') -> no_tail;
map('Stack Off')         -> false;
map(all)                 -> 'Stack On, Tail';
map(true)                -> 'Stack On, Tail';
map(no_tail)             -> 'Stack On, No Tail';
map(false)               -> 'Stack Off'.


%% gui_show_module(Win, Mod, Line, Cm, Pid, How) -> Win
%% gui_show_module(Win, {Mod,Line}, _Reason, Cm, Pid, How) -> Win
%%   How = where | break
%% Show contents of a module in code area
gui_show_module(Win, {Mod,Line}, _Reason, Cm, Pid, How) ->
    gui_show_module(Win, Mod, Line, Cm, Pid, How);
gui_show_module(Win, Mod, Line, Mod, _Pid, How) ->
    dbg_wx_trace_win:mark_line(Win, Line, How);
gui_show_module(Win, Mod, Line, _Cm, Pid, How) ->
    Win2 = case dbg_wx_trace_win:is_shown(Win, Mod) of
	       {true, Win3} -> Win3;
	       false -> gui_load_module(Win, Mod, Pid)
	   end,
    dbg_wx_trace_win:mark_line(Win2, Line, How).

gui_load_module(Win, Mod, _Pid) ->
    dbg_wx_trace_win:display(Win,{text, "Loading module..."}),
    %% Contents = int:contents(Mod, Pid),
    {ok, Contents} = dbg_iserver:call({raw_contents, Mod, any}),
    Win2 = dbg_wx_trace_win:show_code(Win, Mod, Contents),
    dbg_wx_trace_win:display(Win,{text, ""}),
    Win2.

gui_update_bindings(Win,Meta) ->
    Bs = int:meta(Meta, bindings, nostack),
    dbg_wx_trace_win:update_bindings(Win,Bs).

gui_enable_functions(Status) ->
    Enable = enable(Status),
    Disable = all_buttons() -- Enable,
    dbg_wx_trace_win:enable(Disable, false),
    dbg_wx_trace_win:enable(Enable, true).

gui_enable_updown(Flag, Stack) ->
    {Enable, Disable} =
	if
	    Flag =:= false -> {[], ['Up', 'Down']};
	    true ->
		case Stack of
		    {1,1} -> {[], ['Up', 'Down']};
		    {2,2} -> {[], ['Up', 'Down']};
		    {Max,Max} -> {['Up'], ['Down']};
		    {2,_Max} -> {['Down'], ['Up']};
		    {_Cur,_Max} -> {['Up', 'Down'], []}
		end
	end,
    dbg_wx_trace_win:enable(Enable, true),
    dbg_wx_trace_win:enable(Disable, false),
    if
	Enable =:= [] -> dbg_wx_trace_win:enable(['Where'], false);
	true -> dbg_wx_trace_win:enable(['Where'], true)
    end.

gui_enable_btrace(Trace, StackTrace) ->
    Bool = if
	       Trace =:= false -> false;
	       StackTrace =:= false -> false;
	       true -> true
	   end,
    dbg_wx_trace_win:enable(['Back Trace'], Bool).
