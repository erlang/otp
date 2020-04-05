%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
-module(dbg_wx_mon).

-include_lib("kernel/include/file.hrl").
-include_lib("wx/include/wx.hrl").

%% External exports
-export([start/2, stop/0]).

-define(TRACEWIN, ['Search Area', 'Button Area', 'Evaluator Area', 'Bindings Area']).
-define(BACKTRACE, 100).
-define(STRNAME, 'Use range of +pc flag'). % See also erl(1)
-define(STRINGS, [str_on]).
-define(AUTO_ATTACH, [init, exit, break]).

-record(pinfo, {pid,       % pid()
		status     % break | exit | idle | running | waiting
	       }).

-record(state, {mode,      % local | global
		starter,   % bool() 'true' if int was started by me

		win,       % term() Monitor window data
		focus,     % undefined | #pinfo{} Process in focus
		coords,    % {X,Y} Mouse pointer position

		intdir,    % string() Default dir
		pinfos,    % [#pinfo{}] Debugged processes

		tracewin,  % [Area] Areas shown in trace window
		backtrace, % integer() Number of call frames to fetch
                strings,   % [str_on] Integer lists as strings

		attach,    % false | {Flags, Function}

		sfile,     % default | string() Settings file
		changed    % boolean() Settings have been changed
	       }). 

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(Mode, SFile) -> {ok, Pid} | {error, Reason}
%%   Mode = local | global
%%   SFile = string() | default  Settings file
%%   Pid = pid()
%%   Reason = {already_started,Pid} | term()
%%--------------------------------------------------------------------
start(Mode, SFile) ->
    case whereis(?MODULE) of
	undefined ->
	    CallingPid = self(),
	    Pid = spawn(fun () -> init(CallingPid, Mode, SFile) end),
	    receive
		{initialization_complete, Pid} ->
		    {ok, Pid};
		Error ->
		    Error
	    end;

	Pid ->
            {error, {already_started,Pid}}
    end.

%%--------------------------------------------------------------------
%% stop() -> ok
%%--------------------------------------------------------------------
stop() ->
    case whereis(?MODULE) of
	undefined ->
	    ok;
	Pid ->
	    Flag = process_flag(trap_exit, true),
	    link(Pid),
	    Pid ! stop,
	    receive
		{'EXIT', Pid, stop} ->
		    process_flag(trap_exit, Flag),
		    ok
	    end
    end.


%%====================================================================
%% Initialization
%%====================================================================

init(CallingPid, Mode, SFile) ->
    register(?MODULE, self()),

    %% Graphics system
    case catch dbg_wx_mon_win:init() of
	{'EXIT', Reason} ->
	    CallingPid ! {error, Reason};
	GS ->
	    try 
		init2(CallingPid, Mode, SFile, GS)
	    catch 
		exit:stop -> stop;
		Error:Reason:Stacktrace ->
		    io:format("~p: Crashed {~p,~p} in~n  ~p",
			      [?MODULE, Error, Reason, Stacktrace])
	    end
    end.

init2(CallingPid, Mode, SFile, GS) ->
    %% Start Int if necessary and subscribe to information from it
    Bool = case int:start() of
	       {ok, _Int} -> true;
	       {error, {already_started, _Int}} -> false
	   end,
    int:subscribe(),

    %% Start other necessary stuff
    dbg_wx_win:init(),     
    _ = dbg_wx_winman:start(), % Debugger window manager

    %% Create monitor window
    Title = "Monitor",
    Win = dbg_wx_mon_win:create_win(GS, Title, menus()),
    Window = dbg_wx_mon_win:get_window(Win),
    dbg_wx_winman:insert(Title, Window),

    %% Initial process state
    State1 = #state{mode    = Mode,
		    starter = Bool,

		    win     = Win,
		    focus   = undefined,
		    coords  = {-1,-1},

		    intdir  = element(2, file:get_cwd()),
		    pinfos  = [],

		    sfile   = SFile,
		    changed = false
		   },

    State2 = init_options(?TRACEWIN,            % Trace Window
			  int:auto_attach(),    % Auto Attach
			  int:stack_trace(),    % Stack Trace
			  ?BACKTRACE,           % Back Trace Size
                          ?STRINGS,             % Lists as Strings
			  State1),

    State3 = init_contents(int:interpreted(),   % Modules
			   int:all_breaks(),    % Breakpoints
			   int:snapshot(),      % Processes
			   State2),

    %% Disable/enable functionality according to process in focus (none)
    gui_enable_functions(State3#state.focus),

    CallingPid ! {initialization_complete, self()},

    if
	SFile =:= default ->
	    loop(State3);
	true ->
	    loop(load_settings(SFile, State3))
    end.

init_options(TraceWin, AutoAttach, StackTrace, BackTrace, Strings, State) ->
    lists:foreach(fun(Area) ->
			  dbg_wx_mon_win:select(Area, true)
		  end,
		  TraceWin),

    case AutoAttach of
	false -> ignore;
	{Flags, _Function} ->
	    dbg_wx_mon_win:show_option(State#state.win,
				       auto_attach, Flags),
            select(Flags, ?AUTO_ATTACH)
    end,

    dbg_wx_mon_win:show_option(State#state.win,
			       stack_trace, StackTrace),
    dbg_wx_mon_win:select(map(StackTrace), true),

    dbg_wx_mon_win:show_option(State#state.win, back_trace, BackTrace),

    select(Strings, ?STRINGS),
    dbg_wx_mon_win:show_option(State#state.win, strings, Strings),

    State#state{tracewin=TraceWin, backtrace=BackTrace, strings=Strings}.

init_contents(Mods, Breaks, Processes, State) ->
    Win2 =
        lists:foldl(fun(Mod, Win) ->
			    dbg_wx_mon_win:add_module(Win,'Module',Mod)
		    end,
		    State#state.win,
		    Mods),

    Win3 = 
        lists:foldl(fun(Break, Win) ->
			    dbg_wx_mon_win:add_break(Win,'Break',Break)
		    end,
		    Win2,
		    Breaks),

    lists:foldl(fun(PidTuple, State0) ->
			int_cmd({new_process, PidTuple}, State0)
		end,
		State#state{win=Win3},
		Processes).


%%====================================================================
%% Main loop and message handling
%%====================================================================

loop(State) ->
    receive
	stop ->
	    gui_cmd(stopped, State);
	
	%% From the wx-GUI
	#wx{} = GuiEvent ->
	    Cmd = dbg_wx_mon_win:handle_event(GuiEvent,State#state.win),
	    State2 = gui_cmd(Cmd, State),
	    loop(State2);

	%% From the interpreter process
	{int, Cmd} ->
	    State2 = int_cmd(Cmd, State),
	    loop(State2);

	%% From the dbg_ui_interpret process
	{dbg_ui_interpret, Dir} ->
	    loop(State#state{intdir=Dir});
	
	%% From the dbg_wx_winman process (Debugger window manager)
	{dbg_ui_winman, update_windows_menu, Data} ->
	    Window = dbg_wx_mon_win:get_window(State#state.win),
	    dbg_wx_winman:update_windows_menu(Window,Data),
	    loop(State);

	%% From the trace window
	{dbg_wx_trace, From, get_env} -> 
	    From ! {env, self(), wx:get_env(), dbg_wx_mon_win:get_window(State#state.win)},
	    loop(State)
    end.

%%--Commands from the GUI---------------------------------------------
%% Act upon a command from the GUI. In most cases, it is only necessary
%% to call a relevant int-function. int will then report when the action
%% has been taken.

gui_cmd(ignore, State) ->
    State;
gui_cmd(stopped, State) ->
    if
	State#state.starter =:= true -> int:stop();
	true -> int:auto_attach(false)
    end,
    exit(stop);
gui_cmd({coords, Coords}, State) ->
    State#state{coords=Coords};

gui_cmd({shortcut, Key}, State) ->
    case shortcut(Key) of
	{always, Cmd} -> gui_cmd(Cmd, State);
	{if_enabled, Cmd} ->
	    case dbg_wx_mon_win:is_enabled(Cmd) of
		true -> gui_cmd(Cmd, State);
		false -> State
	    end;
	false -> State
    end;

%% File Menu
gui_cmd('Load Settings...', State) ->
    Window = dbg_wx_mon_win:get_window(State#state.win),
    case dbg_wx_settings:load(Window, State#state.coords,State#state.sfile) of
	cancel ->     State;
	{ok, File} -> load_settings(File,State)
    end;
gui_cmd('Save Settings...', State) ->
    Window = dbg_wx_mon_win:get_window(State#state.win),
    case dbg_wx_settings:save(Window, State#state.coords,State#state.sfile) of
	cancel ->     State;
	{ok, File} -> save_settings(File,State)
    end;
gui_cmd('Exit', State) ->
    gui_cmd(stopped, State);

%% Edit Menu
gui_cmd('Refresh', State) ->
    int:clear(),
    Win = dbg_wx_mon_win:clear_processes(State#state.win),
    gui_enable_functions(undefined),
    State2 = State#state{win=Win, focus=undefined, pinfos=[]},
    lists:foldl(fun(PidTuple, S) ->
			int_cmd({new_process,PidTuple}, S)
		end,
		State2,
		int:snapshot());
gui_cmd('Kill All', State) ->
    lists:foreach(fun(PInfo) ->
			  case PInfo#pinfo.status of
			      exit -> ignore;
			      _Status -> exit(PInfo#pinfo.pid, kill)
			  end
		  end,
		  State#state.pinfos),
    State;

%% Module Menu
gui_cmd('Interpret...', State) ->
    Window = dbg_wx_mon_win:get_window(State#state.win),
    dbg_wx_interpret:start(Window, State#state.coords,
 			   State#state.intdir, State#state.mode),
    State;
gui_cmd('Delete All Modules', State) ->
    lists:foreach(fun(Mod) -> int:nn(Mod) end, int:interpreted()),
    State;
gui_cmd({module, Mod, What}, State) ->
    _ = case What of
	delete -> int:nn(Mod);
	view -> 
	    Window = dbg_wx_mon_win:get_window(State#state.win),
	    dbg_wx_view:start(Window, Mod)
    end,
    State;

%% Process Menu
gui_cmd('Step', State) ->
    int:step((State#state.focus)#pinfo.pid),
    State;
gui_cmd('Next', State) ->
    int:next((State#state.focus)#pinfo.pid),
    State;
gui_cmd('Continue', State) ->
    int:continue((State#state.focus)#pinfo.pid),
    State;
gui_cmd('Finish ', State) ->
    int:finish((State#state.focus)#pinfo.pid),
    State;
gui_cmd('Attach', State) ->
    Pid = (State#state.focus)#pinfo.pid,
    case dbg_wx_winman:is_started(dbg_wx_trace:title(Pid)) of
	true -> ignore;
	false -> int:attach(Pid, trace_function(State))
    end,
    State;
gui_cmd('Kill', State) ->
    exit((State#state.focus)#pinfo.pid, kill),
    State;

%% Break Menu
gui_cmd('Line Break...', State) ->
    Window = dbg_wx_mon_win:get_window(State#state.win),
    dbg_wx_break:start(Window, State#state.coords, line),
    State;
gui_cmd('Conditional Break...', State) ->
    Window = dbg_wx_mon_win:get_window(State#state.win),
    dbg_wx_break:start(Window, State#state.coords, conditional),
    State;
gui_cmd('Function Break...', State) ->
    Window = dbg_wx_mon_win:get_window(State#state.win),
    dbg_wx_break:start(Window, State#state.coords, function),
    State;
gui_cmd('Enable All', State) ->
    Breaks = int:all_breaks(),
    lists:foreach(fun ({{Mod, Line}, _Options}) ->
			  int:enable_break(Mod, Line)
		  end,
		  Breaks),
    State;
gui_cmd('Disable All', State) ->
    Breaks = int:all_breaks(),
    lists:foreach(fun ({{Mod, Line}, _Options}) ->
			  int:disable_break(Mod, Line)
		  end,
		  Breaks),
    State;
gui_cmd('Delete All', State) ->
    int:no_break(),
    State;
gui_cmd({break, {Mod, Line}, What}, State) ->
    case What of
	delete -> int:delete_break(Mod, Line);
	{status, inactive} -> int:disable_break(Mod, Line);
	{status, active} -> int:enable_break(Mod, Line);
	{trigger, Action} -> int:action_at_break(Mod, Line, Action)
    end,
    State;

%% Options Commands
gui_cmd({'Trace Window', TraceWin}, State) ->
    State2 = State#state{tracewin=TraceWin},
    case State#state.attach of
	false -> ignore;
	{Flags, {dbg_wx_trace, start, StartFlags}} ->
	    case trace_function(State2) of
		{_, _, StartFlags} -> ignore;
		NewFunction -> % {_, _, NewStartFlags}
		    int:auto_attach(Flags, NewFunction)
	    end;
	_AutoAttach -> ignore
    end,
    State2;
gui_cmd({'Auto Attach', When}, State) ->
    if
	When =:= [] -> int:auto_attach(false);
	true ->
	    Flags = [map(Name) || Name <- When],
	    int:auto_attach(Flags, trace_function(State))
    end,
    State;
gui_cmd({'Stack Trace', [Name]}, State) ->
    int:stack_trace(map(Name)),
    State;
gui_cmd('Back Trace Size...', State) ->
    Window = dbg_wx_mon_win:get_window(State#state.win),
    What = {integer, integer_to_list(State#state.backtrace)},
    case dbg_wx_win:entry(Window, "Backtrace", 'Backtrace:', What) of
	cancel -> 
	    State;
	{_,BackTrace} ->
	    dbg_wx_mon_win:show_option(State#state.win,back_trace, BackTrace),
	    State#state{backtrace=BackTrace}
    end;
gui_cmd({'Strings', Flags}, State) ->
    Names = [map(Flag) || Flag <- Flags],
    dbg_wx_mon_win:show_option(State#state.win, strings, Names),
    select(Names, ?STRINGS),
    State#state{strings=Names};

%% Help Menu
gui_cmd('Debugger', State) ->
    HelpFile = filename:join([code:lib_dir(debugger), "doc", "html", "index.html"]),
    Window = dbg_wx_mon_win:get_window(State#state.win),
    dbg_wx_win:open_help(Window, HelpFile),
    State;

gui_cmd({focus, Pid, Win}, State) ->
    {value, PInfo} =
	lists:keysearch(Pid, #pinfo.pid, State#state.pinfos),
    gui_enable_functions(PInfo),
    State#state{win=Win, focus=PInfo};
gui_cmd(default, State) ->
    case lists:member('Attach', menus(enabled, State#state.focus)) of
	true -> gui_cmd('Attach', State);
	false -> State
    end.

%%--Commands from the interpreter-------------------------------------

int_cmd({interpret, Mod}, State) ->
    Win = dbg_wx_mon_win:add_module(State#state.win, 'Module', Mod),
    State#state{win=Win};
int_cmd({no_interpret, Mod}, State) ->
    Win = dbg_wx_mon_win:delete_module(State#state.win, Mod),
    State#state{win=Win};

int_cmd({new_process, {Pid, Function, Status, Info}}, State) ->

    %% Create record with information about the process
    Name = registered_name(Pid),
    PInfo = #pinfo{pid=Pid, status=Status},

    %% Update window
    Win = dbg_wx_mon_win:add_process(State#state.win,
				     Pid, Name, Function, Status, Info),

    %% Store process information
    PInfos = State#state.pinfos ++ [PInfo],
    State#state{win=Win, pinfos=PInfos};
int_cmd({new_status, Pid, Status, Info}, State) ->

    %% Find stored information about the process
    PInfos = State#state.pinfos,
    {value, PInfo} = lists:keysearch(Pid, #pinfo.pid, PInfos),

    %% Update process information
    PInfo2 = PInfo#pinfo{status=Status},
    PInfos2 = lists:keyreplace(Pid, #pinfo.pid, PInfos, PInfo2),
    State2 = State#state{pinfos=PInfos2},

    %% Update window
    dbg_wx_mon_win:update_process(State2#state.win, Pid, Status, Info),
    case State2#state.focus of
	#pinfo{pid=Pid} ->
	    gui_enable_functions(PInfo2),
	    State2#state{focus=PInfo2};
	_ ->
	    State2
    end;

int_cmd({new_break, Break}, State) ->
    Win = dbg_wx_mon_win:add_break(State#state.win, 'Break', Break),
    State#state{win=Win};
int_cmd({delete_break, Point}, State) ->
    Win = dbg_wx_mon_win:delete_break(State#state.win, Point),
    State#state{win=Win};
int_cmd({break_options, Break}, State) ->
    dbg_wx_mon_win:update_break(State#state.win, Break),
    State;
int_cmd(no_break, State) ->
    Win = dbg_wx_mon_win:clear_breaks(State#state.win),
    State#state{win=Win};
int_cmd({no_break, Mod}, State) ->
    Win = dbg_wx_mon_win:clear_breaks(State#state.win, Mod),
    State#state{win=Win};

int_cmd({auto_attach, AutoAttach}, State) ->
    OnFlags = case AutoAttach of
		  false -> [];
		  {Flags, _Function} -> Flags
	      end,
    dbg_wx_mon_win:show_option(State#state.win, auto_attach, OnFlags),
    select(OnFlags, ?AUTO_ATTACH),
    State#state{attach=AutoAttach};
int_cmd({stack_trace, Flag}, State) ->
    dbg_wx_mon_win:show_option(State#state.win, stack_trace, Flag),
    dbg_wx_mon_win:select(map(Flag), true),
    State.


%%====================================================================
%% GUI auxiliary functions
%%====================================================================

menus() ->
    [{'File', [{'Load Settings...', 0},
	       {'Save Settings...', 2},
	       separator,
	       {'Exit', 0}]},
     {'Edit', [{'Refresh', no},
	       {'Kill All', no}]},
     {'Module', [{'Interpret...', 0},
		 {'Delete All Modules', no},
		 separator]},
     {'Process', [{'Step', 0},
		  {'Next', 0},
		  {'Continue', 0},
		  {'Finish ', 0},
		  separator,
		  {'Attach', 0},
		  {'Kill', no}]},
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
		  {'Auto Attach', no, cascade,
		   [{'First Call', no, check},
		    {'On Break', no, check},
		    {'On Exit', no, check}]},
		  {'Stack Trace', no, cascade,
		   [{'Stack On, Tail', no, radio},
		    {'Stack On, No Tail', no, radio},
		    {'Stack Off', no, radio}]},
		  {'Strings', no, cascade,
		   [{?STRNAME, no, check}]},
		  {'Back Trace Size...', no}]},
     {'Windows', []},
     {'Help', [{'Debugger', no}]}].

menus(enabled,  undefined) ->
    [];
menus(disabled, undefined) ->
    ['Step','Next','Continue','Finish ','Attach','Kill'];
menus(enabled,  #pinfo{status=exit}) ->
    ['Attach'];
menus(disabled, #pinfo{status=exit}) ->
    ['Step','Next','Continue','Finish ','Kill'];
menus(enabled,  #pinfo{status=break}) ->
    ['Step','Next','Continue','Finish ','Attach','Kill'];
menus(disabled, #pinfo{status=break}) ->
    [];
menus(enabled,  _PInfo) ->
    ['Attach','Kill'];
menus(disabled, _PInfo) ->
    ['Step','Next','Continue','Finish '].

shortcut(l) -> {always, 'Load Settings...'};
shortcut(v) -> {always, 'Save Settings...'};
shortcut(e) -> {always, 'Exit'};

shortcut(i) -> {always, 'Interpret...'};

shortcut(s) -> {if_enabled, 'Step'};
shortcut(n) -> {if_enabled, 'Next'};
shortcut(c) -> {if_enabled, 'Continue'};
shortcut(f) -> {if_enabled, 'Finish '};
shortcut(a) -> {if_enabled, 'Attach'};

shortcut(b) -> {always, 'Line Break...'};
shortcut(d) -> {always, 'Delete All'};

shortcut(_) -> false.

%% Enable/disable functionality depending on the state of the process
%% currently in Focus
gui_enable_functions(PInfo) ->
    Enabled = menus(enabled, PInfo),
    Disabled = menus(disabled, PInfo),
    dbg_wx_mon_win:enable(Enabled, true),
    dbg_wx_mon_win:enable(Disabled, false).

%% Map values used by int to/from GUI names
map('First Call')        -> init;               % Auto attach
map('On Exit')           -> exit;
map('On Break')          -> break;
map(init)                -> 'First Call';
map(exit)                -> 'On Exit';
map(break)               -> 'On Break';

map('Stack On, Tail')    -> all;               % Stack trace
map('Stack On, No Tail') -> no_tail;
map('Stack Off')         -> false;
map(all)                 -> 'Stack On, Tail';
map(true)                -> 'Stack On, Tail';
map(no_tail)             -> 'Stack On, No Tail';
map(false)               -> 'Stack Off';

map(?STRNAME)            -> str_on;            % Strings
map(str_on)              -> ?STRNAME.

select(Flags, AllFlags) ->
    OffFlags = AllFlags -- Flags,
    lists:foreach(fun(Flag) ->
                          dbg_wx_mon_win:select(map(Flag), false)
                  end,
                  OffFlags),
    lists:foreach(fun(Flag) ->
                          dbg_wx_mon_win:select(map(Flag), true)
                  end,
                  Flags).

%%====================================================================
%% Debugger settings
%%====================================================================

load_settings(SFile, State) ->
    case file:read_file(SFile) of
	{ok, Binary} ->
	    case catch binary_to_term(Binary) of
		{debugger_settings, Settings} ->
		    load_settings2(Settings,
				   State#state{sfile=SFile,
					       changed=false});
		_Error -> State
	    end;
	{error, _Reason} -> State
    end.

load_settings2(Settings, State) ->
    Vals = loaded(Settings),
    [TraceWin,AutoAttach,StackTrace,BackTrace,Strings,Files,Breaks] = Vals,

    TraceWinAll = ['Button Area', 'Evaluator Area', 'Bindings Area',
		   'Trace Area'],
    lists:foreach(fun(Area) -> dbg_wx_mon_win:select(Area, true) end,
		  TraceWin),
    lists:foreach(fun(Area) -> dbg_wx_mon_win:select(Area, false) end,
		  TraceWinAll--TraceWin),

    case AutoAttach of
	false -> int:auto_attach(false);
	{Flags, Function} -> int:auto_attach(Flags, Function)
    end,

    int:stack_trace(StackTrace),

    if
        Strings =:= keep -> ok;
        true ->
            dbg_wx_mon_win:show_option(State#state.win, strings, Strings),
            select(Strings, ?STRINGS)
    end,

    dbg_wx_mon_win:show_option(State#state.win, back_trace, BackTrace),

    case State#state.mode of
	local -> lists:foreach(fun(File) -> int:i(File) end, Files);
	global -> lists:foreach(fun(File) -> int:ni(File) end, Files)
    end,
    lists:foreach(fun(Break) ->
			  {{Mod, Line}, [Status, Action, _, Cond]} =
			      Break,
			  int:break(Mod, Line),
			  if
			      Status =:= inactive ->
				  int:disable_break(Mod, Line);
			      true -> ignore
			  end,
			  if
			      Action =/= enable ->
				  int:action_at_break(Mod,Line,Action);
			      true -> ignore
			  end,
			  case Cond of
			      CFunction when is_tuple(CFunction) ->
				  int:test_at_break(Mod,Line,CFunction);
			      null -> ignore
			  end
		  end,
		  Breaks),

    State#state{tracewin=TraceWin, backtrace=BackTrace, strings=Strings}.

loaded({TraceWin, AutoAttach, StackTrace, BackTrace, Files, Breaks}) ->
    %% Up to and including R16B
    [TraceWin,AutoAttach,StackTrace,BackTrace,keep,Files,Breaks];
loaded(Settings) when is_list(Settings) ->
    Keys = [trace_win,auto_attach,stack_trace,back_trace,strings,files,
            breaks],
    [proplists:get_value(Key, Settings) || Key <- Keys].

save_settings(SFile, State) ->
    Settings = saved(State),
    Binary = term_to_binary({debugger_settings, Settings}),
    case file:write_file(SFile, Binary) of
	ok ->
	    State#state{sfile=SFile, changed=false};
	{error, _Reason} ->
	    State
    end.

saved(#state{tracewin=TraceWin, backtrace=BackTrace, strings=Strings}) ->
    [{trace_win,TraceWin},
     {auto_attach,int:auto_attach()},
     {stack_trace,int:stack_trace()},
     {back_trace,BackTrace},
     {strings,Strings},
     {files,[int:file(Mod) || Mod <- int:interpreted()]},
     {breaks,int:all_breaks()}].

%%====================================================================
%% Other internal functions
%%====================================================================

registered_name(Pid) ->

    %% Yield in order to give Pid more time to register its name
    timer:sleep(200),

    Node = node(Pid),
    if
	Node =:= node() ->
	    case erlang:process_info(Pid, registered_name) of
		{registered_name, Name} -> Name;
		_ -> undefined
	    end;
	true ->
	    case rpc:call(Node,erlang,process_info,
			  [Pid,registered_name]) of
		{registered_name, Name} -> Name;
		_ -> undefined
	    end
    end.

trace_function(State) ->
    #state{tracewin=Win, backtrace=BT, strings=Str} = State,
    {dbg_wx_trace, start, [Win,BT,Str]}.
