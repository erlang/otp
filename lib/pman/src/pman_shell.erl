%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
%% ---------------------------------------------------------------
%% Purpose: Create a trace window with process 
%%          information or a help window with information 
%%          about pman.
%%
%% ---------------------------------------------------------------

-module(pman_shell).

%% ---------------------------------------------------------------
%% The user interface exports 
%% ---------------------------------------------------------------

-export([start_list/3,
	 start/2,
	 start/1,
	 find_shell/0]).

%% ---------------------------------------------------------------
%% Includes
%% ---------------------------------------------------------------
-include("assert.hrl").
-include("pman_options.hrl").
-include("pman_buf.hrl").


%% ---------------------------------------------------------------
%% Internal record declarations
%% ---------------------------------------------------------------
-record(pman_shell,{win,
		    editor,
		    pid,
		    buffer,
		    father,
		    shell_flag,			% boolean, true for shell
		    trace_options,		% Keeps trace options
		    db}).                       % DB for trace windows


%%
%% Constants
%%

-define (PMAN_DB, pman_db).  % The pman db for trace windows



%% ---------------------------------------------------------------
%% start/1, start/2
%%
%% Starts a new trace shell process.
%%
%% start(Pid, DefaultOptions)
%% Pid			The Pid of the process to trace
%% DefaultOptions	The default trace options passed along from
%%			the calling process.
%%
%%
%% start(Pid)
%% Pid                   The Pid of the process to trace
%%
%% start(Pid) starts without using any default options except for those
%% hardwired into the application. (See pman_options.hrl).
%%
%%
%% Return: Both functions return a process id
%% ---------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_list/3 - Starts a trace window for each of the processes
%%    in the list

start_list(LIPid, Father, Options) ->
    StartFun = fun(Pid) ->
		       start({Pid,Father}, Options)
	       end,
    lists:foreach(StartFun, LIPid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start/1  - Starts a trace window for the specified Pid.
%%

start(Pid) ->
    start(Pid, #trace_options{}).

%%
%% start/2
%%

start(Pid,DefaultOptions) when is_pid(Pid) ->
    start({Pid,self()}, DefaultOptions);

start(Var,DefaultOptions) ->
    Db = db_start(),
    spawn_link(fun() -> internal(Var, DefaultOptions, Db) end).
    
%% ---------------------------------------------------------------
%% Initialize the enviroment for tracing/viewing Object
%%
%% Object can either be {shell,Shell} or a Pid.
%% The main loop is then called, which handles trace and event 
%% requests. The window dies whenever Supervisor dies, while
%% message windows die whenever their parent dies.
%% ---------------------------------------------------------------

internal({Object,Supervisor}, DefaultOptions, Db) ->

    %% (???) This call will cause minor problems when the window has been
    %% invoked with proc/1 from for instance the shell.  The shell
    %% does not handle the exit-signals, so it will exit
    %% when the window is exited.
    

    %% First check that no other process is tracing the process we want
    %% to trace. There is no well defined way of doing this, so the 
    %% code below is used instead. (???)

    pman_relay:start(Object),				%(???) Uses proc. dict.

    Pid = pman_process:get_pid(Object),

    case pman_relay:ok_to_trace(Pid) of

	%% Tracing cannot be performed on the specified process

	false ->
	    T = lists:flatten(io_lib:format("ERROR: Process ~p is already being~ntraced by some other process.~nOr there may be a problem communicating with it.",[Pid])),
	    tool_utils:notify(gs:start(),T),
	    exit(quit);

	%% Tracing can be performed, go ahead!

	true ->
	    
	    case db_insert_key (Db, Pid) of
		true ->
	    
		    link(Supervisor),
		    process_flag(trap_exit, true),

		    case catch pman_win:window(Object) of
			{'EXIT', badrpc} ->
			    T = "ERROR: Could not access node",
			    pman_win:dialog_window(gs:start(),T);
			{'EXIT', dead} ->
			    T = "ERROR: The process is dead",
			    pman_win:dialog_window(gs:start(),T);
			{'EXIT',_W} ->  
			    T = "ERROR: Untracable process \n(unexpected EXIT reason)",
			    pman_win:dialog_window(gs:start(),T);
			{Win, Ed} ->
			    init_monitor_loop(Win,
					      Ed,
					      Object,
					      Supervisor,
					      DefaultOptions,
					      Db)
		    end;
		
		false ->
		    T = lists:flatten(io_lib:format("ERROR: Process ~p is already being~ntraced by some other process.",[Pid])),
		    tool_utils:notify(gs:start(),T),
		    exit(quit);

		Error ->
		    Error
	    end
	    
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% init_monitor_loop/5 

init_monitor_loop(Win,Ed,Object,Supervisor, DefaultOptions, Db) ->

    process_flag(priority, max),
    
    %% Most default options come from the main window. Now we must set
    %% the default file name to something that is shows what process
    %% is being traced.

    %% Find out an appropriate file name to write the trace output
    %% to if the output should go to a file.

    FileName = case pman_process:is_pid_or_shell(Object) of
		   true ->
		       default_file_name(pman_process:get_pid(Object));
		   false ->
		       "NoName"
	       end,

    Buff = pman_buf:start(Ed, FileName),      

    case pman_process:is_running(Object) of

	%% We are tracing a shell process.
	{true,{shell,Pid}} ->
	    safe_link(Pid),
	    NewDefaultOptions =
		DefaultOptions#trace_options{file=FileName},
	    perform_option_changes(Pid, NewDefaultOptions, Buff),
	    monitor_loop(#pman_shell{win=Win, editor=Ed, pid=Pid, buffer=Buff,
				     father = Supervisor,
				     shell_flag = true,
				     trace_options = NewDefaultOptions,
				     db = Db});

	%% We are tracing an ordinary process.
	{true,Pid} ->
 	    safe_link(Pid),
	    NewDefaultOptions =
		DefaultOptions#trace_options{file=FileName},
	    perform_option_changes(Pid, NewDefaultOptions, Buff),
	    monitor_loop(#pman_shell{win=Win, editor=Ed, pid=Pid, buffer=Buff,
				     father = Supervisor,
				     shell_flag = false,
				     trace_options = NewDefaultOptions,
				     db = Db});

	%% The process being traced is dead.
	false ->
	    monitor_loop(#pman_shell{win=Win, editor=Ed, pid=nopid,
				     buffer=Buff,
				     father = Supervisor,
				     shell_flag = false,
				     trace_options= DefaultOptions,
				     db = Db})
    end.

%% ----------------------------------------------------------------
%% What is the Pid of the shell on our node?
%%  ----------------------------------------------------------------

find_shell() ->
    case shell:whereis_evaluator() of
	undefined ->				% noshell
	    noshell;
	Pid ->
	    Pid
    end.

%% ---------------------------------------------------------------
%% Functions called in case of an exit message 
%% ---------------------------------------------------------------

clean_up(Win, Buff,Pid) ->

    %% (???) Unlinks the traced process, but since we are using a safe link
    %% it is probably unnecessary.

    safe_unlink(Pid), 

    %% Kill helper processes

    exit(Buff#buffer.converter, topquit),
    exit(Buff#buffer.buffer, topquit),

    gs:destroy(Win).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exit_cmd/3 - Takes care of the necessary details when
%%    a linked process terminates.


exit_cmd(Pid,_Reason, State) ->
    case State#pman_shell.shell_flag of

	%% This clause handles the case when a shell process dies.
	%% Since it is restarted and the intention is to continue tracing
	%% the restarted shell process, we need to handle it separately by
	%% finding the new shell process.
	true ->
	    
	    NewShell = find_shell(),
	    safe_link(NewShell),
	    pman_relay:start(NewShell),

	    %% Update the window title with the new PID
	    Title = pman_win:title({shell, NewShell}),
	    Win = State#pman_shell.win,
	    gse:config(Win,[{title,Title}]),

	    pman_relay:trac(NewShell, true, flags()),

	    B = State#pman_shell.buffer,
	    B#buffer.converter!{raw,[{shell_died, Pid, NewShell}]},



	    State#pman_shell{pid=NewShell};

	%% This clause handles the case when a traced process that is
	%% not a shell process dies.
	false ->

	    B = State#pman_shell.buffer,
	    B#buffer.converter!{raw,[{died, Pid}]},

	    lists:foreach(fun(X) -> gse:disable(X) end,
			  ['Options',
			   'Kill',
			   'LinksMenu']),
	    State#pman_shell{pid=undefined}
    end.

flags() ->
    [send, 'receive', call, procs,
     set_on_spawn, set_on_first_spawn, set_on_link, set_on_first_link].

options_to_flaglists(Options) ->
    AssocList =
	[{Options#trace_options.send, send},
	 {Options#trace_options.treceive, 'receive'},
	 {Options#trace_options.inherit_on_1st_spawn, set_on_first_spawn},
	 {Options#trace_options.inherit_on_all_spawn, set_on_spawn},
	 {Options#trace_options.inherit_on_1st_link, set_on_first_link},
	 {Options#trace_options.inherit_on_all_link, set_on_link},
	 {Options#trace_options.events, procs},
	 {Options#trace_options.functions,call}],

    TrueFun = fun ({Option,Flag}) ->
		      case Option of
			  true -> Flag;
			  _Otherwise -> false
		      end
	      end,
    TrueFlags = mapfilter(TrueFun, AssocList),

    FalseFun = fun ({Option,Flag}) ->
		       case Option of
			   false -> Flag;
			   _Otherwise -> false
		       end
	       end,
    FalseFlags = mapfilter(FalseFun, AssocList),
    {TrueFlags,FalseFlags}.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% mapfilter/2 - Combines the functionality of lists:map and
%%   lists:filter. mapfilter applies the function argument to
%%   each element in the list. All returned values that are 
%%   not false will occur in the resulting list.
%%
%% Arguments:
%%   Fun	A fun that takes one argument
%%   List	A list. Each element will become an argument to Fun.
%%
%% Returns:
%%  A list of all results from the map operation that are not false.
%%

mapfilter(Fun,[E|Es]) ->
    case apply(Fun,[E]) of
	false ->
	    mapfilter(Fun,Es);
	Value -> [Value | mapfilter(Fun,Es)]
    end;
mapfilter(_Fun, []) -> [].
 

		      
perform_option_changes(Pid,Options,Buffer) ->

    %% Notify the trace output functionality
    %% if the destination is supposed to go to a file...

    case Options#trace_options.to_file of
	true -> 
	    FName = Options#trace_options.file,
	    Buffer#buffer.converter!{file,FName};
	false ->
	    done
    end,

    %%...then set the trace flags of the traced process

    {OnFlags, OffFlags} = options_to_flaglists(Options),
    case catch begin
		   
		   %% (???) Note that the following calls cannot actually fail
		   %% This may be a problem. And the catch appears unnecessary
		   %% However, it may become necessary to let the 
		   %% pman_relay:trac/3 function retrun appropriate values.
		   pman_relay:trac(Pid,true, OnFlags),
		   pman_relay:trac(Pid,false, OffFlags)
	       end of
	true ->
	     ok;
	_ -> pman_win:format("** Illegal trace request ** \n", [])
    end.






%% ---------------------------------------------------------------
%% Take care of the command executed by the user.

execute_cmd(Cmd,Shell_data) ->
    Window = Shell_data#pman_shell.win,
    Editor = Shell_data#pman_shell.editor,
    Shell = Shell_data#pman_shell.pid,
    Buffer = Shell_data#pman_shell.buffer,
    TraceOptions = Shell_data#pman_shell.trace_options,

    case Cmd of
	'Close' ->
	    db_delete_key (Shell_data#pman_shell.db, Shell_data#pman_shell.pid),
	    clean_up(Window, Buffer, Shell),
	    exit(quit);
	'Destroy' ->
	    db_delete_key (Shell_data#pman_shell.db, Shell_data#pman_shell.pid),
	    exit(Buffer#buffer.buffer,topquit),
	    safe_unlink(Shell),
	    exit(Buffer#buffer.converter,topquit),
            exit(Buffer#buffer.buffer,topquit),
	    exit(quit);

	'Clear' when is_pid(Shell) ->
	    New_buffer = pman_buf:clear(Buffer,pman_win:display(Shell),
				       TraceOptions#trace_options.file),
	    Shell_data#pman_shell{buffer = New_buffer};
	'Save buffer' ->
	    DefaultFile = "Pman_buffer." ++ default_file_name(Shell),
	    Result = tool_utils:file_dialog([{type,save},
					     {file,DefaultFile}]),
	    case Result of
		{ok, UserFile, _State} ->	    
		    Buffer#buffer.buffer!{save_buffer,UserFile};
		{error,_Reason} ->
		    true
	    end,
	    Shell_data;
	'Help' ->
	    HelpFile = filename:join([code:lib_dir(pman), "doc", "html", "index.html"]),
	    tool_utils:open_help(gs:start([{kernel, true}]), HelpFile),
	    Shell_data;
	'Kill' when is_pid(Shell)  ->		
	    exit(Buffer#buffer.converter,killed),
            exit(Buffer#buffer.buffer,killed),
	    lists:foreach(fun(X) -> gse:disable(X) end,
			  ['TraceMenu',
			   'Clear']),
	    catch exit(Shell, kill),
	    Shell_data#pman_shell{pid = undefined};
	'All Links' when is_pid(Shell)  ->
	    LIPid = pman_process:pinfo(Shell, links),
	    ?ALWAYS_ASSERT("Just a brutal test"),	    	    
	    start_list(LIPid,
		       Shell_data#pman_shell.father,
		       Shell_data#pman_shell.trace_options), 
	    Shell_data;
	'Module' when is_pid(Shell) ->
	    {ModuleName,_,_} = pman_process:function_info(Shell),
	    pman_module_info:start(ModuleName),
	    Shell_data;
	'Options' when is_pid(Shell) ->
	    case pman_options:dialog(Window,
                                     "Trace Options for Process",
                                     TraceOptions) of
		{error, _Reason} ->
		    Shell_data;
		Options ->
		    perform_option_changes(Shell, Options, Buffer),
		    Shell_data#pman_shell{trace_options=Options}
	    end;
	
	{trac,Choice,Bool} when is_pid(Shell) ->
	    pman_relay:trac(Shell, Bool, [Choice]),
	    Shell_data;


	{configure,{X,Y}} ->
	    configure (Editor, X, Y),
	    Shell_data;

	Pid when is_pid(Pid) ->
	    pman_shell:start({Pid, Shell_data#pman_shell.father},
			     Shell_data#pman_shell.trace_options),
	    Shell_data;
	_Other ->
	    ?ALWAYS_ASSERT("Received unexpected event"),
	    Shell_data
    end.
	    

default_file_name(Shell) when is_pid(Shell) ->
    [A,B,C] =  string:tokens(pid_to_list(Shell),[$.,$<,$>]),
    "pman_trace." ++ A ++ "_" ++ B ++ "_" ++ C;
default_file_name(_OTHER) ->
    "shell".





%% Key accellerators

key(e) -> 'Clear';
key(s) -> 'Save buffer';
key(c) -> 'Close';
key(a) -> 'All';
key(r) -> 'Reset';
key(m) -> 'Module';
key(l) -> 'All Links';
key(k) -> 'Kill';
key(h) -> 'Help';
key(z) -> 'Close';
key(O) -> O.



%% ---------------------------------------------------------------
%% The main loop takes care of data coming in from the traces, as 
%% well as exit signals from proceses we are monitoring. Events
%% caused by the user or window manager are also handled here.
%% ---------------------------------------------------------------


monitor_loop(Shell_data) ->
    receive

	%% WM destroy
	{gs,_Window,destroy,[],[]} ->  %%Avoid links menus
	    execute_cmd('Destroy', Shell_data);


	%% Handle EXIT signal from parent process
	{'EXIT', _Pid, topquit} ->
	    clean_up(Shell_data#pman_shell.win,
		     Shell_data#pman_shell.buffer,
		     Shell_data#pman_shell.pid),
	    exit(topquit);

	%% (???) Ignore "stray" EXIT signal from converter
	{'EXIT', _Pid, win_killed} ->
	    monitor_loop(Shell_data);


	%% Handle EXIT signal from safely linked Pid
	%% This is received when a traced process dies.
	{'SAFE_EXIT', Pid, Reason} ->
	    New_Shell_data = exit_cmd(Pid, Reason,Shell_data ),
	    monitor_loop(New_Shell_data);


	%% Handle EXIT signal from processes where we expect
	%% some EXIT signals, such as the file_dialog opened, and possibly
	%% others. 

	{'EXIT', _Pid, _Reason} ->
	    monitor_loop(Shell_data);

	%% Handle incoming trace messages
	Message when is_tuple(Message) , element(1,Message) == trace->
	    {L, Suspended} = collect_tracs([Message]),
	    Buffer = Shell_data#pman_shell.buffer,
	    Buffer#buffer.converter!{raw,L},
	    lists:foreach(fun(P) -> erlang:resume_process(P) end, Suspended),
	    monitor_loop(Shell_data);


	%% All other messages on the form {...,...,...}
	Message when is_tuple(Message) ->
	    do_link_stuff(Shell_data),
	    
	    New_Shell_data = process_gs_event(Message,Shell_data),
	    monitor_loop(New_Shell_data);

	%% Catch all for unexpected messages
	_Anything ->
	    ?ALWAYS_ASSERT("Received unexpected event"),	    
	    monitor_loop(Shell_data)
	  
    end.		
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_event/1 - Error handling wrapper for gs_cmd

process_gs_event(Message, Shell_data) ->
    case catch gs_cmd(Message,Shell_data) of

	%%
	%% Error exits from gs_cmd

	{'EXIT', badrpc} ->
	    Text = "\nERROR: Could not access node",
	    pman_win:msg_win(Text),
	    Shell_data;
	{'EXIT', dead} ->
	    Text = "\nERROR: The process is dead",
	    pman_win:msg_win(Text),
	    Shell_data;

	%% A controlled application initiated termination
	{'EXIT', quit} ->
	    db_delete_key (Shell_data#pman_shell.db, Shell_data#pman_shell.pid),
	    exit(quit);


	{'EXIT',Reason} ->
	    db_delete_key (Shell_data#pman_shell.db, Shell_data#pman_shell.pid),
	    io:format("Debug info, Reason: ~p~n",[Reason]),
	    ?ALWAYS_ASSERT("Unexpected EXIT reason"),
	    exit({unexpected_EXIT_reason,Reason});
	
	%%
	%% "Proper" exits from gs_cmd

	New_Shell_data ->
	    New_Shell_data
    end.



gs_cmd(Cmd, Shell_data) ->
      case Cmd of

	  %%User Command
	  {gs, Command, click, _Data, _Args}       ->
	      execute_cmd(Command,Shell_data);

	  %%Key accellerator
	  {gs,_Window,keypress,_D,[Key,_,0,1]}     ->
	      execute_cmd(key(Key),Shell_data);

	  %%Window Resize
	  {gs,_Window,configure,_,[X,Y|_]}        ->
	      execute_cmd({configure,{X,Y}},Shell_data);


	  {gs, _Object, _Event, _Data, _Args} ->
	      ?ALWAYS_ASSERT("Unhandled gs event"),
	      Shell_data
	  
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (???) do_link_stuff/1 - I have no clue.
%%

do_link_stuff(Shell_data) ->

    %% This appears to be code to execute for adding
    %% dynamic links menus.

    case Shell_data#pman_shell.pid of
	undefined ->
	    ok;
	Pid ->
	    case pman_process:pinfo(Pid, links) of
		Links when is_list(Links) ->
		    pman_win:links_menus(Links);
		undefined ->
		    ok
	    end
    end.


%% (???) Process dictionary used to safe Pid-Pid pairs.
%% 
%% safe_link/1 - Spawns a process, that links to the Pid, and sends
%%    a message to the caller when the linked process dies.
%%
%% Since we (think we) need to link to the traced process, we want
%% to do it in a way that has the smallest possible risk. The process
%% that links to the Pid is small and simple, which is safer than if
%% the calling process would link directly to the Pid.

safe_link(Pid) when is_pid(Pid) ->
    Self = self(),
    PidSafe = spawn_link(fun() -> safe_init(Self, Pid) end),
    put(Pid, PidSafe).
    

%% safe_unlink/1 - Removes a safe link
%%

safe_unlink(Pid) when is_pid(Pid) ->
    PidSafe = get(Pid),
    PidSafe ! {unlink, self(), Pid},
    erase(Pid);

safe_unlink(_Anything)->
    true.

%% safe_init/2 - Initialize a simple receive loop that controls safe linking
%%    to application processes.
%% 
safe_init(Caller, Pid) ->

    process_flag(trap_exit, true),
    link(Pid),

    safe_loop(Caller, Pid).


%% safe_loop/2 - Simply waits for an exit signal from the linked Pid,
%%   all other messages are disregarded.
%% 


safe_loop(Caller, Pid) ->
    receive
	%% Linked process dies
	{'EXIT' , Pid, Reason} ->
	    Caller ! {'SAFE_EXIT', Pid, Reason};

	%% Caller dies
	{'EXIT', Caller, _Reason} ->
	    unlink(Pid);
	

	%% Unlink request
	{unlink, Caller, Pid} ->
	    unlink(Pid);

	%% Ignore everything else
	_Anything ->
	    safe_loop(Caller, Pid)
    end.
	    
    

configure (Editor, W, H) ->
    gs:config (Editor, [{width, W - 3},
			{height, H - 40}]).
    



%%% The DB is used to avoid multiple trace windows 
%%% of the same process.

%%% db_start  /0
%%%

db_start() ->
    case ets:info(?PMAN_DB) of
	undefined -> ets:new(?PMAN_DB, [public, named_table]);
	_ -> ?PMAN_DB
    end.



%%% db_insert_key  /2
%%%

db_insert_key (Db, Pid) ->
    case ets:lookup (Db, Pid) of
	[] ->
	    case catch ets:insert (Db, {Pid}) of
		true ->
		    true;
		
		_Error ->
		    error_insert_db
	    end;

	_already_exists ->
	    false
    end.



%%% db_delete_key  /2
%%%

db_delete_key (Db, Pid) ->
    ets:delete (Db, Pid).


%% Function to collect all trace messages in the receive queue.
%% Returns: {Messages,SuspendedProcesses}

collect_tracs(Ack) -> collect_tracs(Ack, ordsets:new()).
    
collect_tracs(Ack, Procs) ->
    receive
	Trac when is_tuple(Trac), element(1, Trac) == trace ->
	    P = suspend(Trac, Procs),
	    collect_tracs([Trac | Ack], P)
    after 0 ->
	    {lists:reverse(Ack), ordsets:to_list(Procs)}
    end.

suspend({trace,From,call,_Func}, Suspended) when node(From) == node() ->
    case ordsets:is_element(From, Suspended) of
	true -> Suspended;
	false ->
	    case (catch erlang:suspend_process(From)) of
		true ->
		    ordsets:add_element(From, Suspended);
		_ ->
		    Suspended
	    end
    end;
suspend(_Other, Suspended) -> Suspended.
