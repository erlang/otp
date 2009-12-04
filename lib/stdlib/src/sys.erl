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
-module(sys).

%% External exports
-export([suspend/1, suspend/2, resume/1, resume/2,
	 get_status/1, get_status/2,
	 change_code/4, change_code/5,
	 log/2, log/3, trace/2, trace/3, statistics/2, statistics/3,
	 log_to_file/2, log_to_file/3, no_debug/1, no_debug/2,
	 install/2, install/3, remove/2, remove/3]).
-export([handle_system_msg/6, handle_system_msg/7, handle_debug/4,
	 print_log/1, get_debug/3, debug_options/1, suspend_loop_hib/6]).

%%-----------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------

-type name()         :: pid() | atom() | {'global', atom()}.
-type system_event() :: {'in', _Msg} | {'in', _Msg, _From} | {'out', _Msg, _To}.

%%-----------------------------------------------------------------
%% System messages
%%-----------------------------------------------------------------
suspend(Name) -> send_system_msg(Name, suspend).
suspend(Name, Timeout) -> send_system_msg(Name, suspend, Timeout).

resume(Name) -> send_system_msg(Name, resume).
resume(Name, Timeout) -> send_system_msg(Name, resume, Timeout).

get_status(Name) -> send_system_msg(Name, get_status).
get_status(Name, Timeout) -> send_system_msg(Name, get_status, Timeout).

change_code(Name, Mod, Vsn, Extra) ->
    send_system_msg(Name, {change_code, Mod, Vsn, Extra}).
change_code(Name, Mod, Vsn, Extra, Timeout) ->
    send_system_msg(Name, {change_code, Mod, Vsn, Extra}, Timeout).

%%-----------------------------------------------------------------
%% Debug commands
%%-----------------------------------------------------------------

-type log_flag() :: 'true' | {'true',pos_integer()} | 'false' | 'get' | 'print'.

-spec log(name(), log_flag()) -> 'ok' | {'ok', [system_event()]}.
log(Name, Flag) ->
    send_system_msg(Name, {debug, {log, Flag}}).

-spec log(name(), log_flag(), timeout()) -> 'ok' | {'ok', [system_event()]}.
log(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {log, Flag}}, Timeout).

-spec trace(name(), boolean()) -> 'ok'.
trace(Name, Flag) ->
    send_system_msg(Name, {debug, {trace, Flag}}).

-spec trace(name(), boolean(), timeout()) -> 'ok'.
trace(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {trace, Flag}}, Timeout).

-type l2f_fname() :: string() | 'false'.

-spec log_to_file(name(), l2f_fname()) -> 'ok' | {'error','open_file'}.
log_to_file(Name, FileName) ->
    send_system_msg(Name, {debug, {log_to_file, FileName}}).

-spec log_to_file(name(), l2f_fname(), timeout()) -> 'ok' | {'error','open_file'}.
log_to_file(Name, FileName, Timeout) ->
    send_system_msg(Name, {debug, {log_to_file, FileName}}, Timeout).

statistics(Name, Flag) ->
    send_system_msg(Name, {debug, {statistics, Flag}}).
statistics(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {statistics, Flag}}, Timeout).

-spec no_debug(name()) -> 'ok'.
no_debug(Name) -> send_system_msg(Name, {debug, no_debug}).

-spec no_debug(name(), timeout()) -> 'ok'.
no_debug(Name, Timeout) -> send_system_msg(Name, {debug, no_debug}, Timeout).

install(Name, {Func, FuncState}) ->
    send_system_msg(Name, {debug, {install, {Func, FuncState}}}).
install(Name, {Func, FuncState}, Timeout) ->
    send_system_msg(Name, {debug, {install, {Func, FuncState}}}, Timeout).

remove(Name, Func) ->
    send_system_msg(Name, {debug, {remove, Func}}).
remove(Name, Func, Timeout) ->
    send_system_msg(Name, {debug, {remove, Func}}, Timeout).

%%-----------------------------------------------------------------
%% All system messages sent are on the form {system, From, Msg}
%% The receiving side should send Msg to handle_system_msg/5.
%%-----------------------------------------------------------------
send_system_msg(Name, Request) ->
    case catch gen:call(Name, system, Request) of
	{ok,Res} -> Res;
	{'EXIT', Reason} -> exit({Reason, mfa(Name, Request)})
    end.

send_system_msg(Name, Request, Timeout) ->
    case catch gen:call(Name, system, Request, Timeout) of
	{ok,Res} -> Res;
	{'EXIT', Reason} -> exit({Reason, mfa(Name, Request, Timeout)})
    end.

mfa(Name, {debug, {Func, Arg2}}) ->
    {sys, Func, [Name, Arg2]};
mfa(Name, {change_code, Mod, Vsn, Extra}) ->
    {sys, change_code, [Name, Mod, Vsn, Extra]};
mfa(Name, Atom) ->
    {sys, Atom, [Name]}.
mfa(Name, Req, Timeout) ->
    {M, F, A} = mfa(Name, Req),
    {M, F, A ++ [Timeout]}.

%%-----------------------------------------------------------------
%% Func: handle_system_msg/6
%% Args: Msg    ::= term()
%%       From   ::= {pid(),Ref} but don't count on that
%%       Parent ::= pid()
%%       Module ::= atom()
%%       Debug  ::= [debug_opts()]
%%       Misc   ::= term()
%% Purpose: Used by a process module that wishes to take care of
%%          system messages.  The process receives a {system, From,
%%          Msg} message, and passes the Msg to this function.
%% Returns: This function *never* returns! It calls the function
%%          Module:system_continue(Parent, NDebug, Misc)
%%          there the process continues the execution or
%%          Module:system_terminate(Raeson, Parent, Debug, Misc) if
%%          the process should terminate.
%%          The Module must export system_continue/3, system_terminate/4
%%          and format_status/2 for status information.
%%-----------------------------------------------------------------
handle_system_msg(Msg, From, Parent, Module, Debug, Misc) ->
    handle_system_msg(running, Msg, From, Parent, Module, Debug, Misc, false).

handle_system_msg(Msg, From, Parent, Mod, Debug, Misc, Hib) ->
   handle_system_msg(running, Msg, From, Parent, Mod, Debug, Misc, Hib).

handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib) ->
    case do_cmd(SysState, Msg, Parent, Mod, Debug, Misc) of
	{suspended, Reply, NDebug, NMisc} ->
	    gen:reply(From, Reply),
	    suspend_loop(suspended, Parent, Mod, NDebug, NMisc, Hib);
	{running, Reply, NDebug, NMisc} ->
	    gen:reply(From, Reply),
            Mod:system_continue(Parent, NDebug, NMisc)
    end.

%%-----------------------------------------------------------------
%% Func: handle_debug/4
%% Args: Debug ::= [debug_opts()]
%%       Func  ::= {M,F} | fun()  arity 3
%%       State ::= term()
%%       Event ::= {in, Msg} | {in, Msg, From} | {out, Msg, To} | term()
%% Purpose: Called by a process that wishes to debug an event.
%%          Func is a formatting function, called as Func(Device, Event).
%% Returns: [debug_opts()]
%%-----------------------------------------------------------------
handle_debug([{trace, true} | T], FormFunc, State, Event) ->
    print_event({Event, State, FormFunc}),
    [{trace, true} | handle_debug(T, FormFunc, State, Event)];
handle_debug([{log, {N, LogData}} | T], FormFunc, State, Event) ->
    NLogData = [{Event, State, FormFunc} | trim(N, LogData)],
    [{log, {N, NLogData}} | handle_debug(T, FormFunc, State, Event)];
handle_debug([{log_to_file, Fd} | T], FormFunc, State, Event) ->
    print_event(Fd, {Event, State, FormFunc}),
    [{log_to_file, Fd} | handle_debug(T, FormFunc, State, Event)];
handle_debug([{statistics, StatData} | T], FormFunc, State, Event) ->
    NStatData = stat(Event, StatData),
    [{statistics, NStatData} | handle_debug(T, FormFunc, State, Event)];
handle_debug([{Func, FuncState} | T], FormFunc, State, Event) ->
    case catch Func(FuncState, Event, State) of
	done -> handle_debug(T, FormFunc, State, Event);
	{'EXIT', _} -> handle_debug(T, FormFunc, State, Event);
	NFuncState ->		     
	    [{Func, NFuncState} | handle_debug(T, FormFunc, State, Event)]
    end;
handle_debug([], _FormFunc, _State, _Event) ->
    [].

%%-----------------------------------------------------------------
%% When a process is suspended, it can only respond to system
%% messages.
%%-----------------------------------------------------------------
suspend_loop(SysState, Parent, Mod, Debug, Misc, Hib) ->
    case Hib of
	true ->
	   suspend_loop_hib(SysState, Parent, Mod, Debug, Misc, Hib);
	_ ->
	    receive
		{system, From, Msg} ->
		    handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib);
		{'EXIT', Parent, Reason} ->
		    Mod:system_terminate(Reason, Parent, Debug, Misc)
	    end
    end.

suspend_loop_hib(SysState, Parent, Mod, Debug, Misc, Hib) ->
    receive
	{system, From, Msg} ->
	    handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib);
	{'EXIT', Parent, Reason} ->
            Mod:system_terminate(Reason, Parent, Debug, Misc)
    after 0 -> % Not a system message, go back into hibernation
	 proc_lib:hibernate(?MODULE, suspend_loop_hib, [SysState, Parent, Mod, 
							Debug, Misc, Hib])
    end.


do_cmd(_, suspend, _Parent, _Mod, Debug, Misc) ->
    {suspended, ok, Debug, Misc};
do_cmd(_, resume, _Parent, _Mod, Debug, Misc) ->
    {running, ok, Debug, Misc};
do_cmd(SysState, get_status, Parent, Mod, Debug, Misc) ->
    Res = get_status(SysState, Parent, Mod, Debug, Misc),
    {SysState, Res, Debug, Misc};
do_cmd(SysState, {debug, What}, _Parent, _Mod, Debug, Misc) ->
    {Res, NDebug} = debug_cmd(What, Debug),
    {SysState, Res, NDebug, Misc};
do_cmd(suspended, {change_code, Module, Vsn, Extra}, _Parent,
       Mod, Debug, Misc) ->
    {Res, NMisc} = do_change_code(Mod, Module, Vsn, Extra, Misc),
    {suspended, Res, Debug, NMisc};
do_cmd(SysState, Other, _Parent, _Mod, Debug, Misc) ->
    {SysState, {error, {unknown_system_msg, Other}}, Debug, Misc}.

get_status(SysState, Parent, Mod, Debug, Misc) ->
    PDict = get(),
    FmtMisc =
        case erlang:function_exported(Mod, format_status, 2) of
            true ->
                FmtArgs = [PDict, SysState, Parent, Debug, Misc],
                Mod:format_status(normal, FmtArgs);
            _ ->
                Misc
        end,
    {status, self(), {module, Mod},
     [PDict, SysState, Parent, Debug, FmtMisc]}.

%%-----------------------------------------------------------------
%% These are the system debug commands.
%% {trace,       true|false} -> io:format
%% {log,         true|false|get|print} -> keeps the 10 last debug messages
%% {log_to_file, FileName | false} -> io:format to file.
%% {statistics,  true|false|get}   -> keeps track of messages in/out + reds.
%%-----------------------------------------------------------------
debug_cmd({trace, true}, Debug) ->
    {ok, install_debug(trace, true, Debug)};
debug_cmd({trace, false}, Debug) ->
    {ok, remove_debug(trace, Debug)};
debug_cmd({log, true}, Debug) ->
    {_N, Logs} = get_debug(log, Debug, {0, []}),
    {ok, install_debug(log, {10, trim(10, Logs)}, Debug)};
debug_cmd({log, {true, N}}, Debug) when is_integer(N), N > 0 ->
    {_N, Logs} = get_debug(log, Debug, {0, []}),
    {ok, install_debug(log, {N, trim(N, Logs)}, Debug)};
debug_cmd({log, false}, Debug) ->
    {ok, remove_debug(log, Debug)};
debug_cmd({log, print}, Debug) ->
    print_log(Debug),
    {ok, Debug};
debug_cmd({log, get}, Debug) ->
    {_N, Logs} = get_debug(log, Debug, {0, []}),
    {{ok, lists:reverse(Logs)}, Debug};
debug_cmd({log_to_file, false}, Debug) ->
    NDebug = close_log_file(Debug),
    {ok, NDebug};
debug_cmd({log_to_file, FileName}, Debug) ->
    NDebug = close_log_file(Debug),
    case file:open(FileName, [write]) of
	{ok, Fd} ->
	    {ok, install_debug(log_to_file, Fd, NDebug)};
	_Error ->
	    {{error, open_file}, NDebug}
    end;
debug_cmd({statistics, true}, Debug) ->
    {ok, install_debug(statistics, init_stat(), Debug)};
debug_cmd({statistics, false}, Debug) ->
    {ok, remove_debug(statistics, Debug)};
debug_cmd({statistics, get}, Debug) ->
    {{ok, get_stat(get_debug(statistics, Debug, []))}, Debug};
debug_cmd(no_debug, Debug) ->
    close_log_file(Debug),
    {ok, []};
debug_cmd({install, {Func, FuncState}}, Debug) ->
    {ok, install_debug(Func, FuncState, Debug)};
debug_cmd({remove, Func}, Debug) ->
    {ok, remove_debug(Func, Debug)};
debug_cmd(_Unknown, Debug) ->
    {unknown_debug, Debug}.


do_change_code(Mod, Module, Vsn, Extra, Misc) ->
    case catch Mod:system_code_change(Misc, Module, Vsn, Extra) of
	{ok, NMisc} -> {ok, NMisc};
	Else -> {{error, Else}, Misc}
    end.

print_event(X) -> print_event(standard_io, X).

print_event(Dev, {Event, State, FormFunc}) ->
    FormFunc(Dev, Event, State).

init_stat() -> {erlang:localtime(), process_info(self(), reductions), 0, 0}.
get_stat({Time, {reductions, Reds}, In, Out}) ->
    {reductions, Reds2} = process_info(self(), reductions),
    [{start_time, Time}, {current_time, erlang:localtime()},
     {reductions, Reds2 - Reds}, {messages_in, In}, {messages_out, Out}];
get_stat(_) ->
    no_statistics.

stat({in, _Msg}, {Time, Reds, In, Out}) -> {Time, Reds, In+1, Out};
stat({in, _Msg, _From}, {Time, Reds, In, Out}) -> {Time, Reds, In+1, Out};
stat({out, _Msg, _To}, {Time, Reds, In, Out}) -> {Time, Reds, In, Out+1};
stat(_, StatData) -> StatData.

trim(N, LogData) ->
    lists:sublist(LogData, 1, N-1).

%%-----------------------------------------------------------------
%% Debug structure manipulating functions
%%-----------------------------------------------------------------
install_debug(Item, Data, Debug) ->
    case get_debug(Item, Debug, undefined) of
	undefined -> [{Item, Data} | Debug];
	_ -> Debug
    end.
remove_debug(Item, Debug) -> lists:keydelete(Item, 1, Debug).
get_debug(Item, Debug, Default) -> 
    case lists:keysearch(Item, 1, Debug) of
	{value, {Item, Data}} -> Data;
	_ -> Default
    end.

print_log(Debug) ->
    {_N, Logs} = get_debug(log, Debug, {0, []}),
    lists:foreach(fun print_event/1,
		  lists:reverse(Logs)).
    
close_log_file(Debug) ->
    case get_debug(log_to_file, Debug, []) of
	[] ->
	    Debug;
	Fd -> 
	    ok = file:close(Fd),
	    remove_debug(log_to_file, Debug)
    end.

%%-----------------------------------------------------------------
%% Func: debug_options/1
%% Args: [trace|log|{log,N}|statistics|{log_to_file, FileName}|
%%        {install, {Func, FuncState}}]
%% Purpose: Initiate a debug structure.  Called by a process that
%%          wishes to initiate the debug structure without the
%%          system messages.
%% Returns: [debug_opts()]
%%-----------------------------------------------------------------
debug_options(Options) ->
    debug_options(Options, []).
debug_options([trace | T], Debug) ->
    debug_options(T, install_debug(trace, true, Debug));
debug_options([log | T], Debug) ->
    debug_options(T, install_debug(log, {10, []}, Debug));
debug_options([{log, N} | T], Debug) when is_integer(N), N > 0 ->
    debug_options(T, install_debug(log, {N, []}, Debug));
debug_options([statistics | T], Debug) ->
    debug_options(T, install_debug(statistics, init_stat(), Debug));
debug_options([{log_to_file, FileName} | T], Debug) ->
    case file:open(FileName, [write]) of
	{ok, Fd} ->
	    debug_options(T, install_debug(log_to_file, Fd, Debug));
	_Error ->
	    debug_options(T, Debug)
    end;
debug_options([{install, {Func, FuncState}} | T], Debug) ->
    debug_options(T, install_debug(Func, FuncState, Debug));
debug_options([_ | T], Debug) ->
    debug_options(T, Debug);
debug_options([], Debug) -> 
    Debug.
