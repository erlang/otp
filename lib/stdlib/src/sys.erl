%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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
-module(sys).

%% External exports
-export([suspend/1, suspend/2, resume/1, resume/2,
	 get_status/1, get_status/2,
	 get_state/1, get_state/2,
	 replace_state/2, replace_state/3,
	 change_code/4, change_code/5,
	 terminate/2, terminate/3,
	 log/2, log/3, trace/2, trace/3, statistics/2, statistics/3,
	 log_to_file/2, log_to_file/3, no_debug/1, no_debug/2,
	 install/2, install/3, remove/2, remove/3]).
-export([handle_system_msg/6, handle_system_msg/7, handle_debug/4,
	 print_log/1, get_log/1, get_debug/3, debug_options/1, suspend_loop_hib/6]).
-deprecated([{get_debug,3,eventually}]).

%%-----------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------

-export_type([dbg_opt/0, dbg_fun/0, debug_option/0]).

-type name()         :: pid() | atom()
                      | {'global', term()}
                      | {'via', module(), term()}.
-type system_event() :: {'in', Msg :: _}
                      | {'in', Msg :: _, State :: _}
                      | {'out', Msg :: _, To :: _}
                      | {'out', Msg :: _, To :: _, State :: _}
                      | {'noreply', State :: _}
                      | {'continue', Continuation :: _}
                      | {'code_change', Event :: _, State :: _}
                      | {'postpone', Event :: _, State :: _, NextState :: _}
                      | {'consume', Event :: _, State :: _, NextState :: _}
                      | {'start_timer', Action :: _, State :: _}
                      | {'insert_timeout', Event :: _, State :: _}
                      | {'enter', State :: _}
                      | {'terminate', Reason :: _, State :: _}
                      | term().
-opaque dbg_opt()    :: {'trace', 'true'}
                      | {'log',
                         {N :: non_neg_integer(),
                          [{Event :: system_event(),
                            FuncState :: _,
                            FormFunc :: format_fun()}]}}
                      | {'statistics', {file:date_time(),
                                        {'reductions', non_neg_integer()},
                                        MessagesIn :: non_neg_integer(),
                                        MessagesOut :: non_neg_integer()}}
                      | {'log_to_file', file:io_device()}
                      | {Func :: dbg_fun(), FuncState :: term()}
                      | {FuncId :: term(), Func :: dbg_fun(), FuncState :: term()}.
-type dbg_fun()      :: fun((FuncState :: _,
                             Event :: system_event(),
                             ProcState :: _) -> 'done' | (NewFuncState :: _)).

-type format_fun()   :: fun((Device :: io:device() | file:io_device(),
			     Event :: system_event(),
			     Extra :: term()) -> any()).

-type debug_option() ::
        'trace'
      | 'log'
      | {'log', N :: pos_integer()}
      | 'statistics'
      | {'log_to_file', FileName :: file:name()}
      | {'install',
         {Func :: dbg_fun(), FuncState :: term()}
         | {FuncId :: term(), Func :: dbg_fun(), FuncState :: term()}}.

%%-----------------------------------------------------------------
%% System messages
%%-----------------------------------------------------------------
-spec suspend(Name) -> 'ok' when
      Name :: name().
suspend(Name) -> send_system_msg(Name, suspend).

-spec suspend(Name, Timeout) -> 'ok' when
      Name :: name(),
      Timeout :: timeout().
suspend(Name, Timeout) -> send_system_msg(Name, suspend, Timeout).

-spec resume(Name) -> 'ok' when
      Name :: name().
resume(Name) -> send_system_msg(Name, resume).

-spec resume(Name, Timeout) -> 'ok' when
      Name :: name(),
      Timeout :: timeout().
resume(Name, Timeout) -> send_system_msg(Name, resume, Timeout).

-spec get_status(Name) -> Status when
      Name :: name(),
      Status :: {status, Pid :: pid(), {module, Module :: module()}, [SItem]},
      SItem :: (PDict :: [{Key :: term(), Value :: term()}])
             | (SysState :: 'running' | 'suspended')
             | (Parent :: pid())
             | (Dbg :: [dbg_opt()])
             | (Misc :: term()).
get_status(Name) -> send_system_msg(Name, get_status).

-spec get_status(Name, Timeout) -> Status when
      Name :: name(),
      Timeout :: timeout(),
      Status :: {status, Pid :: pid(), {module, Module :: module()}, [SItem]},
      SItem :: (PDict :: [{Key :: term(), Value :: term()}])
             | (SysState :: 'running' | 'suspended')
             | (Parent :: pid())
             | (Dbg :: [dbg_opt()])
             | (Misc :: term()).
get_status(Name, Timeout) -> send_system_msg(Name, get_status, Timeout).

-spec get_state(Name) -> State when
      Name :: name(),
      State :: term().
get_state(Name) ->
    case send_system_msg(Name, get_state) of
	{error, Reason} -> error(Reason);
	State -> State
    end.

-spec get_state(Name, Timeout) -> State when
      Name :: name(),
      Timeout :: timeout(),
      State :: term().
get_state(Name, Timeout) ->
    case send_system_msg(Name, get_state, Timeout) of
	{error, Reason} -> error(Reason);
	State -> State
    end.

-spec replace_state(Name, StateFun) -> NewState when
      Name :: name(),
      StateFun :: fun((State :: term()) -> NewState :: term()),
      NewState :: term().
replace_state(Name, StateFun) ->
    case send_system_msg(Name, {replace_state, StateFun}) of
	{error, Reason} -> error(Reason);
	State -> State
    end.

-spec replace_state(Name, StateFun, Timeout) -> NewState when
      Name :: name(),
      StateFun :: fun((State :: term()) -> NewState :: term()),
      Timeout :: timeout(),
      NewState :: term().
replace_state(Name, StateFun, Timeout) ->
    case send_system_msg(Name, {replace_state, StateFun}, Timeout) of
	{error, Reason} -> error(Reason);
	State -> State
    end.

-spec change_code(Name, Module, OldVsn, Extra) -> 'ok' | {error, Reason} when
      Name :: name(),
      Module :: module(),
      OldVsn :: 'undefined' | term(),
      Extra :: term(),
      Reason :: term().
change_code(Name, Mod, Vsn, Extra) ->
    send_system_msg(Name, {change_code, Mod, Vsn, Extra}).

-spec change_code(Name, Module, OldVsn, Extra, Timeout) ->
                         'ok' | {error, Reason} when
      Name :: name(),
      Module :: module(),
      OldVsn :: 'undefined' | term(),
      Extra :: term(),
      Timeout :: timeout(),
      Reason :: term().
change_code(Name, Mod, Vsn, Extra, Timeout) ->
    send_system_msg(Name, {change_code, Mod, Vsn, Extra}, Timeout).

-spec terminate(Name, Reason) -> 'ok' when
      Name :: name(),
      Reason :: term().
terminate(Name, Reason) ->
    send_system_msg(Name, {terminate, Reason}).

-spec terminate(Name, Reason, Timeout) -> 'ok' when
      Name :: name(),
      Reason :: term(),
      Timeout :: timeout().
terminate(Name, Reason, Timeout) ->
    send_system_msg(Name, {terminate, Reason}, Timeout).

%%-----------------------------------------------------------------
%% Debug commands
%%-----------------------------------------------------------------

-spec log(Name, Flag) -> 'ok' | {'ok', [system_event()]} when
      Name :: name(),
      Flag :: 'true' |
              {'true', N :: pos_integer()}
            | 'false' | 'get' | 'print'.
log(Name, Flag) ->
    send_system_msg(Name, {debug, {log, Flag}}).

-spec log(Name, Flag, Timeout) -> 'ok' | {'ok', [system_event()]} when
      Name :: name(),
      Flag :: 'true' |
              {'true', N :: pos_integer()}
            | 'false' | 'get' | 'print',
      Timeout :: timeout().
log(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {log, Flag}}, Timeout).

-spec trace(Name, Flag) -> 'ok' when
      Name :: name(),
      Flag :: boolean().
trace(Name, Flag) ->
    send_system_msg(Name, {debug, {trace, Flag}}).

-spec trace(Name, Flag, Timeout) -> 'ok' when
      Name :: name(),
      Flag :: boolean(),
      Timeout :: timeout().
trace(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {trace, Flag}}, Timeout).

-spec log_to_file(Name, Flag) -> 'ok' | {'error','open_file'} when
      Name :: name(),
      Flag :: (FileName :: string()) | 'false'.
log_to_file(Name, FileName) ->
    send_system_msg(Name, {debug, {log_to_file, FileName}}).

-spec log_to_file(Name, Flag, Timeout) -> 'ok' | {'error','open_file'} when
      Name :: name(),
      Flag :: (FileName :: string()) | 'false',
      Timeout :: timeout().
log_to_file(Name, FileName, Timeout) ->
    send_system_msg(Name, {debug, {log_to_file, FileName}}, Timeout).

-spec statistics(Name, Flag) -> 'ok' | {'ok', Statistics} when
      Name :: name(),
      Flag :: 'true' | 'false' | 'get',
      Statistics :: [StatisticsTuple] | no_statistics,
      StatisticsTuple :: {'start_time', DateTime1}
                       | {'current_time', DateTime2}
                       | {'reductions', non_neg_integer()}
                       | {'messages_in', non_neg_integer()}
                       | {'messages_out', non_neg_integer()},
      DateTime1 :: file:date_time(),
      DateTime2 :: file:date_time().
statistics(Name, Flag) ->
    send_system_msg(Name, {debug, {statistics, Flag}}).

-spec statistics(Name, Flag, Timeout) -> 'ok' | {'ok', Statistics} when
      Name :: name(),
      Flag :: 'true' | 'false' | 'get',
      Statistics :: [StatisticsTuple] | no_statistics,
      StatisticsTuple :: {'start_time', DateTime1}
                       | {'current_time', DateTime2}
                       | {'reductions', non_neg_integer()}
                       | {'messages_in', non_neg_integer()}
                       | {'messages_out', non_neg_integer()},
      DateTime1 :: file:date_time(),
      DateTime2 :: file:date_time(),
      Timeout :: timeout().
statistics(Name, Flag, Timeout) ->
    send_system_msg(Name, {debug, {statistics, Flag}}, Timeout).

-spec no_debug(Name) -> 'ok' when
      Name :: name().
no_debug(Name) -> send_system_msg(Name, {debug, no_debug}).

-spec no_debug(Name, Timeout) -> 'ok' when
      Name :: name(),
      Timeout :: timeout().
no_debug(Name, Timeout) -> send_system_msg(Name, {debug, no_debug}, Timeout).

-spec install(Name, FuncSpec) -> 'ok' when
      Name :: name(),
      FuncSpec :: {Func, FuncState} | {FuncId, Func, FuncState},
      FuncId :: term(),
      Func :: dbg_fun(),
      FuncState :: term().
install(Name, {Func, FuncState}) ->
    send_system_msg(Name, {debug, {install, {Func, FuncState}}});
install(Name, {FuncId, Func, FuncState}) ->
    send_system_msg(Name, {debug, {install, {FuncId, Func, FuncState}}}).

-spec install(Name, FuncSpec, Timeout) -> 'ok' when
      Name :: name(),
      FuncSpec :: {Func, FuncState} | {FuncId, Func, FuncState},
      FuncId :: term(),
      Func :: dbg_fun(),
      FuncState :: term(),
      Timeout :: timeout().
install(Name, {Func, FuncState}, Timeout) ->
    send_system_msg(Name, {debug, {install, {Func, FuncState}}}, Timeout);
install(Name, {FuncId, Func, FuncState}, Timeout) ->
    send_system_msg(Name, {debug, {install, {FuncId, Func, FuncState}}}, Timeout).

-spec remove(Name, Func | FuncId) -> 'ok' when
      Name :: name(),
      Func :: dbg_fun(),
      FuncId :: term().
remove(Name, FuncOrFuncId) ->
    send_system_msg(Name, {debug, {remove, FuncOrFuncId}}).

-spec remove(Name, Func | FuncId, Timeout) -> 'ok' when
      Name :: name(),
      Func :: dbg_fun(),
      FuncId :: term(),
      Timeout :: timeout().
remove(Name, FuncOrFuncId, Timeout) ->
    send_system_msg(Name, {debug, {remove, FuncOrFuncId}}, Timeout).

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
mfa(Name, {terminate, Reason}) ->
    {sys, terminate, [Name, Reason]};
mfa(Name, Atom) ->
    {sys, Atom, [Name]}.

mfa(Name, Req, Timeout) ->
    {M, F, A} = mfa(Name, Req),
    {M, F, A ++ [Timeout]}.

%%-----------------------------------------------------------------
%% Func: handle_system_msg/6
%% Purpose: Used by a process module that wishes to take care of
%%          system messages.  The process receives a {system, From,
%%          Msg} message, and passes the Msg to this function.
%% Returns: This function *never* returns! It calls the function
%%          Module:system_continue(Parent, NDebug, Misc)
%%          there the process continues the execution or
%%          Module:system_terminate(Reason, Parent, Debug, Misc) if
%%          the process should terminate.
%%          The Module must export system_continue/3, system_terminate/4
%%          and format_status/2 for status information.
%%-----------------------------------------------------------------
-spec handle_system_msg(Msg, From, Parent, Module, Debug, Misc) ->
                               no_return() when
      Msg :: term(),
      From :: {pid(), Tag :: _},
      Parent :: pid(),
      Module :: module(),
      Debug :: [dbg_opt()],
      Misc :: term().
handle_system_msg(Msg, From, Parent, Module, Debug, Misc) ->
    handle_system_msg(running, Msg, From, Parent, Module, Debug, Misc, false).

handle_system_msg(Msg, From, Parent, Mod, Debug, Misc, Hib) ->
   handle_system_msg(running, Msg, From, Parent, Mod, Debug, Misc, Hib).

handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib) ->
    case do_cmd(SysState, Msg, Parent, Mod, Debug, Misc) of
	{suspended, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
	    suspend_loop(suspended, Parent, Mod, NDebug, NMisc, Hib);
	{running, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
            Mod:system_continue(Parent, NDebug, NMisc);
	{{terminating, Reason}, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
	    Mod:system_terminate(Reason, Parent, NDebug, NMisc)
    end.

%%-----------------------------------------------------------------
%% Func: handle_debug/4
%% Purpose: Called by a process that wishes to debug an event.
%%          Func is a formatting function, called as Func(Device, Event).
%% Returns: [debug_opts()]
%%-----------------------------------------------------------------
-spec handle_debug(Debug, FormFunc, Extra, Event) -> [dbg_opt()] when
      Debug :: [dbg_opt()],
      FormFunc :: format_fun(),
      Extra :: term(),
      Event :: system_event().
handle_debug([{trace, true} = DbgOpt | T], FormFunc, State, Event) ->
    print_event({Event, State, FormFunc}),
    [DbgOpt | handle_debug(T, FormFunc, State, Event)];
handle_debug([{log, NLog} | T], FormFunc, State, Event) ->
    Item = {Event, State, FormFunc},
    [{log, nlog_put(Item, NLog)} | handle_debug(T, FormFunc, State, Event)];
handle_debug([{log_to_file, Fd} = DbgOpt | T], FormFunc, State, Event) ->
    print_event(Fd, {Event, State, FormFunc}),
    [DbgOpt | handle_debug(T, FormFunc, State, Event)];
handle_debug([{statistics, StatData} | T], FormFunc, State, Event) ->
    NStatData = stat(Event, StatData),
    [{statistics, NStatData} | handle_debug(T, FormFunc, State, Event)];
handle_debug([{FuncId, {Func, FuncState}} | T], FormFunc, State, Event) ->
    try Func(FuncState, Event, State) of
        done -> handle_debug(T, FormFunc, State, Event);
        NFuncState ->
            [{FuncId, {Func, NFuncState}} |
             handle_debug(T, FormFunc, State, Event)]
    catch
        done -> handle_debug(T, FormFunc, State, Event);
        NFuncState ->
            [{FuncId, {Func, NFuncState}} |
             handle_debug(T, FormFunc, State, Event)];
        _:_ -> handle_debug(T, FormFunc, State, Event)
    end;
handle_debug([{Func, FuncState} | T], FormFunc, State, Event) ->
    try Func(FuncState, Event, State) of
	done -> handle_debug(T, FormFunc, State, Event);
	NFuncState ->
	    [{Func, NFuncState} | handle_debug(T, FormFunc, State, Event)]
    catch
	done -> handle_debug(T, FormFunc, State, Event);
	NFuncState ->
	    [{Func, NFuncState} | handle_debug(T, FormFunc, State, Event)];
        _:_ -> handle_debug(T, FormFunc, State, Event)
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
do_cmd(SysState, get_state, _Parent, Mod, Debug, Misc) ->
    {SysState, do_get_state(Mod, Misc), Debug, Misc};
do_cmd(SysState, {replace_state, StateFun},  _Parent, Mod, Debug, Misc) ->
    {Res, NMisc} = do_replace_state(StateFun, Mod, Misc),
    {SysState, Res, Debug, NMisc};
do_cmd(SysState, get_status, Parent, Mod, Debug, Misc) ->
    Res = get_status(SysState, Parent, Mod, Debug, Misc),
    {SysState, Res, Debug, Misc};
do_cmd(SysState, {debug, What}, _Parent, _Mod, Debug, Misc) ->
    {Res, NDebug} = debug_cmd(What, Debug),
    {SysState, Res, NDebug, Misc};
do_cmd(_, {terminate, Reason}, _Parent, _Mod, Debug, Misc) ->
    {{terminating, Reason}, ok, Debug, Misc};
do_cmd(suspended, {change_code, Module, Vsn, Extra}, _Parent,
       Mod, Debug, Misc) ->
    {Res, NMisc} = do_change_code(Mod, Module, Vsn, Extra, Misc),
    {suspended, Res, Debug, NMisc};
do_cmd(SysState, Other, _Parent, _Mod, Debug, Misc) ->
    {SysState, {error, {unknown_system_msg, Other}}, Debug, Misc}.

do_get_state(Mod, Misc) ->
    case erlang:function_exported(Mod, system_get_state, 1) of
	true ->
	    try
		{ok, State} = Mod:system_get_state(Misc),
		State
	    catch
		Cl:Exc ->
		    {error, {callback_failed,{Mod,system_get_state},{Cl,Exc}}}
	    end;
	false ->
	    Misc
    end.

do_replace_state(StateFun, Mod, Misc) ->
    case erlang:function_exported(Mod, system_replace_state, 2) of
	true ->
	    try
		{ok, State, NMisc} = Mod:system_replace_state(StateFun, Misc),
		{State, NMisc}
	    catch
		Cl:Exc ->
		    {{error, {callback_failed,{Mod,system_replace_state},{Cl,Exc}}}, Misc}
	    end;
	false ->
	    try
		NMisc = StateFun(Misc),
		{NMisc, NMisc}
	    catch
		Cl:Exc ->
		    {{error, {callback_failed,StateFun,{Cl,Exc}}}, Misc}
	    end
    end.

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
    NLog = get_debug(log, Debug, nlog_new()),
    {ok, install_debug(log, nlog_new(NLog), Debug)};
debug_cmd({log, {true, N}}, Debug) when is_integer(N), 1 =< N ->
    NLog = get_debug(log, Debug, nlog_new(N)),
    {ok, install_debug(log, nlog_new(N, NLog), Debug)};
debug_cmd({log, false}, Debug) ->
    {ok, remove_debug(log, Debug)};
debug_cmd({log, print}, Debug) ->
    print_log(Debug),
    {ok, Debug};
debug_cmd({log, get}, Debug) ->
    NLog = get_debug(log, Debug, nlog_new()),
    {{ok, [Event || {Event, _State, _FormFunc} <- nlog_get(NLog)]}, Debug};
debug_cmd({log_to_file, false}, Debug) ->
    NDebug = close_log_file(Debug),
    {ok, NDebug};
debug_cmd({log_to_file, FileName}, Debug) ->
    NDebug = close_log_file(Debug),
    case file:open(FileName, [write,{encoding,utf8}]) of
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
debug_cmd({install, {FuncId, Func, FuncState}}, Debug) ->
    {ok, install_debug(FuncId, {Func, FuncState}, Debug)};
debug_cmd({remove, FuncOrFuncId}, Debug) ->
    {ok, remove_debug(FuncOrFuncId, Debug)};
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
stat({out, _Msg, _To, _State}, {Time, Reds, In, Out}) -> {Time, Reds, In, Out+1};
stat(_, StatData) -> StatData.

%%-----------------------------------------------------------------
%% Debug structure manipulating functions
%%-----------------------------------------------------------------
install_debug(Item, Data, Debug) ->
    case lists:keysearch(Item, 1, Debug) of
        false -> [{Item, Data} | Debug];
        _ -> Debug
    end.
remove_debug(Item, Debug) -> lists:keydelete(Item, 1, Debug).

-spec get_debug(Item, Debug, Default) -> term() when
      Item :: 'log' | 'statistics',
      Debug :: [dbg_opt()],
      Default :: term().
get_debug(Item, Debug, Default) -> 
    get_debug2(Item, Debug, Default).

%% Workaround: accepts more Item types than get_debug/3.
get_debug2(Item, Debug, Default) ->
    case lists:keysearch(Item, 1, Debug) of
	{value, {Item, Data}} -> Data;
	_ -> Default
    end.

-spec print_log(Debug) -> 'ok' when
      Debug :: [dbg_opt()].
print_log(Debug) ->
    NLog = get_debug(log, Debug, nlog_new()),
    lists:foreach(fun print_event/1, nlog_get(NLog)).

-spec get_log(Debug) -> [system_event()] when
      Debug :: [dbg_opt()].
get_log(Debug) ->
    NLog = get_debug(log, Debug, nlog_new()),
    [Event || {Event, _State, _FormFunc} <- nlog_get(NLog)].
    
close_log_file(Debug) ->
    case get_debug2(log_to_file, Debug, []) of
	[] ->
	    Debug;
	Fd -> 
	    ok = file:close(Fd),
	    remove_debug(log_to_file, Debug)
    end.

%%-----------------------------------------------------------------
%% Keep the last N Log functions
%%-----------------------------------------------------------------
%%
%% Streamlined Okasaki queue as base for "keep the last N" log.
%%
%% To the reverse list head we cons new items.
%% The forward list contains elements in insertion order,
%% so the head is the oldest and the one to drop off
%% when the log is full.
%%
%% Here is how we can get away with only using one cons cell
%% to wrap the forward and reverse list, and the log size:
%%
%% A full log does not need a counter; we just cons one
%% and drop one:
%%
%%     [ReverseList|ForwardList]
%%
%% A non-full log is filling up to N elements;
%% use a down counter instead of a list as first element:
%%
%%     [RemainingToFullCount|ReverseList]

nlog_new() ->
    nlog_new(10).
%%
nlog_new([_|_] = NLog) ->
    nlog_new(10, NLog);
nlog_new(N) ->
    [N]. % Empty log size N >= 1
%%
nlog_new(N, NLog) ->
    lists:foldl(
      fun (Item, NL) -> nlog_put(Item, NL) end,
      nlog_new(N),
      nlog_get(NLog)).

%%
nlog_put(Item, NLog) ->
    case NLog of
        [R|FF] when is_list(R) ->
            %% Full log
            case FF of
                [_|F] ->
                    %% Cons to reverse list, drop from forward list
                    [[Item|R]|F];
                [] ->
                    %% Create new forward list from reverse list,
                    %% create new empty reverse list
                    [_|F] = lists:reverse(R, [Item]),
                    [[]|F]
            end;
        [1|R] ->
            %% Log now gets full
            [[Item|R]];
        [J|R] ->
            %% Filling up to N elements
            [J - 1,Item|R]
    end.

nlog_get([[]|F]) ->
    F;
nlog_get([[_|_] = R|F]) ->
    F ++ lists:reverse(R);
nlog_get([_J|R]) ->
    lists:reverse(R).

%%-----------------------------------------------------------------
%% Func: debug_options/1
%% Purpose: Initiate a debug structure.  Called by a process that
%%          wishes to initiate the debug structure without the
%%          system messages.
%% Returns: [debug_opts()]
%%-----------------------------------------------------------------

-spec debug_options([Opt :: debug_option()]) -> [dbg_opt()].
debug_options(Options) ->
    debug_options(Options, []).

debug_options([trace | T], Debug) ->
    debug_options(T, install_debug(trace, true, Debug));
debug_options([log | T], Debug) ->
    debug_options(T, install_debug(log, nlog_new(), Debug));
debug_options([{log, N} | T], Debug) when is_integer(N), N > 0 ->
    debug_options(T, install_debug(log, nlog_new(N), Debug));
debug_options([statistics | T], Debug) ->
    debug_options(T, install_debug(statistics, init_stat(), Debug));
debug_options([{log_to_file, FileName} | T], Debug) ->
    case file:open(FileName, [write,{encoding,utf8}]) of
	{ok, Fd} ->
	    debug_options(T, install_debug(log_to_file, Fd, Debug));
	_Error ->
	    debug_options(T, Debug)
    end;
debug_options([{install, {Func, FuncState}} | T], Debug) ->
    debug_options(T, install_debug(Func, FuncState, Debug));
debug_options([{install, {FuncId, Func, FuncState}} | T], Debug) ->
    debug_options(T, install_debug(FuncId, {Func, FuncState}, Debug));
debug_options([_ | T], Debug) ->
    debug_options(T, Debug);
debug_options([], Debug) -> 
    Debug.
