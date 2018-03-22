%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2018. All Rights Reserved.
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
-module(gen_statem).

%% API
-export(
   [start/3,start/4,start_link/3,start_link/4,
    stop/1,stop/3,
    cast/2,call/2,call/3,
    enter_loop/4,enter_loop/5,enter_loop/6,
    reply/1,reply/2]).

%% gen callbacks
-export(
   [init_it/6]).

%% sys callbacks
-export(
   [system_continue/3,
    system_terminate/4,
    system_code_change/4,
    system_get_state/1,
    system_replace_state/2,
    format_status/2]).

%% Internal callbacks
-export(
   [wakeup_from_hibernate/3]).

%% Type exports for templates and callback modules
-export_type(
   [event_type/0,
    callback_mode_result/0,
    init_result/1,
    state_enter_result/1,
    event_handler_result/1,
    reply_action/0,
    enter_action/0,
    action/0]).
%% Old types, not advertised
-export_type(
   [state_function_result/0,
    handle_event_result/0]).

%% Type that is exported just to be documented
-export_type([transition_option/0]).

%%%==========================================================================
%%% Interface functions.
%%%==========================================================================

-type from() ::
	{To :: pid(), Tag :: term()}. % Reply-to specifier for call

-type state() ::
	state_name() | % For StateName/3 callback functions
	term(). % For handle_event/4 callback function

-type state_name() :: atom().

-type data() :: term().

-type event_type() ::
        external_event_type() | timeout_event_type() | 'internal'.
-type external_event_type() ::
        {'call',From :: from()} | 'cast' | 'info'.
-type timeout_event_type() ::
        'timeout' | {'timeout', Name :: term()} | 'state_timeout'.

-type callback_mode_result() ::
	callback_mode() | [callback_mode() | state_enter()].
-type callback_mode() :: 'state_functions' | 'handle_event_function'.
-type state_enter() :: 'state_enter'.

-type transition_option() ::
	postpone() | hibernate() |
	event_timeout() | generic_timeout() | state_timeout().
-type postpone() ::
	%% If 'true' postpone the current event
	%% and retry it when the state changes (=/=)
	boolean().
-type hibernate() ::
	%% If 'true' hibernate the server instead of going into receive
	boolean().
-type event_timeout() ::
	%% Generate a ('timeout', EventContent, ...) event
	%% unless some other event is delivered
	Time :: timeout() | integer().
-type generic_timeout() ::
	%% Generate a ({'timeout',Name}, EventContent, ...) event
	Time :: timeout() | integer().
-type state_timeout() ::
	%% Generate a ('state_timeout', EventContent, ...) event
	%% unless the state is changed
	Time :: timeout() | integer().
-type timeout_option() :: {abs,Abs :: boolean()}.

-type action() ::
	%% During a state change:
	%% * NextState and NewData are set.
	%% * All action()s are executed in order of apperance.
	%% * Postponing the current event is performed
	%%   iff 'postpone' is 'true'.
	%% * A state timeout is started iff 'timeout' is set.
	%% * Pending events are handled or if there are
	%%   no pending events the server goes into receive
	%%   or hibernate (iff 'hibernate' is 'true')
	%%
	%% These action()s are executed in order of appearence
	%% in the containing list. The ones that set options
	%% will override any previous so the last of each kind wins.
	%%
	'postpone' |  % Set the postpone option
	{'postpone', Postpone :: postpone()} |
	%%
	%% All 'next_event' events are kept in a list and then
	%% inserted at state changes so the first in the
	%% action() list is the first to be delivered.
	{'next_event', % Insert event as the next to handle
	 EventType :: event_type(),
	 EventContent :: term()} |
	enter_action().
-type enter_action() ::
	'hibernate' | % Set the hibernate option
	{'hibernate', Hibernate :: hibernate()} |
        timeout_action() |
	reply_action().
-type timeout_action() ::
	(Timeout :: event_timeout()) | % {timeout,Timeout}
	{'timeout', % Set the event_timeout option
	 Time :: event_timeout(), EventContent :: term()} |
	{'timeout', % Set the event_timeout option
	 Time :: event_timeout(),
	 EventContent :: term(),
	 Options :: (timeout_option() | [timeout_option()])} |
	%%
	{{'timeout', Name :: term()}, % Set the generic_timeout option
	 Time :: generic_timeout(), EventContent :: term()} |
	{{'timeout', Name :: term()}, % Set the generic_timeout option
	 Time :: generic_timeout(),
	 EventContent :: term(),
	 Options :: (timeout_option() | [timeout_option()])} |
	%%
	{'state_timeout', % Set the state_timeout option
	 Time :: state_timeout(), EventContent :: term()} |
	{'state_timeout', % Set the state_timeout option
	 Time :: state_timeout(),
	 EventContent :: term(),
	 Options :: (timeout_option() | [timeout_option()])}.
-type reply_action() ::
	{'reply', % Reply to a caller
	 From :: from(), Reply :: term()}.

-type init_result(StateType) ::
    {ok, State :: StateType, Data :: data()} |
    {ok, State :: StateType, Data :: data(),
     Actions :: [action()] | action()} |
    'ignore' |
    {'stop', Reason :: term()}.

%% Old, not advertised
-type state_function_result() ::
	event_handler_result(state_name()).
-type handle_event_result() ::
	event_handler_result(state()).
%%
-type state_enter_result(State) ::
	{'next_state', % {next_state,NextState,NewData,[]}
	 State,
	 NewData :: data()} |
	{'next_state', % State transition, maybe to the same state
	 State,
	 NewData :: data(),
	 Actions :: [enter_action()] | enter_action()} |
	state_callback_result(enter_action()).
-type event_handler_result(StateType) ::
	{'next_state', % {next_state,NextState,NewData,[]}
	 NextState :: StateType,
	 NewData :: data()} |
	{'next_state', % State transition, maybe to the same state
	 NextState :: StateType,
	 NewData :: data(),
	 Actions :: [action()] | action()} |
	state_callback_result(action()).
-type state_callback_result(ActionType) ::
	{'keep_state', % {keep_state,NewData,[]}
	 NewData :: data()} |
	{'keep_state', % Keep state, change data
	 NewData :: data(),
	 Actions :: [ActionType] | ActionType} |
	'keep_state_and_data' | % {keep_state_and_data,[]}
	{'keep_state_and_data', % Keep state and data -> only actions
	 Actions :: [ActionType] | ActionType} |
	%%
	{'repeat_state', % {repeat_state,NewData,[]}
	 NewData :: data()} |
	{'repeat_state', % Repeat state, change data
	 NewData :: data(),
	 Actions :: [ActionType] | ActionType} |
	'repeat_state_and_data' | % {repeat_state_and_data,[]}
	{'repeat_state_and_data', % Repeat state and data -> only actions
	 Actions :: [ActionType] | ActionType} |
	%%
	'stop' | % {stop,normal}
	{'stop', % Stop the server
	 Reason :: term()} |
	{'stop', % Stop the server
	 Reason :: term(),
	 NewData :: data()} |
	%%
	{'stop_and_reply', % Reply then stop the server
	 Reason :: term(),
	 Replies :: [reply_action()] | reply_action()} |
	{'stop_and_reply', % Reply then stop the server
	 Reason :: term(),
	 Replies :: [reply_action()] | reply_action(),
	 NewData :: data()}.


%% The state machine init function.  It is called only once and
%% the server is not running until this function has returned
%% an {ok, ...} tuple.  Thereafter the state callbacks are called
%% for all events to this server.
-callback init(Args :: term()) -> init_result(state()).

%% This callback shall return the callback mode of the callback module.
%%
%% It is called once after init/0 and code_change/4 but before
%% the first state callback StateName/3 or handle_event/4.
-callback callback_mode() -> callback_mode_result().

%% Example state callback for StateName = 'state_name'
%% when callback_mode() =:= state_functions.
%%
%% In this mode all states has to be of type state_name() i.e atom().
%%
%% Note that the only callbacks that have arity 3 are these
%% StateName/3 callbacks and terminate/3, so the state name
%% 'terminate' is unusable in this mode.
-callback state_name(
	    'enter',
	    OldStateName :: state_name(),
	    Data :: data()) ->
    state_enter_result('state_name');
           (event_type(),
	    EventContent :: term(),
	    Data :: data()) ->
    event_handler_result(state_name()).
%%
%% State callback for all states
%% when callback_mode() =:= handle_event_function.
-callback handle_event(
	    'enter',
	    OldState :: state(),
	    State, % Current state
	    Data :: data()) ->
    state_enter_result(State);
           (event_type(),
	    EventContent :: term(),
	    State :: state(), % Current state
	    Data :: data()) ->
    event_handler_result(state()).

%% Clean up before the server terminates.
-callback terminate(
	    Reason :: 'normal' | 'shutdown' | {'shutdown', term()}
		    | term(),
	    State :: state(),
	    Data :: data()) ->
    any().

%% Note that the new code can expect to get an OldState from
%% the old code version not only in code_change/4 but in the first
%% state callback function called thereafter
-callback code_change(
	    OldVsn :: term() | {'down', term()},
	    OldState :: state(),
	    OldData :: data(),
	    Extra :: term()) ->
    {ok, NewState :: state(), NewData :: data()} |
    (Reason :: term()).

%% Format the callback module state in some sensible that is
%% often condensed way.  For StatusOption =:= 'normal' the preferred
%% return term is [{data,[{"State",FormattedState}]}], and for
%% StatusOption =:= 'terminate' it is just FormattedState.
-callback format_status(
	    StatusOption,
	    [ [{Key :: term(), Value :: term()}] |
	      state() |
	      data()]) ->
    Status :: term() when
      StatusOption :: 'normal' | 'terminate'.

-optional_callbacks(
   [format_status/2, % Has got a default implementation
    terminate/3, % Has got a default implementation
    code_change/4, % Only needed by advanced soft upgrade
    %%
    state_name/3, % Example for callback_mode() =:= state_functions:
    %% there has to be a StateName/3 callback function
    %% for every StateName in your state machine but the state name
    %% 'state_name' does of course not have to be used.
    %%
    handle_event/4 % For callback_mode() =:= handle_event_function
   ]).



%% Type validation functions
-compile(
   {inline,
    [callback_mode/1, state_enter/1, from/1, event_type/1]}).
%%
callback_mode(CallbackMode) ->
    case CallbackMode of
	state_functions -> true;
	handle_event_function -> true;
	_ -> false
    end.
%%
state_enter(StateEnter) ->
    case StateEnter of
        state_enter ->
            true;
        _ ->
            false
    end.
%%
from({Pid,_}) when is_pid(Pid) -> true;
from(_) -> false.
%%
event_type({call,From}) ->
    from(From);
event_type(Type) ->
    case Type of
	{call,From} -> from(From);
	cast -> true;
	info -> true;
	timeout -> true;
	state_timeout -> true;
	internal -> true;
	{timeout,_} -> true;
	_ -> false
    end.



-define(
   STACKTRACE(),
   element(2, erlang:process_info(self(), current_stacktrace))).

-define(not_sys_debug, []).
%%
%% This is a macro to only evaluate arguments if Debug =/= [].
%% Debug is evaluated multiple times.
-define(
   sys_debug(Debug, NameState, Entry),
   case begin Debug end of
       ?not_sys_debug ->
           begin Debug end;
       _ ->
           sys_debug(begin Debug end, begin NameState end, begin Entry end)
    end).

-record(state,
        {callback_mode = undefined :: callback_mode() | undefined,
         state_enter = false :: boolean(),
         module :: atom(),
         name :: atom(),
         state :: term(),
         data :: term(),
         postponed = [] :: [{event_type(),term()}],
         %%
         timer_refs = #{} :: % timer ref => the timer's event type
           #{reference() => timeout_event_type()},
         timer_types = #{} ::  % timer's event type => timer ref
           #{timeout_event_type() => reference()},
         cancel_timers = 0 :: non_neg_integer(),
         %% We add a timer to both timer_refs and timer_types
         %% when we start it.  When we request an asynchronous
         %% timer cancel we remove it from timer_types.  When
         %% the timer cancel message arrives we remove it from
         %% timer_refs.
         %%
         hibernate = false :: boolean(),
         hibernate_after = infinity :: timeout()}).

-record(trans_opts,
        {hibernate = false,
         postpone = false,
         timeouts_r = [],
         next_events_r = []}).

%%%==========================================================================
%%% API

-type server_name() ::
      {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), Name :: term()}
      | {'local', atom()}.
-type server_ref() ::
      pid()
      | (LocalName :: atom())
      | {Name :: atom(), Node :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.
-type debug_opt() ::
	{'debug',
	 Dbgs ::
	   ['trace' | 'log' | 'statistics' | 'debug'
	    | {'logfile', string()}]}.
-type hibernate_after_opt() ::
	{'hibernate_after', HibernateAfterTimeout :: timeout()}.
-type start_opt() ::
	debug_opt()
      | {'timeout', Time :: timeout()}
	  | hibernate_after_opt()
      | {'spawn_opt', [proc_lib:spawn_option()]}.
-type start_ret() ::  {'ok', pid()} | 'ignore' | {'error', term()}.



%% Start a state machine
-spec start(
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start(Module, Args, Opts) ->
    gen:start(?MODULE, nolink, Module, Args, Opts).
%%
-spec start(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start(ServerName, Module, Args, Opts) ->
    gen:start(?MODULE, nolink, ServerName, Module, Args, Opts).

%% Start and link to a state machine
-spec start_link(
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start_link(Module, Args, Opts) ->
    gen:start(?MODULE, link, Module, Args, Opts).
%%
-spec start_link(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start_link(ServerName, Module, Args, Opts) ->
    gen:start(?MODULE, link, ServerName, Module, Args, Opts).

%% Stop a state machine
-spec stop(ServerRef :: server_ref()) -> ok.
stop(ServerRef) ->
    gen:stop(ServerRef).
%%
-spec stop(
	ServerRef :: server_ref(),
	Reason :: term(),
	Timeout :: timeout()) -> ok.
stop(ServerRef, Reason, Timeout) ->
    gen:stop(ServerRef, Reason, Timeout).

%% Send an event to a state machine that arrives with type 'event'
-spec cast(ServerRef :: server_ref(), Msg :: term()) -> ok.
cast(ServerRef, Msg) when is_pid(ServerRef) ->
    send(ServerRef, wrap_cast(Msg));
cast(ServerRef, Msg) when is_atom(ServerRef) ->
    send(ServerRef, wrap_cast(Msg));
cast({global,Name}, Msg) ->
    try	global:send(Name, wrap_cast(Msg)) of
	_ -> ok
    catch
	_:_ -> ok
    end;
cast({via,RegMod,Name}, Msg) ->
    try	RegMod:send(Name, wrap_cast(Msg)) of
	_ -> ok
    catch
	_:_ -> ok
    end;
cast({Name,Node} = ServerRef, Msg) when is_atom(Name), is_atom(Node) ->
    send(ServerRef, wrap_cast(Msg)).

%% Call a state machine (synchronous; a reply is expected) that
%% arrives with type {call,From}
-spec call(ServerRef :: server_ref(), Request :: term()) -> Reply :: term().
call(ServerRef, Request) ->
    call(ServerRef, Request, infinity).
%%
-spec call(
	ServerRef :: server_ref(),
	Request :: term(),
	Timeout ::
	  timeout() |
	  {'clean_timeout',T :: timeout()} |
	  {'dirty_timeout',T :: timeout()}) ->
		  Reply :: term().
call(ServerRef, Request, infinity = T = Timeout) ->
    call_dirty(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {dirty_timeout, T} = Timeout) ->
    call_dirty(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {clean_timeout, T} = Timeout) ->
    call_clean(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {_, _} = Timeout) ->
    erlang:error(badarg, [ServerRef,Request,Timeout]);
call(ServerRef, Request, Timeout) ->
    call_clean(ServerRef, Request, Timeout, Timeout).

%% Reply from a state machine callback to whom awaits in call/2
-spec reply([reply_action()] | reply_action()) -> ok.
reply({reply,From,Reply}) ->
    reply(From, Reply);
reply(Replies) when is_list(Replies) ->
    replies(Replies).
%%
-compile({inline, [reply/2]}).
-spec reply(From :: from(), Reply :: term()) -> ok.
reply({To,Tag}, Reply) when is_pid(To) ->
    Msg = {Tag,Reply},
    try To ! Msg of
	_ ->
	    ok
    catch
	_:_ -> ok
    end.

%% Instead of starting the state machine through start/3,4
%% or start_link/3,4 turn the current process presumably
%% started by proc_lib into a state machine using
%% the same arguments as you would have returned from init/1
-spec enter_loop(
	Module :: module(), Opts :: [debug_opt() | hibernate_after_opt()],
	State :: state(), Data :: data()) ->
			no_return().
enter_loop(Module, Opts, State, Data) ->
    enter_loop(Module, Opts, State, Data, self()).
%%
-spec enter_loop(
	Module :: module(), Opts :: [debug_opt() | hibernate_after_opt()],
	State :: state(), Data :: data(),
	Server_or_Actions ::
	  server_name() | pid() | [action()]) ->
			no_return().
enter_loop(Module, Opts, State, Data, Server_or_Actions) ->
    if
	is_list(Server_or_Actions) ->
	    enter_loop(Module, Opts, State, Data, self(), Server_or_Actions);
	true ->
	    enter_loop(Module, Opts, State, Data, Server_or_Actions, [])
    end.
%%
-spec enter_loop(
	Module :: module(), Opts :: [debug_opt() | hibernate_after_opt()],
	State :: state(), Data :: data(),
	Server :: server_name() | pid(),
	Actions :: [action()] | action()) ->
			no_return().
enter_loop(Module, Opts, State, Data, Server, Actions) ->
    is_atom(Module) orelse error({atom,Module}),
    Parent = gen:get_parent(),
    enter(Module, Opts, State, Data, Server, Actions, Parent).

%%---------------------------------------------------------------------------
%% API helpers

-compile({inline, [wrap_cast/1]}).
wrap_cast(Event) ->
    {'$gen_cast',Event}.

call_dirty(ServerRef, Request, Timeout, T) ->
    try gen:call(ServerRef, '$gen_call', Request, T) of
        {ok,Reply} ->
            Reply
    catch
        Class:Reason:Stacktrace ->
            erlang:raise(
              Class,
              {Reason,{?MODULE,call,[ServerRef,Request,Timeout]}},
              Stacktrace)
    end.

call_clean(ServerRef, Request, Timeout, T) ->
    %% Call server through proxy process to dodge any late reply
    Ref = make_ref(),
    Self = self(),
    Pid = spawn(
            fun () ->
                    Self !
                        try gen:call(
                              ServerRef, '$gen_call', Request, T) of
                            Result ->
                                {Ref,Result}
                        catch Class:Reason:Stacktrace ->
                                {Ref,Class,Reason,Stacktrace}
                        end
            end),
    Mref = monitor(process, Pid),
    receive
        {Ref,Result} ->
            demonitor(Mref, [flush]),
            case Result of
                {ok,Reply} ->
                    Reply
            end;
        {Ref,Class,Reason,Stacktrace} ->
            demonitor(Mref, [flush]),
            erlang:raise(
              Class,
              {Reason,{?MODULE,call,[ServerRef,Request,Timeout]}},
              Stacktrace);
        {'DOWN',Mref,_,_,Reason} ->
            %% There is a theoretical possibility that the
            %% proxy process gets killed between try--of and !
            %% so this clause is in case of that
            exit(Reason)
    end.

replies([{reply,From,Reply}|Replies]) ->
    reply(From, Reply),
    replies(Replies);
replies([]) ->
    ok.

%% Might actually not send the message in case of caught exception
send(Proc, Msg) ->
    try erlang:send(Proc, Msg)
    catch
        error:_ -> ok
    end,
    ok.

%% Here the init_it/6 and enter_loop/5,6,7 functions converge
enter(Module, Opts, State, Data, Server, Actions, Parent) ->
    %% The values should already have been type checked
    Name = gen:get_proc_name(Server),
    Debug = gen:debug_options(Name, Opts),
    HibernateAfterTimeout = gen:hibernate_after(Opts),
    Events = [],
    Event = {internal,init_state},
    %% We enforce {postpone,false} to ensure that
    %% our fake Event gets discarded, thought it might get logged
    NewActions = listify(Actions) ++ [{postpone,false}],
    S =
        #state{
           module = Module,
           name = Name,
           state = State,
           data = Data,
           hibernate_after = HibernateAfterTimeout},
    CallEnter = true,
    NewDebug = ?sys_debug(Debug, {Name,State}, {enter,Event,State}),
    case call_callback_mode(S) of
	#state{} = NewS ->
	    loop_event_actions_list(
	      Parent, NewDebug, NewS,
	      Events, Event, State, Data, false,
              NewActions, CallEnter);
	[Class,Reason,Stacktrace] ->
	    terminate(
	      Class, Reason, Stacktrace, NewDebug,
	      S, [Event|Events])
    end.

%%%==========================================================================
%%%  gen callbacks

init_it(Starter, self, ServerRef, Module, Args, Opts) ->
    init_it(Starter, self(), ServerRef, Module, Args, Opts);
init_it(Starter, Parent, ServerRef, Module, Args, Opts) ->
    try Module:init(Args) of
	Result ->
	    init_result(Starter, Parent, ServerRef, Module, Result, Opts)
    catch
	Result ->
	    init_result(Starter, Parent, ServerRef, Module, Result, Opts);
	Class:Reason:Stacktrace ->
	    Name = gen:get_proc_name(ServerRef),
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    error_info(
	      Class, Reason, Stacktrace,
	      #state{name = Name},
	      [], undefined),
	    erlang:raise(Class, Reason, Stacktrace)
    end.

%%---------------------------------------------------------------------------
%% gen callbacks helpers

init_result(Starter, Parent, ServerRef, Module, Result, Opts) ->
    case Result of
	{ok,State,Data} ->
	    proc_lib:init_ack(Starter, {ok,self()}),
	    enter(Module, Opts, State, Data, ServerRef, [], Parent);
	{ok,State,Data,Actions} ->
	    proc_lib:init_ack(Starter, {ok,self()}),
	    enter(Module, Opts, State, Data, ServerRef, Actions, Parent);
	{stop,Reason} ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    exit(Reason);
	ignore ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	_ ->
	    Name = gen:get_proc_name(ServerRef),
	    gen:unregister_name(ServerRef),
	    Error = {bad_return_from_init,Result},
	    proc_lib:init_ack(Starter, {error,Error}),
	    error_info(
	      error, Error, ?STACKTRACE(),
	      #state{name = Name},
	      [], undefined),
	    exit(Error)
    end.

%%%==========================================================================
%%% sys callbacks

system_continue(Parent, Debug, S) ->
    loop(Parent, Debug, S).

system_terminate(Reason, _Parent, Debug, S) ->
    terminate(exit, Reason, ?STACKTRACE(), Debug, S, []).

system_code_change(
  #state{
     module = Module,
     state = State,
     data = Data} = S,
  _Mod, OldVsn, Extra) ->
    case
	try Module:code_change(OldVsn, State, Data, Extra)
	catch
	    Result -> Result
	end
    of
	{ok,NewState,NewData} ->
	    {ok,
	     S#state{
               callback_mode = undefined,
               state = NewState,
               data = NewData}};
	{ok,_} = Error ->
	    error({case_clause,Error});
	Error ->
	    Error
    end.

system_get_state(#state{state = State, data = Data}) ->
    {ok,{State,Data}}.

system_replace_state(
  StateFun,
  #state{
     state = State,
     data = Data} = S) ->
    {NewState,NewData} = Result = StateFun({State,Data}),
    {ok,Result,S#state{state = NewState, data = NewData}}.

format_status(
  Opt,
  [PDict,SysState,Parent,Debug,
   #state{name = Name, postponed = P} = S]) ->
    Header = gen:format_status_header("Status for state machine", Name),
    Log = sys:get_debug(log, Debug, []),
    [{header,Header},
     {data,
      [{"Status",SysState},
       {"Parent",Parent},
       {"Logged Events",Log},
       {"Postponed",P}]} |
     case format_status(Opt, PDict, S) of
	 L when is_list(L) -> L;
	 T -> [T]
     end].

%%---------------------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%---------------------------------------------------------------------------

sys_debug(Debug, NameState, Entry) ->
  sys:handle_debug(Debug, fun print_event/3, NameState, Entry).

print_event(Dev, {in,Event}, {Name,State}) ->
    io:format(
      Dev, "*DBG* ~tp receive ~ts in state ~tp~n",
      [Name,event_string(Event),State]);
print_event(Dev, {out,Reply,{To,_Tag}}, {Name,State}) ->
    io:format(
      Dev, "*DBG* ~tp send ~tp to ~p from state ~tp~n",
      [Name,Reply,To,State]);
print_event(Dev, {terminate,Reason}, {Name,State}) ->
    io:format(
      Dev, "*DBG* ~tp terminate ~tp in state ~tp~n",
      [Name,Reason,State]);
print_event(Dev, {Tag,Event,NextState}, {Name,State}) ->
    StateString =
	case NextState of
	    State ->
		io_lib:format("~tp", [State]);
	    _ ->
		io_lib:format("~tp => ~tp", [State,NextState])
	end,
    io:format(
      Dev, "*DBG* ~tp ~tw ~ts in state ~ts~n",
      [Name,Tag,event_string(Event),StateString]).

event_string(Event) ->
    case Event of
	{{call,{Pid,_Tag}},Request} ->
	    io_lib:format("call ~tp from ~w", [Request,Pid]);
	{EventType,EventContent} ->
	    io_lib:format("~tw ~tp", [EventType,EventContent])
    end.

%%%==========================================================================
%%% Internal callbacks

wakeup_from_hibernate(Parent, Debug, S) ->
    %% It is a new message that woke us up so we have to receive it now
    loop_receive(Parent, Debug, S).

%%%==========================================================================
%%% State Machine engine implementation of proc_lib/gen server

%% Server loop, consists of all loop* functions
%% and detours through sys:handle_system_message/7 and proc_lib:hibernate/3

%% Entry point for system_continue/3
loop(Parent, Debug, #state{hibernate = true, cancel_timers = 0} = S) ->
    loop_hibernate(Parent, Debug, S);
loop(Parent, Debug, S) ->
    loop_receive(Parent, Debug, S).

loop_hibernate(Parent, Debug, S) ->
    %%
    %% Does not return but restarts process at
    %% wakeup_from_hibernate/3 that jumps to loop_receive/3
    %%
    proc_lib:hibernate(
      ?MODULE, wakeup_from_hibernate, [Parent,Debug,S]),
    error(
      {should_not_have_arrived_here_but_instead_in,
       {wakeup_from_hibernate,3}}).

%% Entry point for wakeup_from_hibernate/3
loop_receive(
  Parent, Debug, #state{hibernate_after = HibernateAfterTimeout} = S) ->
    %%
    receive
	Msg ->
	    case Msg of
		{system,Pid,Req} ->
		    %% Does not return but tail recursively calls
		    %% system_continue/3 that jumps to loop/3
		    sys:handle_system_msg(
		      Req, Pid, Parent, ?MODULE, Debug, S,
		      S#state.hibernate);
		{'EXIT',Parent,Reason} = EXIT ->
		    %% EXIT is not a 2-tuple therefore
		    %% not an event but this will stand out
		    %% in the crash report...
		    Q = [EXIT],
		    terminate(exit, Reason, ?STACKTRACE(), Debug, S, Q);
		{timeout,TimerRef,TimerMsg} ->
		    #state{
                       timer_refs = TimerRefs,
                       timer_types = TimerTypes} = S,
		    case TimerRefs of
			#{TimerRef := TimerType} ->
			    %% We know of this timer; is it a running
			    %% timer or a timer being cancelled that
			    %% managed to send a late timeout message?
			    case TimerTypes of
				#{TimerType := TimerRef} ->
				    %% The timer type maps back to this
				    %% timer ref, so it was a running timer
				    %% Unregister the triggered timeout
				    NewTimerRefs =
					maps:remove(TimerRef, TimerRefs),
				    NewTimerTypes =
					maps:remove(TimerType, TimerTypes),
				    loop_receive_result(
				      Parent, Debug,
				      S#state{
					timer_refs = NewTimerRefs,
					timer_types = NewTimerTypes},
				      TimerType, TimerMsg);
				_ ->
				    %% This was a late timeout message
				    %% from timer being cancelled, so
				    %% ignore it and expect a cancel_timer
				    %% msg shortly
				    loop_receive(Parent, Debug, S)
			    end;
			_ ->
			    %% Not our timer; present it as an event
			    loop_receive_result(Parent, Debug, S, info, Msg)
		    end;
		{cancel_timer,TimerRef,_} ->
		    #state{
                       timer_refs = TimerRefs,
                       cancel_timers = CancelTimers,
                       hibernate = Hibernate} = S,
		    case TimerRefs of
			#{TimerRef := _} ->
			    %% We must have requested a cancel
			    %% of this timer so it is already
			    %% removed from TimerTypes
			    NewTimerRefs =
				maps:remove(TimerRef, TimerRefs),
			    NewCancelTimers = CancelTimers - 1,
			    NewS =
				S#state{
				  timer_refs = NewTimerRefs,
				  cancel_timers = NewCancelTimers},
			    if
				Hibernate =:= true, NewCancelTimers =:= 0 ->
				    %% No more cancel_timer msgs to expect;
				    %% we can hibernate
				    loop_hibernate(Parent, Debug, NewS);
				NewCancelTimers >= 0 -> % Assert
				    loop_receive(Parent, Debug, NewS)
			    end;
			_ ->
			    %% Not our cancel_timer msg;
			    %% present it as an event
			    loop_receive_result(Parent, Debug, S, info, Msg)
		    end;
		_ ->
		    %% External msg
                    case Msg of
                        {'$gen_call',From,Request} ->
                            loop_receive_result(
                              Parent, Debug, S, {call,From}, Request);
                        {'$gen_cast',Cast} ->
                            loop_receive_result(Parent, Debug, S, cast, Cast);
                        _ ->
                            loop_receive_result(Parent, Debug, S, info, Msg)
                    end
	    end
    after
	    HibernateAfterTimeout ->
		    loop_hibernate(Parent, Debug, S)
    end.

loop_receive_result(Parent, ?not_sys_debug, S, Type, Content) ->
    %% Here is the queue of not yet handled events created
    Events = [],
    loop_event(Parent, ?not_sys_debug, S, Events, Type, Content);
loop_receive_result(
  Parent, Debug, #state{name = Name, state = State} = S, Type, Content) ->
    NewDebug = sys_debug(Debug, {Name,State}, {in,{Type,Content}}),
    %% Here is the queue of not yet handled events created
    Events = [],
    loop_event(Parent, NewDebug, S, Events, Type, Content).

%% Entry point for handling an event, received or enqueued
loop_event(
  Parent, Debug, #state{hibernate = Hibernate} = S,
  Events, Type, Content) ->
    %%
    case Hibernate of
        true ->
            %%
            %% If (this old) Hibernate is true here it can only be
            %% because it was set from an event action
            %% and we did not go into hibernation since there were
            %% events in queue, so we do what the user
            %% might rely on i.e collect garbage which
            %% would have happened if we actually hibernated
            %% and immediately was awakened.
            %%
            _ = garbage_collect(),
            loop_event_state_function(
              Parent, Debug, S, Events, Type, Content);
        false ->
            loop_event_state_function(
              Parent, Debug, S, Events, Type, Content)
    end.

%% Call the state function
loop_event_state_function(
  Parent, Debug,
  #state{state = State, data = Data} = S,
  Events, Type, Content) ->
    %%
    %% The field 'hibernate' in S is now invalid and will be
    %% restored when looping back to loop/3 or loop_event/6.
    %%
    Event = {Type,Content},
    TransOpts = false,
    case call_state_function(S, Type, Content, State, Data) of
        {Result, NewS} ->
            loop_event_result(
              Parent, Debug, NewS,
              Events, Event, State, Data, TransOpts, Result);
	[Class,Reason,Stacktrace] ->
	    terminate(
	      Class, Reason, Stacktrace, Debug, S, [Event|Events])
    end.

%% Make a state enter call to the state function
loop_event_state_enter(
  Parent, Debug, #state{state = PrevState} = S,
  Events, Event, NextState, NewData, TransOpts) ->
    %%
    case call_state_function(S, enter, PrevState, NextState, NewData) of
        {Result, NewS} ->
            loop_event_result(
              Parent, Debug, NewS,
              Events, Event, NextState, NewData, TransOpts, Result);
	[Class,Reason,Stacktrace] ->
	    terminate(
	      Class, Reason, Stacktrace, Debug, S, [Event|Events])
    end.

%% Process the result from the state function.
%% When TransOpts =:= false it was a state function call,
%% otherwise it is an option tuple and it was a state enter call.
%%
loop_event_result(
  Parent, Debug, S,
  Events, Event, State, Data, TransOpts, Result) ->
    %%
    case Result of
	{next_state,State,NewData} ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, NewData, TransOpts,
              [], false);
	{next_state,NextState,NewData}
          when TransOpts =:= false ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, NextState, NewData, TransOpts,
              [], true);
	{next_state,State,NewData,Actions} ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, NewData, TransOpts,
              Actions, false);
	{next_state,NextState,NewData,Actions}
          when TransOpts =:= false ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, NextState, NewData, TransOpts,
              Actions, true);
        %%
        {keep_state,NewData} ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, NewData, TransOpts,
              [], false);
        {keep_state,NewData,Actions} ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, NewData, TransOpts,
              Actions, false);
        %%
        keep_state_and_data ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, Data, TransOpts,
              [], false);
        {keep_state_and_data,Actions} ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, Data, TransOpts,
              Actions, false);
        %%
        {repeat_state,NewData} ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, NewData, TransOpts,
              [], true);
        {repeat_state,NewData,Actions} ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, NewData, TransOpts,
              Actions, true);
        %%
        repeat_state_and_data ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, Data, TransOpts,
              [], true);
        {repeat_state_and_data,Actions} ->
            loop_event_actions(
              Parent, Debug, S,
              Events, Event, State, Data, TransOpts,
              Actions, true);
        %%
	stop ->
            terminate(
              exit, normal, ?STACKTRACE(), Debug,
              S#state{
                state = State, data = Data,
                hibernate = hibernate_in_trans_opts(TransOpts)},
              [Event|Events]);
	{stop,Reason} ->
            terminate(
              exit, Reason, ?STACKTRACE(), Debug,
              S#state{
                state = State, data = Data,
                hibernate = hibernate_in_trans_opts(TransOpts)},
              [Event|Events]);
	{stop,Reason,NewData} ->
            terminate(
              exit, Reason, ?STACKTRACE(), Debug,
              S#state{
                state = State, data = NewData,
                hibernate = hibernate_in_trans_opts(TransOpts)},
              [Event|Events]);
	%%
	{stop_and_reply,Reason,Replies} ->
            reply_then_terminate(
              exit, Reason, ?STACKTRACE(), Debug,
              S#state{
                state = State, data = Data,
                hibernate = hibernate_in_trans_opts(TransOpts)},
              [Event|Events], Replies);
	{stop_and_reply,Reason,Replies,NewData} ->
            reply_then_terminate(
              exit, Reason, ?STACKTRACE(), Debug,
              S#state{
                state = State, data = NewData,
                hibernate = hibernate_in_trans_opts(TransOpts)},
              [Event|Events], Replies);
	%%
	_ ->
            terminate(
              error,
              {bad_return_from_state_function,Result},
              ?STACKTRACE(), Debug,
              S#state{
                state = State, data = Data,
                hibernate = hibernate_in_trans_opts(TransOpts)},
              [Event|Events])
    end.

-compile({inline, [hibernate_in_trans_opts/1]}).
hibernate_in_trans_opts(false) ->
    (#trans_opts{})#trans_opts.hibernate;
hibernate_in_trans_opts(#trans_opts{hibernate = Hibernate}) ->
    Hibernate.

%% Ensure that Actions are a list
loop_event_actions(
  Parent, Debug, S,
  Events, Event, NextState, NewerData, TransOpts,
  Actions, CallEnter) ->
    loop_event_actions_list(
      Parent, Debug, S,
      Events, Event, NextState, NewerData, TransOpts,
      listify(Actions), CallEnter).

%% Process actions from the state function
loop_event_actions_list(
  Parent, Debug, #state{state_enter = StateEnter} = S,
  Events, Event, NextState, NewerData, TransOpts,
  Actions, CallEnter) ->
    %%
    case parse_actions(TransOpts, Debug, S, Actions) of
        {NewDebug,NewTransOpts}
          when StateEnter, CallEnter ->
            loop_event_state_enter(
              Parent, NewDebug, S,
              Events, Event, NextState, NewerData, NewTransOpts);
        {NewDebug,NewTransOpts} ->
            loop_event_done(
              Parent, NewDebug, S,
              Events, Event, NextState, NewerData, NewTransOpts);
        [Class,Reason,Stacktrace,NewDebug] ->
            terminate(
              Class, Reason, Stacktrace, NewDebug,
              S#state{
                state = NextState,
                data = NewerData,
                hibernate = TransOpts#trans_opts.hibernate},
              [Event|Events])
    end.

parse_actions(false, Debug, S, Actions) ->
    parse_actions(true, Debug, S, Actions, #trans_opts{});
parse_actions(TransOpts, Debug, S, Actions) ->
    parse_actions(false, Debug, S, Actions, TransOpts).
%%
parse_actions(_StateCall, Debug, _S, [], TransOpts) ->
    {Debug,TransOpts};
parse_actions(StateCall, Debug, S, [Action|Actions], TransOpts) ->
    case Action of
	%% Actual actions
	{reply,From,Reply} ->
            parse_actions_reply(
              StateCall, Debug, S, Actions, TransOpts, From, Reply);
	%%
	%% Actions that set options
	{hibernate,NewHibernate} when is_boolean(NewHibernate) ->
            parse_actions(
              StateCall, Debug, S, Actions,
              TransOpts#trans_opts{hibernate = NewHibernate});
	hibernate ->
            parse_actions(
              StateCall, Debug, S, Actions,
              TransOpts#trans_opts{hibernate = true});
	%%
	{postpone,NewPostpone} when not NewPostpone orelse StateCall ->
            parse_actions(
              StateCall, Debug, S, Actions,
              TransOpts#trans_opts{postpone = NewPostpone});
	postpone when StateCall ->
            parse_actions(
              StateCall, Debug, S, Actions,
              TransOpts#trans_opts{postpone = true});
	%%
	{next_event,Type,Content} ->
            parse_actions_next_event(
              StateCall, Debug, S, Actions, TransOpts, Type, Content);
	%%
        _ ->
            parse_actions_timeout(
              StateCall, Debug, S, Actions, TransOpts, Action)
    end.

parse_actions_reply(
  StateCall, ?not_sys_debug, S, Actions, TransOpts,
  From, Reply) ->
    %%
    case from(From) of
        true ->
            reply(From, Reply),
            parse_actions(StateCall, ?not_sys_debug, S, Actions, TransOpts);
        false ->
            [error,
             {bad_action_from_state_function,{reply,From,Reply}},
             ?STACKTRACE(),
             ?not_sys_debug]
    end;
parse_actions_reply(
  StateCall, Debug, #state{name = Name, state = State} = S,
  Actions, TransOpts, From, Reply) ->
    %%
    case from(From) of
        true ->
            reply(From, Reply),
            NewDebug = sys_debug(Debug, {Name,State}, {out,Reply,From}),
            parse_actions(StateCall, NewDebug, S, Actions, TransOpts);
        false ->
            [error,
             {bad_action_from_state_function,{reply,From,Reply}},
             ?STACKTRACE(),
             Debug]
    end.

parse_actions_next_event(
  StateCall, ?not_sys_debug, S,
  Actions, TransOpts, Type, Content) ->
    case event_type(Type) of
        true when StateCall ->
            NextEventsR = TransOpts#trans_opts.next_events_r,
            parse_actions(
              StateCall, ?not_sys_debug, S, Actions,
              TransOpts#trans_opts{
                next_events_r = [{Type,Content}|NextEventsR]});
        _ ->
            [error,
             {bad_action_from_state_function,{next_event,Type,Content}},
             ?STACKTRACE(),
             ?not_sys_debug]
    end;
parse_actions_next_event(
  StateCall, Debug, #state{name = Name, state = State} = S,
  Actions, TransOpts, Type, Content) ->
    case event_type(Type) of
        true when StateCall ->
            NewDebug = sys_debug(Debug, {Name,State}, {in,{Type,Content}}),
            NextEventsR = TransOpts#trans_opts.next_events_r,
            parse_actions(
              StateCall, NewDebug, S, Actions,
              TransOpts#trans_opts{
                next_events_r = [{Type,Content}|NextEventsR]});
        _ ->
            [error,
             {bad_action_from_state_function,{next_event,Type,Content}},
             ?STACKTRACE(),
             Debug]
    end.

parse_actions_timeout(
  StateCall, Debug, S, Actions, TransOpts,
  {TimerType,Time,TimerMsg,TimerOpts} = AbsoluteTimeout) ->
    %%
    case classify_timer(Time, listify(TimerOpts)) of
        absolute ->
            parse_actions_timeout_add(
              StateCall, Debug, S, Actions,
              TransOpts, AbsoluteTimeout);
        relative ->
            RelativeTimeout = {TimerType,Time,TimerMsg},
            parse_actions_timeout_add(
              StateCall, Debug, S, Actions,
              TransOpts, RelativeTimeout);
        badarg ->
            [error,
             {bad_action_from_state_function,AbsoluteTimeout},
             ?STACKTRACE(),
             Debug]
    end;
parse_actions_timeout(
  StateCall, Debug, S, Actions, TransOpts,
  {_,Time,_} = RelativeTimeout) ->
    case classify_timer(Time, []) of
        relative ->
            parse_actions_timeout_add(
              StateCall, Debug, S, Actions,
              TransOpts, RelativeTimeout);
        badarg ->
            [error,
             {bad_action_from_state_function,RelativeTimeout},
             ?STACKTRACE(),
             Debug]
    end;
parse_actions_timeout(
  StateCall, Debug, S, Actions, TransOpts,
  Timeout) ->
    case classify_timer(Timeout, []) of
        relative ->
            parse_actions_timeout_add(
              StateCall, Debug, S, Actions, TransOpts, Timeout);
        badarg ->
            [error,
             {bad_action_from_state_function,Timeout},
             ?STACKTRACE(),
             Debug]
    end.

parse_actions_timeout_add(
  StateCall, Debug, S, Actions,
  #trans_opts{timeouts_r = TimeoutsR} = TransOpts, Timeout) ->
    parse_actions(
      StateCall, Debug, S, Actions,
      TransOpts#trans_opts{timeouts_r = [Timeout|TimeoutsR]}).

%% Do the state transition
loop_event_done(
  Parent, ?not_sys_debug,
  #state{postponed = P} = S,
  Events, Event, NextState, NewData,
  #trans_opts{
     postpone = Postpone, hibernate = Hibernate,
     timeouts_r = [], next_events_r = []}) ->
    %%
    %% Optimize the simple cases
    %% i.e no timer changes, no inserted events and no debug,
    %% by duplicate stripped down code
    %%
    %% Fast path
    %%
    case Postpone of
        true ->
            loop_event_done_fast(
              Parent, Hibernate,
              S,
              Events, [Event|P], NextState, NewData);
        false ->
            loop_event_done_fast(
              Parent, Hibernate,
              S,
              Events, P, NextState, NewData)
    end;
loop_event_done(
  Parent, Debug_0,
  #state{
     state = State, postponed = P_0,
     timer_refs = TimerRefs_0, timer_types = TimerTypes_0,
     cancel_timers = CancelTimers_0} = S,
  Events_0, Event_0, NextState, NewData,
  #trans_opts{
     hibernate = Hibernate, timeouts_r = TimeoutsR,
     postpone = Postpone, next_events_r = NextEventsR}) ->
    %%
    %% All options have been collected and next_events are buffered.
    %% Do the actual state transition.
    %%
    %% Full feature path
    %%
    [Debug_1|P_1] = % Move current event to postponed if Postpone
	case Postpone of
	    true ->
		[?sys_debug(
                    Debug_0,
                    {S#state.name,State},
                    {postpone,Event_0,State}),
		 Event_0|P_0];
	    false ->
		[?sys_debug(
                    Debug_0,
                    {S#state.name,State},
                    {consume,Event_0,State})|P_0]
	end,
    {Events_2,P_2,Timers_2} =
	%% Move all postponed events to queue,
        %% cancel the event timer,
        %% and cancel the state timeout if the state changes
	if
	    NextState =:= State ->
		{Events_0,P_1,
                 cancel_timer_by_type(
                   timeout, {TimerTypes_0,CancelTimers_0})};
	    true ->
		{lists:reverse(P_1, Events_0),
		 [],
		 cancel_timer_by_type(
		   state_timeout,
                   cancel_timer_by_type(
                     timeout, {TimerTypes_0,CancelTimers_0}))}
		    %% The state timer is removed from TimerTypes
		    %% but remains in TimerRefs until we get
		    %% the cancel_timer msg
	end,
    {TimerRefs_3,{TimerTypes_3,CancelTimers_3},TimeoutEvents} =
	%% Stop and start timers
	parse_timers(TimerRefs_0, Timers_2, TimeoutsR),
    %% Place next events last in reversed queue
    Events_3R = lists:reverse(Events_2, NextEventsR),
    %% Enqueue immediate timeout events
    Events_4R =	prepend_timeout_events(TimeoutEvents, Events_3R),
    loop_event_done(
      Parent, Debug_1,
      S#state{
        state = NextState,
        data = NewData,
        postponed = P_2,
        timer_refs = TimerRefs_3,
        timer_types = TimerTypes_3,
        cancel_timers = CancelTimers_3,
        hibernate = Hibernate},
      lists:reverse(Events_4R)).

%% Fast path
%%
loop_event_done_fast(
  Parent, Hibernate,
  #state{
     state = NextState,
     timer_types = #{timeout := _} = TimerTypes,
     cancel_timers = CancelTimers} = S,
  Events, P, NextState, NewData) ->
    %%
    %% Same state, event timeout active
    %%
    loop_event_done_fast(
      Parent, Hibernate, S,
      Events, P, NextState, NewData,
      cancel_timer_by_type(
        timeout, {TimerTypes,CancelTimers}));
loop_event_done_fast(
  Parent, Hibernate,
  #state{state = NextState} = S,
  Events, P, NextState, NewData) ->
    %%
    %% Same state
    %%
    loop_event_done(
      Parent, ?not_sys_debug,
      S#state{
        data = NewData,
        postponed = P,
        hibernate = Hibernate},
      Events);
loop_event_done_fast(
  Parent, Hibernate,
  #state{
     timer_types = #{timeout := _} = TimerTypes,
     cancel_timers = CancelTimers} = S,
  Events, P, NextState, NewData) ->
    %%
    %% State change, event timeout active
    %%
    loop_event_done_fast(
      Parent, Hibernate, S,
      lists:reverse(P, Events), [], NextState, NewData,
        cancel_timer_by_type(
          state_timeout,
          cancel_timer_by_type(
            timeout, {TimerTypes,CancelTimers})));
loop_event_done_fast(
  Parent, Hibernate,
  #state{
     timer_types = #{state_timeout := _} = TimerTypes,
     cancel_timers = CancelTimers} = S,
  Events, P, NextState, NewData) ->
    %%
    %% State change, state timeout active
    %%
    loop_event_done_fast(
      Parent, Hibernate, S,
      lists:reverse(P, Events), [], NextState, NewData,
        cancel_timer_by_type(
          state_timeout,
          cancel_timer_by_type(
            timeout, {TimerTypes,CancelTimers})));
loop_event_done_fast(
  Parent, Hibernate,
  #state{} = S,
  Events, P, NextState, NewData) ->
    %%
    %% State change, no timeout to automatically cancel
    %%
    loop_event_done(
      Parent, ?not_sys_debug,
      S#state{
        state = NextState,
        data = NewData,
        postponed = [],
        hibernate = Hibernate},
      lists:reverse(P, Events)).
%%
%% Fast path
%%
loop_event_done_fast(
  Parent, Hibernate, S,
  Events, P, NextState, NewData,
  {TimerTypes,CancelTimers}) ->
    %%
    loop_event_done(
      Parent, ?not_sys_debug,
      S#state{
        state = NextState,
        data = NewData,
        postponed = P,
        timer_types = TimerTypes,
        cancel_timers = CancelTimers,
        hibernate = Hibernate},
      Events).

loop_event_done(Parent, Debug, S, Q) ->
    case Q of
        [] ->
            %% Get a new event
            loop(Parent, Debug, S);
        [{Type,Content}|Events] ->
	    %% Loop until out of enqueued events
	    loop_event(Parent, Debug, S, Events, Type, Content)
    end.


%%---------------------------------------------------------------------------
%% Server loop helpers

call_callback_mode(#state{module = Module} = S) ->
    try Module:callback_mode() of
	CallbackMode ->
	    callback_mode_result(S, CallbackMode)
    catch
	CallbackMode ->
	    callback_mode_result(S, CallbackMode);
	Class:Reason:Stacktrace ->
	    [Class,Reason,Stacktrace]
    end.

callback_mode_result(S, CallbackMode) ->
    callback_mode_result(
      S, CallbackMode, listify(CallbackMode), undefined, false).
%%
callback_mode_result(_S, CallbackMode, [], undefined, _StateEnter) ->
    [error,
     {bad_return_from_callback_mode,CallbackMode},
     ?STACKTRACE()];
callback_mode_result(S, _CallbackMode, [], CBMode, StateEnter) ->
    S#state{callback_mode = CBMode, state_enter = StateEnter};
callback_mode_result(S, CallbackMode, [H|T], CBMode, StateEnter) ->
    case callback_mode(H) of
	true ->
            callback_mode_result(S, CallbackMode, T, H, StateEnter);
	false ->
	    case state_enter(H) of
		true ->
                    callback_mode_result(S, CallbackMode, T, CBMode, true);
		false ->
                    [error,
                     {bad_return_from_callback_mode,CallbackMode},
                     ?STACKTRACE()]
	    end
    end.


call_state_function(
  #state{callback_mode = undefined} = S, Type, Content, State, Data) ->
    case call_callback_mode(S) of
	#state{} = NewS ->
	    call_state_function(NewS, Type, Content, State, Data);
	Error ->
	    Error
    end;
call_state_function(
  #state{callback_mode = CallbackMode, module = Module} = S,
  Type, Content, State, Data) ->
    try
	case CallbackMode of
	    state_functions ->
		Module:State(Type, Content, Data);
	    handle_event_function ->
		Module:handle_event(Type, Content, State, Data)
	end
    of
	Result ->
	    {Result,S}
    catch
	Result ->
	    {Result,S};
	Class:Reason:Stacktrace ->
	    [Class,Reason,Stacktrace]
    end.


%% -> absolute | relative | badarg
classify_timer(Time, Opts) ->
    classify_timer(Time, Opts, false).
%%
classify_timer(Time, [], Abs) ->
    case Abs of
        true when
              is_integer(Time);
              Time =:= infinity ->
            absolute;
        false when
              is_integer(Time), 0 =< Time;
              Time =:= infinity ->
            relative;
        _ ->
            badarg
    end;
classify_timer(Time, [{abs,Abs}|Opts], _) when is_boolean(Abs) ->
    classify_timer(Time, Opts, Abs);
classify_timer(_, Opts, _) when is_list(Opts) ->
    badarg.

%% Stop and start timers as well as create timeout zero events
%% and pending event timer
%%
%% Stop and start timers non-event timers
parse_timers(TimerRefs, Timers, TimeoutsR) ->
    parse_timers(TimerRefs, Timers, TimeoutsR, #{}, []).
%%
parse_timers(
  TimerRefs, Timers, [], _Seen, TimeoutEvents) ->
    %%
    {TimerRefs,Timers,TimeoutEvents};
parse_timers(
  TimerRefs, Timers, [Timeout|TimeoutsR], Seen, TimeoutEvents) ->
    %%
    case Timeout of
	{TimerType,Time,TimerMsg,TimerOpts} ->
	    %% Absolute timer
	    parse_timers(
	      TimerRefs, Timers, TimeoutsR, Seen, TimeoutEvents,
	      TimerType, Time, TimerMsg, listify(TimerOpts));
	%% Relative timers below
	{TimerType,0,TimerMsg} ->
	    parse_timers(
	      TimerRefs, Timers, TimeoutsR, Seen, TimeoutEvents,
	      TimerType, zero, TimerMsg, []);
	{TimerType,Time,TimerMsg} ->
	    parse_timers(
	      TimerRefs, Timers, TimeoutsR, Seen, TimeoutEvents,
	      TimerType, Time, TimerMsg, []);
	0 ->
	    parse_timers(
	      TimerRefs, Timers, TimeoutsR, Seen, TimeoutEvents,
	      timeout, zero, 0, []);
	Time ->
	    parse_timers(
	      TimerRefs, Timers, TimeoutsR, Seen, TimeoutEvents,
	      timeout, Time, Time, [])
    end.

parse_timers(
  TimerRefs, Timers, TimeoutsR, Seen, TimeoutEvents,
  TimerType, Time, TimerMsg, TimerOpts) ->
    case Seen of
	#{TimerType := _} ->
	    %% Type seen before - ignore
	    parse_timers(
              TimerRefs, Timers, TimeoutsR, Seen, TimeoutEvents);
	#{} ->
	    %% Unseen type - handle
	    NewSeen = Seen#{TimerType => true},
	    case Time of
		infinity ->
		    %% Cancel any running timer
		    parse_timers(
		      TimerRefs, cancel_timer_by_type(TimerType, Timers),
                      TimeoutsR, NewSeen, TimeoutEvents);
		zero ->
		    %% Cancel any running timer
		    %% Handle zero time timeouts later
		    parse_timers(
		      TimerRefs, cancel_timer_by_type(TimerType, Timers),
                      TimeoutsR, NewSeen,
                      [{TimerType,TimerMsg}|TimeoutEvents]);
		_ ->
		    %% (Re)start the timer
		    TimerRef =
			erlang:start_timer(
			  Time, self(), TimerMsg, TimerOpts),
		    case Timers of
			{#{TimerType := OldTimerRef} = TimerTypes,
                         CancelTimers} ->
			    %% Cancel the running timer
			    cancel_timer(OldTimerRef),
			    NewCancelTimers = CancelTimers + 1,
			    %% Insert the new timer into
			    %% both TimerRefs and TimerTypes
			    parse_timers(
			      TimerRefs#{TimerRef => TimerType},
			      {TimerTypes#{TimerType => TimerRef},
                               NewCancelTimers},
                              TimeoutsR, NewSeen, TimeoutEvents);
			{#{} = TimerTypes,CancelTimers} ->
			    %% Insert the new timer into
			    %% both TimerRefs and TimerTypes
			    parse_timers(
			      TimerRefs#{TimerRef => TimerType},
			      {TimerTypes#{TimerType => TimerRef},
                               CancelTimers},
                              TimeoutsR, NewSeen, TimeoutEvents)
		    end
	    end
    end.

%% Enqueue immediate timeout events (timeout 0 events)
%%
%% Event timer timeout 0 events gets special treatment since
%% an event timer is cancelled by any received event,
%% so if there are enqueued events before the event timer
%% timeout 0 event - the event timer is cancelled hence no event.
%%
%% Other (state_timeout) timeout 0 events that are after
%% the event timer timeout 0 events are considered to
%% belong to timers that were started after the event timer
%% timeout 0 event fired, so they do not cancel the event timer.
%%
prepend_timeout_events([], EventsR) ->
    EventsR;
prepend_timeout_events([{timeout,_} = TimeoutEvent|TimeoutEvents], []) ->
    prepend_timeout_events(TimeoutEvents, [TimeoutEvent]);
prepend_timeout_events([{timeout,_}|TimeoutEvents], EventsR) ->
    %% Ignore since there are other events in queue
    %% so they have cancelled the event timeout 0.
    prepend_timeout_events(TimeoutEvents, EventsR);
prepend_timeout_events([TimeoutEvent|TimeoutEvents], EventsR) ->
    %% Just prepend all others
    prepend_timeout_events(TimeoutEvents, [TimeoutEvent|EventsR]).



%%---------------------------------------------------------------------------
%% Server helpers

reply_then_terminate(Class, Reason, Stacktrace, Debug, S, Q, Replies) ->
    do_reply_then_terminate(
      Class, Reason, Stacktrace, Debug, S, Q, listify(Replies)).
%%
do_reply_then_terminate(
  Class, Reason, Stacktrace, Debug, S, Q, []) ->
    terminate(Class, Reason, Stacktrace, Debug, S, Q);
do_reply_then_terminate(
  Class, Reason, Stacktrace, Debug, S, Q, [R|Rs]) ->
    case R of
	{reply,{_To,_Tag}=From,Reply} ->
            reply(From, Reply),
            NewDebug =
                ?sys_debug(
                   Debug,
                   begin
                       #state{name = Name, state = State} = S,
                       {Name,State}
                   end,
                   {out,Reply,From}),
	    do_reply_then_terminate(
	      Class, Reason, Stacktrace, NewDebug, S, Q, Rs);
	_ ->
	    terminate(
	      error,
	      {bad_reply_action_from_state_function,R},
	      ?STACKTRACE(),
	      Debug, S, Q)
    end.

terminate(
  Class, Reason, Stacktrace, Debug,
  #state{module = Module, state = State, data = Data} = S,
  Q) ->
    case erlang:function_exported(Module, terminate, 3) of
	true ->
	    try Module:terminate(Reason, State, Data) of
		_ -> ok
	    catch
		_ -> ok;
		C:R:ST ->
		    error_info(
		      C, R, ST, S, Q,
		      format_status(terminate, get(), S)),
		    sys:print_log(Debug),
		    erlang:raise(C, R, ST)
	    end;
	false ->
	    ok
    end,
    _ =
	case Reason of
	    normal ->
                terminate_sys_debug(Debug, S, State, Reason);
	    shutdown ->
                terminate_sys_debug(Debug, S, State, Reason);
	    {shutdown,_} ->
                terminate_sys_debug(Debug, S, State, Reason);
	    _ ->
		error_info(
		  Class, Reason, Stacktrace, S, Q,
		  format_status(terminate, get(), S)),
		sys:print_log(Debug)
	end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	_ ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

terminate_sys_debug(Debug, S, State, Reason) ->
    ?sys_debug(Debug, {S#state.name,State}, {terminate,Reason}).


error_info(
  Class, Reason, Stacktrace,
  #state{
     name = Name,
     callback_mode = CallbackMode,
     state_enter = StateEnter,
     postponed = P},
  Q, FmtData) ->
    {FixedReason,FixedStacktrace} =
	case Stacktrace of
	    [{M,F,Args,_}|ST]
	      when Class =:= error, Reason =:= undef ->
		case code:is_loaded(M) of
		    false ->
			{{'module could not be loaded',M},ST};
		    _ ->
			Arity =
			    if
				is_list(Args) ->
				    length(Args);
				is_integer(Args) ->
				    Args
			    end,
			case erlang:function_exported(M, F, Arity) of
			    true ->
				{Reason,Stacktrace};
			    false ->
				{{'function not exported',{M,F,Arity}},
				 ST}
			end
		end;
	    _ -> {Reason,Stacktrace}
	end,
    [LimitedP, LimitedFmtData, LimitedFixedReason] =
        [error_logger:limit_term(D) || D <- [P, FmtData, FixedReason]],
    CBMode =
	 case StateEnter of
	     true ->
		 [CallbackMode,state_enter];
	     false ->
		 CallbackMode
	 end,
    error_logger:format(
      "** State machine ~tp terminating~n" ++
	  case Q of
	      [] -> "";
	      _ -> "** Last event = ~tp~n"
	  end ++
	  "** When server state  = ~tp~n" ++
	  "** Reason for termination = ~w:~tp~n" ++
	  "** Callback mode = ~p~n" ++
	  case Q of
	      [_,_|_] -> "** Queued = ~tp~n";
	      _ -> ""
	  end ++
	  case P of
	      [] -> "";
	      _ -> "** Postponed = ~tp~n"
	  end ++
	  case FixedStacktrace of
	      [] -> "";
	      _ -> "** Stacktrace =~n**  ~tp~n"
	  end,
      [Name |
       case Q of
	   [] -> [];
	   [Event|_] -> [Event]
       end] ++
	  [LimitedFmtData,
	   Class,LimitedFixedReason,
	   CBMode] ++
	  case Q of
	      [_|[_|_] = Events] -> [Events];
	      _ -> []
	  end ++
	  case P of
	      [] -> [];
	      _ -> [LimitedP]
	  end ++
	  case FixedStacktrace of
	      [] -> [];
	      _ -> [FixedStacktrace]
	  end).


%% Call Module:format_status/2 or return a default value
format_status(
  Opt, PDict,
  #state{module = Module, state = State, data = Data}) ->
    case erlang:function_exported(Module, format_status, 2) of
	true ->
	    try Module:format_status(Opt, [PDict,State,Data])
	    catch
		Result -> Result;
		_:_ ->
		    format_status_default(
		      Opt, State,
		      atom_to_list(Module) ++ ":format_status/2 crashed")
	    end;
	false ->
	    format_status_default(Opt, State, Data)
    end.

%% The default Module:format_status/2
format_status_default(Opt, State, Data) ->
    StateData = {State,Data},
    case Opt of
	terminate ->
	    StateData;
	_ ->
	    [{data,[{"State",StateData}]}]
    end.

-compile({inline, [listify/1]}).
listify(Item) when is_list(Item) ->
    Item;
listify(Item) ->
    [Item].

%% Cancel timer if running, otherwise no op
%%
%% This is an asynchronous cancel so the timer is not really cancelled
%% until we get a cancel_timer msg i.e {cancel_timer,TimerRef,_}.
%% In the mean time we might get a timeout message.
%%
%% Remove the timer from TimerTypes.
%% When we get the cancel_timer msg we remove it from TimerRefs.
-compile({inline, [cancel_timer_by_type/2]}).
cancel_timer_by_type(TimerType, {TimerTypes,CancelTimers} = TT_CT) ->
    case TimerTypes of
	#{TimerType := TimerRef} ->
            ok = erlang:cancel_timer(TimerRef, [{async,true}]),
	    {maps:remove(TimerType, TimerTypes),CancelTimers + 1};
	#{} ->
	    TT_CT
    end.

-compile({inline, [cancel_timer/1]}).
cancel_timer(TimerRef) ->
    ok = erlang:cancel_timer(TimerRef, [{async,true}]).
