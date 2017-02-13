%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2017. All Rights Reserved.
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
	{'call',From :: from()} | 'cast' | 'info' |
	'timeout' | {'timeout', Name :: term()} | 'state_timeout' |
	'internal'.

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
	%%
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
	 Options :: (timeout_option() | [timeout_option()])} |
	%%
	reply_action().
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
%% often condensed way.  For StatusOption =:= 'normal' the perferred
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
callback_mode(CallbackMode) ->
    case CallbackMode of
	state_functions -> true;
	handle_event_function -> true;
	_ -> false
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
   try throw(ok) catch _ -> erlang:get_stacktrace() end).

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
-type start_opt() ::
	debug_opt()
      | {'timeout', Time :: timeout()}
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
    send(ServerRef, wrap_cast(Msg));
cast(ServerRef, Msg) when is_atom(ServerRef) ->
    send(ServerRef, wrap_cast(Msg));
cast(ServerRef, Msg) when is_pid(ServerRef) ->
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
call(ServerRef, Request, Timeout) ->
    case parse_timeout(Timeout) of
	{dirty_timeout,T} ->
	    try gen:call(ServerRef, '$gen_call', Request, T) of
		{ok,Reply} ->
		    Reply
	    catch
		Class:Reason ->
		    erlang:raise(
		      Class,
		      {Reason,{?MODULE,call,[ServerRef,Request,Timeout]}},
		      erlang:get_stacktrace())
	    end;
	{clean_timeout,T} ->
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
				catch Class:Reason ->
					{Ref,Class,Reason,
					 erlang:get_stacktrace()}
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
	    end;
	Error when is_atom(Error) ->
	    erlang:error(Error, [ServerRef,Request,Timeout])
    end.

parse_timeout(Timeout) ->
    case Timeout of
	{clean_timeout,infinity} ->
	    {dirty_timeout,infinity};
	{clean_timeout,_} ->
	    Timeout;
	{dirty_timeout,_} ->
	    Timeout;
	{_,_} ->
	    %% Be nice and throw a badarg for speling errors
	    badarg;
	infinity ->
	    {dirty_timeout,infinity};
	T ->
	    {clean_timeout,T}
    end.

%% Reply from a state machine callback to whom awaits in call/2
-spec reply([reply_action()] | reply_action()) -> ok.
reply({reply,From,Reply}) ->
    reply(From, Reply);
reply(Replies) when is_list(Replies) ->
    replies(Replies).
%%
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
	Module :: module(), Opts :: [debug_opt()],
	State :: state(), Data :: data()) ->
			no_return().
enter_loop(Module, Opts, State, Data) ->
    enter_loop(Module, Opts, State, Data, self()).
%%
-spec enter_loop(
	Module :: module(), Opts :: [debug_opt()],
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
	Module :: module(), Opts :: [debug_opt()],
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

wrap_cast(Event) ->
    {'$gen_cast',Event}.

replies([{reply,From,Reply}|Replies]) ->
    reply(From, Reply),
    replies(Replies);
replies([]) ->
    ok.

%% Might actually not send the message in case of caught exception
send(Proc, Msg) ->
    try erlang:send(Proc, Msg, [noconnect]) of
	noconnect ->
	    _ = spawn(erlang, send, [Proc,Msg]),
	    ok;
	ok ->
	    ok
    catch
	_:_ ->
	    ok
    end.

%% Here the init_it/6 and enter_loop/5,6,7 functions converge
enter(Module, Opts, State, Data, Server, Actions, Parent) ->
    %% The values should already have been type checked
    Name = gen:get_proc_name(Server),
    Debug = gen:debug_options(Name, Opts),
    Events = [],
    P = [],
    Event = {internal,init_state},
    %% We enforce {postpone,false} to ensure that
    %% our fake Event gets discarded, thought it might get logged
    NewActions =
	if
	    is_list(Actions) ->
		Actions ++ [{postpone,false}];
	    true ->
		[Actions,{postpone,false}]
	end,
    TimerRefs = #{},
    %% Key: timer ref
    %% Value: the timer type i.e the timer's event type
    %%
    TimerTypes = #{},
    %% Key: timer type i.e the timer's event type
    %% Value: timer ref
    %%
    %% We add a timer to both timer_refs and timer_types
    %% when we start it.  When we request an asynchronous
    %% timer cancel we remove it from timer_types.  When
    %% the timer cancel message arrives we remove it from
    %% timer_refs.
    %%
    Hibernate = false,
    CancelTimers = 0,
    S =	#{
      callback_mode => undefined,
      state_enter => false,
      module => Module,
      name => Name,
      state => State,
      data => Data,
      postponed => P,
      %%
      %% The following fields are finally set from to the arguments to
      %% loop_event_actions/9 when it finally loops back to loop/3
      %% in loop_event_result/11
      timer_refs => TimerRefs,
      timer_types => TimerTypes,
      hibernate => Hibernate,
      cancel_timers => CancelTimers
     },
    NewDebug = sys_debug(Debug, S, State, {enter,Event,State}),
    case call_callback_mode(S) of
	{ok,NewS} ->
	    loop_event_actions(
	      Parent, NewDebug, NewS,
	      Events, Event, State, Data, NewActions, true);
	{Class,Reason,Stacktrace} ->
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
	Class:Reason ->
	    Stacktrace = erlang:get_stacktrace(),
	    Name = gen:get_proc_name(ServerRef),
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    error_info(
	      Class, Reason, Stacktrace,
	      #{name => Name,
		callback_mode => undefined,
		state_enter => false},
	      [], [], undefined),
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
	      #{name => Name,
		callback_mode => undefined,
		state_enter => false},
	      [], [], undefined),
	    exit(Error)
    end.

%%%==========================================================================
%%% sys callbacks

system_continue(Parent, Debug, S) ->
    loop(Parent, Debug, S).

system_terminate(Reason, _Parent, Debug, S) ->
    terminate(exit, Reason, ?STACKTRACE(), Debug, S, []).

system_code_change(
  #{module := Module,
    state := State,
    data := Data} = S,
  _Mod, OldVsn, Extra) ->
    case
	try Module:code_change(OldVsn, State, Data, Extra)
	catch
	    Result -> Result
	end
    of
	{ok,NewState,NewData} ->
	    {ok,
	     S#{callback_mode := undefined,
		state := NewState,
		data := NewData}};
	{ok,_} = Error ->
	    error({case_clause,Error});
	Error ->
	    Error
    end.

system_get_state(#{state := State, data := Data}) ->
    {ok,{State,Data}}.

system_replace_state(
  StateFun,
  #{state := State,
    data := Data} = S) ->
    {NewState,NewData} = Result = StateFun({State,Data}),
    {ok,Result,S#{state := NewState, data := NewData}}.

format_status(
  Opt,
  [PDict,SysState,Parent,Debug,
   #{name := Name, postponed := P} = S]) ->
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

print_event(Dev, {in,Event}, {Name,State}) ->
    io:format(
      Dev, "*DBG* ~p receive ~s in state ~p~n",
      [Name,event_string(Event),State]);
print_event(Dev, {out,Reply,{To,_Tag}}, {Name,State}) ->
    io:format(
      Dev, "*DBG* ~p send ~p to ~p from state ~p~n",
      [Name,Reply,To,State]);
print_event(Dev, {terminate,Reason}, {Name,State}) ->
    io:format(
      Dev, "*DBG* ~p terminate ~p in state ~p~n",
      [Name,Reason,State]);
print_event(Dev, {Tag,Event,NextState}, {Name,State}) ->
    StateString =
	case NextState of
	    State ->
		io_lib:format("~p", [State]);
	    _ ->
		io_lib:format("~p => ~p", [State,NextState])
	end,
    io:format(
      Dev, "*DBG* ~p ~w ~s in state ~s~n",
      [Name,Tag,event_string(Event),StateString]).

event_string(Event) ->
    case Event of
	{{call,{Pid,_Tag}},Request} ->
	    io_lib:format("call ~p from ~w", [Request,Pid]);
	{EventType,EventContent} ->
	    io_lib:format("~w ~p", [EventType,EventContent])
    end.

sys_debug(Debug, #{name := Name}, State, Entry) ->
    case Debug of
	[] ->
	    Debug;
	_ ->
	    sys:handle_debug(
	      Debug, fun print_event/3, {Name,State}, Entry)
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
loop(Parent, Debug, #{hibernate := true, cancel_timers := 0} = S) ->
    loop_hibernate(Parent, Debug, S);
loop(Parent, Debug, S) ->
    loop_receive(Parent, Debug, S).

loop_hibernate(Parent, Debug, S) ->
    %% Does not return but restarts process at
    %% wakeup_from_hibernate/3 that jumps to loop_receive/3
    proc_lib:hibernate(
      ?MODULE, wakeup_from_hibernate, [Parent,Debug,S]),
    error(
      {should_not_have_arrived_here_but_instead_in,
       {wakeup_from_hibernate,3}}).

%% Entry point for wakeup_from_hibernate/3
loop_receive(Parent, Debug, S) ->
    receive
	Msg ->
	    case Msg of
		{system,Pid,Req} ->
		    #{hibernate := Hibernate} = S,
		    %% Does not return but tail recursively calls
		    %% system_continue/3 that jumps to loop/3
		    sys:handle_system_msg(
		      Req, Pid, Parent, ?MODULE, Debug, S,
		      Hibernate);
		{'EXIT',Parent,Reason} = EXIT ->
		    %% EXIT is not a 2-tuple therefore
		    %% not an event but this will stand out
		    %% in the crash report...
		    Q = [EXIT],
		    terminate(exit, Reason, ?STACKTRACE(), Debug, S, Q);
		{timeout,TimerRef,TimerMsg} ->
		    #{timer_refs := TimerRefs,
		      timer_types := TimerTypes,
		      hibernate := Hibernate} = S,
		    case TimerRefs of
			#{TimerRef := TimerType} ->
			    %% We know of this timer; is it a running
			    %% timer or a timer being cancelled that
			    %% managed to send a late timeout message?
			    case TimerTypes of
				#{TimerType := TimerRef} ->
				    %% The timer type maps back to this
				    %% timer ref, so it was a running timer
				    Event = {TimerType,TimerMsg},
				    %% Unregister the triggered timeout
				    NewTimerRefs =
					maps:remove(TimerRef, TimerRefs),
				    NewTimerTypes =
					maps:remove(TimerType, TimerTypes),
				    loop_receive_result(
				      Parent, Debug,
				      S#{
					timer_refs := NewTimerRefs,
					timer_types := NewTimerTypes},
				      Hibernate,
				      Event);
				_ ->
				    %% This was a late timeout message
				    %% from timer being cancelled, so
				    %% ignore it and expect a cancel_timer
				    %% msg shortly
				    loop_receive(Parent, Debug, S)
			    end;
			_ ->
			    %% Not our timer; present it as an event
			    Event = {info,Msg},
			    loop_receive_result(
			      Parent, Debug, S, Hibernate, Event)
		    end;
		{cancel_timer,TimerRef,_} ->
		    #{timer_refs := TimerRefs,
		      cancel_timers := CancelTimers,
		      hibernate := Hibernate} = S,
		    case TimerRefs of
			#{TimerRef := _} ->
			    %% We must have requested a cancel
			    %% of this timer so it is already
			    %% removed from TimerTypes
			    NewTimerRefs =
				maps:remove(TimerRef, TimerRefs),
			    NewCancelTimers = CancelTimers - 1,
			    NewS =
				S#{
				  timer_refs := NewTimerRefs,
				  cancel_timers := NewCancelTimers},
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
			    Event = {info,Msg},
			    loop_receive_result(
			      Parent, Debug, S, Hibernate, Event)
		    end;
		_ ->
		    %% External msg
		    #{hibernate := Hibernate} = S,
		    Event =
			case Msg of
			    {'$gen_call',From,Request} ->
				{{call,From},Request};
			    {'$gen_cast',E} ->
				{cast,E};
			    _ ->
				{info,Msg}
			end,
		    loop_receive_result(
		      Parent, Debug, S, Hibernate, Event)
	    end
    end.

loop_receive_result(
  Parent, Debug,
  #{state := State,
    timer_types := TimerTypes, cancel_timers := CancelTimers} = S,
  Hibernate, Event) ->
    %% From now the 'hibernate' field in S is invalid
    %% and will be restored when looping back
    %% in loop_event_result/11
    NewDebug = sys_debug(Debug, S, State, {in,Event}),
    %% Here is the queue of not yet handled events created
    Events = [],
    %% Cancel any running event timer
    case
	cancel_timer_by_type(timeout, TimerTypes, CancelTimers)
    of
	{_,CancelTimers} ->
	    %% No timer cancelled
	    loop_event(Parent, NewDebug, S, Events, Event, Hibernate);
	{NewTimerTypes,NewCancelTimers} ->
	    %% The timer is removed from NewTimerTypes but
	    %% remains in TimerRefs until we get
	    %% the cancel_timer msg
	    NewS =
		S#{
		  timer_types := NewTimerTypes,
		  cancel_timers := NewCancelTimers},
	    loop_event(Parent, NewDebug, NewS, Events, Event, Hibernate)
    end.

%% Entry point for handling an event, received or enqueued
loop_event(
  Parent, Debug,
  #{state := State, data := Data} = S,
  Events, {Type,Content} = Event, Hibernate) ->
    %%
    %% If (this old) Hibernate is true here it can only be
    %% because it was set from an event action
    %% and we did not go into hibernation since there were
    %% events in queue, so we do what the user
    %% might rely on i.e collect garbage which
    %% would have happened if we actually hibernated
    %% and immediately was awakened
    Hibernate andalso garbage_collect(),
    case call_state_function(S, Type, Content, State, Data) of
	{ok,Result,NewS} ->
	    {NextState,NewData,Actions,EnterCall} =
		parse_event_result(
		  true, Debug, NewS,
		  Events, Event, State, Data, Result),
	    loop_event_actions(
	      Parent, Debug, NewS,
	      Events, Event, NextState, NewData, Actions, EnterCall);
	{Class,Reason,Stacktrace} ->
	    terminate(
	      Class, Reason, Stacktrace, Debug, S,
	      [Event|Events])
    end.

loop_event_actions(
  Parent, Debug,
  #{state := State, state_enter := StateEnter} = S,
  Events, Event, NextState, NewData,
  Actions, EnterCall) ->
    %% Hibernate is reborn here as false being
    %% the default value from parse_actions/4
    case parse_actions(Debug, S, State, Actions) of
	{ok,NewDebug,Hibernate,TimeoutsR,Postpone,NextEventsR} ->
	    if
		StateEnter, EnterCall ->
		    loop_event_enter(
		      Parent, NewDebug, S,
		      Events, Event, NextState, NewData,
		      Hibernate, TimeoutsR, Postpone, NextEventsR);
		true ->
		    loop_event_result(
		      Parent, NewDebug, S,
		      Events, Event, NextState, NewData,
		      Hibernate, TimeoutsR, Postpone, NextEventsR)
	    end;
	{Class,Reason,Stacktrace} ->
	    terminate(
	      Class, Reason, Stacktrace, Debug, S,
	      [Event|Events])
    end.

loop_event_enter(
  Parent, Debug, #{state := State} = S,
  Events, Event, NextState, NewData,
  Hibernate, TimeoutsR, Postpone, NextEventsR) ->
    case call_state_function(S, enter, State, NextState, NewData) of
	{ok,Result,NewS} ->
	    case parse_event_result(
		   false, Debug, NewS,
		   Events, Event, NextState, NewData, Result) of
		{_,NewerData,Actions,EnterCall} ->
		    loop_event_enter_actions(
		      Parent, Debug, NewS,
		      Events, Event, NextState, NewerData,
		      Hibernate, TimeoutsR, Postpone, NextEventsR,
		      Actions, EnterCall)
	    end;
	{Class,Reason,Stacktrace} ->
	    terminate(
	      Class, Reason, Stacktrace, Debug,
	      S#{
		state := NextState,
		data := NewData,
		hibernate := Hibernate},
	      [Event|Events])
    end.

loop_event_enter_actions(
  Parent, Debug, #{state_enter := StateEnter} = S,
  Events, Event, NextState, NewData,
  Hibernate, TimeoutsR, Postpone, NextEventsR,
  Actions, EnterCall) ->
    case
	parse_enter_actions(
	  Debug, S, NextState, Actions, Hibernate, TimeoutsR)
    of
	{ok,NewDebug,NewHibernate,NewTimeoutsR,_,_} ->
	    if
		StateEnter, EnterCall ->
		    loop_event_enter(
		      Parent, NewDebug, S,
		      Events, Event, NextState, NewData,
		      NewHibernate, NewTimeoutsR, Postpone, NextEventsR);
		true ->
		    loop_event_result(
		      Parent, NewDebug, S,
		      Events, Event, NextState, NewData,
		      NewHibernate, NewTimeoutsR, Postpone, NextEventsR)
	    end;
	{Class,Reason,Stacktrace} ->
	    terminate(
	      Class, Reason, Stacktrace, Debug,
	      S#{
		state := NextState,
		data := NewData,
		hibernate := Hibernate},
	      [Event|Events])
    end.

loop_event_result(
  Parent, Debug_0,
  #{state := State, postponed := P_0,
    timer_refs := TimerRefs_0, timer_types := TimerTypes_0,
    cancel_timers := CancelTimers_0} = S_0,
  Events_0, Event_0, NextState, NewData,
  Hibernate, TimeoutsR, Postpone, NextEventsR) ->
    %%
    %% All options have been collected and next_events are buffered.
    %% Do the actual state transition.
    %%
    {Debug_1,P_1} = % Move current event to postponed if Postpone
	case Postpone of
	    true ->
		{sys_debug(Debug_0, S_0, State, {postpone,Event_0,State}),
		 [Event_0|P_0]};
	    false ->
		{sys_debug(Debug_0, S_0, State, {consume,Event_0,State}),
		 P_0}
	end,
    {Events_1,P_2,{TimerTypes_1,CancelTimers_1}} =
	%% Move all postponed events to queue and cancel the
	%% state timeout if the state changes
	if
	    NextState =:= State ->
		{Events_0,P_1,{TimerTypes_0,CancelTimers_0}};
	    true ->
		{lists:reverse(P_1, Events_0),
		 [],
		 cancel_timer_by_type(
		   state_timeout, TimerTypes_0, CancelTimers_0)}
		    %% The state timer is removed from TimerTypes_1
		    %% but remains in TimerRefs_0 until we get
		    %% the cancel_timer msg
	end,
    {TimerRefs_2,TimerTypes_2,CancelTimers_2,TimeoutEvents} =
	%% Stop and start non-event timers
	parse_timers(TimerRefs_0, TimerTypes_1, CancelTimers_1, TimeoutsR),
    %% Place next events last in reversed queue
    Events_2R = lists:reverse(Events_1, NextEventsR),
    %% Enqueue immediate timeout events and start event timer
    Events_3R =	prepend_timeout_events(TimeoutEvents, Events_2R),
    S_1 =
	S_0#{
	  state := NextState,
	  data := NewData,
	  postponed := P_2,
	  timer_refs := TimerRefs_2,
	  timer_types := TimerTypes_2,
	  cancel_timers := CancelTimers_2,
	  hibernate := Hibernate},
    case lists:reverse(Events_3R) of
	[] ->
	    %% Get a new event
	    loop(Parent, Debug_1, S_1);
	[Event|Events] ->
	    %% Loop until out of enqueued events
	    loop_event(Parent, Debug_1, S_1, Events, Event, Hibernate)
    end.


%%---------------------------------------------------------------------------
%% Server loop helpers

call_callback_mode(#{module := Module} = S) ->
    try Module:callback_mode() of
	CallbackMode ->
	    callback_mode_result(S, CallbackMode)
    catch
	CallbackMode ->
	    callback_mode_result(S, CallbackMode);
	Class:Reason ->
	    {Class,Reason,erlang:get_stacktrace()}
    end.

callback_mode_result(S, CallbackMode) ->
    case
	parse_callback_mode(
	  if
	      is_atom(CallbackMode) ->
		  [CallbackMode];
	      true ->
		  CallbackMode
	  end, undefined, false)
    of
	{undefined,_} ->
	    {error,
	     {bad_return_from_callback_mode,CallbackMode},
	     ?STACKTRACE()};
	{CBMode,StateEnter} ->
	    {ok,
	     S#{
	       callback_mode := CBMode,
	       state_enter := StateEnter}}
    end.

parse_callback_mode([], CBMode, StateEnter) ->
    {CBMode,StateEnter};
parse_callback_mode([H|T], CBMode, StateEnter) ->
    case callback_mode(H) of
	true ->
	    parse_callback_mode(T, H, StateEnter);
	false ->
	    case H of
		state_enter ->
		    parse_callback_mode(T, CBMode, true);
		_ ->
		    {undefined,StateEnter}
	    end
    end;
parse_callback_mode(_, _CBMode, StateEnter) ->
    {undefined,StateEnter}.


call_state_function(
  #{callback_mode := undefined} = S, Type, Content, State, Data) ->
    case call_callback_mode(S) of
	{ok,NewS} ->
	    call_state_function(NewS, Type, Content, State, Data);
	Error ->
	    Error
    end;
call_state_function(
  #{callback_mode := CallbackMode, module := Module} = S,
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
	    {ok,Result,S}
    catch
	Result ->
	    {ok,Result,S};
	Class:Reason ->
	    {Class,Reason,erlang:get_stacktrace()}
    end.


%% Interpret all callback return variants
parse_event_result(
  AllowStateChange, Debug, S,
  Events, Event, State, Data, Result) ->
    case Result of
	stop ->
	    terminate(
	      exit, normal, ?STACKTRACE(), Debug,
	      S#{state := State, data := Data},
	      [Event|Events]);
	{stop,Reason} ->
	    terminate(
	      exit, Reason, ?STACKTRACE(), Debug,
	      S#{state := State, data := Data},
	      [Event|Events]);
	{stop,Reason,NewData} ->
	    terminate(
	      exit, Reason, ?STACKTRACE(), Debug,
	      S#{state := State, data := NewData},
	      [Event|Events]);
	%%
	{stop_and_reply,Reason,Replies} ->
	    reply_then_terminate(
	      exit, Reason, ?STACKTRACE(), Debug,
	      S#{state := State, data := Data},
	      [Event|Events], Replies);
	{stop_and_reply,Reason,Replies,NewData} ->
	    reply_then_terminate(
	      exit, Reason, ?STACKTRACE(), Debug,
	      S#{state := State, data := NewData},
	      [Event|Events], Replies);
	%%
	{next_state,State,NewData} ->
	    {State,NewData,[],false};
	{next_state,NextState,NewData} when AllowStateChange ->
	    {NextState,NewData,[],true};
	{next_state,State,NewData,Actions} ->
	    {State,NewData,Actions,false};
	{next_state,NextState,NewData,Actions} when AllowStateChange ->
	    {NextState,NewData,Actions,true};
	%%
	{keep_state,NewData} ->
	    {State,NewData,[],false};
	{keep_state,NewData,Actions} ->
	    {State,NewData,Actions,false};
	keep_state_and_data ->
	    {State,Data,[],false};
	{keep_state_and_data,Actions} ->
	    {State,Data,Actions,false};
	%%
	{repeat_state,NewData} ->
	    {State,NewData,[],true};
	{repeat_state,NewData,Actions} ->
	    {State,NewData,Actions,true};
	repeat_state_and_data ->
	    {State,Data,[],true};
	{repeat_state_and_data,Actions} ->
	    {State,Data,Actions,true};
	%%
	_ ->
	    terminate(
	      error,
	      {bad_return_from_state_function,Result},
	      ?STACKTRACE(), Debug,
	      S#{state := State, data := Data},
	      [Event|Events])
    end.


parse_enter_actions(Debug, S, State, Actions, Hibernate, TimeoutsR) ->
    Postpone = forbidden,
    NextEventsR = forbidden,
    parse_actions(
      Debug, S, State, listify(Actions),
      Hibernate, TimeoutsR, Postpone, NextEventsR).

parse_actions(Debug, S, State, Actions) ->
    Hibernate = false,
    TimeoutsR = [infinity], %% Will cancel event timer
    Postpone = false,
    NextEventsR = [],
    parse_actions(
      Debug, S, State, listify(Actions),
      Hibernate, TimeoutsR, Postpone, NextEventsR).
%%
parse_actions(
  Debug, _S, _State, [],
  Hibernate, TimeoutsR, Postpone, NextEventsR) ->
    {ok,Debug,Hibernate,TimeoutsR,Postpone,NextEventsR};
parse_actions(
  Debug, S, State, [Action|Actions],
  Hibernate, TimeoutsR, Postpone, NextEventsR) ->
    case Action of
	%% Actual actions
	{reply,From,Reply} ->
	    case from(From) of
		true ->
		    NewDebug = do_reply(Debug, S, State, From, Reply),
		    parse_actions(
		      NewDebug, S, State, Actions,
		      Hibernate, TimeoutsR, Postpone, NextEventsR);
		false ->
		    {error,
		     {bad_action_from_state_function,Action},
		     ?STACKTRACE()}
	    end;
	%%
	%% Actions that set options
	{hibernate,NewHibernate} when is_boolean(NewHibernate) ->
	    parse_actions(
	      Debug, S, State, Actions,
	      NewHibernate, TimeoutsR, Postpone, NextEventsR);
	hibernate ->
	    NewHibernate = true,
	    parse_actions(
	      Debug, S, State, Actions,
	      NewHibernate, TimeoutsR, Postpone, NextEventsR);
	%%
	{postpone,NewPostpone}
	  when is_boolean(NewPostpone), Postpone =/= forbidden ->
	    parse_actions(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, NewPostpone, NextEventsR);
	postpone when Postpone =/= forbidden ->
	    NewPostpone = true,
	    parse_actions(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, NewPostpone, NextEventsR);
	%%
	{next_event,Type,Content} ->
	    case event_type(Type) of
		true when NextEventsR =/= forbidden ->
		    NewDebug =
			sys_debug(Debug, S, State, {in,{Type,Content}}),
		    parse_actions(
		      NewDebug, S, State, Actions,
		      Hibernate, TimeoutsR, Postpone,
		      [{Type,Content}|NextEventsR]);
		_ ->
		    {error,
		     {bad_action_from_state_function,Action},
		     ?STACKTRACE()}
	    end;
	%%
	{{timeout,_},_,_} = Timeout ->
	    parse_actions_timeout(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, Postpone, NextEventsR, Timeout);
	{{timeout,_},_,_,_} = Timeout ->
	    parse_actions_timeout(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, Postpone, NextEventsR, Timeout);
	{timeout,_,_} = Timeout ->
	    parse_actions_timeout(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, Postpone, NextEventsR, Timeout);
	{timeout,_,_,_} = Timeout ->
	    parse_actions_timeout(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, Postpone, NextEventsR, Timeout);
	{state_timeout,_,_} = Timeout ->
	    parse_actions_timeout(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, Postpone, NextEventsR, Timeout);
	{state_timeout,_,_,_} = Timeout ->
	    parse_actions_timeout(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, Postpone, NextEventsR, Timeout);
	Time ->
	    parse_actions_timeout(
	      Debug, S, State, Actions,
	      Hibernate, TimeoutsR, Postpone, NextEventsR, Time)
    end.

parse_actions_timeout(
  Debug, S, State, Actions,
  Hibernate, TimeoutsR, Postpone, NextEventsR, Timeout) ->
    case Timeout of
	{TimerType,Time,TimerMsg,TimerOpts} ->
	    case validate_timer_args(Time, listify(TimerOpts)) of
		true ->
		    parse_actions(
		      Debug, S, State, Actions,
		      Hibernate, [Timeout|TimeoutsR],
		      Postpone, NextEventsR);
		false ->
		    NewTimeout = {TimerType,Time,TimerMsg},
		    parse_actions(
		      Debug, S, State, Actions,
		      Hibernate, [NewTimeout|TimeoutsR],
		      Postpone, NextEventsR);
		error ->
		    {error,
		     {bad_action_from_state_function,Timeout},
		     ?STACKTRACE()}
	    end;
	{_,Time,_} ->
	    case validate_timer_args(Time, []) of
		false ->
		    parse_actions(
		      Debug, S, State, Actions,
		      Hibernate, [Timeout|TimeoutsR],
		      Postpone, NextEventsR);
		error ->
		    {error,
		     {bad_action_from_state_function,Timeout},
		     ?STACKTRACE()}
	    end;
	Time ->
	    case validate_timer_args(Time, []) of
		false ->
		    parse_actions(
		      Debug, S, State, Actions,
		      Hibernate, [Timeout|TimeoutsR],
		      Postpone, NextEventsR);
		error ->
		    {error,
		     {bad_action_from_state_function,Timeout},
		     ?STACKTRACE()}
	    end
    end.

validate_timer_args(Time, Opts) ->
    validate_timer_args(Time, Opts, false).
%%
validate_timer_args(Time, [], true) when is_integer(Time) ->
    true;
validate_timer_args(Time, [], false) when is_integer(Time), Time >= 0 ->
    false;
validate_timer_args(infinity, [], Abs) ->
    Abs;
validate_timer_args(Time, [{abs,Abs}|Opts], _) when is_boolean(Abs) ->
    validate_timer_args(Time, Opts, Abs);
validate_timer_args(_, [_|_], _) ->
    error.

%% Stop and start timers as well as create timeout zero events
%% and pending event timer
%%
%% Stop and start timers non-event timers
parse_timers(TimerRefs, TimerTypes, CancelTimers, TimeoutsR) ->
    parse_timers(TimerRefs, TimerTypes, CancelTimers, TimeoutsR, #{}, []).
%%
parse_timers(
  TimerRefs, TimerTypes, CancelTimers, [], _Seen, TimeoutEvents) ->
    {TimerRefs,TimerTypes,CancelTimers,TimeoutEvents};
parse_timers(
  TimerRefs, TimerTypes, CancelTimers, [Timeout|TimeoutsR],
  Seen, TimeoutEvents) ->
    case Timeout of
	{TimerType,Time,TimerMsg,TimerOpts} ->
	    %% Absolute timer
	    parse_timers(
	      TimerRefs, TimerTypes, CancelTimers, TimeoutsR,
	      Seen, TimeoutEvents,
	      TimerType, Time, TimerMsg, listify(TimerOpts));
	%% Relative timers below
	{TimerType,0,TimerMsg} ->
	    parse_timers(
	      TimerRefs, TimerTypes, CancelTimers, TimeoutsR,
	      Seen, TimeoutEvents,
	      TimerType, zero, TimerMsg, []);
	{TimerType,Time,TimerMsg} ->
	    parse_timers(
	      TimerRefs, TimerTypes, CancelTimers, TimeoutsR,
	      Seen, TimeoutEvents,
	      TimerType, Time, TimerMsg, []);
	0 ->
	    parse_timers(
	      TimerRefs, TimerTypes, CancelTimers, TimeoutsR,
	      Seen, TimeoutEvents,
	      timeout, zero, 0, []);
	Time ->
	    parse_timers(
	      TimerRefs, TimerTypes, CancelTimers, TimeoutsR,
	      Seen, TimeoutEvents,
	      timeout, Time, Time, [])
    end.

parse_timers(
  TimerRefs, TimerTypes, CancelTimers, TimeoutsR,
  Seen, TimeoutEvents,
  TimerType, Time, TimerMsg, TimerOpts) ->
    case Seen of
	#{TimerType := _} ->
	    %% Type seen before - ignore
	    parse_timers(
	      TimerRefs, TimerTypes, CancelTimers, TimeoutsR,
	      Seen, TimeoutEvents);
	#{} ->
	    %% Unseen type - handle
	    NewSeen = Seen#{TimerType => true},
	    case Time of
		infinity ->
		    %% Cancel any running timer
		    {NewTimerTypes,NewCancelTimers} =
			cancel_timer_by_type(
			  TimerType, TimerTypes, CancelTimers),
		    parse_timers(
		      TimerRefs, NewTimerTypes, NewCancelTimers, TimeoutsR,
		      NewSeen, TimeoutEvents);
		zero ->
		    %% Cancel any running timer
		    {NewTimerTypes,NewCancelTimers} =
			cancel_timer_by_type(
			  TimerType, TimerTypes, CancelTimers),
		    %% Handle zero time timeouts later
		    TimeoutEvent = {TimerType,TimerMsg},
		    parse_timers(
		      TimerRefs, NewTimerTypes, NewCancelTimers, TimeoutsR,
		      NewSeen, [TimeoutEvent|TimeoutEvents]);
		_ ->
		    %% (Re)start the timer
		    TimerRef =
			erlang:start_timer(
			  Time, self(), TimerMsg, TimerOpts),
		    case TimerTypes of
			#{TimerType := OldTimerRef} ->
			    %% Cancel the running timer
			    cancel_timer(OldTimerRef),
			    NewCancelTimers = CancelTimers + 1,
			    %% Insert the new timer into
			    %% both TimerRefs and TimerTypes
			    parse_timers(
			      TimerRefs#{TimerRef => TimerType},
			      TimerTypes#{TimerType => TimerRef},
			      NewCancelTimers, TimeoutsR,
			      NewSeen, TimeoutEvents);
			#{} ->
			    %% Insert the new timer into
			    %% both TimerRefs and TimerTypes
			    parse_timers(
			      TimerRefs#{TimerRef => TimerType},
			      TimerTypes#{TimerType => TimerRef},
			      CancelTimers, TimeoutsR,
			      NewSeen, TimeoutEvents)
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
    prepend_timeout_events(TimeoutEvents, EventsR);
prepend_timeout_events([TimeoutEvent|TimeoutEvents], EventsR) ->
    %% Just prepend all others
    prepend_timeout_events(TimeoutEvents, [TimeoutEvent|EventsR]).



%%---------------------------------------------------------------------------
%% Server helpers

reply_then_terminate(
  Class, Reason, Stacktrace, Debug,
  #{state := State} = S, Q, Replies) ->
    do_reply_then_terminate(
      Class, Reason, Stacktrace, Debug,
      S, Q, listify(Replies), State).
%%
do_reply_then_terminate(
  Class, Reason, Stacktrace, Debug, S, Q, [], _State) ->
    terminate(Class, Reason, Stacktrace, Debug, S, Q);
do_reply_then_terminate(
  Class, Reason, Stacktrace, Debug, S, Q, [R|Rs], State) ->
    case R of
	{reply,{_To,_Tag}=From,Reply} ->
	    NewDebug = do_reply(Debug, S, State, From, Reply),
	    do_reply_then_terminate(
	      Class, Reason, Stacktrace, NewDebug, S, Q, Rs, State);
	_ ->
	    terminate(
	      error,
	      {bad_reply_action_from_state_function,R},
	      ?STACKTRACE(),
	      Debug, S, Q)
    end.

do_reply(Debug, S, State, From, Reply) ->
    reply(From, Reply),
    sys_debug(Debug, S, State, {out,Reply,From}).


terminate(
  Class, Reason, Stacktrace, Debug,
  #{module := Module, state := State, data := Data, postponed := P} = S,
  Q) ->
    case erlang:function_exported(Module, terminate, 3) of
	true ->
	    try Module:terminate(Reason, State, Data) of
		_ -> ok
	    catch
		_ -> ok;
		C:R ->
		    ST = erlang:get_stacktrace(),
		    error_info(
		      C, R, ST, S, Q, P,
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
		sys_debug(Debug, S, State, {terminate,Reason});
	    shutdown ->
		sys_debug(Debug, S, State, {terminate,Reason});
	    {shutdown,_} ->
		sys_debug(Debug, S, State, {terminate,Reason});
	    _ ->
		error_info(
		  Class, Reason, Stacktrace, S, Q, P,
		  format_status(terminate, get(), S)),
		sys:print_log(Debug)
	end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	_ ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

error_info(
  Class, Reason, Stacktrace,
  #{name := Name,
    callback_mode := CallbackMode,
    state_enter := StateEnter},
  Q, P, FmtData) ->
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
    CBMode =
	 case StateEnter of
	     true ->
		 [CallbackMode,state_enter];
	     false ->
		 CallbackMode
	 end,
    error_logger:format(
      "** State machine ~p terminating~n" ++
	  case Q of
	      [] -> "";
	      _ -> "** Last event = ~p~n"
	  end ++
	  "** When server state  = ~p~n" ++
	  "** Reason for termination = ~w:~p~n" ++
	  "** Callback mode = ~p~n" ++
	  case Q of
	      [_,_|_] -> "** Queued = ~p~n";
	      _ -> ""
	  end ++
	  case P of
	      [] -> "";
	      _ -> "** Postponed = ~p~n"
	  end ++
	  case FixedStacktrace of
	      [] -> "";
	      _ -> "** Stacktrace =~n**  ~p~n"
	  end,
      [Name |
       case Q of
	   [] -> [];
	   [Event|_] -> [Event]
       end] ++
	  [FmtData,
	   Class,FixedReason,
	   CBMode] ++
	  case Q of
	      [_|[_|_] = Events] -> [Events];
	      _ -> []
	  end ++
	  case P of
	      [] -> [];
	      _ -> [P]
	  end ++
	  case FixedStacktrace of
	      [] -> [];
	      _ -> [FixedStacktrace]
	  end).


%% Call Module:format_status/2 or return a default value
format_status(
  Opt, PDict,
  #{module := Module, state := State, data := Data}) ->
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
cancel_timer_by_type(TimerType, TimerTypes, CancelTimers) ->
    case TimerTypes of
	#{TimerType := TimerRef} ->
	    cancel_timer(TimerRef),
	    {maps:remove(TimerType, TimerTypes),CancelTimers + 1};
	#{} ->
	    {TimerTypes,CancelTimers}
    end.

cancel_timer(TimerRef) ->
    ok = erlang:cancel_timer(TimerRef, [{async,true}]).
