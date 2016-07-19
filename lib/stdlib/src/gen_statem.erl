%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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
    enter_loop/5,enter_loop/6,enter_loop/7,
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

%% Type exports for templates
-export_type(
   [event_type/0,
    callback_mode/0,
    state_function_result/0,
    handle_event_result/0,
    action/0]).

%% Fix problem for doc build
-export_type([transition_option/0]).

%%%==========================================================================
%%% Interface functions.
%%%==========================================================================

-type from() ::
	{To :: pid(), Tag :: term()}. % Reply-to specifier for call

-type state() ::
	state_name() | % For state callback function StateName/5
	term(). % For state callback function handle_event/5

-type state_name() :: atom().

-type data() :: term().

-type event_type() ::
	{'call',From :: from()} | 'cast' |
	'info' | 'timeout' | 'internal'.

-type callback_mode() :: 'state_functions' | 'handle_event_function'.

-type transition_option() ::
	postpone() | hibernate() | event_timeout().
-type postpone() ::
	%% If 'true' postpone the current event
	%% and retry it when the state changes (=/=)
	boolean().
-type hibernate() ::
	%% If 'true' hibernate the server instead of going into receive
	boolean().
-type event_timeout() ::
	%% Generate a ('timeout', EventContent, ...) event after Time
	%% unless some other event is delivered
	Time :: timeout().

-type action() ::
	%% During a state change:
	%% * NextState and NewData are set.
	%% * All action()s are executed in order of apperance.
	%% * Postponing the current event is performed
	%%   iff 'postpone' is 'true'.
	%% * A state timer is started iff 'timeout' is set.
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
	'hibernate' | % Set the hibernate option
	{'hibernate', Hibernate :: hibernate()} |
	%%
	(Timeout :: event_timeout()) | % {timeout,Timeout}
	{'timeout', % Set the event timeout option
	 Time :: event_timeout(), EventContent :: term()} |
	%%
	reply_action() |
	%%
	%% All 'next_event' events are kept in a list and then
	%% inserted at state changes so the first in the
	%% action() list is the first to be delivered.
	{'next_event', % Insert event as the next to handle
	 EventType :: event_type(),
	 EventContent :: term()}.
-type reply_action() ::
	{'reply', % Reply to a caller
	 From :: from(), Reply :: term()}.

-type state_function_result() ::
	{'next_state', % {next_state,NextStateName,NewData,[]}
	 NextStateName :: state_name(),
	 NewData :: data()} |
	{'next_state', % State transition, maybe to the same state
	 NextStateName :: state_name(),
	 NewData :: data(),
	 Actions :: [action()] | action()} |
	common_state_callback_result().
-type handle_event_result() ::
	{'next_state', % {next_state,NextState,NewData,[]}
	 NextState :: state(),
	 NewData :: data()} |
	{'next_state', % State transition, maybe to the same state
	 NextState :: state(),
	 NewData :: data(),
	 Actions :: [action()] | action()} |
	common_state_callback_result().
-type common_state_callback_result() ::
	'stop' | % {stop,normal}
	{'stop', % Stop the server
	 Reason :: term()} |
	{'stop', % Stop the server
	 Reason :: term(),
	 NewData :: data()} |
	{'stop_and_reply', % Reply then stop the server
	 Reason :: term(),
	 Replies :: [reply_action()] | reply_action()} |
	{'stop_and_reply', % Reply then stop the server
	 Reason :: term(),
	 Replies :: [reply_action()] | reply_action(),
	 NewData :: data()} |
	{'keep_state', % {keep_state,NewData,[]}
	 NewData :: data()} |
	{'keep_state', % Keep state, change data
	 NewData :: data(),
	 Actions :: [action()] | action()} |
	'keep_state_and_data' | % {keep_state_and_data,[]}
	{'keep_state_and_data', % Keep state and data -> only actions
	 Actions :: [action()] | action()}.


%% The state machine init function.  It is called only once and
%% the server is not running until this function has returned
%% an {ok, ...} tuple.  Thereafter the state callbacks are called
%% for all events to this server.
-callback init(Args :: term()) ->
    {callback_mode(), state(), data()} |
    {callback_mode(), state(), data(), [action()] | action()} |
    'ignore' |
    {'stop', Reason :: term()}.

%% Example state callback for callback_mode() =:= state_functions
%% state name 'state_name'.
%%
%% In this mode all states has to be type state_name() i.e atom().
%%
%% Note that state callbacks and only state callbacks have arity 5
%% and that is intended.
-callback state_name(
	    event_type(),
	    EventContent :: term(),
	    Data :: data()) ->
    state_function_result().
%%
%% State callback for callback_mode() =:= handle_event_function.
%%
%% Note that state callbacks and only state callbacks have arity 5
%% and that is intended.
-callback handle_event(
	    event_type(),
	    EventContent :: term(),
	    State :: state(), % Current state
	    Data :: data()) ->
    handle_event_result().

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
    {CallbackMode :: callback_mode(),
     NewState :: state(),
     NewData :: data()} |
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
   [init/1, % One may use enter_loop/5,6,7 instead
    format_status/2, % Has got a default implementation
    %%
    state_name/3, % Example for callback_mode =:= state_functions:
    %% there has to be a StateName/5 callback function for every StateName.
    %%
    handle_event/4]). % For callback_mode =:= handle_event_function

%% Type validation functions
callback_mode(CallbackMode) ->
    case CallbackMode of
	state_functions ->
	    true;
	handle_event_function ->
	    true;
	_ ->
	    false
    end.
%%
from({Pid,_}) when is_pid(Pid) ->
    true;
from(_) ->
    false.
%%
event_type({call,From}) ->
    from(From);
event_type(Type) ->
    case Type of
	cast ->
	    true;
	info ->
	    true;
	timeout ->
	    true;
	internal ->
	    true;
	_ ->
	    false
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
	Timeout :: timeout()) ->
		  Reply :: term().
call(ServerRef, Request, infinity) ->
    try gen:call(ServerRef, '$gen_call', Request, infinity) of
	{ok,Reply} ->
	    Reply
    catch
	Class:Reason ->
	    erlang:raise(
	      Class,
	      {Reason,{?MODULE,call,[ServerRef,Request,infinity]}},
	      erlang:get_stacktrace())
    end;
call(ServerRef, Request, Timeout) ->
    %% Call server through proxy process to dodge any late reply
    Ref = make_ref(),
    Self = self(),
    Pid = spawn(
	    fun () ->
		    Self !
			try gen:call(
			      ServerRef, '$gen_call', Request, Timeout) of
			    Result ->
				{Ref,Result}
			catch Class:Reason ->
				{Ref,Class,Reason,erlang:get_stacktrace()}
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
	CallbackMode :: callback_mode(),
	State :: state(), Data :: data()) ->
			no_return().
enter_loop(Module, Opts, CallbackMode, State, Data) ->
    enter_loop(Module, Opts, CallbackMode, State, Data, self()).
%%
-spec enter_loop(
	Module :: module(), Opts :: [debug_opt()],
	CallbackMode :: callback_mode(),
	State :: state(), Data :: data(),
	Server_or_Actions ::
	  server_name() | pid() | [action()]) ->
			no_return().
enter_loop(Module, Opts, CallbackMode, State, Data, Server_or_Actions) ->
    if
	is_list(Server_or_Actions) ->
	    enter_loop(
	      Module, Opts, CallbackMode, State, Data,
	      self(), Server_or_Actions);
	true ->
	    enter_loop(
	      Module, Opts, CallbackMode, State, Data,
	      Server_or_Actions, [])
    end.
%%
-spec enter_loop(
	Module :: module(), Opts :: [debug_opt()],
	CallbackMode :: callback_mode(),
	State :: state(), Data :: data(),
	Server :: server_name() | pid(),
	Actions :: [action()] | action()) ->
			no_return().
enter_loop(Module, Opts, CallbackMode, State, Data, Server, Actions) ->
    is_atom(Module) orelse error({atom,Module}),
    callback_mode(CallbackMode) orelse error({callback_mode,CallbackMode}),
    Parent = gen:get_parent(),
    enter(Module, Opts, CallbackMode, State, Data, Server, Actions, Parent).

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
enter(Module, Opts, CallbackMode, State, Data, Server, Actions, Parent) ->
    %% The values should already have been type checked
    Name = gen:get_proc_name(Server),
    Debug = gen:debug_options(Name, Opts),
    P = Events = [],
    Event = {internal,initial_state},
    %% We enforce {postpone,false} to ensure that
    %% our fake Event gets discarded, thought it might get logged
    NewActions =
	if
	    is_list(Actions) ->
		Actions ++ [{postpone,false}];
	    true ->
		[Actions,{postpone,false}]
	end,
    S =	#{
      callback_mode => CallbackMode,
      module => Module,
      name => Name,
      %% All fields below will be replaced according to the arguments to
      %% loop_event_actions/10 when it finally loops back to loop/3
      state => State,
      data => Data,
      postponed => P,
      hibernate => false,
      timer => undefined},
    NewDebug = sys_debug(Debug, S, State, {enter,Event,State}),
    loop_event_actions(
      Parent, NewDebug, S, Events,
      State, Data, P, Event, State, NewActions).

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
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.

%%---------------------------------------------------------------------------
%% gen callbacks helpers

init_result(Starter, Parent, ServerRef, Module, Result, Opts) ->
    case Result of
	{CallbackMode,State,Data} ->
	    case callback_mode(CallbackMode) of
		true ->
		    proc_lib:init_ack(Starter, {ok,self()}),
		    enter(
		      Module, Opts, CallbackMode, State, Data,
		      ServerRef, [], Parent);
		false ->
		    Error = {callback_mode,CallbackMode},
		    proc_lib:init_ack(Starter, {error,Error}),
		    exit(Error)
	    end;
	{CallbackMode,State,Data,Actions} ->
	    case callback_mode(CallbackMode) of
		true ->
		    proc_lib:init_ack(Starter, {ok,self()}),
		    enter(
		      Module, Opts, CallbackMode, State, Data,
		      ServerRef, Actions, Parent);
		false ->
		    Error = {callback_mode,CallbackMode},
		    proc_lib:init_ack(Starter, {error,Error}),
		    exit(Error)
	    end;
	{stop,Reason} ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    exit(Reason);
	ignore ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	_ ->
	    Error = {bad_return_value,Result},
	    proc_lib:init_ack(Starter, {error,Error}),
	    exit(Error)
    end.

%%%==========================================================================
%%% sys callbacks

system_continue(Parent, Debug, S) ->
    loop(Parent, Debug, S).

system_terminate(
  Reason, _Parent, Debug,
  #{state := State, data := Data, postponed := P} = S) ->
    terminate(
      exit, Reason, ?STACKTRACE(),
      Debug, S, [], State, Data, P).

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
	{CallbackMode,NewState,NewData} ->
	    callback_mode(CallbackMode) orelse
		error({callback_mode,CallbackMode}),
	    {ok,
	     S#{callback_mode := CallbackMode,
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
   #{name := Name, postponed := P, state := State, data := Data} = S]) ->
    Header = gen:format_status_header("Status for state machine", Name),
    Log = sys:get_debug(log, Debug, []),
    [{header,Header},
     {data,
      [{"Status",SysState},
       {"Parent",Parent},
       {"Logged Events",Log},
       {"Postponed",P}]} |
     case format_status(Opt, PDict, S, State, Data) of
	 L when is_list(L) -> L;
	 T -> [T]
     end].

%%---------------------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%---------------------------------------------------------------------------

print_event(Dev, {in,Event}, {Name,_}) ->
    io:format(
      Dev, "*DBG* ~p received ~s~n",
      [Name,event_string(Event)]);
print_event(Dev, {out,Reply,{To,_Tag}}, {Name,_}) ->
    io:format(
      Dev, "*DBG* ~p sent ~p to ~p~n",
      [Name,Reply,To]);
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
loop(Parent, Debug, #{hibernate := Hibernate} = S) ->
    case Hibernate of
	true ->
	    %% Does not return but restarts process at
	    %% wakeup_from_hibernate/3 that jumps to loop_receive/3
	    proc_lib:hibernate(
	      ?MODULE, wakeup_from_hibernate, [Parent,Debug,S]),
	    error(
	      {should_not_have_arrived_here_but_instead_in,
	       {wakeup_from_hibernate,3}});
	false ->
	    loop_receive(Parent, Debug, S)
    end.

%% Entry point for wakeup_from_hibernate/3
loop_receive(Parent, Debug, #{timer := Timer} = S) ->
    receive
	Msg ->
	    case Msg of
		{system,Pid,Req} ->
		    #{hibernate := Hibernate} = S,
		    %% Does not return but tail recursively calls
		    %% system_continue/3 that jumps to loop/3
		    sys:handle_system_msg(
		      Req, Pid, Parent, ?MODULE, Debug, S, Hibernate);
		{'EXIT',Parent,Reason} = EXIT ->
		    #{state := State, data := Data, postponed := P} = S,
		    %% EXIT is not a 2-tuple and therefore
		    %% not an event and has no event_type(),
		    %% but this will stand out in the crash report...
		    terminate(
		      exit, Reason, ?STACKTRACE(),
		      Debug, S, [EXIT], State, Data, P);
		{timeout,Timer,Content} when Timer =/= undefined ->
		    loop_receive_result(
		      Parent, Debug, S, {timeout,Content});
		_ ->
		    %% Cancel Timer if running
		    case Timer of
			undefined ->
			    ok;
			_ ->
			    case erlang:cancel_timer(Timer) of
				TimeLeft when is_integer(TimeLeft) ->
				    ok;
				false ->
				    receive
					{timeout,Timer,_} ->
					    ok
				    after 0 ->
					    ok
				    end
			    end
		    end,
		    Event =
			case Msg of
			    {'$gen_call',From,Request} ->
				{{call,From},Request};
			    {'$gen_cast',E} ->
				{cast,E};
			    _ ->
				{info,Msg}
			end,
		    loop_receive_result(Parent, Debug, S, Event)
	    end
    end.

loop_receive_result(
  Parent, Debug,
  #{state := State,
    data := Data,
    postponed := P} = S,
  Event) ->
    %% The engine state map S is now dismantled
    %% and will not be restored until we return to loop/3.
    %%
    %% The fields 'callback_mode', 'module', and 'name' are still valid.
    %% The fields 'state', 'data', and 'postponed' are held in arguments.
    %% The fields 'timer' and 'hibernate' will be recalculated.
    %%
    NewDebug = sys_debug(Debug, S, State, {in,Event}),
    %% Here the queue of not yet handled events is created
    Events = [],
    Hibernate = false,
    loop_event(
      Parent, NewDebug, S, Events, State, Data, P, Event, Hibernate).

%% Process the event queue, or if it is empty
%% loop back to loop/3 to receive a new event
loop_events(
  Parent, Debug, S, [Event|Events],
  State, Data, P, Hibernate, _Timeout) ->
    %%
    %% If there was a state timer requested we just ignore that
    %% since we have events to handle which cancels the timer
    loop_event(
      Parent, Debug, S, Events, State, Data, P, Event, Hibernate);
loop_events(
  Parent, Debug, S, [],
  State, Data, P, Hibernate, Timeout) ->
    case Timeout of
	{timeout,0,EventContent} ->
	    %% Immediate timeout - simulate it
	    %% so we do not get the timeout message
	    %% after any received event
	    loop_event(
	      Parent, Debug, S, [],
	      State, Data, P, {timeout,EventContent}, Hibernate);
	{timeout,Time,EventContent} ->
	    %% Actually start a timer
	    Timer = erlang:start_timer(Time, self(), EventContent),
	    loop_events_done(
	      Parent, Debug, S, Timer, State, Data, P, Hibernate);
	undefined ->
	    %% No state timeout has been requested
	    Timer = undefined,
	    loop_events_done(
	      Parent, Debug, S, Timer, State, Data, P, Hibernate)
    end.
%%
loop_events_done(Parent, Debug, S, Timer, State, Data, P, Hibernate) ->
    NewS =
	S#{
	  state := State,
	  data := Data,
	  postponed := P,
	  hibernate := Hibernate,
	  timer := Timer},
    loop(Parent, Debug, NewS).

loop_event(
  Parent, Debug,
  #{callback_mode := CallbackMode,
    module := Module} = S,
  Events,
  State, Data, P, {Type,Content} = Event, Hibernate) ->
    %%
    %% If Hibernate is true here it can only be
    %% because it was set from an event action
    %% and we did not go into hibernation since there
    %% were events in queue, so we do what the user
    %% might depend on i.e collect garbage which
    %% would have happened if we actually hibernated
    %% and immediately was awakened
    Hibernate andalso garbage_collect(),
    %%
    try
	case CallbackMode of
	    state_functions ->
		Module:State(Type, Content, Data);
	    handle_event_function ->
		Module:handle_event(Type, Content, State, Data)
	end of
	Result ->
	    loop_event_result(
	      Parent, Debug, S, Events, State, Data, P, Event, Result)
    catch
	Result ->
	    loop_event_result(
	      Parent, Debug, S, Events, State, Data, P, Event, Result);
	error:badarg when CallbackMode =:= state_functions ->
	    case erlang:get_stacktrace() of
		[{erlang,apply,[Module,State,_],_}|Stacktrace] ->
		    Args = [Type,Content,Data],
		    terminate(
		      error,
		      {undef_state_function,{Module,State,Args}},
		      Stacktrace,
		      Debug, S, [Event|Events], State, Data, P);
		Stacktrace ->
		    terminate(
		      error, badarg, Stacktrace,
		      Debug, S, [Event|Events], State, Data, P)
	    end;
	error:undef ->
	    %% Process an undef to check for the simple mistake
	    %% of calling a nonexistent state function
	    case erlang:get_stacktrace() of
		[{Module,State,
		  [Type,Content,Data]=Args,
		  _}
		 |Stacktrace]
		when CallbackMode =:= state_functions ->
		    terminate(
		      error,
		      {undef_state_function,{Module,State,Args}},
		      Stacktrace,
		      Debug, S, [Event|Events], State, Data, P);
		[{Module,handle_event,
		  [Type,Content,State,Data]=Args,
		  _}
		 |Stacktrace]
		when CallbackMode =:= handle_event_function ->
		    terminate(
		      error,
		      {undef_state_function,{Module,handle_event,Args}},
		      Stacktrace,
		      Debug, S, [Event|Events], State, Data, P);
		Stacktrace ->
		    terminate(
		      error, undef, Stacktrace,
		      Debug, S, [Event|Events], State, Data, P)
	    end;
	Class:Reason ->
	    Stacktrace = erlang:get_stacktrace(),
	    terminate(
	      Class, Reason, Stacktrace,
	      Debug, S, [Event|Events], State, Data, P)
    end.

%% Interpret all callback return variants
loop_event_result(
  Parent, Debug, S, Events, State, Data, P, Event, Result) ->
    case Result of
	stop ->
	    terminate(
	      exit, normal, ?STACKTRACE(),
	      Debug, S, [Event|Events], State, Data, P);
	{stop,Reason} ->
	    terminate(
	      exit, Reason, ?STACKTRACE(),
	      Debug, S, [Event|Events], State, Data, P);
	{stop,Reason,NewData} ->
	    terminate(
	      exit, Reason, ?STACKTRACE(),
	      Debug, S, [Event|Events], State, NewData, P);
	{stop_and_reply,Reason,Replies} ->
	    Q = [Event|Events],
	    reply_then_terminate(
	      exit, Reason, ?STACKTRACE(),
	      Debug, S, Q, State, Data, P, Replies);
	{stop_and_reply,Reason,Replies,NewData} ->
	    Q = [Event|Events],
	    reply_then_terminate(
	      exit, Reason, ?STACKTRACE(),
	      Debug, S, Q, State, NewData, P, Replies);
	{next_state,NextState,NewData} ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, []);
	{next_state,NextState,NewData,Actions} ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions);
	{keep_state,NewData} ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, State, []);
	{keep_state,NewData,Actions} ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, State, Actions);
	keep_state_and_data ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, Data, P, Event, State, []);
	{keep_state_and_data,Actions} ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, Data, P, Event, State, Actions);
	_ ->
	    terminate(
	      error, {bad_return_value,Result}, ?STACKTRACE(),
	      Debug, S, [Event|Events], State, Data, P)
    end.

loop_event_actions(
  Parent, Debug, S, Events, State, NewData, P, Event, NextState, Actions) ->
    Postpone = false, % Shall we postpone this event; boolean()
    Hibernate = false,
    Timeout = undefined,
    NextEvents = [],
    loop_event_actions(
      Parent, Debug, S, Events, State, NewData, P, Event, NextState,
      if
	  is_list(Actions) ->
	      Actions;
	  true ->
	      [Actions]
      end,
      Postpone, Hibernate, Timeout, NextEvents).
%%
%% Process all actions
loop_event_actions(
  Parent, Debug, S, Events,
  State, NewData, P, Event, NextState, [Action|Actions],
  Postpone, Hibernate, Timeout, NextEvents) ->
    case Action of
	%% Actual actions
	{reply,From,Reply} ->
	    case from(From) of
		true ->
		    NewDebug = do_reply(Debug, S, State, From, Reply),
		    loop_event_actions(
		      Parent, NewDebug, S, Events,
		      State, NewData, P, Event, NextState, Actions,
		      Postpone, Hibernate, Timeout, NextEvents);
		false ->
		    terminate(
		      error, {bad_action,Action}, ?STACKTRACE(),
		      Debug, S, [Event|Events], State, NewData, P)
	    end;
	{next_event,Type,Content} ->
	    case event_type(Type) of
		true ->
		    loop_event_actions(
		      Parent, Debug, S, Events,
		      State, NewData, P, Event, NextState, Actions,
		      Postpone, Hibernate, Timeout,
		      [{Type,Content}|NextEvents]);
		false ->
		    terminate(
		      error, {bad_action,Action}, ?STACKTRACE(),
		      Debug, S, [Event|Events], State, NewData, P)
	    end;
	%% Actions that set options
	{postpone,NewPostpone} when is_boolean(NewPostpone) ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions,
	      NewPostpone, Hibernate, Timeout, NextEvents);
	{postpone,_} ->
	    terminate(
	      error, {bad_action,Action}, ?STACKTRACE(),
	      Debug, S, [Event|Events], State, NewData, P);
	postpone ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions,
	      true, Hibernate, Timeout, NextEvents);
	{hibernate,NewHibernate} when is_boolean(NewHibernate) ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions,
	      Postpone, NewHibernate, Timeout, NextEvents);
	{hibernate,_} ->
	    terminate(
	      error, {bad_action,Action}, ?STACKTRACE(),
	      Debug, S, [Event|Events], State, NewData, P);
	hibernate ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions,
	      Postpone, true, Timeout, NextEvents);
	{timeout,infinity,_} -> % Clear timer - it will never trigger
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions,
	      Postpone, Hibernate, undefined, NextEvents);
	{timeout,Time,_} = NewTimeout when is_integer(Time), Time >= 0 ->
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions,
	      Postpone, Hibernate, NewTimeout, NextEvents);
	{timeout,_,_} ->
	    terminate(
	      error, {bad_action,Action}, ?STACKTRACE(),
	      Debug, S, [Event|Events], State, NewData, P);
	infinity -> % Clear timer - it will never trigger
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions,
	      Postpone, Hibernate, undefined, NextEvents);
	Time when is_integer(Time), Time >= 0 ->
	    NewTimeout = {timeout,Time,Time},
	    loop_event_actions(
	      Parent, Debug, S, Events,
	      State, NewData, P, Event, NextState, Actions,
	      Postpone, Hibernate, NewTimeout, NextEvents);
	_ ->
	    terminate(
	      error, {bad_action,Action}, ?STACKTRACE(),
	      Debug, S, [Event|Events], State, NewData, P)
    end;
%%
%% End of actions list
loop_event_actions(
  Parent, Debug, S, Events,
  State, NewData, P0, Event, NextState, [],
  Postpone, Hibernate, Timeout, NextEvents) ->
    %%
    %% All options have been collected and next_events are buffered.
    %% Do the actual state transition.
    %%
    P1 = % Move current event to postponed if Postpone
	case Postpone of
	    true ->
		[Event|P0];
	    false ->
		P0
	end,
    {Q2,P} = % Move all postponed events to queue if state change
	if
	    NextState =:= State ->
		{Events,P1};
	    true ->
		{lists:reverse(P1, Events),[]}
	end,
    %% Place next events first in queue
    Q = lists:reverse(NextEvents, Q2),
    %%
    NewDebug =
	sys_debug(
	  Debug, S, State,
	  case Postpone of
	      true ->
		  {postpone,Event,NextState};
	      false ->
		  {consume,Event,NextState}
	  end),
    loop_events(
      Parent, NewDebug, S, Q, NextState, NewData, P, Hibernate, Timeout).

%%---------------------------------------------------------------------------
%% Server helpers

reply_then_terminate(
  Class, Reason, Stacktrace,
  Debug, S, Q, State, Data, P, Replies) ->
    if
	is_list(Replies) ->
	    do_reply_then_terminate(
	      Class, Reason, Stacktrace,
	      Debug, S, Q, State, Data, P, Replies);
	true ->
	    do_reply_then_terminate(
	      Class, Reason, Stacktrace,
	      Debug, S, Q, State, Data, P, [Replies])
    end.
%%
do_reply_then_terminate(
  Class, Reason, Stacktrace, Debug, S, Q, State, Data, P, []) ->
    terminate(Class, Reason, Stacktrace, Debug, S, Q, State, Data, P);
do_reply_then_terminate(
  Class, Reason, Stacktrace, Debug, S, Q, State, Data, P, [R|Rs]) ->
    case R of
	{reply,{_To,_Tag}=From,Reply} ->
	    NewDebug = do_reply(Debug, S, State, From, Reply),
	    do_reply_then_terminate(
	      Class, Reason, Stacktrace,
	      NewDebug, S, Q, State, Data, P, Rs);
	_ ->
	    terminate(
	      error, {bad_action,R}, ?STACKTRACE(),
	      Debug, S, Q, State, Data, P)
    end.

do_reply(Debug, S, State, From, Reply) ->
    reply(From, Reply),
    sys_debug(Debug, S, State, {out,Reply,From}).


terminate(
  Class, Reason, Stacktrace,
  Debug, #{module := Module} = S, Q, State, Data, P) ->
    try Module:terminate(Reason, State, Data) of
	_ -> ok
    catch
	_ -> ok;
	C:R ->
	    ST = erlang:get_stacktrace(),
	    error_info(
	      C, R, ST, Debug, S, Q, P,
	      format_status(terminate, get(), S, State, Data)),
	    erlang:raise(C, R, ST)
    end,
    case Reason of
	normal -> ok;
	shutdown -> ok;
	{shutdown,_} -> ok;
	_ ->
	    error_info(
	      Class, Reason, Stacktrace, Debug, S, Q, P,
	      format_status(terminate, get(), S, State, Data))
    end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	_ ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

error_info(
  Class, Reason, Stacktrace, Debug,
  #{name := Name, callback_mode := CallbackMode},
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
	  [FmtData,Class,FixedReason,
	   CallbackMode] ++
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
	  end),
    sys:print_log(Debug),
    ok.


%% Call Module:format_status/2 or return a default value
format_status(Opt, PDict, #{module := Module}, State, Data) ->
    case erlang:function_exported(Module, format_status, 2) of
	true ->
	    try Module:format_status(Opt, [PDict,State,Data])
	    catch
		Result -> Result;
		_:_ ->
		    format_status_default(
		      Opt, State,
		      "Module:format_status/2 crashed")
	    end;
	false ->
	    format_status_default(Opt, State, Data)
    end.

%% The default Module:format_status/2
format_status_default(Opt, State, Data) ->
    SSD = {State,Data},
    case Opt of
	terminate ->
	    SSD;
	_ ->
	    [{data,[{"State",SSD}]}]
    end.
