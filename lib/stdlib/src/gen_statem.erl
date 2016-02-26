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

%% Fix problem for doc build
-export_type([transition_option/0,state_callback_result/0]).

%%%==========================================================================
%%% Interface functions.
%%%==========================================================================

-type caller() ::
	{To :: pid(), Tag :: term()}. % Reply-to specifier for call

-type state() ::
	state_name() | % For state callback function StateName/5
	term(). % For state callback function handle_event/5

-type state_name() :: atom().

-type data() :: term().

-type event_type() ::
	{'call',Caller :: caller()} | 'cast' |
	'info' | 'timeout' | 'internal'.

-type event_predicate() :: % Return true for the event in question
	fun((event_type(), term()) -> boolean()).

-type callback_mode() :: 'state_functions' | 'handle_event_function'.

-type transition_option() ::
	postpone() | hibernate() | state_timeout().
-type postpone() ::
	%% If 'true' postpone the current event
	%% and retry it when the state changes (=/=)
	boolean().
-type hibernate() ::
	%% If 'true' hibernate the server instead of going into receive
	boolean().
-type state_timeout() ::
	%% Generate a ('timeout', Msg, ...) event after Time
	%% unless some other event is delivered
	Time :: timeout().

-type action() ::
	%% During a state change:
	%% * NewState and NewData are set.
	%% * All action()s are executed in order of apperance.
	%% * Postponing the current event is performed
	%%   iff 'postpone' is 'true'.
	%% * A state timer is started iff 'timeout' is set.
	%% * Pending events are processed or if there are
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
	(Timeout :: state_timeout()) | % {timeout,Timeout}
	{'timeout', % Set the timeout option
	 Time :: state_timeout(), Msg :: term()} |
	%%
	reply_action() |
	%%
	%% All 'next_event' events are kept in a list and then
	%% inserted at state changes so the first in the
	%% action() list is the first to be delivered.
	{'next_event', % Insert event as the next to handle
	 EventType :: event_type(),
	 EventContent :: term()} |
	%%
	{'remove_event', % Remove the oldest matching (=:=) event
	 EventType :: event_type(), EventContent :: term()} |
	{'remove_event', % Remove the oldest event satisfying predicate
	 EventPredicate :: event_predicate()} |
	%%
	{'cancel_timer', % Cancel timer and clean up mess(ages)
	 TimerRef :: reference()} |
	{'demonitor', % Demonitor and clean up mess(ages)
	 MonitorRef :: reference()} |
	{'unlink', % Unlink and clean up mess(ages)
	 Id :: pid() | port()}.
-type reply_action() ::
	{'reply', % Reply to a caller
	 Caller :: caller(), Reply :: term()}.

-type state_callback_result() ::
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
	{'next_state', % {next_state,NewState,NewData,[]}
	 NewState :: state(),
	 NewData :: data()} |
	{'next_state', % State transition, maybe to the same state
	 NewState :: state(),
	 NewData :: data(),
	 Actions :: [action()] | action()} |
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
    state_callback_result().
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
    state_callback_result().

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
    {ok, {NewState :: state(), NewData :: data()}}.

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
caller({Pid,Tag}) when is_pid(Pid), is_reference(Tag) ->
    true;
caller(_) ->
    false.
%%
event_type({call,Caller}) ->
    caller(Caller);
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

-define(
   TERMINATE(Class, Reason, Debug, S, Q),
   terminate(
     Class,
     begin Reason end,
     ?STACKTRACE(),
     begin Debug end,
     begin S end,
     begin Q end)).

%%%==========================================================================
%%% API

-type server_name() ::
      {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), Name :: term()}
      | {'local', atom()}.
-type server_ref() ::
      {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}
      | (LocalName :: atom())
      | {Name :: atom(), Node :: atom()}
      | pid().
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
    try	global:send(Name, cast(Msg)) of
	_ -> ok
    catch
	_:_ -> ok
    end;
cast({via,RegMod,Name}, Msg) ->
    try	RegMod:send(Name, cast(Msg)) of
	_ -> ok
    catch
	_:_ -> ok
    end;
cast({Name,Node} = ServerRef, Msg) when is_atom(Name), is_atom(Node) ->
    do_send(ServerRef, cast(Msg));
cast(ServerRef, Msg) when is_atom(ServerRef) ->
    do_send(ServerRef, cast(Msg));
cast(ServerRef, Msg) when is_pid(ServerRef) ->
    do_send(ServerRef, cast(Msg)).

%% Call a state machine (synchronous; a reply is expected) that
%% arrives with type {call,Caller}
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
	    %% There is just a theoretical possibility that the
	    %% proxy process gets killed between try--of and !
	    %% so this clause is in case of that
	    exit(Reason)
    end.

%% Reply from a state machine callback to whom awaits in call/2
-spec reply([reply_action()] | reply_action()) -> ok.
reply({reply,{_To,_Tag}=Caller,Reply}) ->
    reply(Caller, Reply);
reply(Replies) when is_list(Replies) ->
    [reply(Reply) || Reply <- Replies],
    ok.
%%
-spec reply(Caller :: caller(), Reply :: term()) -> ok.
reply({To,Tag}, Reply) ->
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
    case is_atom(Module) andalso callback_mode(CallbackMode) of
	true ->
	    Parent = gen:get_parent(),
	    enter(
	      Module, Opts, CallbackMode, State, Data, Server,
	      Actions, Parent);
	false ->
	    error(
	      badarg,
	      [Module,Opts,CallbackMode,State,Data,Server,Actions])
    end.

%%---------------------------------------------------------------------------
%% API helpers

cast(Event) ->
    {'$gen_cast',Event}.

%% Might actually not send the message in case of caught exception
do_send(Proc, Msg) ->
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

%% Here init_it/6 and enter_loop/5,6,7 functions converge
enter(Module, Opts, CallbackMode, State, Data, Server, Actions, Parent) ->
    %% The values should already have been type checked
    Name = gen:get_proc_name(Server),
    Debug = gen:debug_options(Name, Opts),
    OldState = make_ref(), % Will be discarded by loop_event_actions/9
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
      state => OldState,
      data => Data,
      timer => undefined,
      postponed => [],
      hibernate => false},
    loop_event_actions(
      Parent, Debug, S, [],
      {event,undefined}, % Will be discarded thanks to {postpone,false}
      OldState, State, Data, NewActions).

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
		true
		  when CallbackMode =:= state_functions, is_atom(State);
		       CallbackMode =/= state_functions ->
		    proc_lib:init_ack(Starter, {ok,self()}),
		    enter(
		      Module, Opts, CallbackMode, State, Data,
		      ServerRef, [], Parent);
		false ->
		    Error = {bad_return_value,Result},
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
		    Error = {bad_return_value,Result},
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

system_terminate(Reason, _Parent, Debug, S) ->
    ?TERMINATE(exit, Reason, Debug, S, []).

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
	{ok,{NewState,NewData}} ->
	    {ok,
	     S#{
	       state := NewState,
	       data := NewData}};
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

print_event(Dev, {in,Event}, #{name := Name}) ->
    io:format(
      Dev, "*DBG* ~p received ~s~n",
      [Name,event_string(Event)]);
print_event(Dev, {out,Reply,{To,_Tag}}, #{name := Name}) ->
    io:format(
      Dev, "*DBG* ~p sent ~p to ~p~n",
      [Name,Reply,To]);
print_event(Dev, {Tag,Event,NewState}, #{name := Name, state := State}) ->
    StateString =
	case NewState of
	    State ->
		io_lib:format("~p", [State]);
	    _ ->
		io_lib:format("~p => ~p", [State,NewState])
	end,
    io:format(
      Dev, "*DBG* ~p ~w ~s in state ~s~n",
      [Name,Tag,event_string(Event),StateString]).

event_string(Event) ->
    case Event of
	{{call,{Pid,_Tag}},Request} ->
	    io_lib:format("call ~p from ~w", [Request,Pid]);
	{Tag,Content} ->
	    io_lib:format("~w ~p", [Tag,Content])
    end.

sys_debug(Debug, S, Entry) ->
    case Debug of
	[] ->
	    Debug;
	_ ->
	    sys:handle_debug(Debug, fun print_event/3, S, Entry)
    end.

%%%==========================================================================
%%% Internal callbacks

wakeup_from_hibernate(Parent, Debug, S) ->
    %% It is a new message that woke us up so we have to receive it now
    loop_receive(Parent, Debug, S).

%%%==========================================================================
%%% State Machine engine implementation of proc_lib/gen server

%% Server loop, consists of all loop* functions
%% and some detours through sys and proc_lib

%% Entry point for system_continue/3
loop(Parent, Debug, #{hibernate := Hib} = S) ->
    case Hib of
	true ->
	    loop_hibernate(Parent, Debug, S);
	false ->
	    loop_receive(Parent, Debug, S)
    end.

loop_hibernate(Parent, Debug, S) ->
    %% Does not return but restarts process at
    %% wakeup_from_hibernate/3 that jumps to loop_receive/3
    proc_lib:hibernate(
      ?MODULE, wakeup_from_hibernate, [Parent,Debug,S]),
    error(
      {should_not_have_arrived_here_but_instead_in,
       {wakeup_from_hibernate,3}}).

%% Entry point for wakeup_from_hibernate/3
loop_receive(Parent, Debug, #{timer := Timer} = S) ->
    receive
	Msg ->
	    case Msg of
		{system,Pid,Req} ->
		    %% Does not return but tail recursively calls
		    %% system_continue/3 that jumps to loop/3
		    sys:handle_system_msg(
		      Req, Pid, Parent, ?MODULE, Debug, S,
		      maps:get(hibernate, S));
		{'EXIT',Parent,Reason} = EXIT ->
		    %% EXIT is not a 2-tuple and therefore
		    %% not an event and has no event_type(),
		    %% but this will stand out in the crash report...
		    ?TERMINATE(exit, Reason, Debug, S, [EXIT]);
		{timeout,Timer,Content} when Timer =/= undefined ->
		    loop_receive(
		      Parent, Debug, S, {timeout,Content}, undefined);
		_ ->
		    Event =
			case Msg of
			    {'$gen_call',Caller,Request} ->
				{{call,Caller},Request};
			    {'$gen_cast',E} ->
				{cast,E};
			    _ ->
				{info,Msg}
			end,
		    loop_receive(Parent, Debug, S, Event, Timer)
	    end
    end.

loop_receive(Parent, Debug, S, Event, Timer) ->
    NewDebug = sys_debug(Debug, S, {in,Event}),
    %% Here the queue of not yet processed events is created
    loop_events(Parent, NewDebug, S, [Event], Timer).

%% Process first event in queue, or if there is none receive a new
%%
%% The loop_event* functions optimize S map handling by dismantling it,
%% passing the parts in arguments to avoid map lookups and construct the
%% new S map in one go on exit.  Premature optimization, I know, but
%% there were quite some map lookups repeated in different functions.
loop_events(Parent, Debug, S, [], _Timer) ->
    loop(Parent, Debug, S);
loop_events(
  Parent, Debug,
  #{callback_mode := CallbackMode,
    module := Module,
    state := State,
    data := Data} = S,
  [{Type,Content} = Event|Events] = Q, Timer) ->
    _ = (Timer =/= undefined) andalso
	cancel_timer(Timer),
    try
	case CallbackMode of
	    state_functions ->
		Module:State(Type, Content, Data);
	    handle_event_function ->
		Module:handle_event(Type, Content, State, Data)
	end of
	Result ->
	    loop_event_result(
	      Parent, Debug, S, Events, Event, Result)
    catch
	Result ->
	    loop_event_result(
	      Parent, Debug, S, Events, Event, Result);
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
		      Debug, S, Q);
		[{Module,handle_event,
		  [Type,Content,State,Data]=Args,
		  _}
		 |Stacktrace]
		when CallbackMode =:= handle_event_function ->
		    terminate(
		      error,
		      {undef_state_function,
		       {Module,handle_event,Args}},
		      Stacktrace,
		      Debug, S, Q);
		Stacktrace ->
		    terminate(error, undef, Stacktrace, Debug, S, Q)
	    end;
	Class:Reason ->
	    Stacktrace = erlang:get_stacktrace(),
	    terminate(Class, Reason, Stacktrace, Debug, S, Q)
    end.

%% Interpret all callback return value variants
loop_event_result(
  Parent, Debug,
  #{callback_mode := CallbackMode, state := State, data := Data} = S,
  Events, Event, Result) ->
    case Result of
	stop ->
	    ?TERMINATE(exit, normal, Debug, S, [Event|Events]);
	{stop,Reason} ->
	    ?TERMINATE(exit, Reason, Debug, S, [Event|Events]);
	{stop,Reason,NewData} ->
	    NewS = S#{data := NewData},
	    Q = [Event|Events],
	    ?TERMINATE(exit, Reason, Debug, NewS, Q);
	{stop_and_reply,Reason,Replies} ->
	    Q = [Event|Events],
	    [Class,NewReason,Stacktrace,NewDebug] =
		reply_then_terminate(
		  exit, Reason, ?STACKTRACE(), Debug, S, Q, Replies),
	    %% Since we got back here Replies was bad
	    terminate(Class, NewReason, Stacktrace, NewDebug, S, Q);
	{stop_and_reply,Reason,Replies,NewData} ->
	    NewS = S#{data := NewData},
	    Q = [Event|Events],
	    [Class,NewReason,Stacktrace,NewDebug] =
		reply_then_terminate(
		  exit, Reason, ?STACKTRACE(), Debug, NewS, Q, Replies),
	    %% Since we got back here Replies was bad
	    terminate(Class, NewReason, Stacktrace, NewDebug, NewS, Q);
	{next_state,NewState,NewData}
	  when CallbackMode =:= state_functions, is_atom(NewState);
	       CallbackMode =/= state_functions ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewData, []);
	{next_state,NewState,NewData,Actions}
	  when CallbackMode =:= state_functions, is_atom(NewState);
	       CallbackMode =/= state_functions ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewData, Actions);
	{keep_state,NewData} ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, State, NewData, []);
	{keep_state,NewData,Actions} ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, State, NewData, Actions);
	keep_state_and_data ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, State, Data, []);
	{keep_state_and_data,Actions} ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, State, Data, Actions);
	_ ->
	    ?TERMINATE(
	      error, {bad_return_value,Result}, Debug, S, [Event|Events])
    end.

loop_event_actions(
  Parent, Debug, S, Events, Event, State, NewState, NewData, Actions) ->
    Postpone = false, % Shall we postpone this event, true or false
    Hibernate = false,
    Timeout = undefined,
    NextEvents = [],
    P = false, % The postponed list or false if unchanged
    loop_event_actions(
      Parent, Debug, S, Events, Event, State, NewState, NewData,
      if
	  is_list(Actions) ->
	      Actions;
	  true ->
	      [Actions]
      end,
      Postpone, Hibernate, Timeout, NextEvents, P).
%%
loop_event_actions(
  Parent, Debug, S, Events, Event,
  State, NewState, NewData, [Action|Actions],
  Postpone, Hibernate, Timeout, NextEvents, P) ->
    case Action of
	%% Set options
	postpone ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewData, Actions,
	      true, Hibernate, Timeout, NextEvents, P);
	{postpone,NewPostpone} when is_boolean(NewPostpone) ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewData, Actions,
	      NewPostpone, Hibernate, Timeout, NextEvents, P);
	{postpone,_} ->
	    ?TERMINATE(
	       error, {bad_action,Action}, Debug, S, [Event|Events]);
	hibernate ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewData, Actions,
	      Postpone, true, Timeout, NextEvents, P);
	{hibernate,NewHibernate} when is_boolean(NewHibernate) ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewData, Actions,
	      Postpone, NewHibernate, Timeout, NextEvents, P);
	{hibernate,_} ->
	    ?TERMINATE(
	       error, {bad_action,Action}, Debug, S, [Event|Events]);
	{timeout,infinity,_} -> % Clear timer - it will never trigger
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewData, Actions,
	      Postpone, Hibernate, undefined, NextEvents, P);
	{timeout,Time,_} = NewTimeout when is_integer(Time), Time >= 0 ->
	    loop_event_actions(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewData, Actions,
	      Postpone, Hibernate, NewTimeout, NextEvents, P);
	{timeout,_,_} ->
	    ?TERMINATE(
	       error, {bad_action,Action}, Debug, S, [Event|Events]);
	%% Actual actions
	{reply,Caller,Reply} ->
	    case caller(Caller) of
		true ->
		    NewDebug = do_reply(Debug, S, Caller, Reply),
		    loop_event_actions(
		      Parent, NewDebug, S, Events, Event,
		      State, NewState, NewData, Actions,
		      Postpone, Hibernate, Timeout, NextEvents, P);
		false ->
		    ?TERMINATE(
		       error, {bad_action,Action}, Debug, S, [Event|Events])
	    end;
	{next_event,Type,Content} ->
	    case event_type(Type) of
		true ->
		    loop_event_actions(
		      Parent, Debug, S, Events, Event,
		      State, NewState, NewData, Actions,
		      Postpone, Hibernate, Timeout,
		      [{Type,Content}|NextEvents], P);
		false ->
		    ?TERMINATE(
		       error, {bad_action,Action}, Debug, S, [Event|Events])
	    end;
	_ ->
	    %% All others are remove actions
	    case remove_fun(Action) of
		false ->
		    loop_event_actions(
		      Parent, Debug, S, Events, Event,
		      State, NewState, NewData, Actions,
		      Postpone, Hibernate, Timeout, NextEvents, P);
		undefined ->
		    ?TERMINATE(
		       error, {bad_action,Action}, Debug, S, [Event|Events]);
		RemoveFun when is_function(RemoveFun, 2) ->
		    P0 =
			case P of
			    false ->
				maps:get(postponed, S);
			    _ ->
				P
			end,
		    case remove_event(RemoveFun, Events, P0) of
			false ->
			    loop_event_actions(
			      Parent, Debug, S, Events, Event,
			      State, NewState, NewData, Actions,
			      Postpone, Hibernate, Timeout, NextEvents, P);
			{NewEvents,false} ->
			    loop_event_actions(
			      Parent, Debug, S, NewEvents, Event,
			      State, NewState, NewData, Actions,
			      Postpone, Hibernate, Timeout, NextEvents, P);
			{false,NewP} ->
			    loop_event_actions(
			      Parent, Debug, S, Events, Event,
			      State, NewState, NewData, Actions,
			      Postpone, Hibernate, Timeout, NextEvents,
			      NewP);
			[Class,Reason,Stacktrace] ->
			    terminate(
			      Class, Reason, Stacktrace,
			      Debug, S, [Event|Events])
		    end;
		[Class,Reason,Stacktrace] ->
		    terminate(
		      Class, Reason, Stacktrace, Debug, S, [Event|Events])
	    end
    end;
loop_event_actions(
  Parent, Debug, S, Events, Event, State, NewState, NewData, [],
  Postpone, Hibernate, Timeout, NextEvents, P) ->
    P0 =
	case P of
	    false ->
		maps:get(postponed, S);
	    _ ->
		P
	end,
    P1 = % Move current event to postponed if Postpone
	case Postpone of
	    true ->
		[Event|P0];
	    false ->
		P0
	end,
    {Timer,Q1} =
	case Timeout of
	    undefined ->
		{undefined,Events};
	    {timeout,0,Msg} ->
		%% Pretend the timeout has just been received
		{undefined,Events ++ [{timeout,Msg}]};
	    {timeout,Time,Msg} ->
		{erlang:start_timer(Time, self(), Msg),
		Events}
	end,
    {Q2,P2} = % Move all postponed events to queue if state change
	if
	    NewState =:= State ->
		{Q1,P1};
	    true ->
		{lists:reverse(P1, Q1),[]}
	end,
    %% Place next events first in queue
    Q3 = lists:reverse(NextEvents, Q2),
    %%
    NewDebug =
	sys_debug(
	  Debug, S,
	  case Postpone of
	      true ->
		  {postpone,Event,NewState};
	      false ->
		  {consume,Event,NewState}
	  end),
    %% Loop to top; process next event
    loop_events(
      Parent, NewDebug,
      S#{
	state := NewState,
	data := NewData,
	timer := Timer,
	hibernate := Hibernate,
	postponed := P2},
      Q3, Timer).

%%---------------------------------------------------------------------------
%% Server helpers

reply_then_terminate(Class, Reason, Stacktrace, Debug, S, Q, Replies) ->
    if
	is_list(Replies) ->
	    do_reply_then_terminate(
	      Class, Reason, Stacktrace, Debug, S, Q, Replies);
	true ->
	    do_reply_then_terminate(
	      Class, Reason, Stacktrace, Debug, S, Q, [Replies])
    end.
%%
do_reply_then_terminate(Class, Reason, Stacktrace, Debug, S, Q, []) ->
    terminate(Class, Reason, Stacktrace, Debug, S, Q);
do_reply_then_terminate(
  Class, Reason, Stacktrace, Debug, S, Q, [R|Rs]) ->
    case R of
	{reply,{_To,_Tag}=Caller,Reply} ->
	    NewDebug = do_reply(Debug, S, Caller, Reply),
	    do_reply_then_terminate(
	      Class, Reason, Stacktrace, NewDebug, S, Q, Rs);
	_ ->
	    [error,{bad_action,R},?STACKTRACE(),Debug]
    end.

do_reply(Debug, S, Caller, Reply) ->
    reply(Caller, Reply),
    sys_debug(Debug, S, {out,Reply,Caller}).


%% Remove oldest matching event from the queue(s)
remove_event(RemoveFun, Q, P) ->
    try
	case remove_tail_event(RemoveFun, P) of
	    false ->
		case remove_head_event(RemoveFun, Q) of
		    false ->
			false;
		    NewQ ->
			{false,NewQ}
		end;
	    NewP ->
		{NewP,false}
	end
    catch
	Class:Reason ->
	    [Class,Reason,erlang:get_stacktrace()]
    end.

%% Do the given action and create an event removal predicate fun()
remove_fun({remove_event,Type,Content}) ->
    fun (T, C) when T =:= Type, C =:= Content -> true;
	(_, _) -> false
    end;
remove_fun({remove_event,RemoveFun}) when is_function(RemoveFun, 2) ->
    RemoveFun;
remove_fun({cancel_timer,TimerRef}) ->
    try cancel_timer(TimerRef) of
	false ->
	    false;
	true ->
	    fun
		(info, {timeout,TRef,_})
		  when TRef =:= TimerRef ->
		    true;
		(_, _) ->
		    false
	    end
    catch
	Class:Reason ->
	    [Class,Reason,erlang:get_stacktrace()]
    end;
remove_fun({demonitor,MonitorRef}) ->
    try erlang:demonitor(MonitorRef, [flush,info]) of
	false ->
	    false;
	true ->
	    fun (info, {'DOWN',MRef,_,_,_})
		  when MRef =:= MonitorRef->
		    true;
		(_, _) ->
		    false
	    end
    catch
	Class:Reason ->
	    [Class,Reason,erlang:get_stacktrace()]
    end;
remove_fun({unlink,Id}) ->
    try unlink(Id) of
	true ->
	    receive
		{'EXIT',Id,_} ->
		    ok
	    after 0 ->
		    ok
	    end,
	    fun (info, {'EXIT',I,_})
		  when I =:= Id ->
		    true;
		(_, _) ->
		    false
	    end
    catch
	Class:Reason ->
	    [Class,Reason,erlang:get_stacktrace()]
    end;
remove_fun(_) ->
    undefined.


%% Cancel a timer and clense the process mailbox returning
%% false if no such timer message can arrive after this or
%% true otherwise
cancel_timer(TimerRef) ->
    case erlang:cancel_timer(TimerRef) of
	TimeLeft when is_integer(TimeLeft) ->
	    false;
	false ->
	    receive
		{timeout,TimerRef,_} ->
		    false
	    after 0 ->
		    true
	    end
    end.


terminate(
  Class, Reason, Stacktrace, Debug,
  #{module := Module,
    state := State, data := Data} = S,
  Q) ->
    try Module:terminate(Reason, State, Data) of
	_ -> ok
    catch
	_ -> ok;
	C:R ->
	    ST = erlang:get_stacktrace(),
	    error_info(
	      C, R, ST, Debug, S, Q,
	      format_status(terminate, get(), S)),
	    erlang:raise(C, R, ST)
    end,
    case Reason of
	normal -> ok;
	shutdown -> ok;
	{shutdown,_} -> ok;
	_ ->
	    error_info(
	      Class, Reason, Stacktrace, Debug, S, Q,
	      format_status(terminate, get(), S))
    end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	_ ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

error_info(
  Class, Reason, Stacktrace, Debug,
  #{name := Name, callback_mode := CallbackMode,
    state := State, postponed := P},
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
    error_logger:format(
      "** State machine ~p terminating~n" ++
	  case Q of
	      [] ->
		  "";
	      _ ->
		  "** Last event = ~p~n"
	  end ++
	  "** When Server state  = ~p~n" ++
	  "** Reason for termination = ~w:~p~n" ++
	  "** State = ~p~n" ++
	  "** Callback mode = ~p~n" ++
	  "** Queued/Posponed = ~w/~w~n" ++
	  case FixedStacktrace of
	      [] ->
		  "";
	      _ ->
		  "** Stacktrace =~n"
		      "**  ~p~n"
	  end,
      [Name |
       case Q of
	   [] ->
	       [];
	   [Event|_] ->
	       [Event]
       end] ++
	  [FmtData,Class,FixedReason,
	   State,CallbackMode,length(Q),length(P)] ++
	  case FixedStacktrace of
	      [] ->
		  [];
	      _ ->
		  [FixedStacktrace]
	  end),
    sys:print_log(Debug),
    ok.


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

%%---------------------------------------------------------------------------
%% Farily general helpers

%% Return the modified list where the first element that satisfies
%% the RemoveFun predicate is removed, or false if no such element exists.
remove_head_event(_RemoveFun, []) ->
    false;
remove_head_event(RemoveFun, [{Tag,Content}|Events]) ->
    case RemoveFun(Tag, Content) of
	false ->
	    remove_head_event(RemoveFun, Events);
	true ->
	    Events
    end.

%% Return the modified list where the last element that satisfies
%% the RemoveFun predicate is removed, or false if no such element exists.
remove_tail_event(_RemoveFun, []) ->
    false;
remove_tail_event(RemoveFun, [{Tag,Content} = Event|Events]) ->
    case remove_tail_event(RemoveFun, Events) of
	false ->
	    RemoveFun(Tag, Content) andalso Events;
	NewEvents ->
	    [Event|NewEvents]
    end.
