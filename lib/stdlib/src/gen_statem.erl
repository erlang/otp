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

%% Fix problem for doc build
-export_type([state_callback_result/0]).

%%%==========================================================================
%%% Interface functions.
%%%==========================================================================

-type client() ::
	{To :: pid(), Tag :: term()}. % Reply-to specifier for call
-type state() ::
	state_name() | % For state callback function StateName/5
	term(). % For state callback function handle_event/5
-type state_name() :: atom().
-type state_data() :: term().
-type event_type() ::
	{'call',Client :: client()} | 'cast' |
	'info' | 'timeout' | 'internal'.
-type event_predicate() :: % Return true for the event in question
	fun((event_type(), term()) -> boolean()).
-type init_option() ::
	{'callback_mode', callback_mode()}.
-type callback_mode() :: 'state_functions' | 'handle_event_function'.
-type state_op() ::
	%% First NewState and NewStateData are set,
	%% then all state_operations() are executed in order of
	%% apperance. Postponing the current event is performed
	%% (iff state_option() 'postpone' is 'true').
	%% Lastly pending events are processed or if there are
	%% no pending events the server goes into receive
	%% or hibernate (iff state_option() 'hibernate' is 'true')
	state_option() | state_operation().
-type state_option() ::
	%% The first of each kind in the state_op() list takes precedence
	'postpone' |  % Postpone the current event to a different (=/=) state
	{'postpone', Postpone :: boolean()} |
	'hibernate' | % Hibernate the server instead of going into receive
	{'hibernate', Hibernate :: boolean()} |
	(Timeout :: timeout()) | % {timeout,Timeout}
	{'timeout', % Generate a ('timeout', Msg, ...) event after Time
	 Time :: timeout(), Msg :: term()}.
-type state_operation() ::
	%% These can occur multiple times and are executed in order
	%% of appearence in the state_op() list
	reply_operation() |
	{'next_event', % Insert event as the next to handle
	 EventType :: event_type(),
	 EventContent :: term()} |
	{'remove_event', % Remove the oldest matching (=:=) event
	 EventType :: event_type(), EventContent :: term()} |
	{'remove_event', % Remove the oldest event satisfying predicate
	 EventPredicate :: event_predicate()} |
	{'cancel_timer', % Cancel timer and clean up mess(ages)
	 TimerRef :: reference()} |
	{'demonitor', % Demonitor and clean up mess(ages)
	 MonitorRef :: reference()} |
	{'unlink', % Unlink and clean up mess(ages)
	 Id :: pid() | port()}.
-type reply_operation() ::
	{'reply', % Reply to a client
	 Client :: client(), Reply :: term()}.
-type state_callback_result() ::
    {'stop', % Stop the server
     Reason :: term()} |
    {'stop', % Stop the server
     Reason :: term(),
     NewStateData :: state_data()} |
    {'stop', % Stop the server
     Reason :: term(),
     Replies :: [reply_operation()] | reply_operation(),
     NewStateData :: state_data()} |
    {'next_state', % {next_state,NewState,NewStateData,[]}
     NewState :: state(),
     NewStateData :: state_data()} |
    {'next_state', % State transition, maybe to the same state
     NewState :: state(),
     NewStateData :: state_data(),
     StateOps :: [state_op()] | state_op()} |
    {'keep_state', % {keep_state,NewStateData,[]}
     NewStateData :: state_data()} |
    {'keep_state',
     NewStateData :: state_data(),
     StateOps :: [state_op()] | state_op()} |
    {'keep_state_and_data'} | % {keep_state_and_data,[]}
    {'keep_state_and_data',
     StateOps :: [state_op()] | state_op()}.

%% The state machine init function.  It is called only once and
%% the server is not running until this function has returned
%% an {ok, ...} tuple.  Thereafter the state callbacks are called
%% for all events to this server.
-callback init(Args :: term()) ->
    {'ok', state(), state_data()} |
    {'ok', state(), state_data(), [state_op()|init_option()]} |
    'ignore' |
    {'stop', Reason :: term()}.

%% Example callback for callback_mode =:= state_functions
%% state name 'state_name'.
%%
%% In this mode all states has to be type state_name() i.e atom().
%%
%% Note that state callbacks and only state callbacks have arity 5
%% and that is intended.
-callback state_name(
	    event_type(),
	    EventContent :: term(),
	    PrevStateName :: state_name() | reference(),
	    StateName :: state_name(), % Current state
	    StateData :: state_data()) ->
    state_callback_result().
%%
%% Callback for callback_mode =:= handle_event_function.
%%
%% Note that state callbacks and only state callbacks have arity 5
%% and that is intended.
-callback handle_event(
	    event_type(),
	    EventContent :: term(),
	    PrevState :: state(),
	    State :: state(), % Current state
	    StateData :: state_data()) ->
    state_callback_result().

%% Clean up before the server terminates.
-callback terminate(
	    Reason :: 'normal' | 'shutdown' | {'shutdown', term()}
		    | term(),
	    State :: state(),
	    StateData :: state_data()) ->
    any().

%% Note that the new code can expect to get an OldState from
%% the old code version not only in code_change/4 but in the first
%% state callback function called thereafter
-callback code_change(
	    OldVsn :: term() | {'down', term()},
	    OldState :: state(),
	    OldStateData :: state_data(),
	    Extra :: term()) ->
    {ok, {NewState :: state(), NewStateData :: state_data()}}.

%% Format the callback module state in some sensible that is
%% often condensed way.  For StatusOption =:= 'normal' the perferred
%% return term is [{data,[{"State",FormattedState}]}], and for
%% StatusOption =:= 'terminate' it is just FormattedState.
-callback format_status(
	    StatusOption,
	    [ [{Key :: term(), Value :: term()}] |
	      state() |
	      state_data()]) ->
    Status :: term() when
      StatusOption :: 'normal' | 'terminate'.

-optional_callbacks(
   [format_status/2, % Has got a default implementation
    %%
    state_name/5, % Example for callback_mode =:= state_functions:
    %% there has to be a StateName/5 callback function for every StateName.
    %%
    handle_event/5]). % For callback_mode =:= handle_event_function

%% Type validation functions
client({Pid,Tag}) when is_pid(Pid), is_reference(Tag) ->
    true;
client(_) ->
    false.
%%
event_type({call,Client}) ->
    client(Client);
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
   TERMINATE(Reason, Debug, S, Q),
   terminate(
     exit,
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
      | {'spawn_opt', SOpts :: [proc_lib:spawn_option()]}.
-type start_ret() ::  {'ok', pid()} | 'ignore' | {'error', term()}.



%% Start a state machine
-spec start(
	Module :: module(), Args :: term(), Options :: [start_opt()]) ->
		   start_ret().
start(Module, Args, Options) ->
    gen:start(?MODULE, nolink, Module, Args, Options).
%%
-spec start(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Options :: [start_opt()]) ->
		   start_ret().
start(ServerName, Module, Args, Options) ->
    gen:start(?MODULE, nolink, ServerName, Module, Args, Options).

%% Start and link to a state machine
-spec start_link(
	Module :: module(), Args :: term(), Options :: [start_opt()]) ->
		   start_ret().
start_link(Module, Args, Options) ->
    gen:start(?MODULE, link, Module, Args, Options).
%%
-spec start_link(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Options :: [start_opt()]) ->
		   start_ret().
start_link(ServerName, Module, Args, Options) ->
    gen:start(?MODULE, link, ServerName, Module, Args, Options).

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
%% arrives with type {call,Client}
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
-spec reply(ReplyOperation :: reply_operation()) -> ok.
reply({reply,{_To,_Tag}=Client,Reply}) ->
    reply(Client, Reply).
%%
-spec reply(Client :: client(), Reply :: term()) -> ok.
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
	Module :: module(), Options :: [debug_opt()],
	State :: state(), StateData :: state_data()) ->
			no_return().
enter_loop(Module, Options, State, StateData) ->
    enter_loop(Module, Options, State, StateData, self()).
%%
-spec enter_loop(
	Module :: module(), Options :: [debug_opt()],
	State :: state(), StateData :: state_data(),
	Server_or_StateOps ::
	  server_name() | pid() | [state_op()|init_option()]) ->
			no_return().
enter_loop(Module, Options, State, StateData, Server_or_StateOps) ->
    if
	is_list(Server_or_StateOps) ->
	    enter_loop(
	      Module, Options, State, StateData,
	      self(), Server_or_StateOps);
	true ->
	    enter_loop(
	      Module, Options, State, StateData,
	      Server_or_StateOps, [])
    end.
%%
-spec enter_loop(
	Module :: module(), Options :: [debug_opt()],
	State :: state(), StateData :: state_data(),
	Server :: server_name() | pid(),
	StateOps :: [state_op()|init_option()]) ->
			no_return().
enter_loop(Module, Options, State, StateData, Server, StateOps) ->
    Parent = gen:get_parent(),
    enter(Module, Options, State, StateData, Server, StateOps, Parent).

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

%% Here init_it and all enter_loop functions converge
enter(Module, Options, State, StateData, Server, InitOps, Parent) ->
    Name = gen:get_proc_name(Server),
    Debug = gen:debug_options(Name, Options),
    PrevState = undefined,
    S = #{
      callback_mode => state_functions,
      module => Module,
      name => Name,
      prev_state => PrevState,
      state => PrevState, % Will be discarded by loop_event_state_ops
      state_data => StateData,
      timer => undefined,
      postponed => [],
      hibernate => false},
    case collect_init_options(InitOps) of
	{CallbackMode,StateOps} ->
	    loop_event_state_ops(
	      Parent, Debug,
	      S#{callback_mode := CallbackMode},
	      [],
	      {event,undefined}, % Will be discarded by {postpone,false}
	      PrevState, State, StateData,
	      StateOps++[{postpone,false}]);
	[Reason] ->
	    ?TERMINATE(Reason, Debug, S, [])
    end.

%%%==========================================================================
%%%  gen callbacks

init_it(Starter, Parent, ServerRef, Module, Args, Options) ->
    try Module:init(Args) of
	Result ->
	    init_result(Starter, Parent, ServerRef, Module, Result, Options)
    catch
	Result ->
	    init_result(Starter, Parent, ServerRef, Module, Result, Options);
	Class:Reason ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.

%%---------------------------------------------------------------------------
%% gen callbacks helpers

init_result(Starter, Parent, ServerRef, Module, Result, Options) ->
    case Result of
	{ok,State,StateData} ->
	    proc_lib:init_ack(Starter, {ok,self()}),
	    enter(
	      Module, Options, State, StateData, ServerRef,
	      [], Parent);
	{ok,State,StateData,StateOps} ->
	    proc_lib:init_ack(Starter, {ok,self()}),
	    enter(
	      Module, Options, State, StateData, ServerRef,
	      StateOps, Parent);
	{stop,Reason} ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    exit(Reason);
	ignore ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	Other ->
	    Error = {bad_return_value,Other},
	    proc_lib:init_ack(Starter, {error,Error}),
	    exit(Error)
    end.

%%%==========================================================================
%%% sys callbacks

system_continue(Parent, Debug, S) ->
    loop(Parent, Debug, S).

system_terminate(Reason, _Parent, Debug, S) ->
    ?TERMINATE(Reason, Debug, S, []).

system_code_change(
  #{module := Module,
    state := State,
    state_data := StateData} = S,
  _Mod, OldVsn, Extra) ->
    case
	try Module:code_change(OldVsn, State, StateData, Extra)
	catch
	    Result -> Result
	end
    of
	{ok,{NewState,NewStateData}} ->
	    {ok,
	     S#{
	       state := NewState,
	       state_data := NewStateData}};
	Error ->
	    Error
    end.

system_get_state(#{state := State, state_data := StateData}) ->
    {ok,{State,StateData}}.

system_replace_state(
  StateFun,
  #{state := State,
    state_data := StateData} = S) ->
    {NewState,NewStateData} = Result = StateFun({State,StateData}),
    {ok,Result,S#{state := NewState, state_data := NewStateData}}.

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
%%% STate Machine engine implementation of proc_lib/gen server

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
		    ?TERMINATE(Reason, Debug, S, [EXIT]);
		{timeout,Timer,Content} when Timer =/= undefined ->
		    loop_receive(
		      Parent, Debug, S, {timeout,Content}, undefined);
		_ ->
		    Event =
			case Msg of
			    {'$gen_call',Client,Request} ->
				{{call,Client},Request};
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
    prev_state := PrevState,
    state := State,
    state_data := StateData} = S,
  [{Type,Content} = Event|Events] = Q, Timer) ->
    _ = (Timer =/= undefined) andalso
	cancel_timer(Timer),
    Func =
	case CallbackMode of
	    handle_event_function ->
		handle_event;
	    state_functions ->
		State
	end,
    try Module:Func(Type, Content, PrevState, State, StateData) of
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
		[{Module,Func,
		  [Type,Content,PrevState,State,StateData]=Args,
		  _}
		 |Stacktrace] ->
		    terminate(
		      error,
		      {undef_state_function,{Module,State,Args}},
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
  #{state := State, state_data := StateData} = S,
  Events, Event, Result) ->
    case Result of
	{stop,Reason} ->
	    ?TERMINATE(Reason, Debug, S, [Event|Events]);
	{stop,Reason,NewStateData} ->
	    ?TERMINATE(
	      Reason, Debug,
	      S#{state_data := NewStateData},
	      [Event|Events]);
	{stop,Reason,Reply,NewStateData} ->
	    NewS = S#{state_data := NewStateData},
	    Q = [Event|Events],
	    Replies =
		if
		    is_list(Reply) ->
			Reply;
		    true ->
			[Reply]
		end,
	    BadReplies =
		reply_then_terminate(
		  exit, Reason, ?STACKTRACE(), Debug, NewS, Q, Replies),
	    %% Since we got back here Replies was bad
	    ?TERMINATE(
	      {bad_return_value,{stop,Reason,BadReplies,NewStateData}},
	      Debug, NewS, Q);
	{next_state,NewState,NewStateData} ->
	    loop_event_state_ops(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewStateData, []);
	{next_state,NewState,NewStateData,StateOps}
	  when is_list(StateOps) ->
	    loop_event_state_ops(
	      Parent, Debug, S, Events, Event,
	      State, NewState, NewStateData, StateOps);
	{keep_state,NewStateData} ->
	    loop_event_state_ops(
	      Parent, Debug, S, Events, Event,
	      State, State, NewStateData, []);
	{keep_state,NewStateData,StateOps} ->
	    loop_event_state_ops(
	      Parent, Debug, S, Events, Event,
	      State, State, NewStateData, StateOps);
	{keep_state_and_data} ->
	    loop_event_state_ops(
	      Parent, Debug, S, Events, Event,
	      State, State, StateData, []);
	{keep_state_and_data,StateOps} ->
	    loop_event_state_ops(
	      Parent, Debug, S, Events, Event,
	      State, State, StateData, StateOps);
	_ ->
	    ?TERMINATE(
	      {bad_return_value,Result}, Debug, S, [Event|Events])
    end.

loop_event_state_ops(
  Parent, Debug0, #{postponed := P0} = S, Events, Event,
  State, NewState, NewStateData, StateOps) ->
    case collect_state_options(StateOps) of
	{Postpone,Hibernate,Timeout,Operations} ->
	    P1 = % Move current event to postponed if Postpone
		case Postpone of
		    true ->
			[Event|P0];
		    false ->
			P0
		end,
	    {Q2,P2} = % Move all postponed events to queue if state change
		if
		    NewState =:= State ->
			{Events,P1};
		    true ->
			{lists:reverse(P1, Events),[]}
		end,
	    %%
	    case process_state_operations(
		   Operations, Debug0, S, Q2, P2) of
		{Debug,Q3,P} ->
		    NewDebug =
			sys_debug(
			  Debug, S,
			  case Postpone of
			      true ->
				  {postpone,Event,NewState};
			      false ->
				  {consume,Event,NewState}
			  end),
		    {Timer,Q} =
			case Timeout of
			    undefined ->
				{undefined,Q3};
			    {timeout,0,Msg} ->
				%% Pretend the timeout has just been received
				{undefined,Q3 ++ [{timeout,Msg}]};
			    {timeout,Time,Msg} ->
				{erlang:start_timer(Time, self(), Msg),
				 Q3}
			end,
		    loop_events(
		      Parent, NewDebug,
		      S#{
			prev_state := State,
			state := NewState,
			state_data := NewStateData,
			timer := Timer,
			hibernate := Hibernate,
			postponed := P},
		      Q, Timer);
		[Reason,Debug] ->
		    ?TERMINATE(Reason, Debug, S, [Event|Events]);
		[Class,Reason,Stacktrace,Debug] ->
		    terminate(
		      Class, Reason, Stacktrace, Debug, S, [Event|Events])
	    end;
	%%
	[Reason] ->
	    ?TERMINATE(Reason, Debug0, S, [Event|Events])
    end.

%%---------------------------------------------------------------------------
%% Server helpers

collect_init_options(InitOps) ->
    if
	is_list(InitOps) ->
	    collect_init_options(InitOps, state_functions, []);
	true ->
	    collect_init_options([InitOps], state_functions, [])
    end.
%% Keep the last of each kind
collect_init_options([], CallbackMode, StateOps) ->
    {CallbackMode,lists:reverse(StateOps)};
collect_init_options([InitOp|InitOps] = IOIOs, CallbackMode, StateOps) ->
    case InitOp of
	{callback_mode,Mode}
	  when Mode =:= state_functions;
	       Mode =:= handle_event_function ->
	    collect_init_options(InitOps, Mode, StateOps);
	{callback_mode,_} ->
	    [{bad_init_ops,IOIOs}];
	_ -> % Collect others as StateOps
	    collect_init_options(
	      InitOps, CallbackMode, [InitOp|StateOps])
    end.

collect_state_options(StateOps) ->
    if
	is_list(StateOps) ->
	    collect_state_options(StateOps, false, false, undefined, []);
	true ->
	    collect_state_options([StateOps], false, false, undefined, [])
    end.
%% Keep the last of each kind
collect_state_options(
  [], Postpone, Hibernate, Timeout, Operations) ->
    {Postpone,Hibernate,Timeout,lists:reverse(Operations)};
collect_state_options(
  [StateOp|StateOps] = SOSOs, Postpone, Hibernate, Timeout, Operations) ->
    case StateOp of
	postpone ->
	    collect_state_options(
	      StateOps, true, Hibernate, Timeout, Operations);
	{postpone,NewPostpone} when is_boolean(NewPostpone) ->
	    collect_state_options(
	      StateOps, NewPostpone, Hibernate, Timeout, Operations);
	{postpone,_} ->
	    [{bad_state_ops,SOSOs}];
	hibernate ->
	    collect_state_options(
	      StateOps, Postpone, true, Timeout, Operations);
	{hibernate,NewHibernate} when is_boolean(NewHibernate) ->
	    collect_state_options(
	      StateOps, Postpone, NewHibernate, Timeout, Operations);
	{hibernate,_} ->
	    [{bad_state_ops,SOSOs}];
	{timeout,infinity,_} -> % Ignore since it will never time out
	    collect_state_options(
	      StateOps, Postpone, Hibernate, undefined, Operations);
	{timeout,Time,_} = NewTimeout when is_integer(Time), Time >= 0 ->
	    collect_state_options(
	      StateOps, Postpone, Hibernate, NewTimeout, Operations);
	{timeout,_,_} ->
	    [{bad_state_ops,SOSOs}];
	_ -> % Collect others as operations
	    collect_state_options(
	      StateOps, Postpone, Hibernate, Timeout, [StateOp|Operations])
    end.

process_state_operations([], Debug, _S, Q, P) ->
    {Debug,Q,P};
process_state_operations([Operation|Operations] = OOs, Debug, S, Q, P) ->
    case Operation of
	{reply,{_To,_Tag}=Client,Reply} ->
	    NewDebug = do_reply(Debug, S, Client, Reply),
	    process_state_operations(Operations, NewDebug, S, Q, P);
	{next_event,Type,Content} ->
	    case event_type(Type) of
		true ->
		    process_state_operations(
		      Operations, Debug, S, [{Type,Content}|Q], P);
		false ->
		    [{bad_state_ops,OOs},Debug]
	    end;
	_ ->
	    %% All others are remove operations
	    case remove_fun(Operation) of
		false ->
		    process_state_operations(
		      Operations, Debug, S, Q, P);
		undefined ->
		    [{bad_state_ops,OOs},Debug];
		RemoveFun when is_function(RemoveFun, 2) ->
		    case remove_event(RemoveFun, Q, P) of
			{NewQ,NewP} ->
			    process_state_operations(
			      Operations, Debug, S, NewQ, NewP);
			Error ->
			    Error ++ [Debug]
		    end;
		Error ->
		    Error ++ [Debug]
	    end
    end.

reply_then_terminate(Class, Reason, Stacktrace, Debug, S, Q, []) ->
    terminate(Class, Reason, Stacktrace, Debug, S, Q);
reply_then_terminate(
  Class, Reason, Stacktrace, Debug, S, Q, [R|Rs] = RRs) ->
    case R of
	{reply,{_To,_Tag}=Client,Reply} ->
	    NewDebug = do_reply(Debug, S, Client, Reply),
	    reply_then_terminate(
	      Class, Reason, Stacktrace, NewDebug, S, Q, Rs);
	_ ->
	    RRs % bad_return_value
    end.

do_reply(Debug, S, Client, Reply) ->
    reply(Client, Reply),
    sys_debug(Debug, S, {out,Reply,Client}).


%% Remove oldest matching event from the queue(s)
remove_event(RemoveFun, Q, P) ->
    try
	case remove_tail_event(RemoveFun, P) of
	    false ->
		case remove_head_event(RemoveFun, Q) of
		    false ->
			{P,Q};
		    NewQ ->
			{P,NewQ}
		end;
	    NewP ->
		{NewP,Q}
	end
    catch
	Class:Reason ->
	    [Class,Reason,erlang:get_stacktrace()]
    end.

%% Do the given state operation and create an event removal predicate fun()
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
    state := State, state_data := StateData} = S,
  Q) ->
    try Module:terminate(Reason, State, StateData) of
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
  Q, FmtStateData) ->
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
	  [FmtStateData,Class,FixedReason,
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
  #{module := Module, state := State, state_data := StateData}) ->
    case erlang:function_exported(Module, format_status, 2) of
	true ->
	    try Module:format_status(Opt, [PDict,State,StateData])
	    catch
		Result -> Result;
		_:_ ->
		    format_status_default(Opt, State, StateData)
	    end;
	false ->
	    format_status_default(Opt, State, StateData)
    end.

%% The default Module:format_status/2
format_status_default(Opt, State, StateData) ->
    SSD = {State,StateData},
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
