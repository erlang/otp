%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2019. All Rights Reserved.
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

-include("logger.hrl").

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

%% logger callback
-export([format_log/1]).

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

%% Type exports for start_link & friends
-export_type(
   [server_name/0,
    server_ref/0,
    start_opt/0,
    start_ret/0,
    enter_loop_opt/0]).

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
	(Time :: event_timeout()) | % {timeout,Time,Time}
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
        timeout_cancel_action() |
        timeout_update_action().
-type timeout_cancel_action() ::
        {'timeout', 'cancel'} |
        {{'timeout', Name :: term()}, 'cancel'} |
        {'state_timeout', 'cancel'}.
-type timeout_update_action() ::
        {'timeout', 'update', EventContent :: term()} |
        {{'timeout', Name :: term()}, 'update', EventContent :: term()} |
        {'state_timeout', 'update', EventContent :: term()}.
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
%% - return true if the value is of the type, false otherwise
-compile(
   {inline,
    [callback_mode/1, state_enter/1,
     event_type/1, from/1, timeout_event_type/1]}).
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
event_type(Type) ->
    case Type of
	{call,From} -> from(From);
        %%
	cast -> true;
	info -> true;
	internal -> true;
        _ -> timeout_event_type(Type)
    end.
%%
from({Pid,_}) when is_pid(Pid) -> true;
from(_) -> false.
%%
timeout_event_type(Type) ->
    case Type of
        timeout -> true;
        state_timeout -> true;
        {timeout,_Name} -> true;
        _ -> false
    end.


-define(
   relative_timeout(T),
   ((is_integer(T) andalso 0 =< (T)) orelse (T) =:= infinity)).

-define(
   absolute_timeout(T),
   (is_integer(T) orelse (T) =:= infinity)).

-define(
   STACKTRACE(),
   element(2, erlang:process_info(self(), current_stacktrace))).

-define(not_sys_debug, []).
%%
%% This is a macro to only evaluate arguments if Debug =/= [].
%% Debug is evaluated 2 times.
-define(
   sys_debug(Debug, Extra, SystemEvent),
   case begin Debug end of
       ?not_sys_debug ->
           begin Debug end;
       _ ->
           sys_debug(
             begin Debug end, begin Extra end, begin SystemEvent end)
    end).

-record(params,
        {callback_mode = undefined :: callback_mode() | undefined,
         state_enter = false :: boolean(),
         parent :: pid(),
         module :: atom(),
         name :: atom() | pid(),
         hibernate_after = infinity :: timeout()
        }).

-record(state,
        {state_data = {undefined,undefined} ::
           {State :: term(),Data :: term()},
         postponed = [] :: [{event_type(),term()}],
         timers = #{} ::
           #{TimeoutType :: timeout_event_type() =>
                            {TimerRef :: reference(), TimeoutMsg :: term()}},
          hibernate = false :: boolean()
        }).

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
-type start_opt() ::
        {'timeout', Time :: timeout()}
      | {'spawn_opt', [proc_lib:spawn_option()]}
      | enter_loop_opt().
-type start_ret() ::
        {'ok', pid()}
      | 'ignore'
      | {'error', term()}.
-type enter_loop_opt() ::
	{'hibernate_after', HibernateAfterTimeout :: timeout()}
      | {'debug', Dbgs :: [sys:debug_option()]}.



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
	Module :: module(), Opts :: [enter_loop_opt()],
	State :: state(), Data :: data()) ->
			no_return().
enter_loop(Module, Opts, State, Data) ->
    enter_loop(Module, Opts, State, Data, self()).
%%
-spec enter_loop(
	Module :: module(), Opts :: [enter_loop_opt()],
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
	Module :: module(), Opts :: [enter_loop_opt()],
	State :: state(), Data :: data(),
	Server :: server_name() | pid(),
	Actions :: [action()] | action()) ->
			no_return().
enter_loop(Module, Opts, State, Data, Server, Actions) ->
    is_atom(Module) orelse error({atom,Module}),
    Parent = gen:get_parent(),
    Name = gen:get_proc_name(Server),
    Debug = gen:debug_options(Name, Opts),
    HibernateAfterTimeout = gen:hibernate_after(Opts),
    enter(
      Parent, Debug, Module, Name, HibernateAfterTimeout,
      State, Data, Actions).

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
enter(
  Parent, Debug, Module, Name, HibernateAfterTimeout,
  State, Data, Actions) ->
    %% The values should already have been type checked
    Q = [{internal,init_state}],
    %% We enforce {postpone,false} to ensure that
    %% our fake Event gets discarded, thought it might get logged
    Actions_1 = listify(Actions) ++ [{postpone,false}],
    P =
        #params{
           parent = Parent,
           module = Module,
           name = Name,
           hibernate_after = HibernateAfterTimeout},
    S = #state{state_data = {State,Data}},
    Debug_1 = ?sys_debug(Debug, Name, {enter,State}),
    loop_callback_mode(
      P, Debug_1, S, Q, {State,Data},
      %% Tunneling Actions through CallbackEvent here...
      %% Special path to go to action handling, after first
      %% finding out the callback mode.  CallbackEvent is
      %% a 2-tuple and Actions a list, which achieves this distinction.
      Actions_1).

%%%==========================================================================
%%%  gen callbacks

init_it(Starter, self, ServerRef, Module, Args, Opts) ->
    init_it(Starter, self(), ServerRef, Module, Args, Opts);
init_it(Starter, Parent, ServerRef, Module, Args, Opts) ->
    Name = gen:get_proc_name(ServerRef),
    Debug = gen:debug_options(Name, Opts),
    HibernateAfterTimeout = gen:hibernate_after(Opts),
    try Module:init(Args) of
	Result ->
	    init_result(
              Starter, Parent, ServerRef, Module, Result,
              Name, Debug, HibernateAfterTimeout)
    catch
	Result ->
	    init_result(
              Starter, Parent, ServerRef, Module, Result,
              Name, Debug, HibernateAfterTimeout);
	Class:Reason:Stacktrace ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    error_info(
	      Class, Reason, Stacktrace, Debug,
              #params{parent = Parent, name = Name, module = Module},
              #state{}, []),
	    erlang:raise(Class, Reason, Stacktrace)
    end.

%%---------------------------------------------------------------------------
%% gen callbacks helpers

init_result(
  Starter, Parent, ServerRef, Module, Result,
  Name, Debug, HibernateAfterTimeout) ->
    case Result of
	{ok,State,Data} ->
	    proc_lib:init_ack(Starter, {ok,self()}),
            enter(
              Parent, Debug, Module, Name, HibernateAfterTimeout,
              State, Data, []);
	{ok,State,Data,Actions} ->
	    proc_lib:init_ack(Starter, {ok,self()}),
            enter(
              Parent, Debug, Module, Name, HibernateAfterTimeout,
              State, Data, Actions);
	{stop,Reason} ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, {error,Reason}),
	    exit(Reason);
	ignore ->
	    gen:unregister_name(ServerRef),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	_ ->
	    gen:unregister_name(ServerRef),
	    Error = {bad_return_from_init,Result},
	    proc_lib:init_ack(Starter, {error,Error}),
	    error_info(
	      error, Error, ?STACKTRACE(), Debug,
              #params{parent = Parent, name = Name, module = Module},
              #state{}, []),
	    exit(Error)
    end.

%%%==========================================================================
%%% sys callbacks
%%%
%%% We use {P,S} as state (Misc) for the sys module,
%%% wrap/unwrap it for the server loop* and update
%%% P#params{parent = Parent}.

system_continue(Parent, Debug, {P,S}) ->
    loop(update_parent(P, Parent), Debug, S).

system_terminate(Reason, Parent, Debug, {P,S}) ->
    terminate(
      exit, Reason, ?STACKTRACE(),
      update_parent(P, Parent), Debug, S, []).

system_code_change(
  {#params{module = Module} = P,
   #state{state_data = {State,Data}} = S},
  _Mod, OldVsn, Extra) ->
    case
	try Module:code_change(OldVsn, State, Data, Extra)
	catch
	    Result -> Result
	end
    of
	{ok,NewState,NewData} ->
	    {ok,
	     {P#params{callback_mode = undefined},
              S#state{state_data = {NewState,NewData}}}};
	{ok,_} = Error ->
	    error({case_clause,Error});
	Error ->
	    Error
    end.

system_get_state({_P,#state{state_data = State_Data}}) ->
    {ok,State_Data}.

system_replace_state(
  StateFun, {P,#state{state_data = State_Data} = S}) ->
    %%
    NewState_NewData = StateFun(State_Data),
    {ok,NewState_NewData,{P,S#state{state_data = NewState_NewData}}}.

format_status(
  Opt,
  [PDict,SysState,Parent,Debug,
   {#params{name = Name} = P,
    #state{postponed = Postponed, timers = Timers} = S}]) ->
    Header = gen:format_status_header("Status for state machine", Name),
    Log = sys:get_log(Debug),
    [{header,Header},
     {data,
      [{"Status",SysState},
       {"Parent",Parent},
       {"Time-outs",list_timeouts(Timers)},
       {"Logged Events",Log},
       {"Postponed",Postponed}]} |
     case format_status(Opt, PDict, update_parent(P, Parent), S) of
	 L when is_list(L) -> L;
	 T -> [T]
     end].

%% Update #params.parent only if it differs.  This should not
%% be possible today (OTP-22.0), but could happen for example
%% if someone implements changing a server's parent
%% in a new sys call.
-compile({inline, update_parent/2}).
update_parent(P, Parent) ->
    case P of
        #params{parent = Parent} ->
            P;
        #params{} ->
            P#params{parent = Parent}
    end.

%%---------------------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%---------------------------------------------------------------------------

sys_debug(Debug, NameState, Entry) ->
  sys:handle_debug(Debug, fun print_event/3, NameState, Entry).

print_event(Dev, SystemEvent, Name) ->
    case SystemEvent of
        {in,Event,State} ->
            io:format(
              Dev, "*DBG* ~tp receive ~ts in state ~tp~n",
              [Name,event_string(Event),State]);
        {code_change,Event,State} ->
            io:format(
              Dev, "*DBG* ~tp receive ~ts after code change in state ~tp~n",
              [Name,event_string(Event),State]);
        {out,Reply,{To,_Tag}} ->
            io:format(
              Dev, "*DBG* ~tp send ~tp to ~tw~n",
              [Name,Reply,To]);
        {enter,State} ->
            io:format(
              Dev, "*DBG* ~tp enter in state ~tp~n",
              [Name,State]);
        {start_timer,Action,State} ->
            io:format(
              Dev, "*DBG* ~tp start_timer ~tp in state ~tp~n",
              [Name,Action,State]);
        {insert_timeout,Event,State} ->
            io:format(
              Dev, "*DBG* ~tp insert_timeout ~tp in state ~tp~n",
              [Name,Event,State]);
        {terminate,Reason,State} ->
            io:format(
              Dev, "*DBG* ~tp terminate ~tp in state ~tp~n",
              [Name,Reason,State]);
        {Tag,Event,State,NextState}
          when Tag =:= postpone; Tag =:= consume ->
            StateString =
                case NextState of
                    State ->
                        io_lib:format("~tp", [State]);
                    _ ->
                        io_lib:format("~tp => ~tp", [State,NextState])
                end,
            io:format(
              Dev, "*DBG* ~tp ~tw ~ts in state ~ts~n",
              [Name,Tag,event_string(Event),StateString])
    end.

event_string(Event) ->
    case Event of
	{{call,{Pid,_Tag}},Request} ->
	    io_lib:format("call ~tp from ~tw", [Request,Pid]);
	{EventType,EventContent} ->
	    io_lib:format("~tw ~tp", [EventType,EventContent])
    end.

%%%==========================================================================
%%% Internal callbacks

wakeup_from_hibernate(P, Debug, S) ->
    %% It is a new message that woke us up so we have to receive it now
    loop_receive(P, Debug, S).

%%%==========================================================================
%%% State Machine engine implementation on proc_lib/gen

%% Server loop, consists of all loop* functions
%% and detours through sys:handle_system_message/7 and proc_lib:hibernate/3
%%
%% The loop tries to keep all temporary values in arguments
%% and takes shortcuts for ?not_sys_debug, empty lists, etc.
%% The engine state #state{} is picked apart during the loop,
%% new values are kept in arguments, and a new #state{} is
%% composed at the end of the loop.  #params{} collect engine
%% state fields that rarely changes.
%%
%% The loop is optimized a bit for staying in the loop, assuming that
%% system events are rare.  So a detour to sys requires re-packing
%% of the engine state.

%% Entry point for system_continue/3
%%
loop(P, Debug, #state{hibernate = true} = S) ->
    loop_hibernate(P, Debug, S);
loop(P, Debug, S) ->
    loop_receive(P, Debug, S).

%% Go to hibernation
%%
loop_hibernate(P, Debug, S) ->
    %%
    %% Does not return but restarts process at
    %% wakeup_from_hibernate/3 that jumps to loop_receive/3
    %%
    proc_lib:hibernate(?MODULE, wakeup_from_hibernate, [P, Debug, S]),
    error(
      {should_not_have_arrived_here_but_instead_in,
       {?MODULE,wakeup_from_hibernate,3}}).


%% Entry point for wakeup_from_hibernate/3
%%
%% Receive a new process message
%%
loop_receive(
  #params{hibernate_after = HibernateAfterTimeout} = P, Debug, S) ->
    %%
    receive
	Msg ->
	    case Msg of
                {'$gen_call',From,Request} ->
                    loop_receive_result(P, Debug, S, {{call,From},Request});
                {'$gen_cast',Cast} ->
                    loop_receive_result(P, Debug, S, {cast,Cast});
                %%
		{timeout,TimerRef,TimeoutType} ->
                    case S#state.timers of
                        #{TimeoutType := {TimerRef,TimeoutMsg}} = Timers ->
                            %% Our timer
                            Timers_1 = maps:remove(TimeoutType, Timers),
                            S_1 = S#state{timers = Timers_1},
                            loop_receive_result(
                              P, Debug, S_1, {TimeoutType,TimeoutMsg});
                        #{} ->
			    loop_receive_result(P, Debug, S, {info,Msg})
		    end;
                %%
		{system,Pid,Req} ->
		    %% Does not return but tail recursively calls
		    %% system_continue/3 that jumps to loop/3
		    sys:handle_system_msg(
		      Req, Pid, P#params.parent, ?MODULE, Debug,
                      {P,S},
		      S#state.hibernate);
		{'EXIT',Pid,Reason} ->
                    case P#params.parent of
                        Pid ->
                            terminate(
                              exit, Reason, ?STACKTRACE(), P, Debug, S, []);
                        _ ->
                            loop_receive_result(P, Debug, S, {info,Msg})
                    end;
                %%
                _ ->
                    loop_receive_result(P, Debug, S, {info,Msg})
	    end
    after
        HibernateAfterTimeout ->
            loop_hibernate(P, Debug, S)
    end.

%% We have received an event
%%
loop_receive_result(P, ?not_sys_debug = Debug, S, Event) ->
    %% Here is the queue of not yet handled events created
    Events = [],
    loop_event(P, Debug, S, Event, Events);
loop_receive_result(
  #params{name = Name, callback_mode = CallbackMode} = P, Debug,
  #state{state_data = {State,_Data}} = S, Event) ->
    Debug_1 =
        case CallbackMode of
            undefined ->
                sys_debug(Debug, Name, {code_change,Event,State});
            _ ->
                sys_debug(Debug, Name, {in,Event,State})
        end,
    %% Here is the queue of not yet handled events created
    Events = [],
    loop_event(P, Debug_1, S, Event, Events).

%% Handle one event; received or enqueued
%%
loop_event(
  P, Debug, #state{hibernate = true} = S, Event, Events) ->
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
    loop_event_handler(P, Debug, S, Event, Events);
loop_event(P, Debug, S, Event, Events) ->
    loop_event_handler(P, Debug, S, Event, Events).

%% Call the state function, eventually
%%
-compile({inline, [loop_event_handler/5]}).
loop_event_handler(
  P, Debug, #state{state_data = State_Data} = S, Event, Events) ->
    %%
    %% The field 'hibernate' in S is now invalid and will be
    %% restored when looping back to loop/3 or loop_event/5.
    %%
    Q = [Event|Events],
    loop_callback_mode(P, Debug, S, Q, State_Data, Event).

%% Figure out the callback mode
%%
loop_callback_mode(
  #params{callback_mode = undefined} = P, Debug, S,
  Q, State_Data, CallbackEvent) ->
    %%
    Module = P#params.module,
    try Module:callback_mode() of
	CallbackMode ->
	    loop_callback_mode_result(
              P, Debug, S,
              Q, State_Data, CallbackEvent,
              CallbackMode, listify(CallbackMode), undefined, false)
    catch
	CallbackMode ->
	    loop_callback_mode_result(
              P, Debug, S,
              Q, State_Data, CallbackEvent,
              CallbackMode, listify(CallbackMode), undefined, false);
	Class:Reason:Stacktrace ->
	    terminate(
	      Class, Reason, Stacktrace, P, Debug, S, Q)
    end;
loop_callback_mode(P, Debug, S, Q, State_Data, CallbackEvent) ->
    loop_state_callback(P, Debug, S, Q, State_Data, CallbackEvent).

%% Check the result of Module:callback_mode()
%%
loop_callback_mode_result(
  P, Debug, S, Q, State_Data, CallbackEvent,
  CallbackMode, [H|T], NewCallbackMode, NewStateEnter) ->
    %%
    case callback_mode(H) of
        true ->
            loop_callback_mode_result(
              P, Debug, S, Q, State_Data, CallbackEvent,
              CallbackMode, T, H, NewStateEnter);
        false ->
            case state_enter(H) of
                true ->
                    loop_callback_mode_result(
                      P, Debug, S, Q, State_Data, CallbackEvent,
                      CallbackMode, T, NewCallbackMode, true);
                false ->
                    terminate(
                      error,
                      {bad_return_from_callback_mode,CallbackMode},
                      ?STACKTRACE(),
                      P, Debug, S, Q)
            end
    end;
loop_callback_mode_result(
  P, Debug, S, Q, State_Data, CallbackEvent,
  CallbackMode, [], NewCallbackMode, NewStateEnter) ->
    %%
    case NewCallbackMode of
        undefined ->
            terminate(
              error,
              {bad_return_from_callback_mode,CallbackMode},
              ?STACKTRACE(),
              P, Debug, S, Q);
        _ ->
            P_1 =
                P#params{
                  callback_mode = NewCallbackMode,
                  state_enter = NewStateEnter},
            loop_state_callback(
              P_1, Debug, S, Q, State_Data, CallbackEvent)
    end.


%% Make a state enter call to the state function, we loop back here
%% from further down if state enter calls are enabled
%%
loop_state_enter(
  P, Debug, #state{state_data = {PrevState,_PrevData}} = S,
  Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone) ->
    %%
    StateCall = false,
    CallbackEvent = {enter,PrevState},
    loop_state_callback(
      P, Debug, S, Q, NextState_NewData,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      StateCall, CallbackEvent).

%% Make a state call (not state enter call) to the state function
%%
loop_state_callback(P, Debug, S, Q, State_Data, CallbackEvent) ->
    NextEventsR = [],
    Hibernate = false,
    TimeoutsR = [],
    Postpone = false,
    StateCall = true,
    loop_state_callback(
      P, Debug, S, Q, State_Data,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      StateCall, CallbackEvent).
%%
loop_state_callback(
  #params{callback_mode = CallbackMode, module = Module} = P,
  Debug, S, Q, {State,Data} = State_Data,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  StateCall, {Type,Content}) ->
    try
	case CallbackMode of
	    state_functions ->
		Module:State(Type, Content, Data);
	    handle_event_function ->
		Module:handle_event(Type, Content, State, Data)
	end
    of
	Result ->
            loop_state_callback_result(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              StateCall, Result)
    catch
	Result ->
            loop_state_callback_result(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              StateCall, Result);
	Class:Reason:Stacktrace ->
	    terminate(Class, Reason, Stacktrace, P, Debug, S, Q)
    end;
loop_state_callback(
  P, Debug, S, Q, State_Data,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  StateCall, Actions) when is_list(Actions) ->
    %% Tunneled actions from enter/8
    CallEnter = true,
    loop_actions_list(
      P, Debug, S, Q, State_Data,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      CallEnter, StateCall, Actions).

%% Process the result from the state function
%%
loop_state_callback_result(
  P, Debug, S, Q, {State,_Data} = State_Data,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  StateCall, Result) ->
    %%
    case Result of
	{next_state,State,NewData} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false);
	{next_state,NextState,NewData}
          when StateCall ->
            loop_actions(
              P, Debug, S, Q, {NextState,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true);
	{next_state,_NextState,_NewData} ->
            terminate(
              error,
              {bad_state_enter_return_from_state_function,Result},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q);
	{next_state,State,NewData,Actions} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false, StateCall, Actions);
	{next_state,NextState,NewData,Actions}
          when StateCall ->
            loop_actions(
              P, Debug, S, Q, {NextState,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true, StateCall, Actions);
	{next_state,_NextState,_NewData,_Actions} ->
            terminate(
              error,
              {bad_state_enter_return_from_state_function,Result},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q);
        %%
        {keep_state,NewData} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false);
        {keep_state,NewData,Actions} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false, StateCall, Actions);
        %%
        keep_state_and_data ->
            loop_actions(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false);
        {keep_state_and_data,Actions} ->
            loop_actions(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              false, StateCall, Actions);
        %%
        {repeat_state,NewData} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true);
        {repeat_state,NewData,Actions} ->
            loop_actions(
              P, Debug, S, Q, {State,NewData},
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true, StateCall, Actions);
        %%
        repeat_state_and_data ->
            loop_actions(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true);
        {repeat_state_and_data,Actions} ->
            loop_actions(
              P, Debug, S, Q, State_Data,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              true, StateCall, Actions);
        %%
	stop ->
            terminate(
              exit, normal, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q);
	{stop,Reason} ->
            terminate(
              exit, Reason, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q);
	{stop,Reason,NewData} ->
            terminate(
              exit, Reason, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = {State,NewData},
                hibernate = Hibernate},
              Q);
	%%
	{stop_and_reply,Reason,Replies} ->
            reply_then_terminate(
              exit, Reason, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q, Replies);
	{stop_and_reply,Reason,Replies,NewData} ->
            reply_then_terminate(
              exit, Reason, ?STACKTRACE(), P, Debug,
              S#state{
                state_data = {State,NewData},
                hibernate = Hibernate},
              Q, Replies);
	%%
	_ ->
            terminate(
              error,
              {bad_return_from_state_function,Result},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = State_Data,
                hibernate = Hibernate},
              Q)
    end.

%% Ensure that Actions are a list
%%
loop_actions(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, _StateCall, []) ->
    loop_actions(
      P, Debug, S, Q, NextState_NewData,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      CallEnter);
loop_actions(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions) ->
    %%
    loop_actions_list(
      P, Debug, S, Q, NextState_NewData,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      CallEnter, StateCall, listify(Actions)).
%%
%% Shortcut for no actions
-compile({inline, [loop_actions/10]}).
loop_actions(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter) ->
    %%
    %% Shortcut for no actions
    case CallEnter andalso P#params.state_enter of
	true ->
            loop_state_enter(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone);
	false ->
            loop_state_transition(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone)
    end.

%% Process the returned actions
%%
loop_actions_list(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, _StateCall, []) ->
    %%
    case P#params.state_enter of
        true when CallEnter ->
            loop_state_enter(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone);
        _ ->
            loop_state_transition(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone)
    end;
loop_actions_list(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, [Action|Actions]) ->
    %%
    case Action of
	%% Actual actions
	{reply,From,Reply} ->
            loop_actions_reply(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions,
              From, Reply);
	%%
	%% Actions that set options
	{hibernate,Hibernate_1} when is_boolean(Hibernate_1) ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate_1, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions);
	hibernate ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, true, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions);
	%%
	{postpone,Postpone_1} when not Postpone_1 orelse StateCall ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone_1,
              CallEnter, StateCall, Actions);
	postpone when StateCall ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, true,
              CallEnter, StateCall, Actions);
	postpone ->
            terminate(
              error,
              {bad_state_enter_action_from_state_function,Action},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q);
	%%
	{next_event,Type,Content} ->
            loop_actions_next_event(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions, Type, Content);
	%%
        Timeout ->
            loop_actions_timeout(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions, Timeout)
    end.

%% Process a reply action
%%
loop_actions_reply(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  From, Reply) ->
    %%
    case from(From) of
        true ->
            %% No need for a separate ?not_sys_debug clause here
            %% since the external call to erlang:'!'/2 in reply/2
            %% will cause swap out of all live registers anyway
            reply(From, Reply),
            Debug_1 = ?sys_debug(Debug, P#params.name, {out,Reply,From}),
            loop_actions_list(
              P, Debug_1, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions);
        false ->
            terminate(
              error,
              {bad_action_from_state_function,{reply,From,Reply}},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end.

%% Process a next_event action
%%
loop_actions_next_event(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions, Type, Content) ->
    case event_type(Type) of
        true when StateCall ->
            NextEvent = {Type,Content},
            case Debug of
                ?not_sys_debug ->
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      [NextEvent|NextEventsR],
                      Hibernate, TimeoutsR, Postpone,
                      CallEnter, StateCall, Actions);
                _ ->
                    Name = P#params.name,
                    {State,_Data} = S#state.state_data,
                    Debug_1 =
                        sys_debug(Debug, Name, {in,{Type,Content},State}),
                    loop_actions_list(
                      P, Debug_1, S, Q, NextState_NewData,
                      [NextEvent|NextEventsR],
                      Hibernate, TimeoutsR, Postpone,
                      CallEnter, StateCall, Actions)
              end;
        _ ->
            terminate(
              error,
              {if
                   StateCall ->
                       bad_action_from_state_function;
                   true ->
                       bad_state_enter_action_from_state_function
               end,
               {next_event,Type,Content}},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end.

%% Process a timeout action, or also any unrecognized action
%%
loop_actions_timeout(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  {TimeoutType,Time,TimeoutMsg,TimeoutOpts} = Timeout) ->
    %%
    case timeout_event_type(TimeoutType) of
        true ->
            case listify(TimeoutOpts) of
                %% Optimization cases
                [{abs,true}] when ?absolute_timeout(Time) ->
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      NextEventsR, Hibernate,
                      [Timeout|TimeoutsR], Postpone,
                      CallEnter, StateCall, Actions);
                [{abs,false}] when ?relative_timeout(Time) ->
                    RelativeTimeout = {TimeoutType,Time,TimeoutMsg},
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      NextEventsR, Hibernate,
                      [RelativeTimeout|TimeoutsR], Postpone,
                      CallEnter, StateCall, Actions);
                [] when ?relative_timeout(Time) ->
                    RelativeTimeout = {TimeoutType,Time,TimeoutMsg},
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      NextEventsR, Hibernate,
                      [RelativeTimeout|TimeoutsR], Postpone,
                      CallEnter, StateCall, Actions);
                %% Generic case
                TimeoutOptsList ->
                    case parse_timeout_opts_abs(TimeoutOptsList) of
                        true when ?absolute_timeout(Time) ->
                            loop_actions_list(
                              P, Debug, S, Q, NextState_NewData,
                              NextEventsR, Hibernate,
                              [Timeout|TimeoutsR], Postpone,
                              CallEnter, StateCall, Actions);
                        false when ?relative_timeout(Time) ->
                            RelativeTimeout = {TimeoutType,Time,TimeoutMsg},
                            loop_actions_list(
                              P, Debug, S, Q, NextState_NewData,
                              NextEventsR, Hibernate,
                              [RelativeTimeout|TimeoutsR], Postpone,
                              CallEnter, StateCall, Actions);
                        _ ->
                            terminate(
                              error,
                              {bad_action_from_state_function,Timeout},
                              ?STACKTRACE(), P, Debug,
                              S#state{
                                state_data = NextState_NewData,
                                hibernate = Hibernate},
                              Q)
                    end
            end;
        false ->
            terminate(
              error,
              {bad_action_from_state_function,Timeout},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end;
loop_actions_timeout(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  {TimeoutType,Time,_TimeoutMsg} = Timeout) ->
    %%
    case timeout_event_type(TimeoutType) of
        true
          when ?relative_timeout(Time);
               Time =:= update ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate,
              [Timeout|TimeoutsR], Postpone,
              CallEnter, StateCall, Actions);
        _ ->
            terminate(
              error,
              {bad_action_from_state_function,Timeout},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end;
loop_actions_timeout(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  {TimeoutType,cancel} = Action) ->
    %%
    case timeout_event_type(TimeoutType) of
        true ->
            Timeout = {TimeoutType,infinity,undefined},
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate,
              [Timeout|TimeoutsR], Postpone,
              CallEnter, StateCall, Actions);
        false ->
            terminate(
              error,
              {bad_action_from_state_function,Action},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end;
loop_actions_timeout(
  P, Debug, S, Q, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone,
  CallEnter, StateCall, Actions,
  Time) ->
    %%
    if
        ?relative_timeout(Time) ->
            Timeout = {timeout,Time,Time},
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate,
              [Timeout|TimeoutsR], Postpone,
              CallEnter, StateCall, Actions);
        true ->
            terminate(
              error,
              {bad_action_from_state_function,Time},
              ?STACKTRACE(), P, Debug,
              S#state{
                state_data = NextState_NewData,
                hibernate = Hibernate},
              Q)
    end.

%% Do the state transition
%%
loop_state_transition(
  P, Debug, #state{state_data = {State,_Data}, postponed = Postponed} = S,
  [Event|Events], {NextState,_NewData} = NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postpone) ->
    %%
    %% All options have been collected and next_events are buffered.
    %% Do the actual state transition.
    %%
    Postponed_1 = % Move current event to postponed if Postpone
	case Postpone of
	    true ->
                [Event|Postponed];
	    false ->
                Postponed
	end,
    case Debug of
        ?not_sys_debug ->
	    %% Optimization for no sys_debug
	    %% - avoid calling sys_debug/3
	    if
		NextState =:= State ->
		    loop_keep_state(
                      P, Debug, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed_1);
		true ->
		    loop_state_change(
                      P, Debug, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed_1)
	    end;
        _ ->
            %% With sys_debug
            Name = P#params.name,
	    Debug_1 =
		case Postpone of
		    true ->
			sys_debug(
			   Debug, Name,
			   {postpone,Event,State,NextState});
		    false ->
			sys_debug(
			   Debug, Name,
			   {consume,Event,State,NextState})
		end,
	    if
		NextState =:= State ->
		    loop_keep_state(
                      P, Debug_1, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed_1);
		true ->
		    loop_state_change(
                      P, Debug_1, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed_1)
	    end
    end.

%% State transition to the same state
%%
loop_keep_state(
  P, Debug, #state{timers = Timers} = S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed) ->
    %%
    %% Cancel event timeout
    %%
    case Timers of
        #{timeout := {TimerRef,_TimeoutMsg}} ->
	    %% Event timeout active
	    loop_next_events(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
	      cancel_timer(timeout, TimerRef, Timers));
	_ ->
	    %% No event timeout active
	    loop_next_events(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers)
    end.

%% State transition to a different state
%%
loop_state_change(
  P, Debug, S, Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed) ->
    %%
    %% Retry postponed events
    %%
    case Postponed of
        [] ->
            loop_state_change(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR);
        [E1] ->
            loop_state_change(
              P, Debug, S,
              [E1|Events], NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR);
        [E2,E1] ->
            loop_state_change(
              P, Debug, S,
              [E1,E2|Events], NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR);
        _ ->
            loop_state_change(
              P, Debug, S,
              lists:reverse(Postponed, Events), NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR)
    end.
%%
loop_state_change(
  P, Debug, #state{timers = Timers} = S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR) ->
    %%
    %% Cancel state and event timeout
    %%
    case Timers of
	%% Optimization
	%% - only cancel timeout when it is active
	%%
	#{state_timeout := {TimerRef,_TimeoutMsg}} ->
	    %% State timeout active
            %% - cancel event timeout too since it is faster than inspecting
	    loop_next_events(
              P, Debug, S, Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, [],
              cancel_timer(
                timeout,
                cancel_timer(state_timeout, TimerRef, Timers)));
        #{timeout := {TimerRef,_TimeoutMsg}} ->
            %% Event timeout active but not state timeout
            %% - cancel event timeout only
            loop_next_events(
              P, Debug, S, Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, [],
              cancel_timer(timeout, TimerRef, Timers));
        _ ->
            %% No state nor event timeout active.
            loop_next_events(
              P, Debug, S, Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, [],
              Timers)
    end.

%% Continue state transition with processing of
%% timeouts and inserted events
%%
loop_next_events(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, [], Postponed,
  Timers) ->
    %%
    %% Optimization when there are no timeouts
    %% hence no timeout zero events to append to Events
    %% - avoid loop_timeouts
    loop_done(
      P, Debug,
      S#state{
        state_data = NextState_NewData,
	postponed = Postponed,
        timers = Timers,
	hibernate = Hibernate},
      NextEventsR, Events);
loop_next_events(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers) ->
    %%
    Seen = #{},
    TimeoutEvents = [],
    loop_timeouts(
      P, Debug, S,
      Events, NextState_NewData,
      NextEventsR, Hibernate, TimeoutsR, Postponed,
      Timers, Seen, TimeoutEvents).

%% Continue state transition with processing of timeouts
%% and finally inserted events
%%
loop_timeouts(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, [], Postponed,
  Timers, _Seen, TimeoutEvents) ->
    %%
    %% End of timeouts
    %%
    S_1 =
        S#state{
          state_data = NextState_NewData,
          postponed = Postponed,
          timers = Timers,
          hibernate = Hibernate},
    case TimeoutEvents of
        [] ->
            loop_done(P, Debug, S_1, NextEventsR, Events);
        _ ->
            case Events of
                [] ->
                    loop_prepend_timeout_events(
                      P, Debug, S_1, TimeoutEvents,
                      NextEventsR);
                [E1] ->
                    loop_prepend_timeout_events(
                      P, Debug, S_1, TimeoutEvents,
                      [E1|NextEventsR]);
                [E2,E1] ->
                    loop_prepend_timeout_events(
                      P, Debug, S_1, TimeoutEvents,
                      [E1,E2|NextEventsR]);
                _ ->
                    loop_prepend_timeout_events(
                      P, Debug, S_1, TimeoutEvents,
                      lists:reverse(Events, NextEventsR))
            end
    end;
loop_timeouts(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, [Timeout|TimeoutsR], Postponed,
  Timers, Seen, TimeoutEvents) ->
    %%
    TimeoutType = element(1, Timeout),
    case Seen of
        #{TimeoutType := _} ->
            %% Type seen before - ignore
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents);
        #{} ->
            case Timeout of
                {_,Time,TimeoutMsg} ->
                    %% Relative timeout or update
                    loop_timeouts_start(
                      P, Debug, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed,
                      Timers, Seen, TimeoutEvents,
                      TimeoutType, Time, TimeoutMsg, []);
                {_,Time,TimeoutMsg,TimeoutOpts} ->
                    %% Absolute timeout
                    loop_timeouts_start(
                      P, Debug, S,
                      Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed,
                      Timers, Seen, TimeoutEvents,
                      TimeoutType, Time, TimeoutMsg, listify(TimeoutOpts))
            end
    end.

%% Loop helper to start or restart a timeout
%%
loop_timeouts_start(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, Time, TimeoutMsg, TimeoutOpts) ->
    %%
    case Time of
        infinity ->
            %% Cancel any running timer
            loop_timeouts_cancel(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType);
        0 when TimeoutOpts =:= [] ->
            %% Relative timeout zero
            %% - cancel any running timer
            %%   handle timeout zero events later
            %%
            loop_timeouts_cancel(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, [{TimeoutType,TimeoutMsg}|TimeoutEvents],
              TimeoutType);
        update ->
            loop_timeouts_update(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType, TimeoutMsg);
        _ ->
            %% (Re)start the timer
            TimerRef =
                erlang:start_timer(Time, self(), TimeoutType, TimeoutOpts),
            case Debug of
                ?not_sys_debug ->
                    loop_timeouts_register(
                      P, Debug, S, Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed,
                      Timers, Seen, TimeoutEvents,
                      TimeoutType, TimerRef, TimeoutMsg);
                _ ->
                    {State,_Data} = NextState_NewData,
                    Debug_1 =
                        sys_debug(
                          Debug, P#params.name,
                          {start_timer,
                           {TimeoutType,Time,TimeoutMsg,TimeoutOpts},
                           State}),
                    loop_timeouts_register(
                      P, Debug_1, S, Events, NextState_NewData,
                      NextEventsR, Hibernate, TimeoutsR, Postponed,
                      Timers, Seen, TimeoutEvents,
                      TimeoutType, TimerRef, TimeoutMsg)
            end
    end.

%% Loop helper to register a newly started timer
%% and to cancel any running timer
%%
loop_timeouts_register(
  P, Debug, S, Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, TimerRef, TimeoutMsg) ->
    %%
    case Timers of
        #{TimeoutType := {OldTimerRef,_OldTimeoutMsg}} ->
            %% Cancel the running timer,
            %% and update timer type and ref
            cancel_timer(OldTimerRef),
            Timers_1 = Timers#{TimeoutType := {TimerRef,TimeoutMsg}},
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true}, TimeoutEvents);
        #{} ->
            %% Insert the new timer type and ref
            Timers_1 = Timers#{TimeoutType => {TimerRef,TimeoutMsg}},
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true}, TimeoutEvents)
    end.

%% Loop helper to cancel a timeout
%%
loop_timeouts_cancel(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents, TimeoutType) ->
    %% This function body should have been:
    %%    Timers_1 = cancel_timer(TimeoutType, Timers),
    %%    loop_timeouts(
    %%      P, Debug, S,
    %%      Events, NextState_NewData,
    %%      NextEventsR, Hibernate, TimeoutsR, Postponed,
    %%      Timers_1, Seen#{TimeoutType => true}, TimeoutEvents).
    %%
    %% Explicitly separate cases to get separate code paths for when
    %% the map key exists vs. not, since otherwise the external call
    %% to erlang:cancel_timer/1 and to map:remove/2 within
    %% cancel_timer/2 would cause all live registers
    %% to be saved to and restored from the stack also for
    %% the case when the map key TimeoutType does not exist
    case Timers of
        #{TimeoutType := {TimerRef,_TimeoutMsg}} ->
            Timers_1 = cancel_timer(TimeoutType, TimerRef, Timers),
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true}, TimeoutEvents);
        #{} ->
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen#{TimeoutType => true}, TimeoutEvents)
    end.

%% Loop helper to update the timeout message,
%% or insert an event if no timer is running
%%
loop_timeouts_update(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, TimeoutMsg) ->
    %%
    case Timers of
        #{TimeoutType := {TimerRef,_OldTimeoutMsg}} ->
            Timers_1 = Timers#{TimeoutType := {TimerRef,TimeoutMsg}},
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true},
              TimeoutEvents);
        #{} ->
            TimeoutEvents_1 =
                [{TimeoutType,TimeoutMsg}|TimeoutEvents],
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen#{TimeoutType => true},
              TimeoutEvents_1)
    end.

%% Continue state transition with prepending timeout zero events
%% before event queue reversal i.e appending timeout zero events
%%
loop_prepend_timeout_events(P, Debug, S, TimeoutEvents, EventsR) ->
    {Debug_1,Events_1R} =
        prepend_timeout_events(P, Debug, S, TimeoutEvents, EventsR),
    loop_done(P, Debug_1, S, Events_1R, []).

%% Place inserted events first in the event queue
%%
loop_done(P, Debug, S, NextEventsR, Events) ->
    case NextEventsR of
        [] ->
            loop_done(P, Debug, S, Events);
        [E1] ->
            loop_done(P, Debug, S, [E1|Events]);
        [E2,E1] ->
            loop_done(P, Debug, S, [E1,E2|Events]);
        _ ->
            loop_done(P, Debug, S, lists:reverse(NextEventsR, Events))
    end.
%%
%% State transition is done, keep looping if there are
%% enqueued events, otherwise get a new event
%%
loop_done(P, Debug, S, Q) ->
%%%    io:format(
%%%      "loop_done: state_data = ~p,~n"
%%%      "    postponed = ~p, Q = ~p,~n",
%%%      "    timers = ~p.~n"
%%%      [S#state.state_data,,S#state.postponed,Q,S#state.timers]),
    case Q of
        [] ->
            %% Get a new event
            loop(P, Debug, S);
        [Event|Events] ->
	    %% Loop until out of enqueued events
	    loop_event(P, Debug, S, Event, Events)
    end.

%%---------------------------------------------------------------------------
%% Server loop helpers

%% Parse an option list for erlang:start_timer/4 to figure out
%% if the timeout will be absolute or relative
%%
parse_timeout_opts_abs(Opts) ->
    parse_timeout_opts_abs(Opts, false).
%%
parse_timeout_opts_abs(Opts, Abs) ->
    case Opts of
        [] ->
            Abs;
        [{abs,Abs_1}|Opts] when is_boolean(Abs_1) ->
            parse_timeout_opts_abs(Opts, Abs_1);
        _ ->
            badarg
    end.

%% Enqueue immediate timeout events (timeout 0 events)
%%
%% Event timeout 0 events gets special treatment since
%% an event timeout is cancelled by any received event,
%% so if there are enqueued events before the event
%% timeout 0 event - the event timeout is cancelled hence no event.
%%
%% Other (state_timeout and {timeout,Name}) timeout 0 events
%% that occur after an event timer timeout 0 event are considered to
%% belong to timers that were started after the event timer
%% timeout 0 event fired, so they do not cancel the event timer.
%%
prepend_timeout_events(_P, Debug, _S, [], EventsR) ->
    {Debug,EventsR};
prepend_timeout_events(
  P, Debug, S, [{timeout,_} = TimeoutEvent|TimeoutEvents], []) ->
    %% Prepend this since there are no other events in queue
    case Debug of
        ?not_sys_debug ->
            prepend_timeout_events(
              P, Debug, S, TimeoutEvents, [TimeoutEvent]);
        _ ->
            {State,_Data} = S#state.state_data,
            Debug_1 =
              sys_debug(
                Debug, P#params.name,
                {insert_timeout,TimeoutEvent,State}),
            prepend_timeout_events(
              P, Debug_1, S, TimeoutEvents, [TimeoutEvent])
    end;
prepend_timeout_events(
  P, Debug, S, [{timeout,_}|TimeoutEvents], EventsR) ->
    %% Ignore since there are other events in queue
    %% so they have cancelled the event timeout 0.
    prepend_timeout_events(P, Debug, S, TimeoutEvents, EventsR);
prepend_timeout_events(
  P, Debug, S, [TimeoutEvent|TimeoutEvents], EventsR) ->
    %% Just prepend all others
    case Debug of
        ?not_sys_debug ->
            prepend_timeout_events(
              P, Debug, S, TimeoutEvents, [TimeoutEvent|EventsR]);
        _ ->
            {State,_Data} = S#state.state_data,
            Debug_1 =
                sys_debug(
                  Debug, P#params.name,
                  {insert_timeout,TimeoutEvent,State}),
            prepend_timeout_events(
              P, Debug_1, S, TimeoutEvents, [TimeoutEvent|EventsR])
    end.



%%---------------------------------------------------------------------------
%% Server helpers

reply_then_terminate(Class, Reason, Stacktrace, P, Debug, S, Q, Replies) ->
    do_reply_then_terminate(
      Class, Reason, Stacktrace, P, Debug, S, Q, listify(Replies)).
%%
do_reply_then_terminate(
  Class, Reason, Stacktrace, P, Debug, S, Q, []) ->
    terminate(Class, Reason, Stacktrace, P, Debug, S, Q);
do_reply_then_terminate(
  Class, Reason, Stacktrace, P, Debug, S, Q, [R|Rs]) ->
    case R of
        {reply,From,Reply} ->
            case from(From) of
                true ->
                    reply(From, Reply),
                    Debug_1 =
                        ?sys_debug(
                           Debug,
                           P#params.name,
                           {out,Reply,From}),
                    do_reply_then_terminate(
                      Class, Reason, Stacktrace, P, Debug_1, S, Q, Rs);
                false ->
                    terminate(
                      error,
                      {bad_reply_action_from_state_function,R},
                      ?STACKTRACE(),
                      P, Debug, S, Q)
            end;
	_ ->
	    terminate(
	      error,
	      {bad_reply_action_from_state_function,R},
	      ?STACKTRACE(),
	      P, Debug, S, Q)
    end.

terminate(
  Class, Reason, Stacktrace,
  #params{module = Module} = P, Debug,
  #state{state_data = {State,Data}} = S, Q) ->
    case erlang:function_exported(Module, terminate, 3) of
	true ->
	    try Module:terminate(Reason, State, Data) of
		_ -> ok
	    catch
		_ -> ok;
		C:R:ST ->
		    error_info(C, R, ST, Debug, P, S, Q),
		    erlang:raise(C, R, ST)
	    end;
	false ->
	    ok
    end,
    _ =
	case Reason of
	    normal ->
                terminate_sys_debug(Debug, P, State, Reason);
	    shutdown ->
                terminate_sys_debug(Debug, P, State, Reason);
	    {shutdown,_} ->
                terminate_sys_debug(Debug, P, State, Reason);
	    _ ->
		error_info(Class, Reason, Stacktrace, Debug, P, S, Q)
	end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	_ ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

terminate_sys_debug(Debug, P, State, Reason) ->
    ?sys_debug(Debug, P#params.name, {terminate,Reason,State}).


error_info(
  Class, Reason, Stacktrace, Debug,
  #params{
     name = Name,
     callback_mode = CallbackMode,
     state_enter = StateEnter} = P,
  #state{
     postponed = Postponed,
     timers = Timers} = S,
  Q) ->
    Log = sys:get_log(Debug),
    ?LOG_ERROR(#{label=>{gen_statem,terminate},
                 name=>Name,
                 queue=>Q,
                 postponed=>Postponed,
                 callback_mode=>CallbackMode,
                 state_enter=>StateEnter,
                 state=>format_status(terminate, get(), P, S),
                 timeouts=>list_timeouts(Timers),
                 log=>Log,
                 reason=>{Class,Reason,Stacktrace},
                 client_info=>client_stacktrace(Q)},
               #{domain=>[otp],
                 report_cb=>fun gen_statem:format_log/1,
                 error_logger=>#{tag=>error}}).

client_stacktrace([]) ->
    undefined;
client_stacktrace([{{call,{Pid,_Tag}},_Req}|_]) when is_pid(Pid) ->
    if
        node(Pid) =:= node() ->
            case
                process_info(Pid, [current_stacktrace, registered_name])
            of
                undefined ->
                    {Pid,dead};
                [{current_stacktrace, Stacktrace},
                 {registered_name, []}] ->
                    {Pid,{Pid,Stacktrace}};
                [{current_stacktrace, Stacktrace},
                 {registered_name, Name}] ->
                    {Pid,{Name,Stacktrace}}
            end;
        true ->
            {Pid,remote}
    end;
client_stacktrace([_|_]) ->
    undefined.


format_log(#{label:={gen_statem,terminate},
             name:=Name,
             queue:=Q,
             postponed:=Postponed,
             callback_mode:=CallbackMode,
             state_enter:=StateEnter,
             state:=FmtData,
             timeouts:=Timeouts,
             log:=Log,
             reason:={Class,Reason,Stacktrace},
             client_info:=ClientInfo}) ->
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
				{{'function not exported',{M,F,Arity}},ST}
			end
		end;
	    _ -> {Reason,Stacktrace}
	end,
    {ClientFmt,ClientArgs} = format_client_log(ClientInfo),
    CBMode =
	 case StateEnter of
	     true ->
		 [CallbackMode,state_enter];
	     false ->
		 CallbackMode
	 end,
    {"** State machine ~tp terminating~n" ++
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
         case Postponed of
             [] -> "";
             _ -> "** Postponed = ~tp~n"
         end ++
         case FixedStacktrace of
             [] -> "";
             _ -> "** Stacktrace =~n**  ~tp~n"
         end ++
         case Timeouts of
             {0,_} -> "";
             _ -> "** Time-outs: ~p~n"
         end ++
         case Log of
             [] -> "";
             _ -> "** Log =~n**  ~tp~n"
         end ++ ClientFmt,
     [Name |
      case Q of
          [] -> [];
          [Event|_] -> [error_logger:limit_term(Event)]
      end] ++
         [error_logger:limit_term(FmtData),
          Class,error_logger:limit_term(FixedReason),
          CBMode] ++
         case Q of
             [_|[_|_] = Events] -> [error_logger:limit_term(Events)];
             _ -> []
         end ++
         case Postponed of
             [] -> [];
             _ -> [error_logger:limit_term(Postponed)]
         end ++
         case FixedStacktrace of
             [] -> [];
             _ -> [error_logger:limit_term(FixedStacktrace)]
         end ++
         case Timeouts of
             {0,_} -> [];
             _ -> [error_logger:limit_term(Timeouts)]
         end ++
         case Log of
             [] -> [];
             _ -> [[error_logger:limit_term(T) || T <- Log]]
         end ++ ClientArgs}.

format_client_log(undefined) ->
    {"", []};
format_client_log({Pid,dead}) ->
    {"** Client ~p is dead~n", [Pid]};
format_client_log({Pid,remote}) ->
    {"** Client ~p is remote on node ~p~n", [Pid, node(Pid)]};
format_client_log({_Pid,{Name,Stacktrace}}) ->
    {"** Client ~tp stacktrace~n"
     "** ~tp~n",
     [Name, error_logger:limit_term(Stacktrace)]}.


%% Call Module:format_status/2 or return a default value
format_status(
  Opt, PDict,
  #params{module = Module},
  #state{state_data = {State,Data} = State_Data}) ->
    case erlang:function_exported(Module, format_status, 2) of
	true ->
	    try Module:format_status(Opt, [PDict,State,Data])
	    catch
		Result -> Result;
		_:_ ->
		    format_status_default(
		      Opt,
                      {State,
                       atom_to_list(Module) ++ ":format_status/2 crashed"})
	    end;
	false ->
	    format_status_default(Opt, State_Data)
    end.

%% The default Module:format_status/3
format_status_default(Opt, State_Data) ->
    case Opt of
	terminate ->
	    State_Data;
	_ ->
	    [{data,[{"State",State_Data}]}]
    end.

-compile({inline, [listify/1]}).
listify(Item) when is_list(Item) ->
    Item;
listify(Item) ->
    [Item].


-define(
   cancel_timer(TimerRef),
   case erlang:cancel_timer(TimerRef) of
       false ->
           %% No timer found and we have not seen the timeout message
           receive
               {timeout,(TimerRef),_} ->
                   ok
           end;
       _ ->
           %% Timer was running
           ok
   end).
%%
%% Cancel timer and consume timeout message
%%
-compile({inline, [cancel_timer/1]}).
cancel_timer(TimerRef) ->
    ?cancel_timer(TimerRef).

-define(
   cancel_timer(TimeoutType, TimerRef, Timers),
   begin
       ?cancel_timer(TimerRef),
       maps:remove(begin TimeoutType end, begin Timers end)
   end).
%%
%% Cancel timer and remove from Timers
%%
-compile({inline, [cancel_timer/3]}).
cancel_timer(TimeoutType, TimerRef, Timers) ->
    ?cancel_timer(TimeoutType, TimerRef, Timers).

%% Cancel timer if running, otherwise no op
%%
%% Remove the timer from Timers
-compile({inline, [cancel_timer/2]}).
cancel_timer(TimeoutType, Timers) ->
    case Timers of
        #{TimeoutType := {TimerRef, _TimeoutMsg}} ->
            ?cancel_timer(TimeoutType, TimerRef, Timers);
        #{} ->
            Timers
    end.

%% Return a list of all pending timeouts
list_timeouts(Timers) ->
    {maps:size(Timers),
     maps:fold(
       fun(TimeoutType, {_TimerRef,TimeoutMsg}, Acc) ->
               [{TimeoutType,TimeoutMsg}|Acc]
       end, [], Timers)}.
