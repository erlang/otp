%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2023. All Rights Reserved.
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

%%%
%%% NOTE: If init_ack() return values are modified, see comment
%%%       above monitor_return() in gen.erl!
%%%

%% API
-export(
   [start/3,start/4,start_link/3,start_link/4,
    start_monitor/3,start_monitor/4,
    stop/1,stop/3,
    cast/2,call/2,call/3,
    send_request/2, send_request/4,
    wait_response/1, wait_response/2, wait_response/3,
    receive_response/1, receive_response/2, receive_response/3,
    check_response/2, check_response/3,
    reqids_new/0, reqids_size/1,
    reqids_add/3, reqids_to_list/1,
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
-export([format_log/1, format_log/2]).

%% Type exports for templates and callback modules
-export_type(
   [event_type/0,
    from/0,
    reply_tag/0,
    callback_mode_result/0,
    init_result/1,
    init_result/2,
    state_enter_result/1,
    state_enter_result/2,
    event_handler_result/1,
    event_handler_result/2,
    reply_action/0,
    enter_action/0,
    action/0,
    request_id/0,
    request_id_collection/0,
    format_status/0
   ]).
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
    enter_loop_opt/0,
    start_ret/0,
    start_mon_ret/0]).

%% -define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).


%%%==========================================================================
%%% Interface functions.
%%%==========================================================================

-type from() ::
	{To :: pid(), Tag :: reply_tag()}. % Reply-to specifier for call
-opaque reply_tag() :: gen:reply_tag().

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

-type event_content() :: term().

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
	 EventContent :: event_content()} |
        {'change_callback_module', NewModule :: module()} |
        {'push_callback_module', NewModule :: module()} |
        'pop_callback_module' |
	enter_action().
-type enter_action() ::
	'hibernate' | % Set the hibernate option
	{'hibernate', Hibernate :: hibernate()} |
        timeout_action() |
	reply_action().
-type timeout_action() ::
	(Time :: event_timeout()) | % {timeout,Time,Time}
	{'timeout', % Set the event_timeout option
	 Time :: event_timeout(), EventContent :: event_content()} |
	{'timeout', % Set the event_timeout option
	 Time :: event_timeout(),
	 EventContent :: event_content(),
	 Options :: (timeout_option() | [timeout_option()])} |
	%%
	{{'timeout', Name :: term()}, % Set the generic_timeout option
	 Time :: generic_timeout(), EventContent :: event_content()} |
	{{'timeout', Name :: term()}, % Set the generic_timeout option
	 Time :: generic_timeout(),
	 EventContent :: event_content(),
	 Options :: (timeout_option() | [timeout_option()])} |
	%%
	{'state_timeout', % Set the state_timeout option
	 Time :: state_timeout(), EventContent :: event_content()} |
	{'state_timeout', % Set the state_timeout option
	 Time :: state_timeout(),
	 EventContent :: event_content(),
	 Options :: (timeout_option() | [timeout_option()])} |
        timeout_cancel_action() |
        timeout_update_action().
-type timeout_cancel_action() ::
        {'timeout', 'cancel'} |
        {{'timeout', Name :: term()}, 'cancel'} |
        {'state_timeout', 'cancel'}.
-type timeout_update_action() ::
        {'timeout', 'update', EventContent :: event_content()} |
        {{'timeout', Name :: term()},
         'update', EventContent :: event_content()} |
        {'state_timeout', 'update', EventContent :: event_content()}.
-type reply_action() ::
	{'reply', % Reply to a caller
	 From :: from(), Reply :: term()}.

-type init_result(StateType) :: init_result(StateType, term()).
-type init_result(StateType, DataType) ::
    {ok, State :: StateType, Data :: DataType} |
    {ok, State :: StateType, Data :: DataType,
     Actions :: [action()] | action()} |
        'ignore' |
        {'stop', Reason :: term()} |
        {'error', Reason :: term()}.

%% Old, not advertised
-type state_function_result() ::
	event_handler_result(state_name()).
-type handle_event_result() ::
	event_handler_result(state()).
%%
-type state_enter_result(State) :: state_enter_result(State, term()).
-type state_enter_result(State, DataType) ::
	{'next_state', % {next_state,State,NewData,[]}
	 State,
	 NewData :: DataType} |
	{'next_state', % State entry for state State
	 State,
	 NewData :: DataType,
	 Actions :: [enter_action()] | enter_action()} |
	state_callback_result(enter_action()).
-type event_handler_result(StateType) ::
    event_handler_result(StateType, term()).
-type event_handler_result(StateType, DataType) ::
	{'next_state', % {next_state,NextState,NewData,[]}
	 NextState :: StateType,
	 NewData :: DataType} |
	{'next_state', % State transition, maybe to the same state
	 NextState :: StateType,
	 NewData :: DataType,
	 Actions :: [action()] | action()} |
	state_callback_result(action()).
-type state_callback_result(ActionType) ::
    state_callback_result(ActionType, term()).
-type state_callback_result(ActionType, DataType) ::
	{'keep_state', % {keep_state,NewData,[]}
	 NewData :: DataType} |
	{'keep_state', % Keep state, change data
	 NewData :: DataType,
	 Actions :: [ActionType] | ActionType} |
	'keep_state_and_data' | % {keep_state_and_data,[]}
	{'keep_state_and_data', % Keep state and data -> only actions
	 Actions :: [ActionType] | ActionType} |
	%%
	{'repeat_state', % {repeat_state,NewData,[]}
	 NewData :: DataType} |
	{'repeat_state', % Repeat state, change data
	 NewData :: DataType,
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
	 NewData :: DataType} |
	%%
	{'stop_and_reply', % Reply then stop the server
	 Reason :: term(),
	 Replies :: [reply_action()] | reply_action()} |
	{'stop_and_reply', % Reply then stop the server
	 Reason :: term(),
	 Replies :: [reply_action()] | reply_action(),
	 NewData :: DataType}.

-opaque request_id() :: gen:request_id().

-opaque request_id_collection() :: gen:request_id_collection().

-type response_timeout() ::
        timeout() | {abs, integer()}.

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
	    data()) ->
    state_enter_result('state_name');
           (event_type(),
	    event_content(),
	    data()) ->
    event_handler_result(state_name()).
%%
%% State callback for all states
%% when callback_mode() =:= handle_event_function.
-callback handle_event(
	    'enter',
	    OldState :: state(),
	    CurrentState,
	    data()) ->
    state_enter_result(CurrentState);
           (event_type(),
	    event_content(),
	    CurrentState :: state(),
	    data()) ->
    event_handler_result(state()). % New state

%% Clean up before the server terminates.
-callback terminate(
	    Reason :: 'normal' | 'shutdown' | {'shutdown', term()}
		    | term(),
	    CurrentState :: state(),
	    data()) ->
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
%%
%% Deprecated
-callback format_status(
	    StatusOption,
	    [ [{Key :: term(), Value :: term()}] |
	      state() |
	      data()]) ->
    Status :: term() when
      StatusOption :: 'normal' | 'terminate'.

-type format_status() ::
        #{ state => state(),
           data => data(),
           reason => term(),
           queue => [{event_type(), event_content()}],
           postponed => [{event_type(), event_content()}],
           timeouts => [{timeout_event_type(), event_content()}],
           log => [sys:system_event()] }.

%% Format the callback module status in some sensible that is
%% often condensed way.
-callback format_status(Status) -> NewStatus when
      Status    :: format_status(),
      NewStatus :: format_status().

-optional_callbacks(
   [format_status/1, % Has got a default implementation
    format_status/2, % Has got a default implementation
    terminate/3, % Has got a default implementation
    code_change/4, % Only needed by advanced soft upgrade
    %%
    state_name/3, % Just an example callback;
    %% for callback_mode() =:= state_functions
    %% there has to be a StateName/3 callback function
    %% for every StateName in your state machine,
    %% but not one has to be named 'state_name'
    %%
    handle_event/4 % Only for callback_mode() =:= handle_event_function
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
-define(
   timeout_event_type(Type),
   case (Type) of
       timeout -> true;
       state_timeout -> true;
       {timeout,_} -> true;
       _ -> false
   end).
timeout_event_type(Type) ->
    ?timeout_event_type(Type).
%%
-define(
   from(From),
   case (From) of
       {_,_} when is_pid(element(1, (From))) -> true;
       _ -> false
   end).
from(From) ->
    ?from(From).
%%
event_type(Type) ->
    case Type of
	{call,From} -> ?from(From);
        %%
	cast -> true;
	info -> true;
	internal -> true;
        %%
        _ -> ?timeout_event_type(Type)
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
        {callback_mode = state_functions :: callback_mode(),
         state_enter = false :: boolean(),
         parent :: pid(),
         modules = [?MODULE] :: nonempty_list(module()),
         name :: atom() | pid(),
         hibernate_after = infinity :: timeout()
        }).

-record(state,
        {state_data = {undefined,undefined} ::
           {State :: term(),Data :: term()},
         postponed = [] :: [{event_type(),event_content()}],
         timers = #{t0q => []} ::
           #{
              %% Timeout 0 Queue.
              %% Marked in the table with TimerRef = 0.
              %% Stored here because they also are updated
              %% by e.g cancel_timer/3.
              't0q' := [timeout_event_type()],

              TimeoutType :: timeout_event_type() =>
                             {TimerRef :: reference() | 0,
                              TimeoutMsg :: event_content()}},
         hibernate = false :: boolean()
        }).

%%%==========================================================================
%%% API

-type server_name() :: % Duplicate of gen:emgr_name()
        {'local', atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), Name :: term()}.

-type server_ref() :: % What gen:call/3,4 and gen:stop/1,3 accepts
        pid()
      | (LocalName :: atom())
      | {Name :: atom(), Node :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.

-type start_opt() :: % Duplicate of gen:option()
        {'timeout', Time :: timeout()}
      | {'spawn_opt', [proc_lib:spawn_option()]}
      | enter_loop_opt().
%%
-type enter_loop_opt() :: % Some gen:option()s works for enter_loop/*
	{'hibernate_after', HibernateAfterTimeout :: timeout()}
      | {'debug', Dbgs :: [sys:debug_option()]}.

-type start_ret() :: % gen:start_ret() without monitor return
        {'ok', pid()}
      | 'ignore'
      | {'error', term()}.

-type start_mon_ret() :: % gen:start_ret() with only monitor return
        {'ok', {pid(),reference()}}
      | 'ignore'
      | {'error', term()}.




%% Start a state machine
-spec start(
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start(Module, Args, Opts)
  when is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, nolink, Module, Args, Opts);
start(Module, Args, Opts) ->
    error(badarg, [Module, Args, Opts]).
%%
-spec start(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start(ServerName, Module, Args, Opts)
  when is_tuple(ServerName), is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, nolink, ServerName, Module, Args, Opts);
start(ServerName, Module, Args, Opts) ->
    error(badarg, [ServerName, Module, Args, Opts]).

%% Start and link to a state machine
-spec start_link(
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start_link(Module, Args, Opts)
  when is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, link, Module, Args, Opts);
start_link(Module, Args, Opts) ->
    error(badarg, [Module, Args, Opts]).
%%
-spec start_link(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_ret().
start_link(ServerName, Module, Args, Opts)
  when is_tuple(ServerName), is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, link, ServerName, Module, Args, Opts);
start_link(ServerName, Module, Args, Opts) ->
    error(badarg, [ServerName, Module, Args, Opts]).

%% Start and monitor a state machine
-spec start_monitor(
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_mon_ret().
start_monitor(Module, Args, Opts)
  when is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, monitor, Module, Args, Opts);
start_monitor(Module, Args, Opts) ->
    error(badarg, [Module, Args, Opts]).
%%
-spec start_monitor(
	ServerName :: server_name(),
	Module :: module(), Args :: term(), Opts :: [start_opt()]) ->
		   start_mon_ret().
start_monitor(ServerName, Module, Args, Opts)
  when is_tuple(ServerName), is_atom(Module), is_list(Opts) ->
    gen:start(?MODULE, monitor, ServerName, Module, Args, Opts);
start_monitor(ServerName, Module, Args, Opts) ->
    error(badarg, [ServerName, Module, Args, Opts]).

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
    call(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {dirty_timeout, T} = Timeout) ->
    call(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {clean_timeout, T} = Timeout) ->
    call(ServerRef, Request, Timeout, T);
call(ServerRef, Request, {_, _} = Timeout) ->
    error(badarg, [ServerRef,Request,Timeout]);
call(ServerRef, Request, Timeout) ->
    call(ServerRef, Request, Timeout, Timeout).

-spec send_request(ServerRef::server_ref(), Request::term()) ->
        ReqId::request_id().
send_request(Name, Request) ->
    try
        gen:send_request(Name, '$gen_call', Request)
    catch
        error:badarg ->
            error(badarg, [Name, Request])
    end.

-spec send_request(ServerRef::server_ref(),
                   Request::term(),
                   Label::term(),
                   ReqIdCollection::request_id_collection()) ->
          NewReqIdCollection::request_id_collection().

send_request(ServerRef, Request, Label, ReqIdCol) ->
    try
        gen:send_request(ServerRef, '$gen_call', Request, Label, ReqIdCol)
    catch
        error:badarg ->
            error(badarg, [ServerRef, Request, Label, ReqIdCol])
    end.


-spec wait_response(ReqId) -> Result when
      ReqId :: request_id(),
      Response :: {reply, Reply::term()}
                | {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

wait_response(ReqId) ->
    wait_response(ReqId, infinity).

-spec wait_response(ReqId, WaitTime) -> Result when
      ReqId :: request_id(),
      WaitTime :: response_timeout(),
      Response :: {reply, Reply::term()}
                | {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

wait_response(ReqId, WaitTime) ->
    try
        gen:wait_response(ReqId, WaitTime)
    catch
        error:badarg ->
            error(badarg, [ReqId, WaitTime])
    end.

-spec wait_response(ReqIdCollection, WaitTime, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      WaitTime :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

wait_response(ReqIdCol, WaitTime, Delete) ->
    try
        gen:wait_response(ReqIdCol, WaitTime, Delete)
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, WaitTime, Delete])
    end.

-spec receive_response(ReqId) -> Result when
      ReqId :: request_id(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

receive_response(ReqId) ->
    receive_response(ReqId, infinity).

-spec receive_response(ReqId, Timeout) -> Result when
      ReqId :: request_id(),
      Timeout :: response_timeout(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

receive_response(ReqId, Timeout) ->
    try
        gen:receive_response(ReqId, Timeout)
    catch
        error:badarg ->
            error(badarg, [ReqId, Timeout])
    end.

-spec receive_response(ReqIdCollection, Timeout, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      Timeout :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

receive_response(ReqIdCol, Timeout, Delete) ->
    try
        gen:receive_response(ReqIdCol, Timeout, Delete)
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, Timeout, Delete])
    end.

-spec check_response(Msg, ReqId) -> Result when
      Msg :: term(),
      ReqId :: request_id(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: Response | 'no_reply'.

check_response(Msg, ReqId) ->
    try
        gen:check_response(Msg, ReqId)
    catch
        error:badarg ->
            error(badarg, [Msg, ReqId])
    end.

-spec check_response(Msg, ReqIdCollection, Delete) -> Result when
      Msg :: term(),
      ReqIdCollection :: request_id_collection(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'no_reply'.

check_response(Msg, ReqIdCol, Delete) ->
    try
        gen:check_response(Msg, ReqIdCol, Delete)
    catch
        error:badarg ->
            error(badarg, [Msg, ReqIdCol, Delete])
    end.

-spec reqids_new() ->
          NewReqIdCollection::request_id_collection().

reqids_new() ->
    gen:reqids_new().

-spec reqids_size(ReqIdCollection::request_id_collection()) ->
          non_neg_integer().

reqids_size(ReqIdCollection) ->
    try
        gen:reqids_size(ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqIdCollection])
    end.

-spec reqids_add(ReqId::request_id(), Label::term(),
                 ReqIdCollection::request_id_collection()) ->
          NewReqIdCollection::request_id_collection().

reqids_add(ReqId, Label, ReqIdCollection) ->
    try
        gen:reqids_add(ReqId, Label, ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqId, Label, ReqIdCollection])
    end.

-spec reqids_to_list(ReqIdCollection::request_id_collection()) ->
          [{ReqId::request_id(), Label::term()}].

reqids_to_list(ReqIdCollection) ->
    try
        gen:reqids_to_list(ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqIdCollection])
    end.

%% Reply from a state machine callback to whom awaits in call/2
-spec reply([reply_action()] | reply_action()) -> ok.
reply({reply,From,Reply}) ->
    reply(From, Reply);
reply(Replies) when is_list(Replies) ->
    replies(Replies).
%%
-compile({inline, [reply/2]}).
-spec reply(From :: from(), Reply :: term()) -> ok.
reply(From, Reply) ->
    gen:reply(From, Reply).

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

-compile({inline, [call/4]}).
call(ServerRef, Request, Timeout, T) ->
    try gen:call(ServerRef, '$gen_call', Request, T) of
        {ok,Reply} ->
            Reply
    catch
        %% 'gen' raises 'exit' for problems
        Class:Reason:Stacktrace when Class =:= exit ->
            erlang:raise(
              Class,
              %% Wrap the reason according to tradition
              {Reason,{?MODULE,call,[ServerRef,Request,Timeout]}},
              Stacktrace)
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
    Modules = [Module],
    P =
        #params{
           parent = Parent,
           name = Name,
           hibernate_after = HibernateAfterTimeout},
    S = #state{state_data = {State,Data}},
    case get_callback_mode(P, Modules) of
        #params{} = P_1 ->
            Debug_1 = ?sys_debug(Debug, Name, {enter,Module,State}),
            loop_enter(P_1, Debug_1, S, Q, {State,Data}, Actions_1);
        {Class, Reason, Stacktrace} ->
            P_1 = P#params{modules = Modules},
            terminate(Class, Reason, Stacktrace, P_1, Debug, S, Q)
    end.

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
	    error_info(
	      Class, Reason, Stacktrace, Debug,
              #params{parent = Parent, name = Name, modules = [Module]},
              #state{}, []),
            proc_lib:init_fail(
              Starter, {error,Reason}, {Class,Reason,Stacktrace})
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
            exit(Reason);
	{error, _Reason} = ERROR ->
            %% The point of this clause is that we shall have a *silent*
            %% termination. The error reason will be returned to the
            %% 'Starter' ({error, Reason}), but *no* crash report.
	    gen:unregister_name(ServerRef),
	    proc_lib:init_fail(Starter, ERROR, {exit,normal});
	ignore ->
	    gen:unregister_name(ServerRef),
            proc_lib:init_fail(Starter, ignore, {exit,normal});
	_ ->
	    gen:unregister_name(ServerRef),
	    Reason = {bad_return_from_init,Result},
	    error_info(
	      error, Reason, ?STACKTRACE(), Debug,
              #params{parent = Parent, name = Name, modules = [Module]},
              #state{}, []),
            exit(Reason)
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
  {#params{modules = [Module | _] = Modules} = P,
   #state{state_data = {State,Data}} = S},
  _Mod, OldVsn, Extra) ->
    case
	try Module:code_change(OldVsn, State, Data, Extra)
	catch
	    Result -> Result
	end
    of
	{ok,NewState,NewData} ->
            case get_callback_mode(P, Modules) of
                #params{} = P_1 ->
                    {ok,
                     {P_1,
                      S#state{state_data = {NewState,NewData}}}};
                {Class, Reason, Stacktrace} ->
                    erlang:raise(Class, Reason, Stacktrace)
            end;
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
   {#params{name = Name, modules = [Mod | _] = Modules},
    #state{postponed = Postponed, timers = Timers,
           state_data = {State,Data}}}]) ->
    Header = gen:format_status_header("Status for state machine", Name),

    {NumTimers, ListTimers} = list_timeouts(Timers),
    StatusMap = #{ state => State, data => Data,
                   postponed => Postponed, log => sys:get_log(Debug),
                   timeouts => ListTimers
                 },

    NewStatusMap =
        case gen:format_status(Mod, Opt, StatusMap, [PDict,State,Data]) of
            #{ 'EXIT' := R } ->
                Crashed = [{data,[{"State",{State,R}}]}],
                StatusMap#{ '$status' => Crashed };
            %% Status is set when the old format_status/2 is called,
            %% so we do a little backwards compatibility dance here
            #{ '$status' := L } = SM when is_list(L) -> SM;
            #{ '$status' := T } = SM -> SM#{ '$status' := [T] };
            #{ state := S, data := D } = SM ->
                SM#{ '$status' => [{data,[{"State",{S,D}}]}]}
        end,

    [{header,Header},
     {data,
      [{"Status",SysState},
       {"Parent",Parent},
       {"Modules",Modules},
       {"Time-outs",{NumTimers,maps:get(timeouts,NewStatusMap)}},
       {"Logged Events",maps:get(log,NewStatusMap)},
       {"Postponed",maps:get(postponed,NewStatusMap)}]} |
     maps:get('$status',NewStatusMap)].

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
        {out,Reply,{To,_Tag}} ->
            io:format(
              Dev, "*DBG* ~tp send ~tp to ~tw~n",
              [Name,Reply,To]);
        {enter,Module,State} ->
            io:format(
              Dev, "*DBG* ~tp enter ~tp in state ~tp~n",
              [Name,Module,State]);
        {module,Module,State} ->
            io:format(
              Dev, "*DBG* ~tp module ~tp in state ~tp~n",
              [Name,Module,State]);
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
                        #{TimeoutType := {TimerRef,TimeoutMsg}} = Timers
                          when TimeoutType =/= t0q->
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
  #params{name = Name} = P, Debug,
  #state{state_data = {State,_Data}} = S, Event) ->
    Debug_1 = sys_debug(Debug, Name, {in,Event,State}),
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
    %%
    %% The field 'hibernate' in S is now invalid and will be
    %% restored when looping back to loop/3 or loop_event/5.
    %%
    Q = [Event|Events],
    loop_state_callback(P, Debug, S, Q, S#state.state_data, Event);
loop_event(P, Debug, S, Event, Events) ->
    %%
    %% The field 'hibernate' in S is now invalid and will be
    %% restored when looping back to loop/3 or loop_event/5.
    %%
    Q = [Event|Events],
    loop_state_callback(P, Debug, S, Q, S#state.state_data, Event).

%% Make a state enter call to the state function, we loop back here
%% from further down if state enter calls are enabled
%%
-compile({inline, [loop_state_enter/9]}).
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

%% Loop entry point from enter/8 skipping to loop_actions_list
%% since we should not call a state callback, but initialize
%% loop variables in the same way; compare to
%% loop_state_callback/6 just below
-compile({inline, [loop_enter/6]}).
loop_enter(P, Debug, S, Q, State_Data, Actions) ->
    NextEventsR = [],
    Hibernate = false,
    TimeoutsR = [],
    Postpone = false,
    CallEnter = true,
    StateCall = true,
    loop_actions_list(
      P, Debug, S, Q, State_Data,
      NextEventsR, Hibernate, TimeoutsR, Postpone,
      CallEnter, StateCall, Actions).

%% Make a state call (not state enter call) to the state function
%%
-compile({inline, [loop_state_callback/6]}).
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
  #params{callback_mode = CallbackMode, modules = [Module | _]} = P,
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
    end.

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
        {Tag, NewModule}
          when Tag =:= change_callback_module, is_atom(NewModule);
               Tag =:= push_callback_module, is_atom(NewModule) ->
            if
                StateCall ->
                    NewModules =
                        case Tag of
                            change_callback_module ->
                                [NewModule | tl(P#params.modules)];
                            push_callback_module ->
                                [NewModule | P#params.modules]
                        end,
                    case get_callback_mode(P, NewModules) of
                        #params{} = P_1 ->
                            {NextState,_NewData} = NextState_NewData,
                            Debug_1 =
                                ?sys_debug(
                                   Debug, P#params.name,
                                   {module,NewModule,NextState}),
                            loop_actions_list(
                              P_1, Debug_1, S, Q, NextState_NewData,
                              NextEventsR, Hibernate, TimeoutsR, Postpone,
                              CallEnter, StateCall, Actions);
                        {Class, Reason, Stacktrace} ->
                            terminate(
                              Class, Reason, Stacktrace, P, Debug,
                              S#state{
                                state_data = NextState_NewData,
                                hibernate = Hibernate},
                              Q)
                    end;
                true ->
                    terminate(
                      error,
                      {bad_state_enter_action_from_state_function,Action},
                      ?STACKTRACE(), P, Debug,
                      S#state{
                        state_data = NextState_NewData,
                        hibernate = Hibernate},
                      Q)
            end;
        pop_callback_module when tl(P#params.modules) =/= [] ->
            if
                StateCall ->
                    NewModules = tl(P#params.modules),
                    case get_callback_mode(P, NewModules) of
                        #params{} = P_1 ->
                            {NextState,_NewData} = NextState_NewData,
                            Debug_1 =
                                ?sys_debug(
                                   Debug, P#params.name,
                                   {module,hd(NewModules),NextState}),
                            loop_actions_list(
                              P_1, Debug_1, S, Q, NextState_NewData,
                              NextEventsR, Hibernate, TimeoutsR, Postpone,
                              CallEnter, StateCall, Actions);
                        {Class, Reason, Stacktrace} ->
                            terminate(
                              Class, Reason, Stacktrace, P, Debug,
                              S#state{
                                state_data = NextState_NewData,
                                hibernate = Hibernate},
                              Q)
                    end;
                true ->
                    terminate(
                      error,
                      {bad_state_enter_action_from_state_function,Action},
                      ?STACKTRACE(), P, Debug,
                      S#state{
                        state_data = NextState_NewData,
                        hibernate = Hibernate},
                      Q)
            end;
	%%
        _ ->
            loop_actions_list(
              P, Debug, S, Q, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postpone,
              CallEnter, StateCall, Actions, Action)
    end.

%% Process all other actions, i.e timeout actions,
%% all others are unrecognized
%%
loop_actions_list(
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
loop_actions_list(
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
loop_actions_list(
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
loop_actions_list(
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
            if
                Debug =:= ?not_sys_debug ->
                    loop_actions_list(
                      P, Debug, S, Q, NextState_NewData,
                      [NextEvent|NextEventsR],
                      Hibernate, TimeoutsR, Postpone,
                      CallEnter, StateCall, Actions);
                true ->
                    Name = P#params.name,
                    {NextState,_NewData} = NextState_NewData,
                    Debug_1 = sys_debug(Debug, Name, {in,NextEvent,NextState}),
                    loop_actions_list(
                      P, Debug_1, S, Q, NextState_NewData,
                      [NextEvent|NextEventsR],
                      Hibernate, TimeoutsR, Postpone,
                      CallEnter, StateCall, Actions)
            end;
        _ ->
            loop_actions_next_event_bad(
              P, Debug, S, Q, NextState_NewData,
              StateCall, Hibernate, Type, Content)
    end.

loop_actions_next_event_bad(
  P, Debug, S, Q, NextState_NewData,
  StateCall, Hibernate, Type, Content) ->
    terminate(
      error,
      {case StateCall of
           true ->
               bad_action_from_state_function;
           false ->
               bad_state_enter_action_from_state_function
       end,
       {next_event,Type,Content}},
      ?STACKTRACE(), P, Debug,
      S#state{
        state_data = NextState_NewData,
        hibernate = Hibernate},
      Q).

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
	%% Optimization
	%% - only cancel timeout when it is active
	%%
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
        [_,_|_] ->
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
    %% hence no zero timeout events to append to Events
    %% - avoid loop_timeouts
    loop_done(
      P, Debug,
      S#state{
        state_data = NextState_NewData,
	postponed = Postponed,
        timers = Timers,
	hibernate = Hibernate},
      Events, NextEventsR);
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
    case TimeoutEvents of
        [] ->
            S_1 =
                S#state{
                  state_data = NextState_NewData,
                  postponed = Postponed,
                  timers = Timers,
                  hibernate = Hibernate},
            loop_done(P, Debug, S_1, Events, NextEventsR);
        [_|_] ->
            #{t0q := T0Q} = Timers,
            S_1 =
                S#state{
                  state_data = NextState_NewData,
                  postponed = Postponed,
                  timers = Timers#{t0q := T0Q ++ TimeoutEvents},
                  hibernate = Hibernate},
            loop_done(P, Debug, S_1, Events, NextEventsR)
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
        update ->
            loop_timeouts_update(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType, TimeoutMsg);
        0 ->
            %% (Re)start zero timeout
            TimerRef = 0,
            TimeoutEvents_1 = [TimeoutType | TimeoutEvents],
            loop_timeouts_register(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents_1,
              TimeoutType, Time, TimeoutMsg, TimeoutOpts, TimerRef);
        _ ->
            %% (Re)start the timer
            TimerRef =
                erlang:start_timer(Time, self(), TimeoutType, TimeoutOpts),
            loop_timeouts_register(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers, Seen, TimeoutEvents,
              TimeoutType, Time, TimeoutMsg, TimeoutOpts, TimerRef)
    end.

%% Loop helper to register a newly started timer
%% and to cancel any running timer
%%
loop_timeouts_register(
  P, Debug, S,
  Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, Time, TimeoutMsg, TimeoutOpts, TimerRef) ->
    %%
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
    end.
%%
loop_timeouts_register(
  P, Debug, S, Events, NextState_NewData,
  NextEventsR, Hibernate, TimeoutsR, Postponed,
  Timers, Seen, TimeoutEvents,
  TimeoutType, TimerRef, TimeoutMsg) ->
    %%
    case Timers of
        #{TimeoutType := {0,_OldTimeoutMsg},
          t0q := T0Q} ->
            %% Cancel the running timer,
            %% and update timer type and ref
            Timers_1 =
                Timers
                #{TimeoutType := {0,TimeoutMsg},
                  t0q := lists:delete(TimeoutType, T0Q)},
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true}, TimeoutEvents);
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
    %% to erlang:cancel_timer/1 and to maps:remove/2 within
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
%% or start a zero timeout if no timer is running
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
            Timers_1 = Timers#{TimeoutType => {0, TimeoutMsg}},
            TimeoutEvents_1 = [TimeoutType|TimeoutEvents],
            loop_timeouts(
              P, Debug, S,
              Events, NextState_NewData,
              NextEventsR, Hibernate, TimeoutsR, Postponed,
              Timers_1, Seen#{TimeoutType => true},
              TimeoutEvents_1)
    end.

%% Place inserted events first in the event queue
%%
loop_done(P, Debug, S, Events, NextEventsR) ->
    case NextEventsR of
        [] ->
            loop_done(P, Debug, S, Events);
        [E1] ->
            loop_done(P, Debug, S, [E1|Events]);
        [E2,E1] ->
            loop_done(P, Debug, S, [E1,E2|Events]);
        [_,_|_] ->
            loop_done(P, Debug, S, lists:reverse(NextEventsR, Events))
    end.
%%
%% State transition is done, keep looping if there are
%% enqueued events, or if there are zero timeouts,
%% otherwise get a new event
%%
loop_done(P, Debug, S, Q) ->
%%%    io:format(
%%%      "loop_done: state_data = ~p,~n"
%%%      "    postponed = ~p, Q = ~p,~n",
%%%      "    timers = ~p.~n"
%%%      [S#state.state_data,,S#state.postponed,Q,S#state.timers]),
    case Q of
        [] ->
            case S#state.timers of
                #{t0q := [TimeoutType|_]} = Timers ->
                    #{TimeoutType := {0 = TimerRef, TimeoutMsg}} = Timers,
                    Timers_1 = cancel_timer(TimeoutType, TimerRef, Timers),
                    S_1 = S#state{timers = Timers_1},
                    Event = {TimeoutType, TimeoutMsg},
                    loop_receive_result(P, Debug, S_1, Event);
                #{} ->
                    %% Get a new event
                    loop(P, Debug, S)
            end;
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

%% Get the callback mode, update #params{}
%%
get_callback_mode(P, [Module | _] = Modules) ->
    try Module:callback_mode() of
        CallbackModeResult ->
            callback_mode_result(P, Modules, CallbackModeResult)
    catch
        CallbackModeResult ->
            callback_mode_result(P, Modules, CallbackModeResult);
        Class:Reason:Stacktrace ->
            {Class,Reason,Stacktrace}
    end.

callback_mode_result(P, Modules, CallbackModeResult) ->
    callback_mode_result(
      P, Modules, CallbackModeResult,
      listify(CallbackModeResult), undefined, false).
%%
callback_mode_result(
  P, Modules, CallbackModeResult,
  [H|T], CallbackMode, StateEnter) ->
    case callback_mode(H) of
        true ->
            callback_mode_result(
              P, Modules, CallbackModeResult,
              T, H, StateEnter);
        false ->
            case state_enter(H) of
                true ->
                    callback_mode_result(
                      P, Modules, CallbackModeResult,
                      T, CallbackMode, true);
                false ->
                    {error,
                     {bad_return_from_callback_mode, CallbackModeResult},
                     ?STACKTRACE()}
            end
    end;
callback_mode_result(
  P, Modules, CallbackModeResult,
  [], CallbackMode, StateEnter) ->
    if
        CallbackMode =:= undefined ->
            {error,
             {bad_return_from_callback_mode, CallbackModeResult},
             ?STACKTRACE()};
        true ->
            P#params{
              modules = Modules,
              callback_mode = CallbackMode,
              state_enter = StateEnter}
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
  #params{modules = [Module | _]} = P, Debug,
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
	[_|_] ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

terminate_sys_debug(Debug, P, State, Reason) ->
    ?sys_debug(Debug, P#params.name, {terminate,Reason,State}).


error_info(
  Class, Reason, Stacktrace, Debug,
  #params{
     name = Name,
     modules = [Mod|_] = Modules,
     callback_mode = CallbackMode,
     state_enter = StateEnter},
  #state{
     postponed = Postponed,
     timers = Timers,
     state_data = {State,Data}},
  Q) ->

    {NumTimers,ListTimers} = list_timeouts(Timers),

    Status =
        gen:format_status(Mod, terminate,
                          #{ reason => Reason,
                             state => State,
                             data => Data,
                             queue => Q,
                             postponed => Postponed,
                             timeouts => ListTimers,
                             log => sys:get_log(Debug)},
                          [get(),State,Data]),
    NewState = case maps:find('$status', Status) of
                   error ->
                       {maps:get(state,Status),maps:get(data,Status)};
                   {ok, S} ->
                       S
               end,
    ?LOG_ERROR(#{label=>{gen_statem,terminate},
                 name=>Name,
                 queue=>maps:get(queue,Status),
                 postponed=>maps:get(postponed,Status),
                 modules=>Modules,
                 callback_mode=>CallbackMode,
                 state_enter=>StateEnter,
                 state=>NewState,
                 timeouts=>{NumTimers,maps:get(timeouts,Status)},
                 log=>maps:get(log,Status),
                 reason=>{Class,maps:get(reason,Status),Stacktrace},
                 client_info=>client_stacktrace(Q)},
               #{domain=>[otp],
                 report_cb=>fun gen_statem:format_log/2,
                 error_logger=>
                     #{tag=>error,
                       report_cb=>fun gen_statem:format_log/1}}).

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


%% format_log/1 is the report callback used by Logger handler
%% error_logger only. It is kept for backwards compatibility with
%% legacy error_logger event handlers. This function must always
%% return {Format,Args} compatible with the arguments in this module's
%% calls to error_logger prior to OTP-21.0.
format_log(Report) ->
    Depth = error_logger:get_format_depth(),
    FormatOpts = #{chars_limit => unlimited,
                   depth => Depth,
                   single_line => false,
                   encoding => utf8},
    format_log_multi(limit_report(Report, Depth), FormatOpts).

limit_report(Report, unlimited) ->
    Report;
limit_report(#{label:={gen_statem,terminate},
               queue:=Q,
               postponed:=Postponed,
               modules:=Modules,
               state:=FmtData,
	       timeouts:=Timeouts,
               log:=Log,
               reason:={Class,Reason,Stacktrace},
               client_info:=ClientInfo}=Report,
             Depth) ->
    Report#{queue =>
                case Q of
                    [Event|Events] ->
                        [io_lib:limit_term(Event, Depth)
                         |io_lib:limit_term(Events, Depth)];
                    _ -> []
                end,
            postponed =>
                case Postponed of
                    [] -> [];
                    _ -> io_lib:limit_term(Postponed, Depth)
                end,
            modules => io_lib:limit_term(Modules, Depth),
            state => io_lib:limit_term(FmtData, Depth),
	    timeouts =>
	    	     case Timeouts of
                         {0,_} -> Timeouts;
                         _ -> io_lib:limit_term(Timeouts, Depth)
                     end,
            log =>
                case Log of
                    [] -> [];
                    _ -> [io_lib:limit_term(T, Depth) || T <- Log]
                end,
            reason =>
                {Class,
                 io_lib:limit_term(Reason, Depth),
                 io_lib:limit_term(Stacktrace, Depth)},
            client_info => limit_client_info(ClientInfo, Depth)}.


limit_client_info({Pid,{Name,Stacktrace}}, Depth) ->
    {Pid,{Name,io_lib:limit_term(Stacktrace, Depth)}};
limit_client_info(Client, _Depth) ->
    Client.

%% format_log/2 is the report callback for any Logger handler, except
%% error_logger.
format_log(Report, FormatOpts0) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    FormatOpts = maps:merge(Default,FormatOpts0),
    IoOpts =
        case FormatOpts of
            #{chars_limit:=unlimited} ->
                [];
            #{chars_limit:=Limit} ->
                [{chars_limit,Limit}]
        end,
    {Format,Args} = format_log_single(Report, FormatOpts),
    io_lib:format(Format, Args, IoOpts).

format_log_single(#{label:={gen_statem,terminate},
                    name:=Name,
                    queue:=Q,
                    %% postponed
                    %% callback_mode
                    %% state_enter
                    state:=FmtData,
		    %% timeouts
                    log:=Log,
                    reason:={Class,Reason,Stacktrace},
                    client_info:=ClientInfo},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    {FixedReason,FixedStacktrace} = fix_reason(Class, Reason, Stacktrace),
    {ClientFmt,ClientArgs} = format_client_log_single(ClientInfo, P, Depth),
    Format =
        lists:append(
          ["State machine ",P," terminating. Reason: ",P,
           case FixedStacktrace of
               [] -> "";
               _ -> ". Stack: "++P
           end,
           case Q of
               [] -> "";
               _ -> ". Last event: "++P
           end,
           ". State: ",P,
           case Log of
               [] -> "";
               _ -> ". Log: "++P
           end,
          "."]),
    Args0 =
        [Name,FixedReason] ++
        case FixedStacktrace of
            [] -> [];
            _ -> [FixedStacktrace]
        end ++
        case Q of
            [] -> [];
            [Event|_] -> [Event]
        end ++
        [FmtData] ++
        case Log of
            [] -> [];
            _ -> [Log]
        end,
    Args = case Depth of
               unlimited ->
                   Args0;
               _ ->
                   lists:flatmap(fun(A) -> [A, Depth] end, Args0)
           end,
    {Format++ClientFmt, Args++ClientArgs};
format_log_single(Report, FormatOpts) ->
    format_log_multi(Report, FormatOpts).

format_log_multi(#{label:={gen_statem,terminate},
                   name:=Name,
                   queue:=Q,
                   postponed:=Postponed,
                   modules:=Modules,
                   callback_mode:=CallbackMode,
                   state_enter:=StateEnter,
                   state:=FmtData,
		   timeouts:=Timeouts,
                   log:=Log,
                   reason:={Class,Reason,Stacktrace},
                   client_info:=ClientInfo},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    {FixedReason,FixedStacktrace} = fix_reason(Class, Reason, Stacktrace),
    {ClientFmt,ClientArgs} = format_client_log(ClientInfo, P, Depth),
    CBMode =
	 case StateEnter of
	     true ->
		 [CallbackMode,state_enter];
	     false ->
		 CallbackMode
	 end,
    Format =
        lists:append(
          ["** State machine ",P," terminating~n",
           case Q of
               [] -> "";
               _ -> "** Last event = "++P++"~n"
           end,
           "** When server state  = ",P,"~n",
           "** Reason for termination = ",P,":",P,"~n",
           "** Callback modules = ",P,"~n",
           "** Callback mode = ",P,"~n",
           case Q of
               [_,_|_] -> "** Queued = "++P++"~n";
               _ -> ""
           end,
           case Postponed of
               [] -> "";
               _ -> "** Postponed = "++P++"~n"
           end,
           case FixedStacktrace of
               [] -> "";
               _ -> "** Stacktrace =~n**  "++P++"~n"
           end,
           case Timeouts of
               {0,_} -> "";
               _ -> "** Time-outs: "++P++"~n"
           end,
           case Log of
               [] -> "";
               _ -> "** Log =~n**  "++P++"~n"
           end]),
    Args0 =
        [Name |
         case Q of
             [] -> [];
             [Event|_] -> [Event]
         end] ++
        [FmtData,
         Class,FixedReason,
         Modules,
         CBMode] ++
        case Q of
            [_|[_|_] = Events] -> [Events];
            _ -> []
        end ++
        case Postponed of
            [] -> [];
            _ -> [Postponed]
        end ++
        case FixedStacktrace of
            [] -> [];
            _ -> [FixedStacktrace]
        end  ++
        case Timeouts of
            {0,_} -> [];
            _ -> [Timeouts]
        end ++
        case Log of
            [] -> [];
            _ -> [Log]
        end,
    Args = case Depth of
               unlimited ->
                   Args0;
               _ ->
                   lists:flatmap(fun(A) -> [A, Depth] end, Args0)
           end,
    {Format++ClientFmt,Args++ClientArgs}.

fix_reason(Class, Reason, Stacktrace) ->
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
    end.

format_client_log_single(undefined, _, _) ->
    {"", []};
format_client_log_single({Pid,dead}, _, _) ->
    {" Client ~0p is dead.", [Pid]};
format_client_log_single({Pid,remote}, _, _) ->
    {" Client ~0p is remote on node ~0p.", [Pid,node(Pid)]};
format_client_log_single({_Pid,{Name,Stacktrace0}}, P, Depth) ->
    %% Minimize the stacktrace a bit for single line reports. This is
    %% hopefully enough to point out the position.
    Stacktrace = lists:sublist(Stacktrace0, 4),
    Format = lists:append([" Client ",P," stacktrace: ",P,"."]),
    Args = case Depth of
               unlimited ->
                   [Name, Stacktrace];
               _ ->
                   [Name, Depth, Stacktrace, Depth]
           end,
    {Format, Args}.

format_client_log(undefined, _, _) ->
    {"", []};
format_client_log({Pid,dead}, _, _) ->
    {"** Client ~p is dead~n", [Pid]};
format_client_log({Pid,remote}, _, _) ->
    {"** Client ~p is remote on node ~p~n", [Pid,node(Pid)]};
format_client_log({_Pid,{Name,Stacktrace}}, P, Depth) ->
    Format = lists:append(["** Client ",P," stacktrace~n** ",P,"~n"]),
    Args = case Depth of
               unlimited ->
                   [Name, Stacktrace];
               _ ->
                   [Name, Depth, Stacktrace, Depth]
           end,
    {Format,Args}.

p(#{single_line:=Single,depth:=Depth,encoding:=Enc}) ->
    "~"++single(Single)++mod(Enc)++p(Depth);
p(unlimited) ->
    "p";
p(_Depth) ->
    "P".

single(true) -> "0";
single(false) -> "".

mod(latin1) -> "";
mod(_) -> "t".


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
%% Cancel erlang: timer and consume timeout message
%%
-compile({inline, [cancel_timer/1]}).
cancel_timer(TimerRef) ->
    ?cancel_timer(TimerRef).


-define(
   cancel_timer(TimeoutType, TimerRef, Timers),
   case (TimerRef) of
       0 ->
           maps:remove(
             begin TimeoutType end,
             maps:update(
               t0q,
               lists:delete(
                 begin TimeoutType end,
                 maps:get(t0q, begin Timers end)),
               begin Timers end));
       _ ->
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
    {maps:size(Timers) - 1, % Subtract fixed key 't0q'
     [{TimeoutType, TimeoutMsg}
      || TimeoutType := {_TimerRef, TimeoutMsg} <- Timers,
         TimeoutType =/= t0q]}.
