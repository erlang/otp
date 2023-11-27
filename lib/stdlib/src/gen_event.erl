%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
-module(gen_event).

%%% 
%%% A general event handler.
%%% Several handlers (functions) can be added.
%%% Each handler holds a state and will be called
%%% for every event received of the handler.
%%% 

%%% Modified by Magnus.
%%%       Take care of fault situations and made notify asynchronous.
%%% Re-written by Joe with new functional interface !
%%% Modified by Martin - uses proc_lib, sys and gen!

%%%
%%% NOTE: If init_ack() return values are modified, see comment
%%%       above monitor_return() in gen.erl!
%%%

-export([start/0, start/1, start/2,
         start_link/0, start_link/1, start_link/2,
         start_monitor/0, start_monitor/1, start_monitor/2,
         stop/1, stop/3,
	 notify/2, sync_notify/2,
	 add_handler/3, add_sup_handler/3, delete_handler/3, swap_handler/3,
	 swap_sup_handler/3, which_handlers/1, call/3, call/4,
         send_request/3, send_request/5,
         wait_response/2, receive_response/2, check_response/2,
         wait_response/3, receive_response/3, check_response/3,
         reqids_new/0, reqids_size/1,
         reqids_add/3, reqids_to_list/1,
         wake_hib/5]).

-export([init_it/6,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 system_get_state/1,
	 system_replace_state/2,
	 format_status/2]).

%% logger callback
-export([format_log/1, format_log/2]).

-export_type([handler/0, handler_args/0, add_handler_ret/0,
              del_handler_ret/0, request_id/0, request_id_collection/0,
              format_status/0]).

-record(handler, {module             :: atom(),
		  id = false,
		  state,
		  supervised = false :: 'false' | pid()}).

-include("logger.hrl").

%%%=========================================================================
%%%  API
%%%=========================================================================

%% gen_event:start(Handler) -> {ok, Pid} | {error, What}
%%   gen_event:add_handler(Handler, Mod, Args) -> ok | Other
%%      gen_event:notify(Handler, Event) -> ok
%%      gen_event:call(Handler, Mod, Query) -> {ok, Val} | {error, Why}
%%      gen_event:call(Handler, Mod, Query, Timeout) -> {ok, Val} | {error, Why}
%%   gen_event:delete_handler(Handler, Mod, Args) -> Val
%%   gen_event:swap_handler(Handler, {OldMod, Args1}, {NewMod, Args2}) -> ok
%%   gen_event:which_handler(Handler) -> [Mod]
%% gen_event:stop(Handler) -> ok 

-callback init(InitArgs :: term()) ->
    {ok, State :: term()} |
    {ok, State :: term(), hibernate} |
    {error, Reason :: term()}.
-callback handle_event(Event :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), hibernate} |
    {swap_handler, Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler.
-callback handle_call(Request :: term(), State :: term()) ->
    {ok, Reply :: term(), NewState :: term()} |
    {ok, Reply :: term(), NewState :: term(), hibernate} |
    {swap_handler, Reply :: term(), Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    {remove_handler, Reply :: term()}.
-callback handle_info(Info :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), hibernate} |
    {swap_handler, Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler.
-callback terminate(Args :: (term() | {stop, Reason :: term()} |
                             stop | remove_handler |
                             {error, {'EXIT', Reason :: term()}} |
                             {error, term()}),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}),
                      State :: term(), Extra :: term()) ->
    {ok, NewState :: term()}.
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().
-type format_status() ::
        #{ state => term(),
           message => term(),
           reason => term(),
           log => [sys:system_event()] }.
-callback format_status(Status) -> NewStatus when
      Status    :: format_status(),
      NewStatus :: format_status().

-optional_callbacks(
    [handle_info/2, terminate/2, code_change/3, format_status/1, format_status/2]).

%%---------------------------------------------------------------------------

-type handler()          :: atom() | {atom(), term()}.
-type handler_args()     :: term().
-type add_handler_ret()  :: ok | term() | {'EXIT',term()}.
-type del_handler_ret()  :: ok | term() | {'EXIT',term()}.

-type emgr_name() :: {'local', atom()} | {'global', term()}
                   | {'via', atom(), term()}.
-type debug_flag() :: 'trace' | 'log' | 'statistics' | 'debug'
                    | {'logfile', string()}.
-type option() :: {'timeout', timeout()}
                | {'debug', [debug_flag()]}
                | {'spawn_opt', [proc_lib:start_spawn_option()]}
                | {'hibernate_after', timeout()}.
-type emgr_ref()  :: atom() | {atom(), atom()} |  {'global', term()}
                   | {'via', atom(), term()} | pid().
-type start_ret() :: {'ok', pid()} | {'error', term()}.
-type start_mon_ret() :: {'ok', {pid(),reference()}} | {'error', term()}.

-opaque request_id() :: gen:request_id().

-opaque request_id_collection() :: gen:request_id_collection().

-type response_timeout() ::
        timeout() | {abs, integer()}.

%%---------------------------------------------------------------------------

-define(NO_CALLBACK, 'no callback module').

%% -----------------------------------------------------------------
%% Starts a generic event handler.
%% start()
%% start(MgrName | Options)
%% start(MgrName, Options)
%% start_link()
%% start_link(MgrName | Options)
%% start_link(MgrName, Options)
%%    MgrName ::= {local, atom()} | {global, term()} | {via, atom(), term()}
%%    Options ::= [{timeout, Timeout} | {debug, [Flag]} | {spawn_opt,SOpts}]
%%       Flag ::= trace | log | {logfile, File} | statistics | debug
%%          (debug == log && statistics)
%% Returns: {ok, Pid} |
%%          {error, {already_started, Pid}} |
%%          {error, Reason}
%% -----------------------------------------------------------------

-spec start() -> start_ret().
start() ->
    gen:start(?MODULE, nolink, ?NO_CALLBACK, [], []).

-spec start(emgr_name() | [option()]) -> start_ret().
start(Name) when is_tuple(Name) ->
    gen:start(?MODULE, nolink, Name, ?NO_CALLBACK, [], []);
start(Options) when is_list(Options) ->
    gen:start(?MODULE, nolink, ?NO_CALLBACK, [], Options);
start(Arg) ->
    error(badarg, [Arg]).

-spec start(emgr_name(), [option()]) -> start_ret().
start(Name, Options) when is_tuple(Name), is_list(Options) ->
    gen:start(?MODULE, nolink, Name, ?NO_CALLBACK, [], Options);
start(Name, Options) ->
    error(badarg, [Name, Options]).

-spec start_link() -> start_ret().
start_link() ->
    gen:start(?MODULE, link, ?NO_CALLBACK, [], []).

-spec start_link(emgr_name() | [option()]) -> start_ret().
start_link(Name) when is_tuple(Name) ->
    gen:start(?MODULE, link, Name, ?NO_CALLBACK, [], []);
start_link(Options) when is_list(Options) ->
    gen:start(?MODULE, link, ?NO_CALLBACK, [], Options);
start_link(Arg) ->
    error(badarg, [Arg]).

-spec start_link(emgr_name(), [option()]) -> start_ret().
start_link(Name, Options) when is_tuple(Name), is_list(Options) ->
    gen:start(?MODULE, link, Name, ?NO_CALLBACK, [], Options);
start_link(Name, Options) ->
    error(badarg, [Name, Options]).

-spec start_monitor() -> start_mon_ret().
start_monitor() ->
    gen:start(?MODULE, monitor, ?NO_CALLBACK, [], []).

-spec start_monitor(emgr_name() | [option()]) -> start_mon_ret().
start_monitor(Name) when is_tuple(Name) ->
    gen:start(?MODULE, monitor, Name, ?NO_CALLBACK, [], []);
start_monitor(Options) when is_list(Options) ->
    gen:start(?MODULE, monitor, ?NO_CALLBACK, [], Options);
start_monitor(Arg) ->
    error(badarg, [Arg]).

-spec start_monitor(emgr_name(), [option()]) -> start_mon_ret().
start_monitor(Name, Options) when is_tuple(Name), is_list(Options) ->
    gen:start(?MODULE, monitor, Name, ?NO_CALLBACK, [], Options);
start_monitor(Name, Options) ->
    error(badarg, [Name, Options]).

%% -spec init_it(pid(), 'self' | pid(), emgr_name(), module(), [term()], [_]) -> 
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, _, _, Options) ->
    process_flag(trap_exit, true),
    Name = gen:name(Name0),
    Debug = gen:debug_options(Name, Options),
	HibernateAfterTimeout = gen:hibernate_after(Options),
    proc_lib:init_ack(Starter, {ok, self()}),
    loop(Parent, Name, [], HibernateAfterTimeout, Debug, false).

-spec add_handler(emgr_ref(), handler(), term()) -> term().
add_handler(M, Handler, Args) -> rpc(M, {add_handler, Handler, Args}).

-spec add_sup_handler(emgr_ref(), handler(), term()) -> term().
add_sup_handler(M, Handler, Args)  ->
    rpc(M, {add_sup_handler, Handler, Args, self()}).

-spec notify(emgr_ref(), term()) -> 'ok'.
notify(M, Event) -> send(M, {notify, Event}).

-spec sync_notify(emgr_ref(), term()) -> 'ok'.
sync_notify(M, Event) -> rpc(M, {sync_notify, Event}).

-spec call(emgr_ref(), handler(), term()) -> term().
call(M, Handler, Query) -> call1(M, Handler, Query).

-spec call(emgr_ref(), handler(), term(), timeout()) -> term().
call(M, Handler, Query, Timeout) -> call1(M, Handler, Query, Timeout).

-spec send_request(EventMgrRef::emgr_ref(), Handler::handler(), Request::term()) ->
          ReqId::request_id().
send_request(M, Handler, Request) ->
    try
        gen:send_request(M, self(), {call, Handler, Request})
    catch
        error:badarg ->
            error(badarg, [M, Handler, Request])
    end.

-spec send_request(EventMgrRef::emgr_ref(),
                   Handler::handler(),
                   Request::term(),
                   Label::term(),
                   ReqIdCollection::request_id_collection()) ->
          NewReqIdCollection::request_id_collection().
send_request(M, Handler, Request, Label, ReqIdCol) ->
    try
        gen:send_request(M, self(), {call, Handler, Request}, Label, ReqIdCol)
    catch
        error:badarg ->
            error(badarg, [M, Handler, Request, Label, ReqIdCol])
    end.

-spec wait_response(ReqId, WaitTime) -> Result when
      ReqId :: request_id(),
      WaitTime :: response_timeout(),
      Response :: {reply, Reply::term()}
                | {error, {Reason::term(), emgr_ref()}},
      Result :: Response | 'timeout'.

wait_response(ReqId, WaitTime) ->
    try gen:wait_response(ReqId, WaitTime) of
        {reply, {error, _} = Err} -> Err;
        Return -> Return
    catch
        error:badarg ->
            error(badarg, [ReqId, WaitTime])
    end.

-spec wait_response(ReqIdCollection, WaitTime, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      WaitTime :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

wait_response(ReqIdCol, WaitTime, Delete) ->
    try gen:wait_response(ReqIdCol, WaitTime, Delete) of
        {{reply, {error, _} = Err}, Label, NewReqIdCol} ->
            {Err, Label, NewReqIdCol};
        Return ->
            Return
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, WaitTime, Delete])
    end.

-spec receive_response(ReqId, Timeout) -> Result when
      ReqId :: request_id(),
      Timeout :: response_timeout(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: Response | 'timeout'.

receive_response(ReqId, Timeout) ->
    try gen:receive_response(ReqId, Timeout) of
        {reply, {error, _} = Err} -> Err;
        Return -> Return
    catch
        error:badarg ->
            error(badarg, [ReqId, Timeout])
    end.

-spec receive_response(ReqIdCollection, Timeout, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      Timeout :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

receive_response(ReqIdCol, Timeout, Delete) ->
    try gen:receive_response(ReqIdCol, Timeout, Delete) of
        {{reply, {error, _} = Err}, Label, NewReqIdCol} ->
            {Err, Label, NewReqIdCol};
        Return ->
            Return
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, Timeout, Delete])
    end.

-spec check_response(Msg, ReqId) -> Result when
      Msg :: term(),
      ReqId :: request_id(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: Response | 'no_reply'.

check_response(Msg, ReqId) ->
    try gen:check_response(Msg, ReqId) of
        {reply, {error, _} = Err} -> Err;
        Return -> Return
    catch
        error:badarg ->
            error(badarg, [Msg, ReqId])
    end.

-spec check_response(Msg, ReqIdCollection, Delete) -> Result when
      Msg :: term(),
      ReqIdCollection :: request_id_collection(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), emgr_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'no_reply'.

check_response(Msg, ReqIdCol, Delete) ->
    try gen:check_response(Msg, ReqIdCol, Delete) of
        {{reply, {error, _} = Err}, Label, NewReqIdCol} ->
            {Err, Label, NewReqIdCol};
        Return ->
            Return
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

-spec delete_handler(emgr_ref(), handler(), term()) -> term().
delete_handler(M, Handler, Args) -> rpc(M, {delete_handler, Handler, Args}).

-spec swap_handler(emgr_ref(), {handler(), term()}, {handler(), term()}) ->
	    'ok' | {'error', term()}.
swap_handler(M, {H1, A1}, {H2, A2}) -> rpc(M, {swap_handler, H1, A1, H2, A2}).

-spec swap_sup_handler(emgr_ref(), {handler(), term()}, {handler(), term()}) ->
	    'ok' | {'error', term()}.
swap_sup_handler(M, {H1, A1}, {H2, A2}) ->
    rpc(M, {swap_sup_handler, H1, A1, H2, A2, self()}).

-spec which_handlers(emgr_ref()) -> [handler()].
which_handlers(M) -> rpc(M, which_handlers).

-spec stop(emgr_ref()) -> 'ok'.
stop(M) ->
    gen:stop(M).

stop(M, Reason, Timeout) ->
    gen:stop(M, Reason, Timeout).

rpc(M, Cmd) ->
    {ok, Reply} = gen:call(M, self(), Cmd, infinity),
    Reply.

call1(M, Handler, Query) ->
    Cmd = {call, Handler, Query},
    try gen:call(M, self(), Cmd) of
	{ok, Res} ->
	    Res
    catch
	exit:Reason ->
	    exit({Reason, {?MODULE, call, [M, Handler, Query]}})
    end.

call1(M, Handler, Query, Timeout) ->
    Cmd = {call, Handler, Query},
    try gen:call(M, self(), Cmd, Timeout) of
	{ok, Res} ->
	    Res
    catch
	exit:Reason ->
	    exit({Reason, {?MODULE, call, [M, Handler, Query, Timeout]}})
    end.

send({global, Name}, Cmd) ->
    catch global:send(Name, Cmd),
    ok;
send({via, Mod, Name}, Cmd) ->
    catch Mod:send(Name, Cmd),
    ok;
send(M, Cmd) ->
    M ! Cmd,
    ok.

loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true) ->
     proc_lib:hibernate(?MODULE, wake_hib, [Parent, ServerName, MSL, HibernateAfterTimeout, Debug]);
loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, _) ->
    fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false).

wake_hib(Parent, ServerName, MSL, HibernateAfterTimeout, Debug) ->
    fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true).

fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib) ->
    receive
	Msg ->
	    decode_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib)
    after HibernateAfterTimeout ->
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true)
    end.

decode_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib) ->
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [ServerName, MSL, HibernateAfterTimeout, Hib],Hib);
	{'EXIT', Parent, Reason} ->
	    terminate_server(Reason, Parent, MSL, ServerName);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, []);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      ServerName, {in, Msg}),
	    handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug1)
    end.

handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug) ->
    case Msg of
	{notify, Event} ->
	    {Hib,MSL1} = server_notify(Event, handle_event, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {sync_notify, Event}} ->
	    {Hib, MSL1} = server_notify(Event, handle_event, MSL, ServerName),
	    reply(Tag, ok),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{'EXIT', From, Reason} ->
	    MSL1 = handle_exit(From, Reason, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, false);
	{_From, Tag, {call, Handler, Query}} ->
	    {Hib, Reply, MSL1} = server_call(Handler, Query, MSL, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {add_handler, Handler, Args}} ->
	    {Hib, Reply, MSL1} = server_add_handler(Handler, Args, MSL),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {add_sup_handler, Handler, Args, SupP}} ->
	    {Hib, Reply, MSL1} = server_add_sup_handler(Handler, Args, MSL, SupP),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {delete_handler, Handler, Args}} ->
	    {Reply, MSL1} = server_delete_handler(Handler, Args, MSL,
						  ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, false);
	{_From, Tag, {swap_handler, Handler1, Args1, Handler2, Args2}} ->
	    {Hib, Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
						     Args2, MSL, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {swap_sup_handler, Handler1, Args1, Handler2, Args2,
		     Sup}} ->
	    {Hib, Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
						Args2, MSL, Sup, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, stop} ->
	    catch terminate_server(normal, Parent, MSL, ServerName),
	    reply(Tag, ok);
	{_From, Tag, which_handlers} ->
	    reply(Tag, the_handlers(MSL)),
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false);
	{_From, Tag, get_modules} ->
	    reply(Tag, get_modules(MSL)),
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false);
	Other  ->
	    {Hib, MSL1} = server_notify(Other, handle_info, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib)
    end.

terminate_server(Reason, Parent, MSL, ServerName) ->
    stop_handlers(MSL, ServerName),
    do_unlink(Parent, MSL),
    exit(Reason).

reply(From, Reply) ->
    gen:reply(From, Reply).

%% unlink the supervisor process of all supervised handlers.
%% We do not want a handler supervisor to EXIT due to the
%% termination of the event manager (server).
%% Do not unlink Parent !
do_unlink(Parent, MSL) ->
    lists:foreach(fun(Handler) when Handler#handler.supervised =:= Parent ->
			  true;
		     (Handler) when is_pid(Handler#handler.supervised) ->
			  unlink(Handler#handler.supervised),
			  true;
		     (_) ->
			  true
		  end,
		  MSL).

%% First terminate the supervised (if exists) handlers and
%% then inform other handlers.
%% We do not know if any handler really is interested but it
%% may be so !
handle_exit(From, Reason, MSL, SName) ->
    MSL1 = terminate_supervised(From, Reason, MSL, SName),
    {_,MSL2}=server_notify({'EXIT', From, Reason}, handle_info, MSL1, SName),
    MSL2.

terminate_supervised(Pid, Reason, MSL, SName) ->
    F = fun(Ha) when Ha#handler.supervised =:= Pid ->
		do_terminate(Ha#handler.module,
			     Ha,
			     {stop,Reason},
			     Ha#handler.state,
			     {parent_terminated, {Pid,Reason}},
			     SName,
			     shutdown),
		false;
	   (_) ->
		true
	end,
    lists:filter(F, MSL).

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [ServerName, MSL, HibernateAfterTimeout, Hib]) ->
    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib).

-spec system_terminate(_, _, _, [_]) -> no_return().
system_terminate(Reason, Parent, _Debug, [ServerName, MSL, _HibernateAfterTimeout, _Hib]) ->
    terminate_server(Reason, Parent, MSL, ServerName).

%%-----------------------------------------------------------------
%% Module here is sent in the system msg change_code.  It specifies
%% which module should be changed.
%%-----------------------------------------------------------------
system_code_change([ServerName, MSL, HibernateAfterTimeout, Hib], Module, OldVsn, Extra) ->
    MSL1 = lists:zf(fun(H) when H#handler.module =:= Module ->
			    {ok, NewState} =
				Module:code_change(OldVsn,
						   H#handler.state, Extra),
			    {true, H#handler{state = NewState}};
		       (_) -> true
		    end,
		    MSL),
    {ok, [ServerName, MSL1, HibernateAfterTimeout, Hib]}.

system_get_state([_ServerName, MSL, _HibernateAfterTimeout, _Hib]) ->
    {ok, [{Mod,Id,State} || #handler{module=Mod, id=Id, state=State} <- MSL]}.

system_replace_state(StateFun, [ServerName, MSL, HibernateAfterTimeout, Hib]) ->
    {NMSL, NStates} =
		lists:unzip([begin
				 Cur = {Mod,Id,State},
				 try
				     NState = {Mod,Id,NS} = StateFun(Cur),
				     {HS#handler{state=NS}, NState}
				 catch
				     _:_ ->
					 {HS, Cur}
				 end
			     end || #handler{module=Mod, id=Id, state=State}=HS <- MSL]),
    {ok, NStates, [ServerName, NMSL, HibernateAfterTimeout, Hib]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{notify, Event} ->
	    io:format(Dev, "*DBG* ~tp got event ~tp~n", [Name, Event]);
	{_,_,{call, Handler, Query}} ->
	    io:format(Dev, "*DBG* ~tp(~tp) got call ~tp~n",
		      [Name, Handler, Query]);
	_ ->
	    io:format(Dev, "*DBG* ~tp got ~tp~n", [Name, Msg])
    end;
print_event(Dev, Dbg, Name) ->
    io:format(Dev, "*DBG* ~tp : ~tp~n", [Name, Dbg]).


%% server_add_handler(Handler, Args, MSL) -> {Ret, MSL'}.
%%   where MSL = [#handler{}]
%%   Ret goes to the top level MSL' is the new internal state of the
%%   event handler

server_add_handler({Mod,Id}, Args, MSL) ->
    Handler = #handler{module = Mod,
		       id = Id},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_handler(Mod, Args, MSL) ->
    Handler = #handler{module = Mod},
    server_add_handler(Mod, Handler, Args, MSL).

server_add_handler(Mod, Handler, Args, MSL) ->
    case catch Mod:init(Args) of
        {ok, State} ->
	    {false, ok, [Handler#handler{state = State}|MSL]};
        {ok, State, hibernate} ->
	    {true, ok, [Handler#handler{state = State}|MSL]};
        Other ->
            {false, Other, MSL}
    end.

%% Set up a link to the supervising process.
%% (Ought to be unidirected links here, Erl5.0 !!)
%% NOTE: This link will not be removed then the
%% handler is removed in case another handler has
%% own link to this process.
server_add_sup_handler({Mod,Id}, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
		       id = Id,
		       supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_sup_handler(Mod, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
		       supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL).

%% server_delete_handler(HandlerId, Args, MSL) -> {Ret, MSL'}

server_delete_handler(HandlerId, Args, MSL, SName) ->
    case split(HandlerId, MSL) of
	{Mod, Handler, MSL1} ->
	    {do_terminate(Mod, Handler, Args,
			  Handler#handler.state, delete, SName, normal),
	     MSL1};
	error ->
	    {{error, module_not_found}, MSL}
    end.

%% server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SN) -> MSL'
%% server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SN) -> MSL'

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SName) ->
    {State2, Sup, MSL1} = split_and_terminate(Handler1, Args1, MSL,
					      SName, Handler2, false),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
	{Hib, ok, MSL2} ->
	    {Hib, ok, MSL2};
	{Hib, What, MSL2} ->
	    {Hib, {error, What}, MSL2}
    end.

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SName) ->
    {State2, _, MSL1} = split_and_terminate(Handler1, Args1, MSL,
					    SName, Handler2, Sup),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
	{Hib, ok, MSL2} ->
	    {Hib, ok, MSL2};
	{Hib, What, MSL2} ->
	    {Hib, {error, What}, MSL2}
    end.

s_s_h(false, Handler, Args, MSL) ->
    server_add_handler(Handler, Args, MSL);
s_s_h(Pid, Handler, Args, MSL) ->
    server_add_sup_handler(Handler, Args, MSL, Pid).

split_and_terminate(HandlerId, Args, MSL, SName, Handler2, Sup) ->
    case split(HandlerId, MSL) of
	{Mod, Handler, MSL1} ->
	    OldSup = Handler#handler.supervised,
	    NewSup = if
			 not Sup -> OldSup;
			 true    -> Sup
		     end,
	    {do_terminate(Mod, Handler, Args,
			  Handler#handler.state, swapped, SName,
			  {swapped, Handler2, NewSup}),
	     OldSup,
	     MSL1};
	error ->
            {error, false, MSL}
    end.

%% server_notify(Event, Func, MSL, SName) -> MSL'

server_notify(Event, Func, [Handler|T], SName) ->
    case server_update(Handler, Func, Event, SName) of
	{ok, Handler1} ->
	    {Hib, NewHandlers} = server_notify(Event, Func, T, SName),
	    {Hib, [Handler1|NewHandlers]};
	{hibernate, Handler1} ->
	    {_Hib, NewHandlers} = server_notify(Event, Func, T, SName),
	    {true, [Handler1|NewHandlers]};
	no ->
	    server_notify(Event, Func, T, SName)
    end;
server_notify(_, _, [], _) ->
    {false, []}.

%% server_update(Handler, Func, Event, ServerName) -> Handler1 | no

server_update(Handler1, Func, Event, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:Func(Event, State) of
	{ok, State1} ->
	    {ok, Handler1#handler{state = State1}};
	{ok, State1, hibernate} ->
	    {hibernate, Handler1#handler{state = State1}};
	{swap_handler, Args1, State1, Handler2, Args2} ->
	    do_swap(Mod1, Handler1, Args1, State1, Handler2, Args2, SName);
	remove_handler ->
	    do_terminate(Mod1, Handler1, remove_handler, State,
			 remove, SName, normal),
	    no;
        {'EXIT', {undef, [{Mod1, handle_info, [_,_], _}|_]}} ->
            ?LOG_WARNING(#{label=>{gen_event,no_handle_info},
                           module=>Mod1,
                           message=>Event},
                         #{domain=>[otp],
                           report_cb=>fun gen_event:format_log/2,
                           error_logger=>
                               #{tag=>warning_msg, % warningmap??
                                 report_cb=>fun gen_event:format_log/1}}),
            {ok, Handler1};
	Other ->
	    do_terminate(Mod1, Handler1, {error, Other}, State,
			 Event, SName, crash),
	    no
    end.

do_swap(Mod1, Handler1, Args1, State1, Handler2, Args2, SName) ->
    %% finalise the existing handler
    State2 = do_terminate(Mod1, Handler1, Args1, State1,
			  swapped, SName,
			  {swapped, Handler2, Handler1#handler.supervised}),
    {Mod2, Handler} = new_handler(Handler2, Handler1),
    case catch Mod2:init({Args2, State2}) of
	{ok, State2a} ->
	    {ok, Handler#handler{state = State2a}};
	Other ->
	    report_terminate(Handler, crash, {error, Other}, SName, false),
	    no
    end.

new_handler({Mod,Id}, Handler1) ->
    {Mod, #handler{module = Mod,
		   id = Id,
		   supervised = Handler1#handler.supervised}};
new_handler(Mod, Handler1) ->
    {Mod, #handler{module = Mod,
		   supervised = Handler1#handler.supervised}}.


-spec split(handler(), [#handler{}]) ->
	{atom(), #handler{}, [#handler{}]} | 'error'.

split(Ha, MSL) -> split(Ha, MSL, []).

split({Mod,Id}, [Ha|T], L) when Ha#handler.module =:= Mod,
                                Ha#handler.id =:= Id ->
    {Mod, Ha, lists:reverse(L, T)};
split(Mod, [Ha|T], L) when Ha#handler.module =:= Mod,
                           not Ha#handler.id ->
    {Mod, Ha, lists:reverse(L, T)};
split(Ha, [H|T], L) ->
    split(Ha, T, [H|L]);
split(_, [], _) ->
    error.

%% server_call(Handler, Query, MSL, ServerName) ->
%%    {Reply, MSL1}

server_call(Handler, Query, MSL, SName) ->
    case search(Handler, MSL) of
	{ok, Ha} ->
	    case server_call_update(Ha, Query, SName) of
		{no, Reply} ->
		    {false, Reply, delete(Handler, MSL)};
		{{ok, Ha1}, Reply} ->
		    {false, Reply, replace(Handler, MSL, Ha1)};
		{{hibernate, Ha1}, Reply} ->
		    {true, Reply, replace(Handler, MSL, Ha1)}
	    end;
	false ->
	    {false, {error, bad_module}, MSL}
    end.

search({Mod, Id}, [Ha|_MSL]) when Ha#handler.module =:= Mod,
				  Ha#handler.id =:= Id ->
    {ok, Ha};
search(Mod, [Ha|_MSL]) when Ha#handler.module =:= Mod,
			    not Ha#handler.id ->
    {ok, Ha};
search(Handler, [_|MSL]) ->
    search(Handler, MSL);
search(_, []) ->
    false.

delete({Mod, Id}, [Ha|MSL]) when Ha#handler.module =:= Mod,
                                 Ha#handler.id =:= Id ->
    MSL;
delete(Mod, [Ha|MSL]) when Ha#handler.module =:= Mod,
                           not Ha#handler.id ->
    MSL;
delete(Handler, [Ha|MSL]) ->
    [Ha|delete(Handler, MSL)];
delete(_, []) ->
    [].

replace({Mod, Id}, [Ha|MSL], NewHa) when Ha#handler.module =:= Mod,
                                         Ha#handler.id =:= Id ->
    [NewHa|MSL];
replace(Mod, [Ha|MSL], NewHa) when Ha#handler.module =:= Mod,
                                   not Ha#handler.id ->
    [NewHa|MSL];
replace(Handler, [Ha|MSL], NewHa) ->
    [Ha|replace(Handler, MSL, NewHa)];
replace(_, [], NewHa) ->
    [NewHa].

%% server_call_update(Handler, Query, ServerName) ->
%%    {{Handler1, State1} | 'no', Reply}

server_call_update(Handler1, Query, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:handle_call(Query, State) of
	{ok, Reply, State1} ->
	    {{ok, Handler1#handler{state = State1}}, Reply};
	{ok, Reply, State1, hibernate} ->
	    {{hibernate, Handler1#handler{state = State1}},
	     Reply};
	{swap_handler, Reply, Args1, State1, Handler2, Args2} ->
	    {do_swap(Mod1,Handler1,Args1,State1,Handler2,Args2,SName), Reply};
	{remove_handler, Reply} ->
	    do_terminate(Mod1, Handler1, remove_handler, State,
			 remove, SName, normal),
	    {no, Reply};
	Other ->
	    do_terminate(Mod1, Handler1, {error, Other}, State,
			 Query, SName, crash),
	    {no, {error, Other}}
    end.

do_terminate(Mod, Handler, Args, State, LastIn, SName, Reason) ->
    case erlang:function_exported(Mod, terminate, 2) of
	true ->
	    Res = (catch Mod:terminate(Args, State)),
	    report_terminate(Handler, Reason, Args, State, LastIn, SName, Res),
	    Res;
	false ->
	    report_terminate(Handler, Reason, Args, State, LastIn, SName, ok),
	    ok
    end.

-spec report_terminate(_, How, _, _, _, _, _) -> ok when
      How :: crash | normal | shutdown | {swapped, handler(), false | pid()}.
report_terminate(Handler, crash, {error, Why}, State, LastIn, SName, _) ->
    report_terminate(Handler, Why, State, LastIn, SName);
report_terminate(Handler, How, _, State, LastIn, SName, _) ->
    %% How == normal | shutdown | {swapped, NewHandler, NewSupervisor}
    report_terminate(Handler, How, State, LastIn, SName).

report_terminate(Handler, Reason, State, LastIn, SName) ->
    report_error(Handler, Reason, State, LastIn, SName),
    case Handler#handler.supervised of
	false ->
	    ok;
	Pid ->
	    Pid ! {gen_event_EXIT,handler(Handler),Reason},
	    ok
    end.

report_error(_Handler, normal, _, _, _)             -> ok;
report_error(_Handler, shutdown, _, _, _)           -> ok;
report_error(_Handler, {swapped,_,_}, _, _, _)      -> ok;
report_error(Handler, Exit, State, LastIn, SName) ->

    %% The reason comes from a catch expression, so we remove
    %% the 'EXIT' and stacktrace from it so that the format_status
    %% callback does not have deal with that.
    {Reason, ReasonFun} =
        case Exit of
            {'EXIT',{R,ST}} ->
                {R, fun(Reason) -> {'EXIT',{Reason,ST}} end};
            {'EXIT',R} ->
                {R, fun(Reason) -> {'EXIT',Reason} end};
            R ->
                {R, fun(Reason) -> Reason end}
        end,
    Status = gen:format_status(
               Handler#handler.module,
               terminate,
               #{ state => State,
                  message => LastIn,
                  reason => Reason
                },
               [get(), State]),
    ?LOG_ERROR(#{label=>{gen_event,terminate},
                 handler=>handler(Handler),
                 name=>SName,
                 last_message=>maps:get(message,Status),
                 state=>maps:get('$status',Status,maps:get(state,Status)),
                 reason=>ReasonFun(maps:get(reason,Status))},
               #{domain=>[otp],
                 report_cb=>fun gen_event:format_log/2,
                 error_logger=>#{tag=>error,
                                 report_cb=>fun gen_event:format_log/1}}).

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
limit_report(#{label:={gen_event,terminate},
               last_message:=LastIn,
               state:=State,
               reason:=Reason}=Report,
             Depth) ->
    Report#{last_message => io_lib:limit_term(LastIn, Depth),
            state => io_lib:limit_term(State, Depth),
            reason => io_lib:limit_term(Reason, Depth)};
limit_report(#{label:={gen_event,no_handle_info},
               message:=Msg}=Report,
             Depth) ->
    Report#{message => io_lib:limit_term(Msg, Depth)}.

%% format_log/2 is the report callback for any Logger handler, except
%% error_logger.
format_log(Report, FormatOpts0) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    FormatOpts = maps:merge(Default, FormatOpts0),
    IoOpts =
        case FormatOpts of
            #{chars_limit:=unlimited} ->
                [];
            #{chars_limit:=Limit} ->
                [{chars_limit,Limit}]
        end,
    {Format,Args} = format_log_single(Report, FormatOpts),
    io_lib:format(Format, Args, IoOpts).

format_log_single(#{label:={gen_event,terminate},
                    handler:=Handler,
                    name:=SName,
                    last_message:=LastIn,
                    state:=State,
                    reason:=Reason},
                  #{single_line:=true, depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Reason1 = fix_reason(Reason),
    Format1 = lists:append(["Generic event handler ",P," crashed. "
                            "Installed: ",P,". Last event: ",P,
                            ". State: ",P,". Reason: ",P,"."]),
    Args1 =
        case Depth of
            unlimited ->
                [Handler,SName,LastIn,State,Reason1];
            _ ->
                [Handler,Depth,SName,Depth,LastIn,Depth,
                 State,Depth,Reason1,Depth]
        end,
    {Format1, Args1};
format_log_single(#{label:={gen_event,no_handle_info},
                    module:=Mod,
                    message:=Msg},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format = lists:append(["Undefined handle_info in ",P,
                           ". Unhandled message: ",P,"."]),
    Args =
        case Depth of
            unlimited ->
                [Mod,Msg];
            _ ->
                [Mod,Depth,Msg,Depth]
        end,
    {Format,Args};
format_log_single(Report,FormatOpts) ->
    format_log_multi(Report,FormatOpts).

format_log_multi(#{label:={gen_event,terminate},
                   handler:=Handler,
                   name:=SName,
                   last_message:=LastIn,
                   state:=State,
                   reason:=Reason},
                 #{depth:=Depth}=FormatOpts) ->
    Reason1 = fix_reason(Reason),
    P = p(FormatOpts),
    Format =
        lists:append(["** gen_event handler ",P," crashed.\n",
                      "** Was installed in ",P,"\n",
                      "** Last event was: ",P,"\n",
                      "** When handler state == ",P,"\n",
                      "** Reason == ",P,"\n"]),
    Args =
        case Depth of
            unlimited ->
                [Handler,SName,LastIn,State,Reason1];
            _ ->
                [Handler,Depth,SName,Depth,LastIn,Depth,State,Depth,
                 Reason1,Depth]
        end,
    {Format,Args};
format_log_multi(#{label:={gen_event,no_handle_info},
                   module:=Mod,
                   message:=Msg},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format =
        "** Undefined handle_info in ~p\n"
        "** Unhandled message: "++P++"\n",
    Args =
        case Depth of
            unlimited ->
                [Mod,Msg];
            _ ->
                [Mod,Msg,Depth]
        end,
    {Format,Args}.

fix_reason({'EXIT',{undef,[{M,F,A,_L}|_]=MFAs}=Reason}) ->
    case code:is_loaded(M) of
        false ->
            {'module could not be loaded',MFAs};
        _ ->
            case erlang:function_exported(M, F, length(A)) of
                true ->
                    Reason;
                false ->
                    {'function not exported',MFAs}
            end
    end;
fix_reason({'EXIT',Reason}) ->
    Reason;
fix_reason(Reason) ->
    Reason.

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

handler(Handler) when not Handler#handler.id ->
    Handler#handler.module;
handler(Handler) ->
    {Handler#handler.module, Handler#handler.id}.

the_handlers(MSL) ->
    [handler(Handler) || Handler <- MSL].

%% stop_handlers(MSL, ServerName) -> []

stop_handlers([Handler|T], SName) ->
    Mod = Handler#handler.module,
    do_terminate(Mod, Handler, stop, Handler#handler.state,
		 stop, SName, shutdown),
    stop_handlers(T, SName);
stop_handlers([], _) ->
    [].

%% Message from the release_handler.
%% The list of modules got to be a set, i.e. no duplicate elements!
get_modules(MSL) ->
    Mods = [Handler#handler.module || Handler <- MSL],
    ordsets:to_list(ordsets:from_list(Mods)).

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [ServerName, MSL, _HibernateAfterTimeout, _Hib]] = StatusData,
    Header = gen:format_status_header("Status for event handler", ServerName),
    {FmtMSL, Logs} =
        lists:mapfoldl(
          fun(#handler{module = Mod, state = State} = MS, Logs) ->
                  Status = gen:format_status(
                             Mod, Opt, #{ log => Logs, state => State },
                             [PDict, State]),
                  {MS#handler{state=maps:get('$status',Status,maps:get(state,Status))},
                   maps:get(log,Status)}
          end, sys:get_log(Debug), MSL),
    [{header, Header},
     {data, [{"Status", SysState},
             {"Logged Events", Logs},
	     {"Parent", Parent}]},
     {items, {"Installed handlers", FmtMSL}}].
