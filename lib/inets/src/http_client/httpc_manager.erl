%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
%%
-module(httpc_manager).

-behaviour(gen_server).

-include_lib("inets/src/http_lib/http_internal.hrl").
-include("httpc_internal.hrl").

%% Internal Application API
-export([
	 start_link/3, 
	 request/2, 
	 cancel_request/2,
	 request_done/2, 
	 retry_request/2, 
	 redirect_request/2,
	 insert_session/2, 
	 lookup_session/2, 
	 update_session/4, 
	 delete_session/2, 
	 which_sessions/1, 
	 which_session_info/1, 
	 set_options/2, 
	 get_options/2, 
	 store_cookies/3,
	 which_cookies/1, which_cookies/2, which_cookies/3, 
	 reset_cookies/1, 
	 session_type/1,
	 info/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, 
	{
	  cancel = [],	 % [{RequestId, HandlerPid, ClientPid}]  
	  handler_db,    % ets() - Entry: #handler_info{}
	  cookie_db,     % cookie_db()
	  session_db,    % ets() - Entry:  #session{}
	  profile_name,  % atom()
	  options = #options{}
	 }).

-define(DELAY, 500).


%%====================================================================
%% Internal Application API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(ProfileName, CookieDir, ManagedHow) -> {ok, Pid}
%%
%% ProfileName - httpc_manager_<Profile>
%% CookieDir - directory()
%% ManagedHow - stand_alone | inets
%%
%% Description: Starts the http request manager process. 
%% (If ManagedHow = inets then started by the inets supervisor.)
%%--------------------------------------------------------------------

start_link(Profile, CookieDir, stand_alone) ->
    ProfileName = httpc:profile_name("stand_alone_", Profile), 
    Args        = [ProfileName, CookieDir], 
    Opts        = [], 
    %% Opts        = [{debug, [log, statistics]}], 
    gen_server:start_link(?MODULE, Args, Opts);
start_link(Profile, CookieDir, _) ->
    ProfileName  = httpc:profile_name(Profile), 
    Server       = {local, ProfileName}, 
    Args         = [ProfileName, CookieDir],
    Opts         = [], 
    %% Opts        = [{debug, [log, statistics]}], 
    gen_server:start_link(Server, ?MODULE, Args, Opts).

    
%%--------------------------------------------------------------------
%% Function: request(Request, ProfileName) ->
%%                                      {ok, Requestid} | {error, Reason}
%%	Request = #request{}
%%      ProfileName = atom()
%%
%% Description: Sends a request to the httpc manager process.
%%--------------------------------------------------------------------

request(Request, ProfileName) ->
    call(ProfileName, {request, Request}).


%%--------------------------------------------------------------------
%% Function: retry_request(Request, ProfileName) -> _
%%	Request = #request{}
%%      ProfileName = atom()
%%
%% Description: Resends a request to the httpc manager process, intended
%% to be called by the httpc handler process if it has to terminate with
%% a non empty pipeline.
%%--------------------------------------------------------------------

retry_request(Request, ProfileName) ->
    cast(ProfileName, {retry_or_redirect_request, Request}).


%%--------------------------------------------------------------------
%% Function: redirect_request(Request, ProfileName) -> _
%%	Request = #request{}
%%      ProfileName = atom()
%%
%% Description: Sends an atoumatic redirect request to the httpc
%% manager process, intended to be called by the httpc handler process
%% when the automatic redirect option is set.
%%--------------------------------------------------------------------

redirect_request(Request, ProfileName) ->
    cast(ProfileName, {retry_or_redirect_request, Request}).


%%--------------------------------------------------------------------
%% Function: cancel_request(RequestId, ProfileName) -> ok
%%	RequestId - ref()
%%      ProfileName = atom()
%%
%% Description: Cancels the request with <RequestId>.
%%--------------------------------------------------------------------

cancel_request(RequestId, ProfileName) ->
    cast(ProfileName, {cancel_request, RequestId}).

%%--------------------------------------------------------------------
%% Function: request_done(RequestId, ProfileName) -> ok
%%	RequestId - ref()
%%      ProfileName = atom()
%%
%% Description: Inform tha manager that a request has been completed.
%%--------------------------------------------------------------------

request_done(RequestId, ProfileName) ->
    cast(ProfileName, {request_done, RequestId}).


%%--------------------------------------------------------------------
%% Function: insert_session(Session, ProfileName) -> _
%%	Session - #session{}
%%      ProfileName - atom()
%%
%% Description: Inserts session information into the httpc manager
%% table <ProfileName>_session_db. Intended to be called by
%% the httpc request handler process.
%%--------------------------------------------------------------------

insert_session(Session, ProfileName) ->
    SessionDbName = session_db_name(ProfileName), 
    ?hcrt("insert session", [{session, Session}, {profile, ProfileName}]),
    ets:insert(SessionDbName, Session).


%%--------------------------------------------------------------------
%% Function: lookup_session(SessionId, ProfileName) -> _
%%      SessionId - term()
%%      ProfileName - atom()
%%
%% Description: Looks up a session record in the httpc manager
%% table <ProfileName>__session_db. 
%%--------------------------------------------------------------------

lookup_session(SessionId, ProfileName) ->
    SessionDbName = session_db_name(ProfileName), 
    ?hcrt("lookup session", [{session_id, SessionId}, {profile, ProfileName}]),
    ets:lookup(SessionDbName, SessionId).


%%--------------------------------------------------------------------
%% Function: update_session(ProfileName, SessionId, Pos, Value) -> _
%%	Session - #session{}
%%      ProfileName - atom()
%%
%% Description: Update, only one field (Pos) of the session record
%%              identified by the SessionId, the session information 
%%              of the httpc manager table <ProfileName>__session_db. 
%%              Intended to be called by the httpc request handler process.
%%--------------------------------------------------------------------

update_session(ProfileName, SessionId, Pos, Value) ->
    SessionDbName = session_db_name(ProfileName), 
    ?hcrt("update session", 
	  [{id,      SessionId}, 
	   {pos,     Pos}, 
	   {value,   Value}, 
	   {profile, ProfileName}]),
    ets:update_element(SessionDbName, SessionId, {Pos, Value}).


%%--------------------------------------------------------------------
%% Function: delete_session(SessionId, ProfileName) -> void()
%%	SessionId -  {{Host, Port}, HandlerPid}
%%      ProfileName - atom()
%% 
%% Description: Deletes session information from the httpc manager
%% table <ProfileName>__session_db. Intended to be called by
%% the httpc request handler process.
%%--------------------------------------------------------------------

delete_session(SessionId, ProfileName) ->
    SessionDbName = session_db_name(ProfileName), 
    ?hcrt("delete session", [{session_is, SessionId}, {profile, ProfileName}]),
    ets:delete(SessionDbName, SessionId).


%%--------------------------------------------------------------------
%% Function: which sessions(ProfileName) -> SessionsInfo
%%      ProfileName - atom()
%%      SessionsInfo - {GoodSessions, BadSessions, NonSessions}
%%      GoodSessions - [#session{}]
%%      BadSessions  - [tuple()]
%%      NonSessions  - [term()]
%%
%% Description: Produces a list of all sessions in the session db.
%% Used for debugging and since that is the intent, there is some
%% checking and transforming done, which produces the results.
%%--------------------------------------------------------------------

which_sessions(ProfileName) ->
    ?hcrt("which_sessions", [{profile, ProfileName}]),
    SessionDbName = session_db_name(ProfileName), 
    which_sessions2(SessionDbName).

which_sessions2(SessionDbName) ->
    Sessions     = which_sessions_order(ets:tab2list(SessionDbName)),
    GoodSessions = [GoodSession || {good_session, GoodSession} <- Sessions],
    BadSessions  = [BadSession  || {bad_session,  BadSession}  <- Sessions],
    NonSessions  = [NonSession  || {non_session,  NonSession}  <- Sessions],
    {lists:keysort(#session.id, GoodSessions), 
     lists:keysort(#session.id, BadSessions), 
     lists:sort(NonSessions)}.

which_sessions_order([]) ->
    [];
which_sessions_order([Session|Sessions]) when is_record(Session, session) ->
    [{good_session, Session} | which_sessions_order(Sessions)];
which_sessions_order([BadSession|Sessions]) 
  when is_tuple(BadSession) andalso 
       (element(1, BadSession) =:= session) ->
    [{bad_session, BadSession} | which_sessions_order(Sessions)];
which_sessions_order([NonSession|Sessions]) ->
    [{non_session, NonSession} | which_sessions_order(Sessions)].


%%--------------------------------------------------------------------
%% Function: which session_info(ProfileName) -> list()
%%
%% Description: Produces a ets table info list of the sessions table
%%--------------------------------------------------------------------

which_session_info(ProfileName) ->
    SessionDbName = session_db_name(ProfileName), 
    ?hcrt("which_session_info", [{profile, ProfileName}]),
    ets:info(SessionDbName).


%%--------------------------------------------------------------------
%% Function: set_options(Options, ProfileName) -> ok
%%
%%	Options = [Option]
%%	Option = {proxy, {Proxy, [NoProxy]}} 
%%              | {max_pipeline_length, integer()} |
%%                {max_sessions, integer()} | {pipeline_timeout, integer()}
%%	Proxy = {Host, Port}
%%	NoProxy - [Domain | HostName | IPAddress]     
%%	Max - integer() 
%%	ProfileName = atom()
%% 
%% Description: Sets the options to be used by the client.
%%--------------------------------------------------------------------

set_options(Options, ProfileName) ->
    cast(ProfileName, {set_options, Options}).


%%--------------------------------------------------------------------
%% Function: get_options(OptionItems, ProfileName) -> Values
%%
%%	OptionItems = [OptionItem]
%%	OptionItem = Any or all fields of the current #options{} record 
%%      Values = [{OptionItem, Value}]
%%      Value = term()
%% 
%% Description: Gets the specified options used by the client.
%%--------------------------------------------------------------------

get_options(Options, ProfileName) ->
    call(ProfileName, {get_options, Options}).


%%--------------------------------------------------------------------
%% Function: store_cookies(Cookies, Address, ProfileName) -> ok
%%
%%	Cookies = [Cookie]
%%	Cookie = #http_cookie{}
%%	ProfileName = atom()
%% 
%% Description: Stores cookies from the server.
%%--------------------------------------------------------------------
store_cookies([], _, _) ->
    ok;
store_cookies(Cookies, Address, ProfileName) ->
    cast(ProfileName, {store_cookies, {Cookies, Address}}).


%%--------------------------------------------------------------------
%% Function: reset_cookies(ProfileName) -> void()
%%
%%	Url = string()
%%      ProfileName = atom()
%%
%% Description: Resets the cookie database
%%--------------------------------------------------------------------

reset_cookies(ProfileName) ->
    call(ProfileName, reset_cookies).


%%--------------------------------------------------------------------
%% Function: which_cookies(ProfileName)               -> [cookie()]
%%           which_cookies(Url, ProfileName)          -> [cookie()]
%%           which_cookies(Url, Options, ProfileName) -> [cookie()]
%%
%%	Url = string()
%%	Options = [option()]
%%      ProfileName = atom()
%%      option() = {ipv6_host_with_brackets, boolean()}
%%
%% Description: Retrieves the cookies that would be sent when 
%% requesting <Url>.
%%--------------------------------------------------------------------

which_cookies(ProfileName) when is_atom(ProfileName) ->
    call(ProfileName, which_cookies).

which_cookies(Url, ProfileName) 
  when is_list(Url) andalso is_atom(ProfileName) ->
    which_cookies(Url, [], ProfileName).

which_cookies(Url, Options, ProfileName) 
  when is_list(Url) andalso is_list(Options) andalso is_atom(ProfileName) ->
    call(ProfileName, {which_cookies, Url, Options}).


%%--------------------------------------------------------------------
%% Function: info(ProfileName) -> list()
%%
%%      ProfileName = atom()
%%
%% Description: Retrieves various info about the manager and the
%%              handlers it manages
%%--------------------------------------------------------------------

info(ProfileName) ->
    call(ProfileName, info).


%%--------------------------------------------------------------------
%% Function: session_type(Options) -> ok
%%
%%	Options = #options{}
%%
%% Description: Determines if to use pipelined sessions or not.
%%--------------------------------------------------------------------

session_type(#options{pipeline_timeout = 0}) -> 
    keep_alive;
session_type(_) ->
    pipeline.


%%====================================================================
%% gen_server callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([ProfileName, CookiesConf]) -> {ok, State} | 
%%                       {ok, State, Timeout} | ignore |{stop, Reason}
%% Description: Initiates the httpc_manger process
%%--------------------------------------------------------------------
init([ProfileName, CookiesDir]) ->
    process_flag(trap_exit, true),
    ?hcrv("starting", [{profile, ProfileName}]),
    case (catch do_init(ProfileName, CookiesDir)) of
	{ok, _} = OK ->
	    ?hcrd("started", [OK]),
	    OK;
	{error, Reason} ->
	    {stop, Reason};
	Crap ->
	    {stop, Crap}
    end.


do_init(ProfileName, CookiesDir) ->
    %% Create session db
    ?hcrt("create session db", []),
    SessionDbName = session_db_name(ProfileName), 
    ets:new(SessionDbName, 
	    [public, set, named_table, {keypos, #session.id}]),

    %% Create handler db
    ?hcrt("create handler/request db", []),
    HandlerDbName = handler_db_name(ProfileName), 
    ets:new(HandlerDbName, [protected, set, named_table, {keypos, 1}]),

    %% Cookie DB
    ?hcrt("create cookie db", []),
    SessionCookieDbName = session_cookie_db_name(ProfileName), 
    CookieDbName        = cookie_db_name(ProfileName), 
    CookieDb            = httpc_cookie:open_db(CookieDbName, CookiesDir, 
					       SessionCookieDbName),

    State = #state{handler_db   = HandlerDbName, 
		   cookie_db    = CookieDb, 
		   session_db   = SessionDbName,
		   profile_name = ProfileName}, 
    {ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({request, Request}, _, State) ->
    ?hcri("request", [{request, Request}]),
    case (catch handle_request(Request, State)) of
	{reply, Msg, NewState} ->
	    {reply, Msg, NewState};
	Error ->
	    {stop, Error, httpc_response:error(Request, Error), State}
    end;

handle_call(reset_cookies, _, #state{cookie_db = CookieDb} = State) ->
    ?hcrv("reset cookies", []),
    httpc_cookie:reset_db(CookieDb),
    {reply, ok, State};

handle_call(which_cookies, _, #state{cookie_db = CookieDb} = State) ->
    ?hcrv("which cookies", []),
    CookieHeaders = httpc_cookie:which_cookies(CookieDb),
    {reply, CookieHeaders, State};

handle_call({which_cookies, Url, Options}, _, 
	    #state{cookie_db = CookieDb} = State) ->
    ?hcrv("which cookies", [{url, Url}, {options, Options}]),
    case uri_parse(Url, Options) of
	{ok, {Scheme, _, Host, Port, Path, _}} ->
	    CookieHeaders = 
		httpc_cookie:header(CookieDb, Scheme, {Host, Port}, Path),
	    {reply, CookieHeaders, State};
	{error, _} = ERROR ->
	    {reply, ERROR, State}
    end;

handle_call({get_options, OptionItems}, _, #state{options = Options} = State) ->
    ?hcrv("get options", [{option_items, OptionItems}]),
    Reply = [{OptionItem, get_option(OptionItem, Options)} || 
		OptionItem <- OptionItems], 
    {reply, Reply, State};

handle_call(info, _, State) ->
    ?hcrv("info", []),
    Info = get_manager_info(State), 
    {reply, Info, State};

handle_call(Req, From, #state{profile_name = ProfileName} = State) ->
    error_report(ProfileName, 
		 "received unkown request"
		 "~n   Req:  ~p"
		 "~n   From: ~p", [Req, From]),
    {reply, {error, 'API_violation'}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({retry_or_redirect_request, {Time, Request}}, 
	    #state{profile_name = ProfileName} = State) ->
    {ok, _} = timer:apply_after(Time, ?MODULE, retry_request, [Request, ProfileName]),
    {noreply, State};

handle_cast({retry_or_redirect_request, Request}, State) ->
    case (catch handle_request(Request, State)) of
	{reply, {ok, _}, NewState} ->
	    {noreply, NewState};
	Error  ->
	    httpc_response:error(Request, Error),
	    {stop, Error, State}
    end;

handle_cast({cancel_request, RequestId}, 
	    #state{handler_db = HandlerDb} = State) ->
    case ets:lookup(HandlerDb, RequestId) of
	[] ->
	    %% Request already compleated nothing to 
	    %% cancel
	    {noreply, State};
	[{_, Pid, _}] ->
	    httpc_handler:cancel(RequestId, Pid),
	    ets:delete(State#state.handler_db, RequestId),
	    {noreply, State}
    end;
  
handle_cast({request_done, RequestId}, State) ->
    ?hcrv("request done", [{request_id, RequestId}]),
    ets:delete(State#state.handler_db, RequestId),
    {noreply, State};

handle_cast({set_options, Options}, State = #state{options = OldOptions}) ->
    ?hcrv("set options", [{options, Options}, {old_options, OldOptions}]),
    NewOptions = 
	#options{proxy                 = get_proxy(Options, OldOptions),
		 https_proxy           = get_https_proxy(Options, OldOptions),
		 pipeline_timeout      = get_pipeline_timeout(Options, OldOptions), 
		 max_pipeline_length   = get_max_pipeline_length(Options, OldOptions), 
		 max_keep_alive_length = get_max_keep_alive_length(Options, OldOptions), 
		 keep_alive_timeout    = get_keep_alive_timeout(Options, OldOptions), 
		 max_sessions          = get_max_sessions(Options, OldOptions), 
		 cookies               = get_cookies(Options, OldOptions), 
		 ipfamily              = get_ipfamily(Options, OldOptions), 
		 ip                    = get_ip(Options, OldOptions),
		 port                  = get_port(Options, OldOptions),
		 verbose               = get_verbose(Options, OldOptions),
		 socket_opts           = get_socket_opts(Options, OldOptions)
		}, 
    case {OldOptions#options.verbose, NewOptions#options.verbose} of
	{Same, Same} ->
	    ok;
	{_, false} ->
	    dbg:stop();
	{false, Level}  ->
	    dbg:tracer(),
	    handle_verbose(Level);
	{_, Level} ->
	    dbg:stop(),
	    dbg:tracer(),
	    handle_verbose(Level)
    end,
    {noreply, State#state{options = NewOptions}};

handle_cast({store_cookies, _}, 
	    State = #state{options = #options{cookies = disabled}}) ->
    {noreply, State};

handle_cast({store_cookies, {Cookies, _}}, State) ->
    ok = do_store_cookies(Cookies, State),
    {noreply, State};

handle_cast(Msg, #state{profile_name = ProfileName} = State) ->
    error_report(ProfileName, 
		 "recived unknown message"
		 "~n   Msg: ~p", [Msg]),
    {noreply, State}.
	    
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling all non call/cast messages
%%---------------------------------------------------------
handle_info({'EXIT', _, _}, State) ->
    %% Handled in DOWN
    {noreply, State};
handle_info({'DOWN', _, _, Pid, _}, State) ->
    ets:match_delete(State#state.handler_db, {'_', Pid, '_'}),  
    {noreply, State};
handle_info(Info, State) ->
    Report = io_lib:format("Unknown message in "
			   "httpc_manager:handle_info ~p~n", [Info]),
    error_logger:error_report(Report),
    {noreply, State}. 

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> _  (ignored by gen_server)
%% Description: Shutdown the httpc_handler
%%--------------------------------------------------------------------
terminate(_, State) ->
    httpc_cookie:close_db(State#state.cookie_db),
    ets:delete(State#state.session_db),
    ets:delete(State#state.handler_db).


%%--------------------------------------------------------------------
%% Func: code_change(_OldVsn, State, Extra) -> {ok, NewState}
%% Purpose: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_, 
	    #state{session_db = SessionDB} = State, 
	    upgrade_from_pre_5_8_1) ->
    Upgrade = 
	fun({session, 
	     Id, ClientClose, Scheme, Socket, SocketType, QueueLen, Type}) ->
		{ok, #session{id           = Id, 
			      client_close = ClientClose, 
			      scheme       = Scheme, 
			      socket       = Socket, 
			      socket_type  = SocketType,
			      queue_length = QueueLen, 
			      type         = Type}};
	   (_) -> % Already upgraded (by handler)
		ignore
	end,
    (catch update_session_table(SessionDB, Upgrade)),
    {ok, State};

code_change(_, 
	    #state{session_db = SessionDB} = State, 
	    downgrade_to_pre_5_8_1) ->
    Downgrade = 
	fun(#session{id           = Id, 
		     client_close = ClientClose, 
		     scheme       = Scheme, 
		     socket       = Socket, 
		     socket_type  = SocketType,
		     queue_length = QueueLen, 
		     type         = Type}) ->
		{ok, {session, 
		      Id, ClientClose, Scheme, Socket, SocketType, 
		      QueueLen, Type}};
	   (_) -> % Already downgraded (by handler)
		ignore
	end,
    (catch update_session_table(SessionDB, Downgrade)),
    {ok, State};

code_change(_, State, _) ->
    {ok, State}.

%% This function is used to catch everything that falls through the cracks...
update_session_table(SessionDB, Transform) ->
    ets:safe_fixtable(SessionDB, true),
    update_session_table(SessionDB, ets:first(SessionDB), Transform),
    ets:safe_fixtable(SessionDB, false).

update_session_table(_SessionDB, '$end_of_table', _Transform) ->
    ok;
update_session_table(SessionDB, Key, Transform) ->
    case ets:lookup(SessionDB, Key) of
	[OldSession] ->
	    case Transform(OldSession) of
		{ok, NewSession} -> 
		    ets:insert(SessionDB, NewSession);
		ignore ->
		    ok
	    end;
	_ ->
	    ok
    end,
    update_session_table(SessionDB, ets:next(SessionDB, Key), Transform).


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

get_manager_info(#state{handler_db = HDB, 
			session_db = SDB, 
			cookie_db  = CDB, 
			options    = Options} = _State) ->
    HandlerInfo = get_handler_info(HDB),
    SessionInfo = which_sessions2(SDB), 
    OptionsInfo = 
	[{Item, get_option(Item, Options)} || 
		  Item <- record_info(fields, options)], 
    CookieInfo  = httpc_cookie:which_cookies(CDB),
    [{handlers, HandlerInfo}, 
     {sessions, SessionInfo}, 
     {options,  OptionsInfo}, 
     {cookies,  CookieInfo}].

sort_handlers(Unsorted) ->
    sort_handlers2(lists:keysort(1, Unsorted)).

sort_handlers2([]) ->
    [];
sort_handlers2([{HandlerPid, RequestId}|L]) ->
    {Handler, Rest} = sort_handlers2(HandlerPid, [RequestId], L),
    [Handler | sort_handlers2(Rest)].

sort_handlers2(HandlerPid, Reqs, []) ->
    {{HandlerPid, lists:sort(Reqs)}, []};
sort_handlers2(HandlerPid, Reqs, [{HandlerPid, ReqId}|Rest]) ->
    sort_handlers2(HandlerPid, [ReqId|Reqs], Rest);
sort_handlers2(HandlerPid1, Reqs, [{HandlerPid2, _}|_] = Rest) 
  when HandlerPid1 =/= HandlerPid2 ->
    {{HandlerPid1, lists:sort(Reqs)}, Rest}.

get_handler_info(Tab) ->
    Pattern   = {'$2', '$1', '_'},
    Handlers1 = [{Pid, Id} || [Pid, Id] <- ets:match(Tab, Pattern)],
    Handlers2 = sort_handlers(Handlers1), 
    [{Pid, Reqs, httpc_handler:info(Pid)} || {Pid, Reqs} <- Handlers2].

handle_request(#request{settings = 
			#http_options{version = "HTTP/0.9"}} = Request,
	       State) ->
    %% Act as an HTTP/0.9 client that does not know anything
    %% about persistent connections

    NewRequest = handle_cookies(generate_request_id(Request), State),
    NewHeaders =
	(NewRequest#request.headers)#http_request_h{connection
						    = undefined},
    start_handler(NewRequest#request{headers = NewHeaders}, State),
    {reply, {ok, NewRequest#request.id}, State};

handle_request(#request{settings = 
			#http_options{version = "HTTP/1.0"}} = Request,
	       State) ->
    %% Act as an HTTP/1.0 client that does not
    %% use persistent connections

    NewRequest = handle_cookies(generate_request_id(Request), State),
    NewHeaders =
	(NewRequest#request.headers)#http_request_h{connection
						    = "close"},
    start_handler(NewRequest#request{headers = NewHeaders}, State),
    {reply, {ok, NewRequest#request.id}, State};

handle_request(Request, State = #state{options = Options}) ->

    NewRequest = handle_cookies(generate_request_id(Request), State),
    SessionType = session_type(Options),
    case select_session(Request#request.method,
			Request#request.address,
			Request#request.scheme, SessionType, State) of
	{ok, HandlerPid} ->
	    pipeline_or_keep_alive(NewRequest, HandlerPid, State);
	no_connection ->
	    start_handler(NewRequest, State);
	{no_session,  OpenSessions} when OpenSessions
	< Options#options.max_sessions ->
	    start_handler(NewRequest, State);
	{no_session, _} ->
	    %% Do not start any more persistent connections
	    %% towards this server.
	    NewHeaders =
		(NewRequest#request.headers)#http_request_h{connection
							    = "close"},
	    start_handler(NewRequest#request{headers = NewHeaders}, State)
    end,
    {reply, {ok, NewRequest#request.id}, State}.


start_handler(#request{id   = Id, 
		       from = From} = Request, 
	      #state{profile_name = ProfileName, 
		     handler_db   = HandlerDb, 
		     options      = Options}) ->
    {ok, Pid} =
	case is_inets_manager() of
	    true ->
		httpc_handler_sup:start_child([whereis(httpc_handler_sup),
					       Request, Options, ProfileName]);
	    false ->
		httpc_handler:start_link(self(), Request, Options, ProfileName)
	end,
    HandlerInfo = {Id, Pid, From}, 
    ets:insert(HandlerDb, HandlerInfo), 
    erlang:monitor(process, Pid).


select_session(Method, HostPort, Scheme, SessionType, 
	       #state{options = #options{max_pipeline_length   = MaxPipe,
					 max_keep_alive_length = MaxKeepAlive},
		      session_db = SessionDb}) ->
    ?hcrd("select session", [{session_type,          SessionType},
			     {max_pipeline_length,   MaxPipe},
			     {max_keep_alive_length, MaxKeepAlive}]),
    case httpc_request:is_idempotent(Method) orelse 
	(SessionType =:= keep_alive) of
	true ->
	    %% Look for handlers connecting to this host (HostPort)
	    %% session with record name field (session) and 
	    %% socket fields ignored. The fields id (part of: HostPort), 
	    %% client_close, scheme and type specified. 
	    %% The fields id (part of: HandlerPid) and queue_length
	    %% specified.
	    Pattern = #session{id           = {HostPort, '$1'},
			       client_close = false,
			       scheme       = Scheme,
			       queue_length = '$2',
			       type         = SessionType,
			       available    = true, 
			       _            = '_'},
	    %% {'_', {HostPort, '$1'}, false, Scheme, '_', '$2', SessionTyp}, 
	    Candidates = ets:match(SessionDb, Pattern), 
	    ?hcrd("select session", [{host_port,  HostPort}, 
				     {scheme,     Scheme}, 
				     {type,       SessionType}, 
				     {candidates, Candidates}]),	    
	    select_session(Candidates, MaxKeepAlive, MaxPipe, SessionType);
	false ->
	    no_connection
    end.
    
select_session(Candidates, Max, _, keep_alive) ->
    select_session(Candidates, Max);
select_session(Candidates, _, Max, pipeline) ->
    select_session(Candidates, Max).

select_session([] = _Candidates, _Max) ->
    ?hcrd("select session - no candicate", []),
    no_connection; 
select_session(Candidates, Max) ->
    NewCandidates = 
	[{Pid, Length} || [Pid, Length] <- Candidates, Length =< Max],
    case lists:keysort(2, NewCandidates) of
	[] ->
	    {no_session, length(Candidates)};
	[{HandlerPid, _} | _] ->
	    ?hcrd("select session - found one", [{handler, HandlerPid}]),
	    {ok, HandlerPid}
    end.

pipeline_or_keep_alive(#request{id   = Id, 
				from = From} = Request, 
		       HandlerPid, 
		       #state{handler_db = HandlerDb} = State) ->
    case (catch httpc_handler:send(Request, HandlerPid)) of
	ok ->
	    HandlerInfo = {Id, HandlerPid, From}, 
	    ets:insert(HandlerDb, HandlerInfo);
	_  -> % timeout pipelining failed
	    start_handler(Request, State)
    end.

is_inets_manager() ->
    case get('$ancestors') of
	[httpc_profile_sup | _] ->
	    true;
	_ ->
	    false
    end.

generate_request_id(Request) ->
    case Request#request.id of
	undefined ->
	    RequestId = make_ref(),
	    Request#request{id = RequestId};
	_ ->
	    %% This is an automatic redirect or a retryed pipelined request 
	    %% => keep the old id.
	    Request
    end.

handle_cookies(Request, #state{options = #options{cookies = disabled}}) ->
    Request;
handle_cookies(
  #request{scheme  = Scheme, 
	   address = Address,
	   path    = Path, 
	   headers = #http_request_h{other = Other} = Hdrs} = Request, 
  #state{cookie_db = CookieDb}) ->
    case httpc_cookie:header(CookieDb, Scheme, Address, Path) of
	{"cookie", ""} ->
	    Request;
	CookieHeader ->
	    NewHeaders = Hdrs#http_request_h{other = [CookieHeader | Other]},
	    Request#request{headers = NewHeaders}
    end.

do_store_cookies([], _) ->
    ok;
do_store_cookies([Cookie | Cookies], #state{cookie_db = CookieDb} = State) ->
    ok = httpc_cookie:insert(CookieDb, Cookie),
    do_store_cookies(Cookies, State).

session_db_name(ProfileName) ->
    make_db_name(ProfileName, "__session_db").

cookie_db_name(ProfileName) ->
    make_db_name(ProfileName, "__cookie_db").

session_cookie_db_name(ProfileName) ->
    make_db_name(ProfileName, "__session_cookie_db").

handler_db_name(ProfileName) ->
    make_db_name(ProfileName, "__handler_db").

make_db_name(ProfileName, Post) ->
    list_to_atom(atom_to_list(ProfileName) ++ Post).
    

%%--------------------------------------------------------------------------
%% These functions is just simple wrappers to parse specifically HTTP URIs
%%--------------------------------------------------------------------------

scheme_defaults() ->
    [{http, 80}, {https, 443}].

uri_parse(URI, Opts) ->
    http_uri:parse(URI, [{scheme_defaults, scheme_defaults()} | Opts]).


%%--------------------------------------------------------------------------


call(ProfileName, Msg) ->
    Timeout = infinity, 
    call(ProfileName, Msg, Timeout).
call(ProfileName, Msg, Timeout) ->
    gen_server:call(ProfileName, Msg, Timeout).

cast(ProfileName, Msg) ->
   gen_server:cast(ProfileName, Msg).


get_option(proxy, #options{proxy = Proxy}) ->
    Proxy;
get_option(https_proxy, #options{https_proxy = Proxy}) ->
    Proxy;
get_option(pipeline_timeout, #options{pipeline_timeout = Timeout}) ->
    Timeout;
get_option(max_pipeline_length, #options{max_pipeline_length = Length}) ->
    Length;
get_option(keep_alive_timeout, #options{keep_alive_timeout = Timeout}) ->
    Timeout;
get_option(max_keep_alive_length, #options{max_keep_alive_length = Length}) ->
    Length;
get_option(max_sessions, #options{max_sessions = MaxSessions}) ->
    MaxSessions;
get_option(cookies, #options{cookies = Cookies}) ->
    Cookies;
get_option(verbose, #options{verbose = Verbose}) ->
    Verbose;
get_option(ipfamily, #options{ipfamily = IpFamily}) ->
    IpFamily;
get_option(ip, #options{ip = IP}) ->
    IP;
get_option(port, #options{port = Port}) ->
    Port;
get_option(socket_opts, #options{socket_opts = SocketOpts}) ->
    SocketOpts.

get_proxy(Opts, #options{proxy = Default}) ->
    proplists:get_value(proxy, Opts, Default).

get_https_proxy(Opts, #options{https_proxy = Default}) ->
    proplists:get_value(https_proxy, Opts, Default).

get_pipeline_timeout(Opts, #options{pipeline_timeout = Default}) ->
    proplists:get_value(pipeline_timeout, Opts, Default).

get_max_pipeline_length(Opts, #options{max_pipeline_length = Default}) ->
    proplists:get_value(max_pipeline_length, Opts, Default).

get_max_keep_alive_length(Opts, #options{max_keep_alive_length = Default}) ->
    proplists:get_value(max_keep_alive_length, Opts, Default).

get_keep_alive_timeout(Opts, #options{keep_alive_timeout = Default}) ->
    proplists:get_value(keep_alive_timeout, Opts, Default).

get_max_sessions(Opts, #options{max_sessions = Default}) ->
    proplists:get_value(max_sessions, Opts, Default).

get_cookies(Opts, #options{cookies = Default}) ->
    proplists:get_value(cookies, Opts, Default).

get_ipfamily(Opts, #options{ipfamily = IpFamily}) ->
    case lists:keysearch(ipfamily, 1, Opts) of
	false -> 
	    case proplists:get_value(ipv6, Opts) of
		enabled ->
		    inet6fb4;
		disabled ->
		    inet;
		_ ->
		    IpFamily
	    end;
	{value, {_, Value}} ->
	    Value
    end.

get_ip(Opts, #options{ip = Default}) ->
    proplists:get_value(ip, Opts, Default).

get_port(Opts, #options{port = Default}) ->
    proplists:get_value(port, Opts, Default).

get_verbose(Opts, #options{verbose = Default}) ->
    proplists:get_value(verbose, Opts, Default).

get_socket_opts(Opts, #options{socket_opts = Default}) ->
    proplists:get_value(socket_opts, Opts, Default).


handle_verbose(debug) ->
    dbg:p(self(), [call]),
    dbg:tp(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(trace) ->
    dbg:p(self(), [call]),
    dbg:tpl(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(_) ->
    ok.  

error_report(Profile, F, A) ->
    Report = io_lib:format("HTTPC-MANAGER<~p> " ++ F ++ "~n", [Profile | A]), 
    error_logger:error_report(Report).
