%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
	 request_canceled/2, 
	 request_done/2, 
	 retry_request/2, 
	 redirect_request/2,
	 insert_session/2, 
	 delete_session/2, 
	 set_options/2, 
	 store_cookies/3,
	 which_cookies/1, which_cookies/2, 
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
	  session_db,    % ets() - Entry:  #tcp_session{}
	  profile_name,  % atom()
	  options = #options{}
	 }).

-record(handler_info, 
	{
	  id,      % Id of the request:          request_id()
	  starter, % Pid of the handler starter process (temp): pid()
	  handler, % Pid of the handler process: pid()
	  from,    % From for the request:  from()
	  state    % State of the handler: initiating | started | operational | canceled
	 }).

%% Entries in the handler / request cross-ref table
%% -record(request_info, 
%% 	{
%% 	  id,      % Id of the request
%% 	  handler, % Pid of the handler process
%% 	  from,    % The From value for the caller
%% 	  mref     % Monitor ref for the caller
%% 	 }).


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
    call(ProfileName, {cancel_request, RequestId}).


%%--------------------------------------------------------------------
%% Function: request_canceled(RequestId, ProfileName) -> ok
%%	RequestId - ref()
%%      ProfileName = atom()
%%
%% Description: Confirms that a request has been canceld. Intended to
%% be called by the httpc handler process.
%%--------------------------------------------------------------------

request_canceled(RequestId, ProfileName) ->
    cast(ProfileName, {request_canceled, RequestId}).


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
%%	Session - #tcp_session{}
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
%% Function: delete_session(SessionId, ProfileName) -> _
%%	SessionId -  {{Host, Port}, HandlerPid}
%%      ProfileName - atom()
%% 
%% Description: Deletes session information from the httpc manager
%% table httpc_manager_session_db_<Profile>. Intended to be called by
%% the httpc request handler process.
%%--------------------------------------------------------------------

delete_session(SessionId, ProfileName) ->
    SessionDbName = session_db_name(ProfileName), 
    ?hcrt("delete session", [{session_is, SessionId}, {profile, ProfileName}]),
    ets:delete(SessionDbName, SessionId).


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
%% Function: which_cookies(Url, ProfileName) -> [cookie()]
%%
%%	Url = string()
%%      ProfileName = atom()
%%
%% Description: Retrieves the cookies that would be sent when 
%% requesting <Url>.
%%--------------------------------------------------------------------

which_cookies(ProfileName) ->
    call(ProfileName, which_cookies).
which_cookies(Url, ProfileName) ->
    call(ProfileName, {which_cookies, Url}).


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
    ets:new(HandlerDbName, 
	    [protected, set, named_table, {keypos, #handler_info.id}]),

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
handle_call({request, Request}, _From, State) ->
    ?hcrv("request", [{request, Request}]),
    case (catch handle_request(Request, State)) of
	{ok, ReqId, NewState} ->
	    {reply, {ok, ReqId}, NewState};
	
	Error ->
	    NewError = {error, {failed_process_request, Error}}, 
	    {reply, NewError, State}
    end;
	
handle_call({cancel_request, RequestId}, From, 
	    #state{handler_db = HandlerDb} = State) ->
    ?hcrv("cancel_request", [{request_id, RequestId}]),
    case ets:lookup(State#state.handler_db, RequestId) of
	[] ->
	    ?hcrd("nothing to cancel", []),
	    Reply = ok, %% Nothing to cancel
	    {reply, Reply, State};

	[#handler_info{handler = Pid}] when is_pid(Pid) ->
	    ?hcrd("found operational handler for this request", 
		  [{handler, Pid}]),
	    httpc_handler:cancel(RequestId, Pid),
	    {noreply, State#state{cancel = 
				  [{RequestId, Pid, From} | 
				   State#state.cancel]}};

	[#handler_info{starter = Pid, state = HandlerState}] 
	when is_pid(Pid) ->
	    ?hcri("found *initiating* handler for this request", 
		  [{starter, Pid}, {state, HandlerState}]),
	    ets:update_element(HandlerDb, RequestId, 
			       {#handler_info.state, canceled}),
	    {noreply, State#state{cancel = 
				  [{RequestId, Pid, From} | 
				   State#state.cancel]}}

    end;

handle_call(reset_cookies, _, #state{cookie_db = CookieDb} = State) ->
    ?hcrv("reset cookies", []),
    httpc_cookie:reset_db(CookieDb),
    {reply, ok, State};

handle_call(which_cookies, _, #state{cookie_db = CookieDb} = State) ->
    ?hcrv("which cookies", []),
    CookieHeaders = httpc_cookie:which_cookies(CookieDb),
    {reply, CookieHeaders, State};

handle_call({which_cookies, Url}, _, #state{cookie_db = CookieDb} = State) ->
    ?hcrv("which cookies", [{url, Url}]),
    case http_uri:parse(Url) of
	{Scheme, _, Host, Port, Path, _} ->
	    CookieHeaders = 
		httpc_cookie:header(CookieDb, Scheme, {Host, Port}, Path),
	    {reply, CookieHeaders, State};
	Msg ->
	    {reply, Msg, State}
    end;

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
    ?hcrv("retry or redirect request", [{time, Time}, {request, Request}]),
    case timer:apply_after(Time, ?MODULE, retry_request, 
			   [Request, ProfileName]) of
	{ok, _} ->
	    {noreply, State};
	{error, Reason} ->
	    error_report(ProfileName, 
			 "failed scheduling retry/redirect request"
			 "~n   Time:    ~p"
			 "~n   Request: ~p"
			 "~n   Reason:  ~p", [Time, Request, Reason]),
	    {noreply, State}
    end;

handle_cast({retry_or_redirect_request, Request}, 
	    #state{profile_name = Profile, 
		   handler_db   = HandlerDb} = State) ->
    ?hcrv("retry or redirect request", [{request, Request}]),
    case (catch handle_request(Request, State)) of
	{ok, _, NewState} ->
	    {noreply, NewState};

	Error  ->
	    ReqId = Request#request.id, 
	    error_report(Profile, 
			 "failed to retry or redirect request ~p"
			 "~n   Error: ~p", [ReqId, Error]),
	    case ets:lookup(HandlerDb, ReqId) of
		[#handler_info{from = From}] ->	
		    Error2 = httpc_response:error(Request, Error), 
		    httpc_response:send(From, Error2),
		    ok;
		
		_ ->
		    ok
	    end,
	    {noreply, State}
    end;

handle_cast({request_canceled, RequestId}, State) ->
    ?hcrv("request canceled", [{request_id, RequestId}]),
    ets:delete(State#state.handler_db, RequestId),
    case lists:keysearch(RequestId, 1, State#state.cancel) of
	{value, Entry = {RequestId, _, From}} ->
	    ?hcrt("found in cancel", [{from, From}]),
	    gen_server:reply(From, ok),
	    {noreply, 
	     State#state{cancel = lists:delete(Entry, State#state.cancel)}};
	Else ->
	    ?hcrt("not found in cancel", [{else, Else}]),
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

handle_info({started, StarterPid, ReqId, HandlerPid}, State) ->
    handle_started(StarterPid, ReqId, HandlerPid, State),
    {noreply, State};

handle_info({connect_and_send, StarterPid, ReqId, HandlerPid, Res}, State) ->
    handle_connect_and_send(StarterPid, ReqId, HandlerPid, Res, State),
    {noreply, State};

handle_info({failed_starting_handler, StarterPid, ReqId, Res}, State) ->
    handle_failed_starting_handler(StarterPid, ReqId, Res, State),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #state{handler_db = HandlerDb} = State) ->
    maybe_handle_terminating_starter(Pid, Reason, HandlerDb), 
    {noreply, State};

handle_info({'DOWN', _, _, Pid, _}, State) ->
    
    %% 
    %% Check what happens to waiting requests! Chall we not send a reply?
    %% 

    Pattern = #handler_info{handler = Pid, _ = '_'}, 
    ets:match_delete(State#state.handler_db, Pattern),

    %% If there where any canceled request, handled by the
    %% the process that now has terminated, the
    %% cancelation can be viewed as sucessfull!
    NewCanceledList = 
	lists:foldl(fun({_, HandlerPid, From} = Entry, Acc)  ->
			    case HandlerPid of
				Pid ->
				    gen_server:reply(From, ok),
				    lists:delete(Entry, Acc);
				_ ->
				    Acc
			    end 
		    end, State#state.cancel, State#state.cancel),
    {noreply, State#state{cancel = NewCanceledList}};    

handle_info(Info, #state{profile_name = ProfileName} = State) ->
    error_report(ProfileName, 
		 "received unknown info"
		 "~n   Info: ~p", [Info]),
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

get_manager_info(#state{handler_db = HDB, 
			cookie_db  = CDB} = _State) ->
    HandlerInfo = get_handler_info(HDB),
    CookieInfo  = httpc_cookie:which_cookies(CDB),
    [{handlers, HandlerInfo}, {cookies, CookieInfo}].

get_handler_info(Tab) ->
    Pattern = #handler_info{handler = '$1',
			    state   = '$2', 
			    _ = '_'},
    Handlers1 = [{Pid, State} || [Pid, State] <- ets:match(Tab, Pattern)],
    F = fun({Pid, State} = Elem, Acc) when State =/= canceled -> 
		case lists:keymember(Pid, 1, Acc) of
		    true ->
			Acc;
		    false ->
			[Elem | Acc]
		end;
	   (_, Acc) ->
		Acc
	end,
    Handlers2 = lists:foldl(F, [], Handlers1),
    Handlers3 = [{Pid, State, httpc_handler:info(Pid)} || 
		    {Pid, State} <- Handlers2],
    Handlers3.


%% 
%% The request handler process is started asynchronously by a 
%% "starter process". When the handler has sucessfully been started,
%% this message (started) is sent.
%% 

handle_started(StarterPid, ReqId, HandlerPid, 
			#state{profile_name = Profile, 
			       handler_db   = HandlerDb}) ->
    case ets:lookup(HandlerDb, ReqId) of
	[#handler_info{state = initiating} = HandlerInfo] ->
	    ?hcri("received started ack for initiating handler", []),
	    HandlerInfo2 = HandlerInfo#handler_info{handler = HandlerPid,
						    state   = started}, 
	    ets:insert(HandlerDb, HandlerInfo2),
	    ok;

	[#handler_info{state = State}] ->
	    error_report(Profile, 
			 "unexpected (started) message for handler (~p) in state "
			 "~p regarding request ~p - ignoring", [HandlerPid, State, ReqId]),
	    ?hcri("received unexpected started message", [{state, State}]),
	    ok;

	[] ->
	    error_report(Profile, 
			 "unknown handler ~p (~p) started for request ~w - canceling",
			 [HandlerPid, StarterPid, ReqId]),
	    httpc_handler:cancel(ReqId, HandlerPid)
    end.


%% 
%% The request handler process is started asynchronously by a 
%% "starter process". When that process terminates it sends 
%% one of two messages. These ara handled by the two functions
%% below.
%% 

handle_connect_and_send(_StarterPid, ReqId, HandlerPid, Result, 
			#state{profile_name = Profile, 
			       handler_db   = HandlerDb}) ->
    case ets:lookup(HandlerDb, ReqId) of
	[#handler_info{state = started} = HandlerInfo] when Result =:= ok ->
	    ?hcri("received connect-and-send ack for started handler", []),
	    HandlerInfo2 = HandlerInfo#handler_info{starter = undefined, 
						    handler = HandlerPid,
						    state   = operational}, 
	    ets:insert(HandlerDb, HandlerInfo2),
	    ok;

	[#handler_info{state = canceled} = HandlerInfo] when Result =:= ok ->
	    ?hcri("received connect-and-send ack for canceled handler", []),
	    httpc_handler:cancel(ReqId, HandlerPid),
	    HandlerInfo2 = HandlerInfo#handler_info{starter = undefined, 
						    handler = HandlerPid}, 
	    ets:insert(HandlerDb, HandlerInfo2),
	    ok;

	[#handler_info{state = State}] when Result =/= ok ->
	    error_report(Profile, 
			 "handler (~p, ~w) failed to connect and/or "
			 "send request ~p"
			 "~n   Result: ~p", 
			 [HandlerPid, State, ReqId, Result]),
	    ?hcri("received connect-and-send error", 
		  [{result, Result}, {state, State}]),
	    %% We don't need to send a response to the original caller
	    %% because the handler already sent one in its terminate
	    %% function.
	    ets:delete(HandlerDb, ReqId), 
	    ok;

	[] ->
	    error_report(Profile, 
			 "handler (~p) successfully started "
			 "for unknown request ~p => canceling",
			 [HandlerPid, ReqId]),
	    httpc_handler:cancel(ReqId, HandlerPid)
    end.


handle_failed_starting_handler(_StarterPid, ReqId, Error, 
			       #state{profile_name = Profile, 
				      handler_db = HandlerDb}) ->
    case ets:lookup(HandlerDb, ReqId) of
	[#handler_info{state = canceled}] ->
	    error_report(Profile, 
			 "failed starting handler for request ~p"
			 "~n   Error: ~p", [ReqId, Error]),
	    request_canceled(Profile, ReqId), % Fake signal from handler
	    ets:delete(HandlerDb, ReqId), 
	    ok;

	[#handler_info{from = From}] ->
	    error_report(Profile, 
			 "failed starting handler for request ~p"
			 "~n   Error: ~p", [ReqId, Error]),
	    Reason2 = 
		case Error of
		    {error, Reason} ->
			{failed_connecting, Reason};
		    _ ->
			{failed_connecting, Error}
		end,
	    DummyReq = #request{id = ReqId}, 
	    httpc_response:send(From, httpc_response:error(DummyReq, Reason2)),
	    ets:delete(HandlerDb, ReqId), 
	    ok;

	[] ->
	    error_report(Profile, 
			 "failed starting handler for unknown request ~p"
			 "~n   Error: ~p", [ReqId, Error]),
	    ok
    end.


maybe_handle_terminating_starter(MeybeStarterPid, Reason, HandlerDb) ->
    Pattern = #handler_info{starter = MeybeStarterPid, _ = '_'}, 
    case ets:match_object(HandlerDb, Pattern) of
	[#handler_info{id = ReqId, from = From, state = initiating}] ->
	    %% The starter process crashed before it could start the 
	    %% the handler process, therefor we need to answer the 
	    %% original caller.
	    ?hcri("starter process crashed bfore starting handler", 
		  [{starter, MeybeStarterPid}, {reason, Reason}]),
	    Reason2 = 
		case Reason of
		    {error, Error} ->
			{failed_connecting, Error};
		    _ ->
			{failed_connecting, Reason}
		end,
	    DummyReq = #request{id = ReqId}, 
	    httpc_response:send(From, httpc_response:error(DummyReq, Reason2)),
	    ets:delete(HandlerDb, ReqId),
	    ok;

	[#handler_info{state = State} = HandlerInfo] ->
	    %% The starter process crashed after the handler was started. 
	    %% The handler will answer to the original caller.
	    ?hcri("starter process crashed after starting handler", 
		  [{starter, MeybeStarterPid}, {reason, Reason}, {state, State}]),
	    HandlerInfo2 = HandlerInfo#handler_info{starter = undefined}, 
	    ets:insert(HandlerDb, HandlerInfo2),
	    ok;

	_ ->
	    ok
    end.


%% -----
%% Act as an HTTP/0.9 client that does not know anything
%% about persistent connections
handle_request(#request{settings = 
			#http_options{version = "HTTP/0.9"}} = Request0, 
	       State) ->
    Request1 = handle_cookies(generate_request_id(Request0), State),
    Hdrs0    = Request1#request.headers, 
    Hdrs1    = Hdrs0#http_request_h{connection = undefined},
    Request2 = Request1#request{headers = Hdrs1}, 
    create_handler_starter(Request2, State),
    {ok, Request2#request.id, State};

%% -----
%% Act as an HTTP/1.0 client that does not 
%% use persistent connections
handle_request(#request{settings = 
			#http_options{version = "HTTP/1.0"}} = Request0, 
	       State) ->
    Request1 = handle_cookies(generate_request_id(Request0), State),
    Hdrs0    = Request1#request.headers, 
    Hdrs1    = Hdrs0#http_request_h{connection = "close"},
    Request2 = Request1#request{headers = Hdrs1}, 
    create_handler_starter(Request2, State),
    {ok, Request2#request.id, State};


%% -----
handle_request(#request{method  = Method,
			address = Address,
			scheme  = Scheme} = Request0, 
	       #state{options = Opts} = State) ->
    Request1    = handle_cookies(generate_request_id(Request0), State),
    SessionType = session_type(Opts),
    case select_session(Method, Address, Scheme, SessionType, State) of
	{ok, HandlerPid} ->
	    pipeline_or_keep_alive(Request1, HandlerPid, State);
	no_connection ->
	    create_handler_starter(Request1, State);
	{no_session, OpenSessions} 
	when OpenSessions < Opts#options.max_sessions ->
	    create_handler_starter(Request1, State);
	{no_session, _} ->
	    %% Do not start any more persistent connections
	    %% towards this server.
	    Hdrs0    = Request1#request.headers, 
	    Hdrs1    = Hdrs0#http_request_h{connection = "close"},
	    Request2 = Request1#request{headers = Hdrs1}, 
	    create_handler_starter(Request2, State)
    end,
    {ok, Request1#request.id, State}.


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
	    %% tcp_session with record name field (tcp_session) and 
	    %% socket fields ignored. The fields id (part of: HostPort), 
	    %% client_close, scheme and type specified. 
	    %% The fields id (part of: HandlerPid) and queue_length
	    %% specified.
	    Pattern = #session{id           = {HostPort, '$1'},
			       client_close = false,
			       scheme       = Scheme,
			       queue_length = '$2',
			       type         = SessionType,
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
	    
pipeline_or_keep_alive(#request{id = Id} = Request, HandlerPid, State) ->
    ?hcrd("pipeline of keep-alive", [{id, Id}, {handler, HandlerPid}]),
    case (catch httpc_handler:send(Request, HandlerPid)) of
	ok ->
	    ?hcrd("pipeline or keep-alive - successfully sent", []),
	    Entry = #handler_info{id      = Id,
				  handler = HandlerPid,
				  state   = operational},
	    ets:insert(State#state.handler_db, Entry); 
		
	_  -> %% timeout pipelining failed 
	    ?hcrd("pipeline or keep-alive - failed sending -> "
		  "start a new handler", []),
	    create_handler_starter(Request, State)
    end.


create_handler_starter(#request{socket_opts = SocketOpts} = Request, 
		       #state{options = Options} = State) 
  when is_list(SocketOpts) ->
    %% The user provided us with (override) socket options
    ?hcrt("create handler starter", [{socket_opts, SocketOpts}, {options, Options}]),
    Options2 = Options#options{socket_opts = SocketOpts}, 
    create_handler_starter(Request#request{socket_opts = undefined}, 
			   State#state{options = Options2});

create_handler_starter(#request{id          = Id, 
				from        = From} = Request, 
		       #state{profile_name = ProfileName,
			      options      = Options,
			      handler_db   = HandlerDb} = _State) ->
    ?hcrv("create handler starter", [{id, Id}, {profile, ProfileName}]),
    IsInetsManager = is_inets_manager(), 
    ManagerPid = self(), 
    StarterFun = 
	fun() ->
		?hcrd("handler starter - start", 
		      [{id,            Id}, 
		       {profile,       ProfileName}, 
		       {inets_manager, IsInetsManager}]),
		Result1 = 
		    case IsInetsManager of
			true ->
			    httpc_handler_sup:start_child(Options, 
							  ProfileName);
			false ->
			    httpc_handler:start_link(Options, 
						     ProfileName)
		    end,
		?hcrd("handler starter - maybe connect and send", 
		      [{id, Id}, {profile, ProfileName}, {result, Result1}]),
		case Result1 of
		    {ok, HandlerPid} ->
			StartedMessage = 
			    {started, self(), Id, HandlerPid}, 
			ManagerPid ! StartedMessage, 
			Result2 = httpc_handler:connect_and_send(Request, 
								 HandlerPid),
			?hcrd("handler starter - connected and sent", 
			      [{id, Id}, {profile, ProfileName}, 
			       {handler, HandlerPid}, {result, Result2}]),
			ConnAndSendMessage = 
			    {connect_and_send, 
			     self(), Id, HandlerPid, Result2}, 
			ManagerPid ! ConnAndSendMessage;
		     {error, Reason} ->
			StartFailureMessage = 
			    {failed_starting_handler, self(), Id, Reason}, 
			ManagerPid ! StartFailureMessage;
		     _ ->
			StartFailureMessage = 
			    {failed_starting_handler, self(), Id, Result1}, 
			ManagerPid ! StartFailureMessage
		end
	end,
    Starter = erlang:spawn_link(StarterFun),
    ?hcrd("create handler starter - started", [{id, Id}, {starter, Starter}]),
    Entry = #handler_info{id      = Id,
			  starter = Starter, 
			  from    = From, 
			  state   = initiating},
    ets:insert(HandlerDb, Entry),
    ok.
		
	
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
    


call(ProfileName, Msg) ->
    Timeout = infinity, 
    call(ProfileName, Msg, Timeout).
call(ProfileName, Msg, Timeout) ->
    gen_server:call(ProfileName, Msg, Timeout).

cast(ProfileName, Msg) ->
   gen_server:cast(ProfileName, Msg).



get_proxy(Opts, #options{proxy = Default}) ->
    proplists:get_value(proxy, Opts, Default).

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


%% d(F) ->
%%    d(F, []).

%% d(F, A) -> 
%%     d(get(dbg), F, A).

%% d(true, F, A) ->
%%     io:format(user, "~w:~w:" ++ F ++ "~n", [self(), ?MODULE | A]);
%% d(_, _, _) ->
%%     ok.

