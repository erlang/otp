%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2011. All Rights Reserved.
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
	 request_canceled/3,
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
	  session_db,    % ets() - Entry:  #session{}
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
    call(ProfileName, {cancel_request, RequestId}).


%%--------------------------------------------------------------------
%% Function: request_canceled(RequestId, ProfileName) -> ok
%%	RequestId - ref()
%%      ProfileName = atom()
%%
%% Description: Confirms that a request has been canceld. Intended to
%% be called by the httpc handler process.
%%--------------------------------------------------------------------

request_canceled(RequestId, ProfileName, From) ->
    gen_server:reply(From, ok),
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
handle_call({request, Request}, _, State) ->
    ?hcri("request", [{request, Request}]),
    case (catch handle_request(Request, State)) of
	{reply, Msg, NewState} ->
	    {reply, Msg, NewState};
	Error ->
	    {stop, Error, httpc_response:error(Request, Error), State}
    end;

handle_call({cancel_request, RequestId}, From, State) ->
    ?hcri("cancel_request", [{request_id, RequestId}]),
    case ets:lookup(State#state.handler_db, RequestId) of
	[] ->
	    %% The request has allready compleated make sure
	    %% it is deliverd to the client process queue so
	    %% it can be thrown away by httpc:cancel_request
	    %% This delay is hopfully a temporary workaround.
	    %% Note that it will not not delay the manager,
	    %% only the client that called httpc:cancel_request
	    timer:apply_after(?DELAY, gen_server, reply, [From, ok]),
	    {noreply, State};
	[{_, Pid, _}] ->
	    httpc_handler:cancel(RequestId, Pid, From),
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

handle_cast({request_canceled, RequestId}, State) ->
    ?hcrv("request canceled", [{request_id, RequestId}]),
    ets:delete(State#state.handler_db, RequestId),
    case lists:keysearch(RequestId, 1, State#state.cancel) of
	{value, Entry = {RequestId, _, From}} ->
	    ?hcrt("found in cancel", [{from, From}]),
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
handle_info({'EXIT', _, _}, State) ->
    %% Handled in DOWN
    {noreply, State};
handle_info({'DOWN', _, _, Pid, _}, State) ->
    ets:match_delete(State#state.handler_db, {'_', Pid, '_'}),

    %% If there where any canceled request, handled by the
    %% the process that now has terminated, the
    %% cancelation can be viewed as sucessfull!
    NewCanceldList =
	lists:foldl(fun(Entry = {_, HandlerPid, From}, Acc)  ->
			    case HandlerPid of
				Pid ->
				    gen_server:reply(From, ok),
				    lists:delete(Entry, Acc);
				_ ->
				    Acc
			    end 
		    end, State#state.cancel, State#state.cancel),
    {noreply, State#state{cancel = NewCanceldList}};
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
    Handlers3 = [{Pid, State, 
		  case (catch httpc_handler:info(Pid)) of
		      {'EXIT', _} -> 
			  %% Why would this crash? 
			  %% Only if the process has died, but we don't 
			  %% know about it?
			  [];
		      Else ->
			  Else
		  end} || 
		    {Pid, State} <- Handlers2],
    Handlers3.

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


start_handler(Request, State) ->
    {ok, Pid} =
	case is_inets_manager() of
	    true ->
		httpc_handler_sup:start_child([whereis(httpc_handler_sup),
					       Request, State#state.options,
					       State#state.profile_name]);
	    false ->
		httpc_handler:start_link(self(), Request, State#state.options,
					 State#state.profile_name)
	end,
    ets:insert(State#state.handler_db, {Request#request.id,
					Pid, Request#request.from}),
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

pipeline_or_keep_alive(Request, HandlerPid, State) ->
    case (catch httpc_handler:send(Request, HandlerPid)) of
	ok ->
	    ets:insert(State#state.handler_db, {Request#request.id,
						HandlerPid,
						Request#request.from});
	_  -> %timeout pipelining failed
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
