%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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

-include("httpc_internal.hrl").
-include("http_internal.hrl").

%% Internal Application API
-export([start_link/1, start_link/2, request/2, cancel_request/2,
	 request_canceled/2, retry_request/2, redirect_request/2,
	 insert_session/2, delete_session/2, set_options/2, store_cookies/3,
	 cookies/2, session_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {
	  cancel = [],	 % [{RequestId, HandlerPid, ClientPid}]  
	  handler_db,    % ets() - Entry: {Requestid, HandlerPid, ClientPid}
	  cookie_db,     % {ets(), dets()} - {session_cookie_db, cookie_db}
	  session_db,    % ets() - Entry:  #tcp_session{}
	  profile_name,  % atom()
	  options = #options{}
	 }).


%%====================================================================
%% Internal Application API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link({ProfileName, CookieDir}) -> {ok, Pid}
%%
%% ProfileName - httpc_manager_<Profile>
%% CookieDir - directory()
%%
%% Description: Starts the http request manger process. (Started by
%% the intes supervisor.)
%%--------------------------------------------------------------------
start_link({default, CookieDir}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 
			  [?MODULE, {http_default_cookie_db, CookieDir}],
			  []);
start_link({Profile, CookieDir}) ->
    ProfileName = list_to_atom("httpc_manager_" ++ atom_to_list(Profile)), 
    gen_server:start_link({local, ProfileName}, ?MODULE, 
			  [ProfileName, 
			   {http_default_cookie_db, CookieDir}], []).
start_link({Profile, CookieDir}, stand_alone) ->
    ProfileName = list_to_atom("stand_alone_" ++ atom_to_list(Profile)), 
    gen_server:start_link(?MODULE, [ProfileName, 
				    {http_default_cookie_db, CookieDir}], 
			  []).
%%--------------------------------------------------------------------
%% Function: request(Request, ProfileName) ->
%%                                      {ok, Requestid} | {error, Reason}
%%	Request = #request{}
%%      ProfileName = atom()
%%
%% Description: Sends a request to the httpc manager process.
%%--------------------------------------------------------------------
request(Request, ProfileName) ->
    call(ProfileName, {request, Request}, infinity).

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
    call(ProfileName, {cancel_request, RequestId}, infinity).

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
%% Function: insert_session(Session, ProfileName) -> _
%%	Session - #tcp_session{}
%%      ProfileName - atom()
%%
%% Description: Inserts session information into the httpc manager
%% table <ProfileName>_session_db. Intended to be called by
%% the httpc request handler process.
%%--------------------------------------------------------------------
insert_session(Session, ProfileName) ->
    Db = list_to_atom(atom_to_list(ProfileName) ++ "_session_db"),
    ets:insert(Db, Session).

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
    Db = list_to_atom(atom_to_list(ProfileName) ++ "_session_db"),
    ets:delete(Db, SessionId).

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
%% Function: cookies(Url, ProfileName) -> ok
%%
%%	Url = string()
%%      ProfileName = atom()
%%
%% Description: Retrieves the cookies that would be sent when 
%% requesting <Url>.
%%--------------------------------------------------------------------
cookies(Url, ProfileName) ->
    call(ProfileName, {cookies, Url}, infinity).

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
init([ProfileName, CookiesConf | _]) ->
    process_flag(trap_exit, true),
    SessionDb = list_to_atom(atom_to_list(ProfileName) ++ "_session_db"),
    ets:new(SessionDb, 
	    [public, set, named_table, {keypos, #tcp_session.id}]),
    ?hcri("starting", [{profile, ProfileName}]),
    {ok, #state{handler_db = ets:new(handler_db, [protected, set]),
		cookie_db = 
		http_cookie:open_cookie_db({CookiesConf, 
					    http_session_cookie_db}),
		session_db = SessionDb,
		profile_name = ProfileName
	       }}.

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
	    ok, %% Nothing to cancel
	      {reply, ok, State};
	[{_, Pid, _}] ->
	    httpc_handler:cancel(RequestId, Pid),
	    {noreply, State#state{cancel = 
				  [{RequestId, Pid, From} |
				   State#state.cancel]}}
    end;

handle_call({cookies, Url}, _, State) ->
    case http_uri:parse(Url) of
	{Scheme, _, Host, Port, Path, _} ->
	    CookieHeaders = 
		http_cookie:header(Scheme, {Host, Port}, 
				   Path, State#state.cookie_db),
	    {reply, CookieHeaders, State};
	Msg ->
	    {reply, Msg, State}
    end;

handle_call(Msg, From, State) ->
    Report = io_lib:format("HTTPC_MANAGER recived unkown call: ~p"
			   "from: ~p~n", [Msg, From]),
    error_logger:error_report(Report),
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
    ets:delete(State#state.handler_db, RequestId),
    case lists:keysearch(RequestId, 1, State#state.cancel) of
	{value, Entry = {RequestId, _, From}} ->
	    gen_server:reply(From, ok),
	    {noreply, 
	     State#state{cancel = lists:delete(Entry, State#state.cancel)}};
	_ ->
	   {noreply, State}
    end;
handle_cast({set_options, Options}, State = #state{options = OldOptions}) ->
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
		 verbose               = get_verbose(Options, OldOptions)
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

handle_cast(Msg, State) ->
    Report = io_lib:format("HTTPC_MANAGER recived unkown cast: ~p", 
			   [Msg]),
    error_logger:error_report(Report),
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
    http_cookie:close_cookie_db(State#state.cookie_db),
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

select_session(Method, HostPort, Scheme, SessionTyp, 
	       #state{options = #options{max_pipeline_length =
					 MaxPipe,
					 max_keep_alive_length = MaxKeepAlive},
		      session_db = SessionDb}) ->
    case httpc_request:is_idempotent(Method) or (SessionTyp == keep_alive) of
	true ->
	    Candidates = ets:match(SessionDb,
				   {'_', {HostPort, '$1'}, 
				    false, Scheme, '_', '$2', SessionTyp}),
	    select_session(Candidates, MaxKeepAlive, MaxPipe, SessionTyp);
	false ->
	    no_connection
    end.
    
select_session(Candidates, Max, _, keep_alive) ->
    select_session(Candidates, Max);
select_session(Candidates, _, Max, pipeline) ->
    select_session(Candidates, Max).

select_session(Candidates, Max) ->
    case Candidates of 
	[] ->
	  no_connection; 
	_ ->
	    NewCandidates = 
		lists:foldl(
		  fun([Pid, Length], Acc) when Length =< Max ->
			  [{Pid, Length} | Acc];
		     (_, Acc) ->
			  Acc
		  end, [], Candidates),
	    
	    case lists:keysort(2, NewCandidates) of
		[] ->
		    {no_session, length(Candidates)};
		[{HandlerPid, _} | _] ->
		    {ok, HandlerPid}
	    end
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

start_handler(Request, State) ->
    {ok, Pid} =
	case is_inets_manager() of
	    true ->
		httpc_handler_sup:start_child([Request, State#state.options,
					       State#state.profile_name]);
	    false ->
		httpc_handler:start_link(Request, State#state.options,
					 State#state.profile_name)
	end,
    ets:insert(State#state.handler_db, {Request#request.id, 
					Pid, Request#request.from}),
    erlang:monitor(process, Pid).

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
	    %% This is an automatic redirect or a retryed pipelined
	    %% request keep the old id.
	    Request
    end.

handle_cookies(Request, #state{options = #options{cookies = disabled}}) ->
    Request;
handle_cookies(Request = #request{scheme = Scheme, address = Address,
				  path = Path, headers = 
		 Headers = #http_request_h{other = Other}}, 
	       #state{cookie_db = Db}) ->
    case http_cookie:header(Scheme, Address, Path, Db) of
	{"cookie", ""} ->
	    Request;
	CookieHeader ->
	    NewHeaders = 
		Headers#http_request_h{other = [CookieHeader | Other]},
	    Request#request{headers = NewHeaders}
    end.

do_store_cookies([], _) ->
    ok;
do_store_cookies([Cookie | Cookies], State) ->
    ok = http_cookie:insert(Cookie, State#state.cookie_db),
    do_store_cookies(Cookies, State).

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


handle_verbose(debug) ->
    dbg:p(self(), [call]),
    dbg:tp(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(trace) ->
    dbg:p(self(), [call]),
    dbg:tpl(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(_) ->
    ok.  

%% d(F) ->
%%    d(F, []).

%% d(F, A) -> 
%%     d(get(dbg), F, A).

%% d(true, F, A) ->
%%     io:format(user, "~w:~w:" ++ F ++ "~n", [self(), ?MODULE | A]);
%% d(_, _, _) ->
%%     ok.

