%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Manages ssl sessions and trusted certifacates
%%----------------------------------------------------------------------

-module(ssl_manager).
-behaviour(gen_server).

-include("ssl_internal.hrl").

%% Internal application API
-export([start_link/1, start_link_dist/1,
	 connection_init/2, cache_pem_file/2,
	 lookup_trusted_cert/4, issuer_candidate/2, client_session_id/4,
	 server_session_id/4,
	 register_session/2, register_session/3, invalidate_session/2,
	 invalidate_session/3]).

% Spawn export
-export([init_session_validator/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {
	  session_cache,
	  session_cache_cb,
	  session_lifetime,
	  certificate_db,
	  session_validation_timer,
	  last_delay_timer %% Keep for testing purposes
	 }).

-define('24H_in_msec', 8640000).
-define('24H_in_sec', 8640).
-define(SESSION_VALIDATION_INTERVAL, 60000).
-define(CERTIFICATE_CACHE_CLEANUP, 30000).
-define(CLEAN_SESSION_DB, 60000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
%%
%% Description: Starts the ssl manager that takes care of sessions
%% and certificate caching.
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [?MODULE, Opts], []).

%%--------------------------------------------------------------------
-spec start_link_dist(list()) -> {ok, pid()} | ignore | {error, term()}.
%%
%% Description: Starts a special instance of the ssl manager to
%% be used by the erlang distribution. Note disables soft upgrade!
%%--------------------------------------------------------------------
start_link_dist(Opts) ->
    gen_server:start_link({local, ssl_manager_dist}, ?MODULE, [ssl_manager_dist, Opts], []).

%%--------------------------------------------------------------------
-spec connection_init(string()| {der, list()}, client | server) ->
			     {ok, certdb_ref(), db_handle(), db_handle()}.
%%			     
%% Description: Do necessary initializations for a new connection.
%%--------------------------------------------------------------------
connection_init(Trustedcerts, Role) ->
    call({connection_init, Trustedcerts, Role}).
%%--------------------------------------------------------------------
-spec cache_pem_file(string(), term()) -> {ok, term()} | {error, reason()}.
%%		    
%% Description: Cach a pem file and return its content.
%%--------------------------------------------------------------------
cache_pem_file(File, DbHandle) ->
    try file:read_file_info(File) of
	{ok, #file_info{mtime = LastWrite}} ->
	    cache_pem_file(File, LastWrite, DbHandle)
    catch
	_:Reason ->
	    {error, Reason}
    end.
%%--------------------------------------------------------------------
-spec lookup_trusted_cert(term(), reference(), serialnumber(), issuer()) ->
				 undefined | 
				 {ok, {der_cert(), #'OTPCertificate'{}}}.
%%				 
%% Description: Lookup the trusted cert with Key = {reference(),
%% serialnumber(), issuer()}.
%% --------------------------------------------------------------------
lookup_trusted_cert(DbHandle, Ref, SerialNumber, Issuer) ->
    ssl_certificate_db:lookup_trusted_cert(DbHandle, Ref, SerialNumber, Issuer).
%%--------------------------------------------------------------------
-spec issuer_candidate(cert_key() | no_candidate, term()) ->
			      {cert_key(),
			       {der_cert(),
				#'OTPCertificate'{}}} | no_more_candidates.
%%
%% Description: Return next issuer candidate.
%%--------------------------------------------------------------------
issuer_candidate(PrevCandidateKey, DbHandle) ->
    ssl_certificate_db:issuer_candidate(PrevCandidateKey, DbHandle).
%%--------------------------------------------------------------------
-spec client_session_id(host(), inet:port_number(), #ssl_options{},
			der_cert() | undefined) -> session_id().
%%
%% Description: Select a session id for the client.
%%--------------------------------------------------------------------
client_session_id(Host, Port, SslOpts, OwnCert) ->
    call({client_session_id, Host, Port, SslOpts, OwnCert}).

%%--------------------------------------------------------------------
-spec server_session_id(host(), inet:port_number(), #ssl_options{},
			der_cert()) -> session_id().
%%
%% Description: Select a session id for the server.
%%--------------------------------------------------------------------
server_session_id(Port, SuggestedSessionId, SslOpts, OwnCert) ->
    call({server_session_id, Port, SuggestedSessionId, SslOpts, OwnCert}).

%%--------------------------------------------------------------------
-spec register_session(inet:port_number(), #session{}) -> ok.
-spec register_session(host(), inet:port_number(), #session{}) -> ok.
%%
%% Description: Make the session available for reuse.
%%--------------------------------------------------------------------
register_session(Host, Port, Session) ->
    cast({register_session, Host, Port, Session}).

register_session(Port, Session) ->
    cast({register_session, Port, Session}).
%%--------------------------------------------------------------------
-spec invalidate_session(inet:port_number(), #session{}) -> ok.
-spec invalidate_session(host(), inet:port_number(), #session{}) -> ok.
%%
%% Description: Make the session unavailable for reuse. After
%% a the session has been marked "is_resumable = false" for some while
%% it will be safe to remove the data from the session database.
%%--------------------------------------------------------------------
invalidate_session(Host, Port, Session) ->
    cast({invalidate_session, Host, Port, Session}).

invalidate_session(Port, Session) ->
    cast({invalidate_session, Port, Session}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
-spec init(list()) -> {ok, #state{}}.
%% Possible return values not used now. 
%% |  {ok, #state{}, timeout()} | ignore | {stop, term()}.		  
%%
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Name, Opts]) ->
    put(ssl_manager, Name),
    process_flag(trap_exit, true),
    CacheCb = proplists:get_value(session_cb, Opts, ssl_session_cache),
    SessionLifeTime =  
	proplists:get_value(session_lifetime, Opts, ?'24H_in_sec'),
    CertDb = ssl_certificate_db:create(),
    SessionCache = CacheCb:init(proplists:get_value(session_cb_init_args, Opts, [])),
    Timer = erlang:send_after(SessionLifeTime * 1000, 
			      self(), validate_sessions),
    {ok, #state{certificate_db = CertDb,
		session_cache = SessionCache,
		session_cache_cb = CacheCb,
		session_lifetime = SessionLifeTime,
		session_validation_timer = Timer}}.

%%--------------------------------------------------------------------
-spec handle_call(msg(), from(), #state{}) -> {reply, reply(), #state{}}. 
%% Possible return values not used now.  
%%					      {reply, reply(), #state{}, timeout()} |
%%					      {noreply, #state{}} |
%%					      {noreply, #state{}, timeout()} |
%%					      {stop, reason(), reply(), #state{}} |
%%					      {stop, reason(), #state{}}.
%%
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({{connection_init, "", _Role}, Pid}, _From, 
	    #state{certificate_db = [CertDb |_],
		   session_cache = Cache} = State) ->
    erlang:monitor(process, Pid),
    Result = {ok, make_ref(),CertDb, Cache},
    {reply, Result, State};

handle_call({{connection_init, Trustedcerts, _Role}, Pid}, _From,
	    #state{certificate_db = [CertDb|_] =Db,
		   session_cache = Cache} = State) ->
    erlang:monitor(process, Pid),
    Result = 
	try
	    {ok, Ref} = ssl_certificate_db:add_trusted_certs(Pid, Trustedcerts, Db),
	    {ok, Ref, CertDb, Cache}
	catch
	    _:Reason ->
		{error, Reason}
	end,
    {reply, Result, State};

handle_call({{client_session_id, Host, Port, SslOpts, OwnCert}, _}, _,
	    #state{session_cache = Cache,
		  session_cache_cb = CacheCb} = State) ->
    Id = ssl_session:id({Host, Port, SslOpts}, Cache, CacheCb, OwnCert),
    {reply, Id, State};

handle_call({{server_session_id, Port, SuggestedSessionId, SslOpts, OwnCert}, _},
	    _, #state{session_cache_cb = CacheCb,
		      session_cache = Cache,
		      session_lifetime = LifeTime} = State) ->
    Id = ssl_session:id(Port, SuggestedSessionId, SslOpts,
			Cache, CacheCb, LifeTime, OwnCert),
    {reply, Id, State};

handle_call({{cache_pem, File, LastWrite}, Pid}, _, 
	    #state{certificate_db = Db} = State) ->
    try ssl_certificate_db:cache_pem_file(Pid, File, LastWrite, Db) of
	Result ->
	    {reply, Result, State}
    catch 
	_:Reason ->
	    {reply, {error, Reason}, State}
    end;
handle_call({{recache_pem, File, LastWrite}, Pid}, From,
	    #state{certificate_db = Db} = State) ->
    ssl_certificate_db:uncache_pem_file(File, Db),
    cast({recache_pem, File, LastWrite, Pid, From}),
    {noreply, State}.

%%--------------------------------------------------------------------
-spec  handle_cast(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.  
%%				      | {noreply, #state{}, timeout()} |
%%				       {stop, reason(), #state{}}.
%%
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register_session, Host, Port, Session}, 
	    #state{session_cache = Cache,
		   session_cache_cb = CacheCb} = State) ->
    TimeStamp = calendar:datetime_to_gregorian_seconds({date(), time()}),
    NewSession = Session#session{time_stamp = TimeStamp},
    CacheCb:update(Cache, {{Host, Port}, 
		   NewSession#session.session_id}, NewSession),
    {noreply, State};

handle_cast({register_session, Port, Session},  
	    #state{session_cache = Cache,
		   session_cache_cb = CacheCb} = State) ->    
    TimeStamp = calendar:datetime_to_gregorian_seconds({date(), time()}),
    NewSession = Session#session{time_stamp = TimeStamp},
    CacheCb:update(Cache, {Port, NewSession#session.session_id}, NewSession),
    {noreply, State};

%%% When a session is invalidated we need to wait a while before deleting
%%% it as there might be pending connections that rightfully needs to look
%%% up the session data but new connections should not get to use this session.
handle_cast({invalidate_session, Host, Port,
	     #session{session_id = ID} = Session},
	    #state{session_cache = Cache,
		   session_cache_cb = CacheCb} = State) ->
    CacheCb:update(Cache, {{Host, Port}, ID}, Session#session{is_resumable = false}),
    TRef =
	erlang:send_after(delay_time(), self(), {delayed_clean_session, {{Host, Port}, ID}}),
    {noreply, State#state{last_delay_timer = TRef}};

handle_cast({invalidate_session, Port, #session{session_id = ID} = Session},
	    #state{session_cache = Cache,
		   session_cache_cb = CacheCb} = State) ->
    CacheCb:update(Cache, {Port, ID}, Session#session{is_resumable = false}),
    TRef =
	erlang:send_after(delay_time(), self(), {delayed_clean_session, {Port, ID}}),
    {noreply, State#state{last_delay_timer = TRef}};

handle_cast({recache_pem, File, LastWrite, Pid, From},
	    #state{certificate_db = [_, FileToRefDb, _]} = State0) ->
    case ssl_certificate_db:lookup(File, FileToRefDb) of
	undefined ->
	    {reply, Msg, State} =
		handle_call({{cache_pem, File, LastWrite}, Pid}, From, State0),
	    gen_server:reply(From, Msg),
	    {noreply, State};
	_ -> %% Send message to self letting cleanup messages be handled
	     %% first so that no reference to the old version of file
             %% exists when we cache the new one.
	    cast({recache_pem, File, LastWrite, Pid, From}),
	    {noreply, State0}
    end.

%%--------------------------------------------------------------------
-spec handle_info(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.
%%				      |{noreply, #state{}, timeout()} |
%%				      {stop, reason(), #state{}}.
%%
%% Description: Handling all non call/cast messages
%%-------------------------------------------------------------------- 
handle_info(validate_sessions, #state{session_cache_cb = CacheCb,
				      session_cache = Cache,
				      session_lifetime = LifeTime
				     } = State) ->
    Timer = erlang:send_after(?SESSION_VALIDATION_INTERVAL, 
			      self(), validate_sessions),
    start_session_validator(Cache, CacheCb, LifeTime),
    {noreply, State#state{session_validation_timer = Timer}};

handle_info({delayed_clean_session, Key}, #state{session_cache = Cache,
                   session_cache_cb = CacheCb
                   } = State) ->
    CacheCb:delete(Cache, Key),
    {noreply, State};

handle_info({'EXIT', _, _}, State) ->
    %% Session validator died!! Do we need to take any action?
    %% maybe error log
    {noreply, State};

handle_info({'DOWN', _Ref, _Type, _Pid, ecacertfile}, State) ->
    {noreply, State};

handle_info({'DOWN', _Ref, _Type, Pid, shutdown}, State) ->
    handle_info({remove_trusted_certs, Pid}, State);
handle_info({'DOWN', _Ref, _Type, Pid, _Reason}, State) ->
    erlang:send_after(?CERTIFICATE_CACHE_CLEANUP, self(), 
		      {remove_trusted_certs, Pid}),
    {noreply, State};
handle_info({remove_trusted_certs, Pid}, 
	    #state{certificate_db = Db} = State) ->
    ssl_certificate_db:remove_trusted_certs(Pid, Db),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
-spec terminate(reason(), #state{}) -> term().
%%		       
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{certificate_db = Db,
			  session_cache = SessionCache,
			  session_cache_cb = CacheCb,
			  session_validation_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    ssl_certificate_db:remove(Db),
    CacheCb:terminate(SessionCache),
    ok.

%%--------------------------------------------------------------------
-spec code_change(term(), #state{}, list()) -> {ok, #state{}}.			 
%%
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call(Msg) ->
    gen_server:call(get(ssl_manager), {Msg, self()}, infinity).

cast(Msg) ->
    gen_server:cast(get(ssl_manager), Msg).
 
validate_session(Host, Port, Session, LifeTime) ->
    case ssl_session:valid_session(Session, LifeTime) of
	true ->
	    ok;
	false ->
	    invalidate_session(Host, Port, Session)
    end.

validate_session(Port, Session, LifeTime) ->
    case ssl_session:valid_session(Session, LifeTime) of
	true ->
	    ok;
	false ->
	    invalidate_session(Port, Session)
    end.
		    
start_session_validator(Cache, CacheCb, LifeTime) ->
    spawn_link(?MODULE, init_session_validator, 
	       [[get(ssl_manager), Cache, CacheCb, LifeTime]]).

init_session_validator([SslManagerName, Cache, CacheCb, LifeTime]) ->
    put(ssl_manager, SslManagerName),
    CacheCb:foldl(fun session_validation/2,
		  LifeTime, Cache).

session_validation({{{Host, Port}, _}, Session}, LifeTime) ->
    validate_session(Host, Port, Session, LifeTime),
    LifeTime;
session_validation({{Port, _}, Session}, LifeTime) ->
    validate_session(Port, Session, LifeTime),
    LifeTime.

cache_pem_file(File, LastWrite, DbHandle) ->
    case ssl_certificate_db:lookup_cached_certs(DbHandle,File) of
	[{_, {Mtime, Content}}] ->
	    case LastWrite of
		Mtime ->
		    {ok, Content};
		_ ->
		    call({recache_pem, File, LastWrite})
	    end;
	[] ->
	    call({cache_pem, File, LastWrite})
    end.

delay_time() ->
    case application:get_env(ssl, session_delay_cleanup_time) of
	{ok, Time} when is_integer(Time) ->
	    Time;
	_ ->
	   ?CLEAN_SESSION_DB
    end.
