%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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

%% Internal application API
-export([start_link/1, start_link_dist/1,
	 connection_init/2, cache_pem_file/2,
	 lookup_trusted_cert/4,
	 new_session_id/1, clean_cert_db/2,
	 register_session/2, register_session/3, invalidate_session/2,
	 invalidate_session/3, clear_pem_cache/0, manager_name/1]).

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
	  last_delay_timer  = {undefined, undefined}%% Keep for testing purposes
	 }).

-define('24H_in_msec', 8640000).
-define('24H_in_sec', 8640).
-define(GEN_UNIQUE_ID_MAX_TRIES, 10).
-define(SESSION_VALIDATION_INTERVAL, 60000).
-define(CLEAR_PEM_CACHE, 120000).
-define(CLEAN_SESSION_DB, 60000).
-define(CLEAN_CERT_DB, 500).
-define(NOT_TO_BIG, 10).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
-spec manager_name(normal | dist) -> atom().
%%
%% Description: Returns the registered name of the ssl manager process
%% in the operation modes 'normal' and 'dist'.
%%--------------------------------------------------------------------
manager_name(normal) ->
    ?MODULE;
manager_name(dist) ->
    list_to_atom(atom_to_list(?MODULE) ++ "dist").

%%--------------------------------------------------------------------
-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
%%
%% Description: Starts the ssl manager that takes care of sessions
%% and certificate caching.
%%--------------------------------------------------------------------
start_link(Opts) ->
    DistMangerName = manager_name(normal),
    gen_server:start_link({local, DistMangerName}, ?MODULE, [DistMangerName, Opts], []).

%%--------------------------------------------------------------------
-spec start_link_dist(list()) -> {ok, pid()} | ignore | {error, term()}.
%%
%% Description: Starts a special instance of the ssl manager to
%% be used by the erlang distribution. Note disables soft upgrade!
%%--------------------------------------------------------------------
start_link_dist(Opts) ->
    DistMangerName = manager_name(dist),
    gen_server:start_link({local, DistMangerName}, ?MODULE, [DistMangerName, Opts], []).

%%--------------------------------------------------------------------
-spec connection_init(binary()| {der, list()}, client | server) ->
			     {ok, certdb_ref(), db_handle(), db_handle(), db_handle(), db_handle()}.
%%			     
%% Description: Do necessary initializations for a new connection.
%%--------------------------------------------------------------------
connection_init({der, _} = Trustedcerts, Role) ->
    call({connection_init, Trustedcerts, Role});

connection_init(<<>> = Trustedcerts, Role) ->
    call({connection_init, Trustedcerts, Role});

connection_init(Trustedcerts, Role) ->
    call({connection_init, Trustedcerts, Role}).

%%--------------------------------------------------------------------
-spec cache_pem_file(binary(), term()) -> {ok, term()} | {error, reason()}.
%%		    
%% Description: Cache a pem file and return its content.
%%--------------------------------------------------------------------
cache_pem_file(File, DbHandle) ->
    MD5 = crypto:hash(md5, File),
    case ssl_pkix_db:lookup_cached_pem(DbHandle, MD5) of
	[{Content,_}] ->
	    {ok, Content};
	[Content] ->
	   {ok, Content};
	undefined ->
	    call({cache_pem, {MD5, File}})
    end.

%%--------------------------------------------------------------------
-spec clear_pem_cache() -> ok.
%%
%% Description: Clear the PEM cache
%%--------------------------------------------------------------------
clear_pem_cache() ->
    %% Not supported for distribution at the moement, should it be?
    put(ssl_manager, manager_name(normal)),
    call(unconditionally_clear_pem_cache).

%%--------------------------------------------------------------------
-spec lookup_trusted_cert(term(), reference(), serialnumber(), issuer()) ->
				 undefined | 
				 {ok, {der_cert(), #'OTPCertificate'{}}}.
%%				 
%% Description: Lookup the trusted cert with Key = {reference(),
%% serialnumber(), issuer()}.
%% --------------------------------------------------------------------
lookup_trusted_cert(DbHandle, Ref, SerialNumber, Issuer) ->
    ssl_pkix_db:lookup_trusted_cert(DbHandle, Ref, SerialNumber, Issuer).

%%--------------------------------------------------------------------
-spec new_session_id(integer()) -> session_id().
%%
%% Description: Creates a session id for the server.
%%--------------------------------------------------------------------
new_session_id(Port) ->
    call({new_session_id, Port}).

%%--------------------------------------------------------------------
-spec clean_cert_db(reference(), binary()) -> ok.
%%
%% Description: Send clean request of cert db to ssl_manager process should
%% be called by ssl-connection processes. 
%%--------------------------------------------------------------------
clean_cert_db(Ref, File) ->
    erlang:send_after(?CLEAN_CERT_DB, get(ssl_manager), {clean_cert_db, Ref, File}),
    ok.

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
    CertDb = ssl_pkix_db:create(),
    SessionCache = CacheCb:init(proplists:get_value(session_cb_init_args, Opts, [])),
    Timer = erlang:send_after(SessionLifeTime * 1000 + 5000, 
			      self(), validate_sessions),
    erlang:send_after(?CLEAR_PEM_CACHE, self(), clear_pem_cache),
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
handle_call({{connection_init, <<>>, _Role}, _Pid}, _From,
	    #state{certificate_db = [CertDb, FileRefDb, PemChace],
		   session_cache = Cache} = State) ->
    Result = {ok, make_ref(),CertDb, FileRefDb, PemChace, Cache},
    {reply, Result, State};

handle_call({{connection_init, Trustedcerts, _Role}, Pid}, _From,
	    #state{certificate_db = [CertDb, FileRefDb, PemChace] = Db,
		   session_cache = Cache} = State) ->
    Result = 
	try
	    {ok, Ref} = ssl_pkix_db:add_trusted_certs(Pid, Trustedcerts, Db),
	    {ok, Ref, CertDb, FileRefDb, PemChace, Cache}
	catch
	    _:Reason ->
		{error, Reason}
	end,
    {reply, Result, State};

handle_call({{new_session_id,Port}, _},
	    _, #state{session_cache_cb = CacheCb,
		      session_cache = Cache} = State) ->
    Id = new_id(Port, ?GEN_UNIQUE_ID_MAX_TRIES, Cache, CacheCb),
    {reply, Id, State};


handle_call({{cache_pem, File}, _Pid}, _,
	    #state{certificate_db = Db} = State) ->
    try ssl_pkix_db:cache_pem_file(File, Db) of
	Result ->
	    {reply, Result, State}
    catch 
	_:Reason ->
	    {reply, {error, Reason}, State}
    end;
handle_call({unconditionally_clear_pem_cache, _},_, #state{certificate_db = [_,_,PemChace]} = State) ->
    ssl_pkix_db:clear(PemChace),
    {reply, ok,  State}.

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

handle_cast({invalidate_session, Host, Port,
	     #session{session_id = ID} = Session},
	    #state{session_cache = Cache,
		   session_cache_cb = CacheCb} = State) ->
    invalidate_session(Cache, CacheCb, {{Host, Port}, ID}, Session, State);

handle_cast({invalidate_session, Port, #session{session_id = ID} = Session},
	    #state{session_cache = Cache,
		   session_cache_cb = CacheCb} = State) ->
    invalidate_session(Cache, CacheCb, {Port, ID}, Session, State).

%%--------------------------------------------------------------------
-spec handle_info(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.
%%				      |{noreply, #state{}, timeout()} |
%%				      {stop, reason(), #state{}}.
%%
%% Description: Handling all non call/cast messages
%%-------------------------------------------------------------------
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

handle_info(clear_pem_cache, #state{certificate_db = [_,_,PemChace]} = State) ->
    case ssl_pkix_db:db_size(PemChace) of
	N  when N < ?NOT_TO_BIG ->
	    ok;
	_ ->
	    ssl_pkix_db:clear(PemChace)
    end,
    erlang:send_after(?CLEAR_PEM_CACHE, self(), clear_pem_cache),
    {noreply, State};


handle_info({clean_cert_db, Ref, File},
	    #state{certificate_db = [CertDb,RefDb, PemCache]} = State) ->
    
    case ssl_pkix_db:lookup(Ref, RefDb) of
	undefined -> %% Alredy cleaned
	    ok;
	_ ->
	    clean_cert_db(Ref, CertDb, RefDb, PemCache, File)
    end,
    {noreply, State};

handle_info({'EXIT', _, _}, State) ->
    %% Session validator died!! Do we need to take any action?
    %% maybe error log
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
-spec terminate(reason(), #state{}) -> ok.
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
    ssl_pkix_db:remove(Db),
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

delay_time() ->
    case application:get_env(ssl, session_delay_cleanup_time) of
	{ok, Time} when is_integer(Time) ->
	    Time;
	_ ->
	   ?CLEAN_SESSION_DB
    end.

invalidate_session(Cache, CacheCb, Key, Session, #state{last_delay_timer = LastTimer} = State) ->
    case CacheCb:lookup(Cache, Key) of
	undefined -> %% Session is already invalidated
	    {noreply, State};
	#session{is_resumable = new} ->
	    CacheCb:delete(Cache, Key),
	    {noreply, State};
	_ ->
	    %% When a registered session is invalidated we need to wait a while before deleting
	    %% it as there might be pending connections that rightfully needs to look
	    %% up the session data but new connections should not get to use this session.
	    CacheCb:update(Cache, Key, Session#session{is_resumable = false}),
	    TRef =
		erlang:send_after(delay_time(), self(), {delayed_clean_session, Key}),
	    {noreply, State#state{last_delay_timer = last_delay_timer(Key, TRef, LastTimer)}}
    end.

last_delay_timer({{_,_},_}, TRef, {LastServer, _}) ->
    {LastServer, TRef};
last_delay_timer({_,_}, TRef, {_, LastClient}) ->
    {TRef, LastClient}.

%% If we can not generate a not allready in use session ID in
%% ?GEN_UNIQUE_ID_MAX_TRIES we make the new session uncacheable The
%% value of ?GEN_UNIQUE_ID_MAX_TRIES is stolen from open SSL which
%% states : "If we can not find a session id in
%% ?GEN_UNIQUE_ID_MAX_TRIES either the RAND code is broken or someone
%% is trying to open roughly very close to 2^128 (or 2^256) SSL
%% sessions to our server"
new_id(_, 0, _, _) ->
    <<>>;
new_id(Port, Tries, Cache, CacheCb) ->
    Id = crypto:rand_bytes(?NUM_OF_SESSION_ID_BYTES),
    case CacheCb:lookup(Cache, {Port, Id}) of
	undefined ->
	    Now =  calendar:datetime_to_gregorian_seconds({date(), time()}),
	    %% New sessions can not be set to resumable
	    %% until handshake is compleate and the
	    %% other session values are set.
	    CacheCb:update(Cache, {Port, Id}, #session{session_id = Id,
						       is_resumable = false,
						       time_stamp = Now}),
	    Id;
	_ ->
	    new_id(Port, Tries - 1, Cache, CacheCb)
    end.

clean_cert_db(Ref, CertDb, RefDb, PemCache, File) ->
    case ssl_pkix_db:ref_count(Ref, RefDb, 0) of
	0 ->	  
	    MD5 = crypto:hash(md5, File),
	    case ssl_pkix_db:lookup_cached_pem(PemCache, MD5) of
		[{Content, Ref}] ->
		    ssl_pkix_db:insert(MD5, Content, PemCache);		
		_ ->
		    ok
	    end,
	    ssl_pkix_db:remove(Ref, RefDb),
	    ssl_pkix_db:remove_trusted_certs(Ref, CertDb);
	_ ->
	    ok
    end.
