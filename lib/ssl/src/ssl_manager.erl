%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Manages ssl sessions and trusted certifacates
%%----------------------------------------------------------------------

-module(ssl_manager).
-behaviour(gen_server).

%% Internal application API
-export([start_link/1, start_link_dist/1,
	 connection_init/3, cache_pem_file/2,
	 lookup_trusted_cert/4,
	 new_session_id/1, clean_cert_db/2,
	 register_session/2, register_session/3, invalidate_session/2,
	 insert_crls/2, insert_crls/3, delete_crls/1, delete_crls/2, 
	 invalidate_session/3, name/1]).

-export([init_session_validator/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {
	  session_cache_client    :: db_handle(),
	  session_cache_server    :: db_handle(),
	  session_cache_cb        :: atom(),
	  session_lifetime        :: integer(),
	  certificate_db          :: db_handle(),
	  session_validation_timer :: reference(),
	  last_delay_timer  = {undefined, undefined},%% Keep for testing purposes	 
	  session_cache_client_max   :: integer(),
	  session_cache_server_max   :: integer(),
	  session_server_invalidator :: undefined | pid(),
	  session_client_invalidator :: undefined | pid()
	 }).

-define(GEN_UNIQUE_ID_MAX_TRIES, 10).
-define(SESSION_VALIDATION_INTERVAL, 60000).
-define(CLEAN_SESSION_DB, 60000).
-define(CLEAN_CERT_DB, 500).
-define(DEFAULT_MAX_SESSION_CACHE, 1000).
-define(LOAD_MITIGATION, 10).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
-spec name(normal | dist) -> atom().
%%
%% Description: Returns the registered name of the ssl manager process
%% in the operation modes 'normal' and 'dist'.
%%--------------------------------------------------------------------
name(normal) ->
    ?MODULE;
name(dist) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_dist").

%%--------------------------------------------------------------------
-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
%%
%% Description: Starts the ssl manager that takes care of sessions
%% and certificate caching.
%%--------------------------------------------------------------------
start_link(Opts) ->
    MangerName = name(normal),
    CacheName = ssl_pem_cache:name(normal),
    gen_server:start_link({local, MangerName}, 
			  ?MODULE, [MangerName, CacheName, Opts], []).

%%--------------------------------------------------------------------
-spec start_link_dist(list()) -> {ok, pid()} | ignore | {error, term()}.
%%
%% Description: Starts a special instance of the ssl manager to
%% be used by the erlang distribution. Note disables soft upgrade!
%%--------------------------------------------------------------------
start_link_dist(Opts) ->
    DistMangerName = name(dist),
    DistCacheName = ssl_pem_cache:name(dist),
    gen_server:start_link({local, DistMangerName}, 
			  ?MODULE, [DistMangerName, DistCacheName, Opts], []).

%%--------------------------------------------------------------------
-spec connection_init(binary()| {der, list()}, client | server, 
		      {Cb :: atom(), Handle:: term()}) ->
			     {ok, map()}.
%%			     
%% Description: Do necessary initializations for a new connection.
%%--------------------------------------------------------------------
connection_init({der, _} = Trustedcerts, Role, CRLCache) ->
    {ok, Extracted} = ssl_pkix_db:extract_trusted_certs(Trustedcerts),
    call({connection_init, Extracted, Role, CRLCache});
connection_init(Trustedcerts, Role, CRLCache) ->
    call({connection_init, Trustedcerts, Role, CRLCache}).

%%--------------------------------------------------------------------
-spec cache_pem_file(binary(), term()) -> {ok, term()} | {error, reason()}.
%%		    
%% Description: Cache a pem file and return its content.
%%--------------------------------------------------------------------
cache_pem_file(File, DbHandle) ->
    case ssl_pkix_db:lookup(File, DbHandle) of
	[Content]  ->
	    {ok, Content};
	undefined ->
            ssl_pem_cache:insert(File)
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
    erlang:send_after(?CLEAN_CERT_DB, get(ssl_manager), 
		      {clean_cert_db, Ref, File}),
    ok.

%%--------------------------------------------------------------------
%%
%% Description: Make the session available for reuse.
%%--------------------------------------------------------------------
-spec register_session(host(), inet:port_number(), #session{}) -> ok.
register_session(Host, Port, Session) ->
    cast({register_session, Host, Port, Session}).

-spec register_session(inet:port_number(), #session{}) -> ok.
register_session(Port, Session) ->
    cast({register_session, Port, Session}).
%%--------------------------------------------------------------------
%%
%% Description: Make the session unavailable for reuse. After
%% a the session has been marked "is_resumable = false" for some while
%% it will be safe to remove the data from the session database.
%%--------------------------------------------------------------------
-spec invalidate_session(host(), inet:port_number(), #session{}) -> ok.
invalidate_session(Host, Port, Session) ->
    load_mitigation(),
    cast({invalidate_session, Host, Port, Session}).

-spec invalidate_session(inet:port_number(), #session{}) -> ok.
invalidate_session(Port, Session) ->
    load_mitigation(),
    cast({invalidate_session, Port, Session}).

insert_crls(Path, CRLs)->
    insert_crls(Path, CRLs, normal).
insert_crls(?NO_DIST_POINT_PATH = Path, CRLs, ManagerType)->
    put(ssl_manager, name(ManagerType)),
    cast({insert_crls, Path, CRLs});
insert_crls(Path, CRLs, ManagerType)->
    put(ssl_manager, name(ManagerType)),
    call({insert_crls, Path, CRLs}).

delete_crls(Path)->
    delete_crls(Path, normal).
delete_crls(?NO_DIST_POINT_PATH = Path, ManagerType)->
    put(ssl_manager, name(ManagerType)),
    cast({delete_crls, Path});
delete_crls(Path, ManagerType)->
    put(ssl_manager, name(ManagerType)),
    call({delete_crls, Path}).

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
init([ManagerName, PemCacheName, Opts]) ->
    put(ssl_manager, ManagerName),
    put(ssl_pem_cache, PemCacheName),
    process_flag(trap_exit, true),
    CacheCb = proplists:get_value(session_cb, Opts, ssl_session_cache),
    SessionLifeTime =  
	proplists:get_value(session_lifetime, Opts, ?'24H_in_sec'),
    CertDb = ssl_pkix_db:create(PemCacheName),
    ClientSessionCache = 
	CacheCb:init([{role, client} | 
		      proplists:get_value(session_cb_init_args, Opts, [])]),
    ServerSessionCache = 
	CacheCb:init([{role, server} | 
		      proplists:get_value(session_cb_init_args, Opts, [])]),
    Timer = erlang:send_after(SessionLifeTime * 1000 + 5000, 
			      self(), validate_sessions),
    {ok, #state{certificate_db = CertDb,
		session_cache_client = ClientSessionCache,
		session_cache_server = ServerSessionCache,
		session_cache_cb = CacheCb,
		session_lifetime = SessionLifeTime,
		session_validation_timer = Timer,
		session_cache_client_max = 
		    max_session_cache_size(session_cache_client_max),
		session_cache_server_max = 
		    max_session_cache_size(session_cache_server_max),
		session_client_invalidator = undefined,
		session_server_invalidator = undefined
	       }}.

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
handle_call({{connection_init, <<>>, Role, {CRLCb, UserCRLDb}}, _Pid}, _From,
	    #state{certificate_db = [CertDb, FileRefDb, PemChace | _] = Db} = State) ->
    Ref = make_ref(), 
    {reply, {ok, #{cert_db_ref => Ref, 
                   cert_db_handle => CertDb, 
                   fileref_db_handle => FileRefDb, 
                   pem_cache => PemChace, 
                   session_cache => session_cache(Role, State), 
                   crl_db_info => {CRLCb, crl_db_info(Db, UserCRLDb)}}}, State};

handle_call({{connection_init, Trustedcerts, Role, {CRLCb, UserCRLDb}}, Pid}, _From,
	    #state{certificate_db = [CertDb, FileRefDb, PemChace | _] = Db} = State) ->
    case add_trusted_certs(Pid, Trustedcerts, Db) of
	{ok, Ref} ->
	    {reply, {ok, #{cert_db_ref => Ref, 
                           cert_db_handle => CertDb, 
                           fileref_db_handle => FileRefDb, 
                           pem_cache => PemChace, 
                           session_cache => session_cache(Role, State), 
                           crl_db_info => {CRLCb, crl_db_info(Db, UserCRLDb)}}}, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({{insert_crls, Path, CRLs}, _}, _From,   
	    #state{certificate_db = Db} = State) ->
    ssl_pkix_db:add_crls(Db, Path, CRLs),
    {reply, ok, State};

handle_call({{delete_crls, CRLsOrPath}, _}, _From,   
	    #state{certificate_db = Db} = State) ->
    ssl_pkix_db:remove_crls(Db, CRLsOrPath),
    {reply, ok, State};

handle_call({{new_session_id, Port}, _},
	    _, #state{session_cache_cb = CacheCb,
		      session_cache_server = Cache} = State) ->
    Id = new_id(Port, ?GEN_UNIQUE_ID_MAX_TRIES, Cache, CacheCb),
    {reply, Id, State}.

%%--------------------------------------------------------------------
-spec  handle_cast(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.  
%%				      | {noreply, #state{}, timeout()} |
%%				       {stop, reason(), #state{}}.
%%
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register_session, Host, Port, Session}, State0) ->
    State = ssl_client_register_session(Host, Port, Session, State0), 
    {noreply, State};

handle_cast({register_session, Port, Session}, State0) ->    
    State = server_register_session(Port, Session, State0), 		       
    {noreply, State};

handle_cast({invalidate_session, Host, Port,
	     #session{session_id = ID} = Session},
	    #state{session_cache_client = Cache,
		   session_cache_cb = CacheCb} = State) ->
    invalidate_session(Cache, CacheCb, {{Host, Port}, ID}, Session, State);

handle_cast({invalidate_session, Port, #session{session_id = ID} = Session},
	    #state{session_cache_server = Cache,
		   session_cache_cb = CacheCb} = State) ->
    invalidate_session(Cache, CacheCb, {Port, ID}, Session, State);


handle_cast({insert_crls, Path, CRLs},   
	    #state{certificate_db = Db} = State) ->
    ssl_pkix_db:add_crls(Db, Path, CRLs),
    {noreply, State};

handle_cast({delete_crls, CRLsOrPath},   
	    #state{certificate_db = Db} = State) ->
    ssl_pkix_db:remove_crls(Db, CRLsOrPath),
    {noreply, State}.

%%--------------------------------------------------------------------
-spec handle_info(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.
%%				      |{noreply, #state{}, timeout()} |
%%				      {stop, reason(), #state{}}.
%%
%% Description: Handling all non call/cast messages
%%-------------------------------------------------------------------
handle_info(validate_sessions, #state{session_cache_cb = CacheCb,
				      session_cache_client = ClientCache,
				      session_cache_server = ServerCache,
				      session_lifetime = LifeTime,
				      session_client_invalidator = Client,
				      session_server_invalidator = Server				      
				     } = State) ->
    Timer = erlang:send_after(?SESSION_VALIDATION_INTERVAL, 
			      self(), validate_sessions),
    CPid = start_session_validator(ClientCache, CacheCb, LifeTime, Client),
    SPid = start_session_validator(ServerCache, CacheCb, LifeTime, Server),
    {noreply, State#state{session_validation_timer = Timer, 
			  session_client_invalidator = CPid,
			  session_server_invalidator = SPid}};


handle_info({delayed_clean_session, Key, Cache}, #state{session_cache_cb = CacheCb
						       } = State) ->
    CacheCb:delete(Cache, Key),
    {noreply, State};

handle_info({clean_cert_db, Ref, File},
	    #state{certificate_db = [CertDb, {RefDb, FileMapDb} | _]} = State) ->
    
    case ssl_pkix_db:lookup(Ref, RefDb) of
	undefined -> %% Alredy cleaned
	    ok;
	_ ->
	    clean_cert_db(Ref, CertDb, RefDb, FileMapDb, File)
    end,
    {noreply, State};

handle_info({'EXIT', Pid, _}, #state{session_client_invalidator = Pid} = State) ->
    {noreply, State#state{session_client_invalidator = undefined}};
handle_info({'EXIT', Pid, _}, #state{session_server_invalidator = Pid} = State) ->
    {noreply, State#state{session_server_invalidator = undefined}};

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
			  session_cache_client = ClientSessionCache,
			  session_cache_server = ServerSessionCache,
			  session_cache_cb = CacheCb,
			  session_validation_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    ssl_pkix_db:remove(Db),
    catch CacheCb:terminate(ClientSessionCache),
    catch CacheCb:terminate(ServerSessionCache),
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
		    
start_session_validator(Cache, CacheCb, LifeTime, undefined) ->
    spawn_link(?MODULE, init_session_validator, 
	       [[get(ssl_manager), Cache, CacheCb, LifeTime]]);
start_session_validator(_,_,_, Pid) ->
    Pid.

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

max_session_cache_size(CacheType) ->
    case application:get_env(ssl, CacheType) of
	{ok, Size} when is_integer(Size) ->
	    Size;
	_ ->
	   ?DEFAULT_MAX_SESSION_CACHE
    end.

invalidate_session(Cache, CacheCb, Key, Session, State) ->
    case CacheCb:lookup(Cache, Key) of
	undefined -> %% Session is already invalidated
	    {noreply, State};
	#session{is_resumable = new} ->
	    CacheCb:delete(Cache, Key),
	    {noreply, State};
	_ ->
	    delayed_invalidate_session(CacheCb, Cache, Key, Session, State)
    end.

delayed_invalidate_session(CacheCb, Cache, Key, Session, 
			   #state{last_delay_timer = LastTimer} = State) ->
    %% When a registered session is invalidated we need to
    %% wait a while before deleting it as there might be
    %% pending connections that rightfully needs to look up
    %% the session data but new connections should not get to
    %% use this session.
    CacheCb:update(Cache, Key, Session#session{is_resumable = false}),
    TRef =
	erlang:send_after(delay_time(), self(), 
			  {delayed_clean_session, Key, Cache}),
    {noreply, State#state{last_delay_timer = 
			      last_delay_timer(Key, TRef, LastTimer)}}.

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
    Id = ssl_cipher:random_bytes(?NUM_OF_SESSION_ID_BYTES),
    case CacheCb:lookup(Cache, {Port, Id}) of
	undefined ->
	    Now = erlang:monotonic_time(),
	    %% New sessions can not be set to resumable
	    %% until handshake is compleate and the
	    %% other session values are set.
	    CacheCb:update(Cache, {Port, Id}, #session{session_id = Id,
						       is_resumable = new,
						       time_stamp = Now}),
	    Id;
	_ ->
	    new_id(Port, Tries - 1, Cache, CacheCb)
    end.

clean_cert_db(Ref, CertDb, RefDb, FileMapDb, File) ->
    case ssl_pkix_db:ref_count(Ref, RefDb, 0) of
	0 ->	  
	    ssl_pkix_db:remove(Ref, RefDb),
	    ssl_pkix_db:remove(File, FileMapDb),
	    ssl_pkix_db:remove_trusted_certs(Ref, CertDb);
	_ ->
	    ok
    end.

ssl_client_register_session(Host, Port, Session, #state{session_cache_client = Cache,
							session_cache_cb = CacheCb,
							session_cache_client_max = Max,
							session_client_invalidator = Pid0} = State) ->
    TimeStamp = erlang:monotonic_time(),
    NewSession = Session#session{time_stamp = TimeStamp},
    
    case CacheCb:select_session(Cache, {Host, Port}) of
	no_session ->
	    Pid = do_register_session({{Host, Port}, 
				     NewSession#session.session_id}, 
				      NewSession, Max, Pid0, Cache, CacheCb),
	    State#state{session_client_invalidator = Pid};
	Sessions ->
	    register_unique_session(Sessions, NewSession, {Host, Port}, State)
    end.

server_register_session(Port, Session, #state{session_cache_server_max = Max,
					      session_cache_server = Cache,
					      session_cache_cb = CacheCb,
					      session_server_invalidator = Pid0} = State) ->
    TimeStamp =  erlang:monotonic_time(),
    NewSession = Session#session{time_stamp = TimeStamp},
    Pid = do_register_session({Port, NewSession#session.session_id}, 
			      NewSession, Max, Pid0, Cache, CacheCb),
    State#state{session_server_invalidator = Pid}.

do_register_session(Key, Session, Max, Pid, Cache, CacheCb) ->
    try CacheCb:size(Cache) of
	Size when Size >= Max ->
	    invalidate_session_cache(Pid, CacheCb, Cache);
	_ ->	
	    CacheCb:update(Cache, Key, Session),
	    Pid
    catch 
	error:undef ->
	    CacheCb:update(Cache, Key, Session),
            Pid		
    end.


%% Do not let dumb clients create a gigantic session table
%% for itself creating big delays at connection time. 
register_unique_session(Sessions, Session, PartialKey, 
			#state{session_cache_client_max = Max,
			       session_cache_client = Cache,
			       session_cache_cb = CacheCb,
			       session_client_invalidator = Pid0} = State) ->
    case exists_equivalent(Session , Sessions) of
	true ->
	    State;
	false ->
	    Pid = do_register_session({PartialKey, 
				       Session#session.session_id}, 
				      Session, Max, Pid0, Cache, CacheCb),
	    State#state{session_client_invalidator = Pid}
    end.

exists_equivalent(_, []) ->
    false;
exists_equivalent(#session{
		     peer_certificate = PeerCert,
		     own_certificate = OwnCert,
		     compression_method = Compress,
		     cipher_suite = CipherSuite,
		     srp_username = SRP,
		     ecc = ECC} , 
		  [#session{
		      peer_certificate = PeerCert,
		      own_certificate = OwnCert,
		      compression_method = Compress,
		      cipher_suite = CipherSuite,
		      srp_username = SRP,
		      ecc = ECC} | _]) ->
    true;
exists_equivalent(Session, [ _ | Rest]) ->
    exists_equivalent(Session, Rest).

add_trusted_certs(Pid, Trustedcerts, Db) ->
    try
	ssl_pkix_db:add_trusted_certs(Pid, Trustedcerts, Db) 	    
    catch
	_:Reason ->
	    {error, Reason}
    end.

session_cache(client, #state{session_cache_client = Cache}) ->
    Cache;
session_cache(server, #state{session_cache_server = Cache}) ->
    Cache.

crl_db_info([_,_,_,Local], {internal, Info}) ->
    {Local, Info};
crl_db_info(_, UserCRLDb) ->
    UserCRLDb.

%% Only start a session invalidator if there is not
%% one already active
invalidate_session_cache(undefined, CacheCb, Cache) ->
    start_session_validator(Cache, CacheCb, {invalidate_before, erlang:monotonic_time()}, undefined);
invalidate_session_cache(Pid, _CacheCb, _Cache) ->
    Pid.

load_mitigation() ->
    MSec = rand:uniform(?LOAD_MITIGATION),
    receive 
    after 
	MSec ->
	    continue
    end.
