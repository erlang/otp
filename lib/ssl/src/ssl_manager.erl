%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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
%% (Note: See the document internal_doc/pem_and_cert_cache.md for additional
%% information)
%%----------------------------------------------------------------------

-module(ssl_manager).
-moduledoc false.
-behaviour(gen_server).

%% Internal application API
-export([start_link/1, start_link_dist/1,
	 connection_init/3, cache_pem_file/2,
	 lookup_trusted_cert/4,
	 clean_cert_db/2,
         refresh_trusted_db/1, refresh_trusted_db/2,
	 register_session/4, invalidate_session/2,
	 insert_crls/2, insert_crls/3, delete_crls/1, delete_crls/2, 
	 invalidate_session/3, name/1]).

-export([init_session_validator/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").
-include("ssl_api.hrl").

-include_lib("kernel/include/file.hrl").

-record(state, {
	  session_cache_client    :: db_handle(),
	  session_cache_client_cb :: atom(),
	  session_lifetime        :: integer(),
	  certificate_db          :: db_handle(),
	  session_validation_timer :: reference(),
	  session_cache_client_max   :: integer(),
          session_client_invalidator :: undefined | pid(),
          options                    :: list(),
          client_session_order       :: gb_trees:tree()
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
            case ssl_pkix_db:decode_pem_file(File) of
                {ok, Content} ->
                    ssl_pem_cache:insert(File, Content),
                    {ok, Content};
                Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
-spec lookup_trusted_cert(term(), reference(), serialnumber(), issuer()) ->
          undefined |
          {ok, public_key:combined_cert()}.
%%
%% Description: Lookup the trusted cert with Key = {reference(),
%% serialnumber(), issuer()}.
%% --------------------------------------------------------------------
lookup_trusted_cert(DbHandle, Ref, SerialNumber, Issuer) ->
    ssl_pkix_db:lookup_trusted_cert(DbHandle, Ref, SerialNumber, Issuer).

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
-spec refresh_trusted_db(normal | dist) -> ok.
%%
%% Description: Send refresh of trusted cert db to ssl_manager process should
%% be called by ssl-connection processes.
%%--------------------------------------------------------------------
refresh_trusted_db(ManagerType) ->
    put(ssl_manager, name(ManagerType)),
    call(refresh_trusted_db).

refresh_trusted_db(ManagerType, File) ->
    put(ssl_manager, name(ManagerType)),
    call({refresh_trusted_db, File}).

%%--------------------------------------------------------------------
%%
%% Description: Make the session available for reuse.
%%--------------------------------------------------------------------
-spec register_session(ssl:host(), inet:port_number(), #session{}, unique | true) -> ok.
register_session(Host, Port, Session, true) ->
    call({register_session, Host, Port, Session});
register_session(Host, Port, Session, unique = Save) ->
    cast({register_session, Host, Port, Session, Save}).

%%--------------------------------------------------------------------
%%
%% Description: Make the session unavailable for reuse. After
%% a the session has been marked "is_resumable = false" for some while
%% it will be safe to remove the data from the session database.
%%--------------------------------------------------------------------
-spec invalidate_session(ssl:host(), inet:port_number(), #session{}) -> ok.
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

    #{session_cb := DefaultCacheCb,
      session_cb_init_args := DefaultCacheCbInitArgs,
      lifetime := DefaultSessLifeTime,
      max := ClientSessMax
     } = ssl_config:pre_1_3_session_opts(client),
    CacheCb = proplists:get_value(session_cb, Opts, DefaultCacheCb),
    SessionLifeTime =  
	proplists:get_value(session_lifetime, Opts, DefaultSessLifeTime),
    ClientSessionCache = 
	CacheCb:init([{role, client} | 
		      proplists:get_value(session_cb_init_args, Opts, DefaultCacheCbInitArgs)]),

    CertDb = ssl_pkix_db:create(PemCacheName),
    Timer = erlang:send_after(SessionLifeTime * 1000 + 5000, 
			      self(), validate_sessions),
    {ok, #state{certificate_db = CertDb,
		session_cache_client = ClientSessionCache,
		session_cache_client_cb = CacheCb,
		session_lifetime = SessionLifeTime,
		session_validation_timer = Timer,
		session_cache_client_max = ClientSessMax,
		session_client_invalidator = undefined,
                options = Opts,
                client_session_order = gb_trees:empty()
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
	    #state{certificate_db = [CertDb, FileRefDb, PemCache | _] = Db} = State) ->
    Ref = make_ref(), 
    {reply, {ok, #{cert_db_ref => Ref, 
                   cert_db_handle => CertDb, 
                   fileref_db_handle => FileRefDb, 
                   pem_cache => PemCache,
                   session_cache => session_cache(Role, State), 
                   crl_db_info => {CRLCb, crl_db_info(Db, UserCRLDb)}}}, State};

handle_call({{connection_init, Trustedcerts, Role, {CRLCb, UserCRLDb}}, Pid}, _From,
	    #state{certificate_db = [CertDb, FileRefDb, PemCache | _] = Db} = State) ->
    case add_trusted_certs(Pid, Trustedcerts, Db) of
	{ok, Ref} ->
	    {reply, {ok, #{cert_db_ref => Ref, 
                           cert_db_handle => CertDb, 
                           fileref_db_handle => FileRefDb, 
                           pem_cache => PemCache,
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
handle_call({{register_session, Host, Port, Session}, _}, _, State0) ->
    State = client_register_session(Host, Port, Session, State0), 
    {reply, ok, State};
handle_call({refresh_trusted_db, _}, _, #state{certificate_db = Db} = State) ->
    PemCache = get(ssl_pem_cache),
    ssl_pkix_db:refresh_trusted_certs(Db, PemCache),
    {reply, ok, State};
handle_call({{refresh_trusted_db, File}, _}, _, #state{certificate_db = Db} = State) ->
    PemCache = get(ssl_pem_cache),
    ssl_pkix_db:refresh_trusted_certs(File, Db, PemCache),
    {reply, ok, State}.

%%--------------------------------------------------------------------
-spec  handle_cast(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.  
%%				      | {noreply, #state{}, timeout()} |
%%				       {stop, reason(), #state{}}.
%%
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register_session, Host, Port, Session, unique}, State0) ->
    State = client_register_unique_session(Host, Port, Session, State0), 
    {noreply, State};

handle_cast({invalidate_session, Host, Port,
	     #session{session_id = ID} = Session},
	    #state{session_cache_client = Cache,
		   session_cache_client_cb = CacheCb} = State) ->
    invalidate_session(Cache, CacheCb, {{Host, Port}, ID}, Session, State);
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
handle_info(validate_sessions, #state{session_cache_client_cb = CacheCb,
				      session_cache_client = ClientCache,
				      session_lifetime = LifeTime,
				      session_client_invalidator = Client
				     } = State) ->
    Timer = erlang:send_after(?SESSION_VALIDATION_INTERVAL, 
			      self(), validate_sessions),
    CPid = start_session_validator(ClientCache, CacheCb, LifeTime, Client),
    {noreply, State#state{session_validation_timer = Timer, 
			  session_client_invalidator = CPid}};

handle_info({clean_cert_db, Ref, File},
	    #state{certificate_db = [CertDb, {RefDb, FileMapDb} | _]} = State) ->
    
    case ssl_pkix_db:lookup(Ref, RefDb) of
	undefined -> %% Already cleaned
	    ok;
	_ ->
	    clean_cert_db(Ref, CertDb, RefDb, FileMapDb, File)
    end,
    {noreply, State};
handle_info({'EXIT', Pid, _}, #state{session_client_invalidator = Pid} = State) ->
    {noreply, State#state{session_client_invalidator = undefined}};
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
			  session_cache_client_cb = CacheCb,
			  session_validation_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    ssl_pkix_db:remove(Db),
    catch CacheCb:terminate(ClientSessionCache),
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

invalidate_session(Cache, CacheCb, Key, _Session,
                   #state{client_session_order = Order} = State) ->
    case CacheCb:lookup(Cache, Key) of
	undefined -> %% Session is already invalidated
	    {noreply, State};
	#session{internal_id = InternalId} ->
	    CacheCb:delete(Cache, Key),
	    {noreply, State#state{session_cache_client = Cache,
                                  client_session_order = gb_trees:delete(InternalId, Order)}}
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

client_register_unique_session(Host, Port, Session, #state{session_cache_client = Cache0,
                                                           session_cache_client_cb = CacheCb,
                                                           session_cache_client_max = Max,
                                                           options = Options,
                                                           client_session_order = Order0} = State) ->
    TimeStamp = erlang:monotonic_time(),
    NewSession = Session#session{time_stamp = TimeStamp},
    
    case CacheCb:select_session(Cache0, {Host, Port}) of
	no_session ->
	    {Cache, Order} = do_register_session({{Host, Port},
                                                  NewSession#session.session_id},
                                                 NewSession, Max, Cache0, CacheCb, Options, Order0),
	    State#state{session_cache_client = Cache, client_session_order = Order};
	Sessions ->
	    register_unique_session(Sessions, NewSession, {Host, Port}, State)
    end.

client_register_session(Host, Port, Session, #state{session_cache_client = Cache0,
                                                    session_cache_client_cb = CacheCb,
                                                    session_cache_client_max = Max,
                                                    options = Options,
                                                    client_session_order = Order0} = State) ->
    TimeStamp = erlang:monotonic_time(),
    NewSession = Session#session{time_stamp = TimeStamp},
    SessionId = NewSession#session.session_id,
    {Cache, Order} = do_register_session({{Host, Port}, SessionId},
                                         NewSession, Max, Cache0, CacheCb, Options, Order0),
    State#state{session_cache_client = Cache,
                client_session_order = Order}.

do_register_session(Key, #session{time_stamp = TimeStamp} = Session0,
                    Max, Cache, CacheCb, Options, Order0) ->
    try 
        case CacheCb:size(Cache) of
            Max ->
                InternalId = {TimeStamp, erlang:unique_integer([monotonic])},
                Session = Session0#session{internal_id = InternalId},
                {_, OldKey, Order1} = gb_trees:take_smallest(Order0),
                Order = gb_trees:insert(InternalId, Key, Order1),
                CacheCb:delete(Cache, OldKey),
                CacheCb:update(Cache, Key, Session),
                {Cache, Order};
            _ ->
                InternalId = {TimeStamp, erlang:unique_integer([monotonic])},
                Session = Session0#session{internal_id = InternalId},
                Order = gb_trees:insert(InternalId, Key, Order0),
                CacheCb:update(Cache, Key, Session),
                {Cache, Order}
        end
    catch 
	_:_ ->
            %% Backwards compatibility if size functions is not implemented by callback
            Args = proplists:get_value(session_cb_init_args, Options, []),
            CacheCb:terminate(Cache),
	    {CacheCb:init(Args), gb_trees:empty()}
    end.


%% Do not let dumb clients create a gigantic session table
%% for itself creating big delays at connection time. 
register_unique_session(Sessions, Session, PartialKey, 
			#state{session_cache_client_max = Max,
			       session_cache_client = Cache0,
			       session_cache_client_cb = CacheCb,
                               options = Options,
                               client_session_order = Order0} = State) ->
    case exists_equivalent(Session , Sessions) of
	true ->
	    State;
	false ->
	    {Cache, Order} = do_register_session({PartialKey,
                                                  Session#session.session_id},
                                                 Session, Max, Cache0, CacheCb, Options, Order0),
	    State#state{session_cache_client = Cache,
                        client_session_order = Order}
    end.

exists_equivalent(_, []) ->
    false;
exists_equivalent(#session{
		     peer_certificate = PeerCert,
		     own_certificates = [OwnCert | _],
		     cipher_suite = CipherSuite,
		     srp_username = SRP,
		     ecc = ECC} , 
		  [#session{
		      peer_certificate = PeerCert,
		      own_certificates = [OwnCert | _],
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
session_cache(server, _) ->
    no_longer_defined.

crl_db_info([_,_,_,Local], {internal, Info}) ->
    {Local, Info};
crl_db_info(_, UserCRLDb) ->
    UserCRLDb.

load_mitigation() ->
    MSec = rand:uniform(?LOAD_MITIGATION),
    receive 
    after 
	MSec ->
	    continue
    end.
