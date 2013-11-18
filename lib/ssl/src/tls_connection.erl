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

%%
%%----------------------------------------------------------------------
%% Purpose: Handles an ssl connection, e.i. both the setup
%% e.i. SSL-Handshake, SSL-Alert and SSL-Cipher protocols and delivering
%% data to the application. All data on the connectinon is received and 
%% sent according to the SSL-record protocol.  
%%----------------------------------------------------------------------

-module(tls_connection).

-behaviour(gen_fsm).

-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("ssl_alert.hrl").
-include("tls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl"). 

%% Internal application API
-export([send/2, recv/3, connect/7, ssl_accept/6, handshake/2,
	 socket_control/3, close/1, shutdown/2,
	 new_user/2, get_opts/2, set_opts/2, info/1, session_info/1, 
	 peer_certificate/1, renegotiation/1, negotiated_next_protocol/1, prf/5,
	 send_handshake/2, send_alert/2, send_change_cipher/2, next_record/1, next_state/4,
	 handle_unexpected_message/3, ack_connection/1, handle_own_alert/4, next_state_connection/2,
	 register_session/4
	]).

%% Called by ssl_connection_sup
-export([start_link/7]). 

%% gen_fsm callbacks
-export([init/1, hello/2, certify/2, cipher/2,
	 abbreviated/2, connection/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%%====================================================================
%% Internal application API
%%====================================================================	     

%%--------------------------------------------------------------------
-spec send(pid(), iodata()) -> ok | {error, reason()}.
%%
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(Pid, Data) -> 
    sync_send_all_state_event(Pid, {application_data, 
				    %% iolist_to_binary should really
				    %% be called iodata_to_binary()
				    erlang:iolist_to_binary(Data)}).

%%--------------------------------------------------------------------
-spec recv(pid(), integer(), timeout()) ->  
    {ok, binary() | list()} | {error, reason()}.
%%
%% Description:  Receives data when active = false
%%--------------------------------------------------------------------
recv(Pid, Length, Timeout) -> 
    sync_send_all_state_event(Pid, {recv, Length, Timeout}).
%%--------------------------------------------------------------------
-spec connect(host(), inet:port_number(), port(), {#ssl_options{}, #socket_options{}},
	      pid(), tuple(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Connect to an ssl server.
%%--------------------------------------------------------------------
connect(Host, Port, Socket, Options, User, CbInfo, Timeout) ->
    try start_fsm(client, Host, Port, Socket, Options, User, CbInfo,
		  Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssl_not_started}
    end.
%%--------------------------------------------------------------------
-spec ssl_accept(inet:port_number(), port(), {#ssl_options{}, #socket_options{}},
				      pid(), tuple(), timeout()) ->
    {ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake. 
%%--------------------------------------------------------------------
ssl_accept(Port, Socket, Opts, User, CbInfo, Timeout) ->
    try start_fsm(server, "localhost", Port, Socket, Opts, User, 
		  CbInfo, Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssl_not_started}
    end.	

%%--------------------------------------------------------------------
-spec handshake(#sslsocket{}, timeout()) ->  ok | {error, reason()}.
%%
%% Description: Starts ssl handshake. 
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = Pid}, Timeout) ->  
    case sync_send_all_state_event(Pid, {start, Timeout}) of
	connected ->
	    ok;
 	Error ->
	    Error
    end.
%--------------------------------------------------------------------
-spec socket_control(port(), pid(), atom()) -> 
    {ok, #sslsocket{}} | {error, reason()}.  
%%
%% Description: Set the ssl process to own the accept socket
%%--------------------------------------------------------------------	    
socket_control(Socket, Pid, Transport) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    {ok, ssl_socket:socket(Pid, Transport, Socket, ?MODULE)};
	{error, Reason}	->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
-spec close(pid()) -> ok | {error, reason()}.  
%%
%% Description:  Close an ssl connection
%%--------------------------------------------------------------------
close(ConnectionPid) ->
    case sync_send_all_state_event(ConnectionPid, close) of
	{error, closed} ->
	    ok;
	Other ->
	    Other
    end.

%%--------------------------------------------------------------------
-spec shutdown(pid(), atom()) -> ok | {error, reason()}.  
%%
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(ConnectionPid, How) ->
    sync_send_all_state_event(ConnectionPid, {shutdown, How}).

%%--------------------------------------------------------------------
-spec new_user(pid(), pid()) ->  ok | {error, reason()}.
%%
%% Description:  Changes process that receives the messages when active = true
%% or once. 
%%--------------------------------------------------------------------
new_user(ConnectionPid, User) ->
    sync_send_all_state_event(ConnectionPid, {new_user, User}).

%%--------------------------------------------------------------------
-spec negotiated_next_protocol(pid()) -> {ok, binary()} | {error, reason()}.
%%
%% Description:  Returns the negotiated protocol
%%--------------------------------------------------------------------
negotiated_next_protocol(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, negotiated_next_protocol).

%%--------------------------------------------------------------------
-spec get_opts(pid(), list()) -> {ok, list()} | {error, reason()}.    
%%
%% Description: Same as inet:getopts/2
%%--------------------------------------------------------------------
get_opts(ConnectionPid, OptTags) ->
    sync_send_all_state_event(ConnectionPid, {get_opts, OptTags}).
%%--------------------------------------------------------------------
-spec set_opts(pid(), list()) -> ok | {error, reason()}. 
%%
%% Description:  Same as inet:setopts/2
%%--------------------------------------------------------------------
set_opts(ConnectionPid, Options) ->
    sync_send_all_state_event(ConnectionPid, {set_opts, Options}).

%%--------------------------------------------------------------------
-spec info(pid()) ->  {ok, {atom(), tuple()}} | {error, reason()}. 
%%
%% Description:  Returns ssl protocol and cipher used for the connection
%%--------------------------------------------------------------------
info(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, info). 

%%--------------------------------------------------------------------
-spec session_info(pid()) -> {ok, list()} | {error, reason()}. 
%%
%% Description:  Returns info about the ssl session
%%--------------------------------------------------------------------
session_info(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, session_info). 

%%--------------------------------------------------------------------
-spec peer_certificate(pid()) -> {ok, binary()| undefined} | {error, reason()}.
%%
%% Description: Returns the peer cert
%%--------------------------------------------------------------------
peer_certificate(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, peer_certificate). 

%%--------------------------------------------------------------------
-spec renegotiation(pid()) -> ok | {error, reason()}.
%%
%% Description: Starts a renegotiation of the ssl session.
%%--------------------------------------------------------------------
renegotiation(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, renegotiate). 

%%--------------------------------------------------------------------
-spec prf(pid(), binary() | 'master_secret', binary(),
	  binary() | ssl:prf_random(), non_neg_integer()) ->
		 {ok, binary()} | {error, reason()} | {'EXIT', term()}.
%%
%% Description: use a ssl sessions TLS PRF to generate key material
%%--------------------------------------------------------------------
prf(ConnectionPid, Secret, Label, Seed, WantedLength) ->
    sync_send_all_state_event(ConnectionPid, {prf, Secret, Label, Seed, WantedLength}).


send_handshake(Handshake, #state{negotiated_version = Version,
				 socket = Socket,
				 transport_cb = Transport,
				 tls_handshake_history = Hist0,
				 connection_states = ConnectionStates0} = State0) ->
    {BinHandshake, ConnectionStates, Hist} =
	encode_handshake(Handshake, Version, ConnectionStates0, Hist0),
    Transport:send(Socket, BinHandshake),
    State0#state{connection_states = ConnectionStates,
		tls_handshake_history = Hist
	       }.

send_alert(Alert, #state{negotiated_version = Version,
			 socket = Socket,
			 transport_cb = Transport,
			 connection_states = ConnectionStates0} = State0) ->
    {BinMsg, ConnectionStates} =
	encode_alert(Alert, Version, ConnectionStates0),
    Transport:send(Socket, BinMsg),
    State0#state{connection_states = ConnectionStates}.

send_change_cipher(Msg, #state{connection_states = ConnectionStates0,
			       socket = Socket,
			       negotiated_version = Version,
			       transport_cb = Transport} = State0) ->
    {BinChangeCipher, ConnectionStates} =
	encode_change_cipher(Msg, Version, ConnectionStates0),
    Transport:send(Socket, BinChangeCipher),
    State0#state{connection_states = ConnectionStates}.

%%====================================================================
%% ssl_connection_sup API
%%====================================================================

%%--------------------------------------------------------------------
-spec start_link(atom(), host(), inet:port_number(), port(), list(), pid(), tuple()) ->
    {ok, pid()} | ignore |  {error, reason()}.
%%
%% Description: Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Role, Host, Port, Socket, Options, User, CbInfo) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Role, Host, Port, Socket, Options, User, CbInfo]])}.

init([Role, Host, Port, Socket, {SSLOpts0, _} = Options,  User, CbInfo]) ->
    State0 = initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
    Handshake = ssl_handshake:init_handshake_history(),
    TimeStamp = calendar:datetime_to_gregorian_seconds({date(), time()}),
    try ssl_init(SSLOpts0, Role) of
	{ok, Ref, CertDbHandle, FileRefHandle, CacheHandle, OwnCert, Key, DHParams} ->
	    Session = State0#state.session,
	    State = State0#state{
				 tls_handshake_history = Handshake,
				 session = Session#session{own_certificate = OwnCert,
							   time_stamp = TimeStamp},
				 file_ref_db = FileRefHandle,
				 cert_db_ref = Ref,
				 cert_db = CertDbHandle,
				 session_cache = CacheHandle,
				 private_key = Key,
				 diffie_hellman_params = DHParams},
	    gen_fsm:enter_loop(?MODULE, [], hello, State, get_timeout(State))
    catch
	throw:Error ->
	    gen_fsm:enter_loop(?MODULE, [], error, {Error,State0}, get_timeout(State0))
    end.

%%--------------------------------------------------------------------
%% Description:There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent
%% using gen_fsm:send_event/2, the instance of this function with the
%% same name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
hello(start, #state{host = Host, port = Port, role = client,
		    ssl_options = SslOpts,
		    session = #session{own_certificate = Cert} = Session0,
		    session_cache = Cache, session_cache_cb = CacheCb,
		    transport_cb = Transport, socket = Socket,
		    connection_states = ConnectionStates0,
		    renegotiation = {Renegotiation, _}} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert),
    
    Version = Hello#client_hello.client_version,
    Handshake0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates, Handshake} =
        encode_handshake(Hello, Version, ConnectionStates0, Handshake0),
    Transport:send(Socket, BinMsg),
    State1 = State0#state{connection_states = ConnectionStates,
			  negotiated_version = Version, %% Requested version
			  session =
			      Session0#session{session_id = Hello#client_hello.session_id},
			  tls_handshake_history = Handshake},
    {Record, State} = next_record(State1),
    next_state(hello, hello, Record, State);

hello(Hello = #client_hello{client_version = ClientVersion,
			    extensions = #hello_extensions{hash_signs = HashSigns}},
      State = #state{connection_states = ConnectionStates0,
		     port = Port, session = #session{own_certificate = Cert} = Session0,
		     renegotiation = {Renegotiation, _},
		     session_cache = Cache,
		     session_cache_cb = CacheCb,
		     ssl_options = SslOpts}) ->
    HashSign = ssl_handshake:select_hashsign(HashSigns, Cert),
    case tls_handshake:hello(Hello, SslOpts, {Port, Session0, Cache, CacheCb,
					      ConnectionStates0, Cert}, Renegotiation) of
        {Version, {Type, Session},
	 ConnectionStates,
	 #hello_extensions{ec_point_formats = EcPointFormats,
			   elliptic_curves = EllipticCurves} = ServerHelloExt} ->
            ssl_connection:hello({common_client_hello, Type, ServerHelloExt, HashSign},
				 State#state{connection_states  = ConnectionStates,
					     negotiated_version = Version,
					     session = Session,
					     client_ecc = {EllipticCurves, EcPointFormats}}, ?MODULE);
        #alert{} = Alert ->
            handle_own_alert(Alert, ClientVersion, hello, State)
    end;
hello(#server_hello{cipher_suite = CipherSuite,
		    compression_method = Compression} = Hello,
      #state{session = #session{session_id = OldId},
	     connection_states = ConnectionStates0,
	     role = client,
	     negotiated_version = ReqVersion,
	     renegotiation = {Renegotiation, _},
	     ssl_options = SslOptions} = State0) ->
    case tls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, ReqVersion, hello, State0);
	{Version, NewId, ConnectionStates, NextProtocol} ->
	    {KeyAlgorithm, _, _, _} =
		ssl_cipher:suite_definition(CipherSuite),

	    PremasterSecret = make_premaster_secret(ReqVersion, KeyAlgorithm),
	    
	    NewNextProtocol = case NextProtocol of
				  undefined ->
				      State0#state.next_protocol;
				  _ ->
				      NextProtocol
			      end,

	    State = State0#state{key_algorithm = KeyAlgorithm,
				 negotiated_version = Version,
				 connection_states = ConnectionStates,
				 premaster_secret = PremasterSecret,
				 expecting_next_protocol_negotiation = NextProtocol =/= undefined,
				 next_protocol = NewNextProtocol},
	    
	    case ssl_session:is_new(OldId, NewId) of
		true ->
		    handle_new_session(NewId, CipherSuite, Compression,
				       State#state{connection_states = ConnectionStates});
		false ->
		    handle_resumed_session(NewId,
					   State#state{connection_states = ConnectionStates})
	    end
    end;

hello(Msg, State) ->
    ssl_connection:hello(Msg, State, ?MODULE).

abbreviated(Msg, State) ->
    ssl_connection:abbreviated(Msg, State, ?MODULE).

certify(Msg, State) ->
    ssl_connection:certify(Msg, State, ?MODULE).

cipher(Msg, State) ->
     ssl_connection:cipher(Msg, State, ?MODULE).

connection(#hello_request{}, #state{host = Host, port = Port,
				    session = #session{own_certificate = Cert} = Session0,
				    session_cache = Cache, session_cache_cb = CacheCb,
				    ssl_options = SslOpts,
				    connection_states = ConnectionStates0,
				    renegotiation = {Renegotiation, _}} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert),
    State1 = send_handshake(Hello, State0),
    {Record, State} =
	next_record(
	  State1#state{session = Session0#session{session_id
						  = Hello#client_hello.session_id}}),
    next_state(connection, hello, Record, State);

connection(#client_hello{} = Hello, #state{role = server, allow_renegotiate = true} = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    hello(Hello, State#state{allow_renegotiate = false});

connection(#client_hello{}, #state{role = server, allow_renegotiate = false} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State = send_alert(Alert, State0),
    next_state_connection(connection, State);
  
connection(Msg, State) ->
     ssl_connection:connection(Msg, State, tls_connection).

%%--------------------------------------------------------------------
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event. Not currently used!
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State, get_timeout(State)}.

%%--------------------------------------------------------------------
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event({application_data, Data}, From, connection, State) ->
    %% We should look into having a worker process to do this to 
    %% parallize send and receive decoding and not block the receiver
    %% if sending is overloading the socket.
    try
	write_application_data(Data, From, State)
    catch throw:Error ->
	    {reply, Error, connection, State, get_timeout(State)}
    end;
handle_sync_event({application_data, Data}, From, StateName, 
		  #state{send_queue = Queue} = State) ->
    %% In renegotiation priorities handshake, send data when handshake is finished
    {next_state, StateName,
     State#state{send_queue = queue:in({From, Data}, Queue)},
     get_timeout(State)};

handle_sync_event({start, Timeout}, StartFrom, hello, State) ->
    Timer = start_or_recv_cancel_timer(Timeout, StartFrom),
    hello(start, State#state{start_or_recv_from = StartFrom,
			     timer = Timer});

%% The two clauses below could happen if a server upgrades a socket in
%% active mode. Note that in this case we are lucky that
%% controlling_process has been evalueated before receiving handshake
%% messages from client. The server should put the socket in passive
%% mode before telling the client that it is willing to upgrade
%% and before calling ssl:ssl_accept/2. These clauses are 
%% here to make sure it is the users problem and not owers if
%% they upgrade an active socket. 
handle_sync_event({start,_}, _, connection, State) ->
    {reply, connected, connection, State, get_timeout(State)};
handle_sync_event({start,_}, _From, error, {Error, State = #state{}}) ->
    {stop, {shutdown, Error}, {error, Error}, State};

handle_sync_event({start, Timeout}, StartFrom, StateName, State) ->
    Timer = start_or_recv_cancel_timer(Timeout, StartFrom),
    {next_state, StateName, State#state{start_or_recv_from = StartFrom,
					timer = Timer}, get_timeout(State)};

handle_sync_event(close, _, StateName, State) ->
    %% Run terminate before returning
    %% so that the reuseaddr inet-option will work
    %% as intended.
    (catch terminate(user_close, StateName, State)),
    {stop, normal, ok, State#state{terminated = true}};

handle_sync_event({shutdown, How0}, _, StateName,
		  #state{transport_cb = Transport,
			 negotiated_version = Version,
			 connection_states = ConnectionStates,
			 socket = Socket} = State) ->
    case How0 of
	How when How == write; How == both ->	    
	    Alert = ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
	    {BinMsg, _} =
		encode_alert(Alert, Version, ConnectionStates),
	    Transport:send(Socket, BinMsg);
	_ ->
	    ok
    end,
    
    case Transport:shutdown(Socket, How0) of
	ok ->
	    {reply, ok, StateName, State, get_timeout(State)};
	Error ->
	    {stop, normal, Error, State}
    end;
    
handle_sync_event({recv, N, Timeout}, RecvFrom, connection = StateName, State0) ->
    Timer = start_or_recv_cancel_timer(Timeout, RecvFrom),
    passive_receive(State0#state{bytes_to_read = N,
				 start_or_recv_from = RecvFrom, timer = Timer}, StateName);

%% Doing renegotiate wait with handling request until renegotiate is
%% finished. Will be handled by next_state_is_connection/2.
handle_sync_event({recv, N, Timeout}, RecvFrom, StateName, State) ->
    Timer = start_or_recv_cancel_timer(Timeout, RecvFrom),
    {next_state, StateName, State#state{bytes_to_read = N, start_or_recv_from = RecvFrom,
					timer = Timer},
     get_timeout(State)};

handle_sync_event({new_user, User}, _From, StateName, 
		  State =#state{user_application = {OldMon, _}}) ->
    NewMon = erlang:monitor(process, User),
    erlang:demonitor(OldMon, [flush]),
    {reply, ok, StateName, State#state{user_application = {NewMon,User}},
     get_timeout(State)};

handle_sync_event({get_opts, OptTags}, _From, StateName,
		  #state{socket = Socket,
			 transport_cb = Transport,
			 socket_options = SockOpts} = State) ->
    OptsReply = get_socket_opts(Transport, Socket, OptTags, SockOpts, []),
    {reply, OptsReply, StateName, State, get_timeout(State)};

handle_sync_event(negotiated_next_protocol, _From, StateName, #state{next_protocol = undefined} = State) ->
    {reply, {error, next_protocol_not_negotiated}, StateName, State, get_timeout(State)};
handle_sync_event(negotiated_next_protocol, _From, StateName, #state{next_protocol = NextProtocol} = State) ->
    {reply, {ok, NextProtocol}, StateName, State, get_timeout(State)};

handle_sync_event({set_opts, Opts0}, _From, StateName0, 
		  #state{socket_options = Opts1, 
			 socket = Socket,
			 transport_cb = Transport,
			 user_data_buffer = Buffer} = State0) ->
    {Reply, Opts} = set_socket_opts(Transport, Socket, Opts0, Opts1, []),
    State1 = State0#state{socket_options = Opts},
    if 
	Opts#socket_options.active =:= false ->
	    {reply, Reply, StateName0, State1, get_timeout(State1)};
	Buffer =:= <<>>, Opts1#socket_options.active =:= false ->
            %% Need data, set active once
	    {Record, State2} = next_record_if_active(State1),
	    %% Note: Renogotiation may cause StateName0 =/= StateName
	    case next_state(StateName0, StateName0, Record, State2) of
		{next_state, StateName, State, Timeout} ->
		    {reply, Reply, StateName, State, Timeout};
		{stop, Reason, State} ->
		    {stop, Reason, State}
	    end;
	Buffer =:= <<>> ->
            %% Active once already set 
	    {reply, Reply, StateName0, State1, get_timeout(State1)};
	true ->
	    case read_application_data(<<>>, State1) of
		Stop = {stop,_,_} ->
		    Stop;
		{Record, State2} ->
		    %% Note: Renogotiation may cause StateName0 =/= StateName
		    case next_state(StateName0, StateName0, Record, State2) of
			{next_state, StateName, State, Timeout} ->
			    {reply, Reply, StateName, State, Timeout};
			{stop, Reason, State} ->
			    {stop, Reason, State}
		    end
	    end
    end;

handle_sync_event(renegotiate, From, connection, State) ->
    renegotiate(State#state{renegotiation = {true, From}});

handle_sync_event(renegotiate, _, StateName, State) ->
    {reply, {error, already_renegotiating}, StateName, State, get_timeout(State)};

handle_sync_event({prf, Secret, Label, Seed, WantedLength}, _, StateName,
		  #state{connection_states = ConnectionStates,
			 negotiated_version = Version} = State) ->
    ConnectionState =
	ssl_record:current_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{master_secret = MasterSecret,
			 client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Reply = try
		SecretToUse = case Secret of
				  _ when is_binary(Secret) -> Secret;
				  master_secret -> MasterSecret
			      end,
		SeedToUse = lists:reverse(
			      lists:foldl(fun(X, Acc) when is_binary(X) -> [X|Acc];
					     (client_random, Acc) -> [ClientRandom|Acc];
					     (server_random, Acc) -> [ServerRandom|Acc]
					  end, [], Seed)),
		ssl_handshake:prf(Version, SecretToUse, Label, SeedToUse, WantedLength)
	    catch
		exit:_ -> {error, badarg};
		error:Reason -> {error, Reason}
	    end,
    {reply, Reply, StateName, State, get_timeout(State)};

handle_sync_event(info, _, StateName, 
		  #state{negotiated_version = Version,
			 session = #session{cipher_suite = Suite}} = State) ->
    
    AtomVersion = tls_record:protocol_version(Version),
    {reply, {ok, {AtomVersion, ssl:suite_definition(Suite)}},
     StateName, State, get_timeout(State)};

handle_sync_event(session_info, _, StateName, 
		  #state{session = #session{session_id = Id,
					    cipher_suite = Suite}} = State) ->
    {reply, [{session_id, Id}, 
	     {cipher_suite, ssl:suite_definition(Suite)}],
     StateName, State, get_timeout(State)};

handle_sync_event(peer_certificate, _, StateName, 
		  #state{session = #session{peer_certificate = Cert}} 
		  = State) ->
    {reply, {ok, Cert}, StateName, State, get_timeout(State)}.

%%--------------------------------------------------------------------
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------

%% raw data from TCP, unpack records
handle_info({Protocol, _, Data}, StateName,
            #state{data_tag = Protocol} = State0) ->
    case next_tls_record(Data, State0) of
	{Record, State} ->
	    next_state(StateName, StateName, Record, State);
	#alert{} = Alert ->
	    handle_normal_shutdown(Alert, StateName, State0), 
	    {stop, {shutdown, own_alert}, State0}
    end;

handle_info({CloseTag, Socket}, StateName,
            #state{socket = Socket, close_tag = CloseTag,
		   negotiated_version = Version} = State) ->
    %% Note that as of TLS 1.1,
    %% failure to properly close a connection no longer requires that a
    %% session not be resumed.  This is a change from TLS 1.0 to conform
    %% with widespread implementation practice.
    case Version of
	{1, N} when N >= 1 ->
	    ok;
	_ ->
	    %% As invalidate_sessions here causes performance issues,
	    %% we will conform to the widespread implementation
	    %% practice and go aginst the spec
	    %%invalidate_session(Role, Host, Port, Session)
	    ok
    end,
    handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
    {stop, {shutdown, transport_closed}, State};

handle_info({ErrorTag, Socket, econnaborted}, StateName,  
	    #state{socket = Socket, transport_cb = Transport,
		   start_or_recv_from = StartFrom, role = Role,
		   error_tag = ErrorTag} = State)  when StateName =/= connection ->
    alert_user(Transport, Socket, StartFrom, ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), Role),
    {stop, normal, State};

handle_info({ErrorTag, Socket, Reason}, StateName, #state{socket = Socket,
							  error_tag = ErrorTag} = State)  ->
    Report = io_lib:format("SSL: Socket error: ~p ~n", [Reason]),
    error_logger:info_report(Report),
    handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
    {stop, normal, State};

handle_info({'DOWN', MonitorRef, _, _, _}, _, 
	    State = #state{user_application={MonitorRef,_Pid}}) ->
    {stop, normal, State};   

handle_info(allow_renegotiate, StateName, State) ->
    {next_state, StateName, State#state{allow_renegotiate = true}, get_timeout(State)};

handle_info({cancel_start_or_recv, StartFrom}, StateName,
	    #state{renegotiation = {false, first}} = State) when StateName =/= connection ->
    gen_fsm:reply(StartFrom, {error, timeout}),
    {stop, {shutdown, user_timeout}, State#state{timer = undefined}};

handle_info({cancel_start_or_recv, RecvFrom}, StateName, #state{start_or_recv_from = RecvFrom} = State) ->
    gen_fsm:reply(RecvFrom, {error, timeout}),
    {next_state, StateName, State#state{start_or_recv_from = undefined,
					bytes_to_read = undefined,
					timer = undefined}, get_timeout(State)};

handle_info({cancel_start_or_recv, _RecvFrom}, StateName, State) ->
    {next_state, StateName, State#state{timer = undefined}, get_timeout(State)};

handle_info(Msg, StateName, State) ->
    Report = io_lib:format("SSL: Got unexpected info: ~p ~n", [Msg]),
    error_logger:info_report(Report),
    {next_state, StateName, State, get_timeout(State)}.

%%--------------------------------------------------------------------
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_, _, #state{terminated = true}) ->
    %% Happens when user closes the connection using ssl:close/1
    %% we want to guarantee that Transport:close has been called
    %% when ssl:close/1 returns.
    ok;

terminate({shutdown, transport_closed}, StateName, #state{send_queue = SendQueue,
							  renegotiation = Renegotiate} = State) ->
    handle_unrecv_data(StateName, State),
    handle_trusted_certs_db(State),
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate);

terminate({shutdown, own_alert}, _StateName, #state{send_queue = SendQueue,
				      renegotiation = Renegotiate} = State) ->
    handle_trusted_certs_db(State),
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate);

terminate(Reason, connection, #state{negotiated_version = Version,
				      connection_states = ConnectionStates,
				      transport_cb = Transport,
				      socket = Socket, send_queue = SendQueue,
				      renegotiation = Renegotiate} = State) ->
    handle_trusted_certs_db(State),
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate),
    BinAlert = terminate_alert(Reason, Version, ConnectionStates),
    Transport:send(Socket, BinAlert),
    workaround_transport_delivery_problems(Socket, Transport);

terminate(_Reason, _StateName, #state{transport_cb = Transport,
				      socket = Socket, send_queue = SendQueue,
				      renegotiation = Renegotiate} = State) ->
    handle_trusted_certs_db(State),
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate),
    Transport:close(Socket).

%%--------------------------------------------------------------------
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = false},_} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
	{ok, Pid} = ssl_connection_sup:start_child([Role, Host, Port, Socket, 
						    Opts, User, CbInfo]), 
	{ok, SslSocket} = socket_control(Socket, Pid, CbModule),
	ok = handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end;

start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = true},_} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
	{ok, Pid} = ssl_connection_sup:start_child_dist([Role, Host, Port, Socket, 
							 Opts, User, CbInfo]), 
	{ok, SslSocket} = socket_control(Socket, Pid, CbModule),
	ok = handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

ssl_init(SslOpts, Role) ->
    
    init_manager_name(SslOpts#ssl_options.erl_dist),

    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, OwnCert} = init_certificates(SslOpts, Role),
    PrivateKey =
	init_private_key(PemCacheHandle, SslOpts#ssl_options.key, SslOpts#ssl_options.keyfile,
			 SslOpts#ssl_options.password, Role),
    DHParams = init_diffie_hellman(PemCacheHandle, SslOpts#ssl_options.dh, SslOpts#ssl_options.dhfile, Role),
    {ok, CertDbRef, CertDbHandle, FileRefHandle, CacheHandle, OwnCert, PrivateKey, DHParams}.

init_manager_name(false) ->
    put(ssl_manager, ssl_manager:manager_name(normal));
init_manager_name(true) ->
    put(ssl_manager, ssl_manager:manager_name(dist)).

init_certificates(#ssl_options{cacerts = CaCerts,
			       cacertfile = CACertFile,
			       certfile = CertFile,
			       cert = Cert}, Role) ->
    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle} =
	try 
	    Certs = case CaCerts of
			undefined ->
			    CACertFile;
			_ ->
			    {der, CaCerts}
		    end,
	    {ok, _, _, _, _, _} = ssl_manager:connection_init(Certs, Role)
	catch
	    _:Reason ->
		file_error(CACertFile, {cacertfile, Reason})
	end,
    init_certificates(Cert, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, CertFile, Role).

init_certificates(undefined, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, <<>>, _) ->
    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, undefined};

init_certificates(undefined, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, CertFile, client) ->
    try 
	%% Ignoring potential proxy-certificates see: 
	%% http://dev.globus.org/wiki/Security/ProxyFileFormat
	[OwnCert|_] = ssl_certificate:file_to_certificats(CertFile, PemCacheHandle),
	{ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, OwnCert}
    catch _Error:_Reason  ->
	    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheHandle, undefined}
    end;

init_certificates(undefined, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheRef, CertFile, server) ->
    try
	[OwnCert|_] = ssl_certificate:file_to_certificats(CertFile, PemCacheHandle),
	{ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheRef, OwnCert}
    catch
	_:Reason ->
	    file_error(CertFile, {certfile, Reason})	    
    end;
init_certificates(Cert, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheRef, _, _) ->
    {ok, CertDbRef, CertDbHandle, FileRefHandle, PemCacheHandle, CacheRef, Cert}.

init_private_key(_, undefined, <<>>, _Password, _Client) ->
    undefined;
init_private_key(DbHandle, undefined, KeyFile, Password, _) ->
    try
	{ok, List} = ssl_manager:cache_pem_file(KeyFile, DbHandle),
	[PemEntry] = [PemEntry || PemEntry = {PKey, _ , _} <- List,
				  PKey =:= 'RSAPrivateKey' orelse
				      PKey =:= 'DSAPrivateKey' orelse
				      PKey =:= 'ECPrivateKey' orelse
				      PKey =:= 'PrivateKeyInfo'
		     ],
	private_key(public_key:pem_entry_decode(PemEntry, Password))
    catch 
	_:Reason ->
	    file_error(KeyFile, {keyfile, Reason}) 
    end;

init_private_key(_,{Asn1Type, PrivateKey},_,_,_) ->
    private_key(init_private_key(Asn1Type, PrivateKey)).

init_private_key(Asn1Type, PrivateKey) ->
    public_key:der_decode(Asn1Type, PrivateKey).

private_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'rsaEncryption'},
			     privateKey = Key}) ->
    public_key:der_decode('RSAPrivateKey', iolist_to_binary(Key));

private_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-dsa'},
			     privateKey = Key}) ->
    public_key:der_decode('DSAPrivateKey', iolist_to_binary(Key));

private_key(Key) ->
    Key.

-spec(file_error(_,_) -> no_return()).
file_error(File, Throw) ->
    case Throw of
	{Opt,{badmatch, {error, {badmatch, Error}}}} ->
	    throw({options, {Opt, binary_to_list(File), Error}});
	_ ->
	    throw(Throw)
    end.

init_diffie_hellman(_,Params, _,_) when is_binary(Params)->
    public_key:der_decode('DHParameter', Params);
init_diffie_hellman(_,_,_, client) ->
    undefined;
init_diffie_hellman(_,_,undefined, _) ->
    ?DEFAULT_DIFFIE_HELLMAN_PARAMS;
init_diffie_hellman(DbHandle,_, DHParamFile, server) ->
    try
	{ok, List} = ssl_manager:cache_pem_file(DHParamFile,DbHandle),
	case [Entry || Entry = {'DHParameter', _ , _} <- List] of
	    [Entry] ->
		public_key:pem_entry_decode(Entry);
	    [] ->
		?DEFAULT_DIFFIE_HELLMAN_PARAMS
	end
    catch
	_:Reason ->
	    file_error(DHParamFile, {dhfile, Reason}) 
    end.

sync_send_all_state_event(FsmPid, Event) ->
    try gen_fsm:sync_send_all_state_event(FsmPid, Event, infinity)
    catch
 	exit:{noproc, _} ->
 	    {error, closed};
	exit:{normal, _} ->
	    {error, closed};
	exit:{{shutdown, _},_} ->
	    {error, closed}
    end.


encode_handshake(Handshake, Version, ConnectionStates0, Hist0) ->
    Frag = tls_handshake:encode_handshake(Handshake, Version),
    Hist = ssl_handshake:update_handshake_history(Hist0, Frag),
    {Encoded, ConnectionStates} =
        ssl_record:encode_handshake(Frag, Version, ConnectionStates0),
    {Encoded, ConnectionStates, Hist}.

encode_alert(#alert{} = Alert, Version, ConnectionStates) ->
    ssl_record:encode_alert_record(Alert, Version, ConnectionStates).

encode_change_cipher(#change_cipher_spec{}, Version, ConnectionStates) ->
    ssl_record:encode_change_cipher_spec(Version, ConnectionStates).


encode_packet(Data, #socket_options{packet=Packet}) ->
    case Packet of
	1 -> encode_size_packet(Data, 8,  (1 bsl 8) - 1);
	2 -> encode_size_packet(Data, 16, (1 bsl 16) - 1);
	4 -> encode_size_packet(Data, 32, (1 bsl 32) - 1);
	_ -> Data
    end.

encode_size_packet(Bin, Size, Max) ->
    Len = erlang:byte_size(Bin),
    case Len > Max of
	true  -> throw({error, {badarg, {packet_to_large, Len, Max}}});
	false -> <<Len:Size, Bin/binary>>
    end.

decode_alerts(Bin) ->
    decode_alerts(Bin, []).

decode_alerts(<<?BYTE(Level), ?BYTE(Description), Rest/binary>>, Acc) ->
    A = ?ALERT_REC(Level, Description),
    decode_alerts(Rest, [A | Acc]);
decode_alerts(<<>>, Acc) ->
    lists:reverse(Acc, []).

passive_receive(State0 = #state{user_data_buffer = Buffer}, StateName) -> 
    case Buffer of
	<<>> ->
	    {Record, State} = next_record(State0),
	    next_state(StateName, StateName, Record, State);
	_ ->
	    case read_application_data(<<>>, State0) of
		Stop = {stop, _, _} ->
		    Stop;
		{Record, State} ->
		    next_state(StateName, StateName, Record, State)
	    end
    end.

read_application_data(Data, #state{user_application = {_Mon, Pid},
				   socket = Socket,
				   transport_cb = Transport,
				   socket_options = SOpts,
				   bytes_to_read = BytesToRead,
				   start_or_recv_from = RecvFrom,
				   timer = Timer,
				   user_data_buffer = Buffer0} = State0) ->
    Buffer1 = if 
		  Buffer0 =:= <<>> -> Data;
		  Data =:= <<>> -> Buffer0;
		  true -> <<Buffer0/binary, Data/binary>>
	      end,
    case get_data(SOpts, BytesToRead, Buffer1) of
	{ok, ClientData, Buffer} -> % Send data
	    SocketOpt = deliver_app_data(Transport, Socket, SOpts, ClientData, Pid, RecvFrom),
	    cancel_timer(Timer),
	    State = State0#state{user_data_buffer = Buffer,
				 start_or_recv_from = undefined,
				 timer = undefined,
				 bytes_to_read = undefined,
				 socket_options = SocketOpt 
				},
	    if
		SocketOpt#socket_options.active =:= false; Buffer =:= <<>> -> 
		    %% Passive mode, wait for active once or recv
		    %% Active and empty, get more data
		    next_record_if_active(State);
	 	true -> %% We have more data
 		    read_application_data(<<>>, State)
	    end;
	{more, Buffer} -> % no reply, we need more data
	    next_record(State0#state{user_data_buffer = Buffer});
	{passive, Buffer} ->
	    next_record_if_active(State0#state{user_data_buffer = Buffer});
	{error,_Reason} -> %% Invalid packet in packet mode
	    deliver_packet_error(Transport, Socket, SOpts, Buffer1, Pid, RecvFrom),
	    {stop, normal, State0}
    end.

write_application_data(Data0, From, #state{socket = Socket,
					   negotiated_version = Version,
					   transport_cb = Transport,
					   connection_states = ConnectionStates0,
					   send_queue = SendQueue,
					   socket_options = SockOpts,
					   ssl_options = #ssl_options{renegotiate_at = RenegotiateAt}} = State) ->
    Data = encode_packet(Data0, SockOpts),
    
    case time_to_renegotiate(Data, ConnectionStates0, RenegotiateAt) of
	true ->
	    renegotiate(State#state{send_queue = queue:in_r({From, Data}, SendQueue),
				    renegotiation = {true, internal}});
	false ->
	    {Msgs, ConnectionStates} = ssl_record:encode_data(Data, Version, ConnectionStates0),
	    Result = Transport:send(Socket, Msgs),
	    {reply, Result,
	     connection, State#state{connection_states = ConnectionStates}, get_timeout(State)}
    end.

time_to_renegotiate(_Data, #connection_states{current_write = 
						    #connection_state{sequence_number = Num}}, RenegotiateAt) ->
    
    %% We could do test:
    %% is_time_to_renegotiate((erlang:byte_size(_Data) div ?MAX_PLAIN_TEXT_LENGTH) + 1, RenegotiateAt),
    %% but we chose to have a some what lower renegotiateAt and a much cheaper test 
    is_time_to_renegotiate(Num, RenegotiateAt).

is_time_to_renegotiate(N, M) when N < M->
    false;
is_time_to_renegotiate(_,_) ->
    true.

%% Picks ClientData 
get_data(_, _, <<>>) ->
    {more, <<>>};
%% Recv timed out save buffer data until next recv
get_data(#socket_options{active=false}, undefined, Buffer) ->
    {passive, Buffer};
get_data(#socket_options{active=Active, packet=Raw}, BytesToRead, Buffer) 
  when Raw =:= raw; Raw =:= 0 ->   %% Raw Mode
    if 
	Active =/= false orelse BytesToRead =:= 0  ->
	    %% Active true or once, or passive mode recv(0)  
	    {ok, Buffer, <<>>};
	byte_size(Buffer) >= BytesToRead ->  
	    %% Passive Mode, recv(Bytes) 
	    <<Data:BytesToRead/binary, Rest/binary>> = Buffer,
	    {ok, Data, Rest};
	true ->
	    %% Passive Mode not enough data
	    {more, Buffer}
    end;
get_data(#socket_options{packet=Type, packet_size=Size}, _, Buffer) ->
    PacketOpts = [{packet_size, Size}], 
    case decode_packet(Type, Buffer, PacketOpts) of
	{more, _} ->
	    {more, Buffer};
	Decoded ->
	    Decoded
    end.

decode_packet({http, headers}, Buffer, PacketOpts) ->
    decode_packet(httph, Buffer, PacketOpts);
decode_packet({http_bin, headers}, Buffer, PacketOpts) ->
    decode_packet(httph_bin, Buffer, PacketOpts);
decode_packet(Type, Buffer, PacketOpts) ->
    erlang:decode_packet(Type, Buffer, PacketOpts).

%% Just like with gen_tcp sockets, an ssl socket that has been configured with
%% {packet, http} (or {packet, http_bin}) will automatically switch to expect
%% HTTP headers after it sees a HTTP Request or HTTP Response line. We
%% represent the current state as follows:
%%    #socket_options.packet =:= http: Expect a HTTP Request/Response line
%%    #socket_options.packet =:= {http, headers}: Expect HTTP Headers
%% Note that if the user has explicitly configured the socket to expect
%% HTTP headers using the {packet, httph} option, we don't do any automatic
%% switching of states.
deliver_app_data(Transport, Socket, SOpts = #socket_options{active=Active, packet=Type},
		 Data, Pid, From) ->
    send_or_reply(Active, Pid, From, format_reply(Transport, Socket, SOpts, Data)),
    SO = case Data of
	     {P, _, _, _} when ((P =:= http_request) or (P =:= http_response)),
			       ((Type =:= http) or (Type =:= http_bin)) ->
	         SOpts#socket_options{packet={Type, headers}};
	     http_eoh when tuple_size(Type) =:= 2 ->
                 % End of headers - expect another Request/Response line
	         {Type1, headers} = Type,
	         SOpts#socket_options{packet=Type1};
	     _ ->
	         SOpts
	 end,
    case Active of
        once ->
            SO#socket_options{active=false};
	_ ->
	    SO
    end.

format_reply(_, _,#socket_options{active = false, mode = Mode, packet = Packet,
			     header = Header}, Data) ->
    {ok, do_format_reply(Mode, Packet, Header, Data)};
format_reply(Transport, Socket, #socket_options{active = _, mode = Mode, packet = Packet,
						header = Header}, Data) ->
    {ssl, ssl_socket:socket(self(), Transport, Socket, ?MODULE), do_format_reply(Mode, Packet, Header, Data)}.

deliver_packet_error(Transport, Socket, SO= #socket_options{active = Active}, Data, Pid, From) ->
    send_or_reply(Active, Pid, From, format_packet_error(Transport, Socket, SO, Data)).

format_packet_error(_, _,#socket_options{active = false, mode = Mode}, Data) ->
    {error, {invalid_packet, do_format_reply(Mode, raw, 0, Data)}};
format_packet_error(Transport, Socket, #socket_options{active = _, mode = Mode}, Data) ->
    {ssl_error, ssl_socket:socket(self(), Transport, Socket, ?MODULE), {invalid_packet, do_format_reply(Mode, raw, 0, Data)}}.

do_format_reply(binary, _, N, Data) when N > 0 ->  % Header mode
    header(N, Data);
do_format_reply(binary, _, _, Data) ->
    Data;
do_format_reply(list, Packet, _, Data)
  when Packet == http; Packet == {http, headers};
       Packet == http_bin; Packet == {http_bin, headers};
       Packet == httph; Packet == httph_bin ->
    Data;
do_format_reply(list, _,_, Data) ->
    binary_to_list(Data).

header(0, <<>>) ->
    <<>>;
header(_, <<>>) ->
    [];
header(0, Binary) ->
    Binary;
header(N, Binary) ->
    <<?BYTE(ByteN), NewBinary/binary>> = Binary,
    [ByteN | header(N-1, NewBinary)].

send_or_reply(false, _Pid, From, Data) when From =/= undefined ->
    gen_fsm:reply(From, Data);
%% Can happen when handling own alert or tcp error/close and there is
%% no outstanding gen_fsm sync events
send_or_reply(false, no_pid, _, _) ->
    ok;
send_or_reply(_, Pid, _From, Data) ->
    send_user(Pid, Data).



send_user(Pid, Msg) ->
    Pid ! Msg.

handle_tls_handshake(Handle, StateName,
		     #state{protocol_buffers =
				#protocol_buffers{tls_packets = [Packet]} = Buffers} = State) ->
    FsmReturn = {next_state, StateName, State#state{protocol_buffers =
							Buffers#protocol_buffers{tls_packets = []}}},
    Handle(Packet, FsmReturn);

handle_tls_handshake(Handle, StateName,
		     #state{protocol_buffers =
				#protocol_buffers{tls_packets = [Packet | Packets]} = Buffers} =
			 State0) ->
    FsmReturn = {next_state, StateName, State0#state{protocol_buffers =
							 Buffers#protocol_buffers{tls_packets =
										      Packets}}},
    case Handle(Packet, FsmReturn) of
	{next_state, NextStateName, State, _Timeout} ->
	    handle_tls_handshake(Handle, NextStateName, State);
	{stop, _,_} = Stop ->
	    Stop
    end.

next_state(Current,_, #alert{} = Alert, #state{negotiated_version = Version} = State) ->
    handle_own_alert(Alert, Version, Current, State);

next_state(_,Next, no_record, State) ->
    {next_state, Next, State, get_timeout(State)};

next_state(_,Next, #ssl_tls{type = ?ALERT, fragment = EncAlerts}, State) ->
    Alerts = decode_alerts(EncAlerts),
    handle_alerts(Alerts,  {next_state, Next, State, get_timeout(State)});

next_state(Current, Next, #ssl_tls{type = ?HANDSHAKE, fragment = Data},
	   State0 = #state{protocol_buffers =
			       #protocol_buffers{tls_handshake_buffer = Buf0} = Buffers,
			   negotiated_version = Version}) ->
    Handle = 
   	fun({#hello_request{} = Packet, _}, {next_state, connection = SName, State}) ->
   		%% This message should not be included in handshake
   		%% message hashes. Starts new handshake (renegotiation)
		Hs0 = ssl_handshake:init_handshake_history(),
		?MODULE:SName(Packet, State#state{tls_handshake_history=Hs0,
   						  renegotiation = {true, peer}});
   	   ({#hello_request{} = Packet, _}, {next_state, SName, State}) ->
   		%% This message should not be included in handshake
   		%% message hashes. Already in negotiation so it will be ignored!
   		?MODULE:SName(Packet, State);
	   ({#client_hello{} = Packet, Raw}, {next_state, connection = SName, State}) ->
		Version = Packet#client_hello.client_version,
		Hs0 = ssl_handshake:init_handshake_history(),
		Hs1 = ssl_handshake:update_handshake_history(Hs0, Raw),
		?MODULE:SName(Packet, State#state{tls_handshake_history=Hs1,
   						  renegotiation = {true, peer}});
	   ({Packet, Raw}, {next_state, SName, State = #state{tls_handshake_history=Hs0}}) ->
		Hs1 = ssl_handshake:update_handshake_history(Hs0, Raw),
		?MODULE:SName(Packet, State#state{tls_handshake_history=Hs1});
   	   (_, StopState) -> StopState
   	end,
    try
	{Packets, Buf} = tls_handshake:get_tls_handshake(Version,Data,Buf0),
	State = State0#state{protocol_buffers =
				 Buffers#protocol_buffers{tls_packets = Packets,
							  tls_handshake_buffer = Buf}},
	handle_tls_handshake(Handle, Next, State)
    catch throw:#alert{} = Alert ->
	    handle_own_alert(Alert, Version, Current, State0)
    end;

next_state(_, StateName, #ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, State0) ->
    case read_application_data(Data, State0) of
	Stop = {stop,_,_} ->
   	    Stop;
	{Record, State} ->
   	    next_state(StateName, StateName, Record, State)
    end;
next_state(Current, Next, #ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = <<1>>} = 
 	   _ChangeCipher, 
 	   #state{connection_states = ConnectionStates0} = State0) ->
    ConnectionStates1 =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read),
    {Record, State} = next_record(State0#state{connection_states = ConnectionStates1}),
    next_state(Current, Next, Record, State);
next_state(Current, Next, #ssl_tls{type = _Unknown}, State0) ->
    %% Ignore unknown type 
    {Record, State} = next_record(State0),
    next_state(Current, Next, Record, State).

next_tls_record(Data, #state{protocol_buffers = #protocol_buffers{tls_record_buffer = Buf0,
						tls_cipher_texts = CT0} = Buffers} = State0) ->
    case tls_record:get_tls_records(Data, Buf0) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{protocol_buffers =
					 Buffers#protocol_buffers{tls_record_buffer = Buf1,
								  tls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    Alert
    end.

next_record(#state{protocol_buffers = #protocol_buffers{tls_packets = [], tls_cipher_texts = []},
		   socket = Socket,
		   transport_cb = Transport} = State) ->
    ssl_socket:setopts(Transport, Socket, [{active,once}]),
    {no_record, State};
next_record(#state{protocol_buffers =
		       #protocol_buffers{tls_packets = [], tls_cipher_texts = [CT | Rest]}
		   = Buffers,
		   connection_states = ConnStates0} = State) ->
    case tls_record:decode_cipher_text(CT, ConnStates0) of
	{Plain, ConnStates} ->		      
	    {Plain, State#state{protocol_buffers =
				    Buffers#protocol_buffers{tls_cipher_texts = Rest},
				connection_states = ConnStates}};
	#alert{} = Alert ->
	    {Alert, State}
    end;
next_record(State) ->
    {no_record, State}.

next_record_if_active(State = 
		      #state{socket_options = 
			     #socket_options{active = false}}) ->    
    {no_record ,State};

next_record_if_active(State) ->
    next_record(State).

next_state_connection(StateName, #state{send_queue = Queue0,
					negotiated_version = Version,
					socket = Socket,
					transport_cb = Transport,
					connection_states = ConnectionStates0
				       } = State) ->     
    %% Send queued up data that was queued while renegotiating
    case queue:out(Queue0) of
	{{value, {From, Data}}, Queue} ->
	    {Msgs, ConnectionStates} = 
		ssl_record:encode_data(Data, Version, ConnectionStates0),
	    Result = Transport:send(Socket, Msgs),
	    gen_fsm:reply(From, Result),
	    next_state_connection(StateName,
				  State#state{connection_states = ConnectionStates,
						      send_queue = Queue});		
	{empty, Queue0} ->
	    next_state_is_connection(StateName, State)
    end.

%% In next_state_is_connection/1: clear tls_handshake,
%% premaster_secret and public_key_info (only needed during handshake)
%% to reduce memory foot print of a connection.
next_state_is_connection(_, State = 
		      #state{start_or_recv_from = RecvFrom,
			     socket_options =
			     #socket_options{active = false}}) when RecvFrom =/= undefined ->
    passive_receive(State#state{premaster_secret = undefined,
				public_key_info = undefined,
				tls_handshake_history = ssl_handshake:init_handshake_history()}, connection);

next_state_is_connection(StateName, State0) ->
    {Record, State} = next_record_if_active(State0),
    next_state(StateName, connection, Record, State#state{premaster_secret = undefined,
							  public_key_info = undefined,
							  tls_handshake_history = ssl_handshake:init_handshake_history()}).


handle_new_session(NewId, CipherSuite, Compression, #state{session = Session0} = State0) ->
    Session = Session0#session{session_id = NewId,
			       cipher_suite = CipherSuite,
			       compression_method = Compression},
    {Record, State} = next_record(State0#state{session = Session}),
    next_state(hello, certify, Record, State).

handle_resumed_session(SessId, #state{connection_states = ConnectionStates0,
				      negotiated_version = Version,
				      host = Host, port = Port,
				      session_cache = Cache,
				      session_cache_cb = CacheCb} = State0) ->
    Session = CacheCb:lookup(Cache, {{Host, Port}, SessId}),
    case ssl_handshake:master_secret(tls_record, Version, Session,
				     ConnectionStates0, client) of
	{_, ConnectionStates} ->
	    {Record, State} =
		next_record(State0#state{
			      connection_states = ConnectionStates,
			      session = Session}),
	    next_state(hello, abbreviated, Record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, hello, State0)
    end.


register_session(client, Host, Port, #session{is_resumable = new} = Session0) ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(Host, Port, Session),
    Session;
register_session(server, _, Port, #session{is_resumable = new} = Session0) ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(Port, Session),
    Session;
register_session(_, _, _, Session) ->
    Session. %% Already registered

invalidate_session(client, Host, Port, Session) ->
    ssl_manager:invalidate_session(Host, Port, Session);
invalidate_session(server, _, Port, Session) ->
    ssl_manager:invalidate_session(Port, Session).

initial_state(Role, Host, Port, Socket, {SSLOptions, SocketOptions}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag}) ->
    ConnectionStates = ssl_record:init_connection_states(Role),
    
    SessionCacheCb = case application:get_env(ssl, session_cb) of
			 {ok, Cb} when is_atom(Cb) ->
			    Cb;
			 _  ->
			     ssl_session_cache
		     end,
    
    Monitor = erlang:monitor(process, User),

    #state{socket_options = SocketOptions,
	   %% We do not want to save the password in the state so that
	   %% could be written in the clear into error logs.
	   ssl_options = SSLOptions#ssl_options{password = undefined},	   
	   session = #session{is_resumable = new},
	   transport_cb = CbModule,
	   data_tag = DataTag,
	   close_tag = CloseTag,
	   error_tag = ErrorTag,
	   role = Role,
	   host = Host,
	   port = Port,
	   socket = Socket,
	   connection_states = ConnectionStates,
	   protocol_buffers = #protocol_buffers{},
	   user_application = {Monitor, User},
	   user_data_buffer = <<>>,
	   session_cache_cb = SessionCacheCb,
	   renegotiation = {false, first},
	   start_or_recv_from = undefined,
	   send_queue = queue:new()
	  }.

get_socket_opts(_,_,[], _, Acc) ->
    {ok, Acc};
get_socket_opts(Transport, Socket, [mode | Tags], SockOpts, Acc) ->
    get_socket_opts(Transport, Socket, Tags, SockOpts, 
		    [{mode, SockOpts#socket_options.mode} | Acc]);
get_socket_opts(Transport, Socket, [packet | Tags], SockOpts, Acc) ->
    case SockOpts#socket_options.packet of
	{Type, headers} ->
	    get_socket_opts(Transport, Socket, Tags, SockOpts, [{packet, Type} | Acc]);
	Type ->
	    get_socket_opts(Transport, Socket, Tags, SockOpts, [{packet, Type} | Acc])
    end;
get_socket_opts(Transport, Socket, [header | Tags], SockOpts, Acc) ->
    get_socket_opts(Transport, Socket, Tags, SockOpts, 
		    [{header, SockOpts#socket_options.header} | Acc]);
get_socket_opts(Transport, Socket, [active | Tags], SockOpts, Acc) ->
    get_socket_opts(Transport, Socket, Tags, SockOpts, 
		    [{active, SockOpts#socket_options.active} | Acc]);
get_socket_opts(Transport, Socket, [Tag | Tags], SockOpts, Acc) ->
    try ssl_socket:getopts(Transport, Socket, [Tag]) of
	{ok, [Opt]} ->
	    get_socket_opts(Transport, Socket, Tags, SockOpts, [Opt | Acc]);
	{error, Error} ->
	    {error, {options, {socket_options, Tag, Error}}}
    catch
	%% So that inet behavior does not crash our process
	_:Error -> {error, {options, {socket_options, Tag, Error}}}
    end;
get_socket_opts(_, _,Opts, _,_) ->
    {error, {options, {socket_options, Opts, function_clause}}}.

set_socket_opts(_,_, [], SockOpts, []) ->
    {ok, SockOpts};
set_socket_opts(Transport, Socket, [], SockOpts, Other) ->
    %% Set non emulated options 
    try ssl_socket:setopts(Transport, Socket, Other) of
	ok ->
	    {ok, SockOpts};
	{error, InetError} ->
	    {{error, {options, {socket_options, Other, InetError}}}, SockOpts}
    catch
	_:Error ->
	    %% So that inet behavior does not crash our process
	    {{error, {options, {socket_options, Other, Error}}}, SockOpts}
    end;

set_socket_opts(Transport,Socket, [{mode, Mode}| Opts], SockOpts, Other) when Mode == list; Mode == binary ->
    set_socket_opts(Transport, Socket, Opts, 
		    SockOpts#socket_options{mode = Mode}, Other);
set_socket_opts(_, _, [{mode, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(Transport,Socket, [{packet, Packet}| Opts], SockOpts, Other) when Packet == raw;
									Packet == 0;
									Packet == 1;
									Packet == 2;
									Packet == 4;
									Packet == asn1;
									Packet == cdr;
									Packet == sunrm;
									Packet == fcgi;
									Packet == tpkt;
									Packet == line;
									Packet == http;
									Packet == httph;
									Packet == http_bin;
									Packet == httph_bin ->
    set_socket_opts(Transport, Socket, Opts, 
		    SockOpts#socket_options{packet = Packet}, Other);
set_socket_opts(_, _, [{packet, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(Transport, Socket, [{header, Header}| Opts], SockOpts, Other) when is_integer(Header) ->
    set_socket_opts(Transport, Socket, Opts, 
		    SockOpts#socket_options{header = Header}, Other);
set_socket_opts(_, _, [{header, _} = Opt| _], SockOpts, _) ->
    {{error,{options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(Transport, Socket, [{active, Active}| Opts], SockOpts, Other) when Active == once;
										   Active == true;
										   Active == false ->
    set_socket_opts(Transport, Socket, Opts, 
		    SockOpts#socket_options{active = Active}, Other);
set_socket_opts(_, _, [{active, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}} }, SockOpts};
set_socket_opts(Transport, Socket, [Opt | Opts], SockOpts, Other) ->
    set_socket_opts(Transport, Socket, Opts, SockOpts, [Opt | Other]).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop, _, _} = Stop) ->
    %% If it is a fatal alert immediately close 
    Stop;
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Timeout}) ->
    handle_alerts(Alerts, handle_alert(Alert, StateName, State)).

handle_alert(#alert{level = ?FATAL} = Alert, StateName,
	     #state{socket = Socket, transport_cb = Transport, ssl_options = SslOpts, start_or_recv_from = From, host = Host,
		    port = Port, session = Session, user_application = {_Mon, Pid},
		    role = Role, socket_options = Opts} = State) ->
    invalidate_session(Role, Host, Port, Session),
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    alert_user(Transport, Socket, StateName, Opts, Pid, From, Alert, Role),
    {stop, normal, State};

handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert, 
	     StateName, State) -> 
    handle_normal_shutdown(Alert, StateName, State),
    {stop, {shutdown, peer_close}, State};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{ssl_options = SslOpts, renegotiation = {true, internal}} = State) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    handle_normal_shutdown(Alert, StateName, State),
    {stop, {shutdown, peer_close}, State};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{ssl_options = SslOpts, renegotiation = {true, From}} = State0) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    gen_fsm:reply(From, {error, renegotiation_rejected}),
    {Record, State} = next_record(State0),
    next_state(StateName, connection, Record, State);

handle_alert(#alert{level = ?WARNING, description = ?USER_CANCELED} = Alert, StateName, 
	     #state{ssl_options = SslOpts} = State0) ->
    log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
    {Record, State} = next_record(State0),
    next_state(StateName, StateName, Record, State).

alert_user(Transport, Socket, connection, Opts, Pid, From, Alert, Role) ->
    alert_user(Transport,Socket, Opts#socket_options.active, Pid, From, Alert, Role);
alert_user(Transport, Socket,_, _, _, From, Alert, Role) ->
    alert_user(Transport, Socket, From, Alert, Role).

alert_user(Transport, Socket, From, Alert, Role) ->
    alert_user(Transport, Socket, false, no_pid, From, Alert, Role).

alert_user(_,_, false = Active, Pid, From,  Alert, Role) ->
    %% If there is an outstanding ssl_accept | recv
    %% From will be defined and send_or_reply will
    %% send the appropriate error message.
    ReasonCode = ssl_alert:reason_code(Alert, Role),
    send_or_reply(Active, Pid, From, {error, ReasonCode});
alert_user(Transport, Socket, Active, Pid, From, Alert, Role) ->
    case ssl_alert:reason_code(Alert, Role) of
	closed ->
	    send_or_reply(Active, Pid, From,
			  {ssl_closed, ssl_socket:socket(self(), Transport, Socket, ?MODULE)});
	ReasonCode ->
	    send_or_reply(Active, Pid, From,
			  {ssl_error, ssl_socket:socket(self(), Transport, Socket, ?MODULE), ReasonCode})
    end.

log_alert(true, Info, Alert) ->
    Txt = ssl_alert:alert_txt(Alert),
    error_logger:format("SSL: ~p: ~s\n", [Info, Txt]);
log_alert(false, _, _) ->
    ok.

handle_own_alert(Alert, Version, StateName, 
		 #state{transport_cb = Transport,
			socket = Socket,
			connection_states = ConnectionStates,
			ssl_options = SslOpts} = State) ->
    try %% Try to tell the other side
	{BinMsg, _} =
	encode_alert(Alert, Version, ConnectionStates),
	Transport:send(Socket, BinMsg),
	workaround_transport_delivery_problems(Socket, Transport)
    catch _:_ ->  %% Can crash if we are in a uninitialized state
	    ignore
    end,
    try %% Try to tell the local user
	log_alert(SslOpts#ssl_options.log_alert, StateName, Alert),
	handle_normal_shutdown(Alert,StateName, State)
    catch _:_ ->
	    ok
    end,
    {stop, {shutdown, own_alert}, State}.

handle_normal_shutdown(Alert, _, #state{socket = Socket,
					transport_cb = Transport,
					start_or_recv_from = StartFrom,
					role = Role, renegotiation = {false, first}}) ->
    alert_user(Transport, Socket, StartFrom, Alert, Role);

handle_normal_shutdown(Alert, StateName, #state{socket = Socket,
						socket_options = Opts,
						transport_cb = Transport,
						user_application = {_Mon, Pid},
						start_or_recv_from = RecvFrom, role = Role}) ->
    alert_user(Transport, Socket, StateName, Opts, Pid, RecvFrom, Alert, Role).

handle_unexpected_message(Msg, Info, #state{negotiated_version = Version} = State) ->
    Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE),
    handle_own_alert(Alert, Version, {Info, Msg}, State).

make_premaster_secret({MajVer, MinVer}, rsa) ->
    Rand = ssl:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    <<?BYTE(MajVer), ?BYTE(MinVer), Rand/binary>>;
make_premaster_secret(_, _) ->
    undefined.

ack_connection(#state{renegotiation = {true, Initiater}} = State) 
  when Initiater == internal;
       Initiater == peer ->
    State#state{renegotiation = undefined};
ack_connection(#state{renegotiation = {true, From}} = State) ->    
    gen_fsm:reply(From, ok),
    State#state{renegotiation = undefined};
ack_connection(#state{renegotiation = {false, first}, 
		      start_or_recv_from = StartFrom,
		      timer = Timer} = State) when StartFrom =/= undefined ->
    gen_fsm:reply(StartFrom, connected),
    cancel_timer(Timer),
    State#state{renegotiation = undefined, start_or_recv_from = undefined, timer = undefined};
ack_connection(State) ->
    State.

renegotiate(#state{role = client} = State) ->
    %% Handle same way as if server requested
    %% the renegotiation
    Hs0 = ssl_handshake:init_handshake_history(),
    connection(#hello_request{}, State#state{tls_handshake_history = Hs0});
renegotiate(#state{role = server,
		   socket = Socket,
		   transport_cb = Transport,
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State0) ->
    HelloRequest = ssl_handshake:hello_request(),
    Frag = tls_handshake:encode_handshake(HelloRequest, Version),
    Hs0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates} = 
	ssl_record:encode_handshake(Frag, Version, ConnectionStates0),
    Transport:send(Socket, BinMsg),
    {Record, State} = next_record(State0#state{connection_states = 
					       ConnectionStates,
					       tls_handshake_history = Hs0}),
    next_state(connection, hello, Record, State#state{allow_renegotiate = true}).

notify_senders(SendQueue) -> 
    lists:foreach(fun({From, _}) ->
 			  gen_fsm:reply(From, {error, closed})
 		  end, queue:to_list(SendQueue)).

notify_renegotiater({true, From}) when not is_atom(From)  ->
    gen_fsm:reply(From, {error, closed});
notify_renegotiater(_) ->
    ok.

terminate_alert(Reason, Version, ConnectionStates) when Reason == normal;
							Reason == user_close ->
    {BinAlert, _} = encode_alert(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
				 Version, ConnectionStates),
    BinAlert;
terminate_alert({shutdown, _}, Version, ConnectionStates) ->
    {BinAlert, _} = encode_alert(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
				 Version, ConnectionStates),
    BinAlert;

terminate_alert(_, Version, ConnectionStates) ->
    {BinAlert, _} = encode_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR),
				 Version, ConnectionStates),
    BinAlert.

workaround_transport_delivery_problems(Socket, gen_tcp = Transport) ->
    %% Standard trick to try to make sure all
    %% data sent to the tcp port is really delivered to the
    %% peer application before tcp port is closed so that the peer will
    %% get the correct TLS alert message and not only a transport close.
    ssl_socket:setopts(Transport, Socket, [{active, false}]),
    Transport:shutdown(Socket, write),
    %% Will return when other side has closed or after 30 s
    %% e.g. we do not want to hang if something goes wrong
    %% with the network but we want to maximise the odds that
    %% peer application gets all data sent on the tcp connection.
    Transport:recv(Socket, 0, 30000);
workaround_transport_delivery_problems(Socket, Transport) ->
    Transport:close(Socket).

get_timeout(#state{ssl_options=#ssl_options{hibernate_after = undefined}}) ->
    infinity;
get_timeout(#state{ssl_options=#ssl_options{hibernate_after = HibernateAfter}}) ->
    HibernateAfter.

handle_trusted_certs_db(#state{ssl_options = #ssl_options{cacertfile = <<>>}}) ->
    %% No trusted certs specified
    ok;
handle_trusted_certs_db(#state{cert_db_ref = Ref,
			       cert_db = CertDb,
			       ssl_options = #ssl_options{cacertfile = undefined}}) ->
    %% Certs provided as DER directly can not be shared
    %% with other connections and it is safe to delete them when the connection ends.
    ssl_pkix_db:remove_trusted_certs(Ref, CertDb);
handle_trusted_certs_db(#state{file_ref_db = undefined}) ->
    %% Something went wrong early (typically cacertfile does not exist) so there is nothing to handle
    ok;
handle_trusted_certs_db(#state{cert_db_ref = Ref,
			       file_ref_db = RefDb,
			       ssl_options = #ssl_options{cacertfile = File}}) ->
    case ssl_pkix_db:ref_count(Ref, RefDb, -1) of
	0 ->
	    ssl_manager:clean_cert_db(Ref, File);
	_ ->
	    ok
    end.


start_or_recv_cancel_timer(infinity, _RecvFrom) ->
    undefined;
start_or_recv_cancel_timer(Timeout, RecvFrom) ->
    erlang:send_after(Timeout, self(), {cancel_start_or_recv, RecvFrom}).

cancel_timer(undefined) ->
    ok;
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    ok.

handle_unrecv_data(StateName, #state{socket = Socket, transport_cb = Transport} = State) ->
    ssl_socket:setopts(Transport, Socket, [{active, false}]),
    case Transport:recv(Socket, 0, 0) of
	{error, closed} ->
	    ok;
	{ok, Data} ->
	    handle_close_alert(Data, StateName, State)
    end.

handle_close_alert(Data, StateName, State0) ->
    case next_tls_record(Data, State0) of
	{#ssl_tls{type = ?ALERT, fragment = EncAlerts}, State} ->
	    [Alert|_] = decode_alerts(EncAlerts),
	    handle_normal_shutdown(Alert, StateName, State);
	_ ->
	    ok
    end.
