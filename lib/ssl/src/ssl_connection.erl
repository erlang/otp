%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2015. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Common handling of a TLS/SSL/DTLS connection, see also
%% tls_connection.erl and dtls_connection.erl
%%----------------------------------------------------------------------

-module(ssl_connection).

-include("ssl_api.hrl").
-include("ssl_connection.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Setup
-export([connect/8, ssl_accept/7, handshake/2, handshake/3,
	 socket_control/4, socket_control/5]).

%% User Events 
-export([send/2, recv/3, close/2, shutdown/2,
	 new_user/2, get_opts/2, set_opts/2, session_info/1, 
	 peer_certificate/1, renegotiation/1, negotiated_protocol/1, prf/5,
	 connection_information/1
	]).

-export([handle_session/7]).

%% SSL FSM state functions 
-export([hello/3, abbreviated/3, certify/3, cipher/3, connection/3]).
%% SSL all state functions 
-export([handle_sync_event/4, handle_info/3, terminate/3, format_status/2]).


%%====================================================================
%% Internal application API
%%====================================================================	     
%%--------------------------------------------------------------------
-spec connect(tls_connection | dtls_connection,
	      host(), inet:port_number(), port(),
	      {#ssl_options{}, #socket_options{},
	       %% Tracker only needed on server side
	       undefined},
	      pid(), tuple(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Connect to an ssl server.
%%--------------------------------------------------------------------
connect(Connection, Host, Port, Socket, Options, User, CbInfo, Timeout) ->
    try Connection:start_fsm(client, Host, Port, Socket, Options, User, CbInfo,
			     Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssl_not_started}
    end.
%%--------------------------------------------------------------------
-spec ssl_accept(tls_connection | dtls_connection,
		 inet:port_number(), port(),
		 {#ssl_options{}, #socket_options{}, undefined | pid()},
		 pid(), tuple(), timeout()) ->
			{ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake. 
%%--------------------------------------------------------------------
ssl_accept(Connection, Port, Socket, Opts, User, CbInfo, Timeout) ->
    try Connection:start_fsm(server, "localhost", Port, Socket, Opts, User, 
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

%%--------------------------------------------------------------------
-spec handshake(#sslsocket{}, {#ssl_options{},#socket_options{}},
		timeout()) ->  ok | {error, reason()}.
%%
%% Description: Starts ssl handshake with some new options 
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = Pid}, SslOptions, Timeout) ->  
    case sync_send_all_state_event(Pid, {start, SslOptions, Timeout}) of
	connected ->
	    ok;
 	Error ->
	    Error
    end.

%--------------------------------------------------------------------
-spec socket_control(tls_connection | dtls_connection, port(), pid(), atom()) -> 
    {ok, #sslsocket{}} | {error, reason()}.  
%%
%% Description: Set the ssl process to own the accept socket
%%--------------------------------------------------------------------	    
socket_control(Connection, Socket, Pid, Transport) ->
    socket_control(Connection, Socket, Pid, Transport, undefined).

%--------------------------------------------------------------------
-spec socket_control(tls_connection | dtls_connection, port(), pid(), atom(), pid()| undefined) -> 
    {ok, #sslsocket{}} | {error, reason()}.  
%%--------------------------------------------------------------------	    
socket_control(Connection, Socket, Pid, Transport, ListenTracker) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    {ok, ssl_socket:socket(Pid, Transport, Socket, Connection, ListenTracker)};
	{error, Reason}	->
	    {error, Reason}
    end.

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
-spec connection_information(pid()) -> {ok, list()} | {error, reason()}.
%%
%% Description: Get the SNI hostname
%%--------------------------------------------------------------------
connection_information(Pid) when is_pid(Pid) ->
    sync_send_all_state_event(Pid, connection_information).

%%--------------------------------------------------------------------
-spec close(pid(), {close, Timeout::integer() | 
				    {NewController::pid(), Timeout::integer()}}) -> 
		   ok | {ok, port()} | {error, reason()}.  
%%
%% Description:  Close an ssl connection
%%--------------------------------------------------------------------
close(ConnectionPid, How) ->
    case sync_send_all_state_event(ConnectionPid, How) of
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
-spec negotiated_protocol(pid()) -> {ok, binary()} | {error, reason()}.
%%
%% Description:  Returns the negotiated protocol
%%--------------------------------------------------------------------
negotiated_protocol(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, negotiated_protocol).

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


handle_session(#server_hello{cipher_suite = CipherSuite,
			     compression_method = Compression}, 
	       Version, NewId, ConnectionStates, ProtoExt, Protocol0,
	       #state{session = #session{session_id = OldId},
		      negotiated_version = ReqVersion,
			  negotiated_protocol = CurrentProtocol} = State0) ->
    {KeyAlgorithm, _, _, _} =
	ssl_cipher:suite_definition(CipherSuite),
    
    PremasterSecret = make_premaster_secret(ReqVersion, KeyAlgorithm),

	{ExpectNPN, Protocol} = case Protocol0 of
		undefined -> {false, CurrentProtocol};
		_ -> {ProtoExt =:= npn, Protocol0}
	end,

    State = State0#state{key_algorithm = KeyAlgorithm,
				 negotiated_version = Version,
			 connection_states = ConnectionStates,
			 premaster_secret = PremasterSecret,
			 expecting_next_protocol_negotiation = ExpectNPN,
			 negotiated_protocol = Protocol},
    
    case ssl_session:is_new(OldId, NewId) of
	true ->
	    handle_new_session(NewId, CipherSuite, Compression,
			       State#state{connection_states = ConnectionStates});
	false ->
	    handle_resumed_session(NewId,
				   State#state{connection_states = ConnectionStates})
    end.
	
%%--------------------------------------------------------------------
-spec hello(start | #hello_request{} | #server_hello{} | term(),
	    #state{}, tls_connection | dtls_connection) ->
		   gen_fsm_state_return().
%%--------------------------------------------------------------------
hello(start, #state{role = server} = State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(hello, hello, Record, State);

hello(#hello_request{}, #state{role = client} = State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(hello, hello, Record, State);

hello({common_client_hello, Type, ServerHelloExt},
      State, Connection) ->
    do_server_hello(Type, ServerHelloExt, State, Connection);
hello(timeout, State, _) ->
    {next_state, hello, State, hibernate};

hello(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, hello, State).

%%--------------------------------------------------------------------
-spec abbreviated(#hello_request{} | #finished{} | term(),
		  #state{}, tls_connection | dtls_connection) ->
			 gen_fsm_state_return().
%%--------------------------------------------------------------------
abbreviated(#hello_request{}, State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(abbreviated, hello, Record, State);

abbreviated(#finished{verify_data = Data} = Finished,
	    #state{role = server,
		   negotiated_version = Version,
		   expecting_finished = true,
		   tls_handshake_history = Handshake,
		   session = #session{master_secret = MasterSecret},
		   connection_states = ConnectionStates0} =
		State, Connection) ->
    case ssl_handshake:verify_connection(Version, Finished, client,
					 get_current_prf(ConnectionStates0, write),
					 MasterSecret, Handshake) of
        verified ->
	    ConnectionStates =
		ssl_record:set_client_verify_data(current_both, Data, ConnectionStates0),
	    Connection:next_state_connection(abbreviated,
					     ack_connection(
					       State#state{connection_states = ConnectionStates,
							   expecting_finished = false}));
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, abbreviated, State)
    end;

abbreviated(#finished{verify_data = Data} = Finished,
	    #state{role = client, tls_handshake_history = Handshake0,
		   session = #session{master_secret = MasterSecret},
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State0, Connection) ->
    case ssl_handshake:verify_connection(Version, Finished, server,
					 get_pending_prf(ConnectionStates0, write),
					 MasterSecret, Handshake0) of
        verified ->
	    ConnectionStates1 =
		ssl_record:set_server_verify_data(current_read, Data, ConnectionStates0),
	    State =
		finalize_handshake(State0#state{connection_states = ConnectionStates1},
				   abbreviated, Connection),
	    Connection:next_state_connection(abbreviated,
					     ack_connection(State#state{expecting_finished = false}));
        #alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, abbreviated, State0)
    end;

%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
abbreviated(#next_protocol{selected_protocol = SelectedProtocol},
	    #state{role = server, expecting_next_protocol_negotiation = true} = State0,
	    Connection) ->
    {Record, State} = Connection:next_record(State0#state{negotiated_protocol = SelectedProtocol}),
    Connection:next_state(abbreviated, abbreviated, Record, State#state{expecting_next_protocol_negotiation = false});

abbreviated(timeout, State, _) ->
    {next_state, abbreviated, State, hibernate };

abbreviated(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, abbreviated, State).

%%--------------------------------------------------------------------
-spec certify(#hello_request{} | #certificate{} |  #server_key_exchange{} |
	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
	      #state{}, tls_connection | dtls_connection) ->
		     gen_fsm_state_return().
%%--------------------------------------------------------------------
certify(#hello_request{}, State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(certify, hello, Record, State);

certify(#certificate{asn1_certificates = []},
	#state{role = server, negotiated_version = Version,
	       ssl_options = #ssl_options{verify = verify_peer,
					  fail_if_no_peer_cert = true}} =
	    State, Connection) ->
    Alert =  ?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE),
    Connection:handle_own_alert(Alert, Version, certify, State);

certify(#certificate{asn1_certificates = []},
	#state{role = server,
	       ssl_options = #ssl_options{verify = verify_peer,
					  fail_if_no_peer_cert = false}} =
	State0, Connection) ->
    {Record, State} = Connection:next_record(State0#state{client_certificate_requested = false}),
    Connection:next_state(certify, certify, Record, State);

certify(#certificate{} = Cert,
        #state{negotiated_version = Version,
	       role = Role,
	       cert_db = CertDbHandle,
	       cert_db_ref = CertDbRef,
	       crl_db = CRLDbInfo,
	       ssl_options = Opts} = State, Connection) ->
    case ssl_handshake:certify(Cert, CertDbHandle, CertDbRef, 
			       Opts#ssl_options.depth,
			       Opts#ssl_options.verify,
			       Opts#ssl_options.verify_fun,
			       Opts#ssl_options.partial_chain,
			       Opts#ssl_options.crl_check,
			       CRLDbInfo,		       
			       Role) of
        {PeerCert, PublicKeyInfo} ->
	    handle_peer_cert(Role, PeerCert, PublicKeyInfo,
			     State#state{client_certificate_requested = false}, Connection);
	#alert{} = Alert ->
            Connection:handle_own_alert(Alert, Version, certify, State)
    end;

certify(#server_key_exchange{exchange_keys = Keys},
        #state{role = client, negotiated_version = Version,
	       key_algorithm = Alg,
	       public_key_info = PubKeyInfo,
	       connection_states = ConnectionStates} = State, Connection)
  when Alg == dhe_dss; Alg == dhe_rsa;
       Alg == ecdhe_rsa; Alg == ecdhe_ecdsa;
       Alg == dh_anon; Alg == ecdh_anon;
       Alg == psk; Alg == dhe_psk; Alg == rsa_psk;
       Alg == srp_dss; Alg == srp_rsa; Alg == srp_anon ->

    Params = ssl_handshake:decode_server_key(Keys, Alg, Version),
    %% Use negotiated value if TLS-1.2 otherwhise return default
    HashSign = negotiated_hashsign(Params#server_key_params.hashsign, Alg, PubKeyInfo, Version),
    case is_anonymous(Alg) of
	true ->
	    calculate_secret(Params#server_key_params.params,
			     State#state{hashsign_algorithm = HashSign}, Connection);
	false ->
	    case  ssl_handshake:verify_server_key(Params, HashSign, ConnectionStates, Version, PubKeyInfo) of
		true ->
		    calculate_secret(Params#server_key_params.params,
				     State#state{hashsign_algorithm = HashSign}, Connection);
		false ->
		    Connection:handle_own_alert(?ALERT_REC(?FATAL, ?DECRYPT_ERROR),
						Version, certify, State)
	    end
    end;

certify(#server_key_exchange{} = Msg,
        #state{role = client, key_algorithm = rsa} = State, Connection) ->
    Connection:handle_unexpected_message(Msg, certify_server_keyexchange, State);

certify(#certificate_request{hashsign_algorithms = HashSigns},
	#state{session = #session{own_certificate = Cert},
	       key_algorithm = KeyExAlg,
	       ssl_options = #ssl_options{signature_algs = SupportedHashSigns},
	       negotiated_version = Version} = State0, Connection) ->

    case ssl_handshake:select_hashsign(HashSigns, Cert, KeyExAlg, SupportedHashSigns, Version) of
	#alert {} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0);
	NegotiatedHashSign -> 
	    {Record, State} = Connection:next_record(State0#state{client_certificate_requested = true}),
	    Connection:next_state(certify, certify, Record,
				  State#state{cert_hashsign_algorithm = NegotiatedHashSign})
    end;

%% PSK and RSA_PSK might bypass the Server-Key-Exchange
certify(#server_hello_done{},
	#state{session = #session{master_secret = undefined},
	       negotiated_version = Version,
	       psk_identity = PSKIdentity,
	       ssl_options = #ssl_options{user_lookup_fun = PSKLookup},
	       premaster_secret = undefined,
	       role = client,
	       key_algorithm = Alg} = State0, Connection)
  when Alg == psk ->
    case ssl_handshake:premaster_secret({Alg, PSKIdentity}, PSKLookup) of
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0);
	PremasterSecret ->
	    State = master_secret(PremasterSecret,
				  State0#state{premaster_secret = PremasterSecret}),
	    client_certify_and_key_exchange(State, Connection)
    end;

certify(#server_hello_done{},
	#state{session = #session{master_secret = undefined},
	       ssl_options = #ssl_options{user_lookup_fun = PSKLookup},
	       negotiated_version = {Major, Minor},
	       psk_identity = PSKIdentity,
	       premaster_secret = undefined,
	       role = client,
	       key_algorithm = Alg} = State0, Connection)
  when Alg == rsa_psk ->
    Rand = ssl:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    RSAPremasterSecret = <<?BYTE(Major), ?BYTE(Minor), Rand/binary>>,
    case ssl_handshake:premaster_secret({Alg, PSKIdentity}, PSKLookup, RSAPremasterSecret) of
	#alert{} = Alert ->
	    Alert;
	PremasterSecret ->
	    State = master_secret(PremasterSecret, State0#state{premaster_secret = RSAPremasterSecret}),
	    client_certify_and_key_exchange(State, Connection)
    end;

%% Master secret was determined with help of server-key exchange msg
certify(#server_hello_done{},
	#state{session = #session{master_secret = MasterSecret} = Session,
	       connection_states = ConnectionStates0,
	       negotiated_version = Version,
	       premaster_secret = undefined,
	       role = client} = State0, Connection) ->
    case ssl_handshake:master_secret(record_cb(Connection), Version, Session,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    State = State0#state{connection_states = ConnectionStates},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

%% Master secret is calculated from premaster_secret
certify(#server_hello_done{},
	#state{session = Session0,
	       connection_states = ConnectionStates0,
	       negotiated_version = Version,
	       premaster_secret = PremasterSecret,
	       role = client} = State0, Connection) ->
    case ssl_handshake:master_secret(record_cb(Connection), Version, PremasterSecret,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State = State0#state{connection_states = ConnectionStates,
				 session = Session},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end;

certify(#client_key_exchange{} = Msg,
	#state{role = server,
	       client_certificate_requested = true,
	       ssl_options = #ssl_options{fail_if_no_peer_cert = true}} = State, Connection) ->
    %% We expect a certificate here
    Connection:handle_unexpected_message(Msg, certify_client_key_exchange, State);

certify(#client_key_exchange{exchange_keys = Keys},
	State = #state{key_algorithm = KeyAlg, negotiated_version = Version}, Connection) ->
    try
	certify_client_key_exchange(ssl_handshake:decode_client_key(Keys, KeyAlg, Version),
				    State, Connection)
    catch
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State)
    end;

certify(timeout, State, _) ->
    {next_state, certify, State, hibernate};

certify(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, certify, State).

%%--------------------------------------------------------------------
-spec cipher(#hello_request{} | #certificate_verify{} | #finished{} | term(),
	     #state{}, tls_connection | dtls_connection) ->
		    gen_fsm_state_return().
%%--------------------------------------------------------------------
cipher(#hello_request{}, State0, Connection) ->
    {Record, State} = Connection:next_record(State0),
    Connection:next_state(cipher, hello, Record, State);

cipher(#certificate_verify{signature = Signature, hashsign_algorithm = CertHashSign},
       #state{role = server,
	      key_algorithm = KexAlg,
	      public_key_info = PublicKeyInfo,
	      negotiated_version = Version,
	      session = #session{master_secret = MasterSecret},
	      tls_handshake_history = Handshake
	     } = State0, Connection) ->
    
    %% Use negotiated value if TLS-1.2 otherwhise return default
    HashSign = negotiated_hashsign(CertHashSign, KexAlg, PublicKeyInfo, Version),
    case ssl_handshake:certificate_verify(Signature, PublicKeyInfo,
					  Version, HashSign, MasterSecret, Handshake) of
	valid ->
	    {Record, State} = Connection:next_record(State0),
	    Connection:next_state(cipher, cipher, Record,
				  State#state{cert_hashsign_algorithm = HashSign});
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, cipher, State0)
    end;

%% client must send a next protocol message if we are expecting it
cipher(#finished{}, #state{role = server, expecting_next_protocol_negotiation = true,
			   negotiated_protocol = undefined, negotiated_version = Version} = State0,
       Connection) ->
    Connection:handle_own_alert(?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE), Version, cipher, State0);

cipher(#finished{verify_data = Data} = Finished,
       #state{negotiated_version = Version,
	      host = Host,
	      port = Port,
	      role = Role,
	      expecting_finished = true,
	      session = #session{master_secret = MasterSecret}
	      = Session0,
	      connection_states = ConnectionStates0,
	      tls_handshake_history = Handshake0} = State, Connection) ->
    case ssl_handshake:verify_connection(Version, Finished,
					 opposite_role(Role),
					 get_current_prf(ConnectionStates0, read),
					 MasterSecret, Handshake0) of
        verified ->
	    Session = register_session(Role, Host, Port, Session0),
	    cipher_role(Role, Data, Session, State#state{expecting_finished = false}, Connection);
        #alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, cipher, State)
    end;

%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
cipher(#next_protocol{selected_protocol = SelectedProtocol},
       #state{role = server, expecting_next_protocol_negotiation = true,
	      expecting_finished = true} = State0, Connection) ->
    {Record, State} = Connection:next_record(State0#state{negotiated_protocol = SelectedProtocol}),
    Connection:next_state(cipher, cipher, Record, State#state{expecting_next_protocol_negotiation = false});

cipher(timeout, State, _) ->
    {next_state, cipher, State, hibernate};

cipher(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, cipher, State).

%%--------------------------------------------------------------------
-spec connection(term(), #state{}, tls_connection | dtls_connection) ->
			gen_fsm_state_return().
%%--------------------------------------------------------------------
connection(timeout, State,  _) ->
    {next_state, connection, State, hibernate};

connection(Msg, State, Connection) ->
    Connection:handle_unexpected_message(Msg, connection, State).

%%--------------------------------------------------------------------
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event({application_data, Data}, From, connection,  
		  #state{protocol_cb = Connection} = State) ->
    %% We should look into having a worker process to do this to 
    %% parallize send and receive decoding and not block the receiver
    %% if sending is overloading the socket.
    try
	Connection:write_application_data(Data, From, State)
    catch throw:Error ->
	    {reply, Error, connection, State, get_timeout(State)}
    end;
handle_sync_event({application_data, Data}, From, StateName, 
		  #state{send_queue = Queue} = State) ->
    %% In renegotiation priorities handshake, send data when handshake is finished
    {next_state, StateName,
     State#state{send_queue = queue:in({From, Data}, Queue)},
     get_timeout(State)};

handle_sync_event({start, Timeout}, StartFrom, hello, #state{role = Role,
							     protocol_cb = Connection, 
							     ssl_options = SSLOpts} = State0) ->
    try 
	State = ssl_config(SSLOpts, Role, State0),
	Timer = start_or_recv_cancel_timer(Timeout, StartFrom),
	Connection:hello(start, State#state{start_or_recv_from = StartFrom,
					    timer = Timer})
    catch throw:Error ->
	    {stop, normal, {error, Error}, State0}
    end;

handle_sync_event({start, {Opts, EmOpts}, Timeout}, From, StateName, State) ->
    try 
	handle_sync_event({start, Timeout}, From, StateName, State#state{socket_options = EmOpts,
									 ssl_options = Opts})
    catch throw:Error ->
	    {stop, normal, {error, Error}, State}
    end;	

%% These two clauses below could happen if a server upgrades a socket in
%% active mode. Note that in this case we are lucky that
%% controlling_process has been evalueated before receiving handshake
%% messages from client. The server should put the socket in passive
%% mode before telling the client that it is willing to upgrade
%% and before calling ssl:ssl_accept/2. These clauses are 
%% here to make sure it is the users problem and not owers if
%% they upgrade an active socket. 
handle_sync_event({start,_}, _, connection, State) ->
    {reply, connected, connection, State, get_timeout(State)};

handle_sync_event({start, Timeout}, StartFrom, StateName,  #state{role = Role, ssl_options = SslOpts} = State0) ->
    try 
	State = ssl_config(SslOpts, Role, State0),
	Timer = start_or_recv_cancel_timer(Timeout, StartFrom),
	{next_state, StateName, State#state{start_or_recv_from = StartFrom,
					    timer = Timer}, get_timeout(State)}
    catch throw:Error ->
	    {stop, normal, {error, Error}, State0}
    end;	

handle_sync_event({close, _} = Close, _, StateName, #state{protocol_cb = Connection} = State) ->
    %% Run terminate before returning so that the reuseaddr
    %% inet-option and possible downgrade will work as intended.
    Result = Connection:terminate(Close, StateName, State),
    {stop, normal, Result, State#state{terminated = true}};

handle_sync_event({shutdown, How0}, _, StateName,
		  #state{transport_cb = Transport,
			 negotiated_version = Version,
			 connection_states = ConnectionStates,
			 socket = Socket} = State) ->
    case How0 of
	How when How == write; How == both ->	    
	    Alert = ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
	    {BinMsg, _} =
		ssl_alert:encode(Alert, Version, ConnectionStates),
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
handle_sync_event({recv, _N, _Timeout}, _RecvFrom, StateName,  
		  #state{socket_options = #socket_options{active = Active}} = State) when Active =/= false ->
    {reply, {error, einval}, StateName, State, get_timeout(State)};
handle_sync_event({recv, N, Timeout}, RecvFrom, connection = StateName,  
		  #state{protocol_cb = Connection} = State0) ->
    Timer = start_or_recv_cancel_timer(Timeout, RecvFrom),
    Connection:passive_receive(State0#state{bytes_to_read = N,
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
handle_sync_event(negotiated_protocol, _From, StateName, #state{negotiated_protocol = undefined} = State) ->
    {reply, {error, protocol_not_negotiated}, StateName, State, get_timeout(State)};
handle_sync_event(negotiated_protocol, _From, StateName, #state{negotiated_protocol = SelectedProtocol} = State) ->
    {reply, {ok, SelectedProtocol}, StateName, State, get_timeout(State)};
handle_sync_event({set_opts, Opts0}, _From, StateName0, 
		  #state{socket_options = Opts1, 
			 protocol_cb = Connection,
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
	    {Record, State2} = Connection:next_record_if_active(State1),
	    %% Note: Renogotiation may cause StateName0 =/= StateName
	    case Connection:next_state(StateName0, StateName0, Record, State2) of
		{next_state, StateName, State, Timeout} ->
		    {reply, Reply, StateName, State, Timeout};
		{stop, Reason, State} ->
		    {stop, Reason, State}
	    end;
	Buffer =:= <<>> ->
            %% Active once already set 
	    {reply, Reply, StateName0, State1, get_timeout(State1)};
	true ->
	    case Connection:read_application_data(<<>>, State1) of
		Stop = {stop,_,_} ->
		    Stop;
		{Record, State2} ->
		    %% Note: Renogotiation may cause StateName0 =/= StateName
		    case Connection:next_state(StateName0, StateName0, Record, State2) of
			{next_state, StateName, State, Timeout} ->
			    {reply, Reply, StateName, State, Timeout};
			{stop, Reason, State} ->
			    {stop, Reason, State}
		    end
	    end
    end;
handle_sync_event(renegotiate, From, connection,  #state{protocol_cb = Connection} = State) ->
    Connection:renegotiate(State#state{renegotiation = {true, From}});
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
			 server_random = ServerRandom,
			 prf_algorithm = PRFAlgorithm} = SecParams,
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
		ssl_handshake:prf(Version, PRFAlgorithm, SecretToUse, Label, SeedToUse, WantedLength)
	    catch
		exit:_ -> {error, badarg};
		error:Reason -> {error, Reason}
	    end,
    {reply, Reply, StateName, State, get_timeout(State)};
handle_sync_event(session_info, _, StateName, 
		  #state{session = #session{session_id = Id,
					    cipher_suite = Suite}} = State) ->
    {reply, [{session_id, Id}, 
	     {cipher_suite, ssl_cipher:erl_suite_definition(Suite)}],
     StateName, State, get_timeout(State)};
handle_sync_event(peer_certificate, _, StateName, 
		  #state{session = #session{peer_certificate = Cert}} 
		  = State) ->
    {reply, {ok, Cert}, StateName, State, get_timeout(State)};
handle_sync_event(connection_information, _, StateName, State) ->
    Info = connection_info(State),
    {reply, {ok, Info}, StateName, State, get_timeout(State)}.

connection_info(#state{sni_hostname = SNIHostname, 
		       session = #session{cipher_suite = CipherSuite}, 
		       negotiated_version = Version, ssl_options = Opts}) ->
    [{protocol, tls_record:protocol_version(Version)}, 
     {cipher_suite, ssl_cipher:erl_suite_definition(CipherSuite)}, 
     {sni_hostname, SNIHostname}] ++ ssl_options_list(Opts).

handle_info({ErrorTag, Socket, econnaborted}, StateName,  
	    #state{socket = Socket, transport_cb = Transport,
		   start_or_recv_from = StartFrom, role = Role,
		   protocol_cb = Connection,
		   error_tag = ErrorTag,
		   tracker = Tracker} = State)  when StateName =/= connection ->
    Connection:alert_user(Transport, Tracker,Socket, StartFrom, ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), Role),
    {stop, normal, State};

handle_info({ErrorTag, Socket, Reason}, StateName, #state{socket = Socket,
							  protocol_cb = Connection,
							  error_tag = ErrorTag} = State)  ->
    Report = io_lib:format("SSL: Socket error: ~p ~n", [Reason]),
    error_logger:info_report(Report),
    Connection:handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
    {stop, normal, State};

handle_info({'DOWN', MonitorRef, _, _, _}, _, 
	    State = #state{user_application={MonitorRef,_Pid}}) ->
    {stop, normal, State};   

%%% So that terminate will be run when supervisor issues shutdown
handle_info({'EXIT', _Sup, shutdown}, _StateName, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', Socket, normal}, _StateName, #state{socket = Socket} = State) ->
    %% Handle as transport close"
    {stop, {shutdown, transport_closed}, State};

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

handle_info(Msg, StateName, #state{socket = Socket, error_tag = Tag} = State) ->
    Report = io_lib:format("SSL: Got unexpected info: ~p ~n", [{Msg, Tag, Socket}]),
    error_logger:info_report(Report),
    {next_state, StateName, State, get_timeout(State)}.


terminate(_, _, #state{terminated = true}) ->
    %% Happens when user closes the connection using ssl:close/1
    %% we want to guarantee that Transport:close has been called
    %% when ssl:close/1 returns.
    ok;
terminate({shutdown, transport_closed} = Reason, 
	  _StateName, #state{send_queue = SendQueue, protocol_cb = Connection,
			     socket = Socket, transport_cb = Transport,
			     renegotiation = Renegotiate} = State) ->
    handle_trusted_certs_db(State),
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate),
    Connection:close(Reason, Socket, Transport, undefined, undefined);
terminate({shutdown, own_alert}, _StateName, #state{send_queue = SendQueue, protocol_cb = Connection,
						    socket = Socket, transport_cb = Transport,
						    renegotiation = Renegotiate} = State) ->
    handle_trusted_certs_db(State),
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate),
    case application:get_env(ssl, alert_timeout) of
	{ok, Timeout} when is_integer(Timeout) ->
	    Connection:close({timeout, Timeout}, Socket, Transport, undefined, undefined);
	_ ->
	    Connection:close({timeout, ?DEFAULT_TIMEOUT}, Socket, Transport, undefined, undefined)
    end;
terminate(Reason, connection, #state{negotiated_version = Version,
				     protocol_cb = Connection,
				     connection_states = ConnectionStates0, 
				     ssl_options = #ssl_options{padding_check = Check},
				     transport_cb = Transport, socket = Socket, 
				     send_queue = SendQueue, renegotiation = Renegotiate} = State) ->
    handle_trusted_certs_db(State),
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate),
    {BinAlert, ConnectionStates} = terminate_alert(Reason, Version, ConnectionStates0),
    Transport:send(Socket, BinAlert),
    Connection:close(Reason, Socket, Transport, ConnectionStates, Check);

terminate(Reason, _StateName, #state{transport_cb = Transport, protocol_cb = Connection,
				      socket = Socket, send_queue = SendQueue,
				      renegotiation = Renegotiate} = State) ->
    handle_trusted_certs_db(State),
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate),
    Connection:close(Reason, Socket, Transport, undefined, undefined).

format_status(normal, [_, State]) ->
    [{data, [{"StateData", State}]}];  
format_status(terminate, [_, State]) ->
    SslOptions = (State#state.ssl_options),
    NewOptions = SslOptions#ssl_options{password = ?SECRET_PRINTOUT,
					cert = ?SECRET_PRINTOUT,
					cacerts = ?SECRET_PRINTOUT,
					key = ?SECRET_PRINTOUT,			      
					dh = ?SECRET_PRINTOUT,
					psk_identity = ?SECRET_PRINTOUT,
					srp_identity = ?SECRET_PRINTOUT},
    [{data, [{"StateData", State#state{connection_states = ?SECRET_PRINTOUT,
				       protocol_buffers =  ?SECRET_PRINTOUT,
				       user_data_buffer = ?SECRET_PRINTOUT,
				       tls_handshake_history =  ?SECRET_PRINTOUT,
				       session =  ?SECRET_PRINTOUT,
				       private_key =  ?SECRET_PRINTOUT,
				       diffie_hellman_params = ?SECRET_PRINTOUT,
				       diffie_hellman_keys =  ?SECRET_PRINTOUT,
				       srp_params = ?SECRET_PRINTOUT,
				       srp_keys =  ?SECRET_PRINTOUT,
				       premaster_secret =  ?SECRET_PRINTOUT,
				       ssl_options = NewOptions
				      }}]}].
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
ssl_config(Opts, Role, State) ->
    {ok, Ref, CertDbHandle, FileRefHandle, CacheHandle, CRLDbInfo, OwnCert, Key, DHParams} = 
	ssl_config:init(Opts, Role), 
    Handshake = ssl_handshake:init_handshake_history(),
    TimeStamp = erlang:monotonic_time(),
    Session = State#state.session,
    State#state{tls_handshake_history = Handshake,
		session = Session#session{own_certificate = OwnCert,
					  time_stamp = TimeStamp},
		file_ref_db = FileRefHandle,
		cert_db_ref = Ref,
		cert_db = CertDbHandle,
		crl_db = CRLDbInfo,
		session_cache = CacheHandle,
		private_key = Key,
		diffie_hellman_params = DHParams,
		ssl_options = Opts}.

do_server_hello(Type, #hello_extensions{next_protocol_negotiation = NextProtocols} =
		    ServerHelloExt,
		#state{negotiated_version = Version,
		       session = #session{session_id = SessId},
		       connection_states = ConnectionStates0}
		= State0, Connection) when is_atom(Type) ->

    ServerHello =
	ssl_handshake:server_hello(SessId, Version, ConnectionStates0, ServerHelloExt),
    State = server_hello(ServerHello,
			 State0#state{expecting_next_protocol_negotiation =
					  NextProtocols =/= undefined}, Connection),
    case Type of
	new ->
	    new_server_hello(ServerHello, State, Connection);
	resumed ->
	    resumed_server_hello(State, Connection)
    end.

new_server_hello(#server_hello{cipher_suite = CipherSuite,
			      compression_method = Compression,
			      session_id = SessionId},
		#state{session = Session0,
		       negotiated_version = Version} = State0, Connection) ->
    try server_certify_and_key_exchange(State0, Connection) of
        #state{} = State1 ->
            State2 = server_hello_done(State1, Connection),
	    Session =
		Session0#session{session_id = SessionId,
				 cipher_suite = CipherSuite,
				 compression_method = Compression},
	    {Record, State} = Connection:next_record(State2#state{session = Session}),
	    Connection:next_state(hello, certify, Record, State)
    catch
        #alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, hello, State0)
    end.

resumed_server_hello(#state{session = Session,
			    connection_states = ConnectionStates0,
			    negotiated_version = Version} = State0, Connection) ->

    case ssl_handshake:master_secret(record_cb(Connection), Version, Session,
				     ConnectionStates0, server) of
	{_, ConnectionStates1} ->
	    State1 = State0#state{connection_states = ConnectionStates1,
				  session = Session},
	    State2 =
		finalize_handshake(State1, abbreviated, Connection),
	    {Record, State} = Connection:next_record(State2),
	    Connection:next_state(hello, abbreviated, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, hello, State0)
    end.

server_hello(ServerHello, State0, Connection) ->
    CipherSuite = ServerHello#server_hello.cipher_suite,
    {KeyAlgorithm, _, _, _} = ssl_cipher:suite_definition(CipherSuite),
    State = Connection:send_handshake(ServerHello, State0),
    State#state{key_algorithm = KeyAlgorithm}.

server_hello_done(State, Connection) ->
    HelloDone = ssl_handshake:server_hello_done(),
    Connection:send_handshake(HelloDone, State).

handle_peer_cert(Role, PeerCert, PublicKeyInfo,
		 #state{session = #session{cipher_suite = CipherSuite} = Session} = State0,
		 Connection) ->
    State1 = State0#state{session =
			 Session#session{peer_certificate = PeerCert},
			 public_key_info = PublicKeyInfo},
    {KeyAlg,_,_,_} = ssl_cipher:suite_definition(CipherSuite),
    State2 = handle_peer_cert_key(Role, PeerCert, PublicKeyInfo, KeyAlg, State1),

    {Record, State} = Connection:next_record(State2),
    Connection:next_state(certify, certify, Record, State).

handle_peer_cert_key(client, _,
		     {?'id-ecPublicKey',  #'ECPoint'{point = _ECPoint} = PublicKey,
		      PublicKeyParams},
		     KeyAlg, State)  when KeyAlg == ecdh_rsa;
					  KeyAlg == ecdh_ecdsa ->
    ECDHKey = public_key:generate_key(PublicKeyParams),
    PremasterSecret = ssl_handshake:premaster_secret(PublicKey, ECDHKey),
    master_secret(PremasterSecret, State#state{diffie_hellman_keys = ECDHKey});

%% We do currently not support cipher suites that use fixed DH.
%% If we want to implement that the following clause can be used
%% to extract DH parameters form cert.
%% handle_peer_cert_key(client, _PeerCert, {?dhpublicnumber, PublicKey, PublicKeyParams},
%%                      {_,SignAlg},
%% 		        #state{diffie_hellman_keys = {_, MyPrivatKey}} = State) when
%%                                                                           SignAlg == dh_rsa;
%% 									     SignAlg == dh_dss ->
%%     dh_master_secret(PublicKeyParams, PublicKey, MyPrivatKey, State);
handle_peer_cert_key(_, _, _, _, State) ->
    State.

certify_client(#state{client_certificate_requested = true, role = client,
		      cert_db = CertDbHandle,
                      cert_db_ref = CertDbRef,
		      session = #session{own_certificate = OwnCert}}
	       = State, Connection) ->
    Certificate = ssl_handshake:certificate(OwnCert, CertDbHandle, CertDbRef, client),
    Connection:send_handshake(Certificate, State);

certify_client(#state{client_certificate_requested = false} = State, _) ->
    State.

verify_client_cert(#state{client_certificate_requested = true, role = client,
			  negotiated_version = Version,
			  private_key = PrivateKey,
			  session = #session{master_secret = MasterSecret,
					     own_certificate = OwnCert},
			  cert_hashsign_algorithm = HashSign,
			  tls_handshake_history = Handshake0} = State, Connection) ->

    case ssl_handshake:client_certificate_verify(OwnCert, MasterSecret,
						 Version, HashSign, PrivateKey, Handshake0) of
        #certificate_verify{} = Verified ->
           Connection:send_handshake(Verified, State);
	ignore ->
	    State;
	#alert{} = Alert ->
	    throw(Alert)
    end;
verify_client_cert(#state{client_certificate_requested = false} = State, _) ->
    State.

client_certify_and_key_exchange(#state{negotiated_version = Version} =
				State0, Connection) ->
    try do_client_certify_and_key_exchange(State0, Connection) of
        State1 = #state{} ->
	    State2 = finalize_handshake(State1, certify, Connection),
            State3 = State2#state{
		       %% Reinitialize
		       client_certificate_requested = false},
	    {Record, State} = Connection:next_record(State3),
	    Connection:next_state(certify, cipher, Record, State)
    catch
        throw:#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end.

do_client_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_client(State0, Connection),
    State2 = key_exchange(State1, Connection),
    verify_client_cert(State2, Connection).

server_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_server(State0, Connection),
    State2 = key_exchange(State1, Connection),
    request_client_cert(State2, Connection).

certify_client_key_exchange(#encrypted_premaster_secret{premaster_secret= EncPMS},
			    #state{private_key = Key} = State, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(EncPMS, Key),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);

certify_client_key_exchange(#client_diffie_hellman_public{dh_public = ClientPublicDhKey},
			    #state{diffie_hellman_params = #'DHParameter'{} = Params,
				   diffie_hellman_keys = {_, ServerDhPrivateKey}} = State,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientPublicDhKey, ServerDhPrivateKey, Params),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);

certify_client_key_exchange(#client_ec_diffie_hellman_public{dh_public = ClientPublicEcDhPoint},
			    #state{diffie_hellman_keys = ECDHKey} = State, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(#'ECPoint'{point = ClientPublicEcDhPoint}, ECDHKey),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);

certify_client_key_exchange(#client_psk_identity{} = ClientKey,
			    #state{ssl_options = #ssl_options{user_lookup_fun = PSKLookup}} = State0, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);

certify_client_key_exchange(#client_dhe_psk_identity{} = ClientKey,
			    #state{diffie_hellman_params = #'DHParameter'{} = Params,
				   diffie_hellman_keys = {_, ServerDhPrivateKey},
				   ssl_options = #ssl_options{user_lookup_fun = PSKLookup}} = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, ServerDhPrivateKey, Params, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);
certify_client_key_exchange(#client_rsa_psk_identity{} = ClientKey,
			    #state{private_key = Key,
				   ssl_options = #ssl_options{user_lookup_fun = PSKLookup}} = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, Key, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);

certify_client_key_exchange(#client_srp_public{} = ClientKey,
			    #state{srp_params = Params,
				   srp_keys = Key
				  } = State0, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, Key, Params),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher).

certify_server(#state{key_algorithm = Algo} = State, _)
  when Algo == dh_anon; Algo == ecdh_anon; Algo == psk; Algo == dhe_psk; Algo == srp_anon  ->
    State;

certify_server(#state{cert_db = CertDbHandle,
		      cert_db_ref = CertDbRef,
		      session = #session{own_certificate = OwnCert}} = State, Connection) ->
    case ssl_handshake:certificate(OwnCert, CertDbHandle, CertDbRef, server) of
	Cert = #certificate{} ->
	    Connection:send_handshake(Cert, State);
	Alert = #alert{} ->
	    throw(Alert)
    end.

key_exchange(#state{role = server, key_algorithm = rsa} = State,_) ->
    State;
key_exchange(#state{role = server, key_algorithm = Algo,
		    hashsign_algorithm = HashSignAlgo,
		    diffie_hellman_params = #'DHParameter'{} = Params,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == dhe_dss;
       Algo == dhe_rsa;
       Algo == dh_anon ->
    DHKeys = public_key:generate_key(Params),
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, Version, {dh, DHKeys, Params,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    State = Connection:send_handshake(Msg, State0),
    State#state{diffie_hellman_keys = DHKeys};

key_exchange(#state{role = server, private_key = Key, key_algorithm = Algo} = State, _)
  when Algo == ecdh_ecdsa; Algo == ecdh_rsa ->
    State#state{diffie_hellman_keys = Key};
key_exchange(#state{role = server, key_algorithm = Algo,
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == ecdhe_ecdsa; Algo == ecdhe_rsa;
       Algo == ecdh_anon ->

    ECDHKeys = public_key:generate_key(select_curve(State0)),
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, Version, {ecdh, ECDHKeys,
							HashSignAlgo, ClientRandom,
							ServerRandom,
							PrivateKey}),
    State = Connection:send_handshake(Msg, State0),
    State#state{diffie_hellman_keys = ECDHKeys};

key_exchange(#state{role = server, key_algorithm = psk,
		    ssl_options = #ssl_options{psk_identity = undefined}} = State, _) ->
    State;
key_exchange(#state{role = server, key_algorithm = psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, Version, {psk, PskIdentityHint,
						       HashSignAlgo, ClientRandom,
						       ServerRandom,
						       PrivateKey}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = server, key_algorithm = dhe_psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    diffie_hellman_params = #'DHParameter'{} = Params,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    DHKeys = public_key:generate_key(Params),
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, Version, {dhe_psk, PskIdentityHint, DHKeys, Params,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    State = Connection:send_handshake(Msg, State0),
    State#state{diffie_hellman_keys = DHKeys};

key_exchange(#state{role = server, key_algorithm = rsa_psk,
		    ssl_options = #ssl_options{psk_identity = undefined}} = State, _) ->
    State;
key_exchange(#state{role = server, key_algorithm = rsa_psk,
		    ssl_options = #ssl_options{psk_identity = PskIdentityHint},
		    hashsign_algorithm = HashSignAlgo,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection) ->
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, Version, {psk, PskIdentityHint,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = server, key_algorithm = Algo,
		    ssl_options = #ssl_options{user_lookup_fun = LookupFun},
		    hashsign_algorithm = HashSignAlgo,
		    session = #session{srp_username = Username},
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version
		   } = State0, Connection)
  when Algo == srp_dss;
       Algo == srp_rsa;
       Algo == srp_anon ->
    SrpParams = handle_srp_identity(Username, LookupFun),
    Keys = case generate_srp_server_keys(SrpParams, 0) of
	       Alert = #alert{} ->
		   throw(Alert);
	       Keys0 = {_,_} ->
		   Keys0
	   end,
    ConnectionState =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, Version, {srp, Keys, SrpParams,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    State = Connection:send_handshake(Msg, State0),
    State#state{srp_params = SrpParams,
		srp_keys = Keys};

key_exchange(#state{role = client,
		    key_algorithm = rsa,
		    public_key_info = PublicKeyInfo,
		    negotiated_version = Version,
		    premaster_secret = PremasterSecret} = State0, Connection) ->
    Msg = rsa_key_exchange(Version, PremasterSecret, PublicKeyInfo),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    diffie_hellman_keys = {DhPubKey, _}
		   } = State0, Connection)
  when Algorithm == dhe_dss;
       Algorithm == dhe_rsa;
       Algorithm == dh_anon ->
    Msg =  ssl_handshake:key_exchange(client, Version, {dh, DhPubKey}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    diffie_hellman_keys = Keys} = State0, Connection)
  when Algorithm == ecdhe_ecdsa; Algorithm == ecdhe_rsa;
       Algorithm == ecdh_ecdsa; Algorithm == ecdh_rsa;
       Algorithm == ecdh_anon ->
    Msg = ssl_handshake:key_exchange(client, Version, {ecdh, Keys}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = psk,
		    negotiated_version = Version} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, Version, {psk, SslOpts#ssl_options.psk_identity}),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = dhe_psk,
		    negotiated_version = Version,
		    diffie_hellman_keys = {DhPubKey, _}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, Version,
				      {dhe_psk, SslOpts#ssl_options.psk_identity, DhPubKey}),
    Connection:send_handshake(Msg, State0);
key_exchange(#state{role = client,
		    ssl_options = SslOpts,
		    key_algorithm = rsa_psk,
		    public_key_info = PublicKeyInfo,
		    negotiated_version = Version,
		    premaster_secret = PremasterSecret}
	     = State0, Connection) ->
    Msg = rsa_psk_key_exchange(Version, SslOpts#ssl_options.psk_identity,
			       PremasterSecret, PublicKeyInfo),
    Connection:send_handshake(Msg, State0);

key_exchange(#state{role = client,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    srp_keys = {ClientPubKey, _}}
	     = State0, Connection)
  when Algorithm == srp_dss;
       Algorithm == srp_rsa;
       Algorithm == srp_anon ->
    Msg =  ssl_handshake:key_exchange(client, Version, {srp, ClientPubKey}),
    Connection:send_handshake(Msg, State0).

rsa_key_exchange(Version, PremasterSecret, PublicKeyInfo = {Algorithm, _, _})
  when Algorithm == ?rsaEncryption;
       Algorithm == ?md2WithRSAEncryption;
       Algorithm == ?md5WithRSAEncryption;
       Algorithm == ?sha1WithRSAEncryption;
       Algorithm == ?sha224WithRSAEncryption;
       Algorithm == ?sha256WithRSAEncryption;
       Algorithm == ?sha384WithRSAEncryption;
       Algorithm == ?sha512WithRSAEncryption
       ->
    ssl_handshake:key_exchange(client, Version,
			       {premaster_secret, PremasterSecret,
				PublicKeyInfo});
rsa_key_exchange(_, _, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE)).

rsa_psk_key_exchange(Version, PskIdentity, PremasterSecret, PublicKeyInfo = {Algorithm, _, _})
  when Algorithm == ?rsaEncryption;
       Algorithm == ?md2WithRSAEncryption;
       Algorithm == ?md5WithRSAEncryption;
       Algorithm == ?sha1WithRSAEncryption;
       Algorithm == ?sha224WithRSAEncryption;
       Algorithm == ?sha256WithRSAEncryption;
       Algorithm == ?sha384WithRSAEncryption;
       Algorithm == ?sha512WithRSAEncryption
       ->
    ssl_handshake:key_exchange(client, Version,
			       {psk_premaster_secret, PskIdentity, PremasterSecret,
				PublicKeyInfo});
rsa_psk_key_exchange(_, _, _, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE)).

request_client_cert(#state{ssl_options = #ssl_options{verify = verify_peer, 
						      signature_algs = SupportedHashSigns},
			   connection_states = ConnectionStates0,
			   cert_db = CertDbHandle,
			   cert_db_ref = CertDbRef,
			   negotiated_version = Version} = State0, Connection) ->
    #connection_state{security_parameters =
			  #security_parameters{cipher_suite = CipherSuite}} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    HashSigns = ssl_handshake:available_signature_algs(SupportedHashSigns, Version, [Version]),
    Msg = ssl_handshake:certificate_request(CipherSuite, CertDbHandle, CertDbRef, 
					    HashSigns, Version),
    State = Connection:send_handshake(Msg, State0),
    State#state{client_certificate_requested = true};

request_client_cert(#state{ssl_options = #ssl_options{verify = verify_none}} =
		    State, _) ->
    State.

calculate_master_secret(PremasterSecret, #state{negotiated_version = Version,
						       connection_states = ConnectionStates0,
						       session = Session0} = State0, Connection,
			Current, Next) ->
    case ssl_handshake:master_secret(record_cb(Connection), Version, PremasterSecret,
				     ConnectionStates0, server) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State1 = State0#state{connection_states = ConnectionStates,
				  session = Session},
	    {Record, State} = Connection:next_record(State1),
	    Connection:next_state(Current, Next, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, certify, State0)
    end.

finalize_handshake(State0, StateName, Connection) ->
    #state{connection_states = ConnectionStates0} =
	State1 = cipher_protocol(State0, Connection),

    ConnectionStates =
        ssl_record:activate_pending_connection_state(ConnectionStates0,
                                                     write),

    State2 = State1#state{connection_states = ConnectionStates},
    State = next_protocol(State2, Connection),
    finished(State, StateName, Connection).

next_protocol(#state{role = server} = State, _) ->
    State;
next_protocol(#state{negotiated_protocol = undefined} = State, _) ->
    State;
next_protocol(#state{expecting_next_protocol_negotiation = false} = State, _) ->
    State;
next_protocol(#state{negotiated_protocol = NextProtocol} = State0, Connection) ->
    NextProtocolMessage = ssl_handshake:next_protocol(NextProtocol),
    Connection:send_handshake(NextProtocolMessage, State0).

cipher_protocol(State, Connection) ->
    Connection:send_change_cipher(#change_cipher_spec{}, State).

finished(#state{role = Role, negotiated_version = Version,
		session = Session,
                connection_states = ConnectionStates0,
                tls_handshake_history = Handshake0} = State0, StateName, Connection) ->
    MasterSecret = Session#session.master_secret,
    Finished = ssl_handshake:finished(Version, Role,
				       get_current_prf(ConnectionStates0, write),
				       MasterSecret, Handshake0),
    ConnectionStates = save_verify_data(Role, Finished, ConnectionStates0, StateName),
    Connection:send_handshake(Finished, State0#state{connection_states =
								 ConnectionStates}).

save_verify_data(client, #finished{verify_data = Data}, ConnectionStates, certify) ->
    ssl_record:set_client_verify_data(current_write, Data, ConnectionStates);
save_verify_data(server, #finished{verify_data = Data}, ConnectionStates, cipher) ->
    ssl_record:set_server_verify_data(current_both, Data, ConnectionStates);
save_verify_data(client, #finished{verify_data = Data}, ConnectionStates, abbreviated) ->
    ssl_record:set_client_verify_data(current_both, Data, ConnectionStates);
save_verify_data(server, #finished{verify_data = Data}, ConnectionStates, abbreviated) ->
    ssl_record:set_server_verify_data(current_write, Data, ConnectionStates).

calculate_secret(#server_dh_params{dh_p = Prime, dh_g = Base, dh_y = ServerPublicDhKey} = Params,
		     State, Connection) ->
    Keys = {_, PrivateDhKey} = crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret =
	ssl_handshake:premaster_secret(ServerPublicDhKey, PrivateDhKey, Params),
    calculate_master_secret(PremasterSecret,
			    State#state{diffie_hellman_keys = Keys}, Connection, certify, certify);

calculate_secret(#server_ecdh_params{curve = ECCurve, public = ECServerPubKey},
		     State, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),
    PremasterSecret = ssl_handshake:premaster_secret(#'ECPoint'{point = ECServerPubKey}, ECDHKeys),
    calculate_master_secret(PremasterSecret,
			    State#state{diffie_hellman_keys = ECDHKeys}, Connection, certify, certify);

calculate_secret(#server_psk_params{
			hint = IdentityHint},
		 State0, Connection) ->
    %% store for later use
    {Record, State} = Connection:next_record(State0#state{psk_identity = IdentityHint}),
    Connection:next_state(certify, certify, Record, State);

calculate_secret(#server_dhe_psk_params{
		    dh_params = #server_dh_params{dh_p = Prime, dh_g = Base}} = ServerKey,
		    #state{ssl_options = #ssl_options{user_lookup_fun = PSKLookup}} = State, Connection) ->
    Keys = {_, PrivateDhKey} =
	crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, PrivateDhKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State#state{diffie_hellman_keys = Keys},
			    Connection, certify, certify);

calculate_secret(#server_srp_params{srp_n = Prime, srp_g = Generator} = ServerKey,
		 #state{ssl_options = #ssl_options{srp_identity = SRPId}} = State, Connection) ->
    Keys = generate_srp_client_keys(Generator, Prime, 0),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, Keys, SRPId),
    calculate_master_secret(PremasterSecret, State#state{srp_keys = Keys}, Connection, certify, certify).

master_secret(#alert{} = Alert, _) ->
    Alert;
master_secret(PremasterSecret, #state{session = Session,
				      negotiated_version = Version, role = Role,
				      connection_states = ConnectionStates0} = State) ->
    case ssl_handshake:master_secret(tls_record, Version, PremasterSecret,
				     ConnectionStates0, Role) of
	{MasterSecret, ConnectionStates} ->
	    State#state{
	      session =
		  Session#session{master_secret = MasterSecret},
	      connection_states = ConnectionStates};
	#alert{} = Alert ->
	    Alert
    end.

generate_srp_server_keys(_SrpParams, 10) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
generate_srp_server_keys(SrpParams =
			     #srp_user{generator = Generator, prime = Prime,
				       verifier = Verifier}, N) ->
    case crypto:generate_key(srp, {host, [Verifier, Generator, Prime, '6a']}) of
	error ->
	    generate_srp_server_keys(SrpParams, N+1);
	Keys ->
	    Keys
    end.

generate_srp_client_keys(_Generator, _Prime, 10) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
generate_srp_client_keys(Generator, Prime, N) ->

    case crypto:generate_key(srp, {user, [Generator, Prime, '6a']}) of
	error ->
	    generate_srp_client_keys(Generator, Prime, N+1);
	Keys ->
	    Keys
    end.

handle_srp_identity(Username, {Fun, UserState}) ->
    case Fun(srp, Username, UserState) of
	{ok, {SRPParams, Salt, DerivedKey}}
	  when is_atom(SRPParams), is_binary(Salt), is_binary(DerivedKey) ->
	    {Generator, Prime} = ssl_srp_primes:get_srp_params(SRPParams),
	    Verifier = crypto:mod_pow(Generator, DerivedKey, Prime),
	    #srp_user{generator = Generator, prime = Prime,
		      salt = Salt, verifier = Verifier};
	#alert{} = Alert ->
	    throw(Alert);
	_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end.


cipher_role(client, Data, Session, #state{connection_states = ConnectionStates0} = State,
	    Connection) ->
    ConnectionStates = ssl_record:set_server_verify_data(current_both, Data, ConnectionStates0),
    Connection:next_state_connection(cipher,
				     ack_connection(
				       State#state{session = Session,
						   connection_states = ConnectionStates}));

cipher_role(server, Data, Session,  #state{connection_states = ConnectionStates0} = State0,
	    Connection) ->
    ConnectionStates1 = ssl_record:set_client_verify_data(current_read, Data, ConnectionStates0),
    State =
	finalize_handshake(State0#state{connection_states = ConnectionStates1,
					session = Session}, cipher, Connection),
    Connection:next_state_connection(cipher, ack_connection(State#state{session = Session})).

select_curve(#state{client_ecc = {[Curve|_], _}}) ->
    {namedCurve, Curve};
select_curve(_) ->
    {namedCurve, ?secp256r1}.

is_anonymous(Algo) when Algo == dh_anon;
			Algo == ecdh_anon;
			Algo == psk;
			Algo == dhe_psk;
			Algo == rsa_psk;
			Algo == srp_anon ->
    true;
is_anonymous(_) ->
    false.

get_current_prf(CStates, Direction) ->
	CS = ssl_record:current_connection_state(CStates, Direction),
	CS#connection_state.security_parameters#security_parameters.prf_algorithm.
get_pending_prf(CStates, Direction) ->
	CS = ssl_record:pending_connection_state(CStates, Direction),
	CS#connection_state.security_parameters#security_parameters.prf_algorithm.

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.

record_cb(tls_connection) ->
    tls_record;
record_cb(dtls_connection) ->
    dtls_record.

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

start_or_recv_cancel_timer(infinity, _RecvFrom) ->
    undefined;
start_or_recv_cancel_timer(Timeout, RecvFrom) ->
    erlang:send_after(Timeout, self(), {cancel_start_or_recv, RecvFrom}).

get_timeout(#state{ssl_options=#ssl_options{hibernate_after = undefined}}) ->
    infinity;
get_timeout(#state{ssl_options=#ssl_options{hibernate_after = HibernateAfter}}) ->
    HibernateAfter.

terminate_alert(normal, Version, ConnectionStates)  ->
    ssl_alert:encode(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
		     Version, ConnectionStates);
terminate_alert({Reason, _}, Version, ConnectionStates) when Reason == close;
							     Reason == shutdown ->
    ssl_alert:encode(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
		     Version, ConnectionStates);

terminate_alert(_, Version, ConnectionStates) ->
    ssl_alert:encode(?ALERT_REC(?FATAL, ?INTERNAL_ERROR),
		     Version, ConnectionStates).

handle_trusted_certs_db(#state{ssl_options = #ssl_options{cacertfile = <<>>, cacerts = []}}) ->
    %% No trusted certs specified
    ok;
handle_trusted_certs_db(#state{cert_db_ref = Ref,
			       cert_db = CertDb,
			       ssl_options = #ssl_options{cacertfile = <<>>}}) when CertDb =/= undefined ->
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

notify_senders(SendQueue) -> 
    lists:foreach(fun({From, _}) ->
 			  gen_fsm:reply(From, {error, closed})
 		  end, queue:to_list(SendQueue)).

notify_renegotiater({true, From}) when not is_atom(From)  ->
    gen_fsm:reply(From, {error, closed});
notify_renegotiater(_) ->
    ok.

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

cancel_timer(undefined) ->
    ok;
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    ok.

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

handle_new_session(NewId, CipherSuite, Compression, #state{session = Session0,
							   protocol_cb = Connection} = State0) ->
    Session = Session0#session{session_id = NewId,
			       cipher_suite = CipherSuite,
			       compression_method = Compression},
    {Record, State} = Connection:next_record(State0#state{session = Session}),
    Connection:next_state(hello, certify, Record, State).

handle_resumed_session(SessId, #state{connection_states = ConnectionStates0,
				      negotiated_version = Version,
				      host = Host, port = Port,
				      protocol_cb = Connection,
				      session_cache = Cache,
				      session_cache_cb = CacheCb} = State0) ->
    Session = CacheCb:lookup(Cache, {{Host, Port}, SessId}),
    case ssl_handshake:master_secret(tls_record, Version, Session,
				     ConnectionStates0, client) of
	{_, ConnectionStates} ->
	    {Record, State} =
		Connection:next_record(State0#state{
			      connection_states = ConnectionStates,
			      session = Session}),
	    Connection:next_state(hello, abbreviated, Record, State);
	#alert{} = Alert ->
	    Connection:handle_own_alert(Alert, Version, hello, State0)
    end.

make_premaster_secret({MajVer, MinVer}, rsa) ->
    Rand = ssl:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    <<?BYTE(MajVer), ?BYTE(MinVer), Rand/binary>>;
make_premaster_secret(_, _) ->
    undefined.

negotiated_hashsign(undefined, KexAlg, PubKeyInfo, Version) ->
    %% Not negotiated choose default 
    case is_anonymous(KexAlg) of
	true ->
	    {null, anon};
	false ->
	    {PubAlg, _, _} = PubKeyInfo,
	    ssl_handshake:select_hashsign_algs(undefined, PubAlg, Version)
    end;
negotiated_hashsign(HashSign = {_, _}, _, _, _) ->
    HashSign.

ssl_options_list(SslOptions) ->
    Fileds = record_info(fields, ssl_options),
    Values = tl(tuple_to_list(SslOptions)),
    ssl_options_list(Fileds, Values, []).

ssl_options_list([],[], Acc) ->
    lists:reverse(Acc);
%% Skip internal options, only return user options
ssl_options_list([protocol | Keys], [_ | Values], Acc) ->
    ssl_options_list(Keys, Values, Acc);
ssl_options_list([erl_dist | Keys], [_ | Values], Acc) ->
    ssl_options_list(Keys, Values, Acc);
ssl_options_list([renegotiate_at | Keys], [_ | Values], Acc) ->
    ssl_options_list(Keys, Values, Acc);
ssl_options_list([ciphers = Key | Keys], [Value | Values], Acc) ->
   ssl_options_list(Keys, Values, 
		    [{Key, lists:map(
			     fun(Suite) -> 
				     ssl_cipher:erl_suite_definition(Suite) 
			     end, Value)} 
		     | Acc]);
ssl_options_list([Key | Keys], [Value | Values], Acc) ->
   ssl_options_list(Keys, Values, [{Key, Value} | Acc]).


