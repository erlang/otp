%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

-module(ssl_connection).

-behaviour(gen_fsm).

-include("ssl_debug.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl"). 
-include("ssl_internal.hrl").
-include("ssl_int.hrl").
-include_lib("public_key/include/public_key.hrl"). 

%% Internal application API
-export([send/2, recv/3, connect/7, ssl_accept/6, handshake/2,
	 socket_control/3, close/1, shutdown/2,
	 new_user/2, get_opts/2, set_opts/2, info/1, session_info/1, 
	 peer_certificate/1, sockname/1, peername/1, renegotiation/1]).

%% Called by ssl_connection_sup
-export([start_link/7]). 

%% gen_fsm callbacks
-export([init/1, hello/2, certify/2, cipher/2, connection/2, 
	 abbreviated/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
          role,               % client | server
          user_application,   % {MonitorRef, pid()} 
          transport_cb,       % atom() - callback module 
          data_tag,           % atom()  - ex tcp.
	  close_tag,          % atom()  - ex tcp_closed
	  error_tag,          % atom() - ex  tcp_error
          host,               % string() | ipadress()
          port,               % integer()
          socket,             % socket() 
          ssl_options,        % #ssl_options{}
          socket_options,     % #socket_options{}
          connection_states,  % #connection_states{} from ssl_record.hrl
	  tls_packets = [],        % Not yet handled decode ssl/tls packets.
          tls_record_buffer,  % binary() buffer of incomplete records
          tls_handshake_buffer, % binary() buffer of incomplete handshakes
	  %% {{md5_hash, sha_hash}, {prev_md5, prev_sha}} (binary())
          tls_handshake_hashes, % see above 
          tls_cipher_texts,     % list() received but not deciphered yet
          own_cert,             % binary()  
          session,              % #session{} from ssl_handshake.hrl
	  session_cache,        % 
	  session_cache_cb,     %
          negotiated_version,   % #protocol_version{}
          supported_protocol_versions, % [atom()]
          client_certificate_requested = false,
	  key_algorithm,       % atom as defined by cipher_suite
          public_key_info,     % PKIX: {Algorithm, PublicKey, PublicKeyParams}
          private_key,         % PKIX: #'RSAPrivateKey'{}
	  diffie_hellman_params, % PKIX: #'DHParameter'{} relevant for server side
	  diffie_hellman_keys, % {PublicKey, PrivateKey}
          premaster_secret,    %
          cert_db_ref,         % ets_table()
          from,                % term(), where to reply
          bytes_to_read,       % integer(), # bytes to read in passive mode
          user_data_buffer,    % binary()
	  log_alert,           % boolean() 
	  renegotiation,        % {boolean(), From | internal | peer}
	  recv_during_renegotiation,  %boolean() 
	  send_queue           % queue()
	 }).

-define(DEFAULT_DIFFIE_HELLMAN_PARAMS, 
	#'DHParameter'{prime = ?DEFAULT_DIFFIE_HELLMAN_PRIME, 
		       base = ?DEFAULT_DIFFIE_HELLMAN_GENERATOR}).

%%====================================================================
%% Internal application API
%%====================================================================	     

%%--------------------------------------------------------------------
%% Function: send(Pid, Data) ->  ok | {error, Reason} 
%%
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(Pid, Data) -> 
    sync_send_all_state_event(Pid, {application_data, 
				    erlang:iolist_to_binary(Data)}, infinity).

%%--------------------------------------------------------------------
%% Function:  recv(Socket, Length Timeout) -> {ok, Data} | {error, reason}
%%
%% Description:  Receives data when active = false
%%--------------------------------------------------------------------
recv(Pid, Length, Timeout) -> 
    sync_send_all_state_event(Pid, {recv, Length}, Timeout).
%%--------------------------------------------------------------------
%% Function: : connect(Host, Port, Socket, Options, 
%%                     User, CbInfo, Timeout) -> {ok, Socket}
%%
%% Description: Connect to a ssl server.
%%--------------------------------------------------------------------
connect(Host, Port, Socket, Options, User, CbInfo, Timeout) ->
    try start_fsm(client, Host, Port, Socket, Options, User, CbInfo,
		  Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssl_not_started}
    end.
%%--------------------------------------------------------------------
%% Function: accept(Port, Socket, Opts, User, 
%%                  CbInfo, Timeout) -> {ok, Socket} | {error, Reason}
%%
%% Description: Performs accept on a ssl listen socket. e.i. performs
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
%% Function: handshake(SslSocket, Timeout) -> ok | {error, Reason}
%%
%% Description: Starts ssl handshake. 
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = Pid}, Timeout) ->  
    case sync_send_all_state_event(Pid, start, Timeout) of
	connected ->
	    ok;
 	Error ->
	    Error
    end.
%--------------------------------------------------------------------
%% Function: socket_control(Pid) -> {ok, SslSocket} | {error, Reason}
%%
%% Description: Set the ssl process to own the accept socket
%%--------------------------------------------------------------------	    
socket_control(Socket, Pid, CbModule) ->
    case CbModule:controlling_process(Socket, Pid) of
	ok ->
	    {ok, sslsocket(Pid)};
	{error, Reason}	->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function:  close() -> ok 
%%
%% Description:  Close a ssl connection
%%--------------------------------------------------------------------
close(ConnectionPid) ->
    case sync_send_all_state_event(ConnectionPid, close) of
	{error, closed} ->
	    ok;
	Other ->
	    Other
    end.

%%--------------------------------------------------------------------
%% Function:  shutdown(Socket, How) -> ok | {error, Reason}
%%
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(ConnectionPid, How) ->
    sync_send_all_state_event(ConnectionPid, {shutdown, How}).

%%--------------------------------------------------------------------
%% Function: new_user(ConnectionPid, User) -> ok | {error, Reason}
%%
%% Description:  Changes process that receives the messages when active = true
%% or once. 
%%--------------------------------------------------------------------
new_user(ConnectionPid, User) ->
    sync_send_all_state_event(ConnectionPid, {new_user, User}).
%%--------------------------------------------------------------------
%% Function: sockname(ConnectionPid) -> {ok, {Address, Port}} | {error, Reason}
%%
%% Description:  Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, sockname).
%%--------------------------------------------------------------------
%% Function: peername(ConnectionPid) -> {ok, {Address, Port}} | {error, Reason}
%%
%% Description:  Same as inet:peername/1
%%--------------------------------------------------------------------
peername(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, peername).
%%--------------------------------------------------------------------
%% Function: get_opts(ConnectionPid, OptTags) -> {ok, Options} | {error, Reason}
%%
%% Description: Same as inet:getopts/2
%%--------------------------------------------------------------------
get_opts(ConnectionPid, OptTags) ->
    sync_send_all_state_event(ConnectionPid, {get_opts, OptTags}).
%%--------------------------------------------------------------------
%% Function:  setopts(Socket, Options) -> ok | {error, Reason}
%%
%% Description:  Same as inet:setopts/2
%%--------------------------------------------------------------------
set_opts(ConnectionPid, Options) ->
    sync_send_all_state_event(ConnectionPid, {set_opts, Options}).

%%--------------------------------------------------------------------
%% Function: info(ConnectionPid) ->  {ok, {Protocol, CipherSuite}} | 
%%                                      {error, Reason}
%%
%% Description:  Returns ssl protocol and cipher used for the connection
%%--------------------------------------------------------------------
info(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, info). 

%%--------------------------------------------------------------------
%% Function: session_info(ConnectionPid) -> {ok, PropList} | {error, Reason} 
%%
%% Description:  Returns info about the ssl session
%%--------------------------------------------------------------------
session_info(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, session_info). 

%%--------------------------------------------------------------------
%% Function:  peercert(ConnectionPid) -> {ok, Cert} | {error, Reason}
%%
%% Description: Returns the peer cert
%%--------------------------------------------------------------------
peer_certificate(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, peer_certificate). 

%%--------------------------------------------------------------------
%% Function: renegotiation(ConnectionPid) -> ok | {error, Reason}
%%
%% Description: Starts a renegotiation of the ssl session.
%%--------------------------------------------------------------------
renegotiation(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, renegotiate). 

%%====================================================================
%% ssl_connection_sup API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%%
%% Description: Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Role, Host, Port, Socket, Options, User, CbInfo) ->
    gen_fsm:start_link(?MODULE, [Role, Host, Port, Socket, Options,
				 User, CbInfo], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([Role, Host, Port, Socket, {SSLOpts0, _} = Options, 
      User, CbInfo]) ->
    State0 = initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
    Hashes0 = ssl_handshake:init_hashes(),    

    try ssl_init(SSLOpts0, Role) of
	{ok, Ref, CacheRef, OwnCert, Key, DHParams} ->	   
	    State = State0#state{tls_handshake_hashes = Hashes0,
				 own_cert = OwnCert,
				 cert_db_ref = Ref,
				 session_cache = CacheRef,
				 private_key = Key,
				 diffie_hellman_params = DHParams},
	    {ok, hello, State}
    catch   
	throw:Error ->
	    {stop, Error}
    end.
   
%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%%
%% Description:There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent
%% using gen_fsm:send_event/2, the instance of this function with the
%% same name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%%--------------------------------------------------------------------
hello(start, #state{host = Host, port = Port, role = client,
		    ssl_options = SslOpts, 
		    transport_cb = Transport, socket = Socket,
		    connection_states = ConnectionStates,
		    own_cert = Cert,
		    renegotiation = {Renegotiation, _}}
      = State0) ->

    Hello = ssl_handshake:client_hello(Host, Port, 
				       ConnectionStates, 
				       SslOpts, Cert,
				       Renegotiation),

    Version = Hello#client_hello.client_version,
    Hashes0 = ssl_handshake:init_hashes(),
    {BinMsg, CS2, Hashes1} = 
        encode_handshake(Hello, Version, ConnectionStates, Hashes0),
    Transport:send(Socket, BinMsg),
    State1 = State0#state{connection_states = CS2,
			 negotiated_version = Version, %% Requested version
			 session = 
                         #session{session_id = Hello#client_hello.session_id,
                                  is_resumable = false},
                         tls_handshake_hashes = Hashes1},
    {Record, State} = next_record(State1),
    next_state(hello, Record, State);
    
hello(start, #state{role = server} = State0) ->
    {Record, State} = next_record(State0),
    next_state(hello, Record, State);

hello(#hello_request{}, #state{role = client} = State0) ->
    {Record, State} = next_record(State0),
    next_state(hello, Record, State);

hello(#server_hello{cipher_suite = CipherSuite,
		    compression_method = Compression} = Hello,
      #state{session = Session0 = #session{session_id = OldId},
	     connection_states = ConnectionStates0,
	     role = client,
	     negotiated_version = ReqVersion,
	     host = Host, port = Port,
	     renegotiation = {Renegotiation, _},
	     ssl_options = SslOptions,
	     session_cache = Cache,
	     session_cache_cb = CacheCb} = State0) ->

    case ssl_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
	{Version, NewId, ConnectionStates1} ->
	    {KeyAlgorithm, _, _} = 
		ssl_cipher:suite_definition(CipherSuite),
	    
	    PremasterSecret = make_premaster_secret(ReqVersion, KeyAlgorithm),
	    
	    State1 = State0#state{key_algorithm = KeyAlgorithm,
				  negotiated_version = Version,
				  connection_states = ConnectionStates1,
				  premaster_secret = PremasterSecret},
	    
	    case ssl_session:is_new(OldId, NewId) of
		true ->
		    Session = Session0#session{session_id = NewId,
					       cipher_suite = CipherSuite,
				       compression_method = Compression}, 
		    {Record, State} = next_record(State1#state{session = Session}),
		    next_state(certify, Record, State);
		false ->
		    Session = CacheCb:lookup(Cache, {{Host, Port}, NewId}),
		    case ssl_handshake:master_secret(Version, Session, 
						     ConnectionStates1, client) of
			{_, ConnectionStates2} ->	
			    {Record, State} = 
				next_record(State1#state{
					      connection_states = ConnectionStates2,
					      session = Session}),
			    next_state(abbreviated, Record, State);
			#alert{} = Alert ->
			    handle_own_alert(Alert, Version, hello, State1), 
			    {stop, normal, State1}
		    end
	    end;
	#alert{} = Alert ->
	    handle_own_alert(Alert, ReqVersion, hello, State0), 
            {stop, normal, State0}
    end;

hello(Hello = #client_hello{client_version = ClientVersion}, 
      State = #state{connection_states = ConnectionStates0,
		     port = Port, session = Session0,
		     renegotiation = {Renegotiation, _},
		     session_cache = Cache,		  
		     session_cache_cb = CacheCb,
		     ssl_options = SslOpts,
		     own_cert = Cert}) ->
    
    case ssl_handshake:hello(Hello, SslOpts, {Port, Session0, Cache, CacheCb,
				     ConnectionStates0, Cert}, Renegotiation) of
        {Version, {Type, Session}, ConnectionStates} ->       
            do_server_hello(Type, State#state{connection_states  = 
					      ConnectionStates,
					      negotiated_version = Version,
					      session = Session});
        #alert{} = Alert ->
            handle_own_alert(Alert, ClientVersion, hello, State), 
            {stop, normal, State}
    end;

hello(Msg, State) ->
    handle_unexpected_message(Msg, hello, State).

abbreviated(#hello_request{}, State0) ->
    {Record, State} = next_record(State0),
    next_state(hello, Record, State);

abbreviated(#finished{verify_data = Data} = Finished,
	    #state{role = server,
		   negotiated_version = Version,
		   tls_handshake_hashes = Hashes,
		   session = #session{master_secret = MasterSecret},
		  connection_states = ConnectionStates0} = 
	    State) ->
    case ssl_handshake:verify_connection(Version, Finished, client,
					 MasterSecret, Hashes) of
        verified ->  
	    ConnectionStates = ssl_record:set_client_verify_data(current_both, Data, ConnectionStates0),
	    next_state_connection(abbreviated, 
				  ack_connection(State#state{connection_states = ConnectionStates}));
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, abbreviated, State),
            {stop, normal, State} 
    end;

abbreviated(#finished{verify_data = Data} = Finished,
	    #state{role = client, tls_handshake_hashes = Hashes0,
		   session = #session{master_secret = MasterSecret},
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State) ->
    case ssl_handshake:verify_connection(Version, Finished, server,
					 MasterSecret, Hashes0) of
        verified ->
	    ConnectionStates1 = ssl_record:set_server_verify_data(current_read, Data, ConnectionStates0),
	    {ConnectionStates, Hashes} = 
		finalize_handshake(State#state{connection_states = ConnectionStates1}, abbreviated),
	    next_state_connection(abbreviated, 
				  ack_connection(State#state{tls_handshake_hashes = Hashes,
							     connection_states = 
							     ConnectionStates}));
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, abbreviated, State),
            {stop, normal, State} 
    end;

abbreviated(Msg, State) ->
    handle_unexpected_message(Msg, abbreviated, State).

certify(#hello_request{}, State0) ->
    {Record, State} = next_record(State0),
    next_state(hello, Record, State);

certify(#certificate{asn1_certificates = []}, 
	#state{role = server, negotiated_version = Version,
	       ssl_options = #ssl_options{verify = verify_peer,
					  fail_if_no_peer_cert = true}} = 
	State) ->
    Alert =  ?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE),
    handle_own_alert(Alert, Version, certify_certificate, State),
    {stop, normal, State};

certify(#certificate{asn1_certificates = []}, 
	#state{role = server,
	       ssl_options = #ssl_options{verify = verify_peer,
					  fail_if_no_peer_cert = false}} = 
	State0) ->
    {Record, State} = next_record(State0#state{client_certificate_requested = false}),
    next_state(certify, Record, State);

certify(#certificate{} = Cert, 
        #state{negotiated_version = Version,
	       role = Role,
	       cert_db_ref = CertDbRef,
	       ssl_options = Opts} = State) ->
    case ssl_handshake:certify(Cert, CertDbRef, Opts#ssl_options.depth, 
			       Opts#ssl_options.verify,
			       Opts#ssl_options.verify_fun,
			       Opts#ssl_options.validate_extensions_fun, Role) of
        {PeerCert, PublicKeyInfo} ->
	    handle_peer_cert(PeerCert, PublicKeyInfo, 
			     State#state{client_certificate_requested = false});
	#alert{} = Alert ->
            handle_own_alert(Alert, Version, certify_certificate, State),
            {stop, normal, State}
    end;

certify(#server_key_exchange{} = KeyExchangeMsg, 
        #state{role = client, negotiated_version = Version,
	       key_algorithm = Alg} = State0) 
  when Alg == dhe_dss; Alg == dhe_rsa ->
    case handle_server_key(KeyExchangeMsg, State0) of
	#state{} = State1 ->
	    {Record, State} = next_record(State1),
	    next_state(certify, Record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, certify_server_keyexchange, 
			     State0),
	    {stop, normal, State0}
    end;

certify(#server_key_exchange{} = Msg, 
        #state{role = client, key_algorithm = rsa} = State) -> 
    handle_unexpected_message(Msg, certify_server_keyexchange, State);

certify(#certificate_request{}, State0) ->
    {Record, State} = next_record(State0#state{client_certificate_requested = true}),
    next_state(certify, Record, State);

%% Master secret was determined with help of server-key exchange msg
certify(#server_hello_done{},
	#state{session = #session{master_secret = MasterSecret} = Session,
	       connection_states = ConnectionStates0,
	       negotiated_version = Version,
	       premaster_secret = undefined,
	       role = client} = State0) ->    
    case ssl_handshake:master_secret(Version, Session, 
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates1} -> 
	    State = State0#state{connection_states = ConnectionStates1},
	    client_certify_and_key_exchange(State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, 
			     certify_server_hello_done, State0),
	    {stop, normal, State0} 
    end;

%% Master secret is calculated from premaster_secret
certify(#server_hello_done{},
	#state{session = Session0,
	       connection_states = ConnectionStates0,
	       negotiated_version = Version,
	       premaster_secret = PremasterSecret,
	       role = client} = State0) ->    
    case ssl_handshake:master_secret(Version, PremasterSecret, 
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates1} -> 
	    Session = Session0#session{master_secret = MasterSecret},
	    State = State0#state{connection_states = ConnectionStates1,
				  session = Session},
	    client_certify_and_key_exchange(State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, 
			     certify_server_hello_done, State0),
	    {stop, normal, State0} 
    end;

certify(#client_key_exchange{} = Msg,
	#state{role = server,
	       client_certificate_requested = true,
	       ssl_options = #ssl_options{fail_if_no_peer_cert = true}} = State) ->
    %% We expect a certificate here
    handle_unexpected_message(Msg, certify_client_key_exchange, State);

certify(#client_key_exchange{exchange_keys 
			     = #encrypted_premaster_secret{premaster_secret 
							   = EncPMS}},
        #state{negotiated_version = Version,
	       connection_states = ConnectionStates0,
	       session = Session0,
	       private_key = Key} = State0) ->
    try ssl_handshake:decrypt_premaster_secret(EncPMS, Key) of
	PremasterSecret ->
	    case ssl_handshake:master_secret(Version, PremasterSecret, 
					     ConnectionStates0, server) of
		{MasterSecret, ConnectionStates} ->
		    Session = Session0#session{master_secret = MasterSecret},
		    State1 = State0#state{connection_states = ConnectionStates,
					 session = Session},
		    {Record, State} = next_record(State1),
		    next_state(cipher, Record, State);
		#alert{} = Alert ->
		    handle_own_alert(Alert, Version, 
				     certify_client_key_exchange, State0),
		    {stop, normal, State0} 
	    end
    catch 
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, certify_client_key_exchange, 
			     State0),
	    {stop, normal, State0} 
    end;

certify(#client_key_exchange{exchange_keys = #client_diffie_hellman_public{
			       dh_public = ClientPublicDhKey}},
        #state{negotiated_version = Version,
	       diffie_hellman_params = #'DHParameter'{prime = P,
						      base = G},
	       diffie_hellman_keys = {_, ServerDhPrivateKey},
	       role = Role,
	       session = Session,
	       connection_states = ConnectionStates0} = State0) ->
    
    PMpint = crypto:mpint(P),
    GMpint = crypto:mpint(G),	
    PremasterSecret = crypto:dh_compute_key(mpint_binary(ClientPublicDhKey),
					    ServerDhPrivateKey,
					    [PMpint, GMpint]),
    
    case ssl_handshake:master_secret(Version, PremasterSecret,
				     ConnectionStates0, Role) of
	{MasterSecret, ConnectionStates} ->
	    State1 = State0#state{session = 
				 Session#session{master_secret 
						 = MasterSecret},
				connection_states = ConnectionStates},

	    {Record, State} = next_record(State1),
	    next_state(cipher, Record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, 
			     certify_client_key_exchange, State0),
	    {stop, normal, State0} 
    end;

certify(Msg, State) ->
    handle_unexpected_message(Msg, certify, State).

cipher(#hello_request{}, State0) ->
    {Record, State} = next_record(State0),
    next_state(hello, Record, State);

cipher(#certificate_verify{signature = Signature}, 
       #state{role = server, 
	      public_key_info = PublicKeyInfo,
	      negotiated_version = Version,
	      session = #session{master_secret = MasterSecret},
	      key_algorithm = Algorithm,
	      tls_handshake_hashes = Hashes
	     } = State0) -> 
    case ssl_handshake:certificate_verify(Signature, PublicKeyInfo,
					  Version, MasterSecret, 
					  Algorithm, Hashes) of
	valid ->
	    {Record, State} = next_record(State0),
	    next_state(cipher, Record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, cipher, State0), 
	    {stop, normal, State0}
    end;

cipher(#finished{verify_data = Data} = Finished, 
       #state{negotiated_version = Version,
	      host = Host,
	      port = Port,
	      role = Role,
	      session = #session{master_secret = MasterSecret} 
	      = Session0,
	      tls_handshake_hashes = Hashes0,
	     connection_states = ConnectionStates0} = State) ->    
    
    case ssl_handshake:verify_connection(Version, Finished, 
					 opposite_role(Role), 
                                         MasterSecret, Hashes0) of
        verified ->
	    Session = register_session(Role, Host, Port, Session0),
            case Role of
                client ->
		    ConnectionStates = ssl_record:set_server_verify_data(current_both, Data, ConnectionStates0),
                    next_state_connection(cipher, ack_connection(State#state{session = Session,
									     connection_states = ConnectionStates}));
                server ->
		    ConnectionStates1 = ssl_record:set_client_verify_data(current_read, Data, ConnectionStates0),
                    {ConnectionStates, Hashes} = 
                        finalize_handshake(State#state{ 
						    connection_states = ConnectionStates1,
						    session = Session}, cipher),
                    next_state_connection(cipher, ack_connection(State#state{connection_states = 
									     ConnectionStates,
									     session = Session,
									     tls_handshake_hashes =
									     Hashes}))
            end;
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, cipher, State),
            {stop, normal, State} 
    end;

cipher(Msg, State) ->
    handle_unexpected_message(Msg, cipher, State).


connection(#hello_request{}, #state{host = Host, port = Port,
				    socket = Socket,
				    ssl_options = SslOpts,
				    negotiated_version = Version,
				    own_cert = Cert,
				    transport_cb = Transport,
				    connection_states = ConnectionStates0,
				    renegotiation = {Renegotiation, _},
				    tls_handshake_hashes = Hashes0} = State0) ->
   
    Hello = ssl_handshake:client_hello(Host, Port, 
				       ConnectionStates0, SslOpts, Cert, Renegotiation),
  
    {BinMsg, ConnectionStates1, Hashes1} =
        encode_handshake(Hello, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    {Record, State} = next_record(State0#state{connection_states =  
					       ConnectionStates1,
					       tls_handshake_hashes = Hashes1}),
    next_state(hello, Record, State);
connection(#client_hello{} = Hello, #state{role = server} = State) ->
    hello(Hello, State);

connection(Msg, State) ->
    handle_unexpected_message(Msg, connection, State).
%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%                                                  NextState} |
%%                                          {next_state, NextStateName, 
%%                                                  NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event({application_data, Data0}, From, connection, 
		  #state{socket = Socket,
			 negotiated_version = Version,
			 transport_cb = Transport,
			 connection_states = ConnectionStates0,
			 send_queue = SendQueue,
			 socket_options = SockOpts,
			 ssl_options = #ssl_options{renegotiate_at = RenegotiateAt}} 
		  = State) ->
    %% We should look into having a worker process to do this to 
    %% parallize send and receive decoding and not block the receiver
    %% if sending is overloading the socket.
    try
	Data = encode_packet(Data0, SockOpts),
	case encode_data(Data, Version, ConnectionStates0, RenegotiateAt) of
	    {Msgs, [], ConnectionStates} ->
		Result = Transport:send(Socket, Msgs),
		{reply, Result,
		 connection, State#state{connection_states = ConnectionStates}};
	    {Msgs, RestData, ConnectionStates} ->
		if 
		    Msgs =/= [] ->
			Transport:send(Socket, Msgs);
		    true ->
			ok
		end,
		renegotiate(State#state{connection_states = ConnectionStates,
					send_queue = queue:in_r({From, RestData}, SendQueue),
					renegotiation = {true, internal}})
	end
    catch throw:Error ->
	    {reply, Error, connection, State}
    end;
handle_sync_event({application_data, Data}, From, StateName, 
		  #state{send_queue = Queue} = State) ->
    %% In renegotiation priorities handshake, send data when handshake is finished
    {next_state, StateName, State#state{send_queue = queue:in({From, Data}, Queue)}};

handle_sync_event(start, From, hello, State) ->
    hello(start, State#state{from = From});

%% The two clauses below could happen if a server upgrades a socket in
%% active mode. Note that in this case we are lucky that
%% controlling_process has been evalueated before receiving handshake
%% messages from client. The server should put the socket in passive
%% mode before telling the client that it is willing to upgrade
%% and before calling ssl:ssl_accept/2. These clauses are 
%% here to make sure it is the users problem and not owers if
%% they upgrade a active socket. 
handle_sync_event(start, _, connection, State) ->
    {reply, connected, connection, State};
handle_sync_event(start, From, StateName, State) ->
    {next_state, StateName, State#state{from = From}};

handle_sync_event(close, _, _StateName, State) ->
    {stop, normal, ok, State};

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
	    {reply, ok, StateName, State};
	Error ->
	    {stop, normal, Error, State}
    end;
    
handle_sync_event({recv, N}, From, connection = StateName, State0) ->
    passive_receive(State0#state{bytes_to_read = N, from = From}, StateName);

%% Doing renegotiate wait with handling request until renegotiate is
%% finished. Will be handled by next_state_connection/2.
handle_sync_event({recv, N}, From, StateName, State) ->
    {next_state, StateName, State#state{bytes_to_read = N, from = From,
				       recv_during_renegotiation = true}};

handle_sync_event({new_user, User}, _From, StateName, 
		  State =#state{user_application = {OldMon, _}}) ->
    NewMon = erlang:monitor(process, User),
    erlang:demonitor(OldMon, [flush]),
    {reply, ok, StateName, State#state{user_application = {NewMon,User}}};

handle_sync_event({get_opts, OptTags}, _From, StateName,
		  #state{socket = Socket,
			 socket_options = SockOpts} = State) ->
    OptsReply = get_socket_opts(Socket, OptTags, SockOpts, []),
    {reply, OptsReply, StateName, State};

handle_sync_event(sockname, _From, StateName,
		  #state{socket = Socket} = State) ->
    SockNameReply = inet:sockname(Socket),
    {reply, SockNameReply, StateName, State};

handle_sync_event(peername, _From, StateName,
		  #state{socket = Socket} = State) ->
    PeerNameReply = inet:peername(Socket),
    {reply, PeerNameReply, StateName, State};

handle_sync_event({set_opts, Opts0}, _From, StateName, 
		  #state{socket_options = Opts1, 
			 socket = Socket,
			 user_data_buffer = Buffer} = State0) ->
    Opts   = set_socket_opts(Socket, Opts0, Opts1, []),
    State1 = State0#state{socket_options = Opts},
    if 
	Opts#socket_options.active =:= false ->
	    {reply, ok, StateName, State1};
	Buffer =:= <<>>, Opts1#socket_options.active =:= false ->
            %% Need data, set active once
	    {Record, State2} = next_record_if_active(State1),
	    case next_state(StateName, Record, State2) of
		{next_state, StateName, State} ->
		    {reply, ok, StateName, State};
		{stop, Reason, State} ->
		    {stop, Reason, State}
	    end;
	Buffer =:= <<>> ->
            %% Active once already set 
	    {reply, ok, StateName, State1};
	true ->
	    case application_data(<<>>, State1) of
		Stop = {stop,_,_} ->
		    Stop;
		{Record, State2} ->
		    case next_state(StateName, Record, State2) of
			{next_state, StateName, State} ->
			    {reply, ok, StateName, State};
			{stop, Reason, State} ->
			    {stop, Reason, State}
		    end
	    end
    end;

handle_sync_event(renegotiate, From, connection, State) ->
    renegotiate(State#state{renegotiation = {true, From}});

handle_sync_event(renegotiate, _, StateName, State) ->
    {reply, {error, already_renegotiating}, StateName, State};

handle_sync_event(info, _, StateName, 
		  #state{negotiated_version = Version,
			 session = #session{cipher_suite = Suite}} = State) ->
    
    AtomVersion = ssl_record:protocol_version(Version),
    {reply, {ok, {AtomVersion, ssl_cipher:suite_definition(Suite)}}, 
     StateName, State};

handle_sync_event(session_info, _, StateName, 
		  #state{session = #session{session_id = Id,
					    cipher_suite = Suite}} = State) ->
    {reply, [{session_id, Id}, 
	     {cipher_suite, ssl_cipher:suite_definition(Suite)}], 
     StateName, State};

handle_sync_event(peer_certificate, _, StateName, 
		  #state{session = #session{peer_certificate = Cert}} 
		  = State) ->
    {reply, {ok, Cert}, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------

%% raw data from TCP, unpack records
handle_info({Protocol, _, Data}, StateName,
            #state{data_tag = Protocol,
		   negotiated_version = Version} = State0) ->
    case next_tls_record(Data, State0) of
	{Record, State} ->
	    next_state(StateName, Record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, StateName, State0), 
	    {stop, normal, State0}
    end;

handle_info({CloseTag, Socket}, _StateName,
            #state{socket = Socket, close_tag = CloseTag,
		   negotiated_version = Version, host = Host,
		   port = Port, socket_options = Opts, 
		   user_application = {_Mon,Pid}, from = From, 
		   role = Role, session = Session} = State) ->
    %% Debug option maybe, the user do NOT want to see these in their logs
    %% error_logger:info_report("SSL: Peer did not send close notify alert."),
    case Version of
	{1, N} when N >= 1 ->
	    ok;
	_ ->
	    invalidate_session(Role, Host, Port, Session)
    end,
    alert_user(Opts#socket_options.active, Pid, From,
	       ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY), Role),
    {stop, normal, State};

handle_info({ErrorTag, Socket, econnaborted}, StateName,  
	    #state{socket = Socket, from = User, role = Role, 
		   error_tag = ErrorTag} = State)  when StateName =/= connection ->
    alert_user(User, ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE), Role),
    {stop, normal, State};

handle_info({ErrorTag, Socket, Reason}, _,  
	    #state{socket = Socket, from = User, 
		   role = Role, error_tag = ErrorTag} = State)  ->
    Report = io_lib:format("SSL: Socket error: ~p ~n", [Reason]),
    error_logger:info_report(Report),
    alert_user(User,  ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), Role),
    {stop, normal, State};

handle_info({'DOWN', MonitorRef, _, _, _}, _, 
	    State = #state{user_application={MonitorRef,_Pid}}) ->
    {stop, normal, State};   

handle_info(Msg, StateName, State) ->
    Report = io_lib:format("SSL: Got unexpected info: ~p ~n", [Msg]),
    error_logger:info_report(Report),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, connection, #state{negotiated_version = Version,
				      connection_states = ConnectionStates,
				      transport_cb = Transport,
				      socket = Socket, send_queue = SendQueue,
				      renegotiation = Renegotiate}) ->
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate),
    {BinAlert, _} = encode_alert(?ALERT_REC(?WARNING,?CLOSE_NOTIFY),
				 Version, ConnectionStates),
    Transport:send(Socket, BinAlert),
    workaround_transport_delivery_problems(Socket, Transport),
    Transport:close(Socket);
terminate(_Reason, _StateName, #state{transport_cb = Transport, 
				      socket = Socket, send_queue = SendQueue,
				      renegotiation = Renegotiate}) ->
    notify_senders(SendQueue),
    notify_renegotiater(Renegotiate),
    workaround_transport_delivery_problems(Socket, Transport),
    Transport:close(Socket).

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_fsm(Role, Host, Port, Socket, Opts,  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    case ssl_connection_sup:start_child([Role, Host, Port, Socket, 
                                         Opts, User, CbInfo]) of
        {ok, Pid} -> 
	    case socket_control(Socket, Pid, CbModule) of
		{ok, SslSocket} ->
		    case handshake(SslSocket, Timeout) of
			ok  ->
			    {ok, SslSocket};
			{error, Reason} ->
			    {error, Reason} 
		    end;
		{error, Reason} ->
		    {error, Reason}
            end;
	{error, Reason} ->
            {error, Reason}
    end.
 
ssl_init(SslOpts, Role) ->
    {ok, CertDbRef, CacheRef, OwnCert} = init_certificates(SslOpts, Role),
    PrivateKey =
	init_private_key(SslOpts#ssl_options.key, SslOpts#ssl_options.keyfile,
			 SslOpts#ssl_options.password, Role),
    DHParams = init_diffie_hellman(SslOpts#ssl_options.dhfile, Role),
    {ok, CertDbRef, CacheRef, OwnCert, PrivateKey, DHParams}.

init_certificates(#ssl_options{cacertfile = CACertFile,
			       certfile = CertFile}, Role) ->

    case ssl_manager:connection_init(CACertFile, Role) of
	{ok, CertDbRef, CacheRef} ->
	    init_certificates(CertDbRef, CacheRef, CertFile, Role);
	{error, Reason} ->
	    handle_file_error(?LINE, error, Reason, CACertFile, ecacertfile,
			      erlang:get_stacktrace())
    end.

init_certificates(CertDbRef, CacheRef, CertFile, client) -> 
    try 
	[OwnCert] = ssl_certificate:file_to_certificats(CertFile),
	{ok, CertDbRef, CacheRef, OwnCert}
    catch _E:_R  ->
	    {ok, CertDbRef, CacheRef, undefined}
    end;

init_certificates(CertDbRef, CacheRef, CertFile, server) ->
     try 
	[OwnCert] = ssl_certificate:file_to_certificats(CertFile),
	{ok, CertDbRef, CacheRef, OwnCert}
     catch
	 Error:Reason ->
	     handle_file_error(?LINE, Error, Reason, CertFile, ecertfile,
			       erlang:get_stacktrace())
     end.

init_private_key(undefined, "", _Password, client) -> 
    undefined;
init_private_key(undefined, KeyFile, Password, _)  -> 
    case ssl_manager:cache_pem_file(KeyFile) of
	{ok, List} ->
	    [Der] = [Der || Der = {PKey, _ , _} <- List,
			    PKey =:= rsa_private_key orelse 
				PKey =:= dsa_private_key],
	    {ok, Decoded} = public_key:decode_private_key(Der,Password),
	    Decoded;
	{error, Reason} -> 
	    handle_file_error(?LINE, error, Reason, KeyFile, ekeyfile,
			      erlang:get_stacktrace()) 
    end;

init_private_key(PrivateKey, _, _,_) ->
    PrivateKey.

handle_file_error(Line, Error, {badmatch, Reason}, File, Throw, Stack) ->
    file_error(Line, Error, Reason, File, Throw, Stack);
handle_file_error(Line, Error, Reason, File, Throw, Stack) ->
    file_error(Line, Error, Reason, File, Throw, Stack).

file_error(Line, Error, Reason, File, Throw, Stack) ->
    Report = io_lib:format("SSL: ~p: ~p:~p ~s~n  ~p~n",
			   [Line, Error, Reason, File, Stack]),
    error_logger:error_report(Report),
    throw(Throw).

init_diffie_hellman(_, client) ->
    undefined;
init_diffie_hellman(undefined, _) ->
    ?DEFAULT_DIFFIE_HELLMAN_PARAMS;
init_diffie_hellman(DHParamFile, server) ->
    case ssl_manager:cache_pem_file(DHParamFile) of
	{ok, List} ->
	    case [Der || Der = {dh_params, _ , _} <- List] of
		[Der] ->
		    {ok, Decoded} = public_key:decode_dhparams(Der),
		    Decoded;
		[] ->
		    ?DEFAULT_DIFFIE_HELLMAN_PARAMS
	    end;
	{error, Reason} ->
	    handle_file_error(?LINE, error, Reason, DHParamFile, edhfile,  erlang:get_stacktrace()) 
    end.

sync_send_all_state_event(FsmPid, Event) ->
    sync_send_all_state_event(FsmPid, Event, infinity).

sync_send_all_state_event(FsmPid, Event, Timeout) ->
    try gen_fsm:sync_send_all_state_event(FsmPid, Event, Timeout)
    catch
 	exit:{noproc, _} ->
 	    {error, closed};
 	exit:{timeout, _} ->
 	    {error, timeout};
	exit:{normal, _} ->
	    {error, closed};
	exit:{shutdown, _} -> 
	    {error, closed}
    end.

%% We do currently not support cipher suites that use fixed DH.
%% If we want to implement that we should add a code
%% here to extract DH parameters form cert.
handle_peer_cert(PeerCert, PublicKeyInfo, 
		 #state{session = Session} = State0) ->
    State1 = State0#state{session = 
			 Session#session{peer_certificate = PeerCert},
			 public_key_info = PublicKeyInfo},
    {Record, State} = next_record(State1),
    next_state(certify, Record, State).

certify_client(#state{client_certificate_requested = true, role = client,
                      connection_states = ConnectionStates0,
                      transport_cb = Transport,
                      negotiated_version = Version,
                      cert_db_ref = CertDbRef,
                      own_cert = OwnCert,
                      socket = Socket,
                      tls_handshake_hashes = Hashes0} = State) ->
    Certificate = ssl_handshake:certificate(OwnCert, CertDbRef, client),
    {BinCert, ConnectionStates1, Hashes1} =
        encode_handshake(Certificate, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinCert),
            State#state{connection_states = ConnectionStates1, 
                        tls_handshake_hashes = Hashes1};
certify_client(#state{client_certificate_requested = false} = State) ->
    State.

verify_client_cert(#state{client_certificate_requested = true, role = client,
			  connection_states = ConnectionStates0,
			  transport_cb = Transport,
			  negotiated_version = Version,
			  own_cert = OwnCert,
			  socket = Socket,
			  key_algorithm = KeyAlg,
			  private_key = PrivateKey,
			  session = #session{master_secret = MasterSecret},
			  tls_handshake_hashes = Hashes0} = State) ->
    case ssl_handshake:client_certificate_verify(OwnCert, MasterSecret, 
						 Version, KeyAlg, 
						 PrivateKey, Hashes0) of
        #certificate_verify{} = Verified ->
            {BinVerified, ConnectionStates1, Hashes1} = 
                encode_handshake(Verified, KeyAlg, Version, 
                                 ConnectionStates0, Hashes0),
            Transport:send(Socket, BinVerified),
            State#state{connection_states = ConnectionStates1,
                        tls_handshake_hashes = Hashes1};
	ignore ->
	    State;
	 #alert{} = Alert ->
	    handle_own_alert(Alert, Version, certify, State)
	    
    end;
verify_client_cert(#state{client_certificate_requested = false} = State) ->
    State.

do_server_hello(Type, #state{negotiated_version = Version,
			     session = Session,
			     connection_states = ConnectionStates0,
			     renegotiation = {Renegotiation, _}} 
		= State0) when is_atom(Type) -> 
    ServerHello = 
        ssl_handshake:server_hello(Session#session.session_id, Version, 
                                   ConnectionStates0, Renegotiation),
    State1 = server_hello(ServerHello, State0),
    
    case Type of	
	new ->
	    do_server_hello(ServerHello, State1);
	resumed ->
	    ConnectionStates1 = State1#state.connection_states,
	    case ssl_handshake:master_secret(Version, Session,
					     ConnectionStates1, server) of
		{_, ConnectionStates2} ->
		    State2 = State1#state{connection_states=ConnectionStates2, 
					  session = Session},		    
		    {ConnectionStates, Hashes} = 
			finalize_handshake(State2, abbreviated),
		    State3 = State2#state{connection_states = 
					  ConnectionStates,
					  tls_handshake_hashes = Hashes},
		    {Record, State} = next_record(State3),
		    next_state(abbreviated, Record, State);
		#alert{} = Alert ->
		    handle_own_alert(Alert, Version, hello, State1), 
		    {stop, normal, State1}
	    end
    end;

do_server_hello(#server_hello{cipher_suite = CipherSuite,
			      compression_method = Compression,
			      session_id = SessionId}, 
		#state{session = Session0,
		       negotiated_version = Version} = State0) ->
    try server_certify_and_key_exchange(State0) of 
        #state{} = State1 ->
            State2 = server_hello_done(State1),
	    Session = 
		Session0#session{session_id = SessionId,
				 cipher_suite = CipherSuite,
				 compression_method = Compression},
	    {Record, State} = next_record(State2#state{session = Session}),
	    next_state(certify, Record, State)
    catch        
        #alert{} = Alert ->  
	    handle_own_alert(Alert, Version, hello, State0),
	    {stop, normal, State0}
    end.

client_certify_and_key_exchange(#state{negotiated_version = Version} = 
				State0) ->
    try do_client_certify_and_key_exchange(State0) of 
        State1 = #state{} ->
	    {ConnectionStates, Hashes} = finalize_handshake(State1, certify),
            State2 = State1#state{connection_states = ConnectionStates,
				 %% Reinitialize 
				 client_certificate_requested = false,
				 tls_handshake_hashes = Hashes},
	    {Record, State} = next_record(State2),
	    next_state(cipher, Record, State)
    catch        
        #alert{} = Alert ->  
	    handle_own_alert(Alert, Version, client_certify_and_key_exchange, State0),
            {stop, normal, State0}
    end.

do_client_certify_and_key_exchange(State0) ->
    State1 = certify_client(State0), 
    State2 = key_exchange(State1),
    verify_client_cert(State2).

server_certify_and_key_exchange(State0) ->
    State1 = certify_server(State0), 
    State2 = key_exchange(State1),
    request_client_cert(State2).
    
server_hello(ServerHello, #state{transport_cb = Transport,
                                 socket = Socket,
                                 negotiated_version = Version,
                                 connection_states = ConnectionStates0,
                                 tls_handshake_hashes = Hashes0} = State) ->
    CipherSuite = ServerHello#server_hello.cipher_suite,
    {KeyAlgorithm, _, _} = ssl_cipher:suite_definition(CipherSuite),
    {BinMsg, ConnectionStates1, Hashes1} = 
        encode_handshake(ServerHello, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    State#state{connection_states = ConnectionStates1,
                tls_handshake_hashes = Hashes1,
                key_algorithm = KeyAlgorithm}.
   
server_hello_done(#state{transport_cb = Transport,
                         socket = Socket,
                         negotiated_version = Version,
                         connection_states = ConnectionStates,
                         tls_handshake_hashes = Hashes} = State) ->
    
    HelloDone = ssl_handshake:server_hello_done(),
    
    {BinHelloDone, NewConnectionStates, NewHashes} =
        encode_handshake(HelloDone, Version, ConnectionStates, Hashes),
    Transport:send(Socket, BinHelloDone),
    State#state{connection_states = NewConnectionStates,
		tls_handshake_hashes = NewHashes}.
   
certify_server(#state{transport_cb = Transport,
                      socket = Socket,
                      negotiated_version = Version,
                      connection_states = ConnectionStates,
                      tls_handshake_hashes = Hashes,
                      cert_db_ref = CertDbRef,
                      own_cert = OwnCert} = State) ->

    case ssl_handshake:certificate(OwnCert, CertDbRef, server) of
	CertMsg = #certificate{} ->
	    {BinCertMsg, NewConnectionStates, NewHashes} =
		encode_handshake(CertMsg, Version, ConnectionStates, Hashes),
	    Transport:send(Socket, BinCertMsg),
	    State#state{connection_states = NewConnectionStates,
			tls_handshake_hashes = NewHashes
		       };
	Alert = #alert{} ->
	    throw(Alert)
    end.

key_exchange(#state{role = server, key_algorithm = rsa} = State) ->
    State;
key_exchange(#state{role = server, key_algorithm = Algo,
		    diffie_hellman_params = Params,
		    private_key = PrivateKey,
		    connection_states = ConnectionStates0,
		    negotiated_version = Version,
		    tls_handshake_hashes = Hashes0,
		    socket = Socket,
		    transport_cb = Transport
		   } = State) 
  when Algo == dhe_dss;
       Algo == dhe_dss_export;
       Algo == dhe_rsa;
       Algo == dhe_rsa_export  ->

    Keys = public_key:gen_key(Params),
    ConnectionState = 
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams, 
    Msg =  ssl_handshake:key_exchange(server, {dh, Keys, Params,
					       Algo, ClientRandom, 
					       ServerRandom,
					       PrivateKey}),
    {BinMsg, ConnectionStates, Hashes1} =
        encode_handshake(Msg, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    State#state{connection_states = ConnectionStates,
		diffie_hellman_keys = Keys,
                tls_handshake_hashes = Hashes1};


%% key_algorithm = dh_anon is not supported. Should be by default disabled
%% if support is implemented and then we need a key_exchange clause for it
%% here. 
   
key_exchange(#state{role = client, 
		    connection_states = ConnectionStates0,
		    key_algorithm = rsa,
		    public_key_info = PublicKeyInfo,
		    negotiated_version = Version,
		    premaster_secret = PremasterSecret,
		    socket = Socket, transport_cb = Transport,
		    tls_handshake_hashes = Hashes0} = State) ->
    Msg = rsa_key_exchange(PremasterSecret, PublicKeyInfo),    
    {BinMsg, ConnectionStates1, Hashes1} =
        encode_handshake(Msg, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    State#state{connection_states = ConnectionStates1,
                tls_handshake_hashes = Hashes1};
key_exchange(#state{role = client, 
		    connection_states = ConnectionStates0,
		    key_algorithm = Algorithm,
		    negotiated_version = Version,
		    diffie_hellman_keys = {DhPubKey, _},
		    socket = Socket, transport_cb = Transport,
		    tls_handshake_hashes = Hashes0} = State) 
  when Algorithm == dhe_dss;
       Algorithm == dhe_dss_export;
       Algorithm == dhe_rsa;
       Algorithm == dhe_rsa_export ->
    Msg =  ssl_handshake:key_exchange(client, {dh, DhPubKey}),
    {BinMsg, ConnectionStates1, Hashes1} =
        encode_handshake(Msg, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    State#state{connection_states = ConnectionStates1,
                tls_handshake_hashes = Hashes1}.

rsa_key_exchange(PremasterSecret, PublicKeyInfo = {Algorithm, _, _})  
  when Algorithm == ?rsaEncryption;
       Algorithm == ?md2WithRSAEncryption;
       Algorithm == ?md5WithRSAEncryption;
       Algorithm == ?sha1WithRSAEncryption ->
    ssl_handshake:key_exchange(client, 
			       {premaster_secret, PremasterSecret,
				PublicKeyInfo});
rsa_key_exchange(_, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE)).

request_client_cert(#state{ssl_options = #ssl_options{verify = verify_peer},
			   connection_states = ConnectionStates0,
			   cert_db_ref = CertDbRef,
			   tls_handshake_hashes = Hashes0,
			   negotiated_version = Version,
			   socket = Socket,
			   transport_cb = Transport} = State) ->
    Msg = ssl_handshake:certificate_request(ConnectionStates0, CertDbRef),
    {BinMsg, ConnectionStates1, Hashes1} =
        encode_handshake(Msg, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    State#state{client_certificate_requested = true,
		connection_states = ConnectionStates1,      
		tls_handshake_hashes = Hashes1};
request_client_cert(#state{ssl_options = #ssl_options{verify = verify_none}} =
		    State) ->
    State.

finalize_handshake(State, StateName) ->
    ConnectionStates0 = cipher_protocol(State),    
    ConnectionStates =
        ssl_record:activate_pending_connection_state(ConnectionStates0,
                                                     write),
    finished(State#state{connection_states = ConnectionStates}, StateName).
    
cipher_protocol(#state{connection_states = ConnectionStates0,
                       socket = Socket,
                       negotiated_version = Version,
                       transport_cb = Transport}) ->
    {BinChangeCipher, ConnectionStates} =
        encode_change_cipher(#change_cipher_spec{}, 
			     Version, ConnectionStates0),
    Transport:send(Socket, BinChangeCipher),
    ConnectionStates.
   
finished(#state{role = Role, socket = Socket, negotiated_version = Version,
                transport_cb = Transport,
		session = Session,
                connection_states = ConnectionStates0,
                tls_handshake_hashes = Hashes0}, StateName) ->
    MasterSecret = Session#session.master_secret,
    Finished = ssl_handshake:finished(Version, Role, MasterSecret, Hashes0),
    ConnectionStates1 = save_verify_data(Role, Finished, ConnectionStates0, StateName),
    {BinFinished, ConnectionStates, Hashes} = 
        encode_handshake(Finished, Version, ConnectionStates1, Hashes0),
    Transport:send(Socket, BinFinished),
    {ConnectionStates, Hashes}.

save_verify_data(client, #finished{verify_data = Data}, ConnectionStates, certify) ->
    ssl_record:set_client_verify_data(current_write, Data, ConnectionStates);
save_verify_data(server, #finished{verify_data = Data}, ConnectionStates, cipher) ->
    ssl_record:set_server_verify_data(current_both, Data, ConnectionStates);
save_verify_data(client, #finished{verify_data = Data}, ConnectionStates, abbreviated) ->
    ssl_record:set_client_verify_data(current_both, Data, ConnectionStates);
save_verify_data(server, #finished{verify_data = Data}, ConnectionStates, abbreviated) ->
    ssl_record:set_server_verify_data(current_write, Data, ConnectionStates).

handle_server_key(
  #server_key_exchange{params = 
		       #server_dh_params{dh_p = P,
					 dh_g = G,
					 dh_y = ServerPublicDhKey}, 
		       signed_params = Signed}, 
  #state{session = Session, negotiated_version = Version, role = Role,
	 public_key_info = PubKeyInfo,
	 key_algorithm = KeyAlgo,
	 connection_states = ConnectionStates0} = State) ->
     
    PLen = size(P),
    GLen = size(G),
    YLen = size(ServerPublicDhKey),

    ConnectionState = 
	ssl_record:pending_connection_state(ConnectionStates0, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams, 
    Hash = ssl_handshake:server_key_exchange_hash(KeyAlgo,
						  <<ClientRandom/binary, 
						   ServerRandom/binary, 
						   ?UINT16(PLen), P/binary, 
						   ?UINT16(GLen), G/binary,
						   ?UINT16(YLen),
						   ServerPublicDhKey/binary>>),
    
    case verify_dh_params(Signed, Hash, PubKeyInfo) of
	true ->
	    PMpint = mpint_binary(P),
	    GMpint = mpint_binary(G),	
	    Keys = {_, ClientDhPrivateKey} = 
		crypto:dh_generate_key([PMpint,GMpint]),
	    PremasterSecret = 
		crypto:dh_compute_key(mpint_binary(ServerPublicDhKey), 
				      ClientDhPrivateKey, [PMpint, GMpint]),
	    case ssl_handshake:master_secret(Version, PremasterSecret, 
					     ConnectionStates0, Role) of
		{MasterSecret, ConnectionStates} ->
		    State#state{diffie_hellman_keys = Keys,
				 session = 
				 Session#session{master_secret 
						 = MasterSecret},
				 connection_states = ConnectionStates};
		#alert{} = Alert ->
		    Alert
	    end;
	false ->
	    ?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE)
    end.


verify_dh_params(Signed, Hashes, {?rsaEncryption, PubKey, _PubKeyParams}) ->
    case public_key:decrypt_public(Signed, PubKey, 
				   [{rsa_pad, rsa_pkcs1_padding}]) of
	Hashes ->
	    true;
	_ ->
	    false
    end;
verify_dh_params(Signed, Hash, {?'id-dsa', PublicKey, PublicKeyParams}) ->
    public_key:verify_signature(Hash, none, Signed, PublicKey, PublicKeyParams). 


encode_alert(#alert{} = Alert, Version, ConnectionStates) ->
    ?DBG_TERM(Alert),
    ssl_record:encode_alert_record(Alert, Version, ConnectionStates).

encode_change_cipher(#change_cipher_spec{}, Version, ConnectionStates) ->
    ?DBG_TERM(#change_cipher_spec{}),
    ssl_record:encode_change_cipher_spec(Version, ConnectionStates).

encode_handshake(HandshakeRec, Version, ConnectionStates, Hashes) ->
    encode_handshake(HandshakeRec, undefined, Version, 
		     ConnectionStates, Hashes).

encode_handshake(HandshakeRec, SigAlg, Version, ConnectionStates0, Hashes0) ->
    ?DBG_TERM(HandshakeRec),
    Frag = ssl_handshake:encode_handshake(HandshakeRec, Version, SigAlg),
    Hashes1 = ssl_handshake:update_hashes(Hashes0, Frag),
    {E, ConnectionStates1} =
        ssl_record:encode_handshake(Frag, Version, ConnectionStates0),
    {E, ConnectionStates1, Hashes1}.

encode_packet(Data, #socket_options{packet=Packet}) ->
    case Packet of
	1 -> encode_size_packet(Data, 8,  (1 bsl 8) - 1);
	2 -> encode_size_packet(Data, 16, (1 bsl 16) - 1);
	4 -> encode_size_packet(Data, 32, (1 bsl 32) - 1);
	_ -> Data
    end.

encode_size_packet(Bin, Size, Max) ->
    Len = byte_size(Bin),
    case Len > Max of
	true  -> throw({error, {badarg, {packet_to_large, Len, Max}}});
	false -> <<Len:Size, Bin/binary>>
    end.

encode_data(Data, Version, ConnectionStates, RenegotiateAt) ->
    ssl_record:encode_data(Data, Version, ConnectionStates, RenegotiateAt).

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
	    next_state(StateName, Record, State);
	_ ->
	    case application_data(<<>>, State0) of
		Stop = {stop, _, _} ->
		    Stop;
		{Record, State} ->
		    next_state(StateName, Record, State)
	    end
    end.

application_data(Data, #state{user_application = {_Mon, Pid},
                              socket_options = SOpts,
                              bytes_to_read = BytesToRead,
                              from = From,
                              user_data_buffer = Buffer0} = State0) ->
    Buffer1 = if 
		  Buffer0 =:= <<>> -> Data;
		  Data =:= <<>> -> Buffer0;
		  true -> <<Buffer0/binary, Data/binary>>
	      end,
    case get_data(SOpts, BytesToRead, Buffer1) of
	{ok, <<>>, Buffer} -> % no reply, we need more data
	    next_record(State0#state{user_data_buffer = Buffer});
	{ok, ClientData, Buffer} -> % Send data
	    SocketOpt = deliver_app_data(SOpts, ClientData, Pid, From),
	    State = State0#state{user_data_buffer = Buffer,
				 from = undefined,
				 bytes_to_read = 0,
				 socket_options = SocketOpt 
				},
	    if
		SocketOpt#socket_options.active =:= false; Buffer =:= <<>> -> 
		    %% Passive mode, wait for active once or recv
		    %% Active and empty, get more data
		    next_record_if_active(State);
	 	true -> %% We have more data
 		    application_data(<<>>, State)
	    end;
	{error,_Reason} -> %% Invalid packet in packet mode
	    deliver_packet_error(SOpts, Buffer1, Pid, From),
	    {stop, normal, State0}
    end.

%% Picks ClientData 
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
	    {ok, <<>>, Buffer}
    end;
get_data(#socket_options{packet=Type, packet_size=Size}, _, Buffer) ->
    PacketOpts = [{packet_size, Size}], 
    case decode_packet(Type, Buffer, PacketOpts) of
	{more, _} ->
	    {ok, <<>>, Buffer};
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
deliver_app_data(SOpts = #socket_options{active=Active, packet=Type},
			Data, Pid, From) ->
    send_or_reply(Active, Pid, From, format_reply(SOpts, Data)),
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

format_reply(#socket_options{active = false, mode = Mode, packet = Packet,
			     header = Header}, Data) ->
    {ok, format_reply(Mode, Packet, Header, Data)};
format_reply(#socket_options{active = _, mode = Mode, packet = Packet, 
			     header = Header}, Data) ->
    {ssl, sslsocket(), format_reply(Mode, Packet, Header, Data)}.

deliver_packet_error(SO= #socket_options{active = Active}, Data, Pid, From) ->
    send_or_reply(Active, Pid, From, format_packet_error(SO, Data)).

format_packet_error(#socket_options{active = false, mode = Mode}, Data) ->
    {error, {invalid_packet, format_reply(Mode, raw, 0, Data)}};
format_packet_error(#socket_options{active = _, mode = Mode}, Data) ->
    {ssl_error, sslsocket(), {invalid_packet, format_reply(Mode, raw, 0, Data)}}.

format_reply(binary, _, N, Data) when N > 0 ->  % Header mode
    header(N, Data);
format_reply(binary, _, _, Data) ->  Data;
format_reply(list, Packet, _, Data) when is_integer(Packet); Packet == raw -> 
    binary_to_list(Data);
format_reply(list, _,_, Data) ->
    Data.

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
send_or_reply(_, Pid, _From, Data) ->
    send_user(Pid, Data).

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.

send_user(Pid, Msg) ->
    Pid ! Msg.

handle_tls_handshake(Handle, StateName, #state{tls_packets = [Packet]} = State) ->
    FsmReturn = {next_state, StateName, State#state{tls_packets = []}},
    Handle(Packet, FsmReturn);

handle_tls_handshake(Handle, StateName, #state{tls_packets = [Packet | Packets]} = State0) ->
    FsmReturn = {next_state, StateName, State0#state{tls_packets = Packets}},
    case Handle(Packet, FsmReturn) of
	{next_state, NextStateName, State} ->
	    handle_tls_handshake(Handle, NextStateName, State);
	{stop, _,_} = Stop ->
	    Stop
    end.

next_state(_, #alert{} = Alert, #state{negotiated_version = Version} = State) ->
    handle_own_alert(Alert, Version, decipher_error, State),
    {stop, normal, State};

next_state(Next, no_record, State) ->
    {next_state, Next, State};

next_state(Next, #ssl_tls{type = ?ALERT, fragment = EncAlerts}, State) ->
    Alerts = decode_alerts(EncAlerts),
    handle_alerts(Alerts,  {next_state, Next, State});

next_state(StateName, #ssl_tls{type = ?HANDSHAKE, fragment = Data},
  	   State0 = #state{key_algorithm = KeyAlg,
  			   tls_handshake_buffer = Buf0,
  			   negotiated_version = Version}) ->
    Handle = 
   	fun({#hello_request{} = Packet, _}, {next_state, connection = SName, State}) ->
   		%% This message should not be included in handshake
   		%% message hashes. Starts new handshake (renegotiation)
   		Hs0 = ssl_handshake:init_hashes(),
   		?MODULE:SName(Packet, State#state{tls_handshake_hashes=Hs0,
   						  renegotiation = {true, peer}});
   	   ({#hello_request{} = Packet, _}, {next_state, SName, State}) ->
   		%% This message should not be included in handshake
   		%% message hashes. Already in negotiation so it will be ignored!
   		?MODULE:SName(Packet, State);
	   ({#client_hello{} = Packet, Raw}, {next_state, connection = SName, State}) ->
   		Hs0 = ssl_handshake:init_hashes(),
   		Hs1 = ssl_handshake:update_hashes(Hs0, Raw),
   		?MODULE:SName(Packet, State#state{tls_handshake_hashes=Hs1,
   						  renegotiation = {true, peer}});
 	   ({Packet, Raw}, {next_state, SName, State = #state{tls_handshake_hashes=Hs0}}) ->
   		Hs1 = ssl_handshake:update_hashes(Hs0, Raw),
   		?MODULE:SName(Packet, State#state{tls_handshake_hashes=Hs1});
   	   (_, StopState) -> StopState
   	end,
    try
   	{Packets, Buf} = ssl_handshake:get_tls_handshake(Data,Buf0, KeyAlg,Version),
	State = State0#state{tls_packets = Packets, tls_handshake_buffer = Buf},
	handle_tls_handshake(Handle, StateName, State)
    catch throw:#alert{} = Alert ->
   	    handle_own_alert(Alert, Version, StateName, State0), 
   	    {stop, normal, State0}
    end;

next_state(StateName, #ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, State0) ->
    case application_data(Data, State0) of
	Stop = {stop,_,_} ->
   	    Stop;
	 {Record, State} ->
   	    next_state(StateName, Record, State)
    end;
next_state(StateName, #ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = <<1>>} = 
 	   _ChangeCipher, 
 	   #state{connection_states = ConnectionStates0} = State0) ->
    ?DBG_TERM(_ChangeCipher),
    ConnectionStates1 =
 	ssl_record:activate_pending_connection_state(ConnectionStates0, read),
    {Record, State} = next_record(State0#state{connection_states = ConnectionStates1}),
    next_state(StateName, Record, State);
next_state(StateName, #ssl_tls{type = _Unknown}, State0) ->
    %% Ignore unknown type 
    {Record, State} = next_record(State0),
    next_state(StateName, Record, State).

next_tls_record(Data, #state{tls_record_buffer = Buf0,
		       tls_cipher_texts = CT0} = State0) ->
    case ssl_record:get_tls_records(Data, Buf0) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{tls_record_buffer = Buf1,
				     tls_cipher_texts = CT1});
	#alert{} = Alert ->
	    Alert
    end.

next_record(#state{tls_packets = [], tls_cipher_texts = [], socket = Socket} = State) ->
    inet:setopts(Socket, [{active,once}]),
    {no_record, State};
next_record(#state{tls_packets = [], tls_cipher_texts = [CT | Rest],
		   connection_states = ConnStates0} = State) ->
    case ssl_record:decode_cipher_text(CT, ConnStates0) of
	{Plain, ConnStates} ->		      
	    {Plain, State#state{tls_cipher_texts = Rest, connection_states = ConnStates}};
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
					connection_states = ConnectionStates0,
					ssl_options = #ssl_options{renegotiate_at = RenegotiateAt}
				       } = State) ->     
    %% Send queued up data
    case queue:out(Queue0) of
	{{value, {From, Data}}, Queue} ->
	    case encode_data(Data, Version, ConnectionStates0, RenegotiateAt) of
		{Msgs, [], ConnectionStates} ->
		    Result = Transport:send(Socket, Msgs),
		    gen_fsm:reply(From, Result),
		    next_state_connection(StateName,
					  State#state{connection_states = ConnectionStates,
						      send_queue = Queue});
		%% This is unlikely to happen. User configuration of the 
		%% undocumented test option renegotiation_at can make it more likely.
		{Msgs, RestData, ConnectionStates} ->
		    if 
			Msgs =/= [] -> 
			    Transport:send(Socket, Msgs);
			true ->
			    ok
		    end,
		    renegotiate(State#state{connection_states = ConnectionStates,
					    send_queue = queue:in_r({From, RestData}, Queue),
					    renegotiation = {true, internal}})
	    end;
	{empty, Queue0} ->
	    next_state_is_connection(State)
    end.

next_state_is_connection(State = 
		      #state{recv_during_renegotiation = true, socket_options = 
			     #socket_options{active = false}})  -> 
    passive_receive(State#state{recv_during_renegotiation = false}, connection);

next_state_is_connection(State0) ->
    {Record, State} = next_record_if_active(State0),
    next_state(connection, Record, State).

register_session(_, _, _, #session{is_resumable = true} = Session) ->
    Session; %% Already registered
register_session(client, Host, Port, Session0) ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(Host, Port, Session),
    Session;
register_session(server, _, Port, Session0) ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(Port, Session),
    Session.

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
	   session = #session{is_resumable = false},
	   transport_cb = CbModule,
	   data_tag = DataTag,
	   close_tag = CloseTag,
	   error_tag = ErrorTag,
	   role = Role,
	   host = Host,
	   port = Port,
	   socket = Socket,
	   connection_states = ConnectionStates,
	   tls_handshake_buffer = <<>>,
	   tls_record_buffer = <<>>,
	   tls_cipher_texts = [],
	   user_application = {Monitor, User},
	   bytes_to_read = 0,
	   user_data_buffer = <<>>,
	   log_alert = true,
	   session_cache_cb = SessionCacheCb,
	   renegotiation = {false, first},
	   recv_during_renegotiation = false,
	   send_queue = queue:new()
	  }.

sslsocket(Pid) ->
    #sslsocket{pid = Pid, fd = new_ssl}.

sslsocket() ->
    sslsocket(self()).

get_socket_opts(_,[], _, Acc) ->
    {ok, Acc};
get_socket_opts(Socket, [mode | Tags], SockOpts, Acc) ->
    get_socket_opts(Socket, Tags, SockOpts, 
		    [{mode, SockOpts#socket_options.mode} | Acc]);
get_socket_opts(Socket, [packet | Tags], SockOpts, Acc) ->
    case SockOpts#socket_options.packet of
	{Type, headers} ->
	    get_socket_opts(Socket, Tags, SockOpts, [{packet, Type} | Acc]);
	Type ->
	    get_socket_opts(Socket, Tags, SockOpts, [{packet, Type} | Acc])
    end;
get_socket_opts(Socket, [header | Tags], SockOpts, Acc) ->
    get_socket_opts(Socket, Tags, SockOpts, 
		    [{header, SockOpts#socket_options.header} | Acc]);
get_socket_opts(Socket, [active | Tags], SockOpts, Acc) ->
    get_socket_opts(Socket, Tags, SockOpts, 
		    [{active, SockOpts#socket_options.active} | Acc]);
get_socket_opts(Socket, [Tag | Tags], SockOpts, Acc) ->
    case inet:getopts(Socket, [Tag]) of
	{ok, [Opt]} ->
	    get_socket_opts(Socket, Tags, SockOpts, [Opt | Acc]);
	{error, Error} ->
	    {error, Error}
    end.

set_socket_opts(_, [], SockOpts, []) ->
    SockOpts;
set_socket_opts(Socket, [], SockOpts, Other) ->
    %% Set non emulated options 
    inet:setopts(Socket, Other),
    SockOpts;
set_socket_opts(Socket, [{mode, Mode}| Opts], SockOpts, Other) ->
    set_socket_opts(Socket, Opts, 
		    SockOpts#socket_options{mode = Mode}, Other);
set_socket_opts(Socket, [{packet, Packet}| Opts], SockOpts, Other) ->
    set_socket_opts(Socket, Opts, 
		    SockOpts#socket_options{packet = Packet}, Other);
set_socket_opts(Socket, [{header, Header}| Opts], SockOpts, Other) ->
    set_socket_opts(Socket, Opts, 
		    SockOpts#socket_options{header = Header}, Other);
set_socket_opts(Socket, [{active, Active}| Opts], SockOpts, Other) ->
    set_socket_opts(Socket, Opts, 
		    SockOpts#socket_options{active = Active}, Other);
set_socket_opts(Socket, [Opt | Opts], SockOpts, Other) ->
    set_socket_opts(Socket, Opts, SockOpts, [Opt | Other]).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop, _, _} = Stop) ->
    %% If it is a fatal alert immediately close 
    Stop;
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
    handle_alerts(Alerts, handle_alert(Alert, StateName, State)).

handle_alert(#alert{level = ?FATAL} = Alert, StateName,
	     #state{from = From, host = Host, port = Port, session = Session,
		    user_application = {_Mon, Pid},
		    log_alert = Log, role = Role, socket_options = Opts} = State) ->
    invalidate_session(Role, Host, Port, Session),
    log_alert(Log, StateName, Alert),
    alert_user(StateName, Opts, Pid, From, Alert, Role),
    {stop, normal, State};

handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert, 
	     StateName, #state{from = From, role = Role,  
			       user_application = {_Mon, Pid}, socket_options = Opts} = State) -> 
    alert_user(StateName, Opts, Pid, From, Alert, Role),
    {stop, normal, State};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{log_alert = Log, renegotiation = {true, internal}, from = From,
		    role = Role} = State) ->
    log_alert(Log, StateName, Alert),
    alert_user(From, Alert, Role),
    {stop, normal, State};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{log_alert = Log, renegotiation = {true, From}} = State0) ->
    log_alert(Log, StateName, Alert),
    gen_fsm:reply(From, {error, renegotiation_rejected}),
    {Record, State} = next_record(State0),
    next_state(connection, Record, State);

handle_alert(#alert{level = ?WARNING, description = ?USER_CANCELED} = Alert, StateName, 
	     #state{log_alert = Log} = State0) ->
    log_alert(Log, StateName, Alert),
    {Record, State} = next_record(State0),
    next_state(StateName, Record, State).

alert_user(connection, Opts, Pid, From, Alert, Role) ->
    alert_user(Opts#socket_options.active, Pid, From, Alert, Role);
alert_user(_, _, _, From, Alert, Role) ->
    alert_user(From, Alert, Role).

alert_user(From, Alert, Role) ->
    alert_user(false, no_pid, From, Alert, Role).

alert_user(false = Active, Pid, From,  Alert, Role) ->
    %% If there is an outstanding ssl_accept | recv
    %% From will be defined and send_or_reply will
    %% send the appropriate error message.
    ReasonCode = ssl_alert:reason_code(Alert, Role),
    send_or_reply(Active, Pid, From, {error, ReasonCode});
alert_user(Active, Pid, From, Alert, Role) ->
    case ssl_alert:reason_code(Alert, Role) of
	closed ->
	    send_or_reply(Active, Pid, From,
			  {ssl_closed, sslsocket()});
	ReasonCode ->
	    send_or_reply(Active, Pid, From,
			  {ssl_error, sslsocket(), ReasonCode})
    end.

log_alert(true, Info, Alert) ->
    Txt = ssl_alert:alert_txt(Alert),
    error_logger:format("SSL: ~p: ~s\n", [Info, Txt]);
log_alert(false, _, _) ->
    ok.

handle_own_alert(Alert, Version, Info, 
		 #state{transport_cb = Transport,
			socket = Socket,
			from = User,
			role = Role,
			connection_states = ConnectionStates,
			log_alert = Log}) ->
    try %% Try to tell the other side
	{BinMsg, _} =
	encode_alert(Alert, Version, ConnectionStates),
	linux_workaround_transport_delivery_problems(Alert, Socket),
	Transport:send(Socket, BinMsg)
    catch _:_ ->  %% Can crash if we are in a uninitialized state
	    ignore
    end,
    try %% Try to tell the local user
	log_alert(Log, Info, Alert),
	alert_user(User, Alert, Role)
    catch _:_ ->
	    ok
    end.

handle_unexpected_message(Msg, Info, #state{negotiated_version = Version} = State) ->
    Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE),
    handle_own_alert(Alert, Version, {Info, Msg}, State),
    {stop, normal, State}.

make_premaster_secret({MajVer, MinVer}, rsa) ->
    Rand = crypto:rand_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    <<?BYTE(MajVer), ?BYTE(MinVer), Rand/binary>>;
make_premaster_secret(_, _) ->
    undefined.

mpint_binary(Binary)  ->
    Size = byte_size(Binary),
    <<?UINT32(Size), Binary/binary>>.


ack_connection(#state{renegotiation = {true, Initiater}} = State) 
  when Initiater == internal;
       Initiater == peer ->
    State#state{renegotiation = undefined};
ack_connection(#state{renegotiation = {true, From}} = State) ->    
    gen_fsm:reply(From, ok),
    State#state{renegotiation = undefined};
ack_connection(#state{renegotiation = {false, first}, 
				  from = From} = State) when From =/= undefined ->
    gen_fsm:reply(From, connected),
    State#state{renegotiation = undefined};
ack_connection(State) ->
    State.

renegotiate(#state{role = client} = State) ->
    %% Handle same way as if server requested
    %% the renegotiation
    Hs0 = ssl_handshake:init_hashes(),
    connection(#hello_request{}, State#state{tls_handshake_hashes = Hs0});  
renegotiate(#state{role = server,
		   socket = Socket,
		   transport_cb = Transport,
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State0) ->
    HelloRequest = ssl_handshake:hello_request(),
    Frag = ssl_handshake:encode_handshake(HelloRequest, Version, undefined),
    Hs0 = ssl_handshake:init_hashes(),
    {BinMsg, ConnectionStates} = 
	ssl_record:encode_handshake(Frag, Version, ConnectionStates0),
    Transport:send(Socket, BinMsg),
    {Record, State} = next_record(State0#state{connection_states = 
					       ConnectionStates,
					       tls_handshake_hashes = Hs0}),
    next_state(hello, Record, State).

notify_senders(SendQueue) -> 
    lists:foreach(fun({From, _}) ->
 			  gen_fsm:reply(From, {error, closed})
 		  end, queue:to_list(SendQueue)).

notify_renegotiater({true, From}) when not is_atom(From)  ->
    gen_fsm:reply(From, {error, closed});
notify_renegotiater(_) ->
    ok.

workaround_transport_delivery_problems(Socket, Transport) ->
    %% Standard trick to try to make sure all
    %% data sent to to tcp port is really sent
    %% before tcp port is closed.
    inet:setopts(Socket, [{active, false}]),
    Transport:shutdown(Socket, write),
    Transport:recv(Socket, 0).

linux_workaround_transport_delivery_problems(#alert{level = ?FATAL}, Socket) ->
    case os:type() of
	{unix, linux} ->
	    inet:setopts(Socket, [{nodelay, true}]);
	_ ->
	    ok
    end;
linux_workaround_transport_delivery_problems(_, _) ->
    ok.
