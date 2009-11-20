%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
-export([send/2, send/3, recv/3, connect/7, accept/6, close/1, shutdown/2,
	 new_user/2, get_opts/2, set_opts/2, info/1, session_info/1, 
	 peer_certificate/1,
	 sockname/1, peername/1]).

%% Called by ssl_connection_sup
-export([start_link/7]). 

%% gen_fsm callbacks
-export([init/1, hello/2, certify/2, cipher/2, connection/2, connection/3, abbreviated/2,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
          role,               % client | server
          user_application,   % {MonitorRef, pid()} 
          transport_cb,       % atom() - callback module 
          data_tag,           % atom()  - ex tcp.
	  close_tag,          % atom()  - ex tcp_closed
          host,               % string() | ipadress()
          port,               % integer()
          socket,             % socket() 
          ssl_options,        % #ssl_options{}
          socket_options,     % #socket_options{}
          connection_states,  % #connection_states{} from ssl_record.hrl
          tls_record_buffer,  % binary() buffer of incomplete records
          tls_handshake_buffer, % binary() buffer of incomplete handshakes
	  %% {{md5_hash, sha_hash}, {prev_md5, prev_sha}} (binary())
          tls_handshake_hashes, % see above 
          tls_cipher_texts,     % list() received but not deciphered yet
          own_cert,             % binary()  
          session,              % #session{} from ssl_handshake.erl
	  session_cache,        % 
	  session_cache_cb,     %
          negotiated_version,   % #protocol_version{}
          supported_protocol_versions, % [atom()]
          client_certificate_requested = false,
	  key_algorithm,       % atom as defined by cipher_suite
          public_key_info,     % PKIX: {Algorithm, PublicKey, PublicKeyParams}
          private_key,         % PKIX: 'RSAPrivateKey'
	  diffie_hellman_params, % 
          premaster_secret,    %
          cert_db_ref,         % ets_table()
          from,                % term(), where to reply
          bytes_to_read,       % integer(), # bytes to read in passive mode
          user_data_buffer,    % binary()
%%	  tls_buffer,          % Keeps a lookahead one packet if available
	  log_alert            % boolan() 
	 }).

%%====================================================================
%% Internal application API
%%====================================================================	     

%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
send(Pid, Data) -> 
    sync_send_event(Pid, {application_data, erlang:iolist_to_binary(Data)}, infinity).
send(Pid, Data, Timeout) -> 
    sync_send_event(Pid, {application_data, erlang:iolist_to_binary(Data)}, Timeout).
%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
recv(Pid, Length, Timeout) -> % TODO: Prio with renegotiate? 
    sync_send_all_state_event(Pid, {recv, Length}, Timeout).
%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
connect(Host, Port, Socket, Options, User, CbInfo, Timeout) ->
    start_fsm(client, Host, Port, Socket, Options, User, CbInfo,
	      Timeout).
%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
accept(Port, Socket, Opts, User, CbInfo, Timeout) ->
    start_fsm(server, "localhost", Port, Socket, Opts, User, 
	      CbInfo, Timeout).
%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
close(ConnectionPid) ->
    case sync_send_all_state_event(ConnectionPid, close) of
	{error, closed} ->
	    ok;
	Other ->
	    Other
    end.

%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
shutdown(ConnectionPid, How) ->
    sync_send_all_state_event(ConnectionPid, {shutdown, How}).


%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
new_user(ConnectionPid, User) ->
    sync_send_all_state_event(ConnectionPid, {new_user, User}).
%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
sockname(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, sockname).
%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
peername(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, peername).
%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
get_opts({ListenSocket, {_SslOpts, SockOpts}, _}, OptTags) ->
    get_socket_opts(ListenSocket, OptTags, SockOpts, []);
get_opts(ConnectionPid, OptTags) ->
    sync_send_all_state_event(ConnectionPid, {get_opts, OptTags}).
%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
set_opts(ConnectionPid, Options) ->
    sync_send_all_state_event(ConnectionPid, {set_opts, Options}).

%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
info(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, info). 

%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
session_info(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, session_info). 

%%--------------------------------------------------------------------
%% Function: 
%%
%% Description: 
%%--------------------------------------------------------------------
peer_certificate(ConnectionPid) ->
    sync_send_all_state_event(ConnectionPid, peer_certificate). 

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
init([Role, Host, Port, Socket, {SSLOpts, _} = Options, 
      User, CbInfo]) ->
    State0 = initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
    Hashes0 = ssl_handshake:init_hashes(),    

    try ssl_init(SSLOpts, Role) of
	{ok, Ref, CacheRef, OwnCert, Key} ->	   
	    State = State0#state{tls_handshake_hashes = Hashes0,
				 own_cert = OwnCert,
				 cert_db_ref = Ref,
				 session_cache = CacheRef,
				 private_key = Key},
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
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
hello(socket_control, #state{host = Host, port = Port, role = client,
			     ssl_options = SslOpts, 
			     transport_cb = Transport, socket = Socket,
			     connection_states = ConnectionStates}
      = State0) ->
    Hello = ssl_handshake:client_hello(Host, Port, ConnectionStates, SslOpts),
    Version = Hello#client_hello.client_version,
    Hashes0 = ssl_handshake:init_hashes(),
    {BinMsg, CS2, Hashes1} = 
        encode_handshake(Hello, Version, ConnectionStates, Hashes0),
    Transport:send(Socket, BinMsg),
    State = State0#state{connection_states = CS2,
			 negotiated_version = Version, %% Requested version
			 session = 
                         #session{session_id = Hello#client_hello.session_id,
                                  is_resumable = false},
                         tls_handshake_hashes = Hashes1},
    {next_state, hello, next_record(State)};
    
hello(socket_control, #state{role = server} = State) ->
    {next_state, hello, next_record(State)};

hello(hello, #state{role = client} = State) ->
    {next_state, hello, State};

hello(#server_hello{cipher_suite = CipherSuite,
		    compression_method = Compression} = Hello,
      #state{session = Session0 = #session{session_id = OldId},
	     connection_states = ConnectionStates0,
	     role = client,
	     negotiated_version = ReqVersion,
	     host = Host, port = Port,
	     session_cache = Cache,
	     session_cache_cb = CacheCb} = State0) ->
    {Version, NewId, ConnectionStates1} =
        ssl_handshake:hello(Hello, ConnectionStates0),

    {KeyAlgorithm, _, _, _} = 
        ssl_cipher:suite_definition(CipherSuite),
    
    PremasterSecret = make_premaster_secret(ReqVersion),
    
    State = State0#state{key_algorithm = KeyAlgorithm,
			 negotiated_version = Version,
			 connection_states = ConnectionStates1,
			 premaster_secret = PremasterSecret},

    case ssl_session:is_new(OldId, NewId) of
        true ->
	    Session = Session0#session{session_id = NewId,
				       cipher_suite = CipherSuite,
				       compression_method = Compression}, 
            {next_state, certify, 
             next_record(State#state{session = Session})};
        false ->
	    Session = CacheCb:lookup(Cache, {{Host, Port}, NewId}),
	    case ssl_handshake:master_secret(Version, Session, 
					     ConnectionStates1, client) of
		{_, ConnectionStates2} ->	
		    {next_state, abbreviated,
		     next_record(State#state{
				   connection_states = ConnectionStates2,
				   session = Session})};
		#alert{} = Alert ->
		    handle_own_alert(Alert, Version, hello, State), 
		    {stop, normal, State}
	    end
    end;

hello(Hello = #client_hello{client_version = ClientVersion}, 
      State = #state{connection_states = ConnectionStates0,
		     port = Port, session = Session0,
		     session_cache = Cache,
		     session_cache_cb = CacheCb,
		     ssl_options = SslOpts}) ->
    
    case ssl_handshake:hello(Hello, {Port, SslOpts,  
				     Session0, Cache, CacheCb,
				     ConnectionStates0}) of
        {Version, {Type, Session}, ConnectionStates} ->       
            do_server_hello(Type, State#state{connection_states  = 
					      ConnectionStates,
					      negotiated_version = Version,
					      session = Session});
        #alert{} = Alert ->
            handle_own_alert(Alert, ClientVersion, hello, State), 
            {stop, normal, State}
    end.

abbreviated(socket_control, #state{role = server} = State) ->
    {next_state, abbreviated, State};
abbreviated(hello, State) ->
    {next_state, certify, State};

abbreviated(Finished = #finished{},
	    #state{role = server,
		   negotiated_version = Version,
		   tls_handshake_hashes = Hashes,
		   session = #session{master_secret = MasterSecret},
		   from = From} = State) ->
    case ssl_handshake:verify_connection(Version, Finished, client,
					 MasterSecret, Hashes) of
        verified ->
            gen_fsm:reply(From, connected),
	    {next_state, connection, next_record_if_active(State)};
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, abbreviated, State),
            {stop, normal, State} 
    end;

abbreviated(Finished = #finished{},
	    #state{role = client, tls_handshake_hashes = Hashes0,
		   session = #session{master_secret = MasterSecret},
		   from = From,
		   negotiated_version = Version} = State) ->
    case ssl_handshake:verify_connection(Version, Finished, server,
					 MasterSecret, Hashes0) of
        verified ->
	    {ConnectionStates, Hashes} = finalize_client_handshake(State),
            gen_fsm:reply(From, connected),
	    {next_state, connection,
	     next_record_if_active(State#state{tls_handshake_hashes = Hashes,
					       connection_states = 
					       ConnectionStates})};
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, abbreviated, State),
            {stop, normal, State} 
    end.

certify(socket_control, #state{role = server} = State) ->
    {next_state, certify, State};
certify(hello, State) ->
    {next_state, certify, State};

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
	State) ->
    {next_state, certify, next_record(State#state{client_certificate_requested = false})};

certify(#certificate{} = Cert, 
        #state{session = Session, 
	       negotiated_version = Version,
	       cert_db_ref = CertDbRef,
	       ssl_options = Opts} = State0) ->
    case ssl_handshake:certify(Cert, CertDbRef, Opts#ssl_options.depth, 
			       Opts#ssl_options.verify,
			       Opts#ssl_options.verify_fun) of
        {PeerCert, PublicKeyInfo} ->
            State = State0#state{session = 
                                 Session#session{peer_certificate = PeerCert},				 
                                 public_key_info = PublicKeyInfo,
				 client_certificate_requested = false
				},
            {next_state, certify, next_record(State)};
	#alert{} = Alert ->
            handle_own_alert(Alert, Version, certify_certificate, State0),
            {stop, normal, State0}
    end;

certify(#server_key_exchange{} = KeyExchangeMsg, 
        #state{role = client,
	       key_algorithm = Alg} = State) 
  when Alg == dhe_dss; Alg == dhe_rsa; Alg == dh_anon; Alg == krb5 ->
    NewState = handle_server_key(KeyExchangeMsg, State),
    {next_state, certify, NewState};

certify(#server_key_exchange{}, 
        State = #state{role = client, negotiated_version = Version,
                       key_algorithm = Alg}) 
  when Alg == rsa; Alg == dh_dss; Alg == dh_rsa ->
    Alert = ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE),
    handle_own_alert(Alert, Version, certify_server_key_exchange, State),
    {stop, normal, State};

certify(KeyExchangeMsg = #server_key_exchange{}, State = 
        #state{role = server}) ->
    NewState = handle_clinet_key(KeyExchangeMsg, State),
    {next_state, cipher, NewState};

certify(#certificate_request{}, State) ->
    NewState = State#state{client_certificate_requested = true},
    {next_state, certify, next_record(NewState)};

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

certify(#client_key_exchange{},
	State = #state{role = server,
		       client_certificate_requested = true,
		       ssl_options = #ssl_options{fail_if_no_peer_cert = true},
		       negotiated_version = Version}) ->
    %% We expect a certificate here
    Alert = ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE),
    handle_own_alert(Alert, Version, certify_server_waiting_certificate, State),
    {stop, normal, State};


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
		    State = State0#state{connection_states = ConnectionStates,
					 session = Session},
		    {next_state, cipher, next_record(State)};
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
    end.

cipher(socket_control, #state{role = server} = State) ->
    {next_state, cipher, State};
cipher(hello, State) ->
    {next_state, cipher, State};

cipher(#certificate_verify{signature = Signature}, 
       #state{role = server, 
	      public_key_info = PublicKeyInfo,
	      negotiated_version = Version,
	      session = #session{master_secret = MasterSecret},
	      key_algorithm = Algorithm,
	      tls_handshake_hashes = Hashes
	     } = State) -> 
    case ssl_handshake:certificate_verify(Signature, PublicKeyInfo,
					  Version, MasterSecret, 
					  Algorithm, Hashes) of
	valid ->
	    {next_state, cipher, next_record(State)};
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, cipher, State), 
	    {stop, normal, State}
    end;

cipher(#finished{} = Finished, 
       State = #state{from = From,
                      negotiated_version = Version,
		      host = Host,
		      port = Port,
		      role = Role,
		      session = #session{master_secret = MasterSecret} 
		      = Session0,
                      tls_handshake_hashes = Hashes}) ->    

    case ssl_handshake:verify_connection(Version, Finished, 
					 opposite_role(Role), 
                                         MasterSecret, Hashes) of
        verified ->
            gen_fsm:reply(From, connected),
	    Session = register_session(Role, Host, Port, Session0),
            case Role of
                client ->
                    {next_state, connection, 
		     next_record_if_active(State#state{session = Session})};
                server ->
                    {NewConnectionStates, NewHashes} = 
                        finalize_server_handshake(State#state{
						    session = Session}),
                    NewState = 
                        State#state{connection_states = NewConnectionStates,
				    session = Session,
                                    tls_handshake_hashes = NewHashes},
                    {next_state, connection, next_record_if_active(NewState)}
            end;
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, cipher, State),
            {stop, normal, State} 
    end.

connection(socket_control, #state{role = server} = State) ->
    {next_state, connection, State};
connection(hello, State = #state{host = Host, port = Port,
                                 socket = Socket,
				 ssl_options = SslOpts,
                                 negotiated_version = Version,
                                 transport_cb = Transport,
                                 connection_states = ConnectionStates0,
                                 tls_handshake_hashes = Hashes0}) ->

    Hello = ssl_handshake:client_hello(Host, Port, 
				       ConnectionStates0, SslOpts),
    {BinMsg, ConnectionStates1, Hashes1} =
        encode_handshake(Hello, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    {next_state, hello, State#state{connection_states = ConnectionStates1,
                                    tls_handshake_hashes = Hashes1}}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
connection({application_data, Data}, _From, 
           State = #state{socket = Socket,
                          negotiated_version = Version,
                          transport_cb = Transport,
                          connection_states = ConnectionStates0}) ->
    %% We should look into having a worker process to do this to 
    %% parallize send and receive decoding and not block the receiver
    %% if sending is overloading the socket.
    {Msgs, ConnectionStates1} = encode_data(Data, Version, ConnectionStates0),
    Result = Transport:send(Socket, Msgs),
    {reply, Result, 
     connection, State#state{connection_states = ConnectionStates1}}.

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
handle_event(#ssl_tls{type = ?HANDSHAKE, fragment = Data},
	     StateName,
	     State = #state{key_algorithm = KeyAlg,
			    tls_handshake_buffer = Buf0,
			    negotiated_version = Version}) ->
    Handle = 
	fun({Packet, Raw}, {next_state, SName, AS=#state{tls_handshake_hashes=Hs0}}) ->		
		Hs1 = ssl_handshake:update_hashes(Hs0, Raw),
		?MODULE:SName(Packet, AS#state{tls_handshake_hashes=Hs1});
	   (_, StopState) -> StopState
	end,
    try
	{Packets, Buf} = ssl_handshake:get_tls_handshake(Data,Buf0, KeyAlg,Version),
	Start = {next_state, StateName, State#state{tls_handshake_buffer = Buf}},
	lists:foldl(Handle, Start, Packets)
    catch throw:#alert{} = Alert ->
	    handle_own_alert(Alert, Version, StateName, State), 
	    {stop, normal, State}
    end;

handle_event(#ssl_tls{type = ?APPLICATION_DATA, fragment = Data},
             StateName, State0) ->
    case application_data(Data, State0) of
	Stop = {stop,_,_} ->
	    Stop;
	State ->
	    {next_state, StateName, State}
    end;

handle_event(#ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = <<1>>} = 
	     _ChangeCipher,
             StateName, 
	     State = #state{connection_states = ConnectionStates0}) ->
    ?DBG_TERM(_ChangeCipher),
    ConnectionStates1 =
        ssl_record:activate_pending_connection_state(ConnectionStates0, read),
    {next_state, StateName, 
     next_record(State#state{connection_states = ConnectionStates1})};

handle_event(#ssl_tls{type = ?ALERT, fragment = Data}, StateName, State) ->
    Alerts = decode_alerts(Data),
    ?DBG_TERM(Alerts),
    [alert_event(A) || A <- Alerts],
    {next_state, StateName, State};

handle_event(#alert{level = ?FATAL} = Alert, connection, 
	     #state{from = From, user_application = {_Mon, Pid}, log_alert = Log,
		    host = Host, port = Port, session = Session,
		    role = Role, socket_options = Opts} = State) ->
    invalidate_session(Role, Host, Port, Session),
    log_alert(Log, connection, Alert),
    alert_user(Opts#socket_options.active, Pid, From, Alert, Role),
    {stop, normal, State};
handle_event(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert, 
	     connection, #state{from = From,
				role = Role,
				user_application = {_Mon, Pid}, 
				socket_options = Opts} = State) ->
    alert_user(Opts#socket_options.active, Pid, From, Alert, Role),
    {stop, normal, State};

handle_event(#alert{level = ?FATAL} = Alert, StateName,
	     #state{from = From, host = Host, port = Port, session = Session,
		    log_alert = Log, role = Role} = State) ->
    invalidate_session(Role, Host, Port, Session),
    log_alert(Log, StateName, Alert),
    alert_user(From, Alert, Role),
    {stop, normal, State};
handle_event(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert, 
	     _, #state{from = From, role = Role} = State) -> 
    alert_user(From, Alert, Role),
    {stop, normal, State};
handle_event(#alert{level = ?WARNING} = Alert, StateName, 
	     #state{log_alert = Log} = State) ->
    log_alert(Log, StateName, Alert),
%%TODO:  Could be user_canceled or no_negotiation should the latter be 
    %% treated as fatal?! 
    {next_state, StateName, next_record(State)}.

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
handle_sync_event(started, From, StateName, State) ->
    {next_state, StateName, State#state{from = From}};

handle_sync_event(close, From, _StateName, State) ->
    {stop, normal, ok, State#state{from = From}};

handle_sync_event({shutdown, How}, From, StateName,
		  #state{transport_cb = CbModule,
			 socket = Socket} = State) ->
    case CbModule:shutdown(Socket, How) of
	ok ->
	    {reply, ok, StateName, State};
	Error ->
	    {stop, normal, Error, State#state{from = From}}
    end;
    
%% TODO: men vad gör next_record om det är t.ex. renegotiate? kanske
%% inte bra... tål att tänkas på!
handle_sync_event({recv, N}, From, StateName,
		  State0 = #state{user_data_buffer = Buffer}) ->
    State1 = State0#state{bytes_to_read = N, from = From},
    case Buffer of
	<<>> ->
	    State = next_record(State1),
	    {next_state, StateName, State};
	_ ->
	    case application_data(<<>>, State1) of
		Stop = {stop, _, _} ->
		    Stop;
		State ->
		    {next_state, StateName, State}
	    end
    end;

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
	    {reply, ok, StateName, next_record_if_active(State1)};
	Buffer =:= <<>> ->
            %% Active once already set 
	    {reply, ok, StateName, State1};
	true ->
	    case application_data(<<>>, State1) of
		Stop = {stop,_,_} ->
		    Stop;
		State ->
		    {reply, ok, StateName, State}
	    end
    end;	

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
handle_info({Protocol, _, Data}, StateName, State =
            #state{data_tag = Protocol,
		   negotiated_version = Version,
                   tls_record_buffer = Buf0,
                   tls_cipher_texts = CT0}) ->
    case ssl_record:get_tls_records(Data, Buf0) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    {next_state, StateName, 
	     next_record(State#state{tls_record_buffer = Buf1,
				     tls_cipher_texts = CT1})};
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, StateName, State), 
	    {stop, normal, State}
    end;

%% %% This is the code for {packet,ssl} removed because it was slower 
%% %% than handling it in erlang.
%% handle_info(Data = #ssl_tls{}, StateName, 
%% 	    State = #state{tls_buffer = Buffer,
%% 			   socket = Socket,
%% 			   connection_states = ConnectionStates0}) ->
%%     case Buffer of
%% 	buffer ->
%% 	    {next_state, StateName, State#state{tls_buffer = [Data]}};
%% 	continue ->
%% 	    inet:setopts(Socket, [{active,once}]),
%% 	    {Plain, ConnectionStates} =
%% 		ssl_record:decode_cipher_text(Data, ConnectionStates0),
%% 	    gen_fsm:send_all_state_event(self(), Plain),
%% 	    {next_state, StateName, 
%% 	     State#state{tls_buffer = buffer, 
%% 			 connection_states = ConnectionStates}};
%% 	List when is_list(List) ->
%% 	    {next_state, StateName, 
%% 	     State#state{tls_buffer = Buffer ++ [Data]}}	
%%     end;

%% handle_info(CloseMsg = {_, Socket}, StateName0, 
%% 	    #state{socket = Socket,tls_buffer = [Msg]} = State0) ->
%%     %% Hmm we have a ssl_tls msg buffered, handle that first
%%     %% and it proberbly is a close alert  
%%     {next_state, StateName0, State0#state{tls_buffer=[Msg,{ssl_close,CloseMsg}]}};
	
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

handle_info({'DOWN', MonitorRef, _, _, _}, _, 
	    State = #state{user_application={MonitorRef,_Pid}}) ->
    {stop, normal, State};   

handle_info(A, StateName, State) ->
    io:format("SSL: Bad info (state ~w): ~w\n", [StateName, A]),
    {stop, bad_info, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, connection, _S=#state{negotiated_version = Version,
				      connection_states = ConnectionStates,
				      transport_cb = Transport,
				      socket = Socket}) ->
    {BinAlert, _} = encode_alert(?ALERT_REC(?WARNING,?CLOSE_NOTIFY),
				 Version, ConnectionStates),
    Transport:send(Socket, BinAlert),
    Transport:close(Socket);
terminate(_Reason, _StateName, _S=#state{transport_cb = Transport, socket = Socket}) ->
    Transport:close(Socket),
    ok.

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
start_fsm(Role, Host, Port, Socket, Opts,  User, {CbModule, _,_} = CbInfo, 
	  Timeout) -> 
    case ssl_connection_sup:start_child([Role, Host, Port, Socket, 
                                         Opts, User, CbInfo]) of
        {ok, Pid} -> 
            CbModule:controlling_process(Socket, Pid),
	    send_event(Pid, socket_control),
            case sync_send_all_state_event(Pid, started, Timeout) of
                connected ->
                    {ok, sslsocket(Pid)};
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
    ?DBG_TERM(PrivateKey),
    {ok, CertDbRef, CacheRef, OwnCert, PrivateKey}.

init_certificates(#ssl_options{cacertfile = CACertFile,
			       certfile = CertFile}, Role) ->

    case ssl_manager:connection_init(CACertFile, Role) of
	{ok, CertDbRef, CacheRef} ->
	    init_certificates(CertDbRef, CacheRef, CertFile, Role);
	{error, _Error} ->
	    Report = io_lib:format("SSL: Error ~p ~n",[_Error]),
	    error_logger:error_report(Report),
	    throw(ecacertfile)
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
    catch _E:_R  ->
	    Report = io_lib:format("SSL: ~p: ~p:~p ~p~n",
				   [?LINE, _E,_R, erlang:get_stacktrace()]),
	    error_logger:error_report(Report),
	    throw(ecertfile)
    end.

init_private_key(undefined, "", _Password, client) -> 
    undefined;
init_private_key(undefined, KeyFile, Password, _)  -> 
    try 
	{ok, List} = ssl_manager:cache_pem_file(KeyFile),
	[Der] = [Der || Der = {PKey, _ , _} <- List,
			PKey =:= rsa_private_key orelse PKey =:= dsa_private_key],
	{ok, Decoded} = public_key:decode_private_key(Der,Password),
	Decoded
    catch _E:_R ->
	    Report = io_lib:format("SSL: ~p: ~p:~p ~p~n",
				   [?LINE, _E,_R, erlang:get_stacktrace()]),
	    error_logger:error_report(Report),
	    throw(ekeyfile)
    end;
init_private_key(PrivateKey, _, _,_) ->
    PrivateKey.

send_event(FsmPid, Event) ->
    gen_fsm:send_event(FsmPid, Event).

sync_send_event(FsmPid, Event, Timeout) ->
    try gen_fsm:sync_send_event(FsmPid, Event, Timeout) of
 	Reply ->
 	    Reply
    catch
 	exit:{noproc, _} ->
 	    {error, closed};
 	exit:{timeout, _} ->
 	    {error, timeout};
	exit:{normal, _} ->
	    {error, closed}
    end.



send_all_state_event(FsmPid, Event) ->
    gen_fsm:send_all_state_event(FsmPid, Event).

sync_send_all_state_event(FsmPid, Event) ->
    sync_send_all_state_event(FsmPid, Event, ?DEFAULT_TIMEOUT
). 

sync_send_all_state_event(FsmPid, Event, Timeout) ->
    try gen_fsm:sync_send_all_state_event(FsmPid, Event, Timeout)
    catch
 	exit:{noproc, _} ->
 	    {error, closed};
 	exit:{timeout, _} ->
 	    {error, timeout};
	exit:{normal, _} ->
	    {error, closed}
    end.

%% Events: #alert{}
alert_event(Alert) ->
    send_all_state_event(self(), Alert).

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
	ignore -> %% No key or cert or fixed_diffie_hellman
            State;
        Verified ->
	    SigAlg = ssl_handshake:sig_alg(KeyAlg),
            {BinVerified, ConnectionStates1, Hashes1} = 
                encode_handshake(Verified, SigAlg, Version, 
                                 ConnectionStates0, Hashes0),
            Transport:send(Socket, BinVerified),
            State#state{connection_states = ConnectionStates1,
                        tls_handshake_hashes = Hashes1}
    end;
verify_client_cert(#state{client_certificate_requested = false} = State) ->
    State.

do_server_hello(Type, #state{negotiated_version = Version,
			     session = Session,
			     connection_states = ConnectionStates0} 
		= State0) when is_atom(Type) -> 
    ServerHello = 
        ssl_handshake:server_hello(Session#session.session_id, Version, 
                                   ConnectionStates0),
    State = server_hello(ServerHello, State0),
    
    case Type of	
	new ->
	    do_server_hello(ServerHello, State);
	resumed ->
	    case ssl_handshake:master_secret(Version, Session,
					     ConnectionStates0, server) of
		{_, ConnectionStates1} ->
		    {ConnectionStates, Hashes} =
			finished(State#state{connection_states =
					     ConnectionStates1}),
		    {next_state, abbreviated,
		     next_record(State#state{connection_states = 
					     ConnectionStates,
					     tls_handshake_hashes = Hashes})};
		#alert{} = Alert ->
		    handle_own_alert(Alert, Version, hello, State), 
		    {stop, normal, State}
	    end
    end;

do_server_hello(#server_hello{cipher_suite = CipherSuite,
			      compression_method = Compression,
			      session_id = SessionId}, 
		#state{session = Session0,
		       negotiated_version = Version} = State0) ->
    try server_certify_and_key_exchange(State0) of 
        #state{} = State1 ->
            State = server_hello_done(State1),
	    Session = 
		Session0#session{session_id = SessionId,
				 cipher_suite = CipherSuite,
				 compression_method = Compression},
            {next_state, certify, State#state{session = Session}}
    catch        
        #alert{} = Alert ->  
	    handle_own_alert(Alert, Version, hello, State0),
	    {stop, normal, State0}
    end.

client_certify_and_key_exchange(#state{negotiated_version = Version} = 
				State0) ->
    try do_client_certify_and_key_exchange(State0) of 
        State1 = #state{} ->
	    {ConnectionStates, Hashes} = finalize_client_handshake(State1),
            State = State1#state{connection_states = ConnectionStates,
				 %% Reinitialize 
				 client_certificate_requested = false,
				 tls_handshake_hashes = Hashes},
            {next_state, cipher, next_record(State)}
    
    catch        
        #alert{} = Alert ->  
	    handle_own_alert(Alert, Version, certify_foo, State0),
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
    {KeyAlgorithm, _, _, _} = ssl_cipher:suite_definition(CipherSuite),
    %% Version = ServerHello#server_hello.server_version, TODO ska kontrolleras
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
                         tls_handshake_hashes = Hashes} = State0) ->
    
    HelloDone = ssl_handshake:server_hello_done(),

    {BinHelloDone, NewConnectionStates, NewHashes} =
        encode_handshake(HelloDone, Version, ConnectionStates, Hashes),
    Transport:send(Socket, BinHelloDone),
    State = State0#state{connection_states = NewConnectionStates,
                         tls_handshake_hashes = NewHashes},
    next_record(State).
    
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

key_exchange(#state{role = server, key_algorithm = Algo} = State) 
  when Algo == rsa;
       Algo == dh_dss;
       Algo == dh_rsa ->
    State;

key_exchange(#state{role = server, key_algorithm = rsa_export} = State) ->
    %% TODO when the public key in the server certificate is
    %% less than or equal to 512 bits in length dont send key_exchange
    %% but do it otherwise
    State;

key_exchange(#state{role = server, key_algorithm = Algo,
		    diffie_hellman_params = Params,
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
    Msg =  ssl_handshake:key_exchange(server, Params),
    {BinMsg, ConnectionStates1, Hashes1} =
        encode_handshake(Msg, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    State#state{connection_states = ConnectionStates1,
                tls_handshake_hashes = Hashes1};

key_exchange(#state{role = server, key_algorithm = dh_anon,
			    connection_states = ConnectionStates0,
			    negotiated_version = Version,
			    tls_handshake_hashes = Hashes0,
			    socket = Socket,
			    transport_cb = Transport
		   } = State) ->
    Msg = ssl_handshake:key_exchange(server, anonymous),    
    {BinMsg, ConnectionStates1, Hashes1} =
        encode_handshake(Msg, Version, ConnectionStates0, Hashes0),
    Transport:send(Socket, BinMsg),
    State#state{connection_states = ConnectionStates1,
                tls_handshake_hashes = Hashes1};
   
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
		    public_key_info = PublicKeyInfo,
		    negotiated_version = Version,
		    diffie_hellman_params = Params,
		    own_cert = Cert,
		    socket = Socket, transport_cb = Transport,
		    tls_handshake_hashes = Hashes0} = State) 
  when Algorithm == dhe_dss;
       Algorithm == dhe_dss_export;
       Algorithm == dhe_rsa;
       Algorithm == dhe_rsa_export ->
    Msg = dh_key_exchange(Cert, Params, PublicKeyInfo),    
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

dh_key_exchange(OwnCert, Params, PublicKeyInfo) ->
    case public_key:pkix_is_fixed_dh_cert(OwnCert) of
	true ->
	    ssl_handshake:key_exchange(client, fixed_diffie_hellman);
	false ->
	    ssl_handshake:key_exchange(client, {dh, Params, PublicKeyInfo})
    end.

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

finalize_client_handshake(#state{connection_states = ConnectionStates0} 
                          = State) ->
    ConnectionStates1 = 
        cipher_protocol(State#state{connection_states = 
                                    ConnectionStates0}),    
    ConnectionStates2 =
        ssl_record:activate_pending_connection_state(ConnectionStates1,
                                                     write),
    finished(State#state{connection_states = ConnectionStates2}).
    

finalize_server_handshake(State) ->
    ConnectionStates0 = cipher_protocol(State),
    ConnectionStates = 
        ssl_record:activate_pending_connection_state(ConnectionStates0, write),
    finished(State#state{connection_states = ConnectionStates}).

cipher_protocol(#state{connection_states = ConnectionStates,
                       socket = Socket,
                       negotiated_version = Version,
                       transport_cb = Transport}) ->
    {BinChangeCipher, NewConnectionStates} =
        encode_change_cipher(#change_cipher_spec{}, Version, ConnectionStates),
    Transport:send(Socket, BinChangeCipher),
    NewConnectionStates.
   
finished(#state{role = Role, socket = Socket, negotiated_version = Version,
                transport_cb = Transport,
		session = Session,
                connection_states = ConnectionStates,
                tls_handshake_hashes = Hashes}) ->
    MasterSecret = Session#session.master_secret,
    Finished = ssl_handshake:finished(Version, Role, MasterSecret, Hashes),
    {BinFinished, NewConnectionStates, NewHashes} = 
        encode_handshake(Finished, Version, ConnectionStates, Hashes),
    Transport:send(Socket, BinFinished),
    {NewConnectionStates, NewHashes}.

handle_server_key(_KeyExchangeMsg, State) ->
    State.
handle_clinet_key(_KeyExchangeMsg, State) ->
    State.

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

encode_data(Data, Version, ConnectionStates) ->
    ssl_record:encode_data(Data, Version, ConnectionStates).

decode_alerts(Bin) ->
    decode_alerts(Bin, []).

decode_alerts(<<?BYTE(Level), ?BYTE(Description), Rest/binary>>, Acc) ->
    A = ?ALERT_REC(Level, Description),
    decode_alerts(Rest, [A | Acc]);
decode_alerts(<<>>, Acc) ->
    lists:reverse(Acc, []).

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
		SocketOpt#socket_options.active =:= false -> 
		    State; %% Passive mode, wait for active once or recv
		Buffer =:= <<>> -> %% Active and empty, get more data
		    next_record(State);
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
    case erlang:decode_packet(Type, Buffer, PacketOpts) of
	{more, _} ->
	    {ok, <<>>, Buffer};
	Decoded ->
	    Decoded
    end.

deliver_app_data(SO = #socket_options{active=once}, Data, Pid, From) ->
    send_or_reply(once, Pid, From, format_reply(SO, Data)),
    SO#socket_options{active=false};
deliver_app_data(SO= #socket_options{active=Active}, Data, Pid, From) ->
    send_or_reply(Active, Pid, From, format_reply(SO, Data)),
    SO.

format_reply(#socket_options{active=false, mode=Mode, header=Header}, Data) ->
    {ok, format_reply(Mode, Header, Data)};
format_reply(#socket_options{active=_, mode=Mode, header=Header}, Data) ->
    {ssl, sslsocket(), format_reply(Mode, Header, Data)}.

deliver_packet_error(SO= #socket_options{active=Active}, Data, Pid, From) ->
    send_or_reply(Active, Pid, From, format_packet_error(SO, Data)).

format_packet_error(#socket_options{active=false, mode=Mode}, Data) ->
    {error, {invalid_packet, format_reply(Mode, raw, Data)}};
format_packet_error(#socket_options{active=_, mode=Mode}, Data) ->
    {ssl_error, sslsocket(), {invalid_packet, format_reply(Mode, raw, Data)}}.

format_reply(list,     _, Data) ->  binary_to_list(Data);
format_reply(binary,   0, Data) ->  Data;
format_reply(binary, raw, Data) ->  Data;
format_reply(binary,   N, Data) ->  % Header mode
    <<Header:N/binary, Rest/binary>> = Data,
    [binary_to_list(Header), Rest].

%% tcp_closed
send_or_reply(false, _Pid, undefined, _Data) ->
    Report = io_lib:format("SSL(debug): Unexpected Data ~p ~n",[_Data]),
    error_logger:error_report(Report),
    erlang:error({badarg, _Pid, undefined, _Data}),
    ok;
send_or_reply(false, _Pid, From, Data) ->
    gen_fsm:reply(From, Data);
send_or_reply(_, Pid, _From, Data) ->
    send_user(Pid, Data).

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.

send_user(Pid, Msg) ->
    Pid ! Msg.

%% %% This is the code for {packet,ssl} removed because it was slower 
%% %% than handling it in erlang.
%% next_record(#state{socket = Socket, 
%% 		   tls_buffer = [Msg|Rest],
%% 		   connection_states = ConnectionStates0} = State) ->
%%     Buffer =
%% 	case Rest of
%% 	    [] -> 
%% 		inet:setopts(Socket, [{active,once}]),
%% 		buffer;
%% 	    _ -> Rest
%% 	end,
%%     case Msg of
%% 	#ssl_tls{} ->
%% 	    {Plain, ConnectionStates} =
%% 		ssl_record:decode_cipher_text(Msg, ConnectionStates0),
%% 	    gen_fsm:send_all_state_event(self(), Plain),
%% 	    State#state{tls_buffer=Buffer, connection_states = ConnectionStates};
%% 	{ssl_close, Msg} ->
%% 	    self() ! Msg,
%% 	    State#state{tls_buffer=Buffer}
%%     end;
%% next_record(#state{socket = Socket, tls_buffer = undefined} = State) ->
%%     inet:setopts(Socket, [{active,once}]),
%%     State#state{tls_buffer=continue};
%% next_record(State) ->
%%     State#state{tls_buffer=continue}.

next_record(#state{tls_cipher_texts = [], socket = Socket} = State) ->
    inet:setopts(Socket, [{active,once}]),
    State;
next_record(#state{tls_cipher_texts = [CT | Rest], 
		   connection_states = ConnStates0} = State) ->
    {Plain, ConnStates} = ssl_record:decode_cipher_text(CT, ConnStates0),
    gen_fsm:send_all_state_event(self(), Plain),
    State#state{tls_cipher_texts = Rest, connection_states = ConnStates}.

next_record_if_active(State = 
		      #state{socket_options = 
			     #socket_options{active = false}}) ->    
    State;
next_record_if_active(State) ->
    next_record(State).

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
	      {CbModule, DataTag, CloseTag}) ->
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
	   session_cache_cb = SessionCacheCb
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
    get_socket_opts(Socket, Tags, SockOpts, 
		    [{packet, SockOpts#socket_options.packet} | Acc]);
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
    set_socket_opts(Socket, Opts, SockOpts#socket_options{mode = Mode}, Other);
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

alert_user(From, Alert, Role) ->
    alert_user(false, no_pid, From, Alert, Role).

alert_user(false = Active, Pid, From,  Alert, Role) ->
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

log_alert(true, StateName, Alert) ->
    Txt = ssl_alert:alert_txt(Alert),
    error_logger:format("SSL: ~p: ~s\n", [StateName, Txt]);
log_alert(false, _, _) ->
    ok.

handle_own_alert(Alert, Version, StateName, 
		 #state{transport_cb = Transport,
			socket = Socket,
			from = User,
			role = Role,
			connection_states = ConnectionStates,
			log_alert = Log}) ->
    {BinMsg, _} =
	encode_alert(Alert, Version, ConnectionStates),
    Transport:send(Socket, BinMsg),
    log_alert(Log, StateName, Alert),
    alert_user(User, Alert, Role).

make_premaster_secret({MajVer, MinVer}) ->
    Rand = crypto:rand_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    <<?BYTE(MajVer), ?BYTE(MinVer), Rand/binary>>.
