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
%% Purpose: Help funtions for handling the SSL-handshake protocol
%%----------------------------------------------------------------------

-module(ssl_handshake).

-include("ssl_handshake.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([master_secret/4, client_hello/6, server_hello/4, hello/4,
	 hello_request/0, certify/7, certificate/4,
	 client_certificate_verify/5, certificate_verify/5,
	 certificate_request/3, key_exchange/2, server_key_exchange_hash/2,
	 finished/4, verify_connection/5, get_tls_handshake/2,
	 decode_client_key/3, server_hello_done/0,
	 encode_handshake/2, init_hashes/0, update_hashes/2,
	 decrypt_premaster_secret/2]).

-export([dec_hello_extensions/2]).

-type tls_handshake() :: #client_hello{} | #server_hello{} |
			 #server_hello_done{} | #certificate{} | #certificate_request{} |
			 #client_key_exchange{} | #finished{} | #certificate_verify{} |
			 #hello_request{}.

%%====================================================================
%% Internal application API
%%====================================================================
%%--------------------------------------------------------------------
-spec client_hello(host(), inet:port_number(), #connection_states{},
		   #ssl_options{}, boolean(), der_cert()) -> #client_hello{}.
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(Host, Port, ConnectionStates, #ssl_options{versions = Versions,
							ciphers = UserSuites} 
	     = SslOpts, Renegotiation, OwnCert) ->
    
    Fun = fun(Version) ->
		  ssl_record:protocol_version(Version)
	  end,
    Version = ssl_record:highest_protocol_version(lists:map(Fun, Versions)),
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
    Ciphers = available_suites(UserSuites, Version),

    Id = ssl_manager:client_session_id(Host, Port, SslOpts, OwnCert),

    #client_hello{session_id = Id, 
		  client_version = Version,
		  cipher_suites = cipher_suites(Ciphers, Renegotiation),
		  compression_methods = ssl_record:compressions(),
		  random = SecParams#security_parameters.client_random,
		  renegotiation_info  = 
		  renegotiation_info(client, ConnectionStates, Renegotiation)
		 }.

%%--------------------------------------------------------------------
-spec server_hello(session_id(), tls_version(), #connection_states{}, 
		   boolean()) -> #server_hello{}.
%%
%% Description: Creates a server hello message.
%%--------------------------------------------------------------------
server_hello(SessionId, Version, ConnectionStates, Renegotiation) ->
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
    #server_hello{server_version = Version,
		  cipher_suite = SecParams#security_parameters.cipher_suite,
                  compression_method = 
		  SecParams#security_parameters.compression_algorithm,
		  random = SecParams#security_parameters.server_random,
		  session_id = SessionId,
		  renegotiation_info = 
		  renegotiation_info(server, ConnectionStates, Renegotiation)
		 }.

%%--------------------------------------------------------------------
-spec hello_request() -> #hello_request{}.
%%
%% Description: Creates a hello request message sent by server to 
%% trigger renegotiation.
%%--------------------------------------------------------------------
hello_request() ->
    #hello_request{}.

%%--------------------------------------------------------------------
-spec hello(#server_hello{} | #client_hello{}, #ssl_options{},
	    #connection_states{} | {inet:port_number(), #session{}, db_handle(),
 				    atom(), #connection_states{}, binary()},
 	    boolean()) -> {tls_version(), session_id(), #connection_states{}}| 
 			  {tls_version(), {resumed | new, #session{}}, 
 			   #connection_states{}} | #alert{}.
%%
%% Description: Handles a recieved hello message
%%--------------------------------------------------------------------
hello(#server_hello{cipher_suite = CipherSuite, server_version = Version,
		    compression_method = Compression, random = Random,
		    session_id = SessionId, renegotiation_info = Info},
      #ssl_options{secure_renegotiate = SecureRenegotation},
      ConnectionStates0, Renegotiation) ->

    case ssl_record:is_acceptable_version(Version) of
	true ->
	    case handle_renegotiation_info(client, Info, ConnectionStates0, 
					   Renegotiation, SecureRenegotation, []) of
		{ok, ConnectionStates1} ->
		    ConnectionStates =
			hello_pending_connection_states(client, CipherSuite, Random, 
							Compression, ConnectionStates1),
		    {Version, SessionId, ConnectionStates};
		#alert{} = Alert ->
		    Alert
	    end;
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end;
			       
hello(#client_hello{client_version = ClientVersion, random = Random,
		    cipher_suites = CipherSuites,
		    renegotiation_info = Info} = Hello,
      #ssl_options{versions = Versions, 
		   secure_renegotiate = SecureRenegotation} = SslOpts,
      {Port, Session0, Cache, CacheCb, ConnectionStates0, Cert}, Renegotiation) ->
    Version = select_version(ClientVersion, Versions),
    case ssl_record:is_acceptable_version(Version) of
	true ->
	    {Type, #session{cipher_suite = CipherSuite,
			    compression_method = Compression} = Session} 
		= select_session(Hello, Port, Session0, Version, 
				 SslOpts, Cache, CacheCb, Cert),
	    case CipherSuite of 
		no_suite ->
		    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY);
		_ ->
		    case handle_renegotiation_info(server, Info, ConnectionStates0,
						   Renegotiation, SecureRenegotation, 
						   CipherSuites) of
			{ok, ConnectionStates1} ->
			    ConnectionStates =
				hello_pending_connection_states(server, 
								CipherSuite,
								Random, 
								Compression,
								ConnectionStates1),
			    {Version, {Type, Session}, ConnectionStates};
			#alert{} = Alert ->
			    Alert
		    end
	    end;
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end.

%%--------------------------------------------------------------------
-spec certify(#certificate{}, db_handle(), certdb_ref(), integer() | nolimit,
	      verify_peer | verify_none, {fun(), term},
	      client | server) ->  {der_cert(), public_key_info()} | #alert{}.
%%
%% Description: Handles a certificate handshake message
%%--------------------------------------------------------------------
certify(#certificate{asn1_certificates = ASN1Certs}, CertDbHandle, CertDbRef,
	MaxPathLen, _Verify, VerifyFunAndState, Role) ->
    [PeerCert | _] = ASN1Certs,
      
    ValidationFunAndState =
	case VerifyFunAndState of
	    undefined ->
		{fun(OtpCert, ExtensionOrError, SslState) ->
			 ssl_certificate:validate_extension(OtpCert,
							    ExtensionOrError, SslState)
		 end, Role};
	    {Fun, UserState0} ->
		{fun(OtpCert, ExtensionOrError, {SslState, UserState}) ->
			 case ssl_certificate:validate_extension(OtpCert,
								 ExtensionOrError,
								 SslState) of
			     {valid, NewSslState} ->
				 {valid, {NewSslState, UserState}};
			     {fail, Reason} ->
				 apply_user_fun(Fun, OtpCert, Reason, UserState,
						SslState);
			     {unknown, _} ->
				 apply_user_fun(Fun, OtpCert,
						ExtensionOrError, UserState, SslState)
			 end
		 end, {Role, UserState0}}
	end,

    {TrustedErlCert, CertPath}  =
	ssl_certificate:trusted_cert_and_path(ASN1Certs, CertDbHandle, CertDbRef),

    case public_key:pkix_path_validation(TrustedErlCert,
					 CertPath,
					 [{max_path_length,
					   MaxPathLen},
					  {verify_fun, ValidationFunAndState}]) of
	{ok, {PublicKeyInfo,_}} ->
	    {PeerCert, PublicKeyInfo};
	{error, Reason} ->
	    path_validation_alert(Reason)
    end.

%%--------------------------------------------------------------------
-spec certificate(der_cert(), db_handle(), certdb_ref(), client | server) -> #certificate{} | #alert{}.
%%
%% Description: Creates a certificate message.
%%--------------------------------------------------------------------
certificate(OwnCert, CertDbHandle, CertDbRef, client) ->
    Chain =
	case ssl_certificate:certificate_chain(OwnCert, CertDbHandle, CertDbRef) of
	    {ok, CertChain} ->
		CertChain;
	    {error, _} -> 
		%% If no suitable certificate is available, the client
		%% SHOULD send a certificate message containing no
		%% certificates. (chapter 7.4.6. RFC 4346)
		[]	 
	end,
    #certificate{asn1_certificates = Chain};

certificate(OwnCert, CertDbHandle, CertDbRef, server) ->
    case ssl_certificate:certificate_chain(OwnCert, CertDbHandle, CertDbRef) of
	{ok, Chain} ->
	    #certificate{asn1_certificates = Chain};
	{error, _} ->
	    ?ALERT_REC(?FATAL, ?INTERNAL_ERROR)
    end.

%%--------------------------------------------------------------------
-spec client_certificate_verify(undefined | der_cert(), binary(),
				tls_version(), private_key(),
				{{binary(), binary()},{binary(), binary()}}) ->  
    #certificate_verify{} | ignore | #alert{}.
%%
%% Description: Creates a certificate_verify message, called by the client.
%%--------------------------------------------------------------------
client_certificate_verify(undefined, _, _, _, _) ->
    ignore;
client_certificate_verify(_, _, _, undefined, _) ->
    ignore;
client_certificate_verify(OwnCert, MasterSecret, Version,
			  PrivateKey, {Hashes0, _}) ->
    case public_key:pkix_is_fixed_dh_cert(OwnCert) of
	true ->
	    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE);
	false ->	    
	    Hashes = 
		calc_certificate_verify(Version, MasterSecret,
					alg_oid(PrivateKey), Hashes0),
	    Signed = digitally_signed(Hashes, PrivateKey),
	    #certificate_verify{signature = Signed}
    end.

%%--------------------------------------------------------------------
-spec certificate_verify(binary(), public_key_info(), tls_version(),
			 binary(), {_, {binary(), binary()}}) -> valid | #alert{}.
%%
%% Description: Checks that the certificate_verify message is valid.
%%--------------------------------------------------------------------
certificate_verify(Signature, {?'rsaEncryption'= Algorithm, PublicKey, _}, Version,
		   MasterSecret, {_, Hashes0}) ->
    Hashes = calc_certificate_verify(Version, MasterSecret,
					   Algorithm, Hashes0),
    case public_key:decrypt_public(Signature, PublicKey,
				   [{rsa_pad, rsa_pkcs1_padding}]) of
	Hashes ->
	    valid;
	_ ->
	    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE)
    end;
certificate_verify(Signature, {?'id-dsa' = Algorithm, PublicKey, PublicKeyParams}, Version,
		   MasterSecret, {_, Hashes0}) ->
    Hashes = calc_certificate_verify(Version, MasterSecret,
				     Algorithm, Hashes0),
    case public_key:verify(Hashes, none, Signature, {PublicKey, PublicKeyParams}) of
    	true ->
    	    valid;
    	false ->
    	    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE)
    end.


%%--------------------------------------------------------------------
-spec certificate_request(#connection_states{}, db_handle(), certdb_ref()) ->
    #certificate_request{}.
%%
%% Description: Creates a certificate_request message, called by the server.
%%--------------------------------------------------------------------
certificate_request(ConnectionStates, CertDbHandle, CertDbRef) ->
    #connection_state{security_parameters = 
		      #security_parameters{cipher_suite = CipherSuite}} =
	ssl_record:pending_connection_state(ConnectionStates, read),
    Types = certificate_types(CipherSuite),
    Authorities = certificate_authorities(CertDbHandle, CertDbRef),
    #certificate_request{
		    certificate_types = Types,
		    certificate_authorities = Authorities
		   }.

%%--------------------------------------------------------------------
-spec key_exchange(client | server, 
		   {premaster_secret, binary(), public_key_info()} |
		   {dh, binary()} |
		   {dh, {binary(), binary()}, #'DHParameter'{}, key_algo(),
		   binary(), binary(), private_key()}) ->
    #client_key_exchange{} | #server_key_exchange{}.
%%
%% Description: Creates a keyexchange message.
%%--------------------------------------------------------------------
key_exchange(client, {premaster_secret, Secret, {_, PublicKey, _}}) ->
    EncPremasterSecret =
	encrypted_premaster_secret(Secret, PublicKey),
    #client_key_exchange{exchange_keys = EncPremasterSecret};

key_exchange(client, {dh, <<?UINT32(Len), PublicKey:Len/binary>>}) ->
    #client_key_exchange{
	      exchange_keys = #client_diffie_hellman_public{
		dh_public = PublicKey}
	       };

key_exchange(server, {dh, {<<?UINT32(Len), PublicKey:Len/binary>>, _}, 
		      #'DHParameter'{prime = P, base = G},
		      KeyAlgo, ClientRandom, ServerRandom, PrivateKey}) ->
    <<?UINT32(_), PBin/binary>> = crypto:mpint(P),
    <<?UINT32(_), GBin/binary>> = crypto:mpint(G),
    PLen = byte_size(PBin),
    GLen = byte_size(GBin),
    YLen = byte_size(PublicKey),
    ServerDHParams = #server_dh_params{dh_p = PBin, 
				       dh_g = GBin, dh_y = PublicKey},    

    case KeyAlgo of
	dh_anon ->
	    #server_key_exchange{params = ServerDHParams,
				 signed_params = <<>>};
	_ ->
	    Hash =
		server_key_exchange_hash(KeyAlgo, <<ClientRandom/binary,
						    ServerRandom/binary,
						    ?UINT16(PLen), PBin/binary,
						    ?UINT16(GLen), GBin/binary,
						    ?UINT16(YLen), PublicKey/binary>>),
	    Signed = digitally_signed(Hash, PrivateKey),
	    #server_key_exchange{params = ServerDHParams,
				 signed_params = Signed}
    end.

%%--------------------------------------------------------------------
-spec master_secret(tls_version(), #session{} | binary(), #connection_states{},
		   client | server) -> {binary(), #connection_states{}} | #alert{}.
%%    
%% Description: Sets or calculates the master secret and calculate keys,
%% updating the pending connection states. The Mastersecret and the update
%% connection states are returned or an alert if the calculation fails.
%%-------------------------------------------------------------------
master_secret(Version, #session{master_secret = Mastersecret}, 
	      ConnectionStates, Role) ->
    ConnectionState = 
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    try master_secret(Version, Mastersecret, SecParams, 
		      ConnectionStates, Role)
    catch
	exit:Reason ->
	    Report = io_lib:format("Key calculation failed due to ~p",
				   [Reason]),
	    error_logger:error_report(Report),
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
    end;

master_secret(Version, PremasterSecret, ConnectionStates, Role) ->
    ConnectionState = 
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams, 
    try master_secret(Version, 
		      calc_master_secret(Version,PremasterSecret,
				       ClientRandom, ServerRandom),
		      SecParams, ConnectionStates, Role) 
    catch
	exit:Reason ->
	    Report = io_lib:format("Master secret calculation failed"
				   " due to ~p", [Reason]),
	    error_logger:error_report(Report),
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
    end.

%%--------------------------------------------------------------------
-spec finished(tls_version(), client | server, binary(), {{binary(), binary()},_}) ->
    #finished{}.
%%
%% Description: Creates a handshake finished message
%%-------------------------------------------------------------------
finished(Version, Role, MasterSecret, {Hashes, _}) -> % use the current hashes
    #finished{verify_data = 
	      calc_finished(Version, Role, MasterSecret, Hashes)}.

%%--------------------------------------------------------------------
-spec verify_connection(tls_version(), #finished{}, client | server, binary(), 
			{_, {binary(), binary()}}) -> verified | #alert{}.
%%
%% Description: Checks the ssl handshake finished message to verify
%%              the connection.
%%-------------------------------------------------------------------
verify_connection(Version, #finished{verify_data = Data}, 
		  Role, MasterSecret, {_, {MD5, SHA}}) -> 
    %% use the previous hashes
    case calc_finished(Version, Role, MasterSecret, {MD5, SHA}) of
	Data ->
	    verified;
	_ ->
	    ?ALERT_REC(?FATAL, ?DECRYPT_ERROR)
    end.
%%--------------------------------------------------------------------
-spec server_hello_done() ->  #server_hello_done{}.
%%     
%% Description: Creates a server hello done message.
%%--------------------------------------------------------------------	    
server_hello_done() ->
    #server_hello_done{}.

%%--------------------------------------------------------------------
-spec encode_handshake(tls_handshake(), tls_version()) -> iolist().
%%     
%% Description: Encode a handshake packet to binary
%%--------------------------------------------------------------------
encode_handshake(Package, Version) ->
    {MsgType, Bin} = enc_hs(Package, Version),
    Len = byte_size(Bin),
    [MsgType, ?uint24(Len), Bin].

%%--------------------------------------------------------------------
-spec get_tls_handshake(binary(), binary() | iolist()) ->
     {[tls_handshake()], binary()}.
%%
%% Description: Given buffered and new data from ssl_record, collects
%% and returns it as a list of handshake messages, also returns leftover
%% data.
%%--------------------------------------------------------------------
get_tls_handshake(Data, <<>>) ->
    get_tls_handshake_aux(Data, []);
get_tls_handshake(Data, Buffer) ->
    get_tls_handshake_aux(list_to_binary([Buffer, Data]), []).

%%--------------------------------------------------------------------
-spec decode_client_key(binary(), key_algo(), tls_version()) ->
			    #encrypted_premaster_secret{} | #client_diffie_hellman_public{}.
%%
%% Description: Decode client_key data and return appropriate type
%%--------------------------------------------------------------------
decode_client_key(ClientKey, Type, Version) ->
    dec_client_key(ClientKey, key_exchange_alg(Type), Version).

%%--------------------------------------------------------------------
-spec init_hashes() ->{{binary(), binary()}, {binary(), binary()}}.

%%
%% Description: Calls crypto hash (md5 and sha) init functions to
%% initalize the hash context.
%%--------------------------------------------------------------------
init_hashes() ->
    T = {crypto:md5_init(), crypto:sha_init()},
    {T, T}.

%%--------------------------------------------------------------------
-spec update_hashes({{binary(), binary()}, {binary(), binary()}}, Data ::term()) ->
			   {{binary(), binary()}, {binary(), binary()}}.
%%
%% Description: Calls crypto hash (md5 and sha) update functions to
%% update the hash context with Data.
%%--------------------------------------------------------------------
update_hashes(Hashes, % special-case SSL2 client hello
	      <<?CLIENT_HELLO, ?UINT24(_), ?BYTE(Major), ?BYTE(Minor),
		?UINT16(CSLength), ?UINT16(0),
		?UINT16(CDLength),
	       CipherSuites:CSLength/binary,
	       ChallengeData:CDLength/binary>>) ->
    update_hashes(Hashes,
		  <<?CLIENT_HELLO, ?BYTE(Major), ?BYTE(Minor),
		   ?UINT16(CSLength), ?UINT16(0),
		   ?UINT16(CDLength),
		   CipherSuites:CSLength/binary,
		   ChallengeData:CDLength/binary>>);
update_hashes({{MD50, SHA0}, _Prev}, Data) ->
    {MD51, SHA1} = {crypto:md5_update(MD50, Data),
		    crypto:sha_update(SHA0, Data)},
    {{MD51, SHA1}, {MD50, SHA0}}.

%%--------------------------------------------------------------------
-spec decrypt_premaster_secret(binary(), #'RSAPrivateKey'{}) -> binary().

%%
%% Description: Public key decryption using the private key.
%%--------------------------------------------------------------------
decrypt_premaster_secret(Secret, RSAPrivateKey) ->
    try public_key:decrypt_private(Secret, RSAPrivateKey,
				   [{rsa_pad, rsa_pkcs1_padding}])
    catch
	_:_ ->
	    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
    end.

%%--------------------------------------------------------------------
-spec server_key_exchange_hash(rsa | dhe_rsa| dhe_dss | dh_anon, binary()) -> binary().

%%
%% Description: Calculate server key exchange hash
%%--------------------------------------------------------------------
server_key_exchange_hash(Algorithm, Value) when Algorithm == rsa;
						Algorithm == dhe_rsa ->
    MD5 = crypto:md5(Value),
    SHA =  crypto:sha(Value),
    <<MD5/binary, SHA/binary>>;

server_key_exchange_hash(dhe_dss, Value) ->
    crypto:sha(Value).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_tls_handshake_aux(<<?BYTE(Type), ?UINT24(Length), 
			Body:Length/binary,Rest/binary>>, Acc) ->
    Raw = <<?BYTE(Type), ?UINT24(Length), Body/binary>>,
    H = dec_hs(Type, Body),
    get_tls_handshake_aux(Rest, [{H,Raw} | Acc]);
get_tls_handshake_aux(Data, Acc) ->
    {lists:reverse(Acc), Data}.

path_validation_alert({bad_cert, cert_expired}) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_EXPIRED);
path_validation_alert({bad_cert, invalid_issuer}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, invalid_signature}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, name_not_permitted}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, unknown_critical_extension}) ->
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE);
path_validation_alert({bad_cert, cert_revoked}) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_REVOKED);
path_validation_alert({bad_cert, selfsigned_peer}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, unknown_ca}) ->
     ?ALERT_REC(?FATAL, ?UNKNOWN_CA);
path_validation_alert(_) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE).

select_session(Hello, Port, Session, Version, 
	       #ssl_options{ciphers = UserSuites} = SslOpts, Cache, CacheCb, Cert) ->
    SuggestedSessionId = Hello#client_hello.session_id,
    SessionId = ssl_manager:server_session_id(Port, SuggestedSessionId, 
					      SslOpts, Cert),
    
    Suites = available_suites(Cert, UserSuites, Version), 
    case ssl_session:is_new(SuggestedSessionId, SessionId) of
        true ->
	    CipherSuite = 
		select_cipher_suite(Hello#client_hello.cipher_suites, Suites),
	    Compressions = Hello#client_hello.compression_methods,
	    Compression = select_compression(Compressions),
	    {new, Session#session{session_id = SessionId,
				  cipher_suite = CipherSuite,
				  compression_method = Compression}};
	false ->	    
	    {resumed, CacheCb:lookup(Cache, {Port, SessionId})}
    end.

available_suites(UserSuites, Version) ->
    case UserSuites of
	[] ->
	    ssl_cipher:suites(Version);
	_ ->
	    UserSuites
    end.

available_suites(ServerCert, UserSuites, Version) ->
    ssl_cipher:filter(ServerCert, available_suites(UserSuites, Version)).
 
cipher_suites(Suites, false) ->
    [?TLS_EMPTY_RENEGOTIATION_INFO_SCSV | Suites];
cipher_suites(Suites, true) ->
    Suites.

renegotiation_info(client, _, false) ->
    #renegotiation_info{renegotiated_connection = undefined};
renegotiation_info(server, ConnectionStates, false) ->
    CS  = ssl_record:current_connection_state(ConnectionStates, read),
    case CS#connection_state.secure_renegotiation of
	true ->
	    #renegotiation_info{renegotiated_connection = ?byte(0)};
	false ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end;
renegotiation_info(client, ConnectionStates, true) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    case CS#connection_state.secure_renegotiation of
	true ->
	    Data = CS#connection_state.client_verify_data,
	    #renegotiation_info{renegotiated_connection = Data};
	false ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end;

renegotiation_info(server, ConnectionStates, true) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    case CS#connection_state.secure_renegotiation of
	true ->
	    CData = CS#connection_state.client_verify_data,
	    SData  =CS#connection_state.server_verify_data,
	    #renegotiation_info{renegotiated_connection = <<CData/binary, SData/binary>>};
	false ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end. 

handle_renegotiation_info(_, #renegotiation_info{renegotiated_connection = ?byte(0)}, 
			  ConnectionStates, false, _, _) ->
    {ok, ssl_record:set_renegotiation_flag(true, ConnectionStates)};

handle_renegotiation_info(server, undefined, ConnectionStates, _, _, CipherSuites) -> 
    case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	true ->
	    {ok, ssl_record:set_renegotiation_flag(true, ConnectionStates)};
	false ->
	    {ok, ssl_record:set_renegotiation_flag(false, ConnectionStates)}
    end;

handle_renegotiation_info(_, undefined, ConnectionStates, false, _, _) ->
    {ok, ssl_record:set_renegotiation_flag(false, ConnectionStates)};

handle_renegotiation_info(client, #renegotiation_info{renegotiated_connection = ClientServerVerify}, 
			  ConnectionStates, true, _, _) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    CData = CS#connection_state.client_verify_data,
    SData = CS#connection_state.server_verify_data,    
    case <<CData/binary, SData/binary>> == ClientServerVerify of
	true ->
	    {ok, ConnectionStates};
	false ->
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
    end;
handle_renegotiation_info(server, #renegotiation_info{renegotiated_connection = ClientVerify}, 
			  ConnectionStates, true, _, CipherSuites) ->
    
      case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	  true ->
	      ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
	  false ->	
	      CS = ssl_record:current_connection_state(ConnectionStates, read),
	      Data = CS#connection_state.client_verify_data,
	      case Data == ClientVerify of
		  true ->
		      {ok, ConnectionStates};
		  false ->
		      ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
	      end
      end;

handle_renegotiation_info(client, undefined, ConnectionStates, true, SecureRenegotation, _) ->
    handle_renegotiation_info(ConnectionStates, SecureRenegotation);

handle_renegotiation_info(server, undefined, ConnectionStates, true, SecureRenegotation, CipherSuites) ->
     case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	  true ->
	     ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
	 false ->
	     handle_renegotiation_info(ConnectionStates, SecureRenegotation)
     end.

handle_renegotiation_info(ConnectionStates, SecureRenegotation) ->
    CS = ssl_record:current_connection_state(ConnectionStates, read),
    case {SecureRenegotation, CS#connection_state.secure_renegotiation} of
	{_, true} ->
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
	{true, false} ->
	    ?ALERT_REC(?FATAL, ?NO_RENEGOTIATION);
	{false, false} ->
	    {ok, ConnectionStates}
    end.

%% Update pending connection states with parameters exchanged via 
%% hello messages
%% NOTE : Role is the role of the receiver of the hello message
%%        currently being processed.
hello_pending_connection_states(Role, CipherSuite, Random, Compression,
				 ConnectionStates) ->    
    ReadState =  
	ssl_record:pending_connection_state(ConnectionStates, read),
    WriteState = 
	ssl_record:pending_connection_state(ConnectionStates, write),
    
    NewReadSecParams = 
	hello_security_parameters(Role, ReadState, CipherSuite, 
			    Random, Compression),
    
    NewWriteSecParams =
	hello_security_parameters(Role, WriteState, CipherSuite,
			    Random, Compression),
 
    ssl_record:update_security_params(NewReadSecParams,
				    NewWriteSecParams,
				    ConnectionStates).

hello_security_parameters(client, ConnectionState, CipherSuite, Random,
			  Compression) ->   
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(CipherSuite, SecParams),
    NewSecParams#security_parameters{
      server_random = Random,
      compression_algorithm = Compression
     };

hello_security_parameters(server, ConnectionState, CipherSuite, Random, 
			  Compression) ->
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(CipherSuite, SecParams),
    NewSecParams#security_parameters{
      client_random = Random,
      compression_algorithm = Compression
     }.

select_version(ClientVersion, Versions) ->   
    Fun = fun(Version) ->
		  ssl_record:protocol_version(Version)
	  end,
    ServerVersion = ssl_record:highest_protocol_version(lists:map(Fun,
								  Versions)),
    ssl_record:lowest_protocol_version(ClientVersion, ServerVersion).

select_cipher_suite([], _) ->
   no_suite;
select_cipher_suite([Suite | ClientSuites], SupportedSuites) ->
    case is_member(Suite, SupportedSuites) of
	true ->
	    Suite;
        false ->
	    select_cipher_suite(ClientSuites, SupportedSuites)
    end.

is_member(Suite, SupportedSuites) ->
    lists:member(Suite, SupportedSuites).

select_compression(_CompressionMetodes) ->
    ?NULL.

master_secret(Version, MasterSecret, #security_parameters{
			 client_random = ClientRandom,
			 server_random = ServerRandom,
			 hash_size = HashSize,
			 key_material_length = KML,
			 expanded_key_material_length = EKML,
			 iv_size = IVS},
	      ConnectionStates, Role) ->
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV} =
	setup_keys(Version, MasterSecret, ServerRandom, 
		   ClientRandom, HashSize, KML, EKML, IVS),

    ConnStates1 = ssl_record:set_master_secret(MasterSecret, ConnectionStates),
    ConnStates2 =
	ssl_record:set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret,
				  Role, ConnStates1),

    ClientCipherState = #cipher_state{iv = ClientIV, key = ClientWriteKey},
    ServerCipherState = #cipher_state{iv = ServerIV, key = ServerWriteKey}, 
    {MasterSecret, 
     ssl_record:set_pending_cipher_state(ConnStates2, ClientCipherState, 
					 ServerCipherState, Role)}.


dec_hs(?HELLO_REQUEST, <<>>) ->
    #hello_request{};

%% Client hello v2.
%% The server must be able to receive such messages, from clients that
%% are willing to use ssl v3 or higher, but have ssl v2 compatibility.
dec_hs(?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor),
		       ?UINT16(CSLength), ?UINT16(0),
		       ?UINT16(CDLength), 
		       CipherSuites:CSLength/binary, 
		       ChallengeData:CDLength/binary>>) ->
    #client_hello{client_version = {Major, Minor},
		  random = ssl_ssl2:client_random(ChallengeData, CDLength),
		  session_id = 0,
		  cipher_suites = from_3bytes(CipherSuites),
		  compression_methods = [?NULL],
		  renegotiation_info = undefined
		 };
dec_hs(?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
		       ?BYTE(Cm_length), Comp_methods:Cm_length/binary,
		       Extensions/binary>>) ->
    
    RenegotiationInfo = proplists:get_value(renegotiation_info, dec_hello_extensions(Extensions),
					   undefined),    
    #client_hello{
	client_version = {Major,Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suites = from_2bytes(CipherSuites),
	compression_methods = Comp_methods,
	renegotiation_info = RenegotiationInfo 
       };

dec_hs(?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       Cipher_suite:2/binary, ?BYTE(Comp_method)>>) ->
    #server_hello{
	server_version = {Major,Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suite = Cipher_suite,
	compression_method = Comp_method,
	renegotiation_info = undefined};

dec_hs(?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       Cipher_suite:2/binary, ?BYTE(Comp_method), 
		       ?UINT16(ExtLen), Extensions:ExtLen/binary>>) ->
    
    RenegotiationInfo = proplists:get_value(renegotiation_info, dec_hello_extensions(Extensions, []),
					   undefined),   
    #server_hello{
	server_version = {Major,Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suite = Cipher_suite,
	compression_method = Comp_method,
	renegotiation_info = RenegotiationInfo};
dec_hs(?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>) ->
    #certificate{asn1_certificates = certs_to_list(ASN1Certs)};

dec_hs(?SERVER_KEY_EXCHANGE, <<?UINT16(PLen), P:PLen/binary,
			      ?UINT16(GLen), G:GLen/binary,
			      ?UINT16(YLen), Y:YLen/binary,
			       ?UINT16(0)>>) -> %% May happen if key_algorithm is dh_anon
    #server_key_exchange{params = #server_dh_params{dh_p = P,dh_g = G,
						    dh_y = Y},
			 signed_params = <<>>};
dec_hs(?SERVER_KEY_EXCHANGE, <<?UINT16(PLen), P:PLen/binary,
			      ?UINT16(GLen), G:GLen/binary,
			      ?UINT16(YLen), Y:YLen/binary,
			      ?UINT16(Len), Sig:Len/binary>>) ->
    #server_key_exchange{params = #server_dh_params{dh_p = P,dh_g = G, 
						    dh_y = Y},
			 signed_params = Sig};
dec_hs(?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>) ->
    #certificate_request{certificate_types = CertTypes,
			 certificate_authorities = CertAuths};
dec_hs(?SERVER_HELLO_DONE, <<>>) ->
    #server_hello_done{};
dec_hs(?CERTIFICATE_VERIFY,<<?UINT16(_), Signature/binary>>)->
    #certificate_verify{signature = Signature};
dec_hs(?CLIENT_KEY_EXCHANGE, PKEPMS) ->
    #client_key_exchange{exchange_keys = PKEPMS};
dec_hs(?FINISHED, VerifyData) ->
    #finished{verify_data = VerifyData};
dec_hs(_, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

dec_client_key(PKEPMS, ?KEY_EXCHANGE_RSA, {3, 0}) ->
    #encrypted_premaster_secret{premaster_secret = PKEPMS};
dec_client_key(<<?UINT16(_), PKEPMS/binary>>, ?KEY_EXCHANGE_RSA, _) ->
    #encrypted_premaster_secret{premaster_secret = PKEPMS};
dec_client_key(<<>>, ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    throw(?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE));
dec_client_key(<<?UINT16(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    #client_diffie_hellman_public{dh_public = DH_Y}.

dec_hello_extensions(<<>>) ->
    [];
dec_hello_extensions(<<?UINT16(ExtLen), Extensions:ExtLen/binary>>) ->
    dec_hello_extensions(Extensions, []);
dec_hello_extensions(_) ->
    [].

dec_hello_extensions(<<>>, Acc) ->
    Acc;
dec_hello_extensions(<<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), Info:Len/binary, Rest/binary>>, Acc) ->
    RenegotiateInfo = case Len of
			  1 ->  % Initial handshake
			      Info; % should be <<0>> will be matched in handle_renegotiation_info
			  _ ->
			      VerifyLen = Len - 1,
			      <<?BYTE(VerifyLen), VerifyInfo/binary>> = Info,
			      VerifyInfo
		      end,	    
    dec_hello_extensions(Rest, [{renegotiation_info, 
			   #renegotiation_info{renegotiated_connection = RenegotiateInfo}} | Acc]);

%% Ignore data following the ClientHello (i.e.,
%% extensions) if not understood.
dec_hello_extensions(<<?UINT16(_), ?UINT16(Len), _Unknown:Len/binary, Rest/binary>>, Acc) ->
    dec_hello_extensions(Rest, Acc);
%% This theoretically should not happen if the protocol is followed, but if it does it is ignored.
dec_hello_extensions(_, Acc) ->
    Acc.

encrypted_premaster_secret(Secret, RSAPublicKey) -> 
    try 
	PreMasterSecret = public_key:encrypt_public(Secret, RSAPublicKey, 
						    [{rsa_pad, 
						      rsa_pkcs1_padding}]),
	#encrypted_premaster_secret{premaster_secret = PreMasterSecret}
    catch
	_:_->
	    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE))
    end.

%% encode/decode stream of certificate data to/from list of certificate data 
certs_to_list(ASN1Certs) ->
    certs_to_list(ASN1Certs, []).

certs_to_list(<<?UINT24(CertLen), Cert:CertLen/binary, Rest/binary>>, Acc) ->
    certs_to_list(Rest, [Cert | Acc]);
certs_to_list(<<>>, Acc) ->
    lists:reverse(Acc, []).

certs_from_list(ACList) ->
    list_to_binary([begin
			CertLen = byte_size(Cert),
                        <<?UINT24(CertLen), Cert/binary>>
		    end || Cert <- ACList]).

enc_hs(#hello_request{}, _Version) ->
    {?HELLO_REQUEST, <<>>};
enc_hs(#client_hello{client_version = {Major, Minor},
		     random = Random,
		     session_id = SessionID,
		     cipher_suites = CipherSuites,
		     compression_methods = CompMethods, 
		     renegotiation_info = RenegotiationInfo}, _Version) ->
    SIDLength = byte_size(SessionID),
    BinCompMethods = list_to_binary(CompMethods),
    CmLength = byte_size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = byte_size(BinCipherSuites),
    Extensions  = hello_extensions(RenegotiationInfo),
    ExtensionsBin = enc_hello_extensions(Extensions),
    {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SIDLength), SessionID/binary,
		     ?UINT16(CsLength), BinCipherSuites/binary,
		     ?BYTE(CmLength), BinCompMethods/binary, ExtensionsBin/binary>>};

enc_hs(#server_hello{server_version = {Major, Minor},
		     random = Random,
		     session_id = Session_ID,
		     cipher_suite = Cipher_suite,
		     compression_method = Comp_method,
		     renegotiation_info = RenegotiationInfo}, _Version) ->
    SID_length = byte_size(Session_ID),
    Extensions  = hello_extensions(RenegotiationInfo),
    ExtensionsBin = enc_hello_extensions(Extensions),
    {?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SID_length), Session_ID/binary,
                     Cipher_suite/binary, ?BYTE(Comp_method), ExtensionsBin/binary>>};
enc_hs(#certificate{asn1_certificates = ASN1CertList}, _Version) ->
    ASN1Certs = certs_from_list(ASN1CertList),
    ACLen = erlang:iolist_size(ASN1Certs),
    {?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>};
enc_hs(#server_key_exchange{params = #server_dh_params{
			      dh_p = P, dh_g = G, dh_y = Y},
	signed_params = SignedParams}, _Version) ->
    PLen = byte_size(P),
    GLen = byte_size(G),
    YLen = byte_size(Y),
    SignedLen = byte_size(SignedParams),
    {?SERVER_KEY_EXCHANGE, <<?UINT16(PLen), P/binary, 
			    ?UINT16(GLen), G/binary,
			    ?UINT16(YLen), Y/binary,
			    ?UINT16(SignedLen), SignedParams/binary>>
    };
enc_hs(#certificate_request{certificate_types = CertTypes,
			    certificate_authorities = CertAuths}, 
       _Version) ->
    CertTypesLen = byte_size(CertTypes),
    CertAuthsLen = byte_size(CertAuths),
    {?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes/binary,
	?UINT16(CertAuthsLen), CertAuths/binary>>
    };
enc_hs(#server_hello_done{}, _Version) ->
    {?SERVER_HELLO_DONE, <<>>};
enc_hs(#client_key_exchange{exchange_keys = ExchangeKeys}, Version) ->
    {?CLIENT_KEY_EXCHANGE, enc_cke(ExchangeKeys, Version)};
enc_hs(#certificate_verify{signature = BinSig}, _) ->
    EncSig = enc_bin_sig(BinSig),
    {?CERTIFICATE_VERIFY, EncSig};
enc_hs(#finished{verify_data = VerifyData}, _Version) ->
    {?FINISHED, VerifyData}.

enc_cke(#encrypted_premaster_secret{premaster_secret = PKEPMS},{3, 0}) ->
    PKEPMS;
enc_cke(#encrypted_premaster_secret{premaster_secret = PKEPMS}, _) ->
    PKEPMSLen = byte_size(PKEPMS),
    <<?UINT16(PKEPMSLen), PKEPMS/binary>>;
enc_cke(#client_diffie_hellman_public{dh_public = DHPublic}, _) ->
    Len = byte_size(DHPublic),
    <<?UINT16(Len), DHPublic/binary>>.

enc_bin_sig(BinSig) ->
    Size = byte_size(BinSig),
    <<?UINT16(Size), BinSig/binary>>.

%% Renegotiation info, only current extension
hello_extensions(#renegotiation_info{renegotiated_connection = undefined}) ->
    [];
hello_extensions(#renegotiation_info{} = Info) ->
    [Info].

enc_hello_extensions(Extensions) ->
    enc_hello_extensions(Extensions, <<>>).
enc_hello_extensions([], <<>>) ->
    <<>>;
enc_hello_extensions([], Acc) ->
    Size = byte_size(Acc),
    <<?UINT16(Size), Acc/binary>>;

enc_hello_extensions([#renegotiation_info{renegotiated_connection = ?byte(0) = Info} | Rest], Acc) ->
    Len = byte_size(Info),
    enc_hello_extensions(Rest, <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), Info/binary, Acc/binary>>);

enc_hello_extensions([#renegotiation_info{renegotiated_connection = Info} | Rest], Acc) ->
    InfoLen = byte_size(Info),
    Len = InfoLen +1,
    enc_hello_extensions(Rest, <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), ?BYTE(InfoLen), Info/binary, Acc/binary>>).


from_3bytes(Bin3) ->
    from_3bytes(Bin3, []).

from_3bytes(<<>>, Acc) ->
    lists:reverse(Acc);
from_3bytes(<<?UINT24(N), Rest/binary>>, Acc) ->
    from_3bytes(Rest, [?uint16(N) | Acc]).

from_2bytes(Bin2) ->
    from_2bytes(Bin2, []).

from_2bytes(<<>>, Acc) ->
    lists:reverse(Acc);
from_2bytes(<<?UINT16(N), Rest/binary>>, Acc) ->
    from_2bytes(Rest, [?uint16(N) | Acc]).

certificate_types({KeyExchange, _, _, _})  
  when KeyExchange == rsa;
       KeyExchange == dhe_dss;
       KeyExchange == dhe_rsa ->
    <<?BYTE(?RSA_SIGN), ?BYTE(?DSS_SIGN)>>;

certificate_types(_) ->
    <<?BYTE(?RSA_SIGN)>>.

certificate_authorities(CertDbHandle, CertDbRef) ->
    Authorities = certificate_authorities_from_db(CertDbHandle, CertDbRef),
    Enc = fun(#'OTPCertificate'{tbsCertificate=TBSCert}) ->
		  OTPSubj = TBSCert#'OTPTBSCertificate'.subject,
		  DNEncodedBin = public_key:pkix_encode('Name', OTPSubj, otp),
		  %%Subj = public_key:pkix_transform(OTPSubj, encode),
		  %% {ok, DNEncoded} = 'OTP-PUB-KEY':encode('Name', Subj),
		  %% DNEncodedBin = iolist_to_binary(DNEncoded),
		  DNEncodedLen = byte_size(DNEncodedBin),
		  <<?UINT16(DNEncodedLen), DNEncodedBin/binary>>
	  end,
    list_to_binary([Enc(Cert) || {_, Cert} <- Authorities]).

certificate_authorities_from_db(CertDbHandle, CertDbRef) ->
    ConnectionCerts = fun({{Ref, _, _}, Cert}, Acc) when Ref  == CertDbRef ->
			      [Cert | Acc];
			 (_, Acc) ->
			      Acc
		      end,	
    ssl_certificate_db:foldl(ConnectionCerts, [], CertDbHandle).

digitally_signed(Hash, #'RSAPrivateKey'{} = Key) ->
    public_key:encrypt_private(Hash, Key,
			       [{rsa_pad, rsa_pkcs1_padding}]);
digitally_signed(Hash, #'DSAPrivateKey'{} = Key) ->
    public_key:sign(Hash, none, Key).
    
calc_master_secret({3,0}, PremasterSecret, ClientRandom, ServerRandom) ->
    ssl_ssl3:master_secret(PremasterSecret, ClientRandom, ServerRandom);

calc_master_secret({3,N},PremasterSecret, ClientRandom, ServerRandom) 
  when N == 1; N == 2 ->
    ssl_tls1:master_secret(PremasterSecret, ClientRandom, ServerRandom).

setup_keys({3,0}, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, EKML, IVS) ->
    ssl_ssl3:setup_keys(MasterSecret, ServerRandom, 
			ClientRandom, HashSize, KML, EKML, IVS);

setup_keys({3,1}, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, _EKML, IVS) ->
    ssl_tls1:setup_keys(MasterSecret, ServerRandom, ClientRandom, HashSize, 
			KML, IVS).

calc_finished({3, 0}, Role, MasterSecret, Hashes) ->
    ssl_ssl3:finished(Role, MasterSecret, Hashes);
calc_finished({3, N}, Role, MasterSecret, Hashes) 
  when  N == 1; N == 2 ->
    ssl_tls1:finished(Role, MasterSecret, Hashes).

calc_certificate_verify({3, 0}, MasterSecret, Algorithm, Hashes) ->
    ssl_ssl3:certificate_verify(Algorithm, MasterSecret, Hashes);
calc_certificate_verify({3, N}, _, Algorithm, Hashes) 
  when  N == 1; N == 2 ->
    ssl_tls1:certificate_verify(Algorithm, Hashes).

key_exchange_alg(rsa) ->
    ?KEY_EXCHANGE_RSA;
key_exchange_alg(Alg) when Alg == dhe_rsa; Alg == dhe_dss;
			    Alg == dh_dss; Alg == dh_rsa; Alg == dh_anon ->
    ?KEY_EXCHANGE_DIFFIE_HELLMAN;
key_exchange_alg(_) ->
    ?NULL.

apply_user_fun(Fun, OtpCert, ExtensionOrError, UserState0, SslState) ->
    case Fun(OtpCert, ExtensionOrError, UserState0) of
	{valid, UserState} ->
	    {valid, {SslState, UserState}};
	{fail, _} = Fail ->
	    Fail;
	{unknown, UserState} ->
	    {unknown, {SslState, UserState}}
    end.

alg_oid(#'RSAPrivateKey'{}) ->
    ?'rsaEncryption';
alg_oid(#'DSAPrivateKey'{}) ->
    ?'id-dsa'.
