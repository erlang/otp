%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
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
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([master_secret/4, client_hello/8, server_hello/5, hello/4,
	 hello_request/0, certify/7, certificate/4,
	 client_certificate_verify/6, certificate_verify/6, verify_signature/5,
	 certificate_request/3, key_exchange/3, server_key_exchange_hash/2,
	 finished/5, verify_connection/6, get_tls_handshake/3,
	 decode_client_key/3, decode_server_key/3, server_hello_done/0,
	 encode_handshake/2, init_handshake_history/0, update_handshake_history/2,
	 decrypt_premaster_secret/2, prf/5, next_protocol/1]).

-export([dec_hello_extensions/2]).

-type tls_handshake() :: #client_hello{} | #server_hello{} |
			 #server_hello_done{} | #certificate{} | #certificate_request{} |
			 #client_key_exchange{} | #finished{} | #certificate_verify{} |
			 #hello_request{} | #next_protocol{}.

%%====================================================================
%% Internal application API
%%====================================================================
%%--------------------------------------------------------------------
-spec client_hello(host(), inet:port_number(), #connection_states{},
		   #ssl_options{}, integer(), atom(), boolean(), der_cert()) ->
			  #client_hello{}.
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(Host, Port, ConnectionStates,
	     #ssl_options{versions = Versions,
			  ciphers = UserSuites
			 } = SslOpts,
	     Cache, CacheCb, Renegotiation, OwnCert) ->

    Fun = fun(Version) ->
		  ssl_record:protocol_version(Version)
	  end,
    Version = ssl_record:highest_protocol_version(lists:map(Fun, Versions)),
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
    Ciphers = available_suites(UserSuites, Version),
    SRP = srp_user(SslOpts),

    Id = ssl_session:client_id({Host, Port, SslOpts}, Cache, CacheCb, OwnCert),

    #client_hello{session_id = Id,
		  client_version = Version,
		  cipher_suites = cipher_suites(Ciphers, Renegotiation),
		  compression_methods = ssl_record:compressions(),
		  random = SecParams#security_parameters.client_random,

		  renegotiation_info =
		      renegotiation_info(client, ConnectionStates, Renegotiation),
		  srp = SRP,
		  hash_signs = default_hash_signs(),
		  next_protocol_negotiation =
		      encode_client_protocol_negotiation(SslOpts#ssl_options.next_protocol_selector, Renegotiation)
		 }.

encode_protocol(Protocol, Acc) ->
	Len = byte_size(Protocol),
	<<Acc/binary, ?BYTE(Len), Protocol/binary>>.

encode_protocols_advertised_on_server(undefined) ->
	undefined;

encode_protocols_advertised_on_server(Protocols) ->
	#next_protocol_negotiation{extension_data = lists:foldl(fun encode_protocol/2, <<>>, Protocols)}.

%%--------------------------------------------------------------------
-spec server_hello(session_id(), tls_version(), #connection_states{}, 
		   boolean(), [binary()] | undefined) -> #server_hello{}.
%%
%% Description: Creates a server hello message.
%%--------------------------------------------------------------------
server_hello(SessionId, Version, ConnectionStates, Renegotiation, ProtocolsAdvertisedOnServer) ->
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
    #server_hello{server_version = Version,
		  cipher_suite = SecParams#security_parameters.cipher_suite,
                  compression_method = 
		  SecParams#security_parameters.compression_algorithm,
		  random = SecParams#security_parameters.server_random,
		  session_id = SessionId,
		  renegotiation_info = 
		  renegotiation_info(server, ConnectionStates, Renegotiation),
		  next_protocol_negotiation = encode_protocols_advertised_on_server(ProtocolsAdvertisedOnServer)
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
	    boolean()) ->
			  {tls_version(), session_id(), #connection_states{}, binary() | undefined}|
			  {tls_version(), {resumed | new, #session{}}, #connection_states{}, list(binary()) | undefined} |
			  #alert{}.
%%
%% Description: Handles a recieved hello message
%%--------------------------------------------------------------------
hello(#server_hello{cipher_suite = CipherSuite, server_version = Version,
		    compression_method = Compression, random = Random,
		    session_id = SessionId, renegotiation_info = Info,
		    hash_signs = _HashSigns} = Hello,
      #ssl_options{secure_renegotiate = SecureRenegotation, next_protocol_selector = NextProtocolSelector},
      ConnectionStates0, Renegotiation) ->
    %%TODO: select hash and signature algorigthm
    case ssl_record:is_acceptable_version(Version) of
	true ->
	    case handle_renegotiation_info(client, Info, ConnectionStates0, 
					   Renegotiation, SecureRenegotation, []) of
		{ok, ConnectionStates1} ->
		    ConnectionStates =
			hello_pending_connection_states(client, Version, CipherSuite, Random,
							Compression, ConnectionStates1),
		    case handle_next_protocol(Hello, NextProtocolSelector, Renegotiation) of
			#alert{} = Alert ->
			    Alert;
			    Protocol ->
			    {Version, SessionId, ConnectionStates, Protocol}
		    end;
		#alert{} = Alert ->
		    Alert
	    end;
	false ->
	    ?ALERT_REC(?FATAL, ?PROTOCOL_VERSION)
    end;
			       
hello(#client_hello{client_version = ClientVersion, random = Random,
		    cipher_suites = CipherSuites,
		    renegotiation_info = Info,
		    srp = SRP} = Hello,
      #ssl_options{versions = Versions,
		   secure_renegotiate = SecureRenegotation} = SslOpts,
      {Port, Session0, Cache, CacheCb, ConnectionStates0, Cert}, Renegotiation) ->
%% TODO: select hash and signature algorithm
    Version = select_version(ClientVersion, Versions),
    case ssl_record:is_acceptable_version(Version) of
	true ->
	    {Type, #session{cipher_suite = CipherSuite,
			    compression_method = Compression} = Session1}
		= select_session(Hello, Port, Session0, Version, 
				 SslOpts, Cache, CacheCb, Cert),
	    case CipherSuite of 
		no_suite ->
		    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY);
		_ ->
		    Session = handle_srp_info(SRP, Session1),
		    case handle_renegotiation_info(server, Info, ConnectionStates0,
						   Renegotiation, SecureRenegotation, 
						   CipherSuites) of
			{ok, ConnectionStates1} ->
			    ConnectionStates =
				hello_pending_connection_states(server, 
								Version,
								CipherSuite,
								Random, 
								Compression,
								ConnectionStates1),
				case handle_next_protocol_on_server(Hello, Renegotiation, SslOpts) of
					#alert{} = Alert ->
						Alert;
					ProtocolsToAdvertise ->
						{Version, {Type, Session}, ConnectionStates, ProtocolsToAdvertise}
			    end;
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
		{fun(OtpCert, ExtensionOrVerifyResult, SslState) ->
			 ssl_certificate:validate_extension(OtpCert,
							    ExtensionOrVerifyResult, SslState)
		 end, Role};
	    {Fun, UserState0} ->
		{fun(OtpCert, {extension, _} = Extension, {SslState, UserState}) ->
			 case ssl_certificate:validate_extension(OtpCert,
								 Extension,
								 SslState) of
			     {valid, NewSslState} ->
				 {valid, {NewSslState, UserState}};
			     {fail, Reason} ->
				 apply_user_fun(Fun, OtpCert, Reason, UserState,
						SslState);
			     {unknown, _} ->
				 apply_user_fun(Fun, OtpCert,
						Extension, UserState, SslState)
			 end;
		    (OtpCert, VerifyResult, {SslState, UserState}) ->
			 apply_user_fun(Fun, OtpCert, VerifyResult, UserState,
					SslState)
		 end, {Role, UserState0}}
	end,

    try
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
	end
    catch
	error:_ ->
	    %% ASN-1 decode of certificate somehow failed
	    ?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN)
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
				tls_version(), term(), private_key(),
				tls_handshake_history()) ->
    #certificate_verify{} | ignore | #alert{}.
%%
%% Description: Creates a certificate_verify message, called by the client.
%%--------------------------------------------------------------------
client_certificate_verify(undefined, _, _, _, _, _) ->
    ignore;
client_certificate_verify(_, _, _, _, undefined, _) ->
    ignore;
client_certificate_verify(OwnCert, MasterSecret, Version,
			  {HashAlgo, SignAlgo},
			  PrivateKey, {Handshake, _}) ->
    case public_key:pkix_is_fixed_dh_cert(OwnCert) of
	true ->
	    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE);
	false ->
	    Hashes =
		calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
	    Signed = digitally_signed(Version, Hashes, HashAlgo, PrivateKey),
	    #certificate_verify{signature = Signed, hashsign_algorithm = {HashAlgo, SignAlgo}}
    end.

%%--------------------------------------------------------------------
-spec certificate_verify(binary(), public_key_info(), tls_version(), term(),
			 binary(), tls_handshake_history()) -> valid | #alert{}.
%%
%% Description: Checks that the certificate_verify message is valid.
%%--------------------------------------------------------------------
certificate_verify(Signature, PublicKeyInfo, Version,
		   HashSign = {HashAlgo, _}, MasterSecret, {_, Handshake}) ->
    Hash = calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
    case verify_signature(Version, Hash, HashSign, Signature, PublicKeyInfo) of
	true ->
	    valid;
	_ ->
    	    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE)
    end.

%%--------------------------------------------------------------------
-spec verify_signature(tls_version(), binary(), {term(), term()}, binary(),
				   public_key_info()) -> true | false.
%%
%% Description: Checks that a public_key signature is valid.
%%--------------------------------------------------------------------
verify_signature(_Version, _Hash, {_HashAlgo, anon}, _Signature, _) ->
    true;
verify_signature({3, Minor}, Hash, {HashAlgo, rsa}, Signature, {?rsaEncryption, PubKey, _PubKeyParams})
  when Minor >= 3 ->
    public_key:verify({digest, Hash}, HashAlgo, Signature, PubKey);
verify_signature(_Version, Hash, _HashAlgo, Signature, {?rsaEncryption, PubKey, _PubKeyParams}) ->
    case public_key:decrypt_public(Signature, PubKey,
				   [{rsa_pad, rsa_pkcs1_padding}]) of
	Hash -> true;
	_    -> false
    end;
verify_signature(_Version, Hash, {HashAlgo, dsa}, Signature, {?'id-dsa', PublicKey, PublicKeyParams}) ->
    public_key:verify({digest, Hash}, HashAlgo, Signature, {PublicKey, PublicKeyParams}).


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
    HashSigns = default_hash_signs(),
    Authorities = certificate_authorities(CertDbHandle, CertDbRef),
    #certificate_request{
		    certificate_types = Types,
		    hashsign_algorithms = HashSigns,
		    certificate_authorities = Authorities
		   }.

%%--------------------------------------------------------------------
-spec key_exchange(client | server, tls_version(),
		   {premaster_secret, binary(), public_key_info()} |
		   {dh, binary()} |
		   {dh, {binary(), binary()}, #'DHParameter'{}, {HashAlgo::atom(), SignAlgo::atom()},
		   binary(), binary(), private_key()} |
		   {psk, binary()} |
		   {dhe_psk, binary(), binary()} |
		   {srp, {binary(), binary()}, #srp_user{}, {HashAlgo::atom(), SignAlgo::atom()},
		   binary(), binary(), private_key()}) ->
    #client_key_exchange{} | #server_key_exchange{}.
%%
%% Description: Creates a keyexchange message.
%%--------------------------------------------------------------------
key_exchange(client, _Version, {premaster_secret, Secret, {_, PublicKey, _}}) ->
    EncPremasterSecret =
	encrypted_premaster_secret(Secret, PublicKey),
    #client_key_exchange{exchange_keys = EncPremasterSecret};

key_exchange(client, _Version, {dh, <<?UINT32(Len), PublicKey:Len/binary>>}) ->
    #client_key_exchange{
	      exchange_keys = #client_diffie_hellman_public{
		dh_public = PublicKey}
	       };

key_exchange(client, _Version, {psk, Identity}) ->
    #client_key_exchange{
	      exchange_keys = #client_psk_identity{
		identity = Identity}
	       };

key_exchange(client, _Version, {dhe_psk, Identity, <<?UINT32(Len), PublicKey:Len/binary>>}) ->
    #client_key_exchange{
	      exchange_keys = #client_dhe_psk_identity{
		identity = Identity,
		dh_public = PublicKey}
	       };

key_exchange(client, _Version, {psk_premaster_secret, PskIdentity, Secret, {_, PublicKey, _}}) ->
    EncPremasterSecret =
	encrypted_premaster_secret(Secret, PublicKey),
    #client_key_exchange{
		exchange_keys = #client_rsa_psk_identity{
		  identity = PskIdentity,
		  exchange_keys = EncPremasterSecret}};

key_exchange(client, _Version, {srp, PublicKey}) ->
    #client_key_exchange{
	      exchange_keys = #client_srp_public{
		srp_a = PublicKey}
	       };

key_exchange(server, Version, {dh, {<<?UINT32(Len), PublicKey:Len/binary>>, _},
		      #'DHParameter'{prime = P, base = G},
		      HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    <<?UINT32(_), PBin/binary>> = crypto:mpint(P),
    <<?UINT32(_), GBin/binary>> = crypto:mpint(G),
    ServerDHParams = #server_dh_params{dh_p = PBin, 
				       dh_g = GBin, dh_y = PublicKey},    
    enc_server_key_exchange(Version, ServerDHParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {psk, PskIdentityHint,
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerPSKParams = #server_psk_params{hint = PskIdentityHint},
    enc_server_key_exchange(Version, ServerPSKParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {dhe_psk, PskIdentityHint, {<<?UINT32(Len), PublicKey:Len/binary>>, _},
			       #'DHParameter'{prime = P, base = G},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    <<?UINT32(_), PBin/binary>> = crypto:mpint(P),
    <<?UINT32(_), GBin/binary>> = crypto:mpint(G),
    ServerEDHPSKParams = #server_dhe_psk_params{
      hint = PskIdentityHint,
      dh_params = #server_dh_params{dh_p = PBin,
				    dh_g = GBin, dh_y = PublicKey}
     },
    enc_server_key_exchange(Version, ServerEDHPSKParams,
			    HashSign, ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {srp, {PublicKey, _},
			       #srp_user{generator = Generator, prime = Prime,
					 salt = Salt},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerSRPParams = #server_srp_params{srp_n = Prime, srp_g = Generator,
					 srp_s = Salt, srp_b = PublicKey},
    enc_server_key_exchange(Version, ServerSRPParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey).

enc_server_key_exchange(Version, Params, {HashAlgo, SignAlgo},
			ClientRandom, ServerRandom, PrivateKey) ->
    EncParams = enc_server_key(Params),
    case HashAlgo of
	null ->
	    #server_key_params{params = Params,
			       params_bin = EncParams,
			       hashsign = {null, anon},
			       signature = <<>>};
	_ ->
	    Hash =
		server_key_exchange_hash(HashAlgo, <<ClientRandom/binary,
						     ServerRandom/binary,
						     EncParams/binary>>),
	    Signature = digitally_signed(Version, Hash, HashAlgo, PrivateKey),
	    #server_key_params{params = Params,
			       params_bin = EncParams,
			       hashsign = {HashAlgo, SignAlgo},
			       signature = Signature}
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
    #security_parameters{prf_algorithm = PrfAlgo,
			 client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams, 
    try master_secret(Version, 
		      calc_master_secret(Version,PrfAlgo,PremasterSecret,
				       ClientRandom, ServerRandom),
		      SecParams, ConnectionStates, Role) 
    catch
	exit:Reason ->
	    Report = io_lib:format("Master secret calculation failed"
				   " due to ~p", [Reason]),
	    error_logger:error_report(Report),
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
    end.

-spec next_protocol(binary()) -> #next_protocol{}.

next_protocol(SelectedProtocol) ->
  #next_protocol{selected_protocol = SelectedProtocol}.

%%--------------------------------------------------------------------
-spec finished(tls_version(), client | server, integer(), binary(), tls_handshake_history()) ->
    #finished{}.
%%
%% Description: Creates a handshake finished message
%%-------------------------------------------------------------------
finished(Version, Role, PrfAlgo, MasterSecret, {Handshake, _}) -> % use the current handshake
    #finished{verify_data = 
	      calc_finished(Version, Role, PrfAlgo, MasterSecret, Handshake)}.

%%--------------------------------------------------------------------
-spec verify_connection(tls_version(), #finished{}, client | server, integer(), binary(),
			tls_handshake_history()) -> verified | #alert{}.
%%
%% Description: Checks the ssl handshake finished message to verify
%%              the connection.
%%-------------------------------------------------------------------
verify_connection(Version, #finished{verify_data = Data}, 
		  Role, PrfAlgo, MasterSecret, {_, Handshake}) ->
    %% use the previous hashes
    case calc_finished(Version, Role, PrfAlgo, MasterSecret, Handshake) of
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
%%--------------------------------------------------------------------x
encode_handshake(Package, Version) ->
    {MsgType, Bin} = enc_hs(Package, Version),
    Len = byte_size(Bin),
    [MsgType, ?uint24(Len), Bin].

%%--------------------------------------------------------------------
-spec get_tls_handshake(tls_version(), binary(), binary() | iolist()) ->
     {[tls_handshake()], binary()}.
%%
%% Description: Given buffered and new data from ssl_record, collects
%% and returns it as a list of handshake messages, also returns leftover
%% data.
%%--------------------------------------------------------------------
get_tls_handshake(Version, Data, <<>>) ->
    get_tls_handshake_aux(Version, Data, []);
get_tls_handshake(Version, Data, Buffer) ->
    get_tls_handshake_aux(Version, list_to_binary([Buffer, Data]), []).

%%--------------------------------------------------------------------
-spec decode_client_key(binary(), key_algo(), tls_version()) ->
			    #encrypted_premaster_secret{}
			    | #client_diffie_hellman_public{}
			    | #client_psk_identity{}
			    | #client_dhe_psk_identity{}
			    | #client_rsa_psk_identity{}
			    | #client_srp_public{}.
%%
%% Description: Decode client_key data and return appropriate type
%%--------------------------------------------------------------------
decode_client_key(ClientKey, Type, Version) ->
    dec_client_key(ClientKey, key_exchange_alg(Type), Version).

%%--------------------------------------------------------------------
-spec decode_server_key(binary(), key_algo(), tls_version()) ->
			       #server_key_params{}.
%%
%% Description: Decode server_key data and return appropriate type
%%--------------------------------------------------------------------
decode_server_key(ServerKey, Type, Version) ->
    dec_server_key(ServerKey, key_exchange_alg(Type), Version).

%%--------------------------------------------------------------------
-spec init_handshake_history() -> tls_handshake_history().

%%
%% Description: Initialize the empty handshake history buffer.
%%--------------------------------------------------------------------
init_handshake_history() ->
    {[], []}.

%%--------------------------------------------------------------------
-spec update_handshake_history(tls_handshake_history(), Data ::term()) ->
				      tls_handshake_history().
%%
%% Description: Update the handshake history buffer with Data.
%%--------------------------------------------------------------------
update_handshake_history(Handshake, % special-case SSL2 client hello
			 <<?CLIENT_HELLO, ?UINT24(_), ?BYTE(Major), ?BYTE(Minor),
			   ?UINT16(CSLength), ?UINT16(0),
			   ?UINT16(CDLength),
			   CipherSuites:CSLength/binary,
			   ChallengeData:CDLength/binary>>) ->
    update_handshake_history(Handshake,
			     <<?CLIENT_HELLO, ?BYTE(Major), ?BYTE(Minor),
			       ?UINT16(CSLength), ?UINT16(0),
			       ?UINT16(CDLength),
			       CipherSuites:CSLength/binary,
			       ChallengeData:CDLength/binary>>);
update_handshake_history({Handshake0, _Prev}, Data) ->
    {[Data|Handshake0], Handshake0}.

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
			io:format("decrypt_premaster_secret error"),
	    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
    end.

%%--------------------------------------------------------------------
-spec server_key_exchange_hash(md5sha | md5 | sha | sha224 |sha256 | sha384 | sha512, binary()) -> binary().
%%
%% Description: Calculate server key exchange hash
%%--------------------------------------------------------------------
server_key_exchange_hash(md5sha, Value) ->
    MD5 = crypto:md5(Value),
    SHA = crypto:sha(Value),
    <<MD5/binary, SHA/binary>>;

server_key_exchange_hash(Hash, Value) ->
    crypto:hash(Hash, Value).

%%--------------------------------------------------------------------
-spec prf(tls_version(), binary(), binary(), [binary()], non_neg_integer()) ->
		 {ok, binary()} | {error, undefined}.
%%
%% Description: use the TLS PRF to generate key material
%%--------------------------------------------------------------------
prf({3,0}, _, _, _, _) ->
    {error, undefined};
prf({3,1}, Secret, Label, Seed, WantedLength) ->
    {ok, ssl_tls1:prf(?MD5SHA, Secret, Label, Seed, WantedLength)};
prf({3,_N}, Secret, Label, Seed, WantedLength) ->
    {ok, ssl_tls1:prf(?SHA256, Secret, Label, Seed, WantedLength)}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_tls_handshake_aux(Version, <<?BYTE(Type), ?UINT24(Length),
			Body:Length/binary,Rest/binary>>, Acc) ->
    Raw = <<?BYTE(Type), ?UINT24(Length), Body/binary>>,
    H = dec_hs(Version, Type, Body),
    get_tls_handshake_aux(Version, Rest, [{H,Raw} | Acc]);
get_tls_handshake_aux(_Version, Data, Acc) ->
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
    {SessionId, Resumed} = ssl_session:server_id(Port, SuggestedSessionId,
						 SslOpts, Cert,
						 Cache, CacheCb),
    Suites = available_suites(Cert, UserSuites, Version),
    case Resumed of
        undefined ->
	    CipherSuite = select_cipher_suite(Hello#client_hello.cipher_suites, Suites),
	    Compressions = Hello#client_hello.compression_methods,
	    Compression = select_compression(Compressions),
	    {new, Session#session{session_id = SessionId,
				  cipher_suite = CipherSuite,
				  compression_method = Compression}};
	_ ->
	    {resumed, Resumed}
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

srp_user(#ssl_options{srp_identity = {UserName, _}}) ->
    #srp{username = UserName};
srp_user(_) ->
    undefined.

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

decode_next_protocols({next_protocol_negotiation, Protocols}) ->
    decode_next_protocols(Protocols, []).
decode_next_protocols(<<>>, Acc) ->
    lists:reverse(Acc);
decode_next_protocols(<<?BYTE(Len), Protocol:Len/binary, Rest/binary>>, Acc) ->
    case Len of
        0 ->
            {error, invalid_next_protocols};
        _ ->
            decode_next_protocols(Rest, [Protocol|Acc])
    end;
decode_next_protocols(_Bytes, _Acc) ->
    {error, invalid_next_protocols}.

next_protocol_extension_allowed(NextProtocolSelector, Renegotiating) ->
    NextProtocolSelector =/= undefined andalso not Renegotiating.

handle_next_protocol_on_server(#client_hello{next_protocol_negotiation = undefined}, _Renegotiation, _SslOpts) ->
    undefined;

handle_next_protocol_on_server(#client_hello{next_protocol_negotiation = {next_protocol_negotiation, <<>>}},
			       false, #ssl_options{next_protocols_advertised = Protocols}) ->
    Protocols;

handle_next_protocol_on_server(_Hello, _Renegotiation, _SSLOpts) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE). % unexpected next protocol extension

handle_next_protocol(#server_hello{next_protocol_negotiation = undefined},
    _NextProtocolSelector, _Renegotiating) ->
    undefined;

handle_next_protocol(#server_hello{next_protocol_negotiation = Protocols},
    NextProtocolSelector, Renegotiating) ->

    case next_protocol_extension_allowed(NextProtocolSelector, Renegotiating) of
        true ->
            select_next_protocol(decode_next_protocols(Protocols), NextProtocolSelector);
        false ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE) % unexpected next protocol extension
    end.

select_next_protocol({error, _Reason}, _NextProtocolSelector) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
select_next_protocol(Protocols, NextProtocolSelector) ->
    case NextProtocolSelector(Protocols) of
	?NO_PROTOCOL ->
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE);
	Protocol when is_binary(Protocol)  ->
	    Protocol
    end.

handle_srp_info(undefined, Session) ->
    Session;
handle_srp_info(#srp{username = Username}, Session) ->
    Session#session{srp_username = Username}.

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
hello_pending_connection_states(Role, Version, CipherSuite, Random, Compression,
				 ConnectionStates) ->    
    ReadState =  
	ssl_record:pending_connection_state(ConnectionStates, read),
    WriteState = 
	ssl_record:pending_connection_state(ConnectionStates, write),
    
    NewReadSecParams = 
	hello_security_parameters(Role, Version, ReadState, CipherSuite,
			    Random, Compression),
    
    NewWriteSecParams =
	hello_security_parameters(Role, Version, WriteState, CipherSuite,
			    Random, Compression),
 
    ssl_record:update_security_params(NewReadSecParams,
				    NewWriteSecParams,
				    ConnectionStates).

hello_security_parameters(client, Version, ConnectionState, CipherSuite, Random,
			  Compression) ->   
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(Version, CipherSuite, SecParams),
    NewSecParams#security_parameters{
      server_random = Random,
      compression_algorithm = Compression
     };

hello_security_parameters(server, Version, ConnectionState, CipherSuite, Random,
			  Compression) ->
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(Version, CipherSuite, SecParams),
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
			 prf_algorithm = PrfAlgo,
			 key_material_length = KML,
			 expanded_key_material_length = EKML,
			 iv_size = IVS},
	      ConnectionStates, Role) ->
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV} =
	setup_keys(Version, PrfAlgo, MasterSecret, ServerRandom,
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


dec_hs(_, ?NEXT_PROTOCOL, <<?BYTE(SelectedProtocolLength), SelectedProtocol:SelectedProtocolLength/binary,
                         ?BYTE(PaddingLength), _Padding:PaddingLength/binary>>) ->
	#next_protocol{selected_protocol = SelectedProtocol};

dec_hs(_, ?HELLO_REQUEST, <<>>) ->
    #hello_request{};

%% Client hello v2.
%% The server must be able to receive such messages, from clients that
%% are willing to use ssl v3 or higher, but have ssl v2 compatibility.
dec_hs(_Version, ?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor),
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
dec_hs(_Version, ?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
		       ?BYTE(Cm_length), Comp_methods:Cm_length/binary,
		       Extensions/binary>>) ->

    DecodedExtensions = dec_hello_extensions(Extensions),
    RenegotiationInfo = proplists:get_value(renegotiation_info, DecodedExtensions, undefined),
    SRP = proplists:get_value(srp, DecodedExtensions, undefined),
    HashSigns = proplists:get_value(hash_signs, DecodedExtensions, undefined),
    NextProtocolNegotiation = proplists:get_value(next_protocol_negotiation, DecodedExtensions, undefined),

    #client_hello{
       client_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suites = from_2bytes(CipherSuites),
       compression_methods = Comp_methods,
       renegotiation_info = RenegotiationInfo,
	srp = SRP,
       hash_signs = HashSigns,
       next_protocol_negotiation = NextProtocolNegotiation
      };

dec_hs(_Version, ?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       Cipher_suite:2/binary, ?BYTE(Comp_method)>>) ->
    #server_hello{
	server_version = {Major,Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suite = Cipher_suite,
	compression_method = Comp_method,
	renegotiation_info = undefined,
	hash_signs = undefined};

dec_hs(_Version, ?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       Cipher_suite:2/binary, ?BYTE(Comp_method),
		       ?UINT16(ExtLen), Extensions:ExtLen/binary>>) ->
    
    HelloExtensions = dec_hello_extensions(Extensions, []),
    RenegotiationInfo = proplists:get_value(renegotiation_info, HelloExtensions,
					   undefined),
    HashSigns = proplists:get_value(hash_signs, HelloExtensions,
					   undefined),
    NextProtocolNegotiation = proplists:get_value(next_protocol_negotiation, HelloExtensions, undefined),

    #server_hello{
	server_version = {Major,Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suite = Cipher_suite,
	compression_method = Comp_method,
	renegotiation_info = RenegotiationInfo,
	hash_signs = HashSigns,
       next_protocol_negotiation = NextProtocolNegotiation};
dec_hs(_Version, ?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>) ->
    #certificate{asn1_certificates = certs_to_list(ASN1Certs)};
dec_hs(_Version, ?SERVER_KEY_EXCHANGE, Keys) ->
    #server_key_exchange{exchange_keys = Keys};
dec_hs({Major, Minor}, ?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(HashSignsLen), HashSigns:HashSignsLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>)
  when Major == 3, Minor >= 3 ->
    HashSignAlgos = [{ssl_cipher:hash_algorithm(Hash), ssl_cipher:sign_algorithm(Sign)} ||
			<<?BYTE(Hash), ?BYTE(Sign)>> <= HashSigns],
    #certificate_request{certificate_types = CertTypes,
			 hashsign_algorithms = #hash_sign_algos{hash_sign_algos = HashSignAlgos},
			 certificate_authorities = CertAuths};
dec_hs(_Version, ?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>) ->
    #certificate_request{certificate_types = CertTypes,
			 certificate_authorities = CertAuths};
dec_hs(_Version, ?SERVER_HELLO_DONE, <<>>) ->
    #server_hello_done{};
dec_hs({Major, Minor}, ?CERTIFICATE_VERIFY,<<HashSign:2/binary, ?UINT16(SignLen), Signature:SignLen/binary>>)
  when Major == 3, Minor >= 3 ->
    #certificate_verify{hashsign_algorithm = hashsign_dec(HashSign), signature = Signature};
dec_hs(_Version, ?CERTIFICATE_VERIFY,<<?UINT16(SignLen), Signature:SignLen/binary>>)->
    #certificate_verify{signature = Signature};
dec_hs(_Version, ?CLIENT_KEY_EXCHANGE, PKEPMS) ->
    #client_key_exchange{exchange_keys = PKEPMS};
dec_hs(_Version, ?FINISHED, VerifyData) ->
    #finished{verify_data = VerifyData};
dec_hs(_, _, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

dec_client_key(PKEPMS, ?KEY_EXCHANGE_RSA, {3, 0}) ->
    #encrypted_premaster_secret{premaster_secret = PKEPMS};
dec_client_key(<<?UINT16(_), PKEPMS/binary>>, ?KEY_EXCHANGE_RSA, _) ->
    #encrypted_premaster_secret{premaster_secret = PKEPMS};
dec_client_key(<<>>, ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    throw(?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE));
dec_client_key(<<?UINT16(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    #client_diffie_hellman_public{dh_public = DH_Y};
dec_client_key(<<?UINT16(Len), Id:Len/binary>>,
	       ?KEY_EXCHANGE_PSK, _) ->
    #client_psk_identity{identity = Id};
dec_client_key(<<?UINT16(Len), Id:Len/binary,
		 ?UINT16(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_DHE_PSK, _) ->
    #client_dhe_psk_identity{identity = Id, dh_public = DH_Y};
dec_client_key(<<?UINT16(Len), Id:Len/binary, PKEPMS/binary>>,
	       ?KEY_EXCHANGE_RSA_PSK, {3, 0}) ->
    #client_rsa_psk_identity{identity = Id, exchange_keys = #encrypted_premaster_secret{premaster_secret = PKEPMS}};
dec_client_key(<<?UINT16(Len), Id:Len/binary, ?UINT16(_), PKEPMS/binary>>,
	       ?KEY_EXCHANGE_RSA_PSK, _) ->
    #client_rsa_psk_identity{identity = Id, exchange_keys = #encrypted_premaster_secret{premaster_secret = PKEPMS}};
dec_client_key(<<?UINT16(ALen), A:ALen/binary>>,
	       ?KEY_EXCHANGE_SRP, _) ->
    #client_srp_public{srp_a = A}.

dec_ske_params(Len, Keys, Version) ->
    <<Params:Len/bytes, Signature/binary>> = Keys,
    dec_ske_signature(Params, Signature, Version).

dec_ske_signature(Params, <<?BYTE(HashAlgo), ?BYTE(SignAlgo),
			    ?UINT16(0)>>, {Major, Minor})
  when Major == 3, Minor >= 3 ->
    HashSign = {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)},
    {Params, HashSign, <<>>};
dec_ske_signature(Params, <<?BYTE(HashAlgo), ?BYTE(SignAlgo),
			    ?UINT16(Len), Signature:Len/binary>>, {Major, Minor})
  when Major == 3, Minor >= 3 ->
    HashSign = {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)},
    {Params, HashSign, Signature};
dec_ske_signature(Params, <<>>, _) ->
    {Params, {null, anon}, <<>>};
dec_ske_signature(Params, <<?UINT16(0)>>, _) ->
    {Params, {null, anon}, <<>>};
dec_ske_signature(Params, <<?UINT16(Len), Signature:Len/binary>>, _) ->
    {Params, undefined, Signature};
dec_ske_signature(_, _, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

dec_server_key(<<?UINT16(PLen), P:PLen/binary,
		 ?UINT16(GLen), G:GLen/binary,
		 ?UINT16(YLen), Y:YLen/binary, _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_DIFFIE_HELLMAN, Version) ->
    Params = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y},
    {BinMsg, HashSign, Signature} = dec_ske_params(PLen + GLen + YLen + 6, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(<<?UINT16(Len), PskIdentityHint:Len/binary>> = KeyStruct,
	       KeyExchange, Version)
  when KeyExchange == ?KEY_EXCHANGE_PSK; KeyExchange == ?KEY_EXCHANGE_RSA_PSK ->
    Params = #server_psk_params{
      hint = PskIdentityHint},
    {BinMsg, HashSign, Signature} = dec_ske_params(Len + 2, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(<<?UINT16(Len), IdentityHint:Len/binary,
		 ?UINT16(PLen), P:PLen/binary,
		 ?UINT16(GLen), G:GLen/binary,
		 ?UINT16(YLen), Y:YLen/binary, _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_DHE_PSK, Version) ->
    DHParams = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y},
    Params = #server_dhe_psk_params{
      hint = IdentityHint,
      dh_params = DHParams},
    {BinMsg, HashSign, Signature} = dec_ske_params(Len + PLen + GLen + YLen + 8, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(<<?UINT16(NLen), N:NLen/binary,
		 ?UINT16(GLen), G:GLen/binary,
		 ?BYTE(SLen), S:SLen/binary,
		 ?UINT16(BLen), B:BLen/binary, _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_SRP, Version) ->
    Params = #server_srp_params{srp_n = N, srp_g = G, srp_s = S, srp_b = B},
    {BinMsg, HashSign, Signature} = dec_ske_params(NLen + GLen + SLen + BLen + 7, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(_, _, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)).

dec_hello_extensions(<<>>) ->
    [];
dec_hello_extensions(<<?UINT16(ExtLen), Extensions:ExtLen/binary>>) ->
    dec_hello_extensions(Extensions, []);
dec_hello_extensions(_) ->
    [].

dec_hello_extensions(<<>>, Acc) ->
    Acc;
dec_hello_extensions(<<?UINT16(?NEXTPROTONEG_EXT), ?UINT16(Len), ExtensionData:Len/binary, Rest/binary>>, Acc) ->
    Prop = {next_protocol_negotiation, #next_protocol_negotiation{extension_data = ExtensionData}},
    dec_hello_extensions(Rest, [Prop | Acc]);
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

dec_hello_extensions(<<?UINT16(?SRP_EXT), ?UINT16(Len), ?BYTE(SRPLen), SRP:SRPLen/binary, Rest/binary>>, Acc)
  when Len == SRPLen + 2 ->
    dec_hello_extensions(Rest, [{srp,
			   #srp{username = SRP}} | Acc]);

dec_hello_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Acc) ->
    SignAlgoListLen = Len - 2,
    <<?UINT16(SignAlgoListLen), SignAlgoList/binary>> = ExtData,
    HashSignAlgos = [{ssl_cipher:hash_algorithm(Hash), ssl_cipher:sign_algorithm(Sign)} ||
			<<?BYTE(Hash), ?BYTE(Sign)>> <= SignAlgoList],
    dec_hello_extensions(Rest, [{hash_signs,
				 #hash_sign_algos{hash_sign_algos = HashSignAlgos}} | Acc]);

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

enc_hs(#next_protocol{selected_protocol = SelectedProtocol}, _Version) ->
    PaddingLength = 32 - ((byte_size(SelectedProtocol) + 2) rem 32),

    {?NEXT_PROTOCOL, <<?BYTE((byte_size(SelectedProtocol))), SelectedProtocol/binary,
                         ?BYTE(PaddingLength), 0:(PaddingLength * 8)>>};
enc_hs(#hello_request{}, _Version) ->
    {?HELLO_REQUEST, <<>>};
enc_hs(#client_hello{client_version = {Major, Minor},
		     random = Random,
		     session_id = SessionID,
		     cipher_suites = CipherSuites,
		     compression_methods = CompMethods, 
		     renegotiation_info = RenegotiationInfo,
		     srp = SRP,
		     hash_signs = HashSigns,
		     next_protocol_negotiation = NextProtocolNegotiation}, _Version) ->
    SIDLength = byte_size(SessionID),
    BinCompMethods = list_to_binary(CompMethods),
    CmLength = byte_size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = byte_size(BinCipherSuites),
    Extensions0 = hello_extensions(RenegotiationInfo, SRP, NextProtocolNegotiation),
    Extensions1 = if
		      Major == 3, Minor >=3 -> Extensions0 ++ hello_extensions(HashSigns);
		      true -> Extensions0
		  end,
    ExtensionsBin = enc_hello_extensions(Extensions1),

 {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SIDLength), SessionID/binary,
		     ?UINT16(CsLength), BinCipherSuites/binary,
		     ?BYTE(CmLength), BinCompMethods/binary, ExtensionsBin/binary>>};

enc_hs(#server_hello{server_version = {Major, Minor},
		     random = Random,
		     session_id = Session_ID,
		     cipher_suite = Cipher_suite,
		     compression_method = Comp_method,
		     renegotiation_info = RenegotiationInfo,
		     next_protocol_negotiation = NextProtocolNegotiation}, _Version) ->
    SID_length = byte_size(Session_ID),
    Extensions  = hello_extensions(RenegotiationInfo, NextProtocolNegotiation),
    ExtensionsBin = enc_hello_extensions(Extensions),
    {?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SID_length), Session_ID/binary,
                     Cipher_suite/binary, ?BYTE(Comp_method), ExtensionsBin/binary>>};
enc_hs(#certificate{asn1_certificates = ASN1CertList}, _Version) ->
    ASN1Certs = certs_from_list(ASN1CertList),
    ACLen = erlang:iolist_size(ASN1Certs),
    {?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>};
enc_hs(#server_key_exchange{exchange_keys = Keys}, _Version) ->
    {?SERVER_KEY_EXCHANGE, Keys};
enc_hs(#server_key_params{params_bin = Keys, hashsign = HashSign,
			  signature = Signature}, Version) ->
    EncSign = enc_sign(HashSign, Signature, Version),
    {?SERVER_KEY_EXCHANGE, <<Keys/binary, EncSign/binary>>};
enc_hs(#certificate_request{certificate_types = CertTypes,
			    hashsign_algorithms = #hash_sign_algos{hash_sign_algos = HashSignAlgos},
			    certificate_authorities = CertAuths},
       {Major, Minor})
  when Major == 3, Minor >= 3 ->
    HashSigns= << <<(ssl_cipher:hash_algorithm(Hash)):8, (ssl_cipher:sign_algorithm(Sign)):8>> ||
		   {Hash, Sign} <- HashSignAlgos >>,
    CertTypesLen = byte_size(CertTypes),
    HashSignsLen = byte_size(HashSigns),
    CertAuthsLen = byte_size(CertAuths),
    {?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes/binary,
	?UINT16(HashSignsLen), HashSigns/binary,
	?UINT16(CertAuthsLen), CertAuths/binary>>
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
enc_hs(#certificate_verify{signature = BinSig, hashsign_algorithm = HashSign}, Version) ->
    EncSig = enc_sign(HashSign, BinSig, Version),
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
    <<?UINT16(Len), DHPublic/binary>>;
enc_cke(#client_psk_identity{identity = undefined}, _) ->
    Id = <<"psk_identity">>,
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary>>;
enc_cke(#client_psk_identity{identity = Id}, _) ->
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary>>;
enc_cke(Identity = #client_dhe_psk_identity{identity = undefined}, Version) ->
    enc_cke(Identity#client_dhe_psk_identity{identity = <<"psk_identity">>}, Version);
enc_cke(#client_dhe_psk_identity{identity = Id, dh_public = DHPublic}, _) ->
    Len = byte_size(Id),
    DHLen = byte_size(DHPublic),
    <<?UINT16(Len), Id/binary, ?UINT16(DHLen), DHPublic/binary>>;
enc_cke(Identity = #client_rsa_psk_identity{identity = undefined}, Version) ->
    enc_cke(Identity#client_rsa_psk_identity{identity = <<"psk_identity">>}, Version);
enc_cke(#client_rsa_psk_identity{identity = Id, exchange_keys = ExchangeKeys}, Version) ->
    EncPMS = enc_cke(ExchangeKeys, Version),
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary, EncPMS/binary>>;
enc_cke(#client_srp_public{srp_a = A}, _) ->
    Len = byte_size(A),
    <<?UINT16(Len), A/binary>>.

enc_server_key(#server_dh_params{dh_p = P, dh_g = G, dh_y = Y}) ->
    PLen = byte_size(P),
    GLen = byte_size(G),
    YLen = byte_size(Y),
    <<?UINT16(PLen), P/binary, ?UINT16(GLen), G/binary, ?UINT16(YLen), Y/binary>>;
enc_server_key(#server_psk_params{hint = PskIdentityHint}) ->
    Len = byte_size(PskIdentityHint),
    <<?UINT16(Len), PskIdentityHint/binary>>;
enc_server_key(Params = #server_dhe_psk_params{hint = undefined}) ->
    enc_server_key(Params#server_dhe_psk_params{hint = <<>>});
enc_server_key(#server_dhe_psk_params{
		  hint = PskIdentityHint,
		  dh_params = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y}}) ->
    Len = byte_size(PskIdentityHint),
    PLen = byte_size(P),
    GLen = byte_size(G),
    YLen = byte_size(Y),
    <<?UINT16(Len), PskIdentityHint/binary,
      ?UINT16(PLen), P/binary, ?UINT16(GLen), G/binary, ?UINT16(YLen), Y/binary>>;
enc_server_key(#server_srp_params{srp_n = N, srp_g = G,	srp_s = S, srp_b = B}) ->
    NLen = byte_size(N),
    GLen = byte_size(G),
    SLen = byte_size(S),
    BLen = byte_size(B),
    <<?UINT16(NLen), N/binary, ?UINT16(GLen), G/binary,
      ?BYTE(SLen), S/binary, ?UINT16(BLen), B/binary>>.

enc_sign({_, anon}, _Sign, _Version) ->
    <<>>;
enc_sign({HashAlg, SignAlg}, Signature, _Version = {Major, Minor})
  when Major == 3, Minor >= 3->
	SignLen = byte_size(Signature),
	HashSign = hashsign_enc(HashAlg, SignAlg),
	<<HashSign/binary, ?UINT16(SignLen), Signature/binary>>;
enc_sign(_HashSign, Sign, _Version) ->
	SignLen = byte_size(Sign),
	<<?UINT16(SignLen), Sign/binary>>.

hello_extensions(RenegotiationInfo, NextProtocolNegotiation) ->
    hello_extensions(RenegotiationInfo) ++ next_protocol_extension(NextProtocolNegotiation).

hello_extensions(RenegotiationInfo, SRP, NextProtocolNegotiation) ->
    hello_extensions(RenegotiationInfo) ++ hello_extensions(SRP) ++ next_protocol_extension(NextProtocolNegotiation).

%% Renegotiation info
hello_extensions(#renegotiation_info{renegotiated_connection = undefined}) ->
    [];
hello_extensions(#renegotiation_info{} = Info) ->
    [Info];
hello_extensions(#srp{} = Info) ->
    [Info];
hello_extensions(#hash_sign_algos{} = Info) ->
    [Info];
hello_extensions(undefined) ->
    [].

next_protocol_extension(undefined) ->
    [];
next_protocol_extension(#next_protocol_negotiation{} = Info) ->
    [Info].

enc_hello_extensions(Extensions) ->
    enc_hello_extensions(Extensions, <<>>).
enc_hello_extensions([], <<>>) ->
    <<>>;
enc_hello_extensions([], Acc) ->
    Size = byte_size(Acc),
    <<?UINT16(Size), Acc/binary>>;

enc_hello_extensions([#next_protocol_negotiation{extension_data = ExtensionData} | Rest], Acc) ->
    Len = byte_size(ExtensionData),
    enc_hello_extensions(Rest, <<?UINT16(?NEXTPROTONEG_EXT), ?UINT16(Len), ExtensionData/binary, Acc/binary>>);
enc_hello_extensions([#renegotiation_info{renegotiated_connection = ?byte(0) = Info} | Rest], Acc) ->
    Len = byte_size(Info),
    enc_hello_extensions(Rest, <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), Info/binary, Acc/binary>>);

enc_hello_extensions([#renegotiation_info{renegotiated_connection = Info} | Rest], Acc) ->
    InfoLen = byte_size(Info),
    Len = InfoLen +1,
    enc_hello_extensions(Rest, <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), ?BYTE(InfoLen), Info/binary, Acc/binary>>);

enc_hello_extensions([#srp{username = UserName} | Rest], Acc) ->
    SRPLen = byte_size(UserName),
    Len = SRPLen + 2,
    enc_hello_extensions(Rest, <<?UINT16(?SRP_EXT), ?UINT16(Len), ?BYTE(SRPLen), UserName/binary, Acc/binary>>);

enc_hello_extensions([#hash_sign_algos{hash_sign_algos = HashSignAlgos} | Rest], Acc) ->
    SignAlgoList = << <<(ssl_cipher:hash_algorithm(Hash)):8, (ssl_cipher:sign_algorithm(Sign)):8>> ||
		       {Hash, Sign} <- HashSignAlgos >>,
    ListLen = byte_size(SignAlgoList),
    Len = ListLen + 2,
    enc_hello_extensions(Rest, <<?UINT16(?SIGNATURE_ALGORITHMS_EXT), 
				 ?UINT16(Len), ?UINT16(ListLen), SignAlgoList/binary, Acc/binary>>).

encode_client_protocol_negotiation(undefined, _) ->
    undefined;
encode_client_protocol_negotiation(_, false) ->
	#next_protocol_negotiation{extension_data = <<>>};
encode_client_protocol_negotiation(_, _) ->
	undefined.

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

hashsign_dec(<<?BYTE(HashAlgo), ?BYTE(SignAlgo)>>) ->
    {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)}.

hashsign_enc(HashAlgo, SignAlgo) ->
    Hash = ssl_cipher:hash_algorithm(HashAlgo),
    Sign = ssl_cipher:sign_algorithm(SignAlgo),
    <<?BYTE(Hash), ?BYTE(Sign)>>.

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


digitally_signed({3, Minor}, Hash, HashAlgo, Key) when Minor >= 3 ->
    public_key:sign({digest, Hash}, HashAlgo, Key);
digitally_signed(_Version, Hash, HashAlgo, #'DSAPrivateKey'{} = Key) ->
    public_key:sign({digest, Hash}, HashAlgo, Key);
digitally_signed(_Version, Hash, _HashAlgo, #'RSAPrivateKey'{} = Key) ->
    public_key:encrypt_private(Hash, Key,
			       [{rsa_pad, rsa_pkcs1_padding}]).

calc_master_secret({3,0}, _PrfAlgo, PremasterSecret, ClientRandom, ServerRandom) ->
    ssl_ssl3:master_secret(PremasterSecret, ClientRandom, ServerRandom);

calc_master_secret({3,_}, PrfAlgo, PremasterSecret, ClientRandom, ServerRandom) ->
    ssl_tls1:master_secret(PrfAlgo, PremasterSecret, ClientRandom, ServerRandom).

setup_keys({3,0}, _PrfAlgo, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, EKML, IVS) ->
    ssl_ssl3:setup_keys(MasterSecret, ServerRandom,
			ClientRandom, HashSize, KML, EKML, IVS);

setup_keys({3,N}, PrfAlgo, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, _EKML, IVS) ->
    ssl_tls1:setup_keys(N, PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
			KML, IVS).

calc_finished({3, 0}, Role, _PrfAlgo, MasterSecret, Handshake) ->
    ssl_ssl3:finished(Role, MasterSecret, lists:reverse(Handshake));
calc_finished({3, N}, Role, PrfAlgo, MasterSecret, Handshake) ->
    ssl_tls1:finished(Role, N, PrfAlgo, MasterSecret, lists:reverse(Handshake)).

calc_certificate_verify({3, 0}, HashAlgo, MasterSecret, Handshake) ->
    ssl_ssl3:certificate_verify(HashAlgo, MasterSecret, lists:reverse(Handshake));
calc_certificate_verify({3, N}, HashAlgo, _MasterSecret, Handshake) ->
    ssl_tls1:certificate_verify(HashAlgo, N, lists:reverse(Handshake)).

key_exchange_alg(rsa) ->
    ?KEY_EXCHANGE_RSA;
key_exchange_alg(Alg) when Alg == dhe_rsa; Alg == dhe_dss;
			    Alg == dh_dss; Alg == dh_rsa; Alg == dh_anon ->
    ?KEY_EXCHANGE_DIFFIE_HELLMAN;
key_exchange_alg(psk) ->
    ?KEY_EXCHANGE_PSK;
key_exchange_alg(dhe_psk) ->
    ?KEY_EXCHANGE_DHE_PSK;
key_exchange_alg(rsa_psk) ->
    ?KEY_EXCHANGE_RSA_PSK;
key_exchange_alg(Alg)
  when Alg == srp_rsa; Alg == srp_dss; Alg == srp_anon ->
    ?KEY_EXCHANGE_SRP;
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

-define(TLSEXT_SIGALG_RSA(MD), {MD, rsa}).
-define(TLSEXT_SIGALG_DSA(MD), {MD, dsa}).

-define(TLSEXT_SIGALG(MD), ?TLSEXT_SIGALG_RSA(MD)).

default_hash_signs() ->
    #hash_sign_algos{hash_sign_algos =
			 [?TLSEXT_SIGALG(sha512),
			  ?TLSEXT_SIGALG(sha384),
			  ?TLSEXT_SIGALG(sha256),
			  ?TLSEXT_SIGALG(sha224),
			  ?TLSEXT_SIGALG(sha),
			  ?TLSEXT_SIGALG_DSA(sha),
			  ?TLSEXT_SIGALG_RSA(md5)]}.
