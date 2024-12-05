%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2024. All Rights Reserved.
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

%----------------------------------------------------------------------
%% Purpose: Help functions for handling the SSL-handshake protocol (common
%% to SSL/TLS and DTLS
%%----------------------------------------------------------------------

-module(ssl_handshake).
-moduledoc false.

-include("ssl_handshake.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include("tls_handshake_1_3.hrl").
-include_lib("public_key/include/public_key.hrl").

-export_type([ssl_handshake/0,
              ssl_handshake_history/0,
	      public_key_info/0,
              oid/0]).

-type oid()               :: tuple().
-type public_key_params() :: #'Dss-Parms'{} |  {namedCurve, oid()} | #'ECParameters'{} | term().
-type public_key_info()   :: {oid(), #'RSAPublicKey'{} | integer() | #'ECPoint'{}, public_key_params()}.
-type ssl_handshake_history() :: {iodata(), iodata()}.

-type ssl_handshake() :: #server_hello{} | #server_hello_done{} | #certificate{} | #certificate_request{} |
			 #client_key_exchange{} | #finished{} | #certificate_verify{} |
			 #hello_request{} | #next_protocol{} | #end_of_early_data{}.

%% Needed for legacy TLS-1.0 and TLS-1.1 functionality
-compile({nowarn_deprecated_function, [{crypto, private_encrypt, 4},
                                       {crypto, private_decrypt, 4},
                                       {public_key, encrypt_private, 3},
                                       {public_key, decrypt_private, 3},
                                       {public_key, encrypt_public, 3},
                                       {public_key, decrypt_public, 3}
                                      ]}).

%% Create handshake messages
-export([hello_request/0,
         server_hello/4,
         server_hello_done/0,
	 certificate/4,
         client_certificate_verify/6,
         certificate_request/5,
         key_exchange/3,
	 finished/5,
         next_protocol/1,
         digitally_signed/5,
         certificate_authorities/2]).

%% Handle handshake messages
-export([certify/9,
         certificate_verify/6,
         verify_signature/5,
	 master_secret/4,
         server_key_exchange_hash/2,
         verify_connection/6,
	 init_handshake_history/0,
         update_handshake_history/2,
         verify_server_key/5,
         select_version/3,
         select_supported_version/2,
         extension_value/1
	]).

%% Encode
-export([encode_handshake/2,
         encode_hello_extensions/1,
         encode_extensions/1,
         encode_extensions/2,
	 encode_client_protocol_negotiation/2,
         encode_protocols_advertised_on_server/1]).
%% Decode
-export([decode_handshake/3,
         decode_vector/1,
         decode_hello_extensions/4,
         decode_extensions/3,
	 decode_server_key/3,
         decode_client_key/3,
	 decode_suites/2
	]).

%% Cipher suites handling
-export([available_suites/2,
         available_signature_algs/2,
         available_signature_algs/3,
         cipher_suites/3,
         select_session/8,
         premaster_secret/2,
         premaster_secret/3,
         premaster_secret/4]).

%% Extensions handling
-export([client_hello_extensions/10,
	 handle_client_hello_extensions/10, %% Returns server hello extensions
	 handle_server_hello_extensions/9,
         select_curve/2,
         select_curve/3,
         select_hashsign/4,
         select_hashsign/5,
	 select_hashsign_algs/3,
         empty_extensions/2,
         add_server_share/3,
	 add_alpn/2,
         add_selected_version/1,
         decode_alpn/1,
         supported_hashsigns/1,
         max_frag_enum/1
	]).

%% Certificate handling
-export([get_cert_params/1,
         select_own_cert/1,
         path_validation/10,
         validation_fun_and_state/4,
         path_validation_options/2]).

%% Tracing
-export([handle_trace/3]).
%%====================================================================
%% Create handshake messages 
%%====================================================================

%%--------------------------------------------------------------------
-spec hello_request() -> #hello_request{}.
%%
%% Description: Creates a hello request message sent by server to
%% trigger renegotiation.
%%--------------------------------------------------------------------
hello_request() ->
    #hello_request{}.

%%--------------------------------------------------------------------
%%-spec server_hello(binary(), ssl_record:ssl_version(), ssl_record:connection_states(),
%%		   Extension::map()) -> #server_hello{}.
%%
%% Description: Creates a server hello message.
%%--------------------------------------------------------------------
server_hello(SessionId, Version, ConnectionStates, Extensions) ->
    #{security_parameters := SecParams} = 
	ssl_record:pending_connection_state(ConnectionStates, read),
    #server_hello{server_version = Version,
		  cipher_suite = SecParams#security_parameters.cipher_suite,
		  random = SecParams#security_parameters.server_random,
		  session_id = SessionId,
		  extensions = Extensions
		 }.

%%--------------------------------------------------------------------
-spec server_hello_done() ->  #server_hello_done{}.
%%
%% Description: Creates a server hello done message.
%%--------------------------------------------------------------------
server_hello_done() ->
    #server_hello_done{}.

%%--------------------------------------------------------------------
-spec certificate([public_key:der_encoded()], ssl_manager:db_handle(), ssl_manager:certdb_ref(), client | server) -> #certificate{} | #alert{}.
%%
%% Description: Creates a certificate message.
%%--------------------------------------------------------------------
certificate([[]], _, _, client) ->
    %% If no suitable certificate is available, the client
    %% SHOULD send a certificate message containing no
    %% certificates. (chapter 7.4.6. RFC 4346)
    #certificate{asn1_certificates = []};
certificate([OwnCert], CertDbHandle, CertDbRef, _) ->
    {ok, _,  CertChain} =  ssl_certificate:certificate_chain(OwnCert, CertDbHandle, CertDbRef),
    #certificate{asn1_certificates = CertChain};
certificate([_, _ |_] = Chain, _, _, _) ->
    #certificate{asn1_certificates = Chain}.

%%--------------------------------------------------------------------
-spec client_certificate_verify([public_key:der_encoded()], binary(),
				ssl_record:ssl_version(), term(), public_key:private_key(),
				ssl_handshake_history()) ->
    #certificate_verify{} | ignore | #alert{}.
%%
%% Description: Creates a certificate_verify message, called by the client.
%%--------------------------------------------------------------------
client_certificate_verify([[]], _, _, _, _, _) ->
    ignore;
client_certificate_verify(_, _, _, _, undefined, _) ->
    ignore;
client_certificate_verify([OwnCert|_], MasterSecret, Version,
			  {HashAlgo, SignAlgo},
			  PrivateKey, {Handshake, _}) ->
    case public_key:pkix_is_fixed_dh_cert(OwnCert) of
	true ->
            ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, fixed_diffie_hellman_prohibited);
	false ->
	    Hashes =
		calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
	    Signed = digitally_signed(Version, Hashes, HashAlgo, PrivateKey, SignAlgo),
	    #certificate_verify{signature = Signed, hashsign_algorithm = {HashAlgo, SignAlgo}}
    end.

%%--------------------------------------------------------------------
-spec certificate_request(ssl_manager:db_handle(),
			  ssl_manager:certdb_ref(),  #hash_sign_algos{}, 
                          ssl_record:ssl_version(), boolean()) ->
          #certificate_request{}.
%%
%% Description: Creates a certificate_request message, called by the server.
%%--------------------------------------------------------------------
certificate_request(CertDbHandle, CertDbRef, HashSigns, Version, IncludeCertAuths) ->
    Types = certificate_types(Version),
    Authorities = case IncludeCertAuths of
                      true ->
                          certificate_authorities(CertDbHandle, CertDbRef);
                      false ->
                          []
                  end,
    #certificate_request{
       certificate_types = Types,
       hashsign_algorithms = HashSigns,
       certificate_authorities = Authorities
      }.
%%--------------------------------------------------------------------
-spec key_exchange(client | server, ssl_record:ssl_version(),
		   {premaster_secret, binary(), public_key_info()} |
		   {dh, binary()} |
		   {dh, {binary(), binary()}, #'DHParameter'{}, {HashAlgo::atom(), SignAlgo::atom()},
		   binary(), binary(), public_key:private_key()} |
		   {ecdh, _, _, _, _, _} |
		   {ecdh, #'ECPrivateKey'{}} |
		   {psk, _, _, _, _, _} |
		   {psk, binary()} |
		   {dhe_psk, _, _, _, _, _, _, _} |
		   {dhe_psk, binary(), binary()} |
		   {ecdhe_psk, _, _, _, _, _, _} |
		   {ecdhe_psk, binary(), #'ECPrivateKey'{}} |
		   {srp, {binary(), binary()}, #srp_user{}, {HashAlgo::atom(), SignAlgo::atom()},
                    binary(), binary(), public_key:private_key()} |
		   {srp, _} |
                   {psk_premaster_secret, _, _, _}) ->
    #client_key_exchange{} | #server_key_exchange{}.

%%
%% Description: Creates a keyexchange message.
%%--------------------------------------------------------------------
key_exchange(client, _Version, {premaster_secret, Secret, {_, PublicKey, _}}) ->
    EncPremasterSecret =
	encrypted_premaster_secret(Secret, PublicKey),
    #client_key_exchange{exchange_keys = EncPremasterSecret};

key_exchange(client, _Version, {dh, PublicKey}) ->
    #client_key_exchange{
	      exchange_keys = #client_diffie_hellman_public{
		dh_public = PublicKey}
	       };

key_exchange(client, _Version, {ecdh, #'ECPrivateKey'{publicKey =  ECPublicKey}}) ->
    #client_key_exchange{
	      exchange_keys = #client_ec_diffie_hellman_public{
		dh_public = ECPublicKey}
	       };

key_exchange(client, _Version, {psk, Identity}) ->
    #client_key_exchange{
       exchange_keys = #client_psk_identity{
			  identity = Identity}
      };

key_exchange(client, _Version, {dhe_psk, Identity, PublicKey}) ->
    #client_key_exchange{
	      exchange_keys = #client_dhe_psk_identity{
		identity = Identity,
		dh_public = PublicKey}
	       };

key_exchange(client, _Version, {ecdhe_psk, Identity, #'ECPrivateKey'{publicKey =  ECPublicKey}}) ->
    #client_key_exchange{
       exchange_keys = #client_ecdhe_psk_identity{
			  identity = Identity,
			  dh_public = ECPublicKey}
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

key_exchange(server, Version, {dh, {PublicKey, _},
			       #'DHParameter'{prime = P, base = G},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerDHParams = #server_dh_params{dh_p = binary:encode_unsigned(P),
				       dh_g = binary:encode_unsigned(G), dh_y = PublicKey},
    enc_server_key_exchange(Version, ServerDHParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {ecdh,  #'ECPrivateKey'{publicKey =  ECPublicKey,
						      parameters = ECCurve}, HashSign,
			       ClientRandom, ServerRandom, PrivateKey}) ->
    ServerECParams = #server_ecdh_params{curve = ECCurve, public = ECPublicKey},
    enc_server_key_exchange(Version, ServerECParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {psk, PskIdentityHint,
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerPSKParams = #server_psk_params{hint = PskIdentityHint},
    enc_server_key_exchange(Version, ServerPSKParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {dhe_psk, PskIdentityHint, {PublicKey, _},
			       #'DHParameter'{prime = P, base = G},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerEDHPSKParams = #server_dhe_psk_params{
      hint = PskIdentityHint,
      dh_params = #server_dh_params{dh_p = binary:encode_unsigned(P),
				    dh_g = binary:encode_unsigned(G), dh_y = PublicKey}
     },
    enc_server_key_exchange(Version, ServerEDHPSKParams,
			    HashSign, ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {ecdhe_psk, PskIdentityHint,
			       #'ECPrivateKey'{publicKey =  ECPublicKey,
					       parameters = ECCurve},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerECDHEPSKParams = #server_ecdhe_psk_params{
      hint = PskIdentityHint,
      dh_params = #server_ecdh_params{curve = ECCurve, public = ECPublicKey}},
    enc_server_key_exchange(Version, ServerECDHEPSKParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey);

key_exchange(server, Version, {srp, {PublicKey, _},
			       #srp_user{generator = Generator, prime = Prime,
					 salt = Salt},
			       HashSign, ClientRandom, ServerRandom, PrivateKey}) ->
    ServerSRPParams = #server_srp_params{srp_n = Prime, srp_g = Generator,
					 srp_s = Salt, srp_b = PublicKey},
    enc_server_key_exchange(Version, ServerSRPParams, HashSign,
			    ClientRandom, ServerRandom, PrivateKey).

%%--------------------------------------------------------------------
-spec finished(ssl_record:ssl_version(), client | server, integer(), binary(), ssl_handshake_history()) ->
    #finished{}.
%%
%% Description: Creates a handshake finished message
%%-------------------------------------------------------------------
finished(Version, Role, PrfAlgo, MasterSecret, {Handshake, _}) -> % use the current handshake
    #finished{verify_data =
	      calc_finished(Version, Role, PrfAlgo, MasterSecret, Handshake)}.
%%--------------------------------------------------------------------
-spec next_protocol(binary()) -> #next_protocol{}.
%%
%% Description: Creates a next protocol message
%%-------------------------------------------------------------------
next_protocol(SelectedProtocol) ->
  #next_protocol{selected_protocol = SelectedProtocol}.

%%====================================================================
%% Handle handshake messages 
%%====================================================================
%%--------------------------------------------------------------------
-spec certify([public_key:combined_cert()], ssl_manager:db_handle(), ssl_manager:certdb_ref(), ssl_options(), term(),
	      client | server, inet:hostname() | inet:ip_address(),
              ssl_record:ssl_version(), map()) ->  {#cert{}, public_key_info()} | #alert{}.
%%
%% Description: Handles a certificate handshake message
%%--------------------------------------------------------------------
certify(Certs, CertDbHandle, CertDbRef,
        #{partial_chain := PartialChain} = SSlOptions,
        CRLDbHandle, Role, Host, Version, ExtInfo) ->
    ServerName = server_name(SSlOptions, Host, Role),
    [PeerCert | _ChainCerts ] = Certs,
    try
	PathsAndAnchors  =
	    ssl_certificate:trusted_cert_and_paths(Certs, CertDbHandle, CertDbRef, PartialChain),

	case path_validate(PathsAndAnchors, ServerName, Role, CertDbHandle, CertDbRef, CRLDbHandle,
                           Version, SSlOptions, ExtInfo) of
	    {ok, {PublicKeyInfo, _}} ->
                {PeerCert, PublicKeyInfo};
	    {error, Reason} ->
                path_validation_alert(Reason, ServerName, PeerCert)
	end
    catch
        error:OtherReason:ST ->
            ?SSL_LOG(info, internal_error, [{error, OtherReason}, {stacktrace, ST}]),
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {unexpected_error, OtherReason})
    end.
%%--------------------------------------------------------------------
-spec certificate_verify(binary(), public_key_info(), ssl_record:ssl_version(), term(),
			 binary(), ssl_handshake_history()) -> valid | #alert{}.
%%
%% Description: Checks that the certificate_verify message is valid.
%%--------------------------------------------------------------------
certificate_verify(_, _, _, undefined, _, _) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, invalid_certificate_verify_message);

certificate_verify(Signature, PublicKeyInfo, Version,
		   HashSign = {HashAlgo, _}, MasterSecret, {_, Handshake}) ->
    Msg = calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
    case verify_signature(Version, Msg, HashSign, Signature, PublicKeyInfo) of
	true ->
	    valid;
	_ ->
	    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE)
    end.
%%--------------------------------------------------------------------
-spec verify_signature(ssl_record:ssl_version(), binary(), {term(), term()}, binary(),
				   public_key_info()) -> true | false.
%%
%% Description: Checks that a public_key signature is valid.
%%--------------------------------------------------------------------
verify_signature(_, Msg, {HashAlgo, SignAlgo}, Signature,
                 {_, PubKey, _}) when  SignAlgo == rsa_pss_rsae;
                                       SignAlgo == rsa_pss_pss ->
    Options = verify_options(SignAlgo, HashAlgo),
    public_key:verify(Msg, HashAlgo, Signature, PubKey, Options);
verify_signature(Version, Msg, {HashAlgo, SignAlgo}, Signature, {?rsaEncryption, PubKey, _})
  when ?TLS_GTE(Version, ?TLS_1_2) ->
    Options = verify_options(SignAlgo, HashAlgo),
    public_key:verify(Msg, HashAlgo, Signature, PubKey, Options);
verify_signature(Version, {digest, Digest}, _HashAlgo, Signature, {?rsaEncryption, PubKey, _})
  when ?TLS_LTE(Version, ?TLS_1_1) ->
    case public_key:decrypt_public(Signature, PubKey,
				   [{rsa_pad, rsa_pkcs1_padding}]) of
	Digest -> true;
	_   -> false
    end;
verify_signature(?TLS_1_3, Msg, {_, eddsa}, Signature, {?'id-Ed25519', PubKey, PubKeyParams}) ->
    public_key:verify(Msg, none, Signature, {PubKey, PubKeyParams});
verify_signature(?TLS_1_3, Msg, {_, eddsa}, Signature, {?'id-Ed448', PubKey, PubKeyParams}) ->
    public_key:verify(Msg, none, Signature, {PubKey, PubKeyParams});
verify_signature(_, Msg, {HashAlgo, _SignAlg}, Signature,
		 {?'id-ecPublicKey', PublicKey, PublicKeyParams}) ->
    public_key:verify(Msg, HashAlgo, Signature, {PublicKey, PublicKeyParams});
verify_signature(Version, _Msg, {_HashAlgo, anon}, _Signature, _)
  when ?TLS_1_X(Version), ?TLS_LTE(Version, ?TLS_1_2) ->
    true;
verify_signature(Version, Msg, {HashAlgo, dsa}, Signature, {?'id-dsa', PublicKey, PublicKeyParams})
  when ?TLS_1_X(Version), ?TLS_LTE(Version, ?TLS_1_2) ->
    public_key:verify(Msg, HashAlgo, Signature, {PublicKey, PublicKeyParams}).

%%--------------------------------------------------------------------
-spec master_secret(ssl_record:ssl_version(), #session{} | binary(), ssl_record:connection_states(),
		   client | server) -> {binary(), ssl_record:connection_states()} | #alert{}.
%%
%% Description: Sets or calculates the master secret and calculate keys,
%% updating the pending connection states. The Mastersecret and the update
%% connection states are returned or an alert if the calculation fails.
%%-------------------------------------------------------------------
master_secret(Version, #session{master_secret = Mastersecret},
	      ConnectionStates, Role) ->
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates, read),
    try master_secret(Version, Mastersecret, SecParams,
		      ConnectionStates, Role)
    catch
	exit:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, key_calculation_failure)
    end;

master_secret(Version, PremasterSecret, ConnectionStates, Role) ->
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates, read),
    
    #security_parameters{prf_algorithm = PrfAlgo,
			 client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    try master_secret(Version,
		      calc_master_secret(Version,PrfAlgo,PremasterSecret,
					 ClientRandom, ServerRandom),
		      SecParams, ConnectionStates, Role)
    catch
	exit:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, master_secret_calculation_failure)
    end.

%%--------------------------------------------------------------------
-spec server_key_exchange_hash(md5sha | sha | sha224 |sha256 | sha384 | sha512, binary()) -> binary() | {digest, binary()}.
%%
%% Description: Calculate the digest of the server key exchange hash if it is complex
%%--------------------------------------------------------------------
server_key_exchange_hash(md5sha, Value) ->
    MD5 = crypto:hash(md5, Value),
    SHA = crypto:hash(sha, Value),
    {digest, <<MD5/binary, SHA/binary>>};

server_key_exchange_hash(_, Value) ->
    %% Optimization: Let crypto calculate the hash in sign/verify call
    Value.

%%--------------------------------------------------------------------
-spec verify_connection(ssl_record:ssl_version(), #finished{}, client | server, integer(), binary(),
			ssl_handshake_history()) -> verified | #alert{}.
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
-spec init_handshake_history() -> ssl_handshake_history().

%%
%% Description: Initialize the empty handshake history buffer.
%%--------------------------------------------------------------------
init_handshake_history() ->
    {[], []}.

%%--------------------------------------------------------------------
-spec update_handshake_history(ssl_handshake:ssl_handshake_history(), Data ::term()) ->
				      ssl_handshake:ssl_handshake_history().
%%
%% Description: Update the handshake history buffer with Data.
%%--------------------------------------------------------------------
update_handshake_history({Handshake0, _Prev}, Data) ->
    {[Data|Handshake0], Handshake0}.

verify_server_key(#server_key_params{params_bin = EncParams,
				     signature = Signature},
		  HashSign = {HashAlgo, _},
		  ConnectionStates, Version, PubKeyInfo) ->
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,

    Hash = server_key_exchange_hash(HashAlgo,
                                    <<ClientRandom/binary,
                                      ServerRandom/binary,
                                      EncParams/binary>>),
    verify_signature(Version, Hash, HashSign, Signature, PubKeyInfo).

select_version(RecordCB, ClientVersion, Versions) ->
    do_select_version(RecordCB, ClientVersion, Versions).


%% Called by TLS 1.2/1.3 Server when "supported_versions" is present
%% in ClientHello.
%% Input lists are ordered (highest first)
select_supported_version(ClientVersions, ServerVersions) ->
    Fn = fun (ClientVersion) -> lists:member(ClientVersion, ServerVersions) end,
    case lists:search(Fn, ClientVersions) of
        {value, ClientVersion} -> ClientVersion;
        false -> undefined
    end.

%%====================================================================
%% Encode handshake 
%%====================================================================

encode_handshake(#next_protocol{selected_protocol = SelectedProtocol}, _Version) ->
    PaddingLength = 32 - ((byte_size(SelectedProtocol) + 2) rem 32),
    {?NEXT_PROTOCOL, <<?BYTE((byte_size(SelectedProtocol))), SelectedProtocol/binary,
                         ?BYTE(PaddingLength), 0:(PaddingLength * 8)>>};
encode_handshake(#server_hello{server_version = ServerVersion,
			       random = Random,
			       session_id = Session_ID,
			       cipher_suite = CipherSuite,
			       extensions = Extensions}, _Version) ->
			SID_length = byte_size(Session_ID),
    {Major,Minor} = ServerVersion,
    ExtensionsBin = encode_hello_extensions(Extensions),
    {?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
                      ?BYTE(SID_length), Session_ID/binary,
                      CipherSuite/binary, ?BYTE(?NO_COMPRESSION), ExtensionsBin/binary>>};
encode_handshake(#certificate{asn1_certificates = ASN1CertList}, _Version) ->
    ASN1Certs = certs_from_list(ASN1CertList),
    ACLen = erlang:iolist_size(ASN1Certs),
    {?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>};
encode_handshake(#server_key_exchange{exchange_keys = Keys}, _Version) ->
    {?SERVER_KEY_EXCHANGE, Keys};
encode_handshake(#server_key_params{params_bin = Keys, hashsign = HashSign,
			  signature = Signature}, Version) ->
    EncSign = enc_sign(HashSign, Signature, Version),
    {?SERVER_KEY_EXCHANGE, <<Keys/binary, EncSign/binary>>};
encode_handshake(#certificate_request{certificate_types = CertTypes,
                                      hashsign_algorithms = #hash_sign_algos{hash_sign_algos = HashSignAlgos},
                                      certificate_authorities = CertAuths},
                 ?TLS_1_2) ->
    HashSigns = << <<(ssl_cipher:signature_scheme(SignatureScheme)):16 >> ||
		       SignatureScheme <- HashSignAlgos >>,
    EncCertAuths = encode_cert_auths(CertAuths),
    CertTypesLen = byte_size(CertTypes),
    HashSignsLen = byte_size(HashSigns),
    CertAuthsLen = byte_size(EncCertAuths),
    {?CERTIFICATE_REQUEST,
     <<?BYTE(CertTypesLen), CertTypes/binary,
       ?UINT16(HashSignsLen), HashSigns/binary,
       ?UINT16(CertAuthsLen), EncCertAuths/binary>>
    };
encode_handshake(#certificate_request{certificate_types = CertTypes,
                                      certificate_authorities = CertAuths},
       _Version) ->
    EncCertAuths = encode_cert_auths(CertAuths),
    CertTypesLen = byte_size(CertTypes),
    CertAuthsLen = byte_size(EncCertAuths),
    {?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes/binary,
	?UINT16(CertAuthsLen), EncCertAuths/binary>>
    };
encode_handshake(#server_hello_done{}, _Version) ->
    {?SERVER_HELLO_DONE, <<>>};
encode_handshake(#client_key_exchange{exchange_keys = ExchangeKeys}, _Version) ->
    {?CLIENT_KEY_EXCHANGE, encode_client_key(ExchangeKeys)};
encode_handshake(#certificate_verify{signature = BinSig, hashsign_algorithm = HashSign}, Version) ->
    EncSig = enc_sign(HashSign, BinSig, Version),
    {?CERTIFICATE_VERIFY, EncSig};
encode_handshake(#finished{verify_data = VerifyData}, _Version) ->
    {?FINISHED, VerifyData}.

encode_hello_extensions(Extensions) ->
    encode_extensions(hello_extensions_list(Extensions), <<>>).

encode_extensions(Exts) ->
    encode_extensions(Exts, <<>>).
    
encode_extensions([], <<>>) ->
    <<?UINT16(0)>>;
encode_extensions([], Acc) ->
    Size = byte_size(Acc),
    <<?UINT16(Size), Acc/binary>>;
encode_extensions([#alpn{extension_data = ExtensionData} | Rest], Acc) ->
    Len = byte_size(ExtensionData),
    ExtLen = Len + 2,
    encode_extensions(Rest, <<?UINT16(?ALPN_EXT), ?UINT16(ExtLen), ?UINT16(Len),
                              ExtensionData/binary, Acc/binary>>);
encode_extensions([#next_protocol_negotiation{extension_data = ExtensionData} | Rest], Acc) ->
    Len = byte_size(ExtensionData),
    encode_extensions(Rest, <<?UINT16(?NEXTPROTONEG_EXT), ?UINT16(Len),
				    ExtensionData/binary, Acc/binary>>);
encode_extensions([#renegotiation_info{renegotiated_connection = undefined} | Rest], Acc) ->
    encode_extensions(Rest, Acc);
encode_extensions([#renegotiation_info{renegotiated_connection = ?byte(0) = Info} | Rest], Acc) ->
    Len = byte_size(Info),
    encode_extensions(Rest, <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), Info/binary, Acc/binary>>);

encode_extensions([#renegotiation_info{renegotiated_connection = Info} | Rest], Acc) ->
    InfoLen = byte_size(Info),
    Len = InfoLen +1,
    encode_extensions(Rest, <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len), ?BYTE(InfoLen),
				    Info/binary, Acc/binary>>);
encode_extensions([#elliptic_curves{elliptic_curve_list = EllipticCurves} | Rest], Acc) ->

    EllipticCurveList = << <<(tls_v1:oid_to_enum(X)):16>> || X <- EllipticCurves>>,
    ListLen = byte_size(EllipticCurveList),
    Len = ListLen + 2,
    encode_extensions(Rest, <<?UINT16(?ELLIPTIC_CURVES_EXT),
				 ?UINT16(Len), ?UINT16(ListLen), EllipticCurveList/binary, Acc/binary>>);
encode_extensions([#supported_groups{supported_groups = SupportedGroups} | Rest], Acc) ->

    SupportedGroupList = << <<(tls_v1:group_to_enum(X)):16>> || X <- SupportedGroups>>,
    ListLen = byte_size(SupportedGroupList),
    Len = ListLen + 2,
    encode_extensions(Rest, <<?UINT16(?ELLIPTIC_CURVES_EXT),
                              ?UINT16(Len), ?UINT16(ListLen), 
                              SupportedGroupList/binary, Acc/binary>>);
encode_extensions([#ec_point_formats{ec_point_format_list = ECPointFormats} | Rest], Acc) ->
    ECPointFormatList = list_to_binary(ECPointFormats),
    ListLen = byte_size(ECPointFormatList),
    Len = ListLen + 1,
    encode_extensions(Rest, <<?UINT16(?EC_POINT_FORMATS_EXT),
				 ?UINT16(Len), ?BYTE(ListLen), ECPointFormatList/binary, Acc/binary>>);
encode_extensions([#srp{username = UserName} | Rest], Acc) ->
    SRPLen = byte_size(UserName),
    Len = SRPLen + 1,
    encode_extensions(Rest, <<?UINT16(?SRP_EXT), ?UINT16(Len), ?BYTE(SRPLen),
				    UserName/binary, Acc/binary>>);
encode_extensions([#hash_sign_algos{hash_sign_algos = HashSignAlgos} | Rest], Acc) ->
    SignAlgoList = << <<(ssl_cipher:signature_scheme(SignatureScheme)):16 >> ||
		       SignatureScheme <- HashSignAlgos >>,
    ListLen = byte_size(SignAlgoList),
    Len = ListLen + 2,
    encode_extensions(Rest, <<?UINT16(?SIGNATURE_ALGORITHMS_EXT),
				 ?UINT16(Len), ?UINT16(ListLen), SignAlgoList/binary, Acc/binary>>);
encode_extensions([#signature_algorithms{
                            signature_scheme_list = SignatureSchemes} | Rest], Acc) ->
    SignSchemeList = << <<(ssl_cipher:signature_scheme(SignatureScheme)):16 >> ||
		       SignatureScheme <- SignatureSchemes >>,
    ListLen = byte_size(SignSchemeList),
    Len = ListLen + 2,
    encode_extensions(Rest, <<?UINT16(?SIGNATURE_ALGORITHMS_EXT),
				 ?UINT16(Len), ?UINT16(ListLen), SignSchemeList/binary, Acc/binary>>);
encode_extensions([#signature_algorithms_cert{
                            signature_scheme_list = SignatureSchemes} | Rest], Acc) ->
    SignSchemeList = << <<(ssl_cipher:signature_scheme(SignatureScheme)):16 >> ||
		       SignatureScheme <- SignatureSchemes >>,
    ListLen = byte_size(SignSchemeList),
    Len = ListLen + 2,
    encode_extensions(Rest, <<?UINT16(?SIGNATURE_ALGORITHMS_CERT_EXT),
				 ?UINT16(Len), ?UINT16(ListLen), SignSchemeList/binary, Acc/binary>>);
encode_extensions([#sni{hostname = ""} | Rest], Acc) ->
    HostnameBin = <<>>,
    encode_extensions(Rest, <<?UINT16(?SNI_EXT), ?UINT16(0),
                              HostnameBin/binary,
                              Acc/binary>>);
encode_extensions([#sni{hostname = Hostname} | Rest], Acc) ->
    HostLen = length(Hostname),
    HostnameBin = list_to_binary(Hostname),
    % Hostname type (1 byte) + Hostname length (2 bytes) + Hostname (HostLen bytes)
    ServerNameLength = 1 + 2 + HostLen,
    % ServerNameListSize (2 bytes) + ServerNameLength
    ExtLength = 2 + ServerNameLength,
    encode_extensions(Rest, <<?UINT16(?SNI_EXT), ?UINT16(ExtLength),
                              ?UINT16(ServerNameLength),
                              ?BYTE(?SNI_NAMETYPE_HOST_NAME),
                              ?UINT16(HostLen), HostnameBin/binary,
                              Acc/binary>>);
encode_extensions([#use_srtp{protection_profiles = Profiles, mki = MKI} | Rest], Acc) ->
    ProfilesBin = iolist_to_binary(Profiles),
    ProfilesLength = byte_size(ProfilesBin),
    MKILength = byte_size(MKI),
    ExtLength = ProfilesLength + 2 + MKILength + 1,
    encode_extensions(Rest, <<?UINT16(?USE_SRTP_EXT), ?UINT16(ExtLength),
                              ?UINT16(ProfilesLength), ProfilesBin/binary,
                              ?BYTE(MKILength), MKI/binary,
                              Acc/binary>>);
encode_extensions([#max_frag_enum{enum = MaxFragEnum} | Rest], Acc) ->
    ExtLength = 1,
    encode_extensions(Rest, <<?UINT16(?MAX_FRAGMENT_LENGTH_EXT), ?UINT16(ExtLength), ?BYTE(MaxFragEnum),
                              Acc/binary>>);
encode_extensions([#client_hello_versions{versions = Versions0} | Rest], Acc) ->
    Versions = encode_versions(Versions0),
    VerLen = byte_size(Versions),
    Len = VerLen + 1,
    encode_extensions(Rest, <<?UINT16(?SUPPORTED_VERSIONS_EXT),
                              ?UINT16(Len), ?BYTE(VerLen), Versions/binary, Acc/binary>>);
encode_extensions([#server_hello_selected_version{selected_version = Version0} | Rest], Acc) ->
    Version = encode_versions([Version0]),
    Len = byte_size(Version), %% 2
    encode_extensions(Rest, <<?UINT16(?SUPPORTED_VERSIONS_EXT),
                              ?UINT16(Len), Version/binary, Acc/binary>>);
encode_extensions([#key_share_client_hello{client_shares = ClientShares0} | Rest], Acc) ->
    ClientShares = encode_client_shares(ClientShares0),
    ClientSharesLen = byte_size(ClientShares),
    Len = ClientSharesLen + 2,
    encode_extensions(Rest, <<?UINT16(?KEY_SHARE_EXT),
                              ?UINT16(Len), ?UINT16(ClientSharesLen),
                              ClientShares/binary, Acc/binary>>);
encode_extensions([#key_share_server_hello{server_share = ServerShare0} | Rest], Acc) ->
    ServerShare = encode_key_share_entry(ServerShare0),
    Len = byte_size(ServerShare),
    encode_extensions(Rest, <<?UINT16(?KEY_SHARE_EXT),
                              ?UINT16(Len), ServerShare/binary, Acc/binary>>);
encode_extensions([#key_share_hello_retry_request{selected_group = Group0} | Rest], Acc) ->
    Group = tls_v1:group_to_enum(Group0),
    encode_extensions(Rest, <<?UINT16(?KEY_SHARE_EXT),
                              ?UINT16(2), ?UINT16(Group), Acc/binary>>);
encode_extensions([#psk_key_exchange_modes{ke_modes = KEModes0} | Rest], Acc) ->
    KEModes = encode_psk_key_exchange_modes(KEModes0),
    KEModesLen = byte_size(KEModes),
    ExtLen = KEModesLen + 1,
    encode_extensions(Rest, <<?UINT16(?PSK_KEY_EXCHANGE_MODES_EXT),
                              ?UINT16(ExtLen), ?BYTE(KEModesLen), KEModes/binary, Acc/binary>>);
encode_extensions([
    #certificate_status_request{
        status_type = StatusRequest,
        request = Request} | Rest], Acc) ->
    CertStatusReq = encode_cert_status_req(StatusRequest, Request),
    Len = byte_size(CertStatusReq),
    encode_extensions(
        Rest, <<?UINT16(?STATUS_REQUEST), ?UINT16(Len),
        CertStatusReq/binary, Acc/binary>>);
encode_extensions([#pre_shared_key_client_hello{
                      offered_psks = #offered_psks{
                                        identities = Identities0,
                                        binders = Binders0}} | Rest], Acc) ->
    Identities = encode_psk_identities(Identities0),
    Binders = encode_psk_binders(Binders0),
    Len = byte_size(Identities) + byte_size(Binders),
    %% The "pre_shared_key" extension MUST be the last extension in the
    %% ClientHello (this facilitates implementation as described below).
    %% Servers MUST check that it is the last extension and otherwise fail
    %% the handshake with an "illegal_parameter" alert.
    encode_extensions(Rest, <<Acc/binary,?UINT16(?PRE_SHARED_KEY_EXT),
                              ?UINT16(Len), Identities/binary, Binders/binary>>);
encode_extensions([#pre_shared_key_server_hello{selected_identity = Identity} | Rest], Acc) ->
    encode_extensions(Rest, <<?UINT16(?PRE_SHARED_KEY_EXT),
                              ?UINT16(2), ?UINT16(Identity), Acc/binary>>);
encode_extensions([#cookie{cookie = Cookie} | Rest], Acc) ->
    CookieLen = byte_size(Cookie),
    Len = CookieLen + 2,
    encode_extensions(Rest, <<?UINT16(?COOKIE_EXT), ?UINT16(Len), ?UINT16(CookieLen),
                              Cookie/binary, Acc/binary>>);
encode_extensions([#early_data_indication{} | Rest], Acc) ->
    encode_extensions(Rest, <<?UINT16(?EARLY_DATA_EXT),
                              ?UINT16(0), Acc/binary>>);
encode_extensions([#early_data_indication_nst{indication = MaxSize} | Rest], Acc) ->
    encode_extensions(Rest, <<?UINT16(?EARLY_DATA_EXT),
                              ?UINT16(4), ?UINT32(MaxSize), Acc/binary>>);
encode_extensions([#certificate_authorities{authorities = CertAuths}| Rest], Acc) ->
    EncCertAuths = encode_cert_auths(CertAuths),
    CertAuthsLen = byte_size(EncCertAuths),
    Len = CertAuthsLen + 2,
    encode_extensions(Rest, <<?UINT16(?CERTIFICATE_AUTHORITIES_EXT), ?UINT16(Len),
                              ?UINT16(CertAuthsLen), EncCertAuths/binary, Acc/binary>>).

encode_cert_status_req(
    StatusType,
    #ocsp_status_request{
        responder_id_list = ResponderIDList,
        request_extensions = ReqExtns}) ->
    ResponderIDListBin = encode_responderID_list(ResponderIDList),
    ReqExtnsBin = encode_request_extensions(ReqExtns),
    <<?BYTE(StatusType), ResponderIDListBin/binary, ReqExtnsBin/binary>>.

encode_responderID_list([]) ->
    <<?UINT16(0)>>;
encode_responderID_list(List) ->
    do_encode_responderID_list(List, <<>>).

%% ResponderID is DER-encoded ASN.1 type defined in RFC6960.
do_encode_responderID_list([], Acc) ->
    Len = byte_size(Acc),
    <<?UINT16(Len), Acc/binary>>;
do_encode_responderID_list([Responder | Rest], Acc)
  when is_binary(Responder) ->
    Len = byte_size(Responder),
    do_encode_responderID_list(
        Rest, <<Acc/binary, ?UINT16(Len), Responder/binary>>).

%% Extensions are DER-encoded ASN.1 type defined in RFC6960 following
%% extension model employed in X.509 version 3 certificates(RFC5280).
encode_request_extensions([]) ->
    <<?UINT16(0)>>;
encode_request_extensions(Extns) when is_list(Extns) ->
    ExtnBin = public_key:der_encode('Extensions', Extns),
    Len = byte_size(ExtnBin),
    <<?UINT16(Len), ExtnBin/binary>>.

encode_client_protocol_negotiation(undefined, _) ->
    undefined;
encode_client_protocol_negotiation(_, false) ->
	#next_protocol_negotiation{extension_data = <<>>};
encode_client_protocol_negotiation(_, _) ->
	undefined.

encode_protocols_advertised_on_server(undefined) ->
	undefined;

encode_protocols_advertised_on_server(Protocols) ->
	#next_protocol_negotiation{
        extension_data = lists:foldl(fun encode_protocol/2, <<>>, Protocols)}.

encode_cert_auths(Auths) ->
    DNEncode = fun (Auth) ->
                       DNEncodedBin = public_key:pkix_encode('Name', Auth, otp),
                       DNEncodedLen = byte_size(DNEncodedBin),
                       <<?UINT16(DNEncodedLen), DNEncodedBin/binary>>
               end,
    list_to_binary(lists:map(DNEncode, Auths)).

%%====================================================================
%% Decode handshake 
%%====================================================================

decode_handshake(_, ?HELLO_REQUEST, <<>>) ->
    #hello_request{};
decode_handshake(_, ?NEXT_PROTOCOL, <<?BYTE(SelectedProtocolLength),
				      SelectedProtocol:SelectedProtocolLength/binary,
				      ?BYTE(PaddingLength), _Padding:PaddingLength/binary>>) ->
    #next_protocol{selected_protocol = SelectedProtocol};

decode_handshake(Version, ?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
                                           ?BYTE(SID_length), Session_ID:SID_length/binary,
                                           Cipher_suite:2/binary, ?BYTE(?NO_COMPRESSION)>>) ->
    #server_hello{
       server_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suite = Cipher_suite,
       extensions = empty_extensions(Version, server_hello)};

decode_handshake(Version, ?SERVER_HELLO,
                 <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
                   ?BYTE(SID_length), Session_ID:SID_length/binary,
                   Cipher_suite:2/binary, ?BYTE(?NO_COMPRESSION),
                   ?UINT16(ExtLen), Extensions:ExtLen/binary>>) ->
    HelloExtensions = decode_hello_extensions(Extensions, Version, {Major, Minor}, server_hello),
    #server_hello{
       server_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suite = Cipher_suite,
       extensions = HelloExtensions};
decode_handshake(_Version, ?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>) ->
    #certificate{asn1_certificates = certs_to_list(ASN1Certs)};
%% RFC 6066, servers return a certificate response along with their certificate
%% by sending a "CertificateStatus" message immediately after the "Certificate"
%% message and before any "ServerKeyExchange" or "CertificateRequest" messages.
decode_handshake(_Version, ?CERTIFICATE_STATUS, <<?BYTE(?CERTIFICATE_STATUS_TYPE_OCSP),
                 ?UINT24(Len), ASN1OcspResponse:Len/binary>>) ->
    #certificate_status{
        status_type = ?CERTIFICATE_STATUS_TYPE_OCSP,
        response = ASN1OcspResponse};
decode_handshake(_Version, ?SERVER_KEY_EXCHANGE, Keys) ->
    #server_key_exchange{exchange_keys = Keys};
decode_handshake(?TLS_1_2 = Version, ?CERTIFICATE_REQUEST,
                 <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
                   ?UINT16(HashSignsLen), HashSigns:HashSignsLen/binary,
                   ?UINT16(CertAuthsLen), EncCertAuths:CertAuthsLen/binary>>) ->
    HashSignAlgos = decode_sign_alg(Version, HashSigns),
    #certificate_request{certificate_types = CertTypes,
                         hashsign_algorithms = #hash_sign_algos{hash_sign_algos = HashSignAlgos},
			 certificate_authorities = decode_cert_auths(EncCertAuths, [])};
decode_handshake(_Version, ?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
         ?UINT16(CertAuthsLen), EncCertAuths:CertAuthsLen/binary>>) ->
    #certificate_request{certificate_types = CertTypes,
			 certificate_authorities = decode_cert_auths(EncCertAuths, [])};
decode_handshake(_Version, ?SERVER_HELLO_DONE, <<>>) ->
    #server_hello_done{};
decode_handshake(?TLS_1_2, ?CERTIFICATE_VERIFY,<<HashSign:2/binary, ?UINT16(SignLen),
                                                           Signature:SignLen/binary>>) ->
    #certificate_verify{hashsign_algorithm = dec_hashsign(HashSign), signature = Signature};
decode_handshake(_Version, ?CERTIFICATE_VERIFY,<<?UINT16(SignLen), Signature:SignLen/binary>>)->
    #certificate_verify{signature = Signature};
decode_handshake(_Version, ?CLIENT_KEY_EXCHANGE, PKEPMS) ->
    #client_key_exchange{exchange_keys = PKEPMS};
decode_handshake(_Version, ?FINISHED, VerifyData) ->
    #finished{verify_data = VerifyData};
decode_handshake(_, MessageType, _) ->
    throw(?ALERT_REC(?FATAL, ?DECODE_ERROR, {unknown_or_malformed_handshake, MessageType})).


%%--------------------------------------------------------------------
-spec decode_vector(binary()) -> binary().
%%
%% Description: Remove length tag from TLS Vector type. Needed
%% for client hello when extensions in older versions may be empty.
%% 
%%--------------------------------------------------------------------
decode_vector(<<>>) ->
    <<>>;
decode_vector(<<?UINT16(Len), Vector:Len/binary>>) ->
    Vector.

%%--------------------------------------------------------------------
-spec decode_hello_extensions(binary(), ssl_record:ssl_version(),
                              ssl_record:ssl_version(), atom()) -> map().
%%
%% Description: Decodes TLS hello extensions
%%--------------------------------------------------------------------
decode_hello_extensions(Extensions, LocalVersion, LegacyVersion, MessageType0) ->
    %% Convert legacy atoms
    MessageType =
        case MessageType0 of
            client -> client_hello;
            server -> server_hello;
            T -> T
        end,
    %% RFC 8446 - 4.2.1
    %% Servers MUST be prepared to receive ClientHellos that include this extension but
    %% do not include 0x0304 in the list of versions.
    %% Clients MUST check for this extension prior to processing the rest of the
    %% ServerHello (although they will have to parse the ServerHello in order to read
    %% the extension).
    Version = process_supported_versions_extension(Extensions, LocalVersion, LegacyVersion),
    decode_extensions(Extensions, Version, MessageType, empty_extensions(Version, MessageType)).

%%--------------------------------------------------------------------
-spec decode_extensions(binary(),tuple(), atom()) -> map().
%%
%% Description: Decodes TLS hello extensions
%%--------------------------------------------------------------------
decode_extensions(Extensions, Version, MessageType) ->
    decode_extensions(Extensions, Version, MessageType, empty_extensions()).

%%--------------------------------------------------------------------
-spec decode_server_key(binary(), ssl:kex_algo(), ssl_record:ssl_version()) ->
			       #server_key_params{}.
%%
%% Description: Decode server_key data and return appropriate type
%%--------------------------------------------------------------------
decode_server_key(ServerKey, Type, Version) ->
    dec_server_key(ServerKey, key_exchange_alg(Type), Version).

%%--------------------------------------------------------------------
-spec decode_client_key(binary(), ssl:kex_algo(), ssl_record:ssl_version()) ->
			    #encrypted_premaster_secret{}
			    | #client_diffie_hellman_public{}
			    | #client_ec_diffie_hellman_public{}
			    | #client_psk_identity{}
			    | #client_dhe_psk_identity{}
			    | #client_ecdhe_psk_identity{}
			    | #client_rsa_psk_identity{}
			    | #client_srp_public{}.
%%
%% Description: Decode client_key data and return appropriate type
%%--------------------------------------------------------------------
decode_client_key(ClientKey, Type, Version) ->
    dec_client_key(ClientKey, key_exchange_alg(Type), Version).

%%--------------------------------------------------------------------
-spec decode_suites('2_bytes'|'3_bytes', binary()) -> list().
%%
%% Description:
%%--------------------------------------------------------------------
decode_suites('2_bytes', Dec) ->
    from_2bytes(Dec);
decode_suites('3_bytes', Dec) ->
    from_3bytes(Dec).


%%====================================================================
%% Cipher suite handling
%%====================================================================

available_suites(UserSuites, Version) ->
    VersionSuites = ssl_cipher:all_suites(Version) ++ ssl_cipher:anonymous_suites(Version),
    lists:filtermap(fun(Suite) -> lists:member(Suite, VersionSuites) end, UserSuites).

available_suites(ServerCert, UserSuites, Version, undefined, Curve) ->
    Suites = ssl_cipher:filter(ServerCert, available_suites(UserSuites, Version), Version),
    filter_unavailable_ecc_suites(Curve, Suites);
available_suites(ServerCert, UserSuites, Version, HashSigns, Curve) ->
    Suites = available_suites(ServerCert, UserSuites, Version, undefined, Curve),
    filter_hashsigns(Suites, [ssl_cipher_format:suite_bin_to_map(Suite) || Suite <- Suites], HashSigns, Version).

available_signature_algs(undefined, _)  ->
    undefined;
available_signature_algs(SupportedHashSigns, Version) when ?TLS_GTE(Version, ?TLS_1_2) ->
    case contains_scheme(SupportedHashSigns) of
        true ->
            case Version of 
                ?TLS_1_2 ->
                    #hash_sign_algos{hash_sign_algos = ssl_cipher:signature_schemes_1_2(SupportedHashSigns)};
                _ ->
                    #signature_algorithms{signature_scheme_list = SupportedHashSigns}
            end;
        false ->
            #hash_sign_algos{hash_sign_algos = SupportedHashSigns}
    end;
available_signature_algs(_, _) ->
    undefined.

available_signature_algs(undefined, SupportedHashSigns, Version) when
      ?TLS_GTE(Version, ?TLS_1_2) ->
    SupportedHashSigns;
available_signature_algs(#hash_sign_algos{hash_sign_algos = ClientHashSigns}, SupportedHashSigns0, 
                         Version) when ?TLS_GTE(Version, ?TLS_1_2) ->
    SupportedHashSigns =
        case (Version == ?TLS_1_2) andalso contains_scheme(SupportedHashSigns0) of
            true ->
                ssl_cipher:signature_schemes_1_2(SupportedHashSigns0);
            false ->
                SupportedHashSigns0
        end,
    sets:to_list(sets:intersection(sets:from_list(ClientHashSigns), 
				   sets:from_list(SupportedHashSigns)));
available_signature_algs(_, _, _) ->
    undefined.

contains_scheme(Schemes) ->
    lists:any(fun erlang:is_atom/1, Schemes).

cipher_suites(Suites, Renegotiation, true) ->
    %% TLS_FALLBACK_SCSV should be placed last -RFC7507
    cipher_suites(Suites, Renegotiation) ++ [?TLS_FALLBACK_SCSV];
cipher_suites(Suites, Renegotiation, false) ->
    cipher_suites(Suites, Renegotiation).

cipher_suites(Suites, false) ->
    [?TLS_EMPTY_RENEGOTIATION_INFO_SCSV | Suites];
cipher_suites(Suites, true) ->
    Suites.

select_session(SuggestedSessionId, CipherSuites, HashSigns, SessIdTracker, Session0,
               Version, SslOpts, CertKeyAlts) ->
    CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts, Version),
    {SessionId, Resumed} = ssl_session:server_select_session(Version, SessIdTracker, SuggestedSessionId,
                                                             SslOpts, CertKeyPairs),
    case Resumed of
        undefined ->
            %% Select Cert
            Session = new_session_parameters(SessionId, Session0, CipherSuites,
                                             SslOpts, Version, HashSigns, CertKeyPairs),
	    {new, Session};
	_ ->
	    {resumed, Resumed}
    end.

new_session_parameters(SessionId, #session{ecc = ECCCurve0} = Session, CipherSuites, SslOpts,
                       Version, HashSigns, CertKeyPairs) ->
    {Certs, Key, {ECCCurve, CipherSuite}} =
        server_select_cert_key_pair_and_params(CipherSuites, CertKeyPairs, HashSigns,
                                               ECCCurve0, SslOpts, Version),
    Session#session{session_id = SessionId,
                    ecc = ECCCurve,
                    own_certificates = Certs,
                    private_key = Key,
                    cipher_suite = CipherSuite}.

%% Possibly support part of "trusted_ca_keys" extension that corresponds to TLS-1.3 certificate_authorities?!

server_select_cert_key_pair_and_params(CipherSuites, [#{private_key := NoKey, certs := [[]] = NoCerts}], HashSigns, ECCCurve0,
              #{ciphers := UserSuites, honor_cipher_order := HonorCipherOrder}, Version) ->
    %% This can happen if anonymous cipher suites are enabled
    Suites = available_suites(undefined, UserSuites, Version, HashSigns, ECCCurve0),
    CipherSuite0 = select_cipher_suite(CipherSuites, Suites, HonorCipherOrder),
    CurveAndSuite = cert_curve(undefined, ECCCurve0, CipherSuite0),
    {NoCerts, NoKey, CurveAndSuite};
server_select_cert_key_pair_and_params(CipherSuites, [#{private_key := Key, certs := [Cert | _] = Certs}], HashSigns, ECCCurve0,
                                #{ciphers := UserSuites, honor_cipher_order := HonorCipherOrder}, Version) ->
    Suites = available_suites(Cert, UserSuites, Version, HashSigns, ECCCurve0),
    CipherSuite0 = select_cipher_suite(CipherSuites, Suites, HonorCipherOrder),
    CurveAndSuite = cert_curve(Cert, ECCCurve0, CipherSuite0),
    {Certs, Key, CurveAndSuite};
server_select_cert_key_pair_and_params(CipherSuites, [#{private_key := Key, certs := [Cert | _] = Certs} | Rest], HashSigns, ECCCurve0,
                 #{ciphers := UserSuites, honor_cipher_order := HonorCipherOrder} = Opts, Version) ->
    Suites = available_suites(Cert, UserSuites, Version, HashSigns, ECCCurve0),
    case select_cipher_suite(CipherSuites, Suites, HonorCipherOrder) of
        no_suite ->
            server_select_cert_key_pair_and_params(CipherSuites, Rest, HashSigns, ECCCurve0, Opts, Version);
        CipherSuite0 ->
            case is_acceptable_cert(Cert, HashSigns, ssl:tls_version(Version)) of
                true ->
                    CurveAndSuite = cert_curve(Cert, ECCCurve0, CipherSuite0),
                    {Certs, Key, CurveAndSuite};
                false ->
                    server_select_cert_key_pair_and_params(CipherSuites, Rest, HashSigns, ECCCurve0, Opts, Version)
            end
    end.

is_acceptable_cert(Cert, HashSigns, Version)
  when ?TLS_1_X(Version),
       ?TLS_GTE(Version, ?TLS_1_2) ->
    {SignAlgo0, Param, _, _, _} = get_cert_params(Cert),
    SignAlgo = sign_algo(SignAlgo0, Param),
    is_acceptable_hash_sign(SignAlgo, HashSigns);
is_acceptable_cert(_,_,_) ->
    %% Not negotiable pre TLS-1.2. So if cert is available for version it is acceptable
    true.

premaster_secret(OtherPublicDhKey, MyPrivateKey, #'DHParameter'{} = Params) ->
    try
	public_key:compute_key(OtherPublicDhKey, MyPrivateKey, Params)
    catch
	error:Reason:ST ->
            ?SSL_LOG(debug, crypto_error, [{reason, Reason}, {stacktrace, ST}]),
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;
premaster_secret(PublicDhKey, PrivateDhKey, #server_dh_params{dh_p = Prime, dh_g = Base}) ->
    try
	crypto:compute_key(dh, PublicDhKey, PrivateDhKey, [Prime, Base])
    catch
	error:Reason:ST ->
            ?SSL_LOG(debug, crypto_error, [{reason, Reason}, {stacktrace, ST}]),
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;
premaster_secret(#client_srp_public{srp_a = ClientPublicKey}, ServerKey, #srp_user{prime = Prime,
										   verifier = Verifier}) ->
    try crypto:compute_key(srp, ClientPublicKey, ServerKey, {host, [Verifier, Prime, '6a']})
    catch
	error:Reason:ST ->
            ?SSL_LOG(debug, crypto_error, [{reason, Reason}, {stacktrace, ST}]),
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;
premaster_secret(#server_srp_params{srp_n = Prime, srp_g = Generator, srp_s = Salt, srp_b = Public},
		 ClientKeys, {Username, Password}) ->
    case ssl_srp_primes:check_srp_params(Generator, Prime) of
	ok ->
	    DerivedKey = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
	    try crypto:compute_key(srp, Public, ClientKeys, {user, [DerivedKey, Prime, Generator, '6a']})
            catch
		error:Reason:ST ->
                    ?SSL_LOG(debug, crypto_error, [{reason, Reason}, {stacktrace, ST}]),
		    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
	    end;
	not_accepted ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;
premaster_secret(#client_rsa_psk_identity{
		    identity = PSKIdentity,
		    exchange_keys = #encrypted_premaster_secret{premaster_secret = EncPMS}
		   }, #'RSAPrivateKey'{} = Key, PSKLookup) ->
    PremasterSecret = premaster_secret(EncPMS, Key),
    psk_secret(PSKIdentity, PSKLookup, PremasterSecret);
premaster_secret(#server_dhe_psk_params{
		    hint = IdentityHint,
		    dh_params =  #server_dh_params{dh_y = PublicDhKey} = Params},
		    PrivateDhKey,
		    LookupFun) ->
    PremasterSecret = premaster_secret(PublicDhKey, PrivateDhKey, Params),
    psk_secret(IdentityHint, LookupFun, PremasterSecret);
premaster_secret(#server_ecdhe_psk_params{
		    hint = IdentityHint,
                    dh_params = #server_ecdh_params{
                                   public = ECServerPubKey}},
		    PrivateEcDhKey,
		    LookupFun) ->
    PremasterSecret = premaster_secret(#'ECPoint'{point = ECServerPubKey}, PrivateEcDhKey),
    psk_secret(IdentityHint, LookupFun, PremasterSecret);
premaster_secret({rsa_psk, PSKIdentity}, PSKLookup, RSAPremasterSecret) ->
    psk_secret(PSKIdentity, PSKLookup, RSAPremasterSecret);
premaster_secret(#client_ecdhe_psk_identity{
		    identity =  PSKIdentity,
		    dh_public = PublicEcDhPoint}, PrivateEcDhKey, PSKLookup) ->
    PremasterSecret = premaster_secret(#'ECPoint'{point = PublicEcDhPoint}, PrivateEcDhKey),
    psk_secret(PSKIdentity, PSKLookup, PremasterSecret).
premaster_secret(#client_dhe_psk_identity{
		    identity =  PSKIdentity,
		    dh_public = PublicDhKey}, PrivateKey, #'DHParameter'{} = Params, PSKLookup) ->
    PremasterSecret = premaster_secret(PublicDhKey, PrivateKey, Params),
    psk_secret(PSKIdentity, PSKLookup, PremasterSecret).
premaster_secret(#client_psk_identity{identity = PSKIdentity}, PSKLookup) ->
    psk_secret(PSKIdentity, PSKLookup);
premaster_secret({psk, PSKIdentity}, PSKLookup) ->
    psk_secret(PSKIdentity, PSKLookup);
premaster_secret(#'ECPoint'{} = ECPoint, #'ECPrivateKey'{} = ECDHKeys) ->
    public_key:compute_key(ECPoint, ECDHKeys);
premaster_secret(EncSecret, #'RSAPrivateKey'{} = RSAPrivateKey) ->
    try public_key:decrypt_private(EncSecret, RSAPrivateKey,
				   [{rsa_pad, rsa_pkcs1_padding}])
    catch
	_:Reason:ST ->
            ?SSL_LOG(debug, decrypt_error, [{reason, Reason}, {stacktrace, ST}]),
	    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
    end;
premaster_secret(EncSecret, #{algorithm := rsa} = Engine) ->
    try crypto:private_decrypt(rsa, EncSecret, maps:remove(algorithm, Engine),
				   [{rsa_pad, rsa_pkcs1_padding}])
    catch
	_:Reason:ST ->
            ?SSL_LOG(debug, decrypt_error, [{reason, Reason}, {stacktrace, ST}]),
	    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
    end.
%%====================================================================
%% Extensions handling
%%====================================================================
client_hello_extensions(Version, CipherSuites, SslOpts, ConnectionStates, Renegotiation, KeyShare,
                        TicketData, OcspNonce, CertDbHandle, CertDbRef) ->
    HelloExtensions0 = add_tls12_extensions(Version, SslOpts, ConnectionStates, Renegotiation),
    HelloExtensions1 = add_common_extensions(Version, HelloExtensions0, CipherSuites, SslOpts),
    HelloExtensions2 = maybe_add_certificate_status_request(Version, SslOpts, OcspNonce, HelloExtensions1),
    maybe_add_tls13_extensions(Version, HelloExtensions2, SslOpts, KeyShare, TicketData, CertDbHandle, CertDbRef).


add_tls12_extensions(_Version,
                     #{alpn_advertised_protocols := AlpnAdvertisedProtocols,
                       max_fragment_length := MaxFragmentLength} = SslOpts,
                     ConnectionStates,
                     Renegotiation) ->
    SRP = srp_user(SslOpts),
    NextProtocolSelector = maps:get(next_protocol_selector, SslOpts, undefined),
    #{renegotiation_info => renegotiation_info(tls_record, client,
                                               ConnectionStates, Renegotiation),
      srp => SRP,
      alpn => encode_alpn(AlpnAdvertisedProtocols, Renegotiation),
      next_protocol_negotiation =>
          encode_client_protocol_negotiation(NextProtocolSelector,
                                             Renegotiation),
      sni => sni(SslOpts),
      use_srtp => use_srtp_ext(SslOpts),
      max_frag_enum => max_frag_enum(MaxFragmentLength)
     }.


add_common_extensions(?TLS_1_3,
                      HelloExtensions,
                      _CipherSuites,
                      #{eccs := SupportedECCs,
                        supported_groups := Groups,
                        signature_algs := SignatureSchemes,
                        signature_algs_cert := SignatureCertSchemes}) ->
    {EcPointFormats, _} =
        client_ecc_extensions(SupportedECCs),
    HelloExtensions#{ec_point_formats => EcPointFormats,
                     elliptic_curves => Groups,
                     signature_algs => signature_algs_ext(SignatureSchemes),
                     signature_algs_cert =>
                         signature_algs_cert(SignatureCertSchemes)};

add_common_extensions(Version,
                      HelloExtensions,
                      CipherSuites,
                      #{eccs := SupportedECCs,
                        signature_algs := SupportedHashSigns,
                        signature_algs_cert := SignatureCertSchemes}) ->

    {EcPointFormats, EllipticCurves} =
        case advertises_ec_ciphers(
               lists:map(fun ssl_cipher_format:suite_bin_to_map/1,
                         CipherSuites)) of
            true ->
                client_ecc_extensions(SupportedECCs);
            false ->
                {undefined, undefined}
        end,
    HelloExtensions#{ec_point_formats => EcPointFormats,
                     elliptic_curves => EllipticCurves,
                     signature_algs => available_signature_algs(SupportedHashSigns, Version),
                     signature_algs_cert =>
                         signature_algs_cert(SignatureCertSchemes)}.

maybe_add_tls13_extensions(?TLS_1_3,
                           HelloExtensions0,
                           #{versions := SupportedVersions} = Opts,
                           KeyShare,
                           TicketData, CertDbHandle, CertDbRef) ->
    HelloExtensions1 =
        HelloExtensions0#{client_hello_versions =>
                              #client_hello_versions{versions = SupportedVersions}},
    HelloExtensions2 = maybe_add_key_share(HelloExtensions1, KeyShare),
    HelloExtensions = maybe_add_pre_shared_key(HelloExtensions2, TicketData),
    AddCA = maps:get(certificate_authorities, Opts, false),
    maybe_add_certificate_auths(HelloExtensions, CertDbHandle, CertDbRef, AddCA);

maybe_add_tls13_extensions(_, HelloExtensions, _, _, _, _,_) ->
    HelloExtensions.

maybe_add_certificate_status_request(_Version, #{stapling := _Stapling},
                                     OcspNonce, HelloExtensions) ->
    OcspRequestExtns = public_key:ocsp_extensions(OcspNonce),
    Req = #ocsp_status_request{request_extensions = OcspRequestExtns},
    CertStatusReqExtn = #certificate_status_request{
        status_type = ?CERTIFICATE_STATUS_TYPE_OCSP,
        request = Req
    },
    HelloExtensions#{status_request => CertStatusReqExtn};
maybe_add_certificate_status_request(_Version, _SslOpts, _OcspNonce,
                                     HelloExtensions) ->
    HelloExtensions.

%% TODO: Add support for PSK key establishment

%% RFC 8446 (TLS 1.3) - 4.2.8.  Key Share
%%
%% 4.2.8.1.  Diffie-Hellman Parameters
%% Diffie-Hellman [DH76] parameters for both clients and servers are
%% encoded in the opaque key_exchange field of a KeyShareEntry in a
%% KeyShare structure.  The opaque value contains the Diffie-Hellman
%% public value (Y = g^X mod p) for the specified group (see [RFC7919]
%% for group definitions) encoded as a big-endian integer and padded to
%% the left with zeros to the size of p in bytes.
%%
%% 4.2.8.2.  ECDHE Parameters
%%
%% ECDHE parameters for both clients and servers are encoded in the
%% opaque key_exchange field of a KeyShareEntry in a KeyShare structure.
%%
%% For secp256r1, secp384r1, and secp521r1, the contents are the
%% serialized value of the following struct:
%%
%%    struct {
%%        uint8 legacy_form = 4;
%%        opaque X[coordinate_length];
%%        opaque Y[coordinate_length];
%%    } UncompressedPointRepresentation;
%%
%% X and Y, respectively, are the binary representations of the x and y
%% values in network byte order.  There are no internal length markers,
%% so each number representation occupies as many octets as implied by
%% the curve parameters.  For P-256, this means that each of X and Y use
%% 32 octets, padded on the left by zeros if necessary.  For P-384, they
%% take 48 octets each.  For P-521, they take 66 octets each.
maybe_add_key_share(HelloExtensions, undefined) ->
    HelloExtensions;
maybe_add_key_share(HelloExtensions, KeyShare) ->
    #key_share_client_hello{client_shares = ClientShares0} = KeyShare,
    %% Keep only public keys
    ClientShares = lists:map(fun kse_remove_private_key/1, ClientShares0),
    HelloExtensions#{key_share => #key_share_client_hello{
                                     client_shares = ClientShares}}.


maybe_add_pre_shared_key(HelloExtensions, undefined) ->
    HelloExtensions;
maybe_add_pre_shared_key(HelloExtensions, TicketData) ->
    {Identities, Binders} = get_identities_binders(TicketData),

    %% A client MUST provide a "psk_key_exchange_modes" extension if it
    %% offers a "pre_shared_key" extension.
    HelloExtensions#{pre_shared_key =>
                         #pre_shared_key_client_hello{
                            offered_psks =
                                #offered_psks{
                                   identities = Identities,
                                   binders = Binders}},
                     psk_key_exchange_modes =>
                         #psk_key_exchange_modes{
                            ke_modes = [psk_ke, psk_dhe_ke]}}.

maybe_add_certificate_auths(HelloExtensions,_,_,false) ->
    HelloExtensions;
maybe_add_certificate_auths(HelloExtensions, CertDbHandle, CertDbRef, true) ->
    Auths = certificate_authorities(CertDbHandle, CertDbRef),
    HelloExtensions#{certificate_authorities => #certificate_authorities{authorities = Auths}}.

get_identities_binders(TicketData) ->
    get_identities_binders(TicketData, {[], []}, 0).
%%
get_identities_binders([], {Identities, Binders}, _) ->
    {lists:reverse(Identities), lists:reverse(Binders)};
get_identities_binders([#ticket_data{key = Key,
                                     identity = Identity,
                                     cipher_suite = {_, HKDF}}|T], {I0, B0}, N) ->
    %% Use dummy binder for proper calculation of packet size when creating
    %% the real binder value.
    Binder = dummy_binder(HKDF),
    %% Store ticket position in identities
    tls_client_ticket_store:update_ticket(Key, N),
    get_identities_binders(T, {[Identity|I0], [Binder|B0]}, N + 1).


dummy_binder(HKDF) ->
    binary:copy(<<0>>, ssl_cipher:hash_size(HKDF)).


add_server_share(server_hello, Extensions, KeyShare) ->
    #key_share_server_hello{server_share = ServerShare0} = KeyShare,
    %% Keep only public keys
    ServerShare = kse_remove_private_key(ServerShare0),
    Extensions#{key_share => #key_share_server_hello{
                                server_share = ServerShare}};
add_server_share(hello_retry_request, Extensions,
                 #key_share_server_hello{
                    server_share = #key_share_entry{group = Group}}) ->
    Extensions#{key_share => #key_share_hello_retry_request{
                                selected_group = Group}}.

add_alpn(Extensions, ALPN0) ->
    ALPN = encode_alpn([ALPN0], false),
    Extensions#{alpn => ALPN}.

add_selected_version(Extensions) ->
    SupportedVersions = #server_hello_selected_version{selected_version = ?TLS_1_3},
    Extensions#{server_hello_selected_version => SupportedVersions}.

kse_remove_private_key(#key_share_entry{
                      group = Group,
                      key_exchange =
                          #'ECPrivateKey'{publicKey = PublicKey}}) ->
    #key_share_entry{
       group = Group,
       key_exchange = PublicKey};
kse_remove_private_key(#key_share_entry{
                      group = Group,
                      key_exchange =
                          {PublicKey, _}}) ->
    #key_share_entry{
       group = Group,
       key_exchange = PublicKey}.


signature_algs_ext(undefined) ->
    undefined;
signature_algs_ext(SignatureSchemes0) ->
    %% The SSL option signature_algs contains both hash-sign algorithms (tuples) and
    %% signature schemes (atoms) if TLS 1.3 is configured.
    #signature_algorithms{signature_scheme_list = SignatureSchemes0}.

signature_algs_cert(undefined) ->
    undefined;
signature_algs_cert([default | SignatureSchemes]) ->
    #signature_algorithms_cert{signature_scheme_list = SignatureSchemes};
signature_algs_cert(SignatureSchemes) ->
    #signature_algorithms_cert{signature_scheme_list = SignatureSchemes}.

use_srtp_ext(#{use_srtp := #{protection_profiles := Profiles, mki := MKI}}) ->
    #use_srtp{protection_profiles = Profiles, mki = MKI};
use_srtp_ext(#{}) ->
    undefined.


handle_client_hello_extensions(RecordCB, Random, ClientCipherSuites,
                               Exts, Version,
			       #{secure_renegotiate := SecureRenegotation,
                                 alpn_preferred_protocols := ALPNPreferredProtocols} = Opts,
			       #session{cipher_suite = NegotiatedCipherSuite} = Session0,
			       ConnectionStates0, Renegotiation, IsResumed) ->
    Session = handle_srp_extension(maps:get(srp, Exts, undefined), Session0),
    MaxFragEnum = handle_mfl_extension(maps:get(max_frag_enum, Exts, undefined)),
    ConnectionStates1 = ssl_record:set_max_fragment_length(MaxFragEnum, ConnectionStates0),
    ConnectionStates = handle_renegotiation_extension(server, RecordCB, Version, maps:get(renegotiation_info, Exts, undefined),
						      Random, NegotiatedCipherSuite, 
						      ClientCipherSuites,
						      ConnectionStates1, Renegotiation, SecureRenegotation),

    Empty = empty_extensions(Version, server_hello),
    %% RFC 6066 - server doesn't include max_fragment_length for resumed sessions
    ServerMaxFragEnum = if IsResumed ->
                                undefined;
                           true ->
                                MaxFragEnum
                        end,
    ServerHelloExtensions = Empty#{renegotiation_info => renegotiation_info(RecordCB, server,
                                                                            ConnectionStates, Renegotiation),
                                   ec_point_formats => server_ecc_extension(Version, 
                                                                            maps:get(ec_point_formats, Exts, undefined)),
                                   use_srtp => use_srtp_ext(Opts),
                                   max_frag_enum => ServerMaxFragEnum
                                  },
    
    %% If we receive an ALPN extension and have ALPN configured for this connection,
    %% we handle it. Otherwise we check for the NPN extension.
    ALPN = maps:get(alpn, Exts, undefined),
    if
        ALPN =/= undefined, ALPNPreferredProtocols =/= undefined ->
            Protocol = handle_alpn_extension(ALPNPreferredProtocols, decode_alpn(ALPN)),
            {Session, ConnectionStates, Protocol,
             ServerHelloExtensions#{alpn => encode_alpn([Protocol], Renegotiation)}};
        true ->
            NextProtocolNegotiation = maps:get(next_protocol_negotiation, Exts, undefined),
            ProtocolsToAdvertise = handle_next_protocol_extension(NextProtocolNegotiation, Renegotiation, Opts),
            {Session, ConnectionStates, undefined,
             ServerHelloExtensions#{next_protocol_negotiation =>
                                        encode_protocols_advertised_on_server(ProtocolsToAdvertise)}}
    end.

handle_server_hello_extensions(RecordCB, Random, CipherSuite,
                               Exts, Version,
			       #{secure_renegotiate := SecureRenegotation} =
                                   SslOpts,
			       ConnectionStates0, Renegotiation, IsNew) ->
    ConnectionStates = handle_renegotiation_extension(client, RecordCB, Version,  
                                                      maps:get(renegotiation_info, Exts, undefined), Random, 
						      CipherSuite, undefined,
						      ConnectionStates0,
						      Renegotiation, SecureRenegotation),

    %% RFC 6066: handle received/expected maximum fragment length
    if IsNew ->
            ServerMaxFragEnum = maps:get(max_frag_enum, Exts, undefined),
            ConnMaxFragLen = maps:get(max_fragment_length, ConnectionStates0, undefined),
            ClientMaxFragEnum = max_frag_enum(ConnMaxFragLen),

            if ServerMaxFragEnum == ClientMaxFragEnum ->
                    ok;
               true ->
                    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
            end;
       true ->
            ok
    end,

    case handle_cert_status_extension(SslOpts, Exts) of
        #alert{} = Alert ->
            Alert;
        StaplingState ->
            %% If we receive an ALPN extension then this is the protocol selected,
            %% otherwise handle the NPN extension.
            ALPN = maps:get(alpn, Exts, undefined),
            case decode_alpn(ALPN) of
                %% ServerHello contains exactly one protocol: the one selected.
                %% We also ignore the ALPN extension during renegotiation (see encode_alpn/2).
                [Protocol] when not Renegotiation ->
                    {ConnectionStates, alpn, Protocol, StaplingState};
                [_] when Renegotiation ->
                    {ConnectionStates, alpn, undefined, StaplingState};
                undefined ->
                    NextProtocolNegotiation = maps:get(next_protocol_negotiation, Exts, undefined),
                    NextProtocolSelector = maps:get(next_protocol_selector, SslOpts, undefined),
                    Protocol = handle_next_protocol(NextProtocolNegotiation, NextProtocolSelector, Renegotiation),
                    {ConnectionStates, npn, Protocol, StaplingState};
                {error, Reason} ->
                    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason);
                [] ->
                    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, no_protocols_in_server_hello);
                [_|_] ->
                    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, too_many_protocols_in_server_hello)
            end
    end.

select_curve(Client, Server) ->
    select_curve(Client, Server, false).

select_curve(#elliptic_curves{elliptic_curve_list = ClientCurves},
	     #elliptic_curves{elliptic_curve_list = ServerCurves},
	     ServerOrder) ->
    case ServerOrder of
        false ->
            select_shared_curve(ClientCurves, ServerCurves);
        true ->
            select_shared_curve(ServerCurves, ClientCurves)
    end;
select_curve(undefined, _, _) ->
    %% Client did not send ECC extension use default curve if 
    %% ECC cipher is negotiated
    case tls_v1:ecc_curves([secp256r1]) of
        [] ->
            %% Curve not supported by cryptolib ECC algorithms can not be negotiated
            no_curve;
        [CurveOid] ->
            {namedCurve, CurveOid}
    end;
select_curve({supported_groups, Groups}, Server, HonorServerOrder) ->
    %% TLS-1.3 hello but lesser version chosen
    TLSCommonCurves = [secp256r1,secp384r1,secp521r1],
    Curves = [Name || Name <- Groups, lists:member(Name, TLSCommonCurves)],
    case tls_v1:ecc_curves(Curves) of
        [] ->
            select_curve(undefined, Server, HonorServerOrder);
        [_|_] = ClientCurves ->
            select_curve(#elliptic_curves{elliptic_curve_list = ClientCurves}, Server, HonorServerOrder)
    end.

%%--------------------------------------------------------------------
-spec select_hashsign(#hash_sign_algos{} | undefined,  undefined | binary(), 
		      atom(), [atom()], ssl_record:ssl_version()) ->
			     {atom(), atom()} | undefined  | #alert{}.

%%
%% Description: Handles signature_algorithms hello extension (server)
%%--------------------------------------------------------------------
select_hashsign(_, _, KeyExAlgo, _, _Version) when KeyExAlgo == dh_anon;
                                                   KeyExAlgo == ecdh_anon;
                                                   KeyExAlgo == srp_anon;
                                                   KeyExAlgo == psk;
                                                   KeyExAlgo == dhe_psk;
                                                   KeyExAlgo == ecdhe_psk ->
    {null, anon};
%% The signature_algorithms extension was introduced with TLS 1.2. Ignore it if we have
%% negotiated a lower version.
select_hashsign({ClientHashSigns, ClientSignatureSchemes},
                Cert, KeyExAlgo, undefined, ?TLS_1_2 = Version) ->
    select_hashsign({ClientHashSigns, ClientSignatureSchemes}, Cert, KeyExAlgo,
                    tls_v1:default_signature_algs([Version]), Version);
select_hashsign({#hash_sign_algos{hash_sign_algos = ClientHashSigns},
                 ClientSignatureSchemes0},
                Cert, KeyExAlgo, SupportedHashSigns, ?TLS_1_2) ->
    ClientSignatureSchemes = client_signature_schemes(ClientHashSigns, ClientSignatureSchemes0),
    {SignAlgo0, Param, PublicKeyAlgo0, _, _} = get_cert_params(Cert),
    SignAlgo = sign_algo(SignAlgo0, Param),
    PublicKeyAlgo = ssl_certificate:public_key_type(PublicKeyAlgo0),

    %% RFC 5246 (TLS 1.2)
    %% If the client provided a "signature_algorithms" extension, then all
    %% certificates provided by the server MUST be signed by a
    %% hash/signature algorithm pair that appears in that extension.
    %%
    %% RFC 8446 (TLS 1.3)
    %% TLS 1.3 provides two extensions for indicating which signature
    %% algorithms may be used in digital signatures.  The
    %% "signature_algorithms_cert" extension applies to signatures in
    %% certificates and the "signature_algorithms" extension, which
    %% originally appeared in TLS 1.2, applies to signatures in
    %% CertificateVerify messages.
    %%
    %% If no "signature_algorithms_cert" extension is
    %% present, then the "signature_algorithms" extension also applies to
    %% signatures appearing in certificates.
    case is_supported_sign(SignAlgo, ClientSignatureSchemes) of
        true ->
            case
                (KeyExAlgo == psk) orelse
                (KeyExAlgo == dhe_psk) orelse
                (KeyExAlgo == ecdhe_psk) orelse
                (KeyExAlgo == srp_anon) orelse
                (KeyExAlgo == dh_anon) orelse
                (KeyExAlgo == ecdhe_anon) of
                true ->
                    ClientSignatureSchemes;
                false ->
                    do_select_hashsign(ClientSignatureSchemes, PublicKeyAlgo, SupportedHashSigns)
            end;
        false ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)
    end;
select_hashsign(_, Cert, _, _, Version) ->
    #'OTPCertificate'{tbsCertificate = TBSCert} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPSubjectPublicKeyInfo'{algorithm = {_,Algo, _}} = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    select_hashsign_algs(undefined, Algo, Version).
%%--------------------------------------------------------------------
-spec select_hashsign(#certificate_request{},  binary(), 
		      [atom()], ssl_record:ssl_version()) ->
			     {atom(), atom()} | #alert{}.

%%
%% Description: Handles signature algorithms selection for certificate requests (client) 
%%--------------------------------------------------------------------
select_hashsign(#certificate_request{
                   hashsign_algorithms = #hash_sign_algos{
                                            hash_sign_algos = HashSigns},
                   certificate_types = Types},
                Cert,
                SupportedHashSigns0,
		?TLS_1_2) ->
    {SignAlgo0, Param, PublicKeyAlgo0, _, _} = get_cert_params(Cert),
    SignAlgo = sign_algo(SignAlgo0, Param),
    PublicKeyAlgo = ssl_certificate:public_key_type(PublicKeyAlgo0),
    case is_acceptable_cert_type(PublicKeyAlgo, Types) andalso
        is_supported_sign(SignAlgo, HashSigns) of
	true ->
            SupportedHashSigns = ssl_cipher:signature_schemes_1_2(SupportedHashSigns0),
            do_select_hashsign(HashSigns, PublicKeyAlgo, SupportedHashSigns);
	false ->
	    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)
    end;
select_hashsign(#certificate_request{certificate_types = Types}, Cert, _, Version) ->
    {_, _, PublicKeyAlgo0, _, _} = get_cert_params(Cert),
    PublicKeyAlgo = ssl_certificate:public_key_type(PublicKeyAlgo0),

    %% Check cert even for TLS 1.0/1.1
    case is_acceptable_cert_type(PublicKeyAlgo, Types) of
        true ->
            select_hashsign(undefined, Cert, undefined, [], Version);
        false ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)
    end.

do_select_hashsign(HashSigns, PublicKeyAlgo, SupportedHashSigns) ->
    TLS12Scheme = 
        fun(Scheme) ->
                {H, S, _} = ssl_cipher:scheme_to_components(Scheme),
                case S of
                    rsa_pkcs1 when PublicKeyAlgo == rsa ->
                        is_acceptable_hash_sign({H, rsa}, SupportedHashSigns) %% TLS-1.2 name
                            orelse is_acceptable_hash_sign(Scheme, SupportedHashSigns); %% TLS-1.3 legacy name
                    rsa_pss_rsae when PublicKeyAlgo == rsa  -> %% Backported
                        is_acceptable_hash_sign(Scheme, SupportedHashSigns);
                    rsa_pss_pss when PublicKeyAlgo  == rsa_pss_pss -> %% Backported
                        is_acceptable_hash_sign(Scheme, SupportedHashSigns);
                    ecdsa when (PublicKeyAlgo == ecdsa) andalso (H == sha) ->
                        is_acceptable_hash_sign({H, S}, SupportedHashSigns) orelse  %% TLS-1.2 name
                            is_acceptable_hash_sign(Scheme, SupportedHashSigns); %% TLS-1.3 legacy name
                    ecdsa when (PublicKeyAlgo == ecdsa)  ->
                        is_acceptable_hash_sign({H, S}, SupportedHashSigns);
                    _ ->
                        false
                end
        end,

    case lists:filter(
           fun({H, rsa_pss_pss = S} = Algos) when S == PublicKeyAlgo -> 
                   %% Backported from TLS-1.3, but only TLS-1.2 configured
                   is_acceptable_hash_sign(list_to_existing_atom(atom_to_list(S) ++ "_" ++ atom_to_list(H)), 
                                           SupportedHashSigns) orelse 
                       is_acceptable_hash_sign(Algos, SupportedHashSigns);
              ({H, rsa_pss_rsae = S} = Algos) when PublicKeyAlgo == rsa -> 
                   %% Backported from TLS-1.3, but only TLS-1.2 configured
                   is_acceptable_hash_sign(list_to_existing_atom(atom_to_list(S) ++ "_" ++ atom_to_list(H)), 
                                           SupportedHashSigns) orelse
                       is_acceptable_hash_sign(Algos, SupportedHashSigns);
              ({_, S} = Algos) when S == PublicKeyAlgo ->
                   is_acceptable_hash_sign(Algos, SupportedHashSigns);
              %% Backported or legacy schemes from TLS-1.3 (TLS-1.2 negotiated when TLS-1.3 supported)
              (Scheme) when is_atom(Scheme) ->
                   TLS12Scheme(Scheme);
              (_) ->
                   false
           end, HashSigns) of
        [] ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm);
        [HashSign | _] ->
            case ssl_cipher:scheme_to_components(HashSign) of
                {Hash, rsa_pkcs1, _} ->
                    {Hash, rsa};
                {Hash, Sign, _} ->
                    {Hash, Sign}
            end
    end.

%% Gets the relevant parameters of a certificate:
%% - signature algorithm
%% - parameters of the signature algorithm
%% - public key algorithm (key type)
%% - RSA key size in bytes
%% - Elliptic Curve (public key)
get_cert_params(Cert) ->
    #'OTPCertificate'{tbsCertificate = TBSCert,
		      signatureAlgorithm =
                          {_,SignAlgo, Param}} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPSubjectPublicKeyInfo'{algorithm = {_, PublicKeyAlgo, _},
                               subjectPublicKey = PublicKey} =
        TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    RSAKeySize =
        case PublicKey of
            #'RSAPublicKey'{modulus = Modulus} ->
                %% Get RSA key size in bytes
                byte_size(binary:encode_unsigned(Modulus));
            _ ->
                undefined
        end,
    Curve = get_ec_curve(TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo),
    {SignAlgo, Param, PublicKeyAlgo, RSAKeySize, Curve}.

get_ec_curve(#'OTPSubjectPublicKeyInfo'{
                algorithm = #'PublicKeyAlgorithm'{
                               algorithm = ?'id-ecPublicKey',
                               parameters = {namedCurve, ?'secp256r1'}}}) ->
    secp256r1;
get_ec_curve(#'OTPSubjectPublicKeyInfo'{
                algorithm = #'PublicKeyAlgorithm'{
                               algorithm = ?'id-ecPublicKey',
                               parameters = {namedCurve, ?'secp384r1'}}}) ->
    secp384r1;
get_ec_curve(#'OTPSubjectPublicKeyInfo'{
                algorithm = #'PublicKeyAlgorithm'{
                               algorithm = ?'id-ecPublicKey',
                               parameters = {namedCurve, ?'secp521r1'}}}) ->
    secp521r1;
get_ec_curve(#'OTPSubjectPublicKeyInfo'{
                algorithm = #'PublicKeyAlgorithm'{
                               algorithm = ?'id-ecPublicKey',
                               parameters = {namedCurve, ?'brainpoolP512r1'}}}) ->
    brainpoolP512r1;
get_ec_curve(#'OTPSubjectPublicKeyInfo'{
                algorithm = #'PublicKeyAlgorithm'{
                               algorithm = ?'id-ecPublicKey',
                               parameters = {namedCurve, ?'brainpoolP384r1'}}}) ->
    brainpoolP384r1;
get_ec_curve(#'OTPSubjectPublicKeyInfo'{
                algorithm = #'PublicKeyAlgorithm'{
                               algorithm = ?'id-ecPublicKey',
                               parameters = {namedCurve, ?'brainpoolP256r1'}}}) ->
    brainpoolP256r1;
get_ec_curve(#'OTPSubjectPublicKeyInfo'{
                algorithm = #'PublicKeyAlgorithm'{
                               algorithm = ?'id-ecPublicKey',
                               parameters = {ecParameters,
                                             #'ECParameters'{curve = #'Curve'{} = Curve,
                                                             base = Base,
                                                             order = Order,
                                                             cofactor = Cofactor}}}}) ->
    curve_to_atom(Curve, Base, Order, Cofactor);
get_ec_curve(_) ->
    unsupported.

curve_to_atom(#'Curve'{
                a = <<255,255,255,255,0,0,0,1,0,0,0,0,0,0,0,0,
                      0,0,0,0,255,255,255,255,255,255,255,255,255,255,255,252>>,
                b = <<90,198,53,216,170,58,147,231,179,235,189,85,118,152,134,188,
                      101,29,6,176,204,83,176,246,59,206,60,62,39,210,96,75>>,
                seed = <<196,157,54,8,134,231,4,147,106,102,120,225,19,157,38,183,
                         129,159,126,144>>},
              <<4,107,23,209,242,225,44,66,71,248,188,230,229,99,164,64,
                242,119,3,125,129,45,235,51,160,244,161,57,69,216,152,194,
                150,79,227,66,226,254,26,127,155,142,231,235,74,124,15,158,
                22,43,206,51,87,107,49,94,206,203,182,64,104,55,191,81,
                245>>,
              115792089210356248762697446949407573529996955224135760342422259061068512044369,
              1
             ) ->
    secp256r1;
curve_to_atom(#'Curve'{
                 a = <<255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                       255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,
                       255,255,255,255,0,0,0,0,0,0,0,0,255,255,255,252>>,
                 b = <<179,49,47,167,226, 62,231,228,152,142,5,107,227,248,45,25,
                       24,29,156,110,254,129,65,18,3,20,8,143,80,19,135,90,
                       198,86,57,141,138,46,209,157,42,133,200,237,211,236,42,239>>,
                 seed = <<163,53,146,106,163,25,162,122,29,0,137,106,103,115,164,130,
                          122,205,172,115>>},
              <<4,170,135,202,34,190,139,5,55,142,177,199,30,243,32,173,
                116,110,29,59,98,139,167,155,152,89,247,65,224,130,84,42,
                56,85,2,242,93,191,85,41,108,58,84,94,56,114,118,10,
                183,54,23,222,74,150,38,44,111,93,158,152,191,146,146,220,
                41,248,244,29,189,40,154,20,124,233,218,49,19,181,240,184,
                192,10,96,177,206,29,126,129,157,122,67,29,124,144,234,14,
                95>>,
              39402006196394479212279040100143613805079739270465446667946905279627659399113263569398956308152294913554433653942643,
              1) ->
    secp384r1;
curve_to_atom(#'Curve'{
                 a = <<1,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                       255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                       255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                       255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                       255,252>>,
                 b = <<0,81,149,62,185,97,142,28,154,31,146,154,33,160,182,133,
                       64,238,162,218,114,91,153,179,21,243,184,180,137,145,142,241,
                       9,225,86,25,57,81,236,126,147,123,22,82,192,189,59,177,
                       191,7,53,115,223,136,61,44,52,241,239,69,31,212,107,80,
                       63,0>>,
                 seed = <<208,158,136,0,41,28,184,83,150,204,103,23,57,50,132,170,
                          160,218,100,186>>},
              <<4,0,198,133,142,6,183,4,4,233,205,158,62,203,102,35,
                149,180,66,156,100,129,57,5,63,181,33,248,40,175,96,107,
                77,61,186,161,75,94,119,239,231,89,40,254,29,193,39,162,
                255,168,222,51,72,179,193,133,106,66,155,249,126,126,49,194,
                229,189,102,1,24,57,41,106,120,154,59,192,4,92,138,95,
                180,44,125,27,217,152,245,68,73,87,155,68,104,23,175,189,
                23,39,62,102,44,151,238,114,153,94,244,38,64,197,80,185,
                1,63,173,7,97,53,60,112,134,162,114,194,64,136,190,148,
                118,159,209,102,80>>,
              6864797660130609714981900799081393217269435300143305409394463459185543183397655394245057746333217197532963996371363321113864768612440380340372808892707005449,
              1) ->
    secp521r1;
curve_to_atom(_, _, _, _) ->
    unsupported.

select_own_cert([OwnCert| _]) ->
    OwnCert;
select_own_cert(undefined) ->
    undefined.

client_signature_schemes(ClientHashSigns, undefined) ->
    ClientHashSigns;
client_signature_schemes(_, #signature_algorithms_cert{
                               signature_scheme_list = ClientSignatureSchemes}) ->
    ssl_cipher:signature_schemes_1_2(ClientSignatureSchemes).


%%--------------------------------------------------------------------
-spec select_hashsign_algs({atom(), atom()}| undefined, oid(), ssl_record:ssl_version()) ->
				  {atom(), atom()}.

%% Description: For TLS 1.2 hash function and signature algorithm pairs can be
%% negotiated with the signature_algorithms extension,
%% for previous versions always use appropriate defaults.
%% RFC 5246, Sect. 7.4.1.4.1.  Signature Algorithms
%% If the client does not send the signature_algorithms extension, the
%% server MUST do the following: (e.i defaults for TLS 1.2)
%%
%% -  If the negotiated key exchange algorithm is one of (RSA, DHE_RSA,
%%    DH_RSA, RSA_PSK, ECDH_RSA, ECDHE_RSA), behave as if client had
%%    sent the value {sha1,rsa}.
%%
%% -  If the negotiated key exchange algorithm is one of (DHE_DSS,
%%    DH_DSS), behave as if the client had sent the value {sha1,dsa}.
%%
%% -  If the negotiated key exchange algorithm is one of (ECDH_ECDSA,
%%    ECDHE_ECDSA), behave as if the client had sent value {sha1,ecdsa}.

%%--------------------------------------------------------------------
select_hashsign_algs(HashSign, _, ?TLS_1_2) when HashSign =/= undefined  ->
    HashSign;
select_hashsign_algs(undefined, ?rsaEncryption, ?TLS_1_2)  ->
    {sha, rsa};
select_hashsign_algs(undefined,?'id-ecPublicKey', _) ->
    {sha, ecdsa};
select_hashsign_algs(undefined, ?rsaEncryption, _) -> 
    {md5sha, rsa};
select_hashsign_algs(undefined, ?'id-dsa', _) ->
    {sha, dsa}.

srp_user(#{srp_identity := {UserName, _}}) ->
    #srp{username = UserName};
srp_user(_) ->
    undefined.

extension_value(undefined) ->
    undefined;
extension_value(#sni{hostname = HostName}) ->
    HostName;
extension_value(#use_srtp{protection_profiles = ProtectionProfiles, mki = MKI}) ->
    #{protection_profiles => ProtectionProfiles, mki => MKI};
extension_value(#ec_point_formats{ec_point_format_list = List}) ->
    List;
extension_value(#elliptic_curves{elliptic_curve_list = List}) ->
    List;
extension_value(#supported_groups{supported_groups = SupportedGroups}) ->
    SupportedGroups;
extension_value(#hash_sign_algos{hash_sign_algos = Algos}) ->
    Algos;
extension_value(#alpn{extension_data = Data}) ->
    Data;
extension_value(#max_frag_enum{enum = Enum}) ->
    Enum;
extension_value(#next_protocol_negotiation{extension_data = Data}) ->
    Data;
extension_value(#srp{username = Name}) ->
    Name;
extension_value(#renegotiation_info{renegotiated_connection = Data}) ->
    Data;
extension_value(#signature_algorithms{signature_scheme_list = Schemes}) ->
    Schemes;
extension_value(#signature_algorithms_cert{signature_scheme_list = Schemes}) ->
    Schemes;
extension_value(#key_share_client_hello{client_shares = ClientShares}) ->
    ClientShares;
extension_value(#key_share_server_hello{server_share = ServerShare}) ->
    ServerShare;
extension_value(#client_hello_versions{versions = Versions}) ->
    Versions;
extension_value(#server_hello_selected_version{selected_version = SelectedVersion}) ->
    SelectedVersion;
extension_value(#pre_shared_key_client_hello{offered_psks = PSKs}) ->
    PSKs;
extension_value(#pre_shared_key_server_hello{selected_identity = SelectedIdentity}) ->
    SelectedIdentity;
extension_value(#psk_key_exchange_modes{ke_modes = Modes}) ->
    Modes;
extension_value(#cookie{cookie = Cookie}) ->
    Cookie.

handle_cert_status_extension(#{stapling := _Stapling}, Extensions) ->
    case maps:get(status_request, Extensions, false) of
        undefined -> %% status_request received in server hello
            #{configured => true,
              status => negotiated};
        false ->
            #{configured => true,
              status => not_negotiated};
        _Else ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, status_request_not_empty)
    end;
handle_cert_status_extension(_SslOpts, Extensions) ->
    case maps:get(status_request, Extensions, false) of
        false ->
            #{configured => false,
              status => not_negotiated};
        _Else -> %% unsolicited status_request
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, unexpected_status_request)
    end.

certificate_authorities(CertDbHandle, CertDbRef) ->
    Auths = fun(#'OTPCertificate'{tbsCertificate = TBSCert}) ->
                    TBSCert#'OTPTBSCertificate'.subject
            end,
    [Auths(Cert) || #cert{otp = Cert} <- certificate_authorities_from_db(CertDbHandle, CertDbRef)].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%------------- Create handshake messages ----------------------------

%% The end-entity certificate provided by the client MUST contain a
%% key that is compatible with certificate_types.
certificate_types(Version) when ?TLS_LTE(Version, ?TLS_1_2) ->
    ECDSA = supported_cert_type_or_empty(ecdsa, ?ECDSA_SIGN),
    RSA = supported_cert_type_or_empty(rsa, ?RSA_SIGN),
    DSS = supported_cert_type_or_empty(dss, ?DSS_SIGN),
    <<ECDSA/binary,RSA/binary,DSS/binary>>.

%% Returns encoded certificate_type if algorithm is supported
supported_cert_type_or_empty(Algo, Type) ->
    case proplists:get_bool(
           Algo,
           proplists:get_value(public_keys, crypto:supports())) of
        true ->
            <<?BYTE(Type)>>;
        false ->
            <<>>
    end.

certificate_authorities_from_db(CertDbHandle, CertDbRef) when is_reference(CertDbRef) ->
    ssl_pkix_db:select_certs_by_ref(CertDbRef, CertDbHandle);
certificate_authorities_from_db(_CertDbHandle, {extracted, CertDbData}) ->
    %% Cache disabled, Ref contains data
    lists:foldl(fun({decoded, {_Key,Cert}}, Acc) -> [Cert | Acc] end,
		[], CertDbData).

%%-------------Handle handshake messages --------------------------------
path_validate(TrustedAndPath, ServerName, Role, CertDbHandle, CertDbRef, CRLDbHandle,
              Version, SslOptions, ExtInfo) ->
    InitialPotentialError = {error, {bad_cert, unknown_ca}},
    InitialInvalidated = [],
    path_validate(TrustedAndPath, ServerName, Role, CertDbHandle, CertDbRef, CRLDbHandle,
                  Version, SslOptions, ExtInfo, InitialInvalidated, InitialPotentialError).

validation_fun_and_state({Fun, UserState0}, VerifyState, CertPath, LogLevel) ->
    {fun(OtpCert, DerCert, {extension, _} = Extension, {SslState, UserState}) ->
	     case ssl_certificate:validate(OtpCert,
					   Extension,
					   SslState,
                                           LogLevel) of
		 {valid, NewSslState} ->
		     {valid, {NewSslState, UserState}};
		 {fail, Reason} ->
		     apply_user_fun(Fun, OtpCert, DerCert, Reason, UserState,
                                    SslState, CertPath, LogLevel);
		 {unknown, _} ->
		     apply_user_fun(Fun, OtpCert, DerCert,
                                    Extension, UserState, SslState,
                                    CertPath, LogLevel)
	     end;
	(OtpCert, DerCert, VerifyResult, {SslState, UserState}) ->
	     apply_user_fun(Fun, OtpCert, DerCert, VerifyResult, UserState,
			    SslState, CertPath, LogLevel)
     end, {VerifyState, UserState0}};
validation_fun_and_state(undefined, VerifyState, CertPath, LogLevel) ->
    {fun(OtpCert, _DerCert, {extension, _} = Extension, SslState) ->
	     ssl_certificate:validate(OtpCert,
				      Extension,
				      SslState,
                                      LogLevel);
	(OtpCert, _DerCert, VerifyResult, SslState) when (VerifyResult == valid) or
                                                        (VerifyResult == valid_peer) ->
	     case cert_status_check(OtpCert, SslState, VerifyResult, CertPath, LogLevel) of
		 valid ->
                     ssl_certificate:validate(OtpCert, VerifyResult, SslState, LogLevel);
		 Reason ->
		     {fail, Reason}
	     end;
	(OtpCert, _DerCert, VerifyResult, SslState) ->
	     ssl_certificate:validate(OtpCert,
				      VerifyResult,
				      SslState, LogLevel)
     end, VerifyState}.

path_validation_options(Opts, ValidationFunAndState) ->
    PolicyOpts = maps:get(cert_policy_opts, Opts, []),
    [{max_path_length, maps:get(depth, Opts, ?DEFAULT_DEPTH)},
     {verify_fun, ValidationFunAndState} | PolicyOpts].

apply_user_fun(Fun, OtpCert, DerCert, VerifyResult0, UserState0, SslState, CertPath, LogLevel) when
      (VerifyResult0 == valid) or (VerifyResult0 == valid_peer) ->
    VerifyResult = maybe_check_hostname(OtpCert, VerifyResult0, SslState, LogLevel),
    case apply_fun(Fun, OtpCert, DerCert, VerifyResult, UserState0) of
	{Valid, UserState} when (Valid == valid) orelse (Valid == valid_peer) ->
	    case cert_status_check(OtpCert, SslState, VerifyResult, CertPath, LogLevel) of
		valid ->
		    {Valid, {SslState, UserState}};
		Result ->
		    apply_user_fun(Fun, OtpCert, DerCert, Result, UserState, SslState, CertPath, LogLevel)
	    end;
	{fail, _} = Fail ->
	    Fail
    end;
apply_user_fun(Fun, OtpCert, DerCert, ExtensionOrError, UserState0, SslState, _, _) ->
    case apply_fun(Fun, OtpCert, DerCert, ExtensionOrError, UserState0) of
	{Valid, UserState} when (Valid == valid) orelse (Valid == valid_peer)->
	    {Valid, {SslState, UserState}};
	{fail, _} = Fail ->
	    Fail;
	{unknown, UserState} ->
	    {unknown, {SslState, UserState}}
    end.

apply_fun(Fun, OtpCert, DerCert, ExtensionOrError, UserState) ->
    if is_function(Fun, 4) ->
            Fun(OtpCert, DerCert, ExtensionOrError, UserState);
       is_function(Fun, 3) ->
            Fun(OtpCert, ExtensionOrError, UserState)
    end.

maybe_check_hostname(OtpCert, valid_peer, SslState, LogLevel) ->
    case ssl_certificate:validate(OtpCert, valid_peer, SslState, LogLevel) of
        {valid, _} ->
            valid_peer;
        {fail, Reason} ->
            Reason
    end;
maybe_check_hostname(_, valid, _, _) ->
    valid.

path_validation_alert({bad_cert, cert_expired}, _, _) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_EXPIRED);
path_validation_alert({bad_cert, invalid_issuer}, _, _) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE, invalid_issuer);
path_validation_alert({bad_cert, invalid_signature}, _, _) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE, invalid_signature);
path_validation_alert({bad_cert, unsupported_signature}, _, _) ->
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, unsupported_signature);
path_validation_alert({bad_cert, name_not_permitted}, _, _) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE, name_not_permitted);
path_validation_alert({bad_cert, unknown_critical_extension}, _, _) ->
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, unknown_critical_extension);
path_validation_alert({bad_cert, {revoked, _}}, _, _) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_REVOKED);
path_validation_alert({bad_cert, {revocation_status_undetermined, Details}}, _, _) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE, Details);
path_validation_alert({bad_cert, selfsigned_peer}, _, _) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE, selfsigned_peer);
path_validation_alert({bad_cert, unknown_ca}, _, _) ->
    ?ALERT_REC(?FATAL, ?UNKNOWN_CA);
path_validation_alert({bad_cert, hostname_check_failed}, ServerName, #cert{otp = PeerCert}) ->
    SubjAltNames = subject_altnames(PeerCert),
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE,{bad_cert, {hostname_check_failed, {requested, ServerName}, 
                                                      {received, SubjAltNames}}});
path_validation_alert({bad_cert, invalid_ext_keyusage}, _, _) -> %% Detected by public key
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, {invalid_ext_keyusage,
                                                  "CA cert purpose anyExtendedKeyUsage"
                                                  "and extended-key-usage extension marked critical is not allowed"});
path_validation_alert({bad_cert, {invalid_ext_keyusage, ExtKeyUses}}, _, _) ->
     Uses = extkey_oids_to_names(ExtKeyUses, []),
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, {invalid_ext_keyusage, Uses});
path_validation_alert({bad_cert, {ca_invalid_ext_keyusage, ExtKeyUses}}, _, _) ->
     Uses = extkey_oids_to_names(ExtKeyUses, []),
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, {ca_invalid_ext_keyusage, Uses});
path_validation_alert({bad_cert, {key_usage_mismatch, _} = Reason}, _, _) ->
    ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, Reason);
path_validation_alert(Reason, _,_) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason).

digitally_signed(Version, Msg, HashAlgo, PrivateKey, SignAlgo) ->
    try do_digitally_signed(Version, Msg, HashAlgo, PrivateKey, SignAlgo)
    catch
	error:Reason:ST ->
            ?SSL_LOG(info, sign_error, [{error, Reason}, {stacktrace, ST}]),
	    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, bad_key(PrivateKey)))
    end.

do_digitally_signed(Version, Msg, HashAlgo, {#'RSAPrivateKey'{} = Key,
                                             #'RSASSA-PSS-params'{}}, SignAlgo) when ?TLS_GTE(Version, ?TLS_1_2) ->
    Options = signature_options(SignAlgo, HashAlgo),
    public_key:sign(Msg, HashAlgo, Key, Options);
do_digitally_signed(Version, {digest, Digest}, _HashAlgo, #'RSAPrivateKey'{} = Key, rsa)
  when ?TLS_LTE(Version, ?TLS_1_1) ->
    public_key:encrypt_private(Digest, Key,
			       [{rsa_pad, rsa_pkcs1_padding}]);
do_digitally_signed(Version, {digest, Digest}, _HashAlgo, #{algorithm := rsa, encrypt_fun := Fun} = Key0, rsa)
  when ?TLS_LTE(Version, ?TLS_1_1) ->
    CustomOpts = maps:get(encrypt_opts, Key0, []),
    Key = #{algorithm => rsa, encrypt_fun => Fun},
    public_key:encrypt_private(Digest, Key, CustomOpts ++ [{rsa_pad, rsa_pkcs1_padding}]);
do_digitally_signed(Version, {digest, Digest}, _,
                    #{algorithm := rsa, engine := _} = Engine, rsa) when ?TLS_LTE(Version, ?TLS_1_1) ->
    crypto:private_encrypt(rsa, Digest, maps:remove(algorithm, Engine),
                           rsa_pkcs1_padding);
do_digitally_signed(_, Msg, HashAlgo, #{algorithm := Alg, engine := _} = Engine, SignAlgo) ->
    Options = signature_options(SignAlgo, HashAlgo),
    crypto:sign(Alg, HashAlgo, Msg, maps:remove(algorithm, Engine), Options);
do_digitally_signed(Version, {digest, _} = Msg , HashAlgo, Key, _) when ?TLS_LTE(Version,?TLS_1_1) ->
    public_key:sign(Msg, HashAlgo, Key);
do_digitally_signed(_, Msg, HashAlgo, #{algorithm := SignAlgo, sign_fun := Fun} = Key0, SignAlgo) ->
    CustomOpts = maps:get(sign_opts, Key0, []),
    Options = signature_options(SignAlgo, HashAlgo),
    Key = #{algorithm => SignAlgo, sign_fun => Fun},
    public_key:sign(Msg, HashAlgo, Key, CustomOpts ++ Options);
do_digitally_signed(_, Msg, HashAlgo, Key, SignAlgo) ->
    Options = signature_options(SignAlgo, HashAlgo),
    public_key:sign(Msg, HashAlgo, Key, Options).
    
signature_options(rsa_pss_rsae, HashAlgo) ->
    pss_options(HashAlgo, hash_algo_byte_size(HashAlgo));
signature_options(rsa_pss_pss, HashAlgo) ->
    pss_options(HashAlgo, hash_algo_byte_size(HashAlgo));
signature_options(_, _) ->
    [].

verify_options(rsa_pss_rsae, HashAlgo) ->
    pss_options(HashAlgo, hash_algo_byte_size(HashAlgo));
verify_options(rsa_pss_pss, HashAlgo) ->
    pss_options(HashAlgo, hash_algo_byte_size(HashAlgo));
verify_options(_, _) ->
    [].

pss_options(HashAlgo, SaltLen) ->
    [{rsa_padding, rsa_pkcs1_pss_padding},
     {rsa_pss_saltlen, SaltLen},
     {rsa_mgf1_md, HashAlgo}].

hash_algo_byte_size(sha256) ->
    32;
hash_algo_byte_size(sha384) ->
    48;
hash_algo_byte_size(sha512) ->
    64.

bad_key(#'DSAPrivateKey'{}) ->
    unacceptable_dsa_key;
bad_key(#'RSAPrivateKey'{}) ->
    unacceptable_rsa_key;
bad_key(#'ECPrivateKey'{}) ->
    unacceptable_ecdsa_key;
bad_key(#{algorithm := rsa}) ->
    unacceptable_rsa_key;
bad_key(#{algorithm := rsa_pss_pss}) ->
    unacceptable_rsa_pss_pss_key;
bad_key(#{algorithm := eddsa}) ->
    unacceptable_eddsa_key;
bad_key(#{algorithm := ecdsa}) ->
    unacceptable_ecdsa_key.

cert_status_check(_,
                  #{stapling_state := #{configured := true,
                                        status := received_staple}},
                  _VerifyResult, _, _) ->
    %% OCSP staple(s) will now be checked by
    %% ssl_certificate:verify_cert_extensions/2 in ssl_certificate:validate
    valid;
cert_status_check(OtpCert,
                  #{stapling_state := #{configured := false}} = SslState,
                  VerifyResult, CertPath, LogLevel) ->
    maybe_check_crl(OtpCert, SslState, VerifyResult, CertPath, LogLevel);
cert_status_check(_OtpCert,
                  #{stapling_state := #{configured := true,
                                        status := StaplingStatus}},
                  _VerifyResult, _CertPath, _LogLevel)
  when StaplingStatus == not_negotiated; StaplingStatus == not_received ->
    %% RFC6066 section 8
    %% Servers that receive a client hello containing the "status_request"
    %% extension MAY return a suitable certificate status response to the
    %% client along with their certificate.
    valid.


maybe_check_crl(_, #{crl_check := false}, _, _, _) ->
    valid;
maybe_check_crl(_, #{crl_check := peer}, valid, _, _) -> %% Do not check CAs with this option.
    valid;
maybe_check_crl(OtpCert, #{crl_check := Check, 
                     certdb := CertDbHandle,
                     certdb_ref := CertDbRef,
                     crl_db := {Callback, CRLDbHandle}}, _, CertPath, LogLevel) ->
    Options = [{issuer_fun, {fun(_DP, CRL, Issuer, DBInfo) ->
				     ssl_crl:trusted_cert_and_path(CRL, Issuer, CertPath,
                                                                   DBInfo)
			     end, {CertDbHandle, CertDbRef}}}, 
	       {update_crl, fun(DP, CRL) ->
                                    case Callback:fresh_crl(DP, CRL) of
                                        {logger, LogInfo, Fresh} ->
                                            handle_log(LogLevel, LogInfo),
                                            Fresh;
                                        Fresh ->
                                            Fresh
                                    end
                            end},
               {undetermined_details, true}
	      ],
    case dps_and_crls(OtpCert, Callback, CRLDbHandle, ext, LogLevel) of
	no_dps ->
	    crl_check_same_issuer(OtpCert, Check,
				  dps_and_crls(OtpCert, Callback, CRLDbHandle, same_issuer, LogLevel),
				  Options);
	DpsAndCRLs ->  %% This DP list may be empty if relevant CRLs existed 
	    %% but could not be retrieved, will result in {bad_cert, revocation_status_undetermined}
	    case public_key:pkix_crls_validate(OtpCert, DpsAndCRLs, Options) of
		{bad_cert, {revocation_status_undetermined, _}} ->
		    crl_check_same_issuer(OtpCert, Check, 
                                          dps_and_crls(OtpCert, Callback, 
                                                       CRLDbHandle, same_issuer, LogLevel), Options);
		Other ->
		    Other
	    end
    end.

crl_check_same_issuer(OtpCert, best_effort, Dps, Options) ->		
    case public_key:pkix_crls_validate(OtpCert, Dps, Options) of 
	{bad_cert, {revocation_status_undetermined, _}}   ->
	    valid;
	Other ->
	    Other
    end;
crl_check_same_issuer(OtpCert, _, Dps, Options) ->    
    public_key:pkix_crls_validate(OtpCert, Dps, Options).

dps_and_crls(OtpCert, Callback, CRLDbHandle, ext, LogLevel) ->
    case public_key:pkix_dist_points(OtpCert) of
        [] ->
            no_dps;
        DistPoints ->
            Issuer = OtpCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer,
            CRLs = distpoints_lookup(DistPoints, Issuer, Callback, CRLDbHandle, LogLevel),
            [{DP, {CRL, public_key:der_decode('CertificateList', CRL)}} ||  DP <- DistPoints, CRL <- CRLs]
    end;

dps_and_crls(OtpCert, Callback, CRLDbHandle, same_issuer, LogLevel) ->    
    DP = #'DistributionPoint'{distributionPoint = {fullName, GenNames}} = 
	public_key:pkix_dist_point(OtpCert),
    CRLs = lists:flatmap(fun({directoryName, Issuer}) -> 
				 case Callback:select(Issuer, CRLDbHandle) of
                                     {logger, LogInfo, Return} ->
                                         handle_log(LogLevel, LogInfo),
                                         Return;
                                     Return ->
                                         Return
                                 end;
			    (_) ->
				 []
			 end, GenNames),
    [{DP, {CRL, public_key:der_decode('CertificateList', CRL)}} ||  CRL <- CRLs].


distpoints_lookup([],_, _, _, _) ->
    [];
distpoints_lookup([DistPoint | Rest], Issuer, Callback, CRLDbHandle, LogLevel) ->
    Result =
	try Callback:lookup(DistPoint, Issuer, CRLDbHandle)
	catch
	    error:undef ->
		%% The callback module still uses the 2-argument
		%% version of the lookup function.
		Callback:lookup(DistPoint, CRLDbHandle)
	end,
    case Result of
	not_available ->
	    distpoints_lookup(Rest, Issuer, Callback, CRLDbHandle, LogLevel);
	{logger, LogInfo, CRLs} ->
            handle_log(LogLevel, LogInfo),
            CRLs;
        CRLs ->
	    CRLs
    end.

encrypted_premaster_secret(Secret, RSAPublicKey) ->
    try
	PreMasterSecret = public_key:encrypt_public(Secret, RSAPublicKey,
						    [{rsa_pad,
						      rsa_pkcs1_padding}]),
	#encrypted_premaster_secret{premaster_secret = PreMasterSecret}
    catch
        _:Reason:ST->
            ?SSL_LOG(debug, encrypt_error, [{reason, Reason}, {stacktrace, ST}]),
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, premaster_encryption_failed))
    end.

-spec calc_certificate_verify(ssl_record:ssl_version(), md5sha | ssl:hash(), _, [binary()]) -> binary().
calc_certificate_verify(Version, HashAlgo, _MasterSecret, Handshake) when ?TLS_1_X(Version) ->
    tls_v1:certificate_verify(HashAlgo, lists:reverse(Handshake)).

calc_finished(Version, Role, PrfAlgo, MasterSecret, Handshake) when ?TLS_1_X(Version) ->
    tls_v1:finished(Role, Version, PrfAlgo, MasterSecret, lists:reverse(Handshake)).

master_secret(Version, MasterSecret,
	      #security_parameters{
		 bulk_cipher_algorithm = BCA,
		 client_random = ClientRandom,
		 server_random = ServerRandom,
		 hash_size = HashSize,
		 prf_algorithm = PrfAlgo,
		 key_material_length = KML,
		 iv_size = IVS},
	      ConnectionStates, Role) ->
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV} =
	setup_keys(Version, PrfAlgo, MasterSecret, ServerRandom,
		   ClientRandom, HashSize, KML, IVS),

    ConnStates1 = ssl_record:set_master_secret(MasterSecret, ConnectionStates),
    ConnStates2 =
	ssl_record:set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret,
				  Role, ConnStates1),

    ClientCipherState = ssl_cipher:cipher_init(BCA, ClientIV, ClientWriteKey),
    ServerCipherState = ssl_cipher:cipher_init(BCA, ServerIV, ServerWriteKey),
    {MasterSecret,
     ssl_record:set_pending_cipher_state(ConnStates2, ClientCipherState,
					 ServerCipherState, Role)}.
setup_keys(Version, PrfAlgo, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, IVS) when ?TLS_1_X(Version)->
    tls_v1:setup_keys(Version, PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
                      KML, IVS).
calc_master_secret(Version, PrfAlgo, PremasterSecret, ClientRandom, ServerRandom)
  when ?TLS_LT(Version, ?TLS_1_3) ->
    tls_v1:master_secret(PrfAlgo, PremasterSecret, ClientRandom, ServerRandom).
	
%% Update pending connection states with parameters exchanged via
%% hello messages
%% NOTE : Role is the role of the receiver of the hello message
%%        currently being processed.
hello_pending_connection_states(_RecordCB, Role, Version, CipherSuite, Random,
				 ConnectionStates) ->
    ReadState =
	ssl_record:pending_connection_state(ConnectionStates, read),
    WriteState =
	ssl_record:pending_connection_state(ConnectionStates, write),

    NewReadSecParams =
	hello_security_parameters(Role, Version, ReadState, CipherSuite, Random),
    
    NewWriteSecParams =
	hello_security_parameters(Role, Version, WriteState, CipherSuite, Random),

    ssl_record:set_security_params(NewReadSecParams,
				    NewWriteSecParams,
				    ConnectionStates).

hello_security_parameters(client, Version, #{security_parameters := SecParams},
                          CipherSuite, Random) ->
    NewSecParams = ssl_cipher:security_parameters(Version, CipherSuite, SecParams),
    NewSecParams#security_parameters{server_random = Random};

hello_security_parameters(server, Version, #{security_parameters := SecParams},
                          CipherSuite, Random) ->
    NewSecParams = ssl_cipher:security_parameters(Version, CipherSuite, SecParams),
    NewSecParams#security_parameters{
      client_random = Random
     }.

do_select_version(_, ClientVersion, []) ->
    ClientVersion;
do_select_version(RecordCB, ClientVersion, [Version | Versions]) ->
    case RecordCB:is_higher(Version, ClientVersion) of
	true ->
	    %% Version too high for client - keep looking
	    do_select_version(RecordCB, ClientVersion, Versions);
	false ->
	    %% Version ok for client - look for a higher
	    do_select_version(RecordCB, ClientVersion, Versions, Version)
    end.
%%
do_select_version(_, _, [], GoodVersion) ->
    GoodVersion;
do_select_version(
  RecordCB, ClientVersion, [Version | Versions], GoodVersion) ->
    BetterVersion =
	case RecordCB:is_higher(Version, ClientVersion) of
	    true ->
		%% Version too high for client
		GoodVersion;
	    false ->
		%% Version ok for client
		case RecordCB:is_higher(Version, GoodVersion) of
		    true ->
			%% Use higher version
			Version;
		    false ->
			GoodVersion
		end
	end,
    do_select_version(RecordCB, ClientVersion, Versions, BetterVersion).

%%-------------Encode handshakes --------------------------------

encode_server_key(#server_dh_params{dh_p = P, dh_g = G, dh_y = Y}) ->
    PLen = byte_size(P),
    GLen = byte_size(G),
    YLen = byte_size(Y),
    <<?UINT16(PLen), P/binary, ?UINT16(GLen), G/binary, ?UINT16(YLen), Y/binary>>;
encode_server_key(#server_ecdh_params{curve = {namedCurve, ECCurve}, public = ECPubKey}) ->
    %%TODO: support arbitrary keys
    KLen = byte_size(ECPubKey),
    <<?BYTE(?NAMED_CURVE), ?UINT16((tls_v1:oid_to_enum(ECCurve))),
      ?BYTE(KLen), ECPubKey/binary>>;
encode_server_key(#server_psk_params{hint = PskIdentityHint}) ->
    Len = byte_size(PskIdentityHint),
    <<?UINT16(Len), PskIdentityHint/binary>>;
encode_server_key(Params = #server_dhe_psk_params{hint = undefined}) ->
    encode_server_key(Params#server_dhe_psk_params{hint = <<>>});
encode_server_key(#server_dhe_psk_params{
		  hint = PskIdentityHint,
		  dh_params = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y}}) ->
    Len = byte_size(PskIdentityHint),
    PLen = byte_size(P),
    GLen = byte_size(G),
    YLen = byte_size(Y),
    <<?UINT16(Len), PskIdentityHint/binary,
      ?UINT16(PLen), P/binary, ?UINT16(GLen), G/binary, ?UINT16(YLen), Y/binary>>;
encode_server_key(Params = #server_ecdhe_psk_params{hint = undefined}) ->
    encode_server_key(Params#server_ecdhe_psk_params{hint = <<>>});
encode_server_key(#server_ecdhe_psk_params{
		     hint = PskIdentityHint,
		     dh_params = #server_ecdh_params{
		       curve = {namedCurve, ECCurve}, public = ECPubKey}}) ->
    %%TODO: support arbitrary keys
    Len = byte_size(PskIdentityHint),
    KLen = byte_size(ECPubKey),
    <<?UINT16(Len), PskIdentityHint/binary,
      ?BYTE(?NAMED_CURVE), ?UINT16((tls_v1:oid_to_enum(ECCurve))),
      ?BYTE(KLen), ECPubKey/binary>>;
encode_server_key(#server_srp_params{srp_n = N, srp_g = G,	srp_s = S, srp_b = B}) ->
    NLen = byte_size(N),
    GLen = byte_size(G),
    SLen = byte_size(S),
    BLen = byte_size(B),
    <<?UINT16(NLen), N/binary, ?UINT16(GLen), G/binary,
      ?BYTE(SLen), S/binary, ?UINT16(BLen), B/binary>>.

encode_client_key(#encrypted_premaster_secret{premaster_secret = PKEPMS}) ->
    PKEPMSLen = byte_size(PKEPMS),
    <<?UINT16(PKEPMSLen), PKEPMS/binary>>;
encode_client_key(#client_diffie_hellman_public{dh_public = DHPublic}) ->
    Len = byte_size(DHPublic),
    <<?UINT16(Len), DHPublic/binary>>;
encode_client_key(#client_ec_diffie_hellman_public{dh_public = DHPublic}) ->
    Len = byte_size(DHPublic),
    <<?BYTE(Len), DHPublic/binary>>;
encode_client_key(#client_psk_identity{identity = undefined}) ->
    Id = <<"psk_identity">>,
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary>>;
encode_client_key(#client_psk_identity{identity = Id}) ->
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary>>;
encode_client_key(Identity = #client_dhe_psk_identity{identity = undefined}) ->
    encode_client_key(Identity#client_dhe_psk_identity{identity = <<"psk_identity">>});
encode_client_key(#client_dhe_psk_identity{identity = Id, dh_public = DHPublic}) ->
    Len = byte_size(Id),
    DHLen = byte_size(DHPublic),
    <<?UINT16(Len), Id/binary, ?UINT16(DHLen), DHPublic/binary>>;
encode_client_key(Identity = #client_ecdhe_psk_identity{identity = undefined}) ->
    encode_client_key(Identity#client_ecdhe_psk_identity{identity = <<"psk_identity">>});
encode_client_key(#client_ecdhe_psk_identity{identity = Id, dh_public = DHPublic}) ->
    Len = byte_size(Id),
    DHLen = byte_size(DHPublic),
    <<?UINT16(Len), Id/binary, ?BYTE(DHLen), DHPublic/binary>>;
encode_client_key(Identity = #client_rsa_psk_identity{identity = undefined}) ->
    encode_client_key(Identity#client_rsa_psk_identity{identity = <<"psk_identity">>});
encode_client_key(#client_rsa_psk_identity{identity = Id, exchange_keys = ExchangeKeys}) ->
    EncPMS = encode_client_key(ExchangeKeys),
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary, EncPMS/binary>>;
encode_client_key(#client_srp_public{srp_a = A}) ->
    Len = byte_size(A),
    <<?UINT16(Len), A/binary>>.

enc_sign({_, anon}, _Sign, _Version) ->
    <<>>;
enc_sign({HashAlg, SignAlg}, Signature, Version)
  when ?TLS_GTE(Version, ?TLS_1_2)->
	SignLen = byte_size(Signature),
	HashSign = enc_hashsign(HashAlg, SignAlg),
	<<HashSign/binary, ?UINT16(SignLen), Signature/binary>>;
enc_sign(_HashSign, Sign, _Version) ->
	SignLen = byte_size(Sign),
	<<?UINT16(SignLen), Sign/binary>>.

enc_hashsign(HashAlgo, SignAlgo) when SignAlgo == rsa_pss_pss;
                                      SignAlgo == rsa_pss_rsae ->
    Sign = ssl_cipher:signature_scheme(list_to_existing_atom(atom_to_list(SignAlgo) ++ "_" ++ atom_to_list(HashAlgo))),
    <<?UINT16(Sign)>>;
enc_hashsign(HashAlgo, SignAlgo) ->
    Hash = ssl_cipher:hash_algorithm(HashAlgo),
    Sign = ssl_cipher:sign_algorithm(SignAlgo),
    <<?BYTE(Hash), ?BYTE(Sign)>>.

encode_protocol(Protocol, Acc) ->
	Len = byte_size(Protocol),
	<<Acc/binary, ?BYTE(Len), Protocol/binary>>.

enc_server_key_exchange(Version, Params, {HashAlgo, SignAlgo},
			ClientRandom, ServerRandom, PrivateKey) ->
    EncParams = encode_server_key(Params),
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
	    Signature = digitally_signed(Version, Hash, HashAlgo, PrivateKey, SignAlgo),
	    #server_key_params{params = Params,
			       params_bin = EncParams,
			       hashsign = {HashAlgo, SignAlgo},
			       signature = Signature}
    end.

%% While the RFC opens the door to allow ALPN during renegotiation, in practice
%% this does not work and it is recommended to ignore any ALPN extension during
%% renegotiation, as done here.
encode_alpn(_, true) ->
    undefined;
encode_alpn(undefined, _) ->
    undefined;
encode_alpn(Protocols, _) ->
    #alpn{extension_data = lists:foldl(fun encode_protocol/2, <<>>, Protocols)}.


encode_versions(Versions) ->
    << <<?BYTE(M),?BYTE(N)>> || {M,N} <- Versions>>.

encode_client_shares(ClientShares) ->
    << << (encode_key_share_entry(KeyShareEntry0))/binary >> || KeyShareEntry0 <- ClientShares >>.

encode_key_share_entry(#key_share_entry{group = Group,
                                        key_exchange = KeyExchange}) ->
    Len = byte_size(KeyExchange),
    <<?UINT16((tls_v1:group_to_enum(Group))),?UINT16(Len),KeyExchange/binary>>.

encode_psk_key_exchange_modes(KEModes) ->
    << <<?BYTE((choose_psk_key(PskKey)))>> || PskKey <- KEModes>>.
%
choose_psk_key(psk_ke) -> ?PSK_KE;
choose_psk_key(psk_dhe_ke) -> ?PSK_DHE_KE.

encode_psk_identities(Identities) ->
    Result = << << ?UINT16((byte_size(Identity))), Identity/binary,?UINT32(Age) >>
                || #psk_identity{ identity = Identity, obfuscated_ticket_age = Age}  <- Identities >>,
    Len = byte_size(Result),
    <<?UINT16(Len), Result/binary>>.

encode_psk_binders(Binders) ->
    Result = << << ?BYTE((byte_size(Binder))),Binder/binary >> || Binder <- Binders >>,
    Len = byte_size(Result),
    <<?UINT16(Len), Result/binary>>.


hello_extensions_list(HelloExtensions) ->
    [Ext || {_, Ext} <- maps:to_list(HelloExtensions), Ext =/= undefined].

%%-------------Decode handshakes---------------------------------
dec_server_key(<<?UINT16(PLen), P:PLen/binary,
		 ?UINT16(GLen), G:GLen/binary,
		 ?UINT16(YLen), Y:YLen/binary, _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_DIFFIE_HELLMAN, Version) ->
    Params = #server_dh_params{dh_p = P, dh_g = G, dh_y = Y},
    {BinMsg, HashSign, Signature} = dec_server_key_params(PLen + GLen + YLen + 6, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
%% ECParameters with named_curve
%% TODO: explicit curve
dec_server_key(<<?BYTE(?NAMED_CURVE), ?UINT16(CurveID),
		 ?BYTE(PointLen), ECPoint:PointLen/binary,
		 _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN, Version) ->
    Params = #server_ecdh_params{curve = {namedCurve, tls_v1:enum_to_oid(CurveID)},
				 public = ECPoint},
    {BinMsg, HashSign, Signature} = dec_server_key_params(PointLen + 4, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(<<?UINT16(Len), PskIdentityHint:Len/binary, _/binary>> = KeyStruct,
	       KeyExchange, Version)
  when KeyExchange == ?KEY_EXCHANGE_PSK; KeyExchange == ?KEY_EXCHANGE_RSA_PSK ->
    Params = #server_psk_params{
		hint = PskIdentityHint},
    {BinMsg, HashSign, Signature} = dec_server_key_params(Len + 2, KeyStruct, Version),
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
    {BinMsg, HashSign, Signature} = dec_server_key_params(Len + PLen + GLen + YLen + 8, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(<<?UINT16(Len), IdentityHint:Len/binary,
		 ?BYTE(?NAMED_CURVE), ?UINT16(CurveID),
		 ?BYTE(PointLen), ECPoint:PointLen/binary,
		 _/binary>> = KeyStruct,
	       ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN_PSK, Version) ->
    DHParams = #server_ecdh_params{
                  curve = {namedCurve, tls_v1:enum_to_oid(CurveID)},
                  public = ECPoint},
    Params = #server_ecdhe_psk_params{
                hint = IdentityHint,
                dh_params = DHParams},
    {BinMsg, HashSign, Signature} = dec_server_key_params(Len + 2 + PointLen + 4, KeyStruct, Version),
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
    {BinMsg, HashSign, Signature} = dec_server_key_params(NLen + GLen + SLen + BLen + 7, KeyStruct, Version),
    #server_key_params{params = Params,
		       params_bin = BinMsg,
		       hashsign = HashSign,
		       signature = Signature};
dec_server_key(_, KeyExchange, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {unknown_or_malformed_key_exchange, KeyExchange})).

dec_client_key(<<?UINT16(_), PKEPMS/binary>>, ?KEY_EXCHANGE_RSA, _) ->
    #encrypted_premaster_secret{premaster_secret = PKEPMS};
dec_client_key(<<>>, ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    throw(?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, empty_dh_public));
dec_client_key(<<?UINT16(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    #client_diffie_hellman_public{dh_public = DH_Y};
dec_client_key(<<>>, ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN, _) ->
    throw(?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, empty_dh_public));
dec_client_key(<<?BYTE(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN, _) ->
    #client_ec_diffie_hellman_public{dh_public = DH_Y};
dec_client_key(<<?UINT16(Len), Id:Len/binary>>,
	       ?KEY_EXCHANGE_PSK, _) ->
    #client_psk_identity{identity = Id};
dec_client_key(<<?UINT16(Len), Id:Len/binary,
		 ?UINT16(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_DHE_PSK, _) ->
    #client_dhe_psk_identity{identity = Id, dh_public = DH_Y};
dec_client_key(<<?UINT16(Len), Id:Len/binary,
		 ?BYTE(DH_YLen), DH_Y:DH_YLen/binary>>,
	       ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN_PSK, _) ->
    #client_ecdhe_psk_identity{identity = Id, dh_public = DH_Y};
dec_client_key(<<?UINT16(Len), Id:Len/binary, ?UINT16(_), PKEPMS/binary>>,
	       ?KEY_EXCHANGE_RSA_PSK, _) ->
    #client_rsa_psk_identity{identity = Id,
			     exchange_keys = #encrypted_premaster_secret{premaster_secret = PKEPMS}};
dec_client_key(<<?UINT16(ALen), A:ALen/binary>>,
	       ?KEY_EXCHANGE_SRP, _) ->
    #client_srp_public{srp_a = A}.

dec_server_key_params(Len, Keys, Version) ->
    <<Params:Len/bytes, Signature/binary>> = Keys,
    dec_server_key_signature(Params, Signature, Version).

dec_server_key_signature(Params, <<?BYTE(8), ?BYTE(SignAlgo),
                                   ?UINT16(0)>>, Version)
  when ?TLS_GTE(Version, ?TLS_1_2) ->
    <<?UINT16(Scheme0)>> = <<?BYTE(8), ?BYTE(SignAlgo)>>,
    Scheme = ssl_cipher:signature_scheme(Scheme0),
    {Hash, Sign, _} = ssl_cipher:scheme_to_components(Scheme),
    {Params, {Hash, Sign}, <<>>};
dec_server_key_signature(Params, <<?BYTE(8), ?BYTE(SignAlgo),
                                   ?UINT16(Len), Signature:Len/binary>>, Version)
  when ?TLS_GTE(Version, ?TLS_1_2) ->
    <<?UINT16(Scheme0)>> = <<?BYTE(8), ?BYTE(SignAlgo)>>,
    Scheme = ssl_cipher:signature_scheme(Scheme0),
    {Hash, Sign, _} = ssl_cipher:scheme_to_components(Scheme),
    {Params, {Hash, Sign}, Signature};
dec_server_key_signature(Params, <<?BYTE(HashAlgo), ?BYTE(SignAlgo),
			    ?UINT16(0)>>, Version)
  when ?TLS_GTE(Version, ?TLS_1_2) ->
    HashSign = {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)},
    {Params, HashSign, <<>>};
dec_server_key_signature(Params, <<?BYTE(HashAlgo), ?BYTE(SignAlgo),
			    ?UINT16(Len), Signature:Len/binary>>, Version)
  when ?TLS_GTE(Version, ?TLS_1_2) ->
    HashSign = {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)},
    {Params, HashSign, Signature};
dec_server_key_signature(Params, <<>>, _) ->
    {Params, {null, anon}, <<>>};
dec_server_key_signature(Params, <<?UINT16(0)>>, _) ->
    {Params, {null, anon}, <<>>};
dec_server_key_signature(Params, <<?UINT16(Len), Signature:Len/binary>>, _) ->
    {Params, undefined, Signature};
dec_server_key_signature(_, _, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, failed_to_decrypt_server_key_sign)).

%% Processes a ClientHello/ServerHello message and returns the version to be used
%% in the decoding functions. The following rules apply:
%% - IF supported_versions extension is absent:
%%   RETURN the lowest of (LocalVersion and LegacyVersion)
%% - IF supported_versions estension is present:
%%   RETURN the lowest of (LocalVersion and first element of supported versions)
process_supported_versions_extension(<<>>, LocalVersion, LegacyVersion)
  when LegacyVersion =< LocalVersion ->
    LegacyVersion;
process_supported_versions_extension(<<>>, LocalVersion, _LegacyVersion) ->
    LocalVersion;
process_supported_versions_extension(<<?UINT16(?SUPPORTED_VERSIONS_EXT), ?UINT16(Len),
                                       ExtData:Len/binary, _Rest/binary>>,
                                     LocalVersion, _LegacyVersion) when Len > 2 ->
    <<?BYTE(_),Versions0/binary>> = ExtData,
    [Highest|_] = decode_versions(Versions0),
    if Highest =< LocalVersion ->
            Highest;
       true ->
            LocalVersion
    end;
process_supported_versions_extension(<<?UINT16(?SUPPORTED_VERSIONS_EXT), ?UINT16(Len),
                                       ?BYTE(Major),?BYTE(Minor), _Rest/binary>>,
                                     LocalVersion, _LegacyVersion) when Len =:= 2 ->
    SelectedVersion = {Major, Minor},
    if SelectedVersion =< LocalVersion ->
            SelectedVersion;
       true ->
            LocalVersion
    end;
process_supported_versions_extension(<<?UINT16(_), ?UINT16(Len),
                                       _ExtData:Len/binary, Rest/binary>>,
                                     LocalVersion, LegacyVersion) ->
    process_supported_versions_extension(Rest, LocalVersion, LegacyVersion);
%% Tolerate protocol encoding errors and skip parsing the rest of the extension.
process_supported_versions_extension(_, LocalVersion, LegacyVersion)
  when LegacyVersion =< LocalVersion ->
    LegacyVersion;
process_supported_versions_extension(_, LocalVersion, _) ->
    LocalVersion.

decode_extensions(<<>>, _Version, _MessageType, Acc) ->
    Acc;
decode_extensions(<<?UINT16(?ALPN_EXT), ?UINT16(ExtLen), ?UINT16(Len),
                    ExtensionData:Len/binary, Rest/binary>>, Version, MessageType, Acc)
  when Len + 2 =:= ExtLen ->
    ALPN = #alpn{extension_data = ExtensionData},
    decode_extensions(Rest, Version, MessageType, Acc#{alpn => ALPN});
decode_extensions(<<?UINT16(?NEXTPROTONEG_EXT), ?UINT16(Len),
                    ExtensionData:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    NextP = #next_protocol_negotiation{extension_data = ExtensionData},
    decode_extensions(Rest, Version, MessageType, Acc#{next_protocol_negotiation => NextP});
decode_extensions(<<?UINT16(?RENEGOTIATION_EXT), ?UINT16(Len),
                    Info:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    RenegotiateInfo = case Len of
			  1 ->  % Initial handshake
			      Info; % should be <<0>> will be matched in handle_renegotiation_info
			  _ ->
			      VerifyLen = Len - 1,
			      <<?BYTE(VerifyLen), VerifyInfo/binary>> = Info,
			      VerifyInfo
		      end,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{renegotiation_info =>
                               #renegotiation_info{renegotiated_connection =
                                                       RenegotiateInfo}});

decode_extensions(<<?UINT16(?SRP_EXT), ?UINT16(Len), ?BYTE(SRPLen),
                    SRP:SRPLen/binary, Rest/binary>>, Version, MessageType, Acc)
  when Len == SRPLen + 1 ->
    decode_extensions(Rest, Version, MessageType, Acc#{srp => #srp{username = SRP}});

decode_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc)
  when ?TLS_LT(Version, ?TLS_1_2) ->
    SignAlgoListLen = Len - 2,
    <<?UINT16(SignAlgoListLen), SignAlgoList/binary>> = ExtData,
    HashSignAlgos = [{ssl_cipher:hash_algorithm(Hash), ssl_cipher:sign_algorithm(Sign)} ||
			<<?BYTE(Hash), ?BYTE(Sign)>> <= SignAlgoList],
    decode_extensions(Rest, Version, MessageType,
                      Acc#{signature_algs =>
                               #hash_sign_algos{hash_sign_algos =
                                                    HashSignAlgos}});
decode_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, ?TLS_1_2=Version, MessageType, Acc) ->
    SignSchemeListLen = Len - 2,
    <<?UINT16(SignSchemeListLen), SignSchemeList/binary>> = ExtData,
    HashSigns = decode_sign_alg(Version, SignSchemeList),
    decode_extensions(Rest, Version, MessageType,
                      Acc#{signature_algs =>
                               #hash_sign_algos{
                                  hash_sign_algos = HashSigns}});
decode_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, ?TLS_1_3=Version, MessageType, Acc) ->
    SignSchemeListLen = Len - 2,
    <<?UINT16(SignSchemeListLen), SignSchemeList/binary>> = ExtData,
    SignSchemes = decode_sign_alg(Version, SignSchemeList),
    decode_extensions(Rest, Version, MessageType,
                      Acc#{signature_algs =>
                               #signature_algorithms{
                                  signature_scheme_list = SignSchemes}});

decode_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, ?TLS_1_3=Version, MessageType, Acc) ->
    SignSchemeListLen = Len - 2,
    <<?UINT16(SignSchemeListLen), SignSchemeList/binary>> = ExtData,
    %% Ignore unknown signature algorithms
    Fun = fun(Elem) ->
                  case ssl_cipher:signature_scheme(Elem) of
                      unassigned ->
                          false;
                      Value ->
                          {true, Value}
                  end
          end,
    SignSchemes= lists:filtermap(Fun, [SignScheme ||
                                          <<?UINT16(SignScheme)>> <= SignSchemeList]),
    decode_extensions(Rest, Version, MessageType,
                      Acc#{signature_algs =>
                               #signature_algorithms{
                                  signature_scheme_list = SignSchemes}});

decode_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_CERT_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    SignSchemeListLen = Len - 2,
    <<?UINT16(SignSchemeListLen), SignSchemeList/binary>> = ExtData,
    %% Ignore unknown signature algorithms
    Fun = fun(Elem) ->
                  case ssl_cipher:signature_scheme(Elem) of
                      unassigned ->
                          false;
                      Value ->
                          {true, Value}
                  end
          end,
    SignSchemes= lists:filtermap(Fun, [SignScheme ||
                                          <<?UINT16(SignScheme)>> <= SignSchemeList]),
    decode_extensions(Rest, Version, MessageType,
                      Acc#{signature_algs_cert =>
                               #signature_algorithms_cert{
                                  signature_scheme_list = SignSchemes}});

decode_extensions(<<?UINT16(?USE_SRTP_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    <<?UINT16(ProfilesLen), ProfilesBin:ProfilesLen/binary, ?BYTE(MKILen), MKI:MKILen/binary>> = ExtData,
    Profiles = [P || <<P:2/binary>> <= ProfilesBin],
    decode_extensions(Rest, Version, MessageType,
                      Acc#{use_srtp =>
                              #use_srtp{
                                 protection_profiles = Profiles,
                                 mki = MKI}});

decode_extensions(<<?UINT16(?ELLIPTIC_CURVES_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc)
  when ?TLS_LT(Version, ?TLS_1_3) ->
    <<?UINT16(_), EllipticCurveList/binary>> = ExtData,
    %% Ignore unknown curves
    Pick = fun(Enum) ->
		   case tls_v1:enum_to_oid(Enum) of
		       undefined ->
			   false;
		       Oid ->
			   {true, Oid}
		   end
	   end,
    EllipticCurves = lists:filtermap(Pick, [ECC || <<ECC:16>> <= EllipticCurveList]),
    decode_extensions(Rest, Version, MessageType,
                      Acc#{elliptic_curves =>
                               #elliptic_curves{elliptic_curve_list =
                                                    EllipticCurves}});

decode_extensions(<<?UINT16(?ELLIPTIC_CURVES_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, ?TLS_1_3=Version, MessageType, Acc) ->
    <<?UINT16(_), GroupList/binary>> = ExtData,
    %% Ignore unknown curves
    Pick = fun(Enum) ->
		   case tls_v1:enum_to_group(Enum) of
		       undefined ->
			   false;
		       Group ->
			   {true, Group}
		   end
	   end,
    SupportedGroups = lists:filtermap(Pick, [Group || <<Group:16>> <= GroupList]),
    decode_extensions(Rest, Version, MessageType,
                      Acc#{elliptic_curves =>
                               #supported_groups{supported_groups =
                                                     SupportedGroups}});

decode_extensions(<<?UINT16(?EC_POINT_FORMATS_EXT), ?UINT16(Len),
                    ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    <<?BYTE(_), ECPointFormatList/binary>> = ExtData,
    ECPointFormats = binary_to_list(ECPointFormatList),
    decode_extensions(Rest, Version, MessageType,
                      Acc#{ec_point_formats =>
                               #ec_point_formats{ec_point_format_list =
                                                     ECPointFormats}});

decode_extensions(<<?UINT16(?SNI_EXT), ?UINT16(Len),
                    Rest/binary>>, Version, MessageType, Acc) when Len == 0 ->
    decode_extensions(Rest, Version, MessageType,
                      Acc#{sni => #sni{hostname = ""}}); %% Server may send an empty SNI

decode_extensions(<<?UINT16(?SNI_EXT), ?UINT16(Len),
                ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    <<?UINT16(_), NameList/binary>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{sni => dec_sni(NameList)});

decode_extensions(<<?UINT16(?MAX_FRAGMENT_LENGTH_EXT), ?UINT16(1), ?BYTE(MaxFragEnum), Rest/binary>>,
                  Version, MessageType, Acc) ->
    %% RFC 6066 Section 4
    decode_extensions(Rest, Version, MessageType, Acc#{max_frag_enum => #max_frag_enum{enum = MaxFragEnum}});
decode_extensions(<<?UINT16(?SUPPORTED_VERSIONS_EXT), ?UINT16(Len),
                       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc) when Len > 2 ->
    <<?BYTE(_),Versions/binary>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{client_hello_versions =>
                               #client_hello_versions{
                                  versions = decode_versions(Versions)}});

decode_extensions(<<?UINT16(?SUPPORTED_VERSIONS_EXT), ?UINT16(Len),
                       ?UINT16(SelectedVersion), Rest/binary>>, Version, MessageType, Acc)
  when Len =:= 2, SelectedVersion =:= 16#0304 ->
    decode_extensions(Rest, Version, MessageType,
                      Acc#{server_hello_selected_version =>
                               #server_hello_selected_version{selected_version = ?TLS_1_3}});

decode_extensions(<<?UINT16(?KEY_SHARE_EXT), ?UINT16(Len),
                       ExtData:Len/binary, Rest/binary>>,
                  Version, MessageType = client_hello, Acc) ->
    <<?UINT16(_),ClientShares/binary>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{key_share =>
                               #key_share_client_hello{
                                  client_shares = decode_client_shares(ClientShares)}});

decode_extensions(<<?UINT16(?KEY_SHARE_EXT), ?UINT16(Len),
                    ExtData:Len/binary, Rest/binary>>,
                  Version, MessageType = server_hello, Acc) ->
    <<?UINT16(Group),?UINT16(KeyLen),KeyExchange:KeyLen/binary>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{key_share =>
                               #key_share_server_hello{
                                  server_share =
                                      #key_share_entry{
                                         group = tls_v1:enum_to_group(Group),
                                         key_exchange = KeyExchange}}});

decode_extensions(<<?UINT16(?KEY_SHARE_EXT), ?UINT16(Len),
                    ExtData:Len/binary, Rest/binary>>,
                  Version, MessageType = hello_retry_request, Acc) ->
    <<?UINT16(Group)>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{key_share =>
                               #key_share_hello_retry_request{
                                  selected_group = tls_v1:enum_to_group(Group)}});

decode_extensions(<<?UINT16(?PSK_KEY_EXCHANGE_MODES_EXT), ?UINT16(Len),
                       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    <<?BYTE(PLen),KEModes:PLen/binary>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{psk_key_exchange_modes =>
                               #psk_key_exchange_modes{
                                  ke_modes = decode_psk_key_exchange_modes(KEModes)}});

decode_extensions(<<?UINT16(?PRE_SHARED_KEY_EXT), ?UINT16(Len),
                       ExtData:Len/binary, Rest/binary>>,
                  Version, MessageType = client_hello, Acc) ->
    <<?UINT16(IdLen),Identities:IdLen/binary,?UINT16(BLen),Binders:BLen/binary>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{pre_shared_key =>
                               #pre_shared_key_client_hello{
                                  offered_psks = #offered_psks{
                                                    identities = decode_psk_identities(Identities),
                                                    binders = decode_psk_binders(Binders)}}});

decode_extensions(<<?UINT16(?PRE_SHARED_KEY_EXT), ?UINT16(Len),
                       ExtData:Len/binary, Rest/binary>>,
                  Version, MessageType = server_hello, Acc) ->
    <<?UINT16(Identity)>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{pre_shared_key =>
                               #pre_shared_key_server_hello{
                                  selected_identity = Identity}});

decode_extensions(<<?UINT16(?COOKIE_EXT), ?UINT16(Len), ?UINT16(CookieLen),
                    Cookie:CookieLen/binary, Rest/binary>>,
                  Version, MessageType, Acc)
  when Len == CookieLen + 2 ->
    decode_extensions(Rest, Version, MessageType,
                      Acc#{cookie => #cookie{cookie = Cookie}});

%% RFC6066, if a server returns a "CertificateStatus" message, then
%% the server MUST have included an extension of type "status_request"
%% with empty "extension_data" in the extended server hello.
decode_extensions(<<?UINT16(?STATUS_REQUEST), ?UINT16(Len),
                    _ExtensionData:Len/binary, Rest/binary>>, Version,
                    MessageType = server_hello, Acc)
  when Len =:= 0 ->
    decode_extensions(Rest, Version, MessageType,
                      Acc#{status_request => undefined});
%% RFC8446 4.4.2.1, In TLS1.3, the body of the "status_request" extension
%% from the server MUST be a CertificateStatus structure as defined in
%% RFC6066.
decode_extensions(<<?UINT16(?STATUS_REQUEST), ?UINT16(Len),
                    CertStatus:Len/binary, Rest/binary>>, Version,
                    MessageType, Acc) ->
    case CertStatus of
        <<?BYTE(?CERTIFICATE_STATUS_TYPE_OCSP),
          ?UINT24(OCSPLen),
          ASN1OCSPResponse:OCSPLen/binary>> ->
            decode_extensions(Rest, Version, MessageType,
                      Acc#{status_request => #certificate_status{response = ASN1OCSPResponse}});
        _Other ->
            decode_extensions(Rest, Version, MessageType, Acc)
    end;

decode_extensions(<<?UINT16(?EARLY_DATA_EXT), ?UINT16(0), Rest/binary>>,
                  Version, MessageType, Acc) ->
    decode_extensions(Rest, Version, MessageType,
                      Acc#{early_data => #early_data_indication{}});

decode_extensions(<<?UINT16(?EARLY_DATA_EXT), ?UINT16(4), ?UINT32(MaxSize),
                    Rest/binary>>,
                  Version, MessageType, Acc) ->
    decode_extensions(Rest, Version, MessageType,
                      Acc#{early_data =>
                               #early_data_indication_nst{indication = MaxSize}});
decode_extensions(<<?UINT16(?CERTIFICATE_AUTHORITIES_EXT), ?UINT16(Len), 
                    CertAutsExt:Len/binary, Rest/binary>>,
                  Version, MessageType, Acc) ->
    CertAutsLen = Len - 2,
    <<?UINT16(CertAutsLen), EncCertAuts/binary>> = CertAutsExt,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{certificate_authorities =>
                               #certificate_authorities{authorities = decode_cert_auths(EncCertAuts, [])}});
%% Ignore data following the ClientHello (i.e.,
%% extensions) if not understood.
decode_extensions(<<?UINT16(_), ?UINT16(Len), _Unknown:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    decode_extensions(Rest, Version, MessageType, Acc);
%% This theoretically should not happen if the protocol is followed, but if it does it is ignored.
decode_extensions(_, _, _, Acc) ->
    Acc.

decode_sign_alg(?TLS_1_2, SignSchemeList) ->
    %% Ignore unknown signature algorithms
    Fun = fun(Elem) ->
                  case ssl_cipher:signature_scheme(Elem) of
                      unassigned ->
                          false;
                      Value when is_atom(Value)->
                          case ssl_cipher:scheme_to_components(Value) of
                              {Hash, rsa_pss_rsae = Sign, _} ->
                                  {true, {Hash, Sign}};
                              {Hash, rsa_pss_pss = Sign, _} ->
                                  {true,{Hash, Sign}};
                              {Hash, rsa_pkcs1, _} ->
                                  {true,{Hash, rsa}};
                              {Hash, ecdsa, _} ->
                                  {true,{Hash, ecdsa}};
                              _ ->
                                  false
                          end;
                      Value ->
                          {true, Value}
                  end
          end,
    lists:filtermap(Fun, [SignScheme ||
                             <<?UINT16(SignScheme)>> <= SignSchemeList]);
decode_sign_alg(?TLS_1_3, SignSchemeList) ->
    %% Ignore unknown signature algorithms
    Fun = fun(Elem) ->
                  case ssl_cipher:signature_scheme(Elem) of
                      unassigned ->
                          false;
                      Value ->
                          {true, Value}
                  end
          end,
    lists:filtermap(Fun, [SignScheme ||
                             <<?UINT16(SignScheme)>> <= SignSchemeList]).

dec_hashsign(Value) ->
    [HashSign] = decode_sign_alg(?TLS_1_2, Value),
    HashSign.


%% Ignore unknown names (only host_name is supported)
dec_sni(<<?BYTE(?SNI_NAMETYPE_HOST_NAME), ?UINT16(Len),
                HostName:Len/binary, _/binary>>) ->
    #sni{hostname = binary_to_list(HostName)};
dec_sni(<<?BYTE(_), ?UINT16(Len), _:Len, Rest/binary>>) -> dec_sni(Rest);
dec_sni(_) -> undefined.

decode_alpn(undefined) ->
    undefined;
decode_alpn(#alpn{extension_data=Data}) ->
    decode_protocols(Data, []).

decode_versions(Versions) ->
    decode_versions(Versions, []).
%%
decode_versions(<<>>, Acc) ->
    lists:reverse(Acc);
decode_versions(<<?BYTE(M),?BYTE(N),Rest/binary>>, Acc) ->
    decode_versions(Rest, [{M,N}|Acc]).


decode_client_shares(ClientShares) ->
    decode_client_shares(ClientShares, []).
%%
decode_client_shares(<<>>, Acc) ->
    lists:reverse(Acc);
decode_client_shares(<<?UINT16(Group0),?UINT16(Len),KeyExchange:Len/binary,Rest/binary>>, Acc) ->
    case tls_v1:enum_to_group(Group0) of
        undefined ->
            %% Ignore key_share with unknown group
            decode_client_shares(Rest, Acc);
        Group ->
            decode_client_shares(Rest, [#key_share_entry{
                                           group = Group,
                                           key_exchange= KeyExchange
                                          }|Acc])
    end.


decode_next_protocols({next_protocol_negotiation, Protocols}) ->
    decode_protocols(Protocols, []).

decode_protocols(<<>>, Acc) ->
    lists:reverse(Acc);
decode_protocols(<<?BYTE(Len), Protocol:Len/binary, Rest/binary>>, Acc) ->
    case Len of
        0 ->
            {error, invalid_protocols};
        _ ->
            decode_protocols(Rest, [Protocol|Acc])
    end;
decode_protocols(_Bytes, _Acc) ->
    {error, invalid_protocols}.


decode_psk_key_exchange_modes(KEModes) ->
    decode_psk_key_exchange_modes(KEModes, []).
%%
decode_psk_key_exchange_modes(<<>>, Acc) ->
    lists:reverse(Acc);
decode_psk_key_exchange_modes(<<?BYTE(?PSK_KE), Rest/binary>>, Acc) ->
    decode_psk_key_exchange_modes(Rest, [psk_ke|Acc]);
decode_psk_key_exchange_modes(<<?BYTE(?PSK_DHE_KE), Rest/binary>>, Acc) ->
    decode_psk_key_exchange_modes(Rest, [psk_dhe_ke|Acc]);
%% Ignore unknown PskKeyExchangeModes
decode_psk_key_exchange_modes(<<?BYTE(_), Rest/binary>>, Acc) ->
    decode_psk_key_exchange_modes(Rest, Acc).


decode_psk_identities(Identities) ->
    decode_psk_identities(Identities, []).
%%
decode_psk_identities(<<>>, Acc) ->
    lists:reverse(Acc);
decode_psk_identities(<<?UINT16(Len), Identity:Len/binary, ?UINT32(Age), Rest/binary>>, Acc) ->
    decode_psk_identities(Rest, [#psk_identity{
                                    identity = Identity,
                                    obfuscated_ticket_age = Age}|Acc]).

decode_psk_binders(Binders) ->
    decode_psk_binders(Binders, []).
%%
decode_psk_binders(<<>>, Acc) ->
    lists:reverse(Acc);
decode_psk_binders(<<?BYTE(Len), Binder:Len/binary, Rest/binary>>, Acc) ->
    decode_psk_binders(Rest, [Binder|Acc]).

decode_cert_auths(<<>>, Acc) ->
    lists:reverse(Acc);
decode_cert_auths(<<?UINT16(Len), Auth:Len/binary, Rest/binary>>, Acc) ->
    decode_cert_auths(Rest, [public_key:pkix_normalize_name(Auth) | Acc]).

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

key_exchange_alg(rsa) ->
    ?KEY_EXCHANGE_RSA;
key_exchange_alg(Alg) when Alg == dhe_rsa; Alg == dhe_dss;
			   Alg == dh_dss; Alg == dh_rsa; Alg == dh_anon ->
    ?KEY_EXCHANGE_DIFFIE_HELLMAN;
key_exchange_alg(Alg) when Alg == ecdhe_rsa; Alg == ecdh_rsa;
			   Alg == ecdhe_ecdsa; Alg == ecdh_ecdsa;
			   Alg == ecdh_anon ->
    ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN;
key_exchange_alg(psk) ->
    ?KEY_EXCHANGE_PSK;
key_exchange_alg(dhe_psk) ->
    ?KEY_EXCHANGE_DHE_PSK;
key_exchange_alg(ecdhe_psk) ->
    ?KEY_EXCHANGE_EC_DIFFIE_HELLMAN_PSK;
key_exchange_alg(rsa_psk) ->
    ?KEY_EXCHANGE_RSA_PSK;
key_exchange_alg(Alg)
  when Alg == srp_rsa; Alg == srp_dss; Alg == srp_anon ->
    ?KEY_EXCHANGE_SRP;
key_exchange_alg(_) ->
    ?NULL.

%%-------------Cipher suite handling -----------------------------
select_cipher_suite(CipherSuites, Suites, false) ->
    select_cipher_suite(CipherSuites, Suites);
select_cipher_suite(CipherSuites, Suites, true) ->
    select_cipher_suite(Suites, CipherSuites).

select_cipher_suite(ClientSuites, SupportedSuites) ->
    F = fun(Suite) -> is_member(Suite, SupportedSuites) end,
    case lists:search(F, ClientSuites) of
        {value, Suite} -> Suite;
        false -> no_suite
    end.

is_member(Suite, SupportedSuites) ->
    lists:member(Suite, SupportedSuites).

psk_secret(PSKIdentity, PSKLookup) ->
    case handle_psk_identity(PSKIdentity, PSKLookup) of
	{ok, PSK} when is_binary(PSK) ->
	    Len = erlang:byte_size(PSK),
	    <<?UINT16(Len), 0:Len/unit:8, ?UINT16(Len), PSK/binary>>;
	#alert{} = Alert ->
	    Alert;
	_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end.

psk_secret(PSKIdentity, PSKLookup, PremasterSecret) ->
    case handle_psk_identity(PSKIdentity, PSKLookup) of
	{ok, PSK} when is_binary(PSK) ->
	    Len = erlang:byte_size(PremasterSecret),
	    PSKLen = erlang:byte_size(PSK),
	    <<?UINT16(Len), PremasterSecret/binary, ?UINT16(PSKLen), PSK/binary>>;
	#alert{} = Alert ->
	    Alert;
	_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end.

handle_psk_identity(_PSKIdentity, LookupFun)
  when LookupFun == undefined ->
    error;
handle_psk_identity(PSKIdentity, {Fun, UserState}) ->
    Fun(psk, PSKIdentity, UserState).


filter_hashsigns(Suites, Algos, HashSigns, Version) ->
    %% HashSigns, and Version never change
    ZipperF = fun (Suite, #{key_exchange := KeyExchange}) -> {Suite, KeyExchange} end,
    SuiteAlgoPairs = lists:zipwith(ZipperF, Suites, Algos),
    FilterHashSign = fun ({Suite, Kex}) ->
                             maybe true ?= filter_hashsigns_helper(Kex, HashSigns, Version),
                                   {true, Suite}
                             end
                     end,
    lists:filtermap(FilterHashSign, SuiteAlgoPairs).

filter_hashsigns_helper(KeyExchange, HashSigns, _Version)
  when KeyExchange == dhe_ecdsa;
       KeyExchange == ecdhe_ecdsa ->
    lists:keymember(ecdsa, 2, HashSigns);
filter_hashsigns_helper(KeyExchange, HashSigns, ?TLS_1_2) when KeyExchange == rsa;
			   KeyExchange == dhe_rsa;
			   KeyExchange == ecdhe_rsa;
			   KeyExchange == srp_rsa;
			   KeyExchange == rsa_psk ->
    lists:any(fun (H) -> lists:keymember(H, 2, HashSigns) end,
              [rsa, rsa_pss_rsae, rsa_pss_pss]);
filter_hashsigns_helper(KeyExchange, HashSigns, _Version) when KeyExchange == rsa;
			   KeyExchange == dhe_rsa;
			   KeyExchange == ecdhe_rsa;
			   KeyExchange == srp_rsa;
			   KeyExchange == rsa_psk ->
    lists:keymember(rsa, 2, HashSigns);
filter_hashsigns_helper(KeyExchange, HashSigns, _Version) when
      KeyExchange == dhe_dss;
      KeyExchange == srp_dss ->
    lists:keymember(dsa, 2, HashSigns);

filter_hashsigns_helper(KeyExchange, _HashSigns, _Version) when
      KeyExchange == dh_dss; 
      KeyExchange == dh_rsa; 
      KeyExchange == dh_ecdsa;
      KeyExchange == ecdh_rsa;    
      KeyExchange == ecdh_ecdsa ->
      %%  Fixed DH certificates MAY be signed with any hash/signature
      %%  algorithm pair appearing in the hash_sign extension.  The names
    %%  DH_DSS, DH_RSA, ECDH_ECDSA, and ECDH_RSA are historical.
    true;
filter_hashsigns_helper(KeyExchange, _HashSigns, _Version) when
      KeyExchange == dh_anon;
      KeyExchange == ecdh_anon;
      KeyExchange == srp_anon;
      KeyExchange == psk;
      KeyExchange == dhe_psk;
      KeyExchange == ecdhe_psk ->
    %% In this case hashsigns is not used as the kexchange is anonaymous
    true.

filter_unavailable_ecc_suites(no_curve, Suites) ->
    ECCSuites = ssl_cipher:filter_suites(Suites, #{key_exchange_filters => [fun(ecdh_ecdsa) -> true; 
                                                                               (ecdhe_ecdsa) -> true; 
                                                                               (ecdh_rsa) -> true; 
                                                                               (_) -> false 
                                                                            end],
                                                   cipher_filters => [],
                                                   mac_filters => [],
                                                   prf_filters => []}),
    Suites -- ECCSuites;       
filter_unavailable_ecc_suites(_, Suites) ->
    Suites.
%%-------------Extension handling --------------------------------

handle_renegotiation_extension(Role, RecordCB, Version, Info, Random, NegotiatedCipherSuite, 
			       ClientCipherSuites,
			       ConnectionStates0, Renegotiation, SecureRenegotation) ->
    {ok, ConnectionStates} = handle_renegotiation_info(Version, RecordCB, Role, Info, ConnectionStates0,
                                                       Renegotiation, SecureRenegotation,
                                                       ClientCipherSuites),
    hello_pending_connection_states(RecordCB, Role,
                                    Version,
                                    NegotiatedCipherSuite,
                                    Random,
                                    ConnectionStates).

%% Receive protocols, choose one from the list, return it.
handle_alpn_extension(_, {error, Reason}) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason));
handle_alpn_extension([], _) ->
    throw(?ALERT_REC(?FATAL, ?NO_APPLICATION_PROTOCOL));
handle_alpn_extension([ServerProtocol|Tail], ClientProtocols) ->
    case lists:member(ServerProtocol, ClientProtocols) of
        true -> ServerProtocol;
            false -> handle_alpn_extension(Tail, ClientProtocols)
    end.

handle_mfl_extension(#max_frag_enum{enum = Enum}=MaxFragEnum) when Enum >= 1, Enum =< 4 ->
    MaxFragEnum;
handle_mfl_extension(#max_frag_enum{}) ->
    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER));
handle_mfl_extension(_) ->
    undefined.

handle_next_protocol(undefined,
		     _NextProtocolSelector, _Renegotiating) ->
    undefined;

handle_next_protocol(#next_protocol_negotiation{} = NextProtocols,
    NextProtocolSelector, Renegotiating) ->

    case next_protocol_extension_allowed(NextProtocolSelector, Renegotiating) of
        true ->
            select_next_protocol(decode_next_protocols(NextProtocols), NextProtocolSelector);
        false ->
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, unexpected_next_protocol_extension))
    end.


handle_next_protocol_extension(NextProtocolNegotiation, Renegotiation, SslOpts)->
    case handle_next_protocol_on_server(NextProtocolNegotiation, Renegotiation, SslOpts) of
	#alert{} = Alert ->
	    throw(Alert);
	ProtocolsToAdvertise ->
	    ProtocolsToAdvertise
    end.

handle_next_protocol_on_server(undefined, _Renegotiation, _SslOpts) ->
    undefined;
handle_next_protocol_on_server(#next_protocol_negotiation{extension_data = <<>>},
			       false, SslOpts) ->
    maps:get(next_protocols_advertised, SslOpts, undefined);

handle_next_protocol_on_server(_Hello, _Renegotiation, _SSLOpts) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, unexpected_next_protocol_extension).

next_protocol_extension_allowed(NextProtocolSelector, Renegotiating) ->
    NextProtocolSelector =/= undefined andalso not Renegotiating.

select_next_protocol({error, Reason}, _NextProtocolSelector) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason);
select_next_protocol(Protocols, NextProtocolSelector) ->
    case NextProtocolSelector(Protocols) of
	?NO_PROTOCOL ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, no_next_protocol);
	Protocol when is_binary(Protocol)  ->
	    Protocol
    end.

handle_srp_extension(undefined, Session) ->
    Session;
handle_srp_extension(#srp{username = Username}, Session) ->
    Session#session{srp_username = Username}.

is_acceptable_hash_sign(Algos, SupportedHashSigns) ->
    lists:member(Algos, SupportedHashSigns).

is_acceptable_cert_type(Sign, Types) ->
    lists:member(sign_type(Sign), binary_to_list(Types)).

%% {'SignatureAlgorithm',{1,2,840,113549,1,1,11},'NULL'}
%% TODO: Implement validation for the curve used in the signature
%% RFC 3279 - 2.2.3 ECDSA Signature Algorithm
%% When the ecdsa-with-SHA1 algorithm identifier appears as the
%% algorithm field in an AlgorithmIdentifier, the encoding MUST omit the
%% parameters field.  That is, the AlgorithmIdentifier SHALL be a
%% SEQUENCE of one component: the OBJECT IDENTIFIER ecdsa-with-SHA1.
%%
%% The elliptic curve parameters in the subjectPublicKeyInfo field of
%% the certificate of the issuer SHALL apply to the verification of the
%% signature.
is_supported_sign({Hash, Sign}, SignatureSchemes) ->
    Fun = fun (Scheme) ->
                  {H, S0, _} = ssl_cipher:scheme_to_components(Scheme),
                  S1 = case S0 of
                           rsa_pkcs1 ->
                               rsa;
                           rsa_pss_rsae ->
                               rsa;
                           ecdsa_sha1 ->
                               ecdsa;
                           S ->
                               S
                       end,
                  (Sign  =:= S1) andalso (Hash  =:= H)
          end,
    lists:any(Fun, SignatureSchemes).


%% SupportedSignatureAlgorithms SIGNATURE-ALGORITHM-CLASS ::= {
%%   dsa-with-sha1 | dsaWithSHA1 |  md2-with-rsa-encryption |
%%   md5-with-rsa-encryption | sha1-with-rsa-encryption | sha-1with-rsa-encryption |
%%   sha224-with-rsa-encryption |
%%   sha256-with-rsa-encryption |
%%   sha384-with-rsa-encryption |
%%   sha512-with-rsa-encryption |
%%   ecdsa-with-sha1 |
%%   ecdsa-with-sha224 |
%%   ecdsa-with-sha256 |
%%   ecdsa-with-sha384 |
%%   ecdsa-with-sha512 }
sign_algo(?'id-RSASSA-PSS', #'RSASSA-PSS-params'{maskGenAlgorithm =
                                                     #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                                                         parameters = #'HashAlgorithm'{algorithm = HashOid}}}) ->
    {public_key:pkix_hash_type(HashOid), rsa_pss_pss};
sign_algo(Alg, _) ->
    public_key:pkix_sign_types(Alg).

sign_type(rsa_pss_pss) ->
    ?RSA_SIGN;
sign_type(rsa) ->
    ?RSA_SIGN;
sign_type(dsa) ->
    ?DSS_SIGN;
sign_type(ecdsa) ->
    ?ECDSA_SIGN.

server_name(_, _, server) ->
    undefined; %% Not interesting to check your own name.
server_name(SSLOpts, Host, client) ->
    case maps:get(server_name_indication, SSLOpts, undefined) of
        disable -> disable;
        undefined -> convert_hostname(Host); %% Fallback to Host argument to connect
        UserSNI -> convert_hostname(UserSNI)  %% If Server Name Indication is available
    end.

convert_hostname(SNI) when is_atom(SNI) ->
    atom_to_list(SNI);
convert_hostname(SNI) ->
    SNI.

client_ecc_extensions(SupportedECCs) ->
    CryptoSupport = proplists:get_value(public_keys, crypto:supports()),
    case proplists:get_bool(ecdh, CryptoSupport) of
	true ->
            %% RFC 8422 - 5.1.  Client Hello Extensions
            %% Clients SHOULD send both the Supported Elliptic Curves Extension and the
            %% Supported Point Formats Extension.  If the Supported Point Formats
            %% Extension is indeed sent, it MUST contain the value 0 (uncompressed)
            %% as one of the items in the list of point formats.
	    EcPointFormats = #ec_point_formats{ec_point_format_list = [?ECPOINT_UNCOMPRESSED]},
	    EllipticCurves = SupportedECCs,
	    {EcPointFormats, EllipticCurves};
	_ ->
	    {undefined, undefined}
    end.

server_ecc_extension(_Version, EcPointFormats) ->
    CryptoSupport = proplists:get_value(public_keys, crypto:supports()),
    case proplists:get_bool(ecdh, CryptoSupport) of
	true ->
	    handle_ecc_point_fmt_extension(EcPointFormats);
	false ->
	    undefined
    end.

handle_ecc_point_fmt_extension(undefined) ->
    undefined;
handle_ecc_point_fmt_extension(_) ->
    #ec_point_formats{ec_point_format_list = [?ECPOINT_UNCOMPRESSED]}.

advertises_ec_ciphers(ListKex) ->
    KeyExchanges = [ecdh_ecdsa, ecdhe_ecdsa, ecdh_rsa, ecdhe_rsa, ecdh_anon],
    F = fun (#{key_exchange := Kex}) -> lists:member(Kex, KeyExchanges);
            ({ecdhe_psk, _,_,_}) -> true
        end,
    lists:any(F, ListKex).

select_shared_curve(SharedCurves, Curves) ->
    case lists:search(fun (Curve) -> lists:member(Curve, Curves) end, SharedCurves) of
        {value, SharedCurve} -> {namedCurve, SharedCurve};
        false -> no_curve
    end.

sni(SslOpts) ->
    case maps:get(server_name_indication, SslOpts, undefined) of
        undefined -> undefined;
        disable -> undefined;
        Hostname -> #sni{hostname = Hostname}
    end.
supported_hashsigns(undefined) ->
    undefined;
supported_hashsigns([default | SigAlgs]) ->
    supported_hashsigns(SigAlgs);
supported_hashsigns(SigAlgs) ->
    ssl_cipher:signature_schemes_1_2(SigAlgs).

%% convert max_fragment_length (in bytes) to the RFC 6066 ENUM
max_frag_enum(?MAX_FRAGMENT_LENGTH_BYTES_1) ->
    #max_frag_enum{enum = 1};
max_frag_enum(?MAX_FRAGMENT_LENGTH_BYTES_2) ->
    #max_frag_enum{enum = 2};
max_frag_enum(?MAX_FRAGMENT_LENGTH_BYTES_3) ->
    #max_frag_enum{enum = 3};
max_frag_enum(?MAX_FRAGMENT_LENGTH_BYTES_4) ->
    #max_frag_enum{enum = 4};
max_frag_enum(undefined) ->
    undefined.

renegotiation_info(_, client, _, false) ->
    #renegotiation_info{renegotiated_connection = undefined};
renegotiation_info(_RecordCB, server, ConnectionStates, false) ->
    case ssl_record:current_connection_state(ConnectionStates, read) of
        #{reneg := #{secure_renegotiation := true}} ->
	    #renegotiation_info{renegotiated_connection = ?byte(0)};
	_ ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end;
renegotiation_info(_RecordCB, client, ConnectionStates, true) ->
    case ssl_record:current_connection_state(ConnectionStates, read) of
        #{reneg := #{secure_renegotiation := true, client_verify_data := Data}} ->
	    #renegotiation_info{renegotiated_connection = Data};
	_ ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end;
renegotiation_info(_RecordCB, server, ConnectionStates, true) ->
    case ssl_record:current_connection_state(ConnectionStates, read) of
        #{reneg := #{secure_renegotiation := true,
                     client_verify_data := CData,
                     server_verify_data := SData}} ->
                #renegotiation_info{renegotiated_connection = <<CData/binary, SData/binary>>};
	_ ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end.

handle_renegotiation_info(_, _RecordCB, _, #renegotiation_info{renegotiated_connection = ?byte(0)},
			  ConnectionStates, false, _, _) ->
    {ok, ssl_record:set_renegotiation_flag(true, ConnectionStates)};

handle_renegotiation_info(_, _RecordCB, server, undefined, ConnectionStates, _, _, CipherSuites) ->
    case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	true ->
	    {ok, ssl_record:set_renegotiation_flag(true, ConnectionStates)};
	false ->
	    {ok, ssl_record:set_renegotiation_flag(false, ConnectionStates)}
    end;

handle_renegotiation_info(_, _RecordCB, _, undefined, ConnectionStates, false, _, _) ->
    {ok, ssl_record:set_renegotiation_flag(false, ConnectionStates)};

handle_renegotiation_info(_, _RecordCB, client, #renegotiation_info{renegotiated_connection = ClientServerVerify},
			  ConnectionStates, true, _, _) ->
    #{reneg := ReNeg} = ssl_record:current_connection_state(ConnectionStates, read),
    #{client_verify_data := CData, server_verify_data := SData} = ReNeg,
    case <<CData/binary, SData/binary>> == ClientServerVerify of
	true ->
	    {ok, ConnectionStates};
	false ->
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, client_renegotiation))
    end;
handle_renegotiation_info(_, _RecordCB, server, #renegotiation_info{renegotiated_connection = ClientVerify},
			  ConnectionStates, true, _, CipherSuites) ->

      case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	  true ->
              throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {server_renegotiation, empty_renegotiation_info_scsv}));
	  false ->
	      case ssl_record:current_connection_state(ConnectionStates, read) of
		  #{reneg := #{client_verify_data := ClientVerify}} ->
		      {ok, ConnectionStates};
		  _ ->
                      throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, server_renegotiation))
	      end
      end;
handle_renegotiation_info(_, RecordCB, client, undefined, ConnectionStates, true, SecureRenegotation, _) ->
    handle_renegotiation_info(RecordCB, ConnectionStates, SecureRenegotation);

handle_renegotiation_info(_, RecordCB, server, undefined, ConnectionStates, true, SecureRenegotation, CipherSuites) ->
     case is_member(?TLS_EMPTY_RENEGOTIATION_INFO_SCSV, CipherSuites) of
	  true ->
             throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {server_renegotiation, empty_renegotiation_info_scsv}));
	 false ->
	     handle_renegotiation_info(RecordCB, ConnectionStates, SecureRenegotation)
     end.

handle_renegotiation_info(_RecordCB, ConnectionStates, SecureRenegotation) ->
    #{reneg := #{secure_renegotiation := SR}} = ssl_record:current_connection_state(ConnectionStates, read),
    case {SecureRenegotation, SR} of
	{_, true} ->
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, already_secure));
	{true, false} ->
	    throw(?ALERT_REC(?FATAL, ?NO_RENEGOTIATION));
	{false, false} ->
	    {ok, ConnectionStates}
    end.

cert_curve(_, _, no_suite) ->
    {no_curve, no_suite};
cert_curve(Cert, ECCCurve0, CipherSuite) ->
    case ssl_cipher_format:suite_bin_to_map(CipherSuite) of
        #{key_exchange := Kex} when Kex == ecdh_ecdsa; 
                                    Kex == ecdh_rsa ->
            OtpCert = public_key:pkix_decode_cert(Cert, otp),
            TBSCert = OtpCert#'OTPCertificate'.tbsCertificate,
            #'OTPSubjectPublicKeyInfo'{algorithm = AlgInfo} 
                = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
            {namedCurve, Oid}  = AlgInfo#'PublicKeyAlgorithm'.parameters,
            {{namedCurve, Oid}, CipherSuite};
        _ ->
            {ECCCurve0, CipherSuite}
    end.

empty_extensions() ->
     #{}.

empty_extensions(?TLS_1_3, client_hello) ->
    #{
      sni => undefined,
      %% max_frag_enum => undefined,
      %% status_request => undefined,
      elliptic_curves => undefined,
      signature_algs => undefined,
      use_srtp => undefined,
      %% heartbeat => undefined,
      alpn => undefined,
      %% signed_cert_timestamp => undefined,
      %% client_cert_type => undefined,
      %% server_cert_type => undefined,
      %% padding => undefined,
      key_share => undefined,
      pre_shared_key => undefined,
      psk_key_exchange_modes => undefined,
      %% early_data => undefined,
      cookie => undefined,
      client_hello_versions => undefined,
      certificate_authorities => undefined,
      %% post_handshake_auth => undefined,
      signature_algs_cert => undefined
     };
empty_extensions(?TLS_1_2, client_hello) ->
    Ext = empty_extensions(?TLS_1_1, client_hello),
    Ext#{signature_algs => undefined};
empty_extensions(_, client_hello) ->
    #{renegotiation_info => undefined,
      alpn => undefined,
      next_protocol_negotiation => undefined,
      srp => undefined,
      ec_point_formats => undefined,
      elliptic_curves => undefined,
      sni => undefined};
empty_extensions(?TLS_1_3, server_hello) ->
    #{server_hello_selected_version => undefined,
      key_share => undefined,
      pre_shared_key => undefined
     };
empty_extensions(?TLS_1_3, hello_retry_request) ->
    #{server_hello_selected_version => undefined,
      key_share => undefined,
      pre_shared_key => undefined, %% TODO remove!
      cookie => undefined
     };
empty_extensions(_, server_hello) ->
    #{renegotiation_info => undefined,
      alpn => undefined,
      next_protocol_negotiation => undefined,
      ec_point_formats => undefined}.

handle_log(Level, {LogLevel, ReportMap, Meta}) ->
    ssl_logger:log(Level, LogLevel, ReportMap, Meta).

%%% Recurse over possible paths until we find a valid one
%%% or run out of alternatives.
path_validate([], _, _, _, _, _, _, _, _, _, {error, {bad_cert, root_cert_expired}}) ->
    %% ROOT cert expired and no alternative ROOT certs could be found
    %% to validate this path or shorter variant of this path. So fail it
    %% with normal certificate expire reason.
    {error, {bad_cert, cert_expired}};
path_validate([], _, _, _, _, _, _, _, _, _, Error) ->
    Error;
path_validate([{TrustedCert, Path} | Rest], ServerName, Role, CertDbHandle, CertDbRef, CRLDbHandle,
              Version, SslOptions, ExtInfo, InvalidatedList, Error) ->
    CB = path_validation_cb(Version),
    case CB:path_validation(trusted_unwrap(TrustedCert), Path, ServerName,
                            Role, CertDbHandle, CertDbRef, CRLDbHandle,
                            Version, SslOptions, ExtInfo) of
        {error, {bad_cert, root_cert_expired}} = NewError ->
            NewInvalidatedList = [TrustedCert | InvalidatedList],
            Alt = ssl_certificate:find_cross_sign_root_paths(
                    Path, CertDbHandle,CertDbRef, NewInvalidatedList),
            path_validate(Alt ++ Rest, ServerName, Role, CertDbHandle,
                          CertDbRef, CRLDbHandle, Version, SslOptions,
                          ExtInfo, NewInvalidatedList, NewError);
        {error, {bad_cert, unknown_ca}} = NewError ->
            Alt = ssl_certificate:find_cross_sign_root_paths(
                    Path, CertDbHandle, CertDbRef, InvalidatedList),
            path_validate(Alt ++ Rest, ServerName, Role, CertDbHandle,
                          CertDbRef, CRLDbHandle, Version, SslOptions,
                          ExtInfo, InvalidatedList,
                          error_to_propagate(Error, NewError));
        {error, _} when Rest =/= []->
            path_validate(Rest, ServerName, Role, CertDbHandle, CertDbRef, CRLDbHandle,
                          Version, SslOptions, ExtInfo, InvalidatedList, Error);
        Result ->
            Result
    end.

trusted_unwrap(#cert{otp = TrustedCert}) ->
    TrustedCert;
trusted_unwrap(#'OTPCertificate'{} = TrustedCert) ->
    TrustedCert;
trusted_unwrap(ErrAtom) when is_atom(ErrAtom) ->
    ErrAtom.

%% Call basic path validation algorithm in public_key pre TLS-1.3
path_validation(TrustedCert, Path, ServerName, Role, CertDbHandle, CertDbRef, CRLDbHandle, Version,
                #{verify_fun := VerifyFun,
                  customize_hostname_check := CustomizeHostnameCheck,
                  crl_check := CrlCheck,
                  allow_any_ca_purpose := AllowAnyPurpose,
                  log_level := Level} = Opts,
                #{cert_ext := CertExt,
                  stapling_state := StaplingState}) ->
    SignAlgos = maps:get(signature_algs, Opts, undefined),
    SignAlgosCert = supported_cert_signs(maps:get(signature_algs_cert, Opts, undefined)),
    ValidationFunAndState =
        validation_fun_and_state(VerifyFun, #{role => Role,
                                              certdb => CertDbHandle,
                                              certdb_ref => CertDbRef,
                                              server_name => ServerName,
                                              customize_hostname_check =>
                                                  CustomizeHostnameCheck,
                                              signature_algs => SignAlgos,
                                              signature_algs_cert => SignAlgosCert,
                                              version => Version,
                                              crl_check => CrlCheck,
                                              crl_db => CRLDbHandle,
                                              cert_ext => CertExt,
                                              issuer => TrustedCert,
                                              stapling_state => StaplingState,
                                              allow_any_ca_purpose => AllowAnyPurpose,
                                              path_len => length(Path)
                                             },
                                 Path, Level),
    Options = path_validation_options(Opts, ValidationFunAndState),
    public_key:pkix_path_validation(TrustedCert, Path, Options).

error_to_propagate({error, {bad_cert, root_cert_expired}} = Error, _) ->
    Error;
error_to_propagate(_, Error) ->
    Error.

path_validation_cb(?TLS_1_3) ->
    tls_handshake_1_3;
path_validation_cb(_) ->
    ?MODULE.

supported_cert_signs(undefined) ->
    undefined;
supported_cert_signs([default|Signs]) ->
    Signs;
supported_cert_signs(Signs) ->
    Signs.

subject_altnames(#'OTPCertificate'{tbsCertificate = TBSCert} = OTPCert) ->
    Extensions = extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
    %% Fallback to CN-ids
    {_, Names} = public_key:pkix_subject_id(OTPCert),
    subject_altnames(Extensions, Names).
    
subject_altnames([], Names) ->
    Names;
subject_altnames([#'Extension'{extnID = ?'id-ce-subjectAltName',
                              extnValue = Value} | _], _) ->
    Value;
subject_altnames([#'Extension'{} | Extensions], Names) ->
    subject_altnames(Extensions, Names).

extensions_list(asn1_NOVALUE) ->
    [];
extensions_list(Extensions) ->
    Extensions.

extkey_oids_to_names([], Acc) ->
    Acc;
extkey_oids_to_names([?'id-kp-serverAuth'| Rest], Acc) ->
    extkey_oids_to_names(Rest, ["id-kp-serverAuth"| Acc]);
extkey_oids_to_names([?'id-kp-clientAuth'| Rest], Acc) ->
    extkey_oids_to_names(Rest, ["id-kp-clientAuth"| Acc]);
extkey_oids_to_names([?'id-kp-OCSPSigning'| Rest], Acc) ->
    extkey_oids_to_names(Rest, ["id-kp-OCSPSigning"| Acc]);
extkey_oids_to_names([?'id-kp-timeStamping'| Rest], Acc) ->
    extkey_oids_to_names(Rest, ["id-kp-emailProtection"| Acc]);
extkey_oids_to_names([?'id-kp-emailProtection'| Rest], Acc) ->
    extkey_oids_to_names(Rest, ["id-kp-emailProtection"| Acc]);
extkey_oids_to_names([?'id-kp-codeSigning'| Rest], Acc) ->
    extkey_oids_to_names(Rest, ["id-kp-codeSigning"| Acc]);
extkey_oids_to_names([?'anyExtendedKeyUsage'| Rest], Acc) ->
    extkey_oids_to_names(Rest, ["anyExtendedKeyUsage"| Acc]);
extkey_oids_to_names([Other| Rest], Acc) ->
    extkey_oids_to_names(Rest, [Other | Acc]).

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(csp,
             {call, {?MODULE, maybe_add_certificate_status_request,
                     [_Version, SslOpts,
                      _OcspNonce, _HelloExtensions]}},
             Stack) ->
    Stapling = maps:get(stapling, SslOpts, false),
    {io_lib:format("#1 ADD crt status request / Stapling option = ~W",
                   [Stapling, 10]), Stack}.
