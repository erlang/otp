%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2018. All Rights Reserved.
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
%% Purpose: Help funtions for handling the SSL-handshake protocol (common
%% to SSL/TLS and DTLS
%%----------------------------------------------------------------------

-module(ssl_handshake).

-include("ssl_handshake.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include("tls_handshake_1_3.hrl").
-include_lib("public_key/include/public_key.hrl").

-export_type([ssl_handshake/0, ssl_handshake_history/0,
	      public_key_info/0, oid/0]).

-type oid()               :: tuple().
-type public_key_params() :: #'Dss-Parms'{} |  {namedCurve, oid()} | #'ECParameters'{} | term().
-type public_key_info()   :: {oid(), #'RSAPublicKey'{} | integer() | #'ECPoint'{}, public_key_params()}.
-type ssl_handshake_history() :: {iodata(), iodata()}.

-type ssl_handshake() :: #server_hello{} | #server_hello_done{} | #certificate{} | #certificate_request{} |
			 #client_key_exchange{} | #finished{} | #certificate_verify{} |
			 #hello_request{} | #next_protocol{}.

%% Create handshake messages
-export([hello_request/0, server_hello/4, server_hello_done/0,
	 certificate/4,  client_certificate_verify/6,  certificate_request/5, key_exchange/3,
	 finished/5,  next_protocol/1]).

%% Handle handshake messages
-export([certify/7, certificate_verify/6, verify_signature/5,
	 master_secret/4, server_key_exchange_hash/2, verify_connection/6,
	 init_handshake_history/0, update_handshake_history/2, verify_server_key/5,
         select_version/3, select_supported_version/2, extension_value/1
	]).

%% Encode
-export([encode_handshake/2, encode_hello_extensions/2, encode_extensions/1, encode_extensions/2,
	 encode_client_protocol_negotiation/2, encode_protocols_advertised_on_server/1]).
%% Decode
-export([decode_handshake/3, decode_vector/1, decode_hello_extensions/4, decode_extensions/3,
	 decode_server_key/3, decode_client_key/3,
	 decode_suites/2
	]).

%% Cipher suites handling
-export([available_suites/2, available_signature_algs/2,  available_signature_algs/4, 
         cipher_suites/3, prf/6, select_session/11, supported_ecc/1,
         premaster_secret/2, premaster_secret/3, premaster_secret/4]).

%% Extensions handling
-export([client_hello_extensions/6,
	 handle_client_hello_extensions/9, %% Returns server hello extensions
	 handle_server_hello_extensions/9, select_curve/2, select_curve/3,
         select_hashsign/4, select_hashsign/5,
	 select_hashsign_algs/3, empty_extensions/2, add_server_share/3,
	 add_alpn/2, add_selected_version/1, decode_alpn/1
	]).

-export([get_cert_params/1,
         server_name/3,
         validation_fun_and_state/9,
         handle_path_validation_error/7]).

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
                  compression_method =
		  SecParams#security_parameters.compression_algorithm,
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
-spec certificate(der_cert(), db_handle(), certdb_ref(), client | server) -> #certificate{} | #alert{}.
%%
%% Description: Creates a certificate message.
%%--------------------------------------------------------------------
certificate(OwnCert, CertDbHandle, CertDbRef, client) ->
    Chain =
	case ssl_certificate:certificate_chain(OwnCert, CertDbHandle, CertDbRef) of
	    {ok, _,  CertChain} ->
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
	{ok, _, Chain} ->
	    #certificate{asn1_certificates = Chain};
	{error, Error} ->
            ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, {server_has_no_suitable_certificates, Error})
    end.

%%--------------------------------------------------------------------
-spec client_certificate_verify(undefined | der_cert(), binary(),
				ssl_record:ssl_version(), term(), public_key:private_key(),
				ssl_handshake_history()) ->
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
            ?ALERT_REC(?FATAL, ?UNSUPPORTED_CERTIFICATE, fixed_diffie_hellman_prohibited);
	false ->
	    Hashes =
		calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
	    Signed = digitally_signed(Version, Hashes, HashAlgo, PrivateKey),
	    #certificate_verify{signature = Signed, hashsign_algorithm = {HashAlgo, SignAlgo}}
    end.

%%--------------------------------------------------------------------
-spec certificate_request(ssl_cipher_format:cipher_suite(), db_handle(), 
			  certdb_ref(),  #hash_sign_algos{}, ssl_record:ssl_version()) ->
				 #certificate_request{}.
%%
%% Description: Creates a certificate_request message, called by the server.
%%--------------------------------------------------------------------
certificate_request(CipherSuite, CertDbHandle, CertDbRef, HashSigns, Version) ->
    Types = certificate_types(ssl_cipher_format:suite_bin_to_map(CipherSuite), Version),
    Authorities = certificate_authorities(CertDbHandle, CertDbRef),
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
    ServerDHParams = #server_dh_params{dh_p = int_to_bin(P),
				       dh_g = int_to_bin(G), dh_y = PublicKey},
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
      dh_params = #server_dh_params{dh_p = int_to_bin(P),
				    dh_g = int_to_bin(G), dh_y = PublicKey}
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
-spec certify(#certificate{}, db_handle(), certdb_ref(), ssl_options(), term(),
	      client | server, inet:hostname() | inet:ip_address()) ->  {der_cert(), public_key_info()} | #alert{}.
%%
%% Description: Handles a certificate handshake message
%%--------------------------------------------------------------------
certify(#certificate{asn1_certificates = ASN1Certs}, CertDbHandle, CertDbRef,
        #{server_name_indication := ServerNameIndication,
         partial_chain := PartialChain,
         verify_fun := VerifyFun,
         customize_hostname_check := CustomizeHostnameCheck,
         crl_check := CrlCheck,
         depth := Depth} = Opts, CRLDbHandle, Role, Host) ->

    ServerName = server_name(ServerNameIndication, Host, Role),
    [PeerCert | ChainCerts ] = ASN1Certs,       
    try
	{TrustedCert, CertPath}  =
	    ssl_certificate:trusted_cert_and_path(ASN1Certs, CertDbHandle, CertDbRef,  
                                                  PartialChain),
        ValidationFunAndState = validation_fun_and_state(VerifyFun, Role,
                                                         CertDbHandle, CertDbRef, ServerName,
                                                         CustomizeHostnameCheck,
                                                         CrlCheck, CRLDbHandle, CertPath),
        Options = [{max_path_length, Depth},
                   {verify_fun, ValidationFunAndState}],
	case public_key:pkix_path_validation(TrustedCert, CertPath, Options) of
	    {ok, {PublicKeyInfo,_}} ->
		{PeerCert, PublicKeyInfo};
	    {error, Reason} ->
		handle_path_validation_error(Reason, PeerCert, ChainCerts, Opts, Options, 
                                             CertDbHandle, CertDbRef)
	end
    catch
	error:{badmatch,{error, {asn1, Asn1Reason}}} ->
	    %% ASN-1 decode of certificate somehow failed
            ?ALERT_REC(?FATAL, ?CERTIFICATE_UNKNOWN, {failed_to_decode_certificate, Asn1Reason});
        error:OtherReason ->
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
    Hash = calc_certificate_verify(Version, HashAlgo, MasterSecret, Handshake),
    case verify_signature(Version, Hash, HashSign, Signature, PublicKeyInfo) of
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
    public_key:verify({digest, Hash}, HashAlgo, Signature, {PublicKey, PublicKeyParams});
verify_signature(_, Hash, {HashAlgo, _SignAlg}, Signature,
		 {?'id-ecPublicKey', PublicKey, PublicKeyParams}) ->
    public_key:verify({digest, Hash}, HashAlgo, Signature, {PublicKey, PublicKeyParams}).

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
	exit:_ ->
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
	exit:_ ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, master_secret_calculation_failure)
    end.

%%--------------------------------------------------------------------
-spec server_key_exchange_hash(md5sha | md5 | sha | sha224 |sha256 | sha384 | sha512, binary()) -> binary().
%%
%% Description: Calculate server key exchange hash
%%--------------------------------------------------------------------
server_key_exchange_hash(md5sha, Value) ->
    MD5 = crypto:hash(md5, Value),
    SHA = crypto:hash(sha, Value),
    <<MD5/binary, SHA/binary>>;

server_key_exchange_hash(Hash, Value) ->
    crypto:hash(Hash, Value).

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
select_supported_version([], _ServerVersions) ->
    undefined;
select_supported_version([ClientVersion|T], ServerVersions) ->
    case lists:member(ClientVersion, ServerVersions) of
        true ->
            ClientVersion;
        false ->
            select_supported_version(T, ServerVersions)
    end.


%%====================================================================
%% Encode handshake 
%%====================================================================

encode_handshake(#next_protocol{selected_protocol = SelectedProtocol}, _Version) ->
    PaddingLength = 32 - ((byte_size(SelectedProtocol) + 2) rem 32),
    {?NEXT_PROTOCOL, <<?BYTE((byte_size(SelectedProtocol))), SelectedProtocol/binary,
                         ?BYTE(PaddingLength), 0:(PaddingLength * 8)>>};
encode_handshake(#server_hello{server_version = {Major, Minor} = Version,
			       random = Random,
			       session_id = Session_ID,
			       cipher_suite = CipherSuite,
			       compression_method = Comp_method,
			       extensions = Extensions}, _Version) ->
			SID_length = byte_size(Session_ID),
    ExtensionsBin = encode_hello_extensions(Extensions, Version),
    {?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SID_length), Session_ID/binary,
                     CipherSuite/binary, ?BYTE(Comp_method), ExtensionsBin/binary>>};
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
encode_handshake(#certificate_request{certificate_types = CertTypes,
			    certificate_authorities = CertAuths},
       _Version) ->
    CertTypesLen = byte_size(CertTypes),
    CertAuthsLen = byte_size(CertAuths),
    {?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes/binary,
	?UINT16(CertAuthsLen), CertAuths/binary>>
    };
encode_handshake(#server_hello_done{}, _Version) ->
    {?SERVER_HELLO_DONE, <<>>};
encode_handshake(#client_key_exchange{exchange_keys = ExchangeKeys}, Version) ->
    {?CLIENT_KEY_EXCHANGE, encode_client_key(ExchangeKeys, Version)};
encode_handshake(#certificate_verify{signature = BinSig, hashsign_algorithm = HashSign}, Version) ->
    EncSig = enc_sign(HashSign, BinSig, Version),
    {?CERTIFICATE_VERIFY, EncSig};
encode_handshake(#finished{verify_data = VerifyData}, _Version) ->
    {?FINISHED, VerifyData}.

encode_hello_extensions(_, {3, 0}) ->
    <<>>;
encode_hello_extensions(Extensions, _) ->
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
    SignAlgoList = << <<(ssl_cipher:hash_algorithm(Hash)):8, (ssl_cipher:sign_algorithm(Sign)):8>> ||
		       {Hash, Sign} <- HashSignAlgos >>,
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
encode_extensions([#pre_shared_key_client_hello{
                      offered_psks = #offered_psks{
                                        identities = Identities0,
                                        binders = Binders0} = PSKs} | Rest], Acc) ->
    Identities = encode_psk_identities(Identities0),
    Binders = encode_psk_binders(Binders0),
    Len = byte_size(Identities) + byte_size(Binders),
    encode_extensions(Rest, <<?UINT16(?PRE_SHARED_KEY_EXT),
                              ?UINT16(Len), Identities/binary, Binders/binary, Acc/binary>>);
encode_extensions([#pre_shared_key_server_hello{selected_identity = Identity} | Rest], Acc) ->
    encode_extensions(Rest, <<?UINT16(?PRE_SHARED_KEY_EXT),
                              ?UINT16(2), ?UINT16(Identity), Acc/binary>>).


encode_client_protocol_negotiation(undefined, _) ->
    undefined;
encode_client_protocol_negotiation(_, false) ->
	#next_protocol_negotiation{extension_data = <<>>};
encode_client_protocol_negotiation(_, _) ->
	undefined.

encode_protocols_advertised_on_server(undefined) ->
	undefined;

encode_protocols_advertised_on_server(Protocols) ->
	#next_protocol_negotiation{extension_data = lists:foldl(fun encode_protocol/2, <<>>, Protocols)}.

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
					    Cipher_suite:2/binary, ?BYTE(Comp_method)>>) ->
    #server_hello{
       server_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suite = Cipher_suite,
       compression_method = Comp_method,
       extensions = empty_extensions(Version, server_hello)};

decode_handshake(Version, ?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       Cipher_suite:2/binary, ?BYTE(Comp_method),
		       ?UINT16(ExtLen), Extensions:ExtLen/binary>>) ->
    HelloExtensions = decode_hello_extensions(Extensions, Version, {Major, Minor}, server_hello),

    #server_hello{
       server_version = {Major,Minor},
       random = Random,
       session_id = Session_ID,
       cipher_suite = Cipher_suite,
       compression_method = Comp_method,
       extensions = HelloExtensions};
decode_handshake(_Version, ?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>) ->
    #certificate{asn1_certificates = certs_to_list(ASN1Certs)};
decode_handshake(_Version, ?SERVER_KEY_EXCHANGE, Keys) ->
    #server_key_exchange{exchange_keys = Keys};
decode_handshake({Major, Minor}, ?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(HashSignsLen), HashSigns:HashSignsLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>)
  when Major >= 3, Minor >= 3 ->
    HashSignAlgos = [{ssl_cipher:hash_algorithm(Hash), ssl_cipher:sign_algorithm(Sign)} ||
			<<?BYTE(Hash), ?BYTE(Sign)>> <= HashSigns],
    #certificate_request{certificate_types = CertTypes,
			 hashsign_algorithms = #hash_sign_algos{hash_sign_algos = HashSignAlgos},
			 certificate_authorities = CertAuths};
decode_handshake(_Version, ?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>) ->
    #certificate_request{certificate_types = CertTypes,
			 certificate_authorities = CertAuths};
decode_handshake(_Version, ?SERVER_HELLO_DONE, <<>>) ->
    #server_hello_done{};
decode_handshake({Major, Minor}, ?CERTIFICATE_VERIFY,<<HashSign:2/binary, ?UINT16(SignLen),
						       Signature:SignLen/binary>>)
  when Major == 3, Minor >= 3 ->
    #certificate_verify{hashsign_algorithm = dec_hashsign(HashSign), signature = Signature};
decode_handshake(_Version, ?CERTIFICATE_VERIFY,<<?UINT16(SignLen), Signature:SignLen/binary>>)->
    #certificate_verify{signature = Signature};
decode_handshake(_Version, ?CLIENT_KEY_EXCHANGE, PKEPMS) ->
    #client_key_exchange{exchange_keys = PKEPMS};
decode_handshake(_Version, ?FINISHED, VerifyData) ->
    #finished{verify_data = VerifyData};
decode_handshake(_, Message, _) ->
    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, {unknown_or_malformed_handshake, Message})).


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
    filter_hashsigns(Suites, [ssl_cipher_format:suite_bin_to_map(Suite) || Suite <- Suites], HashSigns, 
                     Version, []).

available_signature_algs(undefined, _)  ->
    undefined;
available_signature_algs(SupportedHashSigns, Version) when Version >= {3, 3} ->
    #hash_sign_algos{hash_sign_algos = SupportedHashSigns};
available_signature_algs(_, _) ->
    undefined.
available_signature_algs(undefined, SupportedHashSigns, _, Version) when 
      Version >= {3,3} ->
    SupportedHashSigns;
available_signature_algs(#hash_sign_algos{hash_sign_algos = ClientHashSigns}, SupportedHashSigns, 
                         _, Version) when Version >= {3,3} ->
    sets:to_list(sets:intersection(sets:from_list(ClientHashSigns), 
				   sets:from_list(SupportedHashSigns)));
available_signature_algs(_, _, _, _) -> 
    undefined.

cipher_suites(Suites, Renegotiation, true) ->
    %% TLS_FALLBACK_SCSV should be placed last -RFC7507
    cipher_suites(Suites, Renegotiation) ++ [?TLS_FALLBACK_SCSV];
cipher_suites(Suites, Renegotiation, false) ->
    cipher_suites(Suites, Renegotiation).
cipher_suites(Suites, false) ->
    [?TLS_EMPTY_RENEGOTIATION_INFO_SCSV | Suites];
cipher_suites(Suites, true) ->
    Suites.
%%--------------------------------------------------------------------
-spec prf(ssl_record:ssl_version(), non_neg_integer(), binary(), binary(), [binary()], non_neg_integer()) ->
		 {ok, binary()} | {error, undefined}.
%%
%% Description: use the TLS PRF to generate key material
%%--------------------------------------------------------------------
prf({3,0}, _, _, _, _, _) ->
    {error, undefined};
prf({3,_N}, PRFAlgo, Secret, Label, Seed, WantedLength) ->
    {ok, tls_v1:prf(PRFAlgo, Secret, Label, Seed, WantedLength)}.

select_session(SuggestedSessionId, CipherSuites, HashSigns, Compressions, Port, #session{ecc = ECCCurve0} = 
		   Session, Version,
	       #{ciphers := UserSuites, honor_cipher_order := HonorCipherOrder} = SslOpts,
	       Cache, CacheCb, Cert) ->
    {SessionId, Resumed} = ssl_session:server_id(Port, SuggestedSessionId,
						 SslOpts, Cert,
						 Cache, CacheCb),
    case Resumed of
        undefined ->
	    Suites = available_suites(Cert, UserSuites, Version, HashSigns, ECCCurve0),
	    CipherSuite0 = select_cipher_suite(CipherSuites, Suites, HonorCipherOrder),
            {ECCCurve, CipherSuite} = cert_curve(Cert, ECCCurve0, CipherSuite0),
	    Compression = select_compression(Compressions),
	    {new, Session#session{session_id = SessionId,
                                  ecc = ECCCurve,
				  cipher_suite = CipherSuite,
				  compression_method = Compression}};
	_ ->
	    {resumed, Resumed}
    end.

supported_ecc({Major, Minor}) when ((Major == 3) and (Minor >= 1)) orelse (Major > 3) ->
    Curves = tls_v1:ecc_curves(Minor),
    #elliptic_curves{elliptic_curve_list = Curves};
supported_ecc(_) ->
    #elliptic_curves{elliptic_curve_list = []}.

premaster_secret(OtherPublicDhKey, MyPrivateKey, #'DHParameter'{} = Params) ->
    try 
	public_key:compute_key(OtherPublicDhKey, MyPrivateKey, Params)  
    catch 
	error:computation_failed -> 
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;	   
premaster_secret(PublicDhKey, PrivateDhKey, #server_dh_params{dh_p = Prime, dh_g = Base}) ->
    try 
	crypto:compute_key(dh, PublicDhKey, PrivateDhKey, [Prime, Base])
    catch 
	error:computation_failed -> 
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;
premaster_secret(#client_srp_public{srp_a = ClientPublicKey}, ServerKey, #srp_user{prime = Prime,
										   verifier = Verifier}) ->
    try crypto:compute_key(srp, ClientPublicKey, ServerKey, {host, [Verifier, Prime, '6a']}) of
	PremasterSecret ->
	    PremasterSecret
    catch
	error:_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end;
premaster_secret(#server_srp_params{srp_n = Prime, srp_g = Generator, srp_s = Salt, srp_b = Public},
		 ClientKeys, {Username, Password}) ->
    case ssl_srp_primes:check_srp_params(Generator, Prime) of
	ok ->
	    DerivedKey = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
	    try crypto:compute_key(srp, Public, ClientKeys, {user, [DerivedKey, Prime, Generator, '6a']}) of
		PremasterSecret ->
		    PremasterSecret
            catch
		error ->
		    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
	    end;
	_ ->
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
	_:_ ->
	    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
    end;
premaster_secret(EncSecret, #{algorithm := rsa} = Engine) ->
    try crypto:private_decrypt(rsa, EncSecret, maps:remove(algorithm, Engine),
				   [{rsa_pad, rsa_pkcs1_padding}])
    catch
	_:_ ->
	    throw(?ALERT_REC(?FATAL, ?DECRYPT_ERROR))
    end.
%%====================================================================
%% Extensions handling
%%====================================================================
client_hello_extensions(Version, CipherSuites, SslOpts, ConnectionStates, Renegotiation, KeyShare) ->
    HelloExtensions0 = add_tls12_extensions(Version, SslOpts, ConnectionStates, Renegotiation),
    HelloExtensions1 = add_common_extensions(Version, HelloExtensions0, CipherSuites, SslOpts),
    maybe_add_tls13_extensions(Version, HelloExtensions1, SslOpts, KeyShare).


add_tls12_extensions(_Version,
                     #{alpn_advertised_protocols := AlpnAdvertisedProtocols,
                      next_protocol_selector := NextProtocolSelector,
                      server_name_indication := ServerNameIndication} = SslOpts,
                     ConnectionStates,
                     Renegotiation) ->
    SRP = srp_user(SslOpts),
    #{renegotiation_info => renegotiation_info(tls_record, client,
                                               ConnectionStates, Renegotiation),
      srp => SRP,
      alpn => encode_alpn(AlpnAdvertisedProtocols, Renegotiation),
      next_protocol_negotiation =>
          encode_client_protocol_negotiation(NextProtocolSelector,
                                             Renegotiation),
      sni => sni(ServerNameIndication)
     }.


add_common_extensions({3,4},
                      HelloExtensions,
                      _CipherSuites,
                      #{eccs := SupportedECCs,
                        supported_groups := Groups,
                        signature_algs := SignatureSchemes}) ->
    {EcPointFormats, _} =
        client_ecc_extensions(SupportedECCs),
    HelloExtensions#{ec_point_formats => EcPointFormats,
                     elliptic_curves => Groups,
                     signature_algs => signature_algs_ext(SignatureSchemes)};

add_common_extensions(Version,
                      HelloExtensions,
                      CipherSuites,
                      #{eccs := SupportedECCs,
                        signature_algs := SupportedHashSigns}) ->

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
                     signature_algs => available_signature_algs(SupportedHashSigns, Version)}.


maybe_add_tls13_extensions({3,4},
                           HelloExtensions0,
                           #{signature_algs_cert := SignatureSchemes,
                                        versions := SupportedVersions},
                           KeyShare) ->
    HelloExtensions =
        HelloExtensions0#{client_hello_versions =>
                              #client_hello_versions{versions = SupportedVersions},
                          signature_algs_cert =>
                              signature_algs_cert(SignatureSchemes)},
    maybe_add_key_share(HelloExtensions, KeyShare);
maybe_add_tls13_extensions(_, HelloExtensions, _, _) ->
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
    SupportedVersions = #server_hello_selected_version{selected_version = {3,4}},
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
signature_algs_cert(SignatureSchemes) ->
    #signature_algorithms_cert{signature_scheme_list = SignatureSchemes}.

handle_client_hello_extensions(RecordCB, Random, ClientCipherSuites,
                               Exts, Version,
			       #{secure_renegotiate := SecureRenegotation,
                                 alpn_preferred_protocols := ALPNPreferredProtocols} = Opts,
			       #session{cipher_suite = NegotiatedCipherSuite,
					compression_method = Compression} = Session0,
			       ConnectionStates0, Renegotiation) ->
    Session = handle_srp_extension(maps:get(srp, Exts, undefined), Session0),
    ConnectionStates = handle_renegotiation_extension(server, RecordCB, Version, maps:get(renegotiation_info, Exts, undefined),
						      Random, NegotiatedCipherSuite, 
						      ClientCipherSuites, Compression,
						      ConnectionStates0, Renegotiation, SecureRenegotation),

    Empty = empty_extensions(Version, server_hello),
    ServerHelloExtensions = Empty#{renegotiation_info => renegotiation_info(RecordCB, server,
                                                                            ConnectionStates, Renegotiation),
                                   ec_point_formats => server_ecc_extension(Version, 
                                                                            maps:get(ec_point_formats, Exts, undefined))
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

handle_server_hello_extensions(RecordCB, Random, CipherSuite, Compression,
                               Exts, Version,
			       #{secure_renegotiate := SecureRenegotation,
                                 next_protocol_selector := NextProtoSelector},
			       ConnectionStates0, Renegotiation) ->
    ConnectionStates = handle_renegotiation_extension(client, RecordCB, Version,  
                                                      maps:get(renegotiation_info, Exts, undefined), Random, 
						      CipherSuite, undefined,
						      Compression, ConnectionStates0,
						      Renegotiation, SecureRenegotation),

    %% If we receive an ALPN extension then this is the protocol selected,
    %% otherwise handle the NPN extension.
    ALPN = maps:get(alpn, Exts, undefined),
    case decode_alpn(ALPN) of
        %% ServerHello contains exactly one protocol: the one selected.
        %% We also ignore the ALPN extension during renegotiation (see encode_alpn/2).
        [Protocol] when not Renegotiation ->
            {ConnectionStates, alpn, Protocol};
        [_] when Renegotiation ->
            {ConnectionStates, alpn, undefined};
        undefined ->
            NextProtocolNegotiation = maps:get(next_protocol_negotiation, Exts, undefined),
            Protocol = handle_next_protocol(NextProtocolNegotiation, NextProtoSelector, Renegotiation),
            {ConnectionStates, npn, Protocol};
        {error, Reason} ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason);
        [] ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, no_protocols_in_server_hello);
        [_|_] ->
            ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, too_many_protocols_in_server_hello)
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
    {namedCurve, ?secp256r1}.

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
                Cert, KeyExAlgo, undefined, {Major, Minor} = Version)
  when Major >= 3 andalso Minor >= 3->
    select_hashsign({ClientHashSigns, ClientSignatureSchemes}, Cert, KeyExAlgo,
                    tls_v1:default_signature_algs(Version), Version);
select_hashsign({#hash_sign_algos{hash_sign_algos = ClientHashSigns},
                 ClientSignatureSchemes0},
                Cert, KeyExAlgo, SupportedHashSigns, {Major, Minor})
  when Major >= 3 andalso Minor >= 3 ->
    ClientSignatureSchemes = get_signature_scheme(ClientSignatureSchemes0),
    {SignAlgo0, Param, PublicKeyAlgo0} = get_cert_params(Cert),
    SignAlgo = sign_algo(SignAlgo0),
    PublicKeyAlgo = public_key_algo(PublicKeyAlgo0),

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
    case is_supported_sign(SignAlgo, Param, ClientHashSigns, ClientSignatureSchemes) of
        true ->
            case lists:filter(fun({_, S} = Algos) when S == PublicKeyAlgo ->
                                      is_acceptable_hash_sign(Algos, KeyExAlgo, SupportedHashSigns);
                                 (_)  ->
                                      false
                              end, ClientHashSigns) of
                [] ->
                    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm);
                [HashSign | _] ->
                    HashSign
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
                SupportedHashSigns,
		{Major, Minor})  when Major >= 3 andalso Minor >= 3->
    {SignAlgo0, Param, PublicKeyAlgo0} = get_cert_params(Cert),
    SignAlgo = sign_algo(SignAlgo0),
    PublicKeyAlgo = public_key_algo(PublicKeyAlgo0),

    case is_acceptable_cert_type(PublicKeyAlgo, Types) andalso
        %% certificate_request has no "signature_algorithms_cert"
        %% extension in TLS 1.2.
        is_supported_sign(SignAlgo, Param, HashSigns, undefined) of
	true ->
	    case lists:filter(fun({_, S} = Algos) when S == PublicKeyAlgo ->
				 is_acceptable_hash_sign(Algos, SupportedHashSigns);
			    (_)  ->
				      false
			      end, HashSigns) of
		[] ->
		    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm);
		[HashSign | _] ->
		    HashSign
	    end;
	false ->
	    ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)
    end;
select_hashsign(#certificate_request{certificate_types = Types}, Cert, _, Version) ->
    {_, _, PublicKeyAlgo0} = get_cert_params(Cert),
    PublicKeyAlgo = public_key_algo(PublicKeyAlgo0),

    %% Check cert even for TLS 1.0/1.1
    case is_acceptable_cert_type(PublicKeyAlgo, Types) of
        true ->
            select_hashsign(undefined, Cert, undefined, [], Version);
        false ->
            ?ALERT_REC(?FATAL, ?INSUFFICIENT_SECURITY, no_suitable_signature_algorithm)
    end.


%% Gets the relevant parameters of a certificate:
%% - signature algorithm
%% - parameters of the signature algorithm
%% - public key algorithm (key type)
get_cert_params(Cert) ->
    #'OTPCertificate'{tbsCertificate = TBSCert,
		      signatureAlgorithm =
                          {_,SignAlgo, Param}} = public_key:pkix_decode_cert(Cert, otp),
    #'OTPSubjectPublicKeyInfo'{algorithm = {_, PublicKeyAlgo, _}} =
        TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
    {SignAlgo, Param, PublicKeyAlgo}.


get_signature_scheme(undefined) ->
    undefined;
get_signature_scheme(#signature_algorithms_cert{
                        signature_scheme_list = ClientSignatureSchemes}) ->
    ClientSignatureSchemes.


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
select_hashsign_algs(HashSign, _, {Major, Minor}) when HashSign =/= undefined andalso
						       Major >= 3 andalso Minor >= 3 ->
    HashSign;
select_hashsign_algs(undefined, ?rsaEncryption, {Major, Minor}) when Major >= 3 andalso Minor >= 3 ->
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
    SelectedVersion.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%------------- Create handshake messages ----------------------------

int_to_bin(I) ->
    L = (length(integer_to_list(I, 16)) + 1) div 2,
    <<I:(L*8)>>.

%% TLS 1.0+
%% The end-entity certificate provided by the client MUST contain a
%% key that is compatible with certificate_types.
certificate_types(_, {N, M}) when N >= 3 andalso M >= 1 ->
    ECDSA = supported_cert_type_or_empty(ecdsa, ?ECDSA_SIGN),
    RSA = supported_cert_type_or_empty(rsa, ?RSA_SIGN),
    DSS = supported_cert_type_or_empty(dss, ?DSS_SIGN),
    <<ECDSA/binary,RSA/binary,DSS/binary>>;
%% SSL 3.0
certificate_types(_, _) ->
    RSA = supported_cert_type_or_empty(rsa, ?RSA_SIGN),
    DSS = supported_cert_type_or_empty(dss, ?DSS_SIGN),
    <<RSA/binary,DSS/binary>>.

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

certificate_authorities(CertDbHandle, CertDbRef) ->
    Authorities = certificate_authorities_from_db(CertDbHandle, CertDbRef),
    Enc = fun(#'OTPCertificate'{tbsCertificate=TBSCert}) ->
		  OTPSubj = TBSCert#'OTPTBSCertificate'.subject,
		  DNEncodedBin = public_key:pkix_encode('Name', OTPSubj, otp),
		  DNEncodedLen = byte_size(DNEncodedBin),
		  <<?UINT16(DNEncodedLen), DNEncodedBin/binary>>
	  end,
    list_to_binary([Enc(Cert) || {_, Cert} <- Authorities]).

certificate_authorities_from_db(CertDbHandle, CertDbRef) when is_reference(CertDbRef) ->
    ConnectionCerts = fun({{Ref, _, _}, Cert}, Acc) when Ref  == CertDbRef ->
			      [Cert | Acc];
			 (_, Acc) ->
			      Acc
		      end,
    ssl_pkix_db:foldl(ConnectionCerts, [], CertDbHandle);
certificate_authorities_from_db(_CertDbHandle, {extracted, CertDbData}) ->
    %% Cache disabled, Ref contains data
    lists:foldl(fun({decoded, {_Key,Cert}}, Acc) -> [Cert | Acc] end,
		[], CertDbData).

%%-------------Handle handshake messages --------------------------------
validation_fun_and_state({Fun, UserState0}, Role,  CertDbHandle, CertDbRef, 
                         ServerNameIndication, CustomizeHostCheck, CRLCheck, CRLDbHandle, CertPath) ->
    {fun(OtpCert, {extension, _} = Extension, {SslState, UserState}) ->
	     case ssl_certificate:validate(OtpCert,
					   Extension,
					   SslState) of
		 {valid, NewSslState} ->
		     {valid, {NewSslState, UserState}};
		 {fail, Reason} ->
		     apply_user_fun(Fun, OtpCert, Reason, UserState,
				    SslState, CertPath);
		 {unknown, _} ->
		     apply_user_fun(Fun, OtpCert,
				    Extension, UserState, SslState, CertPath)
	     end;
	(OtpCert, VerifyResult, {SslState, UserState}) ->
	     apply_user_fun(Fun, OtpCert, VerifyResult, UserState,
			    SslState, CertPath)
     end, {{Role, CertDbHandle, CertDbRef, {ServerNameIndication, CustomizeHostCheck}, CRLCheck, CRLDbHandle}, UserState0}};
validation_fun_and_state(undefined, Role, CertDbHandle, CertDbRef, 
                         ServerNameIndication, CustomizeHostCheck, CRLCheck, CRLDbHandle, CertPath) ->
    {fun(OtpCert, {extension, _} = Extension, SslState) ->
	     ssl_certificate:validate(OtpCert,
				      Extension,
				      SslState);
	(OtpCert, VerifyResult, SslState) when (VerifyResult == valid) or 
                                               (VerifyResult == valid_peer) -> 
	     case crl_check(OtpCert, CRLCheck, CertDbHandle, CertDbRef, 
                            CRLDbHandle, VerifyResult, CertPath) of
		 valid ->                     
                     ssl_certificate:validate(OtpCert,
                                              VerifyResult,
                                              SslState);
		 Reason ->
		     {fail, Reason}
	     end;
	(OtpCert, VerifyResult, SslState) ->
	     ssl_certificate:validate(OtpCert,
				      VerifyResult,
				      SslState)
     end, {Role, CertDbHandle, CertDbRef, {ServerNameIndication, CustomizeHostCheck}, CRLCheck, CRLDbHandle}}.

apply_user_fun(Fun, OtpCert, VerifyResult, UserState0, 
	       {_, CertDbHandle, CertDbRef, _, CRLCheck, CRLDbHandle} = SslState, CertPath) when
      (VerifyResult == valid) or (VerifyResult == valid_peer) ->
    case Fun(OtpCert, VerifyResult, UserState0) of
	{Valid, UserState} when (Valid == valid) or (Valid == valid_peer) ->
	    case crl_check(OtpCert, CRLCheck, CertDbHandle, CertDbRef, 
                           CRLDbHandle, VerifyResult, CertPath) of
		valid ->
		    {Valid, {SslState, UserState}};
		Result ->
		    apply_user_fun(Fun, OtpCert, Result, UserState, SslState, CertPath)
	    end;
	{fail, _} = Fail ->
	    Fail
    end;
apply_user_fun(Fun, OtpCert, ExtensionOrError, UserState0, SslState, _CertPath) ->
    case Fun(OtpCert, ExtensionOrError, UserState0) of
	{Valid, UserState} when (Valid == valid) or (Valid == valid_peer)->
	    {Valid, {SslState, UserState}};
	{fail, _} = Fail ->
	    Fail;
	{unknown, UserState} ->
	    {unknown, {SslState, UserState}}
    end.

handle_path_validation_error({bad_cert, unknown_ca} = Reason, PeerCert, Chain,  
                             Opts, Options, CertDbHandle, CertsDbRef) ->
    handle_incomplete_chain(PeerCert, Chain, Opts, Options, CertDbHandle, CertsDbRef, Reason);
handle_path_validation_error({bad_cert, invalid_issuer} = Reason, PeerCert, Chain0, 
			     #{partial_chain := PartialChain} = Opts, Options, CertDbHandle, CertsDbRef) ->
    case ssl_certificate:certificate_chain(PeerCert, CertDbHandle, CertsDbRef, Chain0) of
	{ok, _, [PeerCert | Chain] = OrdedChain} when  Chain =/= Chain0 -> %% Chain appaears to be unorded 
            {Trusted, Path} = ssl_certificate:trusted_cert_and_path(OrdedChain,
                                                                    CertDbHandle, CertsDbRef,
                                                                    PartialChain),
            case public_key:pkix_path_validation(Trusted, Path, Options) of
		{ok, {PublicKeyInfo,_}} ->
		    {PeerCert, PublicKeyInfo};
                {error, PathError} ->
		    handle_path_validation_error(PathError, PeerCert, Path,
                                                 Opts, Options, CertDbHandle, CertsDbRef)
	    end;
        _ ->
            path_validation_alert(Reason)
    end;
handle_path_validation_error(Reason, _, _, _, _,_, _) ->
    path_validation_alert(Reason).

handle_incomplete_chain(PeerCert, Chain0,
                        #{partial_chain := PartialChain}, Options, CertDbHandle, CertsDbRef, PathError0) ->
    case ssl_certificate:certificate_chain(PeerCert, CertDbHandle, CertsDbRef) of
        {ok, _, [PeerCert | _] = Chain} when Chain =/= Chain0 -> %% Chain candidate found          
            {Trusted, Path} = ssl_certificate:trusted_cert_and_path(Chain,
                                                                    CertDbHandle, CertsDbRef,
                                                                    PartialChain),
            case public_key:pkix_path_validation(Trusted, Path, Options) of
		{ok, {PublicKeyInfo,_}} ->
		    {PeerCert, PublicKeyInfo};
                {error, PathError} ->
		    path_validation_alert(PathError)
	    end;
        _ ->
            path_validation_alert(PathError0)
    end.

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
path_validation_alert({bad_cert, {revoked, _}}) ->
    ?ALERT_REC(?FATAL, ?CERTIFICATE_REVOKED);
path_validation_alert({bad_cert, {revocation_status_undetermined, Details}}) ->
    Alert = ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE),
    Alert#alert{reason = Details};
path_validation_alert({bad_cert, selfsigned_peer}) ->
    ?ALERT_REC(?FATAL, ?BAD_CERTIFICATE);
path_validation_alert({bad_cert, unknown_ca}) ->
     ?ALERT_REC(?FATAL, ?UNKNOWN_CA);
path_validation_alert(Reason) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, Reason).

digitally_signed(Version, Hashes, HashAlgo, PrivateKey) ->
    try do_digitally_signed(Version, Hashes, HashAlgo, PrivateKey) of
	Signature ->
	    Signature
    catch
	error:badkey->
	    throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, bad_key(PrivateKey)))
    end.
do_digitally_signed({3, Minor}, Hash, HashAlgo, #{algorithm := Alg} = Engine) 
  when Minor >= 3 ->
    crypto:sign(Alg, HashAlgo, {digest, Hash}, maps:remove(algorithm, Engine));
do_digitally_signed({3, Minor}, Hash, HashAlgo, Key) when Minor >= 3 ->
    public_key:sign({digest, Hash}, HashAlgo, Key);
do_digitally_signed(_Version, Hash, _HashAlgo, #'RSAPrivateKey'{} = Key) ->
    public_key:encrypt_private(Hash, Key,
			       [{rsa_pad, rsa_pkcs1_padding}]);
do_digitally_signed({3, _}, Hash, _, 
                    #{algorithm := rsa} = Engine) ->
    crypto:private_encrypt(rsa, Hash, maps:remove(algorithm, Engine),
                           rsa_pkcs1_padding);
do_digitally_signed({3, _}, Hash, HashAlgo, #{algorithm := Alg} = Engine) ->
    crypto:sign(Alg, HashAlgo, {digest, Hash}, maps:remove(algorithm, Engine));
do_digitally_signed(_Version, Hash, HashAlgo, Key) ->
    public_key:sign({digest, Hash}, HashAlgo, Key).

bad_key(#'DSAPrivateKey'{}) ->
    unacceptable_dsa_key;
bad_key(#'RSAPrivateKey'{}) ->
    unacceptable_rsa_key;
bad_key(#'ECPrivateKey'{}) ->
    unacceptable_ecdsa_key.

crl_check(_, false, _,_,_, _, _) ->
    valid;
crl_check(_, peer, _, _,_, valid, _) -> %% Do not check CAs with this option.
    valid;
crl_check(OtpCert, Check, CertDbHandle, CertDbRef, {Callback, CRLDbHandle}, _, CertPath) ->
    Options = [{issuer_fun, {fun(_DP, CRL, Issuer, DBInfo) ->
				     ssl_crl:trusted_cert_and_path(CRL, Issuer, {CertPath,
                                                                                 DBInfo})
			     end, {CertDbHandle, CertDbRef}}}, 
	       {update_crl, fun(DP, CRL) -> Callback:fresh_crl(DP, CRL) end},
               {undetermined_details, true}
	      ],
    case dps_and_crls(OtpCert, Callback, CRLDbHandle, ext) of
	no_dps ->
	    crl_check_same_issuer(OtpCert, Check,
				  dps_and_crls(OtpCert, Callback, CRLDbHandle, same_issuer),
				  Options);
	DpsAndCRLs ->  %% This DP list may be empty if relevant CRLs existed 
	    %% but could not be retrived, will result in {bad_cert, revocation_status_undetermined}
	    case public_key:pkix_crls_validate(OtpCert, DpsAndCRLs, Options) of
		{bad_cert, {revocation_status_undetermined, _}} ->
		    crl_check_same_issuer(OtpCert, Check, dps_and_crls(OtpCert, Callback, 
								       CRLDbHandle, same_issuer), Options);
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

dps_and_crls(OtpCert, Callback, CRLDbHandle, ext) ->
    case public_key:pkix_dist_points(OtpCert) of
	[] ->
	    no_dps;
	DistPoints ->
	    Issuer = OtpCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.issuer,
	    CRLs = distpoints_lookup(DistPoints, Issuer, Callback, CRLDbHandle),
            dps_and_crls(DistPoints, CRLs, [])
    end;

dps_and_crls(OtpCert, Callback, CRLDbHandle, same_issuer) ->    
    DP = #'DistributionPoint'{distributionPoint = {fullName, GenNames}} = 
	public_key:pkix_dist_point(OtpCert),
    CRLs = lists:flatmap(fun({directoryName, Issuer}) -> 
				 Callback:select(Issuer, CRLDbHandle);
			    (_) ->
				 []
			 end, GenNames),
    [{DP, {CRL, public_key:der_decode('CertificateList', CRL)}} ||  CRL <- CRLs].

dps_and_crls([], _, Acc) ->
    Acc;
dps_and_crls([DP | Rest], CRLs, Acc) ->
    DpCRL = [{DP, {CRL, public_key:der_decode('CertificateList', CRL)}} ||  CRL <- CRLs],
    dps_and_crls(Rest, CRLs, DpCRL ++ Acc).
    
distpoints_lookup([],_, _, _) ->
    [];
distpoints_lookup([DistPoint | Rest], Issuer, Callback, CRLDbHandle) ->
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
	    distpoints_lookup(Rest, Issuer, Callback, CRLDbHandle);
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
        _:_->
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, premaster_encryption_failed))
    end.

calc_certificate_verify({3, 0}, HashAlgo, MasterSecret, Handshake) ->
    ssl_v3:certificate_verify(HashAlgo, MasterSecret, lists:reverse(Handshake));
calc_certificate_verify({3, N}, HashAlgo, _MasterSecret, Handshake) ->
    tls_v1:certificate_verify(HashAlgo, N, lists:reverse(Handshake)).

calc_finished({3, 0}, Role, _PrfAlgo, MasterSecret, Handshake) ->
    ssl_v3:finished(Role, MasterSecret, lists:reverse(Handshake));
calc_finished({3, N}, Role, PrfAlgo, MasterSecret, Handshake) ->
    tls_v1:finished(Role, N, PrfAlgo, MasterSecret, lists:reverse(Handshake)).

master_secret(Version, MasterSecret,
	      #security_parameters{
		 bulk_cipher_algorithm = BCA,
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

    ClientCipherState = ssl_cipher:cipher_init(BCA, ClientIV, ClientWriteKey),
    ServerCipherState = ssl_cipher:cipher_init(BCA, ServerIV, ServerWriteKey),
    {MasterSecret,
     ssl_record:set_pending_cipher_state(ConnStates2, ClientCipherState,
					 ServerCipherState, Role)}.

setup_keys({3,0}, _PrfAlgo, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, EKML, IVS) ->
    ssl_v3:setup_keys(MasterSecret, ServerRandom,
			ClientRandom, HashSize, KML, EKML, IVS);

setup_keys({3,N}, PrfAlgo, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, _EKML, IVS) ->
    tls_v1:setup_keys(N, PrfAlgo, MasterSecret, ServerRandom, ClientRandom, HashSize,
			KML, IVS).

calc_master_secret({3,0}, _PrfAlgo, PremasterSecret, ClientRandom, ServerRandom) ->
    ssl_v3:master_secret(PremasterSecret, ClientRandom, ServerRandom);

calc_master_secret({3,_}, PrfAlgo, PremasterSecret, ClientRandom, ServerRandom) ->
    tls_v1:master_secret(PrfAlgo, PremasterSecret, ClientRandom, ServerRandom).
	
%% Update pending connection states with parameters exchanged via
%% hello messages
%% NOTE : Role is the role of the receiver of the hello message
%%        currently being processed.
hello_pending_connection_states(_RecordCB, Role, Version, CipherSuite, Random, Compression,
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

    ssl_record:set_security_params(NewReadSecParams,
				    NewWriteSecParams,
				    ConnectionStates).

hello_security_parameters(client, Version, #{security_parameters := SecParams}, CipherSuite, Random,
			  Compression) ->
    NewSecParams = ssl_cipher:security_parameters(Version, CipherSuite, SecParams),
    NewSecParams#security_parameters{
      server_random = Random,
      compression_algorithm = Compression
     };

hello_security_parameters(server, Version, #{security_parameters := SecParams}, CipherSuite, Random,
			  Compression) ->
    NewSecParams = ssl_cipher:security_parameters(Version, CipherSuite, SecParams),
    NewSecParams#security_parameters{
      client_random = Random,
      compression_algorithm = Compression
     }.

select_compression(_CompressionMetodes) ->
    ?NULL.

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
    KLen = size(ECPubKey),
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
    KLen = size(ECPubKey),
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

encode_client_key(#encrypted_premaster_secret{premaster_secret = PKEPMS},{3, 0}) ->
    PKEPMS;
encode_client_key(#encrypted_premaster_secret{premaster_secret = PKEPMS}, _) ->
    PKEPMSLen = byte_size(PKEPMS),
    <<?UINT16(PKEPMSLen), PKEPMS/binary>>;
encode_client_key(#client_diffie_hellman_public{dh_public = DHPublic}, _) ->
    Len = byte_size(DHPublic),
    <<?UINT16(Len), DHPublic/binary>>;
encode_client_key(#client_ec_diffie_hellman_public{dh_public = DHPublic}, _) ->
    Len = byte_size(DHPublic),
    <<?BYTE(Len), DHPublic/binary>>;
encode_client_key(#client_psk_identity{identity = undefined}, _) ->
    Id = <<"psk_identity">>,
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary>>;
encode_client_key(#client_psk_identity{identity = Id}, _) ->
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary>>;
encode_client_key(Identity = #client_dhe_psk_identity{identity = undefined}, Version) ->
    encode_client_key(Identity#client_dhe_psk_identity{identity = <<"psk_identity">>}, Version);
encode_client_key(#client_dhe_psk_identity{identity = Id, dh_public = DHPublic}, _) ->
    Len = byte_size(Id),
    DHLen = byte_size(DHPublic),
    <<?UINT16(Len), Id/binary, ?UINT16(DHLen), DHPublic/binary>>;
encode_client_key(Identity = #client_ecdhe_psk_identity{identity = undefined}, Version) ->
    encode_client_key(Identity#client_ecdhe_psk_identity{identity = <<"psk_identity">>}, Version);
encode_client_key(#client_ecdhe_psk_identity{identity = Id, dh_public = DHPublic}, _) ->
    Len = byte_size(Id),
    DHLen = byte_size(DHPublic),
    <<?UINT16(Len), Id/binary, ?BYTE(DHLen), DHPublic/binary>>;
encode_client_key(Identity = #client_rsa_psk_identity{identity = undefined}, Version) ->
    encode_client_key(Identity#client_rsa_psk_identity{identity = <<"psk_identity">>}, Version);
encode_client_key(#client_rsa_psk_identity{identity = Id, exchange_keys = ExchangeKeys}, Version) ->
    EncPMS = encode_client_key(ExchangeKeys, Version),
    Len = byte_size(Id),
    <<?UINT16(Len), Id/binary, EncPMS/binary>>;
encode_client_key(#client_srp_public{srp_a = A}, _) ->
    Len = byte_size(A),
    <<?UINT16(Len), A/binary>>.

enc_sign({_, anon}, _Sign, _Version) ->
    <<>>;
enc_sign({HashAlg, SignAlg}, Signature, _Version = {Major, Minor})
  when Major == 3, Minor >= 3->
	SignLen = byte_size(Signature),
	HashSign = enc_hashsign(HashAlg, SignAlg),
	<<HashSign/binary, ?UINT16(SignLen), Signature/binary>>;
enc_sign(_HashSign, Sign, _Version) ->
	SignLen = byte_size(Sign),
	<<?UINT16(SignLen), Sign/binary>>.

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
	    Signature = digitally_signed(Version, Hash, HashAlgo, PrivateKey),
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
    encode_versions(lists:reverse(Versions), <<>>).
%%
encode_versions([], Acc) ->
    Acc;
encode_versions([{M,N}|T], Acc) ->
    encode_versions(T, <<?BYTE(M),?BYTE(N),Acc/binary>>).

encode_client_shares(ClientShares) ->
    encode_client_shares(ClientShares, <<>>).
%%
encode_client_shares([], Acc) ->
    Acc;
encode_client_shares([KeyShareEntry0|T], Acc) ->
    KeyShareEntry = encode_key_share_entry(KeyShareEntry0),
    encode_client_shares(T, <<Acc/binary,KeyShareEntry/binary>>).

encode_key_share_entry(#key_share_entry{
                          group = Group,
                          key_exchange = KeyExchange}) ->
    Len = byte_size(KeyExchange),
    <<?UINT16((tls_v1:group_to_enum(Group))),?UINT16(Len),KeyExchange/binary>>.

encode_psk_key_exchange_modes(KEModes) ->
    encode_psk_key_exchange_modes(lists:reverse(KEModes), <<>>).
%%
encode_psk_key_exchange_modes([], Acc) ->
    Acc;
encode_psk_key_exchange_modes([psk_ke|T], Acc) ->
    encode_psk_key_exchange_modes(T, <<?BYTE(?PSK_KE),Acc/binary>>);
encode_psk_key_exchange_modes([psk_dhe_ke|T], Acc) ->
    encode_psk_key_exchange_modes(T, <<?BYTE(?PSK_DHE_KE),Acc/binary>>).


encode_psk_identities(Identities) ->
    encode_psk_identities(Identities, <<>>).
%%
encode_psk_identities([], Acc) ->
    Len = byte_size(Acc),
    <<?UINT16(Len), Acc/binary>>;
encode_psk_identities([#psk_identity{
                          identity = Identity,
                          obfuscated_ticket_age = Age}|T], Acc) ->
    IdLen = byte_size(Identity),
    encode_psk_identities(T, <<Acc/binary,?UINT16(IdLen),Identity/binary,Age/binary>>).


encode_psk_binders(Binders) ->
    encode_psk_binders(Binders, <<>>).
%%
encode_psk_binders([], Acc) ->
    Len = byte_size(Acc),
    <<?UINT16(Len), Acc/binary>>;
encode_psk_binders([Binder|T], Acc) ->
    Len = byte_size(Binder),
    encode_psk_binders(T, <<Acc/binary,?BYTE(Len),Binder/binary>>).


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

dec_client_key(PKEPMS, ?KEY_EXCHANGE_RSA, {3, 0}) ->
    #encrypted_premaster_secret{premaster_secret = PKEPMS};
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
dec_client_key(<<?UINT16(Len), Id:Len/binary, PKEPMS/binary>>,
	       ?KEY_EXCHANGE_RSA_PSK, {3, 0}) ->
    #client_rsa_psk_identity{identity = Id,
			     exchange_keys = #encrypted_premaster_secret{premaster_secret = PKEPMS}};
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

dec_server_key_signature(Params, <<?BYTE(HashAlgo), ?BYTE(SignAlgo),
			    ?UINT16(0)>>, {Major, Minor})
  when Major == 3, Minor >= 3 ->
    HashSign = {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)},
    {Params, HashSign, <<>>};
dec_server_key_signature(Params, <<?BYTE(HashAlgo), ?BYTE(SignAlgo),
			    ?UINT16(Len), Signature:Len/binary>>, {Major, Minor})
  when Major == 3, Minor >= 3 ->
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
  when Version < {3,4} ->
    SignAlgoListLen = Len - 2,
    <<?UINT16(SignAlgoListLen), SignAlgoList/binary>> = ExtData,
    HashSignAlgos = [{ssl_cipher:hash_algorithm(Hash), ssl_cipher:sign_algorithm(Sign)} ||
			<<?BYTE(Hash), ?BYTE(Sign)>> <= SignAlgoList],
    decode_extensions(Rest, Version, MessageType,
                      Acc#{signature_algs =>
                               #hash_sign_algos{hash_sign_algos =
                                                    HashSignAlgos}});

decode_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc)
  when Version =:= {3,4} ->
    SignSchemeListLen = Len - 2,
    <<?UINT16(SignSchemeListLen), SignSchemeList/binary>> = ExtData,
    SignSchemes = [ssl_cipher:signature_scheme(SignScheme) ||
			<<?UINT16(SignScheme)>> <= SignSchemeList],
    decode_extensions(Rest, Version, MessageType,
                      Acc#{signature_algs =>
                               #signature_algorithms{
                                  signature_scheme_list = SignSchemes}});

decode_extensions(<<?UINT16(?SIGNATURE_ALGORITHMS_CERT_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    SignSchemeListLen = Len - 2,
    <<?UINT16(SignSchemeListLen), SignSchemeList/binary>> = ExtData,
    SignSchemes = [ssl_cipher:signature_scheme(SignScheme) ||
			<<?UINT16(SignScheme)>> <= SignSchemeList],
    decode_extensions(Rest, Version, MessageType,
                      Acc#{signature_algs_cert =>
                               #signature_algorithms_cert{
                                  signature_scheme_list = SignSchemes}});

decode_extensions(<<?UINT16(?ELLIPTIC_CURVES_EXT), ?UINT16(Len),
		       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc)
  when Version < {3,4} ->
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
		       ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc)
  when Version =:= {3,4} ->
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
                      Acc#{sni => #sni{hostname = ""}}); %% Server may send an empy SNI

decode_extensions(<<?UINT16(?SNI_EXT), ?UINT16(Len),
                ExtData:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    <<?UINT16(_), NameList/binary>> = ExtData,
    decode_extensions(Rest, Version, MessageType,
                      Acc#{sni => dec_sni(NameList)});

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
                               #server_hello_selected_version{selected_version =
                                                                  {3,4}}});

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

%% Ignore data following the ClientHello (i.e.,
%% extensions) if not understood.
decode_extensions(<<?UINT16(_), ?UINT16(Len), _Unknown:Len/binary, Rest/binary>>, Version, MessageType, Acc) ->
    decode_extensions(Rest, Version, MessageType, Acc);
%% This theoretically should not happen if the protocol is followed, but if it does it is ignored.
decode_extensions(_, _, _, Acc) ->
    Acc.

dec_hashsign(<<?BYTE(HashAlgo), ?BYTE(SignAlgo)>>) ->
    {ssl_cipher:hash_algorithm(HashAlgo), ssl_cipher:sign_algorithm(SignAlgo)}.

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
decode_client_shares(<<?UINT16(Group),?UINT16(Len),KeyExchange:Len/binary,Rest/binary>>, Acc) ->
    decode_client_shares(Rest, [#key_share_entry{
                                   group = tls_v1:enum_to_group(Group),
                                   key_exchange= KeyExchange
                                  }|Acc]).

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
    decode_psk_key_exchange_modes(Rest, [psk_dhe_ke|Acc]).


decode_psk_identities(Identities) ->
    decode_psk_identities(Identities, []).
%%
decode_psk_identities(<<>>, Acc) ->
    lists:reverse(Acc);
decode_psk_identities(<<?UINT16(Len), Identity:Len/binary, Age:4/binary, Rest/binary>>, Acc) ->
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

psk_secret(PSKIdentity, PSKLookup) ->
    case handle_psk_identity(PSKIdentity, PSKLookup) of
	{ok, PSK} when is_binary(PSK) ->
	    Len = erlang:byte_size(PSK),
	    <<?UINT16(Len), 0:(Len*8), ?UINT16(Len), PSK/binary>>;
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


filter_hashsigns([], [], _, _, Acc) ->
    lists:reverse(Acc);
filter_hashsigns([Suite | Suites], [#{key_exchange := KeyExchange} | Algos], HashSigns, Version,
 		 Acc) when KeyExchange == dhe_ecdsa;
 			   KeyExchange == ecdhe_ecdsa ->
    do_filter_hashsigns(ecdsa, Suite, Suites, Algos, HashSigns, Version, Acc); 
filter_hashsigns([Suite | Suites], [#{key_exchange := KeyExchange} | Algos], HashSigns, Version,
		 Acc) when KeyExchange == rsa;
			   KeyExchange == dhe_rsa;
			   KeyExchange == ecdhe_rsa;
			   KeyExchange == srp_rsa;
			   KeyExchange == rsa_psk ->
    do_filter_hashsigns(rsa, Suite, Suites, Algos, HashSigns, Version, Acc);
filter_hashsigns([Suite | Suites], [#{key_exchange := KeyExchange} | Algos], HashSigns, Version, Acc) when 
      KeyExchange == dhe_dss;
      KeyExchange == srp_dss ->							       
    do_filter_hashsigns(dsa, Suite, Suites, Algos, HashSigns, Version, Acc);
filter_hashsigns([Suite | Suites], [#{key_exchange := KeyExchange} | Algos], HashSigns, Verion,
                 Acc) when 
      KeyExchange == dh_dss; 
      KeyExchange == dh_rsa; 
      KeyExchange == dh_ecdsa;
      KeyExchange == ecdh_rsa;    
      KeyExchange == ecdh_ecdsa ->
      %%  Fixed DH certificates MAY be signed with any hash/signature
      %%  algorithm pair appearing in the hash_sign extension.  The names
    %%  DH_DSS, DH_RSA, ECDH_ECDSA, and ECDH_RSA are historical.
    filter_hashsigns(Suites, Algos, HashSigns, Verion, [Suite| Acc]);
filter_hashsigns([Suite | Suites], [#{key_exchange := KeyExchange} | Algos], HashSigns, Version,
                 Acc) when 
      KeyExchange == dh_anon;
      KeyExchange == ecdh_anon;
      KeyExchange == srp_anon;
      KeyExchange == psk;
      KeyExchange == dhe_psk;
      KeyExchange == ecdhe_psk ->
    %% In this case hashsigns is not used as the kexchange is anonaymous
    filter_hashsigns(Suites, Algos, HashSigns, Version, [Suite| Acc]).

do_filter_hashsigns(SignAlgo, Suite, Suites, Algos, HashSigns, Version, Acc) ->
    case lists:keymember(SignAlgo, 2, HashSigns) of
	true ->
	    filter_hashsigns(Suites, Algos, HashSigns, Version, [Suite| Acc]);
	false ->
	    filter_hashsigns(Suites, Algos, HashSigns, Version, Acc)
    end.

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
			       ClientCipherSuites, Compression,
			       ConnectionStates0, Renegotiation, SecureRenegotation) ->
    {ok, ConnectionStates} = handle_renegotiation_info(Version, RecordCB, Role, Info, ConnectionStates0,
                                                       Renegotiation, SecureRenegotation,
                                                       ClientCipherSuites),
    hello_pending_connection_states(RecordCB, Role,
                                    Version,
                                    NegotiatedCipherSuite,
                                    Random,
                                    Compression,
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
			       false, #{next_protocols_advertised := Protocols}) ->
    Protocols;

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

is_acceptable_hash_sign( _, KeyExAlgo, _) when 
      KeyExAlgo == psk;
      KeyExAlgo == dhe_psk;
      KeyExAlgo == ecdhe_psk;
      KeyExAlgo == srp_anon;
      KeyExAlgo == dh_anon;
      KeyExAlgo == ecdhe_anon     
      ->
    true; 
is_acceptable_hash_sign(Algos,_, SupportedHashSigns) -> 
    is_acceptable_hash_sign(Algos, SupportedHashSigns).

is_acceptable_hash_sign(Algos, SupportedHashSigns) ->
    lists:member(Algos, SupportedHashSigns).

is_acceptable_cert_type(Sign, Types) ->
    lists:member(sign_type(Sign), binary_to_list(Types)).

%% signature_algorithms_cert = undefined
is_supported_sign(SignAlgo, _, HashSigns, undefined) ->
    lists:member(SignAlgo, HashSigns);

%% {'SignatureAlgorithm',{1,2,840,113549,1,1,11},'NULL'}
is_supported_sign({Hash, Sign}, 'NULL', _, SignatureSchemes) ->
    Fun = fun (Scheme, Acc) ->
                  {H0, S0, _} = ssl_cipher:scheme_to_components(Scheme),
                  S1 = case S0 of
                             rsa_pkcs1 -> rsa;
                             S -> S
                         end,
                  H1 = case H0 of
                             sha1 -> sha;
                             H -> H
                         end,
                  Acc orelse (Sign =:= S1 andalso
                              Hash =:= H1)
          end,
    lists:foldl(Fun, false, SignatureSchemes);

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
is_supported_sign({Hash, Sign}, _Param, _, SignatureSchemes) ->
    Fun = fun (Scheme, Acc) ->
                  {H0, S0, _} = ssl_cipher:scheme_to_components(Scheme),
                  S1 = case S0 of
                             rsa_pkcs1 -> rsa;
                             S -> S
                         end,
                  H1 = case H0 of
                             sha1 -> sha;
                             H -> H
                         end,
                  Acc orelse (Sign  =:= S1 andalso
                              Hash  =:= H1)
          end,
    lists:foldl(Fun, false, SignatureSchemes).

%% SupportedPublicKeyAlgorithms PUBLIC-KEY-ALGORITHM-CLASS ::= {
%%   dsa | rsa-encryption | dh  | kea  | ec-public-key }
public_key_algo(?rsaEncryption) ->
    rsa;
public_key_algo(?'id-ecPublicKey') ->
    ecdsa;
public_key_algo(?'id-dsa') ->
    dsa.

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
sign_algo(Alg) ->
    public_key:pkix_sign_types(Alg).

sign_type(rsa) ->
    ?RSA_SIGN;
sign_type(dsa) ->
    ?DSS_SIGN;
sign_type(ecdsa) ->
    ?ECDSA_SIGN.

server_name(_, _, server) ->
    undefined; %% Not interesting to check your own name.
server_name(undefined, Host, client) ->
    {fallback, Host}; %% Fallback to Host argument to connect
server_name(SNI, _, client) ->
    SNI. %% If Server Name Indication is available

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

advertises_ec_ciphers([]) ->
    false;
advertises_ec_ciphers([#{key_exchange := ecdh_ecdsa} | _]) ->
    true;
advertises_ec_ciphers([#{key_exchange := ecdhe_ecdsa} | _]) ->
    true;
advertises_ec_ciphers([#{key_exchange := ecdh_rsa} | _]) ->
    true;
advertises_ec_ciphers([#{key_exchange := ecdhe_rsa} | _]) ->
    true;
advertises_ec_ciphers([#{key_exchange := ecdh_anon} | _]) ->
    true;
advertises_ec_ciphers([{ecdhe_psk, _,_,_} | _]) ->
    true;
advertises_ec_ciphers([_| Rest]) ->
    advertises_ec_ciphers(Rest).

select_shared_curve([], _) ->
    no_curve;
select_shared_curve([Curve | Rest], Curves) ->
    case lists:member(Curve, Curves) of
	true ->
	    {namedCurve, Curve};
	false ->
	    select_shared_curve(Rest, Curves)
    end.

sni(undefined) ->
    undefined;
sni(disable) ->
    undefined;
sni(Hostname) ->
    #sni{hostname = Hostname}.

renegotiation_info(_, client, _, false) ->
    #renegotiation_info{renegotiated_connection = undefined};
renegotiation_info(_RecordCB, server, ConnectionStates, false) ->
    ConnectionState  = ssl_record:current_connection_state(ConnectionStates, read),
    case maps:get(secure_renegotiation, ConnectionState) of
	true ->
	    #renegotiation_info{renegotiated_connection = ?byte(0)};
	false ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end;
renegotiation_info(_RecordCB, client, ConnectionStates, true) ->
    ConnectionState = ssl_record:current_connection_state(ConnectionStates, read),
    case  maps:get(secure_renegotiation, ConnectionState) of
	true ->
	    Data = maps:get(client_verify_data, ConnectionState),
	    #renegotiation_info{renegotiated_connection = Data};
	false ->
	    #renegotiation_info{renegotiated_connection = undefined}
    end;

renegotiation_info(_RecordCB, server, ConnectionStates, true) ->
    ConnectionState = ssl_record:current_connection_state(ConnectionStates, read),
    case maps:get(secure_renegotiation, ConnectionState) of
	true ->
	    CData = maps:get(client_verify_data, ConnectionState),
	    SData = maps:get(server_verify_data, ConnectionState),
	    #renegotiation_info{renegotiated_connection = <<CData/binary, SData/binary>>};
	false ->
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
    ConnectionState = ssl_record:current_connection_state(ConnectionStates, read),
    CData = maps:get(client_verify_data, ConnectionState),
    SData = maps:get(server_verify_data, ConnectionState),
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
	      ConnectionState = ssl_record:current_connection_state(ConnectionStates, read),
	      Data =  maps:get(client_verify_data, ConnectionState),
	      case Data == ClientVerify of
		  true ->
		      {ok, ConnectionStates};
		  false ->
                      throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, server_renegotiation))
	      end
      end;
handle_renegotiation_info({3,0}, _RecordCB, client, undefined, ConnectionStates, true, _SecureRenegotation, _) ->
    {ok, ssl_record:set_renegotiation_flag(true, ConnectionStates)};

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
    ConnectionState = ssl_record:current_connection_state(ConnectionStates, read),
    case {SecureRenegotation, maps:get(secure_renegotiation, ConnectionState)} of
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

empty_extensions({3,4}, client_hello) ->
    #{
      sni => undefined,
      %% max_fragment_length => undefined,
      %% status_request => undefined,
      elliptic_curves => undefined,
      signature_algs => undefined,
      %% use_srtp => undefined,
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
      %% cookie => undefined,
      client_hello_versions => undefined,
      %% cert_authorities => undefined,
      %% post_handshake_auth => undefined,
      signature_algs_cert => undefined
     };
empty_extensions({3, 3}, client_hello) ->
    Ext = empty_extensions({3,2}, client_hello),
    Ext#{signature_algs => undefined};
empty_extensions(_, client_hello) ->
    #{renegotiation_info => undefined,
      alpn => undefined,
      next_protocol_negotiation => undefined,
      srp => undefined,
      ec_point_formats => undefined,
      elliptic_curves => undefined,
      sni => undefined};
empty_extensions({3,4}, server_hello) ->
    #{server_hello_selected_version => undefined,
      key_share => undefined,
      pre_shared_key => undefined
     };
empty_extensions({3,4}, hello_retry_request) ->
    #{server_hello_selected_version => undefined,
      key_share => undefined,
      pre_shared_key => undefined
     };
empty_extensions({3,0}, _) ->
    empty_extensions();
empty_extensions(_, server_hello) ->
    #{renegotiation_info => undefined,
      alpn => undefined,
      next_protocol_negotiation => undefined,
      ec_point_formats => undefined}.
