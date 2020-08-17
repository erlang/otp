%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2020. All Rights Reserved.
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
%% Purpose: Record and constant defenitions for the SSL-handshake protocol
%% see RFC 5246. Also includes supported hello extensions.
%%----------------------------------------------------------------------

-ifndef(ssl_handshake).
-define(ssl_handshake, true).

-include_lib("public_key/include/public_key.hrl").

-define(NO_PROTOCOL, <<>>).

%% Signature algorithms
-define(ANON, 0).
-define(RSA, 1).
-define(DSA, 2).
-define(ECDSA, 3).

-record(session, {
	  session_id,
	  peer_certificate,
	  own_certificate,
	  compression_method,
	  cipher_suite,
	  master_secret,
	  srp_username,
	  is_resumable,
	  time_stamp,
	  ecc,                   %% TLS 1.3 Group
	  sign_alg,              %% TLS 1.3 Signature Algorithm
	  dh_public_value        %% TLS 1.3 DH Public Value from peer
	  }).

-define(NUM_OF_SESSION_ID_BYTES, 32).  % TSL 1.1 & SSL 3
-define(NUM_OF_PREMASTERSECRET_BYTES, 48).
-define(DEFAULT_DIFFIE_HELLMAN_GENERATOR, ssl_dh_groups:modp2048_generator()).
-define(DEFAULT_DIFFIE_HELLMAN_PRIME, ssl_dh_groups:modp2048_prime()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handsake protocol - RFC 4346 section 7.4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% enum {
%%        hello_request(0), client_hello(1), server_hello(2),
%%        certificate(11), server_key_exchange (12),
%%        certificate_request(13), server_hello_done(14),
%%        certificate_verify(15), client_key_exchange(16),
%%        finished(20), (255)
%%    } HandshakeType;

-define(HELLO_REQUEST, 0).
-define(CLIENT_HELLO, 1).
-define(CLIENT_HELLO_V2, 3).
-define(SERVER_HELLO, 2).
-define(CERTIFICATE, 11).
-define(SERVER_KEY_EXCHANGE, 12).
-define(CERTIFICATE_REQUEST, 13).
-define(SERVER_HELLO_DONE, 14).
-define(CERTIFICATE_VERIFY, 15).
-define(CLIENT_KEY_EXCHANGE, 16).
-define(FINISHED, 20).

-define(MAX_UNIT24, 8388607).
-define(DEFAULT_MAX_HANDSHAKE_SIZE,  (256*1024)).

-record(random, {
	  gmt_unix_time, % uint32
	  random_bytes   % opaque random_bytes[28]
	  }).

%% enum { null(0), (255) } CompressionMethod;
% -define(NULL, 0). %% Already defined by ssl_internal.hrl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hello messages - RFC 5246 section 7.4.1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% client_hello defined in tls_handshake.hrl and dtls_handshake.hrl

-record(hello_extensions, {
	  renegotiation_info,
	  signature_algs,          % supported combinations of hashes/signature algos
          alpn,
	  next_protocol_negotiation = undefined, % [binary()]
	  srp,
	  ec_point_formats,
	  elliptic_curves,
	  sni,
          client_hello_versions,
          server_hello_selected_version,
          signature_algs_cert,
          key_share
	 }).

-record(server_hello, {
	  server_version,
	  random,             
	  session_id,         % opaque SessionID<0..32>
	  cipher_suite,       % cipher_suites
	  compression_method, % compression_method
	  extensions
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server authentication and key exchange messages - RFC 5246 section 7.4.3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% opaque ASN.1Cert<2^24-1>;

-record(certificate, {
	  asn1_certificates %% certificate_list<1..2^24-1>
	 }).
	
%% enum { rsa, diffie_hellman } KeyExchangeAlgorithm;

-define(KEY_EXCHANGE_RSA, 0).
-define(KEY_EXCHANGE_DIFFIE_HELLMAN, 1).
-define(KEY_EXCHANGE_EC_DIFFIE_HELLMAN, 6).
-define(KEY_EXCHANGE_PSK, 2).
-define(KEY_EXCHANGE_EC_DIFFIE_HELLMAN_PSK, 7).
-define(KEY_EXCHANGE_DHE_PSK, 3).
-define(KEY_EXCHANGE_RSA_PSK, 4).
-define(KEY_EXCHANGE_SRP, 5).

-record(server_rsa_params, {
	  rsa_modulus,  %%  opaque RSA_modulus<1..2^16-1>
	  rsa_exponent  %%  opaque RSA_exponent<1..2^16-1>
	 }).

-record(server_dh_params, {
	  dh_p, %% opaque DH_p<1..2^16-1>
	  dh_g, %% opaque DH_g<1..2^16-1>
	  dh_y  %% opaque DH_Ys<1..2^16-1>
	 }).

-record(server_ecdh_params, {
	  curve,
	  public           %% opaque encoded ECpoint
	 }).

-record(server_psk_params, {
	  hint
	 }).

-record(server_dhe_psk_params, {
	  hint,
	  dh_params
	 }).

-record(server_ecdhe_psk_params, {
	  hint,
	  dh_params
	 }).

-record(server_srp_params, {
	  srp_n, %% opaque srp_N<1..2^16-1>
	  srp_g, %% opaque srp_g<1..2^16-1>
	  srp_s, %% opaque srp_s<1..2^8-1>
	  srp_b  %% opaque srp_B<1..2^16-1>
	 }).

-record(server_key_exchange, {
	  exchange_keys
	 }).

-record(server_key_params, {
	  params, %% #server_rsa_params{} | #server_dh_params{}
	  params_bin,
	  hashsign, %% term(atom(), atom())
	  signature %% #signature{}
	 }).
	
%% enum { anonymous, rsa, dsa } SignatureAlgorithm;

-define(SIGNATURE_ANONYMOUS, 0).
-define(SIGNATURE_RSA, 1).
-define(SIGNATURE_DSA, 2).

-record(hello_request, {}).
-record(server_hello_done, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Certificate request  - RFC 5246 section 7.4.4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%    enum {
%%        rsa_sign(1), dss_sign(2), rsa_fixed_dh(3), dss_fixed_dh(4),
%%        (255)
%%    } ClientCertificateType;

-define(RSA_SIGN, 1).
-define(DSS_SIGN, 2).
-define(RSA_FIXED_DH, 3).
-define(DSS_FIXED_DH, 4).
-define(ECDSA_SIGN, 64).
-define(RSA_FIXED_ECDH, 65).
-define(ECDSA_FIXED_ECDH, 66).

% opaque DistinguishedName<1..2^16-1>;

-record(certificate_request, {
	  certificate_types,        %ClientCertificateType   <1..2^8-1>
	  hashsign_algorithms,      %%SignatureAndHashAlgorithm <2^16-1>;
	  certificate_authorities   %DistinguishedName       <0..2^16-1>
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Client authentication and key exchange messages - RFC 4346 section 7.4.7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(client_key_exchange, {
	  exchange_keys %% #encrypted_premaster_secret{} (rsa ) |
			%%  DiffieHellmanClientPublicValue 
	  }).
   
-record(pre_master_secret, {
	  client_version, % ProtocolVersion client_version
	  random          % opaque random[46];
	 }).

-record(encrypted_premaster_secret, {
	  premaster_secret
	 }).

%% enum { implicit, explicit } PublicValueEncoding;

-define(IMPLICIT, 0).
-define(EXPLICIT, 1).

-record(client_diffie_hellman_public, {
	  dh_public
	 }).

-record(client_ec_diffie_hellman_public, {
	  dh_public
	 }).

-record(client_psk_identity, {
	  identity
	 }).

-record(client_dhe_psk_identity, {
	  identity,
	  dh_public
	 }).

-record(client_ecdhe_psk_identity, {
	  identity,
	  dh_public
	 }).

-record(client_rsa_psk_identity, {
	  identity,
	  exchange_keys
	 }).

-record(client_srp_public, {
	  srp_a
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Certificate verify - RFC 4346 section 7.4.8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(certificate_verify, {
	  hashsign_algorithm,
	  signature % binary()
	 }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handshake finalization message  RFC 4346 section 7.4.9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(finished, {
	  verify_data %opaque verify_data[12]
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Renegotiation info  RFC 5746 section 3.2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(RENEGOTIATION_EXT, 16#ff01).

-record(renegotiation_info,{
	  renegotiated_connection
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SRP  RFC 5054 section 2.8.1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SRP_EXT, 12).

-record(srp, {
	  username
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Signature Algorithms  RFC 5746 section 7.4.1.4.1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SIGNATURE_ALGORITHMS_EXT, 13).

-record(hash_sign_algos, {hash_sign_algos}).
%% RFC 8446 (TLS 1.3)
-record(signature_algorithms, {signature_scheme_list}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RFC 7301 Application-Layer Protocol Negotiation  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ALPN_EXT, 16).

-record(alpn, {extension_data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Next Protocol Negotiation
%% (http://tools.ietf.org/html/draft-agl-tls-nextprotoneg-02)
%% (http://technotes.googlecode.com/git/nextprotoneg.html)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NEXTPROTONEG_EXT, 13172).
-define(NEXT_PROTOCOL, 67).
-record(next_protocol_negotiation, {extension_data}).

-record(next_protocol, {selected_protocol}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ECC Extensions RFC 8422  section 4 and 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ELLIPTIC_CURVES_EXT, 10).
-define(EC_POINT_FORMATS_EXT, 11).

-record(elliptic_curves, {
	  elliptic_curve_list
	 }).

%% RFC 8446 (TLS 1.3) renamed the "elliptic_curve" extension.
-record(supported_groups, {
	  supported_groups
	 }).

-record(ec_point_formats, {
	  ec_point_format_list
	 }).

-define(ECPOINT_UNCOMPRESSED, 0).
%% Defined in RFC 4492, deprecated by RFC 8422
%% RFC 8422 compliant implementations MUST not support the two formats below
-define(ECPOINT_ANSIX962_COMPRESSED_PRIME, 1).
-define(ECPOINT_ANSIX962_COMPRESSED_CHAR2, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ECC RFC 4492 Handshake Messages, Section 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EXPLICIT_PRIME, 1).
-define(EXPLICIT_CHAR2, 2).
-define(NAMED_CURVE, 3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RFC 6066 TLS Extensions: Extension Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% section 3
-define(SNI_EXT, 0).

%% enum { host_name(0), (255) } NameType;
-define(SNI_NAMETYPE_HOST_NAME, 0).

-record(sni, {
          hostname = undefined
        }).

%% enum{ 2^9(1), 2^10(2), 2^11(3), 2^12(4), (255) } MaxFragmentLength;
-define(MAX_FRAGMENT_LENGTH_EXT, 1).
-define(MAX_FRAGMENT_LENGTH_BYTES_1,  512).
-define(MAX_FRAGMENT_LENGTH_BYTES_2, 1024).
-define(MAX_FRAGMENT_LENGTH_BYTES_3, 2048).
-define(MAX_FRAGMENT_LENGTH_BYTES_4, 4096).

-record(max_frag_enum, {
          enum = undefined  %% contains the enum value 1..4
        }).

%% Section 8, Certificate Status Request
-define(STATUS_REQUEST, 5).
-define(CERTIFICATE_STATUS_TYPE_OCSP, 1).
-define(CERTIFICATE_STATUS, 22).

%% status request record defined in RFC 6066, section 8
-record(certificate_status_request, {
	status_type,
	request
}).

-record(ocsp_status_request, {
	responder_id_list = [],
	request_extensions = []
}).

-record(certificate_status, {
	status_type,
	response
}).

%% Other possible values from RFC 6066, not supported
-define(CLIENT_CERTIFICATE_URL, 2).
-define(TRUSTED_CA_KEYS, 3).
-define(TRUNCATED_HMAC, 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RFC 7250 Using Raw Public Keys in Transport Layer Security (TLS)
%% and Datagram Transport Layer Security (DTLS)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Not supported
-define(CLIENT_CERTIFICATE_TYPE, 19).            
-define(SERVER_CERTIFICATE_TYPE, 20).         

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RFC 6520 Transport Layer Security (TLS) and
%% Datagram Transport Layer Security (DTLS) Heartbeat Extension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Not supported
-define(HS_HEARTBEAT, 15).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RFC 6962 Certificate Transparency     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Not supported
-define(SIGNED_CERTIFICATE_TIMESTAMP, 18).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RFC 7685  A Transport Layer Security (TLS) ClientHello Padding Extension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Not supported
-define(PADDING, 21).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Supported Versions RFC 8446 (TLS 1.3) section 4.2.1 also affects TLS-1.2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SUPPORTED_VERSIONS_EXT, 43).

-record(client_hello_versions, {versions}).
-record(server_hello_selected_version, {selected_version}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Signature Algorithms RFC 8446 (TLS 1.3) section 4.2.3 also affects TLS-1.2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SIGNATURE_ALGORITHMS_CERT_EXT, 50).

-record(signature_algorithms_cert, {signature_scheme_list}).

-endif. % -ifdef(ssl_handshake).
