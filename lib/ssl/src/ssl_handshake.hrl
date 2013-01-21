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

%%
%%----------------------------------------------------------------------
%% Purpose: Record and constant defenitions for the SSL-handshake protocol
%% see RFC 4346
%%----------------------------------------------------------------------

-ifndef(ssl_handshake).
-define(ssl_handshake, true).

-include_lib("public_key/include/public_key.hrl").

-type algo_oid()          :: ?'rsaEncryption' | ?'id-dsa'.
-type public_key_params() :: #'Dss-Parms'{} | term().
-type public_key_info()   :: {algo_oid(), #'RSAPublicKey'{} | integer() , public_key_params()}.
-type tls_handshake_history() :: {[binary()], [binary()]}.

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
	  is_resumable,
	  time_stamp
	  }).

-define(NUM_OF_SESSION_ID_BYTES, 32).  % TSL 1.1 & SSL 3
-define(NUM_OF_PREMASTERSECRET_BYTES, 48).
-define(DEFAULT_DIFFIE_HELLMAN_GENERATOR, 2).
-define(DEFAULT_DIFFIE_HELLMAN_PRIME,  16#FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF).

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

-record(random, {
	  gmt_unix_time, % uint32
	  random_bytes   % opaque random_bytes[28]
	  }).

%% enum { null(0), (255) } CompressionMethod;
% -define(NULL, 0). %% Already defined by ssl_internal.hrl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hello messages - RFC 4346 section 7.4.2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(client_hello, {
	  client_version,
	  random,             
	  session_id,         % opaque SessionID<0..32>
	  cipher_suites,      % cipher_suites<2..2^16-1>
	  compression_methods, % compression_methods<1..2^8-1>,
	  renegotiation_info,
	  hash_signs,          % supported combinations of hashes/signature algos
	  next_protocol_negotiation = undefined % [binary()]
	 }).

-record(server_hello, {
	  server_version,
	  random,             
	  session_id,         % opaque SessionID<0..32>
	  cipher_suite,       % cipher_suites
	  compression_method, % compression_method
	  renegotiation_info,
	  hash_signs,          % supported combinations of hashes/signature algos
	  next_protocol_negotiation = undefined % [binary()]
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server authentication and key exchange messages - RFC 4346 section 7.4.3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% opaque ASN.1Cert<2^24-1>;

-record(certificate, {
	  asn1_certificates %% certificate_list<1..2^24-1>
	 }).
	
%% enum { rsa, diffie_hellman } KeyExchangeAlgorithm;

-define(KEY_EXCHANGE_RSA, 0).
-define(KEY_EXCHANGE_DIFFIE_HELLMAN, 1).

-record(server_rsa_params, {
	  rsa_modulus,  %%  opaque RSA_modulus<1..2^16-1>
	  rsa_exponent  %%  opaque RSA_exponent<1..2^16-1>
	 }).

-record(server_dh_params, {
	  dh_p, %% opaque DH_p<1..2^16-1>
	  dh_g, %% opaque DH_g<1..2^16-1>
	  dh_y  %% opaque DH_Ys<1..2^16-1>
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
%%% Certificate request  - RFC 4346 section 7.4.4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%    enum {
%%        rsa_sign(1), dss_sign(2), rsa_fixed_dh(3), dss_fixed_dh(4),
%%        (255)
%%    } ClientCertificateType;

-define(RSA_SIGN, 1).
-define(DSS_SIGN, 2).
-define(RSA_FIXED_DH, 3).
-define(DSS_FIXED_DH, 4).

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
%% Signature Algorithms  RFC 5746 section 7.4.1.4.1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SIGNATURE_ALGORITHMS_EXT, 13).

-record(hash_sign_algos, {
	  hash_sign_algos
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Next Protocol Negotiation
%% (http://tools.ietf.org/html/draft-agl-tls-nextprotoneg-02)
%% (http://technotes.googlecode.com/git/nextprotoneg.html)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NEXTPROTONEG_EXT, 13172).
-define(NEXT_PROTOCOL, 67).
-record(next_protocol_negotiation, {extension_data}).

-record(next_protocol, {selected_protocol}).

-endif. % -ifdef(ssl_handshake).


     
