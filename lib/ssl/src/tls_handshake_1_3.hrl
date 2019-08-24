%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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
%% Purpose: Record and constant defenitions for the TLS-handshake protocol
%% see RFC 8446. Also includes supported hello extensions.
%%----------------------------------------------------------------------

-ifndef(tls_handshake_1_3).
-define(tls_handshake_1_3, true).

%% Common to TLS-1.3 and previous TLS versions 
%% Some defenitions may not exist in TLS-1.3 this is 
%% handled elsewhere
-include("tls_handshake.hrl"). 

%% New handshake types in TLS-1.3 RFC 8446 B.3
-define(NEW_SESSION_TICKET, 4).
-define(END_OF_EARLY_DATA, 5).
-define(ENCRYPTED_EXTENSIONS, 8).
-define(KEY_UPDATE, 24).
%% %% Not really a message but special way to handle handshake hashes
%% %% when a "hello-retry-request" (special server_hello) is sent
-define(MESSAGE_HASH, 254). 

%% %%  RFC 8446 B.3.1.
%% %% New extension types in TLS-1.3
-define(PRE_SHARED_KEY_EXT, 41).
-define(EARLY_DATA_EXT, 42).
%%-define(SUPPORTED_VERSIONS_EXT, 43). %% Updates TLS 1.2 so defined in ssl_handshake.hrl
-define(COOKIE_EXT, 44).
-define(PSK_KEY_EXCHANGE_MODES_EXT, 45).
-define(CERTIFICATE_AUTHORITIES_EXT, 47).
-define(OID_FILTERS_EXT, 48).
-define(POST_HANDSHAKE_AUTH_EXT, 49).
%% -define(SIGNATURE_ALGORITHMS_CERT_EXT, 50). %% Updates TLS 1.2 so defined in ssl_handshake.hrl
-define(KEY_SHARE_EXT, 51).

%%  RFC 8446 B.3.1
-record(key_share_entry, {
          group,  %NamedGroup
          key_exchange %key_exchange<1..2^16-1>;
         }).
-record(key_share_client_hello, {
          client_shares   %% KeyShareEntry client_shares<0..2^16-1>;
         }).
-record(key_share_hello_retry_request, {
          selected_group  %%  NamedGroup
         }).
-record(key_share_server_hello, {
          server_share  %% KeyShareEntry server_share;
         }).

-record(uncompressed_point_representation, {
          legacy_form = 4, %     uint8 legacy_form = 4;
          x,               %     opaque X[coordinate_length];
          y                %     opaque Y[coordinate_length];
         }).

%% RFC 8446 4.2.9.  Pre-Shared Key Exchange Modes

%% enum { psk_ke(0), psk_dhe_ke(1), (255) } PskKeyExchangeMode;
-define(PSK_KE, 0).
-define(PSK_DHE_KE, 1).

-record(psk_key_exchange_modes, {
          ke_modes % ke_modes<1..255>
         }).

%% RFC 8446 4.2.10.  Early Data Indication
-record(empty, {
         }).
-record(early_data_indication, {
          indication % uint32 max_early_data_size (new_session_ticket) | 
          %% #empty{} (client_hello, encrypted_extensions)
         }).

%% RFC 8446 4.2.11. Pre-Shared Key Extension
-record(psk_identity,
        {
         identity,             % opaque identity<1..2^16-1>
         obfuscated_ticket_age % uint32
        }).

-record(offered_psks,
        {
         identities,    % PskIdentity identities<7..2^16-1>;
         binders        % PskBinderEntry binders<33..2^16-1>; opaque PskBinderEntry<32..255>
        }).

%% struct {
%%     select (Handshake.msg_type) {
%%         case client_hello: OfferedPsks;
%%         case server_hello: uint16 selected_identity;
%%     };
%% } PreSharedKeyExtension;
-record(pre_shared_key_client_hello,
        {
         offered_psks
        }).

-record(pre_shared_key_server_hello,
        {
         selected_identity
        }).

%% RFC 8446 B.3.1.2.
-record(cookie, {
          cookie %cookie<1..2^16-1>;
         }).

%%% RFC 8446 B.3.1.3.  Signature Algorithm Extension
%% Signature Schemes
%% RSASSA-PKCS1-v1_5 algorithms
-define(RSA_PKCS1_SHA256, 16#0401).
-define(RSA_PKCS1_SHA384, 16#0501).
-define(RSA_PKCS1_SHA512, 16#0601).

%% ECDSA algorithms 
-define(ECDSA_SECP256R1_SHA256, 16#0403).
-define(ECDSA_SECP384R1_SHA384, 16#0503).
-define(ECDSA_SECP521R1_SHA512, 16#0603).

%% RSASSA-PSS algorithms with public key OID rsaEncryption 
-define(RSA_PSS_RSAE_SHA256, 16#0804).
-define(RSA_PSS_RSAE_SHA384, 16#0805).
-define(RSA_PSS_RSAE_SHA512, 16#0806).

%% EdDSA algorithms 
-define(ED25519, 16#0807).
-define(ED448, 16#0808).

%% RSASSA-PSS algorithms with public key OID RSASSA-PSS 
-define(RSA_PSS_PSS_SHA256, 16#0809).
-define(RSA_PSS_PSS_SHA384, 16#080a).
-define(RSA_PSS_PSS_SHA512, 16#080b).

%% Legacy algorithms
-define(RSA_PKCS1_SHA1, 16#201).
-define(ECDSA_SHA1, 16#0203).

%%  RFC 8446 B.3.1.4.  Supported Groups Extension
%% Elliptic Curve Groups (ECDHE)
-define(SECP256R1, 16#0017). 
-define(SECP384R1, 16#0018). 
-define(SECP521R1, 16#0019).
-define(X25519, 16#001D).
-define(X448, 16#001E).

%% RFC 8446 Finite Field Groups (DHE)
-define(FFDHE2048, 16#0100).
-define(FFDHE3072, 16#0101).
-define(FFDHE4096, 16#0102). 
-define(FFDHE6144, 16#0103).
-define(FFDHE8192 ,16#0104).

-record(named_group_list, {
        named_group_list  %named_group_list<2..2^16-1>;
         }).

%%  RFC 8446 B.3.2 Server Parameters Messages
%%  opaque DistinguishedName<1..2^16-1>;XS
-record(certificate_authoritie_sextension, {
          authorities  %DistinguishedName authorities<3..2^16-1>;
         }).

-record(oid_filter, {
          certificate_extension_oid, % opaque certificate_extension_oid<1..2^8-1>;
          certificate_extension_values % opaque certificate_extension_values<0..2^16-1>;
         }).

-record(oid_filter_extension, {
          filters %OIDFilter filters<0..2^16-1>;
         }).
-record(post_handshake_auth, {
         }).

-record(encrypted_extensions, {
          extensions  %extensions<0..2^16-1>;
         }).

-record(certificate_request_1_3, {
          certificate_request_context, % opaque certificate_request_context<0..2^8-1>;
          extensions %Extension extensions<2..2^16-1>;
         }).

%%  RFC 8446 B.3.3  Authentication Messages

%% Certificate Type
-define(X509, 0).
-define(OpenPGP_RESERVED, 1).
-define(RawPublicKey, 2).

-record(certificate_entry, {
          data,  
          %% select (certificate_type) {
          %%     case RawPublicKey:
          %%       /* From RFC 7250 ASN.1_subjectPublicKeyInfo */
          %%       opaque ASN1_subjectPublicKeyInfo<1..2^24-1>;
          %%
          %%     case X509:
          %%       opaque cert_data<1..2^24-1>;
          %% };
          extensions %% Extension extensions<0..2^16-1>;
         }).

-record(certificate_1_3, {
          certificate_request_context, % opaque certificate_request_context<0..2^8-1>;
          certificate_list             % CertificateEntry certificate_list<0..2^24-1>;
         }).

-record(certificate_verify_1_3, {
          algorithm, % SignatureScheme
          signature  % signature<0..2^16-1>
	 }).

%% RFC 8446 B.3.4. Ticket Establishment
-record(new_session_ticket, {
          ticket_lifetime,  %unit32
          ticket_age_add,   %unit32
          ticket_nonce,     %opaque ticket_nonce<0..255>;
          ticket,           %opaque ticket<1..2^16-1>
          extensions        %extensions<0..2^16-2>
         }).

%%  RFC 8446 B.3.5. Updating Keys
-record(end_of_early_data, {
         }).

-define(UPDATE_NOT_REQUESTED, 0).
-define(UPDATE_REQUESTED, 1).

-record(key_update, {
          request_update
         }).

-type tls_handshake_1_3() :: #encrypted_extensions{} |
                             #certificate_request_1_3{} |
                             #certificate_1_3{} |
                             #certificate_verify_1_3{}.

-export_type([tls_handshake_1_3/0]).

-endif. % -ifdef(tls_handshake_1_3).
