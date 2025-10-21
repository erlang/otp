%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
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
%% Purpose: Record and constant definitions for the SSL-record protocol
% see RFC 2246
%%----------------------------------------------------------------------

-ifndef(ssl_record).
-define(ssl_record, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Connection states - RFC 4346 section 6.1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For documentation purposes are now maps in implementation
%% -record(connection_state, {
%% 	  security_parameters,
%% 	  cipher_state,
%% 	  mac_secret,
%% 	  sequence_number,
%%        reneg =  %% RFC 5746
%% 	     #{secure_renegotiation,
%% 	       client_verify_data,
%% 	       server_verify_data},
%% 	  %% How to do BEAST mitigation?
%% 	  beast_mitigation
%% 	 }).

%% -record(connection_states, {
%% 	  current_read,
%% 	  pending_read,
%% 	  current_write,
%% 	  pending_write,
%% 	 }).

-record(security_parameters, {
          cipher_suite,
          connection_end,
          bulk_cipher_algorithm,
          cipher_type,
          iv_size,
          key_material_length,			% unit 8 
          mac_algorithm,			% unit 8  
          prf_algorithm,			% unit 8
          hash_size,				% unit 8
          master_secret,			% opaque 48
          resumption_master_secret,
          application_traffic_secret,
          client_early_data_secret,
          client_random,			% opaque 32
          server_random			        % opaque 32
       }). 

-define(INITIAL_BYTES, 5).

-define(MAX_SEQUENCE_NUMBER, 18446744073709551615). %% (1 bsl 64) - 1 = 18446744073709551615
%% Sequence numbers cannot wrap so when max is about to be reached we should renegotiate.
%% We will renegotiate a little before so that there will be sequence numbers left
%% for the rehandshake and a little data. Currently we decided to renegotiate a little more
%% often as we can have a cheaper test to check if it is time to renegotiate. It will still
%% be fairly seldom. 
-define(DEFAULT_RENEGOTIATE_AT, 268435456). %% math:pow(2, 28) 

%% ConnectionEnd
-define(SERVER, 0).
-define(CLIENT, 1).

%% BulkCipherAlgorithm
%-define(NULL, 0). %% Already defined by ssl_internal.hrl
-define(RC4, 1).
-define(RC2, 2).
-define(DES, 3).
-define('3DES', 4).
-define(DES40, 5).
-define(IDEA, 6).
-define(AES_CBC, 7).
-define(AES_GCM, 8).
-define(CHACHA20_POLY1305, 9).
%% Following two are not defined in any RFC but we want to have the
%% same type of handling internally, all of these "bulk_cipher_algorithm"
%% enums are only used internally anyway.
-define(AES_CCM, 10). 
-define(AES_CCM_8, 11). 

%% CipherType
-define(STREAM, 0).
-define(BLOCK, 1).
-define(AEAD, 2).

%% IsExportable
%-define(TRUE, 0).  %% Already defined by ssl_internal.hrl
%-define(FALSE, 1). %% Already defined by ssl_internal.hrl

%% MAC and PRF Algorithms
%-define(NULL, 0). %% Already defined by ssl_internal.hrl
-define(MD5, 1).
-define(SHA, 2).
-define(MD5SHA, 4711). %% Not defined in protocol used to represent old prf
-define(SHA224, 3).
-define(SHA256, 4).
-define(SHA384, 5).
-define(SHA512, 6).

%% See also cipher.hrl for #cipher_state{}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Record layer - RFC 2246 section 6.2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%enum {
%%      change_cipher_spec(20), alert(21), handshake(22),
%%      application_data(23), (255)
%%       } ContentType;

-define(CHANGE_CIPHER_SPEC, 20). 
-define(ALERT, 21).
-define(HANDSHAKE, 22).
-define(APPLICATION_DATA, 23).
-define(HEARTBEAT, 24).
-define(KNOWN_RECORD_TYPE(Type),
        (is_integer(Type) andalso (20 =< (Type)) andalso ((Type) =< 23))).
-define(MAX_PLAIN_TEXT_LENGTH, 16384).
-define(MAX_CIPHER_TEXT_LENGTH, (?MAX_PLAIN_TEXT_LENGTH+2048)).
-define(TLS13_MAX_CIPHER_TEXT_LENGTH, (?MAX_PLAIN_TEXT_LENGTH+256)).
-define(MAX_PADDING_LENGTH,256).
-define(MAX_MAC_LENGTH,32).

%% -record(protocol_version, {
%% 	  major,  % unit 8
%% 	  minor   % unit 8
%% 	 }).

-record(generic_stream_cipher, {
          content,  % opaque content[TLSCompressed.length];
          mac       % opaque MAC[CipherSpec.hash_size]; 
         }).

-record(generic_block_cipher, {
          iv,            % opaque IV[CipherSpec.block_length];
          content, % opaque content[TLSCompressed.length];
          mac,     % opaque MAC[CipherSpec.hash_size];
          padding, % unit 8 padding[GenericBlockCipher.padding_length];
          padding_length, % uint8 padding_length;
          next_iv  % opaque IV[SecurityParameters.record_iv_length];
         }). 

-define(PROTOCOL_TO_BINARY_VERSION(Version), (Version)).
-define(BINARY_PROTOCOL_TO_INTERNAL_REPRESENTATION(Version), (Version)).

-define(TLS_1_X(Version), (element(1,Version) == 3)).
-define(DTLS_1_X(Version), (element(1,Version) == 254)).

-define(TLS_GTE(Version1, Version2), (Version1 >= Version2)).
-define(TLS_GT(Version1, Version2),  (Version1 > Version2)).
-define(TLS_LTE(Version1, Version2), (Version1 =< Version2)).
-define(TLS_LT(Version1, Version2),  (Version1 < Version2)).

-define(DTLS_GTE(Version1, Version2), (Version1 =< Version2)).
-define(DTLS_GT(Version1, Version2),  (Version1 < Version2)).
-define(DTLS_LTE(Version1, Version2), (Version >= Version2)).
-define(DTLS_LT(Version1, Version2),  (Version1 > Version2)).

%% Atoms used to refer to protocols

-define(TLS_1_3, {3,4}).
-define(TLS_1_2, {3,3}).
-define(TLS_1_1, {3,2}).
-define(TLS_1_0, {3,1}).

-define(DTLS_1_2, {254,253}).
-define(DTLS_1_0, {254,255}).

-define(SSL_3_0, {3,0}).
-define(SSL_2_0, {2,0}).

-endif. % -ifdef(ssl_record).
