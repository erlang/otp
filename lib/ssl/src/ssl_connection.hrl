%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2019. All Rights Reserved.
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
%% Purpose: SSL/TLS specific state
%%----------------------------------------------------------------------

-ifndef(ssl_connection).
-define(ssl_connection, true).

-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_handshake.hrl").
-include("ssl_srp.hrl").
-include("ssl_cipher.hrl").
-include_lib("public_key/include/public_key.hrl").

-record(static_env, {
                     role                  :: client | server,
                     transport_cb          :: atom(),   % callback module
                     protocol_cb           :: tls_connection | dtls_connection,
                     data_tag              :: atom(),   % ex tcp.
                     close_tag             :: atom(),   % ex tcp_closed
                     error_tag             :: atom(),   % ex tcp_error
                     passive_tag           :: atom(),   % ex tcp_passive
                     host                  :: string() | inet:ip_address(),
                     port                  :: integer(),
                     socket                :: port() | tuple(), %% TODO: dtls socket
                     cert_db               :: reference() | 'undefined',
                     session_cache         :: db_handle(),
                     session_cache_cb      :: atom(),
                     crl_db                :: term(),
                     file_ref_db          :: db_handle(),
                     cert_db_ref          :: certdb_ref() | 'undefined',
                     tracker              :: pid() | 'undefined' %% Tracker process for listen socket
                    }).

-record(handshake_env, {
                        client_hello_version  :: ssl_record:ssl_version() | 'undefined',
                        unprocessed_handshake_events = 0    :: integer(),
                        tls_handshake_history :: ssl_handshake:ssl_handshake_history() | secret_printout()
                                               | 'undefined',
                        expecting_finished =                  false ::boolean(),
                        renegotiation        :: undefined | {boolean(), From::term() | internal | peer},
                        allow_renegotiate = true                    ::boolean(),
                        %% Ext handling
                        hello,                %%:: #client_hello{} | #server_hello{}            
                        sni_hostname = undefined,
                        expecting_next_protocol_negotiation = false ::boolean(),
                        next_protocol = undefined                   :: undefined | binary(),
                        alpn = undefined,     %% Used in TLS 1.3
                        negotiated_protocol,
                        hashsign_algorithm = {undefined, undefined},
                        cert_hashsign_algorithm = {undefined, undefined},
                        %% key exchange
                        kex_algorithm         :: ssl:kex_algo(),  
                        kex_keys  :: {PublicKey :: binary(), PrivateKey :: binary()} | #'ECPrivateKey'{} |  undefined |  secret_printout(),        
                        diffie_hellman_params:: #'DHParameter'{} | undefined | secret_printout(),
                        srp_params           :: #srp_user{} | secret_printout() | 'undefined',
                        public_key_info      :: ssl_handshake:public_key_info() | 'undefined',
                        premaster_secret     :: binary() | secret_printout() | 'undefined',
                        server_psk_identity         :: binary() | 'undefined'  % server psk identity hint
                       }).

-record(connection_env, { 
                          user_application      :: {Monitor::reference(), User::pid()},
                          downgrade,
                          terminated = false                          ::boolean() | closed,  
                          negotiated_version    :: ssl_record:ssl_version() | 'undefined',
                          erl_dist_handle = undefined :: erlang:dist_handle() | 'undefined',
                          private_key          :: public_key:private_key() | secret_printout() | 'undefined'
                        }).

-record(state, {
                static_env            :: #static_env{},
                connection_env        :: #connection_env{} | secret_printout(),
                ssl_options           :: ssl_options(),
                socket_options        :: #socket_options{},

                %% Hanshake %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                handshake_env         :: #handshake_env{} | secret_printout(),
                %% Buffer of TLS/DTLS records, used during the TLS
                %% handshake to when possible pack more than one TLS
                %% record into the underlaying packet
                %% format. Introduced by DTLS - RFC 4347.  The
                %% mecahnism is also usefull in TLS although we do not
                %% need to worry about packet loss in TLS. In DTLS we
                %% need to track DTLS handshake seqnr
                flight_buffer = []   :: list() | map(),  
                client_certificate_requested = false :: boolean(),
                protocol_specific = #{}      :: map(),
                session               :: #session{} | secret_printout(),
                key_share,
                %% Data shuffling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                connection_states     :: ssl_record:connection_states() | secret_printout(),
                protocol_buffers      :: term() | secret_printout() , %% #protocol_buffers{} from tls_record.hrl or dtls_recor.hr
                user_data_buffer     :: undefined | {[binary()],non_neg_integer(),[binary()]} | secret_printout(),
                bytes_to_read        :: undefined | integer(), %% bytes to read in passive mode
                %% recv and start handling
                start_or_recv_from   :: term(),
                log_level
               }).

-define(DEFAULT_DIFFIE_HELLMAN_PARAMS,
	#'DHParameter'{prime = ?DEFAULT_DIFFIE_HELLMAN_PRIME,
		       base = ?DEFAULT_DIFFIE_HELLMAN_GENERATOR}).
-define(WAIT_TO_ALLOW_RENEGOTIATION, 12000).


%%----------------------------------------------------------------------
%% TLS 1.3
%%----------------------------------------------------------------------

%% TLS 1.3 uses the same state record with the following differences:
%%
%% state :: record()
%%
%%   session_cache                - not implemented
%%   session_cache_cb             - not implemented
%%   crl_db                       - not implemented
%%   client_hello_version         - Bleichenbacher mitigation in TLS 1.2
%%   client_certificate_requested - Built into TLS 1.3 state machine
%%   key_algorithm                - not used
%%   diffie_hellman_params        - used in TLS 1.2 ECDH key exchange
%%   diffie_hellman_keys          - used in TLS 1.2 ECDH key exchange
%%   psk_identity                 - not used
%%   srp_params                   - not used, no srp extension in TLS 1.3
%%   srp_keys                     - not used, no srp extension in TLS 1.3
%%   premaster_secret             - not used
%%   renegotiation                - TLS 1.3 forbids renegotiation
%%   hello                        - used in user_hello, handshake continue
%%   allow_renegotiate            - TLS 1.3 forbids renegotiation
%%   expecting_next_protocol_negotiation - ALPN replaced NPN, depricated in TLS 1.3
%%   expecting_finished           - not implemented, used by abbreviated
%%   next_protocol                - ALPN replaced NPN, depricated in TLS 1.3
%%
%% connection_state :: map()
%%
%%   compression_state            - not used
%%   mac_secret                   - not used
%%   sequence_number              - not used
%%   secure_renegotiation         - not used, no renegotiation_info in TLS 1.3
%%   client_verify_data           - not used, no renegotiation_info in TLS 1.3
%%   server_verify_data           - not used, no renegotiation_info in TLS 1.3
%%   beast_mitigation             - not used
%%
%% security_parameters :: map()
%%
%%   cipher_type                  - TLS 1.3 uses only AEAD ciphers
%%   iv_size                      - not used
%%   key_size                     - not used
%%   key_material_length          - not used
%%   expanded_key_material_length - used in SSL 3.0
%%   mac_algorithm                - not used
%%   prf_algorithm                - not used
%%   hash_size                    - not used
%%   compression_algorithm        - not used
%%   master_secret                - used for multiple secret types in TLS 1.3
%%   client_random                - not used
%%   server_random                - not used
%%   exportable                   - not used
%%
%% cipher_state :: record()
%%   nonce - used for sequence_number

-endif. % -ifdef(ssl_connection).
