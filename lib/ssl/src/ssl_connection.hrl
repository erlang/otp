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
-include("ssl_alert.hrl").
-include_lib("public_key/include/public_key.hrl").

-record(static_env, {
                     role                  :: client | server,
                     transport_cb          :: atom(),   % callback module
                     protocol_cb           :: tls_gen_connection | dtls_gen_connection,
                     data_tag              :: atom(),   % ex tcp.
                     close_tag             :: atom(),   % ex tcp_closed
                     error_tag             :: atom(),   % ex tcp_error
                     passive_tag           :: atom(),   % ex tcp_passive
                     host                  :: string() | inet:ip_address(),
                     port                  :: integer(),
                     socket                :: port() | tuple(), %% TODO: dtls socket
                     cert_db               :: reference() | 'undefined',
                     session_cache         :: ssl_manager:db_handle(),
                     session_cache_cb      :: atom(),
                     crl_db                :: term(),
                     file_ref_db          :: ssl_manager:db_handle(),
                     cert_db_ref          :: ssl_manager:certdb_ref() | 'undefined',
                     trackers              :: [{atom(), pid()}] | 'undefined' %% Tracker process for listen socket
                    }).


-record(handshake_env, {
                        client_hello_version  :: ssl_record:ssl_version() | 'undefined', %% Legacy client hello
                        unprocessed_handshake_events = 0    :: integer(),
                        tls_handshake_history :: ssl_handshake:ssl_handshake_history() | term()
                                               | 'undefined',
                        expecting_finished =                  false ::boolean(),
                        renegotiation        :: undefined | {boolean(), From::term() | internal | peer},
                        resumption = false   :: boolean(),  %% TLS 1.3
                        change_cipher_spec_sent = false :: boolean(),  %% TLS 1.3
                        sni_guided_cert_selection = false :: boolean(), %% TLS 1.3
                        early_data_accepted = false :: boolean(), %% TLS 1.3
                        allow_renegotiate = true                    ::boolean(),
                        %% Ext handling
                        %% continue_status reflects handling of the option handshake that is either full or
                        %% hello (will pause at hello message to allow user to act on hello extensions)
                        continue_status,  %% full | pause | {pause, ClientVersionsExt} | continue
                        sni_hostname = undefined,
                        max_frag_enum :: undefined | {max_frag_enum, integer()},
                        expecting_next_protocol_negotiation = false ::boolean(),
                        next_protocol = undefined                   :: undefined | binary(),
                        alpn = undefined,     %% Used in TLS 1.3
                        negotiated_protocol,
                        hashsign_algorithm = {undefined, undefined},
                        cert_hashsign_algorithm = {undefined, undefined},
                        %% key exchange
                        kex_algorithm         :: ssl:kex_algo(),  
                        kex_keys  :: {PublicKey :: binary(), PrivateKey :: binary()} | #'ECPrivateKey'{} |  undefined |
                                     term(),
                        diffie_hellman_params:: #'DHParameter'{} | undefined | term(),
                        srp_params           :: #srp_user{} | term() | 'undefined',
                        public_key_info      :: ssl_handshake:public_key_info() | 'undefined',
                        premaster_secret     :: binary() | term() | 'undefined',
                        server_psk_identity         :: binary() | 'undefined',  % server psk identity hint
                        cookie_iv_shard         :: {binary(), binary()} %% IV, Shard
                                                 | 'undefined',
                        client_certificate_status = not_requested :: not_requested | requested |
                                                                     empty | needs_verifying | verified,
                        key_share,
                        %% Buffer of TLS/DTLS records, used during the TLS
                        %% handshake to when possible pack more than one TLS
                        %% record into the underlying packet
                        %% format. Introduced by DTLS - RFC 4347.  The
                        %% mechanism is also useful in TLS although we do not
                        %% need to worry about packet loss in TLS. In DTLS we
                        %% need to track DTLS handshake seqnr
                        flight_buffer = []   :: list() | map(),
                        stapling_state = #{configured => false,
                                           status => not_negotiated}
                       }).

-record(connection_env, { 
                          user_application      :: {Monitor::reference(), User::pid()},
                          downgrade             :: {NewController::pid(), From::gen_statem:from()} | 'undefined',
                          socket_terminated = false                          ::boolean(),
                          socket_tls_closed = false                          ::boolean() | #alert{},
                          negotiated_version    :: ssl_record:ssl_version() | 'undefined',
                          erl_dist_handle = undefined :: erlang:dist_handle() | 'undefined',
                          cert_key_alts  = undefined ::  #{eddsa => list(),
                                                           ecdsa => list(),
                                                           rsa_pss_pss => list(),
                                                           rsa => list(),
                                                           dsa => list()
                                                          } | list() | 'undefined'
                        }).

-record(recv, {
               from                   :: term(),                %% start or recv from
               bytes_to_read          :: undefined | integer()  %% bytes to read in passive mode
              }).

-record(state, {
                static_env            :: #static_env{},
                connection_env        :: #connection_env{} | ssl_gen_statem:secret_printout(),
                ssl_options           :: ssl_options(),
                socket_options        :: #socket_options{},

                %% Handshake %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                handshake_env         :: #handshake_env{} | ssl_gen_statem:secret_printout(),
                protocol_specific = #{}      :: map(),
                session               :: #session{} | ssl_gen_statem:secret_printout(),
                %% Data shuffling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                recv = #recv{}        :: #recv{},
                connection_states     :: ssl_record:connection_states() | ssl_gen_statem:secret_printout(),
                %% #protocol_buffers{} from tls_record.hrl or dtls_recor.hrl
                protocol_buffers      :: term() | ssl_gen_statem:secret_printout(),
                user_data_buffer     :: undefined | {[binary()],non_neg_integer(),[binary()]} | ssl_gen_statem:secret_printout()
               }).

-define(DEFAULT_DIFFIE_HELLMAN_PARAMS,
	#'DHParameter'{prime = ?DEFAULT_DIFFIE_HELLMAN_PRIME,
		       base = ?DEFAULT_DIFFIE_HELLMAN_GENERATOR}).
-define(WAIT_TO_ALLOW_RENEGOTIATION, 12000).


-define(STATE(StateName), (StateName = ?FUNCTION_NAME)).


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
%%   client_certificate_status    - only uses non_requested| requested
%%   key_algorithm                - only uses  not_requested and requested 
%%   diffie_hellman_params        - used in TLS 1.2 ECDH key exchange
%%   diffie_hellman_keys          - used in TLS 1.2 ECDH key exchange
%%   psk_identity                 - not used
%%   srp_params                   - not used, no srp extension in TLS 1.3
%%   srp_keys                     - not used, no srp extension in TLS 1.3
%%   premaster_secret             - not used
%%   renegotiation                - TLS 1.3 forbids renegotiation
%%   hello                        - used in user_hello, handshake continue
%%   allow_renegotiate            - TLS 1.3 forbids renegotiation
%%   expecting_next_protocol_negotiation - ALPN replaced NPN, deprecated in TLS 1.3
%%   expecting_finished           - not implemented, used by abbreviated
%%   next_protocol                - ALPN replaced NPN, deprecated in TLS 1.3
%%
%% connection_state :: map()
%%
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
%%   key_material_length          - not used
%%   mac_algorithm                - not used
%%   prf_algorithm                - not used
%%   hash_size                    - not used
%%   master_secret                - used for multiple secret types in TLS 1.3
%%   client_random                - not used
%%   server_random                - not used
%%
%% cipher_state :: record()
%%   nonce - used for sequence_number

-endif. % -ifdef(ssl_connection).
