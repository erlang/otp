%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2015. All Rights Reserved.
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

-record(state, {
          role                  :: client | server,
	  user_application      :: {Monitor::reference(), User::pid()},
          transport_cb          :: atom(),   % callback module
	  protocol_cb           :: tls_connection | dtls_connection,
          data_tag              :: atom(),   % ex tcp.
	  close_tag             :: atom(),   % ex tcp_closed
	  error_tag             :: atom(),   % ex tcp_error
          host                  :: string() | inet:ip_address(),
          port                  :: integer(),
          socket                :: port(),
          ssl_options           :: #ssl_options{},
          socket_options        :: #socket_options{},
          connection_states     :: #connection_states{} | secret_printout(),
	  protocol_buffers      :: term() | secret_printout() , %% #protocol_buffers{} from tls_record.hrl or dtls_recor.hrl
          tls_handshake_history :: ssl_handshake:ssl_handshake_history() | secret_printout()
                                 | 'undefined',
	  cert_db               :: reference() | 'undefined',
          session               :: #session{} | secret_printout(),
	  session_cache         :: db_handle(),
	  session_cache_cb      :: atom(),
	  crl_db                :: term(), 
          negotiated_version    :: ssl_record:ssl_version() | 'undefined',
          client_certificate_requested = false :: boolean(),
	  key_algorithm         :: ssl_cipher:key_algo(),
	  hashsign_algorithm = {undefined, undefined},
	  cert_hashsign_algorithm,
          public_key_info      :: ssl_handshake:public_key_info() | 'undefined',
          private_key          :: public_key:private_key() | secret_printout() | 'undefined',
	  diffie_hellman_params:: #'DHParameter'{} | undefined | secret_printout(),
	  diffie_hellman_keys  :: {PublicKey :: binary(), PrivateKey :: binary()} | #'ECPrivateKey'{} |  undefined |  secret_printout(),  
	  psk_identity         :: binary() | 'undefined', % server psk identity hint
	  srp_params           :: #srp_user{} | secret_printout() | 'undefined',
	  srp_keys             ::{PublicKey :: binary(), PrivateKey :: binary()} | secret_printout() | 'undefined',
          premaster_secret     :: binary() | secret_printout() | 'undefined',
	  file_ref_db          :: db_handle(),
          cert_db_ref          :: certdb_ref() | 'undefined',
          bytes_to_read        :: undefined | integer(), %% bytes to read in passive mode
          user_data_buffer     :: undefined | binary() | secret_printout(), 
	  renegotiation        :: undefined | {boolean(), From::term() | internal | peer},
	  start_or_recv_from   :: term(),
	  timer                :: undefined | reference(), % start_or_recive_timer
	  %%send_queue           :: queue:queue(),
	  terminated = false                          ::boolean(),
	  allow_renegotiate = true                    ::boolean(),
          expecting_next_protocol_negotiation = false ::boolean(),
	  expecting_finished =                  false ::boolean(),
          negotiated_protocol = undefined             :: undefined | binary(),
	  client_ecc,          % {Curves, PointFmt}
	  tracker              :: pid() | 'undefined', %% Tracker process for listen socket
	  sni_hostname = undefined,
	  downgrade,
	  flight_buffer = []   :: list()  %% Buffer of TLS/DTLS records, used during the TLS handshake
				          %% to when possible pack more than on TLS record into the 
                                          %% underlaying packet format. Introduced by DTLS - RFC 4347.
				          %% The mecahnism is also usefull in TLS although we do not
				          %% need to worry about packet loss in TLS.
	 }).

-define(DEFAULT_DIFFIE_HELLMAN_PARAMS,
	#'DHParameter'{prime = ?DEFAULT_DIFFIE_HELLMAN_PRIME,
		       base = ?DEFAULT_DIFFIE_HELLMAN_GENERATOR}).
-define(WAIT_TO_ALLOW_RENEGOTIATION, 12000).

-endif. % -ifdef(ssl_connection).
