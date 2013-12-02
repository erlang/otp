
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
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
          host                  :: string() | inet:ipaddress(),
          port                  :: integer(),
          socket                :: port(),
          ssl_options           :: #ssl_options{},
          socket_options        :: #socket_options{},
          connection_states     :: #connection_states{},
	  protocol_buffers      :: term(), %% #protocol_buffers{} from tls_record.hrl or dtls_recor.hrl
          tls_handshake_history ::tls_handshake_history(),
	  cert_db               :: reference(),
          session               :: #session{},
	  session_cache         :: db_handle(),
	  session_cache_cb      :: atom(),
          negotiated_version    :: tls_version(),
          client_certificate_requested = false :: boolean(),
	  key_algorithm         :: key_algo(),
	  hashsign_algorithm = {undefined, undefined},
	  cert_hashsign_algorithm,
          public_key_info      ::public_key_info(),
          private_key          ::public_key:private_key(),
	  diffie_hellman_params, % PKIX: #'DHParameter'{} relevant for server side
	  diffie_hellman_keys, % {PublicKey, PrivateKey}
	  psk_identity         :: binary(), % server psk identity hint
	  srp_params           :: #srp_user{},
	  srp_keys             ::{PublicKey :: binary(), PrivateKey :: binary()},
          premaster_secret     :: binary(),
	  file_ref_db          :: db_handle(),
          cert_db_ref          :: certdb_ref(),
          bytes_to_read        :: undefined | integer(), %% bytes to read in passive mode
          user_data_buffer     :: undefined | binary(),
	  renegotiation        :: undefined | {boolean(), From::term() | internal | peer},
	  start_or_recv_from   :: term(),
	  timer                :: undefined | reference(), % start_or_recive_timer
	  send_queue           :: queue(),
	  terminated = false                          ::boolean(),
	  allow_renegotiate = true                    ::boolean(),
          expecting_next_protocol_negotiation = false ::boolean(),
          next_protocol = undefined                   :: undefined | binary(),
	  client_ecc          % {Curves, PointFmt}
	 }).

-define(DEFAULT_DIFFIE_HELLMAN_PARAMS,
	#'DHParameter'{prime = ?DEFAULT_DIFFIE_HELLMAN_PRIME,
		       base = ?DEFAULT_DIFFIE_HELLMAN_GENERATOR}).
-define(WAIT_TO_ALLOW_RENEGOTIATION, 12000).

-endif. % -ifdef(ssl_connection).
