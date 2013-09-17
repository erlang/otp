
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

-record(state, {
          role                :: client | server,
	  user_application    :: {Monitor::reference(), User::pid()},
          transport_cb        :: atom(),   % callback module
          data_tag            :: atom(),   % ex tcp.
	  close_tag           :: atom(),   % ex tcp_closed
	  error_tag           :: atom(),   % ex tcp_error
          host,               % string() | ipadress()
          port                :: integer(),
          socket,             % socket()
          ssl_options,        % #ssl_options{}
          socket_options,     % #socket_options{}
          connection_states,  % #connection_states{} from ssl_record.hrl
	  protocol_buffers,
          tls_handshake_history, % tls_handshake_history()
	  cert_db,              %
          session,              % #session{} from tls_handshake.hrl
	  session_cache,        %
	  session_cache_cb,     %
          negotiated_version,   % tls_version()
          client_certificate_requested = false,
	  key_algorithm,       % atom as defined by cipher_suite
	  hashsign_algorithm = {undefined, undefined},
	  cert_hashsign_algorithm,
          public_key_info,     % PKIX: {Algorithm, PublicKey, PublicKeyParams}
          private_key,         % PKIX: #'RSAPrivateKey'{}
	  diffie_hellman_params, % PKIX: #'DHParameter'{} relevant for server side
	  diffie_hellman_keys, % {PublicKey, PrivateKey}
	  psk_identity,        % binary() - server psk identity hint
	  srp_params,          % #srp_user{}
	  srp_keys,            % {PublicKey, PrivateKey}
          premaster_secret,    %
	  file_ref_db,         % ets()
          cert_db_ref,         % ref()
          bytes_to_read,       % integer(), # bytes to read in passive mode
          user_data_buffer,    % binary()
	  renegotiation,       % {boolean(), From | internal | peer}
	  start_or_recv_from,  % "gen_fsm From"
	  timer,               % start_or_recv_timer
	  send_queue,          % queue()
	  terminated = false ::boolean(),
	  allow_renegotiate = true ::boolean(),
          expecting_next_protocol_negotiation = false :: boolean(),
          next_protocol = undefined :: undefined | binary(),
	  client_ecc          % {Curves, PointFmt}
	 }).

-define(DEFAULT_DIFFIE_HELLMAN_PARAMS,
	#'DHParameter'{prime = ?DEFAULT_DIFFIE_HELLMAN_PRIME,
		       base = ?DEFAULT_DIFFIE_HELLMAN_GENERATOR}).
-define(WAIT_TO_ALLOW_RENEGOTIATION, 12000).

-endif. % -ifdef(ssl_connection).
