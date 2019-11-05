%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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

-ifndef(ssl_internal).
-define(ssl_internal, true).

-include_lib("public_key/include/public_key.hrl"). 

-define(VSN, "8.2.6").
-define(SECRET_PRINTOUT, "***").

-type reason()            :: any().
-type reply()             :: any().
-type msg()               :: any().
-type from()              :: any().
-type certdb_ref()        :: reference().
-type db_handle()         :: any().
-type der_cert()          :: binary().
-type issuer()            :: tuple().
-type serialnumber()      :: integer().
-type cert_key()          :: {reference(), integer(), issuer()}.
-type secret_printout()   :: list().

%% basic binary constructors
-define(BOOLEAN(X),  X:8/unsigned-big-integer).
-define(BYTE(X),     X:8/unsigned-big-integer).
-define(UINT16(X),   X:16/unsigned-big-integer).
-define(UINT24(X),   X:24/unsigned-big-integer).
-define(UINT32(X),   X:32/unsigned-big-integer).
-define(UINT48(X),   X:48/unsigned-big-integer).
-define(UINT64(X),   X:64/unsigned-big-integer).
-define(STRING(X),   ?UINT32((size(X))), (X)/binary).

-define(byte(X),   << ?BYTE(X) >> ).
-define(uint16(X), << ?UINT16(X) >> ).
-define(uint24(X), << ?UINT24(X) >> ).
-define(uint32(X), << ?UINT32(X) >> ).
-define(uint48(X), << ?UINT48(X) >> ).
-define(uint64(X), << ?UINT64(X) >> ).

-define(CDR_MAGIC, "GIOP").
-define(CDR_HDR_SIZE, 12).
-define(INTERNAL_ACTIVE_N, 100).
-define(DEPTH, 20).

-define(DEFAULT_TIMEOUT, 5000).
-define(NO_DIST_POINT, "http://dummy/no_distribution_point").
-define(NO_DIST_POINT_PATH, "dummy/no_distribution_point").

%% Common enumerate values in for SSL-protocols 
-define(NULL, 0).
-define(TRUE, 0).
-define(FALSE, 1).

%% sslv3 is considered insecure due to lack of padding check (Poodle attack)
%% Keep as interop with legacy software but do not support as default
%% tlsv1.0 and tlsv1.1 is now also considered legacy
%% tlsv1.3 is under development (experimental).
-define(ALL_AVAILABLE_VERSIONS, ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', tlsv1, sslv3]).
-define(ALL_AVAILABLE_DATAGRAM_VERSIONS, ['dtlsv1.2', dtlsv1]).
%% Defines the default versions when not specified by an ssl option.
-define(ALL_SUPPORTED_VERSIONS, ['tlsv1.2']).
-define(MIN_SUPPORTED_VERSIONS, ['tlsv1.1']).

%% Versions allowed in TLSCiphertext.version (TLS 1.2 and prior) and
%% TLSCiphertext.legacy_record_version (TLS 1.3).
%% TLS 1.3 sets TLSCiphertext.legacy_record_version to 0x0303 for all records
%% generated other than an than an initial ClientHello, where it MAY also be 0x0301.
%% Thus, the allowed range is limited to 0x0300 - 0x0303.
-define(ALL_TLS_RECORD_VERSIONS, ['tlsv1.2', 'tlsv1.1', tlsv1, sslv3]).

-define(ALL_DATAGRAM_SUPPORTED_VERSIONS, ['dtlsv1.2']).
-define(MIN_DATAGRAM_SUPPORTED_VERSIONS, [dtlsv1]).

%% TLS 1.3 - Section 4.1.3
%%
%% If negotiating TLS 1.2, TLS 1.3 servers MUST set the last eight bytes
%% of their Random value to the bytes:
%%
%%   44 4F 57 4E 47 52 44 01
%%
%% If negotiating TLS 1.1 or below, TLS 1.3 servers MUST and TLS 1.2
%% servers SHOULD set the last eight bytes of their Random value to the
%% bytes:
%%
%%   44 4F 57 4E 47 52 44 00
-define(RANDOM_OVERRIDE_TLS12, <<16#44,16#4F,16#57,16#4E,16#47,16#52,16#44,16#01>>).
-define(RANDOM_OVERRIDE_TLS11, <<16#44,16#4F,16#57,16#4E,16#47,16#52,16#44,16#00>>).

-define('24H_in_msec', 86400000).
-define('24H_in_sec', 86400).


%% This map stores all supported options with default values and
%% list of dependencies:
%%   #{<option> => {<default_value>, [<option>]},
%%     ...}
-define(RULES,
        #{
          alpn_advertised_protocols  => {undefined, [versions]},
          alpn_preferred_protocols   => {undefined, [versions]},
          beast_mitigation           => {one_n_minus_one, [versions]},
          cacertfile                 => {undefined, [versions,
                                                     verify_fun,
                                                     cacerts]},
          cacerts                    => {undefined, [versions]},
          cert                       => {undefined, [versions]},
          certfile                   => {<<>>,      [versions]},
          ciphers                    => {[],        [versions]},
          client_renegotiation       => {undefined, [versions]},
          crl_cache                  => {{ssl_crl_cache, {internal, []}}, [versions]},
          crl_check                  => {false,     [versions]},
          customize_hostname_check   => {[],        [versions]},
          depth                      => {1,         [versions]},
          dh                         => {undefined, [versions]},
          dhfile                     => {undefined, [versions]},
          eccs                       => {undefined, [versions]},
          erl_dist                   => {false,     [versions]},
          fail_if_no_peer_cert       => {false,     [versions]},
          fallback                   => {false,     [versions]},
          handshake                  => {full,      [versions]},
          hibernate_after            => {infinity,  [versions]},
          honor_cipher_order         => {false,     [versions]},
          honor_ecc_order            => {undefined, [versions]},
          key                        => {undefined, [versions]},
          keyfile                    => {undefined, [versions,
                                                     certfile]},
          log_level                  => {notice,    [versions]},
          max_handshake_size         => {?DEFAULT_MAX_HANDSHAKE_SIZE, [versions]},
          next_protocol_selector     => {undefined, [versions]},
          next_protocols_advertised  => {undefined, [versions]},
          padding_check              => {true,      [versions]},
          partial_chain              => {fun(_) -> unknown_ca end, [versions]},
          password                   => {"",        [versions]},
          protocol                   => {tls,       []},
          psk_identity               => {undefined, [versions]},
          renegotiate_at             => {?DEFAULT_RENEGOTIATE_AT, [versions]},
          reuse_session              => {undefined, [versions]},
          reuse_sessions             => {true,      [versions]},
          anti_replay                => {undefined, [versions, session_tickets]},
          secure_renegotiate         => {true,      [versions]},
          server_name_indication     => {undefined, [versions]},
          session_tickets            => {disabled,     [versions]},
          signature_algs             => {undefined, [versions]},
          signature_algs_cert        => {undefined, [versions]},
          sni_fun                    => {undefined, [versions,
                                                     sni_hosts]},
          sni_hosts                  => {[],        [versions]},
          srp_identity               => {undefined, [versions]},
          supported_groups           => {undefined, [versions]},
          use_ticket                 => {undefined, [versions]},
          user_lookup_fun            => {undefined, [versions]},
          validate_extensions_fun    => {undefined, [versions]},
          verify                     => {verify_none, [versions,
                                                       fail_if_no_peer_cert,
                                                       partial_chain,
                                                       verify_client_once]},
          verify_client_once         => {false,     [versions]},
          verify_fun                 =>
              {
               {fun(_,{bad_cert, _}, UserState) ->
                        {valid, UserState};
                   (_,{extension, #'Extension'{critical = true}}, UserState) ->
                        %% This extension is marked as critical, so
                        %% certificate verification should fail if we don't
                        %% understand the extension.  However, this is
                        %% `verify_none', so let's accept it anyway.
                        {valid, UserState};
                   (_,{extension, _}, UserState) ->
                        {unknown, UserState};
                   (_, valid, UserState) ->
                        {valid, UserState};
                   (_, valid_peer, UserState) ->
                        {valid, UserState}
                end, []},
               [versions, verify]},
          versions                   => {[], [protocol]}
         }).

-record(socket_options,
	{
	  mode   = list, 
	  packet = 0,
	  packet_size = 0,
	  header = 0,
	  active = true
	 }).

-record(config, {ssl,               %% SSL parameters
		 inet_user,         %% User set inet options
		 emulated,          %% Emulated option list or 
                 trackers, 
		 dtls_handler,
		 inet_ssl,          %% inet options for internal ssl socket
		 transport_info,                 %% Callback info
		 connection_cb
		}).

-type state_name()           :: hello | abbreviated | certify | cipher | connection.
-type gen_fsm_state_return() :: {next_state, state_name(), any()} |
				{next_state, state_name(), any(), timeout()} |
				{stop, any(), any()}.
-type ssl_options()          :: map().

-endif. % -ifdef(ssl_internal).





