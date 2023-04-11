%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2023. All Rights Reserved.
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

-include_lib("kernel/include/logger.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(CLIENT_ROLE, client).
-define(SERVER_ROLE, server).

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
-define(ALL_AVAILABLE_VERSIONS, ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', tlsv1]).
-define(ALL_AVAILABLE_DATAGRAM_VERSIONS, ['dtlsv1.2', dtlsv1]).
%% Defines the default versions when not specified by an ssl option.
-define(ALL_SUPPORTED_VERSIONS, ['tlsv1.3', 'tlsv1.2']).
-define(MIN_SUPPORTED_VERSIONS, ['tlsv1.1']).

%% Versions allowed in TLSCiphertext.version (TLS 1.2 and prior) and
%% TLSCiphertext.legacy_record_version (TLS 1.3).
%% TLS 1.3 sets TLSCiphertext.legacy_record_version to 0x0303 for all records
%% generated other than an than an initial ClientHello, where it MAY also be 0x0301.
%% Thus, the allowed range is limited to 0x0300 - 0x0303.
-define(ALL_TLS_RECORD_VERSIONS, ['tlsv1.2', 'tlsv1.1', tlsv1]).

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

%% https://tools.ietf.org/html/rfc8446#section-5.5
%% Limits on Key Usage
%% http://www.isg.rhul.ac.uk/~kp/TLS-AEbounds.pdf
%% Number of records * Record length
%% 2^24.5 * 2^14 = 2^38.5
-define(KEY_USAGE_LIMIT_AES_GCM, 388736063997).

-define(DEFAULT_MAX_EARLY_DATA_SIZE, 16384).

-define('TLS-1_3_ONLY_OPTIONS', [anti_replay,
                                 certificate_authorities,
                                 cookie,
                                 early_data,
                                 key_update_at,
                                 middlebox_comp_mode,
                                 session_tickets,
                                 supported_groups,
                                 use_ticket]).
-define('FROM_TLS-1_2_ONLY_OPTIONS', [signature_algs,
                                      signature_algs_cert]).
-define('PRE_TLS-1_3_ONLY_OPTIONS', [client_renegotiation,
                                     dh_file,
                                     eccs,
                                     fallback,
                                     secure_renegotiate,
                                     psk_identity,
                                     reuse_session,
                                     reuse_sessions,
                                     srp_identity,
                                     user_lookup_fun
                                    ]).
-define('TLS-1_0_ONLY_OPTIONS', [padding_check,
                                 beast_mitigation]).

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


-define(SSL_LOG(Level, Descr, Reason),
        fun() ->
                case get(log_level) of
                    undefined ->
                        %% Use debug here, i.e. log everything and let loggers
                        %% log_level decide if it should be logged
                        ssl_logger:log(Level, debug,
                                       #{description => Descr, reason => Reason},
                                       ?LOCATION);
                    __LogLevel__ ->
                        ssl_logger:log(Level, __LogLevel__,
                                       #{description => Descr, reason => Reason},
                                       ?LOCATION)
                end
        end()).


%% Internal ticket data record holding pre-processed ticket data.
-record(ticket_data,
        {key,                  %% key in client ticket store
         pos,                  %% ticket position in binders list
         identity,             %% opaque ticket binary
         psk,                  %% pre-shared key
         nonce,                %% ticket nonce
         cipher_suite,         %% cipher suite - hash, bulk cipher algorithm
         max_size              %% max early data size allowed by this ticket
        }).

-define(DEFAULT_DEPTH, 10).
-define(DEFAULT_OCSP_STAPLING, false).
-define(DEFAULT_OCSP_NONCE, true).
-define(DEFAULT_OCSP_RESPONDER_CERTS, []).
-endif. % -ifdef(ssl_internal).
