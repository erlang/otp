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

-record(ssl_options, {
	  protocol    :: tls | dtls | 'undefined',
	  versions    :: [ssl_record:ssl_version()] | 'undefined', %% ssl_record:atom_version() in API
	  verify      :: verify_none | verify_peer | 'undefined',
	  verify_fun,  %%:: fun(CertVerifyErrors::term()) -> boolean(),
	  partial_chain       :: fun() | 'undefined',
	  fail_if_no_peer_cert ::  boolean() | 'undefined',
	  verify_client_once   ::  boolean() | 'undefined',
	  %% fun(Extensions, State, Verify, AccError) ->  {Extensions, State, AccError}
	  validate_extensions_fun, 
	  depth                :: integer() | 'undefined',
	  certfile             :: binary() | 'undefined',
	  cert                 :: public_key:der_encoded() | secret_printout() | 'undefined',
	  keyfile              :: binary() | 'undefined',
	  key	               :: {'RSAPrivateKey' | 'DSAPrivateKey' | 'ECPrivateKey' | 'PrivateKeyInfo' | 'undefined',
                                   public_key:der_encoded()} | map()  %%map() -> ssl:key() how to handle dialyzer?
                                | secret_printout() | 'undefined',
	  password	       :: string() | secret_printout() | 'undefined',
	  cacerts              :: [public_key:der_encoded()] | secret_printout() | 'undefined',
	  cacertfile           :: binary() | 'undefined',
	  dh                   :: public_key:der_encoded() | secret_printout() | 'undefined',
	  dhfile               :: binary() | secret_printout() | 'undefined',
	  user_lookup_fun,  % server option, fun to lookup the user
	  psk_identity         :: binary() | secret_printout() | 'undefined',
	  srp_identity,  % client option {User, Password}
	  ciphers,    % 
	  %% Local policy for the server if it want's to reuse the session
	  %% or not. Defaluts to allways returning true.
	  %% fun(SessionId, PeerCert, Compression, CipherSuite) -> boolean()
	  reuse_session        :: fun() | binary() | undefined, %% Server side is a fun()
	  %% If false sessions will never be reused, if true they
	  %% will be reused if possible.
	  reuse_sessions       :: boolean() | save | 'undefined',  %% Only client side can use value save
	  renegotiate_at,
	  secure_renegotiate,
	  client_renegotiation,
	  %% undefined if not hibernating, or number of ms of
	  %% inactivity after which ssl_connection will go into
	  %% hibernation
	  hibernate_after      :: timeout() | 'undefined',
	  %% This option should only be set to true by inet_tls_dist
	  erl_dist = false     :: boolean(),
          alpn_advertised_protocols = undefined :: [binary()] | undefined,
          alpn_preferred_protocols = undefined  :: [binary()] | undefined,
	  next_protocols_advertised = undefined :: [binary()] | undefined,
	  next_protocol_selector = undefined,  %% fun([binary()]) -> binary())
	  log_level = notice :: atom(),
	  server_name_indication = undefined,
	  sni_hosts  :: [{inet:hostname(), [tuple()]}] | 'undefined',
	  sni_fun :: function() | undefined,
	  %% Should the server prefer its own cipher order over the one provided by
	  %% the client?
	  honor_cipher_order = false :: boolean(),
	  padding_check = true       :: boolean(),
	  %%Should we use 1/n-1 or 0/n splitting to mitigate BEAST, or disable
	  %%mitigation entirely?
	  beast_mitigation = one_n_minus_one :: one_n_minus_one | zero_n | disabled,
	  fallback = false           :: boolean(),
	  crl_check                  :: boolean() | peer | best_effort | 'undefined',
	  crl_cache,
	  signature_algs,
	  signature_algs_cert,
	  eccs,
	  supported_groups,  %% RFC 8422, RFC 8446
	  honor_ecc_order            :: boolean() | 'undefined',
          max_handshake_size         :: integer() | 'undefined',
          handshake,
          customize_hostname_check
    %%                 ,
      %%    save_session               :: boolean()            
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
		 emulated,          %% Emulated option list or "inherit_tracker" pid
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





