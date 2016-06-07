%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2015. All Rights Reserved.
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

-define(SECRET_PRINTOUT, "***").

-type reason()            :: term().
-type reply()             :: term().
-type msg()               :: term().
-type from()              :: term().
-type host()		  :: inet:ip_address() | inet:hostname().
-type session_id()        :: 0 | binary().
-type certdb_ref()        :: reference().
-type db_handle()         :: term().
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

-define(DEFAULT_TIMEOUT, 5000).
-define(NO_DIST_POINT, "http://dummy/no_distribution_point").
-define(NO_DIST_POINT_PATH, "dummy/no_distribution_point").

%% Common enumerate values in for SSL-protocols 
-define(NULL, 0).
-define(TRUE, 0).
-define(FALSE, 1).

%% sslv3 is considered insecure due to lack of padding check (Poodle attack)
%% Keep as interop with legacy software but do not support as default 
-define(ALL_AVAILABLE_VERSIONS, ['tlsv1.2', 'tlsv1.1', tlsv1, sslv3]).
-define(ALL_SUPPORTED_VERSIONS, ['tlsv1.2', 'tlsv1.1', tlsv1]).
-define(MIN_SUPPORTED_VERSIONS, ['tlsv1.1', tlsv1]).
-define(ALL_DATAGRAM_SUPPORTED_VERSIONS, ['dtlsv1.2', dtlsv1]).
-define(MIN_DATAGRAM_SUPPORTED_VERSIONS, ['dtlsv1.2', dtlsv1]).

-define('24H_in_msec', 86400000).
-define('24H_in_sec', 86400).

-record(ssl_options, {
	  protocol    :: tls | dtls,
	  versions    :: [ssl_record:ssl_version()], %% ssl_record:atom_version() in API
	  verify      :: verify_none | verify_peer,
	  verify_fun,  %%:: fun(CertVerifyErrors::term()) -> boolean(),
	  partial_chain       :: fun(),
	  fail_if_no_peer_cert ::  boolean(),
	  verify_client_once   ::  boolean(),
	  %% fun(Extensions, State, Verify, AccError) ->  {Extensions, State, AccError}
	  validate_extensions_fun, 
	  depth                :: integer(),
	  certfile             :: binary(),
	  cert                 :: public_key:der_encoded() | secret_printout() | 'undefined',
	  keyfile              :: binary(),
	  key	               :: {'RSAPrivateKey' | 'DSAPrivateKey' | 'ECPrivateKey' | 'PrivateKeyInfo', public_key:der_encoded()} | secret_printout() | 'undefined',
	  password	       :: string() | secret_printout() | 'undefined',
	  cacerts              :: [public_key:der_encoded()] | secret_printout() | 'undefined',
	  cacertfile           :: binary(),
	  dh                   :: public_key:der_encoded() | secret_printout(),
	  dhfile               :: binary() | secret_printout() | 'undefined',
	  user_lookup_fun,  % server option, fun to lookup the user
	  psk_identity         :: binary() | secret_printout() | 'undefined',
	  srp_identity,  % client option {User, Password}
	  ciphers,    % 
	  %% Local policy for the server if it want's to reuse the session
	  %% or not. Defaluts to allways returning true.
	  %% fun(SessionId, PeerCert, Compression, CipherSuite) -> boolean()
	  reuse_session,  
	  %% If false sessions will never be reused, if true they
	  %% will be reused if possible.
	  reuse_sessions       :: boolean(),
	  renegotiate_at,
	  secure_renegotiate,
	  client_renegotiation,
	  %% undefined if not hibernating, or number of ms of
	  %% inactivity after which ssl_connection will go into
	  %% hibernation
	  hibernate_after      :: timeout(),
	  %% This option should only be set to true by inet_tls_dist
	  erl_dist = false     :: boolean(),
          alpn_advertised_protocols = undefined :: [binary()] | undefined ,
          alpn_preferred_protocols = undefined  :: [binary()] | undefined,
	  next_protocols_advertised = undefined :: [binary()] | undefined,
	  next_protocol_selector = undefined,  %% fun([binary()]) -> binary())
	  log_alert             :: boolean(),
	  server_name_indication = undefined,
	  sni_hosts  :: [{inet:hostname(), [tuple()]}],
	  sni_fun :: function() | undefined,
	  %% Should the server prefer its own cipher order over the one provided by
	  %% the client?
	  honor_cipher_order = false :: boolean(),
	  padding_check = true       :: boolean(),
	  %%Should we use 1/n-1 or 0/n splitting to mitigate BEAST, or disable
	  %%mitigation entirely?
	  beast_mitigation = one_n_minus_one :: one_n_minus_one | zero_n | disabled,
	  fallback = false           :: boolean(),
	  crl_check                  :: boolean() | peer | best_effort, 
	  crl_cache,
	  signature_algs,
	  v2_hello_compatible        :: boolean()
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
		 inet_ssl,          %% inet options for internal ssl socket
		 transport_info,                 %% Callback info
		 connection_cb
		}).


-type state_name()           :: hello | abbreviated | certify | cipher | connection.
-type gen_fsm_state_return() :: {next_state, state_name(), term()} |
				{next_state, state_name(), term(), timeout()} |
				{stop, term(), term()}.
-endif. % -ifdef(ssl_internal).





