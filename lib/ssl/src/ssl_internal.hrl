%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2014. All Rights Reserved.
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

-ifndef(ssl_internal).
-define(ssl_internal, true).

-include_lib("public_key/include/public_key.hrl"). 

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

%% Common enumerate values in for SSL-protocols 
-define(NULL, 0).
-define(TRUE, 0).
-define(FALSE, 1).

-define(ALL_SUPPORTED_VERSIONS, ['tlsv1.2', 'tlsv1.1', tlsv1, sslv3]).
-define(MIN_SUPPORTED_VERSIONS, ['tlsv1.1', tlsv1, sslv3]).
-define(ALL_DATAGRAM_SUPPORTED_VERSIONS, ['dtlsv1.2', dtlsv1]).
-define(MIN_DATAGRAM_SUPPORTED_VERSIONS, ['dtlsv1.2', dtlsv1]).

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
	  cert                 :: public_key:der_encoded(),
	  keyfile              :: binary(),
	  key	               :: {'RSAPrivateKey' | 'DSAPrivateKey' | 'ECPrivateKey' | 'PrivateKeyInfo', public_key:der_encoded()},
	  password	       :: string(),
	  cacerts              :: [public_key:der_encoded()],
	  cacertfile           :: binary(),
	  dh                   :: public_key:der_encoded(),
	  dhfile               :: binary(),
	  user_lookup_fun,  % server option, fun to lookup the user
	  psk_identity         :: binary(),
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
	  %% undefined if not hibernating, or number of ms of
	  %% inactivity after which ssl_connection will go into
	  %% hibernation
	  hibernate_after      :: boolean(),
	  %% This option should only be set to true by inet_tls_dist
	  erl_dist = false     :: boolean(),
	  next_protocols_advertised = undefined, %% [binary()],
	  next_protocol_selector = undefined,  %% fun([binary()]) -> binary())
	  log_alert             :: boolean(),
	  server_name_indication = undefined,
	  %% Should the server prefer its own cipher order over the one provided by
	  %% the client?
	  honor_cipher_order = false
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





