%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
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

%% Looks like it does for backwards compatibility reasons
-record(sslsocket, {fd = nil, pid = nil}).

-type reason()            :: term().
-type reply()             :: term().
-type msg()               :: term().
-type from()              :: term().
-type host()		  :: inet:ip_address() | inet:hostname().
-type session_id()        :: 0 | binary().
-type tls_version()       :: {integer(), integer()}.
-type tls_atom_version()  :: sslv3 | tlsv1 | 'tlsv1.1' | 'tlsv1.2'.
-type certdb_ref()        :: reference().
-type db_handle()         :: term().
-type key_algo()          :: null | rsa | dhe_rsa | dhe_dss | dh_anon.
-type der_cert()          :: binary().
-type private_key()       :: #'RSAPrivateKey'{} | #'DSAPrivateKey'{}.
-type issuer()            :: tuple().
-type serialnumber()      :: integer().
-type cert_key()          :: {reference(), integer(), issuer()}.

%% basic binary constructors
-define(BOOLEAN(X),  X:8/unsigned-big-integer).
-define(BYTE(X),     X:8/unsigned-big-integer).
-define(UINT16(X),   X:16/unsigned-big-integer).
-define(UINT24(X),   X:24/unsigned-big-integer).
-define(UINT32(X),   X:32/unsigned-big-integer).
-define(UINT64(X),   X:64/unsigned-big-integer).
-define(STRING(X),   ?UINT32((size(X))), (X)/binary).

-define(byte(X),   << ?BYTE(X) >> ).
-define(uint16(X), << ?UINT16(X) >> ).
-define(uint24(X), << ?UINT24(X) >> ).
-define(uint32(X), << ?UINT32(X) >> ).
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

-record(ssl_options, {
	  versions,   % 'tlsv1.2' | 'tlsv1.1' | tlsv1 | sslv3
	  verify,     %   verify_none | verify_peer
	  verify_fun, % fun(CertVerifyErrors) -> boolean()
	  fail_if_no_peer_cert, % boolean()
	  verify_client_once,  % boolean()
	  %% fun(Extensions, State, Verify, AccError) ->  {Extensions, State, AccError}
	  validate_extensions_fun, 
	  depth,      % integer()
	  certfile,   % file()
	  cert,       % der_encoded()
	  keyfile,    % file()
	  key,	      % der_encoded()
	  password,   % 
	  cacerts,    % [der_encoded()]
	  cacertfile, % file()
	  dh,         % der_encoded()
	  dhfile,     % file()
	  user_lookup_fun,  % server option, fun to lookup the user
	  psk_identity,  % binary
	  ciphers,    % 
	  %% Local policy for the server if it want's to reuse the session
	  %% or not. Defaluts to allways returning true.
	  %% fun(SessionId, PeerCert, Compression, CipherSuite) -> boolean()
	  reuse_session,  
	  %% If false sessions will never be reused, if true they
	  %% will be reused if possible.
	  reuse_sessions, % boolean()
	  renegotiate_at,
	  secure_renegotiate,
	  debug,
	  hibernate_after,% undefined if not hibernating,
                          % or number of ms of inactivity
			  % after which ssl_connection will
                          % go into hibernation
	  %% This option should only be set to true by inet_tls_dist
	  erl_dist = false,
	  next_protocols_advertised = undefined, %% [binary()],
	  next_protocol_selector = undefined  %% fun([binary()]) -> binary())
	  }).

-record(socket_options,
	{
	  mode   = list, 
	  packet = 0,
	  packet_size = 0,
	  header = 0,
	  active = true
	 }).

-endif. % -ifdef(ssl_internal).





