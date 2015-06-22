%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2014. All Rights Reserved.
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

-ifndef(ssl_api).
-define(ssl_api, true).

-include("ssl_cipher.hrl").

%% Visible in API
-export_type([connect_option/0, listen_option/0, ssl_option/0, transport_option/0,
	      prf_random/0, sslsocket/0]).


%% Looks like it does for backwards compatibility reasons
-record(sslsocket, {fd = nil, pid = nil}).


-type sslsocket()                :: #sslsocket{}.
-type connect_option()           :: socket_connect_option() | ssl_option() | transport_option().
-type socket_connect_option()    :: gen_tcp:connect_option().
-type listen_option()            :: socket_listen_option() | ssl_option() | transport_option().
-type socket_listen_option()     :: gen_tcp:listen_option().

-type ssl_option()    :: {versions, ssl_record:ssl_atom_version()} |
			 {verify, verify_type()} |
			 {verify_fun, {fun(), InitialUserState::term()}} |
                         {fail_if_no_peer_cert, boolean()} | {depth, integer()} |
                         {cert, Der::binary()} | {certfile, path()} | {key, Der::binary()} |
                         {keyfile, path()} | {password, string()} | {cacerts, [Der::binary()]} |
                         {cacertfile, path()} | {dh, Der::binary()} | {dhfile, path()} |
                         {user_lookup_fun, {fun(), InitialUserState::term()}} |
                         {psk_identity, string()} |
                         {srp_identity, {string(), string()}} |
                         {ciphers, ciphers()} | {ssl_imp, ssl_imp()} | {reuse_sessions, boolean()} |
                         {reuse_session, fun()} | {hibernate_after, integer()|undefined} |
                         {alpn_advertised_protocols, [binary()]} |
                         {alpn_preferred_protocols, [binary()]} |
                         {next_protocols_advertised, list(binary())} |
                         {client_preferred_next_protocols, binary(), client | server, list(binary())}.

-type verify_type()  :: verify_none | verify_peer.
-type path()         :: string().
-type ciphers()      :: [ssl_cipher:erl_cipher_suite()] |
			string(). % (according to old API)
-type ssl_imp()      :: new | old.

-type transport_option() :: {cb_info, {CallbackModule::atom(), DataTag::atom(),
				       ClosedTag::atom(), ErrTag::atom()}}.
-type prf_random() :: client_random | server_random.

-endif. % -ifdef(ssl_api).
