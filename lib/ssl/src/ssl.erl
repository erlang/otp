%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2013. All Rights Reserved.
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

%%% Purpose : Backwards compatibility

-module(ssl).

-export([start/0, start/1, stop/0, transport_accept/1,
	 transport_accept/2, ssl_accept/1, ssl_accept/2, ssl_accept/3,
	 cipher_suites/0, cipher_suites/1, suite_definition/1,
	 close/1, shutdown/2,
	 connect/3, connect/2, connect/4, connection_info/1,
	 controlling_process/2, listen/2, peername/1, peercert/1,
	 recv/2, recv/3, send/2, getopts/2, setopts/2, sockname/1,
	 versions/0, session_info/1, format_error/1,
	 renegotiate/1, prf/5, clear_pem_cache/0, random_bytes/1, negotiated_next_protocol/1]).

-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_handshake.hrl").

-include_lib("public_key/include/public_key.hrl"). 

%% Visible in API
-export_type([connect_option/0, listen_option/0, ssl_option/0, transport_option/0,
	      erl_cipher_suite/0, %% From ssl_cipher.hrl 
	      tls_atom_version/0, %% From ssl_internal.hrl
	      prf_random/0, sslsocket/0]).

-type sslsocket()                :: #sslsocket{}.
-type connect_option()           :: socket_connect_option() | ssl_option() | transport_option().
-type socket_connect_option()    :: gen_tcp:connect_option().
-type listen_option()            :: socket_listen_option() | ssl_option() | transport_option().
-type socket_listen_option()     :: gen_tcp:listen_option().

-type ssl_option()    :: {verify, verify_type()} |
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
                        {next_protocols_advertised, list(binary())} |
                        {client_preferred_next_protocols, binary(), client | server, list(binary())}.

-type verify_type()  :: verify_none | verify_peer.
-type path()         :: string().
-type ciphers()      :: [erl_cipher_suite()] |
			string(). % (according to old API)
-type ssl_imp()      :: new | old.

-type transport_option() :: {cb_info, {CallbackModule::atom(), DataTag::atom(), 
				       ClosedTag::atom(), ErrTag::atom()}}.
-type prf_random() :: client_random | server_random.

%%--------------------------------------------------------------------
-spec start() -> ok  | {error, reason()}.
-spec start(permanent | transient | temporary) -> ok | {error, reason()}.
%%
%% Description: Utility function that starts the ssl, 
%% crypto and public_key applications. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
   tls:start().
start(Type) ->
    tls:start(Type).

stop() ->
   tls:stop().

connect(Socket, SslOptions) ->
    tls:connect(Socket, SslOptions).

connect(Socket, SslOptions0, TimeoutOrOpts) ->
    tls:connect(Socket, SslOptions0, TimeoutOrOpts).

connect(Host, Port, Options, Timeout) ->
    tls:connect(Host, Port, Options, Timeout).

listen(Port, Options) ->
    tls:listen(Port, Options).

transport_accept(ListenSocket) ->
    tls:transport_accept(ListenSocket).

transport_accept(ListenSocket, Timeout) ->
    tls:transport_accept(ListenSocket, Timeout).
   
ssl_accept(ListenSocket) ->
    tls:ssl_accept(ListenSocket, infinity).

ssl_accept(#sslsocket{} = Socket, Timeout) ->
    tls:ssl_accept(Socket, Timeout);
    
ssl_accept(ListenSocket, SslOptions)  when is_port(ListenSocket) -> 
    tls:ssl_accept(ListenSocket, SslOptions, infinity).

ssl_accept(Socket, SslOptions, Timeout) when is_port(Socket) -> 
    tls:ssl_accept(Socket, SslOptions, Timeout).

close(Socket) ->
    tls:close(Socket).

send(Socket, Data)  ->
    tls:send(Socket, Data).

recv(Socket, Length) ->
    tls:recv(Socket, Length, infinity).
recv(Socket, Length, Timeout) ->
    tls:recv(Socket, Length, Timeout).

controlling_process(Socket, NewOwner) ->
    tls:controlling_process(Socket, NewOwner).
    
connection_info(Socket) ->
    tls:connection_info(Socket).

peername(Socket) ->
    tls:peername(Socket).

peercert(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    case tls_connection:peer_certificate(Pid) of
	{ok, undefined} ->
	    {error, no_peercert};
        Result ->
	    Result
    end;
peercert(#sslsocket{pid = {Listen, _}}) when is_port(Listen) ->
    {error, enotconn}.

suite_definition(S) ->
    {KeyExchange, Cipher, Hash, _} = ssl_cipher:suite_definition(S),
    {KeyExchange, Cipher, Hash}.

negotiated_next_protocol(#sslsocket{pid = Pid}) ->
    tls_connection:negotiated_next_protocol(Pid).

%%%--------------------------------------------------------------------
-spec cipher_suites() -> [erl_cipher_suite()].
-spec cipher_suites(erlang | openssl | all ) -> [erl_cipher_suite()] | [string()].
			   
%% Description: Returns all supported cipher suites.
%%--------------------------------------------------------------------

cipher_suites() ->
    cipher_suites(erlang).
  
cipher_suites(erlang) ->
    Version = tls_record:highest_protocol_version([]),
    [suite_definition(S) || S <- ssl_cipher:suites(Version)];

cipher_suites(openssl) ->
    Version = tls_record:highest_protocol_version([]),
    [ssl_cipher:openssl_suite_name(S) || S <- ssl_cipher:suites(Version)];

cipher_suites(all) ->
    Version = tls_record:highest_protocol_version([]),
    Supported = ssl_cipher:suites(Version)
	++ ssl_cipher:anonymous_suites()
	++ ssl_cipher:psk_suites(Version)
	++ ssl_cipher:srp_suites(),
    [suite_definition(S) || S <- Supported].

getopts(Socket, OptionTags) ->
    tls:getopts(Socket, OptionTags).

setopts(Socket, Options) ->
    tls:setopts(Socket, Options).

shutdown(Socket, How) ->
    tls:shutdown(Socket, How).

sockname(Socket)  ->
    tls:sockname(Socket).

session_info(#sslsocket{pid = Pid}) when is_pid(Pid) ->
    tls_connection:session_info(Pid);
session_info(#sslsocket{pid = {Listen,_}}) when is_port(Listen) ->
    {error, enotconn}.

versions() ->
   tls:versions().

renegotiate(Socket) ->
    tls:renegotiate(Socket).

prf(Socket, Secret, Label, Seed, WantedLength) ->
    tls:prf(Socket, Secret, Label, Seed, WantedLength).

clear_pem_cache() ->
    tls:clear_pem_cache().

format_error(Error)  ->
    tls:format_error(Error).

random_bytes(N) ->
   tls:random_bytes(N).
