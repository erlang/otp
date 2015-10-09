%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2013. All Rights Reserved.
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

%%% Purpose : Reflect DTLS specific API options (fairly simple wrapper at the moment)
%% First implementation will support DTLS connections only in a "TLS/TCP like way"

-module(dtls).

-include("ssl_api.hrl").
-include("ssl_internal.hrl").

-export([connect/2, connect/3, listen/2, accept/1, accept/2,
	 handshake/1, handshake/2, handshake/3]).

%%--------------------------------------------------------------------
%%
%% Description: Connect to a DTLS server.
%%--------------------------------------------------------------------

-spec connect(host() | port(), [connect_option()]) -> {ok, #sslsocket{}} |
					      {error, reason()}.

connect(Socket, Options) when is_port(Socket) ->
    connect(Socket, Options, infinity).

-spec connect(host() | port(), [connect_option()] | inet:port_number(),
	      timeout() | list()) ->
		     {ok, #sslsocket{}} | {error, reason()}.

connect(Socket, SslOptions, Timeout)  when is_port(Socket) ->
    DTLSOpts = [{protocol, dtls} | SslOptions],
    ssl:connect(Socket, DTLSOpts, Timeout);
connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).

-spec connect(host() | port(), inet:port_number(), list(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.

connect(Host, Port, Options, Timeout) ->
    DTLSOpts = [{protocol, dtls} | Options],
    ssl:connect(Host, Port, DTLSOpts, Timeout).

%%--------------------------------------------------------------------
-spec listen(inet:port_number(), [listen_option()]) ->{ok, #sslsocket{}} | {error, reason()}.

%%
%% Description: Creates an ssl listen socket.
%%--------------------------------------------------------------------
listen(Port, Options) ->
    DTLSOpts = [{protocol, dtls} | Options],
    ssl:listen(Port, DTLSOpts).

%%--------------------------------------------------------------------
%%
%% Description: Performs transport accept on an ssl listen socket
%%--------------------------------------------------------------------
-spec accept(#sslsocket{}) -> {ok, #sslsocket{}} |
					{error, reason()}.
accept(ListenSocket) ->
    accept(ListenSocket, infinity).

-spec accept(#sslsocket{}, timeout()) -> {ok, #sslsocket{}} |
						   {error, reason()}.
accept(Socket, Timeout) ->
    ssl:transport_accept(Socket, Timeout).

%%--------------------------------------------------------------------
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake.
%%--------------------------------------------------------------------

-spec handshake(#sslsocket{}) -> ok | {error, reason()}.

handshake(ListenSocket) ->
    handshake(ListenSocket, infinity).


-spec handshake(#sslsocket{} | port(), timeout()| [ssl_option()
						    | transport_option()]) ->
			ok | {ok, #sslsocket{}} | {error, reason()}.

handshake(#sslsocket{} = Socket, Timeout) ->
    ssl:ssl_accept(Socket, Timeout);

handshake(ListenSocket, SslOptions)  when is_port(ListenSocket) ->
    handshake(ListenSocket, SslOptions, infinity).


-spec handshake(port(), [ssl_option()| transport_option()], timeout()) ->
			{ok, #sslsocket{}} | {error, reason()}.

handshake(Socket, SslOptions, Timeout) when is_port(Socket) ->
    ssl:ssl_accept(Socket, SslOptions, Timeout).
