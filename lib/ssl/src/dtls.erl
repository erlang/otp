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

%%% Purpose : Reflect DTLS specific API options (fairly simple wrapper at the moment)
%% First implementation will support DTLS connections only in a "TLS/TCP like way"

-module(dtls).

-include("ssl_api.hrl").
-include("ssl_internal.hrl").

-export([connect/2, connect/3, listen/2, accept/1, accept/2,
	 handshake/1, handshake/2, handshake/3]).

%%--------------------------------------------------------------------
-spec connect(host() | port(), [connect_option()]) -> {ok, #sslsocket{}} |
					      {error, reason()}.
-spec connect(host() | port(), [connect_option()] | inet:port_number(),
	      timeout() | list()) ->
		     {ok, #sslsocket{}} | {error, reason()}.
-spec connect(host() | port(), inet:port_number(), list(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.

%%
%% Description: Connect to an DTLS server.
%%--------------------------------------------------------------------

connect(Socket, Options) when is_port(Socket) ->
    connect(Socket, Options, infinity).
connect(Socket, SslOptions, Timeout)  when is_port(Socket) ->
    DTLSOpts = [{protocol, dtls} | SslOptions],
    ssl:connect(Socket, DTLSOpts, Timeout);
connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).
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
-spec accept(#sslsocket{}) -> {ok, #sslsocket{}} |
					{error, reason()}.
-spec accept(#sslsocket{}, timeout()) -> {ok, #sslsocket{}} |
						   {error, reason()}.
%%
%% Description: Performs transport accept on an ssl listen socket
%%--------------------------------------------------------------------
accept(ListenSocket) ->
    accept(ListenSocket, infinity).
accept(Socket, Timeout) ->
    ssl:transport_accept(Socket, Timeout).

%%--------------------------------------------------------------------
-spec handshake(#sslsocket{}) -> ok | {error, reason()}.
-spec handshake(#sslsocket{} | port(), timeout()| [ssl_option()
						    | transport_option()]) ->
			ok | {ok, #sslsocket{}} | {error, reason()}.
-spec handshake(port(), [ssl_option()| transport_option()], timeout()) ->
			{ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake.
%%--------------------------------------------------------------------

handshake(ListenSocket) ->
    handshake(ListenSocket, infinity).

handshake(#sslsocket{} = Socket, Timeout) ->
    ssl:ssl_accept(Socket, Timeout);

handshake(ListenSocket, SslOptions)  when is_port(ListenSocket) ->
    handshake(ListenSocket, SslOptions, infinity).

handshake(Socket, SslOptions, Timeout) when is_port(Socket) ->
    ssl:ssl_accept(Socket, SslOptions, Timeout).
