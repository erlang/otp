%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2017. All Rights Reserved.
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
-module(dtls_socket).

-include("ssl_internal.hrl").
-include("ssl_api.hrl").

-export([send/3, listen/3, accept/3, connect/4, socket/4, setopts/3, getopts/3, getstat/3, 
	 peername/2, sockname/2, port/2, close/2]).
-export([emulated_options/0, internal_inet_values/0, default_inet_values/0, default_cb_info/0]).

send(Transport, {{IP,Port},Socket}, Data) ->
    Transport:send(Socket, IP, Port, Data).

listen(gen_udp = Transport, Port, #config{transport_info = {Transport, _, _, _},
					  ssl = SslOpts, 
					  emulated = EmOpts,
					  inet_user = Options} = Config) ->
    
    
    case dtls_udp_sup:start_child([Port, emulated_socket_options(EmOpts, #socket_options{}), 
				   Options ++ internal_inet_values(), SslOpts]) of
	{ok, Pid} ->
	    {ok, #sslsocket{pid = {udp, Config#config{udp_handler = {Pid, Port}}}}};
	Err = {error, _} ->
	    Err
    end.

accept(udp, #config{transport_info = {Transport = gen_udp,_,_,_},
		    connection_cb = ConnectionCb,
		    udp_handler = {Listner, _}}, _Timeout) -> 
    case dtls_udp_listener:accept(Listner, self()) of
	{ok, Pid, Socket} ->
	    {ok, socket(Pid, Transport, {Listner, Socket}, ConnectionCb)};
	{error, Reason} ->
	    {error, Reason}
    end.

connect(Address, Port, #config{transport_info = {Transport, _, _, _} = CbInfo,
				connection_cb = ConnectionCb,
				ssl = SslOpts,
				emulated = EmOpts,
				inet_ssl = SocketOpts}, Timeout) ->
    case Transport:open(0, SocketOpts ++ internal_inet_values()) of
	{ok, Socket} ->
	    ssl_connection:connect(ConnectionCb, Address, Port, {{Address, Port},Socket}, 
				   {SslOpts, 
				    emulated_socket_options(EmOpts, #socket_options{}), undefined},
				   self(), CbInfo, Timeout);
	{error, _} = Error->	
	    Error
    end.

close(gen_udp, {_Client, _Socket}) ->
    ok.

socket(Pid, gen_udp = Transport, {{_, _}, Socket}, ConnectionCb) ->
    #sslsocket{pid = Pid, 
	       %% "The name "fd" is keept for backwards compatibility
	       fd = {Transport, Socket, ConnectionCb}};
socket(Pid, Transport, Socket, ConnectionCb) ->
    #sslsocket{pid = Pid, 
	       %% "The name "fd" is keept for backwards compatibility
	       fd = {Transport, Socket, ConnectionCb}}.
%% Vad göra med emulerade
setopts(gen_udp, #sslsocket{pid = {Socket, _}}, Options) ->
    {SockOpts, _} = tls_socket:split_options(Options),
    inet:setopts(Socket, SockOpts);
setopts(_, #sslsocket{pid = {ListenSocket, #config{transport_info = {Transport,_,_,_}}}}, Options) ->
    {SockOpts, _} = tls_socket:split_options(Options),
    Transport:setopts(ListenSocket, SockOpts);
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
setopts(gen_udp, Socket, Options) ->
    inet:setopts(Socket, Options);
setopts(Transport, Socket, Options) ->
    Transport:setopts(Socket, Options).

getopts(gen_udp,  #sslsocket{pid = {Socket, #config{emulated = EmOpts}}}, Options) ->
    {SockOptNames, EmulatedOptNames} = tls_socket:split_options(Options),
    EmulatedOpts = get_emulated_opts(EmOpts, EmulatedOptNames),
    SocketOpts = tls_socket:get_socket_opts(Socket, SockOptNames, inet),
    {ok, EmulatedOpts ++ SocketOpts}; 
getopts(Transport,  #sslsocket{pid = {ListenSocket, #config{emulated = EmOpts}}}, Options) ->
    {SockOptNames, EmulatedOptNames} = tls_socket:split_options(Options),
    EmulatedOpts = get_emulated_opts(EmOpts, EmulatedOptNames),
    SocketOpts = tls_socket:get_socket_opts(ListenSocket, SockOptNames, Transport),
    {ok, EmulatedOpts ++ SocketOpts}; 
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
getopts(gen_udp, {_,Socket}, Options) ->
    inet:getopts(Socket, Options);
getopts(Transport, Socket, Options) ->
    Transport:getopts(Socket, Options).
getstat(gen_udp, {_,Socket}, Options) ->
	inet:getstat(Socket, Options);
getstat(Transport, Socket, Options) ->
	Transport:getstat(Socket, Options).
peername(udp, _) ->
    {error, enotconn};
peername(gen_udp, {_, {Client, _Socket}}) ->
    {ok, Client};
peername(Transport, Socket) ->
    Transport:peername(Socket).
sockname(gen_udp, {_, {_,Socket}}) ->
    inet:sockname(Socket);
sockname(gen_udp, Socket) ->
    inet:sockname(Socket);
sockname(Transport, Socket) ->
    Transport:sockname(Socket).

port(gen_udp, {_,Socket}) ->
    inet:port(Socket);
port(Transport, Socket) ->
    Transport:port(Socket).

emulated_options() ->
    [mode, active,  packet, packet_size].

internal_inet_values() ->
    [{active, false}, {mode,binary}].

default_inet_values() ->
    [{active, true}, {mode, list}].

default_cb_info() ->
    {gen_udp, udp, udp_closed, udp_error}.

get_emulated_opts(EmOpts, EmOptNames) -> 
    lists:map(fun(Name) -> {value, Value} = lists:keysearch(Name, 1, EmOpts),
			   Value end,
	      EmOptNames).

emulated_socket_options(InetValues, #socket_options{
				       mode   = Mode,
				       active = Active}) ->
    #socket_options{
       mode   = proplists:get_value(mode, InetValues, Mode),
       active = proplists:get_value(active, InetValues, Active)
      }.
