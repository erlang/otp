-module(ssl_socket).

-include("ssl_internal.hrl").
-include("ssl_api.hrl").

-export([socket/4, setopts/3, getopts/3, peername/2, sockname/2, port/2]).

socket(Pid, Transport, Socket, ConnectionCb) ->
    #sslsocket{pid = Pid, 
	       %% "The name "fd" is keept for backwards compatibility
	       fd = {Transport, Socket, ConnectionCb}}.

setopts(gen_tcp, Socket, Options) ->
    inet:setopts(Socket, Options);
setopts(Transport, Socket, Options) ->
    Transport:setopts(Socket, Options).

getopts(gen_tcp, Socket, Options) ->
    inet:getopts(Socket, Options);
getopts(Transport, Socket, Options) ->
    Transport:getopts(Socket, Options).

peername(gen_tcp, Socket) ->
    inet:peername(Socket);
peername(Transport, Socket) ->
    Transport:peername(Socket).

sockname(gen_tcp, Socket) ->
    inet:sockname(Socket);
sockname(Transport, Socket) ->
    Transport:sockname(Socket).

port(gen_tcp, Socket) ->
    inet:port(Socket);
port(Transport, Socket) ->
    Transport:port(Socket).
