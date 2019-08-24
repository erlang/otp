%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(http_transport).

% Internal application API
-export([
	 start/1, 
	 connect/3, connect/4, 
	 listen/4, listen/5,
	 accept/2, accept/3, 
	 close/2,
	 send/3, 
	 controlling_process/3, 
	 setopts/3, getopts/2, getopts/3, 
	 getstat/2, 
	 peername/2, sockname/2, 
	 resolve/0
	]).
-export([negotiate/3]).
-export([ipv4_name/1, ipv6_name/1]).

-include_lib("inets/src/inets_app/inets_internal.hrl").
-include("http_internal.hrl").


%%%=========================================================================
%%%  Internal application API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% start(SocketType) -> ok | {error, Reason}
%%      SocketType = ip_comm | {ssl, _}  
%%                                   
%% Description: Makes sure ssl is started. 
%%-------------------------------------------------------------------------
start(ip_comm) ->
    ok;
start({ip_comm, _}) ->
    ok;
start({ssl, _}) ->
    do_start_ssl();
start({essl, _}) ->
    do_start_ssl().

do_start_ssl() ->
    try lists:foreach(fun(App) -> 
			      ok = application:ensure_started(App)
		      end,
		      [crypto, asn1, public_key, ssl])
    catch
	_:Reason ->
	    {error, Reason}
    end.
	 

%%-------------------------------------------------------------------------
%% connect(SocketType, Address, Options, Timeout) ->
%%                                            {ok, Socket} | {error, Reason}
%%      SocketType = ip_comm | {ssl, SslConfig}  
%%      Address = {Host, Port}
%%      Options = [option()]
%%      Socket = socket()
%%      option() = ipfamily() | {ip, ip_address()} | {port, integer()}
%%      ipfamily() = inet | inet6 
%%                                   
%% Description: Connects to the Host and Port specified in HTTPRequest.
%%-------------------------------------------------------------------------

connect(SocketType, Address, Opts) ->
    connect(SocketType, Address, Opts, infinity).
connect(ip_comm, {Host, Port}, Opts0, Timeout) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr, true} | Opts0 ],
    try gen_tcp:connect(Host, Port, Opts, Timeout) of
	{ok, _} = OK ->
	    OK;
	{error, _} = ERROR ->
	    ERROR
    catch 
	exit:{badarg, _} ->
	    {error, {eoptions, Opts}};
	exit:badarg ->
	    {error, {eoptions, Opts}}
    end;

%% Wrapper for backaward compatibillity
connect({ssl, SslConfig}, Address, Opts, Timeout) ->
    connect({?HTTP_DEFAULT_SSL_KIND, SslConfig}, Address, Opts, Timeout);

connect({essl, SslConfig}, {Host, Port}, Opts0, Timeout) -> 
    Opts = [binary, {active, false}, {ssl_imp, new} | Opts0] ++ SslConfig,
    case (catch ssl:connect(Host, Port, Opts, Timeout)) of
	{'EXIT', Reason} ->
	    {error, {eoptions, Reason}};
	{ok, _} = OK ->
	    OK;
	{error, _} = ERROR ->
	    ERROR
    end.


%%-------------------------------------------------------------------------
%% listen(SocketType, Addr, Port, Fd) -> {ok, Socket} | {error, Reason}
%%      SocketType = ip_comm | {ssl, SSLConfig}  
%%      Port = integer() 
%%      Socket = socket()
%%      Fd = undefined | fd()
%%
%% Description: Sets up socket to listen on the port Port on the local
%% host using either gen_tcp or ssl. In the gen_tcp case the port
%% might allready have been initiated by a wrapper-program and is
%% given as an Fd that can be retrieved by init:get_argument. The
%% reason for this to enable a HTTP-server not running as root to use
%% port 80.
%%-------------------------------------------------------------------------
listen(ip_comm, Addr, Port, Fd, IpFamily) ->
    listen_ip_comm(Addr, Port, [], Fd, IpFamily);

listen({ip_comm, SockOpts}, Addr, Port, Fd, IpFamily) ->
    listen_ip_comm(Addr, Port, SockOpts, Fd, IpFamily);

listen({essl, SSLConfig}, Addr, Port, Fd, IpFamily) ->
    listen_ssl(Addr, Port, Fd, SSLConfig, IpFamily, []).

listen(ip_comm, Addr, Port, IpFamily) ->
    listen_ip_comm(Addr, Port, [], undefined, IpFamily);

%% Wrapper for backaward compatibillity
listen({ssl, SSLConfig}, Addr, Port, IpFamily) ->
    listen({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Addr, Port, IpFamily);

listen({essl, SSLConfig}, Addr, Port, IpFamily) ->
    {SSLConfig2, ExtraOpts} = case proplists:get_value(log_alert, SSLConfig, undefined) of
		    undefined ->
			{SSLConfig, []};
		    LogAlert ->
			{proplists:delete(log_alert, SSLConfig), [{log_alert, LogAlert}]}
		end,
    listen_ssl(Addr, Port, undefined, SSLConfig2, IpFamily, ExtraOpts).

listen_ip_comm(Addr, Port, SockOpts, Fd, IpFamily) ->
    case (catch do_listen_ip_comm(Addr, Port, SockOpts, Fd, IpFamily)) of
	{'EXIT', Reason} ->
	    {error, {exit, Reason}};
	Else ->
	    Else
    end.

do_listen_ip_comm(Addr, Port, SockOpts, Fd, IpFamily) ->
    Backlog = proplists:get_value(backlog, SockOpts, 128),
    {NewPort, Opts} = get_socket_info(Addr, Port, Fd,
				      [{backlog, Backlog}, {reuseaddr, true} | SockOpts]),
    Opts2 = [IpFamily | Opts],
    gen_tcp:listen(NewPort, Opts2).

listen_ssl(Addr, Port, Fd, Opts0, IpFamily, ExtraOpts) ->
    Backlog = proplists:get_value(backlog, Opts0, 128),
    {NewPort, SockOpt} = get_socket_info(Addr, Port, Fd, 
					 [{backlog, Backlog}, {reuseaddr, true}]),
    Opts = SockOpt ++ Opts0,
    Opts2 = [IpFamily | Opts],
    ssl:listen(NewPort, Opts2 ++ ExtraOpts).

get_socket_info(Addr, Port, Fd, BaseOpts) ->
    %% The presence of a file descriptor takes precedence
    case Fd of
	undefined ->
	    {Port, sock_opts(Addr, BaseOpts)};
	Fd -> 
	    {0, sock_opts([{fd, Fd} | BaseOpts])}
    end.
	    
%%-------------------------------------------------------------------------
%% accept(SocketType, ListenSocket) -> {ok, Socket} | {error, Reason}
%% accept(SocketType, ListenSocket, Timeout) -> ok | {error, Reason}
%%   SocketType = ip_comm | {ssl, SSLConfig}  
%%   ListenSocket = socket()    
%%   Timeout = infinity | integer() >= 0
%%   Socket = socket()
%%                                   
%% Description: Accepts an incoming connection request on a listen socket,
%% using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
accept(SocketType, ListenSocket) ->
    accept(SocketType, ListenSocket, infinity).

accept(ip_comm, ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);
accept({ip_comm, _}, ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);

%% Wrapper for backaward compatibillity
accept({ssl, SSLConfig}, ListenSocket, Timeout) ->
    accept({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, ListenSocket, Timeout);

accept({essl, _SSLConfig}, ListenSocket, Timeout) ->
    ssl:transport_accept(ListenSocket, Timeout).


%%-------------------------------------------------------------------------
%% controlling_process(SocketType, Socket, NewOwner) -> ok | {error, Reason}
%%   SocketType = ip_comm | {ssl, _}  
%%   Socket = socket()        
%%   NewOwner = pid()
%%                                
%% Description: Assigns a new controlling process to Socket. 
%%-------------------------------------------------------------------------
controlling_process(ip_comm, Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner);
controlling_process({ip_comm, _}, Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner);

%% Wrapper for backaward compatibillity
controlling_process({ssl, SSLConfig}, Socket, NewOwner) ->
    controlling_process({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, NewOwner);

controlling_process({essl, _}, Socket, NewOwner) ->
    ssl:controlling_process(Socket, NewOwner).


%%-------------------------------------------------------------------------
%% setopts(SocketType, Socket, Options) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()
%%     Options = list()                              
%% Description: Sets one or more options for a socket, using either
%% gen_tcp or ssl.
%%-------------------------------------------------------------------------
setopts(ip_comm, Socket, Options) ->
    inet:setopts(Socket, Options);
setopts({ip_comm, _}, Socket, Options) ->
    inet:setopts(Socket, Options);

%% Wrapper for backaward compatibillity
setopts({ssl, SSLConfig}, Socket, Options) ->
    setopts({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, Options);

setopts({essl, _}, Socket, Options) ->
    (catch ssl:setopts(Socket, Options)).


%%-------------------------------------------------------------------------
%% getopts(SocketType, Socket [, Opts]) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()
%%     Opts   = socket_options()
%% Description: Gets the values for some options. 
%%-------------------------------------------------------------------------
getopts(SocketType, Socket) ->
    Opts = [packet, packet_size, recbuf, sndbuf, priority, tos, send_timeout], 
    getopts(SocketType, Socket, Opts).

getopts({ip_comm, _}, Socket, Options) ->
    getopts(ip_comm, Socket, Options);

getopts(ip_comm, Socket, Options) ->
    case inet:getopts(Socket, Options) of
	{ok, SocketOpts} ->
	    SocketOpts;
	{error, _} -> 
	    []
    end;

%% Wrapper for backaward compatibillity
getopts({ssl, SSLConfig}, Socket, Options) ->
    getopts({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, Options);

getopts({essl, _}, Socket, Options) ->
    getopts_ssl(Socket, Options).

getopts_ssl(Socket, Options) ->
    case ssl:getopts(Socket, Options) of
	{ok, SocketOpts} ->
	    SocketOpts;
	{error, _} -> 
	    []
    end.
    

%%-------------------------------------------------------------------------
%% getstat(SocketType, Socket) -> socket_stats()
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()
%%     socket_stats() = list()
%% Description: Gets the socket stats values for the socket
%%-------------------------------------------------------------------------
getstat(ip_comm = _SocketType, Socket) ->
    case inet:getstat(Socket) of
	{ok, Stats} ->
	    Stats;
	{error, _} ->
	    []
    end;

%% Wrapper for backaward compatibillity
getstat({ssl, SSLConfig}, Socket) ->
    getstat({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket);

getstat({essl, _} = _SocketType, _Socket) ->
    [].


%%-------------------------------------------------------------------------
%% send(RequestOrSocketType, Socket, Message) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()
%%     Message = list() | binary()                           
%% Description: Sends a packet on a socket, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
send(ip_comm, Socket, Message) ->
    gen_tcp:send(Socket, Message);
send({ip_comm, _}, Socket, Message) ->
    gen_tcp:send(Socket, Message);

%% Wrapper for backaward compatibillity
send({ssl, SSLConfig}, Socket, Message) ->
    send({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, Message);

send({essl, _}, Socket, Message) ->
    ssl:send(Socket, Message).

%%-------------------------------------------------------------------------
%% close(SocketType, Socket) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()  
%%                                   
%% Description: Closes a socket, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
close(ip_comm, Socket) ->
    gen_tcp:close(Socket);
close({ip_comm, []}, Socket) ->
    gen_tcp:close(Socket);

%% Wrapper for backaward compatibillity
close({ssl, SSLConfig}, Socket) ->
    close({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket);

close({essl, _}, Socket) ->
    ssl:close(Socket).


%%-------------------------------------------------------------------------
%% peername(SocketType, Socket) -> {Port, SockName}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket() 
%%     Port = integer()  (-1 if error occured)
%%     PeerName = string()
%%                          
%% Description: Returns the address and port for the other end of a
%% connection, usning either gen_tcp or ssl.
%%-------------------------------------------------------------------------
peername(ip_comm, Socket) ->
    do_peername(inet:peername(Socket));
peername({ip_comm, _}, Socket) ->
    do_peername(inet:peername(Socket));

%% Wrapper for backaward compatibillity
peername({ssl, SSLConfig}, Socket) ->
    peername({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket);

peername({essl, _}, Socket) ->
    do_peername(ssl:peername(Socket)).

do_peername({ok, {Addr, Port}}) 
  when is_tuple(Addr) andalso (size(Addr) =:= 4) ->
    PeerName = ipv4_name(Addr), 
    {Port, PeerName};
do_peername({ok, {Addr, Port}}) 
  when is_tuple(Addr) andalso (size(Addr) =:= 8) ->
    PeerName = ipv6_name(Addr), 
    {Port, PeerName};
do_peername({error, _}) ->
    {-1, "unknown"}.


%%-------------------------------------------------------------------------
%% sockname(SocketType, Socket) -> {Port, SockName}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket() 
%%     Port = integer()  (-1 if error occured)
%%     SockName = string()
%%                          
%% Description: Returns the address and port for the local (our) end 
%% other end of connection, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
sockname(ip_comm, Socket) ->
    do_sockname(inet:sockname(Socket));
sockname({ip_comm, _}, Socket) ->
    do_sockname(inet:sockname(Socket));
%% Wrapper for backaward compatibillity
sockname({ssl, SSLConfig}, Socket) ->
    sockname({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket);

sockname({essl, _}, Socket) ->
    do_sockname(ssl:sockname(Socket)).

do_sockname({ok, {Addr, Port}}) 
  when is_tuple(Addr) andalso (size(Addr) =:= 4) ->
    SockName = ipv4_name(Addr), 
    {Port, SockName};
do_sockname({ok, {Addr, Port}}) 
  when is_tuple(Addr) andalso (size(Addr) =:= 8) ->
    SockName = ipv6_name(Addr), 
    {Port, SockName};
do_sockname({error, _}) ->
    {-1, "unknown"}.


%%-------------------------------------------------------------------------
%% resolve() -> HostName
%%     HostName = string()
%%     
%% Description: Returns the local hostname. 
%%-------------------------------------------------------------------------
resolve() ->
    {ok, Name} = inet:gethostname(),
    Name.


%%-------------------------------------------------------------------------
%% ipv4_name(Ipv4Addr) -> string()
%% ipv6_name(Ipv6Addr) -> string()
%%     Ipv4Addr = ip4_address()
%%     Ipv6Addr = ip6_address()
%%     
%% Description: Returns the local hostname. 
%%-------------------------------------------------------------------------
ipv4_name({A, B, C, D}) ->
    integer_to_list(A) ++ "." ++
	integer_to_list(B) ++ "." ++
	integer_to_list(C) ++ "." ++
	integer_to_list(D).

ipv6_name({A, B, C, D, E, F, G, H}) ->
    http_util:integer_to_hexlist(A) ++ ":"++ 
	http_util:integer_to_hexlist(B) ++ ":" ++  
	http_util:integer_to_hexlist(C) ++ ":" ++ 
	http_util:integer_to_hexlist(D) ++ ":" ++  
	http_util:integer_to_hexlist(E) ++ ":" ++  
	http_util:integer_to_hexlist(F) ++ ":" ++  
	http_util:integer_to_hexlist(G) ++ ":" ++  
	http_util:integer_to_hexlist(H).


%%%========================================================================
%%% Internal functions
%%%========================================================================

%% -- sock_opts --
%% Address any comes from directive: BindAddress "*"
sock_opts(undefined, Opts) -> 
    sock_opts(Opts);
sock_opts(any = Addr, Opts) -> 
    sock_opts([{ip, Addr} | Opts]);
sock_opts(Addr, Opts) ->
    sock_opts([{ip, Addr} | Opts]).

sock_opts(Opts) ->
    [{packet, 0}, {active, false} | Opts].


%% -- negotiate --
negotiate(ip_comm,_,_) ->
    ok;
negotiate({ip_comm, _},_,_) ->
    ok;
negotiate({ssl, SSLConfig}, Socket, Timeout) ->
    negotiate({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, Timeout);
negotiate({essl, _}, Socket, Timeout) ->
    negotiate_ssl(Socket, Timeout).

negotiate_ssl(Socket, Timeout) ->
    ssl:ssl_accept(Socket, Timeout). 
