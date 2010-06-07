%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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

-module(http_transport).

% Internal application API
-export([
	 start/1, 
	 connect/3, connect/4, 
	 listen/2, listen/3, 
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

-include_lib("inets/src/inets_app/inets_internal.hrl").
-include("http_internal.hrl").

-define(SERVICE, httpl).
-define(hlri(Label, Content), ?report_important(Label, ?SERVICE, Content)).
-define(hlrv(Label, Content), ?report_verbose(Label,   ?SERVICE, Content)).
-define(hlrd(Label, Content), ?report_debug(Label,     ?SERVICE, Content)).
-define(hlrt(Label, Content), ?report_trace(Label,     ?SERVICE, Content)).


%%%=========================================================================
%%%  Internal application API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% start(SocketType) -> ok | {error, Reason}
%%      SocketType = ip_comm | {ssl, _}  
%%                                   
%% Description: Makes sure inet_db or ssl is started. 
%%-------------------------------------------------------------------------
start(ip_comm) ->
    do_start_ip_comm();

%% This is just for backward compatibillity
start({ssl, _}) ->
    do_start_ssl();
start({ossl, _}) ->
    do_start_ssl();
start({essl, _}) ->
    do_start_ssl().


do_start_ip_comm() ->
    case inet_db:start() of
	{ok, _} ->
	    ok;
	{error, {already_started, _}} ->
	    ok;
	Error ->
	    Error
    end.

do_start_ssl() ->
    case ssl:start() of
	ok ->
	    ok;
	{error, {already_started,_}} ->
	    ok;
	Error ->
	    Error
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

connect(ip_comm = _SocketType, {Host, Port}, Opts0, Timeout) 
  when is_list(Opts0) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr, true} | Opts0],
    ?hlrt("connect using gen_tcp", 
	  [{host, Host}, {port, Port}, {opts, Opts}, {timeout, Timeout}]),
    gen_tcp:connect(Host, Port, Opts, Timeout);

%% Wrapper for backaward compatibillity
connect({ssl, SslConfig}, Address, Opts, Timeout) ->
    connect({?HTTP_DEFAULT_SSL_KIND, SslConfig}, Address, Opts, Timeout);

connect({ossl, SslConfig}, {Host, Port}, _, Timeout) ->
    Opts = [binary, {active, false}, {ssl_imp, old}] ++ SslConfig,
    ?hlrt("connect using ossl", 
	  [{host,       Host}, 
	   {port,       Port}, 
	   {ssl_config, SslConfig}, 
	   {timeout,    Timeout}]),
    ssl:connect(Host, Port, Opts, Timeout);

connect({essl, SslConfig}, {Host, Port}, _, Timeout) ->
    Opts = [binary, {active, false}, {ssl_imp, new}] ++ SslConfig,
    ?hlrt("connect using essl", 
	  [{host,       Host}, 
	   {port,       Port}, 
	   {ssl_config, SslConfig}, 
	   {timeout,    Timeout}]),
    ssl:connect(Host, Port, Opts, Timeout).


%%-------------------------------------------------------------------------
%% listen(SocketType, Port) -> {ok, Socket} | {error, Reason}
%%      SocketType = ip_comm | {ssl, SSLConfig}  
%%      Port = integer() 
%%      Socket = socket()                            
%%
%% Description: Sets up socket to listen on the port Port on the local
%% host using either gen_tcp or ssl. In the gen_tcp case the port
%% might allready have been initiated by a wrapper-program and is
%% given as an Fd that can be retrieved by init:get_argument. The
%% reason for this to enable a HTTP-server not running as root to use
%% port 80.
%%-------------------------------------------------------------------------
listen(SocketType, Port) ->
    listen(SocketType, undefined, Port).

listen(ip_comm, Addr, Port) ->
    case (catch listen_ip_comm(Addr, Port)) of
	{'EXIT', Reason} ->
	    {error, {exit, Reason}};
	Else ->
	    Else
    end;

%% Wrapper for backaward compatibillity
listen({ssl, SSLConfig}, Addr, Port) ->
    ?hlrt("listen (wrapper)", 
	  [{addr,       Addr}, 
	   {port,       Port}, 
	   {ssl_config, SSLConfig}]),
    listen({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Addr, Port);

listen({ossl, SSLConfig} = Ssl, Addr, Port) ->
    ?hlrt("listen (ossl)", 
	  [{addr,       Addr}, 
	   {port,       Port}, 
	   {ssl_config, SSLConfig}]),
    Opt = sock_opt(Ssl, Addr, SSLConfig),
    ?hlrt("listen options", [{opt, Opt}]),
    ssl:listen(Port, [{ssl_imp, old} | Opt]);

listen({essl, SSLConfig} = Ssl, Addr, Port) ->
    ?hlrt("listen (essl)", 
	  [{addr,       Addr}, 
	   {port,       Port}, 
	   {ssl_config, SSLConfig}]),
    Opt = sock_opt(Ssl, Addr, SSLConfig),
    ?hlrt("listen options", [{opt, Opt}]),
    Opt2 = [{ssl_imp, new}, {reuseaddr, true} | Opt], 
    ssl:listen(Port, Opt2).


listen_ip_comm(Addr, Port) ->
    {NewPort, Opts, IpFamily} = get_socket_info(Addr, Port),
    case IpFamily of
	inet6fb4 -> 
	    Opts2 = [inet6 | Opts], 
	    case (catch gen_tcp:listen(NewPort, Opts2)) of
		{error, Reason} when ((Reason =:= nxdomain) orelse 
				      (Reason =:= eafnosupport)) ->
		    Opts3 = [inet | Opts], 
		    gen_tcp:listen(NewPort, Opts3);

		%% This is when a given hostname has resolved to a 
		%% IPv4-address. The inet6-option together with a 
		%% {ip, IPv4} option results in badarg
		{'EXIT', _} -> 
		    Opts3 = [inet | Opts], 
		    gen_tcp:listen(NewPort, Opts3); 

		Other ->
		    Other
	    end;
	_ ->
	    Opts2 = [IpFamily | Opts],
	    gen_tcp:listen(NewPort, Opts2)
    end.

ipfamily_default(Addr, Port) ->
    httpd_conf:lookup(Addr, Port, ipfamily, inet6fb4).

get_socket_info(Addr, Port) ->
    Key  = list_to_atom("httpd_" ++ integer_to_list(Port)),
    BaseOpts =  [{backlog, 128}, {reuseaddr, true}], 
    IpFamilyDefault = ipfamily_default(Addr, Port), 
    case init:get_argument(Key) of
	{ok, [[Value]]} ->
	    {Fd, IpFamily} = 
		case string:tokens(Value, [$|]) of
		    [FdStr, IpFamilyStr] ->
			Fd0       = fd_of(FdStr),
			IpFamily0 = ip_family_of(IpFamilyStr),
			{Fd0, IpFamily0};
		    [FdStr] ->
			{fd_of(FdStr), IpFamilyDefault};
		    _ ->
			throw({error, {bad_descriptor, Value}})
		end,
	    {0, sock_opt(ip_comm, Addr, [{fd, Fd} | BaseOpts]), IpFamily};
	error ->
	    {Port, sock_opt(ip_comm, Addr, BaseOpts), IpFamilyDefault}
    end.
	    

fd_of(FdStr) ->
    case (catch list_to_integer(FdStr)) of
	Fd when is_integer(Fd) ->
	    Fd;
	_ ->
	    throw({error, {bad_descriptor, FdStr}})
    end.

ip_family_of(IpFamilyStr) ->
    IpFamily = list_to_atom(IpFamilyStr),
    case lists:member(IpFamily, [inet, inet6, inet6fb4]) of
	true ->
	    IpFamily;
	false ->
	    throw({error, {bad_ipfamily, IpFamilyStr}})
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

%% Wrapper for backaward compatibillity
accept({ssl, SSLConfig}, ListenSocket, Timeout) ->
    accept({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, ListenSocket, Timeout);

accept({ossl, _SSLConfig}, ListenSocket, Timeout) ->
    ssl:transport_accept(ListenSocket, Timeout);
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

%% Wrapper for backaward compatibillity
controlling_process({ssl, SSLConfig}, Socket, NewOwner) ->
    controlling_process({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, NewOwner);

controlling_process({ossl, _}, Socket, NewOwner) ->
    ssl:controlling_process(Socket, NewOwner);

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
    ?hlrt("ip_comm setopts", [{socket, Socket}, {options, Options}]),
    inet:setopts(Socket, Options);

%% Wrapper for backaward compatibillity
setopts({ssl, SSLConfig}, Socket, Options) ->
    setopts({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, Options);

setopts({ossl, _}, Socket, Options) ->
    ?hlrt("[o]ssl setopts", [{socket, Socket}, {options, Options}]),
    Reason = (catch ssl:setopts(Socket, Options)),
    ?hlrt("[o]ssl setopts result", [{reason, Reason}]),
    Reason;
	

setopts({essl, _}, Socket, Options) ->
    ?hlrt("[e]ssl setopts", [{socket, Socket}, {options, Options}]),
    Reason = (catch ssl:setopts(Socket, Options)),
    ?hlrt("[e]ssl setopts result", [{reason, Reason}]),
    Reason.


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

getopts(ip_comm, Socket, Options) ->
    ?hlrt("ip_comm getopts", [{socket, Socket}, {options, Options}]),
    case inet:getopts(Socket, Options) of
	{ok, SocketOpts} ->
	    SocketOpts;
	{error, _} -> 
	    []
    end;

%% Wrapper for backaward compatibillity
getopts({ssl, SSLConfig}, Socket, Options) ->
    getopts({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, Options);

getopts({ossl, _}, Socket, Options) ->
    ?hlrt("ssl getopts", [{socket, Socket}, {options, Options}]),
    getopts_ssl(Socket, Options);

getopts({essl, _}, Socket, Options) ->
    ?hlrt("essl getopts", [{socket, Socket}, {options, Options}]),
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
    ?hlrt("ip_comm getstat", [{socket, Socket}]),
    case inet:getstat(Socket) of
	{ok, Stats} ->
	    Stats;
	{error, _} ->
	    []
    end;

%% Wrapper for backaward compatibillity
getstat({ssl, SSLConfig}, Socket) ->
    getstat({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket);

getstat({ossl, _} = _SocketType, _Socket) ->
    [];

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

%% Wrapper for backaward compatibillity
send({ssl, SSLConfig}, Socket, Message) ->
    send({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, Message);

send({ossl, _}, Socket, Message) ->
    ssl:send(Socket, Message);

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

%% Wrapper for backaward compatibillity
close({ssl, SSLConfig}, Socket) ->
    close({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket);

close({ossl, _}, Socket) ->
    ssl:close(Socket);

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
    case inet:peername(Socket) of
	{ok,{{A, B, C, D}, Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port, PeerName};
	{ok,{{A, B, C, D, E, F, G, H}, Port}} ->
	    PeerName =  http_util:integer_to_hexlist(A) ++ ":"++  
		http_util:integer_to_hexlist(B) ++ ":" ++  
		http_util:integer_to_hexlist(C) ++ ":" ++ 
		http_util:integer_to_hexlist(D) ++ ":" ++  
		http_util:integer_to_hexlist(E) ++ ":" ++  
		http_util:integer_to_hexlist(F) ++ ":" ++  
		http_util:integer_to_hexlist(G) ++":"++  
		http_util:integer_to_hexlist(H),
	    {Port, PeerName};
	{error, _} ->
	    {-1, "unknown"}
    end;

%% Wrapper for backaward compatibillity
peername({ssl, SSLConfig}, Socket) ->
    peername({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket);

peername({ossl, _}, Socket) ->
    peername_ssl(Socket);

peername({essl, _}, Socket) ->
    peername_ssl(Socket).

peername_ssl(Socket) ->
    case ssl:peername(Socket) of
	{ok,{{A, B, C, D}, Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port, PeerName};
	{error, _} ->
	    {-1, "unknown"}
    end.


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
    case inet:sockname(Socket) of
	{ok,{{A, B, C, D}, Port}} ->
	    SockName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port, SockName};
	{ok,{{A, B, C, D, E, F, G, H}, Port}} ->
	    SockName =  http_util:integer_to_hexlist(A) ++ ":"++  
		http_util:integer_to_hexlist(B) ++ ":" ++  
		http_util:integer_to_hexlist(C) ++ ":" ++ 
		http_util:integer_to_hexlist(D) ++ ":" ++  
		http_util:integer_to_hexlist(E) ++ ":" ++  
		http_util:integer_to_hexlist(F) ++ ":" ++  
		http_util:integer_to_hexlist(G) ++":"++  
		http_util:integer_to_hexlist(H),
	    {Port, SockName};
	{error, _} ->
	    {-1, "unknown"}
    end;

%% Wrapper for backaward compatibillity
sockname({ssl, SSLConfig}, Socket) ->
    sockname({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket);

sockname({ossl, _}, Socket) ->
    sockname_ssl(Socket);

sockname({essl, _}, Socket) ->
    sockname_ssl(Socket).

sockname_ssl(Socket) ->
    case ssl:sockname(Socket) of
	{ok,{{A, B, C, D}, Port}} ->
	    SockName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port, SockName};
	{error, _} ->
	    {-1, "unknown"}
    end.


%%-------------------------------------------------------------------------
%% resolve() -> HostName
%%     HostName = string()
%%     
%% Description: Returns the local hostname. 
%%-------------------------------------------------------------------------
resolve() ->
    {ok, Name} = inet:gethostname(),
    Name.


%%%========================================================================
%%% Internal functions
%%%========================================================================

%% Address any comes from directive: BindAddress "*"
sock_opt(ip_comm, any = Addr, Opts) -> 
    sock_opt2([{ip, Addr} | Opts]);
sock_opt(ip_comm, undefined, Opts) -> 
    sock_opt2(Opts);
sock_opt(_, any = _Addr, Opts) ->
    sock_opt2(Opts);
sock_opt(_, undefined = _Addr, Opts) ->
    sock_opt2(Opts);
sock_opt(_, {_,_,_,_} = Addr, Opts) -> 
    sock_opt2([{ip, Addr} | Opts]);
sock_opt(ip_comm, Addr, Opts) -> 
    sock_opt2([{ip, Addr} | Opts]);
sock_opt(_, Addr, Opts) ->
    sock_opt2([{ip, Addr} | Opts]).

sock_opt2(Opts) ->
    [{packet, 0}, {active, false} | Opts].

negotiate(ip_comm,_,_) ->
    ?hlrt("negotiate(ip_comm)", []),
    ok;
negotiate({ssl, SSLConfig}, Socket, Timeout) ->
    ?hlrt("negotiate(ssl)", []),
    negotiate({?HTTP_DEFAULT_SSL_KIND, SSLConfig}, Socket, Timeout);
negotiate({ossl, _}, Socket, Timeout) ->
    ?hlrt("negotiate(ossl)", []),
    negotiate_ssl(Socket, Timeout);
negotiate({essl, _}, Socket, Timeout) ->
    ?hlrt("negotiate(essl)", []),
    negotiate_ssl(Socket, Timeout).

negotiate_ssl(Socket, Timeout) ->
    ?hlrt("negotiate_ssl", [{socket, Socket}, {timeout, Timeout}]),
    case ssl:ssl_accept(Socket, Timeout) of
	ok ->
	    ok;
	{error, Reason} ->
	    ?hlrd("negotiate_ssl - accept failed", [{reason, Reason}]),
	    %% Look for "valid" error reasons
	    ValidReasons = [timeout, econnreset, esslaccept, esslerrssl], 
	    case lists:member(Reason, ValidReasons) of
		true ->
		    {error, normal};
		false ->
		    {error, Reason}
           end
    end.
