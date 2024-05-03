%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
-moduledoc false.

% Internal application API
-export([
	 start/1, 
	 connect/3, connect/4, 
	 listen/4, listen/5,
	 accept/2, accept/3, 
	 close/2,
         close_tag/1,
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

%%%=========================================================================
%%%  Internal application API
%%%=========================================================================

start(ip_comm) ->
    ok;
start({ip_comm, _}) ->
    ok;
start({ssl, _}) ->
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

connect(SocketType, Address, Opts) ->
    connect(SocketType, Address, Opts, infinity).
connect(ip_comm, {Host, Port}, Opts0, Timeout) ->
    Opts = [binary, {packet, 0}, {active, false} | Opts0],
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

connect({ssl, SslConfig}, {Host, Port}, Opts0, Timeout) -> 
    Opts = [binary,  {packet, 0}, {active, false} | Opts0] ++ SslConfig,
    case (catch ssl:connect(Host, Port, Opts, Timeout)) of
	{'EXIT', Reason} ->
	    {error, {eoptions, Reason}};
	{ok, _} = OK ->
	    OK;
	{error, _} = ERROR ->
	    ERROR
    end.

listen(ip_comm, Addr, Port, Fd, IpFamily) ->
    listen_ip_comm(Addr, Port, [], Fd, IpFamily);

listen({ip_comm, SockOpts}, Addr, Port, Fd, IpFamily) ->
    listen_ip_comm(Addr, Port, SockOpts, Fd, IpFamily);

listen({ssl, SSLConfig}, Addr, Port, Fd, IpFamily) ->
    listen_ssl(Addr, Port, Fd, SSLConfig, IpFamily, []).

listen(ip_comm, Addr, Port, IpFamily) ->
    listen_ip_comm(Addr, Port, [], undefined, IpFamily);

listen({ssl, SSLConfig}, Addr, Port, IpFamily) ->
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

-spec accept(SocketType, ListenSocket) -> {ok, Socket} | {error, Reason} when
      SocketType   :: ip_comm | {ssl, SSLConfig},
      SSLConfig    :: term(),
      ListenSocket :: gen_tcp:socket(),
      Socket       :: gen_tcp:socket(),
      Reason       :: term().
accept(SocketType, ListenSocket) ->
    accept(SocketType, ListenSocket, infinity).

-spec accept(SocketType, ListenSocket, Timeout) -> {ok, Socket} | {error, Reason} when
      SocketType   :: ip_comm | {ssl, SSLConfig},
      SSLConfig    :: term(),
      Timeout      :: timeout(),
      ListenSocket :: gen_tcp:socket(),
      Socket       :: gen_tcp:socket(),
      Reason       :: term().
accept(ip_comm, ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);
accept({ip_comm, _}, ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);

accept({ssl, _SSLConfig}, ListenSocket, Timeout) ->
    ssl:transport_accept(ListenSocket, Timeout).

%%-------------------------------------------------------------------------
%% Description: Assigns a new controlling process to Socket.
%%-------------------------------------------------------------------------
-spec controlling_process(SocketType, Socket, NewOwner) -> Object when
      SocketType :: ip_comm | {ip_comm | ssl, _Config},
      Socket     :: gen_tcp:socket(),
      NewOwner   :: pid(),
      Object     :: ok | {error, Reason},
      Reason     :: closed | not_owner | badarg | inet:posix().
controlling_process(ip_comm, Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner);
controlling_process({ip_comm, _}, Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner);

controlling_process({ssl, _}, Socket, NewOwner) ->
    ssl:controlling_process(Socket, NewOwner).


%%-------------------------------------------------------------------------
%% Description: Sets one or more options for a socket, using either
%% gen_tcp or ssl.
%%-------------------------------------------------------------------------
-spec setopts(SocketType, Socket, Options) -> ok | {error, inet:posix()} when
      SocketType :: ip_comm | {ip_comm | ssl, _Config},
      Socket     :: inet:socket() | ssl:sslsocket(),
      Options    :: [inet:socket_setopt()] |  [gen_tcp:option()].
setopts(ip_comm, Socket, Options) ->
    inet:setopts(Socket, Options);
setopts({ip_comm, _}, Socket, Options) ->
    inet:setopts(Socket, Options);

setopts({ssl, _}, Socket, Options) ->
    (catch ssl:setopts(Socket, Options)).


%%-------------------------------------------------------------------------
%% Description: Gets the values for some options.
%%-------------------------------------------------------------------------
-spec getopts(SocketType, Socket) -> Object when
      SocketType :: ip_comm | {ip_comm | ssl, _Conf},
      Socket     :: ssl:sslsocket() | inet:socket(),
      Object     :: [gen_tcp:option()] | [inet:socket_setopt() | gen_tcp:pktoptions_value()] | [].
getopts(SocketType, Socket) ->
    Opts = [packet, packet_size, recbuf, sndbuf, priority, tos, send_timeout], 
    getopts(SocketType, Socket, Opts).

-spec getopts(SocketType, Socket, Options) -> Object when
      SocketType :: ip_comm | {ip_comm | ssl, _Conf},
      Socket     :: ssl:sslsocket() | inet:socket(),
      Options    :: [gen_tcp:option_name()],
      Object     :: [gen_tcp:option()] | [inet:socket_setopt() | gen_tcp:pktoptions_value()] | [].
getopts(ip_comm, Socket, Options) ->
    case inet:getopts(Socket, Options) of
	{ok, SocketOpts} ->
	    SocketOpts;
	{error, _} -> 
	    []
    end;
getopts({ip_comm, _}, Socket, Options) ->
    getopts(ip_comm, Socket, Options);
getopts({ssl, _}, Socket, Options) ->
    getopts_ssl(Socket, Options).

-spec getopts_ssl(SslSocket, Options) ->
		     [gen_tcp:option()] | [] when
      SslSocket :: ssl:sslsocket(),
      Options :: [gen_tcp:option_name()].
getopts_ssl(Socket, Options) ->
    case ssl:getopts(Socket, Options) of
	{ok, SocketOpts} ->
	    SocketOpts;
	{error, _} -> 
	    []
    end.

getstat(ip_comm = _SocketType, Socket) ->
    case inet:getstat(Socket) of
	{ok, Stats} ->
	    Stats;
	{error, _} ->
	    []
    end;

getstat({ssl, _} = _SocketType, _Socket) ->
    [].


send(ip_comm, Socket, Message) ->
    gen_tcp:send(Socket, Message);
send({ip_comm, _}, Socket, Message) ->
    gen_tcp:send(Socket, Message);

send({ssl, _}, Socket, Message) ->
    ssl:send(Socket, Message).

close(ip_comm, Socket) ->
    gen_tcp:close(Socket);
close({ip_comm, []}, Socket) ->
    gen_tcp:close(Socket);
close({ssl, _}, Socket) ->
    ssl:close(Socket).

peername(ip_comm, Socket) ->
    do_peername(inet:peername(Socket));
peername({ip_comm,_}, Socket) ->
    do_peername(inet:peername(Socket));
peername({ssl, _}, Socket) ->
    do_peername(ssl:peername(Socket)).

do_peername({ok, {Addr, Port}}) 
  when tuple_size(Addr) =:= 4 ->
    PeerName = ip_name(Addr),
    {Port, PeerName};
do_peername({ok, {Addr, Port}}) 
  when tuple_size(Addr) =:= 8 ->
    PeerName = ip_name(Addr),
    {Port, PeerName};
do_peername({error, _}) ->
    {-1, "unknown"}.

sockname(ip_comm, Socket) ->
    do_sockname(inet:sockname(Socket));
sockname({ip_comm,_}, Socket) ->
    do_sockname(inet:sockname(Socket));
sockname({ssl, _}, Socket) ->
    do_sockname(ssl:sockname(Socket)).

do_sockname({ok, {Addr, Port}}) 
  when tuple_size(Addr) =:= 4 ->
    SockName = ip_name(Addr),
    {Port, SockName};
do_sockname({ok, {Addr, Port}}) 
  when tuple_size(Addr) =:= 8 ->
    SockName = ip_name(Addr),
    {Port, SockName};
do_sockname({error, _}) ->
    {-1, "unknown"}.

resolve() ->
    {ok, Name} = inet:gethostname(),
    Name.

ip_name(Ip) ->
    inet:ntoa(Ip).

close_tag(ip_comm) ->
    tcp_closed;
close_tag(_) ->
    ssl_closed.

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
negotiate({ssl, _}, Socket, Timeout) ->
    negotiate_ssl(Socket, Timeout).

negotiate_ssl(Socket, Timeout) ->
    ssl:handshake(Socket, Timeout). 
