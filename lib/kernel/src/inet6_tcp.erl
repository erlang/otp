%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-module(inet6_tcp).

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, send/3, recv/2, recv/3, unrecv/2]).
-export([shutdown/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([family/0, mask/2, parse_address/1]). % inet_tcp_dist
-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).
-export([translate_ip/1]).

-include("inet_int.hrl").

-define(FAMILY, inet6).
-define(PROTO, tcp).
-define(TYPE, stream).

%% my address family
family() -> ?FAMILY.

%% Apply netmask on address
mask({M1,M2,M3,M4,M5,M6,M7,M8}, {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8}) ->
    {M1 band IP1,
     M2 band IP2,
     M3 band IP3,
     M4 band IP4,
     M5 band IP5,
     M6 band IP6,
     M7 band IP7,
     M8 band IP8 }.

%% Parse address string
parse_address(Host) ->
    inet_parse:ipv6strict_address(Host).

%% inet_tcp port lookup
getserv(Port) when is_integer(Port) -> {ok, Port};
getserv(Name) when is_atom(Name) -> inet:getservbyname(Name, ?PROTO).

%% inet_tcp address lookup
getaddr(Address) -> inet:getaddr(Address, ?FAMILY).
getaddr(Address, Timer) -> inet:getaddr_tm(Address, ?FAMILY, Timer).

%% inet_tcp address lookup
getaddrs(Address) -> inet:getaddrs(Address, ?FAMILY).
getaddrs(Address, Timer) -> inet:getaddrs_tm(Address, ?FAMILY, Timer).

%% inet_udp special this side addresses
translate_ip(IP) -> inet:translate_ip(IP, ?FAMILY).

%%
%% Send data on a socket
%%
send(Socket, Packet, Opts) -> prim_inet:send(Socket, Packet, Opts).
send(Socket, Packet) -> prim_inet:send(Socket, Packet, []).

%%
%% Receive data from a socket (inactive only)
%%
recv(Socket, Length) -> prim_inet:recv(Socket, Length).
recv(Socket, Length, Timeout) -> prim_inet:recv(Socket, Length, Timeout).

unrecv(Socket, Data) -> prim_inet:unrecv(Socket, Data).

%%
%% Shutdown one end of a socket
%%
shutdown(Socket, How) ->
    prim_inet:shutdown(Socket, How).

%%
%% Close a socket (async)
%%
close(Socket) ->
    inet:tcp_close(Socket).

%%
%% Set controlling process
%% FIXME: move messages to new owner!!!
%%
controlling_process(Socket, NewOwner) ->
    inet:tcp_controlling_process(Socket, NewOwner).

%%
%% Connect
%%
connect(Address, Port, Opts) ->
    do_connect(Address, Port, Opts, infinity).

connect(Address, Port, Opts, infinity) ->
    do_connect(Address, Port, Opts, infinity);
connect(Address, Port, Opts, Timeout)
  when is_integer(Timeout), Timeout >= 0 ->
    do_connect(Address, Port, Opts, Timeout).

do_connect(Addr = {A,B,C,D,E,F,G,H}, Port, Opts, Time)
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port) ->
    case inet:connect_options(Opts, ?MODULE) of
	{error, Reason} -> exit(Reason);
	{ok,
	 #connect_opts{
	    fd = Fd,
	    ifaddr = BAddr = {Ab,Bb,Cb,Db,Eb,Fb,Gb,Hb},
	    port = BPort,
	    opts = SockOpts}}
	when ?ip6(Ab,Bb,Cb,Db,Eb,Fb,Gb,Hb), ?port(BPort) ->
	    case inet:open(
		   Fd, BAddr, BPort, SockOpts,
		   ?PROTO, ?FAMILY, ?TYPE, ?MODULE) of
		{ok, S} ->
		    case prim_inet:connect(S, Addr, Port, Time) of
			ok -> {ok,S};
			Error -> prim_inet:close(S), Error
		    end;
		Error -> Error
	    end;
	{ok, _} -> exit(badarg)
    end.

%% 
%% Listen
%%
listen(Port, Opts) ->
    case inet:listen_options([{port,Port} | Opts], ?MODULE) of
	{error, Reason} -> exit(Reason);
	{ok,
	 #listen_opts{
	    fd = Fd,
	    ifaddr = BAddr = {A,B,C,D,E,F,G,H},
	    port = BPort,
	    opts = SockOpts} = R}
	when ?ip6(A,B,C,D,E,F,G,H), ?port(BPort) ->
	    case inet:open(
		   Fd, BAddr, BPort, SockOpts,
		   ?PROTO, ?FAMILY, ?TYPE, ?MODULE) of
		{ok, S} ->
		    case prim_inet:listen(S, R#listen_opts.backlog) of
			ok -> {ok, S};
			Error -> prim_inet:close(S), Error
		    end;
		Error -> Error
	    end;
	{ok, _} -> exit(badarg)
    end.

%%
%% Accept
%%
accept(L) -> 
    case prim_inet:accept(L, accept_family_opts()) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.

accept(L, Timeout) ->
    case prim_inet:accept(L, Timeout, accept_family_opts()) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.

accept_family_opts() -> [tclass, recvtclass].

%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    inet:fdopen(Fd, Opts, ?PROTO, ?FAMILY, ?TYPE, ?MODULE).
