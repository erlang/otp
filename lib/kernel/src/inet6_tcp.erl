%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
-module(inet6_tcp).

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, send/3, recv/2, recv/3, unrecv/2]).
-export([shutdown/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).

-include("inet_int.hrl").

%% inet_tcp port lookup
getserv(Port) when is_integer(Port) -> {ok, Port};
getserv(Name) when is_atom(Name)    -> inet:getservbyname(Name,tcp).

%% inet_tcp address lookup
getaddr(Address) -> inet:getaddr(Address, inet6).
getaddr(Address,Timer) -> inet:getaddr_tm(Address, inet6, Timer).

%% inet_tcp address lookup
getaddrs(Address) -> inet:getaddrs(Address, inet6).
getaddrs(Address,Timer) -> inet:getaddrs_tm(Address,inet6,Timer).

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
%% Close a socket (async)
%%
close(Socket) -> 
    inet:tcp_close(Socket).

%%
%% Shutdown one end of a socket
%%
shutdown(Socket, How) ->
    prim_inet:shutdown(Socket, How).

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
connect(Address, Port, Opts, Timeout) when is_integer(Timeout), 
                                           Timeout >= 0 ->
    do_connect(Address, Port, Opts, Timeout).

do_connect(Addr = {A,B,C,D,E,F,G,H}, Port, Opts, Time) when 
  ?ip6(A,B,C,D,E,F,G,H), ?port(Port) ->
    case inet:connect_options(Opts, inet6) of
	{error, Reason} -> exit(Reason);
	{ok, #connect_opts{fd=Fd,
			   ifaddr=BAddr={Ab,Bb,Cb,Db,Eb,Fb,Gb,Hb},
			   port=BPort,
			   opts=SockOpts}}
	when ?ip6(Ab,Bb,Cb,Db,Eb,Fb,Gb,Hb), ?port(BPort) ->
	    case inet:open(Fd,BAddr,BPort,SockOpts,tcp,inet6,stream,?MODULE) of
		{ok, S} ->
		    case prim_inet:connect(S, Addr, Port, Time) of
			ok    -> {ok,S};
			Error ->  prim_inet:close(S), Error
		    end;
		Error -> Error
	    end;
	{ok, _} -> exit(badarg)
    end.

%% 
%% Listen
%%
listen(Port, Opts) ->
    case inet:listen_options([{port,Port} | Opts], inet6) of
	{error, Reason} -> exit(Reason);
	{ok, #listen_opts{fd=Fd,
			  ifaddr=BAddr={A,B,C,D,E,F,G,H},
			  port=BPort,
			  opts=SockOpts}=R}
	when ?ip6(A,B,C,D,E,F,G,H), ?port(BPort) ->
	    case inet:open(Fd,BAddr,BPort,SockOpts,tcp,inet6,stream,?MODULE) of
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
    case prim_inet:accept(L) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.
	    
accept(L,Timeout) -> 
    case prim_inet:accept(L,Timeout) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.
    
%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    inet:fdopen(Fd, Opts, tcp, inet6, stream, ?MODULE).

