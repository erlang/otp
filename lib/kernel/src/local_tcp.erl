%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(local_tcp).

%% Socket server for TCP/IP

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, send/3, recv/2, recv/3, unrecv/2]).
-export([shutdown/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).
-export([translate_ip/1]).

-include("inet_int.hrl").

-define(FAMILY, local).
-define(PROTO, tcp).
-define(TYPE, stream).

%% port lookup
getserv(0) -> {ok, 0}.

%% no address lookup
getaddr({?FAMILY, _} = Address) -> {ok, Address}.
getaddr({?FAMILY, _} = Address, _Timer) -> {ok, Address}.

%% no address lookup
getaddrs({?FAMILY, _} = Address) -> {ok, [Address]}.
getaddrs({?FAMILY, _} = Address, _Timer) -> {ok, [Address]}.

%% special this side addresses
translate_ip(IP) -> IP.

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
%%
connect(Address, Port, Opts, infinity) ->
    do_connect(Address, Port, Opts, infinity);
connect(Address, Port, Opts, Timeout)
  when is_integer(Timeout), Timeout >= 0 ->
    do_connect(Address, Port, Opts, Timeout).

do_connect(Addr = {?FAMILY, _}, 0, Opts, Time) ->
    case inet:connect_options(Opts, ?MODULE) of
	{error, Reason} -> exit(Reason);
	{ok,
	 #connect_opts{
	    fd = Fd,
	    ifaddr = BAddr,
	    port = 0,
	    opts = SockOpts}}
	when tuple_size(BAddr) =:= 2, element(1, BAddr) =:= ?FAMILY;
	     BAddr =:= any ->
	    case inet:open(
		   Fd,
		   case BAddr of
		       any ->
			   undefined;
		       _ ->
			   BAddr
		   end,
		   0, SockOpts, ?PROTO, ?FAMILY, ?TYPE, ?MODULE) of
		{ok, S} ->
		    case prim_inet:connect(S, Addr, 0, Time) of
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
listen(0, Opts) ->
    case inet:listen_options([{port,0} | Opts], ?MODULE) of
	{error, Reason} -> exit(Reason);
	{ok,
	 #listen_opts{
	    fd = Fd,
	    ifaddr = BAddr,
	    port = 0,
	    opts = SockOpts} = R}
	when tuple_size(BAddr) =:= 2, element(1, BAddr) =:= ?FAMILY;
	     BAddr =:= any ->
	    case inet:open(
		   Fd, BAddr, 0, SockOpts,
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
    case prim_inet:accept(L) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.
%%
accept(L, Timeout) ->
    case prim_inet:accept(L, Timeout) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.

%%
%% Create a port/socket from a file descriptor
%%
fdopen(Fd, Opts) ->
    inet:open(Fd, undefined, 0, Opts, ?PROTO, ?FAMILY, ?TYPE, ?MODULE).
