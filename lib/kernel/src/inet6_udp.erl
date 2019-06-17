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
-module(inet6_udp).

-export([open/1, open/2, close/1]).
-export([send/2, send/4, recv/2, recv/3, connect/3]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, translate_ip/1]).

-include("inet_int.hrl").

-define(FAMILY, inet6).
-define(PROTO, udp).
-define(TYPE, dgram).


%% inet_udp port lookup
getserv(Port) when is_integer(Port) -> {ok, Port};
getserv(Name) when is_atom(Name) -> inet:getservbyname(Name, ?PROTO).

%% inet_udp address lookup
getaddr(Address) -> inet:getaddr(Address, ?FAMILY).
getaddr(Address, Timer) -> inet:getaddr(Address, ?FAMILY, Timer).

%% inet_udp special this side addresses
translate_ip(IP) -> inet:translate_ip(IP, ?FAMILY).

-spec open(_) -> {ok, inet:socket()} | {error, atom()}.
open(Port) -> open(Port, []).

-spec open(_, _) -> {ok, inet:socket()} | {error, atom()}.
open(Port, Opts) ->
    case inet:udp_options(
	   [{port,Port} | Opts],
	   ?MODULE) of
	{error, Reason} -> exit(Reason);
	{ok,
	 #udp_opts{
	    fd = Fd,
	    ifaddr = BAddr = {A,B,C,D,E,F,G,H},
	    port = BPort,
	    opts = SockOpts}}
	when ?ip6(A,B,C,D,E,F,G,H), ?port(BPort) ->
	    inet:open(
	      Fd, BAddr, BPort, SockOpts, ?PROTO, ?FAMILY, ?TYPE, ?MODULE);
	{ok, _} -> exit(badarg)
    end.

send(S, {A,B,C,D,E,F,G,H} = IP, Port, Data)
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port) ->
    prim_inet:sendto(S, {IP, Port}, [], Data);
send(S, {{A,B,C,D,E,F,G,H}, Port} = Addr, AncData, Data)
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port), is_list(AncData) ->
    prim_inet:sendto(S, Addr, AncData, Data);
send(S, {?FAMILY, {{A,B,C,D,E,F,G,H}, Port}} = Address, AncData, Data)
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port), is_list(AncData) ->
    prim_inet:sendto(S, Address, AncData, Data);
send(S, {?FAMILY, {loopback, Port}} = Address, AncData, Data)
  when ?port(Port), is_list(AncData) ->
    prim_inet:sendto(S, Address, AncData, Data).

send(S, Data) ->
    prim_inet:sendto(S, {any, 0}, [], Data).
    
connect(S, Addr = {A,B,C,D,E,F,G,H}, Port) 
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port) ->
    prim_inet:connect(S, Addr, Port).

recv(S, Len) ->
    prim_inet:recvfrom(S, Len).

recv(S, Len, Time) ->
    prim_inet:recvfrom(S, Len, Time).

-spec close(inet:socket()) -> ok.
close(S) ->
    inet:udp_close(S).

%%
%% Set controlling process:
%% 1) First sync socket into a known state
%% 2) Move all messages onto the new owners message queue
%% 3) Commit the owner 
%% 4) Wait for ack of new Owner (since socket does some link and unlink)
%%

controlling_process(Socket, NewOwner) ->
    inet:udp_controlling_process(Socket, NewOwner).

%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    inet:fdopen(Fd, Opts, ?PROTO, ?FAMILY, ?TYPE, ?MODULE).
