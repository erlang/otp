%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(inet6_udp).

-export([open/1, open/2, close/1]).
-export([send/2, send/4, recv/2, recv/3, connect/3]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2]).

-include("inet_int.hrl").

%% inet_udp port lookup
getserv(Port) when is_integer(Port) -> {ok, Port};
getserv(Name) when is_atom(Name)    -> inet:getservbyname(Name,udp).

%% inet_udp address lookup
getaddr(Address) -> inet:getaddr(Address, inet6).
getaddr(Address,Timer) -> inet:getaddr(Address, inet6, Timer).

open(Port) -> open(Port, []).

open(Port, Opts) ->
    case inet:udp_options([{port,Port} | Opts], inet6) of
	{error, Reason} -> exit(Reason);
	{ok, #udp_opts{fd=Fd,
		       ifaddr=BAddr={A,B,C,D,E,F,G,H},
		       port=BPort,
		       opts=SockOpts}}
	when ?ip6(A,B,C,D,E,F,G,H), ?port(BPort) ->
	    inet:open(Fd,BAddr,BPort,SockOpts,udp,inet6,?MODULE);
	{ok, _} -> exit(badarg)
    end.

send(S, Addr = {A,B,C,D,E,F,G,H}, P, Data)
  when ?ip6(A,B,C,D,E,F,G,H), ?port(P) ->
    prim_inet:sendto(S, Addr, P, Data).

send(S, Data) ->
    prim_inet:sendto(S, {0,0,0,0,0,0,0,0}, 0, Data).
    
connect(S, Addr = {A,B,C,D,E,F,G,H}, P) 
  when ?ip6(A,B,C,D,E,F,G,H), ?port(P) ->
    prim_inet:connect(S, Addr, P).

recv(S,Len) ->
    prim_inet:recvfrom(S, Len).

recv(S,Len,Time) ->
    prim_inet:recvfrom(S, Len, Time).

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
    inet:fdopen(Fd, Opts, udp, inet6, ?MODULE).
