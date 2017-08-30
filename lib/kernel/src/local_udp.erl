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
-module(local_udp).

-export([open/1, open/2, close/1]).
-export([send/2, send/4, recv/2, recv/3, connect/3]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, translate_ip/1]).

-include("inet_int.hrl").

-define(FAMILY, local).
-define(PROTO, udp).
-define(TYPE, dgram).


%% port lookup
getserv(0) -> {ok, 0}.

%% no address lookup
getaddr({?FAMILY, _} = Address) -> {ok, Address}.
getaddr({?FAMILY, _} = Address, _Timer) -> {ok, Address}.

%% special this side addresses
translate_ip(IP) -> IP.

open(0) -> open(0, []).
%%
open(0, Opts) ->
    case inet:udp_options(
	   [{port,0} | Opts],
	   ?MODULE) of
	{error, Reason} -> exit(Reason);
	{ok,
	 #udp_opts{
	    fd = Fd,
	    ifaddr = BAddr,
	    port = 0,
	    opts = SockOpts}}
	when tuple_size(BAddr) =:= 2, element(1, BAddr) =:= ?FAMILY;
	     BAddr =:= any ->
	    inet:open(
	      Fd,
	      case BAddr of
		  any ->
		      undefined;
		  _ ->
		      BAddr
	      end,
	      0, SockOpts, ?PROTO, ?FAMILY, ?TYPE, ?MODULE);
	{ok, _} -> exit(badarg)
    end.

send(S, Addr = {?FAMILY,_}, 0, Data) ->
    prim_inet:sendto(S, Addr, 0, Data).
%%
send(S, Data) ->
    prim_inet:sendto(S, {?FAMILY,<<>>}, 0, Data).

connect(S, Addr = {?FAMILY,_}, 0) ->
    prim_inet:connect(S, Addr, 0).

recv(S, Len) ->
    prim_inet:recvfrom(S, Len).
%%
recv(S, Len, Time) ->
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
    inet:fdopen(Fd, Opts, ?PROTO, ?FAMILY, ?TYPE, ?MODULE).
