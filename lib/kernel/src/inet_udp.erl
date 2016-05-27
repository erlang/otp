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
-module(inet_udp).

-export([open/1, open/2, close/1]).
-export([send/2, send/4, recv/2, recv/3, connect/3]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, translate_ip/1]).

-include("inet_int.hrl").

-define(FAMILY, inet).
-define(PROTO, udp).
-define(TYPE, dgram).
-define(RECBUF, (8*1024)).


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
	   [{port,Port}, {recbuf, ?RECBUF} | Opts], 
	   ?MODULE) of
	{error, Reason} -> exit(Reason);
	{ok,
	 #udp_opts{
	    fd = Fd,
	    ifaddr = BAddr = {A,B,C,D},
	    port = BPort,
	    opts = SockOpts}}
	  when ?ip(A,B,C,D), ?port(BPort) ->
	    inet:open(
	      Fd, BAddr, BPort, SockOpts, ?PROTO, ?FAMILY, ?TYPE, ?MODULE);
	{ok, _} -> exit(badarg)
    end.

send(S, {A,B,C,D} = Addr, P, Data)
  when ?ip(A,B,C,D), ?port(P) ->
    prim_inet:sendto(S, Addr, P, Data).

send(S, Data) ->
    prim_inet:sendto(S, {0,0,0,0}, 0, Data).
    
connect(S, Addr = {A,B,C,D}, P)
  when ?ip(A,B,C,D), ?port(P) ->
    prim_inet:connect(S, Addr, P).

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
    inet:fdopen(
      Fd, optuniquify([{recbuf, ?RECBUF} | Opts]),
      ?PROTO, ?FAMILY, ?TYPE, ?MODULE).


%% Remove all duplicate options from an option list.
%% The last occurring duplicate is used, and the order is preserved.
%% 
%% Here's how:
%%   Reverse the list.
%%   For each head option go through the tail and remove
%%   all occurences of the same option from the tail.
%%   Store that head option and iterate using the new tail.
%%   Return the list of stored head options.
optuniquify(List) ->
    optuniquify(lists:reverse(List), []).

optuniquify([], Result) ->
    Result;
optuniquify([Opt | Tail], Result) ->
    %% Remove all occurences of Opt in Tail, 
    %% prepend Opt to Result, 
    %% then iterate back here.
    optuniquify(Opt, Tail, [], Result).

%% All duplicates of current option are now removed
optuniquify(Opt, [], Rest, Result) ->
    %% Store unique option
    optuniquify(lists:reverse(Rest), [Opt | Result]);
%% Duplicate option tuple
optuniquify(Opt0, [Opt1 | Tail], Rest, Result)
  when tuple_size(Opt0) =:= tuple_size(Opt1), 
       element(1, Opt0) =:= element(1, Opt1) ->
    %% Waste duplicate
    optuniquify(Opt0, Tail, Rest, Result);
%% Duplicate option atom or other term
optuniquify(Opt, [Opt | Tail], Rest, Result) ->
    %% Waste duplicate
    optuniquify(Opt, Tail, Rest, Result);
%% Non-duplicate option
optuniquify(Opt, [X | Tail], Rest, Result) ->
    %% Keep non-duplicate
    optuniquify(Opt, Tail, [X | Rest], Result).
