%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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
-module(test_inet_tcp).
-moduledoc false.

%% Socket server for TCP/IP

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, send/3, recv/2, recv/3, unrecv/2]).
-export([shutdown/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([family/0, mask/2, parse_address/1]). % inet_tcp_dist
-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).
-export([translate_ip/1]).


%% -define(FAMILY, inet).
%% -define(PROTO,  tcp).
%% -define(TYPE,   stream).

-define(MOD, inet_tcp).
-define(NOTIFY(), self() ! {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}).


%% my address family
family() -> ?NOTIFY(), ?MOD:?FUNCTION_NAME().

%% Apply netmask on address
mask(M, IP) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(M, IP).

%% Parse address string
parse_address(Host) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Host).

%% inet_tcp port lookup
getserv(Arg) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Arg).

%% inet_tcp address lookup
getaddr(Address)        -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Address).
getaddr(Address, Timer) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Address, Timer).

%% inet_tcp address lookup
getaddrs(Address)        -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Address).
getaddrs(Address, Timer) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Address, Timer).

%% inet_udp special this side addresses
translate_ip(IP) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(IP).

%%
%% Send data on a socket
%%
send(Socket, Packet, Opts) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Socket, Packet, Opts).
send(Socket, Packet)       -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Socket, Packet).

%%
%% Receive data from a socket (inactive only)
%%
recv(Socket, Length)          -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Socket, Length).
recv(Socket, Length, Timeout) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Socket, Length, Timeout).

unrecv(Socket, Data) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Socket, Data).

%%
%% Shutdown one end of a socket
%%
shutdown(Socket, How) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Socket, How).

%%
%% Close a socket (async)
%%
close(Socket) -> 
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Socket).

%%
%% Set controlling process
%%
controlling_process(Socket, NewOwner) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Socket, NewOwner).

%%
%% Connect
%%
connect(Arg1, Arg2, Arg3) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Arg1, Arg2, Arg3).

connect(Arg1, Arg2, Arg3, Arg4) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Arg1, Arg2, Arg3, Arg4).


%% 
%% Listen
%%
listen(Arg1, Arg2) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Arg1, Arg2).

%%
%% Accept
%%
accept(Arg1) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Arg1).

accept(Arg1, Arg2) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Arg1, Arg2).


%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Fd, Opts).
