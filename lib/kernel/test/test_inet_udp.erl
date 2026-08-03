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
-module(test_inet_udp).
-moduledoc false.

-export([open/1, open/2, close/1]).
-export([send/2, send/4, recv/2, recv/3, connect/2, connect/3]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, translate_ip/1]).

%% -define(FAMILY, inet).
%% -define(PROTO,  udp).
%% -define(TYPE,   dgram).

-define(MOD, inet_udp).
-define(NOTIFY(), self() ! {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}).


%% inet_udp port lookup
getserv(Port) when is_integer(Port) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Port);
getserv(Name) when is_atom(Name) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Name).

%% inet_udp address lookup
getaddr(Address)        -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Address).
getaddr(Address, Timer) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Address, Timer).

%% inet_udp special this side addresses
translate_ip(IP) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(IP).

-spec open(_) -> {ok, port()} | {error, atom()}.
open(Port) -> ?NOTIFY(), ?MOD:?FUNCTION_NAME(Port).

-spec open(_, _) -> {ok, port()} | {error, atom()}.
open(Port, Opts) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Port, Opts).

send(S, Arg1, Arg2, Arg3) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(S, Arg1, Arg2, Arg3).

send(S, Data) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(S, Data).
    
connect(S, SockAddr) -> 
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(S, SockAddr).

connect(S, Addr, Port) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(S, Addr, Port).

recv(S, Len) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(S, Len).

recv(S, Len, Time) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(S, Len, Time).

-spec close(port()) -> ok.
close(S) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(S).

controlling_process(S, NewOwner) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(S, NewOwner).

%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    ?NOTIFY(), ?MOD:?FUNCTION_NAME(Fd, Opts).

