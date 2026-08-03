%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Piotr Dorobisz 2011. All Rights Reserved.
%% Copyright Andrzej Teleżyński. All Rights Reserved.
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(client).
-compile(export_all).

-compile([{nowarn_possibly_unsafe_function, {erlang, list_to_atom, 1}}]).

init(Node) ->
    application:start(runtime_tools),
    net_kernel:connect_node(Node).

init() ->
    init(server_node()).

restart() ->
    init:restart().

server_node() ->
    {ok,HostName} = inet:gethostname(),
    list_to_atom("server@" ++ HostName).

get() ->
    erlang:send({server,server_node()}, {get,self()}),
    receive Data -> Data
    after 1000   -> no_reply
    end.

put(Thing) ->
    erlang:send({server,server_node()}, {put,self(),Thing}),
    receive ok -> timer:sleep(2), ok
    after 1000 -> no_reply
    end.
