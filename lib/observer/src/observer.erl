%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2023. All Rights Reserved.
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

-module(observer).

-export([start/0, start/1, start_and_wait/0, start_and_wait/1, stop/0]).


start() ->
    observer_wx:start().

start(Node) when is_atom(Node) ->
    start([Node]);
start([Node]) ->
    Node1 = to_atom(Node),
    case net_kernel:connect_node(Node1) of
        true ->
            case observer_wx:start() of
                ok ->
                    observer_wx:set_node(Node1),
                    ok;
                Err ->
                    Err
            end;
        _ ->
            {error, failed_to_connect}
    end.

start_and_wait() ->
    ok = start(),
    MonitorRef = monitor(process, observer),
    receive
        {'DOWN', MonitorRef, process, _, _} ->
            ok
    end.

start_and_wait(Node) when is_atom(Node) ->
    start_and_wait([Node]);
start_and_wait(List) when is_list(List) ->
    ok = start(List),
    MonitorRef = monitor(process, observer),
    receive
        {'DOWN', MonitorRef, process, _, _} ->
            ok
    end.

stop() ->
    observer_wx:stop().

to_atom(Node) when is_atom(Node) ->
    Node;
to_atom(Node) when is_list(Node) ->
    list_to_atom(Node).
