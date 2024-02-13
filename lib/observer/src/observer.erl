%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2024. All Rights Reserved.
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
-moduledoc """
A GUI tool for observing an Erlang system.

Observer is a graphical tool for observing the characteristics of Erlang
systems. The tool Observer displays system information, application supervisor
trees, process information, ETS tables, Mnesia tables, and contains a front end
for Erlang tracing with module `m:ttb`.

For details about how to get started, see the [`User's Guide`](observer_ug.md).
""".
-moduledoc(#{since => "OTP R15B"}).

-export([start/0, start/1, start_and_wait/0, start_and_wait/1, stop/0]).

-doc """
start() -> ok

Starts the Observer GUI. To stop the tool, close the window or call `stop/0`.
""".
-doc(#{since => <<"OTP R15B">>}).
-spec start() -> ok | {error, term()}.
start() ->
    observer_wx:start().

-doc """
start(Node) -> ok

Starts the Observer GUI and tries to connect it to `Node`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec start(node()|[node()]) -> ok | {error, term()}.
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

-doc """
start_and_wait() -> ok

Starts the Observer GUI and only return when it is either stopped or the window
is closed
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec start_and_wait() -> ok.
start_and_wait() ->
    ok = start(),
    MonitorRef = monitor(process, observer),
    receive
        {'DOWN', MonitorRef, process, _, _} ->
            ok
    end.

-doc """
start_and_wait(Node) -> ok

Starts the Observer GUI and only return when it is either stopped or the window
is closed, connects it directly to `Node` like `start/1`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec start_and_wait(node()|[node()]) -> ok.
start_and_wait(Node) when is_atom(Node) ->
    start_and_wait([Node]);
start_and_wait(List) when is_list(List) ->
    ok = start(List),
    MonitorRef = monitor(process, observer),
    receive
        {'DOWN', MonitorRef, process, _, _} ->
            ok
    end.

-doc """
stop() -> ok

Stops the Observer GUI.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec stop() -> ok.
stop() ->
    observer_wx:stop().

to_atom(Node) when is_atom(Node) ->
    Node;
to_atom(Node) when is_list(Node) ->
    list_to_atom(Node).
