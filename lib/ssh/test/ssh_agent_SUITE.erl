%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

%%

-module(ssh_agent_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh_agent.hrl").
-include("ssh_test_lib.hrl").
-include("ssh.hrl").

-compile(export_all).

%% Test configuration

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [request_identities].

init_per_suite(Config) ->
    ok = ssh:start(),
    Config.

end_per_suite(_Config) ->
    ok = ssh:stop().

init_per_testcase(_TestCase, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    PathPrefix = filename:join(PrivDir, "agent."),
    SocketPath = test_server:temp_name(PathPrefix),
    put_socket_path(Config, SocketPath).

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test cases

request_identities() ->
    [{doc, "Request a list of identities"}].

request_identities(Config) ->
    Request = #ssh_agent_identities_request{},
    SocketPath = get_socket_path(Config),

    agent(Config, <<?UINT32(41),            % message length
                    ?BYTE(12),              % message type   (1 byte)
                    ?UINT32(2),             % number of keys (4 bytes)
                    ?STRING(<<"lorem">>),   % key 1 comment  (4 + 5 bytes)
                    ?STRING(<<"key-1">>),   % key 1 blob     (4 + 5 bytes)
                    ?STRING(<<"ipsum">>),   % key 2 comment  (4 + 5 bytes)
                    ?STRING(<<"key-2">>)    % key 2 blob     (4 + 5 bytes)
                  >>),

    Response = ssh_agent:send(Request, SocketPath, infinity),

    #ssh_agent_identities_response{keys = Keys} = Response,

    [{ssh_agent_key,<<"lorem">>,<<"key-1">>},
     {ssh_agent_key,<<"ipsum">>,<<"key-2">>}] = Keys,

    ok.

%% Utility functions

put_socket_path(Config, SocketPath) ->
    [{socket_path, SocketPath} | Config].

get_socket_path(Config) ->
    proplists:get_value(socket_path, Config).

%% Mock agent

agent(Config, BinResponse) ->
    SocketPath = get_socket_path(Config),
    Parent = self(),

    spawn(fun() ->
              Address = {local, SocketPath},
              ConnectOpts = [local, binary, {ip, Address}, {packet, 0}, {active, false}],

              {ok, ListenSocket} = gen_tcp:listen(0, ConnectOpts),
              {ok, Socket} = gen_tcp:accept(ListenSocket),
              {ok, BinRequest} = gen_tcp:recv(Socket, 0),

              Parent ! {request, BinRequest},

              ok = gen_tcp:send(Socket, BinResponse),

              ok = gen_tcp:close(Socket),
              ok = file:delete(SocketPath)
          end).
