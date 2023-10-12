%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2023. All Rights Reserved.
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
-module(ssl_key_update_SUITE).

-behaviour(ct_suite).

%% Callback functions
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Testcases
-export([key_update_at_client/0,
         key_update_at_client/1,
         key_update_at_server/0,
         key_update_at_server/1,
         explicit_key_update/0,
         explicit_key_update/1]).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_api.hrl").
-include_lib("ssl/src/ssl_connection.hrl").
all() ->
    [{group, 'tlsv1.3'}].

groups() ->
    [{'tlsv1.3', [], tls_1_3_tests()}].

tls_1_3_tests() ->
    [key_update_at_client,
     key_update_at_server,
     explicit_key_update].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
        ok ->
            ssl_test_lib:clean_start(),
            case proplists:get_bool(ecdh, proplists:get_value(public_keys, crypto:supports())) of
                true ->
                    ssl_test_lib:make_ecdsa_cert(Config0);
                false ->
                    {skip, "Missing EC crypto support"}
            end
    catch _:_ ->
            {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:unload(ssl),
    application:stop(crypto).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config).

end_per_group(GroupName, Config) ->
  ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.


%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
key_update_at_client() ->
    [{doc,"Test option 'key_update_at' between erlang client and erlang server."
      "Client initiating the update."}].
key_update_at_client(Config) ->
    key_update_at(Config, client).

key_update_at_server() ->
    [{doc,"Test option 'key_update_at' between erlang client and erlang server."
      "Server initiating the update."}].
key_update_at_server(Config) ->
    key_update_at(Config, server).

key_update_at(Config, Role) ->
    Data = "123456789012345",  %% 15 bytes
    Server = ssl_test_lib:start_server(erlang,
                                       [{options, [{keep_secrets, true},
                                                   {key_update_at, 14}]}],
                                       Config),
    Port = ssl_test_lib:inet_port(Server),
    ClientResult = ssl_test_lib:start_client(erlang,
                                             [return_socket, {port, Port},
                                              {options, [{keep_secrets, true},
                                                         {key_update_at, 14}]}],
                                             Config),
    {Client, ClientSocket} = ClientResult,
    Server ! get_socket,
    ServerSocket = receive
                       {Server, {socket, S}} -> S
                   end,
    Keys0 = get_traffic_secrets(ClientSocket, ServerSocket),
    ?CT_LOG("connected", []),
    {Sender, Receiver} = case Role of
                             client -> {Client, Server};
                             server -> {Server, Client}
                         end,
    %% Sending bytes over limit triggers key update
    ssl_test_lib:send(Sender, Data),
    Data = ssl_test_lib:check_active_receive(Receiver, Data),
    %% TODO check if key has been updated (needs debug logging of secrets)
    ct:sleep(500),
    ?CT_LOG("sent and waited", []),
    Keys1 = get_traffic_secrets(ClientSocket, ServerSocket),
    verify_key_update(Keys0, Keys1),
    %% Test mechanism to prevent infinite loop of key updates
    BigData = binary:copy(<<"1234567890">>, 10),  %% 100 bytes
    ok = ssl_test_lib:send(Sender, BigData),
    ct:sleep(500),
    ?CT_LOG("sent and waited 2", []),
    Keys2 = get_traffic_secrets(ClientSocket, ServerSocket),
    verify_key_update(Keys1, Keys2),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

get_traffic_secrets(ClientSocket, ServerSocket) ->
    ProcessSocket =
        fun(Socket, Role) ->
                {ok, [{keylog, KeyLog}]} = ssl:connection_information(Socket, [keylog]),
                Interesting =
                    fun(S) ->
                            Patterns = ["CLIENT_TRAFFIC_SECRET", "SERVER_TRAFFIC_SECRET"],
                            SearchResults = [string:find(S, P) || P <- Patterns],
                            lists:any(fun(I) -> I /= nomatch end, SearchResults)
                    end,
                TrafficSecrets = lists:filter(Interesting, KeyLog),
                Print = fun(Secret) ->
                                [Name, _A, B] = string:lexemes(Secret, " "),
                                [Key] = io_lib:format("~s", [B]),
                                {Name, {Role, Key}}
                        end,
                [Print(Scr) || Scr <- TrafficSecrets]
        end,
    Secrets = lists:flatten(
                [ProcessSocket(S, R) ||
                    {S, R} <-
                        [{ClientSocket, client}, {ServerSocket, server}]]),
    P = fun(Direction) ->
                Vals = proplists:get_all_values(Direction, Secrets),
                ?CT_LOG("~30s ~10s(c) ~10s(s)",
                     [Direction, proplists:get_value(client, Vals),
                      proplists:get_value(server, Vals)]),
                {Direction, [proplists:get_value(client, Vals),
                             proplists:get_value(server, Vals)]}
        end,
    [P(Direction) ||
        Direction <-
            ["CLIENT_TRAFFIC_SECRET_0", "SERVER_TRAFFIC_SECRET_0"]].

verify_key_update(Keys0, Keys1) ->
    CTS0 = proplists:get_value("CLIENT_TRAFFIC_SECRET_0", Keys0),
    CTS1 = proplists:get_value("CLIENT_TRAFFIC_SECRET_0", Keys1),
    STS0 = proplists:get_value("SERVER_TRAFFIC_SECRET_0", Keys0),
    STS1 = proplists:get_value("SERVER_TRAFFIC_SECRET_0", Keys1),
    CTS = lists:zip(CTS0, CTS1),
    STS = lists:zip(STS0, STS1),
    Pred = fun({A, B}) when A == B ->
                   ct:fail(no_key_update),
                   false;
              (_) ->
                   true
           end,
    [true = lists:all(Pred, X) || X <- [CTS, STS]].

explicit_key_update() ->
    [{doc,"Test ssl:update_key/2 between erlang client and erlang server."}].

explicit_key_update(Config) ->
    Data = "123456789012345",  %% 15 bytes

    Server = ssl_test_lib:start_server(erlang, [], Config),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client(erlang, [{port, Port}], Config),
    ssl_test_lib:send_recv_result_active(Client, Server, Data),

    ssl_test_lib:update_keys(Client, write),
    ssl_test_lib:update_keys(Client, read_write),
    ssl_test_lib:send_recv_result_active(Client, Server, Data),

    ssl_test_lib:update_keys(Server, write),
    ssl_test_lib:update_keys(Server, read_write),
    ssl_test_lib:send_recv_result_active(Client, Server, Data),
    %% TODO check if key has been updated (needs debug logging of secrets)

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
