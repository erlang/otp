%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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
         explicit_key_update/1,
         keylog_client_cb/0,
         keylog_client_cb/1,
         keylog_server_cb/0,
         keylog_server_cb/1,
         key_update_unexpected_msg/0,
         key_update_unexpected_msg/1
        ]).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_api.hrl").
-include_lib("ssl/src/ssl_connection.hrl").
all() ->
    [{group, 'tlsv1.3'}].

groups() ->
    [{'tlsv1.3', [], [{group, transport_socket} | tls_1_3_tests()]},
     {transport_socket, [], tls_1_3_tests()}].

tls_1_3_tests() ->
    [key_update_at_client,
     key_update_at_server,
     explicit_key_update,
     keylog_client_cb,
     keylog_server_cb,
     key_update_unexpected_msg].

init_per_suite(Config0) ->
    case application:ensure_started(crypto) of
        ok ->
            ssl_test_lib:clean_start(),
            case proplists:get_bool(ecdh, proplists:get_value(public_keys, crypto:supports())) of
                true ->
                    ssl_test_lib:make_ecdsa_cert(Config0);
                false ->
                    {skip, "Missing EC crypto support"}
            end;
        _ ->
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

keylog_client_cb() ->
    [{doc,"Test option {keep_secrets, {keylog, fun()}}"}].
keylog_client_cb(Config) ->
    Data = "123456789012345",  %% 15 bytes
    TestCase = self(),
    Fun = fun(KeyLogInfo) ->
                  TestCase ! {keylog, KeyLogInfo}
          end,
    Server = ssl_test_lib:start_server(erlang,[], Config),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, ClientSocket} =
        ssl_test_lib:start_client(erlang, [return_socket,{port, Port},
                                          {options, [{keep_secrets, {keylog, Fun}},
                                                     {key_update_at, 9}]}],Config),
    ssl:send(ClientSocket, Data),
    receive
        {keylog, #{items := HSKeylog}} ->
            ["CLIENT_HANDSHAKE_TRAFFIC_SECRET" ++ _| _] = HSKeylog
    end,
    Role = receive
               {keylog, #{items := TConKeylog0}} ->
                   traffic_secret_0(TConKeylog0)
           end,
    OppositeRole = opposite_role(Role),
    receive
        {keylog, #{items := TConKeylog2}} ->
            OppositeRole = traffic_secret_0(TConKeylog2)
    end,
    ok = traffic_secret_1_and_2([{client,1}, {client, 2}, {server,1}, {server, 2}]).

keylog_server_cb() ->
    [{doc,"Test option {keep_secrets, {keylog, fun()}}"}].
keylog_server_cb(Config) ->
    Data = "123456789012345",  %% 15 bytes
    TestCase = self(),
    Fun = fun(KeyLogInfo) ->
                  TestCase ! {keylog, KeyLogInfo}
          end,
    Server = ssl_test_lib:start_server(erlang,[{options, [{keep_secrets, {keylog, Fun}},
                                                          {key_update_at, 9}]}], Config),
    Port = ssl_test_lib:inet_port(Server),
    
    _ = ssl_test_lib:start_client(erlang, [{port, Port}], Config),

    Server ! get_socket,
    ServerSocket = receive
                       {Server, {socket, S}} -> S
                   end,
    ssl:send(ServerSocket, Data),
    receive
        {keylog, #{items := HSKeylog}} ->
             ["CLIENT_HANDSHAKE_TRAFFIC_SECRET" ++ _| _] = HSKeylog
    end,
    Role = receive
               {keylog, #{items := TConKeylog0}} ->
                   traffic_secret_0(TConKeylog0)
           end,
    OppositeRole = opposite_role(Role),
    receive
        {keylog, #{items := TConKeylog2}} ->
            OppositeRole = traffic_secret_0(TConKeylog2)
    end,
    ok = traffic_secret_1_and_2([{client,1}, {client, 2}, {server,1}, {server, 2}]).


key_update_unexpected_msg() ->
    [{doc,"Test that internla sync messages are not sent to socket user"}].
key_update_unexpected_msg(Config) ->
    Data = "123456789012345",  %% 15 bytes
    Server = ssl_test_lib:start_server(erlang,[], Config),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = ssl:connect(net_adm:localhost(), Port, [{verify, verify_none}, {key_update_at, 9}]),

    ok = ssl:send(Socket, Data),

    receive
        {_, ok} = Msg ->
            ct:fail({unexpected_message, Msg})
    after 500 ->
          ok
    end.

%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------
traffic_secret_1_and_2([]) ->
    ok;
traffic_secret_1_and_2([_|_] = List) ->
    receive
        {keylog, #{items := ["SERVER_TRAFFIC_SECRET_1" ++ _| _]}} ->
            traffic_secret_1_and_2(lists:delete({server, 1}, List));
        {keylog, #{items := ["CLIENT_TRAFFIC_SECRET_1" ++ _| _]}} ->
            traffic_secret_1_and_2(lists:delete({client, 1}, List));
        {keylog, #{items := ["SERVER_TRAFFIC_SECRET_2" ++ _| _]}} ->
            traffic_secret_1_and_2(lists:delete({server, 2}, List));
        {keylog, #{items := ["CLIENT_TRAFFIC_SECRET_2" ++ _| _]}} ->
            traffic_secret_1_and_2(lists:delete({client, 2}, List))
    end.

traffic_secret_0(KeyLog) ->
    case KeyLog of
        ["CLIENT_TRAFFIC_SECRET_0" ++ _| _] ->
            client;
        ["SERVER_TRAFFIC_SECRET_0" ++ _| _] ->
            server
    end.

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.

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
    %% Verify prevention 
    {["CLIENT_TRAFFIC_SECRET_10" ++ _| _], ["CLIENT_TRAFFIC_SECRET_10" ++ _| _]} = Keys2,
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

get_traffic_secrets(ClientSocket, ServerSocket) ->
    {ok, [{keylog, KeyLog1}]} = ssl:connection_information(ClientSocket, [keylog]),
    {ok, [{keylog, KeyLog2}]} = ssl:connection_information(ServerSocket, [keylog]),
    {KeyLog1, KeyLog2}.


%% Only traffic secrets
verify_key_update({[TS1, TS2, TS3, TS4, TS5, TS6],
                   [TS1, TS2, TS3, TS4, TS5, TS6]} = TSC,
                  {[TS7, TS8, TS9, TS10, TS11, TS12],
                   [TS7, TS8, TS9, TS10, TS11, TS12]}= TSS
                 ) ->
    TSC =/= TSS;
%% Handshake + traffic secrets
verify_key_update({[CH1, SH1 | T1], [CH1, SH1 | T1]},
                  {[CH2, SH2 | T2], [CH2, SH2 | T2]}) ->
    CH1 =/= CH2 andalso
        SH1 =/= SH2 andalso
        T1 =/= T2.

