%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2022. All Rights Reserved.
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
                                       [{options, [{key_update_at, 14}]}],
                                       Config),
    Port = ssl_test_lib:inet_port(Server),
    {Client,
     #sslsocket{pid =
                    [ClientReceiverPid, ClientSenderPid]}} =
        ssl_test_lib:start_client(erlang,
                                  [return_socket, {port, Port},
                                   {options, [{key_update_at, 14}]}],
                                  Config),
    Server ! get_socket,
    #sslsocket{pid =
                   [ServerReceiverPid, ServerSenderPid]} =
        receive
            {Server, {socket, S}} -> S
        end,
    Keys0 = get_keys(ClientReceiverPid, ClientSenderPid,
                     ServerReceiverPid, ServerSenderPid),
    {Sender, Receiver} = case Role of
                             client -> {Client, Server};
                             server -> {Server, Client}
                         end,
    %% Sending bytes over limit triggers key update
    ssl_test_lib:send(Sender, Data),
    Data = ssl_test_lib:check_active_receive(Receiver, Data),
    %% TODO check if key has been updated (needs debug logging of secrets)
    ct:sleep(500),
    Keys1 = get_keys(ClientReceiverPid, ClientSenderPid,
                     ServerReceiverPid, ServerSenderPid),
    verify_key_update(Keys0, Keys1),
    %% Test mechanism to prevent infinite loop of key updates
    BigData = binary:copy(<<"1234567890">>, 10),  %% 100 bytes
    ok = ssl_test_lib:send(Sender, BigData),
    ct:sleep(500),
    Keys2 = get_keys(ClientReceiverPid, ClientSenderPid,
                     ServerReceiverPid, ServerSenderPid),
    verify_key_update(Keys1, Keys2),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

get_keys(ClientReceiverPid, ClientSenderPid,
         ServerReceiverPid, ServerSenderPid) ->
    F = fun(Pid) ->
                {connection, D} = sys:get_state(Pid),
                M0 = element(3, D),
                Cr = maps:get(current_write, M0),
                {Pid, {maps:get(security_parameters, Cr),
                 maps:get(cipher_state, Cr)}}
        end,
    SendersKeys = [F(P) || P <- [ClientSenderPid, ServerSenderPid]],

    G = fun(Pid) ->
                {connection, D} = sys:get_state(Pid),
                #state{connection_states = Cs} = D,
                Cr = maps:get(current_read, Cs),
                {Pid, {maps:get(security_parameters,Cr),
                 maps:get(cipher_state, Cr)}}
        end,
    ReceiversKeys = [G(P) || P <- [ClientReceiverPid, ServerReceiverPid]],
    maps:from_list(SendersKeys ++ ReceiversKeys).

verify_key_update(Keys0, Keys1) ->
    V = fun(Pid, CurrentKeys) ->
                BaseKeys = maps:get(Pid, Keys0),
                ct:log("Pid = ~p~nBaseKeys = ~p~nCurrentKeys = ~p",
                      [Pid, BaseKeys, CurrentKeys], [esc_chars]),
                case BaseKeys == CurrentKeys of
                    true ->
                        ct:fail("Keys don't differ for ~w", [Pid]);
                    false ->
                        ok
                end
        end,
    maps:foreach(V, Keys1).

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
