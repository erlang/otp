%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, 'tlsv1.3'}].

groups() ->
    [{'tlsv1.3', [], tls_1_3_tests()}].

tls_1_3_tests() ->
    [ssl_client_ssl_server_key_update_at,
     ssl_client_ssl_server_explicit_key_update,
     openssl_client_ssl_server_explicit_key_update,
     ssl_client_openssl_server_explicit_key_update].

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
    case ssl_test_lib:is_tls_version(GroupName) of
        true ->
            case ssl_test_lib:sufficient_crypto_support(GroupName) of
                true ->
                    [{client_type, erlang},
                     {server_type, erlang}, {version, GroupName}
                     | ssl_test_lib:init_tls_version(GroupName, Config)];
                false ->
                    {skip, "Missing crypto support"}
            end;
        _ ->
            ssl:start(),
            Config
    end.

end_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
        true ->
            ssl_test_lib:clean_tls_version(Config);
        false ->
            Config
    end.

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.


%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

ssl_client_ssl_server_key_update_at() ->
    [{doc,"Test option 'key_update_at' between erlang client and erlang server."}].

ssl_client_ssl_server_key_update_at(Config) ->
    %% {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Data = "123456789012345",  %% 15 bytes

    Server = ssl_test_lib:start_server(erlang, [{log_level, debug},
                                                {key_update_at, 15}], Config),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client(erlang, [{port, Port},
                                                {log_level, debug},
                                                {key_update_at, 15}], Config),
    %% Sending bytes over limit triggers key update
    ssl_test_lib:send(Client, Data),
    Data = ssl_test_lib:check_active_receive(Server, Data),
    %% TODO check if key has been updated (needs debug logging of secrets)

    %% Test mechanism to prevent infinite loop of key updates
    BigData = binary:copy(<<"1234567890">>, 10),  %% 100 bytes
    ok = ssl_test_lib:send(Client, BigData),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

ssl_client_ssl_server_explicit_key_update() ->
    [{doc,"Test ssl:update_key/2 between erlang client and erlang server."}].

ssl_client_ssl_server_explicit_key_update(Config) ->
    Data = "123456789012345",  %% 15 bytes

    Server = ssl_test_lib:start_server(erlang, [{log_level, debug}], Config),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client(erlang, [{port, Port}, {log_level, debug}], Config),
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

openssl_client_ssl_server_explicit_key_update() ->
    [{doc,"Test ssl:update_key/2 between openssl s_client and erlang server."}].

openssl_client_ssl_server_explicit_key_update(Config) ->
    Data = "123456789012345",  %% 15 bytes

    Server = ssl_test_lib:start_server(erlang, [{log_level, debug}], Config),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client(openssl, [{port, Port}], Config),
    ssl_test_lib:send_recv_result_active(Client, Server, Data),

    %% TODO s_client can hang after sending special commands e.g "k", "K"
    %% ssl_test_lib:update_keys(Client, write),
    %% ssl_test_lib:update_keys(Client, read_write),
    ssl_test_lib:update_keys(Server, write),
    ssl_test_lib:update_keys(Server, read_write),

    ssl_test_lib:send_recv_result_active(Client, Server, Data),

    ssl_test_lib:close(Client),
    ssl_test_lib:close(Server).

ssl_client_openssl_server_explicit_key_update() ->
    [{doc,"Test ssl:update_key/2 between ssl client and s_server."}].

ssl_client_openssl_server_explicit_key_update(Config) ->
    Data = "123456789012345",  %% 15 bytes

    Server = ssl_test_lib:start_server(openssl, [], Config),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client(erlang, [{port, Port},
                                                {log_level, debug},
                                                {versions, ['tlsv1.2','tlsv1.3']}],Config),
    ssl_test_lib:send_recv_result_active(Server, Client, Data),

    ssl_test_lib:update_keys(Client, write),
    ssl_test_lib:update_keys(Client, read_write),
    ssl_test_lib:update_keys(Server, write),
    ssl_test_lib:update_keys(Server, read_write),

    ssl_test_lib:send_recv_result_active(Client, Server, Data),

    ssl_test_lib:close(Client),
    ssl_test_lib:close(Server).
