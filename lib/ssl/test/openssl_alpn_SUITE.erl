%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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

-module(openssl_alpn_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(OPENSSL_QUIT, "Q\n").
-define(OPENSSL_RENEGOTIATE, "R\n").
-define(SLEEP, 1000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    %% Note: ALPN not supported in sslv3
    case ssl_test_lib:openssl_sane_dtls_alpn() of 
        true ->
            [
             {group, 'tlsv1.3'},
             {group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'},
             {group, 'dtlsv1.2'},
             {group, 'dtlsv1'}];
        false ->
            [{group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'}]
    end.

groups() ->
    case ssl_test_lib:openssl_sane_dtls_alpn() of 
        true ->             
            [
             {'tlsv1.3', [], alpn_tests()},
             {'tlsv1.2', [], alpn_tests() ++ alpn_npn_coexist() ++ rengotiation_tests()},
             {'tlsv1.1', [], alpn_tests() ++ alpn_npn_coexist() ++ rengotiation_tests()},
             {'tlsv1', [], alpn_tests() ++ alpn_npn_coexist() ++ rengotiation_tests()},
             {'dtlsv1.2', [], alpn_tests()},
             {'dtlsv1', [], alpn_tests()}
            ];
         false ->
            [
             {'tlsv1.3', [], alpn_tests()},
             {'tlsv1.2', [], alpn_tests() ++ alpn_npn_coexist() ++ rengotiation_tests()},
             {'tlsv1.1', [], alpn_tests() ++ alpn_npn_coexist() ++ rengotiation_tests()},
             {'tlsv1', [], alpn_tests() ++ alpn_npn_coexist() ++ rengotiation_tests()}
            ]
     end.
 
alpn_tests() ->
    [erlang_client_alpn_openssl_server_alpn,
     erlang_server_alpn_openssl_client_alpn,
     erlang_client_alpn_openssl_server,
     erlang_client_openssl_server_alpn,
     erlang_server_alpn_openssl_client,
     erlang_server_openssl_client_alpn].

alpn_npn_coexist() ->
    [
     erlang_client_alpn_npn_openssl_server_alpn_npn,
     erlang_server_alpn_npn_openssl_client_alpn_npn  
    ].
rengotiation_tests() ->
    [
     erlang_client_alpn_openssl_server_alpn_renegotiate,
     erlang_server_alpn_openssl_client_alpn_renegotiate
    ].

init_per_suite(Config0) ->
    case os:find_executable("openssl") of
        false ->
            {skip, "Openssl not found"};
        _ ->
            case check_openssl_alpn_support(Config0) of
                {skip, _} = Skip ->
                    Skip;
                _ ->
                    ct:pal("Version: ~p", [os:cmd("openssl version")]),
                    catch crypto:stop(),
                    try crypto:start() of
                        ok ->
                            ssl_test_lib:clean_start(),
                            ssl_test_lib:make_rsa_cert(Config0)
                    catch _:_  ->
                            {skip, "Crypto did not start"}
                    end
            end
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto),
    ssl_test_lib:kill_openssl().

init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
        true ->
            case ssl_test_lib:supports_ssl_tls_version(GroupName) of
                 true ->
                    case ssl_test_lib:check_sane_openssl_version(GroupName) of
                         true ->
                            ssl_test_lib:init_tls_version(GroupName, Config);
                         false ->
                            {skip, openssl_does_not_support_version}
                    end;
                false ->
                    {skip, openssl_does_not_support_version}
            end; 
         _ ->
            Config
    end.

end_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
        true ->
            ssl_test_lib:clean_tls_version(Config);
       false ->
            Config
    end.

init_per_testcase(TestCase, Config) -> 
    ct:timetrap({seconds, 30}),
    special_init(TestCase, Config).

special_init(erlang_client_alpn_openssl_server_alpn_renegotiate, Config) ->
    {ok, Version} = application:get_env(ssl, protocol_version),
    ssl_test_lib:check_sane_openssl_renegotaite(Config, Version);
special_init(erlang_server_alpn_openssl_client_alpn_renegotiate, Config) ->
    {ok, Version} = application:get_env(ssl, protocol_version),
    case ssl_test_lib:check_sane_openssl_renegotaite(Config, Version) of
        Config ->
            ssl_test_lib:openssl_allows_client_renegotaite(Config);
        Skip ->
            Skip
    end;
special_init(_, Config) ->
     Config.

end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

erlang_client_alpn_openssl_server_alpn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_alpn_negotiation(Config, Data, fun(Client, OpensslPort) ->
                                                                                      true = port_command(OpensslPort, Data),
                                                                                      ssl_test_lib:check_result(Client, Data)
                                                                              end).

%%--------------------------------------------------------------------

erlang_server_alpn_openssl_client_alpn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_alpn_negotiation(Config, Data, fun(Client, OpensslPort) ->
                                                                                      true = port_command(OpensslPort, Data),
                                                                                      ssl_test_lib:check_result(Client, Data)
                                                                              end).

%%--------------------------------------------------------------------------

erlang_client_alpn_openssl_server(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    ssl_test_lib:start_erlang_client_and_openssl_server_with_opts(Config,
                                                                  [{alpn_advertised_protocols, [<<"spdy/2">>]}],
                                                                  [],
                                                                  Data, fun(Client, OpensslPort) ->
                                                                                true = port_command(OpensslPort, Data),
                                                                                ssl_test_lib:check_result(Client, Data)
                                                                        end).

%%--------------------------------------------------------------------------

erlang_client_openssl_server_alpn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    ssl_test_lib:start_erlang_client_and_openssl_server_with_opts(Config,
                                                                  [],
                                                                  ["-alpn", "spdy/2"],
                                                                  Data, fun(Client, OpensslPort) ->
                                                                                true = port_command(OpensslPort, Data),
                                                                                ssl_test_lib:check_result(Client, Data)
                                                                        end).

%%--------------------------------------------------------------------------

erlang_server_alpn_openssl_client(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    ssl_test_lib:start_erlang_server_and_openssl_client_with_opts(Config,
						     [{alpn_preferred_protocols, [<<"spdy/2">>]}],
                                                                  [],
                                                                  Data, fun(Server, OpensslPort) ->
                                                                                true = port_command(OpensslPort, Data),
                                                                                ssl_test_lib:check_result(Server, Data)
                                                                        end).

%%--------------------------------------------------------------------------

erlang_server_openssl_client_alpn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    ssl_test_lib:start_erlang_server_and_openssl_client_with_opts(Config,
                                                                  [],
                                                                  ["-alpn", "spdy/2"],
                                                                  Data, fun(Server, OpensslPort) ->
                                                                                true = port_command(OpensslPort, Data),
                                                                                ssl_test_lib:check_result(Server, Data)
                                                                        end).

%%--------------------------------------------------------------------

erlang_client_alpn_openssl_server_alpn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_alpn_negotiation(Config, Data, fun(Client, OpensslPort) ->
                                                                                      true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
                                                                                      ct:sleep(?SLEEP),
                                                                                      true = port_command(OpensslPort, Data),
                                                                                      ssl_test_lib:check_result(Client, Data)
                                                                              end).

%%--------------------------------------------------------------------

erlang_server_alpn_openssl_client_alpn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_alpn_negotiation(Config, Data, fun(Server, OpensslPort) ->
                                                                                      true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
                                                                                      ct:sleep(?SLEEP),
                                                                                      true = port_command(OpensslPort, Data),
                                                                                      ssl_test_lib:check_result(Server, Data)
                                                                              end).

%%--------------------------------------------------------------------

erlang_client_alpn_npn_openssl_server_alpn_npn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_alpn_npn_negotiation(Config, Data, fun(Client, OpensslPort) ->
                                                                                          true = port_command(OpensslPort, Data),
                                                                                          ssl_test_lib:check_result(Client, Data)
                                                                                  end).

%%--------------------------------------------------------------------

erlang_server_alpn_npn_openssl_client_alpn_npn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_alpn_npn_negotiation(Config, Data, fun(Server, OpensslPort) ->
                                                                                          true = port_command(OpensslPort, Data),
                                                                                          ssl_test_lib:check_result(Server, Data)
                                                                                  end).


%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------
check_openssl_alpn_support(Config) ->
    HelpText = os:cmd("openssl s_client --help"),
    case string:str(HelpText, "alpn") of
        0 ->
            {skip, "Openssl not compiled with alpn support"};
        _ ->
            Config
    end.

start_erlang_client_and_openssl_server_for_alpn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts0 =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientOpts = [{alpn_advertised_protocols, [<<"spdy/2">>]} | ClientOpts0],

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_server", "-msg", "-alpn", "http/1.1,spdy/2", "-accept", 
            integer_to_list(Port), ssl_test_lib:version_flag(Version),
            "-CAfile", CaCertFile,
	    "-cert", CertFile, "-key", KeyFile],
    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),  
    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {ssl_test_lib,
                           erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options,  [{reuse_sessions, false} | ClientOpts]}]),

    Callback(Client, OpensslPort),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_server_and_openssl_client_for_alpn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 =  ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{alpn_preferred_protocols, [<<"spdy/2">>]} | ServerOpts0],

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {ssl_test_lib, erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_client", "-alpn", "http/1.0,spdy/2", "-msg", "-port", 
	    integer_to_list(Port), ssl_test_lib:version_flag(Version),
	    "-host", "localhost"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    Callback(Server, OpenSslPort),

    ssl_test_lib:close(Server),

    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

start_erlang_client_and_openssl_server_for_alpn_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts0 =  ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = [{alpn_advertised_protocols, [<<"spdy/2">>]},
        {client_preferred_next_protocols, {client, [<<"spdy/3">>, <<"http/1.1">>]}} | ClientOpts0],

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_server", "-msg", "-alpn", "http/1.1,spdy/2", "-nextprotoneg", 
	    "spdy/3", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
	    "-cert", CertFile, "-key",  KeyFile],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {ssl_test_lib,
                           erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options, [{reuse_sessions, false} | ClientOpts]}]),

    Callback(Client, OpensslPort),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_server_and_openssl_client_for_alpn_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ServerOpts = [{alpn_preferred_protocols, [<<"spdy/2">>]},
                  {next_protocols_advertised, [<<"spdy/3">>, <<"http/1.1">>]} | ServerOpts0],

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_client", "-alpn", "http/1.1,spdy/2", "-nextprotoneg", "spdy/3", 
	    "-msg", "-port", integer_to_list(Port), ssl_test_lib:version_flag(Version),
	    "-host", "localhost"],
    OpenSslPort =  ssl_test_lib:portable_open_port(Exe, Args),  

    Callback(Server, OpenSslPort),

    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

