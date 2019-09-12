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

-module(openssl_npn_SUITE).

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
    %% NPN is not supported in TLS-1.3 (replaced by ALPN and deprecated in TLS 1.2)
    %% OpenSSL DTLS support for NPN is either not there or broken.
    [{group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'}].

groups() ->
    [{'tlsv1.2', [], npn_tests() ++ npn_renegotiate_tests()},
     {'tlsv1.1', [], npn_tests() ++ npn_renegotiate_tests()},
     {'tlsv1', [], npn_tests() ++ npn_renegotiate_tests()}
    ].
 
npn_tests() ->
    [erlang_client_openssl_server_npn,
     erlang_server_openssl_client_npn,
     erlang_server_openssl_client_npn_only_client,
     erlang_server_openssl_client_npn_only_server,
     erlang_client_openssl_server_npn_only_client,
     erlang_client_openssl_server_npn_only_server].

npn_renegotiate_tests() ->
           [
            erlang_server_openssl_client_npn_renegotiate,
            erlang_client_openssl_server_npn_renegotiate
           ].

init_per_suite(Config0) ->
    case os:find_executable("openssl") of
        false ->
            {skip, "Openssl not found"};
        _ ->
            case check_openssl_npn_support(Config0) of
                {skip, _} = Skip ->
                    Skip;
                _ ->
                    ct:pal("Version: ~p", [os:cmd("openssl version")]),
                    catch crypto:stop(),
                    try crypto:start() of
                        ok ->
                            ssl_test_lib:clean_start(),
                            ssl:clear_pem_cache(),
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
    ct:timetrap({seconds, 10}),
    special_init(TestCase, Config).

special_init(erlang_client_openssl_server_npn_renegotiate, Config) ->
    {ok, Version} = application:get_env(ssl, protocol_version),
    ssl_test_lib:check_sane_openssl_renegotaite(Config, Version);
special_init(erlang_server_openssl_client_npn_renegotiate, Config) ->
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

erlang_client_openssl_server_npn() ->
    [{doc,"Test erlang client with openssl server doing npn negotiation"}].

erlang_client_openssl_server_npn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, 
                                                               fun(Client, OpensslPort) ->
                                                                       true = port_command(OpensslPort, Data),
                                                                       ssl_test_lib:check_result(Client, Data)
                                                               end).

%%--------------------------------------------------------------------
erlang_client_openssl_server_npn_renegotiate() ->
    [{doc,"Test erlang client with openssl server doing npn negotiation and renegotiate"}].

erlang_client_openssl_server_npn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, 
                                                               fun(Client, OpensslPort) ->
                                                                       true = port_command(OpensslPort, 
                                                                                           ?OPENSSL_RENEGOTIATE),
                                                                       ct:sleep(?SLEEP),
                                                                       true = port_command(OpensslPort, Data),
                                                                       ssl_test_lib:check_result(Client, Data)
                                                               end).
%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn() ->
    [{doc,"Test erlang server with openssl client and npn negotiation"}].

erlang_server_openssl_client_npn(Config) when is_list(Config) ->

    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, 
                                                               fun(Server, OpensslPort) ->
                                                                       true = port_command(OpensslPort, Data),
                                                                       ssl_test_lib:check_result(Server, Data)
                                                               end).

%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn_renegotiate() ->
    [{doc,"Test erlang server with openssl client and npn negotiation with renegotiation"}].

erlang_server_openssl_client_npn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, 
                                                               fun(Server, OpensslPort) ->
                                                                       true = port_command(OpensslPort, 
                                                                                           ?OPENSSL_RENEGOTIATE),
                                                                       ct:sleep(?SLEEP),
                                                                       true = port_command(OpensslPort, Data),
                                                                       ssl_test_lib:check_result(Server, Data)
                                                               end).
%%--------------------------------------------------------------------------
erlang_client_openssl_server_npn_only_server(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    ssl_test_lib:start_erlang_client_and_openssl_server_with_opts(Config, [],
                                                                  ["-nextprotoneg", "spdy/2"], Data, 
                                                                  fun(Server, OpensslPort) ->
                                                                          true = port_command(OpensslPort, Data),
                                                                          ssl_test_lib:check_result(Server, Data)
                                                                  end).

%%--------------------------------------------------------------------------

erlang_client_openssl_server_npn_only_client(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    ssl_test_lib:start_erlang_client_and_openssl_server_with_opts(Config,
                                                                  [{client_preferred_next_protocols,
                                                                    {client, [<<"spdy/2">>], <<"http/1.1">>}}], [],
                                                                  Data, 
                                                                  fun(Server, OpensslPort) ->
                                                                          true = port_command(OpensslPort, Data),
                                                                          ssl_test_lib:check_result(Server, Data)
                                                                  end).

%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn_only_server(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    ssl_test_lib:start_erlang_server_and_openssl_client_with_opts(Config, 
                                                                  [{next_protocols_advertised, [<<"spdy/2">>]}], [],
                                                                  Data, 
                                                                  fun(Server, OpensslPort) ->
                                                                          true = port_command(OpensslPort, Data),
                                                                          ssl_test_lib:check_result(Server, Data)
                                                                  end).

erlang_server_openssl_client_npn_only_client(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    ssl_test_lib:start_erlang_server_and_openssl_client_with_opts(Config, [], ["-nextprotoneg", "spdy/2"],
                                                                  Data, 
                                                                  fun(Server, OpensslPort) ->
                                                                          true = port_command(OpensslPort, Data),
                                                                          ssl_test_lib:check_result(Server, Data)
                                                                  end).

%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------

start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientOpts = [{client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"http/1.1">>}} | ClientOpts0],

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    
    Exe = "openssl",
    Args = ["s_server", "-msg", "-nextprotoneg", "http/1.1,spdy/2", "-accept", integer_to_list(Port),
	    ssl_test_lib:version_flag(Version),
            "-CAfile", CaCertFile,
	    "-cert", CertFile, "-key", KeyFile],
    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {ssl_test_lib,
                           erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options, ClientOpts}]),

    Callback(Client, OpensslPort),

    %% Clean close down! Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{next_protocols_advertised, [<<"spdy/2">>]} | ServerOpts0],

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {ssl_test_lib, erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_client", "-nextprotoneg", "http/1.0,spdy/2", "-msg", "-connect", 
            ssl_test_lib:hostname_format(Hostname) ++ ":" 
	    ++ integer_to_list(Port), ssl_test_lib:version_flag(Version)],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    Callback(Server, OpenSslPort),

    ssl_test_lib:close(Server),

    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

check_openssl_npn_support(Config) ->
    HelpText = os:cmd("openssl s_client --help"),
    case string:str(HelpText, "nextprotoneg") of
        0 ->
            {skip, "Openssl not compiled with nextprotoneg support"};
        _ ->
            Config
    end.
