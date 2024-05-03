%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1,
         init_per_group/2,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_group/2,
         end_per_testcase/2
        ]).

%% Test cases
-export([erlang_client_alpn_openssl_server_alpn/1,
         erlang_server_alpn_openssl_client_alpn/1,
         erlang_client_alpn_openssl_server/1,
         erlang_client_openssl_server_alpn/1,
         erlang_server_alpn_openssl_client/1,
         erlang_server_openssl_client_alpn/1,
         erlang_client_alpn_npn_openssl_server_alpn_npn/1,
         erlang_server_alpn_npn_openssl_client_alpn_npn/1,
         erlang_client_alpn_openssl_server_alpn_renegotiate/1,
         erlang_server_alpn_openssl_client_alpn_renegotiate/1
        ]).


-define(OPENSSL_QUIT, "Q\n").
-define(OPENSSL_RENEGOTIATE, "R\n").
-define(SLEEP, 1000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    case ssl_test_lib:openssl_sane_dtls_alpn() of
        true ->
            [
             {group, 'tlsv1.3'},
             {group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'},
             {group, 'dtlsv1.2'},
             {group, 'dtlsv1'}
            ];
        false ->
            [
             {group, 'tlsv1.3'},
             {group, 'tlsv1.2'},
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
     erlang_server_openssl_client_alpn
    ].

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
    Config1 = ssl_test_lib:init_per_suite(Config0, openssl),
    case check_openssl_alpn_support(Config1) of
        false ->
            {skip, "No ALPN support"};
        true ->
            ssl_test_lib:make_rsa_cert(Config1)
    end.

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(TestCase, Config) ->
    ct:timetrap({seconds, 30}),
    special_init(TestCase, Config).

special_init(TestCase, Config) when TestCase == erlang_client_alpn_openssl_server_alpn_renegotiate;
                                    TestCase == erlang_server_alpn_openssl_client_alpn_renegotiate ->
    [Version | _] = ssl_test_lib:default_tls_version(Config),
    case ssl_test_lib:check_sane_openssl_renegotiate(Config, Version) of
        {skip, _} = Skip ->
            Skip;
        Config ->
            ssl_test_lib:openssl_allows_server_renegotiate(Config)
    end;
special_init(TestCase, Config) when TestCase == erlang_client_alpn_npn_openssl_server_alpn_npn;
                                    TestCase == erlang_server_alpn_npn_openssl_client_alpn_npn ->
    case ssl_test_lib:check_openssl_npn_support(Config) of
        false ->
            {skip, "npn not supported"};
        true ->
            Config
    end;
special_init(_, Config) ->
     Config.

end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

erlang_client_alpn_openssl_server_alpn(Config) when is_list(Config) ->
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    AlpnProtocol = <<"spdy/2">>,

    {Server, OpenSSLPort} =
        ssl_test_lib:start_server(openssl, [{alpn,"http/1.1,spdy/2"}, return_port],
                                  [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),

    {Client, CSocket} = ssl_test_lib:start_client(erlang, [{port, Port},
                                                           return_socket],
                                                  [{client_opts,
                                                    [{alpn_advertised_protocols,
                                                      [AlpnProtocol]} | ClientOpts]}
                                                  | Config]),

    case ssl:negotiated_protocol(CSocket) of
        {ok, AlpnProtocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected,  AlpnProtocol}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort),
    ssl:close(CSocket).

%%--------------------------------------------------------------------

erlang_server_alpn_openssl_client_alpn(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_rsa_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Protocol = <<"spdy/2">>,
    Server = ssl_test_lib:start_server(erlang, [{from, self()}],
                                       [{server_opts, [{alpn_preferred_protocols,
                                                        [<<"spdy/2">>]} |ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort} =
        ssl_test_lib:start_client(openssl, [{port, Port},{alpn, "spdy/2"},
                                            {options, ClientOpts}, return_port], Config),

    Server ! get_socket,
    SSocket =
        receive
            {Server, {socket, Socket}} ->
                Socket
        end,
    case ssl:negotiated_protocol(SSocket) of
        {ok, Protocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected, Protocol}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Server, OpenSSLPort),
    ssl:close(SSocket).

%%--------------------------------------------------------------------------

erlang_client_alpn_openssl_server(Config) when is_list(Config) ->
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    Protocol = <<"spdy/2">>,

    {Server, OpenSSLPort} = ssl_test_lib:start_server(openssl, [return_port],
                                                      [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),

    {Client, CSocket} =
        ssl_test_lib:start_client(erlang, [{port, Port},
                                           return_socket],
                                  [{client_opts, [{alpn_advertised_protocols,
                                                   [Protocol]} | ClientOpts]} | Config]),


    case ssl:negotiated_protocol(CSocket) of
        {error, protocol_not_negotiated} ->
            ok;
        Result ->
            ct:fail({error, {{expected, undefined}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort).

%%--------------------------------------------------------------------------

erlang_client_openssl_server_alpn(Config) when is_list(Config) ->
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {Server, OpenSSLPort} = ssl_test_lib:start_server(openssl, [{alpn,"spdy/2"}, return_port],
                                       [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),

    {Client, CSocket} = ssl_test_lib:start_client(erlang, [{port, Port},
                                                           return_socket],
                                                  [{client_opts, ClientOpts} | Config]),

    case ssl:negotiated_protocol(CSocket) of
        {error, protocol_not_negotiated} ->
            ok;
        Result ->
            ct:fail({error, {{expected, undefined}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort).

%%--------------------------------------------------------------------------
erlang_server_alpn_openssl_client(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    Server = ssl_test_lib:start_server(erlang, [{from, self()}],
                                       [{server_opts, [{alpn_preferred_protocols,
                                                        [<<"spdy/2">>]} | ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort} =
        ssl_test_lib:start_client(openssl, [{port, Port},
                                            {options, ClientOpts}, return_port], Config),

    Server ! get_socket,
    SSocket =
        receive
            {Server, {socket, Socket}} ->
                Socket
        end,
    case ssl:negotiated_protocol(SSocket) of
        {error, protocol_not_negotiated} ->
            ok;
        Result ->
            ct:fail({error, {{expected, undefined}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Server, OpenSSLPort),
    ssl:close(SSocket).
%%--------------------------------------------------------------------------

erlang_server_openssl_client_alpn(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    Server = ssl_test_lib:start_server(erlang, [{from, self()}],
                                       [{server_opts, [ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort} =
        ssl_test_lib:start_client(openssl, [{port, Port}, {alpn, "spdy/2"},
                                            {options, ClientOpts}, return_port], Config),

    Server ! get_socket,
    SSocket =
        receive
            {Server, {socket, Socket}} ->
                Socket
        end,
    case ssl:negotiated_protocol(SSocket) of
        {error, protocol_not_negotiated} ->
            ok;
        Result ->
            ct:fail({error, {{expected, undefined}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Server, OpenSSLPort),
    ssl:close(SSocket).

%%--------------------------------------------------------------------

erlang_client_alpn_openssl_server_alpn_renegotiate(Config) when is_list(Config) ->

    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    AlpnProtocol = <<"spdy/2">>,

    {Server, OpenSSLPort} =
        ssl_test_lib:start_server(openssl, [{alpn,"http/1.1,spdy/2"}, return_port],
                                  [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),

    {Client, CSocket} =
        ssl_test_lib:start_client(erlang, [{port, Port},
                                           return_socket],
                                  [{client_opts,
                                    [{alpn_advertised_protocols,
                                      [AlpnProtocol]} | ClientOpts]} | Config]),

    case ssl:negotiated_protocol(CSocket) of
        {ok, AlpnProtocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected,  AlpnProtocol}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort),
    ssl_test_lib:send(Server, ?OPENSSL_RENEGOTIATE),
    ct:sleep(1000),
    %%% Should still be the same as initially negotiated
    case ssl:negotiated_protocol(CSocket) of
        {ok, AlpnProtocol} ->
            ok;
        Other ->
            ct:fail({error, {{expected,  AlpnProtocol}, {got, Other}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort),
    ssl:close(CSocket).
%%--------------------------------------------------------------------

erlang_server_alpn_openssl_client_alpn_renegotiate(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    AlpnProtocol = <<"spdy/2">>,
    Server = ssl_test_lib:start_server(erlang, [{from, self()}],
                                       [{server_opts, [{alpn_preferred_protocols,
                                                        [AlpnProtocol]} | ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort} =
        ssl_test_lib:start_client(openssl, [{port, Port}, {alpn, "spdy/2"},
                                            {options, ClientOpts}, return_port], Config),

    Server ! get_socket,
    SSocket =
        receive
            {Server, {socket, Socket}} ->
                Socket
        end,
    case ssl:negotiated_protocol(SSocket) of
        {ok, AlpnProtocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected,  AlpnProtocol}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Server, OpenSSLPort),
    ssl:renegotiate(SSocket),
    case ssl:negotiated_protocol(SSocket) of
        {ok, AlpnProtocol} ->
            ok;
        Other ->
            ct:fail({error, {{expected,  AlpnProtocol}, {got, Other}}})
    end,
    ssl_test_lib:sanity_check(Server, OpenSSLPort),
    ssl:close(SSocket).

%%--------------------------------------------------------------------

erlang_client_alpn_npn_openssl_server_alpn_npn(Config) when is_list(Config) ->
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    AlpnProtocol = <<"spdy/2">>,

    {Server, OpenSSLPort} =
        ssl_test_lib:start_server(openssl, [{alpn,"http/1.1,spdy/2"},
                                            {np,  "spdy/3"}, return_port],
                                  [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),

    {Client, CSocket} =
        ssl_test_lib:start_client(erlang, [{port, Port},
                                           return_socket],
                                  [{client_opts,
                                    [{alpn_advertised_protocols, [AlpnProtocol]},
                                     {client_preferred_next_protocols,
                                      {client, [<<"spdy/3">>, <<"http/1.1">>]}} | ClientOpts]}]  ++ Config),
    case ssl:negotiated_protocol(CSocket) of
        {ok, AlpnProtocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected,  AlpnProtocol}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort).

%%--------------------------------------------------------------------

erlang_server_alpn_npn_openssl_client_alpn_npn(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    AlpnProtocol = <<"spdy/2">>,
    Server = ssl_test_lib:start_server(erlang,
                                       [{from, self()}],
                                       [{server_opts, [{alpn_preferred_protocols,
                                                        [<<"spdy/2">>]},
                                                       {next_protocols_advertised,
                                                        [<<"spdy/3">>, <<"http/1.1">>]}
                                                      | ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort} =
        ssl_test_lib:start_client(openssl, [{port, Port}, {alpn, "http/1.1,spdy/2"},
                                            {np,"spdy/3"}, {options, ClientOpts},
                                            return_port], Config),

    Server ! get_socket,
    SSocket =
        receive
            {Server, {socket, Socket}} ->
                Socket
        end,
    case ssl:negotiated_protocol(SSocket) of
        {ok, AlpnProtocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected, AlpnProtocol}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Server, OpenSSLPort),
    ssl:close(SSocket).

%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------
check_openssl_alpn_support(Config) ->
    case proplists:get_value(openssl_version, Config) of
        "OpenSSL 1.0."  ++ _ = Str->
            SubStr = Str -- "OpenSSL 1.0.",
            atleast(SubStr, 2);
        "OpenSSL 1.1" ++ _ ->
            true;
        "OpenSSL 3" ++ _ ->
            true;
        "LibreSSL 2.0" ++ _ ->
            false;
        "LibreSSL 2.1." ++ _ = Str ->
            SubStr = Str -- "LibreSSL 2.1.",
            atleast(SubStr, 3);
        _ ->
            false
    end.

atleast([StrNum|_], Num) ->
    list_to_integer([StrNum]) >= Num.
