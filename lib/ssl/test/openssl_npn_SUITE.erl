%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2022. All Rights Reserved.
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
-export([erlang_client_openssl_server_npn/0,
         erlang_client_openssl_server_npn/1,
         erlang_server_openssl_client_npn/0,
         erlang_server_openssl_client_npn/1,
         erlang_server_openssl_client_npn_only_client/1,
         erlang_server_openssl_client_npn_only_server/1,
         erlang_client_openssl_server_npn_only_client/1,
         erlang_client_openssl_server_npn_only_server/1,
         erlang_server_openssl_client_npn_renegotiate/1,
         erlang_client_openssl_server_npn_renegotiate/0,
         erlang_client_openssl_server_npn_renegotiate/1
        ]).

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
    Config1 = ssl_test_lib:init_per_suite(Config0, openssl),
    case ssl_test_lib:check_openssl_npn_support(Config1) of
        true ->
            ssl_test_lib:make_rsa_cert(Config1);
        false ->
            {skip, "npn_not_supported"}
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

special_init(erlang_client_openssl_server_npn_renegotiate, Config) ->
    {ok, Version} = application:get_env(ssl, protocol_version),
    ssl_test_lib:check_sane_openssl_renegotiate(Config, Version);
special_init(erlang_server_openssl_client_npn_renegotiate, Config) ->
    {ok, Version} = application:get_env(ssl, protocol_version),
    case ssl_test_lib:check_sane_openssl_renegotiate(Config, Version) of
        Config ->
            ssl_test_lib:openssl_allows_client_renegotiate(Config);
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
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    NpnProtocol = <<"spdy/2">>,
   
    {Server, OpenSSLPort} =
        ssl_test_lib:start_server(openssl, [{np,"http/1.1,spdy/2"},return_port],
                                  [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    
    {Client, CSocket} =
        ssl_test_lib:start_client(erlang, [{port, Port},
                                           return_socket],
                                  [{client_opts,
                                    [{client_preferred_next_protocols,
                                      {client, [NpnProtocol], <<"http/1.1">>}} | ClientOpts]}
                                  | Config]),

    case ssl:negotiated_protocol(CSocket) of
        {ok, NpnProtocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected,  NpnProtocol}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort),
    ssl:close(CSocket).

%%--------------------------------------------------------------------
erlang_client_openssl_server_npn_renegotiate() ->
    [{doc,"Test erlang client with openssl server doing npn negotiation and renegotiate"}].

erlang_client_openssl_server_npn_renegotiate(Config) when is_list(Config) ->
      
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    NpnProtocol = <<"spdy/2">>,
   
    Server = ssl_test_lib:start_server(openssl, [{np,"http/1.1,spdy/2"}],
                                       [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    
    {_, CSocket} =
        ssl_test_lib:start_client(erlang, [{port, Port},
                                           return_socket],
                                  [{client_opts,
                                    [{client_preferred_next_protocols,
                                      {client, [NpnProtocol], <<"http/1.1">>}} | ClientOpts]} | Config]),
    
    case ssl:negotiated_protocol(CSocket) of
        {ok, NpnProtocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected,  NpnProtocol}, {got, Result}}})
    end,
    ssl_test_lib:send(Server, ?OPENSSL_RENEGOTIATE),
    ct:sleep(1000),
    %%% Should still be the same as initially negotiated
    case ssl:negotiated_protocol(CSocket) of
        {ok, NpnProtocol} ->
            ok;
        Other ->
            ct:fail({error, {{expected,  NpnProtocol}, {got, Other}}})
    end.
    
%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn() ->
    [{doc,"Test erlang server with openssl client and npn negotiation"}].

erlang_server_openssl_client_npn(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_rsa_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    Protocol = <<"spdy/2">>,
    Server = ssl_test_lib:start_server(erlang, [{from, self()}],  
                                       [{server_opts, [{next_protocols_advertised, 
                                                        [<<"spdy/2">>]} |ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort} =
        ssl_test_lib:start_client(openssl, [{port, Port},
                                            {np, "spdy/2"},
                                            {options, ClientOpts},
                                            return_port], Config),
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
%% erlang_server_openssl_client_npn_renegotiate() ->
%%     [{doc,"Test erlang server with openssl client and npn negotiation with renegotiation"}].

erlang_server_openssl_client_npn_renegotiate(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    NpnProtocol = <<"spdy/2">>,
    Server =
        ssl_test_lib:start_server(erlang, [{from, self()}],
                                  [{server_opts, [{next_protocols_advertised,
                                                   [NpnProtocol]} | ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort} = 
        ssl_test_lib:start_client(openssl, [{port, Port}, {np, "spdy/2"}, 
                                            {options, ClientOpts}, return_port], Config),
    
    Server ! get_socket,
    SSocket = 
        receive 
            {Server, {socket, Socket}} ->
                Socket
        end,
    case ssl:negotiated_protocol(SSocket) of
        {ok, NpnProtocol} ->
            ok;
        Result ->
            ct:fail({error, {{expected,  NpnProtocol}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Server, OpenSSLPort),
    ssl:renegotiate(SSocket),
    case ssl:negotiated_protocol(SSocket) of
        {ok, NpnProtocol} ->
            ok;
        Other ->
            ct:fail({error, {{expected,  NpnProtocol}, {got, Other}}})
    end,
    ssl_test_lib:sanity_check(Server, OpenSSLPort),
    ssl:close(SSocket).
%%--------------------------------------------------------------------------
erlang_client_openssl_server_npn_only_client(Config) when is_list(Config) ->
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
   
    {Server, OpenSSLPort} =
        ssl_test_lib:start_server(openssl, [{np,"spdy/2"}, return_port],
                                  [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    
    {Client, CSocket} =
        ssl_test_lib:start_client(erlang, [{port, Port},
                                           return_socket],
                                  [{client_opts, ClientOpts} | Config]),

    case ssl:negotiated_protocol(CSocket) of
        {error, protocol_not_negotiated} ->
            ok;
        Result ->
            ct:fail({error, {{expected, undefined}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort),
    ssl:close(CSocket).

%%--------------------------------------------------------------------------
erlang_client_openssl_server_npn_only_server(Config) when is_list(Config) ->
   ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    ClientOpts =  ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
   
    {Server, OpenSSLPort} =
        ssl_test_lib:start_server(openssl, [{np,"spdy/2"}, return_port],
                                  [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    
    {Client, CSocket} =
        ssl_test_lib:start_client(erlang, [{port, Port},
                                           return_socket],
                                  [{client_opts, ClientOpts} | Config]),
    
    case ssl:negotiated_protocol(CSocket) of
        {error, protocol_not_negotiated} ->
            ok;
        Result ->
            ct:fail({error, {{expected, undefined}, {got, Result}}})
    end,
    ssl_test_lib:sanity_check(Client, OpenSSLPort),
    ssl:close(CSocket).
        
%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn_only_server(Config) when is_list(Config) ->
  ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    Server =
        ssl_test_lib:start_server(erlang, [{from, self()}],
                                  [{server_opts,
                                    [{next_protocols_advertised,
                                      [<<"spdy/2">>, <<"http/1.1">>]}
                                    | ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort} = ssl_test_lib:start_client(openssl, [{port, Port},
                                                                 {options, ClientOpts},
                                                                 return_port], Config),

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
erlang_server_openssl_client_npn_only_client(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    Server = ssl_test_lib:start_server(erlang, [{from, self()}],  
                                       [{server_opts, [ServerOpts]} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, OpenSSLPort}
        = ssl_test_lib:start_client(openssl, [{port, Port},
                                              {np, "spdy/2"},
                                              {options, ClientOpts},
                                              return_port], Config),
    
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
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------
