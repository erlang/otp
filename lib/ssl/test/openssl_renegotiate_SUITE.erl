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

-module(openssl_renegotiate_SUITE).

-include_lib("common_test/include/ct.hrl").

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
-export([erlang_client_openssl_server_renegotiate/0,
         erlang_client_openssl_server_renegotiate/1,
         erlang_client_openssl_server_renegotiate_after_client_data/0,
         erlang_client_openssl_server_renegotiate_after_client_data/1,
         erlang_client_openssl_server_nowrap_seqnum/0,
         erlang_client_openssl_server_nowrap_seqnum/1,
         erlang_server_openssl_client_nowrap_seqnum/0,
         erlang_server_openssl_client_nowrap_seqnum/1
        ]).

%% Apply export
-export([delayed_send/2,
         send_wait_send/2
         ]).

-define(SLEEP, 1000).
-define(OPENSSL_RENEGOTIATE, "R\n").
-define(DEFAULT_TIMEOUT, {seconds, 30}).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    case ssl_test_lib:openssl_sane_dtls() of 
        true ->
            [{group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'},
             {group, 'dtlsv1.2'},
             {group, 'dtlsv1'}];
        false ->
            [{group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'}
             ]
    end.

groups() ->
     case ssl_test_lib:openssl_sane_dtls() of 
         true ->
             [{'tlsv1.2', [], all_versions_tests()},
              {'tlsv1.1', [], all_versions_tests()},
              {'tlsv1', [], all_versions_tests()},
              {'dtlsv1.2', [], all_versions_tests()},
              {'dtlsv1', [], all_versions_tests()}
             ];
        false ->
             [{'tlsv1.2', [], all_versions_tests()},
              {'tlsv1.1', [], all_versions_tests()},
              {'tlsv1', [], all_versions_tests()}
           ]
     end.
 
all_versions_tests() ->
    [    
     erlang_client_openssl_server_renegotiate,
     erlang_client_openssl_server_renegotiate_after_client_data,
     erlang_client_openssl_server_nowrap_seqnum,
     erlang_server_openssl_client_nowrap_seqnum
    ].


init_per_suite(Config0) ->
    Config = ssl_test_lib:init_per_suite(Config0, openssl),
    ssl_test_lib:make_rsa_cert(Config).

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    case ssl_test_lib:check_sane_openssl_version(GroupName, Config) of
        true ->
            case ssl_test_lib:check_sane_openssl_renegotiate(Config, GroupName) of
                {skip,_} = Skip ->
                    Skip;
                _ ->
                    ssl_test_lib:init_per_group_openssl(GroupName, Config)
            end;
        false  ->
            {skip, {atom_to_list(GroupName) ++ " not supported by OpenSSL"}}
    end.
end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(erlang_client_openssl_server_nowrap_seqnum, Config) ->
    ct:timetrap(?DEFAULT_TIMEOUT),
    ssl_test_lib:openssl_allows_client_renegotiate(Config);
init_per_testcase(_TestCase, Config) ->
    ct:timetrap(?DEFAULT_TIMEOUT),
    Config.

end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

erlang_client_openssl_server_renegotiate() ->
    [{doc,"Test erlang client when openssl server issuses a renegotiate"}].
erlang_client_openssl_server_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    ErlData = "From erlang to openssl",
    OpenSslData = "From openssl to erlang",

    {Server, OpenSSLPort} = ssl_test_lib:start_server(openssl, [return_port], 
                                                      [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               delayed_send, [[ErlData, OpenSslData]]}},
                                         {options, [{reuse_sessions, false} | ClientOpts]}]),

    true = port_command(OpenSSLPort, ?OPENSSL_RENEGOTIATE),
    ct:sleep(?SLEEP),
    true = port_command(OpenSSLPort, OpenSslData),
    
    ssl_test_lib:check_result(Client, OpenSslData), 
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
erlang_client_openssl_server_renegotiate_after_client_data() ->
    [{doc,"Test erlang client when openssl server issuses a renegotiate after reading client data"}].
erlang_client_openssl_server_renegotiate_after_client_data(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    ErlData = "From erlang to openssl",
    OpenSslData = "From openssl to erlang",

    {Server, OpenSSLPort} = ssl_test_lib:start_server(openssl, [return_port], 
                                                      [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               send_wait_send, [[ErlData, OpenSslData]]}},
                                        {options, [{reuse_sessions, false} |ClientOpts]}]),

    true = port_command(OpenSSLPort, ?OPENSSL_RENEGOTIATE),
    ct:sleep(?SLEEP),
    true = port_command(OpenSSLPort, OpenSslData),

    ssl_test_lib:check_result(Client, OpenSslData),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
erlang_client_openssl_server_nowrap_seqnum() ->
    [{doc, "Test that erlang client will renegotiate session when",
      "max sequence number celing is about to be reached. Although"
      "in the testcase we use the test option renegotiate_at"
      " to lower treashold substantially."}].
erlang_client_openssl_server_nowrap_seqnum(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    ErlData = "From erlang to openssl\n",
    N = 10,

    Server = ssl_test_lib:start_server(openssl, [], 
                                       [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib,
                                               trigger_renegotiate, [[ErlData, N+2]]}},
                                        {options, [{reuse_sessions, false},
                                                   {renegotiate_at, N} | ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok), 
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
erlang_server_openssl_client_nowrap_seqnum() ->
    [{doc, "Test that erlang server will renegotiate session when",
      "max sequence number celing is about to be reached. Although"
      "in the testcase we use the test option renegotiate_at"
      " to lower treashold substantially."}].
erlang_server_openssl_client_nowrap_seqnum(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {_, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    N = 10,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {from, self()},
                                        {mfa, {ssl_test_lib,
                                               trigger_renegotiate, [[Data, N+2]]}},
                                        {options, [{renegotiate_at, N}, {reuse_sessions, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    {_Client, OpenSSLPort} = ssl_test_lib:start_client(openssl, [{port, Port}, 
                                                                 {options, ClientOpts}, 
                                                                 return_port], Config),
    true = port_command(OpenSSLPort, Data),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------
delayed_send(Socket, [ErlData, OpenSslData]) ->
    ct:sleep(?SLEEP),
    ssl:send(Socket, ErlData),
    ssl_test_lib:active_recv(Socket, length(OpenSslData)).


send_wait_send(Socket, [ErlData, OpenSslData]) ->
    ssl:send(Socket, ErlData),
    ct:sleep(?SLEEP),
    ssl:send(Socket, ErlData),
    ssl_test_lib:active_recv(Socket, length(OpenSslData)).
    
