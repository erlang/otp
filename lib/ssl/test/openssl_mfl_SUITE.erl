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

%%
-module(openssl_mfl_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1, 
         init_per_group/2, 
         init_per_testcase/2,
         end_per_suite/1,
         end_per_group/2,
         end_per_testcase/2]).

%% Testcases
-export([openssl_client/1, 
         openssl_server/1, 
         reuse_session_erlang_server/1, 
         reuse_session_erlang_client/1]).


-export([session_id_and_data/3]).

-define(SLEEP, 500).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [{group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}].
        
groups() ->
    [{'tlsv1.3', [], common_tests()},
     {'tlsv1.2', [], common_tests() ++ pre_tls_1_3()},
     {'tlsv1.1', [], common_tests() ++ pre_tls_1_3()},
     {'tlsv1', [], common_tests() ++ pre_tls_1_3()},
     {'dtlsv1.2', [], common_tests() ++ pre_tls_1_3()},
     {'dtlsv1', [], common_tests() ++ pre_tls_1_3()}
    ].

init_per_suite(Config0) ->
    Config1 = ssl_test_lib:init_per_suite(Config0, openssl),
    case ssl_test_lib:openssl_maxfraglen_support() of
        true ->
            Config = ssl_test_lib:make_rsa_cert(Config1),
            ssl_test_lib:cert_options(Config);
        false ->
            {skip, "max_fragment_length not supported by OpenSSL"} 
    end.

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

common_tests() ->
    [openssl_client, openssl_server].

pre_tls_1_3() ->
    [reuse_session_erlang_server, reuse_session_erlang_client].

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
openssl_server(Config) when is_list(Config) ->
    openssl_server(512, Config),
    openssl_server(2048, Config).

%--------------------------------------------------------------------------------
%% check max_fragment_length interworking with openssl client
openssl_client(Config) when is_list(Config) ->
    openssl_client(1024, Config),
    openssl_client(4096, Config).

%--------------------------------------------------------------------------------
reuse_session_erlang_server(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    Protocol = proplists:get_value(protocol, ServerOpts, tls),
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    MFL = 512,
    Data = max_frag_data(Protocol, MFL),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, active_recv, [length(Data)]}},
                                        {reconnect_times, 5},
                                        {options,  ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {_Client, OpenSSLPort} = ssl_test_lib:start_client(openssl, [{port, Port}, 
                                                                 {reconnect, true},
                                                                 {maxfrag, MFL},
                                                                 {options, ClientOpts}, 
                                                                 return_port], Config),
    max_frag_len_test(Server, OpenSSLPort, MFL, Data),
    ssl_test_lib:close(Server).

%%--------------------------------------------------------------------

reuse_session_erlang_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    Protocol = proplists:get_value(protocol, ClientOpts0, tls),
    MFL = 512,
    Data = max_frag_data(Protocol, MFL),
    ClientOpts = [{max_fragment_length, MFL} | ClientOpts0],

    {Server, OpenSSLPort} = ssl_test_lib:start_server(openssl, [{maxfrag, MFL}, return_port], 
                                                      [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),


    Client0 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()},
                                   {options, [{reuse_sessions, save}, {verify, verify_peer}| ClientOpts]}]),

    SID = receive
              {Client0, Id0} ->
                  Id0
          end,

    ssl_test_lib:close(Client0),

    Client1 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {?MODULE, session_id_and_data, [self(), length(Data)]}},
                                   {from, self()},  {options, [{reuse_session, SID} | ClientOpts]}]),
    receive
        {Client1, SID} ->
            ok
    after ?SLEEP ->
            ct:fail(session_not_reused)
    end,

    max_frag_len_test(Client1, OpenSSLPort, MFL, Data),
    ssl_test_lib:close(Client1).


openssl_client(MFL, Config) ->  
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    Protocol = proplists:get_value(protocol, ServerOpts, tls),
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),

    Data = max_frag_data(Protocol, MFL),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, active_recv, [length(Data)]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {_Client, OpenSSLPort} = ssl_test_lib:start_client(openssl, [{port, Port}, 
                                                                 {maxfrag, MFL},
                                                                 {options, ClientOpts}, 
                                                                 return_port], Config),

    max_frag_len_test(Server, OpenSSLPort, MFL, Data).

%% -------------------------------------------------------------------------------
%% Internal functions 
%%--------------------------------------------------------------------------------

openssl_server(MFL, Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    Protocol = proplists:get_value(protocol, ClientOpts, tls),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = max_frag_data(Protocol, MFL),

    {Server, OpenSSLPort} = ssl_test_lib:start_server(openssl, [{maxfrag, MFL},
                                                                return_port], 
                                                      [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib,
                                               active_recv, [length(Data)]}},
                                        {options, [{max_fragment_length, MFL} | ClientOpts]}]),

    max_frag_len_test(Client, OpenSSLPort, MFL, Data).

%% ------------------------------------------------------------
max_frag_len_test(ErlProc, OpenSSL, MFL, Data) ->
    openssl_send(OpenSSL, Data),
    receive
        {ErlProc, Data} ->
            ok
    end,
    ErlProc ! get_socket,
    ErlSocket = receive
                    {ErlProc, {socket, ErlSocket0}} ->
                        ErlSocket0
                end,
    ssl_test_lib:assert_mfl(ErlSocket, MFL).

session_id_and_data(Socket, Pid, Len) ->
    {ok, [{session_id, ID}]} = ssl:connection_information(Socket, [session_id]),
    Pid ! {self(), ID},
    ssl_test_lib:active_recv(Socket, Len).

max_frag_data(tls, MaxFragLen) ->
    "Send more data than max frag length " ++ lists:duplicate(MaxFragLen, $s);
max_frag_data(dtls, _MaxFragLen) ->
    "Send small data as OpenSSL s_client/s_server does not create UDP packets "
        "with appropriate fragments".

openssl_send(OpenSSL, Data) ->
    port_command(OpenSSL, Data).
