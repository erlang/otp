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

-define(SLEEP, 500).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    case ssl_test_lib:openssl_dtls_maxfraglen_support() of
        true ->
            [{group, 'tlsv1.3'},
             {group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'},
             {group, 'dtlsv1.2'},
             {group, 'dtlsv1'}];
        false ->
            [{group, 'tlsv1.3'},
             {group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'}]
    end.
        
groups() ->
    [{'tlsv1.3', [], common_tests()},
     {'tlsv1.2', [], common_tests() ++ pre_tls_1_3()},
     {'tlsv1.1', [], common_tests() ++ pre_tls_1_3()},
     {'tlsv1', [], common_tests() ++ pre_tls_1_3()},
     {'dtlsv1.2', [], common_tests() ++ pre_tls_1_3()},
     {'dtlsv1', [], common_tests() ++ pre_tls_1_3()}
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            case ssl_test_lib:openssl_maxfraglen_support() of
                true ->
                    ssl_test_lib:clean_start(),
                    ssl:clear_pem_cache(),
                    Config = ssl_test_lib:make_rsa_cert(Config0),
                    ssl_test_lib:cert_options(Config);
                false ->
                    {skip, "max_fragment_length not supported by OpenSSL"} 
            end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

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
    ClientOpts = proplists:get_value(client_rsa_opts, Config),

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),

    MFL = 512,
    Data = "reuse_session_erlang_server " ++ lists:duplicate(MFL, $r),

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
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    MFL = 512,
    Data = "reuse_session_erlang_client " ++ lists:duplicate(MFL, $r),
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

    %% quit s_server's current session so we can interact with the next client
    true = port_command(OpenSSLPort, "q\n"),
    ssl_test_lib:close(Client0),

    Client1 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()},  {options, [{reuse_session, SID} | ClientOpts]}]),
    receive
        {Client1, SID} ->
            ok
    after ?SLEEP ->
            ct:fail(session_not_reused)
    end,

    ErlRecvFun = fun() ->
                         Data = ssl_test_lib:check_active_receive(Client1, Data)
                 end,
    max_frag_len_test(Client1, OpenSSLPort, MFL, Data, ErlRecvFun),
    ssl_test_lib:close(Client1).


openssl_client(MFL, Config) ->  
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = proplists:get_value(client_rsa_opts, Config),
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),

    Data = "mfl_openssl_server " ++ lists:duplicate(MFL, $s),
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
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    Data = "mfl_openssl_server " ++ lists:duplicate(MFL, $s),

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
    ErlRecvFun = fun() ->
                         receive
                             {ErlProc, Data} ->
                                 ok
                         end
                 end,
    max_frag_len_test(ErlProc, OpenSSL, MFL, Data, ErlRecvFun).

max_frag_len_test(ErlProc, OpenSSL, MFL, Data, ErlRecvFun) ->
    true = port_command(OpenSSL, Data),
    ErlRecvFun(),

    ErlProc ! get_socket,
    ErlSocket = receive
                    {ErlProc, {socket, ErlSocket0}} ->
                        ErlSocket0
                end,
    ssl_test_lib:assert_mfl(ErlSocket, MFL).
