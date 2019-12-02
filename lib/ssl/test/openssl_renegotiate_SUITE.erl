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

-module(openssl_renegotiate_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

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
             {group, 'sslv3'},
             {group, 'dtlsv1.2'},
             {group, 'dtlsv1'}];
        false ->
            [{group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'},
             {group, 'sslv3'}]
    end.

groups() ->
     case ssl_test_lib:openssl_sane_dtls() of 
         true ->
             [{'tlsv1.2', [], all_versions_tests()},
              {'tlsv1.1', [], all_versions_tests()},
              {'tlsv1', [], all_versions_tests()},
              {'sslv3', [], all_versions_tests()},
              {'dtlsv1.2', [], all_versions_tests()},
              {'dtlsv1', [], all_versions_tests()}
             ];
        false ->
             [{'tlsv1.2', [], all_versions_tests()},
              {'tlsv1.1', [], all_versions_tests()},
              {'tlsv1', [], all_versions_tests()},
              {'sslv3', [], all_versions_tests()}
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
    case os:find_executable("openssl") of
        false ->
            {skip, "Openssl not found"};
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
                            ssl_test_lib:check_sane_openssl_renegotaite(ssl_test_lib:init_tls_version(GroupName, 
                                                                                                      Config),
                                                                        GroupName);
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
init_per_testcase(erlang_client_openssl_server_nowrap_seqnum, Config) ->
    ct:timetrap(?DEFAULT_TIMEOUT),
    ssl_test_lib:openssl_allows_client_renegotaite(Config);
init_per_testcase(TestCase, Config) ->
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
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    ErlData = "From erlang to openssl",
    OpenSslData = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),  
            ssl_test_lib:version_flag(Version),
            "-CAfile", CaCertFile,
            "-cert", CertFile, "-key", KeyFile, "-msg"],
    
    OpensslPort =  ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               delayed_send, [[ErlData, OpenSslData]]}},
                                         {options, [{reuse_sessions, false} | ClientOpts]}]),

    true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
    ct:sleep(?SLEEP),
    true = port_command(OpensslPort, OpenSslData),
    
    ssl_test_lib:check_result(Client, OpenSslData), 

     %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client),
     process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
erlang_client_openssl_server_renegotiate_after_client_data() ->
    [{doc,"Test erlang client when openssl server issuses a renegotiate after reading client data"}].
erlang_client_openssl_server_renegotiate_after_client_data(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    ErlData = "From erlang to openssl",
    OpenSslData = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-CAfile", CaCertFile,
            "-cert", CertFile, "-key", KeyFile, "-msg"],

    OpensslPort =  ssl_test_lib:portable_open_port(Exe, Args),

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               send_wait_send, [[ErlData, OpenSslData]]}},
                                        {options, [{reuse_sessions, false} |ClientOpts]}]),

    true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
    ct:sleep(?SLEEP),
    true = port_command(OpensslPort, OpenSslData),

    ssl_test_lib:check_result(Client, OpenSslData),
    
    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------
erlang_client_openssl_server_nowrap_seqnum() ->
    [{doc, "Test that erlang client will renegotiate session when",
      "max sequence number celing is about to be reached. Although"
      "in the testcase we use the test option renegotiate_at"
      " to lower treashold substantially."}].
erlang_client_openssl_server_nowrap_seqnum(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    ErlData = "From erlang to openssl\n",
    N = 10,

    Port = ssl_test_lib:inet_port(node()),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-CAfile", CaCertFile,
            "-cert", CertFile, "-key", KeyFile, "-msg"],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib,
                                               trigger_renegotiate, [[ErlData, N+2]]}},
                                        {options, [{reuse_sessions, false},
                                                   {renegotiate_at, N} | ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok), 

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).
%%--------------------------------------------------------------------
erlang_server_openssl_client_nowrap_seqnum() ->
    [{doc, "Test that erlang server will renegotiate session when",
      "max sequence number celing is about to be reached. Although"
      "in the testcase we use the test option renegotiate_at"
      " to lower treashold substantially."}].
erlang_server_openssl_client_nowrap_seqnum(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    N = 10,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {from, self()},
                                        {mfa, {ssl_test_lib,
                                               trigger_renegotiate, [[Data, N+2]]}},
                                        {options, [{renegotiate_at, N}, {reuse_sessions, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl", 
    Args = ["s_client","-connect", ssl_test_lib:hostname_format(Hostname) ++ ":" ++ integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-msg"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args), 

    true = port_command(OpenSslPort, Data),

    ssl_test_lib:check_result(Server, ok),

    %% Clean close down! Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
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
    
