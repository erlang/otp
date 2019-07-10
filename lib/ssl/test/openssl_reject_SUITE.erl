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

-module(openssl_reject_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(SLEEP, 1000).
-define(OPENSSL_GARBAGE, "P\n").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    [{group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'}].

groups() ->
    [{'tlsv1.2', [], all_versions_tests()},
     {'tlsv1.1', [], all_versions_tests()},
     {'tlsv1', [], all_versions_tests()},
     {'sslv3', [], all_versions_tests()}
    ].
 
all_versions_tests() ->
    [    
     erlang_client_bad_openssl_server,
     ssl2_erlang_server_openssl_client
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

special_init(ssl2_erlang_server_openssl_client, Config) ->
    case ssl_test_lib:supports_ssl_tls_version(sslv2) of
        true ->
            Config;
        false ->
            {skip, "sslv2 not supported by openssl"}
     end;
special_init(_, Config) ->
     Config.

end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
erlang_client_bad_openssl_server() ->
    [{doc,"Test what happens if openssl server sends garbage to erlang ssl client"}].
erlang_client_bad_openssl_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
            "-cert", CertFile, "-key", KeyFile],
    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                         {host, Hostname},
                                         {from, self()},
                                         {mfa, {?MODULE, server_sent_garbage, []}},
                                         {options,
                                          [{versions, [Version]} | ClientOpts]}]),

    %% Send garbage
    true = port_command(OpensslPort, ?OPENSSL_GARBAGE),

    ct:sleep(?SLEEP),

    Client0 ! server_sent_garbage,

    ssl_test_lib:check_result(Client0, true),

    ssl_test_lib:close(Client0),

    %% Make sure openssl does not hang and leave zombie process
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                         {host, Hostname},
                                         {from, self()},
                                         {mfa, {ssl_test_lib, no_result, []}},
                                         {options,
                                          [{versions, [Version]} | ClientOpts]}]),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client1),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
ssl2_erlang_server_openssl_client() ->
    [{doc,"Test that ssl v2 clients are rejected"}].

ssl2_erlang_server_openssl_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
                                              {from, self()},
                                              {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Exe = "openssl",
    Args = ["s_client", "-connect", ssl_test_lib:hostname_format(Hostname) ++ ":" ++ integer_to_list(Port), 
            "-ssl2", "-msg"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    ct:log("Ports ~p~n", [[erlang:port_info(P) || P <- erlang:ports()]]), 
    ssl_test_lib:consume_port_exit(OpenSslPort),
    ssl_test_lib:check_server_alert(Server, unexpected_message),
    process_flag(trap_exit, false).


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

server_sent_garbage(Socket) ->
    receive 
	server_sent_garbage ->
	    {error, closed} == ssl:send(Socket, "data")
	    
    end.
