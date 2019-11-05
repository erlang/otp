%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-module(openssl_session_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(SLEEP, 1000).
-define(EXPIRE, 10).

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
             [{'tlsv1.2', [], tests()},
              {'tlsv1.1', [], tests()},
              {'tlsv1', [], tests()},
              {'sslv3', [], tests()},
              {'dtlsv1.2', [], tests()},
              {'dtlsv1', [], tests()}
             ];
        false ->
             [{'tlsv1.2', [], tests()},
              {'tlsv1.1', [], tests()},
              {'tlsv1', [], tests()},
              {'sslv3', [], tests()}
           ]
     end.
 
tests() ->
    [    
         reuse_session_erlang_server,
         reuse_session_erlang_client
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

init_per_testcase(reuse_session_erlang_client, Config) ->
    ct:timetrap(?EXPIRE * 1000 * 5),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, ?EXPIRE),
    ssl:start(),
    Config;
init_per_testcase(reuse_session_erlang_server, Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    case ssl_test_lib:is_dtls_version(Version) of
        true ->
            case ssl_test_lib:openssl_sane_dtls_session_reuse() of
                true ->
                    ct:timetrap({seconds, 10}),
                    Config;
                false ->
                    {skip, "Broken OpenSSL DTLS session reuse"}
            end;
        false ->
            ct:timetrap({seconds, 10}),
            Config
    end;
init_per_testcase(TestCase, Config) ->
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(reuse_session_erlang_client, Config) ->
    application:unset_env(ssl, session_lifetime),
    Config;
end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
reuse_session_erlang_server() ->
    [{doc, "Test erlang server with openssl client that reconnects with the"
      "same session id, to test reusing of sessions."}].
reuse_session_erlang_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {from, self()},
                                        {mfa, {ssl_test_lib, active_recv, [length(Data)]}},
                                        {reconnect_times, 5},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),
    
    Exe = "openssl",
    Args = ["s_client", "-connect", ssl_test_lib:hostname_format(Hostname)
            ++ ":" ++ integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-reconnect"],
    
    OpenSslPort =  ssl_test_lib:portable_open_port(Exe, Args),
    
    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, Data),
    
    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort).

%%--------------------------------------------------------------------

reuse_session_erlang_client() ->
    [{doc, "Test erlang ssl client that wants to reuse sessions"}].
reuse_session_erlang_client(Config) when is_list(Config) -> 
    process_flag(trap_exit, true),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Version = ssl_test_lib:protocol_version(Config),    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    CACertFile = proplists:get_value(cacertfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
            "-cert", CertFile,"-key", KeyFile, "-CAfile", CACertFile],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port,  proplists:get_value(protocol, Config)),
    
    Client0 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()},  {options, [{reuse_sessions, save}, {verify, verify_peer}| ClientOpts]}]),
    
    SID = receive
              {Client0, Id0} ->
                  Id0
          end,
    
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
    
    
    ssl_test_lib:close(Client1),
    %% Make sure session is unregistered due to expiration
    ct:sleep(20000),
    
    Client2 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()},  {options, ClientOpts}]),
    receive
        {Client2, ID} ->
            case ID of
                SID ->    
                    ct:fail(expired_session_reused);
                _  ->
                    ok
            end
    end,
    
    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client2).


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
