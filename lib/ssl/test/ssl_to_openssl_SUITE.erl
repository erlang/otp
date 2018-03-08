%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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

-module(ssl_to_openssl_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(SLEEP, 1000).
-define(OPENSSL_RENEGOTIATE, "R\n").
-define(OPENSSL_QUIT, "Q\n").
-define(OPENSSL_GARBAGE, "P\n").
-define(EXPIRE, 10).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    [
     {group, basic},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [{basic, [], basic_tests()},
     {'tlsv1.2', [], all_versions_tests() ++ alpn_tests() ++ npn_tests() ++ sni_server_tests()},
     {'tlsv1.1', [], all_versions_tests() ++ alpn_tests() ++ npn_tests() ++ sni_server_tests()},
      {'tlsv1', [], all_versions_tests()++ alpn_tests() ++ npn_tests() ++ sni_server_tests()},
      {'sslv3', [], all_versions_tests()},
      {'dtlsv1.2', [], dtls_all_versions_tests()},
      {'dtlsv1', [], dtls_all_versions_tests()}
     ].

basic_tests() ->
    [basic_erlang_client_openssl_server,
     basic_erlang_server_openssl_client,
     expired_session
    ].

all_versions_tests() ->
    [
     erlang_client_openssl_server,
     erlang_server_openssl_client,
     erlang_client_openssl_server_dsa_cert,
     erlang_server_openssl_client_dsa_cert,
     erlang_client_openssl_server_anon,
     erlang_server_openssl_client_anon,
     erlang_server_openssl_client_anon_with_cert,
     erlang_server_openssl_client_reuse_session,
     erlang_client_openssl_server_renegotiate,
     erlang_client_openssl_server_nowrap_seqnum,
     erlang_server_openssl_client_nowrap_seqnum,
     erlang_client_openssl_server_no_server_ca_cert,
     erlang_client_openssl_server_client_cert,
     erlang_server_openssl_client_client_cert,
     ciphers_rsa_signed_certs,
     ciphers_dsa_signed_certs,
     erlang_client_bad_openssl_server,
     expired_session,
     ssl2_erlang_server_openssl_client
    ].
dtls_all_versions_tests() ->
    [
     erlang_client_openssl_server,
     erlang_server_openssl_client,
     erlang_client_openssl_server_dsa_cert,
     erlang_server_openssl_client_dsa_cert,
     erlang_client_openssl_server_anon,
     erlang_server_openssl_client_anon,
     erlang_server_openssl_client_anon_with_cert,
     erlang_server_openssl_client_reuse_session,
     erlang_client_openssl_server_renegotiate,
     erlang_client_openssl_server_nowrap_seqnum,
     erlang_server_openssl_client_nowrap_seqnum,
     erlang_client_openssl_server_no_server_ca_cert,
     erlang_client_openssl_server_client_cert,
     erlang_server_openssl_client_client_cert,
     ciphers_rsa_signed_certs,
     ciphers_dsa_signed_certs
     %%erlang_client_bad_openssl_server,
     %%expired_session
    ].

alpn_tests() ->
    [erlang_client_alpn_openssl_server_alpn,
     erlang_server_alpn_openssl_client_alpn,
     erlang_client_alpn_openssl_server,
     erlang_client_openssl_server_alpn,
     erlang_server_alpn_openssl_client,
     erlang_server_openssl_client_alpn,
     erlang_client_alpn_openssl_server_alpn_renegotiate,
     erlang_server_alpn_openssl_client_alpn_renegotiate,
     erlang_client_alpn_npn_openssl_server_alpn_npn,
     erlang_server_alpn_npn_openssl_client_alpn_npn].

npn_tests() ->
    [erlang_client_openssl_server_npn,
     erlang_server_openssl_client_npn,
     erlang_server_openssl_client_npn_renegotiate,
     erlang_client_openssl_server_npn_renegotiate,
     erlang_server_openssl_client_npn_only_client,
     erlang_server_openssl_client_npn_only_server,
     erlang_client_openssl_server_npn_only_client,
     erlang_client_openssl_server_npn_only_server].

sni_server_tests() ->
    [erlang_server_openssl_client_sni_match,
     erlang_server_openssl_client_sni_match_fun,
     erlang_server_openssl_client_sni_no_match,
     erlang_server_openssl_client_sni_no_match_fun,
     erlang_server_openssl_client_sni_no_header,
     erlang_server_openssl_client_sni_no_header_fun].


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
                    Config = 
                        case ssl_test_lib:openssl_dsa_support() of
                            true ->
                                Config1 = ssl_test_lib:make_rsa_cert(Config0),
                                ssl_test_lib:make_dsa_cert(Config1);
                            false ->
                                ssl_test_lib:make_rsa_cert(Config0)
                        end,
                    ssl_test_lib:cipher_restriction(Config)
            catch _:_  ->
                    {skip, "Crypto did not start"}
            end
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(basic, Config0) ->
    ssl_test_lib:clean_tls_version(Config0);

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

init_per_testcase(expired_session, Config) ->
    ct:timetrap(?EXPIRE * 1000 * 5),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, ?EXPIRE),
    ssl:start(),
     Config;

init_per_testcase(TestCase, Config) when 
      TestCase == ciphers_dsa_signed_certs;
      TestCase ==  erlang_client_openssl_server_dsa_cert;
      TestCase == erlang_server_openssl_client_dsa_cert;
       TestCase == erlang_client_openssl_server_dsa_cert;
      TestCase ==  erlang_server_openssl_client_dsa_cert ->
    case ssl_test_lib:openssl_dsa_support() of
        true ->
            special_init(TestCase, Config);
        false ->
            {skip, "DSA not supported by OpenSSL"}
    end; 
init_per_testcase(TestCase, Config) ->
    ct:timetrap({seconds, 35}),
    special_init(TestCase, Config).

special_init(TestCase, Config) when 
      TestCase == ciphers_rsa_signed_certs;
      TestCase == ciphers_dsa_signed_certs->
    ct:timetrap({seconds, 90}),
    Config;
special_init(TestCase, Config)
  when TestCase == erlang_client_openssl_server_renegotiate;
        TestCase == erlang_client_openssl_server_nowrap_seqnum;
       TestCase == erlang_server_openssl_client_nowrap_seqnum
        ->
    {ok, Version} = application:get_env(ssl, protocol_version),
    check_sane_openssl_renegotaite(Config, Version);

special_init(ssl2_erlang_server_openssl_client, Config) ->
    case ssl_test_lib:supports_ssl_tls_version(sslv2) of
        true ->
            Config;
        false ->
            {skip, "sslv2 not supported by openssl"}
     end;

special_init(TestCase, Config)
     when TestCase == erlang_client_alpn_openssl_server_alpn;
          TestCase == erlang_server_alpn_openssl_client_alpn;
          TestCase == erlang_client_alpn_openssl_server;
          TestCase == erlang_client_openssl_server_alpn;
          TestCase == erlang_server_alpn_openssl_client;
          TestCase == erlang_server_openssl_client_alpn ->
    check_openssl_alpn_support(Config);

special_init(TestCase, Config)
     when TestCase == erlang_client_alpn_openssl_server_alpn_renegotiate;
          TestCase == erlang_server_alpn_openssl_client_alpn_renegotiate ->
     {ok, Version} = application:get_env(ssl, protocol_version),
     case check_sane_openssl_renegotaite(Config, Version) of
         {skip, _} = Skip ->
             Skip;
         _ ->
             check_openssl_alpn_support(Config)
     end;

special_init(TestCase, Config)
  when TestCase == erlang_client_alpn_npn_openssl_server_alpn_npn;
       TestCase == erlang_server_alpn_npn_openssl_client_alpn_npn ->
    case check_openssl_alpn_support(Config) of
        {skip, _} = Skip ->
            Skip;
        _ ->
             check_openssl_npn_support(Config)
    end;

special_init(TestCase, Config)
  when TestCase == erlang_client_openssl_server_npn;
       TestCase == erlang_server_openssl_client_npn;
       TestCase == erlang_server_openssl_client_npn_only_server;
       TestCase == erlang_server_openssl_client_npn_only_client;
       TestCase == erlang_client_openssl_server_npn_only_client;
       TestCase == erlang_client_openssl_server_npn_only_server ->
    check_openssl_npn_support(Config);

special_init(TestCase, Config)
  when TestCase == erlang_server_openssl_client_npn_renegotiate;
       TestCase == erlang_client_openssl_server_npn_renegotiate ->
    {ok, Version} = application:get_env(ssl, protocol_version),
     case check_sane_openssl_renegotaite(Config, Version) of
         {skip, _} = Skip ->
             Skip;
         _ ->
             check_openssl_npn_support(Config)
     end;

special_init(TestCase, Config0)
  when TestCase == erlang_server_openssl_client_sni_match;
       TestCase == erlang_server_openssl_client_sni_no_match;
       TestCase == erlang_server_openssl_client_sni_no_header;
       TestCase == erlang_server_openssl_client_sni_match_fun;
       TestCase == erlang_server_openssl_client_sni_no_match_fun;
       TestCase == erlang_server_openssl_client_sni_no_header_fun ->
     RsaOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config0),
    Config = [{sni_server_opts, [{sni_hosts,
                                  [{"a.server", [
                                                 {certfile, proplists:get_value(certfile, RsaOpts)},
                                                 {keyfile,  proplists:get_value(keyfile, RsaOpts)}
                                                ]},
                                   {"b.server", [
                                                  {certfile, proplists:get_value(certfile, RsaOpts)},
                                                 {keyfile, proplists:get_value(keyfile, RsaOpts)}
                                                ]}
                                  ]}]} | Config0], 
    check_openssl_sni_support(Config);

special_init(_, Config) ->
     Config.

end_per_testcase(reuse_session_expired, Config) ->
    application:unset_env(ssl, session_lifetime),
     Config;
end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
basic_erlang_client_openssl_server() ->
    [{doc,"Test erlang client with openssl server"}].
basic_erlang_client_openssl_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    
    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    Exe = "openssl",
     Args = ["s_server", "-accept", integer_to_list(Port),
             "-cert", CertFile, "-key", KeyFile],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args), 


    ssl_test_lib:wait_for_openssl_server(Port, tls),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                         {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               erlang_ssl_receive, [Data]}},
                                         {options, ClientOpts}]),
    true = port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------    
basic_erlang_server_openssl_client() ->
    [{doc,"Test erlang server with openssl client"}].
basic_erlang_server_openssl_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {from, self()},
                                        {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
                                        {options,ServerOpts}]),

    Port = ssl_test_lib:inet_port(Server),
    
    Exe = "openssl",
    Args = ["s_client", "-connect", hostname_format(Hostname) ++
                ":" ++ integer_to_list(Port) ++ no_v2_flag() | workaround_openssl_s_clinent()],
    
    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args), 
    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
erlang_client_openssl_server() ->
    [{doc,"Test erlang client with openssl server"}].
erlang_client_openssl_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-cert", CertFile, "-key", KeyFile],

    OpensslPort =  ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                                erlang_ssl_receive, [Data]}},
                                        {options, ClientOpts}]),
    true = port_command(OpensslPort, Data),
    
    ssl_test_lib:check_result(Client, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
erlang_server_openssl_client() ->
    [{doc,"Test erlang server with openssl client"}].
erlang_server_openssl_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                         {from, self()},
                                        {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_client", "-connect", hostname_format(Hostname) ++":" ++ integer_to_list(Port), 
             ssl_test_lib:version_flag(Version)],
    
    OpenSslPort =  ssl_test_lib:portable_open_port(Exe, Args),

    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

erlang_client_openssl_server_dsa_cert() ->
    [{doc,"Test erlang server with openssl client"}].
erlang_client_openssl_server_dsa_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ssl_test_lib:ssl_options(client_dsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_dsa_verify_opts, Config),

     {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CaCertFile =  proplists:get_value(cacertfile, ServerOpts),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-cert", CertFile, "-CAfile", CaCertFile,
             "-key", KeyFile, "-Verify", "2", "-msg"],

    OpensslPort =  ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               erlang_ssl_receive, [Data]}},
                                        {options, ClientOpts}]),

    true = port_command(OpensslPort, Data),
    
     ssl_test_lib:check_result(Client, ok),

    %% Clean close down!   Server needs to be closed first !!
     ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
erlang_server_openssl_client_dsa_cert() ->
    [{doc,"Test erlang server with openssl client"}].
erlang_server_openssl_client_dsa_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ssl_test_lib:ssl_options(client_dsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_dsa_verify_opts, Config),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",
    CaCertFile =  proplists:get_value(cacertfile, ClientOpts),
    CertFile = proplists:get_value(certfile, ClientOpts),
    KeyFile = proplists:get_value(keyfile, ClientOpts),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_client", "-connect", hostname_format(Hostname) ++ ":" ++ integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-cert", CertFile,
            "-CAfile", CaCertFile,
            "-key", KeyFile, "-msg"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args), 
    true = port_command(OpenSslPort, Data),

    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

 %%--------------------------------------------------------------------
erlang_client_openssl_server_anon() ->
     [{doc,"Test erlang client with openssl server, anonymous"}].
erlang_client_openssl_server_anon(Config) when is_list(Config) ->
     process_flag(trap_exit, true),
    %% OpenSSL expects a certificate and key, even if the cipher spec
    %% is restructed to aNULL, so we use 'server_rsa_opts' here
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_anon_opts, Config),
    VersionTuple = ssl_test_lib:protocol_version(Config, tuple),
    Ciphers = ssl_test_lib:anonymous_suites(VersionTuple),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-cert", CertFile, "-key", KeyFile,
            "-cipher", "aNULL", "-msg"],

    OpensslPort =  ssl_test_lib:portable_open_port(Exe, Args),

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

     Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                         {host, Hostname},
                                         {from, self()},
                                         {mfa, {?MODULE,
                                                erlang_ssl_receive, [Data]}},
                                         {options, [{ciphers, Ciphers} | ClientOpts]}]),

    true = port_command(OpensslPort, Data),

    ssl_test_lib:check_result(Client, ok),

    %% Clean close down!   Server needs to be closed first !!
     ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false),
    ok.
%%--------------------------------------------------------------------
erlang_server_openssl_client_anon() ->
    [{doc,"Test erlang server with openssl client, anonymous"}].
erlang_server_openssl_client_anon(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_anon_opts, Config),
    VersionTuple = ssl_test_lib:protocol_version(Config, tuple),
    Ciphers = ssl_test_lib:anonymous_suites(VersionTuple),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
                                        {options, [{ciphers, Ciphers} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_client", "-connect", hostname_format(Hostname) ++ ":" ++ integer_to_list(Port),
             ssl_test_lib:version_flag(Version),
            "-cipher", "aNULL", "-msg"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),
    true = port_command(OpenSslPort, Data),

     ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
     ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
erlang_server_openssl_client_anon_with_cert() ->
     [{doc,"Test erlang server with openssl client, anonymous (with cert)"}].
erlang_server_openssl_client_anon_with_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    VersionTuple = ssl_test_lib:protocol_version(Config, tuple),
    Ciphers = ssl_test_lib:anonymous_suites(VersionTuple),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
                                        {options, [{ciphers, Ciphers} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_client", "-connect", hostname_format(Hostname) ++ ":" ++ integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-cipher", "aNULL", "-msg"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),
    true = port_command(OpenSslPort, Data),

    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

 %%--------------------------------------------------------------------
erlang_server_openssl_client_reuse_session() ->
    [{doc, "Test erlang server with openssl client that reconnects with the"
      "same session id, to test reusing of sessions."}].
erlang_server_openssl_client_reuse_session(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {from, self()},
                                        {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
                                        {reconnect_times, 5},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_client", "-connect", hostname_format(Hostname)
            ++ ":" ++ integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-reconnect"],
    
    OpenSslPort =  ssl_test_lib:portable_open_port(Exe, Args),

    true = port_command(OpenSslPort, Data),
    
    ssl_test_lib:check_result(Server, ok),
    
    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
     process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

erlang_client_openssl_server_renegotiate() ->
    [{doc,"Test erlang client when openssl server issuses a renegotiate"}].
erlang_client_openssl_server_renegotiate(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    
    ErlData = "From erlang to openssl",
    OpenSslData = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),  
            ssl_test_lib:version_flag(Version),
            "-cert", CertFile, "-key", KeyFile, "-msg"],
    
    OpensslPort =  ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               delayed_send, [[ErlData, OpenSslData]]}},
                                         {options, ClientOpts}]),

    true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
    ct:sleep(?SLEEP),
    true = port_command(OpensslPort, OpenSslData),
    
    ssl_test_lib:check_result(Client, ok), 

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
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    ErlData = "From erlang to openssl\n",
    N = 10,

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
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
    [{doc, "Test that erlang client will renegotiate session when",
      "max sequence number celing is about to be reached. Although"
      "in the testcase we use the test option renegotiate_at"
      " to lower treashold substantially."}].
erlang_server_openssl_client_nowrap_seqnum(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

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
    Args = ["s_client","-connect", hostname_format(Hostname) ++ ":" ++ integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-msg"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args), 

    true = port_command(OpenSslPort, Data),

    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------

erlang_client_openssl_server_no_server_ca_cert() ->
    [{doc, "Test erlang client when openssl server sends a cert chain not"
      "including the ca cert. Explicitly test this even if it is"
      "implicitly tested eleswhere."}].
erlang_client_openssl_server_no_server_ca_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), 
            ssl_test_lib:version_flag(Version),
            "-cert", CertFile, "-key", KeyFile, "-msg"],

    OpensslPort =  ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               erlang_ssl_receive, [Data]}},
                                        {options, ClientOpts}]),

    true = port_command(OpensslPort, Data),

    ssl_test_lib:check_result(Client, ok), 

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------
erlang_client_openssl_server_client_cert() ->
    [{doc,"Test erlang client with openssl server when client sends cert"}].
erlang_client_openssl_server_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    CaCertFile = proplists:get_value(cacertfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),
            ssl_test_lib:version_flag(Version),
            "-cert", CertFile, "-CAfile", CaCertFile,
            "-key", KeyFile, "-Verify", "2"],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),   

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               erlang_ssl_receive, [Data]}},
                                        {options, ClientOpts}]),
    true = port_command(OpensslPort, Data),

    ssl_test_lib:check_result(Client, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort), 
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------  
erlang_server_openssl_client_client_cert() ->
    [{doc,"Test erlang server with openssl client when client sends cert"}].
erlang_server_openssl_client_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               erlang_ssl_receive, [Data]}},
                                        {options,
                                         [{verify , verify_peer}
                                          | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    CaCertFile = proplists:get_value(cacertfile, ClientOpts),
    CertFile = proplists:get_value(certfile, ClientOpts),
    KeyFile = proplists:get_value(keyfile, ClientOpts),
    Version = ssl_test_lib:protocol_version(Config),
    Exe = "openssl",
    Args = ["s_client", "-cert", CertFile,
            "-CAfile", CaCertFile,
            "-key", KeyFile,"-connect", hostname_format(Hostname) ++ ":" ++ integer_to_list(Port),
            ssl_test_lib:version_flag(Version)],
    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args), 

    true = port_command(OpenSslPort, Data),    
    ssl_test_lib:check_result(Server, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpenSslPort),
    ssl_test_lib:close(Server),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------  
erlang_server_erlang_client_client_cert() ->
    [{doc,"Test erlang server with erlang client when client sends cert"}].
erlang_server_erlang_client_client_cert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),  
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),  
    Version = ssl_test_lib:protocol_version(Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {from, self()},
                                        {mfa, {?MODULE,
                                               erlang_ssl_receive,
                                               %% Due to 1/n-1 splitting countermeasure Rizzo/Duong-Beast
                                               [Data]}},
                                        {options,
                                         [{verify , verify_peer}
                                          | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
                                        {from, self()},
                                        %% Due to 1/n-1 splitting countermeasure Rizzo/Duong-Beast
                                        {mfa, {ssl, send, [Data]}},
                                        {options,
                                         [{versions, [Version]} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------

ciphers_rsa_signed_certs() ->
    [{doc,"Test cipher suites that uses rsa certs"}].
ciphers_rsa_signed_certs(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    Ciphers = ssl_test_lib:rsa_suites(openssl),
    run_suites(Ciphers, Version, Config, rsa).
%%--------------------------------------------------------------------

ciphers_dsa_signed_certs() ->
    [{doc,"Test cipher suites that uses dsa certs"}].
ciphers_dsa_signed_certs(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    NVersion = ssl_test_lib:protocol_version(Config, tuple),
    Ciphers = ssl_test_lib:dsa_suites(NVersion),
    run_suites(Ciphers, Version, Config, dsa).

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
                                         {mfa, {ssl_test_lib, no_result_msg, []}},
                                         {options,
                                          [{versions, [Version]} | ClientOpts]}]),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client1),
    process_flag(trap_exit, false),
    ok.

%%--------------------------------------------------------------------

expired_session() ->
    [{doc, "Test our ssl client handling of expired sessions. Will make"
      "better code coverage of the ssl_manager module"}].
expired_session(Config) when is_list(Config) -> 
    process_flag(trap_exit, true),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port),
            "-cert", CertFile,"-key", KeyFile],

    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port, tls),

    Client0 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, no_result, []}},
                                   {from, self()},  {options, ClientOpts}]),

    ssl_test_lib:close(Client0),

    %% Make sure session is registered
    ct:sleep(?SLEEP),

    Client1 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, no_result, []}},
                                   {from, self()},  {options, ClientOpts}]),

    ssl_test_lib:close(Client1),
    %% Make sure session is unregistered due to expiration
    ct:sleep((?EXPIRE+1) * 1000),

    Client2 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, no_result, []}},
                                   {from, self()},  {options, ClientOpts}]),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),
    ssl_test_lib:close(Client2),
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
    Args = ["s_client", "-connect", hostname_format(Hostname) ++ ":" ++ integer_to_list(Port), 
            "-ssl2", "-msg"],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    ct:log("Ports ~p~n", [[erlang:port_info(P) || P <- erlang:ports()]]), 
    consume_port_exit(OpenSslPort),
    ssl_test_lib:check_result(Server, {error, {tls_alert, "bad record mac"}}),
    process_flag(trap_exit, false).

%%--------------------------------------------------------------------

erlang_client_alpn_openssl_server_alpn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_alpn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, Data),

        ssl_test_lib:check_result(Client, ok)
    end),
    ok.

%%--------------------------------------------------------------------

erlang_server_alpn_openssl_client_alpn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_alpn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, Data),

        ssl_test_lib:check_result(Client, ok)
    end),
    ok.

%%--------------------------------------------------------------------------

erlang_client_alpn_openssl_server(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_with_opts(Config,
						     [{alpn_advertised_protocols, [<<"spdy/2">>]}],
                                                     [],
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------

erlang_client_openssl_server_alpn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_with_opts(Config,
						     [],
                                                     ["-alpn", "spdy/2"],
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------

erlang_server_alpn_openssl_client(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_with_opts(Config,
						     [{alpn_preferred_protocols, [<<"spdy/2">>]}],
                                                     [],
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------

erlang_server_openssl_client_alpn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_with_opts(Config,
						     [],
                                                     ["-alpn", "spdy/2"],
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------

erlang_client_alpn_openssl_server_alpn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_alpn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
        ct:sleep(?SLEEP),
        true = port_command(OpensslPort, Data),

        ssl_test_lib:check_result(Client, ok)
    end),
    ok.

%%--------------------------------------------------------------------

erlang_server_alpn_openssl_client_alpn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_alpn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
        ct:sleep(?SLEEP),
        true = port_command(OpensslPort, Data),

        ssl_test_lib:check_result(Client, ok)
    end),
    ok.

%%--------------------------------------------------------------------

erlang_client_alpn_npn_openssl_server_alpn_npn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_alpn_npn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, Data),

        ssl_test_lib:check_result(Client, ok)
    end),
    ok.

%%--------------------------------------------------------------------

erlang_server_alpn_npn_openssl_client_alpn_npn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_alpn_npn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, Data),

        ssl_test_lib:check_result(Client, ok)
    end),
    ok.

%%--------------------------------------------------------------------
erlang_client_openssl_server_npn() ->
    [{doc,"Test erlang client with openssl server doing npn negotiation"}].

erlang_client_openssl_server_npn(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, Data),

        ssl_test_lib:check_result(Client, ok)
    end),
    ok.

%%--------------------------------------------------------------------
erlang_client_openssl_server_npn_renegotiate() ->
    [{doc,"Test erlang client with openssl server doing npn negotiation and renegotiate"}].

erlang_client_openssl_server_npn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, fun(Client, OpensslPort) ->
        true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
        ct:sleep(?SLEEP),
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Client, ok)
    end),
    ok.
%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn() ->
    [{doc,"Test erlang server with openssl client and npn negotiation"}].

erlang_server_openssl_client_npn(Config) when is_list(Config) ->

    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn_renegotiate() ->
    [{doc,"Test erlang server with openssl client and npn negotiation with renegotiation"}].

erlang_server_openssl_client_npn_renegotiate(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, ?OPENSSL_RENEGOTIATE),
        ct:sleep(?SLEEP),
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.
%%--------------------------------------------------------------------------
erlang_client_openssl_server_npn_only_server(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_with_opts(Config, [],
						     ["-nextprotoneg", "spdy/2"], Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------

erlang_client_openssl_server_npn_only_client(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_client_and_openssl_server_with_opts(Config,
						     [{client_preferred_next_protocols,
						       {client, [<<"spdy/2">>], <<"http/1.1">>}}], [],
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

%%--------------------------------------------------------------------------
erlang_server_openssl_client_npn_only_server(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_with_opts(Config, [{next_protocols_advertised, [<<"spdy/2">>]}], [],
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.

erlang_server_openssl_client_npn_only_client(Config) when is_list(Config) ->
    Data = "From openssl to erlang",
    start_erlang_server_and_openssl_client_with_opts(Config, [], ["-nextprotoneg", "spdy/2"],
						     Data, fun(Server, OpensslPort) ->
        true = port_command(OpensslPort, Data),
        ssl_test_lib:check_result(Server, ok)
    end),
    ok.
%--------------------------------------------------------------------------
erlang_server_openssl_client_sni_no_header(Config) when is_list(Config) ->
    erlang_server_openssl_client_sni_test(Config, undefined, undefined, "server Peer cert").

erlang_server_openssl_client_sni_no_header_fun(Config) when is_list(Config) ->
    erlang_server_openssl_client_sni_test_sni_fun(Config, undefined, undefined, "server Peer cert").

erlang_server_openssl_client_sni_match(Config) when is_list(Config) ->  
    erlang_server_openssl_client_sni_test(Config, "a.server", "a.server", "server Peer cert").

erlang_server_openssl_client_sni_match_fun(Config) when is_list(Config) ->
    erlang_server_openssl_client_sni_test_sni_fun(Config, "a.server", "a.server", "server Peer cert").

erlang_server_openssl_client_sni_no_match(Config) when is_list(Config) ->
    erlang_server_openssl_client_sni_test(Config, "c.server", undefined, "server Peer cert").

erlang_server_openssl_client_sni_no_match_fun(Config) when is_list(Config) ->
    erlang_server_openssl_client_sni_test_sni_fun(Config, "c.server", undefined, "server Peer cert").


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
run_suites(Ciphers, Version, Config, Type) ->
    {ClientOpts, ServerOpts} =
	case Type of
	    rsa ->
		{ssl_test_lib:ssl_options(client_rsa_opts, Config),
		 ssl_test_lib:ssl_options(server_rsa_opts, Config)};
	    dsa ->
		{ssl_test_lib:ssl_options(client_dsa_opts, Config),
		 ssl_test_lib:ssl_options(server_dsa_verify_opts, Config)}
	end,

    Result =  lists:map(fun(Cipher) ->
				cipher(Cipher, Version, Config, ClientOpts, ServerOpts) end,
			Ciphers),
    case lists:flatten(Result) of
	[] ->
	    ok;
	Error ->
	    ct:log("Cipher suite errors: ~p~n", [Error]),
	    ct:fail(cipher_suite_failed_see_test_case_log)
    end.

client_read_check([], _Data) -> 
    ok;
client_read_check([Hd | T], Data) ->
    case binary:match(Data, list_to_binary(Hd)) of
        nomatch ->
            nomatch;
        _ ->
            client_read_check(T, Data)
    end.
client_check_result(Port, DataExpected, DataReceived) ->
    receive
        {Port, {data, TheData}} ->
            Data = list_to_binary(TheData),
            NewData = <<DataReceived/binary, Data/binary>>,
            ct:log("New Data: ~p", [NewData]),
            case client_read_check(DataExpected, NewData) of
                ok ->
                    ok;
                _ ->
                    client_check_result(Port, DataExpected, NewData)
            end
    after 20000 ->
	    ct:fail({"Time out on openSSL Client", {expected, DataExpected},
		     {got, DataReceived}})   
    end.
client_check_result(Port, DataExpected) ->
    client_check_result(Port, DataExpected, <<"">>).

send_and_hostname(SSLSocket) ->
    ssl:send(SSLSocket, "OK"),
    case ssl:connection_information(SSLSocket, [sni_hostname]) of
	{ok, []} ->
	    undefined;
	{ok, [{sni_hostname, Hostname}]} ->
	    Hostname
    end.

erlang_server_openssl_client_sni_test(Config, SNIHostname, ExpectedSNIHostname, ExpectedCN) ->
    ct:log("Start running handshake, Config: ~p, SNIHostname: ~p, ExpectedSNIHostname: ~p, ExpectedCN: ~p", [Config, SNIHostname, ExpectedSNIHostname, ExpectedCN]),
    ServerOptions = proplists:get_value(sni_server_opts, Config) ++ proplists:get_value(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {?MODULE, send_and_hostname, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Exe = "openssl",
    ClientArgs = case SNIHostname of
		     undefined ->
			 openssl_client_args(ssl_test_lib:supports_ssl_tls_version(sslv2), Hostname,Port);
		     _ ->
			 openssl_client_args(ssl_test_lib:supports_ssl_tls_version(sslv2), Hostname, Port, SNIHostname)
		 end,       
    ClientPort = ssl_test_lib:portable_open_port(Exe, ClientArgs),  
  
    ssl_test_lib:check_result(Server, ExpectedSNIHostname),
    ssl_test_lib:close_port(ClientPort),
    ssl_test_lib:close(Server),
    ok.


erlang_server_openssl_client_sni_test_sni_fun(Config, SNIHostname, ExpectedSNIHostname, ExpectedCN) ->
    ct:log("Start running handshake for sni_fun, Config: ~p, SNIHostname: ~p, ExpectedSNIHostname: ~p, ExpectedCN: ~p", [Config, SNIHostname, ExpectedSNIHostname, ExpectedCN]),
    [{sni_hosts, ServerSNIConf}] = proplists:get_value(sni_server_opts, Config),
    SNIFun = fun(Domain) -> proplists:get_value(Domain, ServerSNIConf, undefined) end,
    ServerOptions = proplists:get_value(server_rsa_opts, Config) ++ [{sni_fun, SNIFun}],
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {?MODULE, send_and_hostname, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Exe = "openssl",
    ClientArgs = case SNIHostname of
		     undefined ->
			 openssl_client_args(ssl_test_lib:supports_ssl_tls_version(sslv2), Hostname,Port);
		     _ ->
			 openssl_client_args(ssl_test_lib:supports_ssl_tls_version(sslv2), Hostname, Port, SNIHostname)
		 end,       

    ClientPort = ssl_test_lib:portable_open_port(Exe, ClientArgs), 
    
    ssl_test_lib:check_result(Server, ExpectedSNIHostname),
    ssl_test_lib:close_port(ClientPort),
    ssl_test_lib:close(Server).


cipher(CipherSuite, Version, Config, ClientOpts, ServerOpts) ->
    process_flag(trap_exit, true),
    ct:log("Testing CipherSuite ~p~n", [CipherSuite]),
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),

    Exe = "openssl",
    Args = ["s_server", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
	    "-cert", CertFile, "-key", KeyFile],

    OpenSslPort =  ssl_test_lib:portable_open_port(Exe, Args), 

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    ConnectionInfo = {ok, {Version, CipherSuite}},

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, cipher_result, [ConnectionInfo]}},
					{options,
					 [{ciphers,[CipherSuite]} |
			     ClientOpts]}]),

    true = port_command(OpenSslPort, "Hello\n"),

    receive
	{Port, {data, _}} when is_port(Port) ->
	    ok
    after 500 ->
	    ct:log("Time out on openssl port, check that"
			       " the messages Hello and world are received"
			       " during close of port" , []),
	    ok
    end,

    true = port_command(OpenSslPort, " world\n"),

    Result = ssl_test_lib:wait_for_result(Client, ok),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpenSslPort),
    ssl_test_lib:close(Client),

    Return = case Result of
		 ok ->
		     [];
		 Error ->
		     [{CipherSuite, Error}]
	     end,
    process_flag(trap_exit, false),
    Return.

start_erlang_client_and_openssl_server_with_opts(Config, ErlangClientOpts, OpensslServerOpts, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = ErlangClientOpts ++ ClientOpts0,

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = case OpensslServerOpts of
	       [] -> 
		   ["s_server", "-accept", 
		    integer_to_list(Port), ssl_test_lib:version_flag(Version),
		    "-cert", CertFile,"-key", KeyFile];
	       [Opt, Value] ->
		   ["s_server", Opt, Value, "-accept", 
		    integer_to_list(Port), ssl_test_lib:version_flag(Version),
		    "-cert", CertFile,"-key", KeyFile]
	   end,
		   
    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {?MODULE,
                           erlang_ssl_receive, [Data]}},
                    {options, ClientOpts}]),

    Callback(Client, OpensslPort),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_client_and_openssl_server_for_alpn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    ClientOpts0 = proplists:get_value(client_rsa_opts, Config),
    ClientOpts = [{alpn_advertised_protocols, [<<"spdy/2">>]} | ClientOpts0],

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_server", "-msg", "-alpn", "http/1.1,spdy/2", "-accept", integer_to_list(Port), ssl_test_lib:version_flag(Version),
	    "-cert", CertFile, "-key", KeyFile],
    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),  
    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {?MODULE,
                           erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options, ClientOpts}]),

    Callback(Client, OpensslPort),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_server_and_openssl_client_for_alpn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 = proplists:get_value(server_rsa_opts, Config),
    ServerOpts = [{alpn_preferred_protocols, [<<"spdy/2">>]} | ServerOpts0],

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
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
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    ClientOpts0 = proplists:get_value(client_rsa_opts, Config),
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
                    {mfa, {?MODULE,
                           erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options, ClientOpts}]),

    Callback(Client, OpensslPort),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_server_and_openssl_client_for_alpn_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 = proplists:get_value(server_rsa_opts, Config),
    ServerOpts = [{alpn_preferred_protocols, [<<"spdy/2">>]},
        {next_protocols_advertised, [<<"spdy/3">>, <<"http/1.1">>]} | ServerOpts0],

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
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

start_erlang_client_and_openssl_server_for_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = [{client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"http/1.1">>}} | ClientOpts0],

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From openssl to erlang",

    Port = ssl_test_lib:inet_port(node()),
    CertFile = proplists:get_value(certfile, ServerOpts),
    KeyFile = proplists:get_value(keyfile, ServerOpts),
    Version = ssl_test_lib:protocol_version(Config),
    
    Exe = "openssl",
    Args = ["s_server", "-msg", "-nextprotoneg", "http/1.1,spdy/2", "-accept", integer_to_list(Port),
	    ssl_test_lib:version_flag(Version),
	    "-cert", CertFile, "-key", KeyFile],
    OpensslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    ssl_test_lib:wait_for_openssl_server(Port, proplists:get_value(protocol, Config)),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {?MODULE,
                           erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options, ClientOpts}]),

    Callback(Client, OpensslPort),

    %% Clean close down!   Server needs to be closed first !!
    ssl_test_lib:close_port(OpensslPort),

    ssl_test_lib:close(Client),
    process_flag(trap_exit, false).

start_erlang_server_and_openssl_client_for_npn_negotiation(Config, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{next_protocols_advertised, [<<"spdy/2">>]}, ServerOpts0],

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, erlang_ssl_receive_and_assert_negotiated_protocol, [<<"spdy/2">>, Data]}},
                    {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),

    Exe = "openssl",
    Args = ["s_client", "-nextprotoneg", "http/1.0,spdy/2", "-msg", "-connect", 
            hostname_format(Hostname) ++ ":" 
	    ++ integer_to_list(Port), ssl_test_lib:version_flag(Version)],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),  

    Callback(Server, OpenSslPort),

    ssl_test_lib:close(Server),

    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).


start_erlang_server_and_openssl_client_with_opts(Config, ErlangServerOpts, OpenSSLClientOpts, Data, Callback) ->
    process_flag(trap_exit, true),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = ErlangServerOpts ++  ServerOpts0,

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
                    {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Version = ssl_test_lib:protocol_version(Config),
   
    Exe = "openssl",
    Args = ["s_client"] ++ OpenSSLClientOpts ++ ["-msg",  "-connect", 
                                                 hostname_format(Hostname) ++ ":" ++ integer_to_list(Port),
                                                 ssl_test_lib:version_flag(Version)],

    OpenSslPort = ssl_test_lib:portable_open_port(Exe, Args),

    Callback(Server, OpenSslPort),

    ssl_test_lib:close(Server),

    ssl_test_lib:close_port(OpenSslPort),
    process_flag(trap_exit, false).


erlang_ssl_receive_and_assert_negotiated_protocol(Socket, Protocol, Data) ->
    {ok, Protocol} = ssl:negotiated_protocol(Socket),
    erlang_ssl_receive(Socket, Data),
    {ok, Protocol} = ssl:negotiated_protocol(Socket),
    ok.

erlang_ssl_receive(Socket, Data) ->
    ct:log("Connection info: ~p~n",
		       [ssl:connection_information(Socket)]),
    receive
	{ssl, Socket, Data} ->
	    io:format("Received ~p~n",[Data]),
	    %% open_ssl server sometimes hangs waiting in blocking read
	    ssl:send(Socket, "Got it"), 
	    ok;
	{ssl, Socket, Byte} when length(Byte) == 1 ->
	    erlang_ssl_receive(Socket, tl(Data));
	{Port, {data,Debug}} when is_port(Port) ->
	    io:format("openssl ~s~n",[Debug]),
	    erlang_ssl_receive(Socket,Data);
	Other ->
	    ct:fail({unexpected_message, Other})
    end.
 
connection_info(Socket, Version) ->
    case ssl:connection_information(Socket, [version]) of
	{ok, [{version, Version}] = Info} ->
	    ct:log("Connection info: ~p~n", [Info]),
	    ok;
	{ok,  [{version, OtherVersion}]} ->
	    {wrong_version, OtherVersion}
    end.

connection_info_result(Socket) ->                                            
    ssl:connection_information(Socket).


delayed_send(Socket, [ErlData, OpenSslData]) ->
    ct:sleep(?SLEEP),
    ssl:send(Socket, ErlData),
    erlang_ssl_receive(Socket, OpenSslData).

server_sent_garbage(Socket) ->
    receive 
	server_sent_garbage ->
	    {error, closed} == ssl:send(Socket, "data")
	    
    end.
    
check_openssl_sni_support(Config) ->
    HelpText = os:cmd("openssl s_client --help"),
    case string:str(HelpText, "-servername") of
        0 ->
            {skip, "Current openssl doesn't support SNI"};
        _ ->
            Config
    end.

check_openssl_npn_support(Config) ->
    HelpText = os:cmd("openssl s_client --help"),
    case string:str(HelpText, "nextprotoneg") of
        0 ->
            {skip, "Openssl not compiled with nextprotoneg support"};
        _ ->
            Config
    end.

check_openssl_alpn_support(Config) ->
    HelpText = os:cmd("openssl s_client --help"),
    case string:str(HelpText, "alpn") of
        0 ->
            {skip, "Openssl not compiled with alpn support"};
        _ ->
            Config
    end.

check_sane_openssl_renegotaite(Config, Version) when Version == 'tlsv1.1';
						     Version == 'tlsv1.2' ->
    case os:cmd("openssl version") of     
	"OpenSSL 1.0.1c" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 1.0.1b" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 1.0.1a" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 1.0.1 " ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	_ ->
	    check_sane_openssl_renegotaite(Config)
    end;
check_sane_openssl_renegotaite(Config, _) ->
    check_sane_openssl_renegotaite(Config).
	
check_sane_openssl_renegotaite(Config) ->
    case os:cmd("openssl version") of  
	"OpenSSL 1.0.0" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 0.9.8" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	"OpenSSL 0.9.7" ++ _ ->
	    {skip, "Known renegotiation bug in OpenSSL"};
	_ ->
	    Config
    end.

workaround_openssl_s_clinent() ->
    %% http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=683159
    %% https://bugs.archlinux.org/task/33919
    %% Bug seems to manifests it self if TLS version is not
    %% explicitly specified 
    case os:cmd("openssl version") of 
	"OpenSSL 1.0.1c" ++ _ ->
	    ["-no_tls1_2"];
	"OpenSSL 1.0.1d" ++ _ ->
	    ["-no_tls1_2"];
	"OpenSSL 1.0.1e" ++ _ ->
	    ["-no_tls1_2"];
	"OpenSSL 1.0.1f" ++ _ ->
	    ["-no_tls1_2"];
	_  ->
	    []
    end.

openssl_client_args(false, Hostname, Port) ->
    ["s_client", "-connect", Hostname ++ ":" ++ integer_to_list(Port)];
openssl_client_args(true, Hostname, Port) ->
    ["s_client",  "-no_ssl2", "-connect", Hostname ++ ":" ++ integer_to_list(Port)].

openssl_client_args(false, Hostname, Port, ServerName) ->
    ["s_client",  "-connect", Hostname ++ ":" ++ 
	 integer_to_list(Port), "-servername", ServerName];
openssl_client_args(true, Hostname, Port, ServerName) ->
    ["s_client",  "-no_ssl2", "-connect", Hostname ++ ":" ++ 
	 integer_to_list(Port), "-servername", ServerName].

consume_port_exit(OpenSSLPort) ->
    receive    	
        {'EXIT', OpenSSLPort, _} ->
            ok
    end.

hostname_format(Hostname) ->
    case lists:member($., Hostname) of
        true ->  
            Hostname;
        false ->
            "localhost"   
    end.

no_v2_flag() ->
    case ssl_test_lib:supports_ssl_tls_version(sslv2) of
        true ->
            " -no_ssl2 ";
        false ->
            ""
    end.
