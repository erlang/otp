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

-module(openssl_sni_SUITE).

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
-export([sni_match/1,
         sni_match_fun/1,
         sni_no_match/1,
         sni_no_match_fun/1,
         sni_no_header/1,
         sni_no_header_fun/1
        ]).

%% Apply exports
-export([send_and_hostname/1
        ]).

-define(OPENSSL_QUIT, "Q\n").
-define(OPENSSL_RENEGOTIATE, "R\n").
-define(SLEEP, 1000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    %% Note: SNI not supported in sslv3
    case ssl_test_lib:openssl_sane_dtls() of 
        true ->
            [{group, 'tlsv1.3'},
             {group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'},
             {group, 'dtlsv1.2'},
             {group, 'dtlsv1'}
            ];
        false ->
            [{group, 'tlsv1.3'},
             {group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'}]
    end.

groups() ->
     case ssl_test_lib:openssl_sane_dtls() of 
         true ->
             [{'tlsv1.3', [], sni_tests()},
              {'tlsv1.2', [], sni_tests()},
              {'tlsv1.1', [], sni_tests()},
              {'tlsv1', [], sni_tests()},
              {'dtlsv1.2', [], sni_tests()},
              {'dtlsv1', [], sni_tests()}
             ];
        false ->
             [{'tlsv1.3', [], sni_tests()},
              {'tlsv1.2', [], sni_tests()},
              {'tlsv1.1', [], sni_tests()},
              {'tlsv1', [], sni_tests()}
             ]
     end.
 
sni_tests() ->
    [sni_match,
     sni_match_fun,
     sni_no_match,
     sni_no_match_fun,
     sni_no_header,
     sni_no_header_fun].

init_per_suite(Config0) ->
    Config1 = ssl_test_lib:init_per_suite(Config0, openssl),
    Config2 = check_openssl_sni_support(Config1),
    Config = ssl_test_lib:make_rsa_cert(Config2),
    Hostname = net_adm:localhost(),
    {#{server_config := ServerConf,
       client_config := ClientConf},
     #{server_config := LServerConf,
       client_config := LClientConf}} = ssl_test_lib:make_rsa_sni_configs(),
    [{client_opts, ClientConf}, {client_local_opts, LClientConf},
     {sni_server_opts, [{sni_hosts, [{Hostname, ServerConf}]} | LServerConf]} | Config].


end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(_TestCase, Config) -> 
    ct:timetrap({seconds, 10}),
    Config.


end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
sni_no_header(Config) when is_list(Config) ->
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    ServerOptions = ssl_test_lib:ssl_options(proplists:get_value(sni_server_opts, Config), Config),
    ClientOptions = ssl_test_lib:ssl_options([{server_name_indication, disable} |
                                              proplists:get_value(client_local_opts, Config)], Config),
    case proplists:get_value(openssl_disable, Config) of
        true ->
            sni_test(ServerNode, ServerOptions, ClientOptions, Config);
        false ->
            {skip, no_openssl_sni_disable_flag}
    end.

sni_no_header_fun(Config) when is_list(Config) ->
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    [{sni_hosts, ServerSNIConf}| DefaultConf] = proplists:get_value(sni_server_opts, Config),
    SNIFun = fun(Domain) -> proplists:get_value(Domain, ServerSNIConf, []) end,
    ServerOptions =  ssl_test_lib:ssl_options(DefaultConf, Config) ++ [{sni_fun, SNIFun}],
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, disable} |
                                               proplists:get_value(client_local_opts, Config)], Config),
    case proplists:get_value(openssl_disable, Config) of
        true ->
            sni_test(ServerNode, ServerOptions, ClientOptions, Config);
        false ->
            {skip, no_openssl_sni_disable_flag}
    end.

sni_match(Config) when is_list(Config) ->
    {_, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    ServerOptions = ssl_test_lib:ssl_options(proplists:get_value(sni_server_opts, Config), Config),
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, HostName} |
                                               proplists:get_value(client_opts, Config)], Config),
    sni_test(ServerNode, ServerOptions, ClientOptions, Config).

sni_match_fun(Config) when is_list(Config) ->
    {_, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    [{sni_hosts, ServerSNIConf}| DefaultConf] = proplists:get_value(sni_server_opts, Config),
    SNIFun = fun(Domain) -> proplists:get_value(Domain, ServerSNIConf, undefined) end,
    ServerOptions =  ssl_test_lib:ssl_options(DefaultConf, Config) ++ [{sni_fun, SNIFun}],
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, HostName} |
                                               proplists:get_value(client_opts, Config)], Config),
    sni_test(ServerNode, ServerOptions, ClientOptions, Config).

sni_no_match(Config) when is_list(Config) ->
    {_, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    [_ | DefaultConf] = proplists:get_value(sni_server_opts, Config),
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, HostName} |
                                               proplists:get_value(client_opts, Config)], Config),
    ServerOptions =  ssl_test_lib:ssl_options(DefaultConf, Config),
    sni_test(ServerNode, ServerOptions, ClientOptions, Config).

sni_no_match_fun(Config) when is_list(Config) ->
    {_, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    [{sni_hosts, _}| DefaultConf] = proplists:get_value(sni_server_opts, Config),
    ServerOptions =  ssl_test_lib:ssl_options(DefaultConf, Config),
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, HostName} |
                                               proplists:get_value(client_local_opts, Config)], Config),
    sni_test(ServerNode, ServerOptions, ClientOptions, Config).


%%--------------------------------------------------------------------
%% Callback functions  ------------------------------------------------
%%--------------------------------------------------------------------
send_and_hostname(SSLSocket) ->
    ssl:send(SSLSocket, "OK"),
    case ssl:connection_information(SSLSocket, [sni_hostname]) of
	{ok, []} ->
	    undefined;
	{ok, [{sni_hostname, Hostname}]} ->
	    Hostname
    end.
%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------
sni_test(ServerNode, ServerOptions0, ClientOptions, Config) ->
    %% Test Erlang server SNI handling with OpenSSL client
    Version = ssl_test_lib:protocol_version(Config),
    ServerOptions = maybe_add_sigalgs(Version, ServerOptions0),


    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {?MODULE, send_and_hostname, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),

    SNIHostname = case proplists:get_value(server_name_indication, ClientOptions) of
                      disable ->
                          undefined;
                      Name ->
                          Name
                  end,

    Sigalgs = maybe_add_openssl_sigalgs(Version),
    ClientPort = ssl_test_lib:start_client(openssl, 
                                           Sigalgs ++ [{port, Port}, 
                                                       {server_name, SNIHostname} | 
                                                                ClientOptions], 
                                           Config),
  
    ssl_test_lib:check_result(Server, SNIHostname),
    ssl_test_lib:close_port(ClientPort),
    ssl_test_lib:close(Server),
    ok.

maybe_add_sigalgs(Version, ServerOptions) when Version == 'tlsv1.3';
                                               Version == 'tlsv1.2' ->
    case maybe_add_openssl_sigalgs(Version) of
        [] ->
            [{signature_algs, [rsa_pss_rsae_sha512,
                               rsa_pss_rsae_sha384,
                               rsa_pss_rsae_sha256,
                               {sha512, rsa},
                               {sha384, rsa},
                               {sha256, rsa},
                               {sha224, rsa},
                               {sha, rsa}
                              ]
             } | ServerOptions];
        _ ->
            [{signature_algs, [rsa_pss_rsae_sha512,
                               rsa_pss_rsae_sha384,
                               rsa_pss_rsae_sha256]} | ServerOptions]
    end;
maybe_add_sigalgs(_, ServerOptions) ->
    ServerOptions.

maybe_add_openssl_sigalgs(Version) when Version == 'tlsv1.3';
                                        Version == 'tlsv1.2' ->
    case ssl_test_lib:portable_cmd("openssl", ["version"]) of
        "OpenSSL 1.0" ++  _ ->
            [];
        _ ->
            HelpText = ssl_test_lib:portable_cmd("openssl", ["s_client", "--help"]),
            case string:str(HelpText, "-sigalgs") of
                0 ->
                    [];
                _ ->
                    [{sigalgs, "rsa_pss_rsae_sha512:rsa_pss_rsae_sha384:rsa_pss_rsae_sha256"}]
            end
    end;
maybe_add_openssl_sigalgs(_) ->
    [].

check_openssl_sni_support(Config) ->
    HelpText = ssl_test_lib:portable_cmd("openssl", ["s_client", "--help"]),
    case string:str(HelpText, "-servername") of
        0 ->
            throw({skip, "Current openssl doesn't support SNI"});
        _ ->
            case string:str(HelpText, "-noservername") of
                0 ->
                    [{openssl_disable, false} | Config];
                _ ->
                    [{openssl_disable, true} | Config]
            end
    end.
