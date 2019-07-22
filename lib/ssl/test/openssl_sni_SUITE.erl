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

-module(openssl_sni_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

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
            [{group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'}
             %% Seems broken in openssl 
             %%{group, 'dtlsv1.2'},
             %%{group, 'dtlsv1'}
            ];
        false ->
            [{group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'}]
    end.

groups() ->
     case ssl_test_lib:openssl_sane_dtls() of 
         true ->
             [{'tlsv1.2', [], sni_tests()},
              {'tlsv1.1', [], sni_tests()},
              {'tlsv1', [], sni_tests()}
              %% Seems broken in openssl 
              %%{'dtlsv1.2', [], sni_tests()},
              %%{'dtlsv1', [], sni_tests()}
             ];
        false ->
             [{'tlsv1.2', [], sni_tests()},
              {'tlsv1.1', [], sni_tests()},
              {'tlsv1', [], sni_tests()}
             ]
     end.
 
sni_tests() ->
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
            case check_openssl_sni_support(Config0) of
                {skip, _} = Skip ->
                    Skip;
                _ ->
                    ct:pal("Version: ~p", [os:cmd("openssl version")]),
                    catch crypto:stop(),
                    try crypto:start() of
                        ok ->
                            ssl_test_lib:clean_start(),
                            Config = ssl_test_lib:make_rsa_cert(Config0),
                            RsaOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
                            [{sni_server_opts, [{sni_hosts,
                                  [{"a.server", [
                                                 {certfile, proplists:get_value(certfile, RsaOpts)},
                                                 {keyfile,  proplists:get_value(keyfile, RsaOpts)}
                                                ]},
                                   {"b.server", [
                                                 {certfile, proplists:get_value(certfile, RsaOpts)},
                                                 {keyfile, proplists:get_value(keyfile, RsaOpts)}
                                                ]}
                                  ]}]} | Config]
                    catch _:_  ->
                            {skip, "Crypto did not start"}
                    end
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

init_per_testcase(_TestCase, Config) -> 
    ct:timetrap({seconds, 10}),
    Config.


end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
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


erlang_server_openssl_client_sni_test(Config, SNIHostname, ExpectedSNIHostname, ExpectedCN) ->
    Version = ssl_test_lib:protocol_version(Config),
    ct:log("Start running handshake, Config: ~p, SNIHostname: ~p, ExpectedSNIHostname: ~p, ExpectedCN: ~p", 
           [Config, SNIHostname, ExpectedSNIHostname, ExpectedCN]),
    ServerOptions = proplists:get_value(sni_server_opts, Config) ++ proplists:get_value(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {?MODULE, send_and_hostname, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Exe = "openssl",
    ClientArgs = case SNIHostname of
		     undefined ->
			 openssl_client_args(Version, Hostname,Port);
		     _ ->
			 openssl_client_args(Version, Hostname, Port, SNIHostname)
		 end,       
    ClientPort = ssl_test_lib:portable_open_port(Exe, ClientArgs),  
  
    ssl_test_lib:check_result(Server, ExpectedSNIHostname),
    ssl_test_lib:close_port(ClientPort),
    ssl_test_lib:close(Server),
    ok.


erlang_server_openssl_client_sni_test_sni_fun(Config, SNIHostname, ExpectedSNIHostname, ExpectedCN) ->
    Version = ssl_test_lib:protocol_version(Config),
    ct:log("Start running handshake for sni_fun, Config: ~p, SNIHostname: ~p, ExpectedSNIHostname: ~p, ExpectedCN: ~p",
           [Config, SNIHostname, ExpectedSNIHostname, ExpectedCN]),
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
			 openssl_client_args(Version, Hostname,Port);
		     _ ->
			 openssl_client_args(Version, Hostname, Port, SNIHostname)
		 end,       

    ClientPort = ssl_test_lib:portable_open_port(Exe, ClientArgs), 
    
    ssl_test_lib:check_result(Server, ExpectedSNIHostname),
    ssl_test_lib:close_port(ClientPort),
    ssl_test_lib:close(Server).

send_and_hostname(SSLSocket) ->
    ssl:send(SSLSocket, "OK"),
    case ssl:connection_information(SSLSocket, [sni_hostname]) of
	{ok, []} ->
	    undefined;
	{ok, [{sni_hostname, Hostname}]} ->
	    Hostname
    end.

openssl_client_args(Version, Hostname, Port) ->
    ["s_client", "-connect", Hostname ++ ":" ++ integer_to_list(Port), ssl_test_lib:version_flag(Version)].

openssl_client_args(Version, Hostname, Port, ServerName) ->
    ["s_client",  "-connect", Hostname ++ ":" ++ 
	 integer_to_list(Port), ssl_test_lib:version_flag(Version), "-servername", ServerName].

check_openssl_sni_support(Config) ->
    HelpText = os:cmd("openssl s_client --help"),
    case ssl_test_lib:is_sane_oppenssl_client() of
        true ->
            case string:str(HelpText, "-servername") of
                0 ->
                    {skip, "Current openssl doesn't support SNI"};
                _ ->
                    Config
            end;
        false ->
            {skip, "Current openssl doesn't support SNI or extension handling is flawed"}
    end.
