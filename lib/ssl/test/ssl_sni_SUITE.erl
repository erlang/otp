%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2023. All Rights Reserved.
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

-module(ssl_sni_SUITE).

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/inet.hrl").

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
-export([no_sni_header/1,
         sni_match/1,
         sni_no_match/1,
         no_sni_header_fun/1,
         sni_match_fun/1,
         sni_no_match_fun/1,
         dns_name/1,
         ip_fallback/1,
         no_ip_fallback/1,
         dns_name_reuse/1,
         customize_hostname_check/0,
         customize_hostname_check/1,
         sni_no_trailing_dot/0,
         sni_no_trailing_dot/1,
         hostname_trailing_dot/0,
         hostname_trailing_dot/1
        ]).

%% Apply export
-export([send_and_hostname/1,
         recv_and_certificate/1
        ]).

-define(TIMEOUT, {seconds, 6}).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [{group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.3', [], sni_tests()},
     {'tlsv1.2', [], sni_tests()},
     {'tlsv1.1', [], sni_tests()},
     {'tlsv1', [], sni_tests()},
     {'dtlsv1.2', [], sni_tests()},
     {'dtlsv1', [], sni_tests()}
    ].

sni_tests() ->
    [no_sni_header, 
     sni_match, 
     sni_no_match,
     no_sni_header_fun, 
     sni_match_fun, 
     sni_no_match_fun,
     dns_name,
     ip_fallback,
     no_ip_fallback,
     dns_name_reuse,
     customize_hostname_check,
     sni_no_trailing_dot,
     hostname_trailing_dot].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
        ok ->
            ssl_test_lib:clean_start(),
            Hostname = net_adm:localhost(),
            {#{server_config := ServerConf,
               client_config := ClientConf},
             #{server_config := LServerConf,
               client_config := LClientConf}} = ssl_test_lib:make_rsa_sni_configs(),
            %% RSA certs files needed by *dot cases
            ssl_test_lib:make_rsa_cert([{client_opts, [{verify, verify_peer} | ClientConf]},
                                        {client_local_opts, [{verify, verify_peer} | LClientConf]},
                                        {sni_server_opts, [{sni_hosts, [{Hostname, ServerConf}]} | LServerConf]}
                                       | Config0])
    catch _:_  ->
            {skip, "Crypto did not start"}
    end.
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config).

end_per_group(GroupName, Config) ->
   ssl_test_lib:end_per_group(GroupName, Config).

end_per_suite(_) ->
    ssl:stop(),
    application:stop(crypto).

init_per_testcase(customize_hostname_check, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ssl_test_lib:clean_start(keep_version),
    ct:timetrap(?TIMEOUT),
    Config;
init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    Version = proplists:get_value(version, Config),
    ?CT_LOG("Ciphers: ~p~n ", [ ssl:cipher_suites(default, Version)]),
    ct:timetrap(?TIMEOUT),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
no_sni_header(Config) ->
    {ClientNode, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    ServerOptions = ssl_test_lib:ssl_options(proplists:get_value(sni_server_opts, Config), Config),
    ClientOptions = ssl_test_lib:ssl_options([{server_name_indication, disable} |
                                              proplists:get_value(client_local_opts, Config)], Config),
    basic_sni_test(ServerNode, ServerOptions, ClientNode, ClientOptions, HostName, undefined).

no_sni_header_fun(Config) ->
    {ClientNode, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    [{sni_hosts, ServerSNIConf}| DefaultConf] = proplists:get_value(sni_server_opts, Config),
    SNIFun = fun(Domain) -> proplists:get_value(Domain, ServerSNIConf, []) end,
    ServerOptions =  ssl_test_lib:ssl_options(DefaultConf, Config) ++ [{sni_fun, SNIFun}],
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, disable} |
                                               proplists:get_value(client_local_opts, Config)], Config),
    basic_sni_test(ServerNode, ServerOptions, ClientNode, ClientOptions, HostName, undefined).

sni_match(Config) ->
    {ClientNode, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    ServerOptions = ssl_test_lib:ssl_options(proplists:get_value(sni_server_opts, Config), Config),
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, HostName} |
                                               proplists:get_value(client_opts, Config)], Config),
    basic_sni_test(ServerNode, ServerOptions, ClientNode, ClientOptions, HostName, HostName).

sni_match_fun(Config) ->
    {ClientNode, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    [{sni_hosts, ServerSNIConf}| DefaultConf] = proplists:get_value(sni_server_opts, Config),
    SNIFun = fun(Domain) -> proplists:get_value(Domain, ServerSNIConf, undefined) end,
    ServerOptions =  ssl_test_lib:ssl_options(DefaultConf, Config) ++ [{sni_fun, SNIFun}],
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, HostName} |
                                               proplists:get_value(client_opts, Config)], Config),
    basic_sni_test(ServerNode, ServerOptions, ClientNode, ClientOptions, HostName, HostName).

sni_no_match(Config) ->
   {ClientNode, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    [_ | DefaultConf] = proplists:get_value(sni_server_opts, Config),
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, HostName} |
                                               proplists:get_value(client_opts, Config)], Config),
    ServerOptions =  ssl_test_lib:ssl_options(DefaultConf, Config),
    basic_sni_alert_test(ServerNode, ServerOptions, ClientNode, ClientOptions, HostName).

sni_no_match_fun(Config) ->
    {ClientNode, ServerNode, HostName} = ssl_test_lib:run_where(Config),
    [{sni_hosts, _}| DefaultConf] = proplists:get_value(sni_server_opts, Config),
    ServerOptions =  ssl_test_lib:ssl_options(DefaultConf, Config),
    ClientOptions =  ssl_test_lib:ssl_options([{server_name_indication, HostName} |
                                               proplists:get_value(client_local_opts, Config)], Config),
    basic_sni_alert_test(ServerNode, ServerOptions, ClientNode, ClientOptions, HostName).

dns_name(Config) ->
    Hostname = "OTP.test.server",
    #{server_config := ServerOpts0,
      client_config := ClientOpts0} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer => [{extensions,  [#'Extension'{extnID =
                                                                                   ?'id-ce-subjectAltName',
                                                                               extnValue = [{dNSName, Hostname}],
                                                                               critical = false}]},
                                                   {key, ssl_test_lib:hardcode_rsa_key(3)}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(4)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(6)}]}}),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerConf = ssl_test_lib:sig_algs(rsa, Version) ++  ServerOpts0,
    ClientConf = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,
    unsuccessfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], undefined, Config),
    successfull_connect(ServerConf, [{verify, verify_peer},
                                     {server_name_indication, Hostname} | ClientConf], undefined, Config),
    unsuccessfull_connect(ServerConf, [{verify, verify_peer}, {server_name_indication, "foo"} | ClientConf],
                          undefined, Config),
    successfull_connect(ServerConf, [{verify, verify_peer}, {server_name_indication, disable} | ClientConf],
                        undefined, Config).

ip_fallback(Config) ->
    Hostname = net_adm:localhost(),
    {ok, #hostent{h_addr_list = [IP |_]}} = inet:gethostbyname(net_adm:localhost()),
    IPList = tuple_to_list(IP),
    IPStr  = lists:flatten(integer_to_list(hd(IPList)) ++ [io_lib:format(".~w", [I]) || I <- tl(IPList)]),
    #{server_config := ServerOpts0,
      client_config := ClientOpts0} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer => [{extensions, [#'Extension'{extnID =
                                                                                  ?'id-ce-subjectAltName',
                                                                              extnValue = [{dNSName, Hostname},
                                                                                           {iPAddress, IPList}],
                                                                              critical = false}]},
                                                   {key, ssl_test_lib:hardcode_rsa_key(3)}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(4)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(6)}]}}),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerConf = ssl_test_lib:sig_algs(rsa, Version) ++  ServerOpts0,
    ClientConf = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,
    successfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], Hostname, Config),
    successfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], IP, Config),
    successfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], IPStr, Config),
    successfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], list_to_atom(Hostname), Config).

no_ip_fallback(Config) ->
    Hostname = net_adm:localhost(),
    {ok, #hostent{h_addr_list = [IP |_]}} = inet:gethostbyname(net_adm:localhost()),
    IPList = tuple_to_list(IP),
    IPStr  = lists:flatten(integer_to_list(hd(IPList)) ++ [io_lib:format(".~w", [I]) || I <- tl(IPList)]),
    #{server_config := ServerOpts0,
      client_config := ClientOpts0} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer => [{extensions, [#'Extension'{extnID =
                                                                                  ?'id-ce-subjectAltName',
                                                                              extnValue = [{dNSName, Hostname}],
                                                                              critical = false}]},
                                                   {key, ssl_test_lib:hardcode_rsa_key(3)}
                                                  ]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(4)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(6)}]}}),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerConf = ssl_test_lib:sig_algs(rsa, Version) ++  ServerOpts0,
    ClientConf = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,
    successfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], Hostname, Config),
    unsuccessfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], IP, Config),
    unsuccessfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], IPStr, Config).

dns_name_reuse(Config) ->
    SNIHostname = "OTP.test.server",
    #{server_config := ServerOpts0,
      client_config := ClientOpts0} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer => [{extensions,  [#'Extension'{extnID =
                                                                                   ?'id-ce-subjectAltName',
                                                                               extnValue = [{dNSName, SNIHostname}],
                                                                               critical = false}
                                                                 ]},
                                                   {key, ssl_test_lib:hardcode_rsa_key(3)}
                                                  ]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(4)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(6)}]}}),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerConf = ssl_test_lib:sig_algs(rsa, Version) ++  ServerOpts0,
    ClientConf = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    unsuccessfull_connect(ServerConf, [{verify, verify_peer} | ClientConf], undefined, Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, ServerConf}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
                                   {from, self()}, {options, [{verify, verify_peer},
                                                              {server_name_indication, SNIHostname} | ClientConf]}]),
    receive
        {Server, _} ->
            ok
    end,

    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},

    %% Make sure session is registered
    ct:sleep(1000),

    Client1 =
	ssl_test_lib:start_client_error([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_info_result, []}},
                                         {from, self()},  {options, [{verify, verify_peer} | ClientConf]}]),

    ssl_test_lib:check_client_alert(Client1, handshake_failure),
    ssl_test_lib:close(Client0).


customize_hostname_check() ->
    [{doc,"Test option customize_hostname_check."}].
customize_hostname_check(Config) when is_list(Config) ->
    Ext = [#'Extension'{extnID = ?'id-ce-subjectAltName',
                        extnValue = [{dNSName, "*.example.org"}],
                        critical = false}
          ],
    #{server_config := ServerOpts0,
      client_config := ClientOpts0} = ssl_test_lib:make_cert_chains_pem(rsa, [{server_chain,
                                                                               [[], 
                                                                                [],
                                                                                [{extensions, Ext}]
                                                                               ]}], 
                                                                        Config, "https_hostname_convention"),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),  
                                        
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
  
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {host, Hostname},
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    CustomFun = public_key:pkix_verify_hostname_match_fun(https),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
                                        {host, Hostname},
					 {from, self()}, 
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options,
                                         [{verify, verify_peer},
                                          {server_name_indication, "other.example.org"},
                                          {customize_hostname_check, 
                                           [{match_fun, CustomFun}]} | ClientOpts]
					 }]),    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
                                        
    Client1 = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
                                               {host, Hostname},
                                               {from, self()}, 
                                               {mfa, {ssl_test_lib, no_result, []}},
                                               {options, [{verify, verify_peer},
                                                          {server_name_indication, "other.example.org"} | ClientOpts]}
                                              ]),    
    ssl_test_lib:check_client_alert(Server, Client1, handshake_failure).

sni_no_trailing_dot() ->
      [{doc,"Test that sni may not include a triling dot"}].
sni_no_trailing_dot(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(sni_server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
                                 {from, self()},
                                 {mfa, {ssl_test_lib, no_result, []}},
                                 {options, [ServerOpts]}]),

    Port  = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
                                              {mfa, {ssl_test_lib, no_result, []}},
					      {options,
                                               [{server_name_indication, Hostname ++ "."} | ClientOpts]}]),
    ssl_test_lib:check_server_alert(Server, Client, unrecognized_name).

hostname_trailing_dot() ->
    [{doc,"Test that fallback sni removes trailing dot of hostname"}].

hostname_trailing_dot(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(sni_server_opts, Config),
    {ClientNode, ServerNode, Hostname0} = ssl_test_lib:run_where(Config),

    case trailing_dot_hostname(Hostname0) of
        {ok, Hostname} ->
            Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
                                                {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                                {options, ServerOpts}]),
            Port = ssl_test_lib:inet_port(Server),
            Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
                                                {host, Hostname},
                                                {from, self()},
                                                {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, ClientOpts}]),

            ssl_test_lib:check_result(Server, ok, Client, ok),

            ssl_test_lib:close(Server),
            ssl_test_lib:close(Client);
        {skip, _ } = Skip ->
            Skip
    end.

%%--------------------------------------------------------------------
%% Callback Functions ------------------------------------------------
%%--------------------------------------------------------------------
send_and_hostname(SSLSocket) ->
    ssl:send(SSLSocket, "OK"),
    case  ssl:connection_information(SSLSocket, [sni_hostname]) of
	{ok, [{sni_hostname, Hostname}]} ->
	    Hostname;
	{ok, []} ->
	    undefined
    end.

recv_and_certificate(SSLSocket) ->
    ssl_recv(SSLSocket, "OK"),
    {ok, PeerCert} = ssl:peercert(SSLSocket),
    #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{subject = {rdnSequence, Subject}}} 
	= public_key:pkix_decode_cert(PeerCert, otp),
    ?CT_LOG("Subject of certificate received from server: ~p", [Subject]),
    rdn_to_string(rdnPart(Subject, ?'id-at-commonName')).

%%--------------------------------------------------------------------
%% Internal Functions ------------------------------------------------
%%--------------------------------------------------------------------
basic_sni_test(ServerNode, ServerOptions, ClientNode, ClientOptions, HostName, Expected) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {?MODULE, send_and_hostname, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, HostName}, {from, self()},
                                        {mfa, {?MODULE, recv_and_certificate, []}},
                                        {options, [{verify, verify_peer} | ClientOptions]}]),
    ssl_test_lib:check_result(Server, Expected, Client,  "server Peer cert"),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

basic_sni_alert_test(ServerNode, ServerOptions, ClientNode, ClientOptions, HostName) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {ssl_test_lib, no_result, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                              {host, HostName}, {from, self()},
                                              {options, [{verify, verify_peer} | ClientOptions]}]),
    ssl_test_lib:check_client_alert(Client, handshake_failure),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

successfull_connect(ServerOptions, ClientOptions, Hostname0, Config) ->  
    {ClientNode, ServerNode, Hostname1} = ssl_test_lib:run_where(Config),
    Hostname = host_name(Hostname0, Hostname1),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname}, {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, ClientOptions}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

unsuccessfull_connect(ServerOptions, ClientOptions, Hostname0, Config) ->
    {ClientNode, ServerNode, Hostname1} = ssl_test_lib:run_where(Config),
    Hostname = host_name(Hostname0, Hostname1),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()}, 
					      {options, ClientOptions}]),
    
    ssl_test_lib:check_server_alert(Server, Client, handshake_failure).

host_name(undefined, Hostname) ->
    Hostname;
host_name(Hostname, _) ->
    Hostname.

trailing_dot_hostname(HostName) ->
    case lists:member($., HostName) of
        true ->
            case lists:last(HostName) =/= $. of
                true ->
                    {ok, HostName ++ "."};
                false ->
                    {ok, HostName}
            end;
        _ ->
            {skip, "Trailing dot conf not possible"}
     end.

ssl_recv(SSLSocket, Expect) ->
    ssl_recv(SSLSocket, "", Expect).

ssl_recv(SSLSocket, CurrentData, ExpectedData) ->
    receive
        {ssl, SSLSocket, Data} ->
            NeweData = CurrentData ++ Data,
            case NeweData of
                ExpectedData ->
                    ok;
                _  ->
                    ssl_recv(SSLSocket, NeweData, ExpectedData)
            end;
        Other ->
            ct:fail({unexpected_message, Other})
    after 4000 ->
            ct:fail({timeout, CurrentData, ExpectedData})
    end.

rdnPart([[#'AttributeTypeAndValue'{type=Type, value=Value} | _] | _], Type) ->
    Value;
rdnPart([_ | Tail], Type) ->
    rdnPart(Tail, Type);
rdnPart([], _) ->
    unknown.

rdn_to_string({utf8String, Binary}) ->
    erlang:binary_to_list(Binary);
rdn_to_string({printableString, String}) ->
    String.
