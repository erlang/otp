%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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

-module(ssl_basic_SUITE).

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("ssl/src/ssl_api.hrl").

%% Callback functions
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Testcases
-export([app/0,
         app/1,
         appup/0,
         appup/1,
         version_option/0,
         version_option/1,
         connect_twice/0,
         connect_twice/1,
         connect_dist/0,
         connect_dist/1,
         defaults/1,
         fallback/0,
         fallback/1,
         cipher_format/0,
         cipher_format/1,
         tls_versions_option/0,
         tls_versions_option/1,
         spawn_opts_option/0,
         spawn_opts_option/1,
         eccs/0,
         eccs/1,
         cipher_suites/0,
         cipher_suites/1,
         cipher_suites_mix/0,
         cipher_suites_mix/1,
         unordered_protocol_versions_server/0,
         unordered_protocol_versions_server/1,
         unordered_protocol_versions_client/0,
         unordered_protocol_versions_client/1,
         fake_root/0,
         fake_root/1,
         fake_root_legacy/0,
         fake_root_legacy/1,
         fake_root_no_intermediate/0,
         fake_root_no_intermediate/1,
         fake_root_no_intermediate_legacy/0,
         fake_root_no_intermediate_legacy/1,
         fake_intermediate_cert/0,
         fake_intermediate_cert/1,
         incomplete_chain_length/0,
         incomplete_chain_length/1,
         user_dies/0,
         user_dies/1
        ]).

%% Apply export
-export([tcp_send_recv_result/1,
         result_ok/1,
         protocol_info_result/1,
         version_info_result/1,
         min_heap_size_info/1,
         connect_dist_s/1,
         connect_dist_c/1,
         dummy/1,
         many_client_starter/4
        ]).

-define(TIMEOUT, 20000).
-define(EXPIRE, 10).
-define(SLEEP, 500).
-define(RENEGOTIATION_DISABLE_TIME, 12000).
-define(CLEAN_SESSION_DB, 60000).
-define(SEC_RENEGOTIATION_TIMEOUT, 30).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     {group, basic},
     {group, options}
    ].

groups() ->
    [{basic, [], basic_tests()},
     {options, [], options_tests()}
    ].

basic_tests() ->
    [app,
     appup,
     version_option,
     connect_twice,
     connect_dist,
     defaults,
     fallback,
     cipher_format,
     tls_versions_option,
     spawn_opts_option,
     eccs,
     cipher_suites,
     cipher_suites_mix,
     fake_root,
     fake_root_no_intermediate,
     fake_root_legacy,
     fake_root_no_intermediate_legacy,
     fake_intermediate_cert,
     incomplete_chain_length,
     user_dies
    ].

options_tests() ->
    [
     unordered_protocol_versions_server,
     unordered_protocol_versions_client].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
            ssl_test_lib:make_rsa_cert(Config0)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_testcase(eccs, Config) ->
    case ssl:eccs() of
        [] ->
            {skip, "named curves not supported"};
        [_|_] ->
            ssl_test_lib:ct_log_supported_protocol_versions(Config),
            ct:timetrap({seconds, 5}),
            Config
    end;
init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 5}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.
%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
app() ->
    [{doc, "Test that the ssl app file is ok"}].
app(Config) when is_list(Config) ->
    ok = test_server:app_test(ssl).
%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the ssl appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(ssl).
%%--------------------------------------------------------------------
version_option() ->
    [{doc, "Use version option and do no specify ciphers list. Bug specified incorrect ciphers"}].
version_option(Config) when is_list(Config) ->
    Versions = proplists:get_value(supported, ssl:versions()),
    [version_option_test(Config, Version) || Version <- Versions].

%%--------------------------------------------------------------------
connect_twice() ->
    [{doc,""}].
connect_twice(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{keepalive, true},{active, false}
					       | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{keepalive, true},{active, false}
					      | ClientOpts]}]),
    Server ! listen,

    {Client1, #sslsocket{}} =
	ssl_test_lib:start_client([return_socket,
				   {node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{keepalive, true},{active, false}
					      | ClientOpts]}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:check_result(Server, ok, Client1, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ssl_test_lib:close(Client1).
defaults(Config) when is_list(Config)->
    Versions = ssl:versions(),
    false = lists:member(sslv3, proplists:get_value(available, Versions)),
    false = lists:member(sslv3,  proplists:get_value(supported, Versions)),
    true = lists:member('tlsv1', proplists:get_value(available, Versions)),
    false = lists:member('tlsv1',  proplists:get_value(supported, Versions)),
    true = lists:member('tlsv1.1', proplists:get_value(available, Versions)),
    false = lists:member('tlsv1.1',  proplists:get_value(supported, Versions)),
    true = lists:member('tlsv1.2', proplists:get_value(available, Versions)),
    true = lists:member('tlsv1.2',  proplists:get_value(supported, Versions)),
    true = lists:member('dtlsv1.2', proplists:get_value(available_dtls, Versions)),
    true = lists:member('dtlsv1', proplists:get_value(available_dtls, Versions)),
    true = lists:member('dtlsv1.2', proplists:get_value(supported_dtls, Versions)),
    false = lists:member('dtlsv1', proplists:get_value(supported_dtls, Versions)).


fallback() ->
    [{doc, "Test TLS_FALLBACK_SCSV downgrade prevention"}].

fallback(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					 {from, self()},
					 {options, ServerOpts}]),

    Port = ssl_test_lib:inet_port(Server),

    Client =
        ssl_test_lib:start_client_error([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {from, self()},  {options,
                                                           [{fallback, true},
                                                            {versions, ['tlsv1']}
                                                            | ClientOpts]}]),
    ssl_test_lib:check_server_alert(Server, Client, inappropriate_fallback).

%%--------------------------------------------------------------------
cipher_format() ->
    [{doc, "Test that cipher conversion from maps | tuples | strings to binarys works"}].
cipher_format(Config) when is_list(Config) ->
    {ok, Socket0} = ssl:listen(0, [{ciphers, ssl:cipher_suites(default, 'tlsv1.2')}]),
    ssl:close(Socket0).

%%--------------------------------------------------------------------

cipher_suites() ->
    [{doc,"Test API function cipher_suites/2, filter_cipher_suites/2"
      " and prepend|append_cipher_suites/2"}].

cipher_suites(Config) when is_list(Config) ->
    chipher_suite_checks('tlsv1.3'),
    chipher_suite_checks('tlsv1.2'),
    chipher_suite_checks('tlsv1.1'),
    chipher_suite_checks('tlsv1'),
    chipher_suite_checks('dtlsv1.2'),
    chipher_suite_checks('dtlsv1'),
    anon_chipher_suite_checks('tlsv1.3'),
    anon_chipher_suite_checks('tlsv1.2'),
    anon_chipher_suite_checks('tlsv1.1'),
    anon_chipher_suite_checks('tlsv1'),
    anon_chipher_suite_checks('dtlsv1.2'),
    anon_chipher_suite_checks('dtlsv1').


%%--------------------------------------------------------------------
cipher_suites_mix() ->
    [{doc,"Test to have old and new cipher suites at the same time"}].

cipher_suites_mix(Config) when is_list(Config) ->
    CipherSuites = [{dhe_rsa,aes_128_cbc,sha256,sha256}, {dhe_rsa,aes_128_cbc,sha}],
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    ServerCipherSuites =  ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                                   [{key_exchange, fun(srp_rsa) -> false;
                                                                      (srp_dss) -> false;
                                                                      (_) -> true
                                                                   end}]),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{ciphers, ServerCipherSuites} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{versions, ['tlsv1.2']},{ciphers, CipherSuites} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
unordered_protocol_versions_server() ->
    [{doc,"Test that the highest protocol is selected even"
      " when it is not first in the versions list."}].

unordered_protocol_versions_server(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, protocol_info_result, []}},
					{options, [{versions, ['tlsv1.1', 'tlsv1.2']} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, protocol_info_result, []}},
					{options, ClientOpts}]),

    ServerMsg = ClientMsg = {ok,'tlsv1.2'},
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg).

%%--------------------------------------------------------------------
unordered_protocol_versions_client() ->
    [{doc,"Test that the highest protocol is selected even"
      " when it is not first in the versions list."}].

unordered_protocol_versions_client(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, protocol_info_result, []}},
					{options, ServerOpts }]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, protocol_info_result, []}},
					{options,  [{versions, ['tlsv1.1', 'tlsv1.2']} | ClientOpts]}]),

    ServerMsg = ClientMsg = {ok, 'tlsv1.2'},
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg).

connect_dist() ->
    [{doc,"Test a simple connect as is used by distribution"}].

connect_dist(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = [{active, false}, {packet,4}|ClientOpts0],
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{active, false}, {packet,4}|ServerOpts0],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, connect_dist_s, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, connect_dist_c, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
eccs() ->
    [{doc, "Test API functions eccs/0 and eccs/1"}].

eccs(Config) when is_list(Config) ->
    [_|_] = All = ssl:eccs(),
    [_|_] = Tls = ssl:eccs(tlsv1),
    [_|_] = Tls1 = ssl:eccs('tlsv1.1'),
    [_|_] = Tls2 = ssl:eccs('tlsv1.2'),
    [_|_] = Tls1 = ssl:eccs('dtlsv1'),
    [_|_] = Tls2 = ssl:eccs('dtlsv1.2'),
    ok.

tls_versions_option() ->
    [{doc,"Test API versions option to connect/listen."}].
tls_versions_option(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    Supported = proplists:get_value(supported, ssl:versions()),
    Available = proplists:get_value(available, ssl:versions()),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{versions, Supported} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    Server ! listen,
    Versions = remove_supported_versions(Available, Supported),
    ErrClient = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
						 {host, Hostname},
						 {from, self()},
						 {options, [{versions , Versions} | ClientOpts]}]),
    receive
	{Server, _} ->
	    ok
    end,
    ssl_test_lib:check_client_alert(ErrClient, protocol_version).

spawn_opts_option() ->
    [{doc,"Test API spawn_opts option to sender/receiver."}].
spawn_opts_option(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, min_heap_size_info, []}},
                    {options, [{sender_spawn_opts, [{min_heap_size, 1598}]},
                               {receiver_spawn_opts, [{min_heap_size, 2586}]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {?MODULE, min_heap_size_info, []}},
                    {options, [{sender_spawn_opts, [{min_heap_size, 1598}]},
                               {receiver_spawn_opts, [{min_heap_size, 2586}]} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, {ok, 2586, 1598}, Client, {ok, 2586, 1598}).

fake_root() ->
    [{doc,"Test that we can not use a fake root signed by other key but with correct name and serial number."}].
fake_root(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Ext = x509_test:extensions([{key_usage, [keyCertSign, cRLSign, digitalSignature, keyAgreement]}]),
    ROOT = #{cert := Cert,
             key := _Key} = public_key:pkix_test_root_cert("SERVER ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(6)},
                                                                              {digest, sha256},
                                                                              {extensions, Ext}]),
    FakeKey = ssl_test_lib:hardcode_rsa_key(1),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    FakeCert = public_key:pkix_sign(TBS, FakeKey),

    AuthExt = #'AuthorityKeyIdentifier'{authorityCertIssuer = [{directoryName, TBS#'OTPTBSCertificate'.issuer}],
                                        authorityCertSerialNumber = TBS#'OTPTBSCertificate'.serialNumber},
    [AuthKeyExt] = x509_test:extensions([{?'id-ce-authorityKeyIdentifier',
                                          AuthExt,
                                          false}]),
    #{server_config := ServerConf,
      client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => ROOT,
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)},
                                                             {digest, sha256},
                                                             {extensions, [AuthKeyExt]}]],
                                          peer =>  [{key, ssl_test_lib:hardcode_rsa_key(4)},
                                                    {digest, sha256}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}
                                 ),

    #{server_config := FakeServerConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root =>  #{cert => FakeCert, key => FakeKey},
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256},
                                                             {extensions, [AuthKeyExt]}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}],
                                          intermediates =>  [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                              {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}
                                 ),

    test_fake_root(Hostname, ServerNode, ClientNode, ServerConf,
                   ClientConf, FakeCert, FakeServerConf, bad_certificate, bad_certificate).

fake_root_no_intermediate() ->
    [{doc,"Test that we can not use a fake root signed by other key but with correct name and serial number."}].

fake_root_no_intermediate(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Ext = x509_test:extensions([{key_usage, [keyCertSign, cRLSign, digitalSignature, keyAgreement]}]),
    ROOT = #{cert := Cert,
             key := _Key} = public_key:pkix_test_root_cert("SERVER ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(6)},
                                                                              {digest, sha256},
                                                                              {extensions, Ext}]),

    FakeKey = ssl_test_lib:hardcode_rsa_key(1),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    FakeCert = public_key:pkix_sign(TBS, FakeKey),

    AuthExt = #'AuthorityKeyIdentifier'{authorityCertIssuer = [{directoryName, TBS#'OTPTBSCertificate'.issuer}],
                                        authorityCertSerialNumber = TBS#'OTPTBSCertificate'.serialNumber},
    [AuthKeyExt] = x509_test:extensions([{?'id-ce-authorityKeyIdentifier',
                                          AuthExt,
                                          false}]),
    #{server_config := ServerConf,
      client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => ROOT,
                                          intermediates => [],
                                          peer =>  [{key, ssl_test_lib:hardcode_rsa_key(4)},
                                                    {digest, sha256},
                                                    {extensions, [AuthKeyExt]}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}),

    #{server_config := FakeServerConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root =>  #{cert => FakeCert, key => FakeKey},
                                          intermediates => [],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256},
                                                   {extensions, [AuthKeyExt]}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}],
                                          intermediates =>  [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                              {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}),

    test_fake_root(Hostname, ServerNode, ClientNode, ServerConf,
                   ClientConf, FakeCert, FakeServerConf, bad_certificate, bad_certificate).

fake_root_legacy() ->
    [{doc,"Test that we can not use a fake root signed by other key but with correct name and serial number."}].
fake_root_legacy(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Ext = x509_test:extensions([{key_usage, [keyCertSign, cRLSign, digitalSignature, keyAgreement]}]),
    ROOT = #{cert := Cert,
             key := _Key} = public_key:pkix_test_root_cert("SERVER ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(6)},
                                                                              {digest, sha256},
                                                                              {extensions, Ext}]),
    FakeKey = ssl_test_lib:hardcode_rsa_key(1),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    FakeCert = public_key:pkix_sign(TBS, FakeKey),
    #{server_config := ServerConf,
      client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => ROOT,
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)},
                                                             {digest, sha256}]],
                                          peer =>  [{key, ssl_test_lib:hardcode_rsa_key(4)},
                                                    {digest, sha256}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}),
    #{server_config := FakeServerConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root =>  #{cert => FakeCert, key => FakeKey},
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256} ],
                                          intermediates =>  [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                              {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}
                                 ),
    test_fake_root(Hostname, ServerNode, ClientNode, ServerConf,
                   ClientConf, FakeCert, FakeServerConf, unknown_ca, unknown_ca).

fake_root_no_intermediate_legacy() ->
    [{doc,"Test that we can not use a fake root signed by other key but with correct name and serial number."}].
fake_root_no_intermediate_legacy(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Ext = x509_test:extensions([{key_usage, [keyCertSign, cRLSign, digitalSignature, keyAgreement]}]),
    ROOT = #{cert := Cert,
             key := _Key} = public_key:pkix_test_root_cert("SERVER ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(6)},
                                                                              {digest, sha256},
                                                                              {extensions, Ext}]),

    FakeKey = ssl_test_lib:hardcode_rsa_key(1),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    FakeCert = public_key:pkix_sign(TBS, FakeKey),

    #{server_config := ServerConf,
      client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => ROOT,
                                          intermediates => [],
                                          peer =>  [{key, ssl_test_lib:hardcode_rsa_key(4)},
                                                    {digest, sha256}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)}]}}
                                 ),
    #{server_config := FakeServerConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root =>  #{cert => FakeCert, key => FakeKey},
                                          intermediates => [],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}
                                                  ],
                                          intermediates =>  [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                              {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}),
    test_fake_root(Hostname, ServerNode, ClientNode, ServerConf,
                   ClientConf, FakeCert, FakeServerConf, unknown_ca, unknown_ca).

fake_intermediate_cert() ->
    [{doc,"Test that we can not use a fake intermediat cert claiming to be signed by a trusted ROOT but is not."}].

fake_intermediate_cert(Config) when is_list(Config) ->
     {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
     Ext = x509_test:extensions([{key_usage, [keyCertSign, cRLSign, digitalSignature, keyAgreement]}]),
     ROOT = #{cert := Cert,
              key := _Key} = public_key:pkix_test_root_cert("SERVER ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(6)},
                                                                               {digest, sha256},
                                                                               {extensions, Ext}]),
    OtherSROOT = #{cert := OtherSCert,
                   key := OtherSKey} =
        public_key:pkix_test_root_cert("OTHER SERVER ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                                {digest, sha256},
                                                                {extensions, Ext}]),
    OtherCROOT = #{cert := OtherCCert,
                   key := _OtherCKey} =
        public_key:pkix_test_root_cert("OTHER Client ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                                {digest, sha256},
                                                                {extensions, Ext}]),
    #{client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => ROOT,
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)},
                                                             {digest, sha256}]],
                                          peer =>  [{key, ssl_test_lib:hardcode_rsa_key(4)},
                                                    {digest, sha256}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}
                                 ),

    #{server_config := OtherServerConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => OtherSROOT,
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer =>  [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                    {digest, sha256}]},
                                    client_chain =>
                                        #{root => OtherCROOT,
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}
                                 ),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    TBSExt = TBS#'OTPTBSCertificate'.extensions,
    AuthExt = #'AuthorityKeyIdentifier'{authorityCertIssuer = [{directoryName, TBS#'OTPTBSCertificate'.issuer}],
                                        authorityCertSerialNumber = TBS#'OTPTBSCertificate'.serialNumber},
    [AuthKeyExt] = x509_test:extensions([{?'id-ce-authorityKeyIdentifier',
                                          AuthExt,
                                          false}]),

    CAs = proplists:get_value(cacerts, OtherServerConf),

    [ICA] = CAs -- [OtherSCert, OtherCCert],

    OTPICACert = public_key:pkix_decode_cert(ICA, otp),
    ICATBS = OTPICACert#'OTPCertificate'.tbsCertificate,

    FakeICA = public_key:pkix_sign(ICATBS#'OTPTBSCertificate'{extensions = [AuthKeyExt | TBSExt]}, OtherSKey),

    ServerCert = proplists:get_value(cert, OtherServerConf),
    FakeServer = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                            {from, self()},
                                            {mfa, {ssl_test_lib, no_result, []}},
                                            {options, [{cert, [ServerCert, FakeICA]} |
                                                       proplists:delete(cert, OtherServerConf)]
                                            }]),
    Port1 = ssl_test_lib:inet_port(FakeServer),
    Client1 = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port1},
                                               {host, Hostname},
                                               {from, self()},
                                               {options, [{verify, verify_peer} | ClientConf]}]),

    ssl_test_lib:check_client_alert(Client1, bad_certificate).

incomplete_chain_length() ->
    [{doc,"Test that attempts to reconstruct incomplete chains does not make shorter incomplete chains"}].
incomplete_chain_length(Config) when is_list(Config)->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Ext = x509_test:extensions([{key_usage, [keyCertSign, cRLSign, digitalSignature, keyAgreement]}]),
    ROOT = public_key:pkix_test_root_cert("SERVER ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(6)},
                                                             {digest, sha256},
                                                             {extensions, Ext}]),

    OtherROOT = public_key:pkix_test_root_cert("OTHER SERVER ROOT CA", [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                                                              {extensions, Ext}]),
    #{client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => ROOT,
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)},
                                                             {digest, sha256}]],
                                          peer =>  [{key, ssl_test_lib:hardcode_rsa_key(4)}, {digest, sha256}]},
                                    client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256} ],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}
                                 ),

    #{server_config := ServerConf} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => OtherROOT,
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256} ],
                                                            [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                             {digest, sha256} ]
                                                           ],
                                          peer =>  [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                    {digest, sha256}]},
                                                                 client_chain =>
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)},
                                                   {digest, sha256}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)},
                                                             {digest, sha256}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)},
                                                   {digest, sha256}]}}
                                 ),

    VerifyFun = {fun(_,{bad_cert, unknown_ca}, UserState) ->
                         %% accept this error to provoke the
                         %% building of an shorter incomplete chain
                         %% than the one received
                         {valid, UserState};
                    (_,{extension, _} = Extension, #{ext := N} = UserState) ->
                         ?CT_LOG("~p", [Extension]),
                         {unknown,  UserState#{ext => N +1}};
                    (_, valid, #{intermediates := N} = UserState) ->
                         {valid, UserState#{intermediates => N +1}};
                    (_, valid_peer, #{intermediates := 2,
                                      ext := 1} = UserState) ->
                         {valid, UserState};
                    (_, valid_peer, UserState) ->
                         ?CT_LOG("~p", [UserState]),
                         {error, {bad_cert, too_short_path}}
                 end, #{intermediates => 0,
                        ext => 0}},

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, ServerConf}
                                        ]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{verify, verify_peer}, {verify_fun, VerifyFun} | ClientConf]}]),
    ssl_test_lib:check_result(Client, ok, Server, ok).


user_dies() ->
    [{doc, "Test that we do not leak processess when user dies during startup of connection"}].
user_dies(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(ServerNode),
    Server = spawn_link(fun() ->
                                {ok, L} = ssl:listen(Port, ServerOpts),
                                loop(L)
                        end),
    do_wait_tls_server(Hostname, Port),
    {ok,_}  = ssl:connect(Hostname, Port, ClientOpts),
    check_process_count(2),
    Pid = spawn_link(fun() -> many_client_starter(Hostname, Port, ClientOpts, Server) end),
    receive
        {'EXIT', Pid, _} ->
            check_process_count(2)
    end.

%%--------------------------------------------------------------------
%% callback functions ------------------------------------------------
%%--------------------------------------------------------------------
tcp_send_recv_result(Socket) ->
    gen_tcp:send(Socket, "Hello world"),
    {ok,"Hello world"} = gen_tcp:recv(Socket, 11),
    ok.

result_ok(_Socket) ->
    ok.


protocol_info_result(Socket) ->
    {ok, [{protocol, PVersion}]} = ssl:connection_information(Socket, [protocol]),
    {ok, PVersion}.

version_info_result(Socket) ->
    {ok, [{version, Version}]} = ssl:connection_information(Socket, [version]),
    {ok, Version}.

min_heap_size_info(#sslsocket{pid = [Receiver, Sender]}) ->
    {garbage_collection, ReceiverGc} = process_info(Receiver, garbage_collection),
    {garbage_collection, SenderGc} = process_info(Sender, garbage_collection),
    {ok, proplists:get_value(min_heap_size, ReceiverGc), proplists:get_value(min_heap_size, SenderGc)}.

connect_dist_s(S) ->
    Msg = term_to_binary({erlang,term}),
    ok = ssl:send(S, Msg).

connect_dist_c(S) ->
    Test = binary_to_list(term_to_binary({erlang,term})),
    {ok, Test} = ssl:recv(S, 0, 10000),
    ok.

dummy(_Socket) ->
    %% Should not happen as the ssl connection will not be established
    %% due to fatal handshake failure
    exit(kill).
%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
version_option_test(Config, Version) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false}, {versions, [Version]}| ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false}, {versions, [Version]}| ClientOpts]}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

remove_supported_versions(Available, Supported) ->
    Versions0 = Available -- Supported,
    case lists:member('tlsv1.3', Versions0) andalso
        not lists:member('tlsv1.2', Versions0) of
        true ->
            %% If 'tlsv1.2' is removed, remove also 'tlsv1.3'
            Versions0 -- ['tlsv1.3'];
        _ ->
            Versions0
    end.


test_fake_root(Hostname, ServerNode, ClientNode, ServerConf, ClientConf, FakeCert, FakeServerConf,
               ResultRootIncluded, ResultRootExcluded) ->
    RealServer = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                            {from, self()},
                                            {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                             {options, ServerConf}]),
    Port0 = ssl_test_lib:inet_port(RealServer),
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port0},
                                         {host, Hostname},
                                          {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {from, self()},
                                         {options, [{verify, verify_peer} | ClientConf]}]),

    ssl_test_lib:check_result(RealServer, ok, Client0, ok),

    ssl_test_lib:close(RealServer),
    ssl_test_lib:close(Client0),

    %% Fake server sends ROOT cert
    FakeServer = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                            {from, self()},
                                            {mfa, {ssl_test_lib, no_result, []}},
                                            {options, FakeServerConf}]),
    Port1 = ssl_test_lib:inet_port(FakeServer),

    Client1 = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port1},
                                               {host, Hostname},
                                               {from, self()},
                                               {options, [{verify, verify_peer} | ClientConf]}]),

    ssl_test_lib:check_client_alert(Client1, ResultRootIncluded),

    %%Fake server does not send ROOT cert
    CAS0 = proplists:get_value(cacerts, FakeServerConf),
    CAS1 = CAS0 -- [FakeCert],

    FakeServer1 = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                             {from, self()},
                                             {mfa, {ssl_test_lib, no_result, []}},
                                             {options, [{cacerts, CAS1} | proplists:delete(cacerts, FakeServerConf)]}]),

    Port2 = ssl_test_lib:inet_port(FakeServer1),
    Client2 = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port2},
                                               {host, Hostname},
                                              {from, self()},
                                               {options, [{verify, verify_peer} | ClientConf]}]),
    ssl_test_lib:check_client_alert(Client2, ResultRootExcluded),
    ssl_test_lib:close(FakeServer1).

anon_chipher_suite_checks('tlsv1.3' = Version) ->
    [] = ssl:cipher_suites(anonymous, Version),
    [] = ssl:cipher_suites(exclusive_anonymous, Version);
anon_chipher_suite_checks(Version) ->
    [_|_] = ssl:cipher_suites(anonymous, Version),
    [_|_] = ssl:cipher_suites(exclusive_anonymous, Version).

chipher_suite_checks(Version) ->
    MandatoryCipherSuiteTLS1_0 = #{key_exchange => dhe_dss,
                                   cipher => '3des_ede_cbc',
                                   mac => sha,
                                   prf => default_prf},
    MandatoryCipherSuiteTLS1_1 = #{key_exchange => rsa,
                                   cipher => '3des_ede_cbc',
                                   mac => sha,
                                   prf => default_prf},
    MandatoryCipherSuiteTLS1_0TLS1_2 = #{key_exchange =>rsa,
                                         cipher => 'aes_128_cbc',
                                         mac => sha,
                                         prf => default_prf},
    All = [_|_] = ssl:cipher_suites(all, Version),
    Default = [_|_] = ssl:cipher_suites(default, Version),
    Anonymous = ssl:cipher_suites(anonymous, Version),
    true = length(Default) < length(All),

    Filters = [{key_exchange,
                fun(dhe_rsa) ->
                        true;
                   (_) ->
                        false
                end
               },
               {cipher,
                fun(aes_256_cbc) ->
                        true;
                   (_) ->
                        false
                end
               },
               {mac,

                fun(sha) ->
                        true;
                   (_) ->
                        false
                end
               }
              ],
    Cipher = #{cipher => aes_256_cbc,
               key_exchange => dhe_rsa,
               mac => sha,
               prf => default_prf},
    [Cipher] = ssl:filter_cipher_suites(All, Filters),
    [Cipher | Rest0] = ssl:prepend_cipher_suites([Cipher], Default),
    case (Version == 'tlsv1') orelse (Version == 'tlsv1.1')  orelse (Version == 'dtlsv1') of
        true ->
            true = lists:member(Cipher, Default),
            [Cipher | Rest0] = ssl:prepend_cipher_suites(Filters, Default),
            false = lists:member(Cipher, Rest0);
        false ->
            false = lists:member(Cipher, Default)
    end,
    [Cipher | Rest1] = lists:reverse(ssl:append_cipher_suites([Cipher], Default)),
    case (Version == 'tlsv1') orelse (Version == 'tlsv1.1') orelse (Version == 'dtlsv1') of
        true ->
            true = lists:member(Cipher, Default),
            [Cipher | Rest1] = lists:reverse(ssl:append_cipher_suites(Filters, Default)),
            false = lists:member(Cipher, Rest1);
         false ->
            false = lists:member(Cipher, Default)
    end,
    [] = lists:dropwhile(fun(X) -> not lists:member(X, Default) end, Anonymous),
    [] = lists:dropwhile(fun(X) -> not lists:member(X, All) end, Anonymous),
    case Version of
        tlsv1 ->
           true = lists:member(MandatoryCipherSuiteTLS1_0, All);
        'tlsv1.1' ->
            true = lists:member(MandatoryCipherSuiteTLS1_1, All),
            true = lists:member(MandatoryCipherSuiteTLS1_0TLS1_2, All);
        'tlsv1.2' ->
            ok;
        'tlsv1.3' ->
            ok;
        'dtlsv1' ->
            true = lists:member(MandatoryCipherSuiteTLS1_0TLS1_2, All);
        'dtlsv1.2' ->
            ok
    end.

many_client_starter(Hostname, Port, ClientOpts, Server) ->
    spawn_clients(Hostname, Port, ClientOpts, 50),
    ct:sleep(100),
    many_client_starter(Hostname, Port, ClientOpts, Server).

spawn_clients(_, _, _, 0) ->
    ok;
spawn_clients(Hostname, Port, ClientOpts, N) ->
    spawn_link(fun() ->
                     case N of
                         20 ->
                             exit(self(), kill);
                         _ ->
                             {ok, _} = ssl:connect(Hostname, Port, ClientOpts)
                     end
               end),
    spawn_clients(Hostname, Port, ClientOpts, N-1).

loop(L) ->
    {ok, A} = ssl:transport_accept(L),
    spawn(fun() ->
                  ssl:handshake(A)
          end),
    loop(L).

check_process_count(Count) ->
    check_process_count(Count, 5).

check_process_count(Count, 0) ->
    Count = proplists:get_value(supervisors, supervisor:count_children(tls_connection_sup));
check_process_count(Count, Try) ->
    case proplists:get_value(supervisors, supervisor:count_children(tls_connection_sup)) of
        Count ->
            ok;
        Other ->
            ?CT_PAL("Not expected number of cildren ~p on try ~p",
                    [Other, Try]),
            ct:sleep(500), %% Wait long enough
            check_process_count(Count, Try - 1)
    end.

do_wait_tls_server(Hostname, Port) ->
    do_wait_tls_server(Hostname, Port, 5).

do_wait_tls_server(_,_, 0)->
    ct:fail(to_connect_to_server);
do_wait_tls_server(Hostname, Port, Try) ->
    case gen_tcp:connect(Hostname, Port, []) of
	{ok, S} ->
	    gen_tcp:close(S),
            ok;
	_  ->
	    ct:sleep(?SLEEP),
	    do_wait_tls_server(Hostname, Port, Try-1)
    end.
