%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2024. All Rights Reserved.
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

-module(tls_1_3_version_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1,
         init_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         end_per_suite/1,
         end_per_group/2
        ]).

%% Test cases
-export([tls13_client_tls12_server/0,
         tls13_client_tls12_server/1,
         tls13_client_with_ext_tls12_server/0,
         tls13_client_with_ext_tls12_server/1,
         tls12_client_tls13_server/0,
         tls12_client_tls13_server/1,
         tls_client_tls10_server/0,
         tls_client_tls10_server/1,
         tls_client_tls11_server/0,
         tls_client_tls11_server/1,
         tls_client_tls12_server/0,
         tls_client_tls12_server/1,
         tls10_client_tls_server/0,
         tls10_client_tls_server/1,
         tls11_client_tls_server/0,
         tls11_client_tls_server/1,
         tls12_client_tls_server/0,
         tls12_client_tls_server/1,
         legacy_tls12_client_tls_server/0,
         legacy_tls12_client_tls_server/1,
         legacy_tls12_server_tls_client/0,
         legacy_tls12_server_tls_client/1,
         tls13_client_tls11_server/0,
         tls13_client_tls11_server/1,
         tls12_legacy_cert_sign/0,
         tls12_legacy_cert_sign/1,
         tls13_legacy_cert_sign/0,
         tls13_legacy_cert_sign/1,
         tls13_legacy_cert_sign_with_pss_rsae/0,
         tls13_legacy_cert_sign_with_pss_rsae/1,
         tls12_legacy_cert_sign_with_pss_rsae/0,
         tls12_legacy_cert_sign_with_pss_rsae/1,
         reject_legacy_cert/0,
         reject_legacy_cert/1,
         middle_box_tls13_client/0,
         middle_box_tls13_client/1,
         middle_box_tls12_enabled_client/0,
         middle_box_tls12_enabled_client/1,
         middle_box_client_tls_v2_session_reused/0,
         middle_box_client_tls_v2_session_reused/1,
         renegotiate_error/0,
         renegotiate_error/1,
         client_cert_fail_alert_active/0,
         client_cert_fail_alert_active/1,
         client_cert_fail_alert_passive/0,
         client_cert_fail_alert_passive/1,
         keylog_on_alert/0,
         keylog_on_alert/1
        ]).


%% Test callback
-export([check_session_id/2]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     cert_groups()
    ].

groups() ->
    [
     {rsa, [], [reject_legacy_cert | tls_1_3_1_2_tests() ++ legacy_tests()]},
     {ecdsa, [], tls_1_3_1_2_tests()}
    ].

cert_groups() ->
    [{group, rsa},
     {group, ecdsa}].

tls_1_3_1_2_tests() ->
    [tls13_client_tls12_server,
     tls13_client_with_ext_tls12_server,
     tls12_client_tls13_server,
     tls_client_tls12_server,
     tls12_client_tls_server,
     legacy_tls12_client_tls_server,
     legacy_tls12_server_tls_client,
     middle_box_tls13_client,
     middle_box_tls12_enabled_client,
     middle_box_client_tls_v2_session_reused,
     renegotiate_error,
     client_cert_fail_alert_active,
     client_cert_fail_alert_passive,
     keylog_on_alert
    ].
legacy_tests() ->
    [tls_client_tls10_server,
     tls_client_tls11_server,
     tls_client_tls12_server,
     tls10_client_tls_server,
     tls11_client_tls_server,
     tls12_client_tls_server,
     tls13_client_tls11_server,
     tls13_legacy_cert_sign,
     tls13_legacy_cert_sign_with_pss_rsae,
     tls12_legacy_cert_sign,
     tls12_legacy_cert_sign_with_pss_rsae
    ].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            case ssl_test_lib:sufficient_crypto_support('tlsv1.3') of
                true ->
                    ssl_test_lib:clean_start(),
                    [{client_type, erlang}, {server_type, erlang} |
                     Config];
                false ->
                    {skip, "Insufficient crypto support for TLS-1.3"}
            end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(rsa, Config0) ->
    Config = ssl_test_lib:make_rsa_cert(Config0),
    COpts = proplists:get_value(client_rsa_opts, Config),
    SOpts = proplists:get_value(server_rsa_opts, Config),
    [{client_type, erlang},
     {server_type, erlang},{client_cert_opts, COpts}, {server_cert_opts, SOpts} |
     lists:delete(server_cert_opts, lists:delete(client_cert_opts, Config))];
init_per_group(ecdsa, Config0) ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso
        (lists:member(ecdh, PKAlg) orelse lists:member(dh, PKAlg)) of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            [{client_type, erlang},
             {server_type, erlang},{client_cert_opts, COpts}, {server_cert_opts, SOpts} |
             lists:delete(server_cert_opts, lists:delete(client_cert_opts, Config))];
        false ->
            {skip, "Missing EC crypto support"}
    end.

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 20}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

tls13_client_tls12_server() ->
    [{doc,"Test that a TLS 1.3 client can connect to a TLS 1.2 server."}].

tls13_client_tls12_server(Config) when is_list(Config) ->
    ClientOpts = [{versions, ['tlsv1.3', 'tlsv1.2']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.1', 'tlsv1.2']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).


tls13_client_with_ext_tls12_server() ->
     [{doc,"Test basic connection between TLS 1.2 server and TLS 1.3 client when "
       "client has TLS 1.3 specific extensions"}].

tls13_client_with_ext_tls12_server(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),

    ServerOpts = [{versions, ['tlsv1.2']},
                  {verify, verify_peer}, {fail_if_no_peer_cert, true},
                  {signature_algs, [rsa_pss_rsae_sha256,
                                    {sha256, rsa},
                                    {sha256, ecdsa},
                                    {sha, ecdsa}]}| ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {signature_algs_cert, [ecdsa_secp384r1_sha384,
                                         ecdsa_secp256r1_sha256,
                                         rsa_pss_rsae_sha256,
                                         rsa_pkcs1_sha256,
                                         ecdsa_sha1]}|ClientOpts0],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls12_client_tls13_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.3 server."}].

tls12_client_tls13_server(Config) when is_list(Config) ->
    ClientOpts = [{versions, ['tlsv1.1', 'tlsv1.2']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.3', 'tlsv1.2']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls10_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.0 server."}].
tls_client_tls10_server(Config) when is_list(Config) ->
    CCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),
    ClientOpts = [{versions, ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                  {ciphers, CCiphers} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {ciphers, ssl:cipher_suites(all, 'tlsv1')}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls11_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.1 server."}].
tls_client_tls11_server(Config) when is_list(Config) ->
    CCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),
    ClientOpts = [{versions,
                   ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                  {ciphers, CCiphers} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,['tlsv1.1']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {ciphers, ssl:cipher_suites(all, 'tlsv1.1')}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls_client_tls12_server() ->
    [{doc,"Test that a TLS 1.0-1.3 client can connect to a TLS 1.2 server."}].
tls_client_tls12_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1', 'tlsv1.1', 'tlsv1.2', 'tlsv1.3']} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.2']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls10_client_tls_server() ->
    [{doc,"Test that a TLS 1.0 client can connect to a TLS 1.0-1.3 server."}].
tls10_client_tls_server(Config) when is_list(Config) ->
    SCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),
    ClientOpts = [{versions, ['tlsv1']},
                  {ciphers, ssl:cipher_suites(all, 'tlsv1')} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {ciphers, SCiphers} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls11_client_tls_server() ->
    [{doc,"Test that a TLS 1.1 client can connect to a TLS 1.0-1.3 server."}].
tls11_client_tls_server(Config) when is_list(Config) ->
    SCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),
    ClientOpts = [{versions, ['tlsv1.1']},
                  {ciphers, ssl:cipher_suites(all, 'tlsv1.1')} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {ciphers, SCiphers} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls12_client_tls_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.0-1.3 server."}].
tls12_client_tls_server(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.2']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions,
                   ['tlsv1','tlsv1.1', 'tlsv1.2', 'tlsv1.3']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

legacy_tls12_client_tls_server() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.3 server."}].

legacy_tls12_client_tls_server(Config) when is_list(Config) ->
    SHA = ssl_test_lib:appropriate_sha(crypto:supports()),
    ClientOpts = [{versions, ['tlsv1.1', 'tlsv1.2']}, {signature_algs, [{SHA, rsa}, {SHA, ecdsa}]} |
                  ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.3', 'tlsv1.2']},
                   {signature_algs, ssl:signature_algs(default, 'tlsv1.3')},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

legacy_tls12_server_tls_client() ->
    [{doc,"Test that a TLS 1.3 enabled client can connect to legacy TLS-1.2 server."}].

legacy_tls12_server_tls_client(Config) when is_list(Config) ->
    SHA = sha384,
    Prop = proplists:get_value(tc_group_properties, Config),
    Alg = proplists:get_value(name, Prop),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(Alg, [{server_chain,
                                                                               [[{digest, SHA}],
                                                                                [{digest, SHA}],
                                                                                [{digest, SHA}]]},
                                                                              {client_chain,
                                                                               [[{digest, SHA}],
                                                                                [{digest, SHA}],
                                                                                [{digest, SHA}]]}
                                                                             ]),

    ClientOpts = [{versions, ['tlsv1.3', 'tlsv1.2']} | ClientOpts0],
    ServerOpts =  [{versions, ['tlsv1.2']}, {verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {signature_algs, [{SHA, Alg}]}
                  | ServerOpts0],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

tls13_legacy_cert_sign() ->
    [{doc,"Test that a TLS 1.3 client can connect to  TLS-1.3 server with pkcs1_SHA2 cert"}].

tls13_legacy_cert_sign(Config) when is_list(Config) ->
    ClientOpts = [{versions, ['tlsv1.3']},
                  {signature_algs, rsa_pss_rsae_algs() ++ legacy_rsa_algs()}],
    ServerOpts = [{versions, ['tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true},
                  {signature_algs, rsa_pss_rsae_algs()},
                  {signature_algs_cert, legacy_rsa_algs()}],

    test_rsa_pcks1_cert(sha256, ClientOpts, ServerOpts, Config),
    test_rsa_pcks1_cert(sha512, ClientOpts, ServerOpts, Config),
    test_rsa_pcks1_cert(sha384, ClientOpts, ServerOpts, Config).

tls13_legacy_cert_sign_with_pss_rsae() ->
    [{doc,"Test that a TLS 1.3 enabled client can connect to legacy TLS-1.2 server with legacy pkcs1_SHA2 cert"}].

tls13_legacy_cert_sign_with_pss_rsae(Config) when is_list(Config) ->
    ClientOpts =  [{versions, ['tlsv1.3', 'tlsv1.2']},
                   {signature_algs, rsa_pss_rsae_algs()},
                   {signature_algs_cert, legacy_rsa_algs()}
                  ],
    ServerOpts = [{versions, ['tlsv1.2']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true},
                  {signature_algs, rsa_pss_rsae_algs()},
                  {signature_algs_cert, legacy_rsa_algs()}
                 ],

    test_rsa_pcks1_cert(sha256, ClientOpts, ServerOpts, Config),
    test_rsa_pcks1_cert(sha512, ClientOpts, ServerOpts, Config),
    test_rsa_pcks1_cert(sha384, ClientOpts, ServerOpts, Config).

tls12_legacy_cert_sign() ->
    [{doc,"Test that a TLS 1.2 client (with old configuration) can connect to  TLS-1.2 server with pkcs1_SHA2 cert"}].

tls12_legacy_cert_sign(Config) when is_list(Config) ->
    ClientOpts = [{versions, ['tlsv1.2']},
                  {signature_algs, rsa_algs()}],
    ServerOpts = [{versions, ['tlsv1.2']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true},
                  {signature_algs, rsa_algs()}],

    test_rsa_pcks1_cert(sha256, ClientOpts, ServerOpts, Config),
    test_rsa_pcks1_cert(sha512, ClientOpts, ServerOpts, Config),
    test_rsa_pcks1_cert(sha384, ClientOpts, ServerOpts, Config).

tls12_legacy_cert_sign_with_pss_rsae() ->
    [{doc,"Test that a modern TLS 1.2 client can connect to TLS-1.2 server with legacy pkcs1_SHA2 cert"}].

tls12_legacy_cert_sign_with_pss_rsae(Config) when is_list(Config) ->
    ClientOpts =  [{versions, ['tlsv1.2']},
                   {signature_algs, rsa_pss_rsae_algs() ++ rsa_algs()}
                  ],
    ServerOpts = [{versions, ['tlsv1.2']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true},
                  {signature_algs, rsa_pss_rsae_algs()},
                  {signature_algs_cert, legacy_rsa_algs()}
                 ],

    test_rsa_pcks1_cert(sha256, ClientOpts, ServerOpts, Config),
    test_rsa_pcks1_cert(sha512, ClientOpts, ServerOpts, Config),
    test_rsa_pcks1_cert(sha384, ClientOpts, ServerOpts, Config).

reject_legacy_cert() ->
    [{doc,"Test that client sends empty cert if does only have legacy pkcs1_SHA2 cert that is not supported by the server"
      "and do not make connection with client that requires better cert and only option is legacy pkcs1_SHA2 cert"}].

reject_legacy_cert(Config) when is_list(Config) ->
    reject_legacy_cert('tlsv1.3', certificate_required, Config),
    reject_legacy_cert('tlsv1.2', handshake_failure, Config).

reject_legacy_cert(Version, Alert, Config) ->
    COpts =  [{signature_algs, rsa_pss_pss_algs() ++ rsa_pss_rsae_algs()},
              {versions, [Version]}
             ],
    SOpts = [{verify, verify_peer},
             {fail_if_no_peer_cert, true},
             {signature_algs, rsa_pss_pss_algs()},
             {versions, [Version]}
            ],
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} =
        public_key:pkix_test_data(#{client_chain =>
                                        #{root => root_key(sha256),
                                          intermedites => intermediates(sha256, 1),
                                          peer => peer_key(sha256)}, 
                                    server_chain => 
                                        #{root => root_key(sha256, ssl_test_lib:pss_params(sha256)),
                                          intermedites => intermediates(sha256, 
                                                                        ssl_test_lib:pss_params(sha256), 
                                                                        1),
                                          peer => peer_key(sha256, ssl_test_lib:pss_params(sha256))
                                         }}),   
    ClientOpts = ClientOpts0 ++ COpts,
    ServerOpts = ServerOpts0 ++ SOpts,
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, Alert),
    RevClientOpts = ServerOpts0 ++ [{signature_algs, rsa_pss_pss_algs()}],
    RevServerOtps = ClientOpts0 ++ [{signature_algs, rsa_pss_pss_algs() ++ rsa_pss_rsae_algs()}],
    ssl_test_lib:basic_alert(RevClientOpts,  RevServerOtps, Config, insufficient_security).

middle_box_tls13_client() ->
    [{doc,"Test that a TLS 1.3 client can connect to a 1.3 server with and without middle box compatible mode."}].
middle_box_tls13_client(Config) when is_list(Config) ->
    ClientOpts = [{versions,
                   ['tlsv1.3']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.3']} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    middlebox_test(true, not_empty, ClientOpts, ServerOpts, Config),
    middlebox_test(false, empty, ClientOpts, ServerOpts, Config).

middle_box_tls12_enabled_client() ->
    [{doc,"Test that a TLS 1.2 enabled client can connect to a TLS 1.3 server with and without middle box compatible mode."}].
middle_box_tls12_enabled_client(Config) when is_list(Config) ->
    ClientOpts = [{versions, ['tlsv1.2', 'tlsv1.3']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1.3']} |
                   ssl_test_lib:ssl_options(server_cert_opts, Config)],
    middlebox_test(true, not_empty, ClientOpts, ServerOpts, Config),
    middlebox_test(false, empty, ClientOpts, ServerOpts, Config).

middle_box_client_tls_v2_session_reused() ->
    [{doc, "Test that TLS-1.3 middlebox enabled client can reuse TLS-1.2 session when talking to TLS-1.2 server"}].
middle_box_client_tls_v2_session_reused(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_cert_opts, Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{versions, ['tlsv1.2']} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, CSock} = ssl_test_lib:start_client([return_socket, {node, ClientNode}, {port, Port},
                                                  {host, Hostname},
                                                  {from, self()},
                                                  {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                                  {options,
                                                   [{versions, ['tlsv1.2']}, {reuse_sessions, save}| ClientOpts]}]),
    Server ! listen,
    {ok,[{session_id, SessionId}, {session_data, SessData}]} = ssl:connection_information(CSock, [session_id, session_data]),
    {_Client1, CSock1}  = ssl_test_lib:start_client([return_socket,
                                                     {node, ClientNode}, {port, Port},
                                                     {host, Hostname},
                                                     {from, self()},
                                                     {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                                     {options,
                                                      [{versions, ['tlsv1.3', 'tlsv1.2']},
                                                       {middlebox_comp_mode, true},
                                                       {reuse_session, {SessionId, SessData}} | ClientOpts]}]),
    {ok,[{session_id, SessionId}]}  = ssl:connection_information(CSock1, [session_id]).

renegotiate_error() ->
    [{doc, "Test that an error is returned when ssl:renegotiate/1 is called on a connection running TLS-1.3"}].
renegotiate_error(Config) when is_list(Config) ->
    {_ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_cert_opts, Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{versions, ['tlsv1.3']} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Options = [{versions, ['tlsv1.3']} | ClientOpts],
    case ssl:connect(Hostname, Port, Options) of
        {ok, Socket} ->
            {error, notsup} = ssl:renegotiate(Socket);
        {error, Reason} ->
            ct:fail(Reason)
    end.

client_cert_fail_alert_active() ->
    [{doc, "Check that we receive alert message"}].
client_cert_fail_alert_active(Config) when is_list(Config) ->
    ssl:clear_pem_cache(),
    {_ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts0 = ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    NewClientCertFile = filename:join(PrivDir, "client_invalid_cert.pem"),

    create_bad_client_certfile(NewClientCertFile, ClientOpts0),

    ClientOpts = [{active, true},
                  {verify, verify_peer},
                  {certfile, NewClientCertFile} | proplists:delete(certfile, ClientOpts0)],
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}| ServerOpts0],
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, no_result, []}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, Socket} = ssl:connect(Hostname, Port, ClientOpts),
    receive
        {Server, {error, {tls_alert, {unknown_ca, _}}}} ->
            receive
                {ssl_error, Socket, {tls_alert, {unknown_ca, _}}} ->
                    ok
            after 500 ->
                    ct:fail(no_acticv_msg)
            end
    end.

client_cert_fail_alert_passive() ->
    [{doc, "Check that recv or setopts return alert"}].
client_cert_fail_alert_passive(Config) when is_list(Config) ->
    ssl:clear_pem_cache(),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts0 = ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    NewClientCertFile = filename:join(PrivDir, "client_invalid_cert.pem"),

    create_bad_client_certfile(NewClientCertFile, ClientOpts0),

    ClientOpts = [{active, false},
                  {verify, verify_peer},
                  {certfile, NewClientCertFile} | proplists:delete(certfile, ClientOpts0)],
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}| ServerOpts0],
    alert_passive(ServerOpts, ClientOpts, recv,
                  ServerNode, Hostname, unknown_ca),
    alert_passive(ServerOpts, ClientOpts, setopts,
                  ServerNode, Hostname, unknown_ca).

tls13_client_tls11_server() ->
    [{doc,"Test that a TLS 1.3 client gets old server alert from TLS 1.0 server."}].
tls13_client_tls11_server(Config) when is_list(Config) ->
    ClientOpts = [{versions, ['tlsv1.3']} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{versions, ['tlsv1']} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, insufficient_security).

keylog_on_alert() ->
    [{doc,"Test that keep_secrets keylog_hs callback, if specified, "
      "is called with keylog info when handshake alert is raised"}].

keylog_on_alert(Config) when is_list(Config) ->
    ssl:clear_pem_cache(),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts0 = ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    NewClientCertFile = filename:join(PrivDir, "client_invalid_cert.pem"),
    create_bad_client_certfile(NewClientCertFile, ClientOpts0),
    ClientOpts = [{versions, ['tlsv1.3']}, {active, false}, {certfile, NewClientCertFile}|
                  proplists:delete(certfile, ClientOpts0)],
    ServerOpts =  [{versions, ['tlsv1.3']},
                   {verify, verify_peer}, {fail_if_no_peer_cert, true}
                  | ServerOpts0],

    Me = self(),
    Fun = fun(AlertInfo) ->
                  Me ! {alert_info, AlertInfo}
          end,
    keylog_alert_passive([{keep_secrets, {keylog_hs, Fun}} | ServerOpts], ClientOpts, recv,
                         ServerNode, Hostname),

    receive_server_keylog_for_cert_alert(),

    keylog_alert_passive(ServerOpts,
                         [{keep_secrets, {keylog_hs, Fun}} | ClientOpts], recv,
                         ServerNode, Hostname),

    receive_client_keylog_for_cert_alert(),

    ClientNoCert = proplists:delete(keyfile, proplists:delete(certfile, ClientOpts0)),
    keylog_alert_passive([{keep_secrets, {keylog_hs, Fun}} | ServerOpts],
                         [{active, false} | ClientNoCert],
                         recv, ServerNode, Hostname),
    receive_server_keylog_for_cert_alert().

receive_server_keylog_for_cert_alert() ->
    %% This alert will be decrypted with application secrets
    %% as client is already in connection
    receive
        {alert_info, #{items := SKeyLog}} ->
            case keylog_prefixes(["CLIENT_HANDSHAKE_TRAFFIC_SECRET",
                                  "SERVER_HANDSHAKE_TRAFFIC_SECRET",
                                  "SERVER_TRAFFIC_SECRET_0"], SKeyLog) of
                true ->
                    ok;
                false ->
                    ct:fail({server_received, SKeyLog})
            end
    end.

receive_client_keylog_for_cert_alert() ->
    receive
        {alert_info, #{items := CKeyLog}} ->
            case keylog_prefixes(["CLIENT_HANDSHAKE_TRAFFIC_SECRET",
                                  "SERVER_HANDSHAKE_TRAFFIC_SECRET",
                                  "CLIENT_TRAFFIC_SECRET_0",
                                  "SERVER_TRAFFIC_SECRET_0"], CKeyLog) of
                true ->
                    ok;
                false ->
                    ct:fail({client_received, CKeyLog})
            end
    end.
keylog_prefixes([], []) ->
    true;
keylog_prefixes([Prefix | Prefixes], [Secret | Secrets]) ->
    case lists:prefix(Prefix, Secret) of
        true  ->
            keylog_prefixes(Prefixes, Secrets);
        false ->
            false
    end.

%%--------------------------------------------------------------------
%% Internal functions and callbacks -----------------------------------
%%--------------------------------------------------------------------

middlebox_test(Mode, Expected, ClientOpts, ServerOpts, Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {?MODULE, check_session_id, [Expected]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE, check_session_id, [Expected]}},
                                        {options,
                                         [{middlebox_comp_mode, Mode}| ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok).

check_session_id(Socket, Expected) ->
   {ok, [{session_id, SessionId}]} = ssl:connection_information(Socket, [session_id]),
    case {Expected, SessionId} of
        {empty, <<>>} ->
            ok;
        {not_empty, SessionId} when SessionId =/= <<>> ->
            ok;
        _ ->
            {nok, {{expected, Expected}, {got, SessionId}}}
    end.

alert_passive(ServerOpts, ClientOpts, Function,
              ServerNode, Hostname, AlertAtom) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, no_result, []}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, Socket} = ssl:connect(Hostname, Port, ClientOpts),
    ct:sleep(500),
    case Function of
        recv ->
            {error, {tls_alert, {AlertAtom,_}}} = ssl:recv(Socket, 0);
        setopts ->
            {error, {tls_alert, {unknown_ca,_}}} = ssl:setopts(Socket, [{active, once}])
    end.

keylog_alert_passive(ServerOpts, ClientOpts, Function,
                     ServerNode, Hostname) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, no_result, []}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Fun = fun() ->
                  {ok, Socket} = ssl:connect(Hostname, Port, ClientOpts),
                  case Function of
                      recv ->
                          ssl:recv(Socket, 0);
                      setopts ->
                          ssl:setopts(Socket, [{active, once}])
                  end
          end,
    %% Execute in other process and let test case detect key-log message.
    spawn_link(Fun).

create_bad_client_certfile(NewClientCertFile, ClientOpts) ->
    KeyFile =  proplists:get_value(keyfile, ClientOpts),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),
    ClientCertFile = proplists:get_value(certfile, ClientOpts),

    [{'Certificate', ClientDerCert, _}] = ssl_test_lib:pem_to_der(ClientCertFile),
    ClientOTPCert = public_key:pkix_decode_cert(ClientDerCert, otp),
    ClientOTPTbsCert = ClientOTPCert#'OTPCertificate'.tbsCertificate,
    NewClientDerCert = public_key:pkix_sign(ClientOTPTbsCert, Key),
    ssl_test_lib:der_to_pem(NewClientCertFile, [{'Certificate', NewClientDerCert, not_encrypted}]).

test_rsa_pcks1_cert(SHA, COpts, SOpts, Config) ->
    #{client_config := ClientOpts,
      server_config := ServerOpts} =
        public_key:pkix_test_data(#{server_chain => #{root => root_key(SHA),
                                                      intermediates => intermediates(SHA, 1),
                                                      peer => peer_key(SHA)},
                                    client_chain => #{root => root_key(SHA),
                                                      intermediates => intermediates(SHA, 1),
                                                      peer => peer_key(SHA)}}),
    ssl_test_lib:basic_test(COpts ++ ClientOpts, SOpts ++ ServerOpts, Config).


root_key(SHA) ->
    root_key(SHA, undefined).

peer_key(SHA) ->
    peer_key(SHA, undefined).

intermediates(SHA, N) ->
    intermediates(SHA, undefined, N).

root_key(SHA, undefined) ->
    %% As rsa keygen is not guaranteed to be fast
    [{digest, SHA},{key, ssl_test_lib:hardcode_rsa_key(6)}];
root_key(SHA, Params) ->
    [{digest, SHA}, {key, {ssl_test_lib:hardcode_rsa_key(6), Params}}].

peer_key(SHA, undefined) ->
    %% As rsa keygen is not guaranteed to be fast
    [{digest, SHA}, {key, ssl_test_lib:hardcode_rsa_key(5)}];
peer_key(SHA, Params) ->
     [{digest, SHA}, {key, {ssl_test_lib:hardcode_rsa_key(5), Params}}].

intermediates(SHA, undefined, N) when N =< 2 ->
    Default = lists:duplicate(N, [{digest, SHA}]),
    %% As rsa keygen is not guaranteed to be fast
    hardcode_rsa_keys(Default, N, []);
intermediates(SHA, Params, N) when N =< 2 ->
    Default = lists:duplicate(N, [{digest, SHA}]),
    %% As rsa keygen is not guaranteed to be fast
    hardcode_rsa_keys(Default, Params, N, []).

hardcode_rsa_keys([], 0, Acc) ->
    Acc;
hardcode_rsa_keys([Head | Tail], N, Acc) ->
    hardcode_rsa_keys(Tail, N-1, [[{key, ssl_test_lib:hardcode_rsa_key(N)} | Head] | Acc]).

hardcode_rsa_keys([], _, 0, Acc) ->
    Acc;
hardcode_rsa_keys([Head | Tail], Params, N, Acc) ->
    hardcode_rsa_keys(Tail, N-1, [[{key, {ssl_test_lib:hardcode_rsa_key(N)}, Params} | Head] | Acc]).

rsa_algs() ->
    [{sha512, rsa}, {sha384, rsa}, {sha256, rsa}].

legacy_rsa_algs() ->
    [rsa_pkcs1_sha512,rsa_pkcs1_sha384,rsa_pkcs1_sha256].

rsa_pss_rsae_algs() ->
    [rsa_pss_rsae_sha512,rsa_pss_rsae_sha384,rsa_pss_rsae_sha256].

rsa_pss_pss_algs() ->
    [rsa_pss_pss_sha512,rsa_pss_pss_sha384,rsa_pss_pss_sha256].

