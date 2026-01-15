%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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
-module(openssl_client_cert_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1,
         init_per_group/2,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_group/2,
         end_per_testcase/2
        ]).

%% Test cases
-export([no_auth/0,
         no_auth/1,
         auth/0,
         auth/1,
         client_auth_empty_cert_accepted/0,
         client_auth_empty_cert_accepted/1,
         client_auth_empty_cert_rejected/0,
         client_auth_empty_cert_rejected/1,
         client_auth_use_partial_chain/0,
         client_auth_use_partial_chain/1,
         client_auth_do_not_use_partial_chain/0,
         client_auth_do_not_use_partial_chain/1,
         client_auth_partial_chain_fun_fail/0,
         client_auth_partial_chain_fun_fail/1,
         missing_root_cert_no_auth/0,
         missing_root_cert_no_auth/1,
         unsupported_sign_algo_client_auth/0,
         unsupported_sign_algo_client_auth/1,
         unsupported_sign_algo_cert_client_auth/0,
         unsupported_sign_algo_cert_client_auth/1,
         hello_retry_request/0,
         hello_retry_request/1,
         custom_groups/0,
         custom_groups/1,
         mlkem_groups/0,
         mlkem_groups/1,
         mlkem_hybrid_groups/0,
         mlkem_hybrid_groups/1,
         hello_retry_client_auth/0,
         hello_retry_client_auth/1,
         hello_retry_client_auth_empty_cert_accepted/0,
         hello_retry_client_auth_empty_cert_accepted/1,
         hello_retry_client_auth_empty_cert_rejected/0,
         hello_retry_client_auth_empty_cert_rejected/1
        ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     {group,  openssl_client}
    ].

groups() ->
    [
     {openssl_client, [], protocol_groups()},
     {'tlsv1.3', [], transport_group()},
     {transport_socket, [], tls_1_3_protocol_groups()},
     {'tlsv1.2', [], pre_tls_1_3_protocol_groups() ++ [{group, ecdsa}, {group, rsa_pss_rsae}, {group, rsa_pss_pss}]},
     {'tlsv1.1', [], pre_tls_1_3_protocol_groups()},
     {'tlsv1', [], pre_tls_1_3_protocol_groups()},
     {'dtlsv1.2', [], pre_tls_1_3_protocol_groups() ++ [{group, ecdsa}]},
     {'dtlsv1', [], pre_tls_1_3_protocol_groups()},
     {rsa, [parallel], all_version_tests()},
     {ecdsa, [parallel], all_version_tests() ++ partial_chain_with_ecdsa()},
     {dsa, [parallel], all_version_tests()},
     {rsa_1_3, [parallel],
      all_version_tests() ++
          tls_1_3_tests() ++
          [unsupported_sign_algo_client_auth,
           unsupported_sign_algo_cert_client_auth]},
     {rsa_pss_rsae, [parallel], all_version_tests()},
     {rsa_pss_pss, [parallel], all_version_tests()},
     {rsa_pss_rsae_1_3, [parallel], all_version_tests() ++ tls_1_3_tests()},
     {rsa_pss_pss_1_3, [parallel], all_version_tests() ++ tls_1_3_tests()},
     {ecdsa_1_3, [parallel], all_version_tests() ++ tls_1_3_tests()},
     {eddsa, [parallel], all_version_tests() ++ tls_1_3_tests() ++ partial_chain_with_ecdsa()},
     {mldsa, [parallel], all_version_tests() ++ tls_1_3_tests()}
    ].

protocol_groups() ->
    case ssl_test_lib:openssl_sane_dtls() of
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
             {group, 'tlsv1'}
            ]
     end.

pre_tls_1_3_protocol_groups() ->
    [{group, rsa},
     {group, dsa}].

transport_group() ->
    [{group, transport_socket}].

tls_1_3_protocol_groups() ->
    [{group, rsa_1_3},
     {group, rsa_pss_rsae_1_3},
     {group, rsa_pss_pss_1_3},
     {group, ecdsa_1_3},
     {group, eddsa},
     {group, mldsa}
    ].

tls_1_3_tests() ->
    [
     hello_retry_request,
     custom_groups,
     mlkem_groups,
     mlkem_hybrid_groups,
     hello_retry_client_auth,
     hello_retry_client_auth_empty_cert_accepted,
     hello_retry_client_auth_empty_cert_rejected
    ].

all_version_tests() ->
    [
     no_auth,
     auth,
     client_auth_empty_cert_accepted,
     client_auth_empty_cert_rejected,
     missing_root_cert_no_auth
    ].

partial_chain_with_ecdsa() ->
    %% Concept of partial chain is not dependent
    %% of cert type, introp test it with ecdsa.
    [client_auth_use_partial_chain,
     client_auth_do_not_use_partial_chain,
     client_auth_partial_chain_fun_fail].

init_per_suite(Config0) ->
    Config = ssl_test_lib:init_per_suite(Config0, openssl),
    case ssl_test_lib:working_openssl_client(Config) of
        true -> Config;
        false -> throw({skip, "Broken OpenSSL s_client"})
    end.

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(openssl_client, Config) ->
    [{client_type, openssl}, {server_type, erlang} | Config];
init_per_group(Group, Config) when Group == rsa;
                                    Group == rsa_1_3 ->
    ssl_cert_tests:rsa_config(Config);
init_per_group(Alg, Config) when
      Alg == rsa_pss_rsae;
      Alg == rsa_pss_pss;
      Alg == rsa_pss_rsae_1_3;
      Alg == rsa_pss_pss_1_3 ->
    RSAAlg = rsa_alg(Alg),
    case ssl_test_lib:is_sane_openssl_pss(RSAAlg) of
        true ->
            ssl_cert_tests:rsa_pss_config(RSAAlg, Config);
        false ->
            {skip, "Missing RSA-PSS OpenSSL stack support"}
    end;
init_per_group(Group, Config0) when Group == ecdsa;
                                    Group == ecdsa_1_3 ->
    case ssl_test_lib:openssl_ecdsa_suites() =/= [] of
        true ->
            ssl_cert_tests:ecdsa_config(Config0);
        false ->
            {skip, "Missing ECDSA OpenSSL stack support"}
    end;
init_per_group(eddsa, Config) ->
    ssl_cert_tests:openssl_eddsa_config(Config);
init_per_group(mldsa, Config) ->
    ssl_cert_tests:mldsa_config(Config);
init_per_group(dsa, Config) ->
    ssl_cert_tests:openssl_dsa_config(Config);
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(mlkem_groups, Config) ->
    ssl_cert_tests:support_kems(Config);
init_per_testcase(mlkem_hybrid_groups, Config) ->
   ssl_cert_tests:support_kems(Config);
init_per_testcase(TestCase, Config) when
      TestCase == client_auth_empty_cert_accepted;
      TestCase == client_auth_empty_cert_rejected ->
    Version = ssl_test_lib:protocol_version(Config),

    case Version of
        sslv3 ->
            %% Openssl client sends "No Certificate Reserved" warning ALERT
            %% instead of sending EMPTY cert message in SSL-3.0 so empty cert test are not
            %% relevant
            {skip, openssl_behaves_differently};
        _ ->
            ssl_test_lib:ct_log_supported_protocol_versions(Config),
            ct:timetrap({seconds, 30}),
            Config
    end;
init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 30}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

no_auth() ->
     ssl_cert_tests:no_auth().

no_auth(Config) ->
      ssl_cert_tests:no_auth(Config).
%%--------------------------------------------------------------------
auth() ->
    ssl_cert_tests:auth().
auth(Config) ->
    ssl_cert_tests:auth(Config).
%%--------------------------------------------------------------------
client_auth_empty_cert_accepted() ->
     ssl_cert_tests:client_auth_empty_cert_accepted().
client_auth_empty_cert_accepted(Config) ->
    ssl_cert_tests:client_auth_empty_cert_accepted(Config).
%%--------------------------------------------------------------------
client_auth_empty_cert_rejected() ->
      ssl_cert_tests:client_auth_empty_cert_rejected().
client_auth_empty_cert_rejected(Config) ->
    ssl_cert_tests:client_auth_empty_cert_rejected(Config).
%%--------------------------------------------------------------------
%%  Have to use partial chain functionality on side running Erlang (we are not testing OpenSSL features)
client_auth_use_partial_chain() ->
    [{doc, "Server does not trust an intermediat CA and fails the connetion as ROOT has expired"}].
client_auth_use_partial_chain(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Group = proplists:get_value(name, Prop),
    Version = proplists:get_value(version, Config),
    Alg = proplists:get_value(cert_key_alg, Config),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(Group),
    Ciphers = appropriate_ciphers(Group, Version),

    {Year, Month, Day} = date(),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_pem(Alg,
                                                                        [{client_chain,
                                                                          [[{validity, {{Year-2, Month, Day},
                                                                                        {Year-1, Month, Day}}}],
                                                                           [],
                                                                           []
                                                                          ]},
                                                                         {server_chain, DefaultCertConf}],
                                                                        Config, "use_partial_chain"),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    CaCertsFile = proplists:get_value(cacertfile, ClientOpts),
    ClientCACerts = [DerCA  ||  {'Certificate', DerCA, _} <- ssl_test_lib:pem_to_der(CaCertsFile)],
    [_, IntermidiateCA, _] = ClientCACerts,
    PartialChain =  fun(CertChain) ->
			    case lists:member(IntermidiateCA, CertChain) of
				true ->
				    {trusted_ca, IntermidiateCA};
				false ->
				    unknown_ca
			    end
		    end,
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}, {partial_chain, PartialChain} |
                  ssl_test_lib:ssl_options(extra_server, [{ciphers, Ciphers} | ServerOpts0], Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
%%  Have to use partial chain functionality on side running Erlang (we are not testing OpenSSL features)
client_auth_do_not_use_partial_chain() ->
   ssl_cert_tests:client_auth_do_not_use_partial_chain().
client_auth_do_not_use_partial_chain(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Group = proplists:get_value(name, Prop),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(Group),
    Version = proplists:get_value(version, Config),
    Alg = proplists:get_value(cert_key_alg, Config),
    Ciphers = appropriate_ciphers(Group, Version),

    {Year, Month, Day} = date(),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_pem(Alg,
                                                                        [{client_chain,
                                                                          [[{validity, {{Year-2, Month, Day},
                                                                                        {Year-1, Month, Day}}}],
                                                                           [],
                                                                           []
                                                                          ]},
                                                                         {server_chain, DefaultCertConf}], Config, "do_not_use_partial_chain"),
    PartialChain =  fun(_CertChain) ->
			    unknown_ca
		    end,
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}, {partial_chain, PartialChain} |
                  ssl_test_lib:ssl_options(extra_server,  [{ciphers, Ciphers} | ServerOpts0], Config)],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_expired).

%%--------------------------------------------------------------------
%%  Have to use partial chain functionality on side running Erlang (we are not testing OpenSSL features)
client_auth_partial_chain_fun_fail() ->
   ssl_cert_tests:client_auth_partial_chain_fun_fail().
client_auth_partial_chain_fun_fail(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Group = proplists:get_value(name, Prop),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(Group),
    Version = proplists:get_value(version, Config),
    Alg = proplists:get_value(cert_key_alg, Config),
    Ciphers = appropriate_ciphers(Group, Version),

    {Year, Month, Day} = date(),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_pem(Alg,
                                                                        [{client_chain,
                                                                          [[{validity, {{Year-2, Month, Day},
                                                                                        {Year-1, Month, Day}}}],
                                                                           [],
                                                                           []
                                                                          ]},
                                                                         {server_chain, DefaultCertConf}], Config, "partial_chain_fun_fail"),
    PartialChain = fun(_CertChain) ->
                           error(crash_on_purpose)
                   end,
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}, {partial_chain, PartialChain} |
                  ssl_test_lib:ssl_options(extra_server,  [{ciphers, Ciphers} | ServerOpts0], Config)],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_expired).

%%--------------------------------------------------------------------
missing_root_cert_no_auth() ->
   ssl_cert_tests:missing_root_cert_no_auth().
missing_root_cert_no_auth(Config) when is_list(Config) ->
    ssl_cert_tests:missing_root_cert_no_auth(Config).

%%--------------------------------------------------------------------
%% TLS 1.3 Test Cases ------------------------------------------------
%%--------------------------------------------------------------------
hello_retry_request() ->
    ssl_cert_tests:hello_retry_request().
hello_retry_request(Config) ->
    ssl_cert_tests:hello_retry_request(Config).
%%--------------------------------------------------------------------
custom_groups() ->
 ssl_cert_tests:custom_groups().
custom_groups(Config) ->
  ssl_cert_tests:custom_groups(Config).
mlkem_groups() ->
    ssl_cert_tests:mlkem_groups().
mlkem_groups(Config) ->
    ssl_cert_tests:mlkem_groups(Config).
mlkem_hybrid_groups() ->
    ssl_cert_tests:mlkem_hybrid_groups().
mlkem_hybrid_groups(Config) ->
    ssl_cert_tests:mlkem_hybrid_groups(Config).
unsupported_sign_algo_cert_client_auth() ->
 ssl_cert_tests:unsupported_sign_algo_cert_client_auth().
unsupported_sign_algo_cert_client_auth(Config) ->
    ssl_cert_tests:unsupported_sign_algo_cert_client_auth(Config).
unsupported_sign_algo_client_auth() ->
 ssl_cert_tests:unsupported_sign_algo_client_auth().
unsupported_sign_algo_client_auth(Config) ->
    ssl_cert_tests:unsupported_sign_algo_client_auth(Config).
%%--------------------------------------------------------------------
hello_retry_client_auth() ->
 ssl_cert_tests:hello_retry_client_auth().
hello_retry_client_auth(Config) ->
  ssl_cert_tests:hello_retry_client_auth(Config).
%%--------------------------------------------------------------------
hello_retry_client_auth_empty_cert_accepted() ->
    ssl_cert_tests:hello_retry_client_auth_empty_cert_accepted().
hello_retry_client_auth_empty_cert_accepted(Config) ->
    ssl_cert_tests:hello_retry_client_auth_empty_cert_accepted(Config).
%%--------------------------------------------------------------------
hello_retry_client_auth_empty_cert_rejected() ->
    ssl_cert_tests:hello_retry_client_auth_empty_cert_rejected().
hello_retry_client_auth_empty_cert_rejected(Config) ->
   ssl_cert_tests:hello_retry_client_auth_empty_cert_rejected(Config).

rsa_alg(rsa_pss_rsae_1_3) ->
    rsa_pss_rsae;
rsa_alg(rsa_pss_pss_1_3) ->
    rsa_pss_pss;
rsa_alg(Atom) ->
    Atom.

appropriate_ciphers(dsa, Version) ->
    ssl:cipher_suites(all, Version);
appropriate_ciphers(_, Version) ->
    ssl:cipher_suites(default, Version).
