%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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
-module(openssl_server_cert_SUITE).

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
         client_auth_partial_chain/0,
         client_auth_partial_chain/1,
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
     {group,  openssl_server}].

groups() ->
    [
     {openssl_server, [], protocol_groups()},
     {'tlsv1.3', [], tls_1_3_protocol_groups()},
     {'tlsv1.2', [], pre_tls_1_3_protocol_groups()},
     {'tlsv1.1', [], pre_tls_1_3_protocol_groups()},
     {'tlsv1', [], pre_tls_1_3_protocol_groups()},
     {'dtlsv1.2', [], pre_tls_1_3_protocol_groups()},
     {'dtlsv1', [], pre_tls_1_3_protocol_groups()},
     {rsa, [], all_version_tests()},
     {ecdsa, [], all_version_tests()},
     {dsa, [], all_version_tests()},
     {rsa_1_3, [], all_version_tests() ++ tls_1_3_tests()},
      %% TODO: Create proper conf of openssl server
      %%++ [unsupported_sign_algo_client_auth,
      %% unsupported_sign_algo_cert_client_auth]},
     {rsa_pss_rsae, [], all_version_tests() ++ tls_1_3_tests()},
     {rsa_pss_pss, [], all_version_tests() ++ tls_1_3_tests()},
     {ecdsa_1_3, [], all_version_tests() ++ tls_1_3_tests()}
     %% Enable test later when we have OpenSSL version that
     %% is able to read EDDSA private key files
     %%{eddsa_1_3, [], all_version_tests() ++ tls_1_3_tests()}
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
     {group, ecdsa},
     {group, dsa}].

tls_1_3_protocol_groups() ->
    [{group, rsa_1_3},
     {group, rsa_pss_rsae},
     {group, rsa_pss_pss},
     {group, ecdsa_1_3}
     %%{group, eddsa_1_3}
    ].

tls_1_3_tests() ->
    [
     hello_retry_request,
     custom_groups,
     hello_retry_client_auth,
     hello_retry_client_auth_empty_cert_accepted,
     hello_retry_client_auth_empty_cert_rejected
    ].

all_version_tests() ->
    [
     no_auth,
     auth,
     missing_root_cert_no_auth
    ].

init_per_suite(Config) ->
    ssl_test_lib:init_per_suite(Config, openssl).

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(openssl_server, Config0) ->
    Config = proplists:delete(server_type, proplists:delete(client_type, Config0)),
    [{client_type, erlang}, {server_type, openssl} | Config];
init_per_group(rsa = Group, Config0) ->
    Config = ssl_test_lib:make_rsa_cert(Config0),
    COpts = proplists:get_value(client_rsa_opts, Config),
    SOpts = proplists:get_value(server_rsa_opts, Config),
    %% Make sure _rsa* suite is chosen by ssl_test_lib:start_server
    Version = ssl_test_lib:protocol_version(Config),
    Ciphers = ssl_cert_tests:test_ciphers(fun(dhe_rsa) ->
                                                  true;
                                             (ecdhe_rsa) ->
                                                  true;
                                             (_) ->
                                                  false
                                          end, Version),
    case Ciphers of
        [_|_] ->
            [{cert_key_alg, rsa} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, [{ciphers, Ciphers} | COpts]},
                           {server_cert_opts, SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        [] ->
            {skip, {no_sup, Group, Version}}
    end;
init_per_group(rsa_1_3 = Group, Config0) ->
    Config = ssl_test_lib:make_rsa_cert(Config0),
    COpts = proplists:get_value(client_rsa_opts, Config),
    SOpts = proplists:get_value(server_rsa_opts, Config),
    %% Make sure _rsa* suite is chosen by ssl_test_lib:start_server
    Version = ssl_test_lib:protocol_version(Config),
    Ciphers = ssl_cert_tests:test_ciphers(undefined, Version),
    case Ciphers of
        [_|_] ->
            [{cert_key_alg, rsa} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, [{ciphers, Ciphers} | COpts]},
                           {server_cert_opts, SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        [] ->
            {skip, {no_sup, Group, Version}}
    end;
init_per_group(Alg, Config) when Alg == rsa_pss_rsae;
                                 Alg == rsa_pss_pss ->
    Supports = crypto:supports(),
    RSAOpts = proplists:get_value(rsa_opts, Supports),

    case lists:member(rsa_pkcs1_pss_padding, RSAOpts)
        andalso lists:member(rsa_pss_saltlen, RSAOpts)
        andalso lists:member(rsa_mgf1_md, RSAOpts)
        andalso ssl_test_lib:is_sane_oppenssl_pss(Alg)
    of
        true ->
            #{client_config := COpts,
              server_config := SOpts} = ssl_test_lib:make_rsa_pss_pem(Alg, [], Config, ""),
            [{cert_key_alg, Alg} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, COpts},
                           {server_cert_opts, SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        false ->
            {skip, "Missing crypto or OpenSSL support"}
    end;
init_per_group(ecdsa = Group, Config0) ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso (lists:member(ecdh, PKAlg) orelse
                                                lists:member(dh, PKAlg))
        andalso (ssl_test_lib:openssl_ecdsa_suites() =/= [])
    of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            %% Make sure ecdh* suite is chosen by ssl_test_lib:start_server
            Version = ssl_test_lib:protocol_version(Config),
            Ciphers =  ssl_cert_tests:test_ciphers(fun(ecdh_ecdsa) ->
                                                           true;
                                                      (ecdhe_ecdsa) ->
                                                           true;
                                                      (_) ->
                                                           false
                                                   end, Version),
            case Ciphers of
                [_|_] ->
                    [{cert_key_alg, ecdsa} |
                     lists:delete(cert_key_alg,
                                  [{client_cert_opts, [{ciphers, Ciphers} | COpts]},
                                   {server_cert_opts, SOpts} |
                                   lists:delete(server_cert_opts,
                                                lists:delete(client_cert_opts, Config))]
                                 )];
                        [] ->
                    {skip, {no_sup, Group, Version}}
            end;
        false ->
            {skip, "Missing EC crypto support"}
    end;
init_per_group(ecdsa_1_3 = Group, Config0) ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso (lists:member(ecdh, PKAlg) orelse
                                             lists:member(dh, PKAlg))
        andalso (ssl_test_lib:openssl_ecdsa_suites() =/= [])
    of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            %% Make sure ecdh* suite is chosen by ssl_test_lib:start_server
            Version = ssl_test_lib:protocol_version(Config),
            Ciphers =  ssl_cert_tests:test_ciphers(undefined, Version),
            case Ciphers of
                [_|_] ->
                    [{cert_key_alg, ecdsa} |
                     lists:delete(cert_key_alg,
                                  [{client_cert_opts, [{ciphers, Ciphers} | COpts]},
                                   {server_cert_opts, SOpts} |
                                   lists:delete(server_cert_opts,
                                                lists:delete(client_cert_opts, Config))]
                                 )];
                        [] ->
                    {skip, {no_sup, Group, Version}}
            end;
        false ->
            {skip, "Missing EC crypto support"}
    end;

init_per_group(eddsa_1_3, Config0) ->
    PKAlg = crypto:supports(public_keys),
    PrivDir = proplists:get_value(priv_dir, Config0),
    case lists:member(eddsa, PKAlg) andalso (lists:member(ecdh, PKAlg)) of
        true ->
            Conf = public_key:pkix_test_data(#{server_chain => #{root => ssl_test_lib:eddsa_conf(),
                                                                 intermediates => [ssl_test_lib:eddsa_conf()],
                                                                 peer =>  ssl_test_lib:eddsa_conf()},
                                               client_chain => #{root => ssl_test_lib:eddsa_conf(),
                                                                 intermediates => [ssl_test_lib:eddsa_conf()],
                                                                 peer =>  ssl_test_lib:eddsa_conf()}}),
            [{server_config, SOpts},
             {client_config, COpts}] = x509_test:gen_pem_config_files(Conf, filename:join(PrivDir, "client_eddsa"),
                                                                      filename:join(PrivDir, "server_eddsa")),

            [{cert_key_alg, eddsa} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, COpts},
                           {server_cert_opts, SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config0))]
                         )];
        false ->
            {skip, "Missing EC crypto support"}
    end;
init_per_group(dsa = Group, Config0) ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(dss, PKAlg) andalso lists:member(dh, PKAlg) andalso
        (ssl_test_lib:openssl_dsa_suites() =/= [])
        andalso (ssl_test_lib:check_sane_openssl_dsa(Config0))
    of
        true ->
            Config = ssl_test_lib:make_dsa_cert(Config0),
            COpts = proplists:get_value(client_dsa_opts, Config),
            SOpts = proplists:get_value(server_dsa_opts, Config),
            %% Make sure dhe_dss* suite is chosen by ssl_test_lib:start_server
            Version = ssl_test_lib:protocol_version(Config),
            Ciphers =  ssl_cert_tests:test_ciphers(fun(dh_dss) ->
                                                           true;
                                                      (dhe_dss) ->
                                                           true;
                                                      (_) ->
                                                           false
                                                   end, Version),
            case Ciphers of
                [_|_] ->
                    [{cert_key_alg, dsa} |
                     lists:delete(cert_key_alg,
                                  [{client_cert_opts, [{ciphers, Ciphers} | COpts]  ++ ssl_test_lib:sig_algs(dsa, Version)},
                                   {server_cert_opts, SOpts} |
                                   lists:delete(server_cert_opts,
                                                lists:delete(client_cert_opts, Config))])];
                [] ->
                    {skip, {no_sup, Group, Version}}
            end;
        false ->
            {skip, "Missing DSS crypto support"}
    end;
init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_protocol_version(GroupName) of
        true  ->
            case ssl_test_lib:check_sane_openssl_version(GroupName, Config) of
                true ->
                    ssl_test_lib:init_per_group_openssl(GroupName, Config);
                false  ->
                    {skip, {atom_to_list(GroupName) ++ " not supported by OpenSSL"}}
            end;
        false ->
            Config
    end.

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

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
client_auth_partial_chain() ->
    ssl_cert_tests:client_auth_partial_chain().
client_auth_partial_chain(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_partial_chain(Config).

%%--------------------------------------------------------------------
client_auth_use_partial_chain() ->
    ssl_cert_tests:client_auth_use_partial_chain().
client_auth_use_partial_chain(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_use_partial_chain(Config).
%%--------------------------------------------------------------------
client_auth_do_not_use_partial_chain() ->
   ssl_cert_tests:client_auth_do_not_use_partial_chain().
client_auth_do_not_use_partial_chain(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_do_not_use_partial_chain(Config).

%%--------------------------------------------------------------------
client_auth_partial_chain_fun_fail() ->
   ssl_cert_tests:client_auth_partial_chain_fun_fail().
client_auth_partial_chain_fun_fail(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_partial_chain_fun_fail(Config).

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
