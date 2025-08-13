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
     {'tlsv1.3', [], [{group, transport_socket}]},
     {transport_socket, [], tls_1_3_protocol_groups()},
     {'tlsv1.2', [], pre_tls_1_3_protocol_groups()},
     {'tlsv1.1', [], pre_tls_1_3_protocol_groups()},
     {'tlsv1', [], pre_tls_1_3_protocol_groups()},
     {'dtlsv1.2', [], pre_tls_1_3_protocol_groups()},
     {'dtlsv1', [], pre_tls_1_3_protocol_groups()},
     {rsa, [parallel], all_version_tests()},
     {ecdsa, [parallel], all_version_tests()},
     {dsa, [parallel], all_version_tests()},
     {rsa_1_3, [parallel], all_version_tests() ++ tls_1_3_tests()},
      %% TODO: Create proper conf of openssl server
      %%++ [unsupported_sign_algo_client_auth,
      %% unsupported_sign_algo_cert_client_auth]},
     {rsa_pss_rsae, [parallel], all_version_tests() ++ tls_1_3_tests()},
     {rsa_pss_pss, [parallel], all_version_tests() ++ tls_1_3_tests()},
     {ecdsa_1_3, [parallel], all_version_tests() ++ tls_1_3_tests()},
     %% Enable test later when we have OpenSSL version that
     %% is able to read EDDSA private key files
     %%{eddsa_1_3, [], all_version_tests() ++ tls_1_3_tests()}
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
     {group, ecdsa},
     {group, dsa}].

tls_1_3_protocol_groups() ->
    [{group, rsa_1_3},
     {group, rsa_pss_rsae},
     {group, rsa_pss_pss},
     {group, ecdsa_1_3},
     %%{group, eddsa_1_3}
     {group, mldsa}
    ].

tls_1_3_tests() ->
    [
     hello_retry_request,
     custom_groups,
     mlkem_groups,
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
init_per_group(openssl_client, Config) ->
    [{client_type, openssl}, {server_type, erlang} | Config];

init_per_group(Group, Config) when Group == rsa;
                                    Group == rsa_1_3 ->
    ssl_cert_tests:rsa_config(Config);
init_per_group(Alg, Config) when Alg == rsa_pss_rsae;
                                 Alg == rsa_pss_pss ->
    case ssl_test_lib:is_sane_openssl_pss(Alg) of
        true ->
            ssl_cert_tests:rsa_pss_config(Alg, Config);
        false ->
            {skip, "Missing RSA PSS support in OpenSSL stack."}
    end;
init_per_group(Group, Config0) when Group == ecdsa;
                                    Group == ecdsa_1_3 ->
    case ssl_test_lib:openssl_ecdsa_suites() =/= [] of
        true ->
            ssl_cert_tests:ecdsa_config(Config0);
        false ->
            {skip, "No ECDSA OpenSSL support"}
    end;
init_per_group(eddsa_1_3, Config0) ->
    ssl_cert_tests:eddsa_config(Config0);
init_per_group(Group, Config) when Group == mldsa ->
    ssl_cert_tests:mldsa_config(Config);
init_per_group(dsa, Config) ->
    ssl_cert_tests:openssl_dsa_config(Config);
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
            ssl_test_lib:init_per_group(GroupName, Config)
    end.

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(mlkem_groups, Config) ->
    ssl_cert_tests:support_kems(Config);
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
