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
-module(ssl_cert_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("ssl_record.hrl").

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
         client_auth_custom_key/0,
         client_auth_custom_key/1,
         client_auth_empty_cert_accepted/0,
         client_auth_empty_cert_accepted/1,
         client_auth_empty_cert_rejected/0,
         client_auth_empty_cert_rejected/1,
         client_auth_no_suitable_chain/0,
         client_auth_no_suitable_chain/1,
         client_auth_use_partial_chain/0,
         client_auth_use_partial_chain/1,
         client_auth_do_not_use_partial_chain/0,
         client_auth_do_not_use_partial_chain/1,
         client_auth_partial_chain_fun_fail/0,
         client_auth_partial_chain_fun_fail/1,
         client_auth_sni/0,
         client_auth_sni/1,
         client_auth_seelfsigned_peer/0,
         client_auth_seelfsigned_peer/1,
         missing_root_cert_no_auth/0,
         missing_root_cert_no_auth/1,
         missing_root_cert_auth/0,
         missing_root_cert_auth/1,
         missing_root_cert_auth_user_verify_fun_accept/0,
         missing_root_cert_auth_user_verify_fun_accept/1,
         missing_root_cert_auth_user_verify_fun_reject/0,
         missing_root_cert_auth_user_verify_fun_reject/1,
         missing_root_cert_auth_user_old_verify_fun_accept/0,
         missing_root_cert_auth_user_old_verify_fun_accept/1,
         verify_fun_always_run_client/0,
         verify_fun_always_run_client/1,
         verify_fun_always_run_server/0,
         verify_fun_always_run_server/1,
         incomplete_chain_auth/0,
         incomplete_chain_auth/1,
         no_chain_client_auth/0,
         no_chain_client_auth/1,
         invalid_signature_client/0,
         invalid_signature_client/1,
         invalid_signature_server/0,
         invalid_signature_server/1,
         critical_extension_auth/0,
         critical_extension_auth/1,
         critical_extension_client_auth/0,
         critical_extension_client_auth/1,
         critical_extension_no_auth/0,
         critical_extension_no_auth/1,
         extended_key_usage_auth/0,
         extended_key_usage_auth/1,
         extended_key_usage_client_auth/0,
         extended_key_usage_client_auth/1,
         cert_expired/0,
         cert_expired/1,
         no_auth_key_identifier_ext/0,
         no_auth_key_identifier_ext/1,
         no_auth_key_identifier_ext_keyEncipherment/0,
         no_auth_key_identifier_ext_keyEncipherment/1,
         unsupported_sign_algo_client_auth/0,
         unsupported_sign_algo_client_auth/1,
         unsupported_sign_algo_cert_client_auth/0,
         unsupported_sign_algo_cert_client_auth/1,
         longer_chain/0,
         longer_chain/1,
         cross_signed_chain/0,
         cross_signed_chain/1,
         expired_root_with_cross_signed_root/0,
         expired_root_with_cross_signed_root/1,
         key_auth_ext_sign_only/0,
         key_auth_ext_sign_only/1,
         hello_retry_request/0,
         hello_retry_request/1,
         custom_groups/0,
         custom_groups/1,
         hello_retry_client_auth/0,
         hello_retry_client_auth/1,
         hello_retry_client_auth_empty_cert_accepted/0,
         hello_retry_client_auth_empty_cert_accepted/1,
         hello_retry_client_auth_empty_cert_rejected/0,
         hello_retry_client_auth_empty_cert_rejected/1,
         basic_rsa_1024/0,
         basic_rsa_1024/1,
         signature_algorithms_bad_curve_secp256r1/0,
         signature_algorithms_bad_curve_secp256r1/1,
         signature_algorithms_bad_curve_secp384r1/0,
         signature_algorithms_bad_curve_secp384r1/1,
         signature_algorithms_bad_curve_secp521r1/0,
         signature_algorithms_bad_curve_secp521r1/1,
         server_certificate_authorities_disabled/0,
         server_certificate_authorities_disabled/1,
         cert_auth_in_first_ca/0,
         cert_auth_in_first_ca/1
         ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.3', [], tls_1_3_protocol_groups()}, 
     {'tlsv1.2', [], tls_1_2_protocol_groups()},
     {'tlsv1.1', [], ssl_protocol_groups()},
     {'tlsv1', [], ssl_protocol_groups()},
     {'dtlsv1.2', [], tls_1_2_protocol_groups()},
     {'dtlsv1', [], ssl_protocol_groups()},
     {rsa, [], all_version_tests() ++ rsa_tests() ++ pre_tls_1_3_rsa_tests() ++ [client_auth_seelfsigned_peer]},
     {ecdsa, [], all_version_tests()},
     {dsa, [], all_version_tests()},
     {rsa_1_3, [], all_version_tests() ++ rsa_tests() ++
          tls_1_3_tests() ++ tls_1_3_rsa_tests() ++ [client_auth_seelfsigned_peer, basic_rsa_1024]},
     {rsa_pss_rsae, [], all_version_tests() ++ tls_1_2_rsa_tests()},
     {rsa_pss_rsae_1_3, [], all_version_tests() ++ rsa_tests() ++ tls_1_3_tests() ++ tls_1_3_rsa_tests()},
     {rsa_pss_pss, [], all_version_tests()},
     {rsa_pss_pss_1_3, [], all_version_tests() ++ rsa_tests() ++ tls_1_3_tests() ++ tls_1_3_rsa_tests()},
     {ecdsa_1_3, [], all_version_tests() ++ tls_1_3_tests() ++
          [signature_algorithms_bad_curve_secp256r1,
           signature_algorithms_bad_curve_secp384r1,
           signature_algorithms_bad_curve_secp521r1]},
     {eddsa_1_3, [], all_version_tests() ++ tls_1_3_tests()}
    ].

ssl_protocol_groups() ->
    [{group, rsa},
     {group, dsa}].

tls_1_2_protocol_groups() ->
    [{group, rsa},
     {group, ecdsa},
     {group, dsa},
     {group, rsa_pss_rsae},
     {group, rsa_pss_pss}
    ].

tls_1_3_protocol_groups() ->
    [{group, rsa_1_3},
     {group, ecdsa_1_3},
     {group, eddsa_1_3},
     {group, rsa_pss_rsae_1_3},
     {group, rsa_pss_pss_1_3}
    ].

tls_1_3_tests() ->
    [
     hello_retry_request,
     custom_groups,
     client_auth_no_suitable_chain,
     cert_auth_in_first_ca,
     hello_retry_client_auth,
     hello_retry_client_auth_empty_cert_accepted,
     hello_retry_client_auth_empty_cert_rejected,
     server_certificate_authorities_disabled
    ].

pre_tls_1_3_rsa_tests() ->
    [
     key_auth_ext_sign_only
    ].

rsa_tests() ->
   [
    longer_chain,
    cross_signed_chain,
    expired_root_with_cross_signed_root
   ].

tls_1_3_rsa_tests() ->
     [
      unsupported_sign_algo_client_auth,
      unsupported_sign_algo_cert_client_auth
     ].

tls_1_2_rsa_tests() ->
     [
      unsupported_sign_algo_client_auth,
      unsupported_sign_algo_cert_client_auth
     ].

all_version_tests() ->
    [
     no_auth,
     auth,
     client_auth_custom_key,
     client_auth_empty_cert_accepted,
     client_auth_empty_cert_rejected,
     client_auth_use_partial_chain,
     client_auth_do_not_use_partial_chain,
     client_auth_partial_chain_fun_fail,
     client_auth_sni,
     missing_root_cert_no_auth,
     missing_root_cert_auth,
     missing_root_cert_auth_user_verify_fun_accept,
     missing_root_cert_auth_user_verify_fun_reject,
     missing_root_cert_auth_user_old_verify_fun_accept,
     verify_fun_always_run_client,
     verify_fun_always_run_server,
     incomplete_chain_auth,
     no_chain_client_auth,
     invalid_signature_client,
     invalid_signature_server,
     critical_extension_auth,
     critical_extension_client_auth,
     critical_extension_no_auth,
     extended_key_usage_auth,
     extended_key_usage_client_auth,
     cert_expired,
     no_auth_key_identifier_ext,
     no_auth_key_identifier_ext_keyEncipherment
    ].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:unload(ssl),
    application:stop(crypto).


init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_protocol_version(GroupName) of
        true  ->
            ssl_test_lib:clean_start(),
            ssl_test_lib:init_per_group(GroupName,
                                        [{client_type, erlang},
                                         {server_type, erlang},
                                         {version, GroupName} | Config]);
        false ->
            do_init_per_group(GroupName, Config)
    end.

do_init_per_group(Group, Config0) when Group == rsa;
                                       Group == rsa_1_3 ->
    Config1 = ssl_test_lib:make_rsa_cert(Config0),
    Config = ssl_test_lib:make_rsa_1024_cert(Config1),
    COpts = proplists:get_value(client_rsa_verify_opts, Config),
    SOpts = proplists:get_value(server_rsa_opts, Config),
    Version = proplists:get_value(version, Config),
    [{cert_key_alg, rsa},
     {extra_client, ssl_test_lib:sig_algs(rsa, Version)},
     {extra_server, ssl_test_lib:sig_algs(rsa, Version)} |
     lists:delete(cert_key_alg,
                  [{client_cert_opts, COpts},
                   {server_cert_opts, SOpts} |
                   lists:delete(server_cert_opts,
                                lists:delete(client_cert_opts, Config))])];
do_init_per_group(Alg, Config) when Alg == rsa_pss_rsae;
                                    Alg == rsa_pss_pss ->
    Supports = crypto:supports(),
    RSAOpts = proplists:get_value(rsa_opts, Supports),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),

    case lists:member(rsa_pkcs1_pss_padding, RSAOpts)
        andalso lists:member(rsa_pss_saltlen, RSAOpts)
        andalso lists:member(rsa_mgf1_md, RSAOpts) of
        true ->
            #{client_config := COpts,
              server_config := SOpts} = ssl_test_lib:make_rsa_pss_pem(rsa_alg(Alg), [], Config, ""),
            [{cert_key_alg, Alg},
             {extra_client, ssl_test_lib:sig_algs(Alg, Version)},
             {extra_server, ssl_test_lib:sig_algs(Alg, Version)} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, COpts},
                           {server_cert_opts, SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        false ->
            {skip, "Missing EC crypto support"}
    end;
do_init_per_group(Alg, Config) when Alg == rsa_pss_rsae_1_3;
                                    Alg == rsa_pss_pss_1_3 ->

    Supports = crypto:supports(),
    RSAOpts = proplists:get_value(rsa_opts, Supports),
    
    case lists:member(rsa_pkcs1_pss_padding, RSAOpts) 
        andalso lists:member(rsa_pss_saltlen, RSAOpts) 
        andalso lists:member(rsa_mgf1_md, RSAOpts) of
        true ->
            #{client_config := COpts,
              server_config := SOpts} = ssl_test_lib:make_rsa_pss_pem(rsa_alg(Alg), [], Config, ""),
            [{cert_key_alg, rsa_alg(Alg)} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, COpts},
                           {server_cert_opts, SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        false ->
            {skip, "Missing EC crypto support"}
    end;
do_init_per_group(Group, Config0) when Group == ecdsa;
                                       Group == ecdsa_1_3 ->

    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso (lists:member(ecdh, PKAlg) orelse lists:member(dh, PKAlg)) of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_verify_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            [{cert_key_alg, ecdsa} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, COpts},
                           {server_cert_opts, SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))]
                         )];
        false ->
            {skip, "Missing EC crypto support"}
    end;
do_init_per_group(eddsa_1_3, Config0) ->
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
do_init_per_group(dsa = Alg, Config0) ->
    PKAlg = crypto:supports(public_keys),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config0)),
    case lists:member(dss, PKAlg) andalso lists:member(dh, PKAlg) of
        true ->
            Config = ssl_test_lib:make_dsa_cert(Config0),
            COpts = proplists:get_value(client_dsa_opts, Config),
            SOpts = proplists:get_value(server_dsa_opts, Config),
            [{cert_key_alg, dsa},
             {extra_client, ssl_test_lib:sig_algs(Alg, Version) ++
                  [{ciphers, ssl_test_lib:dsa_suites(Version)}]},
             {extra_server, ssl_test_lib:sig_algs(Alg, Version) ++
                  [{ciphers, ssl_test_lib:dsa_suites(Version)}]} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, COpts},
                           {server_cert_opts, SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        false ->
            {skip, "Missing DSS crypto support"}
    end;
do_init_per_group(_Group, Config) ->
    Config.

end_per_group(GroupName, Config) ->
  ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(signature_algorithms_bad_curve_secp256r1, Config) ->
    init_ecdsa_opts(Config, secp256r1);
init_per_testcase(signature_algorithms_bad_curve_secp384r1, Config) ->
    init_ecdsa_opts(Config, secp384r1);
init_per_testcase(signature_algorithms_bad_curve_secp521r1, Config) ->
    init_ecdsa_opts(Config, secp521r1);
init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

init_ecdsa_opts(Config0, Curve) ->
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config0)),
    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso (lists:member(ecdh, PKAlg) orelse lists:member(dh, PKAlg)) of
        true ->
            Config = ssl_test_lib:make_rsa_ecdsa_cert(Config0, Curve),
            COpts = proplists:get_value(client_ecdsa_verify_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            [{cert_key_alg, ecdsa} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, ssl_test_lib:sig_algs(ecdsa, Version) ++ COpts},
                           {server_cert_opts, ssl_test_lib:sig_algs(ecdsa, Version) ++ SOpts} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))]
                         )];
        false ->
            {skip, "Missing EC crypto support"}
    end.

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
client_auth_custom_key() ->
    ssl_cert_tests:client_auth_custom_key().
client_auth_custom_key(Config) ->
    ssl_cert_tests:client_auth_custom_key(Config).
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
client_auth_no_suitable_chain() ->
    ssl_cert_tests:client_auth_no_suitable_chain().
client_auth_no_suitable_chain(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_no_suitable_chain(Config).

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
client_auth_sni() ->
   ssl_cert_tests:client_auth_sni().
client_auth_sni(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_sni(Config).
%%--------------------------------------------------------------------
client_auth_seelfsigned_peer() ->
   ssl_cert_tests:client_auth_seelfsigned_peer().
client_auth_seelfsigned_peer(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_seelfsigned_peer(Config).

%%--------------------------------------------------------------------
missing_root_cert_no_auth() ->
   ssl_cert_tests:missing_root_cert_no_auth().
missing_root_cert_no_auth(Config) when is_list(Config) ->
    ssl_cert_tests:missing_root_cert_no_auth(Config).

%%--------------------------------------------------------------------
missing_root_cert_auth() ->
    [{doc,"Must have ROOT certs to be able to verify verify peer"}].
missing_root_cert_auth(Config) when is_list(Config) ->
    ServerOpts =  proplists:delete(cacertfile, ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config)),
    {ClientNode, ServerNode, _} = ssl_test_lib:run_where(Config),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, no_reuse(Version) ++ [{verify, verify_peer}
                                                                             | ServerOpts]}]),

    Error = {error, {options, incompatible,
                     [{verify,verify_peer},{cacerts,undefined}]}},
    ssl_test_lib:check_result(Server, Error),
    
    ClientOpts =  proplists:delete(cacertfile, ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config)),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, 0},
					      {from, self()},
					      {options, [{verify, verify_peer}
							 | ClientOpts]}]),

    ssl_test_lib:check_result(Client, Error).
    
%%--------------------------------------------------------------------
missing_root_cert_auth_user_verify_fun_accept() ->
    [{doc, "Test that the client succeeds if the ROOT CA is unknown in verify_peer mode"
     " with a verify_fun that accepts the unknown CA error"}].

missing_root_cert_auth_user_verify_fun_accept(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    ClientCaCerts = public_key:cacerts_get(),
    FunAndState =  {fun(_,{bad_cert, unknown_ca}, UserState) ->
			    {valid, UserState};
		       (_,{bad_cert, _} = Reason, _) ->
			    {fail, Reason};
		       (_,{extension, _}, UserState) ->
			    {unknown, UserState};
		       (_, valid, UserState) ->
			    {valid, UserState};
		       (_, valid_peer, UserState) ->
			    {valid, UserState}
		    end, []},
    ClientOpts = ssl_test_lib:ssl_options(extra_client,
                                          [{verify, verify_peer}, {verify_fun, FunAndState},
                                           {cacerts, ClientCaCerts}],
                                          Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
missing_root_cert_auth_user_old_verify_fun_accept() ->
    [{doc, "Test old style verify fun"}].

missing_root_cert_auth_user_old_verify_fun_accept(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    ClientCaCerts = public_key:cacerts_get(),
    AcceptBadCa = fun({bad_cert,unknown_ca}, Acc) ->  Acc;
                     (Other, Acc) -> [Other | Acc]
		  end,
    VerifyFun =
	fun(ErrorList) ->
		case lists:foldl(AcceptBadCa, [], ErrorList) of
		    [] ->    true;
		    [_|_] -> false
		end
	end,
    ClientOpts = ssl_test_lib:ssl_options(extra_client,
                                          [{verify, verify_peer},
                                           {verify_fun, VerifyFun},
                                           {cacerts, ClientCaCerts}], Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
missing_root_cert_auth_user_verify_fun_reject() ->
    [{doc, "Test that the client fails if the ROOT CA is unknown in verify_peer mode"
     " with a verify_fun that rejects the unknown CA error"}].

missing_root_cert_auth_user_verify_fun_reject(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    ClientCaCerts = public_key:cacerts_get(),
    FunAndState =  {fun(_,{bad_cert, unknown_ca} = Reason, _UserState) ->
			    {fail, Reason};
		       (_,{bad_cert, _} = Reason, _) ->
			    {fail, Reason};
		       (_,{extension, UserState}, _) ->
			    {unknown, UserState};
		       (_, valid, UserState) ->
			    {valid, UserState};
		       (_, valid_peer, UserState) ->
			    {valid, UserState}
		    end, []},
    ClientOpts = ssl_test_lib:ssl_options(extra_client,
                                          [{verify, verify_peer},
                                           {verify_fun, FunAndState},
                                           {cacerts, ClientCaCerts}],
                                          Config),
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, unknown_ca).


%%--------------------------------------------------------------------
incomplete_chain_auth() ->
    [{doc,"Test that we can verify an incompleat chain when we have the certs to rebuild it"}].
incomplete_chain_auth(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Group = proplists:get_value(name, Prop),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(Group),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain, DefaultCertConf},
                                                                         {client_chain, DefaultCertConf}]),   
    [ServerRoot| _] = ServerCas = proplists:get_value(cacerts, ServerOpts0),
    ClientCas = proplists:get_value(cacerts, ClientOpts0),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, [{verify, verify_peer},
                                                         {cacerts,  ServerCas ++ ClientCas} |
                                                         proplists:delete(cacerts, ClientOpts0)], Config),
    ServerOpts = ssl_test_lib:ssl_options(extra_server, [{verify, verify_peer},
                                                         {cacerts, [ServerRoot]} |
                                                         proplists:delete(cacerts, ServerOpts0)], Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
no_chain_client_auth() ->
    [{doc,"In TLS-1.3 test that we allow sending only peer cert if chain CAs are missing and hence"
      " we can not determine if client is in servers auth domain or not, so send and hope"
      " that the cert chain is in the auth domain and that the server possess "
      " intermediates to recreate the chain."}].
no_chain_client_auth(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Group = proplists:get_value(name, Prop),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(Group),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain, DefaultCertConf},
                                                                         {client_chain, DefaultCertConf}]),
    ServerCas = proplists:get_value(cacerts, ServerOpts0),
    [ClientRoot| _] = ClientCas = proplists:get_value(cacerts, ClientOpts0),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, [{verify, verify_peer},
                                                         {cacerts, [ClientRoot]} |
                                                         proplists:delete(cacerts, ClientOpts0)], Config),
    ServerOpts = ssl_test_lib:ssl_options(extra_server, [{verify, verify_peer},
                                                         {fail_if_no_peer_cert, true},
                                                         {cacerts,  ClientCas ++ ServerCas} |
                                                         proplists:delete(cacerts, ServerOpts0)], Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
verify_fun_always_run_client() ->
    [{doc,"Verify that user verify_fun is always run (for valid and "
      "valid_peer not only unknown_extension)"}].

verify_fun_always_run_client(Config) when is_list(Config) ->
    ClientOpts =  ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = proplists:get_value(version, Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options, no_reuse(ssl_test_lib:n_version(Version)) ++ ServerOpts}]),
    Port  = ssl_test_lib:inet_port(Server),

    %% If user verify fun is called correctly we fail the connection.
    %% otherwise we cannot tell this case apart form where we miss
    %% to call users verify fun
    FunAndState =  {fun(_, Der, {extension, _}, UserState) ->
                            true = is_binary(Der),
			    {unknown, UserState};
		       (_, Der, valid, [ChainLen]) ->
                            true = is_binary(Der),
			    {valid, [ChainLen + 1]};
		       (_, Der, valid_peer, [1]) ->
                            true = is_binary(Der),
			    {fail, "verify_fun_was_always_run"};
		       (_, Der, valid_peer, UserState) ->
                            true = is_binary(Der),
			    {valid, UserState}
		    end, [0]},

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options,
					       [{verify, verify_peer},
						{verify_fun, FunAndState}
						| ClientOpts]}]),

    ssl_test_lib:check_client_alert(Server, Client, handshake_failure).

%%--------------------------------------------------------------------
verify_fun_always_run_server() ->
    [{doc,"Verify that user verify_fun is always run (for valid and "
      "valid_peer not only unknown_extension)"}].
verify_fun_always_run_server(Config) when is_list(Config) ->
    ClientOpts =  ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% If user verify fun is called correctly we fail the connection.
    %% otherwise we cannot tell this case apart form where we miss
    %% to call users verify fun
    FunAndState =  {fun(_, Der, {extension, _}, UserState) ->
                            true = is_binary(Der),
			    {unknown, UserState};
		       (_, Der, valid, [ChainLen]) ->
                            true = is_binary(Der),
			    {valid, [ChainLen + 1]};
		       (_, Der, valid_peer, [1]) ->
                            true = is_binary(Der),
			    {fail, "verify_fun_was_always_run"};
		       (_, Der, valid_peer, UserState) ->
                            true = is_binary(Der),
			    {valid, UserState}
		    end, [0]},

    Version = proplists:get_value(version, Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options,
                                               no_reuse(ssl_test_lib:n_version(Version)) ++ [{verify, verify_peer},
                                                                                {verify_fun, FunAndState} |
                                                                                ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options, ClientOpts}]),
    
    ssl_test_lib:check_client_alert(Server, Client, handshake_failure).

%%--------------------------------------------------------------------
invalid_signature_client() ->
    ssl_cert_tests:invalid_signature_client().
invalid_signature_client(Config) when is_list(Config) ->
    ssl:clear_pem_cache(),
    ssl_cert_tests:invalid_signature_client(Config).
%%--------------------------------------------------------------------
invalid_signature_server() ->
    ssl_cert_tests:invalid_signature_server().
invalid_signature_server(Config) when is_list(Config) ->
    ssl:clear_pem_cache(),
    ssl_cert_tests:invalid_signature_server(Config).

%%--------------------------------------------------------------------
critical_extension_auth() ->
    [{doc,"Test cert that has a critical unknown extension in verify_peer mode"}].

critical_extension_auth(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(proplists:get_value(name, Prop)),
    Ext = x509_test:extensions([{{2,16,840,1,113730,1,1}, <<3,2,6,192>>, true}]),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0}  = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                         [{server_chain, 
                                                                           [[],[],[{extensions, Ext}]]},
                                                                          {client_chain, DefaultCertConf}]),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config),
 
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = proplists:get_value(version, Config),
    Server = ssl_test_lib:start_server_error(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {ssl_test_lib,  no_result, []}},
                {options, no_reuse(ssl_test_lib:n_version(Version)) ++ [{verify, verify_none} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error(
               [{node, ClientNode}, {port, Port},
                {host, Hostname},
                {from, self()},
                {mfa, {ssl_test_lib, no_result, []}},
                {options, [{verify, verify_peer} | ClientOpts]}]),

    ssl_test_lib:check_client_alert(Server, Client, unsupported_certificate).

%%--------------------------------------------------------------------
critical_extension_client_auth() ->
    [{doc,"Test cert that has a critical unknown extension in verify_peer mode"}].

critical_extension_client_auth(Config) when is_list(Config) ->
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
    Ext = x509_test:extensions([{{2,16,840,1,113730,1,1}, <<3,2,6,192>>, true}]),
     #{client_config := ClientOpts0,
      server_config := ServerOpts0}  = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                         [{client_chain, 
                                                                           [[],[],[{extensions, Ext}]]},
                                                                          {server_chain, DefaultCertConf}]),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config),
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = proplists:get_value(version, Config),
    Server = ssl_test_lib:start_server_error(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {ssl_test_lib, no_result, []}},
                {options, no_reuse(ssl_test_lib:n_version(Version)) ++ [{verify, verify_peer} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error(
               [{node, ClientNode}, {port, Port},
                {host, Hostname},
                {from, self()},
                {mfa, {ssl_test_lib, no_result, []}},
                {options, [{verify, verify_none} | ClientOpts]}]),

    %% This certificate has a critical extension that we don't
    %% understand.  Therefore, verification should fail.          
    ssl_test_lib:check_server_alert(Server, Client, unsupported_certificate).

%%--------------------------------------------------------------------
critical_extension_no_auth() ->
    [{doc,"Test cert that has a critical unknown extension in verify_none mode"}].

critical_extension_no_auth(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(proplists:get_value(name, Prop)),
    Ext = x509_test:extensions([{{2,16,840,1,113730,1,1}, <<3,2,6,192>>, true}]),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0}  = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                            [{server_chain, 
                                                                              [[],[], [{extensions, Ext}]]},
                                                                             {client_chain, DefaultCertConf}]),
    ClientOpts = [{verify, verify_none} | ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config)],
    ServerOpts = [{verify, verify_none} | ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config)],
    
     ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).


%%--------------------------------------------------------------------
extended_key_usage_auth() ->
    [{doc,"Test cert that has a critical extended_key_usage extension in server cert"}].

extended_key_usage_auth(Config) when is_list(Config) -> 
    Prop = proplists:get_value(tc_group_properties, Config),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(proplists:get_value(name, Prop)),
    Ext = x509_test:extensions([{?'id-ce-extKeyUsage',
                                 [?'id-kp-serverAuth'], true}]),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain, 
                                                                          [[],[], [{extensions, Ext}]]},
                                                                         {client_chain, DefaultCertConf}
                                                                        ]),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = proplists:get_value(version, Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
			   {options, no_reuse(ssl_test_lib:n_version(Version)) ++ [{verify, verify_none} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{verify, verify_peer} |
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
extended_key_usage_client_auth() ->
    [{doc,"Test cert that has a critical extended_key_usage extension in client and server cert"}].

extended_key_usage_client_auth(Config) when is_list(Config) ->
    ServerExt = x509_test:extensions([{?'id-ce-extKeyUsage',
                                       [?'id-kp-serverAuth'], true}]),
    ClientExt = x509_test:extensions([{?'id-ce-extKeyUsage',
                                       [?'id-kp-clientAuth'], true}]),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{client_chain, [[],[],[{extensions, ClientExt}]]},
                                                                         {server_chain, [[],[],[{extensions, ServerExt}]]}]),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config),
   
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = proplists:get_value(version, Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, no_reuse(ssl_test_lib:n_version(Version)) ++ [{verify, verify_peer} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{verify, verify_peer} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
cert_expired() ->
    [{doc,"Test server with expired certificate"}].

cert_expired(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(proplists:get_value(name, Prop)),
    {Year, Month, Day} = date(),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain,
                                                                          [[],
                                                                           [{validity, {{Year-2, Month, Day},
                                                                                        {Year-1, Month, Day}}}],
                                                                           []
                                                                     ]},
                                                                         {client_chain, DefaultCertConf}]),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config),
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = proplists:get_value(version, Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, no_reuse(ssl_test_lib:n_version(Version)) ++ ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {options, [{verify, verify_peer} | ClientOpts]}]),    
    
    ssl_test_lib:check_client_alert(Server, Client, certificate_expired).

%%--------------------------------------------------------------------
no_auth_key_identifier_ext() ->
    [{doc, "Test cert that does not have authorityKeyIdentifier extension"}].

no_auth_key_identifier_ext(Config) when is_list(Config) ->
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
      #{client_config := ClientOpts0,
        server_config := ServerOpts0} = 
        ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                          [{client_chain, DefaultCertConf},
                                           {server_chain, DefaultCertConf}]),
    ClientOpts = [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config)],
    ServerOpts = [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config)],
 
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
    
%%--------------------------------------------------------------------
no_auth_key_identifier_ext_keyEncipherment() ->
    [{doc, "Test cert with keyEncipherment key_usage an no"
      " authorityKeyIdentifier extension"}].

no_auth_key_identifier_ext_keyEncipherment(Config) when is_list(Config) ->
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
    ClientExt = x509_test:extensions([{key_usage, [digitalSignature, keyEncipherment]}]),
    #{client_config := ClientOpts0,
        server_config := ServerOpts0} = 
        ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                          [{client_chain, 
                                            [[],[],[{extensions, ClientExt}]]},
                                           {server_chain, DefaultCertConf}
                                          ]),
    ClientOpts =   [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config)],
    ServerOpts =   [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
key_auth_ext_sign_only() ->
    [{doc, "Test that client with a certificate without keyEncipherment usage "
    " extension can connect to a server with restricted cipher suites "}].
key_auth_ext_sign_only(Config) when is_list(Config) ->
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
    ClientExt = x509_test:extensions([{key_usage, [digitalSignature]}]),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = 
        ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                          [{client_chain, 
                                            [[],[],[{extensions, ClientExt}]]},
                                           {server_chain, DefaultCertConf}
                                          ]),
    Version = proplists:get_value(version, Config),
    ClientOpts =  [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config)],
    ServerOpts = [{verify, verify_peer}, {ciphers, 
                                          ssl_test_lib:rsa_non_signed_suites(ssl_test_lib:n_version(Version))} 
                 | ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
cert_auth_in_first_ca() ->
    [{doc,"Test cert auth will be available in first ca in chain, make it happen by only having one"}].
cert_auth_in_first_ca(Config) when is_list(Config) ->
    #{server_config := ServerOpts0,
      client_config := ClientOpts0} =
        public_key:pkix_test_data(#{server_chain => #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                                      intermediates => [[]],
                                                      peer => [{key, ssl_test_lib:hardcode_rsa_key(5)}]},
                                    client_chain => #{root => [{key, ssl_test_lib:hardcode_rsa_key(3)}], 
                                                      intermediates => [[]],
                                                      peer => [{key, ssl_test_lib:hardcode_rsa_key(1)}]}}), 
    ClientOpts = [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config)],
    ServerOpts =  [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config)],

    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------


longer_chain() ->
    [{doc,"Test depth option"}].
longer_chain(Config) when is_list(Config) ->
    #{server_config := ServerOpts0,
      client_config := ClientOpts0} =
        public_key:pkix_test_data(#{server_chain => #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                                      intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}],
                                                                        [{key, ssl_test_lib:hardcode_rsa_key(3)}],
                                                                        [{key, ssl_test_lib:hardcode_rsa_key(4)}]],
                                                      peer => [{key, ssl_test_lib:hardcode_rsa_key(5)}]},
                                    client_chain => #{root => [{key, ssl_test_lib:hardcode_rsa_key(3)}],
                                                      intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                                      peer => [{key, ssl_test_lib:hardcode_rsa_key(1)}]}}),
    [ServerRoot| _] = ServerCas = proplists:get_value(cacerts, ServerOpts0),
    ClientCas = proplists:get_value(cacerts, ClientOpts0),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    
    ServerOpts = ssl_test_lib:ssl_options(extra_server, [{verify, verify_peer}, {cacerts, [ServerRoot]} |
                                           proplists:delete(cacerts, ServerOpts0)] ++ ssl_test_lib:sig_algs(rsa, Version), Config),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, [{verify, verify_peer},
                                           {depth, 5},
                                           {cacerts,  ServerCas ++ ClientCas} |
                                           proplists:delete(cacerts, ClientOpts0)]++ ssl_test_lib:sig_algs(rsa, Version) , Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

cross_signed_chain() ->
    [{doc, "Manual test of chain with duplicate entries that is cross signing certs are present."}].
cross_signed_chain(Config)
  when is_list(Config) ->
    Key1 = ssl_test_lib:hardcode_rsa_key(1),
    Key2 = ssl_test_lib:hardcode_rsa_key(2),
    Key3 = ssl_test_lib:hardcode_rsa_key(3),
    Key4 = ssl_test_lib:hardcode_rsa_key(4),
    Key5 = ssl_test_lib:hardcode_rsa_key(5),

    #{server_config := ServerOpts0, client_config := ClientOpts0} =
        public_key:pkix_test_data(#{server_chain => #{root => [{key, Key1}],
                                                      peer => [{key, Key5}]},
                                    client_chain => #{root => [{key, Key3}],
                                                      intermediates => [[{key, Key2}], [{key, Key3}]],
                                                      peer => [{key, Key1}]}}),

    #{client_config := ClientOptsNew} =
        public_key:pkix_test_data(#{server_chain => #{root => [{key, Key1}],
                                                      peer => [{key, Key5}]},
                                    client_chain => #{root => [{key, Key4}],
                                                      intermediates => [[{key, Key2}], [{key, Key1}]],
                                                      peer => [{key, Key1}]}}),

    ServerCas0 = proplists:get_value(cacerts, ServerOpts0),
    ClientCas0 = proplists:get_value(cacerts, ClientOpts0),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),

    {[Peer,CI1,CI2,CROld], CROld} = chain_and_root(ClientOpts0),
    {[_Peer,CI1New,CI2New,CRNew], CRNew} = chain_and_root(ClientOptsNew),

    ServerCas = [CRNew|ServerCas0 -- [CROld]],
    ServerOpts = ssl_test_lib:ssl_options(extra_server, [{verify, verify_peer} |
                                                         lists:keyreplace(cacerts, 1, ServerOpts0, {cacerts, ServerCas})]
                                          ++ ssl_test_lib:sig_algs(rsa, Version),
                                          Config),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, [{verify, verify_peer} |
                                                         lists:keyreplace(cacerts, 1,
                                                                          lists:keyreplace(cert, 1, ClientOpts0,
                                                                             {cert, [Peer,CI1New,CI2New,CI1,CI2,CRNew,CROld]}),
                                                                          {cacerts, ClientCas0})] ++ ssl_test_lib:sig_algs(rsa, Version),
                                          Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config),
    ClientOpts2 = ssl_test_lib:ssl_options(extra_client, [{verify, verify_peer} |
                                                          lists:keyreplace(cacerts, 1,
                                                                           lists:keyreplace(cert, 1, ClientOpts0,
                                                                                            {cert, [Peer,CI1,CI1New,CI2,CI2New,CROld,CRNew]}),
                                                                           {cacerts, ClientCas0})] ++ ssl_test_lib:sig_algs(rsa, Version),
                                           Config),
    ssl_test_lib:basic_test(ClientOpts2, ServerOpts, Config),
    ok.

expired_root_with_cross_signed_root() ->
    [{doc,"Test that we can verify a chain with an expired Root Cert if there is an alternative chain with"
      " a cross signed Root CA further down the chain that is however not present in the sent chain"}].
expired_root_with_cross_signed_root(Config) when is_list(Config) ->

    Key1 = ssl_test_lib:hardcode_rsa_key(1),
    Key2 = ssl_test_lib:hardcode_rsa_key(2),
    Key3 = ssl_test_lib:hardcode_rsa_key(3),
    Key4 = ssl_test_lib:hardcode_rsa_key(4),
    Key5 = ssl_test_lib:hardcode_rsa_key(5),
    Key6 = ssl_test_lib:hardcode_rsa_key(6),
    {Year, Month, Day} = date(),

    %% Create expired ROOT
    #{cert := Root} = SRoot = public_key:pkix_test_root_cert("OTP test server ROOT", [{key, Key1},
                                                                    {validity, {{Year-2, Month, Day},
                                                                                {Year-1, Month, Day}}}]),
    #{server_config := ServerOpts0, client_config := ClientOpts0} =
        public_key:pkix_test_data(#{server_chain => #{root => SRoot,
                                                      intermediates => [[{key, Key2}], [{key, Key3}]],
                                                      peer => [{key, Key4}]},
                                    client_chain => #{root => [{key, Key5}],
                                                      peer => [{key, Key6}]}}),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ClientOpts = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,
    ServerOpts = ssl_test_lib:sig_algs(rsa, Version) ++ ServerOpts0,

    SCert = proplists:get_value(cert, ServerOpts),
    SCerts = proplists:get_value(cacerts, ServerOpts),

    {ok, ExtractedCAs} = ssl_pkix_db:extract_trusted_certs({der, SCerts}),
    {ok, Root, [_Peer, CA1, CA2, Root]} = ssl_certificate:certificate_chain(SCert, ets:new(foo, []), 
                                                                            ExtractedCAs, [], encoded),

    OTPCA1 = public_key:pkix_decode_cert(CA1, otp),
    OTPCA2 = public_key:pkix_decode_cert(CA2, otp),

    TBS1 = OTPCA1#'OTPCertificate'.tbsCertificate,
    TBS2 = OTPCA2#'OTPCertificate'.tbsCertificate,

    Issuer = TBS1#'OTPTBSCertificate'.issuer,

    SubjectPublicKeyInfo = TBS2#'OTPTBSCertificate'.subjectPublicKeyInfo,

    AltCrossRoot = public_key:pkix_sign(TBS1#'OTPTBSCertificate'{subject = Issuer,
                                                                 subjectPublicKeyInfo = SubjectPublicKeyInfo}, Key2),

    ClientCas0 = proplists:get_value(cacerts, ClientOpts),

    %% Only expired ROOT present
    ssl_test_lib:basic_alert([{verify, verify_peer} | ClientOpts], ServerOpts, Config, certificate_expired),
    %% Only CROSS ROOT present
    ssl_test_lib:basic_test([{verify, verify_peer},
                             {cacerts, [AltCrossRoot]} | proplists:delete(cacerts, ClientOpts)], ServerOpts, Config),
    %% Both expired ROOT and CROSS ROOT present
    ssl_test_lib:basic_test([{verify, verify_peer},
                             {cacerts, [AltCrossRoot | ClientCas0]} | proplists:delete(cacerts, ClientOpts)],
                            ServerOpts, Config).

%%--------------------------------------------------------------------
%% TLS 1.3 Test cases  -----------------------------------------------
%%--------------------------------------------------------------------
hello_retry_request() ->
    [{doc,"Test that ssl server can request a new group when the client's first key share"
      "is not supported"}].

hello_retry_request(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [x448, x25519]}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [secp256r1, x25519]} | ClientOpts0],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
custom_groups() ->
    [{doc,"Test that ssl server can select a common group for key-exchange"}].

custom_groups(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),

    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [x448, secp256r1, secp384r1]}|ServerOpts0],
    ClientOpts1 = [{versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ClientOpts = [{supported_groups,[secp384r1, secp256r1, x25519]}|ClientOpts1],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
%% Triggers a Server Alert as ssl client does not have a certificate with a
%% signature algorithm supported by the server (signature_algorithms_cert extension
%% of CertificateRequest does not contain the algorithm of the client certificate).
%% ssl client sends an empty certificate.
unsupported_sign_algo_cert_client_auth() ->
     [{doc,"TLS 1.3 (backported to TLS-1.2) : Test client authentication with unsupported signature_algorithm_cert"}].

unsupported_sign_algo_cert_client_auth(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    ServerOpts = [{verify, verify_peer},
                  {signature_algs, [rsa_pkcs1_sha256, rsa_pkcs1_sha384, rsa_pss_rsae_sha256, rsa_pss_pss_sha256]},
                  %% Skip rsa_pkcs1_sha256!
                  {signature_algs_cert, [rsa_pkcs1_sha384, rsa_pkcs1_sha512]},
                  {fail_if_no_peer_cert, true}|ServerOpts0],
    Version = proplists:get_value(version, Config),
    case Version of
        'tlsv1.3' ->
            ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_required);
        _  ->
            ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, bad_certificate)
    end.

%%--------------------------------------------------------------------
unsupported_sign_algo_client_auth() ->
     [{doc,"TLS 1.3 (backported to TLS-1.2): Test client authentication with unsupported signature_algorithm"}].

unsupported_sign_algo_client_auth(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    ClientOpts = [{signature_algs, [rsa_pkcs1_sha256, rsa_pss_rsae_sha256]} | ClientOpts0],
    ServerOpts = [{verify, verify_peer},
                  {signature_algs, [ecdsa_sha1, rsa_pss_pss_sha256]},
                  {fail_if_no_peer_cert, true}|ServerOpts0],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, insufficient_security).
%%--------------------------------------------------------------------
hello_retry_client_auth() ->
    [{doc, "TLS 1.3 (HelloRetryRequest): Test client authentication."}].

hello_retry_client_auth(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    ServerOpts1 = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [x448, x25519]}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [secp256r1, x25519]}|ClientOpts0],
    ServerOpts = [{verify, verify_peer},
                  {fail_if_no_peer_cert, true} | ServerOpts1],

    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
hello_retry_client_auth_empty_cert_accepted() ->
     [{doc,"TLS 1.3 (HelloRetryRequest): Test client authentication when client sends an empty " 
       "certificate and fail_if_no_peer_cert is set to true."}].

hello_retry_client_auth_empty_cert_accepted(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    %% Delete Client Cert and Key
    ClientOpts1 = proplists:delete(certfile, ClientOpts0),
    ClientOpts2 = proplists:delete(keyfile, ClientOpts1),

    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, false},
                  {supported_groups, [x448, x25519]}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [secp256r1, x25519]}|ClientOpts2],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
hello_retry_client_auth_empty_cert_rejected() ->
     [{doc,"TLS 1.3 (HelloRetryRequest): Test client authentication when client "
       "sends an empty certificate and fail_if_no_peer_cert is set to true."}].

hello_retry_client_auth_empty_cert_rejected(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    %% Delete Client Cert and Key
    ClientOpts1 = proplists:delete(certfile, ClientOpts0),
    ClientOpts2 = proplists:delete(keyfile, ClientOpts1),

    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true},
                  {supported_groups, [x448, x25519]}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [secp256r1, x25519]}|ClientOpts2],
   
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_required).

%%--------------------------------------------------------------------
signature_algorithms_bad_curve_secp256r1() ->
     [{doc,"TLS 1.3: Test that the the client fails to connect "
       "if server's certificate has a key using an unsupported curve."}].

signature_algorithms_bad_curve_secp256r1(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']} | ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {signature_algs, [ecdsa_secp384r1_sha384,
                                    ecdsa_secp521r1_sha512,
                                    {sha256,rsa}]}|ClientOpts0],

    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, insufficient_security).

%%--------------------------------------------------------------------
signature_algorithms_bad_curve_secp384r1() ->
     [{doc,"TLS 1.3: Test that the the client fails to connect "
       "if server's certificate has a key using an unsupported curve."}].

signature_algorithms_bad_curve_secp384r1(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']} | ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {signature_algs, [ecdsa_secp256r1_sha256,
                                    ecdsa_secp521r1_sha512,
                                    {sha256,rsa}]}|ClientOpts0],

    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, insufficient_security).

%%--------------------------------------------------------------------
signature_algorithms_bad_curve_secp521r1() ->
     [{doc,"TLS 1.3: Test that the the client fails to connect "
       "if server's certificate has a key using an unsupported curve."}].

signature_algorithms_bad_curve_secp521r1(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {signature_algs, [ecdsa_secp512r1_sha256,
                                    {sha256,rsa}]}
                 | ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {signature_algs, [ecdsa_secp256r1_sha256,
                                    ecdsa_secp384r1_sha384,
                                    {sha256,rsa}]}|ClientOpts0],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, insufficient_security).

%%--------------------------------------------------------------------
basic_rsa_1024() ->
    [{doc, "TLS 1.3 (Basic): Test if connection can be established using 1024 bits RSA keys in certificates."}].

basic_rsa_1024(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_1024_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_1024_opts, Config),
    ServerOpts1 = [{versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ServerOpts = [{verify, verify_peer},
                  {fail_if_no_peer_cert, true} | ServerOpts1],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
server_certificate_authorities_disabled() ->
     [{doc,"TLS 1.3: Disabling certificate_authorities extension on the server when verify_peer is set to true"
       " allows the client to send a chain that could be verifiable by the server but that would not adhere to"
       " the certificate_authorities extension as it is not part of the regular trusted certificate set"}].

server_certificate_authorities_disabled(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),

    % Strip out the ClientRoot to simulate cases where the they are manually managed and
    % not expected to be included in certificate requests during mutual authentication.
    {ok, CACerts0} = ssl_pkix_db:decode_pem_file(proplists:get_value(cacertfile, ServerOpts0)),
    [_ClientRoot | ServerCACerts] = [CertDER || {_, CertDER, _} <- CACerts0],

    FunAndState =  {fun(_,{extension, _}, UserState) ->
                            {unknown, UserState};
                       (_, valid, UserState) ->
                            {valid, UserState};
                       % Because this is a manually managed setup, we also need to manually verify
                       % an unknown_ca (ClientCert) as expected. Typically you would have custom logic
                       % here to decide if you know the cert (like looking up pinned values in a DB)
                       % but for testing purposes, we'll allow everything
                       (_, {bad_cert, unknown_ca}, UserState) ->
                            {valid, UserState};
                       (_, valid_peer, UserState) ->
                            {valid, UserState}
                    end, [0]},

    ClientOpts = [{versions, ['tlsv1.3']}, {verify, verify_peer} | ClientOpts0],
    ServerOpts = [{versions, ['tlsv1.3']}, {verify, verify_peer},
                  {fail_if_no_peer_cert, true}, {cacerts, ServerCACerts},
                  {verify_fun, FunAndState} | ServerOpts0],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_required),
    ssl_test_lib:basic_test(ClientOpts, [{certificate_authorities, false} | ServerOpts], Config).

%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------
rsa_alg(rsa_pss_rsae_1_3) ->
    rsa_pss_rsae;
rsa_alg(rsa_pss_pss_1_3) ->
    rsa_pss_pss;
rsa_alg(Atom) ->
    Atom.

no_reuse(?TLS_1_3) ->
    [];
no_reuse(_) ->
    [{reuse_sessions, false}].

chain_and_root(Config) ->
    OwnCert = proplists:get_value(cert, Config),
    {ok, ExtractedCAs} = ssl_pkix_db:extract_trusted_certs({der, proplists:get_value(cacerts, Config)}),
    {ok, Root, Chain} = ssl_certificate:certificate_chain(OwnCert, ets:new(foo, []), ExtractedCAs, [], encoded),
    {Chain, Root}.

