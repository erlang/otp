%%
%% %CopyrightBegin%
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
-module(ssl_api_SUITE).

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_api.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("ssl/src/ssl_record.hrl").

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
-export([conf_signature_algs/0,
         conf_signature_algs/1,
         no_common_signature_algs/0,
         no_common_signature_algs/1,
         default_reject_anonymous/0,
         default_reject_anonymous/1,
         connection_information_with_srp/0,
         connection_information_with_srp/1,
         peercert/0,
         peercert/1,
         peercert_with_client_cert/0,
         peercert_with_client_cert/1,
         select_best_cert/0,
         select_best_cert/1,
         select_sha1_cert/0,
         select_sha1_cert/1,
         root_any_sign/0,
         root_any_sign/1,
         connection_information/0,
         connection_information/1,
         secret_connection_info/0,
         secret_connection_info/1,
         keylog_connection_info/0,
         keylog_connection_info/1,
         versions/0,
         versions/1,
         versions_option_based_on_sni/0,
         versions_option_based_on_sni/1,
         ciphers_option_based_on_sni/0,
         ciphers_option_based_on_sni/1,
         active_n/0,
         active_n/1,
         dh_params/0,
         dh_params/1,
         invalid_dhfile/0,
         invalid_dhfile/1,
         hibernate_client/0,
         hibernate_client/1,
         hibernate_server/0,
         hibernate_server/1,
         listen_socket/0,
         listen_socket/1,
         peername/0,
         peername/1,
         recv_active/0,
         recv_active/1,
         recv_active_once/0,
         recv_active_once/1,
         recv_active_n/0,
         recv_active_n/1,
         recv_no_active_msg/0,
         recv_no_active_msg/1,
         recv_timeout/0,
         recv_timeout/1,
         recv_close/0,
         recv_close/1,
         controlling_process/0,
         controlling_process/1,
         controller_dies/0,
         controller_dies/1,
         controlling_process_transport_accept_socket/0,
         controlling_process_transport_accept_socket/1,
         close_with_timeout/0,
         close_with_timeout/1,
         close_in_error_state/0,
         close_in_error_state/1,
         call_in_error_state/0,
         call_in_error_state/1,
         close_transport_accept/0,
         close_transport_accept/1,
         abuse_transport_accept_socket/0,
         abuse_transport_accept_socket/1,
         honor_server_cipher_order/0,
         honor_server_cipher_order/1,
         honor_client_cipher_order/0,
         honor_client_cipher_order/1,
         honor_server_cipher_order_tls12/0,
         honor_server_cipher_order_tls12/1,
         honor_client_cipher_order_tls12/0,
         honor_client_cipher_order_tls12/1,
         honor_client_cipher_order_tls13/0,
         honor_client_cipher_order_tls13/1,
         honor_server_cipher_order_tls13/0,
         honor_server_cipher_order_tls13/1,
         ipv6/0,
         ipv6/1,
         der_input/0,
         der_input/1,
         new_options_in_handshake/0,
         new_options_in_handshake/1,
         max_handshake_size/0,
         max_handshake_size/1,
         invalid_certfile/0,
         invalid_certfile/1,
         invalid_cacertfile/0,
         invalid_cacertfile/1,
         invalid_keyfile/0,
         invalid_keyfile/1,
         options_not_proplist/0,
         options_not_proplist/1,
         options_whitebox/0, options_whitebox/1,
         cb_info/0,
         cb_info/1,
         log_alert/0,
         log_alert/1,
         getstat/0,
         getstat/1,
         handshake_continue/0,
         handshake_continue/1,
         handshake_continue_timeout/0,
         handshake_continue_timeout/1,
         handshake_continue_change_verify/0,
         handshake_continue_change_verify/1,
         handshake_hello_postpone_opts_verify/0,
         handshake_hello_postpone_opts_verify/1,
         hello_client_cancel/0,
         hello_client_cancel/1,
         hello_server_cancel/0,
         hello_server_cancel/1,
         handshake_continue_tls13_client/0,
         handshake_continue_tls13_client/1,
         rizzo_disabled/0,
         rizzo_disabled/1,
         rizzo_zero_n/0,
         rizzo_zero_n/1,
         rizzo_one_n_minus_one/0,
         rizzo_one_n_minus_one/1,
         supported_groups/0,
         supported_groups/1,
         client_options_negative_version_gap/0,
         client_options_negative_version_gap/1,
         client_options_negative_dependency_version/0,
         client_options_negative_dependency_version/1,
         client_options_negative_dependency_stateless/0,
         client_options_negative_dependency_stateless/1,
         client_options_negative_dependency_role/0,
         client_options_negative_dependency_role/1,
         client_options_negative_early_data/0,
         client_options_negative_early_data/1,
         server_options_negative_early_data/0,
         server_options_negative_early_data/1,
         server_options_negative_version_gap/0,
         server_options_negative_version_gap/1,
         server_options_negative_dependency_role/0,
         server_options_negative_dependency_role/1,
         server_options_negative_stateless_tickets_seed/0,
         server_options_negative_stateless_tickets_seed/1,
         invalid_options_tls13/0,
         invalid_options_tls13/1,
         ssl_not_started/0,
         ssl_not_started/1,
         cookie/0,
         cookie/1,
         check_random_nonce/0,
         check_random_nonce/1,
         cipher_listing/0,
         cipher_listing/1,
         format_error/0,
         format_error/1,
         export_key_materials/0,
         export_key_materials/1,
         exporter_master_secret_consumed/0,
         exporter_master_secret_consumed/1,
         legacy_prf/0,
         legacy_prf/1
        ]).

%% Apply export
-export([connection_information_result/1,
         connection_info_result/1,
         secret_connection_info_result/1,
         keylog_connection_info_result/2,
         check_srp_in_connection_information/3,
         check_connection_info/2,
         try_recv_active/1,
         try_recv_active_once/1,
         controlling_process_result/3,
         controller_dies_result/3,
         send_recv_result_timeout_client/1,
         send_recv_result_timeout_server/1,
         do_recv_close/1,
         tls_close/1,
         no_recv_no_active/1,
         ssl_getstat/1,
	 log/2,
         get_connection_information/3,
         protocol_version_check/2,
         suite_check/2,
         check_peercert/2,
         %%TODO Keep?
         run_error_server/1,
         run_error_server_close/1,
         run_client_error/1
        ]).

-compile([nowarn_deprecated_function]).


-define(SLEEP, 500).
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
     {group, 'dtlsv1'},
     {group, transport_socket}
    ] ++ simple_api_tests().

groups() ->
    [
     {'tlsv1.3', [parallel], ((gen_api_tests() ++ tls13_group() ++
                           handshake_paus_tests()) --
                          [dh_params,
                           new_options_in_handshake,
                           handshake_continue_tls13_client])
      ++ (since_1_2() -- [conf_signature_algs])},
     {'tlsv1.2', [parallel],  gen_api_tests() ++ since_1_2() ++ handshake_paus_tests() ++ pre_1_3() ++
          [honor_client_cipher_order_tls12,honor_server_cipher_order_tls12]},
     {'tlsv1.1', [parallel],  gen_api_tests() ++ handshake_paus_tests() ++ pre_1_3() ++ pre_1_2()},
     {'tlsv1', [parallel],  gen_api_tests() ++ handshake_paus_tests() ++ pre_1_3() ++ pre_1_2() ++
          beast_mitigation_test()},
     {'dtlsv1.2', [parallel], gen_api_tests() -- [new_options_in_handshake, hibernate_server] ++
          handshake_paus_tests() -- [handshake_continue_tls13_client] ++ pre_1_3()},
     {'dtlsv1', [parallel],  gen_api_tests() -- [new_options_in_handshake, hibernate_server] ++
          handshake_paus_tests() -- [handshake_continue_tls13_client] ++ pre_1_3() ++ pre_1_2()},
     {transport_socket,  [parallel], gen_api_tests() -- [ssl_not_started, dh_params]}
    ].

since_1_2() ->
    [
     conf_signature_algs,
     no_common_signature_algs,
     versions_option_based_on_sni,
     ciphers_option_based_on_sni,
     select_best_cert,
     root_any_sign
    ].

pre_1_3() ->
    [
     default_reject_anonymous,
     connection_information_with_srp
    ].

pre_1_2() ->
    [honor_server_cipher_order,
     honor_client_cipher_order].

simple_api_tests() ->
    [
     invalid_keyfile,
     invalid_certfile,
     invalid_cacertfile,
     invalid_dhfile,
     options_not_proplist,
     options_whitebox,
     format_error,
     ssl_not_started
    ].

gen_api_tests() ->
    [
     peercert,
     peercert_with_client_cert,
     select_sha1_cert,
     connection_information,
     secret_connection_info,
     keylog_connection_info,
     versions,
     new_options_in_handshake,
     active_n,
     dh_params,
     hibernate_client,
     hibernate_server,
     listen_socket,
     peername,
     recv_active,
     recv_active_once,
     recv_active_n,
     recv_no_active_msg,
     recv_timeout,
     recv_close,
     controlling_process,
     controller_dies,
     controlling_process_transport_accept_socket,
     close_with_timeout,
     close_in_error_state,
     call_in_error_state,
     close_transport_accept,
     ipv6,
     der_input,
     max_handshake_size,
     cb_info,
     log_alert,
     getstat,
     check_random_nonce,
     cipher_listing,
     export_key_materials,
     legacy_prf
    ].

handshake_paus_tests() ->
    [
     handshake_continue, 
     handshake_continue_timeout,
     handshake_continue_change_verify,
     handshake_hello_postpone_opts_verify,
     hello_client_cancel,
     hello_server_cancel,
     handshake_continue_tls13_client
    ].

%% Only relevant for SSL 3.0 and TLS 1.0
beast_mitigation_test() ->
    [%% Original option
     rizzo_disabled,
     %% Same effect as disable
     rizzo_zero_n, 
     %% Same as default
     rizzo_one_n_minus_one 
    ].

tls13_group() ->
    [
     supported_groups,
     honor_server_cipher_order_tls13,
     honor_client_cipher_order_tls13,
     client_options_negative_version_gap,
     client_options_negative_dependency_version,
     client_options_negative_dependency_stateless,
     client_options_negative_dependency_role,
     client_options_negative_early_data,
     server_options_negative_early_data,
     server_options_negative_version_gap,
     server_options_negative_dependency_role,
     server_options_negative_stateless_tickets_seed,
     invalid_options_tls13,
     cookie,
     exporter_master_secret_consumed
    ].

init_per_suite(Config0) ->
    catch application:stop(crypto),
    try application:start(crypto) of
	ok ->
	    ssl_test_lib:clean_start(),
	    ssl_test_lib:make_rsa_cert(Config0)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:unload(ssl),
    application:stop(crypto).

init_per_group(transport_socket, Config) ->
    ssl_test_lib:init_per_group(transport_socket,
                                [{client_type, erlang},
                                 {server_type, erlang}
                                | Config]);
init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_protocol_version(GroupName) of
        true  ->
            ssl_test_lib:init_per_group(GroupName, 
                                        [{client_type, erlang},
                                         {server_type, erlang},
                                         {version, GroupName}
                                        | Config]);
        false ->
            Config
    end.

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(handshake_continue_tls13_client, Config) ->
    case ssl_test_lib:sufficient_crypto_support('tlsv1.3') of
        true ->
            ssl_test_lib:ct_log_supported_protocol_versions(Config),
            ct:timetrap({seconds, 10}),
            Config;
        false ->
            {skip, "Missing crypto support: TLS 1.3 not supported"}
    end;
init_per_testcase(connection_information_with_srp, Config) ->
    PKAlg = proplists:get_value(public_keys, crypto:supports()),
    case lists:member(srp, PKAlg) of
        true ->
            Config;
        false ->
            {skip, "Missing SRP crypto support"}
    end;
init_per_testcase(conf_signature_algs, Config) ->
    case ssl_test_lib:appropriate_sha(crypto:supports()) of
        sha256 ->
            ssl_test_lib:ct_log_supported_protocol_versions(Config),
            ct:timetrap({seconds, 10}),
            Config;
        sha ->
            {skip, "Tests needs certs with sha256"}
    end;
init_per_testcase(check_random_nonce, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 20}),
    Config;
init_per_testcase(select_best_cert, Config) ->
    ct:timetrap({seconds, 10}),
    Version = ssl_test_lib:protocol_version(Config),
    %% We need to make sure TLS-1.3 can be supported as
    %% want to generate a TLS-1.3 specific certificate that will not
    %% be chosen
    case Version of
        'tlsv1.2' ->
              case ssl_test_lib:sufficient_crypto_support('tlsv1.3') of
                  true ->
                      Config;
                  false ->
                      {skip, "Crypto does not support EDDSA"}
              end;
        _ ->
            Config
    end;
init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
peercert() ->
    [{doc,"Test API function peercert/1"}].
peercert(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
			   {mfa, {ssl, peercert, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl, peercert, []}},
			   {options, ClientOpts}]),

    CertFile = proplists:get_value(certfile, ServerOpts),
    [{'Certificate', BinCert, _}]= ssl_test_lib:pem_to_der(CertFile),

    ServerMsg = {error, no_peercert},
    ClientMsg = {ok, BinCert},

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

peercert_with_client_cert() ->
    [{doc,"Test API function peercert/1"}].
peercert_with_client_cert(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
			   {mfa, {ssl, peercert, []}},
			   {options, [{verify, verify_peer} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl, peercert, []}},
			   {options, ClientOpts}]),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    [{'Certificate', ServerBinCert, _}]= ssl_test_lib:pem_to_der(ServerCertFile),
     ClientCertFile = proplists:get_value(certfile, ClientOpts),
    [{'Certificate', ClientBinCert, _}]= ssl_test_lib:pem_to_der(ClientCertFile),

    ServerMsg = {ok, ClientBinCert},
    ClientMsg = {ok, ServerBinCert},

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
select_best_cert() ->
    [{doc,"Basic test of the certs_keys option."}].

select_best_cert(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    Conf = test_config(Version, Config),
    lists:foreach(
      fun({#{server_config := SConfig,
             client_config := CConfig},
           {client_peer, CExpected},
           {server_peer, SExpected}}) ->
              selected_peer(CExpected, SExpected,
                            ssl_test_lib:ssl_options(CConfig, Config),
                            ssl_test_lib:ssl_options(SConfig, Config),
                            Conf)
      end, Conf).



%%--------------------------------------------------------------------
select_sha1_cert() ->
    [{doc,"Use cert signed with rsa and sha1"}].

select_sha1_cert(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    TestConfRSA =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => [{digest, sha},
                                                   {key, ssl_test_lib:hardcode_rsa_key(1)}],
                                          intermediates => [[{digest, sha},
                                                             {key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer =>  [{digest, sha}, {key, ssl_test_lib:hardcode_rsa_key(3)}]
                                         },
                                    client_chain =>
                                        #{root => [{digest, sha},
                                                   {key, ssl_test_lib:hardcode_rsa_key(3)}],
                                          intermediates => [[{digest, sha},
                                                             {key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer => [{digest, sha},
                                                   {key, ssl_test_lib:hardcode_rsa_key(1)}]}}),

    TestConfECDSA =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => [{digest, sha},
                                                   {key, {namedCurve, secp256r1}}],
                                          intermediates =>
                                              [[{digest, sha},
                                                {key,{namedCurve, secp256r1}}]],
                                          peer =>  [{digest, sha},
                                                    {key,{namedCurve, secp256r1}}]
                                         },
                                    client_chain =>
                                        #{root => [{digest, sha},
                                                   {key, {namedCurve, secp256r1}}],
                                          intermediates => [[{digest, sha},
                                                             {key, {namedCurve, secp256r1}}]],
                                          peer => [{digest, sha},
                                                   {key, {namedCurve, secp256r1}}]}}),
    test_sha1_cert_conf(Version, TestConfRSA, TestConfECDSA, Config).

%%--------------------------------------------------------------------
root_any_sign() ->
    [{doc,"Use cert signed with unsupported signature for the root will succeed, "
      "as it is not verified"}].

root_any_sign(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    #{client_config := CSucess, server_config := SSucess} =
        public_key:pkix_test_data(#{server_chain =>
                                         #{root => [{digest, sha},
                                                    {key, ssl_test_lib:hardcode_rsa_key(1)}],
                                           intermediates => [[{digest, sha256},
                                                              {key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                           peer =>  [{digest, sha256}, {key, ssl_test_lib:hardcode_rsa_key(3)}]
                                          },
                                     client_chain =>
                                         #{root => [{digest, sha},
                                                   {key, ssl_test_lib:hardcode_rsa_key(3)}],
                                           intermediates => [[{digest, sha256},
                                                              {key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                           peer => [{digest, sha256},
                                                    {key, ssl_test_lib:hardcode_rsa_key(1)}]}}),

    #{client_config := CFail, server_config := SFail} =
        public_key:pkix_test_data(#{server_chain =>
                                        #{root => [{digest, sha256},
                                                   {key, ssl_test_lib:hardcode_rsa_key(1)}],
                                          intermediates => [[{digest, sha},
                                                             {key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer =>  [{digest, sha256}, {key, ssl_test_lib:hardcode_rsa_key(3)}]
                                         },
                                    client_chain =>
                                        #{root => [{digest, sha256},
                                                   {key, ssl_test_lib:hardcode_rsa_key(3)}],
                                          intermediates => [[{digest, sha},
                                                             {key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer => [{digest, sha256},
                                                   {key, ssl_test_lib:hardcode_rsa_key(1)}]}}),

    %% Makes sha1 disallowed for certificate signatures when set explicitly 
    %% (default for signature_algs_cert was changed to allow them if signatures_algs is not set explicitly)
    SigAlgs = ssl:signature_algs(default, Version), 
    %% Root signatures are not validated, so its signature will not fail the connection                             
    ssl_test_lib:basic_test(CSucess, [{verify, verify_peer}, {signature_algs, SigAlgs} | SSucess], Config),
    %% Intermediate cert signatures are validated, so sha1 signatures will fail connection                             
    ssl_test_lib:basic_alert(CFail, [{verify, verify_peer}, {signature_algs, SigAlgs} | SFail],
                             Config, unsupported_certificate).

%%--------------------------------------------------------------------
connection_information() ->
    [{doc,"Test the API function ssl:connection_information/1"}].
connection_information(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, connection_information_result, []}},
					{options, ServerOpts}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, connection_information_result, []}},
			   {options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
connection_information_with_srp() ->
    [{doc,"Test the result of API function ssl:connection_information/1"
          "includes srp_username."}].
connection_information_with_srp(Config) when is_list(Config) ->
    run_conn_info_srp_test(srp_anon, 'aes_128_cbc', Config).

run_conn_info_srp_test(Kex, Cipher, Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    TestCiphers = ssl_test_lib:test_ciphers(Kex, Cipher, Version),

    case TestCiphers of
        [] ->
            {skip, {not_sup, Kex, Cipher, Version}};
        [TestCipher | _T] ->
            do_run_conn_info_srp_test(TestCipher, Version, Config)
    end.

do_run_conn_info_srp_test(ErlangCipherSuite, Version, Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    SOpts = [{user_lookup_fun, {fun ssl_test_lib:user_lookup/3, undefined}}],
    COpts = [{verify, verify_none}, {srp_identity, {"Test-User", "secret"}}],

    ServerOpts = ssl_test_lib:ssl_options(SOpts, Config),
    ClientOpts = ssl_test_lib:ssl_options(COpts, Config),

    ?CT_LOG("Erlang Cipher Suite is: ~p~n", [ErlangCipherSuite]),

    Server =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {?MODULE, check_srp_in_connection_information, [<<"Test-User">>, server]}},
                                   {options, [{versions, [Version]}, {ciphers, [ErlangCipherSuite]} |
                                              ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, check_srp_in_connection_information, [<<"Test-User">>, client]}},
           {options, [{versions, [Version]}, {ciphers, [ErlangCipherSuite]} |
                      ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

secret_connection_info() ->
    [{doc,"Test the API function ssl:connection_information/2"}].
secret_connection_info(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {?MODULE, secret_connection_info_result, []}},
                                   {options, [{verify, verify_peer} | ServerOpts]}]),
    
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                   {host, Hostname},
                                   {from, self()},
                                   {mfa, {?MODULE, secret_connection_info_result, []}},
                                   {options,  [{verify, verify_peer} |ClientOpts]}]),
    
    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),
			   
    ssl_test_lib:check_result(Server, true, Client, true),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
keylog_connection_info() ->
    [{doc,"Test the API function ssl:connection_information/2"}].
keylog_connection_info(Config) when is_list(Config) ->
    keylog_connection_info(Config, true),
    keylog_connection_info(Config, false).
keylog_connection_info(Config, KeepSecrets) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {?MODULE, keylog_connection_info_result, [KeepSecrets]}},
                                   {options, [{verify, verify_peer}, {keep_secrets, KeepSecrets} | ServerOpts]}]),

    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                   {host, Hostname},
                                   {from, self()},
                                   {mfa, {?MODULE, keylog_connection_info_result, [KeepSecrets]}},
                                   {options,  [{verify, verify_peer}, {keep_secrets, KeepSecrets} |ClientOpts]}]),

    ?CT_LOG("KeepSecrets ~p Client ~p  Server ~p ~n", [KeepSecrets, Client, Server]),

    ServerKeylog = receive
                       {Server, {ok, Keylog}} ->
                           Keylog;
                       {Server, ServerError} ->
                           ct:fail({server, ServerError})
                   after 5000 ->
                           ct:fail({server, timeout})
                   end,

    receive
        {Client, {ok, ServerKeylog}} ->
            ok;
        {Client, {ok, ClientKeylog}} ->
            ct:fail({mismatch, {ServerKeylog, ClientKeylog}});
        {Client, ClientError} ->
            ct:fail({client, ClientError})
    after 5000 ->
            ct:fail({client, timeout})
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
dh_params() ->
    [{doc,"Test to specify DH-params file in server."}].

dh_params(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    DataDir = proplists:get_value(data_dir, Config),
    DHParamFile = filename:join(DataDir, "dHParam.pem"),
    Ciphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.2'),
                                       [{key_exchange, fun(srp_rsa)  -> false;
                                                          (srp_anon) -> false;
                                                          (srp_dss) -> false;
                                                          (_) -> true end}]),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {ssl_test_lib, send_recv_result_active, []}},
			   {options, [{dhfile, DHParamFile}, {ciphers, Ciphers} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {ssl_test_lib, send_recv_result_active, []}},
			   {options,
			    [{ciphers,[{dhe_rsa,aes_256_cbc,sha}]} | 
				       ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
invalid_dhfile() ->
    [{doc,"Test to check invalid DH-params file in server."}].
invalid_dhfile(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    DataDir = proplists:get_value(data_dir, Config),
    DHParamFile = filename:join(DataDir, "dHParam-invalid.pem"),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
                                              {from, self()},
                                              {mfa, {ssl_test_lib,
                                                     send_recv_result_active,
                                                     []}},
                                               {options, [{dhfile, DHParamFile}
                                                          | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                              {host, Hostname}, {from, self()},
                                              {mfa, {ssl_test_lib,
                                                     send_recv_result_active,
                                                     []}},
                                              {options, [{ciphers,
                                                          [{dhe_rsa,
                                                            aes_256_cbc, sha}]}
                                                         | ClientOpts]}]),

    %% assert server error
    [{Server, {error, {options, {dhfile, DHParamFile,
                                 {error, {asn1,
                                          {{invalid_value, 0}, _Stack}}}}}}}] =
        ssl_test_lib:get_result([Server]),

    %% assert client error
    ssl_test_lib:check_result(Client, {error, closed}),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
conf_signature_algs() ->
    [{doc,"Test to set the signature_algs option on both client and server"}].
conf_signature_algs(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false}, {signature_algs, [{sha256, rsa}]}, 
                                               {versions, ['tlsv1.2']} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false}, {signature_algs, [{sha256, rsa}]},
                                              {versions, ['tlsv1.2']} | ClientOpts]}]),
    
    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
no_common_signature_algs()  ->
    [{doc,"Set the signature_algs option so that there client and server does not share any hash sign algorithms"}].
no_common_signature_algs(Config) when is_list(Config) ->
    
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),


    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, [{signature_algs, [{sha256, rsa}]}
							 | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                              {host, Hostname},
                                              {from, self()},
                                              {options, [{signature_algs, [{sha384, rsa}]}
                                                         | ClientOpts]}]),
    
    ssl_test_lib:check_server_alert(Server, Client, insufficient_security).

%%--------------------------------------------------------------------
handshake_continue() ->
    [{doc, "Test API function ssl:handshake_continue/3"}].
handshake_continue(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {host, Hostname},
                                   {from, self()},
                                   {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                   {options, ssl_test_lib:ssl_options([{reuseaddr, true},
                                                                            {verify, verify_peer},
                                                                       {handshake, hello} | ServerOpts
                                                                      ],
                                                                      Config)},
                                   {continue_options, proplists:delete(reuseaddr, ServerOpts)}
                                  ]),
    
    Port = ssl_test_lib:inet_port(Server),

    Client =
        ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                   {host, Hostname},
                                   {from, self()},
                                   {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                   {options, ssl_test_lib:ssl_options([{handshake, hello},
                                                                       {verify, verify_none} | ClientOpts
                                                                      ],
                                                                      Config)},
                                   {continue_options,  [{verify, verify_peer} | ClientOpts]}]),
     
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
handshake_continue_tls13_client() ->
    [{doc, "Test API function ssl:handshake_continue/3 with fixed TLS 1.3 client"}].
handshake_continue_tls13_client(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    SCiphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                        [{key_exchange, fun(srp_rsa)  -> false;
                                                           (srp_anon) -> false;
                                                           (srp_dss) -> false;
                                                           (_) -> true end}]),
    Server =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                   {options, ssl_test_lib:ssl_options([{reuseaddr, true},
                                                                       {verify, verify_peer},
                                                                       {ciphers, SCiphers},
                                                                       {handshake, hello} | ServerOpts
                                                                      ],
                                                                      Config)},
                                   {continue_options, proplists:delete(reuseaddr, ServerOpts)}
                                  ]),

    Port = ssl_test_lib:inet_port(Server),

    DummyTicket =
        <<131,116,0,0,0,5,100,0,4,104,107,100,102,100,0,6,115,104,97,51,
          56,52,100,0,3,112,115,107,109,0,0,0,48,150,90,38,127,26,12,5,
          228,180,235,229,214,215,27,236,149,182,82,14,140,50,81,0,150,
          248,152,180,193,207,80,52,107,196,200,2,77,4,96,140,65,239,205,
          224,125,129,179,147,103,100,0,3,115,110,105,107,0,25,112,114,
          111,117,100,102,111,111,116,46,111,116,112,46,101,114,105,99,
          115,115,111,110,46,115,101,100,0,6,116,105,99,107,101,116,104,6,
          100,0,18,110,101,119,95,115,101,115,115,105,111,110,95,116,105,
          99,107,101,116,98,0,0,28,32,98,127,110,83,249,109,0,0,0,8,0,0,0,
          0,0,0,0,5,109,0,0,0,113,112,154,74,26,27,0,111,147,51,110,216,
          43,45,4,100,215,152,195,118,96,22,34,1,184,170,42,166,238,109,
          187,138,196,147,102,205,116,83,241,174,227,232,156,148,60,153,3,
          175,128,115,192,36,103,191,239,58,222,192,172,190,239,92,8,131,
          195,0,217,187,222,143,104,6,86,53,93,27,218,198,205,138,223,202,
          11,55,168,104,6,219,228,217,157,37,52,205,252,165,135,167,116,
          216,172,231,222,189,84,97,0,8,106,108,88,47,114,48,116,0,0,0,0,
          100,0,9,116,105,109,101,115,116,97,109,112,98,93,205,0,44>>,

    %% Send dummy session ticket to trigger sending of pre_shared_key and
    %% psk_key_exchange_modes extensions.
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {ssl_test_lib, send_recv_result_active, []}},
           {options, ssl_test_lib:ssl_options([{handshake, hello},
                                               {session_tickets, manual},
                                               {use_ticket, [DummyTicket]},
                                               {versions, ['tlsv1.3',
                                                           'tlsv1.2',
                                                           'tlsv1.1',
                                                           'tlsv1'
                                                          ]},
                                               {ciphers, ssl:cipher_suites(all, 'tlsv1.3')},
                                               {verify, verify_peer} |
                                               proplists:delete(versions, ClientOpts)
                                              ],
                                              Config)},
           {continue_options, proplists:delete(versions, proplists:delete(reuseaddr, ClientOpts))}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%------------------------------------------------------------------
handshake_continue_timeout() ->
    [{doc, "Test API function ssl:handshake_continue/3 with short timeout"}].
handshake_continue_timeout(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {timeout, 1},
                                        {options, ssl_test_lib:ssl_options([{reuseaddr, true}, {handshake, hello},
                                                                            {verify, verify_none} | ServerOpts],
                                                                           Config)},
                                        {continue_options, [{verify, verify_peer} | ServerOpts]}
                                       ]),

    Port = ssl_test_lib:inet_port(Server),


    ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                     {host, Hostname},
                                     {from, self()},
                                     {options, [{verify, verify_peer} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, {error,timeout}),
    ssl_test_lib:close(Server).
%%------------------------------------------------------------------
handshake_continue_change_verify() ->
    [{doc, "Test API function ssl:handshake_continue with updated verify option. "
      "Use a verification that will fail to make sure verification is run"}].
handshake_continue_change_verify(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {options, ssl_test_lib:ssl_options(
                            [{handshake, hello},
                             {verify, verify_peer} | ServerOpts], Config)},
                {continue_options, proplists:delete(reuseaddr, ServerOpts)}]),

    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client_error(
               [{node, ClientNode}, {port, Port},
                {host, Hostname},
                {from, self()},
                {options, ssl_test_lib:ssl_options(
                            [{handshake, hello},
                             {server_name_indication, "foobar"},
                             {verify, verify_none}
                            | ClientOpts], Config)},
                {continue_options, [{verify, verify_peer} | ClientOpts]}]),
    ssl_test_lib:check_client_alert(Client,  handshake_failure).

%%------------------------------------------------------------------
handshake_hello_postpone_opts_verify() ->
   [{doc, "Test that cert key option validation is postponed until full handshake is performed"}].
handshake_hello_postpone_opts_verify(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    TcpPort = ssl_test_lib:inet_port(node()),
    Host = net_adm:localhost(),
    Version = ssl_test_lib:protocol_version(Config),

    {ok,TcpListenSocket} = gen_tcp:listen(TcpPort, [binary, {active, false}, inet, {reuseaddr, true}]),
    spawn(fun() ->
                  {ok, TcpSocket} = gen_tcp:accept(TcpListenSocket),
                  {ok, ASocket, _Ext} = ssl:handshake(TcpSocket, [{handshake, hello}, {versions, [Version]}]),
                  ssl:handshake_continue(ASocket, ServerOpts, 5000)
          end),
    {ok, UpgradeClient} = ssl:connect(Host, TcpPort, ClientOpts),
    ssl:close(UpgradeClient),

    Port = ssl_test_lib:inet_port(node()),
    {ok,TlsListenSocket} = ssl:listen(Port, [binary, {active, false}, inet, {reuseaddr, true}]),
    spawn(fun() ->
                  {ok, ASocket} = ssl:transport_accept(TlsListenSocket),
                  {ok, _Ext} = ssl:handshake(ASocket, [{handshake, hello}, {versions, [Version]}]),
                  ssl:handshake_continue(ASocket, ServerOpts, 5000)
          end),
    {ok, Client} = ssl:connect(Host, Port, [{versions, [Version]} | ClientOpts]),
    ssl:close(Client).

%%--------------------------------------------------------------------
hello_client_cancel() ->
    [{doc, "Test API function ssl:handshake_cancel/1 on the client side"}].
hello_client_cancel(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {host, Hostname},
                                   {from, self()},
                                   {options, ssl_test_lib:ssl_options([{handshake, hello},
                                                                       {verify, verify_none} | ServerOpts], Config)},
                                   {continue_options, [{verify, verify_peer} | ServerOpts]}]),
    
    Port = ssl_test_lib:inet_port(Server),

    %% That is ssl:handshake_cancel returns ok
    {connect_failed, ok} =
        ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                   {host, Hostname},
                                   {from, self()},
                                   {options, ssl_test_lib:ssl_options([{handshake, hello}
                                                                      | ClientOpts], Config)},
                                   {continue_options, cancel}]),
    ssl_test_lib:check_server_alert(Server, user_canceled).
%%--------------------------------------------------------------------
hello_server_cancel() ->
    [{doc, "Test API function ssl:handshake_cancel/1 on the server side"}].
hello_server_cancel(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {options, ssl_test_lib:ssl_options([{handshake, hello},
                                                                       {verify, verify_peer} | ServerOpts
                                                                      ], Config)},
                                   {continue_options, cancel}]),
    
    Port = ssl_test_lib:inet_port(Server),

    ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                     {host, Hostname},
                                     {from, self()}, 
                                     {options, ssl_test_lib:ssl_options([{handshake, hello},
                                                                         {verify, verify_peer} | ClientOpts
                                                                        ], Config)},
                                     {continue_options, proplists:delete(reuseaddr, ClientOpts)}]),
    
    ssl_test_lib:check_result(Server, ok).

%%--------------------------------------------------------------------
versions() ->
    [{doc,"Test API function versions/0"}].

versions(Config) when is_list(Config) -> 
    [_|_] = Versions = ssl:versions(),
    ?CT_LOG("~p~n", [Versions]).

%%--------------------------------------------------------------------

versions_option_based_on_sni() ->
    [{doc,"Test that SNI versions option is selected over default versions"}].

versions_option_based_on_sni(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    TestVersion = ssl_test_lib:protocol_version(Config),
    {Version, Versions} = test_versions_for_option_based_on_sni(TestVersion),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Ciphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, TestVersion),
                                       [{key_exchange, fun(srp_rsa) -> false;
                                                          (srp_dss) -> false;
                                                          (_) -> true
                                                       end}]),

    SNI = net_adm:localhost(),
    Fun = fun(ServerName) ->
              case ServerName of
                  SNI ->
                      [{versions, [Version]}, {ciphers, Ciphers} |
                       proplists:delete(versions, ServerOpts)];
                  _ ->
                      ServerOpts
              end
          end,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, protocol_version_check, [Version]}},
					{options, [{sni_fun, Fun},
                                                   {versions, Versions} |
                                                   proplists:delete(versions, ServerOpts)]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, no_result, []}},
					{options, [{server_name_indication, SNI}, {versions, Versions},
                                                   {ciphers, Ciphers}
                                                  | proplists:delete(versions, ClientOpts)]}]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------

ciphers_option_based_on_sni() ->
    [{doc,"Test that SNI versions option is selected over default ciphers option"}].

ciphers_option_based_on_sni(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    TestVersion = ssl_test_lib:protocol_version(Config),
    Suites = rsa_cipher_suites_not_default(TestVersion),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    SNI = net_adm:localhost(),
    Fun = fun(ServerName) ->
              case ServerName of
                  SNI ->
                      [{ciphers, Suites} | ServerOpts];
                  _ ->
                      ServerOpts
              end
          end,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, suite_check, [TestVersion]}},
					{options, [{sni_fun, Fun} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, no_result, []}},
					{options, [{server_name_indication, SNI} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
%% Test case adapted from gen_tcp_misc_SUITE.
active_n() ->
    [{doc,"Test {active,N} option"}].

active_n(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Port = ssl_test_lib:inet_port(node()),
    Host = net_adm:localhost(),
    N = 3,
    LS = ok(ssl:listen(Port, [{active,N}|ServerOpts])),
    [{active,N}] = ok(ssl:getopts(LS, [active])),
    active_n_common(LS, N),
    Self = self(),
    spawn_link(fun() ->
        S0 = ok(ssl:transport_accept(LS)),
        {ok, S} = ssl:handshake(S0),
        ok = ssl:setopts(S, [{active,N}]),
        [{active,N}] = ok(ssl:getopts(S, [active])),
        ssl:controlling_process(S, Self),
        Self ! {server, S}
    end),
    C = ok(ssl:connect(Host, Port, [{active,N}|ClientOpts])),
    [{active,N}] = ok(ssl:getopts(C, [active])),
    S = receive
        {server, S0} -> S0
    after
        1000 ->
            exit({error, connect})
    end,
    active_n_common(C, N),
    active_n_common(S, N),
    ok = ssl:setopts(C, [{active,N}]),
    ok = ssl:setopts(S, [{active,N}]),
    ReceiveMsg = fun(Socket, Msg) ->
        receive
            {ssl,Socket,Msg} ->
                ok;
            {ssl,Socket,Begin} ->
                receive
                    {ssl,Socket,End} ->
                        Msg = Begin ++ End,
                        ok
                after 1000 ->
                    exit(timeout)
                end
        after 1000 ->
            exit(timeout)
        end
    end,
    repeat(3, fun(I) ->
        Msg = "message "++integer_to_list(I),
        ok = ssl:send(C, Msg),
        ReceiveMsg(S, Msg),
        ok = ssl:send(S, Msg),
        ReceiveMsg(C, Msg)
    end),
    receive
        {ssl_passive,S} ->
            [{active,false}] = ok(ssl:getopts(S, [active]))
    after
        1000 ->
            exit({error,ssl_passive})
    end,
    receive
        {ssl_passive,C} ->
            [{active,false}] = ok(ssl:getopts(C, [active]))
    after
        1000 ->
            exit({error,ssl_passive})
    end,
    LS2 = ok(ssl:listen(0, [{active,0}])),
    receive
        {ssl_passive,LS2} ->
            [{active,false}] = ok(ssl:getopts(LS2, [active]))
    after
        1000 ->
            exit({error,ssl_passive})
    end,
    ok = ssl:close(LS2),
    ok = ssl:close(C),
    ok = ssl:close(S),
    ok = ssl:close(LS),
    ok.

hibernate_client() ->
    [{doc,"Check that an SSL connection on client side that is started with "
      "option hibernate_after indeed hibernates after inactivity"}].

hibernate_client(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    StartServerOpts = [return_socket, {node, ServerNode},
                       {port, 0}, {from, self()},
                       {mfa, {ssl_test_lib, send_recv_result_active, []}},
                       {options, [{hibernate_after, infinity} | ServerOpts]}],
    StartClientOpts = [return_socket, {node, ClientNode}, {host, Hostname},
                       {from, self()},
                       {mfa, {ssl_test_lib, send_recv_result_active, []}}],
    [ok = hibernate_helper(?config(version, Config), false,
                           StartServerOpts, StartClientOpts,
                           ServerOpts, ClientOpts, T, infinity) ||
        T <- [1000, 0, 1]].

hibernate_server() ->
    [{doc,"Check that an SSL connection on server side that is started with "
      "option hibernate_after indeed hibernates after inactivity."
      "Note: for DTLS test will not be stable, because cookie secret refresh "
      "mechanism might disturb hibernation of a server process."}].
hibernate_server(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    StartServerOpts = [return_socket, {node, ServerNode}, {port, 0},
                       {from, self()},
                       {mfa, {ssl_test_lib, send_recv_result_active, []}}],
    StartClientOpts = [return_socket, {node, ClientNode}, {host, Hostname},
                       {from, self()},
                       {mfa, {ssl_test_lib, send_recv_result_active, []}}],
    [ok = hibernate_helper(?config(version, Config), true,
                           StartServerOpts, StartClientOpts,
                           ServerOpts, ClientOpts, infinity, T) ||
        T <- [1000, 0, 1]].

hibernate_helper(Version, CheckServer, StartServerOpts, StartClientOpts,
                 ServerOpts0, ClientOpts0,
                 ClientHibernateAfter, ServerHibernateAfter) ->
    AllServerOpts = StartServerOpts ++
        [{options, [{hibernate_after, ServerHibernateAfter} | ServerOpts0]}],
    Server = ssl_test_lib:start_server(AllServerOpts),
    Port = ssl_test_lib:inet_port(Server),
    AllClientOpts = StartClientOpts ++
        [{port, Port},
         {options, [{hibernate_after, ClientHibernateAfter} | ClientOpts0]}],
    {Client, #sslsocket{connection_handler = ClientReceiverPid,
                        payload_sender = ClientPotentialSenderPid}} =
        ssl_test_lib:start_client(AllClientOpts),
    Results = ssl_test_lib:get_result([Client, Server]),
    {ok, ServerAcceptSocket} = proplists:get_value(Server, Results),
    ok = proplists:get_value(Client, Results),
    #sslsocket{connection_handler = ServerReceiverPid,
               payload_sender = ServerPotentialSenderPid} =
        ServerAcceptSocket,
    {ReceiverPid, PotentialSenderPid, HibernateAfter} =
        case CheckServer of
            true -> {ServerReceiverPid, ServerPotentialSenderPid,
                     ServerHibernateAfter};
            false -> {ClientReceiverPid, ClientPotentialSenderPid,
                      ClientHibernateAfter}
        end,
    SleepAmount = max(1.5*HibernateAfter, 500),
    ?CT_LOG("HibernateAfter = ~w SleepAmount = ~w", [HibernateAfter, SleepAmount]),
    ct:sleep(SleepAmount), %% Schedule out
    {current_function, {gen_statem, loop_hibernate, 3}} =
        process_info(ReceiverPid, current_function),
    IsTls = ssl_test_lib:is_tls_version(Version),
    case IsTls of
        true ->
            SenderPid = PotentialSenderPid,
            {current_function, {gen_statem, loop_hibernate, 3}} =
                process_info(SenderPid, current_function);
        _ -> %% DTLS (no sender process)
            ok
    end,
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ok.

%%--------------------------------------------------------------------
listen_socket() ->
    [{doc,"Check error handling and inet compliance when calling API functions with listen sockets."}].

listen_socket(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ok, ListenSocket} = ssl:listen(0, ServerOpts),
    Protocol = proplists:get_value(protocol, ServerOpts, tls),
    %% This can be a valid thing to do as
    %% options are inherited by the accept socket
    ok = ssl:controlling_process(ListenSocket, self()),

    {ok, _} = ssl:sockname(ListenSocket),

    Check = fun({error, enotconn}) -> ok;
               ({error, epipe}) -> ok;
               ({error, #{info := enotconn}}) -> ok  %% socket error msgs
            end,

    Check(ssl:send(ListenSocket, <<"data">>)),
    Check(ssl:recv(ListenSocket, 0)),
    Check(ssl:connection_information(ListenSocket)),
    Check(ssl:peername(ListenSocket)),
    Check(ssl:peercert(ListenSocket)),
    Check(ssl:renegotiate(ListenSocket)),
    Check(ssl:export_key_materials(ListenSocket, [<<"Label">>], [<<"Context">>], 256)),
    %% Legacy test
    Check(ssl:prf(ListenSocket, master_secret,
                  <<"Label">>, [client_random, server_random], 256)),
    case Protocol of
        tls ->
            Check(ssl:shutdown(ListenSocket, read_write));
        dtls ->
            {error, notsup} = ssl:shutdown(ListenSocket, read_write)
    end,
    ok = ssl:close(ListenSocket).

%%--------------------------------------------------------------------
peername() ->
    [{doc,"Test API function peername/1"}].

peername(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server(
               [
                {node, ServerNode}, {port, 0},
                {from, self()},
                {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, CSocket} = ssl_test_lib:start_client(
                          [return_socket,
                           {node, ClientNode}, {port, Port},
                           {host, Hostname},
                           {from, self()},
                           {options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    Server ! get_socket,
    SSocket =
        receive
            {Server, {socket, Socket}} ->
                Socket
        end,

    {ok, ServerPeer} = ssl:peername(SSocket),
    ?CT_LOG("Server's peer: ~p~n", [ServerPeer]),
    {ok, ClientPeer} = ssl:peername(CSocket),
    ?CT_LOG("Client's peer: ~p~n", [ClientPeer]),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

recv_active() ->
    [{doc,"Test recv on active socket"}].

recv_active(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {?MODULE, try_recv_active, []}},
				   {options,  [{active, true} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {?MODULE, try_recv_active, []}},
				   {options, [{active, true} | ClientOpts]}]),
        
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
recv_active_once() ->
    [{doc,"Test recv on active (once) socket"}].

recv_active_once(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {?MODULE, try_recv_active_once, []}},
				   {options,  [{active, once} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {?MODULE, try_recv_active_once, []}},
				   {options, [{active, once} | ClientOpts]}]),
        
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
recv_active_n() ->
    [{doc,"Test recv on active (n) socket"}].

recv_active_n(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, try_recv_active_once, []}},
				   {options,  [{active, 1} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {?MODULE, try_recv_active_once, []}},
				   {options, [{active, 1} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
recv_timeout() ->
    [{doc,"Test ssl:recv timeout"}].

recv_timeout(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, send_recv_result_timeout_server, []}},
				   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       send_recv_result_timeout_client, []}},
					{options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
recv_close() ->
    [{doc,"Special case of call error handling"}].
recv_close(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					  {from, self()},
					  {mfa, {?MODULE, do_recv_close, []}},
					 {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {_Client, #sslsocket{} = SslSocket} = ssl_test_lib:start_client([return_socket,
									   {node, ClientNode}, {port, Port},
									   {host, Hostname},
									   {from, self()},
									   {mfa, {ssl_test_lib, no_result, []}},
									   {options, ClientOpts}]),
    ssl:close(SslSocket),
    ssl_test_lib:check_result(Server, ok).


%%--------------------------------------------------------------------
recv_no_active_msg() ->
    [{doc,"If we have a passive socket and do not call recv and peer closes we should no get"
      "receive an active message"}].
recv_no_active_msg(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					  {from, self()},
					  {mfa, {?MODULE, no_recv_no_active, []}},
					 {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, no_result, []}},
                                        {options, ClientOpts}]),
    ssl_test_lib:close(Client),
    ssl_test_lib:check_result(Server, ok).

%%--------------------------------------------------------------------
controlling_process() ->
    [{doc,"Test API function controlling_process/2"}].

controlling_process(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientMsg = "Server hello",
    ServerMsg = "Client hello",
   
    Server = ssl_test_lib:start_server([
                                        {node, ServerNode}, {port, 0}, 
                                        {from, self()}, 
                                        {mfa, {?MODULE, 
                                               controlling_process_result, [self(),
                                                                            ServerMsg]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, CSocket} = ssl_test_lib:start_client([return_socket,
                                                   {node, ClientNode}, {port, Port}, 
                                                   {host, Hostname},
                                        {from, self()}, 
			   {mfa, {?MODULE, 
				  controlling_process_result, [self(),
							       ClientMsg]}},
			   {options, ClientOpts}]),
    
    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),
    
    ServerMsg = ssl_test_lib:active_recv(CSocket, length(ServerMsg)),
    %% We do not have the TLS server socket but all messages form the client
    %% socket are now read, so ramining are form the server socket
    ClientMsg = ssl_active_recv(length(ClientMsg)),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
controller_dies() ->
    [{doc,"Test that the socket is closed after controlling process dies"}].
controller_dies(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientMsg = "Hello server",
    ServerMsg = "Hello client",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, 
					       controller_dies_result, [self(),
									ServerMsg]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       controller_dies_result, [self(),
									    ClientMsg]}},
					{options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),
    ct:sleep(?SLEEP), %% so that they are connected
    
    process_flag(trap_exit, true),

    %% Test that clients die
    exit(Client, killed),
    get_close(Client, ?LINE),

    %% Test that clients die when process disappear
    Server ! listen, 
    Tester = self(),
    Connect = fun(Pid) ->
		      {ok, Socket} = ssl:connect(Hostname, Port, ClientOpts),
		      %% Make sure server finishes and verification
		      %% and is in coonection state before
		      %% killing client
		      ct:sleep(?SLEEP),
		      Pid ! {self(), connected, Socket},
		      receive die_nice -> normal end
	      end,
    Client2 = spawn_link(fun() -> Connect(Tester) end),
    receive {Client2, connected, _Socket} ->  Client2 ! die_nice end,
    
    get_close(Client2, ?LINE),
    
    %% Test that clients die when the controlling process have changed 
    Server ! listen, 

    Client3 = spawn_link(fun() -> Connect(Tester) end),
    Controller = spawn_link(fun() -> receive die_nice -> normal end end),
    receive 
	{Client3, connected, Socket} ->  
	    ok = ssl:controlling_process(Socket, Controller),
	    Client3 ! die_nice 
    end,

    ?CT_LOG("Waiting on exit ~p~n",[Client3]),
    receive {'EXIT', Client3, normal} -> ok end,
    
    receive   %% Client3 is dead but that doesn't matter, socket should not be closed.
	Unexpected ->
	    ?CT_LOG("Unexpected ~p~n",[Unexpected]),
	    ct:fail({line, ?LINE-1})
    after 1000 ->
	    ok
    end,
    Controller ! die_nice,
    get_close(Controller, ?LINE),
    
    %% Test that servers die
    Server ! listen, 
    LastClient = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					    {host, Hostname},
					    {from, self()}, 
					    {mfa, {?MODULE, 
						   controller_dies_result, [self(),
									    ClientMsg]}},
					    {options, ClientOpts}]),
    ct:sleep(?SLEEP), %% so that they are connected
    
    exit(Server, killed),
    get_close(Server, ?LINE),
    process_flag(trap_exit, false),
    ssl_test_lib:close(LastClient).
%%--------------------------------------------------------------------
controlling_process_transport_accept_socket() ->
    [{doc,"Only ssl:handshake and ssl:controlling_process is allowed for transport_accept:sockets"}].
controlling_process_transport_accept_socket(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_transport_control([{node, ServerNode}, 
                                                          {port, 0},
                                                          {from, self()},
                                                          {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    _Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                              {host, Hostname},
                                              {from, self()},
                                              {options, ClientOpts}]),
    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server).

%%--------------------------------------------------------------------
close_with_timeout() ->
      [{doc,"Test normal (not downgrade) ssl:close/2"}].
close_with_timeout(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, tls_close, []}},
					{options,[{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, tls_close, []}},
					{options, [{active, false} |ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok).

%%--------------------------------------------------------------------
close_in_error_state() ->
    [{doc,"Special case of closing socket in error state"}].
close_in_error_state(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{cacertfile, "foo.pem"} | proplists:delete(cacertfile, ServerOpts0)],
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    _ = spawn(?MODULE, run_error_server_close, [[self() | ServerOpts]]),
    receive
        {_Pid, Port} ->
            spawn_link(?MODULE, run_client_error, [[Port, [{verify, verify_none} | ClientOpts]]])
    end,
    receive
        ok ->
            ok;
        Other ->
            ct:fail(Other)
    end.

%%--------------------------------------------------------------------
call_in_error_state() ->
    [{doc,"Special case of call error handling"}].
call_in_error_state(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = [{cacertfile, "foo.pem"} | proplists:delete(cacertfile, ServerOpts0)],
    Pid = spawn(?MODULE, run_error_server, [[self() | ServerOpts]]),
    receive
        {Pid, Port} ->
            spawn_link(?MODULE, run_client_error, [[Port, [{verify, verify_none} | ClientOpts]]])
    end,
    receive
        {error, closed} ->
            ok;
        Other ->
            ct:fail(Other)
    end.
%%--------------------------------------------------------------------
close_transport_accept() ->
    [{doc,"Tests closing ssl socket when waiting on ssl:transport_accept/1"}].

close_transport_accept(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Port = 0,
    Opts = [{active, false} | ServerOpts],
    {ok, ListenSocket} = rpc:call(ServerNode, ssl, listen, [Port, Opts]),
    spawn_link(fun() ->
			ct:sleep(?SLEEP),
			rpc:call(ServerNode, ssl, close, [ListenSocket])
	       end),
    case rpc:call(ServerNode, ssl, transport_accept, [ListenSocket]) of
	{error, closed} ->
	    ok;
	Other ->
	    exit({?LINE, Other})
    end.
%%--------------------------------------------------------------------
abuse_transport_accept_socket() ->
    [{doc,"Only ssl:handshake and ssl:controlling_process is allowed for transport_accept:sockets"}].
abuse_transport_accept_socket(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_transport_abuse_socket([{node, ServerNode}, 
                                                               {port, 0},
                                                               {from, self()},
                                                               {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, no_result, []}},
                                        {options, ClientOpts}]),
    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

invalid_keyfile() ->
    [{doc,"Test what happens with an invalid key file"}].
invalid_keyfile(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    BadKeyFile = filename:join([proplists:get_value(priv_dir, Config), 
			      "badkey.pem"]),
    BadOpts = [{keyfile, BadKeyFile}| proplists:delete(keyfile, ServerOpts)],
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					 {from, self()},
			    {options, BadOpts}]),

    Port = ssl_test_lib:inet_port(Server),

    Client =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
			    {port, Port}, {host, Hostname},
			    {from, self()},  {options, ClientOpts}]),

    File = proplists:get_value(keyfile,BadOpts),    
    ssl_test_lib:check_result(Server,
                              {error,{options, {keyfile, File, {error,enoent}}}}, Client,
                              {error, closed}).

%%--------------------------------------------------------------------
honor_server_cipher_order_tls12() ->
    [{doc,"Test API honor server cipher order."}].
honor_server_cipher_order_tls12(Config) when is_list(Config) ->
    ClientCiphers = [#{key_exchange => ecdhe_rsa,
                       cipher => aes_128_gcm,
                       mac => aead,
                       prf => sha256},
                     #{key_exchange => ecdhe_rsa,
                       cipher => aes_256_gcm,
                       mac => aead,
                       prf => sha384}],
    ServerCiphers = [#{key_exchange => ecdhe_rsa,
                       cipher => aes_256_gcm,
                       mac => aead,
                       prf => sha384},
                     #{key_exchange => ecdhe_rsa,
                       cipher => aes_128_gcm,
                       mac => aead,
                       prf => sha256}],
    honor_cipher_order(Config, true, ServerCiphers,
                       ClientCiphers, #{key_exchange => ecdhe_rsa,
                                        cipher => aes_256_gcm,
                                        mac => aead,
                                        prf => sha384}).

%%--------------------------------------------------------------------

honor_client_cipher_order_tls12() ->
    [{doc,"Test API honor server cipher order."}].
honor_client_cipher_order_tls12(Config) when is_list(Config) ->
     ClientCiphers = [#{key_exchange => ecdhe_rsa,
                       cipher => aes_128_gcm,
                       mac => aead,
                       prf => sha256},
                     #{key_exchange => ecdhe_rsa,
                       cipher => aes_256_gcm,
                       mac => aead,
                       prf => sha384}],
    ServerCiphers = [#{key_exchange => ecdhe_rsa,
                       cipher => aes_256_gcm,
                       mac => aead,
                       prf => sha384},
                     #{key_exchange => ecdhe_rsa,
                       cipher => aes_128_gcm,
                       mac => aead,
                       prf => sha256}],
    honor_cipher_order(Config, false, ServerCiphers,
                       ClientCiphers, #{key_exchange => ecdhe_rsa,
                                        cipher => aes_128_gcm,
                                        mac => aead,
                                        prf => sha256}).

%%--------------------------------------------------------------------
honor_server_cipher_order() ->
    [{doc,"Test API honor server cipher order."}].
honor_server_cipher_order(Config) when is_list(Config) ->
    ClientCiphers = [#{key_exchange => dhe_rsa,
                       cipher => aes_128_cbc,
                       mac => sha,
                       prf => default_prf},
                     #{key_exchange => dhe_rsa,
                       cipher => aes_256_cbc,
                       mac => sha,
                       prf => default_prf}],
    ServerCiphers = [#{key_exchange => dhe_rsa,
                       cipher => aes_256_cbc,
                       mac => sha,
                       prf => default_prf},
                     #{key_exchange => dhe_rsa,
                       cipher => aes_128_cbc,
                       mac => sha,
                       prf => default_prf}],
    honor_cipher_order(Config, true, ServerCiphers,
                       ClientCiphers, #{key_exchange => dhe_rsa,
                                        cipher => aes_256_cbc,
                                        mac => sha,
                                        prf => default_prf}).

%%--------------------------------------------------------------------

honor_client_cipher_order() ->
    [{doc,"Test API honor server cipher order."}].
honor_client_cipher_order(Config) when is_list(Config) ->
     ClientCiphers = [#{key_exchange => dhe_rsa,
                       cipher => aes_128_cbc,
                       mac => sha,
                       prf => default_prf},
                     #{key_exchange => dhe_rsa,
                       cipher => aes_256_cbc,
                       mac => sha,
                       prf => default_prf}],
    ServerCiphers = [#{key_exchange => dhe_rsa,
                       cipher => aes_256_cbc,
                       mac => sha,
                       prf => default_prf},
                     #{key_exchange => dhe_rsa,
                       cipher => aes_128_cbc,
                       mac => sha,
                       prf => default_prf}],
    honor_cipher_order(Config, false, ServerCiphers,
                       ClientCiphers, #{key_exchange => dhe_rsa,
                                        cipher => aes_128_cbc,
                                        mac => sha,
                                        prf => default_prf}).


%%--------------------------------------------------------------------
ipv6() ->
    [{require, ipv6_hosts},
     {doc,"Test ipv6."}].
ipv6(Config) when is_list(Config) ->
    {ok, Hostname0} = inet:gethostname(),
    
    case lists:member(list_to_atom(Hostname0), ct:get_config(ipv6_hosts)) of
	true ->
	    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
	    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
	    {ClientNode, ServerNode, Hostname} = 
		ssl_test_lib:run_where(Config, ipv6),
	    Server = ssl_test_lib:start_server([{node, ServerNode}, 
				   {port, 0}, {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  
				    [inet6, {active, false} | ServerOpts]}]),
	    Port = ssl_test_lib:inet_port(Server), 
	    Client = ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port}, {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, 
				    [inet6, {active, false} | ClientOpts]}]),
	    
	    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),
	    
	    ssl_test_lib:check_result(Server, ok, Client, ok),
	    
	    ssl_test_lib:close(Server),
	    ssl_test_lib:close(Client);
	false ->
	    {skip, "Host does not support IPv6"}
    end.

%%--------------------------------------------------------------------
der_input() ->
    [{doc,"Test to input certs and key as der"}].

der_input(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    DHParamFile = filename:join(DataDir, "dHParam.pem"),

    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    [CADb | _] = element(5, State),
    ct:sleep(?SLEEP*2), %%Make sure there is no outstanding clean cert db msg in manager
    Size = ets:info(CADb, size),
    ?CT_LOG("Size ~p", [Size]),

    SeverVerifyOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ServerCert, ServerKey, ServerCaCerts, DHParams} = der_input_opts([{dhfile, DHParamFile} |
								       SeverVerifyOpts]),
    ClientVerifyOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    {ClientCert, ClientKey, ClientCaCerts, DHParams} = der_input_opts([{dhfile, DHParamFile} |
								       ClientVerifyOpts]),
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true},
		  {dh, DHParams},
		  {cert, ServerCert}, {key, ServerKey}, {cacerts, ServerCaCerts}],
    ClientOpts = [{verify, verify_peer},
		  {cert, ClientCert}, {key, ClientKey}, {cacerts, ClientCaCerts}],
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result, []}},
					{options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result, []}},
					{options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    %% Using only DER input should not increase file indexed DB 
    Size = ets:info(CADb, size),

    ServerOpts1 = [{verify, verify_peer}, {fail_if_no_peer_cert, true},
                   {dh, DHParams},
                   {cert, ServerCert}, {key, ServerKey},
                   {cacerts, [ #cert{der=Der, otp=public_key:pkix_decode_cert(Der, otp)}
                               || Der <- ServerCaCerts]}],
    ClientOpts1 = [{verify, verify_peer},
                   {cert, ClientCert}, {key, ClientKey},
                   {cacerts, [ #cert{der=Der, otp=public_key:pkix_decode_cert(Der, otp)}
                               || Der <- ClientCaCerts]}],
    Server1 = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                         {from, self()},
                                         {mfa, {ssl_test_lib, send_recv_result, []}},
                                         {options, [{active, false} | ServerOpts1]}]),
    Port1 = ssl_test_lib:inet_port(Server1),
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port1},
                                         {host, Hostname},
                                         {from, self()},
                                         {mfa, {ssl_test_lib, send_recv_result, []}},
                                         {options, [{active, false} | ClientOpts1]}]),

    ssl_test_lib:check_result(Server1, ok, Client1, ok),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1),
    %% Using only DER input should not increase file indexed DB 
    Size = ets:info(CADb, size),

    ok.

%%--------------------------------------------------------------------
invalid_certfile() ->
    [{doc,"Test what happens with an invalid cert file"}].

invalid_certfile(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    BadCertFile = filename:join([proplists:get_value(priv_dir, Config), 
                                 "badcert.pem"]),
    ServerBadOpts = [{certfile, BadCertFile}| proplists:delete(certfile, ServerOpts)],
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),


    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = 
	ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					 {from, self()},
					 {options, ServerBadOpts}]),

    Port = ssl_test_lib:inet_port(Server),

    Client =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
					 {port, Port}, {host, Hostname},
					 {from, self()}, 
					 {options, ClientOpts}]),
    File = proplists:get_value(certfile, ServerBadOpts),
    ssl_test_lib:check_result(Server, {error,{options, {certfile, File, {error,enoent}}}}, 
			      Client, {error, closed}).
    

%%--------------------------------------------------------------------
invalid_cacertfile() ->
    [{doc,"Test what happens with an invalid cacert file"}].

invalid_cacertfile(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    BadCACertFile = filename:join([proplists:get_value(priv_dir, Config), 
                                 "badcacert.pem"]),
    ServerBadOpts = [{cacertfile, BadCACertFile}| proplists:delete(cacertfile, ServerOpts)],
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server0  = 
	ssl_test_lib:start_server_error([{node, ServerNode}, 
					 {port, 0}, {from, self()},
					 {options, ServerBadOpts}]),

    Port0 = ssl_test_lib:inet_port(Server0),
    

    Client0 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
					 {port, Port0}, {host, Hostname},
					 {from, self()}, 
					 {options, ClientOpts}]),

    File0 = proplists:get_value(cacertfile, ServerBadOpts),
    
    ssl_test_lib:check_result(Server0, {error, {options, {cacertfile, File0,{error,enoent}}}},
			      Client0, {error, closed}),
    
    File = File0 ++ "do_not_exit.pem",
    ServerBadOpts1 = [{cacertfile, File}|proplists:delete(cacertfile, ServerBadOpts)],
            
    Server1  = 
	ssl_test_lib:start_server_error([{node, ServerNode}, 
					 {port, 0}, {from, self()},
					 {options, ServerBadOpts1}]),

    Port1 = ssl_test_lib:inet_port(Server1),
    
    Client1 =
	ssl_test_lib:start_client_error([{node, ClientNode}, 
					 {port, Port1}, {host, Hostname},
					 {from, self()}, 
					 {options, ClientOpts}]),


    ssl_test_lib:check_result(Server1, {error, {options, {cacertfile, File,{error,enoent}}}},
			      Client1, {error, closed}),
    ok.

%%--------------------------------------------------------------------
new_options_in_handshake() ->
    [{doc,"Test that you can set ssl options in handshake/3 and not only in tcp upgrade"}].
new_options_in_handshake(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Version = ssl_test_lib:protocol_version(Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Filter = fun(any) when Version =:= 'tlsv1.3' ->
                     true;
                (_) when Version =:= 'tlsv1.3' ->
                     false;
                (dhe_rsa) ->
                     true;
                (ecdhe_rsa) ->
                     true;
                (rsa) ->
                     false;
                (_) ->
                     false
             end,

    Ciphers = [_, Cipher | _] = ssl:filter_cipher_suites(ssl:cipher_suites(all, Version), 
                                                         [{key_exchange, Filter}]),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{ssl_extra_opts, [{versions, [Version]},
							  {ciphers,[Cipher]}]}, %% To be set in handshake/3
					{mfa, {?MODULE, connection_info_result, []}},
					{options, ServerOpts}]),
    
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, connection_info_result, []}},
					{options, [{ciphers, Ciphers} | ClientOpts]}]),
    
    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ServerMsg = ClientMsg = {ok, {Version, Cipher}},
   
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%-------------------------------------------------------------------
max_handshake_size() ->
    [{doc,"Test that we can set max_handshake_size to max value."}].

max_handshake_size(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),  

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options,  [{max_handshake_size, 8388607} |ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{max_handshake_size, 8388607} | ClientOpts]}]),
 
    ssl_test_lib:check_result(Server, ok, Client, ok).
  

%%-------------------------------------------------------------------

-define(FUNC_CLAUSE(EXPR), try EXPR, error(should_fail) catch _:function_clause -> ok end).

options_not_proplist() ->
    [{doc,"Test what happens if an option is not a key value tuple"}].

options_not_proplist(Config) when is_list(Config) ->
    BadOption =  {client_preferred_next_protocols, 
		  client, [<<"spdy/3">>,<<"http/1.1">>], <<"http/1.1">>},
    {error, {options, {option_not_a_key_value_tuple, BadOption}}} =
	ssl:connect("twitter.com", 443, [binary, {active, false}, 
					 BadOption]),
    ?FUNC_CLAUSE(ssl:connect("twitter.com", 443)),
    ?FUNC_CLAUSE(ssl:connect("twitter.com", 443, infinity)),
    ?FUNC_CLAUSE(ssl:connect("twitter.com", [], infinity_misspelled)),
    ?FUNC_CLAUSE(ssl:connect("twitter.com", foo, [], infinity)),
    %% While at it test some other functions as well for regression testing
    ?FUNC_CLAUSE(ssl:listen([], 443)),
    ?FUNC_CLAUSE(ssl:transport_accept(#sslsocket{}, [])),
    ?FUNC_CLAUSE(ssl:handshake(#sslsocket{}, [])),
    ?FUNC_CLAUSE(ssl:handshake(foo, #sslsocket{})),
    ?FUNC_CLAUSE(ssl:handshake(foo, #sslsocket{}, 1000)),
    ?FUNC_CLAUSE(ssl:handshake(#sslsocket{}, 1000, 1000)),
    ?FUNC_CLAUSE(ssl:handshake(#sslsocket{}, [opt_list], [])),
    ?FUNC_CLAUSE(ssl:handshake_continue(socket, [opt_list], [])),
    ?FUNC_CLAUSE(ssl:handshake_continue(socket, 1000, 1000)),

    ok.

options_whitebox() ->
    [{doc,"Whitebox tests of option handling"}].


customize_defaults(Opts, Role, Host) ->
    %% In many options test scenarios we do not care about verifcation options
    %% but the client now requiers verification options by default.
    DefOpts = case Role of
                  client ->
                      case proplists:get_value(verify, Opts, undefined) of
                          undefined -> [{verify, verify_none}];
                          _ -> []
                      end;
                  server ->
                      Ciphers = proplists:get_value(ciphers, Opts, undefined),
                      Cert = proplists:get_value(cert, Opts, undefined),
                      Key = proplists:get_value(key, Opts, undefined),
                      CertsKeys = proplists:get_value(certs_keys, Opts, undefined),
                      NoCertOrKeys = Cert == undefined orelse Key == undefined andalso
                          CertsKeys == undefined,
                      if Ciphers == undefined andalso NoCertOrKeys ->
                              [{certs_keys, [#{cert => <<>>, key => {rsa, <<>>}}]}];
                         true ->
                              []
                      end
              end,
    NoVerify = case Role of
                   client -> [{verify, verify_none}|DefOpts];
                   server -> DefOpts
               end,
    case proplists:get_value(protocol, Opts, tls) of
        dtls ->
            {ok, #config{ssl=DOpts}} = ssl_config:handle_options([{protocol, dtls}|NoVerify], Role, Host),
            {DOpts, DefOpts ++ Opts};
        tls ->
            {ok, #config{ssl=DOpts}} = ssl_config:handle_options(NoVerify, Role, Host),
            case proplists:get_value(versions, Opts) of
                undefined ->
                    {DOpts, DefOpts ++ [{versions, ['tlsv1.2','tlsv1.3']}|Opts]};
                _ ->
                    {DOpts, DefOpts ++ Opts}
            end;
        _ ->
            {ok, #config{ssl=DOpts}} = ssl_config:handle_options(NoVerify, Role, Host),
            {DOpts, DefOpts ++ Opts}
    end.

-define(OK(EXP, Opts, Role), ?OK(EXP,Opts, Role, [])).
-define(OK(EXP, Opts, Role, ShouldBeMissing),
        fun() ->
                Host = "dummy.host.org",
                {__DefOpts, __Opts} = customize_defaults(Opts, Role, Host),
                try ssl_config:handle_options(__Opts, Role, Host) of
                    {ok, #config{ssl=EXP = __ALL}} ->
                        check_expected(ShouldBeMissing, ShouldBeMissing -- maps:keys(__ALL)),
                        check_expected(__ALL, ssl_config:update_options([], Role, __ALL));
                    Other ->
                        ?CT_PAL("ssl_config:handle_options(~0p,~0p,~0p).",[__Opts,Role,Host]),
                        error({unexpected, Other})
                catch
                    throw:{error,{options,{insufficient_crypto_support,{'tlsv1.3',_}}}} -> ignored;
                    C:Other:ST ->
                        ?CT_PAL("ssl_config:handle_options(~0p,~0p,~0p).",[__Opts,Role,Host]),
                        error({unexpected, C, Other,ST})
                end,
                try ssl_config:update_options(__Opts, Role, __DefOpts) of
                    EXP = __ALL2 ->
                        check_expected(ShouldBeMissing, ShouldBeMissing -- maps:keys(__ALL2));
                    Other2 ->
                        ?CT_PAL("{ok,Cfg} = ssl_config:handle_options([],~p,~p),"
                               "ssl_config:update_options(~w,~w, element(2,Cfg)).",
                               [Role,Host,__Opts,Role]),
                        error({unexpected2, Other2})
                catch
                    throw:{error,{options,{insufficient_crypto_support,{'tlsv1.3',_}}}} -> ignored;
                    C2:Other2:ST2 ->
                        ?CT_PAL("{ok,Cfg} = ssl_config:handle_options([],~p,~p),"
                               "ssl_config:update_options(~p,~p, element(2,Cfg)).",
                               [Role,Host,__Opts,Role]),
                        error({unexpected, C2, Other2, ST2})
                end
        end()).

-define(ERR(EXP, Opts, Role),
        fun() ->
                Host = "dummy.host.org",
                {__DefOpts, __Opts} = customize_defaults(Opts, Role, Host),
                try ssl_config:handle_options(__Opts, Role, Host) of
                    Other ->
                        ?CT_PAL("ssl_config:handle_options(~0p,~0p,~0p).",[__Opts,Role,Host]),
                        error({unexpected, Other})
                catch
                    throw:{error,{options,{insufficient_crypto_support,{'tlsv1.3',_}}}} -> ignored;
                    throw:{error, {options, EXP}} -> ok;
                    throw:{error, EXP} -> ok;
                    C:Other:ST ->
                        ?CT_PAL("ssl_config:handle_options(~0p,~0p,~0p).",[__Opts,Role,Host]),
                        error({unexpected, C, Other,ST})
                end,
                try ssl_config:update_options(__Opts, Role, __DefOpts) of
                    Other2 ->
                        ?CT_PAL("{ok,Cfg} = ssl_config:handle_options([],~p,~p),"
                               "ssl_config:update_options(~p,~p, element(2,Cfg)).",
                               [Role,Host,__Opts,Role]),
                        error({unexpected, Other2})
                catch
                    throw:{error,{options,{insufficient_crypto_support,{'tlsv1.3',_}}}} -> ignored;
                    throw:{error, {options, EXP}} -> ok;
                    throw:{error, EXP} -> ok;
                    C2:Other2:ST2 ->
                        ?CT_PAL("{ok,Cfg} = ssl_config:handle_options([],~p,~p),"
                               "ssl_config:update_options(~p,~p, element(2,Cfg)).",
                               [Role,Host,__Opts,Role]),
                        error({unexpected, C2, Other2,ST2})
                end
        end()).

-define(ERR_UPD(EXP, Opts, Role),
        fun() ->
                Host = "dummy.host.org",
                {__DefOpts, __Opts} = customize_defaults(Opts, Role, Host),
                try ssl_config:handle_options(__Opts, Role, Host) of
                    {ok, #config{}} ->
                        ok;
                    Other ->
                        ?CT_PAL("ssl_config:handle_options(~0p,~0p,~0p).",[__Opts,Role,Host]),
                        error({unexpected, Other})
                catch
                    throw:{error,{options,{insufficient_crypto_support,{'tlsv1.3',_}}}} -> ignored;
                    C:Other:ST ->
                        ?CT_PAL("ssl_config:handle_options(~0p,~0p,~0p).",[__Opts,Role,Host]),
                        error({unexpected, C, Other,ST})
                end,
                try ssl_config:update_options(__Opts, Role, __DefOpts) of
                    Other2 ->
                        ?CT_PAL("{ok,Cfg} = ssl_config:handle_options([],~p,~p),"
                                "ssl_config:update_options(~p,~p, element(2,Cfg)).",
                                [Role,Host,__Opts,Role]),
                        error({unexpected, Other2})
                catch
                    throw:{error,{options,{insufficient_crypto_support,{'tlsv1.3',_}}}} -> ignored;
                    throw:{error, {options, EXP}} -> ok;
                    throw:{error, EXP} -> ok;
                    C2:Other2:ST2 ->
                        ?CT_PAL("{ok,Cfg} = ssl_config:handle_options([],~p,~p),"
                                "ssl_config:update_options(~p,~p, element(2,Cfg)).",
                                [Role,Host,__Opts,Role]),
                        error({unexpected, C2, Other2,ST2})
                end
        end()).

check_expected(A, A) ->
    ok;
check_expected(A, B) when is_list(A), is_list(B) ->
    Diff1 = A -- B,
    Diff2 = B -- A,
    ct:log("NOT EQUAL~n ~p~n ~p~n", [A,B]),
    if Diff1 =/= [], Diff2 =/= [] ->
            ct:fail({not_equal, {line, ?LINE}, Diff1, Diff2});
       Diff2 =/= [] ->
            ct:fail({not_equal, {line, ?LINE}, Diff2});
       true ->
            ct:fail({not_equal, {line, ?LINE}, Diff1})
    end;
check_expected(A, B) when is_map(A), is_map(B) ->
    Diff1 = [{KeyA, ValA}
             || KeyA := ValA <- A, ValA =/= maps:get(KeyA, B, missing_key_val)],
    Diff2 = [{KeyB, ValB}
             || KeyB := ValB <- B, ValB =/= maps:get(KeyB, A, missing_key_val)],
    ct:log("NOT EQUAL~n ~p~n ~p~n", [A,B]),
    if Diff1 =/= [], Diff2 =/= [] ->
            ct:fail({not_equal, {line, ?LINE}, Diff1, Diff2});
       Diff2 =/= [] ->
            ct:fail({not_equal, {line, ?LINE}, Diff2});
       true ->
            ct:fail({not_equal, {line, ?LINE}, Diff1})
    end;
check_expected({ok, Term1}, {ok, Term2}) ->
    check_expected(Term1, Term2);
check_expected(A, B) ->
    ct:log("NOT EQUAL~n ~p~n~p~n", [A,B]),
    ct:fail(not_equal).


options_whitebox(Config) when is_list(Config) ->
    Cert = proplists:get_value(cert, ssl_test_lib:ssl_options(server_rsa_der_opts, Config)),
    true = is_binary(Cert),
    ?OK(#{verify := verify_peer},  %% Check Option order last wins
        [{verify, verify_none}, {verify,verify_peer}, {cacerts, [Cert]}], client),
    options_protocol(Config),
    options_version(Config),
    options_alpn(Config),
    options_anti_replay(Config),
    options_beast_mitigation(Config),
    options_cacerts(Config),
    options_cert(Config),
    options_certificate_authorities(Config),
    options_ciphers(Config),
    options_client_renegotiation(Config),
    options_cookie(Config),
    options_crl(Config),
    options_hostname_check(Config),
    options_dh(Config),
    options_early_data(Config),
    options_eccs(Config),
    options_verify(Config),
    options_fallback(Config),
    options_handshake(Config),
    options_process(Config),
    options_honor(Config),
    options_debug(Config),
    options_renegotiate(Config),
    options_middlebox(Config),
    options_frag_len(Config),
    options_stapling(Config),
    options_padding(Config),
    options_identity(Config),
    options_reuse_session(Config),
    options_sni(Config),
    options_sign_alg(Config),
    options_supported_groups(Config),
    options_use_srtp(Config),
    ok.

options_protocol(_Config) ->
    ?OK(#{protocol := tls},  [], client),
    ?OK(#{protocol := tls},  [{protocol, tls}], client),
    ?OK(#{protocol := dtls}, [{protocol, dtls}], client),

    %% Errors
    ?ERR({protocol, foo}, [{protocol, 'foo'}], client),

    begin %% erl_dist
        ?OK(#{}, [], client, [erl_dist]),
        ?OK(#{}, [{erl_dist, false}], client, [erl_dist]),
        ?OK(#{erl_dist := true}, [{erl_dist, true}], client),
        ?OK(#{}, [], client, [ktls]),
        ?OK(#{}, [{ktls, false}], client, [ktls]),
        ?OK(#{ktls := true}, [{ktls, true}], client)
    end,
    ok.

options_version(_Config) ->
    ?OK(#{versions := [_|_]}, [], client),  %% Hmm some machines still default only ?TLS_1_2
    ?OK(#{versions := [?DTLS_1_2]}, [{protocol, dtls}], client),

    ?OK(#{versions := [?TLS_1_3,?TLS_1_2,?TLS_1_1,?TLS_1_0]},
        [{versions, ['tlsv1','tlsv1.1','tlsv1.2','tlsv1.3']}],
        client),
    ?OK(#{versions := [?TLS_1_3]}, [{versions, ['tlsv1.3']}], client),

    ?OK(#{versions := [?DTLS_1_2,?DTLS_1_0]},
        [{protocol, dtls}, {versions, ['dtlsv1', 'dtlsv1.2']}],  client),
    ?OK(#{versions := [?DTLS_1_2]}, [{protocol, dtls}, {versions, ['dtlsv1.2']}], client),


    %% Errors
    ?ERR({versions, []}, [{versions, []}], client),
    ?ERR({versions, []}, [{protocol, dtls}, {versions, []}], client),

    ?ERR({'tlsv1.4',{versions, ['tlsv1.4']}},
         [{versions, ['tlsv1.4']}], client),
    ?ERR({'dtlsv1', {versions, _}},
         [{versions, ['dtlsv1', 'dtlsv1.2']}], client),

    ?ERR({'tlsv1', {versions, _}},
         [{protocol, dtls}, {versions, ['tlsv1','tlsv1.1','tlsv1.2','tlsv1.3']}],
         client),
    ?ERR({'dtlsv1.3',{versions, ['dtlsv1.3']}},
         [{protocol, dtls}, {versions, ['dtlsv1.3']}],
         client),
    ?ERR({options,missing_version,{'tlsv1.2',_}},
         [{versions, ['tlsv1.1','tlsv1.3']}],
         client),
    ok.

options_alpn(_Config) -> %% alpn & next_protocols
    Http = <<"HTTP/2">>,
    ?OK(#{alpn_advertised_protocols := undefined}, [], client,
        [alpn_preferred_protocols, next_protocol_selector, next_protocols_advertised]),
    ?OK(#{alpn_preferred_protocols := undefined},  [], server,
        [alpn_advertised_protocols, next_protocol_selector, next_protocols_advertised]),

    ?OK(#{alpn_preferred_protocols := [Http]}, [{alpn_preferred_protocols, [Http]}],
        server, [alpn_advertised_protocols, next_protocol_selector, next_protocols_advertised]),
    ?OK(#{alpn_advertised_protocols := [Http]}, [{alpn_advertised_protocols, [Http]}],
        client, [alpn_preferred_protocols, next_protocol_selector, next_protocols_advertised]),

    %% Note names have been swapped in client/server variants

    ?OK(#{alpn_preferred_protocols := undefined, next_protocols_advertised := [Http]},
        [{next_protocols_advertised, [Http]}], server, [alpn_advertised_protocols, next_protocol_selector]),
    ?OK(#{alpn_advertised_protocols := undefined, next_protocol_selector := _},
        [{client_preferred_next_protocols, {server,[Http], Http}}],
        client, [alpn_preferred_protocols, next_protocols_advertised]),

    %% Errors
    ?ERR({alpn_preferred_protocols, {invalid_protocol, <<>>}}, [{alpn_preferred_protocols, [Http, <<>>]}], server),
    ?ERR({alpn_advertised_protocols, Http}, [{alpn_advertised_protocols, Http}], client),
    ?ERR({alpn_preferred_protocols, undefined}, [{alpn_preferred_protocols, undefined}], server),
    ?ERR({alpn_advertised_protocols, undefined}, [{alpn_advertised_protocols, undefined}], client),
    ?ERR({option, server_only, alpn_preferred_protocols}, [{alpn_preferred_protocols, [Http]}], client),
    ?ERR({option, client_only, alpn_advertised_protocols}, [{alpn_advertised_protocols, [Http]}], server),
    ?ERR({option, server_only, next_protocols_advertised}, [{next_protocols_advertised, [Http]}], client),
    ?ERR({option, client_only, client_preferred_next_protocols}, [{client_preferred_next_protocols, [Http]}], server),
    ok.

options_anti_replay(_Config) ->
    ?OK(#{anti_replay := undefined}, [], server),
    ?OK(#{anti_replay := {_,_,_}},
        [{anti_replay, '10k'}, {session_tickets, stateless}],
        server),
    ?OK(#{anti_replay := {_,_,_}},
        [{anti_replay, {42,4711,21}}, {session_tickets, stateless}],
        server),
    ?OK(#{anti_replay := {_,_,_}},
        [{anti_replay, '10k'}, {session_tickets, stateless_with_cert}],
        server),
    ?OK(#{anti_replay := {_,_,_}},
        [{anti_replay, {42,4711,21}}, {session_tickets, stateless_with_cert}],
        server),


    %% Errors
    ?ERR({option, server_only, anti_replay},
         [{anti_replay, '10k'}, {session_tickets, manual}],
         client),
    ?ERR({options, incompatible, [{anti_replay, _}, {session_tickets, disabled}]},
         [{anti_replay, '10k'}],
         server),
    ?ERR({options,incompatible, [session_tickets,{versions,['tlsv1']}]},
         [{anti_replay, '10k'}, {session_tickets, stateless}, {versions, ['tlsv1']}],
         server),
    ?ERR({anti_replay, '1k'},
         [{anti_replay, '1k'}, {session_tickets, stateless}],
         server),
    ?ERR({anti_replay, _},
         [{anti_replay, {1,1,1,1}}, {session_tickets, stateless}],
         server),
    ?ERR({options,incompatible, [session_tickets,{versions,['tlsv1']}]},
         [{anti_replay, '10k'}, {session_tickets, stateless_with_cert}, {versions, ['tlsv1']}],
         server),
    ?ERR({anti_replay, '1k'},
         [{anti_replay, '1k'}, {session_tickets, stateless_with_cert}],
         server),
    ?ERR({anti_replay, _},
         [{anti_replay, {1,1,1,1}}, {session_tickets, stateless_with_cert}],
         server),
    ok.

options_beast_mitigation(_Config) -> %% Beast mitigation TLS-1.0 option only
    ?OK(#{beast_mitigation := one_n_minus_one}, [{versions, [tlsv1,'tlsv1.1']}], client),

    ?OK(#{}, [{versions, ['tlsv1.1']}], client, [beast_mitigation]),
    ?OK(#{}, [{beast_mitigation, disabled}, {versions, [tlsv1]}], client),
    ?OK(#{beast_mitigation := zero_n}, [{beast_mitigation, zero_n}, {versions, [tlsv1]}], client),

    %% Errors
    ?ERR({beast_mitigation, enabled},
         [{beast_mitigation, enabled}, {versions, [tlsv1]}], client),
    ?ERR({options, incompatible, [beast_mitigation, {versions, _}]},
         [{beast_mitigation, disabled}], client),
    ok.

options_cacerts(Config) ->  %% cacert[s]file
    Cert = proplists:get_value(cert, ssl_test_lib:ssl_options(server_rsa_der_opts, Config)),
    File = case os:type() of
               {win32, _} -> <<"c:/tmp/foo">>;
               _ -> <<"/tmp/foo">>
           end,
    ?OK(#{cacerts := undefined}, [], client, [cacertfile]),
    ?OK(#{cacerts := undefined, cacertfile := File},
        [{cacertfile, File}], client),
    ?OK(#{cacerts := [Cert]}, [{cacerts, [Cert]}, {verify, verify_peer}],
        client, [cacertfile]),
    ?OK(#{cacerts := [#cert{}]}, [{cacerts, [#cert{der=Cert, otp=dummy}]}],
        client, [cacertfile]),
    ?OK(#{cacerts := [Cert]}, [{cacerts, [Cert]}, {cacertfile, "/tmp/foo"}],
        client, [cacertfile]),

    %% Errors
    ?ERR({options, incompatible, _}, [{verify, verify_peer}], server),
    ?ERR({cacerts, Cert}, [{cacerts, Cert}], client),
    ?ERR({cacertfile, cert}, [{cacertfile, cert}], client),

    ?OK(#{}, [], client, [depth]),
    ?OK(#{depth := 5}, [{depth, 5}], client),
    %% Error
    ?ERR({depth, 256}, [{depth, 256}], client),
    ?ERR({depth, not_an_int}, [{depth, not_an_int}], client),
    ok.

options_cert(Config) -> %% cert[file] cert_keys keys password
    Cert = proplists:get_value(cert, ssl_test_lib:ssl_options(server_rsa_der_opts, Config)),
    Old = [cert, certfile, key, keyfile, password],
    ?OK(#{certs_keys := []}, [], client, Old),
    ?OK(#{certs_keys := [#{cert := [Cert]}]}, [{cert,Cert}], client, Old),
    ?OK(#{certs_keys := [#{cert := [Cert]}]}, [{cert,[Cert]}], client, Old),
    ?OK(#{certs_keys := [#{certfile := <<"/tmp/foo">>, keyfile := <<"/tmp/foo">>}]},
        [{certfile, <<"/tmp/foo">>}], client, Old),

    ?OK(#{certs_keys := []}, [{certs_keys, [#{}]}], client),

    ?OK(#{certs_keys := [#{key := {rsa, <<>>}}]},
        [{key, {rsa, <<>>}}], client, Old),
    ?OK(#{certs_keys := [#{key := #{}}]},
        [{key, #{engine => foo, algorithm => foo, key_id => foo}}], client, Old),
    ?OK(#{certs_keys := [#{key := #{}}]},
        [{key, #{algorithm => eddsa, sign_fun => fun(_,_,_,_) -> << "dummy signature">> end}},
         {versions, ['tlsv1.3']}], client, Old),
    ?OK(#{certs_keys := [#{key := #{}}]},
        [{key, #{algorithm => ecdsa, sign_fun => fun(_,_,_,_) -> << "dummy signature">> end}}],
        client, Old),
    ?OK(#{certs_keys := [#{key := #{}}]},
        [{key, #{algorithm => rsa,
                  sign_fun => fun(_,_,_,_) -> << "dummy signature">> end,
                 encrypt_fun => fun(_,_,_) -> << "dummy encrypt">> end}},
         {versions, ['tlsv1.3', 'tlsv1.2', 'tlsv1.1']}], client, Old),
    ?OK(#{certs_keys := [#{password := _}]}, [{password, "foobar"}], client, Old),
    ?OK(#{certs_keys := [#{password := _}]}, [{password, <<"foobar">>}], client, Old),
    Pwd = fun() -> "foobar" end,
    ?OK(#{certs_keys := [#{password := Pwd}]}, [{password, Pwd}], client, Old),

    ?OK(#{certs_keys := [#{certfile := <<"/tmp/foo">>, keyfile := <<"/tmp/baz">>}]},
        [{certfile, <<"/tmp/foo">>}, {keyfile, "/tmp/baz"}], client, Old),

    ?OK(#{certs_keys := []},
        [{cert, Cert}, {certfile, "/tmp/foo"}, {certs_keys, [#{}]}],
        client, Old),

    %% Errors
    ?ERR({options,incompatible,[key,{versions,['tlsv1.2']}]},
         [{key, #{algorithm => eddsa, sign_fun => fun(_,_,_,_) -> << "dummy signature">> end}},
          {versions, ['tlsv1.2']}], client),
    ?ERR({options,incompatible,[key,{versions,['tlsv1.3','tlsv1.2']}]},
         [{key, #{algorithm => rsa,
                  sign_fun => fun(_,_,_,_) -> << "dummy signature">> end,
                  encrypt_fun => fun(_,_,_) -> << "dummy encrypt">> end
                 }}], client),
    ?ERR({cert, #{}}, [{cert, #{}}], client),
    ?ERR({certfile, cert}, [{certfile, cert}], client),
    ?ERR({certs_keys, #{}}, [{certs_keys, #{}}], client),
    ?ERR_UPD({certs_keys, cert_and_key_required}, [{certs_keys, []}], server),
    ?ERR({keyfile, #{}}, [{keyfile, #{}}], client),
    ?ERR({key, <<>>}, [{key, <<>>}], client),
    ?ERR({password, _}, [{password, fun(Arg) -> Arg end}], client),
    ok.

options_certificate_authorities(_Config) ->
    ?OK(#{}, [], client, [certificate_authorities]),
    ?OK(#{}, [], server, [certificate_authorities]),
    ?OK(#{certificate_authorities := true}, [{certificate_authorities, true}], client),
    ?OK(#{}, [{certificate_authorities, false}], client, [certificate_authorities]),
    ?OK(#{certificate_authorities := false}, [{certificate_authorities, false}], server),
    ?OK(#{certificate_authorities := false}, [{certificate_authorities, false}, {versions, ['tlsv1.2']}], server),
    ?OK(#{}, [{certificate_authorities, true}], server, [certificate_authorities]),

    %% Errors
    ?ERR({certificate_authorities, []},
         [{certificate_authorities,  []}], client),
    ?ERR({options, incompatible, [certificate_authorities, {versions, _}]},
         [{certificate_authorities, true}, {versions, ['tlsv1.2']}],
         client),
    ok.

options_ciphers(_Config) ->
    CipherSuite = ssl_test_lib:ecdh_dh_anonymous_suites(?TLS_1_2),
    ?OK(#{ciphers := [_|_]}, [], client),
    ?OK(#{ciphers := [_|_]}, [{ciphers, CipherSuite}], client),
    ?OK(#{ciphers := [_|_]}, [{ciphers, "RC4-SHA:RC4-MD5"}], client),
    ?OK(#{ciphers := [_|_]}, [{ciphers, ["RC4-SHA", "RC4-MD5"]}], client),

    ?OK(#{ciphers := [_|_]}, [{ciphers, ["TLS_DH_anon_WITH_AES_256_CBC_SHA256"]}], server),
    %% Errors
    ?ERR({ciphers, _}, [{ciphers, "foobar:RC4-MD5"}], client),
    ?ERR({ciphers, _}, [{ciphers, ["RC4-SHA:RC4-MD5", "RC4-SHA:RC4-MD5"]}], client),
    ?ERR_UPD({certs_keys, cert_and_key_required}, [{ciphers, "RC4-SHA:RC4-MD5"}], server),
    ok.

options_client_renegotiation(_Config) ->
    ?OK(#{client_renegotiation := true}, [], server),
    ?OK(#{client_renegotiation := false}, [{client_renegotiation, false}], server),

    %% Errors
    ?ERR({client_renegotiation, []}, [{client_renegotiation,  []}], server),
    ?ERR({option, server_only, client_renegotiation}, [{client_renegotiation, true}], client),
    ?ERR({options, incompatible, [client_renegotiation, {versions, _}]},
         [{client_renegotiation, true}, {versions, ['tlsv1.3']}],
         server),
    ok.


options_cookie(_Config) ->
    ?OK(#{cookie := true},  [], server),
    ?OK(#{cookie := false}, [{cookie, false}], server),

    %% Errors
    ?ERR({cookie, []}, [{cookie,  []}], server),
    ?ERR({option, server_only, cookie}, [{cookie, true}], client),
    ?ERR({options, incompatible, [cookie, {versions, _}]},
         [{cookie, true}, {versions, ['tlsv1.2']}], server),
    ok.

options_crl(_Config) ->
    ?OK(#{crl_cache := {ssl_crl_cache, _}, crl_check := false}, [], server),
    ?OK(#{crl_cache := {ssl_crl_cache, _}, crl_check := true},  [{crl_check, true}], server),
    ?OK(#{crl_cache := {ssl_crl_hash_dir, {_,_}}, crl_check := true},
        [{crl_check, true}, {crl_cache, {ssl_crl_hash_dir, {internal, [{dir, "/tmp"}]}}}],
        server),
    ?OK(#{crl_cache := {ssl_crl_cache, {_,_}}, crl_check := true},
        [{crl_check, true}, {crl_cache, {ssl_crl_cache, {internal, [{http, 5000}]}}}],
        server),
    %% Errors
    ?ERR({crl_check, foo}, [{crl_check, foo}], server),
    ?ERR({crl_cache, {a,b,c}}, [{crl_cache, {a,b,c}}], server),
    ok.

options_hostname_check(_Config) ->
    ?OK(#{customize_hostname_check := []}, [], client),
    ?OK(#{customize_hostname_check := [{match_fun, _}]},
        [{customize_hostname_check, [{match_fun, fun() -> ok end}]}],
        client),
    %% Error
    ?ERR({customize_hostname_check, _}, [{customize_hostname_check, {match_fun, pb_fun}}], client),
    ok.

options_dh(_Config) -> %% dh dhfile
    ?OK(#{}, [], server, [dh, dhfile]),
    ?OK(#{dh := <<>>}, [{dh, <<>>}], server, [dhfile]),
    ?OK(#{dhfile := <<"/tmp/foo">>}, [{dhfile, <<"/tmp/foo">>}], server, [dh]),
    ?OK(#{dh := <<>>}, [{dh, <<>>}, {dhfile, <<"/tmp/foo">>}], server, [dhfile]),

    %% Should be an error
    ?OK(#{dhfile := <<"/tmp/foo">>},  %% Not available in 1.3
        [{dhfile, <<"/tmp/foo">>}, {versions, ['tlsv1.3']}], server, [dh]),

    %% Error
    ?ERR({dh, not_a_bin}, [{dh, not_a_bin}], server),
    ?ERR({dhfile, not_a_filename}, [{dhfile, not_a_filename}], server),
    ?ERR({option, server_only, dhfile}, [{dhfile, "file"}], client),
    ?ERR({option, server_only, dh}, [{dh, <<"DER">>}], client),
    ok.

options_early_data(_Config) -> %% early_data, session_tickets and use_ticket
    ?OK(#{early_data := undefined, session_tickets := disabled},
        [], client),
    ?OK(#{early_data := disabled, session_tickets := disabled, stateless_tickets_seed := undefined},
        [], server),

    ?OK(#{early_data := <<>>, session_tickets := auto},
        [{early_data, <<>>}, {session_tickets, auto}], client),
    ?OK(#{early_data := <<>>, session_tickets := manual, use_ticket := [<<1>>]},
        [{early_data, <<>>}, {session_tickets, manual}, {use_ticket, [<<1>>]}],
        client),

    ?OK(#{early_data := enabled, stateless_tickets_seed := <<"foo">>},
        [{early_data, enabled}, {session_tickets, stateless}, {stateless_tickets_seed, <<"foo">>}], server),
    ?OK(#{early_data := enabled, stateless_tickets_seed := <<"foo">>},
        [{early_data, enabled}, {session_tickets, stateless_with_cert}, {stateless_tickets_seed, <<"foo">>}], server),

    ?OK(#{early_data := disabled}, [{early_data, disabled}], server),

    %% Errors
    ?ERR({option, client_only, use_ticket}, [{use_ticket, []}], server),
    ?ERR({options, incompatible, _}, [{use_ticket, [<<>>]}], client),

    ?ERR({options, {session_tickets, foo}}, [{session_tickets, foo}], server),
    ?ERR({options, {session_tickets, manual}}, [{session_tickets, manual}], server),
    ?ERR({options, {session_tickets, stateful}}, [{session_tickets, stateful}], client),
    ?ERR({options, incompatible, [session_tickets, {versions, _}]},
         [{session_tickets, stateful}, {versions, ['tlsv1.2']}], server),
    ?ERR({options, incompatible, [session_tickets, {versions, _}]},
         [{session_tickets, stateful_with_cert}, {versions, ['tlsv1.2']}], server),

    ?ERR({use_ticket, foo},
         [{use_ticket, foo}, {session_tickets, manual}], client),

    ?ERR({early_data,undefined}, [{early_data, undefined}], client),
    ?ERR({options, incompatible, [early_data, {session_tickets, _}]},
         [{early_data, <<>>}], client),
    ?ERR({options, {early_data, enabled}},
         [{early_data, enabled}, {session_tickets, auto}], client),
    ?ERR({options, incompatible, [early_data, _, {use_ticket, _}]},
         [{early_data, <<>>}, {session_tickets, manual}], client),

    ?ERR({options, incompatible, [early_data, {session_tickets, _}]},
         [{early_data, enabled}], server),
    ?ERR({options, {early_data, <<>>}},
         [{early_data, <<>>}, {session_tickets, stateless}], server),
    ?ERR({options, {early_data, <<>>}},
         [{early_data, <<>>}, {session_tickets, stateless_with_cert}], server),

    ?ERR({options, incompatible, [stateless_tickets_seed, {session_tickets, stateful}]},
         [{stateless_tickets_seed, <<"foo">>}, {session_tickets, stateful}],
         server),
    ?ERR({options, incompatible, [stateless_tickets_seed, {session_tickets, stateful_with_cert}]},
         [{stateless_tickets_seed, <<"foo">>}, {session_tickets, stateful_with_cert}],
         server),
    ?ERR({stateless_tickets_seed, foo},
         [{stateless_tickets_seed, foo}, {session_tickets, stateless}],
         server),
    ?ERR({stateless_tickets_seed, foo},
         [{stateless_tickets_seed, foo}, {session_tickets, stateless_with_cert}],
         server),
    ?ERR({option, server_only, stateless_tickets_seed},
         [{stateless_tickets_seed, <<"foo">>}], client),
    ok.

options_eccs(_Config) ->
    Curves = tl(ssl:eccs()),
    ?OK(#{eccs := {elliptic_curves, [_|_]}}, [], client),
    ?OK(#{eccs := {elliptic_curves, [_|_]}}, [{eccs, Curves}], client),

    %% Errors
    ?ERR({eccs, not_a_list}, [{eccs, not_a_list}], client),
    ?ERR({eccs, none_valid}, [{eccs, []}], client),
    ?ERR({options,{eccs,[foo]}}, [{eccs, [foo]}], client),
    ok.

options_verify(Config) ->  %% fail_if_no_peer_cert, verify, verify_fun, partial_chain
    Cert = proplists:get_value(cert, ssl_test_lib:ssl_options(server_rsa_der_opts, Config)),
    {ok, #config{ssl = DefOpts = #{verify_fun := {DefVerify,_}}}} = ssl_config:handle_options([{verify, verify_none}], client, "dummy.host.org"),

    ?OK(#{fail_if_no_peer_cert := false, verify := verify_none, verify_fun := {DefVerify, []}, partial_chain := _},
        [], server),
    ?OK(#{fail_if_no_peer_cert := true, verify := verify_peer, verify_fun := undefined, partial_chain := _},
        [{fail_if_no_peer_cert, true}, {verify, verify_peer}, {cacerts, [Cert]}],
        server),
     ?OK(#{fail_if_no_peer_cert := true, verify := verify_peer, verify_fun := undefined, partial_chain := _},
         [{verify, verify_peer}, {cacerts, [Cert]}], server),

    %% Test ssl option handling. Option values are verified by public_key tests
    CertPolicyOpts = [{policy_set, [?anyPolicy]}, {explicit_policy, false}],

    ?OK(#{cert_policy_opts := CertPolicyOpts}, [{verify, verify_peer}, {cacerts, [Cert]}, {cert_policy_opts, CertPolicyOpts}],
        client),

    NewF3 = fun(_,_,_) -> ok end,
    NewF4 = fun(_,_,_,_) -> ok end,
    ?OK(#{}, [], client, [fail_if_no_peer_cert]),
    ?OK(#{verify := verify_none, verify_fun := {NewF3, foo}, partial_chain := _},
        [{verify_fun, {NewF3, foo}}], client),
    ?OK(#{fail_if_no_peer_cert := true, verify := verify_peer, verify_fun := {NewF3, foo}, partial_chain := _},
        [{verify_fun, {NewF3, foo}}, {verify, verify_peer}, {cacerts, [Cert]}],
        server),
    ?OK(#{fail_if_no_peer_cert := true, verify := verify_peer, verify_fun := {NewF4, foo}, partial_chain := _},
        [{verify_fun, {NewF4, foo}}, {verify, verify_peer}, {cacerts, [Cert]}],
        server),


    %% check verify_fun in update_options case
    #{verify_fun := undefined} = ssl_config:update_options([{verify, verify_peer}, {cacerts, [Cert]}], client, DefOpts),
    #{verify_fun := {NewF3, bar}} = ssl_config:update_options([{verify, verify_peer}, {cacerts, [Cert]},
                                                        {verify_fun, {NewF3, bar}}],
                                                       client, DefOpts),

    ?OK(#{allow_any_ca_purpose := true},
        [{allow_any_ca_purpose, true}, {verify, verify_peer}, {cacerts, [Cert]}],
        server),
    ?OK(#{allow_any_ca_purpose := false},
        [{verify, verify_peer}, {cacerts, [Cert]}],
        client),

    %% Errors
    ?ERR({partial_chain, undefined}, [{partial_chain, undefined}], client),
    ?ERR({options, incompatible, [{verify, verify_none}, {fail_if_no_peer_cert, true}]},
         [{fail_if_no_peer_cert, true}], server),
    ?ERR({options, incompatible, [{verify, _}, {cacerts, undefined}]}, [{verify, verify_peer}], client),
    ?ERR({option, server_only, fail_if_no_peer_cert},
         [{fail_if_no_peer_cert, true}, {verify, verify_peer}, {cacerts, [Cert]}],
         client),
    ?ERR({verify, verify}, [{verify, verify}], client),
    ?ERR({options, incompatible, [{verify, _}, {cacerts, undefined}]}, [{verify, verify_peer}], server),
    ?ERR({partial_chain, not_a_fun}, [{partial_chain, not_a_fun}], client),
    ?ERR({verify_fun, not_a_fun}, [{verify_fun, not_a_fun}], client),
    ?ERR({cert_policy_opts, {foo, bar}}, [{verify, verify_peer}, {cacerts, [Cert]}, {cert_policy_opts, [{foo,bar}]}],
        client),
    ?ERR({cert_policy_opts, {explicit_policy, bar}}, [{verify, verify_peer}, {cacerts, [Cert]}, {cert_policy_opts, [{explicit_policy,bar}]}],
         client),
    ?ERR({cert_policy_opts, {inhibit_policy_mapping, bar}}, [{verify, verify_peer}, {cacerts, [Cert]}, {cert_policy_opts, [{explicit_policy, true},
                                                                                                                           {inhibit_policy_mapping,bar}]}],
         client),
    ?ERR({cert_policy_opts, {inhibit_any_policy, bar}}, [{verify, verify_peer}, {cacerts, [Cert]}, {cert_policy_opts, [{inhibit_any_policy,bar}]}],
         client),
    ?ERR({middlebox_comp_mode, foo}, [{middlebox_comp_mode, foo}], server),
    ?ERR({allow_any_ca_purpose, foo},[{allow_any_ca_purpose, foo}, {verify, verify_peer}, {cacerts, [Cert]}], server),
    ok.

options_fallback(_Config) ->
    ?OK(#{fallback := false}, [], client),
    ?OK(#{fallback := true}, [{fallback, true}], client),

    %% Errors
    ?ERR({option, client_only, fallback},  [{fallback, true}], server),
    ?ERR({fallback, []},  [{fallback, []}], client),
    ?ERR({options, incompatible, [fallback, {versions, _}]},
         [{fallback, true}, {versions, ['tlsv1.3']}], client),
    ok.

options_handshake(_Config) -> %% handshake
    ?OK(#{handshake := full, max_handshake_size := 131072},
        [], client),
    ?OK(#{handshake := hello, max_handshake_size := 123800},
        [{handshake, hello}, {max_handshake_size, 123800}], client),


    %% Errors
    ?ERR({handshake, []},  [{handshake, []}], server),
    ?ERR({max_handshake_size, 8388608},  [{max_handshake_size, 8388608}], server),
    ?ERR({max_handshake_size, -1}, [{handshake, hello}, {max_handshake_size, -1}], client),
    ok.

options_process(_Config) -> % hibernate_after, spawn_opts
    ?OK(#{}, [], client, [hibernate_after, receiver_spawn_opts, sender_spawn_opts]),
    ?OK(#{hibernate_after := 10000,
          receiver_spawn_opts := [{fullsweep_after, 500}],
          sender_spawn_opts := [{fullsweep_after, 500}]},
        [{hibernate_after, 10000},
         {receiver_spawn_opts,[{fullsweep_after, 500}]},
         {sender_spawn_opts, [{fullsweep_after, 500}]}],
        client),
    %% Errors
    ?ERR({hibernate_after, -1}, [{hibernate_after, -1}], server),
    ?ERR({receiver_spawn_opts, not_a_list}, [{receiver_spawn_opts, not_a_list}], server),
    ?ERR({sender_spawn_opts, not_a_list}, [{sender_spawn_opts, not_a_list}], server),
    ok.

options_honor(_Config) ->  %% honor_cipher_order & honor_ecc_order
    ?OK(#{honor_cipher_order := false, honor_ecc_order := false},
        [], server),
    ?OK(#{honor_cipher_order := true, honor_ecc_order := true},
        [{honor_cipher_order, true}, {honor_ecc_order, true}],
        server),
    %% Errors
    ?ERR({option, server_only, honor_cipher_order},
         [{honor_cipher_order, true}], client),
    ?ERR({option, server_only, honor_ecc_order},
         [{honor_ecc_order, true}], client),
    ?ERR({honor_ecc_order, foo}, [{honor_ecc_order, foo}], server),
    ok.

options_debug(_Config) -> %% debug  log_level keep_secrets
    ?OK(#{log_level := notice}, [], server, [keep_secrets]),
    ?OK(#{log_level := debug, keep_secrets := true},
        [{log_level, debug}, {keep_secrets, true}], server),
    ?OK(#{log_level := info},
        [{log_level, info}, {keep_secrets, false}], server, [keep_secrets]),

    %% Errors
    ?ERR({log_level, foo}, [{log_level, foo}], server),
    ?ERR({keep_secrets, foo}, [{keep_secrets, foo}], server),
    ok.

options_renegotiate(_Config) -> %% key_update_at   renegotiate_at  secure_renegotiate
    ?OK(#{key_update_at := ?KEY_USAGE_LIMIT_AES_GCM, renegotiate_at := 268435456, secure_renegotiate := true},
        [], server),
    ?OK(#{key_update_at := 123456, renegotiate_at := 64000, secure_renegotiate := false},
        [{key_update_at, 123456}, {renegotiate_at, 64000}, {secure_renegotiate, false}],
        server),

    %% Errors
    ?ERR({options, incompatible, [key_update_at, {versions, _}]},
         [{key_update_at, 123456}, {versions, ['tlsv1.2']}], server),
    ?ERR({options, incompatible, [secure_renegotiate, {versions, _}]},
         [{secure_renegotiate, true}, {versions, ['tlsv1.3']}], server),

    ?ERR({key_update_at, -1}, [{key_update_at, -1}], server),
    ?ERR({renegotiate_at, not_a_int}, [{renegotiate_at, not_a_int}], server),
    ?ERR({renegotiate_at, -1}, [{renegotiate_at, -1}], server),
    ok.

options_middlebox(_Config) -> %% middlebox_comp_mode
    ?OK(#{}, [], client, [middlebox_comp_mode]),
    ?OK(#{}, [{middlebox_comp_mode, true}], client, [middlebox_comp_mode]),
    ?OK(#{middlebox_comp_mode := false}, [{middlebox_comp_mode, false}], client),

    %% Errors
    ?ERR({middlebox_comp_mode, foo}, [{middlebox_comp_mode, foo}], server),
    ?ERR({options, incompatible, [middlebox_comp_mode, {versions, _}]},
         [{middlebox_comp_mode, false}, {versions, ['tlsv1.2']}], server),
    ok.

options_frag_len(_Config) -> %% max_fragment_length
    ?OK(#{max_fragment_length := undefined}, [], client),
    ?OK(#{max_fragment_length := 2048}, [{max_fragment_length, 2048}], client),

    %% Errors
    ?ERR({option, client_only, max_fragment_length},
         [{max_fragment_length, 2048}], server),
    ?ERR({max_fragment_length,2000}, [{max_fragment_length, 2000}], client),
    ok.

options_stapling(_Config) ->
    ?OK(#{}, [], client, [stapling]),
    ?OK(#{}, [{stapling, no_staple}], client, [stapling]),

    ?OK(#{stapling := #{ocsp_nonce := false}},
        [{stapling, staple}], client),
    ?OK(#{stapling := #{ocsp_nonce := false}},
        [{stapling, #{}}], client),
    ?OK(#{stapling := #{ocsp_nonce := true}},
        [{stapling, #{ocsp_nonce => true}}], client),

    ?OK(#{stapling := #{ocsp_nonce := false}},
        [{stapling, #{ocsp_nonce => false}}], client),

    %% Errors
    ?ERR({stapling, foo}, [{stapling, 'foo'}], client),
    ?ERR({option, client_only, stapling}, [{stapling, true}], server),
    ?ERR({option, client_only, stapling}, [{stapling, false}], server),
    ?ERR({option, client_only, stapling}, [{stapling, #{}}], server),
    ok.

options_padding(_Config) ->
    ?OK(#{}, [], server, [padding_check]),
    ?OK(#{padding_check := false}, [{padding_check, false}, {versions, [tlsv1]}], server),
    %% Errors
    ?ERR({padding_check, foo}, [{padding_check, foo}, {versions, [tlsv1]}], server),
    ?ERR({options, incompatible, [padding_check, {versions, ['tlsv1.3','tlsv1.2']}]},
         [{padding_check, false}], server),
    ok.

options_identity(_Config) ->  %% psk_identity  srp_identity and user_lookup_fun
    ?OK(#{psk_identity := undefined, srp_identity := undefined, user_lookup_fun := undefined},
        [], client),
    ?OK(#{psk_identity := <<"foobar">>, srp_identity := undefined, user_lookup_fun := undefined},
        [{psk_identity, "foobar"}], server),
    ?OK(#{psk_identity := undefined, srp_identity := {<<"user">>, <<"pwd">>}, user_lookup_fun := undefined},
        [{srp_identity, {"user", "pwd"}}], client),
    ?OK(#{psk_identity := undefined, srp_identity := undefined, user_lookup_fun := {_, args}},
        [{user_lookup_fun, {fun(_,_,_) -> ok end, args}}], client),

    %% Should fail client option only
    ?OK(#{psk_identity := undefined, srp_identity := {<<"user">>, <<"pwd">>}, user_lookup_fun := undefined},
        [{srp_identity, {"user", "pwd"}}], server),

    %% Errors
    ?ERR({srp_identity, {_,_}},  %% FIXME doesn't like binary strings
         [{srp_identity, {<<"user">>, <<"pwd">>}}], client),
    ?ERR({psk_identity, _},  %% FIXME doesn't like binary strings
         [{psk_identity, <<"user">>}], client),
    ?ERR({srp_identity, _}, [{srp_identity, "user"}], client),
    ?ERR({user_lookup_fun, _},
         [{user_lookup_fun, {fun(_,_) -> ok end, args}}],
         client),

    ?ERR({options, incompatible, [psk_identity, _]},
         [{psk_identity, "foobar"},{versions, ['tlsv1.3']}],
         server),
    ?ERR({options, incompatible, [srp_identity, _]},
         [{srp_identity, {"user", "pwd"}},{versions, ['tlsv1.3']}],
         client),
    ?ERR({options, incompatible, [user_lookup_fun, _]},
         [{user_lookup_fun, {fun(_,_,_) -> ok end, args}}, {versions, ['tlsv1.3']}],
         client),
    ok.

options_reuse_session(_Config) ->
    ?OK(#{reuse_session := undefined, reuse_sessions := true}, [], client),
    ?OK(#{reuse_session := _, reuse_sessions := true}, [], server),

    ?OK(#{reuse_session := <<>>, reuse_sessions := save},
        [{reuse_session, <<>>}, {reuse_sessions, save}], client),
    ?OK(#{reuse_session := {<<>>, <<>>}, reuse_sessions := false},
        [{reuse_session, {<<>>, <<>>}}, {reuse_sessions, false}], client),

    RS_F = fun(_,_,_,_) -> ok end,
    ?OK(#{reuse_session := RS_F, reuse_sessions := false},
        [{reuse_session, RS_F}, {reuse_sessions, false}],
        server),

    %% Errors
    ?ERR({options, incompatible, [reuse_session, _]},
         [{reuse_session, RS_F}, {versions, ['tlsv1.3']}],
         server),
    ?ERR({options, incompatible, [reuse_sessions, _]},
         [{reuse_sessions, true}, {versions, ['tlsv1.3']}],
         server),
    ?ERR({reuse_session, RS_F},
         [{reuse_session, RS_F},{reuse_sessions, false}],
         client),
    ?ERR({reuse_sessions, foo},  [{reuse_sessions, foo}], server),
    ?ERR({reuse_session, foo},  [{reuse_session, foo}], server),
    ?ERR({reuse_sessions, save}, [{reuse_sessions, save}], server),
    ?ERR({reuse_session, <<>>}, [{reuse_session, <<>>}], server),

    ok.

options_sni(_Config) -> %% server_name_indication
    ?OK(#{server_name_indication := "dummy.host.org"}, [], client),
    ?OK(#{}, [], server, [server_name_indication]),
    ?OK(#{server_name_indication := disable}, [{server_name_indication, disable}], client),
    ?OK(#{server_name_indication := "dummy.org"}, [{server_name_indication, "dummy.org"}], client),

    ?OK(#{sni_fun := _}, [], server, [sni_hosts]),

    ?OK(#{sni_fun := _}, [{sni_hosts, [{"a", []}]}], server, [sni_hosts]),
    SNI_F = fun(_) -> sni end,
    ?OK(#{sni_fun := SNI_F}, [{sni_fun, SNI_F}], server, [sni_hosts]),

    %% Errors
    ?ERR({option, client_only, server_name_indication},
         [{server_name_indication, "dummy.org"}], server),
    ?ERR({option, server_only, sni_hosts},  [{sni_hosts, [{"a", []}]}], client),
    ?ERR({option, server_only, sni_fun},  [{sni_fun, SNI_F}], client),

    ?ERR({server_name_indication, foo},  [{server_name_indication, foo}], client),
    ?ERR({sni_hosts, foo}, [{sni_hosts, foo}], server),
    ?ERR({sni_fun, foo}, [{sni_fun, foo}], server),

    ?ERR({options, incompatible, [sni_fun, sni_hosts]},
         [{sni_fun, SNI_F}, {sni_hosts, [{"a", []}]}], server),
    ok.

options_sign_alg(_Config) ->  %% signature_algs[_cert]
    ?OK(#{signature_algs := [_|_], signature_algs_cert := [_|_]},
        [], client),
    ?OK(#{signature_algs := [rsa_pss_rsae_sha512,{sha512,rsa}], signature_algs_cert := undefined},
        [{signature_algs, [rsa_pss_rsae_sha512,{sha512,rsa}]}], client),
    ?OK(#{signature_algs := [_|_], signature_algs_cert := [eddsa_ed25519, rsa_pss_rsae_sha512]},
        [{signature_algs_cert, [eddsa_ed25519, rsa_pss_rsae_sha512]}],
        client),

    %% Errors
    ?ERR({signature_algs, not_a_list}, [{signature_algs, not_a_list}], client),
    ?ERR({signature_algs_cert, not_a_list}, [{signature_algs_cert, not_a_list}], client),
    ?ERR({signature_algs, [foobar]}, [{signature_algs, [foobar]}], client),
    ?ERR({signature_algs_cert, [foobar]}, [{signature_algs_cert, [foobar]}], client),
    ?ERR({options, incompatible, [signature_algs_cert, {versions, _}]},
         [{signature_algs_cert, [eddsa_ed25519, rsa_pss_rsae_sha512]}, {versions, ['tlsv1.1']}],
         client),
    ?ERR({options, incompatible, [signature_algs_cert, {versions, _}]},
         [{signature_algs_cert, [eddsa_ed25519, rsa_pss_rsae_sha512]}, {versions, ['tlsv1.1']}],
         server),
    ?ERR({options, {no_supported_algorithms, {signature_algs,[]}}},
         [{signature_algs, []}], client),
    ?ERR({options, {no_supported_signature_schemes, {signature_algs_cert,[]}}},
         [{signature_algs_cert, []}],
         client),
    ok.

options_supported_groups(_Config) ->
    Default = ssl:groups(default),
    ?OK(#{supported_groups := {supported_groups, Default}},
        [], client),
    ?OK(#{supported_groups := {supported_groups, [secp521r1, ffdhe2048]}},
        [{supported_groups, [secp521r1, ffdhe2048]}], client),

    %% ERRORs
    ?ERR({{'tlvs1.2'},{versions,[{'tlvs1.2'}]}},
         [{supported_groups, []}, {versions, [{'tlvs1.2'}]}], client),
    ?ERR({supported_groups, not_a_list}, [{supported_groups, not_a_list}], client),
    ?ERR({supported_groups, none_valid},  [{supported_groups, []}], client),
    ?ERR({supported_groups, none_valid},  [{supported_groups, [foo]}], client),
    ok.

options_use_srtp(_Config) ->
    ?OK(#{use_srtp := #{protection_profiles := [<<0,2>>, <<0,5>>], mki := <<>>}},
        [{protocol, dtls},
         {use_srtp, #{protection_profiles => [<<0,2>>, <<0,5>>]}}], client),
    ?OK(#{use_srtp := #{protection_profiles := [<<0,2>>], mki := <<>>}},
        [{protocol, dtls},
         {use_srtp, #{protection_profiles => [<<0,2>>]}}], server),
    ?OK(#{use_srtp := #{protection_profiles := [<<0,2>>, <<0,5>>], mki := <<"123">>}},
        [{protocol, dtls},
         {use_srtp, #{protection_profiles => [<<0,2>>, <<0,5>>], mki => <<"123">>}}], client),
    ?OK(#{use_srtp := #{protection_profiles := [<<0,2>>], mki := <<"123">>}},
        [{protocol, dtls},
         {use_srtp, #{protection_profiles => [<<0,2>>], mki => <<"123">>}}], server),

    %% invalid parameters
    ?ERR({options, {use_srtp, {no_protection_profiles, _}}},
         [{protocol, dtls},
          {use_srtp, #{}}], client),
    ?ERR({options, {use_srtp, {no_protection_profiles, _}}},
         [{protocol, dtls},
          {use_srtp, #{protection_profiles => []}}], client),
    ?ERR({options, {use_srtp, {invalid_protection_profiles, _}}},
         [{protocol, dtls},
          {use_srtp, #{protection_profiles => [<<"bad">>]}}], client),
    ?ERR({options, {use_srtp, {unknown_parameters,[whatever]}}},
         [{protocol, dtls},
          {use_srtp, #{protection_profiles => [<<0,2>>], whatever => xxx}}], client),

    %% use_srtp is DTLS-only extension, by default setting its options should raise an error
    ?ERR({options, incompatible, _},
         [{use_srtp, #{protection_profiles => [<<0,2>>, <<0,5>>]}}], client),
    ?ERR({options, incompatible, _},
         [{use_srtp, #{protection_profiles => [<<0,2>>]}}], server),
    ok.

%%-------------------------------------------------------------------

default_reject_anonymous()->
    [{doc,"Test that by default anonymous cipher suites are rejected "}].
default_reject_anonymous(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Version = ssl_test_lib:protocol_version(Config),
    TLSVersion = ssl_test_lib:tls_version(Version),
    
   [CipherSuite | _] = ssl_test_lib:ecdh_dh_anonymous_suites(TLSVersion),
    
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                              {host, Hostname},
                                              {from, self()},
                                              {options,
                                               [{ciphers,[CipherSuite]} |
                                                ClientOpts]}]),

    ssl_test_lib:check_server_alert(Server, Client, insufficient_security).

%%-------------------------------------------------------------------
cb_info() ->
    [{doc,"Test that we can set cb_info."}].

cb_info(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Protocol = proplists:get_value(protocol, Config, tls),
    CbInfo =
        case Protocol of
            tls ->
                {cb_info, {gen_tcp, tcp, tcp_closed, tcp_error}};
            dtls ->
                {cb_info, {gen_udp, udp, udp_closed, udp_error}}
        end,
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options,  [CbInfo | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [CbInfo | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok).

%%-------------------------------------------------------------------
log_alert() ->
    [{doc,"Test that we can set log_alert and that it translates to correct log_level" 
      " that has replaced this option"}].

log_alert(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options,  [{log_alert, true} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, CSock} = ssl_test_lib:start_client([return_socket,
                                                 {node, ClientNode}, {port, Port},
                                                 {host, Hostname},
                                                 {from, self()},
                                                 {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                                 {options, [{log_alert, false} | ClientOpts]}]),
    
    {ok, [{log_level, none}]} = ssl:connection_information(CSock, [log_level]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok).


%%-------------------------------------------------------------------
%% Note that these test only test that the options are valid to set. As application data
%% is a stream you can not test that the send acctually splits it up as when it arrives
%% again at the user layer it may be concatenated. But COVER can show that the split up
%% code has been run.
   
rizzo_disabled() ->
     [{doc, "Test original beast mitigation disable option for SSL 3.0 and TLS 1.0"}].

rizzo_disabled(Config) ->
    ClientOpts = [{beast_mitigation, disabled} | ssl_test_lib:ssl_options(client_rsa_opts, Config)],
    ServerOpts =  [{beast_mitigation, disabled} | ssl_test_lib:ssl_options(server_rsa_opts, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%-------------------------------------------------------------------
rizzo_zero_n() ->
     [{doc, "Test zero_n beast mitigation option (same affect as original disable option) for SSL 3.0 and TLS 1.0"}].

rizzo_zero_n(Config) ->
    ClientOpts = [{beast_mitigation, zero_n} | ssl_test_lib:ssl_options(client_rsa_opts, Config)],
    ServerOpts =  [{beast_mitigation, zero_n} | ssl_test_lib:ssl_options(server_rsa_opts, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%-------------------------------------------------------------------
rizzo_one_n_minus_one () ->
     [{doc, "Test beast_mitigation option one_n_minus_one (same affect as default) for SSL 3.0 and TLS 1.0"}].

rizzo_one_n_minus_one (Config) ->
    ClientOpts = [{beast_mitigation, one_n_minus_one } | ssl_test_lib:ssl_options(client_rsa_opts, Config)],
    ServerOpts =  [{beast_mitigation, one_n_minus_one} | ssl_test_lib:ssl_options(server_rsa_opts, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%-------------------------------------------------------------------

supported_groups() ->
    [{doc,"Test the supported_groups option in TLS 1.3."}].

supported_groups(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{supported_groups, [x448, x25519]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{supported_groups,[x448]} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
honor_client_cipher_order_tls13() ->
    [{doc,"Test API honor server cipher order in TLS 1.3."}].
honor_client_cipher_order_tls13(Config) when is_list(Config) ->
    ClientCiphers = [#{key_exchange => any,
                       cipher => aes_256_gcm,
                       mac => aead,
                       prf => sha384},
                     #{key_exchange => any,
                       cipher => aes_128_gcm,
                       mac => aead,
                       prf => sha256}],
    ServerCiphers = [#{key_exchange => any,
                       cipher => aes_128_gcm,
                       mac => aead,
                       prf => sha256},
                     #{key_exchange => any,
                       cipher => aes_256_gcm,
                       mac => aead,
                       prf => sha384}],
    honor_cipher_order(Config, false, ServerCiphers, ClientCiphers, #{key_exchange => any,
                                                                      cipher => aes_256_gcm,
                                                                      mac => aead,
                                                                      prf => sha384}).

%%--------------------------------------------------------------------
client_options_negative_version_gap() ->
    [{doc,"Test client options with faulty version gap."}].
client_options_negative_version_gap(Config) when is_list(Config) ->
    start_client_negative(Config, [{versions, ['tlsv1', 'tlsv1.3']}],
                          {options, missing_version, 
                           {'tlsv1.2', {versions,[tlsv1, 'tlsv1.3']}}}).

%%--------------------------------------------------------------------
client_options_negative_dependency_version() ->
    [{doc,"Test client options with faulty version dependency."}].
client_options_negative_dependency_version(Config) when is_list(Config) ->
    start_client_negative(Config, [{versions, ['tlsv1.1', 'tlsv1.2']},
                                   {session_tickets, manual}],
                          {options,incompatible,
                           [session_tickets,{versions,['tlsv1.2', 'tlsv1.1']}]}).

%%--------------------------------------------------------------------
client_options_negative_dependency_stateless() ->
    [{doc,"Test client options with faulty 'session_tickets' option."}].
client_options_negative_dependency_stateless(Config) when is_list(Config) ->
    start_client_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {anti_replay, '10k'},
                                   {session_tickets, manual}],
                          {option, server_only, anti_replay}).


%%--------------------------------------------------------------------
client_options_negative_dependency_role() ->
    [{doc,"Test client options with faulty role."}].
client_options_negative_dependency_role(Config) when is_list(Config) ->
    start_client_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, stateless}],
                          {options,{session_tickets,stateless}}).

%%--------------------------------------------------------------------
client_options_negative_early_data() ->
    [{doc,"Test client option early_data."}].
client_options_negative_early_data(Config) when is_list(Config) ->
    start_client_negative(Config, [{versions, ['tlsv1.2']},
                                   {session_tickets, manual},
                                   {early_data, <<"test">>}],
                          {options,incompatible,
                           [session_tickets,{versions,['tlsv1.2']}]}),
    start_client_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {early_data, <<"test">>}],
                          {options,incompatible,
                           [early_data,{session_tickets,disabled}]}),

    start_client_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, stateful},
                                   {early_data, <<"test">>}],
                          {options, {session_tickets, stateful}}),

    start_client_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, manual},
                                   {early_data, <<"test">>}],
                          {options,incompatible,
                           [early_data, {session_tickets, manual}, {use_ticket, undefined}]}),
    start_client_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, manual},
                                   {use_ticket, [<<"ticket">>]},
                                   {early_data, "test"}],
                          {options, {early_data, "test"}}),
    %% All options are ok but there is no server
    start_client_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, manual},
                                   {use_ticket, [<<"ticket">>]},
                                   {early_data, <<"test">>}],
                          econnrefused),

    %% All options are ok but there is no server
    start_client_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, auto},
                                   {early_data, <<"test">>}],
                          econnrefused).

%%--------------------------------------------------------------------
server_options_negative_early_data() ->
    [{doc,"Test server option early_data."}].
server_options_negative_early_data(Config) when is_list(Config) ->
    start_server_negative(Config, [{versions, ['tlsv1.2']},
                                   {early_data, enabled},
                                   {session_tickets, stateful}
                                  ],
                          {options,incompatible,
                           [session_tickets,{versions,['tlsv1.2']}]}),
    start_server_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {early_data, enabled}],
                          {options,incompatible,
                           [early_data,{session_tickets,disabled}]}),

    start_server_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, manual},
                                   {early_data, enabled}],
                          {options, {session_tickets, manual}}),

    start_server_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, stateful},
                                   {early_data, "test"}],
                          {options, {early_data, "test"}}).

%%--------------------------------------------------------------------
server_options_negative_version_gap() ->
    [{doc,"Test server options with faulty version gap."}].
server_options_negative_version_gap(Config) when is_list(Config) ->
    start_server_negative(Config, [{versions, ['tlsv1', 'tlsv1.3']}],
                          {options, missing_version,
                           {'tlsv1.2', {versions,[tlsv1, 'tlsv1.3']}}}).

%%--------------------------------------------------------------------
server_options_negative_dependency_role() ->
    [{doc,"Test server options with faulty role."}].
server_options_negative_dependency_role(Config) when is_list(Config) ->
    start_server_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, manual}],
                          {options,{session_tickets,manual}}).

%%--------------------------------------------------------------------
server_options_negative_stateless_tickets_seed() ->
    [{doc, "Test server option stateless_tickets_seed"}].
server_options_negative_stateless_tickets_seed(Config) ->
    Seed = crypto:strong_rand_bytes(32),

    start_server_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {stateless_tickets_seed, Seed}],
                          {options, incompatible,
                           [stateless_tickets_seed, {session_tickets, disabled}]}),

    start_server_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, disabled},
                                   {stateless_tickets_seed, Seed}],
                          {options, incompatible,
                           [stateless_tickets_seed, {session_tickets, disabled}]}),

    start_server_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, stateful},
                                   {stateless_tickets_seed, Seed}],
                          {options, incompatible,
                           [stateless_tickets_seed, {session_tickets, stateful}]}),

    InvalidSeed1 = 12345,
    start_server_negative(Config, [{versions, ['tlsv1.2', 'tlsv1.3']},
                                   {session_tickets, stateless},
                                   {stateless_tickets_seed, InvalidSeed1}],
                          {options, {stateless_tickets_seed, InvalidSeed1}}).

%%--------------------------------------------------------------------
honor_server_cipher_order_tls13() ->
    [{doc,"Test API honor server cipher order in TLS 1.3."}].
honor_server_cipher_order_tls13(Config) when is_list(Config) ->
    ClientCiphers = [#{key_exchange => any,
                       cipher => aes_256_gcm,
                       mac => aead,
                       prf => sha384},
                     #{key_exchange => any,
                       cipher => aes_128_gcm,
                       mac => aead,
                       prf => sha256}],
    ServerCiphers = [#{key_exchange => any,
                       cipher => aes_128_gcm,
                       mac => aead,
                       prf => sha256},
                     #{key_exchange => any,
                       cipher => aes_256_gcm,
                       mac => aead,
                       prf => sha384}],
    honor_cipher_order(Config, true, ServerCiphers,
                       ClientCiphers, #{key_exchange => any,
                                        cipher => aes_128_gcm,
                                        mac => aead,
                                        prf => sha256}).
%%--------------------------------------------------------------------
getstat() ->
    [{doc, "Test that you use ssl:getstat on a TLS socket"}].

getstat(Config) when is_list(Config) ->    
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Port0 = ssl_test_lib:inet_port(ServerNode),
    {ok, ListenSocket} = ssl:listen(Port0, [ServerOpts]),
    {ok, _} = ssl:getstat(ListenSocket),
    ssl:close(ListenSocket),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
                                        {mfa, {?MODULE, ssl_getstat, []}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
                                        {from, self()},
                                        {mfa, {?MODULE, ssl_getstat, []}},
                                        {options, ClientOpts}]),
    ssl_test_lib:check_result(Server, ok, Client, ok).

invalid_options_tls13() ->
    [{doc, "Test invalid options with TLS 1.3"}].
invalid_options_tls13(Config) when is_list(Config) ->
    TestOpts =
        [{{beast_mitigation, one_n_minus_one},
          {options, incompatible,
           [beast_mitigation,{versions,['tlsv1.3']}]},
          common},

         {{next_protocols_advertised, [<<"http/1.1">>]},
          {options, incompatible,
           [next_protocols_advertised, {versions,['tlsv1.3']}]},
          server},

         {{client_preferred_next_protocols,
           {client, [<<"http/1.1">>]}},
          {options, incompatible,
           [client_preferred_next_protocols, {versions,['tlsv1.3']}]},
          client},

         {{client_renegotiation, false},
          {options, incompatible,
           [client_renegotiation, {versions,['tlsv1.3']}]},
          server
         },

         {{cookie, false},
          {option , server_only, cookie},
          client
         },

         {{padding_check, false},
          {options, incompatible, [padding_check,{versions,['tlsv1.3']}]},
          common},

         {{psk_identity, "Test-User"},
          {options, incompatible, [psk_identity,{versions,['tlsv1.3']}]},
          common},

         {{user_lookup_fun,
           {fun ssl_test_lib:user_lookup/3, <<1,2,3>>}},
          {options, incompatible, [user_lookup_fun,{versions,['tlsv1.3']}]},
          common},

         {{reuse_session, fun(_,_,_,_) -> false end},
          {options, incompatible, [reuse_session, {versions,['tlsv1.3']}]},
          server},

         {{reuse_session, <<1,2,3,4>>},
          {options, incompatible, [reuse_session, {versions,['tlsv1.3']}]},
          client},

         {{reuse_sessions, true},
          {options, incompatible, [reuse_sessions, {versions,['tlsv1.3']}]},
          common},

         {{secure_renegotiate, false},
          {options, incompatible, [secure_renegotiate, {versions,['tlsv1.3']}]},
          common},

         {{srp_identity, {"user", "passwd"}},
          {options, incompatible, [srp_identity, {versions,['tlsv1.3']}]},
          client}
        ],

    Fun = fun(Option, ErrorMsg, Type) ->
                  case Type of
                      server ->
                          start_server_negative(Config,
                                                [Option,{versions, ['tlsv1.3']}],
                                                ErrorMsg);
                      client ->
                          start_client_negative(Config,
                                                [Option,{versions, ['tlsv1.3']}],
                                                ErrorMsg);
                      common ->
                          start_server_negative(Config,
                                                [Option,{versions, ['tlsv1.3']}],
                                                ErrorMsg),
                          start_client_negative(Config,
                                                [Option,{versions, ['tlsv1.3']}],
                                                ErrorMsg)
                  end
          end,
    [Fun(Option, ErrorMsg, Type) || {Option, ErrorMsg, Type} <- TestOpts].


ssl_not_started() ->
    [{doc, "Test that an error is returned if ssl is not started"}].
ssl_not_started(Config) when is_list(Config) ->
    application:stop(ssl),
    R1 = try
             {error, ssl_not_started} = ssl:connect("localhost", 22, [{verify, verify_none},
                                                                      {protocol, tls}]),
             ok
         catch _:Reason ->
                 Reason
         end,
    R2 = try
             {error, ssl_not_started} = ssl:connect("localhost", 22, [{verify, verify_none},
                                                                      {protocol, dtls}]),
             ok
         catch _:Reason2 ->
                 Reason2
         end,
    ssl:start(),
    ok = R1 = R2.

cookie() ->
    [{doc, "Test cookie extension in TLS 1.3"}].
cookie(Config) when is_list(Config) ->
    cookie_extension(Config, true),
    cookie_extension(Config, false).

%%--------------------------------------------------------------------
check_random_nonce() ->
    [{doc,"Test random nonce - expecting 32B random for TLS1.3 and 4B UTC "
      "epoch with 28B random for older version"}].
check_random_nonce(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    N = 10,
    F = fun (Id) -> establish_connection(Id,ServerNode, ServerOpts,
                                         ClientNode, ClientOpts,
                                         Hostname)
        end,
    ConnectionPairs = [F(Id) || Id <- lists:seq(1, N)],
    Randoms = lists:flatten([ssl_test_lib:get_result([Server, Client]) ||
                                {Server, Client} <- ConnectionPairs]),
    Deltas = [abs(FourBytes - SecsSince) ||
                 {_FromPid,
                  {_Id, {_, <<FourBytes:32, _/binary>>}, SecsSince}} <- Randoms],
    MeanDelta = lists:sum(Deltas) div N,
    case ?config(version, Config) of
        Vsn when Vsn =:= 'tlsv1.3'; Vsn =:= undefined ->
            %% 4B "random" expected since TLS1.3
            RndThreshold   = 10000,
            true = MeanDelta > RndThreshold;
        _Vsn ->
            ?CT_LOG("Using Version: ~p", [_Vsn]),
            %% 4 epoch based bytes expected pre TLS1.3
            EpochThreshold = 10,
            true = MeanDelta < EpochThreshold
    end,
    [begin
         ssl_test_lib:close(Server),
         ssl_test_lib:close(Client)
     end || {Server, Client} <- ConnectionPairs].
%%--------------------------------------------------------------------
cipher_listing() ->
    [{doc, "Check that exclusive cipher for possible supported version adds up to all cipher " 
      "for the max version. Note that TLS-1.3 will contain two distinct sets of ciphers "
      "one for TLS-1.3 and one pre TLS-1.3"}].
cipher_listing(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    length_exclusive(Version) == length_all(Version).

%%--------------------------------------------------------------------
format_error() ->
    "".
format_error(Config) when is_list(Config) ->
    Errors = [{error, enotconn},
              {error, closed},
              {options, {keyfile, "TestFileName", {error,enoent}}},
              {options, {certfile, "TestFileName", {error,enoent}}},
              {options, {cacertfile, "TestFileName",{error,enoent}}},
              {options, {option_not_a_key_value_tuple, [foo]}},
              {options, {insufficient_crypto_support,{'tlsv1.3','tlsv1.1'}}},
              {options, incompatible, [{verify,verify_peer},{cacerts,undefined}]},
              {options, {protocol, foo}},
              {option, server_only, alpn_preferred_protocols},
              {options, {alpn_advertised_protocols, undefined}},
              {options, missing_version, {'tlsv1.2',{versions, ['tlsv1.1','tlsv1.3']}}},
              {options, {no_supported_algorithms, {signature_algs,[]}}},
              {options, {no_supported_signature_schemes, {signature_algs_cert,[]}}}
             ],
    Check = fun(Err) ->
                    Str = ssl:format_error(Err),
                    io:format("~p => ~s ~n", [Err, Str]),
                    %% Verify flat string (why do ssl flatten strings?)
                    Str = [C || C <- Str, is_integer(C)]
            end,
    [Check(Err) || Err <- Errors],
    ok.

%%--------------------------------------------------------------------

export_key_materials() ->
    [{doc, "Test export_key_materials/4,5"}].

export_key_materials(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = ssl_test_lib:protocol_version(Config, atom),
    BaseOpts = [{active, true}, {versions, [Version]}, {protocol, tls_or_dtls(Version)}],
    ServerOpts = BaseOpts ++ ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = BaseOpts ++ ssl_test_lib:ssl_options(client_rsa_opts, Config),

    Label = <<"EXPERIMENTAL-otp">>,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, {from, self()},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {_, ClientS} = ssl_test_lib:start_client([return_socket, {node, ClientNode}, {port, Port},
                                             {host, Hostname}, {from, self()},
                                             {options, ClientOpts}]),
    Server ! get_socket,
    ServerS = receive
                  {Server, {socket, S}} -> S
              end,

    case Version of
        'tlsv1.3' ->
            {ok, [ExportKeyMaterial]} = ssl:export_key_materials(ServerS, [Label], [<<>>], [32], false),
            {ok, [ExportKeyMaterial]} = ssl:export_key_materials(ClientS, [Label], [<<>>], [32], false),
            {ok, [ExportKeyMaterial]} = ssl:export_key_materials(ServerS, [Label], [no_context], [32]),
            {ok, [ExportKeyMaterial]} = ssl:export_key_materials(ClientS, [Label], [no_context], [32]);
        _ ->
            {ok, ExportKeyMaterial1} = ssl:export_key_materials(ServerS, [Label], [<<>>], [20]),
            {ok, ExportKeyMaterial1} = ssl:export_key_materials(ClientS, [Label], [<<>>], [20]),
            {ok, ExportKeyMaterial2} = ssl:export_key_materials(ServerS, [Label], [no_context], [20]),
            {ok, ExportKeyMaterial2} = ssl:export_key_materials(ClientS, [Label], [no_context], [20]),
            true = ExportKeyMaterial1 =/= ExportKeyMaterial2
    end.
%%--------------------------------------------------------------------
exporter_master_secret_consumed() ->
    [{doc, "Test export_key_materials/4,5 TLS-1.3 functionality"}].

exporter_master_secret_consumed(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = ssl_test_lib:protocol_version(Config, atom),
    BaseOpts = [{active, true}, {versions, [Version]}, {protocol, tls_or_dtls(Version)}],
    ServerOpts = BaseOpts ++ ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = BaseOpts ++ ssl_test_lib:ssl_options(client_rsa_opts, Config),

    Label1 = <<"EXPERIMENTAL-otp1">>,
    Label2 = <<"EXPERIMENTAL-otp2">>,
    Label3 = <<"EXPERIMENTAL-otp3">>,
    Label4 = <<"EXPERIMENTAL-otp4">>,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, {from, self()},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {_, ClientS} = ssl_test_lib:start_client([return_socket, {node, ClientNode}, {port, Port},
                                             {host, Hostname}, {from, self()},
                                             {options, ClientOpts}]),
    Server ! get_socket,
    ServerS = receive
                  {Server, {socket, S}} -> S
              end,

    {ok, ExportKeyMaterials} = ssl:export_key_materials(ServerS, [Label1, Label2], [<<>>, <<>>], [32, 32]),
    {error, exporter_master_secret_already_consumed} = ssl:export_key_materials(ServerS, [Label3], [<<>>], [32]),
    {ok, ExportKeyMaterials} = ssl:export_key_materials(ClientS, [Label1, Label2], [<<>>, <<>>], [32, 32], false),
    {error, bad_input} = ssl:export_key_materials(ClientS, [Label3, Label4], [<<>>], [32]).

%%--------------------------------------------------------------------

legacy_prf() ->
    [{doc, "Test prf/5"}].

legacy_prf(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = ssl_test_lib:protocol_version(Config, atom),
    BaseOpts = [{active, true}, {versions, [Version]}, {protocol, tls_or_dtls(Version)}],
    ServerOpts = BaseOpts ++ ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = BaseOpts ++ ssl_test_lib:ssl_options(client_rsa_opts, Config),

   {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = ssl_test_lib:protocol_version(Config, atom),
    BaseOpts = [{active, true}, {versions, [Version]}, {protocol, tls_or_dtls(Version)}],
    ServerOpts = BaseOpts ++ ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = BaseOpts ++ ssl_test_lib:ssl_options(client_rsa_opts, Config),

    Label = <<"EXPERIMENTAL-otp">>,

    Server = ssl_test_lib:start_server(
                    [return_socket,
                     {node, ServerNode}, {port, 0}, {from, self()},
                     {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {_, ClientS} = ssl_test_lib:start_client([return_socket, {node, ClientNode}, {port, Port},
                                             {host, Hostname}, {from, self()},
                                             {options, ClientOpts}]),

    Server ! get_socket,
    ServerS = receive
                  {Server, {socket, S}} -> S
              end,

    case Version of
        'tlsv1.3' ->
            {ok, ExportKeyMaterials} = ssl:prf(ServerS, master_secret, Label, [client_random, server_random, <<>>], 32),
            {ok, ExportKeyMaterials} = ssl:prf(ClientS, master_secret, Label, [client_random, server_random, <<>>], 32);
        _ ->
            {ok, ExportKeyMaterials1} = ssl:prf(ServerS, master_secret, Label, [client_random, server_random], 32),
            {ok, ExportKeyMaterials1} = ssl:prf(ServerS, master_secret, Label, [client_random, server_random], 32),
            {ok, ExportKeyMaterials2} = ssl:prf(ServerS, master_secret, Label, [client_random, server_random, <<>>], 32),
            {ok, ExportKeyMaterials2} = ssl:prf(ClientS, master_secret, Label, [client_random, server_random, <<>>], 32),
            true = ExportKeyMaterials1 =/= ExportKeyMaterials2
    end.

%%--------------------------------------------------------------------

establish_connection(Id, ServerNode, ServerOpts, ClientNode, ClientOpts, Hostname) ->
    Server =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {?MODULE, get_connection_information,
                                          [Id, server_random]}},
                                   {options, [{verify, verify_peer} | ServerOpts]}]),

    ListenPort = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client([{node, ClientNode}, {port, ListenPort},
                                   {host, Hostname},
                                   {from, self()},
                                   {mfa, {?MODULE, get_connection_information,
                                          [Id, client_random]}},
                                   {options,  [{verify, verify_peer} |ClientOpts]}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),
    {Server, Client}.

%%% Checker functions
get_connection_information(Socket, ConnectionId, InfoType) ->
    {ok, [ConnectionInfo]} = ssl:connection_information(Socket, [InfoType]),
    {ConnectionId, ConnectionInfo, secs_since_1970()}.
secs_since_1970() ->
    calendar:datetime_to_gregorian_seconds(
        calendar:universal_time()) - 62167219200.

connection_information_result(Socket) ->
    {ok, Info = [_ | _]} = ssl:connection_information(Socket),
    case  length(Info) > 3 of
	true -> 
	    %% At least one ssl_option() is set
	    ?CT_LOG("Info ~p", [Info]),
	    ok;
	false ->
	    ct:fail(no_ssl_options_returned)
    end.

secret_connection_info_result(Socket) ->
    {ok, [{protocol, Protocol}]} = ssl:connection_information(Socket, [protocol]),
    {ok, ConnInfo} = ssl:connection_information(Socket, [client_random, server_random, master_secret]),
    check_connection_info(Protocol, ConnInfo).

keylog_connection_info_result(Socket, KeepSecrets) ->
    {ok, [{protocol, Protocol}]} = ssl:connection_information(Socket, [protocol]),
    {ok, ConnInfo} = ssl:connection_information(Socket, [keylog]),
    check_keylog_info(Protocol, ConnInfo, KeepSecrets).

check_keylog_info('tlsv1.3', [{keylog, ["CLIENT_HANDSHAKE_TRAFFIC_SECRET"++_,_|_]=Keylog}], true) ->
    {ok, Keylog};
check_keylog_info('tlsv1.3', []=Keylog, false) ->
    {ok, Keylog};
check_keylog_info(_, [{keylog, ["CLIENT_RANDOM"++_]=Keylog}], _) ->
    {ok, Keylog};
check_keylog_info(_, Unexpected, Keep) ->
    {unexpected, Keep, Unexpected}.

check_srp_in_connection_information(_Socket, _Username, client) ->
    ok;
check_srp_in_connection_information(Socket, Username, server) ->
    {ok, Info} = ssl:connection_information(Socket),
    ?CT_LOG("Info ~p~n", [Info]),
    case proplists:get_value(srp_username, Info, not_found) of
        Username ->
	        ok;
        not_found ->
            ct:fail(srp_username_not_found)
    end.

%% In TLS 1.3 the master_secret field is used to store multiple secrets from the key schedule and it is a tuple.
%% client_random and server_random are not used in the TLS 1.3 key schedule.
check_connection_info('tlsv1.3', [{client_random, ClientRand},{server_random, ServerRand},
                                  {master_secret, {master_secret, MasterSecret}}]) ->
    is_binary(ClientRand) andalso is_binary(ServerRand) andalso is_binary(MasterSecret);
check_connection_info(_, [{client_random, ClientRand}, {server_random, ServerRand}, {master_secret, MasterSecret}]) ->
    is_binary(ClientRand) andalso is_binary(ServerRand) andalso is_binary(MasterSecret);
check_connection_info(_, _) ->
    false.

try_recv_active(Socket) ->
    ssl:send(Socket, "Hello world"),
    {error, einval} = ssl:recv(Socket, 11),
    ok.
try_recv_active_once(Socket) ->
    {error, einval} = ssl:recv(Socket, 11),
    ok.

controlling_process_result(Socket, Pid, Msg) ->
    ok = ssl:controlling_process(Socket, Pid),
    %% Make sure other side has evaluated controlling_process
    %% before message is sent
    ct:sleep(?SLEEP),
    ssl:send(Socket, Msg),
    no_result_msg.

controller_dies_result(_Socket, _Pid, _Msg) ->
    receive Result -> Result end.
send_recv_result_timeout_client(Socket) ->
    try ssl:recv(Socket, 11, not_infinity) catch error : function_clause -> ok end,
    try ssl:recv(Socket, 11, -1) catch error : function_clause -> ok end,
    try ssl:recv(Socket, not_integer, 500) catch error : function_clause -> ok end,
    try ssl:recv(Socket, -1, 500) catch error : function_clause -> ok end,
    {error, timeout} = ssl:recv(Socket, 11, 500),
    {error, timeout} = ssl:recv(Socket, 11, 0),
    ssl:send(Socket, "Hello world"),
    receive
	Msg ->
	    io:format("Msg ~p~n",[Msg])
    after 500 ->
	    ok
    end,
    {ok, "Hello world"} = ssl:recv(Socket, 11, 500),
    ok.
send_recv_result_timeout_server(Socket) ->
    ssl:send(Socket, "Hello"),
    {ok, "Hello world"} = ssl:recv(Socket, 11),
    ssl:send(Socket, " world"),
    ok.

do_recv_close(Socket) ->
    {error, closed} = ssl:recv(Socket, 11),
    receive
	{_,{error,closed}} ->
	    error_extra_close_sent_to_user_process
    after 500 ->
	    ok
    end.

tls_close(Socket) ->
    ok = ssl_test_lib:send_recv_result(Socket),
    %% So both sides will have time to send
    %% and recv before close is called, as this
    %% function will be called by both client
    %% and server in the test.
    ct:sleep(100),
    case ssl:close(Socket, 10000) of
        ok ->
            ok;
        {error, closed} ->
            ok;
        Other ->
            ct:fail(Other)
    end.

run_error_server_close([Pid | Opts]) ->
    {ok, Listen} = ssl:listen(0, Opts),
    {ok,{_, Port}} = ssl:sockname(Listen),
    Pid ! {self(), Port},
    {ok, Socket} = ssl:transport_accept(Listen),
    Pid ! ssl:close(Socket).

run_error_server([ Pid | Opts]) ->
    {ok, Listen} = ssl:listen(0, Opts),
    {ok,{_, Port}} = ssl:sockname(Listen),
    Pid ! {self(), Port},
    {ok, Socket} = ssl:transport_accept(Listen),
    Pid ! ssl:controlling_process(Socket, self()).

run_client_error([Port, Opts]) ->
    ssl:connect("localhost", Port, Opts).

no_recv_no_active(Socket) ->
    receive
        {ssl_closed, Socket} ->
            ct:fail(received_active_msg)
    after 500 ->
            ok
    end.

connection_info_result(Socket) ->
    {ok, Info} = ssl:connection_information(Socket, [protocol, selected_cipher_suite]),
    {ok, {proplists:get_value(protocol, Info), proplists:get_value(selected_cipher_suite, Info)}}.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

tls_or_dtls('dtlsv1') ->
    dtls;
tls_or_dtls('dtlsv1.2') ->
    dtls;
tls_or_dtls(_) ->
    tls.

active_n_common(S, N) ->
    ok = ssl:setopts(S, [{active,-N}]),
    receive
        {ssl_passive, S} -> ok
    after
        1000 ->
            error({error,ssl_passive_failure, flush()})
    end,
    [{active,false}] = ok(ssl:getopts(S, [active])),
    ok = ssl:setopts(S, [{active,0}]),
    receive
        {ssl_passive, S} -> ok
    after
        1000 ->
            error({error,ssl_passive_failure, flush()})
    end,
    ok = ssl:setopts(S, [{active,32767}]),
    {error,{options,_}} = ssl:setopts(S, [{active,1}]),
    {error,{options,_}} = ssl:setopts(S, [{active,-32769}]),
    ok = ssl:setopts(S, [{active,-32768}]),
    receive
        {ssl_passive, S} -> ok
    after
        1000 ->
            error({error,ssl_passive_failure, flush()})
    end,
    [{active,false}] = ok(ssl:getopts(S, [active])),
    ok = ssl:setopts(S, [{active,N}]),
    ok = ssl:setopts(S, [{active,true}]),
    [{active,true}] = ok(ssl:getopts(S, [active])),
    receive
        _Msg -> error({error,active_n, _Msg})
    after
        100 ->
            ok
    end,
    ok = ssl:setopts(S, [{active,N}]),
    ok = ssl:setopts(S, [{active,once}]),
    [{active,once}] = ok(ssl:getopts(S, [active])),
    receive
        _Msg2 -> error({error,active_n, _Msg2})
    after
        100 ->
            ok
    end,
    {error,{options,_}} = ssl:setopts(S, [{active,32768}]),
    ok = ssl:setopts(S, [{active,false}]),
    [{active,false}] = ok(ssl:getopts(S, [active])),
    ok.

flush() ->
    receive Msg ->
            [Msg|flush()]
    after 0 ->
            []
    end.


ok({ok,V}) -> V.

repeat(N, Fun) ->
    Repeat = fun F(Arg) when is_integer(Arg), Arg > 0 -> Fun(N - Arg), F(Arg - 1);
                        F(_) -> ok end,
    Repeat(N).


get_close(Pid, Where) ->
    receive
	{'EXIT', Pid, _Reason} ->
	    receive
		{_, {ssl_closed, Socket}} ->
		    ?CT_LOG("Socket closed ~p~n",[Socket]);
		Unexpected ->
		    ?CT_LOG("Unexpected ~p~n",[Unexpected]),
		    ct:fail({line, ?LINE-1})
	    after 5000 ->
		    ct:fail({timeout, {line, ?LINE, Where}})
	    end;
	Unexpected ->
	    ?CT_LOG("Unexpected ~p~n",[Unexpected]),
	    ct:fail({line, ?LINE-1})
    after 5000 ->
	    ct:fail({timeout, {line, ?LINE, Where}})
    end.

ssl_active_recv(N) ->
    ssl_active_recv(N, []).

ssl_active_recv(0, Acc) ->
    Acc;
ssl_active_recv(N, Acc) ->
    receive 
	{ssl, _, Bytes} ->
            ssl_active_recv(N-length(Bytes),  Acc ++ Bytes)
    end.



honor_cipher_order(Config, Honor, ServerCiphers, ClientCiphers, Expected) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, connection_info_result, []}},
					{options, [{ciphers, ServerCiphers}, {honor_cipher_order, Honor}
						   | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, connection_info_result, []}},
					{options, [{ciphers, ClientCiphers}
						   | ClientOpts]}]),

    Version = ssl_test_lib:protocol_version(Config),

    ServerMsg = ClientMsg = {ok, {Version, Expected}},

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

cookie_extension(Config, Cookie) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Data = "123456789012345",  %% 15 bytes

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% Trigger a HelloRetryRequest
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{options, [{supported_groups, [x448,
                                                                       secp256r1,
                                                                       secp384r1]},
                                                   {cookie, Cookie}|ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{options, ClientOpts}]),

    ok = ssl_test_lib:send(Client, Data),
    Data = ssl_test_lib:check_active_receive(Server, Data),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

start_client_negative(Config, Options, Error) ->
    ClientOpts = proplists:delete(versions, ssl_test_lib:ssl_options(client_rsa_opts, Config)),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Port = ssl_test_lib:inet_port(ServerNode),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
                                        {return_error, econnrefused},
					{mfa, {?MODULE, connection_info_result, []}},
					{options, Options ++ ClientOpts}]),
    ?CT_LOG("Actual: ~p~nExpected: ~p", [Client, {connect_failed, Error}]),
    {connect_failed, Error} = Client.

start_server_negative(Config, Options, Error) ->
    ServerOpts = proplists:delete(versions, ssl_test_lib:ssl_options(server_rsa_opts, Config)),
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, connection_info_result, []}},
					{options,  Options ++ ServerOpts}]),

    ?CT_LOG("Actual: ~p~nExpected: ~p", [Server,Error]),
    Error = Server.

der_input_opts(Opts) ->
    Certfile = proplists:get_value(certfile, Opts),
    CaCertsfile = proplists:get_value(cacertfile, Opts),
    Keyfile = proplists:get_value(keyfile, Opts),
    Dhfile = proplists:get_value(dhfile, Opts),
    [{_, Cert, _}] = ssl_test_lib:pem_to_der(Certfile),
    [{Asn1Type, Key, _}]  = ssl_test_lib:pem_to_der(Keyfile),
    [{_, DHParams, _}]  = ssl_test_lib:pem_to_der(Dhfile),
    CaCerts =
	lists:map(fun(Entry) ->
			  {_, CaCert, _} = Entry,
			  CaCert
		  end, ssl_test_lib:pem_to_der(CaCertsfile)),
    {Cert, {Asn1Type, Key}, CaCerts, DHParams}.

ssl_getstat(Socket) ->
    ssl:send(Socket, "From Erlang to Erlang"),
    {ok, Stats} = ssl:getstat(Socket),
    List = lists:dropwhile(fun({_, 0}) ->
                                   true;
                              ({_, _}) ->
                                   false
                           end, Stats),
    case List of
        [] ->
            nok;
        _  ->
            ok
    end.

test_versions_for_option_based_on_sni('tlsv1.3') ->
    {'tlsv1.2', ['tlsv1.3', 'tlsv1.2']};
test_versions_for_option_based_on_sni('tlsv1.2') ->
    {'tlsv1.1', ['tlsv1.2', 'tlsv1.1']}.

protocol_version_check(Socket, Version) ->
    case ssl:connection_information(Socket, [protocol]) of
        {ok, [{protocol, Version}]} ->
            ok;
        Other ->
            ct:fail({expected, Version, got, Other})
    end.

log(#{msg:={report,_Report}},#{config:=Pid}) ->
    Pid ! warning_generated;
log(_,_) ->
    ok.

length_exclusive(Version) when Version == 'tlsv1.3';
                               Version == 'tlsv1.2';
                               Version == 'tlsv1.1';
                               Version == 'tlsv1' ->
    length(exclusive_default_up_to_version(Version)) +
        length(exclusive_non_default_up_to_version(Version));
length_exclusive(Version) when Version == 'dtlsv1.2';
                               Version == 'dtlsv1' ->
    length(dtls_exclusive_default_up_to_version(Version)) +
        length(dtls_exclusive_non_default_up_to_version(Version)).

length_all(Version) ->
    length(ssl:cipher_suites(all, Version)).

exclusive_default_up_to_version(Version) ->
    lists:flatmap(fun (Vsn) ->

                          ssl:cipher_suites(exclusive, Vsn)
                  end,
                  up_to_version_helper(Version)).

up_to_version_helper('tlsv1.3') -> ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', 'tlsv1'];
up_to_version_helper('tlsv1.2') -> ['tlsv1.2', 'tlsv1.1', 'tlsv1'];
up_to_version_helper('tlsv1.1') -> ['tlsv1.1', 'tlsv1'];
up_to_version_helper('tlsv1') -> ['tlsv1'].


dtls_exclusive_default_up_to_version(Version) ->
    lists:flatmap(fun (Vsn) ->
                          ssl:cipher_suites(exclusive, Vsn)
                  end,
                  dtls_up_to_version_helper(Version)).

dtls_up_to_version_helper('dtlsv1.2') -> ['dtlsv1.2', 'dtlsv1'];
dtls_up_to_version_helper('dtlsv1') -> ['dtlsv1'].


exclusive_non_default_up_to_version(Version) ->
    lists:flatmap(fun exclusive_non_default_version/1,
                  up_to_version_helper(Version)).


dtls_exclusive_non_default_up_to_version(Version) ->
    lists:flatmap(fun dtls_exclusive_non_default_version/1,
                  dtls_up_to_version_helper(Version)).

exclusive_non_default_version(Version) ->
    Ls = [ fun tls_v1:psk_exclusive/1
         , fun tls_v1:srp_exclusive/1
         , fun tls_v1:rsa_exclusive/1
         , fun tls_v1:des_exclusive/1
         , fun tls_v1:rc4_exclusive/1],
    lists:flatmap(fun(Fn) -> Fn(Version) end, Ls).

dtls_exclusive_non_default_version(DTLSVersion) ->        
    Version = ssl:tls_version(dtls_record:protocol_version_name(DTLSVersion)),
    Fns = [ fun tls_v1:psk_exclusive/1
          , fun tls_v1:srp_exclusive/1
          , fun tls_v1:rsa_exclusive/1
          , fun tls_v1:des_exclusive/1],
    lists:flatmap(fun (Fn) -> Fn(Version) end, Fns).

selected_peer(ExpectedClient,
              ExpectedServer, ClientOpts, ServerOpts, Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {?MODULE, check_peercert, [ExpectedServer]}},
                                        {options, ssl_test_lib:ssl_options(ServerOpts, Config)}]),
    Port = ssl_test_lib:inet_port(Server),

    Client  = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                         {host, Hostname},
                                         {from, self()},
                                         {mfa, {?MODULE, check_peercert, [ExpectedClient]}},
                                         {options,  ssl_test_lib:ssl_options(ClientOpts, Config)}
					]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    %% Make sure to start next test fresh
    ssl:clear_pem_cache(),
    ok.

test_config('tlsv1.3', _) ->
    #{server_config := SEDDSAOpts,
      client_config := CEDDSAOpts} = eddsa_cert_chains(),
    #{server_config := SECDSAOpts,
      client_config := CECDSAOpts } = ecdsa_cert_chains(),

    {SEDDSACert, SEDDSAKey, SEDDSACACerts} = get_single_options(cert, key, cacerts, SEDDSAOpts),
    {CEDDSACert, CEDDSAKey, CEDDSACACerts} = get_single_options(cert, key, cacerts, CEDDSAOpts),

    {SECDSACert, SECDSAKey, SECDSACACerts} = get_single_options(cert, key, cacerts, SECDSAOpts),
    {CECDSACert, CECDSAKey, CECDSACACerts} = get_single_options(cert, key, cacerts, CECDSAOpts),

    ServerCertKeys = [#{cert => SECDSACert, key => SECDSAKey},
                      #{cert => SEDDSACert, key => SEDDSAKey}],

    ClientCertKeys = [#{cert => CECDSACert, key => CECDSAKey},
                      #{cert => CEDDSACert, key => CEDDSAKey}],

    [{#{server_config => [{certs_keys,ServerCertKeys},
                          {verify, verify_peer},  {versions, ['tlsv1.3', 'tlsv1.2']},
                          {cacerts, SEDDSACACerts ++ SECDSACACerts}],
        client_config => [{certs_keys, ClientCertKeys},
                          {verify, verify_peer}, {versions, ['tlsv1.3', 'tlsv1.2']},
                          {cacerts, CEDDSACACerts ++ CECDSACACerts}]
       },
      {client_peer, SEDDSACert}, {server_peer, CEDDSACert}},
     {#{server_config => [{certs_keys, ServerCertKeys},
                          {verify, verify_peer}, {versions, ['tlsv1.2']},
                          {cacerts, SEDDSACACerts ++ SECDSACACerts}],
        client_config => [{certs_keys, ClientCertKeys},
                          {verify, verify_peer},  {versions, ['tlsv1.2']},
                          {cacerts, CEDDSACACerts ++ CECDSACACerts}]},
      {client_peer, SECDSACert}, {server_peer, CECDSACert}}
    ];
test_config('tlsv1.2', _) ->
    #{server_config := SRSAOpts,
      client_config := CRSAOpts} = eddsa_cert_chains(),
    #{server_config := SDSAOpts,
      client_config := CDSAOpts} = dsa_cert_chains(),

    {SRSACert, SRSAKey, SRSACACerts} = get_single_options(cert, key, cacerts, SRSAOpts),
    {CRSACert, CRSAKey, CRSACACerts} = get_single_options(cert, key, cacerts,  CRSAOpts),

    {SDSACert, SDSAKey, SDSACACerts} = get_single_options(cert, key, cacerts, SDSAOpts),
    {CDSACert, CDSAKey, CDSACACerts} = get_single_options(cert, key, cacerts,  CDSAOpts),

    AllSigAlgs1_2 = ssl_test_lib:all_1_2_sig_algs(),

    [{#{server_config => [{certs_keys,
                           [#{cert => SDSACert, key => SDSAKey},
                            #{cert => SRSACert, key => SRSAKey}]},
                          {verify, verify_peer},
                          {versions, ['tlsv1.3', 'tlsv1.2']},
                          {cacerts, SRSACACerts ++ SDSACACerts}],
        client_config => [{certs_keys,
                           [#{cert => CDSACert, key => CDSAKey},
                            #{cert => CRSACert, key => CRSAKey}]},
                          {verify, verify_peer},
                          {versions, ['tlsv1.3', 'tlsv1.2']},
                          {cacerts, CRSACACerts ++ CDSACACerts}]
       },
      {client_peer, SRSACert}, {server_peer, CRSACert}},
     {#{server_config => [{certs_keys, [#{cert => SDSACert, key => SDSAKey},
                                        #{cert => SRSACert, key => SRSAKey}]},
                          {verify, verify_peer},
                          AllSigAlgs1_2,
                          {versions, ['tlsv1.2']},
                          {cacerts, SRSACACerts ++ SDSACACerts}],
        client_config => [{certs_keys, [#{cert => CDSACert, key => CDSAKey},
                                        #{cert => CRSACert, key => CRSAKey}]},
                          {verify, verify_peer},
                          {versions, ['tlsv1.2']},
                          AllSigAlgs1_2,
                          {cacerts, CRSACACerts ++ CDSACACerts}]
       },
      {client_peer, SDSACert}, {server_peer, CDSACert}}];
test_config('dtlsv1.2', Config) ->
    #{server_config := SRSAPSSOpts,
      client_config := CRSAPSSOpts} = ssl_test_lib:make_rsa_pss_pem(rsa_pss_pss, [], Config, "dtls_pss_pss_conf"),
    #{server_config := SRSAPSSRSAEOpts,
      client_config := CRSAPSSRSAEOpts} = ssl_test_lib:make_rsa_pss_pem(rsa_pss_rsae, [], Config, "dtls_pss_rsae_conf"),

    {SRSAPSSCert, SRSAPSSKey, SRSAPSSCACerts} = get_single_options(certfile, keyfile, cacertfile, SRSAPSSOpts),
    {CRSAPSSCert, CRSAPSSKey, CRSAPSSCACerts} = get_single_options(certfile, keyfile, cacertfile,  CRSAPSSOpts),

    {SRSAPSSRSAECert, SRSAPSSRSAEKey, SRSAPSSRSAECACerts} = get_single_options(certfile, keyfile, cacertfile, SRSAPSSRSAEOpts),
    {CRSAPSSRSAECert, CRSAPSSRSAEKey, CRSAPSSRSAECACerts} = get_single_options(certfile, keyfile, cacertfile, CRSAPSSRSAEOpts),

    [{#{server_config => [{certs_keys, [#{certfile => SRSAPSSRSAECert, keyfile => SRSAPSSRSAEKey},
                                        #{certfile => SRSAPSSCert, keyfile => SRSAPSSKey}]},
                          {verify, verify_peer},
                          {cacertfile, SRSAPSSCACerts}],
        client_config => [{certs_keys, [#{certfile => CRSAPSSRSAECert, keyfile => CRSAPSSRSAEKey},
                                        #{certfile => CRSAPSSCert, keyfile => CRSAPSSKey}]},
                          {verify, verify_peer},
                          {signature_algs, [rsa_pss_pss_sha256]},
                          {cacertfile, CRSAPSSCACerts}]
       },
      {client_peer, pem_to_der_cert(SRSAPSSCert)}, {server_peer, pem_to_der_cert(CRSAPSSCert)}},
     {#{server_config => [{certs_keys, [#{certfile => SRSAPSSRSAECert, keyfile => SRSAPSSRSAEKey},
                                        #{certfile => SRSAPSSCert, keyfile => SRSAPSSKey}]},
                          {verify, verify_peer},
                          {cacertfile, SRSAPSSRSAECACerts}],
        client_config => [{certs_keys, [#{certfile => CRSAPSSRSAECert, keyfile => CRSAPSSRSAEKey}]},
                          {verify, verify_peer}, {signature_algs, [rsa_pss_rsae_sha256]},
                          {cacertfile, CRSAPSSRSAECACerts}]
       },
      {client_peer, pem_to_der_cert(SRSAPSSRSAECert)}, {server_peer, pem_to_der_cert(CRSAPSSRSAECert)}}
    ];
test_config(_, Config) ->
    RSAConf1 = ssl_test_lib:make_rsa_cert(Config),
    SRSA1Opts = ssl_test_lib:ssl_options(server_rsa_opts, RSAConf1),
    CRSA1Opts = ssl_test_lib:ssl_options(client_rsa_opts, RSAConf1),

    RSAConf2 = ssl_test_lib:make_rsa_1024_cert(Config),
    SRSA2Opts = ssl_test_lib:ssl_options(server_rsa_1024_opts, RSAConf2),
    CRSA2Opts = ssl_test_lib:ssl_options(client_rsa_1024_opts, RSAConf2),

    {SRSA1Cert, SRSA1Key, _SRSA1CACerts} = get_single_options(certfile, keyfile, cacertfile, SRSA1Opts),
    {CRSA1Cert, CRSA1Key, _CRSA1CACerts} = get_single_options(certfile, keyfile, cacertfile, CRSA1Opts),

    {SRSA2Cert, SRSA2Key, SRSA2CACerts} = get_single_options(certfile, keyfile, cacertfile, SRSA2Opts),
    {CRSA2Cert, CRSA2Key, CRSA2CACerts} = get_single_options(certfile, keyfile, cacertfile, CRSA2Opts),

    [{#{server_config => [{certs_keys, [#{certfile => SRSA2Cert, keyfile => SRSA2Key},
                                        #{certfile => SRSA1Cert, keyfile => SRSA1Key}]},
                          {verify, verify_peer},
                          {cacertfile, SRSA2CACerts}],
        client_config => [{certs_keys, [#{certfile => CRSA2Cert, keyfile  => CRSA2Key},
                                        #{certfile => CRSA1Cert, keyfile => CRSA1Key}]},
                          {verify, verify_peer},
                          {cacertfile, CRSA2CACerts}]
       }, {client_peer,  pem_to_der_cert(SRSA2Cert)}, {server_peer, pem_to_der_cert(CRSA2Cert)}}].

check_peercert(Socket, Cert) ->
    case ssl:peercert(Socket) of
        {ok, Cert} ->
            ok;
        {ok, Other} ->
            {error, {{expected, public_key:pkix_decode_cert(Cert, otp)}, {got, public_key:pkix_decode_cert(Other, otp)}}}
    end.


eddsa_cert_chains() ->
    public_key:pkix_test_data(#{server_chain => #{root => ssl_test_lib:eddsa_conf(),
                                                  intermediates => [ssl_test_lib:eddsa_conf()],
                                                  peer =>  ssl_test_lib:eddsa_conf()},
                                client_chain => #{root => ssl_test_lib:eddsa_conf(),
                                                  intermediates => [ssl_test_lib:eddsa_conf()],
                                                  peer =>  ssl_test_lib:eddsa_conf()}}).

ecdsa_cert_chains() ->
    public_key:pkix_test_data(#{server_chain => #{root => ssl_test_lib:ecdsa_conf(),
                                                  intermediates => [ssl_test_lib:ecdsa_conf()],
                                                  peer =>  ssl_test_lib:ecdsa_conf()},
                                client_chain => #{root => ssl_test_lib:ecdsa_conf(),
                                                  intermediates => [ssl_test_lib:ecdsa_conf()],
                                                  peer =>  ssl_test_lib:ecdsa_conf()}}).
dsa_cert_chains() ->
    public_key:pkix_test_data(#{server_chain => #{root => [{key, ssl_test_lib:hardcode_dsa_key(1)}],
                                                  intermediates => [[{key, ssl_test_lib:hardcode_dsa_key(2)}]],
                                                  peer =>  [{key, ssl_test_lib:hardcode_dsa_key(3)}]
                                                 },
                                client_chain => #{root => [{key, ssl_test_lib:hardcode_dsa_key(3)}],
                                                  intermediates => [[{key, ssl_test_lib:hardcode_dsa_key(2)}]],
                                                  peer => [{key, ssl_test_lib:hardcode_dsa_key(1)}]}}).

get_single_options(CertOptName, KeyOptName, CaOptName, Opts) ->
    CertOpt = proplists:get_value(CertOptName, Opts),
    KeyOpt = proplists:get_value(KeyOptName, Opts),
    CaOpt = proplists:get_value(CaOptName, Opts),
    {CertOpt, KeyOpt, CaOpt}.

pem_to_der_cert(Pem) ->
    [{'Certificate', Der, _}] = ssl_test_lib:pem_to_der(Pem),
    Der.

test_sha1_cert_conf('tlsv1.3'= Version, RSA, ECDSA, Config) ->
    run_sha1_cert_conf(Version, ECDSA, Config, ecdsa_sha1),
    run_sha1_cert_conf(Version, RSA, Config, rsa_pkcs1_sha1);
test_sha1_cert_conf(Version, RSA, ECDSA, Config) when Version == 'tlsv1.2';
                                                      Version == 'dtlsv1.2' ->
    run_sha1_cert_conf(Version, RSA, Config, {sha, rsa}),
    run_sha1_cert_conf(Version, ECDSA, Config, {sha, ecdsa});
test_sha1_cert_conf(Version,RSA,_,Config) ->
    run_sha1_cert_conf(Version, RSA, Config, undefined).

run_sha1_cert_conf('tlsv1.3', #{client_config := ClientOpts, server_config := ServerOpts}, Config, LegacyAlg) ->
    SigAlgs = [%% ECDSA
               ecdsa_secp521r1_sha512,
               ecdsa_secp384r1_sha384,
               ecdsa_secp256r1_sha256,
               %% RSASSA-PSS
               rsa_pss_pss_sha512,
               rsa_pss_pss_sha384,
               rsa_pss_pss_sha256,
               rsa_pss_rsae_sha512,
               rsa_pss_rsae_sha384,
               rsa_pss_rsae_sha256,
               %% EDDSA
               eddsa_ed25519,
               eddsa_ed448
              ],
    IncludeLegacyAlg =  SigAlgs ++ [LegacyAlg],
    Extra = case proplists:get_value(transport, Config) of
                socket -> [{cb_info, tls_socket_tcp:cb_info()}];
                _ -> []
            end,
    ssl_test_lib:basic_alert([{verify, verify_peer}, {signature_algs,  SigAlgs} | ClientOpts ++ Extra],
                             [{signature_algs,  IncludeLegacyAlg} | ServerOpts ++ Extra], Config, handshake_failure),
    ssl_test_lib:basic_test([{verify, verify_peer}, {signature_algs,  IncludeLegacyAlg} | ClientOpts ++ Extra],
                            [{signature_algs,  IncludeLegacyAlg} | ServerOpts ++ Extra], Config);

run_sha1_cert_conf(Version, #{client_config := ClientOpts, server_config := ServerOpts}, Config, LegacyAlg) when Version == 'tlsv1.2';
                                                                                                                 Version == 'dtlsv1.2' ->
    SigAlgs =  [%% SHA2
                {sha512, ecdsa},
                {sha512, rsa},
                {sha384, ecdsa},
                {sha384, rsa},
                {sha256, ecdsa},
                {sha256, rsa},
                {sha224, ecdsa},
                {sha224, rsa}],
    IncludeLegacyAlg = SigAlgs ++ [LegacyAlg],
    ssl_test_lib:basic_test( [{verify, verify_peer}, {signature_algs,  IncludeLegacyAlg} | ClientOpts],
                             [{signature_algs,  IncludeLegacyAlg} | ServerOpts], Config);
run_sha1_cert_conf(_, #{client_config := ClientOpts, server_config := ServerOpts}, Config, LegacyAlg) ->
    NVersion = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    SigOpts = ssl_test_lib:sig_algs(LegacyAlg, NVersion),
    ssl_test_lib:basic_test([{verify, verify_peer} | ClientOpts] ++ SigOpts, ServerOpts, Config).


rsa_cipher_suites_not_default('tlsv1.3'= Version) ->
    [_ | Suites] = ssl:cipher_suites(default, Version),
    Suites;
rsa_cipher_suites_not_default(Version) ->
    ssl_test_lib:test_ciphers(ecdhe_rsa, aes_128_gcm, Version).

suite_check(Socket, 'tlsv1.3'= Version) ->
    [_, Suite| _] = ssl:cipher_suites(default, Version),
    case ssl:connection_information(Socket, [selected_cipher_suite]) of
        {ok, [{selected_cipher_suite, Suite}]} ->
            ok;
        Other ->
            ct:fail({expected, Suite, got, Other})
    end;
suite_check(Socket, Version) ->
    [Suite |_] = rsa_cipher_suites_not_default(Version),
    case ssl:connection_information(Socket, [selected_cipher_suite]) of
        {ok, [{selected_cipher_suite, Suite}]} ->
            ok;
        Other ->
            ct:fail({expected, Suite, got, Other})
    end.
