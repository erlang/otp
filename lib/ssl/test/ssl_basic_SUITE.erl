%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-include("ssl_api.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("tls_record.hrl").
-include("tls_handshake.hrl").

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
     {group, basic_tls},
     {group, options},
     {group, options_tls},
     {group, session},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'},
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'}
    ].

groups() ->
    [{basic, [], basic_tests()},
     {basic_tls, [], basic_tests_tls()},
     {options, [], options_tests()},
     {options_tls, [], options_tests_tls()},
     {'dtlsv1.2', [], all_versions_groups()},
     {'dtlsv1', [], all_versions_groups()},
     {'tlsv1.3', [], tls13_test_group()},
     {'tlsv1.2', [], all_versions_groups() ++ tls_versions_groups() ++ [conf_signature_algs, no_common_signature_algs]},
     {'tlsv1.1', [], all_versions_groups() ++ tls_versions_groups()},
     {'tlsv1', [], all_versions_groups() ++ tls_versions_groups() ++ rizzo_tests()},
     {'sslv3', [], all_versions_groups() ++ tls_versions_groups() ++ rizzo_tests() ++ [tls_ciphersuite_vs_version]},
     {api,[], api_tests()},
     {api_tls,[], api_tests_tls()},
     {session, [], session_tests()},
     {renegotiate, [], renegotiate_tests()},
     {ciphers, [], cipher_tests()},
     {error_handling_tests, [], error_handling_tests()},
     {error_handling_tests_tls, [], error_handling_tests_tls()}
    ].

tls_versions_groups ()->
    [
     {group, api_tls},
     {group, error_handling_tests_tls}].

all_versions_groups ()->
    [{group, api},
     {group, renegotiate},
     {group, ciphers},
     {group, error_handling_tests}].


basic_tests() ->
    [app,
     appup,
     alerts,
     alert_details,
     alert_details_not_too_big,
     version_option,
     connect_twice,
     connect_dist,
     clear_pem_cache,
     defaults,
     fallback,
     cipher_format,
     suite_to_str
    ].

basic_tests_tls() ->
    [tls_send_close
    ].

options_tests() ->
    [der_input,
     ssl_options_not_proplist,
     raw_ssl_option,
     invalid_inet_get_option,
     invalid_inet_get_option_not_list,
     invalid_inet_get_option_improper_list,
     invalid_inet_set_option,
     invalid_inet_set_option_not_list,
     invalid_inet_set_option_improper_list,
     dh_params,
     invalid_certfile,
     invalid_cacertfile,
     invalid_keyfile,
     invalid_options,
     protocol_versions,
     empty_protocol_versions,
     ipv6,
     reuseaddr,
     honor_server_cipher_order,
     honor_client_cipher_order,
     unordered_protocol_versions_server,
     unordered_protocol_versions_client,
     max_handshake_size
].

options_tests_tls() ->
    [tls_misc_ssl_options,
     tls_tcp_reuseaddr].

api_tests() ->
    [secret_connection_info,
     connection_information,
     peercert,
     peercert_with_client_cert,
     versions,
     eccs,
     controlling_process,
     getstat,
     close_with_timeout,
     hibernate,
     hibernate_right_away,
     listen_socket,
     ssl_recv_timeout,
     server_name_indication_option,
     accept_pool,
     prf,
     socket_options,
     active_n,
     internal_active_1,
     cipher_suites,
     handshake_continue,
     handshake_continue_timeout,
     hello_client_cancel,
     hello_server_cancel
    ].

api_tests_tls() ->
    [tls_versions_option,
     tls_upgrade,
     tls_upgrade_with_timeout,
     tls_ssl_accept_timeout,
     tls_downgrade,
     tls_shutdown,
     tls_shutdown_write,
     tls_shutdown_both,
     tls_shutdown_error,
     peername,
     sockname,
     tls_socket_options,
     new_options_in_accept
    ].

session_tests() ->
    [reuse_session,
     reuse_session_expired,
     server_does_not_want_to_reuse_session,
     no_reuses_session_server_restart_new_cert,
     no_reuses_session_server_restart_new_cert_file].

renegotiate_tests() ->
    [client_renegotiate,
     server_renegotiate,
     client_secure_renegotiate,
     client_secure_renegotiate_fallback,
     client_renegotiate_reused_session,
     server_renegotiate_reused_session,
     client_no_wrap_sequence_number,
     server_no_wrap_sequence_number,
     renegotiate_dos_mitigate_active,
     renegotiate_dos_mitigate_passive,
     renegotiate_dos_mitigate_absolute].

cipher_tests() ->
    [old_cipher_suites,
     cipher_suites_mix,     
     default_reject_anonymous].

error_handling_tests()->
    [close_transport_accept,
     recv_active,
     recv_active_once,
     recv_active_n,
     recv_error_handling,
     call_in_error_state,
     close_in_error_state,
     abuse_transport_accept_socket,
     controlling_process_transport_accept_socket
    ].

error_handling_tests_tls()->
    [controller_dies,
     tls_client_closes_socket,
     tls_closed_in_active_once,
     tls_tcp_error_propagation_in_active_mode,
     tls_tcp_connect,
     tls_tcp_connect_big,
     tls_dont_crash_on_handshake_garbage
    ].

rizzo_tests() ->
    [rizzo,
     no_rizzo_rc4,
     rizzo_one_n_minus_one,
     rizzo_zero_n,
     rizzo_disabled].

%% For testing TLS 1.3 features and possible regressions
tls13_test_group() ->
    [handshake_continue_tls13_client,
     tls13_enable_client_side,
     tls13_enable_server_side,
     tls_record_1_3_encode_decode,
     tls13_finished_verify_data,
     tls13_1_RTT_handshake,
     tls12_ssl_server_tls13_ssl_client,
     tls13_basic_ssl_server_openssl_client,
     tls13_custom_groups_ssl_server_openssl_client,
     tls13_hello_retry_request_ssl_server_openssl_client,
     tls13_client_auth_empty_cert_alert_ssl_server_openssl_client,
     tls13_client_auth_empty_cert_ssl_server_openssl_client,
     tls13_client_auth_ssl_server_openssl_client,
     tls13_hrr_client_auth_empty_cert_alert_ssl_server_openssl_client,
     tls13_hrr_client_auth_empty_cert_ssl_server_openssl_client,
     tls13_hrr_client_auth_ssl_server_openssl_client,
     tls13_unsupported_sign_algo_client_auth_ssl_server_openssl_client,
     tls13_unsupported_sign_algo_cert_client_auth_ssl_server_openssl_client,
     tls13_connection_information].

%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
	    %% make rsa certs using oppenssl
	    {ok, _} = make_certs:all(proplists:get_value(data_dir, Config0),
				     proplists:get_value(priv_dir, Config0)),
	    Config1 = ssl_test_lib:make_dsa_cert(Config0),
	    Config2 = ssl_test_lib:make_ecdsa_cert(Config1),
            Config3 = ssl_test_lib:make_rsa_cert(Config2),
	    Config = ssl_test_lib:make_ecdh_rsa_cert(Config3),
	    ssl_test_lib:cert_options(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------

init_per_group(GroupName, Config) when GroupName == basic_tls;
                                       GroupName == options_tls;
                                       GroupName == options;
                                       GroupName == basic;
                                       GroupName == session;
                                       GroupName == error_handling_tests_tls;
                                       GroupName == tls13_test_group
                                       ->
    ssl_test_lib:clean_tls_version(Config);                          
init_per_group(GroupName, Config) ->
    ssl_test_lib:clean_tls_version(Config),                          
    case ssl_test_lib:is_tls_version(GroupName) andalso ssl_test_lib:sufficient_crypto_support(GroupName) of
	true ->
	    ssl_test_lib:init_tls_version(GroupName, Config);
	_ ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    ssl:start(),
		    Config;
		false ->
		    {skip, "Missing crypto support"}
	    end
    end.

end_per_group(GroupName, Config) ->
  case ssl_test_lib:is_tls_version(GroupName) of
      true ->
          ssl_test_lib:clean_tls_version(Config);
      false ->
          Config
  end.

%%--------------------------------------------------------------------
init_per_testcase(Case, Config) when Case ==  unordered_protocol_versions_client;
				     Case == unordered_protocol_versions_server->
    case proplists:get_value(supported, ssl:versions()) of
	['tlsv1.2' | _] ->
	    ct:timetrap({seconds, 5}),
	    Config;
	_ ->
	    {skip, "TLS 1.2 need but not supported on this platform"}
    end;

init_per_testcase(protocol_versions, Config)  ->
    ssl:stop(),
    application:load(ssl),
    %% For backwards compatibility sslv2 should be filtered out.
    application:set_env(ssl, protocol_version, [sslv2, sslv3, tlsv1]),
    ssl:start(),
    ct:timetrap({seconds, 5}),
    Config;

init_per_testcase(reuse_session_expired, Config)  ->
    ssl:stop(),
    application:load(ssl),
    ssl_test_lib:clean_env(),
    application:set_env(ssl, session_lifetime, ?EXPIRE),
    application:set_env(ssl, session_delay_cleanup_time, 500),
    ssl:start(),
    ct:timetrap({seconds, 30}),
    Config;

init_per_testcase(empty_protocol_versions, Config)  ->
    ssl:stop(),
    application:load(ssl),
    ssl_test_lib:clean_env(),
    application:set_env(ssl, protocol_version, []),
    ssl:start(),
    ct:timetrap({seconds, 5}),
    Config;

init_per_testcase(fallback, Config)  ->
    case tls_record:highest_protocol_version([]) of
	{3, N} when N > 1 ->
	    ct:timetrap({seconds, 5}),
	    Config;
	_ ->
	    {skip, "Not relevant if highest supported version is less than 3.2"}
    end;

init_per_testcase(TestCase, Config) when TestCase == client_renegotiate;
					 TestCase == server_renegotiate;
					 TestCase == client_secure_renegotiate;
					 TestCase == client_renegotiate_reused_session;
					 TestCase == server_renegotiate_reused_session;
					 TestCase == client_no_wrap_sequence_number;
					 TestCase == server_no_wrap_sequence_number;
					 TestCase == renegotiate_dos_mitigate_active;
					 TestCase == renegotiate_dos_mitigate_passive;
					 TestCase == renegotiate_dos_mitigate_absolute ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, ?SEC_RENEGOTIATION_TIMEOUT + 5}),
    Config;

init_per_testcase(TestCase, Config) when TestCase == versions_option;
					 TestCase == tls_tcp_connect_big ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 60}),
    Config;

init_per_testcase(version_option, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config;

init_per_testcase(reuse_session, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config;

init_per_testcase(rizzo, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 60}),
    Config;

init_per_testcase(no_rizzo_rc4, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 60}),
    Config;

init_per_testcase(rizzo_one_n_minus_one, Config) ->
    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()]),
    ct:timetrap({seconds, 60}),
    rizzo_add_mitigation_option(one_n_minus_one, Config);

init_per_testcase(rizzo_zero_n, Config) ->
    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()]),
    ct:timetrap({seconds, 60}),
    rizzo_add_mitigation_option(zero_n, Config);

init_per_testcase(rizzo_disabled, Config) ->
    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()]),
    ct:timetrap({seconds, 60}),
    rizzo_add_mitigation_option(disabled, Config);

init_per_testcase(TestCase, Config) when TestCase == no_reuses_session_server_restart_new_cert_file;
                                         TestCase == no_reuses_session_server_restart_new_cert ->
    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()]),
    ct:timetrap({seconds, 15}),
    Config;

init_per_testcase(prf, Config) ->
    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()]),
    ct:timetrap({seconds, 40}),
    case proplists:get_value(tc_group_path, Config) of
        [] -> Prop = [];
        [Prop] -> Prop
    end,
    case proplists:get_value(name, Prop) of
        undefined -> TlsVersions = [sslv3, tlsv1, 'tlsv1.1', 'tlsv1.2'];
        TlsVersion when is_atom(TlsVersion) ->
            TlsVersions = [TlsVersion]
    end,
    PRFS=[md5, sha, sha256, sha384, sha512],
    %All are the result of running tls_v1:prf(PrfAlgo, <<>>, <<>>, <<>>, 16)
    %with the specified PRF algorithm
    ExpectedPrfResults=
    [{md5, <<96,139,180,171,236,210,13,10,28,32,2,23,88,224,235,199>>},
     {sha, <<95,3,183,114,33,169,197,187,231,243,19,242,220,228,70,151>>},
     {sha256, <<166,249,145,171,43,95,158,232,6,60,17,90,183,180,0,155>>},
     {sha384, <<153,182,217,96,186,130,105,85,65,103,123,247,146,91,47,106>>},
     {sha512, <<145,8,98,38,243,96,42,94,163,33,53,49,241,4,127,28>>},
     %TLS 1.0 and 1.1 PRF:
     {md5sha, <<63,136,3,217,205,123,200,177,251,211,17,229,132,4,173,80>>}],
    TestPlan = prf_create_plan(TlsVersions, PRFS, ExpectedPrfResults),
    [{prf_test_plan, TestPlan} | Config];

init_per_testcase(TestCase, Config) when TestCase == tls_ssl_accept_timeout;
					 TestCase == tls_client_closes_socket;
					 TestCase == tls_closed_in_active_once;
					 TestCase == tls_downgrade ->
    ssl:stop(),
    ssl:start(),
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 15}),
    Config;
init_per_testcase(TestCase, Config) when TestCase == clear_pem_cache;
						TestCase == der_input;
						TestCase == defaults ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    %% White box test need clean start
    ssl:stop(),
    ssl:start(),
    ct:timetrap({seconds, 20}),
    Config;
init_per_testcase(raw_ssl_option, Config) ->
    ct:timetrap({seconds, 5}),
    case os:type() of
        {unix,linux} ->
            Config;
        _ ->
            {skip, "Raw options are platform-specific"}
    end;

init_per_testcase(accept_pool, Config) ->
    ct:timetrap({seconds, 5}),
    case proplists:get_value(protocol, Config) of
	dtls ->
            {skip, "Not yet supported on DTLS sockets"};
	_ ->
	    ssl_test_lib:ct_log_supported_protocol_versions(Config),
	    Config
    end;

init_per_testcase(internal_active_1, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, internal_active_n, 1),
    ssl:start(),
    ct:timetrap({seconds, 5}),
    Config;

init_per_testcase(controller_dies, Config) ->
    ct:timetrap({seconds, 10}),
    Config;
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

end_per_testcase(reuse_session_expired, Config) ->
    application:unset_env(ssl, session_lifetime),
    application:unset_env(ssl, session_delay_cleanup_time),
    end_per_testcase(default_action, Config);

end_per_testcase(internal_active_n, Config) ->
    application:unset_env(ssl, internal_active_n),
    end_per_testcase(default_action, Config);

end_per_testcase(Case, Config) when Case == protocol_versions;
				    Case == empty_protocol_versions->
    application:unset_env(ssl, protocol_versions),
    end_per_testcase(default_action, Config);

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
app() ->
    [{doc, "Test that the ssl app file is ok"}].
app(Config) when is_list(Config) ->
    ok = ?t:app_test(ssl).
%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the ssl appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(ssl).
%%--------------------------------------------------------------------
alerts() ->
    [{doc, "Test ssl_alert:alert_txt/1"}].
alerts(Config) when is_list(Config) ->
    Descriptions = [?CLOSE_NOTIFY, ?UNEXPECTED_MESSAGE, ?BAD_RECORD_MAC,
		    ?DECRYPTION_FAILED_RESERVED, ?RECORD_OVERFLOW, ?DECOMPRESSION_FAILURE,
		    ?HANDSHAKE_FAILURE, ?BAD_CERTIFICATE, ?UNSUPPORTED_CERTIFICATE,
		    ?CERTIFICATE_REVOKED,?CERTIFICATE_EXPIRED, ?CERTIFICATE_UNKNOWN,
		    ?ILLEGAL_PARAMETER, ?UNKNOWN_CA, ?ACCESS_DENIED, ?DECODE_ERROR,
		    ?DECRYPT_ERROR, ?EXPORT_RESTRICTION, ?PROTOCOL_VERSION, 
		    ?INSUFFICIENT_SECURITY, ?INTERNAL_ERROR, ?USER_CANCELED,
		    ?NO_RENEGOTIATION, ?UNSUPPORTED_EXTENSION, ?CERTIFICATE_UNOBTAINABLE,
		    ?UNRECOGNISED_NAME, ?BAD_CERTIFICATE_STATUS_RESPONSE,
		    ?BAD_CERTIFICATE_HASH_VALUE, ?UNKNOWN_PSK_IDENTITY, 
		    255 %% Unsupported/unknow alert will result in a description too
		   ],
    Alerts = [?ALERT_REC(?WARNING, ?CLOSE_NOTIFY) | 
	      [?ALERT_REC(?FATAL, Desc) || Desc <- Descriptions]],
    lists:foreach(fun(Alert) ->
                          try ssl_alert:alert_txt(Alert)
                          catch
			    C:E:T ->
                                  ct:fail({unexpected, {C, E, T}})
			end 
		  end, Alerts).
%%--------------------------------------------------------------------
alert_details() ->
    [{doc, "Test that ssl_alert:alert_txt/1 result contains extendend error description"}].
alert_details(Config) when is_list(Config) ->
    Unique = make_ref(),
    UniqueStr = lists:flatten(io_lib:format("~w", [Unique])),
    Alert = ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY, Unique),
    case string:str(ssl_alert:alert_txt(Alert), UniqueStr) of
        0 ->
            ct:fail(error_details_missing);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
alert_details_not_too_big() ->
    [{doc, "Test that ssl_alert:alert_txt/1 limits printed depth of extended error description"}].
alert_details_not_too_big(Config) when is_list(Config) ->
    Reason = lists:duplicate(10, lists:duplicate(10, lists:duplicate(10, {some, data}))),
    Alert = ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY, Reason),
    case length(ssl_alert:alert_txt(Alert)) < 1000 of
        true ->
            ok;
        false ->
            ct:fail(ssl_alert_text_too_big)
    end.

%%--------------------------------------------------------------------
new_options_in_accept() ->
    [{doc,"Test that you can set ssl options in ssl_accept/3 and not only in tcp upgrade"}].
new_options_in_accept(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_dsa_opts, Config),
    [_ , _ | ServerSslOpts] = ssl_test_lib:ssl_options(server_opts, Config), %% Remove non ssl opts
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = ssl_test_lib:protocol_options(Config, [{tls, sslv3}, {dtls, dtlsv1}]),
    Cipher = ssl_test_lib:protocol_options(Config, [{tls, #{key_exchange =>rsa,
                                                            cipher => rc4_128,
                                                            mac => sha,
                                                            prf => default_prf
                                                           }}, 
                                                    {dtls, #{key_exchange =>rsa,
                                                             cipher => aes_128_cbc,
                                                             mac => sha,
                                                             prf => default_prf
                                                            }}]),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{ssl_extra_opts, [{versions, [Version]},
							  {ciphers,[Cipher]} | ServerSslOpts]}, %% To be set in ssl_accept/3
					{mfa, {?MODULE, connection_info_result, []}},
					{options, proplists:delete(cacertfile, ServerOpts0)}]),
    
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, connection_info_result, []}},
					{options, [{versions, [Version]},
						   {ciphers,[Cipher]} | ClientOpts]}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ServerMsg = ClientMsg = {ok, {Version, Cipher}},
   
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
handshake_continue() ->
    [{doc, "Test API function ssl:handshake_continue/3"}].
handshake_continue(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, ssl_test_lib:ssl_options([{reuseaddr, true}, {handshake, hello}], 
                                                                           Config)},
                                        {continue_options, proplists:delete(reuseaddr, ServerOpts)}
                                       ]),
    
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()}, 
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, ssl_test_lib:ssl_options([{handshake, hello}], 
                                                                           Config)},
                                        {continue_options,  proplists:delete(reuseaddr, ClientOpts)}]),
     
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
handshake_continue_tls13_client() ->
    [{doc, "Test API function ssl:handshake_continue/3"}].
handshake_continue_tls13_client(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],

    ClientOptsHello0 = ssl_test_lib:ssl_options([{handshake, hello}], Config),
    ClientOptsHello = [{versions, ['tlsv1.2','tlsv1.3']}|ClientOptsHello0],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, ssl_test_lib:ssl_options([{reuseaddr, true}, {handshake, hello}],
                                                                           Config)},
                                        {continue_options, proplists:delete(reuseaddr, ServerOpts)}
                                       ]),

    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, ClientOptsHello},
                                        {continue_options,  proplists:delete(reuseaddr, ClientOpts)}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%------------------------------------------------------------------
handshake_continue_timeout() ->
    [{doc, "Test API function ssl:handshake_continue/3 with short timeout"}].
handshake_continue_timeout(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_verification_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_verification_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {timeout, 1},
                                        {options, ssl_test_lib:ssl_options([{reuseaddr, true}, {handshake, hello}],
                                                                           Config)},
                                        {continue_options, proplists:delete(reuseaddr, ServerOpts)}
                                       ]),

    Port = ssl_test_lib:inet_port(Server),


    {connect_failed, _} = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                                     {host, Hostname},
                                                     {from, self()},
                                                     {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server, {error,timeout}),
    ssl_test_lib:close(Server).


%%--------------------------------------------------------------------
hello_client_cancel() ->
    [{doc, "Test API function ssl:handshake_cancel/1 on the client side"}].
hello_client_cancel(Config) when is_list(Config) -> 
    ServerOpts = ssl_test_lib:ssl_options(server_verification_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_verification_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{options, ssl_test_lib:ssl_options([{handshake, hello}], Config)},
                                        {continue_options, proplists:delete(reuseaddr, ServerOpts)}]),
    
    Port = ssl_test_lib:inet_port(Server),

    %% That is ssl:handshake_cancel returns ok
    {connect_failed, ok} = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                                      {host, Hostname},
                                                      {from, self()}, 
                                                      {options, ssl_test_lib:ssl_options([{handshake, hello}], Config)},
                                                      {continue_options, cancel}]),    
    ssl_test_lib:check_server_alert(Server, user_canceled).
%%--------------------------------------------------------------------
hello_server_cancel() ->
    [{doc, "Test API function ssl:handshake_cancel/1 on the server side"}].
hello_server_cancel(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{options, ssl_test_lib:ssl_options([{handshake, hello}], Config)},
                                        {continue_options, cancel}]),
    
    Port = ssl_test_lib:inet_port(Server),

    {connect_failed, _} = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                                     {host, Hostname},
                                                     {from, self()}, 
                                                     {options, ssl_test_lib:ssl_options([{handshake, hello}], Config)},
                                                     {continue_options, proplists:delete(reuseaddr, ClientOpts)}]),
    
    ssl_test_lib:check_result(Server, ok).

%%--------------------------------------------------------------------
prf() ->
    [{doc,"Test that ssl:prf/5 uses the negotiated PRF."}].
prf(Config) when is_list(Config) ->
    TestPlan = proplists:get_value(prf_test_plan, Config),
    case TestPlan of
        [] -> ct:fail({error, empty_prf_test_plan});
        _ -> lists:foreach(fun(Suite) ->
                                   lists:foreach(
                                     fun(Test) ->
                                             V = proplists:get_value(tls_ver, Test),
                                             C = proplists:get_value(ciphers, Test),
                                             E = proplists:get_value(expected, Test),
                                             P = proplists:get_value(prf, Test),
                                             prf_run_test(Config, V, C, E, P)
                                     end, Suite)
                           end, TestPlan)
    end.

%%--------------------------------------------------------------------

secret_connection_info() ->
    [{doc,"Test the API function ssl:connection_information/2"}].
secret_connection_info(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, secret_connection_info_result, []}},
					{options, ServerOpts}]),
    
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()}, 
                                        {mfa, {?MODULE, secret_connection_info_result, []}},
                                        {options, ClientOpts}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
			   
    ssl_test_lib:check_result(Server, true, Client, true),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------

connection_information() ->
    [{doc,"Test the API function ssl:connection_information/1"}].
connection_information(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    ServerMsg = ClientMsg = ok,
			   
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
protocol_versions() ->
    [{doc,"Test to set a list of protocol versions in app environment."}].

protocol_versions(Config) when is_list(Config) -> 
    basic_test(Config).

%%--------------------------------------------------------------------
empty_protocol_versions() ->
    [{doc,"Test to set an empty list of protocol versions in app environment."}].

empty_protocol_versions(Config) when is_list(Config) -> 
    basic_test(Config).

%%--------------------------------------------------------------------

controlling_process() ->
    [{doc,"Test API function controlling_process/2"}].

controlling_process(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
           [self(), Client, Server]),
    
    ServerMsg = ssl_test_lib:active_recv(CSocket, length(ServerMsg)),
    %% We do not have the TLS server socket but all messages form the client
    %% socket are now read, so ramining are form the server socket
    ClientMsg = ssl_active_recv(length(ClientMsg)),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
getstat() ->
    [{doc,"Test API function getstat/2"}].

getstat(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server1 =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {ssl_test_lib, send_recv_result, []}},
                                   {options,  [{active, false} | ServerOpts]}]),
    Port1 = ssl_test_lib:inet_port(Server1),
    Server2 =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {ssl_test_lib, send_recv_result, []}},
                                   {options,  [{active, false} | ServerOpts]}]),
    Port2 = ssl_test_lib:inet_port(Server2),
    {ok, ActiveC} = rpc:call(ClientNode, ssl, connect,
                          [Hostname,Port1,[{active, once}|ClientOpts]]),
    {ok, PassiveC} = rpc:call(ClientNode, ssl, connect,
                          [Hostname,Port2,[{active, false}|ClientOpts]]),

    ct:log("Testcase ~p, Client ~p  Servers ~p, ~p ~n",
                       [self(), self(), Server1, Server2]),

    %% We only check that the values are non-zero initially
    %% (due to the handshake), and that sending more changes the values.

    %% Passive socket.

    {ok, InitialStats} = ssl:getstat(PassiveC),
    ct:pal("InitialStats  ~p~n", [InitialStats]),
    [true] = lists:usort([0 =/= proplists:get_value(Name, InitialStats)
        || Name <- [recv_cnt, recv_oct, recv_avg, recv_max, send_cnt, send_oct, send_avg, send_max]]),

    ok = ssl:send(PassiveC, "Hello world"),
    wait_for_send(PassiveC),
    {ok, SStats} = ssl:getstat(PassiveC, [send_cnt, send_oct]),
    ct:pal("SStats  ~p~n", [SStats]),
    [true] = lists:usort([proplists:get_value(Name, SStats) =/= proplists:get_value(Name, InitialStats)
        || Name <- [send_cnt, send_oct]]),

    %% Active socket.

    {ok, InitialAStats} = ssl:getstat(ActiveC),
    ct:pal("InitialAStats  ~p~n", [InitialAStats]),
    [true] = lists:usort([0 =/= proplists:get_value(Name, InitialAStats)
        || Name <- [recv_cnt, recv_oct, recv_avg, recv_max, send_cnt, send_oct, send_avg, send_max]]),

    _ = receive
        {ssl, ActiveC, _} ->
            ok
    after
        ?SLEEP ->
            exit(timeout)
    end,

    ok = ssl:send(ActiveC, "Hello world"),
    wait_for_send(ActiveC),
    {ok, ASStats} = ssl:getstat(ActiveC, [send_cnt, send_oct]),
    ct:pal("ASStats  ~p~n", [ASStats]),
    [true] = lists:usort([proplists:get_value(Name, ASStats) =/= proplists:get_value(Name, InitialAStats)
        || Name <- [send_cnt, send_oct]]),

    ok.

%%--------------------------------------------------------------------
controller_dies() ->
    [{doc,"Test that the socket is closed after controlling process dies"}].
controller_dies(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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

    ct:log("Testcase ~p, Client ~p  Server ~p ~n", [self(), Client, Server]),
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

    ct:log("Wating on exit ~p~n",[Client3]),
    receive {'EXIT', Client3, normal} -> ok end,
    
    receive   %% Client3 is dead but that doesn't matter, socket should not be closed.
	Unexpected ->
	    ct:log("Unexpected ~p~n",[Unexpected]),
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
tls_client_closes_socket() ->
    [{doc,"Test what happens when client closes socket before handshake is compleated"}].

tls_client_closes_socket(Config) when is_list(Config) -> 
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],
    
    Server = ssl_test_lib:start_upgrade_server_error([{node, ServerNode}, {port, 0}, 
						      {from, self()}, 
						      {tcp_options, TcpOpts},
						      {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Connect = fun() ->
		      {ok, _Socket} = rpc:call(ClientNode, gen_tcp, connect, 
					      [Hostname, Port, [binary]]),	      
		      %% Make sure that ssl_accept is called before 
		      %% client process ends and closes socket.
		      ct:sleep(?SLEEP)
	      end,
    
    _Client = spawn_link(Connect),

    ssl_test_lib:check_result(Server, {error,closed}).

%%--------------------------------------------------------------------
tls_closed_in_active_once() ->
    [{doc, "Test that ssl_closed is delivered in active once with non-empty buffer, check ERL-420."}].

tls_closed_in_active_once(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {_ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],
    Port = ssl_test_lib:inet_port(node()),
    Server = fun() ->
		     {ok, Listen} = gen_tcp:listen(Port, TcpOpts),
		     {ok, TcpServerSocket} = gen_tcp:accept(Listen),
		     {ok, ServerSocket} = ssl:ssl_accept(TcpServerSocket, ServerOpts),
		     lists:foreach(
		       fun(_) ->
			       ssl:send(ServerSocket, "some random message\r\n")
		       end, lists:seq(1, 20)),
		     %% Close TCP instead of SSL socket to trigger the bug:
		     gen_tcp:close(TcpServerSocket),
		     gen_tcp:close(Listen)
	     end,
    spawn_link(Server),
    {ok, Socket} = ssl:connect(Hostname, Port, [{active, false} | ClientOpts]),
    Result = tls_closed_in_active_once_loop(Socket),
    ssl:close(Socket),
    case Result of
	ok -> ok;
	_ -> ct:fail(Result)
    end.

tls_closed_in_active_once_loop(Socket) ->
    case ssl:setopts(Socket, [{active, once}]) of
        ok ->
            receive
                {ssl, Socket, _} ->
                    tls_closed_in_active_once_loop(Socket);
                {ssl_closed, Socket} ->
                    ok
            after 5000 ->
                    no_ssl_closed_received
            end;
        {error, closed} ->
            ok
    end.
%%--------------------------------------------------------------------
connect_dist() ->
    [{doc,"Test a simple connect as is used by distribution"}].

connect_dist(Config) when is_list(Config) -> 
    ClientOpts0 = ssl_test_lib:ssl_options(client_kc_opts, Config),
    ClientOpts = [{ssl_imp, new},{active, false}, {packet,4}|ClientOpts0],
    ServerOpts0 = ssl_test_lib:ssl_options(server_kc_opts, Config),
    ServerOpts = [{ssl_imp, new},{active, false}, {packet,4}|ServerOpts0],

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

%%--------------------------------------------------------------------

clear_pem_cache() ->
    [{doc,"Test that internal reference tabel is cleaned properly even when "
     " the PEM cache is cleared" }].
clear_pem_cache(Config) when is_list(Config) -> 
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    [_,{FilRefDb, _} |_] = element(6, State),
    {Server, Client} = basic_verify_test_no_close(Config),
    CountReferencedFiles = fun({_, -1}, Acc) ->
				   Acc;
			      ({_, N}, Acc) ->
				   N + Acc
			   end,
    
    2 = ets:foldl(CountReferencedFiles, 0, FilRefDb), 
    ssl:clear_pem_cache(),
    _ = sys:get_status(whereis(ssl_manager)),
    {Server1, Client1} = basic_verify_test_no_close(Config),
    4 =  ets:foldl(CountReferencedFiles, 0, FilRefDb), 
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ct:sleep(2000),
    _ = sys:get_status(whereis(ssl_manager)),
    2 =  ets:foldl(CountReferencedFiles, 0, FilRefDb), 
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1),
    ct:sleep(2000),
    _ = sys:get_status(whereis(ssl_manager)),
    0 =  ets:foldl(CountReferencedFiles, 0, FilRefDb).

%%--------------------------------------------------------------------

fallback() ->
    [{doc, "Test TLS_FALLBACK_SCSV downgrade prevention"}].

fallback(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
    [{doc, "Test that cipher conversion from maps | tuples | stings to binarys works"}].
cipher_format(Config) when is_list(Config) ->
    {ok, Socket0} = ssl:listen(0, [{ciphers, ssl:cipher_suites(default, 'tlsv1.2')}]),
    ssl:close(Socket0),
    %% Legacy
    {ok, Socket1} = ssl:listen(0, [{ciphers, ssl:cipher_suites()}]),
    ssl:close(Socket1),
    {ok, Socket2} = ssl:listen(0, [{ciphers, ssl:cipher_suites(openssl)}]),
    ssl:close(Socket2).

%%--------------------------------------------------------------------
suite_to_str() ->
    [{doc, "Test that the suite_to_str API works"}].
suite_to_str(Config) when is_list(Config) ->
    "TLS_EMPTY_RENEGOTIATION_INFO_SCSV" =
        ssl:suite_to_str(#{key_exchange => null,
                           cipher => null,
                           mac => null,
                           prf => null}),
    "TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256" =
        ssl:suite_to_str(#{key_exchange => ecdhe_ecdsa,
                           cipher => aes_128_gcm,
                           mac => aead,
                           prf => sha256}),
    "TLS_ECDH_RSA_WITH_AES_128_CBC_SHA256" =
        ssl:suite_to_str(#{key_exchange => ecdh_rsa,
                           cipher => aes_128_cbc,
                           mac => sha256,
                           prf => sha256}).

%%--------------------------------------------------------------------

peername() ->
    [{doc,"Test API function peername/1"}].

peername(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, peername_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, peername_result, []}},
					{options, [{port, 0} | ClientOpts]}]),
    
    ClientPort = ssl_test_lib:inet_port(Client),
    ServerIp = ssl_test_lib:node_to_hostip(ServerNode, server),
    ClientIp = ssl_test_lib:node_to_hostip(ClientNode, client),
    ServerMsg = {ok, {ClientIp, ClientPort}},
    ClientMsg = {ok, {ServerIp, Port}},

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
peercert() ->
    [{doc,"Test API function peercert/1"}].
peercert(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, peercert_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, peercert_result, []}},
			   {options, ClientOpts}]),

    CertFile = proplists:get_value(certfile, ServerOpts),
    [{'Certificate', BinCert, _}]= ssl_test_lib:pem_to_der(CertFile),

    ServerMsg = {error, no_peercert},
    ClientMsg = {ok, BinCert},

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

peercert_result(Socket) ->
    ssl:peercert(Socket).
%%--------------------------------------------------------------------

peercert_with_client_cert() ->
    [{doc,"Test API function peercert/1"}].
peercert_with_client_cert(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_dsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_dsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, peercert_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {?MODULE, peercert_result, []}},
			   {options, ClientOpts}]),

    ServerCertFile = proplists:get_value(certfile, ServerOpts),
    [{'Certificate', ServerBinCert, _}]= ssl_test_lib:pem_to_der(ServerCertFile),
     ClientCertFile = proplists:get_value(certfile, ClientOpts),
    [{'Certificate', ClientBinCert, _}]= ssl_test_lib:pem_to_der(ClientCertFile),

    ServerMsg = {ok, ClientBinCert},
    ClientMsg = {ok, ServerBinCert},

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
sockname() ->
    [{doc,"Test API function sockname/1"}].
sockname(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, sockname_result, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, sockname_result, []}},
			   {options, [{port, 0} | ClientOpts]}]),

    ClientPort = ssl_test_lib:inet_port(Client),
    ServerIp =
	case proplists:get_value(protocol, Config) of
	    dtls ->
		%% DTLS sockets are not connected on the server side,
		%% so we can only get a ClientIP, ServerIP will always be 0.0.0.0
		{0,0,0,0};
	    _ ->
		ssl_test_lib:node_to_hostip(ServerNode, server)
	end,

    ClientIp = ssl_test_lib:node_to_hostip(ClientNode, client),
    ServerMsg = {ok, {ServerIp, Port}},
    ClientMsg = {ok, {ClientIp, ClientPort}},
			   
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

sockname_result(S) ->
    ssl:sockname(S).

%%--------------------------------------------------------------------

cipher_suites() ->
    [{doc,"Test API function cipher_suites/2, filter_cipher_suites/2"
      " and prepend|append_cipher_suites/2"}].

cipher_suites(Config) when is_list(Config) -> 
    MandatoryCipherSuiteTLS1_0TLS1_1 = #{key_exchange => rsa,
                                         cipher => '3des_ede_cbc',
                                         mac => sha,
                                         prf => default_prf},
    MandatoryCipherSuiteTLS1_0TLS1_2 = #{key_exchange =>rsa,
                                         cipher => 'aes_128_cbc',
                                         mac => sha,
                                         prf => default_prf}, 
    Version = ssl_test_lib:protocol_version(Config),
    All = [_|_] = ssl:cipher_suites(all, Version),
    Default = [_|_] = ssl:cipher_suites(default, Version),
    Anonymous = [_|_] = ssl:cipher_suites(anonymous, Version),
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
    [Cipher | Rest0] = ssl:prepend_cipher_suites(Filters, Default),
    true = lists:member(Cipher, Default), 
    false = lists:member(Cipher, Rest0), 
    [Cipher | Rest1] = lists:reverse(ssl:append_cipher_suites([Cipher], Default)),
    [Cipher | Rest1] = lists:reverse(ssl:append_cipher_suites(Filters, Default)),
    true = lists:member(Cipher, Default),
    false = lists:member(Cipher, Rest1),
    [] = lists:dropwhile(fun(X) -> not lists:member(X, Default) end, Anonymous),
    [] = lists:dropwhile(fun(X) -> not lists:member(X, All) end, Anonymous),        
    true = lists:member(MandatoryCipherSuiteTLS1_0TLS1_1, All),
    true = lists:member(MandatoryCipherSuiteTLS1_0TLS1_2, All).

%%--------------------------------------------------------------------

old_cipher_suites() ->
    [{doc,"Test API function cipher_suites/0"}].

old_cipher_suites(Config) when is_list(Config) -> 
    MandatoryCipherSuite = {rsa, '3des_ede_cbc', sha},
    [_|_] = Suites = ssl:cipher_suites(),
    Suites = ssl:cipher_suites(erlang),
    [_|_] = ssl:cipher_suites(openssl),
    true = lists:member(MandatoryCipherSuite,  ssl:cipher_suites(all)).

%%--------------------------------------------------------------------
cipher_suites_mix() ->
    [{doc,"Test to have old and new cipher suites at the same time"}].

cipher_suites_mix(Config) when is_list(Config) -> 
    CipherSuites = [{dhe_rsa,aes_128_cbc,sha256,sha256}, {dhe_rsa,aes_128_cbc,sha}],
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{ciphers, CipherSuites} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
tls_socket_options() ->
    [{doc,"Test API function getopts/2 and setopts/2"}].

tls_socket_options(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Values = [{mode, list}, {packet, 0}, {header, 0},
		      {active, true}],    
    %% Shall be the reverse order of Values! 
    Options = [active, header, packet, mode],
    
    NewValues = [{mode, binary}, {active, once}],
    %% Shall be the reverse order of NewValues! 
    NewOptions = [active, mode],
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, tls_socket_options_result, 
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, tls_socket_options_result, 
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    
    {ok, Listen} = ssl:listen(0, ServerOpts),
    {ok,[{mode,list}]} = ssl:getopts(Listen, [mode]),
    ok = ssl:setopts(Listen, [{mode, binary}]),
    {ok,[{mode, binary}]} = ssl:getopts(Listen, [mode]),
    {ok,[{recbuf, _}]} = ssl:getopts(Listen, [recbuf]),
    ssl:close(Listen).

tls_socket_options_result(Socket, Options, DefaultValues, NewOptions, NewValues) ->
    %% Test get/set emulated opts
    {ok, DefaultValues} = ssl:getopts(Socket, Options), 
    ssl:setopts(Socket, NewValues),
    {ok, NewValues} = ssl:getopts(Socket, NewOptions),
    %% Test get/set inet opts
    {ok,[{nodelay,false}]} = ssl:getopts(Socket, [nodelay]),  
    ssl:setopts(Socket, [{nodelay, true}]),
    {ok,[{nodelay, true}]} = ssl:getopts(Socket, [nodelay]),
    {ok, All} = ssl:getopts(Socket, []),
    ct:log("All opts ~p~n", [All]),
    ok.


%%--------------------------------------------------------------------
socket_options() ->
    [{doc,"Test API function getopts/2 and setopts/2"}].

socket_options(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Values = [{mode, list}, {active, true}],    
    %% Shall be the reverse order of Values! 
    Options = [active, mode],
    
    NewValues = [{mode, binary}, {active, once}],
    %% Shall be the reverse order of NewValues! 
    NewOptions = [active, mode],
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, socket_options_result, 
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, socket_options_result, 
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    
    {ok, Listen} = ssl:listen(0, ServerOpts),
    {ok,[{mode,list}]} = ssl:getopts(Listen, [mode]),
    ok = ssl:setopts(Listen, [{mode, binary}]),
    {ok,[{mode, binary}]} = ssl:getopts(Listen, [mode]),
    {ok,[{recbuf, _}]} = ssl:getopts(Listen, [recbuf]),
    ssl:close(Listen).


socket_options_result(Socket, Options, DefaultValues, NewOptions, NewValues) ->
    %% Test get/set emulated opts
    {ok, DefaultValues} = ssl:getopts(Socket, Options), 
    ssl:setopts(Socket, NewValues),
    {ok, NewValues} = ssl:getopts(Socket, NewOptions),
    %% Test get/set inet opts
    {ok,[{reuseaddr, _}]} = ssl:getopts(Socket, [reuseaddr]),  
    {ok, All} = ssl:getopts(Socket, []),
    ct:log("All opts ~p~n", [All]),
    ok.


%%--------------------------------------------------------------------
invalid_inet_get_option() ->
    [{doc,"Test handling of invalid inet options in getopts"}].

invalid_inet_get_option(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
invalid_inet_get_option_not_list() ->
    [{doc,"Test handling of invalid type in getopts"}].

invalid_inet_get_option_not_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option_not_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


get_invalid_inet_option_not_list(Socket) ->
    {error, {options, {socket_options, some_invalid_atom_here}}}
     = ssl:getopts(Socket, some_invalid_atom_here),
     ok.

%%--------------------------------------------------------------------
invalid_inet_get_option_improper_list() ->
    [{doc,"Test handling of invalid type in getopts"}].

invalid_inet_get_option_improper_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option_improper_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


get_invalid_inet_option_improper_list(Socket) ->
    {error, {options, {socket_options, foo,_}}} = ssl:getopts(Socket, [packet | foo]),
    ok.

%%--------------------------------------------------------------------
invalid_inet_set_option() ->
    [{doc,"Test handling of invalid inet options in setopts"}].

invalid_inet_set_option(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

set_invalid_inet_option(Socket) ->
    {error, {options, {socket_options, {packet, foo}}}} = ssl:setopts(Socket, [{packet, foo}]),
    {error, {options, {socket_options, {header, foo}}}} = ssl:setopts(Socket, [{header, foo}]),
    {error, {options, {socket_options, {active, foo}}}} = ssl:setopts(Socket, [{active, foo}]),
    {error, {options, {socket_options, {mode, foo}}}}   = ssl:setopts(Socket, [{mode, foo}]),
    ok.
%%--------------------------------------------------------------------
invalid_inet_set_option_not_list() ->
    [{doc,"Test handling of invalid type in setopts"}].

invalid_inet_set_option_not_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option_not_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


set_invalid_inet_option_not_list(Socket) ->
    {error, {options, {not_a_proplist, some_invalid_atom_here}}}
	= ssl:setopts(Socket, some_invalid_atom_here),
    ok.

%%--------------------------------------------------------------------
invalid_inet_set_option_improper_list() ->
    [{doc,"Test handling of invalid tye in setopts"}].

invalid_inet_set_option_improper_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option_improper_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

set_invalid_inet_option_improper_list(Socket) ->
    {error, {options, {not_a_proplist, [{packet, 0} | {foo, 2}]}}} =
	ssl:setopts(Socket, [{packet, 0} | {foo, 2}]),
    ok.

%%--------------------------------------------------------------------
tls_misc_ssl_options() ->
    [{doc,"Test what happens when we give valid options"}].

tls_misc_ssl_options(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    %% Check that ssl options not tested elsewhere are filtered away e.i. not passed to inet.
    TestOpts = [{depth, 1}, 
		{key, undefined}, 
		{password, []},
		{reuse_session, fun(_,_,_,_) -> true end},
		{cb_info, {gen_tcp, tcp, tcp_closed, tcp_error}}],
    
   Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options,  TestOpts ++ ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, TestOpts ++ ClientOpts}]),

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
ssl_options_not_proplist() ->
    [{doc,"Test what happens if an option is not a key value tuple"}].

ssl_options_not_proplist(Config) when is_list(Config) ->
    BadOption =  {client_preferred_next_protocols, 
		  client, [<<"spdy/3">>,<<"http/1.1">>], <<"http/1.1">>},
    {option_not_a_key_value_tuple, BadOption} =
	ssl:connect("twitter.com", 443, [binary, {active, false}, 
					 BadOption]).

%%--------------------------------------------------------------------
raw_ssl_option() ->
    [{doc,"Ensure that a single 'raw' option is passed to ssl:listen correctly."}].

raw_ssl_option(Config) when is_list(Config) ->
    % 'raw' option values are platform-specific; these are the Linux values:
    IpProtoTcp = 6,
    % Use TCP_KEEPIDLE, because (e.g.) TCP_MAXSEG can't be read back reliably.
    TcpKeepIdle = 4,
    KeepAliveTimeSecs = 55,
    LOptions = [{raw, IpProtoTcp, TcpKeepIdle, <<KeepAliveTimeSecs:32/native>>}],
    {ok, LSocket} = ssl:listen(0, LOptions),
    % Per http://www.erlang.org/doc/man/inet.html#getopts-2, we have to specify
    % exactly which raw option we want, and the size of the buffer.
    {ok, [{raw, IpProtoTcp, TcpKeepIdle, <<KeepAliveTimeSecs:32/native>>}]} = ssl:getopts(LSocket, [{raw, IpProtoTcp, TcpKeepIdle, 4}]).


%%--------------------------------------------------------------------
versions() ->
    [{doc,"Test API function versions/0"}].

versions(Config) when is_list(Config) -> 
    [_|_] = Versions = ssl:versions(),
    ct:log("~p~n", [Versions]).


%%--------------------------------------------------------------------
eccs() ->
    [{doc, "Test API functions eccs/0 and eccs/1"}].

eccs(Config) when is_list(Config) ->
    [_|_] = All = ssl:eccs(),
    [] = SSL3 = ssl:eccs(sslv3),
    [_|_] = Tls = ssl:eccs(tlsv1),
    [_|_] = Tls1 = ssl:eccs('tlsv1.1'),
    [_|_] = Tls2 = ssl:eccs('tlsv1.2'),
    [_|_] = Tls1 = ssl:eccs('dtlsv1'),
    [_|_] = Tls2 = ssl:eccs('dtlsv1.2'),
    %% ordering is currently unverified by the test
    true = lists:sort(All) =:= lists:usort(SSL3 ++ Tls ++ Tls1 ++ Tls2),
    ok.

%%--------------------------------------------------------------------
send_recv() ->
    [{doc,""}].
send_recv(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false} | ClientOpts]}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_send_close() ->
    [{doc,""}].
tls_send_close(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, TcpS} = rpc:call(ClientNode, gen_tcp, connect, 
			  [Hostname,Port,[binary, {active, false}]]),
    {ok, SslS} = rpc:call(ClientNode, ssl, connect, 
			  [TcpS,[{active, false}|ClientOpts]]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), self(), Server]),
    ok = ssl:send(SslS, "Hello world"),      
    {ok,<<"Hello world">>} = ssl:recv(SslS, 11),    
    gen_tcp:close(TcpS),    
    {error, _} = ssl:send(SslS, "Hello world").

%%--------------------------------------------------------------------
version_option() ->
    [{doc, "Use version option and do no specify ciphers list. Bug specified incorrect ciphers"}].
version_option(Config) when is_list(Config) ->
    Versions = proplists:get_value(supported, ssl:versions()),
    [version_option_test(Config, Version) || Version <- Versions].
   
%%--------------------------------------------------------------------
close_transport_accept() ->
    [{doc,"Tests closing ssl socket when waiting on ssl:transport_accept/1"}].

close_transport_accept(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
recv_active() ->
    [{doc,"Test recv on active socket"}].

recv_active(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
%% Test case adapted from gen_tcp_misc_SUITE.
active_n() ->
    [{doc,"Test {active,N} option"}].

active_n(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    Port = ssl_test_lib:inet_port(node()),
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
    C = ok(ssl:connect("localhost", Port, [{active,N}|ClientOpts])),
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

active_n_common(S, N) ->
    ok = ssl:setopts(S, [{active,-N}]),
    receive
        {ssl_passive, S} -> ok
    after
        1000 ->
            error({error,ssl_passive_failure})
    end,
    [{active,false}] = ok(ssl:getopts(S, [active])),
    ok = ssl:setopts(S, [{active,0}]),
    receive
        {ssl_passive, S} -> ok
    after
        1000 ->
            error({error,ssl_passive_failure})
    end,
    ok = ssl:setopts(S, [{active,32767}]),
    {error,{options,_}} = ssl:setopts(S, [{active,1}]),
    {error,{options,_}} = ssl:setopts(S, [{active,-32769}]),
    ok = ssl:setopts(S, [{active,-32768}]),
    receive
        {ssl_passive, S} -> ok
    after
        1000 ->
            error({error,ssl_passive_failure})
    end,
    [{active,false}] = ok(ssl:getopts(S, [active])),
    ok = ssl:setopts(S, [{active,N}]),
    ok = ssl:setopts(S, [{active,true}]),
    [{active,true}] = ok(ssl:getopts(S, [active])),
    receive
        _ -> error({error,active_n})
    after
        0 ->
            ok
    end,
    ok = ssl:setopts(S, [{active,N}]),
    ok = ssl:setopts(S, [{active,once}]),
    [{active,once}] = ok(ssl:getopts(S, [active])),
    receive
        _ -> error({error,active_n})
    after
        0 ->
            ok
    end,
    {error,{options,_}} = ssl:setopts(S, [{active,32768}]),
    ok = ssl:setopts(S, [{active,false}]),
    [{active,false}] = ok(ssl:getopts(S, [active])),
    ok.

ok({ok,V}) -> V.

repeat(N, Fun) ->
    repeat(N, N, Fun).

repeat(N, T, Fun) when is_integer(N), N > 0 ->
    Fun(T-N),
    repeat(N-1, T, Fun);
repeat(_, _, _) ->
    ok.

%%--------------------------------------------------------------------
dh_params() ->
    [{doc,"Test to specify DH-params file in server."}].

dh_params(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    DataDir = proplists:get_value(data_dir, Config),
    DHParamFile = filename:join(DataDir, "dHParam.pem"),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {ssl_test_lib, send_recv_result_active, []}},
			   {options, [{dhfile, DHParamFile} | ServerOpts]}]),
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
tls_upgrade() ->
    [{doc,"Test that you can upgrade an tcp connection to an ssl connection"}].

tls_upgrade(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0}, 
						{from, self()}, 
						{mfa, {?MODULE, 
						       upgrade_result, []}},
						{tcp_options, 
						 [{active, false} | TcpOpts]},
						{ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client([{node, ClientNode}, 
						{port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {?MODULE, upgrade_result, []}},
				   {tcp_options, [binary]},
				   {ssl_options, ClientOpts}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

upgrade_result(Socket) ->
    ssl:setopts(Socket, [{active, true}]),
    ok = ssl:send(Socket, "Hello world"),
    %% Make sure binary is inherited from tcp socket and that we do
    %% not get the list default!
    receive 
	{ssl, _, <<"H">>} ->
	    receive 
		{ssl, _, <<"ello world">>} ->
		    ok
	    end;
	{ssl, _, <<"Hello world">>}  ->
	    ok
    end.


%%--------------------------------------------------------------------
internal_active_1() ->
    [{doc,"Test internal active 1 (behave as internal active once)"}].

internal_active_1(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options,  [{active, true} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, [{active, true} | ClientOpts]}]),
        
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_upgrade_with_timeout() ->
    [{doc,"Test ssl_accept/3"}].

tls_upgrade_with_timeout(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Server = ssl_test_lib:start_upgrade_server([{node, ServerNode}, {port, 0}, 
						{from, self()}, 
						{timeout, 5000},
						{mfa, {?MODULE, 
						       upgrade_result, []}},
						{tcp_options, 
						 [{active, false} | TcpOpts]},
						{ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_upgrade_client([{node, ClientNode}, 
						{port, Port}, 
						{host, Hostname},
						{from, self()}, 
						{mfa, {?MODULE, upgrade_result, []}},
						{tcp_options, TcpOpts},
						{ssl_options, ClientOpts}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
		       [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_downgrade() ->
      [{doc,"Test that you can downgarde an ssl connection to an tcp connection"}].
tls_downgrade(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, tls_downgrade_result, [self()]}},
					{options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, tls_downgrade_result, [self()]}},
					{options, [{active, false} |ClientOpts]}]),

                                                   
    ssl_test_lib:check_result(Server, ready, Client, ready),

    Server ! go,
    Client ! go,

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
close_with_timeout() ->
      [{doc,"Test normal (not downgrade) ssl:close/2"}].
close_with_timeout(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    
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
tls_tcp_connect() ->
    [{doc,"Test what happens when a tcp tries to connect, i,e. a bad (ssl) packet is sent first"}].

tls_tcp_connect(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}, {active, false}],

    Server = ssl_test_lib:start_upgrade_server_error([{node, ServerNode}, {port, 0},
						      {from, self()},
						      {timeout, 5000},
						      {mfa, {?MODULE, dummy, []}},
						      {tcp_options, TcpOpts},
						      {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    ct:log("Testcase ~p connected to Server ~p ~n", [self(), Server]),
    gen_tcp:send(Socket, "<SOME GARBLED NON SSL MESSAGE>"),

    receive 
	{tcp_closed, Socket} ->
	    receive 
		{Server, {error, Error}} ->
		    ct:log("Error ~p", [Error])
	    end
    end.
%%--------------------------------------------------------------------
tls_tcp_connect_big() ->
    [{doc,"Test what happens when a tcp tries to connect, i,e. a bad big (ssl) packet is sent first"}].

tls_tcp_connect_big(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    TcpOpts = [binary, {reuseaddr, true}],

    Rand = crypto:strong_rand_bytes(?MAX_CIPHER_TEXT_LENGTH+1),
    Server = ssl_test_lib:start_upgrade_server_error([{node, ServerNode}, {port, 0},
						      {from, self()},
						      {timeout, 5000},
						      {mfa, {?MODULE, dummy, []}},
						      {tcp_options, TcpOpts},
						      {ssl_options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {packet, 0}]),
    ct:log("Testcase ~p connected to Server ~p ~n", [self(), Server]),

    gen_tcp:send(Socket, <<?BYTE(0),
			   ?BYTE(3), ?BYTE(1), ?UINT16(?MAX_CIPHER_TEXT_LENGTH), Rand/binary>>),

    receive
	{tcp_closed, Socket} ->
	    receive
		{Server, {error, timeout}} ->
		    ct:fail("hangs");
		{Server, {error, Error}} ->
		    ct:log("Error ~p", [Error]);
		{'EXIT', Server, _} ->
		    ok	 
	    end
    end.

%%--------------------------------------------------------------------
ipv6() ->
    [{require, ipv6_hosts},
     {doc,"Test ipv6."}].
ipv6(Config) when is_list(Config) ->
    {ok, Hostname0} = inet:gethostname(),
    
    case lists:member(list_to_atom(Hostname0), ct:get_config(ipv6_hosts)) of
	true ->
	    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
	    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
	    
	    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			       [self(), Client, Server]),
	    
	    ssl_test_lib:check_result(Server, ok, Client, ok),
	    
	    ssl_test_lib:close(Server),
	    ssl_test_lib:close(Client);
	false ->
	    {skip, "Host does not support IPv6"}
    end.

%%--------------------------------------------------------------------

invalid_keyfile() ->
    [{doc,"Test what happens with an invalid key file"}].
invalid_keyfile(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    BadOpts = ssl_test_lib:ssl_options(server_bad_key, Config),
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
    ssl_test_lib:check_result(Server, {error,{options, {keyfile, File, {error,enoent}}}}, Client,  
				       {error, closed}).

%%--------------------------------------------------------------------

invalid_certfile() ->
    [{doc,"Test what happens with an invalid cert file"}].

invalid_certfile(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerBadOpts = ssl_test_lib:ssl_options(server_bad_cert, Config),
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
    ClientOpts    = [{reuseaddr, true}|ssl_test_lib:ssl_options(client_opts, Config)],
    ServerBadOpts = [{reuseaddr, true}|ssl_test_lib:ssl_options(server_bad_ca, Config)],
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
invalid_options() ->
    [{doc,"Test what happens when we give invalid options"}].
       
invalid_options(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Check = fun(Client, Server, {versions, [sslv2, sslv3]} = Option) ->
		    ssl_test_lib:check_result(Server, 
					      {error, {options, {sslv2, Option}}}, 
					      Client,
					      {error, {options, {sslv2, Option}}});
	       (Client, Server, Option) ->
		    ssl_test_lib:check_result(Server, 
					      {error, {options, Option}}, 
					      Client,
					      {error, {options, Option}})
	    end,

    TestOpts = 
         [{versions, [sslv2, sslv3]}, 
          {verify, 4}, 
          {verify_fun, function},
          {fail_if_no_peer_cert, 0}, 
          {verify_client_once, 1},
          {depth, four}, 
          {certfile, 'cert.pem'}, 
          {keyfile,'key.pem' }, 
          {password, foo},
          {cacertfile, ""}, 
          {dhfile,'dh.pem' },
          {ciphers, [{foo, bar, sha, ignore}]},
          {reuse_session, foo},
          {reuse_sessions, 0},
          {renegotiate_at, "10"},
          {mode, depech},
          {packet, 8.0},
          {packet_size, "2"},
          {header, a},
          {active, trice},
          {key, 'key.pem' }],

    [begin
	 Server =
	     ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					{from, self()},
					{options, [TestOpt | ServerOpts]}]),
	 %% Will never reach a point where port is used.
	 Client =
	     ssl_test_lib:start_client_error([{node, ClientNode}, {port, 0},
					      {host, Hostname}, {from, self()},
					      {options, [TestOpt | ClientOpts]}]),
	 Check(Client, Server, TestOpt),
	 ok
     end || TestOpt <- TestOpts],
    ok.

%%--------------------------------------------------------------------
tls_shutdown() ->
    [{doc,"Test API function ssl:shutdown/2"}].
tls_shutdown(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, tls_shutdown_result, [server]}},
			   {options, [{exit_on_close, false},
				      {active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, tls_shutdown_result, [client]}},
					{options, 
					 [{exit_on_close, false},
					  {active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_shutdown_write() ->
    [{doc,"Test API function ssl:shutdown/2 with option write."}].
tls_shutdown_write(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, tls_shutdown_write_result, [server]}},
			   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, tls_shutdown_write_result, [client]}},
			   {options, [{active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, {error, closed}).

%%--------------------------------------------------------------------
tls_shutdown_both() ->
    [{doc,"Test API function ssl:shutdown/2 with option both."}].
tls_shutdown_both(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, tls_shutdown_both_result, [server]}},
			   {options, [{active, false} | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, tls_shutdown_both_result, [client]}},
			   {options, [{active, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, {error, closed}).

%%--------------------------------------------------------------------
tls_shutdown_error() ->
    [{doc,"Test ssl:shutdown/2 error handling"}].
tls_shutdown_error(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    Port = ssl_test_lib:inet_port(node()),
    {ok, Listen} = ssl:listen(Port, ServerOpts),
    {error, enotconn} = ssl:shutdown(Listen, read_write),
    ok = ssl:close(Listen),
    {error, closed} = ssl:shutdown(Listen, read_write).

%%--------------------------------------------------------------------
default_reject_anonymous()->
    [{doc,"Test that by default anonymous cipher suites are rejected "}].
default_reject_anonymous(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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

%%--------------------------------------------------------------------
reuse_session() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
reuse_session(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    
    ssl_test_lib:reuse_session(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
reuse_session_expired() ->
    [{doc,"Test sessions is not reused when it has expired"}].
reuse_session_expired(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server0 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {tcp_options, [{active, false}]},
				   {options, ServerOpts}]),
    Port0 = ssl_test_lib:inet_port(Server0),
    
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, [{reuse_sessions, save} | ClientOpts]}]),
    Server0 ! listen,
    
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port0}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, ClientOpts}]),    
    
    SID = receive
              {Client0, Id0} ->
                  Id0
          end,
       
    receive
        {Client1, SID} ->
            ok
    after ?SLEEP ->
              ct:fail(session_not_reused)
    end,
    
    Server0 ! listen,
    
    %% Make sure session is unregistered due to expiration
    ct:sleep((?EXPIRE*2)),

    make_sure_expired(Hostname, Port0, SID),
    
    Client2 =
	ssl_test_lib:start_client([{node, ClientNode}, 
				   {port, Port0}, {host, Hostname},
				   {mfa, {ssl_test_lib, session_id, []}},
				   {from, self()}, {options, ClientOpts}]),   
    receive
	{Client2, SID} ->
	    ct:fail(session_reused_when_session_expired);
	{Client2, _} ->
	    ok
    end,
    process_flag(trap_exit, false),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Client2).

make_sure_expired(Host, Port, Id) ->
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    ClientCache = element(2, State),

    case ssl_session_cache:lookup(ClientCache, {{Host,  Port}, Id}) of
	undefined ->
   	   ok; 
	#session{is_resumable = false} ->
   	   ok;
	_ ->
	    ct:sleep(?SLEEP),
            make_sure_expired(Host, Port, Id)
    end.     

%%--------------------------------------------------------------------
server_does_not_want_to_reuse_session() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
server_does_not_want_to_reuse_session(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()},
		      {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, [{reuse_session, fun(_,_,_,_) ->
								      false
							      end} | 
					      ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),   
    SessionInfo = 
	receive
	    {Server, Info} ->
		Info
	end,
       
    Server ! {listen, {mfa, {ssl_test_lib, no_result, []}}},
    
    %% Make sure session is registered
    ct:sleep(?SLEEP),
    ssl_test_lib:close(Client0),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, 
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {from, self()},  {options, ClientOpts}]),    
    receive
	{Client1, SessionInfo} ->
	    ct:fail(session_reused_when_server_does_not_want_to);
	{Client1, _Other} ->
	   ok
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
client_renegotiate() ->
    [{doc,"Test ssl:renegotiate/1 on client."}].
client_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_secure_renegotiate() ->
    [{doc,"Test ssl:renegotiate/1 on client."}].
client_secure_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, [{secure_renegotiate, true} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, [{reuse_sessions, false},
						   {secure_renegotiate, true}| ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_secure_renegotiate_fallback() ->
    [{doc,"Test that we can set secure_renegotiate to false that is "
      "fallback option, we however do not have a insecure server to test against!"}].
client_secure_renegotiate_fallback(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, [{secure_renegotiate, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, [{reuse_sessions, false},
						   {secure_renegotiate, false}| ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
server_renegotiate() ->
    [{doc,"Test ssl:renegotiate/1 on server."}].
server_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, erlang_ssl_receive, [Data]}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_renegotiate_reused_session() ->
    [{doc,"Test ssl:renegotiate/1 on client when the ssl session will be reused."}].
client_renegotiate_reused_session(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_reuse_session, [Data]}},
					{options, [{reuse_sessions, true} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
server_renegotiate_reused_session() ->
    [{doc,"Test ssl:renegotiate/1 on server when the ssl session will be reused."}].
server_renegotiate_reused_session(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_reuse_session, [Data]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, erlang_ssl_receive, [Data]}},
					{options, [{reuse_sessions, true} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
client_no_wrap_sequence_number() ->
    [{doc,"Test that erlang client will renegotiate session when",
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at"
     " to lower treashold substantially."}].

client_no_wrap_sequence_number(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    ErlData = "From erlang to erlang",
    N = 12,

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Version = ssl_test_lib:protocol_version(Config, tuple),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib,
					       trigger_renegotiate, [[ErlData, treashold(N, Version)]]}},
					{options, [{reuse_sessions, false},
						   {renegotiate_at, N} | ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
server_no_wrap_sequence_number() ->
    [{doc, "Test that erlang server will renegotiate session when",
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at"
     " to lower treashold substantially."}].

server_no_wrap_sequence_number(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",
    N = 12,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib,
					       trigger_renegotiate, [[Data, N+2]]}},
					{options, [{renegotiate_at, N} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, no_result, []}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
der_input() ->
    [{doc,"Test to input certs and key as der"}].

der_input(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    DHParamFile = filename:join(DataDir, "dHParam.pem"),

    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    [CADb | _] = element(6, State),

    Size = ets:info(CADb, size),

    SeverVerifyOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ServerCert, ServerKey, ServerCaCerts, DHParams} = der_input_opts([{dhfile, DHParamFile} |
								       SeverVerifyOpts]),
    ClientVerifyOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    {ClientCert, ClientKey, ClientCaCerts, DHParams} = der_input_opts([{dhfile, DHParamFile} |
								       ClientVerifyOpts]),
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true},
		  {dh, DHParams},
		  {cert, ServerCert}, {key, ServerKey}, {cacerts, ServerCaCerts}],
    ClientOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true},
		  {dh, DHParams},
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
    Size = ets:info(CADb, size).

%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
no_reuses_session_server_restart_new_cert() ->
    [{doc,"Check that a session is not reused if the server is restarted with a new cert."}].
no_reuses_session_server_restart_new_cert(Config) when is_list(Config) ->

    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    DsaServerOpts = ssl_test_lib:ssl_options(server_dsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
		      {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),
    SessionInfo =
	receive
	    {Server, Info} ->
		Info
	end,

    %% Make sure session is registered
    ct:sleep(?SLEEP),
    Monitor = erlang:monitor(process, Server),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),
    receive
	{'DOWN', Monitor, _, _, _} ->
	    ok
    end,
    
    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
		      {mfa, {ssl_test_lib, no_result, []}},
				   {options, [{reuseaddr, true} | DsaServerOpts]}]),

    Client1 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
		      {from, self()},  {options, ClientOpts}]),
    receive
	{Client1, SessionInfo} ->
	    ct:fail(session_reused_when_server_has_new_cert);
	{Client1, _Other} ->
	   ok
    end,
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
no_reuses_session_server_restart_new_cert_file() ->
    [{doc,"Check that a session is not reused if a server is restarted with a new "
      "cert contained in a file with the same name as the old cert."}].

no_reuses_session_server_restart_new_cert_file(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_verification_opts, Config),
    DsaServerOpts = ssl_test_lib:ssl_options(server_dsa_opts, Config),
    PrivDir =  proplists:get_value(priv_dir, Config),

    NewServerOpts0 = new_config(PrivDir, ServerOpts),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
		      {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, NewServerOpts0}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
			    {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),
    SessionInfo =
	receive
	    {Server, Info} ->
		Info
	end,

    %% Make sure session is registered and we get
    %% new file time stamp when calling new_config!
    ct:sleep(?SLEEP* 2),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),

    ssl:clear_pem_cache(),

    NewServerOpts1 = new_config(PrivDir, DsaServerOpts),

    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
		      {mfa, {ssl_test_lib, no_result, []}},
				   {options,  [{reuseaddr, true} | NewServerOpts1]}]),
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
		      {mfa, {ssl_test_lib, session_info_result, []}},
				   {from, self()},  {options, ClientOpts}]),
    receive
	{Client1, SessionInfo} ->
	    ct:fail(session_reused_when_server_has_new_cert);
	{Client1, _Other} ->
	   ok
    end,
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
defaults(Config) when is_list(Config)->
    Versions = ssl:versions(),
    true = lists:member(sslv3, proplists:get_value(available, Versions)),
    false = lists:member(sslv3,  proplists:get_value(supported, Versions)),
    true = lists:member('tlsv1', proplists:get_value(available, Versions)),
    false = lists:member('tlsv1',  proplists:get_value(supported, Versions)),
    true = lists:member('tlsv1.1', proplists:get_value(available, Versions)),
    false = lists:member('tlsv1.1',  proplists:get_value(supported, Versions)),
    true = lists:member('tlsv1.2', proplists:get_value(available, Versions)),
    true = lists:member('tlsv1.2',  proplists:get_value(supported, Versions)),    
    false = lists:member({rsa,rc4_128,sha}, ssl:cipher_suites()),
    true = lists:member({rsa,rc4_128,sha}, ssl:cipher_suites(all)),
    false = lists:member({rsa,des_cbc,sha}, ssl:cipher_suites()),
    true = lists:member({rsa,des_cbc,sha}, ssl:cipher_suites(all)),
    false = lists:member({dhe_rsa,des_cbc,sha}, ssl:cipher_suites()),
    true = lists:member({dhe_rsa,des_cbc,sha}, ssl:cipher_suites(all)),
    true = lists:member('dtlsv1.2', proplists:get_value(available_dtls, Versions)),
    true = lists:member('dtlsv1', proplists:get_value(available_dtls, Versions)),
    true = lists:member('dtlsv1.2', proplists:get_value(supported_dtls, Versions)),
    false = lists:member('dtlsv1', proplists:get_value(supported_dtls, Versions)).

%%--------------------------------------------------------------------
reuseaddr() ->
    [{doc,"Test reuseaddr option"}].

reuseaddr(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, [{active, false} | ClientOpts]}]),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    
    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false} | ServerOpts]}]),
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server1, ok, Client1, ok),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
tls_tcp_reuseaddr() ->
    [{doc, "Reference test case."}].
tls_tcp_reuseaddr(Config) when is_list(Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {transport, gen_tcp},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options,  [{active, false}, {reuseaddr, true}]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
                                   {transport, gen_tcp},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, [{active, false}]}]),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    
    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
				   {from, self()},
				   {transport, gen_tcp},
				   {mfa, {?MODULE, tcp_send_recv_result, []}},
				   {options,  [{active, false}, {reuseaddr, true}]}]),
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
                                   {transport, gen_tcp}, 
				   {mfa, {?MODULE, tcp_send_recv_result, []}},
				   {options, [{active, false}]}]),

    ssl_test_lib:check_result(Server1, ok, Client1, ok),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1).

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
                       mac =>sha,
                       prf => default_prf},
                     #{key_exchange => dhe_rsa, 
                       cipher => aes_128_cbc, 
                       mac => sha,
                       prf => default_prf}],
    honor_cipher_order(Config, true, ServerCiphers, ClientCiphers, #{key_exchange => dhe_rsa, 
                                                                     cipher => aes_256_cbc, 
                                                                     mac => sha,
                                                                     prf => default_prf}).

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
                       mac =>sha,
                       prf => default_prf},
                     #{key_exchange => dhe_rsa, 
                       cipher => aes_128_cbc, 
                       mac => sha,
                       prf => default_prf}],
honor_cipher_order(Config, false, ServerCiphers, ClientCiphers, #{key_exchange => dhe_rsa, 
                                                                  cipher => aes_128_cbc, 
                                                                  mac => sha,
                                                                  prf => default_prf}).

honor_cipher_order(Config, Honor, ServerCiphers, ClientCiphers, Expected) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

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
					{options, [{ciphers, ClientCiphers}, {honor_cipher_order, Honor}
						   | ClientOpts]}]),

    Version = ssl_test_lib:protocol_version(Config),

    ServerMsg = ClientMsg = {ok, {Version, Expected}},

    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_ciphersuite_vs_version()  ->
    [{doc,"Test a SSLv3 client cannot negotiate a TLSv* cipher suite."}].
tls_ciphersuite_vs_version(Config) when is_list(Config) ->
    
    {_ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {active, false}]),
    ok = gen_tcp:send(Socket, 
		      <<22, 3,0, 49:16, % handshake, SSL 3.0, length
			1, 45:24, % client_hello, length
			3,0, % SSL 3.0
			16#deadbeef:256, % 32 'random' bytes = 256 bits
			0, % no session ID
			%% three cipher suites -- null, one with sha256 hash and one with sha hash
			6:16, 0,255, 0,61, 0,57, 
			1, 0 % no compression
		      >>),
    {ok, <<22, RecMajor:8, RecMinor:8, _RecLen:16, 2, HelloLen:24>>} = gen_tcp:recv(Socket, 9, 10000),
    {ok, <<HelloBin:HelloLen/binary>>} = gen_tcp:recv(Socket, HelloLen, 5000),
    ServerHello = tls_handshake:decode_handshake({RecMajor, RecMinor}, 2, HelloBin),
    case ServerHello of
	#server_hello{server_version = {3,0}, cipher_suite = <<0,57>>} -> 
	    ok;
	_ ->
	    ct:fail({unexpected_server_hello, ServerHello})
    end.
			
%%--------------------------------------------------------------------
conf_signature_algs() ->
    [{doc,"Test to set the signature_algs option on both client and server"}].
conf_signature_algs(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false}, {signature_algs, [{sha, rsa}]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false}, {signature_algs, [{sha, rsa}]} | ClientOpts]}]),
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
no_common_signature_algs()  ->
    [{doc,"Set the signature_algs option so that there client and server does not share any hash sign algorithms"}].
no_common_signature_algs(Config) when is_list(Config) ->
    
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

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

tls_dont_crash_on_handshake_garbage() ->
    [{doc, "Ensure SSL server worker thows an alert on garbage during handshake "
      "instead of crashing and exposing state to user code"}].

tls_dont_crash_on_handshake_garbage(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

    {_ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    unlink(Server), monitor(process, Server),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [binary, {active, false}]),

    % Send hello and garbage record
    ok = gen_tcp:send(Socket,
                      [<<22, 3,3, 49:16, 1, 45:24, 3,3, % client_hello
                         16#deadbeef:256, % 32 'random' bytes = 256 bits
                         0, 6:16, 0,255, 0,61, 0,57, 1, 0 >>, % some hello values

                       <<22, 3,3, 5:16, 92,64,37,228,209>> % garbage
                      ]),
    % Send unexpected change_cipher_spec
    ok = gen_tcp:send(Socket, <<20, 3,3, 12:16, 111,40,244,7,137,224,16,109,197,110,249,152>>),

    % Ensure we receive an alert, not sudden disconnect
    {ok, <<21, _/binary>>} = drop_handshakes(Socket, 1000).

drop_handshakes(Socket, Timeout) ->
    {ok, <<RecType:8, _RecMajor:8, _RecMinor:8, RecLen:16>> = Header} = gen_tcp:recv(Socket, 5, Timeout),
    {ok, <<Frag:RecLen/binary>>} = gen_tcp:recv(Socket, RecLen, Timeout),
    case RecType of
        22 -> drop_handshakes(Socket, Timeout);
        _ -> {ok, <<Header/binary, Frag/binary>>}
    end.


%%--------------------------------------------------------------------

hibernate() ->
    [{doc,"Check that an SSL connection that is started with option "
      "{hibernate_after, 1000} indeed hibernates after 1000ms of "
      "inactivity"}].

hibernate(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, #sslsocket{pid=[Pid|_]}} = ssl_test_lib:start_client([return_socket,
                    {node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{hibernate_after, 1000}|ClientOpts]}]),
    {current_function, _} =
        process_info(Pid, current_function),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    timer:sleep(1500),
    {current_function, {erlang, hibernate, 3}} =
	process_info(Pid, current_function),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

hibernate_right_away() ->
    [{doc,"Check that an SSL connection that is configured to hibernate "
    "after 0 or 1 milliseconds hibernates as soon as possible and not "
    "crashes"}].

hibernate_right_away(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    StartServerOpts = [{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {ssl_test_lib, send_recv_result_active, []}},
                    {options, ServerOpts}],
    StartClientOpts = [return_socket,
                    {node, ClientNode},
                    {host, Hostname},
                    {from, self()},
                    {mfa, {ssl_test_lib, send_recv_result_active, []}}],

    Server1 = ssl_test_lib:start_server(StartServerOpts),
    Port1 = ssl_test_lib:inet_port(Server1),
    {Client1, #sslsocket{pid = [Pid1|_]}} = ssl_test_lib:start_client(StartClientOpts ++
                    [{port, Port1}, {options, [{hibernate_after, 0}|ClientOpts]}]),

    ssl_test_lib:check_result(Server1, ok, Client1, ok),
  
     {current_function, {erlang, hibernate, 3}} =
	process_info(Pid1, current_function),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1),
    
    Server2 = ssl_test_lib:start_server(StartServerOpts),
    Port2 = ssl_test_lib:inet_port(Server2),
    {Client2, #sslsocket{pid = [Pid2|_]}} = ssl_test_lib:start_client(StartClientOpts ++
                    [{port, Port2}, {options, [{hibernate_after, 1}|ClientOpts]}]),

    ssl_test_lib:check_result(Server2, ok, Client2, ok),

    ct:sleep(1000), %% Schedule out
    
    {current_function, {erlang, hibernate, 3}} =
	process_info(Pid2, current_function),

    ssl_test_lib:close(Server2),
    ssl_test_lib:close(Client2).

%%--------------------------------------------------------------------
listen_socket() ->
    [{doc,"Check error handling and inet compliance when calling API functions with listen sockets."}].

listen_socket(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ok, ListenSocket} = ssl:listen(0, ServerOpts),

    %% This can be a valid thing to do as
    %% options are inherited by the accept socket
    ok = ssl:controlling_process(ListenSocket, self()),

    {ok, _} = ssl:sockname(ListenSocket),

    {error, enotconn} = ssl:send(ListenSocket, <<"data">>),
    {error, enotconn} = ssl:recv(ListenSocket, 0),
    {error, enotconn} = ssl:connection_information(ListenSocket),
    {error, enotconn} = ssl:peername(ListenSocket),
    {error, enotconn} = ssl:peercert(ListenSocket),
    {error, enotconn} = ssl:renegotiate(ListenSocket),
    {error, enotconn} = ssl:prf(ListenSocket, 'master_secret', <<"Label">>, [client_random], 256),
    {error, enotconn} = ssl:shutdown(ListenSocket, read_write),

    ok = ssl:close(ListenSocket).
%%--------------------------------------------------------------------
tls_ssl_accept_timeout() ->
    [{doc,"Test ssl:ssl_accept timeout"}].

tls_ssl_accept_timeout(Config) ->
    process_flag(trap_exit, true),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{timeout, 5000},
					{mfa, {ssl_test_lib,
					       no_result_msg, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {ok, CSocket} = gen_tcp:connect(Hostname, Port, [binary, {active, true}]),

    receive
	{tcp_closed, CSocket} ->
	    ssl_test_lib:check_result(Server, {error, timeout}),
	    receive
		{'EXIT', Server, _} ->
		    %% Make sure supervisor had time to react on process exit
		    %% Could we come up with a better solution to this?
		    ct:sleep(500), 
		    [] = supervisor:which_children(tls_connection_sup)
	    end
    end.

%%--------------------------------------------------------------------
ssl_recv_timeout() ->
    [{doc,"Test ssl:ssl_accept timeout"}].

ssl_recv_timeout(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

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
connect_twice() ->
    [{doc,""}].
connect_twice(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

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

    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
			 [self(), Client, Server]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:check_result(Server, ok, Client1, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
renegotiate_dos_mitigate_active() ->
    [{doc, "Mitigate DOS computational attack by not allowing client to renegotiate many times in a row",
      "immediately after each other"}].
renegotiate_dos_mitigate_active(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_immediately, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
renegotiate_dos_mitigate_passive() ->
    [{doc, "Mitigate DOS computational attack by not allowing client to renegotiate many times in a row",
      "immediately after each other"}].
renegotiate_dos_mitigate_passive(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
 
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       renegotiate_immediately, []}},
					{options, ClientOpts}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok), 
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
renegotiate_dos_mitigate_absolute() ->
    [{doc, "Mitigate DOS computational attack by not allowing client to initiate renegotiation"}].
renegotiate_dos_mitigate_absolute(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, [{client_renegotiation, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_rejected,
					       []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
tls_tcp_error_propagation_in_active_mode() ->
    [{doc,"Test that process recives {ssl_error, Socket, closed} when tcp error ocurres"}].
tls_tcp_error_propagation_in_active_mode(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					 {from, self()},
					 {mfa, {ssl_test_lib, no_result, []}},
					 {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    {Client, #sslsocket{pid=[Pid|_]} = SslSocket} = ssl_test_lib:start_client([return_socket,
                                                                               {node, ClientNode}, {port, Port},
                                                                               {host, Hostname},
                                                                               {from, self()},
                                                                               {mfa, {?MODULE, receive_msg, []}},
                                                                               {options, ClientOpts}]),
    
    {status, _, _, StatusInfo} = sys:get_status(Pid),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    StaticEnv = element(2, State),
    Socket = element(11, StaticEnv),
    %% Fake tcp error
    Pid ! {tcp_error, Socket, etimedout},

    ssl_test_lib:check_result(Client, {ssl_closed, SslSocket}).

%%--------------------------------------------------------------------
recv_error_handling() ->
    [{doc,"Special case of call error handling"}].
recv_error_handling(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server  = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					  {from, self()},
					  {mfa, {?MODULE, recv_close, []}},
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
call_in_error_state() ->
    [{doc,"Special case of call error handling"}].
call_in_error_state(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = [{cacertfile, "foo.pem"} | proplists:delete(cacertfile, ServerOpts0)],
    Pid = spawn_link(?MODULE, run_error_server, [[self() | ServerOpts]]),
    receive
        {Pid, Port} ->
            spawn_link(?MODULE, run_client_error, [[Port, ClientOpts]])
    end,
    receive
        {error, closed} ->
            ok;
        Other ->
            ct:fail(Other)
    end.

run_client_error([Port, Opts]) ->
    ssl:connect("localhost", Port, Opts).
    
run_error_server([ Pid | Opts]) ->
    {ok, Listen} = ssl:listen(0, Opts),
    {ok,{_, Port}} = ssl:sockname(Listen),
    Pid ! {self(), Port},
    {ok, Socket} = ssl:transport_accept(Listen),
    Pid ! ssl:controlling_process(Socket, self()).

%%--------------------------------------------------------------------

close_in_error_state() ->
    [{doc,"Special case of closing socket in error state"}].
close_in_error_state(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(server_opts, Config),
    ServerOpts = [{cacertfile, "foo.pem"} | proplists:delete(cacertfile, ServerOpts0)],
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    _ = spawn_link(?MODULE, run_error_server_close, [[self() | ServerOpts]]),
    receive
        {_Pid, Port} ->
            spawn_link(?MODULE, run_client_error, [[Port, ClientOpts]])
    end,
    receive
        ok ->
            ok;
        Other ->
            ct:fail(Other)
    end.
%%--------------------------------------------------------------------
abuse_transport_accept_socket() ->
    [{doc,"Only ssl:handshake and ssl:controlling_process is allowed for transport_accept:sockets"}].
abuse_transport_accept_socket(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
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
controlling_process_transport_accept_socket() ->
    [{doc,"Only ssl:handshake and ssl:controlling_process is allowed for transport_accept:sockets"}].
controlling_process_transport_accept_socket(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
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
run_error_server_close([Pid | Opts]) ->
    {ok, Listen} = ssl:listen(0, Opts),
    {ok,{_, Port}} = ssl:sockname(Listen),
    Pid ! {self(), Port},
    {ok, Socket} = ssl:transport_accept(Listen),
    Pid ! ssl:close(Socket).

%%--------------------------------------------------------------------

rizzo() ->
    [{doc, "Test that there is a 1/n-1-split for non RC4 in 'TLS < 1.1' as it is
    vunrable to Rizzo/Dungon attack"}].

rizzo(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    NVersion = ssl_test_lib:protocol_version(Config, tuple),
    Ciphers  = ssl:filter_cipher_suites(ssl:cipher_suites(all, NVersion),  
                                        [{key_exchange, 
                                          fun(Alg) when Alg == ecdh_rsa; Alg == ecdhe_rsa-> 
                                                  true;
                                             (_) -> 
                                                  false 
                                          end},
                                         {cipher, 
                                          fun(rc4_128) -> 
                                                  false;
                                             (chacha20_poly1305) ->
                                                  false;
                                             (_) -> 
                                                  true 
                                          end}]),

    run_send_recv_rizzo(Ciphers, Config, Version,
			 {?MODULE, send_recv_result_active_rizzo, []}).
%%--------------------------------------------------------------------
no_rizzo_rc4() ->
    [{doc,"Test that there is no 1/n-1-split for RC4 as it is not vunrable to Rizzo/Dungon attack"}].

no_rizzo_rc4(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    NVersion = ssl_test_lib:protocol_version(Config, tuple),
    %% Test uses RSA certs
    Ciphers  = ssl:filter_cipher_suites(ssl_test_lib:rc4_suites(NVersion),  
                                        [{key_exchange, 
                                          fun(Alg) when Alg == ecdh_rsa; Alg == ecdhe_rsa-> 
                                                  true;
                                             (_) -> 
                                                  false 
                                          end}]),
    run_send_recv_rizzo(Ciphers, Config, Version,
			{?MODULE, send_recv_result_active_no_rizzo, []}).

rizzo_one_n_minus_one() ->
    [{doc,"Test that the 1/n-1-split mitigation of Rizzo/Dungon attack can be explicitly selected"}].

rizzo_one_n_minus_one(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    NVersion = ssl_test_lib:protocol_version(Config, tuple),
    Ciphers  = ssl:filter_cipher_suites(ssl:cipher_suites(all, NVersion),
                                        [{key_exchange, 
                                          fun(Alg) when Alg == ecdh_rsa; Alg == ecdhe_rsa-> 
                                                  true;
                                             (_) -> 
                                                  false 
                                          end}, 
                                         {cipher, 
                                          fun(rc4_128) ->
                                                  false;
                                             %% TODO: remove this clause when chacha is fixed!
                                             (chacha20_poly1305) ->
                                                  false;
                                             (_) -> 
                                                  true 
                                          end}]),
    run_send_recv_rizzo(Ciphers, Config, Version,
                        {?MODULE, send_recv_result_active_rizzo, []}).

rizzo_zero_n() ->
    [{doc,"Test that the 0/n-split mitigation of Rizzo/Dungon attack can be explicitly selected"}].

rizzo_zero_n(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    NVersion = ssl_test_lib:protocol_version(Config, tuple),
    Ciphers  = ssl:filter_cipher_suites(ssl:cipher_suites(default, NVersion),
                                        [{cipher, 
                                          fun(rc4_128) ->
                                                  false;
                                             (_) -> 
                                                  true 
                                          end}]),
    run_send_recv_rizzo(Ciphers, Config, Version,
			 {?MODULE, send_recv_result_active_no_rizzo, []}).

rizzo_disabled() ->
    [{doc,"Test that the mitigation of Rizzo/Dungon attack can be explicitly disabled"}].

rizzo_disabled(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    Version = proplists:get_value(name, Prop),
    NVersion = ssl_test_lib:protocol_version(Config, tuple),
    Ciphers  = ssl:filter_cipher_suites(ssl:cipher_suites(default, NVersion),
                                        [{cipher, 
                                          fun(rc4_128) ->
                                                  false;
                                             (_) -> 
                                                  true 
                                          end}]),
    run_send_recv_rizzo(Ciphers, Config, Version,
			 {?MODULE, send_recv_result_active_no_rizzo, []}).

%%--------------------------------------------------------------------
new_server_wants_peer_cert() ->
    [{doc, "Test that server configured to do client certification does"
      " not reuse session without a client certificate."}].
new_server_wants_peer_cert(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    VServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
		  | ssl_test_lib:ssl_options(server_verification_opts, Config)],
    ClientOpts = ssl_test_lib:ssl_options(client_verification_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, peercert_result, []}},
				   {options,  [ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ClientOpts}]),

    Monitor = erlang:monitor(process, Server),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    receive
	{'DOWN', Monitor, _, _, _} ->
	    ok
    end,
    
    Server1 = ssl_test_lib:start_server([{node, ServerNode}, {port, Port},
					 {from, self()},
					 {mfa, {?MODULE, peercert_result, []}},
					 {options,  VServerOpts}]), 
    Client1 =
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
				   {host, Hostname},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, [ClientOpts]}]),

    CertFile = proplists:get_value(certfile, ClientOpts),
    [{'Certificate', BinCert, _}]= ssl_test_lib:pem_to_der(CertFile),

    ServerMsg = {error, no_peercert},
    Sever1Msg = {ok, BinCert},
   
    ssl_test_lib:check_result(Server, ServerMsg, Server1, Sever1Msg),

    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------
session_cache_process_list() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
session_cache_process_list(Config) when is_list(Config) ->
    session_cache_process(list,Config).
%%--------------------------------------------------------------------
session_cache_process_mnesia() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
session_cache_process_mnesia(Config) when is_list(Config) ->
    session_cache_process(mnesia,Config).

%%--------------------------------------------------------------------

tls_versions_option() ->
    [{doc,"Test API versions option to connect/listen."}].
tls_versions_option(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

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
    
    ErrClient = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
						 {host, Hostname},
						 {from, self()},
						 {options, [{versions , Available -- Supported} | ClientOpts]}]),
    receive
	{Server, _} ->
	    ok
    end,	    
    ssl_test_lib:check_client_alert(ErrClient, protocol_version).


%%--------------------------------------------------------------------
unordered_protocol_versions_server() ->
    [{doc,"Test that the highest protocol is selected even" 
      " when it is not first in the versions list."}].

unordered_protocol_versions_server(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),  

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
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),  

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
  
%%--------------------------------------------------------------------
max_handshake_size() ->
    [{doc,"Test that we can set max_handshake_size to max value."}].

max_handshake_size(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),  

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
  
%%--------------------------------------------------------------------

server_name_indication_option() ->
    [{doc,"Test API server_name_indication option to connect."}].
server_name_indication_option(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),  

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()}, 
					 {mfa, {ssl_test_lib, send_recv_result_active, []}},
					 {options,  
					  [{server_name_indication, disable} | 
					   ClientOpts]}
					]),
    
    ssl_test_lib:check_result(Server, ok, Client0, ok),
    Server ! listen,				       
    
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()}, 
					 {mfa, {ssl_test_lib, send_recv_result_active, []}},
					 {options,
					  [{server_name_indication, Hostname} | ClientOpts]
					 }]),    
    ssl_test_lib:check_result(Server, ok, Client1, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1).
%%--------------------------------------------------------------------

accept_pool() ->
    [{doc,"Test having an accept pool."}].
accept_pool(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),  

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server0 = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{accepters, 3},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server0),
    [Server1, Server2] = ssl_test_lib:accepters(2),

    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()}, 
					 {mfa, {ssl_test_lib, send_recv_result_active, []}},
					 {options, ClientOpts}
					]),
    
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()}, 
					 {mfa, {ssl_test_lib, send_recv_result_active, []}},
					 {options, ClientOpts}
					]),
    
    Client2 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					 {host, Hostname},
					 {from, self()}, 
					 {mfa, {ssl_test_lib, send_recv_result_active, []}},
					 {options, ClientOpts}
					]),

    ssl_test_lib:check_ok([Server0, Server1, Server2, Client0, Client1, Client2]),
    
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Server2),
    ssl_test_lib:close(Client0),
    ssl_test_lib:close(Client1),
    ssl_test_lib:close(Client2).
    
%%--------------------------------------------------------------------
%% TLS 1.3
%%--------------------------------------------------------------------

tls13_enable_client_side() ->
    [{doc,"Test that a TLS 1.3 client can connect to a TLS 1.2 server."}].

tls13_enable_client_side(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, protocol_info_result, []}},
					{options, [{versions,
                                                    ['tlsv1.1', 'tlsv1.2']} | ServerOpts] }]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, protocol_info_result, []}},
					{options,  [{versions,
                                                     ['tlsv1.2', 'tlsv1.3']} | ClientOpts]}]),

    ServerMsg = ClientMsg = {ok, 'tlsv1.2'},
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg).

tls13_enable_server_side() ->
    [{doc,"Test that a TLS 1.2 client can connect to a TLS 1.3 server."}].

tls13_enable_server_side(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, protocol_info_result, []}},
					{options, [{versions,
                                                    ['tlsv1.2', 'tlsv1.3']} | ServerOpts] }]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, protocol_info_result, []}},
					{options,  [{versions,
                                                     ['tlsv1.2', 'tlsv1.1']} | ClientOpts]}]),

    ServerMsg = ClientMsg = {ok, 'tlsv1.2'},
    ssl_test_lib:check_result(Server, ServerMsg, Client, ClientMsg).

tls_record_1_3_encode_decode() ->
     [{doc,"Test TLS 1.3 record encode/decode functions"}].

tls_record_1_3_encode_decode(_Config) ->
    ConnectionStates =
        #{current_read =>
              #{beast_mitigation => one_n_minus_one,
                cipher_state =>
                    {cipher_state,
                     <<14,172,111,243,199,170,242,203,126,205,34,93,122,115,226,14,
                       15,117,155,48,24,112,61,15,113,208,127,51,179,227,194,232>>,
                     <<197,54,168,218,54,91,157,58,30,201,197,142,51,58,53,231,228,
                       131,57,122,170,78,82,196,30,48,23,16,95,255,185,236>>,
                     undefined,undefined,undefined,16},
                client_verify_data => undefined,compression_state => undefined,
                mac_secret => undefined,secure_renegotiation => undefined,
                security_parameters =>
                    {security_parameters,
                     <<19,2>>,
                     0,8,2,undefined,undefined,undefined,undefined,undefined,
                     sha384,undefined,undefined,
                     {handshake_secret,
                      <<128,229,186,211,62,127,182,20,62,166,233,23,135,64,121,
                        3,104,251,214,161,253,31,3,2,232,37,8,221,189,72,64,218,
                        121,41,112,148,254,34,68,164,228,60,161,201,132,55,56,
                        157>>},
                     undefined,
                     <<92,24,205,75,244,60,136,212,250,32,214,20,37,3,213,87,61,207,
                       147,61,168,145,177,118,160,153,33,53,48,108,191,174>>,
                     undefined},
                sequence_number => 0,server_verify_data => undefined},
          current_write =>
              #{beast_mitigation => one_n_minus_one,
                cipher_state =>
                    {cipher_state,
                     <<14,172,111,243,199,170,242,203,126,205,34,93,122,115,226,14,
                       15,117,155,48,24,112,61,15,113,208,127,51,179,227,194,232>>,
                     <<197,54,168,218,54,91,157,58,30,201,197,142,51,58,53,231,228,
                       131,57,122,170,78,82,196,30,48,23,16,95,255,185,236>>,
                     undefined,undefined,undefined,16},
                client_verify_data => undefined,compression_state => undefined,
                mac_secret => undefined,secure_renegotiation => undefined,
                security_parameters =>
                    {security_parameters,
                     <<19,2>>,
                     0,8,2,undefined,undefined,undefined,undefined,undefined,
                     sha384,undefined,undefined,
                     {handshake_secret,
                      <<128,229,186,211,62,127,182,20,62,166,233,23,135,64,121,
                        3,104,251,214,161,253,31,3,2,232,37,8,221,189,72,64,218,
                        121,41,112,148,254,34,68,164,228,60,161,201,132,55,56,
                        157>>},
                     undefined,
                     <<92,24,205,75,244,60,136,212,250,32,214,20,37,3,213,87,61,207,
                       147,61,168,145,177,118,160,153,33,53,48,108,191,174>>,
                     undefined},
                sequence_number => 0,server_verify_data => undefined}},

    PlainText = [11,
                 <<0,2,175>>,
                 <<0,0,2,171,0,2,166,48,130,2,162,48,130,1,138,2,9,0,186,57,220,137,88,255,
                   191,235,48,13,6,9,42,134,72,134,247,13,1,1,11,5,0,48,18,49,16,48,14,6,3,85,
                   4,3,12,7,84,101,115,116,32,67,65,48,30,23,13,49,56,48,53,48,52,49,52,49,50,
                   51,56,90,23,13,50,56,48,50,48,52,49,52,49,50,51,56,90,48,20,49,18,48,16,6,
                   3,85,4,3,12,9,108,111,99,97,108,104,111,115,116,48,130,1,34,48,13,6,9,42,
                   134,72,134,247,13,1,1,1,5,0,3,130,1,15,0,48,130,1,10,2,130,1,1,0,169,40,
                   144,176,121,63,134,97,144,126,243,183,225,157,37,131,183,225,87,243,23,88,
                   230,70,9,134,32,147,7,27,167,98,51,81,224,75,199,12,229,251,195,207,75,179,
                   181,78,128,3,255,44,58,39,43,172,142,45,186,58,51,65,187,199,154,153,245,
                   70,133,137,1,27,87,42,116,65,251,129,109,145,233,97,171,71,54,213,185,74,
                   209,166,11,218,189,119,206,86,170,60,212,213,85,189,30,50,215,23,185,53,
                   132,238,132,176,198,250,139,251,198,221,225,128,109,113,23,220,39,143,71,
                   30,59,189,51,244,61,158,214,146,180,196,103,169,189,221,136,78,129,216,148,
                   2,9,8,65,37,224,215,233,13,209,21,235,20,143,33,74,59,53,208,90,152,94,251,
                   54,114,171,39,88,230,227,158,211,135,37,182,67,205,161,59,20,138,58,253,15,
                   53,48,8,157,9,95,197,9,177,116,21,54,9,125,78,109,182,83,20,16,234,223,116,
                   41,155,123,87,77,17,120,153,246,239,124,130,105,219,166,146,242,151,66,198,
                   75,72,63,28,246,86,16,244,223,22,36,50,15,247,222,98,6,152,136,154,72,150,
                   73,127,2,3,1,0,1,48,13,6,9,42,134,72,134,247,13,1,1,11,5,0,3,130,1,1,0,76,
                   33,54,160,229,219,219,193,150,116,245,252,18,39,235,145,86,12,167,171,52,
                   117,166,30,83,5,216,245,177,217,247,95,1,136,94,246,212,108,248,230,111,
                   225,202,189,6,129,8,70,128,245,18,204,215,87,82,129,253,227,122,66,182,184,
                   189,30,193,169,144,218,216,109,105,110,215,144,60,104,162,178,101,164,218,
                   122,60,37,41,143,57,150,52,59,51,112,238,113,239,168,114,69,183,143,154,73,
                   61,58,80,247,172,95,251,55,28,186,28,200,206,230,118,243,92,202,189,49,76,
                   124,252,76,0,247,112,85,194,69,59,222,163,228,103,49,110,104,109,251,155,
                   138,9,37,167,49,189,48,134,52,158,185,129,24,96,153,196,251,90,206,76,239,
                   175,119,174,165,133,108,222,125,237,125,187,149,152,83,190,16,202,94,202,
                   201,40,218,22,254,63,189,41,174,97,140,203,70,18,196,118,237,175,134,79,78,
                   246,2,61,54,77,186,112,32,17,193,192,188,217,252,215,200,7,245,180,179,132,
                   183,212,229,155,15,152,206,135,56,81,88,3,123,244,149,110,182,72,109,70,62,
                   146,152,146,151,107,126,216,210,9,93,0,0>>],

    {[_Header|Encoded], _} = tls_record_1_3:encode_plain_text(22, PlainText, ConnectionStates),
    CipherText = #ssl_tls{type = 23, version = {3,3}, fragment = Encoded},

    {#ssl_tls{type = 22, version = {3,4}, fragment = DecodedText}, _} =
        tls_record_1_3:decode_cipher_text(CipherText, ConnectionStates),

    DecodedText = iolist_to_binary(PlainText),
    ct:log("Decoded: ~p ~n", [DecodedText]),
    ok.

tls13_1_RTT_handshake() ->
     [{doc,"Test TLS 1.3 1-RTT Handshake"}].

tls13_1_RTT_handshake(_Config) ->
    %% ConnectionStates with NULL cipher
    ConnStatesNull =
       #{current_write =>
             #{security_parameters =>
                   #security_parameters{cipher_suite = ?TLS_NULL_WITH_NULL_NULL},
               sequence_number => 0
              }
        },

    %% {client}  construct a ClientHello handshake message:
    %%
    %%    ClientHello (196 octets):  01 00 00 c0 03 03 cb 34 ec b1 e7 81 63
    %%       ba 1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12 ec 18 a2 ef 62 83
    %%       02 4d ec e7 00 00 06 13 01 13 03 13 02 01 00 00 91 00 00 00 0b
    %%       00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00
    %%       12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 23
    %%       00 00 00 33 00 26 00 24 00 1d 00 20 99 38 1d e5 60 e4 bd 43 d2
    %%       3d 8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a
    %%       af 2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03
    %%       02 03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06
    %%       02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01
    %%
    %% {client}  send handshake record:
    %%
    %%    payload (196 octets):  01 00 00 c0 03 03 cb 34 ec b1 e7 81 63 ba
    %%       1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12 ec 18 a2 ef 62 83 02
    %%       4d ec e7 00 00 06 13 01 13 03 13 02 01 00 00 91 00 00 00 0b 00
    %%       09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00 12
    %%       00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 23 00
    %%       00 00 33 00 26 00 24 00 1d 00 20 99 38 1d e5 60 e4 bd 43 d2 3d
    %%       8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a af
    %%       2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03 02
    %%       03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06 02
    %%       02 02 00 2d 00 02 01 01 00 1c 00 02 40 01
    %%
    %%    complete record (201 octets):  16 03 01 00 c4 01 00 00 c0 03 03 cb
    %%       34 ec b1 e7 81 63 ba 1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12
    %%       ec 18 a2 ef 62 83 02 4d ec e7 00 00 06 13 01 13 03 13 02 01 00
    %%       00 91 00 00 00 0b 00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01
    %%       00 00 0a 00 14 00 12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02
    %%       01 03 01 04 00 23 00 00 00 33 00 26 00 24 00 1d 00 20 99 38 1d
    %%       e5 60 e4 bd 43 d2 3d 8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d
    %%       54 13 69 1e 52 9a af 2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e
    %%       04 03 05 03 06 03 02 03 08 04 08 05 08 06 04 01 05 01 06 01 02
    %%       01 04 02 05 02 06 02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01
    ClientHello =
        hexstr2bin("01 00 00 c0 03 03 cb 34 ec b1 e7 81 63
          ba 1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12 ec 18 a2 ef 62 83
          02 4d ec e7 00 00 06 13 01 13 03 13 02 01 00 00 91 00 00 00 0b
          00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01 00 00 0a 00 14 00
          12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 23
          00 00 00 33 00 26 00 24 00 1d 00 20 99 38 1d e5 60 e4 bd 43 d2
          3d 8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a
          af 2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e 04 03 05 03 06 03
          02 03 08 04 08 05 08 06 04 01 05 01 06 01 02 01 04 02 05 02 06
          02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01"),

    ClientHelloRecord =
        %% Current implementation always sets
        %% legacy_record_version to Ox0303
        hexstr2bin("16 03 03 00 c4 01 00 00 c0 03 03 cb
          34 ec b1 e7 81 63 ba 1c 38 c6 da cb 19 6a 6d ff a2 1a 8d 99 12
          ec 18 a2 ef 62 83 02 4d ec e7 00 00 06 13 01 13 03 13 02 01 00
          00 91 00 00 00 0b 00 09 00 00 06 73 65 72 76 65 72 ff 01 00 01
          00 00 0a 00 14 00 12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02
          01 03 01 04 00 23 00 00 00 33 00 26 00 24 00 1d 00 20 99 38 1d
          e5 60 e4 bd 43 d2 3d 8e 43 5a 7d ba fe b3 c0 6e 51 c1 3c ae 4d
          54 13 69 1e 52 9a af 2c 00 2b 00 03 02 03 04 00 0d 00 20 00 1e
          04 03 05 03 06 03 02 03 08 04 08 05 08 06 04 01 05 01 06 01 02
          01 04 02 05 02 06 02 02 02 00 2d 00 02 01 01 00 1c 00 02 40 01"),

    {CHEncrypted, _} =
	tls_record:encode_handshake(ClientHello, {3,4}, ConnStatesNull),
    ClientHelloRecord = iolist_to_binary(CHEncrypted),

    %% {server}  extract secret "early":
    %%
    %%    salt:  0 (all zero octets)
    %%
    %%    IKM (32 octets):  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%                      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%
    %%    secret (32 octets):  33 ad 0a 1c 60 7e c0 3b 09 e6 cd 98 93 68 0c
    %%       e2 10 ad f3 00 aa 1f 26 60 e1 b2 2e 10 f1 70 f9 2a
    HKDFAlgo = sha256,
    Salt = binary:copy(<<?BYTE(0)>>, 32),
    IKM = binary:copy(<<?BYTE(0)>>, 32),
    EarlySecret =
        hexstr2bin("33 ad 0a 1c 60 7e c0 3b 09 e6 cd 98 93 68 0c
          e2 10 ad f3 00 aa 1f 26 60 e1 b2 2e 10 f1 70 f9 2a"),

    {early_secret, EarlySecret} = tls_v1:key_schedule(early_secret, HKDFAlgo, {psk, Salt}),

    %% {client}  create an ephemeral x25519 key pair:
    %%
    %%    private key (32 octets):  49 af 42 ba 7f 79 94 85 2d 71 3e f2 78
    %%       4b cb ca a7 91 1d e2 6a dc 56 42 cb 63 45 40 e7 ea 50 05
    %%
    %%    public key (32 octets):  99 38 1d e5 60 e4 bd 43 d2 3d 8e 43 5a 7d
    %%       ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a af 2c
    CPublicKey =
        hexstr2bin("99 38 1d e5 60 e4 bd 43 d2 3d 8e 43 5a 7d
          ba fe b3 c0 6e 51 c1 3c ae 4d 54 13 69 1e 52 9a af 2c"),

    %% {server}  create an ephemeral x25519 key pair:
    %%
    %%   private key (32 octets):  b1 58 0e ea df 6d d5 89 b8 ef 4f 2d 56
    %%      52 57 8c c8 10 e9 98 01 91 ec 8d 05 83 08 ce a2 16 a2 1e
    %%
    %%   public key (32 octets):  c9 82 88 76 11 20 95 fe 66 76 2b db f7 c6
    %%      72 e1 56 d6 cc 25 3b 83 3d f1 dd 69 b1 b0 4e 75 1f 0f
    SPrivateKey =
        hexstr2bin("b1 58 0e ea df 6d d5 89 b8 ef 4f 2d 56
         52 57 8c c8 10 e9 98 01 91 ec 8d 05 83 08 ce a2 16 a2 1e"),

    SPublicKey =
        hexstr2bin("c9 82 88 76 11 20 95 fe 66 76 2b db f7 c6
         72 e1 56 d6 cc 25 3b 83 3d f1 dd 69 b1 b0 4e 75 1f 0f"),

    %% {server}  construct a ServerHello handshake message:
    %%
    %%    ServerHello (90 octets):  02 00 00 56 03 03 a6 af 06 a4 12 18 60
    %%       dc 5e 6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14 34 da c1 55 77 2e
    %%       d3 e2 69 28 00 13 01 00 00 2e 00 33 00 24 00 1d 00 20 c9 82 88
    %%       76 11 20 95 fe 66 76 2b db f7 c6 72 e1 56 d6 cc 25 3b 83 3d f1
    %%       dd 69 b1 b0 4e 75 1f 0f 00 2b 00 02 03 04
    ServerHello =
        hexstr2bin("02 00 00 56 03 03 a6 af 06 a4 12 18 60
          dc 5e 6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14 34 da c1 55 77 2e
          d3 e2 69 28 00 13 01 00 00 2e 00 33 00 24 00 1d 00 20 c9 82 88
          76 11 20 95 fe 66 76 2b db f7 c6 72 e1 56 d6 cc 25 3b 83 3d f1
          dd 69 b1 b0 4e 75 1f 0f 00 2b 00 02 03 04"),

    %% {server}  derive secret for handshake "tls13 derived":
    %%
    %%    PRK (32 octets):  33 ad 0a 1c 60 7e c0 3b 09 e6 cd 98 93 68 0c e2
    %%       10 ad f3 00 aa 1f 26 60 e1 b2 2e 10 f1 70 f9 2a
    %%
    %%    hash (32 octets):  e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24
    %%       27 ae 41 e4 64 9b 93 4c a4 95 99 1b 78 52 b8 55
    %%
    %%    info (49 octets):  00 20 0d 74 6c 73 31 33 20 64 65 72 69 76 65 64
    %%       20 e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24 27 ae 41 e4
    %%       64 9b 93 4c a4 95 99 1b 78 52 b8 55
    %%
    %%    expanded (32 octets):  6f 26 15 a1 08 c7 02 c5 67 8f 54 fc 9d ba
    %%       b6 97 16 c0 76 18 9c 48 25 0c eb ea c3 57 6c 36 11 ba
    Hash =
        hexstr2bin("e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24
          27 ae 41 e4 64 9b 93 4c a4 95 99 1b 78 52 b8 55"),

    Hash = crypto:hash(HKDFAlgo, <<>>),

    Info =
        hexstr2bin("00 20 0d 74 6c 73 31 33 20 64 65 72 69 76 65 64
          20 e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24 27 ae 41 e4
          64 9b 93 4c a4 95 99 1b 78 52 b8 55"),

    Info = tls_v1:create_info(<<"derived">>, Hash,  ssl_cipher:hash_size(HKDFAlgo)),

    Expanded =
        hexstr2bin("6f 26 15 a1 08 c7 02 c5 67 8f 54 fc 9d ba
          b6 97 16 c0 76 18 9c 48 25 0c eb ea c3 57 6c 36 11 ba"),

    Expanded = tls_v1:derive_secret(EarlySecret, <<"derived">>, <<>>, HKDFAlgo),

    %% {server}  extract secret "handshake":
    %%
    %%    salt (32 octets):  6f 26 15 a1 08 c7 02 c5 67 8f 54 fc 9d ba b6 97
    %%       16 c0 76 18 9c 48 25 0c eb ea c3 57 6c 36 11 ba
    %%
    %%    IKM (32 octets):  8b d4 05 4f b5 5b 9d 63 fd fb ac f9 f0 4b 9f 0d
    %%       35 e6 d6 3f 53 75 63 ef d4 62 72 90 0f 89 49 2d
    %%
    %%    secret (32 octets):  1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b
    %%       01 04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac

    %% salt = Expanded
    HandshakeIKM =
        hexstr2bin("8b d4 05 4f b5 5b 9d 63 fd fb ac f9 f0 4b 9f 0d
          35 e6 d6 3f 53 75 63 ef d4 62 72 90 0f 89 49 2d"),

    HandshakeSecret =
        hexstr2bin("1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b
          01 04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac"),

    HandshakeIKM = crypto:compute_key(ecdh, CPublicKey, SPrivateKey, x25519),

    {handshake_secret, HandshakeSecret} =
        tls_v1:key_schedule(handshake_secret, HKDFAlgo, HandshakeIKM,
                            {early_secret, EarlySecret}),

    %% {server}  derive secret "tls13 c hs traffic":
    %%
    %%    PRK (32 octets):  1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b 01
    %%       04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac
    %%
    %%    hash (32 octets):  86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58 ed
    %%       d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8
    %%
    %%    info (54 octets):  00 20 12 74 6c 73 31 33 20 63 20 68 73 20 74 72
    %%       61 66 66 69 63 20 86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58
    %%       ed d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8
    %%
    %%    expanded (32 octets):  b3 ed db 12 6e 06 7f 35 a7 80 b3 ab f4 5e
    %%       2d 8f 3b 1a 95 07 38 f5 2e 96 00 74 6a 0e 27 a5 5a 21

    %% PRK = HandshakeSecret
    CHSTHash =
        hexstr2bin("86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58 ed
          d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8"),

    CHSTInfo =
        hexstr2bin("00 20 12 74 6c 73 31 33 20 63 20 68 73 20 74 72
          61 66 66 69 63 20 86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58
          ed d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8"),

    CHSTrafficSecret =
        hexstr2bin(" b3 ed db 12 6e 06 7f 35 a7 80 b3 ab f4 5e
          2d 8f 3b 1a 95 07 38 f5 2e 96 00 74 6a 0e 27 a5 5a 21"),

    CHSH =  <<ClientHello/binary,ServerHello/binary>>,
    CHSTHash = crypto:hash(HKDFAlgo, CHSH),
    CHSTInfo =  tls_v1:create_info(<<"c hs traffic">>, CHSTHash,  ssl_cipher:hash_size(HKDFAlgo)),

    CHSTrafficSecret =
        tls_v1:client_handshake_traffic_secret(HKDFAlgo, {handshake_secret, HandshakeSecret}, CHSH),

    %% {server}  derive secret "tls13 s hs traffic":
    %%
    %%    PRK (32 octets):  1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b 01
    %%       04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac
    %%
    %%    hash (32 octets):  86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58 ed
    %%       d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8
    %%
    %%    info (54 octets):  00 20 12 74 6c 73 31 33 20 73 20 68 73 20 74 72
    %%       61 66 66 69 63 20 86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58
    %%       ed d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8
    %%
    %%    expanded (32 octets):  b6 7b 7d 69 0c c1 6c 4e 75 e5 42 13 cb 2d
    %%       37 b4 e9 c9 12 bc de d9 10 5d 42 be fd 59 d3 91 ad 38

    %% PRK = HandshakeSecret
    %% hash = CHSTHash
    SHSTInfo =
        hexstr2bin("00 20 12 74 6c 73 31 33 20 73 20 68 73 20 74 72
          61 66 66 69 63 20 86 0c 06 ed c0 78 58 ee 8e 78 f0 e7 42 8c 58
          ed d6 b4 3f 2c a3 e6 e9 5f 02 ed 06 3c f0 e1 ca d8"),

    SHSTrafficSecret =
        hexstr2bin("b6 7b 7d 69 0c c1 6c 4e 75 e5 42 13 cb 2d
          37 b4 e9 c9 12 bc de d9 10 5d 42 be fd 59 d3 91 ad 38"),

    SHSTInfo =  tls_v1:create_info(<<"s hs traffic">>, CHSTHash,  ssl_cipher:hash_size(HKDFAlgo)),

    SHSTrafficSecret =
        tls_v1:server_handshake_traffic_secret(HKDFAlgo, {handshake_secret, HandshakeSecret}, CHSH),


    %% {server}  derive secret for master "tls13 derived":
    %%
    %%    PRK (32 octets):  1d c8 26 e9 36 06 aa 6f dc 0a ad c1 2f 74 1b 01
    %%       04 6a a6 b9 9f 69 1e d2 21 a9 f0 ca 04 3f be ac
    %%
    %%    hash (32 octets):  e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24
    %%       27 ae 41 e4 64 9b 93 4c a4 95 99 1b 78 52 b8 55
    %%
    %%    info (49 octets):  00 20 0d 74 6c 73 31 33 20 64 65 72 69 76 65 64
    %%       20 e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24 27 ae 41 e4
    %%       64 9b 93 4c a4 95 99 1b 78 52 b8 55
    %%
    %%    expanded (32 octets):  43 de 77 e0 c7 77 13 85 9a 94 4d b9 db 25
    %%       90 b5 31 90 a6 5b 3e e2 e4 f1 2d d7 a0 bb 7c e2 54 b4

    %% PRK = HandshakeSecret
    %% hash = Hash
    %% info = Info
    MasterDeriveSecret =
        hexstr2bin("43 de 77 e0 c7 77 13 85 9a 94 4d b9 db 25
          90 b5 31 90 a6 5b 3e e2 e4 f1 2d d7 a0 bb 7c e2 54 b4"),

    MasterDeriveSecret = tls_v1:derive_secret(HandshakeSecret, <<"derived">>, <<>>, HKDFAlgo),

    %% {server}  extract secret "master":
    %%
    %%    salt (32 octets):  43 de 77 e0 c7 77 13 85 9a 94 4d b9 db 25 90 b5
    %%       31 90 a6 5b 3e e2 e4 f1 2d d7 a0 bb 7c e2 54 b4
    %%
    %%    IKM (32 octets):  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    %%
    %%    secret (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a
    %%       47 80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19

    %% salt = MasterDeriveSecret
    %% IKM = IKM
    MasterSecret =
        hexstr2bin("18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a
          47 80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19"),

    {master_secret, MasterSecret} =
        tls_v1:key_schedule(master_secret, HKDFAlgo, {handshake_secret, HandshakeSecret}),

    %% {server}  send handshake record:
    %%
    %%    payload (90 octets):  02 00 00 56 03 03 a6 af 06 a4 12 18 60 dc 5e
    %%       6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14 34 da c1 55 77 2e d3 e2
    %%       69 28 00 13 01 00 00 2e 00 33 00 24 00 1d 00 20 c9 82 88 76 11
    %%       20 95 fe 66 76 2b db f7 c6 72 e1 56 d6 cc 25 3b 83 3d f1 dd 69
    %%       b1 b0 4e 75 1f 0f 00 2b 00 02 03 04
    %%
    %%    complete record (95 octets):  16 03 03 00 5a 02 00 00 56 03 03 a6
    %%       af 06 a4 12 18 60 dc 5e 6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14
    %%       34 da c1 55 77 2e d3 e2 69 28 00 13 01 00 00 2e 00 33 00 24 00
    %%       1d 00 20 c9 82 88 76 11 20 95 fe 66 76 2b db f7 c6 72 e1 56 d6
    %%       cc 25 3b 83 3d f1 dd 69 b1 b0 4e 75 1f 0f 00 2b 00 02 03 04

    %% payload = ServerHello
    ServerHelloRecord =
        hexstr2bin("16 03 03 00 5a 02 00 00 56 03 03 a6
          af 06 a4 12 18 60 dc 5e 6e 60 24 9c d3 4c 95 93 0c 8a c5 cb 14
          34 da c1 55 77 2e d3 e2 69 28 00 13 01 00 00 2e 00 33 00 24 00
          1d 00 20 c9 82 88 76 11 20 95 fe 66 76 2b db f7 c6 72 e1 56 d6
          cc 25 3b 83 3d f1 dd 69 b1 b0 4e 75 1f 0f 00 2b 00 02 03 04"),

    {SHEncrypted, _} =
	tls_record:encode_handshake(ServerHello, {3,4}, ConnStatesNull),
    ServerHelloRecord = iolist_to_binary(SHEncrypted),

    %% {server}  derive write traffic keys for handshake data:
    %%
    %%    PRK (32 octets):  b6 7b 7d 69 0c c1 6c 4e 75 e5 42 13 cb 2d 37 b4
    %%       e9 c9 12 bc de d9 10 5d 42 be fd 59 d3 91 ad 38
    %%
    %%    key info (13 octets):  00 10 09 74 6c 73 31 33 20 6b 65 79 00
    %%
    %%    key expanded (16 octets):  3f ce 51 60 09 c2 17 27 d0 f2 e4 e8 6e
    %%       e4 03 bc
    %%
    %%    iv info (12 octets):  00 0c 08 74 6c 73 31 33 20 69 76 00
    %%
    %%    iv expanded (12 octets):  5d 31 3e b2 67 12 76 ee 13 00 0b 30

    %% PRK = SHSTrafficSecret
    WriteKeyInfo =
        hexstr2bin("00 10 09 74 6c 73 31 33 20 6b 65 79 00"),

    WriteKey =
        hexstr2bin("3f ce 51 60 09 c2 17 27 d0 f2 e4 e8 6e e4 03 bc"),

    WriteIVInfo =
        hexstr2bin("00 0c 08 74 6c 73 31 33 20 69 76 00"),

    WriteIV =
        hexstr2bin(" 5d 31 3e b2 67 12 76 ee 13 00 0b 30"),

    Cipher = aes_128_gcm, %% TODO: get from ServerHello

    WriteKeyInfo = tls_v1:create_info(<<"key">>, <<>>,  ssl_cipher:key_material(Cipher)),
    %% TODO: remove hardcoded IV size
    WriteIVInfo = tls_v1:create_info(<<"iv">>, <<>>,  12),

    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, SHSTrafficSecret),

    %% {server}  construct an EncryptedExtensions handshake message:
    %%
    %%    EncryptedExtensions (40 octets):  08 00 00 24 00 22 00 0a 00 14 00
    %%       12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 1c
    %%       00 02 40 01 00 00 00 00
    %%
    %% {server}  construct a Certificate handshake message:
    %%
    %%    Certificate (445 octets):  0b 00 01 b9 00 00 01 b5 00 01 b0 30 82
    %%       01 ac 30 82 01 15 a0 03 02 01 02 02 01 02 30 0d 06 09 2a 86 48
    %%       86 f7 0d 01 01 0b 05 00 30 0e 31 0c 30 0a 06 03 55 04 03 13 03
    %%       72 73 61 30 1e 17 0d 31 36 30 37 33 30 30 31 32 33 35 39 5a 17
    %%       0d 32 36 30 37 33 30 30 31 32 33 35 39 5a 30 0e 31 0c 30 0a 06
    %%       03 55 04 03 13 03 72 73 61 30 81 9f 30 0d 06 09 2a 86 48 86 f7
    %%       0d 01 01 01 05 00 03 81 8d 00 30 81 89 02 81 81 00 b4 bb 49 8f
    %%       82 79 30 3d 98 08 36 39 9b 36 c6 98 8c 0c 68 de 55 e1 bd b8 26
    %%       d3 90 1a 24 61 ea fd 2d e4 9a 91 d0 15 ab bc 9a 95 13 7a ce 6c
    %%       1a f1 9e aa 6a f9 8c 7c ed 43 12 09 98 e1 87 a8 0e e0 cc b0 52
    %%       4b 1b 01 8c 3e 0b 63 26 4d 44 9a 6d 38 e2 2a 5f da 43 08 46 74
    %%       80 30 53 0e f0 46 1c 8c a9 d9 ef bf ae 8e a6 d1 d0 3e 2b d1 93
    %%       ef f0 ab 9a 80 02 c4 74 28 a6 d3 5a 8d 88 d7 9f 7f 1e 3f 02 03
    %%       01 00 01 a3 1a 30 18 30 09 06 03 55 1d 13 04 02 30 00 30 0b 06
    %%       03 55 1d 0f 04 04 03 02 05 a0 30 0d 06 09 2a 86 48 86 f7 0d 01
    %%       01 0b 05 00 03 81 81 00 85 aa d2 a0 e5 b9 27 6b 90 8c 65 f7 3a
    %%       72 67 17 06 18 a5 4c 5f 8a 7b 33 7d 2d f7 a5 94 36 54 17 f2 ea
    %%       e8 f8 a5 8c 8f 81 72 f9 31 9c f3 6b 7f d6 c5 5b 80 f2 1a 03 01
    %%       51 56 72 60 96 fd 33 5e 5e 67 f2 db f1 02 70 2e 60 8c ca e6 be
    %%       c1 fc 63 a4 2a 99 be 5c 3e b7 10 7c 3c 54 e9 b9 eb 2b d5 20 3b
    %%       1c 3b 84 e0 a8 b2 f7 59 40 9b a3 ea c9 d9 1d 40 2d cc 0c c8 f8
    %%       96 12 29 ac 91 87 b4 2b 4d e1 00 00
    %%
    %% {server}  construct a CertificateVerify handshake message:
    %%
    %%    CertificateVerify (136 octets):  0f 00 00 84 08 04 00 80 5a 74 7c
    %%       5d 88 fa 9b d2 e5 5a b0 85 a6 10 15 b7 21 1f 82 4c d4 84 14 5a
    %%       b3 ff 52 f1 fd a8 47 7b 0b 7a bc 90 db 78 e2 d3 3a 5c 14 1a 07
    %%       86 53 fa 6b ef 78 0c 5e a2 48 ee aa a7 85 c4 f3 94 ca b6 d3 0b
    %%       be 8d 48 59 ee 51 1f 60 29 57 b1 54 11 ac 02 76 71 45 9e 46 44
    %%       5c 9e a5 8c 18 1e 81 8e 95 b8 c3 fb 0b f3 27 84 09 d3 be 15 2a
    %%       3d a5 04 3e 06 3d da 65 cd f5 ae a2 0d 53 df ac d4 2f 74 f3
    EncryptedExtensions =
        hexstr2bin("08 00 00 24 00 22 00 0a 00 14 00
          12 00 1d 00 17 00 18 00 19 01 00 01 01 01 02 01 03 01 04 00 1c
          00 02 40 01 00 00 00 00"),

    Certificate =
        hexstr2bin("0b 00 01 b9 00 00 01 b5 00 01 b0 30 82
          01 ac 30 82 01 15 a0 03 02 01 02 02 01 02 30 0d 06 09 2a 86 48
          86 f7 0d 01 01 0b 05 00 30 0e 31 0c 30 0a 06 03 55 04 03 13 03
          72 73 61 30 1e 17 0d 31 36 30 37 33 30 30 31 32 33 35 39 5a 17
          0d 32 36 30 37 33 30 30 31 32 33 35 39 5a 30 0e 31 0c 30 0a 06
          03 55 04 03 13 03 72 73 61 30 81 9f 30 0d 06 09 2a 86 48 86 f7
          0d 01 01 01 05 00 03 81 8d 00 30 81 89 02 81 81 00 b4 bb 49 8f
          82 79 30 3d 98 08 36 39 9b 36 c6 98 8c 0c 68 de 55 e1 bd b8 26
          d3 90 1a 24 61 ea fd 2d e4 9a 91 d0 15 ab bc 9a 95 13 7a ce 6c
          1a f1 9e aa 6a f9 8c 7c ed 43 12 09 98 e1 87 a8 0e e0 cc b0 52
          4b 1b 01 8c 3e 0b 63 26 4d 44 9a 6d 38 e2 2a 5f da 43 08 46 74
          80 30 53 0e f0 46 1c 8c a9 d9 ef bf ae 8e a6 d1 d0 3e 2b d1 93
          ef f0 ab 9a 80 02 c4 74 28 a6 d3 5a 8d 88 d7 9f 7f 1e 3f 02 03
          01 00 01 a3 1a 30 18 30 09 06 03 55 1d 13 04 02 30 00 30 0b 06
          03 55 1d 0f 04 04 03 02 05 a0 30 0d 06 09 2a 86 48 86 f7 0d 01
          01 0b 05 00 03 81 81 00 85 aa d2 a0 e5 b9 27 6b 90 8c 65 f7 3a
          72 67 17 06 18 a5 4c 5f 8a 7b 33 7d 2d f7 a5 94 36 54 17 f2 ea
          e8 f8 a5 8c 8f 81 72 f9 31 9c f3 6b 7f d6 c5 5b 80 f2 1a 03 01
          51 56 72 60 96 fd 33 5e 5e 67 f2 db f1 02 70 2e 60 8c ca e6 be
          c1 fc 63 a4 2a 99 be 5c 3e b7 10 7c 3c 54 e9 b9 eb 2b d5 20 3b
          1c 3b 84 e0 a8 b2 f7 59 40 9b a3 ea c9 d9 1d 40 2d cc 0c c8 f8
          96 12 29 ac 91 87 b4 2b 4d e1 00 00"),

    CertificateVerify =
        hexstr2bin("0f 00 00 84 08 04 00 80 5a 74 7c
          5d 88 fa 9b d2 e5 5a b0 85 a6 10 15 b7 21 1f 82 4c d4 84 14 5a
          b3 ff 52 f1 fd a8 47 7b 0b 7a bc 90 db 78 e2 d3 3a 5c 14 1a 07
          86 53 fa 6b ef 78 0c 5e a2 48 ee aa a7 85 c4 f3 94 ca b6 d3 0b
          be 8d 48 59 ee 51 1f 60 29 57 b1 54 11 ac 02 76 71 45 9e 46 44
          5c 9e a5 8c 18 1e 81 8e 95 b8 c3 fb 0b f3 27 84 09 d3 be 15 2a
          3d a5 04 3e 06 3d da 65 cd f5 ae a2 0d 53 df ac d4 2f 74 f3"),

    %% {server}  calculate finished "tls13 finished":
    %%
    %%    PRK (32 octets):  b6 7b 7d 69 0c c1 6c 4e 75 e5 42 13 cb 2d 37 b4
    %%       e9 c9 12 bc de d9 10 5d 42 be fd 59 d3 91 ad 38
    %%
    %%    hash (0 octets):  (empty)
    %%
    %%    info (18 octets):  00 20 0e 74 6c 73 31 33 20 66 69 6e 69 73 68 65
    %%       64 00
    %%
    %%    expanded (32 octets):  00 8d 3b 66 f8 16 ea 55 9f 96 b5 37 e8 85
    %%       c3 1f c0 68 bf 49 2c 65 2f 01 f2 88 a1 d8 cd c1 9f c8
    %%
    %%    finished (32 octets):  9b 9b 14 1d 90 63 37 fb d2 cb dc e7 1d f4
    %%       de da 4a b4 2c 30 95 72 cb 7f ff ee 54 54 b7 8f 07 18

    %% PRK = SHSTrafficSecret
    FInfo =
        hexstr2bin("00 20 0e 74 6c 73 31 33 20 66 69 6e 69 73 68 65
          64 00"),

    FExpanded =
        hexstr2bin("00 8d 3b 66 f8 16 ea 55 9f 96 b5 37 e8 85
          c3 1f c0 68 bf 49 2c 65 2f 01 f2 88 a1 d8 cd c1 9f c8"),

    FinishedVerifyData =
        hexstr2bin("9b 9b 14 1d 90 63 37 fb d2 cb dc e7 1d f4
          de da 4a b4 2c 30 95 72 cb 7f ff ee 54 54 b7 8f 07 18"),

    FInfo = tls_v1:create_info(<<"finished">>, <<>>,  ssl_cipher:hash_size(HKDFAlgo)),

    FExpanded = tls_v1:finished_key(SHSTrafficSecret, HKDFAlgo),

    MessageHistory0 = [CertificateVerify,
                       Certificate,
                       EncryptedExtensions,
                       ServerHello,
                       ClientHello],

    FinishedVerifyData = tls_v1:finished_verify_data(FExpanded, HKDFAlgo, MessageHistory0),

    %% {server}  construct a Finished handshake message:
    %%
    %%    Finished (36 octets):  14 00 00 20 9b 9b 14 1d 90 63 37 fb d2 cb
    %%       dc e7 1d f4 de da 4a b4 2c 30 95 72 cb 7f ff ee 54 54 b7 8f 07
    %%       18
    FinishedHSBin =
        hexstr2bin("14 00 00 20 9b 9b 14 1d 90 63 37 fb d2 cb
          dc e7 1d f4 de da 4a b4 2c 30 95 72 cb 7f ff ee 54 54 b7 8f 07
          18"),

    FinishedHS = #finished{verify_data = FinishedVerifyData},

    FinishedIOList = tls_handshake:encode_handshake(FinishedHS, {3,4}),
    FinishedHSBin = iolist_to_binary(FinishedIOList),

    %% {server}  derive secret "tls13 c ap traffic":
    %%
    %%    PRK (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a 47
    %%       80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19
    %%
    %%    hash (32 octets):  96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a
    %%       00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    info (54 octets):  00 20 12 74 6c 73 31 33 20 63 20 61 70 20 74 72
    %%       61 66 66 69 63 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b
    %%       1a 00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    expanded (32 octets):  9e 40 64 6c e7 9a 7f 9d c0 5a f8 88 9b ce
    %%       65 52 87 5a fa 0b 06 df 00 87 f7 92 eb b7 c1 75 04 a5

    %% PRK = MasterSecret
    CAPTHash =
        hexstr2bin("96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a
          00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13"),
    CAPTInfo =
        hexstr2bin("00 20 12 74 6c 73 31 33 20 63 20 61 70 20 74 72
          61 66 66 69 63 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b
          1a 00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13"),

    CAPTrafficSecret =
        hexstr2bin("9e 40 64 6c e7 9a 7f 9d c0 5a f8 88 9b ce
          65 52 87 5a fa 0b 06 df 00 87 f7 92 eb b7 c1 75 04 a5"),

    CHSF = <<ClientHello/binary,
             ServerHello/binary,
             EncryptedExtensions/binary,
             Certificate/binary,
             CertificateVerify/binary,
             FinishedHSBin/binary>>,

    CAPTHash = crypto:hash(HKDFAlgo, CHSF),

    CAPTInfo =
        tls_v1:create_info(<<"c ap traffic">>, CAPTHash, ssl_cipher:hash_size(HKDFAlgo)),

    CAPTrafficSecret =
        tls_v1:client_application_traffic_secret_0(HKDFAlgo, {master_secret, MasterSecret}, CHSF),

    %% {server}  derive secret "tls13 s ap traffic":
    %%
    %%    PRK (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a 47
    %%       80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19
    %%
    %%    hash (32 octets):  96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a
    %%       00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    info (54 octets):  00 20 12 74 6c 73 31 33 20 73 20 61 70 20 74 72
    %%       61 66 66 69 63 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b
    %%       1a 00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    expanded (32 octets):  a1 1a f9 f0 55 31 f8 56 ad 47 11 6b 45 a9
    %%       50 32 82 04 b4 f4 4b fb 6b 3a 4b 4f 1f 3f cb 63 16 43

    %% PRK = MasterSecret
    %% hash = CAPTHash
    SAPTInfo =
        hexstr2bin(" 00 20 12 74 6c 73 31 33 20 73 20 61 70 20 74 72
          61 66 66 69 63 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b
          1a 00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13"),

    SAPTrafficSecret =
        hexstr2bin("a1 1a f9 f0 55 31 f8 56 ad 47 11 6b 45 a9
          50 32 82 04 b4 f4 4b fb 6b 3a 4b 4f 1f 3f cb 63 16 43"),

    SAPTInfo =
        tls_v1:create_info(<<"s ap traffic">>, CAPTHash, ssl_cipher:hash_size(HKDFAlgo)),

    SAPTrafficSecret =
        tls_v1:server_application_traffic_secret_0(HKDFAlgo, {master_secret, MasterSecret}, CHSF),

    %% {server}  derive secret "tls13 exp master":
    %%
    %%    PRK (32 octets):  18 df 06 84 3d 13 a0 8b f2 a4 49 84 4c 5f 8a 47
    %%       80 01 bc 4d 4c 62 79 84 d5 a4 1d a8 d0 40 29 19
    %%
    %%    hash (32 octets):  96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a
    %%       00 0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    info (52 octets):  00 20 10 74 6c 73 31 33 20 65 78 70 20 6d 61 73
    %%       74 65 72 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a 00
    %%       0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13
    %%
    %%    expanded (32 octets):  fe 22 f8 81 17 6e da 18 eb 8f 44 52 9e 67
    %%       92 c5 0c 9a 3f 89 45 2f 68 d8 ae 31 1b 43 09 d3 cf 50

    %% PRK = MasterSecret
    %% hash = CAPTHash
    ExporterInfo =
        hexstr2bin("00 20 10 74 6c 73 31 33 20 65 78 70 20 6d 61 73
          74 65 72 20 96 08 10 2a 0f 1c cc 6d b6 25 0b 7b 7e 41 7b 1a 00
          0e aa da 3d aa e4 77 7a 76 86 c9 ff 83 df 13"),

    ExporterMasterSecret =
        hexstr2bin("fe 22 f8 81 17 6e da 18 eb 8f 44 52 9e 67
          92 c5 0c 9a 3f 89 45 2f 68 d8 ae 31 1b 43 09 d3 cf 50"),

    ExporterInfo =
        tls_v1:create_info(<<"exp master">>, CAPTHash, ssl_cipher:hash_size(HKDFAlgo)),

    ExporterMasterSecret =
        tls_v1:exporter_master_secret(HKDFAlgo, {master_secret, MasterSecret}, CHSF),

    %% {server}  derive write traffic keys for application data:
    %%
    %%    PRK (32 octets):  a1 1a f9 f0 55 31 f8 56 ad 47 11 6b 45 a9 50 32
    %%       82 04 b4 f4 4b fb 6b 3a 4b 4f 1f 3f cb 63 16 43
    %%
    %%    key info (13 octets):  00 10 09 74 6c 73 31 33 20 6b 65 79 00
    %%
    %%    key expanded (16 octets):  9f 02 28 3b 6c 9c 07 ef c2 6b b9 f2 ac
    %%       92 e3 56
    %%
    %%    iv info (12 octets):  00 0c 08 74 6c 73 31 33 20 69 76 00
    %%
    %%    iv expanded (12 octets):  cf 78 2b 88 dd 83 54 9a ad f1 e9 84

    %% PRK = SAPTrafficsecret
    %% key info = WriteKeyInfo
    %% iv info = WrtieIVInfo
    SWKey =
        hexstr2bin("9f 02 28 3b 6c 9c 07 ef c2 6b b9 f2 ac 92 e3 56"),

    SWIV =
        hexstr2bin("cf 78 2b 88 dd 83 54 9a ad f1 e9 84"),

    {SWKey, SWIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, SAPTrafficSecret),

    %% {server}  derive read traffic keys for handshake data:
    %%
    %%    PRK (32 octets):  b3 ed db 12 6e 06 7f 35 a7 80 b3 ab f4 5e 2d 8f
    %%       3b 1a 95 07 38 f5 2e 96 00 74 6a 0e 27 a5 5a 21
    %%
    %%    key info (13 octets):  00 10 09 74 6c 73 31 33 20 6b 65 79 00
    %%
    %%    key expanded (16 octets):  db fa a6 93 d1 76 2c 5b 66 6a f5 d9 50
    %%       25 8d 01
    %%
    %%    iv info (12 octets):  00 0c 08 74 6c 73 31 33 20 69 76 00
    %%
    %%    iv expanded (12 octets):  5b d3 c7 1b 83 6e 0b 76 bb 73 26 5f

    %% PRK = CHSTrafficsecret
    %% key info = WriteKeyInfo
    %% iv info = WrtieIVInfo
    SRKey =
        hexstr2bin("db fa a6 93 d1 76 2c 5b 66 6a f5 d9 50 25 8d 01"),

    SRIV =
        hexstr2bin("5b d3 c7 1b 83 6e 0b 76 bb 73 26 5f"),

    {SRKey, SRIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, CHSTrafficSecret).


tls13_finished_verify_data() ->
     [{doc,"Test TLS 1.3 Finished message handling"}].

tls13_finished_verify_data(_Config) ->
    ClientHello =
        hexstr2bin("01 00 00 c6 03 03 00 01  02 03 04 05 06 07 08 09
                    0a 0b 0c 0d 0e 0f 10 11  12 13 14 15 16 17 18 19
                    1a 1b 1c 1d 1e 1f 20 e0  e1 e2 e3 e4 e5 e6 e7 e8
                    e9 ea eb ec ed ee ef f0  f1 f2 f3 f4 f5 f6 f7 f8
                    f9 fa fb fc fd fe ff 00  06 13 01 13 02 13 03 01
                    00 00 77 00 00 00 18 00  16 00 00 13 65 78 61 6d
                    70 6c 65 2e 75 6c 66 68  65 69 6d 2e 6e 65 74 00
                    0a 00 08 00 06 00 1d 00  17 00 18 00 0d 00 14 00
                    12 04 03 08 04 04 01 05  03 08 05 05 01 08 06 06
                    01 02 01 00 33 00 26 00  24 00 1d 00 20 35 80 72
                    d6 36 58 80 d1 ae ea 32  9a df 91 21 38 38 51 ed
                    21 a2 8e 3b 75 e9 65 d0  d2 cd 16 62 54 00 2d 00
                    02 01 01 00 2b 00 03 02  03 04"),

    ServerHello =
        hexstr2bin("02 00 00 76 03 03 70 71  72 73 74 75 76 77 78 79
                    7a 7b 7c 7d 7e 7f 80 81  82 83 84 85 86 87 88 89
                    8a 8b 8c 8d 8e 8f 20 e0  e1 e2 e3 e4 e5 e6 e7 e8
                    e9 ea eb ec ed ee ef f0  f1 f2 f3 f4 f5 f6 f7 f8
                    f9 fa fb fc fd fe ff 13  01 00 00 2e 00 33 00 24
                    00 1d 00 20 9f d7 ad 6d  cf f4 29 8d d3 f9 6d 5b
                    1b 2a f9 10 a0 53 5b 14  88 d7 f8 fa bb 34 9a 98
                    28 80 b6 15 00 2b 00 02  03 04"),

    EncryptedExtensions =
        hexstr2bin("08 00 00 02 00 00"),

    Certificate =
        hexstr2bin("0b 00 03 2e 00 00 03 2a  00 03 25 30 82 03 21 30
                    82 02 09 a0 03 02 01 02  02 08 15 5a 92 ad c2 04
                    8f 90 30 0d 06 09 2a 86  48 86 f7 0d 01 01 0b 05
                    00 30 22 31 0b 30 09 06  03 55 04 06 13 02 55 53
                    31 13 30 11 06 03 55 04  0a 13 0a 45 78 61 6d 70
                    6c 65 20 43 41 30 1e 17  0d 31 38 31 30 30 35 30
                    31 33 38 31 37 5a 17 0d  31 39 31 30 30 35 30 31
                    33 38 31 37 5a 30 2b 31  0b 30 09 06 03 55 04 06
                    13 02 55 53 31 1c 30 1a  06 03 55 04 03 13 13 65
                    78 61 6d 70 6c 65 2e 75  6c 66 68 65 69 6d 2e 6e
                    65 74 30 82 01 22 30 0d  06 09 2a 86 48 86 f7 0d
                    01 01 01 05 00 03 82 01  0f 00 30 82 01 0a 02 82
                    01 01 00 c4 80 36 06 ba  e7 47 6b 08 94 04 ec a7
                    b6 91 04 3f f7 92 bc 19  ee fb 7d 74 d7 a8 0d 00
                    1e 7b 4b 3a 4a e6 0f e8  c0 71 fc 73 e7 02 4c 0d
                    bc f4 bd d1 1d 39 6b ba  70 46 4a 13 e9 4a f8 3d
                    f3 e1 09 59 54 7b c9 55  fb 41 2d a3 76 52 11 e1
                    f3 dc 77 6c aa 53 37 6e  ca 3a ec be c3 aa b7 3b
                    31 d5 6c b6 52 9c 80 98  bc c9 e0 28 18 e2 0b f7
                    f8 a0 3a fd 17 04 50 9e  ce 79 bd 9f 39 f1 ea 69
                    ec 47 97 2e 83 0f b5 ca  95 de 95 a1 e6 04 22 d5
                    ee be 52 79 54 a1 e7 bf  8a 86 f6 46 6d 0d 9f 16
                    95 1a 4c f7 a0 46 92 59  5c 13 52 f2 54 9e 5a fb
                    4e bf d7 7a 37 95 01 44  e4 c0 26 87 4c 65 3e 40
                    7d 7d 23 07 44 01 f4 84  ff d0 8f 7a 1f a0 52 10
                    d1 f4 f0 d5 ce 79 70 29  32 e2 ca be 70 1f df ad
                    6b 4b b7 11 01 f4 4b ad  66 6a 11 13 0f e2 ee 82
                    9e 4d 02 9d c9 1c dd 67  16 db b9 06 18 86 ed c1
                    ba 94 21 02 03 01 00 01  a3 52 30 50 30 0e 06 03
                    55 1d 0f 01 01 ff 04 04  03 02 05 a0 30 1d 06 03
                    55 1d 25 04 16 30 14 06  08 2b 06 01 05 05 07 03
                    02 06 08 2b 06 01 05 05  07 03 01 30 1f 06 03 55
                    1d 23 04 18 30 16 80 14  89 4f de 5b cc 69 e2 52
                    cf 3e a3 00 df b1 97 b8  1d e1 c1 46 30 0d 06 09
                    2a 86 48 86 f7 0d 01 01  0b 05 00 03 82 01 01 00
                    59 16 45 a6 9a 2e 37 79  e4 f6 dd 27 1a ba 1c 0b
                    fd 6c d7 55 99 b5 e7 c3  6e 53 3e ff 36 59 08 43
                    24 c9 e7 a5 04 07 9d 39  e0 d4 29 87 ff e3 eb dd
                    09 c1 cf 1d 91 44 55 87  0b 57 1d d1 9b df 1d 24
                    f8 bb 9a 11 fe 80 fd 59  2b a0 39 8c de 11 e2 65
                    1e 61 8c e5 98 fa 96 e5  37 2e ef 3d 24 8a fd e1
                    74 63 eb bf ab b8 e4 d1  ab 50 2a 54 ec 00 64 e9
                    2f 78 19 66 0d 3f 27 cf  20 9e 66 7f ce 5a e2 e4
                    ac 99 c7 c9 38 18 f8 b2  51 07 22 df ed 97 f3 2e
                    3e 93 49 d4 c6 6c 9e a6  39 6d 74 44 62 a0 6b 42
                    c6 d5 ba 68 8e ac 3a 01  7b dd fc 8e 2c fc ad 27
                    cb 69 d3 cc dc a2 80 41  44 65 d3 ae 34 8c e0 f3
                    4a b2 fb 9c 61 83 71 31  2b 19 10 41 64 1c 23 7f
                    11 a5 d6 5c 84 4f 04 04  84 99 38 71 2b 95 9e d6
                    85 bc 5c 5d d6 45 ed 19  90 94 73 40 29 26 dc b4
                    0e 34 69 a1 59 41 e8 e2  cc a8 4b b6 08 46 36 a0
                    00 00"),

    CertificateVerify =
        hexstr2bin("0f 00 01 04 08 04 01 00  17 fe b5 33 ca 6d 00 7d
                    00 58 25 79 68 42 4b bc  3a a6 90 9e 9d 49 55 75
                    76 a5 20 e0 4a 5e f0 5f  0e 86 d2 4f f4 3f 8e b8
                    61 ee f5 95 22 8d 70 32  aa 36 0f 71 4e 66 74 13
                    92 6e f4 f8 b5 80 3b 69  e3 55 19 e3 b2 3f 43 73
                    df ac 67 87 06 6d cb 47  56 b5 45 60 e0 88 6e 9b
                    96 2c 4a d2 8d ab 26 ba  d1 ab c2 59 16 b0 9a f2
                    86 53 7f 68 4f 80 8a ef  ee 73 04 6c b7 df 0a 84
                    fb b5 96 7a ca 13 1f 4b  1c f3 89 79 94 03 a3 0c
                    02 d2 9c bd ad b7 25 12  db 9c ec 2e 5e 1d 00 e5
                    0c af cf 6f 21 09 1e bc  4f 25 3c 5e ab 01 a6 79
                    ba ea be ed b9 c9 61 8f  66 00 6b 82 44 d6 62 2a
                    aa 56 88 7c cf c6 6a 0f  38 51 df a1 3a 78 cf f7
                    99 1e 03 cb 2c 3a 0e d8  7d 73 67 36 2e b7 80 5b
                    00 b2 52 4f f2 98 a4 da  48 7c ac de af 8a 23 36
                    c5 63 1b 3e fa 93 5b b4  11 e7 53 ca 13 b0 15 fe
                    c7 e4 a7 30 f1 36 9f 9e"),

    BaseKey =
        hexstr2bin("a2 06 72 65 e7 f0 65 2a  92 3d 5d 72 ab 04 67 c4
                    61 32 ee b9 68 b6 a3 2d  31 1c 80 58 68 54 88 14"),

    VerifyData =
        hexstr2bin("ea 6e e1 76 dc cc 4a f1  85 9e 9e 4e 93 f7 97 ea
                    c9 a7 8c e4 39 30 1e 35  27 5a d4 3f 3c dd bd e3"),

    Messages = [CertificateVerify,
                Certificate,
                EncryptedExtensions,
                ServerHello,
                ClientHello],

    FinishedKey = tls_v1:finished_key(BaseKey, sha256),
    VerifyData = tls_v1:finished_verify_data(FinishedKey, sha256, Messages).


tls12_ssl_server_tls13_ssl_client() ->
     [{doc,"Test basic connection between TLS 1.2 server and TLS 1.3 client"}].

tls12_ssl_server_tls13_ssl_client(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2']}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {signature_algs_cert, [ecdsa_secp384r1_sha384,
                                         rsa_pss_rsae_sha256,
                                         rsa_pkcs1_sha256,
                                         {sha256,rsa},{sha256,dsa}]}|ClientOpts0],

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


tls13_basic_ssl_server_openssl_client() ->
     [{doc,"Test TLS 1.3 basic connection between ssl server and openssl s_client"}].

tls13_basic_ssl_server_openssl_client(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).

tls13_custom_groups_ssl_server_openssl_client() ->
    [{doc,"Test that ssl server can select a common group for key-exchange"}].

tls13_custom_groups_ssl_server_openssl_client(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [x448, secp256r1, secp384r1]}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{groups,"P-384:P-256:X25519"}|ClientOpts0],
    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).

tls13_hello_retry_request_ssl_server_openssl_client() ->
    [{doc,"Test that ssl server can request a new group when the client's first key share"
      "is not supported"}].

tls13_hello_retry_request_ssl_server_openssl_client(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {supported_groups, [x448, x25519]}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{groups,"P-256:X25519"}|ClientOpts0],
    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).

tls13_client_auth_empty_cert_alert_ssl_server_openssl_client() ->
     [{doc,"TLS 1.3: Test client authentication when client sends an empty certificate and fail_if_no_peer_cert is set to true."}].

tls13_client_auth_empty_cert_alert_ssl_server_openssl_client(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    %% Delete Client Cert and Key
    ClientOpts1 = proplists:delete(certfile, ClientOpts0),
    ClientOpts = proplists:delete(keyfile, ClientOpts1),

    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_server_alert(Server, certificate_required),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


tls13_client_auth_empty_cert_ssl_server_openssl_client() ->
     [{doc,"TLS 1.3: Test client authentication when client sends an empty certificate and fail_if_no_peer_cert is set to false."}].

tls13_client_auth_empty_cert_ssl_server_openssl_client(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    %% Delete Client Cert and Key
    ClientOpts1 = proplists:delete(certfile, ClientOpts0),
    ClientOpts = proplists:delete(keyfile, ClientOpts1),

    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, false}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


tls13_client_auth_ssl_server_openssl_client() ->
     [{doc,"TLS 1.3: Test client authentication."}].

tls13_client_auth_ssl_server_openssl_client(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


tls13_hrr_client_auth_empty_cert_alert_ssl_server_openssl_client() ->
     [{doc,"TLS 1.3 (HelloRetryRequest): Test client authentication when client sends an empty certificate and fail_if_no_peer_cert is set to true."}].

tls13_hrr_client_auth_empty_cert_alert_ssl_server_openssl_client(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    %% Delete Client Cert and Key
    ClientOpts1 = proplists:delete(certfile, ClientOpts0),
    ClientOpts2 = proplists:delete(keyfile, ClientOpts1),
    ClientOpts = [{groups,"P-256:X25519"}|ClientOpts2],

    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true},
                  {supported_groups, [x448, x25519]}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_server_alert(Server, certificate_required),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


tls13_hrr_client_auth_empty_cert_ssl_server_openssl_client() ->
     [{doc,"TLS 1.3 (HelloRetryRequest): Test client authentication when client sends an empty certificate and fail_if_no_peer_cert is set to false."}].

tls13_hrr_client_auth_empty_cert_ssl_server_openssl_client(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    %% Delete Client Cert and Key
    ClientOpts1 = proplists:delete(certfile, ClientOpts0),
    ClientOpts2 = proplists:delete(keyfile, ClientOpts1),
    ClientOpts = [{groups,"P-256:X25519"}|ClientOpts2],

    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, false},
                  {supported_groups, [x448, x25519]}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


tls13_hrr_client_auth_ssl_server_openssl_client() ->
     [{doc,"TLS 1.3 (HelloRetryRequest): Test client authentication."}].

tls13_hrr_client_auth_ssl_server_openssl_client(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = [{groups,"P-256:X25519"}|ClientOpts0],

    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {fail_if_no_peer_cert, true},
                  {supported_groups, [x448, x25519]}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


tls13_unsupported_sign_algo_client_auth_ssl_server_openssl_client() ->
     [{doc,"TLS 1.3: Test client authentication with unsupported signature_algorithm"}].

tls13_unsupported_sign_algo_client_auth_ssl_server_openssl_client(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  %% Skip rsa_pkcs1_sha256!
                  {signature_algs, [rsa_pkcs1_sha384, rsa_pkcs1_sha512]},
                  {fail_if_no_peer_cert, true}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_server_alert(Server, insufficient_security),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


%% Triggers Client Alert as openssl s_client does not have a certificate with a
%% signature algorithm supported by the server (signature_algorithms_cert extension
%% of CertificateRequest does not contain the algorithm of the client certificate).
tls13_unsupported_sign_algo_cert_client_auth_ssl_server_openssl_client() ->
     [{doc,"TLS 1.3: Test client authentication with unsupported signature_algorithm_cert"}].

tls13_unsupported_sign_algo_cert_client_auth_ssl_server_openssl_client(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {log_level, debug},
                  {verify, verify_peer},
                  {signature_algs, [rsa_pkcs1_sha256, rsa_pkcs1_sha384, rsa_pss_rsae_sha256]},
                  %% Skip rsa_pkcs1_sha256!
                  {signature_algs_cert, [rsa_pkcs1_sha384, rsa_pkcs1_sha512]},
                  {fail_if_no_peer_cert, true}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_server_alert(Server, certificate_required),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


tls13_connection_information() ->
     [{doc,"Test the API function ssl:connection_information/1 in a TLS 1.3 connection"}].

tls13_connection_information(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    %% Set versions
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']}|ServerOpts0],
    {_ClientNode, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE, connection_information_result, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_basic_client(openssl, 'tlsv1.3', Port, ClientOpts),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close_port(Client).


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
send_recv_result(Socket) ->
    ssl:send(Socket, "Hello world"),
    {ok,"Hello world"} = ssl:recv(Socket, 11),
    ok.
tcp_send_recv_result(Socket) ->
    gen_tcp:send(Socket, "Hello world"),
    {ok,"Hello world"} = gen_tcp:recv(Socket, 11),
    ok.

basic_verify_test_no_close(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    {Server, Client}.

basic_test(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

prf_create_plan(TlsVersions, PRFs, Results) ->
    lists:foldl(fun(Ver, Acc) ->
                        A = prf_ciphers_and_expected(Ver, PRFs, Results),
                        [A|Acc]
                end, [], TlsVersions).
prf_ciphers_and_expected(TlsVer, PRFs, Results) ->
    case TlsVer of
        TlsVer when TlsVer == sslv3 orelse TlsVer == tlsv1
                    orelse TlsVer == 'tlsv1.1' orelse TlsVer == 'dtlsv1' ->
            Ciphers = ssl:cipher_suites(),
            {_, Expected} = lists:keyfind(md5sha, 1, Results),
            [[{tls_ver, TlsVer}, {ciphers, Ciphers}, {expected, Expected}, {prf, md5sha}]];
        TlsVer when  TlsVer == 'tlsv1.2' orelse  TlsVer == 'dtlsv1.2'->
            lists:foldl(
              fun(PRF, Acc) ->
                      Ciphers = prf_get_ciphers(TlsVer, PRF),
                      case Ciphers of
                          [] ->
                              ct:log("No ciphers for PRF algorithm ~p. Skipping.", [PRF]),
                              Acc;
                          Ciphers ->
                              {_, Expected} = lists:keyfind(PRF, 1, Results),
                              [[{tls_ver, TlsVer}, {ciphers, Ciphers}, {expected, Expected},
                                {prf, PRF}] | Acc]
                      end
              end, [], PRFs)
    end.
prf_get_ciphers(_, PRF) ->
    lists:filter(
      fun(C) when tuple_size(C) == 4 andalso
                  element(4, C) == PRF -> 
              true;
         (_) -> 
              false
      end, 
      ssl:cipher_suites()).
prf_run_test(_, TlsVer, [], _, Prf) ->
    ct:fail({error, cipher_list_empty, TlsVer, Prf});
prf_run_test(Config, TlsVer, Ciphers, Expected, Prf) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    BaseOpts = [{active, true}, {versions, [TlsVer]}, {ciphers, Ciphers}, {protocol, tls_or_dtls(TlsVer)}],
    ServerOpts = BaseOpts ++ proplists:get_value(server_opts, Config),
    ClientOpts = BaseOpts ++ proplists:get_value(client_opts, Config),
    Server = ssl_test_lib:start_server(
               [{node, ServerNode}, {port, 0}, {from, self()},
                {mfa, {?MODULE, prf_verify_value, [TlsVer, Expected, Prf]}},
                {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client(
               [{node, ClientNode}, {port, Port},
                {host, Hostname}, {from, self()},
                {mfa, {?MODULE, prf_verify_value, [TlsVer, Expected, Prf]}},
                {options, ClientOpts}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
prf_verify_value(Socket, TlsVer, Expected, Algo) ->
    Ret = ssl:prf(Socket, <<>>, <<>>, [<<>>], 16),
    case TlsVer of
        sslv3 ->
            case Ret of
                {error, undefined} -> ok;
                _ ->
                    {error, {expected, {error, undefined},
                             got, Ret, tls_ver, TlsVer, prf_algorithm, Algo}}
            end;
        _ ->
            case Ret of
                {ok, Expected} -> ok;
                {ok, Val} -> {error, {expected, Expected, got, Val, tls_ver, TlsVer,
                                      prf_algorithm, Algo}}
            end
    end.

send_recv_result_timeout_client(Socket) ->
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

recv_close(Socket) ->
    {error, closed} = ssl:recv(Socket, 11),
    receive
	{_,{error,closed}} ->
	    error_extra_close_sent_to_user_process
    after 500 ->
	    ok
    end.


send_recv_result_active_rizzo(Socket) ->
    ssl:send(Socket, "Hello world"),
    "Hello world" = ssl_test_lib:active_recv(Socket, 11),
    ok.

send_recv_result_active_no_rizzo(Socket) ->
    ssl:send(Socket, "Hello world"),
    "Hello world" = ssl_test_lib:active_recv(Socket, 11),
    ok.


ssl_active_recv(N) ->
    ssl_active_recv(N, []).

ssl_active_recv(0, Acc) ->
    Acc;
ssl_active_recv(N, Acc) ->
    receive 
	{ssl, _, Bytes} ->
            ssl_active_recv(N-length(Bytes),  Acc ++ Bytes)
    end.

result_ok(_Socket) ->
    ok.

renegotiate(Socket, Data) ->
    ct:log("Renegotiating ~n", []),
    Result = ssl:renegotiate(Socket),
    ct:log("Result ~p~n", [Result]),
    ssl:send(Socket, Data),
    case Result of
	ok ->
	    ok;
	Other ->
	    Other
    end.

renegotiate_reuse_session(Socket, Data) ->
    %% Make sure session is registered
    ct:sleep(?SLEEP),
    renegotiate(Socket, Data).

renegotiate_immediately(Socket) ->
    _ = ssl_test_lib:active_recv(Socket, 11),
    ok = ssl:renegotiate(Socket),  
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    ct:sleep(?RENEGOTIATION_DISABLE_TIME + ?SLEEP),
    ok = ssl:renegotiate(Socket),
    ct:log("Renegotiated again"),
    ssl:send(Socket, "Hello world"),
    ok.

renegotiate_rejected(Socket) ->
    _ = ssl_test_lib:active_recv(Socket, 11),
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    ct:sleep(?RENEGOTIATION_DISABLE_TIME +1),
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    ct:log("Failed to renegotiate again"),
    ssl:send(Socket, "Hello world"),
    ok.

rizzo_add_mitigation_option(Value, Config) ->
    lists:foldl(fun(Opt, Acc) ->
                    case proplists:get_value(Opt, Acc) of
                      undefined -> Acc;
                      C ->
                        N = lists:keystore(beast_mitigation, 1, C,
                                           {beast_mitigation, Value}),
                        lists:keystore(Opt, 1, Acc, {Opt, N})
                    end
                end, Config,
                [client_opts, client_dsa_opts, server_opts, server_dsa_opts,
                 server_ecdsa_opts, server_ecdh_rsa_opts]).
    
new_config(PrivDir, ServerOpts0) ->
    CaCertFile = proplists:get_value(cacertfile, ServerOpts0),
    CertFile = proplists:get_value(certfile, ServerOpts0),
    KeyFile = proplists:get_value(keyfile, ServerOpts0),
    NewCaCertFile = filename:join(PrivDir, "new_ca.pem"),
    NewCertFile = filename:join(PrivDir, "new_cert.pem"),
    NewKeyFile = filename:join(PrivDir, "new_key.pem"),
    file:copy(CaCertFile, NewCaCertFile),
    file:copy(CertFile, NewCertFile),
    file:copy(KeyFile, NewKeyFile),
    ServerOpts1 = proplists:delete(cacertfile, ServerOpts0),
    ServerOpts2 = proplists:delete(certfile, ServerOpts1),
    ServerOpts = proplists:delete(keyfile, ServerOpts2),

    {ok, PEM} = file:read_file(NewCaCertFile),
    ct:log("CA file content: ~p~n", [public_key:pem_decode(PEM)]),

    [{cacertfile, NewCaCertFile}, {certfile, NewCertFile},
     {keyfile, NewKeyFile} | ServerOpts].

session_cache_process(_Type,Config) when is_list(Config) ->
    reuse_session(Config).

init([Type]) ->
    ets:new(ssl_test, [named_table, public, set]),
    ets:insert(ssl_test, {type, Type}),
    case Type of
	list ->
	    spawn(fun() -> session_loop([]) end);
	mnesia ->
	    mnesia:start(),
	    {atomic,ok} = mnesia:create_table(sess_cache, []),
	    sess_cache
    end.

session_cb() ->
    [{type, Type}] = ets:lookup(ssl_test, type),
    Type.

terminate(Cache) ->
    case session_cb() of
	list ->
	    Cache ! terminate;
	mnesia ->
	    catch {atomic,ok} = 
		mnesia:delete_table(sess_cache)
    end.

lookup(Cache, Key) ->        
    case session_cb() of
	list ->
	    Cache ! {self(), lookup, Key},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    case mnesia:transaction(fun() -> 
					    mnesia:read(sess_cache, 
							Key, read) 
				    end) of
		{atomic, [{sess_cache, Key, Value}]} -> 
		    Value;
		_ -> 
		    undefined
	    end
	end.

update(Cache, Key, Value) ->
    case session_cb() of
	list ->
	    Cache ! {update, Key, Value};
	mnesia ->
	    {atomic, ok} = 
		mnesia:transaction(fun() -> 
					   mnesia:write(sess_cache, 
							{sess_cache, Key, Value}, write) 
				   end)
    end.

delete(Cache, Key) -> 
    case session_cb() of
	list ->
	    Cache ! {delete, Key};
	mnesia ->
	    {atomic, ok} = 
		mnesia:transaction(fun() -> 
					   mnesia:delete(sess_cache, Key) 
				   end)
    end.

foldl(Fun, Acc, Cache) -> 
    case session_cb() of
	list ->
	    Cache ! {self(),foldl,Fun,Acc},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    Foldl = fun() ->
			    mnesia:foldl(Fun, Acc, sess_cache)
		    end,
	    {atomic, Res} = mnesia:transaction(Foldl),
	    Res
    end.
    
select_session(Cache, PartialKey) ->
    case session_cb() of
	list ->
	    Cache ! {self(),select_session, PartialKey},
	    receive 
		{Cache, Res} -> 
		    Res 
	    end;
	mnesia ->
	    Sel = fun() ->
			  mnesia:select(Cache,
					[{{sess_cache,{PartialKey,'$1'}, '$2'},
					  [],['$$']}])
		  end,
	    {atomic, Res} = mnesia:transaction(Sel),
	    Res
    end.

session_loop(Sess) ->
    receive 
	terminate ->
	    ok;
	{Pid, lookup, Key} ->
	    case lists:keysearch(Key,1,Sess) of
		{value, {Key,Value}} ->
		    Pid ! {self(), Value};
		_ -> 
		    Pid ! {self(), undefined}
	    end,
	    session_loop(Sess);
	{update, Key, Value} ->
	    NewSess = [{Key,Value}| lists:keydelete(Key,1,Sess)],
	    session_loop(NewSess);
	{delete, Key} ->
	    session_loop(lists:keydelete(Key,1,Sess));
	{Pid,foldl,Fun,Acc} ->
	    Res = lists:foldl(Fun, Acc,Sess),
	    Pid ! {self(), Res},
	    session_loop(Sess);
	{Pid,select_session,PKey} ->
	    Sel = fun({{PKey0, Id},Session}, Acc) when PKey == PKey0 -> 
			  [[Id, Session]|Acc];
		     (_,Acc) -> 
			  Acc
		  end, 
	    Sessions = lists:foldl(Sel, [], Sess),
	    Pid ! {self(), Sessions},
	    session_loop(Sess)
    end.
	    

erlang_ssl_receive(Socket, Data) ->
    case ssl_test_lib:active_recv(Socket, length(Data)) of
        Data ->
            ok;
        Other ->
            ct:fail({{expected, Data}, {got, Other}})
    end.

receive_msg(_) ->
    receive
	Msg ->
	   Msg
    end.

controlling_process_result(Socket, Pid, Msg) ->
    ok = ssl:controlling_process(Socket, Pid),
    %% Make sure other side has evaluated controlling_process
    %% before message is sent
    ct:sleep(?SLEEP),
    ssl:send(Socket, Msg),
    no_result_msg.


controller_dies_result(_Socket, _Pid, _Msg) ->
    receive Result -> Result end.

get_close(Pid, Where) ->
    receive
	{'EXIT', Pid, _Reason} ->
	    receive
		{_, {ssl_closed, Socket}} ->
		    ct:log("Socket closed ~p~n",[Socket]);
		Unexpected ->
		    ct:log("Unexpected ~p~n",[Unexpected]),
		    ct:fail({line, ?LINE-1})
	    after 5000 ->
		    ct:fail({timeout, {line, ?LINE, Where}})
	    end;
	Unexpected ->
	    ct:log("Unexpected ~p~n",[Unexpected]),
	    ct:fail({line, ?LINE-1})
    after 5000 ->
	    ct:fail({timeout, {line, ?LINE, Where}})
    end.

run_send_recv_rizzo(Ciphers, Config, Version, Mfa) ->
    Result =  lists:map(fun(Cipher) ->
				rizzo_test(Cipher, Config, Version, Mfa) end,
			Ciphers),
    case lists:flatten(Result) of
	[] ->
	    ok;
	Error ->
	    ct:log("Cipher suite errors: ~p~n", [Error]),
	    ct:fail(cipher_suite_failed_see_test_case_log)
    end.

rizzo_test(Cipher, Config, Version, Mfa) ->
   {ClientOpts, ServerOpts} = client_server_opts(Cipher, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, Mfa},
			   {options, [{active, true}, {ciphers, [Cipher]},
				       {versions, [Version]}
				      | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, Mfa},
			   {options, [{active, true}, {ciphers, [Cipher]}| ClientOpts]}]),

    Result = ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    case Result of
	ok ->
	    [];
	Error ->
	    [{Cipher, Error}]
    end.

client_server_opts(#{key_exchange := KeyAlgo}, Config)
  when KeyAlgo == rsa orelse
       KeyAlgo == dhe_rsa orelse
       KeyAlgo == ecdhe_rsa orelse
       KeyAlgo == rsa_psk orelse
       KeyAlgo == srp_rsa ->
    {ssl_test_lib:ssl_options(client_opts, Config),
     ssl_test_lib:ssl_options(server_opts, Config)};
client_server_opts(#{key_exchange := KeyAlgo}, Config) when KeyAlgo == dss orelse KeyAlgo == dhe_dss ->
    {ssl_test_lib:ssl_options(client_dsa_opts, Config),
     ssl_test_lib:ssl_options(server_dsa_opts, Config)};
client_server_opts(#{key_exchange := KeyAlgo}, Config) when KeyAlgo == ecdh_ecdsa orelse KeyAlgo == ecdhe_ecdsa ->
    {ssl_test_lib:ssl_options(client_opts, Config),
     ssl_test_lib:ssl_options(server_ecdsa_opts, Config)};
client_server_opts(#{key_exchange := KeyAlgo}, Config) when KeyAlgo == ecdh_rsa ->
    {ssl_test_lib:ssl_options(client_opts, Config),
     ssl_test_lib:ssl_options(server_ecdh_rsa_opts, Config)}.

connection_information_result(Socket) ->
    {ok, Info = [_ | _]} = ssl:connection_information(Socket),
    case  length(Info) > 3 of
	true -> 
	    %% Atleast one ssl_option() is set
	    ct:log("Info ~p", [Info]),
	    ok;
	false ->
	    ct:fail(no_ssl_options_returned)
    end.

connection_info_result(Socket) ->
    {ok, Info} = ssl:connection_information(Socket, [protocol, selected_cipher_suite]),
    {ok, {proplists:get_value(protocol, Info), proplists:get_value(selected_cipher_suite, Info)}}.

protocol_info_result(Socket) ->
    {ok, [{protocol, PVersion}]} = ssl:connection_information(Socket, [protocol]),
    {ok, PVersion}.

version_info_result(Socket) ->
    {ok, [{version, Version}]} = ssl:connection_information(Socket, [version]),
    {ok, Version}.

secret_connection_info_result(Socket) ->
    {ok, [{client_random, ClientRand}, {server_random, ServerRand}, {master_secret, MasterSecret}]} 
        = ssl:connection_information(Socket, [client_random, server_random, master_secret]),
    is_binary(ClientRand) andalso is_binary(ServerRand) andalso is_binary(MasterSecret). 
    
connect_dist_s(S) ->
    Msg = term_to_binary({erlang,term}),
    ok = ssl:send(S, Msg).

connect_dist_c(S) ->
    Test = binary_to_list(term_to_binary({erlang,term})),
    {ok, Test} = ssl:recv(S, 0, 10000),
    ok.

tls_downgrade_result(Socket, Pid) ->
    ok = ssl_test_lib:send_recv_result(Socket),
    Pid ! {self(), ready},
    receive 
        go ->
            ok
    end,
    case ssl:close(Socket, {self(), 10000})  of
	{ok, TCPSocket} -> 
            inet:setopts(TCPSocket, [{active, true}]),
	    gen_tcp:send(TCPSocket, "Downgraded"),
            receive 
                {tcp, TCPSocket, <<"Downgraded">>} ->
	             ok;
                {tcp_closed, TCPSocket} ->
                    ct:fail("Peer timed out, downgrade aborted"),
	            ok;
	        Other ->
                    {error, Other}
            end;
	{error, timeout} ->
	    ct:fail("Timed out, downgrade aborted"),
	    ok;
	Fail ->
	    {error, Fail}
    end.

tls_close(Socket) ->
    ok = ssl_test_lib:send_recv_result(Socket),
    case ssl:close(Socket, 5000) of
        ok ->
            ok;
        {error, closed} ->
            ok;
        Other ->
            ct:fail(Other)
    end.

 %% First two clauses handles 1/n-1 splitting countermeasure Rizzo/Duong-Beast
treashold(N, {3,0}) ->
    (N div 2) + 1;
treashold(N, {3,1}) ->
    (N div 2) + 1;
treashold(N, _) ->
    N + 1.

get_invalid_inet_option(Socket) ->
    {error, {options, {socket_options, foo, _}}} = ssl:getopts(Socket, [foo]),
    ok.

tls_shutdown_result(Socket, server) ->
    ssl:send(Socket, "Hej"),
    ok = ssl:shutdown(Socket, write),
    {ok, "Hej hopp"} = ssl:recv(Socket, 8),
    ok;

tls_shutdown_result(Socket, client) ->
    ssl:send(Socket, "Hej hopp"),
    ok = ssl:shutdown(Socket, write),
    {ok, "Hej"} = ssl:recv(Socket, 3),
    ok.

tls_shutdown_write_result(Socket, server) ->
    ct:sleep(?SLEEP),
    ssl:shutdown(Socket, write);
tls_shutdown_write_result(Socket, client) ->
    ssl:recv(Socket, 0).

dummy(_Socket) ->
    %% Should not happen as the ssl connection will not be established
    %% due to fatal handshake failiure
    exit(kill).

tls_shutdown_both_result(Socket, server) ->
    ct:sleep(?SLEEP),
    ssl:shutdown(Socket, read_write);
tls_shutdown_both_result(Socket, client) ->
    ssl:recv(Socket, 0).

peername_result(S) ->
    ssl:peername(S).

version_option_test(Config, Version) ->
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
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
    
    ct:log("Testcase ~p, Client ~p  Server ~p ~n",
	   [self(), Client, Server]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

try_recv_active(Socket) ->
    ssl:send(Socket, "Hello world"),
    {error, einval} = ssl:recv(Socket, 11),
    ok.
try_recv_active_once(Socket) ->
    {error, einval} = ssl:recv(Socket, 11),
    ok.

    
wait_for_send(Socket) ->
    %% Make sure TLS process processed send message event
    _ = ssl:connection_information(Socket).

tls_or_dtls('dtlsv1') ->
    dtls;
tls_or_dtls('dtlsv1.2') ->
    dtls;
tls_or_dtls(_) ->
    tls.

hexstr2int(S) ->
    B = hexstr2bin(S),
    Bits = size(B) * 8,
    <<Integer:Bits/integer>> = B,
    Integer.

hexstr2bin(S) when is_binary(S) ->
    hexstr2bin(S, <<>>);
hexstr2bin(S) ->
    hexstr2bin(list_to_binary(S), <<>>).
%%
hexstr2bin(<<>>, Acc) ->
    Acc;
hexstr2bin(<<C,T/binary>>, Acc) when C =:= 32;   %% SPACE
                                     C =:= 10;   %% LF
                                     C =:= 13 -> %% CR
    hexstr2bin(T, Acc);
hexstr2bin(<<X,Y,T/binary>>, Acc) ->
    I = hex2int(X) * 16 + hex2int(Y),
    hexstr2bin(T, <<Acc/binary,I>>).

hex2int(C) when $0 =< C, C =< $9 ->
    C - $0;
hex2int(C) when $A =< C, C =< $F ->
    C - $A + 10;
hex2int(C) when $a =< C, C =< $f ->
    C - $a + 10.
