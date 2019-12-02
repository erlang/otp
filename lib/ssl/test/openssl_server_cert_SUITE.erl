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
-module(openssl_server_cert_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

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
     {'sslv3', [], ssl_protocol_groups()},
     {'dtlsv1.2', [], pre_tls_1_3_protocol_groups()},
     {'dtlsv1', [], pre_tls_1_3_protocol_groups()},
     {rsa, [], all_version_tests()},
     {ecdsa, [], all_version_tests()},
     {dsa, [], all_version_tests()},
     {rsa_1_3, [], all_version_tests() ++ tls_1_3_tests()},
      %% TODO: Create proper conf of openssl server
      %%++ [unsupported_sign_algo_client_auth,
      %% unsupported_sign_algo_cert_client_auth]},
     {ecdsa_1_3, [], all_version_tests() ++ tls_1_3_tests()}
    ].

protocol_groups() ->
    [{group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
     ].

ssl_protocol_groups() ->
    [{group, rsa},
     {group, dsa}].

pre_tls_1_3_protocol_groups() ->
    [{group, rsa},
     {group, ecdsa},
     {group, dsa}].

tls_1_3_protocol_groups() ->
    [{group, rsa_1_3},
     {group, ecdsa_1_3}].

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
     %%invalid_signature_client
    ].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
            Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:unload(ssl),
    application:stop(crypto).

init_per_group(openssl_server, Config0) ->
    Config = proplists:delete(server_type, proplists:delete(client_type, Config0)),
    [{client_type, erlang}, {server_type, openssl} | Config];    
init_per_group(rsa = Group, Config0) ->
    Config = ssl_test_lib:make_rsa_cert(Config0),
    COpts = proplists:get_value(client_rsa_opts, Config),
    SOpts = proplists:get_value(server_rsa_opts, Config),
    %% Make sure _rsa* suite is choosen by ssl_test_lib:start_server
    Version = proplists:get_value(version,Config),
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
    %% Make sure _rsa* suite is choosen by ssl_test_lib:start_server
    Version = proplists:get_value(version,Config),
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
init_per_group(ecdsa = Group, Config0) ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso (lists:member(ecdh, PKAlg) orelse 
                                             lists:member(dh, PKAlg)) of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            %% Make sure ecdh* suite is choosen by ssl_test_lib:start_server
            Version = proplists:get_value(version,Config),
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
                                             lists:member(dh, PKAlg)) of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_opts, Config),
            SOpts = proplists:get_value(server_ecdsa_opts, Config),
            %% Make sure ecdh* suite is choosen by ssl_test_lib:start_server
            Version = proplists:get_value(version,Config),
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
init_per_group(Group, Config0) when Group == dsa ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(dss, PKAlg) andalso lists:member(dh, PKAlg) of
        true ->
            Config = ssl_test_lib:make_dsa_cert(Config0),    
            COpts = proplists:get_value(client_dsa_opts, Config),
            SOpts = proplists:get_value(server_dsa_opts, Config),
            %% Make sure dhe_dss* suite is choosen by ssl_test_lib:start_server
            Version = proplists:get_value(version,Config),
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
                                  [{client_cert_opts, [{ciphers, Ciphers} | COpts]}, 
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
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    case ssl_test_lib:check_sane_openssl_version(GroupName) of
		true ->
		    [{version, GroupName} 
                     | ssl_test_lib:init_tls_version(GroupName, Config)];
		false ->
		    {skip, "Missing openssl support"}
	    end;
	_ ->
	    ssl:start(),
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
client_auth_allow_partial_chain() ->
    ssl_cert_tests:client_auth_allow_partial_chain().
client_auth_allow_partial_chain(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_allow_partial_chain(Config).
%%--------------------------------------------------------------------
client_auth_do_not_allow_partial_chain() ->
   ssl_cert_tests:client_auth_do_not_allow_partial_chain().
client_auth_do_not_allow_partial_chain(Config) when is_list(Config) ->
    ssl_cert_tests:client_auth_do_not_allow_partial_chain(Config).

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
invalid_signature_client() ->
    ssl_cert_tests:invalid_signature_client().
invalid_signature_client(Config) when is_list(Config) ->
    ssl_cert_tests:invalid_signature_client(Config).
%%--------------------------------------------------------------------
invalid_signature_server() ->
    ssl_cert_tests:invalid_signature_client().
invalid_signature_server(Config) when is_list(Config) ->
    ssl_cert_tests:invalid_signature_client(Config).

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
