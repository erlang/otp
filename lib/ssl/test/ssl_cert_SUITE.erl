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
-module(ssl_cert_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.3', [], tls_1_3_protocol_groups()}, 
     {'tlsv1.2', [], tls_1_2_protocol_groups()},
     {'tlsv1.1', [], ssl_protocol_groups()},
     {'tlsv1', [], ssl_protocol_groups()},
     {'sslv3', [], ssl_protocol_groups()},
     {'dtlsv1.2', [], tls_1_2_protocol_groups()},
     {'dtlsv1', [], ssl_protocol_groups()},
     {rsa, [], all_version_tests() ++ rsa_tests() ++ pre_tls_1_3_rsa_tests()},
     {ecdsa, [], all_version_tests()},
     {dsa, [], all_version_tests()},
     {rsa_1_3, [], all_version_tests() ++ rsa_tests() ++ tls_1_3_tests() ++ tls_1_3_rsa_tests()},
     {ecdsa_1_3, [], all_version_tests() ++ tls_1_3_tests()}
    ].

ssl_protocol_groups() ->
    [{group, rsa},
     {group, dsa}].

tls_1_2_protocol_groups() ->
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

pre_tls_1_3_rsa_tests() ->
    [
     key_auth_ext_sign_only
    ].

rsa_tests() ->
   [
    longer_chain
   ].

tls_1_3_rsa_tests() ->
     [
      unsupported_sign_algo_client_auth,
      unsupported_sign_algo_cert_client_auth
     ].

all_version_tests() ->
    [
     no_auth,
     auth,
     client_auth_empty_cert_accepted,
     client_auth_empty_cert_rejected,
     client_auth_partial_chain,
     client_auth_allow_partial_chain,
     client_auth_do_not_allow_partial_chain,
     client_auth_partial_chain_fun_fail,
     missing_root_cert_no_auth,
     missing_root_cert_auth,
     missing_root_cert_auth_user_verify_fun_accept,
     missing_root_cert_auth_user_verify_fun_reject,
     verify_fun_always_run_client,
     verify_fun_always_run_server,
     incomplete_chain_auth,
     invalid_signature_client,
     invalid_signature_server,
     critical_extension_auth,
     critical_extension_client_auth,
     critical_extension_no_auth,
     extended_key_usage_auth,
     extended_key_usage_client_auth,
     cert_expired,
     client_auth_once,
     no_auth_key_identifier_ext,
     no_auth_key_identifier_ext_keyEncipherment
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

init_per_group(Group, Config0) when Group == rsa;
                                    Group == rsa_1_3 ->
    Config = ssl_test_lib:make_rsa_cert(Config0),
    COpts = proplists:get_value(client_rsa_opts, Config),
    SOpts = proplists:get_value(server_rsa_opts, Config),
    [{cert_key_alg, rsa} |
     lists:delete(cert_key_alg,                                 
                  [{client_cert_opts, COpts}, 
                   {server_cert_opts, SOpts} | 
                   lists:delete(server_cert_opts, 
                                lists:delete(client_cert_opts, Config))])];
init_per_group(Group, Config0) when Group == ecdsa;
                                    Group == ecdsa_1_3 ->

    PKAlg = crypto:supports(public_keys),
    case lists:member(ecdsa, PKAlg) andalso (lists:member(ecdh, PKAlg) orelse lists:member(dh, PKAlg)) of
        true ->
            Config = ssl_test_lib:make_ecdsa_cert(Config0),
            COpts = proplists:get_value(client_ecdsa_opts, Config),
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

init_per_group(Group, Config0) when Group == dsa ->
    PKAlg = crypto:supports(public_keys),
    case lists:member(dss, PKAlg) andalso lists:member(dh, PKAlg) of
        true ->
            Config = ssl_test_lib:make_dsa_cert(Config0),    
            COpts = proplists:get_value(client_dsa_opts, Config),
            SOpts = proplists:get_value(server_dsa_opts, Config),
            [{cert_key_alg, dsa} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, COpts}, 
                           {server_cert_opts, SOpts} | 
                           lists:delete(server_cert_opts, 
                                        lists:delete(client_cert_opts, Config))])];
        false ->
            {skip, "Missing DSS crypto support"}
    end;    
init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    [{client_type, erlang},
                     {server_type, erlang}, {version, GroupName} 
                     | ssl_test_lib:init_tls_version(GroupName, Config)];
		false ->
		    {skip, "Missing crypto support"}
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
    ct:timetrap({seconds, 10}),
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
missing_root_cert_auth() ->
    [{doc,"Must have ROOT certs to be able to verify verify peer"}].
missing_root_cert_auth(Config) when is_list(Config) ->
    ServerOpts =  proplists:delete(cacertfile, ssl_test_lib:ssl_options(server_cert_opts, Config)),
    {ClientNode, ServerNode, _} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, [{verify, verify_peer}
							 | ServerOpts]}]),

    ssl_test_lib:check_result(Server, {error, {options, {cacertfile, ""}}}),
    
    ClientOpts =  proplists:delete(cacertfile, ssl_test_lib:ssl_options(client_cert_opts, Config)),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, 0},
					      {from, self()},
					      {options, [{verify, verify_peer}
							 | ClientOpts]}]),

    ssl_test_lib:check_result(Client, {error, {options, {cacertfile, ""}}}).
    
%%--------------------------------------------------------------------
missing_root_cert_auth_user_verify_fun_accept() ->
    [{doc, "Test that the client succeds if the ROOT CA is unknown in verify_peer mode"
     " with a verify_fun that accepts the unknown CA error"}].

missing_root_cert_auth_user_verify_fun_accept(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_cert_opts, Config),
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
    ClientOpts = ssl_test_lib:ssl_options([{verify, verify_peer},
                                            {verify_fun, FunAndState}], Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
missing_root_cert_auth_user_backwardscompatibility_verify_fun_accept() ->
    [{doc, "Test old style verify fun"}].

missing_root_cert_auth_user_backwardscompatibility_verify_fun_accept(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_cert_opts, Config),
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
    
    ClientOpts = ssl_test_lib:ssl_options([{verify, verify_peer},
                                           {verify_fun, VerifyFun}], Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
missing_root_cert_auth_user_verify_fun_reject() ->
    [{doc, "Test that the client fails if the ROOT CA is unknown in verify_peer mode"
     " with a verify_fun that rejects the unknown CA error"}].

missing_root_cert_auth_user_verify_fun_reject(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_cert_opts, Config),
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
    ClientOpts =  ssl_test_lib:ssl_options([{verify, verify_peer},
                                            {verify_fun, FunAndState}], Config),
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, unknown_ca).
%%--------------------------------------------------------------------
incomplete_chain_auth() ->
    [{doc,"Test that we can verify an incompleat chain when we have the certs to rebuild it"}].
incomplete_chain_auth(Config) when is_list(Config) ->
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain, DefaultCertConf},
                                                                         {client_chain, DefaultCertConf}]),   
    [ServerRoot| _] = ServerCas = proplists:get_value(cacerts, ServerOpts0),
    ClientCas = proplists:get_value(cacerts, ClientOpts0),
    ClientOpts = ssl_test_lib:ssl_options([{verify, verify_peer},
                                            {cacerts,  ServerCas ++ ClientCas} | 
                                            proplists:delete(cacerts, ClientOpts0)], Config),
    ServerOpts = ssl_test_lib:ssl_options([{verify, verify_peer},
                                           {cacerts, [ServerRoot]} |  
                                           proplists:delete(cacerts, ServerOpts0)], Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
verify_fun_always_run_client() ->
    [{doc,"Verify that user verify_fun is always run (for valid and "
      "valid_peer not only unknown_extension)"}].

verify_fun_always_run_client(Config) when is_list(Config) ->
    ClientOpts =  ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_cert_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options, ServerOpts}]),
    Port  = ssl_test_lib:inet_port(Server),

    %% If user verify fun is called correctly we fail the connection.
    %% otherwise we cannot tell this case apart form where we miss
    %% to call users verify fun
    FunAndState =  {fun(_,{extension, _}, UserState) ->
			    {unknown, UserState};
		       (_, valid, [ChainLen]) ->
			    {valid, [ChainLen + 1]};
		       (_, valid_peer, [1]) ->
			    {fail, "verify_fun_was_always_run"};
		       (_, valid_peer, UserState) ->
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
    ClientOpts =  ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts =  ssl_test_lib:ssl_options(server_cert_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% If user verify fun is called correctly we fail the connection.
    %% otherwise we cannot tell this case apart form where we miss
    %% to call users verify fun
    FunAndState =  {fun(_,{extension, _}, UserState) ->
			    {unknown, UserState};
		       (_, valid, [ChainLen]) ->
			    {valid, [ChainLen + 1]};
		       (_, valid_peer, [1]) ->
			    {fail, "verify_fun_was_always_run"};
		       (_, valid_peer, UserState) ->
			    {valid, UserState}
		    end, [0]},

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options,
					       [{verify, verify_peer},
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
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
    Ext = x509_test:extensions([{{2,16,840,1,113730,1,1}, <<3,2,6,192>>, true}]),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0}  = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                         [{server_chain, 
                                                                           [[],[],[{extensions, Ext}]]},
                                                                          {client_chain, DefaultCertConf}]),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),              
 
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {ssl_test_lib,  no_result, []}},
                {options, [{verify, verify_none} | ServerOpts]}]),
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
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),              
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error(
               [{node, ServerNode}, {port, 0},
                {from, self()},
                {mfa, {ssl_test_lib, no_result, []}},
                {options, [{verify, verify_peer} | ServerOpts]}]),
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
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
    Ext = x509_test:extensions([{{2,16,840,1,113730,1,1}, <<3,2,6,192>>, true}]),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0}  = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                            [{server_chain, 
                                                                              [[],[], [{extensions, Ext}]]},
                                                                             {client_chain, DefaultCertConf}]),
    ClientOpts = [{verify, verify_none} | ssl_test_lib:ssl_options(ClientOpts0, Config)],
    ServerOpts = [{verify, verify_none} | ssl_test_lib:ssl_options(ServerOpts0, Config)],            
    
     ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).


%%--------------------------------------------------------------------
extended_key_usage_auth() ->
    [{doc,"Test cert that has a critical extended_key_usage extension in server cert"}].

extended_key_usage_auth(Config) when is_list(Config) -> 
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
    Ext = x509_test:extensions([{?'id-ce-extKeyUsage',
                                 [?'id-kp-serverAuth'], true}]),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain, 
                                                                          [[],[], [{extensions, Ext}]]},
                                                                         {client_chain, DefaultCertConf}
                                                                        ]),
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),                                                     

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
			   {options, [{verify, verify_none} | ServerOpts]}]),
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
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),        
   
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{verify, verify_peer} | ServerOpts]}]),
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
    DefaultCertConf = ssl_test_lib:default_cert_chain_conf(),
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
    ClientOpts = ssl_test_lib:ssl_options(ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(ServerOpts0, Config),                                                     
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {options, [{verify, verify_peer} | ClientOpts]}]),    
    
    ssl_test_lib:check_client_alert(Server, Client, certificate_expired).

%%--------------------------------------------------------------------
client_auth_once() ->
    [{doc,"Test server option verify_client_once"}].

client_auth_once(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_cert_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, [{verify, verify_peer},
						   {verify_client_once, true}
						   | ServerOpts]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client0 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                         {host, Hostname},
                                         {from, self()},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server, ok, Client0, ok),
    Server ! {listen, {mfa, {ssl_test_lib, send_recv_result_active, []}}},
    ssl_test_lib:close(Client0),
    Client1 = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                         {host, Hostname},
                                         {from, self()},
                                         {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                         {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Client1, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client1).

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
    ClientOpts = [{verify, verify_peer} | ssl_test_lib:ssl_options(ClientOpts0, Config)],
    ServerOpts = [{verify, verify_peer} | ssl_test_lib:ssl_options(ServerOpts0, Config)],        
 
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
    ClientOpts =   [{verify, verify_peer} | ssl_test_lib:ssl_options(ClientOpts0, Config)],
    ServerOpts =   [{verify, verify_peer} | ssl_test_lib:ssl_options(ServerOpts0, Config)],  
    
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
    ClientOpts =  [{verify, verify_peer} | ssl_test_lib:ssl_options(ClientOpts0, Config)], 
    ServerOpts = [{verify, verify_peer}, {ciphers, 
                                          ssl_test_lib:rsa_non_signed_suites(n_version(Version))} 
                  | ssl_test_lib:ssl_options(ServerOpts0, Config)],
    
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
    
    ServerOpts = ssl_test_lib:ssl_options([{verify, verify_peer}, {cacerts, [ServerRoot]} |  
                                           proplists:delete(cacerts, ServerOpts0)], Config),
    ClientOpts = ssl_test_lib:ssl_options([{verify, verify_peer},
                                           {depth, 5},
                                           {cacerts,  ServerCas ++ ClientCas} | 
                                           proplists:delete(cacerts, ClientOpts0)], Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

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
     [{doc,"TLS 1.3: Test client authentication with unsupported signature_algorithm_cert"}].

unsupported_sign_algo_cert_client_auth(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  {signature_algs, [rsa_pkcs1_sha256, rsa_pkcs1_sha384, rsa_pss_rsae_sha256]},
                  %% Skip rsa_pkcs1_sha256!
                  {signature_algs_cert, [rsa_pkcs1_sha384, rsa_pkcs1_sha512]},
                  {fail_if_no_peer_cert, true}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_required).

%%--------------------------------------------------------------------
unsupported_sign_algo_client_auth() ->
     [{doc,"TLS 1.3: Test client authentication with unsupported signature_algorithm"}].

unsupported_sign_algo_client_auth(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    ServerOpts = [{versions, ['tlsv1.2','tlsv1.3']},
                  {verify, verify_peer},
                  %% Skip rsa_pkcs1_sha256!
                  {signature_algs, [rsa_pkcs1_sha384, rsa_pkcs1_sha512]},
                  {fail_if_no_peer_cert, true}|ServerOpts0],
    ClientOpts = [{versions, ['tlsv1.2','tlsv1.3']}|ClientOpts0],
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
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------
two_digits_str(N) when N < 10 ->
    lists:flatten(io_lib:format("0~p", [N]));
two_digits_str(N) ->
    lists:flatten(io_lib:format("~p", [N])).

delete_authority_key_extension([], Acc) ->
    lists:reverse(Acc);
delete_authority_key_extension([#'Extension'{extnID = ?'id-ce-authorityKeyIdentifier'} | Rest],
 			       Acc) ->
    delete_authority_key_extension(Rest, Acc);
delete_authority_key_extension([Head | Rest], Acc) ->
    delete_authority_key_extension(Rest, [Head | Acc]).

n_version(Version) when Version == 'tlsv1.2';
                        Version == 'tlsv1.1';
                        Version == 'tlsv1';
                        Version == 'sslv3'
                        ->
    tls_record:protocol_version(Version);
n_version(Version) when Version == 'dtlsv1.2';
                        Version == 'dtlsv1' ->
    dtls_record:protocol_version(Version).
