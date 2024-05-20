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
-module(ssl_cert_tests).
-compile({nowarn_deprecated_function, [{public_key, encrypt_private, 3}]}).
-include("ssl_test_lib.hrl").
-include_lib("public_key/include/public_key.hrl").

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
         invalid_signature_client/0,
         invalid_signature_client/1,
         invalid_signature_server/0,
         invalid_signature_server/1,
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

-export([test_ciphers/2, openssl_ciphers/0]).

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

no_auth() ->
     [{doc,"Test connection without authentication"}].

no_auth(Config) ->
    ClientOpts = [{verify, verify_none} | ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config)],
    ServerOpts =  [{verify, verify_none} | ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
auth() ->
     [{doc,"Test connection with mutual authentication"}].

auth(Config) ->
    Version = proplists:get_value(version,Config),
    CommonClientOpts = [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config)],
    ClientOpts =  case Version of
                      'tlsv1.3' ->
                          [{certificate_authorities, true} | CommonClientOpts];
                      _ ->
                          CommonClientOpts
                  end,
    ServerOpts =  [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config)],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
client_auth_custom_key() ->
    [{doc,"Test that client and server can connect using their own signature function"}].

client_auth_custom_key(Config) when is_list(Config) ->
    Version = proplists:get_value(version,Config),
    CommonClientOpts = [{verify, verify_peer} | ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config)],
    ClientOpts0 =  case Version of
                      'tlsv1.3' ->
                           [{certificate_authorities, true} | CommonClientOpts];
                      _ ->
                           CommonClientOpts
                  end,
    ClientKeyFilePath =  proplists:get_value(keyfile, ClientOpts0),
    [ClientKeyEntry] = ssl_test_lib:pem_to_der(ClientKeyFilePath),
    ClientKey = ssl_test_lib:public_key(public_key:pem_entry_decode(ClientKeyEntry)),
    ClientCustomKey = choose_custom_key(ClientKey, Version),

    ClientOpts = [ ClientCustomKey | proplists:delete(key, proplists:delete(keyfile, ClientOpts0))],

    ServerOpts0 = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    ServerKeyFilePath =  proplists:get_value(keyfile, ServerOpts0),
    [ServerKeyEntry] = ssl_test_lib:pem_to_der(ServerKeyFilePath),
    ServerKey = ssl_test_lib:public_key(public_key:pem_entry_decode(ServerKeyEntry)),
    ServerCustomKey = choose_custom_key(ServerKey, Version),

    ServerOpts = [ ServerCustomKey, {verify, verify_peer} | ServerOpts0],

    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
client_auth_empty_cert_accepted() ->
    [{doc,"Client sends empty cert chain as no cert is configured and server allows it"}].

client_auth_empty_cert_accepted(Config) ->
    ClientOpts = [{verify, verify_peer} |
                    proplists:delete(keyfile,
                                     proplists:delete(certfile,
                                                      ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config)))],
    ServerOpts0 = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    ServerOpts = [{verify, verify_peer},
                  {fail_if_no_peer_cert, false} | ServerOpts0],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
client_auth_empty_cert_rejected() ->
    [{doc,"Client sends empty cert chain as no cert is configured"}].

client_auth_empty_cert_rejected(Config) ->
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
                 | ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config)],
    ClientOpts0 = [{verify, verify_none} | ssl_test_lib:ssl_options(extra_client, [], Config)],
    %% Delete Client Cert and Key
    ClientOpts1 = proplists:delete(certfile, ClientOpts0),
    ClientOpts = proplists:delete(keyfile, ClientOpts1),
    
    Version = proplists:get_value(version,Config),
    case Version of
        'tlsv1.3' ->
            ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_required);
        _ ->
            ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, handshake_failure)
    end.
%%--------------------------------------------------------------------
client_auth_no_suitable_chain() ->
    [{doc, "Client sends an empty cert chain as no suitable chain is found."}].

client_auth_no_suitable_chain(Config) when is_list(Config) ->
    CRoot = public_key:pkix_test_root_cert("OTP other client test ROOT", []),
    #{client_config := ClientOpts0} = public_key:pkix_test_data(#{server_chain => #{root => [],
                                                                                    intermediates => [[]],
                                                                                    peer => []},
                                                                  client_chain => #{root => CRoot,
                                                                                    intermediates => [[]],
                                                                                    peer => []}}),
    ClientOpts =  [{verify, verify_none} | ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config)],
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
                 | ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config)],
    Version = proplists:get_value(version, Config),
    
    case Version of
        'tlsv1.3' ->
            ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_required);
        _ ->
            ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, handshake_failure)
    end.

%%--------------------------------------------------------------------
client_auth_use_partial_chain() ->
    [{doc, "Client trusts intermediat CA and verifies the shorter chain."}].

client_auth_use_partial_chain(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(proplists:get_value(name, Prop)),
    {Year, Month, Day} = date(),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain,
                                                                          [[{validity, {{Year-2, Month, Day},
                                                                                        {Year-1, Month, Day}}}],
                                                                           [],
                                                                           []
                                                                          ]},
                                                                         {client_chain, DefaultCertConf}]),
    ClientOpts = ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config),
    ServerOpts = ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config),
    [_, IntermidiateCA, _] = proplists:get_value(cacerts, ServerOpts),
    PartialChain =  fun(CertChain) ->
			    case lists:member(IntermidiateCA, CertChain) of
				true ->
				    {trusted_ca, IntermidiateCA};
				false ->
				    unknown_ca
			    end
		    end,
    ssl_test_lib:basic_test([{verify, verify_peer}, {partial_chain, PartialChain} |ClientOpts], ServerOpts, Config).

 %%--------------------------------------------------------------------
client_auth_do_not_use_partial_chain() ->
    [{doc, "Client does not trust an intermediat CA and fails the connetion as ROOT has expired"}].

client_auth_do_not_use_partial_chain(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(proplists:get_value(name, Prop)),
    {Year, Month, Day} = date(),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain,
                                                                          [[{validity, {{Year-2, Month, Day},
                                                                                        {Year-1, Month, Day}}}],
                                                                           [],
                                                                           []
                                                                          ]},
                                                                         {client_chain, DefaultCertConf}]),
    PartialChain =  fun(_CertChain) ->
			    unknown_ca
		    end,
    ClientOpts = [{verify, verify_peer}, {partial_chain, PartialChain} | ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config)],
    ServerOpts = ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config),
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_expired).
    
 %%--------------------------------------------------------------------
client_auth_partial_chain_fun_fail() ->
    [{doc, "If partial_chain fun crashes, treat it as if it returned unkown_ca"}].

client_auth_partial_chain_fun_fail(Config) when is_list(Config) ->
    Prop = proplists:get_value(tc_group_properties, Config),
    DefaultCertConf = ssl_test_lib:default_ecc_cert_chain_conf(proplists:get_value(name, Prop)),
    {Year, Month, Day} = date(),
    #{client_config := ClientOpts0,
      server_config := ServerOpts0} = ssl_test_lib:make_cert_chains_der(proplists:get_value(cert_key_alg, Config),
                                                                        [{server_chain,
                                                                          [[{validity, {{Year-2, Month, Day},
                                                                                        {Year-1, Month, Day}}}],
                                                                           [],
                                                                           []
                                                                          ]},
                                                                         {client_chain, DefaultCertConf}]),


    PartialChain = fun(_CertChain) ->
                           error(crash_on_purpose)
                   end,
    ClientOpts = [{verify, verify_peer}, {partial_chain, PartialChain} | ssl_test_lib:ssl_options(extra_client, ClientOpts0, Config)],
    ServerOpts = [ssl_test_lib:ssl_options(extra_server, ServerOpts0, Config)],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_expired).
    

%%--------------------------------------------------------------------
client_auth_sni() ->
    [{doc, "Check that sni check works with user verify_fun"}].
client_auth_sni(Config) when is_list(Config) ->
    ServerOpts0 = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),

    FunAndState = {fun(valid_peer, {bad_cert, unknown_ca}, UserState) ->
                           {valid_peer, UserState};
                      (_,{bad_cert, _} = Reason, _) ->                         
                           {fail, Reason};
                      (_,{extension, _}, UserState) ->
                           {unknown, UserState};
                      (_, valid, UserState) ->
                           {valid, UserState};
                      (_, valid_peer, UserState) ->
                           {valid, UserState}
                   end, []},

    ClientOpts0 = ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config),
    ClientOpts = [{verify, verify_peer}, {verify_fun, FunAndState
                                         }, {server_name_indication, "localhost"} | ClientOpts0], 

    {ok, ServerCAs} = file:read_file(proplists:get_value(cacertfile, ServerOpts0)),
    [{_,_,_}, {_, IntermidiateCA, _} | _] = public_key:pem_decode(ServerCAs),

    ServerOpts = [{cacerts, [IntermidiateCA]} |
                  proplists:delete(cacertfile, ServerOpts0)],
    %% Basic test if hostname check is not performed the connection will succeed
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts0, Config, handshake_failure),
    %% Also test that user verify_fun is run.
    %% If user verify fun is not used the ALERT will be unknown_ca
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, handshake_failure).

%%--------------------------------------------------------------------
client_auth_seelfsigned_peer() ->
    [{doc, "Check that selfsigned peer raises alert"}].
client_auth_seelfsigned_peer(Config) when is_list(Config) ->
    Ext = x509_test:extensions([{key_usage, [keyCertSign, cRLSign, digitalSignature, keyAgreement]}]),
    #{cert := Cert,
      key := Key} = public_key:pkix_test_root_cert("OTP test server ROOT", [{key, ssl_test_lib:hardcode_rsa_key(6)},
                                                                            {extensions, Ext}]),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    DerKey = public_key:der_encode('RSAPrivateKey', Key),
    ssl_test_lib:basic_alert(ssl_test_lib:ssl_options(extra_client, [{verify, verify_peer}, {cacerts , [Cert]}] ++
                                                          ssl_test_lib:sig_algs(rsa, Version), Config),
                             ssl_test_lib:ssl_options(extra_server, [{cert, Cert},
                                                                     {key, {'RSAPrivateKey', DerKey}}] ++
                                                          ssl_test_lib:sig_algs(rsa, Version), Config),
                             Config, bad_certificate).
%%--------------------------------------------------------------------
missing_root_cert_no_auth() ->
     [{doc,"Test that the client succeeds if the ROOT CA is unknown in verify_none mode"}].

missing_root_cert_no_auth(Config) ->
    ClientOpts = [{verify, verify_none} | ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config)],
    ServerOpts =  [{verify, verify_none} | ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config)],

    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
invalid_signature_client() ->
    [{doc,"Test that server detects invalid client signature"}].

invalid_signature_client(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    KeyFile =  proplists:get_value(keyfile, ClientOpts0),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ClientCertFile = proplists:get_value(certfile, ClientOpts0),
    NewClientCertFile = filename:join(PrivDir, "client_invalid_cert.pem"),
    [{'Certificate', ClientDerCert, _}] = ssl_test_lib:pem_to_der(ClientCertFile),
    ClientOTPCert = public_key:pkix_decode_cert(ClientDerCert, otp),
    ClientOTPTbsCert = ClientOTPCert#'OTPCertificate'.tbsCertificate,
    NewClientDerCert = public_key:pkix_sign(ClientOTPTbsCert, Key),
    ssl_test_lib:der_to_pem(NewClientCertFile, [{'Certificate', NewClientDerCert, not_encrypted}]),
    ClientOpts = [{certfile, NewClientCertFile} | proplists:delete(certfile, ClientOpts0)],
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true} | ServerOpts0],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, unknown_ca).

%%--------------------------------------------------------------------
invalid_signature_server() ->
    [{doc,"Test that client detects invalid server signature"}].

invalid_signature_server(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(extra_client, client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(extra_server, server_cert_opts, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    KeyFile =  proplists:get_value(keyfile, ServerOpts0),
    [KeyEntry] = ssl_test_lib:pem_to_der(KeyFile),
    Key = ssl_test_lib:public_key(public_key:pem_entry_decode(KeyEntry)),

    ServerCertFile = proplists:get_value(certfile, ServerOpts0),
    NewServerCertFile = filename:join(PrivDir, "server_invalid_cert.pem"),
    [{'Certificate', ServerDerCert, _}] = ssl_test_lib:pem_to_der(ServerCertFile),
    ServerOTPCert = public_key:pkix_decode_cert(ServerDerCert, otp),
    ServerOTPTbsCert = ServerOTPCert#'OTPCertificate'.tbsCertificate,
    NewServerDerCert = public_key:pkix_sign(ServerOTPTbsCert, Key),
    ssl_test_lib:der_to_pem(NewServerCertFile, [{'Certificate', NewServerDerCert, not_encrypted}]),
    ServerOpts = [{certfile, NewServerCertFile} | proplists:delete(certfile, ServerOpts0)],
    ClientOpts = [{verify, verify_peer} | ClientOpts0],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, unknown_ca).
   
%%--------------------------------------------------------------------
%% TLS 1.3 Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
hello_retry_request() ->
    [{doc,"Test that ssl server can request a new group when the client's first key share"
      "is not supported"}].

hello_retry_request(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    
    {ServerOpts, ClientOpts} = group_config(Config,                                           
                                            [{versions, ['tlsv1.2','tlsv1.3']} | ServerOpts0],
                                            [{versions, ['tlsv1.2','tlsv1.3']} | ClientOpts0]), 

    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
custom_groups() ->
    [{doc,"Test that ssl server can select a common group for key-exchange"}].

custom_groups(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),

    {ServerOpts, ClientOpts} = group_config_custom(Config,
                                                   [{versions, ['tlsv1.2','tlsv1.3']} | ServerOpts0],
                                                   [{versions, ['tlsv1.2','tlsv1.3']} | ClientOpts0]),
    
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
   
    {ServerOpts, ClientOpts} = group_config(Config,                                           
                                            [{versions, ['tlsv1.2','tlsv1.3']},
                                             {verify, verify_peer},
                                             {fail_if_no_peer_cert, true} | ServerOpts0],
                                            [{versions, ['tlsv1.2','tlsv1.3']}, {verify, verify_peer} | ClientOpts0]), 
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
hello_retry_client_auth_empty_cert_accepted() ->
     [{doc,"TLS 1.3 (HelloRetryRequest): Test client authentication when client sends an empty " 
       "certificate and fail_if_no_peer_cert is set to false."}].

hello_retry_client_auth_empty_cert_accepted(Config) ->
    ClientOpts0 = proplists:delete(keyfile,
                                   proplists:delete(certfile, 
                                                    ssl_test_lib:ssl_options(client_cert_opts, Config))),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),

    {ServerOpts, ClientOpts} = group_config(Config,                                           
                                            [{versions, ['tlsv1.2','tlsv1.3']},
                                             {verify, verify_peer},
                                             {fail_if_no_peer_cert, false} | ServerOpts0],
                                            [{versions, ['tlsv1.2','tlsv1.3']}, {verify, verify_peer} | ClientOpts0]), 
            
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
hello_retry_client_auth_empty_cert_rejected() ->
     [{doc,"TLS 1.3 (HelloRetryRequest): Test client authentication when client "
       "sends an empty certificate and fail_if_no_peer_cert is set to true."}].

hello_retry_client_auth_empty_cert_rejected(Config) ->
    ClientOpts0 = proplists:delete(keyfile,
                                   proplists:delete(certfile, 
                                                    ssl_test_lib:ssl_options(client_cert_opts, Config))),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    
    {ServerOpts, ClientOpts} = group_config(Config,                                           
                                            [{versions, ['tlsv1.2','tlsv1.3']},
                                             {verify, verify_peer},
                                             {fail_if_no_peer_cert, true} | ServerOpts0],
                                            [{versions, ['tlsv1.2','tlsv1.3']}, {verify, verify_peer} | ClientOpts0]), 
       
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, certificate_required).

test_ciphers(_, 'tlsv1.3' = Version) ->
    Ciphers = ssl:cipher_suites(default, Version),
    ?CT_LOG("Version ~p Testing  ~p~n", [Version, Ciphers]),
    OpenSSLCiphers = openssl_ciphers(),
    ?CT_LOG("OpenSSLCiphers ~p~n", [OpenSSLCiphers]),
    lists:filter(fun(C) ->
                         ?CT_LOG("Cipher ~p~n", [C]),
                         lists:member(ssl_cipher_format:suite_map_to_openssl_str(C), OpenSSLCiphers)
                 end, Ciphers);
test_ciphers(_, Version) when Version == 'dtlsv1';
                                Version == 'dtlsv1.2' ->
    NVersion = dtls_record:protocol_version_name(Version),
    Ciphers = [ssl_cipher_format:suite_bin_to_map(Bin) ||  Bin <- dtls_v1:suites(NVersion)],
    ?CT_LOG("Version ~p Testing  ~p~n", [Version, Ciphers]),
    OpenSSLCiphers = openssl_ciphers(),
    ?CT_LOG("OpenSSLCiphers ~p~n", [OpenSSLCiphers]),
    lists:filter(fun(C) ->
                         ?CT_LOG("Cipher ~p~n", [C]),
                         lists:member(ssl_cipher_format:suite_map_to_openssl_str(C), OpenSSLCiphers)
                 end, Ciphers);
test_ciphers(Kex, Version) ->
    Ciphers = ssl:filter_cipher_suites(ssl:cipher_suites(default, Version), 
                                       [{key_exchange, Kex}]),
    ?CT_LOG("Version ~p Testing  ~p~n", [Version, Ciphers]),
    OpenSSLCiphers = openssl_ciphers(),
    ?CT_LOG("OpenSSLCiphers ~p~n", [OpenSSLCiphers]),
    lists:filter(fun(C) ->
                         ?CT_LOG("Cipher ~p~n", [C]),
                         lists:member(ssl_cipher_format:suite_map_to_openssl_str(C), OpenSSLCiphers)
                 end, Ciphers).



openssl_ciphers() ->
    Str = os:cmd("openssl ciphers"),
    string:split(string:strip(Str, right, $\n), ":", all).

%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------

group_config_custom(Config, ServerOpts, ClientOpts) ->
        case proplists:get_value(client_type, Config) of
            erlang ->
                {[{groups,"X448:P-256:P-384"} | ServerOpts],
                 [{supported_groups, [secp384r1, secp256r1, x25519]} | ClientOpts]};
            openssl ->
                {[{supported_groups, [x448, secp256r1, secp384r1]} | ServerOpts],
                 [{groups,"P-384:P-256:X25519"} | ClientOpts]}
        end.

group_config(Config, ServerOpts, ClientOpts) ->
        case proplists:get_value(client_type, Config) of
            erlang ->
                {[{groups,"X448:X25519"} | ServerOpts],
                 [{supported_groups, [secp256r1, x25519]} | ClientOpts]};
            openssl ->
                {[{supported_groups, [x448, x25519]} | ServerOpts],
                 [{groups,"P-256:X25519"} | ClientOpts]}
        end.

choose_custom_key(#'RSAPrivateKey'{} = Key, Version)
  when (Version == 'dtlsv1') or (Version == 'tlsv1') or (Version == 'tlsv1.1') ->
    EFun = fun (PlainText, Options) ->
                   public_key:encrypt_private(PlainText, Key, Options)
          end,
    SFun = fun (Msg, HashAlgo, Options) ->
                   public_key:sign(Msg, HashAlgo, Key, Options)
           end,
    {key, #{algorithm => rsa, sign_fun => SFun, encrypt_fun => EFun}};
choose_custom_key(Key, _) ->
    Fun = fun (Msg, HashAlgo, Options) ->
                  public_key:sign(Msg, HashAlgo, Key, Options)
          end,
    {key, #{algorithm => alg_key(Key), sign_fun => Fun}}.

alg_key(#'RSAPrivateKey'{}) ->
    rsa;
alg_key({#'RSAPrivateKey'{}, #'RSASSA-PSS-params'{}}) ->
    rsa_pss_pss;
alg_key(#'DSAPrivateKey'{}) ->
    dsa;
alg_key(#'ECPrivateKey'{parameters = {namedCurve, CurveOId}}) when CurveOId == ?'id-Ed25519' orelse
                                                                   CurveOId == ?'id-Ed448' ->
    eddsa;
alg_key(#'ECPrivateKey'{}) ->
    ecdsa.
