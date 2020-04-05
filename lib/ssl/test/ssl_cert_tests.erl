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
-module(ssl_cert_tests).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

no_auth() ->
     [{doc,"Test connection without authentication"}].

no_auth(Config) ->
    ClientOpts = [{verify, verify_none} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{verify, verify_none} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
auth() ->
     [{doc,"Test connection with mutual authentication"}].

auth(Config) ->
    ClientOpts = [{verify, verify_peer} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{verify, verify_peer} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
client_auth_empty_cert_accepted() ->
    [{doc,"Test client authentication when client sends an empty certificate and " 
      "fail_if_no_peer_cert is set to false."}].

client_auth_empty_cert_accepted(Config) ->
    ClientOpts = proplists:delete(keyfile,
                                  proplists:delete(certfile, 
                                                   ssl_test_lib:ssl_options(client_cert_opts, Config))),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
    ServerOpts = [{verify, verify_peer},
                  {fail_if_no_peer_cert, false} | ServerOpts0],
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
%%--------------------------------------------------------------------
client_auth_empty_cert_rejected() ->
     [{doc,"Test client authentication when client sends an empty certificate and " 
       "fail_if_no_peer_cert is set to true."}].

client_auth_empty_cert_rejected(Config) ->
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
		  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ClientOpts0 = ssl_test_lib:ssl_options([], Config),
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
client_auth_partial_chain() ->
    [{doc, "Client sends an incompleate chain, by default not acceptable."}].

client_auth_partial_chain(Config) when is_list(Config) ->
    ServerOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
		  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    {ok, ClientCAs} = file:read_file(proplists:get_value(cacertfile, ClientOpts0)),
    [{_,RootCA,_} | _] = public_key:pem_decode(ClientCAs),
    ClientOpts =  [{cacerts, [RootCA]} |
                   proplists:delete(cacertfile, ClientOpts0)],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, unknown_ca).
    
%%--------------------------------------------------------------------
client_auth_allow_partial_chain() ->
    [{doc, "Server trusts intermediat CA and accepts a partial chain. (partial_chain option)"}].

client_auth_allow_partial_chain(Config) when is_list(Config) ->
    ServerOpts0 = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
		  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),
    {ok, ClientCAs} = file:read_file(proplists:get_value(cacertfile, ClientOpts)),
    [{_,_,_}, {_, IntermidiateCA, _} | _] = public_key:pem_decode(ClientCAs),

    PartialChain =  fun(CertChain) ->
			    case lists:member(IntermidiateCA, CertChain) of
				true ->
				    {trusted_ca, IntermidiateCA};
				false ->
				    unknown_ca
			    end
		    end,
    ServerOpts = [{cacerts, [IntermidiateCA]},
                  {partial_chain, PartialChain} |
                  proplists:delete(cacertfile, ServerOpts0)],

    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

 %%--------------------------------------------------------------------
client_auth_do_not_allow_partial_chain() ->
    [{doc, "Server does not accept the chain sent by the client as ROOT CA is unkown, "
      "and we do not choose to trust the intermediate CA. (partial_chain option)"}].

client_auth_do_not_allow_partial_chain(Config) when is_list(Config) ->
    ServerOpts0 = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
		  | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),
    {ok, ServerCAs} = file:read_file(proplists:get_value(cacertfile, ServerOpts0)),
    [{_,_,_}, {_, IntermidiateCA, _} | _] = public_key:pem_decode(ServerCAs),

    PartialChain =  fun(_CertChain) ->
			    unknown_ca
		    end,
    ServerOpts = [{cacerts, [IntermidiateCA]},
                  {partial_chain, PartialChain} |
                  proplists:delete(cacertfile, ServerOpts0)],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, unknown_ca).
    
 %%--------------------------------------------------------------------
client_auth_partial_chain_fun_fail() ->
    [{doc, "If parial_chain fun crashes, treat it as if it returned unkown_ca"}].

client_auth_partial_chain_fun_fail(Config) when is_list(Config) ->
    ServerOpts0 = [{verify, verify_peer}, {fail_if_no_peer_cert, true}
                   | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    ClientOpts = ssl_test_lib:ssl_options(client_cert_opts, Config),

    {ok, ServerCAs} = file:read_file(proplists:get_value(cacertfile, ServerOpts0)),
    [{_,_,_}, {_, IntermidiateCA, _} | _] = public_key:pem_decode(ServerCAs),

    PartialChain =  fun(_CertChain) ->
                            true = false %% crash on purpose
		    end,
    ServerOpts = [{cacerts, [IntermidiateCA]},
                  {partial_chain, PartialChain} |
                  proplists:delete(cacertfile, ServerOpts0)],
    
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, unknown_ca).

%%--------------------------------------------------------------------
missing_root_cert_no_auth() ->
     [{doc,"Test that the client succeds if the ROOT CA is unknown in verify_none mode"}].

missing_root_cert_no_auth(Config) ->
    ClientOpts = [{verify, verify_none} | ssl_test_lib:ssl_options(client_cert_opts, Config)],
    ServerOpts =  [{verify, verify_none} | ssl_test_lib:ssl_options(server_cert_opts, Config)],
    
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
invalid_signature_client() ->
    [{doc,"Test server with invalid signature"}].

invalid_signature_client(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
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
    ServerOpts = [{verify, verify_peer} | ServerOpts0],
    ssl_test_lib:basic_alert(ClientOpts, ServerOpts, Config, unknown_ca).

%%--------------------------------------------------------------------
invalid_signature_server() ->
    [{doc,"Test client with invalid signature"}].

invalid_signature_server(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),
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

test_ciphers(_, 'tlsv1.3' = Version) ->
    Ciphers = ssl:cipher_suites(default, Version),
    ct:log("Version ~p Testing  ~p~n", [Version, Ciphers]),
    OpenSSLCiphers = openssl_ciphers(),
    ct:log("OpenSSLCiphers ~p~n", [OpenSSLCiphers]),
    lists:filter(fun(C) ->
                         ct:log("Cipher ~p~n", [C]),
                         lists:member(ssl_cipher_format:suite_map_to_openssl_str(C), OpenSSLCiphers)
                 end, Ciphers);
test_ciphers(Kex, Version) ->
    Ciphers = ssl:filter_cipher_suites(ssl:cipher_suites(default, Version), 
                                       [{key_exchange, Kex}]),
    ct:log("Version ~p Testing  ~p~n", [Version, Ciphers]),
    OpenSSLCiphers = openssl_ciphers(),
    ct:log("OpenSSLCiphers ~p~n", [OpenSSLCiphers]),
    lists:filter(fun(C) ->
                         ct:log("Cipher ~p~n", [C]),
                         lists:member(ssl_cipher_format:suite_map_to_openssl_str(C), OpenSSLCiphers)
                 end, Ciphers).



openssl_ciphers() ->
    Str = os:cmd("openssl ciphers"),
    string:split(string:strip(Str, right, $\n), ":", all).
