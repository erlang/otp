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
-module(ssl_cert_tests).
-compile({nowarn_deprecated_function, [{public_key, encrypt_private, 3}]}).
-include("ssl_test_lib.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([support_kems/1,
         mldsa_config/1,
         rsa_config/1,
         rsa_pss_config/2,
         dsa_config/1,
         openssl_dsa_config/1,
         eddsa_config/1,
         ecdsa_config/1,
         openssl_eddsa_config/1]).

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

-export([test_ciphers/2, openssl_ciphers/0]).

support_kems(Config) ->
       case  [] =/= crypto:supports(kems) of
        true ->
            Config;
        false ->
            {skip, "Missing support for mlkem in OpenSSL"}
    end.

mldsa_config(Config) ->
    PKAlgs = crypto:supports(public_keys),
    case lists:member(mldsa44, PKAlgs)
        andalso lists:member(mldsa65, PKAlgs)
        andalso lists:member(mldsa87, PKAlgs) of
        true ->
            DataDir = proplists:get_value(data_dir, Config),
            PrivDir = proplists:get_value(priv_dir, Config),
            [Keys1, Keys2, Keys3] = mldsa_keys(DataDir),
            Conf = #{server_chain =>
                         #{root => [Keys3],
                           intermediates => [[Keys2]],
                           peer =>  [Keys1]},
                     client_chain => #{root => [Keys1],
                                       intermediates => [[Keys2]],
                                       peer =>  [Keys3]
                                      }},
            GenCertData =
                public_key:pkix_test_data(Conf),
            Version = proplists:get_value(version, Config),
            ClientFileBase = filename:join(PrivDir, "mldsa"),
            ServerFileBase = filename:join(PrivDir, "mldsa"),
            [{server_config, ServerConf},
             {client_config, ClientConf}] =
                x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),
            [{cert_key_alg, {mldsa, [[Keys1], [Keys2], [Keys3]]}},
             {cert_key_alg, mldsa},
             {extra_client, ssl_test_lib:sig_algs(mldsa, Version)},
             {extra_server, ssl_test_lib:sig_algs(mldsa, Version)} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, fun() -> ClientConf end},
                           {server_cert_opts, fun() -> ServerConf end} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        false ->
            {skip, "Missing ML-DSA crypto support"}
    end.

rsa_config(Config0) ->
   Config1 = ssl_test_lib:make_rsa_cert(Config0),
    Config = ssl_test_lib:make_rsa_1024_cert(Config1),
    COpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    SOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Version = proplists:get_value(version, Config),
    [{cert_key_alg, rsa},
     {extra_client, ssl_test_lib:sig_algs(rsa, Version)},
     {extra_server, ssl_test_lib:sig_algs(rsa, Version)} |
     lists:delete(cert_key_alg,
                  [{client_cert_opts, fun() -> COpts end},
                   {server_cert_opts, fun() -> SOpts end} |
                   lists:delete(server_cert_opts,
                                lists:delete(client_cert_opts, Config))])].
rsa_pss_config(Alg, Config) ->
    Supports = crypto:supports(),
    RSAOpts = proplists:get_value(rsa_opts, Supports),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    
    case lists:member(rsa_pkcs1_pss_padding, RSAOpts)
        andalso lists:member(rsa_pss_saltlen, RSAOpts)
        andalso lists:member(rsa_mgf1_md, RSAOpts) of
        true ->
            #{client_config := COpts,
              server_config := SOpts} = ssl_test_lib:make_rsa_pss_pem(Alg, [], Config, ""),
            [{cert_key_alg, Alg},
             {extra_client, ssl_test_lib:sig_algs(Alg, Version)},
             {extra_server, ssl_test_lib:sig_algs(Alg, Version)} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, fun() -> COpts end},
                           {server_cert_opts, fun() -> SOpts end} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        false ->
            {skip, "Missing RSA-PSS crypto support"}
    end.

dsa_config(Config0) ->
    PKAlg = crypto:supports(public_keys),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config0)),
    case lists:member(dss, PKAlg) andalso lists:member(dh, PKAlg) of
        true ->
            Config = ssl_test_lib:make_dsa_cert(Config0),
            COpts = ssl_test_lib:ssl_options(client_dsa_opts, Config),
            SOpts = ssl_test_lib:ssl_options(server_dsa_opts, Config),
            ShaDSA = case Version of
                         {3, 3} ->
                             [{signature_algs, [{sha, dsa}]}];
                         _  ->
                             []
                     end,
            [{cert_key_alg, dsa},
             {extra_client, ssl_test_lib:sig_algs(dsa, Version) ++
                  [{ciphers, ssl_test_lib:dsa_suites(Version)}] ++ ShaDSA},
             {extra_server, ssl_test_lib:sig_algs(dsa, Version) ++
                  [{ciphers, ssl_test_lib:dsa_suites(Version)}] ++ ShaDSA} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, fun() -> COpts end},
                           {server_cert_opts, fun() -> SOpts end} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))])];
        false ->
            {skip, "Missing DSS crypto support"}
    end.

openssl_dsa_config(Config0) ->
    PKAlg = crypto:supports(public_keys),
    NVersion = ssl_test_lib:n_version(proplists:get_value(version, Config0)),
    SigAlgs = ssl_test_lib:sig_algs(dsa, NVersion),
    case lists:member(dss, PKAlg) andalso lists:member(dh, PKAlg)
        andalso (ssl_test_lib:openssl_dsa_suites() =/= [])
        andalso (ssl_test_lib:check_sane_openssl_dsa(Config0))
    of
        true ->
            Config = ssl_test_lib:make_dsa_cert(Config0),
            COpts = SigAlgs ++ ssl_test_lib:ssl_options(client_dsa_opts, Config),
            SOpts = SigAlgs ++ ssl_test_lib:ssl_options(server_dsa_opts, Config),
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
                                  [{client_cert_opts, fun() -> [{ciphers, Ciphers} | COpts] end},
                                   {server_cert_opts, fun() -> [{ciphers, Ciphers} | SOpts] end} |
                                   lists:delete(server_cert_opts,
                                                lists:delete(client_cert_opts, Config))])];
                [] ->
                    {skip, {no_sup, dsa_suites, Version}}
            end;
        false ->
            {skip, "Missing DSS crypto support"}
    end.

eddsa_config(Config0) ->
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
                          [{client_cert_opts, fun() -> COpts end},
                           {server_cert_opts, fun() -> SOpts end} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config0))]
                         )];
        false ->
            {skip, "Missing EDDSA crypto support"}
    end.

ecdsa_config(Config0) ->
    Config = ssl_test_lib:make_ecdsa_cert(Config0),
    COpts = ssl_test_lib:ssl_options(client_ecdsa_verify_opts, Config),
    SOpts = ssl_test_lib:ssl_options(server_ecdsa_opts, Config),
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
                          [{client_cert_opts, fun() -> [{ciphers, Ciphers} | COpts] end},
                           {server_cert_opts, fun() -> SOpts end} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config))]
                         )];
        [] ->
            {skip, "Missing EC crypto support"}
    end.
openssl_eddsa_config(Config0)->
    PKAlg = crypto:supports(public_keys),
    PrivDir = proplists:get_value(priv_dir, Config0),
    case lists:member(eddsa, PKAlg) andalso
        (lists:member(ecdh, PKAlg) andalso
         lists:member(ecdsa, PKAlg)) of
        true ->
            Conf = public_key:pkix_test_data(#{server_chain => #{root => ssl_test_lib:eddsa_conf(),
                                                                 intermediates => [ssl_test_lib:eddsa_conf()],
                                                                 peer =>  ssl_test_lib:eddsa_conf()},
                                               %% OpenSSL does currently not support EDDSA private key files
                                               client_chain => #{root => ssl_test_lib:ecdsa_conf(),
                                                                 intermediates => [ssl_test_lib:ecdsa_conf()],
                                                                 peer =>  ssl_test_lib:ecdsa_conf()}}),
            [{server_config, SOpts},
             {client_config, COpts}] = x509_test:gen_pem_config_files(Conf, filename:join(PrivDir,
                                                                                          "client_ecdsa_missing_eddsa"),
                                                                      filename:join(PrivDir, "server_eddsa")),

            [{cert_key_alg, eddsa} |
             lists:delete(cert_key_alg,
                          [{client_cert_opts, fun() -> COpts end},
                           {server_cert_opts, fun() -> SOpts end} |
                           lists:delete(server_cert_opts,
                                        lists:delete(client_cert_opts, Config0))]
                         )];
        false ->
            {skip, "Missing EDDSA/ECDSA OpenSSL stack support"}
    end.

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
mlkem_groups() ->
    [{doc,"Test that ssl server can select a common mlkem group for key-exchange"}].

mlkem_groups(Config) ->
    test_mlkem(Config, mlkem512),
    test_mlkem(Config, mlkem768),
    test_mlkem(Config, mlkem1024).

mlkem_hybrid_groups() ->
    [{doc,"Test that ssl server can select a common mlkem hybrid group for key-exchange"}].

mlkem_hybrid_groups(Config) ->
    test_mlkem(Config, x25519mlkem768),
    test_mlkem(Config, secp256r1mlkem768),
    test_mlkem(Config, secp384r1mlkem1024).

test_mlkem(Config, MLKemGroup) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_cert_opts, Config),
    ServerOpts0 = ssl_test_lib:ssl_options(server_cert_opts, Config),

    {ServerOpts, ClientOpts} = group_config_mlkem(Config,
                                                  [{versions, ['tlsv1.3']} | ServerOpts0],
                                                  [{versions, ['tlsv1.3']} | ClientOpts0], MLKemGroup),

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
%% Utility functions  -----------------------------------------------
%%--------------------------------------------------------------------
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
mldsa_keys(DataDir) ->
    PrivFile1 = filename:join([DataDir, "mldsa-44.pem"]),
    PrivFile2 = filename:join([DataDir, "mldsa-65.pem"]),
    PrivFile3 = filename:join([DataDir, "mldsa-87.pem"]),
    PubFile1 = filename:join([DataDir, "mldsa-44-pub.pem"]),
    PubFile2 = filename:join([DataDir, "mldsa-65-pub.pem"]),
    PubFile3 = filename:join([DataDir, "mldsa-87-pub.pem"]),
    [mldsa_key_spec(PubFile1, PrivFile1),
     mldsa_key_spec(PubFile2, PrivFile2),
     mldsa_key_spec(PubFile3, PrivFile3)].

mldsa_key_spec(PubFile, PrivFile) ->
    [PubPemEntry] = ssl_test_lib:pem_to_der(PubFile),
    [PrivPemEntry] = ssl_test_lib:pem_to_der(PrivFile),
    {key, {both, public_key:pem_entry_decode(PubPemEntry),
           public_key:pem_entry_decode(PrivPemEntry)}}.

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


group_config_mlkem(Config, ServerOpts, ClientOpts, Group) ->
        case proplists:get_value(client_type, Config) of
            erlang ->
                {[{groups, openssl_mlkem(Group)} | ServerOpts],
                 [{supported_groups, supported_groups(Group)} | ClientOpts]};
            openssl ->
                {[{supported_groups, supported_groups(Group)} | ServerOpts],
                 [{groups, openssl_mlkem(Group)} | ClientOpts]}
        end.

openssl_mlkem(mlkem512) ->
    "MLKEM512";
openssl_mlkem(mlkem768) ->
    "MLKEM768";
openssl_mlkem(mlkem1024) ->
    "MLKEM1024";
openssl_mlkem(x25519mlkem768) ->
    "X25519MLKEM768";
openssl_mlkem(secp256r1mlkem768) ->
    "SecP256r1MLKEM768";
openssl_mlkem(secp384r1mlkem1024) ->
    "SecP384r1MLKEM1024".

supported_groups(Group) ->
    [Group].



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
    ecdsa;
alg_key(#'ML-DSAPrivateKey'{}) ->
    mldsa.
