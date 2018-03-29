%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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

-module(ssl_ECC_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.2', [], test_cases()},
     {'tlsv1.1', [], test_cases()},
     {'tlsv1', [], test_cases()},
     {'dtlsv1.2', [], test_cases()},
     {'dtlsv1', [], test_cases()}     
    ].

test_cases()->
    key_cert_combinations()
        ++ misc() 
        ++ ecc_negotiation().

key_cert_combinations() ->
    server_ecdh_rsa() ++
        server_ecdhe_rsa() ++
        server_ecdh_ecdsa() ++
        server_ecdhe_ecdsa().

server_ecdh_rsa() ->
    [client_ecdh_rsa_server_ecdh_rsa,
     client_ecdhe_rsa_server_ecdh_rsa,     
     client_ecdhe_ecdsa_server_ecdh_rsa].

server_ecdhe_rsa() ->
    [client_ecdh_rsa_server_ecdhe_rsa,
     client_ecdhe_rsa_server_ecdhe_rsa,
     client_ecdhe_ecdsa_server_ecdhe_rsa].

server_ecdh_ecdsa() ->
    [client_ecdh_ecdsa_server_ecdh_ecdsa,
     client_ecdhe_rsa_server_ecdh_ecdsa,
     client_ecdhe_ecdsa_server_ecdh_ecdsa].

server_ecdhe_ecdsa() ->
    [client_ecdh_rsa_server_ecdhe_ecdsa,
     client_ecdh_ecdsa_server_ecdhe_ecdsa,
     client_ecdhe_ecdsa_server_ecdhe_ecdsa].

misc()->
    [client_ecdsa_server_ecdsa_with_raw_key].

ecc_negotiation() ->
    [ecc_default_order,
     ecc_default_order_custom_curves,
     ecc_client_order,
     ecc_client_order_custom_curves,
     ecc_unknown_curve,
     client_ecdh_rsa_server_ecdhe_ecdsa_server_custom,
     client_ecdh_rsa_server_ecdhe_rsa_server_custom,
     client_ecdhe_rsa_server_ecdhe_ecdsa_server_custom,
     client_ecdhe_rsa_server_ecdhe_rsa_server_custom,
     client_ecdhe_rsa_server_ecdh_rsa_server_custom,
     client_ecdhe_ecdsa_server_ecdhe_ecdsa_server_custom,
     client_ecdhe_ecdsa_server_ecdhe_rsa_server_custom,
     client_ecdhe_ecdsa_server_ecdhe_ecdsa_client_custom,    
     client_ecdhe_rsa_server_ecdhe_ecdsa_client_custom
    ].

%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    end_per_suite(Config0),
    try crypto:start() of
	ok ->
            case ssl_test_lib:sufficient_crypto_support(cipher_ec) of
                true ->
                    Config0;
                false ->
                    {skip, "Crypto does not support ECC"}
            end
    catch _:_ ->
            {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(ssl),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
            [{tls_version, GroupName},
             {server_type, erlang},
             {client_type, erlang} | ssl_test_lib:init_tls_version(GroupName, Config)];
        _ ->
            Config
    end.

end_per_group(GroupName, Config0) ->
  case ssl_test_lib:is_tls_version(GroupName) of
      true ->
          Config = ssl_test_lib:clean_tls_version(Config0),
          proplists:delete(tls_version, Config);
      false ->
          Config0
  end.

%%--------------------------------------------------------------------

init_per_testcase(TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:log("Ciphers: ~p~n ", [ ssl:cipher_suites()]),
    end_per_testcase(TestCase, Config),
    ssl:start(),
    ct:timetrap({seconds, 15}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    application:stop(ssl),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%% Test diffrent certificate chain types, note that it is the servers
%% chain that affect what cipher suit that will be choosen

%% ECDH_RSA 
client_ecdh_rsa_server_ecdh_rsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdh_rsa_server_ecdh_rsa(Config).
client_ecdhe_rsa_server_ecdh_rsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_rsa_server_ecdh_rsa(Config).
client_ecdhe_ecdsa_server_ecdh_rsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_ecdsa_server_ecdh_rsa(Config).
%% ECDHE_RSA    
client_ecdh_rsa_server_ecdhe_rsa(Config)  when is_list(Config) ->
    ssl_ECC:client_ecdh_rsa_server_ecdhe_rsa(Config).
client_ecdhe_rsa_server_ecdhe_rsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_rsa_server_ecdhe_rsa(Config).
client_ecdhe_ecdsa_server_ecdhe_rsa(Config) when is_list(Config) ->
   ssl_ECC:client_ecdhe_ecdsa_server_ecdhe_rsa(Config).
%% ECDH_ECDSA
client_ecdh_ecdsa_server_ecdh_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdh_ecdsa_server_ecdh_ecdsa(Config).
client_ecdhe_rsa_server_ecdh_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_rsa_server_ecdh_ecdsa(Config).
client_ecdhe_ecdsa_server_ecdh_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdhe_ecdsa_server_ecdh_ecdsa(Config).
%% ECDHE_ECDSA
client_ecdh_rsa_server_ecdhe_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdh_rsa_server_ecdhe_ecdsa(Config).
client_ecdh_ecdsa_server_ecdhe_ecdsa(Config) when is_list(Config) ->
    ssl_ECC:client_ecdh_ecdsa_server_ecdhe_ecdsa(Config).
client_ecdhe_ecdsa_server_ecdhe_ecdsa(Config) when is_list(Config) ->
     ssl_ECC:client_ecdhe_ecdsa_server_ecdhe_ecdsa(Config).

client_ecdsa_server_ecdsa_with_raw_key(Config)  when is_list(Config) ->
     Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                       {client_chain, Default}]
                                                     , ecdhe_ecdsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ServerKeyFile = proplists:get_value(keyfile, SOpts),
    {ok, PemBin} = file:read_file(ServerKeyFile),
    PemEntries = public_key:pem_decode(PemBin),
     {'ECPrivateKey', Key, not_encrypted} = proplists:lookup('ECPrivateKey', PemEntries),
    ServerKey = {'ECPrivateKey', Key},
    SType = proplists:get_value(server_type, Config),
    CType = proplists:get_value(client_type, Config),
    {Server, Port} = ssl_test_lib:start_server_with_raw_key(SType,
                                                            [{key, ServerKey} | proplists:delete(keyfile, SOpts)],
                                                            Config),
    Client = ssl_test_lib:start_client(CType, Port, COpts, Config),
    ssl_test_lib:gen_check_result(Server, SType, Client, CType),
    ssl_test_lib:stop(Server, Client).

ecc_default_order(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                         {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa, Config),   
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [],
    case ssl_test_lib:supported_eccs([{eccs, [sect571r1]}]) of
        true ->  ssl_test_lib:ecc_test(sect571r1, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
     end.

ecc_default_order_custom_curves(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                         {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa, Config),   
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{eccs, [secp256r1, sect571r1]}],
    case ssl_test_lib:supported_eccs(ECCOpts) of
         true ->  ssl_test_lib:ecc_test(sect571r1, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

ecc_client_order(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                         {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa, Config),   
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{honor_ecc_order, false}],
    case ssl_test_lib:supported_eccs([{eccs, [sect571r1]}]) of
         true -> ssl_test_lib:ecc_test(sect571r1, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

ecc_client_order_custom_curves(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                        {client_chain, Default}],
                                                      ecdhe_ecdsa, ecdhe_ecdsa, Config),   
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{honor_ecc_order, false}, {eccs, [secp256r1, sect571r1]}],
    case ssl_test_lib:supported_eccs(ECCOpts) of
        true -> ssl_test_lib:ecc_test(sect571r1, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

ecc_unknown_curve(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                       {client_chain, Default}],
                                                      ecdhe_ecdsa, ecdhe_ecdsa, Config),   
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{eccs, ['123_fake_curve']}],
    ssl_test_lib:ecc_test_error(COpts, SOpts, [], ECCOpts, Config).

client_ecdh_rsa_server_ecdhe_ecdsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                       {client_chain, Default}], 
                                                        ecdh_rsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, sect571r1]}],
     case ssl_test_lib:supported_eccs(ECCOpts) of
         true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], ECCOpts, Config);
         false -> {skip, "unsupported named curves"}
     end.

client_ecdh_rsa_server_ecdhe_rsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                       {client_chain, Default}],
                                                      ecdh_rsa, ecdhe_rsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
     ECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, sect571r1]}],
    
    case ssl_test_lib:supported_eccs(ECCOpts) of
        true -> ssl_test_lib:ecc_test(undefined, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_rsa_server_ecdhe_ecdsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
     {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                        {client_chain, Default}],
                                                       ecdhe_rsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, sect571r1]}],
    case ssl_test_lib:supported_eccs(ECCOpts) of
         true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_rsa_server_ecdhe_rsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                       {client_chain, Default}], 
                                                      ecdhe_rsa, ecdhe_rsa, Config),

    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, sect571r1]}],
    case ssl_test_lib:supported_eccs(ECCOpts) of
        true -> ssl_test_lib:ecc_test(undefined, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
     end.
client_ecdhe_rsa_server_ecdh_rsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Ext = x509_test:extensions([{key_usage, [keyEncipherment]}]),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, [[], [], [{extensions, Ext}]]},
                                                       {client_chain, Default}], 
                                                      ecdhe_rsa, ecdh_rsa, Config),

    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, sect571r1]}],
    
    case ssl_test_lib:supported_eccs(ECCOpts) of
        true -> ssl_test_lib:ecc_test(undefined, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_ecdsa_server_ecdhe_ecdsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                       {client_chain, Default}], 
                                                      ecdhe_ecdsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, sect571r1]}],
    case ssl_test_lib:supported_eccs(ECCOpts) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_ecdsa_server_ecdhe_rsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                       {client_chain, Default}],
                                                      ecdhe_ecdsa, ecdhe_rsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, sect571r1]}],
    case ssl_test_lib:supported_eccs(ECCOpts) of
        true -> ssl_test_lib:ecc_test(undefined, COpts, SOpts, [], ECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_ecdsa_server_ecdhe_ecdsa_client_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                       {client_chain, Default}],
                                                      ecdhe_ecdsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{eccs, [secp256r1, sect571r1]}],
    case ssl_test_lib:supported_eccs(ECCOpts) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, ECCOpts, [], Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_rsa_server_ecdhe_ecdsa_client_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default}, 
                                                       {client_chain, Default}],
                                                      ecdhe_rsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config), 
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCOpts = [{eccs, [secp256r1, sect571r1]}],
    case ssl_test_lib:supported_eccs(ECCOpts) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, ECCOpts, [], Config);
        false -> {skip, "unsupported named curves"}
    end.
