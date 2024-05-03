%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("ssl/src/ssl_record.hrl").
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
-export([ecc_default_order/1,
         ecc_default_order_custom_curves/1,
         ecc_client_order/1,
         ecc_client_order_custom_curves/1,
         ecc_unknown_curve_ecdhe_ecdsa/1,
         ecc_unknown_curve_ecdhe_rsa/1,
         client_ecdh_rsa_server_ecdhe_ecdsa_server_custom/1,
         client_ecdh_rsa_server_ecdhe_rsa_server_custom/1,
         client_ecdhe_rsa_server_ecdhe_ecdsa_server_custom/1,
         client_ecdhe_rsa_server_ecdhe_rsa_server_custom/1,
         client_ecdhe_rsa_server_ecdh_rsa_server_custom/1,
         client_ecdhe_ecdsa_server_ecdhe_ecdsa_server_custom/1,
         client_ecdhe_ecdsa_server_ecdhe_rsa_server_custom/1,
         client_ecdhe_ecdsa_server_ecdhe_ecdsa_client_custom/1,
         client_ecdhe_rsa_server_ecdhe_ecdsa_client_custom/1,
         client_ecdsa_server_ecdsa_with_raw_key/1,
         mix_sign/1
        ]).

-define(DEFAULT_ECDSA, secp256r1).

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
     {'tlsv1.2', [], [mix_sign | test_cases()]},
     {'tlsv1.1', [], test_cases()},
     {'tlsv1', [], test_cases()},
     {'dtlsv1.2', [], [mix_sign | test_cases()]},
     {'dtlsv1', [], test_cases()}
    ].

test_cases()->
    misc() ++ ecc_negotiation().

misc()->
    [client_ecdsa_server_ecdsa_with_raw_key].

ecc_negotiation() ->
    [ecc_default_order,
     ecc_default_order_custom_curves,
     ecc_client_order,
     ecc_client_order_custom_curves,
     ecc_unknown_curve_ecdhe_ecdsa,
     ecc_unknown_curve_ecdhe_rsa,
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
    case ssl_test_lib:is_protocol_version(GroupName) of
	true ->
            ?CT_LOG("Ciphers: ~p~n ", [ssl:cipher_suites(default, GroupName)]),
            ssl_test_lib:init_per_group(GroupName,
                                        [{client_type, erlang},
                                         {server_type, erlang},
                                         {version, GroupName} | Config]);
        false ->
            Config
    end.

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

%%--------------------------------------------------------------------

init_per_testcase(TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    end_per_testcase(TestCase, Config),
    ssl:start(),
    ct:timetrap({seconds, 5}),
    Config.

end_per_testcase(_TestCase, Config) ->
    application:stop(ssl),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%% Test different certificate chain types, note that it is the servers
%% chain that affect what cipher suite that will be chosen

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
    {Server, Port} =
        ssl_test_lib:start_server_with_raw_key(SType,
                                               [{key, ServerKey} | proplists:delete(keyfile, SOpts)],
                                               Config),
    Client = ssl_test_lib:start_client(CType, Port, COpts, Config),
    ssl_test_lib:gen_check_result(Server, SType, Client, CType),
    ssl_test_lib:stop(Server, Client).

ecc_default_order(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa,
                                                        Config, ?DEFAULT_ECDSA),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),

    SECCOpts = [],
    case ssl_test_lib:supported_eccs([{eccs, [DefaultCurve]}], Version) of
        true ->  ssl_test_lib:ecc_test(DefaultCurve, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
     end.

ecc_default_order_custom_curves(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa,
                                                        Config, ?DEFAULT_ECDSA),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    SECCOpts = [{eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true ->  ssl_test_lib:ecc_test(DefaultCurve, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

ecc_client_order(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve =  hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa,
                                                        Config, ?DEFAULT_ECDSA),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    SECCOpts = [{honor_ecc_order, false}],
    case ssl_test_lib:supported_eccs([{eccs, [DefaultCurve]}], Version) of
        true -> ssl_test_lib:ecc_test(DefaultCurve, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

ecc_client_order_custom_curves(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                        {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa,
                                                        Config, ?DEFAULT_ECDSA),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    SECCOpts = [{honor_ecc_order, false}, {eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(DefaultCurve, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

ecc_unknown_curve_ecdhe_ecdsa(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                       {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECCALL = ssl:eccs(),
    SECCOpts = [{eccs, [hd(ECCALL)]}],
    CECCOpts = [{eccs, tl(ECCALL)}],
    ssl_test_lib:ecc_test_error(COpts, SOpts, CECCOpts, SECCOpts, Config).

ecc_unknown_curve_ecdhe_rsa(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                        ecdhe_rsa, ecdhe_rsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    Version = proplists:get_value(version, Config),
    ECCs = ssl:eccs(Version),
    CECCOpts = [{eccs, [hd(ECCs)]}],
    SECCOpts = [{eccs, tl(ECCs)}],
    ssl_test_lib:ecc_test_error(COpts, SOpts, CECCOpts, SECCOpts, Config).

client_ecdh_rsa_server_ecdhe_ecdsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                       {client_chain, Default}],
                                                        ecdh_rsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    SECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdh_rsa_server_ecdhe_rsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                        ecdh_rsa, ecdhe_rsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    SECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, DefaultCurve]}],
    
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_rsa_server_ecdhe_ecdsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                        ecdhe_rsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),

    SECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_rsa_server_ecdhe_rsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                        ecdhe_rsa, ecdhe_rsa, Config),

    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),

    SECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.
client_ecdhe_rsa_server_ecdh_rsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    Ext = x509_test:extensions([{key_usage, [keyEncipherment]}]),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, [[], [], [{extensions, Ext}]]},
                                                         {client_chain, Default}],
                                                        ecdhe_rsa, ecdh_rsa, Config),

    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    SECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, DefaultCurve]}],
    Expected = secp256r1, %% The certificate curve
    
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(Expected, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_ecdsa_server_ecdhe_ecdsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                       {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),

    SECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_ecdsa_server_ecdhe_rsa_server_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_rsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    SECCOpts = [{honor_ecc_order, true}, {eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(SECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, [], SECCOpts, Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_ecdsa_server_ecdhe_ecdsa_client_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = hd(ssl:eccs(Version)),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                       {client_chain, Default}],
                                                        ecdhe_ecdsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),

    CECCOpts = [{eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(CECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, CECCOpts, [], Config);
        false -> {skip, "unsupported named curves"}
    end.

client_ecdhe_rsa_server_ecdhe_ecdsa_client_custom(Config) ->
    Default = ssl_test_lib:default_cert_chain_conf(),
    Version = proplists:get_value(version, Config),
    DefaultCurve = pubkey_cert_records:namedCurves(hd(tls_v1:ecc_curves(?TLS_1_0))),
    {COpts0, SOpts0} = ssl_test_lib:make_ec_cert_chains([{server_chain, Default},
                                                         {client_chain, Default}],
                                                         ecdhe_rsa, ecdhe_ecdsa, Config),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    CECCOpts = [{eccs, [secp256r1, DefaultCurve]}],
    case ssl_test_lib:supported_eccs(CECCOpts, Version) of
        true -> ssl_test_lib:ecc_test(secp256r1, COpts, SOpts, CECCOpts, [], Config);
        false -> {skip, "unsupported named curves"}
    end.

mix_sign(Config) ->
    mix_sign_rsa_peer(Config),
    mix_sign_ecdsa_peer(Config).

mix_sign_ecdsa_peer(Config) ->
    {COpts0, SOpts0} = ssl_test_lib:make_mix_cert([{mix, peer_ecc} |Config]),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECDHE_ECDSA =
        ssl:filter_cipher_suites(ssl:cipher_suites(default, 'tlsv1.2'),
                                 [{key_exchange, fun(ecdhe_ecdsa) -> true; (_) -> false end}]),
    ssl_test_lib:basic_test(COpts, [{ciphers, ECDHE_ECDSA} | SOpts], Config).


mix_sign_rsa_peer(Config) ->
    {COpts0, SOpts0} = ssl_test_lib:make_mix_cert([{mix, peer_rsa} |Config]),
    COpts = ssl_test_lib:ssl_options(COpts0, Config),
    SOpts = ssl_test_lib:ssl_options(SOpts0, Config),
    ECDHE_RSA =
        ssl:filter_cipher_suites(ssl:cipher_suites(default, 'tlsv1.2'),
                                 [{key_exchange, fun(ecdhe_rsa) -> true; (_) -> false end}]),
    ssl_test_lib:basic_test(COpts, [{ciphers, ECDHE_RSA} | SOpts], Config).


