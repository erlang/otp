%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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

-module(ssl_cipher_suite_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
%% Callback functions
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Testcases
-export([dhe_rsa_3des_ede_cbc/1,
         dhe_rsa_aes_128_cbc/1,
         dhe_rsa_aes_128_gcm/1,
         dhe_rsa_aes_256_cbc/1,
         dhe_rsa_aes_256_gcm/1,
         dhe_rsa_chacha20_poly1305/1,
         ecdhe_rsa_3des_ede_cbc/1,
         ecdhe_rsa_rc4_128/1,
         ecdhe_rsa_aes_128_cbc/1,
         ecdhe_rsa_aes_128_gcm/1,
         ecdhe_rsa_aes_256_cbc/1,
         ecdhe_rsa_aes_256_gcm/1,
         ecdhe_rsa_chacha20_poly1305/1,
         ecdhe_ecdsa_rc4_128/1,
         ecdhe_ecdsa_3des_ede_cbc/1,
         ecdhe_ecdsa_aes_128_cbc/1,
         ecdhe_ecdsa_aes_128_gcm/1,
         ecdhe_ecdsa_aes_256_cbc/1,
         ecdhe_ecdsa_aes_256_gcm/1,
         ecdhe_ecdsa_chacha20_poly1305/1,
         rsa_des_cbc/1,
         rsa_3des_ede_cbc/1,
         rsa_aes_128_cbc/1,
         rsa_aes_256_cbc/1,
         rsa_aes_128_gcm/1,
         rsa_aes_256_gcm/1,
         rsa_rc4_128/1,
         dhe_dss_des_cbc/1,
         dhe_dss_3des_ede_cbc/1,
         dhe_dss_aes_128_cbc/1,
         dhe_dss_aes_256_cbc/1,
         dhe_dss_aes_128_gcm/1,
         dhe_dss_aes_256_gcm/1,
         srp_rsa_3des_ede_cbc/1,
         srp_rsa_aes_128_cbc/1,
         srp_rsa_aes_256_cbc/1,
         srp_dss_3des_ede_cbc/1,
         srp_dss_aes_128_cbc/1,
         srp_dss_aes_256_cbc/1,
         rsa_psk_3des_ede_cbc/1,
         rsa_psk_rc4_128/1,
         rsa_psk_aes_128_cbc/1,
         rsa_psk_aes_256_cbc/1,
         dhe_psk_des_cbc/1,
         dhe_psk_3des_ede_cbc/1,
         dhe_psk_rc4_128/1,
         dhe_psk_aes_128_cbc/1,
         dhe_psk_aes_128_gcm/1,
         dhe_psk_aes_128_ccm/1,
         dhe_psk_aes_128_ccm_8/1,
         dhe_psk_aes_256_cbc/1,
         dhe_psk_aes_256_gcm/1,
         dhe_psk_aes_256_ccm/1,
         dhe_psk_aes_256_ccm_8/1,
         ecdhe_psk_3des_ede_cbc/1,
         ecdhe_psk_rc4_128/1,
         ecdhe_psk_aes_128_cbc/1,
         ecdhe_psk_aes_128_gcm/1,
         ecdhe_psk_aes_128_ccm/1,
         ecdhe_psk_aes_128_ccm_8/1,
         ecdhe_psk_aes_256_cbc/1,
         ecdhe_psk_aes_256_gcm/1,
         srp_anon_3des_ede_cbc/1,
         srp_anon_aes_128_cbc/1,
         srp_anon_aes_256_cbc/1,
         psk_3des_ede_cbc/1,
         psk_rc4_128/1,
         psk_aes_128_cbc/1,
         psk_aes_128_gcm/1,
         psk_aes_128_ccm/1,
         psk_aes_128_ccm_8/1,
         psk_aes_256_cbc/1,
         psk_aes_256_gcm/1,
         psk_aes_256_ccm/1,
         psk_aes_256_ccm_8/1,
         dh_anon_rc4_128/1,
         dh_anon_3des_ede_cbc/1,
         dh_anon_aes_128_cbc/1,
         dh_anon_aes_128_gcm/1,
         dh_anon_aes_256_cbc/1,
         dh_anon_aes_256_gcm/1,
         ecdh_anon_3des_ede_cbc/1,
         ecdh_anon_aes_128_cbc/1,
         ecdh_anon_aes_256_cbc/1,
         aes_256_gcm_sha384/1,
         aes_128_gcm_sha256/1,
         chacha20_poly1305_sha256/1,
         aes_128_ccm_sha256/1,
         aes_128_ccm_8_sha256/1,
         ecdhe_ecdsa_with_aes_128_ccm/1,
         ecdhe_ecdsa_with_aes_256_ccm/1,
         ecdhe_ecdsa_with_aes_128_ccm_8/1,
         ecdhe_ecdsa_with_aes_256_ccm_8/1
        ]).

-define(TIMEOUT, {seconds, 10}).

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
     {'tlsv1.3', [], tls_1_3_kex()},
     {'tlsv1.2', [], kex()},
     {'tlsv1.1', [], kex()},
     {'tlsv1', [], kex()},
     {'dtlsv1.2', [], kex()},
     {'dtlsv1', [], kex()},
     {dhe_rsa, [],[dhe_rsa_3des_ede_cbc, 
                   dhe_rsa_aes_128_cbc,
                   dhe_rsa_aes_128_gcm,
                   dhe_rsa_aes_256_cbc,
                   dhe_rsa_aes_256_gcm,
                   dhe_rsa_chacha20_poly1305
                  ]},
     {ecdhe_rsa, [], [ecdhe_rsa_3des_ede_cbc, 
                      ecdhe_rsa_aes_128_cbc,
                      ecdhe_rsa_aes_128_gcm,
                      ecdhe_rsa_aes_256_cbc,
                      ecdhe_rsa_aes_256_gcm,
                      ecdhe_rsa_chacha20_poly1305
                    ]},
     {ecdhe_1_3_rsa_cert, [], tls_1_3_cipher_suites()},
     {ecdhe_ecdsa, [],[ecdhe_ecdsa_rc4_128, 
                       ecdhe_ecdsa_3des_ede_cbc, 
                       ecdhe_ecdsa_aes_128_cbc,
                       ecdhe_ecdsa_aes_128_gcm,
                       ecdhe_ecdsa_aes_256_cbc,
                       ecdhe_ecdsa_aes_256_gcm,
                       ecdhe_ecdsa_chacha20_poly1305,
                       ecdhe_ecdsa_with_aes_128_ccm,
                       ecdhe_ecdsa_with_aes_256_ccm,
                       ecdhe_ecdsa_with_aes_128_ccm_8,
                       ecdhe_ecdsa_with_aes_256_ccm_8
                      ]},
     {rsa, [], [rsa_3des_ede_cbc, 
                rsa_aes_128_cbc,
                rsa_aes_256_cbc,
                rsa_rc4_128
               ]},
     {dhe_dss, [], [dhe_dss_3des_ede_cbc, 
                    dhe_dss_aes_128_cbc,
                    dhe_dss_aes_256_cbc]},
     {srp_rsa, [], [srp_rsa_3des_ede_cbc, 
                    srp_rsa_aes_128_cbc,
                    srp_rsa_aes_256_cbc]},
     {srp_dss, [], [srp_dss_3des_ede_cbc, 
                    srp_dss_aes_128_cbc,
                    srp_dss_aes_256_cbc]},
     {rsa_psk, [], [rsa_psk_3des_ede_cbc,                    
                    rsa_psk_rc4_128,
                    rsa_psk_aes_128_cbc,
                    rsa_psk_aes_256_cbc
                   ]},
     {dh_anon, [], [dh_anon_rc4_128,
                    dh_anon_3des_ede_cbc, 
                    dh_anon_aes_128_cbc,
                    dh_anon_aes_128_gcm,
                    dh_anon_aes_256_cbc,
                    dh_anon_aes_256_gcm]},
     {ecdh_anon, [], [ecdh_anon_3des_ede_cbc, 
                      ecdh_anon_aes_128_cbc,
                      ecdh_anon_aes_256_cbc
                     ]},     
     {srp_anon, [], [srp_anon_3des_ede_cbc, 
                     srp_anon_aes_128_cbc,
                     srp_anon_aes_256_cbc]},
     {psk, [], [psk_3des_ede_cbc,                    
                psk_rc4_128,
                psk_aes_128_cbc,
                psk_aes_128_ccm,
                psk_aes_128_ccm_8,
                psk_aes_256_cbc,
                psk_aes_256_ccm,
                psk_aes_256_ccm_8
               ]},
     {dhe_psk, [], [dhe_psk_des_cbc,
                    dhe_psk_3des_ede_cbc,
                    dhe_psk_rc4_128,
                    dhe_psk_aes_128_cbc,
                    dhe_psk_aes_128_gcm,
                    dhe_psk_aes_128_ccm,
                    dhe_psk_aes_128_ccm_8,
                    dhe_psk_aes_256_cbc,
                    dhe_psk_aes_256_gcm,
                    dhe_psk_aes_256_ccm,
                    dhe_psk_aes_256_ccm_8
               ]},
     {ecdhe_psk, [], [ecdhe_psk_3des_ede_cbc,                    
                      ecdhe_psk_rc4_128,
                      ecdhe_psk_aes_128_cbc,
                      ecdhe_psk_aes_128_gcm,
                      ecdhe_psk_aes_128_ccm,
                      ecdhe_psk_aes_128_ccm_8,
                      ecdhe_psk_aes_256_cbc,
                      ecdhe_psk_aes_256_gcm
               ]}
    ].


tls_1_3_kex() ->
    [{group, ecdhe_1_3_rsa_cert}].
    
tls_1_3_cipher_suites() ->
    [aes_256_gcm_sha384,
     aes_128_gcm_sha256,
     chacha20_poly1305_sha256,
     aes_128_ccm_sha256,
     aes_128_ccm_8_sha256
    ].

kex() ->
     rsa() ++ ecdsa() ++ dss() ++ anonymous().

rsa() ->
    [{group, dhe_rsa},
     {group, ecdhe_rsa},
     {group, rsa},
     {group, srp_rsa},
     {group, rsa_psk}
    ].

ecdsa() ->
    [{group, ecdhe_ecdsa}].
    
dss() ->
    [{group, dhe_dss},
     {group, srp_dss}].

anonymous() ->
    [{group, dh_anon},
     {group, ecdh_anon},
     {group, psk},
     {group, dhe_psk},
     {group, ecdhe_psk},
     {group, srp_anon}
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
    application:stop(crypto).
init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_protocol_version(GroupName) of
        true  ->
            ssl_test_lib:init_per_group(GroupName, [{client_type, erlang},
                                                    {server_type, erlang},
                                                    {version, GroupName} | Config]);
        false -> 
            do_init_per_group(GroupName, Config)
    end.

do_init_per_group(GroupName, Config) when GroupName == ecdhe_1_3_rsa_cert ->    
    case proplists:get_bool(ecdh, proplists:get_value(public_keys, crypto:supports())) of
        true ->
            init_certs(GroupName, Config);
        false ->
            {skip, "Missing EC crypto support"}
    end;
do_init_per_group(GroupName, Config) when GroupName == ecdh_anon;
                                       GroupName == ecdhe_rsa;
                                       GroupName == ecdhe_psk ->
    case proplists:get_bool(ecdh, proplists:get_value(public_keys, crypto:supports())) of
        true ->
            init_certs(GroupName, Config);
        false ->
            {skip, "Missing EC crypto support"}
    end;
do_init_per_group(ecdhe_ecdsa = GroupName, Config) ->
    PKAlg = proplists:get_value(public_keys, crypto:supports()),
    case lists:member(ecdh, PKAlg) andalso lists:member(ecdsa, PKAlg) of
        true ->
            init_certs(GroupName, Config);
        false ->
            {skip, "Missing EC crypto support"}
    end;
do_init_per_group(dhe_dss = GroupName, Config) ->
    PKAlg = proplists:get_value(public_keys, crypto:supports()),
    case lists:member(dss, PKAlg) andalso lists:member(dh, PKAlg) of
        true ->
            init_certs(GroupName, Config);
        false ->
            {skip, "Missing DSS crypto support"}
    end;
do_init_per_group(srp_dss = GroupName, Config) ->
    PKAlg = proplists:get_value(public_keys, crypto:supports()),
    case lists:member(dss, PKAlg) andalso lists:member(srp, PKAlg) of
        true ->
            init_certs(GroupName, Config);
        false ->
            {skip, "Missing DSS_SRP crypto support"}
    end;
do_init_per_group(GroupName, Config) when GroupName == srp_anon;
                                          GroupName == srp_rsa ->
    PKAlg = proplists:get_value(public_keys, crypto:supports()),
    case lists:member(srp, PKAlg) of
        true ->
            init_certs(GroupName, Config);
        false ->
            {skip, "Missing SRP crypto support"}
    end;
do_init_per_group(dhe_psk = GroupName, Config) ->
    PKAlg = proplists:get_value(public_keys, crypto:supports()),
    case lists:member(dh, PKAlg) of
        true ->
            init_certs(GroupName, Config);
        false ->
            {skip, "Missing SRP crypto support"}
    end;
do_init_per_group(GroupName, Config) ->
   init_certs(GroupName, Config).
  
end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(TestCase, Config) when TestCase == psk_3des_ede_cbc;
                                         TestCase == srp_anon_3des_ede_cbc;
                                         TestCase == dhe_psk_3des_ede_cbc;
                                         TestCase == ecdhe_psk_3des_ede_cbc;
                                         TestCase == srp_rsa_3des_ede_cbc;
                                         TestCase == srp_dss_3des_ede_cbc;
                                         TestCase == rsa_psk_3des_ede_cbc;
                                         TestCase == rsa_3des_ede_cbc;
                                         TestCase == dhe_rsa_3des_ede_cbc;
                                         TestCase == dhe_dss_3des_ede_cbc;
                                         TestCase == ecdhe_rsa_3des_ede_cbc;
                                         TestCase == srp_anon_dss_3des_ede_cbc;
                                         TestCase == dh_anon_3des_ede_cbc;
                                         TestCase == ecdh_anon_3des_ede_cbc;
                                         TestCase == ecdhe_ecdsa_3des_ede_cbc ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    case lists:member(des_ede3_cbc, SupCiphers) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing 3DES crypto support"}
    end;
init_per_testcase(TestCase, Config) when TestCase == psk_rc4_128;
                                         TestCase == ecdhe_psk_rc4_128;
                                         TestCase == dhe_psk_rc4_128;
                                         TestCase == rsa_psk_rc4_128;
                                         TestCase == rsa_rc4_128;
                                         TestCase == ecdhe_rsa_rc4_128;
                                         TestCase == ecdhe_ecdsa_rc4_128;
                                         TestCase == dh_anon_rc4_128 ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    case lists:member(rc4, SupCiphers) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing RC4 crypto support"}
    end;
init_per_testcase(TestCase, Config) when TestCase == psk_aes_128_ccm_8;
                                         TestCase == dhe_psk_aes_128_ccm_8;
                                         TestCase == ecdhe_psk_aes_128_ccm_8 ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    case lists:member(aes_128_ccm, SupCiphers) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing AES_128_CCM crypto support"}
    end;
init_per_testcase(TestCase, Config) when TestCase == psk_aes_256_ccm_8;
                                         TestCase == psk_aes_256_ccm_8;
                                         TestCase == dhe_psk_aes_256_ccm_8;
                                         TestCase == ecdhe_psk_aes_256_ccm_8 ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    case lists:member(aes_256_ccm, SupCiphers) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing AES_256_CCM crypto support"}
    end;
init_per_testcase(aes_256_gcm_sha384, Config) ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
      SupHashs = proplists:get_value(hashs, crypto:supports()),
    case (lists:member(aes_256_gcm, SupCiphers)) andalso
        (lists:member(sha384, SupHashs))
    of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing AES_256_GCM_SHA384 crypto support"}
    end;
init_per_testcase(aes_128_gcm_sha256, Config) ->
      SupCiphers = proplists:get_value(ciphers, crypto:supports()),
      SupHashs = proplists:get_value(hashs, crypto:supports()),
      case (lists:member(aes_128_gcm, SupCiphers)) andalso
          (lists:member(sha256, SupHashs))
      of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
          _ ->
            {skip, "Missing AES_128_GCM_SHA256 crypto support"}
    end;
init_per_testcase(chacha20_poly1305_sha256, Config) ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    SupHashs = proplists:get_value(hashs, crypto:supports()),
    case (lists:member(chacha20_poly1305, SupCiphers)) andalso
        (lists:member(sha256, SupHashs))
    of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing chacha20_poly1305_sha256 crypto support"}
    end;
init_per_testcase(aes_128_ccm_sha256, Config) ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    SupHashs = proplists:get_value(hashs, crypto:supports()),
    case (lists:member(aes_128_ccm, SupCiphers)) andalso
        (lists:member(sha256, SupHashs)) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing AES_128_CCM_SHA256 crypto support"}
    end;
init_per_testcase(aes_128_ccm_8_sha256, Config) ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    SupHashs = proplists:get_value(hashs, crypto:supports()),
    case (lists:member(aes_128_ccm, SupCiphers)) andalso
        (lists:member(sha256, SupHashs)) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing AES_128_CCM_8_SHA256 crypto support"}
    end;
init_per_testcase(TestCase, Config) when TestCase == ecdhe_ecdsa_with_aes_128_ccm;
                                         TestCase == ecdhe_ecdsa_with_aes_128_ccm_8->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    case lists:member(aes_128_ccm, SupCiphers) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing AES_128_CCM crypto support"}
    end;
init_per_testcase(TestCase, Config) when TestCase == ecdhe_ecdsa_with_aes_256_ccm;
                                         TestCase == ecdhe_ecdsa_with_aes_256_ccm_8 ->
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    case lists:member(aes_256_ccm, SupCiphers) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, "Missing AES_256_CCM crypto support"}
    end;
init_per_testcase(TestCase, Config) ->
    Cipher = ssl_test_lib:test_cipher(TestCase, Config),
    SupCiphers = proplists:get_value(ciphers, crypto:supports()),
    case lists:member(Cipher, SupCiphers) of
        true ->
            ct:timetrap(?TIMEOUT),
            Config;
        _ ->
            {skip, {Cipher, SupCiphers}}
    end.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Initializtion   ------------------------------------------
%%--------------------------------------------------------------------
init_certs(srp_rsa, Config) ->
    DefConf = ssl_test_lib:default_cert_chain_conf(),
    CertChainConf = ssl_test_lib:gen_conf(rsa, rsa, DefConf, DefConf),
    #{server_config := ServerOpts0,
      client_config := ClientOpts0}
        = public_key:pkix_test_data(CertChainConf),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerOpts = ssl_test_lib:sig_algs(rsa, Version) ++  ServerOpts0,
    ClientOpts = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,
    [{tls_config, #{server_config =>
                        [{user_lookup_fun, {fun ssl_test_lib:user_lookup/3, undefined}} | ServerOpts],
                    client_config =>
                        [{srp_identity, {"Test-User", "secret"}}, {verify, verify_none} | ClientOpts]}} |
     proplists:delete(tls_config, Config)];
init_certs(srp_anon, Config) ->
    [{tls_config, #{server_config => [{user_lookup_fun, {fun ssl_test_lib:user_lookup/3, undefined}}],
                    client_config => [{srp_identity, {"Test-User", "secret"}}, {verify, verify_none}]}} |
     proplists:delete(tls_config, Config)];
init_certs(rsa_psk, Config) ->
    ClientExt = x509_test:extensions([{key_usage, [digitalSignature, keyEncipherment]}]),
    {ClientOpts, ServerOpts} = ssl_test_lib:make_rsa_cert_chains([{server_chain,
                                                                   [[],[],[{extensions, ClientExt}]]}],
                                                                 Config, "_peer_keyEncipherment"),
    PskSharedSecret = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,
    [{tls_config, #{server_config => [{user_lookup_fun, {fun ssl_test_lib:user_lookup/3, PskSharedSecret}} | ServerOpts],
                    client_config => [{psk_identity, "Test-User"},
                                      {user_lookup_fun, {fun ssl_test_lib:user_lookup/3, PskSharedSecret}},
                                      {verify, verify_none} | ClientOpts]}} |
     proplists:delete(tls_config, Config)];
init_certs(rsa, Config) ->
    ClientExt = x509_test:extensions([{key_usage, [digitalSignature, keyEncipherment]}]),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{server_chain,
                                                                   [[],[],[{extensions, ClientExt}]]}],
                                                                 Config, "_peer_keyEncipherment"),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerOpts = ssl_test_lib:sig_algs(rsa, Version) ++  ServerOpts0,
    ClientOpts = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,
    [{tls_config, #{server_config => ServerOpts,
                    client_config => ClientOpts}} |
     proplists:delete(tls_config, Config)];
init_certs(ecdhe_1_3_rsa_cert, Config) ->
    ClientExt = x509_test:extensions([{key_usage, [digitalSignature]}]),
    {ClientOpts0, ServerOpts0} = ssl_test_lib:make_rsa_cert_chains([{server_chain,
                                                                   [[],[],[{extensions, ClientExt}]]}],
                                                                 Config, "_peer_rsa_digitalsign"),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerOpts = ssl_test_lib:sig_algs(rsa, Version) ++  ServerOpts0,
    ClientOpts = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,
    [{tls_config, #{server_config => ServerOpts,
                    client_config => ClientOpts}} |
     proplists:delete(tls_config, Config)];
init_certs(dhe_dss, Config) ->
    DefConf = ssl_test_lib:default_cert_chain_conf(),
    CertChainConf = ssl_test_lib:gen_conf(dsa, dsa, DefConf, DefConf), 
    #{server_config := ServerOpts0,
      client_config := ClientOpts0}
        = public_key:pkix_test_data(CertChainConf),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerOpts = ssl_test_lib:sig_algs(dsa, Version) ++  ServerOpts0,
    ClientOpts = ssl_test_lib:sig_algs(dsa, Version) ++ ClientOpts0,
    [{tls_config, #{server_config => ServerOpts,
                    client_config => ClientOpts}} |
     proplists:delete(tls_config, Config)];
init_certs(srp_dss, Config) ->
    DefConf = ssl_test_lib:default_cert_chain_conf(),
    CertChainConf = ssl_test_lib:gen_conf(dsa, dsa, DefConf, DefConf),
    #{server_config := ServerOpts0,
      client_config := ClientOpts0}
        = public_key:pkix_test_data(CertChainConf),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerOpts = ssl_test_lib:sig_algs(dsa, Version) ++  ServerOpts0,
    ClientOpts = ssl_test_lib:sig_algs(dsa, Version) ++ ClientOpts0,
    [{tls_config, #{server_config => [{user_lookup_fun, {fun ssl_test_lib:user_lookup/3, undefined}} | ServerOpts],
                    client_config => [{srp_identity, {"Test-User", "secret"}} | ClientOpts]}} |
       proplists:delete(tls_config, Config)];
init_certs(GroupName, Config) when GroupName == dhe_rsa;
                                   GroupName == ecdhe_rsa ->
    DefConf = ssl_test_lib:default_cert_chain_conf(),
    CertChainConf = ssl_test_lib:gen_conf(rsa, rsa, DefConf, DefConf),
    #{server_config := ServerOpts0,
      client_config := ClientOpts0}
        = public_key:pkix_test_data(CertChainConf),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerOpts = ssl_test_lib:sig_algs(rsa, Version) ++  ServerOpts0,
    ClientOpts = ssl_test_lib:sig_algs(rsa, Version) ++ ClientOpts0,
    [{tls_config, #{server_config => ServerOpts,
                    client_config => ClientOpts}} |
     proplists:delete(tls_config, Config)];
init_certs(GroupName, Config) when GroupName == dhe_ecdsa;
                                   GroupName == ecdhe_ecdsa ->
    DefConf = ssl_test_lib:default_cert_chain_conf(),
    CertChainConf = ssl_test_lib:gen_conf(ecdsa, ecdsa, DefConf, DefConf),
    #{server_config := ServerOpts0,
      client_config := ClientOpts0}
        = public_key:pkix_test_data(CertChainConf),
    Version = ssl_test_lib:n_version(proplists:get_value(version, Config)),
    ServerOpts = ssl_test_lib:sig_algs(ecdsa, Version) ++  ServerOpts0,
    ClientOpts = ssl_test_lib:sig_algs(ecdsa, Version) ++ ClientOpts0,
    [{tls_config, #{server_config => ServerOpts,
                    client_config => ClientOpts}} |
     proplists:delete(tls_config, Config)];
init_certs(GroupName, Config) when GroupName == psk;
                                   GroupName == dhe_psk;
                                   GroupName == ecdhe_psk ->
    PskSharedSecret = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,
    [{tls_config, #{server_config => [{user_lookup_fun, {fun ssl_test_lib:user_lookup/3, PskSharedSecret}}],
                    client_config => [{verify, verify_none}, {psk_identity, "Test-User"},
                                      {user_lookup_fun, {fun ssl_test_lib:user_lookup/3, PskSharedSecret}}]}} |
     proplists:delete(tls_config, Config)];
init_certs(srp, Config) ->
      [{tls_config, #{server_config => [{user_lookup_fun, {fun ssl_test_lib:user_lookup/3, undefined}}],
                      client_config => [{verify, verify_none},{srp_identity, {"Test-User", "secret"}}]}} |
       proplists:delete(tls_config, Config)];
init_certs(_GroupName, Config) ->
    %% Anonymous does not need certs
     [{tls_config, #{server_config => [{verify, verify_none}],
                     client_config => [{verify, verify_none}]}} |
       proplists:delete(tls_config, Config)].

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

aes_256_gcm_sha384(Config) when is_list(Config)->
    Version = ssl_test_lib:protocol_version(Config),
    cipher_suite_test(ssl:str_to_suite("TLS_AES_256_GCM_SHA384"), Version, Config).

aes_128_gcm_sha256(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    cipher_suite_test(ssl:str_to_suite("TLS_AES_128_GCM_SHA256"), Version, Config). 

chacha20_poly1305_sha256(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    cipher_suite_test(ssl:str_to_suite("TLS_CHACHA20_POLY1305_SHA256"), Version, Config). 

aes_128_ccm_sha256(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    cipher_suite_test(ssl:str_to_suite("TLS_AES_128_CCM_SHA256"), Version, Config).

aes_128_ccm_8_sha256(Config) when is_list(Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    cipher_suite_test(ssl:str_to_suite("TLS_AES_128_CCM_8_SHA256"), Version, Config).

%%--------------------------------------------------------------------
%% SRP --------------------------------------------------------
%%--------------------------------------------------------------------
srp_rsa_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(srp_rsa, '3des_ede_cbc', Config).                 
    
srp_rsa_aes_128_cbc(Config) when is_list(Config) ->
   run_ciphers_test(srp_rsa, 'aes_128_cbc', Config).             

srp_rsa_aes_256_cbc(Config) when is_list(Config) ->
   run_ciphers_test(srp_rsa, 'aes_256_cbc', Config).             

srp_dss_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(srp_dss, '3des_ede_cbc', Config).                 
    
srp_dss_aes_128_cbc(Config) when is_list(Config) ->
   run_ciphers_test(srp_dss, 'aes_128_cbc', Config).             

srp_dss_aes_256_cbc(Config) when is_list(Config) ->
   run_ciphers_test(srp_dss, 'aes_256_cbc', Config).     

%%--------------------------------------------------------------------
%% PSK --------------------------------------------------------
%%--------------------------------------------------------------------
rsa_psk_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(rsa_psk, '3des_ede_cbc', Config).            

rsa_psk_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(rsa_psk, 'aes_128_cbc', Config).             

rsa_psk_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(rsa_psk, 'aes_256_cbc', Config). 
     
rsa_psk_rc4_128(Config) when is_list(Config) ->
    run_ciphers_test(rsa_psk, 'rc4_128', Config).    
         
%%--------------------------------------------------------------------
%% RSA --------------------------------------------------------
%%--------------------------------------------------------------------
rsa_des_cbc(Config) when is_list(Config) ->
    run_ciphers_test(rsa, 'des_cbc', Config).            

rsa_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(rsa, '3des_ede_cbc', Config).            

rsa_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(rsa, 'aes_128_cbc', Config).             

rsa_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(rsa, 'aes_256_cbc', Config).

rsa_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(rsa, 'aes_128_gcm', Config).             

rsa_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(rsa, 'aes_256_gcm', Config).

rsa_rc4_128(Config) when is_list(Config) ->
    run_ciphers_test(rsa, 'rc4_128', Config).    
%%--------------------------------------------------------------------
%% DHE_RSA --------------------------------------------------------
%%--------------------------------------------------------------------
dhe_rsa_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_rsa, '3des_ede_cbc', Config).         

dhe_rsa_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_rsa, 'aes_128_cbc', Config).   

dhe_rsa_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(dhe_rsa, 'aes_128_gcm', Config).   

dhe_rsa_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_rsa, 'aes_256_cbc', Config).   

dhe_rsa_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(dhe_rsa, 'aes_256_gcm', Config).

dhe_rsa_chacha20_poly1305(Config) when is_list(Config) ->
    run_ciphers_test(dhe_rsa, 'chacha20_poly1305', Config).
%%--------------------------------------------------------------------
%% ECDHE_RSA --------------------------------------------------------
%%--------------------------------------------------------------------
ecdhe_rsa_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_rsa, '3des_ede_cbc', Config).         

ecdhe_rsa_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_rsa, 'aes_128_cbc', Config).         

ecdhe_rsa_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_rsa, 'aes_128_gcm', Config).         

ecdhe_rsa_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_rsa, 'aes_256_cbc', Config).   

ecdhe_rsa_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_rsa, 'aes_256_gcm', Config).   

ecdhe_rsa_rc4_128(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_rsa, 'rc4_128', Config).      

ecdhe_rsa_chacha20_poly1305(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_rsa, 'chacha20_poly1305', Config).

%%--------------------------------------------------------------------
%% ECDHE_ECDSA --------------------------------------------------------
%%--------------------------------------------------------------------
ecdhe_ecdsa_rc4_128(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'rc4_128', Config).         

ecdhe_ecdsa_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, '3des_ede_cbc', Config).         

ecdhe_ecdsa_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'aes_128_cbc', Config).         

ecdhe_ecdsa_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'aes_128_gcm', Config).         

ecdhe_ecdsa_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'aes_256_cbc', Config).   

ecdhe_ecdsa_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'aes_256_gcm', Config).   

ecdhe_ecdsa_chacha20_poly1305(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'chacha20_poly1305', Config).

ecdhe_ecdsa_with_aes_128_ccm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'aes_128_ccm', Config). 

ecdhe_ecdsa_with_aes_256_ccm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'aes_256_ccm', Config). 

ecdhe_ecdsa_with_aes_128_ccm_8(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'aes_128_ccm_8', Config). 

ecdhe_ecdsa_with_aes_256_ccm_8(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_ecdsa, 'aes_256_ccm_8', Config). 
%%--------------------------------------------------------------------
%% DHE_DSS --------------------------------------------------------
%%--------------------------------------------------------------------
dhe_dss_des_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_dss, 'des_cbc', Config).            

dhe_dss_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_dss, '3des_ede_cbc', Config).            

dhe_dss_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_dss, 'aes_128_cbc', Config).             

dhe_dss_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_dss, 'aes_256_cbc', Config).

dhe_dss_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(dhe_dss, 'aes_128_gcm', Config).             

dhe_dss_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(dhe_dss, 'aes_256_gcm', Config).

%%--------------------------------------------------------------------
%% Anonymous --------------------------------------------------------
%%--------------------------------------------------------------------
dh_anon_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dh_anon, '3des_ede_cbc', Config).         

dh_anon_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dh_anon, 'aes_128_cbc', Config).         

dh_anon_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(dh_anon, 'aes_128_gcm', Config).         

dh_anon_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dh_anon, 'aes_256_cbc', Config).   

dh_anon_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(dh_anon, 'aes_256_gcm', Config).   

dh_anon_rc4_128(Config) when is_list(Config) ->
    run_ciphers_test(dh_anon, 'rc4_128', Config).      

ecdh_anon_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdh_anon, '3des_ede_cbc', Config).         

ecdh_anon_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdh_anon, 'aes_128_cbc', Config).   

ecdh_anon_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdh_anon, 'aes_256_cbc', Config).   

srp_anon_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(srp_anon, '3des_ede_cbc', Config).                 
    
srp_anon_aes_128_cbc(Config) when is_list(Config) ->
   run_ciphers_test(srp_anon, 'aes_128_cbc', Config).             

srp_anon_aes_256_cbc(Config) when is_list(Config) ->
   run_ciphers_test(srp_anon, 'aes_256_cbc', Config).     

dhe_psk_des_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'des_cbc', Config).            

dhe_psk_rc4_128(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'rc4_128', Config).            

dhe_psk_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, '3des_ede_cbc', Config).            

dhe_psk_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'aes_128_cbc', Config).             

dhe_psk_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'aes_256_cbc', Config).

dhe_psk_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'aes_128_gcm', Config).             

dhe_psk_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'aes_256_gcm', Config).

dhe_psk_aes_128_ccm(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'aes_128_ccm', Config).             

dhe_psk_aes_256_ccm(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'aes_256_ccm', Config).

dhe_psk_aes_128_ccm_8(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'aes_128_ccm_8', Config).

dhe_psk_aes_256_ccm_8(Config) when is_list(Config) ->
    run_ciphers_test(dhe_psk, 'aes_256_ccm_8', Config).

ecdhe_psk_rc4_128(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_psk, 'rc4_128', Config).            

ecdhe_psk_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_psk, '3des_ede_cbc', Config).            

ecdhe_psk_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_psk, 'aes_128_cbc', Config).             

ecdhe_psk_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_psk, 'aes_256_cbc', Config).

ecdhe_psk_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_psk, 'aes_128_gcm', Config).             

ecdhe_psk_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_psk, 'aes_256_gcm', Config).

ecdhe_psk_aes_128_ccm(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_psk, 'aes_128_ccm', Config).             

ecdhe_psk_aes_128_ccm_8(Config) when is_list(Config) ->
    run_ciphers_test(ecdhe_psk, 'aes_128_ccm_8', Config).

psk_rc4_128(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'rc4_128', Config).            

psk_3des_ede_cbc(Config) when is_list(Config) ->
    run_ciphers_test(psk, '3des_ede_cbc', Config).            

psk_aes_128_cbc(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'aes_128_cbc', Config).             

psk_aes_256_cbc(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'aes_256_cbc', Config).

psk_aes_128_gcm(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'aes_128_gcm', Config).             

psk_aes_256_gcm(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'aes_256_gcm', Config).

psk_aes_128_ccm(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'aes_128_ccm', Config).             

psk_aes_256_ccm(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'aes_256_ccm', Config).

psk_aes_128_ccm_8(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'aes_128_ccm_8', Config).             

psk_aes_256_ccm_8(Config) when is_list(Config) ->
    run_ciphers_test(psk, 'aes_256_ccm_8', Config).

%%--------------------------------------------------------------------
%% Internal functions  ----------------------------------------------
%%--------------------------------------------------------------------
run_ciphers_test(Kex, Cipher, Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    TestCiphers = test_ciphers(Kex, Cipher, Version),                  
    
    case TestCiphers of
        [_|_] -> 
            lists:foreach(fun(TestCipher) -> 
                                  cipher_suite_test(TestCipher, Version, Config)
                          end, TestCiphers);
        []  ->
            {skip, {not_sup, Kex, Cipher, Version}}
    end.

cipher_suite_test(ErlangCipherSuite, Version, Config) ->
    #{server_config := SOpts,
      client_config := COpts} = proplists:get_value(tls_config, Config),
    ServerOpts = ssl_test_lib:ssl_options(SOpts, Config),
    ClientOpts = ssl_test_lib:ssl_options(COpts, Config),
    ct:log("Testing CipherSuite ~p~n", [ErlangCipherSuite]),
    ct:log("Server Opts ~p~n", [ServerOpts]),
    ct:log("Client Opts ~p~n", [ClientOpts]),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    ConnectionInfo = {ok, {Version, ErlangCipherSuite}},
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, cipher_result, [ConnectionInfo]}},
                                        {options, [{versions, [Version]}, {ciphers, [ErlangCipherSuite]} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, cipher_result, [ConnectionInfo]}},
					{options, [{versions, [Version]}, {ciphers, [ErlangCipherSuite]} |
                                                   ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


test_ciphers(Kex, Cipher, Version) ->
    ssl:filter_cipher_suites(ssl:cipher_suites(all, Version) ++ ssl:cipher_suites(anonymous, Version),
                             [{key_exchange,
                               fun(Kex0) when Kex0 == Kex -> true;
                                  (_) -> false
                               end},
                              {cipher,
                               fun(Cipher0) when Cipher0 == Cipher -> true;
                                  (_) -> false
                               end}]).


