%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%

%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(crypto_SUITE).

-include_lib("common_test/include/ct.hrl").


-export([
         %% CT callbacks:
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         
         %% Test cases:
         aead_bad_tag/1,
         aead_ng/1,
         all_ciphers/1,
         api_errors_ecdh/1,
         api_ng/0,
         api_ng/1,
         api_ng_one_shot/0,
         api_ng_one_shot/1,
         app/0,
         app/1,
         appup/0,
         appup/1,
         bad_key_length/1,
         bad_cipher_name/1,
         bad_generate_key_name/1,
         bad_cmac_name/1,
         bad_hash_name/1,
         bad_hmac_name/1,
         bad_mac_name/1,
         bad_sign_name/1,
         bad_verify_name/1,
         cipher_info/0,
         cipher_info/1,
         cipher_info_prop_aead_attr/0,
         cipher_info_prop_aead_attr/1,
         cipher_padding/1,
         cmac/0,
         cmac/1,
         cmac_update/0,
         cmac_update/1,
         compute/0,
         compute/1,
         compute_bug/0,
         compute_bug/1,
         crypto_load/1,
         crypto_load_and_call/1,
         exor/0,
         exor/1,
         generate/0,
         generate/1,
         generate_compute/0,
         generate_compute/1,
         hash/0,
         hash/1,
         hash_xof/0,
         hash_xof/1,
         hash_info/0,
         hash_info/1,
         hmac/0,
         hmac/1,
         hmac_update/0,
         hmac_update/1,
         info/1,
         mod_pow/0,
         mod_pow/1,
         no_aead_ng/0,
         no_aead_ng/1,
         no_generate_compute/0,
         no_generate_compute/1,
         no_hash/0,
         no_hash/1,
         no_hmac/0,
         no_hmac/1,
         no_poly1305/0,
         no_poly1305/1,
         no_sign_verify/0,
         no_sign_verify/1,
         no_support/0,
         no_support/1,
         node_supports_cache/1,
         poly1305/0,
         poly1305/1,
         private_encrypt/0,
         private_encrypt/1,
         public_encrypt/0,
         public_encrypt/1,
         rand_plugin/0,
         rand_plugin/1,
         rand_plugin_s/0,
         rand_plugin_s/1,
         rand_threads/0,
         rand_threads/1,
         rand_uniform/0,
         rand_uniform/1,
         hash_equals/0,
         hash_equals/1,
         sign_verify/0,
         sign_verify/1,
         ec_key_padding/1,
         use_all_ec_sign_verify/1,
         use_all_ecdh_generate_compute/1,
         use_all_eddh_generate_compute/1,
         pbkdf2_hmac/0,
         pbkdf2_hmac/1,
         privkey_to_pubkey/1,

         %% Others:
         aes_128_cbc/1,
         aes_128_ccm/1,
         aes_128_cfb128/1,
         aes_128_cfb8/1,
         aes_128_ctr/1,
         aes_128_ecb/1,
         aes_128_gcm/1,
         aes_128_ofb/1,
         aes_192_cbc/1,
         aes_192_ccm/1,
         aes_192_cfb128/1,
         aes_192_cfb8/1,
         aes_192_ctr/1,
         aes_192_ecb/1,
         aes_192_gcm/1,
         aes_192_ofb/1,
         aes_256_cbc/1,
         aes_256_ccm/1,
         aes_256_cfb128/1,
         aes_256_cfb8/1,
         aes_256_ctr/1,
         aes_256_ecb/1,
         aes_256_gcm/1,
         aes_256_ofb/1,
         aes_cbc/1,
         aes_cbc128/1,
         aes_cbc256/1,
         aes_ccm/1,
         aes_cfb128/1,
         aes_cfb8/1,
         aes_ecb/1,
         aes_gcm/1,
         blowfish_cbc/1,
         blowfish_cfb64/1,
         blowfish_ecb/1,
         blowfish_ofb64/1,
         chacha20/1,
         chacha20_poly1305/1,
         des3_cbc/1,
         des3_cbf/1,
         des3_cfb/1,
         des_cbc/1,
         des_cfb/1,
         des_ede3/1,
         des_ede3_cbc/1,
         des_ede3_cfb/1,
         mac_check/1,
         rc2_cbc/1,
         rc4/1,
         ripemd160_incr_digest/0,
         ripemd160_incr_msgs/0,
         rsa_oaep/0,
         rsa_oaep256/0,
         rsa_oaep_label/0
        ]).


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [%%crypto_load,
     %%crypto_load_and_call,
     app,
     {group, api_errors},
     appup,
     all_ciphers,
     {group, fips},
     {group, non_fips},
     cipher_padding,
     ec_key_padding,
     node_supports_cache,
     mod_pow,
     exor,
     rand_uniform,
     rand_threads,
     rand_plugin,
     rand_plugin_s,
     info,
     cipher_info,
     hash_info,
     hash_equals,
     pbkdf2_hmac
    ].

-define(NEW_CIPHER_TYPE_SCHEMA,
       ).
        
groups() ->
    [{non_fips, [], [
                     {group, blake2b},
                     {group, blake2s},
                     {group, poly1305},
                     {group, dss},
                     {group, ecdsa},
                     {group, ed25519},
                     {group, ed448},
                     {group, rsa},

                     {group, md4},
                     {group, md5},
                     {group, ripemd160},
                     {group, sha224},
                     {group, sha256},
                     {group, sha384},
                     {group, sha3_224},
                     {group, sha3_256},
                     {group, sha3_384},
                     {group, sha3_512},
                     {group, sha512},
                     {group, shake128},
                     {group, shake256},
                     {group, sha},

                     {group, dh},
                     {group, ecdh},
                     {group, eddh},
                     {group, srp},

		     {group, chacha20_poly1305},
		     {group, chacha20},
                     {group, blowfish_cbc},
                     {group, blowfish_cfb64},
                     {group, blowfish_ecb},
                     {group, blowfish_ofb64},

                     {group, des_cbc},
                     {group, des_cfb},
                     {group, rc2_cbc},
                     {group, rc4},

                     {group, des_ede3_cbc},
                     {group, des_ede3_cfb},
                     {group, aes_128_cbc},
                     {group, aes_192_cbc},
                     {group, aes_256_cbc},
                     {group, aes_128_ctr},
                     {group, aes_192_ctr},
                     {group, aes_256_ctr},
                     {group, aes_128_ccm},
                     {group, aes_192_ccm},
                     {group, aes_256_ccm},
                     {group, aes_128_ecb},
                     {group, aes_192_ecb},
                     {group, aes_256_ecb},
                     {group, aes_128_gcm},
                     {group, aes_192_gcm},
                     {group, aes_256_gcm},
                     {group, des_ede3_cbc},
                     {group, des_ede3_cfb},
                     {group, aes_128_cfb128},
                     {group, aes_192_cfb128},
                     {group, aes_256_cfb128},
                     {group, aes_128_cfb8},
                     {group, aes_192_cfb8},
                     {group, aes_256_cfb8},
                     {group, aes_128_ofb},
                     {group, aes_192_ofb},
                     {group, aes_256_ofb}
                    ]},
     {fips, [], [
                 {group, no_blake2b},
                 {group, no_blake2s},
                 {group, no_poly1305},
                 {group, dss},
                 {group, ecdsa},
                 {group, no_ed25519},
                 {group, no_ed448},
                 {group, rsa},

                 {group, no_md4},
                 {group, no_md5},
                 {group, no_ripemd160},
                 {group, sha},
                 {group, sha224},
                 {group, sha256},
                 {group, sha384},
                 {group, sha512},

                 {group, dh},
                 {group, ecdh},
                 {group, no_srp},

		 {group, no_chacha20_poly1305},
		 {group, no_chacha20},
                 {group, no_blowfish_cbc},
                 {group, no_blowfish_cfb64},
                 {group, no_blowfish_ecb},
                 {group, no_blowfish_ofb64},

                 {group, no_des_cbc},
                 {group, no_des_cfb},
                 {group, no_rc2_cbc},
                 {group, no_rc4},

                 {group, des_ede3_cbc},
                 {group, des_ede3_cfb},
                 {group, aes_128_cbc},
                 {group, aes_192_cbc},
                 {group, aes_256_cbc},
                 {group, aes_128_ctr},
                 {group, aes_192_ctr},
                 {group, aes_256_ctr},
                 {group, aes_128_ccm},
                 {group, aes_192_ccm},
                 {group, aes_256_ccm},
                 {group, aes_128_ecb},
                 {group, aes_192_ecb},
                 {group, aes_256_ecb},
                 {group, aes_128_gcm},
                 {group, aes_192_gcm},
                 {group, aes_256_gcm},
                 {group, des_ede3_cbc},
                 {group, des_ede3_cfb},
                 {group, aes_128_cfb128},
                 {group, aes_192_cfb128},
                 {group, aes_256_cfb128},
                 {group, aes_128_cfb8},
                 {group, aes_192_cfb8},
                 {group, aes_256_cfb8},
                 {group, aes_128_ofb},
                 {group, aes_192_ofb},
                 {group, aes_256_ofb}
                ]},

     {md4,                  [], [hash]},
     {md5,                  [], [hash, hmac, hmac_update]},
     {ripemd160,            [], [hash]},
     {sha,                  [], [hash, hmac, hmac_update]},
     {sha224,               [], [hash, hmac, hmac_update]},
     {sha256,               [], [hash, hmac, hmac_update]},
     {sha384,               [], [hash, hmac, hmac_update]},
     {sha512,               [], [hash, hmac, hmac_update]},
     {sha3_224,             [], [hash, hmac, hmac_update]},
     {sha3_256,             [], [hash, hmac, hmac_update]},
     {sha3_384,             [], [hash, hmac, hmac_update]},
     {sha3_512,             [], [hash, hmac, hmac_update]},
     {shake128,             [], [hash_xof]},
     {shake256,             [], [hash_xof]},
     {blake2b,              [], [hash, hmac, hmac_update]},
     {blake2s,              [], [hash, hmac, hmac_update]},
     {no_blake2b,           [], [no_hash, no_hmac]},
     {no_blake2s,           [], [no_hash, no_hmac]},
     {rsa,                  [], [sign_verify,
                                 public_encrypt,
                                 private_encrypt,
                                 generate,
                                 privkey_to_pubkey
                                ]},
     {dss,                  [], [sign_verify
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                 %% dsa seem to always have been given bad result. Must fix:
                                 %%      ,privkey_to_pubkey
                                ]},
     {ecdsa,                [], [sign_verify, use_all_ec_sign_verify
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                ]},
     {ed25519,              [], [sign_verify,
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                 generate
                              ]},
     {ed448,                [], [sign_verify,
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                 generate
                                ]},
     {dh,                   [], [generate_compute, compute_bug]},
     {ecdh,                 [], [compute, generate, use_all_ecdh_generate_compute]},
     {eddh,                 [], [compute, generate, use_all_eddh_generate_compute]},
     {srp,                  [], [generate_compute]},
     {des_cbc,              [], [api_ng, api_ng_one_shot, cmac, cmac_update]},
     {des_cfb,              [], [api_ng, api_ng_one_shot]},
     {des_ede3_cbc,         [], [api_ng, api_ng_one_shot, cmac, cmac_update]},
     {des_ede3_cfb,         [], [api_ng, api_ng_one_shot]},
     {rc2_cbc,              [], [api_ng, api_ng_one_shot, cmac, cmac_update]},
     {aes_cfb8,             [], []},
     {aes_128_cfb8,         [], [api_ng, api_ng_one_shot]},
     {aes_192_cfb8,         [], [api_ng, api_ng_one_shot]},
     {aes_256_cfb8,         [], [api_ng, api_ng_one_shot]},
     {no_aes_cfb8,          [], [no_support]},
     {aes_cfb128,           [], []},
     {aes_128_cfb128,       [], [api_ng, api_ng_one_shot]},
     {aes_192_cfb128,       [], [api_ng, api_ng_one_shot]},
     {aes_256_cfb128,       [], [api_ng, api_ng_one_shot]},
     {no_aes_cfb128,        [], [no_support]},
     {blowfish_cbc,         [], [api_ng, api_ng_one_shot, cmac, cmac_update]},
     {blowfish_ecb,         [], [api_ng, api_ng_one_shot]},
     {blowfish_cfb64,       [], [api_ng, api_ng_one_shot]},
     {blowfish_ofb64,       [], [api_ng, api_ng_one_shot]},
     {rc4,                  [], [api_ng, api_ng_one_shot]},
     {chacha20_poly1305,    [], [aead_ng, aead_bad_tag]},
     {chacha20,             [], [api_ng, api_ng_one_shot]},
     {poly1305,             [], [poly1305]},
     {no_poly1305,          [], [no_poly1305]},
     {no_aes_cfb128,        [], [no_support]},
     {no_md4,               [], [no_support, no_hash]},
     {no_md5,               [], [no_support, no_hash, no_hmac]},
     {no_ed25519,           [], [no_support, no_sign_verify
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                ]},
     {no_ed448,             [], [no_support, no_sign_verify
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                ]},
     {no_ripemd160,         [], [no_support, no_hash]},
     {no_srp,               [], [no_support, no_generate_compute]},
     {no_des_cbc,           [], [no_support]},
     {no_des_cfb,           [], [no_support]},
     {no_blowfish_cbc,      [], [no_support]},
     {no_blowfish_ecb,      [], [no_support]},
     {no_blowfish_cfb64,    [], [no_support]},
     {no_blowfish_ofb64,    [], [no_support]},
     {no_chacha20_poly1305, [], [no_support, no_aead_ng]},
     {no_chacha20,          [], [no_support]},
     {no_rc2_cbc,           [], [no_support]},
     {no_rc4,               [], [no_support]},
     {api_errors,           [], [api_errors_ecdh,
                                 bad_key_length,
                                 bad_cipher_name,
                                 bad_generate_key_name,
                                 bad_hash_name,
                                 bad_mac_name,
                                 bad_hmac_name,
                                 bad_cmac_name,
                                 bad_sign_name,
                                 bad_verify_name
                                ]},

     %% New cipher nameing schema
     {des_ede3_cbc, [], [api_ng, api_ng_one_shot]},
     {des_ede3_cfb, [], [api_ng, api_ng_one_shot]},
     {aes_128_cbc,  [], [api_ng, api_ng_one_shot, cmac, cmac_update]},
     {aes_192_cbc,  [], [api_ng, api_ng_one_shot, cmac, cmac_update]},
     {aes_256_cbc,  [], [api_ng, api_ng_one_shot, cmac, cmac_update]},
     {aes_128_ctr,  [], [api_ng, api_ng_one_shot]},
     {aes_192_ctr,  [], [api_ng, api_ng_one_shot]},
     {aes_256_ctr,  [], [api_ng, api_ng_one_shot]},
     {aes_128_ccm,  [], [aead_ng, aead_bad_tag]},
     {aes_192_ccm,  [], [aead_ng, aead_bad_tag]},
     {aes_256_ccm,  [], [aead_ng, aead_bad_tag]},
     {aes_128_ecb,  [], [api_ng, api_ng_one_shot]},
     {aes_192_ecb,  [], [api_ng, api_ng_one_shot]},
     {aes_256_ecb,  [], [api_ng, api_ng_one_shot]},
     {aes_128_gcm,  [], [aead_ng, aead_bad_tag]},
     {aes_192_gcm,  [], [aead_ng, aead_bad_tag]},
     {aes_256_gcm,  [], [aead_ng, aead_bad_tag]},
     {aes_128_ofb,  [], [api_ng, api_ng_one_shot]},
     {aes_192_ofb,  [], [api_ng, api_ng_one_shot]},
     {aes_256_ofb,  [], [api_ng, api_ng_one_shot]}
    ].

%%-------------------------------------------------------------------
init_per_suite(Config) ->
    file:set_cwd(datadir(Config)),
    {ok, _} = zip:unzip("KAT_AES.zip"),
    {ok, _} = zip:unzip("aesmmt.zip"),
    {ok, _} = zip:unzip("cmactestvectors.zip"),
    {ok, _} = zip:unzip("gcmtestvectors.zip"),

    try is_ok(crypto:start()) of
	ok ->
            catch ct:comment("~s",[element(3,hd(crypto:info_lib()))]),
            catch ct:log("crypto:info()     -> ~p~n"
                         "crypto:info_lib() -> ~p~n"
                         "crypto:version()  -> ~p~n"
                         "crypto:supports() -> ~p~n"
                        ,[crypto:info(), crypto:info_lib(), crypto:version(), crypto:supports()]),

	    try crypto:strong_rand_bytes(1) of
		_ ->
		    Config
	    catch error:low_entropy ->
                    %% We are testing on an OS with low entropy in its random
                    %% seed. So we have to seed it with a binary to get started.

		    %% This is NOT how you want to do seeding, it is just here
		    %% to make the tests pass. Check your OS manual for how you
		    %% really want to seed.
		    {H,M,L} = erlang:timestamp(),
		    Bin = <<H:24,M:20,L:20>>,
		    crypto:rand_seed(<< <<Bin/binary>> || _ <- lists:seq(1,16) >>),
		    Config
	    end
                  
    catch C:E:S ->
            ct:log("~p ~p~n~p", [C,E,S]),
	    {fail, "Crypto did not start"}
    end.

is_ok(ok) -> ok;
is_ok({error, already_started}) -> ok;
is_ok({error,{already_started,crypto}}) -> ok.

    

end_per_suite(_Config) ->
    application:stop(crypto).

%%-------------------------------------------------------------------
init_per_group(fips, Config) ->
    try_enable_fips_mode(Config);
init_per_group(non_fips, Config) ->
    NonFIPSConfig = [{fips, false} | Config],
    case crypto:info_fips() of
        enabled ->
            true = crypto:enable_fips_mode(false),
            not_enabled = crypto:info_fips(),
            NonFIPSConfig;
        _NotEnabled ->
            NonFIPSConfig
    end;
init_per_group(api_errors, Config) ->
    Config;
init_per_group(GroupName, Config) ->
    case atom_to_list(GroupName) of
        "no_" ++ TypeStr ->
            %% Negated test case: check the algorithm is not supported
            %% (e.g. due to FIPS mode limitations)
            TypeAtom = list_to_atom(TypeStr),
            [{type, TypeAtom} | group_config(TypeAtom, Config)];
        _Other ->
            %% Regular test case: skip if the algorithm is not supported
            case is_supported(GroupName) of
                true ->
                    [{type, GroupName} | group_config(GroupName, Config)];
                false ->
                    {skip, "Group not supported"}
            end
    end.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(info, Config) ->
    Config;
init_per_testcase(cmac, Config) ->
    case is_supported(cmac) of
        true ->
            configure_mac(cmac, proplists:get_value(type,Config), Config);
        false ->
            {skip, "CMAC is not supported"}
    end;
init_per_testcase(cmac_update, Config) ->
    case {is_supported(cmac),
          maps:get(cryptolib_version_linked, crypto:info(), "")} of
        {true, "OpenSSL 1.0."++_} -> {skip, "cmac_update is not supported"};
        {false,                _} -> {skip, "CMAC is not supported"};
        _ -> Config
    end;
init_per_testcase(generate, Config) ->
    case proplists:get_value(type, Config) of
	rsa ->
	    % RSA key generation is a lengthy process, and is only available
	    % if dirty CPU scheduler support was enabled for this runtime.
	    case try erlang:system_info(dirty_cpu_schedulers) of
		     N -> N > 0
		 catch
		     error:badarg -> false
		 end of
		true -> Config;
		false -> {skip, "RSA key generation requires dirty scheduler support."}
	    end;
	_ -> Config
    end;
init_per_testcase(hmac, Config) ->
    configure_mac(hmac, proplists:get_value(type,Config), Config);
init_per_testcase(_Name,Config) ->
    Skip =
        lists:member(_Name, [%%i_ng_tls
%%                            , node_supports_cache
                            ]) andalso
        asan == maps:get(compile_type, crypto:info(), undefined) andalso
        lists:prefix("OpenSSL 3.0.0 ", maps:get(cryptolib_version_linked, crypto:info(), "")),
    case Skip of
        true -> {skip, "Coredumps 3.0.0 asan"};
        false -> Config
    end.

end_per_testcase(info, Config) ->
    Config;
end_per_testcase(_Name,Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
app() ->
    [{doc, "Test that the crypto app file is ok"}].
app(Config) when is_list(Config) ->
    ok = test_server:app_test(crypto).
%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the crypto appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(crypto).

%%--------------------------------------------------------------------
%% Simple encode/decode for all ciphers in crypto:supports(ciphers). No
%% checking of the encrypted text, just check that it is decrypted back
%% to the plain text.
all_ciphers(_Config) ->
    case [C || C <- crypto:supports(ciphers),
               ok =/= simple_cipher_test(C)] of
        [] ->
            ok;
        BadCiphers ->
            ct:log("Bad ciphers: ~p", [BadCiphers]),
            {fail, "Cipher(s) failed"}
    end.

simple_cipher_test(Cipher) ->
    try
        #{key_length := KeyLength,
          iv_length := IvLength,
          block_size := BlockSize,
          mode := CipherMode
         } = crypto:cipher_info(Cipher),
        Key = <<1:KeyLength/unit:8>>,
        IV = <<0:IvLength/unit:8>>,
        Plain0 = <<"Hello world! Let's do some cipher tests">>,
        Plain = case BlockSize of
                    1 -> Plain0;
                    _ -> <<Plain0:BlockSize/binary>>
                end,
        enc_dec(Cipher, Key, IV, CipherMode, Plain)
    catch
        Class:Error:Stack ->
            ct:log("Error for cipher ~p:~nClass = ~p~nError = ~p~nStack = ~p",
                   [Cipher, Class, Error, Stack]),
            error
    end.



enc_dec(Cipher, Key, 0, _Mode, Plain) ->
    case crypto:crypto_one_time(Cipher, Key, Plain, true) of
        Encrypted when is_binary(Encrypted) ->
             case crypto:crypto_one_time(Cipher, Key, Encrypted, false) of
                 Plain ->
                     ok;
                 Other ->
                     ct:log("~p:~p Error for cipher ~p:~n~p", [?MODULE,?LINE,Cipher, Other]),
                     error
             end;
        Other ->
            ct:log("~p:~p Error for cipher ~p:~n~p", [?MODULE,?LINE,Cipher, Other]),
            error
    end;

enc_dec(Cipher, Key, IV, Mode, Plain) when Mode == ccm_mode ;
                                           Mode == gcm_mode ;
                                           Cipher == chacha20_poly1305 ->
    AAD = aad(Cipher, Mode),
    case crypto:crypto_one_time_aead(Cipher, Key, IV, Plain, AAD, true) of
        {Encrypted,Tag} when is_binary(Encrypted) ->
            case crypto:crypto_one_time_aead(Cipher, Key, IV, Encrypted, AAD, Tag, false) of
                 Plain ->
                     ok;
                 Other ->
                     ct:log("~p:~p Error for cipher ~p:~n~p", [?MODULE,?LINE,Cipher, Other]),
                     error
             end;
        Other ->
            ct:log("~p:~p Error for cipher ~p:~n~p", [?MODULE,?LINE,Cipher, Other]),
            error
    end;

enc_dec(Cipher, Key, IV, _Mode, Plain) ->
    case crypto:crypto_one_time(Cipher, Key, IV, Plain, true) of
        Encrypted when is_binary(Encrypted) ->
             case crypto:crypto_one_time(Cipher, Key, IV, Encrypted, false) of
                 Plain ->
                     ok;
                 Other ->
                     ct:log("~p:~p Error for cipher ~p:~n~p", [?MODULE,?LINE,Cipher, Other]),
                     error
             end;
        Other ->
            ct:log("~p:~p Error for cipher ~p:~n~p", [?MODULE,?LINE,Cipher, Other]),
            error
    end.

aad(_Cipher, _Mode) ->
    %% Any size will do
    <<"Some text">>.

%%--------------------------------------------------------------------
no_support() ->
    [{doc, "Test an algorithm is not reported in the supported list"}].
no_support(Config) when is_list(Config) ->
    Type  = ?config(type, Config),
    false = is_supported(Type).
%%--------------------------------------------------------------------
crypto_load(_Config) ->
    (catch crypto:stop()),
    code:delete(crypto),
    code:purge(crypto),
    crypto:start().
%%--------------------------------------------------------------------
crypto_load_and_call(_Config) ->
    (catch crypto:stop()),
    code:delete(crypto),
    code:purge(crypto),
    Key0 = "ablurf123BX#$;3",
    Bin0 = erlang:md5(<<"whatever">>),
    {Key,IVec,_BlockSize} = make_crypto_key(Key0),
    crypto:crypto_one_time(des_ede3_cbc, Key, IVec, Bin0, true).

make_crypto_key(String) ->
    <<K1:8/binary,K2:8/binary>> = First = erlang:md5(String),
    <<K3:8/binary,IVec:8/binary>> = erlang:md5([First|lists:reverse(String)]),
    {[K1,K2,K3],IVec,8}.
%%--------------------------------------------------------------------
%% Test that a spawned node has initialized the cache

node_supports_cache(_Config) ->
    ECs = crypto:supports(curves),
    {ok,Peer,Node} = ?CT_PEER(),
    case erpc:call(Node, crypto, supports, [curves]) of
        ECs ->
            peer:stop(Peer);
        OtherECs ->
            peer:stop(Peer),
            ct:log("At master:~p~nAt slave:~p~n"
                   "Missing at slave: ~p~nmissing at master: ~p",
                   [ECs, OtherECs, ECs--OtherECs, OtherECs--ECs]),
            {fail, "different support at slave"}
    end.

%%--------------------------------------------------------------------
hash() ->
    [{doc, "Test all different hash functions"}].
hash(Config) when is_list(Config) ->
    {Type, MsgsLE, Digests} = proplists:get_value(hash, Config),
    Msgs = lazy_eval(MsgsLE),
    [LongMsg | _] = lists:reverse(Msgs),
    Inc = iolistify(LongMsg),
    [IncrDigest | _] = lists:reverse(Digests),
    hash(Type, Msgs, Digests),
    hash(Type, lists:map(fun iolistify/1, Msgs), Digests),
    hash_increment(Type, Inc, IncrDigest).

hash_xof() ->
  [{doc, "Test all different hash_xof functions"}].
hash_xof(Config) when is_list(Config) ->
  {Type, MsgsLE, Digests, Lengths} = proplists:get_value(hash_xof, Config),
  Msgs = lazy_eval(MsgsLE),
  hash_xof(Type, Msgs, Digests, Lengths).

%%--------------------------------------------------------------------
no_hash() ->
    [{doc, "Test all disabled hash functions"}].
no_hash(Config) when is_list(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:hash/2, [Type, <<"Hi There">>]),
    notsup(fun crypto:hash_init/1, [Type]).
%%--------------------------------------------------------------------
hmac() ->
     [{doc, "Test hmac function"}].
hmac(Config) when is_list(Config) ->
    Tuples = lazy_eval(proplists:get_value(hmac, Config)),
    do_cipher_tests(fun mac_check/1, Tuples++mac_listify(Tuples)).
%%--------------------------------------------------------------------
no_hmac() ->
     [{doc, "Test all disabled hmac functions"}].
no_hmac(Config) when is_list(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:mac/4, [hmac, Type, <<"Key">>, <<"Hi There">>]).
%%--------------------------------------------------------------------
hmac_update() ->
     [{doc, "Test all incremental hmac functions"}].
hmac_update(Config) ->
    SubType = ?config(type, Config),
    Key = hmac_key(SubType),
    Increments = hmac_inc(SubType),
    mac_increment(hmac, SubType, Key, Increments).
%%--------------------------------------------------------------------
cmac() ->
     [{doc, "Test all different cmac functions"}].
cmac(Config) when is_list(Config) ->
    Pairs = lazy_eval(proplists:get_value(cmac, Config)),
    do_cipher_tests(fun mac_check/1, Pairs ++ mac_listify(Pairs)).
%%--------------------------------------------------------------------
cmac_update() ->
     [{doc, "Test all incremental cmac functions"}].
cmac_update(Config) ->
    SubType = ?config(type, Config),
    Key = cmac_key(SubType),
    Increments = cmac_inc(SubType),
    mac_increment(cmac, SubType, Key, Increments).
%%--------------------------------------------------------------------
poly1305() ->
    [{doc, "Test poly1305 function"}].
poly1305(Config) ->
    lists:foreach(
      fun({Key, Txt, Expect}) ->
              case crypto:mac(poly1305,Key,Txt) of
                  Expect ->
                      ok;
                  Other ->
                      ct:fail({{crypto, mac, [poly1305, Key, Txt]}, {expected, Expect}, {got, Other}})
              end
      end, proplists:get_value(poly1305, Config)).

%%--------------------------------------------------------------------
no_poly1305() ->
    [{doc, "Test disabled poly1305 function"}].
no_poly1305(_Config) ->
    Key = <<133,214,190,120,87,85,109,51,127,68,82,254,66,213,6,168,1,
            3,128,138,251,13,178,253,74,191,246,175,65,73,245,27>>,
    Txt = <<"Cryptographic Forum Research Group">>,
    notsup(fun crypto:mac/3, [poly1305,Key,Txt]).

%%--------------------------------------------------------------------
api_ng() ->
     [{doc, "Test new api"}].

api_ng(Config) when is_list(Config) ->
    [_|_] = Ciphers = lazy_eval(proplists:get_value(cipher, Config, [])),
    lists:foreach(fun api_ng_cipher_increment/1, Ciphers ++ spec_0_bytes(Config)).

api_ng_cipher_increment({Type, Key, PlainTexts}=_X) ->
    ct:log("~p",[_X]),
    api_ng_cipher_increment({Type, Key, <<>>, PlainTexts});

api_ng_cipher_increment({Type, Key, IV, PlainTexts}=_X) ->
    ct:log("~p",[_X]),
    api_ng_cipher_increment({Type, Key, IV, PlainTexts, undefined});

api_ng_cipher_increment({Type, Key, IV, PlainText0, ExpectedEncText}=_X) ->
    ct:log("~p",[_X]),
    PlainTexts = iolistify(PlainText0),
    RefEnc = crypto:crypto_init(Type, Key, IV, true),
    RefDec = crypto:crypto_init(Type, Key, IV, false),
    EncTexts = api_ng_cipher_increment_loop(RefEnc, PlainTexts),
    EncFinal = crypto:crypto_final(RefEnc),
    Enc = iolist_to_binary(EncTexts++[EncFinal]),
    case ExpectedEncText of
        undefined ->
            ok;
        Enc ->
            ok;
        _ ->
            ct:log("encode~nIn: ~p~nExpected: ~p~nEnc: ~p~n", [{Type,Key,IV,PlainTexts}, ExpectedEncText, Enc]),
            ct:fail("api_ng_cipher_increment (encode)",[])
    end,
    Plain = iolist_to_binary(PlainTexts),
    DecTexts = api_ng_cipher_increment_loop(RefDec, EncTexts),
    DecFinal =  crypto:crypto_final(RefDec),
    case iolist_to_binary(DecTexts++[DecFinal]) of
        Plain ->
            ok;
        OtherPT ->
            ct:log("decode~nIn: ~p~nExpected: ~p~nDec: ~p~n", [{Type,Key,IV,EncTexts}, Plain, OtherPT]),
            ct:fail("api_ng_cipher_increment (encode)",[])
    end.


api_ng_cipher_increment_loop(Ref, InTexts) ->
    lists:map(fun(Txt) ->
                      try crypto:crypto_update(Ref, Txt)
                      of
                          Bin when is_binary(Bin) ->
                              Bin
                      catch
                          error:Error ->
                              ct:log("Txt = ~p",[Txt]),
                              ct:fail("~p",[Error])
                      end
              end, InTexts).

%%--------------------------------------------------------------------
%% Check that crypto do not core dump on early 0.9.8 cryptolibs
spec_0_bytes(Config) ->
    Type = proplists:get_value(type, Config),
    #{iv_length := IVS, key_length := KS} = Spec = crypto:cipher_info(Type),
    Key = <<0:KS/unit:8>>,
    IV = <<0:IVS/unit:8>>,
    spec_0_bytes(Type, Key, IV, Spec).


spec_0_bytes(chacha20_poly1305, _, _, _) ->
    [];
spec_0_bytes(Type, Key, IV, #{mode := M}) when M == ccm_mode ;
                                               M == gcm_mode ->
    AAD = <<>>,
    Plain = <<>>,
    {_, Tag} = crypto:crypto_one_time_aead(Type, Key, IV, Plain, AAD, true),
    [{Type, Key, Plain, IV, AAD, <<>>, Tag, []}];
spec_0_bytes(Type, Key, IV, _Spec) ->
    [{Type, Key, IV, <<>>, <<>>}].

%%--------------------------------------------------------------------
api_ng_one_shot() ->
     [{doc, "Test new api"}].

api_ng_one_shot(Config) when is_list(Config) ->
    [_|_] = Ciphers = lazy_eval(proplists:get_value(cipher, Config, [])),
    lists:foreach(fun do_api_ng_one_shot/1, Ciphers ++ spec_0_bytes(Config)).

do_api_ng_one_shot({Type, Key, PlainTexts}=_X) ->
    ct:log("~p",[_X]),
    do_api_ng_one_shot({Type, Key, <<>>, PlainTexts});

do_api_ng_one_shot({Type, Key, IV, PlainTexts}=_X) ->
    ct:log("~p",[_X]),
    do_api_ng_one_shot({Type, Key, IV, PlainTexts, undefined});

do_api_ng_one_shot({Type, Key, IV, PlainText0, ExpectedEncText}=_X) ->
    ct:log("~p",[_X]),
    PlainText = iolist_to_binary(lazy_eval(PlainText0)),
    EncTxt = crypto:crypto_one_time(Type, Key, IV, PlainText, true),
    case ExpectedEncText of
        undefined ->
            ok;
        EncTxt ->
            ok;
        _ ->
            ct:log("encode~nIn: ~p~nExpected: ~p~nEnc: ~p~n", [{Type,Key,IV,PlainText}, ExpectedEncText, EncTxt]),
            ct:fail("api_ng_one_time (encode)",[])
    end,
    case crypto:crypto_one_time(Type, Key, IV, EncTxt, false) of
        PlainText ->
            ok;
        OtherPT ->
            ct:log("decode~nIn: ~p~nExpected: ~p~nDec: ~p~n", [{Type,Key,IV,EncTxt}, PlainText, OtherPT]),
            ct:fail("api_ng_one_time (decode)",[])
    end.

%%--------------------------------------------------------------------
cipher_padding(_Config) ->
    Ciphers = [{C,pkcs_padding}
               || C <- crypto:supports(ciphers),
                  C =/= chacha20_poly1305,
                  case crypto:cipher_info(C) of
                      #{mode := ccm_mode} -> false;
                      #{mode := gcm_mode} -> false;
                      _ -> true
                  end],
    lists:foreach(fun cipher_padding_test/1, Ciphers).

cipher_padding_test({Cipher, Padding}) ->
    #{block_size := Sblock,
      iv_length  := Siv,
      key_length := Skey} = Inf = crypto:cipher_info(Cipher),
    ct:log("~p ~p", [Cipher,Inf]),

    Key = <<1:Skey/unit:8>>,
    IV  = <<0:Siv/unit:8>>,
    MsgLen = 5*Sblock + 3,
    Tplain = crypto:strong_rand_bytes(MsgLen),
    PadSize = if
                  (Padding == zero) ; (Padding == random) ->
                      (Sblock - (MsgLen rem Sblock)) rem Sblock;
                  true ->
                      0
              end,
    Tcrypt =
        case Siv of
            0 ->
                crypto:crypto_one_time(Cipher, Key, Tplain, [{encrypt,true},{padding,Padding}]);
            _ ->
                crypto:crypto_one_time(Cipher, Key, IV, Tplain, [{encrypt,true},{padding,Padding}])
        end,

    TdecryptPadded =
        case Siv of
            0 ->
                crypto:crypto_one_time(Cipher, Key, Tcrypt, [{encrypt,false},{padding,Padding}]);
            _ ->
                crypto:crypto_one_time(Cipher, Key, IV, Tcrypt, [{encrypt,false},{padding,Padding}])
        end,

    case split_binary(TdecryptPadded, size(TdecryptPadded) - PadSize) of
        {Tplain, _} ->
            ok;
        {Tdecrypt,Tpad} ->
            ct:log("Key = ~p~nIV = ~p~nTplain = ~p~nTcrypt = ~p~nTdecrypt = ~p~nPadding = ~p",
                   [Key, IV, Tplain, Tcrypt, Tdecrypt, Tpad]),
            ct:fail("~p", [Cipher])
    end.

%%--------------------------------------------------------------------
ec_key_padding(_Config) ->
    lists:foreach(fun test_ec_key_padding/1,
                  crypto:supports(curves) -- [ed25519, ed448, x25519, x448]
                 ).

test_ec_key_padding(CurveName) ->
    ExpectedSize = expected_ec_size(CurveName),
    repeat(100, % Enough to provoke an error in the 85 curves
               % With for example 1000, the total test time would be too large
           fun() ->
                   case crypto:generate_key(ecdh, CurveName) of
                       {_PubKey, PrivKey} when byte_size(PrivKey) == ExpectedSize ->
                           %% ct:pal("~p:~p Test ~p, size ~p, expected size ~p",
                           %%        [?MODULE,?LINE, CurveName, byte_size(PrivKey), ExpectedSize]),
                           ok;
                       {_PubKey, PrivKey} ->
                           ct:fail("Bad ~p size: ~p expected: ~p", [CurveName, byte_size(PrivKey), ExpectedSize]);
                       Other ->
                           ct:pal("~p:~p ~p", [?MODULE,?LINE,Other]),
                           ct:fail("Bad public_key:generate_key result for ~p", [CurveName])
                   end
           end).

repeat(Times, F) when Times > 0 -> F(), repeat(Times-1, F);
repeat(_, _) -> ok.

expected_ec_size(CurveName) when is_atom(CurveName) ->
    expected_ec_size(crypto_ec_curves:curve(CurveName));
expected_ec_size({{prime_field,_}, _, _, Order, _}) -> byte_size(Order);
expected_ec_size({{characteristic_two_field, _, _}, _, _, Order, _}) -> size(Order).

%%--------------------------------------------------------------------
no_aead_ng() ->
     [{doc, "Test disabled aead ciphers"}].
no_aead_ng(Config) when is_list(Config) ->
    {EncFun, EncryptArgs} =
        case lazy_eval(proplists:get_value(cipher, Config)) of
            [{Type, Key, PlainText, IV, AAD, CipherText, CipherTag, TagLen, _Info} | _] ->
                {fun crypto:crypto_one_time_aead/7, [Type, Key, IV, PlainText, AAD, TagLen, true]};

            [{Type, Key, PlainText, IV, AAD, CipherText, CipherTag, _Info} | _] ->
                {fun crypto:crypto_one_time_aead/6, [Type, Key, IV, PlainText, AAD, true]}
        end,
    notsup(EncFun, EncryptArgs),

    DecryptArgs = [Type, Key, IV, CipherText, AAD, CipherTag, false],
    notsup(fun crypto:crypto_one_time_aead/7, DecryptArgs).

%%--------------------------------------------------------------------
aead_ng(Config) when is_list(Config) ->
    [_|_] = AEADs = lazy_eval(proplists:get_value(cipher, Config)),
    FilteredAEADs =
	case proplists:get_bool(fips, Config) of
	    false ->
		AEADs;
	    true ->
		%% In FIPS mode, the IV length must be at least 12 bytes.
		lists:filter(
		  fun(Tuple) ->
			  IVLen = byte_size(element(4, Tuple)),
			  IVLen >= 12
		  end, AEADs)
	end,
    do_cipher_tests(fun aead_cipher_ng/1, FilteredAEADs ++ spec_0_bytes(Config)).

%%-------------------------------------------------------------------- 
aead_bad_tag(Config) ->
    [_|_] = AEADs = lazy_eval(proplists:get_value(cipher, Config)),
    FilteredAEADs =
	case proplists:get_bool(fips, Config) of
	    false ->
		AEADs;
	    true ->
		%% In FIPS mode, the IV length must be at least 12 bytes.
		lists:filter(
		  fun(Tuple) ->
			  IVLen = byte_size(element(4, Tuple)),
			  IVLen >= 12
		  end, AEADs)
	end,
    do_cipher_tests(fun aead_cipher_bad_tag/1, FilteredAEADs).

%%-------------------------------------------------------------------- 
sign_verify() ->
     [{doc, "Sign/verify digital signatures"}].
sign_verify(Config) when is_list(Config) ->
    SignVerify = proplists:get_value(sign_verify, Config),
    lists:foreach(fun do_sign_verify/1, SignVerify).

%%--------------------------------------------------------------------
no_sign_verify() ->
    [{doc, "Test disabled sign/verify digital signatures"}].
no_sign_verify(Config) when is_list(Config) ->
    [SignVerifyHd|_] = proplists:get_value(sign_verify, Config),
    notsup(fun do_sign_verify/1, [SignVerifyHd]).

%%-------------------------------------------------------------------- 
public_encrypt() ->
     [{doc, "Test public_encrypt/decrypt "}].
public_encrypt(Config) when is_list(Config) ->
    ct:log("public_encrypt", []),
    Params = proplists:get_value(pub_pub_encrypt, Config, []),
    lists:foreach(fun do_public_encrypt/1, Params).

%%-------------------------------------------------------------------- 
private_encrypt() ->
     [{doc, "Test private_encrypt/decrypt functions. "}].
private_encrypt(Config) when is_list(Config) ->
    Params = proplists:get_value(pub_priv_encrypt, Config, []),
    lists:foreach(fun do_private_encrypt/1, Params).

%%--------------------------------------------------------------------
privkey_to_pubkey(Config) ->
    Params = proplists:get_value(privkey_to_pubkey, Config),
    lists:foreach(fun do_privkey_to_pubkey/1, Params).

do_privkey_to_pubkey({Type, Priv, Pub}) ->
    ct:log("~p:~p~nType = ~p,~nPriv = ~p,~n  Pub = ~p", [?MODULE,?LINE,Type,Priv,Pub]),
    case crypto:privkey_to_pubkey(Type, Priv) of
        Pub ->
            ok;
        Priv ->
            ct:fail("Returned private key", []);
        Other ->
            ct:log("~p:~p Other = ~p", [?MODULE,?LINE,Other]),
            ct:fail("bad", [])
    end.

%%--------------------------------------------------------------------
generate_compute() ->
     [{doc, " Test crypto:genarate_key and crypto:compute_key"}].
generate_compute(Config) when is_list(Config) ->
    GenCom = proplists:get_value(generate_compute, Config),
    lists:foreach(fun do_generate_compute/1, GenCom).
%%--------------------------------------------------------------------
compute_bug() ->
    [{doc, "Test that it works even if the Secret is smaller than expected"}].
compute_bug(_Config) ->
    case crypto:info_fips() of
        enabled ->
            %% FIPs on OpenSSL 3 seems to demand P to be a safe prime
            %% where Q = (P-1)/2 is also prime.
            skip;
        _ -> do_compute_bug()
    end.

do_compute_bug() ->
    ExpectedSecret = <<118,89,171,16,156,18,156,103,189,134,130,49,28,144,111,241,247,82,79,32,228,11,209,141,119,176,251,80,105,143,235,251,203,121,223,211,129,3,233,133,45,2,31,157,24,111,5,75,153,66,135,185,128,115,229,178,216,39,73,52,80,151,8,241,34,52,226,71,137,167,53,48,59,224,175,154,89,110,76,83,24,117,149,21,72,6,186,78,149,74,188,56,98,244,30,77,108,248,88,194,195,237,23,51,20,242,254,123,21,12,209,74,217,168,230,65,7,60,211,139,128,239,234,153,22,229,180,59,159,121,41,156,121,200,177,130,163,162,54,224,93,1,94,11,177,254,118,28,156,26,116,10,207,145,219,166,214,189,214,230,221,170,228,15,69,88,31,68,94,255,113,58,49,82,86,192,248,176,131,133,39,186,194,172,206,84,184,16,66,68,153,128,178,227,27,118,52,130,122,92,24,222,102,195,221,207,255,13,152,175,65,32,167,84,54,244,243,109,244,18,234,16,159,224,188,2,106,123,27,17,131,171,226,34,111,251,62,119,155,124,221,124,254,62,97,167,1,105,116,98,98,19,197,30,72,180,79,221,100,134,120,117,124,85,73,132,224,223,222,41,155,137,218,130,238,237,157,161,134,150,69,206,91,141,17,89,120,218,235,229,37,150,76,197,7,157,56,144,42,203,137,100,200,72,141,194,239,1,67,236,238,183,48,214,75,76,108,235,3,237,67,40,137,45,182,236,246,37,116,103,144,237,142,211,88,233,11,24,21,218,41,245,250,51,130,250,104,74,189,17,69,145,70,50,50,215,253,155,10,128,41,114,185,211,82,164,72,92,17,145,104,66,6,140,226,80,43,62,1,166,216,153,118,96,15,147,126,137,118,191,192,75,149,241,206,18,92,17,154,215,219,18,6,139,190,103,210,156,184,29,224,213,157,60,112,189,104,220,125,40,186,50,119,17,143,136,149,38,74,107,21,192,59,61,59,42,231,144,59,175,3,176,87,23,16,122,54,31,82,34,230,211,44,81,41,47,86,37,228,175,130,148,88,136,131,254,241,202,99,199,175,1,141,215,124,155,120,43,141,89,11,140,120,141,29,35,82,219,155,204,75,12,66,241,253,33,250,84,24,85,68,13,80,85,142,227,34,139,26,146,24>>,
    OthersPublicKey = 635619632099733175381667940709387641100492974601603060984753028943194386334921787463327680809776598322996634648015962954045728174069768874873236397421720142610982770302060309928552098274817978606093380781524199673890631795310930242601197479471368910519338301177304682162189801040921618559902948819107531088646753320486728060005223263561551402855338732899079439899705951063999951507319258050864346087428042978411873495523439615429804957374639092580169417598963105885529553632847023899713490485619763926900318508906706745060947269748612049634207985438016935262521715769812475329234748426647554362991758104620357149045960316987533503707855364806010494793980069245562784050236811004893018183726397041999426883788660276453352521120006817370050691205529335316794439089316232980047277245051173281601960196573681285904611182521967067911862467395705665888521948321299521549941618586026714676885890192323289343756440666276226084448279082483536164085883288884231665240707495770544705648564889889198060417915693315346959170105413290799314390963124178046425737828369059171472978294050322371452255088799865552038756937873388385970088906560408959959429398326288750834357514847891423941047433478384621074116184703014798814515161475596555032391555842,
    MyPrivateKey = 387759582879975726965038486537011291913744975764132199838375902680222019267527675651273586836110220500657652661706223760165097275862806031329642160439090779625708664007910974206651834216043397115514725827856461492311499129200688538220719685637154290305617686974719521885238198226075381217068175824097878445476010193039590876624464274744156624589136789060427283492343902761765833713520850870233407503430180028104167029073459918756981323130062648615262139444306321256382009848217866984408901761817655567071716275177768316006340055589170095799943481591033461616307776069027985761229636731465482676467627154100912586936231051371168178564599296638350391246393336702334311781595616786107810962134407697848002331639021101685320844880636050048769216986088652236979636019052557155807310341483407890060105599892252118584570558049301477535792498672552850760356632076013402382600669875697284264329434950712239302528367835155163504374877787288116104285944993818319105835423479332617802010952731990182088670508346704423006877514817882782443833997288652405892920173712497948376815825396272381214976859009518623799156300136570204539240675245115597412280078940442452936425561984312708387584800789375684525365060589104566195610526570099527133097201479,
    P = 818034524162384276004384029858643530286875094391273833506734966261806257117433972760379103507630310628953496150318170372254219924175532996281953750642804369831900894594960807970232131410638888573275563720690293481410915588408505771183615664441221559618326229227448328879290185035795866796496147000467456347856187771645103733376761936369144682074588463621004219054111244232031965820058057143484947957179035662640791007685559024477920075136419228662974090561346329074935312181886940693299380892129818458511403741106419480550004799657220331973244248753744516935739033770420884365608406478656143540532371463443324228730693491647103274058971797182813283112583029849186056551355376851686616057869624968077484471229044125401535456699914745876082047459812392122562460031611344154642406382436701361983114768023990405077450124649862159757605118611426368650203370143674925598905779061402007525955196464201496773278952462368223659263492419274489020447849336502432222101793313731259141617677580646998184158969477474527427664187763741360356528830301163614618231141541403007931347398186427059736520580903587497382362610721261644208653717495736748724114113311672504064943864203789205551568648546606356374830209356446449765364678719909024329058480379,
    G = 2,
    DHParameters = [P, G],
    case crypto:compute_key(dh, OthersPublicKey, MyPrivateKey, DHParameters) of
        ExpectedSecret ->
            ok;
        Others ->
            ct:log("Got ~p",[Others]),
            {fail, "crypto:compute_key(dh,...) failed for the bug test"}
    end.

%%--------------------------------------------------------------------
no_generate_compute() ->
     [{doc, "Test crypto:genarate_key and crypto:compute_key "
       "for disabled algorithms"}].
no_generate_compute(Config) when is_list(Config) ->
    %% This test is specific to the SRP protocol
    srp = ?config(type, Config),
    {srp,
     UserPrivate, UserGenParams, UserComParams,
     HostPublic, HostPrivate, HostGenParams, HostComParams,
     _SessionKey} = srp3(),
    UserPublic = HostPublic,                    % use a fake public key
    notsup(fun crypto:generate_key/3, [srp, UserGenParams, UserPrivate]),
    notsup(fun crypto:generate_key/3, [srp, HostGenParams, HostPrivate]),
    notsup(fun crypto:compute_key/4,
           [srp, HostPublic, {UserPublic, UserPrivate}, UserComParams]),
    notsup(fun crypto:compute_key/4,
           [srp, UserPublic, {HostPublic, HostPrivate}, HostComParams]).
%%--------------------------------------------------------------------
compute() ->
     [{doc, " Test crypto:compute_key"}].
compute(Config) when is_list(Config) ->
    Gen0 = proplists:get_value(compute, Config),
    Gen = case crypto:info_fips() of
              enabled ->
                  SkipCurves = [secp192r1],
                  lists:filter(fun({_,_,_,Curve,_}) ->
                                       not lists:member(Curve,SkipCurves)
                               end,
                               Gen0);
              _ ->
                  Gen0
    end,
    lists:foreach(fun do_compute/1, Gen).

%%--------------------------------------------------------------------
use_all_ec_sign_verify(_Config) ->
    Msg = <<"hello world!">>,
    Sups = crypto:supports(),
    Curves = proplists:get_value(curves, Sups),
    Hashs = proplists:get_value(hashs, Sups) -- [shake128, shake256],
    ct:log("Lib: ~p~nFIPS: ~p~nCurves:~n~p~nHashs: ~p", [crypto:info_lib(),
                                                         crypto:info_fips(),
                                                         Curves,
                                                         Hashs]),
    SkipHashs0 = [md4, md5, ripemd160, sha3_224, sha3_256, sha3_384, sha3_512,
                  blake2b, blake2s],
    SkipCurves0 = [ed25519, ed448, x25519, x448, ipsec3, ipsec4],

    {SkipHashs, SkipCurves}
        = case crypto:info_fips() of
              enabled ->
                  {[sha | SkipHashs0],
                   [secp192r1, prime192v1, sect163k1, sect163r2]};
              _ ->
                  {SkipHashs0, SkipCurves0}
          end,

    Results =
        [{{Curve,Hash},
          try
              ct:log("~p ~p",[Curve,Hash]),
              {Pub,Priv} = crypto:generate_key(ecdh, Curve),
              true = is_binary(Pub),
              true = is_binary(Priv),
              Sig = crypto:sign(ecdsa, Hash, Msg, [Priv, Curve]),
              crypto:verify(ecdsa, Hash, Msg, Sig, [Pub, Curve])
          catch
              C:E ->
                  {C,E}
          end}
         || Curve <- Curves -- SkipCurves,
            Hash <- Hashs -- SkipHashs
        ],
    Fails =
        lists:filter(fun({_,true}) -> false;
                        (_) -> true
                     end, Results),
    case Fails of
        [] ->
            ok;
        _ ->
            ct:log("Fails:~n~p",[Fails]),
            Errors = lists:usort([Err || {_,Err} <- Fails]),
            FailedCurves = [Curve || {Curve,_} <- Fails],
            FailedCurvesPerError = [{E, [C || {C,E0} <- Fails,
                                              E0 == E]}
                                    || E <- Errors],
            ct:pal("~p failed curves: ~p", [length(FailedCurves), FailedCurves]),
            ct:pal("Failed curves per error:~n~s", 
                   [
                    [io_lib:format("  Error: ~p~n Curves: ~p~n~n", [E,Cs])
                     || {E,Cs} <- FailedCurvesPerError]
                   ]
                  ), 
            ct:fail("Bad curve(s)",[])
    end.

%%--------------------------------------------------------------------
use_all_ecdh_generate_compute(Config) ->
    SkipCurves0 = [ed25519, ed448, x25519, x448],
    SkipCurves =
        case crypto:info_fips() of
            enabled ->
                [secp192r1, prime192v1, sect163k1, sect163r2 | SkipCurves0];
            _ ->
                SkipCurves0
        end,
    Curves = crypto:supports(curves) -- SkipCurves,
    do_dh_curves(Config, Curves).

use_all_eddh_generate_compute(Config) ->
    AllCurves = crypto:supports(curves),
    Curves = [C || C <- [x25519, x448],
                     lists:member(C, AllCurves)],
    do_dh_curves(Config, Curves).

do_dh_curves(_Config, Curves) ->
    ct:log("Lib: ~p~nFIPS: ~p~nCurves:~n~p~n", [crypto:info_lib(),
                                                crypto:info_fips(),
                                                Curves]),
    Results =
        [{Curve,
          try
              ct:log("~p",[Curve]),
              {APub,APriv} = crypto:generate_key(ecdh, Curve),
              {BPub,BPriv} = crypto:generate_key(ecdh, Curve),
              true = is_binary(APub),
              true = is_binary(APriv),
              true = is_binary(BPub),
              true = is_binary(BPriv),

              ACommonSecret = crypto:compute_key(ecdh, BPub, APriv, Curve),
              BCommonSecret = crypto:compute_key(ecdh, APub, BPriv, Curve),
              ACommonSecret == BCommonSecret
          catch
              C:E ->
                  {C,E}
          end}
         || Curve <- Curves
        ],

    Fails =
        lists:filter(fun({_,true}) -> false;
                        (_) -> true
                     end, Results),

    Succedes =
        lists:filter(fun({_,true}) -> true;
                        (_) -> false
                     end, Results),

    case Fails of
        [] ->
            ct:comment("All ~p passed",[length(Results)]),
            ok;
        _ ->
            ct:comment("passed: ~p, failed: ~p",[length(Results)-length(Fails),length(Fails)]),
            ct:log("Succedes:~n~p",[Succedes]),
            ct:log("Fails:~n~p",[Fails]),
            Errors = lists:usort([Err || {_,Err} <- Fails]),
            FailedCurves = [C || {C,_} <- Fails],
            FailedCurvesPerError = [{E, [C || {C,E0} <- Fails,
                                              E == E0]}
                                    || E <- Errors],
            ct:pal("~p (~p) failed curves: ~p", [length(FailedCurves), length(Results), FailedCurves]),
            ct:pal("Failed curves per error:~n~s", 
                   [
                    [io_lib:format("  Error: ~p~n Curves: ~p~n~n", [E,Cs])
                     || {E,Cs} <- FailedCurvesPerError]
                   ]
                  ), 
            ct:fail("Bad curve(s)",[])
    end.

%%--------------------------------------------------------------------
generate() ->
     [{doc, " Test crypto:generate_key"}].
generate(Config) when is_list(Config) ->
    Gen = proplists:get_value(generate, Config),
    lists:foreach(fun do_generate/1, Gen).
%%--------------------------------------------------------------------
mod_pow() ->
    [{doc, "mod_pow testing (A ^ M % P with bignums)"}].
mod_pow(Config) when is_list(Config) ->
    mod_pow_aux_test(2, 5, 10, 8).
%%--------------------------------------------------------------------
exor() ->
    [{doc, "Test the exor function"}].
exor(Config) when is_list(Config) ->
    do_exor(<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>),
    do_exor(term_to_binary(lists:seq(1, 1000000))).
%%--------------------------------------------------------------------
hash_equals() ->
    [{doc, "Test the hash_equals function"}].
hash_equals(Config) when is_list(Config) ->
    try
        true = crypto:hash_equals(<<>>, <<>>),
        true = crypto:hash_equals(<<"abc">>, <<"abc">>),
        false = crypto:hash_equals(<<"abc">>, <<"abe">>)
    catch
        error:{notsup,{"hash_equals.c",_Line},"Unsupported CRYPTO_memcmp"++_} ->
            {skip, "No CRYPTO_memcmp"}
    end.
%%--------------------------------------------------------------------
rand_uniform() ->
    [{doc, "rand_uniform and random_bytes testing"}].
rand_uniform(Config) when is_list(Config) ->
    rand_uniform_aux_test(10),
    10 = byte_size(crypto:strong_rand_bytes(10)).

%%--------------------------------------------------------------------
rand_threads() ->
    [{doc, "strong_rand_bytes in parallel threads"}].
rand_threads(Config) when is_list(Config) ->
    %% This will crash the emulator on at least one version of libcrypto
    %% with buggy multithreading in RAND_bytes().
    %% The test needs to run at least a few minutes...
    NofThreads = 4,
    Fun = fun F() -> crypto:strong_rand_bytes(16), F() end,
    PidRefs = [spawn_monitor(Fun) || _ <- lists:seq(1, NofThreads)],
%%% The test case takes too much time to run.
%%% Keep it around for reference by setting it down to just 10 seconds.
%%%    receive after 10 * 60 * 1000 -> ok end, % 10 minutes
    receive after 10 * 1000 -> ok end, % 10 seconds
    spawn_link(fun () -> receive after 5000 -> exit(timeout) end end),
    [exit(Pid, stop) || {Pid,_Ref} <- PidRefs],
    [receive {'DOWN',Ref,_,_,stop} -> ok end || {_Pid,Ref} <- PidRefs],
    ok.

%%--------------------------------------------------------------------
rand_plugin() ->
    [{doc, "crypto rand plugin testing (implicit state / process dictionary)"}].
rand_plugin(Config) when is_list(Config) ->
    rand_plugin_aux(implicit_state).

rand_plugin_s() ->
    [{doc, "crypto rand plugin testing (explicit state)"}].
rand_plugin_s(Config) when is_list(Config) ->
    rand_plugin_aux(explicit_state).

%%--------------------------------------------------------------------
info(_Config) ->
    [{_,_,VerBin}] = crypto:info_lib(),
    LibVer = binary:bin_to_list(VerBin),
    try
        crypto:info()
    of
        #{cryptolib_version_compiled := LibVer,
          cryptolib_version_linked := LibVer,
          compile_type := Tc,
          link_type := Tl} when is_atom(Tc), is_atom(Tl) ->
            ok;

        %% Version strings in header vs lib seen to differ slightly on SUSE
        %% but OpenSSL version numbers should be the same
        #{cryptolib_version_compiled := CompVer,
          cryptolib_version_linked := LibVer,
          compile_type := Tc,
          link_type := Tl} when is_atom(Tc), is_atom(Tl) ->
            RE = "OpenSSL (\\d+\\.\\d+\\.\\d+.)",
            Opts = [{capture,first,list}],
            {match,[CompV]} = re:run(CompVer, RE, Opts),
            {match,[LinkV]} = re:run(LibVer, RE, Opts),
            {CompV,CompV} = {CompV,LinkV},
            ok;

        Other ->
            ct:log("LibVer = ~p~ncrypto:info() -> ~p", [LibVer,Other]),
            ct:fail("Version mismatch", [])
    catch
        C:E ->
            ct:log("Exception ~p:~p", [C,E]),
            ct:fail("Exception when calling crypto:info/0", [])
    end.

%%--------------------------------------------------------------------
cipher_info() ->
    [{doc, "crypto cipher_info testing"}].
cipher_info(Config) when is_list(Config) ->
    #{type := _,key_length := _,iv_length := _,
        block_size := _,mode := _} = crypto:cipher_info(aes_128_cbc),
    {'EXIT',_} = (catch crypto:cipher_info(not_a_cipher)),
    case lists:foldl(fun(C,Ok) ->
                             try crypto:cipher_info(C)
                             of
                                 _ -> Ok
                             catch Cls:Exc ->
                                     ct:log("~p:~p ~p",[Cls,Exc,C]),
                                     false
                             end
                     end,
                     true,
                     crypto:supports(ciphers)
                    )
    of
        true ->
            ok;
        false ->
            ct:fail('Cipher unsupported',[])
    end.
                                                                         
cipher_info_prop_aead_attr() ->
    [{doc, "crypto cipher_info prop_aead attribute testing"}].
cipher_info_prop_aead_attr(Config) when is_list(Config) ->
    AeadCiphers = [aes_128_ccm, aes_192_ccm, aes_256_ccm, aes_128_gcm, aes_192_gcm, aes_256_gcm, chacha20_poly1305],
    case lists:foldl(fun(C,Ok) ->
                        case crypto:cipher_info(C) of
                            #{prop_aead := true} ->
                                true and Ok;
                            _ ->
                                false
                        end
                     end,
                     true,
                     AeadCiphers
                    )
    of
        true ->
            ok;
        false ->
            ct:fail('AEAD Cipher attribute reported false',[])
    end,
    NonAeadCiphers = [aes_ige256, blowfish_cbc, blowfish_cfb64],
    case lists:foldl(fun(C,Ok) ->
                        case crypto:cipher_info(C) of
                            #{prop_aead := false} ->
                                true and Ok;
                            _ ->
                                false
                        end
                     end,
                     true,
                     NonAeadCiphers
                    )
    of
        true ->
            ok;
        false ->
            ct:fail('Non-AEAD Cipher attribute reported true',[])
    end.

%%--------------------------------------------------------------------
hash_info() ->
    [{doc, "crypto hash_info testing"}].
hash_info(Config) when is_list(Config) ->
    #{type := _,size := _,block_size := _} = crypto:hash_info(sha256),
    {'EXIT',_} = (catch crypto:hash_info(not_a_hash)),
    lists:foreach(fun(H) -> crypto:hash_info(H) end,
        proplists:get_value(hashs, crypto:supports())).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
hash(_, [], []) ->
    ok;
hash(Type, [Msg | RestMsg], [Digest| RestDigest]) ->
    case crypto:hash(Type, Msg) of
	Digest ->
	    hash(Type, RestMsg, RestDigest);
	Other ->
	    ct:fail({{crypto, hash, [Type, Msg]}, {expected, Digest}, {got, Other}})
    end.

hash_xof(_, [], [], []) ->
  ok;
hash_xof(Type, [Msg | RestMsg], [Digest | RestDigest], [Length | RestLength]) ->
  case crypto:hash_xof(Type, Msg, Length) of
    Digest ->
      hash_xof(Type, RestMsg, RestDigest, RestLength);
    Other ->
      ct:fail({{crypto, hash_xof, [Type, Msg, Length]}, {expected, Digest}, {got, Other}})
  end.

hash_increment(Type, Increments, Digest) ->
    State = crypto:hash_init(Type),
    case hash_increment(State, Increments) of
	Digest ->
	    ok;
	Other ->
	    ct:fail({{crypto, "hash_init/update/final", [Type, Increments]}, {expected, Digest}, {got, Other}})  
    end.

hash_increment(State, []) ->
    crypto:hash_final(State);
hash_increment(State0, [Increment | Rest]) ->
    State = crypto:hash_update(State0, Increment),
    hash_increment(State, Rest).

%%%----------------------------------------------------------------
mac_check({MacType, SubType, Key, Text, Mac}=T) ->
    ExpMac = iolist_to_binary(Mac),
    cipher_test(T,
                fun() -> crypto:mac(MacType, SubType, Key, Text) end,
                ExpMac);
mac_check({MacType, SubType, Key, Text, Size, Mac}=T) ->
    ExpMac = iolist_to_binary(Mac),
    cipher_test(T,
                fun() -> crypto:macN(MacType, SubType, Key, Text, Size) end,
                ExpMac).

mac_increment(Type, SubType, Key, Increments) ->
    Expected = crypto:mac(Type, SubType, Key, Increments),
    State = crypto:mac_init(Type, SubType, Key),
    case do_mac_increment(State, Increments) of
	Expected ->
	    ok;
	Other ->
	    ct:fail({{crypto, "mac_init/update/final", [Type, SubType, Increments]},
                     {expected, Expected},
                     {got, Other}})  
    end.

do_mac_increment(State, []) ->
    crypto:mac_final(State);
do_mac_increment(State0, [Increment | Rest]) ->
    State = crypto:mac_update(State0, Increment),
    do_mac_increment(State, Rest).

aead_cipher_ng({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, _Info}=T) ->
    Plain = iolist_to_binary(PlainText),
    cipher_test(T,
                fun() -> crypto:crypto_one_time_aead(Type, Key, IV, PlainText, AAD, true) end,
                {CipherText, CipherTag},
                fun() -> crypto:crypto_one_time_aead(Type, Key, IV, CipherText, AAD, CipherTag, false) end,
                Plain);
aead_cipher_ng({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, TagLen, _Info}=T) ->
    <<TruncatedCipherTag:TagLen/binary, _/binary>> = CipherTag,
    Plain = iolist_to_binary(PlainText),
    cipher_test(T,
                fun() -> crypto:crypto_one_time_aead(Type, Key, IV, PlainText, AAD, TagLen, true) end,
                {CipherText, TruncatedCipherTag},
                fun() -> crypto:crypto_one_time_aead(Type, Key, IV, CipherText, AAD, TruncatedCipherTag, false) end,
                Plain).

aead_cipher_bad_tag({Type, Key, _PlainText, IV, AAD, CipherText, CipherTag, _Info}=T) ->
    BadTag = mk_bad_tag(CipherTag),
    cipher_test(T,
                fun() -> crypto:crypto_one_time_aead(Type, Key, IV, CipherText, AAD, BadTag, false) end,
                error);
aead_cipher_bad_tag({Type, Key, _PlainText, IV, AAD, CipherText, CipherTag, TagLen, _Info}=T) ->
    <<TruncatedCipherTag:TagLen/binary, _/binary>> = CipherTag,
    BadTruncatedTag = mk_bad_tag(TruncatedCipherTag),
    cipher_test(T,
                fun() -> crypto:crypto_one_time_aead(Type, Key, IV, CipherText, AAD, BadTruncatedTag, false) end,
                error).


cipher_test(T, Fe, Ee, Fd, Ed) ->
    %% Test encrypt
    Re = cipher_test(encrypt, T, Fe, Ee),
    %% Test decrypt
    Rd = cipher_test(decrypt, T, Fd, Ed),
    case {Re, Rd} of
        {ok,ok} -> ok;
        {ok,_} -> Rd;
        {_,ok} -> Re;
        _ -> {Re,Rd}
    end.

cipher_test(T, F, E) ->
    cipher_test(notag, T, F, E).

cipher_test(Tag, T, F, E) ->
    try F() of
        E -> ok;
        Other -> {other, {Tag,T,Other}}
    catch
        error:Error -> ct:pal("Tag = ~p,~n T = ~p,~n F = ~p,~n E = ~p,~n Error = ~p", [Tag, T, F, E, Error]), {error, {Tag,T,Error}}
    end.

do_cipher_tests(F, TestVectors) when is_function(F,1) ->
    {Passed,Failed} =
        lists:partition(
          fun(R) -> R == ok end,
          lists:map(F, TestVectors)
         ),
    BothFailed = lists:filter(fun({ok,_}) -> false;
                                 ({_,ok}) -> false;
                                 (ok) -> false;
                                 (_) -> true
                              end,
                              Failed),
    ct:log("Passed: ~p, BothFailed: ~p OnlyOneFailed: ~p",
           [length(Passed), length(BothFailed), length(Failed)-length(BothFailed)]),
    case Failed of
        [] ->
            ct:comment("All ~p passed", [length(Passed)]);
        _ ->
            ct:log("~p",[hd(Failed)]),
            ct:comment("Passed: ~p, BothFailed: ~p OnlyOneFailed: ~p",
                       [length(Passed), length(BothFailed), length(Failed)-length(BothFailed)]),
            ct:fail("Failed", [])
    end.


mk_bad_tag(CipherTag) ->
    case <<0:(size(CipherTag))/unit:8>> of
        CipherTag -> % The correct tag may happen to be a suite of zeroes
            <<1:(size(CipherTag))/unit:8>>;
        X ->
            X
    end.

do_sign_verify({Type, undefined=Hash, Private, Public, Msg, Signature}) ->
    case crypto:sign(eddsa, Hash, Msg, [Private,Type]) of
        Signature ->
            ct:log("OK crypto:sign(eddsa, ~p, Msg, [Private,~p])", [Hash,Type]),
            case crypto:verify(eddsa, Hash, Msg, Signature, [Public,Type]) of
                true ->
                    ct:log("OK crypto:verify(eddsa, ~p, Msg, Signature, [Public,~p])", [Hash,Type]),
                    negative_verify(eddsa, Hash, Msg, <<10,20>>, [Public,Type]);
                false ->
                    ct:log("ERROR crypto:verify(eddsa, ~p, Msg= ~p, Signature= ~p, [Public= ~p,~p])",
                           [Hash,Msg,Signature,Public,Type]),
                    ct:fail({{crypto, verify, [eddsa, Hash, Msg, Signature, [Public,Type]]}})
            end;
        ErrorSig ->
            ct:log("ERROR crypto:sign(~p, ~p, ..., [Private= ~p,~p])", [eddsa,Hash,Private,Type]),
            ct:log("ERROR crypto:verify(eddsa, ~p, Msg= ~p, [Public= ~p,~p])~n"
                   "ErrorSig   = ~p~n"
                   "CorrectSig = ~p~n"
                  ,
                   [Hash,Msg,Public,Type,ErrorSig,Signature]),
            ct:fail({{crypto, sign, [Type, Hash, Msg, ErrorSig, [Private]]}})
    end;

do_sign_verify({Type, Hash, Public, Private, Msg}) ->
    case {Hash, crypto:info_fips()} of
        {sha, enabled} ->
            io:format("Skip sign with SHA for FIPS\n");
        _ ->
            Signature = crypto:sign(Type, Hash, Msg, Private),
            case crypto:verify(Type, Hash, Msg, Signature, Public) of
                true ->
                    ct:log("OK crypto:sign(~p, ~p, ..., ..., ...)", [Type,Hash]),
                    negative_verify(Type, Hash, Msg, <<10,20>>, Public);
                false ->
                    ct:log("ERROR crypto:sign(~p, ~p, ..., ..., ...)", [Type,Hash]),
                    ct:fail({{crypto, verify, [Type, Hash, Msg, Signature, Public]}})
            end
    end;
do_sign_verify({Type, Hash, Public, Private, Msg, Options}) ->
    LibVer =
        case crypto:info_lib() of
            [{<<"OpenSSL">>,Ver,<<"OpenSSL",_/binary>>}] -> Ver;
            _ -> infinity
        end,
    Pad = proplists:get_value(rsa_padding, Options),
    NotSupLow = lists:member(Pad, [rsa_pkcs1_pss_padding]),
    try
        crypto:sign(Type, Hash, Msg, Private, Options)
    of
        Signature ->
            case crypto:verify(Type, Hash, Msg, Signature, Public, Options) of
                true ->
                    ct:log("OK crypto:sign(~p, ~p, ..., ..., ..., ~p)", [Type,Hash,Options]),
                    negative_verify(Type, Hash, Msg, <<10,20>>, Public, Options);
                false ->
                    ct:log("ERROR crypto:sign(~p, ~p, ..., ..., ..., ~p)", [Type,Hash,Options]),
                    ct:fail({{crypto, verify, [Type, Hash, Msg, Signature, Public, Options]}})
            end
    catch
        error:notsup when NotSupLow == true,
                          is_integer(LibVer),
                          LibVer < 16#10001000 ->
            %% Those opts where introduced in 1.0.1
            ct:log("notsup but OK in old cryptolib crypto:sign(~p, ~p, ..., ..., ..., ~p)",
                   [Type,Hash,Options]),
            true;
        error:{notsup,_,_} when NotSupLow == true,
                          is_integer(LibVer),
                          LibVer < 16#10001000 ->
            %% Those opts where introduced in 1.0.1
            ct:log("notsup but OK in old cryptolib crypto:sign(~p, ~p, ..., ..., ..., ~p)",
                   [Type,Hash,Options]),
            true;
        C:E ->
            ct:log("~p:~p  crypto:sign(~p, ~p, ..., ..., ..., ~p)", [C,E,Type,Hash,Options]),
            ct:fail({{crypto, sign_verify, [LibVer, Type, Hash, Msg, Public, Options]}})
    end.

negative_verify(Type, Hash, Msg, Signature, Public) ->
    case crypto:verify(Type, Hash, Msg, Signature, Public) of
	true ->
	    ct:fail({{crypto, verify, [Type, Hash, Msg, Signature, Public]}, should_fail});
	false ->
	    ok
    end.

negative_verify(Type, Hash, Msg, Signature, Public, Options) ->
    case crypto:verify(Type, Hash, Msg, Signature, Public, Options) of
	true ->
	    ct:fail({{crypto, verify, [Type, Hash, Msg, Signature, Public, Options]}, should_fail});
	false ->
	    ok
    end.

do_public_encrypt({Type, Public, Private, Msg, Padding}) ->
    ct:log("do_public_encrypt Type=~p, Padding=~p,~nPublic = ~p,~nPrivate = ~p,~nMsg = ~p.",
           [Type, Padding, Public, Private, Msg]),
    timer:sleep(100),
    try
        crypto:public_encrypt(Type, Msg, Public, Padding)
    of
        PublicEcn ->
            ct:log("private_decrypt~nPublicEcn = ~p.", [PublicEcn]),
            timer:sleep(100),
            try
                crypto:private_decrypt(Type, PublicEcn, Private, Padding)
            of
                Msg ->
                    ct:log("~p:~p ok", [?MODULE,?LINE]),
                    timer:sleep(100),
                    ok;
                Other ->
                    ct:log("~p:~p Other = ~p", [?MODULE,?LINE,Other]),
                    timer:sleep(100),
                    ct:fail({{crypto, private_decrypt, [Type, PublicEcn, Private, Padding]}, {expected, Msg}, {got, Other}})
            catch
                CC:EE ->
                    ct:log("~p:~p EXC. ~p:~p", [?MODULE,?LINE,CC,EE]),
                    timer:sleep(100),
                    ct:fail({{crypto, private_decrypt, [Type, PublicEcn, Private, Padding]}, {expected, Msg}, {got, {CC,EE}}})
            end
    catch
        CC:EE ->
            ct:log("~p:~p EXC 2. ~p:~p", [?MODULE,?LINE,CC,EE]),
            timer:sleep(100),
            ct:fail({{crypto, public_encrypt, [Type, Msg, Public, Padding]}, {got, {CC,EE}}})
    end. 


do_private_encrypt({Type, Public, Private, Msg, Padding}) ->
    ct:log("do_private_encrypt Type=~p, Padding=~p,~nPublic = ~p,~nPrivate = ~p,~nMsg = ~p.",
           [Type, Padding, Public, Private, Msg]),
    try
        crypto:private_encrypt(Type, Msg, Private, Padding)
    of
        PrivEcn ->
            try
                ct:log("public_decrypt~nPrivEcn = ~p.", [PrivEcn]),
                crypto:public_decrypt(Type, PrivEcn, Public, Padding)
            of
                Msg ->
                    ct:log("~p:~p ok", [?MODULE,?LINE]),
                    ok;
                Other ->
                    ct:log("~p:~p Other = ~p", [?MODULE,?LINE,Other]),
                    ct:fail({{crypto, public_decrypt, [Type, PrivEcn, Public, Padding]}, {expected, Msg}, {got, Other}})
            catch
                CC:EE ->
                    ct:log("~p:~p EXC. ~p:~p", [?MODULE,?LINE,CC,EE]),
                    ct:fail({{crypto, public_decrypt, [Type, PrivEcn, Public, Padding]}, {expected, Msg}, {got, {CC,EE}}})
            end
    catch
        CC:EE ->
            ct:log("~p:~p EXC 2. ~p:~p", [?MODULE,?LINE,CC,EE]),
            ct:fail({{crypto, private_encrypt, [Type, Msg, Private, Padding]}, {got, {CC,EE}}})
    end.

do_generate_compute({srp = Type, UserPrivate, UserGenParams, UserComParams,
		     HostPublic, HostPrivate, HostGenParams, HostComParam, SessionKey}) ->
    {UserPublic, UserPrivate} = crypto:generate_key(Type, UserGenParams, UserPrivate),
    {HostPublic, HostPrivate} = crypto:generate_key(Type, HostGenParams, HostPrivate),
    SessionKey = crypto:compute_key(Type, HostPublic, {UserPublic, UserPrivate},
     				    UserComParams),
    SessionKey = crypto:compute_key(Type, UserPublic, {HostPublic, HostPrivate},
				    HostComParam);



do_generate_compute({dh, P, G}) ->
    {UserPub, UserPriv} = crypto:generate_key(dh, [P, G]),
    {HostPub, HostPriv} = crypto:generate_key(dh, [P, G]),
    SharedSecret = crypto:compute_key(dh, HostPub, UserPriv, [P, G]),
    SharedSecret = crypto:compute_key(dh, UserPub, HostPriv, [P, G]).
    
do_compute({ecdh = Type, Pub, Priv, Curve, SharedSecret}) ->
    ct:log("~p ~p", [Type,Curve]),
    Secret = crypto:compute_key(Type, Pub, Priv, Curve),
     case Secret of
	 SharedSecret ->
	     ok;
	 Other ->
	     ct:fail({{crypto, compute_key, [Type, Pub, Priv, Curve]}, {expected, SharedSecret}, {got, Other}})
     end.

do_generate({Type, Curve, Priv, Pub}) when Type == ecdh ; Type == eddsa ->
    ct:log("~p ~p", [Type,Curve]),
    case crypto:generate_key(Type, Curve, Priv) of
	{Pub, _} ->
	    ok;
	{Other, _} ->
	    ct:fail({{crypto, generate_key, [Type, Priv, Curve]}, {expected, Pub}, {got, Other}})
    end;
do_generate({rsa = Type, Mod, Exp}) ->
    ct:log("~p", [Type]),
    case crypto:info_fips() of
        enabled when Mod < 3072 ->
            ct:log("SKIP do_generate ~p FIPS=~p, Mod=~p  Exp=~p", [Type, enabled, Mod, Exp]),
            {skip, "FIPS violation"};
        FIPS ->
            ct:log("do_generate ~p FIPS=~p, Mod=~p  Exp=~p", [Type, FIPS, Mod, Exp]),
            {Pub,Priv} = crypto:generate_key(Type, {Mod,Exp}),
            do_sign_verify({rsa, sha256, Pub, Priv, rsa_plain()})
    end.

notsup(Fun, Args) ->
    Result =
        try
            {error, {return, apply(Fun, Args)}}
        catch
            error:notsup ->
                ok;
            error: {notsup, _, _} ->
                ok;
            Class:Error ->
                {error, {Class, Error}}
        end,
    case Result of
        ok ->
            ok;
        {error, Value} ->
            {module, Module} = erlang:fun_info(Fun, module),
            {name,   Name}   = erlang:fun_info(Fun, name),
            ct:fail({{Module, Name, Args}, {expected, {error, notsup}}, {got, Value}})
    end.

hexstr2point(X, Y) ->
    <<4:8, (hexstr2bin(X))/binary, (hexstr2bin(Y))/binary>>.

hexstr2bin(S) when is_binary(S) ->
    list_to_binary(hexstr2list(binary_to_list(S)));
hexstr2bin(S) ->
    list_to_binary(hexstr2list(S)).

hexstr2list([$ |T]) ->
    hexstr2list(T);
hexstr2list([X,Y|T]) ->
    [mkint(X)*16 + mkint(Y) | hexstr2list(T)];
hexstr2list([]) ->
    [].
mkint(C) when $0 =< C, C =< $9 ->
    C - $0;
mkint(C) when $A =< C, C =< $F ->
    C - $A + 10;
mkint(C) when $a =< C, C =< $f ->
    C - $a + 10.

decstr2int(S) when is_binary(S) ->
    list_to_integer(binary:bin_to_list(S));
decstr2int(S) ->
    list_to_integer(S).

is_supported(Group) ->
    lists:member(Group, lists:append([Algo ||  {_, Algo}  <- crypto:supports()])). 


mac_listify(Blocks) ->
    lists:map(fun do_mac_listify/1, Blocks).

do_mac_listify({MType, Type, Key, Text, CMac}) ->
    {MType, Type, iolistify(Key), iolistify(Text), CMac};
do_mac_listify({MType, Type, Key, Text, Size, CMac}) ->
    {MType, Type, iolistify(Key), iolistify(Text), Size, CMac}.


iolistify(X) ->
    iolistify1(lazy_eval(X)).

iolistify1(<<"Test With Truncation">>)->
    %% Do not iolistify as it spoils this special case
    <<"Test With Truncation">>;
iolistify1(Msg) when is_binary(Msg) ->
    Length = erlang:byte_size(Msg),
    Split = Length div 2,
    List0 = binary_to_list(Msg),
   case lists:split(Split, List0) of
       {[Element | List1], List2} ->
	   [[Element], List1, List2];
       {List1, List2}->
	   [List1, List2]
   end;
iolistify1(Msg) when is_list(Msg) ->
    iolistify1(list_to_binary(Msg)).

%%--------------------------------------------------------------------
mod_pow_aux_test(_, _, _, 0) ->
    ok;
mod_pow_aux_test(B, E, M, N) ->
    Result = crypto:bytes_to_integer(crypto:mod_pow(B, E, M)),
    Result = ipow(B, E, M),
    mod_pow_aux_test(B, E*E+1, M*M+1, N-1).

%% mod_exp in erlang (copied from jungerl's ssh_math.erl)
ipow(A, B, M) when M > 0, B >= 0 ->
    if A == 1 -> 
 	    1;
       true -> 
 	    ipow(A, B, M, 1)
    end.

ipow(A, 1, M, Prod) ->
    (A*Prod) rem M;
ipow(_A, 0, _M, Prod) ->
    Prod;
ipow(A, B, M, Prod)  ->
    B1 = B bsr 1,
    A1 = (A*A) rem M,
    if B - B1 == B1 ->
	    ipow(A1, B1, M, Prod);
       true ->
	    ipow(A1, B1, M, (A*Prod) rem M)
    end.

do_exor(B) ->
    Z1 = zero_bin(B),
    Z1 = crypto:exor(B, B),
    B1 = crypto:strong_rand_bytes(100),
    B2 = crypto:strong_rand_bytes(100),
    Z2 = zero_bin(B1),
    Z2 = crypto:exor(B1, B1),
    Z2 = crypto:exor(B2, B2),
    R = xor_bytes(B1, B2),
    R = crypto:exor(B1, B2).

zero_bin(N) when is_integer(N) ->
    N8 = N * 8,
    <<0:N8/integer>>;
zero_bin(B) when is_binary(B) ->
    zero_bin(size(B)).
xor_bytes(Bin1, Bin2) when is_binary(Bin1), is_binary(Bin2) ->
    L1 = binary_to_list(Bin1),
    L2 = binary_to_list(Bin2),
    list_to_binary(xor_bytes(L1, L2));
xor_bytes(L1, L2) ->
    xor_bytes(L1, L2, []).

xor_bytes([], [], Acc) ->
    lists:reverse(Acc);
xor_bytes([N1 | Tl1], [N2 | Tl2], Acc) ->
    xor_bytes(Tl1, Tl2, [N1 bxor N2 | Acc]).
rand_uniform_aux_test(0) ->
    ok;
rand_uniform_aux_test(N) ->
    L = N*1000,
    H = N*100000+1,
    crypto_rand_uniform(L, H),
    crypto_rand_uniform(-L, L),
    crypto_rand_uniform(-H, -L),
    crypto_rand_uniform(-H, L),
    rand_uniform_aux_test(N-1).

crypto_rand_uniform(L,H) ->
    R1 = (L-1) + rand:uniform(H-L),
    case (R1 >= L) and (R1 < H) of
	true  ->
	    ok;
	false ->
	    ct:fail({"Not in interval", R1, L, H})
    end.

foldallmap(_Fun, AccN, []) ->
    {true, AccN};
foldallmap(Fun, AccN, [H|T]) ->
    case Fun(H, AccN) of
        {true, AccM} -> foldallmap(Fun, AccM, T);
        {{false, Result}, AccM} -> {Result, AccM}
    end.

allmap(_Fun, []) ->
    true;
allmap(Fun, [H|T]) ->
    case Fun(H) of
        true -> allmap(Fun, T);
        {false, Result} -> Result
    end.

rand_plugin_aux(StateType) ->
    {Seeder, SeedExporter, FloatGenerator, IntegerGenerator} = rand_plugin_functions(StateType),
    State0 = Seeder(),
    {crypto, no_seed} = SeedExporter(State0),
    {FloatTestResult, State1} = rand_plugin_aux_floats(State0, FloatGenerator),
    case FloatTestResult of
        true ->
            {IntegerTestResult, _State2} = rand_plugin_aux_integers(State1, IntegerGenerator),
            IntegerTestResult;
        {false, _} ->
            FloatTestResult
    end.

% returns {Seeder, SeedExporter, FloatGenerator, IntegerGenerator} with consistent signatures
rand_plugin_functions(implicit_state) ->
    {fun () -> crypto:rand_seed(), implicit_state end,
     fun (implicit_state) -> rand:export_seed() end,
     fun (implicit_state) -> {rand:uniform(), implicit_state} end,
     fun (N, implicit_state) -> {rand:uniform(N), implicit_state} end};
rand_plugin_functions(explicit_state) ->
    {fun crypto:rand_seed_s/0,
     fun rand:export_seed_s/1,
     fun rand:uniform_s/1,
     fun rand:uniform_s/2}.

rand_plugin_aux_floats(State0, FloatGenerator) ->
    {FloatSamples, State1} =
        lists:mapfoldl(
          fun (_, StateAcc) ->
                  FloatGenerator(StateAcc)
          end,
          State0,
          lists:seq(1, 10000)),

    {allmap(
       fun (V) ->
               (V >= 0.0 andalso V < 1.0)
               orelse {false, ct:fail({"Float sample not in interval", V, 0.0, 1.0})}
       end,
       FloatSamples),
     State1}.

rand_plugin_aux_integers(State0, IntegerGenerator) ->
    MaxIntegerCeiling = 1 bsl 32,
    {IntegerCeilings, State1} =
        lists:mapfoldl(
          fun (_, StateAcc) ->
                  IntegerGenerator(MaxIntegerCeiling, StateAcc)
          end,
          State0,
          lists:seq(1, 100)),

    foldallmap(
      fun (Ceiling, StateAcc) ->
              case Ceiling >= 1 andalso Ceiling =< MaxIntegerCeiling of
                  false ->
                      {{false, ct:fail({"Integer ceiling not in interval",
                                        Ceiling, 1, MaxIntegerCeiling})},
                       StateAcc};
                  true ->
                      foldallmap(
                        fun (_, SubStateAcc) ->
                                {Sample, NewSubStateAcc} = IntegerGenerator(Ceiling, SubStateAcc),
                                case Sample >= 1 andalso Sample =< Ceiling of
                                    false ->
                                        {{false, ct:fail({"Integer sample not in interval",
                                                          Sample, 1, Ceiling})},
                                         NewSubStateAcc};
                                    true ->
                                        {true, NewSubStateAcc}
                                end
                        end,
                        StateAcc,
                        lists:seq(1, 100))
              end
      end,
      State1,
      IntegerCeilings).

%%--------------------------------------------------------------------
%% Test data ------------------------------------------------
%%--------------------------------------------------------------------
group_config(md4 = Type, Config) ->
    Msgs = rfc_1321_msgs(),
    Digests = rfc_1321_md4_digests(),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(md5 = Type, Config) ->
    Msgs = rfc_1321_msgs(),
    Digests = rfc_1321_md5_digests(),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(ripemd160 = Type, Config) ->
    Msgs = ripemd160_msgs(),
    Digests = ripemd160_digests(),
   [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha = Type, Config) ->
    Msgs = [rfc_4634_test1(), rfc_4634_test2_1(),long_msg()],
    Digests = rfc_4634_sha_digests() ++ [long_sha_digest()],
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha224 = Type, Config) ->
    Msgs = [rfc_4634_test1(), rfc_4634_test2_1()], 
    Digests = rfc_4634_sha224_digests(),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha256 = Type, Config) ->
    Msgs =   [rfc_4634_test1(), rfc_4634_test2_1(), long_msg()],
    Digests = rfc_4634_sha256_digests()  ++ [long_sha256_digest()],
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha384 = Type, Config) ->
    Msgs =  [rfc_4634_test1(), rfc_4634_test2(), long_msg()],
    Digests = rfc_4634_sha384_digests()  ++ [long_sha384_digest()],
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha512 = Type, Config) ->
    Msgs =  [rfc_4634_test1(), rfc_4634_test2(), long_msg()],
    Digests = rfc_4634_sha512_digests() ++ [long_sha512_digest()],
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha3_224 = Type, Config) ->
    {Msgs,Digests} = sha3_test_vectors(Type),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha3_256 = Type, Config) ->
    {Msgs,Digests} = sha3_test_vectors(Type),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha3_384 = Type, Config) ->
    {Msgs,Digests} = sha3_test_vectors(Type),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(sha3_512 = Type, Config) ->
    {Msgs,Digests} = sha3_test_vectors(Type),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(shake128 = Type, Config) ->
    {Msgs,Digests,Lengths} = sha3_shake128_test_vectors(Type),
    [{hash_xof, {Type, Msgs, Digests, Lengths}} | Config];
group_config(shake256 = Type, Config) ->
    {Msgs,Digests,Lengths} = sha3_shake256_test_vectors(Type),
    [{hash_xof, {Type, Msgs, Digests, Lengths}} | Config];
group_config(blake2b = Type, Config) ->
    {Msgs, Digests} = blake2_test_vectors(Type),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(blake2s = Type, Config) ->
    {Msgs, Digests} = blake2_test_vectors(Type),
    [{hash, {Type, Msgs, Digests}} | Config];
group_config(rsa, Config) ->
    Msg = rsa_plain(),
    Public = rsa_public(),
    Private = rsa_private(),
    PublicS = rsa_public_stronger(),
    PrivateS = rsa_private_stronger(),
    MsgPubEnc = <<"7896345786348 Asldi">>,
    SignVerify_OptsToTry = [[{rsa_padding, rsa_x931_padding}],
                            [{rsa_padding, rsa_pkcs1_padding}],
                            [{rsa_padding, rsa_pkcs1_pss_padding}],
                            [{rsa_padding, rsa_pkcs1_pss_padding}, {rsa_pss_saltlen, -2}],
                            [{rsa_padding, rsa_pkcs1_pss_padding}, {rsa_pss_saltlen, 5}],
                            [{rsa_padding, rsa_pkcs1_pss_padding}, {rsa_mgf1_md,sha}],
                            [{rsa_padding, rsa_pkcs1_pss_padding}, {rsa_mgf1_md,sha}, {rsa_pss_saltlen, 5}]
                           ],
    PrivEnc_OptsToTry = [rsa_pkcs1_padding,              % Compatibility
                         [{rsa_pad, rsa_pkcs1_padding}], % Compatibility
                         [{rsa_padding, rsa_pkcs1_padding}],
                         [{rsa_padding,rsa_x931_padding}]
                        ],
    PubEnc_OptsToTry = [rsa_pkcs1_padding,              % Compatibility
                 [{rsa_pad, rsa_pkcs1_padding}], % Compatibility
                 [{rsa_padding, rsa_pkcs1_padding}],
                 [{rsa_padding,rsa_pkcs1_oaep_padding}],
                 [{rsa_padding,rsa_pkcs1_oaep_padding}, {rsa_oaep_label, <<"Hej hopp">>}],
                 [{rsa_padding,rsa_pkcs1_oaep_padding}, {rsa_oaep_md,sha}],
                 [{rsa_padding,rsa_pkcs1_oaep_padding}, {rsa_oaep_md,sha}, {rsa_oaep_label, <<"Hej hopp">>}],
                 [{rsa_padding,rsa_pkcs1_oaep_padding}, {rsa_mgf1_md,sha}],
                 [{rsa_padding,rsa_pkcs1_oaep_padding}, {rsa_mgf1_md,sha}, {rsa_oaep_label, <<"Hej hopp">>}],
                 [{rsa_padding,rsa_pkcs1_oaep_padding}, {rsa_mgf1_md,sha}, {rsa_oaep_md,sha}, {rsa_oaep_label, <<"Hej hopp">>}]
                 ],
    RsaSignVerify = rsa_sign_verify_tests(Config, Msg, Public, Private, PublicS, PrivateS, SignVerify_OptsToTry),
    [{sign_verify,      RsaSignVerify},
     {pub_priv_encrypt, gen_rsa_pub_priv_tests(PublicS, PrivateS, MsgPubEnc, PrivEnc_OptsToTry)},
     {pub_pub_encrypt,  gen_rsa_pub_priv_tests(PublicS, PrivateS, MsgPubEnc, PubEnc_OptsToTry)},
     {privkey_to_pubkey, get_priv_pub_from_sign_verify(RsaSignVerify)},
     {generate, [{rsa, 1024, 3},  {rsa, 2048, 17},  {rsa, 3072, 65537}]}
     | Config];
group_config(dss = Type, Config) ->
    Msg = dss_plain(),
    Public = dss_params() ++ [dss_public()], 
    Private = dss_params() ++ [dss_private()], 
    SupportedHashs = proplists:get_value(hashs, crypto:supports(), []),
    DssHashs0 =
        case crypto:info_lib() of
            [{<<"OpenSSL">>,LibVer,_}] when is_integer(LibVer), LibVer > 16#10001000 ->
                [sha224, sha256, sha384, sha512];
            [{<<"OpenSSL">>,LibVer,_}] when is_integer(LibVer), LibVer > 16#10000000 ->
                [sha224, sha256];
            _Else ->
                []
        end,
    DssHashs = case crypto:info_fips() of
                   enabled -> DssHashs0;
                   _ -> [sha | DssHashs0]
               end,
    SignVerify = [{Type, Hash, Public, Private, Msg} 
                  || Hash <- DssHashs,
                     lists:member(Hash, SupportedHashs)],
    MsgPubEnc = <<"7896345786348 Asldi">>,
    PubPrivEnc = [{dss, Public, Private, MsgPubEnc, []}],
    [{sign_verify, SignVerify},
     {pub_priv_encrypt, PubPrivEnc},
     {privkey_to_pubkey, get_priv_pub_from_sign_verify(SignVerify)}
     | Config];
group_config(ecdsa = Type, Config) ->
    {Private, Public} = ec_key_named(),
    Msg = ec_msg(),
    SupportedHashs = proplists:get_value(hashs, crypto:supports(), []),
    DssHashs = [sha, sha224, sha256, sha384, sha512],
    SignVerify = [{Type, Hash, Public, Private, Msg} 
                  || Hash <- DssHashs,
                     lists:member(Hash, SupportedHashs)],
    MsgPubEnc = <<"7896345786348 Asldi">>,
    PubPrivEnc = [{ecdsa, Public, Private, MsgPubEnc, []}],
    [{sign_verify, SignVerify}, {pub_priv_encrypt, PubPrivEnc} | Config];
group_config(Type, Config) when Type == ed25519 ; Type == ed448 ->
    TestVectors = eddsa(Type),
    Generate = lists:map(fun({Curve, _Hash, Priv, Pub, _Msg, _Signature}) ->
                             {eddsa, Curve, Priv, Pub}
                         end, TestVectors),
    [{sign_verify,TestVectors}, {generate, Generate} | Config]; 
group_config(srp, Config) ->
    GenerateCompute = [srp3(), srp6(), srp6a(), srp6a_smaller_prime()],
    [{generate_compute, GenerateCompute} | Config];
group_config(ecdh, Config) ->
    Compute = ecdh(),
    Generate = ecc(),
    [{compute, Compute}, {generate, Generate} | Config];
group_config(eddh, Config) ->
    [{compute, []}, {generate, []} | Config];
group_config(dh, Config) ->
    GenerateCompute = [dh()],
    [{generate_compute, GenerateCompute} | Config];
group_config(poly1305, Config) ->
    V = [%% {Key, Txt, Expect}
         {%% RFC7539 2.5.2
           hexstr2bin("85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"),
           <<"Cryptographic Forum Research Group">>,
           hexstr2bin("a8061dc1305136c6c22b8baf0c0127a9")
         }
        ],
    [{poly1305,V} | Config];

group_config(F, Config) ->
    TestVectors = fun() -> ?MODULE:F(Config) end,
    [{cipher, TestVectors} | Config].


configure_mac(MacType, SubType, Config) ->
    case do_configure_mac(MacType, SubType, Config) of
        undefined ->
            {skip, io_lib:format("No ~p test vectors for ~p", [MacType, SubType])};
        Pairs ->
            [{MacType, Pairs} | Config]
    end.

do_configure_mac(hmac, Type, _Config) ->
    case Type of
        md5 ->
            Keys = rfc_2202_md5_keys() ++ [long_hmac_key(md5)],
            Data = rfc_2202_msgs() ++ [long_msg()],
            Hmac = rfc_2202_hmac_md5()  ++ [long_hmac(md5)],
            zip3_special(hmac, Type, Keys, Data, Hmac);
        sha ->
            Keys = rfc_2202_sha_keys() ++ [long_hmac_key(sha)],
            Data = rfc_2202_msgs() ++ [long_msg()],
            Hmac = rfc_2202_hmac_sha()  ++ [long_hmac(sha)],
            zip3_special(hmac, Type, Keys, Data, Hmac);
        sha224 ->
            Keys = rfc_4231_keys(),
            Data = rfc_4231_msgs(),
            Hmac = rfc4231_hmac_sha224(),
            zip3_special(hmac, Type, Keys, Data, Hmac);
        sha256 ->
            Keys = rfc_4231_keys() ++ [long_hmac_key(sha256)],
            Data = rfc_4231_msgs()  ++ [long_msg()],
            Hmac = rfc4231_hmac_sha256()  ++ [long_hmac(sha256)],
            zip3_special(hmac, Type, Keys, Data, Hmac);
        sha384 ->
            Keys = rfc_4231_keys() ++ [long_hmac_key(sha384)],
            Data = rfc_4231_msgs()  ++ [long_msg()],
            Hmac = rfc4231_hmac_sha384()  ++ [long_hmac(sha384)],
            zip3_special(hmac, Type, Keys, Data, Hmac);
        sha512 ->
            Keys = rfc_4231_keys() ++ [long_hmac_key(sha512)],
            Data = rfc_4231_msgs() ++ [long_msg()],
            Hmac = rfc4231_hmac_sha512() ++ [long_hmac(sha512)],
            zip3_special(hmac, Type, Keys, Data, Hmac);
        sha3_224 ->
            hmac_sha3(Type);
        sha3_256 ->
            hmac_sha3(Type);
        sha3_384 ->
            hmac_sha3(Type);
        sha3_512 ->
            hmac_sha3(Type);
        blake2b ->
            blake2_hmac(Type);
        blake2s ->
            blake2_hmac(Type);
        _ ->
            undefined
    end;
do_configure_mac(cmac, Cipher, Config) ->
    case Cipher of
        aes_128_cbc ->
            fun() -> read_rsp(Config, Cipher,  ["CMACGenAES128.rsp", "CMACVerAES128.rsp"]) end;
        aes_192_cbc ->
            fun() -> read_rsp(Config, Cipher,  ["CMACGenAES192.rsp", "CMACVerAES192.rsp"]) end;
        aes_256_cbc ->
            fun() -> read_rsp(Config, Cipher,  ["CMACGenAES256.rsp", "CMACVerAES256.rsp"]) end;
        des_ede3_cbc ->
            fun() -> read_rsp(Config, Cipher,  ["CMACGenTDES3.rsp", "CMACVerTDES3.rsp"]) end;
        _ ->
            undefined
    end.


zip3_special(Type, SubType, As, Bs, Cs) ->
    [mk_zip3_special_elem(Type, SubType, A, B, C)
     || {A,B,C} <- lists:zip3(As, Bs, Cs)].

mk_zip3_special_elem(hmac, sha, Key, <<"Test With Truncation">>=Data, Expected) ->
    {hmac, sha, Key, Data, 20, Expected};
mk_zip3_special_elem(hmac, SubType, Key, <<"Test With Truncation">>=Data, Expected) ->
    {hmac, SubType, Key, Data, 16, Expected};
mk_zip3_special_elem(Type, SubType, A, B, C) ->
    {Type, SubType, A, B, C}.

rsa_sign_verify_tests(Config, Msg, Public, Private, PublicS, PrivateS, OptsToTry) ->
        case ?config(fips, Config) of
            true ->
                %% Use only the strong keys in FIPS mode
                rsa_sign_verify_tests(Msg,
                                      PublicS, PrivateS,
                                      PublicS, PrivateS,
                                      OptsToTry);
            false ->
                rsa_sign_verify_tests(Msg,
                                      Public,  Private,
                                      PublicS, PrivateS,
                                      OptsToTry)
        end.

rsa_sign_verify_tests(Msg, Public, Private, PublicS, PrivateS, OptsToTry) ->
    Hashs0 = [sha224, sha256],
    Hashs = case crypto:info_fips() of
                enabled -> Hashs0;
                _ -> [md5, ripemd160, sha | Hashs0]
            end,
    gen_rsa_sign_verify_tests(Hashs, Msg, Public, Private,
                              [undefined | OptsToTry]) ++
	gen_rsa_sign_verify_tests([sha384, sha512], Msg, PublicS, PrivateS,
                                  [undefined | OptsToTry]).

gen_rsa_sign_verify_tests(Hashs, Msg, Public, Private, Opts) ->
    SupOpts = proplists:get_value(rsa_opts, crypto:supports(), []),
    lists:foldr(fun(Hash, Acc0) ->
	case is_supported(Hash) of
	    true ->
		lists:foldr(fun
		    (undefined, Acc1) ->
			[{rsa, Hash, Public, Private, Msg} | Acc1];
		    ([{rsa_padding, rsa_x931_padding} | _], Acc1)
			    when Hash =:= md5
			    orelse Hash =:= ripemd160
			    orelse Hash =:= sha224 ->
			Acc1;
		    (Opt, Acc1) ->
                        case rsa_opt_is_supported(Opt, SupOpts) of
                            true ->
                                [{rsa, Hash, Public, Private, Msg, Opt} | Acc1];
                            false ->
                                Acc1
                        end
		end, Acc0, Opts);
	    false ->
		Acc0
	end
    end, [], Hashs).


gen_rsa_pub_priv_tests(Public, Private, Msg, OptsToTry) ->
    SupOpts = proplists:get_value(rsa_opts, crypto:supports(), []) --
        [rsa_x931_padding],
    lists:foldr(fun(Opt, Acc) ->
                        case rsa_opt_is_supported(Opt, SupOpts) of
                            true ->
                                [{rsa, Public, Private, Msg, Opt} | Acc];
                             false ->
                                Acc
                        end
                end, [], OptsToTry).


rsa_opt_is_supported([_|_]=Opt, Sup) ->
    lists:all(fun(O) -> rsa_opt_is_supported(O,Sup) end, Opt);
rsa_opt_is_supported({A,B}, Sup) ->
    rsa_opt_is_supported(A,Sup) orelse rsa_opt_is_supported(B,Sup);
rsa_opt_is_supported(Opt, Sup) ->
    lists:member(Opt, Sup).


rfc_1321_msgs() ->
    [<<"">>, 
     <<"a">>,
     <<"abc">>, 
     <<"message digest">>,
     <<"abcdefghijklmnopqrstuvwxyz">>,
     <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789">>,
     <<"12345678901234567890123456789012345678901234567890123456789012345678901234567890">>
    ].

rfc_1321_md4_digests() ->
    [hexstr2bin("31d6cfe0d16ae931b73c59d7e0c089c0"),
     hexstr2bin("bde52cb31de33e46245e05fbdbd6fb24"),
     hexstr2bin("a448017aaf21d8525fc10ae87aa6729d"),
     hexstr2bin("d9130a8164549fe818874806e1c7014b"),
     hexstr2bin("d79e1c308aa5bbcdeea8ed63df412da9"),
     hexstr2bin("043f8582f241db351ce627e153e7f0e4"),
     hexstr2bin("e33b4ddc9c38f2199c3e7b164fcc0536")].

rfc_1321_md5_digests() ->
    [hexstr2bin("d41d8cd98f00b204e9800998ecf8427e"),
     hexstr2bin("0cc175b9c0f1b6a831c399e269772661"),
     hexstr2bin("900150983cd24fb0d6963f7d28e17f72"),
     hexstr2bin("f96b697d7cb7938d525a2f31aaf161d0"),
     hexstr2bin("c3fcd3d76192e4007dfb496cca67e13b"),
     hexstr2bin("d174ab98d277d9f5a5611c2c9f419d9f"),
     hexstr2bin("57edf4a22be3c955ac49da2e2107b67a")].


%% BLAKE2 re-use SHA3 test vectors.
blake2_test_vectors(blake2b) ->
    {sha3_msgs(),
     [ <<186,128,165,63,152,28,77,13,106,39,151,182,159,18,246,233,76,33,47,20,104,90,196,183,75,18,187,111,219,255,162,209,125,135,197,57,42,171,121,45,194,82,213,222,69,51,204,149,24,211,138,168,219,241,146,90,185,35,134,237,212,0,153,35>>
     , <<120,106,2,247,66,1,89,3,198,198,253,133,37,82,210,114,145,47,71,64,225,88,71,97,138,134,226,23,247,31,84,25,210,94,16,49,175,238,88,83,19,137,100,68,147,78,176,75,144,58,104,91,20,72,183,85,213,111,112,26,254,155,226,206>>
     , <<114,133,255,62,139,215,104,214,155,230,43,59,241,135,101,163,37,145,127,169,116,74,194,245,130,162,8,80,188,43,17,65,237,27,62,69,40,89,90,204,144,119,43,223,45,55,220,138,71,19,11,68,243,58,2,232,115,14,90,216,225,102,232,136>>
     , <<206,116,26,197,147,15,227,70,129,17,117,197,34,123,183,191,205,71,244,38,18,250,228,108,8,9,81,79,158,14,58,17,238,23,115,40,113,71,205,234,238,223,245,7,9,170,113,99,65,254,101,36,15,74,214,119,125,107,250,249,114,110,94,82>>
     , <<152,251,62,251,114,6,253,25,235,246,155,111,49,44,247,182,78,59,148,219,225,161,113,7,145,57,117,167,147,241,119,225,208,119,96,157,127,186,54,60,187,160,13,5,247,170,78,79,168,113,93,100,40,16,76,10,117,100,59,15,243,253,62,175>>
     ]};
blake2_test_vectors(blake2s) ->
    {sha3_msgs(),
     [ <<80,140,94,140,50,124,20,226,225,167,43,163,78,235,69,47,55,69,139,32,158,214,58,41,77,153,155,76,134,103,89,130>>
     , <<105,33,122,48,121,144,128,148,225,17,33,208,66,53,74,124,31,85,182,72,44,161,165,30,27,37,13,253,30,208,238,249>>
     , <<111,77,245,17,106,111,51,46,218,177,217,225,14,232,125,246,85,123,234,182,37,157,118,99,243,188,213,114,44,19,241,137>>
     , <<53,141,210,237,7,128,212,5,78,118,203,111,58,91,206,40,65,232,226,245,71,67,29,77,9,219,33,182,109,148,31,199>>
     , <<190,192,192,230,205,229,182,122,203,115,184,31,121,166,122,64,121,174,28,96,218,201,210,102,26,241,142,159,139,80,223,165>>
     ]}.

blake2_hmac(Type) ->
    [{hmac, Type, hexstr2bin(K), hexstr2bin(D), H}
     || {{K, D}, H} <- lists:zip(blake2_hmac_key_data(), blake2_hmac_hmac(Type)) ].

blake2_hmac_key_data() ->
    [ {"0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b 0b0b0b0b",
       "4869205468657265"}
    , {"4a656665",
      "7768617420646f2079612077616e7420 666f72206e6f7468696e673f"}
    , {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaa",
      "dddddddddddddddddddddddddddddddd dddddddddddddddddddddddddddddddd dddddddddddddddddddddddddddddddd dddd"}
    , {"0102030405060708090a0b0c0d0e0f10 111213141516171819",
      "cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd cdcd"}
    , {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaa",
      "54657374205573696e67204c61726765 72205468616e20426c6f636b2d53697a 65204b6579202d2048617368204b6579 204669727374"}
    , {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaa",
      "54657374205573696e67204c61726765 72205468616e20426c6f636b2d53697a 65204b6579202d2048617368204b6579 204669727374"}
    , {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaa",
      "54686973206973206120746573742075 73696e672061206c6172676572207468 616e20626c6f636b2d73697a65206b65 7920616e642061206c61726765722074 68616e20626c6f636b2d73697a652064 6174612e20546865206b6579206e6565 647320746f2062652068617368656420 6265666f7265206265696e6720757365 642062792074686520484d414320616c 676f726974686d2e"}
    , {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaa",
      "54686973206973206120746573742075 73696e672061206c6172676572207468 616e20626c6f636b2d73697a65206b65 7920616e642061206c61726765722074 68616e20626c6f636b2d73697a652064 6174612e20546865206b6579206e6565 647320746f2062652068617368656420 6265666f7265206265696e6720757365 642062792074686520484d414320616c 676f726974686d2e"}
    ].

blake2_hmac_hmac(blake2b) ->
    [ <<53,138,106,24,73,36,137,79,195,75,238,86,128,238,223,87,216,74,55,187,56,131,47,40,142,59,39,220,99,169,140,200,201,30,118,218,71,107,80,139,198,178,212,8,162,72,133,116,82,144,110,74,32,180,140,107,75,85,210,223,15,225,221,36>>
    , <<111,248,132,248,221,194,166,88,107,60,152,164,205,110,189,241,78,193,2,4,182,113,0,115,235,88,101,173,227,122,38,67,184,128,124,19,53,209,7,236,219,159,254,174,182,130,140,70,37,186,23,44,102,55,158,252,210,34,194,222,17,114,122,180>>
    , <<244,59,198,44,122,153,53,60,59,44,96,232,239,36,251,189,66,233,84,120,102,220,156,91,228,237,198,244,167,212,188,10,198,32,194,198,0,52,208,64,240,219,175,134,249,233,205,120,145,160,149,89,94,237,85,226,169,150,33,95,12,21,192,24>>
    , <<229,219,182,222,47,238,66,161,202,160,110,78,123,132,206,64,143,250,92,74,157,226,99,46,202,118,156,222,136,117,1,76,114,208,114,15,234,245,63,118,230,161,128,53,127,82,141,123,244,132,250,58,20,232,204,31,15,59,173,167,23,180,52,145>>
    , <<165,75,41,67,178,162,2,39,212,28,164,108,9,69,175,9,188,31,174,251,47,73,137,76,35,174,188,85,127,183,156,72,137,220,167,68,8,220,134,80,134,102,122,237,238,74,49,133,197,58,73,200,11,129,76,76,88,19,234,12,139,56,168,248>>
    , <<180,214,140,139,182,82,151,170,52,132,168,110,29,51,183,138,70,159,33,234,170,158,212,218,159,236,145,218,71,23,34,61,44,15,163,134,170,47,209,241,255,207,89,23,178,103,84,96,53,237,48,238,164,178,19,162,133,148,211,211,169,179,140,170>>
    , <<171,52,121,128,166,75,94,130,93,209,14,125,50,253,67,160,26,142,109,234,38,122,185,173,125,145,53,36,82,102,24,146,83,17,175,188,176,196,149,25,203,235,221,112,149,64,168,215,37,251,145,26,194,174,233,178,163,170,67,215,150,18,51,147>>
    , <<97,220,242,140,166,12,169,92,130,89,147,39,171,215,169,161,152,111,242,219,211,199,73,69,198,227,35,186,203,76,159,26,94,103,82,93,20,186,141,98,36,177,98,229,102,23,21,37,83,3,69,169,178,86,8,178,125,251,163,180,146,115,213,6>>
    ];
blake2_hmac_hmac(blake2s) ->
    [ <<101,168,183,197,204,145,54,212,36,232,44,55,226,112,126,116,233,19,192,101,91,153,199,95,64,237,243,135,69,58,50,96>>
    , <<144,182,40,30,47,48,56,201,5,106,240,180,167,231,99,202,230,254,93,158,180,56,106,14,201,82,55,137,12,16,79,240>>
    , <<252,196,245,149,41,80,46,52,195,216,218,63,253,171,130,150,106,44,182,55,255,94,155,215,1,19,92,46,148,105,231,144>>
    , <<70,68,52,220,190,206,9,93,69,106,29,98,214,236,86,248,152,230,37,163,158,92,82,189,249,77,175,17,27,173,131,170>>
    , <<210,61,121,57,79,83,213,54,160,150,230,81,68,71,238,170,187,5,222,208,27,227,44,25,55,218,106,143,113,3,188,78>>
    , <<92,76,83,46,110,69,89,83,133,78,21,16,149,38,110,224,127,213,88,129,190,223,139,57,8,217,95,13,190,54,159,234>>
    , <<203,96,246,167,145,241,64,191,138,162,229,31,243,88,205,178,204,92,3,51,4,91,127,183,122,186,122,179,176,207,178,55>>
    , <<190,53,233,217,99,171,215,108,1,184,171,181,22,36,240,209,16,96,16,92,213,22,16,58,114,241,117,214,211,189,30,202>>
    ].

%%% https://www.di-mgt.com.au/sha_testvectors.html
sha3_msgs() ->
    ["abc",
     "",
     "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", % length 448 bits
     "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu", % length 896 bits
     lists:duplicate(1000000,$a)
    ].

sha3_test_vectors(sha3_224) ->
    {sha3_msgs(),
     [hexstr2bin("e642824c3f8cf24a d09234ee7d3c766f c9a3a5168d0c94ad 73b46fdf"),
      hexstr2bin("6b4e03423667dbb7 3b6e15454f0eb1ab d4597f9a1b078e3f 5b5a6bc7"),
      hexstr2bin("8a24108b154ada21 c9fd5574494479ba 5c7e7ab76ef264ea d0fcce33"),
      hexstr2bin("543e6868e1666c1a 643630df77367ae5 a62a85070a51c14c bf665cbc"),
      hexstr2bin("d69335b93325192e 516a912e6d19a15c b51c6ed5c15243e7 a7fd653c")
     ]
    };
sha3_test_vectors(sha3_256) ->
    {sha3_msgs(),
     [hexstr2bin("3a985da74fe225b2 045c172d6bd390bd 855f086e3e9d525b 46bfe24511431532"),
      hexstr2bin("a7ffc6f8bf1ed766 51c14756a061d662 f580ff4de43b49fa 82d80a4b80f8434a"),
      hexstr2bin("41c0dba2a9d62408 49100376a8235e2c 82e1b9998a999e21 db32dd97496d3376"),
      hexstr2bin("916f6061fe879741 ca6469b43971dfdb 28b1a32dc36cb325 4e812be27aad1d18"),
      hexstr2bin("5c8875ae474a3634 ba4fd55ec85bffd6 61f32aca75c6d699 d0cdcb6c115891c1")
     ]
    };
sha3_test_vectors(sha3_384) ->
    {sha3_msgs(),
     [hexstr2bin("ec01498288516fc9 26459f58e2c6ad8d f9b473cb0fc08c25 96da7cf0e49be4b2 98d88cea927ac7f5 39f1edf228376d25"),
      hexstr2bin("0c63a75b845e4f7d 01107d852e4c2485 c51a50aaaa94fc61 995e71bbee983a2a c3713831264adb47 fb6bd1e058d5f004"),
      hexstr2bin("991c665755eb3a4b 6bbdfb75c78a492e 8c56a22c5c4d7e42 9bfdbc32b9d4ad5a a04a1f076e62fea1 9eef51acd0657c22"),
      hexstr2bin("79407d3b5916b59c 3e30b09822974791 c313fb9ecc849e40 6f23592d04f625dc 8c709b98b43b3852 b337216179aa7fc7"),
      hexstr2bin("eee9e24d78c18553 37983451df97c8ad 9eedf256c6334f8e 948d252d5e0e7684 7aa0774ddb90a842 190d2c558b4b8340")
     ]
    };
sha3_test_vectors(sha3_512) ->
    {sha3_msgs(),
     [hexstr2bin("b751850b1a57168a 5693cd924b6b096e 08f621827444f70d 884f5d0240d2712e 10e116e9192af3c9 1a7ec57647e39340 57340b4cf408d5a5 6592f8274eec53f0"),
      hexstr2bin("a69f73cca23a9ac5 c8b567dc185a756e 97c982164fe25859 e0d1dcc1475c80a6 15b2123af1f5f94c 11e3e9402c3ac558 f500199d95b6d3e3 01758586281dcd26"),
      hexstr2bin("04a371e84ecfb5b8 b77cb48610fca818 2dd457ce6f326a0f d3d7ec2f1e91636d ee691fbe0c985302 ba1b0d8dc78c0863 46b533b49c030d99 a27daf1139d6e75e"),
      hexstr2bin("afebb2ef542e6579 c50cad06d2e578f9 f8dd6881d7dc824d 26360feebf18a4fa 73e3261122948efc fd492e74e82e2189 ed0fb440d187f382 270cb455f21dd185"),
      hexstr2bin("3c3a876da14034ab 60627c077bb98f7e 120a2a5370212dff b3385a18d4f38859 ed311d0a9d5141ce 9cc5c66ee689b266 a8aa18ace8282a0e 0db596c90b0a7b87")
     ]
    }.

%%% https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program/secure-hashing
sha3_shake128_test_vectors(shake128) ->
  {[%% SHAKE128 ShortMsg
   hexstr2bin(""),
   hexstr2bin("0e"),
   hexstr2bin("d9e8"),
   hexstr2bin("4c386d97ace346b2a06faab35663ce8a4c54c295b5b9f6161efafce451ca8f617ab7d5ab88ffe117d6a67cdb0bc5250a3f2556c65f0c09b1d2577ba45cc930a443a33711b175af215a338a8d5e8b918a7176a8fb390e54e5f79f7a236a006a5bf1241b30efecb8b9733f5c32195d1bf22b70419d0c65de9bd7f982c94317456eca610a700a0d05c86bf27b3302e2c92ab53ba815a0b9afbcb88e1afe"),
   hexstr2bin("5d8f84b2f208b58a68e88ce8efb543a8404f0ec0c9805c760ad359d13faab84d3f8bb1d2a4bb45e72c0ec9245ffda2e572f94e466cffa44b876d5c5ed914d1ff338e06b74ad1e74d1405d23d759356661b7f3b03a7f7c2dc0d2c2dbe3d60822803408d472b752424ea76af1d79a0e7920388dde0c1903e9364b8d6d7b3b75430754b4d6b91cd83d5740866aab34bdbd0f1bd3dc504f1a1d753ba5f938241ce7f52544e0cc2018cc67b6401ce6abdbc8aafc5629bb643730fa3daced8f425787d61069910073ac760c631876fe81d1127034a544820ad3aa51cbf2d904f8cda936c063561a8a0bd0b1f1801777394630fb6f11cb68a588000861283a2dc9d7d2739ff2ae5ed5af5304cc176cd544a39a99064c1cb3b6bcc88a97ad9f6e381e8a3929781861e91f73516d3ee59d3661b5f584b4b717d0fa7a54da03674ac5fa36d3d76412a826c4c8445f7720337119198"),
   %% SHAKE128 LongMsg
   hexstr2bin("a6fe00064257aa318b621c5eb311d32bb8004c2fa1a969d205d71762cc5d2e633907992629d1b69d9557ff6d5e8deb454ab00f6e497c89a4fea09e257a6fa2074bd818ceb5981b3e3faefd6e720f2d1edd9c5e4a5c51e5009abf636ed5bca53fe159c8287014a1bd904f5c8a7501625f79ac81eb618f478ce21cae6664acffb30572f059e1ad0fc2912264e8f1ca52af26c8bf78e09d75f3dd9fc734afa8770abe0bd78c90cc2ff448105fb16dd2c5b7edd8611a62e537db9331f5023e16d6ec150cc6e706d7c7fcbfff930c7281831fd5c4aff86ece57ed0db882f59a5fe403105d0592ca38a081fed84922873f538ee774f13b8cc09bd0521db4374aec69f4bae6dcb66455822c0b84c91a3474ffac2ad06f0a4423cd2c6a49d4f0d6242d6a1890937b5d9835a5f0ea5b1d01884d22a6c1718e1f60b3ab5e232947c76ef70b344171083c688093b5f1475377e3069863"),
   hexstr2bin("c747ee72b00680920ef1f2e16d32567e7d8900b76749832a61a3dc2a8126ec65ce872cc39d9e9c911bb2fcde26f63f2561533d2d8768a82effd04135b1a34bc9193f40cf9948c4ab14edd7a8b86d4e9a1bda84c28a1e4df19e3b2bf00fd7c520cdf7945e4d0be029212beb07cca06c24d4a63e3d19705abc646d032543ca40b015388f3861efd8536cf2a0c30a957f0db43dd467db68b7d77ced2f9af584a5c3178899247cfc94711acd5ea31956709be229ac08790d4c8a0c431918cc0edd9ffb721aeb463bfccb56418d70a2eb281787f230e89a67011d3df82766706fcf8cea7b7a5569b3f9a946d895f24c5a8ec9ca1ec7cee77fbfd73693c34e8aaf62d40c2d222a8a6c71521219def1c0ad89f8d437d883075c6c02209c5496c2d780be880f7832546f4e0fe38cd035a5ec03edccc5cdd0a306f2e4229b4207bfed109186a9e5356cc37628e1bad485a41de3f1db4e0d7b638637c56519f4692afcf9963084d2fa7d70d3e5332a50ab654a114a295c004ceee325ef109ed484aa092515ac27a54a2d7f4b8d0264ae6d8f7a7440dd760e0ded25a3a94cb0491fe81e7b55221ac8ed24f7c86a75f15494cab6f61d20259f840755983a41d9bb048b42e3a0bbe6a1ab0be031c2b558ace8c0057e2719ef1b7ae57aeeeb16a092f93c7e453c9e129a4c8a436edee9211c30166ff5a8f15feab82f9c8c04f48d37c7a3d98972f5261997482766063ad50e3677fc41e6cffa74038cc35acd02291036675b988ae5ab8855ee940382d0fbe9c089b54b2adc4a2226a4f8408d69fb860d310c58cc98284e4e23922c99d21b1597f4a1fbbb3fd65ae72042268e508c0216a8f5b2803625d34a902ac99468befa8361c83ae321268510a8aaf416e062ea17751d8eede62a7f11dd63c3bbbc6b004b7833eee8aac30c67e337926e4007a7d93f8cd55cd1c0ba6ab1460056f3aa8fb9e0e90013013455b57af1e8de9a03a224af4b6d151f6eec1fb4d969ad38030f9342f259ac4f01cac114e0dfad13dc42bf6d614060f7bd7df68f26e608b1ffe8a24557dffbbbb3d2555a0d5d25d51cee7420d635858134d5b8ea3560ae862582ae921f7c476ed1d437f47f4b8c084a100970b333cbe75b5c2d6c0cbde8e0fd457fd05217da90e33a30893e922bd3ef0f4707ff247c4eba7b1cbeb5e7fee7109adca8f998fa48716b08c153184a5f9253f68ac2efdee0da984e8838c438c2e5e78ec21be6f0be04cf8746c5c731d4bb44dea4047c3ac5421d8c719b3edd9a05955ec5c0eb041b23ce75bd9c862759f34ac6fbceb3d7257a709c21e83c3796f60d115af08e7d8757d099d1e13c7516c0cd7837277cff270bfb464127a58cdf42b67228b4b7a529273dbc289bd36434ad35ba0118bff8f37c2454795e63259c70f748d8134ec22a1b7da66ad2323adb3fea5501a8640bb255558c70975d4af6093a5a11163c1db1bbf81f408e413f32005411dedf53808c1ff5b8cfd97b431594e6717cc467fee79017ef52d8e8bd6277dbb540c5749fa8606c9e3d2da55720996355c025b8acf42140dd47b70ffa922ce80bd163afaf2fa321076399c3cf3fa118ce86411771fff6dcb3c7349e3feba8e1d936c0edfa486d7ebae828bdf39c336fa476216d93309854377c567dd4957230e81ae414c61c48ea8176df7b0b5cd7cf9e37cca05bb08e0e320b259501b7123b0023d80e5cd3bbcd22b2c8e65faa15a086724a9a11a760746b1fa9cc0e7a116ce8f9a3c104944f906646eae437ac764b22cea441fd3d97edac5eeb0b4205f62a6a454d1706878a9915da4d5eda8a81363bf4e17f6798bae0bb70f23be78149dc4cafdff7a9d5f751f715fa3d916dbfca988b093b96e351b761fec642431593ec57de604836a61b20d3a0f867a47db66f033c255dd344e5d966b0700a9eea5ac1537bd8f4f521806f73224fc65be1f08e42af3bcc66e3fdae922df779422729c3082d50e22e6fada49d440841fdc2f58d1e8b567297e9219ead1e637976f5855a3e9dcefa29b72a291aae2abfd4474bd65f797b36ae7ed9117e2abdf1817cf8eebc735d242d228064b207bd2a26997ccfcd20b400f2c9ae6fcefa0cc8ace309e74b56024884d836bf2cf8ca693cdf10e22f2908b8acf32720653558e83f5341adb3eec74e7400d4a0522857645e428d764f3b80472423cd8c0b84a93882f326a8bce4b31e46c3e271ea406f03d4ea7df2f3ff2293a282261c888f08a0bcec42412de0cea14248e999bde7172ee5fe67c0e41cf8856e38bc36a3fd7c0837651dded08921bdafb8718093c659cddaa6d447ea3b0a05cf6c174425770e5839873de96b88b35bd73a008e34843cc8fb0c4befcc045efc0f47bdacd7a4c5780b792cc8c7848d6400f7202b67cac80790555420541efe5074f038d87e2da951f56472e5ac26b5be731658bff5400672e1ac77424832f2eb478f102ee5b9c2c93e7095a6e216355378c030b2a78dfdf236373dbd71e740b3cf4ecb18d3f119c930c41f7530fb1509561d316c435ab66dd61e2dfa97e6f2925653f3fc0f7cb53a118998b95f7e5bca5754e256914e37cc0a8ad05f82d7106f22c60f4462049e022ae686d93746117dc18eccd65b128af81aa3ac55f960a0317cd0a857942edd0bc304733f80fc3e0addf27555674db7d84e904b45c62cacc2ac99459d6da4793b240be6520247eb58f3a9673c7ea8c453ed7a3595afe13b09fa486e7eb50e7a7c9422687a85d01f514bda2af168cbbb534188f0af7d2856eeae05ac623313e9b55d71801fd03ee2dc2cae44deed5da3bf90e306f9f8a804590ffa85b1b05ccdcec521b6a9ac3a3ce1e04d3f69d507857e23b2ffdde70a4d32664c83d3f4912eae155a9281e16c3649addea3fce770687a421e61c113f53961fb262e31c759143f94439d0d377bd7fadc89f6feec40549a354fb60f4512d1cf16ae0d31f15c88688c055eb6761028ddb478c2d7ad2152c6436c852e43091c62ff0ca83c3943911143c75de465d22fda7fd08eeeefb968e6b59d5786d5812203fd6987bb68b6382202b22300cd6af6da83bce3ad7db0a1e3d30cb37fc522ec9b5aed3314465c848411ee3770767f85c30a62487b1eaf02a3e5de2181da9581105cbf33b19bde7d1db8a2219e28bae27f5aea45c0995e950f333e29f4db82ea4c4c080ff82fe32bd63ff76af0abc317f4bb7d62e5b764610f0552544091902d3572ef5053ca149798ed1499d0c6412c0c77432850406a6e8ca24229cdc46854b5159d7d293319c8490c2136a543dc27add5631e6afc17c56c533731043bb266b37e391ee13399c6d2e248cad7e83d12c11c05cd9bb2146d996059f24bc0156c16f6ecbbe5923d9a1cb8afdcce7e0cf42f9553bb4c2c086ef911cff60b19396dc8c3dedd9c18ac4b0ed57ef4f1ccf0d6df887272e3309cd176adc2c5ad0fc771b02d360b6e8c8ee18f83de9aaa9c92bfb93c8d7aca87789df77e59e5976a6781b67e1dcec3b88d658908f75761b49ab7a5fbca5967f943da63cabde033f2c0a3c2fcbaad03624fdee3adf0b27cddafc78b0b5199244da590a4da153ee86f7be1bcf1363096537c5485baf13464df79f553b54bc1684d48c8a1f68a851da83f438ca3b8dfe553bdcf577724638182a24be541e05a9abfd18a4266443f3c66b5d01b5b6943562aba58402bafca3b8ecfd129f737eb246cebdc6f716af3ac7185904f6070e3529f1af62b20baeefbcaaa8e081f79fc6d1c3cc81060242ad591568b11fcf647427b85b2fd9c6e5927ffdb8a6800cd16dd966c550d7b9c6b1f8ed0ca4a7523f88c86a5939fe8f29229f50acd33e35cc0dca4248b4e0af73ef22f9c54e0141e7d9a881fa1f94c6f216774de604f104e82d20c0c73bc55602bb63d6231bd21105ed2499ee4f30f0d8f4b44ff4aa6cfcae42797cb143da4fdbbd381c96ac7e420249e90c865767bf3623b2c16931e68f86f430d0a77ba3ef675b1c07a06e8a477566527452b68d64a25a1f6cb44a12201aa8f3670a21c6c74e2b8c51d0d07ab3c7c8ed4be3eb369395de468c56316470d1d71daa69f49769f88515759880b3f981f8360dba73d7ce9946c4badfb641d435152c053762ba49ef1e23dd67edea9b3c6ab93436df0578251a1662a1175edcccd9bb1202da93d9aa5d5403df76731b4f96e349f98b2412b1deb83e5af30fed019b275b4ceaa8bba2a902836fb8e1ec440340d96316d840c7fb26885dc7d9a76be01d4b96018d4270a7dbd68933bba0a2465e4d13e08664eff5edeb3a14407e643ee96d4ecda6108cd02c67cc6564baf2c6982f7ca6b30192557ad17056d8e4930e382af79d87a86f9890d199dc71adae1a9afb1da7e29947a059d5810e177c70e4761f20ffa67c9187958e81e766fb4c98b32a778299be885d1761fca860a876482fb45b6286ff8b13a7091534c92656e78d580ab54cc3cce21ff133c458fb2bafc184e774adb3b4d341dbb442fca1458d95ee57ce705b9de5e01dd6d074d6f991c80b85948f3bc73cf260ef80860e90d14967df1ca8e873cf1452465b28de7874cfa29650affa0ab73553477c09c0d179345f115e5afcd704090912d2f97410ef45049a27847127e8870d2c93dc6a8febc33e8ed20b0b403b5adcccc67b3a6b67a48c2187b133872764093f76926e148f9270b8364d8637cb7627c4a41293e0647ab39cff9aa3a11a58d1c072b04e17fb1eb02de97710263ad7949b0a4403086c34059d0db5541e37d1f18046aa7d02a4b457406a3c1bb4101b8dd057a41a5be56d1c7220a5c756f67bd2df31058d5d231fe09046e11b49d1ae8dd51aa74c43769242ecd6d07fd3dc93103de045f18fecade6adfe2a815ff60d1f49c1c21b678620334daeb9d01d50c4143cc2e04c2367eac3f9fe6d007ef3b3ac8efb890a209ab3481506e68abc099b695c6768ffb03b5322f4d61cfb2f613ec01516609b173aa7fbe37ddae2185d757a0d3c447eaebd736e3ef3dee3d9b50b048cb0ed30b3dc9ae93971fb88bac2dee0dfb0075b8784c8103906175d0675a4c45649837d7ef6f87a7791ece12bd0541834612618c327ddb3635b5ac95e19a889f24ec7d3e1203d4a3e4c138de41c61aabbde2538fa0d00b24e821dab191762efe13f2ed672def5b2ee1f03d16c0d73f2d675993e94aaedb76b08d494fff57040c3a93ce7ac89ae5f1779326e2c0a278b04d5d68c1b88011d496e13f95b4a6d46ea0b56aa16b6c3994ae933b6bcb8e8fdc75d71c3f73ac76e5a00505ab37c964cf4d4107029fcd21be8ed6c4d968034a5be9b0f58d029cd252d8e1e3ba102a7cd54a20eec5eb25631720450954206973d2b1ce3cd0edb6edc2465e20eac82fe6289e386713d8dcc8486bc852037929d0ecd31b3833bd648edef1620662e64269715173b914ed5a18f97748e60efda92a4f597e33dbf90d7f4ee1d6db4bf2cacc586cb82eefb2081bd14dab0e9e345248a34ade73f3291886b91ea3e8cc742fd884f6ee0ccdaf4c9879f4db12dba58cf491af2541a1d5ef6cc8b1af750ef5d8559ef7ff9cd56d8f599974be3aecd8c0f4c08f3ae50d86f9f822a1e4ca39fd2f0b4d78d22630733a24d8d63ecdf9555411daf205a761c39ef46ff6292e74129bc13a7f4ff20549b3c2de2142152ca5b21384e16ecfc215462659b865ebe7be84410d175e477d7864fb545db158ab4f40c268625da45a03595886f08247bb8852516d45e76fc087b396e29375df9edb68e62e80ba6f51153f509ea6678b18f86eba0213d2b0f5fe0a5d2fca75f22ed191467e406d78cfd361d74ae0fcfe25973e43758807534466d016e40a99d982ba22abfeac5e07fbc625b432a829ea0bec2bb66eac6f9fd6456d8aa85bc9dbc41e85dd59cb7664fd75f4b1fd05d695f1809eda9488f0ded326498124c6ed41198321a435a59bd7fe6837945300eb4800d9fd7690237356638eccc30052ed54ab735ec83b43b6a726285426de9f504b6081b1530986e151ac075e17e308b159a275a41012acb01d9c3cb52f43eceee638002339a8b72015d918674675ee229b26125b4562e86f81892cc0772defab61b0dcbaef15c01aea104a4f195f0f26615dd0eb2fd500dd1b1481dfc542af7003f29fdd3ff7a8b51a79237e554970a6f2723abd9611fbe13266c279e22a34261623d0f6eff778dbec3e2d5cc5da1b8b978bd53742bee1620fa677f5978e86b3e06ef86cc8bc3773a784f05230bb17bb98d12bc2fb85bb4afbfd29bdaa16ae01aaf99ea792efed659476f3c65d946c203d6bc3c7880e90803c935f38e369b3f6bbec4a0f1537a399fb39be21150c23a1ce8125acb20068ec1a8fa1ce0c309d7c5d70850c183d60f624f8d9cc20cb4225923ddadb41034b76df1977e7eb12747ad6add50665a82c6656d9cd2d467ad496d2c569bafdc14206d15b6aeb4ab841d76fcac565c2210fd1da0420ae4fc1dedbd6415ab3addf54843a7a0142efc90c7d70efda91f850121df9a25db602476adce6da362c1b088ad860a1c555a2eedb5321469ef2cfdc3c33f8db09ac28b57f814ab7838f0de2eaea8cf63c8a2dcfb841c9f498f00e6e96fb22d6aefa89b226b24902cc434c157fa205a1f04a5713aa2fb22450f47fad265dbebcd307ede766572ce096b0a04a4ee915cb69fee763ba0522fc4f64e842f64213b4013b1cfb47583c7b6b3ed02961a5029c2d0ca666eeab62a41caa12d40e906ae7bffedd97d19dbefbd255a659bfc0153bc6de5cc5500aba453f2cc50c74310b7d95b8fa630d0efd88545de883702096d1bcdad027e80871074154c892320b3e9d0bd139060294404878be5f4e7dafcd85956acc35bc8ed111a989e50d50084b6faab16329b815ce66c6e0d363ed530180dbf51697d785208cff32e225d40f791cf9c14073387af008c62c0c69c8f67ce915e377f43ed3a79fccb98883c80712cc8167978d89acee570108d109167f84e9cc9bb7f4fb62ae7396859fcf33da5ca6c80c311eb392107afeddebebe0d662a887879e4014187d2fe8feefb01e6fa0d35819d7cfbf139e99451423b62ee35e4dce0ef48094df5bfc478c3fcf90ae257bb9f44f48b419dfde5f133f12068ebf012998805b809e92023d9759d4917f35c4710761309a8b1c6fd34407a972bfcfeb218d0b7ae23d68685187036629ca16d90ab9d7307a509d74290e4520795ed4343d565d85c1a326570cc4062d617711e00c7178ef5b52aa16ead8bf222353170306593e2326ba13f6f4b62ad3406e6e02fd990b1645788b7c9d0c3e557986e08103ff76fc1869796e93c636fdcf9875666798594c40aa87d0ca118a6182df77d5bcb0ccb99f989ee6715af45515db6e35d1d62a3a55d0e737f94f6bef47472329b9d50a02c32fcb8714dc2a831a20a096c4bb45878bd3f4dbb32ea9a7cea42da283d4f67e8d17487906b2a12fe65054b2c7af74dceb99f3e47f160b3021e0dd6aa11759d604365de3cf4621008251d2b0621a7de17b0b2f7ae66f5b3348e17ba99b04fab2317552edf5fe729ef50d62b91d64293d4511efc36fa15f5749914f86fa05a3b81abc6ec102445bb2476a4cf581f5409951947aeaa214e0452ed6a12a44811a93c9f4fb847e7fa5e48052d78b15358a9ca57a19150730a5a4ff9ab8bd04e028e946033f61ef1618dba9ac4ca4618dcfef4bad146d275e781dc2c4dd66bf89956397d3837edf737c40ca60f367ee63d6394b13b31989041f58813c584dffe629a8c64b0e03d5fd99bae95514fd43206823ab963e41b58c63fc43ea719b4b5eac61db522a195a1c37f21075f78de92d4d55751dfc0f345c0cb111b670782c59e535dcd21de8f5381d8f46a7e44066f04d76b18f36a98ddee221f2889c0b293f9dcdd3b230972eb9143a94d3223e44d32f2e65d634374878adf2ee483e7dca799cd0c7daab4e8d08e49b00e1faae0a3dbd279bd93ee2ee9434cc0dde9ef5976daae7c131d6379b8b555995bab4b84196bd065a809cd1069940585832049c19848fbac8a1f877dfab2e1d584498867045f876abb3952e18ef993dd62cb89ad72612f0e4125b39e1d0398e5368ed3d1fb4361d945a721eacfe07248fb5011659bcfd311d6a45a32a633c558d0ffd08d74a3fa3205d7ccb4b2d52b58cb3293828ac16277c92ffdd41ca00190e1f0e0d1e6fdab154a4f85d7ce64197d410762acb6026780872d110ae6f4d83df8269747f8bd1ef3a1831fcc8919d736fb23111ca3ef4cccaf20264fab8eb3b071e56667f06ea04d99f6eceb6942ef06683a118aeb04accc8fbdc00ac0fa1e88edd1c0ba849ee993922f6fece58bcb7c0691064fe1dee1f0c73a50d4cdb38327da5058a81baa455638a12b298450f6f854ec42042eda96e9e44a6b2638479c3db60570b92c2d6e15ebd0a139dc4eec1be190672038743618454271878722940b0f55bb864ca4ae432a52648643b235572b21e82c755849fc88d7066130c1354044c59b77e38c907e829ae79751374d2a901547919be34d172d5c8c5bcbcfea5e04d993068751bf28730e9d059b5aae16bedf07f7e2a1a9e8816795dfceab1db4be0624cabea084c17a4207297878d29c16039bb8001ffaf1437c9555afcf59f4eedfe32cefe58bda16c9b8ab7f3e453b2427856a6acabab1d2001d65f5b9299f05cb30c5918d2ef2b70a23e426079064779f6787891e63bacb96807916f50703915caffd5a24087c0987db6bf3612eb87eef8477f197807c8ea48117ba0f7cc31008a35c903683065cc731bb6dc78a97a89c9ee42ac28b6799be71ba2574e57e1063dd6bda3129024f4cd95afbc73e92833b0934ab3bff1c7b48f4e90fa1eb6500d290939ea084f04f8d44b333dca539ad2f45f1d94065fbb1d86d2ccf32f9486fe98f7c64011160ec0cd66c9c7478ed74fde7945b9c2a95cbe14cedea849978cc2d0c8eb0df48d4834030dfac2b043e793b6094a88be76b37f836a4f833467693f1aa331b97a5bbc3dbd694d96ce19d385c439b26bc16fc64919d0a5eab7ad255fbdb01fac6b2872c142a24aac69b9a20c4f2f07c9923c9f0220256b479c11c90903193d4e8f9e70a9dbdf796a49ca5c12a113d00afa844694de942601a93a5c2532031308ad63c0ded048633935f50a7e000e9695c1efc1e59c426080a7d1e69a93982a408f1f6a4769078f82f6e2b238b548e0d4af271adfa15aa02c5d7d70526e00095ffb7b74cbee4185ab54385f2707e8362e8bd1596937026f6d95e700340b6338ceba1ee854a621ce1e17a016354016200b1f98846aa46254ab15b7a128b1e840f494b2cdc9daccf14107c1e149a7fc27d33121a5cc31a4d74ea6945816a9b7a83850dc2c11d26d767eec44c74b83bfd2ef8a17c37626ed80be10262fe63cf9f804b8460c16d62ae63c8dd0d1241d8aaac5f220e750cb68d8631b162d80afd6b9bf929875bf2e2bc8e2b30e05babd8336be31e41842673a66a68f0c5acd4d7572d0a77970f42199a4da26a56df6aad2fe420e0d5e34448eb2ed33afbfb35dffaba1bf92039df89c038bae3e11c02ea08aba5240c10ea88a45a1d0a8631b269bec99a28b39a3fc5b6b5d1381f7018f15638cc5274ab8dc56a62b2e9e4feef172be20170b17ec72ff67b81c15299f165810222f6a001a281b5df1153a891206aca89ee7baa761a5af7c0493a3af840b9219e358b1ec1dd301f35d4d241b71ad70337bda42f0eadc9434a93ed28f96b6ea073608a314a7272fefd69d030cf22ee6e520b848fa705ed6160fe54bd3bf5e89608506e882a16aced9c3cf80657cd03749f34977ced9749caa9f52b683e64d96af371b293ef4e5053a8ea9422df9dd8be45d5574730f660e79bf4cbaa5f3c93a79b40f0e4e86e0fd999ef4f26c509b0940c7a3eaf1f87c560ad89aff43cd1b9d4863aa3ebc41a3dd7e5b77372b6953dae497fc7f517efe99e553052e645e8be6a3aeb362900c75ce712dfcba712c4c25583728db9a883302939655ef118d603e13fcf421d0cea0f8fb7c49224681d013250defa7d4fd64b69b0b52e95142e4cc1fb6332486716a82a3b02818b25025ccd283198b07c7d9e08519c3c52c655db94f423912b9dc1c95f2315e44be819477e7ff6d2e3ccddaa6da27722aaadf142c2b09ce9472f7fd586f68b64d71fc653decebb4397bf7af30219f25c1d496514e3c73b952b8aa57f4a2bbf7dcd4a9e0456aaeb653ca2d9fa7e2e8a532b1735c4609e9c4f393dd70901393e898ed704db8e9b03b253357f333a66aba24495e7c3d1ad1b5200b7892554b59532ac63af3bdef590b57bd5df4fbf38d2b3fa540fa5bf89455802963036bd173fe3967ed1b7d0611ed907027e72854b9c22cccff1ec72a9b6cbcf4870bf92d4bb16ed6730d1fb540ab78559e17e14e564593c68530aa384dc27b75caf427e84736711db211cc854e0bb873a8cb4f6bcc7d1b9cb8782bab111e66b215041f89d154e944946facfca3d6f8d32e4a9a3283326d55823e730a514a24b55185069850d27e51006ee6a4a7d3df7b19dde92be42c31f9cac9a9dd740c5e8664bd079d2c79d94edd57a305c3cc16754fcc64df55320e917862f6ade22dbc2b38f54721cc11a24215720aa55d8745c9ba5f35d53f0eac24735182000275e5aeb374f1ac9a6280368f07440edfce137d41de81950b86b4baa5de74613eab8216bf842bc4fd331c31335e78c8af67289bcb9073a81f63ce5bf551f555fd993a6c436b1df35d4d1e3be9bfcf59371a2d2a42be3095e56ca1104bf12c66a805b6b72672f1c023792d5c9971714bddb1ca338db0e3e4c36bd6e08f07421ba5b89eff52e1ccb3b57df19d2bbd85260bf2a6bd94aeb21e232614d4a9db5efc5d7a994dae00d8557c21140587eb337028a76ad03f00c5ca6c6704f382b5a99238124fa822163d2de02f3b9d9643a5b39c96bd7476ed457006f44e4500dd4b649f4a6918966d23c44a6d106c3e0c0076ce8848b221be5a99599599d20c9a6bfbf8e88fa5f24bb1a020a53b7a68906f422844b39128bc2bcccd148da38f3631ddacb568581ef90f4bcb9d64cffd15189e1194a51a732ce191ca38d3bf6ff7c69424799d3803bf664db41e3274e3e42312f13ec654ea59a370b8c8132e962652a8c7fcd54d87e4de2c42ef4d700445b926310bbf47768a9af2ff8df4f9ea0afc9ffc05b9a093b29e72dcda5ecb467f026337c338fbfd06ee9e074f1f2755eded032d5ef138106fcfa6eca50d8d93dc25506c2ed8796816c6fc38d53cc0be154d5bc1cd241ee4649851d91901bfd2a24a76381348c8a0024a95fbe9f2cd0ccb4330201bfa58f9d922fa0bfee369eb1a9b5b043c30f08c12a4f0b651e9542957f709b2101df035169429ea1c5a91c58c3d89366f36fa138ef996d3d6ca60fc01926ede7332ed82621767de3a0f414a405b88387505ed1fbd740db8403df66d113ce70f0ee458c707d33a4f22a980617d1bde701643d197e83adfcf57e92064ce1cfdaaa7d838ec4bef2da59ad157f28da515481999ae625776937ee377e5c9825b371807b995d87794c5486b2fcce6b622b52648ae8d6a524eb22fab8e4e8473201ec1f62d7875d2e98ca05d6aea2341ba4474692b759621eac34978d7617ee994380d3bdce8aaa6170916372bb69276944c9945b76fce0b734e3412fa289d2edf1065a070ed5f3945c410c3193a1b9cd954fda4fb82fa5911dc57419eabca89282b2feb8e8f7cd59c46f7c7dd7da7fcb0e5084e9eb60354ebf41ccd6bf1ee419713d4e5e0d7eac04416340c1e871cdf4286220a4913cf412a1eea7d0b02ef9f564fbd560ecc8f4264bc2d5a8561648bb7bfb3a1a4a2476bf5e22a1a23e6b7eb8a675106c07654a524c14c78ba126549361823dc566a531975c9426c5ea52c6d5ee3f7bc9dde212fadacf83064a20960e71ada810873dc92d25a9b3b6148386cf84c19424d0bdf2a270598482b1a361af34c415861c6c81f331e9d4fd7d7c232802a1e5e790574dcba559689b981d6bd8236be2b0a6a0108c09bb0706658b1051f036ed9c9d5ca8e8c2259bbb3dfd092bb61bac0aa676d95a35b3d65b6d5ef2efc61717ae482c887c8f749a6c43c0ce1c7549ee0eac6f60e01e6aaf7705563f441cb8f0b731455119459ce1192a172f2ce97d053728e83ba05c261ee3ce1f5a113ca30521d945b7218d250c6c515ea0cf7b3bae1e9c7fbe2d479c812c1808d7c9ce5bc198b09c1f37ee9378b06a15c786a2fc252f87a48ce9993492aa83a13cc04a49b26647b6fdceb27eb29fd4cb07f5093526d46ae51c67a964b0098176fda738f354151f34c758912d468511ece67d168d1fb7c6bfe129f0c365a3a208aee102ff234fa77420580345b0faa1f9e781435a7c441b937f9317c7101365c4a4f030b3bc6061238d6f815e6e8b25809fc42823b903ea1e4c4586f66bd33d4984775ab76be1ec9172e183bdfb90ed0f53982d8f7271a2aedbc3b6c7c55d98e5d1c8e980de5e9a6c5560830ac4b698f4c78fdbd42517e5d3a2603fb1b93d2947f177559660310b02b96f17cb766699ef401dabc84d51240b44dd7a4ab3dc3eee8805376ebe03aef3135fba566d1b604d71c425a267c7dde4178e0f0941c57a27e9f4f780243106cc07d4d352356f9c9db115d218e0e683ccaee46663ff834ba2bbb92e615f5392ebeb410b455bbdaf6ea27529825537a6438f8308f1299023dd51e43488bbf5e3d15236ba863c90092eba84a4753125c5d71dec6f176c66a34660e8b27cc6727a2ab12879bb74814a5c5da6cb78115b52f5fe83216d44913a6c5d4920940b53b646ac98b14b800bd386206e16f15222ee3e95e998692e8a79bdab87f794f73438b388fa09cceb9a451a973d0f779026f7ff68726c71a6ccb1c8d017afc6167e620ffaeeeba613a71fb826b62d8137d1f0e70acfe63d29c292fc6f9ad1cde5acd2aa66c9cf6d94ccb3bd534bf994d5ae6953e154ce2244269df6de63e602e15a0b11512d3d89fb75a1b8a170ec28e636e1e480a36ca772433316a115550b95d06be34e7bae91943c7a28a5c78dc3d5e5cb076d3e4a5320c2d2fa5c21b75e7fda757eb357ab0b0b7bad8bb4b246ec4145b54b0b77ffabfc002e00741b75621bb2c8722423ab5ccdddac6062c3a008a4d1111deff2f528508584c76d5527ccd613d53c1582381d67bd0735346e41af7d026efe8f41b34ecaed87cdbc7146d55b581c5b0bd5d9b345a43e88be24845736743f6b6db6c285c7829771f5f24b31f80a1e4d7b90f8b5042dc2b94488e0866d8102f30ff49b63d30f34528e5abd6958944b81e9661da91fb434658c0dfec38ec34a13fa7db67f79ce0efe0a1d05c1d9a134a6e9a7c0f5e4c1f40be926378516b5854c91a4ac2f285967c2edbbccf2179b6a1ad0f88b221fe1922ad0541341a1d2351b7a1bd5097b2bdf4f9da96b608efd4c5c53bac4c41d9d8f30f62a2a8f9ca3e9ff2845c974cedfe602be7879a3ac415bf9c67116426a505bbcbf1ad5d57395046610b50217afc01c7ab4ab7021fc023961cdadf82382a4783c9fa8bda9d33024feb7ec1768aa86d31614b4d761faba3522cde3d2edf304752f1c563a73180fcc17a24d61ce2b9eaa63532727ea24c40fcb3b953f1837eac302b9a5e4d7de8e5a4f546b89decf7222f0f464da3106ca09b9bb44a528cdd1f255fc0a453e7336501432c8f810f2abad3d7d0076cd1d36b37cb174ada6215483a75805c41d92826df4e1e824eb2195d9b7518c7624d494ad92b2a7a0f6deece765522aea0ba90cd46fd64b98e3f8db18dd166b60bec96e487a335037b15305ae2e7eec3f6f233053e38455dc81d4e5eb4d402fcf21781e34b1b7f0da6c3a29384142bc3cee9d974d0e70a9685b4240d156433c09c34d4ec2d7b51bf17dd0d2266c223d6ad064ac203f3fc8b4c6cd766ba6a432a93ad55aa34abf34156cd39ae90da1c221db34d98e0426065a5ad2e10aacf78635ee8cdc26893adbcebf9eee008455fd2a558ac9e6dbd814b18dff2b4ed791b93854f03dfd11cc7585fcbf460d8e9340cce5b2d7bb44ca6662e6aebf98c5273e19048c1a7782c883c38a912d508b0e2f81e2599d7bdd816de6eb6e1888e41ca76083dcdc264ac399101fd12e79e81b3c01f1eb75c8e471ada612b9c08fb2d667a7595f0d99d85227e3f2eba3fa443ed8959ae51b03183fa828d32e322db0090145db8f2942c9d8b2c7ca4d38521e0d97e533e2d7bae72feff7c31807e37a1906c4a0e9b9a18debcc23fc724611e3fbfc4953415660bfe6e79b69239c316f8aeabc3ffb394692ffc6911ff7779a248e786a7face18520e9117c1bbd79981ed32f951f301292877954979b49278b5e398ea12f6233a8b89b002271622aa7f68637d7a524e025ca0cd89ff4e3e045ff81c185d92db74bc5913627009e19b18518b60c98462c60d91ed522b3543c5a96fb9391035bd6def65025e22fede175490f0c9c3f1051e43323ccb86d169659a30b875b34693ce2b95c51ec75cdfc43c0ddae1f201ffb2076b041d978b78893b2461f3cf2f233e79d1a3cbe68bad4ad7b193d0dd1d172960248b6fe6c937db9231fbdceb573666e001e68d08f310522e6262bd0cab172c57451beafcf46f03aa40d057902693ee680dd48bb4fccf8447098ed00815c562f3b6fab56075e8a9883a268d4504047d19ddb6ee720eb4872c8f603358c1e9743c5f70fa9ab26a74360546a692594fbbfc5ad3ddca918504e5da200b84194d093a29c7de3315bd0cbb8b78c9ff8d3d52186a758145ed0ba278d416831583b8caf6fa458bb205a81a1fb2031e150896e775f6e2026a8a0e0aef6489c09269f15e77bc34299feaf5466a6431ce5b3322251ce76387c2d056e35171c3ed8332c16fc555dadf832b6a37a58848015a74a8b114234038d428aad6a7e50a307046f0a2e9af80d6e532d096d80691de9834f2366594c13e28f8d38bf8a7a7b1e20d46a1eab06d364d66ee0a09c6548e9fdcc65a18212ec146a99aa94c9fe29961ef8b14d042eb26432f8e3c42afd939f7d2edc534b1cee8b7a4c5d9acb50210735486a2a44c40ce50f576d38aab670526802f20c12eeadc02501cc21ed1513da2a400f46db5b557fb206f78eaa2471db64c5313c87fcac5af14690daddb15497547c1b2336792e9cd1841d3687ac2a72fa023454f8cb6ca465a72615ebb975c74a59183fe491fab4d46b48b0f1c1ecc17e3655f613df613508900673b591f9c2826380fd743a4c603109049e470a3b3f87de208037ad1c95508dbc64b9df43418337dffc53d686277df9cb4f15e9b8559d662e93f2b9114db41cb279670604acd3229fbd11fb4a5321ca1753dcf4c69c356adaa1c387c1b6c9b983225b8a907cd8d04aa02d1d0babb2d6733d0a9ff88bd658af2a389db078f7e098eff5f1892a2aad6b5603c04c79a7cb697b3d6fd09cf6bc269da06f5ffcd52e8e42c4bf514d3f367c3f34698f5586f5649ed3c8b1581d01db8cb98142cac80fbdbb56688b4e9ce7f7dc4d8d36b7c700ef086c5f4d45cfc63877e257eb6b66e0b1ca196189b6ba5e58bdadbfe528ee6a83487587e6de7575c7df9967cc47385df4ef8c5eff0e1df62943d3561d11639eabff7dc7309aa4ad04346d4567e6a2d411ac3da53dcd4e4be40f6d044119dd09d36de3b19cf2e3b86816dc6540d23cff72809bd8d87cb1cc1390c92ef39aa158944445cb9e8fa9347de7453be9926d1ca1fd256e5184eef32248765a972c66f758f32279a7b97bbdb026565db49fc9d82a8268da2b7e5c5dbac7030bce478462abfaf8406b18b190a9647e3f4462c99e76a0e4a0e8e19068277cedb6eb64803717dc45b43b260179c4223b451b83e2fc00e91756fc2711af02da94d0811be0c107178949aa59d1c58ad00c23ac6bac6d73bb8f60e7e7669ed770970d11c63cacf42d31753b2cb8f0780e6f4f79829c5f29a8ab533e8ae5c41fdf72b1300bea57898ddccb60371da7162de32cf571396dad20f9a2628a359b3667a6614e45806d11d9f9c0bbd17645efcee640c2bfea0578c4f8e2e67ddc90c5a9b091ef1b753a477206d990b469596f62172f43b744b3170537794cde315148361fd774e65f6b74e6829a91384d5e0f77c45d0b0fcff2f25223fa1813ca39386d4286725ea55d24b80b683fd5c8c06a709c3e7b11f241a54c4cc9aac73dd510efc73389bccdabf156ffe6ff25322ceafcfd583676322baa44208ca750d2a15c1e2c630da95eb45553de7b51af3ff01b7d664eafa5362d7bea3d9780594f1a9478d0a990accf22bb3f35b9c9cc38c676627566591744a3949ef29eb83909cd3490525b952fa563ad698416da04b2fc765c9953c545043a7ee2edf6c1887925c6f5301d9dba67f3a67348071501451ea5bc407b76c6eb6eb5569e343ec033b0c607e8540cd58d5f30dd002edd2a55b99f121961e015609cb25a81db8268bffaac58e98384f99c02c7172ae2c4e86b39acfe729b83a91a61007f17513efa84600458c240789efa8af3b256c75ed40c3b59c1ea7ce40a6f382a24ed39b6ee078d4897f10f6a4e5f54212ee240fd65af2b401cefaa83b1d5ae89fc61f99ccbefdef386f444dc8000fae9ed7be43335ca75da500098e527f410ba561415530c73d19fb64a7541cca5eba88e7235929cf4c7a31bcbcc8ad5212b45d282117c385a1197a672353c9ca4b3078263999e23a48d14c08fb9d752134a96e22a727ac7f8fb95c7f9f91f6883cd6159de94a8625032b9feeae57d2710fb38258bccc143d70bf4ab50457cc8d9a7608531cb63c18e54fd175bd3cebcc0b993ed046502348adf4eb1038ed7360ced6f850b8232d9c56078ba51095e9b990254c942ef90027ac20e898249b12893ebf8e9da6da8138be19758790a9f3c1061d54ac5d10d462d70be855b0a9d29154f87241cf235f75854e362223c2326ad54afd1ebadcc682957c9450c55edaff774b4b441a45efda136e777fb226ce3f6574f0f42ca7ff0e4d3d94904903535b0e7ec2af6e8a5befd041dfa4261eeec51f62d5369db7a78e69148f1d26e0642e835a7e89b538c83c258e179b65f2ba8669de53eaf456e58a50ca2d00423d36fe3da728dad698604a781fc66c59ea5b09acae121a19cae18b6a2717408762f056a42d67abb9f24ba032ba951de84ef9d6519bbbcdf35bc4181ff2f5e5db96bbbb20b3e403468eb1c6bc282cb8fc65bb76be6df60c6c53b6bad7f56ef82823bfb7190346ea65b3b9457c3132447c78ca705dec6ade6bf8def8faf62b7f554f23d1f7e33668feab8ba4dc9c50ca50cf4e4c95e0c04bd994655af0bed4854a631092221c5c86f9bc506b43332d5160b26e93cd109f5b9c6d72682115100fefa6ad8003765f4b3e2f302319bc72e1946952cc6f45b0179dcb2eabf6c287272c62232500b8e8c0b54c5253c500c682d43b237fe2479b86c4f8c1aad58fe2d4e9dd990982ac9eeb475adf83b1c7d23d087209348ede07b0fa3b52ed2742c91f0e6138b2f2182ad6daa3fe3b44d93d1918542b74dbd29c9e25db09fd55a4e30e833427aab6c53389576dd804e6fdb01fec2ab2c631ea03572e210f61c57dd9ac309e8f79387b620aa7019565fffdd3566c3bc5b3f8efe4e3fe558ca1e19552fefde998a5de015f63b0a75ce7f79f4dc681d18dc298aae64b7cbfa74610b3f70ed35b69b81221d19c5ca6551037acc7771132327b79e0b2968fa689d7b9a170e3983e2b3a3ba160bb9248a96b25ebe87205a7db66035cf0cc21545adc4625648536d805b1c04ab5ca3ad3807d2a2659be4678b00c9fe547f6190f41de1e087cfd937220a70be478d03b9001d0af23c163bbeddaf4b3425f1b5e206664a9115f529953a4a3833751d67fc755eb1aabc96a4b4e81a6a93ce143dacff2f64f0ab58c2ec5b82e900ae6e3edc0a356d48619dae0e2ccabe79b757674630d1c78b928b43c0e84cf9a27e2ff6dc2149a467c58bb61f2e48ad727c2bd1426de5f8ce85ed0aa77310e0954eb32e716d03ed7552e15b5c6f1cbdfa7711fa25e8e46f35dffeb6048e773465aa9854305fecaeed845f1c6a61af769e529a50754445a15e1b40a46c9cd87b701c6ff69d5c7c6f5cf42567bda8795dca79356295029215b3b253744420cd1c510e7a942ce52bb5300d04dbca9612da0b6573af19965252b35f97942facbd9d851b689b43ac831703395dd715dd0471dda1c5947791feeee95d8d9283a3328598adbaaa85f50aeb432390e823cfb6cbbda8abc143680020d246c5dde4a6c45ae84fe11078713bc87c465e8d88f0b23e2804a6a3e19afebeeaa5a0f4c729db84107c6c8b7f838e251b0c174599d27f5fa92046baf6ad431fbef4df75bfaef0a79dbdbd6a2fae8a97abff4b9eeb078696bd95fc84d71195a9bbaeb1cf12989c2bdc7e643aed74b976ab9a7bf800e26079d1d04880276a4f035d4dc86f748936877948f17c5d588368966818ff5005857f8269bb3b5c09d7b3306c93d569f37850e0a7d5bce3ec80e408d2b3037ce36e2e5bbcb524e982d81fc165a4af4769ba0eaf15917b15832a5f5f84caa2cc0e92fcaa63c6bb4467c8cf8cc36b4bb15e7417311ca51566cef85116d8d06be1e99c610ad9709d112b1238ff2068392c344f011327913a5c12636fdb5964bd42f6d30209984318c43f4cf294e71bf60cada362c1b0d6486a1003e20980e81553c8ef46031aaedcd236f162eaa20710f70d09a2c952f4de152a527e1dd9db12b8d249a3a3fb2447cf8478306ccbaa186fe4ea5796887d49e1d3c79aa4eece23d6a56ab1589433f820ba79b197ce855adbb7edd7b5cbc04a54aedbae2b1a31c731b4cae59d906e4c5a4b5e8d08452976cf186cc9a3877e2de21e274f0cf6a67b5e72f2b6df5a33d2e0b99f191ab9f6eabe68efa3fc65f7831ea402e3e70e7cceb1827aabba5c152a5877c3ec5b878e352e4bfeed0cc1dcd87ec3271335bc552fdf45bb4aab3082913618658d57484fc49314030b71358e9c670dec4375aaa02d3c4f4d0a2e522cc5ee2dec627a76cc378153a485a63b9d5d8be6cafb2333fe256cf19870b05b443704fad102518d740c8dad5bf2384e898a31e05ba0caf2bfd6c5ff3604ed969942ff31abf36916e7a1fc6381b793d8c3a11d8d16eae961658febaaae3d12ae6c985154e5ac1d8d97967238ce7c3574dc40933f752feb0aafdf5296598fdbfd6ea59fc706930b7bf458637f8a86b9bd53d72789fef65c58cf337f44c083a62dffd92f1a974eaf3c8e7664ba5e8d8cec9ad36f0f3cb4c9c6742ae6388baf1d9a90c1a3c210457579d66eda6bb2e72bbd41c3ce0187019fee1f340be413688eddd1ed825ffa0dfaae6ca05926103a1299c936c93396636b3a20cd74c9e056966233cb4f46cd1eab34a1b9555bfeecddfc919ae507c33098353985a56c8aefa0c226d96f08f1ee03b72628438fe775ea604b7ea5e0961fc369c9cb124e7b9a5a783b96662910f290a06a7bc834626bc46f5740806097875932422c4fe22afd1feed280c92992c2de8adf0247ebe844f874358f090a4fede6394317b2ca117e5da09c371c8d0fe559d0f47a4718f188031966b8b44017d9e6e93d2bef58d794b037f917b4c5d0c07f0d5b324a3e06ab50b98b6bf69497c1d7cca0c75a30e5d6a41f69a136569deacaba73d8c5dda0357deef407549a384c355e1e7887c4b4caa5c17e7225d1cfdad942cebc4e7a56acf724311962a381cf8c07f6bdfc96dce731b897f8e7a9df070740dc7b5896827848f3d32085530cc24874ca162d19b692d47f46b4105153f5f12f2d9458b4cc3ddfd84aab512b21caf70f290fadefea740a802b12145ab73e13881285d3478c54483c18183a9e695014416408d36be0f1506595b188488135dac65bce6cd14b520eb9cc05c61f5be82ca63ba042202291229a1700d08d3a73d37449dc69e4a1cd8fbbc6e2783071f9bcc7e0548eaf27ea0d983f9e71bb5b8e8a1c7d4e7ef4081d67cca93cc574341b210122940d6b79528dda6fd900356cdc6b295672f081e6c81356d383225d2ab3160c52fd32e79f610feb484ef08dd6e3f32576188300d6541f04e634335dce57e823f1eeb395f2ac1045297ec747323bbe8b4c7d9b135f57e73151fe6ce8878033e29fe6f4092b8a4b72a3c6f8d3f75a29d55a30ad1bda3a0876971033afeeb265e2bd8468b3a2820dee47d609d6aeb5c19e1fc02d9cdf80ff4f26383bf67c651a8ecb35a627d039c432862a7ea1b0e512401dbab118f7b348259fc4ca6eb92ebe61597920d86d082ac9cf22dbd7b64ea865d47ecc029b475f946444737b1d65cde4d8cd6d37b1739e2eb35d85db2778d069c48ad2fc32b3b2060445cb9e21a0b593d9b0efe856129adbb339114e7cfa36e66e6d6a78669efc050037476a5e7621e8dcb22d4cb34bad4b5f5412842f29584652a506e99f5f7a1bd8975b423c3c8f7e425fb9e761b54218082cee2ec6ac9113e1b01cc2557bd8b50df49bc8339a5aaeaaf7fa3ad9b5f5ac4e2228a0dbfa9dc9cc2d20b5218a746b10cd6d6eece62004873972d993e43482b1e2368cc779caf2525ab7247bf1f99871806498c0c65c1a6792c82618b68b35b3dbbc3521c0950814e75482dd48c4e7b0cbf5f847a17b5abc724eb2f0a3763fc47d1e5c7355a88594516d62bdf562b34efeb5effa0e0a3006769b02844161fb7454c14524bf61f551078646a69e4f95bc9718f240729554d61e11976c06eb0343e16991be545dd1cb9813912f9a07f97a3393a1c36589139bb766e4106a8df1a3a16bc4b8baf8a57481b9f4a18f18791842ba70eb5d7206c7f0ff6b09f2a72114134c40ac69cedadc32ddc3a4d3fd7f0742f1d230d0cca8fb593faec125e31c35c58fb7f1c74cfc7349f390b20f3395d7b2de683c561564a63ecc21de7abe6d5926aa7d3179ff84ca259fee519737280a20d4855653c8e4493019669a77857f2ec52112d3f8530e5164273efc1284d00f0b78cc2760738264cbabefbd92f4dc070f881b61b8f3f3a9744e0529f7a91866e13f9f17f291d9797ff81f9053790e728453e963323ecf22d468c353cd205b9220ad90a080a05407bb8298cb4f8e8101c1f92f40fab00b6e031c0c11e967342168272379912b8c800cdfcdb6cda1de35370c3f8388d442db9ebc41ecf30a1c852640e9ebba9fdd92e26430fb845f607d51e1acc35337d00bf5de073d70657f4dc99f367f5379d180eac454f1da13d25aa12df958e0376d6fb6f5ce8bc825f6ebe0869bbd024612fa7454cc42556b8e4db355a7ecc67f658730a7fa7080a80dd0098bac2d3d80ec601da9e9557cc30ed6e6ed3ac76c6c42e311492d5420881e9a8332ca386bc6f84255484eb3b6b6ce76ea4914a1487a9ff3d77dd9df3e32b3bc0c0098771364b121a9946ee5a9c367be0c1dd7e2e146d739d19a825a203467b4f2697951daf6917e8932cc53bc55451ceacff704dd4d8335be43b95d7d09a515e4f1ff61190923c98287e9967508f103f4d3f4af82fb1e77fbedd161bb2ee63f422e8d1d53f8e293142839164a3af603c8ce431b88dc2370f91d8d0f4110b5c19873c4ff2262585bbc884d6bc287cebcc51384f1456d1c9154472eaed55bae2d5666198efab1ff93fa2c98ff645be56f78481cc94f2a74898bff48c8be6c19d31a7a0d35438432cc3b89ae59b643c4cc50d08d0c0ca0c5aa1eaa6e0582ce83734334128aa345ac752f23f0032ecbf733fd415822144c8a737adcf0bdb90f955e7f08cc077072c71f138ff0743450f736cc2f00a3ec9a2d862f0b8389c78baa250c7c9c3461881d3aa72242357d6f4f555bde0919fc263952a9baa8d244df5c379a03f599cd121948488f47bf50936a100ae2e3fcd95d3859896eb32fa7870636a6437a80399d45d8862b838a5459c1e777e233b4facba51b3c9c350a65ff4282af9f9db02d861d6bce8b781eff3ef15c21305aead6de0a1841c7c0fb16c48bac049cd1822f791a62a033b7f5c79f72de7571cbf70420d1b11785d96a20636c2373bfb87e95c2c76da998a5973407c0ff031b2971f92086a64c3ad0e019484e894b6ca52e170faedd293ed38bcb04662e4880d5d42c49a3485e7b59e5780fbd3e1a4849e929bc2b41cdaac44c0311ccfcc46763a62428b227acabd6f2a31632c26ec6f9683464e72c0550fa4062cc8feaf5235560673a3db4d560ca9050f849a6976d35f3cd764d0c4eb5e8117071096959492d8b6bfa9080671ec736d8225c6b42afa2445b8cd763286e002792cbd4087f4d5e8fa8e6750ab05a3f5ecf210570ef02b0b274f510b110dde50b60197a38b5a45b05b48a35c46f97a5809c70bf680b0ad3c6906b7386a67a09256f06568acae2b55605526d3de35b9c44d69df4b1052c9e09344c9a4447e6ef1df60f136efd8cf02921eb85e7ca1a8603e4154fdc8bec71f2d704df7a7399095dbb901b65ec079dd770920a527258f5566b531b5b0dcef20c0f40cdb9c804da5d1625e8f004f710e997f284fe7a9b5ca4d2e401acf95880cd337cef6293bef4a4ac5d06add6bfebe80e99605c41563e8791547d52dcfdc0aefa15af05952bcabcd491eeffefd6a15b2ad336ec751e1d774d00f8dd27331439b9a7a7231aefdf1e25612c56aeefb3ab935c733f74577bec6c960ebc76ad0df5f8af424bb35f943115472952924fa561de71afb5dda6dc5f5660197139a0105c3d991adfed0facae3ab07064489af90642df36ffff60a168b11bc38beed0cdd50ef19b6026846addfc499c1c2046111186e1c36e78297d610315776d14a38908d3554e6d0e326158e05f06dab7c5900c0115bf91d675c300a2d9bfb5398ce4603110ae66f2be22a76d7d3b308dbe086e5807960a78ba14a036f506c4cc1162638ec025de726c21321a30b3be0083ccb0e223e128fd8ae4c825e3e74c4ac5859b62f05f441be0ef917cf61ac3bebb5f6c35f71b8dd35ca5f10622cb9c441dc070540db5384da0d03dec59aaf216c95308d9fdcf997893fab34e3e4e35368e956ab894c21b861085a00eb81eb9b7322a0f24a457ec7be535776492c9b7a09c97132702006e53d472505abdd09b0e8a66f7cc875ef74bb4f8b5efdcd89400cc27e225f1eac1455128d736c75b069b367c4b38234f3a7687b7175658f54ad591da601bd0fb84d91b1bad4951289ccbcaec81ede493267ad1e2e42ea39a4c9d9221059f2bbd596d96bd3af907c3be4ecc2a245457e55bc7281ed4df495239d7218e5b166eaa9f9158a018dc47b6fb629ed7bb0209904ae9fedd41fec6977300d494c928164dc7d2d546bf599d7cd4c5c757be8d563e6461ca80b3f387e488a93fb6e83abf0172a09123a6f7757fcad6ea3c974cea4a48bd1fabb35429c3fb4ea77720c4c580209920b9583c6c5a5e145bf8edc7124e586a72d0c8a718a03b5fc0581b5cc624421ae89a5baeee76ff1cb11f1a928aa523d848825c52c123d95afecc30793d7857b46d2919b578503e4e6e01b625a0093ba52522f6eddfa955ded765f89097e6f1d432f82af6a6908c28076348f6eec4f05f7d4510eeb0bb2149de97ec49621a8118be8a07c8d5a2793801963b8ce372e245d56b42cbe0d3dc2a78e473ea386d85f0845128c291f0860ce12d066e1a9da216a1f7f50bec8dd6ff2ade3819676a80da08e0167a781523052a9a4200f09f8030879db0253043b24688e7915d9b9bf253d0a94936d48ae0cf425076fc02133d31eaaf0cbe588ffe9f0783280d217c7aec602f49ae53cc3fb9cd020da5cb7309893fbe532dc1dd0c472b5364ceb5865b123be480476243a0532f018e94b949a5a51b4e3c0a08b56bfd78bac008b4343353c86b95787caba329ece1624f68017223f1d11fb999a3ae10543bc2519a20942f9a26f1d82f4ec8bc8b24314c3424ff7457e986d2f112432e5249042ff3b5fe0425991ddce6b771798e3c7401ba4ece14b1e9f9ecbbabbbb5f784ddfa4d6eb18fff54820d3f705dff7abc930676c93e839464f15d35ba8835c851e85703d58f118bcbb6df95f3be9cb50716d0d63ee15f73ed05ab1bd9392c29529bacb374c7475f7134ca86d"),
   %% SHAKE128 VariableOut
   hexstr2bin("84e950051876050dc851fbd99e6247b8"),
   hexstr2bin("1822b7cc3c4ea4f2440a362b117f808a"),
   hexstr2bin("2ab3a70f3b01836d8efceb67490c3c38"),
   hexstr2bin("0a13ad2c7a239b4ba73ea6592ae84ea9")
  ],
  [%% SHAKE128 ShortMsg
   hexstr2bin("7f9c2ba4e88f827d616045507605853e"),
   hexstr2bin("fa996dafaa208d72287c23bc4ed4bfd5"),
   hexstr2bin("c7211512340734235bb8d3c4651495aa"),
   hexstr2bin("ac71d8e087ae133f3da590e1a2b54d48"),
   hexstr2bin("b4813895ae01b43c9d9ed85a8b03aaf4"),
   %% SHAKE128 LongMsg
   hexstr2bin("3109d9472ca436e805c6b3db2251a9bc"),
   hexstr2bin("0f994e4aa804282cff22ad7d6229ef2c"),
   %% SHAKE128 VariableOut
   hexstr2bin("8599bd89f63a848c49ca593ec37a12c6"),
   hexstr2bin("19e740d7d87bc322edeee86a05eb59b64bb86f90dc7b98f781720b7cac37fdaf293ce6bd047a14fe"),
   hexstr2bin("ca7ca55bf123aba45287268c4050ab030b1415f4497d5fe8dbc5386ae37d24384a2fd6a715fcad48ff9e810c1d378fa70f1503767e9e338e33697206f863dc8015b4d1e9b8f81ddee22aac59d52055a1b0784a364369cc50f403045a1bdb25b639"),
   hexstr2bin("5feaf99c15f48851943ff9baa6e5055d8377f0dd347aa4dbece51ad3a6d9ce0c01aee9fe2260b80a4673a909b532adcdd1e421c32d6460535b5fe392a58d2634979a5a104d6c470aa3306c400b061db91c463b2848297bca2bc26d1864ba49d7ff949ebca50fbf79a5e63716dc82b600bd52ca7437ed774d169f6bf02e46487956fba2230f34cd2a0485484d")
  ],
  [%% SHAKE128 ShortMsg
   128,
   128,
   128,
   128,
   128,
   %% SHAKE128 LongMsg
   128,
   128,
   %% SHAKE128 VariableOut
   128,
   320,
   776,
   1120
  ]
  }.

%%% https://csrc.nist.gov/projects/cryptographic-algorithm-validation-program/secure-hashing
sha3_shake256_test_vectors(shake256) ->
  {[%% SHAKE256 ShortMsg
    hexstr2bin(""),
    hexstr2bin("0f"),
    hexstr2bin("0dc1"),
    hexstr2bin("a4d7897eaf5c49979b361c39a67f47e26c2f75e5ffe0645539d4de245138eb8cadaa45aef7fa0c7a732dbbce90c85be2bd4bf6e37dfb4fdebee4d0e0671fc45c3051c6ccb674799bcfda7a431a6e93b3db3e32f30636190a9a2e5620302876e0d4d2f6201353fac4554341df6efb591c6f100f5dc21a2aa176ba592bd7db69e14237bbf2371df6bbb072f9ecb1f714e621c97768d82eea6bf98ebf4a82c005262188ff894a5dd549866f88b00ee82bd99872515d71fac230ccb472c55a60"),
    hexstr2bin("104fefe89f08d15d36a2233f42a7defa917c5ad2642e06cac56d5cc51ad914ecfb7d984f4199b9cf5fa5a03bf69207b9a353a9681c9cf6437bea0c49d9c3e3db1f3fc76519c70c40cc1dfdd70a9c150943c272cf9eeb861f485f10100c8f4a3e259c6470501932782512225ba64d70b219cf9d5013a21d25d6d65062dcc6b3deb49d58b90d18933f118df70ff42c807ccc851233a34a221eca56b38971ef858475488988794a975d3894633a19c1ae2f05e9b9c0756affd3cfe823ccf29228f60fa7e025bc39a79943325126409460926b057a3fb28a1b098b938872883804fd2bc245d7fd6d29bcda6ca6198f2eff6ea7e03ef78133de8ba65fc8c45a688160719fa1e7646d878ea44c4b5c2e16f48b"),
    %% SHAKE256 LongMsg
    hexstr2bin("dc5a100fa16df1583c79722a0d72833d3bf22c109b8889dbd35213c6bfce205813edae3242695cfd9f59b9a1c203c1b72ef1a5423147cb990b5316a85266675894e2644c3f9578cebe451a09e58c53788fe77a9e850943f8a275f830354b0593a762bac55e984db3e0661eca3cb83f67a6fb348e6177f7dee2df40c4322602f094953905681be3954fe44c4c902c8f6bba565a788b38f13411ba76ce0f9f6756a2a2687424c5435a51e62df7a8934b6e141f74c6ccf539e3782d22b5955d3baf1ab2cf7b5c3f74ec2f9447344e937957fd7f0bdfec56d5d25f61cde18c0986e244ecf780d6307e313117256948d4230ebb9ea62bb302cfe80d7dfebabc4a51d7687967ed5b416a139e974c005fff507a96"),
    hexstr2bin("8488b6b0a5cb3b778d9e6169cddfdd7468127361da571661589a51e95a0ae5c057ccb457cff0dd588fa4298131a7a6b3e201914668f279a06328a206d234a8b1da3f6425c3f913be44d3b58c006eb4615053c62743e41477e3939c68ba51d4a5954990c388cf25d13293cf30421b50a53a70daac74172f0f624207b81948b2c31153fed826585bb8e34d3d150096c2a729c0957c02711290679fab9df8018a34d5c3770da67efd9cdf67c8e442c75290a5abbf40f5dc05217b114fc05e64a4206903c30a0f2852a4ac4b38450e96488834991f9f553f4fea2500bdc8535947ae5679aa76693ea3f9d45f55df6eebebb660f27e422b813914edd4bb5ee0c8bc06238ff4f9bda738c2df0de75b69ae8443b01ea4d07e8c0bc75bf6122a0b994c5158ac683c124d592853bcb56007c75b56ac8559f257470d2af8cc8ad3deb43cb77e37d0e770174e53bad38897511eb818dd8f6550bd9a86664bbfea026101d75f1c9af2a2dfed5e6a9bbc28a84e9be3b1caa58f91b86dc0a2dc38fc5e9b696472ba3c961368b7d3957fde5d3a07272348fc2da05e50a80107349d869094d22a17e08586e5125df3feeda1eff582a8c205f991615e8e3492bdda57513f5426727717dd89bee6c807ca6d6da583c90806114b37973a79f7bbeb56cf2514e44b84b9e6ac6647eade562487aa4889e42d389479890d991adac3ba316cc9b74b2ba807d65ae62728882a32c4c0a0b2d9019fb50ced8a2477c5f451f29507cf91ac26866e4fd106a8afc91cab1875a3b26a859d8bcdd5839aa194d921b4a504bfb8456036f4ef8e71397c0bc5188f07775230747e90b75d8b54ec7947306c00db364fbdb6af07658a108b279829b6842ea0e9616e9ef85a50c8445aeb712468f00d8ac477e9e333fe1a3e97aeb4b1c2e13cf88ce25f6023f3e3e4a395df95e703273f9e000b49a1d241dd848f1f496a970da51e623bb6607c8f137dcde286231e6bb572d97b0256106baf15c1d3462459fd383f769ac4b8f1b31a101ce2c1c448c136698ba3d6fbc082fd73ce3970972ef3f816a7e450d45c37418309470c738a1108ab9dea8c38e86d0fc53eb88e6c1ed82fcc7c81235cc402d0fe1d7930d8eb43745f97b7eeb492142d3497b76a4b319223887ed905f732fe0fabdee0f59a7340c2ae0274c218c55f9fe863a0cada700a1c733354be4dad1246b0d6e422612aee3abf6488d10b8a36ac0829da77c4e22864b5255b15073d26bc358dee81d7193624bb485be336e8819ebc72cd39af8fd54275dc54d3cdea23ba638a64d8a24700c7c93355c041e728bb2ec5e1d74480f7fddbdfa9c046bdef886155b0f06abc356c12afde2fd38f134c83aa7e1b2abc77de11b6c0a41bbd736e58763891d881bb5bbd435ef994e92b98e6a90ccb094b3e2d3ce90347f4f397b470756682939d7cdcf3783de62efc56e97d4bf3b08b3633328700d871776544a1d0dba18d44185f37dcd4e3be46d07d8570179b9256843d341884a9e94f1d607ea130507893fb4bb39fe1b849876e89b5e8966d39123d467d17fddd782223d4930afadd0c274d2a996527254f96dd60c10d4ecf6c686a3553adf1dce6997d892834c3af11a916e331552d90971e25ecf106c92af5ff12f2ff02ea0efb5b35668547122e4fc05e5ec3987b62eac54f138dfc8fe79a813d64c83d6fdb7e115352f26a70c28e152e8397559904103362bfdc16266c2493f375f88609ab3962dd37b6c564d395508d3af5b510dcd11166d7277d18235adf3d0cd6073ee6f2f7ddfc1a8176c92d18846b24e06654f5076443b658ed72060d2211a9e599f472775a8966c62e0c1e151afe6dfd26d343bc62ebd1d02bc3eec5ab6dba4b9f3d818b5dc6e2bb150430f057db1834d5c084d96bb7c00d5c7ddbaedf06515fda2ae6dbed9bd589b21a2a5143ea6daffd858005848fca3607089670c6c5b549d1216b0e12f089578db4d6dfe714b3932589ffe9b75b3973b5cce4b4689230320c8bd6ccd4ad444e2f3cfa434a3f340bd634c92d0b0353eeed319022ce56084214d1f0bf3cba4f961f185f257454363c97eea578896f61cc379f6061fff786a07059818d51fff256e5b2cd2b6e3c0251f01f5d723787aa381b7e1e1a035ff275a88d51de1c6474b70a700c003fdde070692fb7c41f347858223690f4e37494e845d4275ba3d21fe015bce34aef2117c28b877c16495c1154d32f7f98a620b322ce95cdabf02f39f98efd1bb287eff16edd8d740fbbe8be55b7d1b2ad544a994acb3a09770b650b2cfe9a7cdf0e2eb49ce8aa66587fc1c7835485f3dcf6da8c89c42aa467fa5add9fecfdce7a8cc8a97632461b999dcd740535b108cd92b7d50742cd6622c7f8f77ae08dd92a0aa4182145ca5c791bf60d04dfbfba2733cbbcebcc8b16517a25a0e662b265988eaa49c8a99cefa00323d9a7dc15d192defd885eebcc7e9b19cb210bfbd4396b23c28391e4269cb75d3c2117b21d15b89d35838726f864a9aed1c210325b1e4cced410a480cf2d662f282d7bec3d4e3e775bf38f05779122401fcb1d90b0dce399c8362a05fe685483d145fc0fdf43cf7558219d10b1cdafd89d845c9c393bd78b863e4bf91d172a601eb1c2a56abd119d1255dad00fdff87d3335f3de5f03d1acf7702ee693008fcff7b8f4364684acf972d76dccf791a437c9250ade8afb78050113fc53c8ec2cd644a4ba48da60499160ee7f8cd2a95c0e479dca2523ec759d8d71dcc924a2a980edbb3d659019906bc5e3389909f0c70b95e55a3a909e6f7d84d0f7cf0fecadd2db46a514cf76289187febf7a022e8e359b45e3a53bd90fabcc7975edf190e0d6f48c1ade0040ba0b90873db7272b589542058def8a65fd314fb47dab3257eb9321d7c349893feddfc6c6d9d9dfaece31706c7f9fe709e017f3c837923a0976114b6dc8dfd33b6a171303bc67918198130ec3e23db98e6063a2ffcc29716e28595897522846fcd8bb5e850de9a76a5d430552dd5168bd261f439f64f8404daa72329d89ab4a49e0d18f95f5e88832c5c73b6199645c5cda52bef126880d70a364bfc794f048ec2a638d7b9ceb9ca17aa99a383efdfc332405fb3d3b60419dbc38e95d9b7fa6ab5d78427f297a4b7e21f1091ff3a5b20caa3fe1cbcb09459d9df596a6c8e1ce4f211995e9c51dfd9365a56f5de0e3e6ad17085ae2fd098038eda3b0eb419d26a0204d6e00975d1aa6ebcfb63b2e3ffedf11684ecbf9ea5935554b1e742e3b1865ff1cb50dc8c10a805e76c5010541636a4068a3cfe8333a5e7cf3c889b8430b68698e52587fd235e964a384f78c727d164e0f4234f26d8a790ba69672073e676f04ac2e29e51604a8a55ddb64ec59b4abe06613423e1fb04fa969707642b49343787d2fc82217dd87cbacb2924c7f71f0a173fe43f5d116a8ff463edf912bf9ac6e093e7c5d5d294c7b98f1c7d56a1aae3640ec7d3e0c234cb444e8f31d2d00718e59448fbe6e51c05f2a804cafa419ad4ad5c33942563f02716b56fb0e41105c7f7427476ed4bbab1f69476901b5d4b583386dbf283ad293dec4713e8cf09133ea62d1ed9517d1d5635e2f65f323b58e8973d057193d44c8a2561f06bec0ebbf762c28c85abdfa05d20f1e8aaf7822ff058ed6493de9fe67662a4999cc3b87c8ccd91622e6509d051486fd1e5a3b85d29f8f66dcbbc56638d146d840dfe9cb16937182416158249bd2cbb56a1ff545cdfe80675abc59e5058cb4b2431f51207074fd536ef25bf690b4e42cb6242dcab9cb68c309672ee4943e7cc1eab3fbef9f178473f1c1e37bab61db4c4a916b7c5dad506132e5cbc9ae90a4eb7dc36f9e6c230ec912f7771bb682b8fc6d8f38c3834d3f291c5db8fd1b2a361a437936979c41a43d19bca03e77be812368461a07f1523d0311f87ee9a3b3393585fbd740fa184cb74d37e44cb8be40ab83dd9bb0706553d757dea4f40ed4b6248b71da034d51101ff891cead3f29fc7b067c4326c82e878d3736b2852334b4cf9d04c8475c93f85d60f1e3c9d4a8be7b394d99d1c154e30df9283106ab7c3b1e03db2773f0870b5acd80ad0941e38d936475251a23806f3fe5dea8de6f5354f9042082f1d1d451fc8966b3d34d8171abce1f6f67b559320c437d9f7534620548eaf85e28c723d22cdbca7d90bf2b72abc07100214088345a5a309e33c55058f3fb8d45809a1c97d30ad494a96088997b82935fb18cb47d13731d2e80e9c655e28007be611166a7d75fae7f84f841ed045cae147d6186161dd66cd6c4676c03fdc08e5233f6aa0a22227dc68b4ffd762c96eaa8ef87a60662cb461f6708e96a2d0548875e0f8fc99a9b6cf2252c653ddc776ea26ec5bb55c89ad390d194e692589560e84a25beeb2911a08ab9582049fd48638a61e56d0b398a3ab16ad30a8467b9fcfa60ffde78b6ce4416b19cd7a37552f0e932ad66319ce4a40a9b8cfe4743b945470667987928187e1e74be32512eda3e4548e64bcfd8f13824ca8969e3d2dd6256a52394f56938762b6a86d85d0e2b75290b58c7ebd7f61f866190f461fe6d7bafa6fc79f4607656c4fbbe72ceff8cbc1ae3ca808f803e80b8c66a47c8aeb0badb6fbaf07ef51943728f2ca7ade8443ba238051ec2bb10844363df0920caef45616a25a09c106b67bc8e9a2001b09c1b4872d24a8d70b352ae9eb6e271397244c9fe04325599b2ea2a60ce85d241dc8494f9f7acf687701481e1b97cc4e581e09c92214db27837f4446d907d29824c9d0f363ceb08e98fef4593ae7c20b564406090429ab99642ab931eaab00d5b338f408f6bafc87c580d811b5e8d8e91628ea05f30c074d91a3a9ed9e2c786b4db81b06d4aa240291296bde391551561c5efe167b368eb2a09c7ed07026a9c758a9ce539a36a77d907b281520e7cbd0dd38dad10923a2572557b0911d0405887890869593caf75727d8756d09fab8e7ed5908bc297dd2a97477d795d15f2c1d020ecc437dd1a43ddbd40c3c50a8a919a122004e588fa028327f6c5559827df3996707c395052e8030f03519918c0cd4ad4639fe12e38b61c3d0d76602388bbaa35be1bc95c006ccbbaf886b591f64a776184e41e51404c4cd0eb13e57cbd5c0c43fb294ec8ccd81c0849ed7f3dd3110703a95b05b9b9cff92ab7244e6c6dcb4509522c305d5d33e03f1b0b60e40029e4fffb8dfc2d4c440d919a3202bb400e3fcfe9aed6e35c85fea8996ac14d249af4a0a1016c77a1b56f4ebcc46931e02fe0dde836aa2270d65e8be8910841e86d212dc33fbe5d5f2907a94462eb96f235127a784d13e1422fe83033089f88f4a951ed8bd058a82fa9bde95a94f0b12129bcefc17979ea7a9784dea058eceaef566e291f3ba420b83c795828e3e04b1212c685ea454403bf171adf5708592f817d5f2aa708088ca3ea233cd3d6232c70f817811b36ddf5b9b6927981a04c1e53250c354cc0eeb597bba3de86f6211ce1e4992e7ed6aa87117cd4f546a82f800d565d9535eabd643db8e18667c943e30ddc33d338ac14836f71c89bcda9691d57183a908c0edd5f3de8c67c9ed9ef489eebcdb7744084af7a9e57b23f2b62bdd7748b7076b4b66e3e54380754ae8e1539165023581b60d5db0a6784c3b482283871387b1d65b05be8022070b1ce89108867a25f0cc411bb3abde15782ce3125201829faa9a833ede4fc6700dcc3460233e5acb3b9c3002a3dc0ae74a400d77e387047344206a7ebda4af3fcfe152fb2e298566787a194e98f1ab42350d2dcf9278178730e06741e71f2aeaf3e4aabea3c8989c29de534c2d6d3ddabaecda85f9553a5fd3830024e52fe7f60c5ec2170e9bf2b0d24174a7bbbf41056e778a3772cefb71ce60cb94225754d56ba83fb0d9242259143c03ddde584356a235c360b915c9415412302c4916002da4695a089df1b6093236775b0fd8b333089ffc7e16fa2263370fc439a3ec6853491ac78ac7751c35eeb945abcaa12f4f042ebc0c36cca53bd9b1fe58dd5afbeacc24feb3e03ec3539c4cf54c8951f5ff275a8877730da05faec155165fe24958a020761062af5a06bd6a1cb9e65bd5c7d4bbc86c564dd23338965eeea602bf215785a42cd9573eb3d48912704c3f1d3daecb12377b0d895b19a9723e4a4cfbd964dd2f65beb6efef74328d0c357c5ac527f0fc853bbd543b948345a1a7115de55a8f7e578d5efc1e1eeec2ea3c840cf2b2d5cdc232edb1596bac3fa5570073d9ab25078ecfbfc1c427267a96bbf6a25ad1d22c7adb0f3f38e1acc6b5d49acc3ad9265157e04a0f764f621aae5e34c942278e8f0baea24c5eb8dd714faa30c56d571be26f899e03a8da3a197d7dded05d06537c111847673f33d6d5ff42630df5c77e4112061c6cd06a16fda0a545661000d96a17c25f1e5ef5215b3127b1f4788cb5e40bf055998171713d946f18d8efb978df8c1759e0326f458781d4ae174a296b4199c2ca183072d15fe525c9c76062ca42fa91e84b6e249bd17de3a7f50f1ff6f06817fa777200dfeb0f83c8aff6cf464acfcc5abe4697cccf61af400c2bf2f166d34a5725aa8dc5656fe396013731654d882151c7605b30d7e9c206cb1524e31b87e470a16e8c5a35a2dfc259965bbfd1b02b3de55fb1609172b65cc07f483484e270d789128ee6f234d2de179880ae5de464af96796cf472a8e6b27abeaf6e497f79ec3b5b07f411926749d9b252c9687148076961d034f1d8edb85857fd08cfbb6a3f368c7a1a2a47f7760e78348988035fd5cc9d7ddaa5af35dee7dfa39c9e22bc31194b67b192c22e13245ec87712f716ab3e80d82df4785986b7ba25822b41fa72420c6372e19d5581b9da611abfe8f6f84da081d764e0b350367c1561fd95af734f43cfa69d3618424d7fa0dcd5459b3d6890dd4f8d64fdbbf299d5a48baa45a4af863ef53c28e0f54db30a3609895dfb1dc67cfec461733a5a97e8d26bdc5786c14823b734f239eb49190fdeb340c9a1dbb5c37da082ce5c0d0ae17404b84faa530e00951e0eadc4ae16240f68f868d230e36344a8356981584741f1ecdaa88fa34c67f76dfa96e20b4ed970a11cb60cabd3bedc51f5cf4aeae17cd49a9ffc975087cc569b229ff3ef07a28ae22a2ba1bb1b48bd24408a39237e04a48494b661d944a04b2c33235aa7f511b6a2ad4431cd0e835fa5a204ac90517bea3135606fac17864da788cb86e47ba99c6912aa4d7032114c4c96d945c020451e371e85215754d0c683bfe319232fdca4bbf69855433a550b8fab7265214ee298fcac8f275991225e47351481c2761d5801d5c8d9a46be105e53c6abb3251f712f85bf8d221610d6d7874da4160d2fe2ed11ca65b126b17c3f3d61c9871cdd4f4c0c95cb25d28c48c8e1baee87ceb01b1b047ab5a1b9c9bee2215228d7e171eb6dc6125685b7d1a23fe860cd785129a6915f352472e94dfbcf655f71aa5c236e1a1bfdd34aac260dc40407eaf07ee5abd788c35a2d9c845e8f73514c09753a00db7320ed7d98d2b1e09d76ecf73d2b5376e9ec00dabcb70ca9b1ef60fb1400c83936414a25651eb51a1b767c957642c5f5e878be7f302f30a58903568821bbefbbcc29924afa5242fbe7cf42d311732adcef2e3b26e031f00c6253d5b884b49cea9ea06ef43fb67f0a6749e5b8f606aab465d720d6b4087465a3a986942533f84c126e862c1c9195cf04ab0e4d0e436c04b23b149894fbe538b6cb1123ec7f2b6b915a81f9792dabd226bf64993f7199543d22c07368516b0e02811d6ec2e25ccf842d07643ed0ea9c364b90a472da582d7054cb6f18532d6def95527b8da2b4fe51afc27453c37ab49a18397690c4d215b298de948c4954e0e5cd5a5437085a8262a85731a04e82a2bcee08187f782f966cb8a1ebad7b07ec4cf3da01a65c76e31de071a30eb82aef0395a591293e9638047abb7b61306f1b8ccd7d88c4e632a7ec1fb6d3834b9c22d6d4b7e9745d2f82e3a7d54c91b1b4f40d24e8f377edb458362ccec8e84dfdc39f74e6284a1f23fc710796672647fdff38bfc64aa9bb247d62d0628a2607785ac53edc5792ca9a2e1661a7d3063872c57368bd934196ab9fdad31a2223c13270b0f73e7e34a7bba13b7eaf981bcc3dcd27e575d827c7714505fd9d97c9c3568cc76ab7a317feafdc462757bf4b4d839fbdff982043969df74b1594f1ff540a37745eedd025179e67a122474f80016fc49b7ecb747d4a573e14e5a639acbd76f81f9a880c4539afcbb2372650a13226c32b2ab8dbbd2dc9a0c8337c76ba47c4572ee731605fae0a8fe3309dcb9626d324d4cb075deb8518a7cfc8bdb0103719dc0a0d90d25cbcd83b9bd453c647259c70fd286002ef9b6fdb926c12ef96d8a0101bd44c6d22ac415841eaf14c9c79a92e100a69d3b1b7f4c377e6233c34717e7ac9040d369ac66c6d3f3b1e627d6923e67439cdd71921cfe7e700979ed249dea37d647d37ac518202fc8f8312d23c3221917d3216102029cf4b80171e58a9e6119dbd42774197aa9fe0adc763833bfd65cf9e41997cfd2a04092f4ac3aed9be4e93d704fe3fde0dcd535f935e78aead3d9663a1674701f90d8bd5d995bd218fe2c57a6cd5906fd213bd6b8f0999dd692e4e95fa885771bf72373a6b9c6bc981539d6e21031a9222fa869a626ecf0f5ac25ffcf6059698a3bc7725a9e1a118bb2b817acc90c78e250916b4b595008da88031603be5eabdce72b2603a6b485766f1db6815857dad02c8d6812f8415a07af02c8971303d27671c4fb1c84246cfa92e12fea3e92635e2bf2cc97945f78ef89bb93f41ef761f9742b8e1f86f85c5ad698e051fc4a96fec7781533ae08c9f4c083e4953297ded866d54b874f9eb84992ab744301eeea349773ee81b5bbe1d9bbf25704ee63ad95b248bfde4e8625396faf5857f00dd192047c950c4c0744e1049d47cda2e952b3ff0c57aef45d179cc5fb08eb4715e189a708c53dc17967cd02db535a00a762a59586af53e171f3f62e35028767a59e9df79edb80c660bed06f32e624881c51fd2e4e237796636ed810e3d1b6d22b6d516ea5649fbb7bde9d824a9cd7a9c6a52d11c9608473afdcb5067df2f42715d22684c2ecc511193ba6e478d3cdd624d0b7f89ec2f509c5c20f8769f2c9d6f3618fc25dd0ab87d75a7f1e4427819db569203b24d763d933b36a0c3e3d925252a01fb00cbcbd6014bf789843049d70f6e2e861f22a3b628825371d714ff839a1f238c6260d753a8fdf35daebf76bf077b293059d17f626219c093399b665fd6fec8d8d504f5d87774e396ea57c978ca66c928e7c4d2ea49e9c52d71ded65fe2b03606e01fef0d78960f5bc06008ab60185797516164f5e5d8a9670847172ec51cb53f0c198b99b24cbed5b942fcffd2781aa42012fc2c3fcfb66a853df07287298fabdf5a92252d7f392c5c656d1d276d5c78db3e64375eb7325e70272bd431c10a71e34acb58f215c987313123ac67e1633a0a4c62bd4a76666b526f8efbcacb70baa05100db08f8b40ec10fe5559abb791336a6bf660cf69ab7b17ce2905a2e07ad9dd8f755770e42eb93657cc0c9e3e42be6b342dcab1d166d18b6ee3aead418736245796f4841bd43309cb194fc40eca9a2dbb5c42fd521b30c857ca280a4ea3377024ed182776b411e1d4a7939bcdbb6f286dec9a10504b11751526f2786c71665753e222d9a960ae74f0f198a3ef7795f1c879108950891c082394ae462f544b308110a529184c91299ca19f6af8a1b700576c46c6c72788f80f9dbc120bde4e9228e33f7447da42865412485f5dac3aeaa740bd5748d900db45ac9c5a9671e8f2a7f42279bcbc3b01a3de040f9309a9f71480e4c94a001c18424bdf8e33b1c2e8aaae2e77eb54f321f86f52ea3d3c8cdc3bc74d8b4f2f334591e5e63b781034da9d7b941d5827037dee40c58dc0d74c00996e582bccc8dec9c12550d0d80fe75efd06f3c887b4d46be1fcdc48a24c6fea99f0b519576f773b3c649ffce81322dc45cade07fe80fe727fedf7c9661e7110d9f8b4d25385a510802e33fe0cccafd90421b36a311dda2814beb88e90cff819b5e107e555ade0c0190cbd72da9ec6dbb4b6045da3de00035cc98ee6a6d35d4bda340ed0e23aa2e0837a8ce9c60c106ad3f24160db54088a94a8fb800cbb294da37c3a136dde73f84be222d1438704fcfe611af25c224c3468c77ee95709e4859e342a2b6cebcc5ddeee2fb7f7a21d18707cd1a08aab437745cf99610a22079a46d65f9773e524a7a8f9040c46ffe123e6c005fa0b51a6e9c80bdba58aa60ae229dd3ad6077d055c569d405cb5c2c9c5ddb5384d32d7bf024c16a6157d0876f3b3d35c658c05f6f254354517c8cd6f74a1d6d938b58024376e7a894f45bbd455713324d4960271e02e6bb2cbf4622a96ffaf893af80d4f1f8ba215a179529b6dee973491aa04fedc51baf9e7cd1383c3339998d27eb1c8214027abba8c4f44c38859fc2b3c80061d4a8d8b09b764138fda4fa88dbe1007c31865ebf9031458fceaf5fc0fc2061049855e069047d65233701c38cb3aca869b60f736fb524450606315b4cddef9b5d79ec6cd29f2ecb8748199ca297933dd00d6561d200e41c3949d23a6d3e7fd5f4e03327f155d09ceb98b42bd06fdde73a9d50d1f059bbf1b00059a52c0716385233f4b6d00cadffd88f9a1b2023d3cd2ece986bd2a9efecf4918f438f20fdecfedbf5c477ad0b1dd59592129a9bbd18340804d67fd6c9378c663cbda1717c188041fff7ec21331e6a1265b845bfe3efd4b35dbbffca91f700e528ba67e2dae461ccd9f7394e6d291f6edbb729788c7b97376bc15b6b8f57d9781ae3eb5c617928e34d6d661e01667d11bcb44eac01e404fdf6919aa30b7d9e86dbe034c557ff5df02101797836fe0b63d05abd448b51a46bc6d2c4594c91d77734584a07059d895a1e5a7c43551951cddf4e7d2970eafdeaa22de2759bbc0cb2eaf33608b37c33890c217669a6ded8b5b03b9641f8d999e3544afbde4025c6a1c85affb2a9c4e3e5b3fd70d1717627840cc711766d4300bca2d0177e83f68cab9535adeb2e9c21d6da977163fe09c7a3bb9af5af1553b8df55683e3cab54e738182b499d8b2c76c44b96810794be3e78855d4ad43a2e4bb235b9790002695eb01ae5d9af099d6917cca3a8a57dbd166d32b818cc22ff419f096ccfb2ce5d797c630b6809ee1783372666ef25ff2fd6e98952688115ec200c9b28493b5858c002bb44fb9382c14a8c21865d6f633c075453d441da5d1d7263b196ee81e881bcabea389c350fc4487f84acfb6cf10b301558e5acbf41bbbe0b145dc66dc600f4dfb79052d7db480576197fd002f0c5d996602eb01808ee8d6c8c26d9e739fef007ecf426612f7408daa6a8e41aaa918b3e335755cdfbdd66eee09930d88aa339894f0b1ebb5370d914f4ce3f9d6598cc759807a3c762b1d1f9da5dd226021656cfa97e45cd8f65008acb9aea716a0127a359e6a3ec859156f5401bc7bbe780cf3fc1f4a0802b7b05a6f2b4ec8001f521303c9d5c25aba67e4adb7a187d81082d43611639ba6ed971cf33c34db0719c44850ad4d65b1d18d0607323cc561ad7f7424f8c1764a2a8cc6c546f15eb041d17088a321a74240ed5a0d0bdeb2df814b5a454fd42670d0f9ce71e624d1800a1222286602f5a06b01370b9f17ee964e8f0d03812c3a9f7376ba75941fd1fae4fee3bc4061df30b271439f7678b74a67b2e30c372a114aa47c014cdb72be8565002bc0ecbed297d9b606453c61db506f4fa2bbdc5e48f6d682496416ae15a40cbe60b2a57a6e2a2337d5792b73994cc7bd929edb5950187c21e957f02265fb3e92392e0535008c418f57c7fe1e9f93f3a55921ce8ba54417c36794d2803def9d240b8fa67f4822972503ae1ed34cca856f5cf12065e6fe3bff4aa74847ec058adc5e43e3a9938196814f87f8223f54ffa414697b8863ab5b2e191bcbda2eb19a23b8076754712de22cb202249bc5d7c4cc26073a63c9dfe7e8494ec7b744883db29f368bf06f8bfc9fc1992e909ea7774ff856d778780084a651cce68febfe07d17a5ddc1dfd20385304b970b1285879b811e4fd370cb193f0d92282a473976b38dfe9c23d988f37dee1ef957de17f6e9ce91df64fd667a85035cc369579c8fa314666f8cfd02d7dabefa7f7475860a857048e6cf38397343cfe467ce37d2717533451ca45229f7c32847f30bdfbc0c98efa92bbf292d4ba1fa90db00eec1645273036c14e0e39a61b56d1bc3249f4349d11e78dad7284cc80bcae9a88453b2625b082784af1909dcecd4bf454d19f9c1a00e93c84a13bce08af9576e026ea1cc73bc159bc8b48d5b56e4a93962117427e3e145328ad7e4100084eef96ffc7bb94388fd02329a6c067906a045696efea423fe56c0a05a0c81f4d576f2d3e4865054d2a961f5b5ee06dfa06b146c1add60e7cf8e1993867c5bee53b700360e0bba2d03df5f1aeb5d83101cd34c72fc5982b7bbb5e66b427bc5766679cca162511e568468a4a726ffa493e0a09684fcbc331b2a92ce307552e7157683fe1bc8a2a558bc0cd9a74d1e92ef569a99a9024c9a9c84ee7aeb094aea52eb9e3dad284476ba1292a61f480075afe488fee9f653e0c1329191a4e5a9f6b62e92315e6a80bb04f0c4b219ffed7458f4346c4cc0172c139a43bc61f1b732c8a5278336769e99bfbd68a69c5a2fb774ed648cdb49e1a71925cca32c15bec710b924db3d34e63a72e04729b1410d678a37e1069d77f8785b3bf3ed0c3904dd16d253e07c8da0c6a5c329e45f9cceac233a73819b33a6c9614581430a45d29a63597a3bfa671364e0cdd67eae53bbadc65de1a5db6631bfb4a93f4c2c2170297eb46294bfac883dac6615a0a64e1c93fb0bf51b67f6e284dbd1f2b17cb626b0ef4dd391d442477990f6aff2507539854ba9758b47cd01405e23651902ea3c583f8b95dd4013983f15e56d1bb3b159ac3ca02a9c81d1d61ac6095f2dc5a6f948f5d56f1e0b05c3b7c19c15c4d7e8c5305858dcf71feca83b99eaa795cdd3a7a999952afd3e020f0b1f816f94c503c638273953442805c1cb622e06f822578ca2b3328e21e0f1c660b4fd453484eba2ec30ac562aa50367f1a63c44fecd1cf05ccc146333f9b92ee10a795d4b1c86a175fb17af519dd0e3d5ef7e2950d5081dfe6ba371da92448fb3a85b808296be0dfb10e3246218537da0c45b7c9642055ab1194780bfb1793301160c7681f97a2ca5f8797e051f747c7aae3b07771cfaee9d5d685789614061f1ddf507ef4da2489c3a3f84e1b81b2a8a3486909c6cce22e9bf6ebfe1a78160ab847dfb759b1da7085e9f009fee30cdf9dea5e0ad3a6b3ebec7c1806f8d247958930b19baead59110ec00b8e6c69baeee34febf518cb88a43fec962da1a12f42092092a6742b150dc6dca765f4a441592bffaf5306c9d4da1ebee2dcb5631192a0adef623ab992b95696400b28114490d060149f3e102d1ff1799a8e94a6a2f9f83f245972e75696366381e8673f19486917c8442f3cf5c8608309cddfda680a683addf6ad22d536deead9a8329c43c25e9d16ec4abec375d481dd14e77cb526894778c45a0aa31dc882b3b52aa768a14956acf221042aaae5fc60fe5c1c2ee0d3db813d09ed24bd7bd1ac89bfeedc6781dad6f042bb7fc9ed3951e4ae2749cbc07d83db2d68ee88713e30a052e13387c0a041610023477572ef0c524e2c8a49d783fd29450c7511414ecc93941cf2845e3d68bf1a0a26101f741cd1bcaa54dd91a559a1d1363cfa5489eccdf3f08ddfe308b974d0ae703c3b7efffd8a1f5e09875835cf6274ca8623661dd9e31a8a0cd7a8b02cd4aeda5c6f56221036f9fa5010f2fe2c694682474669ec21e1e96b9da473d9418669a8f323902fb443d8be43d72c3f23662a18d0360bb44080eba704e220b14d769d8082e0c37574112a1e57326a555ee9add2936ad222b0409fc0c51905c4cb1588e59e2f469fba5f6c9efccb13fd39544064236d4393fb91844d70a1b99024f3da4cb777b00b016f1eb4c709df733d366031e2b5d6a48d0427a17a24a28ff581f01a41529643e3c0cef428672373f520abc053cd11c0df090861541a5720b75078f2ce385fd6dc14ebd1a37b666bdb737194191b6b657c20850190db5dfed4efc35ad0a1734054ac03d1caaf3eb76adf602195397a2eeeeb5c16b3101d86f8a06db6db2676fcd8268588f08b03032da4e5fd4aaf9479a1b9cbf52d312c357d12d69b501fe178e0e50f4bd78769cb8e64718bbbd660d23f1c733181c6e096ecd7ac8cd226395109980faec23cc97d27ae91516b5655c501a3d0603339f137c24f4defd3b1e4f1bc9fe5365303a89463e3d3f2db97f9cea3b061e5c402d48497ea4948d75b8af7746d4e570c848daddd94a7c47f23a35b9ff2b656a6d9e0f9c8490d3c4f7b81cebb90a9a54137ff35ff418b5ad882190850bb3c277e07cd23fddc281eceaa6fe41caa6595b38663a9436b0d3a8a2b913160aa61cc9c14ef8f83242874b750bbc3f20b209cb608618444819bc7e688c2734a20c5da9399de9902b16ce29c16dc0ae643594edb265f636d15b5a77032135a67bee6c5f70f00ccf6f483dd0f55771553d38e109578d3a337a7f93dd9b00a3353e5331338dcfcb7ca7e0bb873a4e37fe54c2d86776864e03eb8c51f657b2daa99344946db8c28d617ae43295b8dc7a6f50db614e38d6986ecb8a7b0c60be43ca21c87d541b52bb44915e89439688a271db8b6408c507823cc282f965deac455f91e7d431db06bbee9355a8d9fdda17cc9adbc9db44a172347a7bb349f80547328aa9814c2367f62729d2779f145b424837d27810dd2ad21f987d9002d64fb309b5176ced19fcd4b6fbdcdddc049289ad5a795aeeba2c09c5c7907bd7a115f0adba387e5ce54dbe65b07bc5daf40855c2aad655bf002e7feeadbd590f81dc66e640159de0b0e21afaf1b714df0d430f50d0f452c9dab99c62f20b13f39d2e7d7befb4b2c3a244513e6599b33476cd1d1f73d83a566e54c8e83f8391cb1e929d36711fa64be2faccf9b5288a21582ee9f73c8cf3ace4876b4a3c2bbbdf2b912edd6b5e5fc1998cf2f3886d81eddbecd9a0d317671365daed4e900d7976edb298398877c76dee832af3876e81e2269157e1df817cd519d6b6bde331b4d98c7d43126d74c5960a65dec35a35704662a5ca8409d21c3927022e5edf9d52512f7a9830c55c408514bd7529a8ca3862b6ae9a4ff8132e86b7e6befb2a5b3d24fe32fd5a36382283351057ee1933bf7e4b362e0b544c9a05f30bcc5a6c735b9a222cdb5a680f52d695e8ebda567aefe249c4f080c78348fe01f8c2444eeb96fdc3f09bf605e9b8c65bcc9bb407a7aed5d7e14775f7c220b80935068177cdd5930807a3f7b2cf3c9db045cf46d4c0e6a01d139bb38c10097d344b02d9912100a61af85d2419c7c6a1bbdaed5663cf7e8c44e555032f76ee93a9c11c1441edfe7c214b2f47e2dc3e1ca5135ead5fdb5edf74dcdb2b88fab443911fbef9313bb95d44898326b5138c5495788a136eed6ee1d48296f33ea744d514ee4f5209121893ab39c08b38769892b492d94f380c96dc8d5eee25fa82869159b537e5707b03657fe34e3c160c4a1d29e8b42b6640be16c6f84c87639562bf89fcdd7b0f7c621ae80f9501f5dd8b3f1d02816da091ced8a8d916e322e7c1a6cb4cabdb8e201dfb0d46fda47ef2413eb869a9de8d08ae6a5b88b6ffde80898f482b08cf946c1d50170feeeb5852566321a49546d6ac4614068d92a34069a4c228245c1dc96a48612df3d0a02b05b0f6813bac6d23867e1ed2711499efa9f88792bc108ed721877578790b89a114393fffb7551a1b9c3fcfe6bba2a894486f26ae7d8aea6399c3d52029076f6086d785c49ffa1766f86f716bf3a697d74ff0e6a5ee33f918dbe2ca4c8b2c1910637412f87b022aada2f84a88a2872f5fc562119f9773829beece9a2428e3515ab4f72cc1d4d51c98a0c0a3664fad38d5b2afedc301b34f97da6d8dd18792dde680481c4a30dbb4cefcb1c32c82807ae01ebec33ab35926b2a8a9ed7b30a3865bcf4f2ab91a7cbcf8beb2f403c368bc291277da2c35cfd0945f680049ac7c1461a9f49d371720fabdecaa7c139bcdc85895c27cc0107b15e60794106482874fa2171fbfebb12f38c69083a48391176414b087ef1ecbc7bc50a91463f43a2affc320b8d6fe6f113cbab124b47c2eae59056aae7c6fee1a8f794bd041205dcfeb6fcda72b97d92ece425875f425f8fa6ed4cb1f6bdb02db540af8b0f41072dddc4c25d406df78eeb8068c5eaba779646d29dbc304898e74d4aab59cc3ed01bc9a023027aefed3bac1ad55c7232939521caba3808fa21a4cd7028c7cf5df92d3ed3af6896d1b73f8b0e0bb98a2d4bf2d45032a3f534d2ecb25ba8438d30dae8c9d9c80e0895ede124e46c7b971cf935eaed978e999b353598579a5b8a5a0dc4e71cfd5164bf09e6e63888012d8433d4b20d49664a9fb105ec0e7f8e5b3bfa582327cfd11fbf93b76b74def8c1e58455e629c8b9acb4eee429ead84a54493908ea84a01bbe1a03b90113e1a0d8d3534fe3eda5f7147d4746de77352e1a14425db9be2e8fa450e28263a020b7e6b0392927a361bd364b42a4863e0688213f9ecd1d3ebbab86e6b899ffb42cac801e9e584df57c6a76b179839da9dd06ee0da6ed8170e7fdca5dee7631aa65dd19710b73b35e668d5a42447a91352928f5010325931f3404e328fc9da5fd5ce8375a08f1f448a0a445704be01d7a6b4cbadc3c0bae74105f65da7b76c947825c5e2c0f698d3ecb2deccde85a7ee41400c3d1aa16502a0d4bc6e4e39817f36687e2898730eacb36b8a4bdb250520cc3d394a3e5cc04abab788acabd881eab8a1988d518ee995883a301b23258135efb43a1a5779358634b54260e71bd49ba6d7fa5a13cf91571329b985dd0d85f451acf7f58c812b80b9d6d2d2298be25aa7138cd605bf6fbafb3f0bd1326678269b2d572505fb0afeef23204a9aae13f1635c364b50e5e985e9840f4749b6c293744045f1eb6f39c97ea31410620fd02683bae0ff1bc968828401ad2e4b88cb0385eba6a0666522a90f5f976ba8337fc3af35723b8ff4557775bdced05f61fa3e7638637b9d662c654a5d8fa594a48a8e8ed6d447d4ed162d63c05873aecbb90bee2f64cab9f9bfc7d6a7e60450e95750c9a4ef6b113c156cf71335605f4d703495d791d5e1a743955c4582d1018a50d007a7a9ef6f6ab8fbd9bd4bdf9b70857f1438d2e33ef72098833d4d53d20fdbd8f1e8eb1a15fe69a4b0c684378aba61b5135a103df871bf98f4992dd4e901622e2f6e9e362b808f2d2437c6b253b56c9e3946312c369d865d56610c9a6ee84c9b6842a889db79915fdf17fb961fe90e012f031ff62004deae529bd1ee8a4a01e7446af35c4b70d902866e0a2c15ddb0a79d99c0e3df2d30549051641864b1e9da6df065f6a3edcedec445dbbc5e19b8592911fe3d9ba0e218b78aad6026a46c3c88dbe8a10c78d8ba3ea3568e212590dd7e72c04a7be65ec06f4f930ef5acf2d4403463a3ae9a5f609f9063e9878a9efb42c4ed54a8860a1462a70169fc50e36b3a02bd45b0dacb968d14ea17b8cf4c7d1e0f6fa14a0850de369adcba6018a7c9214de0cf3ac18525b038d35edb7c0129b95c2d7988f597e493300d494c8ca6b69b1eb38976d6f3d5b2030607181924c9bcb20f268ad446ef22727c5ee63548fa507c1eba8aa17e6d387e6b59982f1f23ce1afdef792dc7c78a671141569af65cea6f09ac9eaa8eb7edafeb9e83455900989bdf1b7d8981347417b539c54316a60eff5ca5db7a8c7976a06c1c01e43a27c13d9df3bd6edcfe888b4e905bb9129e6635ffee6898de7c7e59d71d38932a62f20820bf626740ec6dcac58c3f83500d9a18dbcf6e8cee88abe015e753da9177d3054ec793f915c8ccdc03769a293aba52b0d68f15df2365a53f0dc27841d72f0b3b105279aec3584ccaef969334366f3506d9788418e4da157ea6f8d051645243b34fa93b0db265969b7033532baef2eb21cdd0e4bf9bb0a3a3cad08291a1a82273f115039e4bb6515770960196090c1d0a98e05c5cafaff6b86091b68af4f85185dfdcf6eaf4152607d608c6feaf8573c724f01578889fb7c44f8496c2c60c77f002463e781648bdb28acfcae9082594625748379a8e488dd081f147c380aa3dbd419d03d01a18bd60733334e27cb429e21b2fe94b297221e3dd20f415889b4bf383276c7b664dcf197b7e2c6e73f9d95057ef5312c1e41b054b58c33e48a7489ab22ea20064af9ae689f6dc1755bd4db3f3ce9e77be67223eb3e2dcc82695ddbbb5f8cde77b05c7ce86e56a648ac82c13d615f20288c3de97fd06683b8c2fb15d74cd13b91219fdda5891fedd7ae21f79ea0b763c87c0f817120c8f1de13b6d88d7f79ffd93b1e511e843c10b8dd3ceeb16fa53ac232518c7d83c78f88d82b844c6bd973b6596267b1e42da752c62e0744228b0dbc5bce5ad38de4c53f8cc3b9a296624fcf0906920e2d872a4bd59fe1956deb64fd34b08d31a3932b78ae19d91e83b6dc69c6756091d4bb11eaa7ecfe43b648d92b79b2841679653abab888732eb849b24d1e2943db5e4be6d599afcdcd7025c2cd774eefe053dd7b7c38ba1841a89d59bf7caccc494ebc21834742c0913004ba39d9e0212de76e5633ab0928f5e9144b576e7a5a7a0a8e4a8a1d60874d8a5848170e00b85d989a53878c0035b9de50ff9b601c8edc3624da40a8637240c3b2c4db703b25fddbd40bde5829bf1c1ce60ef20e0dc9ee4632494d64d622198086e90dcd09ea9d45cde7be9c00358b0925c201a4b8ac3ebf192016df33174655570d1460c776ba5aca6f8ed40e29854792d130d0c7a62bf55bf7237404b245a695a985d82e7ab38f6c1be57cbe1ba93cdfb5062e953864ad7d26f9cfc59395e8e556e89f99473758157559129a8c457d4ed607fabbe3473bef48004ee02bc5b0a89d9323826ca44978f430122dec187d82ccace1a627622544c2ed7dfefb438261d63c00846b98d7e9112117e2bb60b1fdf16b5fde8533fdf03007d97c829dd2a39949b319c46387f8649280439adcbf12fcd9e1df7d4b3a8407f5ede3424325e07f32b7bcf7c2de29671ad7dadadae984476f04338ff64f72c31e27cdbab1e09ac08d9ae886054efe8110ab232bf9e5b4599133552de3b14efa0723866c98c75535b0b916efc95eea5b1a6686eea83815bbf6fbcee792046b05474db7d8361e822752aed2a57d926f4dae96a09364353ad6f6c9fb4d6fcd697d4522dc7e386ab41dd9f8a637906e0fe123b7facabc719643172a84bffb50ccda872f6edf0e306d91bd130c26b0664eae4046eff52f71ba78de99d5cfc35307a583efe96b2604012b827a7ee34bffae1d5ac00a932bba09f0c39478e"),
    %% SHAKE256 VariableOut
    hexstr2bin("3e20cf32669fa3fd6e94e519b52a1dba33cd1f3a6947975e9829e4db326d2a18"),
    hexstr2bin("6ae23f058f0f2264a18cd609acc26dd4dbc00f5c3ee9e13ecaea2bb5a2f0bb6b"),
    hexstr2bin("e3ef127eadfafaf40408cebb28705df30b68d99dfa1893507ef3062d85461715"),
    hexstr2bin("afc9ef4e2e46c719120b68a65aa872273d0873fc6ea353859ff6f034443005e6"),
    hexstr2bin("8d8001e2c096f1b88e7c9224a086efd4797fbf74a8033a2d422a2b6b8f6747e4")
   ],
   [%% SHAKE256 ShortMsg
    hexstr2bin("46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762f"),
    hexstr2bin("aabb07488ff9edd05d6a603b7791b60a16d45093608f1badc0c9cc9a9154f215"),
    hexstr2bin("8e2df9d379bb034aee064e965f960ebb418a9bb535025fb96427f678cf207877"),
    hexstr2bin("9510ff5231813a865918badd0011f05915364165492ef17b85929a63e4951589"),
    hexstr2bin("46293a63c235750d58a24edca5ba637b96cae74325c6c8122c4155c0d15805e6"),
    %% SHAKE256 LongMsg
    hexstr2bin("2bac5716803a9cda8f9e84365ab0a681327b5ba34fdedfb1c12e6e807f45284b"),
    hexstr2bin("51b560fe5c3cc4c9e457e65f15f1b1619d18dbac916ca83a67a4d022301d5229"),
    %% SHAKE256 VariableOut
    hexstr2bin("3389aea66244b91428f089"),
    hexstr2bin("b9b92544fb25cfe4ec6fe437d8da2bbe00f7bdaface3de97b8775a44d753c3adca3f7c6f183cc864"),
    hexstr2bin("7314002948c057006d4fc21e3e19c258fb5bdd57728fe93c9c6ef265b6d9f559ca73da32c427e135ba0db900d9003b19c9cf116f542a760418b1a435ac75ed5ab4ef151808c3849c3bce11c3cd285dd75e5c9fd0a0b32a89640a68e6e5b270f966f33911cfdffd03488b52b4c7fd1b2219de133e77519c426a63b9d8afac2ccab273ebd23765616b04446d6ac403f46ac0c147eda629eb7583c8bd00dc7c30fcd6711b36f99f80ac"),
    hexstr2bin("45c65255731e3679b4662f55b02bc5d1c8038a1d778fe91144a5c7d3a286c78c54f52135134a3c6a19a9e6e546de21b2e8a7e280290709f0e482a51bffa95137a381268d10195862818309b2a4954c656d1725c7ad1a29973162832d62afd538cf74e1b70d1775a9f77dc7c7380ea034f5b1869af46c1c26bce29e1980f0de9e55543e7eda19a56453c8b7d58a28ad7a33bc243c7242ffda5409cfd8f8ffd4b350c6d0023f27f93e9eb46a871367706170074d8a2080f0a8b68b8fc6b14b8b4da256e9e64dcb7771640e992eea2334e641"),
    hexstr2bin("2e975f6a8a14f0704d51b13667d8195c219f71e6345696c49fa4b9d08e9225d3d39393425152c97e71dd24601c11abcfa0f12f53c680bd3ae757b8134a9c10d429615869217fdd5885c4db174985703a6d6de94a667eac3023443a8337ae1bc601b76d7d38ec3c34463105f0d3949d78e562a039e4469548b609395de5a4fd43c46ca9fd6ee29ada5efc07d84d553249450dab4a49c483ded250c9338f85cd937ae66bb436f3b4026e859fda1ca571432f3bfc09e7c03ca4d183b741111ca0483d0edabc03feb23b17ee48e844ba2408d9dcfd0139d2e8c7310125aee801c61ab7900d1efc47c078281766f361c5e6111346235e1dc38325666c")
   ],
   [%% SHAKE256 ShortMsg
    256,
    256,
    256,
    256,
    256,
    %% SHAKE256 LongMsg
    256,
    256,
    %% SHAKE256 VariableOut
    88,
    320,
    1344,
    1672,
    2000
   ]
  }.

%%% http://www.wolfgang-ehrhardt.de/hmac-sha3-testvectors.html

hmac_sha3(Type) ->
    N = case Type of
            sha3_224 -> 1;
            sha3_256 -> 2;
            sha3_384 -> 3;
            sha3_512 -> 4
        end,
    [{hmac, Type, hexstr2bin(Key), hexstr2bin(Data), hexstr2bin(element(N,Hmacs))} 
     || {Key,Data,Hmacs} <- hmac_sha3_data()].

hmac_sha3_data() ->    
    [
     {"0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b 0b0b0b0b",
      "4869205468657265",
      {"3b16546bbc7be2706a031dcafd56373d 9884367641d8c59af3c860f7",
       "ba85192310dffa96e2a3a40e69774351 140bb7185e1202cdcc917589f95e16bb",
       "68d2dcf7fd4ddd0a2240c8a437305f61 fb7334cfb5d0226e1bc27dc10a2e723a 20d370b47743130e26ac7e3d532886bd",
       "eb3fbd4b2eaab8f5c504bd3a41465aac ec15770a7cabac531e482f860b5ec7ba 47ccb2c6f2afce8f88d22b6dc61380f2 3a668fd3888bb80537c0a0b86407689e"
      }},

     {"4a656665",
      "7768617420646f2079612077616e7420 666f72206e6f7468696e673f",
      {"7fdb8dd88bd2f60d1b798634ad386811 c2cfc85bfaf5d52bbace5e66",
       "c7d4072e788877ae3596bbb0da73b887 c9171f93095b294ae857fbe2645e1ba5",
       "f1101f8cbf9766fd6764d2ed61903f21 ca9b18f57cf3e1a23ca13508a93243ce 48c045dc007f26a21b3f5e0e9df4c20a",
       "5a4bfeab6166427c7a3647b747292b83 84537cdb89afb3bf5665e4c5e709350b 287baec921fd7ca0ee7a0c31d022a95e 1fc92ba9d77df883960275beb4e62024"
       }},

     {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaa",
      "dddddddddddddddddddddddddddddddd dddddddddddddddddddddddddddddddd dddddddddddddddddddddddddddddddd dddd",
      {"676cfc7d16153638780390692be142d2 df7ce924b909c0c08dbfdc1a",
       "84ec79124a27107865cedd8bd82da996 5e5ed8c37b0ac98005a7f39ed58a4207",
       "275cd0e661bb8b151c64d288f1f782fb 91a8abd56858d72babb2d476f0458373 b41b6ab5bf174bec422e53fc3135ac6e",
       "309e99f9ec075ec6c6d475eda1180687 fcf1531195802a99b5677449a8625182 851cb332afb6a89c411325fbcbcd42af cb7b6e5aab7ea42c660f97fd8584bf03"
       }},

     {"0102030405060708090a0b0c0d0e0f10 111213141516171819",
      "cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd cdcd",
      {"a9d7685a19c4e0dbd9df2556cc8a7d2a 7733b67625ce594c78270eeb",
       "57366a45e2305321a4bc5aa5fe2ef8a9 21f6af8273d7fe7be6cfedb3f0aea6d7",
       "3a5d7a879702c086bc96d1dd8aa15d9c 46446b95521311c606fdc4e308f4b984 da2d0f9449b3ba8425ec7fb8c31bc136",
       "b27eab1d6e8d87461c29f7f5739dd58e 98aa35f8e823ad38c5492a2088fa0281 993bbfff9a0e9c6bf121ae9ec9bb09d8 4a5ebac817182ea974673fb133ca0d1d"
       }},

     %% {"0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c 0c0c0c0c",
     %%  "546573742057697468205472756e6361 74696f6e",
     %%  {"49fdd3abd005ebb8ae63fea946d1883c",
     %%   "6e02c64537fb118057abb7fb66a23b3c",
     %%   "47c51ace1ffacffd7494724682615783",
     %%   "0fa7475948f43f48ca0516671e18978c"
     %%   }},

     {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaa",
      "54657374205573696e67204c61726765 72205468616e20426c6f636b2d53697a 65204b6579202d2048617368204b6579 204669727374",
      {"b4a1f04c00287a9b7f6075b313d279b8 33bc8f75124352d05fb9995f",
       "ed73a374b96c005235f948032f09674a 58c0ce555cfc1f223b02356560312c3b",
       "0fc19513bf6bd878037016706a0e57bc 528139836b9a42c3d419e498e0e1fb96 16fd669138d33a1105e07c72b6953bcc",
       "00f751a9e50695b090ed6911a4b65524 951cdc15a73a5d58bb55215ea2cd839a c79d2b44a39bafab27e83fde9e11f634 0b11d991b1b91bf2eee7fc872426c3a4"
       }},

     {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaa",
      "54657374205573696e67204c61726765 72205468616e20426c6f636b2d53697a 65204b6579202d2048617368204b6579 204669727374",
      {
       "b96d730c148c2daad8649d83defaa371 9738d34775397b7571c38515",
       "a6072f86de52b38bb349fe84cd6d97fb 6a37c4c0f62aae93981193a7229d3467",
       "713dff0302c85086ec5ad0768dd65a13 ddd79068d8d4c6212b712e4164944911 1480230044185a99103ed82004ddbfcc",
       "b14835c819a290efb010ace6d8568dc6 b84de60bc49b004c3b13eda763589451 e5dd74292884d1bdce64e6b919dd61dc 9c56a282a81c0bd14f1f365b49b83a5b"
      }},

     {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaa",
      "54686973206973206120746573742075 73696e672061206c6172676572207468 616e20626c6f636b2d73697a65206b65 7920616e642061206c61726765722074 68616e20626c6f636b2d73697a652064 6174612e20546865206b6579206e6565 647320746f2062652068617368656420 6265666f7265206265696e6720757365 642062792074686520484d414320616c 676f726974686d2e",
      {
       "05d8cd6d00faea8d1eb68ade28730bbd 3cbab6929f0a086b29cd62a0",
       "65c5b06d4c3de32a7aef8763261e49ad b6e2293ec8e7c61e8de61701fc63e123",
       "026fdf6b50741e373899c9f7d5406d4e b09fc6665636fc1a530029ddf5cf3ca5 a900edce01f5f61e2f408cdf2fd3e7e8",
       "38a456a004bd10d32c9ab83366841128 62c3db61adcca31829355eaf46fd5c73 d06a1f0d13fec9a652fb3811b577b1b1 d1b9789f97ae5b83c6f44dfcf1d67eba"
       }},

     {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaa",
      "54686973206973206120746573742075 73696e672061206c6172676572207468 616e20626c6f636b2d73697a65206b65 7920616e642061206c61726765722074 68616e20626c6f636b2d73697a652064 6174612e20546865206b6579206e6565 647320746f2062652068617368656420 6265666f7265206265696e6720757365 642062792074686520484d414320616c 676f726974686d2e",
      {
       "c79c9b093424e588a9878bbcb089e018 270096e9b4b1a9e8220c866a",
       "e6a36d9b915f86a093cac7d110e9e04c f1d6100d30475509c2475f571b758b5a",
       "cad18a8ff6c4cc3ad487b95f9769e9b6 1c062aefd6952569e6e6421897054cfc 70b5fdc6605c18457112fc6aaad45585",
       "dc030ee7887034f32cf402df34622f31 1f3e6cf04860c6bbd7fa488674782b46 59fdbdf3fd877852885cfe6e22185fe7 b2ee952043629bc9d5f3298a41d02c66"
       }}
    %%,

    %%  {"4a656665",
    %%   "'11001' or LSB 13 or MSB c8",
    %%   {
     %% "5f8c0ea7fafecd0c3463aad09742cece  b142fe0ab6f4539438c59de8",
     %% "ec8222773fac68b3d3dcb182aec8b050  7ace4448d20a1147e682118da4e3f44c",
     %% "21fbd3bf3ebba3cfc9ef64c0591c92c5  acb265e92d8761d1f91a52a103a6c796  94cfd67a9a2ac1324f02fea63b81effc",
     %% "27f9388c1567ef4ef200602a6cf871d6  8a6fb048d4737ac4418a2f021289d13d  1fd1120fecb9cf964c5b117ab5b11c61  4b2da39dadd51f2f5e22aaccec7d576e"
     %%   }}
    ].



rfc_4634_test1() ->
    <<"abc">>.
rfc_4634_test2_1() ->
    <<"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq">>.
rfc_4634_test2_2a() ->
    <<"abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn">>.
rfc_4634_test2_2b() ->
    <<"hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu">>.
rfc_4634_test2() ->
    A2 =rfc_4634_test2_2a(),
    B2 = rfc_4634_test2_2b(),
    <<A2/binary, B2/binary>>.
 
rfc_4634_sha_digests()->
     [hexstr2bin("A9993E364706816ABA3E25717850C26C9CD0D89D"),
      hexstr2bin("84983E441C3BD26EBAAE4AA1F95129E5E54670F1")].
rfc_4634_sha224_digests() ->
     [hexstr2bin("23097D223405D8228642A477BDA255B32AADBCE4BDA0B3F7E36C9DA7"),
      hexstr2bin("75388B16512776CC5DBA5DA1FD890150B0C6455CB4F58B1952522525")].
rfc_4634_sha256_digests() ->
    [
     hexstr2bin("BA7816BF8F01CFEA4141"
		"40DE5DAE2223B00361A396177A9CB410FF61F20015AD"),
     hexstr2bin("248D6A61D20638B8"
		"E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1")
    ].
rfc_4634_sha384_digests() ->
    [hexstr2bin("CB00753F45A35E8BB5A03D699AC65007272C32AB0EDED1631A8B605A43FF5BED8086072BA1E7CC2358BAECA134C825A7"),
     hexstr2bin("09330C33F71147E83D192FC782CD1B4753111B173B3B05D22FA08086E3B0F712FCC7C71A557E2DB966C3E9FA91746039")
    ].
rfc_4634_sha512_digests() -> 
    [hexstr2bin("DDAF35A193617ABACC417349AE20413112E6FA4E89A97EA2"
		"0A9EEEE64B55D39A2192992A274FC1A836BA3C23A3FEEBBD"
		"454D4423643CE80E2A9AC94FA54CA49F"),
     hexstr2bin("8E959B75DAE313DA8CF4F72814FC143F8F7779C6EB9F7FA17299AEADB6889018501D289E4900F7E4331B99DEC4B5433AC7D329EEB6DD26545E96E55B874BE909")].

long_msg() ->
    fun() -> lists:duplicate(1000000, $a) end.

%% Passing huge terms (like long_msg/0) through config causes excessive memory
%% consumption and long runtimes in the test server. This results in test_server
%% crash with 'no_answer_from_tc_supervisor' sometimes on some machines.
%% Therefore lazy evaluation when test case has started.
lazy_eval(F) when is_function(F) -> F();
lazy_eval(Lst)  when is_list(Lst) -> lists:map(fun lazy_eval/1, Lst);
lazy_eval(Tpl) when is_tuple(Tpl) -> list_to_tuple(lists:map(fun lazy_eval/1, tuple_to_list(Tpl)));
lazy_eval(Term) -> Term.

long_sha_digest() ->
    hexstr2bin("34aa973c" "d4c4daa4" "f61eeb2b" "dbad2731" "6534016f").

long_sha256_digest() ->
    hexstr2bin("cdc76e5c" "9914fb92" "81a1c7e2" "84d73e67" "f1809a48" "a497200e" "046d39cc" "c7112cd0").

long_sha384_digest() ->
    hexstr2bin("9d0e1809716474cb" "086e834e310a4a1c" "ed149e9c00f24852" "7972cec5704c2a5b"
	       "07b8b3dc38ecc4eb" "ae97ddd87f3d8985").

long_sha512_digest() ->
    hexstr2bin("e718483d0ce76964" "4e2e42c7bc15b463" "8e1f98b13b204428" "5632a803afa973eb"
	       "de0ff244877ea60a" "4cb0432ce577c31b" "eb009c5c2c49aa2e" "4eadb217ad8cc09b").

ripemd160_msgs() ->
    [<<"">>,
     <<"a">>,
     <<"abc">>,
     <<"message digest">>,
     <<"abcdefghijklmnopqrstuvwxyz">>,
     <<"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq">>,
     <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789">>
    ].

ripemd160_digests() ->
    [hexstr2bin("9c1185a5c5e9fc54612808977ee8f548b2258d31"),
     hexstr2bin("0bdc9d2d256b3ee9daae347be6f4dc835a467ffe"),
     hexstr2bin("8eb208f7e05d987a9b044a8e98c6b087f15a0bfc"),
     hexstr2bin("5d0689ef49d2fae572b881b123a85ffa21595f36"),
     hexstr2bin("f71c27109c692c1b56bbdceb5b9d2865b3708dbc"),
     hexstr2bin("12a053384a9c0c88e405a06c27dcf49ada62eb2b"),
     hexstr2bin("b0e20b6e3116640286ed3a87a5713079b21f5189")
    ].

ripemd160_incr_msgs() ->
     [<<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefg">>,<<"hijklmnopqrstuvwxyz0123456789">>].
ripemd160_incr_digest() ->
    hexstr2bin("b0e20b6e3116640286ed3a87a5713079b21f5189").

rfc_2202_md5_keys() ->
    [binary:copy(<<16#0b>>, 16),
     <<"Jefe">>,
     binary:copy(<<16#aa>>, 16),
     list_to_binary(lists:seq(1, 16#19)),
     binary:copy(<<16#0c>>, 16),
     binary:copy(<<16#aa>>, 80),
     binary:copy(<<16#aa>>, 80)].

rfc_2202_sha_keys() ->
    [binary:copy(<<16#0b>>, 20),
     <<"Jefe">>,
     binary:copy(<<16#aa>>, 20),
     list_to_binary(lists:seq(1, 16#19)),
     binary:copy(<<16#0c>>, 20),
     binary:copy(<<16#aa>>, 80),
     binary:copy(<<16#aa>>, 80)].

rfc_2202_msgs()->
    [<<"Hi There">>,
     <<"what do ya want for nothing?">>,
     binary:copy(<<16#dd>>, 50),
     binary:copy(<<16#cd>>, 50),
     <<"Test With Truncation">>,
     <<"Test Using Larger Than Block-Size Key - Hash Key First">>,
     <<"Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data">>
    ].

hmac_key(md5) ->
    [<<"A fine speach">>, <<"by a fine man!">>];
hmac_key(_) ->
    hexstr2bin("00010203101112132021222330313233"
	       "04050607141516172425262734353637"
	       "08090a0b18191a1b28292a2b38393a3b"
	       "0c0d0e0f1c1d1e1f2c2d2e2f3c3d3e3f").
hmac_inc(_) ->
    [<<"Sampl">>, <<"e #1">>].


cmac_key(SubType) ->
    rand:bytes(
      maps:get(key_length, crypto:cipher_info(SubType))).

cmac_inc(_) ->
    [<<"Sampl">>, <<"e #1">>].


%% https://www.cosic.esat.kuleuven.be/nessie/testvectors/
long_hmac_key(Type) when Type == sha384;
			 Type == sha512 ->
    hexstr2bin("00112233445566778899AABBCCDDEEFF"
	       "0123456789ABCDEF0011223344556677"
	       "8899AABBCCDDEEFF0123456789ABCDEF"
	       "00112233445566778899AABBCCDDEEFF");
long_hmac_key(_) ->
    hexstr2bin("0123456789ABCDEF0123456789ABCDEF"
	       "0123456789ABCDEF0123456789ABCDEF"
	       "0123456789ABCDEF0123456789ABCDEF"
	       "0123456789ABCDEF0123456789ABCDEF").
long_hmac(md5) ->
    hexstr2bin("82FDDA30202CB6ACC6F24D4F8A50EB7A");
long_hmac(sha) ->
    hexstr2bin("61D1D0B6459860755FDA892938C23DD401E54A7E");
long_hmac(sha256) ->
    hexstr2bin("50008B8DC7ED3926936347FDC1A01E9D"
	       "5220C6CC4B038B482C0F28A4CD88CA37");
long_hmac(sha384) ->
    hexstr2bin("C1EB08DAFA015833D3FC6B29A387558B"
	       "3F6FA1524AA1A8EB64798D5A76A39D6E"
	       "A1465525342E060EE996277B4FFCDDC9");
long_hmac(sha512) ->
    hexstr2bin("D116BF471AAE1264854F1906025E846A"
	       "61618A965FCA30B695220EA2D6E547E3"
	       "F3B5A4B54E6778928C26D5D3D810498E"
	       "8DF86CB3CC1E9F66A00419B13B6B0C9A").

rfc_2202_hmac_md5() ->
    [
     hexstr2bin("9294727a3638bb1c13f48ef8158bfc9d"),
     hexstr2bin("750c783e6ab0b503eaa86e310a5db738"),
     hexstr2bin("56be34521d144c88dbb8c733f0e8b3f6"),
     hexstr2bin("697eaf0aca3a3aea3a75164746ffaa79"),
     hexstr2bin("56461ef2342edc00f9bab995690efd4c"),
     hexstr2bin("6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"),
     hexstr2bin("6f630fad67cda0ee1fb1f562db3aa53e")
    ].

rfc_2202_hmac_sha() ->
    [
     hexstr2bin("b617318655057264e28bc0b6fb378c8ef146be00"),
     hexstr2bin("effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"),
     hexstr2bin("125d7342b9ac11cd91a39af48aa17b4f63f175d3"),
     hexstr2bin("4c9007f4026250c6bc8414f9bf50c86c2d7235da"),
     hexstr2bin("4c1a03424b55e07fe7f27be1d58bb9324a9a5a04"),
     hexstr2bin("aa4ae5e15272d00e95705637ce8a3b55ed402112"),
     hexstr2bin("e8e99d0f45237d786d6bbaa7965c7808bbff1a91")
    ].


rfc_4231_keys() ->
    [binary:copy(<<16#0b>>, 20),
     <<"Jefe">>,
     binary:copy(<<16#aa>>, 20),
     list_to_binary(lists:seq(1, 16#19)),
     binary:copy(<<16#0c>>, 20),
     binary:copy(<<16#aa>>, 131),
     binary:copy(<<16#aa>>, 131)
    ].
    
rfc_4231_msgs() ->
    [<<"Hi There">>,
     <<"what do ya want for nothing?">>,
     binary:copy(<<16#dd>>, 50),
     binary:copy(<<16#cd>>, 50),
     <<"Test With Truncation">>,
     <<"Test Using Larger Than Block-Size Key - Hash Key First">>,
     <<"This is a test using a larger than block-size key and a larger t",
       "han block-size data. The key needs to be hashed before being use",
       "d by the HMAC algorithm.">>
    ].
    
rfc4231_hmac_sha224() ->
    [hexstr2bin("896fb1128abbdf196832107cd49df33f"
		       "47b4b1169912ba4f53684b22"),
     hexstr2bin("a30e01098bc6dbbf45690f3a7e9e6d0f"
		       "8bbea2a39e6148008fd05e44"),
     hexstr2bin("7fb3cb3588c6c1f6ffa9694d7d6ad264"
		       "9365b0c1f65d69d1ec8333ea"),
     hexstr2bin("6c11506874013cac6a2abc1bb382627c"
		       "ec6a90d86efc012de7afec5a"),
     hexstr2bin("0e2aea68a90c8d37c988bcdb9fca6fa8"),
     hexstr2bin("95e9a0db962095adaebe9b2d6f0dbce2"
		       "d499f112f2d2b7273fa6870e"),
     hexstr2bin("3a854166ac5d9f023f54d517d0b39dbd"
		"946770db9c2b95c9f6f565d1")].
rfc4231_hmac_sha256() ->
    [hexstr2bin("b0344c61d8db38535ca8afceaf0bf12b"
		"881dc200c9833da726e9376c2e32cff7"),
     hexstr2bin("5bdcc146bf60754e6a042426089575c7"
		"5a003f089d2739839dec58b964ec3843"),
     hexstr2bin("773ea91e36800e46854db8ebd09181a7"
		"2959098b3ef8c122d9635514ced565fe"),
    hexstr2bin("82558a389a443c0ea4cc819899f2083a"
	       "85f0faa3e578f8077a2e3ff46729665b"),
     hexstr2bin("a3b6167473100ee06e0c796c2955552b"),
     hexstr2bin("60e431591ee0b67f0d8a26aacbf5b77f"
		"8e0bc6213728c5140546040f0ee37f54"),
     hexstr2bin("9b09ffa71b942fcb27635fbcd5b0e944"
		"bfdc63644f0713938a7f51535c3a35e2")].

rfc4231_hmac_sha384() ->
    [hexstr2bin("afd03944d84895626b0825f4ab46907f"
		"15f9dadbe4101ec682aa034c7cebc59c"
		"faea9ea9076ede7f4af152e8b2fa9cb6"),
     hexstr2bin("af45d2e376484031617f78d2b58a6b1b"
		"9c7ef464f5a01b47e42ec3736322445e"
	       "8e2240ca5e69e2c78b3239ecfab21649"),
     hexstr2bin("88062608d3e6ad8a0aa2ace014c8a86f"
	       "0aa635d947ac9febe83ef4e55966144b"
		"2a5ab39dc13814b94e3ab6e101a34f27"),
     hexstr2bin("3e8a69b7783c25851933ab6290af6ca7"
		"7a9981480850009cc5577c6e1f573b4e"
		"6801dd23c4a7d679ccf8a386c674cffb"),
     hexstr2bin("3abf34c3503b2a23a46efc619baef897"),
     hexstr2bin("4ece084485813e9088d2c63a041bc5b4"
		"4f9ef1012a2b588f3cd11f05033ac4c6"
		"0c2ef6ab4030fe8296248df163f44952"),
     hexstr2bin("6617178e941f020d351e2f254e8fd32c"
		"602420feb0b8fb9adccebb82461e99c5"
		"a678cc31e799176d3860e6110c46523e")].
rfc4231_hmac_sha512() ->
    [hexstr2bin("87aa7cdea5ef619d4ff0b4241a1d6cb0"
		"2379f4e2ce4ec2787ad0b30545e17cde"
		"daa833b7d6b8a702038b274eaea3f4e4"
		"be9d914eeb61f1702e696c203a126854"),
     hexstr2bin("164b7a7bfcf819e2e395fbe73b56e0a3"
		"87bd64222e831fd610270cd7ea250554"
		"9758bf75c05a994a6d034f65f8f0e6fd"
		"caeab1a34d4a6b4b636e070a38bce737"),
     hexstr2bin("fa73b0089d56a284efb0f0756c890be9"
		"b1b5dbdd8ee81a3655f83e33b2279d39"
		"bf3e848279a722c806b485a47e67c807"
		"b946a337bee8942674278859e13292fb"),
     hexstr2bin("b0ba465637458c6990e5a8c5f61d4af7"
		"e576d97ff94b872de76f8050361ee3db"
		"a91ca5c11aa25eb4d679275cc5788063"
		"a5f19741120c4f2de2adebeb10a298dd"),
     hexstr2bin("415fad6271580a531d4179bc891d87a6"),
     hexstr2bin("80b24263c7c1a3ebb71493c1dd7be8b4"
		"9b46d1f41b4aeec1121b013783f8f352"
		"6b56d037e05f2598bd0fd2215d6a1e52"
		"95e64f73f63f0aec8b915a985d786598"),
     hexstr2bin("e37b6a775dc87dbaa4dfa9f96e5e3ffd"
		"debd71f8867289865df5a32d20cdc944"
		"b6022cac3c4982b10d5eeb55c3e4de15"
		"134676fb6de0446065c97440fa8c6a58")].
des_cbc(_) ->
    [{des_cbc, 
     hexstr2bin("0123456789abcdef"), 
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">> }].
      
des_cfb(_) ->
    [{des_cfb, 
     hexstr2bin("0123456789abcdef"),
     hexstr2bin("1234567890abcdef"),
     <<"Now is the">>}].

des3_cbc(_) ->
    [{des3_cbc,
     [hexstr2bin("0123456789abcdef"), 
      hexstr2bin("fedcba9876543210"),
      hexstr2bin("0f2d4b6987a5c3e1")],
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">>
     }].

des_ede3(_) ->
    [{des_ede3,
     [hexstr2bin("8000000000000000"),
      hexstr2bin("4000000000000000"),
      hexstr2bin("2000000000000000")],
      hexstr2bin("7AD16FFB79C45926"),
      hexstr2bin("0000000000000000")
     }].

des_ede3_cbc(_) ->
    [{des_ede3_cbc,
     [hexstr2bin("0123456789abcdef"), 
      hexstr2bin("fedcba9876543210"),
      hexstr2bin("0f2d4b6987a5c3e1")],
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">>
     },
     {des_ede3_cbc,
     [hexstr2bin("8000000000000000"),
      hexstr2bin("4000000000000000"),
      hexstr2bin("2000000000000000")],
      hexstr2bin("7AD16FFB79C45926"),
      hexstr2bin("0000000000000000")
     }].

des3_cbf(_) ->
    [{des3_cbf,
     [hexstr2bin("0123456789abcdef"),
      hexstr2bin("fedcba9876543210"),
      hexstr2bin("0f2d4b6987a5c3e1")],
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">>
     }].

des3_cfb(_) ->
    [{des3_cfb,
     [hexstr2bin("0123456789abcdef"),
      hexstr2bin("fedcba9876543210"),
      hexstr2bin("0f2d4b6987a5c3e1")],
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">>
     }].

des_ede3_cfb(_) ->
    [{des_ede3_cfb,
     [hexstr2bin("0123456789abcdef"),
      hexstr2bin("fedcba9876543210"),
      hexstr2bin("0f2d4b6987a5c3e1")],
     hexstr2bin("1234567890abcdef"),
     <<"Now is the time for all ">>
     }].

rc2_cbc(_) ->
    [{rc2_cbc,
     <<146,210,160,124,215,227,153,239,227,17,222,140,3,93,27,191>>,
      <<72,91,135,182,25,42,35,210>>,
     <<36,245,206,158,168,230,58,69,148,137,32,192,250,41,237,181,181,251, 192,2,175,135,177,171,57,30,111,117,159,149,15,28,88,158,28,81,28,115, 85,219,241,82,117,222,91,85,73,117,164,25,182,52,191,64,123,57,26,19, 211,27,253,31,194,219,231,104,247,240,172,130,119,21,225,154,101,247, 32,216,42,216,133,169,78,22,97,27,227,26,196,224,172,168,17,9,148,55, 203,91,252,40,61,226,236,221,215,160,78,63,13,181,68,57,196,241,185, 207, 116,129,152,237,60,139,247,153,27,146,161,246,222,98,185,222,152, 187,135, 236,86,34,7,110,91,230,173,34,160,242,202,222,121,127,181,140, 101,203,195, 190,88,250,86,147,127,87,72,126,171,16,71,47,110,248,88, 14,29,143,161,152, 129,236,148,22,152,186,208,119,70,8,174,193,203,100, 193,203,200,117,102,242, 134,142,96,125,135,200,217,190,76,117,50,70, 209,186,101,241,200,91,40,193,54, 90,195,38,47,59,197,38,234,86,223,16, 51,253,204,129,20,171,66,21,241,26,135,216, 196,114,110,91,15,53,40, 164,201,136,113,95,247,51,181,208,241,68,168,98,151,36, 155,72,24,57, 42,191,14,125,204,10,167,214,233,138,115,125,234,121,134,227,26,247, 77,200,117,110,117,111,168,156,206,67,159,149,189,173,150,193,91,199, 216,153,22, 189,137,185,89,160,13,131,132,58,109,28,110,246,252,251,14, 232,91,38,52,29,101,188,69,123,50,0,130,178,93,73,239,118,7,77,35,59, 253,10,159,45,86,142,37,78,232,48>>
     }].

%% AES CBC test vectors from http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
aes_cbc(Config) ->
    %% RETIRED aes_*_cbc
    read_rsp(Config, aes_cbc,
            ["CBCVarTxt128.rsp", "CBCVarKey128.rsp", "CBCGFSbox128.rsp", "CBCKeySbox128.rsp",
             "CBCVarTxt192.rsp", "CBCVarKey192.rsp", "CBCGFSbox192.rsp", "CBCKeySbox192.rsp",
             "CBCVarTxt256.rsp", "CBCVarKey256.rsp", "CBCGFSbox256.rsp", "CBCKeySbox256.rsp",
             "CBCMMT128.rsp", "CBCMMT192.rsp", "CBCMMT256.rsp"
            ]).

aes_cbc128(Config) ->
    %% RETIRED aes_128_cbc
    read_rsp(Config, aes_cbc128,
             ["CBCVarTxt128.rsp", "CBCVarKey128.rsp", "CBCGFSbox128.rsp", "CBCKeySbox128.rsp",
              "CBCMMT128.rsp"]).

aes_cbc256(Config) ->
    %% RETIRED aes_256_cbc
    read_rsp(Config, aes_cbc256,
             ["CBCVarTxt256.rsp", "CBCVarKey256.rsp", "CBCGFSbox256.rsp", "CBCKeySbox256.rsp",
              "CBCMMT256.rsp"]).

aes_128_cbc(Config) ->
    read_rsp(Config, aes_128_cbc,
             ["CBCVarTxt128.rsp", "CBCVarKey128.rsp", "CBCGFSbox128.rsp", "CBCKeySbox128.rsp",
              "CBCMMT128.rsp"]).

aes_192_cbc(Config) ->
    read_rsp(Config, aes_192_cbc,
             ["CBCVarTxt192.rsp", "CBCVarKey192.rsp", "CBCGFSbox192.rsp", "CBCKeySbox192.rsp",
              "CBCMMT192.rsp"]).

aes_256_cbc(Config) ->
    read_rsp(Config, aes_256_cbc,
             ["CBCVarTxt256.rsp", "CBCVarKey256.rsp", "CBCGFSbox256.rsp", "CBCKeySbox256.rsp",
              "CBCMMT256.rsp"]).

aes_128_ofb(Config) ->
    read_rsp(Config, aes_128_ofb,
             ["OFBVarTxt128.rsp", "OFBVarKey128.rsp", "OFBGFSbox128.rsp", "OFBKeySbox128.rsp",
              "OFBMMT128.rsp"]).

aes_192_ofb(Config) ->
    read_rsp(Config, aes_192_ofb,
             ["OFBVarTxt192.rsp", "OFBVarKey192.rsp", "OFBGFSbox192.rsp", "OFBKeySbox192.rsp",
              "OFBMMT192.rsp"]).

aes_256_ofb(Config) ->
    read_rsp(Config, aes_256_ofb,
             ["OFBVarTxt256.rsp", "OFBVarKey256.rsp", "OFBGFSbox256.rsp", "OFBKeySbox256.rsp",
              "OFBMMT256.rsp"]).

aes_ecb(Config) ->
    read_rsp(Config, aes_ecb,
             ["ECBVarTxt128.rsp", "ECBVarKey128.rsp", "ECBGFSbox128.rsp", "ECBKeySbox128.rsp",
              "ECBVarTxt192.rsp", "ECBVarKey192.rsp", "ECBGFSbox192.rsp", "ECBKeySbox192.rsp",
              "ECBVarTxt256.rsp", "ECBVarKey256.rsp", "ECBGFSbox256.rsp", "ECBKeySbox256.rsp",
              "ECBMMT128.rsp", "ECBMMT192.rsp", "ECBMMT256.rsp"]).

aes_128_ecb(Config) ->
    read_rsp(Config, aes_128_ecb,
             ["ECBVarTxt128.rsp", "ECBVarKey128.rsp", "ECBGFSbox128.rsp", "ECBKeySbox128.rsp",
              "ECBMMT128.rsp"]).

aes_192_ecb(Config) ->
    read_rsp(Config, aes_192_ecb,
             ["ECBVarTxt192.rsp", "ECBVarKey192.rsp", "ECBGFSbox192.rsp", "ECBKeySbox192.rsp",
              "ECBMMT192.rsp"]).

aes_256_ecb(Config) ->
    read_rsp(Config, aes_256_ecb,
             ["ECBVarTxt256.rsp", "ECBVarKey256.rsp", "ECBGFSbox256.rsp", "ECBKeySbox256.rsp",
              "ECBMMT256.rsp"]).

aes_cfb8(Config) ->
    read_rsp(Config, aes_cfb8,
             ["CFB8VarTxt128.rsp", "CFB8VarKey128.rsp", "CFB8GFSbox128.rsp", "CFB8KeySbox128.rsp",
              "CFB8VarTxt192.rsp", "CFB8VarKey192.rsp", "CFB8GFSbox192.rsp", "CFB8KeySbox192.rsp",
              "CFB8VarTxt256.rsp", "CFB8VarKey256.rsp", "CFB8GFSbox256.rsp", "CFB8KeySbox256.rsp",
              "CFB8MMT128.rsp", "CFB8MMT192.rsp", "CFB8MMT256.rsp"]).

aes_128_cfb8(Config) ->
    read_rsp(Config, aes_128_cfb8,
             ["CFB8VarTxt128.rsp", "CFB8VarKey128.rsp", "CFB8GFSbox128.rsp", "CFB8KeySbox128.rsp",
              "CFB8MMT128.rsp"]).

aes_192_cfb8(Config) ->
    read_rsp(Config, aes_192_cfb8,
             ["CFB8VarTxt192.rsp", "CFB8VarKey192.rsp", "CFB8GFSbox192.rsp", "CFB8KeySbox192.rsp",
              "CFB8MMT192.rsp"]).

aes_256_cfb8(Config) ->
    read_rsp(Config, aes_256_cfb8,
             ["CFB8VarTxt256.rsp", "CFB8VarKey256.rsp", "CFB8GFSbox256.rsp", "CFB8KeySbox256.rsp",
              "CFB8MMT256.rsp"]).


aes_cfb128(Config) ->
    read_rsp(Config, aes_cfb128,
             ["CFB128VarTxt128.rsp", "CFB128VarKey128.rsp", "CFB128GFSbox128.rsp", "CFB128KeySbox128.rsp",
              "CFB128VarTxt192.rsp", "CFB128VarKey192.rsp", "CFB128GFSbox192.rsp", "CFB128KeySbox192.rsp",
              "CFB128VarTxt256.rsp", "CFB128VarKey256.rsp", "CFB128GFSbox256.rsp", "CFB128KeySbox256.rsp",
              "CFB128MMT128.rsp", "CFB128MMT192.rsp", "CFB128MMT256.rsp"]).

aes_128_cfb128(Config) ->
    read_rsp(Config, aes_128_cfb128,
             ["CFB128VarTxt128.rsp", "CFB128VarKey128.rsp", "CFB128GFSbox128.rsp", "CFB128KeySbox128.rsp",
              "CFB128MMT128.rsp"]).

aes_192_cfb128(Config) ->
    read_rsp(Config, aes_192_cfb128,
             ["CFB128VarTxt192.rsp", "CFB128VarKey192.rsp", "CFB128GFSbox192.rsp", "CFB128KeySbox192.rsp",
              "CFB128MMT192.rsp"]).

aes_256_cfb128(Config) ->
    read_rsp(Config, aes_256_cfb128,
             ["CFB128VarTxt256.rsp", "CFB128VarKey256.rsp", "CFB128GFSbox256.rsp", "CFB128KeySbox256.rsp",
              "CFB128MMT256.rsp"]).


blowfish_cbc(_) ->
    [{blowfish_cbc,
      hexstr2bin("0123456789ABCDEFF0E1D2C3B4A59687"), 
      hexstr2bin("FEDCBA9876543210"),
      hexstr2bin("37363534333231204E6F77206973207468652074696D6520666F722000000000")
     }].

blowfish_ecb(_) ->
    [
     {blowfish_ecb,
      hexstr2bin("0000000000000000"), 
      hexstr2bin("0000000000000000")},
     {blowfish_ecb,
      hexstr2bin("FFFFFFFFFFFFFFFF"), 
      hexstr2bin("FFFFFFFFFFFFFFFF")},
     {blowfish_ecb,
      hexstr2bin("3000000000000000"), 
      hexstr2bin("1000000000000001")},
     {blowfish_ecb,
      hexstr2bin("1111111111111111"), 
      hexstr2bin("1111111111111111")},
     {blowfish_ecb,
      hexstr2bin("0123456789ABCDEF"), 
      hexstr2bin("1111111111111111")},
     {blowfish_ecb,
      hexstr2bin("0000000000000000"), 
      hexstr2bin("0000000000000000")},
     {blowfish_ecb,
      hexstr2bin("FEDCBA9876543210"), 
      hexstr2bin("0123456789ABCDEF")},
     {blowfish_ecb,
      hexstr2bin("7CA110454A1A6E57"), 
      hexstr2bin("01A1D6D039776742")},
     {blowfish_ecb,
      hexstr2bin("0131D9619DC1376E"), 
      hexstr2bin("5CD54CA83DEF57DA")},
     {blowfish_ecb,
      hexstr2bin("07A1133E4A0B2686"), 
      hexstr2bin("0248D43806F67172")},
     {blowfish_ecb,
      hexstr2bin("3849674C2602319E"), 
      hexstr2bin("51454B582DDF440A")},
     {blowfish_ecb,
      hexstr2bin("04B915BA43FEB5B6"), 
      hexstr2bin("42FD443059577FA2")},
     {blowfish_ecb,
      hexstr2bin("0113B970FD34F2CE"), 
      hexstr2bin("059B5E0851CF143A")},
     {blowfish_ecb,
      hexstr2bin("0170F175468FB5E6"), 
      hexstr2bin("0756D8E0774761D2")},
     {blowfish_ecb,
      hexstr2bin("43297FAD38E373FE"), 
      hexstr2bin("762514B829BF486A")},
     {blowfish_ecb,
      hexstr2bin("07A7137045DA2A16"), 
      hexstr2bin("3BDD119049372802")},
     {blowfish_ecb,
      hexstr2bin("04689104C2FD3B2F"), 
      hexstr2bin("26955F6835AF609A")},
     {blowfish_ecb,
      hexstr2bin("37D06BB516CB7546"), 
      hexstr2bin("164D5E404F275232")},
     {blowfish_ecb,
      hexstr2bin("1F08260D1AC2465E"), 
      hexstr2bin("6B056E18759F5CCA")},
     {blowfish_ecb,
      hexstr2bin("584023641ABA6176"), 
      hexstr2bin("004BD6EF09176062")},
     {blowfish_ecb,
      hexstr2bin("025816164629B007"), 
      hexstr2bin("480D39006EE762F2")},
     {blowfish_ecb,
      hexstr2bin("49793EBC79B3258F"), 
      hexstr2bin("437540C8698F3CFA")},
     {blowfish_ecb,
      hexstr2bin("018310DC409B26D6"), 
      hexstr2bin("1D9D5C5018F728C2")},
     {blowfish_ecb,
      hexstr2bin("1C587F1C13924FEF"), 
      hexstr2bin("305532286D6F295A")},
     {blowfish_ecb,
      hexstr2bin("0101010101010101"), 
      hexstr2bin("0123456789ABCDEF")},
     {blowfish_ecb,
      hexstr2bin("1F1F1F1F0E0E0E0E"), 
      hexstr2bin("0123456789ABCDEF")},
     {blowfish_ecb,
      hexstr2bin("E0FEE0FEF1FEF1FE"), 
      hexstr2bin("0123456789ABCDEF")},
     {blowfish_ecb,
      hexstr2bin("0000000000000000"), 
      hexstr2bin("FFFFFFFFFFFFFFFF")},
     {blowfish_ecb,
      hexstr2bin("FFFFFFFFFFFFFFFF"), 
      hexstr2bin("0000000000000000")},
     {blowfish_ecb,
      hexstr2bin("0123456789ABCDEF"), 
      hexstr2bin("0000000000000000")},
     {blowfish_ecb,
      hexstr2bin("FEDCBA9876543210"), 
      hexstr2bin("FFFFFFFFFFFFFFFF")}
    ].

blowfish_cfb64(_) ->
    [{blowfish_cfb64,
      hexstr2bin("0123456789ABCDEFF0E1D2C3B4A59687"), 
      hexstr2bin("FEDCBA9876543210"),
      hexstr2bin("37363534333231204E6F77206973207468652074696D6520666F722000")
     }].
blowfish_ofb64(_) ->
    [{blowfish_ofb64,
      hexstr2bin("0123456789ABCDEFF0E1D2C3B4A59687"), 
      hexstr2bin("FEDCBA9876543210"),
      hexstr2bin("37363534333231204E6F77206973207468652074696D6520666F722000")
     }].

rc4(_) ->
    [{rc4, <<"apaapa">>, <<"Yo baby yo">>},
     {rc4, <<"apaapa">>, list_to_binary(lists:seq(0, 255))},
     {rc4, <<"apaapa">>, long_msg()}
    ].

aes_128_ctr(_) ->
    [  %% F.5.3  CTR-AES192.Encrypt
       {aes_128_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
	hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
       {aes_128_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
	hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
       {aes_128_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
	hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef") },
       {aes_128_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
	hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")}
    ].
       
aes_192_ctr(_) ->
    [ %% F.5.3  CTR-AES192.Encrypt
      {aes_192_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
       hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_192_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_192_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
	hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_192_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")}
    ].
       
aes_256_ctr(_) ->
    [ %% F.5.5  CTR-AES256.Encrypt
      {aes_256_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
       hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_256_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_256_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_256_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},

      {aes_256_ctr,  hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"),
       long_msg()}
    ].


aes_gcm(Config) ->
    %% RETIRED aes_*_gcm
    read_rsp(Config, aes_gcm,
             ["gcmDecrypt128.rsp",
              "gcmDecrypt192.rsp",
              "gcmDecrypt256.rsp",
              "gcmEncryptExtIV128.rsp",
              "gcmEncryptExtIV192.rsp",
              "gcmEncryptExtIV256.rsp"]).

aes_128_gcm(Config) ->
   read_rsp(Config, aes_128_gcm,
            ["gcmDecrypt128.rsp",
             "gcmEncryptExtIV128.rsp"]).

aes_192_gcm(Config) ->
   read_rsp(Config, aes_192_gcm,
            ["gcmDecrypt192.rsp",
             "gcmEncryptExtIV192.rsp"]).

aes_256_gcm(Config) ->
   read_rsp(Config, aes_256_gcm,
            ["gcmDecrypt256.rsp",
             "gcmEncryptExtIV256.rsp"]).


aes_ccm(Config) ->
    %% RETIRED aes_*_ccm
    read_rsp(Config, aes_ccm,
             ["VADT128.rsp", "VADT192.rsp", "VADT256.rsp",
              "VNT128.rsp",  "VNT192.rsp",  "VNT256.rsp",
              "VPT128.rsp",  "VPT192.rsp",  "VPT256.rsp"
             ]).

aes_128_ccm(Config) ->
    read_rsp(Config, aes_128_ccm,
            ["VADT128.rsp", "VNT128.rsp", "VPT128.rsp"]).

aes_192_ccm(Config) ->
   read_rsp(Config, aes_192_ccm,
            ["VADT192.rsp", "VNT192.rsp", "VPT192.rsp"]).

aes_256_ccm(Config) ->
   read_rsp(Config, aes_256_ccm,
            ["VADT256.rsp", "VNT256.rsp", "VPT256.rsp"]).



%% https://tools.ietf.org/html/rfc7539#appendix-A.5
chacha20_poly1305(_) ->
    [
     {chacha20_poly1305,
      hexstr2bin("1c9240a5eb55d38af333888604f6b5f0"                      %% Key
			"473917c1402b80099dca5cbc207075c0"),
      hexstr2bin("496e7465726e65742d44726166747320"                      %% PlainText
      "61726520647261667420646f63756d65"
      "6e74732076616c696420666f72206120"
      "6d6178696d756d206f6620736978206d"
      "6f6e74687320616e64206d6179206265"
      "20757064617465642c207265706c6163"
      "65642c206f72206f62736f6c65746564"
      "206279206f7468657220646f63756d65"
      "6e747320617420616e792074696d652e"
      "20497420697320696e617070726f7072"
      "6961746520746f2075736520496e7465"
      "726e65742d4472616674732061732072"
      "65666572656e6365206d617465726961"
      "6c206f7220746f206369746520746865"
      "6d206f74686572207468616e20617320"
      "2fe2809c776f726b20696e2070726f67"
      "726573732e2fe2809d"),
      hexstr2bin("000000000102030405060708"),                            %% Nonce
      hexstr2bin("f33388860000000000004e91"),                            %% AAD
      hexstr2bin("64a0861575861af460f062c79be643bd"                      %% CipherText
      "5e805cfd345cf389f108670ac76c8cb2"
      "4c6cfc18755d43eea09ee94e382d26b0"
      "bdb7b73c321b0100d4f03b7f355894cf"
      "332f830e710b97ce98c8a84abd0b9481"
      "14ad176e008d33bd60f982b1ff37c855"
      "9797a06ef4f0ef61c186324e2b350638"
      "3606907b6a7c02b0f9f6157b53c867e4"
      "b9166c767b804d46a59b5216cde7a4e9"
      "9040c5a40433225ee282a1b0a06c523e"
      "af4534d7f83fa1155b0047718cbc546a"
      "0d072b04b3564eea1b422273f548271a"
      "0bb2316053fa76991955ebd63159434e"
      "cebb4e466dae5a1073a6727627097a10"
      "49e617d91d361094fa68f0ff77987130"
      "305beaba2eda04df997b714d6c6f2c29"
      "a6ad5cb4022b02709b"),
      hexstr2bin("eead9d67890cbb22392336fea1851f38"),                    %% CipherTag
      no_info
      }
    ].


chacha20(_) ->
%%% chacha20 (no mode) test vectors from RFC 7539 A.2
    [
     %% Test Vector #1:
     {chacha20,
      hexstr2bin("00000000000000000000000000000000"
                 "00000000000000000000000000000000"),                    %% Key
      hexstr2bin("00000000" % Initial counter = 0, little-endian
                 "000000000000000000000000"),                            %% IV
      hexstr2bin("00000000000000000000000000000000"                      %% PlainText
                 "00000000000000000000000000000000"
                 "00000000000000000000000000000000"
                 "00000000000000000000000000000000"),
      hexstr2bin("76b8e0ada0f13d90405d6ae55386bd28"                      %% CipherText
                 "bdd219b8a08ded1aa836efcc8b770dc7"
                 "da41597c5157488d7724e03fb8d84a37"
                 "6a43b8f41518a11cc387b669b2ee6586")},
     %% Test Vector #2:
     {chacha20,
      hexstr2bin("00000000000000000000000000000000"
                 "00000000000000000000000000000001"),                    %% Key
      hexstr2bin("01000000" % Initial counter = 1, little-endian
                 "000000000000000000000002"),                            %% IV
      hexstr2bin("416e79207375626d697373696f6e2074"                      %% PlainText
                 "6f20746865204945544620696e74656e"
                 "6465642062792074686520436f6e7472"
                 "696275746f7220666f72207075626c69"
                 "636174696f6e20617320616c6c206f72"
                 "2070617274206f6620616e2049455446"
                 "20496e7465726e65742d447261667420"
                 "6f722052464320616e6420616e792073"
                 "746174656d656e74206d616465207769"
                 "7468696e2074686520636f6e74657874"
                 "206f6620616e20494554462061637469"
                 "7669747920697320636f6e7369646572"
                 "656420616e20224945544620436f6e74"
                 "7269627574696f6e222e205375636820"
                 "73746174656d656e747320696e636c75"
                 "6465206f72616c2073746174656d656e"
                 "747320696e2049455446207365737369"
                 "6f6e732c2061732077656c6c20617320"
                 "7772697474656e20616e6420656c6563"
                 "74726f6e696320636f6d6d756e696361"
                 "74696f6e73206d61646520617420616e"
                 "792074696d65206f7220706c6163652c"
                 "20776869636820617265206164647265"
                 "7373656420746f"),
      hexstr2bin("a3fbf07df3fa2fde4f376ca23e827370"                      %% CipherText
                 "41605d9f4f4f57bd8cff2c1d4b7955ec"
                 "2a97948bd3722915c8f3d337f7d37005"
                 "0e9e96d647b7c39f56e031ca5eb6250d"
                 "4042e02785ececfa4b4bb5e8ead0440e"
                 "20b6e8db09d881a7c6132f420e527950"
                 "42bdfa7773d8a9051447b3291ce1411c"
                 "680465552aa6c405b7764d5e87bea85a"
                 "d00f8449ed8f72d0d662ab052691ca66"
                 "424bc86d2df80ea41f43abf937d3259d"
                 "c4b2d0dfb48a6c9139ddd7f76966e928"
                 "e635553ba76c5c879d7b35d49eb2e62b"
                 "0871cdac638939e25e8a1e0ef9d5280f"
                 "a8ca328b351c3c765989cbcf3daa8b6c"
                 "cc3aaf9f3979c92b3720fc88dc95ed84"
                 "a1be059c6499b9fda236e7e818b04b0b"
                 "c39c1e876b193bfe5569753f88128cc0"
                 "8aaa9b63d1a16f80ef2554d7189c411f"
                 "5869ca52c5b83fa36ff216b9c1d30062"
                 "bebcfd2dc5bce0911934fda79a86f6e6"
                 "98ced759c3ff9b6477338f3da4f9cd85"
                 "14ea9982ccafb341b2384dd902f3d1ab"
                 "7ac61dd29c6f21ba5b862f3730e37cfd"
                 "c4fd806c22f221")},
     %%Test Vector #3:
     {chacha20,
      hexstr2bin("1c9240a5eb55d38af333888604f6b5f0"
                 "473917c1402b80099dca5cbc207075c0"),                    %% Key
      hexstr2bin("2a000000" % Initial counter = 42 (decimal), little-endian
                 "000000000000000000000002"),                            %% IV
      hexstr2bin("2754776173206272696c6c69672c2061"                      %% PlainText
                 "6e642074686520736c6974687920746f"
                 "7665730a446964206779726520616e64"
                 "2067696d626c6520696e207468652077"
                 "6162653a0a416c6c206d696d73792077"
                 "6572652074686520626f726f676f7665"
                 "732c0a416e6420746865206d6f6d6520"
                 "7261746873206f757467726162652e"),
      hexstr2bin("62e6347f95ed87a45ffae7426f27a1df"                      %% CipherText
                 "5fb69110044c0d73118effa95b01e5cf"
                 "166d3df2d721caf9b21e5fb14c616871"
                 "fd84c54f9d65b283196c7fe4f60553eb"
                 "f39c6402c42234e32a356b3e764312a6"
                 "1a5532055716ead6962568f87d3f3f77"
                 "04c6a8d1bcd1bf4d50d6154b6da731b1"
                 "87b58dfd728afa36757a797ac188d1")}
    ].


rsa_plain() ->
    <<"7896345786348756234 Hejsan Svejsan, erlang crypto debugger"
      "09812312908312378623487263487623412039812 huagasd">>.
rsa_public() ->
    [65537, 7919488123861148172698919999061127847747888703039837999377650217570191053151807772962118671509138346758471459464133273114654252861270845708312601272799123].
rsa_private() ->
    rsa_public() ++ [7531712708607620783801185371644749935066152052780368689827275932079815492940396744378735701395659435842364793962992309884847527234216715366607660219930945].

rsa_public_stronger() ->
    [65537, 24629450921918866883077380602720734920775458960049554761386137065662137652635369332143446151320538248280934442179850504891395344346514465469955766163141133564033962851182759993807898821114734943339732032639891483186089941567854227407119560631150779000222837755424893038740314247760600374970909894211201220612920040986106639419467243909950276018045907029941478599124238353052062083560294570722081552510960894164859765695309596889747541376908786225647625736062865138957717982693312699025417086612046330464651009693307624955796202070510577399561730651967517158452930742355327167632521808183383868100102455048819375344881].

rsa_private_stronger() ->
    rsa_public_stronger() ++ [13565232776562604620467234237694854016819673873109064019820773052201665024482754648718278717031083946624786145611240731564761987114634269887293030432042088547345315212418830656522115993209293567218379960177754901461542373481136856927955012596579314262051109321754382091434920473734937991286600905464814063189230779981494358415076362038786197620360127262110530926733754185204773610295221669711309000953136320804528874719105049753061737780710448207922456570922652651354760939379096788728229638142403068102990416717272880560951246813789730402978652924934794503277969128609831043469924881848849409122972426787999886557185].

dss_plain() ->
    rsa_plain().
dss_public() ->
    16#289ff18c32a56bb0b8839370647683a38a5a7e291410b93207212adc8088d30f93e9e4abc523f3d46936e7d5c90d88742b36afd37563408f15c8c1a4f7ac24bf05f01008ffee70c8825d57c3a9308bad8a095af2b53b2dda3cbed846d95e301eb9b84766415d11f6c33209a0d28571096ab04a79aa0dc465997529686b68e887cd8a205c2dc8195aef0422eba9979f549ac85548e419413643b7244361153ada1480d238cd00dc16527938955548dd5d027ded1029eeeb8ed6c61b4cd59341d8b15466e9da890a989996f4d7691e6072de136af28b5874bf08bd1f8a60cfb1c00888132909f515e04bce81b02951aa41baac68ffdb8c5dc77a1d32d8f2c10dd7.
dss_private() ->
    16#6132e551cdac88409183bd37ee1452cd247d4834b08814b275be3ff5.
dss_params() ->
    [16#f2d39ed3062b13c916273600a0f2a029e86d7a4b9217b4f1815bf2b24d9710a57ab33f997294b014585b8d0198dfdccbcd75314da5ff85aa344b45adaeaa979b51a312a7bfa94472fb633f1a6f156bb4458867dfd38403f06b851f00fe2d3484077bded71ab7513d04a140220575fb693395480e4c8402b7a46cec2d37a778c305accd1f13e9f62e865315f4b22cc467c8986ec8e4961ddf810566b0c4ee369ac6aa15e43f4744005826f5bde8071a19e30b6909aac4b3d174237270dad02799d09b8a2cc5f22e66894b5422228b2c234f11f5a771c5b89cf465a2acecbbeeaa1725fe8f9b59422be8991052cb556ddf2c8ce8fa9206dbf39feadc194e00f8e5,
     16#8000000000000000c118f49835e4ef733c4d15800fcf059e884d31b1,
     16#e3a93c09da6f560e4d483a382a4c546f2335c36a4c35ac1463c08a3e6dd415df56fdc537f25fd5372be63e4f5300780b782f1acd01c8b4eb33414615fd0ea82573acba7ef83f5a943854151afc2d7dfe121fb8cd03335b065b549c5dcc606be9052483bc284e12ac3c8dba09b426e08402030e70bc1cc2bf8957c4ba0630f3f32ad689389ac47443176063f247d9e2296b3ea5b5bc2335828ea1a080ed35918dee212fd031279d1b894f01afec523833669eac031a420e540ba1320a59c424a3e5849a460a56bcb001647885b1433c4f992971746bfe2977ce7259c550b551a6c35761e4a41af764e8d92132fcc0a59d1684eab90d863f29f41cf7578faa908c].

ec_key_named() ->
    Curve = secp224r1, %hd(crypto:ec_curves()),
    {D2_pub, D2_priv} = crypto:generate_key(ecdh, Curve),
    {[D2_priv, Curve], [D2_pub, Curve]}.

ec_msg() ->
    <<99,234,6,64,190,237,201,99,80,248,58,40,70,45,149,218,5,246,242,63>>.

srp3() ->
    Username = <<"alice">>,
    Password = <<"password123">>,
    Salt = hexstr2bin("2857827A19266A1F2BC6"),
    Prime = hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
		       "9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
		       "8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
		       "7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
		       "FD5138FE8376435B9FC61D2FC0EB06E3"),
    Generator = <<2>>,
    Version = '3',
    Scrambler = hexstr2bin("02E2476A"),

    %% X = hexstr2bin("96E54AB0CD4C5123EDCFA4A1502918AAD3C9E2A8"),
    Verifier = hexstr2bin("96EB5F13621D911AA1CA405DE9C64217D4108EEEECAFFE500034FE0E"
			  "C031E42C8714667C161BCE0E7996F7DDE1B63824C130D2D7286C08C0"
			  "49758420735961347112AE102A3F23B3F687F8FEE0DF2BFAF933C608"
			  "D6FE5B5EEE3116FE54016E065BF8E8C9FDBBC08719231AC215149140"
			  "519E8FDD9AA4F410C28A58AF42974D2D"),
    ClientPrivate = hexstr2bin("6411DE75538BED8170677D577D0608F39112BC95B503C447EB6AC945"
			  "49C75C7B"),
    ServerPrivate = hexstr2bin("85E44A6F694DBE676145DB245A045CD37C99F05C562C7840A31F270D"
			  "9AADCF8B"),
    ClientPublic = hexstr2bin("B22B1FFA2244B8CB94F3A9080F419CAEAB0DBA93EA1965B5E84587EE"
			 "55C79E7A118865DC59B9D0353362C2A8261E7C1B0D221A0E233C2AD1"
			 "640DACBB8664CBC9733EAC392DA7800142860380C3FC573C3C064329"
			 "CF54063FD114C7210E9CB3A611EA8002B1844B698F930D95D143899B"
			 "948A090E0C25938E5F84067D1883DC63"),
    ServerPublic = hexstr2bin("93A8C4D8B7F7395ADCFD4ABA37B015124513D3F37B3E85EB23064BE5"
			 "F53C0AE32FFB9D8C0AA0DCFFA74D632DD67DEBB5C35AAE9812286CC8"
			 "C43CC176ECBC6D3F447594D9554E995B2509127BF88FADDDA4982D03"
			 "8EC3001320712D3B1269308CE70F319B2295FA57674F03A2D993CFB1"
			 "F84C35B7D0C012FA73CD4C8F7D5A71C7"),

    SessionKey = hexstr2bin("C29A986C4D521BBC66428ED11D994CD7431574A6184B83CDCC345092"
			    "791E75748A1D38CAC4BD14760F0D2694B711236419240FF2F172454C"
			    "46ABF4FF39498DAFDD2C82924F7D7BD76CDFCE688C77D93F18A65409"
			    "9176A9192615DC0277AE7C12F1F6A7F6563FCA11675D809AF578BDE5"
			    "2B51E05D440B63099A017A0B45044801"),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime), 
    srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey).

srp6() ->
    Username = <<"alice">>,
    Password = <<"password123">>,
    Salt = hexstr2bin("2857827A19266A1F2BC6"),
    Prime = hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
		       "9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
		       "8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
		       "7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
		       "FD5138FE8376435B9FC61D2FC0EB06E3"),
    Generator = <<2>>,
    Version = '6',
    Scrambler = hexstr2bin("0A2534C0BF52A0DA9001EEC62CF2A546AB0908A7"),
    Verifier = hexstr2bin("96EB5F13621D911AA1CA405DE9C64217D4108EEEECAFFE500034FE0E"
			  "C031E42C8714667C161BCE0E7996F7DDE1B63824C130D2D7286C08C0"
			  "49758420735961347112AE102A3F23B3F687F8FEE0DF2BFAF933C608"
			  "D6FE5B5EEE3116FE54016E065BF8E8C9FDBBC08719231AC215149140"
			  "519E8FDD9AA4F410C28A58AF42974D2D"),
    ClientPrivate = hexstr2bin("6411DE75538BED8170677D577D0608F39112BC95B503C447EB6AC945"
			  "49C75C7B"),
    ServerPrivate = hexstr2bin("85E44A6F694DBE676145DB245A045CD37C99F05C562C7840A31F270D"
			  "9AADCF8B"),
    ClientPublic = hexstr2bin("B22B1FFA2244B8CB94F3A9080F419CAEAB0DBA93EA1965B5E84587EE"
			 "55C79E7A118865DC59B9D0353362C2A8261E7C1B0D221A0E233C2AD1"
			 "640DACBB8664CBC9733EAC392DA7800142860380C3FC573C3C064329"
			 "CF54063FD114C7210E9CB3A611EA8002B1844B698F930D95D143899B"
			 "948A090E0C25938E5F84067D1883DC63"),
    ServerPublic = hexstr2bin("D2D07845CE7ECDB9845DD36B10ACD3598CC29049DE9F467F84CE16B6"
			 "D97A6DC567AF8B0F9FEDF74962400AD5C357951E64E67B641246F264"
			 "C8DE6D9A72E554D6C8D3194548780A0C438A0FCC509CA88A14AA1DEB"
			 "C0F09E4B37A965D1545DB4AD361346F3189B0EA569C06D326C4E4797"
			 "9E381C748293B7C0591BE0BE419E053E"),

    SessionKey = hexstr2bin("19D22C19612874EBF1F2581F8EFCFDC44C6FDA3B87B0A73823D7E962"
				 "554295D4E48D3A336523ADBDDD0EC8FB0F02687109E97E01C17C93CC"
				 "7216F9CD8A4AC39F0429857D8D1023066614BDFCBCB89F59A0FEB81C"
				 "72E992AAD89095A84B6A5FADA152369AB1E350A03693BEF044DF3EDF"
				 "0C34741F4696C30E9F675D09F58ACBEB"),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime),
    srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey).


srp6a_smaller_prime() ->
    Username = <<"alice">>,
    Password = <<"password123">>,
    Salt = <<"mystrongsalt">>,
    Prime = hexstr2bin("894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7"),
    Generator = <<7>>,
    Version = '6a',
    Scrambler = hexstr2bin("18DE4A002AD05EF464B19AE2B6929F9B1319C7AA"),
    Verifier = hexstr2bin("867401D5DE10964768184EAF246B322760C847604075FA66A4423907"
			  "8428BCA5"),
    ClientPrivate = hexstr2bin("C49F832EE8D67ECF9E7F2785EB0622D8B3FE2344C00F96E1AEF4103C"
			  "A44D51F9"),
    ServerPrivate = hexstr2bin("6C78CCEAAEC15E69068A87795B2A20ED7B45CFC5A254EBE2F17F144A"
			  "4D99DB18"),
    ClientPublic = hexstr2bin("2452A57166BBBF690DB77539BAF9C57CD1ED99D5AA15ED925AD9B5C3"
			  "64BBEDFF"),
    ServerPublic = hexstr2bin("2C0464DE84B91E4963A3546CAC0EFE55F31F49208C3F0AD7EE55F444"
			  "8F38BA7F"),

    SessionKey = hexstr2bin("65581B2302580BD26F522A5A421CF969B9CCBCE4051196B034A2A9D22065D848"),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime),
    srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey).

srp6a() ->
    Username = <<"alice">>,
    Password = <<"password123">>,
    Salt = hexstr2bin("BEB25379D1A8581EB5A727673A2441EE"),
    Prime = hexstr2bin("EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C"
		       "9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4"
		       "8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29"
		       "7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A"
		       "FD5138FE8376435B9FC61D2FC0EB06E3"),
    Generator = <<2>>,
    Version = '6a',
    Scrambler = hexstr2bin("CE38B9593487DA98554ED47D70A7AE5F462EF019"),
    Verifier = hexstr2bin("7E273DE8696FFC4F4E337D05B4B375BEB0DDE1569E8FA00A9886D812"
			  "9BADA1F1822223CA1A605B530E379BA4729FDC59F105B4787E5186F5"
			  "C671085A1447B52A48CF1970B4FB6F8400BBF4CEBFBB168152E08AB5"
			  "EA53D15C1AFF87B2B9DA6E04E058AD51CC72BFC9033B564E26480D78"
			  "E955A5E29E7AB245DB2BE315E2099AFB"),
    ClientPrivate = hexstr2bin("60975527035CF2AD1989806F0407210BC81EDC04E2762A56AFD529DD"
			  "DA2D4393"),
    ServerPrivate = hexstr2bin("E487CB59D31AC550471E81F00F6928E01DDA08E974A004F49E61F5D1"
			  "05284D20"),
    ClientPublic = hexstr2bin("61D5E490F6F1B79547B0704C436F523DD0E560F0C64115BB72557EC4"
			      "4352E8903211C04692272D8B2D1A5358A2CF1B6E0BFCF99F921530EC"
			      "8E39356179EAE45E42BA92AEACED825171E1E8B9AF6D9C03E1327F44"
			      "BE087EF06530E69F66615261EEF54073CA11CF5858F0EDFDFE15EFEA"
			      "B349EF5D76988A3672FAC47B0769447B"),
    ServerPublic = hexstr2bin("BD0C61512C692C0CB6D041FA01BB152D4916A1E77AF46AE105393011"
			      "BAF38964DC46A0670DD125B95A981652236F99D9B681CBF87837EC99"
			      "6C6DA04453728610D0C6DDB58B318885D7D82C7F8DEB75CE7BD4FBAA"
			      "37089E6F9C6059F388838E7A00030B331EB76840910440B1B27AAEAE"
			      "EB4012B7D7665238A8E3FB004B117B58"),
    
    SessionKey = hexstr2bin("B0DC82BABCF30674AE450C0287745E7990A3381F63B387AAF271A10D"
			    "233861E359B48220F7C4693C9AE12B0A6F67809F0876E2D013800D6C"
			    "41BB59B6D5979B5C00A172B4A2A5903A0BDCAF8A709585EB2AFAFA8F"
			    "3499B200210DCC1F10EB33943CD67FC88A2F39A4BE5BEC4EC0A3212D"
			    "C346D7E474B29EDE8A469FFECA686E5A"),
    UserPassHash = crypto:hash(sha, [Salt, crypto:hash(sha, [Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime), 
    srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey).

srp(ClientPrivate, Generator, Prime, Version, Verifier, ServerPublic, ServerPrivate, UserPassHash, Scrambler, SessionKey)->
    {srp, ClientPrivate, 
     {user, [Generator, Prime, Version]}, {user, [UserPassHash, Prime, Generator, Version, Scrambler]}, 
     ServerPublic, ServerPrivate, {host, [Verifier, Generator, Prime, Version]},
     {host, [Verifier, Prime, Version, Scrambler]},
     SessionKey}.

eddsa(ed25519) ->
    %% https://tools.ietf.org/html/rfc8032#section-7.1
    %% {ALGORITHM, (SHA), SECRET KEY, PUBLIC KEY,  MESSAGE, SIGNATURE}
    [
     %% TEST 1
     {ed25519, undefined,
      hexstr2bin("9d61b19deffd5a60ba844af492ec2cc4"
                 "4449c5697b326919703bac031cae7f60"),
      hexstr2bin("d75a980182b10ab7d54bfed3c964073a"
                 "0ee172f3daa62325af021a68f707511a"),
      hexstr2bin(""),
      hexstr2bin("e5564300c360ac729086e2cc806e828a"
                 "84877f1eb8e5d974d873e06522490155"
                 "5fb8821590a33bacc61e39701cf9b46b"
                 "d25bf5f0595bbe24655141438e7a100b")},
     %% TEST 2
     {ed25519, undefined,
      hexstr2bin("4ccd089b28ff96da9db6c346ec114e0f"
                 "5b8a319f35aba624da8cf6ed4fb8a6fb"),
      hexstr2bin("3d4017c3e843895a92b70aa74d1b7ebc"
                 "9c982ccf2ec4968cc0cd55f12af4660c"),
      hexstr2bin("72"),
      hexstr2bin("92a009a9f0d4cab8720e820b5f642540"
                 "a2b27b5416503f8fb3762223ebdb69da"
                 "085ac1e43e15996e458f3613d0f11d8c"
                 "387b2eaeb4302aeeb00d291612bb0c00")},
     %% TEST 3
     {ed25519, undefined,
      hexstr2bin("c5aa8df43f9f837bedb7442f31dcb7b1"
                 "66d38535076f094b85ce3a2e0b4458f7"),
      hexstr2bin("fc51cd8e6218a1a38da47ed00230f058"
                 "0816ed13ba3303ac5deb911548908025"),
      hexstr2bin("af82"),
      hexstr2bin("6291d657deec24024827e69c3abe01a3"
                 "0ce548a284743a445e3680d7db5ac3ac"
                 "18ff9b538d16f290ae67f760984dc659"
                 "4a7c15e9716ed28dc027beceea1ec40a")},
     %% TEST 1024
     {ed25519, undefined,
      hexstr2bin("f5e5767cf153319517630f226876b86c"
                 "8160cc583bc013744c6bf255f5cc0ee5"),
      hexstr2bin("278117fc144c72340f67d0f2316e8386"
                 "ceffbf2b2428c9c51fef7c597f1d426e"),
      hexstr2bin("08b8b2b733424243760fe426a4b54908"
                 "632110a66c2f6591eabd3345e3e4eb98"
                 "fa6e264bf09efe12ee50f8f54e9f77b1"
                 "e355f6c50544e23fb1433ddf73be84d8"
                 "79de7c0046dc4996d9e773f4bc9efe57"
                 "38829adb26c81b37c93a1b270b20329d"
                 "658675fc6ea534e0810a4432826bf58c"
                 "941efb65d57a338bbd2e26640f89ffbc"
                 "1a858efcb8550ee3a5e1998bd177e93a"
                 "7363c344fe6b199ee5d02e82d522c4fe"
                 "ba15452f80288a821a579116ec6dad2b"
                 "3b310da903401aa62100ab5d1a36553e"
                 "06203b33890cc9b832f79ef80560ccb9"
                 "a39ce767967ed628c6ad573cb116dbef"
                 "efd75499da96bd68a8a97b928a8bbc10"
                 "3b6621fcde2beca1231d206be6cd9ec7"
                 "aff6f6c94fcd7204ed3455c68c83f4a4"
                 "1da4af2b74ef5c53f1d8ac70bdcb7ed1"
                 "85ce81bd84359d44254d95629e9855a9"
                 "4a7c1958d1f8ada5d0532ed8a5aa3fb2"
                 "d17ba70eb6248e594e1a2297acbbb39d"
                 "502f1a8c6eb6f1ce22b3de1a1f40cc24"
                 "554119a831a9aad6079cad88425de6bd"
                 "e1a9187ebb6092cf67bf2b13fd65f270"
                 "88d78b7e883c8759d2c4f5c65adb7553"
                 "878ad575f9fad878e80a0c9ba63bcbcc"
                 "2732e69485bbc9c90bfbd62481d9089b"
                 "eccf80cfe2df16a2cf65bd92dd597b07"
                 "07e0917af48bbb75fed413d238f5555a"
                 "7a569d80c3414a8d0859dc65a46128ba"
                 "b27af87a71314f318c782b23ebfe808b"
                 "82b0ce26401d2e22f04d83d1255dc51a"
                 "ddd3b75a2b1ae0784504df543af8969b"
                 "e3ea7082ff7fc9888c144da2af58429e"
                 "c96031dbcad3dad9af0dcbaaaf268cb8"
                 "fcffead94f3c7ca495e056a9b47acdb7"
                 "51fb73e666c6c655ade8297297d07ad1"
                 "ba5e43f1bca32301651339e22904cc8c"
                 "42f58c30c04aafdb038dda0847dd988d"
                 "cda6f3bfd15c4b4c4525004aa06eeff8"
                 "ca61783aacec57fb3d1f92b0fe2fd1a8"
                 "5f6724517b65e614ad6808d6f6ee34df"
                 "f7310fdc82aebfd904b01e1dc54b2927"
                 "094b2db68d6f903b68401adebf5a7e08"
                 "d78ff4ef5d63653a65040cf9bfd4aca7"
                 "984a74d37145986780fc0b16ac451649"
                 "de6188a7dbdf191f64b5fc5e2ab47b57"
                 "f7f7276cd419c17a3ca8e1b939ae49e4"
                 "88acba6b965610b5480109c8b17b80e1"
                 "b7b750dfc7598d5d5011fd2dcc5600a3"
                 "2ef5b52a1ecc820e308aa342721aac09"
                 "43bf6686b64b2579376504ccc493d97e"
                 "6aed3fb0f9cd71a43dd497f01f17c0e2"
                 "cb3797aa2a2f256656168e6c496afc5f"
                 "b93246f6b1116398a346f1a641f3b041"
                 "e989f7914f90cc2c7fff357876e506b5"
                 "0d334ba77c225bc307ba537152f3f161"
                 "0e4eafe595f6d9d90d11faa933a15ef1"
                 "369546868a7f3a45a96768d40fd9d034"
                 "12c091c6315cf4fde7cb68606937380d"
                 "b2eaaa707b4c4185c32eddcdd306705e"
                 "4dc1ffc872eeee475a64dfac86aba41c"
                 "0618983f8741c5ef68d3a101e8a3b8ca"
                 "c60c905c15fc910840b94c00a0b9d0"),
      hexstr2bin("0aab4c900501b3e24d7cdf4663326a3a"
                 "87df5e4843b2cbdb67cbf6e460fec350"
                 "aa5371b1508f9f4528ecea23c436d94b"
                 "5e8fcd4f681e30a6ac00a9704a188a03")},
     %% TEST SHA(abc)
     {ed25519, undefined,
      hexstr2bin("833fe62409237b9d62ec77587520911e"
                 "9a759cec1d19755b7da901b96dca3d42"),
      hexstr2bin("ec172b93ad5e563bf4932c70e1245034"
                 "c35467ef2efd4d64ebf819683467e2bf"),
      hexstr2bin("ddaf35a193617abacc417349ae204131"
                 "12e6fa4e89a97ea20a9eeee64b55d39a"
                 "2192992a274fc1a836ba3c23a3feebbd"
                 "454d4423643ce80e2a9ac94fa54ca49f"),
      hexstr2bin("dc2a4459e7369633a52b1bf277839a00"
                 "201009a3efbf3ecb69bea2186c26b589"
                 "09351fc9ac90b3ecfdfbc7c66431e030"
                 "3dca179c138ac17ad9bef1177331a704")}
    ];

eddsa(ed448) ->
    %% https://tools.ietf.org/html/rfc8032#section-7.4
    [{ed448, undefined,
      hexstr2bin("6c82a562cb808d10d632be89c8513ebf"
                 "6c929f34ddfa8c9f63c9960ef6e348a3"
                 "528c8a3fcc2f044e39a3fc5b94492f8f"
                 "032e7549a20098f95b"),
      hexstr2bin("5fd7449b59b461fd2ce787ec616ad46a"
                 "1da1342485a70e1f8a0ea75d80e96778"
                 "edf124769b46c7061bd6783df1e50f6c"
                 "d1fa1abeafe8256180"),
      hexstr2bin(""),
      hexstr2bin("533a37f6bbe457251f023c0d88f976ae"
                 "2dfb504a843e34d2074fd823d41a591f"
                 "2b233f034f628281f2fd7a22ddd47d78"
                 "28c59bd0a21bfd3980ff0d2028d4b18a"
                 "9df63e006c5d1c2d345b925d8dc00b41"
                 "04852db99ac5c7cdda8530a113a0f4db"
                 "b61149f05a7363268c71d95808ff2e65"
                 "2600")},
     %% 1 octet
     {ed448, undefined,
      hexstr2bin("c4eab05d357007c632f3dbb48489924d"
                 "552b08fe0c353a0d4a1f00acda2c463a"
                 "fbea67c5e8d2877c5e3bc397a659949e"
                 "f8021e954e0a12274e"),
      hexstr2bin("43ba28f430cdff456ae531545f7ecd0a"
                 "c834a55d9358c0372bfa0c6c6798c086"
                 "6aea01eb00742802b8438ea4cb82169c"
                 "235160627b4c3a9480"),
      hexstr2bin("03"),
      hexstr2bin("26b8f91727bd62897af15e41eb43c377"
                 "efb9c610d48f2335cb0bd0087810f435"
                 "2541b143c4b981b7e18f62de8ccdf633"
                 "fc1bf037ab7cd779805e0dbcc0aae1cb"
                 "cee1afb2e027df36bc04dcecbf154336"
                 "c19f0af7e0a6472905e799f1953d2a0f"
                 "f3348ab21aa4adafd1d234441cf807c0"
                 "3a00")},

     %% %% 1 octet (with context)
     %% {ed448, undefined,
     %%  hexstr2bin("c4eab05d357007c632f3dbb48489924d"
     %%             "552b08fe0c353a0d4a1f00acda2c463a"
     %%             "fbea67c5e8d2877c5e3bc397a659949e"
     %%             "f8021e954e0a12274e"),
     %%  hexstr2bin("43ba28f430cdff456ae531545f7ecd0a"
     %%             "c834a55d9358c0372bfa0c6c6798c086"
     %%             "6aea01eb00742802b8438ea4cb82169c"
     %%             "235160627b4c3a9480"),
     %%  hexstr2bin("03"),
     %%  hexstr2bin("666f6f"), % Context
     %%  hexstr2bin("d4f8f6131770dd46f40867d6fd5d5055"
     %%             "de43541f8c5e35abbcd001b32a89f7d2"
     %%             "151f7647f11d8ca2ae279fb842d60721"
     %%             "7fce6e042f6815ea000c85741de5c8da"
     %%             "1144a6a1aba7f96de42505d7a7298524"
     %%             "fda538fccbbb754f578c1cad10d54d0d"
     %%             "5428407e85dcbc98a49155c13764e66c"
     %%             "3c00")},

     %% 11 octets
     {ed448, undefined,
      hexstr2bin("cd23d24f714274e744343237b93290f5"
                 "11f6425f98e64459ff203e8985083ffd"
                 "f60500553abc0e05cd02184bdb89c4cc"
                 "d67e187951267eb328"),
      hexstr2bin("dcea9e78f35a1bf3499a831b10b86c90"
                 "aac01cd84b67a0109b55a36e9328b1e3"
                 "65fce161d71ce7131a543ea4cb5f7e9f"
                 "1d8b00696447001400"),
      hexstr2bin("0c3e544074ec63b0265e0c"),
      hexstr2bin("1f0a8888ce25e8d458a21130879b840a"
                 "9089d999aaba039eaf3e3afa090a09d3"
                 "89dba82c4ff2ae8ac5cdfb7c55e94d5d"
                 "961a29fe0109941e00b8dbdeea6d3b05"
                 "1068df7254c0cdc129cbe62db2dc957d"
                 "bb47b51fd3f213fb8698f064774250a5"
                 "028961c9bf8ffd973fe5d5c206492b14"
                 "0e00")},
     %% 12 octets
     {ed448, undefined,
      hexstr2bin("258cdd4ada32ed9c9ff54e63756ae582"
                 "fb8fab2ac721f2c8e676a72768513d93"
                 "9f63dddb55609133f29adf86ec9929dc"
                 "cb52c1c5fd2ff7e21b"),
      hexstr2bin("3ba16da0c6f2cc1f30187740756f5e79"
                 "8d6bc5fc015d7c63cc9510ee3fd44adc"
                 "24d8e968b6e46e6f94d19b945361726b"
                 "d75e149ef09817f580"),
      hexstr2bin("64a65f3cdedcdd66811e2915"),
      hexstr2bin("7eeeab7c4e50fb799b418ee5e3197ff6"
                 "bf15d43a14c34389b59dd1a7b1b85b4a"
                 "e90438aca634bea45e3a2695f1270f07"
                 "fdcdf7c62b8efeaf00b45c2c96ba457e"
                 "b1a8bf075a3db28e5c24f6b923ed4ad7"
                 "47c3c9e03c7079efb87cb110d3a99861"
                 "e72003cbae6d6b8b827e4e6c143064ff"
                 "3c00")},
     %% 13 octets
     {ed448, undefined,
      hexstr2bin("7ef4e84544236752fbb56b8f31a23a10"
                 "e42814f5f55ca037cdcc11c64c9a3b29"
                 "49c1bb60700314611732a6c2fea98eeb"
                 "c0266a11a93970100e"),
      hexstr2bin("b3da079b0aa493a5772029f0467baebe"
                 "e5a8112d9d3a22532361da294f7bb381"
                 "5c5dc59e176b4d9f381ca0938e13c6c0"
                 "7b174be65dfa578e80"),
      hexstr2bin("64a65f3cdedcdd66811e2915e7"),
      hexstr2bin("6a12066f55331b6c22acd5d5bfc5d712"
                 "28fbda80ae8dec26bdd306743c5027cb"
                 "4890810c162c027468675ecf645a8317"
                 "6c0d7323a2ccde2d80efe5a1268e8aca"
                 "1d6fbc194d3f77c44986eb4ab4177919"
                 "ad8bec33eb47bbb5fc6e28196fd1caf5"
                 "6b4e7e0ba5519234d047155ac727a105"
                 "3100")},
     %% 64 octets
     {ed448, undefined,
      hexstr2bin("d65df341ad13e008567688baedda8e9d"
                 "cdc17dc024974ea5b4227b6530e339bf"
                 "f21f99e68ca6968f3cca6dfe0fb9f4fa"
                 "b4fa135d5542ea3f01"),
      hexstr2bin("df9705f58edbab802c7f8363cfe5560a"
                 "b1c6132c20a9f1dd163483a26f8ac53a"
                 "39d6808bf4a1dfbd261b099bb03b3fb5"
                 "0906cb28bd8a081f00"),
      hexstr2bin("bd0f6a3747cd561bdddf4640a332461a"
                 "4a30a12a434cd0bf40d766d9c6d458e5"
                 "512204a30c17d1f50b5079631f64eb31"
                 "12182da3005835461113718d1a5ef944"),
      hexstr2bin("554bc2480860b49eab8532d2a533b7d5"
                 "78ef473eeb58c98bb2d0e1ce488a98b1"
                 "8dfde9b9b90775e67f47d4a1c3482058"
                 "efc9f40d2ca033a0801b63d45b3b722e"
                 "f552bad3b4ccb667da350192b61c508c"
                 "f7b6b5adadc2c8d9a446ef003fb05cba"
                 "5f30e88e36ec2703b349ca229c267083"
                 "3900")},
     %% 256 octets
     {ed448, undefined,
      hexstr2bin("2ec5fe3c17045abdb136a5e6a913e32a"
                 "b75ae68b53d2fc149b77e504132d3756"
                 "9b7e766ba74a19bd6162343a21c8590a"
                 "a9cebca9014c636df5"),
      hexstr2bin("79756f014dcfe2079f5dd9e718be4171"
                 "e2ef2486a08f25186f6bff43a9936b9b"
                 "fe12402b08ae65798a3d81e22e9ec80e"
                 "7690862ef3d4ed3a00"),
      hexstr2bin("15777532b0bdd0d1389f636c5f6b9ba7"
                 "34c90af572877e2d272dd078aa1e567c"
                 "fa80e12928bb542330e8409f31745041"
                 "07ecd5efac61ae7504dabe2a602ede89"
                 "e5cca6257a7c77e27a702b3ae39fc769"
                 "fc54f2395ae6a1178cab4738e543072f"
                 "c1c177fe71e92e25bf03e4ecb72f47b6"
                 "4d0465aaea4c7fad372536c8ba516a60"
                 "39c3c2a39f0e4d832be432dfa9a706a6"
                 "e5c7e19f397964ca4258002f7c0541b5"
                 "90316dbc5622b6b2a6fe7a4abffd9610"
                 "5eca76ea7b98816af0748c10df048ce0"
                 "12d901015a51f189f3888145c03650aa"
                 "23ce894c3bd889e030d565071c59f409"
                 "a9981b51878fd6fc110624dcbcde0bf7"
                 "a69ccce38fabdf86f3bef6044819de11"),
      hexstr2bin("c650ddbb0601c19ca11439e1640dd931"
                 "f43c518ea5bea70d3dcde5f4191fe53f"
                 "00cf966546b72bcc7d58be2b9badef28"
                 "743954e3a44a23f880e8d4f1cfce2d7a"
                 "61452d26da05896f0a50da66a239a8a1"
                 "88b6d825b3305ad77b73fbac0836ecc6"
                 "0987fd08527c1a8e80d5823e65cafe2a"
                 "3d00")},
     %% 1023 octets
     {ed448, undefined,
      hexstr2bin("872d093780f5d3730df7c212664b37b8"
                 "a0f24f56810daa8382cd4fa3f77634ec"
                 "44dc54f1c2ed9bea86fafb7632d8be19"
                 "9ea165f5ad55dd9ce8"),
      hexstr2bin("a81b2e8a70a5ac94ffdbcc9badfc3feb"
                 "0801f258578bb114ad44ece1ec0e799d"
                 "a08effb81c5d685c0c56f64eecaef8cd"
                 "f11cc38737838cf400"),
      hexstr2bin("6ddf802e1aae4986935f7f981ba3f035"
                 "1d6273c0a0c22c9c0e8339168e675412"
                 "a3debfaf435ed651558007db4384b650"
                 "fcc07e3b586a27a4f7a00ac8a6fec2cd"
                 "86ae4bf1570c41e6a40c931db27b2faa"
                 "15a8cedd52cff7362c4e6e23daec0fbc"
                 "3a79b6806e316efcc7b68119bf46bc76"
                 "a26067a53f296dafdbdc11c77f7777e9"
                 "72660cf4b6a9b369a6665f02e0cc9b6e"
                 "dfad136b4fabe723d2813db3136cfde9"
                 "b6d044322fee2947952e031b73ab5c60"
                 "3349b307bdc27bc6cb8b8bbd7bd32321"
                 "9b8033a581b59eadebb09b3c4f3d2277"
                 "d4f0343624acc817804728b25ab79717"
                 "2b4c5c21a22f9c7839d64300232eb66e"
                 "53f31c723fa37fe387c7d3e50bdf9813"
                 "a30e5bb12cf4cd930c40cfb4e1fc6225"
                 "92a49588794494d56d24ea4b40c89fc0"
                 "596cc9ebb961c8cb10adde976a5d602b"
                 "1c3f85b9b9a001ed3c6a4d3b1437f520"
                 "96cd1956d042a597d561a596ecd3d173"
                 "5a8d570ea0ec27225a2c4aaff26306d1"
                 "526c1af3ca6d9cf5a2c98f47e1c46db9"
                 "a33234cfd4d81f2c98538a09ebe76998"
                 "d0d8fd25997c7d255c6d66ece6fa56f1"
                 "1144950f027795e653008f4bd7ca2dee"
                 "85d8e90f3dc315130ce2a00375a318c7"
                 "c3d97be2c8ce5b6db41a6254ff264fa6"
                 "155baee3b0773c0f497c573f19bb4f42"
                 "40281f0b1f4f7be857a4e59d416c06b4"
                 "c50fa09e1810ddc6b1467baeac5a3668"
                 "d11b6ecaa901440016f389f80acc4db9"
                 "77025e7f5924388c7e340a732e554440"
                 "e76570f8dd71b7d640b3450d1fd5f041"
                 "0a18f9a3494f707c717b79b4bf75c984"
                 "00b096b21653b5d217cf3565c9597456"
                 "f70703497a078763829bc01bb1cbc8fa"
                 "04eadc9a6e3f6699587a9e75c94e5bab"
                 "0036e0b2e711392cff0047d0d6b05bd2"
                 "a588bc109718954259f1d86678a579a3"
                 "120f19cfb2963f177aeb70f2d4844826"
                 "262e51b80271272068ef5b3856fa8535"
                 "aa2a88b2d41f2a0e2fda7624c2850272"
                 "ac4a2f561f8f2f7a318bfd5caf969614"
                 "9e4ac824ad3460538fdc25421beec2cc"
                 "6818162d06bbed0c40a387192349db67"
                 "a118bada6cd5ab0140ee273204f628aa"
                 "d1c135f770279a651e24d8c14d75a605"
                 "9d76b96a6fd857def5e0b354b27ab937"
                 "a5815d16b5fae407ff18222c6d1ed263"
                 "be68c95f32d908bd895cd76207ae7264"
                 "87567f9a67dad79abec316f683b17f2d"
                 "02bf07e0ac8b5bc6162cf94697b3c27c"
                 "d1fea49b27f23ba2901871962506520c"
                 "392da8b6ad0d99f7013fbc06c2c17a56"
                 "9500c8a7696481c1cd33e9b14e40b82e"
                 "79a5f5db82571ba97bae3ad3e0479515"
                 "bb0e2b0f3bfcd1fd33034efc6245eddd"
                 "7ee2086ddae2600d8ca73e214e8c2b0b"
                 "db2b047c6a464a562ed77b73d2d841c4"
                 "b34973551257713b753632efba348169"
                 "abc90a68f42611a40126d7cb21b58695"
                 "568186f7e569d2ff0f9e745d0487dd2e"
                 "b997cafc5abf9dd102e62ff66cba87"),
      hexstr2bin("e301345a41a39a4d72fff8df69c98075"
                 "a0cc082b802fc9b2b6bc503f926b65bd"
                 "df7f4c8f1cb49f6396afc8a70abe6d8a"
                 "ef0db478d4c6b2970076c6a0484fe76d"
                 "76b3a97625d79f1ce240e7c576750d29"
                 "5528286f719b413de9ada3e8eb78ed57"
                 "3603ce30d8bb761785dc30dbc320869e"
                 "1a00")}
    ].

ecdh() ->
    %% http://csrc.nist.gov/groups/STM/cavp/
    Curves = crypto:supports(curves),
    lists:filter(
      fun ({_Type, _Pub, _Priv, Curve, _SharedSecret}) ->
              lists:member(Curve, Curves)
      end,

      [{ecdh, hexstr2point("42ea6dd9969dd2a61fea1aac7f8e98edcc896c6e55857cc0", "dfbe5d7c61fac88b11811bde328e8a0d12bf01a9d204b523"),
          hexstr2bin("f17d3fea367b74d340851ca4270dcb24c271f445bed9d527"),
          secp192r1,
          hexstr2bin("803d8ab2e5b6e6fca715737c3a82f7ce3c783124f6d51cd0")},
         {ecdh, hexstr2point("deb5712fa027ac8d2f22c455ccb73a91e17b6512b5e030e7", "7e2690a02cc9b28708431a29fb54b87b1f0c14e011ac2125"),
          hexstr2bin("56e853349d96fe4c442448dacb7cf92bb7a95dcf574a9bd5"),
          secp192r1,
          hexstr2bin("c208847568b98835d7312cef1f97f7aa298283152313c29d")},
         {ecdh, hexstr2point("af33cd0629bc7e996320a3f40368f74de8704fa37b8fab69abaae280", "882092ccbba7930f419a8a4f9bb16978bbc3838729992559a6f2e2d7"),
          hexstr2bin("8346a60fc6f293ca5a0d2af68ba71d1dd389e5e40837942df3e43cbd"),
          secp224r1,
          hexstr2bin("7d96f9a3bd3c05cf5cc37feb8b9d5209d5c2597464dec3e9983743e8")},
         {ecdh, hexstr2point("13bfcd4f8e9442393cab8fb46b9f0566c226b22b37076976f0617a46", "eeb2427529b288c63c2f8963c1e473df2fca6caa90d52e2f8db56dd4"),
          hexstr2bin("043cb216f4b72cdf7629d63720a54aee0c99eb32d74477dac0c2f73d"),
          secp224r1,
          hexstr2bin("ee93ce06b89ff72009e858c68eb708e7bc79ee0300f73bed69bbca09")},
         {ecdh, hexstr2point("700c48f77f56584c5cc632ca65640db91b6bacce3a4df6b42ce7cc838833d287", "db71e509e3fd9b060ddb20ba5c51dcc5948d46fbf640dfe0441782cab85fa4ac"),
          hexstr2bin("7d7dc5f71eb29ddaf80d6214632eeae03d9058af1fb6d22ed80badb62bc1a534"),
          secp256r1,
          hexstr2bin("46fc62106420ff012e54a434fbdd2d25ccc5852060561e68040dd7778997bd7b")},
         {ecdh, hexstr2point("809f04289c64348c01515eb03d5ce7ac1a8cb9498f5caa50197e58d43a86a7ae", "b29d84e811197f25eba8f5194092cb6ff440e26d4421011372461f579271cda3"),
          hexstr2bin("38f65d6dce47676044d58ce5139582d568f64bb16098d179dbab07741dd5caf5"),
          secp256r1,
          hexstr2bin("057d636096cb80b67a8c038c890e887d1adfa4195e9b3ce241c8a778c59cda67")},
         {ecdh, hexstr2point("a7c76b970c3b5fe8b05d2838ae04ab47697b9eaf52e764592efda27fe7513272734466b400091adbf2d68c58e0c50066", "ac68f19f2e1cb879aed43a9969b91a0839c4c38a49749b661efedf243451915ed0905a32b060992b468c64766fc8437a"),
          hexstr2bin("3cc3122a68f0d95027ad38c067916ba0eb8c38894d22e1b15618b6818a661774ad463b205da88cf699ab4d43c9cf98a1"),
          secp384r1,
          hexstr2bin("5f9d29dc5e31a163060356213669c8ce132e22f57c9a04f40ba7fcead493b457e5621e766c40a2e3d4d6a04b25e533f1")},
         {ecdh, hexstr2point("30f43fcf2b6b00de53f624f1543090681839717d53c7c955d1d69efaf0349b7363acb447240101cbb3af6641ce4b88e0", "25e46c0c54f0162a77efcc27b6ea792002ae2ba82714299c860857a68153ab62e525ec0530d81b5aa15897981e858757"),
          hexstr2bin("92860c21bde06165f8e900c687f8ef0a05d14f290b3f07d8b3a8cc6404366e5d5119cd6d03fb12dc58e89f13df9cd783"),
          secp384r1,
          hexstr2bin("a23742a2c267d7425fda94b93f93bbcc24791ac51cd8fd501a238d40812f4cbfc59aac9520d758cf789c76300c69d2ff")},
         {ecdh, hexstr2point("00685a48e86c79f0f0875f7bc18d25eb5fc8c0b07e5da4f4370f3a9490340854334b1e1b87fa395464c60626124a4e70d0f785601d37c09870ebf176666877a2046d", "01ba52c56fc8776d9e8f5db4f0cc27636d0b741bbe05400697942e80b739884a83bde99e0f6716939e632bc8986fa18dccd443a348b6c3e522497955a4f3c302f676"),
          hexstr2bin("017eecc07ab4b329068fba65e56a1f8890aa935e57134ae0ffcce802735151f4eac6564f6ee9974c5e6887a1fefee5743ae2241bfeb95d5ce31ddcb6f9edb4d6fc47"),
          secp521r1,
          hexstr2bin("005fc70477c3e63bc3954bd0df3ea0d1f41ee21746ed95fc5e1fdf90930d5e136672d72cc770742d1711c3c3a4c334a0ad9759436a4d3c5bf6e74b9578fac148c831")},
         {ecdh, hexstr2point("01df277c152108349bc34d539ee0cf06b24f5d3500677b4445453ccc21409453aafb8a72a0be9ebe54d12270aa51b3ab7f316aa5e74a951c5e53f74cd95fc29aee7a", "013d52f33a9f3c14384d1587fa8abe7aed74bc33749ad9c570b471776422c7d4505d9b0a96b3bfac041e4c6a6990ae7f700e5b4a6640229112deafa0cd8bb0d089b0"),
          hexstr2bin("00816f19c1fb10ef94d4a1d81c156ec3d1de08b66761f03f06ee4bb9dcebbbfe1eaa1ed49a6a990838d8ed318c14d74cc872f95d05d07ad50f621ceb620cd905cfb8"),
          secp521r1,
          hexstr2bin("000b3920ac830ade812c8f96805da2236e002acbbf13596a9ab254d44d0e91b6255ebf1229f366fb5a05c5884ef46032c26d42189273ca4efa4c3db6bd12a6853759")},

         %% RFC-6954, Appendix A
         {ecdh, hexstr2point("A9C21A569759DA95E0387041184261440327AFE33141CA04B82DC92E",
                             "98A0F75FBBF61D8E58AE5511B2BCDBE8E549B31E37069A2825F590C1"),
          hexstr2bin("6060552303899E2140715816C45B57D9B42204FB6A5BF5BEAC10DB00"),
          brainpoolP224r1,
          hexstr2bin("1A4BFE705445120C8E3E026699054104510D119757B74D5FE2462C66")},
         {ecdh, hexstr2point("034A56C550FF88056144E6DD56070F54B0135976B5BF77827313F36B",
                             "75165AD99347DC86CAAB1CBB579E198EAF88DC35F927B358AA683681"),
          hexstr2bin("39F155483CEE191FBECFE9C81D8AB1A03CDA6790E7184ACE44BCA161"),
          brainpoolP224r1,
          hexstr2bin("1A4BFE705445120C8E3E026699054104510D119757B74D5FE2462C66")},
         {ecdh, hexstr2point("44106E913F92BC02A1705D9953A8414DB95E1AAA49E81D9E85F929A8E3100BE5",
                             "8AB4846F11CACCB73CE49CBDD120F5A900A69FD32C272223F789EF10EB089BDC"),
          hexstr2bin("55E40BC41E37E3E2AD25C3C6654511FFA8474A91A0032087593852D3E7D76BD3"),
          brainpoolP256r1,
          hexstr2bin("89AFC39D41D3B327814B80940B042590F96556EC91E6AE7939BCE31F3A18BF2B")},
         {ecdh, hexstr2point("8D2D688C6CF93E1160AD04CC4429117DC2C41825E1E9FCA0ADDD34E6F1B39F7B",
                             "990C57520812BE512641E47034832106BC7D3E8DD0E4C7F1136D7006547CEC6A"),
          hexstr2bin("81DB1EE100150FF2EA338D708271BE38300CB54241D79950F77B063039804F1D"),
          brainpoolP256r1,
          hexstr2bin("89AFC39D41D3B327814B80940B042590F96556EC91E6AE7939BCE31F3A18BF2B")},
         {ecdh, hexstr2point("68B665DD91C195800650CDD363C625F4E742E8134667B767B1B476793588F885AB698C852D4A6E77A252D6380FCAF068",
                             "55BC91A39C9EC01DEE36017B7D673A931236D2F1F5C83942D049E3FA20607493E0D038FF2FD30C2AB67D15C85F7FAA59"),
          hexstr2bin("032640BC6003C59260F7250C3DB58CE647F98E1260ACCE4ACDA3DD869F74E01F8BA5E0324309DB6A9831497ABAC96670"),
          brainpoolP384r1,
          hexstr2bin("0BD9D3A7EA0B3D519D09D8E48D0785FB744A6B355E6304BC51C229FBBCE239BBADF6403715C35D4FB2A5444F575D4F42")},
         {ecdh, hexstr2point("4D44326F269A597A5B58BBA565DA5556ED7FD9A8A9EB76C25F46DB69D19DC8CE6AD18E404B15738B2086DF37E71D1EB4",
                             "62D692136DE56CBE93BF5FA3188EF58BC8A3A0EC6C1E151A21038A42E9185329B5B275903D192F8D4E1F32FE9CC78C48"),
          hexstr2bin("1E20F5E048A5886F1F157C74E91BDE2B98C8B52D58E5003D57053FC4B0BD65D6F15EB5D1EE1610DF870795143627D042"),
          brainpoolP384r1,
          hexstr2bin("0BD9D3A7EA0B3D519D09D8E48D0785FB744A6B355E6304BC51C229FBBCE239BBADF6403715C35D4FB2A5444F575D4F42")},
         {ecdh, hexstr2point("0A420517E406AAC0ACDCE90FCD71487718D3B953EFD7FBEC5F7F27E28C6149999397E91E029E06457DB2D3E640668B392C2A7E737A7F0BF04436D11640FD09FD",
                             "72E6882E8DB28AAD36237CD25D580DB23783961C8DC52DFA2EC138AD472A0FCEF3887CF62B623B2A87DE5C588301EA3E5FC269B373B60724F5E82A6AD147FDE7"),
          hexstr2bin("230E18E1BCC88A362FA54E4EA3902009292F7F8033624FD471B5D8ACE49D12CFABBC19963DAB8E2F1EBA00BFFB29E4D72D13F2224562F405CB80503666B25429"),
          brainpoolP512r1,
          hexstr2bin("A7927098655F1F9976FA50A9D566865DC530331846381C87256BAF3226244B76D36403C024D7BBF0AA0803EAFF405D3D24F11A9B5C0BEF679FE1454B21C4CD1F")},
         {ecdh, hexstr2point("9D45F66DE5D67E2E6DB6E93A59CE0BB48106097FF78A081DE781CDB31FCE8CCBAAEA8DD4320C4119F1E9CD437A2EAB3731FA9668AB268D871DEDA55A5473199F",
                             "2FDC313095BCDD5FB3A91636F07A959C8E86B5636A1E930E8396049CB481961D365CC11453A06C719835475B12CB52FC3C383BCE35E27EF194512B71876285FA"),
          hexstr2bin("16302FF0DBBB5A8D733DAB7141C1B45ACBC8715939677F6A56850A38BD87BD59B09E80279609FF333EB9D4C061231FB26F92EEB04982A5F1D1764CAD57665422"),
          brainpoolP512r1,
          hexstr2bin("A7927098655F1F9976FA50A9D566865DC530331846381C87256BAF3226244B76D36403C024D7BBF0AA0803EAFF405D3D24F11A9B5C0BEF679FE1454B21C4CD1F")},

         %% RFC 7748, 6.1
         {ecdh,
          16#8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a,
          16#5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb,
          x25519,
          hexstr2bin("4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")},
         {ecdh,
          16#de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f,
          16#77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a,
          x25519,
          hexstr2bin("4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")},

         %% RFC 7748, 6.2
         {ecdh,
          16#9b08f7cc31b7e3e67d22d5aea121074a273bd2b83de09c63faa73d2c22c5d9bbc836647241d953d40c5b12da88120d53177f80e532c41fa0,
          16#1c306a7ac2a0e2e0990b294470cba339e6453772b075811d8fad0d1d6927c120bb5ee8972b0d3e21374c9c921b09d1b0366f10b65173992d,
          x448,
          hexstr2bin("07fff4181ac6cc95ec1c16a94a0f74d12da232ce40a77552281d282bb60c0b56fd2464c335543936521c24403085d59a449a5037514a879d")},
         {ecdh,
          16#3eb7a829b0cd20f5bcfc0b599b6feccf6da4627107bdb0d4f345b43027d8b972fc3e34fb4232a13ca706dcb57aec3dae07bdc1c67bf33609,
          16#9a8f4925d1519f5775cf46b04b5800d4ee9ee8bae8bc5565d498c28dd9c9baf574a9419744897391006382a6f127ab1d9ac2d8c0a598726b,
          x448,
          hexstr2bin("07fff4181ac6cc95ec1c16a94a0f74d12da232ce40a77552281d282bb60c0b56fd2464c335543936521c24403085d59a449a5037514a879d")}
        ]
        ).

dh() ->
    {dh, 16#FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF97D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD65612433F51F5F066ED0856365553DED1AF3B557135E7F57C935984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE73530ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FBB96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB190B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F619172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD733BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA886B423861285C97FFFFFFFFFFFFFFFF, 2}.


rsa_oaep() ->
    %% ftp://ftp.rsa.com/pub/rsalabs/tmp/pkcs1v15crypt-vectors.txt
    Public = [hexstr2bin("010001"),
	      hexstr2bin("a8b3b284af8eb50b387034a860f146c4919f318763cd6c5598c8ae4811a1e0abc4c7e0b082d693a5e7fced675cf4668512772c0cbc64a742c6c630f533c8cc72f62ae833c40bf25842e984bb78bdbf97c0107d55bdb662f5c4e0fab9845cb5148ef7392dd3aaff93ae1e6b667bb3d4247616d4f5ba10d4cfd226de88d39f16fb")],
    Private = Public ++ [hexstr2bin("53339cfdb79fc8466a655c7316aca85c55fd8f6dd898fdaf119517ef4f52e8fd8e258df93fee180fa0e4ab29693cd83b152a553d4ac4d1812b8b9fa5af0e7f55fe7304df41570926f3311f15c4d65a732c483116ee3d3d2d0af3549ad9bf7cbfb78ad884f84d5beb04724dc7369b31def37d0cf539e9cfcdd3de653729ead5d1"),
			 hexstr2bin("d32737e7267ffe1341b2d5c0d150a81b586fb3132bed2f8d5262864a9cb9f30af38be448598d413a172efb802c21acf1c11c520c2f26a471dcad212eac7ca39d"),
			 hexstr2bin("cc8853d1d54da630fac004f471f281c7b8982d8224a490edbeb33d3e3d5cc93c4765703d1dd791642f1f116a0dd852be2419b2af72bfe9a030e860b0288b5d77"),
			 hexstr2bin("0e12bf1718e9cef5599ba1c3882fe8046a90874eefce8f2ccc20e4f2741fb0a33a3848aec9c9305fbecbd2d76819967d4671acc6431e4037968db37878e695c1"),
			 hexstr2bin("95297b0f95a2fa67d00707d609dfd4fc05c89dafc2ef6d6ea55bec771ea333734d9251e79082ecda866efef13c459e1a631386b7e354c899f5f112ca85d71583"),
			 hexstr2bin("4f456c502493bdc0ed2ab756a3a6ed4d67352a697d4216e93212b127a63d5411ce6fa98d5dbefd73263e3728142743818166ed7dd63687dd2a8ca1d2f4fbd8e1")],
    %%Msg = hexstr2bin("6628194e12073db03ba94cda9ef9532397d50dba79b987004afefe34"),
    Msg =  hexstr2bin("750c4047f547e8e41411856523298ac9bae245efaf1397fbe56f9dd5"),
    {rsa, Public, Private, Msg, [{rsa_padding, rsa_pkcs1_oaep_padding}]}.

rsa_oaep_label() ->
    Public = [hexstr2bin("010001"),
	      hexstr2bin("a8b3b284af8eb50b387034a860f146c4919f318763cd6c5598c8ae4811a1e0abc4c7e0b082d693a5e7fced675cf4668512772c0cbc64a742c6c630f533c8cc72f62ae833c40bf25842e984bb78bdbf97c0107d55bdb662f5c4e0fab9845cb5148ef7392dd3aaff93ae1e6b667bb3d4247616d4f5ba10d4cfd226de88d39f16fb")],
    Private = Public ++ [hexstr2bin("53339cfdb79fc8466a655c7316aca85c55fd8f6dd898fdaf119517ef4f52e8fd8e258df93fee180fa0e4ab29693cd83b152a553d4ac4d1812b8b9fa5af0e7f55fe7304df41570926f3311f15c4d65a732c483116ee3d3d2d0af3549ad9bf7cbfb78ad884f84d5beb04724dc7369b31def37d0cf539e9cfcdd3de653729ead5d1"),
			 hexstr2bin("d32737e7267ffe1341b2d5c0d150a81b586fb3132bed2f8d5262864a9cb9f30af38be448598d413a172efb802c21acf1c11c520c2f26a471dcad212eac7ca39d"),
			 hexstr2bin("cc8853d1d54da630fac004f471f281c7b8982d8224a490edbeb33d3e3d5cc93c4765703d1dd791642f1f116a0dd852be2419b2af72bfe9a030e860b0288b5d77"),
			 hexstr2bin("0e12bf1718e9cef5599ba1c3882fe8046a90874eefce8f2ccc20e4f2741fb0a33a3848aec9c9305fbecbd2d76819967d4671acc6431e4037968db37878e695c1"),
			 hexstr2bin("95297b0f95a2fa67d00707d609dfd4fc05c89dafc2ef6d6ea55bec771ea333734d9251e79082ecda866efef13c459e1a631386b7e354c899f5f112ca85d71583"),
			 hexstr2bin("4f456c502493bdc0ed2ab756a3a6ed4d67352a697d4216e93212b127a63d5411ce6fa98d5dbefd73263e3728142743818166ed7dd63687dd2a8ca1d2f4fbd8e1")],
    Msg = hexstr2bin("750c4047f547e8e41411856523298ac9bae245efaf1397fbe56f9dd5"),
    Lbl = hexstr2bin("1332a67ca7088f75c9b8fb5e3d072882"),
    {rsa, Public, Private, Msg, [{rsa_padding, rsa_pkcs1_oaep_padding}, {rsa_oaep_label, Lbl}]}.

rsa_oaep256() ->
    Public = [hexstr2bin("010001"),
	      hexstr2bin("a8b3b284af8eb50b387034a860f146c4919f318763cd6c5598c8ae4811a1e0abc4c7e0b082d693a5e7fced675cf4668512772c0cbc64a742c6c630f533c8cc72f62ae833c40bf25842e984bb78bdbf97c0107d55bdb662f5c4e0fab9845cb5148ef7392dd3aaff93ae1e6b667bb3d4247616d4f5ba10d4cfd226de88d39f16fb")],
    Private = Public ++ [hexstr2bin("53339cfdb79fc8466a655c7316aca85c55fd8f6dd898fdaf119517ef4f52e8fd8e258df93fee180fa0e4ab29693cd83b152a553d4ac4d1812b8b9fa5af0e7f55fe7304df41570926f3311f15c4d65a732c483116ee3d3d2d0af3549ad9bf7cbfb78ad884f84d5beb04724dc7369b31def37d0cf539e9cfcdd3de653729ead5d1"),
			 hexstr2bin("d32737e7267ffe1341b2d5c0d150a81b586fb3132bed2f8d5262864a9cb9f30af38be448598d413a172efb802c21acf1c11c520c2f26a471dcad212eac7ca39d"),
			 hexstr2bin("cc8853d1d54da630fac004f471f281c7b8982d8224a490edbeb33d3e3d5cc93c4765703d1dd791642f1f116a0dd852be2419b2af72bfe9a030e860b0288b5d77"),
			 hexstr2bin("0e12bf1718e9cef5599ba1c3882fe8046a90874eefce8f2ccc20e4f2741fb0a33a3848aec9c9305fbecbd2d76819967d4671acc6431e4037968db37878e695c1"),
			 hexstr2bin("95297b0f95a2fa67d00707d609dfd4fc05c89dafc2ef6d6ea55bec771ea333734d9251e79082ecda866efef13c459e1a631386b7e354c899f5f112ca85d71583"),
			 hexstr2bin("4f456c502493bdc0ed2ab756a3a6ed4d67352a697d4216e93212b127a63d5411ce6fa98d5dbefd73263e3728142743818166ed7dd63687dd2a8ca1d2f4fbd8e1")],
    Msg = hexstr2bin("750c4047f547e8e41411856523298ac9bae245efaf1397fbe56f9dd5"),
    {rsa, Public, Private, Msg, [{rsa_padding, rsa_pkcs1_oaep_padding}, {rsa_oaep_md, sha256}]}.

ecc() ->
%%               http://point-at-infinity.org/ecc/nisttv
%%
%% Test vectors for the NIST elliptic curves P192, P224, P256, P384, P521,
%% B163, B233, B283, B409, B571, K163, K233, K283, K409 and K571. For more
%% information about the curves see
%%       http://csrc.nist.gov/encryption/dss/ecdsa/NISTReCur.pdf
%%
    Curves = crypto:supports(curves),
    lists:filter(
      fun ({_Type, Curve, _Priv, _Pub}) ->
              lists:member(Curve, Curves)
      end,
        [{ecdh,secp192r1,1,
          hexstr2point("188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012",
                       "07192B95FFC8DA78631011ED6B24CDD573F977A11E794811")},
         {ecdh,secp192r1,2,
          hexstr2point("DAFEBF5828783F2AD35534631588A3F629A70FB16982A888",
                       "DD6BDA0D993DA0FA46B27BBC141B868F59331AFA5C7E93AB")},
         {ecdh,secp192r1,3,
          hexstr2point("76E32A2557599E6EDCD283201FB2B9AADFD0D359CBB263DA",
                       "782C37E372BA4520AA62E0FED121D49EF3B543660CFD05FD")},
         {ecdh,secp192r1,4,
          hexstr2point("35433907297CC378B0015703374729D7A4FE46647084E4BA",
                       "A2649984F2135C301EA3ACB0776CD4F125389B311DB3BE32")},
         %% RFC 7748, 6.2
         {ecdh, x448,
          hexstr2bin("9a8f4925d1519f5775cf46b04b5800d4ee9ee8bae8bc5565d498c28d"
                     "d9c9baf574a9419744897391006382a6f127ab1d9ac2d8c0a598726b"),
          hexstr2bin("9b08f7cc31b7e3e67d22d5aea121074a273bd2b83de09c63faa73d2c"
                     "22c5d9bbc836647241d953d40c5b12da88120d53177f80e532c41fa0")},
         {ecdh, x448,
          hexstr2bin("1c306a7ac2a0e2e0990b294470cba339e6453772b075811d8fad0d1d"
                     "6927c120bb5ee8972b0d3e21374c9c921b09d1b0366f10b65173992d"),
          hexstr2bin("3eb7a829b0cd20f5bcfc0b599b6feccf6da4627107bdb0d4f345b430"
                     "27d8b972fc3e34fb4232a13ca706dcb57aec3dae07bdc1c67bf33609")},
         %% RFC 7748, 6.1
         {ecdh, x25519,
          hexstr2bin("77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"),
          hexstr2bin("8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")},
         {ecdh, x25519,
          hexstr2bin("5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"),
          hexstr2bin("de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f")}
        ]
     ).

datadir(Config) ->
    proplists:get_value(data_dir, Config).

-define(KiB, 1024).
-define(MiB, (1024 * 1024)).
-define(GiB, (1024 * 1024 * 1024)).

fmt_words(Words) ->
    BSize = Words * erlang:system_info(wordsize),
    if BSize < ?KiB ->
            integer_to_list(BSize);
       BSize < ?MiB ->
            io_lib:format("~8.2fKiB (~8w)", [BSize / ?KiB, BSize]);
       BSize < ?GiB ->
            io_lib:format("~8.2fMiB (~8w)", [BSize / ?MiB, BSize]);
       true ->
            io_lib:format("~8.2fGiB (~8w)", [BSize / ?GiB, BSize])
    end.

log_rsp_size(Label, Term) ->
    S = erts_debug:size(Term),
    ct:log("~s: ~w test(s), Memory used: ~s",
           [Label, length(Term), fmt_words(S)]).

read_rsp(Config, Type, Files) ->
    Tests =
        lists:foldl(
          fun(FileName, Acc) ->
                  NewAcc = read_rsp_file(filename:join(datadir(Config), FileName),
                                         Type, Acc),
                  ct:log("~p: ~p tests read.~n",[FileName,length(NewAcc)-length(Acc)]),
                  NewAcc
          end, [], Files),
    log_rsp_size(Type, Tests),
    Tests.

read_rsp_file(FileName, Type, Acc) ->
    case file:read_file(FileName) of
        {ok, Raw} ->
            Split = binary:split(Raw, [<<"\r">>, <<"\n">>], [global, trim_all]),
            parse_rsp(Type, Split, #{file => FileName}, Acc);
        Other ->
            ct:fail("~p ~p",[FileName, Other])
    end.

parse_rsp(_Type, [], _State, Acc) ->
    Acc;
parse_rsp(_Type, [<<"DECRYPT">>|_], _State, Acc) ->
    Acc;
parse_rsp(_Type, [<<"ENCRYPT">>|_], _State, Acc) ->
    Acc;
%% AES format
parse_rsp(Type, [<<"COUNT = ", _/binary>>,
                 <<"KEY = ", Key/binary>>,
                 <<"PLAINTEXT = ", PlainText/binary>>,
                 <<"CIPHERTEXT = ", CipherText/binary>>|Next], State, Acc) ->
    parse_rsp(Type, Next, State,
              [{Type, hexstr2bin(Key), 
                hexstr2bin(PlainText), hexstr2bin(CipherText)}|Acc]);
parse_rsp(Type, [<<"COUNT = ", _/binary>>,
                 <<"KEY = ", Key/binary>>,
                 <<"IV = ", IV/binary>>,
                 <<"PLAINTEXT = ", PlainText/binary>>,
                 <<"CIPHERTEXT = ", CipherText/binary>>|Next], State, Acc) ->
    parse_rsp(Type, Next, State,
              [{Type, hexstr2bin(Key), hexstr2bin(IV),
                hexstr2bin(PlainText), hexstr2bin(CipherText)}|Acc]);
%% CMAC format
parse_rsp(Type, [<<"Count = ", _/binary>>,
                 <<"Klen = ", _/binary>>,
                 <<"Mlen = ", Mlen/binary>>,
                 <<"Tlen = ", Tlen/binary>>,
                 <<"Key = ", Key/binary>>,
                 <<"Msg = ", Msg/binary>>,
                 <<"Mac = ", MAC/binary>>|Rest], State, Acc) ->
    case Rest of
        [<<"Result = P">>|Next] ->
            parse_rsp_cmac(Type, Key, Msg, Mlen, Tlen, MAC, Next, State, Acc);
        [<<"Result = ", _/binary>>|Next] ->
            parse_rsp(Type, Next, State, Acc);
        _ ->
            parse_rsp_cmac(Type, Key, Msg, Mlen, Tlen, MAC, Rest, State, Acc)
    end;
parse_rsp(Type, [<<"Count = ", _/binary>>,
                 <<"Klen = ", _/binary>>,
                 <<"Mlen = ", Mlen/binary>>,
                 <<"Tlen = ", Tlen/binary>>,
                 <<"Key1 = ", Key1/binary>>,
                 <<"Key2 = ", Key2/binary>>,
                 <<"Key3 = ", Key3/binary>>,
                 <<"Msg = ", Msg/binary>>,
                 <<"Mac = ", MAC/binary>>|Rest], State, Acc) ->
    case Rest of
        [<<"Result = P">>|Next] ->
            parse_rsp_cmac(Type, [Key1,Key2,Key3], Msg, Mlen, Tlen, MAC, Next, State, Acc);
        [<<"Result = ", _/binary>>|Next] ->
            parse_rsp(Type, Next, State, Acc);
        _ ->
            parse_rsp_cmac(Type, [Key1,Key2,Key3], Msg, Mlen, Tlen, MAC, Rest, State, Acc)
    end;
%% GCM format decode format
parse_rsp(Type, [<<"Count = ", Count/binary>>,
                 <<"Key = ", Key/binary>>,
                 <<"IV = ",  IV/binary>>,
                 <<"CT = ",  CipherText/binary>>,
                 <<"AAD = ", AAD/binary>>,
                 <<"Tag = ", CipherTag0/binary>>,
                 <<"PT = ",  PlainText/binary>>|Next], #{file:=File}=State, Acc) ->
    CipherTag = hexstr2bin(CipherTag0),
    TestCase = {Type,
                hexstr2bin(Key),
                hexstr2bin(PlainText),
                hexstr2bin(IV),
                hexstr2bin(AAD),
                hexstr2bin(CipherText),
                CipherTag,
                size(CipherTag),
                {File,decstr2int(Count)}},
    parse_rsp(Type, Next, State, [TestCase|Acc]);
%% GCM format encode format
parse_rsp(Type, [<<"Count = ", Count/binary>>,
                 <<"Key = ", Key/binary>>,
                 <<"IV = ",  IV/binary>>,
                 <<"PT = ",  PlainText/binary>>,
                 <<"AAD = ", AAD/binary>>,
                 <<"CT = ",  CipherText/binary>>,
                 <<"Tag = ", CipherTag0/binary>>|Next], #{file:=File}=State, Acc) ->
    CipherTag = hexstr2bin(CipherTag0),
    TestCase = {Type,
                hexstr2bin(Key),
                hexstr2bin(PlainText),
                hexstr2bin(IV),
                hexstr2bin(AAD),
                hexstr2bin(CipherText),
                CipherTag,
                size(CipherTag),
                {File,decstr2int(Count)}},
    parse_rsp(Type, Next, State, [TestCase|Acc]);
%% CCM-VADT format
parse_rsp(Type, [<<"[Alen = ", AlenB0/binary>>|Next], State0, Acc) ->
    AlenSize = size(AlenB0) - 1, % remove closing ']'
    Alen = decstr2int(<<AlenB0:AlenSize/binary>>),
    State = State0#{alen => Alen},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"[Nlen = ", NlenB0/binary>>|Next], State0, Acc) ->
    NlenSize = size(NlenB0) - 1, % remove closing ']'
    Nlen = decstr2int(<<NlenB0:NlenSize/binary>>),
    State = State0#{nlen => Nlen},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"[Plen = ", PlenB0/binary>>|Next], State0, Acc) ->
    PlenSize = size(PlenB0) - 1, % remove closing ']'
    Plen = decstr2int(<<PlenB0:PlenSize/binary>>),
    State = State0#{plen => Plen},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"[Tlen = ", TlenB0/binary>>|Next], State0, Acc) ->
    TlenSize = size(TlenB0) - 1, % remove closing ']'
    Tlen = decstr2int(<<TlenB0:TlenSize/binary>>),
    State = State0#{tlen => Tlen},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Alen = ", B/binary>>|Next], State0, Acc) ->
    State = State0#{alen => decstr2int(B)},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Plen = ", B/binary>>|Next], State0, Acc) ->
    State = State0#{plen => decstr2int(B)},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Count = ", B/binary>>|Next], State0, Acc) ->
    State = State0#{count => B},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Nlen = ", B/binary>>|Next], State0, Acc) ->
    State = State0#{nlen => decstr2int(B)},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Tlen = ", B/binary>>|Next], State0, Acc) ->
    State = State0#{tlen => decstr2int(B)},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Key = ",Key/binary>>|Next], State0, Acc) ->
    State = State0#{key => hexstr2bin(Key)},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Nonce = ",Nonce/binary>>|Next], State0, Acc) ->
    State = State0#{nonce => hexstr2bin(Nonce)},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Adata = ",Adata/binary>>|Next], State0, Acc) ->
    State = State0#{adata => hexstr2bin(Adata)},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type, [<<"Payload = ",Payload/binary>>|Next], State0, Acc) ->
    State = State0#{payload => hexstr2bin(Payload)},
    parse_rsp(Type, Next, State, Acc);
parse_rsp(Type,
          [<<"CT = ", CT/binary>>|Next],
          #{count := Count,
            file := File,
            alen := Alen,
            plen := Plen,
            nlen := _Nlen,
            tlen := Tlen,
            key := Key,
            nonce := IV,
            adata := Adata,
            payload := Payload
           } = State, Acc) ->
    AAD = <<Adata:Alen/binary>>,
    PlainText = <<Payload:Plen/binary>>,
    <<CipherText:Plen/binary, CipherTag:Tlen/binary>> = hexstr2bin(CT),
    TestCase = {Type,
                Key,
                PlainText,
                IV,
                AAD,
                CipherText,
                CipherTag,
                Tlen,
                {File,decstr2int(Count)}},
    parse_rsp(Type, Next, State, [TestCase|Acc]);
parse_rsp(Type, [_|Next], State, Acc) ->
    parse_rsp(Type, Next, State, Acc).


parse_rsp_cmac(Type, Key0, Msg0, Mlen0, Tlen, MAC0, Next, State, Acc) ->
    Key = if is_list(Key0) -> lists:map(fun hexstr2bin/1, Key0);
             true -> hexstr2bin(Key0)
          end,
    Mlen = binary_to_integer(Mlen0),
    <<Msg:Mlen/bytes, _/binary>> = hexstr2bin(Msg0),
    MAC = hexstr2bin(MAC0),
    case binary_to_integer(Tlen) of
        0 ->
            parse_rsp(Type, Next, State, [{cmac, Type, Key, Msg, MAC}|Acc]);
        I ->
            parse_rsp(Type, Next, State, [{cmac, Type, Key, Msg, I, MAC}|Acc])
    end.

api_errors_ecdh(Config) when is_list(Config) ->
    %% Check that we don't segfault when fed garbage.
    Test = fun(Others, Curve) ->
                   {_Pub, Priv} = crypto:generate_key(ecdh, Curve),
                   crypto:compute_key(ecdh, Others, Priv, Curve)
           end,
    Others = [gurka, 0, <<0>>],
    Curves = [gaffel, 0, sect571r1],
    [_= (catch Test(O, C)) || O <- Others, C <- Curves],
    ok.


%%%----- Tests for bad algorithm name as argument
-define(chk_api_name_helper(Call, ExpectPart),
        (fun() -> % avoid binding vars
                 try
                     Call
                 catch 
                     ExpectPart

                     Class:Reason:Stack ->
                         ct:log("~p:~p~n~p", [Class,Reason,Stack]),
                         ct:fail("Bad respons for bad name")
                 end
         end)()
       ).

-define(chk_api_name(Call, Expect),
        ?chk_api_name_helper(Call,  Expect -> ok;)
       ).

-define(chk_api_name(Call, Expect1, Expect2),
        ?chk_api_name_helper(Call,  Expect1 -> ok; Expect2 -> ok; )
       ).

-define(chk_api_name(Call, Expect1, Expect2, Expect3),
        ?chk_api_name_helper(Call,  Expect1 -> ok; Expect2 -> ok; Expect3 -> ok; )
       ).


bad_key_length(_Config) ->
    ?chk_api_name(crypto:crypto_init(aes_128_ctr, <<1>>, <<0:128>>, true),
                  error:{badarg,_,"Bad key size"++_}).

bad_cipher_name(_Config) ->
    ?chk_api_name(crypto:crypto_init(foobar, <<1:128>>, true),
                  error:{badarg,{"api_ng.c",_Line},"Unknown cipher"++_}).

bad_generate_key_name(_Config) ->
    ?chk_api_name(crypto:generate_key(foobar, [1024]),
                  error:function_clause).

bad_hash_name(_Config) ->
    ?chk_api_name(crypto:hash_init(foobar),
                  error:{badarg,{"hash.c",_},"Bad digest type"++_}).

bad_mac_name(_Config) ->
    ?chk_api_name(crypto:mac(foobar, <<1:1024>>, "nothing"),
                  error:function_clause).

bad_hmac_name(_Config) ->
    ?chk_api_name(crypto:mac(hmac, foobar, <<1:1024>>, "nothing"),
                  error:{badarg,{"mac.c",_},"Bad digest algorithm"++_}).

bad_cmac_name(_Config) ->
    ?chk_api_name(crypto:mac(cmac, foobar, <<1:1024>>, "nothing"),
                  error:{badarg,{"mac.c",_},"Unknown cipher"++_},
                  error:{notsup,_,          "Unsupported mac algorithm"++_},
                  error:{badarg,#{},        "Unknown cipher"++_}
                 ).

bad_sign_name(_Config) ->
    ?chk_api_name(crypto:sign(rsa, foobar, "nothing", <<1:1024>>),
                  error:{badarg, {"pkey.c",_}, "Bad digest type"++_}),
    ?chk_api_name(crypto:sign(foobar, sha, "nothing", <<1:1024>>),
                  error:{badarg, {"pkey.c",_}, "Bad algorithm"++_}).
    
bad_verify_name(_Config) ->
    ?chk_api_name(crypto:verify(rsa, foobar, "nothing", <<"nothing">>,  <<1:1024>>),
                  error:{badarg,{"pkey.c",_},"Bad digest type"++_}),
    ?chk_api_name(crypto:verify(foobar, sha, "nothing", <<"nothing">>, <<1:1024>>),
                  error:{badarg, {"pkey.c",_}, "Bad algorithm"++_}).


%%%----------------------------------------------------------------
try_enable_fips_mode(Config) ->
    FIPSConfig = [{fips, true} | Config],
    case crypto:info_fips() of
        enabled ->
            FIPSConfig;
        not_enabled ->
            %% Erlang/crypto configured with --enable-fips
            case crypto:enable_fips_mode(true) of
		true ->
                    %% and also the cryptolib is fips enabled
		    enabled = crypto:info_fips(),
		    FIPSConfig;
		false ->
                    try
                        [{_,_,Inf}] = crypto:info_lib(),
                        re:run(Inf, "(F|f)(I|i)(P|p)(S|s)")
                    of
                        nomatch ->
                            {skip, "FIPS mode not supported in cryptolib"};
                        {match,_} ->
                            {fail, "Failed to enable FIPS mode"}
                    catch
                        _:_ ->
                            {fail, "Failed to check cryptolib info"}
                    end,
		    {skip, "FIPS mode not supported in cryptolib"}
	    end;
        not_supported ->
            {skip, "FIPS mode not supported"}
    end.

pbkdf2_hmac() ->
  [{doc, "Test the pbkdf2_hmac function"}].
pbkdf2_hmac(Config) when is_list(Config) ->
  try
    F = fun (A, B, C, D) ->
            binary:encode_hex(crypto:pbkdf2_hmac(sha, A, B, C, D))
        end,
    %% RFC 6070
    <<"0C60C80F961F0E71F3A9B524AF6012062FE037A6">> =
      F(<<"password">>, <<"salt">>, 1, 20),
    <<"EA6C014DC72D6F8CCD1ED92ACE1D41F0D8DE8957">> =
      F(<<"password">>, <<"salt">>, 2, 20),
    <<"4B007901B765489ABEAD49D926F721D065A429C1">> =
      F(<<"password">>, <<"salt">>, 4096, 20),
    <<"EEFE3D61CD4DA4E4E9945B3D6BA2158C2634E984">> =
      F(<<"password">>, <<"salt">>, 16777216, 20),
    <<"3D2EEC4FE41C849B80C8D83662C0E44A8B291A964CF2F07038">> =
      F(<<"passwordPASSWORDpassword">>, <<"saltSALTsaltSALTsaltSALTsaltSALTsalt">>, 4096, 25),
    <<"56FA6AA75548099DCC37D7F03425E0C3">> =
      F(<<"pass\0word">>, <<"sa\0lt">>, 4096, 16),

    %% RFC 3962
    <<"CDEDB5281BB2F801565A1122B2563515">> =
      F(<<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 1, 16),
    <<"CDEDB5281BB2F801565A1122B25635150AD1F7A04BB9F3A333ECC0E2E1F70837">> =
      F(<<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 1, 32),
    <<"01DBEE7F4A9E243E988B62C73CDA935D">> =
      F(<<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 2, 16),
    <<"01DBEE7F4A9E243E988B62C73CDA935DA05378B93244EC8F48A99E61AD799D86">> =
      F(<<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 2, 32),
    <<"5C08EB61FDF71E4E4EC3CF6BA1F5512B">> =
      F(<<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 1200, 16),
    <<"5C08EB61FDF71E4E4EC3CF6BA1F5512BA7E52DDBC5E5142F708A31E2E62B1E13">> =
      F(<<"password">>, <<"ATHENA.MIT.EDUraeburn">>, 1200, 32),
    <<"D1DAA78615F287E6A1C8B120D7062A49">> =
      F(<<"password">>, binary:encode_unsigned(16#1234567878563412), 5, 16),
    <<"D1DAA78615F287E6A1C8B120D7062A493F98D203E6BE49A6ADF4FA574B6E64EE">> =
      F(<<"password">>, binary:encode_unsigned(16#1234567878563412), 5, 32),
    <<"139C30C0966BC32BA55FDBF212530AC9">> =
      F(<<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">>,
        <<"pass phrase equals block size">>, 1200, 16),
    <<"139C30C0966BC32BA55FDBF212530AC9C5EC59F1A452F5CC9AD940FEA0598ED1">> =
      F(<<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">>,
        <<"pass phrase equals block size">>, 1200, 32),
    <<"9CCAD6D468770CD51B10E6A68721BE61">> =
      F(<<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">>,
        <<"pass phrase exceeds block size">>, 1200, 16),
    <<"9CCAD6D468770CD51B10E6A68721BE611A8B4D282601DB3B36BE9246915EC82A">> =
      F(<<"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX">>,
        <<"pass phrase exceeds block size">>, 1200, 32),
    <<"6B9CF26D45455A43A5B8BB276A403B39">> =
      F(binary:encode_unsigned(16#f09d849e), <<"EXAMPLE.COMpianist">>, 50, 16),
    <<"6B9CF26D45455A43A5B8BB276A403B39E7FE37A0C41E02C281FF3069E1E94F52">> =
      F(binary:encode_unsigned(16#f09d849e), <<"EXAMPLE.COMpianist">>, 50, 32)
  catch
    error:{notsup, _, "Unsupported CRYPTO_PKCS5_PBKDF2_HMAC"++_} ->
            {skip, "No CRYPTO_PKCS5_PBKDF2_HMAC"}
  end.


get_priv_pub_from_sign_verify(L) ->
    lists:foldl(fun get_priv_pub/2, [], L).

get_priv_pub({Type, undefined=_Hash, Private, Public, _Msg, _Signature}, Acc) -> [{Type,Private,Public} | Acc];
get_priv_pub({Type, _Hash, Public, Private, _Msg}, Acc) -> [{Type,Private,Public} | Acc];
get_priv_pub({Type, _Hash, Public, Private, _Msg, _Options}, Acc) -> [{Type,Private,Public} | Acc];
get_priv_pub(_, Acc) -> Acc.
