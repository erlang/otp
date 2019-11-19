%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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

%% Note: This directive should only be used in test suites.
-compile(export_all).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app,
     {group, api_errors},
     appup,
     {group, fips},
     {group, non_fips},
     mod_pow,
     exor,
     rand_uniform,
     rand_threads,
     rand_plugin,
     rand_plugin_s,
     cipher_info,
     hash_info
    ].

-define(NEW_CIPHER_TYPE_SCHEMA,
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
        {group, aes_256_cfb8}
       ).
        
-define(RETIRED_TYPE_ALIASES,
        {group, aes_cbc},
        {group, aes_cbc128},
        {group, aes_cbc256},
        {group, aes_ccm},
        {group, aes_ctr},
        {group, aes_gcm},
        {group, aes_ecb},
        {group, des3_cfb},
        {group, des3_cbc},
        {group, des3_cbf},
        {group, des_ede3},
        {group, aes_cfb128},
        {group, aes_cfb8}
       ).

groups() ->
    [{non_fips, [], [
                     {group, blake2b},
                     {group, blake2s},
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
                     {group, sha},
                     {group, poly1305},

                     {group, dh},
                     {group, ecdh},
                     {group, srp},

		     {group, chacha20_poly1305},
		     {group, chacha20},
                     {group, blowfish_cbc},
                     {group, blowfish_cfb64},
                     {group, blowfish_ecb},
                     {group, blowfish_ofb64},

                     {group, aes_ige256},
                     {group, des_cbc},
                     {group, des_cfb},
                     {group, rc2_cbc},
                     {group, rc4},

                     ?NEW_CIPHER_TYPE_SCHEMA,
                     ?RETIRED_TYPE_ALIASES
                    ]},
     {fips, [], [
                 {group, no_blake2b},
                 {group, no_blake2s},
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
                 {group, no_poly1305},

                 {group, dh},
                 {group, ecdh},
                 {group, no_srp},

		 {group, no_chacha20_poly1305},
		 {group, no_chacha20},
                 {group, no_blowfish_cbc},
                 {group, no_blowfish_cfb64},
                 {group, no_blowfish_ecb},
                 {group, no_blowfish_ofb64},

                 {group, no_aes_ige256},
                 {group, no_des_cbc},
                 {group, no_des_cfb},
                 {group, no_rc2_cbc},
                 {group, no_rc4},

                 ?NEW_CIPHER_TYPE_SCHEMA,
                 ?RETIRED_TYPE_ALIASES
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
     {blake2b,              [], [hash, hmac, hmac_update]},
     {blake2s,              [], [hash, hmac, hmac_update]},
     {no_blake2b,           [], [no_hash, no_hmac]},
     {no_blake2s,           [], [no_hash, no_hmac]},
     {rsa,                  [], [sign_verify,
                                 public_encrypt,
                                 private_encrypt,
                                 generate
                                ]},
     {dss,                  [], [sign_verify
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                ]},
     {ecdsa,                [], [sign_verify
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                ]},
     {ed25519,              [], [sign_verify
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                              ]},
     {ed448,                [], [sign_verify
                                 %% Does not work yet:  ,public_encrypt, private_encrypt
                                ]},
     {dh,                   [], [generate_compute, compute_bug]},
     {ecdh,                 [], [use_all_elliptic_curves, compute, generate]},
     {srp,                  [], [generate_compute]},
     {des_cbc,              [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {des_cfb,              [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {des_ede3_cbc,         [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {des_ede3_cfb,         [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {rc2_cbc,              [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_cfb8,             [], [block]},
     {aes_128_cfb8,         [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_192_cfb8,         [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_256_cfb8,         [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {no_aes_cfb8,          [], [no_support, no_block]},
     {aes_cfb128,           [], [block]},
     {aes_128_cfb128,       [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_192_cfb128,       [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_256_cfb128,       [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {no_aes_cfb128,        [], [no_support, no_block]},
     {aes_ige256,           [], [block]},
     {no_aes_ige256,        [], [no_support, no_block]},
     {blowfish_cbc,         [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {blowfish_ecb,         [], [block, api_ng, api_ng_one_shot]},
     {blowfish_cfb64,       [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {blowfish_ofb64,       [], [block, api_ng, api_ng_one_shot, api_ng_tls]},
     {rc4,                  [], [stream, api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_ctr,              [], [stream]},
     {chacha20_poly1305,    [], [aead, aead_ng, aead_bad_tag]},
     {chacha20,             [], [stream, api_ng, api_ng_one_shot, api_ng_tls]},
     {poly1305,             [], [poly1305]},
     {no_poly1305,          [], [no_poly1305]},
     {no_aes_cfb128,        [], [no_support, no_block]},
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
     {no_des_cbc,           [], [no_support, no_block]},
     {no_des_cfb,           [], [no_support, no_block]},
     {no_blowfish_cbc,      [], [no_support, no_block]},
     {no_blowfish_ecb,      [], [no_support, no_block]},
     {no_blowfish_cfb64,    [], [no_support, no_block]},
     {no_blowfish_ofb64,    [], [no_support, no_block]},
     {no_aes_ige256,        [], [no_support, no_block]},
     {no_chacha20_poly1305, [], [no_support, no_aead, no_aead_ng]},
     {no_chacha20,          [], [no_support, no_stream_ivec]},
     {no_rc2_cbc,           [], [no_support, no_block]},
     {no_rc4,               [], [no_support, no_stream]},
     {api_errors,           [], [api_errors_ecdh,
                                 bad_cipher_name,
                                 bad_generate_key_name,
                                 bad_hash_name,
                                 bad_hmac_name,
                                 bad_mac_name,
                                 bad_sign_name,
                                 bad_verify_name
                                ]},

     %% New cipher nameing schema
     {des_ede3_cbc, [], [api_ng, api_ng_one_shot, api_ng_tls]},
     {des_ede3_cfb, [], [api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_128_cbc,  [], [api_ng, api_ng_one_shot, api_ng_tls, cmac]},
     {aes_192_cbc,  [], [api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_256_cbc,  [], [api_ng, api_ng_one_shot, api_ng_tls, cmac]},
     {aes_128_ctr,  [], [api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_192_ctr,  [], [api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_256_ctr,  [], [api_ng, api_ng_one_shot, api_ng_tls]},
     {aes_128_ccm,  [], [aead, aead_ng, aead_bad_tag]},
     {aes_192_ccm,  [], [aead, aead_ng, aead_bad_tag]},
     {aes_256_ccm,  [], [aead, aead_ng, aead_bad_tag]},
     {aes_128_ecb,  [], [api_ng, api_ng_one_shot]},
     {aes_192_ecb,  [], [api_ng, api_ng_one_shot]},
     {aes_256_ecb,  [], [api_ng, api_ng_one_shot]},
     {aes_128_gcm,  [], [aead, aead_ng, aead_bad_tag]},
     {aes_192_gcm,  [], [aead, aead_ng, aead_bad_tag]},
     {aes_256_gcm,  [], [aead, aead_ng, aead_bad_tag]},

     %% Retired aliases
     {aes_cbc,    [], [block]},
     {aes_cbc128, [], [block]},
     {aes_cbc256, [], [block]},
     {aes_ccm,    [], [aead]},
     {aes_ecb,    [], [block]},
     {aes_gcm,    [], [aead]},
     {des3_cbc,             [], [block]},
     {des_ede3,             [], [block]},
     {des3_cbf,             [], [block]},
     {des3_cfb,             [], [block]}
    ].

%%-------------------------------------------------------------------
init_per_suite(Config) ->
    file:set_cwd(datadir(Config)),
    {ok, _} = zip:unzip("KAT_AES.zip"),
    {ok, _} = zip:unzip("aesmmt.zip"),
    {ok, _} = zip:unzip("cmactestvectors.zip"),
    {ok, _} = zip:unzip("gcmtestvectors.zip"),

    try crypto:start() of
	ok ->
            catch ct:comment("~s",[element(3,hd(crypto:info_lib()))]),
            catch ct:log("crypto:info_lib() -> ~p~n"
                         "crypto:supports() -> ~p~n"
                         "crypto:version()  -> ~p~n"
                        ,[crypto:info_lib(), crypto:supports(), crypto:version()]),

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
    catch _:_ ->
	    {fail, "Crypto did not start"}
    end.

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
    Config.

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
    ok = ?t:app_test(crypto).
%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the crypto appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(crypto).
%%--------------------------------------------------------------------
no_support() ->
    [{doc, "Test an algorithm is not reported in the supported list"}].
no_support(Config) when is_list(Config) ->
    Type  = ?config(type, Config),
    false = is_supported(Type).
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
    lists:foreach(fun hmac_check/1, Tuples),
    lists:foreach(fun hmac_check/1, mac_listify(Tuples)).

%%--------------------------------------------------------------------
no_hmac() ->
     [{doc, "Test all disabled hmac functions"}].
no_hmac(Config) when is_list(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:hmac/3, [Type, <<"Key">>, <<"Hi There">>]).

%%--------------------------------------------------------------------
hmac_update() ->
     [{doc, "Test all incremental hmac functions"}].
hmac_update(Config) ->
    Type = ?config(type, Config),
    hmac_increment(Type).

%%--------------------------------------------------------------------
no_hmac_update() ->
     [{doc, "Test all disabled incremental hmac functions"}].
no_hmac_update(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:hmac_init/2, [Type, <<"Key">>]).

%%--------------------------------------------------------------------
cmac() ->
     [{doc, "Test all different cmac functions"}].
cmac(Config) when is_list(Config) ->
    Pairs = lazy_eval(proplists:get_value(cmac, Config)),
    lists:foreach(fun cmac_check/1, Pairs),
    lists:foreach(fun cmac_check/1, mac_listify(Pairs)).

%%--------------------------------------------------------------------
poly1305() ->
    [{doc, "Test poly1305 function"}].
poly1305(Config) ->
    lists:foreach(
      fun({Key, Txt, Expect}) ->
              case crypto:poly1305(Key,Txt) of
                  Expect ->
                      ok;
                  Other ->
                      ct:fail({{crypto, poly1305, [Key, Txt]}, {expected, Expect}, {got, Other}})
              end
      end, proplists:get_value(poly1305, Config)).

%%--------------------------------------------------------------------
no_poly1305() ->
    [{doc, "Test disabled poly1305 function"}].
no_poly1305(_Config) ->
    Key = <<133,214,190,120,87,85,109,51,127,68,82,254,66,213,6,168,1,
            3,128,138,251,13,178,253,74,191,246,175,65,73,245,27>>,
    Txt = <<"Cryptographic Forum Research Group">>,
    notsup(fun crypto:poly1305/2, [Key,Txt]).

%%--------------------------------------------------------------------
block() ->
     [{doc, "Test block ciphers"}].
block(Config) when is_list(Config) ->
    [_|_] = Blocks = lazy_eval(proplists:get_value(cipher, Config)),
    lists:foreach(fun block_cipher/1, Blocks),
    lists:foreach(fun block_cipher/1, block_iolistify(Blocks)),
    lists:foreach(fun block_cipher_increment/1, block_iolistify(Blocks)).

%%--------------------------------------------------------------------
no_block() ->
     [{doc, "Test disabled block ciphers"}].
no_block(Config) when is_list(Config) ->
    [_|_] = Blocks = lazy_eval(proplists:get_value(cipher, Config)),
    Args = case Blocks of
	       [{_Type, _Key, _PlainText} = A | _] ->
		   tuple_to_list(A);
	       [{_Type, _Key, _IV, _PlainText} = A | _] ->
		   tuple_to_list(A);
	       [{Type, Key, IV, PlainText, _CipherText} | _] ->
		   [Type, Key, IV, PlainText]
	   end,
    N = length(Args),
    notsup(fun crypto:block_encrypt/N, Args),
    notsup(fun crypto:block_decrypt/N, Args).
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
    Enc = iolist_to_binary(EncTexts),
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
    case iolist_to_binary(api_ng_cipher_increment_loop(RefDec, EncTexts)) of
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
                              ct:pal("Txt = ~p",[Txt]),
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
api_ng_tls() ->
     [{doc, "Test special tls api"}].

api_ng_tls(Config) when is_list(Config) ->
    [_|_] = Ciphers = lazy_eval(proplists:get_value(cipher, Config, [])),
    lists:foreach(fun do_api_ng_tls/1, Ciphers).


do_api_ng_tls({Type, Key, PlainTexts}=_X) ->
    ct:log("~p",[_X]),
    do_api_ng_tls({Type, Key, <<>>, PlainTexts});

do_api_ng_tls({Type, Key, IV, PlainTexts}=_X) ->
    ct:log("~p",[_X]),
    do_api_ng_tls({Type, Key, IV, PlainTexts, undefined});

do_api_ng_tls({Type, Key, IV, PlainText0, ExpectedEncText}=_X) ->
    ct:log("~p",[_X]),
    PlainText = iolist_to_binary(lazy_eval(PlainText0)),
    Renc = crypto:crypto_dyn_iv_init(Type, Key, true),
    Rdec = crypto:crypto_dyn_iv_init(Type, Key, false),
    EncTxt = crypto:crypto_dyn_iv_update(Renc, PlainText, IV),
    case ExpectedEncText of
        undefined ->
            ok;
        EncTxt ->
            %% Now check that the state is NOT updated:
            case crypto:crypto_dyn_iv_update(Renc, PlainText, IV) of
                EncTxt ->
                    ok;
                EncTxt2 ->
                    ct:log("2nd encode~nIn: ~p~nExpected: ~p~nEnc: ~p~n", [{Type,Key,IV,PlainText}, EncTxt, EncTxt2]),
                    ct:fail("api_ng_tls (second encode)",[])
            end;
        OtherEnc ->
            ct:log("1st encode~nIn: ~p~nExpected: ~p~nEnc: ~p~n", [{Type,Key,IV,PlainText}, ExpectedEncText, OtherEnc]),
            ct:fail("api_ng_tls (encode)",[])
    end,
    case crypto:crypto_dyn_iv_update(Rdec, EncTxt, IV) of
        PlainText ->
            %% Now check that the state is NOT updated:
            case crypto:crypto_dyn_iv_update(Rdec, EncTxt, IV) of
                PlainText ->
                    ok;
                PlainText2 ->
                    ct:log("2nd decode~nIn: ~p~nExpected: ~p~nDec: ~p~n", [{Type,Key,IV,EncTxt}, PlainText, PlainText2]),
                    ct:fail("api_ng_tls (second decode)",[])
            end;
        OtherPT ->
            ct:log("1st decode~nIn: ~p~nExpected: ~p~nDec: ~p~n", [{Type,Key,IV,EncTxt}, PlainText, OtherPT]),
            ct:fail("api_ng_tlst (decode)",[])
    end.

%%--------------------------------------------------------------------
no_aead() ->
     [{doc, "Test disabled aead ciphers"}].
no_aead(Config) when is_list(Config) ->
    EncArg4 =
        case lazy_eval(proplists:get_value(cipher, Config)) of
            [{Type, Key, PlainText, Nonce, AAD, CipherText, CipherTag, TagLen, _Info} | _] ->
                {AAD, PlainText, TagLen};
            [{Type, Key, PlainText, Nonce, AAD, CipherText, CipherTag, _Info} | _] ->
                {AAD, PlainText}
        end,
    EncryptArgs = [Type, Key, Nonce, EncArg4],
    DecryptArgs = [Type, Key, Nonce, {AAD, CipherText, CipherTag}],
    notsup(fun crypto:block_encrypt/4, EncryptArgs),
    notsup(fun crypto:block_decrypt/4, DecryptArgs).

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
stream() ->
      [{doc, "Test stream ciphers"}].
stream(Config) when is_list(Config) ->
    [_|_] = Streams = lazy_eval(proplists:get_value(cipher, Config)),

    lists:foreach(fun stream_cipher/1, Streams),
    lists:foreach(fun stream_cipher/1, stream_iolistify(Streams)),
    lists:foreach(fun stream_cipher_incment/1, stream_iolistify(Streams)).
%%--------------------------------------------------------------------
no_stream() ->
      [{doc, "Test disabled stream ciphers"}].
no_stream(Config) when is_list(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:stream_init/2, [Type, <<"Key">>]).

%%--------------------------------------------------------------------
no_stream_ivec() ->
      [{doc, "Test disabled stream ciphers that uses ivec"}].
no_stream_ivec(Config) when is_list(Config) ->
    Type = ?config(type, Config),
    notsup(fun crypto:stream_init/3, [Type, <<"Key">>, <<"Ivec">>]).

%%--------------------------------------------------------------------
aead() ->
      [{doc, "Test AEAD ciphers"}].
aead(Config) when is_list(Config) ->
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
    lists:foreach(fun aead_cipher/1, FilteredAEADs).

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
    lists:foreach(fun aead_cipher_ng/1, FilteredAEADs ++ spec_0_bytes(Config)).

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
    lists:foreach(fun aead_cipher_bad_tag/1, FilteredAEADs).

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
    Params = proplists:get_value(pub_pub_encrypt, Config, []),
    lists:foreach(fun do_public_encrypt/1, Params).

%%-------------------------------------------------------------------- 
private_encrypt() ->
     [{doc, "Test private_encrypt/decrypt functions. "}].
private_encrypt(Config) when is_list(Config) ->
    Params = proplists:get_value(pub_priv_encrypt, Config, []),
    lists:foreach(fun do_private_encrypt/1, Params).

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
    Gen = proplists:get_value(compute, Config),
    lists:foreach(fun do_compute/1, Gen).
%%--------------------------------------------------------------------
use_all_elliptic_curves() ->
    [{doc, " Test that all curves from crypto:ec_curves/0"}].
use_all_elliptic_curves(_Config) ->
    Msg = <<"hello world!">>,
    Sups = crypto:supports(),
    Curves = proplists:get_value(curves, Sups),
    Hashs = proplists:get_value(hashs, Sups),
    ct:log("Lib: ~p~nFIPS: ~p~nCurves:~n~p~nHashs: ~p", [crypto:info_lib(),
                                                         crypto:info_fips(),
                                                         Curves,
                                                         Hashs]),
    Results =
        [{{Curve,Hash},
          try
              {Pub,Priv} = crypto:generate_key(ecdh, Curve),
              true = is_binary(Pub),
              true = is_binary(Priv),
              Sig = crypto:sign(ecdsa, Hash, Msg, [Priv, Curve]),
              crypto:verify(ecdsa, Hash, Msg, Sig, [Pub, Curve])
          catch
              C:E ->
                  {C,E}
          end}
         || Curve <- Curves -- [ed25519, ed448, x25519, x448, ipsec3, ipsec4],
            Hash <- Hashs -- [md4, md5, ripemd160, sha3_224, sha3_256, sha3_384, sha3_512, blake2b, blake2s]
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
                                     ct:pal("~p:~p ~p",[Cls,Exc,C]),
                                     false
                             end
                     end,
                     true,
crypto:supports(ciphers)) of
%%                     proplists:get_value(ciphers, crypto:supports())) of
        true ->
            ok;
        false ->
            ct:fail('Cipher unsupported',[])
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
hmac_check({hmac, sha=Type, Key, <<"Test With Truncation">>=Data, Expected}) ->
    do_hmac_check(Type, Key, Data, 20, Expected);
hmac_check({hmac, Type, Key, <<"Test With Truncation">>=Data, Expected}) ->
    do_hmac_check(Type, Key, Data, 16, Expected);
hmac_check({hmac, Type, Key, Data, Expected}) ->
    do_hmac_check(Type, Key, Data, Expected).


do_hmac_check(Type, Key, Data, Expected) ->
    try crypto:hmac(Type, Key, Data)
    of
	Expected ->
	    ok;
	Other ->
	    ct:fail({{crypto,hmac,[Type,Key,Data]}, {expected,Expected}, {got,Other}})
    catch
        error:notsup ->
            ct:fail("HMAC ~p not supported", [Type]);
        Class:Cause ->
            ct:fail({{crypto,hmac,[Type,Key,Data]}, {expected,Expected}, {got,{Class,Cause}}})
    end.

do_hmac_check(Type, Key, Data, MacLength, Expected) ->
    try crypto:hmac(Type, Key, Data, MacLength)
    of
	Expected ->
	    ok;
	Other ->
	    ct:fail({{crypto,hmac,[Type,Key,Data,MacLength]}, {expected,Expected}, {got,Other}})
    catch
        error:notsup ->
            ct:fail("HMAC ~p not supported", [Type]);
        Class:Cause ->
            ct:fail({{crypto,hmac,[Type,Key,Data,MacLength]}, {expected,Expected}, {got,{Class,Cause}}})
    end.


%%%----------------------------------------------------------------
hmac_increment(Type) ->
    Key = hmac_key(Type),
    Increments = hmac_inc(Type),
    Expected = crypto:hmac(Type, Key, lists:flatten(Increments)),
    State = crypto:hmac_init(Type, Key),
    case hmac_increment(State, Increments) of
	Expected ->
	    ok;
	Other ->
	    ct:fail({{crypto, "hmac_init/update/final", [Type, Increments]}, {expected, Expected}, {got, Other}})  
    end.

hmac_increment(State, []) ->
    crypto:hmac_final(State);
hmac_increment(State0, [Increment | Rest]) ->
    State = crypto:hmac_update(State0, Increment),
    hmac_increment(State, Rest).

%%%----------------------------------------------------------------
cmac_check({cmac, Type, Key, Text, CMac}) ->
    ExpCMac = iolist_to_binary(CMac),
    case crypto:cmac(Type, Key, Text) of
        ExpCMac ->
            ok;
        Other ->
            ct:fail({{crypto, cmac, [Type, Key, Text]}, {expected, ExpCMac}, {got, Other}})
    end;
cmac_check({cmac, Type, Key, Text, Size, CMac}) ->
    ExpCMac = iolist_to_binary(CMac),
    case crypto:cmac(Type, Key, Text, Size) of
        ExpCMac ->
            ok;
        Other ->
            ct:fail({{crypto, cmac, [Type, Key, Text, Size]}, {expected, ExpCMac}, {got, Other}})
    end.


mac_check({MacType, SubType, Key, Text, Mac}) ->
    ExpMac = iolist_to_binary(Mac),
    case crypto:mac(MacType, SubType, Key, Text) of
        ExpMac ->
            ok;
        Other ->
            ct:fail({{crypto, mac, [MacType, SubType, Key, Text]}, {expected, ExpMac}, {got, Other}})
    end;
mac_check({MacType, SubType, Key, Text, Size, Mac}) ->
    ExpMac = iolist_to_binary(Mac),
    case crypto:mac(MacType, SubType, Key, Text, Size) of
        ExpMac ->
            ok;
        Other ->
            ct:fail({{crypto, mac, [MacType, SubType, Key, Text]}, {expected, ExpMac}, {got, Other}})
    end.


block_cipher({Type, Key,  PlainText}) ->
    Plain = iolist_to_binary(PlainText),
    CipherText = crypto:block_encrypt(Type, Key, PlainText),
    case crypto:block_decrypt(Type, Key, CipherText) of
	Plain ->
	    ok;
	Other ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, CipherText]}, {expected, Plain}, {got, Other}})
    end;

block_cipher({Type, Key,  IV, PlainText}) ->
    Plain = iolist_to_binary(PlainText),
    CipherText = crypto:block_encrypt(Type, Key, IV, PlainText),
    case crypto:block_decrypt(Type, Key, IV, CipherText) of
	Plain ->
	    ok;
	Other ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, IV, CipherText]}, {expected, Plain}, {got, Other}})
    end;

block_cipher({Type, Key, IV, PlainText, CipherText}) ->
    Plain = iolist_to_binary(PlainText),
    case crypto:block_encrypt(Type, Key, IV, Plain) of
	CipherText ->
	    ok;
	Other0 ->
	    ct:fail({{crypto, block_encrypt, [Type, Key, IV, Plain]}, {expected, CipherText}, {got, Other0}})
    end,
    case crypto:block_decrypt(Type, Key, IV, CipherText) of
	Plain ->
	    ok;
	Other1 ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, IV, CipherText]}, {expected, Plain}, {got, Other1}})
    end.

block_cipher_increment({Type, Key, IV, PlainTexts}) when Type == des_cbc ;
                                                         Type == des3_cbc ;
                                                         Type == aes_128_cbc ;
                                                         Type == aes_192_cbc ;
                                                         Type == aes_256_cbc
                                                         ->
    block_cipher_increment(Type, Key, IV, IV, PlainTexts, iolist_to_binary(PlainTexts), []);
block_cipher_increment({Type, Key, IV, PlainTexts, CipherText}) when Type == des_cbc; 
                                                                     Type == des_ede3_cbc ;
                                                                     Type == des3_cbc ;
                                                                     Type == des_ede3 ;
                                                                     Type == des_ede3_cfb ;
                                                                     Type == des_ede3_cbf ;
                                                                     Type == des3_cbf ;
                                                                     Type == des3_cfb
                                                                     ->
    block_cipher_increment(Type, Key, IV, IV, PlainTexts, iolist_to_binary(PlainTexts), CipherText, []);
block_cipher_increment({Type, Key, IV, PlainTexts, _CipherText}) when Type == aes_128_cbc ;
                                                                      Type == aes_192_cbc ;
                                                                      Type == aes_256_cbc 
                                                                      ->
    Plain = iolist_to_binary(PlainTexts),
    Blocks = [iolistify(Block) || << Block:128/bitstring >> <= Plain],
    block_cipher_increment(Type, Key, IV, IV, Blocks, Plain, []);
block_cipher_increment({_Type, _, _, _, _}) ->
    ok;
block_cipher_increment({_Type, _, _, _}) ->
    ok;
block_cipher_increment({_,_,_}) ->
    ok.
block_cipher_increment(Type, Key, IV0, _IV, [], Plain, Acc) ->
    CipherText = iolist_to_binary(lists:reverse(Acc)),
    case crypto:block_decrypt(Type, Key, IV0, CipherText) of
	Plain ->
	    ok;
	Other ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, IV0, CipherText]}, {expected, Plain}, {got, Other}})
    end;
block_cipher_increment(Type, Key, IV0, IV, [PlainText | PlainTexts], Plain, Acc) ->
    CipherText = crypto:block_encrypt(Type, Key, IV, PlainText),
    NextIV = crypto:next_iv(Type, CipherText),
    block_cipher_increment(Type, Key, IV0, NextIV, PlainTexts, Plain, [CipherText | Acc]).
block_cipher_increment(Type, Key, IV0, _IV, [], _Plain, CipherText, Acc) ->
    case iolist_to_binary(lists:reverse(Acc)) of
	CipherText ->
	    ok;
	Other ->
	    ct:fail({{crypto, block_decrypt, [Type, Key, IV0, CipherText]}, {expected, CipherText}, {got, Other}})
    end;
block_cipher_increment(Type, Key, IV0, IV, [PlainText | PlainTexts], Plain, CipherText, Acc) ->
    CT = crypto:block_encrypt(Type, Key, IV, PlainText),
    NextIV = crypto:next_iv(Type, CT),
    block_cipher_increment(Type, Key, IV0, NextIV, PlainTexts, Plain, CipherText, [CT | Acc]).

stream_cipher({Type, Key, PlainText0}) ->
    PlainText = lazy_eval(PlainText0),
    Plain = iolist_to_binary(lazy_eval(PlainText)),
    StateE = crypto:stream_init(Type, Key),
    StateD = crypto:stream_init(Type, Key),
    {_, CipherText} = crypto:stream_encrypt(StateE, PlainText),
    case crypto:stream_decrypt(StateD, CipherText) of
	{_, Plain} ->
	    ok;
	Other ->
	    ct:fail({{crypto, stream_decrypt, [StateD, CipherText]}, {expected, PlainText}, {got, Other}})
    end;
stream_cipher({Type, Key, IV, PlainText0}) ->
    PlainText = lazy_eval(PlainText0),
    Plain = iolist_to_binary(PlainText),
    StateE = crypto:stream_init(Type, Key, IV),
    StateD = crypto:stream_init(Type, Key, IV),
    {_, CipherText} = crypto:stream_encrypt(StateE, PlainText),
    case crypto:stream_decrypt(StateD, CipherText) of
	{_, Plain} ->
	    ok;
	Other ->
	    ct:fail({{crypto, stream_decrypt, [StateD, CipherText]}, {expected, PlainText}, {got, Other}})
    end;
stream_cipher({Type, Key, IV, PlainText0, CipherText}) ->
    PlainText = lazy_eval(PlainText0),
    Plain = iolist_to_binary(PlainText),
    StateE = crypto:stream_init(Type, Key, IV),
    StateD = crypto:stream_init(Type, Key, IV),
    case crypto:stream_encrypt(StateE, PlainText) of
        {_, CipherText} ->
            ok;
        {_, Other0} ->
            ct:fail({{crypto, stream_encrypt, [StateE, Type, Key, IV, Plain]}, {expected, CipherText}, {got, Other0}})
    end,
    case crypto:stream_decrypt(StateD, CipherText) of
        {_, Plain} ->
            ok;
        Other1 ->
            ct:fail({{crypto, stream_decrypt, [StateD, CipherText]}, {expected, PlainText}, {got, Other1}})
    end.

stream_cipher_incment({Type, Key, PlainTexts}) ->
    StateE = crypto:stream_init(Type, Key),
    StateD = crypto:stream_init(Type, Key),
    stream_cipher_incment_loop(StateE, StateD, PlainTexts, [], iolist_to_binary(PlainTexts));
stream_cipher_incment({Type, Key, IV, PlainTexts}) ->
    StateE = crypto:stream_init(Type, Key, IV),
    StateD = crypto:stream_init(Type, Key, IV),
    stream_cipher_incment_loop(StateE, StateD, PlainTexts, [], iolist_to_binary(PlainTexts));
stream_cipher_incment({Type, Key, IV, PlainTexts, _CipherText}) ->
    stream_cipher_incment({Type, Key, IV, PlainTexts}).

stream_cipher_incment_loop(_State, OrigState, [], Acc, Plain) ->
    CipherText = iolist_to_binary(lists:reverse(Acc)),
    case crypto:stream_decrypt(OrigState, CipherText) of
	{_, Plain} ->
	    ok;
	Other ->
	    ct:fail({{crypto, stream_decrypt, [OrigState, CipherText]}, {expected, Plain}, {got, Other}})
    end;
stream_cipher_incment_loop(State0, OrigState, [PlainText | PlainTexts], Acc, Plain) ->
    {State, CipherText} = crypto:stream_encrypt(State0, PlainText),
    stream_cipher_incment_loop(State, OrigState, PlainTexts, [CipherText | Acc], Plain).

aead_cipher({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, Info}) ->
    Plain = iolist_to_binary(PlainText),
    case crypto:block_encrypt(Type, Key, IV, {AAD, Plain}) of
	{CipherText, CipherTag} ->
	    ok;
	Other0 ->
	    ct:fail({{crypto,
                      block_encrypt,
                      [{info,Info}, {key,Key}, {pt,PlainText}, {iv,IV}, {aad,AAD}, {ct,CipherText}, {tag,CipherTag}]},
                     {expected, {CipherText, CipherTag}},
                     {got, Other0}})
    end,
    case crypto:block_decrypt(Type, Key, IV, {AAD, CipherText, CipherTag}) of
	Plain ->
	    ok;
	Other1 ->
	    ct:fail({{crypto,
                      block_decrypt,
                      [{info,Info}, {key,Key}, {pt,PlainText}, {iv,IV}, {aad,AAD}, {ct,CipherText}, {tag,CipherTag}]},
                     {expected, Plain},
                     {got, Other1}})
    end;
aead_cipher({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, TagLen, Info}) ->
    <<TruncatedCipherTag:TagLen/binary, _/binary>> = CipherTag,
    Plain = iolist_to_binary(PlainText),
    try crypto:block_encrypt(Type, Key, IV, {AAD, Plain, TagLen}) of
	{CipherText, TruncatedCipherTag} ->
	    ok;
	Other0 ->
	    ct:fail({{crypto,
                      block_encrypt, 
                      [{info,Info}, {key,Key}, {pt,PlainText}, {iv,IV}, {aad,AAD}, {ct,CipherText}, {tag,CipherTag}, {taglen,TagLen}]},
                     {expected, {CipherText, TruncatedCipherTag}},
                     {got, Other0}})
    catch
        error:E ->
            ct:log("~p",[{Type, Key, PlainText, IV, AAD, CipherText, CipherTag, TagLen, Info}]),
            try crypto:crypto_one_time_aead(Type, Key, IV, PlainText, AAD, TagLen, true)
            of
                RR ->
                    ct:log("Works: ~p",[RR])
            catch
                CC:EE ->
                    ct:log("~p:~p", [CC,EE])
            end,
            ct:fail("~p",[E])
    end,
    case crypto:block_decrypt(Type, Key, IV, {AAD, CipherText, TruncatedCipherTag}) of
	Plain ->
	    ok;
	Other1 ->
	    ct:fail({{crypto,
                      block_decrypt, 
                      [{info,Info}, {key,Key}, {pt,PlainText}, {iv,IV}, {aad,AAD}, {ct,CipherText}, {tag,CipherTag},
                       {truncated,TruncatedCipherTag}]},
                     {expected, Plain},
                     {got, Other1}})
    end.

aead_cipher_ng({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, Info}) ->
    Plain = iolist_to_binary(PlainText),
    case crypto:crypto_one_time_aead(Type, Key, IV, PlainText, AAD, true) of
	{CipherText, CipherTag} ->
	    ok;
	Other0 ->
	    ct:fail({{crypto,
                      block_encrypt,
                      [{info,Info}, {key,Key}, {pt,PlainText}, {iv,IV}, {aad,AAD}, {ct,CipherText}, {tag,CipherTag}]},
                     {expected, {CipherText, CipherTag}},
                     {got, Other0}})
    end,
    case crypto:crypto_one_time_aead(Type, Key, IV, CipherText, AAD, CipherTag, false) of
	Plain ->
	    ok;
	Other1 ->
	    ct:fail({{crypto,
                      block_decrypt,
                      [{info,Info}, {key,Key}, {pt,PlainText}, {iv,IV}, {aad,AAD}, {ct,CipherText}, {tag,CipherTag}]},
                     {expected, Plain},
                     {got, Other1}})
    end;
aead_cipher_ng({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, TagLen, Info}) ->
    <<TruncatedCipherTag:TagLen/binary, _/binary>> = CipherTag,
    Plain = iolist_to_binary(PlainText),
    try crypto:crypto_one_time_aead(Type, Key, IV, PlainText, AAD, TagLen, true) of
	{CipherText, TruncatedCipherTag} ->
	    ok;
	Other0 ->
	    ct:fail({{crypto,
                      block_encrypt, 
                      [{info,Info}, {key,Key}, {pt,PlainText}, {iv,IV}, {aad,AAD}, {ct,CipherText}, {tag,CipherTag}, {taglen,TagLen}]},
                     {expected, {CipherText, TruncatedCipherTag}},
                     {got, Other0}})
    catch
        error:E ->
            ct:log("~p",[{Type, Key, PlainText, IV, AAD, CipherText, CipherTag, TagLen, Info}]),
            try crypto:crypto_one_time_aead(Type, Key, IV, PlainText, AAD, TagLen, true)
            of
                RR ->
                    ct:log("Works: ~p",[RR])
            catch
                CC:EE ->
                    ct:log("~p:~p", [CC,EE])
            end,
            ct:fail("~p",[E])
    end,
    case crypto:crypto_one_time_aead(Type, Key, IV, CipherText, AAD, TruncatedCipherTag, false) of
	Plain ->
	    ok;
	Other1 ->
	    ct:fail({{crypto,
                      block_decrypt, 
                      [{info,Info}, {key,Key}, {pt,PlainText}, {iv,IV}, {aad,AAD}, {ct,CipherText}, {tag,CipherTag},
                       {truncated,TruncatedCipherTag}]},
                     {expected, Plain},
                     {got, Other1}})
    end.

mk_bad_tag(CipherTag) ->
    case <<0:(size(CipherTag))/unit:8>> of
        CipherTag -> % The correct tag may happen to be a suite of zeroes
            <<1:(size(CipherTag))/unit:8>>;
        X ->
            X
    end.

aead_cipher_bad_tag({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, Info}) ->
    Plain = iolist_to_binary(PlainText),
    BadTag = mk_bad_tag(CipherTag),
    case crypto:crypto_one_time_aead(Type, Key, IV, CipherText, AAD, BadTag, false) of
	error ->
            ok;
	Plain ->
            ct:log("~p:~p~n info: ~p~n key: ~p~n pt: ~p~n iv: ~p~n aad: ~p~n ct: ~p~n tag: ~p~n bad tag: ~p~n",
                   [?MODULE,?LINE,Info, Key, PlainText, IV, AAD, CipherText, CipherTag, BadTag]),
            ct:fail("Didn't fail on bad tag")
    end;
aead_cipher_bad_tag({Type, Key, PlainText, IV, AAD, CipherText, CipherTag, TagLen, Info}) ->
    Plain = iolist_to_binary(PlainText),
    <<TruncatedCipherTag:TagLen/binary, _/binary>> = CipherTag,
    BadTruncatedTag = mk_bad_tag(TruncatedCipherTag),
    case  crypto:crypto_one_time_aead(Type, Key, IV, CipherText, AAD, BadTruncatedTag, false) of
	error ->
	    ok;
	Plain ->
            ct:log("~p:~p~n info: ~p~n key: ~p~n pt: ~p~n iv: ~p~n aad: ~p~n ct: ~p~n tag: ~p~n bad tag: ~p~n",
                   [Info, Key, PlainText, IV, AAD, CipherText, TruncatedCipherTag, BadTruncatedTag]),
            ct:fail("Didn't fail on bad tag")
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
    Signature = crypto:sign(Type, Hash, Msg, Private),
    case crypto:verify(Type, Hash, Msg, Signature, Public) of
	true ->
            ct:log("OK crypto:sign(~p, ~p, ..., ..., ...)", [Type,Hash]),
	    negative_verify(Type, Hash, Msg, <<10,20>>, Public);
	false ->
            ct:log("ERROR crypto:sign(~p, ~p, ..., ..., ...)", [Type,Hash]),
	    ct:fail({{crypto, verify, [Type, Hash, Msg, Signature, Public]}})
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
    try
        crypto:public_encrypt(Type, Msg, Public, Padding)
    of
        PublicEcn ->
            try
                crypto:private_decrypt(Type, PublicEcn, Private, Padding)
            of
                Msg ->
                    ok;
                Other ->
                    ct:fail({{crypto, private_decrypt, [Type, PublicEcn, Private, Padding]}, {expected, Msg}, {got, Other}})
            catch
                CC:EE ->
                    ct:fail({{crypto, private_decrypt, [Type, PublicEcn, Private, Padding]}, {expected, Msg}, {got, {CC,EE}}})
            end
    catch
        CC:EE ->
            ct:fail({{crypto, public_encrypt, [Type, Msg, Public, Padding]}, {got, {CC,EE}}})
    end. 


do_private_encrypt({Type, Public, Private, Msg, Padding}) ->
    try
        crypto:private_encrypt(Type, Msg, Private, Padding)
    of
        PrivEcn ->
            try
                crypto:public_decrypt(Type, PrivEcn, Public, Padding)
            of
                Msg ->
                    ok;
                Other ->
                    ct:fail({{crypto, public_decrypt, [Type, PrivEcn, Public, Padding]}, {expected, Msg}, {got, Other}})
            catch
                CC:EE ->
                    ct:fail({{crypto, public_decrypt, [Type, PrivEcn, Public, Padding]}, {expected, Msg}, {got, {CC,EE}}})
            end
    catch
        CC:EE ->
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
    Secret = crypto:compute_key(Type, Pub, Priv, Curve),
     case Secret of
	 SharedSecret ->
	     ok;
	 Other ->
	     ct:fail({{crypto, compute_key, [Type, Pub, Priv, Curve]}, {expected, SharedSecret}, {got, Other}})
     end.

do_generate({ecdh = Type, Curve, Priv, Pub}) ->
    case crypto:generate_key(Type, Curve, Priv) of
	{Pub, _} ->
	    ok;
	{Other, _} ->
	    ct:fail({{crypto, generate_key, [Type, Priv, Curve]}, {expected, Pub}, {got, Other}})
    end;
do_generate({rsa = Type, Mod, Exp}) ->
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

bin2hexstr(B) when is_binary(B) ->
    io_lib:format("~.16b",[crypto:bytes_to_integer(B)]).    

decstr2int(S) when is_binary(S) ->
    list_to_integer(binary:bin_to_list(S));
decstr2int(S) ->
    list_to_integer(S).

is_supported(Group) ->
    lists:member(Group, lists:append([Algo ||  {_, Algo}  <- crypto:supports()])). 

mac_listify(Blocks) ->
    lists:map(fun do_mac_listify/1, Blocks).
block_iolistify(Blocks) ->
    lists:map(fun do_block_iolistify/1, Blocks).
stream_iolistify(Streams) ->
    lists:map(fun do_stream_iolistify/1, Streams).

do_mac_listify({MType, Type, Key, Text, CMac}) ->
    {MType, Type, iolistify(Key), iolistify(Text), CMac};
do_mac_listify({MType, Type, Key, Text, Size, CMac}) ->
    {MType, Type, iolistify(Key), iolistify(Text), Size, CMac}.

do_stream_iolistify({Type, Key, PlainText}) ->
    {Type, iolistify(Key), iolistify(PlainText)};
do_stream_iolistify({Type, Key, IV, PlainText}) ->
    {Type, iolistify(Key), IV, iolistify(PlainText)};
do_stream_iolistify({Type, Key, IV, PlainText, CipherText}) ->
    {Type, iolistify(Key), IV, iolistify(PlainText), CipherText}.
do_block_iolistify({Type, Key, IV, PlainText}) when Type == des_cbc ;
                                                    Type == des_ede3_cbc ;
                                                    Type == des3_cbc ;
                                                    Type == des_ede3 ;
                                                    Type == des_ede3_cfb ;
                                                    Type == des_ede3_cbf ;
                                                    Type == des3_cbf ;
                                                    Type == des3_cfb
                                                    ->
    {Type, Key, IV, des_iolistify(PlainText)};
do_block_iolistify({Type, Key, PlainText}) ->
    {Type, iolistify(Key), iolistify(PlainText)};
do_block_iolistify({Type, Key, IV, PlainText}) ->
    {Type, iolistify(Key), IV, iolistify(PlainText)};
do_block_iolistify({Type, Key, IV, PlainText, CipherText}) ->
    {Type, iolistify(Key), IV, iolistify(PlainText), CipherText}.

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

des_iolistify(Msg) ->    
    des_iolist(erlang:byte_size(Msg) div 8, Msg, []).

des_iolist(1, Msg, Acc) ->
    lists:reverse([Msg | Acc]);
des_iolist(Split, Msg, Acc) ->
    <<Part:8/binary, Rest/binary>> = Msg,
    des_iolist(Split-1, Rest, [Part | Acc]).

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
    [{sign_verify,      rsa_sign_verify_tests(Config, Msg, Public, Private, PublicS, PrivateS, SignVerify_OptsToTry)},
     {pub_priv_encrypt, gen_rsa_pub_priv_tests(PublicS, PrivateS, MsgPubEnc, PrivEnc_OptsToTry)},
     {pub_pub_encrypt,  gen_rsa_pub_priv_tests(PublicS, PrivateS, MsgPubEnc, PubEnc_OptsToTry)},
     {generate, [{rsa, 1024, 3},  {rsa, 2048, 17},  {rsa, 3072, 65537}]}
     | Config];
group_config(dss = Type, Config) ->
    Msg = dss_plain(),
    Public = dss_params() ++ [dss_public()], 
    Private = dss_params() ++ [dss_private()], 
    SupportedHashs = proplists:get_value(hashs, crypto:supports(), []),
    DssHashs = 
        case crypto:info_lib() of
            [{<<"OpenSSL">>,LibVer,_}] when is_integer(LibVer), LibVer > 16#10001000 ->
                [sha, sha224, sha256, sha384, sha512];
            [{<<"OpenSSL">>,LibVer,_}] when is_integer(LibVer), LibVer > 16#10000000 ->
                [sha, sha224, sha256];
            _Else ->
                [sha]
        end,
    SignVerify = [{Type, Hash, Public, Private, Msg} 
                  || Hash <- DssHashs,
                     lists:member(Hash, SupportedHashs)],
    MsgPubEnc = <<"7896345786348 Asldi">>,
    PubPrivEnc = [{dss, Public, Private, MsgPubEnc, []}],
    [{sign_verify, SignVerify}, {pub_priv_encrypt, PubPrivEnc}  | Config];
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
    [{sign_verify,TestVectors} | Config]; 
group_config(srp, Config) ->
    GenerateCompute = [srp3(), srp6(), srp6a(), srp6a_smaller_prime()],
    [{generate_compute, GenerateCompute} | Config];
group_config(ecdh, Config) ->
    Compute = ecdh(),
    Generate = ecc(),
    [{compute, Compute}, {generate, Generate} | Config];
group_config(dh, Config) ->
    GenerateCompute = [dh()],
    [{generate_compute, GenerateCompute} | Config];
group_config(poly1305, Config) ->
    V = [%% {Key, Txt, Expect}
         {%% RFC7539 2.5.2
           crypto_SUITE:hexstr2bin("85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"),
           <<"Cryptographic Forum Research Group">>,
           crypto_SUITE:hexstr2bin("a8061dc1305136c6c22b8baf0c0127a9")
         }
        ],
    [{poly1305,V} | Config];

group_config(F, Config) ->
    TestVectors = fun() -> ?MODULE:F(Config) end,
    [{cipher, TestVectors} | Config].


configure_mac(MacType, SubType, Config) ->
    case do_configure_mac(MacType, SubType, Config) of
        undefined ->
            {skip, io:format("No ~p test vectors for ~p", [MacType, SubType])};
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
        aes_256_cbc ->
            fun() -> read_rsp(Config, Cipher,  ["CMACGenAES256.rsp", "CMACVerAES256.rsp"]) end;
        _ ->
            undefined
    end.


zip3_special(Type, SubType, As, Bs, Cs) ->
    [{Type, SubType, A, B, C}
     || {A,B,C} <- lists:zip3(As, Bs, Cs)].


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
    gen_rsa_sign_verify_tests([md5, ripemd160, sha, sha224, sha256], Msg, Public, Private,
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
    SupOpts = proplists:get_value(rsa_opts, crypto:supports(), []),
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

aes_ige256(_) ->
    [{aes_ige256,
      hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
      hexstr2bin("000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F"),
      hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
      {aes_ige256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("4D0F9E735749215C05CB20DA00F7814B77D33F8A668BEBBAC1739AB20302D4FE"),
       hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
      {aes_ige256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("2A5569424DAE1ACEABDEEA108DB4606AE21A9227CAB5F55BF52535CFA2B34717"),
       hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
      {aes_ige256,
       hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
       hexstr2bin("15D5A583D2D668E518E683D9BDF1B6D0E0C3B1E5D5C1D51E964822E1ADE88DFA"),
       hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")}
     ].

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

aes_ctr(_) ->
    [  %% F.5.3  CTR-AES192.Encrypt
       {aes_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
	hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
       {aes_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
	hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
       {aes_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
	hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef") },
       {aes_ctr, hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
	hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},
       
       %% F.5.3  CTR-AES192.Encrypt
       {aes_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
	hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
       {aes_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
	hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
       {aes_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
	hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
       {aes_ctr, hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
	hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},
       
       %% F.5.5  CTR-AES256.Encrypt
       {aes_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"), 
	hexstr2bin("6bc1bee22e409f96e93d7e117393172a")},
       {aes_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff00"), 
	hexstr2bin("ae2d8a571e03ac9c9eb76fac45af8e51")},
       {aes_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff01"), 
	hexstr2bin("30c81c46a35ce411e5fbc1191a0a52ef")},
       {aes_ctr, hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"), 
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdff02"), 
	hexstr2bin("f69f2445df4f9b17ad2b417be66c3710")},

       {aes_ctr,  hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
	hexstr2bin("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"),
	long_msg()}
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
    25854665488880835237281628794585130313500176551981812527054397586638455298000483144002221850980183404910190346416063318160497344811383498859129095184158800144312512447497510551471331451396405348497845813002058423110442376886564659959543650802132345311573634832461635601376738282831340827591903548964194832978.
dss_private() ->
    441502407453038284293378221372000880210588566361.  
dss_params() ->
    [109799869232806890760655301608454668257695818999841877165019612946154359052535682480084145133201304812979481136659521529774182959764860329095546511521488413513097576425638476458000255392402120367876345280670101492199681798674053929238558140260669578407351853803102625390950534052428162468100618240968893110797,
     1349199015905534965792122312016505075413456283393,
     18320614775012672475365915366944922415598782131828709277168615511695849821411624805195787607930033958243224786899641459701930253094446221381818858674389863050420226114787005820357372837321561754462061849169568607689530279303056075793886577588606958623645901271866346406773590024901668622321064384483571751669].

ec_key_named() ->
    Curve = hd(crypto:ec_curves()),
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
    %% {ALGORITHM, (SHA)}, SECRET KEY, PUBLIC KEY,  MESSAGE, SIGNATURE}
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
    Curves = crypto:ec_curves() ++ 
        [X  || X <- proplists:get_value(curves, crypto:supports(), []),
               lists:member(X, [x25519,x448])],
    TestCases =
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
        ],
    lists:filter(fun ({_Type, _Pub, _Priv, Curve, _SharedSecret}) ->
                         lists:member(Curve, Curves)
                 end,
                 TestCases).

dh() ->
    {dh, 90970053988169282502023478715631717259407236400413906591937635666709823903223997309250405131675572047545403771567755831138144089197560332757755059848492919215391041119286178688014693040542889497092308638580104031455627238700168892909539193174537248629499995652186913900511641708112112482297874449292467498403, 2}.



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
    Curves = crypto:ec_curves(),
    TestCases =
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
                       "A2649984F2135C301EA3ACB0776CD4F125389B311DB3BE32")}],
    lists:filter(fun ({_Type, Curve, _Priv, _Pub}) ->
                         lists:member(Curve, Curves)
                 end,
                 TestCases).

int_to_bin(X) when X < 0 -> int_to_bin_neg(X, []);
int_to_bin(X) -> int_to_bin_pos(X, []).

int_to_bin_pos(0,Ds=[_|_]) ->
    list_to_binary(Ds);
int_to_bin_pos(X,Ds) ->
    int_to_bin_pos(X bsr 8, [(X band 255)|Ds]).

int_to_bin_neg(-1, Ds=[MSB|_]) when MSB >= 16#80 ->
    list_to_binary(Ds);
int_to_bin_neg(X,Ds) ->
    int_to_bin_neg(X bsr 8, [(X band 255)|Ds]).

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
    Key = hexstr2bin(Key0),
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
-define(chk_api_name(Call, Expect),
        %% Check that we don't segfault on bad names
        (fun() -> % avoid binding vars
                 try
                     Call
                 catch 
                     Expect -> ok;

                     Class:Reason:Stack ->
                         ct:log("~p:~p~n~p", [Class,Reason,Stack]),
                         ct:fail("Bad respons for bad name")
                 end
         end)()
       ).

bad_cipher_name(_Config) ->
    ?chk_api_name(crypto:crypto_init(foobar, <<1:128>>, true),
                  error:{badarg,{"api_ng.c",_Line},"Unknown cipher"}).

bad_generate_key_name(_Config) ->
    ?chk_api_name(crypto:generate_key(foobar, [1024]),
                  error:function_clause).

bad_hash_name(_Config) ->
    ?chk_api_name(crypto:hash_init(foobar),
                  error:badarg).

bad_hmac_name(_Config) ->
    ?chk_api_name(crypto:hmac(foobar, <<1:1024>>, "nothing"),
                  error:badarg).

bad_mac_name(_Config) ->
    ?chk_api_name(crypto:mac(foobar, <<1:1024>>, "nothing"),
                  error:function_clause).

bad_sign_name(_Config) ->
    ?chk_api_name(crypto:sign(rsa, foobar, "nothing", <<1:1024>>),
                  error:badarg),
    ?chk_api_name(crypto:sign(foobar, sha, "nothing", <<1:1024>>),
                  error:badarg).
    
bad_verify_name(_Config) ->
    ?chk_api_name(crypto:verify(rsa, foobar, "nothing","nothing",  <<1:1024>>),
                  error:badarg),
    ?chk_api_name(crypto:verify(foobar, sha, "nothing", "nothing", <<1:1024>>),
                  error:badarg).


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
