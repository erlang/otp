%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(old_crypto_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, init_per_group/2,end_per_group/2, 
	 init_per_testcase/2,
	 end_per_testcase/2,
	 info/1,
	 link_test/1,
	 md5/1,
	 md5_update/1,
	 md4/1,
	 md4_update/1,
	 sha/1,
	 sha_update/1,
         hmac_update_sha/1,
         hmac_update_sha_n/1,
         hmac_update_sha256/1,
         hmac_update_sha512/1,
         hmac_update_md5/1,
         hmac_update_md5_io/1,
         hmac_update_md5_n/1,
         hmac_rfc2202/1,
	 hmac_rfc4231_sha224/1,
	 hmac_rfc4231_sha256/1,
	 hmac_rfc4231_sha384/1,
	 hmac_rfc4231_sha512/1,
   ripemd160/1,
   ripemd160_update/1,
	 sha256/1,
	 sha256_update/1,
	 sha512/1,
	 sha512_update/1,
	 md5_mac/1,
	 md5_mac_io/1,
	 des_cbc/1,
	 des_cbc_iter/1,
	 des_cfb/1,
	 des_cfb_iter/1,
	 des_ecb/1,
	 des3_cbc/1,
	 des3_cfb/1,
	 rc2_cbc/1,
	 aes_cfb/1,
	 aes_cbc/1,
	 aes_cbc_iter/1,
	 aes_ctr/1,
	 aes_ctr_stream/1,
	 mod_exp_test/1,
	 rand_uniform_test/1,
	 strong_rand_test/1,
	 rsa_verify_test/1,
	 dsa_verify_test/1,
	 rsa_sign_test/1,
	 rsa_sign_hash_test/1,
	 dsa_sign_test/1,	 
	 dsa_sign_hash_test/1,
	 rsa_encrypt_decrypt/1,
	 dh/1,
	 srp3/1, srp6/1, srp6a/1,
	 ec/1,
	 exor_test/1,
	 rc4_test/1,
	 rc4_stream_test/1,
	 blowfish_cfb64/1,
	 smp/1]).

-export([hexstr2bin/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [link_test, {group, info}].

groups() ->
    [{info, [sequence],[info, {group, rest}]},
     {rest, [],
      [md5, md5_update, md4, md4_update, md5_mac,
       md5_mac_io, ripemd160, ripemd160_update, sha, sha_update,
       sha256, sha256_update, sha512, sha512_update,
       hmac_update_sha, hmac_update_sha_n, hmac_update_sha256, hmac_update_sha512,
       hmac_update_md5_n, hmac_update_md5_io, hmac_update_md5,
       hmac_rfc2202, hmac_rfc4231_sha224, hmac_rfc4231_sha256,
       hmac_rfc4231_sha384, hmac_rfc4231_sha512,
       des_cbc, aes_cfb, aes_cbc,
       des_cfb, des_cfb_iter, des3_cbc, des3_cfb, rc2_cbc,
       aes_cbc_iter, aes_ctr, aes_ctr_stream, des_cbc_iter, des_ecb,
       rand_uniform_test, strong_rand_test,
       rsa_verify_test, dsa_verify_test, rsa_sign_test,
       rsa_sign_hash_test, dsa_sign_test, dsa_sign_hash_test,
       rsa_encrypt_decrypt, dh, srp3, srp6, srp6a, ec, exor_test,
       rc4_test, rc4_stream_test, mod_exp_test, blowfish_cfb64,
       smp]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(info, Config) ->
    Config;
init_per_testcase(_Name,Config) ->
    io:format("init_per_testcase\n"),
    ?line crypto:start(),
    Config.

end_per_testcase(info, Config) ->
    Config;
end_per_testcase(_Name,Config) ->
    io:format("end_per_testcase\n"),
    ?line crypto:stop(),
    Config.

%%
%%
link_test(doc) ->
    ["Test that the library is statically linked to libcrypto.a."];
link_test(suite) ->
    [];
link_test(Config) when is_list(Config) ->
    ?line case os:type() of
	      {unix,darwin} -> {skipped,"Darwin cannot link statically"};
	      {unix,_} -> link_test_1();
	      _ -> {skip,"Only runs on Unix"}
	  end.

link_test_1() ->    
    ?line CryptoPriv = code:priv_dir(crypto),
    ?line Wc = filename:join([CryptoPriv,"lib","crypto.*"]),
    ?line case filelib:wildcard(Wc) of
	      [] -> {skip,"Didn't find the crypto driver"};
	      [Drv] -> link_test_2(Drv)
	  end.

link_test_2(Drv) ->
    case ldd_program() of
	none ->
	    {skip,"No ldd-like program found"};
	Ldd ->
	    Cmd = Ldd ++ " " ++ Drv,
	    Libs = os:cmd(Cmd),
	    io:format("~p\n", [Libs]),
	    case string:str(Libs, "libcrypto") of
		0 -> 
		    case ?t:is_commercial() of
			true ->
			    ?t:fail({libcrypto,statically_linked});
			false ->
			    {comment,"Statically linked (OK for open-source platform)"}
		    end;
		_ ->
		    ok
	    end
    end.

ldd_program() ->
    case os:find_executable("ldd") of
	false ->
	    case os:type() of
		{unix,darwin} ->
		    case os:find_executable("otool") of
			false -> none;
			Otool -> Otool ++ " -L"
		    end;
		_ ->
		    none
	    end;
 	Ldd when is_list(Ldd) -> Ldd
    end.



info(doc) ->
    ["Call the info function."];
info(suite) ->
    [];
info(Config) when is_list(Config) ->
    case {code:lib_dir(crypto),?t:is_commercial()} of
	{{error,bad_name},false} ->
	    {skip,"Missing crypto application"};
	{_,_} ->
	    ?line crypto:start(),
	    ?line Info = crypto:info(),
	    ?line Exports = lists:usort([F || {F,_} <- crypto:module_info(exports)]),
	    ?line [] = Info -- Exports,
	    ?line NotInInfo = Exports -- Info,
	    io:format("NotInInfo = ~p\n", [NotInInfo]),
	    %% BlackList = lists:sort([des_ede3_cbc_decrypt, des_ede3_cbc_encrypt,
	    %% 			    dh_check, dh_generate_parameters,
	    %% 			    module_info, start, stop, version]),
	    %% ?line BlackList = NotInInfo,

	    ?line InfoLib = crypto:info_lib(),
	    ?line [_|_] = InfoLib,
	    F = fun([{Name,VerN,VerS}|T],Me) ->
			?line true = is_binary(Name),
			?line true = is_integer(VerN),
			?line true = is_binary(VerS),
			Me(T,Me);
		   ([],_) ->
			ok
		end,
	    ?line F(InfoLib,F),
	    ?line crypto:stop()
     end.

%%
%%
md5(doc) ->
    ["Generate MD5 message digests and check the result. Examples are "
     "from RFC-1321."];
md5(suite) ->
    [];
md5(Config) when is_list(Config) ->
    ?line m(crypto:md5(""), 
		hexstr2bin("d41d8cd98f00b204e9800998ecf8427e")),
    ?line m(crypto:md5("a"), 
		hexstr2bin("0cc175b9c0f1b6a831c399e269772661")),
    ?line m(crypto:md5("abc"), 
		hexstr2bin("900150983cd24fb0d6963f7d28e17f72")),
    ?line m(crypto:md5("message digest"),
		hexstr2bin("f96b697d7cb7938d525a2f31aaf161d0")),
    ?line m(crypto:md5("abcdefghijklmnopqrstuvwxyz"),
	    hexstr2bin("c3fcd3d76192e4007dfb496cca67e13b")),
    ?line m(crypto:md5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
		     "0123456789"),  
	    hexstr2bin("d174ab98d277d9f5a5611c2c9f419d9f")),
    ?line m(crypto:md5("12345678901234567890123456789012345678901234567890"
		     "123456789012345678901234567890"),
	    hexstr2bin("57edf4a22be3c955ac49da2e2107b67a")).

%%
%%
md5_update(doc) ->
    ["Generate MD5 message using md5_init, md5_update, and md5_final, and"
     "check the result. Examples are from RFC-1321."];
md5_update(suite) ->
    [];
md5_update(Config) when is_list(Config) ->
    ?line Ctx = crypto:md5_init(),
    ?line Ctx1 = crypto:md5_update(Ctx, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    ?line Ctx2 = crypto:md5_update(Ctx1, "abcdefghijklmnopqrstuvwxyz"
				   "0123456789"),
    ?line m(crypto:md5_final(Ctx2),  
	    hexstr2bin("d174ab98d277d9f5a5611c2c9f419d9f")).

%%
%%
md4(doc) ->
    ["Generate MD4 message digests and check the result. Examples are "
     "from RFC-1321."];
md4(suite) ->
    [];
md4(Config) when is_list(Config) ->
    ?line m(crypto:md4(""), 
	    hexstr2bin("31d6cfe0d16ae931b73c59d7e0c089c0")),
    ?line m(crypto:md4("a"), 
	    hexstr2bin("bde52cb31de33e46245e05fbdbd6fb24")),
    ?line m(crypto:md4("abc"), 
	    hexstr2bin("a448017aaf21d8525fc10ae87aa6729d")),
    ?line m(crypto:md4("message digest"),
	    hexstr2bin("d9130a8164549fe818874806e1c7014b")),
    ?line m(crypto:md4("abcdefghijklmnopqrstuvwxyz"),
	    hexstr2bin("d79e1c308aa5bbcdeea8ed63df412da9")),
    ?line m(crypto:md4("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
		       "0123456789"),  
	    hexstr2bin("043f8582f241db351ce627e153e7f0e4")),
    ?line m(crypto:md4("12345678901234567890123456789012345678901234567890"
		       "123456789012345678901234567890"),
	    hexstr2bin("e33b4ddc9c38f2199c3e7b164fcc0536")).

%%
%%
md4_update(doc) ->
    ["Generate MD5 message using md5_init, md5_update, and md5_final, and"
     "check the result. Examples are from RFC-1321."];
md4_update(suite) ->
    [];
md4_update(Config) when is_list(Config) ->
    ?line Ctx = crypto:md4_init(),
    ?line Ctx1 = crypto:md4_update(Ctx, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    ?line Ctx2 = crypto:md4_update(Ctx1, "abcdefghijklmnopqrstuvwxyz"
				   "0123456789"),
    ?line m(crypto:md4_final(Ctx2),  
	    hexstr2bin("043f8582f241db351ce627e153e7f0e4")).

%%
%%
sha(doc) ->
    ["Generate SHA message digests and check the result. Examples are "
     "from FIPS-180-1."];
sha(suite) ->
    [];
sha(Config) when is_list(Config) ->
    ?line m(crypto:sha("abc"),
	     hexstr2bin("A9993E364706816ABA3E25717850C26C9CD0D89D")),
    ?line m(crypto:sha("abcdbcdecdefdefgefghfghighijhijkijkljklmklm"
		       "nlmnomnopnopq"), 
		hexstr2bin("84983E441C3BD26EBAAE4AA1F95129E5E54670F1")).


%%
hmac_update_sha_n(doc) ->
    ["Request a larger-than-allowed SHA1 HMAC using hmac_init, hmac_update, and hmac_final_n. "
     "Expected values for examples are generated using crypto:sha_mac." ];
hmac_update_sha_n(suite) ->
    [];
hmac_update_sha_n(Config) when is_list(Config) ->
    ?line Key = hexstr2bin("00010203101112132021222330313233"
                           "04050607141516172425262734353637"
                           "08090a0b18191a1b28292a2b38393a3b"
                           "0c0d0e0f1c1d1e1f2c2d2e2f3c3d3e3f"),
    ?line Data = "Sampl",
    ?line Data2 = "e #1",
    ?line Ctx = crypto:hmac_init(sha, Key),
    ?line Ctx2 = crypto:hmac_update(Ctx, Data),
    ?line Ctx3 = crypto:hmac_update(Ctx2, Data2),
    ?line Mac = crypto:hmac_final_n(Ctx3, 1024),
    ?line Exp = crypto:sha_mac(Key, lists:flatten([Data, Data2])),
    ?line m(Exp, Mac),
    ?line m(size(Exp), size(Mac)).


hmac_update_sha(doc) ->
    ["Generate an SHA1 HMAC using hmac_init, hmac_update, and hmac_final. "
     "Expected values for examples are generated using crypto:sha_mac." ];
hmac_update_sha(suite) ->
    [];
hmac_update_sha(Config) when is_list(Config) ->
    ?line Key = hexstr2bin("00010203101112132021222330313233"
                           "04050607141516172425262734353637"
                           "08090a0b18191a1b28292a2b38393a3b"
                           "0c0d0e0f1c1d1e1f2c2d2e2f3c3d3e3f"),
    ?line Data = "Sampl",
    ?line Data2 = "e #1",
    ?line Ctx = crypto:hmac_init(sha, Key),
    ?line Ctx2 = crypto:hmac_update(Ctx, Data),
    ?line Ctx3 = crypto:hmac_update(Ctx2, Data2),
    ?line Mac = crypto:hmac_final(Ctx3),
    ?line Exp = crypto:hmac(sha, Key, lists:flatten([Data, Data2])),
    ?line m(Exp, Mac).

hmac_update_sha256(doc) ->
    ["Generate an SHA256 HMAC using hmac_init, hmac_update, and hmac_final. "
     "Expected values for examples are generated using crypto:sha256_mac." ];
hmac_update_sha256(suite) ->
    [];
hmac_update_sha256(Config) when is_list(Config) ->
    if_supported(sha256, fun() -> hmac_update_sha256_do() end).

hmac_update_sha256_do() ->
    ?line Key = hexstr2bin("00010203101112132021222330313233"
                           "04050607141516172425262734353637"
                           "08090a0b18191a1b28292a2b38393a3b"
                           "0c0d0e0f1c1d1e1f2c2d2e2f3c3d3e3f"),
    ?line Data = "Sampl",
    ?line Data2 = "e #1",
    ?line Ctx = crypto:hmac_init(sha256, Key),
    ?line Ctx2 = crypto:hmac_update(Ctx, Data),
    ?line Ctx3 = crypto:hmac_update(Ctx2, Data2),
    ?line Mac = crypto:hmac_final(Ctx3),
    ?line Exp = crypto:hmac(sha256, Key, lists:flatten([Data, Data2])),
    ?line m(Exp, Mac).

hmac_update_sha512(doc) ->
    ["Generate an SHA512 HMAC using hmac_init, hmac_update, and hmac_final. "
     "Expected values for examples are generated using crypto:sha512_mac." ];
hmac_update_sha512(suite) ->
    [];
hmac_update_sha512(Config) when is_list(Config) ->
    if_supported(sha512, fun() -> hmac_update_sha512_do() end).

hmac_update_sha512_do() ->
    ?line Key = hexstr2bin("00010203101112132021222330313233"
                           "04050607141516172425262734353637"
                           "08090a0b18191a1b28292a2b38393a3b"
                           "0c0d0e0f1c1d1e1f2c2d2e2f3c3d3e3f"),
    ?line Data = "Sampl",
    ?line Data2 = "e #1",
    ?line Ctx = crypto:hmac_init(sha512, Key),
    ?line Ctx2 = crypto:hmac_update(Ctx, Data),
    ?line Ctx3 = crypto:hmac_update(Ctx2, Data2),
    ?line Mac = crypto:hmac_final(Ctx3),
    ?line Exp = crypto:hmac(sha512, Key, lists:flatten([Data, Data2])),
    ?line m(Exp, Mac).
    
hmac_update_md5(doc) ->
    ["Generate an MD5 HMAC using hmac_init, hmac_update, and hmac_final. "
     "Expected values for examples are generated using crypto:md5_mac." ];
hmac_update_md5(suite) ->
    [];
hmac_update_md5(Config) when is_list(Config) ->
    % ?line Key2 = ["A fine speach", "by a fine man!"],
    Key2 = "A fine speach by a fine man!",
    ?line Long1 = "Four score and seven years ago our fathers brought forth on this continent a new nation, conceived in liberty, and dedicated to the proposition that all men are created equal.",
    ?line Long2 = "Now we are engaged in a great civil war, testing whether that nation, or any nation, so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.",
    ?line Long3 = "But, in a larger sense, we can not dedicate, we can not consecrate, we can not hallow this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us-that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion that we here highly resolve that these dead shall not have died in vain-that this nation, under God, shall have a new birth of freedom-and that government of the people, by the people, for the people, shall not perish from the earth.",
    ?line CtxA = crypto:hmac_init(md5, Key2),
    ?line CtxB = crypto:hmac_update(CtxA, Long1),
    ?line CtxC = crypto:hmac_update(CtxB, Long2),
    ?line CtxD = crypto:hmac_update(CtxC, Long3),
    ?line Mac2 = crypto:hmac_final(CtxD),
    ?line Exp2 = crypto:md5_mac(Key2, lists:flatten([Long1, Long2, Long3])), 
    ?line m(Exp2, Mac2).

hmac_rfc2202(doc) ->
    ["Generate an HMAC using hmac, md5_mac, and sha_mac."
     "Test vectors are taken from RFC-2202."];
hmac_rfc2202(suite) ->
    [];
hmac_rfc2202(Config) when is_list(Config) ->
    hmac_rfc2202_md5(),
    hmac_rfc2202_sha().

hmac_rfc2202_md5() ->
    %% Test case 1
    Case1Key = binary:copy(<<16#0b>>, 16),
    Case1Data = <<"Hi There">>,
    Case1Exp = hexstr2bin("9294727a3638bb1c13f48ef8158bfc9d"),

    ?line Case1Mac_1 = crypto:md5_mac(Case1Key, Case1Data),
    ?line Case1Mac_2 = crypto:hmac(md5, Case1Key, Case1Data),
    ?line m(Case1Exp, Case1Mac_1),
    ?line m(Case1Exp, Case1Mac_2),

    %% Test case 2
    Case2Key = <<"Jefe">>,
    Case2Data = <<"what do ya want for nothing?">>,
    Case2Exp = hexstr2bin("750c783e6ab0b503eaa86e310a5db738"),

    ?line Case2Mac_1 = crypto:md5_mac(Case2Key, Case2Data),
    ?line Case2Mac_2 = crypto:hmac(md5, Case2Key, Case2Data),
    ?line m(Case2Exp, Case2Mac_1),
    ?line m(Case2Exp, Case2Mac_2),

    %% Test case 3
    Case3Key = binary:copy(<<16#aa>>, 16),
    Case3Data = binary:copy(<<16#dd>>, 50),
    Case3Exp = hexstr2bin("56be34521d144c88dbb8c733f0e8b3f6"),

    ?line Case3Mac_1 = crypto:md5_mac(Case3Key, Case3Data),
    ?line Case3Mac_2 = crypto:hmac(md5, Case3Key, Case3Data),
    ?line m(Case3Exp, Case3Mac_1),
    ?line m(Case3Exp, Case3Mac_2),

    %% Test case 4
    Case4Key = list_to_binary(lists:seq(1, 16#19)),
    Case4Data = binary:copy(<<16#cd>>, 50),
    Case4Exp = hexstr2bin("697eaf0aca3a3aea3a75164746ffaa79"),

    ?line Case4Mac_1 = crypto:md5_mac(Case4Key, Case4Data),
    ?line Case4Mac_2 = crypto:hmac(md5, Case4Key, Case4Data),
    ?line m(Case4Exp, Case4Mac_1),
    ?line m(Case4Exp, Case4Mac_2),

    %% Test case 5
    Case5Key = binary:copy(<<16#0c>>, 16),
    Case5Data = "Test With Truncation",
    Case5Exp = hexstr2bin("56461ef2342edc00f9bab995690efd4c"),
    Case5Exp96 = hexstr2bin("56461ef2342edc00f9bab995"),

    ?line Case5Mac_1 = crypto:md5_mac(Case5Key, Case5Data),
    ?line Case5Mac_2 = crypto:hmac(md5, Case5Key, Case5Data),
    ?line Case5Mac96_1 = crypto:md5_mac_96(Case5Key, Case5Data),
    ?line Case5Mac96_2 = crypto:hmac(md5, Case5Key, Case5Data, 12),
    ?line m(Case5Exp, Case5Mac_1),
    ?line m(Case5Exp, Case5Mac_2),
    ?line m(Case5Exp96, Case5Mac96_1),
    ?line m(Case5Exp96, Case5Mac96_2),

    %% Test case 6
    Case6Key = binary:copy(<<16#aa>>, 80),
    Case6Data = <<"Test Using Larger Than Block-Size Key - Hash Key First">>,
    Case6Exp = hexstr2bin("6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"),

    ?line Case6Mac_1 = crypto:md5_mac(Case6Key, Case6Data),
    ?line Case6Mac_2 = crypto:hmac(md5, Case6Key, Case6Data),
    ?line m(Case6Exp, Case6Mac_1),
    ?line m(Case6Exp, Case6Mac_2),

    %% Test case 7
    Case7Key = binary:copy(<<16#aa>>, 80),
    Case7Data = <<"Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data">>,
    Case7Exp = hexstr2bin("6f630fad67cda0ee1fb1f562db3aa53e"),

    ?line Case7Mac_1 = crypto:md5_mac(Case7Key, Case7Data),
    ?line Case7Mac_2 = crypto:hmac(md5, Case7Key, Case7Data),
    ?line m(Case7Exp, Case7Mac_1),
    ?line m(Case7Exp, Case7Mac_2).

hmac_rfc2202_sha() ->
    %% Test case 1
    Case1Key = binary:copy(<<16#0b>>, 20),
    Case1Data = <<"Hi There">>,
    Case1Exp = hexstr2bin("b617318655057264e28bc0b6fb378c8ef146be00"),

    ?line Case1Mac_1 = crypto:sha_mac(Case1Key, Case1Data),
    ?line Case1Mac_2 = crypto:hmac(sha, Case1Key, Case1Data),
    ?line m(Case1Exp, Case1Mac_1),
    ?line m(Case1Exp, Case1Mac_2),

    %% Test case 2
    Case2Key = <<"Jefe">>,
    Case2Data = <<"what do ya want for nothing?">>,
    Case2Exp = hexstr2bin("effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"),

    ?line Case2Mac_1 = crypto:sha_mac(Case2Key, Case2Data),
    ?line Case2Mac_2 = crypto:hmac(sha, Case2Key, Case2Data),
    ?line m(Case2Exp, Case2Mac_1),
    ?line m(Case2Exp, Case2Mac_2),

    %% Test case 3
    Case3Key = binary:copy(<<16#aa>>, 20),
    Case3Data = binary:copy(<<16#dd>>, 50),
    Case3Exp = hexstr2bin("125d7342b9ac11cd91a39af48aa17b4f63f175d3"),

    ?line Case3Mac_1 = crypto:sha_mac(Case3Key, Case3Data),
    ?line Case3Mac_2 = crypto:hmac(sha, Case3Key, Case3Data),
    ?line m(Case3Exp, Case3Mac_1),
    ?line m(Case3Exp, Case3Mac_2),

    %% Test case 4
    Case4Key = list_to_binary(lists:seq(1, 16#19)),
    Case4Data = binary:copy(<<16#cd>>, 50),
    Case4Exp = hexstr2bin("4c9007f4026250c6bc8414f9bf50c86c2d7235da"),

    ?line Case4Mac_1 = crypto:sha_mac(Case4Key, Case4Data),
    ?line Case4Mac_2 = crypto:hmac(sha, Case4Key, Case4Data),
    ?line m(Case4Exp, Case4Mac_1),
    ?line m(Case4Exp, Case4Mac_2),

    %% Test case 5
    Case5Key = binary:copy(<<16#0c>>, 20),
    Case5Data = "Test With Truncation",
    Case5Exp = hexstr2bin("4c1a03424b55e07fe7f27be1d58bb9324a9a5a04"),
    Case5Exp96 = hexstr2bin("4c1a03424b55e07fe7f27be1"),

    ?line Case5Mac_1 = crypto:sha_mac(Case5Key, Case5Data),
    ?line Case5Mac_2 = crypto:hmac(sha, Case5Key, Case5Data),
    ?line Case5Mac96_1 = crypto:sha_mac_96(Case5Key, Case5Data),
    ?line Case5Mac96_2 = crypto:hmac(sha, Case5Key, Case5Data, 12),
    ?line m(Case5Exp, Case5Mac_1),
    ?line m(Case5Exp, Case5Mac_2),
    ?line m(Case5Exp96, Case5Mac96_1),
    ?line m(Case5Exp96, Case5Mac96_2),

    %% Test case 6
    Case6Key = binary:copy(<<16#aa>>, 80),
    Case6Data = <<"Test Using Larger Than Block-Size Key - Hash Key First">>,
    Case6Exp = hexstr2bin("aa4ae5e15272d00e95705637ce8a3b55ed402112"),

    ?line Case6Mac_1 = crypto:sha_mac(Case6Key, Case6Data),
    ?line Case6Mac_2 = crypto:hmac(sha, Case6Key, Case6Data),
    ?line m(Case6Exp, Case6Mac_1),
    ?line m(Case6Exp, Case6Mac_2),

    %% Test case 7
    Case7Key = binary:copy(<<16#aa>>, 80),
    Case7Data = <<"Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data">>,
    Case7Exp = hexstr2bin("e8e99d0f45237d786d6bbaa7965c7808bbff1a91"),

    ?line Case7Mac_1 = crypto:sha_mac(Case7Key, Case7Data),
    ?line Case7Mac_2 = crypto:hmac(sha, Case7Key, Case7Data),
    ?line m(Case7Exp, Case7Mac_1),
    ?line m(Case7Exp, Case7Mac_2).

hmac_rfc4231_sha224(doc) ->
    ["Generate an HMAC using crypto:sha224_mac, hmac, and hmac_init, hmac_update, and hmac_final. "
     "Testvectors are take from RFC4231." ];
hmac_rfc4231_sha224(suite) ->
    [];
hmac_rfc4231_sha224(Config) when is_list(Config) ->
    if_supported(sha224, fun() -> hmac_rfc4231_sha224_do() end).

hmac_rfc4231_sha256(doc) ->
    ["Generate an HMAC using crypto:sha256_mac, hmac, and hmac_init, hmac_update, and hmac_final. "
     "Testvectors are take from RFC4231." ];
hmac_rfc4231_sha256(suite) ->
    [];
hmac_rfc4231_sha256(Config) when is_list(Config) ->
    if_supported(sha256, fun() -> hmac_rfc4231_sha256_do() end).

hmac_rfc4231_sha384(doc) ->
    ["Generate an HMAC using crypto:sha384_mac, hmac, and hmac_init, hmac_update, and hmac_final. "
     "Testvectors are take from RFC4231." ];
hmac_rfc4231_sha384(suite) ->
    [];
hmac_rfc4231_sha384(Config) when is_list(Config) ->
    if_supported(sha384, fun() -> hmac_rfc4231_sha384_do() end).

hmac_rfc4231_sha512(doc) ->
    ["Generate an HMAC using crypto:sha512_mac, hmac, and hmac_init, hmac_update, and hmac_final. "
     "Testvectors are take from RFC4231." ];
hmac_rfc4231_sha512(suite) ->
    [];
hmac_rfc4231_sha512(Config) when is_list(Config) ->
    if_supported(sha512, fun() -> hmac_rfc4231_sha512_do() end).

hmac_rfc4231_case(Hash, case1, Exp) ->
    %% Test 1
    Key = binary:copy(<<16#0b>>, 20),
    Data = <<"Hi There">>,
    hmac_rfc4231_case(Hash, Key, Data, Exp);

hmac_rfc4231_case(Hash, case2, Exp) ->
    %% Test 2
    Key = <<"Jefe">>,
    Data = <<"what do ya want for nothing?">>,
    hmac_rfc4231_case(Hash, Key, Data, Exp);

hmac_rfc4231_case(Hash, case3, Exp) ->
    %% Test 3
    Key = binary:copy(<<16#aa>>, 20),
    Data = binary:copy(<<16#dd>>, 50),
    hmac_rfc4231_case(Hash, Key, Data, Exp);

hmac_rfc4231_case(Hash, case4, Exp) ->
    %% Test 4
    Key = list_to_binary(lists:seq(1, 16#19)),
    Data = binary:copy(<<16#cd>>, 50),
    hmac_rfc4231_case(Hash, Key, Data, Exp);

hmac_rfc4231_case(Hash, case5, Exp) ->
    %% Test 5
    Key = binary:copy(<<16#0c>>, 20),
    Data = <<"Test With Truncation">>,
    hmac_rfc4231_case(Hash, Key, Data, 16, Exp);

hmac_rfc4231_case(Hash, case6, Exp) ->
    %% Test 6
    Key = binary:copy(<<16#aa>>, 131),
    Data = <<"Test Using Larger Than Block-Size Key - Hash Key First">>,
    hmac_rfc4231_case(Hash, Key, Data, Exp);

hmac_rfc4231_case(Hash, case7, Exp) ->
    %% Test Case 7
    Key = binary:copy(<<16#aa>>, 131),
    Data = <<"This is a test using a larger than block-size key and a larger t",
	     "han block-size data. The key needs to be hashed before being use",
	     "d by the HMAC algorithm.">>,
    hmac_rfc4231_case(Hash, Key, Data, Exp).

hmac_rfc4231_case(Hash, Key, Data, Exp) ->
    ?line Ctx = crypto:hmac_init(Hash, Key),
    ?line Ctx2 = crypto:hmac_update(Ctx, Data),
    ?line Mac1 = crypto:hmac_final(Ctx2),
    ?line Mac3 = crypto:hmac(Hash, Key, Data),
    ?line m(Exp, Mac1),
    ?line m(Exp, Mac3).

hmac_rfc4231_case(Hash, Key, Data, Trunc, Exp) ->
    ?line Ctx = crypto:hmac_init(Hash, Key),
    ?line Ctx2 = crypto:hmac_update(Ctx, Data),
    ?line Mac1 = crypto:hmac_final_n(Ctx2, Trunc),
    ?line Mac3 = crypto:hmac(Hash, Key, Data, Trunc),
    ?line m(Exp, Mac1),
    ?line m(Exp, Mac3).

hmac_rfc4231_sha224_do() ->
    Case1 = hexstr2bin("896fb1128abbdf196832107cd49df33f"
		       "47b4b1169912ba4f53684b22"),
    Case2 = hexstr2bin("a30e01098bc6dbbf45690f3a7e9e6d0f"
		       "8bbea2a39e6148008fd05e44"),
    Case3 = hexstr2bin("7fb3cb3588c6c1f6ffa9694d7d6ad264"
		       "9365b0c1f65d69d1ec8333ea"),
    Case4 = hexstr2bin("6c11506874013cac6a2abc1bb382627c"
		       "ec6a90d86efc012de7afec5a"),
    Case5 = hexstr2bin("0e2aea68a90c8d37c988bcdb9fca6fa8"),
    Case6 = hexstr2bin("95e9a0db962095adaebe9b2d6f0dbce2"
		       "d499f112f2d2b7273fa6870e"),
    Case7 = hexstr2bin("3a854166ac5d9f023f54d517d0b39dbd"
		       "946770db9c2b95c9f6f565d1"),
    hmac_rfc4231_cases_do(sha224, [Case1, Case2, Case3, Case4, Case5, Case6, Case7]).

hmac_rfc4231_sha256_do() ->
    Case1 = hexstr2bin("b0344c61d8db38535ca8afceaf0bf12b"
		       "881dc200c9833da726e9376c2e32cff7"),
    Case2 = hexstr2bin("5bdcc146bf60754e6a042426089575c7"
		       "5a003f089d2739839dec58b964ec3843"),
    Case3 = hexstr2bin("773ea91e36800e46854db8ebd09181a7"
		       "2959098b3ef8c122d9635514ced565fe"),
    Case4 = hexstr2bin("82558a389a443c0ea4cc819899f2083a"
		       "85f0faa3e578f8077a2e3ff46729665b"),
    Case5 = hexstr2bin("a3b6167473100ee06e0c796c2955552b"),
    Case6 = hexstr2bin("60e431591ee0b67f0d8a26aacbf5b77f"
		       "8e0bc6213728c5140546040f0ee37f54"),
    Case7 = hexstr2bin("9b09ffa71b942fcb27635fbcd5b0e944"
		       "bfdc63644f0713938a7f51535c3a35e2"),
    hmac_rfc4231_cases_do(sha256, [Case1, Case2, Case3, Case4, Case5, Case6, Case7]).

hmac_rfc4231_sha384_do() ->
    Case1 = hexstr2bin("afd03944d84895626b0825f4ab46907f"
		       "15f9dadbe4101ec682aa034c7cebc59c"
		       "faea9ea9076ede7f4af152e8b2fa9cb6"),
    Case2 = hexstr2bin("af45d2e376484031617f78d2b58a6b1b"
		       "9c7ef464f5a01b47e42ec3736322445e"
		       "8e2240ca5e69e2c78b3239ecfab21649"),
    Case3 = hexstr2bin("88062608d3e6ad8a0aa2ace014c8a86f"
		       "0aa635d947ac9febe83ef4e55966144b"
		       "2a5ab39dc13814b94e3ab6e101a34f27"),
    Case4 = hexstr2bin("3e8a69b7783c25851933ab6290af6ca7"
		       "7a9981480850009cc5577c6e1f573b4e"
		       "6801dd23c4a7d679ccf8a386c674cffb"),
    Case5 = hexstr2bin("3abf34c3503b2a23a46efc619baef897"),
    Case6 = hexstr2bin("4ece084485813e9088d2c63a041bc5b4"
		       "4f9ef1012a2b588f3cd11f05033ac4c6"
		       "0c2ef6ab4030fe8296248df163f44952"),
    Case7 = hexstr2bin("6617178e941f020d351e2f254e8fd32c"
		       "602420feb0b8fb9adccebb82461e99c5"
		       "a678cc31e799176d3860e6110c46523e"),
    hmac_rfc4231_cases_do(sha384, [Case1, Case2, Case3, Case4, Case5, Case6, Case7]).

hmac_rfc4231_sha512_do() ->
    Case1 = hexstr2bin("87aa7cdea5ef619d4ff0b4241a1d6cb0"
		       "2379f4e2ce4ec2787ad0b30545e17cde"
		       "daa833b7d6b8a702038b274eaea3f4e4"
		       "be9d914eeb61f1702e696c203a126854"),
    Case2 = hexstr2bin("164b7a7bfcf819e2e395fbe73b56e0a3"
		       "87bd64222e831fd610270cd7ea250554"
		       "9758bf75c05a994a6d034f65f8f0e6fd"
		       "caeab1a34d4a6b4b636e070a38bce737"),
    Case3 = hexstr2bin("fa73b0089d56a284efb0f0756c890be9"
		       "b1b5dbdd8ee81a3655f83e33b2279d39"
		       "bf3e848279a722c806b485a47e67c807"
		       "b946a337bee8942674278859e13292fb"),
    Case4 = hexstr2bin("b0ba465637458c6990e5a8c5f61d4af7"
		       "e576d97ff94b872de76f8050361ee3db"
		       "a91ca5c11aa25eb4d679275cc5788063"
		       "a5f19741120c4f2de2adebeb10a298dd"),
    Case5 = hexstr2bin("415fad6271580a531d4179bc891d87a6"),
    Case6 = hexstr2bin("80b24263c7c1a3ebb71493c1dd7be8b4"
		       "9b46d1f41b4aeec1121b013783f8f352"
		       "6b56d037e05f2598bd0fd2215d6a1e52"
		       "95e64f73f63f0aec8b915a985d786598"),
    Case7 = hexstr2bin("e37b6a775dc87dbaa4dfa9f96e5e3ffd"
		       "debd71f8867289865df5a32d20cdc944"
		       "b6022cac3c4982b10d5eeb55c3e4de15"
		       "134676fb6de0446065c97440fa8c6a58"),
    hmac_rfc4231_cases_do(sha512, [Case1, Case2, Case3, Case4, Case5, Case6, Case7]).

hmac_rfc4231_cases_do(Hash, CasesData) ->
    hmac_rfc4231_cases_do(Hash, [case1, case2, case3, case4, case5, case6, case7], CasesData).

hmac_rfc4231_cases_do(_Hash, _, []) ->
    ok;
hmac_rfc4231_cases_do(Hash, [C|Cases], [D|CasesData]) ->
    hmac_rfc4231_case(Hash, C, D),
    hmac_rfc4231_cases_do(Hash, Cases, CasesData).

hmac_update_md5_io(doc) ->
    ["Generate an MD5 HMAC using hmac_init, hmac_update, and hmac_final. "
     "Expected values for examples are generated using crypto:md5_mac." ];
hmac_update_md5_io(suite) ->
    [];
hmac_update_md5_io(Config) when is_list(Config) ->
    ?line Key = ["A fine speach", "by a fine man!"],
    ?line Data = "Sampl",
    ?line Data2 = "e #1",
    ?line Ctx = crypto:hmac_init(md5, Key),
    ?line Ctx2 = crypto:hmac_update(Ctx, Data),
    ?line Ctx3 = crypto:hmac_update(Ctx2, Data2),
    ?line Mac = crypto:hmac_final(Ctx3),
    ?line Exp = crypto:md5_mac(Key, lists:flatten([Data, Data2])), 
    ?line m(Exp, Mac).
    

hmac_update_md5_n(doc) ->
    ["Generate a shortened MD5 HMAC using hmac_init, hmac_update, and hmac_final. "
     "Expected values for examples are generated using crypto:md5_mac." ];
hmac_update_md5_n(suite) ->
    [];
hmac_update_md5_n(Config) when is_list(Config) ->
    ?line Key = ["A fine speach", "by a fine man!"],
    ?line Data = "Sampl",
    ?line Data2 = "e #1",
    ?line Ctx = crypto:hmac_init(md5, Key),
    ?line Ctx2 = crypto:hmac_update(Ctx, Data),
    ?line Ctx3 = crypto:hmac_update(Ctx2, Data2),
    ?line Mac = crypto:hmac_final_n(Ctx3, 12),
    ?line Exp = crypto:md5_mac_96(Key, lists:flatten([Data, Data2])), 
    ?line m(Exp, Mac).
%%
%%
ripemd160(doc) ->
    ["Generate RIPEMD160 message digests and check the result."];
ripemd160(suite) ->
    [];
ripemd160(Config) when is_list(Config) ->
    ?line m(crypto:hash(ripemd160,"abc"),
	    hexstr2bin("8EB208F7E05D987A9B044A8E98C6B087F15A0BFC")),
    ?line m(crypto:hash(ripemd160,"abcdbcdecdefdefgefghfghighijhijkijkljklmklm"
			  "nlmnomnopnopq"), 
	    hexstr2bin("12A053384A9C0C88E405A06C27DCF49ADA62EB2B")).

   
%%
%%
ripemd160_update(doc) ->
    ["Generate RIPEMD160 message digests by using ripemd160_init,"
     "ripemd160_update, and ripemd160_final and check the result."];
ripemd160_update(suite) ->
    [];
ripemd160_update(Config) when is_list(Config) ->
    ?line Ctx = crypto:hash_init(ripemd160),
    ?line Ctx1 = crypto:hash_update(Ctx, "abcdbcdecdefdefgefghfghighi"),
    ?line Ctx2 = crypto:hash_update(Ctx1, "jhijkijkljklmklmnlmnomnopnopq"),
    ?line m(crypto:hash_final(Ctx2), 
      hexstr2bin("12A053384A9C0C88E405A06C27DCF49ADA62EB2B")).

%%
%%
sha_update(doc) ->
    ["Generate SHA message digests by using sha_init, sha_update, and"
     "sha_final, and check the result. Examples are from FIPS-180-1."];
sha_update(suite) ->
    [];
sha_update(Config) when is_list(Config) ->
    ?line Ctx = crypto:sha_init(),
    ?line Ctx1 = crypto:sha_update(Ctx, "abcdbcdecdefdefgefghfghighi"),
    ?line Ctx2 = crypto:sha_update(Ctx1, "jhijkijkljklmklmnlmnomnopnopq"),
    ?line m(crypto:sha_final(Ctx2), 
	    hexstr2bin("84983E441C3BD26EBAAE4AA1F95129E5E54670F1")).

%%
%%
sha256(doc) ->
    ["Generate SHA-256 message digests and check the result. Examples are "
     "from rfc-4634."];
sha256(suite) ->
    [];
sha256(Config) when is_list(Config) ->
    if_supported(sha256, fun() -> sha256_do() end).

sha256_do() ->
    ?line m(crypto:hash(sha256, "abc"),
	    hexstr2bin("BA7816BF8F01CFEA4141"
		       "40DE5DAE2223B00361A396177A9CB410FF61F20015AD")),
    ?line m(crypto:hash(sha256, "abcdbcdecdefdefgefghfghighijhijkijkljklmklm"
			  "nlmnomnopnopq"), 
	    hexstr2bin("248D6A61D20638B8"
		       "E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1")).

%%
%%
sha256_update(doc) ->
    ["Generate SHA256 message digests by using sha256_init, sha256_update, and"
     "sha256_final, and check the result. Examples are from rfc-4634."];
sha256_update(suite) ->
    [];
sha256_update(Config) when is_list(Config) ->
    if_supported(sha256, fun() -> sha256_update_do() end).

sha256_update_do() ->
    ?line Ctx = crypto:hash_init(sha256),
    ?line Ctx1 = crypto:hash_update(Ctx, "abcdbcdecdefdefgefghfghighi"),
    ?line Ctx2 = crypto:hash_update(Ctx1, "jhijkijkljklmklmnlmnomnopnopq"),
    ?line m(crypto:hash_final(Ctx2),
	    hexstr2bin("248D6A61D20638B8"
		       "E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1")).


%%
%%
sha512(doc) ->
    ["Generate SHA-512 message digests and check the result. Examples are "
     "from rfc-4634."];
sha512(suite) ->
    [];
sha512(Config) when is_list(Config) ->
    if_supported(sha512, fun() -> sha512_do() end).

sha512_do() ->
    ?line m(crypto:hash(sha512, "abc"),
	    hexstr2bin("DDAF35A193617ABACC417349AE20413112E6FA4E89A97EA2"
		       "0A9EEEE64B55D39A2192992A274FC1A836BA3C23A3FEEBBD"
		       "454D4423643CE80E2A9AC94FA54CA49F")),
    ?line m(crypto:hash(sha512, "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"
			  "hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"), 
	    hexstr2bin("8E959B75DAE313DA8CF4F72814FC143F8F7779C6EB9F7FA1"
		       "7299AEADB6889018501D289E4900F7E4331B99DEC4B5433A"
		       "C7D329EEB6DD26545E96E55B874BE909")).

%%
%%
sha512_update(doc) ->
    ["Generate SHA512 message digests by using sha512_init, sha512_update, and"
     "sha512_final, and check the result. Examples are from rfc=4634."];
sha512_update(suite) ->
    [];
sha512_update(Config) when is_list(Config) ->
    if_supported(sha512, fun() -> sha512_update_do() end).

sha512_update_do() ->
    ?line Ctx = crypto:hash_init(sha512),
    ?line Ctx1 = crypto:hash_update(Ctx, "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"),
    ?line Ctx2 = crypto:hash_update(Ctx1, "hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"),
    ?line m(crypto:hash_final(Ctx2),
	    hexstr2bin("8E959B75DAE313DA8CF4F72814FC143F8F7779C6EB9F7FA1"
		       "7299AEADB6889018501D289E4900F7E4331B99DEC4B5433A"
		       "C7D329EEB6DD26545E96E55B874BE909")).

%%
%%
md5_mac(doc) ->
    ["Generate some HMACs, using MD5, and check the result. Examples are "
     "from RFC-2104."];
md5_mac(suite) ->
    [];
md5_mac(Config) when is_list(Config) ->
    ?line m(crypto:md5_mac(hexstr2bin("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"),
			   "Hi There"),
	    hexstr2bin("9294727a3638bb1c13f48ef8158bfc9d")),
    ?line m(crypto:md5_mac(list_to_binary("Jefe"), 
				     "what do ya want for nothing?"),
	    hexstr2bin("750c783e6ab0b503eaa86e310a5db738")),
    ?line m(crypto:md5_mac(hexstr2bin("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"),
			   hexstr2bin("DDDDDDDDDDDDDDDDDDDD"
				      "DDDDDDDDDDDDDDDDDDDD"
				      "DDDDDDDDDDDDDDDDDDDD"
				      "DDDDDDDDDDDDDDDDDDDD"
				      "DDDDDDDDDDDDDDDDDDDD")),
	    hexstr2bin("56be34521d144c88dbb8c733f0e8b3f6")).

%%
%%
md5_mac_io(doc) ->
    ["Generate some HMACs, using MD5, with Key an IO-list, and check the "
     "result. Examples are from RFC-2104."];
md5_mac_io(suite) ->
    [];
md5_mac_io(Config) when is_list(Config) ->
    ?line Key1 = hexstr2bin("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"),
    ?line {B11, B12} = split_binary(Key1, 4),
    ?line Key11 = [B11,binary_to_list(B12)],
    ?line m(crypto:md5_mac(Key11, "Hi There"),
	    hexstr2bin("9294727a3638bb1c13f48ef8158bfc9d")).

%%
%%
des_cbc(doc) ->
    "Encrypt and decrypt according to CBC DES. and check the result. "
	"Example are from FIPS-81.";
des_cbc(suite) ->
    [];
des_cbc(Config) when is_list(Config) ->
    ?line Key =  hexstr2bin("0123456789abcdef"),
    ?line IVec = hexstr2bin("1234567890abcdef"),
    ?line Plain = "Now is the time for all ",
    ?line Cipher = crypto:des_cbc_encrypt(Key, IVec, Plain),
    ?line m(Cipher, hexstr2bin("e5c7cdde872bf27c43e934008c389c"
			       "0f683788499a7c05f6")),
    ?line m(list_to_binary(Plain), 
	    crypto:des_cbc_decrypt(Key, IVec, Cipher)),
    ?line Plain2 = "7654321 Now is the time for " ++ [0, 0, 0, 0],
    ?line Cipher2 = crypto:des_cbc_encrypt(Key, IVec, Plain2),
    ?line m(Cipher2, hexstr2bin("b9916b8ee4c3da64b4f44e3cbefb9"
				"9484521388fa59ae67d58d2e77e86062733")),
    ?line m(list_to_binary(Plain2), 
	    crypto:des_cbc_decrypt(Key, IVec, Cipher2)).

%%
%%
des_cbc_iter(doc) ->
        "Encrypt and decrypt according to CBC DES in two steps, and "
    "check the result. Example are from FIPS-81.";
des_cbc_iter(suite) ->
    [];
des_cbc_iter(Config) when is_list(Config) ->
    ?line Key =  hexstr2bin("0123456789abcdef"),
    ?line IVec = hexstr2bin("1234567890abcdef"),
    ?line Plain1 = "Now is the time ",
    ?line Plain2 = "for all ",
    ?line Cipher1 = crypto:des_cbc_encrypt(Key, IVec, Plain1),
    ?line IVec2 = crypto:des_cbc_ivec(Cipher1),
    ?line Cipher2 = crypto:des_cbc_encrypt(Key, IVec2, Plain2),
    ?line Cipher = list_to_binary([Cipher1, Cipher2]),
    ?line m(Cipher, hexstr2bin("e5c7cdde872bf27c43e934008c389c"
			             "0f683788499a7c05f6")).

%%
%%
des_cfb(doc) ->
    "Encrypt and decrypt according to CFB DES. and check the result. "
	"Example is from FIPS-81.";
des_cfb(suite) ->
    [];
des_cfb(Config) when is_list(Config) ->
    ?line Key =  hexstr2bin("0123456789abcdef"),
    ?line IVec = hexstr2bin("1234567890abcdef"),
    ?line Plain = "Now is the",
    ?line Cipher = crypto:des_cfb_encrypt(Key, IVec, Plain),
    ?line m(Cipher, hexstr2bin("f31fda07011462ee187f")),
    ?line m(list_to_binary(Plain),
	    crypto:des_cfb_decrypt(Key, IVec, Cipher)).

%%
%%
des_cfb_iter(doc) ->
        "Encrypt and decrypt according to CFB DES in two steps, and "
    "check the result. Example is from FIPS-81.";
des_cfb_iter(suite) ->
    [];
des_cfb_iter(Config) when is_list(Config) ->
    ?line Key =  hexstr2bin("0123456789abcdef"),
    ?line IVec = hexstr2bin("1234567890abcdef"),
    ?line Plain1 = "Now i",
    ?line Plain2 = "s the",
    ?line Cipher1 = crypto:des_cfb_encrypt(Key, IVec, Plain1),
    ?line IVec2 = crypto:des_cfb_ivec(IVec, Cipher1),
    ?line Cipher2 = crypto:des_cfb_encrypt(Key, IVec2, Plain2),
    ?line Cipher = list_to_binary([Cipher1, Cipher2]),
    ?line m(Cipher, hexstr2bin("f31fda07011462ee187f")).

%%
%%
des_ecb(doc) ->
    "Encrypt and decrypt according to ECB DES and check the result. "
    "Example are from FIPS-81.";
des_ecb(suite) ->
    [];
des_ecb(Config) when is_list(Config) ->
    ?line Key =  hexstr2bin("0123456789abcdef"),
    ?line Cipher1 = crypto:des_ecb_encrypt(Key, "Now is t"),
    ?line m(Cipher1, hexstr2bin("3fa40e8a984d4815")),
    ?line Cipher2 = crypto:des_ecb_encrypt(Key, "he time "),
    ?line m(Cipher2, hexstr2bin("6a271787ab8883f9")),
    ?line Cipher3 = crypto:des_ecb_encrypt(Key, "for all "),
    ?line m(Cipher3, hexstr2bin("893d51ec4b563b53")),
    ?line Cipher4 = crypto:des_ecb_decrypt(Key, hexstr2bin("3fa40e8a984d4815")),
    ?line m(Cipher4, <<"Now is t">>),
    ?line Cipher5 = crypto:des_ecb_decrypt(Key, hexstr2bin("6a271787ab8883f9")),
    ?line m(Cipher5, <<"he time ">>),
    ?line Cipher6 = crypto:des_ecb_decrypt(Key, hexstr2bin("893d51ec4b563b53")),
    ?line m(Cipher6, <<"for all ">>).
%%
%%
rc2_cbc(doc) ->
    "Encrypt and decrypt according to RC2 CBC and check the result. "
    "Example stripped out from public_key application test";
rc2_cbc(Config) when is_list(Config) ->
   
    Key = <<146,210,160,124,215,227,153,239,227,17,222,140,3,93,27,191>>,
    IV = <<72,91,135,182,25,42,35,210>>,

    Cipher = <<36,245,206,158,168,230,58,69,148,137,32,192,250,41,237,181,181,251, 192,2,175,135,177,171,57,30,111,117,159,149,15,28,88,158,28,81,28,115, 85,219,241,82,117,222,91,85,73,117,164,25,182,52,191,64,123,57,26,19, 211,27,253,31,194,219,231,104,247,240,172,130,119,21,225,154,101,247, 32,216,42,216,133,169,78,22,97,27,227,26,196,224,172,168,17,9,148,55, 203,91,252,40,61,226,236,221,215,160,78,63,13,181,68,57,196,241,185, 207, 116,129,152,237,60,139,247,153,27,146,161,246,222,98,185,222,152, 187,135, 236,86,34,7,110,91,230,173,34,160,242,202,222,121,127,181,140, 101,203,195, 190,88,250,86,147,127,87,72,126,171,16,71,47,110,248,88, 14,29,143,161,152, 129,236,148,22,152,186,208,119,70,8,174,193,203,100, 193,203,200,117,102,242, 134,142,96,125,135,200,217,190,76,117,50,70, 209,186,101,241,200,91,40,193,54, 90,195,38,47,59,197,38,234,86,223,16, 51,253,204,129,20,171,66,21,241,26,135,216, 196,114,110,91,15,53,40, 164,201,136,113,95,247,51,181,208,241,68,168,98,151,36, 155,72,24,57, 42,191,14,125,204,10,167,214,233,138,115,125,234,121,134,227,26,247, 77,200,117,110,117,111,168,156,206,67,159,149,189,173,150,193,91,199, 216,153,22, 189,137,185,89,160,13,131,132,58,109,28,110,246,252,251,14, 232,91,38,52,29,101,188,69,123,50,0,130,178,93,73,239,118,7,77,35,59, 253,10,159,45,86,142,37,78,232,48>>,
    Text = <<48,130,1,85,2,1,0,48,13,6,9,42,134,72,134,247,13,1,1,1,5,0,4,130,1,63,48,130, 1,59,2,1,0,2,65,0,222,187,252,44,9,214,27,173,162,169,70,47,36,34,78,84,204, 107,60,192,117,95,21,206,49,142,245,126,121,223,23,2,107,106,133,204,161,36, 40,2,114,69,4,93,242,5,42,50,154,47,154,211,209,123,120,161,5,114,173,155,34, 191,52,59,2,3,1,0,1,2,64,45,144,169,106,220,236,71,39,67,82,123,192,35,21,61, 143,13,110,150,180,12,142,210,40,39,109,70,125,132,51,6,66,159,134,112,85, 155,243,118,221,65,133,127,99,151,194,252,141,149,224,229,62,214,45,228,32, 184,85,67,14,228,161,184,161,2,33,0,255,202,240,131,130,57,49,224,115,255,83, 79,6,165,212,21,179,212,20,188,97,74,69,68,163,223,247,237,39,24,23,235,2,33, 0,222,234,48,36,33,23,219,45,59,136,55,245,143,29,165,48,255,131,207,146,131, 104,13,163,54,131,236,78,88,54,16,241,2,33,0,230,2,99,129,173,176,166,131, 241,106,143,76,9,107,70,41,121,185,228,39,124,200,159,62,216,169,5,180,111, 169,255,159,2,33,0,151,193,70,212,209,210,179,219,175,83,165,4,255,81,103,76, 92,39,24,0,222,132,208,3,244,241,10,198,171,54,227,129,2,32,43,250,20,31,16, 189,168,116,225,1,125,132,94,130,118,124,28,56,232,39,69,218,244,33,240,200, 205,9,215,101,35,135,7,7,7,7,7,7,7>>,
    
    Text = crypto:rc2_cbc_decrypt(Key, IV, Cipher),
    Cipher = crypto:rc2_cbc_encrypt(Key, IV, Text).

%%
%%
des3_cbc(doc) ->
    "Encrypt and decrypt according to CBC 3DES, and check the result.";
des3_cbc(suite) ->
    [];
des3_cbc(Config) when is_list(Config) ->
    ?line Key1 = hexstr2bin("0123456789abcdef"),
    ?line Key2 = hexstr2bin("fedcba9876543210"),
    ?line Key3 = hexstr2bin("0f2d4b6987a5c3e1"),
    ?line IVec = hexstr2bin("1234567890abcdef"),
    ?line Plain = "Now is the time for all ",
    ?line Cipher = crypto:des3_cbc_encrypt(Key1, Key2, Key3, IVec, Plain),
    ?line m(Cipher, hexstr2bin("8a2667ee5577267cd9b1af2c5a0480"
			       "0bac1ae66970fb2b89")),
    ?line m(list_to_binary(Plain),
	    crypto:des3_cbc_decrypt(Key1, Key2, Key3, IVec, Cipher)),
    ?line Plain2 = "7654321 Now is the time for " ++ [0, 0, 0, 0],
    ?line Cipher2 = crypto:des3_cbc_encrypt(Key1, Key2, Key3, IVec, Plain2),
    ?line m(Cipher2, hexstr2bin("eb33ec6ede2c8e90f6877e77b95d5"
				"4c83cee22907f7f0041ca1b7abe202bfafe")),
    ?line m(list_to_binary(Plain2),
	    crypto:des3_cbc_decrypt(Key1, Key2, Key3, IVec, Cipher2)),

    ?line Key =  hexstr2bin("0123456789abcdef"),
    ?line DESCipher = crypto:des3_cbc_encrypt(Key, Key, Key, IVec, Plain),
    ?line m(DESCipher, hexstr2bin("e5c7cdde872bf27c43e934008c389c"
			       "0f683788499a7c05f6")),
    ?line m(list_to_binary(Plain),
	    crypto:des3_cbc_decrypt(Key, Key, Key, IVec, DESCipher)),
    ?line DESCipher2 = crypto:des3_cbc_encrypt(Key, Key, Key, IVec, Plain2),
    ?line m(DESCipher2, hexstr2bin("b9916b8ee4c3da64b4f44e3cbefb9"
				"9484521388fa59ae67d58d2e77e86062733")),
    ?line m(list_to_binary(Plain2),
	    crypto:des3_cbc_decrypt(Key, Key, Key, IVec, DESCipher2)).

%%
%%
des3_cfb(doc) ->
    "Encrypt and decrypt according to CFB 3DES, and check the result.";
des3_cfb(suite) ->
    [];
des3_cfb(Config) when is_list(Config) ->
    case openssl_version() of
	V when V < 16#90705F -> {skipped,"OpenSSL version too old"};
	_ -> des3_cfb_do()
    end.

des3_cfb_do() ->
    ?line Key1 = hexstr2bin("0123456789abcdef"),
    ?line Key2 = hexstr2bin("fedcba9876543210"),
    ?line Key3 = hexstr2bin("0f2d4b6987a5c3e1"),
    ?line IVec = hexstr2bin("1234567890abcdef"),
    ?line Plain = "Now is the time for all ",
    ?line Cipher = crypto:des3_cfb_encrypt(Key1, Key2, Key3, IVec, Plain),
    ?line m(Cipher, hexstr2bin("fc0ba7a20646ba53cc8bff263f0937"
			       "1deab42a00666db02c")),
    ?line m(list_to_binary(Plain),
	    crypto:des3_cfb_decrypt(Key1, Key2, Key3, IVec, Cipher)),
    ?line Plain2 = "7654321 Now is the time for " ++ [0, 0, 0, 0],
    ?line Cipher2 = crypto:des3_cfb_encrypt(Key1, Key2, Key3, IVec, Plain2),
    ?line m(Cipher2, hexstr2bin("8582c59ac01897422632c0accb66c"
				"e413f5efab838fce7e41e2ba67705bad5bc")),
    ?line m(list_to_binary(Plain2),
	    crypto:des3_cfb_decrypt(Key1, Key2, Key3, IVec, Cipher2)).

%%
%%
aes_cfb(doc) ->
    "Encrypt and decrypt according to AES CFB 128 bit and check "
	"the result. Example are from NIST SP 800-38A.";

aes_cfb(suite) ->
    [];
aes_cfb(Config) when is_list(Config) ->

%% Sample data from NIST Spec.Publ. 800-38A
%% F.3.13 CFB128-AES128.Encrypt
%% Key            2b7e151628aed2a6abf7158809cf4f3c
%% IV             000102030405060708090a0b0c0d0e0f
%% Segment #1
%% Input Block    000102030405060708090a0b0c0d0e0f
%% Output Block   50fe67cc996d32b6da0937e99bafec60
%% Plaintext      6bc1bee22e409f96e93d7e117393172a
%% Ciphertext     3b3fd92eb72dad20333449f8e83cfb4a
%% Segment #2
%% Input Block    3b3fd92eb72dad20333449f8e83cfb4a
%% Output Block   668bcf60beb005a35354a201dab36bda
%% Plaintext      ae2d8a571e03ac9c9eb76fac45af8e51
%% Ciphertext     c8a64537a0b3a93fcde3cdad9f1ce58b
%% Segment #3
%% Input Block    c8a64537a0b3a93fcde3cdad9f1ce58b
%% Output Block   16bd032100975551547b4de89daea630
%% Plaintext      30c81c46a35ce411e5fbc1191a0a52ef
%% Ciphertext     26751f67a3cbb140b1808cf187a4f4df
%% Segment #4
%% Input Block    26751f67a3cbb140b1808cf187a4f4df
%% Output Block   36d42170a312871947ef8714799bc5f6
%% Plaintext      f69f2445df4f9b17ad2b417be66c3710
%% Ciphertext     c04b05357c5d1c0eeac4c66f9ff7f2e6

    ?line Key =  hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"),
    ?line IVec = hexstr2bin("000102030405060708090a0b0c0d0e0f"),
    ?line Plain = hexstr2bin("6bc1bee22e409f96e93d7e117393172a"),
    ?line Cipher = hexstr2bin("3b3fd92eb72dad20333449f8e83cfb4a"),

    %% Try all prefixes of plain and cipher.
    aes_cfb_do(byte_size(Plain), Plain, Cipher, Key, IVec).

aes_cfb_do(N, Plain, Cipher, Key, IVec) when N >= 0 ->
    <<P:N/binary, _/binary>> = Plain,
    <<C:N/binary, _/binary>> = Cipher,
    ?line C = crypto:aes_cfb_128_encrypt(Key, IVec, P),
    ?line P = crypto:aes_cfb_128_decrypt(Key, IVec, C),
    aes_cfb_do(N-1, Plain, Cipher, Key, IVec);
aes_cfb_do(_, _, _, _, _) -> ok.


%%
%%
aes_cbc(doc) ->
    "Encrypt and decrypt according to AES CBC 128 bit. and check the result. "
	"Example are from NIST SP 800-38A.";

aes_cbc(suite) ->
    [];
aes_cbc(Config) when is_list(Config) ->

%% Sample data from NIST Spec.Publ. 800-38A
%% F.2.1 CBC-AES128.Encrypt
%% Key 2b7e151628aed2a6abf7158809cf4f3c 
%% IV 000102030405060708090a0b0c0d0e0f 
%% Block #1 
%% Plaintext 6bc1bee22e409f96e93d7e117393172a 
%% Input Block 6bc0bce12a459991e134741a7f9e1925 
%% Output Block 7649abac8119b246cee98e9b12e9197d 
%% Ciphertext 7649abac8119b246cee98e9b12e9197d 
%% Block #2 
%% Plaintext ae2d8a571e03ac9c9eb76fac45af8e51 
%% Input Block d86421fb9f1a1eda505ee1375746972c 
%% Output Block 5086cb9b507219ee95db113a917678b2 
%% Ciphertext 5086cb9b507219ee95db113a917678b2 
%% Block #3 
%% Plaintext 30c81c46a35ce411e5fbc1191a0a52ef 
%% Input Block 604ed7ddf32efdff7020d0238b7c2a5d 
%% Output Block 73bed6b8e3c1743b7116e69e22229516 
%% Ciphertext 73bed6b8e3c1743b7116e69e22229516 
%% Block #4 
%% Plaintext f69f2445df4f9b17ad2b417be66c3710 
%% Input Block 8521f2fd3c8eef2cdc3da7e5c44ea206 
%% Output Block 3ff1caa1681fac09120eca307586e1a7 
%% Ciphertext 3ff1caa1681fac09120eca307586e1a7 
%%
%% F.2.2 CBC-AES128.Decrypt 
%% Key 2b7e151628aed2a6abf7158809cf4f3c 
%% IV 000102030405060708090a0b0c0d0e0f 
    %% Block #1 
%% Ciphertext 7649abac8119b246cee98e9b12e9197d 
%% Input Block 7649abac8119b246cee98e9b12e9197d 
%% Output Block 6bc0bce12a459991e134741a7f9e1925 
%% Plaintext 6bc1bee22e409f96e93d7e117393172a 
%% Block #2 
%% Ciphertext 5086cb9b507219ee95db113a917678b2 
%% Input Block 5086cb9b507219ee95db113a917678b2 
%% Output Block d86421fb9f1a1eda505ee1375746972c 
%% Plaintext ae2d8a571e03ac9c9eb76fac45af8e51 
%% Block #3 
%% Ciphertext 73bed6b8e3c1743b7116e69e22229516 
%% Input Block 73bed6b8e3c1743b7116e69e22229516 
%% Output Block 604ed7ddf32efdff7020d0238b7c2a5d 
%% Plaintext 30c81c46a35ce411e5fbc1191a0a52ef 
%% Block #4 
%% Ciphertext 3ff1caa1681fac09120eca307586e1a7 
%% Input Block 3ff1caa1681fac09120eca307586e1a7
%% Output Block 8521f2fd3c8eef2cdc3da7e5c44ea206 
%% Plaintext f69f2445df4f9b17ad2b417be66c3710

    ?line Key =  hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"),
    ?line IVec = hexstr2bin("000102030405060708090a0b0c0d0e0f"),
    ?line Plain = hexstr2bin("6bc1bee22e409f96e93d7e117393172a"),
    ?line Cipher = crypto:aes_cbc_128_encrypt(Key, IVec, Plain),
    ?line m(Cipher, hexstr2bin("7649abac8119b246cee98e9b12e9197d")),
    ?line m(Plain, 
	    crypto:aes_cbc_128_decrypt(Key, IVec, Cipher)).

aes_cbc_iter(doc) ->
    "Encrypt and decrypt according to CBC AES in steps";
aes_cbc_iter(suite) -> [];
aes_cbc_iter(Config) when is_list(Config) ->
    Key = list_to_binary(lists:seq(255,256-16*17,-17)),
    IVec = list_to_binary(lists:seq(1,16*7,7)),
    Plain = <<"One, two, three o'clock, four o'clock, rock"
	     "Five, six, seven o'clock, eight o'clock, rock"
	     "Nine, ten, eleven o'clock, twelve o'clock, rock"
	     "We're gonna rock around the clock tonight">>,
    ?line 0 = size(Plain) rem 16,

    ?line Cipher = crypto:aes_cbc_128_encrypt(Key, IVec, Plain),
    ?line Plain = crypto:aes_cbc_128_decrypt(Key, IVec, Cipher),
    
    ?line Cipher = aes_cbc_encrypt_iter(Key,IVec,Plain,<<>>),
    ?line Plain = aes_cbc_decrypt_iter(Key,IVec,Cipher,<<>>),
    ok.

aes_cbc_encrypt_iter(_,_,<<>>, Acc) ->
    Acc;
aes_cbc_encrypt_iter(Key,IVec,Data, Acc) ->
    Bytes = 16 * (1 + size(Data) div (16*3)),
    <<Chunk:Bytes/binary, Rest/binary>> = Data,
    %%io:format("encrypt iter Chunk=~p Rest=~p\n",[Chunk,Rest]),
    ?line Cipher = crypto:aes_cbc_128_encrypt(Key, IVec, Chunk),
    ?line IVec2 = crypto:aes_cbc_ivec(Cipher),
    aes_cbc_encrypt_iter(Key,IVec2,Rest, <<Acc/binary, Cipher/binary>>).

aes_cbc_decrypt_iter(_,_,<<>>, Acc) ->
    Acc;
aes_cbc_decrypt_iter(Key,IVec,Data, Acc) ->
    Bytes = 16 * (1 + size(Data) div (16*5)),
    <<Chunk:Bytes/binary, Rest/binary>> = Data,
    %%io:format("decrypt iter Chunk=~p Rest=~p\n",[Chunk,Rest]),
    ?line Plain = crypto:aes_cbc_128_decrypt(Key, IVec, Chunk),
    ?line IVec2 = crypto:aes_cbc_ivec(Chunk),
    aes_cbc_decrypt_iter(Key,IVec2,Rest, <<Acc/binary, Plain/binary>>).


aes_ctr(doc) -> "CTR";
aes_ctr(Config) when is_list(Config) ->
    %% Sample data from NIST Spec.Publ. 800-38A
    %% F.5.1 CTR-AES128.Encrypt
    Key128 = hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"),
    Samples128 = [{"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff", % Input Block
                   "6bc1bee22e409f96e93d7e117393172a", % Plaintext
                   "874d6191b620e3261bef6864990db6ce"},% Ciphertext
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff00",
                   "ae2d8a571e03ac9c9eb76fac45af8e51",
                   "9806f66b7970fdff8617187bb9fffdff"},
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff01",
                   "30c81c46a35ce411e5fbc1191a0a52ef",
                   "5ae4df3edbd5d35e5b4f09020db03eab"},
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff02",
                   "f69f2445df4f9b17ad2b417be66c3710",
                   "1e031dda2fbe03d1792170a0f3009cee"}],
    lists:foreach(fun(S) -> aes_ctr_do(Key128,S) end, Samples128),

    %% F.5.3  CTR-AES192.Encrypt
    Key192 =  hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
    Samples192 = [{"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff", % Input Block
                   "6bc1bee22e409f96e93d7e117393172a", % Plaintext
                   "1abc932417521ca24f2b0459fe7e6e0b"},% Ciphertext
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff00",
                   "ae2d8a571e03ac9c9eb76fac45af8e51",
                   "090339ec0aa6faefd5ccc2c6f4ce8e94"},
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff01",
                   "30c81c46a35ce411e5fbc1191a0a52ef",
                   "1e36b26bd1ebc670d1bd1d665620abf7"},
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff02",
                   "f69f2445df4f9b17ad2b417be66c3710",
                   "4f78a7f6d29809585a97daec58c6b050"}],    
    lists:foreach(fun(S) -> aes_ctr_do(Key192,S) end, Samples192),

    %% F.5.5  CTR-AES256.Encrypt
    Key256 = hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
    Samples256 = [{"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",  % Input Block
                    "6bc1bee22e409f96e93d7e117393172a", % Plaintext
                    "601ec313775789a5b7a7f504bbf3d228"},% Ciphertext
                   {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff00",
                    "ae2d8a571e03ac9c9eb76fac45af8e51",
                    "f443e3ca4d62b59aca84e990cacaf5c5"},
                   {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff01",
                    "30c81c46a35ce411e5fbc1191a0a52ef",
                    "2b0930daa23de94ce87017ba2d84988d"},
                   {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff02",
                    "f69f2445df4f9b17ad2b417be66c3710",
                    "dfc9c58db67aada613c2dd08457941a6"}],
    lists:foreach(fun(S) -> aes_ctr_do(Key256,S) end, Samples256).


aes_ctr_do(Key,{IVec, Plain, Cipher}) ->
    ?line I = hexstr2bin(IVec),
    ?line P = hexstr2bin(Plain),
    ?line C = crypto:aes_ctr_encrypt(Key, I, P),
    ?line m(C, hexstr2bin(Cipher)),
    ?line m(P, crypto:aes_ctr_decrypt(Key, I, C)).

aes_ctr_stream(doc) -> "CTR Streaming";
aes_ctr_stream(Config) when is_list(Config) ->
    %% Sample data from NIST Spec.Publ. 800-38A
    %% F.5.1 CTR-AES128.Encrypt
    Key128 = hexstr2bin("2b7e151628aed2a6abf7158809cf4f3c"),
    Samples128 = [{"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",       % Input Block
                   ["6bc1bee22e409f", "96e93d7e117393172a"], % Plaintext
                   ["874d6191b620e3261bef6864990db6ce"]},    % Ciphertext
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff00",
                   ["ae2d8a57", "1e03ac9c", "9eb76fac", "45af8e51"],
                   ["9806f66b7970fdff","8617187bb9fffdff"]},
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff01",
                   ["30c81c46a35c", "e411e5fbc119", "1a0a52ef"],
                   ["5ae4df3e","dbd5d3","5e5b4f0902","0db03eab"]},
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff02",
                   ["f69f2445df4f9b17ad2b417be66c3710"],
                   ["1e031dda2fbe","03d1792170a0","f3009cee"]}],
    lists:foreach(fun(S) -> aes_ctr_stream_do(Key128,S) end, Samples128),

    %% F.5.3  CTR-AES192.Encrypt
    Key192 =  hexstr2bin("8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"),
    Samples192 = [{"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",   % Input Block
                   ["6bc1bee22e409f96e93d7e117393172a"], % Plaintext
                   ["1abc9324","17521c","a24f2b04","59fe7e6e0b"]},  % Ciphertext
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff00",
                   ["ae2d8a57", "1e03ac9c9eb76fac", "45af8e51"],
                   ["090339ec0aa6faefd5ccc2c6f4ce8e94"]},
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff01",
                   ["30c81c46a35ce411", "e5fbc1191a0a52ef"],
                   ["1e36b26bd1","ebc670d1bd1d","665620abf7"]},
                  {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff02",
                   ["f69f2445", "df4f9b17ad", "2b417be6", "6c3710"],
                   ["4f78a7f6d2980958","5a97daec58c6b050"]}],    
    lists:foreach(fun(S) -> aes_ctr_stream_do(Key192,S) end, Samples192),

    %% F.5.5  CTR-AES256.Encrypt
    Key256 = hexstr2bin("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"),
    Samples256 = [{"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",         % Input Block
                    ["6bc1bee22e409f96", "e93d7e117393172a"],  % Plaintext
                    ["601ec313775789", "a5b7a7f504bbf3d228"]}, % Ciphertext
                   {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff00",
                    ["ae2d8a571e03ac9c9eb76fac45af8e51"],
                    ["f443e3ca","4d62b59aca84","e990cacaf5c5"]},
                   {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff01",
                    ["30c81c46","a35ce411","e5fbc119","1a0a52ef"],
                    ["2b0930daa23de94ce87017ba2d84988d"]},
                   {"f0f1f2f3f4f5f6f7f8f9fafbfcfdff02",
                    ["f69f2445df4f","9b17ad2b41","7be66c3710"],
                    ["dfc9c5","8db67aada6","13c2dd08","457941a6"]}],
    lists:foreach(fun(S) -> aes_ctr_stream_do(Key256,S) end, Samples256).


aes_ctr_stream_do(Key,{IVec, PlainList, CipherList}) ->
    ?line I = hexstr2bin(IVec),
    ?line S = crypto:aes_ctr_stream_init(Key, I),
    ?line C = aes_ctr_stream_do_iter(
                S, PlainList, [], 
                fun(S2,P) -> crypto:aes_ctr_stream_encrypt(S2, P) end),
    ?line m(C, hexstr2bin(lists:flatten(CipherList))),
    ?line P = aes_ctr_stream_do_iter(
                S, CipherList, [], 
                fun(S2,C2) -> crypto:aes_ctr_stream_decrypt(S2, C2) end),
    ?line m(P, hexstr2bin(lists:flatten(PlainList))).

aes_ctr_stream_do_iter(_State, [], Acc, _CipherFun) ->
    iolist_to_binary(lists:reverse(Acc));
aes_ctr_stream_do_iter(State, [Plain|Rest], Acc, CipherFun) ->
    ?line P = hexstr2bin(Plain),
    ?line {S2, C} = CipherFun(State, P),
    aes_ctr_stream_do_iter(S2, Rest, [C | Acc], CipherFun).

%%
%%
mod_exp_test(doc) ->
    "mod_exp testing (A ^ M % P with bignums)";
mod_exp_test(suite) ->
    [];
mod_exp_test(Config) when is_list(Config) ->
    mod_exp_aux_test(2, 5, 10, 8).

mod_exp_aux_test(_, _, _, 0) ->
    ok;
mod_exp_aux_test(B, E, M, N) ->
    ?line R1 = crypto:mod_exp(B, E, M),
    ?line R2 = ipow(B, E, M),
    ?line m(R1, R2),
    ?line mod_exp_aux_test(B, E*E+1, M*M+1, N-1).

%%
%%
rand_uniform_test(doc) ->
    "rand_uniform and random_bytes testing";
rand_uniform_test(suite) ->
    [];
rand_uniform_test(Config) when is_list(Config) ->
    rand_uniform_aux_test(10),
    ?line 10 = size(crypto:rand_bytes(10)).

rand_uniform_aux_test(0) ->
    ok;
rand_uniform_aux_test(N) ->
    ?line L = N*1000,
    ?line H = N*100000+1,
    ?line crypto_rand_uniform(L, H),
    ?line crypto_rand_uniform(-L, L),
    ?line crypto_rand_uniform(-H, -L),
    ?line crypto_rand_uniform(-H, L),
    ?line rand_uniform_aux_test(N-1).

crypto_rand_uniform(L,H) ->
    ?line R1 = crypto:rand_uniform(L, H),
    ?line t(R1 >= L),
    ?line t(R1 < H).


%%
%%
strong_rand_test(doc) ->
    "strong_rand_mpint and strong_random_bytes testing";
strong_rand_test(suite) ->
    [];
strong_rand_test(Config) when is_list(Config) ->
    strong_rand_aux_test(180),
    ?line 10 = byte_size(crypto:strong_rand_bytes(10)).

strong_rand_aux_test(0) ->
    ?line t(crypto:strong_rand_mpint(0,0,0) =:= <<0,0,0,0>>),
    ok;
strong_rand_aux_test(1) ->
    ?line t(crypto:erlint(crypto:strong_rand_mpint(1,0,1)) =:= 1),
    ?line strong_rand_aux_test(0);
strong_rand_aux_test(N) ->
    ?line t(sru_length(crypto:strong_rand_mpint(N,-1,0)) =< N),
    ?line t(sru_length(crypto:strong_rand_mpint(N,0,0)) =:= N),
    ?line t(crypto:erlint(crypto:strong_rand_mpint(N,0,1)) band 1 =:= 1),
    ?line t(crypto:erlint(crypto:strong_rand_mpint(N,1,0)) bsr (N - 2) =:= 2#11),
    ?line strong_rand_aux_test(N-1).

sru_length(Mpint) ->
    I = crypto:erlint(Mpint),
    length(erlang:integer_to_list(I, 2)).

%%
%%
%%
%%
rsa_verify_test(doc) ->
    "rsa_verify testing (A ^ M % P with bignums)";
rsa_verify_test(suite) ->
    [];
rsa_verify_test(Config) when is_list(Config) ->
    ?line H = <<178,28,54,104,36,80,144,66,140,201,135,17,36,97,114,124,
	       194,164,172,147>>,
    ?line SigBlob = <<153,44,121,71,132,1,192,159,78,33,29,62,153,64,191,70,
		     208,239,166,208,220,167,49,111,128,67,91,253,24,63,194,241,
		     97,157,135,226,121,162,150,156,60,49,236,90,151,67,239,23,
		     92,103,89,254,17,165,78,181,64,128,13,210,86,111,209,76,
		     115,34,107,227,151,47,80,185,143,85,202,55,245,163,226,26,
		     139,104,196,6,96,82,108,197,13,0,12,70,153,109,107,180,
		     130,246,156,182,56,96,31,220,227,218,136,211,252,43,8,14,
		     145,155,191,206,72,194,80,52,54,206,53,27,6,188,195,29>>,
    ?line BadSigBlob = <<153,44,121,71,132,1,192,159,78,33,29,62,153,64,191,70,
			208,239,166,208,220,167,49,111,128,67,91,253,24,63,194,241,
			97,157,135,226,121,162,150,156,60,49,236,90,151,67,239,23,
			92,103,89,254,17,165,78,181,64,128,13,210,86,111,209,76,
			115,107,34,227,151,47,80,185,143,85,202,55,245,163,226,26,
			139,104,196,6,96,82,108,197,13,0,12,70,153,109,107,180,
			130,246,156,182,56,96,31,220,227,218,136,211,252,43,8,14,
			145,155,191,206,72,194,80,52,54,206,53,27,6,188,195,29>>,
    ?line E = <<35>>,
    ?line N = <<0,199,209,142,191,86,92,148,103,37,250,217,175,169,109,10,
	       130,139,34,237,174,90,97,118,7,185,57,137,252,236,177,193,
	       228,16,62,29,153,144,64,207,152,240,152,206,136,89,64,6,
	       3,187,89,57,241,219,88,215,75,70,120,20,145,229,37,1,
	       67,138,204,17,39,231,249,239,116,142,169,99,149,41,65,123,
	       26,225,133,0,41,85,77,181,35,100,162,223,92,220,207,50,
	       63,168,193,171,174,199,23,214,201,63,157,76,125,6,54,73,
	       76,89,40,33,147,208,189,76,98,24,61,8,10,110,165,119,165>>,
    ?line Nbad = <<0,199,209,142,191,86,92,148,103,37,250,217,175,169,109,10,
		  130,139,34,237,174,90,97,118,7,185,57,137,252,236,177,193,
		  228,16,62,29,153,144,64,207,152,240,152,206,136,89,64,6,
		  3,187,89,57,241,219,88,215,75,70,120,20,145,229,37,1,
		  67,138,204,17,39,231,249,239,116,142,169,99,149,41,65,123,
		  26,225,133,0,41,85,77,181,35,100,162,223,92,220,207,50,
		  63,168,193,171,174,199,23,214,201,63,157,76,125,6,54,73,
		  76,89,40,33,147,189,208,76,98,24,61,8,10,110,165,119,165>>,
    ?line Ebad = <<77>>,
    ?line m(crypto:rsa_verify(sized_binary(H), sized_binary(SigBlob),
			      [sized_binary(E), sized_binary(N)]), true),
    ?line m(crypto:rsa_verify(sized_binary(H), sized_binary(SigBlob),
			      [sized_binary(Ebad), sized_binary(N)]), false),
    ?line m(crypto:rsa_verify(sized_binary(H), sized_binary(SigBlob),
			      [sized_binary(E), sized_binary(Nbad)]), false),
    ?line m(crypto:rsa_verify(sized_binary(H), sized_binary(BadSigBlob),
			      [sized_binary(E), sized_binary(N)]), false).

%%
%%
dsa_verify_test(doc) ->
    "dsa_verify testing (A ^ M % P with bignums)";
dsa_verify_test(suite) ->
    [];
dsa_verify_test(Config) when is_list(Config) ->
    ?line Msg = <<48,130,2,245,160,3,2,1,2,2,1,1,48,9,6,7,42,134,72,206,56,4,3,48,
		 58,49,11,48,9,6,3,85,4,6,19,2,85,83,49,26,48,24,6,3,85,4,10,19,17,
		 84,101,115,116,32,67,101,114,116,105,102,105,99,97,116,101,115,49,
		 15,48,13,6,3,85,4,3,19,6,68,83,65,32,67,65,48,30,23,13,48,49,48,
		 52,49,57,49,52,53,55,50,48,90,23,13,49,49,48,52,49,57,49,52,53,55,
		 50,48,90,48,93,49,11,48,9,6,3,85,4,6,19,2,85,83,49,26,48,24,6,3,
		 85,4,10,19,17,84,101,115,116,32,67,101,114,116,105,102,105,99,97,
		 116,101,115,49,50,48,48,6,3,85,4,3,19,41,86,97,108,105,100,32,68,
		 83,65,32,83,105,103,110,97,116,117,114,101,115,32,69,69,32,67,101,
		 114,116,105,102,105,99,97,116,101,32,84,101,115,116,52,48,130,1,
		 182,48,130,1,43,6,7,42,134,72,206,56,4,1,48,130,1,30,2,129,129,0,
		 228,139,175,64,140,21,215,61,124,238,3,150,18,104,193,32,5,232,23,
		 202,158,116,101,75,154,84,151,42,120,51,218,165,197,114,234,52,
		 179,148,104,66,213,27,253,119,240,168,66,158,100,147,144,182,194,
		 2,49,70,19,122,3,105,204,152,45,86,157,94,35,95,40,191,173,127,15,
		 208,105,149,98,92,26,7,42,94,140,115,73,126,253,18,34,142,85,229,
		 86,233,174,114,41,150,135,8,39,215,119,67,240,134,184,9,10,27,20,
		 165,230,3,230,69,121,77,233,250,83,95,193,9,189,126,197,195,2,21,
		 0,128,63,228,252,243,76,229,62,203,15,23,10,42,84,108,208,103,108,
		 13,59,2,129,128,102,212,22,138,32,173,254,209,50,159,165,127,167,
		 179,208,234,119,63,235,108,162,228,41,216,216,188,33,221,154,247,
		 204,229,180,119,77,223,236,218,162,140,156,117,18,90,31,254,102,
		 211,17,194,239,132,67,236,169,136,110,76,186,76,63,53,150,199,103,
		 252,153,189,15,153,41,19,145,78,216,2,174,254,107,175,80,86,170,
		 47,30,181,42,200,238,34,71,37,120,107,33,221,20,63,206,240,16,129,
		 247,150,29,156,65,187,94,68,146,93,46,198,30,184,205,105,200,143,
		 63,59,62,208,79,162,206,217,3,129,132,0,2,129,128,15,83,40,172,56,
		 47,61,243,17,97,65,195,61,167,214,122,247,246,1,50,211,33,113,16,
		 20,213,195,62,77,235,25,162,140,175,158,8,61,65,10,255,204,162,71,
		 130,122,86,161,163,253,236,178,139,183,57,181,202,160,25,133,130,
		 155,150,104,168,187,107,186,144,164,225,173,101,182,68,49,210,30,
		 34,47,83,65,79,250,156,248,47,232,44,67,36,22,126,43,216,100,247,
		 100,250,240,121,72,29,185,2,109,144,54,204,235,54,15,242,57,171,
		 125,39,236,247,71,111,221,51,196,126,77,238,36,87,163,107,48,105,
		 48,29,6,3,85,29,14,4,22,4,20,179,51,215,81,162,4,13,68,251,157,64,
		 241,18,98,113,176,83,246,105,13,48,31,6,3,85,29,35,4,24,48,22,128,
		 20,116,21,213,36,28,189,94,101,136,31,225,139,9,126,127,234,25,72,
		 78,97,48,23,6,3,85,29,32,4,16,48,14,48,12,6,10,96,134,72,1,101,3,
		 2,1,48,1,48,14,6,3,85,29,15,1,1,255,4,4,3,2,6,192>>,

    ?line SigBlob = <<48,45,2,21,0,140,167,200,210,153,212,64,155,249,33,146,104,243,
		     39,38,9,115,162,89,24,2,20,76,254,31,128,187,48,128,215,216,
		     112,198,78,118,160,217,157,180,246,64,234>>,
    ?line P_p = 157224271412839155721795253728878055347359513988016145491388196653004661857517720927482198111104095793441029858267073789634147217022008635826863307553453131345099940951090826856271796188522037524757740796268675508118348391218066949174594918958269259937813776150149068811425194955973128428675945283593831134219,
    ?line Q_p = 1181895316321540581845959276009400765315408342791,
    ?line G_p = 143872196713149000950547166575757355261637863805587906227228163275557375159769599033632918292482002186641475268486598023281100659643528846513898847919251032731261718358900479488287933293278745715922865499005559197328388506945134386346185262919258658109015074718441639029135304654725637911172671711310801418648,
    
    ?line Key = 12603618348903387232593303690286336220738319446775939686476278478034365380027994899970214309288018488811754534229198764622077544117034174589418477472887827980332636062691833965078594576024299807057520016043084384987871640003684704483975314128362610573625803532737054022545217931847268776098203204571431581966,
    
    ValidKey = [crypto:mpint(P_p), 
		crypto:mpint(Q_p), 
		crypto:mpint(G_p),
		crypto:mpint(Key)
	       ],
    
    ?line m(my_dss_verify(sized_binary(Msg), sized_binary(SigBlob),
			      ValidKey), true),

    BadMsg  = one_bit_wrong(Msg),
    ?line m(my_dss_verify(sized_binary(BadMsg), sized_binary(SigBlob),
			      ValidKey), false),
    BadSig = one_bit_wrong(SigBlob),
    ?line m(my_dss_verify(sized_binary(Msg), sized_binary(BadSig),
			      ValidKey), false),
    SizeErr = size(SigBlob) - 13,
    
    BadArg = (catch my_dss_verify(sized_binary(Msg), <<SizeErr:32, SigBlob/binary>>,
				      ValidKey)),
    badarg = case element(1,element(2,BadArg)) of
		 badarg -> badarg;
		 function_clause -> badarg;
		 X -> X
	     end,
    InValidKey = [crypto:mpint(P_p), 
		  crypto:mpint(Q_p), 
		  crypto:mpint(G_p),
		  crypto:mpint(Key+17)
		 ],
    
    ?line m(my_dss_verify(sized_binary(Msg), sized_binary(SigBlob),
			      InValidKey), false).


one_bit_wrong(List) when is_list(List) ->
    lists:map(fun(Bin) -> one_bit_wrong(Bin) end, List);
one_bit_wrong(Bin) ->
    Half = size(Bin) div 2,
    <<First:Half/binary, Byte:8, Last/binary>> = Bin,
    <<First/binary, (Byte+1):8, Last/binary>>.


%%
%%  Sign tests

rsa_sign_test(doc) ->
    "rsa_sign testing";
rsa_sign_test(suite) ->
    [];
rsa_sign_test(Config) when is_list(Config) ->
    PubEx  = 65537,
    PrivEx = 7531712708607620783801185371644749935066152052780368689827275932079815492940396744378735701395659435842364793962992309884847527234216715366607660219930945,
    Mod = 7919488123861148172698919999061127847747888703039837999377650217570191053151807772962118671509138346758471459464133273114654252861270845708312601272799123,
    Msg = <<"7896345786348756234 Hejsan Svejsan, erlang crypto debugger"
	   "09812312908312378623487263487623412039812 huagasd">>,
    
    PrivKey = [PubEx, Mod, PrivEx],
    PubKey  = [PubEx, Mod],
    PubKeyMpint = map_int_to_mpint(PubKey),
    Sig1 = crypto:rsa_sign(sized_binary(Msg), map_int_to_mpint(PrivKey)),
    Sig1 = crypto:sign(rsa, sha, Msg, PrivKey),
    true = crypto:rsa_verify(sized_binary(Msg), sized_binary(Sig1), PubKeyMpint),
    true = crypto:verify(rsa, sha, Msg, Sig1, PubKey),
    
    Sig2 = crypto:rsa_sign(md5, sized_binary(Msg), map_int_to_mpint(PrivKey)),
    Sig2 = crypto:sign(rsa, md5, Msg, PrivKey),
    true = crypto:rsa_verify(md5, sized_binary(Msg), sized_binary(Sig2), PubKeyMpint),
    true = crypto:verify(rsa, md5, Msg, Sig2, PubKey),
    
    false = (Sig1 =:= Sig2),
    false = crypto:rsa_verify(md5, sized_binary(Msg), sized_binary(Sig1), PubKeyMpint),
    false = crypto:verify(rsa, md5, Msg, Sig1, PubKey),
    true = crypto:rsa_verify(sha, sized_binary(Msg), sized_binary(Sig1), PubKeyMpint),
    true = crypto:verify(rsa, sha, Msg, Sig1, PubKey),

    ok.
map_int_to_mpint(List) ->
    lists:map(fun(E) -> crypto:mpint(E) end, List).

rsa_sign_hash_test(doc) ->
    "rsa_sign_hash testing";
rsa_sign_hash_test(suite) ->
    [];
rsa_sign_hash_test(Config) when is_list(Config) ->
    PubEx  = 65537,
    PrivEx = 7531712708607620783801185371644749935066152052780368689827275932079815492940396744378735701395659435842364793962992309884847527234216715366607660219930945,
    Mod = 7919488123861148172698919999061127847747888703039837999377650217570191053151807772962118671509138346758471459464133273114654252861270845708312601272799123,
    Msg = <<"7896345786348756234 Hejsan Svejsan, erlang crypto debugger"
	   "09812312908312378623487263487623412039812 huagasd">>,

    PrivKey = [crypto:mpint(PubEx), crypto:mpint(Mod), crypto:mpint(PrivEx)],
    PubKey  = [crypto:mpint(PubEx), crypto:mpint(Mod)],
    MD5 = crypto:md5(sized_binary(Msg)),
    SHA = crypto:sha(sized_binary(Msg)),
    ?line Sig1 = crypto:rsa_sign(sha, {digest,SHA}, PrivKey),
    ?line m(crypto:rsa_verify(sha, {digest,SHA}, sized_binary(Sig1),PubKey), true),

    ?line Sig2 = crypto:rsa_sign(md5, {digest,MD5}, PrivKey),
    ?line m(crypto:rsa_verify(md5, {digest,MD5}, sized_binary(Sig2),PubKey), true),

    ?line m(Sig1 =:= Sig2, false),
    ?line m(crypto:rsa_verify(md5, {digest,MD5}, sized_binary(Sig1),PubKey), false),
    ?line m(crypto:rsa_verify(sha, {digest,SHA}, sized_binary(Sig2),PubKey), false),

    ok.

dsa_sign_test(doc) ->
    "dsa_sign testing";
dsa_sign_test(suite) ->
    [];
dsa_sign_test(Config) when is_list(Config) ->
    Msg = <<"7896345786348756234 Hejsan Svejsan, erlang crypto debugger"
	   "09812312908312378623487263487623412039812 huagasd">>,

    PubKey = _Y = 25854665488880835237281628794585130313500176551981812527054397586638455298000483144002221850980183404910190346416063318160497344811383498859129095184158800144312512447497510551471331451396405348497845813002058423110442376886564659959543650802132345311573634832461635601376738282831340827591903548964194832978,
    PrivKey = _X = 441502407453038284293378221372000880210588566361,
    ParamP = 109799869232806890760655301608454668257695818999841877165019612946154359052535682480084145133201304812979481136659521529774182959764860329095546511521488413513097576425638476458000255392402120367876345280670101492199681798674053929238558140260669578407351853803102625390950534052428162468100618240968893110797,
    ParamQ = 1349199015905534965792122312016505075413456283393,
    ParamG = 18320614775012672475365915366944922415598782131828709277168615511695849821411624805195787607930033958243224786899641459701930253094446221381818858674389863050420226114787005820357372837321561754462061849169568607689530279303056075793886577588606958623645901271866346406773590024901668622321064384483571751669,

    Params = [crypto:mpint(ParamP), crypto:mpint(ParamQ), crypto:mpint(ParamG)],
    ?line Sig1 = my_dss_sign(sized_binary(Msg), Params ++ [crypto:mpint(PrivKey)]),
    
    ?line m(my_dss_verify(sized_binary(Msg), Sig1, 
			      Params ++ [crypto:mpint(PubKey)]), true),
    
    ?line m(my_dss_verify(sized_binary(one_bit_wrong(Msg)), Sig1, 
			      Params ++ [crypto:mpint(PubKey)]), false),
    
    ?line m(my_dss_verify(sized_binary(Msg), one_bit_wrong(Sig1), 
			      Params ++ [crypto:mpint(PubKey)]), false),

    %%?line Bad = crypto:dss_sign(sized_binary(Msg), [Params, crypto:mpint(PubKey)]),
    
    ok.

dsa_sign_hash_test(doc) ->
    "dsa_sign_hash testing";
dsa_sign_hash_test(suite) ->
    [];
dsa_sign_hash_test(Config) when is_list(Config) ->
    Msg = <<"7896345786348756234 Hejsan Svejsan, erlang crypto debugger"
	   "09812312908312378623487263487623412039812 huagasd">>,
    SHA = crypto:sha(sized_binary(Msg)),

    PubKey = _Y = 25854665488880835237281628794585130313500176551981812527054397586638455298000483144002221850980183404910190346416063318160497344811383498859129095184158800144312512447497510551471331451396405348497845813002058423110442376886564659959543650802132345311573634832461635601376738282831340827591903548964194832978,
    PrivKey = _X = 441502407453038284293378221372000880210588566361,
    ParamP = 109799869232806890760655301608454668257695818999841877165019612946154359052535682480084145133201304812979481136659521529774182959764860329095546511521488413513097576425638476458000255392402120367876345280670101492199681798674053929238558140260669578407351853803102625390950534052428162468100618240968893110797,
    ParamQ = 1349199015905534965792122312016505075413456283393,
    ParamG = 18320614775012672475365915366944922415598782131828709277168615511695849821411624805195787607930033958243224786899641459701930253094446221381818858674389863050420226114787005820357372837321561754462061849169568607689530279303056075793886577588606958623645901271866346406773590024901668622321064384483571751669,

    Params = [crypto:mpint(ParamP), crypto:mpint(ParamQ), crypto:mpint(ParamG)],
    ?line Sig1 = crypto:dss_sign(sha, {digest,SHA}, Params ++ [crypto:mpint(PrivKey)]),

    ?line m(crypto:dss_verify(none, SHA, sized_binary(Sig1),
			      Params ++ [crypto:mpint(PubKey)]), true),

    ?line m(crypto:dss_verify(sized_binary(one_bit_wrong(Msg)), sized_binary(Sig1),
			      Params ++ [crypto:mpint(PubKey)]), false),

    ?line m(crypto:dss_verify(sized_binary(Msg), sized_binary(one_bit_wrong(Sig1)),
			      Params ++ [crypto:mpint(PubKey)]), false),

    %%?line Bad = crypto:dss_sign(sized_binary(Msg), [Params, crypto:mpint(PubKey)]),

    ok.


rsa_encrypt_decrypt(doc) ->
    ["Test rsa_public_encrypt and rsa_private_decrypt functions."];
rsa_encrypt_decrypt(suite) -> [];
rsa_encrypt_decrypt(Config) when is_list(Config) ->
    PubEx  = 65537,
    PrivEx = 7531712708607620783801185371644749935066152052780368689827275932079815492940396744378735701395659435842364793962992309884847527234216715366607660219930945,
    Mod = 7919488123861148172698919999061127847747888703039837999377650217570191053151807772962118671509138346758471459464133273114654252861270845708312601272799123,
    
    PrivKey = [PubEx, Mod, PrivEx],
    PubKey  = [PubEx, Mod],

    Msg = <<"7896345786348 Asldi">>,

    ?line PKCS1 = rsa_public_encrypt(Msg, PubKey, rsa_pkcs1_padding),
    ?line PKCS1Dec = rsa_private_decrypt(PKCS1, PrivKey, rsa_pkcs1_padding),
    io:format("PKCS1Dec ~p~n",[PKCS1Dec]),
    ?line Msg = PKCS1Dec,
    
    ?line OAEP = rsa_public_encrypt(Msg, PubKey, rsa_pkcs1_oaep_padding),
    ?line Msg = rsa_private_decrypt(OAEP, PrivKey, rsa_pkcs1_oaep_padding),

    <<Msg2Len:32,_/binary>> = crypto:mpint(Mod),
    Msg2 = list_to_binary(lists:duplicate(Msg2Len-1, $X)),
    ?line NoPad = rsa_public_encrypt(Msg2, PubKey, rsa_no_padding),
    ?line NoPadDec = rsa_private_decrypt(NoPad, PrivKey, rsa_no_padding),
    ?line NoPadDec = Msg2,
    
    ShouldBeError = (catch rsa_public_encrypt(Msg, PubKey, rsa_no_padding)),
    ?line {'EXIT', {encrypt_failed,_}} = ShouldBeError,
    
%%     ?line SSL = rsa_public_encrypt(Msg, PubKey, rsa_sslv23_padding),
%%     ?line Msg = rsa_private_decrypt(SSL, PrivKey, rsa_sslv23_padding),

    ?line PKCS1_2 = rsa_private_encrypt(Msg, PrivKey, rsa_pkcs1_padding),
    ?line PKCS1_2Dec = rsa_public_decrypt(PKCS1_2, PubKey, rsa_pkcs1_padding),
    io:format("PKCS2Dec ~p~n",[PKCS1_2Dec]),
    ?line Msg = PKCS1_2Dec,

    ?line PKCS1_3 = rsa_private_encrypt(Msg2, PrivKey, rsa_no_padding),
    ?line PKCS1_3Dec = rsa_public_decrypt(PKCS1_3, PubKey, rsa_no_padding),
    io:format("PKCS2Dec ~p~n",[PKCS1_3Dec]),
    ?line Msg2 = PKCS1_3Dec,
    
    ?line {'EXIT', {encrypt_failed,_}} = 
	(catch rsa_private_encrypt(Msg, PrivKey, rsa_no_padding)),
    
    ok.

rsa_public_encrypt(Msg, Key, Pad) ->
    C1 = crypto:rsa_public_encrypt(Msg, Key, Pad),
    C2 = crypto:rsa_public_encrypt(Msg, lists:map(fun(E) -> crypto:mpint(E) end, Key), Pad),
    {C1,C2}.

rsa_public_decrypt(Msg, Key, Pad) ->
    R = crypto:rsa_public_decrypt(Msg, Key, Pad),
    R = crypto:rsa_public_decrypt(Msg, lists:map(fun(E) -> crypto:mpint(E) end, Key), Pad).

rsa_private_encrypt(Msg, Key, Pad) ->
    R = crypto:rsa_private_encrypt(Msg, Key, Pad),
    R = crypto:rsa_private_encrypt(Msg, lists:map(fun(E) -> crypto:mpint(E) end, Key), Pad).

rsa_private_decrypt({C1,C2}, Key, Pad) ->
    R = crypto:rsa_private_decrypt(C1, Key, Pad),
    R = crypto:rsa_private_decrypt(C2, Key, Pad),
    R = crypto:rsa_private_decrypt(C1, lists:map(fun(E) -> crypto:mpint(E) end, Key), Pad),
    R = crypto:rsa_private_decrypt(C2, lists:map(fun(E) -> crypto:mpint(E) end, Key), Pad).


dh(doc) ->
    ["Test dh (Diffie-Hellman) functions."];
dh(suite) -> [];
dh(Config) when is_list(Config) ->
    Self = self(),
    GenP = fun() ->
		   %% Gen Param may take arbitrary long time to finish 
		   %% That's not a bug in erlang crypto application.
		   ?line DHPs = crypto:dh_generate_parameters(512,2),
		   ?line ok = crypto:dh_check(DHPs),
		   Self ! {param, DHPs}
	   end,
    Pid = spawn(GenP),
    receive 
	{param, DHPs} ->
	    timer:sleep(100), 
	    io:format("DHP ~p~n", [DHPs]),
	    DHPs_mpint = lists:map(fun(E) -> sized_binary(E) end, DHPs),
	    ?line {Pub1,Priv1} = crypto:generate_key(dh, DHPs),
	    io:format("Key1:~n~p~n~p~n~n", [Pub1,Priv1]),
	    ?line {Pub2,Priv2} = crypto:dh_generate_key(DHPs_mpint),
	    io:format("Key2:~n~p~n~p~n~n", [Pub2,Priv2]),
	    ?line A = crypto:compute_key(dh, Pub1, unsized_binary(Priv2), DHPs),
	    ?line A = crypto:dh_compute_key(sized_binary(Pub1), Priv2, DHPs_mpint),
	    timer:sleep(100),  %% Get another thread see if that triggers problem
	    ?line B = crypto:compute_key(dh, unsized_binary(Pub2), Priv1, DHPs),
	    ?line B = crypto:dh_compute_key(Pub2, sized_binary(Priv1), DHPs_mpint),
	    io:format("A ~p~n",[A]),
	    io:format("B ~p~n",[B]),
	    ?line A = B
    after 50000 ->
	    io:format("Killing Param generation which took to long ~p~n",[Pid]),
	    exit(Pid, kill)
    end.


ec(doc) ->
    ["Test ec (Ecliptic Curve) functions."];
ec(suite) -> [];
ec(Config) when is_list(Config) ->
    if_supported(ecdh, fun() -> ec_do() end).

ec_do() ->
    %% test for a name curve
    NamedCurve = hd(crypto:ec_curves()),
    {D2_pub, D2_priv} = crypto:generate_key(ecdh, NamedCurve),
    PrivECDH = [D2_priv, NamedCurve],
    PubECDH = [D2_pub, NamedCurve],
    %%TODO: find a published test case for a EC key

    Msg = <<99,234,6,64,190,237,201,99,80,248,58,40,70,45,149,218,5,246,242,63>>,
    Sign = crypto:sign(ecdsa, sha, Msg, PrivECDH),
    ?line true = crypto:verify(ecdsa, sha, Msg, Sign, PubECDH),
    ?line false = crypto:verify(ecdsa, sha, Msg, <<10,20>>, PubECDH),

    ok.

srp3(doc) ->
    ["SRP-3 test vectors generated by http://srp.stanford.edu/demo/demo.html"];
srp3(suite) -> [];
srp3(Config) when is_list(Config) ->
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
    UserPassHash = crypto:sha([Salt, crypto:sha([Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime),

    {ClientPublic, ClientPrivate} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}, ClientPrivate),
    {ServerPublic, ServerPrivate} = crypto:generate_key(srp, {host, [Verifier, Generator, Prime, Version]}, ServerPrivate),
    SessionKey = crypto:compute_key(srp, ServerPublic, {ClientPublic, ClientPrivate},
				    {user, [UserPassHash, Prime, Generator, Version, Scrambler]}),
    SessionKey = crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate},
				    {host, [Verifier, Prime, Version, Scrambler]}).

srp6(doc) ->
    ["SRP-6 test vectors generated by http://srp.stanford.edu/demo/demo.html"];
srp6(suite) -> [];
srp6(Config) when is_list(Config) ->
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
    UserPassHash = crypto:sha([Salt, crypto:sha([Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 
    ClientPublic = crypto:mod_pow(Generator, ClientPrivate, Prime),

    {ClientPublic, ClientPrivate} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}, ClientPrivate),
    {ServerPublic, ServerPrivate} = crypto:generate_key(srp, {host, [Verifier, Generator, Prime, Version]}, ServerPrivate),
    SessionKey = crypto:compute_key(srp, ServerPublic, {ClientPublic, ClientPrivate},
				    {user, [UserPassHash, Prime, Generator, Version, Scrambler]}),
    SessionKey = crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate},
				    {host, [Verifier, Prime, Version, Scrambler]}).

srp6a(doc) ->
    ["SRP-6a test vectors from RFC5054."];
srp6a(suite) -> [];
srp6a(Config) when is_list(Config) ->
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
    UserPassHash = crypto:sha([Salt, crypto:sha([Username, <<$:>>, Password])]),
    Verifier = crypto:mod_pow(Generator, UserPassHash, Prime), 

    {ClientPublic, ClientPrivate} = crypto:generate_key(srp, {user, [Generator, Prime, Version]}, ClientPrivate),
    {ServerPublic, ServerPrivate} = crypto:generate_key(srp, {host, [Verifier, Generator, Prime, Version]}, ServerPrivate),

    SessionKey = crypto:compute_key(srp, ServerPublic, {ClientPublic, ClientPrivate},
				    {user, [UserPassHash, Prime, Generator, Version, Scrambler]}),
    SessionKey = crypto:compute_key(srp, ClientPublic, {ServerPublic, ServerPrivate},
				    {host, [Verifier, Prime, Version, Scrambler]}).
    
%%
%%
exor_test(doc) ->
    ["Test the exor function."];
exor_test(suite) ->
    [];
exor_test(Config) when is_list(Config) ->
    B = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>,
    Z1 = zero_bin(B),
    Z1 = crypto:exor(B, B),
    B1 = crypto:strong_rand_bytes(100),
    B2 = crypto:strong_rand_bytes(100),
    Z2 = zero_bin(B1),
    Z2 = crypto:exor(B1, B1),
    Z2 = crypto:exor(B2, B2),
    R = xor_bytes(B1, B2),
    R = crypto:exor(B1, B2),
    ok.

%%
%%
rc4_test(doc) ->
    ["Test rc4 encryption ."];
rc4_test(suite) ->
    [];
rc4_test(Config) when is_list(Config) ->
    CT1 = <<"Yo baby yo">>,
    R1 = <<118,122,68,110,157,166,141,212,139,39>>,
    K = "apaapa",
    R1 = crypto:rc4_encrypt(K, CT1),
    CT1 = crypto:rc4_encrypt(K, R1),
    CT2 = lists:seq(0, 255),
    R2 = crypto:rc4_encrypt(K, CT2),
    CT2 = binary_to_list(crypto:rc4_encrypt(K, R2)),
    ok.

rc4_stream_test(doc) ->
    ["Test rc4 stream encryption ."];
rc4_stream_test(suite) ->
    [];
rc4_stream_test(Config) when is_list(Config) ->
    CT1 = <<"Yo ">>,
    CT2 = <<"baby yo">>,
    K = "apaapa",
    State0 = crypto:rc4_set_key(K),
    {State1, R1} = crypto:rc4_encrypt_with_state(State0, CT1),
    {_State2, R2} = crypto:rc4_encrypt_with_state(State1, CT2),
    R = list_to_binary([R1, R2]),
    <<118,122,68,110,157,166,141,212,139,39>> = R,
    ok.

blowfish_cfb64(doc) -> ["Test Blowfish encrypt/decrypt."];
blowfish_cfb64(suite) -> [];
blowfish_cfb64(Config) when is_list(Config) ->   			
    Key = <<1,35,69,103,137,171,205,239,240,225,210,195,180,165,150,135>>,

    IVec = <<254,220,186,152,118,84,50,16>>,
    Plain = <<"7654321 Now is the time for ">>,
    Enc = <<231,50,20,162,130,33,57,202,242,110,207,109,46,185,231,110,61,163,222,4,209,81,114,0,81,157,87,166>>,

    Enc = crypto:blowfish_cfb64_encrypt(Key, IVec, Plain),
    Plain = crypto:blowfish_cfb64_decrypt(Key, IVec, Enc),

    Key2 = <<"A2B4C">>,
    IVec2 = <<"12345678">>,
    Plain2 = <<"badger at my table....!">>,
    Enc2 = <<173,76,128,155,70,81,79,228,4,162,188,92,119,53,144,89,93,236,28,164,176,16,138>>,

    Enc2 = crypto:blowfish_cfb64_encrypt(Key2, IVec2, Plain2),
    Plain2 = crypto:blowfish_cfb64_decrypt(Key2, IVec2, Enc2).


smp(doc) -> "Check concurrent access to crypto driver";
smp(suite) -> [];
smp(Config) ->
    case erlang:system_info(smp_support) of
	true ->
	    NumOfProcs = erlang:system_info(schedulers),
	    io:format("smp starting ~p workers\n",[NumOfProcs]),
	    Seeds = [random:uniform(9999) || _ <- lists:seq(1,NumOfProcs)],
	    Parent = self(),
	    Pids = [spawn_link(fun()-> worker(Seed,Config,Parent) end)
		    || Seed <- Seeds],
	    wait_pids(Pids);

	false ->
	    {skipped,"No smp support"}
    end.
	    
worker(Seed, Config, Parent) ->
    io:format("smp worker ~p, seed=~p~n",[self(),Seed]),
    random:seed(Seed,Seed,Seed),
    worker_loop(100, Config),
    %%io:format("worker ~p done\n",[self()]),
    Parent ! self().

worker_loop(0, _) ->
    ok;
worker_loop(N, Config) ->
    Funcs = { md5, md5_update, md5_mac, md5_mac_io, sha, sha_update, des_cbc,
	      aes_cfb, aes_cbc, des_cbc_iter, rand_uniform_test, strong_rand_test,
	      rsa_verify_test, exor_test, rc4_test, rc4_stream_test, mod_exp_test,
              hmac_update_md5, hmac_update_sha, hmac_update_sha256, hmac_update_sha512,
	      hmac_rfc2202, hmac_rfc4231_sha224, hmac_rfc4231_sha256, hmac_rfc4231_sha384,
	      hmac_rfc4231_sha512, aes_ctr_stream },

    F = element(random:uniform(size(Funcs)),Funcs),
    %%io:format("worker ~p calling ~p\n",[self(),F]),
    ?MODULE:F(Config),
    worker_loop(N-1,Config).
    
wait_pids([]) -> 
    ok;
wait_pids(Pids) ->
    receive
	Pid ->
	    ?line true = lists:member(Pid,Pids),
	    Others = lists:delete(Pid,Pids),
	    io:format("wait_pid got ~p, still waiting for ~p\n",[Pid,Others]),
	    wait_pids(Others)
    end.

%%
%% Help functions
%%

% match
m(X, X) ->
    ?line true.
t(true) ->
    true.

% hexstr2bin
hexstr2bin(S) ->
    list_to_binary(hexstr2list(S)).

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

%%
%% Invert an element X mod P
%% Calculated as {1, {A,B}} = egcd(X,P),
%%   1 == P*A + X*B == X*B (mod P) i.e B is the inverse element
%%
%% X > 0, P > 0, X < P   (P should be prime)
%%
%% invert(X,P) when X > 0, P > 0, X < P ->
%%     I = inv(X,P,1,0),
%%     if 
%%         I < 0 -> P + I;
%%         true -> I
%%     end.

%% inv(0,_,_,Q) -> Q;
%% inv(X,P,R1,Q1) ->
%%     D = P div X,
%%     inv(P rem X, X, Q1 - D*R1, R1).    

sized_binary(Binary) when is_binary(Binary) ->
    <<(size(Binary)):32/integer, Binary/binary>>;
sized_binary(List) ->
    sized_binary(list_to_binary(List)).

unsized_binary(<<Sz:32/integer, Binary:Sz/binary>>) ->
    Binary.

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

zero_bin(N) when is_integer(N) ->
    N8 = N * 8,
    <<0:N8/integer>>;
zero_bin(B) when is_binary(B) ->
    zero_bin(size(B)).

my_dss_verify(Data,[Sign|Tail],Key) ->
    Res = my_dss_verify(Data,sized_binary(Sign),Key),
    case Tail of
        [] ->  Res;
        _ -> ?line Res = my_dss_verify(Data,Tail,Key)
    end;       
my_dss_verify(Data,Sign,Key) ->
    ?line Res = crypto:dss_verify(Data, Sign, Key),
    ?line Res = crypto:dss_verify(sha, Data, Sign, Key),
    ?line <<_:32,Raw/binary>> = Data,
    ?line Res = crypto:dss_verify(none, crypto:sha(Raw), Sign, Key),
    Res.

my_dss_sign(Data,Key) ->
    ?line S1 = crypto:dss_sign(Data, Key),
    ?line S2 = crypto:dss_sign(sha, Data, Key),
    ?line <<_:32,Raw/binary>> = Data,
    ?line S3 = crypto:dss_sign(none, crypto:sha(Raw), Key),
    [S1,S2,S3].

openssl_version() ->
    case crypto:info_lib() of
	[{<<"OpenSSL">>,LibVer,_}] when is_integer(LibVer) ->
	    LibVer;
	_ ->
	    undefined
    end.

if_supported(Algorithm, Fun) ->
    case lists:member(Algorithm, lists:append([Algo ||  {_, Algo}  <- crypto:supports()])) of
	true ->
	    Fun();
	_ ->
	    {skipped, io:format("~s not spupported", [Algorithm])}
    end.
