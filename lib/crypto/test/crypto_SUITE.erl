%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(crypto_SUITE).

-include("test_server.hrl").
-include("test_server_line.hrl").

-export([all/1, 
	 init_per_testcase/2,
	 fin_per_testcase/2,
	 info/1,
	 link_test/1,
	 md5/1,
	 md5_update/1,
	 md4/1,
	 md4_update/1,
	 sha/1,
	 sha_update/1,
	 sha256/1,
	 sha256_update/1,
	 sha512/1,
	 sha512_update/1,
	 md5_mac/1,
	 md5_mac_io/1,
	 des_cbc/1,
	 des_cbc_iter/1,
	 des_ecb/1,
	 aes_cfb/1,
	 aes_cbc/1,
	 aes_cbc_iter/1,
	 mod_exp_test/1,
	 rand_uniform_test/1,
	 rsa_verify_test/1,
	 dsa_verify_test/1,
	 rsa_sign_test/1,
	 dsa_sign_test/1,	 
	 rsa_encrypt_decrypt/1,
	 dh/1,
	 exor_test/1,
	 rc4_test/1,
	 blowfish_cfb64/1,
	 smp/1,
	 cleanup/1]).

-export([hexstr2bin/1]).

all(suite) ->
    [link_test,
     {conf,info,[md5,
		 md5_update,
		 md4,
		 md4_update,
		 md5_mac,
		 md5_mac_io,
		 sha,
		 sha_update,
%% 		 sha256,
%% 		 sha256_update,
%% 		 sha512,
%% 		 sha512_update,
		 des_cbc,
		 aes_cfb,
		 aes_cbc,
		 aes_cbc_iter,
		 des_cbc_iter,
		 des_ecb,
		 rand_uniform_test,
		 rsa_verify_test,
		 dsa_verify_test,
		 rsa_sign_test,
		 dsa_sign_test,	 
		 rsa_encrypt_decrypt,
		 dh, 
		 exor_test,
		 rc4_test,
		 mod_exp_test,
		 blowfish_cfb64,
		 smp],
      cleanup}].

init_per_testcase(_Name,Config) ->
    io:format("init_per_testcase\n"),
    ?line crypto:start(),
    Config.

fin_per_testcase(_Name,Config) ->
    io:format("fin_per_testcase\n"),
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
    ?line Wc = filename:join([CryptoPriv,"lib","crypto_drv.*"]),
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
		0 -> ok;
		_ ->
		    case ?t:is_commercial() of
			true ->
			    ?t:fail({libcrypto,not_statically_linked});
			false ->
			    {comment,"Not statically linked (OK for open-source platform)"}
		    end
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
		    end
	    end;
 	Ldd when is_list(Ldd) -> Ldd
    end.

%%
%%
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
	    ?line crypto:info(),
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

cleanup(doc) ->
    ["Cleanup (dummy)."];
cleanup(suite) ->
    [];
cleanup(Config) when is_list(Config) ->
    Config.

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
    ?line m(crypto:sha256("abc"),
	    hexstr2bin("BA7816BF8F01CFEA4141"
		       "40DE5DAE2223B00361A396177A9CB410FF61F20015AD")),
    ?line m(crypto:sha256("abcdbcdecdefdefgefghfghighijhijkijkljklmklm"
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
    ?line Ctx = crypto:sha256_init(),
    ?line Ctx1 = crypto:sha256_update(Ctx, "abcdbcdecdefdefgefghfghighi"),
    ?line Ctx2 = crypto:sha256_update(Ctx1, "jhijkijkljklmklmnlmnomnopnopq"),
    ?line m(crypto:sha256_final(Ctx2), 
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
    ?line m(crypto:sha512("abc"),
	    hexstr2bin("DDAF35A193617ABACC417349AE20413112E6FA4E89A97EA2"
		       "0A9EEEE64B55D39A2192992A274FC1A836BA3C23A3FEEBBD"
		       "454D4423643CE80E2A9AC94FA54CA49F")),
    ?line m(crypto:sha512("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"
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
    ?line Ctx = crypto:sha512_init(),
    ?line Ctx1 = crypto:sha512_update(Ctx, "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"),
    ?line Ctx2 = crypto:sha512_update(Ctx1, "hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"),
    ?line m(crypto:sha512_final(Ctx2), 
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
    ?line Cipher = concat_binary([Cipher1, Cipher2]),
    ?line m(Cipher, hexstr2bin("e5c7cdde872bf27c43e934008c389c"
			             "0f683788499a7c05f6")).

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
    ?line Cipher = crypto:aes_cfb_128_encrypt(Key, IVec, Plain),
    ?line m(Cipher, hexstr2bin("3b3fd92eb72dad20333449f8e83cfb4a")),
    ?line m(Plain, 
	    crypto:aes_cfb_128_decrypt(Key, IVec, Cipher)).

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
    ?line R1 = crypto:rand_uniform(L, H),
    ?line t(R1 >= L),
    ?line t(R1 < H),
    ?line rand_uniform_aux_test(N-1).

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
    
    ?line m(crypto:dss_verify(sized_binary(Msg), sized_binary(SigBlob),
			      ValidKey), true),

    BadMsg  = one_bit_wrong(Msg),
    ?line m(crypto:dss_verify(sized_binary(BadMsg), sized_binary(SigBlob),
			      ValidKey), false),
    BadSig = one_bit_wrong(SigBlob),
    ?line m(crypto:dss_verify(sized_binary(Msg), sized_binary(BadSig),
			      ValidKey), false),
    SizeErr = size(SigBlob) - 13,
    
    BadArg = (catch crypto:dss_verify(sized_binary(Msg), <<SizeErr:32, SigBlob/binary>>,
				      ValidKey)),
    ?line m(element(1,element(2,BadArg)), badarg),
    
    InValidKey = [crypto:mpint(P_p), 
		  crypto:mpint(Q_p), 
		  crypto:mpint(G_p),
		  crypto:mpint(Key+17)
		 ],
    
    ?line m(crypto:dss_verify(sized_binary(Msg), sized_binary(SigBlob),
			      InValidKey), false).

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
    
    PrivKey = [crypto:mpint(PubEx), crypto:mpint(Mod), crypto:mpint(PrivEx)],
    PubKey  = [crypto:mpint(PubEx), crypto:mpint(Mod)],
    ?line Sig1 = crypto:rsa_sign(sized_binary(Msg), PrivKey),
    ?line m(crypto:rsa_verify(sized_binary(Msg), sized_binary(Sig1),PubKey), true),
    
    ?line Sig2 = crypto:rsa_sign(md5, sized_binary(Msg), PrivKey),
    ?line m(crypto:rsa_verify(md5, sized_binary(Msg), sized_binary(Sig2),PubKey), true),
    
    ?line m(Sig1 =:= Sig2, false),
    ?line m(crypto:rsa_verify(md5, sized_binary(Msg), sized_binary(Sig1),PubKey), false),
    ?line m(crypto:rsa_verify(sha, sized_binary(Msg), sized_binary(Sig1),PubKey), true),
  
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
    ?line Sig1 = crypto:dss_sign(sized_binary(Msg), [Params, crypto:mpint(PrivKey)]),
    
    ?line m(crypto:dss_verify(sized_binary(Msg), sized_binary(Sig1), 
			      [Params, crypto:mpint(PubKey)]), true),
    
    ?line m(crypto:dss_verify(sized_binary(one_bit_wrong(Msg)), sized_binary(Sig1), 
			      [Params, crypto:mpint(PubKey)]), false),
    
    ?line m(crypto:dss_verify(sized_binary(Msg), sized_binary(one_bit_wrong(Sig1)), 
			      [Params, crypto:mpint(PubKey)]), false),

    %%?line Bad = crypto:dss_sign(sized_binary(Msg), [Params, crypto:mpint(PubKey)]),
    
    ok.


rsa_encrypt_decrypt(doc) ->
    ["Test rsa_public_encrypt and rsa_private_decrypt functions."];
rsa_encrypt_decrypt(suite) -> [];
rsa_encrypt_decrypt(Config) when is_list(Config) ->
    PubEx  = 65537,
    PrivEx = 7531712708607620783801185371644749935066152052780368689827275932079815492940396744378735701395659435842364793962992309884847527234216715366607660219930945,
    Mod = 7919488123861148172698919999061127847747888703039837999377650217570191053151807772962118671509138346758471459464133273114654252861270845708312601272799123,
    
    PrivKey = [crypto:mpint(PubEx), crypto:mpint(Mod), crypto:mpint(PrivEx)],
    PubKey  = [crypto:mpint(PubEx), crypto:mpint(Mod)],

    Msg = <<"7896345786348 Asldi">>,

    ?line PKCS1 = crypto:rsa_public_encrypt(Msg, PubKey, rsa_pkcs1_padding),
    ?line PKCS1Dec = crypto:rsa_private_decrypt(PKCS1, PrivKey, rsa_pkcs1_padding),
    io:format("PKCS1Dec ~p~n",[PKCS1Dec]),
    ?line Msg = PKCS1Dec,
    
    ?line OAEP = crypto:rsa_public_encrypt(Msg, PubKey, rsa_pkcs1_oaep_padding),
    ?line Msg = crypto:rsa_private_decrypt(OAEP, PrivKey, rsa_pkcs1_oaep_padding),

    <<Msg2Len:32,_/binary>> = crypto:mpint(Mod),
    Msg2 = list_to_binary(lists:duplicate(Msg2Len-1, $X)),
    ?line NoPad = crypto:rsa_public_encrypt(Msg2, PubKey, rsa_no_padding),
    ?line NoPadDec = crypto:rsa_private_decrypt(NoPad, PrivKey, rsa_no_padding),
    ?line NoPadDec = Msg2,
    
    ShouldBeError = (catch crypto:rsa_public_encrypt(Msg, PubKey, rsa_no_padding)),
    ?line {'EXIT', {encrypt_failed,_}} = ShouldBeError,
    
%%     ?line SSL = crypto:rsa_public_encrypt(Msg, PubKey, rsa_sslv23_padding),
%%     ?line Msg = crypto:rsa_private_decrypt(SSL, PrivKey, rsa_sslv23_padding),

    ?line PKCS1_2 = crypto:rsa_private_encrypt(Msg, PrivKey, rsa_pkcs1_padding),
    ?line PKCS1_2Dec = crypto:rsa_public_decrypt(PKCS1_2, PubKey, rsa_pkcs1_padding),
    io:format("PKCS2Dec ~p~n",[PKCS1_2Dec]),
    ?line Msg = PKCS1_2Dec,

    ?line PKCS1_3 = crypto:rsa_private_encrypt(Msg2, PrivKey, rsa_no_padding),
    ?line PKCS1_3Dec = crypto:rsa_public_decrypt(PKCS1_3, PubKey, rsa_no_padding),
    io:format("PKCS2Dec ~p~n",[PKCS1_3Dec]),
    ?line Msg2 = PKCS1_3Dec,
    
    ?line {'EXIT', {encrypt_failed,_}} = 
	(catch crypto:rsa_private_encrypt(Msg, PrivKey, rsa_no_padding)),
    
    ok.


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
	    ?line {Pub1,Priv1} = crypto:dh_generate_key(DHPs),
	    io:format("Key1:~n~p~n~p~n~n", [Pub1,Priv1]),
	    ?line {Pub2,Priv2} = crypto:dh_generate_key(DHPs),
	    io:format("Key2:~n~p~n~p~n~n", [Pub2,Priv2]),
	    ?line A = crypto:dh_compute_key(Pub1, Priv2, DHPs),
	    timer:sleep(100),  %% Get another thread see if that triggers problem
	    ?line B = crypto:dh_compute_key(Pub2, Priv1, DHPs),
	    io:format("A ~p~n",[A]),
	    io:format("B ~p~n",[B]),
	    ?line A = B
    after 50000 ->
	    io:format("Killing Param generation which took to long ~p~n",[Pid]),
	    exit(Pid, kill)
    end.

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
    B1 = crypto:rand_bytes(100),
    B2 = crypto:rand_bytes(100),
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
    CT1 = <<"hej på dig">>,
    R1 = <<71,112,14,44,140,33,212,144,155,47>>,
    K = "apaapa",
    R1 = crypto:rc4_encrypt(K, CT1),
    CT1 = crypto:rc4_encrypt(K, R1),
    CT2 = lists:seq(0, 255),
    R2 = crypto:rc4_encrypt(K, CT2),
    CT2 = binary_to_list(crypto:rc4_encrypt(K, R2)),
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
	      aes_cfb, aes_cbc, des_cbc_iter, rand_uniform_test, 
	      rsa_verify_test, exor_test, rc4_test, mod_exp_test },

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
