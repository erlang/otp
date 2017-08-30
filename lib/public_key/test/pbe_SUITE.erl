%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

-module(pbe_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> 
    [].

all() -> 
    [
     pbdkdf1,
     pbdkdf2,
     old_enc,
     pbes1,
     pbes2].

groups() -> 
    [].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:stop(crypto),
    try crypto:start() of
	ok ->
	    Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(crypto).

%%--------------------------------------------------------------------

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
   Config.


end_per_testcase(_TestCase, _Config) ->
   ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

pbdkdf1() ->
    [{doc,"Test with PKCS #5 PBKDF1 Test Vectors"}].
pbdkdf1(Config) when is_list(Config) ->
    %%Password = "password"
    %%     = (0x)70617373776F7264
    %%Salt     = (0x)78578E5A5D63CB06
    %%Count    = 1000
    %%kLen     = 16
    %%Key      = PBKDF1(Password, Salt, Count, kLen)
    %%= (0x)DC19847E05C64D2FAF10EBFB4A3D2A20

    Password = "password",
    Salt = <<16#78,16#57,16#8E,16#5A,16#5D,16#63,16#CB,16#06>>,
    Count = 1000,

   <<16#DC, 16#19, 16#84, 16#7E,
     16#05, 16#C6, 16#4D, 16#2F,
     16#AF, 16#10, 16#EB, 16#FB,
     16#4A, 16#3D, 16#2A, 16#20, _/binary>> =
	pubkey_pbe:pbdkdf1(Password, Salt, Count, sha).

pbdkdf2() ->
    [{doc,"Test with PKCS #5 PBKDF2 Test Vectors"}].
pbdkdf2(Config) when is_list(Config) ->
    %% Input:
    %%   P = "password" (8 octets)
    %%   S = "salt" (4 octets)
    %%   c = 1
    %%   dkLen = 20
    
    %% Output:
    %%   DK = 0c 60 c8 0f 96 1f 0e 71
    %%        f3 a9 b5 24 af 60 12 06
    %%        2f e0 37 a6             (20 octets)
    
    <<16#0c, 16#60, 16#c8, 16#0f, 16#96, 16#1f, 16#0e, 16#71,
      16#f3, 16#a9, 16#b5, 16#24, 16#af, 16#60, 16#12, 16#06,
      16#2f, 16#e0, 16#37, 16#a6>> = pubkey_pbe:pbdkdf2("password", "salt", 1, 20, fun crypto:hmac/4, sha, 20),
    
    %% Input:
    %%   P = "password" (8 octets)
    %%   S = "salt" (4 octets)
    %%   c = 2
    %%   dkLen = 20
    
    %% Output:
    %%   DK = ea 6c 01 4d c7 2d 6f 8c
    %%   cd 1e d9 2a ce 1d 41 f0
    %%   d8 de 89 57             (20 octets)
    
    <<16#ea, 16#6c, 16#01, 16#4d, 16#c7, 16#2d, 16#6f, 16#8c, 
      16#cd, 16#1e, 16#d9, 16#2a, 16#ce, 16#1d, 16#41, 16#f0,  
      16#d8,  16#de,  16#89, 16#57>>  =
	pubkey_pbe:pbdkdf2("password", "salt", 2, 20, fun crypto:hmac/4, sha, 20),

     %% Input:
     %%   P = "password" (8 octets)
     %%   S = "salt" (4 octets)
     %%   c = 4096
     %%   dkLen = 20

     %% Output:
     %%   DK = 4b 00 79 01 b7 65 48 9a
     %%        be ad 49 d9 26 f7 21 d0
     %%        65 a4 29 c1             (20 octets)

    <<16#4b, 16#00, 16#79, 16#01, 16#b7, 16#65, 16#48, 16#9a,
      16#be, 16#ad, 16#49, 16#d9, 16#26, 16#f7, 16#21, 16#d0,
      16#65, 16#a4, 16#29, 16#c1>> = pubkey_pbe:pbdkdf2("password", "salt", 4096, 20, fun crypto:hmac/4, sha, 20),

    %% Input:
    %%    P = "password" (8 octets)
    %%    S = "salt" (4 octets)
    %%    c = 16777216
    %%    dkLen = 20

    %%  Output:
    %%    DK = ee fe 3d 61 cd 4d a4 e4
    %%         e9 94 5b 3d 6b a2 15 8c
    %%         26 34 e9 84             (20 octets)

    
    <<16#ee, 16#fe, 16#3d, 16#61, 16#cd, 16#4d, 16#a4, 16#e4, 
      16#e9, 16#94, 16#5b, 16#3d, 16#6b, 16#a2, 16#15, 16#8c, 
      16#26, 16#34, 16#e9, 16#84>> = pubkey_pbe:pbdkdf2("password", "salt", 16777216, 20, fun crypto:hmac/4, sha, 20),
    
    %% Input:
    %%    P = "passwordPASSWORDpassword" (24 octets)
    %%    S = "saltSALTsaltSALTsaltSALTsaltSALTsalt" (36 octets)
    %%    c = 4096
    %%    dkLen = 25

    %%  Output:
    %%    DK = 3d 2e ec 4f e4 1c 84 9b
    %%         80 c8 d8 36 62 c0 e4 4a
    %%         8b 29 1a 96 4c f2 f0 70
    %%         38                      (25 octets)
    
    <<16#3d, 16#2e, 16#ec, 16#4f, 16#e4, 16#1c, 16#84, 16#9b, 
      16#80, 16#c8, 16#d8, 16#36, 16#62, 16#c0, 16#e4, 16#4a, 
      16#8b, 16#29, 16#1a, 16#96, 16#4c, 16#f2, 16#f0, 16#70, 
      16#38>>
	= pubkey_pbe:pbdkdf2("passwordPASSWORDpassword", 
			     "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096, 25, fun crypto:hmac/4, sha, 20),
    
     %% Input:
     %%   P = "pass\0word" (9 octets)
     %%   S = "sa\0lt" (5 octets)
     %%   c = 4096
     %%   dkLen = 16

     %% Output:
     %%   DK = 56 fa 6a a7 55 48 09 9d
     %%        cc 37 d7 f0 34 25 e0 c3 (16 octets)

    <<16#56, 16#fa, 16#6a, 16#a7, 16#55, 16#48, 16#09, 16#9d, 
      16#cc, 16#37, 16#d7, 16#f0, 16#34, 16#25, 16#e0, 16#c3>>
	= pubkey_pbe:pbdkdf2("pass\0word", 
			     "sa\0lt", 4096, 16, fun crypto:hmac/4, sha, 20).

old_enc() ->
    [{doc,"Tests encode/decode RSA key encrypted with different ciphers using old PEM encryption scheme"}].
old_enc(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    %% key generated with ssh-keygen -N hello_aes -f old_aes_128_cbc_enc_key.pem
    {ok, PemAesCbc} = file:read_file(filename:join(Datadir, "old_aes_128_cbc_enc_key.pem")),
    
    PemAesCbcEntry = public_key:pem_decode(PemAesCbc),
    ct:print("Pem entry: ~p" , [PemAesCbcEntry]),
    [{'RSAPrivateKey', _, {"AES-128-CBC",_}} = PubAesCbcEntry] = PemAesCbcEntry,
    #'RSAPrivateKey'{} = public_key:pem_entry_decode(PubAesCbcEntry, "hello_aes").

pbes1() ->
    [{doc,"Tests encode/decode EncryptedPrivateKeyInfo encrypted with different ciphers using PBES1"}].
pbes1(Config) when is_list(Config) ->
    decode_encode_key_file("pbes1_des_cbc_md5_enc_key.pem", "password", "DES-CBC", Config).
    
pbes2() ->
    [{doc,"Tests encode/decode EncryptedPrivateKeyInfo encrypted with different ciphers using PBES2"}].
pbes2(Config) when is_list(Config) ->
    decode_encode_key_file("pbes2_des_cbc_enc_key.pem", "password", "DES-CBC", Config),
    decode_encode_key_file("pbes2_des_ede3_cbc_enc_key.pem", "password", "DES-EDE3-CBC", Config),   
    decode_encode_key_file("pbes2_rc2_cbc_enc_key.pem", "password", "RC2-CBC", Config).

check_key_info(#'PrivateKeyInfo'{privateKeyAlgorithm =
				     #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?rsaEncryption},
				 privateKey = Key}) ->
    #'RSAPrivateKey'{} = public_key:der_decode('RSAPrivateKey', iolist_to_binary(Key)).

decode_encode_key_file(File, Password, Cipher, Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, PemKey} = file:read_file(filename:join(Datadir, File)),
    
    PemEntry = public_key:pem_decode(PemKey),
    ct:print("Pem entry: ~p" , [PemEntry]),
    [{Asn1Type, _, {Cipher,_} = CipherInfo} = PubEntry] = PemEntry,
    KeyInfo = public_key:pem_entry_decode(PubEntry, Password),
    PemKey1 = public_key:pem_encode([public_key:pem_entry_encode(Asn1Type, KeyInfo, {CipherInfo, Password})]),
    Pem = strip_ending_newlines(PemKey),
    Pem = strip_ending_newlines(PemKey1),
    check_key_info(KeyInfo).

strip_ending_newlines(Bin) ->
    string:strip(binary_to_list(Bin), right, 10).
