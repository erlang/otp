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

%% Purpose : Main Crypto API module.

-module(crypto).

-export([start/0, stop/0, info/0, info_lib/0]).
-export([md4/1, md4_init/0, md4_update/2, md4_final/1]).
-export([md5/1, md5_init/0, md5_update/2, md5_final/1]).
-export([sha/1, sha_init/0, sha_update/2, sha_final/1]).
%-export([sha256/1, sha256_init/0, sha256_update/2, sha256_final/1]).
%-export([sha512/1, sha512_init/0, sha512_update/2, sha512_final/1]).
-export([md5_mac/2, md5_mac_96/2, sha_mac/2, sha_mac_96/2]).
-export([des_cbc_encrypt/3, des_cbc_decrypt/3, des_cbc_ivec/1]).
-export([des_ecb_encrypt/2, des_ecb_decrypt/2]).
-export([des3_cbc_encrypt/5, des3_cbc_decrypt/5]).
-export([blowfish_ecb_encrypt/2, blowfish_ecb_decrypt/2]).
-export([blowfish_cbc_encrypt/3, blowfish_cbc_decrypt/3]).
-export([blowfish_cfb64_encrypt/3, blowfish_cfb64_decrypt/3]).
-export([blowfish_ofb64_encrypt/3]).
-export([des_ede3_cbc_encrypt/5, des_ede3_cbc_decrypt/5]).
-export([aes_cfb_128_encrypt/3, aes_cfb_128_decrypt/3]).
-export([exor/2]).
-export([rc4_encrypt/2, rc4_set_key/1, rc4_encrypt_with_state/2]).
-export([rc2_40_cbc_encrypt/3, rc2_40_cbc_decrypt/3]).
-export([dss_verify/3, rsa_verify/3, rsa_verify/4]).
-export([dss_sign/2, rsa_sign/2, rsa_sign/3]).
-export([rsa_public_encrypt/3, rsa_private_decrypt/3]).
-export([rsa_private_encrypt/3, rsa_public_decrypt/3]).
-export([dh_generate_key/1, dh_generate_key/2, dh_compute_key/3]).
-export([rand_bytes/1, rand_bytes/3, rand_uniform/2]).
-export([mod_exp/3, mpint/1, erlint/1]).
%% -export([idea_cbc_encrypt/3, idea_cbc_decrypt/3]).
-export([aes_cbc_128_encrypt/3, aes_cbc_128_decrypt/3]).
-export([aes_cbc_256_encrypt/3, aes_cbc_256_decrypt/3]).
-export([aes_cbc_ivec/1]).

-export([dh_generate_parameters/2, dh_check/1]). %% Testing see below

-define(INFO,		 0).
-define(MD5,		 1).
-define(MD5_INIT,	 2).
-define(MD5_UPDATE,	 3).
-define(MD5_FINAL,	 4).
-define(SHA,		 5).
-define(SHA_INIT,	 6).
-define(SHA_UPDATE,	 7).
-define(SHA_FINAL,	 8).
-define(MD5_MAC,	 9).
-define(MD5_MAC_96,	 10).
-define(SHA_MAC,	 11).
-define(SHA_MAC_96,	 12).
-define(DES_CBC_ENCRYPT, 13).
-define(DES_CBC_DECRYPT, 14).
-define(DES_EDE3_CBC_ENCRYPT, 15).
-define(DES_EDE3_CBC_DECRYPT, 16).
-define(AES_CFB_128_ENCRYPT, 17).
-define(AES_CFB_128_DECRYPT, 18).
-define(RAND_BYTES,	 19).
-define(RAND_UNIFORM,    20).
-define(MOD_EXP,	 21).
-define(DSS_VERIFY,	 22).
-define(RSA_VERIFY_SHA,	 23).
%-define(RSA_VERIFY_MD5,	 35).
-define(AES_CBC_128_ENCRYPT, 24).
-define(AES_CBC_128_DECRYPT, 25).
-define(XOR,		 26).
-define(RC4_ENCRYPT,     27).
-define(RC4_SET_KEY, 28).
-define(RC4_ENCRYPT_WITH_STATE, 29).
-define(RC2_40_CBC_ENCRYPT, 30).
-define(RC2_40_CBC_DECRYPT, 31).
-define(AES_CBC_256_ENCRYPT, 32).
-define(AES_CBC_256_DECRYPT, 33).
-define(INFO_LIB,34).
%-define(RSA_VERIFY_SHA,	 23).
-define(RSA_VERIFY_MD5,	 35).
-define(RSA_SIGN_SHA,    36).
-define(RSA_SIGN_MD5,    37).
-define(DSS_SIGN,        38).
-define(RSA_PUBLIC_ENCRYPT,  39).
-define(RSA_PRIVATE_DECRYPT, 40).
-define(RSA_PRIVATE_ENCRYPT, 41).
-define(RSA_PUBLIC_DECRYPT,  42).
-define(DH_GENERATE_PARAMS,  43).
-define(DH_CHECK,            44).
-define(DH_GENERATE_KEY,     45).
-define(DH_COMPUTE_KEY,      46).
-define(MD4,		 47).
-define(MD4_INIT,	 48).
-define(MD4_UPDATE,	 49).
-define(MD4_FINAL,	 50).

%% -define(SHA256,		 51).
%% -define(SHA256_INIT,	 52).
%% -define(SHA256_UPDATE,	 53).
%% -define(SHA256_FINAL,	 54).
%% -define(SHA512,		 55).
%% -define(SHA512_INIT,	 56).
%% -define(SHA512_UPDATE,	 57).
%% -define(SHA512_FINAL,	 58).

-define(BF_CFB64_ENCRYPT, 59).
-define(BF_CFB64_DECRYPT, 60).
-define(BF_ECB_ENCRYPT,   61).
-define(BF_ECB_DECRYPT,   62).
-define(BF_OFB64_ENCRYPT, 63).
-define(BF_CBC_ENCRYPT,   64).
-define(BF_CBC_DECRYPT,   65).

-define(DES_ECB_ENCRYPT, 66).
-define(DES_ECB_DECRYPT, 67).

%% -define(IDEA_CBC_ENCRYPT, 34).
%% -define(IDEA_CBC_DECRYPT, 35).

-define(FUNC_LIST, [md4, md4_init, md4_update, md4_final,
		    md5, md5_init, md5_update, md5_final,
		    sha, sha_init, sha_update, sha_final,
%% 		    sha256, sha256_init, sha256_update, sha256_final,
%% 		    sha512, sha512_init, sha512_update, sha512_final,
		    md5_mac,  md5_mac_96,
		    sha_mac,  sha_mac_96,
		    des_cbc_encrypt, des_cbc_decrypt,
		    des_ecb_encrypt, des_ecb_decrypt,
		    des_ede3_cbc_encrypt, des_ede3_cbc_decrypt,
		    aes_cfb_128_encrypt, aes_cfb_128_decrypt,
		    rand_bytes,
		    rand_uniform,
		    mod_exp,
		    dss_verify,dss_sign,
		    rsa_verify,rsa_sign,
		    rsa_public_encrypt,rsa_private_decrypt, 
		    rsa_private_encrypt,rsa_public_decrypt, 
		    dh_generate_key, dh_compute_key,
		    aes_cbc_128_encrypt, aes_cbc_128_decrypt,
		    exor,
		    rc4_encrypt, rc4_set_key, rc4_encrypt_with_state,
		    rc2_40_cbc_encrypt, rc2_40_cbc_decrypt,
		    %% idea_cbc_encrypt, idea_cbc_decrypt,
		    aes_cbc_256_encrypt, aes_cbc_256_decrypt,
		    info_lib]).

start() ->
    application:start(crypto).

stop() ->
    application:stop(crypto).

info() ->
    lists:map(fun(I) -> 
		      lists:nth(I, ?FUNC_LIST) 
	      end, binary_to_list(control(?INFO, []))).

info_lib() ->
    <<_DrvVer:8, NameSize:8, Name:NameSize/binary,
      VerNum:32, VerStr/binary>> = control(?INFO_LIB,[]),
    [{Name,VerNum,VerStr}].

%% Below Key and Data are binaries or IO-lists. IVec is a binary.
%% Output is always a binary. Context is a binary.

%%
%%  MESSAGE DIGESTS
%%

%%
%%  MD5
%%
md5(Data) ->
    control(?MD5, Data).

md5_init() ->
    control(?MD5_INIT, []).

md5_update(Context, Data) ->
    control(?MD5_UPDATE, [Context, Data]).

md5_final(Context) ->
    control(?MD5_FINAL, Context).

%%
%%  MD4
%%
md4(Data) ->
    control(?MD4, Data).

md4_init() ->
    control(?MD4_INIT, []).

md4_update(Context, Data) ->
    control(?MD4_UPDATE, [Context, Data]).

md4_final(Context) ->
    control(?MD4_FINAL, Context).

%%
%% SHA
%%
sha(Data) ->
    control(?SHA, Data).

sha_init() ->
    control(?SHA_INIT, []).

sha_update(Context, Data) ->
    control(?SHA_UPDATE, [Context, Data]).

sha_final(Context) ->
    control(?SHA_FINAL, Context).

%% sha256 and sha512 requires openssl-0.9.8 removed for now

%% sha256(Data) ->
%%     control(?SHA256, Data).

%% sha256_init() ->
%%     control(?SHA256_INIT, []).

%% sha256_update(Context, Data) ->
%%     control(?SHA256_UPDATE, [Context, Data]).

%% sha256_final(Context) ->
%%         control(?SHA256_FINAL, Context).

%% sha512(Data) ->
%%     control(?SHA512, Data).

%% sha512_init() ->
%%     control(?SHA512_INIT, []).

%% sha512_update(Context, Data) ->
%%     control(?SHA512_UPDATE, [Context, Data]).

%% sha512_final(Context) ->
%%     control(?SHA512_FINAL, Context).

%%
%%  MESSAGE AUTHENTICATION CODES
%%

%%
%%  MD5_MAC
%%
md5_mac(Key, Data) ->
    control_bin(?MD5_MAC, Key, Data).

md5_mac_96(Key, Data) ->
    control_bin(?MD5_MAC_96, Key, Data).

%%
%%  SHA_MAC
%%
sha_mac(Key, Data) ->
    control_bin(?SHA_MAC, Key, Data).

sha_mac_96(Key, Data) ->
    control_bin(?SHA_MAC_96, Key, Data).

%%
%% CRYPTO FUNCTIONS
%%

%%
%% DES - in cipher block chaining mode (CBC)
%%
des_cbc_encrypt(Key, IVec, Data) ->
    control(?DES_CBC_ENCRYPT, [Key, IVec, Data]).

des_cbc_decrypt(Key, IVec, Data) ->
    control(?DES_CBC_DECRYPT, [Key, IVec, Data]).

%%
%% dec_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of 
%% des_cbc_[encrypt|decrypt].
%%
des_cbc_ivec(Data) when is_binary(Data) -> 
    {_, IVec} = split_binary(Data, size(Data) - 8),
    IVec;
des_cbc_ivec(Data) when is_list(Data) ->
    des_cbc_ivec(list_to_binary(Data)).

%%
%% DES - in electronic codebook mode (ECB)
%%
des_ecb_encrypt(Key, Data) ->
    control(?DES_ECB_ENCRYPT, [Key, Data]).

des_ecb_decrypt(Key, Data) ->
    control(?DES_ECB_DECRYPT, [Key, Data]).

%%
%% DES3 - in cipher block chaining mode (CBC)
%%
des3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data).
des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    %%io:format("des_ede3_cbc_encrypt: size(Data)=~p\n", [size(list_to_binary([Data]))]),
    control(?DES_EDE3_CBC_ENCRYPT, [Key1, Key2, Key3, IVec, Data]).

des3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data).
des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    control(?DES_EDE3_CBC_DECRYPT, [Key1, Key2, Key3, IVec, Data]).

%%
%% Blowfish
%%
blowfish_ecb_encrypt(Key, Data) when byte_size(Data) >= 8 ->
    control_bin(?BF_ECB_ENCRYPT, Key, list_to_binary([Data])).

blowfish_ecb_decrypt(Key, Data) when byte_size(Data) >= 8 ->
    control_bin(?BF_ECB_DECRYPT, Key, list_to_binary([Data])).

blowfish_cbc_encrypt(Key, IVec, Data) when byte_size(Data) rem 8 =:= 0 ->
    control_bin(?BF_CBC_ENCRYPT, Key, list_to_binary([IVec, Data])).

blowfish_cbc_decrypt(Key, IVec, Data) when byte_size(Data) rem 8 =:= 0 ->
    control_bin(?BF_CBC_DECRYPT, Key, list_to_binary([IVec, Data])).

blowfish_cfb64_encrypt(Key, IVec, Data) when byte_size(IVec) =:= 8 ->
    control_bin(?BF_CFB64_ENCRYPT, Key, list_to_binary([IVec, Data])).

blowfish_cfb64_decrypt(Key, IVec, Data) when byte_size(IVec) =:= 8 ->
    control_bin(?BF_CFB64_DECRYPT, Key, list_to_binary([IVec, Data])).

blowfish_ofb64_encrypt(Key, IVec, Data) when byte_size(IVec) =:= 8 ->
    control_bin(?BF_OFB64_ENCRYPT, Key, list_to_binary([IVec, Data])).

%%
%% AES in cipher feedback mode (CFB)
%%
aes_cfb_128_encrypt(Key, IVec, Data) ->
    control(?AES_CFB_128_ENCRYPT, [Key, IVec, Data]).

aes_cfb_128_decrypt(Key, IVec, Data) ->
    control(?AES_CFB_128_DECRYPT, [Key, IVec, Data]).    


%% %%
%% %% IDEA - in cipher block chaining mode (CBC)
%% %%
%% idea_cbc_encrypt(Key, IVec, Data) ->
%%     control(?IDEA_CBC_ENCRYPT, [Key, IVec, Data]).

%% idea_cbc_decrypt(Key, IVec, Data) ->
%%     control(?IDEA_CBC_DECRYPT, [Key, IVec, Data]).


%% 
%% RAND - pseudo random numbers using RN_ functions in crypto lib
%%

rand_bytes(Bytes) ->
    rand_bytes(Bytes, 0, 0).
rand_bytes(Bytes, Topmask, Bottommask) ->
    control(?RAND_BYTES,[<<Bytes:32/integer,
			  Topmask:8/integer,
			  Bottommask:8/integer>>]).

rand_uniform(From,To) when is_binary(From), is_binary(To) ->
    case control(?RAND_UNIFORM,[From,To]) of
	<<Len:32/integer, MSB, Rest/binary>> when MSB > 127 ->
	    <<(Len + 1):32/integer, 0, MSB, Rest/binary>>;
	Whatever ->
	    Whatever
    end;
rand_uniform(From,To) when is_integer(From),is_integer(To) ->
    BinFrom = mpint(From),
    BinTo = mpint(To),
    case rand_uniform(BinFrom, BinTo) of
        Result when is_binary(Result) ->
            erlint(Result);
        Other ->
            Other
    end.

%%
%% mod_exp - utility for rsa generation
%%
mod_exp(Base, Exponent, Modulo)
  when is_integer(Base), is_integer(Exponent), is_integer(Modulo) ->
    erlint(mod_exp(mpint(Base), mpint(Exponent), mpint(Modulo)));

mod_exp(Base, Exponent, Modulo) ->
    case control(?MOD_EXP,[Base,Exponent,Modulo]) of
	<<Len:32/integer, MSB, Rest/binary>> when MSB > 127 ->
	    <<(Len + 1):32/integer, 0, MSB, Rest/binary>>;
	Whatever ->
	    Whatever
    end.

%%
%% DSS, RSA - verify
%%

%% Key = [P,Q,G,Y]   P,Q,G=DSSParams  Y=PublicKey
dss_verify(Data,Signature,Key) ->
    control(?DSS_VERIFY, [Data,Signature,Key]) == <<1>>.

% Key = [E,N]  E=PublicExponent N=PublicModulus
rsa_verify(Data,Signature,Key) ->
    rsa_verify(sha, Data,Signature,Key).
rsa_verify(Type,Data,Signature,Key) ->
    control(rsa_verify_digest_type(Type), [Data,Signature,Key]) == <<1>>.

rsa_verify_digest_type(md5) -> ?RSA_VERIFY_MD5;
rsa_verify_digest_type(sha) -> ?RSA_VERIFY_SHA;
rsa_verify_digest_type(Bad) -> erlang:error(badarg, [Bad]).

%%
%% DSS, RSA - sign
%%
%% Key = [P,Q,G,X]   P,Q,G=DSSParams  X=PrivateKey
dss_sign(Data, Key) ->
    <<Ret:8, Signature/binary>> = control(?DSS_SIGN, [Data,Key]),
    case Ret of
	1 -> Signature;
	0 -> erlang:error(badkey, [Data, Key])
    end.

%% Key = [E,N,D]  E=PublicExponent N=PublicModulus  D=PrivateExponent
rsa_sign(Data,Key) ->
    rsa_sign(sha, Data, Key).
rsa_sign(Type, Data, Key) ->
    <<Ret:8, Signature/binary>> = control(rsa_sign_digest_type(Type), [Data,Key]),
    case Ret of
	1 -> Signature;
	0 -> erlang:error(badkey, [Type,Data,Key])
    end.

rsa_sign_digest_type(md5) -> ?RSA_SIGN_MD5;
rsa_sign_digest_type(sha) -> ?RSA_SIGN_SHA;
rsa_sign_digest_type(Bad) -> erlang:error(badarg, [Bad]).

%%
%%  rsa_public_encrypt
%%  rsa_private_decrypt

%% Binary, Key = [E,N]
rsa_public_encrypt(BinMesg, Key, Padding) ->
    Size = iolist_size(BinMesg),
    <<Ret:8, Signature/binary>> = 
	control(?RSA_PUBLIC_ENCRYPT, [<<Size:32>>,BinMesg,Key,rsa_pad(Padding)]),
    case Ret of
	1 -> Signature;
	0 -> erlang:error(encrypt_failed, [BinMesg,Key, Padding])
    end.    

%% Binary, Key = [E,N,D]
rsa_private_decrypt(BinMesg, Key, Padding) ->
    Size = iolist_size(BinMesg),
    <<Ret:8, Signature/binary>> = 
	control(?RSA_PRIVATE_DECRYPT, [<<Size:32>>,BinMesg,Key,rsa_pad(Padding)]),
    case Ret of
	1 -> Signature;
	0 -> erlang:error(decrypt_failed, [BinMesg,Key, Padding])
    end.    

rsa_pad(rsa_pkcs1_padding) -> 1;
rsa_pad(rsa_pkcs1_oaep_padding) -> 2;
%% rsa_pad(rsa_sslv23_padding) -> 3;
rsa_pad(rsa_no_padding) -> 0;
rsa_pad(Bad) -> erlang:error(badarg, [Bad]).
    
%% Binary, Key = [E,N,D]
rsa_private_encrypt(BinMesg, Key, Padding) ->
    Size = iolist_size(BinMesg),
    <<Ret:8, Signature/binary>> = 
	control(?RSA_PRIVATE_ENCRYPT, [<<Size:32>>,BinMesg,Key,rsa_pad(Padding)]),
    case Ret of
	1 -> Signature;
	0 -> erlang:error(encrypt_failed, [BinMesg,Key, Padding])
    end.    

%% Binary, Key = [E,N]
rsa_public_decrypt(BinMesg, Key, Padding) ->
    Size = iolist_size(BinMesg),
    <<Ret:8, Signature/binary>> = 
	control(?RSA_PUBLIC_DECRYPT, [<<Size:32>>,BinMesg,Key,rsa_pad(Padding)]),
    case Ret of
	1 -> Signature;
	0 -> erlang:error(decrypt_failed, [BinMesg,Key, Padding])
    end.
    
%%
%% AES - with 128 or 256 bit key in cipher block chaining mode (CBC)
%%

aes_cbc_128_encrypt(Key, IVec, Data) ->
    control(?AES_CBC_128_ENCRYPT, [Key, IVec, Data]).

aes_cbc_128_decrypt(Key, IVec, Data) ->
    control(?AES_CBC_128_DECRYPT, [Key, IVec, Data]).

aes_cbc_256_encrypt(Key, IVec, Data) ->
    control(?AES_CBC_256_ENCRYPT, [Key, IVec, Data]).

aes_cbc_256_decrypt(Key, IVec, Data) ->
    control(?AES_CBC_256_DECRYPT, [Key, IVec, Data]).

%%
%% aes_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% aes_cbc_*_[encrypt|decrypt].
%% IVec size: 16 bytes
%%
aes_cbc_ivec(Data) when is_binary(Data) ->
    {_, IVec} = split_binary(Data, size(Data) - 16),
    IVec;
aes_cbc_ivec(Data) when is_list(Data) ->
    aes_cbc_ivec(list_to_binary(Data)).


%%
%% XOR - xor to iolists and return a binary
%% NB doesn't check that they are the same size, just concatenates
%% them and sends them to the driver
%%
exor(A, B) ->
    control(?XOR, [A, B]).

%%
%% RC4 - symmetric stream cipher
%%
rc4_encrypt(Key, Data) ->
    control_bin(?RC4_ENCRYPT, Key, Data).

rc4_set_key(Key) ->
    control(?RC4_SET_KEY, Key).

rc4_encrypt_with_state(State, Data) ->
    <<Sz:32/integer-big-unsigned, S:Sz/binary, D/binary>> = 
        control_bin(?RC4_ENCRYPT_WITH_STATE, State, Data),
    {S, D}.

%%
%% RC2 - 40 bits block cipher
%%
rc2_40_cbc_encrypt(Key, IVec, Data) ->
    control(?RC2_40_CBC_ENCRYPT, [Key, IVec, Data]).

rc2_40_cbc_decrypt(Key, IVec, Data) ->
    control(?RC2_40_CBC_DECRYPT, [Key, IVec, Data]).

%%
%% DH Diffie-Hellman functions
%% 

%% Generate (and check) Parameters is not documented because they are implemented
%% for testing (and offline parameter generation) only.
%% From the openssl doc: 
%%  DH_generate_parameters() may run for several hours before finding a suitable prime.
%% Thus dh_generate_parameters may in this implementation block 
%% the emulator for several hours.
%%
%% usage: dh_generate_parameters(1024, 2 or 5) -> 
%%    [Prime=mpint(), SharedGenerator=mpint()]
dh_generate_parameters(PrimeLen, Generator) 
  when is_integer(PrimeLen), is_integer(Generator) ->
    case control(?DH_GENERATE_PARAMS, <<PrimeLen:32, Generator:32>>) of
	<<0:8, _/binary>> ->
	    erlang:error(generation_failed, [PrimeLen,Generator]);
	<<1:8, PLen0:32, _:PLen0/binary, GLen0:32,_:GLen0/binary>> = Bin -> 
	    PLen = PLen0+4, 
	    GLen = GLen0+4,
	    <<_:8, PBin:PLen/binary,GBin:GLen/binary>> = Bin,
	    [PBin, GBin]
    end.

%% Checks that the DHParameters are ok.
%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
dh_check(DHParameters) ->
    case control(?DH_CHECK, DHParameters) of
	<<0:32>>  -> ok;
	<<_:24,_:1,_:1,_:1,1:1>> -> not_prime;
	<<_:24,_:1,_:1,1:1,0:1>> -> not_strong_prime;
	<<_:24,_:1,1:1,0:1,0:1>> -> unable_to_check_generator;
	<<_:24,1:1,0:1,0:1,0:1>> -> not_suitable_generator;
	<<16#FFFF:32>> -> {error, check_failed};
	<<X:32>>  -> {unknown, X}
    end.

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% PrivKey = mpint()
dh_generate_key(DHParameters) ->
    dh_generate_key(<<0:32>>, DHParameters).
dh_generate_key(PrivateKey, DHParameters) ->
    case control(?DH_GENERATE_KEY, [PrivateKey, DHParameters]) of
	<<0:8, _/binary>> ->
	    erlang:error(generation_failed, [PrivateKey,DHParameters]);
	Bin = <<1:8, PubLen0:32, _:PubLen0/binary, PrivLen0:32, _:PrivLen0/binary>> -> 
	    PubLen = PubLen0+4, 
	    PrivLen = PrivLen0+4,
	    <<_:8, PubBin:PubLen/binary,PrivBin:PrivLen/binary>> = Bin,
	    {PubBin, PrivBin}
    end.

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% MyPrivKey, OthersPublicKey = mpint() 
dh_compute_key(OthersPublicKey, MyPrivateKey, DHParameters) ->
    case control(?DH_COMPUTE_KEY, [OthersPublicKey, MyPrivateKey, DHParameters]) of
	<<0:8, _/binary>> ->
	    erlang:error(computation_failed, [OthersPublicKey,MyPrivateKey,DHParameters]);
	<<1:8, Binary/binary>> -> Binary
    end.

%%
%%  LOCAL FUNCTIONS
%%
control_bin(Cmd, Key, Data) ->
    Sz = iolist_size(Key),
    control(Cmd, [<<Sz:32/integer-unsigned>>, Key, Data]).

control(Cmd, Data) ->
    Port = crypto_server:client_port(),
    erlang:port_control(Port, Cmd, Data).


%% sizehdr(N) ->
%%     [(N bsr 24) band 255,
%%      (N bsr 16) band 255,
%%      (N bsr  8) band 255,
%%      N band 255].

%% Flat length of IOlist (or binary)
%% flen(L) when binary(L) ->
%%     size(L);
%% flen(L) ->
%%     flen(L, 0).

%% flen([H| T], N) when list(H) ->
%%     flen(H, flen(T, N));
%% flen([H| T], N) when binary(H) ->
%%     flen(T, N + size(H));
%% flen([H| T], N) when integer(H), 0 =< H, H =<  255 ->
%%     flen(T, N + 1);
%% flen([], N) ->
%%     N.

%% large integer in a binary with 32bit length
%% MP representaion  (SSH2)
mpint(X) when X < 0 ->
    case X of
	-1 ->
	    <<0,0,0,1,16#ff>>;	    
       _ ->
	    mpint_neg(X,0,[])
    end;
mpint(X) ->
    case X of 
	0 ->
	    <<0,0,0,0>>;
	_ ->
	    mpint_pos(X,0,[])
    end.

-define(UINT32(X),   X:32/unsigned-big-integer).

mpint_neg(-1,I,Ds=[MSB|_]) ->
    if MSB band 16#80 =/= 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([255|Ds]))/binary>>;
       true ->
	    (<<?UINT32(I), (list_to_binary(Ds))/binary>>)
    end;
mpint_neg(X,I,Ds)  ->
    mpint_neg(X bsr 8,I+1,[(X band 255)|Ds]).
    
mpint_pos(0,I,Ds=[MSB|_]) ->
    if MSB band 16#80 == 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([0|Ds]))/binary>>;
       true ->
	    (<<?UINT32(I), (list_to_binary(Ds))/binary>>)
    end;
mpint_pos(X,I,Ds) ->
    mpint_pos(X bsr 8,I+1,[(X band 255)|Ds]).

%% int from integer in a binary with 32bit length
erlint(<<MPIntSize:32/integer,MPIntValue/binary>>) ->
    Bits= MPIntSize * 8,
    <<Integer:Bits/integer>> = MPIntValue,
    Integer.
