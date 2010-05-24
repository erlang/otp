%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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

-export([start/0, stop/0, info/0, info_lib/0, version/0]).
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

-type digest_type() :: 'md5' | 'sha'.
-type crypto_integer() :: binary() | integer().

-define(nif_stub,nif_stub_error(?LINE)).

-on_load(on_load/0).

-define(CRYPTO_NIF_VSN,101).

on_load() ->
    LibName = "crypto",
    PrivDir = code:priv_dir(crypto),
    Lib1 = filename:join([PrivDir, "lib", LibName]),
    Status = case erlang:load_nif(Lib1, ?CRYPTO_NIF_VSN) of
		 ok -> ok;
		 {error, {load_failed, _}}=Error1 ->
		     LibDir2 = 
			 filename:join([PrivDir, "lib", 
					erlang:system_info(system_architecture)]),
		     Candidate =
			 filelib:wildcard(filename:join([LibDir2,LibName ++ "*" ])),
		     case Candidate of
			 [] -> Error1;
			 _ ->
			     Lib2 = filename:join([LibDir2, LibName]),
			     erlang:load_nif(Lib2, ?CRYPTO_NIF_VSN)
		     end;
		 Error1 -> Error1
	     end,
    case Status of
	ok -> ok;
	{error, {E, Str}} ->
	    error_logger:error_msg("Unable to load crypto library. Failed with error:~n\"~p, ~s\"~n"
				   "OpenSSL might not be installed on this system.~n",[E,Str]),
	    Status
    end.
    

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

start() ->
    application:start(crypto).

stop() ->
    application:stop(crypto).

info() ->
    ?FUNC_LIST.

info_lib() -> ?nif_stub.

%% Crypto app version history:
%% (no version): Driver implementation
%% 2.0         : NIF implementation, requires OTP R14
version() -> ?CRYPTO_VSN.
     
%% Below Key and Data are binaries or IO-lists. IVec is a binary.
%% Output is always a binary. Context is a binary.

%%
%%  MESSAGE DIGESTS
%%

%%
%%  MD5
%%

-spec md5(iodata()) -> binary().
-spec md5_init() -> binary().
-spec md5_update(binary(), iodata()) -> binary().
-spec md5_final(binary()) -> binary().

md5(_Data) -> ?nif_stub.
md5_init() -> ?nif_stub.
md5_update(_Context, _Data) -> ?nif_stub.
md5_final(_Context) -> ?nif_stub.

%%
%%  MD4
%%
-spec md4(iodata()) -> binary().
-spec md4_init() -> binary().
-spec md4_update(binary(), iodata()) -> binary().
-spec md4_final(binary()) -> binary().

md4(_Data) -> ?nif_stub.
md4_init() -> ?nif_stub.
md4_update(_Context, _Data) -> ?nif_stub.
md4_final(_Context) -> ?nif_stub.

%%
%% SHA
%%
-spec sha(iodata()) -> binary().
-spec sha_init() -> binary().
-spec sha_update(binary(), iodata()) -> binary().
-spec sha_final(binary()) -> binary().

sha(_Data) -> ?nif_stub.
sha_init() -> ?nif_stub.
sha_update(_Context, _Data) -> ?nif_stub.
sha_final(_Context) -> ?nif_stub.


%%
%%  MESSAGE AUTHENTICATION CODES
%%

%%
%%  MD5_MAC
%%
-spec md5_mac(iodata(), iodata()) -> binary.
-spec md5_mac_96(iodata(), iodata()) -> binary.

md5_mac(Key, Data) ->
    md5_mac_n(Key,Data,16).

md5_mac_96(Key, Data) ->
    md5_mac_n(Key,Data,12).

md5_mac_n(_Key,_Data,_MacSz) -> ?nif_stub.
    
%%
%%  SHA_MAC
%%
-spec sha_mac(iodata(), iodata()) -> binary.
-spec sha_mac_96(iodata(), iodata()) -> binary.

sha_mac(Key, Data) ->
    sha_mac_n(Key,Data,20).

sha_mac_96(Key, Data) ->
    sha_mac_n(Key,Data,12).

sha_mac_n(_Key,_Data,_MacSz) -> ?nif_stub.
    
%%
%% CRYPTO FUNCTIONS
%%

%%
%% DES - in cipher block chaining mode (CBC)
%%
-spec des_cbc_encrypt(iodata(), binary(), iodata()) -> binary().
-spec des_cbc_decrypt(iodata(), binary(), iodata()) -> binary().

des_cbc_encrypt(Key, IVec, Data) ->
    des_cbc_crypt(Key, IVec, Data, true).

des_cbc_decrypt(Key, IVec, Data) ->
    des_cbc_crypt(Key, IVec, Data, false).

des_cbc_crypt(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.

%%
%% dec_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of 
%% des_cbc_[encrypt|decrypt].
%%
-spec des_cbc_ivec(iodata()) -> binary().

des_cbc_ivec(Data) when is_binary(Data) -> 
    {_, IVec} = split_binary(Data, size(Data) - 8),
    IVec;
des_cbc_ivec(Data) when is_list(Data) ->
    des_cbc_ivec(list_to_binary(Data)).

%%
%% DES - in electronic codebook mode (ECB)
%%
-spec des_ecb_encrypt(iodata(), iodata()) -> binary().
-spec des_ecb_decrypt(iodata(), iodata()) -> binary().

des_ecb_encrypt(Key, Data) ->
    des_ecb_crypt(Key, Data, true).
des_ecb_decrypt(Key, Data) ->
    des_ecb_crypt(Key, Data, false).
des_ecb_crypt(_Key, _Data, _IsEncrypt) -> ?nif_stub.

%%
%% DES3 - in cipher block chaining mode (CBC)
%%
-spec des3_cbc_encrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().
-spec des3_cbc_decrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().

des3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data).
des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, true).

des3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data).
des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, false).

des_ede3_cbc_crypt(_Key1, _Key2, _Key3, _IVec, _Data, _IsEncrypt) -> ?nif_stub.    

%%
%% Blowfish
%%
-spec blowfish_ecb_encrypt(iodata(), iodata()) -> binary().
-spec blowfish_ecb_decrypt(iodata(), iodata()) -> binary().
-spec blowfish_cbc_encrypt(iodata(), binary(), iodata()) -> binary().
-spec blowfish_cbc_decrypt(iodata(), binary(), iodata()) -> binary().
-spec blowfish_cfb64_encrypt(iodata(), binary(), iodata()) -> binary().
-spec blowfish_cfb64_decrypt(iodata(), binary(), iodata()) -> binary().
-spec blowfish_ofb64_encrypt(iodata(), binary(), iodata()) -> binary().

blowfish_ecb_encrypt(Key, Data) ->
    bf_ecb_crypt(Key,Data, true).

blowfish_ecb_decrypt(Key, Data) ->
    bf_ecb_crypt(Key,Data, false).

bf_ecb_crypt(_Key,_Data,_IsEncrypt) -> ?nif_stub.

blowfish_cbc_encrypt(Key, IVec, Data) ->
    bf_cbc_crypt(Key,IVec,Data,true).

blowfish_cbc_decrypt(Key, IVec, Data) ->
    bf_cbc_crypt(Key,IVec,Data,false).

bf_cbc_crypt(_Key,_IVec,_Data,_IsEncrypt) -> ?nif_stub.    

blowfish_cfb64_encrypt(Key, IVec, Data) ->
    bf_cfb64_crypt(Key, IVec, Data, true).

blowfish_cfb64_decrypt(Key, IVec, Data) ->
    bf_cfb64_crypt(Key, IVec, Data, false).

bf_cfb64_crypt(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.

blowfish_ofb64_encrypt(_Key, _IVec, _Data) -> ?nif_stub.

%%
%% AES in cipher feedback mode (CFB)
%%
-spec aes_cfb_128_encrypt(iodata(), binary(), iodata()) -> binary().
-spec aes_cfb_128_decrypt(iodata(), binary(), iodata()) -> binary().

aes_cfb_128_encrypt(Key, IVec, Data) ->
    aes_cfb_128_crypt(Key, IVec, Data, true).

aes_cfb_128_decrypt(Key, IVec, Data) ->
    aes_cfb_128_crypt(Key, IVec, Data, false).

aes_cfb_128_crypt(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.     


%% 
%% RAND - pseudo random numbers using RN_ functions in crypto lib
%%
-spec rand_bytes(non_neg_integer()) -> binary().
-spec rand_uniform(crypto_integer(), crypto_integer()) ->
			  crypto_integer().

rand_bytes(_Bytes) -> ?nif_stub.
rand_bytes(_Bytes, _Topmask, _Bottommask) -> ?nif_stub.

rand_uniform(From,To) when is_binary(From), is_binary(To) ->
    case rand_uniform_nif(From,To) of
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

rand_uniform_nif(_From,_To) -> ?nif_stub.     

%%
%% mod_exp - utility for rsa generation
%%
mod_exp(Base, Exponent, Modulo)
  when is_integer(Base), is_integer(Exponent), is_integer(Modulo) ->
    erlint(mod_exp(mpint(Base), mpint(Exponent), mpint(Modulo)));

mod_exp(Base, Exponent, Modulo) ->
    case mod_exp_nif(Base,Exponent,Modulo) of
	<<Len:32/integer, MSB, Rest/binary>> when MSB > 127 ->
	    <<(Len + 1):32/integer, 0, MSB, Rest/binary>>;
	Whatever ->
	    Whatever
    end.

mod_exp_nif(_Base,_Exp,_Mod) -> ?nif_stub.    

%%
%% DSS, RSA - verify
%%
-spec dss_verify(binary(), binary(), [binary()]) -> boolean().
-spec rsa_verify(binary(), binary(), [binary()]) -> boolean().
-spec rsa_verify(digest_type(), binary(), binary(), [binary()]) ->
			boolean().

%% Key = [P,Q,G,Y]   P,Q,G=DSSParams  Y=PublicKey
dss_verify(_Data,_Signature,_Key) -> ?nif_stub.    

% Key = [E,N]  E=PublicExponent N=PublicModulus
rsa_verify(Data,Signature,Key) ->
    rsa_verify(sha, Data,Signature,Key).
rsa_verify(_Type,_Data,_Signature,_Key) -> ?nif_stub.


%%
%% DSS, RSA - sign
%%
%% Key = [P,Q,G,X]   P,Q,G=DSSParams  X=PrivateKey
-spec dss_sign(binary(), [binary()]) -> binary().
-spec rsa_sign(binary(), [binary()]) -> binary().
-spec rsa_sign(digest_type(), binary(), [binary()]) -> binary().

dss_sign(Data, Key) ->
    case dss_sign_nif(Data,Key) of
	error -> erlang:error(badkey, [Data, Key]);
	Sign -> Sign
    end.

dss_sign_nif(_Data,_Key) -> ?nif_stub.

%% Key = [E,N,D]  E=PublicExponent N=PublicModulus  D=PrivateExponent
rsa_sign(Data,Key) ->
    rsa_sign(sha, Data, Key).
rsa_sign(Type, Data, Key) ->
    case rsa_sign_nif(Type,Data,Key) of
	error -> erlang:error(badkey, [Type,Data,Key]);
	Sign -> Sign
    end.

rsa_sign_nif(_Type,_Data,_Key) -> ?nif_stub.


%%
%%  rsa_public_encrypt
%%  rsa_private_decrypt
-type rsa_padding() :: 'rsa_pkcs1_padding' | 'rsa_pkcs1_oaep_padding' | 'rsa_no_padding'.

-spec rsa_public_encrypt(binary(), [binary()], rsa_padding()) ->
				binary().
-spec rsa_public_decrypt(binary(), [binary()], rsa_padding()) ->
				binary().
-spec rsa_private_encrypt(binary(), [binary()], rsa_padding()) ->
				binary().
-spec rsa_private_decrypt(binary(), [binary()], rsa_padding()) ->
				binary().

%% Binary, Key = [E,N]
rsa_public_encrypt(BinMesg, Key, Padding) ->
    case rsa_public_crypt(BinMesg, Key, Padding, true) of
	error ->
	    erlang:error(encrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

rsa_public_crypt(_BinMsg, _Key, _Padding, _IsEncrypt) -> ?nif_stub.    

%% Binary, Key = [E,N,D]
rsa_private_decrypt(BinMesg, Key, Padding) ->
    case rsa_private_crypt(BinMesg, Key, Padding, false) of
	error ->
	    erlang:error(decrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

rsa_private_crypt(_BinMsg, _Key, _Padding, _IsEncrypt) -> ?nif_stub.    

    
%% Binary, Key = [E,N,D]
rsa_private_encrypt(BinMesg, Key, Padding) ->
    case rsa_private_crypt(BinMesg, Key, Padding, true) of
	error ->
	    erlang:error(encrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%% Binary, Key = [E,N]
rsa_public_decrypt(BinMesg, Key, Padding) ->
    case rsa_public_crypt(BinMesg, Key, Padding, false) of
	error ->
	    erlang:error(decrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.
    
%%
%% AES - with 128 or 256 bit key in cipher block chaining mode (CBC)
%%
-spec aes_cbc_128_encrypt(iodata(), binary(), iodata()) ->
				 binary().
-spec aes_cbc_128_decrypt(iodata(), binary(), iodata()) ->
				 binary().
-spec aes_cbc_256_encrypt(iodata(), binary(), iodata()) ->
				 binary().
-spec aes_cbc_256_decrypt(iodata(), binary(), iodata()) ->
				 binary().

aes_cbc_128_encrypt(Key, IVec, Data) ->
    aes_cbc_crypt(Key, IVec, Data, true).

aes_cbc_128_decrypt(Key, IVec, Data) ->
    aes_cbc_crypt(Key, IVec, Data, false).

aes_cbc_256_encrypt(Key, IVec, Data) ->
    aes_cbc_crypt(Key, IVec, Data, true).

aes_cbc_256_decrypt(Key, IVec, Data) ->
    aes_cbc_crypt(Key, IVec, Data, false).

aes_cbc_crypt(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.

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
-spec exor(iodata(), iodata()) -> binary().

exor(_A, _B) -> ?nif_stub.

%%
%% RC4 - symmetric stream cipher
%%
-spec rc4_encrypt(iodata(), iodata()) -> binary().

rc4_encrypt(_Key, _Data) -> ?nif_stub.
rc4_set_key(_Key) -> ?nif_stub.
rc4_encrypt_with_state(_State, _Data) -> ?nif_stub.

%%
%% RC2 - 40 bits block cipher
%%
rc2_40_cbc_encrypt(Key, IVec, Data) ->
    rc2_40_cbc_crypt(Key,IVec,Data,true).

rc2_40_cbc_decrypt(Key, IVec, Data) ->
    rc2_40_cbc_crypt(Key,IVec,Data,false).

rc2_40_cbc_crypt(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.    

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
dh_generate_parameters(PrimeLen, Generator) ->
    case dh_generate_parameters_nif(PrimeLen, Generator) of
	error -> erlang:error(generation_failed, [PrimeLen,Generator]);
	Ret -> Ret
    end.  

dh_generate_parameters_nif(_PrimeLen, _Generator) -> ?nif_stub.

%% Checks that the DHParameters are ok.
%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
dh_check([_Prime,_Gen]) -> ?nif_stub.

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% PrivKey = mpint()
-spec dh_generate_key([binary()]) -> {binary(),binary()}.
-spec dh_generate_key(binary()|undefined, [binary()]) ->
			     {binary(),binary()}.

dh_generate_key(DHParameters) ->
    dh_generate_key(undefined, DHParameters).
dh_generate_key(PrivateKey, DHParameters) ->
    case dh_generate_key_nif(PrivateKey, DHParameters) of
	error -> erlang:error(generation_failed, [PrivateKey,DHParameters]);
	Res -> Res
    end.

dh_generate_key_nif(_PrivateKey, _DHParameters) -> ?nif_stub.

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% MyPrivKey, OthersPublicKey = mpint() 
-spec dh_compute_key(binary(), binary(), [binary()]) -> binary().

dh_compute_key(OthersPublicKey, MyPrivateKey, DHParameters) ->
    case dh_compute_key_nif(OthersPublicKey,MyPrivateKey,DHParameters) of
	error -> erlang:error(computation_failed, [OthersPublicKey,MyPrivateKey,DHParameters]);
	Ret -> Ret
    end.

dh_compute_key_nif(_OthersPublicKey, _MyPrivateKey, _DHParameters) -> ?nif_stub.

%%
%%  LOCAL FUNCTIONS
%%


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
