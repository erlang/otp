%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2013. All Rights Reserved.
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

-export([start/0, stop/0, info_lib/0, supports/0, version/0, bytes_to_integer/1]).
-export([hash/2, hash_init/1, hash_update/2, hash_final/1]).
-export([sign/4, verify/5]).
-export([generate_key/2, generate_key/3, compute_key/4]).
-export([hmac/3, hmac/4, hmac_init/2, hmac_update/2, hmac_final/1, hmac_final_n/2]).
-export([exor/2, strong_rand_bytes/1, mod_pow/3]).
-export([rand_bytes/1, rand_bytes/3, rand_uniform/2]).
-export([block_encrypt/3, block_decrypt/3, block_encrypt/4, block_decrypt/4]).
-export([next_iv/2, next_iv/3]).
-export([stream_init/2, stream_init/3, stream_encrypt/2, stream_decrypt/2]).
-export([public_encrypt/4, private_decrypt/4]).
-export([private_encrypt/4, public_decrypt/4]).
-export([dh_generate_parameters/2, dh_check/1]). %% Testing see

%% DEPRECATED
%% Replaced by hash_*
-export([md4/1, md4_init/0, md4_update/2, md4_final/1]).
-export([md5/1, md5_init/0, md5_update/2, md5_final/1]).
-export([sha/1, sha_init/0, sha_update/2, sha_final/1]).
-deprecated({md4, 1, next_major_release}).
-deprecated({md5, 1, next_major_release}).
-deprecated({sha, 1, next_major_release}).
-deprecated({md4_init, 0, next_major_release}).
-deprecated({md5_init, 0, next_major_release}).
-deprecated({sha_init, 0, next_major_release}).
-deprecated({md4_update, 2, next_major_release}).
-deprecated({md5_update, 2, next_major_release}).
-deprecated({sha_update, 2, next_major_release}).
-deprecated({md4_final, 1, next_major_release}).
-deprecated({md5_final, 1, next_major_release}).
-deprecated({sha_final, 1, next_major_release}).

%% Replaced by hmac_*
-export([md5_mac/2, md5_mac_96/2, sha_mac/2, sha_mac/3, sha_mac_96/2]).
-deprecated({md5_mac, 2, next_major_release}).
-deprecated({md5_mac_96, 2, next_major_release}).
-deprecated({sha_mac, 2, next_major_release}).
-deprecated({sha_mac, 3, next_major_release}).
-deprecated({sha_mac_96, 2, next_major_release}).

%% Replaced by sign/verify
-export([dss_verify/3, dss_verify/4, rsa_verify/3, rsa_verify/4]).
-export([dss_sign/2, dss_sign/3, rsa_sign/2, rsa_sign/3]).
-deprecated({dss_verify, 3, next_major_release}).
-deprecated({dss_verify, 4, next_major_release}).
-deprecated({rsa_verify, 3, next_major_release}).
-deprecated({rsa_verify, 4, next_major_release}).
-deprecated({dss_sign, 2, next_major_release}).
-deprecated({dss_sign, 3, next_major_release}).
-deprecated({rsa_sign, 2, next_major_release}).
-deprecated({rsa_sign, 3, next_major_release}).

%% Replaced by generate_key
-export([dh_generate_key/1, dh_generate_key/2, dh_compute_key/3]).
-deprecated({dh_generate_key, 1, next_major_release}).
-deprecated({dh_generate_key, 2, next_major_release}).
-deprecated({dh_compute_key, 3, next_major_release}).

%% Replaced by mod_exp_prim and no longer needed
-export([mod_exp/3, mpint/1, erlint/1,  strong_rand_mpint/3]).
-deprecated({mod_exp, 3, next_major_release}).
-deprecated({mpint, 1, next_major_release}).
-deprecated({erlint, 1, next_major_release}).
-deprecated({strong_rand_mpint, 3, next_major_release}).

%% Replaced by block_*
-export([des_cbc_encrypt/3, des_cbc_decrypt/3, des_cbc_ivec/1]).
-export([des3_cbc_encrypt/5, des3_cbc_decrypt/5]).
-export([des_ecb_encrypt/2, des_ecb_decrypt/2]).
-export([des_ede3_cbc_encrypt/5, des_ede3_cbc_decrypt/5]).
-export([des_cfb_encrypt/3, des_cfb_decrypt/3, des_cfb_ivec/2]).
-export([des3_cfb_encrypt/5, des3_cfb_decrypt/5]).
-deprecated({des_cbc_encrypt, 3, next_major_release}).
-deprecated({des_cbc_decrypt, 3, next_major_release}).
-deprecated({des_cbc_ivec, 1, next_major_release}).
-deprecated({des3_cbc_encrypt, 5, next_major_release}).
-deprecated({des3_cbc_decrypt, 5, next_major_release}).
-deprecated({des_ecb_encrypt, 2, next_major_release}).
-deprecated({des_ecb_decrypt, 2, next_major_release}).
-deprecated({des_ede3_cbc_encrypt, 5, next_major_release}).
-deprecated({des_ede3_cbc_decrypt, 5, next_major_release}).
-deprecated({des_cfb_encrypt, 3, next_major_release}).
-deprecated({des_cfb_decrypt, 3, next_major_release}).
-deprecated({des_cfb_ivec, 2, next_major_release}).
-deprecated({des3_cfb_encrypt, 5, next_major_release}).
-deprecated({des3_cfb_decrypt, 5, next_major_release}).
-export([blowfish_ecb_encrypt/2, blowfish_ecb_decrypt/2]).
-export([blowfish_cbc_encrypt/3, blowfish_cbc_decrypt/3]).
-export([blowfish_cfb64_encrypt/3, blowfish_cfb64_decrypt/3]).
-export([blowfish_ofb64_encrypt/3]).
-deprecated({blowfish_ecb_encrypt, 2, next_major_release}).
-deprecated({blowfish_ecb_decrypt, 2, next_major_release}).
-deprecated({blowfish_cbc_encrypt, 3, next_major_release}).
-deprecated({blowfish_cbc_decrypt, 3, next_major_release}).
-deprecated({blowfish_cfb64_encrypt, 3, next_major_release}).
-deprecated({blowfish_cfb64_decrypt, 3, next_major_release}).
-deprecated({blowfish_ofb64_encrypt, 3, next_major_release}).
-export([aes_cfb_128_encrypt/3, aes_cfb_128_decrypt/3]).
-export([aes_cbc_128_encrypt/3, aes_cbc_128_decrypt/3]).
-export([aes_cbc_256_encrypt/3, aes_cbc_256_decrypt/3]).
-export([aes_cbc_ivec/1]).
-deprecated({aes_cfb_128_encrypt, 3, next_major_release}).
-deprecated({aes_cfb_128_decrypt, 3, next_major_release}).
-deprecated({aes_cbc_128_encrypt, 3, next_major_release}).
-deprecated({aes_cbc_128_decrypt, 3, next_major_release}).
-deprecated({aes_cbc_256_encrypt, 3, next_major_release}).
-deprecated({aes_cbc_256_decrypt, 3, next_major_release}).
-deprecated({aes_cbc_ivec, 1, next_major_release}).
-export([rc2_cbc_encrypt/3, rc2_cbc_decrypt/3]).
-export([rc2_40_cbc_encrypt/3, rc2_40_cbc_decrypt/3]).
-deprecated({rc2_cbc_encrypt, 3, next_major_release}).
-deprecated({rc2_cbc_decrypt, 3, next_major_release}).
%% allready replaced by above!
-deprecated({rc2_40_cbc_encrypt, 3, next_major_release}).
-deprecated({rc2_40_cbc_decrypt, 3, next_major_release}).

%% Replaced by stream_*
-export([aes_ctr_stream_init/2, aes_ctr_stream_encrypt/2, aes_ctr_stream_decrypt/2]).
-export([rc4_set_key/1, rc4_encrypt_with_state/2]).
-deprecated({aes_ctr_stream_init, 2, next_major_release}).
-deprecated({aes_ctr_stream_encrypt, 2, next_major_release}).
-deprecated({aes_ctr_stream_decrypt, 2, next_major_release}).
-deprecated({rc4_set_key, 1, next_major_release}).
-deprecated({rc4_encrypt_with_state, 2, next_major_release}).

%% Not needed special case of stream_*
-export([aes_ctr_encrypt/3, aes_ctr_decrypt/3, rc4_encrypt/2]).
-deprecated({aes_ctr_encrypt, 3, next_major_release}).
-deprecated({aes_ctr_decrypt, 3, next_major_release}).
-deprecated({rc4_encrypt, 2, next_major_release}).

%% Replace by public/private_encrypt/decrypt
-export([rsa_public_encrypt/3, rsa_private_decrypt/3]).
-export([rsa_private_encrypt/3, rsa_public_decrypt/3]).
-deprecated({rsa_public_encrypt, 3, next_major_release}).
-deprecated({rsa_private_decrypt, 3, next_major_release}).
-deprecated({rsa_public_decrypt, 3, next_major_release}).
-deprecated({rsa_private_encrypt, 3, next_major_release}).

%% Replaced by crypto:module_info()
-export([info/0]).
-deprecated({info, 0, next_major_release}).

%% This should correspond to the similar macro in crypto.c
-define(MAX_BYTES_TO_NIF, 20000). %%  Current value is: erlang:system_info(context_reductions) * 10

-type mpint() :: binary().
-type rsa_digest_type() :: 'md5' | 'sha' | 'sha224' | 'sha256' | 'sha384' | 'sha512'.
-type dss_digest_type() :: 'none' | 'sha'.
%%-type ecdsa_digest_type() :: 'md5' | 'sha' | 'sha256' | 'sha384' | 'sha512'.
-type data_or_digest() :: binary() | {digest, binary()}.
-type crypto_integer() :: binary() | integer().
%%-type ec_named_curve() :: atom().
%%-type ec_point() :: crypto_integer().
%%-type ec_basis() :: {tpbasis, K :: non_neg_integer()} | {ppbasis, K1 :: non_neg_integer(), K2 :: non_neg_integer(), K3 :: non_neg_integer()} | onbasis.
%%-type ec_field() :: {prime_field, Prime :: integer()} | {characteristic_two_field, M :: integer(), Basis :: ec_basis()}.
%%-type ec_prime() :: {A :: crypto_integer(), B :: crypto_integer(), Seed :: binary() | none}.
%%-type ec_curve_spec() :: {Field :: ec_field(), Prime :: ec_prime(), Point :: crypto_integer(), Order :: integer(), CoFactor :: none | integer()}.
%%-type ec_curve() :: ec_named_curve() | ec_curve_spec().
%%-type ec_key() :: {Curve :: ec_curve(), PrivKey :: binary() | undefined, PubKey :: ec_point() | undefined}.

-on_load(on_load/0).
-define(CRYPTO_NIF_VSN,201).

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
%% Crypto app version history:
%% (no version): Driver implementation
%% 2.0         : NIF implementation, requires OTP R14
version() -> ?CRYPTO_VSN.

start() ->
    application:start(crypto).

stop() ->
    application:stop(crypto).

supports()->
    Algs = algorithms(),
    PubKeyAlgs = 
	case lists:member(ec, Algs) of
	    true ->
		{public_keys, [rsa, dss, ecdsa, dh, srp, ecdh]};
	    false ->
		{public_keys, [rsa, dss, dh, srp]}
	end,
    [{hashs, Algs -- [ec]},
     {ciphers, [des_cbc, des_cfb,  des3_cbc, des3_cbf, des_ede3, blowfish_cbc,
		blowfish_cfb64, blowfish_ofb64, blowfish_ecb, aes_cbc128, aes_cfb128,
		aes_cbc256, aes_ige256, rc2_cbc, aes_ctr, rc4
	       ]},
     PubKeyAlgs
    ].

info_lib() -> ?nif_stub.

-spec hash(_, iodata()) -> binary().

hash(Hash, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxByts = max_bytes(),
    hash(Hash, Data, erlang:byte_size(Data), MaxByts, initial).

-spec hash_init('md5'|'md4'|'ripemd160'|
                'sha'|'sha224'|'sha256'|'sha384'|'sha512') -> any().

hash_init(md5)       -> {md5, md5_init()};
hash_init(md4)       -> {md4, md4_init()};
hash_init(sha)       -> {sha, sha_init()};
hash_init(ripemd160) -> {ripemd160, ripemd160_init()};
hash_init(sha224)    -> {sha224, sha224_init()};
hash_init(sha256)    -> {sha256, sha256_init()};
hash_init(sha384)    -> {sha384, sha384_init()};
hash_init(sha512)    -> {sha512, sha512_init()}.

-spec hash_update(_, iodata()) -> any().

hash_update(State, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxBytes = max_bytes(),
    hash_update(State, Data, erlang:byte_size(Data), MaxBytes).

-spec hash_final(_) -> binary().

hash_final({md5,Context})       -> md5_final(Context);
hash_final({md4,Context})       -> md4_final(Context);
hash_final({sha,Context})       -> sha_final(Context);
hash_final({ripemd160,Context}) -> ripemd160_final(Context);
hash_final({sha224,Context})    -> sha224_final(Context);
hash_final({sha256,Context})    -> sha256_final(Context);
hash_final({sha384,Context})    -> sha384_final(Context);
hash_final({sha512,Context})    -> sha512_final(Context).


-spec hmac(_, iodata(), iodata()) -> binary().
-spec hmac(_, iodata(), iodata(), integer()) -> binary().
-spec hmac_init(atom(), iodata()) -> binary().
-spec hmac_update(binary(), iodata()) -> binary().
-spec hmac_final(binary()) -> binary().
-spec hmac_final_n(binary(), integer()) -> binary().

hmac(Type, Key, Data0) ->
    Data = iolist_to_binary(Data0),
    hmac(Type, Key, Data, undefined, erlang:byte_size(Data), max_bytes(), initial).
hmac(Type, Key, Data0, MacSize) ->
    Data = iolist_to_binary(Data0),
    hmac(Type, Key, Data, MacSize, erlang:byte_size(Data), max_bytes(), initial).


hmac_init(_Type, _Key) -> ?nif_stub.

hmac_update(State, Data0) ->
    Data = iolist_to_binary(Data0),
    hmac_update(State, Data, erlang:byte_size(Data), max_bytes()).
hmac_final(_Context) -> ? nif_stub.
hmac_final_n(_Context, _HashLen) -> ? nif_stub.

%% Ecrypt/decrypt %%%

-spec block_encrypt(des_cbc | des_cfb | des3_cbc | des3_cbf | des_ede3 | blowfish_cbc |
		    blowfish_cfb64 | aes_cbc128 | aes_cfb128 | aes_cbc256 | rc2_cbc,
		    Key::iodata(), Ivec::binary(), Data::iodata()) -> binary().

block_encrypt(des_cbc, Key, Ivec, Data) ->
    des_cbc_encrypt(Key, Ivec, Data);
block_encrypt(des_cfb, Key, Ivec, Data) ->
    des_cfb_encrypt(Key, Ivec, Data);
block_encrypt(des3_cbc, [Key1, Key2, Key3], Ivec, Data) ->
    des3_cbc_encrypt(Key1, Key2, Key3, Ivec, Data);
block_encrypt(des3_cbf, [Key1, Key2, Key3], Ivec, Data) ->
    des3_cfb_encrypt(Key1, Key2, Key3, Ivec, Data);
block_encrypt(des_ede3, [Key1, Key2, Key3], Ivec, Data) ->
    des_ede3_cbc_encrypt(Key1, Key2, Key3, Ivec, Data);
block_encrypt(blowfish_cbc, Key, Ivec, Data) ->
    blowfish_cbc_encrypt(Key, Ivec, Data);
block_encrypt(blowfish_cfb64, Key, Ivec, Data) ->
    blowfish_cfb64_encrypt(Key, Ivec, Data);
block_encrypt(blowfish_ofb64, Key, Ivec, Data) ->
    blowfish_ofb64_encrypt(Key, Ivec, Data);
block_encrypt(aes_cbc128, Key, Ivec, Data) ->
    aes_cbc_128_encrypt(Key, Ivec, Data);
block_encrypt(aes_cbc256, Key, Ivec, Data) ->
    aes_cbc_256_encrypt(Key, Ivec, Data);
block_encrypt(aes_ige256, Key, Ivec, Data) ->
    aes_ige_256_encrypt(Key, Ivec, Data);
block_encrypt(aes_cfb128, Key, Ivec, Data) ->
    aes_cfb_128_encrypt(Key, Ivec, Data);
block_encrypt(rc2_cbc, Key, Ivec, Data) ->
    rc2_cbc_encrypt(Key, Ivec, Data).

-spec block_decrypt(des_cbc | des_cfb | des3_cbc | des3_cbf | des_ede3 | blowfish_cbc |
	      blowfish_cfb64 | blowfish_ofb64  | aes_cbc128 | aes_cbc256 | aes_ige256 |
          aes_cfb128 | rc2_cbc,
	      Key::iodata(), Ivec::binary(), Data::iodata()) -> binary().

block_decrypt(des_cbc, Key, Ivec, Data) ->
    des_cbc_decrypt(Key, Ivec, Data);
block_decrypt(des_cfb, Key, Ivec, Data) ->
    des_cfb_decrypt(Key, Ivec, Data);
block_decrypt(des3_cbc, [Key1, Key2, Key3], Ivec, Data) ->
    des3_cbc_decrypt(Key1, Key2, Key3, Ivec, Data);
block_decrypt(des3_cbf, [Key1, Key2, Key3], Ivec, Data) ->
    des3_cfb_decrypt(Key1, Key2, Key3, Ivec, Data);
block_decrypt(des_ede3, [Key1, Key2, Key3], Ivec, Data) ->
    des_ede3_cbc_decrypt(Key1, Key2, Key3, Ivec, Data);
block_decrypt(blowfish_cbc, Key, Ivec, Data) ->
    blowfish_cbc_decrypt(Key, Ivec, Data);
block_decrypt(blowfish_cfb64, Key, Ivec, Data) ->
    blowfish_cfb64_decrypt(Key, Ivec, Data);
block_decrypt(blowfish_ofb64, Key, Ivec, Data) ->
    blowfish_ofb64_decrypt(Key, Ivec, Data);
block_decrypt(aes_cbc128, Key, Ivec, Data) ->
    aes_cbc_128_decrypt(Key, Ivec, Data);
block_decrypt(aes_cbc256, Key, Ivec, Data) ->
    aes_cbc_256_decrypt(Key, Ivec, Data);
block_decrypt(aes_ige256, Key, Ivec, Data) ->
    aes_ige_256_decrypt(Key, Ivec, Data);
block_decrypt(aes_cfb128, Key, Ivec, Data) ->
    aes_cfb_128_decrypt(Key, Ivec, Data);
block_decrypt(rc2_cbc, Key, Ivec, Data) ->
    rc2_cbc_decrypt(Key, Ivec, Data).

-spec block_encrypt(des_ecb | blowfish_ecb, Key::iodata(), Data::iodata()) -> binary().

block_encrypt(des_ecb, Key, Data) ->
    des_ecb_encrypt(Key, Data);
block_encrypt(blowfish_ecb, Key, Data) ->
    blowfish_ecb_encrypt(Key, Data).

-spec block_decrypt(des_ecb | blowfish_ecb, Key::iodata(), Data::iodata()) -> binary().

block_decrypt(des_ecb, Key, Data) ->
    des_ecb_decrypt(Key, Data);
block_decrypt(blowfish_ecb, Key, Data) ->
    blowfish_ecb_decrypt(Key, Data).

-spec next_iv(des_cbc | des3_cbc | aes_cbc | aes_ige, Data::iodata()) -> binary().

next_iv(des_cbc, Data) ->
    des_cbc_ivec(Data);
next_iv(des3_cbc, Data) ->
    des_cbc_ivec(Data);
next_iv(aes_cbc, Data) ->
    aes_cbc_ivec(Data);
next_iv(aes_ige, Data) ->
    aes_ige_ivec(Data).

-spec next_iv(des_cfb, Data::iodata(), Ivec::binary()) -> binary().

next_iv(des_cfb, Data, Ivec) ->
    des_cfb_ivec(Ivec, Data);
next_iv(Type, Data, _Ivec) ->
    next_iv(Type, Data).

stream_init(aes_ctr, Key, Ivec) ->
    {aes_ctr, aes_ctr_stream_init(Key, Ivec)}.
stream_init(rc4, Key) ->
    {rc4, rc4_set_key(Key)}.

stream_encrypt(State, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxByts = max_bytes(),
    stream_crypt(fun do_stream_encrypt/2, State, Data, erlang:byte_size(Data), MaxByts, []).

stream_decrypt(State, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxByts = max_bytes(),
    stream_crypt(fun do_stream_decrypt/2, State, Data, erlang:byte_size(Data), MaxByts, []).

%%
%% RAND - pseudo random numbers using RN_ functions in crypto lib
%%
-spec rand_bytes(non_neg_integer()) -> binary().
-spec strong_rand_bytes(non_neg_integer()) -> binary().
-spec rand_uniform(crypto_integer(), crypto_integer()) ->
			  crypto_integer().

rand_bytes(_Bytes) -> ?nif_stub.

strong_rand_bytes(Bytes) ->
    case strong_rand_bytes_nif(Bytes) of
        false -> erlang:error(low_entropy);
        Bin -> Bin
    end.
strong_rand_bytes_nif(_Bytes) -> ?nif_stub.

rand_bytes(_Bytes, _Topmask, _Bottommask) -> ?nif_stub.


rand_uniform(From,To) when is_binary(From), is_binary(To) ->
    case rand_uniform_nif(From,To) of
	<<Len:32/integer, MSB, Rest/binary>> when MSB > 127 ->
	    <<(Len + 1):32/integer, 0, MSB, Rest/binary>>;
	Whatever ->
	    Whatever
    end;
rand_uniform(From,To) when is_integer(From),is_integer(To) ->
    if From < 0 ->
	    rand_uniform_pos(0, To - From) + From;
       true ->
	    rand_uniform_pos(From, To)
    end.

rand_uniform_pos(From,To) when From < To ->
    BinFrom = mpint(From),
    BinTo = mpint(To),
    case rand_uniform(BinFrom, BinTo) of
        Result when is_binary(Result) ->
            erlint(Result);
        Other ->
            Other
    end;
rand_uniform_pos(_,_) ->
    error(badarg).

rand_uniform_nif(_From,_To) -> ?nif_stub.


-spec mod_pow(binary()|integer(), binary()|integer(), binary()|integer()) -> binary() | error.
mod_pow(Base, Exponent, Prime) ->
    case mod_exp_nif(ensure_int_as_bin(Base), ensure_int_as_bin(Exponent), ensure_int_as_bin(Prime), 0) of
	<<0>> -> error;
	R -> R
    end.
verify(dss, none, Data, Signature, Key) when is_binary(Data) ->
    verify(dss, sha, {digest, Data}, Signature, Key);
verify(Alg, Type, Data, Signature, Key) when is_binary(Data) ->
    verify(Alg, Type,  {digest, hash(Type, Data)}, Signature, Key);
verify(dss, Type, Data, Signature, Key) ->
    dss_verify_nif(Type, Data, Signature, map_ensure_int_as_bin(Key));
verify(rsa, Type, DataOrDigest, Signature, Key) ->
    case rsa_verify_nif(Type, DataOrDigest, Signature, map_ensure_int_as_bin(Key)) of
	notsup -> erlang:error(notsup);
	Bool -> Bool
    end;
verify(ecdsa, Type, DataOrDigest, Signature, [Key, Curve]) ->
    case ecdsa_verify_nif(Type, DataOrDigest, Signature, nif_curve_params(Curve), ensure_int_as_bin(Key)) of
	notsup -> erlang:error(notsup);
	Bool -> Bool
    end.
sign(dss, none, Data, Key) when is_binary(Data) ->
    sign(dss, sha, {digest, Data}, Key);
sign(Alg, Type, Data, Key) when is_binary(Data) ->
    sign(Alg, Type, {digest, hash(Type, Data)}, Key);
sign(rsa, Type, DataOrDigest, Key) ->
    case rsa_sign_nif(Type, DataOrDigest, map_ensure_int_as_bin(Key)) of
	error -> erlang:error(badkey, [Type,DataOrDigest,Key]);
	Sign -> Sign
    end;
sign(dss, Type, DataOrDigest, Key) ->
    case dss_sign_nif(Type, DataOrDigest, map_ensure_int_as_bin(Key)) of
	error -> erlang:error(badkey, [DataOrDigest, Key]);
	Sign -> Sign
    end;
sign(ecdsa, Type, DataOrDigest, [Key, Curve]) ->
    case ecdsa_sign_nif(Type, DataOrDigest, nif_curve_params(Curve), ensure_int_as_bin(Key)) of
	error -> erlang:error(badkey, [Type,DataOrDigest,Key]);
	Sign -> Sign
    end.

-spec public_encrypt(rsa, binary(), [binary()], rsa_padding()) ->
				binary().
-spec public_decrypt(rsa, binary(), [integer() | binary()], rsa_padding()) ->
				binary().
-spec private_encrypt(rsa, binary(), [integer() | binary()], rsa_padding()) ->
				binary().
-spec private_decrypt(rsa, binary(), [integer() | binary()], rsa_padding()) ->
				binary().

public_encrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_public_crypt(BinMesg,  map_ensure_int_as_bin(Key), Padding, true) of
	error ->
	    erlang:error(encrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%% Binary, Key = [E,N,D]
private_decrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_private_crypt(BinMesg, map_ensure_int_as_bin(Key), Padding, false) of
	error ->
	    erlang:error(decrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.


%% Binary, Key = [E,N,D]
private_encrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_private_crypt(BinMesg, map_ensure_int_as_bin(Key), Padding, true) of
	error ->
	    erlang:error(encrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%% Binary, Key = [E,N]
public_decrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_public_crypt(BinMesg, map_ensure_int_as_bin(Key), Padding, false) of
	error ->
	    erlang:error(decrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%%
%% XOR - xor to iolists and return a binary
%% NB doesn't check that they are the same size, just concatenates
%% them and sends them to the driver
%%
-spec exor(iodata(), iodata()) -> binary().

exor(Bin1, Bin2) ->
    Data1 = iolist_to_binary(Bin1),
    Data2 = iolist_to_binary(Bin2),
    MaxBytes = max_bytes(),
    exor(Data1, Data2, erlang:byte_size(Data1), MaxBytes, []).

generate_key(Type, Params) ->
    generate_key(Type, Params, undefined).

generate_key(dh, DHParameters, PrivateKey) ->
    dh_generate_key_nif(ensure_int_as_bin(PrivateKey),
			map_ensure_int_as_bin(DHParameters), 0);

generate_key(srp, {host, [Verifier, Generator, Prime, Version]}, PrivArg)
  when is_binary(Verifier), is_binary(Generator), is_binary(Prime), is_atom(Version) ->
    Private = case PrivArg of
		  undefined -> random_bytes(32);
		  _ -> ensure_int_as_bin(PrivArg)
	      end,
    host_srp_gen_key(Private, Verifier, Generator, Prime, Version);

generate_key(srp, {user, [Generator, Prime, Version]}, PrivateArg)
  when is_binary(Generator), is_binary(Prime), is_atom(Version) ->
    Private = case PrivateArg of
		  undefined -> random_bytes(32);
		  _ -> PrivateArg
	      end,
    user_srp_gen_key(Private, Generator, Prime);

generate_key(ecdh, Curve, undefined) ->
    ec_key_generate(Curve).


compute_key(dh, OthersPublicKey, MyPrivateKey, DHParameters) ->
    case dh_compute_key_nif(ensure_int_as_bin(OthersPublicKey),
			    ensure_int_as_bin(MyPrivateKey),
			    map_ensure_int_as_bin(DHParameters)) of
	error -> erlang:error(computation_failed,
			      [OthersPublicKey,MyPrivateKey,DHParameters]);
	Ret -> Ret
    end;

compute_key(srp, HostPublic, {UserPublic, UserPrivate},
	    {user, [DerivedKey, Prime, Generator, Version | ScramblerArg]}) when
      is_binary(Prime),
      is_binary(Generator),
      is_atom(Version) ->
    HostPubBin = ensure_int_as_bin(HostPublic),
    Multiplier = srp_multiplier(Version, Generator, Prime),
    Scrambler = case ScramblerArg of
		    [] -> srp_scrambler(Version, ensure_int_as_bin(UserPublic),
					HostPubBin, Prime);
		    [S] -> S
		end,
    srp_user_secret_nif(ensure_int_as_bin(UserPrivate), Scrambler, HostPubBin,
			Multiplier, Generator, DerivedKey, Prime);

compute_key(srp, UserPublic, {HostPublic, HostPrivate},
	    {host,[Verifier, Prime, Version | ScramblerArg]}) when
      is_binary(Verifier),
      is_binary(Prime),
      is_atom(Version) ->
    UserPubBin = ensure_int_as_bin(UserPublic),
    Scrambler = case ScramblerArg of
		    [] -> srp_scrambler(Version, UserPubBin, ensure_int_as_bin(HostPublic), Prime);
		    [S] -> S
		end,
    srp_host_secret_nif(Verifier, ensure_int_as_bin(HostPrivate), Scrambler,
			UserPubBin, Prime);

compute_key(ecdh, Others, My, Curve) ->
    ecdh_compute_key_nif(ensure_int_as_bin(Others),
			 nif_curve_params(Curve),
			 ensure_int_as_bin(My)).


random_bytes(N) ->
    try strong_rand_bytes(N) of
	RandBytes ->
	    RandBytes
    catch
	error:low_entropy ->
	    rand_bytes(N)
    end.

%%--------------------------------------------------------------------
%%% On load
%%--------------------------------------------------------------------

on_load() ->
    LibBaseName = "crypto",
    PrivDir = code:priv_dir(crypto),
    LibName = case erlang:system_info(build_type) of
		  opt ->
		      LibBaseName;
		  Type ->
		      LibTypeName = LibBaseName ++ "."  ++ atom_to_list(Type),
		      case (filelib:wildcard(
			      filename:join(
				[PrivDir,
				 "lib",
				 LibTypeName ++ "*"])) /= []) orelse
			  (filelib:wildcard(
			     filename:join(
			       [PrivDir,
				"lib",
				erlang:system_info(system_architecture),
				LibTypeName ++ "*"])) /= []) of
			  true -> LibTypeName;
			  false -> LibBaseName
		      end
	      end,
    Lib = filename:join([PrivDir, "lib", LibName]),
    Status = case erlang:load_nif(Lib, {?CRYPTO_NIF_VSN,Lib}) of
		 ok -> ok;
		 {error, {load_failed, _}}=Error1 ->
		     ArchLibDir =
			 filename:join([PrivDir, "lib",
					erlang:system_info(system_architecture)]),
		     Candidate =
			 filelib:wildcard(filename:join([ArchLibDir,LibName ++ "*" ])),
		     case Candidate of
			 [] -> Error1;
			 _ ->
			     ArchLib = filename:join([ArchLibDir, LibName]),
			     erlang:load_nif(ArchLib, {?CRYPTO_NIF_VSN,ArchLib})
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
%%--------------------------------------------------------------------
%%% Internal functions (some internal API functions are part of the deprecated API)
%%--------------------------------------------------------------------
max_bytes() ->
    ?MAX_BYTES_TO_NIF.

%% HASH --------------------------------------------------------------------
hash(Hash, Data, Size, Max, initial) when Size =< Max ->
    do_hash(Hash, Data);
hash(State0, Data, Size, Max, continue) when Size =< Max ->
    State = do_hash_update(State0, Data),
    hash_final(State);
hash(Hash, Data, _Size, Max, initial) ->
    <<Increment:Max/binary, Rest/binary>> = Data,
    State0 = hash_init(Hash),
    State = do_hash_update(State0, Increment),
    hash(State, Rest, erlang:byte_size(Rest), max_bytes(), continue);
hash(State0, Data, _Size, MaxByts, continue) ->
    <<Increment:MaxByts/binary, Rest/binary>> = Data,
    State = do_hash_update(State0, Increment),
    hash(State, Rest, erlang:byte_size(Rest), max_bytes(), continue).

do_hash(md5, Data)          -> md5(Data);
do_hash(md4, Data)          -> md4(Data);
do_hash(sha, Data)          -> sha(Data);
do_hash(ripemd160, Data)    -> ripemd160(Data);
do_hash(sha224, Data)       -> sha224(Data);
do_hash(sha256, Data)       -> sha256(Data);
do_hash(sha384, Data)       -> sha384(Data);
do_hash(sha512, Data)       -> sha512(Data).

hash_update(State, Data, Size, MaxBytes)  when Size =< MaxBytes ->
    do_hash_update(State, Data);
hash_update(State0, Data, _, MaxBytes) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = do_hash_update(State0, Increment),
    hash_update(State, Rest, erlang:byte_size(Rest), MaxBytes).

do_hash_update({md5,Context}, Data)       -> {md5, md5_update(Context,Data)};
do_hash_update({md4,Context}, Data)       -> {md4, md4_update(Context,Data)};
do_hash_update({sha,Context}, Data)       -> {sha, sha_update(Context,Data)};
do_hash_update({ripemd160,Context}, Data) -> {ripemd160, ripemd160_update(Context,Data)};
do_hash_update({sha224,Context}, Data)    -> {sha224, sha224_update(Context,Data)};
do_hash_update({sha256,Context}, Data)    -> {sha256, sha256_update(Context,Data)};
do_hash_update({sha384,Context}, Data)    -> {sha384, sha384_update(Context,Data)};
do_hash_update({sha512,Context}, Data)    -> {sha512, sha512_update(Context,Data)}.


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
%%  RIPEMD160
%%

-spec ripemd160(iodata()) -> binary().
-spec ripemd160_init() -> binary().
-spec ripemd160_update(binary(), iodata()) -> binary().
-spec ripemd160_final(binary()) -> binary().

ripemd160(_Data) -> ?nif_stub.
ripemd160_init() -> ?nif_stub.
ripemd160_update(_Context, _Data) -> ?nif_stub.
ripemd160_final(_Context) -> ?nif_stub.

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

%
%% SHA224
%%
-spec sha224(iodata()) -> binary().
-spec sha224_init() -> binary().
-spec sha224_update(binary(), iodata()) -> binary().
-spec sha224_final(binary()) -> binary().

sha224(Data) ->
    case sha224_nif(Data) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha224_init() ->
    case sha224_init_nif() of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha224_update(Context, Data) ->
    case sha224_update_nif(Context, Data) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha224_final(Context) ->
    case sha224_final_nif(Context) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.

sha224_nif(_Data) -> ?nif_stub.
sha224_init_nif() -> ?nif_stub.
sha224_update_nif(_Context, _Data) -> ?nif_stub.
sha224_final_nif(_Context) -> ?nif_stub.

%
%% SHA256
%%
-spec sha256(iodata()) -> binary().
-spec sha256_init() -> binary().
-spec sha256_update(binary(), iodata()) -> binary().
-spec sha256_final(binary()) -> binary().

sha256(Data) ->
    case sha256_nif(Data) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha256_init() ->
    case sha256_init_nif() of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha256_update(Context, Data) ->
    case sha256_update_nif(Context, Data) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha256_final(Context) ->
    case sha256_final_nif(Context) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.

sha256_nif(_Data) -> ?nif_stub.
sha256_init_nif() -> ?nif_stub.
sha256_update_nif(_Context, _Data) -> ?nif_stub.
sha256_final_nif(_Context) -> ?nif_stub.

%
%% SHA384
%%
-spec sha384(iodata()) -> binary().
-spec sha384_init() -> binary().
-spec sha384_update(binary(), iodata()) -> binary().
-spec sha384_final(binary()) -> binary().

sha384(Data) ->
    case sha384_nif(Data) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha384_init() ->
    case sha384_init_nif() of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha384_update(Context, Data) ->
    case sha384_update_nif(Context, Data) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha384_final(Context) ->
    case sha384_final_nif(Context) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.

sha384_nif(_Data) -> ?nif_stub.
sha384_init_nif() -> ?nif_stub.
sha384_update_nif(_Context, _Data) -> ?nif_stub.
sha384_final_nif(_Context) -> ?nif_stub.

%
%% SHA512
%%
-spec sha512(iodata()) -> binary().
-spec sha512_init() -> binary().
-spec sha512_update(binary(), iodata()) -> binary().
-spec sha512_final(binary()) -> binary().

sha512(Data) ->
    case sha512_nif(Data) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha512_init() ->
    case sha512_init_nif() of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha512_update(Context, Data) ->
    case sha512_update_nif(Context, Data) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.
sha512_final(Context) ->
    case sha512_final_nif(Context) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.

sha512_nif(_Data) -> ?nif_stub.
sha512_init_nif() -> ?nif_stub.
sha512_update_nif(_Context, _Data) -> ?nif_stub.
sha512_final_nif(_Context) -> ?nif_stub.

%% HMAC --------------------------------------------------------------------

hmac(Type, Key, Data, MacSize, Size, MaxBytes, initial) when Size =< MaxBytes ->
    case MacSize of
	undefined ->
	    do_hmac(Type, Key, Data);
	_ ->
	    do_hmac(Type, Key, Data, MacSize)
    end;
hmac(Type, Key, Data, MacSize, _, MaxBytes, initial) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State0 = hmac_init(Type, Key),
    State = hmac_update(State0, Increment),
    hmac(State, Rest, MacSize, erlang:byte_size(Rest), max_bytes(), continue).
hmac(State0, Data, MacSize, Size, MaxBytes, continue) when Size =< MaxBytes ->
    State = hmac_update(State0, Data),
    case MacSize of
	undefined ->
	    hmac_final(State);
	 _ ->
	    hmac_final_n(State, MacSize)
	end;
hmac(State0, Data, MacSize, _Size, MaxBytes, continue) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = hmac_update(State0, Increment),
    hmac(State, Rest, MacSize, erlang:byte_size(Rest), max_bytes(), continue).

hmac_update(State, Data, Size, MaxBytes)  when Size =< MaxBytes ->
    do_hmac_update(State, Data);
hmac_update(State0, Data, _, MaxBytes) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = do_hmac_update(State0, Increment),
    hmac_update(State, Rest, erlang:byte_size(Rest), MaxBytes).

do_hmac(md5, Key, Data)    -> md5_mac(Key, Data);
do_hmac(sha, Key, Data)    -> sha_mac(Key, Data);
do_hmac(sha224, Key, Data) -> sha224_mac(Key, Data);
do_hmac(sha256, Key, Data) -> sha256_mac(Key, Data);
do_hmac(sha384, Key, Data) -> sha384_mac(Key, Data);
do_hmac(sha512, Key, Data) -> sha512_mac(Key, Data).

do_hmac(md5, Key, Data, Size)    -> md5_mac_n(Key, Data, Size);
do_hmac(sha, Key, Data, Size)    -> sha_mac_n(Key, Data, Size);
do_hmac(sha224, Key, Data, Size) -> sha224_mac(Key, Data, Size);
do_hmac(sha256, Key, Data, Size) -> sha256_mac(Key, Data, Size);
do_hmac(sha384, Key, Data, Size) -> sha384_mac(Key, Data, Size);
do_hmac(sha512, Key, Data, Size) -> sha512_mac(Key, Data, Size).

do_hmac_update(_Context, _Data) -> ? nif_stub.

%%
%%  MD5_MAC
%%
-spec md5_mac(iodata(), iodata()) -> binary().
-spec md5_mac_96(iodata(), iodata()) -> binary().

md5_mac(Key, Data) ->
    md5_mac_n(Key,Data,16).

md5_mac_96(Key, Data) ->
    md5_mac_n(Key,Data,12).

md5_mac_n(_Key,_Data,_MacSz) -> ?nif_stub.
    
%%
%%  SHA_MAC
%%
-spec sha_mac(iodata(), iodata()) -> binary().
-spec sha_mac_96(iodata(), iodata()) -> binary().

sha_mac(Key, Data) ->
    sha_mac_n(Key,Data,20).

sha_mac(Key, Data, Size) ->
    sha_mac_n(Key, Data, Size).

sha_mac_96(Key, Data) ->
    sha_mac_n(Key,Data,12).

sha_mac_n(_Key,_Data,_MacSz) -> ?nif_stub.

%%
%%  SHA224_MAC
%%
-spec sha224_mac(iodata(), iodata()) -> binary().

sha224_mac(Key, Data) ->
    sha224_mac(Key, Data, 224 div 8).

sha224_mac(Key, Data, Size) ->
   case sha224_mac_nif(Key, Data, Size) of
       notsup -> erlang:error(notsup);
       Bin -> Bin
   end.

sha224_mac_nif(_Key,_Data,_MacSz) -> ?nif_stub.

%%
%%  SHA256_MAC
%%
-spec sha256_mac(iodata(), iodata()) -> binary().

sha256_mac(Key, Data) ->
    sha256_mac(Key, Data, 256 div 8).

sha256_mac(Key, Data, Size) ->
   case sha256_mac_nif(Key, Data, Size) of
       notsup -> erlang:error(notsup);
       Bin -> Bin
   end.

sha256_mac_nif(_Key,_Data,_MacSz) -> ?nif_stub.

%%
%%  SHA384_MAC
%%
-spec sha384_mac(iodata(), iodata()) -> binary().

sha384_mac(Key, Data) ->
    sha384_mac(Key, Data, 384 div 8).

sha384_mac(Key, Data, Size) ->
   case sha384_mac_nif(Key, Data, Size) of
       notsup -> erlang:error(notsup);
       Bin -> Bin
   end.

sha384_mac_nif(_Key,_Data,_MacSz) -> ?nif_stub.

%%
%%  SHA512_MAC
%%
-spec sha512_mac(iodata(), iodata()) -> binary().

sha512_mac(Key, Data) ->
    sha512_mac(Key, Data, 512 div 8).

sha512_mac(Key, Data, MacSz) ->
   case sha512_mac_nif(Key, Data, MacSz) of
       notsup -> erlang:error(notsup);
       Bin -> Bin
   end.

sha512_mac_nif(_Key,_Data,_MacSz) -> ?nif_stub.

%% CIPHERS --------------------------------------------------------------------

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
    des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, true).
des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, true).

des3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, false).
des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_crypt(Key1, Key2, Key3, IVec, Data, false).

des_ede3_cbc_crypt(_Key1, _Key2, _Key3, _IVec, _Data, _IsEncrypt) -> ?nif_stub.    

%%
%% DES3 - in 8-bits cipher feedback mode (CFB)
%%
-spec des3_cfb_encrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().
-spec des3_cfb_decrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().

des3_cfb_encrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cfb_crypt(Key1, Key2, Key3, IVec, Data, true).

des3_cfb_decrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cfb_crypt(Key1, Key2, Key3, IVec, Data, false).

des_ede3_cfb_crypt(Key1, Key2, Key3, IVec, Data, IsEncrypt) ->
    case des_ede3_cfb_crypt_nif(Key1,Key2,Key3,IVec,Data,IsEncrypt) of
	notsup -> erlang:error(notsup);
	Bin -> Bin
    end.

des_ede3_cfb_crypt_nif(_Key1, _Key2, _Key3, _IVec, _Data, _IsEncrypt) -> ?nif_stub.

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

blowfish_ofb64_decrypt(Key, Ivec, Data) ->
    blowfish_ofb64_encrypt(Key, Ivec, Data).

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
%% DES - in 8-bits cipher feedback mode (CFB)
%%
-spec des_cfb_encrypt(iodata(), binary(), iodata()) -> binary().
-spec des_cfb_decrypt(iodata(), binary(), iodata()) -> binary().

des_cfb_encrypt(Key, IVec, Data) ->
    des_cfb_crypt(Key, IVec, Data, true).

des_cfb_decrypt(Key, IVec, Data) ->
    des_cfb_crypt(Key, IVec, Data, false).

des_cfb_crypt(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.

%%
%% dec_cfb_ivec(IVec, Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% des_cfb_[encrypt|decrypt].
%%

-spec des_cfb_ivec(iodata(), iodata()) -> binary().

des_cfb_ivec(IVec, Data) ->
    IVecAndData = list_to_binary([IVec, Data]),
    {_, NewIVec} = split_binary(IVecAndData, byte_size(IVecAndData) - 8),
    NewIVec.


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
%% AES - with 256 bit key in infinite garble extension mode (IGE)
%%

-spec aes_ige_256_decrypt(iodata(), binary(), iodata()) ->
                 binary().

aes_ige_256_encrypt(Key, IVec, Data) ->
    aes_ige_crypt(Key, IVec, Data, true).

aes_ige_256_decrypt(Key, IVec, Data) ->
    aes_ige_crypt(Key, IVec, Data, false).

aes_ige_crypt(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.

%%
%% aes_ige_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% aes_ige_*_[encrypt|decrypt].
%% IVec size: 32 bytes
%%
aes_ige_ivec(Data) when is_binary(Data) ->
    {_, IVec} = split_binary(Data, size(Data) - 32),
    IVec;
aes_ige_ivec(Data) when is_list(Data) ->
    aes_ige_ivec(list_to_binary(Data)).


%% Stream ciphers --------------------------------------------------------------------

stream_crypt(Fun, State, Data, Size, MaxByts, []) when Size =< MaxByts ->
    Fun(State, Data);
stream_crypt(Fun, State0, Data, Size, MaxByts, Acc) when Size =< MaxByts ->
    {State, Cipher} = Fun(State0, Data),
    {State, list_to_binary(lists:reverse([Cipher | Acc]))};
stream_crypt(Fun, State0, Data, _, MaxByts, Acc) ->
    <<Increment:MaxByts/binary, Rest/binary>> = Data,
    {State, CipherText} = Fun(State0, Increment),
    stream_crypt(Fun, State, Rest, erlang:byte_size(Rest), MaxByts, [CipherText | Acc]).

do_stream_encrypt({aes_ctr, State0}, Data) ->
    {State, Cipher} = aes_ctr_stream_encrypt(State0, Data),
    {{aes_ctr, State}, Cipher};
do_stream_encrypt({rc4, State0}, Data) ->
    {State, Cipher} = rc4_encrypt_with_state(State0, Data),
    {{rc4, State}, Cipher}.

do_stream_decrypt({aes_ctr, State0}, Data) ->
    {State, Text} = aes_ctr_stream_decrypt(State0, Data),
    {{aes_ctr, State}, Text};
do_stream_decrypt({rc4, State0}, Data) ->
    {State, Text} = rc4_encrypt_with_state(State0, Data),
    {{rc4, State}, Text}.

%%
%% AES - in counter mode (CTR)
%%
-spec aes_ctr_encrypt(iodata(), binary(), iodata()) ->
				 binary().
-spec aes_ctr_decrypt(iodata(), binary(), iodata()) ->
				 binary().

aes_ctr_encrypt(_Key, _IVec, _Data) -> ?nif_stub.
aes_ctr_decrypt(_Key, _IVec, _Cipher) -> ?nif_stub.

%%
%% AES - in counter mode (CTR) with state maintained for multi-call streaming 
%%
-type ctr_state() :: { iodata(), binary(), binary(), integer() }.

-spec aes_ctr_stream_init(iodata(), binary()) -> ctr_state().
-spec aes_ctr_stream_encrypt(ctr_state(), binary()) ->
				 { ctr_state(), binary() }.
-spec aes_ctr_stream_decrypt(ctr_state(), binary()) ->
				 { ctr_state(), binary() }.
 
aes_ctr_stream_init(Key, IVec) -> 
    {Key, IVec, << 0:128 >>, 0}.
aes_ctr_stream_encrypt({_Key, _IVec, _ECount, _Num}=_State, _Data) -> ?nif_stub.
aes_ctr_stream_decrypt({_Key, _IVec, _ECount, _Num}=_State, _Cipher) -> ?nif_stub.
     
%%
%% RC4 - symmetric stream cipher
%%
-spec rc4_encrypt(iodata(), iodata()) -> binary().

rc4_encrypt(_Key, _Data) -> ?nif_stub.
rc4_set_key(_Key) -> ?nif_stub.
rc4_encrypt_with_state(_State, _Data) -> ?nif_stub.


%% RC2 block cipher

rc2_cbc_encrypt(Key, IVec, Data) ->
    rc2_cbc_crypt(Key,IVec,Data,true).

rc2_cbc_decrypt(Key, IVec, Data) ->
    rc2_cbc_crypt(Key,IVec,Data,false).

rc2_cbc_crypt(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.

%%
%% RC2 - 40 bits block cipher - Backwards compatibility not documented.
%%
rc2_40_cbc_encrypt(Key, IVec, Data) when erlang:byte_size(Key) == 5 ->
    rc2_cbc_crypt(Key,IVec,Data,true).

rc2_40_cbc_decrypt(Key, IVec, Data)  when erlang:byte_size(Key) == 5 ->
    rc2_cbc_crypt(Key,IVec,Data,false).


%% Secure remote password  -------------------------------------------------------------------

user_srp_gen_key(Private, Generator, Prime) ->
    case mod_pow(Generator, Private, Prime) of
	error ->
	    error;
	Public ->
	    {Public, Private}
    end.

host_srp_gen_key(Private, Verifier, Generator, Prime, Version) ->
 Multiplier = srp_multiplier(Version, Generator, Prime),
   case srp_value_B_nif(Multiplier, Verifier, Generator, Private, Prime) of
   error ->
       error;
   Public ->
       {Public, Private}
   end.

srp_multiplier('6a', Generator, Prime) ->
    %% k = SHA1(N | PAD(g)) from http://srp.stanford.edu/design.html
    C0 = sha_init(),
    C1 = sha_update(C0, Prime),
    C2 = sha_update(C1, srp_pad_to(erlang:byte_size(Prime), Generator)),
    sha_final(C2);
srp_multiplier('6', _, _) ->
    <<3/integer>>;
srp_multiplier('3', _, _) ->
    <<1/integer>>.

srp_scrambler(Version, UserPublic, HostPublic, Prime) when Version == '6'; Version == '6a'->
    %% SHA1(PAD(A) | PAD(B)) from http://srp.stanford.edu/design.html
    PadLength = erlang:byte_size(Prime),
    C0 = sha_init(),
    C1 = sha_update(C0, srp_pad_to(PadLength, UserPublic)),
    C2 = sha_update(C1, srp_pad_to(PadLength, HostPublic)),
    sha_final(C2);
srp_scrambler('3', _, HostPublic, _Prime) ->
    %% The parameter u is a 32-bit unsigned integer which takes its value
    %% from the first 32 bits of the SHA1 hash of B, MSB first.
    <<U:32/bits, _/binary>> = sha(HostPublic),
    U.

srp_pad_length(Width, Length) ->
    (Width - Length rem Width) rem Width.

srp_pad_to(Width, Binary) ->
    case srp_pad_length(Width, size(Binary)) of
        0 -> Binary;
        N -> << 0:(N*8), Binary/binary>>
    end.

srp_host_secret_nif(_Verifier, _B, _U, _A, _Prime) -> ?nif_stub.

srp_user_secret_nif(_A, _U, _B, _Multiplier, _Generator, _Exponent, _Prime) -> ?nif_stub.

srp_value_B_nif(_Multiplier, _Verifier, _Generator, _Exponent, _Prime) -> ?nif_stub.


%% Digital signatures  --------------------------------------------------------------------
rsa_sign_nif(_Type,_Data,_Key) -> ?nif_stub.
dss_sign_nif(_Type,_Data,_Key) -> ?nif_stub.
ecdsa_sign_nif(_Type, _DataOrDigest, _Curve, _Key) -> ?nif_stub.

dss_verify_nif(_Type, _Data, _Signature, _Key) -> ?nif_stub.
rsa_verify_nif(_Type, _Data, _Signature, _Key) -> ?nif_stub.
ecdsa_verify_nif(_Type, _DataOrDigest, _Signature, _Curve, _Key) -> ?nif_stub.

%% Public Keys  --------------------------------------------------------------------
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
    dh_generate_key_nif(undefined, map_mpint_to_bin(DHParameters), 4).
dh_generate_key(PrivateKey, DHParameters) ->
    dh_generate_key_nif(mpint_to_bin(PrivateKey), map_mpint_to_bin(DHParameters), 4).

dh_generate_key_nif(_PrivateKey, _DHParameters, _Mpint) -> ?nif_stub.

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% MyPrivKey, OthersPublicKey = mpint()
-spec dh_compute_key(binary(), binary(), [binary()]) -> binary().

dh_compute_key(OthersPublicKey, MyPrivateKey, DHParameters) ->
    compute_key(dh, mpint_to_bin(OthersPublicKey), mpint_to_bin(MyPrivateKey),
		map_mpint_to_bin(DHParameters)).


dh_compute_key_nif(_OthersPublicKey, _MyPrivateKey, _DHParameters) -> ?nif_stub.

ec_key_generate(_Key) -> ?nif_stub.

ecdh_compute_key_nif(_Others, _Curve, _My) -> ?nif_stub.

%%
%% EC
%%

term_to_nif_prime({prime_field, Prime}) ->
    {prime_field, int_to_bin(Prime)};
term_to_nif_prime(PrimeField) ->
    PrimeField.
term_to_nif_curve({A, B, Seed}) ->
    {ensure_int_as_bin(A), ensure_int_as_bin(B), Seed}.
nif_curve_params({PrimeField, Curve, BasePoint, Order, CoFactor}) ->
    {term_to_nif_prime(PrimeField), term_to_nif_curve(Curve), ensure_int_as_bin(BasePoint), int_to_bin(Order), int_to_bin(CoFactor)};
nif_curve_params(Curve) when is_atom(Curve) ->
    %% named curve
    Curve.


%% MISC --------------------------------------------------------------------

exor(Data1, Data2, Size, MaxByts, [])  when Size =< MaxByts ->
    do_exor(Data1, Data2);
exor(Data1, Data2, Size, MaxByts, Acc) when Size =< MaxByts ->
    Result = do_exor(Data1, Data2),
    list_to_binary(lists:reverse([Result | Acc]));
exor(Data1, Data2, _Size, MaxByts, Acc) ->
     <<Increment1:MaxByts/binary, Rest1/binary>> = Data1,
     <<Increment2:MaxByts/binary, Rest2/binary>> = Data2,
    Result = do_exor(Increment1, Increment2),
    exor(Rest1, Rest2, erlang:byte_size(Rest1), MaxByts, [Result | Acc]).

do_exor(_A, _B) -> ?nif_stub.

algorithms() -> ?nif_stub.

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

bytes_to_integer(Bin) ->
    bin_to_int(Bin).

bin_to_int(Bin) when is_binary(Bin) ->
    Bits = bit_size(Bin),
    <<Integer:Bits/integer>> = Bin,
    Integer;
bin_to_int(undefined) ->
    undefined.

map_ensure_int_as_bin([H|_]=List) when is_integer(H) ->
    lists:map(fun(E) -> int_to_bin(E) end, List);
map_ensure_int_as_bin(List) ->
    List.

ensure_int_as_bin(Int) when is_integer(Int) ->
    int_to_bin(Int);
ensure_int_as_bin(Bin) ->
    Bin.

map_to_norm_bin([H|_]=List) when is_integer(H) ->
    lists:map(fun(E) -> int_to_bin(E) end, List);
map_to_norm_bin(List) ->
    lists:map(fun(E) -> mpint_to_bin(E) end, List).

%%--------------------------------------------------------------------
%%% Deprecated
%%--------------------------------------------------------------------
%%
%%  rsa_public_encrypt
%%  rsa_private_decrypt
-type rsa_padding() :: 'rsa_pkcs1_padding' | 'rsa_pkcs1_oaep_padding' | 'rsa_no_padding'.

-spec rsa_public_encrypt(binary(), [binary()], rsa_padding()) ->
				binary().
-spec rsa_public_decrypt(binary(), [integer() | mpint()], rsa_padding()) ->
				binary().
-spec rsa_private_encrypt(binary(), [integer() | mpint()], rsa_padding()) ->
				binary().
-spec rsa_private_decrypt(binary(), [integer() | mpint()], rsa_padding()) ->
				binary().

%% Binary, Key = [E,N]
rsa_public_encrypt(BinMesg, Key, Padding) ->
    case rsa_public_crypt(BinMesg, map_to_norm_bin(Key), Padding, true) of
	error ->
	    erlang:error(encrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

rsa_public_crypt(_BinMsg, _Key, _Padding, _IsEncrypt) -> ?nif_stub.

%% Binary, Key = [E,N,D]
rsa_private_decrypt(BinMesg, Key, Padding) ->
    case rsa_private_crypt(BinMesg, map_to_norm_bin(Key), Padding, false) of
	error ->
	    erlang:error(decrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

rsa_private_crypt(_BinMsg, _Key, _Padding, _IsEncrypt) -> ?nif_stub.


%% Binary, Key = [E,N,D]
rsa_private_encrypt(BinMesg, Key, Padding) ->
    case rsa_private_crypt(BinMesg, map_to_norm_bin(Key), Padding, true) of
	error ->
	    erlang:error(encrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%% Binary, Key = [E,N]
rsa_public_decrypt(BinMesg, Key, Padding) ->
    case rsa_public_crypt(BinMesg, map_to_norm_bin(Key), Padding, false) of
	error ->
	    erlang:error(decrypt_failed, [BinMesg,Key, Padding]);
	Sign -> Sign
    end.

map_mpint_to_bin(List) ->
    lists:map(fun(E) -> mpint_to_bin(E) end, List ).

%%
%% DSS, RSA - sign
%%
%% Key = [P,Q,G,X]   P,Q,G=DSSParams  X=PrivateKey
-spec dss_sign(data_or_digest(), [binary()]) -> binary().
-spec dss_sign(dss_digest_type(), data_or_digest(), [binary()]) -> binary().
-spec rsa_sign(data_or_digest(), [binary()]) -> binary().
-spec rsa_sign(rsa_digest_type(), data_or_digest(), [binary()]) -> binary().

dss_sign(DataOrDigest,Key) ->
    dss_sign(sha,DataOrDigest,Key).
dss_sign(Type, Data, Key) when is_binary(Data), Type=/=none ->
    sign(dss, Type, mpint_to_bin(Data), map_mpint_to_bin(Key));
dss_sign(Type, Digest, Key) ->
    sign(dss, Type, Digest, map_mpint_to_bin(Key)).


%% Key = [E,N,D]  E=PublicExponent N=PublicModulus  D=PrivateExponent
rsa_sign(DataOrDigest,Key) ->
    rsa_sign(sha, DataOrDigest, Key).

rsa_sign(Type, Data, Key) when is_binary(Data) ->
    sign(rsa, Type, mpint_to_bin(Data), map_mpint_to_bin(Key));
rsa_sign(Type, Digest, Key) ->
    sign(rsa, Type, Digest, map_mpint_to_bin(Key)).

%%
%% DSS, RSA - verify
%%
-spec dss_verify(data_or_digest(), binary(), [binary()]) -> boolean().
-spec dss_verify(dss_digest_type(), data_or_digest(), binary(), [binary()]) -> boolean().
-spec rsa_verify(data_or_digest(), binary(), [binary()]) -> boolean().
-spec rsa_verify(rsa_digest_type(), data_or_digest(), binary(), [binary()]) ->
			boolean().

%% Key = [P,Q,G,Y]   P,Q,G=DSSParams  Y=PublicKey
dss_verify(Data,Signature,Key) ->
    dss_verify(sha, Data, Signature, Key).

dss_verify(Type,Data,Signature,Key) when is_binary(Data), Type=/=none ->
    verify(dss,Type,mpint_to_bin(Data),mpint_to_bin(Signature),map_mpint_to_bin(Key));
dss_verify(Type,Digest,Signature,Key) ->
    verify(dss,Type,Digest,mpint_to_bin(Signature),map_mpint_to_bin(Key)).

% Key = [E,N]  E=PublicExponent N=PublicModulus
rsa_verify(Data,Signature,Key) ->
    rsa_verify(sha, Data,Signature,Key).
rsa_verify(Type, Data, Signature, Key) when is_binary(Data) ->
    verify(rsa, Type, mpint_to_bin(Data), mpint_to_bin(Signature), map_mpint_to_bin(Key));
rsa_verify(Type, Digest, Signature, Key) ->
    verify(rsa, Type, Digest, mpint_to_bin(Signature), map_mpint_to_bin(Key)).

-spec strong_rand_mpint(Bits::non_neg_integer(),
			Top::-1..1,
			Bottom::0..1) -> binary().

strong_rand_mpint(Bits, Top, Bottom) ->
    case strong_rand_mpint_nif(Bits,Top,Bottom) of
        false -> erlang:error(low_entropy);
        Bin -> Bin
    end.
strong_rand_mpint_nif(_Bits, _Top, _Bottom) -> ?nif_stub.


%% large integer in a binary with 32bit length
%% MP representaion  (SSH2)
mpint(X) when X < 0 -> mpint_neg(X);
mpint(X) -> mpint_pos(X).

-define(UINT32(X),   X:32/unsigned-big-integer).


mpint_neg(X) ->
    Bin = int_to_bin_neg(X, []),
    Sz = byte_size(Bin),
    <<?UINT32(Sz), Bin/binary>>.
    
mpint_pos(X) ->
    Bin = int_to_bin_pos(X, []),
    <<MSB,_/binary>> = Bin,
    Sz = byte_size(Bin),
    if MSB band 16#80 == 16#80 ->
	    <<?UINT32((Sz+1)), 0, Bin/binary>>;
       true ->
	    <<?UINT32(Sz), Bin/binary>>
    end.

%% int from integer in a binary with 32bit length
erlint(<<MPIntSize:32/integer,MPIntValue/binary>>) ->
    Bits= MPIntSize * 8,
    <<Integer:Bits/integer>> = MPIntValue,
    Integer.

mpint_to_bin(<<Len:32, Bin:Len/binary>>) ->
    Bin.

%%
%% mod_exp - utility for rsa generation and SRP
%%
mod_exp(Base, Exponent, Modulo)
  when is_integer(Base), is_integer(Exponent), is_integer(Modulo) ->
    bin_to_int(mod_exp_nif(int_to_bin(Base), int_to_bin(Exponent), int_to_bin(Modulo), 0));

mod_exp(Base, Exponent, Modulo) ->
    mod_exp_nif(mpint_to_bin(Base),mpint_to_bin(Exponent),mpint_to_bin(Modulo), 4).

mod_exp_nif(_Base,_Exp,_Mod,_bin_hdr) -> ?nif_stub.

-define(FUNC_LIST, [hash, hash_init, hash_update, hash_final,
		    hmac, hmac_init, hmac_update, hmac_final, hmac_final_n,
		    %% deprecated
		    md4, md4_init, md4_update, md4_final,
		    md5, md5_init, md5_update, md5_final,
		    sha, sha_init, sha_update, sha_final,
		    md5_mac,  md5_mac_96,
		    sha_mac,  sha_mac_96,
		    %%
		    block_encrypt, block_decrypt,
		    %% deprecated
		    des_cbc_encrypt, des_cbc_decrypt,
		    des_cfb_encrypt, des_cfb_decrypt,
		    des_ecb_encrypt, des_ecb_decrypt,
		    des3_cbc_encrypt, des3_cbc_decrypt,
		    des3_cfb_encrypt, des3_cfb_decrypt,
		    aes_cfb_128_encrypt, aes_cfb_128_decrypt,
		    rc2_cbc_encrypt, rc2_cbc_decrypt,
		    rc2_40_cbc_encrypt, rc2_40_cbc_decrypt,
		    aes_cbc_128_encrypt, aes_cbc_128_decrypt,
		    aes_cbc_256_encrypt, aes_cbc_256_decrypt,
		    blowfish_cbc_encrypt, blowfish_cbc_decrypt,
		    blowfish_cfb64_encrypt, blowfish_cfb64_decrypt,
		    blowfish_ecb_encrypt, blowfish_ecb_decrypt, blowfish_ofb64_encrypt,
		    %%
		    rand_bytes,
		    strong_rand_bytes,
		    rand_uniform,
		    mod_pow,
		    exor,
		    %% deprecated
		    mod_exp,strong_rand_mpint,erlint, mpint,
		    %%
		    sign, verify, generate_key, compute_key,
		    %% deprecated
		    dss_verify,dss_sign,
		    rsa_verify,rsa_sign,
		    rsa_public_encrypt,rsa_private_decrypt,
		    rsa_private_encrypt,rsa_public_decrypt,
		    dh_generate_key, dh_compute_key,
		    %%
		    stream_init, stream_encrypt, stream_decrypt,
		    %% deprecated
		    rc4_encrypt, rc4_set_key, rc4_encrypt_with_state,
		    aes_ctr_encrypt, aes_ctr_decrypt,
                    aes_ctr_stream_init, aes_ctr_stream_encrypt, aes_ctr_stream_decrypt,
		    %%
		    next_iv,
		    %% deprecated
		    aes_cbc_ivec,
		    des_cbc_ivec, des_cfb_ivec,
		    info,
		    %%
		    info_lib, supports]).
info() ->
    ?FUNC_LIST.
