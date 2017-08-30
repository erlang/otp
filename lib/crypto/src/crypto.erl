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

%% Purpose : Main Crypto API module.

-module(crypto).

-export([start/0, stop/0, info_lib/0, supports/0, version/0, bytes_to_integer/1]).
-export([hash/2, hash_init/1, hash_update/2, hash_final/1]).
-export([sign/4, verify/5]).
-export([generate_key/2, generate_key/3, compute_key/4]).
-export([hmac/3, hmac/4, hmac_init/2, hmac_update/2, hmac_final/1, hmac_final_n/2]).
-export([exor/2, strong_rand_bytes/1, mod_pow/3]).
-export([rand_uniform/2]).
-export([block_encrypt/3, block_decrypt/3, block_encrypt/4, block_decrypt/4]).
-export([next_iv/2, next_iv/3]).
-export([stream_init/2, stream_init/3, stream_encrypt/2, stream_decrypt/2]).
-export([public_encrypt/4, private_decrypt/4]).
-export([private_encrypt/4, public_decrypt/4]).
-export([dh_generate_parameters/2, dh_check/1]). %% Testing see
-export([ec_curve/1, ec_curves/0]).
-export([rand_seed/1]).

%% DEPRECATED
-export([rand_bytes/1]).
-deprecated({rand_bytes, 1, next_major_release}).

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
-define(CRYPTO_NIF_VSN,301).

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
    {Hashs, PubKeys, Ciphers} = algorithms(),

    [{hashs, Hashs},
     {ciphers, Ciphers},
     {public_keys, PubKeys}
    ].

info_lib() -> ?nif_stub.

-spec hash(_, iodata()) -> binary().

hash(Hash, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxBytes = max_bytes(),
    hash(Hash, Data, erlang:byte_size(Data), MaxBytes).

-spec hash_init('md5'|'md4'|'ripemd160'|
                'sha'|'sha224'|'sha256'|'sha384'|'sha512') -> any().

hash_init(Hash) ->
    notsup_to_error(hash_init_nif(Hash)).

-spec hash_update(_, iodata()) -> any().

hash_update(State, Data0) ->
    Data = iolist_to_binary(Data0),
    MaxBytes = max_bytes(),
    hash_update(State, Data, erlang:byte_size(Data), MaxBytes).

-spec hash_final(_) -> binary().

hash_final(State) ->
    notsup_to_error(hash_final_nif(State)).


-spec hmac(_, iodata(), iodata()) -> binary().
-spec hmac(_, iodata(), iodata(), integer()) -> binary().
-spec hmac_init(atom(), iodata()) -> binary().
-spec hmac_update(binary(), iodata()) -> binary().
-spec hmac_final(binary()) -> binary().
-spec hmac_final_n(binary(), integer()) -> binary().

hmac(Type, Key, Data0) ->
    Data = iolist_to_binary(Data0),
    hmac(Type, Key, Data, undefined, erlang:byte_size(Data), max_bytes()).
hmac(Type, Key, Data0, MacSize) ->
    Data = iolist_to_binary(Data0),
    hmac(Type, Key, Data, MacSize, erlang:byte_size(Data), max_bytes()).

hmac_init(Type, Key) ->
    notsup_to_error(hmac_init_nif(Type, Key)).

hmac_update(State, Data0) ->
    Data = iolist_to_binary(Data0),
    hmac_update(State, Data, erlang:byte_size(Data), max_bytes()).

hmac_final(Context) ->
    notsup_to_error(hmac_final_nif(Context)).
hmac_final_n(Context, HashLen) ->
    notsup_to_error(hmac_final_nif(Context, HashLen)).

%% Ecrypt/decrypt %%%

-spec block_encrypt(des_cbc | des_cfb |
                    des3_cbc | des3_cbf | des_ede3 |
                    blowfish_cbc | blowfish_cfb64 | blowfish_ofb64 |
                    aes_cbc128 | aes_cfb8 | aes_cfb128 | aes_cbc256 | aes_ige256 |
		    aes_cbc |
                    rc2_cbc,
		    Key::iodata(), Ivec::binary(), Data::iodata()) -> binary();
		   (aes_gcm | chacha20_poly1305, Key::iodata(), Ivec::binary(), {AAD::binary(), Data::iodata()}) -> {binary(), binary()}.

block_encrypt(Type, Key, Ivec, Data) when Type =:= des_cbc;
                                          Type =:= des_cfb;
                                          Type =:= blowfish_cbc;
                                          Type =:= blowfish_cfb64;
                                          Type =:= blowfish_ofb64;
                                          Type =:= aes_cbc128;
                                          Type =:= aes_cfb8;
                                          Type =:= aes_cfb128;
                                          Type =:= aes_cbc256;
					  Type =:= aes_cbc;
                                          Type =:= rc2_cbc ->
    block_crypt_nif(Type, Key, Ivec, Data, true);
block_encrypt(Type, Key0, Ivec, Data) when Type =:= des3_cbc;
                                           Type =:= des_ede3 ->
    Key = check_des3_key(Key0),
    block_crypt_nif(des_ede3_cbc, Key, Ivec, Data, true);
block_encrypt(des3_cbf, Key0, Ivec, Data) ->
    Key = check_des3_key(Key0),
    block_crypt_nif(des_ede3_cbf, Key, Ivec, Data, true);
block_encrypt(aes_ige256, Key, Ivec, Data) ->
    aes_ige_crypt_nif(Key, Ivec, Data, true);
block_encrypt(aes_gcm, Key, Ivec, {AAD, Data}) ->
    aes_gcm_encrypt(Key, Ivec, AAD, Data);
block_encrypt(aes_gcm, Key, Ivec, {AAD, Data, TagLength}) ->
    aes_gcm_encrypt(Key, Ivec, AAD, Data, TagLength);
block_encrypt(chacha20_poly1305, Key, Ivec, {AAD, Data}) ->
    chacha20_poly1305_encrypt(Key, Ivec, AAD, Data).

-spec block_decrypt(des_cbc | des_cfb |
                    des3_cbc | des3_cbf | des_ede3 |
                    blowfish_cbc | blowfish_cfb64 | blowfish_ofb64 |
                    aes_cbc128 | aes_cfb8 | aes_cfb128 | aes_cbc256 | aes_ige256 |
		    aes_cbc |
                    rc2_cbc,
		    Key::iodata(), Ivec::binary(), Data::iodata()) -> binary();
		   (aes_gcm | chacha20_poly1305, Key::iodata(), Ivec::binary(),
		    {AAD::binary(), Data::iodata(), Tag::binary()}) -> binary() | error.
block_decrypt(Type, Key, Ivec, Data) when Type =:= des_cbc;
                                          Type =:= des_cfb;
                                          Type =:= blowfish_cbc;
                                          Type =:= blowfish_cfb64;
                                          Type =:= blowfish_ofb64;
					  Type =:= aes_cbc;
                                          Type =:= aes_cbc128;
                                          Type =:= aes_cfb8;
                                          Type =:= aes_cfb128;
                                          Type =:= aes_cbc256;
                                          Type =:= rc2_cbc ->
    block_crypt_nif(Type, Key, Ivec, Data, false);
block_decrypt(Type, Key0, Ivec, Data) when Type =:= des3_cbc;
                                           Type =:= des_ede3 ->
    Key = check_des3_key(Key0),
    block_crypt_nif(des_ede3_cbc, Key, Ivec, Data, false);
block_decrypt(des3_cbf, Key0, Ivec, Data) ->
    Key = check_des3_key(Key0),
    block_crypt_nif(des_ede3_cbf, Key, Ivec, Data, false);
block_decrypt(aes_ige256, Key, Ivec, Data) ->
    notsup_to_error(aes_ige_crypt_nif(Key, Ivec, Data, false));
block_decrypt(aes_gcm, Key, Ivec, {AAD, Data, Tag}) ->
    aes_gcm_decrypt(Key, Ivec, AAD, Data, Tag);
block_decrypt(chacha20_poly1305, Key, Ivec, {AAD, Data, Tag}) ->
    chacha20_poly1305_decrypt(Key, Ivec, AAD, Data, Tag).

-spec block_encrypt(des_ecb | blowfish_ecb | aes_ecb, Key::iodata(), Data::iodata()) -> binary().

block_encrypt(Type, Key, Data) ->
    block_crypt_nif(Type, Key, Data, true).

-spec block_decrypt(des_ecb | blowfish_ecb | aes_ecb, Key::iodata(), Data::iodata()) -> binary().

block_decrypt(Type, Key, Data) ->
    block_crypt_nif(Type, Key, Data, false).

-spec next_iv(des_cbc | des3_cbc | aes_cbc | aes_ige, Data::iodata()) -> binary().

next_iv(Type, Data) when is_binary(Data) ->
    IVecSize = case Type of
                   des_cbc  -> 8;
                   des3_cbc -> 8;
                   aes_cbc  -> 16;
                   aes_ige  -> 32
               end,
    {_, IVec} = split_binary(Data, size(Data) - IVecSize),
    IVec;
next_iv(Type, Data) when is_list(Data) ->
    next_iv(Type, list_to_binary(Data)).

-spec next_iv(des_cfb, Data::iodata(), Ivec::binary()) -> binary().

next_iv(des_cfb, Data, IVec) ->
    IVecAndData = list_to_binary([IVec, Data]),
    {_, NewIVec} = split_binary(IVecAndData, byte_size(IVecAndData) - 8),
    NewIVec;
next_iv(Type, Data, _Ivec) ->
    next_iv(Type, Data).

stream_init(aes_ctr, Key, Ivec) ->
    {aes_ctr, aes_ctr_stream_init(Key, Ivec)}.
stream_init(rc4, Key) ->
    {rc4, notsup_to_error(rc4_set_key(Key))}.

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

-spec rand_seed(binary()) -> ok.
rand_seed(Seed) ->
    rand_seed_nif(Seed).

rand_seed_nif(_Seed) -> ?nif_stub.

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
verify(dss, Type, {digest, Digest}, Signature, Key) ->
    dss_verify_nif(Type, Digest, Signature, map_ensure_int_as_bin(Key));
verify(rsa, Type, {digest, Digest}, Signature, Key) ->
    notsup_to_error(
      rsa_verify_nif(Type, Digest, Signature, map_ensure_int_as_bin(Key)));
verify(ecdsa, Type, {digest, Digest}, Signature, [Key, Curve]) ->
    notsup_to_error(
      ecdsa_verify_nif(Type, Digest, Signature, nif_curve_params(Curve), ensure_int_as_bin(Key))).
sign(dss, none, Data, Key) when is_binary(Data) ->
    sign(dss, sha, {digest, Data}, Key);
sign(Alg, Type, Data, Key) when is_binary(Data) ->
    sign(Alg, Type, {digest, hash(Type, Data)}, Key);
sign(rsa, Type, {digest, Digest}, Key) ->
    case rsa_sign_nif(Type, Digest, map_ensure_int_as_bin(Key)) of
	error -> erlang:error(badkey, [Type,Digest,Key]);
	Sign -> Sign
    end;
sign(dss, Type, {digest, Digest}, Key) ->
    case dss_sign_nif(Type, Digest, map_ensure_int_as_bin(Key)) of
	error -> erlang:error(badkey, [Digest, Key]);
	Sign -> Sign
    end;
sign(ecdsa, Type, {digest, Digest}, [Key, Curve]) ->
    case ecdsa_sign_nif(Type, Digest, nif_curve_params(Curve), ensure_int_as_bin(Key)) of
	error -> erlang:error(badkey, [Type,Digest,Key]);
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
		  undefined -> strong_rand_bytes(32);
		  _ -> ensure_int_as_bin(PrivArg)
	      end,
    host_srp_gen_key(Private, Verifier, Generator, Prime, Version);

generate_key(srp, {user, [Generator, Prime, Version]}, PrivateArg)
  when is_binary(Generator), is_binary(Prime), is_atom(Version) ->
    Private = case PrivateArg of
		  undefined -> strong_rand_bytes(32);
		  _ -> PrivateArg
	      end,
    user_srp_gen_key(Private, Generator, Prime);

generate_key(ecdh, Curve, PrivKey) ->
    ec_key_generate(nif_curve_params(Curve), ensure_int_as_bin(PrivKey)).

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
    notsup_to_error(
    srp_user_secret_nif(ensure_int_as_bin(UserPrivate), Scrambler, HostPubBin,
                          Multiplier, Generator, DerivedKey, Prime));

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
    notsup_to_error(
    srp_host_secret_nif(Verifier, ensure_int_as_bin(HostPrivate), Scrambler,
                          UserPubBin, Prime));

compute_key(ecdh, Others, My, Curve) ->
    ecdh_compute_key_nif(ensure_int_as_bin(Others),
			 nif_curve_params(Curve),
			 ensure_int_as_bin(My)).

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
    LibBin   = path2bin(Lib),
    Status = case erlang:load_nif(Lib, {?CRYPTO_NIF_VSN,LibBin}) of
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
                             ArchBin = path2bin(ArchLib),
			     erlang:load_nif(ArchLib, {?CRYPTO_NIF_VSN,ArchBin})
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

path2bin(Path) when is_list(Path) ->
    Encoding = file:native_name_encoding(),
    case unicode:characters_to_binary(Path,Encoding,Encoding) of
	Bin when is_binary(Bin) ->
	    Bin
    end.

%%--------------------------------------------------------------------
%%% Internal functions (some internal API functions are part of the deprecated API)
%%--------------------------------------------------------------------
max_bytes() ->
    ?MAX_BYTES_TO_NIF.

notsup_to_error(notsup) ->
    erlang:error(notsup);
notsup_to_error(Other) ->
    Other.

%% HASH --------------------------------------------------------------------
hash(Hash, Data, Size, Max) when Size =< Max ->
    notsup_to_error(hash_nif(Hash, Data));
hash(Hash, Data, Size, Max) ->
    State0 = hash_init(Hash),
    State1 = hash_update(State0, Data, Size, Max),
    hash_final(State1).

hash_update(State, Data, Size, MaxBytes)  when Size =< MaxBytes ->
    notsup_to_error(hash_update_nif(State, Data));
hash_update(State0, Data, _, MaxBytes) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = notsup_to_error(hash_update_nif(State0, Increment)),
    hash_update(State, Rest, erlang:byte_size(Rest), MaxBytes).

hash_nif(_Hash, _Data) -> ?nif_stub.
hash_init_nif(_Hash) -> ?nif_stub.
hash_update_nif(_State, _Data) -> ?nif_stub.
hash_final_nif(_State) -> ?nif_stub.


%%
%%  MD5
%%

-spec md5(iodata()) -> binary().
-spec md5_init() -> binary().
-spec md5_update(binary(), iodata()) -> binary().
-spec md5_final(binary()) -> binary().

md5(Data) ->
    hash(md5, Data).
md5_init() ->
    hash_init(md5).
md5_update(Context, Data) ->
    hash_update(Context, Data).
md5_final(Context) ->
    hash_final(Context).

%%
%%  MD4
%%
-spec md4(iodata()) -> binary().
-spec md4_init() -> binary().
-spec md4_update(binary(), iodata()) -> binary().
-spec md4_final(binary()) -> binary().

md4(Data) ->
    hash(md4, Data).
md4_init() ->
    hash_init(md4).
md4_update(Context, Data) ->
    hash_update(Context, Data).
md4_final(Context) ->
    hash_final(Context).

%%
%% SHA
%%
-spec sha(iodata()) -> binary().
-spec sha_init() -> binary().
-spec sha_update(binary(), iodata()) -> binary().
-spec sha_final(binary()) -> binary().

sha(Data) ->
    hash(sha, Data).
sha_init() ->
    hash_init(sha).
sha_update(Context, Data) ->
    hash_update(Context, Data).
sha_final(Context) ->
    hash_final(Context).

%% HMAC --------------------------------------------------------------------

hmac(Type, Key, Data, MacSize, Size, MaxBytes) when Size =< MaxBytes ->
    notsup_to_error(
    case MacSize of
          undefined -> hmac_nif(Type, Key, Data);
          _         -> hmac_nif(Type, Key, Data, MacSize)
      end);
hmac(Type, Key, Data, MacSize, Size, MaxBytes) ->
    State0 = hmac_init(Type, Key),
    State1 = hmac_update(State0, Data, Size, MaxBytes),
    case MacSize of
        undefined -> hmac_final(State1);
        _         -> hmac_final_n(State1, MacSize)
    end.

hmac_update(State, Data, Size, MaxBytes)  when Size =< MaxBytes ->
    notsup_to_error(hmac_update_nif(State, Data));
hmac_update(State0, Data, _, MaxBytes) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = notsup_to_error(hmac_update_nif(State0, Increment)),
    hmac_update(State, Rest, erlang:byte_size(Rest), MaxBytes).

hmac_nif(_Type, _Key, _Data) -> ?nif_stub.
hmac_nif(_Type, _Key, _Data, _MacSize) -> ?nif_stub.
hmac_init_nif(_Type, _Key) -> ?nif_stub.
hmac_update_nif(_Context, _Data) -> ?nif_stub.
hmac_final_nif(_Context) -> ?nif_stub.
hmac_final_nif(_Context, _MacSize) -> ?nif_stub.

%%
%%  MD5_MAC
%%
-spec md5_mac(iodata(), iodata()) -> binary().
-spec md5_mac_96(iodata(), iodata()) -> binary().

md5_mac(Key, Data) -> hmac(md5, Key, Data).

md5_mac_96(Key, Data) -> hmac(md5, Key, Data, 12).

%%
%%  SHA_MAC
%%
-spec sha_mac(iodata(), iodata()) -> binary().
-spec sha_mac_96(iodata(), iodata()) -> binary().

sha_mac(Key, Data) -> hmac(sha, Key, Data).

sha_mac(Key, Data, Size) -> hmac(sha, Key, Data, Size).

sha_mac_96(Key, Data) -> hmac(sha, Key, Data, 12).

%% CIPHERS --------------------------------------------------------------------

block_crypt_nif(_Type, _Key, _Ivec, _Text, _IsEncrypt) -> ?nif_stub.
block_crypt_nif(_Type, _Key, _Text, _IsEncrypt) -> ?nif_stub.

check_des3_key(Key) ->
    case lists:map(fun erlang:iolist_to_binary/1, Key) of
        ValidKey = [B1, B2, B3] when byte_size(B1) =:= 8,
                                     byte_size(B2) =:= 8,
                                     byte_size(B3) =:= 8 ->
            ValidKey;
        _ ->
            error(badarg)
   end.

%%
%% DES - in electronic codebook mode (ECB)
%%
-spec des_ecb_encrypt(iodata(), iodata()) -> binary().
-spec des_ecb_decrypt(iodata(), iodata()) -> binary().

des_ecb_encrypt(Key, Data) ->
    block_encrypt(des_ecb, Key, Data).
des_ecb_decrypt(Key, Data) ->
    block_decrypt(des_ecb, Key, Data).

%%
%% DES3 - in cipher block chaining mode (CBC)
%%
-spec des3_cbc_encrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().
-spec des3_cbc_decrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().

des3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    block_encrypt(des3_cbc, [Key1, Key2, Key3], IVec, Data).
des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    block_encrypt(des_ede3, [Key1, Key2, Key3], IVec, Data).

des3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    block_decrypt(des3_cbc, [Key1, Key2, Key3], IVec, Data).
des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    block_decrypt(des_ede3, [Key1, Key2, Key3], IVec, Data).

%%
%% DES3 - in 8-bits cipher feedback mode (CFB)
%%
-spec des3_cfb_encrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().
-spec des3_cfb_decrypt(iodata(), iodata(), iodata(), binary(), iodata()) ->
			     binary().

des3_cfb_encrypt(Key1, Key2, Key3, IVec, Data) ->
    block_encrypt(des3_cbf, [Key1, Key2, Key3], IVec, Data).

des3_cfb_decrypt(Key1, Key2, Key3, IVec, Data) ->
    block_decrypt(des3_cbf, [Key1, Key2, Key3], IVec, Data).

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
    block_encrypt(blowfish_ecb, Key, Data).

blowfish_ecb_decrypt(Key, Data) ->
    block_decrypt(blowfish_ecb, Key, Data).

blowfish_cbc_encrypt(Key, IVec, Data) ->
    block_encrypt(blowfish_cbc, Key, IVec, Data).

blowfish_cbc_decrypt(Key, IVec, Data) ->
    block_decrypt(blowfish_cbc, Key, IVec, Data).

blowfish_cfb64_encrypt(Key, IVec, Data) ->
    block_encrypt(blowfish_cfb64, Key, IVec, Data).

blowfish_cfb64_decrypt(Key, IVec, Data) ->
    block_decrypt(blowfish_cfb64, Key, IVec, Data).

blowfish_ofb64_encrypt(Key, IVec, Data) ->
    block_encrypt(blowfish_ofb64, Key, IVec, Data).


%%
%% AES in cipher feedback mode (CFB) - 128 bit shift
%%
-spec aes_cfb_128_encrypt(iodata(), binary(), iodata()) -> binary().
-spec aes_cfb_128_decrypt(iodata(), binary(), iodata()) -> binary().

aes_cfb_128_encrypt(Key, IVec, Data) ->
    block_encrypt(aes_cfb128, Key, IVec, Data).

aes_cfb_128_decrypt(Key, IVec, Data) ->
    block_decrypt(aes_cfb128, Key, IVec, Data).

%%
%% AES - in Galois/Counter Mode (GCM)
%%
%% The default tag length is EVP_GCM_TLS_TAG_LEN(16),
aes_gcm_encrypt(Key, Ivec, AAD, In) ->
    aes_gcm_encrypt(Key, Ivec, AAD, In, 16).
aes_gcm_encrypt(_Key, _Ivec, _AAD, _In, _TagLength) -> ?nif_stub.
aes_gcm_decrypt(_Key, _Ivec, _AAD, _In, _Tag) -> ?nif_stub.

%%
%% Chacha20/Ppoly1305
%%
chacha20_poly1305_encrypt(_Key, _Ivec, _AAD, _In) -> ?nif_stub.
chacha20_poly1305_decrypt(_Key, _Ivec, _AAD, _In, _Tag) -> ?nif_stub.

%%
%% DES - in cipher block chaining mode (CBC)
%%
-spec des_cbc_encrypt(iodata(), binary(), iodata()) -> binary().
-spec des_cbc_decrypt(iodata(), binary(), iodata()) -> binary().

des_cbc_encrypt(Key, IVec, Data) ->
    block_encrypt(des_cbc, Key, IVec, Data).

des_cbc_decrypt(Key, IVec, Data) ->
    block_decrypt(des_cbc, Key, IVec, Data).

%%
%% dec_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% des_cbc_[encrypt|decrypt].
%%
-spec des_cbc_ivec(iodata()) -> binary().

des_cbc_ivec(Data) ->
    next_iv(des_cbc, Data).

%%
%% DES - in 8-bits cipher feedback mode (CFB)
%%
-spec des_cfb_encrypt(iodata(), binary(), iodata()) -> binary().
-spec des_cfb_decrypt(iodata(), binary(), iodata()) -> binary().

des_cfb_encrypt(Key, IVec, Data) ->
    block_encrypt(des_cfb, Key, IVec, Data).

des_cfb_decrypt(Key, IVec, Data) ->
    block_decrypt(des_cfb, Key, IVec, Data).

%%
%% dec_cfb_ivec(IVec, Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% des_cfb_[encrypt|decrypt].
%%

-spec des_cfb_ivec(iodata(), iodata()) -> binary().

des_cfb_ivec(IVec, Data) ->
    next_iv(des_cfb, Data, IVec).


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
    block_encrypt(aes_cbc128, Key, IVec, Data).

aes_cbc_128_decrypt(Key, IVec, Data) ->
    block_decrypt(aes_cbc128, Key, IVec, Data).

aes_cbc_256_encrypt(Key, IVec, Data) ->
    block_encrypt(aes_cbc256, Key, IVec, Data).

aes_cbc_256_decrypt(Key, IVec, Data) ->
    block_decrypt(aes_cbc256, Key, IVec, Data).

%%
%% aes_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of
%% aes_cbc_*_[encrypt|decrypt].
%% IVec size: 16 bytes
%%
aes_cbc_ivec(Data) ->
    next_iv(aes_cbc, Data).

%%
%% AES - with 256 bit key in infinite garble extension mode (IGE)
%%

aes_ige_crypt_nif(_Key, _IVec, _Data, _IsEncrypt) -> ?nif_stub.


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
-type ctr_state() :: { iodata(), binary(), binary(), integer() } | binary().

-spec aes_ctr_stream_init(iodata(), binary()) -> ctr_state().
-spec aes_ctr_stream_encrypt(ctr_state(), binary()) ->
				 { ctr_state(), binary() }.
-spec aes_ctr_stream_decrypt(ctr_state(), binary()) ->
				 { ctr_state(), binary() }.
 
aes_ctr_stream_init(_Key, _IVec) -> ?nif_stub.
aes_ctr_stream_encrypt(_State, _Data) -> ?nif_stub.
aes_ctr_stream_decrypt(_State, _Cipher) -> ?nif_stub.
     
%%
%% RC4 - symmetric stream cipher
%%
-spec rc4_encrypt(iodata(), iodata()) -> binary().

rc4_encrypt(_Key, _Data) -> ?nif_stub.
rc4_set_key(_Key) -> ?nif_stub.
rc4_encrypt_with_state(_State, _Data) -> ?nif_stub.


%% RC2 block cipher

rc2_cbc_encrypt(Key, IVec, Data) ->
    block_encrypt(rc2_cbc, Key, IVec, Data).

rc2_cbc_decrypt(Key, IVec, Data) ->
    block_decrypt(rc2_cbc, Key, IVec, Data).

%%
%% RC2 - 40 bits block cipher - Backwards compatibility not documented.
%%
rc2_40_cbc_encrypt(Key, IVec, Data) when erlang:byte_size(Key) == 5 ->
    block_encrypt(rc2_cbc, Key, IVec, Data).

rc2_40_cbc_decrypt(Key, IVec, Data)  when erlang:byte_size(Key) == 5 ->
    block_decrypt(rc2_cbc, Key, IVec, Data).


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
   notsup ->
       erlang:error(notsup);
   Public ->
       {Public, Private}
   end.

srp_multiplier('6a', Generator, Prime) ->
    %% k = SHA1(N | PAD(g)) from http://srp.stanford.edu/design.html
    C0 = hash_init(sha),
    C1 = hash_update(C0, Prime),
    C2 = hash_update(C1, srp_pad_to(erlang:byte_size(Prime), Generator)),
    hash_final(C2);
srp_multiplier('6', _, _) ->
    <<3/integer>>;
srp_multiplier('3', _, _) ->
    <<1/integer>>.

srp_scrambler(Version, UserPublic, HostPublic, Prime) when Version == '6'; Version == '6a'->
    %% SHA1(PAD(A) | PAD(B)) from http://srp.stanford.edu/design.html
    PadLength = erlang:byte_size(Prime),
    C0 = hash_init(sha),
    C1 = hash_update(C0, srp_pad_to(PadLength, UserPublic)),
    C2 = hash_update(C1, srp_pad_to(PadLength, HostPublic)),
    hash_final(C2);
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
rsa_sign_nif(_Type,_Digest,_Key) -> ?nif_stub.
dss_sign_nif(_Type,_Digest,_Key) -> ?nif_stub.
ecdsa_sign_nif(_Type, _Digest, _Curve, _Key) -> ?nif_stub.

dss_verify_nif(_Type, _Digest, _Signature, _Key) -> ?nif_stub.
rsa_verify_nif(_Type, _Digest, _Signature, _Key) -> ?nif_stub.
ecdsa_verify_nif(_Type, _Digest, _Signature, _Curve, _Key) -> ?nif_stub.

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

ec_key_generate(_Curve, _Key) -> ?nif_stub.

ecdh_compute_key_nif(_Others, _Curve, _My) -> ?nif_stub.

ec_curves() ->
    crypto_ec_curves:curves().

ec_curve(X) ->
    crypto_ec_curves:curve(X).

%%
%% EC
%%

term_to_nif_prime({prime_field, Prime}) ->
    {prime_field, ensure_int_as_bin(Prime)};
term_to_nif_prime(PrimeField) ->
    PrimeField.
term_to_nif_curve({A, B, Seed}) ->
    {ensure_int_as_bin(A), ensure_int_as_bin(B), Seed}.
nif_curve_params({PrimeField, Curve, BasePoint, Order, CoFactor}) ->
    {term_to_nif_prime(PrimeField), term_to_nif_curve(Curve), ensure_int_as_bin(BasePoint), ensure_int_as_bin(Order), ensure_int_as_bin(CoFactor)};
nif_curve_params(Curve) when is_atom(Curve) ->
    %% named curve
    crypto_ec_curves:curve(Curve).


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
		    rand_seed,
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
