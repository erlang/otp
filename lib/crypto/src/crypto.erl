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

-export([start/0, stop/0, info_lib/0, info_fips/0, supports/0, enable_fips_mode/1,
         version/0, bytes_to_integer/1]).
-export([hash/2, hash_init/1, hash_update/2, hash_final/1]).
-export([sign/4, verify/5]).
-export([generate_key/2, generate_key/3, compute_key/4]).
-export([hmac/3, hmac/4, hmac_init/2, hmac_update/2, hmac_final/1, hmac_final_n/2]).
-export([cmac/3, cmac/4]).
-export([exor/2, strong_rand_bytes/1, mod_pow/3]).
-export([rand_seed/0]).
-export([rand_seed_s/0]).
-export([rand_plugin_next/1]).
-export([rand_plugin_uniform/1]).
-export([rand_plugin_uniform/2]).
-export([rand_uniform/2]).
-export([block_encrypt/3, block_decrypt/3, block_encrypt/4, block_decrypt/4]).
-export([next_iv/2, next_iv/3]).
-export([stream_init/2, stream_init/3, stream_encrypt/2, stream_decrypt/2]).
-export([public_encrypt/4, private_decrypt/4]).
-export([private_encrypt/4, public_decrypt/4]).
-export([dh_generate_parameters/2, dh_check/1]). %% Testing see
-export([ec_curve/1, ec_curves/0]).
-export([rand_seed/1]).

-deprecated({rand_uniform, 2, next_major_release}).

%% This should correspond to the similar macro in crypto.c
-define(MAX_BYTES_TO_NIF, 20000). %%  Current value is: erlang:system_info(context_reductions) * 10

%% Used by strong_rand_float/0
-define(HALF_DBL_EPSILON, 1.1102230246251565e-16). % math:pow(2, -53)

%%-type ecdsa_digest_type() :: 'md5' | 'sha' | 'sha256' | 'sha384' | 'sha512'.
-type crypto_integer() :: binary() | integer().
%%-type ec_named_curve() :: atom().
%%-type ec_point() :: crypto_integer().
%%-type ec_basis() :: {tpbasis, K :: non_neg_integer()} | {ppbasis, K1 :: non_neg_integer(), K2 :: non_neg_integer(), K3 :: non_neg_integer()} | onbasis.
%%-type ec_field() :: {prime_field, Prime :: integer()} | {characteristic_two_field, M :: integer(), Basis :: ec_basis()}.
%%-type ec_prime() :: {A :: crypto_integer(), B :: crypto_integer(), Seed :: binary() | none}.
%%-type ec_curve_spec() :: {Field :: ec_field(), Prime :: ec_prime(), Point :: crypto_integer(), Order :: integer(), CoFactor :: none | integer()}.
%%-type ec_curve() :: ec_named_curve() | ec_curve_spec().
%%-type ec_key() :: {Curve :: ec_curve(), PrivKey :: binary() | undefined, PubKey :: ec_point() | undefined}.

-compile(no_native).
-on_load(on_load/0).
-define(CRYPTO_NIF_VSN,302).

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

-spec info_fips() -> not_supported | not_enabled | enabled.

info_fips() -> ?nif_stub.

-spec enable_fips_mode(boolean()) -> boolean().

enable_fips_mode(_) -> ?nif_stub.

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

-spec cmac(_, iodata(), iodata()) -> binary().
-spec cmac(_, iodata(), iodata(), integer()) -> binary().

cmac(Type, Key, Data) ->
    notsup_to_error(cmac_nif(Type, Key, Data)).
cmac(Type, Key, Data, MacSize) ->
    erlang:binary_part(cmac(Type, Key, Data), 0, MacSize).

%% Ecrypt/decrypt %%%

-spec block_encrypt(des_cbc | des_cfb |
                    des3_cbc | des3_cbf | des3_cfb | des_ede3 |
                    blowfish_cbc | blowfish_cfb64 | blowfish_ofb64 |
                    aes_cbc128 | aes_cfb8 | aes_cfb128 | aes_cbc256 | aes_ige256 |
                    aes_cbc |
                    rc2_cbc,
                    Key::iodata(), Ivec::binary(), Data::iodata()) -> binary();
                   (aes_gcm | chacha20_poly1305, Key::iodata(), Ivec::binary(), {AAD::binary(), Data::iodata()}) -> {binary(), binary()};
                   (aes_gcm, Key::iodata(), Ivec::binary(), {AAD::binary(), Data::iodata(), TagLength::1..16}) -> {binary(), binary()}.

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
block_encrypt(des3_cfb, Key0, Ivec, Data) ->
    Key = check_des3_key(Key0),
    block_crypt_nif(des_ede3_cfb, Key, Ivec, Data, true);
block_encrypt(aes_ige256, Key, Ivec, Data) ->
    notsup_to_error(aes_ige_crypt_nif(Key, Ivec, Data, true));
block_encrypt(aes_gcm, Key, Ivec, {AAD, Data}) ->
    aes_gcm_encrypt(Key, Ivec, AAD, Data);
block_encrypt(aes_gcm, Key, Ivec, {AAD, Data, TagLength}) ->
    aes_gcm_encrypt(Key, Ivec, AAD, Data, TagLength);
block_encrypt(chacha20_poly1305, Key, Ivec, {AAD, Data}) ->
    chacha20_poly1305_encrypt(Key, Ivec, AAD, Data).

-spec block_decrypt(des_cbc | des_cfb |
                    des3_cbc | des3_cbf | des3_cfb | des_ede3 |
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
block_decrypt(des3_cfb, Key0, Ivec, Data) ->
    Key = check_des3_key(Key0),
    block_crypt_nif(des_ede3_cfb, Key, Ivec, Data, false);
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
%% RAND - pseudo random numbers using RN_ and BN_ functions in crypto lib
%%
-spec strong_rand_bytes(non_neg_integer()) -> binary().
-spec rand_seed() -> rand:state().
-spec rand_seed_s() -> rand:state().
-spec rand_uniform(crypto_integer(), crypto_integer()) ->
			  crypto_integer().

strong_rand_bytes(Bytes) ->
    case strong_rand_bytes_nif(Bytes) of
        false -> erlang:error(low_entropy);
        Bin -> Bin
    end.
strong_rand_bytes_nif(_Bytes) -> ?nif_stub.


rand_seed() ->
    rand:seed(rand_seed_s()).

rand_seed_s() ->
    {#{ type => ?MODULE,
        bits => 64,
        next => fun ?MODULE:rand_plugin_next/1,
        uniform => fun ?MODULE:rand_plugin_uniform/1,
        uniform_n => fun ?MODULE:rand_plugin_uniform/2},
     no_seed}.

rand_plugin_next(Seed) ->
    {bytes_to_integer(strong_rand_range(1 bsl 64)), Seed}.

rand_plugin_uniform(State) ->
    {strong_rand_float(), State}.

rand_plugin_uniform(Max, State) ->
    {bytes_to_integer(strong_rand_range(Max)) + 1, State}.


strong_rand_range(Range) when is_integer(Range), Range > 0 ->
    BinRange = int_to_bin(Range),
    strong_rand_range(BinRange);
strong_rand_range(BinRange) when is_binary(BinRange) ->
    case strong_rand_range_nif(BinRange) of
        false ->
            erlang:error(low_entropy);
        <<BinResult/binary>> ->
            BinResult
    end.
strong_rand_range_nif(_BinRange) -> ?nif_stub.

strong_rand_float() ->
    WholeRange = strong_rand_range(1 bsl 53),
    ?HALF_DBL_EPSILON * bytes_to_integer(WholeRange).

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
	error -> erlang:error(badkey, [rsa, Type, {digest, Digest}, Key]);
	Sign -> Sign
    end;
sign(dss, Type, {digest, Digest}, Key) ->
    case dss_sign_nif(Type, Digest, map_ensure_int_as_bin(Key)) of
	error -> erlang:error(badkey, [dss, Type, {digest, Digest}, Key]);
	Sign -> Sign
    end;
sign(ecdsa, Type, {digest, Digest}, [Key, Curve]) ->
    case ecdsa_sign_nif(Type, Digest, nif_curve_params(Curve), ensure_int_as_bin(Key)) of
	error -> erlang:error(badkey, [ecdsa, Type, {digest, Digest}, [Key, Curve]]);
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
	    erlang:error(encrypt_failed, [rsa, BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%% Binary, Key = [E,N,D]
private_decrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_private_crypt(BinMesg, map_ensure_int_as_bin(Key), Padding, false) of
	error ->
	    erlang:error(decrypt_failed, [rsa, BinMesg,Key, Padding]);
	Sign -> Sign
    end.


%% Binary, Key = [E,N,D]
private_encrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_private_crypt(BinMesg, map_ensure_int_as_bin(Key), Padding, true) of
	error ->
	    erlang:error(encrypt_failed, [rsa, BinMesg,Key, Padding]);
	Sign -> Sign
    end.

%% Binary, Key = [E,N]
public_decrypt(rsa, BinMesg, Key, Padding) ->
    case rsa_public_crypt(BinMesg, map_ensure_int_as_bin(Key), Padding, false) of
	error ->
	    erlang:error(decrypt_failed, [rsa, BinMesg,Key, Padding]);
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

generate_key(dh, DHParameters0, PrivateKey) ->
    {DHParameters, Len} =
        case DHParameters0 of
            [P,G,L] -> {[P,G], L};
            [P,G] -> {[P,G], 0}
        end,
    dh_generate_key_nif(ensure_int_as_bin(PrivateKey),
			map_ensure_int_as_bin(DHParameters),
                        0, Len);

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

generate_key(rsa, {ModulusSize, PublicExponent}, undefined) ->
    case rsa_generate_key_nif(ModulusSize, ensure_int_as_bin(PublicExponent)) of
        error ->
            erlang:error(computation_failed,
                         [rsa,{ModulusSize,PublicExponent}]);
        Private ->
            {lists:sublist(Private, 2), Private}
    end;

generate_key(ecdh, Curve, PrivKey) ->
    ec_key_generate(nif_curve_params(Curve), ensure_int_as_bin(PrivKey)).

compute_key(dh, OthersPublicKey, MyPrivateKey, DHParameters) ->
    case dh_compute_key_nif(ensure_int_as_bin(OthersPublicKey),
			    ensure_int_as_bin(MyPrivateKey),
			    map_ensure_int_as_bin(DHParameters)) of
	error -> erlang:error(computation_failed,
			      [dh,OthersPublicKey,MyPrivateKey,DHParameters]);
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
    FipsMode = application:get_env(crypto, fips_mode, false) == true,
    Status = case erlang:load_nif(Lib, {?CRYPTO_NIF_VSN,LibBin,FipsMode}) of
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
			     erlang:load_nif(ArchLib, {?CRYPTO_NIF_VSN,ArchBin,FipsMode})
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
%%% Internal functions 
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

%% CMAC

cmac_nif(_Type, _Key, _Data) -> ?nif_stub.

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
rc4_set_key(_Key) -> ?nif_stub.
rc4_encrypt_with_state(_State, _Data) -> ?nif_stub.

%% Secure remote password  -------------------------------------------------------------------

user_srp_gen_key(Private, Generator, Prime) ->
    %% Ensure the SRP algorithm is disabled in FIPS mode
    case info_fips() of
        enabled -> erlang:error(notsup);
        _       -> ok
    end,
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
    <<U:32/bits, _/binary>> = hash(sha, HostPublic),
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
%% RSA Rivest-Shamir-Adleman functions
%%

rsa_generate_key_nif(_Bits, _Exp) -> ?nif_stub.

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
dh_generate_key_nif(_PrivateKey, _DHParameters, _Mpint, _Length) -> ?nif_stub.

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% MyPrivKey, OthersPublicKey = mpint()
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

%%--------------------------------------------------------------------
%%
-type rsa_padding() :: 'rsa_pkcs1_padding' | 'rsa_pkcs1_oaep_padding' | 'rsa_no_padding'.

rsa_public_crypt(_BinMsg, _Key, _Padding, _IsEncrypt) -> ?nif_stub.

rsa_private_crypt(_BinMsg, _Key, _Padding, _IsEncrypt) -> ?nif_stub.

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

%%
%% mod_exp - utility for rsa generation and SRP
%%
mod_exp_nif(_Base,_Exp,_Mod,_bin_hdr) -> ?nif_stub.

