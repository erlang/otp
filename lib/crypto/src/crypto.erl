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

-export([start/0, stop/0, info/0, info_lib/0, version/0]).
-export([hash/2, hash_init/1, hash_update/2, hash_final/1]).
-export([md4/1, md4_init/0, md4_update/2, md4_final/1]).
-export([md5/1, md5_init/0, md5_update/2, md5_final/1]).
-export([sha/1, sha_init/0, sha_update/2, sha_final/1]).
-export([sha224/1, sha224_init/0, sha224_update/2, sha224_final/1]).
-export([sha256/1, sha256_init/0, sha256_update/2, sha256_final/1]).
-export([sha384/1, sha384_init/0, sha384_update/2, sha384_final/1]).
-export([sha512/1, sha512_init/0, sha512_update/2, sha512_final/1]).
-export([md5_mac/2, md5_mac_96/2, sha_mac/2, sha_mac/3, sha_mac_96/2]).
-export([sha224_mac/2, sha224_mac/3]).
-export([sha256_mac/2, sha256_mac/3]).
-export([sha384_mac/2, sha384_mac/3]).
-export([sha512_mac/2, sha512_mac/3]).
-export([hmac/3, hmac/4, hmac_init/2, hmac_update/2, hmac_final/1, hmac_final_n/2]).
-export([des_cbc_encrypt/3, des_cbc_decrypt/3, des_cbc_ivec/1]).
-export([des_ecb_encrypt/2, des_ecb_decrypt/2]).
-export([des_cfb_encrypt/3, des_cfb_decrypt/3, des_cfb_ivec/2]).
-export([des3_cbc_encrypt/5, des3_cbc_decrypt/5]).
-export([des3_cfb_encrypt/5, des3_cfb_decrypt/5]).
-export([blowfish_ecb_encrypt/2, blowfish_ecb_decrypt/2]).
-export([blowfish_cbc_encrypt/3, blowfish_cbc_decrypt/3]).
-export([blowfish_cfb64_encrypt/3, blowfish_cfb64_decrypt/3]).
-export([blowfish_ofb64_encrypt/3]).
-export([des_ede3_cbc_encrypt/5, des_ede3_cbc_decrypt/5]).
-export([aes_cfb_128_encrypt/3, aes_cfb_128_decrypt/3]).
-export([exor/2]).
-export([rc4_encrypt/2, rc4_set_key/1, rc4_encrypt_with_state/2]).
-export([rc2_cbc_encrypt/3, rc2_cbc_decrypt/3, rc2_40_cbc_encrypt/3, rc2_40_cbc_decrypt/3]).
-export([dss_verify/3, dss_verify/4, rsa_verify/3, rsa_verify/4]).
-export([dss_sign/2, dss_sign/3, rsa_sign/2, rsa_sign/3]).
-export([rsa_public_encrypt/3, rsa_private_decrypt/3]).
-export([rsa_private_encrypt/3, rsa_public_decrypt/3]).
-export([dh_generate_key/1, dh_generate_key/2, dh_compute_key/3]).
-export([rand_bytes/1, rand_bytes/3, rand_uniform/2]).
-export([strong_rand_bytes/1, strong_rand_mpint/3]).
-export([mod_exp/3, mpint/1, erlint/1]).
-export([srp_mod_exp/3, srp_value_B/5]).
-export([srp3_value_u/1, srp6_value_u/3, srp6a_multiplier/2]).
-export([srp_client_secret/7, srp_server_secret/5]).

%% -export([idea_cbc_encrypt/3, idea_cbc_decrypt/3]).
-export([aes_cbc_128_encrypt/3, aes_cbc_128_decrypt/3]).
-export([aes_cbc_256_encrypt/3, aes_cbc_256_decrypt/3]).
-export([aes_cbc_ivec/1]).
-export([aes_ctr_encrypt/3, aes_ctr_decrypt/3]).
-export([aes_ctr_stream_init/2, aes_ctr_stream_encrypt/2, aes_ctr_stream_decrypt/2]).

-export([dh_generate_parameters/2, dh_check/1]). %% Testing see below


-define(FUNC_LIST, [md4, md4_init, md4_update, md4_final,
		    md5, md5_init, md5_update, md5_final,
		    sha, sha_init, sha_update, sha_final,
		    sha224, sha224_init, sha224_update, sha224_final,
		    sha256, sha256_init, sha256_update, sha256_final,
		    sha384, sha384_init, sha384_update, sha384_final,
		    sha512, sha512_init, sha512_update, sha512_final,
		    md5_mac,  md5_mac_96,
		    sha_mac,  sha_mac_96,
		    sha224_mac, sha256_mac, sha384_mac, sha512_mac,
		    des_cbc_encrypt, des_cbc_decrypt,
		    des_cfb_encrypt, des_cfb_decrypt,
		    des_ecb_encrypt, des_ecb_decrypt,
		    des3_cbc_encrypt, des3_cbc_decrypt,
		    des3_cfb_encrypt, des3_cfb_decrypt,
		    aes_cfb_128_encrypt, aes_cfb_128_decrypt,
		    rand_bytes,
		    strong_rand_bytes,
		    strong_rand_mpint,
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
		    aes_ctr_encrypt, aes_ctr_decrypt,
                    aes_ctr_stream_init, aes_ctr_stream_encrypt, aes_ctr_stream_decrypt,
		    aes_cbc_ivec, blowfish_cbc_encrypt, blowfish_cbc_decrypt,
		    blowfish_cfb64_encrypt, blowfish_cfb64_decrypt,
		    blowfish_ecb_encrypt, blowfish_ecb_decrypt, blowfish_ofb64_encrypt,
		    des_cbc_ivec, des_cfb_ivec, erlint, mpint,
		    hash, hash_init, hash_update, hash_final,
		    hmac, hmac_init, hmac_update, hmac_final, hmac_final_n, info,
		    rc2_cbc_encrypt, rc2_cbc_decrypt,
		    srp_mod_exp, srp_value_B,
		    srp3_value_u, srp6_value_u, srp6a_multiplier,
		    srp_client_secret, srp_server_secret,
		    info_lib]).

-type rsa_digest_type() :: 'md5' | 'sha' | 'sha224' | 'sha256' | 'sha384' | 'sha512'.
-type dss_digest_type() :: 'none' | 'sha'.
-type data_or_digest() :: binary() | {digest, binary()}.
-type crypto_integer() :: binary() | integer().

-define(nif_stub,nif_stub_error(?LINE)).

-on_load(on_load/0).

-define(CRYPTO_NIF_VSN,201).

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

-spec hash(_, iodata()) -> binary().
hash(md5, Data)          -> md5(Data);
hash(md4, Data)          -> md4(Data);
hash(sha, Data)          -> sha(Data);
hash(ripemd160, Data)    -> ripemd160(Data);
hash(sha224, Data)       -> sha224(Data);
hash(sha256, Data)       -> sha256(Data);
hash(sha384, Data)       -> sha384(Data);
hash(sha512, Data)       -> sha512(Data).

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

hash_update({md5,Context}, Data)       -> {md5, md5_update(Context,Data)};
hash_update({md4,Context}, Data)       -> {md4, md4_update(Context,Data)};
hash_update({sha,Context}, Data)       -> {sha, sha_update(Context,Data)};
hash_update({ripemd160,Context}, Data) -> {ripemd160, ripemd160_update(Context,Data)};
hash_update({sha224,Context}, Data)    -> {sha224, sha224_update(Context,Data)};
hash_update({sha256,Context}, Data)    -> {sha256, sha256_update(Context,Data)};
hash_update({sha384,Context}, Data)    -> {sha384, sha384_update(Context,Data)};
hash_update({sha512,Context}, Data)    -> {sha512, sha512_update(Context,Data)}.

-spec hash_final(_) -> binary().

hash_final({md5,Context})       -> md5_final(Context);
hash_final({md4,Context})       -> md4_final(Context);
hash_final({sha,Context})       -> sha_final(Context);
hash_final({ripemd160,Context}) -> ripemd160_final(Context);
hash_final({sha224,Context})    -> sha224_final(Context);
hash_final({sha256,Context})    -> sha256_final(Context);
hash_final({sha384,Context})    -> sha384_final(Context);
hash_final({sha512,Context})    -> sha512_final(Context).

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

%%
%%  MESSAGE AUTHENTICATION CODES
%%

%%
%%  HMAC (multiple hash options)
%%

-spec hmac(_, iodata(), iodata()) -> binary().
-spec hmac(_, iodata(), iodata(), integer()) -> binary().
-spec hmac_init(atom(), iodata()) -> binary().                             
-spec hmac_update(binary(), iodata()) -> binary().
-spec hmac_final(binary()) -> binary().                             
-spec hmac_final_n(binary(), integer()) -> binary().                             

hmac(md5, Key, Data)    -> md5_mac(Key, Data);
hmac(sha, Key, Data)    -> sha_mac(Key, Data);
hmac(sha224, Key, Data) -> sha224_mac(Key, Data);
hmac(sha256, Key, Data) -> sha256_mac(Key, Data);
hmac(sha384, Key, Data) -> sha384_mac(Key, Data);
hmac(sha512, Key, Data) -> sha512_mac(Key, Data).

hmac(md5, Key, Data, Size)    -> md5_mac_n(Key, Data, Size);
hmac(sha, Key, Data, Size)    -> sha_mac(Key, Data, Size);
hmac(sha224, Key, Data, Size) -> sha224_mac(Key, Data, Size);
hmac(sha256, Key, Data, Size) -> sha256_mac(Key, Data, Size);
hmac(sha384, Key, Data, Size) -> sha384_mac(Key, Data, Size);
hmac(sha512, Key, Data, Size) -> sha512_mac(Key, Data, Size).

hmac_init(_Type, _Key) -> ?nif_stub.
hmac_update(_Context, _Data) -> ? nif_stub.
hmac_final(_Context) -> ? nif_stub.
hmac_final_n(_Context, _HashLen) -> ? nif_stub.
     
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
-spec strong_rand_bytes(non_neg_integer()) -> binary().
-spec rand_uniform(crypto_integer(), crypto_integer()) ->
			  crypto_integer().
-spec strong_rand_mpint(Bits::non_neg_integer(),
			Top::-1..1,
			Bottom::0..1) -> binary().

rand_bytes(_Bytes) -> ?nif_stub.

strong_rand_bytes(Bytes) ->
    case strong_rand_bytes_nif(Bytes) of
        false -> erlang:error(low_entropy);
        Bin -> Bin
    end.
strong_rand_bytes_nif(_Bytes) -> ?nif_stub.

rand_bytes(_Bytes, _Topmask, _Bottommask) -> ?nif_stub.

strong_rand_mpint(Bits, Top, Bottom) -> 
    case strong_rand_mpint_nif(Bits,Top,Bottom) of
        false -> erlang:error(low_entropy);
        Bin -> Bin
    end.
strong_rand_mpint_nif(_Bits, _Top, _Bottom) -> ?nif_stub.


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
-spec dss_verify(data_or_digest(), binary(), [binary()]) -> boolean().
-spec dss_verify(dss_digest_type(), data_or_digest(), binary(), [binary()]) -> boolean().
-spec rsa_verify(data_or_digest(), binary(), [binary()]) -> boolean().
-spec rsa_verify(rsa_digest_type(), data_or_digest(), binary(), [binary()]) ->
			boolean().

%% Key = [P,Q,G,Y]   P,Q,G=DSSParams  Y=PublicKey
dss_verify(Data,Signature,Key) ->
    dss_verify(sha, Data, Signature, Key).    
dss_verify(_Type,_Data,_Signature,_Key) -> ?nif_stub.    

% Key = [E,N]  E=PublicExponent N=PublicModulus
rsa_verify(Data,Signature,Key) ->
    rsa_verify_nif(sha, Data,Signature,Key).
rsa_verify(Type, DataOrDigest, Signature, Key) ->
    case rsa_verify_nif(Type, DataOrDigest, Signature, Key) of
    	notsup -> erlang:error(notsup);
	Bool -> Bool
    end.

rsa_verify_nif(_Type, _Data, _Signature, _Key) -> ?nif_stub.


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
dss_sign(Type, DataOrDigest, Key) ->
    case dss_sign_nif(Type,DataOrDigest,Key) of
	error -> erlang:error(badkey, [DataOrDigest, Key]);
	Sign -> Sign
    end.

dss_sign_nif(_Type,_Data,_Key) -> ?nif_stub.

%% Key = [E,N,D]  E=PublicExponent N=PublicModulus  D=PrivateExponent
rsa_sign(DataOrDigest,Key) ->
    rsa_sign(sha, DataOrDigest, Key).
rsa_sign(Type, DataOrDigest, Key) ->
    case rsa_sign_nif(Type,DataOrDigest,Key) of
	error -> erlang:error(badkey, [Type,DataOrDigest,Key]);
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
%% SRP functions
%%
-spec srp_mod_exp(binary(), binary(), binary()) -> binary().
srp_mod_exp(Generator, Exponent, Prime) ->
    Verifier = mod_exp_nif(bin2bn(Generator), bin2bn(Exponent), bin2bn(Prime)),
    case erlint(Verifier) of
	0 -> error;
	_ -> bn2bin(Verifier)
    end.

-spec srp_value_B(binary(), integer(), binary(), binary(), binary()) -> binary().
srp_value_B(Multiplier, Verifier, Generator, Exponent, Prime) ->
    srp_value_B_nif(srp_multiplier(Multiplier), Verifier, Generator, Exponent, Prime).

srp_value_B_nif(_Multiplier, _Verifier, _Generator, _Exponent, _Prime) -> ?nif_stub.

-spec srp_client_secret(binary(), binary(), binary(), integer()|binary(), binary(), binary(), binary()) -> binary().
srp_client_secret(A, U, B, Multiplier, Generator, Exponent, Prime) ->
    srp_client_secret_nif(A, U, B, srp_multiplier(Multiplier), Generator, Exponent, Prime).

srp_client_secret_nif(_A, _U, _B, _Multiplier, _Generator, _Exponent, _Prime) -> ?nif_stub.

-spec srp_server_secret(binary(), binary(), binary(), binary(), binary()) -> binary().
srp_server_secret(_Verifier, _B, _U, _A, _Prime) -> ?nif_stub.

-spec srp6a_multiplier(binary(), binary()) -> binary().
srp6a_multiplier(Generator, Prime) ->
    %% k = SHA1(N | PAD(g))
    C0 = sha_init(),
    C1 = sha_update(C0, Prime),
    C2 = sha_update(C1, srp_pad_to(erlang:byte_size(Prime), Generator)),
    sha_final(C2).

-spec srp3_value_u(binary()) -> binary().
srp3_value_u(B) ->
    %% The parameter u is a 32-bit unsigned integer which takes its value
    %% from the first 32 bits of the SHA1 hash of B, MSB first.
    <<U:32/bits, _/binary>> = sha(B),
    U.

-spec srp6_value_u(binary(), binary(), binary()) -> binary().
srp6_value_u(A, B, Prime) ->
    %% SHA1(PAD(A) | PAD(B))
    PadLength = erlang:byte_size(Prime),
    C0 = sha_init(),
    C1 = sha_update(C0, srp_pad_to(PadLength, A)),
    C2 = sha_update(C1, srp_pad_to(PadLength, B)),
    sha_final(C2).

%%
%%  LOCAL FUNCTIONS
%%

srp_pad_length(Width, Length) ->
    (Width - Length rem Width) rem Width.

srp_pad_to(Width, Binary) ->
    case srp_pad_length(Width, size(Binary)) of
        0 -> Binary;
        N -> << 0:(N*8), Binary/binary>>
    end.

srp_multiplier(Multiplier) when is_binary(Multiplier) ->
    bin2bn(Multiplier);
srp_multiplier(Multiplier) when is_integer(Multiplier) ->
    mpint(Multiplier).

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

bin2bn(Bin) ->
    Len = erlang:byte_size(Bin),
    <<?UINT32(Len), Bin/binary>>.

bn2bin(<<_:32, Bin/binary>>) ->
    Bin.
