%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2023. All Rights Reserved.
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

-export([start/0, stop/0, info/0, info_lib/0, info_fips/0, supports/0, enable_fips_mode/1,
         version/0, bytes_to_integer/1]).
-export([cipher_info/1, hash_info/1]).
-export([hash/2, hash_xof/3, hash_init/1, hash_update/2, hash_final/1, hash_final_xof/2]).
-export([sign/4, sign/5, verify/5, verify/6]).
-export([generate_key/2, generate_key/3, compute_key/4]).
-export([exor/2, strong_rand_bytes/1, mod_pow/3]).
-export([rand_seed/0, rand_seed_alg/1, rand_seed_alg/2]).
-export([rand_seed_s/0, rand_seed_alg_s/1, rand_seed_alg_s/2]).
-export([rand_plugin_next/1]).
-export([rand_plugin_aes_next/1, rand_plugin_aes_jump/1]).
-export([rand_plugin_uniform/1]).
-export([rand_plugin_uniform/2]).
-export([rand_cache_plugin_next/1]).
-export([rand_uniform/2]).
-export([public_encrypt/4, private_decrypt/4]).
-export([private_encrypt/4, public_decrypt/4]).
-export([privkey_to_pubkey/2]).
-export([ec_curve/1, ec_curves/0]).
-export([rand_seed/1]).
-export([format_error/2]).
-export([pbkdf2_hmac/5]).

%%%----------------------------------------------------------------
%% Deprecated functions
-deprecated([crypto_dyn_iv_init/3,
             crypto_dyn_iv_update/3
           ]).

%%%----------------------------------------------------------------
%% Removed functions.
%%
%% Old interface. Now implemented with the New interface.
%% Removed in OTP-24.0 See OTP-16232 (deprecation) and OTP-16656 (removal)

-removed([{next_iv, '_', "see the 'New and Old API' chapter of the CRYPTO User's guide"},
          {hmac, 3, "use crypto:mac/4 instead"},
          {hmac, 4, "use crypto:macN/5 instead"},
          {hmac_init, 2, "use crypto:mac_init/3 instead"},
          {hmac_update, 2, "use crypto:mac_update/2 instead"},
          {hmac_final, 1, "use crypto:mac_final/1 instead"},
          {hmac_final_n, 2, "use crypto:mac_finalN/2 instead"},
          {cmac, 3, "use crypto:mac/4 instead"},
          {cmac, 4, "use crypto:macN/5 instead"},
          {poly1305, 2, "use crypto:mac/3 instead"},
          {stream_init, '_', "use crypto:crypto_init/3 + crypto:crypto_update/2 + "
                             "crypto:crypto_final/1 or crypto:crypto_one_time/4 instead"},
          {stream_encrypt, 2, "use crypto:crypto_update/2 instead"},
          {stream_decrypt, 2, "use crypto:crypto_update/2 instead"},
          {block_encrypt, 3,  "use crypto:crypto_one_time/4 or crypto:crypto_init/3 + "
                              "crypto:crypto_update/2 + crypto:crypto_final/1 instead"},
          {block_encrypt, 4,  "use crypto:crypto_one_time/5, crypto:crypto_one_time_aead/6,7 "
                              "or crypto:crypto_(dyn_iv)?_init + "
                              "crypto:crypto_(dyn_iv)?_update + crypto:crypto_final instead"},
          {block_decrypt, 3,  "use crypto:crypto_one_time/4 or crypto:crypto_init/3 + "
                              "crypto:crypto_update/2 + crypto:crypto_final/1 instead"},
          {block_decrypt, 4,  "use crypto:crypto_one_time/5, crypto:crypto_one_time_aead/6,7 "
                              "or crypto:crypto_(dyn_iv)?_init + "
                              "crypto:crypto_(dyn_iv)?_update + crypto:crypto_final instead"}
         ]).

-removed_type([{retired_cbc_cipher_aliases, 0, "Use aes_*_cbc or des_ede3_cbc"},
               {retired_cfb_cipher_aliases, 0, "Use aes_*_cfb8, aes_*_cfb128 or des_ede3_cfb"},
               {retired_ctr_cipher_aliases, 0, "Use aes_*_ctr"},
               {retired_ecb_cipher_aliases, 0, "Use aes_*_ecb"},
               {stream_state, 0, "see the 'New and Old API' chapter of the CRYPTO User's guide"},
               {hmac_state,   0, "see the 'New and Old API' chapter of the CRYPTO User's guide"}
              ]).

%%%----------------------------------------------------------------
%% New interface
-export([crypto_init/4, crypto_init/3,
         crypto_update/2,

         crypto_one_time/4, crypto_one_time/5,
         crypto_one_time_aead/6, crypto_one_time_aead/7,

         crypto_dyn_iv_init/3,
         crypto_dyn_iv_update/3,
         crypto_final/1,
         crypto_get_data/1,

         hash_equals/2,

         supports/1,
         mac/3, mac/4, macN/4, macN/5,
         mac_init/2, mac_init/3, mac_update/2, mac_final/1, mac_finalN/2
        ]).

%%%----------------------------------------------------------------
%% Engine
-export([
         engine_get_all_methods/0,
         engine_load/3,
         engine_load/4,
         engine_unload/1,
         engine_unload/2,
         engine_by_id/1,
         engine_list/0,
         engine_ctrl_cmd_string/3,
         engine_ctrl_cmd_string/4,
         engine_add/1,
         engine_remove/1,
         engine_register/2,
         engine_unregister/2,
         engine_get_id/1,
         engine_get_name/1,
         ensure_engine_loaded/2,
         ensure_engine_loaded/3,
         ensure_engine_unloaded/1,
         ensure_engine_unloaded/2
        ]).

-nifs([info_nif/0, info_lib/0, info_fips/0, enable_fips_mode_nif/1,
       hash_algorithms/0, pubkey_algorithms/0, cipher_algorithms/0,
       mac_algorithms/0, curve_algorithms/0, rsa_opts_algorithms/0,
       hash_info/1, hash_nif/2, hash_init_nif/1, hash_update_nif/2,
       hash_final_nif/1, hash_final_xof_nif/2, mac_nif/4, mac_init_nif/3, mac_update_nif/2,
       mac_final_nif/1, cipher_info_nif/1, ng_crypto_init_nif/4,
       ng_crypto_update_nif/2, ng_crypto_update_nif/3, ng_crypto_final_nif/1,
       ng_crypto_get_data_nif/1, ng_crypto_one_time_nif/5,
       strong_rand_bytes_nif/1, strong_rand_range_nif/1, rand_uniform_nif/2,
       mod_exp_nif/4, do_exor/2, hash_equals_nif/2, pbkdf2_hmac_nif/5,
       pkey_sign_nif/5, pkey_verify_nif/6, pkey_crypt_nif/6,
       rsa_generate_key_nif/2, dh_generate_key_nif/4, dh_compute_key_nif/3,
       evp_compute_key_nif/3, evp_generate_key_nif/2, privkey_to_pubkey_nif/2,
       srp_value_B_nif/5, srp_user_secret_nif/7, srp_host_secret_nif/5,
       ec_generate_key_nif/2, ecdh_compute_key_nif/3, rand_seed_nif/1,
       aead_cipher_nif/7, engine_by_id_nif/1, engine_init_nif/1,
       engine_free_nif/1, engine_load_dynamic_nif/0,
       engine_ctrl_cmd_strings_nif/3, engine_register_nif/2,
       engine_unregister_nif/2, engine_add_nif/1, engine_remove_nif/1,
       engine_get_first_nif/0, engine_get_next_nif/1, engine_get_id_nif/1,
       engine_get_name_nif/1, engine_get_all_methods_nif/0,
       ensure_engine_loaded_nif/2
      ]).

-export_type([ %% A minimum exported: only what public_key needs.
               dh_private/0,
               dh_public/0,
               dss_digest_type/0,
               ec_named_curve/0,
               ecdsa_digest_type/0,
               pk_encrypt_decrypt_opts/0,
               pk_sign_verify_opts/0,
               rsa_digest_type/0,
               sha1/0,
               sha2/0
             ]).

-export_type([engine_ref/0,
              key_id/0,
              password/0
             ]).

%%% Opaque types must be exported :(
-export_type([
              hash_state/0,
              crypto_state/0,
              mac_state/0
             ]).

%% Private. For tests.
-export([packed_openssl_version/4, engine_methods_convert_to_bitmask/2,
	 get_test_engine/0]).
-export([rand_plugin_aes_jump_2pow20/1]).

-deprecated({rand_uniform, 2, "use rand:uniform/1 instead"}).

%% This should correspond to the similar macro in crypto.c
-define(MAX_BYTES_TO_NIF, 20000). %%  Current value is: erlang:system_info(context_reductions) * 10

%% Used by strong_rand_float/0
-define(HALF_DBL_EPSILON, 1.1102230246251565e-16). % math:pow(2, -53)


%%% ===== BEGIN NEW TYPING ====

%%% Basic
-type key_integer() :: integer() | binary(). % Always binary() when used as return value

%%% Keys
-type rsa_public() :: [key_integer()] . % [E, N]
-type rsa_private() :: [key_integer()] . % [E, N, D] | [E, N, D, P1, P2, E1, E2, C]
-type rsa_params() :: {ModulusSizeInBits::integer(), PublicExponent::key_integer()} .

-type dss_public() :: [key_integer()] . % [P, Q, G, Y]
-type dss_private() :: [key_integer()] . % [P, Q, G, X]

-type ecdsa_public()  :: key_integer() .
-type ecdsa_private() :: key_integer() .
-type ecdsa_params()  :: ec_named_curve() | ec_explicit_curve() .

-type eddsa_public()  :: key_integer() .
-type eddsa_private() :: key_integer() .
-type eddsa_params()  :: edwards_curve_ed() .

-type srp_public() :: key_integer() .
-type srp_private() :: key_integer() .
-type srp_gen_params()  :: {user,srp_user_gen_params()}  | {host,srp_host_gen_params()}.
-type srp_comp_params() :: {user,srp_user_comp_params()} | {host,srp_host_comp_params()}.
-type srp_user_gen_params() :: list(binary() | atom() | list()) .
-type srp_host_gen_params() :: list(binary() | atom() | list()) .
-type srp_user_comp_params() :: list(binary() | atom()) .
-type srp_host_comp_params() :: list(binary() | atom()) .

-type dh_public() :: key_integer() .
-type dh_private() :: key_integer() .
-type dh_params() :: [key_integer()] . % [P, G] | [P, G, PrivateKeyBitLength]

-type ecdh_public()  :: key_integer() .
-type ecdh_private() :: key_integer() .
-type ecdh_params()  :: ec_named_curve() | edwards_curve_dh() | ec_explicit_curve() .


%%% Curves

-type ec_explicit_curve() :: {Field :: ec_field(),
                              Curve :: ec_curve(),
                              BasePoint :: binary(),
                              Order :: binary(),
                              CoFactor :: none | % FIXME: Really?
                                          binary()
                             } .

-type ec_curve() :: {A :: binary(),
                     B :: binary(),
                     Seed :: none | binary()
                    } .

-type ec_field() ::  ec_prime_field() | ec_characteristic_two_field() .

-type ec_prime_field()              :: {prime_field, Prime :: integer()} .
-type ec_characteristic_two_field() :: {characteristic_two_field, M :: integer(), Basis :: ec_basis()} .

-type ec_basis() :: {tpbasis, K :: non_neg_integer()}
                  | {ppbasis, K1 :: non_neg_integer(), K2 :: non_neg_integer(), K3 :: non_neg_integer()}
                  |  onbasis .

-type ec_named_curve() :: brainpoolP160r1
                        | brainpoolP160t1
                        | brainpoolP192r1
                        | brainpoolP192t1
                        | brainpoolP224r1
                        | brainpoolP224t1
                        | brainpoolP256r1
                        | brainpoolP256t1
                        | brainpoolP320r1
                        | brainpoolP320t1
                        | brainpoolP384r1
                        | brainpoolP384t1
                        | brainpoolP512r1
                        | brainpoolP512t1
                        | c2pnb163v1
                        | c2pnb163v2
                        | c2pnb163v3
                        | c2pnb176v1
                        | c2pnb208w1
                        | c2pnb272w1
                        | c2pnb304w1
                        | c2pnb368w1
                        | c2tnb191v1
                        | c2tnb191v2
                        | c2tnb191v3
                        | c2tnb239v1
                        | c2tnb239v2
                        | c2tnb239v3
                        | c2tnb359v1
                        | c2tnb431r1
                        | ipsec3
                        | ipsec4
                        | prime192v1
                        | prime192v2
                        | prime192v3
                        | prime239v1
                        | prime239v2
                        | prime239v3
                        | prime256v1
                        | secp112r1
                        | secp112r2
                        | secp128r1
                        | secp128r2
                        | secp160k1
                        | secp160r1
                        | secp160r2
                        | secp192k1
                        | secp192r1
                        | secp224k1
                        | secp224r1
                        | secp256k1
                        | secp256r1
                        | secp384r1
                        | secp521r1
                        | sect113r1
                        | sect113r2
                        | sect131r1
                        | sect131r2
                        | sect163k1
                        | sect163r1
                        | sect163r2
                        | sect193r1
                        | sect193r2
                        | sect233k1
                        | sect233r1
                        | sect239k1
                        | sect283k1
                        | sect283r1
                        | sect409k1
                        | sect409r1
                        | sect571k1
                        | sect571r1
                        | wtls1
                        | wtls10
                        | wtls11
                        | wtls12
                        | wtls3
                        | wtls4
                        | wtls5
                        | wtls6
                        | wtls7
                        | wtls8
                        | wtls9
                          .

-type edwards_curve_dh() :: x25519 | x448 .

-type edwards_curve_ed() :: ed25519 | ed448 .

%%%----------------------------------------------------------------
%%% New cipher schema
%%%
-type cipher() :: cipher_no_iv()
                | cipher_iv()
                | cipher_aead() .

-type cipher_no_iv() :: aes_128_ecb
                      | aes_192_ecb
                      | aes_256_ecb
                      | aes_ecb

                      | blowfish_ecb
                      | des_ecb
                      | rc4 .

-type cipher_iv() :: aes_128_cbc
                   | aes_192_cbc
                   | aes_256_cbc
                   | aes_cbc

                   | aes_128_ofb
                   | aes_192_ofb
                   | aes_256_ofb

                   | aes_128_cfb128
                   | aes_192_cfb128
                   | aes_256_cfb128
                   | aes_cfb128

                   | aes_128_cfb8
                   | aes_192_cfb8
                   | aes_256_cfb8
                   | aes_cfb8

                   | aes_128_ctr
                   | aes_192_ctr
                   | aes_256_ctr
                   | aes_ctr

                   | blowfish_cbc
                   | blowfish_cfb64
                   | blowfish_ofb64
                   | chacha20
                   | des_ede3_cbc
                   | des_ede3_cfb

                   | des_cbc
                   | des_cfb
                   | rc2_cbc .


-type cipher_aead() :: aes_128_ccm
                     | aes_192_ccm
                     | aes_256_ccm
                     | aes_ccm

                     | aes_128_gcm
                     | aes_192_gcm
                     | aes_256_gcm
                     | aes_gcm

                     | chacha20_poly1305 .


%%%----------------------------------------------------------------

-type rsa_digest_type()   :: sha1() | sha2() | md5 | ripemd160 .
-type dss_digest_type()   :: sha1() | sha2() .
-type ecdsa_digest_type() :: sha1() | sha2() .

-type sha1() :: sha .
-type sha2() :: sha224 | sha256 | sha384 | sha512 .
-type sha3() :: sha3_224 | sha3_256 | sha3_384 | sha3_512 .
-type sha3_xof() :: shake128 | shake256 .
-type blake2() :: blake2b | blake2s .

-type compatibility_only_hash() :: md5 | md4 .

-type crypto_integer() :: binary() | integer().

%% %%%--------------------------------------------------------------------
%% %%% Exceptions
%% %%%
%% %% Exceptions
%% %%   error:badarg
%% %%   error:notsup
%% -type run_time_error() :: badarg | notsup.


%% %% Exceptions
%% %%   error:{badarg, file_line_info, Reason::term()}
%% %%   error:{notsup,Reason::term()}
%% %%   error:{error,Reason::term()}
%% -type descriptive_error() :: {badarg | notsup | error, FileLineInfo::any, Reason::string()}.

%%--------------------------------------------------------------------
%% Compilation and loading
%%--------------------------------------------------------------------
-compile(no_native).
-on_load(on_load/0).
-define(CRYPTO_NIF_VSN,302).

%%--------------------------------------------------------------------
%% When generating documentation from crypto.erl, the macro ?CRYPTO_VSN is not defined.
%% That causes the doc generation to stop...
-ifndef(CRYPTO_VSN).
-define(CRYPTO_VSN, "??").
-endif.

%%--------------------------------------------------------------------
%% Call a nif and handle an error exceptions to fit into the error handling
%% in the Erlang shell.
%% If not called from a shell, an error exception will be propagated.

-define(nif_call(Call), ?nif_call(Call, undefined, {})).

-define(nif_call(Call, ArgMap), ?nif_call(Call, undefined, ArgMap)).

-define(nif_call(Call, Args0, ArgMap),
        try Call
        catch
            error
            : {Id, #{c_file_name := C_file,
                          c_file_line_num := C_line,
                          c_function_arg_num := ArgNum}, Msg}
            : Stack when is_list(C_file),
                         is_integer(C_line),
                         is_integer(ArgNum) ->
                error({Id, {C_file,C_line}, Msg},
                      err_find_args(Args0, Stack),
                      [{error_info, #{erl_function_arg_num => err_remap_C_argnum(ArgNum, ArgMap)}}]
                     )
        end).

%%--------------------------------------------------------------------
%% Error if the crypto nifs not are loaded

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
version() ->
    ?CRYPTO_VSN.

format_error({Ex, {C_file,C_line}, Msg}, [{_M,_F,_Args,Opts} | _CallStack]) when Ex == badarg ;
                                                                                 Ex == notsup ->
    case proplists:get_value(error_info, Opts) of
        #{erl_function_arg_num := ErlArgNum} ->
            FileMsg =
                io_lib:format("(Found in the internal file ~s at line ~p)", [C_file, C_line]),
            case ErlArgNum of
                undefined ->
                    #{general => [Msg," ",FileMsg]};
                N ->#{N => Msg,
                      general => FileMsg
                     }
            end
    end.


-spec start() -> ok | {error, Reason::term()}.
start() ->
    application:start(crypto).

-spec stop() -> ok | {error, Reason::term()}.
stop() ->
    application:stop(crypto).

-spec supports() -> [Support]
                        when Support :: {hashs,   Hashs}
                                      | {ciphers, Ciphers}
                                      | {public_keys, PKs}
                                      | {macs,    Macs}
                                      | {curves,  Curves}
                                      | {rsa_opts, RSAopts},
                             Hashs :: [sha1() | sha2() | sha3() | sha3_xof() | blake2() | ripemd160 | compatibility_only_hash()],
                             Ciphers :: [cipher()],
                             PKs :: [rsa | dss | ecdsa | dh | ecdh | eddh | ec_gf2m],
                             Macs :: [hmac | cmac | poly1305],
                             Curves :: [ec_named_curve() | edwards_curve_dh() | edwards_curve_ed()],
                             RSAopts :: [rsa_sign_verify_opt() | rsa_opt()] .
supports() ->
     [{hashs,       supports(hashs)},
      {ciphers,     supports(ciphers)}
      | [{T,supports(T)} || T <- [public_keys,
                                  macs,
                                  curves,
                                  rsa_opts]
        ]
     ].


-spec supports(Type) -> Support
                        when Type :: hashs
			           | ciphers
                                   | public_keys
                                   | macs
                                   | curves
                                   | rsa_opts,
			     Support :: Hashs
                                      | Ciphers
                                      | PKs
                                      | Macs
                                      | Curves
                                      | RSAopts,
                             Hashs :: [sha1() | sha2() | sha3() | sha3_xof() | blake2() | ripemd160 | compatibility_only_hash()],
                             Ciphers :: [cipher()],
                             PKs :: [rsa | dss | ecdsa | dh | ecdh | eddh | ec_gf2m],
                             Macs :: [hmac | cmac | poly1305],
                             Curves :: [ec_named_curve() | edwards_curve_dh() | edwards_curve_ed()],
                             RSAopts :: [rsa_sign_verify_opt() | rsa_opt()] .

-define(CURVES, '$curves$').

supports(hashs)       -> hash_algorithms();
supports(public_keys) -> pubkey_algorithms();
supports(ciphers)     -> add_cipher_aliases(cipher_algorithms());
supports(macs)        -> mac_algorithms();
supports(curves)      -> curve_algorithms();
supports(rsa_opts)    -> rsa_opts_algorithms().


-spec info_lib() -> [{Name,VerNum,VerStr}] when Name :: binary(),
                                                VerNum :: integer(),
                                                VerStr :: binary() .
info_lib() -> ?nif_stub.

-spec info() -> #{compile_type := normal | debug | valgrind | asan,
                 cryptolib_version_compiled => string() | undefined,
                  cryptolib_version_linked := string(),
                  link_type := dynamic | static,
                  otp_crypto_version := string()
                 }.
info() -> 
    (info_nif())#{otp_crypto_version => crypto:version()}.

info_nif() -> ?nif_stub.


-spec info_fips() -> not_supported | not_enabled | enabled.

info_fips() -> ?nif_stub.

-spec enable_fips_mode(Enable) -> Result when Enable :: boolean(),
                                              Result :: boolean().
enable_fips_mode(Enable) ->
    enable_fips_mode_nif(Enable).

enable_fips_mode_nif(_) -> ?nif_stub.

-spec pbkdf2_hmac(Digest, Pass, Salt, Iter, KeyLen) -> Result
          when Digest :: sha | sha224 | sha256 | sha384 | sha512,
               Pass :: binary(),
               Salt :: binary(),
               Iter :: pos_integer(),
               KeyLen :: pos_integer(),
               Result :: binary().
pbkdf2_hmac(Digest, Pass, Salt, Iter, KeyLen) ->
    ?nif_call(pbkdf2_hmac_nif(Digest, Pass, Salt, Iter, KeyLen)).

pbkdf2_hmac_nif(_, _, _, _, _) -> ?nif_stub.

%%%================================================================
%%%
%%% Hashing
%%%
%%%================================================================

-type hash_algorithm() :: sha1() | sha2() | sha3() | sha3_xof() | blake2() | ripemd160 | compatibility_only_hash() .
-type hash_xof_algorithm() :: sha3_xof() .

-spec hash_info(Type) -> Result
                             when Type :: hash_algorithm(),
                                  Result :: #{size := integer(),
                                              block_size := integer(),
                                              type := integer()
                                             } .
hash_info(Type) ->
    notsup_to_error(hash_info_nif(Type)).

-spec hash(Type, Data) -> Digest when Type :: hash_algorithm(),
                                      Data :: iodata(),
                                      Digest :: binary().
hash(Type, Data) ->
    Data1 = iolist_to_binary(Data),
    MaxBytes = max_bytes(),
    hash(Type, Data1, erlang:byte_size(Data1), MaxBytes).

-spec hash_xof(Type, Data, Length) -> Digest when Type :: hash_xof_algorithm(),
                                               Data :: iodata(),
                                               Length :: non_neg_integer(),
                                               Digest :: binary().
hash_xof(Type, Data, Length) ->
  Data1 = iolist_to_binary(Data),
  hash_xof(Type, Data1, erlang:byte_size(Data1), Length).

-opaque hash_state() :: reference().

-spec hash_init(Type) -> State when Type :: hash_algorithm(),
                                    State :: hash_state().
hash_init(Type) ->
    ?nif_call(hash_init_nif(Type)).

-spec hash_update(State, Data) -> NewState when State :: hash_state(),
                                                NewState :: hash_state(),
                                                Data :: iodata() .
hash_update(Context, Data) ->
    Data1 = iolist_to_binary(Data),
    MaxBytes = max_bytes(),
    hash_update(Context, Data1, erlang:byte_size(Data1), MaxBytes).

-spec hash_final(State) -> Digest when  State :: hash_state(),
                                        Digest :: binary().
hash_final(Context) ->
    ?nif_call(hash_final_nif(Context)).

-spec hash_final_xof(State, Length) -> Digest when State :: hash_state(),
                                                   Length :: non_neg_integer(),
                                                   Digest :: binary().
hash_final_xof(Context, Length) ->
    notsup_to_error(hash_final_xof_nif(Context, Length)).

%%%================================================================
%%%
%%% MACs (Message Authentication Codes)
%%%
%%%================================================================

-type hmac_hash_algorithm() ::  sha1() | sha2() | sha3() | compatibility_only_hash().

-type cmac_cipher_algorithm() :: aes_128_cbc    | aes_192_cbc    | aes_256_cbc    | aes_cbc
                               | blowfish_cbc
                               | des_cbc | des_ede3_cbc
                               | rc2_cbc
                                 .

%%%----------------------------------------------------------------
%%% Calculate MAC for the whole text at once

-spec mac(Type :: poly1305, Key, Data) -> Mac
                     when Key :: iodata(),
                          Data :: iodata(),
                          Mac :: binary().

mac(poly1305, Key, Data) -> mac(poly1305, undefined, Key, Data).


-spec mac(Type, SubType, Key, Data) -> Mac
                     when Type :: hmac | cmac | poly1305,
                          SubType :: hmac_hash_algorithm() | cmac_cipher_algorithm() | undefined,
                          Key :: iodata(),
                          Data :: iodata(),
                          Mac :: binary().

mac(Type, SubType, Key0, Data) ->
    Key = iolist_to_binary(Key0),
    ?nif_call(mac_nif(Type, alias(SubType,Key), Key, Data)).


-spec macN(Type :: poly1305, Key, Data, MacLength) -> Mac
                     when Key :: iodata(),
                          Data :: iodata(),
                          Mac :: binary(),
                          MacLength :: pos_integer().

macN(Type, Key, Data, MacLength) ->
    macN(Type, undefined, Key, Data, MacLength).


-spec macN(Type, SubType, Key, Data, MacLength) -> Mac
                     when Type :: hmac | cmac | poly1305,
                          SubType :: hmac_hash_algorithm() | cmac_cipher_algorithm() | undefined,
                          Key :: iodata(),
                          Data :: iodata(),
                          Mac :: binary(),
                          MacLength :: pos_integer().

macN(Type, SubType, Key, Data, MacLength) ->
    erlang:binary_part(mac(Type,SubType,Key,Data), 0, MacLength).


%%%----------------------------------------------------------------
%%% Calculate the MAC by uppdating by pieces of the text

-opaque mac_state() :: reference() .

-spec mac_init(Type :: poly1305, Key) -> State
                          when Key :: iodata(),
                               State :: mac_state() .
mac_init(poly1305, Key) ->
    ?nif_call(mac_init_nif(poly1305, undefined, Key)).


-spec mac_init(Type, SubType, Key) -> State
                          when Type :: hmac | cmac | poly1305,
                               SubType :: hmac_hash_algorithm() | cmac_cipher_algorithm() | undefined,
                               Key :: iodata(),
                               State :: mac_state() .
mac_init(Type, SubType, Key0) ->
    Key = iolist_to_binary(Key0),
    ?nif_call(mac_init_nif(Type, alias(SubType,Key), Key)).


-spec mac_update(State0, Data) -> State
                     when Data :: iodata(),
                          State0 :: mac_state(),
                          State :: mac_state().
mac_update(Ref, Data) ->
    ?nif_call(mac_update_nif(Ref, Data)).



-spec mac_final(State) -> Mac
                              when State :: mac_state(),
                                   Mac :: binary().
mac_final(Ref) ->
    ?nif_call(mac_final_nif(Ref)).


-spec mac_finalN(State, MacLength) -> Mac
                              when State :: mac_state(),
                                   MacLength :: pos_integer(),
                                   Mac :: binary().
mac_finalN(Ref, MacLength) ->
    erlang:binary_part(mac_final(Ref), 0, MacLength).


%%%----------------------------------------------------------------
%%% NIFs for the functions above

mac_nif(_Type, _SubType, _Key, _Data) -> ?nif_stub.

mac_init_nif(_Type, _SubType, _Key) -> ?nif_stub.
mac_update_nif(_Ref, _Data) -> ?nif_stub.
mac_final_nif(_Ref) -> ?nif_stub.

%%%================================================================
%%%
%%% The "Old API", kept for compatibility
%%%
%%%================================================================

%%%----------------------------------------------------------------
%%% Ciphers


%%%---- Cipher info
-spec cipher_info(Type) -> Result
                               when Type :: cipher(),
                                    Result :: #{key_length := integer(),
                                                iv_length := integer(),
                                                block_size := integer(),
                                                mode := CipherModes,
                                                type := undefined | integer(),
                                                prop_aead := boolean()
                                               },
                                    CipherModes :: undefined
                                                 | cbc_mode
                                                 | ccm_mode
                                                 | cfb_mode
                                                 | ctr_mode
                                                 | ecb_mode
                                                 | gcm_mode
                                                 | ige_mode
                                                 | ocb_mode
                                                 | ofb_mode
                                                 | wrap_mode
                                                 | xts_mode
                                                   .

cipher_info(Type) ->
    try cipher_info_nif(Type)
    catch
        %% These ciphers are not available via the EVP interface on older cryptolibs.
        error:notsup when Type == aes_128_ctr ->
            #{block_size => 1,iv_length => 16,key_length => 16,mode => ctr_mode,type => undefined};

        error:notsup when Type == aes_192_ctr ->
            #{block_size => 1,iv_length => 16,key_length => 24,mode => ctr_mode,type => undefined};

        error:notsup when Type == aes_256_ctr ->
            #{block_size => 1,iv_length => 16,key_length => 32,mode => ctr_mode,type => undefined};

        error:badarg ->
            %% Maybe an alias: we don't know the key length..
            case alias1(Type, 16) of
                Type ->
                    %% Not found, propagate
                    error(badarg);
                NewType ->
                    cipher_info(NewType)
            end
    end.

%%%================================================================
%%%
%%% Encrypt/decrypt, The "New API"
%%%
%%%================================================================

-opaque crypto_state() :: reference() .

-type crypto_opts() :: boolean()
                     | [ crypto_opt() ] .
-type crypto_opt() :: {encrypt,boolean()}
                    | {padding, padding()} .
-type padding() :: cryptolib_padding() | otp_padding().
-type cryptolib_padding() :: none | pkcs_padding .
-type otp_padding() :: zero | random .


%%%----------------------------------------------------------------
%%%
%%% Create and initialize a new state for encryption or decryption
%%%

-spec crypto_init(Cipher, Key, FlagOrOptions) -> State
                                                   when Cipher :: cipher_no_iv(),
                                                        Key :: iodata(),
                                                        FlagOrOptions :: crypto_opts() | boolean(),
                                                        State :: crypto_state() .
crypto_init(Cipher, Key, FlagOrOptions) ->
    ?nif_call(ng_crypto_init_nif(alias(Cipher,Key), Key, <<>>, FlagOrOptions),
              {1,2,-1,3}
             ).

-spec crypto_init(Cipher, Key, IV, FlagOrOptions) -> State
                                                       when Cipher :: cipher_iv(),
                                                            Key :: iodata(),
                                                            IV :: iodata(),
                                                            FlagOrOptions :: crypto_opts(),
                                                            State :: crypto_state() .
crypto_init(Cipher, Key, IV, FlagOrOptions) ->
    ?nif_call(ng_crypto_init_nif(alias(Cipher,Key), Key, IV, FlagOrOptions)).

%%%----------------------------------------------------------------
-spec crypto_dyn_iv_init(Cipher, Key, FlagOrOptions) -> State
                                                          when Cipher :: cipher_iv(),
                                                               Key :: iodata(),
                                                               FlagOrOptions :: crypto_opts() | boolean(),
                                                               State :: crypto_state() .
crypto_dyn_iv_init(Cipher, Key, FlagOrOptions) ->
    %% The IV is supposed to be supplied by calling crypto_update/3
    ?nif_call(ng_crypto_init_nif(alias(Cipher,Key), Key, undefined, FlagOrOptions),
              [Cipher, Key, FlagOrOptions],
              {1,2,-1,3}
             ).

%%%----------------------------------------------------------------
%%%
%%% Encrypt/decrypt a sequence of bytes.  The sum of the sizes
%%% of all blocks must be an integer multiple of the crypto's
%%% blocksize.
%%%

-spec crypto_update(State, Data) -> Result
                            when State :: crypto_state(),
                                 Data :: iodata(),
                                 Result :: binary() .
crypto_update(State, Data) ->
    ?nif_call(ng_crypto_update_nif(State, Data)).

%%%----------------------------------------------------------------
-spec crypto_dyn_iv_update(State, Data, IV) -> Result
                                                   when State :: crypto_state(),
                                                        Data :: iodata(),
                                                        IV :: iodata(),
                                                        Result :: binary() .
crypto_dyn_iv_update(State, Data, IV) ->
    ?nif_call(ng_crypto_update_nif(State, Data, IV)).

%%%----------------------------------------------------------------
%%%
%%% Finalize encrypt/decrypt bytes.  If the size of the bytes in
%%% to crypto_uptate was not an integer number of blocks, the rest
%%% is returned from this function.

-spec crypto_final(State) -> FinalResult
                            when State :: crypto_state(),
                                 FinalResult :: binary() .
crypto_final(State) ->
    ?nif_call(ng_crypto_final_nif(State)).

%%%----------------------------------------------------------------
%%%
%%% Get result of padding etc

-spec crypto_get_data(State) -> Result
                            when State :: crypto_state(),
                                 Result :: map() .
crypto_get_data(State) ->
    ?nif_call(ng_crypto_get_data_nif(State)).

%%%----------------------------------------------------------------
%%%
%%% Encrypt/decrypt one set bytes.
%%% The size must be an integer multiple of the crypto's blocksize.
%%%

-spec crypto_one_time(Cipher, Key, Data, FlagOrOptions) ->
                             Result
                                 when Cipher :: cipher_no_iv(),
                                      Key :: iodata(),
                                      Data :: iodata(),
                                      FlagOrOptions :: crypto_opts() | boolean(),
                                      Result :: binary() .

crypto_one_time(Cipher, Key, Data, FlagOrOptions) ->
    ?nif_call(ng_crypto_one_time_nif(alias(Cipher,Key), Key, <<>>, Data, FlagOrOptions),
              [Cipher, Key, Data, FlagOrOptions],
              {1,2,-1,3,4}
             ).


-spec crypto_one_time(Cipher, Key, IV, Data, FlagOrOptions) ->
                             Result
                                 when Cipher :: cipher_iv(),
                                      Key :: iodata(),
                                      IV :: iodata(),
                                      Data :: iodata(),
                                      FlagOrOptions :: crypto_opts() | boolean(),
                                      Result :: binary() .

crypto_one_time(Cipher, Key, IV, Data, FlagOrOptions) ->
    ?nif_call(ng_crypto_one_time_nif(alias(Cipher,Key), Key, IV, Data, FlagOrOptions),
              [Cipher, Key, IV, Data, FlagOrOptions],
              {}).

%%%----------------------------------------------------------------
-spec crypto_one_time_aead(Cipher, Key, IV, InText, AAD, EncFlag::true) ->
                             Result
                                 when Cipher :: cipher_aead(),
                                      Key :: iodata(),
                                      IV :: iodata(),
                                      InText :: iodata(),
                                      AAD :: iodata(),
                                      Result :: EncryptResult,
                                      EncryptResult :: {OutCryptoText, OutTag},
                                      OutCryptoText :: binary(),
                                      OutTag :: binary().

crypto_one_time_aead(Cipher, Key, IV, PlainText, AAD, true) ->
    ?nif_call(aead_cipher_nif(alias(Cipher,Key), Key, IV, PlainText, AAD, aead_tag_len(Cipher), true),
              {1,2,3,4,5,-1,6}
             ).


-spec crypto_one_time_aead(Cipher, Key, IV, InText, AAD, TagOrTagLength, EncFlag) ->
                             Result
                                 when Cipher :: cipher_aead(),
                                      Key :: iodata(),
                                      IV :: iodata(),
                                      InText :: iodata(),
                                      AAD :: iodata(),
                                      TagOrTagLength :: EncryptTagLength | DecryptTag,
                                      EncryptTagLength :: non_neg_integer(), % or pos_integer() 1..
                                      DecryptTag :: iodata(),
                                      EncFlag :: boolean(),
                                      Result :: EncryptResult | DecryptResult,
                                      EncryptResult :: {OutCryptoText, OutTag},
                                      DecryptResult :: OutPlainText | error,
                                      OutCryptoText :: binary(),
                                      OutTag :: binary(),
                                      OutPlainText :: binary().

crypto_one_time_aead(Cipher, Key, IV, TextIn, AAD, TagOrTagLength, EncFlg) ->
    ?nif_call(aead_cipher_nif(alias(Cipher,Key), Key, IV, TextIn, AAD, TagOrTagLength, EncFlg),
              [Cipher, Key, IV, TextIn, AAD, TagOrTagLength, EncFlg],
              {}
             ).


aead_tag_len(aes_ccm    ) -> 12;
aead_tag_len(aes_128_ccm) -> 12;
aead_tag_len(aes_192_ccm) -> 12;
aead_tag_len(aes_256_ccm) -> 12;
aead_tag_len(aes_gcm    ) -> 16;
aead_tag_len(aes_128_gcm) -> 16;
aead_tag_len(aes_192_gcm) -> 16;
aead_tag_len(aes_256_gcm) -> 16;
aead_tag_len(chacha20_poly1305) -> 16;
aead_tag_len(_) ->
    error({badarg, "Not an AEAD cipher"}).

%%%----------------------------------------------------------------
%%% Cipher NIFs

ng_crypto_init_nif(_Cipher, _Key, _IVec, _OptionsMap) -> ?nif_stub.

ng_crypto_update_nif(_State, _Data) -> ?nif_stub.
ng_crypto_update_nif(_State, _Data, _IV) -> ?nif_stub.

ng_crypto_final_nif(_State) -> ?nif_stub.

ng_crypto_get_data_nif(_State) -> ?nif_stub.

ng_crypto_one_time_nif(_Cipher, _Key, _IVec, _Data, _OptionsMap) -> ?nif_stub.

aead_cipher_nif(_Type, _Key, _Ivec, _AAD, _In, _TagOrTagLength, _EncFlg) -> ?nif_stub.

cipher_info_nif(_Type) -> ?nif_stub.

%%%----------------------------------------------------------------
%%% Cipher aliases
%%%

add_cipher_aliases(Ciphers) ->
    Ciphers ++
        lists:usort(
          lists:foldl(fun(C, Acc) ->
                              case alias1_rev(C) of
                                  C -> Acc;
                                  A -> [A|Acc]
                              end
                      end, [], Ciphers)).

alias(aes_cbc, Key)    -> alias1(aes_cbc, iolist_size(Key));
alias(aes_cfb8, Key)   -> alias1(aes_cfb8, iolist_size(Key));
alias(aes_cfb128, Key) -> alias1(aes_cfb128, iolist_size(Key));
alias(aes_ctr, Key)    -> alias1(aes_ctr, iolist_size(Key));
alias(aes_ecb, Key)    -> alias1(aes_ecb, iolist_size(Key));
alias(aes_gcm, Key)    -> alias1(aes_gcm, iolist_size(Key));
alias(aes_ccm, Key)    -> alias1(aes_ccm, iolist_size(Key));
alias(Alg, _) -> Alg.


alias1(aes_cbc, 16)  -> aes_128_cbc;
alias1(aes_cbc, 24)  -> aes_192_cbc;
alias1(aes_cbc, 32)  -> aes_256_cbc;

alias1(aes_cfb8, 16)  -> aes_128_cfb8;
alias1(aes_cfb8, 24)  -> aes_192_cfb8;
alias1(aes_cfb8, 32)  -> aes_256_cfb8;

alias1(aes_cfb128, 16)  -> aes_128_cfb128;
alias1(aes_cfb128, 24)  -> aes_192_cfb128;
alias1(aes_cfb128, 32)  -> aes_256_cfb128;

alias1(aes_ctr, 16)  -> aes_128_ctr;
alias1(aes_ctr, 24)  -> aes_192_ctr;
alias1(aes_ctr, 32)  -> aes_256_ctr;

alias1(aes_ecb, 16)  -> aes_128_ecb;
alias1(aes_ecb, 24)  -> aes_192_ecb;
alias1(aes_ecb, 32)  -> aes_256_ecb;

alias1(aes_gcm, 16)  -> aes_128_gcm;
alias1(aes_gcm, 24)  -> aes_192_gcm;
alias1(aes_gcm, 32)  -> aes_256_gcm;

alias1(aes_ccm, 16)  -> aes_128_ccm;
alias1(aes_ccm, 24)  -> aes_192_ccm;
alias1(aes_ccm, 32)  -> aes_256_ccm;

alias1(Alg, _) -> Alg.


alias1_rev(aes_128_cbc)    -> aes_cbc;
alias1_rev(aes_192_cbc)    -> aes_cbc;
alias1_rev(aes_256_cbc)    -> aes_cbc;

alias1_rev(aes_128_cfb8)   -> aes_cfb8;
alias1_rev(aes_192_cfb8)   -> aes_cfb8;
alias1_rev(aes_256_cfb8)   -> aes_cfb8;

alias1_rev(aes_128_cfb128) -> aes_cfb128;
alias1_rev(aes_192_cfb128) -> aes_cfb128;
alias1_rev(aes_256_cfb128) -> aes_cfb128;

alias1_rev(aes_128_ctr)    -> aes_ctr;
alias1_rev(aes_192_ctr)    -> aes_ctr;
alias1_rev(aes_256_ctr)    -> aes_ctr;

alias1_rev(aes_128_ecb)    -> aes_ecb;
alias1_rev(aes_192_ecb)    -> aes_ecb;
alias1_rev(aes_256_ecb)    -> aes_ecb;

alias1_rev(aes_128_gcm)    -> aes_gcm;
alias1_rev(aes_192_gcm)    -> aes_gcm;
alias1_rev(aes_256_gcm)    -> aes_gcm;

alias1_rev(aes_128_ccm)    -> aes_ccm;
alias1_rev(aes_192_ccm)    -> aes_ccm;
alias1_rev(aes_256_ccm)    -> aes_ccm;

alias1_rev(C) -> C.

%%%================================================================
%%%
%%% RAND - pseudo random numbers using RN_ and BN_ functions in crypto lib
%%%
%%%================================================================
-type rand_cache_seed() ::
        nonempty_improper_list(non_neg_integer(), binary()).

-spec strong_rand_bytes(N::non_neg_integer()) -> binary().
strong_rand_bytes(Bytes) ->
    case strong_rand_bytes_nif(Bytes) of
        false -> erlang:error(low_entropy);
        Bin -> Bin
    end.
strong_rand_bytes_nif(_Bytes) -> ?nif_stub.


-spec rand_seed() -> rand:state().
rand_seed() ->
    rand:seed(rand_seed_s()).

-spec rand_seed_s() -> rand:state().
rand_seed_s() ->
    rand_seed_alg_s(?MODULE).

-spec rand_seed_alg(Alg :: atom()) ->
                           {rand:alg_handler(),
                            atom() | rand_cache_seed()}.
rand_seed_alg(Alg) ->
    rand:seed(rand_seed_alg_s(Alg)).

-spec rand_seed_alg(Alg :: atom(), Seed :: term()) ->
                           {rand:alg_handler(),
                            atom() | rand_cache_seed()}.
rand_seed_alg(Alg, Seed) ->
    rand:seed(rand_seed_alg_s(Alg, Seed)).

-define(CRYPTO_CACHE_BITS, 56).
-define(CRYPTO_AES_BITS, 58).

-spec rand_seed_alg_s(Alg :: atom()) ->
                             {rand:alg_handler(),
                              atom() | rand_cache_seed()}.
rand_seed_alg_s({AlgHandler, _AlgState} = State) when is_map(AlgHandler) ->
    State;
rand_seed_alg_s({Alg, AlgState}) when is_atom(Alg) ->
    {mk_alg_handler(Alg),AlgState};
 rand_seed_alg_s(Alg) when is_atom(Alg) ->
    {mk_alg_handler(Alg),mk_alg_state(Alg)}.
%%
-spec rand_seed_alg_s(Alg :: atom(), Seed :: term()) ->
                             {rand:alg_handler(),
                              atom() | rand_cache_seed()}.
rand_seed_alg_s(Alg, Seed) when is_atom(Alg) ->
    {mk_alg_handler(Alg),mk_alg_state({Alg,Seed})}.

mk_alg_handler(?MODULE = Alg) ->
    #{ type => Alg,
       bits => 64,
       next => fun ?MODULE:rand_plugin_next/1,
       uniform => fun ?MODULE:rand_plugin_uniform/1,
       uniform_n => fun ?MODULE:rand_plugin_uniform/2};
mk_alg_handler(crypto_cache = Alg) ->
    #{ type => Alg,
       bits => ?CRYPTO_CACHE_BITS,
       next => fun ?MODULE:rand_cache_plugin_next/1};
mk_alg_handler(crypto_aes = Alg) ->
    #{ type => Alg,
       bits => ?CRYPTO_AES_BITS,
       next => fun ?MODULE:rand_plugin_aes_next/1,
       jump => fun ?MODULE:rand_plugin_aes_jump/1}.

mk_alg_state(?MODULE) ->
    no_seed;
mk_alg_state(crypto_cache) ->
    CacheBits = ?CRYPTO_CACHE_BITS,
    BytesPerWord = (CacheBits + 7) div 8,
    GenBytes =
        ((rand_cache_size() + (2*BytesPerWord - 1)) div BytesPerWord)
        * BytesPerWord,
    {CacheBits, GenBytes, <<>>};
mk_alg_state({crypto_aes,Seed}) ->
    %% 16 byte words (128 bit crypto blocks)
    GenWords = (rand_cache_size() + 31) div 16,
    Key = crypto:hash(sha256, Seed),
    {F,Count} = longcount_seed(Seed),
    {Key,GenWords,F,Count}.

rand_cache_size() ->
    DefaultCacheSize = 1024,
    CacheSize =
        application:get_env(crypto, rand_cache_size, DefaultCacheSize),
    if
        is_integer(CacheSize), 0 =< CacheSize ->
            CacheSize;
        true ->
            DefaultCacheSize
    end.

rand_plugin_next(Seed) ->
    {bytes_to_integer(strong_rand_range(1 bsl 64)), Seed}.

rand_plugin_uniform(State) ->
    {strong_rand_float(), State}.

rand_plugin_uniform(Max, State) ->
    {bytes_to_integer(strong_rand_range(Max)) + 1, State}.


rand_cache_plugin_next({CacheBits, GenBytes, <<>>}) ->
    rand_cache_plugin_next(
      {CacheBits, GenBytes, strong_rand_bytes(GenBytes)});
rand_cache_plugin_next({CacheBits, GenBytes, Cache}) ->
    <<I:CacheBits, NewCache/binary>> = Cache,
    {I, {CacheBits, GenBytes, NewCache}}.


%% Encrypt 128 bit counter values and use the 58 lowest
%% encrypted bits as random numbers.
%%
%% The 128 bit counter is handled as 4 32 bit words
%% to avoid bignums.  Generate a bunch of numbers
%% at the time and cache them.
%%
-dialyzer({no_improper_lists, rand_plugin_aes_next/1}).
rand_plugin_aes_next([V|Cache]) ->
    {V,Cache};
rand_plugin_aes_next({Key,GenWords,F,Count}) ->
    rand_plugin_aes_next(Key, GenWords, F, Count);
rand_plugin_aes_next({Key,GenWords,F,_JumpBase,Count}) ->
    rand_plugin_aes_next(Key, GenWords, F, Count).
%%
rand_plugin_aes_next(Key, GenWords, F, Count) ->
    {Cleartext,NewCount} = aes_cleartext(<<>>, F, Count, GenWords),
    Encrypted = block_encrypt(Key, Cleartext),
    [V|Cache] = aes_cache(Encrypted, {Key,GenWords,F,Count,NewCount}),
    {V,Cache}.

block_encrypt(Key, Data) ->
    Cipher = case byte_size(Key) of
                 16 -> aes_128_ecb;
                 24 -> aes_192_ecb;
                 32 -> aes_256_ecb;
                 _ -> error(badarg)
             end,
    try 
        crypto_one_time(Cipher, Key, Data, true)
    catch
        error:{error, {_File,_Line}, _Reason} ->
            error(badarg);
        error:{E, {_File,_Line}, _Reason} when E==notsup ; E==badarg ->
            error(E)
    end.


%% A jump advances the counter 2^512 steps; the jump function
%% is applied to the jump base and then the number of used
%% numbers from the cache has to be wasted for the jump to be correct
%%
rand_plugin_aes_jump({#{type := crypto_aes} = Alg, Cache}) ->
    {Alg,rand_plugin_aes_jump(fun longcount_jump/1, 0, Cache)}.
%% Count cached words and subtract their number from jump
-dialyzer({no_improper_lists, rand_plugin_aes_jump/3}).
rand_plugin_aes_jump(Jump, J, [_|Cache]) ->
    rand_plugin_aes_jump(Jump, J + 1, Cache);
rand_plugin_aes_jump(Jump, J, {Key,GenWords,F,JumpBase, _Count}) ->
    rand_plugin_aes_jump(Jump, GenWords - J, Key, GenWords, F, JumpBase);
rand_plugin_aes_jump(Jump, 0, {Key,GenWords,F,JumpBase}) ->
    rand_plugin_aes_jump(Jump, 0, Key, GenWords, F, JumpBase).
%%
rand_plugin_aes_jump(Jump, Skip, Key, GenWords, F, JumpBase) ->
    Count = longcount_next_count(Skip, Jump(JumpBase)),
    {Key,GenWords,F,Count}.

rand_plugin_aes_jump_2pow20(Cache) ->
    rand_plugin_aes_jump(fun longcount_jump_2pow20/1, 0, Cache).


longcount_seed(Seed) ->
    <<X:64, _:6, F:12, S2:58, S1:58, S0:58>> =
        crypto:hash(sha256, [Seed,<<"Xoroshiro928">>]),
    {F,rand:exro928_seed([S0,S1,S2|rand:seed58(13, X)])}.

longcount_next_count(0, Count) ->
    Count;
longcount_next_count(N, Count) ->
    longcount_next_count(N - 1, rand:exro928_next_state(Count)).

longcount_next(Count) ->
    rand:exro928_next(Count).

longcount_jump(Count) ->
    rand:exro928_jump_2pow512(Count).

longcount_jump_2pow20(Count) ->
    rand:exro928_jump_2pow20(Count).


%% Build binary with counter values to cache
aes_cleartext(Cleartext, _F, Count, 0) ->
    {Cleartext,Count};
aes_cleartext(Cleartext, F, Count, GenWords) ->
    {{S0,S1}, NewCount} = longcount_next(Count),
    aes_cleartext(
      <<Cleartext/binary, F:12, S1:58, S0:58>>,
      F, NewCount, GenWords - 1).

%% Parse and cache encrypted counter values aka random numbers
-dialyzer({no_improper_lists, aes_cache/2}).
aes_cache(<<>>, Cache) ->
    Cache;
aes_cache(
  <<_:(128 - ?CRYPTO_AES_BITS), V:?CRYPTO_AES_BITS, Encrypted/binary>>,
  Cache) ->
    [V|aes_cache(Encrypted, Cache)].


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

-spec rand_uniform(crypto_integer(), crypto_integer()) ->
			  crypto_integer().
rand_uniform(From, To) when is_binary(From), is_binary(To) ->
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
rand_seed(Seed) when is_binary(Seed) ->
    rand_seed_nif(Seed).

rand_seed_nif(_Seed) -> ?nif_stub.

%%%================================================================
%%%
%%% Sign/verify
%%%
%%%================================================================
-type pk_sign_verify_algs() :: rsa | dss | ecdsa | eddsa .

-type pk_sign_verify_opts() :: [ rsa_sign_verify_opt() ] .

-type rsa_sign_verify_opt() :: {rsa_padding, rsa_sign_verify_padding()}
                             | {rsa_pss_saltlen, integer()}
                             | {rsa_mgf1_md, sha2()}.

-type rsa_sign_verify_padding() :: rsa_pkcs1_padding | rsa_pkcs1_pss_padding
                                 | rsa_x931_padding | rsa_no_padding
                                   .


%%%----------------------------------------------------------------
%%% Sign

-spec sign(Algorithm, DigestType, Msg, Key)
          -> Signature
                 when Algorithm :: pk_sign_verify_algs(),
                      DigestType :: rsa_digest_type()
                                  | dss_digest_type()
                                  | ecdsa_digest_type()
                                  | none,
                      Msg :: iodata() | {digest,iodata()},
                      Key :: rsa_private()
                           | dss_private()
                           | [ecdsa_private() | ecdsa_params()]
                           | [eddsa_private() | eddsa_params()]
                           | engine_key_ref(),
                      Signature :: binary() .

sign(Algorithm, Type, Data, Key) ->
    sign(Algorithm, Type, Data, Key, []).


-spec sign(Algorithm, DigestType, Msg, Key, Options)
          -> Signature
                 when Algorithm :: pk_sign_verify_algs(),
                      DigestType :: rsa_digest_type()
                                  | dss_digest_type()
                                  | ecdsa_digest_type()
                                  | none,
                      Msg :: iodata() | {digest,iodata()},
                      Key :: rsa_private()
                           | dss_private()
                           | [ecdsa_private() | ecdsa_params()]
                           | [eddsa_private() | eddsa_params()]
                           | engine_key_ref(),
                      Options :: pk_sign_verify_opts(),
                      Signature :: binary() .

sign(Algorithm0, Type0, Data, Key, Options) ->
    {Algorithm, Type} = sign_verify_compatibility(Algorithm0, Type0, Data),
    ?nif_call(pkey_sign_nif(Algorithm, Type, Data, format_pkey(Algorithm, Key), Options),
              {1, 2, 3, 4, 5},
              [Algorithm0, Type0, Data, Key, Options]).

pkey_sign_nif(_Algorithm, _Type, _Digest, _Key, _Options) -> ?nif_stub.

%%%----------------------------------------------------------------
%%% Verify

-spec verify(Algorithm, DigestType, Msg, Signature, Key)
            -> Result
                   when Algorithm :: pk_sign_verify_algs(),
                        DigestType :: rsa_digest_type()
                                    | dss_digest_type()
                                    | ecdsa_digest_type()
                                    | none,
                        Msg :: iodata() | {digest,iodata()},
                        Signature :: binary(),
                        Key :: rsa_public()
                             | dss_public()
                             | [ecdsa_public() | ecdsa_params()]
                             | [eddsa_public() | eddsa_params()]
                             | engine_key_ref(),
                        Result :: boolean().

verify(Algorithm, Type, Data, Signature, Key) ->
    verify(Algorithm, Type, Data, Signature, Key, []).

-spec verify(Algorithm, DigestType, Msg, Signature, Key, Options)
            -> Result
                   when Algorithm :: pk_sign_verify_algs(),
                        DigestType :: rsa_digest_type()
                                    | dss_digest_type()
                                    | ecdsa_digest_type()
                                    | none,
                        Msg :: iodata() | {digest,iodata()},
                        Signature :: binary(),
                        Key :: rsa_public()
                             | dss_public()
                             | [ecdsa_public() | ecdsa_params()]
                             | [eddsa_public() | eddsa_params()]
                             | engine_key_ref(),
                        Options :: pk_sign_verify_opts(),
                        Result :: boolean().

verify(Algorithm0, Type0, Data, Signature, Key, Options) ->
    {Algorithm, Type} = sign_verify_compatibility(Algorithm0, Type0, Data),
    ?nif_call(pkey_verify_nif(Algorithm, Type, Data, Signature, format_pkey(Algorithm, Key), Options),
              {1,2,3,4,5},
              [Algorithm0, Type0, Data, Signature, Key, Options]).

pkey_verify_nif(_Algorithm, _Type, _Data, _Signature, _Key, _Options) -> ?nif_stub.

%% Backwards compatible:
sign_verify_compatibility(dss, none, Digest) ->
    {sha, {digest, Digest}};
sign_verify_compatibility(Algorithm0, Type0, _Digest) ->
    {Algorithm0, Type0}.

%%%================================================================
%%%
%%% Public/private encrypt/decrypt
%%%
%%% Only rsa works so far (although ecdsa | dss should do it)
%%%================================================================
-type pk_encrypt_decrypt_algs() :: rsa .

-type pk_encrypt_decrypt_opts() ::  [rsa_opt()] | rsa_compat_opts().

-type rsa_compat_opts() :: [{rsa_pad, rsa_padding()}]
                         | rsa_padding() .

-type rsa_padding() :: rsa_pkcs1_padding
                     | rsa_pkcs1_oaep_padding
                     | rsa_sslv23_padding
                     | rsa_x931_padding
                     | rsa_no_padding.

-type rsa_opt() :: {rsa_padding, rsa_padding()}
                 | {signature_md, atom()}
                 | {rsa_mgf1_md, sha}
                 | {rsa_oaep_label, binary()}
                 | {rsa_oaep_md, sha} .

%%%---- Encrypt with public key

-spec public_encrypt(Algorithm, PlainText, PublicKey, Options) ->
                            CipherText when Algorithm :: pk_encrypt_decrypt_algs(),
                                            PlainText :: binary(),
                                            PublicKey :: rsa_public() | engine_key_ref(),
                                            Options :: pk_encrypt_decrypt_opts(),
                                            CipherText :: binary().
public_encrypt(Algorithm, PlainText, PublicKey, Options) ->
    pkey_crypt(Algorithm, PlainText, PublicKey, Options, false, true).

%%%---- Decrypt with private key

-spec private_decrypt(Algorithm, CipherText, PrivateKey, Options) ->
                             PlainText when Algorithm :: pk_encrypt_decrypt_algs(),
                                            CipherText :: binary(),
                                            PrivateKey :: rsa_private() | engine_key_ref(),
                                            Options :: pk_encrypt_decrypt_opts(),
                                            PlainText :: binary() .
private_decrypt(Algorithm, CipherText, PrivateKey, Options) ->
    pkey_crypt(Algorithm, CipherText,  PrivateKey, Options, true, false).

%%%---- Encrypt with private key

-spec private_encrypt(Algorithm, PlainText, PrivateKey, Options) ->
                            CipherText when Algorithm :: pk_encrypt_decrypt_algs(),
                                            PlainText :: binary(),
                                            PrivateKey :: rsa_private() | engine_key_ref(),
                                            Options :: pk_encrypt_decrypt_opts(),
                                            CipherText :: binary().
private_encrypt(Algorithm, PlainText, PrivateKey, Options) ->
    pkey_crypt(Algorithm, PlainText,  PrivateKey, Options, true, true).

%%%---- Decrypt with public key

-spec public_decrypt(Algorithm, CipherText, PublicKey, Options) ->
                             PlainText when Algorithm :: pk_encrypt_decrypt_algs(),
                                            CipherText :: binary(),
                                            PublicKey :: rsa_public() | engine_key_ref(),
                                            Options :: pk_encrypt_decrypt_opts(),
                                            PlainText :: binary() .
public_decrypt(Algorithm, CipherText, PublicKey, Options) ->
    pkey_crypt(Algorithm, CipherText, PublicKey, Options, false, false).

%%%---- Call the nif, but fix a compatibility issue first

%% Backwards compatible (rsa_pad -> rsa_padding is handled by the pkey_crypt_nif):
pkey_crypt(rsa, Text, Key, Padding, PubPriv, EncDec) when is_atom(Padding) ->
    pkey_crypt(rsa, Text, Key, [{rsa_padding, Padding}], PubPriv, EncDec);

pkey_crypt(Alg, Text, Key, Options, PubPriv, EncDec) ->
    ?nif_call(pkey_crypt_nif(Alg, Text, format_pkey(Alg,Key), Options, PubPriv, EncDec)).

pkey_crypt_nif(_Algorithm, _In, _Key, _Options, _IsPrivate, _IsEncrypt) -> ?nif_stub.

%%%================================================================
%%%
%%%
%%%
%%%================================================================

-spec generate_key(Type, Params)
                 -> {PublicKey, PrivKeyOut}
                        when Type :: dh | ecdh | eddh | eddsa | rsa | srp,
                             PublicKey :: dh_public() | ecdh_public() | rsa_public() | srp_public(),
                             PrivKeyOut :: dh_private() | ecdh_private() | rsa_private() | {srp_public(),srp_private()},
                             Params :: dh_params() | ecdh_params() | eddsa_params() | rsa_params() | srp_gen_params()
                                       .
generate_key(Type, Params) ->
    generate_key(Type, Params, undefined).

-spec generate_key(Type, Params, PrivKeyIn)
                 -> {PublicKey, PrivKeyOut}
                        when Type :: dh | ecdh | eddh | eddsa | rsa | srp,
                             PublicKey :: dh_public() | ecdh_public() | rsa_public() | srp_public(),
                             PrivKeyIn :: undefined | dh_private() | ecdh_private() | rsa_private() | {srp_public(),srp_private()},
                             PrivKeyOut :: dh_private() | ecdh_private() | rsa_private() | {srp_public(),srp_private()},
                             Params :: dh_params() | ecdh_params() | eddsa_params() | rsa_params() | srp_comp_params()
                                       .

generate_key(dh, DHParameters0, PrivateKey) ->
    {DHParameters, Len} =
        case DHParameters0 of
            [P,G,L] -> {[P,G], L};
            [P,G] -> {[P,G], 0}
        end,
    ?nif_call(dh_generate_key_nif(ensure_int_as_bin(PrivateKey),
                                  map_ensure_int_as_bin(DHParameters),
                                  0, Len),
              {3, 2, -1, 2},
              [dh, DHParameters0, PrivateKey]);

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
    case ?nif_call(rsa_generate_key_nif(ModulusSize,
                                        ensure_int_as_bin(PublicExponent)),
                   [rsa, {ModulusSize, PublicExponent}, undefined],
                   {2,2}
                  ) of
        error ->
            erlang:error(computation_failed,
                         [rsa,{ModulusSize,PublicExponent}]);
        {Private, OldPrivate} when Private == OldPrivate ->
            {lists:sublist(Private,2), Private};
        {_Private, _OldPrivate} ->
            Where = lists:map(fun({A,B}) -> A == B end,
                              lists:zip(_Private, _OldPrivate)),
            erlang:error({new_old_differ,Where},
                         [rsa,{ModulusSize,PublicExponent}]);
        Private ->
            {lists:sublist(Private,2), Private}
    end;

generate_key(ecdh, Curve, PrivKey) when Curve == x448 ;
                                        Curve == x25519 ->
    %% Legacy: This clause was here before the eddh was added as an own Type
    generate_key(eddh, Curve, PrivKey);
generate_key(eddh, Curve, PrivKey) when Curve == x448 ;
                                        Curve == x25519 ->
    ?nif_call(evp_generate_key_nif(Curve, ensure_int_as_bin(PrivKey)),
              {2, 3},
              [eddh, Curve, PrivKey]
             );

generate_key(ecdh, Curve, PrivKey) ->
    ?nif_call(ec_generate_key_nif(nif_curve_params(Curve), ensure_int_as_bin(PrivKey)));

generate_key(eddsa, Curve, PrivKey) when Curve == ed448 ;
                                         Curve == ed25519 ->
    ?nif_call(evp_generate_key_nif(Curve, ensure_int_as_bin(PrivKey)),
              {2, 3},
              [eddsa, Curve, PrivKey]
             ).

evp_generate_key_nif(_Curve, _PrivKey) -> ?nif_stub.


-spec compute_key(Type, OthersPublicKey, MyPrivateKey, Params)
                 -> SharedSecret
                        when Type :: dh | ecdh | eddh |  srp,
                             SharedSecret :: binary(),
                             OthersPublicKey :: dh_public() | ecdh_public() | srp_public(),
                             MyPrivateKey :: dh_private() | ecdh_private() | {srp_public(),srp_private()},
                             Params :: dh_params() | ecdh_params() | srp_comp_params()
                                       .

compute_key(dh, OthersPublicKey, MyPrivateKey, DHParameters) ->
    ?nif_call(dh_compute_key_nif(ensure_int_as_bin(OthersPublicKey),
                                 ensure_int_as_bin(MyPrivateKey),
                                 map_ensure_int_as_bin(DHParameters)),
              {2, 3, 4},
              [dh, OthersPublicKey, MyPrivateKey, DHParameters]);

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

compute_key(ecdh, Others, My, Curve) when Curve == x448 ;
                                          Curve == x25519 ->
    %% Legacy: This clause was here before the eddh was added as an own Type
    compute_key(eddh, Others, My, Curve);

compute_key(eddh, Others, My, Curve) when Curve == x448 ;
                                          Curve == x25519 ->
    ?nif_call(evp_compute_key_nif(Curve, ensure_int_as_bin(Others), ensure_int_as_bin(My)),
              {2, 3, 4},
              [eddh, Others, My, Curve]);

compute_key(ecdh, Others, My, Curve) ->
    ?nif_call(ecdh_compute_key_nif(ensure_int_as_bin(Others),
                                   nif_curve_params(Curve),
                                   ensure_int_as_bin(My)),
              {2, 4, 3},
              [ecdh, Others, My, Curve]).


evp_compute_key_nif(_Curve, _OthersBin, _MyBin) -> ?nif_stub.


%%%================================================================
%%%
%%% XOR - xor to iolists and return a binary
%%% NB doesn't check that they are the same size, just concatenates
%%% them and sends them to the driver
%%%
%%%================================================================

-spec exor(iodata(), iodata()) -> binary().

exor(Bin1, Bin2) ->
    Data1 = iolist_to_binary(Bin1),
    Data2 = iolist_to_binary(Bin2),
    MaxBytes = max_bytes(),
    exor(Data1, Data2, erlang:byte_size(Data1), MaxBytes, []).


%%%================================================================
%%%
%%% Exponentiation modulo
%%%
%%%================================================================

-spec mod_pow(N, P, M) -> Result when N :: binary() | integer(),
                                      P :: binary() | integer(),
                                      M :: binary() | integer(),
                                      Result :: binary() | error .
mod_pow(Base, Exponent, Prime) ->
    case mod_exp_nif(ensure_int_as_bin(Base), ensure_int_as_bin(Exponent), ensure_int_as_bin(Prime), 0) of
	<<0>> -> error;
	R -> R
    end.

%%%======================================================================
%%%
%%% Engine functions
%%%
%%%======================================================================

%%%---- Referring to keys stored in an engine:
-type key_id()   :: string() | binary() .
-type password() :: string() | binary() .

-type engine_key_ref() :: #{engine :=   engine_ref(),
                            key_id :=   key_id(),
                            password => password(),
                            term() => term()
                           }.

%%%---- Commands:
-type engine_cmnd() :: {unicode:chardata(), unicode:chardata()}.

%%----------------------------------------------------------------------
%% Function: engine_get_all_methods/0
%%----------------------------------------------------------------------
-type engine_method_type() :: engine_method_rsa | engine_method_dsa | engine_method_dh |
                              engine_method_rand | engine_method_ecdh | engine_method_ecdsa |
                              engine_method_ciphers | engine_method_digests | engine_method_store |
                              engine_method_pkey_meths | engine_method_pkey_asn1_meths |
                              engine_method_ec.

-type engine_ref() :: term().

-spec engine_get_all_methods() -> Result when Result :: [engine_method_type()].
engine_get_all_methods() ->
     notsup_to_error(engine_get_all_methods_nif()).

%%----------------------------------------------------------------------
%% Function: engine_load/3
%%----------------------------------------------------------------------
-spec engine_load(EngineId, PreCmds, PostCmds) ->
                         Result when EngineId::unicode:chardata(),
                                     PreCmds::[engine_cmnd()],
                                     PostCmds::[engine_cmnd()],
                                     Result :: {ok, Engine::engine_ref()} | {error, Reason::term()}.
engine_load(EngineId, PreCmds, PostCmds) when is_list(PreCmds),
                                              is_list(PostCmds) ->
    try
        ok = notsup_to_error(engine_load_dynamic_nif()),
        case notsup_to_error(engine_by_id_nif(ensure_bin_chardata(EngineId))) of
            {ok, Engine} ->
                engine_load_1(Engine, PreCmds, PostCmds);
            {error, Error1} ->
                {error, Error1}
        end
    catch
        throw:Error2 ->
            Error2
    end.

-spec engine_load(EngineId, PreCmds, PostCmds, EngineMethods) ->
                         Result when EngineId::unicode:chardata(),
                                     PreCmds::[engine_cmnd()],
                                     PostCmds::[engine_cmnd()],
                                     EngineMethods::[engine_method_type()],
                                     Result :: {ok, Engine::engine_ref()} | {error, Reason::term()}.
engine_load(EngineId, PreCmds, PostCmds, _EngineMethods) when is_list(PreCmds),
							      is_list(PostCmds) ->
    engine_load(EngineId, PreCmds, PostCmds).


%%----------------------------------------------------------------------
engine_load_1(Engine, PreCmds, PostCmds) ->
    try
        ok = engine_nif_wrapper(engine_ctrl_cmd_strings_nif(Engine, ensure_bin_cmds(PreCmds), 0)),
        ok = engine_nif_wrapper(engine_init_nif(Engine)),
        engine_load_2(Engine, PostCmds),
        {ok, Engine}
    catch
        throw:Error ->
            %% The engine couldn't initialise, release the structural reference
            ok = engine_free_nif(Engine),
            throw(Error);
        error:badarg ->
            %% For example bad argument list, release the structural reference
            ok = engine_free_nif(Engine),
            error(badarg)
    end.

engine_load_2(Engine, PostCmds) ->
    try
        ok = engine_nif_wrapper(engine_ctrl_cmd_strings_nif(Engine, ensure_bin_cmds(PostCmds), 0)),
        ok
    catch
       throw:Error ->
          %% The engine registration failed, release the structural and functional references
          ok = engine_free_nif(Engine),
          throw(Error)
    end.

%%----------------------------------------------------------------------
%% Function: engine_unload/1
%%----------------------------------------------------------------------
-spec engine_unload(Engine) -> Result when Engine :: engine_ref(),
                                           Result :: ok | {error, Reason::term()}.
engine_unload(Engine) ->
    try
        %% Release the reference from engine_by_id_nif
        ok = engine_nif_wrapper(engine_free_nif(Engine))
    catch
        throw:Error ->
            Error
    end.

-spec engine_unload(Engine, EngineMethods) -> Result when Engine :: engine_ref(),
					      EngineMethods :: [engine_method_type()],
					      Result :: ok | {error, Reason::term()}.
engine_unload(Engine, _EngineMethods) ->
    engine_unload(Engine).

%%----------------------------------------------------------------------
%% Function: engine_by_id/1
%%----------------------------------------------------------------------
-spec engine_by_id(EngineId) -> Result when EngineId :: unicode:chardata(),
                                            Result :: {ok, Engine::engine_ref()} | {error, Reason::term()} .
engine_by_id(EngineId) ->
    try
        notsup_to_error(engine_by_id_nif(ensure_bin_chardata(EngineId)))
    catch
       throw:Error ->
          Error
    end.

%%----------------------------------------------------------------------
%% Function: engine_add/1
%%----------------------------------------------------------------------
-spec engine_add(Engine) -> Result when Engine :: engine_ref(),
                                        Result ::  ok | {error, Reason::term()} .
engine_add(Engine) ->
    notsup_to_error(engine_add_nif(Engine)).

%%----------------------------------------------------------------------
%% Function: engine_remove/1
%%----------------------------------------------------------------------
-spec engine_remove(Engine) -> Result when Engine :: engine_ref(),
                                           Result ::  ok | {error, Reason::term()} .
engine_remove(Engine) ->
    notsup_to_error(engine_remove_nif(Engine)).

%%----------------------------------------------------------------------
%% Function: engine_register/2
%%----------------------------------------------------------------------
-spec engine_register(Engine, EngineMethods) -> Result when Engine :: engine_ref(),
					       EngineMethods::[engine_method_type()],
					       Result ::  ok | {error, Reason::term()} .
engine_register(Engine, EngineMethods) when is_list(EngineMethods) ->
    try
	[ok = engine_nif_wrapper(engine_register_nif(Engine, engine_method_atom_to_int(Method))) || 
	    Method <- EngineMethods],
        ok
    catch
	throw:Error -> Error
    end.

%%----------------------------------------------------------------------
%% Function: engine_unregister/2
%%----------------------------------------------------------------------
-spec engine_unregister(Engine, EngineMethods) -> Result when Engine :: engine_ref(),
						 EngineMethods::[engine_method_type()],
						 Result ::  ok | {error, Reason::term()} .
engine_unregister(Engine, EngineMethods) when is_list(EngineMethods) ->
    try
	[ok = engine_nif_wrapper(engine_unregister_nif(Engine, engine_method_atom_to_int(Method))) || 
	    Method <- EngineMethods],
        ok
    catch
	throw:Error -> Error
    end.
    
%%----------------------------------------------------------------------
%% Function: engine_get_id/1
%%----------------------------------------------------------------------
-spec engine_get_id(Engine) -> EngineId when Engine :: engine_ref(),
                                             EngineId :: unicode:chardata().
engine_get_id(Engine) ->
    notsup_to_error(engine_get_id_nif(Engine)).

%%----------------------------------------------------------------------
%% Function: engine_get_name/1
%%----------------------------------------------------------------------
-spec engine_get_name(Engine) -> EngineName when Engine :: engine_ref(),
                                                 EngineName :: unicode:chardata().
engine_get_name(Engine) ->
    notsup_to_error(engine_get_name_nif(Engine)).

%%----------------------------------------------------------------------
%% Function: engine_list/0
%%----------------------------------------------------------------------
-spec engine_list() -> Result when Result :: [EngineId::unicode:chardata()].
engine_list() ->
    case notsup_to_error(engine_get_first_nif()) of
        {ok, <<>>} ->
            [];
        {ok, Engine} ->
            case notsup_to_error(engine_get_id_nif(Engine)) of
                <<>> ->
                    engine_list(Engine, []);
                EngineId ->
                    engine_list(Engine, [EngineId])
            end
    end.

engine_list(Engine0, IdList) ->
    case notsup_to_error(engine_get_next_nif(Engine0)) of
        {ok, <<>>} ->
            lists:reverse(IdList);
        {ok, Engine1} ->
            case notsup_to_error(engine_get_id_nif(Engine1)) of
                <<>> ->
                    engine_list(Engine1, IdList);
                EngineId ->
                    engine_list(Engine1, [EngineId |IdList])
            end
    end.

%%----------------------------------------------------------------------
%% Function: engine_ctrl_cmd_string/3
%%----------------------------------------------------------------------
-spec engine_ctrl_cmd_string(Engine, CmdName, CmdArg) ->
                                    Result when Engine::term(),
                                                CmdName::unicode:chardata(),
                                                CmdArg::unicode:chardata(),
                                                Result :: ok | {error, Reason::term()}.
engine_ctrl_cmd_string(Engine, CmdName, CmdArg) ->
    engine_ctrl_cmd_string(Engine, CmdName, CmdArg, false).

%%----------------------------------------------------------------------
%% Function: engine_ctrl_cmd_string/4
%%----------------------------------------------------------------------
-spec engine_ctrl_cmd_string(Engine, CmdName, CmdArg, Optional) ->
                                    Result when Engine::term(),
                                                CmdName::unicode:chardata(),
                                                CmdArg::unicode:chardata(),
                                                Optional::boolean(),
                                                Result :: ok | {error, Reason::term()}.
engine_ctrl_cmd_string(Engine, CmdName, CmdArg, Optional) ->
    case engine_ctrl_cmd_strings_nif(Engine,
                                     ensure_bin_cmds([{CmdName, CmdArg}]),
                                     bool_to_int(Optional)) of
        ok ->
            ok;
        notsup ->
            erlang:error(notsup);
        {error, Error} ->
            {error, Error}
    end.

%%----------------------------------------------------------------------
%% Function: ensure_engine_loaded/2
%% Special version of load that only uses dynamic engine to load
%%----------------------------------------------------------------------
-spec ensure_engine_loaded(EngineId, LibPath) -> Result when EngineId :: unicode:chardata(),
						 LibPath :: unicode:chardata(),
						 Result :: {ok, Engine::engine_ref()} |
	{error, Reason::term()}.
ensure_engine_loaded(EngineId, LibPath) ->
    case notsup_to_error(ensure_engine_loaded_nif(ensure_bin_chardata(EngineId),
                                                  ensure_bin_chardata(LibPath))) of
        {ok, Engine} ->
            {ok, Engine};
        {error, Error1} ->
            {error, Error1}
    end.

%%----------------------------------------------------------------------
%% Function: ensure_engine_loaded/3
%% Special version of load that only uses dynamic engine to load
%%----------------------------------------------------------------------
-spec ensure_engine_loaded(EngineId, LibPath, EngineMethods) ->
                                  Result when EngineId :: unicode:chardata(),
                                              LibPath :: unicode:chardata(),
                                              EngineMethods :: [engine_method_type()],
                                              Result :: {ok, Engine::engine_ref()} |
                                                        {error, Reason::term()}.
ensure_engine_loaded(EngineId, LibPath, _EngineMethods) ->
    ensure_engine_loaded(EngineId, LibPath).

%%----------------------------------------------------------------------
%% Function: ensure_engine_unloaded/1
%%----------------------------------------------------------------------
-spec ensure_engine_unloaded(Engine) -> Result when Engine :: engine_ref(),
                                                    Result :: ok | {error, Reason::term()}.
ensure_engine_unloaded(Engine) ->
    engine_unload(Engine).

%%----------------------------------------------------------------------
%% Function: ensure_engine_unloaded/2
%%----------------------------------------------------------------------
-spec ensure_engine_unloaded(Engine, EngineMethods) ->
                                    Result when Engine :: engine_ref(),
                                                EngineMethods :: [engine_method_type()],
                                                Result :: ok | {error, Reason::term()}.
ensure_engine_unloaded(Engine, _EngineMethods) ->
    ensure_engine_unloaded(Engine).


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
				 LibTypeName ++ "*"]),
                              erl_prim_loader) /= []) orelse
			  (filelib:wildcard(
			     filename:join(
			       [PrivDir,
				"lib",
				erlang:system_info(system_architecture),
				LibTypeName ++ "*"]),
                             erl_prim_loader) /= []) of
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
			 filelib:wildcard(
                           filename:join(
                             [ArchLibDir,LibName ++ "*" ]),
                           erl_prim_loader),
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
            Fmt = "Unable to load crypto library. Failed with error:~n\"~p, ~s\"~n~s",
            Extra = case E of
                        load_failed ->
                            "OpenSSL might not be installed on this system.\n";
                        _ -> ""
                    end,
	    error_logger:error_msg(Fmt, [E,Str,Extra]),
	    Status
    end.

path2bin(Path) when is_list(Path) ->
    Encoding = file:native_name_encoding(),
    case unicode:characters_to_binary(Path,Encoding,Encoding) of
	Bin when is_binary(Bin) ->
	    Bin
    end.

%%%================================================================
%%%================================================================
%%%
%%% Internal functions
%%%
%%%================================================================

max_bytes() ->
    ?MAX_BYTES_TO_NIF.

notsup_to_error(notsup) ->
    erlang:error(notsup);
notsup_to_error(Other) ->
    Other.

%% HASH --------------------------------------------------------------------
hash(Hash, Data, Size, Max) when Size =< Max ->
    ?nif_call(hash_nif(Hash, Data));
hash(Hash, Data, Size, Max) ->
    State0 = hash_init(Hash),
    State1 = hash_update(State0, Data, Size, Max),
    hash_final(State1).

hash_xof(Hash, Data, Size, Length) ->
    Max = max_bytes(),
    State0 = hash_init(Hash),
    State1 = hash_update(State0, Data, Size, Max),
    hash_final_xof(State1, Length).

hash_update(State, Data, Size, MaxBytes)  when Size =< MaxBytes ->
    ?nif_call(hash_update_nif(State, Data), {1,2});
hash_update(State0, Data, _, MaxBytes) ->
    <<Increment:MaxBytes/binary, Rest/binary>> = Data,
    State = ?nif_call(hash_update_nif(State0, Increment), {1,2}),
    hash_update(State, Rest, erlang:byte_size(Rest), MaxBytes).

hash_info_nif(_Hash) -> ?nif_stub.
hash_nif(_Hash, _Data) -> ?nif_stub.
hash_init_nif(_Hash) -> ?nif_stub.
hash_update_nif(_State, _Data) -> ?nif_stub.
hash_final_nif(_State) -> ?nif_stub.
hash_final_xof_nif(_State, _Length) -> ?nif_stub.

%%%================================================================

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
    case srp_pad_length(Width, byte_size(Binary)) of
        0 -> Binary;
        N -> << 0:N/unit:8, Binary/binary>>
    end.

srp_host_secret_nif(_Verifier, _B, _U, _A, _Prime) -> ?nif_stub.

srp_user_secret_nif(_A, _U, _B, _Multiplier, _Generator, _Exponent, _Prime) -> ?nif_stub.

srp_value_B_nif(_Multiplier, _Verifier, _Generator, _Exponent, _Prime) -> ?nif_stub.


%% Public Keys  --------------------------------------------------------------------
%% RSA Rivest-Shamir-Adleman functions
%%

rsa_generate_key_nif(_Bits, _Exp) -> ?nif_stub.

%% DH Diffie-Hellman functions
%%

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% PrivKey = mpint()
dh_generate_key_nif(_PrivateKey, _DHParameters, _Mpint, _Length) -> ?nif_stub.

%% DHParameters = [P (Prime)= mpint(), G(Generator) = mpint()]
%% MyPrivKey, OthersPublicKey = mpint()
dh_compute_key_nif(_OthersPublicKey, _MyPrivateKey, _DHParameters) -> ?nif_stub.

ec_generate_key_nif(_Curve, _Key) -> ?nif_stub.

ecdh_compute_key_nif(_Others, _Curve, _My) -> ?nif_stub.

-spec ec_curves() -> [EllipticCurve] when EllipticCurve :: ec_named_curve()
                                                         | edwards_curve_dh()
                                                         | edwards_curve_ed() .

ec_curves() ->
    crypto_ec_curves:curves().

-spec ec_curve(CurveName) -> ExplicitCurve when CurveName :: ec_named_curve(),
                                                ExplicitCurve :: ec_explicit_curve() .
ec_curve(X) ->
    crypto_ec_curves:curve(X).


-spec privkey_to_pubkey(Type, EnginePrivateKeyRef) -> PublicKey when Type :: rsa | dss,
                                                                     EnginePrivateKeyRef :: engine_key_ref(),
                                                                     PublicKey ::  rsa_public() | dss_public() .
privkey_to_pubkey(Alg, EngineMap) when Alg == rsa; Alg == dss; Alg == ecdsa ->
    try ?nif_call(privkey_to_pubkey_nif(Alg, format_pkey(Alg,EngineMap))) of
        [_|_]=L -> map_ensure_bin_as_int(L);
        X -> X
    catch
        error:{badarg,_,_} when Alg==ecdsa ->
            {error, notsup};
        error:{badarg,_,_} ->
            {error, not_found};
        error:{notsup,_,_} ->
            {error, notsup}
    end.

privkey_to_pubkey_nif(_Alg, _EngineMap) -> ?nif_stub.


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
    {
      {term_to_nif_prime(PrimeField),
       term_to_nif_curve(Curve),
       ensure_int_as_bin(BasePoint),
       ensure_int_as_bin(Order),
       ensure_int_as_bin(CoFactor)
      },
      undefined %% The curve name
    };
nif_curve_params(CurveName) when is_atom(CurveName) ->
    %% A named curve
    case CurveName of
        x448   -> {evp,CurveName};
        x25519 -> {evp,CurveName};
        _ ->
            Spec =
                try
                    crypto_ec_curves:curve(CurveName)
                catch
                    _:_ ->
                        undefined
                end,
            {Spec, CurveName}
    end.


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

-spec hash_equals(BinA, BinB) -> Result
          when BinA :: binary(),
               BinB :: binary(),
               Result :: boolean().
hash_equals(A, B) ->
  hash_equals_nif(A, B).

hash_equals_nif(_A, _B) -> ?nif_stub.

hash_algorithms() -> ?nif_stub.
pubkey_algorithms() -> ?nif_stub.
cipher_algorithms() -> ?nif_stub.
mac_algorithms() -> ?nif_stub.
curve_algorithms() -> ?nif_stub.
rsa_opts_algorithms() -> ?nif_stub.


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

-spec bytes_to_integer(binary()) -> integer() .
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

map_ensure_bin_as_int(List) when is_list(List) ->
    lists:map(fun ensure_bin_as_int/1, List).

ensure_bin_as_int(Bin) when is_binary(Bin) ->
    bin_to_int(Bin);
ensure_bin_as_int(E) ->
    E.

format_pkey(_Alg, #{engine:=_, key_id:=T}=M) when is_binary(T) -> format_pwd(M);
format_pkey(_Alg, #{engine:=_, key_id:=T}=M) when is_list(T) -> format_pwd(M#{key_id:=list_to_binary(T)});
format_pkey(_Alg, #{engine:=_           }=M) -> error({bad_key_id, M});
format_pkey(_Alg, #{}=M) -> error({bad_engine_map, M});
%%%
format_pkey(rsa, Key) ->
    map_ensure_int_as_bin(Key);
format_pkey(ecdsa, [Key, Curve]) ->
    {nif_curve_params(Curve), ensure_int_as_bin(Key)};
format_pkey(eddsa, [PubKey, Curve]) when  Curve == ed25519;
                                          Curve == ed448 ->
    [ensure_int_as_bin(PubKey), Curve];
format_pkey(dss, Key) ->
    map_ensure_int_as_bin(Key);
format_pkey(_, Key) ->
    Key.

format_pwd(#{password := Pwd}=M) when is_list(Pwd) -> M#{password := list_to_binary(Pwd)};
format_pwd(M) -> M.

%%--------------------------------------------------------------------
%%

%% large integer in a binary with 32bit length
%% MP representation  (SSH2)
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

%%%----------------------------------------------------------------
%% 9470495 == V(0,9,8,zh).
%% 268435615 == V(1,0,0,i).
%% 268439663 == V(1,0,1,f).

packed_openssl_version(MAJ, MIN, FIX, P0) ->
    %% crypto.c
    P1 = atom_to_list(P0),
    P = lists:sum([C-$a||C<-P1]),
    ((((((((MAJ bsl 8) bor MIN) bsl 8 ) bor FIX) bsl 8) bor (P+1)) bsl 4) bor 16#f).

%%--------------------------------------------------------------------
%% Engine nifs
engine_by_id_nif(_EngineId) -> ?nif_stub.
engine_init_nif(_Engine) -> ?nif_stub.
engine_free_nif(_Engine) -> ?nif_stub.
engine_load_dynamic_nif() -> ?nif_stub.
engine_ctrl_cmd_strings_nif(_Engine, _Cmds, _Optional) -> ?nif_stub.
engine_add_nif(_Engine)  -> ?nif_stub.
engine_remove_nif(_Engine)  -> ?nif_stub.
engine_register_nif(_Engine, _EngineMethod) -> ?nif_stub.
engine_unregister_nif(_Engine, _EngineMethod) -> ?nif_stub.
engine_get_first_nif() -> ?nif_stub.
engine_get_next_nif(_Engine) -> ?nif_stub.
engine_get_id_nif(_Engine) -> ?nif_stub.
engine_get_name_nif(_Engine) -> ?nif_stub.
engine_get_all_methods_nif() -> ?nif_stub.
ensure_engine_loaded_nif(_EngineId, _LibPath) -> ?nif_stub.

%%--------------------------------------------------------------------
%% Engine internals
engine_nif_wrapper(ok) ->
    ok;
engine_nif_wrapper(notsup) ->
    erlang:error(notsup);
engine_nif_wrapper({error, Error}) ->
    throw({error, Error}).

ensure_bin_chardata(CharData) when is_binary(CharData) ->
    CharData;
ensure_bin_chardata(CharData) ->
    unicode:characters_to_binary(CharData).

ensure_bin_cmds(CMDs) ->
    ensure_bin_cmds(CMDs, []).

ensure_bin_cmds([], Acc) ->
    lists:reverse(Acc);
ensure_bin_cmds([{Key, Value} |CMDs], Acc) ->
    ensure_bin_cmds(CMDs, [{ensure_bin_chardata(Key), ensure_bin_chardata(Value)} | Acc]);
ensure_bin_cmds([Key | CMDs], Acc) ->
    ensure_bin_cmds(CMDs, [{ensure_bin_chardata(Key), <<"">>} | Acc]).

engine_methods_convert_to_bitmask([], BitMask) ->
    BitMask;
engine_methods_convert_to_bitmask(engine_method_all, _BitMask) ->
    16#FFFF;
engine_methods_convert_to_bitmask(engine_method_none, _BitMask) ->
    16#0000;
engine_methods_convert_to_bitmask([M |Ms], BitMask) ->
    engine_methods_convert_to_bitmask(Ms, BitMask bor engine_method_atom_to_int(M)).

bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

engine_method_atom_to_int(engine_method_rsa) -> 16#0001;
engine_method_atom_to_int(engine_method_dsa) -> 16#0002;
engine_method_atom_to_int(engine_method_dh) -> 16#0004;
engine_method_atom_to_int(engine_method_rand) -> 16#0008;
engine_method_atom_to_int(engine_method_ecdh) -> 16#0010;
engine_method_atom_to_int(engine_method_ecdsa) -> 16#0020;
engine_method_atom_to_int(engine_method_ciphers) -> 16#0040;
engine_method_atom_to_int(engine_method_digests) -> 16#0080;
engine_method_atom_to_int(engine_method_store) -> 16#0100;
engine_method_atom_to_int(engine_method_pkey_meths) -> 16#0200;
engine_method_atom_to_int(engine_method_pkey_asn1_meths) -> 16#0400;
engine_method_atom_to_int(engine_method_ec) -> 16#0800;
engine_method_atom_to_int(X) ->
    erlang:error(badarg, [X]).

get_test_engine() ->
    Type = erlang:system_info(system_architecture),
    LibDir = filename:join([code:priv_dir(crypto), "lib"]),
    ArchDir = filename:join([LibDir, Type]),
    case filelib:is_dir(ArchDir) of
	true  -> check_otp_test_engine(ArchDir);
	false -> check_otp_test_engine(LibDir)
    end.

check_otp_test_engine(LibDir) ->
    case choose_otp_test_engine(LibDir) of
        false ->
            {error, notexist};
        LibName ->
            LibPath = filename:join(LibDir,LibName),
            case filelib:is_file(LibPath) of
                true ->
                    {ok, unicode:characters_to_binary(LibPath)};
                false ->
                    {error, notexist}
            end
    end.


choose_otp_test_engine(LibDir) ->
    LibNames = filelib:wildcard("otp_test_engine.*", LibDir),
    Type = atom_to_list(erlang:system_info(build_type)),
    choose_otp_test_engine(LibNames, Type, false).

choose_otp_test_engine([LibName | T], Type, Acc) ->
    case string:lexemes(LibName, ".") of
        [_, Type, _SO] ->
            LibName;  %% Choose typed if exists (valgrind,asan)
        [_, _SO] ->
            %% Fallback on typeless (opt)
            choose_otp_test_engine(T, Type, LibName);
        _ ->
            choose_otp_test_engine(T, Type, Acc)
    end;
choose_otp_test_engine([], _, Acc) ->
    Acc.

%%%----------------------------------------------------------------
%%% Error internals

err_find_args(undefined, [{?MODULE,_F,Args,_Info}|_]) ->  Args;
err_find_args(Args, _) -> Args.


err_remap_C_argnum(ArgNum, ArgMap) ->
    try
        element(ArgNum + 1, ArgMap) % 0-numbered in C-file's argv[], 1-numbered in the tuple
    of
        N when is_integer(N), N>0 -> N;
        _ -> undefined
    catch
        error:badarg when ArgNum >= 0 -> ArgNum+1; % short ArgMap
        error:badarg -> undefined
    end.


%%%----------------------------------------------------------------
