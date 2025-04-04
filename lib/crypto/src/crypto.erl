%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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
-moduledoc """
Crypto Functions

This module provides a set of cryptographic functions.

- **Hash functions** -

  - **SHA1, SHA2** - [Secure Hash Standard (FIPS PUB180-4)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)
  - **SHA3** - [SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions (FIPS PUB 202)](https://www.nist.gov/publications/sha-3-standard-permutation-based-hash-and-extendable-output-functions?pub_id=919061)

  - **BLAKE2** - [BLAKE2 — fast secure hashing](https://blake2.net/)

  - **SM3** - [The SM3 Hash Function (GM/T 0004-2012)](https://datatracker.ietf.org/doc/html/draft-sca-cfrg-sm3-02)

  - **MD5** - [The MD5 Message Digest Algorithm (RFC 1321)](http://www.ietf.org/rfc/rfc1321.txt)

  - **MD4** - [The MD4 Message Digest Algorithm (RFC 1320)](http://www.ietf.org/rfc/rfc1320.txt)

- **MACs - Message Authentication Codes** -

  - **Hmac functions** - [Keyed-Hashing for Message Authentication (RFC 2104)](http://www.ietf.org/rfc/rfc2104.txt)

  - **Cmac functions** - [The AES-CMAC Algorithm (RFC 4493)](http://www.ietf.org/rfc/rfc4493.txt)

  - **POLY1305** - [ChaCha20 and Poly1305 for IETF Protocols (RFC 7539)](http://www.ietf.org/rfc/rfc7539.txt)

- **Symmetric Ciphers** - 

  - **DES, 3DES and AES** - [Block Cipher Techniques (NIST)](https://csrc.nist.gov/projects/block-cipher-techniques)

  - **Blowfish** -
    [Fast Software Encryption, Cambridge Security Workshop Proceedings (December 1993), Springer-Verlag, 1994, pp. 191-204.](https://www.schneier.com/academic/archives/1994/09/description_of_a_new.html)

  - **Chacha20** - [ChaCha20 and Poly1305 for IETF Protocols (RFC 7539)](http://www.ietf.org/rfc/rfc7539.txt)

  - **Chacha20_poly1305** - [ChaCha20 and Poly1305 for IETF Protocols (RFC 7539)](http://www.ietf.org/rfc/rfc7539.txt)

  - **SM4** - [The SM4 Block Cipher Algorithm](https://www.iso.org/standard/81564.html)

- **Modes** -
  - **ECB, CBC, CFB, OFB and CTR** - [Recommendation for Block Cipher Modes of
    Operation: Methods and Techniques (NIST SP 800-38A)](https://csrc.nist.gov/publications/detail/sp/800-38a/final)

  - **GCM** - [Recommendation for Block Cipher Modes of Operation:
    Galois/Counter Mode (GCM) and GMAC (NIST SP 800-38D)](https://csrc.nist.gov/publications/detail/sp/800-38d/final)

  - **CCM** - [Recommendation for Block Cipher Modes of Operation: The CCM Mode
    for Authentication and Confidentiality (NIST SP 800-38C)](https://nvlpubs.nist.gov/nistpubs/legacy/sp/nistspecialpublication800-38c.pdf)

- **Asymmetric Ciphers - Public Key Techniques** -

  - **RSA** - [PKCS #1: RSA Cryptography Specifications (RFC 3447)](http://www.ietf.org/rfc/rfc3447.txt)

  - **DSS** - [Digital Signature Standard (DSS) (FIPS 186-4)](https://csrc.nist.gov/publications/detail/fips/186/4/final)

  - **ECDSA** - [Elliptic Curve Digital Signature Algorithm (ECDSA)](http://csrc.nist.gov/groups/STM/cavp/documents/dss2/ecdsa2vs.pdf)

  - **SRP** - [The SRP Authentication and Key Exchange System (RFC 2945)](http://www.ietf.org/rfc/rfc2945.txt)

> #### Note {: .info }
>
> The actual supported algorithms and features depends on their availability in
> the actual libcrypto used. See the [crypto (App)](crypto_app.md) about
> dependencies.
>
> Enabling FIPS mode will also disable algorithms and features.

The [CRYPTO User's Guide](index.html) has more information on FIPS, Engines and
Algorithm Details like key lengths.

## Exceptions

[](){: #error_old }

### Atoms - the older style

The exception `error:badarg` signifies that one or more arguments are of wrong
data type, or are otherwise badly formed.

The exception `error:notsup` signifies that the algorithm is known but is not
supported by current underlying libcrypto or explicitly disabled when building
that.

For a list of supported algorithms, see [supports(ciphers)](`supports/1`).

[](){: #error_3tup }

### 3-tuples - the new style

The exception is:

```text
error:{Tag, C_FileInfo, Description}

Tag = badarg | notsup | error
C_FileInfo = term()    % Usually only useful for the OTP maintainer
Description = string() % Clear text, sometimes only useful for the OTP maintainer
```

The exception tags are:

- **`badarg`** - Signifies that one or more arguments are of wrong data type or
  are otherwise badly formed.

- **`notsup`** - Signifies that the algorithm is known but is not supported by
  current underlying libcrypto or explicitly disabled when building that one.

- **`error`** - An error condition that should not occur, for example a memory
  allocation failed or the underlying cryptolib returned an error code, for
  example `"Can't initialize context, step 1"`. Those text usually needs
  searching the C-code to be understood.

Usually there are more information in the call stack about which argument caused
the exception and what the values where.

To catch the exception, use for example:

```text
try crypto:crypto_init(Ciph, Key, IV, true)
    catch
        error:{Tag, _C_FileInfo, Description} ->
            do_something(......)
         .....
end
```
""".


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
-deprecated([{start, 0, "use application:start(crypto) instead"},
             {stop,  0, "use application:stop(crypto) instead"},
             {enable_fips_mode, 1, "use config parameter fips_mode"}
            ]).

%%%----------------------------------------------------------------
%% Removed functions.
%%
%% Old interface. Now implemented with the New interface.
%% Removed in OTP-24.0 See OTP-16232 (deprecation) and OTP-16656 (removal)

-removed([{crypto_dyn_iv_init, 3, "not supported, use crypto:crypto_init/4"},
          {crypto_dyn_iv_update, 3, "not supported, use crypto:crypto_update/2"},
          {next_iv, '_', "see the 'New and Old API' chapter of the CRYPTO User's guide"},
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
                              "or crypto:crypto_init + "
                              "crypto:crypto_update + crypto:crypto_final instead"},
          {block_decrypt, 3,  "use crypto:crypto_one_time/4 or crypto:crypto_init/3 + "
                              "crypto:crypto_update/2 + crypto:crypto_final/1 instead"},
          {block_decrypt, 4,  "use crypto:crypto_one_time/5, crypto:crypto_one_time_aead/6,7 "
                              "or crypto:crypto_init + "
                              "crypto:crypto_update + crypto:crypto_final instead"}
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
         crypto_one_time_aead_init/4, crypto_one_time_aead/4,

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
       ng_crypto_update_nif/2, ng_crypto_final_nif/1,
       ng_crypto_get_data_nif/1, ng_crypto_one_time_nif/5,
       strong_rand_bytes_nif/1, strong_rand_range_nif/1, rand_uniform_nif/2,
       mod_exp_nif/4, do_exor/2, hash_equals_nif/2, pbkdf2_hmac_nif/5,
       pkey_sign_nif/5, pkey_verify_nif/6, pkey_crypt_nif/6,
       rsa_generate_key_nif/2, dh_generate_key_nif/4, dh_compute_key_nif/3,
       evp_compute_key_nif/3, evp_generate_key_nif/2, privkey_to_pubkey_nif/2,
       srp_value_B_nif/5, srp_user_secret_nif/7, srp_host_secret_nif/5,
       ec_generate_key_nif/2, ecdh_compute_key_nif/3, rand_seed_nif/1,
       aead_cipher_nif/7, aead_cipher_init_nif/4, aead_cipher_nif/4,
       engine_by_id_nif/1, engine_init_nif/1,
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
               sha2/0,
               sha3/0
             ]).

-export_type([
              hmac_hash_algorithm/0,
              cmac_cipher_algorithm/0
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
-doc "Always `t:binary/0` when used as return value".
-doc(#{group => <<"Keys">>}).
-type key_integer() :: integer() | binary(). % Always binary() when used as return value

%%% Keys
-doc(#{group => <<"Public/Private Keys">>,equiv => rsa_params()}).
-type rsa_public() :: [key_integer()] . % [E, N]
-doc(#{group => <<"Public/Private Keys">>,equiv => rsa_params()}).
-type rsa_private() :: [key_integer()] . % [E, N, D] | [E, N, D, P1, P2, E1, E2, C]
-doc """
```text
rsa_public() = [E, N]
```

```erlang
rsa_private() = [E, N, D] | [E, N, D, P1, P2, E1, E2, C]
```

Where E is the public exponent, N is public modulus and D is the private
exponent. The longer key format contains redundant information that will make
the calculation faster. P1 and P2 are first and second prime factors. E1 and E2
are first and second exponents. C is the CRT coefficient. The terminology is
taken from [RFC 3447](http://www.ietf.org/rfc/rfc3447.txt).
""".
-doc(#{group => <<"Public/Private Keys">>}).
-type rsa_params() :: {ModulusSizeInBits::integer(), PublicExponent::key_integer()} .

-doc(#{group => <<"Public/Private Keys">>,equiv => dss_private()}).
-type dss_public() :: [key_integer()] . % [P, Q, G, Y]
-doc """
```text
dss_public() = [P, Q, G, Y]
```

Where P, Q and G are the dss parameters and Y is the public key.

```text
dss_private() = [P, Q, G, X]
```

Where P, Q and G are the dss parameters and X is the private key.
""".
-doc(#{group => <<"Public/Private Keys">>}).
-type dss_private() :: [key_integer()] . % [P, Q, G, X]

-doc(#{group => <<"Public/Private Keys">>,equiv => ecdsa_params()}).
-type ecdsa_public()  :: key_integer() .
-doc(#{group => <<"Public/Private Keys">>,equiv => ecdsa_params()}).
-type ecdsa_private() :: key_integer() .
-doc(#{group => <<"Public/Private Keys">>}).
-type ecdsa_params()  :: ec_named_curve() | ec_explicit_curve() .

-doc(#{group => <<"Public/Private Keys">>,equiv => eddsa_params()}).
-type eddsa_public()  :: key_integer() .
-doc(#{group => <<"Public/Private Keys">>,equiv => eddsa_params()}).
-type eddsa_private() :: key_integer() .
-doc(#{group => <<"Public/Private Keys">>}).
-type eddsa_params()  :: edwards_curve_ed() .

-doc(#{group => <<"Public/Private Keys">>,equiv => srp_private()}).
-type srp_public() :: key_integer() .
-doc """
```text
srp_public() = key_integer()
```

Where is `A` or `B` from [SRP design](http://srp.stanford.edu/design.html)

```text
srp_private() = key_integer()
```

Where is `a` or `b` from [SRP design](http://srp.stanford.edu/design.html)
""".
-doc(#{group => <<"Public/Private Keys">>}).
-type srp_private() :: key_integer() .
-doc(#{group => <<"Public/Private Keys">>,
       equiv => srp_host_comp_params()}).
-type srp_gen_params()  :: {user,srp_user_gen_params()}  | {host,srp_host_gen_params()}.
-doc(#{group => <<"Public/Private Keys">>,
       equiv => srp_host_comp_params()}).
-type srp_comp_params() :: {user,srp_user_comp_params()} | {host,srp_host_comp_params()}.
-doc(#{group => <<"Public/Private Keys">>,
       equiv => srp_host_comp_params()}).
-type srp_user_gen_params() :: list(binary() | atom() | list()) .
-doc(#{group => <<"Public/Private Keys">>,
       equiv => srp_host_comp_params()}).
-type srp_host_gen_params() :: list(binary() | atom() | list()) .
-doc(#{group => <<"Public/Private Keys">>,
       equiv => srp_host_comp_params()}).
-type srp_user_comp_params() :: list(binary() | atom()) .
-doc """
Where Verifier is `v`, Generator is `g` and Prime is` N`, DerivedKey is `X`, and
Scrambler is `u` (optional will be generated if not provided) from
[SRP design](http://srp.stanford.edu/design.html) Version = '3' | '6' | '6a'
""".
-doc(#{group => <<"Public/Private Keys">>}).
-type srp_host_comp_params() :: list(binary() | atom()) .

-doc(#{group => <<"Diffie-Hellman Keys and parameters">>,
       equiv => dh_private()}).
-type dh_public() :: key_integer() .
-doc(#{group => <<"Diffie-Hellman Keys and parameters">>}).
-type dh_private() :: key_integer() .
-doc """
```text
dh_params() = [P, G] | [P, G, PrivateKeyBitLength]
```
""".
-doc(#{group => <<"Diffie-Hellman Keys and parameters">>}).
-type dh_params() :: [key_integer()] . % [P, G] | [P, G, PrivateKeyBitLength]

-doc(#{group => <<"Diffie-Hellman Keys and parameters">>,
       equiv => ecdh_params()}).
-type ecdh_public()  :: key_integer() .
-doc(#{group => <<"Diffie-Hellman Keys and parameters">>,
       equiv => ecdh_params()}).
-type ecdh_private() :: key_integer() .
-doc(#{group => <<"Diffie-Hellman Keys and parameters">>}).
-type ecdh_params()  :: ec_named_curve() | edwards_curve_dh() | ec_explicit_curve() .


%%% Curves

-doc(#{group => <<"Elliptic Curves">>,equiv => ec_curve()}).
-type ec_explicit_curve() :: {Field :: ec_field(),
                              Curve :: ec_curve(),
                              BasePoint :: binary(),
                              Order :: binary(),
                              CoFactor :: none | % FIXME: Really?
                                          binary()
                             } .

-doc "Parametric curve definition.".
-doc(#{group => <<"Elliptic Curves">>}).
-type ec_curve() :: {A :: binary(),
                     B :: binary(),
                     Seed :: none | binary()
                    } .

-doc(#{group => <<"Elliptic Curves">>,equiv => ec_curve()}).
-type ec_field() ::  ec_prime_field() | ec_characteristic_two_field() .

-doc(#{group => <<"Elliptic Curves">>,equiv => ec_basis()}).
-type ec_prime_field()              :: {prime_field, Prime :: integer()} .
-doc(#{group => <<"Elliptic Curves">>,equiv => ec_basis()}).
-type ec_characteristic_two_field() :: {characteristic_two_field, M :: integer(), Basis :: ec_basis()} .

-doc "Curve definition details.".
-doc(#{group => <<"Elliptic Curves">>}).
-type ec_basis() :: {tpbasis, K :: non_neg_integer()}
                  | {ppbasis, K1 :: non_neg_integer(), K2 :: non_neg_integer(), K3 :: non_neg_integer()}
                  |  onbasis .

-doc(#{group => <<"Elliptic Curves">>,equiv => edwards_curve_ed()}).
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

-doc(#{group => <<"Elliptic Curves">>,equiv => edwards_curve_ed()}).
-type edwards_curve_dh() :: x25519 | x448 .

-doc(#{group => <<"Utility functions">>}).
-doc "Note that some curves are disabled if FIPS is enabled.".
-doc(#{group => <<"Elliptic Curves">>}).
-type edwards_curve_ed() :: ed25519 | ed448 .

%%%----------------------------------------------------------------
%%% New cipher schema
%%%
-doc(#{group => <<"Ciphers">>}).
-type cipher() :: cipher_no_iv()
                | cipher_iv()
                | cipher_aead() .

-doc(#{group => <<"Ciphers">>}).
-type cipher_no_iv() :: aes_128_ecb
                      | aes_192_ecb
                      | aes_256_ecb
                      | aes_ecb

                      | blowfish_ecb
                      | des_ecb
                      | sm4_ecb
                      | rc4 .

-doc(#{group => <<"Ciphers">>}).
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

                   | sm4_cbc
                   | sm4_ofb
                   | sm4_cfb
                   | sm4_ctr

                   | blowfish_cbc
                   | blowfish_cfb64
                   | blowfish_ofb64
                   | chacha20
                   | des_ede3_cbc
                   | des_ede3_cfb

                   | des_cbc
                   | des_cfb
                   | rc2_cbc .


-doc """
Ciphers known by the CRYPTO application.

Note that this list might be reduced if the underlying libcrypto does not
support all of them.
""".
-doc(#{group => <<"Ciphers">>}).
-type cipher_aead() :: aes_128_ccm
                     | aes_192_ccm
                     | aes_256_ccm
                     | aes_ccm

                     | aes_128_gcm
                     | aes_192_gcm
                     | aes_256_gcm
                     | aes_gcm

                     | sm4_gcm
                     | sm4_ccm

                     | chacha20_poly1305 .


%%%----------------------------------------------------------------

-doc(#{group => <<"Digests and hash">>}).
-type rsa_digest_type()   :: sha1() | sha2() | md5 | ripemd160 .
-doc(#{group => <<"Digests and hash">>}).
-type dss_digest_type()   :: sha1() | sha2() .
-doc(#{group => <<"Digests and hash">>}).
-type ecdsa_digest_type() :: sha1() | sha2() .

-doc(#{group => <<"Digests and hash">>,equiv => blake2()}).
-type sha1() :: sha .
-doc(#{group => <<"Digests and hash">>,equiv => blake2()}).
-type sha2() :: sha224 | sha256 | sha384 | sha512 .
-doc(#{group => <<"Digests and hash">>,equiv => blake2()}).
-type sha3() :: sha3_224 | sha3_256 | sha3_384 | sha3_512 .
-doc(#{group => <<"Digests and hash">>,equiv => blake2()}).
-type sha3_xof() :: shake128 | shake256 .
-doc(#{group => <<"Digests and hash">>}).
-type blake2() :: blake2b | blake2s .

-doc """
The `t:compatibility_only_hash/0` algorithms are recommended only for
compatibility with existing applications.
""".
-doc(#{group => <<"Digests and hash">>}).
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
-doc false.
version() ->
    ?CRYPTO_VSN.

-doc false.
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

-doc(#{group => <<"Deprecated API">>}).
-doc """
Use [`application:start(crypto)`](`application:start/1`) instead.

> #### Warning {: .warning }
>
> This function does not work if FIPS mode is to be enabled. FIPS mode will be
> disabled even if configuration parameter `fips_mode` is set to `true`. Use
> [`application:start(crypto)`](`application:start/1`) instead.
""".
-spec start() -> ok | {error, Reason::term()}.
start() ->
    application:start(crypto).

-doc(#{group => <<"Deprecated API">>}).
-doc "Use [`application:stop(crypto)`](`application:stop/1`) instead.".
-spec stop() -> ok | {error, Reason::term()}.
stop() ->
    application:stop(crypto).

-doc false.
-spec supports() -> [Support]
                        when Support :: {hashs,   Hashs}
                                      | {ciphers, Ciphers}
                                      | {public_keys, PKs}
                                      | {macs,    Macs}
                                      | {curves,  Curves}
                                      | {rsa_opts, RSAopts},
                             Hashs :: [sha1() | sha2() | sha3() | sha3_xof() | blake2() | ripemd160 | sm3 | compatibility_only_hash()],
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


-doc """
Get which crypto algorithms that are supported by the underlying libcrypto
library.

See `hash_info/1` and `cipher_info/1` for information about the hash and cipher
algorithms.
""".
-doc(#{since => <<"OTP 22.0">>}).
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

-doc(#{group => <<"Utility Functions">>}).
supports(hashs)       -> hash_algorithms();
supports(public_keys) -> pubkey_algorithms();
supports(ciphers)     -> add_cipher_aliases(cipher_algorithms());
supports(macs)        -> mac_algorithms();
supports(curves)      -> curve_algorithms();
supports(rsa_opts)    -> rsa_opts_algorithms().

-doc(#{group => <<"Utility Functions">>}).
-doc """
Get the name and version of the libraries used by crypto.

`Name` is the name of the library. `VerNum` is the numeric version according to
the library's own versioning scheme. `VerStr` contains a text variant of the
version.

```erlang
> info_lib().
[{<<"OpenSSL">>,269484095,<<"OpenSSL 1.1.0c  10 Nov 2016"">>}]
```

> #### Note {: .info }
>
> From OTP R16 the _numeric version_ represents the version of the OpenSSL
> _header files_ (`openssl/opensslv.h`) used when crypto was compiled. The text
> variant represents the libcrypto library used at runtime. In earlier OTP
> versions both numeric and text was taken from the library.
""".
-spec info_lib() -> [{Name,VerNum,VerStr}] when Name :: binary(),
                                                VerNum :: integer(),
                                                VerStr :: binary() .
info_lib() -> ?nif_stub.

-doc(#{group => <<"Utility Functions">>}).
-doc """
Get information about crypto and the OpenSSL backend.

Returns a map with information about the compilation and linking of crypto.

Example:

```erlang
1> crypto:info().
#{compile_type => normal,
  cryptolib_version_compiled => "OpenSSL 3.0.0 7 sep 2021",
  cryptolib_version_linked => "OpenSSL 3.0.0 7 sep 2021",
  link_type => dynamic,
  otp_crypto_version => "5.0.2",
  fips_provider_available => true,
  fips_provider_buildinfo => "3.0.0"}
2>
```

More association types than documented may be present in the map. Some of the
associations (like fips) may be absent if not supported.
""".
-doc(#{group => <<"Utility Functions">>,
       since => <<"OTP 24.2">>}).
-spec info() -> #{compile_type := normal | debug | valgrind | asan,
                  cryptolib_version_compiled := string() | undefined,
                  cryptolib_version_linked := string(),
                  link_type := dynamic | static,
                  otp_crypto_version := string(),
                  fips_provider_available => boolean(),
                  fips_provider_buildinfo => string()
                 }.
info() -> 
    (info_nif())#{otp_crypto_version => crypto:version()}.

info_nif() -> ?nif_stub.


-doc """
Get information about the operating status of FIPS.

Returns the FIPS operating status of crypto and the underlying libcrypto
library. If crypto was built with FIPS support this can be either `enabled`
(when running in FIPS mode) or `not_enabled`. For other builds
this value is always `not_supported`.

See configuration parameter [fips_mode](`e:crypto:crypto_app.md#fips_mode`)
about how to enable FIPS mode.

> #### Warning {: .warning }
>
> In FIPS mode all non-FIPS compliant algorithms are disabled and raise
> exception `error:notsup`. Check [supports(ciphers)](`supports/1`) that in FIPS
> mode returns the restricted list of available algorithms.
""".
-doc(#{group => <<"Utility Functions">>,
       since => <<"OTP 20.0">>}).
-spec info_fips() -> not_supported | not_enabled | enabled.

info_fips() -> ?nif_stub.

-doc """
Enable or disable FIPs mode.

Argument `Enable` should be `true` to enable and `false` to disable FIPS mode.
Returns `true` if the operation was successful or `false` otherwise.

Note that to enable FIPS mode successfully, OTP must be built with the configure
option `--enable-fips`, and the underlying libcrypto must also support FIPS.

See also `info_fips/0`.
""".
-doc(#{group => <<"Deprecated API">>,
       since => <<"OTP 21.1">>}).
-spec enable_fips_mode(Enable) -> Result when Enable :: boolean(),
                                              Result :: boolean().
enable_fips_mode(Enable) ->
    enable_fips_mode_nif(Enable).

enable_fips_mode_nif(_) -> ?nif_stub.

-doc(#{group => <<"MAC API">>}).
-doc """
PKCS #5 PBKDF2 (Password-Based Key Derivation Function 2) in combination with
HMAC.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 24.2">>}).
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

-doc(#{group => <<"Digests and hash">>}).
-type hash_algorithm() :: sha1() | sha2() | sha3() | sha3_xof() | blake2() | ripemd160 | sm3 | compatibility_only_hash() .
-doc(#{group => <<"Digests and hash">>}).
-type hash_xof_algorithm() :: sha3_xof() .

-doc """
Get information about a hash algorithm.

Returns a map with information about block_size, size and possibly other
properties of the hash algorithm in question.

For a list of supported hash algorithms, see [supports(hashs)](`supports/1`).
""".
-doc(#{group => <<"Utility Functions">>,
       since => <<"OTP 22.0">>}).
-spec hash_info(Type) -> Result
                             when Type :: hash_algorithm(),
                                  Result :: #{size := integer(),
                                              block_size := integer(),
                                              type := integer()
                                             } .
hash_info(Type) ->
    hash_info_nif(Type).

-doc """
Compute a message digest.

Argument `Type` is the digest type and argument `Data` is the full message.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Hash API">>,
       since => <<"OTP R15B02">>}).
-spec hash(Type, Data) -> Digest when Type :: hash_algorithm(),
                                      Data :: iodata(),
                                      Digest :: binary().
hash(Type, Data) ->
    Data1 = iolist_to_binary(Data),
    MaxBytes = max_bytes(),
    hash(Type, Data1, erlang:byte_size(Data1), MaxBytes).

-doc """
Compute a message digest for an `xof_algorithm`.

Argument `Type` is the type of digest, `Data` is the full text and `Length` is
the digest length in bits.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

May raise exception `error:notsup` in case the chosen `Type` is not supported by
the underlying libcrypto implementation.
""".
-doc(#{group => <<"Hash API">>,
       since => <<"OTP 26.0">>}).
-spec hash_xof(Type, Data, Length) -> Digest when Type :: hash_xof_algorithm(),
                                               Data :: iodata(),
                                               Length :: non_neg_integer(),
                                               Digest :: binary().
hash_xof(Type, Data, Length) ->
  Data1 = iolist_to_binary(Data),
  hash_xof(Type, Data1, erlang:byte_size(Data1), Length).

-doc(#{group => <<"Internal data types">>,equiv => mac_state()}).
-opaque hash_state() :: reference().

-doc """
Initialize the state for a streaming hash digest calculation.

Argument `Type` determines which digest to use. The returned state should be
used as argument to `hash_update/2`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Hash API">>,
       since => <<"OTP R15B02">>}).
-spec hash_init(Type) -> State when Type :: hash_algorithm(),
                                    State :: hash_state().
hash_init(Type) ->
    ?nif_call(hash_init_nif(Type)).

-doc """
Add data to a streaming digest calculation.

Update the digest using the given `Data` of any length.

Argument `State` must have been generated by [hash_init](`hash_init/1`) or a
previous call to this function.

Returns `NewState` that must be passed into the next call to `hash_update/2` or
`hash_final/1`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Hash API">>,
       since => <<"OTP R15B02">>}).
-spec hash_update(State, Data) -> NewState when State :: hash_state(),
                                                NewState :: hash_state(),
                                                Data :: iodata() .
hash_update(State, Data) ->
    Data1 = iolist_to_binary(Data),
    MaxBytes = max_bytes(),
    hash_update(State, Data1, erlang:byte_size(Data1), MaxBytes).

-doc """
Finalize a streaming hash calculation.

Argument `State` as returned from the last call to
[hash_update](`hash_update/2`). The size of `Digest` is determined by
the type of hash function used to generate it.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Hash API">>,
       since => <<"OTP R15B02">>}).
-spec hash_final(State) -> Digest when State :: hash_state(),
                                       Digest :: binary().
hash_final(State) ->
    ?nif_call(hash_final_nif(State)).

-doc false.
-spec hash_final_xof(State, Length) -> Digest when State :: hash_state(),
                                                   Length :: non_neg_integer(),
                                                   Digest :: binary().
hash_final_xof(State, Length) ->
    hash_final_xof_nif(State, Length).

%%%================================================================
%%%
%%% MACs (Message Authentication Codes)
%%%
%%%================================================================

-doc(#{group => <<"Digests and hash">>}).
-type hmac_hash_algorithm() ::  sha1() | sha2() | sha3() | sm3 | compatibility_only_hash().

-doc(#{group => <<"Digests and hash">>}).
-type cmac_cipher_algorithm() :: aes_128_cbc    | aes_192_cbc    | aes_256_cbc    | aes_cbc
                               | blowfish_cbc
                               | des_cbc | des_ede3_cbc
                               | rc2_cbc
                                 .

%%%----------------------------------------------------------------
%%% Calculate MAC for the whole text at once

-doc """
Compute a `poly1305` MAC (Message Authentication Code).

Same as [`mac(Type, undefined, Key, Data)`](`mac/4`).

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
-spec mac(Type :: poly1305, Key, Data) -> Mac
                     when Key :: iodata(),
                          Data :: iodata(),
                          Mac :: binary().

mac(poly1305, Key, Data) -> mac(poly1305, undefined, Key, Data).


-doc """
Compute a MAC (Message Authentication Code).

Argument `Type` is the type of MAC and `Data` is the full message.

`SubType` depends on the MAC `Type`:

- For `hmac` it is a hash algorithm, see
  [Algorithm Details](algorithm_details.md#hmac) in the User's Guide.
- For `cmac` it is a cipher suitable for cmac, see
  [Algorithm Details](algorithm_details.md#cmac) in the User's Guide.
- For `poly1305` it should be set to `undefined` or the [mac/2](`mac_init/2`)
  function could be used instead, see
  [Algorithm Details](algorithm_details.md#poly1305) in the User's Guide.

`Key` is the authentication key with a length according to the `Type` and
`SubType`. The key length could be found with the `hash_info/1` (`hmac`) for and
`cipher_info/1` (`cmac`) functions. For `poly1305` the key length is 32 bytes.
Note that the cryptographic quality of the key is not checked.

The `Mac` result will have a default length depending on the `Type` and
`SubType`. To set a shorter length, use `macN/4` or `macN/5` instead. The
default length is documented in
[Algorithm Details](algorithm_details.md#message-authentication-codes-macs) in
the User's Guide.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
-spec mac(Type, SubType, Key, Data) -> Mac
                     when Type :: hmac | cmac | poly1305,
                          SubType :: hmac_hash_algorithm() | cmac_cipher_algorithm() | undefined,
                          Key :: iodata(),
                          Data :: iodata(),
                          Mac :: binary().

mac(Type, SubType, Key0, Data) ->
    Key = iolist_to_binary(Key0),
    ?nif_call(mac_nif(Type, alias(SubType,Key), Key, Data)).


-doc """
Compute a `poly1305` MAC (Message Authentication Code) with a limited length.

Same as [`macN(Type, undefined, Key, Data, MacLength)`](`macN/5`).

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
-spec macN(Type :: poly1305, Key, Data, MacLength) -> Mac
                     when Key :: iodata(),
                          Data :: iodata(),
                          Mac :: binary(),
                          MacLength :: pos_integer().

macN(Type, Key, Data, MacLength) ->
    macN(Type, undefined, Key, Data, MacLength).


-doc """
Compute a MAC (Message Authentication Code) with a limited length.

Works like `mac/3` and `mac/4` but `MacLength` will limit the size of the
resultant `Mac` to at most `MacLength` bytes. Note that if `MacLength` is
greater than the actual number of bytes returned from the underlying hash, the
returned hash will have that shorter length instead.

The max `MacLength` is documented in
[Algorithm Details](algorithm_details.md#message-authentication-codes-macs) in
the User's Guide.
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
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

-doc """
Contexts with an internal state that should not be manipulated but passed
between function calls.
""".
-doc(#{group => <<"Internal data types">>}).
-opaque mac_state() :: reference() .

-doc """
Initialize a state for streaming `poly1305` MAC calculation.

Same as [`mac_init(Type, undefined, Key)`](`mac_init/3`).

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
-spec mac_init(Type :: poly1305, Key) -> State
                          when Key :: iodata(),
                               State :: mac_state() .
mac_init(poly1305, Key) ->
    ?nif_call(mac_init_nif(poly1305, undefined, Key)).


-doc """
Initialize the state for streaming MAC calculation.

`Type` determines which mac algorithm to use in the MAC operation.

`SubType` depends on the MAC `Type`:

- For `hmac` it is a hash algorithm, see
  [Algorithm Details](algorithm_details.md#hmac) in the User's Guide.
- For `cmac` it is a cipher suitable for cmac, see
  [Algorithm Details](algorithm_details.md#cmac) in the User's Guide.
- For `poly1305` it should be set to `undefined` or the [mac/2](`mac_init/2`)
  function could be used instead, see
  [Algorithm Details](algorithm_details.md#poly1305) in the User's Guide.

`Key` is the authentication key with a length according to the `Type` and
`SubType`. The key length could be found with the `hash_info/1` (`hmac`) for and
`cipher_info/1` (`cmac`) functions. For `poly1305` the key length is 32 bytes.
Note that the cryptographic quality of the key is not checked.

The returned `State` should be used in one or more subsequent calls to
`mac_update/2`. The MAC value is finally returned by calling `mac_final/1` or
`mac_finalN/2`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See
[examples in the User's Guide.](new_api.md#example-of-mac_init-mac_update-and-mac_final)
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
-spec mac_init(Type, SubType, Key) -> State
                          when Type :: hmac | cmac | poly1305,
                               SubType :: hmac_hash_algorithm() | cmac_cipher_algorithm() | undefined,
                               Key :: iodata(),
                               State :: mac_state() .
mac_init(Type, SubType, Key0) ->
    Key = iolist_to_binary(Key0),
    ?nif_call(mac_init_nif(Type, alias(SubType,Key), Key)).


-doc """
Add data to a streaming MAC calculation.

Update the MAC represented by `State0` using the given `Data` which could be of
any length.

The `State0` is the State value originally from a MAC init function, that is
`mac_init/2`, `mac_init/3` or the last call to `mac_update/2`. The value
`State0` is returned unchanged by the function as a reference to a mutated
internal state. Hence, it is not possible to branch off a data stream by reusing
old states.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
-spec mac_update(State0, Data) -> State
                     when Data :: iodata(),
                          State0 :: mac_state(),
                          State :: mac_state().
mac_update(Ref, Data) ->
    ?nif_call(mac_update_nif(Ref, Data)).



-doc """
Finalize a streaming MAC operation.

Argument `State` is the state as returned by the last call to `mac_update/2`.

The `Mac` result will have a default length depending on the `Type` and `SubType` in the
[mac_init/2,3](`mac_init/3`) call. To set a shorter length, use `mac_finalN/2`
instead. The default length is documented in
[Algorithm Details](algorithm_details.md#message-authentication-codes-macs) in
the User's Guide.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
-spec mac_final(State) -> Mac
                              when State :: mac_state(),
                                   Mac :: binary().
mac_final(Ref) ->
    ?nif_call(mac_final_nif(Ref)).


-doc """
Finalize a MAC operation with a custom length.

Argument `State` is the state as returned by the last call to `mac_update/2`.

`Mac` will be a binary with at most `MacLength` bytes. Note that if `MacLength`
is greater than the actual number of bytes returned from the underlying hash,
the returned hash will have that shorter length instead.

The max `MacLength` is documented in
[Algorithm Details](algorithm_details.md#message-authentication-codes-macs) in
the User's Guide.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"MAC API">>,
       since => <<"OTP 22.1">>}).
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
-doc """
Get information about a cipher algorithm.

Returns a map with information about block size, key length, IV length, aead
support and possibly other properties of the cipher algorithm in question.

> #### Note {: .info }
>
> The ciphers `aes_cbc`, `aes_cfb8`, `aes_cfb128`, `aes_ctr`, `aes_ecb`,
> `aes_gcm` and `aes_ccm` has no keylength in the `Type` as opposed to for
> example `aes_128_ctr`. They adapt to the length of the key provided in the
> encrypt and decrypt function. Therefore it is impossible to return a valid
> keylength in the map.
>
> Always use a `Type` with an explicit key length,

For a list of supported cipher algorithms, see
[supports(ciphers)](`supports/1`).
""".
-doc(#{group => <<"Utility Functions">>,
       since => <<"OTP 22.0">>}).
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

-doc(#{group => <<"Internal data types">>,equiv => mac_state()}).
-opaque crypto_state() :: reference() .

-doc(#{group => <<"Ciphers">>,equiv => crypto_opt()}).
-type crypto_opts() :: boolean()
                     | [ crypto_opt() ] .
-doc "Selects encryption (`{encrypt,true}`) or decryption (`{encrypt,false}`).".
-doc(#{group => <<"Ciphers">>}).
-type crypto_opt() :: {encrypt,boolean()}
                    | {padding, padding()} .
-doc """
This option handles padding in the last block. If not set, no padding is done
and any bytes in the last unfilled block is silently discarded.
""".
-doc(#{group => <<"Ciphers">>}).
-type padding() :: cryptolib_padding() | otp_padding().
-doc """
The `cryptolib_padding` are paddings that may be present in the underlying
cryptolib linked to the Erlang/OTP crypto app.

For OpenSSL, see the [OpenSSL documentation](https://openssl.org). and find
`EVP_CIPHER_CTX_set_padding()` in cryptolib for your linked version.
""".
-doc(#{group => <<"Ciphers">>}).
-type cryptolib_padding() :: none | pkcs_padding .
-doc "Erlang/OTP adds a either padding of zeroes or padding with random bytes.".
-doc(#{group => <<"Ciphers">>}).
-type otp_padding() :: zero | random .


%%%----------------------------------------------------------------
%%%
%%% Create and initialize a new state for encryption or decryption
%%%

-doc """
Initialize the state for a streaming encryption or decryption
operation.

Equivalent to the call
[`crypto_init(Cipher, Key, <<>>, FlagOrOptions)`](`crypto_init/4`). It is
intended for ciphers without an IV (nounce).

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 22.0">>}).
-spec crypto_init(Cipher, Key, FlagOrOptions) -> State
                                                   when Cipher :: cipher_no_iv(),
                                                        Key :: iodata(),
                                                        FlagOrOptions :: crypto_opts() | boolean(),
                                                        State :: crypto_state() .
crypto_init(Cipher, Key, FlagOrOptions) ->
    ?nif_call(ng_crypto_init_nif(alias(Cipher,Key), Key, <<>>, FlagOrOptions),
              {1,2,-1,3}
             ).

-doc """
Initialize the state for a streaming encryptions or decryptions operation.

The returned state should be used as argument to `crypto_update/2` and
`crypto_final/1` to do the actual encryption or decryption.

If `IV = <<>>`, no IV is used. This is intended for ciphers without an IV
(nounce). See `crypto_init/3`.

For encryption, set the `FlagOrOptions` to `true` or `[{encrypt,true}]`. For
decryption, set it to `false` or `[{encrypt,false}]`.

Padding could be enabled with the option [\{padding,Padding\}](`t:padding/0`).
The [cryptolib_padding](`t:cryptolib_padding/0`) enables `pkcs_padding` or no
padding (`none`). The paddings `zero` or `random` fills the last part of the
last block with zeroes or random bytes. If the last block is already full,
nothing is added.

In decryption, the [cryptolib_padding](`t:cryptolib_padding/0`) removes such
padding, if present. The [otp_padding](`t:otp_padding/0`) is not removed - it
has to be done elsewhere.

If padding is `{padding,none}` or not specified and the total data from all
subsequent [crypto_updates](`crypto_update/2`) does not fill the last block
fully, that last data is lost. In case of `{padding,none}` there will be an
error in this case. If padding is not specified, the bytes of the unfilled block
is silently discarded.

The actual padding is performed by `crypto_final/1`.

For blocksizes call `cipher_info/1`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See
[examples in the User's Guide.](new_api.md#examples-of-crypto_init-4-and-crypto_update-2)
""".
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 22.0">>}).
-spec crypto_init(Cipher, Key, IV, FlagOrOptions) -> State
                                                       when Cipher :: cipher_iv(),
                                                            Key :: iodata(),
                                                            IV :: iodata(),
                                                            FlagOrOptions :: crypto_opts(),
                                                            State :: crypto_state() .
crypto_init(Cipher, Key, IV, FlagOrOptions) ->
    ?nif_call(ng_crypto_init_nif(alias(Cipher,Key), Key, IV, FlagOrOptions)).

%%%----------------------------------------------------------------
%%%
%%% Encrypt/decrypt a sequence of bytes.  The sum of the sizes
%%% of all blocks must be an integer multiple of the crypto's
%%% blocksize.
%%%

-doc """
Add data to a streaming encryption or decryption operation.

If the part is less than a number of full blocks, only the full blocks (possibly
none) are encrypted or decrypted and the remaining bytes are saved to the next
`crypto_update` operation. The `State` should be created with `crypto_init/3` or
`crypto_init/4`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See
[examples in the User's Guide.](new_api.md#examples-of-crypto_init-4-and-crypto_update-2)
""".
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 22.0">>}).
-spec crypto_update(State, Data) -> Result
                            when State :: crypto_state(),
                                 Data :: iodata(),
                                 Result :: binary() .
crypto_update(State, Data) ->
    ?nif_call(ng_crypto_update_nif(State, Data)).


%%%----------------------------------------------------------------
%%%
%%% Finalize encrypt/decrypt bytes.  If the size of the bytes in
%%% to crypto_uptate was not an integer number of blocks, the rest
%%% is returned from this function.

-doc """
Finalize a streaming encryptions or decryptions operation and delivers the final
bytes of the final block.

The data returned from this function may be empty if no padding was enabled in
`crypto_init/3` or `crypto_init/4`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 23.0">>}).
-spec crypto_final(State) -> FinalResult
                            when State :: crypto_state(),
                                 FinalResult :: binary() .
crypto_final(State) ->
    ?nif_call(ng_crypto_final_nif(State)).

%%%----------------------------------------------------------------
%%%
%%% Get result of padding etc

-doc """
Return information about a `t:crypto_state/0`.

The information returned is a map, which currently contains at least:

- **`size`** - The number of bytes encrypted or decrypted so far.

- **`padding_size`** - After a call to `crypto_final/1` it contains the number
  of bytes padded. Otherwise 0.

- **`padding_type`** - The type of the padding as provided in the call to
  `crypto_init/3` or `crypto_init/4`.

- **`encrypt`** - Is `true` if encryption is performed. It is `false` otherwise.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 23.0">>}).
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

-doc """
Do a complete encrypt or decrypt of the full text.

As `crypto_one_time/5` but for ciphers without IVs.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 22.0">>}).
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


-doc """
Do a complete encrypt or decrypt of the full text.

Argument `Data` is the text to be encrypted or decrypted.

For encryption, set the `FlagOrOptions` to `true`. For decryption, set it to
`false`. For setting other options, see `crypto_init/4`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See [examples in the User's Guide.](new_api.md#example-of-crypto_one_time-5)
""".
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 22.0">>}).
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
-doc(#{equiv => crypto_one_time_aead/7}).
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 22.0">>}).
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

-doc """
Do a complete encrypt or decrypt with an AEAD cipher of the full text.

For encryption, set the `EncryptFlag` to `true` and set the `TagOrTagLength` to
the wanted size (in bytes) of the tag, that is, the tag length. If the default
length is wanted, the `crypto_one_time_aead/6` form may be used.

For decryption, set the `EncryptFlag` to `false` and put the tag to be checked
in the argument `TagOrTagLength`.

Additional Authentication Data (AAD) is plaintext data that will not be
encrypted, but will be covered by authenticity protection. It should be provided
through the `AAD` argument, but can be an empty binary as well (`<<>>`) if not
needed. In that case, a plain AE (Authenticated Encryption) is performed instead
of AEAD (Authenticated Encryption with Associated Data). This function only
supports ciphers that can be used both with and without AAD.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See
[examples in the User's Guide.](new_api.md#example-of-crypto_one_time_aead-6)
""".
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 22.0">>}).
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
aead_tag_len(sm4_gcm) -> 16;
aead_tag_len(sm4_ccm) -> 16;
aead_tag_len(_) ->
    error({badarg, "Not an AEAD cipher"}).



-doc("""
Initializes AEAD cipher.

Similar to 'crypto_one_time_aead/7' but only does the initialization part,
returns a handle that can be used with 'crypto_one_time_aead/4' serveral times.

""").
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 28.0">>}).
-spec crypto_one_time_aead_init(Cipher, Key, TagLength, EncFlag) -> Result
              when Cipher :: cipher_aead(),
                   Key :: iodata(),
                   TagLength :: non_neg_integer(), % or pos_integer() 1..
                   EncFlag :: boolean(),
                   Result :: crypto_state().

crypto_one_time_aead_init(Cipher, Key, Length, Encode) when is_integer(Length) ->
    ?nif_call(aead_cipher_init_nif(alias(Cipher,Key), iolist_to_binary(Key), Length, Encode),
              [Cipher, Key, Length, Encode],
              {}
             ).

-doc("""
Do a complete encrypt or decrypt with an AEAD cipher of the full text.

Similar to 'crypto_one_time_aead/7' but uses the handle from 'crypto_one_time_aead_init/4'.

Appends the tag of the specified 'TagLength' to the end of the encrypted data, when doing encryption.
Strips the tag from the end of 'InText' and verifies it when doing decryption.
""").
-doc(#{group => <<"Cipher API">>,
       since => <<"OTP 28.0">>}).
-spec crypto_one_time_aead(State, IV, InText, AAD) ->
                             Result
                                 when State :: crypto_state(),
                                      IV :: iodata(),
                                      InText :: iodata(),
                                      AAD :: iodata(),
                                      Result :: EncryptResult | DecryptResult,
                                      EncryptResult :: binary(),
                                      DecryptResult :: binary() | error.
crypto_one_time_aead(State, IV, InText, AAD) ->
    ?nif_call(aead_cipher_nif(State, IV, InText, AAD)).


%%%----------------------------------------------------------------
%%% Cipher NIFs

ng_crypto_init_nif(_Cipher, _Key, _IVec, _OptionsMap) -> ?nif_stub.

ng_crypto_update_nif(_State, _Data) -> ?nif_stub.

ng_crypto_final_nif(_State) -> ?nif_stub.

ng_crypto_get_data_nif(_State) -> ?nif_stub.

ng_crypto_one_time_nif(_Cipher, _Key, _IVec, _Data, _OptionsMap) -> ?nif_stub.

aead_cipher_nif(_Type, _Key, _Ivec, _AAD, _In, _TagOrTagLength, _EncFlg) -> ?nif_stub.

aead_cipher_init_nif(_Type, _Key, _TagOrTagLength, _EncFlg) -> ?nif_stub.

aead_cipher_nif(_State, _IV, _InText, _AA) -> ?nif_stub.

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

-doc """
Generate bytes with randomly uniform values 0..255.

Returns the result in a binary with `N` bytes.

Uses a cryptographically secure PRNG seeded and periodically mixed with
operating system provided entropy. By default this is the `RAND_bytes` method
from OpenSSL.

May raise exception `error:low_entropy` in case the random generator failed due
to lack of secure "randomness".
""".
-doc(#{group => <<"Random API">>,
       since => <<"OTP R14B03">>}).
-spec strong_rand_bytes(N::non_neg_integer()) -> binary().
strong_rand_bytes(Bytes) ->
    case strong_rand_bytes_nif(Bytes) of
        false -> erlang:error(low_entropy);
        Bin -> Bin
    end.
strong_rand_bytes_nif(_Bytes) -> ?nif_stub.


-doc """
Create a state object for [random number generation](`m:rand`), in order to
generate cryptographically strong random numbers (based on OpenSSL's
`BN_rand_range`).

Saves the state in the process dictionary before returning it as
well. See also `rand:seed/1` and `rand_seed_s/0`.

When using the state object from this function the `m:rand` functions using it
may raise exception `error:low_entropy` in case the random generator failed due
to lack of secure "randomness".

_Example_

```erlang
_ = crypto:rand_seed(),
_IntegerValue = rand:uniform(42), % [1; 42]
_FloatValue = rand:uniform().     % [0.0; 1.0[
```
""".
-doc(#{group => <<"Random API">>,
       since => <<"OTP 20.0">>}).
-spec rand_seed() -> rand:state().
rand_seed() ->
    rand:seed(rand_seed_s()).

-doc """
Create a state object for [random number generation](`m:rand`), in order to
generate cryptographically strongly random numbers (based on OpenSSL's
`BN_rand_range`). See also `rand:seed_s/1`.

When using the state object from this function the `m:rand` functions using it
may raise exception `error:low_entropy` in case the random generator failed due
to lack of secure "randomness".

> #### Note {: .info }
>
> The state returned from this function cannot be used to get a reproducible
> random sequence as from the other `m:rand` functions, since reproducibility
> does not match cryptographically safe.
>
> The only supported usage is to generate one distinct random sequence from this
> start state.
""".
-doc(#{group => <<"Random API">>,
       since => <<"OTP 20.0">>}).
-spec rand_seed_s() -> rand:state().
rand_seed_s() ->
    rand_seed_alg_s(?MODULE).

-doc """
Create a state object for [random number generation](`m:rand`), in order to
generate cryptographically strong random numbers.

Saves the state in the process dictionary before returning it as well. See also
`rand:seed/1` and `rand_seed_alg_s/1`.

When using the state object from this function the `m:rand` functions using it
may raise exception `error:low_entropy` in case the random generator failed due
to lack of secure "randomness".

_Example_

```erlang
_ = crypto:rand_seed_alg(crypto_cache),
_IntegerValue = rand:uniform(42), % [1; 42]
_FloatValue = rand:uniform().     % [0.0; 1.0[
```
""".
-doc(#{group => <<"Random API">>,
       since => <<"OTP 21.0">>}).
-spec rand_seed_alg(Alg :: atom()) ->
                           {rand:alg_handler(),
                            atom() | rand_cache_seed()}.
rand_seed_alg(Alg) ->
    rand:seed(rand_seed_alg_s(Alg)).

-doc """
Creates a state object for [random number generation](`m:rand`), in order to
generate cryptographically unpredictable random numbers.

Saves the state in the process dictionary before returning it as well. See also
`rand_seed_alg_s/2`.

_Example_

```erlang
_ = crypto:rand_seed_alg(crypto_aes, "my seed"),
IntegerValue = rand:uniform(42), % [1; 42]
FloatValue = rand:uniform(),     % [0.0; 1.0[
_ = crypto:rand_seed_alg(crypto_aes, "my seed"),
IntegerValue = rand:uniform(42), % Same values
FloatValue = rand:uniform().     % again
```
""".
-doc(#{group => <<"Random API">>,
       since => <<"OTP-22.0">>}).
-spec rand_seed_alg(Alg :: atom(), Seed :: term()) ->
                           {rand:alg_handler(),
                            atom() | rand_cache_seed()}.
rand_seed_alg(Alg, Seed) ->
    rand:seed(rand_seed_alg_s(Alg, Seed)).

-define(CRYPTO_CACHE_BITS, 56).
-define(CRYPTO_AES_BITS, 58).

-doc(#{group => <<"Random API">>}).
-doc """
Create a state object for [random number generation](`m:rand`), in order to
generate cryptographically strongly random numbers.

See also `rand:seed_s/1`.

If `Alg` is `crypto` this function behaves exactly like `rand_seed_s/0`.

If `Alg` is `crypto_cache` this function fetches random data with OpenSSL's
`RAND_bytes` and caches it for speed using an internal word size of 56 bits that
makes calculations fast on 64 bit machines.

When using the state object from this function the `m:rand` functions using it
may raise exception `error:low_entropy` in case the random generator failed due
to lack of secure "randomness".

The cache size can be changed from its default value using the
[crypto app's ](crypto_app.md)configuration parameter `rand_cache_size`.

> #### Note {: .info }
>
> The state returned from this function cannot be used to get a reproducible
> random sequence as from the other `m:rand` functions, since reproducibility
> does not match cryptographically safe.
>
> In fact since random data is cached some numbers may get reproduced if you
> try, but this is unpredictable.
>
> The only supported usage is to generate one distinct random sequence from this
> start state.
""".
-doc(#{since => <<"OTP 21.0">>}).
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
-doc """
Create a state object for [random number generation](`m:rand`), in order to
generate cryptographically unpredictable random numbers.

See also `rand_seed_alg/1`.

To get a long period the Xoroshiro928 generator from the `m:rand` module is used
as a counter (with period 2^928 - 1) and the generator states are scrambled
through AES to create 58-bit pseudo random values.

The result should be statistically completely unpredictable random values, since
the scrambling is cryptographically strong and the period is ridiculously long.
But the generated numbers are not to be regarded as cryptographically strong
since there is no re-keying schedule.

- If you need cryptographically strong random numbers use `rand_seed_alg_s/1`
  with `Alg =:= crypto` or `Alg =:= crypto_cache`.
- If you need to be able to repeat the sequence use this function.
- If you do not need the statistical quality of this function, there are faster
  algorithms in the `m:rand` module.

Thanks to the used generator the state object supports the
[`rand:jump/0,1`](`rand:jump/0`) function with distance 2^512.

Numbers are generated in batches and cached for speed reasons. The cache size
can be changed from its default value using the
[crypto app's ](crypto_app.md)configuration parameter `rand_cache_size`.
""".
-doc(#{group => <<"Random API">>,
       since => <<"OTP 22.0">>}).
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

-doc false.
rand_plugin_next(Seed) ->
    {bytes_to_integer(strong_rand_range(1 bsl 64)), Seed}.

-doc false.
rand_plugin_uniform(State) ->
    {strong_rand_float(), State}.

-doc false.
rand_plugin_uniform(Max, State) ->
    {bytes_to_integer(strong_rand_range(Max)) + 1, State}.


-doc false.
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
-doc false.
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
-doc false.
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

-doc false.
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

-doc(#{group => <<"Random API">>}).
-doc """
Generate a random integer number.

The interval is `From =< N < To`. Uses the `crypto` library
pseudo-random number generator. `To` must be larger than `From`.
""".
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


-doc """
Set the seed for PRNG to the given binary.

This calls the RAND_seed function from openssl. Only use this if the system you
are running on does not have enough "randomness" built in. Normally this is when
`strong_rand_bytes/1` raises `error:low_entropy`.
""".
-doc(#{group => <<"Random API">>,
       since => <<"OTP 17.0">>}).
-spec rand_seed(binary()) -> ok.
rand_seed(Seed) when is_binary(Seed) ->
    rand_seed_nif(Seed).

rand_seed_nif(_Seed) -> ?nif_stub.

%%%================================================================
%%%
%%% Sign/verify
%%%
%%%================================================================
-doc "Algorithms for sign and verify.".
-doc(#{group => <<"Public Key Sign and Verify">>}).
-type pk_sign_verify_algs() :: rsa | dss | ecdsa | eddsa .

-doc(#{group => <<"Public Key Sign and Verify">>,
       equiv => rsa_sign_verify_padding()}).
-type pk_sign_verify_opts() :: [ rsa_sign_verify_opt() ] .

-doc(#{group => <<"Public Key Sign and Verify">>,
       equiv => rsa_sign_verify_padding()}).
-type rsa_sign_verify_opt() :: {rsa_padding, rsa_sign_verify_padding()}
                             | {rsa_pss_saltlen, integer()}
                             | {rsa_mgf1_md, sha2()}.

-doc """
Options for sign and verify.

> #### Warning {: .warning }
>
> The RSA options are experimental.
>
> The exact set of options and there syntax _may_ be changed without prior
> notice.
""".
-doc(#{group => <<"Public Key Sign and Verify">>}).
-type rsa_sign_verify_padding() :: rsa_pkcs1_padding | rsa_pkcs1_pss_padding
                                 | rsa_x931_padding | rsa_no_padding
                                   .


%%%----------------------------------------------------------------
%%% Sign

-doc(#{group => <<"Sign/Verify API">>,
       since => <<"OTP R16B01">>}).
-doc(#{equiv => sign/5}).
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


-doc """
Create a digital signature.

The msg is either the binary "cleartext" data to be signed or it is the hashed
value of "cleartext" i.e. the digest (plaintext).

Algorithm `dss` can only be used together with digest type `sha`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See also `public_key:sign/3`.
""".
-doc(#{group => <<"Sign/Verify API">>,
       since => <<"OTP 20.1">>}).
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

-doc(#{equiv => verify/6}).
-doc(#{group => <<"Sign/Verify API">>,
       since => <<"OTP R16B01">>}).
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

-doc """
Verify a digital signature.

The msg is either the binary "cleartext" data to be signed or it is the hashed
value of "cleartext" i.e. the digest (plaintext).

Algorithm `dss` can only be used together with digest type `sha`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See also `public_key:verify/4`.
""".
-doc(#{group => <<"Sign/Verify API">>,
       since => <<"OTP 20.1">>}).
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
-doc "Algorithms for public key encrypt/decrypt. Only RSA is supported.".
-doc(#{group => <<"Public Key Ciphers">>}).
-type pk_encrypt_decrypt_algs() :: rsa .

-doc(#{group => <<"Public Key Ciphers">>,equiv => rsa_padding()}).
-type pk_encrypt_decrypt_opts() ::  [rsa_opt()] | rsa_compat_opts().

-doc """
Those option forms are kept only for compatibility and should not be used in new
code.
""".
-doc(#{group => <<"Public Key Ciphers">>}).
-type rsa_compat_opts() :: [{rsa_pad, rsa_padding()}]
                         | rsa_padding() .

-doc """
Options for public key encrypt/decrypt. Only RSA is supported.

> #### Warning {: .warning }
>
> The RSA options are experimental.
>
> The exact set of options and there syntax _may_ be changed without prior
> notice.
""".
-doc(#{group => <<"Public Key Ciphers">>}).
-type rsa_padding() :: rsa_pkcs1_padding
                     | rsa_pkcs1_oaep_padding
                     | rsa_x931_padding
                     | rsa_no_padding.

-doc(#{group => <<"Public Key Ciphers">>,equiv => rsa_padding()}).
-type rsa_opt() :: {rsa_padding, rsa_padding()}
                 | {signature_md, atom()}
                 | {rsa_mgf1_md, sha}
                 | {rsa_oaep_label, binary()}
                 | {rsa_oaep_md, sha} .

%%%---- Encrypt with public key

-doc """
Encrypt using a public key.

Encrypts the `PlainText` (message digest) using the `PublicKey` and returns the
`CipherText`. This is a low level signature operation used for instance by older
versions of the SSL protocol. See also
[public_key:encrypt_public/2,3](`public_key:encrypt_public/2`)

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use together with rsa_pkcs1_padding.

""".
-doc(#{group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R16B01">>}).
-spec public_encrypt(Algorithm, PlainText, PublicKey, Options) ->
                            CipherText when Algorithm :: pk_encrypt_decrypt_algs(),
                                            PlainText :: binary(),
                                            PublicKey :: rsa_public() | engine_key_ref(),
                                            Options :: pk_encrypt_decrypt_opts(),
                                            CipherText :: binary().
public_encrypt(Algorithm, PlainText, PublicKey, Options) ->
    pkey_crypt(Algorithm, PlainText, PublicKey, Options, false, true).

%%%---- Decrypt with private key

-doc """
Decrypt using a private key.

Decrypts the `CipherText`, encrypted with `public_encrypt/4` (or equivalent
function) using the `PrivateKey`, and returns the plaintext (message digest).
This is a low level signature verification operation used for instance by older
versions of the SSL protocol. See also
[public_key:decrypt_private/2,3](`public_key:decrypt_private/2`)

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.

""".

-doc(#{group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R16B01">>}).
-spec private_decrypt(Algorithm, CipherText, PrivateKey, Options) ->
                             PlainText when Algorithm :: pk_encrypt_decrypt_algs(),
                                            CipherText :: binary(),
                                            PrivateKey :: rsa_private() | engine_key_ref(),
                                            Options :: pk_encrypt_decrypt_opts(),
                                            PlainText :: binary() .
private_decrypt(Algorithm, CipherText, PrivateKey, Options) ->
    pkey_crypt(Algorithm, CipherText,  PrivateKey, Options, true, false).

%%%---- Encrypt with private key

-doc """
Encrypt using a private key.

Encrypts the `PlainText` using the `PrivateKey` and returns the ciphertext. This
is a low level signature operation used for instance by older versions of the
SSL protocol. See also
[public_key:encrypt_private/2,3](`public_key:encrypt_private/2`)

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

Public-key decryption using the private key. See also `crypto:private_decrypt/4`

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.
> For digital signatures use of [`sign/4`](`sign/4`) together
> with [`verify/5`](`verify/5`) is the prefered solution.

""".
-doc(#{group => <<"Legacy RSA Encryption API">>,
        since => <<"OTP R16B01">>}).
-spec private_encrypt(Algorithm, PlainText, PrivateKey, Options) ->
                            CipherText when Algorithm :: pk_encrypt_decrypt_algs(),
                                            PlainText :: binary(),
                                            PrivateKey :: rsa_private() | engine_key_ref(),
                                            Options :: pk_encrypt_decrypt_opts(),
                                            CipherText :: binary().
private_encrypt(Algorithm, PlainText, PrivateKey, Options) ->
    pkey_crypt(Algorithm, PlainText,  PrivateKey, Options, true, true).

%%%---- Decrypt with public key

-doc """
Decrypt using a public key.

Decrypts the `CipherText`, encrypted with `private_encrypt/4`(or equivalent
function) using the `PublicKey`, and returns the plaintext (message digest).
This is a low level signature verification operation used for instance by older
versions of the SSL protocol. See also
[public_key:decrypt_public/2,3](`public_key:decrypt_public/2`)

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.
> For digital signatures use of [`verify/5`](`verify/5`) together
> with [`sign/4`](`sign/4`) is the prefered solution.

""".
-doc(#{group => <<"Legacy RSA Encryption API">>,
       since => <<"OTP R16B01">>}).
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

-doc(#{equiv => generate_key/3}).
-doc(#{group => <<"Key API">>,
       since => <<"OTP R16B01">>}).
-spec generate_key(Type, Params)
                 -> {PublicKey, PrivKeyOut}
                        when Type :: dh | ecdh | eddh | eddsa | rsa | srp,
                             PublicKey :: dh_public() | ecdh_public() | rsa_public() | srp_public(),
                             PrivKeyOut :: dh_private() | ecdh_private() | rsa_private() | {srp_public(),srp_private()},
                             Params :: dh_params() | ecdh_params() | eddsa_params() | rsa_params() | srp_gen_params()
                                       .
generate_key(Type, Params) ->
    generate_key(Type, Params, undefined).

-doc """
Generate a public key.

See also `public_key:generate_key/1`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

> #### Note {: .info }
>
> If the linked version of cryptolib is OpenSSL 3.0
>
> - and the `Type` is `dh` (diffie-hellman)
> - and the parameter `P` (in `t:dh_params/0`) is one of the MODP groups (see
>   [RFC 3526](https://tools.ietf.org/html/rfc3526))
> - and the optional `PrivateKeyBitLength` parameter (in `t:dh_params/0`) is
>   present,
>
> then the optional key length parameter must be at least 224, 256, 302, 352 and
> 400 for group sizes of 2048, 3072, 4096, 6144 and 8192, respectively.
""".
-doc(#{group => <<"Key API">>,
       since => <<"OTP R16B01">>}).
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
        {Private, OldPrivate} ->
            Where = [A == B || A <- Private && B <- OldPrivate],
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


-doc """
Compute the shared secret from the private key and the other party's public
key.

See also `public_key:compute_key/2`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.
""".
-doc(#{group => <<"Key API">>,
       since => <<"OTP R16B01">>}).
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

-doc(#{group => <<"Utility Functions">>}).
-doc """
Perform bit-wise XOR (exclusive or) on the data supplied.

The two byte sequences mus be of equal length.
""".
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

-doc(#{group => <<"Utility Functions">>,
       since => <<"OTP R16B01">>}).
-doc "Compute the function `N^P mod M`.".
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
-doc """
Identifies the key to be used. The format depends on the loaded engine. It is
passed to the `ENGINE_load_(private|public)_key` functions in libcrypto.
""".
-doc(#{group => <<"Types for Engines">>}).
-type key_id()   :: string() | binary() .
-doc "The password of the key stored in an engine.".
-doc(#{group => <<"Types for Engines">>}).
-type password() :: string() | binary() .

-doc(#{group => <<"Types for Engines">>,equiv => engine_ref()}).
-type engine_key_ref() :: #{engine :=   engine_ref(),
                            key_id :=   key_id(),
                            password => password(),
                            term() => term()
                           }.

%%%---- Commands:
-doc "Pre and Post commands for [engine_load/3 and /4](`engine_load/3`).".
-doc(#{group => <<"Types for Engines">>}).
-type engine_cmnd() :: {unicode:chardata(), unicode:chardata()}.

%%----------------------------------------------------------------------
%% Function: engine_get_all_methods/0
%%----------------------------------------------------------------------
-doc(#{group => <<"Types for Engines">>}).
-type engine_method_type() :: engine_method_rsa | engine_method_dsa | engine_method_dh |
                              engine_method_rand | engine_method_ecdh | engine_method_ecdsa |
                              engine_method_ciphers | engine_method_digests | engine_method_store |
                              engine_method_pkey_meths | engine_method_pkey_asn1_meths |
                              engine_method_ec.

-doc "The result of a call to `engine_load/3`.".
-doc(#{group => <<"Types for Engines">>}).
-type engine_ref() :: term().

-doc """
Return a list of all possible engine methods.

May raise exception `error:notsup` in case there is no engine support in the
underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 20.2">>}).
-spec engine_get_all_methods() -> Result when Result :: [engine_method_type()].
engine_get_all_methods() ->
    engine_get_all_methods_nif().

%%----------------------------------------------------------------------
%% Function: engine_load/3
%%----------------------------------------------------------------------
-doc """
Load an OpenSSL engine.

Loads the OpenSSL engine given by `EngineId` if it is available and intialize
it. Returns `ok` and an engine handle, or if the engine can't be loaded an error
tuple is returned.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 20.2">>}).
-spec engine_load(EngineId, PreCmds, PostCmds) ->
                         Result when EngineId::unicode:chardata(),
                                     PreCmds::[engine_cmnd()],
                                     PostCmds::[engine_cmnd()],
                                     Result :: {ok, Engine::engine_ref()} | {error, Reason::term()}.
engine_load(EngineId, PreCmds, PostCmds) when is_list(PreCmds),
                                              is_list(PostCmds) ->
    try
        ok = engine_load_dynamic_nif(),
        case engine_by_id_nif(ensure_bin_chardata(EngineId)) of
            {ok, Engine} ->
                engine_load_1(Engine, PreCmds, PostCmds);
            {error, Error1} ->
                {error, Error1}
        end
    catch
        throw:Error2 ->
            Error2
    end.

-doc false.
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
-doc """
Unload an OpenSSL engine.

Unloads the OpenSSL engine given by `Engine`. An error tuple is returned if the
engine can't be unloaded.

The function raises a `error:badarg` if the parameter is in wrong format. It may
also raise the exception `error:notsup` in case there is no engine support in
the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 20.2">>}).
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

-doc false.
-spec engine_unload(Engine, EngineMethods) -> Result when Engine :: engine_ref(),
					      EngineMethods :: [engine_method_type()],
					      Result :: ok | {error, Reason::term()}.
engine_unload(Engine, _EngineMethods) ->
    engine_unload(Engine).

%%----------------------------------------------------------------------
%% Function: engine_by_id/1
%%----------------------------------------------------------------------
-doc """
Get a reference to an already loaded engine with `EngineId`. An error tuple is
returned if the engine can't be unloaded.

The function raises a `error:badarg` if the parameter is in wrong format. It may
also raise the exception `error:notsup` in case there is no engine support in
the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 21.0.6">>}).
-spec engine_by_id(EngineId) -> Result when EngineId :: unicode:chardata(),
                                            Result :: {ok, Engine::engine_ref()} | {error, Reason::term()} .
engine_by_id(EngineId) ->
    try
        engine_by_id_nif(ensure_bin_chardata(EngineId))
    catch
       throw:Error ->
          Error
    end.

%%----------------------------------------------------------------------
%% Function: engine_add/1
%%----------------------------------------------------------------------
-doc """
Add the engine to OpenSSL's internal list.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 21.0.6">>}).
-spec engine_add(Engine) -> Result when Engine :: engine_ref(),
                                        Result ::  ok | {error, Reason::term()} .
engine_add(Engine) ->
    engine_add_nif(Engine).

%%----------------------------------------------------------------------
%% Function: engine_remove/1
%%----------------------------------------------------------------------
-doc """
Remove the engine from OpenSSL's internal list.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 21.0.6">>}).
-spec engine_remove(Engine) -> Result when Engine :: engine_ref(),
                                           Result ::  ok | {error, Reason::term()} .
engine_remove(Engine) ->
    engine_remove_nif(Engine).

%%----------------------------------------------------------------------
%% Function: engine_register/2
%%----------------------------------------------------------------------
-doc """
Register engine to handle some type of methods, for example
engine_method_digests.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 25.1">>}).
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
-doc """
Unregister engine so it don't handle some type of methods.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 25.1">>}).
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
-doc """
Return the ID for the engine, or an empty binary if there is no id set.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 21.0.6">>}).
-spec engine_get_id(Engine) -> EngineId when Engine :: engine_ref(),
                                             EngineId :: unicode:chardata().
engine_get_id(Engine) ->
    engine_get_id_nif(Engine).

%%----------------------------------------------------------------------
%% Function: engine_get_name/1
%%----------------------------------------------------------------------
-doc """
Return the name (eg a description) for the engine, or an empty binary if there
is no name set.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 21.0.6">>}).
-spec engine_get_name(Engine) -> EngineName when Engine :: engine_ref(),
                                                 EngineName :: unicode:chardata().
engine_get_name(Engine) ->
    engine_get_name_nif(Engine).

%%----------------------------------------------------------------------
%% Function: engine_list/0
%%----------------------------------------------------------------------
-doc """
List the id's of all engines in OpenSSL's internal list.

It may also raise the exception `error:notsup` in case there is no engine
support in the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.

May raise exception `error:notsup` in case engine functionality is not supported
by the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 20.2">>}).
-spec engine_list() -> Result when Result :: [EngineId::unicode:chardata()].
engine_list() ->
    case engine_get_first_nif() of
        {ok, <<>>} ->
            [];
        {ok, Engine} ->
            case engine_get_id_nif(Engine) of
                <<>> ->
                    engine_list(Engine, []);
                EngineId ->
                    engine_list(Engine, [EngineId])
            end
    end.

engine_list(Engine0, IdList) ->
    case engine_get_next_nif(Engine0) of
        {ok, <<>>} ->
            lists:reverse(IdList);
        {ok, Engine1} ->
            case engine_get_id_nif(Engine1) of
                <<>> ->
                    engine_list(Engine1, IdList);
                EngineId ->
                    engine_list(Engine1, [EngineId |IdList])
            end
    end.

%%----------------------------------------------------------------------
%% Function: engine_ctrl_cmd_string/3
%%----------------------------------------------------------------------
-doc """
Send ctrl commands to an OpenSSL engine.

This function is the same as calling `engine_ctrl_cmd_string/4` with `Optional`
set to `false`.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 20.2">>}).
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
-doc """
Send ctrl commands to an OpenSSL engine.

`Optional` is a
boolean argument that can relax the semantics of the function. If set to `true`
it will only return failure if the ENGINE supported the given command name but
failed while executing it, if the ENGINE doesn't support the command name it
will simply return success without doing anything. In this case we assume the
user is only supplying commands specific to the given ENGINE so we set this to
`false`.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 20.2">>}).
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
        {error, Error} ->
            {error, Error}
    end.

%%----------------------------------------------------------------------
%% Function: ensure_engine_loaded/2
%% Special version of load that only uses dynamic engine to load
%%----------------------------------------------------------------------
-doc """
Load a dynamic engine if not already done.

Loada the engine given by `EngineId` and the path to the dynamic library
implementing the engine. An error tuple is returned if the engine can't be
loaded.

This function differs from the normal engine_load in the sense that it also add
the engine id to OpenSSL's internal engine list. The difference between the
first call and the following is that the first loads the engine with the
dynamical engine and the following calls fetch it from the OpenSSL's engine
list. All references that is returned are equal.

Use [`engine_unload/1`](`engine_unload/1`) function to remove the references.
But remember that [`engine_unload/1`](`engine_unload/1`) just removes the
references to the engine and not the tag in OpenSSL's engine list. That has to
be done with the [`engine_remove/1`](`engine_remove/1`) function when needed
(just called once, from any of the references you got).

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 21.0.6">>}).
-spec ensure_engine_loaded(EngineId, LibPath) -> Result when EngineId :: unicode:chardata(),
						 LibPath :: unicode:chardata(),
						 Result :: {ok, Engine::engine_ref()} |
	{error, Reason::term()}.
ensure_engine_loaded(EngineId, LibPath) ->
    case ensure_engine_loaded_nif(ensure_bin_chardata(EngineId),
                                  ensure_bin_chardata(LibPath)) of
        {ok, Engine} ->
            {ok, Engine};
        {error, Error1} ->
            {error, Error1}
    end.

%%----------------------------------------------------------------------
%% Function: ensure_engine_loaded/3
%% Special version of load that only uses dynamic engine to load
%%----------------------------------------------------------------------
-doc false.
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
-doc false.
-spec ensure_engine_unloaded(Engine) -> Result when Engine :: engine_ref(),
                                                    Result :: ok | {error, Reason::term()}.
ensure_engine_unloaded(Engine) ->
    engine_unload(Engine).

%%----------------------------------------------------------------------
%% Function: ensure_engine_unloaded/2
%%----------------------------------------------------------------------
-doc false.
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
    {FipsMode,AppLoaded} =
        case application:get_env(crypto, fips_mode) of
            {ok, true} -> {true, loaded};
            {ok, _} -> {false, loaded};
            undefined ->
                %% We assume application crypto has a default value for fips_mode.
                %% If undefined the application has not been loaded.
                {false, unloaded}
        end,
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
	ok ->
            warn_app_not_loaded_maybe(AppLoaded),
            ok;
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

warn_app_not_loaded_maybe(loaded) ->
    ok;
warn_app_not_loaded_maybe(unloaded) ->
    %% For backward compatible reasons we allow application crypto
    %% not being loaded.
    case info_fips() of
        not_enabled ->
            logger:warning("Module 'crypto' loaded without application 'crypto' being loaded.\n"
                           "Without application config 'fips_mode' loaded, FIPS mode is disabled by default.");
        _ ->
            ok
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

-doc(#{group => <<"Utility Functions">>,
       since => <<"OTP 17.0">>}).
-doc "Return all supported named elliptic curves.".
-spec ec_curves() -> [EllipticCurve] when EllipticCurve :: ec_named_curve()
                                                         | edwards_curve_dh()
                                                         | edwards_curve_ed() .

ec_curves() ->
    crypto_ec_curves:curves().

-doc(#{group => <<"Utility Functions">>,
       since => <<"OTP 17.0">>}).
-doc "Return the defining parameters of a elliptic curve.".
-spec ec_curve(CurveName) -> ExplicitCurve when CurveName :: ec_named_curve(),
                                                ExplicitCurve :: ec_explicit_curve() .
ec_curve(X) ->
    crypto_ec_curves:curve(X).


-doc """
Fetch public key from a private key stored in an Engine.

The key must be of the type indicated by the Type parameter.
""".
-doc(#{group => <<"Engine API">>,since => <<"OTP 20.2">>}).
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
            try
                crypto_ec_curves:curve_with_name(CurveName)
            catch
                _:_ ->
                    {undefined, CurveName}
            end
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

-doc """
Compare two binaries in constant time, such as results of HMAC computations.

Returns true if the binaries are identical, false if they are of the same length
but not identical. The function raises an `error:badarg` exception if the
binaries are of different size.
""".
-doc(#{group => <<"Utility Functions">>, since => <<"OTP 25.0">>}).
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

-doc "Convert binary representation, of an integer, to an Erlang integer.".
-doc(#{group => <<"Utility Functions">>,
       since => <<"OTP R16B01">>}).
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

-doc false.
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

-doc false.
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

-doc false.
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
