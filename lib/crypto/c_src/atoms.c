/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2018. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#include "atoms.h"

ERL_NIF_TERM atom_true;
ERL_NIF_TERM atom_false;
ERL_NIF_TERM atom_sha;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_rsa_pkcs1_padding;
ERL_NIF_TERM atom_rsa_pkcs1_oaep_padding;
ERL_NIF_TERM atom_rsa_no_padding;
ERL_NIF_TERM atom_signature_md;
ERL_NIF_TERM atom_undefined;

ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_not_prime;
ERL_NIF_TERM atom_not_strong_prime;
ERL_NIF_TERM atom_unable_to_check_generator;
ERL_NIF_TERM atom_not_suitable_generator;
ERL_NIF_TERM atom_check_failed;
ERL_NIF_TERM atom_unknown;
ERL_NIF_TERM atom_none;
ERL_NIF_TERM atom_notsup;
ERL_NIF_TERM atom_digest;
#ifdef FIPS_SUPPORT
ERL_NIF_TERM atom_enabled;
ERL_NIF_TERM atom_not_enabled;
#else
ERL_NIF_TERM atom_not_supported;
#endif

#if defined(HAVE_EC)
ERL_NIF_TERM atom_ec;
ERL_NIF_TERM atom_prime_field;
ERL_NIF_TERM atom_characteristic_two_field;
ERL_NIF_TERM atom_tpbasis;
ERL_NIF_TERM atom_ppbasis;
ERL_NIF_TERM atom_onbasis;
#endif

ERL_NIF_TERM atom_aes_cfb8;
ERL_NIF_TERM atom_aes_cfb128;
#ifdef HAVE_GCM
ERL_NIF_TERM atom_aes_gcm;
#endif
#ifdef HAVE_CCM
ERL_NIF_TERM atom_aes_ccm;
#endif
#ifdef HAVE_CHACHA20_POLY1305
ERL_NIF_TERM atom_chacha20_poly1305;
#endif
#ifdef HAVE_ECB_IVEC_BUG
ERL_NIF_TERM atom_aes_ecb;
ERL_NIF_TERM atom_des_ecb;
ERL_NIF_TERM atom_blowfish_ecb;
#endif

ERL_NIF_TERM atom_rsa;
ERL_NIF_TERM atom_dss;
ERL_NIF_TERM atom_ecdsa;

#ifdef HAVE_ED_CURVE_DH
ERL_NIF_TERM atom_x25519;
ERL_NIF_TERM atom_x448;
#endif

ERL_NIF_TERM atom_eddsa;
#ifdef HAVE_EDDSA
ERL_NIF_TERM atom_ed25519;
ERL_NIF_TERM atom_ed448;
#endif

ERL_NIF_TERM atom_rsa_mgf1_md;
ERL_NIF_TERM atom_rsa_oaep_label;
ERL_NIF_TERM atom_rsa_oaep_md;
ERL_NIF_TERM atom_rsa_pad; /* backwards compatibility */
ERL_NIF_TERM atom_rsa_padding;
ERL_NIF_TERM atom_rsa_pkcs1_pss_padding;
#ifdef HAVE_RSA_SSLV23_PADDING
ERL_NIF_TERM atom_rsa_sslv23_padding;
#endif
ERL_NIF_TERM atom_rsa_x931_padding;
ERL_NIF_TERM atom_rsa_pss_saltlen;
ERL_NIF_TERM atom_sha224;
ERL_NIF_TERM atom_sha256;
ERL_NIF_TERM atom_sha384;
ERL_NIF_TERM atom_sha512;
ERL_NIF_TERM atom_sha3_224;
ERL_NIF_TERM atom_sha3_256;
ERL_NIF_TERM atom_sha3_384;
ERL_NIF_TERM atom_sha3_512;
ERL_NIF_TERM atom_md5;
ERL_NIF_TERM atom_ripemd160;

#ifdef HAVE_BLAKE2
ERL_NIF_TERM atom_blake2b;
ERL_NIF_TERM atom_blake2s;
#endif

#ifdef HAS_ENGINE_SUPPORT
ERL_NIF_TERM atom_bad_engine_method;
ERL_NIF_TERM atom_bad_engine_id;
ERL_NIF_TERM atom_ctrl_cmd_failed;
ERL_NIF_TERM atom_engine_init_failed;
ERL_NIF_TERM atom_register_engine_failed;
ERL_NIF_TERM atom_add_engine_failed;
ERL_NIF_TERM atom_remove_engine_failed;
ERL_NIF_TERM atom_engine_method_not_supported;

ERL_NIF_TERM atom_engine_method_rsa;
ERL_NIF_TERM atom_engine_method_dsa;
ERL_NIF_TERM atom_engine_method_dh;
ERL_NIF_TERM atom_engine_method_rand;
ERL_NIF_TERM atom_engine_method_ecdh;
ERL_NIF_TERM atom_engine_method_ecdsa;
ERL_NIF_TERM atom_engine_method_ciphers;
ERL_NIF_TERM atom_engine_method_digests;
ERL_NIF_TERM atom_engine_method_store;
ERL_NIF_TERM atom_engine_method_pkey_meths;
ERL_NIF_TERM atom_engine_method_pkey_asn1_meths;
ERL_NIF_TERM atom_engine_method_ec;

ERL_NIF_TERM atom_engine;
ERL_NIF_TERM atom_key_id;
ERL_NIF_TERM atom_password;
#endif

int init_atoms(ErlNifEnv *env, const ERL_NIF_TERM fips_mode, const ERL_NIF_TERM load_info) {
    atom_true  = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    /* Enter FIPS mode */
    if (fips_mode == atom_true) {
#ifdef FIPS_SUPPORT
        if (!FIPS_mode_set(1)) {
#else
        {
#endif
            PRINTF_ERR0("CRYPTO: Could not setup FIPS mode");
            return 0;
        }
    } else if (fips_mode != atom_false) {
        PRINTF_ERR1("CRYPTO: Invalid load_info '%T'", load_info);
        return 0;
    }

    atom_sha = enif_make_atom(env,"sha");
    atom_error = enif_make_atom(env,"error");
    atom_rsa_pkcs1_padding = enif_make_atom(env,"rsa_pkcs1_padding");
    atom_rsa_pkcs1_oaep_padding = enif_make_atom(env,"rsa_pkcs1_oaep_padding");
    atom_rsa_no_padding = enif_make_atom(env,"rsa_no_padding");
    atom_signature_md = enif_make_atom(env,"signature_md");
    atom_undefined = enif_make_atom(env,"undefined");
    atom_ok = enif_make_atom(env,"ok");
    atom_not_prime = enif_make_atom(env,"not_prime");
    atom_not_strong_prime = enif_make_atom(env,"not_strong_prime");
    atom_unable_to_check_generator = enif_make_atom(env,"unable_to_check_generator");
    atom_not_suitable_generator = enif_make_atom(env,"not_suitable_generator");
    atom_check_failed = enif_make_atom(env,"check_failed");
    atom_unknown = enif_make_atom(env,"unknown");
    atom_none = enif_make_atom(env,"none");
    atom_notsup = enif_make_atom(env,"notsup");
    atom_digest = enif_make_atom(env,"digest");

#if defined(HAVE_EC)
    atom_ec = enif_make_atom(env,"ec");
    atom_prime_field = enif_make_atom(env,"prime_field");
    atom_characteristic_two_field = enif_make_atom(env,"characteristic_two_field");
    atom_tpbasis = enif_make_atom(env,"tpbasis");
    atom_ppbasis = enif_make_atom(env,"ppbasis");
    atom_onbasis = enif_make_atom(env,"onbasis");
#endif

    atom_aes_cfb8 = enif_make_atom(env, "aes_cfb8");
    atom_aes_cfb128 = enif_make_atom(env, "aes_cfb128");
#ifdef HAVE_GCM
    atom_aes_gcm = enif_make_atom(env, "aes_gcm");
#endif
#ifdef HAVE_CCM
    atom_aes_ccm = enif_make_atom(env, "aes_ccm");
#endif
#ifdef HAVE_CHACHA20_POLY1305
    atom_chacha20_poly1305 = enif_make_atom(env,"chacha20_poly1305");
#endif
#ifdef HAVE_ECB_IVEC_BUG
    atom_aes_ecb = enif_make_atom(env, "aes_ecb");
    atom_des_ecb = enif_make_atom(env, "des_ecb");
    atom_blowfish_ecb = enif_make_atom(env, "blowfish_ecb");
#endif

#ifdef FIPS_SUPPORT
    atom_enabled = enif_make_atom(env,"enabled");
    atom_not_enabled = enif_make_atom(env,"not_enabled");
#else
    atom_not_supported = enif_make_atom(env,"not_supported");
#endif
    atom_rsa = enif_make_atom(env,"rsa");
    atom_dss = enif_make_atom(env,"dss");
    atom_ecdsa = enif_make_atom(env,"ecdsa");
#ifdef HAVE_ED_CURVE_DH
    atom_x25519 = enif_make_atom(env,"x25519");
    atom_x448 = enif_make_atom(env,"x448");
#endif
    atom_eddsa = enif_make_atom(env,"eddsa");
#ifdef HAVE_EDDSA
    atom_ed25519 = enif_make_atom(env,"ed25519");
    atom_ed448 = enif_make_atom(env,"ed448");
#endif
    atom_rsa_mgf1_md = enif_make_atom(env,"rsa_mgf1_md");
    atom_rsa_oaep_label = enif_make_atom(env,"rsa_oaep_label");
    atom_rsa_oaep_md = enif_make_atom(env,"rsa_oaep_md");
    atom_rsa_pad = enif_make_atom(env,"rsa_pad"); /* backwards compatibility */
    atom_rsa_padding = enif_make_atom(env,"rsa_padding");
    atom_rsa_pkcs1_pss_padding = enif_make_atom(env,"rsa_pkcs1_pss_padding");
#ifdef HAVE_RSA_SSLV23_PADDING
    atom_rsa_sslv23_padding = enif_make_atom(env,"rsa_sslv23_padding");
#endif
    atom_rsa_x931_padding = enif_make_atom(env,"rsa_x931_padding");
    atom_rsa_pss_saltlen = enif_make_atom(env,"rsa_pss_saltlen");
    atom_sha224 = enif_make_atom(env,"sha224");
    atom_sha256 = enif_make_atom(env,"sha256");
    atom_sha384 = enif_make_atom(env,"sha384");
    atom_sha512 = enif_make_atom(env,"sha512");
    atom_sha3_224 = enif_make_atom(env,"sha3_224");
    atom_sha3_256 = enif_make_atom(env,"sha3_256");
    atom_sha3_384 = enif_make_atom(env,"sha3_384");
    atom_sha3_512 = enif_make_atom(env,"sha3_512");
    atom_md5 = enif_make_atom(env,"md5");
    atom_ripemd160 = enif_make_atom(env,"ripemd160");
#ifdef HAVE_BLAKE2
    atom_blake2b = enif_make_atom(env,"blake2b");
    atom_blake2s = enif_make_atom(env,"blake2s");
#endif

#ifdef HAS_ENGINE_SUPPORT
    atom_bad_engine_method = enif_make_atom(env,"bad_engine_method");
    atom_bad_engine_id = enif_make_atom(env,"bad_engine_id");
    atom_ctrl_cmd_failed = enif_make_atom(env,"ctrl_cmd_failed");
    atom_engine_init_failed = enif_make_atom(env,"engine_init_failed");
    atom_engine_method_not_supported = enif_make_atom(env,"engine_method_not_supported");
    atom_add_engine_failed = enif_make_atom(env,"add_engine_failed");
    atom_remove_engine_failed = enif_make_atom(env,"remove_engine_failed");

    atom_engine_method_rsa = enif_make_atom(env,"engine_method_rsa");
    atom_engine_method_dsa = enif_make_atom(env,"engine_method_dsa");
    atom_engine_method_dh = enif_make_atom(env,"engine_method_dh");
    atom_engine_method_rand = enif_make_atom(env,"engine_method_rand");
    atom_engine_method_ecdh = enif_make_atom(env,"engine_method_ecdh");
    atom_engine_method_ecdsa = enif_make_atom(env,"engine_method_ecdsa");
    atom_engine_method_store = enif_make_atom(env,"engine_method_store");
    atom_engine_method_ciphers = enif_make_atom(env,"engine_method_ciphers");
    atom_engine_method_digests = enif_make_atom(env,"engine_method_digests");
    atom_engine_method_pkey_meths = enif_make_atom(env,"engine_method_pkey_meths");
    atom_engine_method_pkey_asn1_meths = enif_make_atom(env,"engine_method_pkey_asn1_meths");
    atom_engine_method_ec = enif_make_atom(env,"engine_method_ec");

    atom_engine = enif_make_atom(env,"engine");
    atom_key_id = enif_make_atom(env,"key_id");
    atom_password = enif_make_atom(env,"password");
#endif

    return 1;
}
