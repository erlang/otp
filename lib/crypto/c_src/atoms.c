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

ERL_NIF_TERM atom_hmac;
ERL_NIF_TERM atom_cmac;
ERL_NIF_TERM atom_poly1305;

ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_none;
ERL_NIF_TERM atom_notsup;
ERL_NIF_TERM atom_badarg;
ERL_NIF_TERM atom_digest;
#ifdef FIPS_SUPPORT
ERL_NIF_TERM atom_enabled;
ERL_NIF_TERM atom_not_enabled;
#else
ERL_NIF_TERM atom_not_supported;
#endif

ERL_NIF_TERM atom_type;
ERL_NIF_TERM atom_size;
ERL_NIF_TERM atom_block_size;
ERL_NIF_TERM atom_key_length;
ERL_NIF_TERM atom_iv_length;
ERL_NIF_TERM atom_mode;
ERL_NIF_TERM atom_ecb_mode;
ERL_NIF_TERM atom_cbc_mode;
ERL_NIF_TERM atom_cfb_mode;
ERL_NIF_TERM atom_ofb_mode;
ERL_NIF_TERM atom_ctr_mode;
ERL_NIF_TERM atom_gcm_mode;
ERL_NIF_TERM atom_ccm_mode;
ERL_NIF_TERM atom_xts_mode;
ERL_NIF_TERM atom_wrap_mode;
ERL_NIF_TERM atom_ocb_mode;
ERL_NIF_TERM atom_stream_cipher;

#if defined(HAVE_EC)
ERL_NIF_TERM atom_prime_field;
ERL_NIF_TERM atom_characteristic_two_field;
ERL_NIF_TERM atom_tpbasis;
ERL_NIF_TERM atom_ppbasis;
ERL_NIF_TERM atom_onbasis;
#endif

ERL_NIF_TERM atom_aes_cfb8;
ERL_NIF_TERM atom_aes_cfb128;
ERL_NIF_TERM atom_aes_ige256;
#ifdef HAVE_GCM
ERL_NIF_TERM atom_aes_gcm;
#endif
#ifdef HAVE_CCM
ERL_NIF_TERM atom_aes_ccm;
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

#ifdef HAVE_BLAKE2
ERL_NIF_TERM atom_blake2b;
ERL_NIF_TERM atom_blake2s;
#endif

#ifdef HAS_ENGINE_SUPPORT

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

    atom_hmac = enif_make_atom(env,"hmac");
    atom_cmac = enif_make_atom(env,"cmac");
    atom_poly1305 = enif_make_atom(env,"poly1305");

    atom_ok = enif_make_atom(env,"ok");
    atom_none = enif_make_atom(env,"none");
    atom_notsup = enif_make_atom(env,"notsup");
    atom_badarg = enif_make_atom(env,"badarg");
    atom_digest = enif_make_atom(env,"digest");

    atom_type = enif_make_atom(env,"type");
    atom_size = enif_make_atom(env,"size");
    atom_block_size = enif_make_atom(env,"block_size");
    atom_key_length = enif_make_atom(env,"key_length");
    atom_iv_length = enif_make_atom(env,"iv_length");
    atom_mode = enif_make_atom(env,"mode");
    atom_ecb_mode = enif_make_atom(env,"ecb_mode");
    atom_cbc_mode = enif_make_atom(env,"cbc_mode");
    atom_cfb_mode = enif_make_atom(env,"cfb_mode");
    atom_ofb_mode = enif_make_atom(env,"ofb_mode");
    atom_ctr_mode = enif_make_atom(env,"ctr_mode");
    atom_gcm_mode = enif_make_atom(env,"gcm_mode");
    atom_ccm_mode = enif_make_atom(env,"ccm_mode");
    atom_xts_mode = enif_make_atom(env,"xts_mode");
    atom_wrap_mode = enif_make_atom(env,"wrap_mode");
    atom_ocb_mode = enif_make_atom(env,"ocb_mode");
    atom_stream_cipher = enif_make_atom(env,"stream_cipher");

#if defined(HAVE_EC)
    atom_prime_field = enif_make_atom(env,"prime_field");
    atom_characteristic_two_field = enif_make_atom(env,"characteristic_two_field");
    atom_tpbasis = enif_make_atom(env,"tpbasis");
    atom_ppbasis = enif_make_atom(env,"ppbasis");
    atom_onbasis = enif_make_atom(env,"onbasis");
#endif

    atom_aes_cfb8 = enif_make_atom(env, "aes_cfb8");
    atom_aes_cfb128 = enif_make_atom(env, "aes_cfb128");
    atom_aes_ige256 = enif_make_atom(env, "aes_ige256");
#ifdef HAVE_GCM
    atom_aes_gcm = enif_make_atom(env, "aes_gcm");
#endif
#ifdef HAVE_CCM
    atom_aes_ccm = enif_make_atom(env, "aes_ccm");
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

#ifdef HAS_ENGINE_SUPPORT

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
