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

#ifndef E_ATOMS_H__
#define E_ATOMS_H__ 1

#include <erl_nif.h>
#include "openssl_config.h"

extern ERL_NIF_TERM atom_true;
extern ERL_NIF_TERM atom_false;
extern ERL_NIF_TERM atom_sha;
extern ERL_NIF_TERM atom_error;
extern ERL_NIF_TERM atom_rsa_pkcs1_padding;
extern ERL_NIF_TERM atom_rsa_pkcs1_oaep_padding;
extern ERL_NIF_TERM atom_rsa_no_padding;
extern ERL_NIF_TERM atom_signature_md;
extern ERL_NIF_TERM atom_undefined;

extern ERL_NIF_TERM atom_hmac;
extern ERL_NIF_TERM atom_cmac;
extern ERL_NIF_TERM atom_poly1305;

extern ERL_NIF_TERM atom_ok;
extern ERL_NIF_TERM atom_none;
extern ERL_NIF_TERM atom_notsup;
extern ERL_NIF_TERM atom_badarg;
extern ERL_NIF_TERM atom_digest;
#ifdef FIPS_SUPPORT
extern ERL_NIF_TERM atom_enabled;
extern ERL_NIF_TERM atom_not_enabled;
#else
extern ERL_NIF_TERM atom_not_supported;
#endif

extern ERL_NIF_TERM atom_type;
extern ERL_NIF_TERM atom_size;
extern ERL_NIF_TERM atom_block_size;
extern ERL_NIF_TERM atom_key_length;
extern ERL_NIF_TERM atom_iv_length;
extern ERL_NIF_TERM atom_mode;
extern ERL_NIF_TERM atom_ecb_mode;
extern ERL_NIF_TERM atom_cbc_mode;
extern ERL_NIF_TERM atom_cfb_mode;
extern ERL_NIF_TERM atom_ofb_mode;
extern ERL_NIF_TERM atom_ctr_mode;
extern ERL_NIF_TERM atom_gcm_mode;
extern ERL_NIF_TERM atom_ccm_mode;
extern ERL_NIF_TERM atom_xts_mode;
extern ERL_NIF_TERM atom_wrap_mode;
extern ERL_NIF_TERM atom_ocb_mode;
extern ERL_NIF_TERM atom_stream_cipher;

#if defined(HAVE_EC)
extern ERL_NIF_TERM atom_prime_field;
extern ERL_NIF_TERM atom_characteristic_two_field;
extern ERL_NIF_TERM atom_tpbasis;
extern ERL_NIF_TERM atom_ppbasis;
extern ERL_NIF_TERM atom_onbasis;
#endif

extern ERL_NIF_TERM atom_aes_cfb8;
extern ERL_NIF_TERM atom_aes_cfb128;
extern ERL_NIF_TERM atom_aes_ige256;
#ifdef HAVE_GCM
extern ERL_NIF_TERM atom_aes_gcm;
#endif
#ifdef HAVE_CCM
extern ERL_NIF_TERM atom_aes_ccm;
#endif

extern ERL_NIF_TERM atom_rsa;
extern ERL_NIF_TERM atom_dss;
extern ERL_NIF_TERM atom_ecdsa;

#ifdef HAVE_ED_CURVE_DH
extern ERL_NIF_TERM atom_x25519;
extern ERL_NIF_TERM atom_x448;
#endif

extern ERL_NIF_TERM atom_eddsa;
#ifdef HAVE_EDDSA
extern ERL_NIF_TERM atom_ed25519;
extern ERL_NIF_TERM atom_ed448;
#endif

extern ERL_NIF_TERM atom_rsa_mgf1_md;
extern ERL_NIF_TERM atom_rsa_oaep_label;
extern ERL_NIF_TERM atom_rsa_oaep_md;
extern ERL_NIF_TERM atom_rsa_pad; /* backwards compatibility */
extern ERL_NIF_TERM atom_rsa_padding;
extern ERL_NIF_TERM atom_rsa_pkcs1_pss_padding;
#ifdef HAVE_RSA_SSLV23_PADDING
extern ERL_NIF_TERM atom_rsa_sslv23_padding;
#endif
extern ERL_NIF_TERM atom_rsa_x931_padding;
extern ERL_NIF_TERM atom_rsa_pss_saltlen;

#ifdef HAS_ENGINE_SUPPORT

extern ERL_NIF_TERM atom_engine_method_rsa;
extern ERL_NIF_TERM atom_engine_method_dsa;
extern ERL_NIF_TERM atom_engine_method_dh;
extern ERL_NIF_TERM atom_engine_method_rand;
extern ERL_NIF_TERM atom_engine_method_ecdh;
extern ERL_NIF_TERM atom_engine_method_ecdsa;
extern ERL_NIF_TERM atom_engine_method_ciphers;
extern ERL_NIF_TERM atom_engine_method_digests;
extern ERL_NIF_TERM atom_engine_method_store;
extern ERL_NIF_TERM atom_engine_method_pkey_meths;
extern ERL_NIF_TERM atom_engine_method_pkey_asn1_meths;
extern ERL_NIF_TERM atom_engine_method_ec;

extern ERL_NIF_TERM atom_engine;
extern ERL_NIF_TERM atom_key_id;
extern ERL_NIF_TERM atom_password;
#endif

int init_atoms(ErlNifEnv *env, const ERL_NIF_TERM fips_mode, const ERL_NIF_TERM load_info);

#endif /* E_ATOMS_H__ */
