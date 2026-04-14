/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

#include "common.h"
#include "algorithms.h"
#include "cipher.h"
#include "mac.h"

#include <vector>

#ifdef HAS_3_0_API
#include "digest.h"
#include "pkey.h"
#endif

#ifdef HAS_3_0_API
#else
// Hash algorithms only exist pre OpenSSL 3.0
struct v1_hash_type_t {
    const char *str;
    ERL_NIF_TERM atom = ERL_CRYPTO_BAD_ATOM_VALUE;

    struct flags_t {
        bool fips_forbidden: 1;
    };
    flags_t flags = {};

    explicit v1_hash_type_t(const char *str_, ERL_NIF_TERM atom_ = ERL_CRYPTO_BAD_ATOM_VALUE, const flags_t flags_ = {}) :
        str(str_), atom(atom_), flags(flags_) {}
};
static std::vector<v1_hash_type_t> algo_hash;
void init_hash_types(ErlNifEnv* env);
#endif

struct pubkey_type_t {
    const char *str;
    ERL_NIF_TERM atom = ERL_CRYPTO_BAD_ATOM_VALUE;

    struct flags_t {
        bool fips_forbidden: 1;
    };
    flags_t flags = {};

    pubkey_type_t(const char *str_, ERL_NIF_TERM atom_, const flags_t flags_)
        : str(str_), atom(atom_), flags(flags_) {}
};

static std::vector<pubkey_type_t> algo_pubkey;
void init_pubkey_types(ErlNifEnv* env);

struct curve_type_t {
    const char *str;
    ERL_NIF_TERM atom = ERL_CRYPTO_BAD_ATOM_VALUE;
    struct flags_t {
        bool fips_forbidden: 1;
    };
    flags_t flags = {};
    curve_type_t(const char *str_, ERL_NIF_TERM atom_, const flags_t flags_)
        : str(str_), atom(atom_), flags(flags_) {}
};

static std::vector<curve_type_t> algo_curve[2];
static ErlNifMutex* mtx_init_curve_types;
static void lazy_init_curves(ErlNifEnv* env, bool fips);

struct rsaopt_type_t {
    const char *str;
    ERL_NIF_TERM atom = ERL_CRYPTO_BAD_ATOM_VALUE;
    struct flags_t {
        bool fips_forbidden: 1;
    };
    flags_t flags = {};
    rsaopt_type_t(const char *str_, ERL_NIF_TERM atom_, const flags_t flags_)
        : str(str_), atom(atom_), flags(flags_) {}
};

static std::vector<rsaopt_type_t> algo_rsa_opts;
void init_rsa_opts_types(ErlNifEnv* env);


void init_algorithms_types(ErlNifEnv* env)
{
#ifdef HAS_3_0_API
#else
    init_hash_types(env);
#endif
    init_pubkey_types(env);
    init_rsa_opts_types(env);
    /* ciphers and macs are initiated statically */
}


int create_curve_mutex()
{
    if (!mtx_init_curve_types) {
        mtx_init_curve_types =  enif_mutex_create(const_cast<char*>("init_curve_types"));
    }
    return !!mtx_init_curve_types;
}

void destroy_curve_mutex()
{
    if (mtx_init_curve_types) {
        enif_mutex_destroy(mtx_init_curve_types);
        mtx_init_curve_types = nullptr;
    }
}

/*================================================================
  Hash algorithms
*/

ERL_NIF_TERM hash_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAS_3_0_API
    return digest_types_as_list(env);
#else
    const bool fips = FIPS_MODE();
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for (const auto &p: algo_hash) {
        if (fips && p.flags.fips_forbidden) continue;
        if (p.atom != ERL_CRYPTO_BAD_ATOM_VALUE) {
            list = enif_make_list_cell(env, p.atom, list);
        }
    }
    return list;
#endif
}

#if !defined(HAS_3_0_API)
void register_v1_hash(ErlNifEnv *env, const char *name, const bool fips_forbidden = false) {
    auto atom = enif_make_atom(env, name);
    algo_hash.emplace_back(name, atom, v1_hash_type_t::flags_t {fips_forbidden});
}
#endif

#if !defined(HAS_3_0_API)
void init_hash_types(ErlNifEnv* env) {
    // Validated algorithms first
    algo_hash.clear();
    register_v1_hash(env, "sha");
#ifdef HAVE_SHA224
    register_v1_hash(env, "sha224");
#endif
#ifdef HAVE_SHA256
    register_v1_hash(env, "sha256");
#endif
#ifdef HAVE_SHA384
    register_v1_hash(env, "sha384");
#endif
#ifdef HAVE_SHA512
    register_v1_hash(env, "sha512");
#endif
#ifdef HAVE_SHA3_224
    register_v1_hash(env, "sha3_224");
#endif
#ifdef HAVE_SHA3_256
    register_v1_hash(env, "sha3_256");
#endif
#ifdef HAVE_SHA3_384
    register_v1_hash(env, "sha3_384");
#endif
#ifdef HAVE_SHA3_512
    register_v1_hash(env, "sha3_512");
#endif
#ifdef HAVE_SHAKE128
    register_v1_hash(env, "shake128");
#endif
#ifdef HAVE_SHAKE256
    register_v1_hash(env, "shake256");
#endif
#ifdef HAVE_SM3
    register_v1_hash(env, "sm3");
#endif
#ifdef HAVE_BLAKE2
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "blake2b");
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "blake2s");
#endif

    // Non-validated algorithms follow
#ifdef HAVE_MD4
    register_v1_hash(env, "md4", true);
#endif
#ifdef HAVE_MD5
    register_v1_hash(env, "md5", true);
#endif
#ifdef HAVE_RIPEMD160
    register_v1_hash(env, "ripemd160", true);
#endif
}
#endif

/*================================================================
  Public key algorithms
*/

ERL_NIF_TERM pubkey_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const bool fips = FIPS_MODE();
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for (const auto &p: algo_pubkey) {
        if (fips && p.flags.fips_forbidden) continue;
        if (p.atom != ERL_CRYPTO_BAD_ATOM_VALUE) {
            list = enif_make_list_cell(env, p.atom, list);
        }
    }

#ifdef HAS_3_0_API
    list = build_pkey_type_list(env, list, fips);
#endif
    return list;
}

static void register_pubkey_type(ErlNifEnv *env, const char *atom_str, const bool fips_forbidden) {
    auto atom = enif_make_atom(env, atom_str);
    algo_pubkey.emplace_back(atom_str, atom, pubkey_type_t::flags_t {fips_forbidden});
}

void init_pubkey_types(ErlNifEnv* env) {
    // Validated algorithms are stored first in both FIPS and non-FIPS arrays
    algo_pubkey.clear();

    register_pubkey_type(env, "rsa", false);
#ifdef HAVE_DSA
    register_pubkey_type(env, "dss", false);
#endif

#ifdef HAVE_DH
    register_pubkey_type(env, "dh", false);
#endif
#if defined(HAVE_EC)

#if !defined(OPENSSL_NO_EC2M)
    register_pubkey_type(env, "ec_gf2m", false);
#endif
    register_pubkey_type(env, "ecdsa", false);
    register_pubkey_type(env, "ecdh", false);
#endif

    // Non-validated algorithms follow only into non-FIPS array
    // Don't know if Edward curves are fips validated
#if defined(HAVE_EDDSA)
    register_pubkey_type(env, "eddsa", true);
#endif

#if defined(HAVE_EDDH)
    register_pubkey_type(env, "eddh", true);
#endif
    register_pubkey_type(env, "srp", true);
}

ERL_NIF_TERM kem_algorithms_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_ML_KEM
    return enif_make_list3(env,
                           atom_mlkem512,
                           atom_mlkem768,
                           atom_mlkem1024);
#else
    return enif_make_list(env, 0);
#endif
}


/*================================================================
  Cipher key algorithms
*/

ERL_NIF_TERM cipher_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return cipher_types_as_list(env); /* Exclude old api ciphers */
}


/*================================================================
  MAC key algorithms
*/

ERL_NIF_TERM mac_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return mac_types_as_list(env);
}


/*================================================================
  Curves
*/

ERL_NIF_TERM curve_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const size_t fips_mode = FIPS_MODE();
    lazy_init_curves(env, fips_mode); // lazily initialize curves array for current FIPS mode

    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (const auto &p: algo_curve[fips_mode]) {
        if (fips_mode && p.flags.fips_forbidden) {
            continue;
        }
        list = enif_make_list_cell(env, p.atom, list);
    }
    return list;
}

static void init_curves(ErlNifEnv* env, bool fips);
#if defined(HAVE_EC)
static bool valid_curve(int nid);
#endif

// Caches curve count with FIPS and non-FIPS. Invokes init_curves once per FIPS mode
void lazy_init_curves(ErlNifEnv* env, const bool fips) {
    static bool algo_curve_initialized = false;
    static bool algo_curve_fips_initialized = false;
    if (!fips && algo_curve_initialized) {
        return;
    }
    if (fips && algo_curve_fips_initialized) {
        return;
    }

    enif_mutex_lock(mtx_init_curve_types);
    if (fips) {
        if (!algo_curve_fips_initialized) {
            init_curves(env, true);
            algo_curve_fips_initialized = true;
        }
    } else {
        if (!algo_curve_initialized) {
            init_curves(env, false);
            algo_curve_initialized = true;
        }
    }
    enif_mutex_unlock(mtx_init_curve_types);
}

static void register_curve(ErlNifEnv *env, const size_t fips_index, const char *name, const int nid) {
    if (!valid_curve(nid)) {
        return;
    }
    auto atom = enif_make_atom(env, name);
    algo_curve[fips_index].emplace_back(name, atom, curve_type_t::flags_t {false});
}

static void register_curve_non_fips(ErlNifEnv *env, const char *name) {
    auto atom = enif_make_atom(env, name);
    algo_curve[0].emplace_back(name, atom, curve_type_t::flags_t {true});
}

void init_curves(ErlNifEnv* env, const bool fips) {
#if defined(HAVE_EC)
#ifdef NID_secp160k1
    register_curve(env, fips, "secp160k1", NID_secp160k1);
#endif

#ifdef NID_secp160r1
    register_curve(env, fips, "secp160r1", NID_secp160r1);
#endif

#ifdef NID_secp160r2
    register_curve(env, fips, "secp160r2", NID_secp160r2);
#endif

#ifdef NID_secp192k1
    register_curve(env, fips, "secp192k1", NID_secp192k1);
#endif

#ifdef NID_secp224k1
    register_curve(env, fips, "secp224k1", NID_secp224k1);
#endif

#ifdef NID_secp224r1
    register_curve(env, fips, "secp224r1", NID_secp224r1);
#endif

#ifdef NID_secp256k1
    register_curve(env, fips, "secp256k1", NID_secp256k1);
#endif

#ifdef NID_secp384r1
    register_curve(env, fips, "secp384r1", NID_secp384r1);
#endif

#ifdef NID_secp521r1
    register_curve(env, fips, "secp521r1", NID_secp521r1);
#endif

#ifdef NID_X9_62_prime192v1
    register_curve(env, fips, "secp192r1", NID_X9_62_prime192v1);
    register_curve(env, fips, "prime192v1", NID_X9_62_prime192v1);
#endif

#ifdef NID_X9_62_prime192v2
    register_curve(env, fips, "prime192v2", NID_X9_62_prime192v2);
#endif

#ifdef NID_X9_62_prime192v3
    register_curve(env, fips, "prime192v3", NID_X9_62_prime192v3);
#endif

#ifdef NID_X9_62_prime239v1
    register_curve(env, fips, "prime239v1", NID_X9_62_prime239v1);
#endif

#ifdef NID_X9_62_prime239v2
    register_curve(env, fips, "prime239v2", NID_X9_62_prime239v2);
#endif

#ifdef NID_X9_62_prime239v3
    register_curve(env, fips, "prime239v3", NID_X9_62_prime239v3);
#endif

#ifdef NID_X9_62_prime256v1
    register_curve(env, fips, "secp256r1", NID_X9_62_prime256v1);
    register_curve(env, fips, "prime256v1", NID_X9_62_prime256v1);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls7
    register_curve(env, fips, "wtls7", NID_wap_wsg_idm_ecid_wtls7);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls9
    register_curve(env, fips, "wtls9", NID_wap_wsg_idm_ecid_wtls9);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls12
    register_curve(env, fips, "wtls12", NID_wap_wsg_idm_ecid_wtls12);
#endif

#ifdef NID_brainpoolP160r1
    register_curve(env, fips, "brainpoolP160r1", NID_brainpoolP160r1);
#endif

#ifdef NID_brainpoolP160t1
    register_curve(env, fips, "brainpoolP160t1", NID_brainpoolP160t1);
#endif

#ifdef NID_brainpoolP192r1
    register_curve(env, fips, "brainpoolP192r1", NID_brainpoolP192r1);
#endif

#ifdef NID_brainpoolP192t1
    register_curve(env, fips, "brainpoolP192t1", NID_brainpoolP192t1);
#endif

#ifdef NID_brainpoolP224r1
    register_curve(env, fips, "brainpoolP224r1", NID_brainpoolP224r1);
#endif

#ifdef NID_brainpoolP224t1
    register_curve(env, fips, "brainpoolP224t1", NID_brainpoolP224t1);
#endif

#ifdef NID_brainpoolP256r1
    register_curve(env, fips, "brainpoolP256r1", NID_brainpoolP256r1);
#endif

#ifdef NID_brainpoolP256t1
    register_curve(env, fips, "brainpoolP256t1", NID_brainpoolP256t1);
#endif

#ifdef NID_brainpoolP320r1
    register_curve(env, fips, "brainpoolP320r1", NID_brainpoolP320r1);
#endif

#ifdef NID_brainpoolP320t1
    register_curve(env, fips, "brainpoolP320t1", NID_brainpoolP320t1);
#endif

#ifdef NID_brainpoolP384r1
    register_curve(env, fips, "brainpoolP384r1", NID_brainpoolP384r1);
#endif

#ifdef NID_brainpoolP384t1
    register_curve(env, fips, "brainpoolP384t1", NID_brainpoolP384t1);
#endif

#ifdef NID_brainpoolP512r1
    register_curve(env, fips, "brainpoolP512r1", NID_brainpoolP512r1);
#endif

#ifdef NID_brainpoolP512t1
    register_curve(env, fips, "brainpoolP512t1", NID_brainpoolP512t1);
#endif

    //#if !defined(OPENSSL_NO_EC2M)        
#ifdef NID_sect163k1
    register_curve(env, fips, "sect163k1", NID_sect163k1);
#endif

#ifdef NID_sect163r1
    register_curve(env, fips, "sect163r1", NID_sect163r1);
#endif

#ifdef NID_sect163r2
    register_curve(env, fips, "sect163r2", NID_sect163r2);
#endif

#ifdef NID_sect193r1
    register_curve(env, fips, "sect193r1", NID_sect193r1);
#endif

#ifdef NID_sect193r2
    register_curve(env, fips, "sect193r2", NID_sect193r2);
#endif

#ifdef NID_sect233k1
    register_curve(env, fips, "sect233k1", NID_sect233k1);
#endif

#ifdef NID_sect233r1
    register_curve(env, fips, "sect233r1", NID_sect233r1);
#endif

#ifdef NID_sect239k1
    register_curve(env, fips, "sect239k1", NID_sect239k1);
#endif

#ifdef NID_sect283k1
    register_curve(env, fips, "sect283k1", NID_sect283k1);
#endif

#ifdef NID_sect283r1
    register_curve(env, fips, "sect283r1", NID_sect283r1);
#endif

#ifdef NID_sect409k1
    register_curve(env, fips, "sect409k1", NID_sect409k1);
#endif

#ifdef NID_sect409r1
    register_curve(env, fips, "sect409r1", NID_sect409r1);
#endif

#ifdef NID_sect571k1
    register_curve(env, fips, "sect571k1", NID_sect571k1);
#endif

#ifdef NID_sect571r1
    register_curve(env, fips, "sect571r1", NID_sect571r1);
#endif

#ifdef NID_X9_62_c2pnb163v1
    register_curve(env, fips, "c2pnb163v1", NID_X9_62_c2pnb163v1);
#endif

#ifdef NID_X9_62_c2pnb163v2
    register_curve(env, fips, "c2pnb163v2", NID_X9_62_c2pnb163v2);
#endif

#ifdef NID_X9_62_c2pnb163v3
    register_curve(env, fips, "c2pnb163v3", NID_X9_62_c2pnb163v3);
#endif

#ifdef NID_X9_62_c2pnb176v1
    register_curve(env, fips, "c2pnb176v1", NID_X9_62_c2pnb176v1);
#endif

#ifdef NID_X9_62_c2tnb191v1
    register_curve(env, fips, "c2tnb191v1", NID_X9_62_c2tnb191v1);
#endif

#ifdef NID_X9_62_c2tnb191v2
    register_curve(env, fips, "c2tnb191v2", NID_X9_62_c2tnb191v2);
#endif

#ifdef NID_X9_62_c2tnb191v3
    register_curve(env, fips, "c2tnb191v3", NID_X9_62_c2tnb191v3);
#endif

#ifdef NID_X9_62_c2pnb208w1
    register_curve(env, fips, "c2pnb208w1", NID_X9_62_c2pnb208w1);
#endif

#ifdef NID_X9_62_c2tnb239v1
    register_curve(env, fips, "c2tnb239v1", NID_X9_62_c2tnb239v1);
#endif

#ifdef NID_X9_62_c2tnb239v2
    register_curve(env, fips, "c2tnb239v2", NID_X9_62_c2tnb239v2);
#endif

#ifdef NID_X9_62_c2tnb239v3
    register_curve(env, fips, "c2tnb239v3", NID_X9_62_c2tnb239v3);
#endif

#ifdef NID_X9_62_c2pnb272w1
    register_curve(env, fips, "c2pnb272w1", NID_X9_62_c2pnb272w1);
#endif

#ifdef NID_X9_62_c2pnb304w1
    register_curve(env, fips, "c2pnb304w1", NID_X9_62_c2pnb304w1);
#endif

#ifdef NID_X9_62_c2tnb359v1
    register_curve(env, fips, "c2tnb359v1", NID_X9_62_c2tnb359v1);
#endif

#ifdef NID_X9_62_c2pnb368w1
    register_curve(env, fips, "c2pnb368w1", NID_X9_62_c2pnb368w1);
#endif

#ifdef NID_X9_62_c2tnb431r1
    register_curve(env, fips, "c2tnb431r1", NID_X9_62_c2tnb431r1);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls3
    register_curve(env, fips, "wtls3", NID_wap_wsg_idm_ecid_wtls3);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls5
    register_curve(env, fips, "wtls5", NID_wap_wsg_idm_ecid_wtls5);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls10
    register_curve(env, fips, "wtls10", NID_wap_wsg_idm_ecid_wtls10);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls11
    register_curve(env, fips, "wtls11", NID_wap_wsg_idm_ecid_wtls11);
#endif

    // Non-validated algorithms follow
#ifdef NID_secp112r1
    register_curve(env, fips, "secp112r1", NID_secp112r1);
#endif

#ifdef NID_secp112r2
    register_curve(env, fips, "secp112r2", NID_secp112r2);
#endif

#ifdef NID_secp128r1
    register_curve(env, fips, "secp128r1", NID_secp128r1);
#endif

#ifdef NID_secp128r2
    register_curve(env, fips, "secp128r2", NID_secp128r2);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls6
    register_curve(env, fips, "wtls6", NID_wap_wsg_idm_ecid_wtls6);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls8
    register_curve(env, fips, "wtls8", NID_wap_wsg_idm_ecid_wtls8);
#endif

    //#if !defined(OPENSSL_NO_EC2M)
#ifdef NID_sect113r1
    register_curve(env, fips, "sect113r1", NID_sect113r1);
#endif

#ifdef NID_sect113r2
    register_curve(env, fips, "sect113r2", NID_sect113r2);
#endif

#ifdef NID_sect131r1
    register_curve(env, fips, "sect131r1", NID_sect131r1);
#endif

#ifdef NID_sect131r2
    register_curve(env, fips, "sect131r2", NID_sect131r2);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls1
    register_curve(env, fips, "wtls1", NID_wap_wsg_idm_ecid_wtls1);
#endif

#ifdef NID_wap_wsg_idm_ecid_wtls4
    register_curve(env, fips, "wtls4", NID_wap_wsg_idm_ecid_wtls4);
#endif

#ifdef NID_ipsec3
    register_curve(env, fips, "ipsec3", NID_ipsec3);
#endif

#ifdef NID_ipsec4
    register_curve(env, fips, "ipsec4", NID_ipsec4);
#endif

    if (!fips) {
#ifdef HAVE_ED25519
        register_curve_non_fips(env, "ed25519");
#endif
#ifdef HAVE_ED448
        register_curve_non_fips(env,"ed448");
#endif
#ifdef HAVE_X25519
        register_curve_non_fips(env,"x25519");
#endif
#ifdef HAVE_X448
        register_curve_non_fips(env,"x448");
#endif
    }
#endif
}

#if defined(HAVE_EC)

/* Check if the curve in nid is supported by the
   current cryptolib and current FIPS state.
*/

bool valid_curve(const int nid) {
    bool ret = false;

#if defined(HAVE_DH)
# if defined(HAS_EVP_PKEY_CTX) && (! DISABLE_EVP_DH)
    EVP_PKEY_CTX *pctx = nullptr, *kctx = nullptr;
    EVP_PKEY *pkey = nullptr, *params = nullptr;
    
    if (nullptr == (pctx = EVP_PKEY_CTX_new_id(EVP_PKEY_EC, nullptr)))
        goto out;
    
    if (1 != EVP_PKEY_paramgen_init(pctx))
        goto out;

    if (1 != EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx, nid))
        goto out;

    if (!EVP_PKEY_paramgen(pctx, &params))
        goto out;

    if (nullptr == (kctx = EVP_PKEY_CTX_new(params, nullptr)))
        goto out;

    if(1 != EVP_PKEY_keygen_init(kctx))
        goto out;
    if (1 != EVP_PKEY_keygen(kctx, &pkey))
        goto out;
    ret = true;
 out:
    if (pkey) EVP_PKEY_free(pkey);
    if (kctx) EVP_PKEY_CTX_free(kctx);
    if (params) EVP_PKEY_free(params);
    if (pctx) EVP_PKEY_CTX_free(pctx);

# else
    EC_KEY *key;

    if (NULL == (key = EC_KEY_new_by_curve_name(nid)))
        goto out;

    if(1 != EC_KEY_generate_key(key))
        goto out;

    ret = true;
 out:
    if (key) EC_KEY_free(key);
# endif
#endif /* HAVE_DH etc */
    
    return ret;
}
#endif /* HAVE_EC */

/*================================================================
  RSA Options
*/

ERL_NIF_TERM rsa_opts_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const bool fips = FIPS_MODE();
    ERL_NIF_TERM hd = enif_make_list(env, 0);
    for (const auto &p: algo_rsa_opts) {
        if (fips && p.flags.fips_forbidden) continue;
        if (p.atom != ERL_CRYPTO_BAD_ATOM_VALUE) {
            hd = enif_make_list_cell(env, p.atom, hd);
        }
    }
    return hd;
}

static void register_rsa_opt(ErlNifEnv *env, const char *str, const bool fips_forbidden = false)
{
    ERL_NIF_TERM atom = enif_make_atom(env, str);
    algo_rsa_opts.emplace_back(str, atom, rsaopt_type_t::flags_t {fips_forbidden});
}

void init_rsa_opts_types(ErlNifEnv* env) {
    algo_rsa_opts.clear();

    // Validated algorithms first
#ifdef HAS_EVP_PKEY_CTX
#ifdef HAVE_RSA_PKCS1_PSS_PADDING
    register_rsa_opt(env, "rsa_pkcs1_pss_padding");
    register_rsa_opt(env, "rsa_pss_saltlen");
#endif
#ifdef HAVE_RSA_MGF1_MD
    register_rsa_opt(env, "rsa_mgf1_md");
#endif
#ifdef HAVE_RSA_OAEP_PADDING
    register_rsa_opt(env, "rsa_pkcs1_oaep_padding");
#endif
#ifdef HAVE_RSA_OAEP_MD
    register_rsa_opt(env, "rsa_oaep_label");
    register_rsa_opt(env, "rsa_oaep_md");
#endif
    register_rsa_opt(env, "signature_md");
#endif
    register_rsa_opt(env, "rsa_pkcs1_padding");
    register_rsa_opt(env, "rsa_x931_padding");
#ifdef HAVE_RSA_SSLV23_PADDING
    register_rsa_opt(env, "rsa_sslv23_padding");
#endif
    register_rsa_opt(env, "rsa_no_padding");
}

