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

#include "algorithms.h"
#include "cipher.h"
#include "common.h"
#include "mac.h"
#include "pkey.h"

#include <openssl/core_names.h>
#ifdef HAS_3_0_API
#include "digest.h"
#endif

#ifdef HAS_3_0_API
#else
static size_t algo_hash_cnt, algo_hash_fips_cnt;
static ERL_NIF_TERM algo_hash[17];   /* increase when extending the list */
void init_hash_types(ErlNifEnv* env);
#endif

struct pkey_availability_t {
    const char* str_v3; /* the algorithm name as in OpenSSL 3.x */
    unsigned flags;     /* combination of PKEY_AVAIL_FLAGS */
    ERL_NIF_TERM atom;  /* added to results when the user is querying */
};

enum PKEY_AVAIL_FLAGS {
    FIPS_PKEY_NOT_AVAIL = 1,
    FIPS_FORBIDDEN_PKEY_KEYGEN = 2,   /* not available by name */
    FIPS_FORBIDDEN_PKEY_SIGN = 4,     /* not available for signing */
    FIPS_FORBIDDEN_PKEY_VERIFY = 8,   /* not available for verification */
    FIPS_FORBIDDEN_PKEY_ENCRYPT = 16, /* not available for encryption */
    FIPS_FORBIDDEN_PKEY_DERIVE = 32,  /* not available for key derivation */
    FIPS_FORBIDDEN_PKEY_ALL = FIPS_FORBIDDEN_PKEY_KEYGEN | FIPS_FORBIDDEN_PKEY_SIGN |
        FIPS_FORBIDDEN_PKEY_VERIFY | FIPS_FORBIDDEN_PKEY_ENCRYPT | FIPS_FORBIDDEN_PKEY_DERIVE
};

#ifdef FIPS_SUPPORT
# define IS_PUBKEY_FORBIDDEN_IN_FIPS(p) (((p)->flags == FIPS_FORBIDDEN_PKEY_ALL || (p)->flags == FIPS_PKEY_NOT_AVAIL) && FIPS_MODE())
#else
# define IS_PUBKEY_FORBIDDEN_IN_FIPS(P) false
#endif

/* Stores all known public key algorithms with their FIPS unavailability flag if FIPS is enabled */
static struct pkey_availability_array_t {
    size_t count;
    struct pkey_availability_t algorithm[12]; /* increase when extending the list */
} algo_pubkey;

struct kem_availability_t {
    const char* str_v3;  /* the algorithm name as in OpenSSL 3.x */
    unsigned flags;      /* combination of KEM_AVAIL_FLAGS */
    ERL_NIF_TERM atom;   /* as returned to the library user on a query */
};

enum KEM_AVAIL_FLAGS {
    FIPS_KEM_NOT_AVAIL = 1,
};

#ifdef FIPS_SUPPORT
# define IS_KEM_FORBIDDEN_IN_FIPS(p) (((p)->flags & FIPS_KEM_NOT_AVAIL) == FIPS_KEM_NOT_AVAIL && FIPS_MODE())
#else
# define IS_KEM_FORBIDDEN_IN_FIPS(P) false
#endif

/* Stores all known KEM algorithms with their FIPS unavailability flag if
 * FIPS is enabled */
static struct kem_availability_array_t {
    size_t count;
    struct kem_availability_t algorithm[3]; /* increase when extending the list */
} algo_kem;

void init_pubkey_types(ErlNifEnv* env);
void init_kem_types(void);

struct curve_availability_t {
    const char* str_v3;         /* the algorithm name as in OpenSSL 3.x */
    unsigned flags;             /* combination of CURVE_AVAIL_FLAGS */
    ERL_NIF_TERM atom;          /* as returned to the library user on a query */
};

enum CURVE_AVAIL_FLAGS {
    FIPS_CURVE_INIT_FAILED = 1,   /* could not find by name or initialize */
};

#ifdef FIPS_SUPPORT
# define IS_CURVE_FORBIDDEN_IN_FIPS(p) ((p)->flags != 0 && FIPS_MODE())
#else
# define IS_CURVE_FORBIDDEN_IN_FIPS(P) false
#endif

static struct curve_availability_array_t {
    ssize_t count; /* Negative -1 serves as a flag for lazy initiazlilization */

    /* [0] contains non-FIPS, and [1] contains FIPS curve details */
    struct curve_availability_t algorithms[89]; /* increase when extending the list */
    ErlNifMutex* mtx_init_curve_types;
} algo_curve = {-1, {0}, NULL};

static size_t curves_lazy_init(ErlNifEnv* env, bool fips_enabled);

static size_t algo_rsa_opts_cnt, algo_rsa_opts_fips_cnt;
static ERL_NIF_TERM algo_rsa_opts[11]; /* increase when extending the list */
void init_rsa_opts_types(ErlNifEnv* env);


void init_algorithms_types(ErlNifEnv* env)
{
#ifdef HAS_3_0_API
#else
    init_hash_types(env);
#endif
    init_pubkey_types(env);
    init_kem_types();
    init_rsa_opts_types(env);
    /* ciphers and macs are initiated statically */
}


int create_curve_mutex(void)
{
    if (!algo_curve.mtx_init_curve_types) {
        algo_curve.mtx_init_curve_types =  enif_mutex_create("init_curve_types");
    }
    return !!algo_curve.mtx_init_curve_types;
}

void destroy_curve_mutex(void)
{
    if (algo_curve.mtx_init_curve_types) {
        enif_mutex_destroy(algo_curve.mtx_init_curve_types);
        algo_curve.mtx_init_curve_types = NULL;
    }
}

/*================================================================
  Hash algorithms
*/

ERL_NIF_TERM hash_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAS_3_0_API
    return digest_types_as_list(env, false);
#else
    unsigned int cnt  =
        FIPS_MODE() ? algo_hash_fips_cnt : algo_hash_cnt;

    return enif_make_list_from_array(env, algo_hash, cnt);
#endif
}

#ifdef HAS_3_0_API
#else
void init_hash_types(ErlNifEnv* env) {
    // Validated algorithms first
    algo_hash_cnt = 0;
    algo_hash[algo_hash_cnt++] = atom_sha;
#ifdef HAVE_SHA224
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha224");
#endif
#ifdef HAVE_SHA256
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha256");
#endif
#ifdef HAVE_SHA384
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha384");
#endif
#ifdef HAVE_SHA512
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha512");
#endif
#ifdef HAVE_SHA3_224
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_224");
#endif
#ifdef HAVE_SHA3_256
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_256");
#endif
#ifdef HAVE_SHA3_384
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_384");
#endif
#ifdef HAVE_SHA3_512
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_512");
#endif
#ifdef HAVE_SHAKE128
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "shake128");
#endif
#ifdef HAVE_SHAKE256
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "shake256");
#endif
#ifdef HAVE_SM3
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sm3");
#endif
#ifdef HAVE_BLAKE2
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "blake2b");
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "blake2s");
#endif

    // Non-validated algorithms follow
    algo_hash_fips_cnt = algo_hash_cnt;
#ifdef HAVE_MD4
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "md4");
#endif
#ifdef HAVE_MD5
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "md5");
#endif
#ifdef HAVE_RIPEMD160
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "ripemd160");
#endif

    ASSERT(algo_hash_cnt <= sizeof(algo_hash)/sizeof(algo_hash[0]));
}
#endif

/*================================================================
  Public key algorithms
*/

static ERL_NIF_TERM pubkey_algorithms_as_list(ErlNifEnv* env, const bool fips_forbidden) {
    ERL_NIF_TERM hd = enif_make_list(env, 0);

    for (size_t i = 0; i < algo_pubkey.count; i++) {
        struct pkey_availability_t* algo = &algo_pubkey.algorithm[i];

        /* Any of the forbidden flags is not set, then something is available */
        if (IS_PUBKEY_FORBIDDEN_IN_FIPS(algo) == fips_forbidden) {
            hd = enif_make_list_cell(env, algo->atom, hd);
        }
    }
    return hd;
}

ERL_NIF_TERM pubkey_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* Filter the results by IS_PUBKEY_FORBIDDEN_IN_FIPS() == false */
    return pubkey_algorithms_as_list(env, false);
}

static void add_pubkey_algorithm(ErlNifEnv* env, const char* str_v3,
    const unsigned unavailable, ERL_NIF_TERM atom)
{
    struct pkey_availability_t* algo = &algo_pubkey.algorithm[algo_pubkey.count];
    algo->str_v3 = str_v3;
    algo->flags = unavailable;

    if (!atom) atom = enif_make_atom(env, str_v3);
    algo->atom = atom;

    algo_pubkey.count++;
}

/*
 * for FIPS will attempt to initialize the pubkey context to verify whether the
 * algorithm is allowed, for non-FIPS the old behavior - always allow.
 * Pass 0 for atom to create one right here.
 */
static void probe_pubkey_algorithm(ErlNifEnv* env, const char* str_v3, ERL_NIF_TERM atom) {
    unsigned unavailable = 0;
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, "fips=yes");
    /* failed: algorithm not available, do not add */
    if (ctx) {
        if (EVP_PKEY_keygen_init(ctx) <= 0) { /* can't generate keys */
            unavailable |= FIPS_FORBIDDEN_PKEY_KEYGEN;
        }
        EVP_PKEY_CTX_free(ctx);

        ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
        if (EVP_PKEY_sign_init(ctx) <= 0) { /* can't sign */
            unavailable |= FIPS_FORBIDDEN_PKEY_SIGN;
        }
        EVP_PKEY_CTX_free(ctx);

        ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
        if (EVP_PKEY_verify_init(ctx) <= 0) { /* can't verify */
            unavailable |= FIPS_FORBIDDEN_PKEY_VERIFY;
        }
        EVP_PKEY_CTX_free(ctx);

        ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
        if (EVP_PKEY_encrypt_init(ctx) <= 0) { /* can't encrypt/decrypt */
            unavailable |= FIPS_FORBIDDEN_PKEY_ENCRYPT;
        }
        EVP_PKEY_CTX_free(ctx);

        ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
        if (EVP_PKEY_derive_init(ctx) <= 0) { /* can't derive */
            unavailable |= FIPS_FORBIDDEN_PKEY_DERIVE;
        }
        EVP_PKEY_CTX_free(ctx);
    } else {
        unavailable |= FIPS_PKEY_NOT_AVAIL;
    }
#endif /* FIPS_SUPPORT && HAS_3_0_API */
    add_pubkey_algorithm(env, str_v3, unavailable, atom);
}

void init_pubkey_types(ErlNifEnv* env) {
    // Validated algorithms first
    algo_pubkey.count = 0;
    probe_pubkey_algorithm(env, "rsa", 0);
#ifdef HAVE_DSA
    probe_pubkey_algorithm(env, "dss", 0);
#endif

#ifdef HAVE_DH
    probe_pubkey_algorithm(env, "dh", 0);
#endif

#if defined(HAVE_EC)
#if !defined(OPENSSL_NO_EC2M)
    probe_pubkey_algorithm(env, "ec_gf2m", 0);
#endif
    probe_pubkey_algorithm(env, "ecdsa", 0);
    probe_pubkey_algorithm(env, "ecdh", 0);
#endif

    // Non-validated algorithms follow
    // Don't know if Edward curves are fips validated
#if defined(HAVE_EDDSA)
    probe_pubkey_algorithm(env, "eddsa", 0);
#endif
#if defined(HAVE_EDDH)
    probe_pubkey_algorithm(env, "eddh", 0);
#endif
    probe_pubkey_algorithm(env, "srp", 0);
#ifdef HAVE_ML_DSA
    probe_pubkey_algorithm(env, "mldsa44", atom_mldsa44);
    probe_pubkey_algorithm(env, "mldsa65", atom_mldsa65);
    probe_pubkey_algorithm(env, "mldsa87", atom_mldsa87);
#endif
    /* When adding a new algorithm, update the size of algo_pubkey.algorithm array */
    ASSERT(algo_pubkey.count <= sizeof(algo_pubkey.algorithm)/sizeof(algo_pubkey.algorithm[0]));
}

#ifdef HAVE_ML_KEM
static void add_kem_algorithm(const char* str_v3, const unsigned unavail_flags, ERL_NIF_TERM atom) {
    struct kem_availability_t* algo = &algo_kem.algorithm[algo_kem.count];
    algo->str_v3 = str_v3;
    algo->flags = unavail_flags;
    algo->atom = atom;
    algo_kem.count++;
}
#endif

#ifdef HAVE_ML_KEM
/*
 * for FIPS will attempt to initialize the KEM context to verify whether the
 * algorithm is allowed, for non-FIPS the old behavior - always allow.
 */
static void probe_kem_algorithm(const char* str_v3, ERL_NIF_TERM atom) {
    unsigned unavailable = 0;
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    EVP_KEM *kem = EVP_KEM_fetch(NULL, str_v3, "fips=yes");
    if (!kem) {
        return; /* not available by name */
    }

    EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, "fips=yes");
    /* failed: algorithm not available, do not add */
    if (ctx) {
        if (EVP_PKEY_encapsulate_init(ctx, NULL) == 1) {
            EVP_PKEY_CTX_free(ctx);
        } else {
            unavailable |= FIPS_KEM_NOT_AVAIL;
        }
    }

    EVP_KEM_free(kem);
#endif /* FIPS_SUPPORT && HAS_3_0_API */

    add_kem_algorithm(str_v3, unavailable, atom);
}
#endif

void init_kem_types(void) {
    algo_kem.count = 0;
#ifdef HAVE_ML_KEM
    probe_kem_algorithm("mlkem512", atom_mlkem512);
    probe_kem_algorithm("mlkem768", atom_mlkem768);
    probe_kem_algorithm("mlkem1024", atom_mlkem1024);
    /* When adding a new algorithm, update the size of algo_kem.algorithm array */
#endif
    ASSERT(algo_kem.count <= sizeof(algo_kem.algorithm)/sizeof(algo_kem.algorithm[0]));
}

static ERL_NIF_TERM kem_algorithms_as_list(ErlNifEnv* env, const bool fips_forbidden)
{
    ERL_NIF_TERM hd = enif_make_list(env, 0);

    for (size_t i = 0; i < algo_kem.count; i++) {
        struct kem_availability_t* p = &algo_kem.algorithm[i];
        if (IS_KEM_FORBIDDEN_IN_FIPS(p) == fips_forbidden) {
            hd = enif_make_list_cell(env, p->atom, hd);
        }
    }

    return hd;
}

ERL_NIF_TERM kem_algorithms_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_ML_KEM
    return kem_algorithms_as_list(env, false);
#else
    return enif_make_list(env, 0);
#endif
}


/*================================================================
  Cipher key algorithms
*/

ERL_NIF_TERM cipher_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* Exclude old API ciphers. Filter the results by IS_CIPHER_FORBIDDEN_IN_FIPS() == false */
    return cipher_types_as_list(env, false);
}


/*================================================================
  MAC key algorithms
*/

ERL_NIF_TERM mac_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return mac_types_as_list(env, false);
}


/*================================================================
  Curves
*/

static ERL_NIF_TERM curve_algorithms_as_list(ErlNifEnv* env, const bool fips_forbidden)
{
    ERL_NIF_TERM hd = enif_make_list(env, 0);

    for (size_t i = 0; i < algo_curve.count; i++) {
        struct curve_availability_t *p = &algo_curve.algorithms[i];

        if (IS_CURVE_FORBIDDEN_IN_FIPS(p) == fips_forbidden) {
            hd = enif_make_list_cell(env, p->atom, hd);
        }
    }

    return hd;
}

ERL_NIF_TERM curve_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    curves_lazy_init(env, FIPS_MODE());
    return curve_algorithms_as_list(env, false);
}

static void init_curves(ErlNifEnv* env, bool fips);
#if defined(HAVE_EC)
static bool is_curve_valid_by_nid(int nid);
#endif

/* Perform late lazy init of curve algorithms, hence the need for mutex */
static size_t curves_lazy_init(ErlNifEnv* env, const bool fips_enabled) {
    size_t result = 0;
    if (algo_curve.count >= 0) return algo_curve.count;

    enif_mutex_lock(algo_curve.mtx_init_curve_types);
    if (algo_curve.count < 0) {
        init_curves(env, fips_enabled); /* also updates algo_curve.count[0] or [1] */
        result = algo_curve.count;
    }
    enif_mutex_unlock(algo_curve.mtx_init_curve_types);

    return result;
}

static void add_curve(ErlNifEnv* env, const char* str_v3,
                      const unsigned unavail_flags)
{
    struct curve_availability_t* curve = &algo_curve.algorithms[algo_curve.count];
    curve->str_v3 = str_v3;
    curve->atom = enif_make_atom(env, str_v3);
    curve->flags = unavail_flags;
    algo_curve.count++;
}

static void add_curve_if_supported(const int nid, ErlNifEnv* env, bool fips_enabled, const char* str_v3) {
    /* Some curves can be pre-checked by their NID */
    if (nid && !is_curve_valid_by_nid(nid)) { /* passing NID=0 will skip this check */
        return;
    }

#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    {
        EVP_PKEY_CTX *pctx = NULL;
        EVP_PKEY *pkey = NULL;
        unsigned unavail_flags = 0;

        /* This checking code only runs under FIPS and OpenSSL 3+, other cases algorithm is always added */
        if (fips_enabled) {
            OSSL_PARAM params[2];

            pctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", "fips=yes");
            if (!pctx) {
                unavail_flags |= FIPS_CURVE_INIT_FAILED;
                goto add_anyway; /* EC keygen context not available */
            }
            if (EVP_PKEY_keygen_init(pctx) <= 0) {
                unavail_flags |= FIPS_CURVE_INIT_FAILED;
                goto add_anyway;
            }
            params[0] = OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_GROUP_NAME, (char *)str_v3, 0);
            params[1] = OSSL_PARAM_construct_end();
            if (EVP_PKEY_CTX_set_params(pctx, params) <= 0) {
                unavail_flags |= FIPS_CURVE_INIT_FAILED;
                goto add_anyway;
            }
            if (EVP_PKEY_generate(pctx, &pkey) <= 0) {
                unavail_flags |= FIPS_CURVE_INIT_FAILED;
            }
        }
        add_anyway:
            add_curve(env, str_v3, unavail_flags);
        EVP_PKEY_free(pkey); /* NULL is allowed */
        EVP_PKEY_CTX_free(pctx); /* NULL is allowed */
    }
#else
    add_curve(env, fips_enabled, str_v3, 0);
#endif
}

void init_curves(ErlNifEnv* env, const bool fips) {
#if defined(HAVE_EC)
    algo_curve.count = 0;

#ifdef NID_secp160k1
    add_curve_if_supported(NID_secp160k1, env, fips, "secp160k1");
#else
#endif
#ifdef NID_secp160r1
    add_curve_if_supported(NID_secp160r1, env, fips, "secp160r1");
#else
#endif
#ifdef NID_secp160r2
    add_curve_if_supported(NID_secp160r2, env, fips, "secp160r2");
#else
#endif
#ifdef NID_secp192k1
    add_curve_if_supported(NID_secp192k1, env, fips, "secp192k1");
#else
#endif
#ifdef NID_secp224k1
    add_curve_if_supported(NID_secp224k1, env, fips, "secp224k1");
#else
#endif
#ifdef NID_secp224r1
    add_curve_if_supported(NID_secp224r1, env, fips, "secp224r1");
#else
#endif
#ifdef NID_secp256k1
    add_curve_if_supported(NID_secp256k1, env, fips, "secp256k1");
#else
#endif
#ifdef NID_secp384r1
    add_curve_if_supported(NID_secp384r1, env, fips, "secp384r1");
#else
#endif
#ifdef NID_secp521r1
    add_curve_if_supported(NID_secp521r1, env, fips, "secp521r1");
#else
#endif
#ifdef NID_X9_62_prime192v1
    add_curve_if_supported(NID_X9_62_prime192v1, env, fips, "secp192r1");
    add_curve_if_supported(NID_X9_62_prime192v1, env, fips, "prime192v1");
#else
#endif
#ifdef NID_X9_62_prime192v2
    add_curve_if_supported(NID_X9_62_prime192v2, env, fips, "prime192v2");
#else
#endif
#ifdef NID_X9_62_prime192v3
    add_curve_if_supported(NID_X9_62_prime192v3, env, fips, "prime192v3");
#else
#endif
#ifdef NID_X9_62_prime239v1
    add_curve_if_supported(NID_X9_62_prime239v1, env, fips, "prime239v1");
#else
#endif
#ifdef NID_X9_62_prime239v2
    add_curve_if_supported(NID_X9_62_prime239v2, env, fips, "prime239v2");
#else
#endif
#ifdef NID_X9_62_prime239v3
    add_curve_if_supported(NID_X9_62_prime239v3, env, fips, "prime239v3");
#else
#endif
#ifdef NID_X9_62_prime256v1
    add_curve_if_supported(NID_X9_62_prime256v1, env, fips, "secp256r1");
    add_curve_if_supported(NID_X9_62_prime256v1, env, fips, "prime256v1");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls7
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls7, env, fips, "wtls7");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls9
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls9, env, fips, "wtls9");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls12
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls12, env, fips, "wtls12");
#else
#endif
#ifdef NID_brainpoolP160r1
    add_curve_if_supported(NID_brainpoolP160r1, env, fips, "brainpoolP160r1");
#else
#endif
#ifdef NID_brainpoolP160t1
    add_curve_if_supported(NID_brainpoolP160t1, env, fips, "brainpoolP160t1");
#else
#endif
#ifdef NID_brainpoolP192r1
    add_curve_if_supported(NID_brainpoolP192r1, env, fips, "brainpoolP192r1");
#else
#endif
#ifdef NID_brainpoolP192t1
    add_curve_if_supported(NID_brainpoolP192t1, env, fips, "brainpoolP192t1");
#else
#endif
#ifdef NID_brainpoolP224r1
    add_curve_if_supported(NID_brainpoolP224r1, env, fips, "brainpoolP224r1");
#else
#endif
#ifdef NID_brainpoolP224t1
    add_curve_if_supported(NID_brainpoolP224t1, env, fips, "brainpoolP224t1");
#else
#endif
#ifdef NID_brainpoolP256r1
    add_curve_if_supported(NID_brainpoolP256r1, env, fips, "brainpoolP256r1");
#else
#endif
#ifdef NID_brainpoolP256t1
    add_curve_if_supported(NID_brainpoolP256t1, env, fips, "brainpoolP256t1");
#else
#endif
#ifdef NID_brainpoolP320r1
    add_curve_if_supported(NID_brainpoolP320r1, env, fips, "brainpoolP320r1");
#else
#endif
#ifdef NID_brainpoolP320t1
    add_curve_if_supported(NID_brainpoolP320t1, env, fips, "brainpoolP320t1");
#else
#endif
#ifdef NID_brainpoolP384r1
    add_curve_if_supported(NID_brainpoolP384r1, env, fips, "brainpoolP384r1");
#else
#endif
#ifdef NID_brainpoolP384t1
    add_curve_if_supported(NID_brainpoolP384t1, env, fips, "brainpoolP384t1");
#else
#endif
#ifdef NID_brainpoolP512r1
    add_curve_if_supported(NID_brainpoolP512r1, env, fips, "brainpoolP512r1");
#else
#endif
#ifdef NID_brainpoolP512t1
    add_curve_if_supported(NID_brainpoolP512t1, env, fips, "brainpoolP512t1");
#else
#endif
    //#if !defined(OPENSSL_NO_EC2M)
#ifdef NID_sect163k1
    add_curve_if_supported(NID_sect163k1, env, fips, "sect163k1");
#else
#endif
#ifdef NID_sect163r1
    add_curve_if_supported(NID_sect163r1, env, fips, "sect163r1");
#else
#endif
#ifdef NID_sect163r2
    add_curve_if_supported(NID_sect163r2, env, fips, "sect163r2");
#else
#endif
#ifdef NID_sect193r1
    add_curve_if_supported(NID_sect193r1, env, fips, "sect193r1");
#else
#endif
#ifdef NID_sect193r2
    add_curve_if_supported(NID_sect193r2, env, fips, "sect193r2");
#else
#endif
#ifdef NID_sect233k1
    add_curve_if_supported(NID_sect233k1, env, fips, "sect233k1");
#else
#endif
#ifdef NID_sect233r1
    add_curve_if_supported(NID_sect233r1, env, fips, "sect233r1");
#else
#endif
#ifdef NID_sect239k1
    add_curve_if_supported(NID_sect239k1, env, fips, "sect239k1");
#else
#endif
#ifdef NID_sect283k1
    add_curve_if_supported(NID_sect283k1, env, fips, "sect283k1");
#else
#endif
#ifdef NID_sect283r1
    add_curve_if_supported(NID_sect283r1, env, fips, "sect283r1");
#else
#endif
#ifdef NID_sect409k1
    add_curve_if_supported(NID_sect409k1, env, fips, "sect409k1");
#else
#endif
#ifdef NID_sect409r1
    add_curve_if_supported(NID_sect409r1, env, fips, "sect409r1");
#else
#endif
#ifdef NID_sect571k1
    add_curve_if_supported(NID_sect571k1, env, fips, "sect571k1");
#else
#endif
#ifdef NID_sect571r1
    add_curve_if_supported(NID_sect571r1, env, fips, "sect571r1");
#else
#endif
#ifdef NID_X9_62_c2pnb163v1
    add_curve_if_supported(NID_X9_62_c2pnb163v1, env, fips, "c2pnb163v1");
#else
#endif
#ifdef NID_X9_62_c2pnb163v2
    add_curve_if_supported(NID_X9_62_c2pnb163v2, env, fips, "c2pnb163v2");
#else
#endif
#ifdef NID_X9_62_c2pnb163v3
    add_curve_if_supported(NID_X9_62_c2pnb163v3, env, fips, "c2pnb163v3");
#else
#endif
#ifdef NID_X9_62_c2pnb176v1
    add_curve_if_supported(NID_X9_62_c2pnb176v1, env, fips, "c2pnb176v1");
#else
#endif
#ifdef NID_X9_62_c2tnb191v1
    add_curve_if_supported(NID_X9_62_c2tnb191v1, env, fips, "c2tnb191v1");
#else
#endif
#ifdef NID_X9_62_c2tnb191v2
    add_curve_if_supported(NID_X9_62_c2tnb191v2, env, fips, "c2tnb191v2");
#else
#endif
#ifdef NID_X9_62_c2tnb191v3
    add_curve_if_supported(NID_X9_62_c2tnb191v3, env, fips, "c2tnb191v3");
#else
#endif
#ifdef NID_X9_62_c2pnb208w1
    add_curve_if_supported(NID_X9_62_c2pnb208w1, env, fips, "c2pnb208w1");
#else
#endif
#ifdef NID_X9_62_c2tnb239v1
    add_curve_if_supported(NID_X9_62_c2tnb239v1, env, fips, "c2tnb239v1");
#else
#endif
#ifdef NID_X9_62_c2tnb239v2
    add_curve_if_supported(NID_X9_62_c2tnb239v2, env, fips, "c2tnb239v2");
#else
#endif
#ifdef NID_X9_62_c2tnb239v3
    add_curve_if_supported(NID_X9_62_c2tnb239v3, env, fips, "c2tnb239v3");
#else
#endif
#ifdef NID_X9_62_c2pnb272w1
    add_curve_if_supported(NID_X9_62_c2pnb272w1, env, fips, "c2pnb272w1");
#else
#endif
#ifdef NID_X9_62_c2pnb304w1
    add_curve_if_supported(NID_X9_62_c2pnb304w1, env, fips, "c2pnb304w1");
#else
#endif
#ifdef NID_X9_62_c2tnb359v1
    add_curve_if_supported(NID_X9_62_c2tnb359v1, env, fips, "c2tnb359v1");
#else
#endif
#ifdef NID_X9_62_c2pnb368w1
    add_curve_if_supported(NID_X9_62_c2pnb368w1, env, fips, "c2pnb368w1");
#else
#endif
#ifdef NID_X9_62_c2tnb431r1
    add_curve_if_supported(NID_X9_62_c2tnb431r1, env, fips, "c2tnb431r1");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls3
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls3, env, fips, "wtls3");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls5
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls5, env, fips, "wtls5");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls10
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls10, env, fips, "wtls10");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls11
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls11, env, fips, "wtls11");
#else
#endif
    // Non-validated algorithms follow
#ifdef NID_secp112r1
    add_curve_if_supported(NID_secp112r1, env, fips, "secp112r1");
#else
#endif
#ifdef NID_secp112r2
    add_curve_if_supported(NID_secp112r2, env, fips, "secp112r2");
#else
#endif
#ifdef NID_secp128r1
    add_curve_if_supported(NID_secp128r1, env, fips, "secp128r1");
#else
#endif
#ifdef NID_secp128r2
    add_curve_if_supported(NID_secp128r2, env, fips, "secp128r2");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls6
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls6, env, fips, "wtls6");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls8
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls8, env, fips, "wtls8");
#else
#endif
    //#if !defined(OPENSSL_NO_EC2M)
#ifdef NID_sect113r1
    add_curve_if_supported(NID_sect113r1, env, fips, "sect113r1");
#else
#endif
#ifdef NID_sect113r2
    add_curve_if_supported(NID_sect113r2, env, fips, "sect113r2");
#else
#endif
#ifdef NID_sect131r1
    add_curve_if_supported(NID_sect131r1, env, fips, "sect131r1");
#else
#endif
#ifdef NID_sect131r2
    add_curve_if_supported(NID_sect131r2, env, fips, "sect131r2");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls1
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls1, env, fips, "wtls1");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls4
    add_curve_if_supported(NID_wap_wsg_idm_ecid_wtls4, env, fips, "wtls4");
#else
#endif
#ifdef NID_ipsec3
    add_curve_if_supported(NID_ipsec3, env, fips, "ipsec3");
#else
#endif
#ifdef NID_ipsec4
    add_curve_if_supported(NID_ipsec4, env, fips, "ipsec4");
#else
#endif

    if (!fips) {
#ifdef HAVE_ED25519
        add_curve_if_supported(0, env, fips, "ed25519");
#endif
#ifdef HAVE_ED448
        add_curve_if_supported(0, env, fips, "ed448");
#endif
#ifdef HAVE_X25519
        add_curve_if_supported(0, env, fips, "x25519");
#endif
#ifdef HAVE_X448
        add_curve_if_supported(0, env, fips, "x448");
#endif
    }

    /* Check buffer overrun just in case */
    ASSERT(algo_curve.count <= sizeof(algo_curve.algorithms)/sizeof(algo_curve.algorithms[0]));
#endif
}

#if defined(HAVE_EC)

/* Check if the curve in nid is supported by the
   current cryptolib and current FIPS state.
*/

bool is_curve_valid_by_nid(const int nid) {
    bool ret = false;

#if defined(HAVE_DH)
# if defined(HAS_EVP_PKEY_CTX) && (! DISABLE_EVP_DH)
    EVP_PKEY_CTX *pctx = NULL, *kctx = NULL;
    EVP_PKEY *pkey = NULL, *params = NULL;
    
    if (NULL == (pctx = EVP_PKEY_CTX_new_id(EVP_PKEY_EC, NULL)))
        goto out;
    
    if (1 != EVP_PKEY_paramgen_init(pctx))
        goto out;

    if (1 != EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx, nid))
        goto out;

    if (!EVP_PKEY_paramgen(pctx, &params))
        goto out;

    if (NULL == (kctx = EVP_PKEY_CTX_new(params, NULL)))
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
    const size_t cnt = FIPS_MODE() ? algo_rsa_opts_fips_cnt : algo_rsa_opts_cnt;
    return enif_make_list_from_array(env, algo_rsa_opts, cnt);
}

void init_rsa_opts_types(ErlNifEnv* env) {
    // Validated algorithms first
    algo_rsa_opts_cnt = 0;
#ifdef HAS_EVP_PKEY_CTX
# ifdef HAVE_RSA_PKCS1_PSS_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_pss_padding");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pss_saltlen");
# endif
# ifdef HAVE_RSA_MGF1_MD
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_mgf1_md");
# endif
# ifdef HAVE_RSA_OAEP_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_oaep_padding");
# endif
# ifdef HAVE_RSA_OAEP_MD
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_oaep_label");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_oaep_md");
# endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"signature_md");
#endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_padding");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_x931_padding");
#ifdef HAVE_RSA_SSLV23_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_sslv23_padding");
#endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_no_padding");
    algo_rsa_opts_fips_cnt = algo_rsa_opts_cnt;

    ASSERT(algo_rsa_opts_cnt <= sizeof(algo_rsa_opts)/sizeof(algo_rsa_opts[0]));
}

ERL_NIF_TERM fips_forbidden_hash_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef FIPS_SUPPORT
    return digest_types_as_list(env, true);
#else
    return enif_make_list(env, 0); /* nothing is forbidden */
#endif
}

ERL_NIF_TERM fips_forbidden_pubkey_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef FIPS_SUPPORT
    /* Filter the results by IS_PUBKEY_FORBIDDEN_IN_FIPS() == true */
    return pubkey_algorithms_as_list(env, true);
#else
    return enif_make_list(env, 0); /* nothing is forbidden */
#endif
}

ERL_NIF_TERM fips_forbidden_cipher_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef FIPS_SUPPORT
    /* Filter the results by IS_CIPHER_FORBIDDEN_IN_FIPS() == true */
    return cipher_types_as_list(env, true);
#else
    return enif_make_list(env, 0); /* nothing is forbidden */
#endif
}

ERL_NIF_TERM fips_forbidden_kem_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef FIPS_SUPPORT
    return kem_algorithms_as_list(env, true);
#else
    return enif_make_list(env, 0); /* nothing is forbidden */
#endif
}

ERL_NIF_TERM fips_forbidden_mac_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef FIPS_SUPPORT
    return mac_types_as_list(env, true);
#else
    return enif_make_list(env, 0); /* not forbidden */
#endif
}

ERL_NIF_TERM fips_forbidden_curve_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    curves_lazy_init(env, FIPS_MODE());
    return curve_algorithms_as_list(env, true);
}
