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
#include "algorithms_pubkey.h"
#include "algorithms_digest.h"
#include "algorithms_curve.h"
#include "cipher.h"
#include "common.h"
#include "mac.h"

#include <openssl/core_names.h>
#include "algorithms_digest.h"

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

void init_kem_types(void);

static size_t algo_rsa_opts_cnt, algo_rsa_opts_fips_cnt;
static ERL_NIF_TERM algo_rsa_opts[11]; /* increase when extending the list */
void init_rsa_opts_types(ErlNifEnv* env);


void init_algorithms_types(ErlNifEnv* env)
{
    init_mac_types(env);
    init_cipher_types(env);
    init_kem_types();
    init_rsa_opts_types(env);
    /* ciphers and macs are initiated statically */
}

/*================================================================
  Hash algorithms
*/

ERL_NIF_TERM hash_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    digest_types_lazy_init(env, FIPS_MODE());
    return digest_types_as_list(env, false);
}

ERL_NIF_TERM pubkey_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    pubkey_algorithms_lazy_init(env, FIPS_MODE());
    // Filter the results by IS_PUBKEY_FORBIDDEN_IN_FIPS() == false
    return pubkey_algorithms_as_list(env, false);
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

ERL_NIF_TERM curve_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    curve_algorithms_lazy_init(env, FIPS_MODE());
    return curve_algorithms_as_list(env, false);
}

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
    digest_types_lazy_init(env, FIPS_MODE());
    // Filter the results by the result of algorithm.is_forbidden_in_fips() == true
    return digest_types_as_list(env, true);
}

ERL_NIF_TERM fips_forbidden_pubkey_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef FIPS_SUPPORT
    pubkey_algorithms_lazy_init(env, 1);
    // Filter the results by the result of algorithm.is_forbidden_in_fips() == true
    return pubkey_algorithms_as_list(env, true);
#else
    return enif_make_list(env, 0); // nothing is forbidden
#endif
}

ERL_NIF_TERM fips_forbidden_cipher_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef FIPS_SUPPORT
    // Filter the results by the result of algorithm.is_forbidden_in_fips() == true
    return cipher_types_as_list(env, true);
#else
    return enif_make_list(env, 0); // nothing is forbidden
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
    curve_algorithms_lazy_init(env, FIPS_MODE());
    return curve_algorithms_as_list(env, true);
}
