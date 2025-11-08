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

#include "algorithms_digest.h"
#include "algorithms_collection.h"

static digest_probe_t digest_probes[] = {
#ifdef HAVE_MD4
        {.str = "md4", .str_v3 = "MD4", .constructor_fn = &EVP_md4},
#endif
#ifdef HAVE_MD5
        {.str = "md5", .str_v3 = "MD5", .constructor_fn = &EVP_md5},
#endif
#ifdef HAVE_RIPEMD160
        {.str = "ripemd160", .str_v3 = "RIPEMD160", .constructor_fn = &EVP_ripemd160},
#endif
        {.str = "sha", .str_v3 = "SHA1", .flags_hint = PBKDF2_ELIGIBLE_DIGEST, .constructor_fn = &EVP_sha1},
#ifdef HAVE_SHA224
        {.str = "sha224", .str_v3 = "SHA2-224", .flags_hint = PBKDF2_ELIGIBLE_DIGEST, .constructor_fn = &EVP_sha224},
#endif
#ifdef HAVE_SHA256
        {.str = "sha256", .str_v3 = "SHA2-256", .flags_hint = PBKDF2_ELIGIBLE_DIGEST, .constructor_fn = &EVP_sha256},
#endif
#ifdef HAVE_SHA384
        {.str = "sha384", .str_v3 = "SHA2-384", .flags_hint = PBKDF2_ELIGIBLE_DIGEST, .constructor_fn = &EVP_sha384},
#endif
#ifdef HAVE_SHA512
        {.str = "sha512", .str_v3 = "SHA2-512", .flags_hint = PBKDF2_ELIGIBLE_DIGEST, .constructor_fn = &EVP_sha512},
#endif
#ifdef HAVE_SHA512_224
        {.str = "sha512_224",
         .str_v3 = "SHA2-512/224",
         .flags_hint = PBKDF2_ELIGIBLE_DIGEST,
         .constructor_fn = &EVP_sha512_224},
#endif
#ifdef HAVE_SHA512_256
        {.str = "sha512_256",
         .str_v3 = "SHA2-512/256",
         .flags_hint = PBKDF2_ELIGIBLE_DIGEST,
         .constructor_fn = &EVP_sha512_256},
#endif
#ifdef HAVE_SHA3_224
        {.str = "sha3_224", .str_v3 = "SHA3-224", .constructor_fn = &EVP_sha3_224},
#endif
#ifdef HAVE_SHA3_256
        {.str = "sha3_256", .str_v3 = "SHA3-256", .constructor_fn = &EVP_sha3_256},
#endif
#ifdef HAVE_SHA3_384
        {.str = "sha3_384", .str_v3 = "SHA3-384", .constructor_fn = &EVP_sha3_384},
#endif
#ifdef HAVE_SHA3_512
        {.str = "sha3_512", .str_v3 = "SHA3-512", .constructor_fn = &EVP_sha3_512},
#endif
#ifdef HAVE_SHAKE128
        {.str = "shake128", .str_v3 = "SHAKE-128", .constructor_fn = &EVP_shake128, .xof_default_length = 6},
#endif
#ifdef HAVE_SHAKE256
        {.str = "shake256", .str_v3 = "SHAKE-256", .constructor_fn = &EVP_shake256, .xof_default_length = 32},
#endif
#ifdef HAVE_SM3
        {.str = "sm3", .str_v3 = "SM3", .constructor_fn = &EVP_sm3},
#endif
#ifdef HAVE_BLAKE2
        {.str = "blake2b", .str_v3 = "BLAKE2b512", .constructor_fn = &EVP_blake2b512},
#endif
#ifdef HAVE_BLAKE2
        {.str = "blake2s",
         .str_v3 = "BLAKE2s256",
         .constructor_fn = &EVP_blake2s256},
#endif
};

digest_collection_t digest_collection("crypto.digest.digest_collection", digest_probes,
                                      sizeof(digest_probes) / sizeof(digest_probes[0]));

ERL_NIF_TERM digest_availability_t::get_atom() const { return this->init->atom; }

#ifdef HAS_3_0_API
#ifdef FIPS_SUPPORT
/* Initialize an algorithm to check that all its dependencies are valid in FIPS */
static int is_valid_in_fips(const EVP_MD *md) {
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    int usable = 0;

    if (md) {
        /* Try to initialize the digest algorithm for use, this will check the dependencies */
        if (EVP_DigestInit_ex(ctx, md, NULL) == 1) {
            usable = 1;
        }
    }

    EVP_MD_CTX_free(ctx);
    return usable;
}
#endif /* FIPS_SUPPORT */
#endif /* HAS_3_0_API */

static void update_digest_type_fips_flags(struct digest_availability_t *p) {
#ifdef FIPS_SUPPORT
    EVP_MD *fetched_md = EVP_MD_fetch(NULL, p->str_v3, "fips=yes");
    /* Deeper check for validity in FIPS, also checks for NULL */
    if (is_valid_in_fips(fetched_md)) {
        p->flags &= ~FIPS_FORBIDDEN_DIGEST;
        p->md.p = fetched_md;
    } else {
        p->flags |= FIPS_FORBIDDEN_DIGEST;
        EVP_MD_free(fetched_md); /* NULL is allowed */
    }
#else
    p->md = EVP_MD_fetch(NULL, p->str_v3, "");
#endif /* FIPS_SUPPORT and >=3.0.0 */
}

static void update_digest_type_availability(struct digest_availability_t *p) {
#ifdef HAS_3_0_API
    if (p->str_v3) {
        update_digest_type_fips_flags(p);
    }
#else
    if (p->md.funcp) {
        p->md.p = p->md.funcp();
    }
#endif
}

void digest_types_delayed_init(ErlNifEnv *env) {
    for (struct digest_probe_t *p = digest_types; p->str; p++) {
        update_digest_type_availability(p);
        p->atom = enif_make_atom(env, p->init->str);
    }

    p->atom = atom_false; /* end marker */
}

struct digest_availability_t *get_digest_type(ERL_NIF_TERM type) {
    struct digest_availability_t *p = NULL;
    for (p = digest_types; p->atom != atom_false; p++) {
        if (type == p->atom) {
            return p;
        }
    }

    return NULL;
}
